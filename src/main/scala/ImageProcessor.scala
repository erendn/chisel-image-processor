package chisel_image_processor

import chisel3._
import chisel3.util._

// Generator function to return the proper image processor instance for the given filter
object ImageProcessorGenerator {
  def get(p: ImageProcessorParams, filterName: String): ImageProcessor = {
    if (FilterGenerator.isKernelFilter(filterName)) {
      return new KernelImageProcessor(p, filterName)
    }
    return new BasicImageProcessor(p, filterName)
  }
}

case class ImageProcessorParams(imageWidth: Int, imageHeight: Int) {
  // Image size must be non-negative
  require(imageWidth > 0)
  require(imageHeight > 0)

  val numRows: Int = imageHeight // Number of rows in the image
  val numCols: Int = imageWidth // Number of columns in the image
  val numChannels: Int = 3 // Channels are (red, green, blue)
}

object ImageProcessorState extends ChiselEnum {
  val idle, fillingTopRow, fillingMidRow, processing, done = Value
}

abstract class ImageProcessor(p: ImageProcessorParams, filterName: String) extends CustomModule(p) {
  val io = IO(new Bundle {
    // Input interface
    val in = Flipped(Decoupled(new Bundle {
      val row = UInt(log2Ceil(p.numRows).W) // For debug purposes
      val col = UInt(log2Ceil(p.numCols).W) // For debug purposes
      val data = HWPixel()
    }))
    // Output interface
    val state = Output(ImageProcessorState()) // For debug purposes
    val out = Decoupled(new Bundle {
        val row = UInt(log2Ceil(p.numRows).W) // For debug purposes
        val col = UInt(log2Ceil(p.numCols).W) // For debug purposes
        val data = HWPixel()
    })
  })

  // Internal state register
  val stateReg = RegInit(ImageProcessorState.idle)
  // Row and column pointers for processing
  val currentRow = RegInit(0.U(log2Ceil(p.numRows).W))
  val currentCol = RegInit(0.U(log2Ceil(p.numCols).W))
  // Instantiate the filter operator
  val filterOperator = Module(FilterGenerator.get(p, filterName))
  for (i <- 0 until filterOperator.numKernelRows * filterOperator.numKernelCols) {
    filterOperator.io.in(i) := emptyPixel
  }

  // Default outputs
  io.in.ready := false.B
  io.state := stateReg
  io.out.valid := false.B
  io.out.bits.row := 0.U
  io.out.bits.col := 0.U
  io.out.bits.data := emptyPixel

  // Update coordinates to iterate in row-major order
  def nextCoordinate(row: UInt, col: UInt): Unit = {
    col := col + 1.U
    when (col === (p.numCols - 1).U) {
      col := 0.U
      row := row + 1.U
    }
  }
}

class BasicImageProcessor(p: ImageProcessorParams, filterName: String) extends ImageProcessor(p, filterName) {
  // Finite state machine
  switch (stateReg) {
    // READY:
    //   This state is supposed to wait until a high "input valid" signal is received. Then, it switches to the
    // PROCESSING state.
    is(ImageProcessorState.idle) {
      io.in.ready := true.B
      // Wait until input is being sent
      when (io.in.valid) {
        // Start taking the input a pixel at a time in the next cycle
        stateReg := ImageProcessorState.processing
        currentRow := 0.U
        currentCol := 0.U
      }
    }
    // PROCESSING:
    //   This state is supposed to apply the filter on the image while keeping taking an input pixel every cycle until
    // the entire image is taken. It assumes pixels will be given in row-major order and there will be a new pixel every
    // cycle. It doesn't check "input valid" signal. Every cycle, the filter will be applied for the
    // (currentRow, currentCol) pixel. When it reaches the end of the last row, it switches to the DONE state.
    is (ImageProcessorState.processing) {
      // Check if the input is in the correct order
      assert(io.in.bits.row === currentRow)
      assert(io.in.bits.col === currentCol)
      // Update the current coordinate
      nextCoordinate(currentRow, currentCol)
      // Apply filter for (currentRow,currentCol)
      for (n <- 0 until filterOperator.numKernelRows; m <- 0 until filterOperator.numKernelCols) {
        if (m != 2) {
          filterOperator.io.in(0) := io.in.bits.data
        }
      }
      // Output the processed pixel
      io.out.bits.data := filterOperator.io.out
      io.out.valid := true.B
      io.out.bits.row := currentRow
      io.out.bits.col := currentCol
      // Stop processing when reached the end
      when (currentCol === (p.numCols - 1).U && currentRow === (p.numRows - 1).U) {
        stateReg := ImageProcessorState.done
      }
    }
    // DONE:
    //   This state is the last state for now. Nothing happens here.
    is(ImageProcessorState.done) {
    }
  }
}

class KernelImageProcessor(p: ImageProcessorParams, filterName: String) extends ImageProcessor(p, filterName) {
  // Pixel matrix to apply the filter kernel on
  // Only has the first two columns because the last column is received by the buffers and the input
  val pixelMatrix = Reg(Vec(p.numChannels, Vec(p.numChannels - 1, HWPixel())))
  // Row buffers
  val topRowBuffer = Module(new RowBufferMemory(p))
  bufferInit(topRowBuffer)
  val midRowBuffer = Module(new RowBufferMemory(p))
  bufferInit(midRowBuffer)
  // Row buffer functions
  def bufferInit(buffer: RowBufferMemory): Unit = {
    buffer.io.rEn := false.B
    buffer.io.rAddr := 0.U
    buffer.io.wEn := false.B
    buffer.io.wAddr := 0.U
    buffer.io.wData := emptyPixel
  }
  def bufferWrite(buffer: RowBufferMemory, addr: UInt, data: Vec[UInt]): Unit = {
    buffer.io.wEn := true.B
    buffer.io.wAddr := addr
    buffer.io.wData := data
  }
  def bufferRead(buffer: RowBufferMemory, addr: UInt): Unit = {
    buffer.io.rEn := true.B
    buffer.io.rAddr := addr
  }

  // Finite state machine
  switch (stateReg) {
    // READY:
    //   This state is supposed to wait until a high "input valid" signal is received. Then, it switches to the
    // FILLING TOP ROW state. It doesn't expect any pixel input in this cycle.
    is(ImageProcessorState.idle) {
      io.in.ready := true.B
      // Wait until input is being sent
      when (io.in.valid) {
        // Start taking the input a pixel at a time in the next cycle
        stateReg := ImageProcessorState.fillingTopRow
        currentRow := 0.U
        currentCol := 0.U
      }
    }
    // FILLING TOP ROW:
    //   This state is supposed to write input pixels to the internal top row buffer. It assumes pixels will be given
    // in row-major order and there will be a new pixel every cycle. It doesn't check "input valid" signal. When it
    // reaches the end of the row, it switches to the FILLING MID ROW state.
    is (ImageProcessorState.fillingTopRow) {
      // Check if the input is in the correct order
      assert(io.in.bits.row === currentRow)
      assert(io.in.bits.col === currentCol)
      // Write to the top row buffer
      bufferWrite(topRowBuffer, currentCol, io.in.bits.data)
      // Update the current coordinate
      nextCoordinate(currentRow, currentCol)
      // Switch to the next row buffer when reached the end
      when (currentCol === (p.numCols - 1).U) {
        stateReg := ImageProcessorState.fillingMidRow
      }
    }
    // FILLING MID ROW:
    //   This state is supposed to write input pixels to the internal mid row buffer. It assumes pixels will be given
    // in row-major order and there will be a new pixel every cycle. It doesn't check "input valid" signal. When it
    // reaches the end of the row, it switches to the PROCESSING state.
    is (ImageProcessorState.fillingMidRow) {
      // Check if the input is in the correct order
      assert(io.in.bits.row === currentRow)
      assert(io.in.bits.col === currentCol)
      // Write to the top row buffer
      bufferWrite(midRowBuffer, currentCol, io.in.bits.data)
      // Update the current coordinate
      nextCoordinate(currentRow, currentCol)
      // Switch to the filtering state when reached the end
      when (currentCol === (p.numCols - 1).U) {
        stateReg := ImageProcessorState.processing
        // Read initial pixels from buffers
        bufferRead(topRowBuffer, 0.U)
        bufferRead(midRowBuffer, 0.U)
      }
    }
    // PROCESSING:
    //   This state is supposed to apply the filter on the image while keeping taking an input pixel every cycle until
    // the entire image is taken. It assumes pixels will be given in row-major order and there will be a new pixel every
    // cycle. It doesn't check "input valid" signal. Every cycle, the filter will be applied for the
    // (currentRow-1, currentCol-1) pixel. When it reaches the end of the last row, it switches to the DONE state.
    // WARNING: The image processor doesn't apply the filter for edge pixels since every filter handles them differently.
    is (ImageProcessorState.processing) {
      // Check if the input is in the correct order
      assert(io.in.bits.row === currentRow)
      assert(io.in.bits.col === currentCol)
      // Update the current coordinate
      nextCoordinate(currentRow, currentCol)
      // Read next pixels from buffers
      bufferRead(topRowBuffer, currentCol + 1.U)
      bufferRead(midRowBuffer, currentCol + 1.U)
      when (currentCol === (p.numCols - 1).U) {
        bufferRead(topRowBuffer, 0.U)
        bufferRead(midRowBuffer, 0.U)
      }
      // Update pixel matrix for the next cycle
      // | a | d |   | h |    | d | h |
      // | b | e | + | i | -> | e | i |
      // | c | f |   | j |    | f | j |
      // h: Output of top row buffer
      // i: Output of mid row buffer
      // j: Input pixel
      for (n <- 0 until filterOperator.numKernelRows) {
        pixelMatrix(n)(0) := pixelMatrix(n)(1)
      }
      pixelMatrix(0)(1) := topRowBuffer.io.rData
      pixelMatrix(1)(1) := midRowBuffer.io.rData
      pixelMatrix(2)(1) := io.in.bits.data
      // Update buffers
      bufferWrite(topRowBuffer, currentCol, midRowBuffer.io.rData) // Shift mid to top
      bufferWrite(midRowBuffer, currentCol, io.in.bits.data) // Put new pixel to mid
      // If first two pixels in the row, we will just read buffers and take the input (no output)
      when (currentCol > 1.U) {
        // Apply filter for (currentRow-1,currentCol-1)
        filterOperator.io.in(0) := pixelMatrix(0)(0)
        filterOperator.io.in(1) := pixelMatrix(0)(1)
        filterOperator.io.in(2) := topRowBuffer.io.rData
        filterOperator.io.in(3) := pixelMatrix(1)(0)
        filterOperator.io.in(4) := pixelMatrix(1)(1)
        filterOperator.io.in(5) := midRowBuffer.io.rData
        filterOperator.io.in(6) := pixelMatrix(2)(0)
        filterOperator.io.in(7) := pixelMatrix(2)(1)
        filterOperator.io.in(8) := io.in.bits.data
        filterOperator.io.in(2) := topRowBuffer.io.rData
        filterOperator.io.in(5) := midRowBuffer.io.rData
        filterOperator.io.in(8) := io.in.bits.data
        // Output the processed pixel
        io.out.bits.data := filterOperator.io.out
        io.out.valid := true.B
        io.out.bits.row := currentRow - 1.U
        io.out.bits.col := currentCol - 1.U
        // Stop processing when reached the end
        when (currentCol === (p.numCols - 1).U && currentRow === (p.numRows - 1).U) {
          stateReg := ImageProcessorState.done
        }
      }
    }
    // DONE:
    //   This state is the last state for now. Nothing happens here.
    is(ImageProcessorState.done) {
    }
  }
}