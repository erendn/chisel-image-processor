package chisel_image_processor

import chisel3._
import chisel3.util._

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

class ImageProcessor(p: ImageProcessorParams, filterFunc: ImageProcessorParams => FilterOperator) extends CustomModule(p) {
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

  // Row buffers
  val topRowBuffer = Module(new RowBufferMemory(p))
  topRowBuffer.io.rEn := false.B
  topRowBuffer.io.rAddr := 0.U
  topRowBuffer.io.wEn := false.B
  topRowBuffer.io.wAddr := 0.U
  topRowBuffer.io.wData := emptyPixel
  val midRowBuffer = Module(new RowBufferMemory(p))
  midRowBuffer.io.rEn := false.B
  midRowBuffer.io.rAddr := 0.U
  midRowBuffer.io.wEn := false.B
  midRowBuffer.io.wAddr := 0.U
  midRowBuffer.io.wData := emptyPixel
  // Row and column pointers for processing
  val currentRow = RegInit(0.U(log2Ceil(p.numRows).W))
  val currentCol = RegInit(0.U(log2Ceil(p.numCols).W))

  // Pixel matrix to apply the filter kernel on
  // Only has the first two columns because the last column is received by the buffers and the input
  val pixelMatrix = Reg(Vec(p.numChannels, Vec(p.numChannels - 1, HWPixel())))

  // Instantiate an operator for each channel
  val filterOperator = Module(filterFunc(p))
  for (i <- 0 until 9) {
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
      topRowBuffer.io.wEn := true.B
      topRowBuffer.io.wAddr := currentCol
      topRowBuffer.io.wData := io.in.bits.data
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
      midRowBuffer.io.wEn := true.B
      midRowBuffer.io.wAddr := currentCol
      midRowBuffer.io.wData := io.in.bits.data
      // Update the current coordinate
      nextCoordinate(currentRow, currentCol)
      // Switch to the filtering state when reached the end
      when (currentCol === (p.numCols - 1).U) {
        stateReg := ImageProcessorState.processing
        // Read initial pixels from buffers
        topRowBuffer.io.rEn := true.B
        topRowBuffer.io.rAddr := 0.U
        midRowBuffer.io.rEn := true.B
        midRowBuffer.io.rAddr := 0.U
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
      topRowBuffer.io.rEn := true.B
      topRowBuffer.io.rAddr := currentCol + 1.U
      midRowBuffer.io.rEn := true.B
      midRowBuffer.io.rAddr := currentCol + 1.U
      when (currentCol === (p.numCols - 1).U) {
        topRowBuffer.io.rAddr := 0.U
        midRowBuffer.io.rAddr := 0.U
      }
      // Update pixel matrix for the next cycle
      // | a | d |   | h |    | d | h |
      // | b | e | + | i | -> | e | i |
      // | c | f |   | j |    | f | j |
      // h: Output of top row buffer
      // i: Output of mid row buffer
      // j: Input pixel
      pixelMatrix(0)(0) := pixelMatrix(0)(1)
      pixelMatrix(1)(0) := pixelMatrix(1)(1)
      pixelMatrix(2)(0) := pixelMatrix(2)(1)
      pixelMatrix(0)(1) := topRowBuffer.io.rData
      pixelMatrix(1)(1) := midRowBuffer.io.rData
      pixelMatrix(2)(1) := io.in.bits.data
      // Update buffers
      topRowBuffer.io.wEn := true.B
      topRowBuffer.io.wAddr := currentCol
      topRowBuffer.io.wData := midRowBuffer.io.rData // Shift mid to top
      midRowBuffer.io.wEn := true.B
      midRowBuffer.io.wAddr := currentCol
      midRowBuffer.io.wData := io.in.bits.data // Put new pixel to mid
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
