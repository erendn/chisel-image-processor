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

case class ImageProcessorParams(imageWidth: Int, imageHeight: Int, pixelsPerCycle: Int) {
  // Parameters must be non-negative
  require(imageWidth > 0)
  require(imageHeight > 0)
  require(pixelsPerCycle > 0)
  // Image width must be divisible by the number of pixels per cycle
  require(imageWidth % pixelsPerCycle == 0)

  val numRows: Int = imageHeight // Number of rows in the image
  val numCols: Int = imageWidth // Number of columns in the image
  val numChannels: Int = 3 // Channels are (red, green, blue)
  val batchSize: Int = pixelsPerCycle // Number of pixels per cycle (batch size)
  val numBatchesInRow: Int = imageWidth / pixelsPerCycle // Number of batches in a column
}

object ImageProcessorState extends ChiselEnum {
  val idle, fillingBuffer, processing, processingCooldown, done = Value
}

abstract class ImageProcessor(p: ImageProcessorParams, filterName: String) extends CustomModule(p) {
  val io = IO(new Bundle {
    // Input interface
    val in = Flipped(Decoupled(new Bundle {
      val row = UInt(log2Ceil(p.numRows).W) // For debug purposes
      val col = UInt(log2Ceil(p.numCols).W) // For debug purposes
      val data = HWPixelBatch()
    }))
    // Output interface
    val state = Output(ImageProcessorState()) // For debug purposes
    val out = Decoupled(new Bundle {
        val row = UInt(log2Ceil(p.numRows).W) // For debug purposes
        val col = UInt(log2Ceil(p.numCols).W) // For debug purposes
        val data = HWPixelBatch()
    })
  })

  // Internal state register
  val stateReg = RegInit(ImageProcessorState.idle)
  // Row and column pointers for processing
  val currentRow = RegInit(0.U(log2Ceil(p.numRows + 2).W)) // +2 since kernel processor is delayed by (p.numBatchesInRow+1)
  val currentCol = RegInit(0.U(log2Ceil(p.numCols).W))
  // Instantiate the filter operator
  val filterOperators = Seq.fill(p.batchSize)(Module(FilterGenerator.get(p, filterName)))
  for (i <- 0 until p.batchSize) {
    for (j <- 0 until filterOperators(i).numPixels) {
      filterOperators(i).io.in(j) := emptyPixel
    }
  }

  // Default outputs
  io.in.ready := false.B
  io.state := stateReg
  io.out.valid := false.B
  io.out.bits.row := 0.U
  io.out.bits.col := 0.U
  io.out.bits.data := emptyBatch

  // Helper functions
  // Return if the column if at the end of the row (last batch)
  def isEndOfRow(col: UInt): Bool = {
    col === (p.numCols - p.batchSize).U
  }
  // Return if the coordinate is at the end of the image (last batch)
  def isEndOfImage(row: UInt, col: UInt): Bool = {
    isEndOfRow(col) && row === (p.numRows - 1).U
  }
  // Return the next column value in row-major order
  def getNextColumn(col: UInt): UInt = {
    Mux(isEndOfRow(col), 0.U, col + p.batchSize.U)
  }
  // Return the next row value in row-major order
  def getNextRow(row: UInt, col: UInt): UInt = {
    Mux(isEndOfRow(col), row + 1.U, row)
  }
  // Return the upper left coordinate's column
  def getUpperLeftColumn(col: UInt): UInt = {
    Mux(col === 0.U, (p.numCols - p.batchSize).U, col - p.batchSize.U)
  }
  // Return the upper left coordinate's row
  def getUpperLeftRow(row: UInt, col: UInt): UInt = {
    Mux(row === 0.U, 0.U, Mux(col === 0.U, row - 2.U, row - 1.U))
  }
  // Return if the given coordinate is inside the image boundaries
  def isInImage(row: UInt, col: UInt): Bool = {
    (row >= 0.U && row <= (p.numRows - 1).U) && (col >= 0.U && col <= (p.numCols - 1).U)
  }
  // Reset the coordinate to (0,0)
  def resetCoordinate(row: UInt, col: UInt): Unit = {
    col := 0.U
    row := 0.U
  }
  // Update coordinates to iterate in row-major order
  def nextCoordinate(row: UInt, col: UInt): Unit = {
    col := getNextColumn(col)
    row := getNextRow(row, col)
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
        resetCoordinate(currentRow, currentCol)
      }
    }
    // PROCESSING:
    //   This state is supposed to apply the filter on the image while keeping taking an input batch of pixels every
    // cycle until the entire image is taken. It assumes batches will be given in row-major order and there will be a
    // new batch every cycle. It doesn't check "input valid" signal. Every cycle, the filter will be applied for the
    // current batch. When it reaches the end of the last row, it switches to the DONE state.
    is (ImageProcessorState.processing) {
      // Check if the input is in the correct order
      assert(io.in.bits.row === currentRow)
      assert(io.in.bits.col === currentCol)
      // Update the current coordinate
      nextCoordinate(currentRow, currentCol)
      // Apply filter for the current batch
      for (i <- 0 until p.batchSize) {
        filterOperators(i).io.in(0) := io.in.bits.data(i)
      }
      // Output the processed batch
      for (i <- 0 until p.batchSize) {
        io.out.bits.data(i) := filterOperators(i).io.out
      }
      io.out.valid := true.B
      io.out.bits.row := currentRow
      io.out.bits.col := currentCol
      // Stop processing when reached the end
      when (isEndOfImage(currentRow, currentCol)) {
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
    buffer.io.wData := emptyBatch
  }
  def bufferRead(buffer: RowBufferMemory, addr: UInt): Unit = {
    buffer.io.rEn := true.B
    buffer.io.rAddr := addr
  }
  def bufferWrite(buffer: RowBufferMemory, addr: UInt, data: Vec[Vec[UInt]]): Unit = {
    buffer.io.wEn := true.B
    buffer.io.wAddr := addr
    buffer.io.wData := data
  }

  // Pixel matrix to apply the filter kernel on
  // Doesn't have the last column because the last column is received by the buffers and the input
  val pixelMatrix = Reg(Vec(filterOperators(0).numKernelRows, Vec(p.batchSize + 1, HWPixel())))
  val pixelMatrixLastColumn = Seq(topRowBuffer.io.rData, midRowBuffer.io.rData, io.in.bits.data)
  // Update pixel matrix for the next cycle
  def updatePixelMatrix(): Unit = {
    // | a | d |   | h |    | d | h |
    // | b | e | + | i | -> | e | i |
    // | c | f |   | j |    | f | j |
    // h: Output of top row buffer
    // i: Output of mid row buffer
    // j: Input pixel
    // Clamp nearest pixels for off-the-edge matrix values for the left edge
    for (r <- 0 until filterOperators(0).numKernelRows) {
      pixelMatrix(r)(0) := Mux(currentCol > 0.U, pixelMatrix(r)(p.batchSize), pixelMatrixLastColumn(r)(0))
      for (b <- 0 until p.batchSize) {
        pixelMatrix(r)(b + 1) := pixelMatrixLastColumn(r)(b)
      }
    }
  }

  // Finite state machine
  switch (stateReg) {
    // READY:
    //   This state is supposed to wait until a high "input valid" signal is received. Then, it switches to the
    // FILLING BUFFER state. It doesn't expect any input pixel batch in this cycle.
    is(ImageProcessorState.idle) {
      io.in.ready := true.B
      // Wait until input is being sent
      when (io.in.valid) {
        // Start taking the input a pixel at a time in the next cycle
        stateReg := ImageProcessorState.fillingBuffer
        resetCoordinate(currentRow, currentCol)
      }
    }
    // FILLING BUFFER:
    //   This state is supposed to write clamped pixels to the internal top and middle row buffers. It assumes pixel
    // batches will be given in row-major order and there will be a new batch every cycle. It doesn't check
    // "input valid" signal. When it reaches the end of the row, it switches to the PROCESSING state.
    is (ImageProcessorState.fillingBuffer) {
      // Check if the input is in the correct order
      assert(io.in.bits.row === currentRow)
      assert(io.in.bits.col === currentCol)
      // Update the current coordinate
      nextCoordinate(currentRow, currentCol)
      // Write to row buffers (write the same data to both buffers to easily clamp pixels for the top edge)
      bufferWrite(topRowBuffer, currentCol / p.batchSize.U, io.in.bits.data)
      bufferWrite(midRowBuffer, currentCol / p.batchSize.U, io.in.bits.data)
      // Switch to processing state to start giving output
      when (isEndOfRow(currentCol)) {
        stateReg := ImageProcessorState.processing
        // Read initial pixels from buffers since we'll start applying the filter in the next cycle
        bufferRead(topRowBuffer, 0.U)
        bufferRead(midRowBuffer, 0.U)
      }
    }
    // PROCESSING:
    //   This state is supposed to apply the filter on the image while keeping taking an input pixel batch every cycle
    // until the entire image is taken. It assumes batches will be given in row-major order and there will be a new
    // pixel batch every cycle. It doesn't check "input valid" signal. Every cycle, the filter will be applied for the
    // upper left pixel batch. When it reaches the end of the last row, it switches to the PROCESSING COOLDOWN state.
    is (ImageProcessorState.processing) {
      // Check if the input is in the correct order
      assert(io.in.bits.row === currentRow)
      assert(io.in.bits.col === currentCol)
      // Update the current coordinate
      nextCoordinate(currentRow, currentCol)
      // Read next pixel batch from buffers
      bufferRead(topRowBuffer, getNextColumn(currentCol) / p.batchSize.U)
      bufferRead(midRowBuffer, getNextColumn(currentCol) / p.batchSize.U)
      // Update pixel matrix for the next cycle
      updatePixelMatrix()
      // Update buffers
      bufferWrite(topRowBuffer, currentCol / p.batchSize.U, midRowBuffer.io.rData) // Shift mid to top
      bufferWrite(midRowBuffer, currentCol / p.batchSize.U, io.in.bits.data) // Put new pixel to mid
      // There won't be an output for only the very first batch when we switch to this state because the pixel matrix
      // hasn't been received yet
      when (currentRow > 1.U || currentCol > 0.U) {
        // Apply filter for upper left batch
        // If upper left batch is the right edge, apply nearest pixels for off-the-edge matrix values
        for (b <- 0 until p.batchSize) {
          for (r <- 0 until filterOperators(b).numKernelRows) {
            filterOperators(b).io.in(r * filterOperators(b).numKernelCols) := pixelMatrix(r)(b)
            filterOperators(b).io.in(r * filterOperators(b).numKernelCols + 1) := pixelMatrix(r)(b + 1)
            if (b < p.batchSize - 1) {
              filterOperators(b).io.in(r * filterOperators(b).numKernelCols + 2) := pixelMatrix(r)(b + 2)
            } else {
              filterOperators(b).io.in(r * filterOperators(b).numKernelCols + 2) := Mux(currentCol === 0.U, pixelMatrix(r)(b + 1), pixelMatrixLastColumn(r)(0))
            }
          }
          // Output the processed pixel
          io.out.bits.data(b) := filterOperators(b).io.out
        }
        io.out.valid := true.B
        io.out.bits.row := getUpperLeftRow(currentRow, currentCol)
        io.out.bits.col := getUpperLeftColumn(currentCol)
      }
      // Switch to the cool down stage after receiving all batches
      when (isEndOfImage(currentRow, currentCol)) {
        stateReg := ImageProcessorState.processingCooldown
      }
    }
    // PROCESSING COOLDOWN:
    //   This state is supposed to apply the filter on the last row of the image without taking any input since all the
    // image has already been received. Every cycle, the filter will be applied for the upper left batch. When it
    // outputs the last pixel batch of the image, it switches to the DONE state.
    is(ImageProcessorState.processingCooldown) {
      // Update the current coordinate
      nextCoordinate(currentRow, currentCol)
      // Read next batch from buffers
      bufferRead(topRowBuffer, getNextColumn(currentCol) / p.batchSize.U)
      bufferRead(midRowBuffer, getNextColumn(currentCol) / p.batchSize.U)
      // Update pixel matrix for the next cycle
      updatePixelMatrix()
      // Apply filter for the upper left batch
      // If the upper left batch is the right edge, apply nearest pixels for off-the-edge matrix values
      for (b <- 0 until p.batchSize) {
        for (r <- 0 until filterOperators(b).numKernelRows - 1) {
          filterOperators(b).io.in(r * filterOperators(b).numKernelCols) := pixelMatrix(r)(b)
          filterOperators(b).io.in(r * filterOperators(b).numKernelCols + 1) := pixelMatrix(r)(b + 1)
          if (b < p.batchSize - 1) {
            filterOperators(b).io.in(r * filterOperators(b).numKernelCols + 2) := pixelMatrix(r)(b + 2)
          } else {
            filterOperators(b).io.in(r * filterOperators(b).numKernelCols + 2) := Mux(currentCol === 0.U, pixelMatrix(r)(b + 1), pixelMatrixLastColumn(r)(0))
          }

        }
        // Bottom row
        when (currentRow === p.numRows.U && currentCol === 0.U) {
          filterOperators(b).io.in(6) := pixelMatrix(2)(b)
          filterOperators(b).io.in(7) := pixelMatrix(2)(b + 1)
          if (b < p.batchSize - 1) {
            filterOperators(b).io.in(8) := pixelMatrix(2)(b + 2)
          } else {
            filterOperators(b).io.in(8) := pixelMatrix(2)(b + 1)
          }
        } .otherwise {
          filterOperators(b).io.in(6) := pixelMatrix(1)(b)
          filterOperators(b).io.in(7) := pixelMatrix(1)(b + 1)
          if (b < p.batchSize - 1) {
            filterOperators(b).io.in(8) := pixelMatrix(1)(b + 2)
          } else {
            filterOperators(b).io.in(8) := Mux(currentCol === 0.U, pixelMatrix(1)(b + 1), pixelMatrixLastColumn(1)(0))
          }
        }
        // Output the processed pixel
        io.out.bits.data(b) := filterOperators(b).io.out
      }
      io.out.valid := true.B
      io.out.bits.row := getUpperLeftRow(currentRow, currentCol)
      io.out.bits.col := getUpperLeftColumn(currentCol)
      // Stop processing when all outputs are given
      when (currentCol === 0.U && currentRow === (p.numRows + 1).U) {
        stateReg := ImageProcessorState.done
      }
    }
    // DONE:
    //   This state is the last state for now. Nothing happens here.
    is(ImageProcessorState.done) {
    }
  }
}
