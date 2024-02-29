package chisel_image_processor

import chisel3._
import chisel3.util._

case class ImageProcessorParams(imageWidth: Int, imageHeight: Int) {
  require(imageWidth > 0)
  require(imageHeight > 0)

  val numRows: Int = imageHeight
  val numCols: Int = imageWidth
  val numChannels: Int = 3 // red, green, blue
}

object ImageProcessorState extends ChiselEnum {
  val idle, busy, done = Value
}

class ImageProcessor(p: ImageProcessorParams) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Bundle {
      val data = Vec(p.numRows, Vec(p.numCols, Vec(p.numChannels, UInt(8.W))))
    }))
    val state = Output(ImageProcessorState()) // For debug purposes
    val out = Decoupled(Vec(p.numRows, Vec(p.numCols, Vec(p.numChannels, UInt(8.W)))))
  })

  // Internal state register
  val stateReg = RegInit(ImageProcessorState.idle)

  // Image register to hold the entire image
  // TODO: Use a memory as a buffer
  val imageReg = Reg(Vec(p.numRows, Vec(p.numCols, Vec(p.numChannels, UInt(8.W)))))
  val filteredImageReg = Reg(Vec(p.numRows, Vec(p.numCols, Vec(p.numChannels, UInt(8.W)))))
  // Row and column pointers for processing
  val currentRow = RegInit(0.U(log2Ceil(p.numRows + 1).W))
  val currentCol = RegInit(0.U(log2Ceil(p.numCols + 1).W))

  // Instantiate an operator for each channel
  // TODO: Make this a function and move out of this class
  val channelOperators = Seq(Module(new SobelFilter()), Module(new SobelFilter()), Module(new SobelFilter()))
  for (i <- 0 until p.numChannels) {
    for (j <- 0 until 9) {
      channelOperators(i).io.in(j) := 0.U
    }
  }

  // Default outputs
  io.state := stateReg
  io.in.ready := false.B
  io.out.valid := false.B
  io.out.bits := io.in.bits.data // FIXME: Passing the filtered image register is *VERY* slow in simulation

  switch (stateReg) {
    // READY STATE
    is(ImageProcessorState.idle) {
      io.in.ready := true.B
      // Wait until input is being sent
      when (io.in.valid) {
        stateReg := ImageProcessorState.busy
        imageReg := io.in.bits.data // TODO: Streamline the input
        // Start from (1,1) since edge pixels will be zeroed out
        currentRow := 1.U
        currentCol := 1.U
      }
    }
    // BUSY STATE
    is (ImageProcessorState.busy) {
      // Process one pixel each cycle
      // TODO: Parallelize this after streamlining
      when (currentCol < (p.numCols - 1).U) {
        for (i <- 0 until p.numChannels) {
          channelOperators(i).io.in(0) := imageReg(currentRow - 1.U)(currentCol - 1.U)(i)
          channelOperators(i).io.in(1) := imageReg(currentRow - 1.U)(currentCol)(i)
          channelOperators(i).io.in(2) := imageReg(currentRow - 1.U)(currentCol + 1.U)(i)
          channelOperators(i).io.in(3) := imageReg(currentRow)(currentCol - 1.U)(i)
          channelOperators(i).io.in(4) := imageReg(currentRow)(currentCol)(i)
          channelOperators(i).io.in(5) := imageReg(currentRow)(currentCol + 1.U)(i)
          channelOperators(i).io.in(6) := imageReg(currentRow + 1.U)(currentCol - 1.U)(i)
          channelOperators(i).io.in(7) := imageReg(currentRow + 1.U)(currentCol)(i)
          channelOperators(i).io.in(8) := imageReg(currentRow + 1.U)(currentCol + 1.U)(i)
          filteredImageReg(currentRow)(currentCol)(i) := channelOperators(i).io.out
        }
        currentCol := currentCol + 1.U
        when (currentCol === (p.numCols - 2).U) {
          currentCol := 1.U
          currentRow := currentRow + 1.U
          // TODO: Streamline the output
          when (currentRow === (p.numRows - 2).U) {
            stateReg := ImageProcessorState.done
          }
        }
      }
    }
    is(ImageProcessorState.done) {
      io.out.valid := true.B
    }
  }
}
