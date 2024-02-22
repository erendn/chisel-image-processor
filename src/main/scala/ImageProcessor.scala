package chisel_image_processor

import chisel3._
import chisel3.util._

case class ImageProcessorParams(numRows: Int, numCols: Int) {
  require(numRows > 0)
  require(numCols > 0)
}

object ImageProcessorState extends ChiselEnum {
  val ready, busy = Value
}

class ImageProcessor(p: ImageProcessorParams) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Bundle {
      val data = UInt(8.W)
    }))
    val out = Decoupled(UInt(p.numRows.W))
  })

  val state = RegInit(ImageProcessorState.ready)

  io.in.ready := false.B
  io.out.valid := false.B
  io.out.bits := 0.U
}
