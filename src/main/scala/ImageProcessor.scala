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
  val ready, busy = Value
}

class ImageProcessor(p: ImageProcessorParams) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Bundle {
      val data = Vec(p.numRows, Vec(p.numCols, Vec(p.numChannels, UInt(8.W))))
    }))
    val out = Decoupled(Vec(p.numRows, Vec(p.numCols, Vec(p.numChannels, UInt(8.W)))))
  })

//  val state = RegInit(ImageProcessorState.ready)

  io.in.ready := true.B
  io.out.valid := true.B
  for (r <- 0 until p.numRows) {
    for (c <- 0 until p.numCols) {
      for (i <- 0 until p.numChannels) {
        io.out.bits(r)(c)(i) := io.in.bits.data(r)(c)(i)
      }
    }
  }
}
