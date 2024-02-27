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
    val state = Output(ImageProcessorState())
    val out = Decoupled(Vec(p.numRows, Vec(p.numCols, Vec(p.numChannels, UInt(8.W)))))
  })

  val st = RegInit(ImageProcessorState.ready)
  val in_ready = RegInit(true.B)
  val out_valid = RegInit(false.B)
  io.state := st
  io.in.ready := in_ready
  io.out.valid := out_valid
  val mat = Reg(Vec(p.numRows, Vec(p.numCols, Vec(p.numChannels, UInt(8.W)))))
  val pRow = RegInit(0.U(16.W))
  val pCol = RegInit(0.U(16.W))
  val pChannel = RegInit(0.U(16.W))
  // for (r <- 0 until p.numRows) {
  //   for (c <- 0 until p.numCols) {
  //     for (i <- 0 until p.numChannels) {
  //       io.out.bits(r)(c)(i) := io.in.bits.data(r)(c)(i)
  //     }
  //   }
  // }
  when (st === ImageProcessorState.ready && io.in.valid) {
    st := ImageProcessorState.busy
    mat := io.in.bits.data
    in_ready := false.B
    pRow := 0.U
    pCol := 0.U
    pChannel := 0.U
  }
  when (st === ImageProcessorState.busy) {
    when (pRow < p.numRows.U) {
      when (pCol < p.numCols.U) {
        mat(pRow)(pCol) := io.in.bits.data(pRow)(pCol) // Apply filter function here
        pCol := pCol + 1.U
      }
      when (pCol + 1.U >= p.numCols.U) {
        pCol := 0.U
        pRow := pRow + 1.U
      }
    }
    when (pRow + 1.U >= p.numRows.U) {
      out_valid := true.B
    }
  }
  io.out.bits := mat
}
