package chisel_image_processor

import chisel3._
import chisel3.util._

// Parent class for all filter operators
class FilterOperator(numRows: Int, numCols: Int) extends Module {
}

class SobelFilter extends FilterOperator(3, 3) {
  val io = IO(new Bundle {
    val in = Input(Vec(9, UInt(8.W))) // Input of 3x3 pixels
    val out = Output(UInt(8.W)) // Output of the middle pixel
  })

  // x and y gradients (11 bits because max value of gx_w and gy_w is 255*4 and last bit for sign)
  val gx = Wire(SInt(11.W))
  val gy = Wire(SInt(11.W))
  // Absolute x and y gradients
  val abs_gx = Wire(SInt(11.W))
  val abs_gy = Wire(SInt(11.W))
  val sum = Wire(SInt(11.W))

  // Horizontal mask
  gx := ((io.in(2).asSInt - io.in(0).asSInt) + ((io.in(5).asSInt - io.in(3).asSInt) * 2.U) + (io.in(8).asSInt - io.in(6).asSInt))
  // Vertical mask
  gy := ((io.in(0).asSInt - io.in(6).asSInt) + ((io.in(1).asSInt - io.in(7).asSInt) * 2.U) + (io.in(2).asSInt - io.in(8).asSInt))

  // Absolute values of both axes
  abs_gx := gx.abs
  abs_gy := gy.abs

  // Add both axes to find the combined value
  sum := abs_gx + abs_gy

  // Limit the max value to 255
  io.out := Mux(sum(10, 8).orR, "hff".U, sum(7, 0))
}