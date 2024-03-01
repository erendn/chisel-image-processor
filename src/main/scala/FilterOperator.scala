package chisel_image_processor

import chisel3._
import chisel3.util._

// Parent class for all filter operators. Each child class must have a corresponding generator function in the
// FilterGenerators object to be passed to the ImageProcessor class instance.
abstract class FilterOperator(p: ImageProcessorParams, numKernelRows: Int, numKernelCols: Int) extends CustomModule(p) {
  val io = IO(new Bundle {
    val in = Input(Vec(numKernelRows * numKernelCols, HWPixel())) // Input of 3x3 pixels
    val out = Output(HWPixel()) // Output of the middle pixel
  })
}

object FilterGenerators {
  def sobelFilter(p: ImageProcessorParams): FilterOperator = {
    new SobelFilter(p)
  }
}

class SobelFilter(p: ImageProcessorParams) extends FilterOperator(p, 3, 3) {
  // x and y gradients (11 bits because max value of gx_w and gy_w is 255*4 and last bit for sign)
  val gx = Wire(Vec(p.numChannels, SInt(11.W)))
  val gy = Wire(Vec(p.numChannels, SInt(11.W)))
  // Absolute x and y gradients
  val abs_gx = Wire(Vec(p.numChannels, SInt(11.W)))
  val abs_gy = Wire(Vec(p.numChannels, SInt(11.W)))
  val sum = Wire(Vec(p.numChannels, SInt(11.W)))

  // Apply the filter separately for each channel
  for (i <- 0 until p.numChannels) {
    // Horizontal mask
    gx(i) := ((io.in(2)(i).zext -& io.in(0)(i).zext) +&
              (io.in(5)(i).zext -& io.in(3)(i).zext).do_<<(1) +&
              (io.in(8)(i).zext -& io.in(6)(i).zext))
    // Vertical mask
    gy(i) := ((io.in(0)(i).zext -& io.in(6)(i).zext) +&
              (io.in(1)(i).zext -& io.in(7)(i).zext).do_<<(1) +&
              (io.in(2)(i).zext -& io.in(8)(i).zext))
    // Absolute values of both axes
    abs_gx(i) := gx(i).abs
    abs_gy(i) := gy(i).abs
    // Add both axes to find the combined value
    sum(i) := abs_gx(i) + abs_gy(i)
    // Limit the max value to 255
    io.out(i) := Mux(sum(i)(10, 8).orR, 255.U, sum(i)(7, 0))
  }
}