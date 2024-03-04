package chisel_image_processor

import chisel3._
import chisel3.util._

object FilterGenerator {
  val sobelFilter = "sobel"
  val bumpFilter = "bump"
  val grayscaleFilter = "gray"
  def get(p: ImageProcessorParams, name: String): FilterOperator = {
    if (name == sobelFilter) {
      return new HWSobelFilter(p)
    } else if (name == bumpFilter) {
      return new HWBumpFilter(p)
    } else if (name == grayscaleFilter) {
      return new HWGrayscaleFilter(p)
    }
    return null
  }
  def isKernelFilter(name: String): Boolean = {
    return Vector(sobelFilter, bumpFilter).contains(name)
  }
}

// Parent class for all filter operators. Each child class must have a corresponding entry in the generator function in
// the FilterGenerators object to be used in the ImageProcessor class instance.
abstract class FilterOperator(p: ImageProcessorParams, rows: Int, cols: Int) extends CustomModule(p) {
  val io = IO(new Bundle {
    val in = Input(Vec(rows * cols, HWPixel())) // Input of 3x3 pixels
    val out = Output(HWPixel()) // Output of the middle pixel
  })
  val numKernelRows: Int = rows
  val numKernelCols: Int = cols
  val isKernelFilter: Boolean = rows > 1 || cols > 1
}

class HWSobelFilter(p: ImageProcessorParams) extends FilterOperator(p, 3, 3) {
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
    // [  1,  0, -1 ]
    // [  2,  0, -2 ]
    // [  1,  0, -1 ]
    gx(i) := ((io.in(2)(i).zext -& io.in(0)(i).zext) +&
              (io.in(5)(i).zext -& io.in(3)(i).zext).do_<<(1) +&
              (io.in(8)(i).zext -& io.in(6)(i).zext))
    // Vertical mask
    // [  1,  2,  1 ]
    // [  0,  0,  0 ]
    // [ -1, -2, -1 ]
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

class HWBumpFilter(p: ImageProcessorParams) extends FilterOperator(p, 3, 3) {
  // Sum can be 255*4 at maximum; therefore, it must be signed 11 bits
  val sum = Wire(Vec(p.numChannels, SInt(11.W)))

  // Apply the filter separately for each channel
  for (i <- 0 until p.numChannels) {
    // Kernel is:
    // [ -1, -1,  0 ]
    // [ -1,  1,  1 ]
    // [  0,  1,  1 ]
    sum(i) := io.in(4)(i).zext +& io.in(5)(i).zext +& io.in(7)(i).zext +& io.in(8)(i).zext -&
              io.in(0)(i).zext -& io.in(1)(i).zext -& io.in(3)(i).zext
    // Clamp negative to 0, overflow to 255
    io.out(i) := Mux(sum(i)(10), 0.U, Mux(sum(i)(9, 8).orR, 255.U, sum(i)(7, 0)))
  }
}

class HWGrayscaleFilter(p: ImageProcessorParams) extends FilterOperator(p, 1, 1) {
  // gray = (red * 0.21) + (green * 0.71) + (blue * 0.07)
  val rScale = io.in(0)(0) * 21.U
  val gScale = io.in(0)(1) * 71.U
  val bScale = io.in(0)(2) * 7.U
  val sum = (rScale +& gScale +& bScale) / 100.U
  io.out(0) := sum
  io.out(1) := sum
  io.out(2) := sum
}