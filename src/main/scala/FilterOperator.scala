package chisel_image_processor

import chisel3._
import chisel3.util._

object FilterGenerator {
  val bumpFilter = "bump"
  val blurFilter = "blur"
  val grayscaleFilter = "gray"
  def get(p: ImageProcessorParams, name: String): FilterOperator = {
    if (name == bumpFilter) {
      return new HWBumpFilter(p)
    }else if (name == blurFilter) {
      return new HWBlurFilter(p)
    } else if (name == grayscaleFilter) {
      return new HWGrayscaleFilter(p)
    }
    return null
  }
  def isKernelFilter(name: String): Boolean = {
    return Vector(bumpFilter, blurFilter).contains(name)
  }
}

// Parent class for all filter operators. Each child class must have a corresponding entry in the generator function in
// the FilterGenerators object to be used in the ImageProcessor class instance.
abstract class FilterOperator(p: ImageProcessorParams, rows: Int, cols: Int) extends CustomModule(p) {
  val numKernelRows: Int = rows
  val numKernelCols: Int = cols
  val numPixels: Int = rows * cols
  val isKernelFilter: Boolean = rows > 1 || cols > 1
  val io = IO(new Bundle {
    val in = Input(Vec(numPixels, HWPixel())) // Input of 3x3 pixels
    val out = Output(HWPixel()) // Output of the middle pixel
  })

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

class HWBlurFilter(p: ImageProcessorParams) extends FilterOperator(p, 3, 3) {
  // Scaled gradient can be 255*2 at maximum; therefore, it must be 9 bits
  val gScale = Wire(Vec(numPixels, Vec(p.numChannels, UInt(9.W))))
  // Gradient can be 255 at maximum; therefore, it must be 8 bits
  val g = Wire(Vec(numPixels, Vec(p.numChannels, UInt(8.W))))

  for (i <- 0 until p.numChannels) {
    // Kernel is:
    // [ 1/14, 2/14, 1/14 ]
    // [ 2/14, 2/14, 2/14 ]
    // [ 1/14, 2/14, 1/14 ]
    gScale(0)(i) := io.in(0)(i)
    gScale(1)(i) := io.in(1)(i) * 2.U
    gScale(2)(i) := io.in(2)(i)
    gScale(3)(i) := io.in(3)(i) * 2.U
    gScale(4)(i) := io.in(4)(i) * 2.U
    gScale(5)(i) := io.in(5)(i) * 2.U
    gScale(6)(i) := io.in(6)(i)
    gScale(7)(i) := io.in(7)(i) * 2.U
    gScale(8)(i) := io.in(8)(i)
    for (j <- 0 until 9) {
      g(j)(i) := gScale(j)(i) / 14.U
    }
    io.out(i) := g(0)(i) + g(1)(i) + g(2)(i) +
                 g(3)(i) + g(4)(i) + g(5)(i) +
                 g(6)(i) + g(7)(i) + g(8)(i)
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