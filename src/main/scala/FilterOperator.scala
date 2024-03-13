package chisel_image_processor

import chisel3._
import chisel3.util._

object FilterGenerator {
  val bumpFilter = "bump"
  val blurFilter = "blur"
  val grayscaleFilter = "grayscale"
  val solarizeFilter = "solarize"
  def get(p: ImageProcessorParams, name: String): FilterOperator = {
    if (name == bumpFilter) {
      return new HWBumpFilter(p)
    } else if (name == blurFilter) {
      return new HWBlurFilter(p)
    } else if (name == grayscaleFilter) {
      return new HWGrayscaleFilter(p)
    } else if (name == solarizeFilter) {
      return new HWSolarizeFilter(p)
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
  // Kernel is:
  // [ -1, -1,  0 ]
  // [ -1,  1,  1 ]
  // [  0,  1,  1 ]
  val kernel = Seq(-1.S, -1.S,  0.S,
                   -1.S,  1.S,  1.S,
                    0.S,  1.S,  1.S)
  for (channel <- 0 until p.numChannels) {
    val scaled = kernel.zipWithIndex.map{ case (x, i) => x * io.in(i)(channel) }
    val sum = scaled.reduce{ _ +& _ }
    io.out(channel) := Mux(sum(10), 0.U, Mux(sum(9, 8).orR, 255.U, sum(7, 0)))
  }
}

class HWBlurFilter(p: ImageProcessorParams) extends FilterOperator(p, 3, 3) {
  // Kernel is:
  // [ 1/14, 2/14, 1/14 ]
  // [ 2/14, 2/14, 2/14 ]
  // [ 1/14, 2/14, 1/14 ]
  val kernel = Seq(1.U, 2.U, 1.U,
                   2.U, 2.U, 2.U,
                   1.U, 2.U, 1.U)
  for (channel <- 0 until p.numChannels) {
    val scaled = kernel.zipWithIndex.map{ case (x, i) => x * io.in(i)(channel) }
    val sum = scaled.reduce{ _ +& _ }
    val roundedSum = (sum +& 7.U) / 14.U
    io.out(channel) := roundedSum
  }
}

class HWGrayscaleFilter(p: ImageProcessorParams) extends FilterOperator(p, 1, 1) {
  // gray = (red * 0.21) + (green * 0.71) + (blue * 0.07)
  val coefficients = Seq(21.U, 71.U, 7.U)
  val scaled = coefficients.zipWithIndex.map{ case (x, i) => x * io.in(0)(i) }
  val sum = scaled.reduce{ _ +& _ } / 100.U
  for (channel <- 0 until p.numChannels) {
    io.out(channel) := sum
  }
}

class HWSolarizeFilter(p: ImageProcessorParams) extends FilterOperator(p, 1, 1) {
  for (channel <- 0 until p.numChannels) {
    io.out(channel) := ((io.in(0)(channel) * 2.U).zext - 255.S).abs.asUInt
  }
}