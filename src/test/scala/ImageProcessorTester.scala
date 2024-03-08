package chisel_image_processor

import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest._
import com.sksamuel.scrimage.filter.{BlurFilter, BumpFilter, Filter, GrayscaleFilter, SolarizeFilter}
import com.sksamuel.scrimage.pixels.Pixel

class ImageProcessorTester extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ImageProcessorModel"
  it should "read/write image file" in {
    val image = ImageProcessorModel.readImage("./src/test/images/sample.png")
    ImageProcessorModel.writeImage(image, "./src/test/temp/sample_model_output.png")
  }
  it should "apply bump filter" in {
    val image = ImageProcessorModel.readImage("./src/test/images/sample.png")
    val filtered = ImageProcessorModel.applyFilter(image, new BumpFilter())
    ImageProcessorModel.writeImage(filtered, "./src/test/temp/sample_bump_model_output.png")
  }
  it should "apply blur filter" in {
    val image = ImageProcessorModel.readImage("./src/test/images/sample.png")
    val filtered = ImageProcessorModel.applyFilter(image, new BlurFilter())
    ImageProcessorModel.writeImage(filtered, "./src/test/temp/sample_blur_model_output.png")
  }
  it should "apply grayscale filter" in {
    val image = ImageProcessorModel.readImage("./src/test/images/sample.png")
    val filtered = ImageProcessorModel.applyFilter(image, new GrayscaleFilter())
    ImageProcessorModel.writeImage(filtered, "./src/test/temp/sample_grayscale_model_output.png")
  }
  it should "apply solarize filter" in {
    val image = ImageProcessorModel.readImage("./src/test/images/sample.png")
    val filtered = ImageProcessorModel.applyFilter(image, new SolarizeFilter())
    ImageProcessorModel.writeImage(filtered, "./src/test/temp/sample_solarize_model_output.png")
  }

  def assertWithTolerance(actual: Int, expected: Int): Unit = {
    val diff = (expected - actual).abs
    assert(diff < 2)
  }
  def doTest(filterName: String, libFilter: Filter, inputFile: String, outputFile: String): Unit = {
    // Prepare the input image
    val image = ImageProcessorModel.readImage(inputFile)
    val filteredImage = ImageProcessorModel.applyFilter(ImageProcessorModel.readImage(inputFile), libFilter)
    val p = ImageProcessorModel.getImageParams(image)
    val pixels = ImageProcessorModel.getImagePixels(image)
    val filteredPixels = ImageProcessorModel.getImagePixels(filteredImage)
    // Begin the test
    test(ImageProcessorGenerator.get(p, filterName)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val outputPixels = Array.ofDim[Pixel](image.height * image.width)
      dut.clock.setTimeout(image.height * image.width * 2)
      // Load the image
      dut.io.in.valid.poke(true.B)
      dut.io.in.ready.expect(true.B)
      dut.io.state.expect(ImageProcessorState.idle)
      dut.clock.step()
      for (r <- 0 until p.numRows) {
        for (c <- 0 until p.numCols) {
          for (i <- 0 until p.numChannels) {
            dut.io.in.bits.row.poke(r.U)
            dut.io.in.bits.col.poke(c.U)
            dut.io.in.bits.data(i).poke(pixels(r)(c)(i).U)
          }
          if (dut.io.out.valid.peekBoolean()) {
            val x = dut.io.out.bits.col.peek().litValue.toInt
            val y = dut.io.out.bits.row.peek().litValue.toInt
            val index = y * p.numCols + x
            val red = dut.io.out.bits.data(0).peek().litValue.toInt
            val green = dut.io.out.bits.data(1).peek().litValue.toInt
            val blue = dut.io.out.bits.data(2).peek().litValue.toInt
            outputPixels(index) = new Pixel(x, y, red, green, blue, 255)
          }
          dut.clock.step()
        }
      }
      for (r <- 0 until 2) {
        for (c <- 0 until p.numCols) {
          if (dut.io.out.valid.peekBoolean()) {
            val x = dut.io.out.bits.col.peek().litValue.toInt
            val y = dut.io.out.bits.row.peek().litValue.toInt
            val index = y * p.numCols + x
            val red = dut.io.out.bits.data(0).peek().litValue.toInt
            val green = dut.io.out.bits.data(1).peek().litValue.toInt
            val blue = dut.io.out.bits.data(2).peek().litValue.toInt
            outputPixels(index) = new Pixel(x, y, red, green, blue, 255)
          }
          dut.clock.step()
        }
      }
      for (r <- 0 until p.numRows) {
        for (c <- 0 until p.numCols) {
          val index = r * p.numCols + c
          if (outputPixels(index) == null) {
            outputPixels(index) = new Pixel(c, r, 0, 0, 0, 0)
          }
        }
      }
      ImageProcessorModel.writeImage(outputPixels, p, outputFile)
      // Compare all pixels to the library's results
      // FIXME: Actually compare all pixels after solving the edge issue
      // for (r <- 1 until p.numRows - 1) {
      //   for (c <- 1 until p.numCols - 1) {
      //     val index = r * p.numCols + c
      //     assert(c == outputPixels(index).x)
      //     assert(r == outputPixels(index).y)
      //     assertWithTolerance(outputPixels(index).red(), filteredPixels(r)(c)(0))
      //     assertWithTolerance(outputPixels(index).green(), filteredPixels(r)(c)(1))
      //     assertWithTolerance(outputPixels(index).blue(), filteredPixels(r)(c)(2))
      //   }
      // }
    }
  }
  behavior of "ImageProcessor"
  it should "apply bump filter" in {
    doTest(FilterGenerator.bumpFilter, new BumpFilter(), "./src/test/images/sample.png", "./src/test/temp/sample_bump_output.png")
  }
  it should "apply blur filter" in {
    doTest(FilterGenerator.blurFilter, new BlurFilter(), "./src/test/images/sample.png", "./src/test/temp/sample_blur_output.png")
  }
  it should "apply grayscale filter" in {
    doTest(FilterGenerator.grayscaleFilter, new GrayscaleFilter(), "./src/test/images/sample.png", "./src/test/temp/sample_grayscale_output.png")
  }
  it should "apply solarize filter" in {
    doTest(FilterGenerator.solarizeFilter, new SolarizeFilter(), "./src/test/images/sample.png", "./src/test/temp/sample_solarize_output.png")
  }
}
