package chisel_image_processor

import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest._
import com.sksamuel.scrimage.filter.{BufferedOpFilter, BumpFilter, EdgeFilter, GrayscaleFilter}
import com.sksamuel.scrimage.pixels.Pixel

class ImageProcessorTester extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ImageProcessorModel"
  it should "read/write image file" in {
    val image = ImageProcessorModel.readImage("./src/test/images/sample.png")
    ImageProcessorModel.writeImage(image, "./src/test/temp/sample_model_output.png")
  }
  it should "apply sobel filter" in {
    val image = ImageProcessorModel.readImage("./src/test/images/sample.png")
    val filtered = ImageProcessorModel.applyFilter(image, new EdgeFilter())
    ImageProcessorModel.writeImage(filtered, "./src/test/temp/sample_sobel_model_output.png")
  }
  it should "apply bump filter" in {
    val image = ImageProcessorModel.readImage("./src/test/images/sample.png")
    val filtered = ImageProcessorModel.applyFilter(image, new BumpFilter())
    ImageProcessorModel.writeImage(filtered, "./src/test/temp/sample_bump_model_output.png")
  }
  it should "apply gray filter" in {
    val image = ImageProcessorModel.readImage("./src/test/images/sample.png")
    val filtered = image.filter(new GrayscaleFilter())
    ImageProcessorModel.writeImage(filtered, "./src/test/temp/sample_gray_model_output.png")
  }

  def doTest(filterFunc: ImageProcessorParams => FilterOperator, libFilter: BufferedOpFilter, inputFile: String, outputFile: String): Unit = {
    // Prepare the input image
    val image = ImageProcessorModel.readImage(inputFile)
    val filteredImage = ImageProcessorModel.applyFilter(ImageProcessorModel.readImage(inputFile), libFilter)
    val p = ImageProcessorModel.getImageParams(image)
    val pixels = ImageProcessorModel.getImagePixels(image)
    val filteredPixels = ImageProcessorModel.getImagePixels(filteredImage)
    // Begin the test
    test(new ImageProcessor(p, filterFunc)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val outputPixels = Array.ofDim[Pixel](image.height * image.width)
      dut.clock.setTimeout(image.height * image.width * 2)
      // Load the image
      dut.io.in.valid.poke(true.B)
      dut.io.in.ready.expect(true.B)
      dut.io.state.expect(ImageProcessorState.idle)
      dut.clock.step()
      var count = 0
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
      for (r <- 0 until p.numRows) {
        for (c <- 0 until p.numCols) {
          val index = r * p.numCols + c
          if (outputPixels(index) == null) {
            outputPixels(index) = new Pixel(c, r, 0, 0, 0, 0)
          }
        }
      }
      ImageProcessorModel.writeImage(outputPixels, p, outputFile)
    }
  }
  behavior of "ImageProcessor"
  it should "apply sobel filter" in {
    doTest(FilterGenerators.sobelFilter, new EdgeFilter(), "./src/test/images/sample.png", "./src/test/temp/sample_sobel_output.png")
  }
  it should "apply bump filter" in {
    doTest(FilterGenerators.bumpFilter, new BumpFilter(), "./src/test/images/sample.png", "./src/test/temp/sample_bump_output.png")
  }
  behavior of "BasicImageProcessor"
  it should "apply gray filter (will fail due to output value not exact matched)" in {
    // This test doesn't pass due to the color value is not perfectly match (issue converting float and int)
    // The resulting output can be +1 or -1 of the expected value from library
    val image = ImageProcessorModel.readImage("./src/test/images/sample.png")
    val filteredImage = image.filter(new GrayscaleFilter())
    val p = ImageProcessorModel.getImageParams(image)
    val pixels = ImageProcessorModel.getImagePixels(image)
    val filteredPixels = ImageProcessorModel.getImagePixels(filteredImage)
    // sobelFilter is used as input since filterFunc is required
    // currently gray filter is directly under BasicImageProcessor
    test(new BasicImageProcessor(p, FilterGenerators.sobelFilter)) { dut =>
      for (n <- 0 until p.numRows) {
        for (m <- 0 until p.numCols) {
          for (c <- 0 until p.numChannels) {
            dut.io.in.bits.row.poke(n.U)
            dut.io.in.bits.col.poke(m.U)
            dut.io.in.bits.data(c).poke(pixels(n)(m)(c).U)
            dut.clock.step()
            if (c == 2) {
              for (i <- 0 until p.numChannels) {
                dut.io.out.bits.data(i).expect(filteredPixels(n)(m)(i).U)
              }
            }
          }
        }
      }
    }
  }
}
