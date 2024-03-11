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
    ImageProcessorModel.writeImage(image, "./src/test/temp/sample_output_model.png")
  }
  it should "apply bump filter" in {
    val image = ImageProcessorModel.readImage("./src/test/images/sample.png")
    val filtered = ImageProcessorModel.applyFilter(image, new BumpFilter())
    ImageProcessorModel.writeImage(filtered, "./src/test/temp/sample_bump_output_model.png")
  }
  it should "apply blur filter" in {
    val image = ImageProcessorModel.readImage("./src/test/images/sample.png")
    val filtered = ImageProcessorModel.applyFilter(image, new BlurFilter())
    ImageProcessorModel.writeImage(filtered, "./src/test/temp/sample_blur_output_model.png")
  }
  it should "apply grayscale filter" in {
    val image = ImageProcessorModel.readImage("./src/test/images/sample.png")
    val filtered = ImageProcessorModel.applyFilter(image, new GrayscaleFilter())
    ImageProcessorModel.writeImage(filtered, "./src/test/temp/sample_grayscale_output_model.png")
  }
  it should "apply solarize filter" in {
    val image = ImageProcessorModel.readImage("./src/test/images/sample.png")
    val filtered = ImageProcessorModel.applyFilter(image, new SolarizeFilter())
    ImageProcessorModel.writeImage(filtered, "./src/test/temp/sample_solarize_output_model.png")
  }
  behavior of "ImageProcessor"
  testAllFilters(1)
  testAllFilters(2)
  testAllFilters(5)

  def testAllFilters(parallelism: Int): Unit = {
    it should s"apply bump filter (${parallelism}-pixel parallelism)" in {
      doTest(FilterGenerator.bumpFilter, new BumpFilter(), parallelism, "sample")
    }
    it should s"apply blur filter (${parallelism}-pixel parallelism)" in {
      doTest(FilterGenerator.blurFilter, new BlurFilter(), parallelism, "sample")
    }
    it should s"apply grayscale filter (${parallelism}-pixel parallelism)" in {
      doTest(FilterGenerator.grayscaleFilter, new GrayscaleFilter(), parallelism, "sample")
    }
    it should s"apply solarize filter (${parallelism}-pixel parallelism)" in {
      doTest(FilterGenerator.solarizeFilter, new SolarizeFilter(), parallelism, "sample")
    }
  }
  def assertWithTolerance(actual: Int, expected: Int): Unit = {
    val diff = (expected - actual).abs
    assert(diff < 2)
  }
  def doTest(filterName: String, libFilter: Filter, parallelism: Int, imageFile: String): Unit = {
    val inputFile = s"./src/test/images/${imageFile}.png"
    val outputFile = s"./src/test/temp/${imageFile}_${filterName}_output_${parallelism}.png"
    // Prepare the input image
    // Original image
    val image = ImageProcessorModel.readImage(inputFile)
    // Filtered image from the library
    val filteredImage = ImageProcessorModel.applyFilter(ImageProcessorModel.readImage(inputFile), libFilter)
    // Parameters for the image
    val p = ImageProcessorModel.getImageParams(image, parallelism)
    // Original image's pixels to pass to the processor
    val pixels = ImageProcessorModel.getImagePixels(image)
    // Filtered pixels from the processor
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
      for (batchIndex <- 0 until p.numRows * p.numBatchesInRow) {
        // Coordinate of the first pixel in the batch
        val fr: Int = batchIndex / p.numBatchesInRow
        val fc: Int = (batchIndex % p.numBatchesInRow) * p.batchSize
        dut.io.in.bits.row.poke(fr.U)
        dut.io.in.bits.col.poke(fc.U)
        // Pass all pixels in the batch
        for (batchCount <- 0 until p.batchSize) {
          // Coordinate of this pixel
          val r: Int = fr
          val c: Int = fc + batchCount
          // Pass all channels of this pixel
          for (channelIndex <- 0 until p.numChannels) {
            dut.io.in.bits.data(batchCount)(channelIndex).poke(pixels(r)(c)(channelIndex).U)
          }
        }
        // Save the output batch of the processor if there is one
        if (dut.io.out.valid.peekBoolean()) {
          // Coordinate of the first pixel in the batch
          val fx = dut.io.out.bits.col.peek().litValue.toInt
          val fy = dut.io.out.bits.row.peek().litValue.toInt
          for (batchCount <- 0 until p.batchSize) {
            // Coordinate of this pixel
            val x = fx + batchCount
            val y = fy
            val index = y * p.numCols + x
            val red = dut.io.out.bits.data(batchCount)(0).peek().litValue.toInt
            val green = dut.io.out.bits.data(batchCount)(1).peek().litValue.toInt
            val blue = dut.io.out.bits.data(batchCount)(2).peek().litValue.toInt
            outputPixels(index) = new Pixel(x, y, red, green, blue, 255)
          }
        }
        dut.clock.step()
      }
      // Wait for kernel processor outputs
      for (_ <- 0 until p.numBatchesInRow + 1) {
        if (dut.io.out.valid.peekBoolean()) {
          // Coordinate of the first pixel in the batch
          val fx = dut.io.out.bits.col.peek().litValue.toInt
          val fy = dut.io.out.bits.row.peek().litValue.toInt
          for (batchCount <- 0 until p.batchSize) {
            // Coordinate of this pixel
            val x = fx + batchCount
            val y = fy
            val index = y * p.numCols + x
            val red = dut.io.out.bits.data(batchCount)(0).peek().litValue.toInt
            val green = dut.io.out.bits.data(batchCount)(1).peek().litValue.toInt
            val blue = dut.io.out.bits.data(batchCount)(2).peek().litValue.toInt
            outputPixels(index) = new Pixel(x, y, red, green, blue, 255)
          }
        }
        dut.clock.step()
      }
      ImageProcessorModel.writeImage(outputPixels, p, outputFile)
      // Compare all pixels to the library's results
      for (r <- 0 until p.numRows) {
        for (c <- 0 until p.numCols) {
          val index = r * p.numCols + c
          assert(c == outputPixels(index).x)
          assert(r == outputPixels(index).y)
          assertWithTolerance(outputPixels(index).red(), filteredPixels(r)(c)(0))
          assertWithTolerance(outputPixels(index).green(), filteredPixels(r)(c)(1))
          assertWithTolerance(outputPixels(index).blue(), filteredPixels(r)(c)(2))
        }
      }
    }
  }
}
