package chisel_image_processor

import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest._
import com.sksamuel.scrimage.filter.EdgeFilter
import com.sksamuel.scrimage.pixels.Pixel

class ImageProcessorTester extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ImageProcessorModel"
    it should "read/write image file" in {
      val image = ImageProcessorModel.readImage("./src/test/images/sample.png")
      ImageProcessorModel.writeImage(image, "./src/test/temp/sample_model_output.png")
    }
    it should "apply filter" in {
      val image = ImageProcessorModel.readImage("./src/test/images/sample.png")
      val filtered = ImageProcessorModel.applyFilter(image, new EdgeFilter())
      ImageProcessorModel.writeImage(filtered, "./src/test/temp/sample_edge_model_output.png")
    }

  def doTest(inputFile: String, outputFile: String): Unit = {
    // Prepare the input image
    val image = ImageProcessorModel.readImage(inputFile)
    val filteredImage = ImageProcessorModel.applyFilter(ImageProcessorModel.readImage(inputFile), new EdgeFilter())
    val p = ImageProcessorModel.getImageParams(image)
    val pixels = ImageProcessorModel.getImagePixels(image)
    val filteredPixels = ImageProcessorModel.getImagePixels(filteredImage)
    // Begin the test
    test(new ImageProcessor(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Load the image
      dut.io.in.valid.poke(true.B)
      dut.io.in.ready.expect(true.B)
      dut.io.state.expect(ImageProcessorState.idle)
      // FIXME: Don't pass the entire image at once after streamlining
      for (r <- 0 until p.numRows) {
        for (c <- 0 until p.numCols) {
          for (i <- 0 until p.numChannels) {
            dut.io.in.bits.data(r)(c)(i).poke(pixels(r)(c)(i).U)
          }
        }
      }
      dut.clock.step()
      // Wait until the filter is applied
      dut.io.in.ready.expect(false.B)
      dut.io.out.valid.expect(false.B)
      dut.clock.step((p.numRows - 2) * (p.numCols - 2))
      // Check the output
      dut.io.in.ready.expect(false.B)
      dut.io.out.valid.expect(true.B)
      dut.io.state.expect(ImageProcessorState.done)
      // Dump the output as an image file first for debug purposes
      val outputPixels = Array.ofDim[Pixel](image.height * image.width)
      for (r <- 0 until p.numRows) {
        for (c <- 0 until p.numCols) {
          outputPixels(r * p.imageWidth + c) = new Pixel(r, c, // x and y
                                                         dut.io.out.bits(r)(c)(0).peek().litValue.toInt, // red
                                                         dut.io.out.bits(r)(c)(1).peek().litValue.toInt, // green
                                                         dut.io.out.bits(r)(c)(2).peek().litValue.toInt, // blue
                                                         255) // alpha
        }
      }
      ImageProcessorModel.writeImage(outputPixels, p, outputFile)
      // Compare the output with the expected filtered image
      for (r <- 1 until p.numRows - 1) {
        for (c <- 1 until p.numCols - 1) {
          for (i <- 0 until p.numChannels) {
            dut.io.out.bits(r)(c)(i).expect(pixels(r)(c)(i).U)
          }
        }
      }
    }
  }
  behavior of "ImageProcessor"
  it should "input and output" in {
    doTest("./src/test/images/sample.png", "./src/test/temp/sample_edge_output.png")
  }
}
