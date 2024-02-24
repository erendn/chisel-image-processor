package chisel_image_processor

import chisel3._
import chiseltest._
import com.sksamuel.scrimage.filter.EdgeFilter
import org.scalatest.flatspec.AnyFlatSpec

//import com.sksamuel.scrimage.scala._

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

  def doTest(imageFile: String): Unit = {
    val image = ImageProcessorModel.readImage(imageFile)
    val p = ImageProcessorModel.getImageParams(image)
    val pixels = ImageProcessorModel.getImagePixels(image)
    test(new ImageProcessor(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // load input matrices
      dut.io.in.valid.poke(true.B)
      dut.io.in.ready.expect(true.B)
      // OK for dut.io.out.valid to be true with junk data
      for (r <- 0 until p.numRows) {
        for (c <- 0 until p.numCols) {
          for (i <- 0 until p.numChannels) {
            dut.io.in.bits.data(r)(c)(i).poke(pixels(r)(c)(i).U)
          }
        }
      }
      dut.clock.step()
      // check for completion & result
      dut.io.in.ready.expect(true.B)
      dut.io.out.valid.expect(true.B)
      for (r <- 0 until p.numRows) {
        for (c <- 0 until p.numCols) {
          for (i <- 0 until p.numChannels) {
            dut.io.out.bits(r)(c)(i).expect(pixels(r)(c)(i).U)
          }
        }
      }
    }
  }
  behavior of "ImageProcessor"
  it should "input and output" in {
    doTest("./src/test/images/sample.png")
  }
}
