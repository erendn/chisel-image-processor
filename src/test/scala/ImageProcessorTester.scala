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
      ImageProcessorModel.writeImage(image, "./src/test/temp/sample_edge_model_output.png")
    }
}
