package chisel_image_processor

import java.io.File
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.nio.PngWriter
import com.sksamuel.scrimage.filter.BufferedOpFilter


object ImageProcessorModel {

  def readImage(file: String): ImmutableImage = {
    ImmutableImage.loader().fromFile(file)
  }

  def applyFilter(image: ImmutableImage, filter: BufferedOpFilter): ImmutableImage = {
    image.filter(filter)
  }

  def writeImage(image: ImmutableImage, file: String): Unit = {
    image.output(new PngWriter(), new File(file))
  }

}