package chisel_image_processor

import java.io.File
import com.sksamuel.scrimage.{ImmutableImage, MutableImage}
import com.sksamuel.scrimage.nio.PngWriter
import com.sksamuel.scrimage.filter.Filter
import com.sksamuel.scrimage.pixels.Pixel

object ImageProcessorModel {

  type PixelType = Seq[Int]
  type ImageType = Seq[Seq[PixelType]]

  def readImage(file: String): ImmutableImage = {
    ImmutableImage.loader().fromFile(file)
  }

  def applyFilter(image: ImmutableImage, filter: Filter): ImmutableImage = {
    image.filter(filter)
  }

  def writeImage(image: MutableImage, file: String): Unit = {
    val outputFile = new File(file)
    outputFile.getParentFile().mkdirs() // Create parent directories
    image.output(new PngWriter(), outputFile)
  }

  def writeImage(image: Array[Pixel], p: ImageProcessorParams, file: String): Unit = {
    val imageObject = ImmutableImage.create(p.imageWidth, p.imageHeight, image)
    this.writeImage(imageObject, file)
  }

  def getImageParams(image: ImmutableImage): ImageProcessorParams = {
    new ImageProcessorParams(image.width, image.height)
  }

  def getImagePixels(image: ImmutableImage): ImageType = {
    val pixelArray = Array.ofDim[PixelType](image.height, image.width)
    for (i <- 0 until image.height) {
      for (j <- 0 until image.width) {
        val pixel = image.pixel(j, i)
        pixelArray(i)(j) = Seq(pixel.red(), pixel.green(), pixel.blue())
      }
    }
    Seq.tabulate(image.height, image.width) { (i, j) => pixelArray(i)(j) }
  }

}