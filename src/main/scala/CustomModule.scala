package chisel_image_processor

import chisel3._

// Custom parent module to include common stuff
abstract class CustomModule(p: ImageProcessorParams) extends Module {
  // Type aliases to make them easy to declare
  def HWPixel(): Vec[UInt] = Vec(p.numChannels, UInt(8.W))
  def HWPixelBatch(): Vec[Vec[UInt]] = Vec(p.batchSize, HWPixel())
  // Empty pixel value for a HWPixel
  val emptyPixel = VecInit(Seq.fill(p.numChannels)(0.U(8.W)))
  // Empty pixel batch for HWPixels
  val emptyBatch = VecInit(Seq.fill(p.batchSize)(emptyPixel))
}
