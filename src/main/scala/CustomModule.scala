package chisel_image_processor

import chisel3._

// Custom parent module to include common stuff
class CustomModule(p: ImageProcessorParams) extends Module {
  // Type alias to make it easy to declare
  def HWPixel(): Vec[UInt] = Vec(p.numChannels, UInt(8.W))
  // Empty pixel value for a HWPixel
  val emptyPixel = VecInit(Seq.fill(p.numChannels)(0.U(8.W)))
}
