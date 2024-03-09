package chisel_image_processor

import chisel3._
import chisel3.util._

class RowBufferMemory(p: ImageProcessorParams) extends CustomModule(p) {
  val io = IO(new Bundle {
    // Read port
    val rEn = Input(Bool())
    val rAddr = Input(UInt(log2Ceil(p.numBatchesInRow).W))
    val rData = Output(HWPixelBatch())
    // Write port
    val wEn = Input(Bool())
    val wAddr = Input(UInt(log2Ceil(p.numBatchesInRow).W))
    val wData = Input(HWPixelBatch())
  })
  // Fixed memory latency of 1 cycle
  val mem = SyncReadMem(p.numBatchesInRow, Vec(p.batchSize, HWPixel()))
  io.rData := DontCare
  when (io.rEn) {
    io.rData := mem(io.rAddr)
  }
  when (io.wEn) {
    mem(io.wAddr) := io.wData
  }
}