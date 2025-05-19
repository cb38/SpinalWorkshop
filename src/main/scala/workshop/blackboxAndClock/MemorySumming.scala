package workshop.blackboxAndClock

import spinal.core._
import spinal.lib._


// Define a Ram as a BlackBox
case class Ram_1w_1r_2c(wordWidth: Int, addressWidth: Int, writeClock : ClockDomain, readClock : ClockDomain) extends BlackBox {
  // TODO define Generics
  addGenerics(("addressWidth", addressWidth), ("wordWidth", wordWidth))
  // TODO define IO
  val io = new Bundle {
    val wr = new Bundle {
        val clk  = in Bool()
        val en   = in Bool()
        val addr = in UInt(addressWidth bits)
        val data = in Bits(wordWidth bits)
    }

    val rd = new Bundle {
      val clk  = in Bool()
      val en  = in Bool()
      val addr = in UInt(addressWidth bits)
      val data = out Bits(wordWidth bits)
    }
  }
  // TODO define ClockDomains mappings
  mapClockDomain(writeClock,io.wr.clk)
  mapClockDomain(readClock,io.rd.clk)
}

// Create the top level and instanciate the Ram
case class MemorySumming(writeClock : ClockDomain,sumClock : ClockDomain) extends Component {
  val io = new Bundle {
    val wr = new Bundle {
      val en   = in Bool()
      val addr = in UInt (8 bits)
      val data = in Bits (16 bits)
    }

    val sum = new Bundle {
      val start = in Bool()
      val done  = out Bool()
      val value = out UInt(16 bits)
    }
  }

  // TODO define the ram
  val ram1 = new Ram_1w_1r_2c(16,8,writeClock,sumClock)
  // TODO connect the io.wr port to the ram
  io.wr.en <> ram1.io.wr.en
  io.wr.addr <> ram1.io.wr.addr
  io.wr.data <> ram1.io.wr.data

  val sumArea = new ClockingArea(sumClock) {
    // TODO define the memory read + summing logic
    val counter= Reg(UInt(8 bits)) init(0)
    val active = Reg(Bool) init(False)
    ram1.io.rd.addr := counter
    ram1.io.rd.en := active
    when(io.sum.start) {
      counter := 0
      active := True
    } elsewhen(active) {
      counter := counter + 1
      when(counter === counter.maxValue) {
        active := False
      }
    } 
    val ReadDataValid = RegNext(ram1.io.rd.en) init(False)
    val sum = Reg(UInt(16 bits)) init(0)
    when(ReadDataValid) { 
        sum := sum + ram1.io.rd.data.asUInt
    } otherwise {
      sum := 0
    }
    io.sum.value := sum
      io.sum.done := False
      when(ReadDataValid && counter === counter.maxValue) {
        io.sum.done := True
      }

  }
}
