package workshop.cache


import spinal.core._
import spinal.lib._
import scala.math._
import spinal.lib.bus.simple._
import spinal.sim._
import spinal.core.sim._

case class DataCacheConfig(cacheSize : Int,
                           bytePerLine : Int,
                           wayCount : Int,
                           addressWidth : Int,
                           cpuDataWidth : Int,
                           var rfDataWidth : Int = -1, //-1 mean cpuDataWidth
                           memDataWidth : Int,
                          
                           tagSizeShift : Int = 0, //Used to force infering ram
                           pendingMax : Int = 64,
                           asyncTagMemory : Boolean = false){

  if(rfDataWidth == -1)  rfDataWidth = cpuDataWidth 

  assert(isPow2(pendingMax))
  assert(rfDataWidth <= memDataWidth)

  def lineCount = cacheSize/bytePerLine/wayCount
  def sizeMax = log2Up(bytePerLine)
  def sizeWidth = log2Up(sizeMax + 1)


  def burstSize = bytePerLine*8/memDataWidth
  val burstLength = bytePerLine/(cpuDataWidth/8)

  def cpuDataBytes = cpuDataWidth/8
  def rfDataBytes = rfDataWidth/8
  def memDataBytes = memDataWidth/8
 
}
case class DataCacheCpuCmd(p: DataCacheConfig) extends Bundle {
  val address = UInt(p.addressWidth bits)
  val wr = Bool()
  val size = UInt(log2Up(log2Up(p.cpuDataBytes)+1) bits)
  val storeData = Bits(p.cpuDataWidth bits)
  val isValid = Bool()
}

case class DataCacheCpuRsp(p: DataCacheConfig) extends Bundle {
  val data = Bits(p.cpuDataWidth bits)
 // val address = UInt(p.addressWidth bits)
  
  val error = Bool()
}


case class DataCacheFlush(lineCount : Int) extends Bundle{
  val singleLine = Bool()
  val lineId = UInt(log2Up(lineCount) bits)
}

case class DataCacheCpuBus(p : DataCacheConfig) extends Bundle with IMasterSlave{

  val cmd = slave(Stream(DataCacheCpuCmd(p)))
  val rsp = master(Stream(DataCacheCpuRsp(p)))
  val redo = Bool()
  val refilling = Bool()
  val flush = master(Stream(DataCacheFlush(p.lineCount)))

  

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
    master(flush)
    in(redo)
    in(refilling)
  }
}


case class DataCacheMemCmd(p : DataCacheConfig) extends Bundle{
  val wr = Bool()
  val uncached = Bool()
  val address = UInt(p.addressWidth bit)
  val data = Bits(p.cpuDataWidth bits)
  val mask = Bits(p.cpuDataWidth/8 bits)
  val size   = UInt(p.sizeWidth bits) //... 1 => 2 bytes ... 2 => 4 bytes ...
 
  val last = Bool

//  def beatCountMinusOne = size.muxListDc((0 to p.sizeMax).map(i => i -> U((1 << i)/p.memDataBytes)))
//  def beatCount = size.muxListDc((0 to p.sizeMax).map(i => i -> U((1 << i)/p.memDataBytes-1)))

  //Utilities which does quite a few assumtions about the bus utilisation
  def byteCountMinusOne = size.muxListDc((0 to p.sizeMax).map(i => i -> U((1 << i)-1, log2Up(p.bytePerLine) bits)))
  def beatCountMinusOne = (size === log2Up(p.bytePerLine)) ? U(p.burstSize-1) | U(0)
  def beatCount         = (size === log2Up(p.bytePerLine)) ? U(p.burstSize) | U(1)
  def isBurst           = size === log2Up(p.bytePerLine)
}
case class DataCacheMemRsp(p : DataCacheConfig) extends Bundle{
  
  val last = Bool()
  val data = Bits(p.memDataWidth bit)
  val error = Bool()

}


case class DataCacheMemBus(p : DataCacheConfig) extends Bundle with IMasterSlave{
  val cmd = Stream (DataCacheMemCmd(p))
  val rsp = Flow (DataCacheMemRsp(p))

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)

   
  }


}

 


class DataCache(val p : DataCacheConfig) extends Component{
  import p._

  val io = new Bundle{
    val cpu = slave(DataCacheCpuBus(p))
    val mem = master(DataCacheMemBus(p))
  }

  val haltCpu = False
  val lineWidth = bytePerLine*8
  val lineCount = cacheSize/bytePerLine
  val wordWidth = cpuDataWidth
  val wordWidthLog2 = log2Up(wordWidth)
  val wordPerLine = lineWidth/wordWidth
  val bytePerWord = wordWidth/8
  val wayLineCount = lineCount/wayCount
  val wayLineLog2 = log2Up(wayLineCount)
  val wayWordCount = wayLineCount * wordPerLine
  val memWordPerLine = lineWidth/memDataWidth
  val memTransactionPerLine = p.bytePerLine / (p.memDataWidth/8)
  val bytePerMemWord = memDataWidth/8
  val wayMemWordCount = wayLineCount * memWordPerLine

  val tagRange = addressWidth-1 downto log2Up(wayLineCount*bytePerLine)
  val lineRange = tagRange.low-1 downto log2Up(bytePerLine)
  val cpuWordRange = log2Up(bytePerLine)-1 downto log2Up(bytePerWord)
  val memWordRange = log2Up(bytePerLine)-1 downto log2Up(bytePerMemWord)
  val hitRange = tagRange.high downto lineRange.low
  val memWordToCpuWordRange = log2Up(bytePerMemWord)-1 downto log2Up(bytePerWord)
  val cpuWordToRfWordRange = log2Up(bytePerWord)-1 downto log2Up(p.rfDataBytes)

  // memory flow interfaces

  class LineInfo() extends Bundle{
    val valid, error = Bool()
    val address = UInt(tagRange.length bit)
  }

  val tagsReadCmd =  Flow(UInt(log2Up(wayLineCount) bits))
 
  val tagsWriteCmd = Flow(new Bundle{
    val way = Bits(wayCount bits)
    val address = UInt(log2Up(wayLineCount) bits)
    val data = new LineInfo()
  })

  val tagsWriteLastCmd = RegNext(tagsWriteCmd)

  val dataReadCmd =  Flow(UInt(log2Up(wayMemWordCount) bits))
  val dataWriteCmd = Flow(new Bundle{
    val way = Bits(wayCount bits)
    val address = UInt(log2Up(wayMemWordCount) bits)
    val data = Bits(memDataWidth bits)
    val mask = Bits(memDataWidth/8 bits)
  })


  val ways = for(i <- 0 until wayCount) yield new Area{
    val tags = Mem(new LineInfo(), wayLineCount).simPublic()
    val data = Mem(Bits(memDataWidth bit), wayMemWordCount).simPublic()

    //Reads
    val tagsReadRsp =  tags.readSync(tagsReadCmd.payload, tagsReadCmd.valid )
     
    val dataReadRspMem = data.readSync(dataReadCmd.payload, dataReadCmd.valid ) // ready ??
    val dataReadRspSel = io.cpu.cmd.address 
    val dataReadRsp = dataReadRspMem.subdivideIn(cpuDataWidth bits).read(dataReadRspSel(memWordToCpuWordRange))

    

    //Writes
    when(tagsWriteCmd.valid && tagsWriteCmd.way(i)){
      tags.write(tagsWriteCmd.address, tagsWriteCmd.data)
    }
    when(dataWriteCmd.valid && dataWriteCmd.way(i)){
      data.write(
        address = dataWriteCmd.address,
        data = dataWriteCmd.data,
        mask = dataWriteCmd.mask
      )
    }
  }


  tagsReadCmd.valid := False
  tagsReadCmd.payload.assignDontCare()
  dataReadCmd.valid := False
  dataReadCmd.payload.assignDontCare()
  tagsWriteCmd.valid := False
  tagsWriteCmd.payload.assignDontCare()
  dataWriteCmd.valid := False
  dataWriteCmd.payload.assignDontCare()

  when(io.cpu.cmd.valid ){
    tagsReadCmd.valid   := True
    dataReadCmd.valid   := True
    tagsReadCmd.payload := io.cpu.cmd.address(lineRange)
    dataReadCmd.payload := io.cpu.cmd.address(lineRange.high downto memWordRange.low)
  }
  
  // detect collision between read and write in the cache
  def collisionProcess(readAddress : UInt, readMask : Bits): Bits ={
    val ret = Bits(wayCount bits)
    val readAddressAligned = (readAddress >> log2Up(memDataWidth/cpuDataWidth))
    val dataWriteMaskAligned = dataWriteCmd.mask.subdivideIn(memDataWidth/cpuDataWidth slices).read(readAddress(log2Up(memDataWidth/cpuDataWidth)-1 downto 0))
    for(i <- 0 until wayCount){
      ret(i) := dataWriteCmd.valid && dataWriteCmd.way(i) && dataWriteCmd.address === readAddressAligned && (readMask & dataWriteMaskAligned) =/= 0
    }
    ret
  }

  // default values
  io.cpu.cmd.ready := True

  val rspSync = True
  val rspLast = True
  val memCmdSent = RegInit(False) setWhen (io.mem.cmd.fire) clearWhen (!io.cpu.cmd.valid)
  
  val refilldone = False

  val stageB = new Area {
    def ramPipe[T <: Data](that : T) =  CombInit(that) 


    val mask = io.cpu.cmd.size.muxListDc((0 to log2Up(p.cpuDataBytes)).map(i => U(i) -> B((1 << (1 << i)) -1, p.cpuDataBytes bits))) |<< io.cpu.cmd.address(log2Up(p.cpuDataBytes)-1 downto 0)

    val dataColisions = collisionProcess(io.cpu.cmd.address(lineRange.high downto cpuWordRange.low), mask)
    val wayInvalidate = B(0, wayCount bits) //Used if invalidate enabled
    val request = io.cpu.cmd
  
   
    val tagsReadRsp = ways.map(w => ramPipe(w.tagsReadRsp))
    val dataReadRsp = ways.map(w => ramPipe(w.dataReadRsp))
 
    val consistancyHazard =  False
 
    
    val waysHitsBeforeInvalidate =  B(tagsReadRsp.map(tag => io.cpu.cmd.address(tagRange) === tag.address && tag.valid).asBits())
    val waysHits = waysHitsBeforeInvalidate & ~wayInvalidate
    //val waysHits = ~wayInvalidate

    val waysHit = waysHits.orR
    val dataMux =  MuxOH(waysHits, dataReadRsp)
  

    //Loader interface
    val loaderValid = False

    val ioMemRspMuxed = io.mem.rsp.data.subdivideIn(cpuDataWidth bits).read(io.cpu.cmd.address(memWordToCpuWordRange))

    

    //Evict the cache after reset logics
    val flusher = new Area {
      val waitDone = RegInit(False) clearWhen(io.cpu.flush.ready)
      val hold = False
      val counter = Reg(UInt(lineRange.size + 1 bits)) init(0)
      when(!counter.msb) {
        tagsWriteCmd.valid := True
        tagsWriteCmd.address := counter.resized
        tagsWriteCmd.way.setAll()
        tagsWriteCmd.data.valid := False
        io.cpu.cmd.ready := False
        when(!hold) {
          counter := counter + 1
          when(io.cpu.flush.valid && io.cpu.flush.singleLine){
            counter.msb := True
          }
        }
      }

      io.cpu.flush.ready := waitDone && counter.msb

      val start = RegInit(True) //Used to relax timings
      start := !waitDone && !start && io.cpu.flush.valid && !io.cpu.cmd.valid   && !io.cpu.redo

      when(start){
        waitDone := True
        counter := 0
        when(io.cpu.flush.valid && io.cpu.flush.singleLine){
          counter := U"0" @@ io.cpu.flush.lineId
        }
      }
    }

    val requestDataBypass = CombInit(io.cpu.cmd.storeData)

    val cpuWriteToCache = False
    when(cpuWriteToCache){
      dataWriteCmd.valid setWhen(request.wr && waysHit)
      dataWriteCmd.address := io.cpu.cmd.address(lineRange.high downto memWordRange.low)
      dataWriteCmd.data.subdivideIn(cpuDataWidth bits).foreach(_ := requestDataBypass)
      dataWriteCmd.mask := 0
      dataWriteCmd.mask.subdivideIn(cpuDataWidth/8 bits).write(io.cpu.cmd.address(memWordToCpuWordRange), mask)
      dataWriteCmd.way := waysHits
    }

    val badPermissions = False
    val loadStoreFault = io.cpu.cmd.fire && ( badPermissions)
    
    io.cpu.redo := False
    
  
    
   

    io.mem.cmd.valid := False
    io.mem.cmd.address := io.cpu.cmd.address
    io.mem.cmd.last := True
    io.mem.cmd.wr := request.wr
    io.mem.cmd.mask := mask
    io.mem.cmd.data := requestDataBypass
    io.mem.cmd.uncached := False
    io.mem.cmd.size := request.size.resized
    


    val bypassCache =  False

    
    when(RegNext(io.cpu.cmd.valid)) {
      
        when(waysHit || request.wr) {   //Do not require a cache refill ?
          cpuWriteToCache := True

          //Write through
          io.mem.cmd.valid setWhen(request.wr)
          //.cpu.cmd.ready clearWhen(!request.wr || io.mem.cmd.ready)

          //On write to read dataColisions
          when((!request.wr ) && (dataColisions & waysHits) =/= 0){
            io.cpu.redo := True
            
          }
          
        } otherwise
           { //Do refill
          //Emit cmd
          io.mem.cmd.valid setWhen(!memCmdSent)
          io.mem.cmd.wr := False
          io.mem.cmd.address(0, lineRange.low bits) := 0
          io.mem.cmd.size := log2Up(p.bytePerLine)
          loaderValid setWhen(io.mem.cmd.ready) clearWhen((io.cpu.rsp.valid))
        
        }
      
    }

    when(bypassCache){
      io.cpu.rsp.data := ioMemRspMuxed
      def isLast =  True
      
    } otherwise {
      io.cpu.rsp.data := dataMux
      
    }

    

    //remove side effects on exceptions
    when(io.cpu.cmd.fire) {
      when(consistancyHazard) {
        io.mem.cmd.valid := False
        tagsWriteCmd.valid := False
        dataWriteCmd.valid := False
        loaderValid := False
        //io.cpu.rsp.valid := False
       
      }
      io.cpu.redo setWhen((consistancyHazard))
    }

  
   
  }

  val loader = new Area{
    val valid = RegInit(False) setWhen(stageB.loaderValid)
    val baseAddress =  io.cpu.cmd.address

    val counter = Counter(memTransactionPerLine)
    val waysAllocator = Reg(Bits(wayCount bits)) init(1)
    val error = RegInit(False)
    val kill = False
    val killReg = RegInit(False) setWhen(kill)

    when(valid && io.mem.rsp.valid && rspLast){
      dataWriteCmd.valid := True
      dataWriteCmd.address := baseAddress(lineRange) @@ counter
      dataWriteCmd.data := io.mem.rsp.data
      dataWriteCmd.mask.setAll()
      dataWriteCmd.way := waysAllocator
      error := error | io.mem.rsp.error
      counter.increment()
    }

    val done = CombInit(counter.willOverflow)
    

    when(done){
      valid := False

      //Update tags
      tagsWriteCmd.valid := True
      tagsWriteCmd.address := baseAddress(lineRange)
      tagsWriteCmd.data.valid := !(kill || killReg)
      tagsWriteCmd.data.address := baseAddress(tagRange)
      tagsWriteCmd.data.error := error || (io.mem.rsp.valid && io.mem.rsp.error)
      tagsWriteCmd.way := waysAllocator

      error := False
      killReg := False
    }

    when(!valid){
      waysAllocator := (waysAllocator ## waysAllocator.msb).resized
    }

    io.cpu.redo setWhen(valid.rise())
    io.cpu.refilling := valid
    io.cpu.rsp.valid := (done && valid) | (RegNext(io.cpu.cmd.valid) && stageB.waysHit)
    io.cpu.rsp.error := error
    
  }

 
}


class DataCacheTopLevel extends Component{
    implicit val p = DataCacheConfig(
      cacheSize = 4096,
      bytePerLine = 32,
      wayCount = 2,
      addressWidth = 32,
      cpuDataWidth = 32,
      memDataWidth = 32,
 
    )
     
    val io = new Bundle{
      val cpu = slave(DataCacheCpuBus(p))
      val mem = master(DataCacheMemBus(p))
     
    
    }
    val cache = new DataCache(p)

    io.cpu <> cache.io.cpu
    io.mem <> cache.io.mem
    cache.io.mem.cmd.payload.setPartialName("cmd")
    cache.io.mem.rsp.payload.setPartialName("rsp")
    
    cache.io.mem.cmd.setPartialName("cmd")
    cache.io.mem.rsp.setPartialName("rsp")
    
    cache.io.cpu.redo.setPartialName("redo")
    cache.io.cpu.flush.setPartialName("flush")


   
 
    

  }

object DataCacheVerilog{

  def main(args: Array[String]) {

      val config = SpinalConfig(
        targetDirectory = "rtl",
        netlistFileName = "DataCache.v",
        anonymSignalPrefix = "_temp",
        mergeAsyncProcess = true
  
        )
      
    config.generateVerilog(new DataCacheTopLevel() ).printPruned()
  }
}