package workshop.pinsec

import spinal.core._
import spinal.core.fiber._

import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink._
import spinal.lib.fsm.{State, StateDelay, StateMachine}
import spinal.lib.misc.Plru
import spinal.lib.pipeline._
import spinal.lib.bus.tilelink.coherent._
import spinal.lib.bus.amba4.axi._
import scala.collection.mutable.ArrayBuffer
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink





class AXIL2Cache(val p : CacheParam) extends Component {

}



// tests

object Axi4BridgeGen extends App{
  SpinalVerilog(new Axi4Bridge(
    new M2sParameters(
      addressWidth = 32,
      dataWidth = 32,
      masters = List.fill(2)(
        M2sAgent(
          name = null,
          M2sSource(
            id = SizeMapping(0, 16),
            emits = M2sTransfers(
              get = SizeRange.upTo(64),
              putFull = SizeRange.upTo(64)
            )
          )
        )
      )
    ).toNodeParameters()
  ).setDefinitionName("rtl/Axi4BridgeGen"))
}





object Axi4ToTilelinkFullGen extends App{
  SpinalVerilog(new Axi4ReadOnlyToTilelinkFull(
    Axi4Config(16, 32, 4),
    64,
    4
  ).setDefinitionName("rtl/Axi4ReadOnlyToTilelinkFull"))
  SpinalVerilog(new Axi4WriteOnlyToTilelinkFull(
    Axi4Config(16, 32, 4),
    64,
    4
  ).setDefinitionName("rtl/Axi4WriteOnlyToTileLinkFull"))

 
}


class Axi4ToTilelinkFull(config: Axi4Config,
                        bytesMax : Int,
                        slotsCount : Int) extends Component{
 val dp = Axi4ReadOnlyToTilelink.getTilelinkProposal(config, bytesMax)
 val io = new Bundle {
   val up = slave port Axi4(config)
   val down = master port Bus(M2sParameters(dp, 1 << config.idWidth))
 }

  val bridge = new Axi4ToTilelinkFiber(64, 4)
  bridge.up load io.up.pipelined(ar = StreamPipe.HALF, aw = StreamPipe.HALF, w = StreamPipe.FULL, b = StreamPipe.HALF, r = StreamPipe.FULL)
  bridge.down.setDownConnection(a = StreamPipe.FULL)

  io.down << bridge.down.bus

  Fiber build {
        bridge.read.get
        bridge.write.get
        
      }
 
}






object Axi4ToTilelinkFullGen2 extends App{
  SpinalVerilog(new Axi4ToTilelinkFull(
    Axi4Config(16, 32, 2),
    64,
    4
  ).setDefinitionName("rtl/Axi4ToTilelink"))

 
}


object AXIL2CacheMain extends App{
  def basicConfig(generalSlotCount : Int = 8,
                  downPendingMax: Int = 16,
                  masterPerChannel: Int = 4,
                  dataWidth: Int = 64,
                  addressWidth: Int = 32,
                  lockSets: Int = 64*1024/64,
                  cacheBytes : Int = 64*1024,
                  cacheWays : Int = 8) = {
    val blockSize = 64
    CacheParam(
      unp = NodeParameters(
        m = M2sParameters(
          addressWidth = addressWidth,
          dataWidth = dataWidth,
          masters = List.tabulate(masterPerChannel)(mId =>
            M2sAgent(
              name = null,
              mapping = List.fill(1)(M2sSource(
                emits = M2sTransfers(
                  get = SizeRange(64),
                  putFull = SizeRange(64),
                  putPartial = SizeRange(64),
                  //acquireT = SizeRange(64),
                  //acquireB = SizeRange(64)
                ),
                id = SizeMapping(mId * 4, 4)
              ))
            )
          )
        ),
        s = S2mParameters(List(
          S2mAgent(
            name = null,
            emits = S2mTransfers(
            ),
            sinkId = SizeMapping(0, generalSlotCount)
          )
        ))
      ),

      cacheWays = cacheWays,
      cacheBytes = cacheBytes,
      aBufferCount = 4,
      downPendingMax = downPendingMax,
      blockSize = blockSize,
      generalSlotCount = generalSlotCount,
      allocateOnMiss = (_,_,_,_,_) => True
    )
  }

  SpinalVerilog(new AXIL2Cache(basicConfig()).setDefinitionName("rtl/AXIL2Cache")).printPruned()

/*
  import spinal.lib.eda.bench._

  val rtls = ArrayBuffer[Rtl]()
  for (probeCount <- List(2)) { //Rtl.ffIo
    rtls += Rtl(SpinalVerilog((new L2Cache(basicConfig(dataWidth = 16, addressWidth = 32, cacheWays = 4,cacheBytes = 128*1024)).setDefinitionName(s"Hub$probeCount"))))
  }
  val targets = XilinxStdTargets().take(2)

  Bench(rtls, targets)

  */

}

/*
tricky cases :
- release while a probe is going on
- release data just before victim probe logic is enabled => think data are still in the victim buffer, while is already written to memory by release data
- acquire T then release data before the victim of the acquire got time to read the $ and get overriden by release data
 */
