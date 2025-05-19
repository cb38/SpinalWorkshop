package workshop.cache

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.SpinalConfig
import spinal.core.sim.SimConfig
import workshop.common.WorkshopSimConfig
import scala.util.Random

import SimUtils._
//import workshop.cache.{InstructionCache, InstructionCacheConfig}

class  L1InstrSim extends AnyFunSuite {
  
  var compiled: SimCompiled[InstructionCache] = null
  
    val bytePerLine = 16

  test("compile") {
    // Compile the design
    print("Compiling L1InstCache")
    compiled = WorkshopSimConfig().compile (
    
      new InstructionCache(p = InstructionCacheConfig(
        cacheSize =256,
        bytePerLine =bytePerLine,
        wayCount = 1,
        wrappedMemAccess = true,
        addressWidth = 16,
        cpuDataWidth = 32,
        memDataWidth = 32))
    )
     
    
  }

  test("testbench") {
    // Run the simulation
    compiled.doSim(seed = 42) { dut =>
        dut.clockDomain.forkStimulus(10)
        SimTimeout(10*5000)
        // expose internal memory datas
        dut.ways(0).datas.simPublic()

        var addr = (Random.nextLong() & 0xFFFCl ) 
        val randinterval = Random.nextLong() & 0xFl
        printf("randinterval = %d\n", randinterval)
        var cnt = 0
        var ramLoaded = false
        val ramContent = Array.fill(64*1024)(Random.nextLong() & 0xFFFFFFFFl)
        //val ramContent = Array.range(0, 64*1024).map(i => i.toLong & 0xFFFFFFFFl)
        var transactionCounter = 0

        dut.io.mem.rsp.valid #= false
         // Fork monitor to answer the mem cmd bus with burst data
        onStreamFire(dut.io.mem.cmd, dut.clockDomain) {
            val addr = dut.io.mem.cmd.payload.address.toLong/4
            //assert(addr < ramContent.length, s"addr $addr out of range")
            for (i <- 0 until (bytePerLine/4)) {
                val data = ramContent((addr + i).toInt)
                dut.io.mem.rsp.payload.data #= data
                dut.io.mem.rsp.valid #= true
                printf("   -> mem cmd addr 0x%x data 0x%x\n", (addr+i)*4, data)
                dut.clockDomain.waitSampling()
            }
            dut.io.mem.rsp.valid #= false
            
            
            
         }

         //fork a driver to randomize the cpu cmd bus valid signal and payload signals
        streamMasterRandomizer(dut.io.cpu.cmd, dut.clockDomain) {
             if (cnt == randinterval) { 
               cnt =0  
               addr = (Random.nextLong() & 0xFFFCl ) 
             }
             dut.io.cpu.cmd.payload.address #= addr
            
        }
        // fork stream driver to randomize the cpu rsp bus ready signal
        streamSlaveRandomizer(dut.io.cpu.rsp, dut.clockDomain)
        // fork a monitor to check the cpu rsp bus payload signal
        onStreamFire(dut.io.cpu.rsp, dut.clockDomain) {
            printf("<- cpu rsp addr 0x%x data 0x%x\n", dut.io.cpu.rsp.payload.address.toLong, dut.io.cpu.rsp.payload.data.toLong)
            assert(dut.io.cpu.rsp.payload.data.toLong == ramContent((dut.io.cpu.rsp.payload.address.toLong/4).toInt ))    
            transactionCounter = transactionCounter + 1
        }
        // fork monitor to print the cpu cmd bus payload signal
        onStreamFire(dut.io.cpu.cmd, dut.clockDomain) {
            printf("-> cpu cmd addr 0x%x \n", dut.io.cpu.cmd.payload.address.toLong)
            addr = addr + 4 ; cnt +=1
           
        }

        // wait end of simulation
        waitUntil (transactionCounter > 10)
        //print value of internal datas memory 
        for (i <- 0 until (64)) {
         //  printf("data %d = 0x%x\n", i, dut.ways(0).datas.getBigInt(i).toLong) 
        }
        //print value of internal tags memory
        for (i <- 0 until (16)) {
         //  printf("tag %d = 0x%x\n", i, dut.ways(0).tags.getBigInt(i).toLong)
        }
        simSuccess()



    }
    
  }
    
  
}