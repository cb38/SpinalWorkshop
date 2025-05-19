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


class  DataSim extends AnyFunSuite {
  
  var compiled: SimCompiled[DataCache] = null
  
    val bytePerLine = 16

  test("compile") {
    // Compile the design
    print("Compiling DataCache")
  compiled = WorkshopSimConfig().compile(
    new DataCache(
      DataCacheConfig(
        cacheSize = 256,
        bytePerLine = bytePerLine,
        wayCount = 1,
        addressWidth = 16,

        cpuDataWidth = 32,
        memDataWidth = 32,
       
      )
    )
  )
     
    
  }

  test("testbench") {
    // Run the simulation
    compiled.doSim(seed = 42) { dut =>
        dut.clockDomain.forkStimulus(10)
        SimTimeout(10*500)


        //var addr = (Random.nextLong() & 0xFFFCl ) 
        var addr = 0x1234l

        //val randinterval = Random.nextLong() & 0x7l
        val randinterval = 4
        printf("randinterval = %d\n", randinterval)
        var cnt = 0
        var ramLoaded = false
        // ramcontent as seen by the cpu 
        //val ramContent = Array.fill(64*1024)(Random.nextLong() & 0xFFFFFFFFl)
        val ramContent = Array.range(0, 64*1024).map(i => 4*i.toLong & 0xFFFFFFFFl)
        // ddrcontent as seen by the mem bus
        val ddrContent = ramContent.map(i => i & 0xFFFFFFFFl)
        var transactionCounter = 0
       
        // set flush to default value
       
        dut.io.cpu.flush.valid #= false
        dut.io.cpu.cmd.wr #= false

        /// wait  for flush ready signal to be high , end of cache initialization
        waitUntil (dut.io.cpu.flush.ready.toBoolean)
        
    
         // Fork monitor to answer the mem cmd bus with burst data
        onStreamFire(dut.io.mem.cmd, dut.clockDomain) {
            val addr = dut.io.mem.cmd.payload.address.toLong/4
            
            // if write command, write data to the ddrContent
            if (dut.io.mem.cmd.payload.wr.toBoolean) {
                for (i <- 0 until (bytePerLine/4)) {
                    val data = dut.io.mem.cmd.payload.data.toLong
                    ddrContent((addr + i).toInt) = data
                    printf("   -> mem cmd wr addr 0x%x  -> ddr data 0x%x\n", (addr+i)*4, data)
                }
            } else { // if read command, send data to the cpu
               
                for (i <- 0 until (bytePerLine/4)) {
                    val data = ddrContent((addr + i).toInt)
                    dut.io.mem.rsp.payload.data #= data
                    dut.io.mem.rsp.valid #= true
                    printf("   -> mem cmd rd addr 0x%x  <- ddr data 0x%x\n", (addr+i)*4, data)
                    dut.clockDomain.waitSampling()
                }
             }
            dut.io.mem.rsp.valid #= false
            
            
            
         }
        var write = false
         //fork a driver to randomize the cpu cmd bus valid signal and payload signals
        fork {
             while(true) {
              dut.io.cpu.cmd.wr #= write
              dut.io.cpu.cmd.valid #= true 
              dut.io.cpu.cmd.payload.address #= addr
              dut.clockDomain.waitSampling()
              dut.io.cpu.cmd.valid #= false 
             
              var redo = false
              
              // wait until rsp is valid
              while (!dut.io.cpu.rsp.valid.toBoolean)  {
              
                dut.clockDomain.waitSampling()
                if(!redo) redo = dut.io.cpu.redo.toBoolean
                
              }    
              if (!redo)
              {
                
                if (cnt == randinterval) { 
                  cnt =0  
                  //addr = (Random.nextLong() & 0xFFFCl ) 
                  addr = 0x1234l
                  write = true
                  // if the command is a write command, write data to the ramContent     
                  //val data = Random.nextLong() & 0xFFFFFFFFl
                  val data = 0xDECAFEEDl
                  ramContent((addr/4).toInt) = data
                  dut.io.cpu.cmd.storeData #= data
                  printf("-> cpu cmd wr addr 0x%x  -> ram data 0x%x\n", (addr)*4, data)
                  
                }
                else { cnt += 1 
                addr = addr + 4 }

              
                
              } else {
                // redo is asserted, so we need to keep the same address
                addr = addr
              }
              
            }
            
        }
        // fork stream driver to randomize the cpu rsp bus ready signal
        //streamSlaveRandomizer(dut.io.cpu.rsp, dut.clockDomain)
        dut.io.cpu.rsp.ready #= true
       
        // fork strem driver to randomize the mem cmd bus ready signal
        //streamSlaveRandomizer(dut.io.mem.cmd, dut.clockDomain)
        dut.io.mem.cmd.ready #= true

        // fork a monitor to check the cpu rsp bus payload signal
        onStreamFire(dut.io.cpu.rsp, dut.clockDomain) {
           if (!dut.io.cpu.rsp.error.toBoolean) printf("<- cpu rsp  data 0x%x\n",  dut.io.cpu.rsp.payload.data.toLong)
           else printf("<- cpu rsp error, retry\n")
           // update ramContent with the data received from the cache
           //ramContent((dut.io.cpu.rsp.address.toLong/4).toInt) = dut.io.cpu.rsp.payload.data.toLong
           
        }
        // fork monitor to print the cpu cmd bus payload signal
        onStreamFire(dut.io.cpu.cmd, dut.clockDomain) {
            // if the command is a write command, print the data
            if (dut.io.cpu.cmd.payload.wr.toBoolean) {                
              printf("->cpu cmd wr addr 0x%x  -> data 0x%x\n", dut.io.cpu.cmd.payload.address.toLong, dut.io.cpu.cmd.address.toLong)
            } else { // if the command is a read command, print the address
              printf("->cpu cmd rd addr 0x%x\n", dut.io.cpu.cmd.payload.address.toLong)
            }
            transactionCounter = transactionCounter + 1
            
            
           
        }

        // wait end of simulation
        waitUntil (transactionCounter > 10)
      
        // flush cache to ddr 
        dut.io.cpu.flush.valid #= true
        dut.io.cpu.flush.payload.lineId #= 4

        // wait for flush to finish
        waitUntil (dut.io.cpu.flush.ready.toBoolean)

        // now check the ddr and ram content are same 
        for (i <- 0 until (64*1024)) {
            if (ddrContent(i) != ramContent(i)) {
                printf("Error: ddrContent(0x%x) = 0x%x != ramContent(0x%x) = 0x%x\n", 4*i, ddrContent(i), 4*i, ramContent(i))
            }
        }
        //print value of internal cache data memory 
        for (i <- 0 until (16)) {
           printf("data %d = 0x%x 0x%x 0x%x 0x%x \n", i, dut.ways(0).data.getBigInt(4*i).toLong,dut.ways(0).data.getBigInt(4*i+1).toLong,
                                                                        dut.ways(0).data.getBigInt(4*i+2).toLong,dut.ways(0).data.getBigInt(4*i+3).toLong) 
        }
        //print value of internal tags memory
        for (i <- 0 until (16)) {
           printf("tag %d = 0x%x\n", i, dut.ways(0).tags.getBigInt(i).toLong)
        }
        simSuccess()



    }
    
  }
  
}