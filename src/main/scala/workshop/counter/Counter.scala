package workshop.counter

import spinal.core._

case class Counter(width: Int) extends Component {
  val io = new Bundle {
    val clear    = in  Bool()
    val value    = out UInt(width bits)
    val full     = out Bool()
  }

  
  val reg1 = Reg(UInt(width bits))  init(0)
 
  when (io.clear) {
     reg1 := 0; 
  } otherwise { 
     reg1 := reg1+1
  
  }  
  io.full := (reg1===reg1.maxValue) 
  io.value := reg1 
  
  
}
