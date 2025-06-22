
import spinal.core._

object Control extends SpinalEnum(binarySequential) {
  val REG_STUFF, FIXED_STUFF, BOTH, NONE = newElement()
}

case class DestuffReg() extends Bundle {
  val valid   = Bool()
  val detect  = Bool()
  val bypass  = Bool()
  val counter = UInt(3 bits)

  def clear(): Unit = {
    valid := False
    detect := False
    bypass := False
    counter := 0
  }
}

class StuffBits extends Component {
  val io = new Bundle {
    val i_ctrl        = in Bits(2 bits)
    val i_valid       = in Bool()
    val i_can_rx      = in Bool()
    val o_data_valid  = out Bool()
    val o_stuff_detect = out Bool()
    val o_fsb_detect   = out Bool()
    val o_stuffbit     = out Bool()
  }

  val r_std  = Reg(DestuffReg()).init(DestuffReg().getZero)
  val r_fsb  = Reg(DestuffReg()).init(DestuffReg().getZero)
  val r_last = Reg(Bool()).init(False)

  val control = Control()
  control := io.i_ctrl.as(Control())

  when(ClockDomain.current.readResetWire === True) {
    r_std.clear()
  } otherwise {
    r_std.valid  := False
    r_std.detect := False

    when(control === Control.REG_STUFF || control === Control.BOTH) {
      when(io.i_valid) {
        r_std.valid := True

        when(r_std.bypass) {
          r_std.bypass := False
          r_std.valid  := False
          r_std.detect := True
        } elsewhen(io.i_can_rx =/= r_last) {
          r_std.counter := 0
        } otherwise {
          r_std.counter := r_std.counter + 1
          when(r_std.counter === 3) {
            r_std.bypass := True
            r_std.counter := 0
          }
        }
      }
    } otherwise {
      r_std.clear()
    }
  }

  when(ClockDomain.current.readResetWire === True) {
    r_fsb.clear()
  } otherwise {
    r_fsb.valid  := False
    r_fsb.detect := False

    when(control === Control.FIXED_STUFF || control === Control.BOTH) {
      when(io.i_valid) {
        r_fsb.valid := True
        r_fsb.counter := r_fsb.counter + 1

        when(r_fsb.counter === 0) {
          r_fsb.detect := True
        }
        when(r_fsb.counter === 4) {
          r_fsb.counter := 0
        }
      }
    } otherwise {
      r_fsb.clear()
    }
  }

  when(io.i_valid) {
    r_last := io.i_can_rx
  }

  io.o_data_valid  := (!r_fsb.detect && !r_std.detect) && (r_fsb.valid || r_std.valid)
  io.o_stuff_detect := r_std.detect
  io.o_fsb_detect   := r_fsb.detect
  io.o_stuffbit     := ~r_last
}

// object StuffBitsVerilog {
//   def main(args: Array[String]): Unit = {
//     SpinalVerilog(new StuffBits)
//   }
// }