package testchipip.cosim

import chisel3._
import chisel3.util._
 
import freechips.rocketchip.subsystem._
import org.chipsalliance.cde.config.{Field, Config, Parameters}
import org.chipsalliance.diplomacy.nodes.{HeterogeneousBag}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink.{TLRAM}
import freechips.rocketchip.rocket.{TraceDoctor}
import freechips.rocketchip.util._
import freechips.rocketchip.tile.{BaseTile, TraceBundle}
import freechips.rocketchip.diplomacy.{BundleBridgeSource, BundleBroadcast, BundleBridgeNexusNode}

//**********************************************
// Archive
//**********************************************


// A per-tile interface that includes the tile's clock and reset, move to seperate file


// The IO matched on by the TraceDoctor bridge: a wrapper around a heterogenous
// bag of TileTraceDoctorIO. Each entry is trace associated with a single tile



// object TraceDoctorOutputTop {
//   def apply(proto: Seq[TraceDoctor]): TraceDoctorOutputTop =
//     new TraceDoctorOutputTop(proto.map(t => t.traceWidth))
// }

// Redo: 
// trait CanHaveTraceDoctorIO { this: HasTiles =>
//   val module: CanHaveTraceDoctorIOModuleImp
//   // Bind all the trace nodes to a BB; we'll use this to generate the IO in the imp
//   val traceDoctorNexus = BundleBridgeNexusNode[TraceDoctor]()
//   tiles.foreach { traceDoctorNexus := _.traceDoctorNode }
// }


// A per-tile interface that includes the tile's clock and reset
class TileTraceDoctorIO(_traceType: TraceDoctor) extends Bundle {
  val clock = Clock()
  val reset = Bool()
  val data = _traceType.cloneType
  def getTraceWidth() : Int = _traceType.bits.size
}


class TraceDoctorOutputTop(traceDoctors: Seq[TraceDoctor]) extends Bundle {
  val tracedoctors: HeterogeneousBag[TileTraceDoctorIO] = Output(HeterogeneousBag.apply(traceDoctors.map(t => new TileTraceDoctorIO(t))))
}

//**********************************************
// Trace IO Key/Traits:
// Used to enable/add the tport on the top level
//**********************************************

case class TraceDoctorPortParams(print: Boolean = false)

object TraceDoctorPortKey extends Field[Option[TraceDoctorPortParams]](None)


// Everything works up until here: 
trait CanHaveTraceDoctorIO { this: HasHierarchicalElementsRootContext with InstantiatesHierarchicalElements =>
  implicit val p: Parameters

  val tileTraceDoctorNodes = traceDoctorNodes.values

  val traceDoctorIO = InModuleBody { p(TraceDoctorPortKey) map ( traceParams => {
    // val traceDoctorSeq = (outer.traceDoctorNexus.in.map(_._1))
    val tileTraceDoctors = tileTraceDoctorNodes.map(_.in(0)._1).toSeq
    val tdio = IO(Output(new TraceDoctorOutputTop (tileTraceDoctors)))

    (tdio.tracedoctors zip (tile_prci_domains.values zip tileTraceDoctors)).foreach { case (port, (prci, tracedoc)) =>
      port.clock := prci.module.clock
      port.reset := prci.module.reset.asBool
      port.data := tracedoc
    }


    if (traceParams.print) {
      for ((trace, idx) <- tdio.tracedoctors.zipWithIndex ) {
        withClockAndReset(trace.clock, trace.reset) {
          when (trace.data.valid) {
            printf(s"TraceDoctor $idx: %x\n", trace.data.bits.asUInt)
          }
        }
      }
    }

    tdio
  })}

}


