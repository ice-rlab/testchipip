package testchipip

import org.chipsalliance.cde.config.{Field, Parameters}
import chisel3._
import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._
import freechips.rocketchip.diplomacy.LazyRawModuleImp
import org.chipsalliance.diplomacy.bundlebridge.BundleBridgeNexusNode
import freechips.rocketchip.rocket.TraceDoctor
import freechips.rocketchip.subsystem.{BaseSubsystem, InstantiatesHierarchicalElements, HasHierarchicalElementsRootContext}
import org.chipsalliance.diplomacy.nodes.HeterogeneousBag

// A per-tile interface that includes the tile's clock and reset
class TileTraceDoctorIO(_traceDoctorType: TraceDoctor) extends Bundle {
  val clock: Clock = Clock()
  val reset: Bool = Bool()
  val bits = Vec(traceWidth, Bool())
  val valid: Bool = Bool()
  //val tracerVTrigger: Bool = Bool()
  val trace = traceType.cloneType
  def traceType = _traceDoctorType
  def traceWidth = traceType.traceWidth
}

// The IO matched on by the TraceDoctor bridge: a wrapper around a heterogenous
// bag of TileTraceDoctorIO. Each entry is trace associated with a single tile

class TraceDoctorOutputTop(traces: Seq[TraceDoctor]) extends Bundle {
  val tracedoctors = Output(org.chipsalliance.diplomacy.nodes.HeterogeneousBag.apply(traces.map(t => new TileTraceDoctorIO(t))))
  val traceWidths = traces.map(t => t.traceWidth)
}

object TraceDoctorOutputTop {
  def apply(proto: Seq[TraceDoctor]): TraceDoctorOutputTop =
    new TraceDoctorOutputTop(proto.map(t => {
      println(s"MG TraceDoctor width: ${t.traceWidth}\n");
      t
    }))
}

case class TraceDoctorPortParams(print: Boolean = false)
object TraceDoctorPortKey extends Field[Option[TraceDoctorPortParams]](None)

trait CanHaveTraceDoctorIO { this: HasHierarchicalElementsRootContext with InstantiatesHierarchicalElements =>
  implicit val p: Parameters

  val tileTraceDoctorNodes = traceDoctorNodes.values

  val traceDoctorIO = InModuleBody { p(TraceDoctorPortKey) map ( traceParams => {
    println(s"MG tileTraceDoctorNodes length is ${tileTraceDoctorNodes.size}")
    val tileTraceDoctors = tileTraceDoctorNodes.map(_.in(0)._1).toSeq
    println(s"MG tileTraceDoctors length is ${tileTraceDoctors.size}")
    //val tileTraceDoctors = (outer.traceDoctorNexus.in.map(_._1))
    val tdio = IO(Output(new TraceDoctorOutputTop(tileTraceDoctors)))

    (tdio.tracedoctors zip (tile_prci_domains.values zip tileTraceDoctorNodes)).foreach { case (port, (prci, tracedoc)) =>
      port.clock := prci.module.clock
      port.reset := prci.module.reset.asBool
      port.trace := tracedoc.bundle
      port.valid := tracedoc.bundle.valid
      port.bits := tracedoc.bundle.bits

      //port.tracerVTrigger := false.B
      //midas.targetutils.TriggerSink.whenEnabled(false.B) {
      //  port.tracerVTrigger := true.B
      //}
    }


    if (traceParams.print) {
      for ((trace, idx) <- tdio.tracedoctors.zipWithIndex ) {
        withClockAndReset(trace.clock, trace.reset) {
          when (trace.trace.valid) {
            printf(s"TraceDoctor $idx: %x\n", trace.trace.bits.asUInt)
          }
        }
      }
    }
    tdio
  })}
}


