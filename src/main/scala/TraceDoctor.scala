package testchipip

//import chipsalliance.rocketchip.config.Field
import org.chipsalliance.cde.config.{Field, Parameters}
import chisel3._
import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._
import freechips.rocketchip.diplomacy.LazyRawModuleImp
import org.chipsalliance.diplomacy.bundlebridge.BundleBridgeNexusNode
import freechips.rocketchip.rocket.TraceDoctor
import freechips.rocketchip.subsystem.{BaseSubsystem, InstantiatesHierarchicalElements, HasHierarchicalElementsRootContext}
//import freechips.rocketchip.util.HeterogeneousBag
import org.chipsalliance.diplomacy.nodes.HeterogeneousBag

// A per-tile interface that includes the tile's clock and reset
class TileTraceDoctorIO(_traceDoctorType: TraceDoctor) extends Bundle {
  val clock: Clock = Clock()
  val reset: Bool = Bool()
  val bits = Vec(traceWidth, Bool())
  val valid: Bool = Bool()
  //val trace: TraceDoctor = new TraceDoctor(traceWidth)
  val tracerVTrigger: Bool = Bool()
  val trace = traceType.cloneType
  def traceType = _traceDoctorType
  def traceWidth = traceType.traceWidth
}

// The IO matched on by the TraceDoctor bridge: a wrapper around a heterogenous
// bag of TileTraceDoctorIO. Each entry is trace associated with a single tile

// must extend chipyard.iobinders.Port
//class TraceOutputTop(coreTraces: Seq[TraceBundle]) extends Bundle {
//  val traces = Output(HeterogeneousBag.apply(coreTraces.map(t => new TileTraceIO(t))))
//}
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

// Use this trait:
//trait CanHaveTraceDoctorIO { this: InstantiatesHierarchicalElements =>
//trait CanHaveTraceDoctorIO { this: BaseSubsystem =>
//  implicit val p: Parameters
//
//  val module: CanHaveTraceDoctorIOModuleImp
//  // Bind all the trace nodes to a BB; we'll use this to generate the IO in the imp
//  val traceDoctorNexus = BundleBridgeNexusNode[TraceDoctor]()
//  //totalTiles.foreach { traceDoctorNexus := _._2.traceDoctorNode }
//  val chipyardSystem = module.asInstanceOf[InstantiatesHierarchicalElements]
//  //val tiles = chipyardSystem.totalTiles.values
//  chipyardSystem.totalTiles.foreach { traceDoctorNexus := _._2.traceDoctorNode }
//}
case class TraceDoctorPortParams(print: Boolean = false)
object TraceDoctorPortKey extends Field[Option[TraceDoctorPortParams]](None)

//trait HasExtInterruptsModuleImp extends LazyRawModuleImp with HasExtInterruptsBundle {
//  val outer: HasExtInterrupts
//  val interrupts = IO(Input(UInt(outer.nExtInterrupts.W)))
//
//  outer.extInterrupts.out.map(_._1).flatten.zipWithIndex.foreach { case(o, i) => o := interrupts(i) }
//}

//trait CanHaveTraceDoctorIOModuleImp extends LazyModuleImp with HasTraceDoctorBundle {
//trait CanHaveTraceDoctorIOModuleImp extends LazyRawModuleImp {
//  //val outer: CanHaveTraceDoctorIO with InstantiatesHierarchicalElements
//  val outer: CanHaveTraceDoctorIO
//
//  val traceDoctorIO = p(TraceDoctorPortKey) map ( traceParams => {
//    val traceDoctorSeq = (outer.traceDoctorNexus.in.map(_._1))
//    val tdio = IO(Output(TraceDoctorOutputTop(traceDoctorSeq)))
//
//    (tdio.tracedoctors zip (outer.tile_prci_domains zip traceDoctorSeq)).foreach { case (port, (prci, tracedoc)) =>
//      port.clock := prci._2.module.clock
//      port.reset := prci._2.module.reset.asBool
//      port.data := tracedoc
//
//      port.tracerVTrigger := false.B
//      midas.targetutils.TriggerSink.whenEnabled(false.B) {
//        port.tracerVTrigger := true.B
//      }
//    }
//
//    if (traceParams.print) {
//      for ((trace, idx) <- tdio.tracedoctors.zipWithIndex ) {
//        withClockAndReset(trace.clock, trace.reset) {
//          when (trace.data.valid) {
//            printf(s"TraceDoctor $idx: %x\n", trace.data.bits.asUInt)
//          }
//        }
//      }
//    }
//    tdio
//  })
//}


trait CanHaveTraceDoctorIO { this: HasHierarchicalElementsRootContext with InstantiatesHierarchicalElements =>
  implicit val p: Parameters

  val tileTraceDoctorNodes = traceDoctorNodes.values
  // val tileTraceNodes = traceNodes.values // TODO FIND where traceNodes and traceDoctorNodes are!

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
      //port.bits := tracedoc.bundle.bits

      port.tracerVTrigger := false.B
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


