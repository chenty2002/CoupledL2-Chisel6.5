package coupledL2

import chisel3._
import chisel3.ltl._
import circt.stage.ChiselStage
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chiselFv._
import coupledL2.tl2tl.TL2TLCoupledL2
import coupledL2.tl2tl.{Slice => TLSliceL2}
import coupledL2AsL1.prefetch.CoupledL2AsL1PrefParam
import coupledL2AsL1.tl2tl.{Slice => TLSliceL1, TL2TLCoupledL2 => TLCoupledL2AsL1}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import huancun._
import org.chipsalliance.cde.config._
import utility._


class VerifyTop_L2L3L2()(implicit p: Parameters) extends LazyModule {

  /* L1D   L1D
   *  |     |
   * L2    L2
   *  \    /
   *    L3
   */

  println("class VerifyTop_L2L3L2:")

  override lazy val desiredName: String = "VerifyTop"
  val delayFactor = 0.2
  val cacheParams = p(L2ParamKey)

  val nrL2 = 2

  def createClientNode(name: String, sources: Int) = {
    val masterNode = TLClientNode(Seq(
      TLMasterPortParameters.v2(
        masters = Seq(
          TLMasterParameters.v1(
            name = name,
            sourceId = IdRange(0, sources),
            supportsProbe = TransferSizes(cacheParams.blockBytes)
          )
        ),
        channelBytes = TLChannelBeatBytes(cacheParams.blockBytes),
        minLatency = 1,
        echoFields = Nil,
        requestFields = Seq(AliasField(2)),
        responseKeys = cacheParams.respKey
      )
    ))
    masterNode
  }

  val l0_nodes = (0 until nrL2).map(i => createClientNode(s"l0$i", 32))

  val coupledL2AsL1 = (0 until nrL2).map(i => LazyModule(new TLCoupledL2AsL1()(baseConfig(1).alterPartial({
    case L2ParamKey => L2Param(
      name = s"l1$i",
      ways = 2,
//      sets = 2,
//      blockBytes = 2,
//      channelBytes = TLChannelBeatBytes(1),
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      echoField = Seq(),
      prefetch = Seq(CoupledL2AsL1PrefParam()),
      mshrs = 4,
      hartId = i
    )
    case BankBitsKey => 0
  }))))
  val l1_nodes = coupledL2AsL1.map(_.node)

  val coupledL2 = (0 until nrL2).map(i => LazyModule(new TL2TLCoupledL2()(baseConfig(1).alterPartial({
    case L2ParamKey => L2Param(
      name = s"l2$i",
      ways = 2,
//      sets = 4,
//      blockBytes = 2,
//      channelBytes = TLChannelBeatBytes(1),
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      echoField = Seq(DirtyField()),
      mshrs = 4,
      hartId = i
    )
    case BankBitsKey => 0
  }))))
  val l2_nodes = coupledL2.map(_.node)

  val l3 = LazyModule(new HuanCun()(baseConfig(1).alterPartial({
    case HCCacheParamsKey => HCCacheParameters(
      name = "L3",
      level = 3,
      ways = 2,
//      sets = 4,
//      blockBytes = 2,
//      channelBytes = TLChannelBeatBytes(1),
      inclusive = false,
      clientCaches = (0 until nrL2).map(i =>
        CacheParameters(
          name = s"l2",
          sets = 128,
          ways = 2 + 2,
          blockGranularity = log2Ceil(128)
        ),
      ),
      echoField = Seq(DirtyField()),
      simulation = true,
      mshrs = 6
    )
  })))


  val xbar = TLXbar()
  val ram = LazyModule(new TLRAM(AddressSet(0, 0xFFFFFFL), beatBytes = 1))

  l0_nodes.zip(l1_nodes) map {
    case (l0, l1) => l1 := l0
  }

  l1_nodes.zip(l2_nodes).zipWithIndex map {
    case ((l1d, l2), i) => l2 := TLLogger(s"L2_L1_$i") := TLBuffer() := l1d
  }

  l2_nodes.zipWithIndex map {
    case (l2, i) => xbar := TLLogger(s"L3_L2_$i") := TLBuffer() := l2
  }

  ram.node :=
    TLXbar() :=*
      TLFragmenter(1, 2) :=*
      TLCacheCork() :=*
      TLBuffer.chainNode(5) :=*
      TLLogger(s"MEM_L3") :=*
      l3.node :=* xbar

  lazy val module = new LazyModuleImp(this) with Formal {
    val timer = WireDefault(0.U(64.W))
    val logEnable = WireDefault(false.B)
    val clean = WireDefault(false.B)
    val dump = WireDefault(false.B)

    dontTouch(timer)
    dontTouch(logEnable)
    dontTouch(clean)
    dontTouch(dump)

    coupledL2AsL1.foreach(_.module.io.debugTopDown := DontCare)
    coupledL2.foreach(_.module.io.debugTopDown := DontCare)


    coupledL2AsL1.foreach(_.module.io.l2_tlb_req <> DontCare)
    coupledL2.foreach(_.module.io.l2_tlb_req <> DontCare)

    coupledL2AsL1.foreach(_.module.io.hartId <> DontCare)
    coupledL2.foreach(_.module.io.hartId <> DontCare)

    l1_nodes.foreach { node =>
      val (l1_in, _) = node.in.head
      dontTouch(l1_in)
    }

    val verify_timer = RegInit(0.U(50.W))
    verify_timer := verify_timer + 1.U

    val offsetBits = 6
    val setBits = 7
    val tagBits = 11
    val bankBits = 0

    val addr_offsetBits = 1
    val addr_setBits = 1
    val addr_tagBits = 3

    // Input signals for formal verification
    val stimuli = IO(new Bundle {
      val set = Input(Vec(nrL2, UInt(addr_setBits.W)))
      val tag = Input(Vec(nrL2, UInt(addr_tagBits.W)))
      val needT = Input(Vec(nrL2, Bool()))
    })

    coupledL2AsL1.zipWithIndex.foreach { case (l2AsL1, i) =>
      l2AsL1.module.io.prefetcherSet := stimuli.set(i)
      l2AsL1.module.io.prefetcherTag := stimuli.tag(i)
      l2AsL1.module.io.prefetcherNeedT := stimuli.needT(i)
      dontTouch(l2AsL1.module.io)
    }

    coupledL2(0).module.slices.head match {
      case tlSlice: TLSliceL2 =>
        val dir_resetFinish = BoringUtils.bore(tlSlice.directory.resetFinish)
        assume(verify_timer < 100.U || dir_resetFinish)
    }

//    coupledL2AsL1.foreach { l1 =>
//      l1.module.slices.head match {
//        case tlSlice: TLSliceL1 =>
//          tlSlice.mshrCtl.mshrs.zipWithIndex.foreach {
//            case (mshr, i) =>
//              val MSHRStatus = BoringUtils.bore(mshr.io.status.valid)
//              val allocStatus = BoringUtils.bore(mshr.io.alloc.valid)
//              val channel = BoringUtils.bore(mshr.io.status.bits.channel)
//              if (i >= 4)
//                assume(!MSHRStatus && !allocStatus)
//              else if (i == 3)
//                assume(channel =/= 1.U)
//          }
//      }
//    }

    coupledL2.foreach { l2 =>
      l2.module.slices.head match {
        case tlSlice: TLSliceL2 =>
          tlSlice.mshrCtl.mshrs.zipWithIndex.foreach {
            case (mshr, i) =>
              val MSHRStatus = BoringUtils.bore(mshr.io.status.valid)
              val allocStatus = BoringUtils.bore(mshr.io.alloc.valid)
//              val channel = BoringUtils.bore(mshr.io.status.bits.channel)
//              if (i >= 4)
//                assume(!MSHRStatus && !allocStatus)
//              else if (i == 3)
//                assume(channel =/= 1.U)

              if(i < 4) {
                astRelaxedLiveness(MSHRStatus, !MSHRStatus, 300)
                astRelaxedLiveness(MSHRStatus, !MSHRStatus, 500)
                astRelaxedLiveness(MSHRStatus, !MSHRStatus, 800)

                AssumeProperty(
                  Sequence.BoolSequence(MSHRStatus).implication(
                    Sequence.BoolSequence(!MSHRStatus).delayRange(1, 1000)
                  )
                )
              }
          }
      }
    }
  }
}

object VerifyTop_L2L3L2 extends App {

  println("object VerifyTop_L2L3L2:")

  val config = baseConfig(1).alterPartial({
    case L2ParamKey => L2Param(
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
    )
    case HCCacheParamsKey => HCCacheParameters(
      echoField = Seq(DirtyField())
    )
  })
  val top = DisableMonitors(p => LazyModule(new VerifyTop_L2L3L2()(p)))(config)

  FileRegisters.writeOutputFile(
    "Verilog",
    "VerifyTop.sv",
    ChiselStage.emitSystemVerilog(top.module, firtoolOpts = Array("--disable-annotation-unknown"))
  )
}