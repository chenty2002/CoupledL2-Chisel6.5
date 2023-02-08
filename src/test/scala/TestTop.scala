package coupledL2

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
// import huancun._

import scala.collection.mutable.ArrayBuffer

class TestTop_L2()(implicit p: Parameters) extends LazyModule {

  /* L1D   L1D
   *  \    /
   *    L2
   */

  val delayFactor = 0.5
  val cacheParams = p(L2ParamKey)

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

  val l1d_nodes = (0 until 1) map( i => createClientNode(s"l1d$i", 32))
  val master_nodes = l1d_nodes

  val l2 = LazyModule(new CoupledL2())
  val xbar = TLXbar()
  val ram = LazyModule(new TLRAM(AddressSet(0, 0xffffL), beatBytes = 32))

  for (l1d <- l1d_nodes) {
    xbar := TLBuffer() := l1d
  }

  ram.node :=
    TLXbar() :=*
      TLFragmenter(32, 64) :=*
      TLCacheCork() :=*
      TLDelayer(delayFactor) :=*
      l2.node :=* xbar

  lazy val module = new LazyModuleImp(this){
    master_nodes.zipWithIndex.foreach{
      case (node, i) =>
        node.makeIOs()(ValName(s"master_port_$i"))
    }
  }
}

// class TestTop_L2L3()(implicit p: Parameters) extends LazyModule {
//   /* L1I    L1D
//    *   \    /
//    *     L2
//    *      |
//    *     L3
//    */
//   val delayFactor = 0.2
//   val cacheParams = p(L2ParamKey)

//   def createClientNode(name: String, sources: Int) = {
//     val masterNode = TLClientNode(Seq(
//       TLMasterPortParameters.v2(
//         masters = Seq(
//           TLMasterParameters.v1(
//             name = name,
//             sourceId = IdRange(0, sources),
//             supportsProbe = TransferSizes(cacheParams.blockBytes)
//           )
//         ),
//         channelBytes = TLChannelBeatBytes(cacheParams.blockBytes),
//         minLatency = 1,
//         echoFields = Nil,
//         requestFields = Seq(AliasField(2)),
//         responseKeys = cacheParams.respKey
//       )
//     ))
//     masterNode
//   }

//   val l1d = createClientNode(s"l1d", 32)
//   val l1i = TLClientNode(Seq(
//     TLMasterPortParameters.v1(
//       clients = Seq(TLMasterParameters.v1(
//         name = s"l1i",
//         sourceId = IdRange(0, 32)
//       ))
//     )
//   ))
//   val master_nodes = Seq(l1d, l1i)

//   val l2 = LazyModule(new CoupledL2()(new Config((_, _, _) => {
//     case L2ParamKey => L2Param(
//       name = s"l2",
//       ways = 4,
//       sets = 128,
//       echoField = Seq(DirtyField())
//     )
//   }))).node

//   val l3 = LazyModule(new HuanCun()(new Config((_, _, _) => {
//     case HCCacheParamsKey => HCCacheParameters(
//       name = "l3",
//       level = 3,
//       ways = 4,
//       sets = 128,
//       inclusive = false,
//       clientCaches = Seq(
//         CacheParameters(
//           name = s"l2",
//           sets = 128,
//           ways = 4,
//           blockGranularity = log2Ceil(128)
//         ),
//       ),
//       echoField = Seq(DirtyField()),
//       simulation = true
//     )
//   })))

//   val xbar = TLXbar()
//   val ram = LazyModule(new TLRAM(AddressSet(0, 0xffffL), beatBytes = 32))

//   xbar := TLBuffer() := l1i
//   xbar := TLBuffer() := l1d

//   ram.node := 
//     TLXbar() :=*
//     TLFragmenter(32, 64) :=*
//     TLCacheCork() :=*
//     TLDelayer(delayFactor) :=*
//     l3.node :=*
//     TLBuffer() :=
//     l2 :=* xbar
  
//   lazy val module = new LazyModuleImp(this) {
//     master_nodes.zipWithIndex.foreach {
//       case (node, i) =>
//         node.makeIOs()(ValName(s"master_port_$i"))
//     }
//   }
// }

// class TestTop_L2L3L2()(implicit p: Parameters) extends LazyModule {

//   /* L1D   L1D
//    *  |     |
//    * L2    L2
//    *  \    /
//    *    L3
//    */

//   val delayFactor = 0.2
//   val cacheParams = p(L2ParamKey)

//   val nrL2 = 2

//   def createClientNode(name: String, sources: Int) = {
//     val masterNode = TLClientNode(Seq(
//       TLMasterPortParameters.v2(
//         masters = Seq(
//           TLMasterParameters.v1(
//             name = name,
//             sourceId = IdRange(0, sources),
//             supportsProbe = TransferSizes(cacheParams.blockBytes)
//           )
//         ),
//         channelBytes = TLChannelBeatBytes(cacheParams.blockBytes),
//         minLatency = 1,
//         echoFields = Nil,
//         requestFields = Seq(AliasField(2)),
//         responseKeys = cacheParams.respKey
//       )
//     ))
//     masterNode
//   }

//   val l1d_nodes = (0 until nrL2).map(i => createClientNode(s"l1d$i", 32))
//   val master_nodes = l1d_nodes

//   val l2_nodes = (0 until nrL2).map(i => LazyModule(new CoupledL2()(new Config((_, _, _) => {
//     case L2ParamKey => L2Param(
//       name = s"l2$i",
//       ways = 4,
//       sets = 128,
//       echoField = Seq(DirtyField())
//     )
//   }))).node)

//   val l3 = LazyModule(new HuanCun()(new Config((_, _, _) => {
//     case HCCacheParamsKey => HCCacheParameters(
//       name = "L3",
//       level = 3,
//       ways = 4,
//       sets = 128,
//       inclusive = false,
//       clientCaches = (0 until nrL2).map(i =>
//         CacheParameters(
//           name = s"l2",
//           sets = 128,
//           ways = 4,
//           blockGranularity = log2Ceil(128)
//         ),
//       ),
//       echoField = Seq(DirtyField()),
//       simulation = true
//     )
//   })))

//   val xbar = TLXbar()
//   val ram = LazyModule(new TLRAM(AddressSet(0, 0xffffL), beatBytes = 32))

//   l1d_nodes.zip(l2_nodes).map {
//     case (l1d, l2) => l2 := TLBuffer() := l1d
//   }

//   for (l2 <- l2_nodes) {
//     xbar := TLBuffer() := l2
//   }

//   ram.node :=
//     TLXbar() :=*
//       TLFragmenter(32, 64) :=*
//       TLCacheCork() :=*
//       TLDelayer(delayFactor) :=*
//       l3.node :=* xbar

//   lazy val module = new LazyModuleImp(this) {
//     master_nodes.zipWithIndex.foreach {
//       case (node, i) =>
//         node.makeIOs()(ValName(s"master_port_$i"))
//     }
//   }
// }

object TestTop_L2 extends App {
  val config = new Config((_, _, _) => {
    case L2ParamKey => L2Param(
      echoField = Seq(DirtyField())
    )
  })
  val top = DisableMonitors(p => LazyModule(new TestTop_L2()(p)) )(config)

  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => top.module)
  ))
}

// object TestTop_L2L3 extends App {
//   val config = new Config((_, _, _) => {
//     case L2ParamKey => L2Param(
//       echoField = Seq(DirtyField())
//     )
//     case HCCacheParamsKey => HCCacheParameters(
//       echoField = Seq(DirtyField())
//     )
//   })
//   val top = DisableMonitors(p => LazyModule(new TestTop_L2L3()(p)) )(config)

//   (new ChiselStage).execute(args, Seq(
//     ChiselGeneratorAnnotation(() => top.module)
//   ))
// }

// object TestTop_L2L3L2 extends App {
//   val config = new Config((_, _, _) => {
//     case L2ParamKey => L2Param(
//       echoField = Seq(DirtyField())
//     )
//     case HCCacheParamsKey => HCCacheParameters(
//       echoField = Seq(DirtyField())
//     )
//   })
//   val top = DisableMonitors(p => LazyModule(new TestTop_L2L3L2()(p)))(config)

//   (new ChiselStage).execute(args, Seq(
//     ChiselGeneratorAnnotation(() => top.module)
//   ))
// }