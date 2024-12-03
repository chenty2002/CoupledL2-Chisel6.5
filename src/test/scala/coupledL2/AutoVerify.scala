package coupledL2

import circt.stage.ChiselStage
import freechips.rocketchip.diplomacy.{DisableMonitors, LazyModule}
import freechips.rocketchip.tile.MaxHartIdBits
import huancun.{DirtyField, HCCacheParameters, HCCacheParamsKey}
import org.chipsalliance.cde.config.Config
import utility._

import java.io._
import scala.collection.mutable.ArrayBuffer
import scala.sys.process._


object AutoVerify_L2L3L2 extends App {
  def modifyPy(filename: String): Unit = {
    val pylines = new ArrayBuffer[String]()
    val pyFile = new BufferedReader(new FileReader(new File("set_verify.py")))
    var line = pyFile.readLine()
    while(line != null) {
      pylines.append(
        line.replaceFirst("open\\('.*', 'w'\\) as fout:", s"open('${filename}', 'w') as fout:")
      )
      line = pyFile.readLine()
    }
    pyFile.close()

    val newPy = new BufferedWriter(new FileWriter(new File("set_verify_.py")))
    pylines.foreach { line =>
      newPy.write(line + "\n")
    }
    newPy.close()
  }

  val config = baseConfig(1).alterPartial({
    case L2ParamKey => L2Param(
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
    )
    case HCCacheParamsKey => HCCacheParameters(
      echoField = Seq(DirtyField())
    )
  })

  val suffix = "acquire"
  val path = "/home/lyj238/VerifyL2"
  val top = DisableMonitors(p => LazyModule(new VerifyTop_L2L3L2()(p)))(config)

  FileRegisters.writeOutputFile(
    "Verilog",
    "VerifyTop.sv",
    ChiselStage.emitSystemVerilog(top.module, firtoolOpts = Array("--disable-annotation-unknown"))
  )
  val cp = s"cp Verilog/L2L3L2/VerifyTop.sv .".!
  val filename = s"VerifyTop_${suffix}.sv"
  modifyPy(filename)
  val py = "python set_verify_.py".!
  val rm = s"rm -f ${path}/${suffix}/${filename}".!
  val cpjg = s"cp ${filename} ${path}/${suffix}".!
}
