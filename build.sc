import mill._
import scalalib._
import scalafmt._
import os.Path
import publish._
import $file.common
import $file.`rocket-chip`.common
import $file.`rocket-chip`.common
import $file.`rocket-chip`.cde.common
import $file.`rocket-chip`.hardfloat.build

val defaultVersions = Map(
  "chisel3" -> "3.6.0",
  "chisel3-plugin" -> "3.6.0",
  "chiseltest" -> "0.6.2",
  "scala" -> "2.13.10",
)

def getVersion(dep: String, org: String = "edu.berkeley.cs", cross: Boolean = false) = {
  val version = sys.env.getOrElse(dep + "Version", defaultVersions(dep))
  if (cross)
    ivy"$org:::$dep:$version"
  else
    ivy"$org::$dep:$version"
}

trait HasChisel extends ScalaModule {
  def chiselModule: Option[ScalaModule] = None

  def chiselPluginJar: T[Option[PathRef]] = None

  def chiselIvy: Option[Dep] = Some(getVersion("chisel3"))

  def chiselPluginIvy: Option[Dep] = Some(getVersion("chisel3-plugin", cross=true))

  override def scalaVersion = defaultVersions("scala")

  override def scalacOptions = super.scalacOptions() ++
    Agg("-language:reflectiveCalls", "-Ymacro-annotations", "-Ytasty-reader")

  override def ivyDeps = super.ivyDeps() ++ Agg(chiselIvy.get)

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(chiselPluginIvy.get)
}

trait HasChisel6 extends ScalaModule {
  def chiselModule: Option[ScalaModule] = None

  def chiselPluginJar: T[Option[PathRef]] = None

  def chiselIvy: Option[Dep] = Some(ivy"org.chipsalliance::chisel:6.5.0")

  def chiselPluginIvy: Option[Dep] = Some(ivy"org.chipsalliance:::chisel-plugin:6.5.0")

  override def scalaVersion = "2.13.14"

  override def scalacOptions = super.scalacOptions() ++
    Agg("-language:reflectiveCalls", "-Ymacro-annotations", "-Ytasty-reader")

  override def ivyDeps = super.ivyDeps() ++ Agg(chiselIvy.get)

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(chiselPluginIvy.get)
}

object rocketchip extends `rocket-chip`.common.RocketChipModule with HasChisel6 {

  val rcPath = os.pwd / "rocket-chip"
  override def millSourcePath = rcPath

  def mainargsIvy = ivy"com.lihaoyi::mainargs:0.7.0"

  def json4sJacksonIvy = ivy"org.json4s::json4s-jackson:4.0.7"

  object macros extends `rocket-chip`.common.MacrosModule with HasChisel6 {
    def scalaReflectIvy = ivy"org.scala-lang:scala-reflect:${scalaVersion}"
  }

  object cde extends `rocket-chip`.cde.common.CDEModule with HasChisel6 {
    override def millSourcePath = rcPath / "cde" / "cde"
  }

  object hardfloat extends `rocket-chip`.hardfloat.common.HardfloatModule with HasChisel6 {
    override def millSourcePath = rcPath / "hardfloat" / "hardfloat"
  }

  def macrosModule = macros

  def hardfloatModule = hardfloat

  def cdeModule = cde

}

object utility extends SbtModule with HasChisel6 {
  override def millSourcePath = os.pwd / "utility"

  override def moduleDeps = super.moduleDeps ++ Seq(rocketchip)
 }

object huancun extends SbtModule with HasChisel6 {
  override def millSourcePath = os.pwd / "HuanCun"
  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip, utility
  )
}

object CoupledL2 extends SbtModule with HasChisel6 with millbuild.common.CoupledL2Module {

  override def millSourcePath = millOuterCtx.millSourcePath

  def rocketModule: ScalaModule = rocketchip

  def utilityModule: ScalaModule = utility

  def huancunModule: ScalaModule = huancun

  object test extends SbtModuleTests with TestModule.ScalaTest {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"edu.berkeley.cs::chiseltest:6.0.0",
    )
  }
}
