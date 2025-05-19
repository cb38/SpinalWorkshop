name := "SpinalWorkshop"

version := "1.0"

scalaVersion := "2.12.20"
val spinalVersion = "1.11.0"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.12" % "3.2.14",
  "com.github.spinalhdl" % "spinalhdl-core_2.12" % spinalVersion,
  "com.github.spinalhdl" % "spinalhdl-lib_2.12"  % spinalVersion,
  "com.github.spinalhdl" % "spinalhdl-tester_2.12"  % spinalVersion,
   compilerPlugin("com.github.spinalhdl" % "spinalhdl-idsl-plugin_2.12" % spinalVersion)
)

fork := true
