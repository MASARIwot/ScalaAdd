import sbt._

object Dependencies {

  object Versions {
    val scalaVersion = "2.11.7"
    val sprayVersion = "1.3.3"
    val scalatestVersion = "2.2.6"

  }

  object Compile {

    val sprayCan = "io.spray" %% "spray-can" % Versions.sprayVersion
    val sprayRouting = "io.spray" %% "spray-routing-shapeless2" % Versions.sprayVersion
    val sprayJson = "io.spray" %% "spray-json" % "1.3.1"
  }

  object Test {
    val scalatest = "org.scalatest" %% "scalatest" % Versions.scalatestVersion //% "it,smoke,test"
    val scalaMock = "org.scalamock" %% "scalamock-scalatest-support" % "3.2" //% "it,smoke,test"
    val sprayTestKit = "io.spray" %% "spray-testkit" % "1.3.1" //% "it,smoke,test"
  }


  lazy val testKit = Seq(Test.scalatest, Test.scalaMock, Compile.sprayJson)

  lazy val sprayTestKit = Seq(Test.sprayTestKit)

  lazy val spray = Seq(Compile.sprayCan, Compile.sprayRouting, Compile.sprayJson)

}
