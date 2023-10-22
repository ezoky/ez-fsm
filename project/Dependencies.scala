import sbt._

/**
 * @author gweinbach on 29/12/2021
 * @since 0.2.0
 */
object Dependencies {

  object Versions {
    //    val scala211 = "2.11.12"
//    val scala212 = "2.12.15"
    val scala213 = "2.13.10"

    val scala2 = scala213


    val scala3 = "3.2.2"
    // Scala standard modules
    val ScalaParallelCollections = "1.0.4"

    // Typelevel
    val Cats = "2.7.0"
    val CatsEffects = "3.4.4"
    val Spire = "0.18.0"

    // Michael Sitko
    val RefinedMini: String = "0.1.0"

    object Test {

//      val SLF4J = "1.7.30"
//      val Logback = "1.2.3"

      val JunitInterface = "0.11"
      val Scalatest = "3.2.10"
      val Scalacheck = "1.15.4"
    }
  }

  // Scala standard modules
  val `scala-parallel-collections` = "org.scala-lang.modules" %% "scala-parallel-collections" % Versions.ScalaParallelCollections

  // Typelevel

    // Cats
    val `cats-kernel` = "org.typelevel" %% "cats-kernel" % Versions.Cats
    val `cats-core` = "org.typelevel" %% "cats-core" % Versions.Cats
    val `cats-mtl` = "org.typelevel" %% "cats-mtl" % Versions.Cats
    // Minimal dependencies to use cats library
    val `cats-minimal` = Seq(`cats-kernel`, `cats-core`)
    val `cats-effects` = "org.typelevel" %% "cats-effect" % Versions.CatsEffects
    // Spire
    val spire = "org.typelevel" %% "spire" % Versions.Spire

    // Refined Mini
    val `refined-mini` = "pl.msitko" %% "mini-refined" % Versions.RefinedMini

  // end Typelevel

  object Test {
    val `junit-interface` =  "com.novocode" % "junit-interface" % Versions.Test.JunitInterface % "test"
    val scalacheck = "org.scalacheck" %% "scalacheck" % Versions.Test.Scalacheck % "test"
    val scalatest = "org.scalatest" %% "scalatest" % Versions.Test.Scalatest % "test"
  }
}
