package test

import java.nio.file.{Files, Paths}

import dk.au.cs.dartkreader.loader.{BinaryBuilder, BinaryLoader}
import org.scalatest._

class SmokeTest extends FlatSpec with Matchers {

  "The Binary Reader" should "be able to read the dartk dill" in {

    val bl = new BinaryLoader()
    val fileBytes = Files.readAllBytes(Paths.get("samples/dartk.dill"))
    val bb = new BinaryBuilder(bl, fileBytes, "dartk.dill").readProgramFile()
    println("Done")
  }

}
