package ru.serbis.ejconf

import java.io.FileNotFoundException

import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import ConfigApi._
import ConfigParser._
import InternalApi._
import org.parboiled2.ParseError
import ru.serbis.ejconf.ConfigFlow.{EmptyConfig, IntermediateConfig}

import scala.io.Source
import scala.util.{Failure, Success}

/**
  * This test checks all api functions for correct operation
  */
class ApiSpec extends WordSpecLike with Matchers with BeforeAndAfterAll {
  "loadConfig function" must {
    "return IntermediateConfig if the correct file path was passed" in {
      val config = loadConfig("src/test/resources/_1.ejc")
      config shouldBe a [IntermediateConfig]
    }

    "return EmptyConfig if file does not exist" in {
      val config = loadConfig("src/test/resources/_notexist.ejc")
      config shouldBe a [EmptyConfig]
    }

    "return EmptyConfig if file permissions error was occurred" in {
      val config = loadConfig("../_perm.ejc")
      config shouldBe a [EmptyConfig]
    }

    "return EmptyConfig if parsing error was occurred" in {
      val config = loadConfig("src/test/resources/_bad.ejc")
      config shouldBe a [EmptyConfig]
    }
  }

  "check function" must {
    "return Left if all passed paths exist in the config" in {
      val config = loadConfig("src/test/resources/_2.ejc")
      val r = check(List(
        config ~> "obj" ~> "a",
        config ~> "obj" ~> "b",
        config ~> "obj" ~> "c"
      ))

      r shouldEqual Right()
    }

    "return Right if one of the passed paths does not exist in the config" in {
      val config = loadConfig("src/test/resources/_2.ejc")
      val r = check(List(
        config ~> "obj" ~> "a",
        config ~> "obj" ~> "x",
        config ~> "obj" ~> "c"
      ))

      r.isLeft shouldEqual true
    }
  }

  "astToMap function" must {
    "return a correct configuration tree for a correct ast" in {
      val inputFile = try {
        Right(Source.fromFile("src/test/resources/_3.ejc"))
      } catch { case e: FileNotFoundException => Left(e.getMessage) }

      if (inputFile.isRight) {
        val inputSource = inputFile.right.get.mkString
        val result = new ConfigParser(inputSource).InputLine.run()

        result match {
          case Success(ast) =>
            val tree = astToMap(ast.toList)
            tree.contains("obj") shouldEqual true

            val obj = tree("obj").asInstanceOf[Map[String, Any]]
            obj.contains("a") shouldEqual true
            obj.contains("b") shouldEqual true
            obj.contains("c") shouldEqual true
            obj.contains("d") shouldEqual true
            obj.contains("e") shouldEqual true
            obj.contains("f") shouldEqual true
            obj("a") shouldEqual "abc"
            obj("b") shouldEqual true
            obj("c") shouldEqual 1
            obj("d") shouldEqual 3.4

            val e = obj("e").asInstanceOf[Map[String, Any]]
            e.contains("1") shouldEqual true
            e.contains("2") shouldEqual true
            e("0") shouldEqual 1
            e("1") shouldEqual 2

            val ar3 = e("2").asInstanceOf[Map[String, Any]]
            ar3.contains("a") shouldEqual true
            ar3("a") shouldEqual true

            val f = obj("f").asInstanceOf[Map[String, Any]]
            f.contains("a") shouldEqual true
            f("a") shouldEqual 1

          case Failure(e: ParseError) => fail()
          case Failure(e) => fail()
        }
      } else {
        fail()
      }
    }
  }
}
