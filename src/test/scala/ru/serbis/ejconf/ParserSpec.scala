package ru.serbis.ejconf

import java.io.FileNotFoundException

import com.sun.jmx.mbeanserver.NamedObject
import org.parboiled2.ParseError
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import ru.serbis.ejconf.ConfigApi.InternalApi.astToMap
import ru.serbis.ejconf.ConfigParser._

import scala.io.Source
import scala.util.{Failure, Success}

/**
  * This test parse a test configuration file that contains all the language
  * features and verifies the correctness of the created ast.
  */
class ParserSpec extends WordSpecLike with Matchers with BeforeAndAfterAll {
  "Parser should produce correct ast from source" in {
    val inputFile = try {
      Right(Source.fromFile("src/test/resources/_4.ejc"))
    } catch { case e: FileNotFoundException => Left(e.getMessage) }

    if (inputFile.isRight) {
      val inputSource = inputFile.right.get.mkString
      val result = new ConfigParser(inputSource).InputLine.run()

      result match {
        case Success(ast) =>
          ast(0) shouldBe a [ObjectNode]
          ast(1) shouldBe a [ObjectNode]

          val obj1 = ast(0)
          obj1.name shouldEqual Some("obj1")

          obj1.body("a") shouldBe StringNode("a\"bc")
          obj1.body("b") shouldBe BoolNode(true)
          obj1.body("c") shouldBe IntNumberNode(1)
          obj1.body("d") shouldBe DoubleNumberNode(3.4)
          obj1.body("e") shouldBe a [ArrayNode]
          obj1.body("f") shouldBe a [ObjectNode]

          val e = obj1.body("e").asInstanceOf[ArrayNode]
          e.elems(0) shouldBe IntNumberNode(1)
          e.elems(1) shouldBe IntNumberNode(2)
          e.elems(2) shouldBe a [ObjectNode]

          val e3 = e.elems(2).asInstanceOf[ObjectNode]
          e3.body("a") shouldBe BoolNode(true)

          val f = obj1.body("f").asInstanceOf[ObjectNode]
          f.body("a") shouldBe IntNumberNode(1)


          //-------------------------------

          val obj2 = ast(1)
          obj2.name shouldEqual Some("obj2")

          obj2.body("a") shouldBe IntNumberNode(1)
          obj2.body("b") shouldBe StringNode("ab`c")
          obj2.body("c") shouldBe BoolNode(true)
          obj2.body("d") shouldBe a [ArrayNode]
          obj2.body("e") shouldBe a [ArrayNode]
          obj2.body("f") shouldBe a [ObjectNode]

          val obj2_d = obj2.body("d").asInstanceOf[ArrayNode]
          obj2_d.elems(0) shouldBe IntNumberNode(1)
          obj2_d.elems(1) shouldBe IntNumberNode(2)

          val obj2_e = obj2.body("e").asInstanceOf[ArrayNode]
          obj2_e.elems(0) shouldBe IntNumberNode(1)
          obj2_e.elems(1) shouldBe IntNumberNode(2)

          val obj2_f = obj2.body("f").asInstanceOf[ObjectNode]
          obj2_f.body("a") shouldBe IntNumberNode(1)


        case Failure(e: ParseError) => fail()
        case Failure(e) => fail()
      }
    } else {
      fail()
    }
  }
}
