package ru.serbis.ejconf

import org.parboiled2.ParseError
import ru.serbis.ejconf.ConfigFlow.{Config, EmptyConfig, IntermediateConfig}
import ConfigParser._
import scala.io.Source
import scala.util.{Failure, Success}

/**
  * Write here how to user this library
  */
object ConfigApi {

  /**
    * Load specified configuration file. At this operation it will be
    * understood next. Open file, read it to string, pass this string
    * to lang syntax parser, create config graph from produced ast and
    * return this graph as an IntermediateConfig. With last we can produce
    * search and casing operation, the finite target of witch - get
    * terminal configuration value. If opening file or parsing operation
    * was failed, function return EmptyConfig stage. For more info about
    * how stages works, see ConfigParser.scala file.
    *
    * @param path path to configuration file
    * @return IntermediateConfig or EmptyConfig if error was occurred
    */
  def loadConfig(path: String): Config = {
    val inputFile = Source.fromFile(path)
    val inputSource = inputFile.mkString
    val result = new ConfigParser(inputSource).InputLine.run()

    result match {
      case Success(ast) => IntermediateConfig(Some(astToMap(ast.toList)))
      case Failure(e: ParseError) => EmptyConfig("Expression is not valid: " + e.format(inputSource))
      case Failure(e) => EmptyConfig("Unexpected error during parsing run: " + e)
    }
  }

  /**
    * Check some configurations paths for exist. For example:
    *
    * {
    *   check(List(
    *     config ~> a
    *     config ~> b
    *     config ~> c
    *   ))
    * }
    *
    * If all of this three expression return FiniteConfig (which means that
    * all three values exist in the configuration) function return empty Right.
    * If at least one return EmptyConfig (which means that this value not found
    * in the configuration) function return Left with error message from first
    * failed expression.
    *
    * TODO:
    * - Add checks for type conforming of the target value
    *
    * @param p list of config paths
    * @return Right if all paths exist if the configuration or Left with error
    *         message
    */
  def check(p: List[Config]): Either[String, Unit] = {
    if (p.isEmpty) {
      Right()
    } else {
      if (p.head.isEmpty)
        Left(p.head.emptyReason.get)
      else
        check(tailOrEmpty(p))
    }
  }

  /**
    * -----------INTERNAL API-------------
    */

  /**
    * Convert parsed ast to the configuration graph. The latter is an acyclic
    * graph represents as Map. Each terminal value in this graph represented
    * as map value (name -> term). Array represented as a map value, where key
    * is a string mirror for an array index (index -> produce). Object
    * represented as a map value, where key is an another map (name -> MAP).
    *
    *
    * @param ast input abstract syntax tree
    * @return configuration graph
    */
  private def astToMap(ast: List[AstNode]) = {
    def rec(tree: Map[String, Any], node: AstNode): Any = {
      node match {
        case t: ObjectNode => t.body.foldLeft(Map.empty[String, Any])((a, v) => a + (v._1 -> rec(Map.empty, v._2)))
        case t: ArrayNode =>
          t.elems.foldLeft((0, Map.empty[String, Any]))((a, v) => (a._1 + 1, a._2 + (a._1.toString -> rec(Map.empty, v))))._2
        case t: StringNode =>
          t.v
        case t: IntNumberNode =>
          t.v
        case t: DoubleNumberNode =>
          t.v
        case
          t: BoolNode => t.v
      }
    }

    ast.foldLeft(Map.empty[String, Any]) ((a, v) => a + (v.asInstanceOf[ObjectNode].name.get -> rec(Map.empty, v)))
  }

  private def tailOrEmpty[T](list: List[T]): List[T] = if (list.lengthCompare(1) <= 0) List.empty else list.tail
}
