package ru.serbis.ejconf

import ConfigApi._

/**
  * This file contains examples of how to use this library
  *
  * LIB VERSION: 0.001
  *
  */
object Examples {
  /**
    * Imagine some correct configuration file:
    *
    * out: {
    *   path: "/var/log/some.log"
    *   batch: 10
    *   rewrite: false
    *   logline: {
    *     template: "[Timestamp]-[Message]"
    *     timestamp: "hh:mm DD:MM:YYYY"
    *   }
    * }
    *
    * In the beginning you need to load the configuration file:
    */

  val config = loadConfig("/etc/myprog/main.ejc")

  /**
    * config is an IntermediateConfig stage. This object contain internal
    * representation of the early loaded file. What will happen, if you specify
    * incorrect file path, this file will be protect from read or he contain
    * incorrect data? loadConfig function return EmptyResult, containing a
    * reason describe why the configuration was not loaded. Simplest way to
    * check that configuration was loaded correctly, is to call isEmpty on
    * created configuration object:
    */

  if (config.isEmpty) {
    val errorMessage = config.emptyReason.get
    //Error report action
  } else {
    //Next action
  }

  /**
    * Now get some value from config:
    */

  val template = config ~> "logline" ~> "template" >|

  /**
    * Using the function ~> you determine the path to some expected value from
    * the configuration root. Function >| return raw result value. It is raw,
    * because you don't know concrete type of the returned value. It may be
    * String, Bool, Int, Double and some other types in the future. What
    * happen if you type wrong path and get his raw value? For example:
    */

  val x =  config ~> "logline" ~> "x" >|

  /**
    * This request return string with the text No value. It's not critical, if
    * type of the returned value is irrelevant to you and if this string may
    * not contain expected value. Say, you append this value to some string. No
    * matter what this, string, number or boolean, in any case it has correct
    * string representation. If this is not the case, then great problems await
    * you, because it may crash your program at runtime if your try cast the
    * value in non-conforming type. In what context it is possible safe to use
    * this function, I describe later in this text. Next, you may use the
    * function >=. This is equal for using >| with asInstanceOf call. It has the
    * same disadvantages as the function >=.
    */

  val batch = config ~> "batch" >= classOf[Int]
  //is equal to
  val batch2 = (config ~> "batch" >|).asInstanceOf[Int]

  /**
    * The next function is fully correct way to get a value from the
    * configuration if you can't guarantee if it is exist and has correct
    * type:
    */

  val rewrite = config ~> "rewrite" >> classOf[Boolean]

  if (rewrite.isLeft) {
    val errorMessage = rewrite.left.get
    //Error report action
  } else {
    //Next action
  }

  /**
    * This expression return Either[String, T], where Right contain expected
    * value or Left with error message.
    *
    * Api contain the function for predictive verification configuration
    * paths:
    */

  val checkList = List(
    config ~> "path",
    config ~> "batch",
    config ~> "rewrite",
    config ~> "logline" ~> "template",
    config ~> "logline" ~> "timestamp")

  val checkResult = check(checkList)

  if (checkResult.isLeft) {
    val errorMessage = checkResult.left.get
    //Error report action
  } else {
    //Next action
  }

  /**
    * The check function receive some configuration paths list, and consistently
    * check them for availability. If at least one from them is not available,
    * function return Left with error message else it return empty Right, that
    * means that all paths is available in configuration. This check is useful
    * to be able to safety use function >|, because after this check, them
    * can't return unexpected result.
    */
}
