package ru.serbis.ejconf

import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import ru.serbis.ejconf.ConfigFlow.{EmptyConfig, FinalConfig, IntermediateConfig}

/**
  * This test checks all configuration stages for correct operation
  */
class FlowSpec extends WordSpecLike with Matchers with BeforeAndAfterAll {
  "IntermediateConfig" must {
    "for isEmpty call must return false" in {
      val config = EmptyConfig("x")
      config.isEmpty shouldEqual true
    }

    "for >| call must return No value string" in {
      val config = IntermediateConfig()
      val r = (config >|)
      r shouldEqual "No value"
    }

    "for >= call must return No value string" in {
      val config = IntermediateConfig()
      val r = (config >= classOf[String])
      r shouldEqual "No value"
    }

    "for >> call must return Left" in {
      val config = IntermediateConfig()
      val r = (config >> classOf[String])
      r.isLeft shouldEqual true
    }

    "for ~> call" must {
      val tree = Map("a" -> Map("b" -> 1))

      "return IntermediateConfig if specified value was found" in {
        val config = IntermediateConfig(Some(tree))
        val r = config ~> "a"
        r shouldBe a [IntermediateConfig]
        r.asInstanceOf[IntermediateConfig].mTree shouldEqual Some(Map("b" -> 1))
      }

      "return FinalConfig if specified value was found and is is term" in {
        val config = IntermediateConfig(Some(tree))
        val r = config ~> "a" ~> "b"
        r shouldBe a [FinalConfig]
        r.asInstanceOf[FinalConfig].res shouldEqual 1
      }

      "return EmptyConfig if specified value was not found" in {
        val config = IntermediateConfig(Some(tree))
        val r = config ~> "a" ~> "c"
        r shouldBe a [EmptyConfig]
        r.asInstanceOf[EmptyConfig].emptyReason shouldEqual Some("Not found child - c")
      }
    }
  }

  "EmptyConfig " must {
    "for isEmpty call must return true" in {
      val config = EmptyConfig("x")
      config.isEmpty shouldEqual true
    }

    "for >| call must return No value string" in {
      val config = EmptyConfig("x")
      val r = (config >|)
      r shouldEqual "No value"
    }

    "for >= call must return No value string" in {
      val config = EmptyConfig("x")
      val r = (config >= classOf[String])
      r shouldEqual "No value"
    }

    "for >> call must return Left" in {
      val config = EmptyConfig("x")
      val r = (config >> classOf[String])
      r.isLeft shouldEqual true
    }
    "for ~> call must return self" in {
      val config = EmptyConfig("")
      val r = config ~> "a"
      r shouldEqual config
    }
  }

  "FinalConfig " must {
    "for isEmpty call must return true" in {
      val config = FinalConfig("x")
      config.isEmpty shouldEqual false
    }

    "for >| call must return raw value" in {
      val config = FinalConfig("x")
      val r = (config >|)
      r shouldEqual "x"
    }

    "for >= call must return casted value" in {
      val config = FinalConfig("x")
      val r = (config >= classOf[String])
      r shouldEqual "x"
    }

    "for >> call must return Right with casted value" in {
      val config = FinalConfig("x")
      val r = (config >> classOf[String])
      r.isRight shouldEqual true
      r.right.get shouldEqual "x"
    }
    "for ~> call must return self" in {
      val config = FinalConfig("x")
      val r = config ~> "a"
      r shouldEqual config
    }
  }
}
