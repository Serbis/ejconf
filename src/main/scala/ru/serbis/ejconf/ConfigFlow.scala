package ru.serbis.ejconf

/**
  * LIB VERSION: 0.001
  */
object ConfigFlow {
  sealed trait Config {
    /** Configuration graph*/
    val tree: Option[Map[String, Any]] = None
    /** Reason while stage is empty. Read this field as an error message*/
    val emptyReason: Option[String] = None
    /** Finite result value. This field fill only in FiniteConfig */
    val result: Option[Any] = None

    /**
      * Check config object for empty. Emptiness is a marker that checked
      * object is EmptyConfig
      */
    def isEmpty = tree.isEmpty

    /**
      * Searches in the configuration tree for the value passed to the method.
      * If the value does not exist it produce EmptyConfig. If the value found
      * is finite, it produce FinalConfig. If the value is not finite, it
      * produce IntermediateConfig with slices config tree. This method
      * realized in IntermediateConfig, in any other stages it return self
      * instance.
      *
      * @param arg value for search in config tree
      * @return next config stage
      */
    def ~>(arg: Any): Config = { this }

    /**
      * Safe finite result casting (In the opposite to >=). Cast finite result
      * to expected class. If cast was successful, return Right with casted
      * value, instead return Left with error message. This method is applicable
      * only for FiniteConfig stage, in others in return specific error message
      * in Left.
      *
      * TODO:
      * - Why class? Class may be replaced by ClassTag? It may by use
      *   as func [T]?
      *
      * @param clazz target class
      * @return Either with casted value in Right or error message in Left
      */
    def >>[T](clazz: Class[T]) : Either[String, T]

    /**
      * Return finite value casted to the expected class. This operation is
      * not verifable. If class cast is failed, it return error message as
      * String. This situation may crash program at runtime. Use this method
      * only if you are completely sure, in that the casted value type is which
      * you expect.
      *
      * TODO:
      * - Why class? Class may be replaced by ClassTag? It may by use
      *   as func [T]?
      *
      *
      * @param clazz target class
      * @return casted finite value
      */
    def >=[T](clazz: Class[T]) : T = "No value".asInstanceOf[T]

    /**
      * Return raw value without type casting from FiniteConfig. In any other
      * stage types, in return String with No value message.
      *
      * @return finite value
      */
    def >| : Any = "No value"
  }

  /**
    * After loading some configuration file, an intermediate config is created.
    * This stage contain only one useful method >> that find some value in the
    * head of the configuration tree, and produce next Config stage. For
    * more detail, see >> method description in extended trait.
    *
    * @param mTree sliced config tree
    */
  sealed case class IntermediateConfig(mTree: Option[Map[String, Any]] = None) extends Config {
    override val tree = mTree
    override val emptyReason = Some("This is an intermediate value")


    override def ~>(arg: Any): Config = {
      arg match {
        case t: String =>
          val node = mTree.get.get(t)
          if (node.isEmpty) {
            EmptyConfig(s"Not found child - $t")
          } else {
            val t = term(node.get)
            if (t.isDefined)
              FinalConfig(t.get)
            else
              IntermediateConfig(Some(node.get.asInstanceOf[Map[String, Any]]))
          }
      }
    }


    /**
      * ---------- INTERNAL API ------------
      */

    /**
      * Check some value from config tree for finite
      *
      * @param v value for testing
      * @return finite value or None if is is not finite
      */
    private def term(v: Any): Option[Any] = {
      v match {
        case t: String => Some(t)
        case t: Int => Some(t)
        case t: Double => Some(t)
        case t: Boolean => Some(t)
        case _ => None
      }
    }

    override def >>[T](clazz: Class[T]) = Left(s"Value is not final")
  }

  /**
    * Stage with final (terminal) value. Any correct configuration path, ends
    * with this stage. He serves all final value retuning operations. See
    * extended trait for more details about this methods.
    */
  sealed case class FinalConfig(res: Any) extends Config {
    override val result = Some(res)
    override def isEmpty = false

    override def >>[T](clazz: Class[T]) = {
      try {
        Right(res.asInstanceOf[T])
      } catch { case _: ClassCastException => Left(s"Value $res unable cast to ${clazz.getName}")}
    }
    override def >=[T](clazz: Class[T]) = res.asInstanceOf[T]
    override def >| = res
  }

  /**
    * Empty config stage, at fact is an error stage. Any other stage transforms
    * to it, if some error was occurred. Configuration file not found, value
    * not found, casting error and others, all errors final at this stage. This
    * class contains only one useful field - reason while config is empty.
    */
  sealed case class EmptyConfig(reason: String) extends Config {
    override val emptyReason = Some(reason)
    override def >>[T](clazz: Class[T]) = Left(emptyReason.getOrElse("Value is empty"))
  }
}
