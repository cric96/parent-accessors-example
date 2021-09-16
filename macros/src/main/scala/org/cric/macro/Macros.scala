package org.cric.`macro`

import scala.reflect.macros.whitebox

/**
  * contains all macro definitions
  */
object Macros {
  def accessors[T](element : T): String = macro MacrosImpl.accessors[T]
}

/**
  * contains macros implementations
  */
object MacrosImpl {
  def accessors[T: c.WeakTypeTag](c: whitebox.Context)(element: c.Expr[T]): c.Expr[String] = {
    import c.universe._
    val weakType = weakTypeTag[T]
    val parents = weakType.tpe.baseClasses

    val accessors = parents
      .map(weakType.tpe.baseType(_))
      .flatMap(_.members)
      .filter(_.isPublic)
      .filter(_.isMethod)
      .map(_.asMethod)
      .filter(_.isAccessor)
      .toSet

    val names = accessors
      .map(_.fullName)
      .map(_.split("\\."))
      .map(_.reverse.head)

    val terms = names.map(TermName(_))

    val accessorValues = terms.map(name => c.Expr[(String, Any)](q"(${name.toString}, ${element}.${name})")).toSeq

    def spliceAnd(seq: Seq[Expr[(String, Any)]]): c.Expr[Seq[(String, Any)]] = seq.headOption match {
      case Some(head) => c.universe.reify(Seq((head.splice._1, head.splice._2)) ++ spliceAnd(seq.tail).splice)
      case _ => c.Expr[Seq[(String, Any)]](q"Seq.empty")
    }

    val elements = spliceAnd(accessorValues)
    c.Expr[String](q"${elements}.mkString")
  }
}
