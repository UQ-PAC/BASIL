package ir.dsl

import scala.quoted.*
import scala.deriving.*
import scala.compiletime.*

import scala.collection.mutable.AbstractIterable

inline def boop[A] = ${ boopImpl[A]() }

def generateForCase[A: Type](using Quotes)(e: Expr[A]): Expr[String] =
  ???

def boopImpl[A: Type](using Quotes)(): Expr[Int] =
  import quotes.reflect.*
  val ty = TypeRepr.of[A].typeSymbol
  report.errorAndAbort(s"hi $ty ${ty.isClassDef}")

  ???

inline def isElemOf[T, Tys <: Tuple]: Boolean =
  inline erasedValue[Tys] match
    case _: (elem *: elems) =>
      inline (erasedValue[T], erasedValue[elem]) match
        case _: (elem, T) => true
        case _ => isElemOf[T, elems]
    case _: EmptyTuple => false

inline def summonInstances[T, Elems <: Tuple, Excl <: T](custom: => Excl => String): List[Boop[?]] =
  inline erasedValue[Elems] match
    case _: (elem *: elems) =>
      val boop = inline erasedValue[elem] match
        case _: Excl => Boop.BoopImpl(custom)
        case _ => deriveOrSummon[T, elem]
      boop :: summonInstances[T, elems, Excl](custom)
    case _: EmptyTuple => Nil

inline def deriveOrSummon[T, Elem]: Boop[Elem] =
  inline erasedValue[Elem] match
    case _: T => deriveRec[T, Elem]
    case _    => summonInline[Boop[Elem]]

inline def deriveRec[T, Elem]: Boop[Elem] =
  inline erasedValue[T] match
    case _: Elem => error("infinite recursive derivation")
    case _       => Boop.derived[Elem](using summonInline[Mirror.Of[Elem]]) // recursive derivation


inline def boopOfSum[T](m: Mirror.SumOf[T], instances: => List[Boop[?]], x: T): String =
  val idx = m.ordinal(x)

  val name = constValue[m.MirroredLabel]
  val prefix = inline erasedValue[m.MirroredMonoType] match
    case _: scala.reflect.Enum => name + "."
    case _ => ""

  prefix + instances(idx).asInstanceOf[Boop[T]].boop(x)


inline def boopOfProduct[T](m: Mirror.ProductOf[T], instances: => List[Boop[?]], x: T): String =
  val name = constValue[m.MirroredLabel]
  val args = inline m match
    case _: Mirror.Singleton => ""
    case _ =>
      val elems = x.asInstanceOf[Product].productIterator
      val args = (instances zip elems).map((f, x) => f.asInstanceOf[Boop[Any]].boop(x))
      s"(${args.mkString(", ")})"

  name + args


trait Boop[A]:
  extension (x: A) def boop: String

// https://docs.scala-lang.org/scala3/reference/contextual/derivation-macro.html

object Boop {

  class BoopImpl[T](f: T => String) extends Boop[T] {
    extension (x: T) def boop: String = f(x)
  }

  inline def derived[T](using m: Mirror.Of[T]): Boop[T] =
    deriveWithExclusions[T, Nothing](???)

  inline def deriveWithExclusions[T, Excl <: T](using m: Mirror.Of[T])(custom: => Excl => String) =
    lazy val lazyCustom = custom
    lazy val elemInstances = summonInstances[T, m.MirroredElemTypes, Excl](lazyCustom) // if you see an error here, are you missing a given instance?
    println("hi 5 " + constValue[m.MirroredLabel])
    BoopImpl((x: T) =>
      inline m match
        case s: Mirror.SumOf[T] => boopOfSum(s, elemInstances, x)
        case p: Mirror.ProductOf[T] => boopOfProduct[T](p, elemInstances, x)
    )

}

enum EAAA {
  case A
  case B
}

given Boop[Int] with
  extension (x: Int) def boop = x.toString

sealed trait Y derives Boop

sealed trait X extends Y derives Boop
case object X1 extends X
case class X2(x: Int, y: Int, rec: X) extends X
case class X2a(x: Int) extends X
case class X3() extends X

given Boop[EAAA] = Boop.deriveWithExclusions[EAAA, EAAA.A.type](x => "custom" + x.toString)

def go =
  println(isElemOf[Int, (String, String)])
  println(isElemOf[Nothing, (String, String)])
  println(isElemOf[String, (String, String)])
  println(classOf[X2])
  println(s"= ${EAAA.A.boop}")
  println(s"= ${X1.boop}")
  println(s"= ${X2a(29).boop}")
  println(s"= ${X2(10, 20, X1).boop}")
  println(s"= ${X3().boop} ")

