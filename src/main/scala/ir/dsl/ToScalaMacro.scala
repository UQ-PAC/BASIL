package ir.dsl

import scala.quoted.*
import scala.deriving.*
import scala.compiletime.*

inline def boop[A] = ${ boopImpl[A]() }

def generateForCase[A: Type](using Quotes)(e: Expr[A]): Expr[String] =
  ???

def boopImpl[A: Type](using Quotes)(): Expr[Int] =
  import quotes.reflect.*
  val ty = TypeRepr.of[A].typeSymbol
  report.errorAndAbort(s"hi $ty ${ty.isClassDef}")

  ???


inline def summonInstances[T, Elems <: Tuple]: List[Boop[?]] =
  inline erasedValue[Elems] match
    case _: (elem *: elems) => deriveOrSummon[T, elem] :: summonInstances[T, elems]
    case _: EmptyTuple => Nil

inline def deriveOrSummon[T, Elem]: Boop[Elem] =
  inline erasedValue[Elem] match
    case _: T => deriveRec[T, Elem]
    case _    => summonInline[Boop[Elem]]

inline def deriveRec[T, Elem]: Boop[Elem] =
  inline erasedValue[T] match
    case _: Elem => error("infinite recursive derivation")
    case _       => Boop.derived[Elem] // recursive derivation

inline def elemTypes[Elems <: Tuple](using quotes: Quotes): List[quotes.reflect.TypeRepr] =
  import quotes.reflect.*
  inline erasedValue[Elems] match
    case _: (elem *: elems) => TypeRepr.of[elem] :: elemTypes[elems]
    case _: EmptyTuple => Nil

def boopOfSum[T](s: Mirror.SumOf[T], elems: => List[Boop[?]]): Boop[T] =
  new Boop[T]:
    def boop(x: T): String =
      val idx = s.ordinal(x)
      elems(s.ordinal(x)).asInstanceOf[Boop[T]].boop(x)

inline def boopOfProduct[T](s: Mirror.ProductOf[T], elems: => List[Boop[?]]): Boop[T] =
  val tys = elemTypes[s.MirroredElemTypes]
  new Boop[T]:
    def boop(x: T): String =

      "boop of product"


trait Boop[A]:
  def boop(x: A): String

// https://docs.scala-lang.org/scala3/reference/contextual/derivation-macro.html

object Boop {

  inline def derived[T]: Boop[T] = ${ derivedMacro[T] }

  inline def derivedMirror[T](m: Mirror.Of[T], instances: List[Boop[?]]): Boop[T] =
    inline m match                                                   // (2)
      case s: Mirror.SumOf[T]     => boopOfSum(s, elemInstances)
      case p: Mirror.ProductOf[T] => boopOfProduct(p, elemInstances)


  def derivedMacro[T: Type](using Quotes): Expr[Boop[T]] =
    import quotes.reflect.*

    val expr: Expr[Mirror.Of[T]] = Expr.summon[Mirror.Of[T]].get
    expr match
      case '{ $m: Mirror.Of[T] } => '{ derivedMirror[T]($m) }

    //
}

