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
    case _       => Boop.derived[Elem](using summonInline[Mirror.Of[Elem]]) // recursive derivation

trait Boop[A]:
  def boop: Int


case class Entity(id: Int, value: String) derives Boop

// https://docs.scala-lang.org/scala3/reference/contextual/derivation-macro.html

object Boop {
  inline def derived[T](using m: Mirror.Of[T]): Boop[T] =
    // lazy val elemInstances = summonInstances[T, m.MirroredElemTypes] // (1)
    inline m match                                                   // (2)
      case s: Mirror.SumOf[T]     =>
        new Boop[T]:
          def boop = 2
      case p: Mirror.ProductOf[T] =>
        new Boop[T]:
          def boop =
            println(m.toString)
            10

}

given Boop[None.type] = Boop.derived
given Boop[Option[Boolean]] = Boop.derived
given Boop[ir.Endian] = Boop.derived
val x = summon[Boop[Entity]].boop
