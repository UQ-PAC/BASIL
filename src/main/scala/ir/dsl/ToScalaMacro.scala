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

def boopOfProduct[T](p: Mirror.ProductOf[T], elems: => List[Boop[?]]): Boop[T] =
  new Boop[T]:
    def boop(x: T): String =
      // XXX: "not a constant type". just pull out all needed things within the derived function and pass through
      constValue[p.MirroredLabel]

trait Boop[A]:
  def boop(x: A): String


case class Entity(id: Int, value: String) derives Boop

// https://docs.scala-lang.org/scala3/reference/contextual/derivation-macro.html

object Boop {

  inline def derived[T](using m: Mirror.Of[T]): Boop[T] =
    lazy val elemInstances = summonInstances[T, m.MirroredElemTypes] // (1)
    println("hi")

    inline erasedValue[m.MirroredMonoType] match
      case _: scala.reflect.Enum => println("is enum")
      case _ => ()

    val a = inline m match                                                   // (2)
      case s: Mirror.SumOf[T]     =>
        println("sum label " + constValue[s.MirroredLabel])
        boopOfSum(s, elemInstances)
      case p: Mirror.ProductOf[T] =>
        println("prod label " + constValue[p.MirroredLabel])
        // println(s"prod type ${p.MirroredType}")

        boopOfProduct[T](p, elemInstances)

    println("bye")
    a
}

enum EAAA {
  case A
  case B
}

given Boop[Int] = ???
given Boop[String] = ???
given Boop[None.type] = Boop.derived
given Boop[ir.Endian] = Boop.derived
given Boop[EAAA] = Boop.derived
val x = summon[Boop[Entity]].boop(Entity(1, "a"))
val y = summon[Boop[EAAA]].boop(EAAA.B)
