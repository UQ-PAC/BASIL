package ir.dsl

import scala.deriving.*
import scala.compiletime.*

import scala.collection.mutable.AbstractIterable


inline def summonInstances[T, Elems <: Tuple, Excl <: T]: List[Boop[?]] =
  inline erasedValue[Elems] match
    case _: (t1 *: t2 *: t3 *: t4 *: rest) =>
      summonOrCustom[T, t1, Excl]
      :: summonOrCustom[T, t2, Excl]
      :: summonOrCustom[T, t3, Excl]
      :: summonOrCustom[T, t4, Excl]
      :: summonInstances[T, rest, Excl]
    case _: (t1 *: t2 *: rest) =>
      summonOrCustom[T, t1, Excl]
      :: summonOrCustom[T, t2, Excl]
      :: summonInstances[T, rest, Excl]
    case _: (t *: rest) =>
      summonOrCustom[T, t, Excl] :: summonInstances[T, rest, Excl]
    case _: EmptyTuple => Nil

inline def summonOrCustom[T, t, Excl]: Boop[?] =
  inline erasedValue[t] match
    case _: Excl => Boop.BoopImpl(summonInline[Excl => String])
    case _ => deriveOrSummon[T, t, Excl]


inline def deriveOrSummon[T, Elem, Excl]: Boop[Elem] =
  inline (erasedValue[Elem], erasedValue[T]) match
    // Elem <: T and T <: Elem means T == Elem
    case _: (T, Elem) => error("infinite recursive derivation\nadjio")

    // Elem <: T means the element is a subtype and we should recurse
    case _: (T, _) =>
      Boop.deriveWithExclusions(using summonInline[Mirror.Of[Elem]])(summonInline[Excl => String]) // recursive derivation

    // otherwise, including T <: Elem, we summon.
    case _ => summonInline[Boop[Elem]]
    //   summonFrom {
    //   case b: Boop[Elem] => b
    //   case _ => error("No given instance of type ${Elem} was found")
    // }


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

  def absurd[T](x: Nothing): T = throw AssertionError("absurd")

  inline def derived[T](using m: Mirror.Of[T]): Boop[T] =
    deriveWithExclusions[T, Nothing](absurd)

  inline def deriveWithExclusions[T, Excl <: T](using m: Mirror.Of[T])(custom: => Excl => String) =
    given (Excl => String) = custom
    lazy val elemInstances = summonInstances[T, m.MirroredElemTypes, Excl] // if you see an error here, are you missing a given instance?
    println("hi 7 " + constValue[m.MirroredLabel])
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

// sealed trait ASD derives Boop
// case class Branch(l: ASD, r: ASD) extends ASD;
// case object Leaf extends ASD;
//

// sealed trait ASD derives Boop
// case class A() extends ASD
// case class A1() extends ASD
// case class A2() extends ASD
// case class A3() extends ASD
// case class A4() extends ASD
// case class A5() extends ASD
// case class A6() extends ASD
// case class A7() extends ASD
// case class A8() extends ASD
// case class A9() extends ASD
// case class A10() extends ASD
// case class A11() extends ASD
// case class A12() extends ASD
// case class A13() extends ASD
// case class A14() extends ASD
// case class A15() extends ASD
// case class A16() extends ASD
// case class A17() extends ASD
// case class A18() extends ASD
// case class A19() extends ASD
// case class A20() extends ASD
// case class A21() extends ASD
// case class A22() extends ASD
// case class A23() extends ASD
// case class A24() extends ASD
// case class A25() extends ASD
// case class A26() extends ASD
// case class A27() extends ASD

// sealed trait ASDjis
// case class II(a: ASD) extends ASDjis

sealed trait L derives Boop
case object N extends L
case class C(x: Int, l: L, d: Double) extends L

// given Boop[Double] with
//   extension (x: Double) def boop = "doule"

given Boop[EAAA] = Boop.deriveWithExclusions[EAAA, EAAA.A.type]((x: EAAA.A.type) => "custom" + x.toString)

def go =
  println(classOf[X2])
  println(s"= ${EAAA.A.boop}")
  println(s"= ${X1.boop} ")
  println(s"= ${X2a(29).boop}")
  println(s"= ${X2(10, 20, X1).boop}")
  println(s"= ${X3().boop} ")
  println(s"= ${C(100, N, 10.0).boop} ")
  // println(s"= ${(null.asInstanceOf[A]).boop} ")

