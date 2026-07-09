import analysis.{LatticeMap, LatticeSet, given}
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.*
import org.scalatest.funsuite.*

@test_util.tags.UnitTest
class LatticeCollectionTests extends AnyFunSuite with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {
  type V = Int
  type D = Int
  type L = LatticeSet[Int]

  val genFinSet: Gen[Set[V]] = Gen.nonEmptyContainerOf[Set, V](Arbitrary.arbitrary)

  val genLatticeSet: Gen[LatticeSet[V]] = Gen.frequency(
    (1, LatticeSet.Top[V]()),
    (1, LatticeSet.Bottom[V]()),
    (20, for { s <- genFinSet } yield LatticeSet.FiniteSet(s)),
    (20, for { s <- genFinSet } yield LatticeSet.DiffSet(s))
  )
  implicit lazy val arbLatticeSet: Arbitrary[LatticeSet[V]] = Arbitrary(genLatticeSet)

  val genFinMap: Gen[Map[D, L]] =
    Gen.nonEmptyBuildableOf[Map[D, L], (D, L)](Gen.zip(Arbitrary.arbitrary, Arbitrary.arbitrary))
  val genNoTopFinMap: Gen[Map[D, L]] = Gen.nonEmptyBuildableOf[Map[D, L], (D, L)](
    Gen.zip(Arbitrary.arbitrary, genLatticeSet suchThat (_ != LatticeSet.Top()))
  )
  val genNoBotFinMap: Gen[Map[D, L]] = Gen.nonEmptyBuildableOf[Map[D, L], (D, L)](
    Gen.zip(Arbitrary.arbitrary, genLatticeSet suchThat (_ != LatticeSet.Bottom()))
  )

  val genLatticeMap: Gen[LatticeMap[D, L]] = Gen.frequency(
    (1, LatticeMap.Top[D, L]()),
    (1, LatticeMap.Bottom[D, L]()),
    (20, for { m <- (genNoTopFinMap) } yield LatticeMap.TopMap(m)),
    (20, for { m <- (genNoBotFinMap) } yield LatticeMap.BottomMap(m))
  )
  implicit lazy val arbLatticeMap: Arbitrary[LatticeMap[D, L]] = Arbitrary(genLatticeMap)

  @annotation.nowarn
  implicit def shrinkLatticeSet[T]: Shrink[LatticeSet[T]] = Shrink {
    case LatticeSet.Top() => Stream()
    case LatticeSet.Bottom() => Stream()
    case LatticeSet.FiniteSet(s) => (for (s2 <- Shrink.shrink(s)) yield LatticeSet.finiteSet(s2))
    case LatticeSet.DiffSet(s) => (for (s2 <- Shrink.shrink(s)) yield LatticeSet.diffSet(s2))
  }

  @annotation.nowarn
  implicit def shrinkLatticeMap(implicit s: Shrink[L]): Shrink[LatticeMap[D, L]] = Shrink {
    case LatticeMap.Top() => Stream()
    case LatticeMap.Bottom() => Stream()
    case LatticeMap.TopMap(m) => (for (m2 <- Shrink.shrink(m)) yield LatticeMap.topMap(m2))
    case LatticeMap.BottomMap(m) => (for (m2 <- Shrink.shrink(m)) yield LatticeMap.bottomMap(m2))
  }

  def terms(s: LatticeSet[V]): List[V] = s match {
    case LatticeSet.Top() => List()
    case LatticeSet.Bottom() => List()
    case LatticeSet.FiniteSet(s) => s.toList
    case LatticeSet.DiffSet(s) => s.toList
  }

  test("idempotence") {
    forAll { (s: LatticeSet[V]) =>
      assert(s.join(s).join(s) == s.join(s))
      assert(s.meet(s).meet(s) == s.meet(s))
    }
    forAll { (m: LatticeMap[D, L]) =>
      assert(m.join(m).join(m) == m.join(m))
      assert(m.meet(m).meet(m) == m.meet(m))
    }
  }

  test("set laws") {
    forAll(minSuccessful(1000)) { (s1: LatticeSet[V], s2: LatticeSet[V]) =>
      forAll(Gen.oneOf(terms(s1) ++ terms(s2) ++ List(0))) { (v: V) =>
        assert((s1 union s2).contains(v) == (s1.contains(v) || s2.contains(v)))
        assert((s1 intersect s2).contains(v) == (s1.contains(v) && s2.contains(v)))
        assert((s1 diff s2).contains(v) == (s1.contains(v) && !s2.contains(v)))
      }
    }
  }

  test("pointwise props") {
    forAll(minSuccessful(1000)) { (m1: LatticeMap[D, L], m2: LatticeMap[D, L]) =>
      val join = m1.join(m2)
      val meet = m1.meet(m2)

      // oneOf must take a nonempty collection, so we can choose 0 to evaluate at a point which is not given explicity
      // by a TopMap or BottomMap
      forAll(Gen.oneOf(m1.toMap.keys ++ m2.toMap.keys ++ List(0))) { (d: D) =>
        assert(join(d) == m1(d).join(m2(d)), join)
        assert(meet(d) == m1(d).meet(m2(d)), meet)
      }
    }
    forAll(minSuccessful(1000)) { (m: LatticeMap[D, L], d: D, l: L) =>
      val m2 = m.update(d -> l)
      assert(m2(d) == l)
      forAll(Gen.oneOf(m.toMap.keys ++ List(0))) { (d2: D) =>
        if (d != d2) {
          assert(m2(d2) == m(d2))
        }
      }
    }
  }
}
