import analysis.{LatticeMap, LatticeSet}
import ir.*
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.*
import org.scalatest.funsuite.*
import translating.PrettyPrinter.*

@test_util.tags.UnitTest
class LatticeCollectionTests extends AnyFunSuite with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {
  type V = Int
  type D = Int
  type L = LatticeSet[Int]

  private implicit val latticeSetTerm: LatticeSet[V] = LatticeSet.Bottom()

  val genFinSet: Gen[Set[V]] = Gen.nonEmptyContainerOf[Set, V](Arbitrary.arbitrary)

  val genLatticeSet: Gen[LatticeSet[V]] = Gen.frequency(
    (1, LatticeSet.Top()),
    (1, LatticeSet.Bottom()),
    (20, for { s <- genFinSet } yield LatticeSet.FiniteSet(s)),
    (20, for { s <- genFinSet } yield LatticeSet.DiffSet(s)),
  )
  implicit lazy val arbLatticeSet: Arbitrary[LatticeSet[V]] = Arbitrary(genLatticeSet)

  val genFinMap: Gen[Map[D, L]] = Gen.buildableOf[Map[D, L], (D, L)](Gen.zip(Arbitrary.arbitrary, Arbitrary.arbitrary))

  val genLatticeMap: Gen[LatticeMap[D, L]] = Gen.frequency(
    (1, LatticeMap.Top()),
    (1, LatticeMap.Bottom()),
    (20, for { m <- (genFinMap suchThat (!_.values.toSet.contains(LatticeSet.Top()))) } yield LatticeMap.TopMap(m)),
    (20, for { m <- (genFinMap suchThat (!_.values.toSet.contains(LatticeSet.Bottom()))) } yield LatticeMap.BottomMap(m)),
  )
  implicit lazy val arbLatticeMap: Arbitrary[LatticeMap[D, L]] = Arbitrary(genLatticeMap)

  implicit def shrinkLatticeSet[T]: Shrink[LatticeSet[T]] = Shrink {
    case LatticeSet.Top() => Stream()
    case LatticeSet.Bottom() => Stream()
    case LatticeSet.FiniteSet(s) => (for (s2 <- Shrink.shrink(s)) yield LatticeSet.FiniteSet(s2))
    case LatticeSet.DiffSet(s) => (for (s2 <- Shrink.shrink(s)) yield LatticeSet.DiffSet(s2))
  }

  implicit def shrinkLatticeMap[D, L](implicit s: Shrink[L]): Shrink[LatticeMap[D, L]] = Shrink {
    case LatticeMap.Top() => Stream()
    case LatticeMap.Bottom() => Stream()
    case LatticeMap.TopMap(m) => (for (m2 <- Shrink.shrink(m)) yield LatticeMap.TopMap(m2))
    case LatticeMap.BottomMap(m) => (for (m2 <- Shrink.shrink(m)) yield LatticeMap.BottomMap(m2))
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

  test("pointwise props") {
    forAll { (m1: LatticeMap[D, L], m2: LatticeMap[D, L]) =>
      val join = m1.join(m2)
      val meet = m1.meet(m2)
      forAll { (d: D) =>
        assert(join(d) == m1(d).join(m2(d)))
        assert(meet(d) == m1(d).meet(m2(d)))
      }
    }
  }
}
