import analysis.{LatticeMap, LatticeSet}
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
    (1, LatticeMap.Top()),
    (1, LatticeMap.Bottom()),
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
  implicit def shrinkLatticeMap[D, L](implicit s: Shrink[L]): Shrink[LatticeMap[D, L]] = Shrink {
    case LatticeMap.Top() => Stream()
    case LatticeMap.Bottom() => Stream()
    case LatticeMap.TopMap(m) => (for (m2 <- Shrink.shrink(m)) yield LatticeMap.topMap(m2))
    case LatticeMap.BottomMap(m) => (for (m2 <- Shrink.shrink(m)) yield LatticeMap.bottomMap(m2))
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
  }
}
