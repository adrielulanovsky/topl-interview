package traffic

import cats._
import cats.implicits._
import org.scalacheck.Arbitrary.arbitrary
import traffic.model.Model.Intersection
import org.scalacheck.{Arbitrary, Gen}
import traffic.main.Main
import traffic.service.DijkstraService
import traffic.service.DijkstraService.{Cost, Edge, Graph}

trait Generators {
  implicit val intersectionArb: Arbitrary[Intersection] = Arbitrary {
    for {
      avenue <- Gen.alphaUpperStr.suchThat(_.nonEmpty)
      street <- Gen.numStr.suchThat(_.nonEmpty)
    } yield Intersection(avenue, street)
  }

  def validIntersectionStringGen: Gen[String] =
    intersectionArb.arbitrary.map(i => i.avenue + i.street)

  def invalidIntersectionStringGen: Gen[String] = {
    Gen.oneOf(
      Gen.alphaUpperStr,
      Gen.numStr,
      Gen.alphaLowerStr.flatMap(a => Gen.numStr.flatMap(b => a+b))
    )
  }

  implicit def tupleArb[A, B](implicit
      arbA: Arbitrary[A],
      arbB: Arbitrary[B]
  ): Arbitrary[(A, B)] = Arbitrary {
    for {
      a <- arbA.arbitrary
      b <- arbB.arbitrary
    } yield (a, b)
  }

  implicit def edgeArb[N](implicit arbN: Arbitrary[N]): Arbitrary[Edge[N]] = Arbitrary {
    for {
      n <- arbN.arbitrary
      m <- arbN.arbitrary
      cost <- Gen.posNum[BigDecimal]
    } yield (m, n, cost)
  }

  implicit def graphArb[N: Eq](implicit arbN: Arbitrary[N]): Arbitrary[Graph[N]] = Arbitrary {
    Gen.nonEmptyListOf(edgeArb[N].arbitrary).map(_.toSet).map(DijkstraService.impl[N].buildGraph)
  }

  def edgeWithoutNode(node: Int): Gen[Edge[Int]] =
    Arbitrary.arbInt.arbitrary.filterNot(i => i === node).flatMap { i =>
      Arbitrary.arbInt.arbitrary.filterNot(i => i === node).flatMap { j =>
        Arbitrary.arbitrary[BigDecimal].flatMap { cost =>
          (i, j, cost)
        }
      }
    }

  def graphWithoutNode(node: Int): Gen[Graph[Int]] = {
    Gen.nonEmptyListOf(edgeWithoutNode(node)).map(_.toSet).map(DijkstraService.impl[Int].buildGraph)
  }
}
