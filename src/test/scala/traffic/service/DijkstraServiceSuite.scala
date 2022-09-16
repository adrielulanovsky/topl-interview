package traffic.service

import cats.effect.IO
import cats.implicits.catsSyntaxEq
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.specs2.mutable.Specification
import traffic.service.DijkstraService.Graph
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import traffic.Generators

class DijkstraServiceSuite
    extends AnyFunSuite
    with ScalaCheckDrivenPropertyChecks
    with Generators {
  val service: DijkstraService[Int] = DijkstraService.impl

  test(
    "pathWithCost should be the empty list if the source is the same as the target"
  ) {
    forAll { (source: Int, graph: Graph[Int]) =>
      whenever(graph.contains(source)) {
        assert(
          service.pathWithCost(graph, source, source) eqv Some(
            List.empty[Int],
            BigDecimal(0)
          )
        )
      }
    }
  }
}