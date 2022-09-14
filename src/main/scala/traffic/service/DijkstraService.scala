package traffic.service

import cats._
import cats.implicits._
import traffic.service.DijkstraService.{Cost, Graph}

import scala.annotation.tailrec

trait DijkstraService[N] {
  def dijkstra(
      graph: Graph[N],
      source: N,
      target: N
  ): (Map[N, Cost], Map[N, N])
  def pathWithCost(
                    graph: Graph[N],
                    source: N,
                    goal: N
                  ): Option[(List[N], Cost)]
}




object DijkstraService {
  type Cost = BigDecimal
  type Graph[N] = Map[N, Map[N, Cost]]

  def impl[N: Eq]: DijkstraService[N] = new DijkstraService[N] {
    override def dijkstra(graph: Graph[N], source: N, target: N): (Map[N, Cost], Map[N, N]) = {
      @tailrec def dijkstraRec(
                                active: Set[N],
                                costFromSource: Map[N, Cost],
                                parents: Map[N, N]
                              ): (Map[N, Cost], Map[N, N]) = {
        val currentNode = active.minBy(costFromSource)
        if (active.isEmpty || currentNode === target) (costFromSource, parents)
        else {
          val currentCost = costFromSource(currentNode)
          //Using Int.MaxValue seems big enough to work as an upper bound for the values and amount of nodes in the file
          val neighbours =
            graph(currentNode)
              .filter { case (n, c) =>
                currentCost + c < costFromSource.getOrElse(n, Int.MaxValue)
              }
              .view
              .mapValues(_ + currentCost)
              .toMap
          val neighboursParents =
            neighbours.view.mapValues(_ => currentNode).toMap
          dijkstraRec(
            active - currentNode ++ neighbours.keys,
            costFromSource ++ neighbours,
            parents ++ neighboursParents
          )
        }
      }

      dijkstraRec(Set(source), Map(source -> 0), Map.empty)
    }

    def pathWithCost(
                      graph: Graph[N],
                      source: N,
                      goal: N
                    ): Option[(List[N], Cost)] = {
      //Assumes the target node is in the parents map and
      @tailrec def buildPathToTarget(
                                      target: N,
                                      parents: Map[N, N],
                                      acc: List[N]
                                    ): List[N] = {
        val parent = parents(target)
        if (parent === source) parent +: acc
        else buildPathToTarget(parent, parents, parent +: acc)
      }

      val (costsFromSource, parents) = dijkstra(graph, source, goal)
      if (costsFromSource.contains(goal)) {
        val path = buildPathToTarget(goal, parents, List.empty)
        val cost = costsFromSource(goal)
        Some((path, cost))
      } else None
    }
  }
}