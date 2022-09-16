package traffic.service

import cats._
import cats.implicits._
import traffic.service.DijkstraService.{Cost, Edge, Graph}

import scala.annotation.tailrec

trait DijkstraService[N] {
  def pathWithCost(
                    graph: Graph[N],
                    source: N,
                    goal: N
                  ): Option[(List[N], Cost)]

  def isReachable(graph: Graph[N], source: N, goal: N): Boolean
  def getNodes(graph: Graph[N]): Set[N]
  def buildGraph(edges: Set[Edge[N]]): Graph[N]
}

object DijkstraService {
  type Cost = BigDecimal
  type Edge[N] = (N, N, Cost)
  type Graph[N] = Map[N, Map[N, Cost]]

  def impl[N: Eq]: DijkstraService[N] = new DijkstraService[N] {
    def dijkstra(graph: Graph[N], source: N, target: N): (Map[N, Cost], Map[N, N]) = {
      @tailrec def dijkstraRec(
                                active: Set[N],
                                costFromSource: Map[N, Cost],
                                parents: Map[N, N]
                              ): (Map[N, Cost], Map[N, N]) = {
        val maybeCurrentNode = active.minByOption(costFromSource)
        maybeCurrentNode.filterNot(_ === target) match {
          //Either we are at the target node or the active set is empty
          case None => (costFromSource, parents)
          case Some(currentNode) =>
            val currentCost = costFromSource(currentNode)
            //Using Int.MaxValue seems big enough to work as an upper bound for the values and amount of nodes in the file
            val neighbours =
              graph.get(currentNode).combineAll
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

    def reachableNodes(graph: Graph[N], source: N): Set[N] = {
      @tailrec def reachableRec(from: Set[N], acc: Set[N]): Set[N] = {
        if (from.isEmpty) acc
        else {
          val explored = acc ++ from
          val neighbours = from.flatMap(n => graph.get(n)).flatMap(_.keys).diff(explored)
          reachableRec(neighbours, explored)
        }
      }

      if(graph.contains(source)) reachableRec(Set(source), Set.empty)
      else Set.empty
    }

    override def isReachable(graph: Graph[N], source: N, goal: N): Boolean = reachableNodes(graph, source).contains(goal)

    override def getNodes(graph: Graph[N]): Set[N] = graph.flatMap(edge => edge._2.keys.toSet + edge._1).toSet

    override def buildGraph(edges: Set[Edge[N]]): Graph[N] = {
      edges
        .groupMap(_._1) { case (_, end, cost) =>
          end -> cost
        }
        .view
        .mapValues(_.toMap)
        .toMap
    }

    //Assumes the source is in the graph
    def pathWithCost(
                      graph: Graph[N],
                      source: N,
                      goal: N
                    ): Option[(List[N], Cost)] = {
      @tailrec def buildPathToTarget(
                                      target: N,
                                      parents: Map[N, N],
                                      acc: List[N]
                                    ): List[N] = {
        val parent = parents(target)
        if (parent === source) parent +: acc
        else buildPathToTarget(parent, parents, parent +: acc)
      }

      if (source === goal) Some(List(), BigDecimal(0))
      else {
        val (costsFromSource, parents) = dijkstra(graph, source, goal)
        if (costsFromSource.contains(goal)) {
          val path = buildPathToTarget(goal, parents, List.empty)
          val cost = costsFromSource(goal)
          Some((path, cost))
        } else None
      }
    }
  }
}