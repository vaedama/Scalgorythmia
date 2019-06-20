package algorithms.sorting

import scala.annotation.tailrec

/**
  * Topological sort or topological ordering of a directed graph is a linear ordering
  * of its vertices such that for every directed edge (U, V) from vertex U to V,
  * U comes before V in the ordering.
  *
  * Applications: Dependency ordering
  */
object TopologicalSort {

  /*
  Examples:
  =========

  Example 1:
  ----------
  Input: Vertices=4, Edges=[3, 2], [3, 0], [2, 0], [2, 1]

  3--->2--->1
  |    |
  |    |
  v    |
  0<---

  Output: Following are the two valid topological sorts for the given graph:
  1) 3, 2, 0, 1
  2) 3, 2, 1, 0

  Example 2:
  ----------
  Input: Vertices=5, Edges=[4, 2], [4, 3], [2, 0], [2, 1], [3, 1]
  Output: Following are all valid topological sorts for the given graph:
  1) 4, 2, 3, 0, 1
  2) 4, 3, 2, 0, 1
  3) 4, 3, 2, 1, 0
  4) 4, 2, 3, 1, 0
  5) 4, 2, 0, 3, 1

  Example 3:
  ----------
  Input: Vertices=7, Edges=[6, 4], [6, 2], [5, 3], [5, 4], [3, 0], [3, 1], [3, 2], [4, 1]
  Output: Following are all valid topological sorts for the given graph:
  1) 5, 6, 3, 4, 0, 1, 2
  2) 6, 5, 3, 4, 0, 1, 2
  3) 5, 6, 4, 3, 0, 2, 1
  4) 6, 5, 4, 3, 0, 1, 2
  5) 5, 6, 3, 4, 0, 2, 1
  6) 5, 6, 3, 4, 1, 2, 0

  Terminology:
  ============
  1. Source: Any node that has no incoming edge and has only outgoing edge(s) is called a Source.
  2. Sink: Any node that has no outgoing edges and has only incoming edge(s) is called a Sink.

  Intuition:
  ===========
  1. A topological ordering starts with one of the sources and ends at one of the sinks.
  2. A topological ordering is only possible with Directed Acyclic Graphs (DAG).
  A cycle exists in a graph if the graph doesn't contain at least one source.
  */

  sealed trait SortResult

  case class SortedOrder[A](result: Seq[A]) extends SortResult

  case class CycleDetectedError[A](cycles: Map[A, Set[A]]) extends SortResult

  // O(V+E) time and space
  def sort[A](edges: Traversable[(A, A)]): Either[CycleDetectedError[A], SortedOrder[A]] = {
    val predecessorsByVertex = edges.foldLeft(Map.empty[A, Set[A]]) { case (memo, (v1, v2)) =>
      memo + (v1 -> memo.getOrElse(v1, Set.empty[A])) + (v2 -> (memo.getOrElse(v1, Set.empty[A]) + v1))
    }

    @tailrec def dfs(predecessorsByVertex: Map[A, Set[A]], memo: Seq[A] = Nil): Either[CycleDetectedError[A], SortedOrder[A]] = {
      val (noPre, hasPre) = predecessorsByVertex.partition(_._2.isEmpty)

      if (noPre.isEmpty) {
        if (hasPre.isEmpty) Right(SortedOrder(memo))
        else Left(CycleDetectedError(hasPre.filter(vertexAndPre => vertexAndPre._2(vertexAndPre._1))))
      } else {
        val sources = noPre.keys
        dfs(hasPre.mapValues(_ -- sources), memo ++ sources)
      }
    }

    dfs(predecessorsByVertex)
  }

}


