package datastructures.immutable.graph

import scala.annotation.tailrec
import scala.collection.immutable.Queue

// Adjacency List implementation of Graph data structure.
// Each vertex maintains a list of all it's adjacent vertices aka neighbors.
abstract class AdjacencyListGraph[Vertex](private val neighborsByVertex: Map[Vertex, List[Vertex]]) {

  def addVertex(vertex: Vertex): AdjacencyListGraph[Vertex]

  def removeVertex(vertex: Vertex): AdjacencyListGraph[Vertex]

  def addEdge(vertex1: Vertex, vertex2: Vertex): AdjacencyListGraph[Vertex]

  def removeEdge(vertex1: Vertex, vertex2: Vertex): AdjacencyListGraph[Vertex]

  // O(1) time and space
  // because map.keySet returns the DefaultKeySet associated with the map
  def vertices: Set[Vertex] = neighborsByVertex.keySet

  // O(1) time and space
  // with a decent hash
  def neighbors(vertex: Vertex): List[Vertex] = neighborsByVertex.getOrElse(vertex, Nil)

  // O(|V| + |E|) time and space
  // We check each vertex and each edge once (or twice for an undirected graph)
  def bfs(source: Vertex): Stream[Vertex] =
    Stream.iterate(Queue(source) -> Set(source)) { case (queue, visited) =>
      val (head, rest) = queue.dequeue
      val unvisited = neighbors(head).filterNot(visited)
      rest.enqueue(unvisited) -> (visited ++ unvisited)
    }.takeWhile(_._1.nonEmpty).map(_._1.head)

  // Similar to the above "bfs" version without using Stream
  // O(|V| + |E|) time and space
  // We check each vertex and each edge once (or twice for an undirected graph)
  def bfsIterator(source: Vertex): Iterator[Vertex] = {
    @tailrec def loop(queue: Queue[Vertex], visited: Set[Vertex], memo: List[Vertex] = Nil): List[Vertex] =
      if (queue.isEmpty) memo
      else {
        val (head, rest) = queue.dequeue
        val unvisited = neighbors(head).filterNot(visited)
        loop(rest.enqueue(unvisited), visited ++ unvisited, head +: memo)
      }

    loop(Queue(source), Set(source)).reverseIterator
  }

  // O(|V| + |E|) time and space
  // We check each vertex and each edge once (or twice for an undirected graph)
  def dfs(source: Vertex): Stream[Vertex] =
    Stream.iterate(List(source) -> Set(source)) { case (stack, visited) =>
      val head = stack.head
      val unvisited = neighbors(head).filterNot(visited)
      unvisited ++ stack.tail -> (visited ++ unvisited)
    }.takeWhile(_._1.nonEmpty).map(_._1.head)

  // Similar to the above "dfs" version without using Stream
  // O(|V| + |E|) time and space
  // We check each vertex and each edge once (or twice for an undirected graph)
  def dfsIterator(source: Vertex): Iterator[Vertex] = {
    @tailrec def loop(stack: List[Vertex], visited: Set[Vertex], memo: List[Vertex] = Nil): List[Vertex] =
      if (stack.isEmpty) memo
      else {
        val head = stack.head
        val unvisited = neighbors(head).filterNot(visited)
        loop(unvisited ++ stack.tail, visited ++ unvisited, head +: memo)
      }

    loop(List(source), Set(source)).reverseIterator
  }

  // O(|V| + |E|) time and space
  def containsPath(start: Vertex, end: Vertex): Boolean =
    dfs(start).contains(end)

  // O(|V| + |E|) time and space
  def predecessorsByVertex: Map[Vertex, Set[Vertex]] =
    neighborsByVertex.foldLeft(Map.empty[Vertex, Set[Vertex]]) { case (memo, (vertex, neighbors)) =>
      val entryForVertex = vertex -> memo.getOrElse(vertex, Set.empty[Vertex])
      val entriesForNeighbors = neighbors.map(n => n -> (memo.getOrElse(n, Set.empty[Vertex]) + vertex))
      val entries = entriesForNeighbors.toMap + entryForVertex
      memo ++ entries
    }

  // O(|V| + |E|) time and space
  def containsCycle: Boolean = {
    val (verticesWoPredecessors, verticesWithPredecessors) = predecessorsByVertex.partition(_._2.isEmpty)
    verticesWoPredecessors.isEmpty && verticesWithPredecessors.nonEmpty
  }

  def ==(that: AdjacencyListGraph[Vertex]): Boolean =
    neighborsByVertex == that.neighborsByVertex

  override def toString: String =
    neighborsByVertex.map { case (vertex, neighbors) =>
      s"|$vertex| => ${neighbors.mkString(",")}"
    }.mkString("\n")

}

object AdjacencyListGraph {

  def emptyDirected[Vertex]: DiGraph[Vertex] = new DiGraph(Map.empty[Vertex, List[Vertex]])

  def emptyUndirected[Vertex]: UndirectedGraph[Vertex] = new UndirectedGraph(Map.empty[Vertex, List[Vertex]])

  def fillDirected[Vertex](initial: Map[Vertex, List[Vertex]]): DiGraph[Vertex] = new DiGraph(initial)

  def fillUndirected[Vertex](initial: Map[Vertex, List[Vertex]]): UndirectedGraph[Vertex] = new UndirectedGraph(initial)

}
