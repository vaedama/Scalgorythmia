package datastructures.immutable.graph

class UndirectedGraph[Vertex](store: Map[Vertex, List[Vertex]]) extends AdjacencyListGraph[Vertex](store) {

  // O(1) time and space
  override def addVertex(vertex: Vertex): UndirectedGraph[Vertex] =
    if (!store.contains(vertex)) new UndirectedGraph(store.updated(vertex, Nil))
    else this

  // O(|V| + 2*|E|) time and space
  override def removeVertex(vertex: Vertex): UndirectedGraph[Vertex] =
    if (!store.contains(vertex)) this
    else new UndirectedGraph(store.filterKeys(_ == vertex).mapValues(_.filter(_ != vertex)))

  // O(1) time and space
  override def addEdge(vertex1: Vertex, vertex2: Vertex): UndirectedGraph[Vertex] =
    new UndirectedGraph(store
      .updated(vertex1, vertex2 +: store.getOrElse(vertex1, Nil))
      .updated(vertex2, vertex1 +: store.getOrElse(vertex2, Nil)))

  // O(2*|E|) time and space
  override def removeEdge(vertex1: Vertex, vertex2: Vertex): UndirectedGraph[Vertex] =
    new UndirectedGraph(store
      .updated(vertex1, store.getOrElse(vertex1, Nil).filter(_ != vertex2))
      .updated(vertex2, store.getOrElse(vertex2, Nil).filter(_ != vertex1)))

}
