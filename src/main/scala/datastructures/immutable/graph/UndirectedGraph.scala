package datastructures.immutable.graph

class UndirectedGraph[Vertex](store: Map[Vertex, List[Vertex]]) extends AdjacencyListGraph[Vertex](store) {

  // O(1) time and space
  override def addVertex(v: Vertex): AdjacencyListGraph[Vertex] =
    if (!store.contains(v)) new UndirectedGraph(store.updated(v, Nil))
    else this

  // O(V) time and space
  override def removeVertex(v: Vertex): AdjacencyListGraph[Vertex] =
    if (!store.contains(v)) this
    else new DiGraph((store - v).mapValues(_.filter(_ != v)))

  // O(1) time and space
  override def addEdge(v1: Vertex, v2: Vertex): AdjacencyListGraph[Vertex] = {
    val v1Entry = v1 -> (v2 +: store.getOrElse(v1, Nil))
    val v2Entry = v2 -> (v1 +: store.getOrElse(v2, Nil))
    new UndirectedGraph(store + v1Entry + v2Entry)
  }

  // O(2*|E|) time and space
  override def removeEdge(v1: Vertex, v2: Vertex): AdjacencyListGraph[Vertex] = {
    val v1Entry = v1 -> store.getOrElse(v1, Nil).filter(_ != v2)
    val v2Entry = v2 -> store.getOrElse(v2, Nil).filter(_ != v1)
    new UndirectedGraph(store + v1Entry + v2Entry)
  }

}
