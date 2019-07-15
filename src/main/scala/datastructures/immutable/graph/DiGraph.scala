package datastructures.immutable.graph

class DiGraph[Vertex](store: Map[Vertex, List[Vertex]]) extends UndirectedGraph[Vertex](store) {

  // O(1) time and space
  override def addEdge(v1: Vertex, v2: Vertex): DiGraph[Vertex] =
    new DiGraph(store + (v1 -> (v2 +: store.getOrElse(v1, Nil))))

  // O(|E|) time and space
  override def removeEdge(v1: Vertex, v2: Vertex): DiGraph[Vertex] =
    if (store.contains(v1)) new DiGraph(store.updated(v1, store(v1).filter(_ != v2)))
    else this

}

