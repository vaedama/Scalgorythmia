package datastructures.immutable.graph

class DiGraph[Vertex](store: Map[Vertex, List[Vertex]]) extends AdjacencyListGraph[Vertex](store) {

  // O(1) time and space
  override def addVertex(vertex: Vertex): DiGraph[Vertex] =
    if (!store.contains(vertex)) new DiGraph(store.updated(vertex, Nil))
    else this

  // O(|V| + |E|) time and space
  override def removeVertex(vertex: Vertex): DiGraph[Vertex] =
    if (!store.contains(vertex)) this
    else new DiGraph((store - vertex).mapValues(_.filter(_ != vertex)))

  // O(1) time and space
  override def addEdge(vertex1: Vertex, vertex2: Vertex): DiGraph[Vertex] =
    new DiGraph(store
      .updated(vertex1, vertex2 +: store.getOrElse(vertex1, Nil))
      .updated(vertex2, store.getOrElse(vertex2, Nil))
    )

  // O(|E|) time and space
  override def removeEdge(vertex1: Vertex, vertex2: Vertex): DiGraph[Vertex] =
    if (store.contains(vertex1)) new DiGraph(store.updated(vertex1, store(vertex1).filter(_ != vertex2)))
    else this

}

