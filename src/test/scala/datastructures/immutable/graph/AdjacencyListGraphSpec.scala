package datastructures.immutable.graph

import org.scalatest.FunSuite

class AdjacencyListGraphSpec extends FunSuite {

  /*

"Sacramento"
    ^
    |
    |
"San Francisco" --------------------
    |                               |
    |                               |
    V                               |
"San Jose" --------> "Santa Cruz"   |
    | |                             |
    | ------------------            |
    |                   |           |
    V                   V           |
"Los Angeles" ------> "Las Vegas" <-
    |
    |
    V
"San Diego"

*/

  private val diGraph = AdjacencyListGraph.emptyDirected[String]
    .addEdge("San Francisco", "Sacramento")
    .addEdge("San Francisco", "San Jose")
    .addEdge("San Francisco", "Las Vegas")

    .addEdge("San Jose", "Los Angeles")
    .addEdge("San Jose", "Santa Cruz")
    .addEdge("San Jose", "Las Vegas")

    .addEdge("Los Angeles", "San Diego")
    .addEdge("Los Angeles", "Las Vegas")


  test("==") {
    val diGraphFilled = AdjacencyListGraph.fillDirected(
      Map(
        "San Francisco" -> List("Las Vegas", "San Jose", "Sacramento"),
        "Las Vegas" -> Nil,
        "San Jose" -> List("Las Vegas", "Santa Cruz", "Los Angeles"),
        "Sacramento" -> Nil,
        "Santa Cruz" -> Nil,
        "Los Angeles" -> List("Las Vegas", "San Diego"),
        "San Diego" -> Nil
      ))

    assert(diGraph == diGraphFilled)
  }

  test("bfs") {
    val actual = diGraph.bfs("San Francisco").toList
    val expected = List("San Francisco", "Las Vegas", "San Jose", "Sacramento", "Santa Cruz", "Los Angeles", "San Diego")
    assert(actual === expected)
  }

  test("bfsIterator") {
    val actual = diGraph.bfsIterator("San Francisco").toList
    val expected = List("San Francisco", "Las Vegas", "San Jose", "Sacramento", "Santa Cruz", "Los Angeles", "San Diego")
    assert(actual === expected)
  }

  test("dfs") {
    val actual = diGraph.dfs("San Francisco").toList
    val expected = List("San Francisco", "Las Vegas", "San Jose", "Santa Cruz", "Los Angeles", "San Diego", "Sacramento")
    assert(actual === expected)
  }

  test("dfsIterator") {
    val actual = diGraph.dfsIterator("San Francisco").toList
    val expected = List("San Francisco", "Las Vegas", "San Jose", "Santa Cruz", "Los Angeles", "San Diego", "Sacramento")
    assert(actual === expected)
  }

  test("containsPath: positive case") {
    assert(diGraph.containsPath("San Francisco", "San Diego"))
  }

  test("containsPath: negative case") {
    assert(!diGraph.containsPath("Sacramento", "San Diego"))
  }

  /*
  0 ----> 1
  ^       |
  |       |
  |       |
  2<------
   */
  private val cyclicGraph = AdjacencyListGraph.fillDirected(Map(
    0 -> List(1),
    1 -> List(2),
    2 -> List(0)
  ))
  private val treeGraph = AdjacencyListGraph.fillDirected(Map(
    0 -> List(1),
    1 -> List(2),
    2 -> List(3)
  ))
  private val disconnectedGraph = AdjacencyListGraph.fillDirected(Map(
    0 -> List(1),
    2 -> List(3)
  ))

  test("containsCycle: positive case") {
    assert(cyclicGraph.containsCycle)
  }

  test("containsCycle: negative case") {
    assert(!treeGraph.containsCycle)
  }

  test("containsCycle: disconnected graph") {
    assert(!disconnectedGraph.containsCycle)
  }

}
