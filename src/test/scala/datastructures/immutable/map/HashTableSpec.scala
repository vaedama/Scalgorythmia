package datastructures.immutable.map

import org.scalatest.FunSuite

class HashTableSpec extends FunSuite {

  private val table = {
    val entries = Seq(
      100 -> "hundred",
      200 -> "two hundred",
      300 -> "three hundred"
    )
    HashTable.build(entries: _*)
  }

  test("get") {
    assert(table.get(200).contains("two hundred"))
  }

  test("put") {
    assert(table.put(400, "four hundred").contains(400))
  }

  test("update") {
    assert(table.update(100, "one hundred").get(100).contains("one hundred"))
  }

  test("delete") {
    assert(table.delete(100).get(100).isEmpty)
  }

  test("contains") {
    assert(table.contains(100))
  }

}
