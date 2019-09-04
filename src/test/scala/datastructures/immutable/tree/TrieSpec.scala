package datastructures.immutable.tree

import org.scalatest.FunSuite

class TrieSpec extends FunSuite {

  val trie: Trie[Int] = Trie.empty[Int]
    .insert("I", 1)
    .insert("said", 2)
    .insert("a", 3)
    .insert("hip", 4)
    .insert("hop", 5)
    .insert("hippy", 6)
    .insert("hippity", 7)
    .insert("to", 8)
    .insert("the", 9)
    .insert("hip", 10)
    .insert("hip", 11)
    .insert("hop", 12)

  test("search") {
    assert(trie.search("hip") === Some(11))
    assert(trie.search("hop") === Some(12))
    assert(trie.search("hippy") === Some(6))
    assert(trie.search("hippity") === Some(7))
    assert(trie.search("hippie") === None)
  }

  test("delete") {
    val newTrie = trie
      .delete("I")
      .delete("said")
      .delete("a")

    assert(newTrie.search("I") === None)
    assert(newTrie.search("said") === None)
    assert(newTrie.search("hippity") === Some(7))
  }

}
