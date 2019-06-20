package datastructures.mutable

import org.scalatest.FunSuite

class LRUCacheSpec extends FunSuite {

  private val cache = LRUCache.empty(2, (-1, -1))

  test("put") {
    cache.put(1, 1)
    cache.size == 1
  }

  test("mru after put") {
    cache.put(2, 2)
    cache.mru == (2, 2)
  }

  test("mru after get") {
    cache.get(1)
    cache.mru == (1, 1)
  }

  test("evict lru") {
    cache.put(3, 3)
    assert(cache.get(2) == -1)
  }

}
