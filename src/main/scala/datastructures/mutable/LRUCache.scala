package datastructures.mutable

import scala.collection.mutable

// Least Recently Used (LRU) cache implementation
// Invalidates the LRU record when capacity has reached before inserting a new item.
// Intuition:
// 1. get and put and O(1) in HashMap
// 2. invalidation ordering can be maintained by LinkedList
object LRUCache {

  def empty[K, V](capacity: Int, defaultKeyValue: (K, V)): LRUCache[K, V] = {
    new LRUCache(capacity, defaultKeyValue)
  }

}

/**
  * ADT to represent a doubly-linked list node
  *
  * @param key   Key of the node
  * @param value Value of the node
  * @param left  Pointer to previous node
  * @param right Pointer to next node
  * @tparam K Key type
  * @tparam V Value type
  */
private case class DLNode[K, V](key: K, var value: V, var left: DLNode[K, V] = null, var right: DLNode[K, V] = null)

class LRUCache[K, V](capacity: Int, defaultKeyValue: (K, V)) {

  private val (defaultKey, defaultValue) = defaultKeyValue

  private val head = DLNode(defaultKey, defaultValue)
  private val last = DLNode(defaultKey, defaultValue)

  locally {
    head.right = last
    last.left = head
  }

  private val cache = mutable.Map[K, DLNode[K, V]]()
  private var _size = 0

  /**
    * @return Get the value (will always be positive) of the key if the key exists in the cache.
    *         Otherwise return default value.
    */
  // O(1) time and space
  def get(key: K): V = {
    cache.get(key).map { node =>
      pushHead(node)
      node.value
    }.getOrElse(defaultValue)
  }

  /**
    * Set or insert the value if the key is not already present.
    */
  // O(1) time and space
  def put(key: K, value: V): Unit = {
    cache.get(key) match {
      case Some(node) =>
        node.value = value
        pushHead(node)
      case None =>
        val node = DLNode(key, value)
        cache.put(key, node)
        prependToDList(node)
        _size += 1

        if (size > capacity) {
          cache.remove(popLast.key)
          _size -= 1
        }
    }
  }

  def size: Int = _size

  def mru: (K, V) = {
    val node = head.right
    node.key -> node.value
  }

  def lru: (K, V) = {
    val node = last.left
    node.key -> node.value
  }

  private def popLast: DLNode[K, V] = {
    val popped = last.left
    removeFromDList(popped)
    popped
  }

  private def pushHead(node: DLNode[K, V]): Unit = {
    removeFromDList(node)
    prependToDList(node)
  }

  private def removeFromDList(node: DLNode[K, V]): Unit = {
    val left = node.left
    val right = node.right

    left.right = right
    right.left = left
  }

  private def prependToDList(node: DLNode[K, V]): Unit = {
    node.left = head
    node.right = head.right

    head.right.left = node
    head.right = node
  }

}
