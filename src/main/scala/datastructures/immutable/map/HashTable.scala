package datastructures.immutable.map

// Functional implementation of HashMap

// Provides effectively constant-time complexity for all operations
// In the worst case, O(n) due to traversal of all entries in the same hash bucket.

// In JDK 8, HashMap has been tweaked so that if keys can be compared for ordering,
// then any densely populated bucket is implemented as a tree. Thus, even if there are
// a lot of entries with the same hash code, the complexity is O(log n).

// Hence, we could further improve the HashVectorTable implementation by
// using Vector[BinarySearchTree[K, V]] as store instead of Vector[List[(K, V)]]
trait HashTable[K, V] {

  def get(key: K): Option[V]

  def put(key: K, value: V): HashTable[K, V]

  def update(key: K, value: => V): HashTable[K, V]

  def delete(key: K): HashTable[K, V]

  def contains(key: K): Boolean

}

private class HashVectorTable[K, V](store: Vector[List[(K, V)]]) extends HashTable[K, V] {

  private val initialSize: Int = {
    val size = store.size
    HashTable.initChecks(size)
    size
  }

  private def hash(key: K): Int = {
    val hashCode = key.##
    val rem = hashCode % initialSize
    if (rem < 0) rem + initialSize else rem
  }

  override def get(key: K): Option[V] = {
    val i = hash(key)
    store(i).collectFirst { case (k, v) if k == key => v }
  }

  override def put(key: K, value: V): HashVectorTable[K, V] = {
    val i = hash(key)
    val entries = key -> value :: store(i).filterNot(_._1 == key)
    new HashVectorTable(store.updated(i, entries))
  }

  override def update(key: K, value: => V): HashVectorTable[K, V] = {
    get(key).fold(this) { oldVal =>
      val newVal = value
      if (oldVal != newVal) put(key, value)
      else this
    }
  }

  override def delete(key: K): HashTable[K, V] = {
    val i = hash(key)
    val entries = store(i).filter(_._1 != key)
    new HashVectorTable(store.updated(i, entries))
  }

  override def contains(key: K): Boolean = {
    val i = hash(key)
    store(i).exists(_._1 == key)
  }

}

object HashTable {

  def fill[K, V](n: Int): HashTable[K, V] = {
    initChecks(n)
    new HashVectorTable[K, V](Vector.fill(n)(List.empty[(K, V)]))
  }

  def build[K, V](kvs: (K, V)*): HashTable[K, V] =
    kvs.foldLeft(fill[K, V](kvs.size)) { case (memo, (k, v)) => memo.put(k, v) }

  private[map] def initChecks(n: Int): Unit = require(n > 0, "size must be > 0")

}
