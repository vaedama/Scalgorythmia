package datastructures.immutable.tree

case class Trie[V](value: Option[V], children: Vector[Option[Trie[V]]]) {

  def insert(key: String, value: V): Trie[V] = insertInternal(key.toLowerCase, value)

  def delete(key: String): Trie[V] = deleteInternal(key.toLowerCase)

  def search(key: String): Option[V] = searchInternal(key.toLowerCase)

  // Index of 'a' = 0 ... 'z' = 25
  private def indexOf(char: Char): Int = char - 'a'

  private def insertInternal(key: String, value: V): Trie[V] =
    if (key.length == 0) copy(value = Some(value))
    else {
      val index = indexOf(key.head)
      copy(children = children.updated(index, Some(children(index)
        .getOrElse(Trie.empty[V])
        .insertInternal(key.tail, value))))
    }

  private def deleteInternal(key: String): Trie[V] =
    if (key.length == 0) copy(value = None)
    else {
      val index = indexOf(key.head)
      children(index) match {
        case None => this
        case Some(nextItem) =>
          val newNode = nextItem.deleteInternal(key.tail)
          copy(children =
            if (newNode.value.isEmpty && newNode.children.forall(_.isEmpty))
              children.updated(index, None)
            else
              children.updated(index, Some(newNode)))
      }
    }

  private def searchInternal(key: String): Option[V] =
    if (key.length == 0) value
    else children(indexOf(key.head)) match {
      case Some(child) => child.searchInternal(key.tail)
      case None => None
    }

  override def toString: String =
    s"Trie(value=$value, children=${
      children.map {
        case None => ""
        case Some(t) => t
      }
    })"

}

object Trie {

  def empty[V]: Trie[V] = new Trie[V](None, Vector.fill(26)(None))

}
