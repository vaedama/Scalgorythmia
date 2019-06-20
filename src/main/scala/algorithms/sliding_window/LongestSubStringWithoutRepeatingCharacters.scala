package algorithms.sliding_window

import scala.annotation.tailrec
import scala.collection.mutable

/*
Intuition: "sub-string" => maintain two pointers

Iterate through the string by maintaining two pointers: "start" and "end".
Store each character in an occurrence hash set.

If the current character exists in the hast set:
  - remove "start" character from occurrence hash set
  - then advance "start" pointer.

If the current character does not exist in the hast set:
  - add "current" character to the occurrence hash set
  - advance "end" pointer
  - increment len if end - start > current length
 */
object LongestSubStringWithoutRepeatingCharacters {

  def lengthIterative(in: String): Int = {
    var start = 0
    var end = 0
    var occ = mutable.Set[Char]()
    var len = 0

    while (in.length > end) {
      val cur = in.charAt(end)
      if (occ(cur)) {
        occ -= in.charAt(start)
        start += 1
      } else {
        occ += cur
        end += 1
        if (end - start > len) len = len + 1
      }
    }
    len
  }

  def lengthRecursive(in: String): Int = {
    @tailrec def loop(start: Int, end: Int, occ: Set[Char], len: Int): Int =
      if (end == in.length) len
      else {
        val cur = in.charAt(end)
        if (occ(cur)) loop(start + 1, end, occ - in.charAt(start), len)
        else loop(start, end + 1, occ + cur, if (end - start < len) len else len + 1)
      }

    loop(0, 0, Set.empty, 0)
  }

}
