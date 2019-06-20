package algorithms.conversions

import scala.annotation.tailrec

object RomanToInt {

  private val dict: Map[Char, Int] = Map(
    'I' -> 1,
    'V' -> 5,
    'X' -> 10,
    'L' -> 50,
    'C' -> 100,
    'D' -> 500,
    'M' -> 1000
  )

  def convert(in: String): Either[IllegalArgumentException, Int] = {
    if (in.trim.length == 0)
      Right(0)
    else if (!in.forall(dict.keySet))
      Left(new IllegalArgumentException(s"Valid roman letters: ${dict.toSeq.sortBy(_._2).map(_._1).mkString(",")}"))
    else {
      @tailrec def loop(rem: Seq[Char], res: Int): Int = rem match {
        case Seq(a, b, t@_*) if dict(a) < dict(b) => loop(t, res + dict(b) - dict(a))
        case Seq(a, t@_*) => loop(t, res + dict(a))
        case _ => res
      }

      Right(loop(in.toSeq, 0))
    }
  }

}
