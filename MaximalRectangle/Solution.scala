object Solution {
  import math.max
  
  case class Segment(start: Int, end: Int) extends Ordered[Segment] {
    override def compare(that: Segment) =
      Integer.compare(this.start, that.start)
    
    def size: Int = end - start

    def isDisjoint(that: Segment): Boolean = {
      end <= that.start || start >= that.end
    }

    def & (that: Segment): Option[Segment] = {
      import math.{min, max}
      if (this.isDisjoint(that)) {
        None
      } else {
        Some(Segment(max(start, that.start), min(end, that.end)))
      }
    }
  }
  
  
  @scala.annotation.tailrec
  def segment(row: Array[Char], start: Int = 0, acc: List[Segment] = Nil): List[Segment] = {
    row.indexOf('1', start) match {
      case -1 =>
        acc.reverse
      case one =>
        row.indexOf('0', one) match {
          case -1 =>
            segment(row, row.length, Segment(one, row.length) :: acc)
          case zero =>
            segment(row, zero, Segment(one, zero) :: acc)
        }
    }
  }
  
  def merge(as: List[Segment], bs: List[Segment], acc: List[Segment] = Nil): List[Segment] = {
    (as, bs) match {
      case (Nil, _) =>
        acc.reverse
      case (_, Nil) =>
        acc.reverse
      case (a :: ass, b :: bss) if a.isDisjoint(b) =>
        if (a.start < b.start) merge(ass, bs, acc) else merge(as, bss, acc)
      case (a :: ass, b :: bss) =>
        if (a.end < b.end) merge(ass, bs, (a & b).get :: acc) else merge(as, bss, (a & b).get :: acc)
    }
  }

  def maximalRectangle(matrix: Array[Array[Char]]): Int = {
    val m = matrix.size
    val n = matrix(0).size
    
    val segments = Range(0, m).map(i => segment(matrix(i)))
    val rects0: Map[(Int,Int),List[Segment]] = Range(0, m).map(i => (i,i) -> segments(i)).toMap
    
    @scala.annotation.tailrec
    def loop(i: Int, j: Int, acc: Map[(Int,Int),List[Segment]]): Map[(Int,Int),List[Segment]] = {
      (i, j) match {
        case _ if i == m => acc
        case _ if j == m => loop(i+1, i+2, acc)
        case _           => loop(i, j+1, acc + ((i->j) -> merge(acc((i,j-1)), segments(j))))
      }
    }
    
    val rects = loop(0, 1, rects0)
    
    val maxArea = rects
      .keys
      .map {
        case (i,j) => (j-i+1) * rects((i,j)).map(_.size).foldLeft(0)(max(_,_))
      }
      .max
    
    maxArea
  }
}
