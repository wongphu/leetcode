object Solution {
  import math.max

  def longestValidParentheses(s: String): Int = {
    def num(c: Char): Int = c match {
      case '(' => 1
      case ')' => -1
      case _   => 0
    }
    
    def show(range: Range): String = {
      s.drop(range.start).take(range.size)
    }

    def split(range: Range): (Range, Range) = {
      val s1 = range
        .map(i => num(s(i)))
        .scanLeft(0)(_+_)
        .takeWhile(_ >= 0)

      val lastZero = s1.lastIndexOf(0)
      if (lastZero != 0) {
        (range.take(lastZero), range.drop(lastZero))
      } else {
        val s2 = range
          .reverse
          .map(i => num(s(i)))
          .scanLeft(0)(_+_)
          .takeWhile(_ <= 0)
        
        (range.takeRight(s2.size - 1), range.dropRight(s2.size - 1))
      }
    }

    def rec(range: Range): Int = {
      var trim = range
        .dropWhile(s(_) == ')') // remove lealing )'s
        .reverse
        .dropWhile(s(_) == '(')  // remove trailing ('s
        .reverse
                   
      if (trim.isEmpty) {
        0
      } else {
        val (first, rest) = split(trim)
        max(first.size, rec(rest))
      }
    }

    rec(Range(0, s.length))
  }
}
