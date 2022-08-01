object Solution {
  def fullJustify(words: Array[String], maxWidth: Int): List[String] = {
    
    def findBreak(start: Int): (Int, Int) = {
      val line = LazyList.range(start+1, words.size)
        .scanLeft(words(start).size)((acc,i) => acc + 1 + words(i).size)
        .takeWhile(_ <= maxWidth)
      (line.size, line.last)
    }
    
    def leftJustify(start: Int, count: Int, size: Int): String = {
      words.slice(start, start+count).mkString(" ") + (" " * (maxWidth - size))
    }
    
    def justify(start: Int, count: Int, size: Int): String = {
      if (count == 1) {
        words(start) + (" " * (maxWidth - words(start).size))
      } else {
        val textSize = size - count + 1
        val spaces = maxWidth - textSize
        val minSpacing = spaces / (count - 1)
        val excessSpaces = spaces % (count - 1)
        if (excessSpaces == 0) {
          words.slice(start, start+count).mkString(" " * minSpacing)
        } else {
          val maxSpacing = minSpacing + 1
          val first = words.slice(start, start+excessSpaces)
            .mkString("", " " * maxSpacing, " " * maxSpacing)
          val second = words.slice(start+excessSpaces, start+count)
            .mkString(" " * minSpacing)
          first + second
        }
      }
    }
    
    def helper(start: Int, acc: List[String]): List[String] = {
      if (start == words.size) {
        acc
      } else {
        val (count, size) = findBreak(start)
        val line = if (start + count < words.size) {
          justify(start, count, size)
        } else {
          leftJustify(start, count, size)
        }
        helper(start+count, line :: acc)
      }
    }
    
    helper(0, List.empty[String]).reverse
  }
}
