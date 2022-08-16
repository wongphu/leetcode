object Solution {
  def restoreIpAddresses(s: String): List[String] = {
    val digits = s.map(_ - '0').toVector
    
    def isValid(start: Int, end: Int): Boolean = {
      val len = end - start + 1
      if (len < 1 || len > 3) {
        false
      } else if (len == 1) {
        true
      } else if (digits(start) == 0) {
        false
      } else if (len == 2) {
        true
      } else {
        100 * digits(start) + 10 * digits(start+1) + digits(start+2) < 256
      }
    }
    
    def split(start: Int, end: Int): List[Int] = {
      val len = end - start + 1
      if (len < 2 || len > 6) {
        Nil
      } else {
        (start+1 to end).filter(i => isValid(start, i-1) && isValid(i, end)).toList
      }
    }
    
    if (s.size < 4 || s.size > 12) {
      Nil
    } else {
      val first = (1 to 3).filter(i => isValid(0, i-1)).toList
      val last = (digits.size - 3 to digits.size - 1).filter(k => isValid(k, s.size-1)).toList
    
      val dots = for {
        i <- first
        k <- last
        j <- split(i, k-1)
      } yield (i, j, k)
    
      dots.map {
        case (i, j, k) => s"${s.substring(0, i)}.${s.substring(i, j)}.${s.substring(j, k)}.${s.substring(k)}"
      }
    }
  }
}
