object Solution {
  import collection.mutable
  
  val cache = mutable.Map[(String, String), Boolean]()
  
  def isScramble(s1: String, s2: String): Boolean = {
    val tuple = s1 -> s2
    if (!cache.contains(tuple)) {
      val result = 
        if (s1 == s2) {
          true
        } else if (s1.size == 1) {
          false
        } else if (s1.sorted != s2.sorted) {
          false
        } else {
          (1 until (s1.size)).exists { n =>
            (isScramble(s1.take(n), s2.takeRight(n)) && isScramble(s1.drop(n), s2.dropRight(n))) ||
            (isScramble(s1.take(n), s2.take(n)) && isScramble(s1.drop(n), s2.drop(n)))
          }
        }
      
      cache(tuple) = result
    }
    
    cache(tuple)
  }
}
