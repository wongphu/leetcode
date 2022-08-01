object Solution {
  import math.max
  import annotation.tailrec
  
  def lengthOfLongestSubstring(s: String): Int = {
    
    @tailrec
    def rec(left: Int, right: Int, chars: Set[Char], maxlen: Int): Int = {
      if (right == s.length) {
        maxlen
      } else {
        if (chars.contains(s(right))) {
          rec(left+1, right, chars-s(left), maxlen)
        } else {
          rec(left, right+1, chars+s(right), max(maxlen, right-left+1))
        }
      }
    }
    rec(0, 0, Set.empty[Char], 0)
  }
}
