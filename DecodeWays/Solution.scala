object Solution {
  def numDecodings(s: String): Int = {
    s.zip(s.drop(1) + "_").foldRight((1,0)) {
      case (('0', _), (a0, a1)) => (0, a0) 
      case (('1', d), (a0, a1)) => (a0+a1, a0)
      case (('2', d), (a0, a1)) if d < '7' => (a0+a1, a0)
      case (_,        (a0, a1)) => (a0, a0)
    }._1
  }
}
