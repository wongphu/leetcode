object Solution {
  
  @scala.annotation.tailrec
  def count(s: String, acc: List[(Int,Char)] = Nil): List[(Int,Char)] =
    if (s.isEmpty) {
      acc.reverse
    } else {
      val digit = s(0)
      val len = (0 until s.size).takeWhile(s(_) == digit).size
      count(s.substring(len), (len -> digit) :: acc)
    }
  
  def say(tuple: (Int,Char)): String = {
    val (len, digit) = tuple
    s"$len$digit"
  }
  
  def countAndSay(n: Int): String = {
    n match {
      case 1 =>
        "1"
      case k if k <= 0 =>
        ""
      case _ =>
        val s = countAndSay(n-1)
        count(s).map(say).mkString
    }
  }
}
