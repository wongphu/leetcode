object Solution {
  def romanToInt(s: String): Int = {
    def convert(prev: Char, char: Char): Int = (prev, char) match {
      case ('I', 'V') => 3
      case ('I', 'X') => 8
      case ('X', 'L') => 30
      case ('X', 'C') => 80
      case ('C', 'D') => 300
      case ('C', 'M') => 800
      case (_, 'I')   => 1
      case (_, 'V')   => 5
      case (_, 'X')   => 10
      case (_, 'L')   => 50
      case (_, 'C')   => 100
      case (_, 'D')   => 500
      case (_, 'M')   => 1000
      case _          => 0
    }
    
    (1 until s.size).foldLeft(convert(' ', s(0))) { (z, i) =>
      z + convert(s(i-1), s(i))
    }
  }
}
