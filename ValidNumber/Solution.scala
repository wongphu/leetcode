object Solution {

  def isNumber(s: String): Boolean = {
    type Parser = List[Char] => (Boolean, List[Char])
    
    def and(a: Parser, b: Parser): Parser = {
      (chars: List[Char]) => {
        a(chars) match {
          case (false, rest) => (false, chars)
          case (true, rest) => b(rest)
        }
      }
    }
    
    def literal(char: Char): Parser = {
      (chars: List[Char]) => chars match {
        case c :: rest if c == char => (true, rest)
        case _            => (false, chars)
      }
    }
    
    def alt(a: Parser, b: Parser): Parser = {
      (chars: List[Char]) => {
        a(chars) match {
          case (true, rest)  => (true, rest)
          case (false, _)    => b(chars)
        }
      }
    }
    
    def zeroOrMore(p: Parser): Parser = {
      (chars: List[Char]) => {
        var result = p(chars)
        while (result._1) {
          result = p(result._2)
        }
        (true, result._2)
      }
    }
    
    def digit(chars: List[Char]): (Boolean, List[Char]) = {
      chars match {
        case char :: rest if (char.isDigit) =>
          (true, rest)
        case _ =>
          (false, chars)
      }
    }
    
    def digits(chars: List[Char]): (Boolean, List[Char]) = {
      chars match {
        case char :: rest if (char.isDigit) =>
          (true, chars.dropWhile(_.isDigit))
        case _ => (false, chars)
      }
    }
    
    def opt(p: Parser): Parser = {
      (chars: List[Char]) => {
        p(chars) match {
          case (true, rest) => (true, rest)
          case (false, rest) => (true, chars)
        }
      }
    }
    
    val dot = literal('.')
    val sign = alt(literal('+'), literal('-'))
    val e = alt(literal('e'), literal('E'))
    val decimal = alt(and(dot, digits), and(digits, and(dot, zeroOrMore(digit))))
    val exponent = and(e, and(opt(sign),  digits))
    val num = and(opt(sign), and(alt(decimal, digits), opt(exponent)))
    num(s.toList) match {
      case (true, rest) => rest.isEmpty
      case _            => false
    }
  }
}
