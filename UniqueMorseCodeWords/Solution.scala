object Solution {
  val codes = Map(
    'a' -> ".-",
    'b' -> "-...",
    'c' -> "-.-.",
    'd' -> "-..",
    'e' -> ".",
    'f' -> "..-.",
    'g' -> "--.",
    'h' -> "....",
    'i' -> "..",
    'j' -> ".---",
    'k' -> "-.-",
    'l' -> ".-..",
    'm' -> "--",
    'n' -> "-.",
    'o' -> "---",
    'p' -> ".--.",
    'q' -> "--.-",
    'r' -> ".-.",
    's' -> "...",
    't' -> "-",
    'u' -> "..-",
    'v' -> "...-",
    'w' -> ".--",
    'x' -> "-..-",
    'y' -> "-.--",
    'z' -> "--.."
  )
  
  def transform(word: String): String =
    word.flatMap(codes)
  
  def uniqueMorseRepresentations(words: Array[String]): Int = {
    words.toSet.map(transform).size
  }
}
