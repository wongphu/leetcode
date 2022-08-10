object Solution {
  def simplifyPath(path: String): String = {
    
    @scala.annotation.tailrec
    def removeDotDot(dirs: List[String], acc: List[String] = Nil): List[String] = dirs match {
      case Nil =>
        acc.reverse
      case ".." :: rest =>
        removeDotDot(rest, acc.drop(1))
      case dir :: rest =>
        removeDotDot(rest, dir :: acc)
    }
    
    val dirs = path
      .split('/')
      .view
      .filter(_.nonEmpty)
      .filter(_ != ".")
      .toList
    
    removeDotDot(dirs).mkString("/", "/", "")
  }
}
