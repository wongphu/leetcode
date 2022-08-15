object Solution {
  import scala.collection.immutable.BitSet
  
  def findLadders(beginWord: String, endWord: String, wordList: List[String]): List[List[String]] = {
    val words = (beginWord :: wordList).toVector.distinct
    val endIndex = words.indexOf(endWord)
    
    if (endIndex == -1) {
      Nil
    } else {
      val distance = (0 until words.size).toVector.map { i =>
        (0 until words.size).toVector.map { j =>
          words(i).zip(words(j)).foldLeft(0) {
            case (z, (x, y)) if x == y => z
            case (z, _)                => z + 1
          }
        }
      }
      
      val neighbors = (0 until words.size).toVector.map { i =>
        BitSet.fromSpecific((0 until words.size).filter(j => distance(i)(j) == 1))
      }
    
      case class State(frontier: BitSet, unexplored: BitSet)
    
      val initialState = State(BitSet(0), BitSet.fromSpecific(1 until words.size))
      val frontiers = List.unfold(initialState) {
        case State(frontier, unexplored) =>
          if (frontier.isEmpty || frontier.contains(endIndex) || unexplored.isEmpty) {
            None
          } else {
            val nextFrontier = unexplored & frontier.flatMap(neighbors(_))
            val nextUnexplored = unexplored.diff(nextFrontier)
            val nextState = State(nextFrontier, nextUnexplored)
            Some((frontier, nextState))
          }
      }
      val ladders = frontiers.foldRight(List(List(endIndex))) { (frontier, lists) =>
        lists.flatMap { list =>
          frontier
            .toList
            .filter(i => distance(i)(list.head) == 1)
            .map(_ :: list)
        }
      }
    
      ladders.map(_.map(words(_)))
    }
  }
}
