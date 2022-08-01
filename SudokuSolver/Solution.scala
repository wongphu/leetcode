// Functional Scala solution.  The board is represented with an immutable class, 
// named Grid.  The strategy is depth first search.  Since Grid is immutable,
// we generate a new Grid for each move, as implemented in the move method in Grid.
// The Grid tracks the current board state in a Vector of Byte and a sorted set 
// (TreeSet) of candidates for unsolved squares.  The sorted set is ordered of
// the number of candidates for each square.  So the unsolved square with the least
// number of candidates is at the top.  The Grid holds 81 squares.  Each square is
// identified by a number from 0 to 80, named loc in the program.

object Solution {
  import collection.immutable.BitSet
  import collection.immutable.TreeSet
  
  case class Move(loc: Int, num: Byte)  // represent a move by placing num (1..9) at square loc
  
  object Move {
    def apply(row: Int, col: Int, char: Char): Move = {
      Move(9 * row + col, Grid.toByte(char))
    }
  }
  
  // Candidates for unsolved square at loc.
  case class Candidates(bits: BitSet, loc: Int) extends Ordered[Candidates] {
    def compare(that: Candidates) = {
      val sizeCompare = bits.size.compare(that.bits.size)
      if (sizeCompare != 0) sizeCompare else loc.compare(that.loc)
    }
  }
  
  object Candidates {
    val allbits = BitSet(1,2,3,4,5,6,7,8,9)
    def apply(loc: Int): Candidates = Candidates(allbits, loc)
  }
  
  // cells hold the digits for each square. unsolved holds the candidates for
  // unsolved squares in order of the number of possible candidates.
  case class Grid(cells: Vector[Byte], unsolved: TreeSet[Candidates]) {
    
    def isValid(move: Move): Boolean = {
      Grid.neighbors(move.loc).forall(n => cells(n) != move.num)
    }
    
    def apply(row: Int, col: Int): Byte =  cells(9 * row + col)
    
    // All squares are done.
    def isComplete = cells.forall(_ != 0)
    
    // Here we generate the new Grid derived by applying move to the current Grid.
    // If the new Grid is invalid, we return None.
    def move(move: Move): Option[Grid] = {
      if (!isValid(move)) {
        None
      } else {
        val neighbors = Grid.neighbors(move.loc)
        val updatedUnsolved = unsolved         // Update the candidates for unsolved
          .filter(_.loc != move.loc)           // squares in the neighborhood of the move.
          .map {
            case cell @ Candidates(bits, loc) =>
              if (neighbors.contains(loc)) {
                Candidates(bits - move.num, loc) 
              } else {
                cell
              }
          }
        
        val updatedCandidatess = cells.updated(move.loc, move.num)
        val updatedGrid = Grid(updatedCandidatess, updatedUnsolved)
        Some(updatedGrid)
      }
    }
    
    def validMoves(loc: Int): List[Move] = {   // Return the possible moves the unsolved
      unsolved.headOption match {              // square at the head, which has the fewest
        case None =>                           // candidates.
          List()
        case Some(cell) =>
          cell.bits
            .toList
            .map(n => Move(loc, n.toByte))
      }
    }
  }
  
  object Grid {
    val allCandidatess = TreeSet[Candidates]() ++ (0 until 81).map(Candidates(_))
    val empty: Grid = Grid(Vector.fill(81)(0), allCandidatess)
    
    // neighbors is set of all other squares in the same row, col or box.
    val neighbors: Vector[Set[Int]] = {
      (0 until 81).toVector.map { loc =>
        val row = loc / 9
        val col = loc % 9
        (0 until 81).filter { n =>
          val r = n / 9
          val c = n % 9
          (n != loc) && ((r == row) || (c == col) || (r/3 == row/3 && c/3 == col/3))
        }.toSet
      }
    }
    
    def toByte(char: Char): Byte = char match {
      case '.' => 0.toByte
      case _   => (char - '0').toByte
    }
    
    // Convert from external input to internal representation of the board.
    def fromArray(array: Array[Array[Char]]):  Option[Grid] = {
      val moves = for {
        row <- 0 until 9
        col <- 0 until 9
        if (array(row)(col) != '.')
      } yield {
        Move(row, col, array(row)(col))
      }
      moves.foldLeft(Option(Grid.empty)) { (g: Option[Grid], m: Move) => g.flatMap(_.move(m)) }
    }
  }
  
  // Solve the sudoku using depth first search.
  def solveSudoku(board: Array[Array[Char]]): Unit = {
    def dfs(grid: Grid): Option[Grid] = {
      if (grid.isComplete) {
        Some(grid)
      } else {
        grid.unsolved.headOption match {
          case None =>
            None
          case Some(cell) =>
            grid.validMoves(cell.loc) match {
              case Nil =>
                None
              case moves =>
                moves.view
                  .flatMap(grid.move(_))
                  .flatMap(dfs)
                  .headOption
           }
        }
      }
    }
    
    Grid.fromArray(board) match {
      case None       =>
        println("Bad Board")
      case Some(grid) =>
        dfs(grid) match {
          case Some(solution) =>    // Convert the solution to external representation.
            for {
              row <- 0 until 9
              col <- 0 until 9
            } {
              board(row)(col) = ('0' + solution(row, col)).toChar
            }
          case None =>
        }
    }
  }
}
