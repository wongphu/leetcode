object Solution {
  def subsetsWithDup(nums: Array[Int]): List[List[Int]] = {
    nums.sorted.foldLeft(List(List.empty[Int])) { (z, n) => (z ++ z.map(n :: _)).distinct }
  }
}
