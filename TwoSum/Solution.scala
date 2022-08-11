object Solution {
  
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val sorted = nums.zipWithIndex.sorted
      
    @annotation.tailrec
    def search(left: Int, right: Int): Array[Int] = {
      if (left >= right) {
        Array(-1,-1)
      } else {
        val sum = sorted(left)._1 + sorted(right)._1
        if (sum == target) {
          Array(sorted(left)._2, sorted(right)._2)
        } else if (sum < target) {
          search(left + 1, right)
        } else {
          search(left, right - 1)
        }
      }
    }
      
    search(0, nums.size-1)
  }
}
