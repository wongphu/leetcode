object Solution {
  import math.{max, min}
  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
    val m = nums1.length
    val n = nums2.length
    val half = (m + n + 1) / 2
    
    if (m > n) {
      findMedianSortedArrays(nums2, nums1)
    } else {
      def rec(left: Int, right: Int): (Int, Int) = {
        val count1 = (left + right) / 2
        val count2 = half - count1
        val l1 = if (count1 <= 0) Int.MinValue else nums1(count1-1)
        val r1 = if (count1 >= m) Int.MaxValue else nums1(count1)
        val l2 = if (count2 <= 0) Int.MinValue else nums2(count2-1)
        val r2 = if (count2 >= n) Int.MaxValue else nums2(count2)
        if (l1 <= r2 && l2 <= r1) {
          val leftMedian = max(l1, l2)
          val rightMedian = min(r1, r2)
          (leftMedian, rightMedian)
        } else {
          if (l1 > r2) {
            rec(left, count1)
          } else {
            rec(count1+1, right)
          }
        }
      }
      val (leftMedian, rightMedian) = rec(0, m)
      if ((n+m) % 2 == 0)
        (leftMedian + rightMedian) / 2.0
      else
        leftMedian
    }
  }
}
