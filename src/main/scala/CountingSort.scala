object CountingSort extends App {
  // 561. Array Partition
  def arrayPairSum(nums: Array[Int]): Int = {
    val frequencies = Array.fill(nums.max + 1)(0)
    for (num <- nums) frequencies(num) += 1
    for (i <- 1 until frequencies.length) {
      frequencies(i) = frequencies(i - 1) + frequencies(i)
    }
    val result = Array.fill(nums.length)(0)
    for (i <- nums.length - 1 to 0 by -1) {
      result(frequencies(nums(i) - 1)) = nums(i)
      frequencies(nums(i)) -= 1
    }

    var res = 0
    result.zipWithIndex.foreach((x, i) => if (i % 2 == 0) res += x)
    res
  }
}
