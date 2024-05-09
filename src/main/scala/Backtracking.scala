
object Backtracking extends App {
  // 46. Permutations
  def permute(nums: Array[Int]): List[List[Int]] =
    helper(i = 0, arr = nums, permutations = Nil)

  def helper(i: Int, arr: Array[Int], permutations: List[List[Int]]): List[List[Int]] =
    if (i == arr.length)
      permutations.appended(arr.toList)
    else
      (i until arr.length).foldLeft(List.empty[List[Int]]) {
        case (acc, j) =>
          swap(arr, i, j)
          val perms = helper(i + 1, arr, permutations)
          swap(arr, i, j)
          perms ::: acc
      }

  def swap(arr: Array[Int], i: Int, j: Int): Unit = {
    val tmp = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp
  }
}
