
object Backtracking extends App {

  // 46. Permutations
  def permute(nums: Array[Int]): List[List[Int]] = {
    var answer = List.empty[List[Int]]

    def backtracking(curr: List[Int]): Unit = {
      if (curr.length == nums.length) answer = answer :+ curr
      else
        for (num <- nums) {
          if (!curr.contains(num))
            backtracking(curr.appended(num))
        }
    }

    backtracking(List.empty)
    answer
  }
}
