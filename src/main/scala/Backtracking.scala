
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

  // 78. Subsets
  def subsets(nums: Array[Int]): List[List[Int]] = {
    var answer = List.empty[List[Int]]

    def backtracking(curr: List[Int]): Unit = {
      if (!answer.contains(curr)) answer = answer :+ curr

      for (num <- nums) {
        if (!curr.contains(num))
          backtracking(curr.appended(num))
      }
    }

    backtracking(List.empty)
    answer
  }

  println(subsets(Array(1, 2, 3)))
  println(subsets(Array(4,1,0)))

}
