

object GreedyAlgorithms extends App {
  // 2126. Destroying Asteroids
  def asteroidsDestroyed(mass: Int, asteroids: Array[Int]): Boolean = {
    asteroids.sorted.foldLeft[Long](mass) { (mass, x) => if (mass < x) mass - x else mass + x } > 0
  }

  // 2294. Partition Array Such That Maximum Difference Is K
  def partitionArray(nums: Array[Int], k: Int): Int = {
    import scala.collection.mutable

    nums.sorted.foldLeft(mutable.Stack[mutable.ArrayDeque[Int]]()) { (stack, num) =>
      if (stack.isEmpty) {
        stack.push(mutable.ArrayDeque(num))
      } else {
        val queue = stack.head
        val value = queue.head
        if (Math.abs(num - value) <= k) {
          queue.addOne(num)
        } else {
          stack.push(mutable.ArrayDeque(num))
        }
      }
      stack
    }.size
  }
}
