

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

  // 502. IPO
  def findMaximizedCapital(k: Int, w: Int, profits: Array[Int], capital: Array[Int]): Int = {
    val queue = collection.mutable.PriorityQueue[Int]()
    val revenues = profits.zip(capital).sortInPlaceBy(_._2)
    var counterInRevenues = 0
    var currentNumberOfProjects = 0
    var money = w
    while (currentNumberOfProjects < k) {
      while (counterInRevenues < revenues.length && revenues(counterInRevenues)._2 <= money) {
        queue.enqueue(revenues(counterInRevenues)._1)
        counterInRevenues += 1
      }

      if (queue.isEmpty) return money
      money += queue.dequeue
      currentNumberOfProjects += 1
    }

    money
  }

  // 1481. Least Number of Unique Integers after K Removals
  def findLeastNumOfUniqueInts(arr: Array[Int], k: Int): Int = {
    var rest = k
    val map = arr.foldLeft(collection.mutable.HashMap.empty[Int, Int]) { (map, x) => map addOne(x, map.getOrElse(x, 0) + 1) }
    val value = map.toSeq.sorted(Ordering.by[(Int, Int), Int](_._2))
    value.dropWhile(x => {
      rest -= x._2
      rest >= 0
    }).size
  }

  // 881. Boats to Save People
  def numRescueBoats(people: Array[Int], limit: Int): Int = {
    val sorted = people.sorted
    var (left, right) = (0, sorted.length - 1)
    var answer = 0

    while (left <= right) {
      if (sorted(left) + sorted(right) <= limit) {
        left += 1
      }
      answer += 1
      right -= 1
    }
    answer
  }

  // 1323. Maximum 69 Number
  def maximum69Number(num: Int): Int = {
    val list = collection.mutable.ListBuffer.empty[Char]
    var changed = false
    for (c <- num.toString) {
      if (!changed && c == '6') {
        changed = true
        list += '9'
      } else {
        list += c
      }
    }
    list.mkString.toInt
  }

  // 1710. Maximum Units on a Truck
  def maximumUnits(boxTypes: Array[Array[Int]], truckSize: Int): Int = {
    var units = 0
    var currentlyBoxes = 0

    val it = boxTypes.sortInPlaceBy { case Array(_, numUnitsPerBox) => -numUnitsPerBox }.iterator

    while (it.hasNext) {
      val arr = it.next()
      val numberOfBoxes = arr(0)
      val numberOfUnitesPerBox = arr(1)

      if (currentlyBoxes < truckSize) {
        // how much boxes could we add yet
        // left: 2, boxes: 5

        val left = truckSize - currentlyBoxes
        val toAdd = Math.min(left, numberOfBoxes)
        units += toAdd * numberOfUnitesPerBox
        currentlyBoxes += toAdd
      }
    }

    units
  }

  // 1196. How Many Apples Can You Put into the Basket
  def maxNumberOfApples(weight: Array[Int]): Int = {
    var counter = 0
    weight.sorted.fold(5000) {(left, x) => {
      val diff = left - x
      if (diff >= 0) counter += 1
      diff
    }}
    counter
  }
}
