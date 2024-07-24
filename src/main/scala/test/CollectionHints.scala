package test

import collection.mutable

object CollectionHints extends App {
  // mutable
  println("ListBuffer")
  val listBuffer = mutable.ListBuffer.empty[Int]
  listBuffer.append(1) // modifies collection
  listBuffer.addOne(2) // modifies collection
  println("Modified collection: " + listBuffer.appended(10).mkString(", ")) // won't change the collection itself, returns the modified collection
  println(listBuffer.mkString(", "))
  listBuffer.remove(0, 2)
  println("Empty: " + listBuffer.mkString(", "))
  listBuffer.addAll(Seq(1,2,3,4))
  listBuffer.prepend(5)
  listBuffer.drop(1) // doesn't modify collection
  println(listBuffer.mkString(", "))
  listBuffer.dropRightInPlace(1) // modifies collection
  println(listBuffer.mkString(", "))
  listBuffer += 10
  println(listBuffer.mkString(", "))

  println("\nStack")
  val stack = mutable.Stack.empty[Int]
  stack.push(1,2,3,4,5)
  println(stack.mkString(", "))
  stack.pop() // modifies collection
  stack += 9
  println(stack.mkString(", "))
  stack.takeInPlace(2) // modifies collection
  println(stack)
  stack.addOne(12)
  println(stack.top)
  stack.prepend(0)
  stack.append(0)
  println(stack)
  stack.removeHead() // modifies
  stack.removeLast()
  println(stack)

  // Priority Queue (Heap)
  println("\nPriority Queue")
  val pq = mutable.PriorityQueue.empty[(Int, Int)](Ordering.by(-_._1))
  pq.addAll(Seq(10, 5, 12, 11, 3, 8).zipWithIndex)
  println(pq)
  pq.enqueue(13 -> 6)
  pq.enqueue(1 -> 7)
  println(pq)
  pq += 9 -> 8
  println(pq)






  // immutable
}

/*
* mutable: addOne, addAll, enqueue, append, prepend, *inPlace, += - modifies collection; appended, :+ - not
* */
