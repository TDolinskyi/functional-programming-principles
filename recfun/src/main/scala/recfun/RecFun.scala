package recfun

import org.w3c.dom.css.Counter
import sun.security.util.Length

import java.util
import scala.annotation.tailrec
import scala.collection.+:

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    println(countChange(7, List(1,2,3)))
  //    for row <- 0 to 10 do
  //      for col <- 0 to row do
  //        print(s"${pascal(col, row)} ")
  //      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    
    @tailrec
    def currentRowBuilder(previous: Vector[Int], current: Vector[Int]): Vector[Int] = {
      previous match {
        case first +: tail if tail.nonEmpty => currentRowBuilder(tail, current :+ (first + tail.head))
        case _ => Vector(1) ++ current ++ Vector(1)
      }
    }
    
    @tailrec
    def triangleBuilder(triangle: Vector[Vector[Int]] = Vector(), currentRow: Int = 0): Vector[Vector[Int]] = {
      if (currentRow == 0) {
        triangleBuilder(triangle :+ Vector(1), 1)
      } else if (currentRow == 1) {
        triangleBuilder(triangle :+ Vector(1, 1), currentRow + 1)
      } else {
        if (currentRow > r) {
          triangle
        } else {
          triangleBuilder(triangle :+ currentRowBuilder(triangle(currentRow - 1), Vector.empty[Int]), currentRow + 1)
        }
      }

    }

    triangleBuilder()(r)(c)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    val open = '('
    val close = ')'

    def iter(chars: List[Char], current: List[Char]): Boolean = {
      chars match
        case h :: tail if h == open => iter(tail, current :+ h)
        case h :: tail if h == close =>
          current.lastOption match
            case Some(el) if el == open => iter(tail, current.dropRight(1))
            case _ => false
        case _ :: tail => iter(tail, current)
        case _ => current.isEmpty

    }

    iter(chars, List.empty[Char])

  }

  /**
   * Exercise 3
   * 4 -> List(1, 2): 1+1+1+1, 1+1+2, 2+2 -> 3
   * 4 -> List(1, 2, 3): 1+1+1+1, 1+1+2, 2+2, 1+3 -> 4
   * 5 -> List(1, 2, 3): 1+1+1+1+1, 1+1+1+2, 1+2+2, 1+1+3, 2+3
   * 6 -> List(1, 2, 3): 1+1+1+1+1+1, 1+1+1+1+2, 1+1+2+2, 2+2+2, 1+2+3, 3+3
   * 7 -> List(1, 2, 3): 1+1+1+1+1+1+1, 1+1+1+1+1+2, 1+1+1+2+2, 1+2+2+2, 1+1+1+1+3 1+1+2+3, 2+2+3, 1+3+3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def count(sum: Int, remainingCoins: List[Int]): Int = {
      if (sum == 0) {
        1
      } else if (sum < 0) {
        0
      } else if (remainingCoins.isEmpty) {
        0
      } else {
        count(sum, remainingCoins.tail) + count(sum - remainingCoins.head, remainingCoins)
      }
    }

    count(money, coins.sorted.reverse)
  }
