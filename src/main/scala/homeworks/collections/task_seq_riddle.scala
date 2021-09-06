package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

import scala.annotation.tailrec

object task_seq_riddle {

  /**
   * Рассмотрим последовательность с числами:
   *
   * 1
   * 1 1
   * 2 1
   * 1 2 1 1
   * 1 1 1 2 2 1
   * 3 1 2 2 1 1
   * ...........
   *
   * 1. Реализуйте функцию генерирующую след последовательность из текущей
   * */

  def nextLine(currentLine: List[Int]): List[Int] = {
    @tailrec
    def loop (list:List[Int], pred:Int, anw: List[Int] = List.empty, ctr:Int = 1):List[Int] = list match {
      case head +: tail => if (head == pred) loop(tail,head,anw,ctr+1)
                           else loop(tail,head,anw:+ ctr :+ pred)
      case Nil => anw:+ ctr:+ pred
    }
    if(currentLine.isEmpty) currentLine
    else loop(currentLine.tail,currentLine.head)
  }

  /**
   * 2. Реализуйте ленивый список, который генерирует данную последовательность
   * Hint: см. LazyList.cons
   *
   * lazy val funSeq: LazyList[List[Int]]  ...
   *
   */

  val funSeq: LazyList[List[Int]] = LazyList.iterate(List(1))(nextLine)
}