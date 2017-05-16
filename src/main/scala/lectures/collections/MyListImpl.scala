package lectures.collections

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Представим, что по какой-то причине Вам понадобилась своя обертка над списком целых чисел List[Int]
  *
  * Вы приняли решение, что будет достаточно реализовать 4 метода:
  * * * * * def flatMap(f: (Int => MyList)) -  реализуете на основе соответствующего метода из List
  * * * * * метод map(f: (Int) => Int) - с помощью только что полученного метода flatMap класса MyList
  * * * * * filter(???) - через метод flatMap класса MyList
  * * * * * foldLeft(acc: Int)(???) - через декомпозицию на head и tail
  *
  * Для того, чтобы выполнить задание:
  * * * * * раскомментируйте код
  * * * * * замените знаки вопроса на сигнатуры и тела методов
  * * * * * не используйте var и мутабильные коллекции
  *
  */
object MyListImpl extends App {

  case class MyList[T, S <: Seq[T]](data: Seq[T]) {

    def flatMap(f: (T => MyList[T, S])): MyList[T, S] =
      MyList[T, S](data.flatMap(inp => f(inp).data))

    def map(f: (T => T)): MyList[T, S] = {
      flatMap(inp => MyList[T, S](List(f(inp))))
    }

    def foldLeft(acc: T)(f: ((T, T)) => T): T = {
      if (data.isEmpty) acc
      else MyList[T, S](data.tail).foldLeft(f((acc, data.head)))(f)
//      data match {
//        case Nil          => acc
//        case head :: tail => MyList[T, S](tail).foldLeft(f((acc, head)))(f)
//      }
    }

    def filter(f: T => Boolean): MyList[T, S] = {
      val filterHelper: PartialFunction[T, MyList[T, S]] = {
        case x if f(x) => MyList[T, S](List(x))
        case _         => MyList[T, S](Nil)
      }
      flatMap(filterHelper)
    }
  }

  class MyListBuffer[T](data: ListBuffer[T]) extends MyList[T, ListBuffer[T]](data)
  object MyListBuffer {
    def apply[T](data: ListBuffer[T]) = new MyListBuffer[T](data)
  }

  class MyIndexedList[T](data: IndexedSeq[T]) extends MyList[T, IndexedSeq[T]](data)
  object MyIndexedList {
    def apply[T](data: IndexedSeq[T]) = new MyIndexedList[T](data)
  }

  require(MyList(List(1, 2, 3, 4, 5, 6)).map(_ * 2).data == List(2, 4, 6, 8, 10, 12))
  require(MyList(List(1, 2, 3, 4, 5, 6)).filter(_ % 2 == 0).data == List(2, 4, 6))
  require(MyList(List(1, 2, 3, 4, 5, 6)).foldLeft(0)((tpl) => tpl._1 + tpl._2) == 21)
  require(MyList(Nil).foldLeft(0)((tpl) => tpl._1 + tpl._2) == 0)

  require(MyList[Int, List[Int]](List(1, 2, 3, 4, 5, 6)).map(p => p * 2).data == List(2, 4, 6, 8, 10, 12))
  require(MyList[Long, ListBuffer[Long]](ListBuffer(1, 2, 3, 4, 5, 6)).filter(_ % 2 == 0).data == List(2, 4, 6))
  require(MyList[Int, List[Int]](List(1, 2, 3, 4, 5, 6)).foldLeft(0)((tpl) => tpl._1 + tpl._2) == 21)
  require(MyList[Float, IndexedSeq[Float]](ArrayBuffer.empty[Float]).foldLeft(0)((tpl) => tpl._1 + tpl._2) == 0)

  require(MyListBuffer[Long](ListBuffer(1, 2, 3, 4, 5, 6)).filter(_ % 2 == 0).data == List(2, 4, 6))
  require(MyIndexedList[Float](ArrayBuffer.empty[Float]).foldLeft(0)((tpl) => tpl._1 + tpl._2) == 0)
}