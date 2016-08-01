package lectures.oop


/**
  * BSTImpl - это бинарное дерево поиска, содержащее только значения типа Int
  *
  * * Оно обладает следующими свойствами:
  * * * * * левое поддерево содержит значения, меньшие значения родителя
  * * * * * правое поддерево содержит значения, большие значения родителя
  * * * * * значения, уже присутствующие в дереве, в него не добавляются
  * * * * * пустые значения(null) не допускаются
  *
  * * Завершите реализацию методов кейс класс BSTImpl.
  * * * * * Трейт BST и BSTImpl разрешается расширять любым образом
  * * * * * Изменять сигнатуры классов и методов, данные в  условии, нельзя
  * * * * * Постарайтесь не использовать var и мутабильные коллекции
  * * * * * В задаче про распечатку дерева, нужгно раскомментировать и реадизовать метод toString()
  *
  * * Для этой структуры нужно реализовать генератор узлов.
  * * Генератор
  * * * * * должен создавать дерево, содержащее nodesCount узлов.
  * * * * * не должен использовать переменные или мутабильные структуры.
  *
  */
trait BST {
  val value: Int
  val left: Option[BST]
  val right: Option[BST]

  def add(newValue: Int): BST

  def find(value: Int): Option[BST]

  def fold(aggregator: Int)(f: (Int, Int) =>(Int)): Int
}

case class BSTImpl(value: Int,
                   left: Option[BSTImpl] = None,
                   right: Option[BSTImpl] = None) extends BST {

  def add(newValue: Int): BSTImpl = {
    if (value == newValue) {
      this
    } else if (newValue < value) {
      this match {
        case BSTImpl(_, Some(l), _) => BSTImpl(value, Some(l.add(newValue)), right)
        case _                      => BSTImpl(value, Some(BSTImpl(newValue, None, None)), right)
      }
    } else {
      this match {
        case BSTImpl(_, _, Some(r)) => BSTImpl(value, left, Some(r.add(newValue)))
        case _                      => BSTImpl(value, left, Some(BSTImpl(newValue, None, None)))
      }
    }
  }

  def find(value: Int): Option[BST] = {
    if (value == this.value) {
      Some(this)
    } else if (value < this.value) {
      this match {
        case BSTImpl(_, Some(l), _) => l.find(value)
        case _                      => None
      }
    } else {
      this match {
        case BSTImpl(_, _, Some(r)) => r.find(value)
        case _                      => None
      }
    }
  }

  def fold(aggregator: Int)(f: (Int, Int) =>(Int)): Int = {
    val aggThis = f(aggregator, value)

    val aggLeft = left match {
      case Some(l) => l.fold(aggThis)(f)
      case _       => aggThis
    }

    val aggRight = right match {
      case Some(r) => r.fold(aggLeft)(f)
      case _       => aggLeft
    }

    aggRight
  }

  // override def toString() = ???

}

object TreeTest extends App {

  val sc = new java.util.Scanner(System.in)
  val maxValue = 110000
  val nodesCount = sc.nextInt()

  val markerItem = (Math.random() * maxValue).toInt
  val markerItem2 = (Math.random() * maxValue).toInt
  val markerItem3 = (Math.random() * maxValue).toInt

  // Generate huge tree
  val root: BST = BSTImpl(maxValue / 2)
  val tree: BST = (1 until nodesCount).foldLeft(root)((t, _) => t.add((Math.random() * maxValue).toInt))

  // add marker items
  val testTree = tree.add(markerItem).add(markerItem2).add(markerItem3)

  // check that search is correct
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem2).isDefined)
  require(testTree.find(markerItem3).isDefined)

  println(testTree)
  println(testTree.fold(0)((agg, x) => agg + x))
  println(testTree.fold(1)((agg, x) => agg * x))
}