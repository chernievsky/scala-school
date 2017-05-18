package lectures.oop.types

import lectures.matching.SortingStuff.Watches

/**
  * Модифицируйте реализацию BSTImpl из предыдущего задания.
  * Используя тайп параметры и паттерн Type Class, реализуйте GeneralBSTImpl таким образом,
  * чтобы дерево могло работать с произвольным типом данных.
  *
  * Наследников GeneralBSTImpl определять нельзя.
  *
  * Создайте генератор для деревьев 3-х типов данных:
  * * * * float
  * * * * String
  * * * * Watches из задачи SortStuff. Большими считаются часы с большей стоимостью
  */

trait GeneralBST[T] {
  val value: T
  val left: Option[GeneralBST[T]]
  val right: Option[GeneralBST[T]]

  def add(newValue: T): GeneralBST[T]

  def find(value: T): Option[GeneralBST[T]]
}

class GeneralBSTImpl[T: Ordering](val value: T,
                                  val left: Option[GeneralBST[T]] = None,
                                  val right: Option[GeneralBST[T]] = None) extends GeneralBST[T] {
  val ord: Ordering[T] = implicitly[Ordering[T]]
  //override val value = _
  //override val left: Option[GeneralBST[T]] = ???
  //override val right: Option[GeneralBST[T]] = ???

  override def find(value: T): Option[GeneralBST[T]] = {
    if (ord.lt(value, this.value))
      left.flatMap(_.find(value))
    else if (ord.gt(value, this.value))
      right.flatMap(_.find(value))
    else
      Some(this)
  }

  override def add(newValue: T): GeneralBST[T] = {
    if (this.value == newValue)
      this
    else if (ord.lt(newValue, this.value))
      new GeneralBSTImpl[T](this.value, left.map(_.add(newValue)).orElse(Some(new GeneralBSTImpl[T](newValue))), right)
    else
      new GeneralBSTImpl[T](this.value, left, right.map(_.add(newValue)).orElse(Some(new GeneralBSTImpl[T](newValue))))
  }

  override def toString: String = "{" + left.getOrElse("-").toString + "." + value + "." + right.getOrElse("-").toString + "}"
}

object BSTGenerator extends App {
  val maxValue = 200

  trait Gen[T] {
    def apply(): T
  }

  implicit val genInt: Gen[Int] = () => (Math.random() * maxValue).toInt
  implicit val genFloat: Gen[Float] = () => (Math.random() * maxValue).toFloat
  implicit val genString: Gen[String] = () => scala.util.Random.alphanumeric.take(5).mkString
  implicit val genWatches: Gen[Watches] = () => Watches(genString(), genFloat())

  def buildBST[T: Ordering](cnt: Int)(implicit evidence: Gen[T]): GeneralBST[T] = {
    val gen = implicitly[Gen[T]]
    val root: GeneralBST[T] = new GeneralBSTImpl[T](gen())

    (1 until cnt).foldLeft(root)((t, _) => t.add(gen()))
  }

  val sc = new java.util.Scanner(System.in)
  val nodesCount = sc.nextInt()

  val intTree: GeneralBST[Int] = buildBST[Int](nodesCount)
  val floatTree: GeneralBST[Float] = buildBST[Float](nodesCount)
  val stringTree: GeneralBST[String] = buildBST[String](nodesCount)

  implicit val watchesOrdering: Ordering[Watches] = Ordering.by(_.cost) // http://stackoverflow.com/questions/25733902/scala-implicit-ordering
  val watchesTree: GeneralBST[Watches] = buildBST[Watches](nodesCount)

  println(intTree)
  println(floatTree)
  println(stringTree)
  println(watchesTree)
}
