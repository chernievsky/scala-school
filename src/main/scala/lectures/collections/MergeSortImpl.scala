package lectures.collections

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  *
  */
object MergeSortImpl extends App {

  def mergeSort(data: Seq[Int]): Seq[Int] = {

    def merge(left: Seq[Int], right: Seq[Int]): Seq[Int] = {
      (left, right) match {
        case (_, Nil)           => left
        case (Nil, _)           => right
        case (l :: ls, r :: rs) => if (r < l) r +: merge(left, rs) else l +: merge(ls, right)
      }
    }

    def sortHelper(arr: Seq[Int]): Seq[Int] = {
      if (arr.length <= 1) {
        arr
      } else if (arr.length == 2) {
        if (arr.head > arr.last) Seq(arr.last, arr.head) else arr
      } else {
        val (left, right) = arr.splitAt(arr.length / 2)
        merge(sortHelper(left), sortHelper(right))
      }
    }

    sortHelper(data)
  }

  println(mergeSort(Seq(2, 5, 4, 7, 8, 1, 3, 6)))
  println(mergeSort(Seq()))
  println(mergeSort(Seq(1)))
}
