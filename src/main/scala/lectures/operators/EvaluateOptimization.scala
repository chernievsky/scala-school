package lectures.operators

import lectures.functions.{Data, Computation, CurriedComputation, FunctionalComputation}

/**
  * В задачке из lectures.functions.Computations мы реализовали
  * один и тот же метод 3-мя разными способами
  *
  * Пришло время оценить, насколько разные имплементации
  * отличаются друг от друга по производительности
  *
  * Для этого
  *   * в классах CurriedComputation и FunctionalComputation уберите extends App, оставьте extends Data
  *   * раскомментируйте код, выполните в циклах вызов 3-х имплементаций,
  *   * оцените разницу во времени выполнения и объясните ее происхожение
  *
  */
object EvaluateOptimization extends App with Data {

  val startTimestamp = System.currentTimeMillis()


  // ВЫПОЛНИТЬ В ЦИКЛЕ  ОТ 1 ДО 100 Computation.computation(
  //    for(??? <- ???) {
  //    print("elapsed time in Computation.computation" + (System.currentTimeMillis() - startTimestamp))
  //   }

  // ВЫПОЛНИТЬ В ЦИКЛЕ  ОТ 1 ДО 100 CurriedComputation.partiallyAppliedCurriedFunction(
  //    for(??? <- ???) {
  //     ???
  //    print("elapsed time " + (System.currentTimeMillis() - startTimestamp))
  //   }

  // ВЫПОЛНИТЬ В ЦИКЛЕ  ОТ 1 ДО 100 FunctionalComputation.filterApplied
  //    for(??? <- ???) {
  //     ???
  //    print("elapsed time " + (System.currentTimeMillis() - startTimestamp))
  //   }

  // ВЫВЕСТИ РАЗНИЦУ В ПРОДОЛЖИТЕЛЬНОСТИ ВЫПОЛНЕНИЯ МЕЖДУ КАРРИРОВАННОЙ ВЕРСИЕЙ
  // И ФУНКЦИОНАЛЬНОЙ

  //  val diff = ???

  ///  print(s"Difference is about $diff milliseconds")
}

