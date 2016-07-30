package lectures.operators

import lectures.functions.{Data, Computation, CurriedComputation, FunctionalComputation}

/**
  * В задачке из lectures.functions.Computations, мы реализовали
  * один и тот же метод 3-я разными способами
  *
  * Пришло время оценить на сколько разные имплементации
  * отличаются друг от друга по производительности
  *
  * Для этого, раскомментируйте код, выполните в циклах вызов 3-х имплементаций
  * Оцените разницу во времени выполнения и объясните ее происхожение
  *
  */
object EvaluateOptimization extends App with Data {

  val startTimestamp = System.currentTimeMillis()

  //ВЫПОЛНИТЬ В ЦИКЛЕ  ОТ 1 ДО 100 Computation.computation(
  for(i <- 1 to 100) {
    Computation.computation(filterData, dataArray)
  }
  val afterCompTimestamp = System.currentTimeMillis()
  println("elapsed time in Computation.computation " + (afterCompTimestamp - startTimestamp))

  //ВЫПОЛНИТЬ В ЦИКЛЕ  ОТ 1 ДО 100 CurriedComputation.partiallyAppliedCurriedFunction(
  for(i <- 1 to 100) {
    CurriedComputation.partiallyAppliedCurriedFunction(dataArray)
  }
  val afterCurrCompTimestamp = System.currentTimeMillis()
  println("elapsed time in CurriedComputation.computation " + (afterCurrCompTimestamp - afterCompTimestamp))

  //ВЫПОЛНИТЬ В ЦИКЛЕ  ОТ 1 ДО 100 FunctionalComputation.filterApplied
  for(i <- 1 to 100) {
    FunctionalComputation.filterApplied(dataArray)
  }
  val afterFuncCompTimestamp = System.currentTimeMillis()
  println("elapsed time in FunctionalComputation.computation " + (afterFuncCompTimestamp - afterCurrCompTimestamp))

  //ВЫВЕСТИ РАЗНИЦУ В ПРОДОЛЖИТЕЛЬНОСТИ ВЫПОЛНЕНИЯ МЕЖДУ КАРРИРОВАННОЙ ВЕРСИЕЙ
  //И ФУНКЦИОНАЛЬНОЙ

  val diff = (afterCurrCompTimestamp - afterCompTimestamp) - (afterFuncCompTimestamp - afterCurrCompTimestamp)
  println(s"Difference is about $diff milliseconds")
}

