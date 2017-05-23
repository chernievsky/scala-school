package lectures.concurrent

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Smooth - это своебразный функциональный кэш, предназначенный для исключения повторных вызовов кода
  * до того, как получен результат первого вызова.
  * Он работает следующим образом:
  * * * * в объект Smooth в метод apply передается код, который может выполняться какое-то время, и возвращает какое-то значение
  * * * * apply создаст инстанс Smooth
  * * * * созданный инстанс при вызове apply возвращает Future
  * * * * * и запускает код, если код еще не запущен
  * * * * * и не запускает код, если код еще не завершился с момента предыдущего запуска
  *
  * Подсказка: можно использовать AtomicReference
  *
  */
object Smooth {
  def apply[T](thunk: => T): Smooth[T] = new Smooth[T](thunk)
}

class Smooth[T](thunk: => T) {
  def t: T = thunk
  var storage: AtomicReference[Option[Future[T]]] = new AtomicReference[Option[Future[T]]](None)

  def apply(): Future[T] = {
    storage.get() match {
      case Some(f) => f
      case None =>
        val f = Future[T](t).andThen{ case _ => storage.set(None) }
        storage.set(Some(f))
        f
    }
  }
}

object Test extends App {
  val s1 = Smooth({ println("start"); Thread.sleep(500); (Math.random() * 1000).toInt })
  val r1 = s1()
  val r2 = s1()
  val r3 = s1()
  Thread.sleep(1000)
  val r4 = s1()
  for {
    rr1 <- r1
    rr2 <- r2
    rr3 <- r3
    rr4 <- r4
  } { println(s"$rr1 $rr2 $rr3 $rr4") }
}
