package sg.beeline.util

import scala.concurrent.duration._

object ExpiringCache {
  def apply[T](duration: Duration)(v: => T) =
    new ExpiringCache[T](duration, v)
}

class ExpiringCache[T](duration: Duration, v : => T)
    extends (() => T) {
  var value : Option[T] = None
  var lastTimestamp : Long = 0L

  def refreshed = {
    value = Some(v)
    lastTimestamp = System.currentTimeMillis()
    value.get
  }

  def apply(): T = synchronized {
    value match {
      case None => refreshed
      case Some(w) =>
        val diff = System.currentTimeMillis() - lastTimestamp

        if (diff.millis > duration) {
          refreshed
        } else {
          w
        }
    }
  }
}
