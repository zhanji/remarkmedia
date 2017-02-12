import java.io.InputStream
import java.io.ObjectInputStream
import scala.collection.mutable.SynchronizedQueue

class Handler(name: String) extends CoTask[Location, Int] {
  val queue = new SynchronizedQueue[Location]()

  def send(l: Location) {
    queue.enqueue(l)
  }

  run = (_: Location) => {
    var done = false;
    while (!done) {
      val (loc, quality) = next()
      var progx = loc.x
      var progy = loc.y

      if (null != loc) {
        loc.orient match {
          case 0 => {
            progx = loc.x + (loc.speed * (2 + quality * 0.5)).toInt
          }
          case 1 => {
            progy = loc.y - (loc.speed * (2 + quality * 0.5)).toInt
          }
          case 2 => {
            progx = loc.x - (loc.speed * (2 + quality * 0.5)).toInt
          }
          case 3 => {
            progy = loc.y + (loc.speed * (2 + quality * 0.5)).toInt
          }
        }
        println(name + ":" + loc + "   prognosis (" + progx + "," + progy + "," + loc.orient + ")")
      } else {
        done = true;
      }
    }
    println("done")
    0
  }
  private var lastloc = null.asInstanceOf[Location];
  private var lastTS = 0L;
  private def next() = {
    var quality = 0;
    while (quality == 0 && queue.size <= 2) {
      sleep(10);
      if (500 < System.currentTimeMillis() - lastTS) {
        if (null != lastloc) quality = 1
      }
    }
    lastTS = System.currentTimeMillis();
    if (0 == quality) {
      val o = queue.dequeue();
      lastloc = o;
      (o, 0)
    } else {
      (lastloc, 1)
    }
  }
}