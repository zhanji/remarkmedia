
import scala.util.Random._
import java.io._

class GenPath {

}

object GenPath {
  /**
   * *
   * orient 0 - 3 , 0:right 1:down 2:left 3:up
   */
  case class Path(x: Int, y: Int, speed: Int, orient: Int, p: IndexedSeq[(Int, Int)]);
  def random() = {
    scala.util.Random.nextInt(50);
  }
  def speed() = {
    scala.util.Random.nextInt(10) + 1;
  }

  def gen(name: String) = {
    val t = for {
      _ <- (0 to 60 * 20)
      dr = random()
    } yield dr
    //var idx = 0;
    val k = t.map { x =>
      {
        val r = random();
        //idx = idx + 1;
        if (1 == r) {
          (1, speed())
        } else if (99 == r) {
          (1, speed())
        } else {
          (0, speed())
        }
      }
    }
    val p = Path(nextInt(10000), nextInt(10000), nextInt(9), nextInt(4), k);
    val oos = new ObjectOutputStream(new FileOutputStream(name))
    oos.writeObject(p);
    oos.close()
  }

  def restore(name: String) = {
    val ois = new ObjectInputStream(new FileInputStream(name))
    val a = ois.readObject().asInstanceOf[Path];
    ois.close()
    a
  }
  def main(args: Array[String]): Unit = {
    println("Hello, world!")
    for {
      x <- (1 to 5)
    } {
      gen("vehicle" + x)
    }
//    for {
//      x <- (1 to 1)
//    } {
//      //val p = restore("path" + x)
//      val pp = new Vehicle("vehicle" + x);
//      val c3 =
//        CoRoutine.generator {
//          pp
//        }
//
//      c3.run(0);
//      //println(p)
//    }
    //ObjectOutputStream
  }

}
