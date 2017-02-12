import java.io.PipedInputStream
import java.io.PipedOutputStream

class Center {

}

object Center {

  def connect(name: String) = {
    val c = new Handler(name);
    CoRoutine.generator { c }.run(null);
    c
  }

  def main(args: Array[String]): Unit = {
    for {
      x <- (1 to 5)
    } {
      val v = new Vehicle("vehicle" + x);
      val c3 =
        CoRoutine.generator {
          v
        }

      c3.run(0);
    }
  }

}

