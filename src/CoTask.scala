import scala.util.continuations._
import scala.concurrent.Future
//import scala.concurrent.ExecutionContext.Implicits.global
import java.util.concurrent._
import java.util.concurrent.Executors
import concurrent.ExecutionContext
import scala.concurrent.Await
import scala.util.control.Breaks._
import scala.util.Success
import scala.util.Failure

abstract class CoTask[T, U] {
  val defaultResult = null.asInstanceOf[U];
  val defaultParam = null.asInstanceOf[T];
  def sleep(ts: Long) = {
    //CoRoutine.suspend(defaultResult, ts);
    shift {
      k: (T => CoRoutine.Trampoline[U]) =>
        {
          CoRoutine.Suspend(defaultResult, ts, defaultParam, k);
        }
    }
  };
  def suspend(u: U) = {
    //CoRoutine.suspend(v, -1);
    shift {
      k: (T => CoRoutine.Trampoline[U]) =>
        {
          CoRoutine.Suspend(u, -1, defaultParam, k);
        }
    }
  };
  var run: (T => U @cps[CoRoutine.Trampoline[U]]) = null;
}