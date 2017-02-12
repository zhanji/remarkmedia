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

object CoRoutine {
  val ex = new ScheduledThreadPoolExecutor(1);
  val executorService = Executors.newFixedThreadPool(1)
  val executionContext = ExecutionContext.fromExecutorService(executorService)
  implicit val ec = executionContext

  sealed trait Trampoline[+U]
  //  case object Done extends Trampoline[Nothing,Nothing]
  case class Done[T, U](result: U, r: T) extends Trampoline[U]
  case class Suspend[T, U](result: U, to: Long, p: T, next: T => Trampoline[U]) extends Trampoline[U]

  class CoRoutine[T, U] {myCR =>
    var nullResult: U = null.asInstanceOf[U];
    //  var myTask: T => U @cpsParam[U,CoRoutine.Trampoline[T]] = null;
    var myTask: T => CoRoutine.Trampoline[U] = null;
    var taskResult: U = nullResult;

    def this(fun2: T => CoRoutine.Trampoline[U]) {
      this();
      myTask = fun2;
    }

    def apply(v : T) : U = {
      call(v);
    }
    def run1(v: T) = {
      val f1 = Future {
        var result =
          myTask(v);

        breakable {
          while (true) {
            result match {
              case CoRoutine.Suspend(tmpV, to, _, func) => {
                //System.out.println("wait ..... " + to);
                if (0 < to) Thread.sleep(to);
                result = func.asInstanceOf[T => Trampoline[U]](v);
              }
              case Done(x, _) => {
                System.out.println("get result " + x);
                taskResult = x;
                break;
              }
            }

          }
        }
        taskResult
      }

      f1.onComplete {
        //case Success(value) => println(s"Got the callback, meaning = $value")
        case Failure(e)     => e.printStackTrace
      }
    }
    def run(v: T) : Unit = {
      val f1 = Future {
        var result =
          myTask(v);

        //val outer = this;
        breakable {
          while (true) {
            result match {
              case CoRoutine.Suspend(tmpV, to, _, func) => {
                //System.out.println("wait ..... " + to);
                myTask = func.asInstanceOf[T => Trampoline[U]];
                if (0 < to) {
                  ex.schedule(new Runnable{
                    def run = {
                       myCR.run(v);
                    }
                  }, to, TimeUnit.MILLISECONDS);
                }
                break;
              }
              case Done(x, _) => {
                System.out.println("get result " + x);
                taskResult = x;
                break;
              }
            }

          }
        }
        taskResult
      }

      f1.onComplete {
        case Success(value) => //println(s"Got the callback, meaning = $value")
        case Failure(e)     => e.printStackTrace
      }
    }

    def call(v: T) = {
      if (null != myTask) {
        var result =
          myTask(v);
        result match {
          case CoRoutine.Suspend(tmpU, to, _, func) => {
            myTask = func.asInstanceOf[T => Trampoline[U]];
            taskResult = tmpU;
          }
          case Done(x, _) => {
            taskResult = x;
            myTask = null;
          }
        }
      }
      taskResult;
    }
    
    def done() = {
      if(null != myTask) {
        false
      } else {
        true
      }
    }
  }
  def generator[T, U](body: T => U @cps[Trampoline[U]]) = {
    new CoRoutine((x: T) => reset { val vv = body(x); Done(vv, null.asInstanceOf[U]) })
  }

  def generator[T, U](t: CoTask[T, U]) = {
    new CoRoutine((x: T) => reset { val vv = t.run(x); Done(vv, null.asInstanceOf[U]) })
  }
  def suspend[T, U](v: U, to: Long = -1) = {
    shift {
      k: (T => Trampoline[U]) =>
        {
          Suspend(v, to, null.asInstanceOf[T], k);
        }
    }
  }

}



