import java.io.ObjectOutputStream



/***
   * orient 0 - 3 , 0:right 1:down 2:left 3:up
   */
case class Location(x: Int, y: Int, speed: Int, orient: Int, angle: Int)
class Vehicle(p: String) extends CoTask[Int, Int]{
  val name = p;
  val path = GenPath.restore(name);
  var x = path.x;
  var y = path.y;
  var orient = path.orient
  var speed = path.speed
  var angle = 0;
  val locations = for (v <- path.p) yield {
      val info = Location(x, y, speed, orient, angle);
      angle = 0;
      val direction = v._1
      val nSpd = v._2
      orient match {
        case 0 => {
          x = x + speed
        }
        case 1 => {
          y = y - speed
        }
        case 2 => {
          x = x - speed
        }
        case 3 => {
          y = y + speed
        }
      }
      speed = nSpd
      direction match {
        case 0 => {
          
        }
        case 1 => {
          angle = 90
          orient = (orient + 1) % 4
        }
        case 2 => {
          angle = 270;
          orient = (orient + 3) % 4
        }
      }
      info
  }
  
  val pout = Center.connect(name);

  run = (_: Int) => {
    val path = GenPath.restore(name);
    var count = 0;
    while(count < locations.length){
      sleep(1000)
      pout.send(locations(count))
      count = count +1
    }
    
    pout.send(null)
    pout.send(null)
    pout.send(null)
    
    0
  }
}