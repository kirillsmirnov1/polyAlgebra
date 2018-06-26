import java.awt.geom.Ellipse2D

import scala.math.{cos, sin}
import java.awt.Graphics2D

//Класс рисуемой точки
class Point(xc: Double, yc: Double) extends Drawable {
  var x : Double = xc
  var y : Double = yc

  //Передвинуть точку на вектор
  def movePoint(vector : Point): Unit ={
    x += vector.x
    y += vector.y
  }

  //Повернуть точку на угол относительно центра координат
  def turnOnRadAngle(angle : Double): Unit ={
    val p = new Point(x*cos(angle) + y*sin(angle), -x*sin(angle)+y*cos(angle))
    x = p.x
    y = p.y
  }

  //Нарисовать точку
  override def draw(g : Graphics2D, transparent : Boolean = false): Unit = {
    val pointSize : Double = 4.0

    g.fill(new Ellipse2D.Double( x-pointSize/2, y-pointSize/2, pointSize, pointSize) )
  }
}
