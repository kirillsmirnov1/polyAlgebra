import java.awt.Graphics2D
//Рисуемая линия, задается двумя точками, рисуема
class Line (P1 : Point,
            P2 : Point,
            initPolygonFather : Int = -1,
            initPolyGroup : Int = -1,
            initLineCloseness : LineCloseness = LineCloseness.UNDEFINED) extends Drawable {

  //точки линии
  var points = Array(P1, P2)

  //Коэффициенты уравнения Ax+By+C=0
  var A : Double = P2.y - P1.y
  var B : Double = P1.x - P2.x
  var C : Double = P1.y * (P2.x - P1.x) + P1.x * (P1.y - P2.y)

  var slope : Double = A / B //угол наклона. по нему определяется можно ли склеить линии

  //Наклон и закрытость линии
  var incline : LineIncline = LineIncline.UNKNOWN //0
  var closeness : LineCloseness = initLineCloseness
  var moveDirection : MoveDirection = MoveDirection.UNDEF

  if(A == 0)  incline = LineIncline.HORIZONTAL //2
  else
  if(B == 0)  incline = LineIncline.VERTICAL  //3
  else
              incline = LineIncline.NORMAL  //1

  //id полигона к которому принадлежала линия
  var polygonFather : Int = initPolygonFather
  var polyGroup : Int = initPolyGroup

  var slice : Int = -1 //вертикальный слой в котором была линия на стадии обрезки, по умолчанию не определен
  var polyCase : Int = -1

  //Возвращает закрытость обратную закрытости линии
  def anotherCloseness() : LineCloseness={
    if(closeness == LineCloseness.OPENING) LineCloseness.CLOSING
    else if(closeness == LineCloseness.CLOSING) LineCloseness.OPENING
    else LineCloseness.UNDEFINED
  }

  //Поворот линии на угол относительно центра координат
  def turnOnRadianAngle(angle : Double): Unit ={
    points(0).turnOnRadAngle(angle)
    points(1).turnOnRadAngle(angle)
  }

  //Перемещение линии на вектор
  def movePoints(moveVector : Point): Unit ={
    points.foreach(_.movePoint(moveVector))
  }

  //Поменять точки местами
  def swapPoints(): Unit ={
    val p = points(0)
    points(0) = points(1)
    points(1) = p
  }

  //Ставит левую точку первой
  def makeLeft(): Unit ={
    if(points(0).x > points(1).x){
      swapPoints()
    } else if(points(0).x == points(1).x && points(0).y < points(1).y){
      swapPoints()
    }
  }

  //Рисует линию
  override def draw(g: Graphics2D, transparent : Boolean = false): Unit = {
    g.drawLine(points(0).x.toInt, points(0).y.toInt, points(1).x.toInt, points(1).y.toInt)
  }

  //Находится ли точка в прямоугольнике надетом на края линии
  def pointInScope(p: Point) : Boolean = {
    pointInXScope(p) && pointInYScope(p)
  }

  //Лежит ли точка в Х диапазоне линии
  def pointInXScope(p : Point) : Boolean = {
    p.x >= points(0).x && p.x <= points(1).x//TODO обработать неидеальный случай
  }

  //Лежит ли точка в Y диапазоне линии
  def pointInYScope(p : Point) : Boolean = {
    if(points(0).y > points(1).y)
      p.y >= points(1).y && p.y <= points(0).y
    else
      p.y <= points(1).y && p.y >= points(0).y
  }

  //Обрезать линию по Х-границам
  def cutLineByXScope(left : Double, right : Double): Unit ={
    points(0) = new Point(left, -(C + A * left) / B)
    points(1) = new Point(right, -(C + A * right) / B)
  }

  def setMoveDirection(): Unit ={
    if(points(0).x < points(1).x) moveDirection = MoveDirection.RIGHT
    else                          moveDirection = MoveDirection.LEFT
  }

  def left(): Point ={
    points(0)
  }

  def right(): Point ={
    points(1)
  }
}
