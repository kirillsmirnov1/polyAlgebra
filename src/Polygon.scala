import java.awt.{Color, Graphics2D}

import scala.collection.mutable.ListBuffer
import scala.util.Random
//Рисуемый полигон
class Polygon(id_ : Int, polyGroup_ : Int = -1) extends Drawable {
  //Точки полигона
  var points = new ListBuffer[Point]()
  //Идентификатор полигона
  val id : Int = id_
  val polyGroup : Int = polyGroup_

  //Разбивка полигона на линии
  def breakToLines() : ListBuffer[Line]={
    //генерирую линии из точек
    var lines = new ListBuffer[Line]()
    for(i <-1 until points.size){
      lines += new Line(points(i-1), points(i), id, polyGroup)
    }
    lines += new Line(points.last, points.head, id, polyGroup)

    //нахожу верхнюю точку полигона
    var upperPointID : Int = 0
    for(i <- lines.indices){
      if(points(i).y > points(upperPointID).y)
        upperPointID = i
    }

    //определяю направление верхней-правой линии
    checkUpperLineOpenness(upperPointID, lines)   //Если на верхней точке лежит вертикальная, то откртость определяется неправильно

    //проверяю линии на открытость-закрытость //TODO упростить циклы
    for(i <- upperPointID+1 until lines.size){
        checkLinesOpenness(lines(i-1), lines(i))
    }

    checkLinesOpenness(lines.last, lines.head)

    for(i <- 1 until upperPointID){
      checkLinesOpenness(lines(i-1), lines(i))
    }

    lines
  }

  //Проверка верхней линии на открытость (верхняя линия начинается с верхней точки)
  def checkUpperLineOpenness(upperPointID : Int, lines : ListBuffer[Line]): Unit ={
    //Определяем линии справа и слева от верхней точки
    val firstLine : Line = lines(upperPointID)
    var secondLine : Line = null
    if(upperPointID == 0){
      secondLine = lines.last}
    else{
      secondLine = lines(upperPointID-1)}

    //Если линия вертикальная, то её открытость не определена
    if(firstLine.incline == LineIncline.VERTICAL){
      firstLine.closeness = LineCloseness.OPENING

      secondLine.setMoveDirection()

      firstLine.moveDirection = secondLine.moveDirection
    }
    else if(secondLine.incline == LineIncline.VERTICAL){
      firstLine.closeness = LineCloseness.OPENING

      firstLine.setMoveDirection()
    }
    else {
      //Если верхняя линия находится под соседней, то она закрывающая
      if (-(secondLine.C + secondLine.A * firstLine.points(1).x) / secondLine.B > firstLine.points(1).y
        && (firstLine.points(1).x - firstLine.points(0).x) * (secondLine.points(1).x - secondLine.points(0).x) < 0)
        firstLine.closeness = LineCloseness.CLOSING
      else
        firstLine.closeness = LineCloseness.OPENING

      firstLine.setMoveDirection()
    }
  }

  //Проверка остальных линий на открытость
  def checkLinesOpenness(previous : Line, current : Line): Unit ={
    if(current.incline == LineIncline.VERTICAL){
      current.moveDirection = previous.moveDirection
      current.closeness     = previous.closeness
    }
    else{
      current.setMoveDirection()

      if(current.moveDirection == previous.moveDirection)
        current.closeness = previous.closeness
      else
        current.closeness = previous.anotherCloseness()
    }
    //Если векторы двух соседних линий по Х сонаправлены, то их открытость совпадает
//    if((current.points(1).x - current.points(0).x) * (previous.points(1).x - previous.points(0).x) >= 0)
//      current.closeness = previous.closeness
//    else //Если нет − нужно ставить другую открытость
//      current.closeness = previous.anotherCloseness()
  }

  def createPolygonByTwoLines(top : Line, bottom : Line): Unit ={
    points += top.points(0)
    points += top.points(1)
    points += bottom.points(1)
    points += bottom.points(0)
  }

  //Отрисовка полигона
  override def draw(g: Graphics2D, transparent : Boolean = false): Unit = {

    val alpha : Int = if (transparent) 0 else 255
    val r = new Random()

    if(points.nonEmpty) { // В один прекрасный момент я начал посылать на отрисовку пустые полигоны
                          // И отрисовка крашилась. Пришлось ввести такое ограничение
      polyGroup match {
        case 1 => g.setColor(new Color(0, 255, 0, alpha))
        case 2 => g.setColor(new Color(255, 0, 0, alpha))
        case 3 => g.setColor(new Color(0,   0, 255, alpha))
        case -1=> g.setColor(new Color(255, 255, 0, alpha))//для булевых операций
        case -2=> g.setColor(Color.black)
        case _ => g.setColor(new Color(123, 123, 123, alpha))
      }

      if (!transparent) { // рисуем грани
        for (i <- 1 until points.length) {
          g.drawLine(points(i).x.toInt, points(i).y.toInt, points(i - 1).x.toInt, points(i - 1).y.toInt)
        }
        g.drawLine(points.head.x.toInt, points.head.y.toInt, points.last.x.toInt, points.last.y.toInt)
      }
      else { // рисуем заливку
        val arrayX, arrayY = new Array[Int](points.size)
        for(i <- points.indices){
          arrayX(i) = points(i).x.toInt
          arrayY(i) = points(i).y.toInt
        }

        g.fillPolygon(arrayX, arrayY, points.size)
      }
    }
  }
}
