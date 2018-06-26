import java.awt.{Color, Graphics2D}
import java.awt.RenderingHints._
import java.awt.geom._
import java.awt.image.BufferedImage
import java.io.{File, FileReader, StringReader}
import java.util.Scanner

import scala.collection.mutable

//import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.util.control.Breaks._

object drawing  {
  // Size of image
  val size: (Int, Int) = (1600, 1400)

  // create an image
  val canvas = new BufferedImage(size._1, size._2, BufferedImage.TYPE_INT_RGB)

  // get Graphics2D for the image
  val transform : AffineTransform = AffineTransform.getTranslateInstance(size._1/2, size._2/2)
  //transform.scale(size._1/2, -size._2/2)
  transform.scale(1, -1)

  val g : Graphics2D = canvas.createGraphics()
  g.setTransform(transform)

  var greatPolygonIterator : Int = 0

  def draw(drawable: Drawable): Unit ={
    drawable.draw(g)
  }

  def draw(drawables : ListBuffer[Drawable], transparent : Boolean = false): Unit ={
    drawables.foreach{_.draw(g, transparent)}
  }

  def drawSectors(points: ListBuffer[Point]): Unit ={
    for(i <- points.indices){
      g.drawLine(points(i).x.toInt, -size._2/2, points(i).x.toInt, size._2/2)
    }
  }

  //отрисовка открывающих линий зеленым а закрывающих красным
  def drawOpenings(lines : ListBuffer[Line]): Unit ={
    lines.foreach{ l =>
      l.closeness match{
        case LineCloseness.UNDEFINED  => g.setColor(Color.YELLOW)
        case LineCloseness.OPENING    => g.setColor(Color.GREEN)
        case LineCloseness.CLOSING    => g.setColor(Color.RED)
      }
      l.draw(g)
    }
  }

  def drawSlices(slices : ListBuffer[ListBuffer[Line]]): Unit ={
    val random = new Random()
    slices.foreach{slice =>
      val color = new Color(random.nextFloat(), random.nextFloat(), random.nextFloat())
      g.setColor(color)
      slice.foreach{line =>
        line.draw(g)
      }
    }
  }

  def readPolygonDataFromFile(fileName : String): ListBuffer[Polygon] ={
    val fileReader = new FileReader(fileName)
    val scan = new Scanner(fileReader)
    var polygons = new ListBuffer[Polygon]()

    while(scan.hasNextLine) {
      var line = scan.nextLine()
      line = line.replace("(", "")
      line = line.replace(")", "")
      val scanner = new Scanner(new StringReader(line))
      var intBuffer = new ListBuffer[Integer]

      while (scanner.hasNextInt()) {
        intBuffer += scanner.nextInt()
      }

      val NumOfPoints = (intBuffer.size - 1) / 2
      var currentPolygon = new Polygon(greatPolygonIterator, intBuffer.head)
      intBuffer.remove(0)

      for (i <- 0 until NumOfPoints) {
        currentPolygon.points += new Point(intBuffer(2 * i).toDouble, intBuffer(2 * i + 1).toDouble)
      }
      polygons += currentPolygon

      greatPolygonIterator += 1
    }
    fileReader.close()

    polygons
  }

  def breakPolygonsToLines(polygons : ListBuffer[Polygon]) : ListBuffer[Line] = {
    var lines = new ListBuffer[Line]()
    polygons.foreach(lines ++= _.breakToLines())

    lines.foreach(_.makeLeft())
    lines = lines.sortWith(_.points(0).x < _.points(0).x)

    lines
  }

  def findIntersection(l1 : Line, l2 : Line) : Point = {

    val p : Point= findEqualPoints(l1, l2)
    if(p != null)
      return p

    l1.incline.ordinal() * l2.incline.ordinal() match {
      //TODO замени магические числа на enum
      case 1 => lineIntersection1(l1, l2) //normal x normal
      case 2 => lineIntersection2(l1, l2) //normal x horizontal
      case 3 => lineIntersection3(l1, l2) //normal x vertical
      case 4 => null //horizontal x horizontal
      case 6 => lineIntersection6(l1, l2) //horizontal x vertical
      case 9 => null //vertical x vertical
      case _ =>
        println("fatal error: wrong types of lines intersections")
        null
    }
  }

  def findEqualPoints(l1 : Line, l2 : Line) : Point =  {
    if(l1.points(0) == l2.points(0) || l1.points(0) == l2.points(1))
      l1.points(0)
    else if(l1.points(1) == l2.points(0) || l1.points(0) == l2.points(1))
      l1.points(1)
    else null
  }

  def lineIntersection1(l1 : Line, l2 : Line) : Point = {

    val B3 : Double = l1.B - l2.B * l1.A / l2.A
    val C3 : Double = l1.C - l2.C * l1.A / l2.A
    val y = -C3 / B3
    val x = -(l1.B * y + l1.C) / l1.A
    val currentPoint = new Point(x, y)

    if(l1.pointInScope(currentPoint) && l2.pointInScope(currentPoint))
      currentPoint
    else
      null
  }

  def lineIntersection2(l1 : Line, l2 : Line) : Point = {
    var locL1 = l1
    var locL2 = l2

    if(l2.A != 0){
      locL1 = l2
      locL2 = l1
    }

    val y = -locL2.C / locL2.B
    val x = -(locL1.B * y + locL1.C) / locL1.A
    val currentPoint = new Point(x, y)

    if(l1.pointInScope(currentPoint) && l2.pointInScope(currentPoint))
      currentPoint
    else
      null
  }

  def lineIntersection3(l1 : Line, l2 : Line) : Point = {
    var locL1 = l1
    var locL2 = l2

    if(l2.B != 0){
      locL1 = l2
      locL2 = l1
    }

    val x = -locL2.C/locL2.A
    val y = -(locL1.A*x+locL1.C)/locL1.B
    val currentPoint = new Point(x, y)

    if(l1.pointInScope(currentPoint) && l2.pointInScope(currentPoint))
      currentPoint
    else
      null
  }

  def lineIntersection6(l1 : Line, l2 : Line) : Point = {
    var locL1 = l1
    var locL2 = l2

    if(l2.B != 0){
      locL1 = l2
      locL2 = l1
    }

    val currentPoint = new Point(-locL2.C/locL2.A, -locL1.C/locL1.B)

    if(l1.pointInScope(currentPoint) && l2.pointInScope(currentPoint))
      currentPoint
    else
      null
  }

  def findPolygonsIntersections(polygons : ListBuffer[Polygon]) : ListBuffer[Point]={
    var intersectionPoints = new ListBuffer[Point]()

    polygons.foreach(intersectionPoints ++= _.points)

    var linesFromPolygons = breakPolygonsToLines(polygons)

    linesFromPolygons.foreach(_.makeLeft())

    linesFromPolygons = linesFromPolygons.sortWith((a, b) =>  a.points(0).x > b.points(0).x ||  a.points(0).x == b.points(0).x && a.points(1).x > b.points(1).x)

    intersectionPoints ++= findLinesIntersections(linesFromPolygons)

    intersectionPoints = intersectionPoints.sortWith(_.x < _.x)

    intersectionPoints
  }

  def findLinesIntersections(lines : ListBuffer[Line]) : ListBuffer[Point]={
    var intersectionPoints = new ListBuffer[Point]()

    for(i <- 0 until lines.size - 1){
      breakable{
        for(j <- i + 1 until lines.size){
          if(lines(i).polygonFather != lines(j).polygonFather) {
            if (lines(i).points(1).x < lines(j).points(0).x)
              break
            var currentPoint = findIntersection(lines(i), lines(j))
            if (currentPoint != null)
              intersectionPoints += currentPoint
          }
        }
      }
    }

    intersectionPoints
  }

  def generateRandomPolygons(numberOfPoints : Int = 3, numberOfPolygons : Int = 5, groupsOfPolygons : Int = 2) : ListBuffer[Polygon]={//TODO сделать проверку на самопересекающийся полигон
  val polygons = new ListBuffer[Polygon]()
    val random = new Random()

    for(k <- 1 to groupsOfPolygons){
      0 until numberOfPolygons foreach{_ =>
        polygons += new Polygon(greatPolygonIterator, k)
        0 until numberOfPoints foreach {_ =>
          polygons.last.points += new Point(random.nextInt(size._1) - size._1 / 2, random.nextInt(size._2) - size._2 / 2)
        }
        greatPolygonIterator += 1
      }
    }

    polygons
  }

  def generateSlices(lines : ListBuffer[Line], divs : ListBuffer[Point]): ListBuffer[ListBuffer[Line]] = {
    var slices = new ListBuffer[ListBuffer[Line]]()

    for(i <- 1 until divs.size){
      val left : Double = divs(i-1).x
      val right : Double = divs(i).x
      if(left != right){
        var currentSlice = new ListBuffer[Line]()
        lines.foreach{ line =>
          if(line.points(0).x <= left && line.points(1).x >= right){
            var newLine = new Line(line.points(0), line.points(1), line.polygonFather, line.polyGroup, line.closeness)
            newLine.cutLineByXScope(left, right)
            newLine.slice = i-1
            currentSlice += newLine
          }
        }
        slices += currentSlice
      }
    }

    slices = for (slice <- slices)
      yield slice.sortWith((a, b) =>
        (a.points(0).y + a.points(1).y) > (b.points(0).y + b.points(1).y))

    slices
  }

  def chooseSlicesWithBoolOperation(slices : ListBuffer[ListBuffer[Line]], operation : BoolOperation, A : Int, B: Int): ListBuffer[ListBuffer[(Line, Line)]]= {

    val newSlices   = new ListBuffer[ListBuffer[(Line, Line)]]()  // Линии из которых строятся эти фрагменты
    var currentTuple : (Line, Line) = new (Line, Line)(null, null)

    slices.foreach{ slice =>
      var state = (0, 0) //открытость по A, B
    var aOpen = 0 //Количество открывающих линий А
    var bOpen = 0 //Количество открывающих линий B
    val newSlice = new ListBuffer[(Line, Line)]() //Новый слайс в котором хранятся только нужные линии

      slice.foreach { line =>

        if (line.incline != LineIncline.VERTICAL) { // Вертикальные линии не влияют на алгоритм, их можно отбросить

          state match {
            case (0, 0) =>
              line.polyGroup match {
                case A =>
                  line.closeness match {
                    case LineCloseness.OPENING =>
                      aOpen = 1

                      if (operation == BoolOperation.OR  ||
                        operation == BoolOperation.XOR ||
                        operation == BoolOperation.NOT) {
                        // открывающая
                        currentTuple = currentTuple.copy(_1 = line)
                      }
                      state = (1, 0)

                    case _ => println("Fatal error: wrong closing #01")
                  }
                case B =>
                  line.closeness match {
                    case LineCloseness.OPENING =>
                      bOpen = 1

                      if (operation == BoolOperation.OR ||
                        operation == BoolOperation.XOR) {
                        // открывающая
                        currentTuple = currentTuple.copy(_1 = line)
                      }
                      state = (0, 1)
                    case _ => println("Fatal error: wrong closing #02")
                  }
                case _ => println("Fatal error: wrong group")
              }
            ///////////////////////////////////////////////////////////////////////////
            case (0, 1) =>
              line.polyGroup match {
                case A =>
                  line.closeness match {
                    case LineCloseness.OPENING =>
                      aOpen = 1

                      if (operation == BoolOperation.AND) {
                        //открывающая
                        currentTuple = currentTuple.copy(_1 = line)
                      }
                      else if (operation == BoolOperation.XOR) {
                        // закрывающая
                        currentTuple = currentTuple.copy(_2 = line)
                        newSlice += currentTuple
                      }

                      state = (1, 1)
                    case _ => println("Fatal error: wrong closing #03")
                  }
                case B =>
                  line.closeness match {
                    case LineCloseness.OPENING =>
                      bOpen += 1
                    case LineCloseness.CLOSING =>
                      bOpen -= 1

                      if (bOpen == 0) {
                        if (operation == BoolOperation.OR ||
                          operation == BoolOperation.XOR){
                          // закрывающая
                          currentTuple = currentTuple.copy(_2 = line)
                          newSlice += currentTuple
                        }

                        state = (0, 0)
                      }
                    case _ => println("Fatal error: wrong closing #04")
                  }
                case _ => println("Fatal error: wrong group")
              }
            //////////////////////////////////////////////////////////////////////////
            case (1, 0) =>

              line.polyGroup match {
                case A =>
                  line.closeness match {
                    case LineCloseness.OPENING =>
                      aOpen += 1
                    case LineCloseness.CLOSING =>
                      aOpen -= 1

                      if (aOpen == 0) {
                        if (operation == BoolOperation.OR  ||
                          operation == BoolOperation.XOR ||
                          operation == BoolOperation.NOT){
                          // закрывающая
                          currentTuple = currentTuple.copy(_2 = line)
                          newSlice += currentTuple
                        }

                        state = (0, 0)
                      }
                    case _ => println("Fatal error: wrong closing #05")
                  }
                case B =>
                  line.closeness match {
                    case LineCloseness.OPENING =>
                      bOpen = 1

                      if (operation == BoolOperation.AND){
                        // открывающая
                        currentTuple = currentTuple.copy(_1 = line)
                      }
                      else if (operation == BoolOperation.XOR ||
                        operation == BoolOperation.NOT){
                        // закрывающая
                        currentTuple = currentTuple.copy(_2 = line)
                        newSlice += currentTuple
                      }

                      state = (1, 1)
                    case _ => println("Fatal error: wrong closing #06")
                  }
                case _ => println("Fatal error: wrong group")
              }
            ///////////////////////////////////////////////////////////////////////////
            case (1, 1) =>

              line.polyGroup match {
                case A =>
                  line.closeness match {
                    case LineCloseness.OPENING =>
                      aOpen += 1
                    case LineCloseness.CLOSING =>
                      aOpen -= 1

                      if (aOpen == 0) {
                        if (operation == BoolOperation.AND){
                          // закрывающая
                          currentTuple = currentTuple.copy(_2 = line)
                          newSlice += currentTuple
                        }
                        else if (operation == BoolOperation.XOR){
                          // открывающая
                          currentTuple = currentTuple.copy(_1 = line)
                        }

                        state = (0, 1)
                      }
                    case _ => println("Fatal error: wrong closing #07")
                  }
                case B =>
                  line.closeness match {
                    case LineCloseness.OPENING =>
                      bOpen += 1
                    case LineCloseness.CLOSING =>
                      bOpen -= 1

                      if (bOpen == 0) {
                        if (operation == BoolOperation.AND){
                          // закрывающая
                          currentTuple = currentTuple.copy(_2 = line)
                          newSlice += currentTuple
                        }
                        else if (operation == BoolOperation.XOR ||
                          operation == BoolOperation.NOT){
                          // открывающая
                          currentTuple = currentTuple.copy(_1 = line)
                        }

                        state = (1, 0)
                      }
                    case _ => println("Fatal error: wrong closing #08")
                  }
                case _ => println("Fatal error: wrong group")
              }
            case _ => println("Fatal error: wrong state")
          }
        }
      }

      if(newSlice.nonEmpty)
        newSlices += newSlice
    }

    newSlices
  }

  def boolOperation(slices : ListBuffer[ListBuffer[Line]], operation: BoolOperation, A : Int, B : Int): ListBuffer[Polygon] = {
    val newPolygons = new ListBuffer[Polygon]()

    val newSlices = chooseSlicesWithBoolOperation(slices, operation, A, B)

    newSlices.foreach{ slice =>
      slice.foreach{ tuple =>
        addPolygonFromTwoLinesToList(tuple._1, tuple._2, newPolygons)
      }
    }

    unitePolygonsFromSlices(newSlices)

    newPolygons
  }
  //work in progress

  def groupPolygonSlices(slices : ListBuffer[ListBuffer[(Line, Line)]]): ListBuffer[Map [Int, ListBuffer[(Line, Line)]]] ={
    var slicesIterator : Int = 0

    val slicesUnion = new ListBuffer[Map [Int,        ListBuffer[(Line, Line)]]] // наука зашла слишком далеко
    //                    полигоны  [слои[номер слоя, пары линий в слое]]

    //Проверяем все слайсы слева направо
    slices.foreach{ slice =>

      //В каждом слайсе берем пару, обзываем её правой
      slice.foreach{ rightTuple =>
        var foundPlaceForTuple : Int = 0
        var polygonsWithTuple = new ListBuffer[Int]()

        //Берем массив объединенных полигонов
        for(polyIter <- slicesUnion.indices) {
          if(!( polygonsWithTuple contains polyIter)) {
            //Смотрим есть ли в текущем полигоне куски с предыдущего слайса
            if (slicesUnion(polyIter) contains slicesIterator - 1) {
              //Если есть
              //Берем каждую пару, называем левой
              slicesUnion(polyIter)(slicesIterator - 1).foreach { leftTuple =>
                //И смотрим, пересекается ли она с правой парой
                if (tupleIntersect(leftTuple, rightTuple)) {
                  foundPlaceForTuple += 1
                  polygonsWithTuple += polyIter

                  //Если пересекается
                  //Но ее некуда положить
                  if (!(slicesUnion(polyIter) contains slicesIterator)) {
                    //Делаем под этот слой новый мэп и кладем пару туда
                    slicesUnion(polyIter) += (slicesIterator -> ListBuffer[(Line, Line)](rightTuple))
                  }
                  else {
                    //Если мэп уже был, добавляем пару туда
                    if ( ! (slicesUnion(polyIter)(slicesIterator) contains rightTuple)){
                      slicesUnion(polyIter)(slicesIterator) += rightTuple
                    }
                  }
                }
              }
            }
          }

        }
        //Если мы не нашли куда примкнуть пару, под неё нужно создать новый полигон
        if(foundPlaceForTuple == 0){
          slicesUnion += Map(slicesIterator -> ListBuffer[(Line, Line)](rightTuple))
        }
        else if (foundPlaceForTuple > 1){
          polygonsWithTuple = polygonsWithTuple.distinct

          if(polygonsWithTuple.size > 1) {
            var unitedPoly: Map[Int, ListBuffer[(Line, Line)]] = Map[Int, ListBuffer[(Line, Line)]]()
            polygonsWithTuple.foreach { n =>
              slicesUnion(n).foreach { map =>
                if (unitedPoly contains map._1) {
                  unitedPoly(map._1) ++= map._2
                  val buffer : ListBuffer[(Line, Line)] = unitedPoly(map._1).distinct
                  unitedPoly -= map._1
                  unitedPoly += (map._1 -> buffer)
                }
                else {
                  unitedPoly += map
                }
              }

            }

            //Удаляю повторения пар
            for (i <- polygonsWithTuple.indices) {
              slicesUnion.remove(polygonsWithTuple(polygonsWithTuple.size - 1 - i))//удаляю с конца чтобы преждевременно не нарушить порядок
            }
            slicesUnion += unitedPoly
          }
        }
      }

      slicesIterator += 1
    }

    drawUnitedPolygons(slicesUnion) //отрисовка групп полигонов из кусочков

    slicesUnion
  }

  def buildPolygonsFromGroups(slicesUnion : ListBuffer[Map [Int, ListBuffer[(Line, Line)]]]) : ListBuffer[Polygon] = {

    val unitedPolygons = new ListBuffer[Polygon]()

    var unitedLinePolygons = new ListBuffer[ListBuffer[Line]]()

    slicesUnion.foreach{ poly => //Сейчас полигон это Map[Int, ListBuffer[(Line, Line)]]

      var tPolygon = new ListBuffer[(Line, Line)]() // У poly сложная структура, гружу все в линейный массив

      poly.foreach{map =>
        map._2.foreach{tuple =>
          tPolygon += tuple
        }
      }

      tPolygon = tPolygon.distinct //на всякий случай

      //println("tPolygon size: " + tPolygon.size + "\ntPolygon slices: ")
      //tPolygon.foreach{l => print(l._1.slice + ", ")}

      var lPolygon = new ListBuffer[Line]()
      var leftSlice = Int.MaxValue
      var leftSlicePos = 0

      for(i <- tPolygon.indices){  //Надо найти самый левый слайс и начать с него
        if(tPolygon(i)._1.slice < leftSlice){
          leftSlice = tPolygon(i)._1.slice
          leftSlicePos = i
        }
        else if(tPolygon(i)._1.slice == leftSlice){
          if(tPolygon(i)._1.left().y > tPolygon(leftSlicePos)._1.left().y){
            leftSlicePos = i
          }
        }
      }

      lPolygon ++= ListBuffer[Line] (tPolygon(leftSlicePos)._1, tPolygon(leftSlicePos)._2)//загружаю первую пару в полигон
      lPolygon(0).polyCase = 1
      lPolygon(1).polyCase = if(lPolygon(0).left().y == lPolygon(1).left().y) 16 else 14
      lPolygon(0).moveDirection = MoveDirection.RIGHT
      lPolygon(1).moveDirection = MoveDirection.LEFT

      tPolygon.remove(leftSlicePos)

      //пользуясь случаем напоминаю, что в этих линиях 0-точка левее 1-точки

      var tupleAdded : Boolean = true

      while (tPolygon.nonEmpty && tupleAdded) {

        tupleAdded = false

        breakable {
        //Вставка пар линий в полигон линий
        tPolygon.foreach { tuple =>

            for (i <- 0 until lPolygon.size - 1) {

              // смотрим как пересекает вертикаль
              val slicePos = tuple._1.slice - lPolygon(i).slice

              lPolygon(i).polyCase match {
                case 0 => if(slicePos == 1) {
                  if (tuple._1.left().y != tuple._2.left().y) {
//                    if (tuple._2.left().y < lPolygon(i).right().y
//                      && tuple._2.left().y >= lPolygon(i + 1).left().y) {
                    if(verticalIntervalsIntersect(lPolygon(i).right(), lPolygon(i+1).left(), tuple._1.left(), tuple._2.left())){

                      lPolygon.insertAll(i + 1, ListBuffer[Line](tuple._1, tuple._2))

                      lPolygon(i).polyCase = returnPolyCase(lPolygon(i).right(), lPolygon(i + 1).left(), 2, 0)
                      lPolygon(i + 1).polyCase = if (lPolygon(i + 1).right().y == lPolygon(i + 2).right().y) 16 else 1
                      lPolygon(i + 2).polyCase = if (lPolygon(i + 2).left().y == lPolygon(i + 3).left().y) 16 else 12

                      lPolygon(i + 1).moveDirection = MoveDirection.RIGHT
                      lPolygon(i + 2).moveDirection = MoveDirection.LEFT

                      tPolygon -= tuple; tupleAdded = true; break
                    }
                  }
                }
                case 1 => if(slicePos == 1) {
                  if (tuple._1.left().y != tuple._2.left().y) {
//                    if (tuple._1.left().y > lPolygon(i + 1).right().y
//                      && tuple._2.left().y < lPolygon(i).right().y) {
                    if(verticalIntervalsIntersect(lPolygon(i).right(), lPolygon(i+1).right(), tuple._1.left(), tuple._2.left())){

                      lPolygon.insertAll(i + 1, ListBuffer[Line](tuple._1, tuple._2))

                      lPolygon(i).polyCase = returnPolyCase(lPolygon(i).right(), lPolygon(i+1).left(), 2, 0)
                      lPolygon(i + 1).polyCase = if (lPolygon(i + 1).right().y == lPolygon(i + 2).right().y) 16 else 1
                      lPolygon(i + 2).polyCase = returnPolyCase(lPolygon(i + 2).left(), lPolygon(i + 3).right(), 15, 13)

                      lPolygon(i + 1).moveDirection = MoveDirection.RIGHT
                      lPolygon(i + 2).moveDirection = MoveDirection.LEFT

                      tPolygon -= tuple; tupleAdded = true; break
                    }
                  }
                }
                case 2 => if(slicePos == 0){
                  if(tuple._1.right().y != tuple._2.right().y){
//                    if(tuple._2.right().y < lPolygon(i+1).left().y
//                    && tuple._2.right().y >= lPolygon(i).right().y){
                    if(verticalIntervalsIntersect(tuple._1.right(), tuple._2.right(), lPolygon(i+1).left(), lPolygon(i).right())){

                      lPolygon.insertAll(i+1, ListBuffer[Line](tuple._2, tuple._1))

                      lPolygon(i).polyCase = if(lPolygon(i).right().y == lPolygon(i+1).right().y) 16 else 3
                      lPolygon(i+1).polyCase = if(lPolygon(i+1).left().y == lPolygon(i+2).left().y) 16 else 14
                      lPolygon(i+2).polyCase = returnPolyCase(lPolygon(i+2).right(), lPolygon(i+3).left(), 2, 0)

                      lPolygon(i+1).moveDirection = MoveDirection.LEFT
                      lPolygon(i+2).moveDirection = MoveDirection.RIGHT

                      tPolygon -= tuple; tupleAdded = true; break
                    }
                  }
                }
                case 3 => if(slicePos == 0){
                  if(tuple._1.right().y != tuple._2.right().y){
//                    if(tuple._2.right().y >= lPolygon(i).right().y
//                    && tuple._1.right().y <= lPolygon(i+1).right().y){
                    if(verticalIntervalsIntersect(tuple._1.right(), tuple._2.right(), lPolygon(i+1).right(), lPolygon(i).right())){

                      lPolygon.insertAll(i+1, ListBuffer[Line](tuple._2, tuple._1))

                      lPolygon(i).polyCase = if(lPolygon(i).right().y == lPolygon(i+1).right().y) 16 else 3
                      lPolygon(i+1).polyCase = if(lPolygon(i+1).left().y == lPolygon(i+2).left().y) 16 else 14
                      lPolygon(i+2).polyCase = if(lPolygon(i+2).right().y == lPolygon(i+3).right().y) 16 else 3

                      lPolygon(i+1).moveDirection = MoveDirection.LEFT
                      lPolygon(i+2).moveDirection = MoveDirection.RIGHT

                      tPolygon -= tuple; tupleAdded = true; break
                     }
                  }
                }
                case 12 => if(slicePos == 0){
                  if(tuple._1.left().y != tuple._2.left().y){
//                    if(tuple._1.left().y <= lPolygon(i).left().y
//                    && tuple._2.left().y >= lPolygon(i+1).left().y){
                    if(verticalIntervalsIntersect(lPolygon(i).left(), lPolygon(i+1).left(), tuple._1.left(), tuple._2.left())){

                      lPolygon.insertAll(i+1, ListBuffer[Line](tuple._1, tuple._2))

                      lPolygon(i).polyCase = if(lPolygon(i).left().y == lPolygon(i+1).left().y) 16 else 12
                      lPolygon(i+1).polyCase = if(lPolygon(i+1).right().y == lPolygon(i+2).right().y) 16 else 1
                      lPolygon(i+2).polyCase = if(lPolygon(i+2).left().y == lPolygon(i+3).left().y) 16 else 12

                      lPolygon(i+1).moveDirection = MoveDirection.RIGHT
                      lPolygon(i+2).moveDirection = MoveDirection.LEFT

                      tPolygon -= tuple; tupleAdded = true; break
                    }
                  }
                }
                case 13 => if(slicePos == 0){
                  if(tuple._1.left().y != tuple._2.left().y){
//                    if(tuple._1.left().y <= lPolygon(i).left().y
//                    && tuple._1.left().y > lPolygon(i+1).right().y){
                    if(verticalIntervalsIntersect(lPolygon(i).left(), lPolygon(i+1).right(), tuple._1.left(), tuple._2.left())){

                      lPolygon.insertAll(i+1, ListBuffer[Line](tuple._1, tuple._2))

                      lPolygon(i).polyCase = if(lPolygon(i).left().y == lPolygon(i+1).left().y) 16 else 12
                      lPolygon(i+1).polyCase = if (lPolygon(i+1).right().y == lPolygon(i+2).right().y) 16 else 1
                      lPolygon(i+2).polyCase = returnPolyCase(lPolygon(i+2).left(), lPolygon(i+3).right(), 15, 13)

                      lPolygon(i+1).moveDirection = MoveDirection.RIGHT
                      lPolygon(i+2).moveDirection = MoveDirection.LEFT

                      tPolygon -= tuple; tupleAdded = true; break
                    }
                  }
                }
                case 14 => if(slicePos == -1){
                  if(tuple._1.right().y != tuple._2.right().y){
//                    if(tuple._1.right().y > lPolygon(i).left().y
//                    && tuple._2.right().y < lPolygon(i+1).left().y){
                    if(verticalIntervalsIntersect(tuple._1.right(), tuple._2.right(), lPolygon(i+1).left(), lPolygon(i).left())){

                      lPolygon.insertAll(i+1, ListBuffer[Line](tuple._2, tuple._1))

                      lPolygon(i).polyCase = returnPolyCase(lPolygon(i).left(), lPolygon(i+1).right(), 15, 13)
                      lPolygon(i+1).polyCase = if(lPolygon(i+1).left().y == lPolygon(i+2).left().y) 16 else 14
                      lPolygon(i+2).polyCase = returnPolyCase(lPolygon(i+2).right(), lPolygon(i+3).left(), 2, 0)

                      lPolygon(i+1).moveDirection = MoveDirection.LEFT
                      lPolygon(i+2).moveDirection = MoveDirection.RIGHT

                      tPolygon -= tuple; tupleAdded = true; break
                    }
                  }
                }
                case 15 => if(slicePos == -1){
                  if(tuple._1.right().y != tuple._2.right().y){
//                    if(tuple._1.right().y <= lPolygon(i+1).right().y
//                    && tuple._1.right().y > lPolygon(i).left().y){
                    if(verticalIntervalsIntersect(tuple._1.right(), tuple._2.right(), lPolygon(i+1).right(), lPolygon(i).left())){

                      lPolygon.insertAll(i+1, ListBuffer[Line](tuple._2, tuple._1))

                      lPolygon(i).polyCase = returnPolyCase(lPolygon(i).left(), lPolygon(i+1).right(), 15, 13)
                      lPolygon(i+1).polyCase = if(lPolygon(i+1).left().y == lPolygon(i+2).left().y) 16 else 14
                      lPolygon(i+2).polyCase = if(lPolygon(i+2).right().y == lPolygon(i+3).right().y) 16 else 3

                      lPolygon(i+1).moveDirection = MoveDirection.LEFT
                      lPolygon(i+2).moveDirection = MoveDirection.RIGHT

                      tPolygon -= tuple; tupleAdded = true; break
                    }
                  }
                }
                case _ =>
              }
            }
          }
        }
      }

      if(tPolygon.nonEmpty) println("There are still some tuples in tPoly, that's bad")

      unitedLinePolygons += lPolygon
    }

    unitedLinePolygons.foreach{ linePoly =>
      val poly = new Polygon(greatPolygonIterator, -2)
      greatPolygonIterator += 1

      linePoly.foreach{line =>

        line.moveDirection match {
          case MoveDirection.RIGHT =>
            poly.points += line.left()
            poly.points += line.right()
          case MoveDirection.LEFT =>
            poly.points += line.right()
            poly.points += line.left()
          case _ => println("Wrong move direction at combining points from lines")
        }
      }

      unitedPolygons += poly
    }

    draw(unitedPolygons.asInstanceOf[ListBuffer[Drawable]])

    unitedPolygons
  }

  def returnPolyCase(first : Point, second : Point, secondUpper : Int, secondLower : Int) : Int = {
    if(second.y > first.y) secondUpper
    else if (second.y < first.y) secondLower
    else 16 // если они совпадают
  }

  def unitePolygonsFromSlices(slices : ListBuffer[ListBuffer[(Line, Line)]]) : ListBuffer[Polygon] = {

    val slicesUnion = groupPolygonSlices(slices)

    val unitedPolygons = buildPolygonsFromGroups(slicesUnion)

    unitedPolygons
  }

  //Отладочная функция, нужна для того чтобы проверить объединились ли объединенные полигоны
  def drawUnitedPolygons(slicesUnion : ListBuffer[Map [Int, ListBuffer[(Line, Line)]]] ): Unit ={
    val r = new Random()
    slicesUnion.foreach{poly =>
      val color = new Color(r.nextInt(255), r.nextInt(255), r.nextInt(255), 122)

      poly.foreach{slice =>
        slice._2.foreach{tuple =>
          val tuplePoly = new Polygon(0, -1)
          tuplePoly.createPolygonByTwoLines(tuple._1, tuple._2)

          val arrayX, arrayY = new Array[Int](tuplePoly.points.size)
          for(i <- tuplePoly.points.indices){
            arrayX(i) = tuplePoly.points(i).x.toInt
            arrayY(i) = tuplePoly.points(i).y.toInt
          }

          g.setColor(color)
          g.fillPolygon(arrayX, arrayY, tuplePoly.points.size)

          //рисую грань полигона чтобы показать что их много и они маленькие
          tuplePoly.draw(g)
        }

      }
    }
  }

  def verticalIntervalsIntersect(leftUp : Point, leftDown : Point, rightUp : Point, rightDown : Point) : Boolean = {
    verticalIntervalsIntersect(leftUp.y, leftDown.y, rightUp.y, rightDown.y)
  }

  def verticalIntervalsIntersect(leftUp : Double, leftDown : Double, rightUp : Double, rightDown : Double) : Boolean = {
    val up    : Double = Math.min(leftUp, rightUp)
    val down  : Double = Math.max(leftDown, rightDown)
    val mid   : Double = (up+down)/2.0

    mid < leftUp && mid > leftDown && mid < rightUp && mid > rightDown
  }

  //пересекаются ли пары линий
  def tupleIntersect(left : (Line, Line), right : (Line, Line)) : Boolean = {
    ( right._1.points(0).y >  left._1.points(1).y && right._2.points(0).y < left._1.points(1).y
      ||
      right._1.points(0).y <= left._1.points(1).y && right._1.points(0).y > left._2.points(1).y)
  }

  //Добавляем полигон из двух линий в массив полигонов
  def addPolygonFromTwoLinesToList(top: Line, bottom: Line, polygons: ListBuffer[Polygon]): Unit ={
    polygons += new Polygon(greatPolygonIterator, -1)
    greatPolygonIterator += 1
    polygons.last.createPolygonByTwoLines(top, bottom)
  }

  def main(args: Array[String]): Unit = {

    1 to 5 foreach {//TODO считывание размера из инишника
      _=>
        // clear background
        g.setColor(Color.WHITE)
        g.fillRect(-size._1 / 2, -size._2 / 2, size._1, size._2)

        // enable anti-aliased rendering (prettier lines and circles)
        g.setRenderingHint(KEY_ANTIALIASING, VALUE_ANTIALIAS_ON)


        val polygons = generateRandomPolygons(3, 3)//TODO сохранять выборки для последующей загрузки
      //val polygons = readPolygonDataFromFile("poly.txt")
        val linesFromPolygons = breakPolygonsToLines(polygons)
        val intersectionsOfLines = findPolygonsIntersections(polygons)
        val slices = generateSlices(linesFromPolygons, intersectionsOfLines)


        val timeStamp = System.nanoTime()
        var image : java.io.File = new File("")

        //Рисую полигоны
        //        g.setColor(Color.WHITE)                                     //TODO запилить функцию которая принимает функцию отрисовки на вход и делает все остальное
        //        g.fillRect(-size._1 / 2, -size._2 / 2, size._1, size._2)
        //        draw(polygons.asInstanceOf[ListBuffer[Drawable]])
        //        // write image to a file
        //        var image: java.io.File = new java.io.File(timeStamp + "00_Origin.png")
        //        image.createNewFile()
        //        javax.imageio.ImageIO.write(canvas, "png", image)

        //Рисую открывающие линии
        //        g.setColor(Color.WHITE)
        //        g.fillRect(-size._1 / 2, -size._2 / 2, size._1, size._2)
        //        drawOpenings(linesFromPolygons)
        //        // write image to a file
        //        image = new java.io.File(timeStamp + "01_Open.png")
        //        image.createNewFile()
        //        javax.imageio.ImageIO.write(canvas, "png", image)

        //Рисую полосы
        //        g.setColor(Color.WHITE)
        //        g.fillRect(-size._1 / 2, -size._2 / 2, size._1, size._2)
        //        g.setColor(Color.BLACK)
        //        drawSectors(intersectionsOfLines)
        //        drawSlices(slices)
        //        // write image to a file
        //        image = new java.io.File(timeStamp + "02_Slices.png")
        //        image.createNewFile()
        //        javax.imageio.ImageIO.write(canvas, "png", image)

        //Рисую AND
        g.setColor(Color.WHITE)
        g.fillRect(-size._1 / 2, -size._2 / 2, size._1, size._2)
        draw(polygons.asInstanceOf[ListBuffer[Drawable]])
        draw(boolOperation(slices, BoolOperation.AND, 1, 2).asInstanceOf[ListBuffer[Drawable]], transparent = true)
        // write image to a file
        image = new java.io.File(timeStamp + "03_AND.png")
        image.createNewFile()
        javax.imageio.ImageIO.write(canvas, "png", image)

        //Рисую OR
        g.setColor(Color.WHITE)
        g.fillRect(-size._1 / 2, -size._2 / 2, size._1, size._2)
        draw(polygons.asInstanceOf[ListBuffer[Drawable]])
        draw(boolOperation(slices, BoolOperation.OR, 1, 2).asInstanceOf[ListBuffer[Drawable]], transparent = true)
        // write image to a file
        image = new java.io.File(timeStamp + "04_OR.png")
        image.createNewFile()
        javax.imageio.ImageIO.write(canvas, "png", image)

        //Рисую XOR
        g.setColor(Color.WHITE)
        g.fillRect(-size._1 / 2, -size._2 / 2, size._1, size._2)
        draw(polygons.asInstanceOf[ListBuffer[Drawable]])
        draw(boolOperation(slices, BoolOperation.XOR, 1, 2).asInstanceOf[ListBuffer[Drawable]], transparent = true)
        // write image to a file
        image = new java.io.File(timeStamp + "05_XOR.png")
        image.createNewFile()
        javax.imageio.ImageIO.write(canvas, "png", image)

        //Рисую NOT
        g.setColor(Color.WHITE)
        g.fillRect(-size._1 / 2, -size._2 / 2, size._1, size._2)
        draw(polygons.asInstanceOf[ListBuffer[Drawable]])
        draw(boolOperation(slices, BoolOperation.NOT, 1, 2).asInstanceOf[ListBuffer[Drawable]], transparent = true)
        // write image to a file
        image = new java.io.File(timeStamp + "06_NOT.png")
        image.createNewFile()
        javax.imageio.ImageIO.write(canvas, "png", image)

    }
    // done with drawing
    g.dispose()
  }
}
