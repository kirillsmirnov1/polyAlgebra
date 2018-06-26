import java.awt.Graphics2D
//Класс рисуемого объекта. Реализуется другими классами.
abstract class Drawable {
  //Визуализация
  def draw(g : Graphics2D, transparent : Boolean = false)
}
