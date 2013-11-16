package examples.miscellanea

import swing.{Panel, MainFrame, SimpleSwingApplication}
import java.awt.{Graphics2D, Dimension}
import java.awt.Point

object ImperativeVersion extends SimpleSwingApplication {
  /* Uncomment to enable logging: */
  //react.ReactiveEngine.log.enableAllLogging
  
  val Size = 50
  val Max_X = 600
  val Max_Y = 600
  
  val position = new Point(20,20)
  val speed = new Point(10,8)
  
  override def main(args: Array[String]){
    super.main(args)
    while (true) {      
	  tick
      Thread sleep 20
    }
  }
  
  def tick(){
    position.translate(speed.x, speed.y)
    if(position.x < 0 || position.x + Size > Max_X) speed.x = -speed.x
    if(position.y < 0 || position.y + Size > Max_Y) speed.y = -speed.y
    frame.repaint
  }
  
  // drawing code
  def top = frame  
  val frame = new MainFrame {
    contents = new Panel() {
      preferredSize = new Dimension(600, 600)
      override def paintComponent(g: Graphics2D) {
	    g.fillOval(position.x,position.y, Size, Size)
      }
    }    
  }
}