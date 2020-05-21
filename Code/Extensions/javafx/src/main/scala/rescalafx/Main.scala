package rescalafx

import scalafx.Includes.handle
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.{JFXApp, Platform}
import scalafx.scene.{Node, Scene}
import scalafx.scene.control.{Label, Slider}
import scalafx.scene.layout.VBox
import rescalafx.JFXAdapter._
import rescala.default._

object Main extends JFXApp {

  private val slider = new Slider(0, 30, 0)
  private val sliderValue = new Label()
  sliderValue.text <== (slider.value.toSignal.map(v => f"$v%.2f"): Signal[String]).toProperty



  def render(): Node = new VBox {
    children = Seq(
      slider,
      sliderValue
    )
  }

  stage = new PrimaryStage {

    width = 1000
    height = 600
    title = "Slider Demo"

    scene = new Scene {
      content = render()
    }

    onCloseRequest = handle {
      Platform.exit()
    }
  }
}
