package rescalafx

import reactives.default.*
import scalafx.application.Platform
import scalafx.beans.property.{BooleanProperty, DoubleProperty, Property, StringProperty}

object JFXAdapter {

  implicit class PropertyToSignal[T, J](p: Property[T, J]) {

    def toSignal: Signal[T] = {
      val v = Var(p.value)
      p.onChange { (_, _, _) => v.set(p.value) }
      v
    }
  }

  implicit class SignalToStringProperty(s: Signal[String]) {
    def toProperty: StringProperty = {
      val p = StringProperty(s.now)
      s.observe({ v =>
        Platform.runLater(p.update(v))
      }, fireImmediately = false)
      p
    }
  }

  implicit class SignalToDoubleProperty(s: Signal[Double]) {
    def toProperty: DoubleProperty = {
      val p = DoubleProperty(s.now)
      s.observe({ v =>
        Platform.runLater(p.update(v))
      }, fireImmediately = false)
      p
    }
  }

  implicit class SignalToBooleanProperty(s: Signal[Boolean]) {
    def toProperty: BooleanProperty = {
      val p = BooleanProperty(s.now)
      s.observe({ v =>
        Platform.runLater(p.update(v))
      }, fireImmediately = false)
      p
    }
  }

}
