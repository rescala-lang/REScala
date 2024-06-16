package rescalafx

import reactives.default.*
import scalafx.application.Platform
import scalafx.beans.property.{BooleanProperty, DoubleProperty, Property, StringProperty}

object JFXAdapter {

  extension [T, J](p: Property[T, J])
    def toSignal: Signal[T] = {
      val v = Var(p.value)
      p.onChange { (_, _, _) => v.set(p.value) }
      v
    }

  extension (s: Signal[String]) {
    def toProperty: StringProperty = {
      val p = StringProperty(s.now)
      s.observe(
        { v =>
          Platform.runLater(p.update(v))
        },
        fireImmediately = false
      )
      p
    }
  }

  extension (s: Signal[Double]) {
    def toProperty: DoubleProperty = {
      val p = DoubleProperty(s.now)
      s.observe(
        { v =>
          Platform.runLater(p.update(v))
        },
        fireImmediately = false
      )
      p
    }
  }

  extension (s: Signal[Boolean]) {
    def toProperty: BooleanProperty = {
      val p = BooleanProperty(s.now)
      s.observe(
        { v =>
          Platform.runLater(p.update(v))
        },
        fireImmediately = false
      )
      p
    }
  }

}
