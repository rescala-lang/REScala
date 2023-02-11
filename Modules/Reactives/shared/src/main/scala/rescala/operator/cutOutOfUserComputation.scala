package rescala.operator

import scala.annotation.StaticAnnotation

/** Annotated definitions are cut out of any reactive macro and only evaluated once when the reactive is created.
  * This causes more dependencies to be static and reduces the number of unnecessarily created reactives.
  */
class cutOutOfUserComputation extends StaticAnnotation
