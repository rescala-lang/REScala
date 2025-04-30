package loreCompilerPlugin.annotation

import scala.annotation.StaticAnnotation

/** An annotation that is added to method definitions that contain a LoRe program.
  * These methods will be processed by the LoRe compiler plugin and verified through Dafny.
  */
class LoReProgram extends StaticAnnotation {}
