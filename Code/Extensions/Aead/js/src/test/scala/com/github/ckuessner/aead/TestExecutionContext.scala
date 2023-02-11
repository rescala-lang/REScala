package com.github.ckuessner.aead

import scala.concurrent.ExecutionContext

object TestExecutionContext {
  implicit val executionContext: ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
}
