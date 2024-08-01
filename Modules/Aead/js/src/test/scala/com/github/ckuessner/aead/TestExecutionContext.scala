package com.github.ckuessner.aead

import scala.concurrent.ExecutionContext

object TestExecutionContext {
  given executionContext: ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
}
