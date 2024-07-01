package dtn

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

extension (fut: Future[Unit])
  def printError(): Unit = {
    fut.onComplete:
      case Failure(exception) =>
        exception.printStackTrace()
      case Success(value) => ()
  }
