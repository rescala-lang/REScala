package dtn

import scala.concurrent.Future
import scala.util.{Failure, Success}

import scala.concurrent.ExecutionContext.Implicits.global

extension (fut: Future[Unit])
  def printError: Unit = {
    fut.onComplete:
      case Failure(exception) =>
        exception.printStackTrace
      case Success(value) => ()
  }
