package lore

import cats.effect._
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import com.monovore.decline._
import cats.implicits._
import cats.data.NonEmptyList
import lore.AST.Term
import java.nio.file.NoSuchFileException
import lore.Parser.ParsingException
import lore.backends.ViperBackend
import lore.cli._

object Compiler extends IOApp {

  private def readFile(path: Path): IO[String] =
    IO.blocking(String(Files.readAllBytes(path), StandardCharsets.UTF_8))

  private def writeFile(path: Path, content: String): IO[Unit] = for {
    // create parent directories
    _ <- IO.blocking(Files.createDirectories(path.getParent)).attempt
    _ <- IO.blocking(
      Files.write(path, content.getBytes(StandardCharsets.UTF_8))
    )
  }
  yield ()

  def toScala(ast: NonEmptyList[AST.Term], options: Options): IO[Unit] = {
    ???
    // for
    //   result <- IO(ScalaBackend.toAmm(ast))
    //   _ <- options.toFile match
    //     case None       => IO.println(result)
    //     case Some(path) => writeFile(path, result)
    // yield result
  }

  def toViper(ast: NonEmptyList[AST.Term], options: Options): IO[Unit] = {
    ViperBackend.compileAsSingleFile(ast.toList)
    options.outputOptions match {
      case OutputOptions.SplitMode(outDir) =>
        ViperBackend
          .compileAsSeparateFiles(ast.toList)
          .map((filename, program) =>
            writeFile(Paths.get(outDir.toString, filename ++ ".vpr"), program)
          )
          .parSequence
          .as(())
      case _ =>
        val result = ViperBackend.compileAsSingleFile(ast.toList)
        options.toFile match {
          case None => IO.println(result)
          case Some(path) => writeFile(path, result)
        }
    }
  }

  def run(args: List[String]): IO[ExitCode] = {
    // parse arguments and combine requested actions
    val subcommand: Subcommand = mainCommand.parse(args) match {
      case h@Left(Help(errors, _, _, _)) =>
        if errors.isEmpty
        then
          return IO.println(h.value).as(ExitCode.Success) // --help flag given
        else
          return IO
            .println(h.value)
            .as(ExitCode.Error) // parsing subcommands error
      case Right(s) =>
        s
    }
    val options = subcommand.options

    val result = for {
      // read program
      program <-
        if options.file.isDefined
        then (readFile(options.file.get))
        else IO((options.inline.get))
      // parse program
      ast <- Parser.parse(program) match {
        case Left(e) =>
          IO.raiseError(Parser.ParsingException(e.show))
        case Right(a) => IO(a)
      }
      // perform requested subcommand
      result <-
        subcommand match {
          case ToREScala(_) => toScala(ast, options)
          case ToViper(_) => toViper(ast, options)
          case Parse(_) =>
            // we already parsed, simply produce output
            options.toFile.match {
              case None => IO.println(ast.toString)
              case Some(path) => writeFile(path, ast.toString)
            }
        }
    }
    yield result

    // check if anything went wrong: print error messages and set return code
    for {
      resultWithErrors <- result.attempt
      exitCode <- resultWithErrors match {
        case Left(e: NoSuchFileException) =>
          IO.println(
            fansi.Color
              .Red(s"Error! No such file: ${e.getFile()}")
              .overlay(fansi.Bold.On, 0, 6)
          ).as(ExitCode.Error)
        case Left(e: ParsingException) =>
          IO.println(
            fansi.Color
              .Red((s"Parsing error!\n${e.getMessage()}"))
              .overlay(fansi.Bold.On, 0, 14)
          ).as(ExitCode.Error)
        case Left(e: Throwable) =>
          IO.println(
            fansi.Color
              .Red(s"Unknown error!")
              .overlay(fansi.Bold.On)
          ) >> IO(e.printStackTrace())
            .as(ExitCode.Error)
        case Right(io) => IO(io).as(ExitCode.Success)
      }
    }
    yield exitCode
  }
}
