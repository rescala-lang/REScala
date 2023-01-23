package lore

import cats.effect._
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import com.monovore.decline._
import cats.implicits._
import cats.data.NonEmptyList
import lore.AST.Term
import java.nio.file.NoSuchFileException
import lore.Parser.ParsingException

object Compiler extends IOApp:

  def readFile(path: Path): IO[String] =
    IO.blocking(String(Files.readAllBytes(path), StandardCharsets.UTF_8))

  def writeFile(path: Path, content: String): IO[Unit] =
    for
      // create parent directories
      _ <- IO.blocking(Files.createDirectories(path.getParent)).attempt
      _ <- IO.blocking(
        Files.write(path, content.getBytes(StandardCharsets.UTF_8))
      )
    yield ()

  // def interprete(ast: Seq[AST.ParsedExpression]): IO[Unit] =
  //   IO.blocking(Interpreter.interprete(ast))

  def toScala(ast: NonEmptyList[AST.Term], options: Options): String =
    ???
    // for
    //   result <- IO(ScalaBackend.toAmm(ast))
    //   _ <- options.toFile match
    //     case None       => IO.println(result)
    //     case Some(path) => writeFile(path, result)
    // yield result

  def toViper(ast: NonEmptyList[AST.Term], options: Options): String =
    ???
    // result = ViperBackend.toViper(ast)
    // for _ <- options.toFile match
    //     case None       => IO.println(result)
    //     case Some(path) => writeFile(path, result)
    // yield result

  def run(args: List[String]): IO[ExitCode] =
    // parse arguments and combine requested actions
    val subcommand = mainCommand.parse(args) match
      case h @ Left(Help(errors, _, _, _)) =>
        if errors.isEmpty
        then
          return IO.println(h.value).as(ExitCode.Success) // --help flag given
        else
          return IO
            .println(h.value)
            .as(ExitCode.Error) // parsing subcommands error
      case Right(s) =>
        s
    val options = subcommand.options

    val result = for
      // read program
      program <-
        if options.file.isDefined
        then (readFile(options.file.get))
        else IO((options.inline.get))
      // parse program
      ast <- Parser.parse(program) match
        case Left(e) =>
          IO.raiseError(Parser.ParsingException(e.show))
        case Right(a) => IO(a)
      // transform program depending on subcommand
      output = subcommand match
        case ToRescala(_) => toScala(ast, options)
        case ToViper(_)   => toViper(ast, options)
        case Parse(_)     => ast.toString
      // write output to file or IO
      result <-
        options.toFile.match
          case None       => IO.println(output)
          case Some(path) => writeFile(path, output)
    yield result

    // check if anything went wrong: print error messages and set return code
    for
      resultWithErrors <- result.attempt
      exitCode <- resultWithErrors match
        case Left(e: NoSuchFileException) =>
          IO.println(s"Error! No such file: ${e.getFile()}").as(ExitCode.Error)
        case Left(e: ParsingException) =>
          IO.println(s"Parsing error!\n${e.getMessage()}").as(ExitCode.Error)
        case Left(e: Throwable) =>
          IO.println(s"Unknown error!") >> IO(e.printStackTrace())
            .as(ExitCode.Error)
        case Right(io) => IO(io).as(ExitCode.Success)
    yield exitCode
