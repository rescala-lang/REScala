package lore

import cats.effect._
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import com.monovore.decline._
import cats.implicits._
import cats.parse
import cats.data.NonEmptyList

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
    // for
    //   result <- IO(ViperBackend.toViper(ast))
    //   _ <- options.toFile match
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
          return IO.println(h).as(ExitCode.Error) // parsing subcommands error
      case Right(s) =>
        s
    val options = subcommand.options
    for
      // read program
      program <-
        if options.file.isDefined
        then readFile(options.file.get)
        else IO.pure(options.inline.get)
      // parse program
      ast = Parser.parse(program)
      result <- ast match
        // print parse errors
        case Left(e) =>
          IO.println(s"Parsing error!\n${e.show}").as(ExitCode.Error)
        case Right(ast) =>
          (
            // run requested subroutines
            subcommand match
              case ToRescala(_) => IO(toScala(ast, options))
              case ToViper(_)   => IO(toViper(ast, options))
              case Parse(_)     => IO.println(ast)
          ).as(ExitCode.Success)
    yield result
