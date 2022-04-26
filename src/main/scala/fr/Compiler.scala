package fr

import cats.effect._
import Parser.parse
import AST._
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import com.monovore.decline._
import cats.implicits._

object Compiler extends IOApp:

  def readFile(path: Path): IO[String] =
    IO.blocking(String(Files.readAllBytes(path), StandardCharsets.UTF_8))

  def writeFile(path: Path, content: String): IO[Unit] =
    for
      // create parent directories
      _ <- IO.blocking(Files.createDirectories(path.getParent)).attempt
      _ <- IO.blocking(Files.write(path, content.getBytes(StandardCharsets.UTF_8)))
    yield ()

  def parse(prog: String): IO[Seq[AST.ParsedExpression]] =
    IO.blocking(Parser.parse(prog))
  
  // def interprete(ast: Seq[AST.ParsedExpression]): IO[Unit] = 
  //   IO.blocking(Interpreter.interprete(ast))
  
  def toScala(ast: Seq[AST.ParsedExpression], options: Options): IO[String]=
    for
      result <- IO(ScalaBackend.toAmm(ast))
      _ <- options.toFile match
        case None =>  IO.println(result)
        case Some(path) =>  writeFile(path, result)
    yield result

  def toViper(ast: Seq[AST.ParsedExpression], options: Options): IO[String]=
    for
      result <- IO(ViperBackend.toViper(ast))
      _ <- options.toFile match
        case None =>  IO.println(result)
        case Some(path) =>  writeFile(path, result)
    yield result

  def run(args: List[String]): IO[ExitCode] =
    // parse arguments and combine requested actions
    val run = for
      subcommand <- frCommand.parse(args)
      options = subcommand.options
      // read program
      program <- if options.file.isDefined
        then Right(readFile(options.file.get))
        else Right(IO.pure(options.inline.get))
      // parse program
      ast <- Right(program.flatMap(parse(_)))
      // run requested subroutines
      result <- Right(subcommand match
            case ToRescala(_) => ast.flatMap(toScala(_, options))
            case ToViper(_) => ast.flatMap(toViper(_, options))
            case Parse(_) => ast.flatMap(IO.println(_))
      )
    yield result
    // generate ExitCode
    run match
      // --help flag given
      case Left(help: Help) if help.errors.isEmpty =>
        IO.println(help).as(ExitCode.Success)
      // errors occured
      case Left(err) => IO.println(err).as(ExitCode.Error)
      // everything fine
      case Right(run: IO[_]) =>
        run.as(ExitCode.Success)