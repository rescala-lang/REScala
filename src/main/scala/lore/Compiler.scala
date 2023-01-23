package lore

import cats.data.EitherT
import cats.implicits._

import cats.effect._
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import com.monovore.decline._
import cats.implicits._
import cats.parse
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
          return IO.println(h).as(ExitCode.Error) // parsing subcommands error
      case Right(s) =>
        s
    val options = subcommand.options

    // read program
    val program: EitherT[IO, Throwable, String] =
      if options.file.isDefined
      then EitherT(readFile(options.file.get).attempt)
      else EitherT.rightT(options.inline.get)

    // parse program
    val ast: EitherT[IO, Throwable, NonEmptyList[Term]] =
      program.flatMap(p =>
        Parser.parse(p) match
          case Left(e) =>
            EitherT.leftT(Parser.ParsingException(e.show))
          case Right(e) => EitherT.rightT(e)
      )

    // transform program depending on subcommand
    val output =
      ast.flatMap(a =>
        EitherT.rightT(subcommand match
          case ToRescala(_) => toScala(a, options)
          case ToViper(_)   => toViper(a, options)
          case Parse(_)     => a.toString
        )
      )

    // write output to file or IO
    val result =
      output.flatMap(o =>
        options.toFile.match
          case None       => EitherT.right(IO.println(o))
          case Some(path) => EitherT(writeFile(path, o).attempt)
      )

    // check if anything went wrong: print error messages and set return code
    return result.value.flatMap(r =>
      r match
        case Left(e: NoSuchFileException) =>
          IO.println(s"Error! No such file: ${e.getFile()}").as(ExitCode.Error)
        case Left(e: ParsingException) =>
          IO.println(s"Parsing error!\n${e.getMessage()}").as(ExitCode.Error)
        case Left(e: Throwable) =>
          IO.println(s"Unknown error!") >> IO(e.printStackTrace())
            .as(ExitCode.Error)
        case Right(io) => IO(io).as(ExitCode.Success)
    )

    // for
    //   p <- program
    //   ast = program.flatMap(p =>
    //     Parser.parse(p) match
    //       case Left(e)  => Left(Exception(s"Parsing error!\n${e.show}"))
    //       case Right(e) => Right(e)
    //   )
    // yield ast

    // for
    //   // read program
    //   program <-
    //     if options.file.isDefined
    //     then readFile(options.file.get)
    //     else IO.pure(options.inline.get)
    //   exitCode <-
    //     // parse program
    //     Parser.parse(program) match
    //       case Left(e) =>
    //         // print parse errors
    //         IO.println(s"Parsing error!\n${e.show}").as(ExitCode.Error)
    //       case Right(ast) =>
    //         // generate output
    //         val output = subcommand match
    //           case ToRescala(_) => toScala(ast, options)
    //           case ToViper(_)   => toViper(ast, options)
    //           case Parse(_)     => ast.toString
    //         // write or print output
    //         options.toFile.match
    //           case None       => IO.println(output)
    //           case Some(path) => writeFile(path, output)
    // yield exitCode

    //       (
    //         // run requested subroutines
    //         subcommand match
    //           case ToRescala(_) => toScala(ast, options)
    //           case ToViper(_)   => toViper(ast, options)
    //           case Parse(_)     => IO.println(ast)
    //       ).as(ExitCode.Success)
    // yield result
