package lore

import cats.data.NonEmptyList
import cats.syntax.show.toShow
import com.monovore.decline.*
import lore.Parser.ParsingException
import lore.ast.Term
import lore.backends.ViperBackend
import lore.cli.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, NoSuchFileException, Path, Paths, StandardOpenOption}

object Compiler {

  private def writeFile(path: Path, content: String): Unit =
    Files.createDirectories(path.getParent)
    Files.writeString(path, content, StandardCharsets.UTF_8, StandardOpenOption.CREATE)
    ()

  def toScala(ast: NonEmptyList[Term], options: Options): Unit = {
    ???
    // for
    //   result <- IO(ScalaBackend.toAmm(ast))
    //   _ <- options.toFile match
    //     case None       => IO.println(result)
    //     case Some(path) => writeFile(path, result)
    // yield result
  }

  def toViper(ast: NonEmptyList[Term], options: Options): Unit = {
    ViperBackend.compileAsSingleFile(ast.toList)
    options.outputOptions match {
      case OutputOptions.SplitMode(outDir) =>
        ViperBackend
          .compileAsSeparateFiles(ast.toList)
          .foreach((filename, program) =>
            writeFile(Paths.get(outDir.toString, filename ++ ".vpr"), program)
          )
      case _ =>
        val result = ViperBackend.compileAsSingleFile(ast.toList)
        options.toFile match {
          case None       => println(result)
          case Some(path) => writeFile(path, result)
        }
    }
  }

  def main(args: Array[String]): Unit = {
    // parse arguments and combine requested actions
    val subcommand: Subcommand = mainCommand.parse(args.toList) match {
      case h @ Left(Help(errors, _, _, _)) =>
        if errors.isEmpty
        then
          return println(h.value) // --help flag given
        else
          return println(h.value)
      case Right(s) =>
        s
    }
    val options = subcommand.options

    def result() = {
      val program =
        if options.file.isDefined
        then Files.readString(options.file.get, StandardCharsets.UTF_8)
        else options.inline.get
        // parse program
      val ast = Parser.parse(program) match {
        case Left(e) =>
          throw Parser.ParsingException(e.show)
        case Right(a) => a
      }
      // perform requested subcommand
      subcommand match {
        case ToREScala(_) => toScala(ast, options)
        case ToViper(_)   => toViper(ast, options)
        case Parse(_)     =>
          // we already parsed, simply produce output
          options.toFile.match {
            case None       => println(ast.toString)
            case Some(path) => writeFile(path, ast.toString)
          }
      }
    }

    // check if anything went wrong: print error messages and set return code
    try result()
    catch
      case e: NoSuchFileException =>
        println(
          fansi.Color
            .Red(s"Error! No such file: ${e.getFile()}")
            .overlay(fansi.Bold.On, 0, 6)
        )
      case e: ParsingException =>
        println(
          fansi.Color
            .Red((s"Parsing error!\n${e.getMessage()}"))
            .overlay(fansi.Bold.On, 0, 14)
        )
      case e: Throwable =>
        println(
          fansi.Color
            .Red(s"Unknown error!")
            .overlay(fansi.Bold.On)
        )
        throw e
  }
}
