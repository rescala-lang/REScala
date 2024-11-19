package probench.benchmark

import java.io.{BufferedWriter, FileOutputStream, OutputStreamWriter}
import java.nio.file.Path

class CSVWriter(
    private val separator: String,
    private val path: Path,
    private val fileName: String,
    private val header: Seq[String]
) {
  path.toFile.mkdirs()
  private val file   = path.resolve(s"$fileName.csv").toFile
  private val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)))

  writer.write(header.mkString(separator))
  writer.write("\n")

  def writeRow(data: String*): Unit = {
    writer.write(data.mkString(separator))
    writer.write("\n")
  }

  def close(): Unit = {
    writer.flush()
    println(s"Saving to ${path.toAbsolutePath}")
    writer.close()
  }

}
