package replication.fbdc

import rdts.base.Bottom
import rdts.datatypes.LastWriterWins

import java.nio.file.{Files, Path}
import java.sql.*
import java.util.Properties
import scala.collection.mutable.ListBuffer
import scala.util.chaining.scalaUtilChainingOps


object Northwind {

  val dbProperties = new Properties().tap(_.setProperty("immutable", "1"))

  def enableConditional(exampleData: FbdcExampleData, path: Path) = {
    if !Files.isRegularFile(path)
    then println(s"northwind $path not found")
    else
      println(s"opening northwind database")
      val connection = DriverManager.getConnection(s"jdbc:sqlite:${path.toString}", dbProperties)

      def query(q: String): List[Map[String, String]] =
        val st = connection.createStatement()
        try
          val res     = st.executeQuery(q)
          val meta    = res.getMetaData
          val columns = (1 to meta.getColumnCount).map(meta.getColumnName)
          val lb      = ListBuffer.empty[Map[String, String]]
          while res.next()
          do
            lb.append(columns.map(c => c -> res.getString(c)).toMap)
          lb.toList.take(10) // limit result size for serialization
        catch
          case e: Exception => List(Map("error" -> e.toString))
        finally
          st.close()

      import exampleData.dataManager
      import dataManager.given
      exampleData.addCapability("northwind")

      exampleData.requestsOf[Req.Northwind].observe { queries =>
        dataManager.modRes { res =>
          val ress = res.mutable
          queries.foreach { q =>
            val resp = Res.Northwind(q.value, query(q.value.query))
            ress.observeRemoveMap.update("northwind", Some(LastWriterWins.now(resp)))
          }
          ress.result
        }
      }

  }
}
