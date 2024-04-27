package benchmarks.encrdt

import benchmarks.encrdt.Codecs.*
import benchmarks.encrdt.mock.insecure.{AlternativeInsecureToDoListClient, AlternativeInsecureToDoListIntermediary}
import benchmarks.encrdt.mock.{DisseminationStats, IntermediarySizeInfo, SecureToDoListClient, ToDoListClient, ToDoListIntermediary}
import benchmarks.encrdt.todolist.{AddToDoItem, CompleteToDoItem, RemoveToDoItems, ToDoEntry, ToDoListInteraction, ToDoListInteractionGenerator}
import com.google.crypto.tink.Aead
import encrdtlib.container.DeltaAddWinsLastWriterWinsMap

import java.io.PrintWriter
import java.nio.file.{Files, Paths}
import java.util.UUID

object ToDoAppBenchmark extends App {

  val USE_ENCRYPTION = true

  val numInteractions  = 1_000_000
  val pruningThreshold = 50
  val keptToDos        = 20

  private val interactions: Iterable[ToDoListInteraction] =
    new ToDoListInteractionGenerator(pruningThreshold, keptToDos).generateInteractions(numInteractions)

  private val clientCrdt = new DeltaAddWinsLastWriterWinsMap[UUID, ToDoEntry]("client")

  private var intermediarySizeInfo: IntermediarySizeInfo = scala.compiletime.uninitialized
  private var aead: Aead                                 = scala.compiletime.uninitialized
  private var clientReplica: ToDoListClient              = scala.compiletime.uninitialized

  if (USE_ENCRYPTION) {
    aead = Helper.setupAead("AES128_GCM")
    val intermediaryReplica = new ToDoListIntermediary
    intermediarySizeInfo = intermediaryReplica
    clientReplica = new SecureToDoListClient("client", clientCrdt, aead, intermediaryReplica)
  } else {
    val intermediaryReplica = new AlternativeInsecureToDoListIntermediary("intermediary")
    intermediarySizeInfo = intermediaryReplica
    clientReplica = new AlternativeInsecureToDoListClient("client", clientCrdt, intermediaryReplica)
  }

  val csvFileF =
    if (USE_ENCRYPTION) Paths.get("./benchmarks/results/todoapp_benchmark.csv")
    else Paths.get("./benchmarks/results/todoapp_benchmark trusted intermediary.csv")
  Files.createDirectories(csvFileF.getParent)
  val csvFile = new PrintWriter(csvFileF.toFile)
  csvFile.println(
    "interactions,intermediarySize,encDeltaCausalitySize,encDeltaCiphertextSize,intermediaryStoredDeltas,completedToDos,uncompletedToDos,last100InteractionsNanoTime,last100InteractionsDisseminatedBytes,last100InteractionsAdditionDisseminatedBytes,last100InteractionsCompletionDisseminatedBytes,last100InteractionsRemovalBytes"
  )

  var lastDisseminationStats: DisseminationStats = DisseminationStats(0, 0, 0, 0)

  val startNanoTime: Long             = System.nanoTime()
  var lastCheckPointEndNanoTime: Long = startNanoTime

  var counter = 0
  interactions.foreach { interaction =>
    performInteraction(interaction)
    counter += 1

    if (counter % 100 == 0) {
      val checkPointStartNanoTime        = System.nanoTime()
      val nanoTimeForLast100Interactions = checkPointStartNanoTime - lastCheckPointEndNanoTime
      val last100DisseminationStatDiff   = clientReplica.disseminationStats - lastDisseminationStats
      lastDisseminationStats = clientReplica.disseminationStats

      val storedDeltasOnIntermediary = intermediarySizeInfo.numberStoredDeltas
      val causalitySize              = intermediarySizeInfo.encDeltaCausalityInfoSizeInBytes
      val rawDeltaSize               = intermediarySizeInfo.rawDeltasSizeInBytes

      val entries            = clientCrdt.values
      val completedEntries   = entries.filter(_._2.completed)
      val uncompletedEntries = entries.filterNot(_._2.completed)

      csvFile.println(
        s"$counter,${causalitySize + rawDeltaSize},$causalitySize,$rawDeltaSize,$storedDeltasOnIntermediary,${completedEntries.size},${uncompletedEntries.size},$nanoTimeForLast100Interactions,${last100DisseminationStatDiff.total},${last100DisseminationStatDiff.addition},${last100DisseminationStatDiff.completion},${last100DisseminationStatDiff.removal}"
      )

      if (counter % 1_000 == 0) {
        println(
          s"$counter/$numInteractions interactions completed / avg over last 100: ${(checkPointStartNanoTime - lastCheckPointEndNanoTime) / (1_000_000.0 * 100)}ms"
        )
      }

      lastCheckPointEndNanoTime = System.nanoTime()
    }
  }

  csvFile.close()

  println("Overall " + clientReplica.disseminationStats)

  def performInteraction(interaction: ToDoListInteraction): Unit = interaction match {
    case AddToDoItem(uuid, toDoEntry) =>
      clientReplica.addToDoItem(uuid, toDoEntry)

    case CompleteToDoItem(uuid) =>
      clientReplica.completeToDoItem(uuid)

    case RemoveToDoItems(uuids) =>
      clientReplica.removeToDoItems(uuids)
  }
}
