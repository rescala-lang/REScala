package benchmarks.encrdt.todolist

import java.util.UUID
import scala.collection.mutable
import scala.util.Random

class ToDoListInteractionGenerator(pruningCompletedToDoThreshold: Int = 50, numKeptCompletedToDos: Int = 20) {
  def generateInteractions(numberInteractions: Int): Iterable[ToDoListInteraction] = {
    val random = new Random(42)
    val state  = new GeneratorState(random, 70, 140)

    0 until numberInteractions map (_ => {
      if state.numberOfCurrentlyCompletedEntries >= pruningCompletedToDoThreshold then { // Pruning
        RemoveToDoItems(state.pruneTodos(state.numberOfCurrentlyCompletedEntries - numKeptCompletedToDos))
      } else { // Normal Interactions
        var chosenInteraction: Option[ToDoListInteraction] = None
        while chosenInteraction.isEmpty do {
          val randomInteraction = random.nextInt(2)
          if randomInteraction == 0 then { // 0 => Add random to do entry
            chosenInteraction = Some(state.addRandomEntry())
          } else if randomInteraction == 1 then {              // 1 => Complete random (existing) to do entry
            chosenInteraction = state.completeRandomEntry() // Might return None, this means another iteration
          } else {
            ??? // This interaction does not *yet* exist and this statement is never reached
          }
        }
        chosenInteraction.get
      }
    })
  }
}

private class GeneratorState(random: Random, val textLengthMin: Int, val textLengthMax: Int) {
  val entryGenerator: ToDoEntryGenerator = new ToDoEntryGenerator(random)

  private val existingEntries: mutable.HashMap[UUID, ToDoEntry] = mutable.HashMap()

  def currentEntryCount: Int = existingEntries.size

  private val uncompletedEntries: mutable.ListBuffer[UUID] = mutable.ListBuffer()
  private val completedEntries: mutable.Queue[UUID]        = mutable.Queue()
  def numberOfCurrentlyCompletedEntries: Int               = completedEntries.length

  def completeRandomEntry(): Option[CompleteToDoItem] = {
    if uncompletedEntries.isEmpty then return None
    val completedEntryIndex = random.nextInt(uncompletedEntries.length)
    val completedEntryUuid  = uncompletedEntries.remove(completedEntryIndex)
    completedEntries.enqueue(completedEntryUuid)
    existingEntries.updateWith(completedEntryUuid) {
      case Some(todo @ ToDoEntry(_, false, _)) => Some(todo.copy(completed = true))
      case _                                   => throw new IllegalStateException()
    }
    Some(CompleteToDoItem(completedEntryUuid))
  }

  def addRandomEntry(): AddToDoItem = {
    val entryUuid = new UUID(random.nextLong(), random.nextLong())
    val entry     = entryGenerator.nextTodoEntry(textLengthMin, textLengthMax + 1)
    uncompletedEntries.addOne(entryUuid)
    assert(!existingEntries.contains(entryUuid))
    existingEntries.put(entryUuid, entry)
    AddToDoItem(entryUuid, entry)
  }

  def pruneTodos(numberOfPrunedToDos: Int): Seq[UUID] = {
    if numberOfPrunedToDos > completedEntries.size then {
      throw new IllegalArgumentException(
        s"Cannot prune $numberOfPrunedToDos todos, since only ${completedEntries.size} are completed"
      )
    } else {
      (1 to numberOfPrunedToDos).map(_ => completedEntries.removeHead(false))
    }
  }
}
