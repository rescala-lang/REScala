package rescala.commons

import scala.collection.mutable.ListBuffer

class Observable[T](var init: T){
    var handlers = new ListBuffer[(T=>Unit)]
    def addObserver(handler: (T=>Unit)) = handlers += handler
    def set(v: T) = { init = v; handlers foreach { _(init) } }
    def get(): T = init
}
object Observable{
    def apply[T](v: T) = new Observable[T](v)
}

/*
class waitAllNotifications[T](toWait: HashSet[Any])(init: T) extends Observable[T](init: T) {
    val alreadyReceived = new Queue[HashSet[Any]]

    def set(caller: Any) = if (alreadyReceived.front.contains(caller))
                                alreadyReceived.map(f)
                           else
                                 ???
}
*/
