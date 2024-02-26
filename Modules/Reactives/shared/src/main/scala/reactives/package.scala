import reactives.operator.Interface

/** see [[reactives.default]] */
package object reactives {

  /** REScala has two main abstractions. [[reactives.default.Event]] and [[reactives.default.Signal]] commonly referred to as reactives.
    * Use [[reactives.default.Var]] to create signal sources and [[reactives.default.Evt]] to create event sources.
    *
    * Events and signals can be created from other reactives by using combinators,
    * signals additionally can be created using [[reactives.default.Signal]] expressions.
    */
  object default extends Interface.FromScheduler(Interface.default)
}
