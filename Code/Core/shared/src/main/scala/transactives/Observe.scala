package transactives

trait Observing[State[_]] extends Transactives[State]:
  /** Generic interface for observers that represent a function registered to trigger for every reevaluation of a reactive value.
    * Currently this interface is only used to allow a removal of registered observer functions.
    *
    * @tparam S Struct type used for the propagation of the signal
    */
  trait Observe {
    def remove()(implicit fac: Scheduler): Unit
  }

  object Observe {

    trait ObserveInteract extends Observation {
      def checkExceptionAndRemoval(): Boolean
    }

    def strong[T](
        dependency: ReSource,
        fireImmediately: Boolean
    )(fun: dependency.Value => ObserveInteract)(implicit ct: CreationTicket): Observe = {
      ct.create[Pulse[Nothing], Observe with Derived](Set(dependency), Pulse.NoChange, fireImmediately) { state =>
        class Obs
            extends Base[Pulse[Nothing]](state, ct.rename)
            with Derived
            with Observe
            with DisconnectableImpl {

          override protected[transactives] def commit(base: Obs.this.Value): Obs.this.Value = Pulse.NoChange

          override protected[transactives] def reevaluate(dt: ReIn): Rout =
            guardReevaluate(dt) {
              val v  = dt.collectStatic(dependency)
              val oi = fun(v)
              if (oi.checkExceptionAndRemoval()) dt.trackDependencies(Set.empty)
              else dt.withEffect(oi)
            }
          override def remove()(implicit fac: Scheduler): Unit = disconnect()
        }
        new Obs
      }
    }

  }
