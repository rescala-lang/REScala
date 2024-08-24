package ex2023

import reactives.default.*
import reactives.structure.Diff

object MLTest {

  /** They use a signal for initialization, but only care about its first value ever,
    * which is equivalent to just a constant.
    * We just use constants because Scala has them.
    */
  def fby[A](init: A, signal: Signal[A]): Signal[A] =
    // The implementation is a bit contrived – the paper has fby as a built in that essentially shifts signal values one step into the past.
    // REScala generally separates concerns by only allowing to fold over events (separating “something happened” from “current values“, but the paper uses a “event stream” kinda way of thinking about things.
    // Anyway, this converts the signals into an event stream first, and then projects that onto the prior value, holding that for just one step, allowing us to access the past.
    signal.change.map: change =>
      change.from.getOrElse(init)
    .hold(init)

  type SD = Signal[Double]

  /** This is assumed to be a built in in the paper, that interacts with the learning process. */
  def param[A](init: A): Signal[A] = Signal(init)

  def dense(i: SD): SD =
    val b = param(0.0)
    val k = param(1.0)
    Signal {
      k.value * i.value + b.value
    }

  def multiply(i: SD) = i.map(v => v * v)
  def app(i: SD) =
    multiply(dense(i))

  def app_pipe(i: SD): SD =
    val x = dense(i)
    val y = fby(0.0, x)
    multiply(y)

  def mse_grad_i(i: SD, gt: SD): SD =
    Signal { 2 * (i.value - gt.value) }

  def diff_param(init: Double, bp: Signal[Boolean], `do`: SD) =
    `do`.change.fold(init): (acc: Double, next: Diff[Double]) =>
      acc + (if bp.value then next.from.get else 0.0)

  def diff_dense(i: => SD, bp: => Signal[Boolean], `do`: => SD) =
    // this indirection probably makes sense if you understand this in a differential equation context …
    def db = `do`
    val dk = Signal { i.value * `do`.value }
    val b  = diff_param(0.0, bp, db)
    val k  = diff_param(1.0, bp, dk)
    val di = Signal { k.value * `do`.value }
    val o  = Signal { k.value * i.value + b.value }
    (o, di)

  def diff_multiply(i: => SD, bp: => Signal[Boolean], `do`: => SD) =
    val o = Signal { i.value * i.value }
    // the original stops propagation when bp.value is false … we don’t actually have an operator for that (though, we could add one)
    val di = Signal { `do`.value * 2.0 * i.value }.changed.filter(_ => bp.value).hold()
    (o, di)

  // this build a cyclic dataflow … which should not loop indefinietly (maybe) but is not safe from REScalas point of view …
  // and most notably this immediately produces a recursion from Scalas point of view ending in a a non terminating execution :(
  def diff_app(i: SD, bp: Signal[Boolean], gt: SD, learn_rate: SD) =
    lazy val (
      (x: SD, di: SD),
      (o: SD, dx: SD),
      (d_o: SD)
    ) =
      (
        diff_dense(i, bp, dx),
        diff_multiply(x, bp, d_o),
        Signal { -learn_rate.value * mse_grad_i(o, gt).value }
      )
    o

  def main(args: Array[String]): Unit = {
    val input              = Signal(10.0)
    val backwardsPropagate = Signal(true)
    val groundTruth        = Signal { math.pow(2 * input.value - 3.0, 2) }
    val learn_rate         = Signal { 0.4 }
    val res                = diff_app(input, backwardsPropagate, groundTruth, learn_rate)
    println(res.now)
  }
}
