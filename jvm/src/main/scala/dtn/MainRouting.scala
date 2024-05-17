package dtn

import dtn.routing.{RdtDotsRouter, EpidemicRouter, DirectRouter}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import routing.BaseRouter

/* 
  A collection of main methods that are used either in direct testing or simulation
*/


@main def start_rdtdots_routing(): Unit = _route_forever(RdtDotsRouter("127.0.0.1", 3000))
@main def start_rdtdots_routing_3000(): Unit = _route_forever(RdtDotsRouter("127.0.0.1", 3000))
@main def start_rdtdots_routing_4000(): Unit = _route_forever(RdtDotsRouter("127.0.0.1", 4000))


@main def start_epidemic_routing(): Unit = _route_forever(EpidemicRouter("127.0.0.1", 3000))


@main def start_direct_routing(): Unit = _route_forever(DirectRouter("127.0.0.1", 3000))



def _route_forever(router: Future[BaseRouter]): Unit = {
  router.flatMap(router => {
    router.start_receiving()
  }).recover(throwable => println(throwable))

  while(true) {
    Thread.sleep(200)
  }
}


