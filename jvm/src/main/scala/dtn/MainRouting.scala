package dtn

import dtn.routers.{RdtDotsRouter, EpidemicRouter, DirectRouter}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/* 
  A collection of main methods that are used either in direct testing or simulation
*/


@main def start_rdtdots_routing(): Unit = _route_forever(RdtDotsRouter(3000))
@main def start_rdtdots_routing_3000(): Unit = _route_forever(RdtDotsRouter(3000))
@main def start_rdtdots_routing_4000(): Unit = _route_forever(RdtDotsRouter(4000))


@main def start_epidemic_routing(): Unit = _route_forever(EpidemicRouter(3000))


@main def start_direct_routing(): Unit = _route_forever(DirectRouter(3000))



def _route_forever(router: Future[BaseRouter]): Unit = {
  router.flatMap(router => {
    router.start_receiving()
  }).recover(throwable => println(throwable))

  while(true) {
    Thread.sleep(200)
  }
}


