package dtn

import dtn.routing.{BaseRouter, DirectRouter, EpidemicRouter, RdtRouter}
import rdts.time.Dots

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/*
  this file contains all jvm main methods
 */

@main def rdt_tool(args: String*): Unit = {
  if args.isEmpty || Set("-?", "-h", "--h", "help", "--help").contains(args(0)) || args.length % 2 != 0 then {
    println("""
commandline options:
  -m => method (mandatory) | available options: checker, routing.direct, routing.epidemic, routing.rdt, client.once, client.continuous
  -a => host address | default: 0.0.0.0 (for checker), 127.0.0.1 (everything else)
  -p => host port | default: 5000 (for checker), 3000 (for everything else)
  -coa => convergence checker address | default: 127.0.0.1
  -cop => convergence checker port | default: 5000
    """)
  } else {
    var keyword_args: Map[String, String] = Map()
    args.sliding(2, 2).foreach(pair => {
      if !pair.head.startsWith("-") then
        throw Exception(s"could not parse commandline. ${pair.head} is not a key. (corresponding value: ${pair(1)}")
      keyword_args += (pair.head -> pair(1))
    })

    val method: String       = keyword_args("-m")
    val host_address: String = keyword_args.getOrElse("-a", if method.equals("checker") then "0.0.0.0" else "127.0.0.1")
    val host_port: Int       = keyword_args.getOrElse("-p", if method.equals("checker") then "5000" else "3000").toInt
    val convergence_checker_address: String = keyword_args.getOrElse("-coa", "127.0.0.1")
    val convergence_checker_port: Int       = keyword_args.getOrElse("-cop", "5000").toInt

    method match
      case "checker"          => start_checker_server(host_address, host_port)
      case "routing.direct"   => _route_forever(DirectRouter(host_address, host_port))
      case "routing.epidemic" => _route_forever(EpidemicRouter(host_address, host_port))
      case "routing.rdt"      => _route_forever(RdtRouter(host_address, host_port))
      case "client.once" =>
        send_one_rdt_package(host_address, host_port, convergence_checker_address, convergence_checker_port)
      case "client.continuous" =>
        send_continuous_rdt_packages(host_address, host_port, convergence_checker_address, convergence_checker_port)
      case s => throw Exception(s"could not partse commandline. unknown method: $s")
  }
}

// a bunch of main methods for testing

@main def start_checker_server_default(): Unit = start_checker_server("0.0.0.0", 5000)

@main def start_rdtdots_routing(): Unit      = _route_forever(RdtRouter("127.0.0.1", 3000))
@main def start_rdtdots_routing_3000(): Unit = _route_forever(RdtRouter("127.0.0.1", 3000))
@main def start_rdtdots_routing_4000(): Unit = _route_forever(RdtRouter("127.0.0.1", 4000))

@main def start_epidemic_routing(): Unit = _route_forever(EpidemicRouter("127.0.0.1", 3000))

@main def start_direct_routing(): Unit = _route_forever(DirectRouter("127.0.0.1", 3000))

@main def send_ping_to_node4000_from_3000(): Unit = send_ping_to_node4000("127.0.0.1", 3000)

@main def send_one_rdt_package_from_3000(): Unit = send_one_rdt_package("127.0.0.1", 3000, "127.0.0.1", 5000)
@main def send_one_rdt_package_from_4000(): Unit = send_one_rdt_package("127.0.0.1", 4000, "127.0.0.1", 5000)

@main def send_continuous_rdt_packages_from_3000(): Unit =
  send_continuous_rdt_packages("127.0.0.1", 3000, "127.0.0.1", 5000)

// all helper methods

def start_checker_server(interface_address: String, port: Int): Unit = {
  val checker = DotsConvergenceChecker(interface_address, port)
  checker.run()
}

def _route_forever(router: Future[BaseRouter]): Unit = {
  router.flatMap(router => {
    router.start_receiving()
  }).recover(throwable => println(throwable))

  while true do {
    Thread.sleep(200)
  }
}

def send_ping_to_node4000(host: String, port: Int): Unit = {
  WSEndpointClient(host, port).flatMap(client => {
    // flush receive forever
    def flush_receive(): Future[Bundle] = {
      client.receiveBundle().flatMap(bundle => {
        println(s"received bundle: $bundle")
        flush_receive()
      })
    }
    flush_receive().recover(throwable => println(throwable))

    val bundle: Bundle = BundleCreation.createBundleUTF8(
      utf8_payload = "Ping",
      full_destination_uri = "dtn://node4000/incoming",
      full_source_uri = Endpoint.NONE_ENDPOINT.full_uri
    )

    client.sendBundle(bundle)
  }).recover(throwable => println(throwable))

  while true do {
    Thread.sleep(200)
  }
}

def send_one_rdt_package(host: String, port: Int, checkerHost: String, checkerPort: Int): Unit = {
  val dots: Dots    = DotsCreation.generate_pseudo_random_dots()
  val checkerClient = DotsConvergenceClient(checkerHost, checkerPort)

  RdtClient(host, port, "testapp", checkerClient).flatMap(client => {
    client.registerOnReceive((message_type: RdtMessageType, payload: Array[Byte], dots: Dots) => {
      println(s"received dots: $dots")
    })

    println(s"sending dots: $dots")
    client.send(message_type = RdtMessageType.Payload, payload = Array(), dots = dots)
  }).recover(throwable => println(throwable))

  while true do {
    Thread.sleep(200)
  }
}

def send_continuous_rdt_packages(host: String, port: Int, checkerHost: String, checkerPort: Int): Unit = {
  var dots: Dots    = Dots.empty
  val checkerClient = DotsConvergenceClient(checkerHost, checkerPort)

  RdtClient(host, port, "testapp", checkerClient).map(client => {
    client.registerOnReceive((message_type: RdtMessageType, payload: Array[Byte], d: Dots) => {
      dots = dots.merge(d)
      println(s"merged rdt-meta data, new dots: $dots")
    })

    while true do {
      Thread.sleep(5000)

      // add dots here

      println(s"sending new dots: $dots")
      client.send(RdtMessageType.Payload, Array(), dots).printError()
    }
  }).recover(throwable => println(throwable))

  while true do {
    Thread.sleep(200)
  }
}
