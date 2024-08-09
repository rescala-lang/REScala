package dtn

import rdts.time.Dots

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import routing.{BaseRouter, DirectRouter, EpidemicRouter, RdtRouter}
import rdt.Client

/*
  this file contains all jvm main methods
 */

@main def rdt_tool(args: String*): Unit = {
  if args.isEmpty || Set("-?", "-h", "--h", "help", "--help").contains(args(0)) || args.length % 2 != 0 then {
    println("""
commandline options:
  -m   => method (mandatory) | available options: monitoring, routing.direct, routing.epidemic, routing.rdt, client, client.once, client.addwins.listen, client.addwins.active, print.received, print.forwarded, print.statedev
  -a   => host address       | default: 0.0.0.0 (for monitoring), 127.0.0.1 (everything else)
  -p   => host port          | default: 5000 (for monitoring), 3000 (for everything else)
  -ma  => monitoring address | default: 127.0.0.1
  -mp  => monitoring port    | default: 5000
  -cid => creation client id | default: dtn://n2/rdt/testapp
    """)
  } else {
    var keyword_args: Map[String, String] = Map()
    args.sliding(2, 2).foreach(pair => {
      if !pair.head.startsWith("-") then
        throw Exception(s"could not parse commandline. ${pair.head} is not a key. (corresponding value: ${pair(1)}")
      keyword_args += (pair.head -> pair(1))
    })

    val method: String = keyword_args("-m")
    val host_address: String =
      keyword_args.getOrElse("-a", if method.equals("monitoring") then "0.0.0.0" else "127.0.0.1")
    val host_port: Int = keyword_args.getOrElse("-p", if method.equals("monitoring") then "5000" else "3000").toInt
    val monitoring_address: String = keyword_args.getOrElse("-ma", "127.0.0.1")
    val monitoring_port: Int       = keyword_args.getOrElse("-mp", "5000").toInt
    val creation_client_id: String = keyword_args.getOrElse("-mid", "dtn://n2/rdt/testapp")

    method match
      case "monitoring" => start_monitoring_server(host_address, host_port)
      case "routing.direct" =>
        _route_forever(DirectRouter(host_address, host_port, MonitoringClient(monitoring_address, monitoring_port)))
      case "routing.epidemic" =>
        _route_forever(EpidemicRouter(host_address, host_port, MonitoringClient(monitoring_address, monitoring_port)))
      case "routing.rdt" =>
        _route_forever(RdtRouter(host_address, host_port, MonitoringClient(monitoring_address, monitoring_port)))
      case "client" =>
        receiving_client(host_address, host_port, monitoring_address, monitoring_port)
      case "client.once" =>
        send_one_rdt_package(host_address, host_port, monitoring_address, monitoring_port)
      case "client.addwins.listen" =>
        addwins_case_study_listen(host_address, host_port, monitoring_address, monitoring_port)
      case "client.addwins.active" =>
        addwins_case_study_active(host_address, host_port, monitoring_address, monitoring_port)
      case "print.received" =>
        MonitoringBundlesReceivedPrinter().run()
      case "print.forwarded" =>
        MonitoringBundlesForwardedPrinter().run()
      case "print.statedev" =>
        MonitoringStateDevelopmentPrinter().run(creationClientId = creation_client_id)
      case s => throw Exception(s"could not partse commandline. unknown method: $s")
  }
}

// a bunch of main methods for testing

@main def start_printing_received(): Unit  = MonitoringBundlesReceivedPrinter().run()
@main def start_printing_forwarded(): Unit = MonitoringBundlesForwardedPrinter().run()
@main def start_printing_state_n2(): Unit  = MonitoringStateDevelopmentPrinter().run("n2")

@main def start_monitoring_server_default(): Unit = start_monitoring_server("0.0.0.0", 5000)

@main def start_rdtdots_routing(): Unit      = _route_forever(RdtRouter("127.0.0.1", 3000))
@main def start_rdtdots_routing_3000(): Unit = _route_forever(RdtRouter("127.0.0.1", 3000))
@main def start_rdtdots_routing_4000(): Unit = _route_forever(RdtRouter("127.0.0.1", 4000))

@main def start_epidemic_routing(): Unit = _route_forever(EpidemicRouter("127.0.0.1", 3000))

@main def start_direct_routing(): Unit = _route_forever(DirectRouter("127.0.0.1", 3000))

@main def send_ping_to_node4000_from_3000(): Unit = send_ping_to_node4000("127.0.0.1", 3000)

@main def send_one_rdt_package_from_3000(): Unit = send_one_rdt_package("127.0.0.1", 3000, "127.0.0.1", 5000)
@main def send_one_rdt_package_from_4000(): Unit = send_one_rdt_package("127.0.0.1", 4000, "127.0.0.1", 5000)

// all helper methods

def start_monitoring_server(interface_address: String, port: Int): Unit = {
  val monitoring_server = MonitoringServer(interface_address, port)
  monitoring_server.run()
}

def _route_forever(router: Future[BaseRouter]): Unit = {
  router.flatMap(router => {
    router.start_receiving()
  }).recover(_.printStackTrace())

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
    flush_receive().recover(_.printStackTrace())

    val bundle: Bundle = BundleCreation.createBundleUTF8(
      utf8_payload = "Ping",
      full_destination_uri = "dtn://node4000/incoming",
      full_source_uri = Endpoint.NONE_ENDPOINT.full_uri
    )

    client.sendBundle(bundle)
  }).recover(_.printStackTrace())

  while true do {
    Thread.sleep(200)
  }
}

def send_one_rdt_package(host: String, port: Int, monitoringHost: String, monitoringPort: Int): Unit = {
  val dots: Dots = DotsCreation.generate_pseudo_random_dots()

  Client(host, port, "testapp", MonitoringClient(monitoringHost, monitoringPort)).flatMap(client => {
    client.registerOnReceive((message_type: RdtMessageType, payload: Array[Byte], dots: Dots) => {
      println(s"received dots: $dots")
    })

    println("waiting 5 seconds")
    Thread.sleep(5000)
    println(s"sending dots: $dots")
    client.send(message_type = RdtMessageType.Payload, payload = Array(), dots = dots)
  }).recover(_.printStackTrace())

  while true do {
    Thread.sleep(200)
  }
}

def receiving_client(host: String, port: Int, monitoringHost: String, monitoringPort: Int): Unit = {
  Client(host, port, "testapp", MonitoringClient(monitoringHost, monitoringPort)).map(client => {
    client.registerOnReceive((message_type: RdtMessageType, payload: Array[Byte], dots: Dots) => {
      println(s"received dots: $dots")
    })
  }).recover(_.printStackTrace())

  while true do {
    Thread.sleep(200)
  }
}

def addwins_case_study_listen(host: String, port: Int, monitoringHost: String, monitoringPort: Int): Unit = {
  val rdt = AddWinsSetRDT()

  rdt.connect(host, port, MonitoringClient(monitoringHost, monitoringPort))
  rdt.caseStudyListen()
}

def addwins_case_study_active(host: String, port: Int, monitoringHost: String, monitoringPort: Int): Unit = {
  val rdt = AddWinsSetRDT()

  rdt.connect(host, port, MonitoringClient(monitoringHost, monitoringPort))
  rdt.caseStudyActive()
}
