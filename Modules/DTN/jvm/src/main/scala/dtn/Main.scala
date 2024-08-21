package dtn

import rdts.time.Dots

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import routing.{BaseRouter, FloodingRouter, DirectRouter, EpidemicRouter, RdtRouter, RdtRouter2, SprayAndWaitRouter}
import rdt.Client

/*
  this file contains all jvm main methods
 */

@main def rdt_tool(args: String*): Unit = {
  if args.isEmpty || Set("-?", "-h", "--h", "help", "--help").contains(args(0)) || args.length % 2 != 0 then {
    println("""
commandline options:
  -m   => method (mandatory)                   | available options: monitoring, routing, client, print.received, print.forwarded, print.statedev
  -a   => host address                         | default: 0.0.0.0 (for monitoring), 127.0.0.1 (everything else)
  -p   => host port                            | default: 5000 (for monitoring), 3000 (for everything else)
  -rs  => routing strategy                     | default: epidemic (options: direct, flooding, epidemic, rdt, rdt2, spray, binary)
  -cr  => client rdt selection                 | default: addwins.listen (options: addwins.listen, addwins.active, observeremove.listen, observeremove.active, lastwriterwins.listen, lastwriterwins.active)
  -ma  => monitoring address                   | default: 127.0.0.1
  -mp  => monitoring port                      | default: 5000
  -mid => monitoring creation client id        | default: dtn://n2/rdt/testapp
  -awa => add-wins rdt number of additions     | default: 1000
  -awt => add-wins rdt sleep time milliseconds | default: 500
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
    val monitoring_address: String                 = keyword_args.getOrElse("-ma", "127.0.0.1")
    val monitoring_port: Int                       = keyword_args.getOrElse("-mp", "5000").toInt
    val creation_client_id: String                 = keyword_args.getOrElse("-mid", "dtn://n2/rdt/testapp")
    val add_wins_rdt_number_of_additions: Int      = keyword_args.getOrElse("-awa", "1000").toInt
    val add_wins_rdt_sleep_time_milliseconds: Long = keyword_args.getOrElse("-awt", "500").toLong
    val routing_strategy: String                   = keyword_args.getOrElse("-rs", "epidemic")
    val client_rdt: String                         = keyword_args.getOrElse("-cr", "addwins.listen")

    method match
      case "monitoring" => start_monitoring_server(host_address, host_port)
      case "routing" => {
        _route_forever(
          routing_strategy match
            case "direct" =>
              DirectRouter(host_address, host_port, MonitoringClient(monitoring_address, monitoring_port))
            case "flooding" =>
              FloodingRouter(host_address, host_port, MonitoringClient(monitoring_address, monitoring_port))
            case "epidemic" =>
              EpidemicRouter(host_address, host_port, MonitoringClient(monitoring_address, monitoring_port))
            case "rdt" =>
              RdtRouter(host_address, host_port, MonitoringClient(monitoring_address, monitoring_port))
            case "rdt2" =>
              RdtRouter2(host_address, host_port, MonitoringClient(monitoring_address, monitoring_port))
            case "spray" =>
              SprayAndWaitRouter(host_address, host_port, MonitoringClient(monitoring_address, monitoring_port))
            case "binary" =>
              throw Exception("binary spray and wait not implemented yet")
            case s =>
              throw Exception(s"unknown routing strategy (-rs): ${s}")
        )
      }
      case "client" => {
        client_rdt match
          case "addwins.listen" =>
            case_study_listen(
              host_address,
              host_port,
              MonitoringClient(monitoring_address, monitoring_port),
              AddWinsSetRDT(add_wins_rdt_number_of_additions, add_wins_rdt_sleep_time_milliseconds)
            )
          case "addwins.active" =>
            case_study_active(
              host_address,
              host_port,
              MonitoringClient(monitoring_address, monitoring_port),
              AddWinsSetRDT(add_wins_rdt_number_of_additions, add_wins_rdt_sleep_time_milliseconds)
            )
          case "observeremove.listen"  => throw Exception("observeremove.listen not implemented yet")
          case "observeremove.active"  => throw Exception("observeremove.active not implemented yet")
          case "lastwriterwins.listen" => throw Exception("lastwriterwins.listen not implemented yet")
          case "lastwriterwins.active" => throw Exception("lastwriterwins.active not implemented yet")
          case s                       => throw Exception(s"unknown client rdt: $s")
      }
      case "print.received" =>
        MonitoringBundlesReceivedPrinter().run()
      case "print.forwarded" =>
        MonitoringBundlesForwardedPrinter().run()
      case "print.statedev" =>
        MonitoringStateDevelopmentPrinter(creationClientId = creation_client_id).run()
      case s => throw Exception(s"could not partse commandline. unknown method: $s")
  }
}

// a bunch of main methods for testing

@main def start_printing_received(): Unit =
  MonitoringBundlesReceivedPrinter(
    MonitoringPaths("/home/kali/REScala/Modules/DTN/simulation/shared/monitoring")
  ).run()
@main def start_printing_forwarded(): Unit =
  MonitoringBundlesForwardedPrinter(
    MonitoringPaths("/home/kali/REScala/Modules/DTN/simulation/shared/monitoring")
  ).run()
@main def start_printing_state_n2(): Unit =
  MonitoringStateDevelopmentPrinter(
    creationClientId = "dtn://n2/rdt/app1",
    paths = MonitoringPaths("/home/kali/REScala/Modules/DTN/simulation/shared/monitoring")
  ).run()
@main def run_ratio_converter(): Unit =
  MonitoringStateDevelopmentToRatioConverter(
    creationClientId = "dtn://n2/rdt/app1",
    paths = MonitoringPaths("/home/kali/REScala/Modules/DTN/simulation/shared/monitoring")
  ).run()

@main def start_monitoring_server_default(): Unit = start_monitoring_server("0.0.0.0", 5000)

@main def start_rdtdots_routing(): Unit      = _route_forever(RdtRouter("127.0.0.1", 3000))
@main def start_rdtdots_routing_3000(): Unit = _route_forever(RdtRouter("127.0.0.1", 3000))
@main def start_rdtdots_routing_4000(): Unit = _route_forever(RdtRouter("127.0.0.1", 4000))

@main def start_epidemic_routing(): Unit = _route_forever(EpidemicRouter("127.0.0.1", 3000))

@main def start_direct_routing(): Unit = _route_forever(DirectRouter("127.0.0.1", 3000))

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
    Thread.sleep(1000)
  }
}

def case_study_listen(
    host: String,
    port: Int,
    monitoringClient: MonitoringClientInterface,
    rdt: CaseStudyRdt
): Unit = {
  rdt.connect(host, port, monitoringClient)
  rdt.caseStudyListen()
}

def case_study_active(
    host: String,
    port: Int,
    monitoringClient: MonitoringClientInterface,
    rdt: CaseStudyRdt
): Unit = {
  rdt.connect(host, port, monitoringClient)
  rdt.caseStudyActive()
}

@main def test(): Unit = {
  WSEndpointClient("127.0.0.1", 3000).map(client => {
    println(client.nodeId)
  })

  while true do {
    Thread.sleep(1000)
  }
}
