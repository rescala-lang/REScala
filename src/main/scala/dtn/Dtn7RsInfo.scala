package dtn

object Dtn7RsInfo {
  val ip: String = "127.0.0.1"
  val port: String = "3000"
  val http_address: String = s"http://$ip"
  val http_api: String = s"$http_address:$port"
  val ws_url: String = s"ws://$ip:$port/ws"
}
