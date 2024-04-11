package dtn

object ConnectionInfo {
  val ip: String = "127.0.0.1"
  val http_address: String = s"http://$ip"
  
  def http_api(port: Int): String = s"$http_address:$port"
  
  def ws_url(port: Int): String = s"ws://$ip:$port/ws"

  def external_routing_ws_url(port: Int): String = s"ws://$ip:$port/ws/erouting"
}
