package reshapes.network
import java.net.InetAddress
import reshapes.figures.Drawable

@serializable
class TransportObject(val shapes: List[Drawable], val senderPort: Int) {

}