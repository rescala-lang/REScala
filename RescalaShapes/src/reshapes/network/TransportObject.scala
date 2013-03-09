package reshapes.network
import java.net.InetAddress
import reshapes.figures.Shape

/**
 * This object is used to send information from client to server.
 * The senderPort is the port the client listens to updates.
 */
@serializable
class TransportObject(val shapes: List[Shape], val senderPort: Int)