package channel

import channel.tcp.TCP
import channel.udp.UDP

import java.net.InetSocketAddress

class EchoServerTestTCP extends EchoCommunicationTest(
      TCP.listen("0", 54467, ChannelTestThreadPools.ec),
      TCP.connect("localhost", 54467, ChannelTestThreadPools.ec)
    )

class EchoServerTestUDP extends EchoCommunicationTest(
      UDP.sendreceive(InetSocketAddress("localhost", 54469), 54468, ChannelTestThreadPools.ec),
      UDP.sendreceive(InetSocketAddress("localhost", 54468), 54469, ChannelTestThreadPools.ec)
    )
