//package examples.demo
//
//import javax.swing.JOptionPane
//
//import examples.demo.LFullyModularBall.BouncingBall
//import examples.demo.MPlayingFieldBall.PlayingField
//import examples.demo.ORacketMultiBall.Racket
//import examples.demo.PSplitscreenRacketBall.Opponent
//import examples.demo.ui._
//import rescala._
//import rescala.parrp.ParRP
//import retier._
//import retier.architectures.ClientServer._
//import retier.rescalaTransmitter._
//import retier.serializable.upickle._
//import retier.tcp._
//
//object Transmittables {
//  import java.awt.{Color, Dimension}
//  import retier.transmission._
//
//  implicit val dimensionTransmittable = new PullBasedTransmittable[Dimension, (Int, Int), Dimension] {
//    def send(value: Dimension, remote: RemoteRef) = (value.width, value.height)
//    def receive(value: (Int, Int), remote: RemoteRef) = new Dimension(value._1, value._2)
//  }
//
//  implicit val colorTransmittable = new PullBasedTransmittable[Color, (Int, Int, Int, Int), Color] {
//    def send(value: Color, remote: RemoteRef) = (value.getRed, value.getGreen, value.getBlue, value.getAlpha)
//    def receive(value: (Int, Int, Int, Int), remote: RemoteRef) = new Color(value._1, value._2, value._3, value._4)
//  }
//
//  type Repr = (Signal[Int], Signal[Int], Signal[Int], Option[Signal[Int]])
//  implicit def shapeTransmittable[Inter](implicit transmittable: Transmittable[Repr, Inter, Repr]) = new PullBasedTransmittable[Shape, Inter, Shape] {
//    def send(value: Shape, remote: RemoteRef) = transmittable send (value match {
//      case shape: Circle => (shape.centerX, shape.centerY, shape.diameter, None)
//      case shape: Rectangle => (shape.centerX, shape.centerY, shape.hitboxWidth, Some(shape.hitboxHeight))
//    })
//    def receive(value: Inter, remote: RemoteRef) = (transmittable receive value) match {
//      case (centerX, centerY, diameter, None) => new Circle(centerX, centerY, diameter)
//      case (centerX, centerY, hitboxWidth, Some(hitboxHeight)) => new Rectangle(centerX, centerY, hitboxWidth, hitboxHeight)
//    }
//  }
//}
//import Transmittables._
//
//@multitier
//object QNetworkRacketBall {
//  trait Server extends ServerPeer[Client]
//  trait Client extends ClientPeer[Server]
//
//  val shapes = placed[Server] { implicit! => Var[List[Shape]](List.empty) }
//  val panelX = placed[Server].local { implicit! => new ShapesPanel(shapes) }
//  val panelSize = placed[Server] { implicit! => panelX.sigSize }
//
//  val playingField = placed[Server].local { implicit! => new PlayingField(panelX.width.map(_ - 25), panelX.height.map(_ - 25)) }
//  val racket = placed[Server].local { implicit! => new Racket(playingField.width, true, playingField.height, panelX.Mouse.y) }
//  placed[Server].local { implicit ! => shapes.transform(playingField.shape :: racket.shape :: _) }
//
//  val opponent = placed[Client].local { implicit! => new Opponent(panelSize.asLocal, shapes.asLocal) }
//  placed[Client].main { implicit! => opponent.main(Array()) }
//  val opponentInput = placed[Client] { implicit! => opponent.panel2.Mouse.y }
//
//  val racket2 = placed[Server].local { implicit! =>
//    new Racket(playingField.width, false, playingField.height, opponentInput.asLocal)
////    new Racket(playingField.width, false, playingField.height, Signal {
////      remote[Client].connected() match {
////        case opponent :: _ => (opponentInput from opponent).asLocal()
////        case _ => 0
////      }
////    })
//  }
//  placed[Server].local { implicit! => shapes.transform(racket2.shape :: _) }
//
//  placed[Server].local { implicit! =>
//    def makeBall(initVx: Double, initVy: Double) = {
//      val bouncingBall = new BouncingBall(initVx, initVy, Var(50), panelX.Mouse.middleButton.pressed)
//      shapes.transform(bouncingBall.shape :: _)
//
//      val fieldCollisions = playingField.colliders(bouncingBall.shape)
//      bouncingBall.horizontalBounceSources.transform(fieldCollisions.left :: fieldCollisions.right :: _)
//      bouncingBall.verticalBounceSources.transform(fieldCollisions.top :: fieldCollisions.bottom :: _)
//
//      val racketCollision = racket.collisionWith(bouncingBall.shape) || racket2.collisionWith(bouncingBall.shape)
//      bouncingBall.horizontalBounceSources.transform(racketCollision :: _)
//    }
//    makeBall(200d, 150d)
//    makeBall(-200d, 100d)
//
//    new Thread(new Runnable{
//      override def run(): Unit = new Main {
//        override val panel: ShapesPanel = QNetworkRacketBall.panelX
//      }.main(Array())
//    }).start()
//  }
//}
//
//object GameServer extends App {
//  retier.multitier setup new QNetworkRacketBall.Server {
//    def connect = listen[QNetworkRacketBall.Client] { TCP(1099) }
//  }
//}
//
//object GameClient extends App {
//  retier.multitier setup new QNetworkRacketBall.Client {
//    def connect = TCP(JOptionPane.showInputDialog(null, "Please enter host address.", "Connect", JOptionPane.QUESTION_MESSAGE, null, null, "localhost").toString, 1099)
//  }
//}
