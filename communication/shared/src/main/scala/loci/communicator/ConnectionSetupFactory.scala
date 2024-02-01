package loci
package communicator

object ConnectionSetupFactory {
  type Properties = Map[String, List[String]]

  trait Implementation[+P <: ProtocolCommon] extends ConnectionSetupFactory[P] {
    type Properties

    val self: Implementation[_]

    final def listener(url: String, props: ConnectionSetupFactory.Properties) =
      setup(url, props, listener)

    final def connector(url: String, props: ConnectionSetupFactory.Properties) =
      setup(url, props, connector)

    private def setup[S <: ConnectionSetup[_]](
        url: String, props: ConnectionSetupFactory.Properties,
        setup: (String, String, String, self.Properties) => Option[S]): Option[S] =
      (schemes
        collectFirst (Function unlift { scheme =>
          val prefix = scheme + "://"
          val prefixLength = prefix.length
          if ((url.substring(0, prefixLength) compareToIgnoreCase prefix) == 0)
            Some((scheme, url.substring(prefixLength)))
          else
            None
        })
        flatMap { case (scheme, location) =>
          setup(url, scheme, location, properties(props))
        })

    protected def properties(
      implicit props: ConnectionSetupFactory.Properties): self.Properties

    protected def listener(
        url: String, scheme: String, location: String, properties: self.Properties):
      Option[Listener[P]]

    protected def connector(
        url: String, scheme: String, location: String, properties: self.Properties):
      Option[Connector[P]]
  }
}

sealed trait ConnectionSetupFactory[+P <: ProtocolCommon] { self =>
  val schemes: Seq[String]

  def listener(url: String, props: ConnectionSetupFactory.Properties): Option[Listener[P]]

  def connector(url: String, props: ConnectionSetupFactory.Properties): Option[Connector[P]]

  final def and[C >: P <: ProtocolCommon](other: ConnectionSetupFactory[C]): ConnectionSetupFactory[C] =
    new ConnectionSetupFactory[C] {
      val schemes = (self.schemes ++ other.schemes).distinct

      final def listener(url: String, props: ConnectionSetupFactory.Properties) =
        self.listener(url, props) orElse other.listener(url, props)

      final def connector(url: String, props: ConnectionSetupFactory.Properties) =
        self.connector(url, props) orElse other.connector(url, props)
    }
}
