package loci

trait NoLogging {
  logging.root.clearHandlers().clearModifiers().replace()
}
