package loci

package object communicator {
  type ProtocolCommon = Protocol with SetupInfo with SecurityInfo with SymmetryInfo

  type Notice[-T] = loci.Notice[T]
  val Notice = loci.Notice

  type MessageBuffer = loci.MessageBuffer
  val MessageBuffer = loci.MessageBuffer
}
