package rdts.time

type Time = Long

object Time:
  def current(): Time = System.currentTimeMillis
