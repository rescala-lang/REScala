package kofre.base

type Time = Long

object Time:
  def current(): Time = System.currentTimeMillis
