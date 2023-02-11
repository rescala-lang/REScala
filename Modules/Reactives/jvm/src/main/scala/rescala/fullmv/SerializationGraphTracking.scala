package rescala.fullmv

import rescala.fullmv.sgt.synchronization.SubsumableLock

sealed trait SCCState {
  def unlockedIfLocked(): SCCConnectivity
  def known: SCCState
}
sealed trait SCCConnectivity extends SCCState {
  override def known: UnlockedSameSCC.type = UnlockedSameSCC
}
case object UnlockedUnknown extends SCCConnectivity {
  override def unlockedIfLocked(): this.type = this
}
case object UnlockedSameSCC extends SCCConnectivity {
  override def unlockedIfLocked(): this.type = this
}
case class LockedSameSCC(lock: SubsumableLock) extends SCCState {
  override def unlockedIfLocked(): UnlockedSameSCC.type = unlock()
  override def known: this.type                         = this
  def unlock(): UnlockedSameSCC.type = {
//    SerializationGraphTracking.released()
    lock.asyncUnlock()
    UnlockedSameSCC
  }
}

//
//trait LockContentionTimer {
//  var contendedAcquiredDuration: Long = 0L
//  var contendedAcquiredStatistics: Array[Int] = Array.fill(200)(0)
//  var heldDuration: Long = 0L
//  var heldStatistics: Array[Int] = Array.fill(200)(0)
//  var contendedAbortedDuration: AtomicLong = new AtomicLong(0)
//  var contendedAbortedStatistics: Array[AtomicInteger] = Array.fill(200)(new AtomicInteger(0))
//
//  def printStatistics(printWriter: PrintStream): Unit = {
//    printWriter.println("category/slot\tacquired\theld\taborted")
//    printWriter.println(s"sum\t$contendedAcquiredDuration\t$heldDuration\t${contendedAbortedDuration.get}")
//    printWriter.println(f"percentage\t${contendedAcquiredDuration/1e9}%.5f\t${heldDuration/1e9}%.5f\t${{contendedAbortedDuration.get}/1e9}%.5f")
//    printWriter.println(s"counts\t${contendedAcquiredStatistics.sum}\t${heldStatistics.sum}\t${contendedAbortedStatistics.map(_.get).sum}")
//    for(i <- contendedAcquiredStatistics.indices)
//      if(contendedAcquiredStatistics(i) != 0 || heldStatistics(i) != 0 || contendedAbortedStatistics(i).get != 0)
//        printWriter.println(s"${i*25}-${i*25+24}\t${contendedAcquiredStatistics(i)}\t${heldStatistics(i)}\t${contendedAbortedStatistics(i).get}")
//  }
//
//  def clear(): Unit = {
//    contendedAcquiredDuration = 0L
//    contendedAcquiredStatistics = Array.fill(contendedAcquiredStatistics.length)(0)
//    heldDuration = 0L
//    heldStatistics = Array.fill(heldStatistics.length)(0)
//    contendedAbortedDuration = new AtomicLong(0)
//    contendedAbortedStatistics = Array.fill(contendedAbortedStatistics.length)(new AtomicInteger(0))
//  }
//
//  val latestTimePerThread = new ThreadLocal[Long]
//
//  def startContending(): Unit = {
//    val now = System.nanoTime()
//    latestTimePerThread.set(now)
//  }
//  def abortContending(): Unit = {
//    val started = latestTimePerThread.get()
//    val now = System.nanoTime()
//    val duration = now - started
//    contendedAbortedDuration.addAndGet(duration)
//    val slot = (duration / 25).toInt
//    if(slot < contendedAbortedStatistics.length) contendedAbortedStatistics(slot).getAndIncrement()
//  }
//  def entered(): Unit = {
//    val started = latestTimePerThread.get()
//    val now = System.nanoTime()
//    val duration = now - started
//    contendedAcquiredDuration += duration
//    val slot = (duration / 25).toInt
//    if(slot < contendedAcquiredStatistics.length) contendedAcquiredStatistics(slot) += 1
//    latestTimePerThread.set(now)
//  }
//  def released(): Unit = {
//    val started = latestTimePerThread.get()
//    val now = System.nanoTime()
//    val duration = now - started
//    heldDuration += duration
//    val slot = (duration / 25).toInt
//    if(slot < heldStatistics.length) heldStatistics(slot) += 1
//  }
//}
