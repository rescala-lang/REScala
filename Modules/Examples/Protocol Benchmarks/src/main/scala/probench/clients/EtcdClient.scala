package probench.clients

import io.etcd.jetcd
import io.etcd.jetcd.ByteSequence
import probench.data
import probench.data.KVOperation
import rdts.base.Uid

class EtcdClient(val name: Uid, val endpoints: List[String]) extends Client(name) {

  private val etcdClient = jetcd.Client.builder().endpoints(endpoints*).build()
  private val kvClient   = etcdClient.getKVClient

  override def handleOpImpl(op: KVOperation[String, String]): Unit = {
    op match
      case data.KVOperation.Read(opKey) =>
        val key = ByteSequence.from(opKey.getBytes)
        val res = kvClient.get(key).get().getKvs.get(0)
        onResultValue(s"${res.getKey}=${res.getValue}")
      case data.KVOperation.Write(opKey, opValue) =>
        val key   = ByteSequence.from(opKey.getBytes)
        val value = ByteSequence.from(opValue.getBytes)
        kvClient.put(key, value).get()
        onResultValue(s"$opKey=$opValue; OK")
  }

}
