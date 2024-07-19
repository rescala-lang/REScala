package lofi_acl.crypto

import com.google.crypto.tink.subtle.ChaCha20Poly1305
import rdts.time.Dot

import scala.annotation.tailrec

sealed trait KeyHierarchy {
  def encryptionKey(path: Array[String], dot: Dot): Option[ChaCha20Poly1305] =
    pathKey(path).map(kdk => ChaCha20Poly1305(kdk.encryptionKey(dot)))

  def pathKey(path: Array[String]): Option[KeyDerivationKey]

  def withKeys(other: KeyHierarchy): KeyHierarchy
}

class RootKey(private val rootKey: KeyDerivationKey) extends KeyHierarchy {
  override def pathKey(path: Array[String]): Option[KeyDerivationKey] =
    Some(rootKey.recursiveChildKeyDerivationKey(path))

  override def withKeys(other: KeyHierarchy): KeyHierarchy = this
}

class PartialKeyHierarchy(private val keys: KeyMap) extends KeyHierarchy {
  override def pathKey(path: Array[String]): Option[KeyDerivationKey] = {
    keys.lookup(path) match
      case Some(kdkPrefix -> kdk) =>
        val remainingPath = path.drop(kdkPrefix.length)
        Some(kdk.recursiveChildKeyDerivationKey(remainingPath))
      case None => None
  }

  override def withKeys(other: KeyHierarchy): KeyHierarchy =
    other match
      case other: PartialKeyHierarchy => PartialKeyHierarchy(keys.merge(other.keys))
      case root: RootKey              => root
}

private case class KeyMap(inner: Map[String, KeyMap | KeyDerivationKey]) {
  def lookup(path: Array[String]): Option[(Array[String], KeyDerivationKey)] = {
    lookupRec(path, 0)
  }

  @tailrec
  private def lookupRec(path: Array[String], index: Int): Option[(Array[String], KeyDerivationKey)] = {
    if index >= path.length then None
    else
      inner.get(path(index)) match
        case Some(keyMap: KeyMap)        => keyMap.lookupRec(path, index + 1)
        case Some(kdk: KeyDerivationKey) => Some(path.take(index + 1), kdk)
        case None                        => None
  }

  def merge(other: KeyMap): KeyMap = KeyMap(
    other.inner.foldLeft(inner) {
      case (current, (pathElement, l)) =>
        current.updatedWith(pathElement) { r =>
          (l, r) match {
            case (l, None)                          => Some(l)
            case (l: KeyDerivationKey, _)           => Some(l)
            case (_, r @ Some(_: KeyDerivationKey)) => r
            case (l: KeyMap, Some(r: KeyMap))       => Some(l.merge(r))
          }
        }
    }
  )
}
