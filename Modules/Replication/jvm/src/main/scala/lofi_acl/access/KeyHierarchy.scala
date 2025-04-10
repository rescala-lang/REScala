package lofi_acl.access

import com.google.crypto.tink.subtle.ChaCha20Poly1305
import rdts.time.Dot

import scala.annotation.tailrec

sealed trait KeyHierarchy {
  def encryptionKey(path: Array[String], dot: Dot): Option[ChaCha20Poly1305] =
    pathKey(path).map(kdk => ChaCha20Poly1305(kdk.encryptionKey(dot)))

  def pathKey(path: Array[String]): Option[KeyDerivationKey]

  def withKeys(other: KeyHierarchy): KeyHierarchy

  // Removes parts that can't be encrypted due to lack of keys. (intentional)
  def encryptDelta(dot: Dot, isolatedDeltaParts: IsolatedDeltaParts): EncryptedDeltaParts

  // Removes parts that can't be decrypted due to lack of keys. (intentional)
  def decryptDelta(dot: Dot, encryptedDeltaParts: EncryptedDeltaParts): IsolatedDeltaParts
}

class FullKeyHierarchy(private val kdk: KeyDerivationKey) extends KeyHierarchy {
  override def pathKey(path: Array[String]): Option[KeyDerivationKey] =
    Some(kdk.recursiveChildKeyDerivationKey(path))

  override def withKeys(other: KeyHierarchy): KeyHierarchy = this

  override def encryptDelta(dot: Dot, isolatedDeltaParts: IsolatedDeltaParts): EncryptedDeltaParts =
    encryptDeltaRec(kdk, dot, isolatedDeltaParts)

  private def encryptDeltaRec(
      kdk: KeyDerivationKey,
      dot: Dot,
      isolatedDeltaParts: IsolatedDeltaParts
  ): EncryptedDeltaParts = {
    isolatedDeltaParts match
      case IsolatedDeltaParts(serializedValue: Array[Byte]) =>
        // We don't have any associated data that needs to be authenticated...
        // The path and dot (including identity) is already part of the derived key
        EncryptedDeltaParts(ChaCha20Poly1305(kdk.encryptionKey(dot)).encrypt(serializedValue, Array.empty))
      case IsolatedDeltaParts(children: Map[String, IsolatedDeltaParts]) =>
        EncryptedDeltaParts(
          children.map((pathElement, suffixParts) =>
            pathElement -> encryptDeltaRec(kdk.childKeyDerivationKey(pathElement), dot, suffixParts)
          ).filterNot(_._2.isEmpty) // Just in case there are paths that lead to nowhere in Map
        )
  }

  override def decryptDelta(dot: Dot, encryptedDeltaParts: EncryptedDeltaParts): IsolatedDeltaParts = {
    decryptDeltaRec(kdk, dot, encryptedDeltaParts)
  }

  private def decryptDeltaRec(
      kdk: KeyDerivationKey,
      dot: Dot,
      encryptedDeltaParts: EncryptedDeltaParts
  ): IsolatedDeltaParts = {
    encryptedDeltaParts match
      case EncryptedDeltaParts(ciphertext: Array[Byte]) =>
        IsolatedDeltaParts(ChaCha20Poly1305(kdk.encryptionKey(dot)).decrypt(ciphertext, Array.empty))
      case EncryptedDeltaParts(children: Map[String, EncryptedDeltaParts]) =>
        IsolatedDeltaParts(
          children.map((pathElement, suffixParts) =>
            pathElement -> decryptDeltaRec(kdk.childKeyDerivationKey(pathElement), dot, suffixParts)
          ).filterNot(_._2.isEmpty) // Just in case there are paths that lead to nowhere in Map
        )
  }
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
      case root: FullKeyHierarchy     => root

  override def encryptDelta(dot: Dot, isolatedDeltaParts: IsolatedDeltaParts): EncryptedDeltaParts =
    encryptDeltaRec(keys, dot, isolatedDeltaParts)

  private def encryptDeltaRec(
      keyMap: KeyMap,
      dot: Dot,
      isolatedDeltaParts: IsolatedDeltaParts
  ): EncryptedDeltaParts = {
    isolatedDeltaParts match
      case IsolatedDeltaParts(serializedValue: Array[Byte]) => EncryptedDeltaParts.empty // Can't derive key to encrypt
      case IsolatedDeltaParts(children: Map[String, IsolatedDeltaParts]) => EncryptedDeltaParts(
          children.flatMap { (pathElement, suffixParts) =>
            (keyMap.inner.get(pathElement), suffixParts.inner) match
              case (Some(childKeyMap: KeyMap), childParts: Map[String, IsolatedDeltaParts]) =>
                val child = encryptDeltaRec(childKeyMap, dot, suffixParts)
                if !child.isEmpty then Some(pathElement -> child)
                else None
              case (Some(kdk: KeyDerivationKey), parts) =>
                Some(pathElement -> FullKeyHierarchy(kdk).encryptDelta(dot, suffixParts))
              case (Some(_: KeyMap), parts: Array[Byte]) => None // Can't derive key to encrypt
              case (None, _)                             => None // Can't derive key to encrypt
          }
        )
  }

  override def decryptDelta(dot: Dot, encryptedDeltaParts: EncryptedDeltaParts): IsolatedDeltaParts = {
    decryptDeltaRec(keys, dot, encryptedDeltaParts)
  }

  private def decryptDeltaRec(
      keyMap: KeyMap,
      dot: Dot,
      encryptedDeltaParts: EncryptedDeltaParts
  ): IsolatedDeltaParts = {
    encryptedDeltaParts match
      case EncryptedDeltaParts(ciphertext: Array[Byte]) => IsolatedDeltaParts.empty // Can't derive key to decrypt
      case EncryptedDeltaParts(children: Map[String, EncryptedDeltaParts]) => IsolatedDeltaParts(
          children.flatMap { (pathElement, suffixParts) =>
            (keyMap.inner.get(pathElement), suffixParts.inner) match
              case (Some(childKeyMap: KeyMap), childParts: Map[String, EncryptedDeltaParts]) =>
                val child = decryptDeltaRec(childKeyMap, dot, suffixParts)
                if !child.isEmpty then Some(pathElement -> child)
                else None
              case (Some(kdk: KeyDerivationKey), parts) =>
                Some(pathElement -> FullKeyHierarchy(kdk).decryptDelta(dot, suffixParts))
              case (Some(_: KeyMap), parts: Array[Byte]) => None // Can't derive key to decrypt
              case (None, _)                             => None // Can't derive key to decrypt
          }
        )
  }
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
