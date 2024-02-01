package loci
package compatibility

import scala.collection.convert.{DecorateAsJava, DecorateAsScala}

private[loci] object jdkCollectionConverters extends DecorateAsJava with DecorateAsScala
