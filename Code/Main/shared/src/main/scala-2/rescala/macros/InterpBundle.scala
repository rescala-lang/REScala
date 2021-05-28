package rescala.macros

import rescala.core.Core

trait InterpBundle extends Core {
    trait InterpMacro[+A] extends Interp[A] with MacroAccess[A, Interp[A]]
}
