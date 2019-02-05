package ersirjs

sealed trait AppState
object AppState {
  case object IndexState extends AppState
}
