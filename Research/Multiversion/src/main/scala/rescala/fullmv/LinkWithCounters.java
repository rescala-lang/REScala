package rescala.fullmv;

import scala.collection.immutable.List$;
import scala.collection.immutable.List;

import java.util.concurrent.atomic.AtomicReference;

public class LinkWithCounters<T> extends AtomicReference<T> {
  private static final List<Thread> EMPTY_THREAD_LIST = List$.MODULE$.<Thread>empty();
  volatile public int pending = 0;
  volatile public int changed = 0;
  volatile public List<Thread> stableSleepers = EMPTY_THREAD_LIST;
  volatile public List<Thread> finalSleepers = EMPTY_THREAD_LIST;
  public LinkWithCounters(T link) {
    super(link);
  }
}
