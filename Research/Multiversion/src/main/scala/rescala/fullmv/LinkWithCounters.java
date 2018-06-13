package rescala.fullmv;

import java.util.concurrent.atomic.AtomicReference;

public class LinkWithCounters<T> extends AtomicReference<T> {
  volatile public int pending = 0;
  volatile public int changed = 0;
  public LinkWithCounters(T link) {
    super(link);
  }
}
