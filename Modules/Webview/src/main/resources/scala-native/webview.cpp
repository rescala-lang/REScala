#define WEBVIEW_STATIC

#include "webview.h"

// export macros
extern "C" {
  int scalanative_WEBVIEW_HINT_NONE = WEBVIEW_HINT_NONE;
  int scalanative_WEBVIEW_HINT_MIN = WEBVIEW_HINT_MIN;
  int scalanative_WEBVIEW_HINT_MAX = WEBVIEW_HINT_MAX;
  int scalanative_WEBVIEW_HINT_FIXED = WEBVIEW_HINT_FIXED;
}
