/**
 * Copyright (c) 2016 rxi
 * 
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#ifndef DYAD_H
#define DYAD_H

#include <stdarg.h>

#ifdef _WIN32
  #include <windows.h> /* For SOCKET */
#endif


#ifdef __cplusplus
extern "C" {
#endif

#if _WIN32
typedef SOCKET dyad_Socket;
#else
typedef int dyad_Socket;
#endif

struct dyad_Stream;
typedef struct dyad_Stream dyad_Stream;

typedef struct {
  int type;
  void *udata;
  dyad_Stream *stream;
  dyad_Stream *remote;
  const char *msg;
  char *data;
  int size;
} dyad_Event;

typedef void (*dyad_Callback)(dyad_Event*);
typedef void (*dyad_PanicCallback)(const char*);

enum {
  DYAD_EVENT_NULL,
  DYAD_EVENT_DESTROY,
  DYAD_EVENT_ACCEPT,
  DYAD_EVENT_LISTEN,
  DYAD_EVENT_CONNECT,
  DYAD_EVENT_CLOSE,
  DYAD_EVENT_READY,
  DYAD_EVENT_DATA,
  DYAD_EVENT_LINE,
  DYAD_EVENT_ERROR,
  DYAD_EVENT_TIMEOUT,
  DYAD_EVENT_TICK
};

enum {
  DYAD_STATE_CLOSED,
  DYAD_STATE_CLOSING,
  DYAD_STATE_CONNECTING,
  DYAD_STATE_CONNECTED,
  DYAD_STATE_LISTENING
};


void dyad_init(void);
void dyad_update(void);
void dyad_shutdown(void);
const char *dyad_getVersion(void);
double dyad_getTime(void);
int  dyad_getStreamCount(void);
void dyad_setTickInterval(double seconds);
void dyad_setUpdateTimeout(double seconds);
dyad_PanicCallback dyad_atPanic(dyad_PanicCallback func);

dyad_Stream *dyad_newStream(void);
int  dyad_listen(dyad_Stream *stream, int port);
int  dyad_listenEx(dyad_Stream *stream, const char *host, int port,
                   int backlog);
int  dyad_connect(dyad_Stream *stream, const char *host, int port);
void dyad_addListener(dyad_Stream *stream, int event,
                      dyad_Callback callback, void *udata);
void dyad_removeListener(dyad_Stream *stream, int event,
                         dyad_Callback callback, void *udata);
void dyad_removeAllListeners(dyad_Stream *stream, int event);
void dyad_end(dyad_Stream *stream);
void dyad_close(dyad_Stream *stream);
void dyad_write(dyad_Stream *stream, const void *data, int size);
void dyad_vwritef(dyad_Stream *stream, const char *fmt, va_list args);
void dyad_writef(dyad_Stream *stream, const char *fmt, ...);
void dyad_setTimeout(dyad_Stream *stream, double seconds);
void dyad_setNoDelay(dyad_Stream *stream, int opt);
int  dyad_getState(dyad_Stream *stream);
const char *dyad_getAddress(dyad_Stream *stream);
int  dyad_getPort(dyad_Stream *stream);
int  dyad_getBytesSent(dyad_Stream *stream);
int  dyad_getBytesReceived(dyad_Stream *stream);
dyad_Socket dyad_getSocket(dyad_Stream *stream);

// Modification
void dyad_setReadFDs(int n, int fds[], void (*handlers[])());
void dyad_setWriteFDs(int n, int fds[], void (*handlers[])());
void dyad_setExceptFDs(int n, int fds[], void (*handlers[])());

#ifdef __cplusplus
} // extern "C"
#endif

#endif
