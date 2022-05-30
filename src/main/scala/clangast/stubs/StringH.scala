package clangast.stubs

import clangast.decl.{CFunctionDecl, CInclude}

object StringH {
  val include: CInclude = CInclude("string.h")
  
  val memchr: CFunctionDecl = CFunctionStub("memchr")
  val memcmp: CFunctionDecl = CFunctionStub("memcmp")
  val memcpy: CFunctionDecl = CFunctionStub("memcpy")
  val memmove: CFunctionDecl = CFunctionStub("memmove")
  val memset: CFunctionDecl = CFunctionStub("memset")
  val strcat: CFunctionDecl = CFunctionStub("strcat")
  val strncat: CFunctionDecl = CFunctionStub("strncat")
  val strchr: CFunctionDecl = CFunctionStub("strchr")
  val strcmp: CFunctionDecl = CFunctionStub("strcmp")
  val strncmp: CFunctionDecl = CFunctionStub("strncmp")
  val strcoll: CFunctionDecl = CFunctionStub("strcoll")
  val strcpy: CFunctionDecl = CFunctionStub("strcpy")
  val strncpy: CFunctionDecl = CFunctionStub("strncpy")
  val strcspn: CFunctionDecl = CFunctionStub("strcspn")
  val strerror: CFunctionDecl = CFunctionStub("strerror")
  val strlen: CFunctionDecl = CFunctionStub("strlen")
  val strpbrk: CFunctionDecl = CFunctionStub("strpbrk")
  val strrchr: CFunctionDecl = CFunctionStub("strrchr")
  val strspn: CFunctionDecl = CFunctionStub("strspn")
  val strstr: CFunctionDecl = CFunctionStub("strstr")
  val strtok: CFunctionDecl = CFunctionStub("strtok")
  val strxfrm: CFunctionDecl = CFunctionStub("strxfrm")
}
