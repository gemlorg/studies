.class public alloca
.super java/lang/Object
.method public <init>()V
  .limit stack 1
  .limit locals 1
  aload_0
  invokenonvirtual java/lang/Object/<init>()V
  return
.end method
.method public static main([Ljava/lang/String;)V
.limit stack 3
.limit locals 3
iconst_1
istore_1
iload_1
iload_1
iadd
istore_1
iload_1
iload_1
iadd
istore_1
getstatic java/lang/System/out Ljava/io/PrintStream;
iload_1
invokevirtual java/io/PrintStream/println(I)V
return
.end method