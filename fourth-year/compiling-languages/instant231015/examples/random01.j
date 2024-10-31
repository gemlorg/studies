.class public random01
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
getstatic java/lang/System/out Ljava/io/PrintStream;
iconst_1
iconst_1
iadd
iconst_1
swap
iadd
invokevirtual java/io/PrintStream/println(I)V
return
.end method