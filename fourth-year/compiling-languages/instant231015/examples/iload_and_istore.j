.class public iload_and_istore
.super java/lang/Object
.method public <init>()V
  .limit stack 1
  .limit locals 1
  aload_0
  invokenonvirtual java/lang/Object/<init>()V
  return
.end method
.method public static main([Ljava/lang/String;)V
.limit stack 2
.limit locals 5
iconst_1
istore_1
getstatic java/lang/System/out Ljava/io/PrintStream;
iload_1
invokevirtual java/io/PrintStream/println(I)V
iconst_2
istore_2
getstatic java/lang/System/out Ljava/io/PrintStream;
iload_2
invokevirtual java/io/PrintStream/println(I)V
iconst_3
istore_3
getstatic java/lang/System/out Ljava/io/PrintStream;
iload_3
invokevirtual java/io/PrintStream/println(I)V
iconst_4
istore 4
getstatic java/lang/System/out Ljava/io/PrintStream;
iload 4
invokevirtual java/io/PrintStream/println(I)V
return
.end method