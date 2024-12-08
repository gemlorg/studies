; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


@_v3 = private   constant [157 x i8] c"\22\0apop\0apowrot:\0agetstatic java/lang/System/out Ljava/io/PrintStream;\0aldc \22zle \22\0ainvokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\0agoto powrot\0aldc \22\00"


define external ccc  i32 @f(i32  %p)    {
entry:
  %_v1 = mul   i32 2, %p 
  %_v2 = add   i32 %p, %_v1 
  %_v4 = bitcast [157 x i8]* @_v3 to i8* 
   call ccc  void  @printString(i8*  %_v4)  
  ret i32 %_v2 
}


define external ccc  i32 @main()    {
entry:
  %_v6 =  call ccc  i32  @f(i32  1)  
  %_v7 = sub   i32 %_v6, 3 
  ret i32 %_v7 
}