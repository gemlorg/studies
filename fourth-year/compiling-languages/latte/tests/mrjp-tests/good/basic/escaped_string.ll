; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


declare external ccc  i8* @_malloc(i32)    


declare external ccc  i32 @_count_arr_length(i8*, i32)    


@_g0 =    constant [157 x i8] c"\22\0apop\0apowrot:\0agetstatic java/lang/System/out Ljava/io/PrintStream;\0aldc \22zle \22\0ainvokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\0agoto powrot\0aldc \22\00"


%_arr = type {i8*, i32}


define external ccc  %_arr* @__arr_malloc()    {
entry:
  %0 = getelementptr inbounds %_arr, %_arr* zeroinitializer, i32 1 
  %1 = ptrtoint %_arr* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %_arr* 
  ret %_arr* %3 
}


define external ccc  i32 @f(i32  %p)    {
entry:
  %_v0 = mul   i32 2, %p 
  %_v1 = add   i32 %p, %_v0 
  %_v2 = bitcast [157 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_v2)  
  ret i32 %_v1 
}


define external ccc  i32 @main()    {
entry:
  br label %IB_1 
IB_1:
  %_iv1 = mul   i32 2, 1 
  %_iv2 = add   i32 1, %_iv1 
  %_iv3 = bitcast [157 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_iv3)  
  br label %IB_2 
IB_2:
  %_v0 = bitcast i32 %_iv2 to i32 
  %_v1 = sub   i32 %_v0, 3 
  ret i32 %_v1 
}