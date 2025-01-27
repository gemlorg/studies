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


%_arr = type {i8*, i32}


define external ccc  %_arr* @__arr_malloc()    {
entry:
  %0 = getelementptr inbounds %_arr, %_arr* zeroinitializer, i32 1 
  %1 = ptrtoint %_arr* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %_arr* 
  ret %_arr* %3 
}


define external ccc  i32 @main()    {
entry:
  br label %IB_1 
IB_1:
  %_iv1 = add   i32 2, 1 
  %_iv2 = add   i32 2, %_iv1 
  %_iv3 = add   i32 1, %_iv2 
  %_iv4 = add   i32 2, %_iv3 
  %_iv5 = add   i32 1, %_iv4 
  %_iv6 = add   i32 2, %_iv5 
  %_iv7 = add   i32 1, %_iv6 
  %_iv8 = add   i32 2, %_iv7 
  %_iv9 = add   i32 1, %_iv8 
  %_iv10 = add   i32 2, %_iv9 
  %_iv11 = add   i32 1, %_iv10 
  %_iv12 = add   i32 2, %_iv11 
  %_iv13 = add   i32 1, %_iv12 
   call ccc  void  @printInt(i32  %_iv13)  
  br label %IB_2 
IB_2:
  %_v0 = bitcast i32 %_iv13 to i32 
  ret i32 %_v0 
}


define external ccc  i32 @foo(i32  %a, i32  %b, i32  %c, i32  %d, i32  %e, i32  %f, i32  %g, i32  %h, i32  %i, i32  %j, i32  %k, i32  %l, i32  %m, i32  %n)    {
entry:
  %_v0 = add   i32 %n, %m 
  %_v1 = add   i32 %l, %_v0 
  %_v2 = add   i32 %k, %_v1 
  %_v3 = add   i32 %j, %_v2 
  %_v4 = add   i32 %i, %_v3 
  %_v5 = add   i32 %h, %_v4 
  %_v6 = add   i32 %g, %_v5 
  %_v7 = add   i32 %f, %_v6 
  %_v8 = add   i32 %e, %_v7 
  %_v9 = add   i32 %d, %_v8 
  %_v10 = add   i32 %c, %_v9 
  %_v11 = add   i32 %b, %_v10 
  %_v12 = add   i32 %a, %_v11 
   call ccc  void  @printInt(i32  %_v12)  
  ret i32 %_v12 
}