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
  %_iv1 = mul   i32 2, 1 
  %_iv2 = sdiv  i32 2, 2 
  %_iv3 = add   i32 %_iv1, %_iv2 
  %_iv4 = add   i32 %_iv3, 1 
  %_iv5 = add   i32 %_iv4, 2 
  %_iv6 = add   i32 %_iv5, 1 
  %_iv7 = add   i32 %_iv6, 2 
  %_iv8 = add   i32 %_iv7, 1 
  %_iv9 = add   i32 %_iv8, 2 
  %_iv10 = add   i32 %_iv9, 1 
  %_iv11 = sdiv  i32 2, 2 
  %_iv12 = add   i32 %_iv10, %_iv11 
  %_iv13 = add   i32 %_iv12, 1 
  %_iv14 = add   i32 %_iv13, 2 
  %_iv15 = add   i32 %_iv14, 1 
  %_iv16 = add   i32 %_iv15, 2 
  %_iv17 = srem i32 %_iv16, 10 
   call ccc  void  @printInt(i32  %_iv17)  
  br label %IB_2 
IB_2:
  %_v0 = bitcast i32 %_iv17 to i32 
  ret i32 %_v0 
}


define external ccc  i32 @foo(i32  %a, i32  %b, i32  %c, i32  %d, i32  %e, i32  %f, i32  %g, i32  %h, i32  %i, i32  %j, i32  %k, i32  %l, i32  %m, i32  %n)    {
entry:
  %_v0 = mul   i32 2, %a 
  %_v1 = sdiv  i32 %b, 2 
  %_v2 = add   i32 %_v0, %_v1 
  %_v3 = add   i32 %_v2, %c 
  %_v4 = add   i32 %_v3, %d 
  %_v5 = add   i32 %_v4, %e 
  %_v6 = add   i32 %_v5, %f 
  %_v7 = add   i32 %_v6, %g 
  %_v8 = add   i32 %_v7, %h 
  %_v9 = add   i32 %_v8, %i 
  %_v10 = sdiv  i32 %j, 2 
  %_v11 = add   i32 %_v9, %_v10 
  %_v12 = add   i32 %_v11, %k 
  %_v13 = add   i32 %_v12, %l 
  %_v14 = add   i32 %_v13, %m 
  %_v15 = add   i32 %_v14, %n 
  %_v16 = srem i32 %_v15, 10 
   call ccc  void  @printInt(i32  %_v16)  
  ret i32 %_v16 
}