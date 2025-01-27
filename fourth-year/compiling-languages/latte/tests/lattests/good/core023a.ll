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
  %_iv1 = add   i32 1, 2 
  %_iv2 = add   i32 %_iv1, 1 
  %_iv3 = add   i32 %_iv2, 2 
  %_iv4 = add   i32 %_iv3, 1 
  %_iv5 = add   i32 %_iv4, 2 
  %_iv6 = add   i32 %_iv5, 1 
  %_iv7 = add   i32 %_iv6, 2 
  %_iv8 = add   i32 %_iv7, 1 
  %_iv9 = add   i32 %_iv8, 2 
  %_iv10 = add   i32 %_iv9, 1 
  %_iv11 = add   i32 %_iv10, 2 
  %_iv12 = add   i32 %_iv11, 2 
  %_iv13 = add   i32 %_iv12, 1 
   call ccc  void  @printInt(i32  %_iv13)  
  br label %IB_2 
IB_2:
  %_v0 = bitcast i32 %_iv13 to i32 
  ret i32 %_v0 
}


define external ccc  i32 @foo(i32  %a, i32  %b, i32  %c, i32  %d, i32  %e, i32  %f, i32  %g, i32  %h, i32  %i, i32  %j, i32  %k, i32  %l, i32  %m, i32  %n)    {
entry:
  %_v0 = add   i32 %a, %b 
  %_v1 = add   i32 %_v0, %c 
  %_v2 = add   i32 %_v1, %d 
  %_v3 = add   i32 %_v2, %e 
  %_v4 = add   i32 %_v3, %f 
  %_v5 = add   i32 %_v4, %g 
  %_v6 = add   i32 %_v5, %h 
  %_v7 = add   i32 %_v6, %i 
  %_v8 = add   i32 %_v7, %j 
  %_v9 = add   i32 %_v8, %k 
  %_v10 = add   i32 %_v9, %l 
  %_v11 = add   i32 %_v10, %n 
  %_v12 = add   i32 %_v11, %m 
   call ccc  void  @printInt(i32  %_v12)  
  ret i32 %_v12 
}