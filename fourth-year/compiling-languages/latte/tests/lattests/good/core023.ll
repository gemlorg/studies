; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


define external ccc  i32 @main()    {
entry:
  %_v1 =  call ccc  i32  @foo(i32  1, i32  2, i32  1, i32  2, i32  1, i32  2, i32  1, i32  2, i32  1, i32  2, i32  1, i32  2, i32  1, i32  2)  
  ret i32 %_v1 
}


define external ccc  i32 @foo(i32  %a, i32  %b, i32  %c, i32  %d, i32  %e, i32  %f, i32  %g, i32  %h, i32  %i, i32  %j, i32  %k, i32  %l, i32  %m, i32  %n)    {
entry:
  %_v2 = mul   i32 2, %a 
  %_v3 = sdiv  i32 %b, 2 
  %_v4 = add   i32 %_v2, %_v3 
  %_v5 = add   i32 %_v4, %c 
  %_v6 = add   i32 %_v5, %d 
  %_v7 = add   i32 %_v6, %e 
  %_v8 = add   i32 %_v7, %f 
  %_v9 = add   i32 %_v8, %g 
  %_v10 = add   i32 %_v9, %h 
  %_v11 = add   i32 %_v10, %i 
  %_v12 = sdiv  i32 %j, 2 
  %_v13 = add   i32 %_v11, %_v12 
  %_v14 = add   i32 %_v13, %k 
  %_v15 = add   i32 %_v14, %l 
  %_v16 = add   i32 %_v15, %m 
  %_v17 = add   i32 %_v16, %n 
  %_v18 = srem i32 %_v17, 10 
   call ccc  void  @printInt(i32  %_v18)  
  ret i32 %_v18 
}