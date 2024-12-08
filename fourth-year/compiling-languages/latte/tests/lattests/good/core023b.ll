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
  %_v2 = add   i32 %n, %m 
  %_v3 = add   i32 %l, %_v2 
  %_v4 = add   i32 %k, %_v3 
  %_v5 = add   i32 %j, %_v4 
  %_v6 = add   i32 %i, %_v5 
  %_v7 = add   i32 %h, %_v6 
  %_v8 = add   i32 %g, %_v7 
  %_v9 = add   i32 %f, %_v8 
  %_v10 = add   i32 %e, %_v9 
  %_v11 = add   i32 %d, %_v10 
  %_v12 = add   i32 %c, %_v11 
  %_v13 = add   i32 %b, %_v12 
  %_v14 = add   i32 %a, %_v13 
   call ccc  void  @printInt(i32  %_v14)  
  ret i32 %_v14 
}