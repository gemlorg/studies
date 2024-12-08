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
  %_v2 = add   i32 %a, %b 
  %_v3 = add   i32 %_v2, %c 
  %_v4 = add   i32 %_v3, %d 
  %_v5 = add   i32 %_v4, %e 
  %_v6 = add   i32 %_v5, %f 
  %_v7 = add   i32 %_v6, %g 
  %_v8 = add   i32 %_v7, %h 
  %_v9 = add   i32 %_v8, %i 
  %_v10 = add   i32 %_v9, %j 
  %_v11 = add   i32 %_v10, %k 
  %_v12 = add   i32 %_v11, %l 
  %_v13 = add   i32 %_v12, %n 
  %_v14 = add   i32 %_v13, %m 
   call ccc  void  @printInt(i32  %_v14)  
  ret i32 %_v14 
}