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
   call ccc  void  @f(i32  5, i32  3)  
   call ccc  void  @f(i32  -5, i32  3)  
  ret i32 0 
}


define external ccc  void @f(i32  %a, i32  %b)    {
entry:
  %_v3 = srem i32 %a, %b 
   call ccc  void  @printInt(i32  %_v3)  
  ret void 
}