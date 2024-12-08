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
  %_v1 =  call ccc  i32  @f(i32  0)  
  ret i32 0 
}


define external ccc  i32 @f(i32  %a)    {
entry:
  %_v2 = icmp eq i32 %a, 0 
  br i1 %_v2, label %_B1, label %_B2 
_B1:
  ret i32 0 
_B2:
  ret i32 1 
}