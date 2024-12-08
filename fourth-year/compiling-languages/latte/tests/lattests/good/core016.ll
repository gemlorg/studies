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
  br label %_B2 
_B1:
  %_v2 = sub   i32 %_v1, 2 
  br label %_B2 
_B2:
  %_v1 = phi i32 [17, %entry], [%_v2, %_B1] 
  %_v3 = icmp sgt i32 %_v1, 0 
  br i1 %_v3, label %_B1, label %_B3 
_B3:
  %_v4 = icmp slt i32 %_v1, 0 
  br i1 %_v4, label %_B4, label %_B5 
_B4:
   call ccc  void  @printInt(i32  0)  
  ret i32 0 
_B5:
   call ccc  void  @printInt(i32  1)  
  ret i32 0 
}