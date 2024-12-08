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
  %_v1 =  call ccc  i32  @readInt()  
  %_v2 =  call ccc  i8*  @readString()  
  %_v3 =  call ccc  i8*  @readString()  
  %_v4 = sub   i32 %_v1, 5 
   call ccc  void  @printInt(i32  %_v4)  
  %_v6 =  call ccc  i8*  @_concatStrings(i8*  %_v2, i8*  %_v3)  
   call ccc  void  @printString(i8*  %_v6)  
  ret i32 0 
}