; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


@_v3 = private   constant [6 x i8] c"World\00"


define external ccc  i32 @main()    {
entry:
  %_v4 = bitcast [6 x i8]* @_v3 to i8* 
   call ccc  void  @printString(i8*  %_v4)  
  ret i32 1 
}