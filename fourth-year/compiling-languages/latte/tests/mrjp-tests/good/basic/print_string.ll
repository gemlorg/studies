; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


@_v1 = private   constant [4 x i8] c"abc\00"


define external ccc  i32 @main()    {
entry:
  %_v2 = bitcast [4 x i8]* @_v1 to i8* 
   call ccc  void  @printString(i8*  %_v2)  
  ret i32 0 
}