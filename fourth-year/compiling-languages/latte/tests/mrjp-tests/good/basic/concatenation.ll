; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


@_v1 = private   constant [2 x i8] c"a\00"


@_v3 = private   constant [2 x i8] c"b\00"


define external ccc  i32 @main()    {
entry:
  %_v2 = bitcast [2 x i8]* @_v1 to i8* 
  %_v4 = bitcast [2 x i8]* @_v3 to i8* 
  %_v5 =  call ccc  i8*  @_concatStrings(i8*  %_v2, i8*  %_v4)  
   call ccc  void  @printString(i8*  %_v5)  
  ret i32 0 
}