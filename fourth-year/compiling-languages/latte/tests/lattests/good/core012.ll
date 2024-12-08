; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


@_v10 = private   constant [7 x i8] c"string\00"


@_v12 = private   constant [2 x i8] c" \00"


@_v15 = private   constant [14 x i8] c"concatenation\00"


@_v19 = private   constant [5 x i8] c"true\00"


@_v22 = private   constant [6 x i8] c"false\00"


define external ccc  i32 @main()    {
entry:
   call ccc  void  @printInt(i32  33)  
   call ccc  void  @printInt(i32  79)  
   call ccc  void  @printInt(i32  -1288)  
   call ccc  void  @printInt(i32  22)  
   call ccc  void  @printInt(i32  0)  
   call ccc  void  @printBool(i1  1)  
   call ccc  void  @printBool(i1  0)  
  %_v11 = bitcast [7 x i8]* @_v10 to i8* 
  %_v13 = bitcast [2 x i8]* @_v12 to i8* 
  %_v14 =  call ccc  i8*  @_concatStrings(i8*  %_v11, i8*  %_v13)  
  %_v16 = bitcast [14 x i8]* @_v15 to i8* 
  %_v17 =  call ccc  i8*  @_concatStrings(i8*  %_v14, i8*  %_v16)  
   call ccc  void  @printString(i8*  %_v17)  
  ret i32 0 
}


define external ccc  void @printBool(i1  %b)    {
entry:
  br i1 %b, label %_B1, label %_B2 
_B1:
  %_v20 = bitcast [5 x i8]* @_v19 to i8* 
   call ccc  void  @printString(i8*  %_v20)  
  ret void 
_B2:
  %_v23 = bitcast [6 x i8]* @_v22 to i8* 
   call ccc  void  @printString(i8*  %_v23)  
  ret void 
_B3:
  ret void 
}