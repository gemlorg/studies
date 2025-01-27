; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


declare external ccc  i8* @_malloc(i32)    


declare external ccc  i32 @_count_arr_length(i8*, i32)    


@_g0 =    constant [7 x i8] c"string\00"


@_g1 =    constant [2 x i8] c" \00"


@_g2 =    constant [14 x i8] c"concatenation\00"


@_g3 =    constant [5 x i8] c"true\00"


@_g4 =    constant [6 x i8] c"false\00"


%_arr = type {i8*, i32}


define external ccc  %_arr* @__arr_malloc()    {
entry:
  %0 = getelementptr inbounds %_arr, %_arr* zeroinitializer, i32 1 
  %1 = ptrtoint %_arr* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %_arr* 
  ret %_arr* %3 
}


define external ccc  i32 @main()    {
entry:
   call ccc  void  @printInt(i32  33)  
   call ccc  void  @printInt(i32  79)  
   call ccc  void  @printInt(i32  -1288)  
   call ccc  void  @printInt(i32  22)  
   call ccc  void  @printInt(i32  0)  
  br label %IB_1 
IB_1:
  br i1 1, label %IB_2, label %IB_3 
IB_2:
  %_iv1 = bitcast [5 x i8]* @_g3 to i8* 
   call ccc  void  @printString(i8*  %_iv1)  
  br label %IB_4 
IB_3:
  %_iv2 = bitcast [6 x i8]* @_g4 to i8* 
   call ccc  void  @printString(i8*  %_iv2)  
  br label %IB_4 
IB_4:
  br label %IB_5 
IB_5:
  br i1 0, label %IB_6, label %IB_7 
IB_6:
  %_iv3 = bitcast [5 x i8]* @_g3 to i8* 
   call ccc  void  @printString(i8*  %_iv3)  
  br label %IB_8 
IB_7:
  %_iv4 = bitcast [6 x i8]* @_g4 to i8* 
   call ccc  void  @printString(i8*  %_iv4)  
  br label %IB_8 
IB_8:
  %_v9 = bitcast [7 x i8]* @_g0 to i8* 
  %_v10 = bitcast [2 x i8]* @_g1 to i8* 
  %_v11 =  call ccc  i8*  @_concatStrings(i8*  %_v9, i8*  %_v10)  
  %_v12 = bitcast [14 x i8]* @_g2 to i8* 
  %_v13 =  call ccc  i8*  @_concatStrings(i8*  %_v11, i8*  %_v12)  
   call ccc  void  @printString(i8*  %_v13)  
  ret i32 0 
}


define external ccc  void @printBool(i1  %b)    {
entry:
  br i1 %b, label %_B1, label %_B2 
_B1:
  %_v0 = bitcast [5 x i8]* @_g3 to i8* 
   call ccc  void  @printString(i8*  %_v0)  
  ret void 
_B2:
  %_v2 = bitcast [6 x i8]* @_g4 to i8* 
   call ccc  void  @printString(i8*  %_v2)  
  ret void 
}