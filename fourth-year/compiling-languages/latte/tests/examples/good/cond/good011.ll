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


@_g0 =    constant [6 x i8] c"1 < 2\00"


@_g1 =    constant [3 x i8] c"ok\00"


@_g2 =    constant [7 x i8] c"2 <= 2\00"


@_g3 =    constant [3 x i8] c"ok\00"


@_g4 =    constant [7 x i8] c"1 <= 2\00"


@_g5 =    constant [3 x i8] c"ok\00"


@_g6 =    constant [7 x i8] c"1 == 1\00"


@_g7 =    constant [3 x i8] c"ok\00"


@_g8 =    constant [7 x i8] c"2 >= 2\00"


@_g9 =    constant [3 x i8] c"ok\00"


@_g10 =    constant [7 x i8] c"2 >= 1\00"


@_g11 =    constant [3 x i8] c"ok\00"


@_g12 =    constant [6 x i8] c"2 > 1\00"


@_g13 =    constant [3 x i8] c"ok\00"


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
  %_v1 = bitcast [6 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_v1)  
  %_v4 = bitcast [3 x i8]* @_g1 to i8* 
   call ccc  void  @printString(i8*  %_v4)  
  %_v7 = bitcast [7 x i8]* @_g2 to i8* 
   call ccc  void  @printString(i8*  %_v7)  
  %_v10 = bitcast [3 x i8]* @_g3 to i8* 
   call ccc  void  @printString(i8*  %_v10)  
  %_v13 = bitcast [7 x i8]* @_g4 to i8* 
   call ccc  void  @printString(i8*  %_v13)  
  %_v16 = bitcast [3 x i8]* @_g5 to i8* 
   call ccc  void  @printString(i8*  %_v16)  
  %_v19 = bitcast [7 x i8]* @_g6 to i8* 
   call ccc  void  @printString(i8*  %_v19)  
  %_v22 = bitcast [3 x i8]* @_g7 to i8* 
   call ccc  void  @printString(i8*  %_v22)  
  %_v25 = bitcast [7 x i8]* @_g8 to i8* 
   call ccc  void  @printString(i8*  %_v25)  
  %_v28 = bitcast [3 x i8]* @_g9 to i8* 
   call ccc  void  @printString(i8*  %_v28)  
  %_v31 = bitcast [7 x i8]* @_g10 to i8* 
   call ccc  void  @printString(i8*  %_v31)  
  %_v34 = bitcast [3 x i8]* @_g11 to i8* 
   call ccc  void  @printString(i8*  %_v34)  
  %_v37 = bitcast [6 x i8]* @_g12 to i8* 
   call ccc  void  @printString(i8*  %_v37)  
  %_v40 = bitcast [3 x i8]* @_g13 to i8* 
   call ccc  void  @printString(i8*  %_v40)  
  ret i32 0 
}