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


@_g0 =    constant [4 x i8] c"abc\00"


@_g1 =    constant [4 x i8] c"def\00"


@_g2 =    constant [5 x i8] c"test\00"


@_g3 =    constant [6 x i8] c"test2\00"


@_g4 =    constant [5 x i8] c"test\00"


@_g5 =    constant [3 x i8] c" a\00"


@_g6 =    constant [5 x i8] c"test\00"


@_g7 =    constant [6 x i8] c"test2\00"


@_g8 =    constant [5 x i8] c"test\00"


@_g9 =    constant [6 x i8] c"test2\00"


@_g10 =    constant [5 x i8] c"test\00"


@_g11 =    constant [6 x i8] c"test2\00"


@_g12 =    constant [5 x i8] c"hejo\00"


@_g13 =    constant [5 x i8] c"hejo\00"


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
  %_v0 = bitcast [4 x i8]* @_g0 to i8* 
  %_v1 = bitcast [4 x i8]* @_g1 to i8* 
  %_v2 =  call ccc  i8*  @_concatStrings(i8*  %_v0, i8*  %_v1)  
  %_v3 = bitcast [5 x i8]* @_g2 to i8* 
  %_v4 = bitcast [6 x i8]* @_g3 to i8* 
  %_v5 =  call ccc  i8*  @_concatStrings(i8*  %_v3, i8*  %_v4)  
  %_v6 =  call ccc  i8*  @_concatStrings(i8*  %_v5, i8*  %_v5)  
  %_v7 = bitcast [5 x i8]* @_g4 to i8* 
  %_v8 = bitcast [3 x i8]* @_g5 to i8* 
  %_v9 =  call ccc  i8*  @_concatStrings(i8*  %_v7, i8*  %_v8)  
   call ccc  void  @printString(i8*  %_v9)  
  %_v11 = bitcast [5 x i8]* @_g6 to i8* 
  %_v12 =  call ccc  i8*  @_concatStrings(i8*  %_v11, i8*  %_v9)  
  %_v13 = bitcast [6 x i8]* @_g7 to i8* 
  %_v14 =  call ccc  i8*  @_concatStrings(i8*  %_v12, i8*  %_v13)  
  %_v15 =  call ccc  i8*  @_concatStrings(i8*  %_v14, i8*  %_v9)  
  %_v16 =  call ccc  i8*  @_concatStrings(i8*  %_v15, i8*  %_v6)  
   call ccc  void  @printString(i8*  %_v9)  
  %_v18 = bitcast [5 x i8]* @_g8 to i8* 
  %_v19 = bitcast [6 x i8]* @_g9 to i8* 
  %_v20 =  call ccc  i8*  @_concatStrings(i8*  %_v18, i8*  %_v19)  
  %_v21 = bitcast [5 x i8]* @_g10 to i8* 
  %_v22 = bitcast [6 x i8]* @_g11 to i8* 
  %_v23 =  call ccc  i8*  @_concatStrings(i8*  %_v21, i8*  %_v22)  
  %_v24 =  call ccc  i8*  @_concatStrings(i8*  %_v23, i8*  %_v20)  
   call ccc  void  @printString(i8*  %_v9)  
   call ccc  void  @printString(i8*  %_v16)  
  %_v27 =  call ccc  i8*  @_concatStrings(i8*  %_v9, i8*  %_v9)  
  %_v28 =  call ccc  i8*  @_concatStrings(i8*  %_v27, i8*  %_v16)  
  %_v29 =  call ccc  i8*  @_concatStrings(i8*  %_v9, i8*  %_v28)  
  %_v30 = bitcast [5 x i8]* @_g12 to i8* 
  %_v31 =  call ccc  i8*  @_concatStrings(i8*  %_v29, i8*  %_v30)  
  %_v32 =  call ccc  i8*  @_concatStrings(i8*  %_v31, i8*  %_v16)  
  %_v33 =  call ccc  i8*  @_concatStrings(i8*  %_v9, i8*  %_v16)  
  %_v34 =  call ccc  i8*  @_concatStrings(i8*  %_v32, i8*  %_v33)  
   call ccc  void  @printString(i8*  %_v34)  
  %_v36 =  call ccc  i8*  @_concatStrings(i8*  %_v9, i8*  %_v9)  
  %_v37 =  call ccc  i8*  @_concatStrings(i8*  %_v36, i8*  %_v16)  
  %_v38 =  call ccc  i8*  @_concatStrings(i8*  %_v9, i8*  %_v37)  
  %_v39 = bitcast [5 x i8]* @_g13 to i8* 
  %_v40 =  call ccc  i8*  @_concatStrings(i8*  %_v38, i8*  %_v39)  
  %_v41 =  call ccc  i8*  @_concatStrings(i8*  %_v40, i8*  %_v16)  
  %_v42 =  call ccc  i8*  @_concatStrings(i8*  %_v9, i8*  %_v16)  
  %_v43 =  call ccc  i8*  @_concatStrings(i8*  %_v41, i8*  %_v42)  
  ret i32 0 
}