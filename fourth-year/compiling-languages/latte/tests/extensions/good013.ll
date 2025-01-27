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


@_g8 =    constant [5 x i8] c"hejo\00"


@_g9 =    constant [5 x i8] c"hejo\00"


@_g10 =    constant [5 x i8] c"hejo\00"


@_g11 =    constant [5 x i8] c"hejo\00"


@_g12 =    constant [5 x i8] c"hejo\00"


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
  %_v17 =  call ccc  i8*  @_concatStrings(i8*  %_v9, i8*  %_v9)  
   call ccc  void  @printString(i8*  %_v17)  
  %_v19 =  call ccc  i8*  @_concatStrings(i8*  %_v9, i8*  %_v9)  
  %_v20 =  call ccc  i8*  @_concatStrings(i8*  %_v19, i8*  %_v16)  
  %_v21 =  call ccc  i8*  @_concatStrings(i8*  %_v9, i8*  %_v20)  
  %_v22 = bitcast [5 x i8]* @_g8 to i8* 
  %_v23 =  call ccc  i8*  @_concatStrings(i8*  %_v21, i8*  %_v22)  
  %_v24 =  call ccc  i8*  @_concatStrings(i8*  %_v23, i8*  %_v16)  
  %_v25 =  call ccc  i8*  @_concatStrings(i8*  %_v9, i8*  %_v16)  
  %_v26 =  call ccc  i8*  @_concatStrings(i8*  %_v24, i8*  %_v25)  
   call ccc  void  @printString(i8*  %_v26)  
  %_v28 =  call ccc  i8*  @_concatStrings(i8*  %_v9, i8*  %_v9)  
  %_v29 =  call ccc  i8*  @_concatStrings(i8*  %_v28, i8*  %_v16)  
  %_v30 =  call ccc  i8*  @_concatStrings(i8*  %_v9, i8*  %_v29)  
  %_v31 = bitcast [5 x i8]* @_g9 to i8* 
  %_v32 =  call ccc  i8*  @_concatStrings(i8*  %_v30, i8*  %_v31)  
  %_v33 =  call ccc  i8*  @_concatStrings(i8*  %_v32, i8*  %_v16)  
  %_v34 =  call ccc  i8*  @_concatStrings(i8*  %_v9, i8*  %_v16)  
  %_v35 =  call ccc  i8*  @_concatStrings(i8*  %_v33, i8*  %_v34)  
  %_v36 =  call ccc  i8*  @_concatStrings(i8*  %_v9, i8*  %_v9)  
  %_v37 =  call ccc  i8*  @_concatStrings(i8*  %_v36, i8*  %_v16)  
  %_v38 =  call ccc  i8*  @_concatStrings(i8*  %_v9, i8*  %_v37)  
  %_v39 = bitcast [5 x i8]* @_g10 to i8* 
  %_v40 =  call ccc  i8*  @_concatStrings(i8*  %_v38, i8*  %_v39)  
  %_v41 =  call ccc  i8*  @_concatStrings(i8*  %_v40, i8*  %_v16)  
  %_v42 =  call ccc  i8*  @_concatStrings(i8*  %_v9, i8*  %_v16)  
  %_v43 =  call ccc  i8*  @_concatStrings(i8*  %_v41, i8*  %_v42)  
   call ccc  void  @printString(i8*  %_v43)  
  %_v45 =  call ccc  i8*  @_concatStrings(i8*  %_v9, i8*  %_v9)  
  %_v46 =  call ccc  i8*  @_concatStrings(i8*  %_v45, i8*  %_v16)  
  %_v47 =  call ccc  i8*  @_concatStrings(i8*  %_v9, i8*  %_v46)  
  %_v48 = bitcast [5 x i8]* @_g11 to i8* 
  %_v49 =  call ccc  i8*  @_concatStrings(i8*  %_v47, i8*  %_v48)  
  %_v50 =  call ccc  i8*  @_concatStrings(i8*  %_v49, i8*  %_v16)  
  %_v51 =  call ccc  i8*  @_concatStrings(i8*  %_v9, i8*  %_v16)  
  %_v52 =  call ccc  i8*  @_concatStrings(i8*  %_v50, i8*  %_v51)  
   call ccc  void  @printString(i8*  %_v52)  
  %_v54 =  call ccc  i8*  @_concatStrings(i8*  %_v9, i8*  %_v9)  
  %_v55 =  call ccc  i8*  @_concatStrings(i8*  %_v54, i8*  %_v16)  
  %_v56 =  call ccc  i8*  @_concatStrings(i8*  %_v9, i8*  %_v55)  
  %_v57 = bitcast [5 x i8]* @_g12 to i8* 
  %_v58 =  call ccc  i8*  @_concatStrings(i8*  %_v56, i8*  %_v57)  
  %_v59 =  call ccc  i8*  @_concatStrings(i8*  %_v58, i8*  %_v16)  
  %_v60 =  call ccc  i8*  @_concatStrings(i8*  %_v9, i8*  %_v16)  
  %_v61 =  call ccc  i8*  @_concatStrings(i8*  %_v59, i8*  %_v60)  
   call ccc  void  @printString(i8*  %_v61)  
  ret i32 0 
}