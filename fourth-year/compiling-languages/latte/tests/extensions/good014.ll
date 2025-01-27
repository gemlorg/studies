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


@_g0 =    constant [10 x i8] c"hello 14 \00"


@_g1 =    constant [1 x i8] c"\00"


%_arr = type {i8*, i32}


define external ccc  %_arr* @__arr_malloc()    {
entry:
  %0 = getelementptr inbounds %_arr, %_arr* zeroinitializer, i32 1 
  %1 = ptrtoint %_arr* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %_arr* 
  ret %_arr* %3 
}


%A = type {%_A_vtype*, i32, i8*}


%_A_vtype = type {void (%A*)*}


@_A_vdata =    global %_A_vtype { void (%A*)* @_cf_A_f }


define external ccc  %A* @_A_malloc()    {
entry:
  %0 = getelementptr inbounds %A, %A* zeroinitializer, i32 1 
  %1 = ptrtoint %A* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %A* 
  ret %A* %3 
}


define external ccc  void @_cf_A_f(%A*  %_this)    {
entry:
  %_v0 = alloca %A* 
  store   %A* %_this, %A** %_v0  
  %_v1 = bitcast [10 x i8]* @_g0 to i8* 
  %_v3 = load   %A*, %A** %_v0  
  %_v2 = getelementptr  %A, %A* %_v3, i32 0, i32 2 
  store   i8* %_v1, i8** %_v2  
  %_v5 = load   %A*, %A** %_v0  
  %_v4 = getelementptr  %A, %A* %_v5, i32 0, i32 2 
  %_v6 = load   i8*, i8** %_v4  
  %_v8 = load   %A*, %A** %_v0  
  %_v7 = getelementptr  %A, %A* %_v8, i32 0, i32 2 
  %_v9 = load   i8*, i8** %_v7  
  %_v10 =  call ccc  i8*  @_concatStrings(i8*  %_v6, i8*  %_v9)  
  %_v12 = load   %A*, %A** %_v0  
  %_v11 = getelementptr  %A, %A* %_v12, i32 0, i32 2 
  store   i8* %_v10, i8** %_v11  
  %_v14 = load   %A*, %A** %_v0  
  %_v13 = getelementptr  %A, %A* %_v14, i32 0, i32 2 
  %_v15 = load   i8*, i8** %_v13  
  %_v17 = load   %A*, %A** %_v0  
  %_v16 = getelementptr  %A, %A* %_v17, i32 0, i32 2 
  %_v18 = load   i8*, i8** %_v16  
  %_v19 =  call ccc  i8*  @_concatStrings(i8*  %_v15, i8*  %_v18)  
  %_v21 = load   %A*, %A** %_v0  
  %_v20 = getelementptr  %A, %A* %_v21, i32 0, i32 2 
  %_v22 = load   i8*, i8** %_v20  
  %_v23 =  call ccc  i8*  @_concatStrings(i8*  %_v19, i8*  %_v22)  
  %_v25 = load   %A*, %A** %_v0  
  %_v24 = getelementptr  %A, %A* %_v25, i32 0, i32 2 
  store   i8* %_v23, i8** %_v24  
  %_v27 = load   %A*, %A** %_v0  
  %_v26 = getelementptr  %A, %A* %_v27, i32 0, i32 2 
  %_v28 = load   i8*, i8** %_v26  
   call ccc  void  @printString(i8*  %_v28)  
  ret void 
}


define external ccc  i32 @main()    {
entry:
  %_v3 = bitcast [1 x i8]* @_g1 to i8* 
  %_v0 =  call ccc  %A*  @_A_malloc()  
  %_v1 = getelementptr  %A, %A* %_v0, i32 0, i32 0 
  store   %_A_vtype* @_A_vdata, %_A_vtype** %_v1  
  %_v2 = getelementptr  %A, %A* %_v0, i32 0, i32 1 
  store   i32 0, i32* %_v2  
  %_v4 = getelementptr  %A, %A* %_v0, i32 0, i32 2 
  store   i8* %_v3, i8** %_v4  
  %_v5 = alloca %A* 
  store   %A* zeroinitializer, %A** %_v5  
  store   %A* %_v0, %A** %_v5  
  %_v6 = load   %A*, %A** %_v5  
  %_v7 = getelementptr  %A, %A* %_v6, i32 0, i32 0 
  %_v8 = load   %_A_vtype*, %_A_vtype** %_v7  
  %_v9 = getelementptr  %_A_vtype, %_A_vtype* %_v8, i32 0, i32 0 
  %_v10 = load   void (%A*)*, void (%A*)** %_v9  
   call ccc  void  %_v10(%A*  %_v6)  
  %_v12 = load   %A*, %A** %_v5  
  %_v13 = getelementptr  %A, %A* %_v12, i32 0, i32 2 
  %_v14 = load   i8*, i8** %_v13  
  %_v15 = load   %A*, %A** %_v5  
  %_v16 = load   %A*, %A** %_v5  
  %_v17 = load   %A*, %A** %_v5  
  %_v18 = getelementptr  %A, %A* %_v17, i32 0, i32 2 
  %_v19 = load   i8*, i8** %_v18  
  ret i32 0 
}