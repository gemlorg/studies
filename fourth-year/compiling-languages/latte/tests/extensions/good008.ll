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


@_g0 =    constant [1 x i8] c"\00"


%_arr = type {i8*, i32}


define external ccc  %_arr* @__arr_malloc()    {
entry:
  %0 = getelementptr inbounds %_arr, %_arr* zeroinitializer, i32 1 
  %1 = ptrtoint %_arr* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %_arr* 
  ret %_arr* %3 
}


%A = type {%_A_vtype*, i32, i32, i8*}


%_A_vtype = type {}


@_A_vdata =    global %_A_vtype {  }


define external ccc  %A* @_A_malloc()    {
entry:
  %0 = getelementptr inbounds %A, %A* zeroinitializer, i32 1 
  %1 = ptrtoint %A* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %A* 
  ret %A* %3 
}


define external ccc  i32 @main()    {
entry:
  %_v4 = bitcast [1 x i8]* @_g0 to i8* 
  %_v0 =  call ccc  %A*  @_A_malloc()  
  %_v1 = getelementptr  %A, %A* %_v0, i32 0, i32 0 
  store   %_A_vtype* @_A_vdata, %_A_vtype** %_v1  
  %_v2 = getelementptr  %A, %A* %_v0, i32 0, i32 1 
  store   i32 0, i32* %_v2  
  %_v3 = getelementptr  %A, %A* %_v0, i32 0, i32 2 
  store   i32 0, i32* %_v3  
  %_v5 = getelementptr  %A, %A* %_v0, i32 0, i32 3 
  store   i8* %_v4, i8** %_v5  
  %_v6 = alloca %A* 
  store   %A* zeroinitializer, %A** %_v6  
  store   %A* %_v0, %A** %_v6  
  %_v7 = load   %A*, %A** %_v6  
  %_v8 = alloca %A* 
  store   %A* zeroinitializer, %A** %_v8  
  store   %A* %_v7, %A** %_v8  
  %_v13 = bitcast [1 x i8]* @_g0 to i8* 
  %_v9 =  call ccc  %A*  @_A_malloc()  
  %_v10 = getelementptr  %A, %A* %_v9, i32 0, i32 0 
  store   %_A_vtype* @_A_vdata, %_A_vtype** %_v10  
  %_v11 = getelementptr  %A, %A* %_v9, i32 0, i32 1 
  store   i32 0, i32* %_v11  
  %_v12 = getelementptr  %A, %A* %_v9, i32 0, i32 2 
  store   i32 0, i32* %_v12  
  %_v14 = getelementptr  %A, %A* %_v9, i32 0, i32 3 
  store   i8* %_v13, i8** %_v14  
  %_v15 = alloca %A* 
  store   %A* zeroinitializer, %A** %_v15  
  store   %A* %_v9, %A** %_v15  
  %_v20 = bitcast [1 x i8]* @_g0 to i8* 
  %_v16 =  call ccc  %A*  @_A_malloc()  
  %_v17 = getelementptr  %A, %A* %_v16, i32 0, i32 0 
  store   %_A_vtype* @_A_vdata, %_A_vtype** %_v17  
  %_v18 = getelementptr  %A, %A* %_v16, i32 0, i32 1 
  store   i32 0, i32* %_v18  
  %_v19 = getelementptr  %A, %A* %_v16, i32 0, i32 2 
  store   i32 0, i32* %_v19  
  %_v21 = getelementptr  %A, %A* %_v16, i32 0, i32 3 
  store   i8* %_v20, i8** %_v21  
  %_v22 = alloca %A* 
  store   %A* zeroinitializer, %A** %_v22  
  store   %A* %_v16, %A** %_v22  
  %_v27 = bitcast [1 x i8]* @_g0 to i8* 
  %_v23 =  call ccc  %A*  @_A_malloc()  
  %_v24 = getelementptr  %A, %A* %_v23, i32 0, i32 0 
  store   %_A_vtype* @_A_vdata, %_A_vtype** %_v24  
  %_v25 = getelementptr  %A, %A* %_v23, i32 0, i32 1 
  store   i32 0, i32* %_v25  
  %_v26 = getelementptr  %A, %A* %_v23, i32 0, i32 2 
  store   i32 0, i32* %_v26  
  %_v28 = getelementptr  %A, %A* %_v23, i32 0, i32 3 
  store   i8* %_v27, i8** %_v28  
  %_v29 = load   %A*, %A** %_v22  
  %_v30 = getelementptr  %A, %A* %_v29, i32 0, i32 1 
  %_v31 = load   i32, i32* %_v30  
  %_v32 = load   %A*, %A** %_v6  
  %_v33 = getelementptr  %A, %A* %_v32, i32 0, i32 3 
  %_v34 = load   i8*, i8** %_v33  
  %_v39 = bitcast [1 x i8]* @_g0 to i8* 
  %_v35 =  call ccc  %A*  @_A_malloc()  
  %_v36 = getelementptr  %A, %A* %_v35, i32 0, i32 0 
  store   %_A_vtype* @_A_vdata, %_A_vtype** %_v36  
  %_v37 = getelementptr  %A, %A* %_v35, i32 0, i32 1 
  store   i32 0, i32* %_v37  
  %_v38 = getelementptr  %A, %A* %_v35, i32 0, i32 2 
  store   i32 0, i32* %_v38  
  %_v40 = getelementptr  %A, %A* %_v35, i32 0, i32 3 
  store   i8* %_v39, i8** %_v40  
  %_v41 = getelementptr  %A, %A* %_v35, i32 0, i32 3 
  %_v42 = load   i8*, i8** %_v41  
   call ccc  void  @printString(i8*  %_v42)  
  ret i32 0 
}