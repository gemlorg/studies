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


%_arr = type {i8*, i32}


define external ccc  %_arr* @__arr_malloc()    {
entry:
  %0 = getelementptr inbounds %_arr, %_arr* zeroinitializer, i32 1 
  %1 = ptrtoint %_arr* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %_arr* 
  ret %_arr* %3 
}


%A = type {%_A_vtype*, i32, i32}


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
  %_v0 =  call ccc  %_arr*  @__arr_malloc()  
  %_v1 = mul   i32 8, 10 
  %_v2 =  call ccc  i8*  @_malloc(i32  %_v1)  
  %_v3 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  store   i8* %_v2, i8** %_v3  
  %_v4 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 1 
  store   i32 10, i32* %_v4  
  %_v5 =  call ccc  %A*  @_A_malloc()  
  %_v6 = getelementptr  %A, %A* %_v5, i32 0, i32 0 
  store   %_A_vtype* @_A_vdata, %_A_vtype** %_v6  
  %_v7 = getelementptr  %A, %A* %_v5, i32 0, i32 1 
  store   i32 0, i32* %_v7  
  %_v8 = getelementptr  %A, %A* %_v5, i32 0, i32 2 
  store   i32 0, i32* %_v8  
  %_v9 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v10 = load   i8*, i8** %_v9  
  %_v11 = bitcast i8* %_v10 to %A** 
  %_v12 = getelementptr  %A*, %A** %_v11, i32 0 
  store   %A* %_v5, %A** %_v12  
  %_v13 =  call ccc  %A*  @_A_malloc()  
  %_v14 = getelementptr  %A, %A* %_v13, i32 0, i32 0 
  store   %_A_vtype* @_A_vdata, %_A_vtype** %_v14  
  %_v15 = getelementptr  %A, %A* %_v13, i32 0, i32 1 
  store   i32 0, i32* %_v15  
  %_v16 = getelementptr  %A, %A* %_v13, i32 0, i32 2 
  store   i32 0, i32* %_v16  
  %_v17 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v18 = load   i8*, i8** %_v17  
  %_v19 = bitcast i8* %_v18 to %A** 
  %_v20 = getelementptr  %A*, %A** %_v19, i32 3 
  store   %A* %_v13, %A** %_v20  
  %_v21 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v22 = load   i8*, i8** %_v21  
  %_v23 = bitcast i8* %_v22 to %A** 
  %_v24 = getelementptr  %A*, %A** %_v23, i32 3 
  %_v25 = load   %A*, %A** %_v24  
  %_v26 = getelementptr  %A, %A* %_v25, i32 0, i32 1 
  store   i32 13, i32* %_v26  
  %_v27 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v28 = load   i8*, i8** %_v27  
  %_v29 = bitcast i8* %_v28 to %A** 
  %_v30 = getelementptr  %A*, %A** %_v29, i32 3 
  %_v31 = load   %A*, %A** %_v30  
  %_v32 = getelementptr  %A, %A* %_v31, i32 0, i32 1 
  %_v33 = load   i32, i32* %_v32  
   call ccc  void  @printInt(i32  %_v33)  
  ret i32 0 
}