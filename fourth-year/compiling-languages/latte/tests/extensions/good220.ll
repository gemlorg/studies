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


@_g1 =    constant [4 x i8] c"hej\00"


@_g2 =    constant [5 x i8] c"hej2\00"


%_arr = type {i8*, i32}


define external ccc  %_arr* @__arr_malloc()    {
entry:
  %0 = getelementptr inbounds %_arr, %_arr* zeroinitializer, i32 1 
  %1 = ptrtoint %_arr* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %_arr* 
  ret %_arr* %3 
}


%A = type {%_A_vtype*, i32, i8*, %_arr*, i8*, i32, i1, %_arr*}


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
  %_v3 = bitcast [1 x i8]* @_g0 to i8* 
  %_v6 = bitcast [1 x i8]* @_g0 to i8* 
  %_v0 =  call ccc  %A*  @_A_malloc()  
  %_v1 = getelementptr  %A, %A* %_v0, i32 0, i32 0 
  store   %_A_vtype* @_A_vdata, %_A_vtype** %_v1  
  %_v2 = getelementptr  %A, %A* %_v0, i32 0, i32 1 
  store   i32 0, i32* %_v2  
  %_v4 = getelementptr  %A, %A* %_v0, i32 0, i32 2 
  store   i8* %_v3, i8** %_v4  
  %_v5 = getelementptr  %A, %A* %_v0, i32 0, i32 3 
  store   %_arr* zeroinitializer, %_arr** %_v5  
  %_v7 = getelementptr  %A, %A* %_v0, i32 0, i32 4 
  store   i8* %_v6, i8** %_v7  
  %_v8 = getelementptr  %A, %A* %_v0, i32 0, i32 5 
  store   i32 0, i32* %_v8  
  %_v9 = getelementptr  %A, %A* %_v0, i32 0, i32 6 
  store   i1 0, i1* %_v9  
  %_v10 = getelementptr  %A, %A* %_v0, i32 0, i32 7 
  store   %_arr* zeroinitializer, %_arr** %_v10  
  %_v11 = alloca %A* 
  store   %A* zeroinitializer, %A** %_v11  
  store   %A* %_v0, %A** %_v11  
  %_v12 = bitcast [4 x i8]* @_g1 to i8* 
  %_v13 = load   %A*, %A** %_v11  
  %_v14 = getelementptr  %A, %A* %_v13, i32 0, i32 2 
  store   i8* %_v12, i8** %_v14  
  %_v15 = load   %A*, %A** %_v11  
  %_v16 = getelementptr  %A, %A* %_v15, i32 0, i32 2 
  %_v17 = load   i8*, i8** %_v16  
  %_v18 = load   %A*, %A** %_v11  
  %_v19 = getelementptr  %A, %A* %_v18, i32 0, i32 2 
  %_v20 = load   i8*, i8** %_v19  
  %_v21 =  call ccc  i8*  @_concatStrings(i8*  %_v17, i8*  %_v20)  
  %_v22 = load   %A*, %A** %_v11  
  %_v23 = getelementptr  %A, %A* %_v22, i32 0, i32 2 
  store   i8* %_v21, i8** %_v23  
  %_v24 = load   %A*, %A** %_v11  
  %_v25 = getelementptr  %A, %A* %_v24, i32 0, i32 1 
  store   i32 5, i32* %_v25  
  %_v26 =  call ccc  %_arr*  @__arr_malloc()  
  %_v27 = mul   i32 4, 10 
  %_v28 =  call ccc  i8*  @_malloc(i32  %_v27)  
  %_v29 = getelementptr  %_arr, %_arr* %_v26, i32 0, i32 0 
  store   i8* %_v28, i8** %_v29  
  %_v30 = getelementptr  %_arr, %_arr* %_v26, i32 0, i32 1 
  store   i32 10, i32* %_v30  
  %_v31 = load   %A*, %A** %_v11  
  %_v32 = getelementptr  %A, %A* %_v31, i32 0, i32 3 
  store   %_arr* %_v26, %_arr** %_v32  
  %_v33 = bitcast [5 x i8]* @_g2 to i8* 
  %_v34 = load   %A*, %A** %_v11  
  %_v35 = getelementptr  %A, %A* %_v34, i32 0, i32 4 
  store   i8* %_v33, i8** %_v35  
  %_v36 = load   %A*, %A** %_v11  
  %_v37 = getelementptr  %A, %A* %_v36, i32 0, i32 4 
  %_v38 = load   i8*, i8** %_v37  
   call ccc  void  @printString(i8*  %_v38)  
  %_v40 = load   %A*, %A** %_v11  
  %_v41 = getelementptr  %A, %A* %_v40, i32 0, i32 3 
  %_v42 = load   %_arr*, %_arr** %_v41  
  %_v43 = getelementptr  %_arr, %_arr* %_v42, i32 0, i32 0 
  %_v44 = load   i8*, i8** %_v43  
  %_v45 = bitcast i8* %_v44 to i32* 
  %_v46 = getelementptr  i32, i32* %_v45, i32 3 
  store   i32 13, i32* %_v46  
  %_v47 = load   %A*, %A** %_v11  
  %_v48 = getelementptr  %A, %A* %_v47, i32 0, i32 3 
  %_v49 = load   %_arr*, %_arr** %_v48  
  %_v50 = getelementptr  %_arr, %_arr* %_v49, i32 0, i32 0 
  %_v51 = load   i8*, i8** %_v50  
  %_v52 = bitcast i8* %_v51 to i32* 
  %_v53 = getelementptr  i32, i32* %_v52, i32 3 
  %_v54 = load   i32, i32* %_v53  
   call ccc  void  @printInt(i32  %_v54)  
  ret i32 0 
}