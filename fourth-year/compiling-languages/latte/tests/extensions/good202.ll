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


@_g1 =    constant [6 x i8] c"hello\00"


@_g2 =    constant [8 x i8] c" there!\00"


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
   call ccc  void  @printInt(i32  1)  
  %_v13 = bitcast [6 x i8]* @_g1 to i8* 
  %_v14 = load   %A*, %A** %_v11  
  %_v15 = getelementptr  %A, %A* %_v14, i32 0, i32 2 
  store   i8* %_v13, i8** %_v15  
   call ccc  void  @printInt(i32  2)  
  %_v17 = load   %A*, %A** %_v11  
  %_v18 = getelementptr  %A, %A* %_v17, i32 0, i32 2 
  %_v19 = load   i8*, i8** %_v18  
   call ccc  void  @printString(i8*  %_v19)  
  %_v21 = load   %A*, %A** %_v11  
  %_v22 = getelementptr  %A, %A* %_v21, i32 0, i32 2 
  %_v23 = load   i8*, i8** %_v22  
  %_v24 = bitcast [8 x i8]* @_g2 to i8* 
  %_v25 =  call ccc  i8*  @_concatStrings(i8*  %_v23, i8*  %_v24)  
  %_v26 = load   %A*, %A** %_v11  
  %_v27 = getelementptr  %A, %A* %_v26, i32 0, i32 4 
  store   i8* %_v25, i8** %_v27  
   call ccc  void  @printInt(i32  3)  
  %_v29 = load   %A*, %A** %_v11  
  %_v30 = getelementptr  %A, %A* %_v29, i32 0, i32 4 
  %_v31 = load   i8*, i8** %_v30  
   call ccc  void  @printString(i8*  %_v31)  
  %_v33 = load   %A*, %A** %_v11  
  %_v34 = getelementptr  %A, %A* %_v33, i32 0, i32 6 
  store   i1 1, i1* %_v34  
   call ccc  void  @printInt(i32  4)  
  %_v36 = load   %A*, %A** %_v11  
  %_v37 = getelementptr  %A, %A* %_v36, i32 0, i32 5 
  store   i32 42, i32* %_v37  
   call ccc  void  @printInt(i32  5)  
  ret i32 0 
}