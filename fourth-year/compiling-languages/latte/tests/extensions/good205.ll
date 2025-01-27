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
  %_v12 = load   %A*, %A** %_v11  
  %_v13 = getelementptr  %A, %A* %_v12, i32 0, i32 5 
  store   i32 10, i32* %_v13  
  br label %_B2 
_B1:
  %_v14 = load   %A*, %A** %_v11  
  %_v15 = getelementptr  %A, %A* %_v14, i32 0, i32 5 
  %_v16 = load   i32, i32* %_v15  
  %_v17 = sub   i32 %_v16, 1 
  %_v18 = load   %A*, %A** %_v11  
  %_v19 = getelementptr  %A, %A* %_v18, i32 0, i32 5 
  store   i32 %_v17, i32* %_v19  
  br label %_B2 
_B2:
  %_v20 = load   %A*, %A** %_v11  
  %_v21 = getelementptr  %A, %A* %_v20, i32 0, i32 5 
  %_v22 = load   i32, i32* %_v21  
  %_v23 = icmp sgt i32 %_v22, 0 
  br i1 %_v23, label %_B1, label %_B3 
_B3:
  ret i32 0 
}