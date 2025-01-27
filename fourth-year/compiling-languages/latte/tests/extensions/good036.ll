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


%A = type {%_A_vtype*, i32, i32, i32}


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
  %_v0 =  call ccc  %A*  @_A_malloc()  
  %_v1 = getelementptr  %A, %A* %_v0, i32 0, i32 0 
  store   %_A_vtype* @_A_vdata, %_A_vtype** %_v1  
  %_v2 = getelementptr  %A, %A* %_v0, i32 0, i32 1 
  store   i32 0, i32* %_v2  
  %_v3 = getelementptr  %A, %A* %_v0, i32 0, i32 2 
  store   i32 0, i32* %_v3  
  %_v4 = getelementptr  %A, %A* %_v0, i32 0, i32 3 
  store   i32 0, i32* %_v4  
  %_v5 = alloca %A* 
  store   %A* zeroinitializer, %A** %_v5  
  store   %A* %_v0, %A** %_v5  
  %_v6 = load   %A*, %A** %_v5  
  %_v7 = load   %A*, %A** %_v5  
  %_v8 = load   %A*, %A** %_v5  
  %_v9 = getelementptr  %A, %A* %_v8, i32 0, i32 1 
  store   i32 2, i32* %_v9  
  %_v10 = load   %A*, %A** %_v5  
  %_v11 = load   %A*, %A** %_v5  
  %_v12 = load   %A*, %A** %_v5  
  %_v13 = getelementptr  %A, %A* %_v12, i32 0, i32 1 
  %_v14 = load   i32, i32* %_v13  
   call ccc  void  @printInt(i32  %_v14)  
  br label %_B2 
_B1:
  %_v17 =  call ccc  %A*  @newA()  
  %_v18 =  call ccc  %A*  @newA()  
  store   %A* %_v18, %A** %_v5  
  %_v19 =  call ccc  %A*  @newA()  
  %_v20 = alloca %A* 
  store   %A* zeroinitializer, %A** %_v20  
  store   %A* %_v19, %A** %_v20  
  %_v21 = alloca %A* 
  store   %A* zeroinitializer, %A** %_v21  
  %_v22 = add   i32 %_v16, 1 
  br label %_B2 
_B2:
  %_v16 = phi i32 [0, %entry], [%_v22, %_B1] 
  %_v23 = icmp sle i32 %_v16, 1000 
  br i1 %_v23, label %_B1, label %_B3 
_B3:
  ret i32 0 
}


define external ccc  %A* @newA()    {
entry:
  %_v0 =  call ccc  %A*  @_A_malloc()  
  %_v1 = getelementptr  %A, %A* %_v0, i32 0, i32 0 
  store   %_A_vtype* @_A_vdata, %_A_vtype** %_v1  
  %_v2 = getelementptr  %A, %A* %_v0, i32 0, i32 1 
  store   i32 0, i32* %_v2  
  %_v3 = getelementptr  %A, %A* %_v0, i32 0, i32 2 
  store   i32 0, i32* %_v3  
  %_v4 = getelementptr  %A, %A* %_v0, i32 0, i32 3 
  store   i32 0, i32* %_v4  
  ret %A* %_v0 
}