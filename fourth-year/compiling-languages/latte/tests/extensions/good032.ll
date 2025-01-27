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


%A = type {%_A_vtype*, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32}


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
  br label %_B2 
_B1:
  %_v1 =  call ccc  %A*  @_A_malloc()  
  %_v2 = getelementptr  %A, %A* %_v1, i32 0, i32 0 
  store   %_A_vtype* @_A_vdata, %_A_vtype** %_v2  
  %_v3 = getelementptr  %A, %A* %_v1, i32 0, i32 1 
  store   i32 0, i32* %_v3  
  %_v4 = getelementptr  %A, %A* %_v1, i32 0, i32 2 
  store   i32 0, i32* %_v4  
  %_v5 = getelementptr  %A, %A* %_v1, i32 0, i32 3 
  store   i32 0, i32* %_v5  
  %_v6 = getelementptr  %A, %A* %_v1, i32 0, i32 4 
  store   i32 0, i32* %_v6  
  %_v7 = getelementptr  %A, %A* %_v1, i32 0, i32 5 
  store   i32 0, i32* %_v7  
  %_v8 = getelementptr  %A, %A* %_v1, i32 0, i32 6 
  store   i32 0, i32* %_v8  
  %_v9 = getelementptr  %A, %A* %_v1, i32 0, i32 7 
  store   i32 0, i32* %_v9  
  %_v10 = getelementptr  %A, %A* %_v1, i32 0, i32 8 
  store   i32 0, i32* %_v10  
  %_v11 = getelementptr  %A, %A* %_v1, i32 0, i32 9 
  store   i32 0, i32* %_v11  
  %_v12 = getelementptr  %A, %A* %_v1, i32 0, i32 10 
  store   i32 0, i32* %_v12  
  %_v13 = getelementptr  %A, %A* %_v1, i32 0, i32 11 
  store   i32 0, i32* %_v13  
  %_v14 = getelementptr  %A, %A* %_v1, i32 0, i32 12 
  store   i32 0, i32* %_v14  
  %_v15 = alloca %A* 
  store   %A* zeroinitializer, %A** %_v15  
  store   %A* %_v1, %A** %_v15  
  %_v16 = add   i32 %_v0, 1 
   call ccc  void  @printInt(i32  %_v16)  
  br label %_B2 
_B2:
  %_v0 = phi i32 [0, %entry], [%_v16, %_B1] 
  %_v18 = icmp slt i32 %_v0, 10000 
  br i1 %_v18, label %_B1, label %_B3 
_B3:
  ret i32 0 
}