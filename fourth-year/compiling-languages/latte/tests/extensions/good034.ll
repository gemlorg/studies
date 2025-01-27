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


%B = type {%_B_vtype*}


%_B_vtype = type {void (%B*, %A*, i32)*}


@_B_vdata =    global %_B_vtype { void (%B*, %A*, i32)* @_cf_B_f }


define external ccc  %B* @_B_malloc()    {
entry:
  %0 = getelementptr inbounds %B, %B* zeroinitializer, i32 1 
  %1 = ptrtoint %B* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %B* 
  ret %B* %3 
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


define external ccc  void @_cf_B_f(%B*  %_this, %A*  %a, i32  %i)    {
entry:
  %_v0 = alloca %B* 
  store   %B* %_this, %B** %_v0  
  %_v1 = alloca %A* 
  store   %A* %a, %A** %_v1  
   call ccc  void  @printInt(i32  %i)  
  ret void 
}


define external ccc  i32 @main()    {
entry:
  %_v0 =  call ccc  %B*  @_B_malloc()  
  %_v1 = getelementptr  %B, %B* %_v0, i32 0, i32 0 
  store   %_B_vtype* @_B_vdata, %_B_vtype** %_v1  
  %_v2 = alloca %B* 
  store   %B* zeroinitializer, %B** %_v2  
  store   %B* %_v0, %B** %_v2  
  br label %_B2 
_B1:
  %_v4 = load   %B*, %B** %_v2  
  %_v5 =  call ccc  %A*  @_A_malloc()  
  %_v6 = getelementptr  %A, %A* %_v5, i32 0, i32 0 
  store   %_A_vtype* @_A_vdata, %_A_vtype** %_v6  
  %_v7 = getelementptr  %A, %A* %_v5, i32 0, i32 1 
  store   i32 0, i32* %_v7  
  %_v8 = getelementptr  %A, %A* %_v5, i32 0, i32 2 
  store   i32 0, i32* %_v8  
  %_v9 = getelementptr  %A, %A* %_v5, i32 0, i32 3 
  store   i32 0, i32* %_v9  
  %_v10 = getelementptr  %A, %A* %_v5, i32 0, i32 4 
  store   i32 0, i32* %_v10  
  %_v11 = getelementptr  %A, %A* %_v5, i32 0, i32 5 
  store   i32 0, i32* %_v11  
  %_v12 = getelementptr  %A, %A* %_v5, i32 0, i32 6 
  store   i32 0, i32* %_v12  
  %_v13 = getelementptr  %A, %A* %_v5, i32 0, i32 7 
  store   i32 0, i32* %_v13  
  %_v14 = getelementptr  %A, %A* %_v5, i32 0, i32 8 
  store   i32 0, i32* %_v14  
  %_v15 = getelementptr  %A, %A* %_v5, i32 0, i32 9 
  store   i32 0, i32* %_v15  
  %_v16 = getelementptr  %A, %A* %_v5, i32 0, i32 10 
  store   i32 0, i32* %_v16  
  %_v17 = getelementptr  %A, %A* %_v5, i32 0, i32 11 
  store   i32 0, i32* %_v17  
  %_v18 = getelementptr  %A, %A* %_v5, i32 0, i32 12 
  store   i32 0, i32* %_v18  
  %_v19 = getelementptr  %B, %B* %_v4, i32 0, i32 0 
  %_v20 = load   %_B_vtype*, %_B_vtype** %_v19  
  %_v21 = getelementptr  %_B_vtype, %_B_vtype* %_v20, i32 0, i32 0 
  %_v22 = load   void (%B*, %A*, i32)*, void (%B*, %A*, i32)** %_v21  
   call ccc  void  %_v22(%B*  %_v4, %A*  %_v5, i32  %_v3)  
  %_v24 = add   i32 %_v3, 1 
  br label %_B2 
_B2:
  %_v3 = phi i32 [0, %entry], [%_v24, %_B1] 
  %_v25 = icmp slt i32 %_v3, 10000 
  br i1 %_v25, label %_B1, label %_B3 
_B3:
  ret i32 0 
}