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


@_g0 =    constant [5 x i8] c"ciao\00"


%_arr = type {i8*, i32}


define external ccc  %_arr* @__arr_malloc()    {
entry:
  %0 = getelementptr inbounds %_arr, %_arr* zeroinitializer, i32 1 
  %1 = ptrtoint %_arr* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %_arr* 
  ret %_arr* %3 
}


%A = type {%_A_vtype*, %_arr*}


%_A_vtype = type {void (%A*)*}


@_A_vdata =    global %_A_vtype { void (%A*)* @_cf_A_setArr }


define external ccc  %A* @_A_malloc()    {
entry:
  %0 = getelementptr inbounds %A, %A* zeroinitializer, i32 1 
  %1 = ptrtoint %A* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %A* 
  ret %A* %3 
}


define external ccc  void @_cf_A_setArr(%A*  %_this)    {
entry:
  %_v0 = alloca %A* 
  store   %A* %_this, %A** %_v0  
  %_v1 = bitcast [5 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_v1)  
  %_v3 =  call ccc  %_arr*  @__arr_malloc()  
  %_v4 = mul   i32 4, 10000 
  %_v5 =  call ccc  i8*  @_malloc(i32  %_v4)  
  %_v6 = getelementptr  %_arr, %_arr* %_v3, i32 0, i32 0 
  store   i8* %_v5, i8** %_v6  
  %_v7 = getelementptr  %_arr, %_arr* %_v3, i32 0, i32 1 
  store   i32 10000, i32* %_v7  
  %_v8 = load   %A*, %A** %_v0  
  %_v9 = getelementptr  %A, %A* %_v8, i32 0, i32 1 
  store   %_arr* %_v3, %_arr** %_v9  
  ret void 
}


define external ccc  i32 @main()    {
entry:
  %_v0 = alloca %A* 
  store   %A* zeroinitializer, %A** %_v0  
  br label %_B2 
_B1:
  %_v2 = add   i32 %_v1, 1 
  %_v3 =  call ccc  %A*  @_A_malloc()  
  %_v4 = getelementptr  %A, %A* %_v3, i32 0, i32 0 
  store   %_A_vtype* @_A_vdata, %_A_vtype** %_v4  
  %_v5 = getelementptr  %A, %A* %_v3, i32 0, i32 1 
  store   %_arr* zeroinitializer, %_arr** %_v5  
  store   %A* %_v3, %A** %_v0  
  %_v6 = load   %A*, %A** %_v0  
  %_v7 = getelementptr  %A, %A* %_v6, i32 0, i32 0 
  %_v8 = load   %_A_vtype*, %_A_vtype** %_v7  
  %_v9 = getelementptr  %_A_vtype, %_A_vtype* %_v8, i32 0, i32 0 
  %_v10 = load   void (%A*)*, void (%A*)** %_v9  
   call ccc  void  %_v10(%A*  %_v6)  
  br label %_B2 
_B2:
  %_v1 = phi i32 [0, %entry], [%_v2, %_B1] 
  %_v12 = icmp slt i32 %_v1, 100 
  br i1 %_v12, label %_B1, label %_B3 
_B3:
  ret i32 0 
}