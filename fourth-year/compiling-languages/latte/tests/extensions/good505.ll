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


@_g0 =    constant [5 x i8] c"hejo\00"


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


%A = type {%_A_vtype*, i32, i32, i8*, i8*}


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
  %_v1 = bitcast [5 x i8]* @_g0 to i8* 
  %_v3 = load   %A*, %A** %_v0  
  %_v2 = getelementptr  %A, %A* %_v3, i32 0, i32 3 
  store   i8* %_v1, i8** %_v2  
  %_v5 = load   %A*, %A** %_v0  
  %_v4 = getelementptr  %A, %A* %_v5, i32 0, i32 3 
  %_v6 = load   i8*, i8** %_v4  
  %_v8 = load   %A*, %A** %_v0  
  %_v7 = getelementptr  %A, %A* %_v8, i32 0, i32 3 
  %_v9 = load   i8*, i8** %_v7  
  %_v10 =  call ccc  i8*  @_concatStrings(i8*  %_v6, i8*  %_v9)  
  %_v12 = load   %A*, %A** %_v0  
  %_v11 = getelementptr  %A, %A* %_v12, i32 0, i32 3 
  store   i8* %_v10, i8** %_v11  
   call ccc  void  @printInt(i32  102)  
  %_v15 = load   %A*, %A** %_v0  
  %_v14 = getelementptr  %A, %A* %_v15, i32 0, i32 1 
  %_v16 = load   i32, i32* %_v14  
   call ccc  void  @printInt(i32  %_v16)  
  ret void 
}


define external ccc  i32 @main()    {
entry:
  %_v0 =  call ccc  %A*  @newA()  
  %_v1 = getelementptr  %A, %A* %_v0, i32 0, i32 1 
  store   i32 5, i32* %_v1  
  ret i32 0 
}


define external ccc  %A* @newA()    {
entry:
  %_v4 = bitcast [1 x i8]* @_g1 to i8* 
  %_v6 = bitcast [1 x i8]* @_g1 to i8* 
  %_v0 =  call ccc  %A*  @_A_malloc()  
  %_v1 = getelementptr  %A, %A* %_v0, i32 0, i32 0 
  store   %_A_vtype* @_A_vdata, %_A_vtype** %_v1  
  %_v2 = getelementptr  %A, %A* %_v0, i32 0, i32 1 
  store   i32 0, i32* %_v2  
  %_v3 = getelementptr  %A, %A* %_v0, i32 0, i32 2 
  store   i32 0, i32* %_v3  
  %_v5 = getelementptr  %A, %A* %_v0, i32 0, i32 3 
  store   i8* %_v4, i8** %_v5  
  %_v7 = getelementptr  %A, %A* %_v0, i32 0, i32 4 
  store   i8* %_v6, i8** %_v7  
  %_v8 = alloca %A* 
  store   %A* zeroinitializer, %A** %_v8  
  store   %A* %_v0, %A** %_v8  
  %_v9 = load   %A*, %A** %_v8  
  %_v10 = getelementptr  %A, %A* %_v9, i32 0, i32 1 
  store   i32 42, i32* %_v10  
   call ccc  void  @printInt(i32  101)  
  %_v12 = load   %A*, %A** %_v8  
  ret %A* %_v12 
}