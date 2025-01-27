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


@_g0 =    constant [2 x i8] c"a\00"


@_g1 =    constant [2 x i8] c"a\00"


%_arr = type {i8*, i32}


define external ccc  %_arr* @__arr_malloc()    {
entry:
  %0 = getelementptr inbounds %_arr, %_arr* zeroinitializer, i32 1 
  %1 = ptrtoint %_arr* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %_arr* 
  ret %_arr* %3 
}


define external ccc  i32 @main()    {
entry:
  %_v0 =  call ccc  %_arr*  @__arr_malloc()  
  %_v1 = mul   i32 8, 20 
  %_v2 =  call ccc  i8*  @_malloc(i32  %_v1)  
  %_v3 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  store   i8* %_v2, i8** %_v3  
  %_v4 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 1 
  store   i32 20, i32* %_v4  
  %_v5 = bitcast [2 x i8]* @_g0 to i8* 
  %_v6 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v7 = load   i8*, i8** %_v6  
  %_v8 = bitcast i8* %_v7 to i8** 
  %_v9 = getelementptr  i8*, i8** %_v8, i32 0 
  store   i8* %_v5, i8** %_v9  
  br label %_B2 
_B1:
  %_v11 = sub   i32 %_v10, 1 
  %_v12 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v13 = load   i8*, i8** %_v12  
  %_v14 = bitcast i8* %_v13 to i8** 
  %_v15 = getelementptr  i8*, i8** %_v14, i32 %_v11 
  %_v16 = load   i8*, i8** %_v15  
  %_v17 = bitcast [2 x i8]* @_g1 to i8* 
  %_v18 =  call ccc  i8*  @_concatStrings(i8*  %_v16, i8*  %_v17)  
  %_v19 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v20 = load   i8*, i8** %_v19  
  %_v21 = bitcast i8* %_v20 to i8** 
  %_v22 = getelementptr  i8*, i8** %_v21, i32 %_v10 
  store   i8* %_v18, i8** %_v22  
  %_v23 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v24 = load   i8*, i8** %_v23  
  %_v25 = bitcast i8* %_v24 to i8** 
  %_v26 = getelementptr  i8*, i8** %_v25, i32 %_v10 
  %_v27 = load   i8*, i8** %_v26  
   call ccc  void  @printString(i8*  %_v27)  
  %_v29 = add   i32 %_v10, 1 
  br label %_B2 
_B2:
  %_v10 = phi i32 [1, %entry], [%_v29, %_B1] 
  %_v30 = icmp slt i32 %_v10, 20 
  br i1 %_v30, label %_B1, label %_B3 
_B3:
  ret i32 0 
}