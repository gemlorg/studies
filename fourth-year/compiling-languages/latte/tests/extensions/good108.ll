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


@_g0 =    constant [4 x i8] c"hej\00"


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
  %_v5 = bitcast [4 x i8]* @_g0 to i8* 
  br label %_B2 
_B1:
  %_v7 =  call ccc  %_arr*  @__arr_malloc()  
  %_v8 = mul   i32 8, 20 
  %_v9 =  call ccc  i8*  @_malloc(i32  %_v8)  
  %_v10 = getelementptr  %_arr, %_arr* %_v7, i32 0, i32 0 
  store   i8* %_v9, i8** %_v10  
  %_v11 = getelementptr  %_arr, %_arr* %_v7, i32 0, i32 1 
  store   i32 20, i32* %_v11  
  %_v12 =  call ccc  i8*  @_concatStrings(i8*  %_v5, i8*  %_v5)  
  %_v13 = getelementptr  %_arr, %_arr* %_v7, i32 0, i32 0 
  %_v14 = load   i8*, i8** %_v13  
  %_v15 = bitcast i8* %_v14 to i8** 
  %_v16 = getelementptr  i8*, i8** %_v15, i32 %_v6 
  store   i8* %_v12, i8** %_v16  
  %_v17 = getelementptr  %_arr, %_arr* %_v7, i32 0, i32 0 
  %_v18 = load   i8*, i8** %_v17  
  %_v19 = bitcast i8* %_v18 to i8** 
  %_v20 = getelementptr  i8*, i8** %_v19, i32 %_v6 
  %_v21 = load   i8*, i8** %_v20  
   call ccc  void  @printString(i8*  %_v21)  
  %_v23 = add   i32 %_v6, 2 
  br label %_B2 
_B2:
  %_v6 = phi i32 [1, %entry], [%_v23, %_B1] 
  %_v24 = icmp slt i32 %_v6, 20 
  br i1 %_v24, label %_B1, label %_B3 
_B3:
  %_v25 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 1 
  %_v26 = load   i32, i32* %_v25  
   call ccc  void  @printInt(i32  %_v26)  
  ret i32 0 
}