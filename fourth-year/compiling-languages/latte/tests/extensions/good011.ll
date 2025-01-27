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


define external ccc  i32 @main()    {
entry:
  %_v0 =  call ccc  %_arr*  @__arr_malloc()  
  %_v1 = mul   i32 4, 32 
  %_v2 =  call ccc  i8*  @_malloc(i32  %_v1)  
  %_v3 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  store   i8* %_v2, i8** %_v3  
  %_v4 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 1 
  store   i32 32, i32* %_v4  
  %_v5 =  call ccc  %_arr*  @__arr_malloc()  
  %_v6 = mul   i32 4, 32 
  %_v7 =  call ccc  i8*  @_malloc(i32  %_v6)  
  %_v8 = getelementptr  %_arr, %_arr* %_v5, i32 0, i32 0 
  store   i8* %_v7, i8** %_v8  
  %_v9 = getelementptr  %_arr, %_arr* %_v5, i32 0, i32 1 
  store   i32 32, i32* %_v9  
  %_v10 =  call ccc  %_arr*  @__arr_malloc()  
  %_v11 = mul   i32 4, 32 
  %_v12 =  call ccc  i8*  @_malloc(i32  %_v11)  
  %_v13 = getelementptr  %_arr, %_arr* %_v10, i32 0, i32 0 
  store   i8* %_v12, i8** %_v13  
  %_v14 = getelementptr  %_arr, %_arr* %_v10, i32 0, i32 1 
  store   i32 32, i32* %_v14  
  %_v15 =  call ccc  %_arr*  @__arr_malloc()  
  %_v16 = mul   i32 4, 32 
  %_v17 =  call ccc  i8*  @_malloc(i32  %_v16)  
  %_v18 = getelementptr  %_arr, %_arr* %_v15, i32 0, i32 0 
  store   i8* %_v17, i8** %_v18  
  %_v19 = getelementptr  %_arr, %_arr* %_v15, i32 0, i32 1 
  store   i32 32, i32* %_v19  
  %_v20 =  call ccc  %_arr*  @__arr_malloc()  
  %_v21 = mul   i32 4, 32 
  %_v22 =  call ccc  i8*  @_malloc(i32  %_v21)  
  %_v23 = getelementptr  %_arr, %_arr* %_v20, i32 0, i32 0 
  store   i8* %_v22, i8** %_v23  
  %_v24 = getelementptr  %_arr, %_arr* %_v20, i32 0, i32 1 
  store   i32 32, i32* %_v24  
  ret i32 0 
}