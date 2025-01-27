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
  %_v1 = mul   i32 4, 10 
  %_v2 =  call ccc  i8*  @_malloc(i32  %_v1)  
  %_v3 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  store   i8* %_v2, i8** %_v3  
  %_v4 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 1 
  store   i32 10, i32* %_v4  
  %_v5 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v6 = load   i8*, i8** %_v5  
  %_v7 = bitcast i8* %_v6 to i32* 
  %_v8 = getelementptr  i32, i32* %_v7, i32 0 
  %_v9 = load   i32, i32* %_v8  
   call ccc  void  @printInt(i32  %_v9)  
  %_v11 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v12 = load   i8*, i8** %_v11  
  %_v13 = bitcast i8* %_v12 to i32* 
  %_v14 = getelementptr  i32, i32* %_v13, i32 0 
  store   i32 10, i32* %_v14  
  %_v15 =  call ccc  %_arr*  @__arr_malloc()  
  %_v16 = mul   i32 4, 5 
  %_v17 =  call ccc  i8*  @_malloc(i32  %_v16)  
  %_v18 = getelementptr  %_arr, %_arr* %_v15, i32 0, i32 0 
  store   i8* %_v17, i8** %_v18  
  %_v19 = getelementptr  %_arr, %_arr* %_v15, i32 0, i32 1 
  store   i32 5, i32* %_v19  
  %_v20 = getelementptr  %_arr, %_arr* %_v15, i32 0, i32 0 
  %_v21 = load   i8*, i8** %_v20  
  %_v22 = bitcast i8* %_v21 to i32* 
  %_v23 = getelementptr  i32, i32* %_v22, i32 0 
  %_v24 = load   i32, i32* %_v23  
  ret i32 0 
}