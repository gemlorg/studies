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
  %_v8 = getelementptr  i32, i32* %_v7, i32 1 
  store   i32 4, i32* %_v8  
  %_v9 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v10 = load   i8*, i8** %_v9  
  %_v11 = bitcast i8* %_v10 to i32* 
  %_v12 = getelementptr  i32, i32* %_v11, i32 3 
  store   i32 2, i32* %_v12  
  %_v13 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v14 = load   i8*, i8** %_v13  
  %_v15 = bitcast i8* %_v14 to i32* 
  %_v16 = getelementptr  i32, i32* %_v15, i32 3 
  %_v17 = load   i32, i32* %_v16  
   call ccc  void  @printInt(i32  %_v17)  
  %_v19 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 1 
  %_v20 = load   i32, i32* %_v19  
   call ccc  void  @printInt(i32  %_v20)  
  ret i32 0 
}