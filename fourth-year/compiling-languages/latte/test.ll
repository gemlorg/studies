; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


declare external ccc  i8* @_malloc(i32)    


%Stack = type {%_Stack_vtype*}


%_Stack_vtype = type {}


@_Stack_vdata =    global %_Stack_vtype {  }


define external ccc  %Stack* @_Stack_malloc()    {
entry:
  %0 = getelementptr inbounds %Stack, %Stack* zeroinitializer, i32 1 
  %1 = ptrtoint %Stack* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %Stack* 
  ret %Stack* %3 
}


define external ccc  i32 @main()    {
entry:
  %_v1 = alloca %Stack* 
  store   %Stack* zeroinitializer, %Stack** %_v1  
  %_v2 =  call ccc  %Stack*  @_Stack_malloc()  
  %_v3 = getelementptr  %Stack, %Stack* %_v2, i32 0, i32 0 
  store   %_Stack_vtype* @_Stack_vdata, %_Stack_vtype** %_v3  
  store   %Stack* %_v2, %Stack** %_v1  
  %_v4 = alloca %Stack* 
  store   %Stack* zeroinitializer, %Stack** %_v4  
  %_v5 =  call ccc  %Stack*  @_Stack_malloc()  
  %_v6 = getelementptr  %Stack, %Stack* %_v5, i32 0, i32 0 
  store   %_Stack_vtype* @_Stack_vdata, %_Stack_vtype** %_v6  
  store   %Stack* %_v5, %Stack** %_v4  
  %_v7 = load   %Stack*, %Stack** %_v4  
  store   %Stack* %_v7, %Stack** %_v1  
  ret i32 0 
}