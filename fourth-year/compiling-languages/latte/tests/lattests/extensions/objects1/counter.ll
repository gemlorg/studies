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


%Node = type {%_Node_vtype*}


%_Node_vtype = type {}


@_Node_vdata =    global %_Node_vtype {  }


define external ccc  %Node* @_Node_malloc()    {
entry:
  %0 = getelementptr inbounds %Node, %Node* zeroinitializer, i32 1 
  %1 = ptrtoint %Node* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %Node* 
  ret %Node* %3 
}


%Counter = type {%_Counter_vtype*, i32}


%_Counter_vtype = type {void (%Counter*)*, i32 (%Counter*)*}


@_Counter_vdata =    global %_Counter_vtype { void (%Counter*)* @_cf_Counter_incr, i32 (%Counter*)* @_cf_Counter_value }


define external ccc  %Counter* @_Counter_malloc()    {
entry:
  %0 = getelementptr inbounds %Counter, %Counter* zeroinitializer, i32 1 
  %1 = ptrtoint %Counter* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %Counter* 
  ret %Counter* %3 
}


define external ccc  void @_cf_Counter_incr(%Counter*  %_this)    {
entry:
  %_v0 = alloca %Counter* 
  store   %Counter* %_this, %Counter** %_v0  
  %_v2 = load   %Counter*, %Counter** %_v0  
  %_v1 = getelementptr  %Counter, %Counter* %_v2, i32 0, i32 1 
  %_v3 = load   i32, i32* %_v1  
  %_v4 = add   i32 %_v3, 1 
  %_v6 = load   %Counter*, %Counter** %_v0  
  %_v5 = getelementptr  %Counter, %Counter* %_v6, i32 0, i32 1 
  store   i32 %_v4, i32* %_v5  
  ret void 
}


define external ccc  i32 @_cf_Counter_value(%Counter*  %_this)    {
entry:
  %_v0 = alloca %Counter* 
  store   %Counter* %_this, %Counter** %_v0  
  %_v2 = load   %Counter*, %Counter** %_v0  
  %_v1 = getelementptr  %Counter, %Counter* %_v2, i32 0, i32 1 
  %_v3 = load   i32, i32* %_v1  
  ret i32 %_v3 
}


define external ccc  i32 @main()    {
entry:
  %_v0 = alloca %Counter* 
  store   %Counter* zeroinitializer, %Counter** %_v0  
  %_v1 =  call ccc  %Counter*  @_Counter_malloc()  
  %_v2 = getelementptr  %Counter, %Counter* %_v1, i32 0, i32 0 
  store   %_Counter_vtype* @_Counter_vdata, %_Counter_vtype** %_v2  
  %_v3 = getelementptr  %Counter, %Counter* %_v1, i32 0, i32 1 
  store   i32 0, i32* %_v3  
  store   %Counter* %_v1, %Counter** %_v0  
  %_v4 = load   %Counter*, %Counter** %_v0  
  %_v5 = getelementptr  %Counter, %Counter* %_v4, i32 0, i32 0 
  %_v6 = load   %_Counter_vtype*, %_Counter_vtype** %_v5  
  %_v7 = getelementptr  %_Counter_vtype, %_Counter_vtype* %_v6, i32 0, i32 0 
  %_v8 = load   void (%Counter*)*, void (%Counter*)** %_v7  
   call ccc  void  %_v8(%Counter*  %_v4)  
  %_v10 = load   %Counter*, %Counter** %_v0  
  %_v11 = getelementptr  %Counter, %Counter* %_v10, i32 0, i32 0 
  %_v12 = load   %_Counter_vtype*, %_Counter_vtype** %_v11  
  %_v13 = getelementptr  %_Counter_vtype, %_Counter_vtype* %_v12, i32 0, i32 0 
  %_v14 = load   void (%Counter*)*, void (%Counter*)** %_v13  
   call ccc  void  %_v14(%Counter*  %_v10)  
  %_v16 = load   %Counter*, %Counter** %_v0  
  %_v17 = getelementptr  %Counter, %Counter* %_v16, i32 0, i32 0 
  %_v18 = load   %_Counter_vtype*, %_Counter_vtype** %_v17  
  %_v19 = getelementptr  %_Counter_vtype, %_Counter_vtype* %_v18, i32 0, i32 0 
  %_v20 = load   void (%Counter*)*, void (%Counter*)** %_v19  
   call ccc  void  %_v20(%Counter*  %_v16)  
  %_v22 = load   %Counter*, %Counter** %_v0  
  %_v23 = getelementptr  %Counter, %Counter* %_v22, i32 0, i32 0 
  %_v24 = load   %_Counter_vtype*, %_Counter_vtype** %_v23  
  %_v25 = getelementptr  %_Counter_vtype, %_Counter_vtype* %_v24, i32 0, i32 1 
  %_v26 = load   i32 (%Counter*)*, i32 (%Counter*)** %_v25  
  %_v27 =  call ccc  i32  %_v26(%Counter*  %_v22)  
   call ccc  void  @printInt(i32  %_v27)  
  ret i32 0 
}