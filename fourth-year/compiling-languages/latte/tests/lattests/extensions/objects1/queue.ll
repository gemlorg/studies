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


%Node = type {%_Node_vtype*, i32, %Node*}


%_Node_vtype = type {void (%Node*, i32)*, void (%Node*, %Node*)*, i32 (%Node*)*, %Node* (%Node*)*}


@_Node_vdata =    global %_Node_vtype { void (%Node*, i32)* @_cf_Node_setElem, void (%Node*, %Node*)* @_cf_Node_setNext, i32 (%Node*)* @_cf_Node_getElem, %Node* (%Node*)* @_cf_Node_getNext }


define external ccc  %Node* @_Node_malloc()    {
entry:
  %0 = getelementptr inbounds %Node, %Node* zeroinitializer, i32 1 
  %1 = ptrtoint %Node* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %Node* 
  ret %Node* %3 
}


%IntQueue = type {%_IntQueue_vtype*, %Node*, %Node*, i32}


%_IntQueue_vtype = type {i1 (%IntQueue*)*, void (%IntQueue*, i32)*, i32 (%IntQueue*)*, void (%IntQueue*)*, i32 (%IntQueue*)*}


@_IntQueue_vdata =    global %_IntQueue_vtype { i1 (%IntQueue*)* @_cf_IntQueue_isEmpty, void (%IntQueue*, i32)* @_cf_IntQueue_insert, i32 (%IntQueue*)* @_cf_IntQueue_first, void (%IntQueue*)* @_cf_IntQueue_rmFirst, i32 (%IntQueue*)* @_cf_IntQueue_size }


define external ccc  %IntQueue* @_IntQueue_malloc()    {
entry:
  %0 = getelementptr inbounds %IntQueue, %IntQueue* zeroinitializer, i32 1 
  %1 = ptrtoint %IntQueue* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %IntQueue* 
  ret %IntQueue* %3 
}


define external ccc  void @_cf_Node_setElem(%Node*  %_this, i32  %e)    {
entry:
  %_v0 = alloca %Node* 
  store   %Node* %_this, %Node** %_v0  
  %_v2 = load   %Node*, %Node** %_v0  
  %_v1 = getelementptr  %Node, %Node* %_v2, i32 0, i32 1 
  store   i32 %e, i32* %_v1  
  ret void 
}


define external ccc  void @_cf_Node_setNext(%Node*  %_this, %Node*  %n)    {
entry:
  %_v0 = alloca %Node* 
  store   %Node* %_this, %Node** %_v0  
  %_v1 = alloca %Node* 
  store   %Node* %n, %Node** %_v1  
  %_v2 = load   %Node*, %Node** %_v1  
  %_v4 = load   %Node*, %Node** %_v0  
  %_v3 = getelementptr  %Node, %Node* %_v4, i32 0, i32 2 
  store   %Node* %_v2, %Node** %_v3  
  ret void 
}


define external ccc  i32 @_cf_Node_getElem(%Node*  %_this)    {
entry:
  %_v0 = alloca %Node* 
  store   %Node* %_this, %Node** %_v0  
  %_v2 = load   %Node*, %Node** %_v0  
  %_v1 = getelementptr  %Node, %Node* %_v2, i32 0, i32 1 
  %_v3 = load   i32, i32* %_v1  
  ret i32 %_v3 
}


define external ccc  %Node* @_cf_Node_getNext(%Node*  %_this)    {
entry:
  %_v0 = alloca %Node* 
  store   %Node* %_this, %Node** %_v0  
  %_v2 = load   %Node*, %Node** %_v0  
  %_v1 = getelementptr  %Node, %Node* %_v2, i32 0, i32 2 
  %_v3 = load   %Node*, %Node** %_v1  
  ret %Node* %_v3 
}


define external ccc  i1 @_cf_IntQueue_isEmpty(%IntQueue*  %_this)    {
entry:
  %_v0 = alloca %IntQueue* 
  store   %IntQueue* %_this, %IntQueue** %_v0  
  %_v2 = load   %IntQueue*, %IntQueue** %_v0  
  %_v1 = getelementptr  %IntQueue, %IntQueue* %_v2, i32 0, i32 3 
  %_v3 = load   i32, i32* %_v1  
  %_v4 = icmp eq i32 %_v3, 0 
  ret i1 %_v4 
}


define external ccc  void @_cf_IntQueue_insert(%IntQueue*  %_this, i32  %x)    {
entry:
  %_v0 = alloca %IntQueue* 
  store   %IntQueue* %_this, %IntQueue** %_v0  
  %_v1 =  call ccc  %Node*  @_Node_malloc()  
  %_v2 = getelementptr  %Node, %Node* %_v1, i32 0, i32 0 
  store   %_Node_vtype* @_Node_vdata, %_Node_vtype** %_v2  
  %_v3 = getelementptr  %Node, %Node* %_v1, i32 0, i32 1 
  store   i32 0, i32* %_v3  
  %_v4 = getelementptr  %Node, %Node* %_v1, i32 0, i32 2 
  store   %Node* zeroinitializer, %Node** %_v4  
  %_v5 = alloca %Node* 
  store   %Node* zeroinitializer, %Node** %_v5  
  store   %Node* %_v1, %Node** %_v5  
  %_v6 = load   %Node*, %Node** %_v5  
  %_v7 = getelementptr  %Node, %Node* %_v6, i32 0, i32 0 
  %_v8 = load   %_Node_vtype*, %_Node_vtype** %_v7  
  %_v9 = getelementptr  %_Node_vtype, %_Node_vtype* %_v8, i32 0, i32 0 
  %_v10 = load   void (%Node*, i32)*, void (%Node*, i32)** %_v9  
   call ccc  void  %_v10(%Node*  %_v6, i32  %x)  
  %_v13 = load   %IntQueue*, %IntQueue** %_v0  
  %_v12 = getelementptr  %IntQueue, %IntQueue* %_v13, i32 0, i32 3 
  %_v14 = load   i32, i32* %_v12  
  %_v15 = icmp eq i32 %_v14, 0 
  br i1 %_v15, label %_B1, label %_B2 
_B1:
  %_v16 = load   %Node*, %Node** %_v5  
  %_v18 = load   %IntQueue*, %IntQueue** %_v0  
  %_v17 = getelementptr  %IntQueue, %IntQueue* %_v18, i32 0, i32 1 
  store   %Node* %_v16, %Node** %_v17  
  br label %_B3 
_B2:
  %_v20 = load   %IntQueue*, %IntQueue** %_v0  
  %_v19 = getelementptr  %IntQueue, %IntQueue* %_v20, i32 0, i32 2 
  %_v21 = load   %Node*, %Node** %_v19  
  %_v22 = load   %Node*, %Node** %_v5  
  %_v23 = getelementptr  %Node, %Node* %_v21, i32 0, i32 0 
  %_v24 = load   %_Node_vtype*, %_Node_vtype** %_v23  
  %_v25 = getelementptr  %_Node_vtype, %_Node_vtype* %_v24, i32 0, i32 1 
  %_v26 = load   void (%Node*, %Node*)*, void (%Node*, %Node*)** %_v25  
   call ccc  void  %_v26(%Node*  %_v21, %Node*  %_v22)  
  br label %_B3 
_B3:
  %_v28 = load   %Node*, %Node** %_v5  
  %_v30 = load   %IntQueue*, %IntQueue** %_v0  
  %_v29 = getelementptr  %IntQueue, %IntQueue* %_v30, i32 0, i32 2 
  store   %Node* %_v28, %Node** %_v29  
  %_v32 = load   %IntQueue*, %IntQueue** %_v0  
  %_v31 = getelementptr  %IntQueue, %IntQueue* %_v32, i32 0, i32 3 
  %_v33 = load   i32, i32* %_v31  
  %_v34 = add   i32 %_v33, 1 
  %_v36 = load   %IntQueue*, %IntQueue** %_v0  
  %_v35 = getelementptr  %IntQueue, %IntQueue* %_v36, i32 0, i32 3 
  store   i32 %_v34, i32* %_v35  
  ret void 
}


define external ccc  i32 @_cf_IntQueue_first(%IntQueue*  %_this)    {
entry:
  %_v0 = alloca %IntQueue* 
  store   %IntQueue* %_this, %IntQueue** %_v0  
  %_v2 = load   %IntQueue*, %IntQueue** %_v0  
  %_v1 = getelementptr  %IntQueue, %IntQueue* %_v2, i32 0, i32 1 
  %_v3 = load   %Node*, %Node** %_v1  
  %_v4 = getelementptr  %Node, %Node* %_v3, i32 0, i32 0 
  %_v5 = load   %_Node_vtype*, %_Node_vtype** %_v4  
  %_v6 = getelementptr  %_Node_vtype, %_Node_vtype* %_v5, i32 0, i32 2 
  %_v7 = load   i32 (%Node*)*, i32 (%Node*)** %_v6  
  %_v8 =  call ccc  i32  %_v7(%Node*  %_v3)  
  ret i32 %_v8 
}


define external ccc  void @_cf_IntQueue_rmFirst(%IntQueue*  %_this)    {
entry:
  %_v0 = alloca %IntQueue* 
  store   %IntQueue* %_this, %IntQueue** %_v0  
  %_v2 = load   %IntQueue*, %IntQueue** %_v0  
  %_v1 = getelementptr  %IntQueue, %IntQueue* %_v2, i32 0, i32 3 
  %_v3 = load   i32, i32* %_v1  
  %_v4 = sub   i32 %_v3, 1 
  %_v6 = load   %IntQueue*, %IntQueue** %_v0  
  %_v5 = getelementptr  %IntQueue, %IntQueue* %_v6, i32 0, i32 3 
  store   i32 %_v4, i32* %_v5  
  %_v8 = load   %IntQueue*, %IntQueue** %_v0  
  %_v7 = getelementptr  %IntQueue, %IntQueue* %_v8, i32 0, i32 1 
  %_v9 = load   %Node*, %Node** %_v7  
  %_v10 = getelementptr  %Node, %Node* %_v9, i32 0, i32 0 
  %_v11 = load   %_Node_vtype*, %_Node_vtype** %_v10  
  %_v12 = getelementptr  %_Node_vtype, %_Node_vtype* %_v11, i32 0, i32 3 
  %_v13 = load   %Node* (%Node*)*, %Node* (%Node*)** %_v12  
  %_v14 =  call ccc  %Node*  %_v13(%Node*  %_v9)  
  %_v16 = load   %IntQueue*, %IntQueue** %_v0  
  %_v15 = getelementptr  %IntQueue, %IntQueue* %_v16, i32 0, i32 1 
  store   %Node* %_v14, %Node** %_v15  
  ret void 
}


define external ccc  i32 @_cf_IntQueue_size(%IntQueue*  %_this)    {
entry:
  %_v0 = alloca %IntQueue* 
  store   %IntQueue* %_this, %IntQueue** %_v0  
  %_v2 = load   %IntQueue*, %IntQueue** %_v0  
  %_v1 = getelementptr  %IntQueue, %IntQueue* %_v2, i32 0, i32 3 
  %_v3 = load   i32, i32* %_v1  
  ret i32 %_v3 
}


define external ccc  i32 @f(i32  %x)    {
entry:
  %_v0 = mul   i32 %x, %x 
  %_v1 = add   i32 %_v0, 3 
  ret i32 %_v1 
}


define external ccc  i32 @main()    {
entry:
  %_v0 =  call ccc  %IntQueue*  @_IntQueue_malloc()  
  %_v1 = getelementptr  %IntQueue, %IntQueue* %_v0, i32 0, i32 0 
  store   %_IntQueue_vtype* @_IntQueue_vdata, %_IntQueue_vtype** %_v1  
  %_v2 = getelementptr  %IntQueue, %IntQueue* %_v0, i32 0, i32 1 
  store   %Node* zeroinitializer, %Node** %_v2  
  %_v3 = getelementptr  %IntQueue, %IntQueue* %_v0, i32 0, i32 2 
  store   %Node* zeroinitializer, %Node** %_v3  
  %_v4 = getelementptr  %IntQueue, %IntQueue* %_v0, i32 0, i32 3 
  store   i32 0, i32* %_v4  
  %_v5 = alloca %IntQueue* 
  store   %IntQueue* zeroinitializer, %IntQueue** %_v5  
  store   %IntQueue* %_v0, %IntQueue** %_v5  
  %_v6 = load   %IntQueue*, %IntQueue** %_v5  
  br label %IB_1 
IB_1:
  %_iv1 = mul   i32 3, 3 
  %_iv2 = add   i32 %_iv1, 3 
  br label %IB_2 
IB_2:
  %_v7 = bitcast i32 %_iv2 to i32 
  %_v8 = getelementptr  %IntQueue, %IntQueue* %_v6, i32 0, i32 0 
  %_v9 = load   %_IntQueue_vtype*, %_IntQueue_vtype** %_v8  
  %_v10 = getelementptr  %_IntQueue_vtype, %_IntQueue_vtype* %_v9, i32 0, i32 1 
  %_v11 = load   void (%IntQueue*, i32)*, void (%IntQueue*, i32)** %_v10  
   call ccc  void  %_v11(%IntQueue*  %_v6, i32  %_v7)  
  %_v13 = load   %IntQueue*, %IntQueue** %_v5  
  %_v14 = getelementptr  %IntQueue, %IntQueue* %_v13, i32 0, i32 0 
  %_v15 = load   %_IntQueue_vtype*, %_IntQueue_vtype** %_v14  
  %_v16 = getelementptr  %_IntQueue_vtype, %_IntQueue_vtype* %_v15, i32 0, i32 1 
  %_v17 = load   void (%IntQueue*, i32)*, void (%IntQueue*, i32)** %_v16  
   call ccc  void  %_v17(%IntQueue*  %_v13, i32  5)  
  %_v19 = load   %IntQueue*, %IntQueue** %_v5  
  %_v20 = getelementptr  %IntQueue, %IntQueue* %_v19, i32 0, i32 0 
  %_v21 = load   %_IntQueue_vtype*, %_IntQueue_vtype** %_v20  
  %_v22 = getelementptr  %_IntQueue_vtype, %_IntQueue_vtype* %_v21, i32 0, i32 2 
  %_v23 = load   i32 (%IntQueue*)*, i32 (%IntQueue*)** %_v22  
  %_v24 =  call ccc  i32  %_v23(%IntQueue*  %_v19)  
   call ccc  void  @printInt(i32  %_v24)  
  %_v26 = load   %IntQueue*, %IntQueue** %_v5  
  %_v27 = getelementptr  %IntQueue, %IntQueue* %_v26, i32 0, i32 0 
  %_v28 = load   %_IntQueue_vtype*, %_IntQueue_vtype** %_v27  
  %_v29 = getelementptr  %_IntQueue_vtype, %_IntQueue_vtype* %_v28, i32 0, i32 4 
  %_v30 = load   i32 (%IntQueue*)*, i32 (%IntQueue*)** %_v29  
  %_v31 =  call ccc  i32  %_v30(%IntQueue*  %_v26)  
   call ccc  void  @printInt(i32  %_v31)  
  %_v33 = load   %IntQueue*, %IntQueue** %_v5  
  %_v34 = getelementptr  %IntQueue, %IntQueue* %_v33, i32 0, i32 0 
  %_v35 = load   %_IntQueue_vtype*, %_IntQueue_vtype** %_v34  
  %_v36 = getelementptr  %_IntQueue_vtype, %_IntQueue_vtype* %_v35, i32 0, i32 3 
  %_v37 = load   void (%IntQueue*)*, void (%IntQueue*)** %_v36  
   call ccc  void  %_v37(%IntQueue*  %_v33)  
  %_v39 = load   %IntQueue*, %IntQueue** %_v5  
  %_v40 = getelementptr  %IntQueue, %IntQueue* %_v39, i32 0, i32 0 
  %_v41 = load   %_IntQueue_vtype*, %_IntQueue_vtype** %_v40  
  %_v42 = getelementptr  %_IntQueue_vtype, %_IntQueue_vtype* %_v41, i32 0, i32 2 
  %_v43 = load   i32 (%IntQueue*)*, i32 (%IntQueue*)** %_v42  
  %_v44 =  call ccc  i32  %_v43(%IntQueue*  %_v39)  
   call ccc  void  @printInt(i32  %_v44)  
  ret i32 0 
}