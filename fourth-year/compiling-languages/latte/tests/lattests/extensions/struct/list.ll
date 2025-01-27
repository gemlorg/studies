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


%Stack = type {%_Stack_vtype*, %Node*, i32}


%_Stack_vtype = type {void (%Stack*, i32)*, i1 (%Stack*)*, i32 (%Stack*)*, void (%Stack*)*}


@_Stack_vdata =    global %_Stack_vtype { void (%Stack*, i32)* @_cf_Stack_push, i1 (%Stack*)* @_cf_Stack_isEmpty, i32 (%Stack*)* @_cf_Stack_top, void (%Stack*)* @_cf_Stack_pop }


define external ccc  %Stack* @_Stack_malloc()    {
entry:
  %0 = getelementptr inbounds %Stack, %Stack* zeroinitializer, i32 1 
  %1 = ptrtoint %Stack* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %Stack* 
  ret %Stack* %3 
}


define external ccc  void @_cf_Node_setElem(%Node*  %_this, i32  %c)    {
entry:
  %_v0 = alloca %Node* 
  store   %Node* %_this, %Node** %_v0  
  %_v2 = load   %Node*, %Node** %_v0  
  %_v1 = getelementptr  %Node, %Node* %_v2, i32 0, i32 1 
  store   i32 %c, i32* %_v1  
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


define external ccc  void @_cf_Stack_push(%Stack*  %_this, i32  %c)    {
entry:
  %_v0 = alloca %Stack* 
  store   %Stack* %_this, %Stack** %_v0  
  %_v2 = load   %Stack*, %Stack** %_v0  
  %_v1 = getelementptr  %Stack, %Stack* %_v2, i32 0, i32 2 
  %_v3 = load   i32, i32* %_v1  
  %_v4 = add   i32 %_v3, 1 
  %_v6 = load   %Stack*, %Stack** %_v0  
  %_v5 = getelementptr  %Stack, %Stack* %_v6, i32 0, i32 2 
  store   i32 %_v4, i32* %_v5  
  %_v7 =  call ccc  %Node*  @_Node_malloc()  
  %_v8 = getelementptr  %Node, %Node* %_v7, i32 0, i32 0 
  store   %_Node_vtype* @_Node_vdata, %_Node_vtype** %_v8  
  %_v9 = getelementptr  %Node, %Node* %_v7, i32 0, i32 1 
  store   i32 0, i32* %_v9  
  %_v10 = getelementptr  %Node, %Node* %_v7, i32 0, i32 2 
  store   %Node* zeroinitializer, %Node** %_v10  
  %_v11 = alloca %Node* 
  store   %Node* zeroinitializer, %Node** %_v11  
  store   %Node* %_v7, %Node** %_v11  
  %_v12 = load   %Node*, %Node** %_v11  
  %_v13 = getelementptr  %Node, %Node* %_v12, i32 0, i32 1 
  store   i32 %c, i32* %_v13  
  %_v14 = load   %Node*, %Node** %_v11  
  %_v16 = load   %Stack*, %Stack** %_v0  
  %_v15 = getelementptr  %Stack, %Stack* %_v16, i32 0, i32 1 
  %_v17 = load   %Node*, %Node** %_v15  
  %_v18 = getelementptr  %Node, %Node* %_v14, i32 0, i32 0 
  %_v19 = load   %_Node_vtype*, %_Node_vtype** %_v18  
  %_v20 = getelementptr  %_Node_vtype, %_Node_vtype* %_v19, i32 0, i32 1 
  %_v21 = load   void (%Node*, %Node*)*, void (%Node*, %Node*)** %_v20  
   call ccc  void  %_v21(%Node*  %_v14, %Node*  %_v17)  
  %_v23 = load   %Node*, %Node** %_v11  
  %_v25 = load   %Stack*, %Stack** %_v0  
  %_v24 = getelementptr  %Stack, %Stack* %_v25, i32 0, i32 1 
  store   %Node* %_v23, %Node** %_v24  
  ret void 
}


define external ccc  i1 @_cf_Stack_isEmpty(%Stack*  %_this)    {
entry:
  %_v0 = alloca %Stack* 
  store   %Stack* %_this, %Stack** %_v0  
  %_v2 = load   %Stack*, %Stack** %_v0  
  %_v1 = getelementptr  %Stack, %Stack* %_v2, i32 0, i32 2 
  %_v3 = load   i32, i32* %_v1  
  %_v4 = icmp eq i32 %_v3, 0 
  ret i1 %_v4 
}


define external ccc  i32 @_cf_Stack_top(%Stack*  %_this)    {
entry:
  %_v0 = alloca %Stack* 
  store   %Stack* %_this, %Stack** %_v0  
  %_v2 = load   %Stack*, %Stack** %_v0  
  %_v1 = getelementptr  %Stack, %Stack* %_v2, i32 0, i32 1 
  %_v3 = load   %Node*, %Node** %_v1  
  %_v4 = getelementptr  %Node, %Node* %_v3, i32 0, i32 0 
  %_v5 = load   %_Node_vtype*, %_Node_vtype** %_v4  
  %_v6 = getelementptr  %_Node_vtype, %_Node_vtype* %_v5, i32 0, i32 2 
  %_v7 = load   i32 (%Node*)*, i32 (%Node*)** %_v6  
  %_v8 =  call ccc  i32  %_v7(%Node*  %_v3)  
  ret i32 %_v8 
}


define external ccc  void @_cf_Stack_pop(%Stack*  %_this)    {
entry:
  %_v0 = alloca %Stack* 
  store   %Stack* %_this, %Stack** %_v0  
  %_v2 = load   %Stack*, %Stack** %_v0  
  %_v1 = getelementptr  %Stack, %Stack* %_v2, i32 0, i32 2 
  %_v3 = load   i32, i32* %_v1  
  %_v4 = sub   i32 %_v3, 1 
  %_v6 = load   %Stack*, %Stack** %_v0  
  %_v5 = getelementptr  %Stack, %Stack* %_v6, i32 0, i32 2 
  store   i32 %_v4, i32* %_v5  
  %_v8 = load   %Stack*, %Stack** %_v0  
  %_v7 = getelementptr  %Stack, %Stack* %_v8, i32 0, i32 1 
  %_v9 = load   %Node*, %Node** %_v7  
  %_v10 = getelementptr  %Node, %Node* %_v9, i32 0, i32 0 
  %_v11 = load   %_Node_vtype*, %_Node_vtype** %_v10  
  %_v12 = getelementptr  %_Node_vtype, %_Node_vtype* %_v11, i32 0, i32 3 
  %_v13 = load   %Node* (%Node*)*, %Node* (%Node*)** %_v12  
  %_v14 =  call ccc  %Node*  %_v13(%Node*  %_v9)  
  %_v16 = load   %Stack*, %Stack** %_v0  
  %_v15 = getelementptr  %Stack, %Stack* %_v16, i32 0, i32 1 
  store   %Node* %_v14, %Node** %_v15  
  ret void 
}


define external ccc  i32 @main()    {
entry:
  %_v0 =  call ccc  %Stack*  @_Stack_malloc()  
  %_v1 = getelementptr  %Stack, %Stack* %_v0, i32 0, i32 0 
  store   %_Stack_vtype* @_Stack_vdata, %_Stack_vtype** %_v1  
  %_v2 = getelementptr  %Stack, %Stack* %_v0, i32 0, i32 1 
  store   %Node* zeroinitializer, %Node** %_v2  
  %_v3 = getelementptr  %Stack, %Stack* %_v0, i32 0, i32 2 
  store   i32 0, i32* %_v3  
  %_v4 = alloca %Stack* 
  store   %Stack* zeroinitializer, %Stack** %_v4  
  store   %Stack* %_v0, %Stack** %_v4  
  br label %_B2 
_B1:
  %_v6 = load   %Stack*, %Stack** %_v4  
  %_v7 = getelementptr  %Stack, %Stack* %_v6, i32 0, i32 0 
  %_v8 = load   %_Stack_vtype*, %_Stack_vtype** %_v7  
  %_v9 = getelementptr  %_Stack_vtype, %_Stack_vtype* %_v8, i32 0, i32 0 
  %_v10 = load   void (%Stack*, i32)*, void (%Stack*, i32)** %_v9  
   call ccc  void  %_v10(%Stack*  %_v6, i32  %_v5)  
  %_v12 = add   i32 %_v5, 1 
  br label %_B2 
_B2:
  %_v5 = phi i32 [0, %entry], [%_v12, %_B1] 
  %_v13 = icmp slt i32 %_v5, 10 
  br i1 %_v13, label %_B1, label %_B3 
_B3:
  %_v14 = load   %Stack*, %Stack** %_v4  
  %_v15 = getelementptr  %Stack, %Stack* %_v14, i32 0, i32 2 
  %_v16 = load   i32, i32* %_v15  
   call ccc  void  @printInt(i32  %_v16)  
  %_v18 = load   %Stack*, %Stack** %_v4  
  %_v19 = getelementptr  %Stack, %Stack* %_v18, i32 0, i32 2 
  store   i32 15, i32* %_v19  
  %_v20 = load   %Stack*, %Stack** %_v4  
  %_v21 = getelementptr  %Stack, %Stack* %_v20, i32 0, i32 2 
  %_v22 = load   i32, i32* %_v21  
   call ccc  void  @printInt(i32  %_v22)  
  %_v24 = load   %Stack*, %Stack** %_v4  
  %_v25 = getelementptr  %Stack, %Stack* %_v24, i32 0, i32 1 
  %_v26 = load   %Node*, %Node** %_v25  
  %_v27 = getelementptr  %Node, %Node* %_v26, i32 0, i32 1 
  store   i32 10, i32* %_v27  
  %_v28 = load   %Stack*, %Stack** %_v4  
  %_v29 = getelementptr  %Stack, %Stack* %_v28, i32 0, i32 1 
  %_v30 = load   %Node*, %Node** %_v29  
  %_v31 = getelementptr  %Node, %Node* %_v30, i32 0, i32 1 
  %_v32 = load   i32, i32* %_v31  
   call ccc  void  @printInt(i32  %_v32)  
  ret i32 0 
}