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


@_g0 =    constant [12 x i8] c"I'm a shape\00"


@_g1 =    constant [17 x i8] c"I'm just a shape\00"


@_g2 =    constant [23 x i8] c"I'm really a rectangle\00"


@_g3 =    constant [20 x i8] c"I'm really a circle\00"


@_g4 =    constant [20 x i8] c"I'm really a square\00"


%_arr = type {i8*, i32}


define external ccc  %_arr* @__arr_malloc()    {
entry:
  %0 = getelementptr inbounds %_arr, %_arr* zeroinitializer, i32 1 
  %1 = ptrtoint %_arr* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %_arr* 
  ret %_arr* %3 
}


%Node = type {%_Node_vtype*, %Shape*, %Node*}


%_Node_vtype = type {void (%Node*, %Shape*)*, void (%Node*, %Node*)*, %Shape* (%Node*)*, %Node* (%Node*)*}


@_Node_vdata =    global %_Node_vtype { void (%Node*, %Shape*)* @_cf_Node_setElem, void (%Node*, %Node*)* @_cf_Node_setNext, %Shape* (%Node*)* @_cf_Node_getElem, %Node* (%Node*)* @_cf_Node_getNext }


define external ccc  %Node* @_Node_malloc()    {
entry:
  %0 = getelementptr inbounds %Node, %Node* zeroinitializer, i32 1 
  %1 = ptrtoint %Node* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %Node* 
  ret %Node* %3 
}


%Stack = type {%_Stack_vtype*, %Node*, %Node*, i32}


%_Stack_vtype = type {i1 (%Stack*)*, void (%Stack*, %Shape*)*, %Shape* (%Stack*)*, void (%Stack*)*, i32 (%Stack*)*}


@_Stack_vdata =    global %_Stack_vtype { i1 (%Stack*)* @_cf_Stack_isEmpty, void (%Stack*, %Shape*)* @_cf_Stack_push, %Shape* (%Stack*)* @_cf_Stack_top, void (%Stack*)* @_cf_Stack_pop, i32 (%Stack*)* @_cf_Stack_size }


define external ccc  %Stack* @_Stack_malloc()    {
entry:
  %0 = getelementptr inbounds %Stack, %Stack* zeroinitializer, i32 1 
  %1 = ptrtoint %Stack* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %Stack* 
  ret %Stack* %3 
}


%Shape = type {%_Shape_vtype*}


%_Shape_vtype = type {void (%Shape*)*, void (%Shape*)*}


@_Shape_vdata =    global %_Shape_vtype { void (%Shape*)* @_cf_Shape_tell, void (%Shape*)* @_cf_Shape_tellAgain }


define external ccc  %Shape* @_Shape_malloc()    {
entry:
  %0 = getelementptr inbounds %Shape, %Shape* zeroinitializer, i32 1 
  %1 = ptrtoint %Shape* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %Shape* 
  ret %Shape* %3 
}


%Rectangle = type {%_Rectangle_vtype*}


%_Rectangle_vtype = type {void (%Shape*)*, void (%Rectangle*)*}


@_Rectangle_vdata =    global %_Rectangle_vtype { void (%Shape*)* @_cf_Shape_tell, void (%Rectangle*)* @_cf_Rectangle_tellAgain }


define external ccc  %Rectangle* @_Rectangle_malloc()    {
entry:
  %0 = getelementptr inbounds %Rectangle, %Rectangle* zeroinitializer, i32 1 
  %1 = ptrtoint %Rectangle* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %Rectangle* 
  ret %Rectangle* %3 
}


%Circle = type {%_Circle_vtype*}


%_Circle_vtype = type {void (%Shape*)*, void (%Circle*)*}


@_Circle_vdata =    global %_Circle_vtype { void (%Shape*)* @_cf_Shape_tell, void (%Circle*)* @_cf_Circle_tellAgain }


define external ccc  %Circle* @_Circle_malloc()    {
entry:
  %0 = getelementptr inbounds %Circle, %Circle* zeroinitializer, i32 1 
  %1 = ptrtoint %Circle* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %Circle* 
  ret %Circle* %3 
}


%Square = type {%_Square_vtype*}


%_Square_vtype = type {void (%Shape*)*, void (%Square*)*}


@_Square_vdata =    global %_Square_vtype { void (%Shape*)* @_cf_Shape_tell, void (%Square*)* @_cf_Square_tellAgain }


define external ccc  %Square* @_Square_malloc()    {
entry:
  %0 = getelementptr inbounds %Square, %Square* zeroinitializer, i32 1 
  %1 = ptrtoint %Square* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %Square* 
  ret %Square* %3 
}


define external ccc  void @_cf_Node_setElem(%Node*  %_this, %Shape*  %c)    {
entry:
  %_v0 = alloca %Node* 
  store   %Node* %_this, %Node** %_v0  
  %_v1 = alloca %Shape* 
  store   %Shape* %c, %Shape** %_v1  
  %_v2 = load   %Shape*, %Shape** %_v1  
  %_v4 = load   %Node*, %Node** %_v0  
  %_v3 = getelementptr  %Node, %Node* %_v4, i32 0, i32 1 
  store   %Shape* %_v2, %Shape** %_v3  
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


define external ccc  %Shape* @_cf_Node_getElem(%Node*  %_this)    {
entry:
  %_v0 = alloca %Node* 
  store   %Node* %_this, %Node** %_v0  
  %_v2 = load   %Node*, %Node** %_v0  
  %_v1 = getelementptr  %Node, %Node* %_v2, i32 0, i32 1 
  %_v3 = load   %Shape*, %Shape** %_v1  
  ret %Shape* %_v3 
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


define external ccc  i1 @_cf_Stack_isEmpty(%Stack*  %_this)    {
entry:
  %_v0 = alloca %Stack* 
  store   %Stack* %_this, %Stack** %_v0  
  %_v2 = load   %Stack*, %Stack** %_v0  
  %_v1 = getelementptr  %Stack, %Stack* %_v2, i32 0, i32 3 
  %_v3 = load   i32, i32* %_v1  
  %_v4 = icmp eq i32 %_v3, 0 
  ret i1 %_v4 
}


define external ccc  void @_cf_Stack_push(%Stack*  %_this, %Shape*  %x)    {
entry:
  %_v0 = alloca %Stack* 
  store   %Stack* %_this, %Stack** %_v0  
  %_v1 = alloca %Shape* 
  store   %Shape* %x, %Shape** %_v1  
  %_v2 =  call ccc  %Node*  @_Node_malloc()  
  %_v3 = getelementptr  %Node, %Node* %_v2, i32 0, i32 0 
  store   %_Node_vtype* @_Node_vdata, %_Node_vtype** %_v3  
  %_v4 = getelementptr  %Node, %Node* %_v2, i32 0, i32 1 
  store   %Shape* zeroinitializer, %Shape** %_v4  
  %_v5 = getelementptr  %Node, %Node* %_v2, i32 0, i32 2 
  store   %Node* zeroinitializer, %Node** %_v5  
  %_v6 = alloca %Node* 
  store   %Node* zeroinitializer, %Node** %_v6  
  store   %Node* %_v2, %Node** %_v6  
  %_v7 = load   %Node*, %Node** %_v6  
  %_v8 = load   %Shape*, %Shape** %_v1  
  %_v9 = getelementptr  %Node, %Node* %_v7, i32 0, i32 0 
  %_v10 = load   %_Node_vtype*, %_Node_vtype** %_v9  
  %_v11 = getelementptr  %_Node_vtype, %_Node_vtype* %_v10, i32 0, i32 0 
  %_v12 = load   void (%Node*, %Shape*)*, void (%Node*, %Shape*)** %_v11  
   call ccc  void  %_v12(%Node*  %_v7, %Shape*  %_v8)  
  %_v15 = load   %Stack*, %Stack** %_v0  
  %_v14 = getelementptr  %Stack, %Stack* %_v15, i32 0, i32 3 
  %_v16 = load   i32, i32* %_v14  
  %_v17 = icmp eq i32 %_v16, 0 
  br i1 %_v17, label %_B1, label %_B2 
_B1:
  %_v18 = load   %Node*, %Node** %_v6  
  %_v20 = load   %Stack*, %Stack** %_v0  
  %_v19 = getelementptr  %Stack, %Stack* %_v20, i32 0, i32 1 
  store   %Node* %_v18, %Node** %_v19  
  br label %_B3 
_B2:
  %_v22 = load   %Stack*, %Stack** %_v0  
  %_v21 = getelementptr  %Stack, %Stack* %_v22, i32 0, i32 2 
  %_v23 = load   %Node*, %Node** %_v21  
  %_v24 = load   %Node*, %Node** %_v6  
  %_v25 = getelementptr  %Node, %Node* %_v23, i32 0, i32 0 
  %_v26 = load   %_Node_vtype*, %_Node_vtype** %_v25  
  %_v27 = getelementptr  %_Node_vtype, %_Node_vtype* %_v26, i32 0, i32 1 
  %_v28 = load   void (%Node*, %Node*)*, void (%Node*, %Node*)** %_v27  
   call ccc  void  %_v28(%Node*  %_v23, %Node*  %_v24)  
  br label %_B3 
_B3:
  %_v30 = load   %Node*, %Node** %_v6  
  %_v32 = load   %Stack*, %Stack** %_v0  
  %_v31 = getelementptr  %Stack, %Stack* %_v32, i32 0, i32 2 
  store   %Node* %_v30, %Node** %_v31  
  %_v34 = load   %Stack*, %Stack** %_v0  
  %_v33 = getelementptr  %Stack, %Stack* %_v34, i32 0, i32 3 
  %_v35 = load   i32, i32* %_v33  
  %_v36 = add   i32 %_v35, 1 
  %_v38 = load   %Stack*, %Stack** %_v0  
  %_v37 = getelementptr  %Stack, %Stack* %_v38, i32 0, i32 3 
  store   i32 %_v36, i32* %_v37  
  ret void 
}


define external ccc  %Shape* @_cf_Stack_top(%Stack*  %_this)    {
entry:
  %_v0 = alloca %Stack* 
  store   %Stack* %_this, %Stack** %_v0  
  %_v2 = load   %Stack*, %Stack** %_v0  
  %_v1 = getelementptr  %Stack, %Stack* %_v2, i32 0, i32 1 
  %_v3 = load   %Node*, %Node** %_v1  
  %_v4 = getelementptr  %Node, %Node* %_v3, i32 0, i32 0 
  %_v5 = load   %_Node_vtype*, %_Node_vtype** %_v4  
  %_v6 = getelementptr  %_Node_vtype, %_Node_vtype* %_v5, i32 0, i32 2 
  %_v7 = load   %Shape* (%Node*)*, %Shape* (%Node*)** %_v6  
  %_v8 =  call ccc  %Shape*  %_v7(%Node*  %_v3)  
  ret %Shape* %_v8 
}


define external ccc  void @_cf_Stack_pop(%Stack*  %_this)    {
entry:
  %_v0 = alloca %Stack* 
  store   %Stack* %_this, %Stack** %_v0  
  %_v2 = load   %Stack*, %Stack** %_v0  
  %_v1 = getelementptr  %Stack, %Stack* %_v2, i32 0, i32 3 
  %_v3 = load   i32, i32* %_v1  
  %_v4 = sub   i32 %_v3, 1 
  %_v6 = load   %Stack*, %Stack** %_v0  
  %_v5 = getelementptr  %Stack, %Stack* %_v6, i32 0, i32 3 
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


define external ccc  i32 @_cf_Stack_size(%Stack*  %_this)    {
entry:
  %_v0 = alloca %Stack* 
  store   %Stack* %_this, %Stack** %_v0  
  %_v2 = load   %Stack*, %Stack** %_v0  
  %_v1 = getelementptr  %Stack, %Stack* %_v2, i32 0, i32 3 
  %_v3 = load   i32, i32* %_v1  
  ret i32 %_v3 
}


define external ccc  void @_cf_Shape_tell(%Shape*  %_this)    {
entry:
  %_v0 = alloca %Shape* 
  store   %Shape* %_this, %Shape** %_v0  
  %_v1 = bitcast [12 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_v1)  
  ret void 
}


define external ccc  void @_cf_Shape_tellAgain(%Shape*  %_this)    {
entry:
  %_v0 = alloca %Shape* 
  store   %Shape* %_this, %Shape** %_v0  
  %_v1 = bitcast [17 x i8]* @_g1 to i8* 
   call ccc  void  @printString(i8*  %_v1)  
  ret void 
}


define external ccc  void @_cf_Rectangle_tellAgain(%Rectangle*  %_this)    {
entry:
  %_v0 = alloca %Rectangle* 
  store   %Rectangle* %_this, %Rectangle** %_v0  
  %_v1 = bitcast [23 x i8]* @_g2 to i8* 
   call ccc  void  @printString(i8*  %_v1)  
  ret void 
}


define external ccc  void @_cf_Circle_tellAgain(%Circle*  %_this)    {
entry:
  %_v0 = alloca %Circle* 
  store   %Circle* %_this, %Circle** %_v0  
  %_v1 = bitcast [20 x i8]* @_g3 to i8* 
   call ccc  void  @printString(i8*  %_v1)  
  ret void 
}


define external ccc  void @_cf_Square_tellAgain(%Square*  %_this)    {
entry:
  %_v0 = alloca %Square* 
  store   %Square* %_this, %Square** %_v0  
  %_v1 = bitcast [20 x i8]* @_g4 to i8* 
   call ccc  void  @printString(i8*  %_v1)  
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
  store   %Node* zeroinitializer, %Node** %_v3  
  %_v4 = getelementptr  %Stack, %Stack* %_v0, i32 0, i32 3 
  store   i32 0, i32* %_v4  
  %_v5 = alloca %Stack* 
  store   %Stack* zeroinitializer, %Stack** %_v5  
  store   %Stack* %_v0, %Stack** %_v5  
  %_v6 =  call ccc  %Shape*  @_Shape_malloc()  
  %_v7 = getelementptr  %Shape, %Shape* %_v6, i32 0, i32 0 
  store   %_Shape_vtype* @_Shape_vdata, %_Shape_vtype** %_v7  
  %_v8 = alloca %Shape* 
  store   %Shape* zeroinitializer, %Shape** %_v8  
  store   %Shape* %_v6, %Shape** %_v8  
  %_v9 = load   %Stack*, %Stack** %_v5  
  %_v10 = load   %Shape*, %Shape** %_v8  
  %_v11 = getelementptr  %Stack, %Stack* %_v9, i32 0, i32 0 
  %_v12 = load   %_Stack_vtype*, %_Stack_vtype** %_v11  
  %_v13 = getelementptr  %_Stack_vtype, %_Stack_vtype* %_v12, i32 0, i32 1 
  %_v14 = load   void (%Stack*, %Shape*)*, void (%Stack*, %Shape*)** %_v13  
   call ccc  void  %_v14(%Stack*  %_v9, %Shape*  %_v10)  
  %_v16 =  call ccc  %Rectangle*  @_Rectangle_malloc()  
  %_v17 = getelementptr  %Rectangle, %Rectangle* %_v16, i32 0, i32 0 
  store   %_Rectangle_vtype* @_Rectangle_vdata, %_Rectangle_vtype** %_v17  
  %_v18 = bitcast %Rectangle* %_v16 to %Shape* 
  store   %Shape* %_v18, %Shape** %_v8  
  %_v19 = load   %Stack*, %Stack** %_v5  
  %_v20 = load   %Shape*, %Shape** %_v8  
  %_v21 = getelementptr  %Stack, %Stack* %_v19, i32 0, i32 0 
  %_v22 = load   %_Stack_vtype*, %_Stack_vtype** %_v21  
  %_v23 = getelementptr  %_Stack_vtype, %_Stack_vtype* %_v22, i32 0, i32 1 
  %_v24 = load   void (%Stack*, %Shape*)*, void (%Stack*, %Shape*)** %_v23  
   call ccc  void  %_v24(%Stack*  %_v19, %Shape*  %_v20)  
  %_v26 =  call ccc  %Square*  @_Square_malloc()  
  %_v27 = getelementptr  %Square, %Square* %_v26, i32 0, i32 0 
  store   %_Square_vtype* @_Square_vdata, %_Square_vtype** %_v27  
  %_v28 = bitcast %Square* %_v26 to %Shape* 
  store   %Shape* %_v28, %Shape** %_v8  
  %_v29 = load   %Stack*, %Stack** %_v5  
  %_v30 = load   %Shape*, %Shape** %_v8  
  %_v31 = getelementptr  %Stack, %Stack* %_v29, i32 0, i32 0 
  %_v32 = load   %_Stack_vtype*, %_Stack_vtype** %_v31  
  %_v33 = getelementptr  %_Stack_vtype, %_Stack_vtype* %_v32, i32 0, i32 1 
  %_v34 = load   void (%Stack*, %Shape*)*, void (%Stack*, %Shape*)** %_v33  
   call ccc  void  %_v34(%Stack*  %_v29, %Shape*  %_v30)  
  %_v36 =  call ccc  %Circle*  @_Circle_malloc()  
  %_v37 = getelementptr  %Circle, %Circle* %_v36, i32 0, i32 0 
  store   %_Circle_vtype* @_Circle_vdata, %_Circle_vtype** %_v37  
  %_v38 = bitcast %Circle* %_v36 to %Shape* 
  store   %Shape* %_v38, %Shape** %_v8  
  %_v39 = load   %Stack*, %Stack** %_v5  
  %_v40 = load   %Shape*, %Shape** %_v8  
  %_v41 = getelementptr  %Stack, %Stack* %_v39, i32 0, i32 0 
  %_v42 = load   %_Stack_vtype*, %_Stack_vtype** %_v41  
  %_v43 = getelementptr  %_Stack_vtype, %_Stack_vtype* %_v42, i32 0, i32 1 
  %_v44 = load   void (%Stack*, %Shape*)*, void (%Stack*, %Shape*)** %_v43  
   call ccc  void  %_v44(%Stack*  %_v39, %Shape*  %_v40)  
  br label %_B2 
_B1:
  %_v46 = load   %Stack*, %Stack** %_v5  
  %_v47 = getelementptr  %Stack, %Stack* %_v46, i32 0, i32 0 
  %_v48 = load   %_Stack_vtype*, %_Stack_vtype** %_v47  
  %_v49 = getelementptr  %_Stack_vtype, %_Stack_vtype* %_v48, i32 0, i32 2 
  %_v50 = load   %Shape* (%Stack*)*, %Shape* (%Stack*)** %_v49  
  %_v51 =  call ccc  %Shape*  %_v50(%Stack*  %_v46)  
  store   %Shape* %_v51, %Shape** %_v8  
  %_v52 = load   %Shape*, %Shape** %_v8  
  %_v53 = getelementptr  %Shape, %Shape* %_v52, i32 0, i32 0 
  %_v54 = load   %_Shape_vtype*, %_Shape_vtype** %_v53  
  %_v55 = getelementptr  %_Shape_vtype, %_Shape_vtype* %_v54, i32 0, i32 0 
  %_v56 = load   void (%Shape*)*, void (%Shape*)** %_v55  
   call ccc  void  %_v56(%Shape*  %_v52)  
  %_v58 = load   %Shape*, %Shape** %_v8  
  %_v59 = getelementptr  %Shape, %Shape* %_v58, i32 0, i32 0 
  %_v60 = load   %_Shape_vtype*, %_Shape_vtype** %_v59  
  %_v61 = getelementptr  %_Shape_vtype, %_Shape_vtype* %_v60, i32 0, i32 1 
  %_v62 = load   void (%Shape*)*, void (%Shape*)** %_v61  
   call ccc  void  %_v62(%Shape*  %_v58)  
  %_v64 = load   %Stack*, %Stack** %_v5  
  %_v65 = getelementptr  %Stack, %Stack* %_v64, i32 0, i32 0 
  %_v66 = load   %_Stack_vtype*, %_Stack_vtype** %_v65  
  %_v67 = getelementptr  %_Stack_vtype, %_Stack_vtype* %_v66, i32 0, i32 3 
  %_v68 = load   void (%Stack*)*, void (%Stack*)** %_v67  
   call ccc  void  %_v68(%Stack*  %_v64)  
  br label %_B2 
_B2:
  %_v70 = load   %Stack*, %Stack** %_v5  
  %_v71 = getelementptr  %Stack, %Stack* %_v70, i32 0, i32 0 
  %_v72 = load   %_Stack_vtype*, %_Stack_vtype** %_v71  
  %_v73 = getelementptr  %_Stack_vtype, %_Stack_vtype* %_v72, i32 0, i32 0 
  %_v74 = load   i1 (%Stack*)*, i1 (%Stack*)** %_v73  
  %_v75 =  call ccc  i1  %_v74(%Stack*  %_v70)  
  %_v76 = xor i1 1, %_v75 
  br i1 %_v76, label %_B1, label %_B3 
_B3:
  ret i32 0 
}