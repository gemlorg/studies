; ModuleID = 'runtime.c'
source_filename = "runtime.c"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

%struct.__sFILE = type { i8*, i32, i32, i16, i16, %struct.__sbuf, i32, i8*, i32 (i8*)*, i32 (i8*, i8*, i32)*, i64 (i8*, i64, i32)*, i32 (i8*, i8*, i32)*, %struct.__sbuf, %struct.__sFILEX*, i32, [3 x i8], [1 x i8], %struct.__sbuf, i32, i64 }
%struct.__sFILEX = type opaque
%struct.__sbuf = type { i8*, i32 }

@__stderrp = external global %struct.__sFILE*, align 8
@.str = private unnamed_addr constant [15 x i8] c"runtime error\0A\00", align 1
@.str.1 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@.str.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@__stdinp = external global %struct.__sFILE*, align 8
@.str.3 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@.str.4 = private unnamed_addr constant [1 x i8] zeroinitializer, align 1

; Function Attrs: noinline nounwind optnone ssp uwtable
define void @error() #0 {
  %1 = load %struct.__sFILE*, %struct.__sFILE** @__stderrp, align 8
  %2 = call i32 (%struct.__sFILE*, i8*, ...) @fprintf(%struct.__sFILE* %1, i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str, i64 0, i64 0))
  call void @exit(i32 1) #7
  unreachable
}

declare i32 @fprintf(%struct.__sFILE*, i8*, ...) #1

; Function Attrs: noreturn
declare void @exit(i32) #2

; Function Attrs: noinline nounwind optnone ssp uwtable
define void @printString(i8* %0) #0 {
  %2 = alloca i8*, align 8
  store i8* %0, i8** %2, align 8
  %3 = load i8*, i8** %2, align 8
  %4 = icmp eq i8* %3, null
  br i1 %4, label %5, label %7

5:                                                ; preds = %1
  %6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.1, i64 0, i64 0))
  br label %10

7:                                                ; preds = %1
  %8 = load i8*, i8** %2, align 8
  %9 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.2, i64 0, i64 0), i8* %8)
  br label %10

10:                                               ; preds = %7, %5
  ret void
}

declare i32 @printf(i8*, ...) #1

; Function Attrs: noinline nounwind optnone ssp uwtable
define i8* @__read_string() #0 {
  %1 = alloca i8*, align 8
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i8*, align 8
  %6 = alloca i64, align 8
  %7 = alloca i8*, align 8
  store i64 8, i64* %2, align 8
  store i64 4, i64* %3, align 8
  store i64 0, i64* %4, align 8
  store i8* null, i8** %5, align 8
  %8 = load %struct.__sFILE*, %struct.__sFILE** @__stdinp, align 8
  %9 = call i64 @getline(i8** %5, i64* %4, %struct.__sFILE* %8)
  store i64 %9, i64* %6, align 8
  %10 = load i64, i64* %6, align 8
  %11 = icmp ule i64 %10, 0
  br i1 %11, label %12, label %13

12:                                               ; preds = %0
  store i8* null, i8** %1, align 8
  br label %32

13:                                               ; preds = %0
  %14 = load i8*, i8** %5, align 8
  %15 = load i64, i64* %6, align 8
  %16 = sub i64 %15, 1
  %17 = getelementptr inbounds i8, i8* %14, i64 %16
  store i8 0, i8* %17, align 1
  %18 = load i64, i64* %6, align 8
  %19 = add i64 8, %18
  %20 = call align 16 i8* @calloc(i64 %19, i64 4) #8
  store i8* %20, i8** %7, align 8
  %21 = load i8*, i8** %7, align 8
  %22 = getelementptr inbounds i8, i8* %21, i64 8
  %23 = load i8*, i8** %5, align 8
  %24 = load i64, i64* %6, align 8
  %25 = load i8*, i8** %7, align 8
  %26 = getelementptr inbounds i8, i8* %25, i64 8
  %27 = call i64 @llvm.objectsize.i64.p0i8(i8* %26, i1 false, i1 true, i1 false)
  %28 = call i8* @__memcpy_chk(i8* %22, i8* %23, i64 %24, i64 %27) #9
  %29 = load i8*, i8** %5, align 8
  call void @free(i8* %29)
  %30 = load i8*, i8** %7, align 8
  %31 = getelementptr inbounds i8, i8* %30, i64 8
  store i8* %31, i8** %1, align 8
  br label %32

32:                                               ; preds = %13, %12
  %33 = load i8*, i8** %1, align 8
  ret i8* %33
}

declare i64 @getline(i8**, i64*, %struct.__sFILE*) #1

; Function Attrs: allocsize(0,1)
declare align 16 i8* @calloc(i64, i64) #3

; Function Attrs: nounwind
declare i8* @__memcpy_chk(i8*, i8*, i64, i64) #4

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.objectsize.i64.p0i8(i8*, i1 immarg, i1 immarg, i1 immarg) #5

declare void @free(i8*) #1

; Function Attrs: noinline nounwind optnone ssp uwtable
define i8* @readString() #0 {
  %1 = alloca i8*, align 8
  %2 = alloca i8*, align 8
  %3 = alloca i64, align 8
  %4 = alloca i8*, align 8
  %5 = call i8* @__read_string()
  store i8* %5, i8** %2, align 8
  %6 = load i8*, i8** %2, align 8
  %7 = icmp eq i8* %6, null
  br i1 %7, label %8, label %9

8:                                                ; preds = %0
  store i8* null, i8** %1, align 8
  br label %29

9:                                                ; preds = %0
  %10 = load i8*, i8** %2, align 8
  %11 = call i64 @strlen(i8* %10)
  store i64 %11, i64* %3, align 8
  %12 = load i64, i64* %3, align 8
  %13 = add i64 %12, 1
  %14 = call align 16 i8* @malloc(i64 %13) #10
  store i8* %14, i8** %4, align 8
  %15 = load i8*, i8** %4, align 8
  %16 = icmp eq i8* %15, null
  br i1 %16, label %17, label %20

17:                                               ; preds = %9
  %18 = load i8*, i8** %2, align 8
  %19 = getelementptr inbounds i8, i8* %18, i64 -8
  call void @free(i8* %19)
  store i8* null, i8** %1, align 8
  br label %29

20:                                               ; preds = %9
  %21 = load i8*, i8** %4, align 8
  %22 = load i8*, i8** %2, align 8
  %23 = load i8*, i8** %4, align 8
  %24 = call i64 @llvm.objectsize.i64.p0i8(i8* %23, i1 false, i1 true, i1 false)
  %25 = call i8* @__strcpy_chk(i8* %21, i8* %22, i64 %24) #9
  %26 = load i8*, i8** %2, align 8
  %27 = getelementptr inbounds i8, i8* %26, i64 -8
  call void @free(i8* %27)
  %28 = load i8*, i8** %4, align 8
  store i8* %28, i8** %1, align 8
  br label %29

29:                                               ; preds = %20, %17, %8
  %30 = load i8*, i8** %1, align 8
  ret i8* %30
}

declare i64 @strlen(i8*) #1

; Function Attrs: allocsize(0)
declare align 16 i8* @malloc(i64) #6

; Function Attrs: nounwind
declare i8* @__strcpy_chk(i8*, i8*, i64) #4

; Function Attrs: noinline nounwind optnone ssp uwtable
define void @printInt(i32 %0) #0 {
  %2 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  %3 = load i32, i32* %2, align 4
  %4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.3, i64 0, i64 0), i32 %3)
  ret void
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define i8* @__read_int_helper() #0 {
  %1 = alloca i8*, align 8
  %2 = alloca i64, align 8
  %3 = alloca i8*, align 8
  %4 = alloca i64, align 8
  store i64 0, i64* %2, align 8
  store i8* null, i8** %3, align 8
  %5 = load %struct.__sFILE*, %struct.__sFILE** @__stdinp, align 8
  %6 = call i64 @getline(i8** %3, i64* %2, %struct.__sFILE* %5)
  store i64 %6, i64* %4, align 8
  %7 = load i64, i64* %4, align 8
  %8 = icmp ule i64 %7, 0
  br i1 %8, label %9, label %10

9:                                                ; preds = %0
  store i8* null, i8** %1, align 8
  br label %16

10:                                               ; preds = %0
  %11 = load i8*, i8** %3, align 8
  %12 = load i64, i64* %4, align 8
  %13 = sub i64 %12, 1
  %14 = getelementptr inbounds i8, i8* %11, i64 %13
  store i8 0, i8* %14, align 1
  %15 = load i8*, i8** %3, align 8
  store i8* %15, i8** %1, align 8
  br label %16

16:                                               ; preds = %10, %9
  %17 = load i8*, i8** %1, align 8
  ret i8* %17
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @readInt() #0 {
  %1 = alloca i8*, align 8
  %2 = alloca i32, align 4
  %3 = call i8* @__read_int_helper()
  store i8* %3, i8** %1, align 8
  %4 = load i8*, i8** %1, align 8
  %5 = call i32 @atoi(i8* %4)
  store i32 %5, i32* %2, align 4
  %6 = load i8*, i8** %1, align 8
  call void @free(i8* %6)
  %7 = load i32, i32* %2, align 4
  ret i32 %7
}

declare i32 @atoi(i8*) #1

; Function Attrs: noinline nounwind optnone ssp uwtable
define i8* @_concatStrings(i8* %0, i8* %1) #0 {
  %3 = alloca i8*, align 8
  %4 = alloca i8*, align 8
  %5 = alloca i8*, align 8
  %6 = alloca i64, align 8
  %7 = alloca i64, align 8
  %8 = alloca i8*, align 8
  %9 = alloca i64, align 8
  %10 = alloca i8*, align 8
  %11 = alloca i64, align 8
  %12 = alloca i64, align 8
  %13 = alloca i8*, align 8
  store i8* %0, i8** %4, align 8
  store i8* %1, i8** %5, align 8
  store i64 8, i64* %6, align 8
  %14 = load i8*, i8** %4, align 8
  %15 = icmp eq i8* %14, null
  br i1 %15, label %16, label %20

16:                                               ; preds = %2
  %17 = load i8*, i8** %5, align 8
  %18 = icmp eq i8* %17, null
  br i1 %18, label %19, label %20

19:                                               ; preds = %16
  store i8* null, i8** %3, align 8
  br label %96

20:                                               ; preds = %16, %2
  %21 = load i8*, i8** %4, align 8
  %22 = icmp eq i8* %21, null
  br i1 %22, label %23, label %41

23:                                               ; preds = %20
  %24 = load i8*, i8** %5, align 8
  %25 = call i64 @strlen(i8* %24)
  store i64 %25, i64* %7, align 8
  %26 = load i64, i64* %7, align 8
  %27 = add i64 %26, 1
  %28 = add i64 %27, 8
  %29 = call align 16 i8* @malloc(i64 %28) #10
  store i8* %29, i8** %8, align 8
  %30 = load i8*, i8** %8, align 8
  %31 = getelementptr inbounds i8, i8* %30, i64 8
  %32 = load i8*, i8** %5, align 8
  %33 = load i64, i64* %7, align 8
  %34 = add i64 %33, 1
  %35 = load i8*, i8** %8, align 8
  %36 = getelementptr inbounds i8, i8* %35, i64 8
  %37 = call i64 @llvm.objectsize.i64.p0i8(i8* %36, i1 false, i1 true, i1 false)
  %38 = call i8* @__memcpy_chk(i8* %31, i8* %32, i64 %34, i64 %37) #9
  %39 = load i8*, i8** %8, align 8
  %40 = getelementptr inbounds i8, i8* %39, i64 8
  store i8* %40, i8** %3, align 8
  br label %96

41:                                               ; preds = %20
  %42 = load i8*, i8** %5, align 8
  %43 = icmp eq i8* %42, null
  br i1 %43, label %44, label %62

44:                                               ; preds = %41
  %45 = load i8*, i8** %4, align 8
  %46 = call i64 @strlen(i8* %45)
  store i64 %46, i64* %9, align 8
  %47 = load i64, i64* %9, align 8
  %48 = add i64 %47, 1
  %49 = add i64 %48, 8
  %50 = call align 16 i8* @malloc(i64 %49) #10
  store i8* %50, i8** %10, align 8
  %51 = load i8*, i8** %10, align 8
  %52 = getelementptr inbounds i8, i8* %51, i64 8
  %53 = load i8*, i8** %4, align 8
  %54 = load i64, i64* %9, align 8
  %55 = add i64 %54, 1
  %56 = load i8*, i8** %10, align 8
  %57 = getelementptr inbounds i8, i8* %56, i64 8
  %58 = call i64 @llvm.objectsize.i64.p0i8(i8* %57, i1 false, i1 true, i1 false)
  %59 = call i8* @__memcpy_chk(i8* %52, i8* %53, i64 %55, i64 %58) #9
  %60 = load i8*, i8** %10, align 8
  %61 = getelementptr inbounds i8, i8* %60, i64 8
  store i8* %61, i8** %3, align 8
  br label %96

62:                                               ; preds = %41
  %63 = load i8*, i8** %4, align 8
  %64 = call i64 @strlen(i8* %63)
  store i64 %64, i64* %11, align 8
  %65 = load i8*, i8** %5, align 8
  %66 = call i64 @strlen(i8* %65)
  store i64 %66, i64* %12, align 8
  %67 = load i64, i64* %11, align 8
  %68 = load i64, i64* %12, align 8
  %69 = add i64 %67, %68
  %70 = add i64 %69, 1
  %71 = add i64 %70, 8
  %72 = call align 16 i8* @malloc(i64 %71) #10
  store i8* %72, i8** %13, align 8
  %73 = load i8*, i8** %13, align 8
  %74 = getelementptr inbounds i8, i8* %73, i64 8
  %75 = load i8*, i8** %4, align 8
  %76 = load i64, i64* %11, align 8
  %77 = load i8*, i8** %13, align 8
  %78 = getelementptr inbounds i8, i8* %77, i64 8
  %79 = call i64 @llvm.objectsize.i64.p0i8(i8* %78, i1 false, i1 true, i1 false)
  %80 = call i8* @__memcpy_chk(i8* %74, i8* %75, i64 %76, i64 %79) #9
  %81 = load i8*, i8** %13, align 8
  %82 = getelementptr inbounds i8, i8* %81, i64 8
  %83 = load i64, i64* %11, align 8
  %84 = getelementptr inbounds i8, i8* %82, i64 %83
  %85 = load i8*, i8** %5, align 8
  %86 = load i64, i64* %12, align 8
  %87 = add i64 %86, 1
  %88 = load i8*, i8** %13, align 8
  %89 = getelementptr inbounds i8, i8* %88, i64 8
  %90 = load i64, i64* %11, align 8
  %91 = getelementptr inbounds i8, i8* %89, i64 %90
  %92 = call i64 @llvm.objectsize.i64.p0i8(i8* %91, i1 false, i1 true, i1 false)
  %93 = call i8* @__memcpy_chk(i8* %84, i8* %85, i64 %87, i64 %92) #9
  %94 = load i8*, i8** %13, align 8
  %95 = getelementptr inbounds i8, i8* %94, i64 8
  store i8* %95, i8** %3, align 8
  br label %96

96:                                               ; preds = %62, %44, %23, %19
  %97 = load i8*, i8** %3, align 8
  ret i8* %97
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define i8* @__rstrconcat(i8* %0, i8* %1) #0 {
  %3 = alloca i8*, align 8
  %4 = alloca i8*, align 8
  store i8* %0, i8** %3, align 8
  store i8* %1, i8** %4, align 8
  %5 = load i8*, i8** %4, align 8
  %6 = load i8*, i8** %3, align 8
  %7 = call i8* @_concatStrings(i8* %5, i8* %6)
  ret i8* %7
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @_compareStrings(i8* %0, i8* %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i8*, align 8
  %5 = alloca i8*, align 8
  %6 = alloca i8*, align 8
  store i8* %0, i8** %4, align 8
  store i8* %1, i8** %5, align 8
  store i8* getelementptr inbounds ([1 x i8], [1 x i8]* @.str.4, i64 0, i64 0), i8** %6, align 8
  %7 = load i8*, i8** %4, align 8
  %8 = icmp eq i8* %7, null
  br i1 %8, label %14, label %9

9:                                                ; preds = %2
  %10 = load i8*, i8** %4, align 8
  %11 = load i8*, i8** %6, align 8
  %12 = call i32 @strcmp(i8* %10, i8* %11)
  %13 = icmp eq i32 %12, 0
  br i1 %13, label %14, label %23

14:                                               ; preds = %9, %2
  %15 = load i8*, i8** %5, align 8
  %16 = icmp eq i8* %15, null
  br i1 %16, label %22, label %17

17:                                               ; preds = %14
  %18 = load i8*, i8** %5, align 8
  %19 = load i8*, i8** %6, align 8
  %20 = call i32 @strcmp(i8* %18, i8* %19)
  %21 = icmp eq i32 %20, 0
  br i1 %21, label %22, label %23

22:                                               ; preds = %17, %14
  store i32 1, i32* %3, align 4
  br label %37

23:                                               ; preds = %17, %9
  %24 = load i8*, i8** %4, align 8
  %25 = icmp eq i8* %24, null
  br i1 %25, label %29, label %26

26:                                               ; preds = %23
  %27 = load i8*, i8** %5, align 8
  %28 = icmp eq i8* %27, null
  br i1 %28, label %29, label %30

29:                                               ; preds = %26, %23
  store i32 0, i32* %3, align 4
  br label %37

30:                                               ; preds = %26
  br label %31

31:                                               ; preds = %30
  %32 = load i8*, i8** %4, align 8
  %33 = load i8*, i8** %5, align 8
  %34 = call i32 @strcmp(i8* %32, i8* %33)
  %35 = icmp eq i32 %34, 0
  %36 = zext i1 %35 to i32
  store i32 %36, i32* %3, align 4
  br label %37

37:                                               ; preds = %31, %29, %22
  %38 = load i32, i32* %3, align 4
  ret i32 %38
}

declare i32 @strcmp(i8*, i8*) #1

; Function Attrs: noinline nounwind optnone ssp uwtable
define void @__incr_ref_counter(i8* %0) #0 {
  %2 = alloca i8*, align 8
  %3 = alloca i32*, align 8
  %4 = alloca i32*, align 8
  store i8* %0, i8** %2, align 8
  %5 = load i8*, i8** %2, align 8
  %6 = icmp ne i8* %5, null
  br i1 %6, label %7, label %15

7:                                                ; preds = %1
  %8 = load i8*, i8** %2, align 8
  %9 = bitcast i8* %8 to i32*
  store i32* %9, i32** %3, align 8
  %10 = load i32*, i32** %3, align 8
  %11 = getelementptr inbounds i32, i32* %10, i64 -1
  store i32* %11, i32** %4, align 8
  %12 = load i32*, i32** %4, align 8
  %13 = load i32, i32* %12, align 4
  %14 = add nsw i32 %13, 1
  store i32 %14, i32* %12, align 4
  br label %15

15:                                               ; preds = %7, %1
  ret void
}

attributes #0 = { noinline nounwind optnone ssp uwtable "frame-pointer"="non-leaf" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+v8.5a,+zcm,+zcz" }
attributes #1 = { "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+v8.5a,+zcm,+zcz" }
attributes #2 = { noreturn "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+v8.5a,+zcm,+zcz" }
attributes #3 = { allocsize(0,1) "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+v8.5a,+zcm,+zcz" }
attributes #4 = { nounwind "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+v8.5a,+zcm,+zcz" }
attributes #5 = { nofree nosync nounwind readnone speculatable willreturn }
attributes #6 = { allocsize(0) "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+v8.5a,+zcm,+zcz" }
attributes #7 = { noreturn }
attributes #8 = { allocsize(0,1) }
attributes #9 = { nounwind }
attributes #10 = { allocsize(0) }

!llvm.module.flags = !{!0, !1, !2, !3, !4, !5, !6, !7}
!llvm.ident = !{!8}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 1, !"branch-target-enforcement", i32 0}
!2 = !{i32 1, !"sign-return-address", i32 0}
!3 = !{i32 1, !"sign-return-address-all", i32 0}
!4 = !{i32 1, !"sign-return-address-with-bkey", i32 0}
!5 = !{i32 7, !"PIC Level", i32 2}
!6 = !{i32 7, !"uwtable", i32 1}
!7 = !{i32 7, !"frame-pointer", i32 1}
!8 = !{!"Homebrew clang version 13.0.1"}
