@dnl          = internal constant [4 x i8]  c"%d\0A\00"
@runtimeError = internal constant [14 x i8] c"runtime error\00"
@getlineError = internal constant [22 x i8] c"error: getline failed\00"

%struct._IO_FILE = type opaque

@stdin = external dso_local global %struct._IO_FILE*

declare i32  @printf(i8*, ...)
declare i32  @scanf(i8*, ...)
declare i32  @puts(i8*)
declare i64  @getline(i8**, i64*, %struct._IO_FILE*)
declare void @exit(i32)
declare i64  @strlen(i8*)
declare i8*  @malloc(i64)
declare void @free(i8*)
declare i8*  @strcpy(i8*, i8*)
declare i8*  @strcat(i8*, i8*)

define void @printInt(i32 %x) {
    %format = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
    call i32 (i8*, ...) @printf(i8* %format, i32 %x)
    ret void
}

define void @printString(i8* %s) {
    call i32 @puts(i8* %s)
    ret void
}

define void @error() {
    %s = bitcast [14 x i8]* @runtimeError to i8*
    call i32 @puts(i8* %s)
    call void @exit(i32 1)
    unreachable
}

define i32 @readInt() {
    %res = alloca i32
    %format = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
    call i32 (i8*, ...) @scanf(i8* %format, i32* %res)
    %ret = load i32, i32* %res
    ret i32 %ret
}

define i8* @readString() {
    %line = alloca i8*
    store i8* null, i8** %line
    %n = alloca i64
    store i64 0, i64* %n
    %stdin = load %struct._IO_FILE*, %struct._IO_FILE** @stdin

    %read = call i64 @getline(i8** %line, i64* %n, %struct._IO_FILE* %stdin)
    %ret = load i8*, i8** %line
    %getlineErr = icmp eq i64 %read, -1
    br i1 %getlineErr, label %getlineFailure, label %getlineSuccess
getlineFailure:
    %s = bitcast [22 x i8]* @getlineError to i8*
    call i32 @puts(i8* %s)
    call void @free(i8* %ret)
    call void @exit(i32 1)
    unreachable
getlineSuccess:
    %newlineIndex = sub i64 %read, 1
    %newlinePointer = getelementptr inbounds i8, i8* %ret, i64 %newlineIndex
    store i8 0, i8* %newlinePointer
    ret i8* %ret
}

define i8* @_concatStrings(i8* %s1, i8* %s2) {
    %len1 = call i64 @strlen(i8* %s1)
    %len2 = call i64 @strlen(i8* %s2)
    %sum = add i64 %len1, %len2
    %reslen = add i64 %sum, 1
    %mem = call i8* @malloc(i64 %reslen)
    %s1copy = call i8* @strcpy(i8* %mem, i8* %s1)
    %res = call i8* @strcat(i8* %s1copy, i8* %s2)
    ret i8* %res
}