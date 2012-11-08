; ModuleID = 'simple_race.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%union.pthread_attr_t = type { i64, [48 x i8] }

@Global = common global i32 0, align 4
@.str = private unnamed_addr constant [11 x i8] c"self-write\00", section "llvm.metadata"
@.str1 = private unnamed_addr constant [14 x i8] c"simple_race.c\00", section "llvm.metadata"
@.str2 = private unnamed_addr constant [4 x i8] c"foo\00", section "llvm.metadata"
@llvm.global.annotations = appending global [2 x { i8*, i8*, i8*, i32 }] [{ i8*, i8*, i8*, i32 } { i8* bitcast (i8* (i8*)* @Thread1 to i8*), i8* getelementptr inbounds ([11 x i8]* @.str, i32 0, i32 0), i8* getelementptr inbounds ([14 x i8]* @.str1, i32 0, i32 0), i32 7 }, { i8*, i8*, i8*, i32 } { i8* bitcast (i8* (i8*)* @Thread2 to i8*), i8* getelementptr inbounds ([11 x i8]* @.str, i32 0, i32 0), i8* getelementptr inbounds ([14 x i8]* @.str1, i32 0, i32 0), i32 12 }], section "llvm.metadata"

define i8* @Thread1(i8* %x) nounwind uwtable {
entry:
  %x.addr = alloca i8*, align 8
  store i8* %x, i8** %x.addr, align 8
  store i32 42, i32* @Global, align 4
  ret i8* null
}

define i8* @Thread2(i8* %x) nounwind uwtable {
entry:
  %x.addr = alloca i8*, align 8
  store i8* %x, i8** %x.addr, align 8
  %x.addr1 = bitcast i8** %x.addr to i8*
  call void @llvm.var.annotation(i8* %x.addr1, i8* getelementptr inbounds ([4 x i8]* @.str2, i32 0, i32 0), i8* getelementptr inbounds ([14 x i8]* @.str1, i32 0, i32 0), i32 12)
  store i32 43, i32* @Global, align 4
  ret i8* null
}

declare void @llvm.var.annotation(i8*, i8*, i8*, i32) nounwind

define i32 @main() nounwind uwtable {
entry:
  %retval = alloca i32, align 4
  %t = alloca [2 x i64], align 16
  store i32 0, i32* %retval
  %arrayidx = getelementptr inbounds [2 x i64]* %t, i32 0, i64 0
  %call = call i32 @pthread_create(i64* %arrayidx, %union.pthread_attr_t* null, i8* (i8*)* @Thread1, i8* null) nounwind
  %arrayidx1 = getelementptr inbounds [2 x i64]* %t, i32 0, i64 1
  %call2 = call i32 @pthread_create(i64* %arrayidx1, %union.pthread_attr_t* null, i8* (i8*)* @Thread2, i8* null) nounwind
  %arrayidx3 = getelementptr inbounds [2 x i64]* %t, i32 0, i64 0
  %0 = load i64* %arrayidx3, align 8
  %call4 = call i32 @pthread_join(i64 %0, i8** null)
  %arrayidx5 = getelementptr inbounds [2 x i64]* %t, i32 0, i64 1
  %1 = load i64* %arrayidx5, align 8
  %call6 = call i32 @pthread_join(i64 %1, i8** null)
  ret i32 0
}

declare i32 @pthread_create(i64*, %union.pthread_attr_t*, i8* (i8*)*, i8*) nounwind

declare i32 @pthread_join(i64, i8**)
