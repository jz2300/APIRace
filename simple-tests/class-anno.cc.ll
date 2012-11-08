; ModuleID = 'class-anno.cc.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%union.pthread_attr_t = type { i64, [48 x i8] }

@_ZN1A6GlobalE = global i32 0, align 4
@.str = private unnamed_addr constant [11 x i8] c"self-write\00", section "llvm.metadata"
@.str1 = private unnamed_addr constant [14 x i8] c"class-anno.cc\00", section "llvm.metadata"
@llvm.global.annotations = appending global [2 x { i8*, i8*, i8*, i32 }] [{ i8*, i8*, i8*, i32 } { i8* bitcast (i8* (i8*)* @_ZN1A7Thread2EPv to i8*), i8* getelementptr inbounds ([11 x i8]* @.str, i32 0, i32 0), i8* getelementptr inbounds ([14 x i8]* @.str1, i32 0, i32 0), i32 14 }, { i8*, i8*, i8*, i32 } { i8* bitcast (i8* (i8*)* @_ZN1A7Thread1EPv to i8*), i8* getelementptr inbounds ([11 x i8]* @.str, i32 0, i32 0), i8* getelementptr inbounds ([14 x i8]* @.str1, i32 0, i32 0), i32 9 }], section "llvm.metadata"

define i32 @main() uwtable {
entry:
  %t = alloca [2 x i64], align 16
  %arrayidx = getelementptr inbounds [2 x i64]* %t, i32 0, i64 0
  %call = call i32 @pthread_create(i64* %arrayidx, %union.pthread_attr_t* null, i8* (i8*)* @_ZN1A7Thread1EPv, i8* null) nounwind
  %arrayidx1 = getelementptr inbounds [2 x i64]* %t, i32 0, i64 1
  %call2 = call i32 @pthread_create(i64* %arrayidx1, %union.pthread_attr_t* null, i8* (i8*)* @_ZN1A7Thread2EPv, i8* null) nounwind
  %arrayidx3 = getelementptr inbounds [2 x i64]* %t, i32 0, i64 0
  %0 = load i64* %arrayidx3, align 8
  %call4 = call i32 @pthread_join(i64 %0, i8** null)
  %arrayidx5 = getelementptr inbounds [2 x i64]* %t, i32 0, i64 1
  %1 = load i64* %arrayidx5, align 8
  %call6 = call i32 @pthread_join(i64 %1, i8** null)
  ret i32 0
}

declare i32 @pthread_create(i64*, %union.pthread_attr_t*, i8* (i8*)*, i8*) nounwind

define linkonce_odr i8* @_ZN1A7Thread1EPv(i8* %x) nounwind uwtable align 2 {
entry:
  %x.addr = alloca i8*, align 8
  store i8* %x, i8** %x.addr, align 8
  %0 = load i32* @_ZN1A6GlobalE, align 4
  %inc = add nsw i32 %0, 1
  store i32 %inc, i32* @_ZN1A6GlobalE, align 4
  ret i8* null
}

define linkonce_odr i8* @_ZN1A7Thread2EPv(i8* %x) nounwind uwtable align 2 {
entry:
  %x.addr = alloca i8*, align 8
  store i8* %x, i8** %x.addr, align 8
  %0 = load i32* @_ZN1A6GlobalE, align 4
  %dec = add nsw i32 %0, -1
  store i32 %dec, i32* @_ZN1A6GlobalE, align 4
  ret i8* null
}

declare i32 @pthread_join(i64, i8**)
