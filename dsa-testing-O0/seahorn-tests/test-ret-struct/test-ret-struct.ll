; ModuleID = 'test-ret-struct.c'
source_filename = "test-ret-struct.c"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "aarch64-unknown-linux-gnu"

%struct.S = type { i32*, i64 }

@A = dso_local global i32* null, align 8
@.str = private unnamed_addr constant [12 x i8] c"*(y.a) == 1\00", align 1
@.str.1 = private unnamed_addr constant [18 x i8] c"test-ret-struct.c\00", align 1
@__PRETTY_FUNCTION__.main = private unnamed_addr constant [11 x i8] c"int main()\00", align 1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local [2 x i64] @foo() #0 {
  %1 = alloca %struct.S, align 8
  %2 = getelementptr inbounds %struct.S, %struct.S* %1, i32 0, i32 0
  %3 = load i32*, i32** @A, align 8
  store i32* %3, i32** %2, align 8
  %4 = getelementptr inbounds %struct.S, %struct.S* %1, i32 0, i32 1
  store i64 2, i64* %4, align 8
  %5 = bitcast %struct.S* %1 to [2 x i64]*
  %6 = load [2 x i64], [2 x i64]* %5, align 8
  ret [2 x i64] %6
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca %struct.S, align 8
  store i32 0, i32* %1, align 4
  store i32 1, i32* %2, align 4
  store i32* %2, i32** @A, align 8
  %4 = call [2 x i64] @foo()
  %5 = bitcast %struct.S* %3 to [2 x i64]*
  store [2 x i64] %4, [2 x i64]* %5, align 8
  %6 = getelementptr inbounds %struct.S, %struct.S* %3, i32 0, i32 0
  %7 = load i32*, i32** %6, align 8
  %8 = load i32, i32* %7, align 4
  %9 = icmp eq i32 %8, 1
  br i1 %9, label %10, label %11

10:                                               ; preds = %0
  br label %12

11:                                               ; preds = %0
  call void @__assert_fail(i8* noundef getelementptr inbounds ([12 x i8], [12 x i8]* @.str, i64 0, i64 0), i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str.1, i64 0, i64 0), i32 noundef 20, i8* noundef getelementptr inbounds ([11 x i8], [11 x i8]* @__PRETTY_FUNCTION__.main, i64 0, i64 0)) #2
  unreachable

12:                                               ; preds = %10
  ret i32 0
}

; Function Attrs: noreturn nounwind
declare dso_local void @__assert_fail(i8* noundef, i8* noundef, i32 noundef, i8* noundef) #1

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="non-leaf" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+neon,+outline-atomics,+v8a" }
attributes #1 = { noreturn nounwind "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+neon,+outline-atomics,+v8a" }
attributes #2 = { noreturn nounwind }

!llvm.module.flags = !{!0, !1, !2, !3, !4, !5, !6}
!llvm.ident = !{!7}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 1, !"branch-target-enforcement", i32 0}
!2 = !{i32 1, !"sign-return-address", i32 0}
!3 = !{i32 1, !"sign-return-address-all", i32 0}
!4 = !{i32 1, !"sign-return-address-with-bkey", i32 0}
!5 = !{i32 7, !"uwtable", i32 1}
!6 = !{i32 7, !"frame-pointer", i32 1}
!7 = !{!"clang version 14.0.6"}
