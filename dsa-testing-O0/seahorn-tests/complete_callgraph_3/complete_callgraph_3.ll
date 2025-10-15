; ModuleID = 'complete_callgraph_3.c'
source_filename = "complete_callgraph_3.c"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "aarch64-unknown-linux-gnu"

%struct.class_t = type { {}*, {}* }

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @foo(%struct.class_t* noundef %0, i32 noundef %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca %struct.class_t*, align 8
  %5 = alloca i32, align 4
  store %struct.class_t* %0, %struct.class_t** %4, align 8
  store i32 %1, i32* %5, align 4
  %6 = load i32, i32* %5, align 4
  %7 = icmp sgt i32 %6, 10
  br i1 %7, label %8, label %17

8:                                                ; preds = %2
  %9 = load %struct.class_t*, %struct.class_t** %4, align 8
  %10 = getelementptr inbounds %struct.class_t, %struct.class_t* %9, i32 0, i32 1
  %11 = bitcast {}** %10 to i32 (%struct.class_t*, i32)**
  %12 = load i32 (%struct.class_t*, i32)*, i32 (%struct.class_t*, i32)** %11, align 8
  %13 = load %struct.class_t*, %struct.class_t** %4, align 8
  %14 = load i32, i32* %5, align 4
  %15 = add nsw i32 %14, 1
  %16 = call i32 %12(%struct.class_t* noundef %13, i32 noundef %15)
  store i32 %16, i32* %3, align 4
  br label %19

17:                                               ; preds = %2
  %18 = load i32, i32* %5, align 4
  store i32 %18, i32* %3, align 4
  br label %19

19:                                               ; preds = %17, %8
  %20 = load i32, i32* %3, align 4
  ret i32 %20
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @bar(%struct.class_t* noundef %0, i32 noundef %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca %struct.class_t*, align 8
  %5 = alloca i32, align 4
  store %struct.class_t* %0, %struct.class_t** %4, align 8
  store i32 %1, i32* %5, align 4
  %6 = load i32, i32* %5, align 4
  %7 = icmp slt i32 %6, 100
  br i1 %7, label %8, label %11

8:                                                ; preds = %2
  %9 = load i32, i32* %5, align 4
  %10 = add nsw i32 %9, 10
  store i32 %10, i32* %3, align 4
  br label %14

11:                                               ; preds = %2
  %12 = load i32, i32* %5, align 4
  %13 = sub nsw i32 %12, 5
  store i32 %13, i32* %3, align 4
  br label %14

14:                                               ; preds = %11, %8
  %15 = load i32, i32* %3, align 4
  ret i32 %15
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca %struct.class_t, align 8
  %3 = alloca i32, align 4
  store i32 0, i32* %1, align 4
  %4 = getelementptr inbounds %struct.class_t, %struct.class_t* %2, i32 0, i32 0
  %5 = bitcast {}** %4 to i32 (%struct.class_t*, i32)**
  store i32 (%struct.class_t*, i32)* @foo, i32 (%struct.class_t*, i32)** %5, align 8
  %6 = getelementptr inbounds %struct.class_t, %struct.class_t* %2, i32 0, i32 1
  %7 = bitcast {}** %6 to i32 (%struct.class_t*, i32)**
  store i32 (%struct.class_t*, i32)* @bar, i32 (%struct.class_t*, i32)** %7, align 8
  %8 = getelementptr inbounds %struct.class_t, %struct.class_t* %2, i32 0, i32 0
  %9 = bitcast {}** %8 to i32 (%struct.class_t*, i32)**
  %10 = load i32 (%struct.class_t*, i32)*, i32 (%struct.class_t*, i32)** %9, align 8
  %11 = call i32 %10(%struct.class_t* noundef %2, i32 noundef 42)
  store i32 %11, i32* %3, align 4
  ret i32 0
}

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="non-leaf" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+neon,+outline-atomics,+v8a" }

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
