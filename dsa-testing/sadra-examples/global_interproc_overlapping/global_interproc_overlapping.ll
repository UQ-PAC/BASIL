; ModuleID = 'dsa-testing/sadra-examples/global_interproc_overlapping/global_interproc_overlapping.c'
source_filename = "dsa-testing/sadra-examples/global_interproc_overlapping/global_interproc_overlapping.c"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "aarch64-unknown-linux-gnu"

%struct.str = type { i64, i64, i64 }

@__const.set_fields.tmp = private unnamed_addr constant %struct.str { i64 1, i64 2, i64 3 }, align 8
@global = dso_local global %struct.str zeroinitializer, align 8

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @set_fields(%struct.str* noundef %0) #0 {
  %2 = alloca %struct.str*, align 8
  %3 = alloca %struct.str, align 8
  store %struct.str* %0, %struct.str** %2, align 8
  %4 = bitcast %struct.str* %3 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %4, i8* align 8 bitcast (%struct.str* @__const.set_fields.tmp to i8*), i64 24, i1 false)
  %5 = load %struct.str*, %struct.str** %2, align 8
  %6 = bitcast %struct.str* %5 to i8*
  %7 = bitcast %struct.str* %3 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %6, i8* align 8 %7, i64 24, i1 false)
  ret void
}

; Function Attrs: argmemonly nofree nounwind willreturn
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* noalias nocapture writeonly, i8* noalias nocapture readonly, i64, i1 immarg) #1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main() #0 {
  store i64 3, i64* getelementptr inbounds (%struct.str, %struct.str* @global, i32 0, i32 0), align 8
  call void @set_fields(%struct.str* noundef @global)
  ret i32 0
}

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="non-leaf" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+neon,+outline-atomics,+v8a" }
attributes #1 = { argmemonly nofree nounwind willreturn }

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
