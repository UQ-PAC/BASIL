; ModuleID = '/host/dsa-test-bodging/split/split.c'
source_filename = "/host/dsa-test-bodging/split/split.c"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "aarch64-unknown-linux-gnu"

@x = common dso_local global i32 0, align 4, !dbg !0
@y = common dso_local local_unnamed_addr global i32* null, align 8, !dbg !6
@array = internal unnamed_addr constant [10 x i32] [i32 0, i32 1, i32 2, i32 3, i32 4, i32 5, i32 6, i32 7, i32 8, i32 9], align 4, !dbg !11

; Function Attrs: nofree norecurse nounwind writeonly
define dso_local i32 @main(i32 %0) local_unnamed_addr #0 !dbg !20 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !24, metadata !DIExpression()), !dbg !25
  store i32* @x, i32** @y, align 8, !dbg !26, !tbaa !27
  %2 = sext i32 %0 to i64, !dbg !31
  %3 = getelementptr inbounds [10 x i32], [10 x i32]* @array, i64 0, i64 %2, !dbg !31
  %4 = load i32, i32* %3, align 4, !dbg !31, !tbaa !32
  ret i32 %4, !dbg !34
}

; Function Attrs: nounwind readnone speculatable willreturn
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

attributes #0 = { nofree norecurse nounwind writeonly "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="non-leaf" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+neon" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable willreturn }

!llvm.dbg.cu = !{!2}
!llvm.module.flags = !{!16, !17, !18}
!llvm.ident = !{!19}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(name: "x", scope: !2, file: !8, line: 2, type: !10, isLocal: false, isDefinition: true)
!2 = distinct !DICompileUnit(language: DW_LANG_C99, file: !3, producer: "clang version 10.0.0-4ubuntu1 ", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, enums: !4, globals: !5, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "/host/dsa-test-bodging/split/split.c", directory: "/")
!4 = !{}
!5 = !{!0, !6, !11}
!6 = !DIGlobalVariableExpression(var: !7, expr: !DIExpression())
!7 = distinct !DIGlobalVariable(name: "y", scope: !2, file: !8, line: 3, type: !9, isLocal: false, isDefinition: true)
!8 = !DIFile(filename: "/host/dsa-test-bodging/split/split.c", directory: "")
!9 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !10, size: 64)
!10 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!11 = !DIGlobalVariableExpression(var: !12, expr: !DIExpression())
!12 = distinct !DIGlobalVariable(name: "array", scope: !2, file: !8, line: 1, type: !13, isLocal: true, isDefinition: true)
!13 = !DICompositeType(tag: DW_TAG_array_type, baseType: !10, size: 320, elements: !14)
!14 = !{!15}
!15 = !DISubrange(count: 10)
!16 = !{i32 7, !"Dwarf Version", i32 4}
!17 = !{i32 2, !"Debug Info Version", i32 3}
!18 = !{i32 1, !"wchar_size", i32 4}
!19 = !{!"clang version 10.0.0-4ubuntu1 "}
!20 = distinct !DISubprogram(name: "main", scope: !8, file: !8, line: 5, type: !21, scopeLine: 5, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !23)
!21 = !DISubroutineType(types: !22)
!22 = !{!10, !10}
!23 = !{!24}
!24 = !DILocalVariable(name: "r", arg: 1, scope: !20, file: !8, line: 5, type: !10)
!25 = !DILocation(line: 0, scope: !20)
!26 = !DILocation(line: 6, column: 7, scope: !20)
!27 = !{!28, !28, i64 0}
!28 = !{!"any pointer", !29, i64 0}
!29 = !{!"omnipotent char", !30, i64 0}
!30 = !{!"Simple C/C++ TBAA"}
!31 = !DILocation(line: 7, column: 12, scope: !20)
!32 = !{!33, !33, i64 0}
!33 = !{!"int", !29, i64 0}
!34 = !DILocation(line: 7, column: 5, scope: !20)
