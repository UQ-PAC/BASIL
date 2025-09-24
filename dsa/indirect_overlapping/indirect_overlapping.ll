; ModuleID = '/host/dsa/indirect_overlapping/indirect_overlapping.c'
source_filename = "/host/dsa/indirect_overlapping/indirect_overlapping.c"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "aarch64-unknown-linux-gnu"

%struct.str = type { i64, i64, i64 }

; Function Attrs: noinline nounwind optnone
define dso_local i32 @main() #0 !dbg !7 {
  %1 = alloca %struct.str, align 8
  %2 = alloca %struct.str*, align 8
  call void @llvm.dbg.declare(metadata %struct.str* %1, metadata !12, metadata !DIExpression()), !dbg !20
  %3 = getelementptr inbounds %struct.str, %struct.str* %1, i32 0, i32 0, !dbg !21
  store i64 3, i64* %3, align 8, !dbg !22
  %4 = getelementptr inbounds %struct.str, %struct.str* %1, i32 0, i32 2, !dbg !23
  store i64 10, i64* %4, align 8, !dbg !24
  call void @llvm.dbg.declare(metadata %struct.str** %2, metadata !25, metadata !DIExpression()), !dbg !27
  store %struct.str* %1, %struct.str** %2, align 8, !dbg !27
  %5 = load %struct.str*, %struct.str** %2, align 8, !dbg !28
  %6 = getelementptr inbounds %struct.str, %struct.str* %5, i32 0, i32 0, !dbg !29
  store i64 4, i64* %6, align 8, !dbg !30
  %7 = load %struct.str*, %struct.str** %2, align 8, !dbg !31
  %8 = getelementptr inbounds %struct.str, %struct.str* %7, i32 0, i32 1, !dbg !32
  store i64 12, i64* %8, align 8, !dbg !33
  %9 = load %struct.str*, %struct.str** %2, align 8, !dbg !34
  %10 = getelementptr inbounds %struct.str, %struct.str* %9, i32 0, i32 2, !dbg !35
  store i64 13, i64* %10, align 8, !dbg !36
  ret i32 0, !dbg !37
}

; Function Attrs: nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

attributes #0 = { noinline nounwind optnone "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="non-leaf" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+neon" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable willreturn }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5}
!llvm.ident = !{!6}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 10.0.0-4ubuntu1 ", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "/host/dsa/indirect_overlapping/indirect_overlapping.c", directory: "/")
!2 = !{}
!3 = !{i32 7, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"wchar_size", i32 4}
!6 = !{!"clang version 10.0.0-4ubuntu1 "}
!7 = distinct !DISubprogram(name: "main", scope: !8, file: !8, line: 10, type: !9, scopeLine: 11, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!8 = !DIFile(filename: "/host/dsa/indirect_overlapping/indirect_overlapping.c", directory: "")
!9 = !DISubroutineType(types: !10)
!10 = !{!11}
!11 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!12 = !DILocalVariable(name: "stack", scope: !7, file: !8, line: 13, type: !13)
!13 = !DIDerivedType(tag: DW_TAG_typedef, name: "str", file: !8, line: 8, baseType: !14)
!14 = distinct !DICompositeType(tag: DW_TAG_structure_type, file: !8, line: 4, size: 192, elements: !15)
!15 = !{!16, !18, !19}
!16 = !DIDerivedType(tag: DW_TAG_member, name: "a", scope: !14, file: !8, line: 5, baseType: !17, size: 64)
!17 = !DIBasicType(name: "long int", size: 64, encoding: DW_ATE_signed)
!18 = !DIDerivedType(tag: DW_TAG_member, name: "b", scope: !14, file: !8, line: 6, baseType: !17, size: 64, offset: 64)
!19 = !DIDerivedType(tag: DW_TAG_member, name: "c", scope: !14, file: !8, line: 7, baseType: !17, size: 64, offset: 128)
!20 = !DILocation(line: 13, column: 9, scope: !7)
!21 = !DILocation(line: 14, column: 11, scope: !7)
!22 = !DILocation(line: 14, column: 13, scope: !7)
!23 = !DILocation(line: 15, column: 11, scope: !7)
!24 = !DILocation(line: 15, column: 13, scope: !7)
!25 = !DILocalVariable(name: "stackPointer", scope: !7, file: !8, line: 17, type: !26)
!26 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !13, size: 64)
!27 = !DILocation(line: 17, column: 10, scope: !7)
!28 = !DILocation(line: 19, column: 5, scope: !7)
!29 = !DILocation(line: 19, column: 19, scope: !7)
!30 = !DILocation(line: 19, column: 21, scope: !7)
!31 = !DILocation(line: 20, column: 5, scope: !7)
!32 = !DILocation(line: 20, column: 19, scope: !7)
!33 = !DILocation(line: 20, column: 21, scope: !7)
!34 = !DILocation(line: 21, column: 5, scope: !7)
!35 = !DILocation(line: 21, column: 19, scope: !7)
!36 = !DILocation(line: 21, column: 21, scope: !7)
!37 = !DILocation(line: 22, column: 1, scope: !7)
