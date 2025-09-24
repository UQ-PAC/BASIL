; ModuleID = '/host/dsa/global_interproc_overlapping/global_interproc_overlapping.c'
source_filename = "/host/dsa/global_interproc_overlapping/global_interproc_overlapping.c"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "aarch64-unknown-linux-gnu"

%struct.str = type { i64, i64, i64 }

@__const.set_fields.tmp = private unnamed_addr constant %struct.str { i64 1, i64 2, i64 3 }, align 8
@global = common dso_local global %struct.str zeroinitializer, align 8, !dbg !0

; Function Attrs: noinline nounwind optnone
define dso_local void @set_fields(%struct.str* %0) #0 !dbg !18 {
  %2 = alloca %struct.str*, align 8
  %3 = alloca %struct.str, align 8
  store %struct.str* %0, %struct.str** %2, align 8
  call void @llvm.dbg.declare(metadata %struct.str** %2, metadata !22, metadata !DIExpression()), !dbg !23
  call void @llvm.dbg.declare(metadata %struct.str* %3, metadata !24, metadata !DIExpression()), !dbg !25
  %4 = bitcast %struct.str* %3 to i8*, !dbg !25
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %4, i8* align 8 bitcast (%struct.str* @__const.set_fields.tmp to i8*), i64 24, i1 false), !dbg !25
  %5 = load %struct.str*, %struct.str** %2, align 8, !dbg !26
  %6 = bitcast %struct.str* %5 to i8*, !dbg !27
  %7 = bitcast %struct.str* %3 to i8*, !dbg !27
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %6, i8* align 8 %7, i64 24, i1 false), !dbg !27
  ret void, !dbg !28
}

; Function Attrs: nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: argmemonly nounwind willreturn
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* noalias nocapture writeonly, i8* noalias nocapture readonly, i64, i1 immarg) #2

; Function Attrs: noinline nounwind optnone
define dso_local i32 @main() #0 !dbg !29 {
  store i64 3, i64* getelementptr inbounds (%struct.str, %struct.str* @global, i32 0, i32 0), align 8, !dbg !33
  call void @set_fields(%struct.str* @global), !dbg !34
  ret i32 0, !dbg !35
}

attributes #0 = { noinline nounwind optnone "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="non-leaf" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+neon" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable willreturn }
attributes #2 = { argmemonly nounwind willreturn }

!llvm.dbg.cu = !{!2}
!llvm.module.flags = !{!14, !15, !16}
!llvm.ident = !{!17}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(name: "global", scope: !2, file: !6, line: 11, type: !7, isLocal: false, isDefinition: true)
!2 = distinct !DICompileUnit(language: DW_LANG_C99, file: !3, producer: "clang version 10.0.0-4ubuntu1 ", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !4, globals: !5, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "/host/dsa/global_interproc_overlapping/global_interproc_overlapping.c", directory: "/")
!4 = !{}
!5 = !{!0}
!6 = !DIFile(filename: "/host/dsa/global_interproc_overlapping/global_interproc_overlapping.c", directory: "")
!7 = !DIDerivedType(tag: DW_TAG_typedef, name: "str", file: !6, line: 8, baseType: !8)
!8 = distinct !DICompositeType(tag: DW_TAG_structure_type, file: !6, line: 4, size: 192, elements: !9)
!9 = !{!10, !12, !13}
!10 = !DIDerivedType(tag: DW_TAG_member, name: "a", scope: !8, file: !6, line: 5, baseType: !11, size: 64)
!11 = !DIBasicType(name: "long int", size: 64, encoding: DW_ATE_signed)
!12 = !DIDerivedType(tag: DW_TAG_member, name: "b", scope: !8, file: !6, line: 6, baseType: !11, size: 64, offset: 64)
!13 = !DIDerivedType(tag: DW_TAG_member, name: "c", scope: !8, file: !6, line: 7, baseType: !11, size: 64, offset: 128)
!14 = !{i32 7, !"Dwarf Version", i32 4}
!15 = !{i32 2, !"Debug Info Version", i32 3}
!16 = !{i32 1, !"wchar_size", i32 4}
!17 = !{!"clang version 10.0.0-4ubuntu1 "}
!18 = distinct !DISubprogram(name: "set_fields", scope: !6, file: !6, line: 13, type: !19, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !2, retainedNodes: !4)
!19 = !DISubroutineType(types: !20)
!20 = !{null, !21}
!21 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !7, size: 64)
!22 = !DILocalVariable(name: "ext", arg: 1, scope: !18, file: !6, line: 13, type: !21)
!23 = !DILocation(line: 13, column: 49, scope: !18)
!24 = !DILocalVariable(name: "tmp", scope: !18, file: !6, line: 15, type: !7)
!25 = !DILocation(line: 15, column: 9, scope: !18)
!26 = !DILocation(line: 16, column: 6, scope: !18)
!27 = !DILocation(line: 16, column: 12, scope: !18)
!28 = !DILocation(line: 18, column: 1, scope: !18)
!29 = distinct !DISubprogram(name: "main", scope: !6, file: !6, line: 20, type: !30, scopeLine: 21, spFlags: DISPFlagDefinition, unit: !2, retainedNodes: !4)
!30 = !DISubroutineType(types: !31)
!31 = !{!32}
!32 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!33 = !DILocation(line: 22, column: 14, scope: !29)
!34 = !DILocation(line: 23, column: 5, scope: !29)
!35 = !DILocation(line: 24, column: 1, scope: !29)
