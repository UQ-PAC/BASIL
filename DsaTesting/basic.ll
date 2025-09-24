; ModuleID = '/host/DsaTesting/basic.c'
source_filename = "/host/DsaTesting/basic.c"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "aarch64-unknown-linux-gnu"

%struct.S = type { i32**, i32** }

@g = common dso_local global i32 0, align 4, !dbg !0

; Function Attrs: noinline nounwind optnone
define dso_local i32 @main(i32 %0, i8** %1) #0 !dbg !14 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i8**, align 8
  %6 = alloca %struct.S, align 8
  %7 = alloca %struct.S, align 8
  %8 = alloca i32*, align 8
  %9 = alloca i32*, align 8
  store i32 0, i32* %3, align 4
  store i32 %0, i32* %4, align 4
  call void @llvm.dbg.declare(metadata i32* %4, metadata !20, metadata !DIExpression()), !dbg !21
  store i8** %1, i8*** %5, align 8
  call void @llvm.dbg.declare(metadata i8*** %5, metadata !22, metadata !DIExpression()), !dbg !23
  call void @llvm.dbg.declare(metadata %struct.S* %6, metadata !24, metadata !DIExpression()), !dbg !31
  call void @llvm.dbg.declare(metadata %struct.S* %7, metadata !32, metadata !DIExpression()), !dbg !33
  call void @llvm.dbg.declare(metadata i32** %8, metadata !34, metadata !DIExpression()), !dbg !35
  %10 = call noalias i8* @malloc(i64 4) #3, !dbg !36
  %11 = bitcast i8* %10 to i32*, !dbg !37
  store i32* %11, i32** %8, align 8, !dbg !35
  call void @llvm.dbg.declare(metadata i32** %9, metadata !38, metadata !DIExpression()), !dbg !39
  %12 = call noalias i8* @malloc(i64 4) #3, !dbg !40
  %13 = bitcast i8* %12 to i32*, !dbg !41
  store i32* %13, i32** %9, align 8, !dbg !39
  %14 = getelementptr inbounds %struct.S, %struct.S* %6, i32 0, i32 0, !dbg !42
  store i32** %8, i32*** %14, align 8, !dbg !43
  %15 = getelementptr inbounds %struct.S, %struct.S* %6, i32 0, i32 1, !dbg !44
  store i32** %9, i32*** %15, align 8, !dbg !45
  %16 = getelementptr inbounds %struct.S, %struct.S* %6, i32 0, i32 0, !dbg !46
  %17 = load i32**, i32*** %16, align 8, !dbg !46
  store i32* @g, i32** %17, align 8, !dbg !47
  ret i32 0, !dbg !48
}

; Function Attrs: nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: nounwind
declare dso_local noalias i8* @malloc(i64) #2

attributes #0 = { noinline nounwind optnone "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="non-leaf" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+neon" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable willreturn }
attributes #2 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="non-leaf" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+neon" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }

!llvm.dbg.cu = !{!2}
!llvm.module.flags = !{!10, !11, !12}
!llvm.ident = !{!13}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(name: "g", scope: !2, file: !9, line: 8, type: !7, isLocal: false, isDefinition: true)
!2 = distinct !DICompileUnit(language: DW_LANG_C99, file: !3, producer: "clang version 10.0.0-4ubuntu1 ", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !4, retainedTypes: !5, globals: !8, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "/host/DsaTesting/basic.c", directory: "/")
!4 = !{}
!5 = !{!6}
!6 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !7, size: 64)
!7 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!8 = !{!0}
!9 = !DIFile(filename: "/host/DsaTesting/basic.c", directory: "")
!10 = !{i32 7, !"Dwarf Version", i32 4}
!11 = !{i32 2, !"Debug Info Version", i32 3}
!12 = !{i32 1, !"wchar_size", i32 4}
!13 = !{!"clang version 10.0.0-4ubuntu1 "}
!14 = distinct !DISubprogram(name: "main", scope: !9, file: !9, line: 10, type: !15, scopeLine: 10, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !2, retainedNodes: !4)
!15 = !DISubroutineType(types: !16)
!16 = !{!7, !7, !17}
!17 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !18, size: 64)
!18 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !19, size: 64)
!19 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_unsigned_char)
!20 = !DILocalVariable(name: "argc", arg: 1, scope: !14, file: !9, line: 10, type: !7)
!21 = !DILocation(line: 10, column: 14, scope: !14)
!22 = !DILocalVariable(name: "argv", arg: 2, scope: !14, file: !9, line: 10, type: !17)
!23 = !DILocation(line: 10, column: 27, scope: !14)
!24 = !DILocalVariable(name: "s1", scope: !14, file: !9, line: 12, type: !25)
!25 = !DIDerivedType(tag: DW_TAG_typedef, name: "S", file: !9, line: 6, baseType: !26)
!26 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "S", file: !9, line: 3, size: 128, elements: !27)
!27 = !{!28, !30}
!28 = !DIDerivedType(tag: DW_TAG_member, name: "x", scope: !26, file: !9, line: 4, baseType: !29, size: 64)
!29 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !6, size: 64)
!30 = !DIDerivedType(tag: DW_TAG_member, name: "y", scope: !26, file: !9, line: 5, baseType: !29, size: 64, offset: 64)
!31 = !DILocation(line: 12, column: 5, scope: !14)
!32 = !DILocalVariable(name: "s2", scope: !14, file: !9, line: 12, type: !25)
!33 = !DILocation(line: 12, column: 9, scope: !14)
!34 = !DILocalVariable(name: "p1", scope: !14, file: !9, line: 14, type: !6)
!35 = !DILocation(line: 14, column: 8, scope: !14)
!36 = !DILocation(line: 14, column: 20, scope: !14)
!37 = !DILocation(line: 14, column: 13, scope: !14)
!38 = !DILocalVariable(name: "q1", scope: !14, file: !9, line: 15, type: !6)
!39 = !DILocation(line: 15, column: 8, scope: !14)
!40 = !DILocation(line: 15, column: 20, scope: !14)
!41 = !DILocation(line: 15, column: 13, scope: !14)
!42 = !DILocation(line: 16, column: 6, scope: !14)
!43 = !DILocation(line: 16, column: 8, scope: !14)
!44 = !DILocation(line: 17, column: 6, scope: !14)
!45 = !DILocation(line: 17, column: 8, scope: !14)
!46 = !DILocation(line: 18, column: 8, scope: !14)
!47 = !DILocation(line: 18, column: 11, scope: !14)
!48 = !DILocation(line: 20, column: 3, scope: !14)
