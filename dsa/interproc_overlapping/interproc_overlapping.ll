; ModuleID = '/host/dsa/interproc_overlapping/interproc_overlapping.c'
source_filename = "/host/dsa/interproc_overlapping/interproc_overlapping.c"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "aarch64-unknown-linux-gnu"

%struct.str = type { i64, i64, i64 }

; Function Attrs: noinline nounwind optnone
define dso_local void @set_fields(%struct.str* %0) #0 !dbg !7 {
  %2 = alloca %struct.str*, align 8
  store %struct.str* %0, %struct.str** %2, align 8
  call void @llvm.dbg.declare(metadata %struct.str** %2, metadata !19, metadata !DIExpression()), !dbg !20
  %3 = load %struct.str*, %struct.str** %2, align 8, !dbg !21
  %4 = getelementptr inbounds %struct.str, %struct.str* %3, i32 0, i32 0, !dbg !22
  store i64 1, i64* %4, align 8, !dbg !23
  %5 = load %struct.str*, %struct.str** %2, align 8, !dbg !24
  %6 = getelementptr inbounds %struct.str, %struct.str* %5, i32 0, i32 2, !dbg !25
  store i64 2, i64* %6, align 8, !dbg !26
  ret void, !dbg !27
}

; Function Attrs: nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind optnone
define dso_local i64 @test(i1 %0, %struct.str* %1) #0 !dbg !28 {
  %3 = alloca i8, align 1
  %4 = alloca %struct.str*, align 8
  %5 = alloca %struct.str, align 8
  %6 = alloca %struct.str*, align 8
  %7 = zext i1 %0 to i8
  store i8 %7, i8* %3, align 1
  call void @llvm.dbg.declare(metadata i8* %3, metadata !32, metadata !DIExpression()), !dbg !33
  store %struct.str* %1, %struct.str** %4, align 8
  call void @llvm.dbg.declare(metadata %struct.str** %4, metadata !34, metadata !DIExpression()), !dbg !35
  call void @llvm.dbg.declare(metadata %struct.str* %5, metadata !36, metadata !DIExpression()), !dbg !37
  %8 = getelementptr inbounds %struct.str, %struct.str* %5, i32 0, i32 0, !dbg !38
  store i64 3, i64* %8, align 8, !dbg !39
  %9 = getelementptr inbounds %struct.str, %struct.str* %5, i32 0, i32 2, !dbg !40
  store i64 10, i64* %9, align 8, !dbg !41
  call void @llvm.dbg.declare(metadata %struct.str** %6, metadata !42, metadata !DIExpression()), !dbg !43
  %10 = load i8, i8* %3, align 1, !dbg !44
  %11 = trunc i8 %10 to i1, !dbg !44
  br i1 %11, label %12, label %14, !dbg !44

12:                                               ; preds = %2
  %13 = load %struct.str*, %struct.str** %4, align 8, !dbg !45
  br label %15, !dbg !44

14:                                               ; preds = %2
  br label %15, !dbg !44

15:                                               ; preds = %14, %12
  %16 = phi %struct.str* [ %13, %12 ], [ %5, %14 ], !dbg !44
  store %struct.str* %16, %struct.str** %6, align 8, !dbg !43
  %17 = load %struct.str*, %struct.str** %6, align 8, !dbg !46
  call void @set_fields(%struct.str* %17), !dbg !47
  %18 = getelementptr inbounds %struct.str, %struct.str* %5, i32 0, i32 2, !dbg !48
  %19 = load i64, i64* %18, align 8, !dbg !48
  ret i64 %19, !dbg !49
}

; Function Attrs: noinline nounwind optnone
define dso_local i32 @main() #0 !dbg !50 {
  %1 = call noalias i8* @malloc(i64 24) #3, !dbg !54
  %2 = bitcast i8* %1 to %struct.str*, !dbg !54
  %3 = call i64 @test(i1 false, %struct.str* %2), !dbg !55
  ret i32 0, !dbg !56
}

; Function Attrs: nounwind
declare dso_local noalias i8* @malloc(i64) #2

attributes #0 = { noinline nounwind optnone "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="non-leaf" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+neon" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable willreturn }
attributes #2 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="non-leaf" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+neon" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5}
!llvm.ident = !{!6}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 10.0.0-4ubuntu1 ", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "/host/dsa/interproc_overlapping/interproc_overlapping.c", directory: "/")
!2 = !{}
!3 = !{i32 7, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"wchar_size", i32 4}
!6 = !{!"clang version 10.0.0-4ubuntu1 "}
!7 = distinct !DISubprogram(name: "set_fields", scope: !8, file: !8, line: 10, type: !9, scopeLine: 10, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!8 = !DIFile(filename: "/host/dsa/interproc_overlapping/interproc_overlapping.c", directory: "")
!9 = !DISubroutineType(types: !10)
!10 = !{null, !11}
!11 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !12, size: 64)
!12 = !DIDerivedType(tag: DW_TAG_typedef, name: "str", file: !8, line: 8, baseType: !13)
!13 = distinct !DICompositeType(tag: DW_TAG_structure_type, file: !8, line: 4, size: 192, elements: !14)
!14 = !{!15, !17, !18}
!15 = !DIDerivedType(tag: DW_TAG_member, name: "a", scope: !13, file: !8, line: 5, baseType: !16, size: 64)
!16 = !DIBasicType(name: "long int", size: 64, encoding: DW_ATE_signed)
!17 = !DIDerivedType(tag: DW_TAG_member, name: "b", scope: !13, file: !8, line: 6, baseType: !16, size: 64, offset: 64)
!18 = !DIDerivedType(tag: DW_TAG_member, name: "c", scope: !13, file: !8, line: 7, baseType: !16, size: 64, offset: 128)
!19 = !DILocalVariable(name: "ext", arg: 1, scope: !7, file: !8, line: 10, type: !11)
!20 = !DILocation(line: 10, column: 49, scope: !7)
!21 = !DILocation(line: 11, column: 5, scope: !7)
!22 = !DILocation(line: 11, column: 10, scope: !7)
!23 = !DILocation(line: 11, column: 12, scope: !7)
!24 = !DILocation(line: 12, column: 5, scope: !7)
!25 = !DILocation(line: 12, column: 10, scope: !7)
!26 = !DILocation(line: 12, column: 12, scope: !7)
!27 = !DILocation(line: 13, column: 1, scope: !7)
!28 = distinct !DISubprogram(name: "test", scope: !8, file: !8, line: 15, type: !29, scopeLine: 15, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!29 = !DISubroutineType(types: !30)
!30 = !{!16, !31, !11}
!31 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!32 = !DILocalVariable(name: "b", arg: 1, scope: !28, file: !8, line: 15, type: !31)
!33 = !DILocation(line: 15, column: 20, scope: !28)
!34 = !DILocalVariable(name: "heap", arg: 2, scope: !28, file: !8, line: 15, type: !11)
!35 = !DILocation(line: 15, column: 28, scope: !28)
!36 = !DILocalVariable(name: "stack", scope: !28, file: !8, line: 16, type: !12)
!37 = !DILocation(line: 16, column: 9, scope: !28)
!38 = !DILocation(line: 17, column: 11, scope: !28)
!39 = !DILocation(line: 17, column: 13, scope: !28)
!40 = !DILocation(line: 18, column: 11, scope: !28)
!41 = !DILocation(line: 18, column: 13, scope: !28)
!42 = !DILocalVariable(name: "ptr", scope: !28, file: !8, line: 19, type: !11)
!43 = !DILocation(line: 19, column: 10, scope: !28)
!44 = !DILocation(line: 19, column: 16, scope: !28)
!45 = !DILocation(line: 19, column: 20, scope: !28)
!46 = !DILocation(line: 20, column: 16, scope: !28)
!47 = !DILocation(line: 20, column: 5, scope: !28)
!48 = !DILocation(line: 21, column: 18, scope: !28)
!49 = !DILocation(line: 21, column: 5, scope: !28)
!50 = distinct !DISubprogram(name: "main", scope: !8, file: !8, line: 24, type: !51, scopeLine: 25, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!51 = !DISubroutineType(types: !52)
!52 = !{!53}
!53 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!54 = !DILocation(line: 27, column: 17, scope: !50)
!55 = !DILocation(line: 27, column: 5, scope: !50)
!56 = !DILocation(line: 29, column: 1, scope: !50)
