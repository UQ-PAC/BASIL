
#include "cxxopts.hpp"
#include "json.hpp"
#include <cassert>
#include <clang-c/Index.h>
#include <format>
#include <iostream>
#include <memory>
#include <string_view>
#include <vector>

using json = nlohmann::json;

std::string to_str(CXString s) {
  auto cstr = clang_getCString(s);

  std::string str{cstr, strlen(cstr)};
  clang_disposeString(s);
  return str;
}

namespace nlohmann {

template <typename T> struct adl_serializer<std::optional<T>> {
  static void to_json(json &j, const std::optional<T> &value) {
    if (value.has_value()) {
      j = *value;
    } else {
      j = nullptr;
    }
  }

  static void from_json(const json &j, std::optional<T> &value) {
    if (j.is_null()) {
      value = {};
    } else {
      value = std::optional<T>(j.template get<T>());
    }
  }
};

}; // namespace nlohmann

struct c_type {
  std::string name;
  long long size;
  bool is_void;
  bool is_func_ptr;
  NLOHMANN_DEFINE_TYPE_INTRUSIVE(c_type, name, size, is_void, is_func_ptr);

  static c_type create(CXType type) {
    bool is_f_ptr = false;
    if (type.kind == CXType_Pointer &&
        (clang_getPointeeType(type).kind &
         (CXType_FunctionProto | CXType_FunctionNoProto))) {
      is_f_ptr = true;
    }

    bool is_void = type.kind == CXTypeKind::CXType_Void;

    long long size = clang_Type_getSizeOf(type);

    return {to_str(clang_getTypeSpelling(type)), size, is_void, is_f_ptr};
  }
};

struct param {
  std::string name;
  c_type type;
  NLOHMANN_DEFINE_TYPE_INTRUSIVE(param, name, type);
};

struct source_location {
  std::string file_name;
  unsigned line;
  unsigned column;

  static source_location create(CXSourceLocation loc) {
    CXFile file;
    unsigned line;
    unsigned col;
    unsigned offset;
    clang_getSpellingLocation(loc, &file, &line, &col, &offset);
    std::string filename{to_str(clang_getFileName(file))};
    return {std::string(filename), line, col};
  }

  NLOHMANN_DEFINE_TYPE_INTRUSIVE(source_location, file_name, line, column);
};

struct function_decl {
  std::string name;
  source_location location;
  c_type result_type;
  bool is_variadic;
  std::vector<param> params;
  NLOHMANN_DEFINE_TYPE_INTRUSIVE(function_decl, name, location, result_type,
                                 is_variadic, params);
};

struct cursor_state {
  std::unordered_map<std::string, function_decl> function_declarations;
  NLOHMANN_DEFINE_TYPE_INTRUSIVE(cursor_state, function_declarations);
};

CXChildVisitResult visitor(CXCursor cursor, CXCursor parent,
                           CXClientData clientData);

CXChildVisitResult funcdecl_visitor(CXCursor cursor, CXCursor parent,
                                    CXClientData clientData);

CXChildVisitResult visitor(CXCursor cursor, CXCursor parent,
                           CXClientData clientData) {
  cursor_state *data = (cursor_state *)clientData;

  CXSourceLocation loc = clang_getCursorLocation(cursor);

  CXCursorKind cursorKind = clang_getCursorKind(cursor);

  std::string name = to_str(clang_getCursorSpelling(cursor));

  if (cursorKind == CXCursorKind::CXCursor_FunctionDecl) {
    auto ftype = clang_getCursorType(cursor);

    int nargs =
        clang_Cursor_getNumArguments(cursor); // num function or call args

    assert(nargs != -1);

    CXType return_type = clang_getResultType(ftype);
    bool variadic = clang_isFunctionTypeVariadic(ftype);
    long long result_size = clang_Type_getSizeOf(return_type);

    function_decl decl{name,
                       source_location::create(clang_getCursorLocation(cursor)),
                       c_type::create(return_type),
                       variadic,
                       {}};

    for (int i = 0; i < nargs; i++) {
      // TODO: check if finalized type, etc
      auto arg = clang_Cursor_getArgument(cursor, i);
      CXType type = clang_getCursorType(arg);

      param p{to_str(clang_getCursorSpelling(arg)), c_type::create(type)};
      decl.params.emplace_back(p);
    }

    CXSourceLocation loc = clang_getCursorLocation(cursor);

    data->function_declarations[decl.name] = decl;
  }

  return CXChildVisit_Continue;
}

int main(int argc, char *argv[]) {
  const char *default_triple = "aarch64-linux-gnu";

  cxxopts::Options opts(argv[0], "Dump C function declaration signatures");

  opts.add_options()("f,file", "Source file", cxxopts::value<std::string>())(
      "t,target",
      "Clang target triple "
      "  (https://clang.llvm.org/docs/CrossCompilation.html#target-triple)",
      cxxopts::value<std::string>()->default_value(default_triple))(
      "c,clang-options", "Additional options to clang",
      cxxopts::value<std::string>());

  auto result = opts.parse(argc, argv);
  if (!result.count("file")) {
    std::cerr << "Missing argument --file.\n";
    std::cerr << opts.help();
    return 1;
  }

  CXIndex index = clang_createIndex(1, 1);

  std::vector<const char *> clang_cli_args{
      "-target", result["target"].as<std::string>().c_str(), "-I",
      "/usr/lib64/clang/16/include/"};

  std::string clang_additional_args;
  if (result.count("clang-options")) {
    clang_additional_args = {result["clang-options"].as<std::string>()};
    bool in_arg = false;
    for (int i = 0; i < clang_additional_args.size(); i++) {
      char c = clang_additional_args[i];
      if (c == ' ') {
        clang_additional_args[i] = '\0';
        in_arg = false;
      } else {
        if (!in_arg) {
          clang_cli_args.push_back(clang_additional_args.data() + i);
        }
        in_arg = true;
      }
    }
  }

  std::cerr << "clang args:";
  for (auto c : clang_cli_args) {
    std::cerr << " " << c;
  }
  std::cerr << std::endl;

  CXTranslationUnit tu = clang_createTranslationUnitFromSourceFile(
      index, result["file"].as<std::string>().c_str(), clang_cli_args.size(),
      clang_cli_args.data(), 0, nullptr);

  if (!tu) {
    std::cerr << "No failed to create tu" << std::endl;
    return -1;
  }

  CXCursor rootCursor = clang_getTranslationUnitCursor(tu);
  cursor_state s{};
  clang_visitChildren(rootCursor, visitor, &s);

  clang_disposeTranslationUnit(tu);
  clang_disposeIndex(index);

  std::cout << json(s).dump(4, ' ', false,
                            nlohmann::json::error_handler_t::strict)
            << std::endl;

  return 0;
}
