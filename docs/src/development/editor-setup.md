# Development Environment

This is a Scala 3 project built using the [mill](https://mill-build.org) build system, it can be run with `./mill run`.

### Local Development

The tool itself is a Scala project and is OS-independent.

Furthermore, lifting input files from a given AArch64 binary is Linux-specific, and all commands given are for Linux.
On Windows, WSL2 may be used to run any Linux-specific tasks.

Installing a [JDK >= 17](https://openjdk.org/install/) is required.


The mill build tool script can bootstrap the rest of the Scala tool chain by running `./mill build` in the git root.
This can also be installed through [coursier](https://get-coursier.io/docs/cli-overview).

# IDE Support


[Mill page on IDE setup](https://mill-build.org/mill/cli/installation-ide.html#_ide_support)

## VSCode

Mill supports the Metals language server and IntelliJ through Build Server Protocol (BSP), which is the recommended use.

When you open the project in VSCode for the first time, it will likely ask you to install Java, and Scala language support extensions.  There is a Scala extension called [`Metals`](https://scalameta.org/metals/docs/editors/vscode/) which may be useful.

## IntelliJ Setup

**This guide assumes that you have java, scala, and ANTLR4 installed on your device. If this is not the case, see the README for instructions on how to install them.**

It's also not necessary to use IntelliJ, other IDEs and text editors can definitely work. However, getting the code to run, and specifically getting the debugger to work is a non-trivial task, so there are instruction here on how to set up everything with IntelliJ.

In the IntelliJ plugin store, download the ANTLR4 and scala plugin.

## Neovim

Some of us use the [AstroNvim](https://astronvim.com/) Neovim distro. For this, the community scala support
[astrocommunity/pack/scala](https://github.com/AstroNvim/astrocommunity/blob/438fdb8c648bc8870bab82e9149cad595ddc7a67/lua/astrocommunity/pack/scala/README.md?plain=1#L2) works out
of the box if you have coursier installed.

Otherwise the treesitter support and the metals language server can be configured manually.

## Ctags

[Ctags](https://en.wikipedia.org/wiki/Ctags) is an ancient piece of software that uses regular expressions to
implement go-to-definition for virtually any programming language. This can be somewhat unreliable, but it is
universally-available and very robust. If you have problems using the Metals LSP or if you just want a more
lightweight experience, you can try using Ctags.

To get started, install a modern version of ctags. [Universal Ctags](https://docs.ctags.io/en/latest/) is
suggested, and you can install this through your package manager. Then, run
```
./mill ctags
```
to generate the `tags` file in the repository root.
The `tags` file records regexes to locate each definition, so it is agnostic to
moving code within in the same file. However, if you add or change a definition (e.g., rename or changed parameters),
you will need to re-run the `./mill ctags` command. `./mill -w ctags` can be used to automatically watch for changes.

Once Ctags is running, you need to set up Ctags with your editor of choice.

For Neovim (and likely Vim), there is excellent support for ctags through the [Tagsrch](https://neovim.io/doc/user/tagsrch.html)
feature. To enable this, all you need is to set the `tags` Vim variable in your init file:
```
:set tags=./tags,tags
```
Then, after restarting Vim, you should be able to use `:tselect main` and it will open a
list of all definitions of "main" in the codebase.

You can use `Ctrl+]` with your cursor over a word to jump to the definition of that word.
Ctags is quite primitive, so it can easily get confused when there are multiple definitions
of the same term (e.g., companion objects).
After using `Ctrl+J`, you can use `:tn` or `:tp` to jump to additional definitions
of the word, or use `:ts` to list all definitions.

With certain Neovim plugins, you can also get a searchable view of all definitions
known to Ctags. For example, with [telescope.nvim](https://github.com/nvim-telescope/telescope.nvim)
installed, you can use `:Telescope tags`.

Putting this all together, here is a sample Neovim config (in Lua) with explanatory comments.
The `vim.cmd` statements can be ported to traditional Vim by keeping only the strings inside
the function call.
```lua
-- detect ctags next to the open file, in the initial directory of nvim, and in .git/tags.
vim.cmd(':set tags=./tags,tags,.git/tags')
-- ]t and [t can be used to go the next/previous definitions. useful for case objects.
vim.cmd('nnoremap ]t :tnext<CR><CR>')
vim.cmd('nnoremap [t :tprevious<CR><CR>')
-- gf can be used to go to the definition of the word under the cursor.
vim.cmd('nnoremap gf <C-]>')

-- helper function to open a Telescope search with certain options.
function search_tags()
  require("telescope.builtin").tags({ only_sort_tags = true, show_line = true, path_display = {"filename_first"}, show_kind=true, layout_strategy = "center", layout_config = {preview_cutoff = 5, anchor = 'N', height = 0.5} })
end

-- <Leader>lf is used to open the search window.
vim.keymap.set("n", "<Leader>lf", function()
  search_tags()
end, opts)
-- <Leader>lF is used to open the search window, pre-populated with the word under the cursor.
vim.keymap.set("n", "<Leader>lF", function()
  local word = vim.api.nvim_eval('expand("<cword>")')
  search_tags()
  vim.api.nvim_input(word .. "<Esc>")
end, opts)
```

## Working on the ANTLR4 parser
The ANTLR4 plugin should provide syntax highlighting
for ANTLR4 (.g4) files, as well as code autocompletion and other intellisense features.

The most useful features is the "ANTLR preview" panel found in a tab at the bottom. This allows you input an aribtrary string or file, and look at the visual parse tree or token hierachy. It also allows you to locate text that your grammar is failing to parse, which speeds up debugging.

Note: For large files / complicated parse trees, this can cause a lot of lag and may often crash IntelliJ. You can use the buttons on the left of the ANTLR4 preview pane to disable the visual parse tree generation to reduce this lag (the hierachy tends to be more useful for larger files anyway). By default the parse tree will be automatically regenerated after every edit - this can be disabled in the same plane.
## Working on the Scala code

In order to get IntelliJ to work, we must correctly mark the source directories in this project. Because we have generated source files (i.e. from antlr4), it slightly more involved than a normal project.

### Folders that should be marked as source
- src/main/java
- src/main/scala
### Folders that should be marked as generated source
- out

Compilation should be handled by `mill` using `./mill build`. This will generate the parser source file (from the antlr4 grammar file),
and then compile all the java and scala source files.


