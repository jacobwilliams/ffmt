# Plan: a modern Fortran source formatter

## 1. Goal

Build a new, better formatter for free-form Fortran, written in modern Fortran and built with fpm.
[fprettify](https://github.com/pseewald/fprettify) is the reference for which features a formatter
needs and for test material, but this is **not a port**. Where fprettify's design or behavior can
be improved, we improve it.

Requirements:

- **Cross-platform:** Linux, macOS and Windows. Only standard Fortran plus fpm, and no
  platform-specific assumptions (paths, line endings, shell). Anything that needs the OS goes behind
  one small, tested module.
- **Builds with fpm:** dependencies are fpm packages. The development environment is the pixi
  workspace in this repo (gfortran 15, fpm, Python for test generation) on all five pixi
  platforms.
- **Clean and modular:** small single-purpose modules, a library API usable without the CLI,
  explicit error handling (no `stop` in library code).
- **Safe:** formatting must never change the meaning of code (see §6, the token-equivalence check).

Reference checkout: `/Users/jwilliam/git/fprettify` at `efac952` (v0.3.7 plus 101 commits). fprettify
is GPLv3. We reuse ideas and test inputs, not code. Its example files can be used as test inputs if
we keep their license notice.

## 2. Status

| Step | State |
|---|---|
| Regex engine ([fregex](https://github.com/jacobwilliams/fregex)) | **Done.** Python `re` syntax and semantics; 16 API tests and 18,543 conformance cases generated from Python's `re`, including every fprettify regex. Its own repository, used as an fpm dependency. |
| Lexer, statements, block structure (`src/`) | **Done.** See below. |
| Indentation and layout, CLI basics | **Done.** See below. |
| Whitespace and case rules | **Done.** See below. |
| Line breaking, relational spelling | **Done.** See below. |
| Configuration, `--diff`, recursive mode, stdin | **Done.** See below. |
| Features from fprettier (milestone 7) | In progress: sorting `use` statements done. |
| Hardening and release (milestone 8) | Planned. |

Milestone 2 results:

- **Modules.** `ffmt_tokens` and `ffmt_lexer` tokenize the source. `ffmt_statements` groups
  tokens into statements and standalone lines. `ffmt_constructs` classifies each statement as
  opening, continuing or closing a construct. `ffmt_structure` gives each statement its nesting
  depth and reports diagnostics. `ffmt_io` reads and writes files as bytes.
- **Tests.** 20 unit tests (`pixi run test`). The corpus checker (`pixi run check-corpus FILE... |
  --files-from LIST`) was run on all 10,697 free-form files under `~/git`: 140 MB in about 4 s.
  - Round trip: every file is reproduced byte for byte by its tokens.
  - Structure: 11 files still get diagnostics, and every one is genuinely unbalanced or out of
    scope (fixed-form source, LFortran's deliberate error tests, fypp macro fragments, fprettify's
    invalid examples, one archived broken file).
- **Design decisions.**
  - The lexer never fails. Unterminated strings and similar errors are flagged, not rejected.
  - Tokens split across a continuation line (`fun&` / `&ction`) are marked as glued, so layout
    keeps them joined.
  - Continued strings are flagged. When the continuation line has no leading `&`, its leading
    blanks are part of the string and must not be re-indented.
  - Inline fypp counts as part of a name (`module m_${k}$`, `${t}$ function f`).
  - Preprocessor conditionals (cpp and fypp) are treated as alternatives. Each branch starts from
    the structure at the `#if`, and the structure at the end of the first branch continues after
    the `#endif`.
  - A main program without a `program` statement is recognized.
  - A UTF-8 byte-order mark is kept as a token of its own.

Milestone 3 results:

- **Modules.** `ffmt_options` (options type), `ffmt_layout` (indentation and continuation
  alignment), `ffmt_format` (`format_text`, the library entry point, with the token-equivalence
  check), `ffmt_cli` and `app/main.f90` (the `fformat` command: in place, `--stdout`,
  `--check`, exit codes 0/1/2), `ffmt_strings`.
- **Defaults** follow the workspace's fprettify settings: 4-space indent, module and program bodies
  not indented (`--indent-units` to change), continuation lines aligned, line length 132.
- **Continuation alignment**, in order of preference: the first item after the innermost open
  bracket; outside brackets, the first item after an assignment `=`/`=>` (not a rename or an
  initialization), after `only:`, or after a declaration's `::`; otherwise a fixed indent.
- **Line length.** If the preferred indentation would make a line longer than the limit, the fixed
  indent is used, then the line's original indentation (with a warning). Formatting never makes a
  line too long.
- **Comments.** Comment lines are indented like the code around them. A comment line that lined up
  with the comment on the line above (a continued trailing `!!` comment) stays lined up with it.
- **Directives.** `!&` keeps a statement unchanged; `!&<` ... `!&>` keeps a region unchanged.
- **Tests.** 32 unit tests. Corpus (10,697 files, two option sets): no internal errors (the
  token-equivalence check never failed) and every file is stable under a second pass.
- **Dogfooding.** This repository's sources are formatted by the tool with the default style.

Milestone 4 results:

- **Defaults from a survey** of 597 hand-written files in the user's own projects: enforce what is
  nearly universal there (`::`, `=>` and assignment `=` spaced; keyword-argument `=`, `**`, `//`,
  slice `:` and `%` unspaced; `if (`), keep what varies (commas 63% spaced, relational 66%,
  `+ -` 58% unspaced, `* /` 77% unspaced). Keywords are 100% lowercase, so case is kept by default.
- **Whitespace** (`ffmt_whitespace`): 14 rules, each `add` / `remove` / `keep`. `add` means at least
  one blank, so column alignment survives. Rules work on tokens and never touch strings, comments,
  line ends or continuation markers. Unary operators, type selectors (`real*8`), `print *`,
  `(/ /)`, `data`/`common` slashes, `format` statements and CUDA chevrons are recognized. Blanks are
  never removed where the two tokens would run together into different ones.
- **Case** (`ffmt_case`): keywords recognized by position (leading words, declaration attributes,
  `intent(in)`, `kind=`, `then`, `only:`, `result()`, `bind()`), so names spelled like keywords keep
  their case; dot operators and logical constants separately.
- **Safety check** now compares names and dot operators case-insensitively.
- **CLI**: `--spaces RULE=MODE,...` and `--case WHAT=MODE,...`.
- **Tests.** 44 unit tests. Corpus (10,697 files, three styles: defaults; every rule `add` with
  uppercase; every rule `remove` with lowercase): no internal errors, every file stable.

Milestone 5 results:

- **Splitting** (`ffmt_linebreak`): after a layout pass, each code line longer than the limit is
  split once at the best break (after a comma, then an operator / `=` / `::`, then an opening
  bracket, then anywhere between tokens; rightmost of the best kind that fits). The layout pass
  then re-aligns the continuation, and the two alternate until every line fits or nothing more can
  be split (at most 50 passes). Splitting one break per pass means later breaks are chosen at the
  final alignment column. Lines that cannot fit are reported.
- **Layout and splitting agree on widths:** only code counts (trailing comments may run past the
  limit, which compilers allow). When splitting is on, an aligned column is used as long as the line
  can be split to fit there, instead of falling back to the fixed indent.
- **Relational spelling** (`ffmt_relational`): `--relational symbols|letters|keep`, as a pre-pass so
  the whitespace rules see the final spelling. The safety check treats both spellings as equal.
  CUDA chevrons and protected code are left alone.
- **Directives** moved to `ffmt_directives`, shared by layout, splitting and relational conversion.
- **Tests.** 50 unit tests. Corpus (10,697 files, three styles including line lengths of 80 and 100,
  which split many lines, and both relational spellings): no internal errors, every file stable.
- **Not done:** joining short continuation lines or re-flowing a list across lines. Splitting can
  leave a short remainder line (e.g. one item of a long array constructor). A possible later option.

Milestone 6 results:

- **Directory listing** (`ffmt_fs`): pure Fortran with `bind(c)` interfaces to `opendir`,
  `readdir`, `closedir` and `getcwd`, which POSIX systems and MinGW-w64 provide. gfortran's
  preprocessor defines no OS macros, so `struct dirent`'s layout is detected at run time: the
  offset of `d_name` is where the `.` and `..` entries show their names (Linux 19, macOS 21,
  FreeBSD 24, MinGW and 32-bit-inode macOS 8). `d_type` is used where the layout has it, so links
  to directories are recognized and not followed. Verified on macOS arm64 only; Linux and Windows
  are untested (Docker is available here for a Linux check).
- **Walking** (`ffmt_walk`): extensions and glob excludes (via fregex), sorted, links not followed.
- **Configuration** (`ffmt_config`, with toml-f): nearest `.fformat.toml` or
  `fpm.toml` `[extra.fformat]`; `[spaces]`, `[case]`, `[files]` tables.
- **Settings** (`ffmt_settings`): one `apply_setting(key, value)` used by both the configuration
  file and the command line, so every option is validated in one place; precedence defaults <
  config < command line.
- **Diff** (`ffmt_diff`): unified diff by Hirschberg's linear-space LCS over hashed lines. Checked by
  applying 300 corpus diffs with `patch`: all reproduce the formatted output.
- **CLI**: directories, `-` for stdin/stdout, `--diff`, `--config`, `--no-config`, `--set`,
  `--exclude`, `--extensions`.
- **Tests.** 59 unit tests, with fixtures in `fixtures/` (outside `test/`, which fpm compiles).
- This repository's own `.fformat.toml` excludes the fixtures; `pixi run format` formats it.

Milestone 7 progress:

- **Sorting `use` statements** (`ffmt_use_sort`): `sort-use` and `sort-use-only`, a text pre-pass
  like relational spelling. An `only:` list that needs sorting is joined onto one line and left to
  line splitting, or, with splitting off, broken after commas to fit the line length (measured at
  the input's indentation); continuation alignment then lines the names up after `only:`. A
  sorted list keeps its layout, so the result is stable. The safety check now
  compares a signature of the code tokens in which, when either option is on, the `use` statements
  of a run (between other statements and preprocessor lines) and the items of each `only:` list are
  sorted. 68 unit tests; corpus (8,896 files, both options on in the second style): no internal
  errors, every file stable.

After milestone 6, for the user's own style (json-fortran, bspline-fortran): `base-indent`
(indent everything, program units included), `indent-procedures = false` (interface bodies are
still indented), `column1-comments = "keep"`, comment blocks after a statement keep extra
indentation (FORD `!!` docs), and `generic :: name => a, &` continuations align after `=>`. With
`base-indent = 4`, `indent-procedures = false` and `column1-comments = "keep"`, 822 of json-fortran's
17,715 lines would change; the rest are mainly hand-aligned tables (e.g. `generic` binding lists,
which `!&<` / `!&>` can protect), `contains` inside a type (indented like the components there),
and lines indented inconsistently in the source.

## 3. What a formatter needs (taken from fprettify)

| Feature | fprettify behavior | Improvement to aim for |
|---|---|---|
| Indentation | by scope (program units, `if`/`do`/`select`/`block`/`associate`/`where`/`forall`/`type`/`interface`/`enum`), options for module/program bodies and fypp blocks | drive it from one table of constructs, including the ones fprettify misses (`critical`, `change team`, `select rank`, `do concurrent` labels, F2023 additions) |
| Continuation lines | align to brackets or `=`, or a fixed indent | same, plus a hanging-indent style option |
| Whitespace | presets 0–4 plus per-operator overrides (comma, assignment, relational, logical, `+ -`, `* /`, `//`, `%`, print/read, intrinsics, declarations) | same knobs, decided per token instead of by regex over the joined line text, so strings, comments and CUDA chevrons can't be mangled |
| Case | upper/lower/keep, separately for keywords, intrinsic procedures/modules, operators and constants | same, based on a token's role rather than its spelling (e.g. `real` the type vs `real()` the intrinsic) |
| Relational operators | `.lt.` ↔ `<` | same |
| Line length | splits long lines at spaces or commas; moves inline comments | split at token boundaries with a preference order; never inside a string |
| Directives | `!&` (skip a line), `!&<` / `!&>` (formatting off/on), `! fprettify: <options>` in the file | `!&`, `!&<`, `!&>` (done); options in the file later |
| Preprocessors | passes through cpp `#` and fypp `#:`/`$:`/`@:` lines, can indent fypp blocks | same |
| OpenMP / OpenACC | keeps `!$omp` / `!$ ` conditional lines | also format the Fortran code in `!$ ` conditional lines |
| Configuration | `.fprettify.rc` in the home directory and parent directories; command-line flags | a TOML config (`.fformat.toml` or `[extra.fformat]` in `fpm.toml`) using toml-f |
| CLI | in place, `--stdout`, `--diff`, recursive with excludes and extensions | add `--check` (non-zero exit if a file would change, for CI and pre-commit), stdin/stdout for editors, consistent exit codes |

fprettify weaknesses to avoid, from reading its source:

- It recognizes and rewrites code with about 85 regexes applied to whole joined lines, so every rule
  has to work around strings and comments separately.
- It runs two full passes (whitespace, then indentation) over a text buffer.
- It reports errors through exceptions with inconsistent exit codes.
- Its behavior depends on configuration spread across command-line arguments, configuration files
  and file annotations, with no single resolved options object.

## 4. Architecture

```
source text
  └─ reader       physical lines; CRLF/LF detection (output keeps the input's line endings)
  └─ lexer        tokens with kind, text, position, and the comments and blank lines around them;
                  handles strings, continuations (&), ;, comments, cpp/fypp lines, !$ sentinels
  └─ statements   tokens grouped into logical statements; each statement is classified
                  (opens/continues/closes a scope, declaration, use, I/O, ...) by matching
                  token patterns, with fregex where a pattern is easiest
  └─ rules        pure passes over a statement's tokens: case, operator replacement,
                  spacing between tokens
  └─ layout       indentation from a scope stack; continuation alignment; line breaking
  └─ printer      emits text that keeps comments, blank lines, directives and
                  formatting-off regions exactly as they were
```

Key ideas:

- **Tokens, not text:** every rule sees typed tokens, so strings and comments can't be changed by
  accident, and rules stay short.
- **One pass:** classification, rules and layout run once per statement. There is no
  separate re-parse for indentation.
- **Options are resolved once:** a `format_options_t` built from defaults, then the config file, then
  the command line, then in-file directives. Each layer can only override settings it states
  explicitly.
- **Library first:** `format_text(src, opts, out, stat, errmsg)` is the core API. The CLI is a thin
  driver around it.

### Modules

| Module | Responsibility | State |
|---|---|---|
| `ffmt_tokens`, `ffmt_lexer` | token types; free-form tokenizer | done |
| `ffmt_statements` | grouping tokens into statements and standalone lines | done |
| `ffmt_constructs` | classifying statements as opening, continuing or closing constructs | done |
| `ffmt_structure` | nesting depth and structure diagnostics | done |
| `ffmt_layout` | indentation, continuation alignment, directives; later line breaking | done (no line breaking yet) |
| `ffmt_format` | `format_text` and the token-equivalence check | done |
| `ffmt_options` | options type | done |
| `ffmt_cli`, `app/main.f90` | argument parsing and file processing | basics done |
| `ffmt_io`, `ffmt_strings` | file I/O as bytes; string helpers | done |
| `ffmt_whitespace`, `ffmt_case` | blanks around operators; letter case | done |
| `ffmt_relational` | relational operator spelling | done |
| `ffmt_linebreak` | splitting long lines | done |
| `ffmt_directives` | `!&`, `!&<`, `!&>` | done |
| `ffmt_fs`, `ffmt_walk` | directory listing through C library interfaces; finding files | done |
| `ffmt_config`, `ffmt_settings` | configuration files; named settings | done |
| `ffmt_diff` | unified diff for `--diff` | done |
| `ffmt_use_sort` | sorting `use` statements and `only:` lists | done |

**Portable file system access.** Standard Fortran cannot list directories, and `stdlib_system`
(checked at stdlib `940bba44`) has `is_directory` and path helpers but no listing. `ffmt_fs` is
written in Fortran with `bind(c)` interfaces to the C library's `opendir`/`readdir`/`closedir`
(available on POSIX and MinGW-w64) and detects the `struct dirent` layout at run time. No C source
and no shell are used.

### Dependencies

| Package | Use |
|---|---|
| [`fregex`](https://github.com/jacobwilliams/fregex) | statement classification, directives, `--exclude` patterns |
| [`toml-f`](https://github.com/toml-f/toml-f) | config files and `fpm.toml` integration |
| [`test-drive`](https://github.com/fortran-lang/test-drive) | tests (dev dependency) |
| `stdlib` | not needed: `ffmt_fs` covers directory listing |

## 5. Milestones

1. **fregex**: done.
2. **Lexer and statements:** tokenizer, logical statements and classification. Test: re-printing
   the tokens with no rules applied gives back the input byte for byte, for every test file.
   *(Done.)*
3. **Indentation and layout:** scope stack, continuation alignment, formatting-off regions and
   directives. *(Done, with a basic CLI: in place, `--stdout`, `--check`.)*
4. **Whitespace and case rules:** per-operator options and keyword/operator case. *(Done.
   Relational spelling, `.lt.` and `<`, was added with milestone 5.)*
5. **Line breaking:** splitting long lines. *(Done; trailing comments stay on the last part.)*
6. **Configuration and CLI:** TOML config, `--diff`, recursive mode with excludes, editor
   integration (stdin/stdout). *(Done. Options inside a source file, e.g. a comment directive,
   are not supported.)*
7. **Features from fprettier:** ideas taken from
   [fprettier](https://github.com/fredjones42/fprettier) (reviewed at `179542e`, v0.5.0), a Rust
   rewrite of fprettify. In this order:
   1. *(Done.)* **Sorting `use` statements** (off by default): `sort-use` sorts the `use`
      statements of a group alphabetically, ignoring case. A group is a run of `use` statements with nothing between
      them; a blank line, comment, preprocessor line or other statement ends it, so statements in
      different `#if` branches never mix. A statement moves with its continuation lines and trailing
      comment, and protected statements (`!&`, `!&<`…`!&>`) stay where they are. `sort-use-only`
      sorts the names of an `only:` list by their local name (`c => x` sorts as `c`), then lets
      line splitting re-break the list; a statement with a comment on any line but its last is left
      alone. The safety check needs to accept reordered `use` statements and `only:` items.
   2. **More case categories:** `procedures` (intrinsic procedures where they are called, e.g.
      `size(a)`), `constants` (names from `iso_fortran_env` and `iso_c_binding`, e.g. `output_unit`,
      `c_int`) and `types` (`kind=` values, literal kind suffixes `2_int64`, exponent letters
      `1.0e-3`). Recognize them by position, as for keywords, so that a local variable named like an
      intrinsic keeps its case. Needs F2023 tables of intrinsic procedures and constants.
   3. **Comment spacing:** `comment-spacing = N` sets the blanks before an inline comment. It must
      leave trailing comments that are aligned in columns alone.
   4. **Settings in a file:** a comment such as `! fformat: indent=2, spaces.comma=add`
      sets options for that file, applied after the configuration file and the command line. Only
      the first such comment counts.
   5. **Merged configuration files:** read every configuration file from the home directory down to
      the file's directory, each overriding only the settings it states, instead of only the
      nearest file.
   6. **Indenting fypp blocks** (option): indent the code inside `#:if`, `#:for`, `#:def`, ... and
      the directive lines with it. Must work with the current treatment of preprocessor branches as
      alternatives.

   Smaller fixes to make along the way:
   - **User-defined operators:** a whitespace rule for user-defined operators such as `.plus.`,
     like the one for logical operators.
   - **Fortran 2023:** recognize `enumeration type` ... `end enumeration type` as a construct, and
     space the `?` and `:` of conditional expressions `(c ? a : b)`.
   - **Language limits:** warn about input lines longer than 10,000 characters or statements longer
     than 1,000,000 (F2023 6.3.2), and make sure line splitting does not create more continuation
     lines than a compiler allows (255 before F2023).
   - **Closed pipe:** check that `--check` or `--stdout` into a closed pipe (`... | head`) does not
     crash.

   Considered and not taken: whitespace "levels" 0–4 (our per-rule settings cover them) and
   fprettify compatibility. Parallel processing is deferred to milestone 8.
8. **Hardening:** run on large codebases (stdlib, fpm, json-fortran, cp2k, wannier90); fuzzing;
   performance; release.
   - **Performance (2026-10-24):** profiling with macOS `sample` found most time in small string
     allocations. Fixed, about 3.7x faster overall: `to_s` without an internal `write`; the safety
     check compares tokens directly (signatures only when `use` statements are sorted) and reuses
     the tokens of the first pass; line pieces refer to source ranges instead of copying text;
     no per-token copies in `classify_statement` and `statement_gaps`; split passes and
     `long_code_lines` are skipped when no line is longer than the limit. Left: a file with long
     lines is formatted again in full for each split pass (about 15% of the time on fortranpack,
     where 25 of 276 files, a third of the bytes, need one or two). Re-laying out only the split
     statements would remove most of it, but the layout of a statement depends on state from
     earlier lines (depth, comment columns), so it needs care to stay stable. Next costs after
     that: `tokenize` (about a quarter) and the remaining `token_lower` copies.
   - **Parallel formatting (later):** format several files at once with OpenMP, as fprettier does
     (`-j`; about 3x faster than its single-threaded run on 10 cores in `bench/benchmark.py`).
     The formatting path has no shared state, and a `run` split into three passes worked: find the
     files and their options serially, format them in an `!$omp parallel do schedule(dynamic)`
     with each file's messages and stdout text buffered, then print in file order; `-j/--jobs N`
     (0 = one per processor) and `!$` sentinels so a build without OpenMP stays serial. Tried and
     reverted (2026-10-24) because of a gfortran bug (15.3 and 16.2): the length of a
     `character(len=:), allocatable` function result is kept in a hidden `static` variable at
     each call site (`static integer(kind=8) slen.N` in `-fdump-tree-original`), so threads
     corrupt each other's strings. fformat has about 200 such sites. Also, a deferred-length
     variable that a parallel region references is copied with a length that hasn't been set yet.
     With `openmp = "*"` in `fpm.toml`, test-drive runs the tests in parallel too, and they fail.
     Options: report the bug to GCC and wait for a fix; turn functions with deferred-length
     results into subroutines with an `intent(out)` argument, with a CI check that the dumps
     contain no `static ... slen`; or run several fformat processes at once instead of threads.

## 6. Testing

- **Unit tests** (test-drive) for each module.
- **Golden files:** input/expected-output pairs per feature and option set, written for this
  project. (fprettify's example files are GPLv3, so they are used only from a local checkout in
  corpus runs, never copied into this MIT-licensed repository.)
- **Token-equivalence check:** for every test and corpus file, the token stream after formatting
  must equal the token stream before, ignoring whitespace, the case of case-insensitive tokens,
  equivalent operator spellings, and continuation markers. Any difference means formatting changed
  the meaning of the code, which is always a bug. This check runs in CI on every file.
- **Idempotence:** formatting a formatted file changes nothing.
- **Corpus runs:** open-source codebases (fprettify's `testsuites.config` lists RosettaCode, cp2k,
  wannier90 and FLAP at pinned versions) checked with token equivalence and idempotence. Their
  formatted output is not compared to fprettify's, since we will differ on purpose.
- **CI:** GitHub Actions on Linux, macOS and Windows using the pixi environment.

## 7. Conventions

- Fortran 2018; `implicit none` everywhere (fpm enforces it); `private` by default; `intent` on
  every argument; `pure`/`elemental` where possible.
- Errors are returned through `stat`/`errmsg` or an error type, and never cause a `stop` in library code.
- gfortran 15 bug to remember: a dummy procedure whose result is `character(:), allocatable`, placed
  before a `character(*)` dummy argument, crashes at run time. Put such procedure arguments after
  string arguments (fregex's `sub(string, fn)` does).
- gfortran 15 internal compiler error: a deferred-length character function result passed directly
  as an actual argument (or in an array constructor) inside a recursive internal procedure, or an
  internal function's deferred-length result used in a structure constructor. Assign it to a local
  variable first (see `ffmt_walk`, `ffmt_format`).
- An allocatable array assigned from a function whose result has lower bound 0 becomes 1-based.
  Use explicit-bounds arrays when the bounds matter (see `ffmt_diff`).
- Avoid editing Fortran with Python string literals: `'\'` sequences are interpreted by Python.
- Exit codes from the program use `stop code, quiet=.true.`, not `error stop`, so normal non-zero
  results (e.g. `--check`) don't print a backtrace.
- **Documentation:** FORD's default style. `!!` lines right after the statement they document
  (module, type, procedure), and trailing `!!` comments on arguments, components and constants.
  `pixi run docs` builds the API docs and must stay warning-free.
- **Formatting:** the repository's sources are formatted by the tool itself with the default style.
- **License:** MIT, for both packages.

## 8. Decisions and open questions

Decided:

- **Names:** the project and executable are `fformat` (renamed from `prettify-fortran`, then from
  `ffmt`, which another Fortran formatter already uses), the modules keep the `ffmt_` prefix,
  and the regex package is `fregex`.
- **License:** MIT.
- **Compatibility:** no fprettify compatibility (flags, config files or output).
- **Documentation:** FORD's default style (see §7).

Open:

1. **Fixed-form source:** out of scope for now. Do we want it later?
2. **Re-flowing continuation lines** (joining short lines, packing lists): worth adding as an option?
