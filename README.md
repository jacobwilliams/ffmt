# fformat

A formatter for free-form Fortran, written in modern Fortran.

It re-indents code (statements by nesting depth, continuation lines by alignment with brackets,
assignments, `only:` and `::`, comment lines like the code around them), adjusts the blanks around
operators, splits code lines that are too long, and can change the letter case of keywords and
operators and the spelling of relational operators. Trailing blanks are removed.

Two guarantees:

- **Safe.** The code tokens of the output are checked against the input (names and dot operators
  ignoring case). If formatting would change anything else, the file is left unchanged and an
  error is reported.
- **Stable.** Formatting a formatted file changes nothing.

Both are checked on every file of a corpus of about 10,700 open-source Fortran files.

## Example

Badly formatted input:

```fortran
MODULE Stats
USE Utils_Mod, ONLY: Swap, Clamp, Assert
USE, INTRINSIC :: iso_fortran_env, ONLY: wp=>real64, int32
IMPLICIT NONE
CONTAINS
FUNCTION Mean(x,mask) RESULT(m)
REAL(wp),INTENT(IN)::x(:)
LOGICAL,INTENT(IN),OPTIONAL::mask(:)
REAL(wp)::m
INTEGER::i,n
m=0.0_wp
n=0
DO i=1,SIZE(x)
IF(PRESENT(mask))THEN
IF(.NOT.mask(i)) CYCLE
END IF
m=m+x(i)
n=n+1
END DO
IF(n.GT.0) m=m/n
END FUNCTION Mean
SUBROUTINE Report(Name,Values,Unit)
CHARACTER(*),INTENT(IN)::Name
REAL(wp),INTENT(IN)::Values(:)
INTEGER,INTENT(IN)::Unit
WRITE(Unit,'(A,": mean = ",F0.4,", min = ",F0.4,", max = ",F0.4)') Name,Mean(Values),MINVAL(Values),MAXVAL(Values)
END SUBROUTINE Report
END MODULE Stats
```

After formatting with

```sh
fformat --case keywords=lower,operators=lower --relational symbols \
     --spaces comma=add,relational=add,plus-minus=add --sort-use --sort-use-only -l 100 stats.f90
```

```fortran
module Stats
use, intrinsic :: iso_fortran_env, only: int32, wp => real64
use Utils_Mod, only: Assert, Clamp, Swap
implicit none
contains
function Mean(x, mask) result(m)
    real(wp), intent(in) :: x(:)
    logical, intent(in), optional :: mask(:)
    real(wp) :: m
    integer :: i, n
    m = 0.0_wp
    n = 0
    do i = 1, SIZE(x)
        if (PRESENT(mask)) then
            if (.not.mask(i)) cycle
        end if
        m = m + x(i)
        n = n + 1
    end do
    if (n > 0) m = m/n
end function Mean
subroutine Report(Name, Values, Unit)
    character(*), intent(in) :: Name
    real(wp), intent(in) :: Values(:)
    integer, intent(in) :: Unit
    write(Unit, '(A,": mean = ",F0.4,", min = ",F0.4,", max = ",F0.4)') Name, Mean(Values), &
        MINVAL(Values), MAXVAL(Values)
end subroutine Report
end module Stats
```

Statements are indented by nesting depth (module bodies are not indented by default; see
`--indent-units`), keywords and `.NOT.` are lowercased while names such as `Mean`, `SIZE` and
`Unit` keep their case, `.GT.` becomes `>`, blanks are added around `=`, `::`, `,`, `+` and
relational operators, the `use` statements and their `only:` lists are sorted, and the long
`write` statement is split to fit 100 columns.

## Usage

```text
fformat [OPTIONS] PATH...
```

Files are formatted in place, and only rewritten if they change. A directory is searched
recursively for Fortran files (`.f90`, `.F90`, `.f95` ... `.F18`), skipping hidden entries and
`build`; `-` reads standard input and writes standard output, for editors. Line endings (LF or
CRLF) are kept. Problems such as an `end do` without a `do` are reported as warnings.

| Option | Effect |
|---|---|
| `-i, --indent N` | spaces per indentation level (default 4) |
| `--base-indent N` | columns added to every line, program units included (default 0) |
| `--indent-units` | indent the bodies of modules, submodules and programs |
| `--no-indent-procedures` | do not indent the bodies of subroutines and functions (interface bodies still are) |
| `--continuation align\|indent` | align continuation lines with brackets and `=` (default), or indent them by a fixed amount |
| `--continuation-indent N` | extra indentation of continuation lines (default 4) |
| `--no-indent-comments` | leave the indentation of comment lines unchanged |
| `--keep-column1-comments` | leave comment lines that start in column 1 there |
| `-l, --line-length N` | maximum line length (default 132, 0 for no limit) |
| `--no-split` | do not split long lines |
| `--sort-use` | sort each group of `use` statements by module name |
| `--sort-use-only` | sort the names in `only:` lists |
| `--relational symbols\|letters\|keep` | `.lt.` → `<`, `<` → `.lt.`, or unchanged (default) |
| `--spaces RULE=MODE,...` | blanks around operators (see below) |
| `--case WHAT=MODE,...` | letter case of `keywords`, `operators` or `all`: `lower`, `upper` or `keep` (default) |
| `--set KEY=VALUE` | any setting, as in a configuration file |
| `--config FILE`, `--no-config` | use FILE, or no configuration file |
| `--exclude PATTERN` | skip matching entries when searching directories (repeatable) |
| `--extensions LIST` | extensions of the files to format when searching directories |
| `-s, --stdout` | write the results to standard output |
| `-d, --diff` | write the changes as a unified diff; change no files |
| `-c, --check` | change no files; exit with status 1 if a file would change |
| `-q, --quiet` | do not report warnings |

Exit status: 0 on success, 1 if `--check` found files to format, 2 on errors.

### Configuration

Settings are read from the nearest `.fformat.toml` in or above a file's directory, or from
an `[extra.fformat]` table in an `fpm.toml` there; the nearest file is used on its own,
not merged with files further up. Command-line options override it.

```toml
indent = 3
base-indent = 0
indent-units = false
indent-procedures = true
column1-comments = "indent"     # or "keep"
continuation = "align"
line-length = 100
split = true
relational = "keep"
sort-use = false
sort-use-only = false

[spaces]             # see the table below
comma = "add"

[case]
keywords = "lower"

[files]              # for directories: these replace the defaults
extensions = ["f90", "F90"]
exclude = [".*", "build", "legacy/**"]
```

For code that is indented as a whole, with procedure bodies at the level of the procedure
statement and documentation comments in column 1 (the style of json-fortran and bspline-fortran),
use for example in `fpm.toml`:

```toml
[extra.fformat]
base-indent = 4
indent-procedures = false
column1-comments = "keep"
```

Comment lines are indented like the code around them. Two exceptions: a comment line that lines up
with the comment at the end of the line above stays lined up with it, and comment lines directly
after a statement that are indented more than it (such as FORD `!!` documentation under a
declaration) keep their extra indentation.

Exclude patterns are globs: `*` and `?` match within a path component, `**` across components. A
pattern without `/` matches an entry's name, one with `/` its path relative to the directory
searched. Links to directories are not followed.

### Blanks around operators

Each rule is `add` (at least one blank on each side, so extra blanks used for alignment are kept),
`remove` or `keep`. Rules are applied to tokens, so strings and comments are never changed, and
unary operators, `real*8`, `print *`, `(/ ... /)`, slashes in `data` and `common`, `format`
statements and CUDA `<<< >>>` are recognized.

| Rule | Applies to | Default |
|---|---|---|
| `assignment` | `=` outside brackets (`x = 1`, `integer :: n = 1`, `do i = 1, n`) | add |
| `keyword-args` | `=` inside brackets (`f(x=1)`, `intent=`, `len=*`) | remove |
| `pointer` | `=>` | add |
| `declarations` | `::` | add |
| `control-paren` | `if (`, `while (`, `select case (`, `where (`, ..., and `) then` | add |
| `power`, `concat` | `**`, `//` | remove |
| `slice-colon`, `percent` | `a(1:n)`, `a%b` | remove |
| `relational`, `logical` | `== /= < <= > >=`, `.eq.` ...; `.and. .or. .eqv. .neqv.` | keep |
| `plus-minus`, `mult-div` | binary `+ -`; binary `* /` | keep |
| `comma` | `,` (with `add`: no blank before, at least one after) | keep |

`all=MODE` sets every rule, e.g. `--spaces all=add,power=remove`. The defaults enforce the
conventions that are nearly universal in practice and leave the ones that vary between projects alone.

### Letter case

`--case keywords=lower` (or `upper`) changes keywords where their position makes them keywords:
the leading words of statements, declaration attributes, `intent(in)`, `kind=`/`len=`, `then`,
`only:`, `result()` and `bind()`. Fortran keywords are not reserved, so a word that names something
(`call open(x)`, a variable called `save`) keeps its case. `--case operators=lower` applies to dot
operators and logical constants (`.and.`, `.eq.`, `.true.`).

### Long lines

Code lines longer than the line length are split at a token boundary, preferring after a comma,
then after an operator, `=` or `::`, then after an opening bracket, and otherwise between any two
tokens; within the best kind, the rightmost break that fits is used. The new line is aligned like
any continuation line. Only code counts toward the length (compilers do not limit comments), so a
trailing comment may extend past it and moves with the last part. Strings, comment lines,
preprocessor lines and protected code are never split; a line that cannot be made to fit is
reported. Existing continuation lines are not joined or re-flowed.

### Sorting `use` statements

`--sort-use` sorts each group of `use` statements by module name, ignoring case. A group is a run of
`use` statements with nothing between them: a blank line, comment, preprocessor line or other
statement ends it, so statements under different `#if` branches never mix. A statement moves with
its continuation lines and trailing comment; statements of the same module keep their order.
Statements on a line with another (`;`) and protected statements stay where they are.

`--sort-use-only` sorts the names in each `only:` list by their local name (`c => x` sorts as `c`),
ignoring case. A list that needs sorting is rewritten and then split like any long line, with its
continuation lines aligned with the first name after `only:` (with `--no-split`, it is broken after
commas to fit the line length); a list that is already sorted keeps its layout. Statements with a comment before their last line
are left alone. The safety check accepts these reorderings and nothing else.

### Directives

Formatter directives in comments:

| Directive | Effect |
|---|---|
| `x = 1  !&` | leave this statement unchanged |
| `!&<` … `!&>` | leave the lines between them unchanged |

Preprocessor lines (cpp `#...`, fypp `#:`, `$:`, `@:`) are left as they are. Code under conditional
branches is indented as if each branch were the only one.

## Library

```fortran
use ffmt_format, only: format_text, format_options_t, FMT_OK
type(format_options_t) :: opts
character(len=:), allocatable :: out, errmsg
integer :: stat

opts%indent = 3
call format_text(source, opts, out, stat, errmsg)
```

## Building

Requires [fpm](https://fpm.fortran-lang.org) and a Fortran 2018 compiler (tested with gfortran 15).
The repository includes a [pixi](https://pixi.sh) environment with everything needed, on Linux,
macOS and Windows:

```sh
pixi run fpm build --profile release      # or: fpm build --profile release
pixi run test                             # unit tests
pixi run check-corpus FILE...             # safety, stability and structure checks on any files
pixi run format                           # format this repository with the tool itself
pixi run docs                             # API documentation with FORD, in doc/
```

Dependencies: [toml-f](https://github.com/toml-f/toml-f) for configuration files, and the regular
expression library [fregex](https://github.com/jacobwilliams/fregex). Directory listing uses
`opendir`/`readdir`/`closedir` from the C library through Fortran interfaces, with no C code.

## Other Fortran formatters

| Tool | Written in | Notes |
|---|---|---|
| [fprettify](https://github.com/pseewald/fprettify) | Python | free-form only; the reference for this project's feature set |
| [findent](https://sourceforge.net/projects/findent/) | C++ | indentation; fixed and free form, and conversion between them |
| [fprettier](https://github.com/fredjones42/fprettier) | Rust | a rewrite of fprettify |
| [ffmt](https://pypi.org/project/ffmt/) | Rust | Fypp, Doxygen and OpenACC/OpenMP support |
| [LFortran](https://lfortran.org) `lfortran fmt` | C++ | formats from the compiler's syntax tree |
| [Fortitude](https://github.com/PlasmaFAIR/fortitude) | Rust | a linter with automatic fixes, not a full formatter |

## License

MIT. See [LICENSE](LICENSE).
