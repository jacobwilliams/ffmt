# fregex

Regular expressions for modern Fortran, using the same pattern syntax as Python's `re` module.

- Pure standard Fortran 2018, with no dependencies. Builds with [fpm](https://fpm.fortran-lang.org) on any platform.
- Backtracking engine with Python semantics: leftmost match, greedy/lazy/possessive quantifiers,
  and Python's rules for empty matches in `find_all`, `split` and `sub`.
- Tested against Python's `re` on about 18,500 generated cases (see [Testing](#testing)).

## Example

```fortran
program demo
    use fregex
    implicit none
    type(regex_t) :: re
    type(match_t) :: m

    re = regex('(?P<name>\w+)\s*::\s*(?P<vars>.*)', RE_IGNORECASE)
    if (re%search('integer :: i, j', m)) then
        print *, m%group('name')                  ! integer
        print *, m%group('vars'), m%start('vars') ! i, j   12
    end if

    print *, re_sub('\s*([=+])\s*', 'x=y+1', ' \1 ')  ! x = y + 1
end program demo
```

Add it to an fpm project with:

```toml
[dependencies]
fregex = { path = "fregex" }   # or a git URL once it has its own repository
```

## Syntax

Patterns are written like Python raw strings: a backslash in a Fortran string is just a backslash.

| Syntax | Meaning |
|---|---|
| `.` `^` `$` | any byte except newline, start, end (`$` also matches before a final newline) |
| `[abc]` `[^a-z]` | character class, negated class (`]` first is literal, `\d \w \s` allowed inside) |
| `\d \D \w \W \s \S` | digit, word character `[A-Za-z0-9_]`, whitespace, and their negations |
| `\b \B \A \Z` | word boundary, not a boundary, start of string, end of string |
| `\n \t \r \f \v \a \xhh \ooo` | byte escapes; any other escaped punctuation is literal |
| `*` `+` `?` `{m}` `{m,n}` `{m,}` `{,n}` | quantifiers, greedy; add `?` for lazy, `+` for possessive |
| `a\|b` | alternation, tried left to right |
| `(...)` `(?:...)` `(?P<name>...)` `(?<name>...)` | capturing, non-capturing and named groups |
| `\1` `(?P=name)` | backreferences |
| `(?=...)` `(?!...)` `(?<=...)` `(?<!...)` | lookahead and lookbehind (lookbehind may have a variable width) |
| `(?>...)` | atomic group |
| `(?ims)` `(?i-m:...)` | inline flags, for the rest of the group or for one group |
| `(?#...)` | comment |

Flags: `RE_IGNORECASE`, `RE_MULTILINE`, `RE_DOTALL` (add them together to combine).

Matching works on bytes with ASCII rules, like Python's `re` on `bytes`. UTF-8 text can be searched,
but `.` and character classes match single bytes, and case-insensitive matching only folds ASCII letters.

## API

Positions are 1-based and spans are inclusive: `string(m%start():m%end())` is the matched text.
An empty match has `end = start - 1`. A group that did not take part in the match has
`start = 0`, `end = -1` and empty text.

| Procedure | Description |
|---|---|
| `re = regex(pattern [, flags, stat, errmsg])` | compile (also `call re%compile(...)`) |
| `re%search(string [, m, pos, stat, errmsg])` | leftmost match at or after `pos` → logical |
| `re%match(string [, m, pos, ...])` | match starting exactly at `pos` |
| `re%fullmatch(string [, m, pos, ...])` | match from `pos` to the end of the string |
| `re%find_all(string [, stat, errmsg])` | all non-overlapping matches → `type(match_t)(:)` |
| `re%split(string [, maxsplit, ...])` | pieces between matches, plus captured groups → `type(string_t)(:)` |
| `re%sub(string, repl [, count, nsubs, ...])` | replace with a template (`\1`, `\g<name>`, `\g<0>`, `\n`, ...) |
| `re%sub(string, fn [, count, nsubs, ...])` | replace with `fn(m)`, a function returning `character(:), allocatable` |
| `re%group_count()`, `re%group_index(name)`, `re%pattern()` | information about the pattern |
| `re%set_step_limit(limit)` | limit backtracking work per search (default 10 million steps) |
| `m%group([k])`, `m%group(name)` | text of a group (default 0, the whole match) |
| `m%start(k)`, `m%end(k)`, `m%matched(k)` | span of a group, and whether it matched (also by name) |
| `re_search`, `re_match`, `re_fullmatch`, `re_find_all`, `re_split`, `re_sub` | one-off versions taking a pattern string |
| `re_escape(string)` | escape special characters |

`re%sub` takes the subject string first, unlike Python's `re.sub(repl, string)`. This matches the
rest of the API, and it avoids a gfortran 15 bug: a function argument with a deferred-length result
that comes before a `character(*)` argument crashes at run time.

### Errors

Procedures that can fail have optional `stat` and `errmsg` arguments. If `stat` is absent and an
error occurs, the program stops with the message. Status codes:

| Code | Meaning |
|---|---|
| `RE_OK` | success |
| `RE_ERR_SYNTAX` | invalid pattern; the message gives the 1-based position, e.g. `missing ), unterminated subpattern at position 2` |
| `RE_ERR_TEMPLATE` | invalid replacement template |
| `RE_ERR_STEP_LIMIT` | the search exceeded the step limit (e.g. `(a+)+b` on a long run of `a`s) |
| `RE_ERR_NOT_COMPILED` | the regex was used before a successful compile |

The `re_*` convenience functions stop on an invalid pattern.

## Differences from Python's `re`

- Lookbehind may have a variable or unbounded width, e.g. `(?<=\bcall\s+)\w+`. Python requires a fixed width.
- `(?<name>...)` is accepted as well as `(?P<name>...)`.
- Inline global flags such as `(?i)` apply from where they appear to the end of the enclosing group. Python only allows them at the start of the pattern.
- `(?x)` (verbose mode) and `\N{...}` are not supported. The `a`, `u` and `L` flags are accepted and ignored.
- `split` returns an empty string for a capture group that did not match, where Python returns `None`.
- Repeat counts are limited to 100,000.

## Design

| Module | Role |
|---|---|
| `fregex` | public API |
| `fregex_parser` | pattern → syntax tree, with Python-style error messages |
| `fregex_compiler` | syntax tree → VM program; also computes the possible first bytes of a match and whether it is anchored, used to skip start positions |
| `fregex_vm` | backtracking VM: one stack holds choice points and the old values needed to undo captures; lookarounds and atomic groups run as nested calls |
| `fregex_match`, `fregex_template` | match results; replacement templates |
| `fregex_charset`, `fregex_string`, `fregex_program` | 256-bit byte sets, string helpers, instruction set |

A repeated sub-pattern that can match the empty string stops repeating after an iteration that
consumes nothing, as Python's engine does. This also rules out infinite loops.

## Testing

```sh
fpm test
```

There are two suites:

- `api`: hand-written tests of the API.
- `conformance`: cases generated by `tools/gen_conformance.py`, which runs Python's `re` on byte strings
  and records the expected results in `test/data`. The cases come from four sources:
  - hand-written feature cases;
  - patterns that must be rejected;
  - every regex in [fprettify](https://github.com/pseewald/fprettify), applied to its example Fortran files;
  - 1,500 random patterns, each tried on several random subjects.

To regenerate the data, which needs Python 3.11 or later and optionally an fprettify checkout:

```sh
python -I tools/gen_conformance.py --fprettify ../../fprettify
```

## License

MIT. See [LICENSE](LICENSE).

API documentation can be built with [FORD](https://github.com/Fortran-FOSS-Programmers/ford): `ford ford.md`
(or `pixi run regex-docs` from the parent repository).
