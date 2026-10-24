#!/usr/bin/env python3
"""Generate conformance test vectors for fregex from Python's `re` module.

Python's `re` on byte strings has the same (ASCII) semantics as fregex, so it is used as the
reference implementation. The output files are read by test/test_conformance.f90, so running
the Fortran tests does not need Python.

Each non-comment line is one case:

    <op> <flags> <pattern> <subject> <repl> => <expected ...>

Strings are hex-encoded bytes, with '.' for the empty string. Flags use fregex's values
(1 = IGNORECASE, 2 = MULTILINE, 4 = DOTALL). Positions are 1-based and inclusive, with
"0 -1" for a group that did not match. Expected results by op:

    compile                    error
    search, match, fullmatch   none | <start end> for group 0..n
    findall                    <count> <start end> of each match
    split                      <count> <hex piece> ...
    sub                        <nsubs> <hex result>

Usage: gen_conformance.py [--fprettify DIR] [--out DIR]
"""

import argparse
import importlib
import random
import re
import sys
import warnings
from pathlib import Path

FLAG_MAP = {1: re.IGNORECASE, 2: re.MULTILINE, 4: re.DOTALL}


def py_flags(flags):
    out = 0
    for bit, flag in FLAG_MAP.items():
        if flags & bit:
            out |= flag
    return out


def hx(b):
    return b.hex() if b else "."


def spans(m):
    out = []
    for g in range(m.re.groups + 1):
        s, e = m.span(g)
        out += ["0", "-1"] if s < 0 else [str(s + 1), str(e)]
    return " ".join(out)


class Writer:
    def __init__(self, path):
        self.path = path
        self.lines = []

    def comment(self, text):
        self.lines.append("# " + text)

    def case(self, op, flags, pattern, subject=b"", repl=b""):
        """Run one case through Python's re and record the expected result.

        Returns False (recording nothing) if Python rejects the pattern and op is not 'compile'.
        """
        try:
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                rx = re.compile(pattern, py_flags(flags))
        except re.error:
            if op == "compile":
                self._add("compile", flags, pattern, b"", b"", "error")
                return True
            return False
        if op == "compile":
            raise ValueError(f"pattern {pattern!r} is valid in Python")
        if op in ("search", "match", "fullmatch"):
            m = getattr(rx, op)(subject)
            expected = spans(m) if m else "none"
        elif op == "findall":
            ms = list(rx.finditer(subject))
            expected = " ".join([str(len(ms))] + [f"{m.start() + 1} {m.end()}" for m in ms])
        elif op == "split":
            parts = [p if p is not None else b"" for p in rx.split(subject)]
            expected = " ".join([str(len(parts))] + [hx(p) for p in parts])
        elif op == "sub":
            out, n = rx.subn(repl, subject)
            expected = f"{n} {hx(out)}"
        else:
            raise ValueError(op)
        self._add(op, flags, pattern, subject, repl, expected)
        return True

    def _add(self, op, flags, pattern, subject, repl, expected):
        self.lines.append(f"{op} {flags} {hx(pattern)} {hx(subject)} {hx(repl)} => {expected}")

    def write(self):
        self.path.write_text("\n".join(self.lines) + "\n")
        print(f"wrote {sum(not l.startswith('#') for l in self.lines)} cases to {self.path}")


# --------------------------------------------------------------------------------------------
# hand-written feature cases: (pattern, flags, subjects)
# --------------------------------------------------------------------------------------------

FEATURES = [
    (rb"abc", 0, [b"abc", b"xabcx", b"ab", b""]),
    (rb"a.c", 0, [b"abc", b"a\nc", b"ac"]),
    (rb"a.c", 4, [b"a\nc"]),
    (rb"^abc$", 0, [b"abc", b"abc\n", b"abc\n\n", b"xabc"]),
    (rb"^b", 2, [b"a\nb", b"b"]),
    (rb"a$", 2, [b"a\nb", b"ba\n"]),
    (rb"\Aa\Z", 0, [b"a", b"a\n"]),
    (rb"$", 0, [b"", b"\n", b"a\n"]),
    (rb"^", 2, [b"a\nb\n"]),
    (rb"\bfoo\b", 0, [b"foo", b"a foo b", b"foobar", b"_foo"]),
    (rb"\Boo\B", 0, [b"foobar", b"oo"]),
    (rb"\B", 0, [b"", b"a", b" "]),
    (rb"\b", 0, [b"", b"ab cd"]),
    (rb"[abc]+", 0, [b"xxcabz"]),
    (rb"[^abc]+", 0, [b"abxyzc"]),
    (rb"[a-cx-z]+", 0, [b"--abyz--"]),
    (rb"[]a]+", 0, [b"]a]b"]),
    (rb"[^]a]+", 0, [b"]a]bc"]),
    (rb"[a-]+", 0, [b"-a-b"]),
    (rb"[-a]+", 0, [b"-a-b"]),
    (rb"[\d.]+", 0, [b"v1.25b"]),
    (rb"[\w\s]+", 0, [b"!a b_1!"]),
    (rb"[\W]+", 0, [b"ab!? cd"]),
    (rb"\d+\D\w\W\s\S", 0, [b"123ab cd", b"12x_ !z"]),
    (rb"\x41\x62\t\n", 0, [b"Ab\t\n"]),
    (rb"\101\0", 0, [b"A\x00"]),
    (rb"[\x41-\x43]+", 0, [b"zABCD"]),
    (rb"\.\*\+\?\(\)\[\]\{\}\|\^\$\\", 0, [b".*+?()[]{}|^$\\"]),
    (rb"a{3}", 0, [b"aa", b"aaaa"]),
    (rb"a{2,3}", 0, [b"a", b"aaaaa"]),
    (rb"a{2,}", 0, [b"a", b"aaaaa"]),
    (rb"a{,2}", 0, [b"aaa"]),
    (rb"a{,}", 0, [b"aaa"]),
    (rb"a{2,3}?", 0, [b"aaaa"]),
    (rb"x{", 0, [b"x{"]),
    (rb"x{}", 0, [b"x{}"]),
    (rb"x{a}", 0, [b"x{a}"]),
    (rb"x{1,a}", 0, [b"x{1,a}"]),
    (rb"{", 0, [b"{"]),
    (rb"a*", 0, [b"", b"baaa", b"aaab"]),
    (rb"a*?b", 0, [b"aaab"]),
    (rb"a+?", 0, [b"aaa"]),
    (rb"a??b", 0, [b"ab", b"b"]),
    (rb"<.*>", 0, [b"<a><b>"]),
    (rb"<.*?>", 0, [b"<a><b>"]),
    (rb"a|b|c", 0, [b"xcba"]),
    (rb"ab|a", 0, [b"ab"]),
    (rb"a|ab", 0, [b"ab"]),
    (rb"(a|ab)(c|bcd)(d*)", 0, [b"abcd"]),
    (rb"|a", 0, [b"a"]),
    (rb"a|", 0, [b"b"]),
    (rb"(a)(b)?", 0, [b"a", b"ab"]),
    (rb"(a|(b))+", 0, [b"ab", b"ba"]),
    (rb"((a)|b)*", 0, [b"ab", b"ba"]),
    (rb"(a*)*", 0, [b"b", b"aab"]),
    (rb"(a*)+", 0, [b"b", b"aab"]),
    (rb"(a|)*", 0, [b"aab"]),
    (rb"(a*?)*", 0, [b"aab"]),
    (rb"(|a)*?b", 0, [b"aab"]),
    (rb"(a*)*?b", 0, [b"aab"]),
    (rb"(?:a*)*b", 0, [b"aaab"]),
    (rb"(a?){3}", 0, [b"aa"]),
    (rb"(a?){2,4}", 0, [b"a"]),
    (rb"(?:(a)|b)*", 0, [b"ab"]),
    (rb"(?P<first>\w+) (?P<last>\w+)", 0, [b"Jane Doe"]),
    (rb"(?P<x>a)(?P=x)", 0, [b"aa", b"ab"]),
    (rb"(a)\1", 0, [b"aa", b"ab"]),
    (rb"(a)|\1", 0, [b"x"]),
    (rb"(a*)b\1", 0, [b"aabaa", b"aaba", b"b"]),
    (rb"(\w+) \1", 1, [b"Hello HELLO"]),
    (rb"(?:ab)+", 0, [b"ababa"]),
    (rb"(?=ab)a", 0, [b"ab", b"ac"]),
    (rb"a(?!b)", 0, [b"ab", b"ac"]),
    (rb"(?<=a)b", 0, [b"ab", b"cb"]),
    (rb"(?<!a)b", 0, [b"ab", b"cb", b"b"]),
    (rb"(?<=ab|cd)x", 0, [b"abx", b"cdx", b"bx"]),
    (rb"(?<=\bPRINT)\s*(\*,?)\s*", 1, [b"print *, x", b"PRINT*,x", b"xprint *"]),
    (rb"(?=(a+))a*b\1", 0, [b"baaabac"]),
    (rb"(?=(\w+))", 0, [b"ab cd"]),
    (rb"(?!(a))b", 0, [b"b"]),
    (rb"(?<=(a))b", 0, [b"ab"]),
    (rb"(?>a+)b", 0, [b"aaab"]),
    (rb"(?>a+)ab", 0, [b"aaab"]),
    (rb"(?>a|ab)c", 0, [b"abc", b"ac"]),
    (rb"a++b", 0, [b"aab"]),
    (rb"a++a", 0, [b"aaa"]),
    (rb"a?+a", 0, [b"a", b"aa"]),
    (rb"a{1,2}+a", 0, [b"aa", b"aaa"]),
    (rb"(?i)abc", 0, [b"ABC", b"aBc"]),
    (rb"abc", 1, [b"ABC", b"aBc"]),
    (rb"[a-c]+", 1, [b"xABCx"]),
    (rb"[^a-c]+", 1, [b"ABCxyz"]),
    (rb"(?i:a)b", 0, [b"Ab", b"AB"]),
    (rb"(?-i:a)b", 1, [b"AB", b"aB"]),
    (rb"(?m)^\w+$", 0, [b"ab\ncd"]),
    (rb"(?s).+", 0, [b"a\nb"]),
    (rb"(?ims)^a.b$", 0, [b"x\nA\nB\n"]),
    (rb"a(?#comment)b", 0, [b"ab"]),
    (rb"(?x)", 0, []),
    (rb"\s*(\.(?:AND|OR|EQV|NEQV)\.)\s*", 1, [b"a .and. b .OR.c"]),
    (rb"(?<=[\w\)\]])\s*(\+|-)\s*", 1, [b"a+b - c", b"1.e-5", b"f(x)-1"]),
    (rb"^\s*$", 0, [b"", b"   ", b"  \n", b" x "]),
    (rb"x*", 0, [b"abxd"]),
    (rb"\W*", 0, [b"a b!c"]),
    (rb"", 0, [b"", b"abc"]),
    (rb"\b", 0, [b"a b"]),
    (rb"(?=a)|b", 0, [b"ab"]),
]

SUB_CASES = [
    (rb"(\w+)@(\w+)", 0, b"a@b c@d", rb"\2 at \1"),
    (rb"(?P<n>\d+)", 0, b"a1b22", rb"<\g<n>>"),
    (rb"\d", 0, b"a1b2", rb"\g<0>\g<0>"),
    (rb"x*", 0, b"abxd", rb"-"),
    (rb"a", 0, b"aaa", rb"\n\t\\"),
    (rb"(a)|b", 0, b"ab", rb"[\1]"),
    (rb"a", 0, b"aa", rb"\101\0"),
    (rb"a", 0, b"a", rb"\&"),
    (rb"", 0, b"abc", rb"-"),
    (rb"\b", 0, b"ab cd", rb"|"),
]

SPLIT_CASES = [
    (rb",", 0, b"a,b,,c,"),
    (rb"(,)", 0, b"a,b"),
    (rb"(,)|(;)", 0, b"a,b;c"),
    (rb"x*", 0, b"axbc"),
    (rb"", 0, b"abc"),
    (rb"\s+", 0, b"  a b  "),
    (rb"\b", 0, b"ab cd"),
    (rb"^", 2, b"a\nb\nc"),
]

ERRORS = [
    rb"*", rb"+a", rb"?", rb"a**", rb"a*+*", rb"a{2}{3}", rb"a??*", rb"(", rb")", rb"a)",
    rb"(a", rb"(?:a", rb"[a", rb"[]", rb"[^]", rb"[z-a]", rb"[\d-z]", rb"[a-\d]", rb"a{3,2}",
    rb"\q", rb"\\\x4", rb"\x", rb"\8", rb"[\8]", rb"\1", rb"(a)\2", rb"(a\1)", rb"(?P<1a>x)",
    rb"(?P<a>x)(?P<a>y)", rb"(?P=foo)", rb"(?P<a>x)(?P=b)", rb"(?P<>x)", rb"(?P<a", rb"(?Px)",
    rb"(?z)", rb"(?i", rb"(?-i)", rb"(?#abc", rb"^*", rb"$+", rb"\b*", rb"\A?", rb"a|*",
    rb"(?<=", rb"(?=a", rb"\400", rb"[\400]", b"\\", b"a\\", rb"(*)", rb"(|*)", rb"{1}",
]

TEMPLATE_ERRORS = [rb"\10", rb"\2", rb"\g<2>", rb"\g<x>", rb"\g<", rb"\g<1", rb"\q", "\\".encode()]


def gen_features(w):
    w.comment("hand-written feature cases")
    for pattern, flags, subjects in FEATURES:
        for subject in subjects:
            for op in ("search", "match", "fullmatch", "findall"):
                assert w.case(op, flags, pattern, subject), pattern
    for pattern, flags, subject, repl in SUB_CASES:
        assert w.case("sub", flags, pattern, subject, repl), pattern
    for pattern, flags, subject in SPLIT_CASES:
        assert w.case("split", flags, pattern, subject), pattern


def gen_errors(w):
    w.comment("patterns that Python rejects")
    for pattern in ERRORS:
        w.case("compile", 0, pattern)
    w.comment("invalid replacement templates: expected result is 'error'")
    for repl in TEMPLATE_ERRORS:
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            try:
                re.sub(rb"(a)", repl, b"a")
                raise ValueError(f"template {repl!r} is valid in Python")
            except (re.error, IndexError):
                pass
        w._add("sub", 0, rb"(a)", b"a", repl, "error")


def gen_fprettify(w, fprettify_dir):
    """Every regex in fprettify, applied to lines of its example Fortran files."""
    # note: importing fprettify replaces sys.stdin and sys.stdout with UTF-8 wrappers
    sys.path.insert(0, str(fprettify_dir))
    mod = importlib.import_module("fprettify")
    utils = importlib.import_module("fprettify.fparse_utils")

    patterns = {}
    for m in (mod, utils):
        for name, value in sorted(vars(m).items()):
            rx = value if isinstance(value, re.Pattern) else getattr(value, "_re", None)
            if isinstance(rx, re.Pattern) and isinstance(rx.pattern, str):
                patterns[name] = rx
    lines = []
    for path in sorted((fprettify_dir / "examples" / "in").glob("*.f90")):
        lines += path.read_bytes().splitlines(keepends=True)
    lines = sorted(set(lines))

    w.comment(f"{len(patterns)} regexes from fprettify ({fprettify_dir.name}) on its example inputs")
    for name, rx in sorted(patterns.items()):
        flags = sum(bit for bit, flag in FLAG_MAP.items() if rx.flags & flag)
        pattern = rx.pattern.encode("ascii")
        brx = re.compile(pattern, py_flags(flags))
        hits = [line for line in lines if brx.search(line)]
        misses = [line for line in lines if not brx.search(line)]
        rng = random.Random(name)
        chosen = rng.sample(hits, min(12, len(hits))) + rng.sample(misses, min(6, len(misses)))
        w.comment(name)
        for line in chosen:
            w.case("search", flags, pattern, line)
        for line in hits[:3]:
            w.case("split", flags, pattern, line)
            w.case("sub", flags, pattern, line, rb"<\g<0>>")


# --------------------------------------------------------------------------------------------
# random differential testing
# --------------------------------------------------------------------------------------------

ATOMS = ["a", "b", "c", "A", " ", ".", "[ab]", "[^a]", "[a-c]", r"\w", r"\W", r"\s", r"\d", "1", r"\n"]
ASSERTS = ["^", "$", r"\b", r"\B", r"\A", r"\Z"]
QUANTS = ["*", "+", "?", "*?", "+?", "??", "{2}", "{1,2}", "{0,2}", "{,2}", "{1,}", "{2,3}?", "*+", "++", "?+"]
FIXED = ["a", "b", "ab", "[ab]", ".", r"\w", "a|b", "(a)", "ba|ab"]


class PatternGen:
    def __init__(self, rng):
        self.rng = rng

    def pattern(self):
        self.groups = 0
        self.closed = []
        return self.alt(0)

    def alt(self, depth):
        n = 1 if self.rng.random() < 0.75 else self.rng.randint(2, 3)
        return "|".join(self.concat(depth) for _ in range(n))

    def concat(self, depth):
        return "".join(self.piece(depth) for _ in range(self.rng.randint(0 if depth else 1, 3)))

    def piece(self, depth):
        r = self.rng.random()
        if r < 0.1:
            return self.rng.choice(ASSERTS)
        atom = self.atom(depth)
        if self.rng.random() < 0.4:
            atom += self.rng.choice(QUANTS)
        return atom

    def atom(self, depth):
        r = self.rng.random()
        if depth >= 3 or r < 0.5:
            return self.rng.choice(ATOMS)
        if r < 0.65:
            self.groups += 1
            g = self.groups
            body = self.alt(depth + 1)
            self.closed.append(g)
            return f"({body})"
        if r < 0.72:
            return f"(?:{self.alt(depth + 1)})"
        if r < 0.78:
            return f"(?{self.rng.choice('=!')}{self.alt(depth + 1)})"
        if r < 0.84:
            return f"(?<{self.rng.choice('=!')}{self.rng.choice(FIXED)})"
        if r < 0.89:
            return f"(?>{self.alt(depth + 1)})"
        if r < 0.95 and self.closed:
            return f"\\{self.rng.choice(self.closed)}"
        return f"(?{self.rng.choice(['i', 's', 'm'])}:{self.alt(depth + 1)})"


def gen_fuzz(w, count, seed):
    rng = random.Random(seed)
    gen = PatternGen(rng)
    w.comment(f"{count} random patterns (seed {seed})")
    done = 0
    while done < count:
        pattern = gen.pattern().encode()
        flags = rng.choice([0, 0, 0, 1, 2, 4, 3, 7])
        try:
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                re.compile(pattern, py_flags(flags))
        except re.error:
            continue
        done += 1
        for _ in range(3):
            subject = "".join(rng.choice("aabbcA 1\n") for _ in range(rng.randint(0, 8))).encode()
            w.case("search", flags, pattern, subject)
            w.case(rng.choice(["match", "fullmatch"]), flags, pattern, subject)
            w.case("findall", flags, pattern, subject)
        subject = "".join(rng.choice("aabbcA 1\n") for _ in range(rng.randint(0, 8))).encode()
        w.case("split", flags, pattern, subject)
        w.case("sub", flags, pattern, subject, b"<\\g<0>>")


def main():
    here = Path(__file__).resolve().parent
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--fprettify", type=Path, default=here.parent.parent.parent / "fprettify",
                        help="fprettify source checkout whose regexes are used as test cases")
    parser.add_argument("--out", type=Path, default=here.parent / "test" / "data")
    parser.add_argument("--fuzz", type=int, default=1500, help="number of random patterns")
    parser.add_argument("--seed", type=int, default=20261023)
    args = parser.parse_args()
    args.out.mkdir(parents=True, exist_ok=True)

    header = f"generated by tools/gen_conformance.py with Python {sys.version.split()[0]}"

    for name, gen in [("features", gen_features), ("errors", gen_errors)]:
        w = Writer(args.out / f"{name}.txt")
        w.comment(header)
        gen(w)
        w.write()

    if (args.fprettify / "fprettify" / "__init__.py").exists():
        w = Writer(args.out / "fprettify.txt")
        w.comment(header)
        gen_fprettify(w, args.fprettify.resolve())
        w.write()
    else:
        print(f"skipping fprettify cases: no checkout at {args.fprettify}")

    w = Writer(args.out / "fuzz.txt")
    w.comment(header)
    gen_fuzz(w, args.fuzz, args.seed)
    w.write()


if __name__ == "__main__":
    main()
