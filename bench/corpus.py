"""Check fformat on other Fortran libraries.

Each library is fetched at a fixed revision into build/corpus/NAME. The corpus checker
(test/check_corpus.f90) then reads every Fortran file of it: the tokens must reproduce the file,
the block structure must balance, and formatting in four styles must succeed without an internal
error and be stable. With --build, the libraries that build with fpm are also formatted in three
styles and built and tested, to check that the formatted code still compiles and passes the
library's own tests (compared with the unformatted code).

Run from the repository root:

    pixi run corpus                      # every library, corpus check only
    pixi run corpus --build fpm          # also format, build and test fpm
    pixi run corpus stdlib cp2k          # some libraries
"""

import argparse
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
from benchmark import ROOT, build_fformat  # noqa: E402

CORPUS_DIR = ROOT / "build" / "corpus"

# name: (repository, revision, file extensions, builds and tests with fpm)
LIBRARIES = {
    "stdlib": ("https://github.com/fortran-lang/stdlib", "f1b49c3ae98a6e475125cc90c7d6d04d3ef9fec8",
               [".f90", ".F90", ".fypp"], False),
    "fpm": ("https://github.com/fortran-lang/fpm", "68971cf22f2fba2bf59f87389d47aedfda28b5eb",
            [".f90", ".F90"], True),
    "json-fortran": ("https://github.com/jacobwilliams/json-fortran", "d9ed2e4ee53745f4ed98d7536e17bee5198aa3b0",
                     [".f90", ".F90"], True),
    # cp2k writes free-form code in .F files
    "cp2k": ("https://github.com/cp2k/cp2k", "94650f7de5580db70b5132d7db0a555e5b017794",
             [".F", ".f90", ".F90"], False),
    "wannier90": ("https://github.com/wannier-developers/wannier90", "546d79f5ac205fc187e49c149a5617f666b8207a",
                  [".f90", ".F90"], False),
}

# styles for the build check, as command-line options (the defaults first)
STYLES = [
    [],
    ["-i", "3", "--continuation", "indent", "-l", "80", "--relational", "symbols", "--case", "all=upper",
     "--spaces", "all=add", "--comment-spacing", "1", "--sort-use", "--sort-use-only"],
    # blanks stay around `::`: fpm's source scanner (0.13) reads `module procedure::name` as a
    # `module` statement and stops with "empty or invalid name for module"
    ["--base-indent", "4", "--no-indent-procedures", "-l", "100", "--relational", "letters",
     "--case", "all=lower", "--spaces", "all=remove,declarations=add"],
]


def run(cmd, cwd=None, timeout=3600):
    return subprocess.run(cmd, cwd=cwd, text=True, capture_output=True, timeout=timeout)


def fetch(name):
    """The library's directory, fetched at its revision if it is not there yet."""
    url, rev, _, _ = LIBRARIES[name]
    dest = CORPUS_DIR / name
    if not (dest / ".git").is_dir():
        print(f"fetching {name} ...", file=sys.stderr, flush=True)
        dest.mkdir(parents=True, exist_ok=True)
        for cmd in (["git", "init", "--quiet"], ["git", "remote", "add", "origin", url],
                    ["git", "fetch", "--quiet", "--depth", "1", "origin", rev],
                    ["git", "checkout", "--quiet", "FETCH_HEAD"]):
            r = run(cmd, cwd=dest)
            if r.returncode != 0:
                sys.exit(f"{name}: {' '.join(cmd)} failed:\n{r.stderr}")
    return dest


def fortran_files(name, root):
    exts = set(LIBRARIES[name][2])
    files = []
    for f in sorted(root.rglob("*")):
        rel = f.relative_to(root)
        if f.suffix in exts and f.is_file() and not any(p.startswith(".") or p == "build" for p in rel.parts):
            files.append(f)
    return files


def corpus_check(name, root):
    """Run the corpus checker over the library's files; True if it found no problem."""
    files = fortran_files(name, root)
    listfile = CORPUS_DIR / f"{name}.files"
    listfile.write_text("\n".join(str(f) for f in files) + "\n")
    r = run(["fpm", "test", "check_corpus", "--profile", "release", "--", "--files-from", str(listfile)],
            cwd=ROOT)
    log = CORPUS_DIR / f"{name}.log"
    log.write_text(r.stdout + r.stderr)
    lines = r.stdout.splitlines()
    summary = lines[lines.index("-" * 60) + 1:] if "-" * 60 in lines else lines[-12:]
    # the checker's own lines name a file; fpm's build progress (`[ 50%] x.f90`) is left out
    problems = [l for l in lines[:len(lines) - len(summary) - 1]
                if l.strip() and not l.startswith("[") and not l.startswith("Project")]
    print(f"\n== {name}: {len(files)} files (full log: {log.relative_to(ROOT)})")
    for l in summary:
        print("  " + l)
    for l in problems[:15]:
        print("  | " + l)
    if len(problems) > 15:
        print(f"  | ... {len(problems) - 15} more")
    return r.returncode == 0


def fpm_build_and_test(root):
    """(build ok, test ok, tail of the output) for an fpm project at `root`."""
    b = run(["fpm", "build"], cwd=root)
    if b.returncode != 0:
        return False, False, (b.stdout + b.stderr)[-1500:]
    t = run(["fpm", "test"], cwd=root)
    return True, t.returncode == 0, (t.stdout + t.stderr)[-1500:]


def build_check(name, root, fformat):
    """Format copies of the library in each style and build and test them with fpm."""
    ok = True
    with tempfile.TemporaryDirectory(prefix=f"fformat-{name}-") as tmp:
        work = Path(tmp)
        copy = work / "original"
        shutil.copytree(root, copy, ignore=shutil.ignore_patterns(".git", "build"))
        base = fpm_build_and_test(copy)
        print(f"  unformatted: build {'ok' if base[0] else 'FAILED'}, tests {'ok' if base[1] else 'FAILED'}")
        for k, style in enumerate(STYLES):
            copy = work / f"style{k}"
            shutil.copytree(root, copy, ignore=shutil.ignore_patterns(".git", "build"))
            f = run([str(fformat), "--no-config", "--quiet", *style, str(copy)])
            result = fpm_build_and_test(copy)
            same = result[:2] == base[:2]
            ok = ok and same and f.returncode == 0
            label = " ".join(style) or "(defaults)"
            print(f"  style {k} [{label}]: format exit {f.returncode}, build {'ok' if result[0] else 'FAILED'}, "
                  f"tests {'ok' if result[1] else 'FAILED'}{'' if same else '  <-- differs from unformatted'}")
            if not same:
                print("    " + result[2].replace("\n", "\n    "))
            if f.returncode != 0:
                print("    " + f.stderr.strip().replace("\n", "\n    "))
    return ok


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("names", nargs="*", help=f"libraries (default: all of {', '.join(LIBRARIES)})")
    parser.add_argument("--build", action="store_true",
                        help="also format, build and test the libraries that build with fpm")
    args = parser.parse_args()
    names = args.names or list(LIBRARIES)
    for n in names:
        if n not in LIBRARIES:
            sys.exit(f"unknown library {n}; choose from {', '.join(LIBRARIES)}")
    CORPUS_DIR.mkdir(parents=True, exist_ok=True)
    fformat = build_fformat("") if args.build else None
    ok = True
    for name in names:
        root = fetch(name)
        ok = corpus_check(name, root) and ok
        if args.build and LIBRARIES[name][3]:
            ok = build_check(name, root, fformat) and ok
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
