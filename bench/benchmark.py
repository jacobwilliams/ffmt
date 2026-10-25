"""Compare the speed of fformat, fprettify, fprettier and ffmt.

Each formatter formats a fresh copy of the same Fortran files in place, several times; the
median wall-clock time is reported. Every tool runs with its default settings, so the tools do
somewhat different amounts of work.

Run from the repository root, in the pixi `bench` environment (which provides fprettify and the
Rust toolchain used to install fprettier and ffmt):

    pixi run -e bench bench                          # default corpus, 5 runs per tool
    pixi run -e bench bench --corpus fortranpack     # fortranpack and all its dependencies
    pixi run -e bench bench --runs 10 DIR...         # your own files or directories
    pixi run -e bench bench --update-readme          # also rewrite the corpus's table in README.md
"""

import argparse
import os
import platform
import shutil
import statistics
import subprocess
import sys
import tempfile
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
BENCH_DIR = ROOT / "build" / "bench"
FPRETTIER_VERSION = "0.5.0"
FFMT_VERSION = "0.4.5"
EXTENSIONS = {".f90", ".F90"}
FORTRANPACK_URL = "https://github.com/jacobwilliams/fortranpack"
FORTRANPACK_REF = "fc527da560b056b7d71ce0fa00edeb75d35ef571"
EXE = ".exe" if os.name == "nt" else ""


def default_corpus():
    """This repository's sources and the Fortran dependencies that fpm fetches."""
    deps = ROOT / "build" / "dependencies"
    return [ROOT / "src", ROOT / "test", deps / "toml-f", deps / "fregex", deps / "test-drive"]


def fortranpack_corpus():
    """fortranpack at FORTRANPACK_REF, with the dependencies that `fpm update` fetches for it
    (their latest revisions, as fortranpack does not pin them), in build/bench/fortranpack."""
    dest = BENCH_DIR / "fortranpack"
    if not (dest / "fpm.toml").exists():
        print(f"cloning {FORTRANPACK_URL} ...", file=sys.stderr)
        BENCH_DIR.mkdir(parents=True, exist_ok=True)
        run(["git", "clone", "--quiet", FORTRANPACK_URL, str(dest)])
        run(["git", "checkout", "--quiet", FORTRANPACK_REF], cwd=dest)
    if not (dest / "build" / "dependencies").is_dir():
        print("fetching the dependencies of fortranpack with fpm ...", file=sys.stderr)
        run(["fpm", "update"], cwd=dest)
    return [dest]


CORPORA = {"default": default_corpus, "fortranpack": fortranpack_corpus}


def run(cmd, **kwargs):
    return subprocess.run(cmd, check=True, text=True, capture_output=True, **kwargs)


def build_fformat(flags):
    """Build fformat with fpm's release profile (for gfortran: -O3 -funroll-loops), plus `flags`,
    and return the path of the executable."""
    cmd = ["fpm", "run", "fformat", "--profile", "release", "--runner", "echo"]
    if flags:
        cmd += ["--flag", flags]
    out = run(cmd, cwd=ROOT)
    for line in reversed((out.stdout + out.stderr).splitlines()):
        exe = ROOT / line.strip()
        if line.strip() and exe.is_file():
            return exe.resolve()
    sys.exit("could not build fformat:\n" + out.stdout + out.stderr)


def install_crate(crate, version):
    """Install a formatter written in Rust with cargo into build/bench/cargo, if not already there,
    and return the path of its executable."""
    root = BENCH_DIR / "cargo"
    exe = root / "bin" / (crate + EXE)
    if not exe.exists():
        print(f"installing {crate} {version} with cargo ...", file=sys.stderr)
        run(["cargo", "install", crate, "--version", version, "--locked", "--root", str(root)])
    return exe


def find_fprettify():
    exe = shutil.which("fprettify")
    if exe is None:
        sys.exit("fprettify not found: run this in the pixi bench environment (pixi run -e bench bench)")
    return exe


def collect(paths):
    """The Fortran files under `paths`, as (source, relative destination) pairs."""
    files = []
    for k, path in enumerate(paths):
        path = Path(path).resolve()
        if path.is_file():
            files.append((path, Path(f"{k}_{path.parent.name}") / path.name))
            continue
        if not path.is_dir():
            sys.exit(f"{path}: no such file or directory")
        for f in sorted(path.rglob("*")):
            rel = f.relative_to(path)
            if f.suffix in EXTENSIONS and f.is_file() and not any(p.startswith(".") for p in rel.parts):
                # rename `build` directories, which fformat skips by default, so all tools see
                # the same files
                rel = Path(*["_build" if p == "build" else p for p in rel.parts])
                files.append((f, Path(f"{k}_{path.name}") / rel))
    if not files:
        sys.exit("no Fortran files found")
    return files


def count_lines(files):
    return sum(len(src.read_bytes().splitlines()) for src, _ in files)


def fresh_copy(files, dest):
    if dest.exists():
        shutil.rmtree(dest)
    for src, rel in files:
        target = dest / rel
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(src, target)


def time_tool(cmd, files, work, runs):
    """Median and minimum wall-clock seconds of `cmd` + [work] over `runs` fresh copies."""
    times = []
    status = 0
    for _ in range(runs):
        fresh_copy(files, work)
        start = time.perf_counter()
        result = subprocess.run(cmd + [str(work)], stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
        times.append(time.perf_counter() - start)
        status = max(status, result.returncode)
    return statistics.median(times), min(times), status


def failing_files(cmd, files, work):
    """The files that `cmd` fails on, one at a time. Some tools stop at the first file they cannot
    format, so these are left out of the corpus for every tool."""
    fresh_copy(files, work)
    if subprocess.run(cmd + [str(work)], capture_output=True).returncode == 0:
        return set()
    bad = set()
    for src, rel in files:
        fresh_copy([(src, rel)], work)
        if subprocess.run(cmd + [str(work)], capture_output=True).returncode != 0:
            bad.add(src)
    return bad


def version(cmd):
    try:
        out = subprocess.run(cmd, text=True, capture_output=True)
        return (out.stdout or out.stderr).strip().splitlines()[0]
    except (OSError, IndexError):
        return "?"


def cpu_name():
    try:
        if sys.platform == "darwin":
            return run(["sysctl", "-n", "machdep.cpu.brand_string"]).stdout.strip()
        if sys.platform.startswith("linux"):
            for line in Path("/proc/cpuinfo").read_text().splitlines():
                if line.startswith("model name"):
                    return line.split(":", 1)[1].strip()
    except (OSError, subprocess.CalledProcessError):
        pass
    return platform.processor() or platform.machine()


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("paths", nargs="*", help="Fortran files or directories, instead of a --corpus")
    parser.add_argument("--corpus", choices=sorted(CORPORA), default="default",
                        help="default: this repository and its dependencies; fortranpack: "
                             f"{FORTRANPACK_URL} and its dependencies")
    parser.add_argument("--runs", type=int, default=5, help="runs per tool (default 5)")
    parser.add_argument("--update-readme", action="store_true", help="replace the benchmark table in README.md")
    parser.add_argument("--fformat-flags", default="",
                        help="compiler flags added to the release profile (-march=native, -flto and -Ofast "
                             "made no measurable difference with gfortran 15)")
    args = parser.parse_args()
    sys.stdout.reconfigure(encoding="utf-8")   # the table has emoji; Windows consoles may not default to UTF-8

    fformat = build_fformat(args.fformat_flags)
    fprettify = find_fprettify()
    fprettier = install_crate("fprettier", FPRETTIER_VERSION)
    ffmt = install_crate("ffmt", FFMT_VERSION)

    # all single-threaded: fprettier formats files in parallel by default (ffmt and fformat do not);
    # ffmt's cache of formatted files would let a run skip work
    tools = [
        ("fformat", [str(fformat), "--no-config"], [str(fformat), "--version"]),
        ("ffmt", [str(ffmt), "--no-cache", "--no-ignore", "--quiet", "-j", "1"], [str(ffmt), "--version"]),
        ("fprettier", [str(fprettier), "-r", "-S", "-j", "1"], [str(fprettier), "--version"]),
        ("fprettify", [fprettify, "-r", "-S"], [fprettify, "--version"]),
    ]

    if args.paths and args.update_readme:
        sys.exit("--update-readme only works with a --corpus, not with paths")
    files = collect(args.paths or CORPORA[args.corpus]())

    results = []
    with tempfile.TemporaryDirectory(prefix="fformat-bench-") as tmp:
        work = Path(tmp) / "corpus"
        skipped = set()
        for name, cmd, _ in tools:
            bad = failing_files(cmd, files, work)
            for f in sorted(bad - skipped):
                print(f"{name} fails on {f.relative_to(ROOT) if f.is_relative_to(ROOT) else f}: left out",
                      file=sys.stderr)
            skipped |= bad
        files = [(src, rel) for src, rel in files if src not in skipped]
        nlines = count_lines(files)
        print(f"{len(files)} files, {nlines} lines, {args.runs} runs per tool", file=sys.stderr)
        for name, cmd, vcmd in tools:
            print(f"timing {name} ...", file=sys.stderr)
            median, best, status = time_tool(cmd, files, work, args.runs)
            results.append((name, version(vcmd), median, best, status))

    fastest = min(r[2] for r in results)
    results.sort(key=lambda r: r[2])      # fastest first

    def marker(median):
        """🟢 for the fastest tool, 🔴 for one 10 times slower or more, 🟡 for the others."""
        if median == fastest:
            return "🟢"
        return "🔴" if median >= 10 * fastest else "🟡"

    lines = [
        f"{len(files)} files, {nlines:,} lines; median of {args.runs} runs, formatting in place with "
        f"default settings. {platform.system()} {platform.machine()}, {cpu_name()}, {os.cpu_count()} cores."
        + (f" {len(skipped)} file(s) that a tool could not format were left out." if skipped else ""),
        "",
        "| Tool | Version | Time (s) | Lines/s | Relative time |",
        "|---|---|--:|--:|--:|",
    ]
    for name, ver, median, best, status in results:
        note = "" if status == 0 else f" (exit status {status})"
        lines.append(f"| {name}{note} | {ver.split()[-1]} | {median:.3f} | {nlines / median:,.0f} | "
                     f"{marker(median)} {median / fastest:.1f}x |")
    table = "\n".join(lines)
    print(table)

    if args.update_readme:
        readme = ROOT / "README.md"
        text = readme.read_text(encoding="utf-8")
        marker_start = f"<!-- benchmark:{args.corpus}:start -->"
        marker_end = f"<!-- benchmark:{args.corpus}:end -->"
        start, end = text.find(marker_start), text.find(marker_end)
        if start < 0 or end < start:
            sys.exit(f"README.md: no {marker_start} ... {marker_end} markers")
        text = text[:start + len(marker_start)] + "\n" + table + "\n" + text[end:]
        readme.write_text(text, encoding="utf-8")
        print("updated README.md", file=sys.stderr)


if __name__ == "__main__":
    main()
