#!/usr/bin/env fish

function repo_integrity_fail --argument-names message
    echo "repo-integrity: $message" >&2
    set -g repo_integrity_failures (math "$repo_integrity_failures + 1")
end

function repo_integrity_expected_namespace --argument-names tree file
    set -l relative_path (string replace -- "$tree/" '' "$file")
    set relative_path (string replace -r '\.(clj|cljc|cljs)$' '' -- "$relative_path")
    set relative_path (string replace -a '/' '.' -- "$relative_path")
    string replace -a '_' '-' -- "$relative_path"
end

function repo_integrity_test_namespace --argument-names namespace
    if string match -q '*-test' -- "$namespace"
        return 0
    end

    for component in (string split '.' -- "$namespace")
        if test "$component" = test
            return 0
        end
    end

    return 1
end

set -g repo_integrity_failures 0

set -l repo_root (command git rev-parse --show-toplevel 2>/dev/null)
or begin
    echo "repo-integrity: current directory is not inside a Git repository" >&2
    exit 2
end

cd "$repo_root"
or exit 2

if not type -q nsof
    echo "repo-integrity: required fish function/command 'nsof' is unavailable" >&2
    exit 2
end

for tree in src test
    if not test -d "$tree"
        repo_integrity_fail "missing repository tree: $tree/"
    end
end

if test "$repo_integrity_failures" -ne 0
    exit 1
end

set -l files (command find src test -type f \( -name '*.clj' -o -name '*.cljc' -o -name '*.cljs' \) -print | command sort)

if test (count $files) -eq 0
    repo_integrity_fail "no Clojure/ClojureScript namespace files found under src/ or test/"
end

set -l seen_namespaces
set -l seen_files
set -l checked 0

for file in $files
    set checked (math "$checked + 1")

    set -l actual_ns (nsof "$file" 2>/dev/null)
    if test $status -ne 0 -o (count $actual_ns) -ne 1
        repo_integrity_fail "cannot determine exactly one namespace for $file"
        continue
    end

    set actual_ns (string trim -- "$actual_ns")
    if test -z "$actual_ns"
        repo_integrity_fail "empty namespace reported for $file"
        continue
    end

    set -l tree (string split -m 1 '/' -- "$file")[1]
    set -l expected_ns (repo_integrity_expected_namespace "$tree" "$file")

    if test "$actual_ns" != "$expected_ns"
        repo_integrity_fail "namespace/path mismatch: $file declares $actual_ns; expected $expected_ns"
    end

    # src/ is production code and must not contain test namespaces. test/
    # may contain ordinary helper namespaces; exact path/namespace agreement
    # above is the invariant that applies there.
    if test "$tree" = src
        if repo_integrity_test_namespace "$actual_ns"
            repo_integrity_fail "test namespace is installed under src/: $file declares $actual_ns"
        end
    end

    set -l duplicate_index (contains --index -- "$actual_ns" $seen_namespaces)
    if test $status -eq 0
        repo_integrity_fail "duplicate namespace $actual_ns: $seen_files[$duplicate_index] and $file"
    else
        set -a seen_namespaces "$actual_ns"
        set -a seen_files "$file"
    end
end

if test "$repo_integrity_failures" -ne 0
    echo "repo-integrity: FAIL — $repo_integrity_failures problem(s) across $checked namespace file(s)" >&2
    exit 1
end

echo "repo-integrity: PASS — $checked namespace file(s)"
