workspace(name = "mdspa")

# External rules
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "io_tweag_rules_nixpkgs",
    sha256 = "52c5ab0b68841b96463e1bd44760ef2bbbffa0804873496b6e0f982f5b3176f6",
    strip_prefix = "rules_nixpkgs-acb9e36f403ec6f38bac81290781cb896f22a85e",  # 2020-11-30
    urls = [
        "https://github.com/tweag/rules_nixpkgs/archive/acb9e36f403ec6f38bac81290781cb896f22a85e.tar.gz",
    ],
)

http_archive(
    name = "rules_haskell",
    sha256 = "8f36f40cce42e6226127c3bf3d31438ad72f15b7d2746774a25a3711f69e25d5",
    strip_prefix = "rules_haskell-bf28f164772c2e70f4f64509006eb4f488f56681",  # 2021-01-20
    urls = [
        "https://github.com/tweag/rules_haskell/archive/bf28f164772c2e70f4f64509006eb4f488f56681.tar.gz",
    ],
)

http_archive(
    name = "io_bazel_rules_rust",
    sha256 = "4ce98d8788014bc91f957ae345ff107eae8e9dd32013e4a822b9d67454ad1547",
    strip_prefix = "rules_rust-bf28c9f7f7ddf022a78216dcfdf9ed318c1d1cab",  # 2021-01-30
    urls = [
        "https://github.com/bazelbuild/rules_rust/archive/bf28c9f7f7ddf022a78216dcfdf9ed318c1d1cab.tar.gz",
    ],
)

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")

rules_haskell_dependencies()

# Setup Nix
load("@io_tweag_rules_nixpkgs//nixpkgs:repositories.bzl", "rules_nixpkgs_dependencies")

rules_nixpkgs_dependencies()

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_cc_configure",
    "nixpkgs_git_repository",
    "nixpkgs_package",
    "nixpkgs_python_configure",
)

nixpkgs_git_repository(
    name = "nixpkgs_default",
    revision = "6e7f25001fe6874f7ae271891f709bbf50a22c45",  # NixOS/nixpkgs@nixos-20.09, 2021-01-29
)

nixpkgs_cc_configure(
    nix_file = "@rules_haskell//nixpkgs:cc-toolchain.nix",
    nix_file_deps = ["@rules_haskell//nixpkgs:default.nix"],
    repository = "@rules_haskell//nixpkgs:default.nix",
)

nixpkgs_python_configure(
    repository = "@rules_haskell//nixpkgs:default.nix",
)

# Use GHC binaries from nixpkgs (NixOS)
load("@rules_haskell//haskell:nixpkgs.bzl", "haskell_register_ghc_nixpkgs")

haskell_register_ghc_nixpkgs(
    attribute_path = "haskell.compiler.ghc8102",
    repositories = {"nixpkgs": "@nixpkgs_default"},
    version = "8.10.2",
)

## ghcide
nixpkgs_package(
    name = "ghcide",
    attribute_path = "haskellPackages.ghcide",
    repository = "@nixpkgs_default",
)

## Project configuration
load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

stack_snapshot(
    name = "stackage",
    packages = [
        # Core libraries
        "array",
        "base",
        "binary",
        "bytestring",
        "containers",
        "deepseq",
        "directory",
        "filepath",
        "mtl",
        "template-haskell",
        "transformers",
        "ghc-prim",
        "process",
        "rts",
        "text",
        # External libraries
        "pretty-simple",  # -3.3.0.0
    ],
    snapshot = "nightly-2020-12-14",
)

# Setup Rust compiler through Nix
http_archive(
    name = "nixpkgs-mozilla",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

filegroup(
    name = "out",
    srcs = ["**"],
)
    """,
    sha256 = "1f69a63766156c0396390212b7c5ecd2941b424ae68e1f25a2c30c8416af7d03",
    strip_prefix = "nixpkgs-mozilla-8c007b60731c07dd7a052cce508de3bb1ae849b4",  # 2020-10-28
    urls = [
        "https://github.com/mozilla/nixpkgs-mozilla/archive/8c007b60731c07dd7a052cce508de3bb1ae849b4.tar.gz",
    ],
)

nixpkgs_package(
    name = "rust_default",
    attribute_path = "rust",
    build_file_content = """
# Adopted from https://github.com/tweag/rules_nixpkgs/blob/ad6db1389261a2bbfca69b2edc206a48cd558919/nixpkgs/BUILD.pkg
package(default_visibility = ["//visibility:public"])

filegroup(
    name = "bin",
    srcs = glob(["bin/*"], allow_empty = True),
)

# Adopted from https://github.com/bazelbuild/rules_rust/blob/67f0c5ec0397d24ccc14264a0eda86915ddf63e8/rust/repositories.bzl#L273
filegroup(
    name = "rust_lib",
    srcs = glob([
        "lib/rustlib/*/lib/*",
    ], allow_empty = True)
)

# Adopted from https://github.com/bazelbuild/rules_rust/blob/67f0c5ec0397d24ccc14264a0eda86915ddf63e8/rust/repositories.bzl#L182
filegroup(
    name = "rustc_lib",
    srcs = glob([
        "lib/*",
        "lib/rustlib/*/codegen-backends/*",
        "lib/rustlib/*/bin/rust-lld*",
        "lib/rustlib/*/lib/*",
    ], allow_empty = True)
)
    """,
    nix_file_content = """
let
  nixpkgs = import <nixpkgs> { overlays = [ (import <rust-overlay>) ]; };
in
(nixpkgs.rustChannelOf { channel = "1.49.0"; sha256 = "0swxyj65fkc5g9kpsc7vzdwk5msjf6csj3l5zx4m0xmd2587ca18"; })
    """,
    repositories = {
        "nixpkgs": "@nixpkgs_default",
        "rust-overlay": "@nixpkgs-mozilla//:rust-overlay.nix",
    },
)

register_toolchains(
    "//:rust_toolchain",
)
