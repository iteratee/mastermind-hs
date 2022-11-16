# Give your project a name. :)
workspace(name = "mastermind-hs")

# Load the repository rule to download an http archive.
load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive"
)

# Download rules_haskell and make it accessible as "@rules_haskell".
http_archive(
    name = "rules_haskell",
    strip_prefix = "rules_haskell-0.15",
    urls = ["https://github.com/tweag/rules_haskell/archive/v0.15.tar.gz"],
    sha256 = "aba3c16015a2363b16e2f867bdc5c792fa71c68cb97d8fe95fddc41e409d6ba8",
)

load(
    "@rules_haskell//haskell:repositories.bzl",
    "rules_haskell_dependencies",
)

# Setup all Bazel dependencies required by rules_haskell.
rules_haskell_dependencies()

load(
    "@rules_haskell//haskell:toolchain.bzl",
    "rules_haskell_toolchains",
)

load(
    "@rules_haskell//haskell:cabal.bzl",
    "stack_snapshot"
)

http_archive(
    name = "retrie-patched",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
load("@haskell-language-server//:packages.bzl", "packages")
haskell_cabal_library(
    name = "retrie",
    version = packages["retrie"].version,
    srcs = glob(["**"]),
    deps = packages["retrie"].deps,
    visibility = ["//visibility:public"],
)
    """,
    patch_args = ["-p1"],
    patches = ["//:retrie.patch"],
    sha256 = "ddb064dc20ffaa7860dfa69fb6f36753e19cc01139480aabc0e1f742324f6fd8",
    strip_prefix = "retrie-1.2.0.1",
    urls = ["https://hackage.haskell.org/package/retrie-1.2.0.1/retrie-1.2.0.1.tar.gz"],
)
http_archive(
    name = "hlint-patched",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
load("@haskell-language-server//:packages.bzl", "packages")
haskell_cabal_library(
    name = "hlint",
    version = packages["hlint"].version,
    srcs = glob(["**"]),
    deps = packages["hlint"].deps,
    flags = ["+ghc-lib"],
    visibility = ["//visibility:public"],
)
    """,
    sha256 = "9f91a135c72452d5e856b7f027ef79a0ac80327909cd364e739b2998d800732e",
    strip_prefix = "hlint-3.4.1",
    urls = ["https://hackage.haskell.org/package/hlint-3.4.1/hlint-3.4.1.tar.gz"],
)
http_archive(
    name = "ghc-lib-parser-ex-patched",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
load("@haskell-language-server//:packages.bzl", "packages")
haskell_cabal_library(
    name = "ghc-lib-parser-ex",
    version = packages["ghc-lib-parser-ex"].version,
    srcs = glob(["**"]),
    deps = packages["ghc-lib-parser-ex"].deps
        + ["@haskell-language-server//:ghc-lib-parser"],
    flags = ["-auto"],
    visibility = ["//visibility:public"],
)
    """,
    sha256 = "9b9c78d1a73d5206b39b9961a06d1d8b604ec752d421010eb8b35d7cfc9d1c8d",
    strip_prefix = "ghc-lib-parser-ex-9.2.0.4",
    urls = ["https://hackage.haskell.org/package/ghc-lib-parser-ex-9.2.0.4/ghc-lib-parser-ex-9.2.0.4.tar.gz"],
)
http_archive(
    name = "ghc-lib-parser-patched",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
load("@haskell-language-server//:packages.bzl", "packages")
haskell_cabal_library(
    name = "ghc-lib-parser",
    version = packages["ghc-lib-parser"].version,
    srcs = glob(["**"]),
    deps = packages["ghc-lib-parser"].deps,
    tools = packages["ghc-lib-parser"].tools,
    visibility = [
      "@ghc-lib-parser-ex-patched//:__pkg__",
      "@haskell-language-server//:__pkg__",
    ],
)
    """,
    sha256 = "ba14abbc1ca9c482d2f52de6367af34cd301e8a60b1f6674d243c3dced379566",
    strip_prefix = "ghc-lib-parser-9.2.4.20220729",
    urls = ["https://hackage.haskell.org/package/ghc-lib-parser-9.2.4.20220729/ghc-lib-parser-9.2.4.20220729.tar.gz"],
)
http_archive(
    name = "stylish-haskell-patched",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
load("@haskell-language-server//:packages.bzl", "packages")
haskell_cabal_library(
    name = "stylish-haskell",
    version = packages["stylish-haskell"].version,
    srcs = glob(["**"]),
    deps = packages["stylish-haskell"].deps
        + ["@haskell-language-server//:ghc-lib-parser"],
    tools = packages["stylish-haskell"].tools,
    visibility = [
      "@haskell-language-server//:__pkg__",
    ],
    flags = ["+ghc-lib"]
)
    """,
    sha256 = "f181edfe62821639d881de6780f6a7130b731519461af9c73687f8fba3764ecc",
    strip_prefix = "stylish-haskell-0.14.2.0",
    urls = ["https://hackage.haskell.org/package/stylish-haskell-0.14.2.0/stylish-haskell-0.14.2.0.tar.gz"],
)

stack_snapshot(
    name = "haskell-language-server",
    # The rules_haskell example project shows how to import libz.
    # https://github.com/tweag/rules_haskell/blob/123e3817156f9135dfa44dcb5a796c424df1f436/examples/WORKSPACE#L42-L63
    extra_deps = {"zlib": ["@zlib.dev//:zlib"]},
    haddock = False,
    local_snapshot = "//:haskell-language-server-stack-snapshot.yaml",
    packages = [
        "haskell-language-server",
    ],
    components = {
        "haskell-language-server": [
            "lib",
            "exe",
            "exe:haskell-language-server-wrapper",
        ],
        "attoparsec": [
            "lib",
            "lib:attoparsec-internal",
        ],
        "ghcide": [ "lib", "exe" ],
    },
    components_dependencies = {
        #"xml-conduit": """{"lib:xml-conduit": ["@stackage//:cabal-doctest"]}"""
        "attoparsec": """{"lib:attoparsec": ["lib:attoparsec-internal"]}""",
    },
    vendored_packages = {
        "ghc-lib-parser": "@ghc-lib-parser-patched//:ghc-lib-parser",
        "ghc-lib-parser-ex": "@ghc-lib-parser-ex-patched//:ghc-lib-parser-ex",
        "hlint": "@hlint-patched//:hlint",
        "retrie": "@retrie-patched//:retrie",
        "stylish-haskell": "@stylish-haskell-patched//:stylish-haskell",
    },
    stack_snapshot_json = "//:haskell-language-server_snapshot.json"
)
# Load nixpkgs_git_repository from rules_nixpkgs,
# which was already initialized by rules_haskell_dependencies above.
load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_cc_configure",
    "nixpkgs_git_repository",
    "nixpkgs_package",
    "nixpkgs_python_configure",
)

# Fetch a version of nixpkgs from GitHub.
# For more information see the documentation of rules_nixpkgs at
# https://github.com/tweag/rules_nixpkgs/blob/master/README.md
#nixpkgs_git_repository(
#    name = "nixpkgs",
#    revision = "46818e4153a19f34d063b30130244bec8225e434",
#    sha256 = "",
#)
local_repository(
    name = "local-nixpkgs",
    path = "/home/kyle/devel/nixpkgs",
)

nixpkgs_cc_configure(
    repositories = {
        "nixpkgs": "@local-nixpkgs//:default.nix",
    },
)

nixpkgs_python_configure(
    repositories = {
        "nixpkgs": "@local-nixpkgs//:default.nix",
    },
)

load(
    "@rules_haskell//haskell:nixpkgs.bzl",
    "haskell_register_ghc_nixpkgs",
)

# Fetch a GHC binary distribution from nixpkgs and register it as a toolchain.
# For more information:
# https://api.haskell.build/haskell/nixpkgs.html#haskell_register_ghc_nixpkgs
haskell_register_ghc_nixpkgs(
    attribute_path = "",
    nix_file_content = """with import <nixpkgs> {}; haskell.packages.ghc924.ghc""",
    version = "9.2.4",
    repositories = {
        "nixpkgs": "@local-nixpkgs//:default.nix",
    },
)

# For zlib.BUILD.bazel
nixpkgs_package(
    name = "nixpkgs_zlib",
    attribute_path = "zlib",
    repositories = {
        "nixpkgs": "@local-nixpkgs//:default.nix",
    },
)

nixpkgs_package(
    name = "zlib.dev",
    build_file = "//:zlib.BUILD.bazel",
    repositories = {
        "nixpkgs": "@local-nixpkgs//:default.nix",
    },
)

stack_snapshot(
    name = "stackage",
    # LTS snapshot published for ghc-8.10.7 (default version used by rules_haskell)
    snapshot = "nightly-2022-10-31",

    packages = [
        "async",
        "attoparsec",
        "bytestring",
        "hspec",
        "hspec-discover",
        "pcg-random",
        "random",
        "socket",
        "stm",
        "vector",
        "vector-algorithms",
    ],
    components = {
        "attoparsec": [
            "lib",
            "lib:attoparsec-internal",
        ],
        "hspec-discover": [
            "lib", "exe"
        ],
    },
    components_dependencies = {
        "attoparsec": """{"lib:attoparsec": ["lib:attoparsec-internal"]}""",
    },
    setup_deps = {
        "pcg-random": ["cabal-doctest"],
    },
    stack_snapshot_json = "//:stackage_snapshot.json",
)
