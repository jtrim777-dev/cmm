scala_library(
    name = "cmm-core",
    srcs = glob(["src/main/scala/*.scala"]),
    deps = [
        "@third_party//3rdparty/jvm/org/typelevel:cats_effect",
    ],
    exports = [
        "@third_party//3rdparty/jvm/org/typelevel:cats_effect",
    ],
)

scala_library(
    name = "cmm-lang",
    srcs = glob(["src/main/scala/lang/*.scala"]),
    deps = [],
    exports = [],
)

scala_library(
    name = "cmm-isa",
    srcs = glob(["src/main/scala/isa/*.scala"]),
    deps = [":cmm-lang"],
    exports = [":cmm-lang"],
)

scala_library(
    name = "cmm-compile-lib",
    srcs = glob(["src/main/scala/compile/*.scala", "src/main/scala/compile/common/*.scala"]),
    deps = [
        ":cmm-lang",
        ":cmm-isa",
        ":cmm-core",
        "@third_party//3rdparty/jvm/org/typelevel:cats_effect",
    ],
    exports = [
        ":cmm-lang",
        ":cmm-isa",
        ":cmm-core",
        "@third_party//3rdparty/jvm/org/typelevel:cats_effect",
    ],
)

scala_library(
    name = "cmm-isa-x64",
    srcs = glob(["src/main/scala/isa/x86_64/*.scala"]),
    deps = [
        ":cmm-isa",
    ],
    exports = [
        ":cmm-isa",
    ],
)

scala_library(
    name = "cmm-compile-x64",
    srcs = glob(["src/main/scala/compile/x86_64/*.scala"]),
    deps = [
        ":cmm-compile-lib",
        ":cmm-isa-x64",
    ],
    exports = [
        ":cmm-compile-lib",
        ":cmm-isa-x64",
    ],
)

scala_library(
    name = "cmm-parse",
    srcs = glob(["src/main/scala/parse/*.scala"]),
    deps = [
        ":cmm-core",
        ":cmm-lang",
        "@third_party//3rdparty/jvm/org/parboiled:parboiled",
    ],
    exports = [
        ":cmm-core",
        ":cmm-lang",
        "@third_party//3rdparty/jvm/org/parboiled:parboiled",
    ],
)

scala_library(
  name = "cmm-lib",
  deps = [
      ":cmm-core",
      ":cmm-lang",
      ":cmm-isa",
      ":cmm-compile-lib",
  ],
  exports = [
      ":cmm-core",
      ":cmm-lang",
      ":cmm-isa",
      ":cmm-compile-lib",
  ],
)

scala_library(
    name = "cmm-x64",
    deps = [
        ":cmm-isa-x64",
        ":cmm-compile-x64",
    ],
    exports = [
        ":cmm-isa-x64",
        ":cmm-compile-x64",
    ],
)

scala_library(
    name = "test-sources",
    srcs = glob(["src/test/scala/**/*.scala"]),
    resources = glob(["src/test/resources/*"]),
    deps = [
        ":cmm-lib",
        ":cmm-parse",
        ":cmm-x64",
        "@third_party//3rdparty/jvm/com/lihaoyi:pprint",
        "@third_party//3rdparty/jvm/org/typelevel:cats_effect",
        "@third_party//3rdparty/jvm/org/parboiled:parboiled",
    ],
    exports = [
        ":cmm-lib",
        ":cmm-parse",
        ":cmm-x64",
        "@third_party//3rdparty/jvm/com/lihaoyi:pprint",
        "@third_party//3rdparty/jvm/org/typelevel:cats_effect",
        "@third_party//3rdparty/jvm/org/parboiled:parboiled",
    ]
)

scala_binary(
    name = "test-parser",
    main_class = "dev.jtrim777.cmm.TestParser",
    deps = [":test-sources"]
)

scala_binary(
    name = "test-validate",
    main_class = "dev.jtrim777.cmm.TestValidate",
    deps = [":test-sources"]
)

scala_binary(
    name = "test-prepare",
    main_class = "dev.jtrim777.cmm.TestPrepare",
    deps = [":test-sources"]
)