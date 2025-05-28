// swift-tools-version:5.3

import Foundation
import PackageDescription

var sources = ["src/parser.c"]
if FileManager.default.fileExists(atPath: "src/scanner.c") {
    sources.append("src/scanner.c")
}

let package = Package(
    name: "TreeSitterBrad",
    products: [
        .library(name: "TreeSitterBrad", targets: ["TreeSitterBrad"]),
    ],
    dependencies: [
        .package(url: "https://github.com/tree-sitter/swift-tree-sitter", from: "0.8.0"),
    ],
    targets: [
        .target(
            name: "TreeSitterBrad",
            dependencies: [],
            path: ".",
            sources: sources,
            resources: [
                .copy("queries")
            ],
            publicHeadersPath: "bindings/swift",
            cSettings: [.headerSearchPath("src")]
        ),
        .testTarget(
            name: "TreeSitterBradTests",
            dependencies: [
                "SwiftTreeSitter",
                "TreeSitterBrad",
            ],
            path: "bindings/swift/TreeSitterBradTests"
        )
    ],
    cLanguageStandard: .c11
)
