import XCTest
import SwiftTreeSitter
import TreeSitterBrad

final class TreeSitterBradTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_brad())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Brad grammar")
    }
}
