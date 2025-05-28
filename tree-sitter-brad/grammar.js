/**
 * @file Brad grammar for tree-sitter
 * @author BRAD Team
 * @license AGPL
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "brad",

  rules: {
    sourc_file: $ => "hello",
  }
});
