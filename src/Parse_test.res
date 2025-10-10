// ParseModule.test.res - ReScriptでVitestバインディングを使用したテスト

// 直接Vitestの関数をインポート（バインディングを使用せずに）
@val external describe: (string, unit => unit) => unit = "describe"
@val external test: (string, unit => unit) => unit = "test"

// Expectのバインディング
module Expect = {
  type t<'a>
  @val external expect: 'a => t<'a> = "expect"
  @send external toBe: (t<'a>, 'a) => unit = "toBe"
  @send external toEqual: (t<'a>, 'a) => unit = "toEqual"
  @send external toBeTruthy: (t<'a>) => unit = "toBeTruthy"
  @send external toBeFalsy: (t<'a>) => unit = "toBeFalsy"
}

open Expect

describe("Parse Module Tests", () => {
  describe("Stream operations", () => {
    test("should create and use stream", () => {
      let stream = Parse.Stream.make("hello")
      expect(stream.source)->toBe("hello")
      expect(stream.location.contents)->toBe(0)
    })

    test("should advance stream position", () => {
      let stream = Parse.Stream.make("abc")
      let char1 = stream->Parse.Stream.next
      let char2 = stream->Parse.Stream.next

      expect(char1)->toEqual(Some("a"))
      expect(char2)->toEqual(Some("b"))
      expect(stream.location.contents)->toBe(2)
    })

    test("should identify whitespace", () => {
      expect(Parse.Stream.isWhitespace(" "))->toBeTruthy
      expect(Parse.Stream.isWhitespace("\t"))->toBeTruthy
      expect(Parse.Stream.isWhitespace("a"))->toBeFalsy
    })
  })

  describe("Token operations", () => {
    test("should convert tokens to strings", () => {
      expect(Parse.stringOfToken(LeftParen))->toBe("(")
      expect(Parse.stringOfToken(RightParen))->toBe(")")
      expect(Parse.stringOfToken(Lambda))->toBe("λ")
      expect(Parse.stringOfToken(End))->toBe("End")
      expect(Parse.stringOfToken(Identifier("x")))->toBe("x")
    })
  })

  describe("Character utilities", () => {
    test("should identify alphabetic characters", () => {
      expect(Parse.isAlphabet("a"))->toBeTruthy
      expect(Parse.isAlphabet("Z"))->toBeTruthy
      expect(Parse.isAlphabet("1"))->toBeFalsy
      expect(Parse.isAlphabet("("))->toBeFalsy
    })
  })

  describe("Lexer", () => {
    test("should tokenize simple expressions", () => {
      open Parse
      let tokens = Parse.lexer("(λa)")
      let expected = list{LeftParen, Lambda, Identifier("a"), RightParen}
      expect(tokens)->toEqual(expected)
    })

    test("should handle empty input", () => {
      let tokens = Parse.lexer("")
      expect(tokens)->toEqual(list{})
    })
  })
})
