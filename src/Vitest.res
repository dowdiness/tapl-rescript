// Vitest.res - Vitestの型安全なバインディング

// テスト関数の型定義
type testFn = unit => unit
type asyncTestFn = unit => promise<unit>

// 基本的なテスト関数
@val external describe: (string, unit => unit) => unit = "describe"
@val external test: (string, testFn) => unit = "test"
@val external testAsync: (string, asyncTestFn) => unit = "test"
@val external it: (string, testFn) => unit = "it"
@val external beforeEach: (unit => unit) => unit = "beforeEach"
@val external afterEach: (unit => unit) => unit = "afterEach"
@val external beforeAll: (unit => unit) => unit = "beforeAll"
@val external afterAll: (unit => unit) => unit = "afterAll"

// Expectモジュール
module Expect = {
  type t<'a>

  @val external expect: 'a => t<'a> = "expect"

  // 基本的なマッチャー
  @send external toBe: (t<'a>, 'a) => unit = "toBe"
  @send external toEqual: (t<'a>, 'a) => unit = "toEqual"
  @send external toStrictEqual: (t<'a>, 'a) => unit = "toStrictEqual"
  @send external toBeTruthy: t<'a> => unit = "toBeTruthy"
  @send external toBeFalsy: t<'a> => unit = "toBeFalsy"
  @send external toBeUndefined: t<'a> => unit = "toBeUndefined"
  @send external toBeNull: t<'a> => unit = "toBeNull"
  @send external toBeDefined: t<'a> => unit = "toBeDefined"

  // 数値マッチャー
  @send external toBeGreaterThan: (t<float>, float) => unit = "toBeGreaterThan"
  @send external toBeGreaterThanOrEqual: (t<float>, float) => unit = "toBeGreaterThanOrEqual"
  @send external toBeLessThan: (t<float>, float) => unit = "toBeLessThan"
  @send external toBeLessThanOrEqual: (t<float>, float) => unit = "toBeLessThanOrEqual"
  @send external toBeCloseTo: (t<float>, float) => unit = "toBeCloseTo"

  // 文字列マッチャー
  @send external toMatch: (t<string>, string) => unit = "toMatch"
  @send external toMatchRe: (t<string>, RegExp.t) => unit = "toMatch"
  @send external toContain: (t<string>, string) => unit = "toContain"

  // 配列マッチャー
  @send external toContainArray: (t<array<'a>>, 'a) => unit = "toContain"
  @send external toHaveLength: (t<array<'a>>, int) => unit = "toHaveLength"

  // エラーマッチャー
  @send external toThrow: t<unit => 'a> => unit = "toThrow"
  @send external toThrowError: (t<unit => 'a>, string) => unit = "toThrowError"

  // スナップショットマッチャー
  @send external toMatchSnapshot: t<'a> => unit = "toMatchSnapshot"
  @send external toMatchInlineSnapshot: (t<'a>, string) => unit = "toMatchInlineSnapshot"

  // 否定
  @get external not: t<'a> => t<'a> = "not"
}

// Expectモジュールの関数をトップレベルでエクスポート
include Expect

// モック関数
module Mock = {
  type mockFn<'args, 'return>

  @val external fn: unit => mockFn<'args, 'return> = "vi.fn"
  @val external fnWithImplementation: ('args => 'return) => mockFn<'args, 'return> = "vi.fn"

  @send external mockReturnValue: (mockFn<'args, 'return>, 'return) => mockFn<'args, 'return> = "mockReturnValue"
  @send external mockResolvedValue: (mockFn<'args, promise<'return>>, 'return) => mockFn<'args, promise<'return>> = "mockResolvedValue"
  @send external mockRejectedValue: (mockFn<'args, promise<'return>>, 'error) => mockFn<'args, promise<'return>> = "mockRejectedValue"

  @get external calls: mockFn<'args, 'return> => array<'args> = "mock.calls"
  @get external results: mockFn<'args, 'return> => array<'return> = "mock.results"
}

// Viユーティリティ
module Vi = {
  @val external clearAllMocks: unit => unit = "vi.clearAllMocks"
  @val external resetAllMocks: unit => unit = "vi.resetAllMocks"
  @val external restoreAllMocks: unit => unit = "vi.restoreAllMocks"
}
