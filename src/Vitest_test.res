// Vitest.test.res - Vitestバインディングのテスト
open Vitest

describe("Vitest bindings", () => {
  test("basic expect assertions", () => {
    expect(2 + 2)->toBe(4)
    expect("hello")->toEqual("hello")
    expect(true)->toBeTruthy
    expect(false)->toBeFalsy
  })

  test("string operations", () => {
    let str = "ReScript"
    expect(String.length(str))->toBe(8)
    expect(str)->toContain("Script")
  })

  test("array operations", () => {
    let arr = [1, 2, 3, 4, 5]
    expect(arr)->toHaveLength(5)
    expect(arr)->toContainArray(3)
  })

  test("number comparisons", () => {
    expect(10.0)->toBeGreaterThan(5.0)
    expect(3.0)->toBeLessThan(5.0)
    expect(3.14159)->toBeCloseTo(3.14)
  })
})

describe("ReScript specific tests", () => {
  test("option type handling", () => {
    let someValue = Some(42)
    let noneValue = None

    expect(someValue)->toBeDefined
    expect(noneValue)->toBeUndefined
  })

  test("list operations", () => {
    let list = list{1, 2, 3}
    expect(List.length(list))->toBe(3)
    expect(List.head(list))->toEqual(Some(1))
  })

  test("pattern matching results", () => {
    let getValue = (opt) => {
      switch opt {
      | Some(x) => x
      | None => 0
      }
    }

    expect(getValue(Some(42)))->toBe(42)
    expect(getValue(None))->toBe(0)
  })
})
