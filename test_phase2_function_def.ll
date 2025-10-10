define i64 @addOne(i64 %x) {
entry:
  %r = add i64 %x, 1
  ret i64 %r
}

define i64 @main() {
entry:
  %result = call i64 @addOne(i64 5)
  ret i64 %result
}
