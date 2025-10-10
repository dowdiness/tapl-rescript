define i64 @double(i64 %x) {
entry:
  %r = add i64 %x, %x
  ret i64 %r
}

define i64 @main() {
entry:
  %result = call i64 @double(i64 21)
  ret i64 %result
}
