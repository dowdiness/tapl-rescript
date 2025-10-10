define i64 @main() {
entry:
  %x = add i64 5, 3
  %cond = icmp ne i64 %x, 0
  br i1 %cond, label %then1, label %else2
then1:
  %result1 = add i64 %x, 10
  ret i64 %result1
else2:
  %result2 = sub i64 %x, 5
  ret i64 %result2
}
