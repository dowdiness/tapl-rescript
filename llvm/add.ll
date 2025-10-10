; ModuleID = 'add_module'
source_filename = "add_module"

; Function definition:
; - 'define' declares a function.
; - 'i64' specifies the return type (a 64-bit integer).
; - '@add' is the function name.
; - '(i64 %a, i64 %b)' are the parameters: two 64-bit integers named 'a' and 'b'.
define i64 @add(i64 %a, i64 %b) {
entry:
  ; The '%1' is a "virtual register". It holds the result of the add operation.
  ; 'nsw' stands for "No Signed Wrap," an optimization hint for the compiler.
  %1 = add nsw i64 %a, %b

  ; Return the value stored in the virtual register %1.
  ret i64 %1
}
