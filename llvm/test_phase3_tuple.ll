define i64 @main() {
entry:
  %t_ptr = alloca { i64, i64, i64 }
  %t_gep0 = getelementptr { i64, i64, i64 }, { i64, i64, i64 }* %t_ptr, i32 0, i32 0
  store i64 1, i64* %t_gep0
  %t_gep1 = getelementptr { i64, i64, i64 }, { i64, i64, i64 }* %t_ptr, i32 0, i32 1
  store i64 2, i64* %t_gep1
  %t_gep2 = getelementptr { i64, i64, i64 }, { i64, i64, i64 }* %t_ptr, i32 0, i32 2
  store i64 3, i64* %t_gep2
  %t = ptrtoint { i64, i64, i64 }* %t_ptr to i64
  %t_ptr_x = inttoptr i64 %t to { i64, i64, i64 }*
  %x_gep = getelementptr { i64, i64, i64 }, { i64, i64, i64 }* %t_ptr_x, i32 0, i32 0
  %x = load i64, i64* %x_gep
  %t_ptr_y = inttoptr i64 %t to { i64, i64, i64 }*
  %y_gep = getelementptr { i64, i64, i64 }, { i64, i64, i64 }* %t_ptr_y, i32 0, i32 2
  %y = load i64, i64* %y_gep
  %sum = add i64 %x, %y
  ret i64 %sum
}
