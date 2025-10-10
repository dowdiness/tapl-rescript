
declare i64* @malloc(i64)

%fty = type i64 (i64, i64)*

define i64 @main() {
  %ptr61 = call i64* @malloc(i64 8)
  %f8 = ptrtoint i64* %ptr61 to i64
  %off62 = getelementptr i64, i64* %ptr61, i32 0
  %casted63 = ptrtoint i64 (i64, i64)* @f8 to i64
  store i64 %casted63, i64* %off62
  %ptr64 = call i64* @malloc(i64 8)
  %f20 = ptrtoint i64* %ptr64 to i64
  %off65 = getelementptr i64, i64* %ptr64, i32 0
  %casted66 = ptrtoint i64 (i64, i64)* @f20 to i64
  store i64 %casted66, i64* %off65
  %t67 = inttoptr i64 %f8 to i64*
  %a68 = getelementptr i64, i64* %t67, i32 0
  %f847 = load i64, i64* %a68
  %c69 = inttoptr i64 %f847 to i64 (i64, i64)*
  %r30 = call i64 %c69(i64 %f8, i64 %f20)
  %t70 = inttoptr i64 %r30 to i64*
  %a71 = getelementptr i64, i64* %t70, i32 0
  %r3048 = load i64, i64* %a71
  %c72 = inttoptr i64 %r3048 to i64 (i64, i64)*
  %r31 = call i64 %c72(i64 %r30, i64 5)
  ret i64 %r31
}

define i64 @f10(i64 %env34, i64 %v7) {
  %t73 = inttoptr i64 %env34 to i64*
  %a74 = getelementptr i64, i64* %t73, i32 1
  %x6 = load i64, i64* %a74
  %t75 = inttoptr i64 %x6 to i64*
  %a76 = getelementptr i64, i64* %t75, i32 0
  %x635 = load i64, i64* %a76
  %c77 = inttoptr i64 %x635 to i64 (i64, i64)*
  %r11 = call i64 %c77(i64 %x6, i64 %x6)
  %t78 = inttoptr i64 %r11 to i64*
  %a79 = getelementptr i64, i64* %t78, i32 0
  %r1136 = load i64, i64* %a79
  %c80 = inttoptr i64 %r1136 to i64 (i64, i64)*
  %r12 = call i64 %c80(i64 %r11, i64 %v7)
  ret i64 %r12
}

define i64 @f15(i64 %env39, i64 %v5) {
  %t81 = inttoptr i64 %env39 to i64*
  %a82 = getelementptr i64, i64* %t81, i32 1
  %x4 = load i64, i64* %a82
  %t83 = inttoptr i64 %x4 to i64*
  %a84 = getelementptr i64, i64* %t83, i32 0
  %x440 = load i64, i64* %a84
  %c85 = inttoptr i64 %x440 to i64 (i64, i64)*
  %r16 = call i64 %c85(i64 %x4, i64 %x4)
  %t86 = inttoptr i64 %r16 to i64*
  %a87 = getelementptr i64, i64* %t86, i32 0
  %r1641 = load i64, i64* %a87
  %c88 = inttoptr i64 %r1641 to i64 (i64, i64)*
  %r17 = call i64 %c88(i64 %r16, i64 %v5)
  ret i64 %r17
}

define i64 @f14(i64 %env38, i64 %x4) {
  %t89 = inttoptr i64 %env38 to i64*
  %a90 = getelementptr i64, i64* %t89, i32 1
  %g3 = load i64, i64* %a90
  %ptr91 = call i64* @malloc(i64 16)
  %f15 = ptrtoint i64* %ptr91 to i64
  %off92 = getelementptr i64, i64* %ptr91, i32 0
  %casted93 = ptrtoint i64 (i64, i64)* @f15 to i64
  store i64 %casted93, i64* %off92
  %off94 = getelementptr i64, i64* %ptr91, i32 1
  store i64 %x4, i64* %off94
  %t95 = inttoptr i64 %g3 to i64*
  %a96 = getelementptr i64, i64* %t95, i32 0
  %g342 = load i64, i64* %a96
  %c97 = inttoptr i64 %g342 to i64 (i64, i64)*
  %r18 = call i64 %c97(i64 %g3, i64 %f15)
  ret i64 %r18
}

define i64 @f9(i64 %env33, i64 %x6) {
  %t98 = inttoptr i64 %env33 to i64*
  %a99 = getelementptr i64, i64* %t98, i32 1
  %g3 = load i64, i64* %a99
  %ptr100 = call i64* @malloc(i64 16)
  %f10 = ptrtoint i64* %ptr100 to i64
  %off101 = getelementptr i64, i64* %ptr100, i32 0
  %casted102 = ptrtoint i64 (i64, i64)* @f10 to i64
  store i64 %casted102, i64* %off101
  %off103 = getelementptr i64, i64* %ptr100, i32 1
  store i64 %x6, i64* %off103
  %t104 = inttoptr i64 %g3 to i64*
  %a105 = getelementptr i64, i64* %t104, i32 0
  %g337 = load i64, i64* %a105
  %c106 = inttoptr i64 %g337 to i64 (i64, i64)*
  %r13 = call i64 %c106(i64 %g3, i64 %f10)
  ret i64 %r13
}

define i64 @f21(i64 %env45, i64 %x2) {
  %p25108 = alloca i64
  %p22107 = alloca i64
  %t113 = inttoptr i64 %env45 to i64*
  %a114 = getelementptr i64, i64* %t113, i32 1
  %f1 = load i64, i64* %a114
  %c115 = icmp sgt i64 %x2, 0
  br i1 %c115, label %then56, label %else55
j23:
  %p22 = load i64, i64* %p22107
  ret i64 %p22
then56:
  %r24 = sub i64 %x2, 1
  %c109 = icmp sgt i64 %r24, 0
  br i1 %c109, label %then54, label %else53
else55:
  store i64 1, i64* %p22107
  br label %j23
j26:
  %p25 = load i64, i64* %p25108
  store i64 %p25, i64* %p22107
  br label %j23
then54:
  %r27 = sub i64 %x2, 1
  %t110 = inttoptr i64 %f1 to i64*
  %a111 = getelementptr i64, i64* %t110, i32 0
  %f146 = load i64, i64* %a111
  %c112 = inttoptr i64 %f146 to i64 (i64, i64)*
  %r28 = call i64 %c112(i64 %f1, i64 %r27)
  %r29 = mul i64 %x2, %r28
  store i64 %r29, i64* %p25108
  br label %j26
else53:
  store i64 1, i64* %p25108
  br label %j26
}

define i64 @f20(i64 %env44, i64 %f1) {
  %ptr116 = call i64* @malloc(i64 16)
  %f21 = ptrtoint i64* %ptr116 to i64
  %off117 = getelementptr i64, i64* %ptr116, i32 0
  %casted118 = ptrtoint i64 (i64, i64)* @f21 to i64
  store i64 %casted118, i64* %off117
  %off119 = getelementptr i64, i64* %ptr116, i32 1
  store i64 %f1, i64* %off119
  ret i64 %f21
}

define i64 @f8(i64 %env32, i64 %g3) {
  %ptr120 = call i64* @malloc(i64 16)
  %f9 = ptrtoint i64* %ptr120 to i64
  %off121 = getelementptr i64, i64* %ptr120, i32 0
  %casted122 = ptrtoint i64 (i64, i64)* @f9 to i64
  store i64 %casted122, i64* %off121
  %off123 = getelementptr i64, i64* %ptr120, i32 1
  store i64 %g3, i64* %off123
  %ptr124 = call i64* @malloc(i64 16)
  %f14 = ptrtoint i64* %ptr124 to i64
  %off125 = getelementptr i64, i64* %ptr124, i32 0
  %casted126 = ptrtoint i64 (i64, i64)* @f14 to i64
  store i64 %casted126, i64* %off125
  %off127 = getelementptr i64, i64* %ptr124, i32 1
  store i64 %g3, i64* %off127
  %t128 = inttoptr i64 %f9 to i64*
  %a129 = getelementptr i64, i64* %t128, i32 0
  %f943 = load i64, i64* %a129
  %c130 = inttoptr i64 %f943 to i64 (i64, i64)*
  %r19 = call i64 %c130(i64 %f9, i64 %f14)
  ret i64 %r19
}
