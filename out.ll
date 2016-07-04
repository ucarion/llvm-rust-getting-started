; ModuleID = 'example_module'

define i64 @main() {
entry:
  %a = alloca i64
  %b = alloca i64
  %c = alloca i64
  store i64 1, i64* %a
  store i64 0, i64* %b
  %a1 = load i64, i64* %a
  %is_nonzero = icmp ne i64 %a1, 0
  br i1 %is_nonzero, label %entry2, label %entry3

entry2:                                           ; preds = %entry
  %b5 = load i64, i64* %b
  %is_nonzero6 = icmp ne i64 %b5, 0
  br i1 %is_nonzero6, label %entry7, label %entry8

entry3:                                           ; preds = %entry
  %b10 = load i64, i64* %b
  %is_nonzero11 = icmp ne i64 %b10, 0
  br i1 %is_nonzero11, label %entry12, label %entry13

entry4:                                           ; preds = %entry14, %entry9
  %iftmp16 = phi i64 [ %iftmp, %entry9 ], [ %iftmp15, %entry14 ]
  store i64 %iftmp16, i64* %c
  %c17 = load i64, i64* %c
  %addtmp = add i64 %c17, 2
  ret i64 %addtmp

entry7:                                           ; preds = %entry2
  br label %entry9

entry8:                                           ; preds = %entry2
  br label %entry9

entry9:                                           ; preds = %entry8, %entry7
  %iftmp = phi i64 [ 11, %entry7 ], [ 40, %entry8 ]
  br label %entry4

entry12:                                          ; preds = %entry3
  br label %entry14

entry13:                                          ; preds = %entry3
  br label %entry14

entry14:                                          ; preds = %entry13, %entry12
  %iftmp15 = phi i64 [ 10, %entry12 ], [ 20, %entry13 ]
  br label %entry4
}
