; Target
target triple = "x86_64-unknown-linux-gnu"
; External declaration of the printf function
declare i32 @printf(i8* noalias nocapture, ...)

; Actual code begins
@.fmt1= global [6 x i8] c"Hello\00"@.fmt2= global [14 x i8] c"a: %d b: %d
 \00"@.fmt3= global [3 x i8] c"%d\00"

define void@f() {
call i32 (i8*, ... ) @printf(i8* getelementptr inbounds ([ 6 x i8 ], [ 6 x i8 ]* @.fmt1, i64 0, i64 0))
ret void
}
define void@main() {
%x = alloca i32
%y = alloca i32
%pair = alloca [ 2 x i32* ]
%tmp2= getelementptr inbounds i32, i32* %y, i64 0, i32 1
store i32%tmp2, i32* %tmp2
%tmp1= getelementptr inbounds i32, i32* %x, i64 0, i32 0
store i32%tmp1, i32* %tmp1
%a4 = alloca i32
%b5 = alloca i32
%c6 = alloca i32
%tmp3 = add i32 2, 5
%tmp4 = mul i32 %tmp3, 2
%tmp5 = add i32 2, 1
%tmp6 = mul i32 %tmp4, %tmp5
store i32 %tmp6, i32* %a4
store i32 0, i32* %b5
br label %while1
while1:
%tmp7= load i32, i32* %a4
%tmp8 = sub nsw i32 %tmp7, 5
%tmp15 = icmp ne i32 %tmp8, 0 
br i1 %tmp15, label %do1, label %done1 
do1:
%tmp9= load i32, i32* %a4
%tmp10 = sub nsw i32 %tmp9, 1
store i32 %tmp10, i32* %a4
%tmp11= load i32, i32* %b5
%tmp12 = add i32 %tmp11, 1
store i32 %tmp12, i32* %b5
%tmp14= load i32, i32* %a4
%tmp13= load i32, i32* %b5
call i32 (i8*, ... ) @printf(i8* getelementptr inbounds ([ 14 x i8 ], [ 14 x i8 ]* @.fmt2, i64 0, i64 0), i32 %tmp14, i32 %tmp13)
br label %while1
done1:
%a7 = alloca [ 3 x i32 ]
%b8 = alloca i32
store i32 1, i32* %b8
%tmp16= load i32, i32* %b8
%tmp17 = add i32 %tmp16, 1
%tmp18= getelementptr inbounds [ 3 x i32 ], [ 3 x i32 ]* %a7, i64 0, i32 %tmp17
store i32 2, i32* %tmp18
%tmp19= load i32, i32* %b8
call i32 (i8*, ... ) @printf(i8* getelementptr inbounds ([ 3 x i8 ], [ 3 x i8 ]* @.fmt3, i64 0, i64 0), i32 %tmp19)
%tmp20= load i32, i32* %b8
%tmp21 = add i32 %tmp20, 1
%tmp22= getelementptr inbounds [ 3 x i32 ], [ 3 x i32 ]* %a7, i64 0, i32 %tmp21
%tmp23= load i32, i32* %tmp22
%tmp24 = sub nsw i32 %tmp23, 2
%tmp31 = icmp ne i32 %tmp24, 0 
br i1 %tmp31, label %then2, label %else2 
then2:
%tmp25= getelementptr inbounds [ 3 x i32 ], [ 3 x i32 ]* %a7, i64 0, i32 0
%tmp26= load i32, i32* %tmp25
store i32 %tmp26, i32* %c6
br label %fi2
else2:
%tmp27= getelementptr inbounds [ 3 x i32 ], [ 3 x i32 ]* %a7, i64 0, i32 1
%tmp28= load i32, i32* %tmp27
store i32 %tmp28, i32* %c6
%tmp29= load i32, i32* %c6
%tmp30 = add i32 %tmp29, 3
store i32 %tmp30, i32* %c6
br label %fi2
fi2:
call void @f()
ret void
}
define i32@g(i32 %a9, i32 %b10) {
%a11 = alloca i32
store i32 %a9, i32* %a11
%b12 = alloca i32
store i32 %b10, i32* %b12
%tmp33= load i32, i32* %a11
%tmp34= load i32, i32* %b12
%tmp35 = add i32 %tmp33, %tmp34
ret i32 %tmp35
ret i32 0
}

