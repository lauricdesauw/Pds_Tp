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
%a3 = alloca i32
%b4 = alloca i32
%c5 = alloca i32
%tmp1 = add i32 2, 5
%tmp2 = mul i32 %tmp1, 2
%tmp3 = add i32 2, 1
%tmp4 = mul i32 %tmp2, %tmp3
store i32 %tmp4, i32* %a3
store i32 0, i32* %b4
br label %while1
while1:
%tmp5= load i32, i32* %a3
%tmp6 = sub nsw i32 %tmp5, 5
%tmp13 = icmp ne i32 %tmp6, 0 
br i1 %tmp13, label %do1, label %done1 
do1:
%tmp7= load i32, i32* %a3
%tmp8 = sub nsw i32 %tmp7, 1
store i32 %tmp8, i32* %a3
%tmp9= load i32, i32* %b4
%tmp10 = add i32 %tmp9, 1
store i32 %tmp10, i32* %b4
%tmp12= load i32, i32* %a3
%tmp11= load i32, i32* %b4
call i32 (i8*, ... ) @printf(i8* getelementptr inbounds ([ 14 x i8 ], [ 14 x i8 ]* @.fmt2, i64 0, i64 0), i32 %tmp12, i32 %tmp11)
br label %while1
done1:
%a6 = alloca [ 3 x i32 ]
%b7 = alloca i32
%c8 = alloca i32
store i32 1, i32* %b7
%tmp14= load i32, i32* %b7
%tmp15 = add i32 %tmp14, 1
%tmp16= getelementptr inbounds [ 3 x i32 ], [ 3 x i32 ]* %a6, i64 0, i32 %tmp15
store i32 2, i32* %tmp16
%tmp17= load i32, i32* %b7
call i32 (i8*, ... ) @printf(i8* getelementptr inbounds ([ 3 x i8 ], [ 3 x i8 ]* @.fmt3, i64 0, i64 0), i32 %tmp17)
%tmp18= load i32, i32* %b7
%tmp19 = add i32 %tmp18, 1
%tmp20= getelementptr inbounds [ 3 x i32 ], [ 3 x i32 ]* %a6, i64 0, i32 %tmp19
%tmp21= load i32, i32* %tmp20
%tmp22 = sub nsw i32 %tmp21, 2
%tmp29 = icmp ne i32 %tmp22, 0 
br i1 %tmp29, label %then2, label %else2 
then2:
%tmp23= getelementptr inbounds [ 3 x i32 ], [ 3 x i32 ]* %a6, i64 0, i32 0
%tmp24= load i32, i32* %tmp23
store i32 %tmp24, i32* %c8
br label %fi2
else2:
%tmp25= getelementptr inbounds [ 3 x i32 ], [ 3 x i32 ]* %a6, i64 0, i32 1
%tmp26= load i32, i32* %tmp25
store i32 %tmp26, i32* %c8
%tmp28= load i32, i32* %c8
call i32 @g(i32 3,i32 %tmp28)
store i32 %tmp27, i32* %c8
br label %fi2
fi2:
call void @f()
ret void
}
define i32@g(i32 %a9, i32 %b10) {
%a11 = alloca i32
store i32 %a9, i32* %TODOa11
%b12 = alloca i32
store i32 %b10, i32* %TODOb12
%tmp31= load i32, i32* %a11
%tmp32= load i32, i32* %b12
%tmp33 = add i32 %tmp31, %tmp32
ret i32 %tmp33
ret i32 0
}

