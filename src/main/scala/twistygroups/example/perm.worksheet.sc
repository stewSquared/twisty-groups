import perms._

"ABC".permutations foreach println
"123".permutations foreach println

val id = Perm()
val a = Perm(1, 2)
val b = Perm(1, 3, 2)

a(2)
b(3)
(a * b)(3)

// The S3 group
id
a
b
a * b
b * a
b * b

a * a
a.inverse
b.inverse

val fourCycle = Perm(1,2)(2,3)(3,4)
fourCycle(4,3)(3,2)(2,1)

val p = Perm(1,2,3,4,5)
val q = Perm(5,6,7,8,9)
Perm(1,3,6,7,5)(1,6,3)(1,5,7)

val s = Perm(1,4,8,6,3,2,5)
s >> Perm(1,5,2) >> Perm(1,3,6) >> Perm(1,8,4)
