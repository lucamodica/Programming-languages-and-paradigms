-- ##logical proposition##

--es1
es1 = 1 <= 2 && 2 <= 3

--es2
es2 = 1 <= 0 || 0 <= 1

--es3
es3 = False && (div 1 0 == 0)

--es4
es4 = True || (div 1 0 == 0)

--es5
e1 = mod 5 2 == 0
e2 = mod 5 2 /= 0
es5and = if e1 then e2 else False
es5or = if e1 then True else e2