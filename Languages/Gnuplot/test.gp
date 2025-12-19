function $f(x) << EOF
s = 0
do for [i=1:x] { s = s + i }
return s
EOF
print $f(5)