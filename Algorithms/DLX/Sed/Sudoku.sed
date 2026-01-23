#!/bin/sed -nf
s/0/./g
s/$/|0/
s/^/>/
:main
/>|/ b solved
s/>\([1-9]\)/\1>/
t main
s/>\./>a/
t check
:try_next
s/>a/>b/; t check
s/>b/>c/; t check
s/>c/>d/; t check
s/>d/>e/; t check
s/>e/>f/; t check
s/>f/>g/; t check
s/>g/>h/; t check
s/>h/>i/; t check
s/>[a-i]/./
:backtrack_search
/^>/ b nosol
s/\(.\)/>\1/
s/>\([1-9]\)/<\1/
t backtrack_move_further
s/>\([a-i]\)/>\1/
t try_next
b backtrack_search
:backtrack_move_further
s/<\(.\)/>\1/
s/\(.\)/>\1/
b backtrack_search
:check
s/|\([0-9]*\)$/|\1:/
:inc
s/9:/:0/; t inc
s/8:/9/; t id
s/7:/8/; t id
s/6:/7/; t id
s/5:/6/; t id
s/4:/5/; t id
s/3:/4/; t id
s/2:/3/; t id
s/1:/2/; t id
s/0:/1/; t id
s/:/1/
:id
s/://g
h
s/|.*//; s/>//
y/abcdefghi/123456789/
s/.{9}/& /
g
/\([1-9]\)[^ ]*\1/ b invalid
s/ //g
/\([1-9]\).{8}\1/ b invalid
/\([1-9]\).{17}\1/ b invalid
/\([1-9]\).{26}\1/ b invalid
/\([1-9]\).{35}\1/ b invalid
/\([1-9]\).{44}\1/ b invalid
/\([1-9]\).{53}\1/ b invalid
/\([1-9]\).{62}\1/ b invalid
/\([1-9]\).{71}\1/ b invalid
g; s/|.*//; s/>//; y/abcdefghi/123456789/
s/^\(..\)\(..\)\(..\)\(..\)\(..\)\(..\)\(..\)\(..\)\(..\).*/\1\4\7 \2\5\8 \3\6\9/
/\([1-9]\)[^ ]*\1/ b invalid
g; s/|.*//; s/>//; y/abcdefghi/123456789/
s/^.\{27\}\(..\)\(..\)\(..\)\(..\)\(..\)\(..\)\(..\)\(..\)\(..\).*/\1\4\7 \2\5\8 \3\6\9/
/\([1-9]\)[^ ]*\1/ b invalid
g; s/|.*//; s/>//; y/abcdefghi/123456789/
s/^.\{54\}\(..\)\(..\)\(..\)\(..\)\(..\)\(..\)\(..\)\(..\)\(..\).*/\1\4\7 \2\5\8 \3\6\9/
/\([1-9]\)[^ ]*\1/ b invalid
g
s/>\(.\)/\1>/
b main
:invalid
g
b try_next
:solved
s/>//; y/abcdefghi/123456789/
p
q
:nosol
i\
No solution found.
q