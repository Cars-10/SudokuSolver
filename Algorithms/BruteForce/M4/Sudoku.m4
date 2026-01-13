dnl Sudoku Solver in M4
dnl Exact match of C brute-force algorithm
dnl changequote([, ]) is already done in wrapper
changecom(/*, */)dnl
dnl
define([iterations], 0)dnl
define([inc_iterations], [define([iterations], eval(iterations + 1))])dnl
dnl
define([get], [defn([cell_$1_$2])])dnl
define([set], [define([cell_$1_$2], [$3])])dnl
dnl
define([forloop], [pushdef([$1], [$2])_forloop([$1], [$2], [$3], [$4])popdef([$1])])dnl
define([_forloop], [$4[]ifelse($1, [$3], , [define([$1], incr($1))_forloop([$1], [$2], [$3], [$4])])])dnl
dnl
define([is_valid], [pushdef([res], 1)dnl
forloop([i], 0, 8, [ifelse(get($1, i), $3, [define([res], 0)])])dnl
ifelse(res, 1, [forloop([i], 0, 8, [ifelse(get(i, $2), $3, [define([res], 0)])])])dnl
ifelse(res, 1, [pushdef([br], eval($1 / 3 * 3))dnl
pushdef([bc], eval($2 / 3 * 3))dnl
forloop([bi], 0, 2, [forloop([bj], 0, 2, [ifelse(get(eval(br+bi), eval(bc+bj)), $3, [define([res], 0)])])])dnl
popdef([br])popdef([bc])])dnl
res[]popdef([res])])dnl
dnl
define([solve], [pushdef([found], 0)dnl
pushdef([r_idx], -1)dnl
pushdef([c_idx], -1)dnl
forloop([r], 0, 8, [ifelse(found, 0, [forloop([c], 0, 8, [ifelse(found, 0, [ifelse(get(r, c), 0, [define([found], 1)define([r_idx], r)define([c_idx], c)])])])])])dnl
pushdef([solve_res], 0)dnl
ifelse(found, 0, [define([solve_res], 1)], [forloop([val], 1, 9, [ifelse(solve_res, 0, [inc_iterations[]dnl
ifelse(is_valid(r_idx, c_idx, val), 1, [set(r_idx, c_idx, val)dnl
ifelse(solve(), 1, [define([solve_res], 1)], [set(r_idx, c_idx, 0)])])])])])dnl
solve_res[]popdef([solve_res])popdef([found])popdef([r_idx])popdef([c_idx])])dnl
dnl
define([print_board], [forloop([r], 0, 8, [forloop([c], 0, 8, [get(r, c) ])errprint([
])])])dnl
dnl
errprint([Puzzle:
])print_board[]dnl
ifelse(solve(), 1, [errprint([
Puzzle:
])print_board[]errprint([
Solved in Iterations=]iterations[
])], [errprint([No solution found.
])])dnl