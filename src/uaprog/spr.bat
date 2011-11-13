@echo off
if x%1==x goto usage
if x%2==x goto usage
echo \begin_command>%2.par
echo \generate_subalgebra>>%2.par
echo \algebra_file{%1.alg}>>%2.par
echo \vectorlist_file{%2.vlf}>>%2.par
echo \output_file{%2.uni}>>%2.par
echo \end_command>>%2.par
echo %%>>%2.par
echo \begin_command>>%2.par
echo \create_subproduct_algebra>>%2.par
echo \vectorlist_file{%2.uni}>>%2.par
echo \algebra_file{%1.alg}>>%2.par
echo \algebra_file{%1.alg}>>%2.par
echo \output_file{%2.alg}>>%2.par
echo \end_command>>%2.par
if x%3==xp goto paronly
uab %2 %3
goto end
:usage
echo ?? spr A B [log_level or `p'] computes Sg(B) in A^2
echo    (needs A.alg, B.vlf, creates B.uni, B.alg).
goto end
:paronly
echo %2.par created.
:end

