@echo off
if x%1==x goto usage
echo \begin_command>%1.par
echo \unary_polynomials>>%1.par
echo \algebra_file{%1.alg}>>%1.par
echo \output_file{%1.p1}>>%1.par
echo \output_file{%1.p1e}>>%1.par
echo \end_command>>%1.par
if x%2==xp goto paronly
uab %1 %2
goto end
:usage
echo ?? p1 A [log_level or `p'] computes unary polynomials
echo    (needs A.alg, creates A.p1, A.p1e).
goto end
:paronly
echo %1.par created.
:end

