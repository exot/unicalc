@echo off
if x%1==x goto usage
if x%2==x goto usage
if x%3==x goto usage
echo \begin_command>%1.par
echo \create_direct_product>>%1.par
echo \number{2}>>%1.par
echo \algebra_file{%1.alg}>>%1.par
echo \algebra_file{%2.alg}>>%1.par
echo \output_file{%3.alg}>>%1.par
echo \output_file{%3.uni}>>%1.par
echo \end_command>>%1.par
if x%4==xp goto paronly
uab %1 %4
goto end
:usage
echo ?? pr A B P [log_level or `p'] prepares P=AxB
echo    (needs A.alg, B.alg, creates P.alg).
goto end
:paronly
echo %1.par created.
:end

