@echo off
if x%1==x goto usage
if x%2==x goto usage
if x%3==x goto usage
echo \begin_command>%1.par
echo \compute_min_sets>>%1.par
echo \algebra_file{%1.alg}>>%1.par
echo \congruence{\conlist_file{%1.con}\number{%2}}>>%1.par
echo \congruence{\conlist_file{%1.con}\number{%3}}>>%1.par
echo \output_file{%1.min}>>%1.par
echo \end_command>>%1.par
if x%4==xp goto paronly
uab %1 %4
goto end
:usage
echo ?? min A n m [log_level or `p'] prepares M_A(n,m)
echo    (needs A.alg, A.con, creates A.min).
goto end
:paronly
echo %1.par created.
:end

