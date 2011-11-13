@echo off
if x%1==x goto usage
if x%2==x goto usage
if x%3==x goto usage
echo \begin_command>%1.par
echo \create_factor_algebra>>%1.par
echo \algebra_file{%1.alg}>>%1.par
echo \congruence{\conlist_file{%1.con}\number{%2}}>>%1.par
echo \output_file{%3.alg}>>%1.par
echo \end_command>>%1.par
if x%4==xp goto paronly
uab %1 %4
goto end
:usage
echo ?? fact A t F [log_level or `p'] prepares F=A/t
echo    (needs A.alg, A.con, creates F.alg).
goto end
:paronly
echo %1.par created.
:end

