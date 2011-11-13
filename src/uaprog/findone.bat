@echo off
if x%1==x goto usage
echo \begin_command>%1.par
echo \find_congruence_number>>%1.par
echo \conlist_file{%1.con}>>%1.par
echo \congruence{\one_congruence{}}>>%1.par
echo \end_command>>%1.par
if x%2==xp goto paronly
uab %1 %2
goto end
:usage
echo ?? findone A [log_level or `p'] finds one_congruence in Con(A)
echo    (needs A.con its # is displayed).
goto end
:paronly
echo %1.par created.
:end

