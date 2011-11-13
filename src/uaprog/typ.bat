@echo off
if x%1==x goto usage
echo \begin_command>%1.par
echo \compute_congruence_lattice>>%1.par
echo \algebra_file{%1.alg}>>%1.par
echo \output_file{%1.lat}>>%1.par
echo \output_file{%1.con}>>%1.par
echo \output_file{%1.pri}>>%1.par
echo \output_file{%1.mi}>>%1.par
echo \end_command>>%1.par
echo %%>>%1.par
echo \begin_command>>%1.par
echo \label_congruence_lattice>>%1.par
echo \algebra_file{%1.alg}>>%1.par
echo \conlist_file{%1.con}>>%1.par
echo \output_file{%1.typ}>>%1.par
echo \end_command>>%1.par
if x%2==xp goto paronly
uab %1 %2
goto end
:usage
echo ?? typ A [log_level or `p'] labels Con(A)
echo    (needs A.alg, creates A.lat,con,pri,mi,typ).
goto end
:paronly
echo %1.par created.
:end

