@echo off
if x%1==x goto usage
if x%2==x goto usage
echo \begin_command>%1.par
echo \generate_congruence>>%1.par
echo \algebra_file{%1.alg}>>%1.par
echo \vectorlist_file{%2.br}>>%1.par
echo \output_file{%2.cg}>>%1.par
echo \end_command>>%1.par
if not exist %1.con goto 1
echo %%>>%1.par
echo \begin_command>>%1.par
echo \find_congruence_number>>%1.par
echo \conlist_file{%1.con}>>%1.par
echo \congruence{\conlist_file{%2.cg}\number{0}}>>%1.par
echo \end_command>>%1.par
:1
if x%3==xp goto paronly
uab %1 %3
goto end
:usage
echo ?? cg A p [log_level or `p'] computes Cg(p) in A
echo    (needs A.alg, p.br, creates p.cg).
echo    If A.con exists, then displays congruence number.
goto end
:paronly
echo %1.par created.
:end

