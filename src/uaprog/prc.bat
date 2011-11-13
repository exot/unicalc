@echo off
if x%1==x goto usage
if x%2==x goto usage
if x%3==x goto usage
if x%4==x goto usage
echo \begin_command>%2.par
echo \product_congruence>>%2.par
echo \vectorlist_file{%2.uni}>>%2.par
if %3==one goto 1one
if %3==zero goto 1zero
echo \congruence{\conlist_file{%1.con}\number{%3}}>>%2.par
goto 2
:1one
echo \congruence{\one_congruence{}}>>%2.par
goto 2
:1zero
echo \congruence{\zero_congruence{}}}>>%2.par
goto 2
:2
if %4==one goto 2one
if %4==zero goto 2zero
echo \congruence{\conlist_file{%1.con}\number{%4}}>>%2.par
goto 3
:2one
echo \congruence{\one_congruence{}}>>%2.par
goto 3
:2zero
echo \congruence{\zero_congruence{}}}>>%2.par
goto 3
:3
echo \output_file{%2.cg}>>%2.par
echo \end_command>>%2.par
echo %%>>%2.par
echo \begin_command>>%2.par
echo \find_congruence_number>>%2.par
echo \conlist_file{%2.con}>>%2.par
echo \congruence{\conlist_file{%2.cg}\number{0}}>>%2.par
echo \end_command>>%2.par
if x%5==xp goto paronly
uab %2 %5
goto end
:usage
echo ?? prc A P n m [log_level or `p'] creates n x m on P \le A x A, where
echo    n, m is a number, or `one', or 'zero'
echo    (needs A.con, P.uni, P.con, creates P.cg, its # is displayed).
goto end
:paronly
echo %2.par created.
:end

