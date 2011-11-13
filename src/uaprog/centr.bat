@echo off
if x%1==x goto usage
if x%2==x goto usage
if x%3==x goto usage
if x%4==x goto usage
if x%5==x goto usage
echo \begin_command>%2.par
if %1==n goto n
if %1==w goto w
if %1==s goto s
if %1==r goto r
echo !! Bad first argument.
goto usage
:n
echo \centrality>>%2.par
goto 1
:w
echo \weak_centrality>>%2.par
goto 1
:s
echo \strong_centrality>>%2.par
goto 1
:r
echo \rectangular_centrality>>%2.par
goto 1
:1
echo \algebra_file{%2.alg}>>%2.par
if %3==one goto 1one
echo \bin_rel{\congruence{\conlist_file{%2.con}\number{%3}}}>>%2.par
goto 2
:1one
echo \bin_rel{\congruence{\one_congruence{}}}>>%2.par
goto 2
:2
if %4==one goto 2one
echo \bin_rel{\congruence{\conlist_file{%2.con}\number{%4}}}>>%2.par
goto 3
:2one
echo \bin_rel{\congruence{\one_congruence{}}}>>%2.par
goto 3
:3
if %5==all goto all
if %5==zero goto zero
echo \congruence{\conlist_file{%2.con}\number{%5}}>>%2.par
goto 4
:all
echo \conlist_file{%2.con}>>%2.par
goto 4
:zero
echo \congruence{\zero_congruence{}}>>%2.par
goto 4
:4
echo \end_command>>%2.par
if x%6==xp goto paronly
uab %2 %6
goto end
:usage
echo ?? centr t A n m k [log_level or `p'] decides if Ct(n,m;k) holds in A,
echo    where t=n(ormal),w(eak),s(trong),r(ectangular);
echo    n, m is a number or `one'; k is a number, `zero', or `all'
echo    (needs A.alg, A.con).
goto end
:paronly
echo %2.par created.
:end

