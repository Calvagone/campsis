## Test environments
* R-HUB: Fedora Linux, R-devel, clang, gfortran (1 NOTE)
* R-HUB: Windows Server 2022, R-devel, 64 bit (1 NOTE)
* R-HUB: Ubuntu Linux 20.04.1 LTS, R-release, GCC (1 NOTE, then ERROR with R-HUB docker when saving artifacts...)
* local R installation, R 4.2.0

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Have the issues why your package was archived been fixed?
The removal of campsis on 2022-04-22 is unclear to me.

In the latest check results (https://cran-archive.r-project.org/web/checks/2022/2022-04-22_check_results_campsis.html), I see an error in flavor ‘r-devel-linux-x86_64-fedora-clang’.

In the tests of that flavor, I see:
*	Package suggested but not available for checking: ‘RxODE’
*	And then, as a consequence, all simulation tests with RxODE do not pass (which is logic!)

Regarding the intermittent M1mac additional issues of this package, they disappeared after a few days on CRAN.
Please let me know if they still appear in this new submission. Thanks!
