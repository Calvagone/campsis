## Test environments
* local R installation, R 4.2.0
* ubuntu-20.04 (GitHub Actions), R 4.2.0
* macOS-latest (GitHub Actions), R 4.2.0
* windows-latest (GitHub Actions), R 4.2.0
* Windows Server 2022, R-devel, 64 bit (R-hub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (R-hub)
* Fedora Linux, R-devel, clang, gfortran (R-hub)

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission, package was archived on CRAN

## Have the issues why your package was archived been fixed?

- M1Mac issues were resolved.
- All simulation tests on CRAN are now executed with rxode2 (instead of RxODE) and mrgsolve.
