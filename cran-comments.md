## Test environments
* local R installation, R 4.0.3
* win-builder (devel)

## R CMD check results

```
── R CMD check ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
─  using log directory ‘/tmp/RtmpI8SZf1/HistDat.Rcheck’ (444ms)
─  using R version 4.0.3 (2020-10-10)
─  using platform: x86_64-conda-linux-gnu (64-bit)
─  using session charset: UTF-8
─  using options ‘--no-manual --as-cran’
✓  checking for file ‘HistDat/DESCRIPTION’
─  checking extension type ... Package
─  this is package ‘HistDat’ version ‘0.1.0’
─  package encoding: UTF-8
✓  checking package namespace information
✓  checking package dependencies (1.5s)
✓  checking if this is a source package
✓  checking if there is a namespace
✓  checking for executable files ...
✓  checking for hidden files and directories
✓  checking for portable file names
✓  checking for sufficient/correct file permissions
✓  checking serialization versions
✓  checking whether package ‘HistDat’ can be installed (2.9s)
✓  checking installed package size ...
✓  checking package directory ...
✓  checking for future file timestamps (1.3s)
✓  checking ‘build’ directory
✓  checking DESCRIPTION meta-information (364ms)
✓  checking top-level files
✓  checking for left-over files
✓  checking index information ...
✓  checking package subdirectories (367ms)
✓  checking R files for non-ASCII characters ...
✓  checking R files for syntax errors ...
✓  checking whether the package can be loaded (348ms)
✓  checking whether the package can be loaded with stated dependencies (342ms)
✓  checking whether the package can be unloaded cleanly (338ms)
✓  checking whether the namespace can be loaded with stated dependencies (366ms)
✓  checking whether the namespace can be unloaded cleanly (408ms)
✓  checking loading without being on the library search path (387ms)
✓  checking dependencies in R code (376ms)
✓  checking S3 generic/method consistency (1.3s)
✓  checking replacement functions (397ms)
✓  checking foreign function calls (404ms)
✓  checking R code for possible problems (3.1s)
✓  checking Rd files (366ms)
✓  checking Rd metadata ...
✓  checking Rd line widths ...
✓  checking Rd cross-references ...
✓  checking for missing documentation entries (413ms)
✓  checking for code/documentation mismatches (1.2s)
✓  checking Rd \usage sections (1.4s)
✓  checking Rd contents ...
✓  checking for unstated dependencies in examples ...
✓  checking installed files from ‘inst/doc’
✓  checking files in ‘vignettes’
✓  checking examples (1.4s)
✓  checking for unstated dependencies in ‘tests’ ...
─  checking tests ...
✓  Running ‘testthat.R’ (903ms)
✓  checking for unstated dependencies in vignettes (1.2s)
✓  checking package vignettes in ‘inst/doc’ ...
✓  checking re-building of vignette outputs (1.7s)
✓  checking for non-standard things in the check directory
✓  checking for detritus in the temp directory
   
   
── R CMD check results ──────────────────────────────────────────────────────────────────────────────────────────────────────────────── HistDat 0.1.0 ────
Duration: 25.1s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓
```

## Winbuilder Output

```
* using log directory 'd:/RCompile/CRANguest/R-release/HistDat.Rcheck'
* using R version 4.0.4 (2021-02-15)
* using platform: x86_64-w64-mingw32 (64-bit)
* using session charset: ISO8859-1
* checking for file 'HistDat/DESCRIPTION' ... OK
* checking extension type ... Package
* this is package 'HistDat' version '0.1.0'
* package encoding: UTF-8
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Michael Milton <michael.r.milton@gmail.com>'

New submission
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking serialization versions ... OK
* checking whether package 'HistDat' can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking for future file timestamps ... OK
* checking 'build' directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* loading checks for arch 'i386'
** checking whether the package can be loaded ... OK
** checking whether the package can be loaded with stated dependencies ... OK
** checking whether the package can be unloaded cleanly ... OK
** checking whether the namespace can be loaded with stated dependencies ... OK
** checking whether the namespace can be unloaded cleanly ... OK
** checking loading without being on the library search path ... OK
** checking use of S3 registration ... OK
* loading checks for arch 'x64'
** checking whether the package can be loaded ... OK
** checking whether the package can be loaded with stated dependencies ... OK
** checking whether the package can be unloaded cleanly ... OK
** checking whether the namespace can be loaded with stated dependencies ... OK
** checking whether the namespace can be unloaded cleanly ... OK
** checking loading without being on the library search path ... OK
** checking use of S3 registration ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... [4s] OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd line widths ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking installed files from 'inst/doc' ... OK
* checking files in 'vignettes' ... OK
* checking examples ...
** running examples for arch 'i386' ... [2s] OK
** running examples for arch 'x64' ... [1s] OK
* checking for unstated dependencies in 'tests' ... OK
* checking tests ...
** running tests for arch 'i386' ... [2s] OK
  Running 'testthat.R' [1s]
** running tests for arch 'x64' ... [2s] OK
  Running 'testthat.R' [1s]
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in 'inst/doc' ... OK
* checking re-building of vignette outputs ... [2s] OK
* checking PDF version of manual ... OK
* checking for detritus in the temp directory ... OK
* DONE
Status: 1 NOTE
```

## CRAN Comments
> Please always write package names, software names and API (application programming interface) names in single quotes in title and description.
e.g: --> 'histdat'

Done. Notably the package is now called `HistDat` however.

> Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar)
Missing Rd-tags:
      as.ecdf.default.Rd: \value
      as.ecdf.hist_dat.Rd: \value
      as.vector.default.Rd: \value
      as.vector.hist_dat.Rd: \value
      sd.default.Rd: \value
      var.default.Rd: \value

Done. HistDat uses S4 classes now, but each implemented method now has a `\value` tag

> Please add some more small executable examples in your Rd-files to illustrate the use of the exported function but also enable automatic testing.

Done.
