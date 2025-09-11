## Resubmission
This is a resubmission. In this version I have:

* Addressed the NOTE found on CRAN via package web check (copied below) by updating the R dependency to R (>= 4.1.0) in the DESCRIPTION file.

  Missing dependency on R >= 4.1.0 because package code uses the pipe
  |> or function shorthand (...) syntax added in R 4.1.0.
  File(s) using such syntax:
    ‘art_pals.R’ ‘art_pals.Rd’ ‘circle_data.R’ ‘circle_data.Rd’
    ‘grid_maker.R’ ‘group_numbers.R’ ‘packer.R’ ‘packer.Rd’ ‘rotator.R’
    ‘square_data.R’ ‘square_data.Rd’ ‘wave_data.R’ ‘wave_data.Rd’
    

* Not added any references describing the methods in this package as I'm not aware of any that need to be listed. To the best of my knowledge, no references are needed.

* Ensured that all updates should be backwards compatible to the previous version.

## revdepcheck results

I checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * I saw 0 new problems
 * I failed to check 0 packages


## R CMD check results

0 errors | 0 warnings | 0 notes



