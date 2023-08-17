# shoredate

<details>

* Version: 1.1.0
* GitHub: https://github.com/isakro/shoredate
* Source code: https://github.com/cran/shoredate
* Date/Publication: 2023-06-02 23:40:02 UTC
* Number of recursive dependencies: 116

Run `revdepcheck::revdep_details(, "shoredate")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running 'testthat.R'
     ERROR
    Running the tests in 'tests/testthat.R' failed.
    Last 13 lines of output:
       1. └─elevatr::get_elev_raster(target_wgs84, z = 14, src = "aws") at test-shoreline_date.R:111:2
       2.   └─elevatr:::loc_check(locations, prj)
       3.     ├─sf::st_coordinates(locations)
       4.     └─sf:::st_coordinates.sfc(locations)
       5.       └─base::matrix(...)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-shoreline_date.R:113:3'): finding site elevation from a raster works ──
      `shoreline_date(target_point, elevation = elev_raster)` threw an unexpected error.
      Message: [extract] raster has no values
      Class:   simpleError/error/condition
      
      [ FAIL 1 | WARN 1 | SKIP 1 | PASS 112 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   R CMD check timed out
    

