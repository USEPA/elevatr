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
          old                     | new                        
      [3] Output                  | Output                  [3]
      [4]   ===============       |   ===============       [4]
      [5]   Site:  1              |   Site:  1              [5]
      [6]   Elevation:  59.96235  -   Elevation:  59.96506  [6]
      [7]                         |                         [7]
      [8]   95% HDR:              |   95% HDR:              [8]
      [9]   7010 BCE-4880 BCE     |   7010 BCE-4880 BCE     [9]
      
      * Run `testthat::snapshot_accept('shoreline_date')` to accept the change.
      * Run `testthat::snapshot_review('shoreline_date')` to interactively review the change.
      
      [ FAIL 1 | WARN 1 | SKIP 1 | PASS 112 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   R CMD check timed out
    

