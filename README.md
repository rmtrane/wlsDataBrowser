
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wlsDataBrowser

<!-- badges: start -->
<!-- badges: end -->

This R-package contains a Shiny application to explore the WLS data. You
will need to obtain a copy of the data for yourself. For more, see
<https://wls.wisc.edu>.

For more details about the data, see the [WLS data
portal](https://wls.portal.ssc.wisc.edu).

## Installation

You can install the development version of wlsDataBrowser like so:

``` r
remotes::install_github("rmtrane/wlsDataBrowser")
```

## Run App

You can run the Shiny application interactively using the following:

``` r
wlsDataBrowser::wlsDataBrowser()
```

You can optionally set the file path so you don’t have to choose a file
every times. Add the following to your .Rprofile file with the path to
your local copy of the data:

``` r
options(wlsDataBrowser.data_path = "/path/to/wls/data.dta")
```

## RStudio add-in

Once installed, you should be able to find the `wlsDataBrowser` in the
RStudio add-in dropdown menu. You might have to restart RStudio for the
add-in to show up.
