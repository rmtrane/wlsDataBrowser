

<!-- README.md is generated from README.Rmd. Please edit that file -->

# wlsDataBrowser

<!-- badges: start -->

[![R-CMD-check](https://github.com/rmtrane/wlsDataBrowser/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rmtrane/wlsDataBrowser/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This R-package contains a Shiny application to explore the WLS data. You
will need to obtain a copy of the data for yourself. For more, see
https://wls.wisc.edu.

For more details about the data, see the [WLS data
portal](https://wls.portal.ssc.wisc.edu).

## Installation

You can install `wlsDataBrowser` using the `remotes` packages as
follows:

``` r
remotes::install_github("rmtrane/wlsDataBrowser")
```

## Run App

You can run the Shiny application interactively using the following:

``` r
wlsDataBrowser::wlsDataBrowser()
```

You can optionally set the file path so you don’t manually have to
choose the file when running the application. Add the following to your
.Rprofile file with the path to your local copy of the data:

``` r
options(wlsDataBrowser.data_path = "/path/to/wls/data.dta")
```

You will need to restart your R session for the changes to take effect.

## RStudio add-in

Once installed, you should be able to find the `wlsDataBrowser` in the
RStudio add-in dropdown menu. You might have to restart RStudio for the
add-in to show up.

## Positron keyboard shortcut

If using [Positron](https://positron.posit.co), the following snippet
can be added to `keybindings.json` to easily launch the data browser
(here, keyboard combo `cmd+alt+shift+w` launches the data browser):

``` json
{
    "key": "cmd+alt+shift+w",
    "command": "workbench.action.executeCode.silently",
    "description": "Browse WLS Data",
    "args": {
        "langId": "r",
        "code": "wlsDataBrowser:::wlsDataBrowser()"
    }
}
```

Search for `Open Keyboard Shortcuts (JSON)` using the command palette
(`cmd/ctrl+shift+p`) to open the `keybindings.json` file.
