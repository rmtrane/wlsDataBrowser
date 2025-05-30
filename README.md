

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
wlsDataBrowser::wlsDataBrowserApp()
```

You can optionally set the file path so you don’t manually have to
choose the file when running the application. Add the following to your
.Rprofile file with the path to your local copy of the data:

``` r
options(wlsDataBrowser.data_path = "/path/to/wls/data.dta")
```

You will need to restart your R session for the changes to take effect.

The app shows a table of all the variables in the data. The table
includes three columns:

- the variable name as it appears in the data file
- the round the variable relates to. This is infered from the variable
  label
- the variable label extracted from the label

## RStudio add-ins

Once installed, you should be able to find two add-ins in RStudio:

- `WLS Data Script Sketch` runs the function `load_data_script()` which
  creates a new file prefilled with a sketch for loading the WLS data.
  If the option `wlsDataBrowser.data_path` is set, this is used for the
  file argument in `haven::read_dta` call. If not, the file argument
  would have to be filled in manually.
- `WLS Data Browser` runs the Shiny application, which allows one to
  browse the data.

## Positron keyboard shortcut

If using [Positron](https://positron.posit.co), the following snippet
can be added to `keybindings.json` to easily launch the data browser
(here, keyboard combo `cmd+alt+shift+w` launches the data browser):

``` json
{
    "key": "cmd+alt+shift+w",
    "command": "workbench.action.executeCode.console",
    "description": "Browse WLS Data",
    "args": {
        "langId": "r",
        "code": "wlsDataBrowser:::wlsDataBrowser()"
    }
}
```

Search for `Open Keyboard Shortcuts (JSON)` using the command palette
(`cmd/ctrl+shift+p`) to open the `keybindings.json` file.
