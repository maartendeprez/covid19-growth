# Covid-19 growth rate calculator

A program to generate tables / graphs on covid-19 growth rates in total / active / recovered cases or deaths. This helps to assess our success in "flattening the curve".

## Prerequisites

To compile and run this program, you need a haskell toolchain. See https://docs.haskellstack.org/en/stable/README/#how-to-install for more details.

## Running

To display growth tables on the terminal, run:
```
stack run -- active Hubei,China "South Korea" Italy Spain Belgium
```

To show the same information in a graph, run:
```
stack run -- --graph active Hubei,China "South Korea" Italy Spain Belgium > graph.html
```
...and open the output file in a browser.

## Usage

Usage: corona [-g|--graph] [-x|--source ARG] [-s|--smoothing ARG] 
              [-m|--minimum ARG] ITEM REGION
  Calculate daily growth rates for the covid-19 pandemic

Available options:
  -h,--help                Show this help text
  -g,--graph               Output a graph instead of a table.
  -x,--source ARG          Where to obtain the input data (csse / worldometers).
  -s,--smoothing ARG       Over how many days to calculate growth
                           rate. (default: 3)
  -m,--minimum ARG         The minimum number to trigger the start of the
                           series. (default: 50)
  ITEM                     The input series (confirmed / active / recovered /
                           deaths).
  REGION                   The region(s) to show
