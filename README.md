# enkar

`enkar` is an R package designed to read data from [1ka](https://www.1ka.si/) files. 1ka does not provide an option to export data in SPSS format with all the variable information, without resorting to running SPSS scripts (.sps files). This package is intended for users who prefer not to (or cannot) use SPSS, but still require data in the SPSS format with as much information as possible, beyond what is available in regular Excel or CSV exports.


## Instalation

You can install `enkar` directly from this repository with `remotes` package.

```r
install.packages('remotes')
remotes::install_github('karakatic/enkar')
```

Or with R `devtools`.

```r
devtools::install_github('karakatic/enkar')
```

## Usage

To use `enkar`, first export the data from 1ka by downloading two files:

- The SPSS script file (.sps)
- The raw data file (.txt)

Next, use the `enkar` package to obtain the data with all the information into R.

```r
library(enkar)

# Reads the data.sps script file and the default raw data file (same filename as script file + '_podatki.txt') and returns a data frame.
data <- read_1ka('data.sps')

# Reads the data.sps and data_podatki.txt files and returns a data frame.
data <- read_1ka('data.sps', 'data_podatki.txt')

# Reads the data.sps and returns a data frame where missing values are not denoted as NAs.
data <- read_1ka('data.sps', user_na = FALSE)

# Reads the data.sps and returns a tibble data frame.
data <- read_1ka('data.sps', as.tibble = TRUE)
```

## License

`enkar` is licensed under GPL-3 and is free and open-source software.
