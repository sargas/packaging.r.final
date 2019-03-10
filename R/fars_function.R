#' Read the FARS data
#'
#' This function reads in one of the Fatality Analysis Reporting System datasets
#' provided by the US National Highway Traffic Safety Administration and returns
#' it with a \code{\link{tbl_df}} class.
#' This function will stop with an error if the filename does not exist.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @param filename A character vector giving the path to the file containing the
#'   dataset
#'
#' @return This function returns a data frame parsed from the given dataset
#'
#' @examples
#' fars_read(make_filename(2015))
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Format the file name used by the FARS datasets
#'
#' This function forms the filename of the datasets for a particular year as
#' used by the Fatality Analysis Reporting System datasets provided by the US
#' National Highway Traffic Safety Administration
#'
#' @param year a year which is coercable to an integer with the
#'   \code{\link{as.integer}} function
#'
#' @return Returns a character string with the file name
#'
#' @examples
#' make_filename(2013)
#' make_filename(2015)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        system.file("extdata", sprintf("accident_%d.csv.bz2", year), package="packaging.r.final")
}

#' Load months and years of FARS records for given year(s)
#'
# This function finds, for each year passed to it, a data table without any data
# besides the month and year of the record.
#'
#' This function will stop with an error if the a dataset for the year does not
#' exist in the working directory or if any empty vector is used.
#'
#' @importFrom dplyr mutate_ select_
#' @importFrom magrittr %>%
#'
#' @param years one or more years which is coercable to an integer with the
#'   \code{\link{as.integer}} function
#'
#' @return a list of data tables, one for each value in years
#'
#' @examples
#' fars_read_years(2013)
#' fars_read_years(c(2013, 2015))
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate_(dat, year = ~year) %>%
                                dplyr::select_(.dots=c('MONTH', 'year'))
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Generate table of FARS records per month for given years
#'
#' This function generates a summary table giving, for each month, the number of
#' records with the Fatality Analysis Reporting System dataset for that year. This
#' is arranged so that the results for each year are reported in a single column.
#'
#' This function will stop with an error if the a dataset for the year does not
#' exist in the working directory or if any empty vector is used.
#'
#' @importFrom dplyr bind_rows group_by_ summarize_
#' @importFrom tidyr spread_
#' @importFrom magrittr %>%
#'
#' @param years one or more years which is coercable to an integer with the
#'   \code{\link{as.integer}} function
#'
#' @return a data.frame containing a column labeled MONTH (with values ranging
#'   from 1 to 12) and another column for each year listing the totals per month.
#'
#' @examples
#' fars_summarize_years(2013) # display summary for 2013
#' fars_summarize_years(c(2013,2015)) # show 2013 and 2015 data
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by_(~ year, ~ MONTH) %>%
                dplyr::summarize_(n = ~ n()) %>%
                tidyr::spread_(key_col='year', value_col='n')
}

#' Show reported fatalities on a state map
#'
#' This function displays a map of the state using the maps package and plots
#' the locations for all records in the Fatality Analysis Reporting System
#' loccateed within that state. This function stops with an error if told to
#' plot a state number that is not in the dataset for that year. Additionally,
#' an error with be shown if a dataset for the specified year does not exist.
#'
#' @importFrom dplyr filter_
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @param state.num a single integer value identifying the state
#' @param year a year which is coercable to an integer with the
#'   \code{\link{as.integer}} function
#'
#' @return There is no value returned
#'
#' @examples
#' fars_map_state(6, 2013) #  California traffic fatalities in 2013
#' fars_map_state(17, 2015) # Illinois traffic fatalities in 2015
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter_(data, ~ STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
