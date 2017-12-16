# Clean columns names
#'
#' @title Clean columns names
#' @description This function cleans dataframe columns names removing especial symbols and blank space
#' @param df  dataframe
#' @importFrom dplyr %>%
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_replace_all
#' @keywords Clean columns names
#' @details details
#' @examples
#' \dontrun{
#' clean_names(df)
#' }
#' @export

clean_names <- function(df) {
  colnames(df) <- df %>%
    names() %>%
    str_to_lower() %>%

    #           "aaaeeioouUca", .) %>%
    #  iconv(., "utf-8", to = "ASCII//TRANSLIT") %>%
    str_replace_all("\\.", "_") %>%
    str_replace_all("\\..", "_") %>%
    str_replace_all("\\,", "_") %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("-", "_") %>%
    str_replace_all("\\+", "_") %>%
    str_replace_all("\\*", "_") %>%
    str_replace_all("\\/,", "_") %>%
    str_replace_all("\\(", "_") %>%
    str_replace_all("\\)", "_") %>%
    str_replace_all("\\'", "_") %>%
    str_replace_all("___", "_") %>%
    str_replace_all("__", "_")
  return(df)
}


# Remove Columns with too many NA values
#'
#' @title Remove Columns with too many NA values
#' @description This function Remove Columns with too many NA values
#' @importFrom dplyr %>% filter select pull
#' @param df dataframe
#' @param d_count_na dataframe with NA information created by na.Count
#' @param tolerance percentual value of tolerance of NA values in a variable
#' @param verbose verbose
#' @details details
#' @examples
#' \dontrun{
#' remove_var_na(df, d.count.na, tolerance = 30, verbose = TRUE)
#' }
#' @export
#'
remove_var_na <- function(df, d_count_na, tolerance = 75, verbose = TRUE) {
  na_relative <- NULL
  variav <- NULL
  v.remove <- d_count_na %>% filter(na_relative > tolerance) %>% pull(variav)
  dr <- df %>% select(-one_of(v.remove))
  if (verbose == TRUE) {
    print(paste(length(v.remove), "variables removed"))
  }
  return(dr)
}


#' Convert variables to factor
#'
#' @title  Convert variables to factor
#' @description This function  Convert variables to factor
#' @param df  dataframe
#' @param vf vector with number of the columns to be removed
#' @importFrom dplyr '%>%' mutate_at funs
#' @export
to_factor <- function(df, vf) {
  df.fac <- df %>% mutate_at(vf, funs(factor))
  return(df.fac)
}
