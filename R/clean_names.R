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
  "." <- NULL
  colnames(df) <- df %>% names %>%
    str_to_lower %>%
    iconv(., "utf-8", to='ASCII//TRANSLIT') %>%
    str_replace_all("\\.", "_") %>%
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
