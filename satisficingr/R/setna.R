



#' Set Defined Values NA
#'
#' @param x Column(s) of a dataframe
#' @param miss Values that are supposed to be set NA
#'
#' @return A dataframe will be printed to the console
#' @export
#'
#' @examples
#' require(data.table) #generation of example data
#' require(dplyr)
#'
#' #example data
#' set.seed(1313)
#' df <- data.table(
#'   first01 = c(4, -2, 4, 5, 6, 2, -1, -1, -9, 4, 5, 1, 3, 4, 3, 4, 5, -1,
#'    -1, 5, 2, 3, 4, 3, 2, 2, -1, -9, 2, 1),
#'   first02 = c(-1, -9, sample(1:5, size = 27, replace = TRUE), -1),
#'   first03 = c(sample(1:5, size = 30, replace = TRUE)),
#'   first04 = c(sample(1:5, size = 30, replace = TRUE)),
#'   first05 = c(sample(1:5, size = 30, replace = TRUE)),
#'   first06 = c(sample(1:5, size = 30, replace = TRUE)),
#'   first07 = c(sample(1:5, size = 30, replace = TRUE)),
#'   first08 = c(sample(1:5, size = 30, replace = TRUE)),
#'   second01 = c(sample(1:11, size = 30, replace = TRUE)),
#'   second02 = c(sample(1:11, size = 30, replace = TRUE)),
#'   second03 = c(sample(1:11, size = 30, replace = TRUE)),
#'   second04 = c(sample(1:11, size = 30, replace = TRUE)),
#'   second05 = c(sample(1:11, size = 30, replace = TRUE)),
#'   second06 = c(sample(1:11, size = 30, replace = TRUE)),
#'   second07 = c(sample(1:11, size = 30, replace = TRUE)),
#'   second08 = c(sample(1:11, size = 30, replace = TRUE)),
#'   third01 = c(sample(1:5, size = 30, replace = TRUE)),
#'   third02 = c(sample(1:5, size = 30, replace = TRUE)),
#'   third03 = c(sample(1:5, size = 30, replace = TRUE)),
#'   third04 = c(sample(1:5, size = 30, replace = TRUE)),
#'   third05 = c(sample(1:5, size = 30, replace = TRUE)),
#'   third06 = c(sample(1:5, size = 30, replace = TRUE)),
#'   fourth = c(sample(1:2, size = 30, replace = TRUE)),
#'   fifth = c(sample(1:11, size = 30, replace = TRUE)))
#'
#'   df[sample(1:30, size = 4), ] <- -10
#'
#'   #apply function
#'   ## First Example (Default)
#'
#'   table(df$first01, useNA = "always")
#'
#'   df2 <- df %>%
#'   mutate(across(all_of(df %>% select(contains("01")) %>% colnames()), setna)
#'
#'   table(df2$first01, useNA = "always")
#'
#'
#'   ## Second Example
#'
#'   table(df$first01, useNA = "always")
#'
#'   df3 <- df %>% mutate(across(first01, ~setna(.x, miss = "-9")))
#'
#'   table(df3$first01, useNA = "always")
#'
#'
#'
#' @author Julia Witton
#' @references Lüdecke D (2018). “sjmisc: Data and Variable Transformation Functions.” Journal of Open Source Software, 3(26), 754. doi:10.21105/joss.00754.
#'
#' @description Simplification of sjmisc function `set_na`, customized for the needs of survey data wrangling
#'
#'
#'

setna <- function(x, miss =  c(c(-1:-10),
                               "[-10] Abbruch",
                               "[-2] Trifft nicht zu",
                               "[-1] Keine Angabe / weiss nicht",
                               "[-9] Moechte ich nicht beantworten")){

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    install.packages("dplyr")
  }
  if (!requireNamespace("sjmisc", quietly = TRUE)) {
    install.packages("sjmisc")
  }
  library(dplyr)
  library(sjmisc)


  set_na(x, na = miss,
    as.tag = FALSE)
}
