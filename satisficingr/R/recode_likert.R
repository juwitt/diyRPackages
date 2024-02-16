

#' Recode Likert scales
#'
#' @param x Column of a dataframe
#' @param i The Likert scale's theoretical maximum value
#' @param set_na_option If TRUE: values from -1 to -10 are set NA. Defaults to FALSE
#'
#' @return A dataframe will be printed in the console
#' @export
#'
#' @author Julia Witton
#' @references Lüdecke D (2018). “sjmisc: Data and Variable Transformation Functions.” Journal of Open Source Software, 3(26), 754. doi:10.21105/joss.00754.
#'
#' @description Function rescales Likert scales. Optional, missing codes can be set NA using `set_na` from package sjmisc.
#'
#' **CAVE** The recoding works if all theoretical Likert levels are represented in the data's values.
#'
#'
#'
#'
#' @examples
#' require(data.table) #generation of example data
#' require(dplyr)
#'
#' #example data
#' set.seed(1313)
#'
#' df <- data.table(
#'   first01 = c(4, -2, 4, 5, 6, 2, -1, -1, -9, 4, 5, 1, 3, 4, 3, 4, 5, -1, -1,
#'    5, 2, 3, 4, 3, 2, 2, -1, -9, 2, 1),
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
#'
#'   table(df$first04, useNA = "always")
#'
#'   df2 <- df %>% mutate(across(first04, ~recode_likert(.x, i = 5, set_na_option = TRUE)))
#'
#'   table(df3$first04, useNA = "always")
#'
#'
#'
#'
#'
#'
recode_likert <- function(x, i, set_na_option = FALSE) {

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    install.packages("dplyr")
  }
  library(dplyr)

  if(set_na_option == TRUE) {

    if (!requireNamespace("sjmisc", quietly = TRUE)) {
      install.packages("sjmisc")
    }
    library(sjmisc)

    # set NA codes NA
    x <- set_na(x, na = c(-1:-10), as.tag = FALSE)
  }
  # recode
  case_when(x %in% c(1:i) ~ ((i+1) - x),
            TRUE ~ x)

}

