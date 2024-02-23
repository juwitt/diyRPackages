
#' Calculation of the mean root of squares of items over multiple item packages
#'
#' @param df The dataframe of interest which includes the columns of interest
#' @param items A list of vectors of colnames() each named in the pattern scalename.items
#'
#' @param missings.threshold the proportion of missings per row and item package. CAVE! missings means values < 0 only. Default: no value is being calculated if > 10% of values are missing in an item package. In that case, NA is displayed
#'
#'
#' @description
#' Function calculates the mean of square root differences of all pairs of values in an item package per person (temp_index). Their theoretical range is 0-1, depending on the given data the observed maximum can be lower than 1. For this temp_index, a 0 represents absolute nondiffentiation, meaning that all obsverved values are equal. The temporary indices are standardized then, which means that they are extended to a range of 0-1. For the final_index, 0 means zero nondifferentiation whereas 1 represents the dataset's maximum of nondifferentiation. The procedure is based on the description of Kim et al., 2019, some adjustments were made. The function is applicable to a list of several item packages. The item packs should be named in the following form: scalename.items. The function automatically sets the value of a respondent NA if more than 10% of the items' values are < 0. The proportion is adjustable. The function does not have an explicite rule included for dealing with NAs, yet. I recommend to set NA's < 0. Takes a good while due to included for-loop. Prints durations per item package to the console.
#'
#'
#'
#' @return A dataframe will be printed to the console
#' @export
#'
#' @author Julia Witton
#' @references Kim, Y., Dykema, J., Stevenson, J., Black, P., & Moberg, D. P. (2019). Straightlining: Overview of Measurement, Comparison of Indicators, and Effects in Mail–Web Mixed-Mode Surveys. Social Science Computer Review, 37(2), 214–233. https://doi.org/10.1177/0894439317752406
#'
#' @examples
#' require(data.table) #generation of example data
#' require(dplyr)
#'
#' #example data
#' set.seed(1313)
#' df <- data.table(
#'   first01 = c(4, -2, 4, 5, 6, 2, -1, -1, -9, 4, 5, 1, 3, 4, 3, 4, 5,
#'    -1, -1, 5, 2, 3, 4, 3, 2, 2, -1, -9, 2, 1),
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
#'   #select item packages of interest
#'   items_first <- df %>% select(contains("first")) %>% colnames()
#'   items_second <- df %>% select(contains("second")) %>% colnames()
#'   items_third <- df %>% select(contains("third")) %>% colnames()
#'
#'   # aggregate item packages of interest in a list
#'   items_list <- list(items_first, items_second, items_third)
#'   names(items_list) <- c("items_first", "items_second", "items_third")
#'   names_list <- c("firstND", "secondND", "thirdND")
#'
#'   #apply function
#'
#'
#'   df %>% meanRootPairs(items_list, 0.1)
#'
#'
#'
#'
#'
#'
#'
#'


meanRootPairs <- function(df, items, missings.threshold = 0.10) {


  fun_meanRootPairs <- function(df, items, missings.threshold) {


    if (!requireNamespace("dplyr", quietly = TRUE)) {
      install.packages("dplyr")
    }
    library(dplyr)

    item_name <- strsplit(items, "\\.")[[1]][1]
    temp_index_name <- paste0("temp_index_", item_name)
    final_index_name <- paste0("final_index_", item_name)


    df %>%
      rowwise() %>%
      mutate(
        !!temp_index_name := case_when(

          (sum(c_across(all_of(items)) < 0) / length(c_across(all_of(items)))) > missings.threshold ~ NA,

          TRUE ~ {
            positive_items <- c_across(all_of(items))[c_across(all_of(items)) >= 0]

            if (length(positive_items) >= 2) {
              sqrt(sum(abs(combn(positive_items, 2, diff)))) / choose(length(positive_items), 2)

            } else {
              NA
            }

          }

        ),

        !!final_index_name := case_when(is.na(.data[[temp_index_name]]) ~ NA,
                                        TRUE ~ (.data[[temp_index_name]] - 1) / (0 - 1))

      )
  }

  t1 <- Sys.time()


  for (i in seq_along(items)){
    x <- Sys.time()
    df <- fun_meanRootPairs(df, items[[i]], missings.threshold )
    print(paste("Duration for", names(items)[i], ":", time_length(interval(x, Sys.time()), "minute"), "minutes"))
  }

  print(paste("Total duration for all item packs:", time_length(interval(t1, Sys.time()), "minute"), "minutes"))

  return(df)
}





