
#' Calculation of the mean root of squares of items over multiple item packages
#'
#' @param df The dataframe of interest which includes the columns of interest
#' @param items A list of vectors of colnames() each named in the pattern scalename.items
#'
#' @param missings.threshold the proportion of missings per row and item package. CAVE! missings means values < 0 only. Default: no value is being calculated if > 10% of values are missing in an item package. In that case, NA is displayed
#'
#'
#' @description
#' Function calculates the mean of the root of differences of all pairs of values in an item package (rowwise, resulting in `temp_index`). Their theoretical range is 0-1, depending on the given data, the observed maximum can be lower than 1.
#' For temp_index, a 0 represents absolute nondiffentiation, meaning that all obsverved values are equal.
#' The temporary indices are standardized in the next step which means that they are extended to a range of 0-1 using the observed maximum of temp_index. For the final_index, 0 means zero nondifferentiation whereas 1 represents the maximum of nondifferentiation. The resulting value is the ratio of the individual variation in comparison to the observed maximum variation.
#' The procedure is based on the description of Mulligan et al., 2001, described by Kim et al., 2019, Chang & Krosnick, 2009, and others.
#' One major adjustments was made compared to the formula published by Kim et al. and Chang & Krosnick, 2009: Only one square root is calculated for the sum of all differences between pairs. Otherwise, the temp_indices exceed the substantially interpretable range of 0-1.
#' The function is applicable to a list of vectors containing item packages. The item packs should be named in the following form: scalename.items. The function automatically sets the temp_index of a respondent NA if more than 10% of the items' values are < 0 (thereby indicating a missing). The proportion is adjustable. The function does not have an explicite rule included for dealing with NAs, yet. I recommend to set NA's < 0.
#' For acceleration of the computation, all columns of interest are being tranformed to numerics before the calculation starts (i.e. columns are being unlabelled).
#'
#'
#'
#' @return A dataframe will be printed to the console. Durations per item package as well as the total duration will be printed to the console.
#' @export
#'
#' @author Julia Witton
#' @references
#' Mulligan, K., Krosnick, J. A., Smith, W., Green, M., & Bizer, G. (2001). Nondifferentiation on Attitude Rating Scales: A Test of Survey Satisflcing Theory Manuscript under review.
#' Kim, Y., Dykema, J., Stevenson, J., Black, P., & Moberg, D. P. (2019). Straightlining: Overview of Measurement, Comparison of Indicators, and Effects in Mail–Web Mixed-Mode Surveys. Social Science Computer Review, 37(2), 214–233. https://doi.org/10.1177/0894439317752406.
#' Chang, L., & Krosnick, J. A. (2009). National Surveys Via Rdd Telephone Interviewing Versus the Internet. Public Opinion Quarterly, 73(4), 641–678. https://doi.org/10.1093/poq/nfp075
#' Fricker, S. (2005). An Experimental Comparison of Web and Telephone Surveys. Public Opinion Quarterly, 69(3), 370–392. https://doi.org/10.1093/poq/nfi027
#'
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


    # in case data is labelled, the transformation to numeric saves a lot of time
    vector_list <- items
    all_item_names <- unlist(vector_list)
    df <-
      df %>% mutate_at(vars(colnames(df)[colnames(df) %in% all_item_names]), as.numeric)

    # get the name of the itempackage: first element of the vector in the list (only strings, no numbers)
    item_name <- gsub("\\d", "", items[[1]][1])

    # introduce and define all names of new variables
    temp_index_name <- paste0("temp_index_", item_name)
    temp_index_max <- paste0(item_name, "_temp_max")
    final_index_name <- paste0("final_index_", item_name)


    df %>%
      rowwise() %>%
      mutate(
        !!temp_index_name := case_when(

          # if proportion of missings in data is higher than threshold - set NA
          (sum(c_across(all_of(items)) < 0) / length(c_across(all_of(items)))) > missings.threshold ~ NA,

          # if not...
          TRUE ~ {

            # take all positive values in the item package of the observation
            positive_items <- c_across(all_of(items))[c_across(all_of(items)) >= 0]

            # if there are at least 2 values (1 pair), we can calculate at least 1 difference
            if (length(positive_items) >= 2) {

              # calculate the sqrt of the sum of all differences. Divide this then by number of differences (pairs) to calculate the mean.
              sqrt(sum(abs(combn(positive_items, 2, diff)))) / choose(length(positive_items), 2)

              # in case there are no pairs
            } else {
              # set temp_index for this obs. NA
              NA
            }

          }

        )) %>%

      ungroup() %>%

      # Create a new column for the maximum of temp_index_name
      mutate(!!temp_index_max := max(!!rlang::parse_expr(temp_index_name), na.rm = TRUE)) %>%

      # Go back rowwise
      rowwise() %>%

      # Calculate the new value for final_index_name

      mutate(
        # paste the temp_index_name and substract from it the pasted temp_index_max dividing this by
        !!final_index_name := ((!!rlang::parse_expr(temp_index_name) - !!rlang::parse_expr(temp_index_max))/
                                 # minus the pasted temp_index_maximum
                                 (0 - !!rlang::parse_expr(temp_index_max)))
      )


  }

  t1 <- Sys.time()


  for (i in seq_along(items)){
    x <- Sys.time()

    # apply the function to all elements of the given list
    df <- fun_meanRootPairs(df, items[[i]], missings.threshold )

    # send an information about the duration of the process to the console
    itempack_name <- gsub("\\d", "", items[[i]][1])  # Remove numbers from the name
    cat("Duration for", itempack_name, ":", time_length(interval(x, Sys.time()), "minute"), "minutes\n")

  }

  cat("All done!\nTotal duration for all item packs:", time_length(interval(t1, Sys.time()), "minute"), "minutes.")

  return(df)
}




