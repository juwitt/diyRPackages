
#' Calculation of nondifferentiation over multiple item packages
#'
#' @param df The dataframe of interest which includes the columns of interest
#' @param items A list of vectors of colnames()
#' @param new_var_name A vector of new names for the variables to be generated
#' @param proportion_allowed_missings the proportion of missings per row and item package. CAVE! missings means values < 0 only. Default: no value is being calculated if > 10% of values are missing in an item package. In that case, NA is displayed
#'
#'
#' @description
#' Function calculates the mean of square root differences of all pairs of values in an item package per person. This is then standardized using the minimum and maximum values in the dataset.The values range from 0 (least differentiation) to 1 (maximum differentiation). 0 in many cases means, the row contains the same value for all items of the considered item package. The procedure follows the description of Kim et al., 2019. The function is applicable to a list of several item packages. A vector of names needs to be defined. The function automatically sets the value of a respondent NA if more than 10% of the items' values are < 0. The proportion is adjustable. The function does not have a rule included for dealing with NAs, yet. Takes a good while due to included for-loop. Prints durations per item package into the console.
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
#'   calc_nondiff(df, items_list, names_list, 0.1)
#'
#'
#'
#'
#'
#'
#'
#'


calc_nondiff <- function(df, items = items_list, new_var_name = names_list, proportion_allowed_missings = 0.1) {

  fun_nondiff <- function(df, items, new_var_name, proportion_allowed_missings) {

    if (!requireNamespace("dplyr", quietly = TRUE)) {
      install.packages("dplyr")
    }
    library(dplyr)

    # Converts the string into a symbol that can be used in dplyr functions
    new_var_name <- rlang::sym(new_var_name)

    df <- df %>%
      rowwise() %>%

      # Create a new column with the name stored in new_var_name
      mutate(!!new_var_name := case_when(

        # If more than 10% of the values are less than 0, set the value to NA
        sum(c_across(all_of(items)) < 0) / length(c_across(all_of(items))) >= proportion_allowed_missings ~ NA_real_,
        TRUE ~ {

          # ...create a vector with all positive values
          positive_values <- c_across(all_of(items))[c_across(all_of(items)) > 0]

          # Drop combinations of 2 that contain a negative value
          if (length(positive_values) < 2) NA_real_ else {

            # Otherwise, calculate the square root of the absolute difference of all combinations of two positive values
            diff_values <- combn(positive_values, 2, function(x) sqrt(abs(diff(x))))

            # Calculate the average of these values
            mean(diff_values, na.rm = TRUE)
          }
        })) %>%
      ungroup() %>%

      # Create a new column for the maximum of new_var_name
      mutate(!!paste0(new_var_name, "max") := max(!!new_var_name, na.rm = TRUE),
             # Create a new column for the minimum of new_var_name
             !!paste0(new_var_name, "min") := min(!!new_var_name, na.rm = TRUE)) %>%
      rowwise() %>%

      # Calculate the new value for new_var_name
      mutate(!!new_var_name := (!!new_var_name - !!rlang::sym(paste0(new_var_name, "max")))/
               (!!rlang::sym(paste0(new_var_name, "min")) - !!rlang::sym(paste0(new_var_name, "max"))))


  }

  x <- Sys.time()

  for (i in seq_along(items)) {
    starttime <- Sys.time()
    df <- fun_nondiff(df, items[[i]], new_var_name[i], proportion_allowed_missings)
    print(paste("Duration for", names(items)[i] , ":", Sys.time() - starttime))
  }

  print(paste("Total duration:", Sys.time()-x))

  return(df)
}



