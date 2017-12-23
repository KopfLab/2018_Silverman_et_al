# constantes and templates ====

# start random seed
set.seed(42) 

# color-blind palette (source: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#F0E442", "#999999")

# figure themes
theme_figure <- function(legend = TRUE, grid = TRUE, text_size = 20, axis_text_size = NULL) {
  the_theme <- theme_bw() + 
    theme(text = element_text(size = text_size),
          plot.background = element_blank(), panel.background = element_blank(),
          panel.border = element_rect(color="black", size=1), 
          strip.background = element_rect(color="black", linetype = 1),
          plot.margin = unit(c(0.1, 0.3, 0.1, 0.1), "cm")
    )
  # adjust grid
  if(!grid)
    the_theme <- the_theme + theme(panel.grid = element_blank())
  else
    the_theme <- the_theme + theme(panel.grid.minor = element_blank())
  # adjust legend
  if (!legend)
    the_theme <- the_theme + theme(legend.position = "none")
  # overwrite axis text size if provided
  if (!is.null(axis_text_size))
    the_theme <- the_theme + 
      theme(axis.text = element_text(size = axis_text_size)) 
  return(the_theme)
}


# functions to simplify plotting ====


# functions for data tables ====

# finds the max number of decimal places
# @param max_sig_digits
get_n_decimals <- function(x, max_sig_digits = NULL) {
  stopifnot(is.numeric(x))
  
  # check if there's max signification digits
  if (!is.null(max_sig_digits)) 
    .x = signif(x, max_sig_digits)
  else
    .x <- x
  
  # calculate number of decimal places
  n_deci <- .x %>% as.character() %>% stringr::str_match("^[0-9]*\\.([0-9]*)0*$") %>% { nchar(.[,2]) }
  n_main <- .x %>% as.character() %>% stringr::str_match("^[1-9]([0-9]?[1-9])*(0*)$") %>% { nchar(.[,3]) }
  n_deci <- ifelse(!is.na(n_deci), n_deci, -n_main)
  
  # evaluate max sig. digits scenario where last digit is 0 (is it significant or not?)
  if (!is.null(max_sig_digits)) {
    n_actual_sigs = .x %>% as.character() %>% 
      stringr::str_replace("\\.", "") %>% 
      stringr::str_match("^0*([1-9]([0-9]?[1-9])*)[0]*$") %>% { nchar(.[,2]) }
    n_deci <- ifelse( abs(x - .x) > 0 & n_actual_sigs < max_sig_digits, n_deci + 1L, n_deci)
  }
  return(n_deci)
}

# format to the number of significant digits given by the error (pads data correctly)
table_format_to_err <- function(x, err, sig_digits, max_sig_digits = NULL) {
  stopifnot(length(x) == length(err))
  n_decs <- get_n_decimals(err, sig_digits)
  if (!is.null(max_sig_digits))
    n_decs <- ifelse(n_decs > max_sig_digits, max_sig_digits, n_decs)
  sprintf("%%.%df", n_decs) %>% sprintf(round(x, n_decs))
}

# format with the error and rounded to the significant digits of the error (pads data correctly)
table_format_with_err <- function(x, err, sig_digits, max_sig_digits = NULL) {
  stopifnot(length(x) == length(err))
  n_decs <- get_n_decimals(err, sig_digits)
  if (!is.null(max_sig_digits))
    n_decs <- ifelse(n_decs > max_sig_digits, max_sig_digits, n_decs)
  sprintf("%%.%df ± %%.%df", n_decs, n_decs) %>% sprintf(round(x, n_decs), round(err, n_decs))
}

# format error alone
table_format_err <- function(err, sig_digits, max_sig_digits = NULL) {
  n_decs <- get_n_decimals(err, sig_digits)
  if (!is.null(max_sig_digits))
    n_decs <- ifelse(n_decs > max_sig_digits, max_sig_digits, n_decs)
  sprintf("± %%.%df", n_decs) %>% sprintf(round(err, n_decs))
}

# round to a specific number of digits
table_round <- function(x, n_decs) {
  sprintf("%%.%df", n_decs) %>% sprintf(round(x, n_decs))
}

# round to a specific number of digits
table_round_with_err <- function(x, err, n_decs) {
  sprintf("%%.%df ± %%.%df", n_decs, n_decs) %>% sprintf(round(x, n_decs), round(err, n_decs))
}

# export data table
export_data_table <- function(x, filename) {
  if(!dir.exists("tables")) dir.create("tables")
  write.xlsx(x, file.path("tables", filename))
}
