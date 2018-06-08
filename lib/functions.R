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


# functions for carbonate chemistry calculations ====

# constants
R_ideal <- 0.083144598 # [L bar K-1 mol-1]

#' Calculate Henry's law constant KH by scaling henry's constant by arrhenius
#' 
#' Source: Sander, R.: Compilation of Henry's law constants (version 4.0) for water as solvent, Atmos. Chem. Phys., 15, 4399-4981, https://doi.org/10.5194/acp-15-4399-2015, 2015.
#' 
#' @param temp.C temperature (in degrees Celsius)
#' @return Henry's law constant for the selected gas at the provided temperature, in M/atm
calc_KH <- function(gas, temp.C) {
  constants <- 
    tribble(
      ~name,   ~`dH/d(1/T)`, ~T0,     ~H0,       
      # name, slope         K,       M/bar (converted from mol/m3 Pa)
      "CO2",  2400,         298.15,  3.3 * 10^-4 * 100,
      "O2",   1500,         298.15,  1.3 * 10^-5 * 100
    )
  
  gas_constants <- filter(constants, name == gas)
  
  if (nrow(gas_constants) == 0) 
    stop("no constants stored for gas ", gas, call. = FALSE)
  else if (nrow(gas_constants) > 1)
    stop("more than one set of constants for gas ", gas, call. = FALSE)
  
  KH <- with(gas_constants, H0 * exp(`dH/d(1/T)` * (1/convert_to_temp_in_K(temp.C) - 1/T0) ))
  return(KH)
}

#' Convert temperature in celsius to temperature in Kelvin
#' @return temperature in Kelvin
convert_to_temp_in_K <- function(temp.C) {
  return(273.15 + temp.C)
}

#' function to calculate DIC (in M) based on pH and pCO2 (in bar) or H2CO3* (M)
#' @inheritParams calc_open_system_pH
#' @return DIC in mol/L
calc_DIC_conc <- function(
  pH, pCO2.bar, `H2CO3*.M` = KH * pCO2.bar, temp.C = 25, 
  KH = calc_KH("CO2", temp.C), pK1 = 6.3, pK2 = 10.3) {
  `H2CO3*.M` * (1 + 10^(pH - pK1) + 10^(2*pH - pK1 - pK2))
}

#' calculate total inorganic carbon in the liquid phase (in mol)
#' @inheritParams calc_closed_system_pH
#' @param ... passed on to \link{calc_DIC_conc}
#' @return DIC in mol
calc_DIC_amount <- function(
  pH, pCO2.bar, `H2CO3*.M` = KH * pCO2.bar, Vl.L,
  temp.C = 25, KH = calc_KH("CO2", temp.C), ...) {
  calc_DIC_conc(pH = pH, `H2CO3*.M` = `H2CO3*.M`, temp.C = temp.C, ...) * Vl.L
}

#' function to calculate the gas phase CO2 equivalent concentration (in M) (i.e. mol of CO2 / L of volume)
#' assumes ideal gas law applies to CO2
#' @inheritParams calc_open_system_pH
#' @return CO2(g) concentration in mol CO2 / L headspace
calc_CO2g_conc <- function(pCO2.bar, temp.C = 25) {
  pCO2.bar / (R_ideal * convert_to_temp_in_K(temp.C))
}

#' calculate total inorganic carbon in the gas phase (in mol)
#' @inheritParams calc_closed_system_pH
#' @return amount of CO2 in the given volume, in mol
calc_CO2g_amount <- function(pCO2.bar, Vg.L, temp.C = 25) {
  calc_CO2g_conc(pCO2.bar = pCO2.bar, temp.C = temp.C) * Vg.L
}

#' calculate total inorganic carbon in a closed system (in mol)
#' @inheritParams calc_closed_system_pH
#' @return total amount of inorganic carbon in the closed system, in mol
calc_CIT_amount <- function(pH, pCO2.bar, Vg.L, Vl.L, temp.C = 25) {
  calc_CO2g_amount(pCO2.bar = pCO2.bar, Vg.L = Vg.L, temp.C = temp.C) + 
    calc_DIC_amount(pH = pH, pCO2.bar = pCO2.bar, Vl.L = Vl.L, temp.C = temp.C)
}

#' calculate pCO2 in a closed system
#' @inheritParams calc_closed_system_pH
#' @return pCO2 (in bar) in the closed system
calc_closed_system_pCO2 <- function(
  pH, CIT.mol, Vl.L, Vg.L, temp.C = 25, 
  KH = calc_KH("CO2", temp.C), pK1 = 6.3, pK2 = 10.3) {
  
  CIT.mol / 
    ( Vg.L / (R_ideal * convert_to_temp_in_K(temp.C)) + 
       Vl.L * KH * (1 + 10^(pH - pK1) + 10^(2*pH - pK1 - pK2)) )
}

#' calculate O2 in a closed system
#' @param O2_total.mol total mole of O2 in the system (both dissolved and gaseous)
#' @inheritParams calc_closed_system_pH
#' @return pO2 (in bar) in a closed system
calc_closed_system_pO2 <- function(O2_total.mol, Vl.L, Vg.L, temp.C = 25) {
  O2_total.mol / 
    (Vg.L / (R_ideal * convert_to_temp_in_K(temp.C)) + calc_KH("O2", temp.C) * Vl.L)
}

#' Calculate pH in open system (i.e. unlimited pCO2)
#' @param pCO2.bar pressure of CO2 in bar
#' @param `H2CO3*.M` concentration of dissolved CO2 + carbonic acid (H2CO3*), calculated from pCO2 unless explicitly provided
#' @param buffer.M [optional] total buffer concentration in mol/L: a protonated buffer, make sure to add same amount to hard_ions IF a buffer salt is used (e.g. Na-buffer), and provide the appropriate pKa.
#' @param pKa buffer acid dissociation constant, required if \code{buffer} is provided
#' @param unbalanced_ions.M [optional] concentration of all unbalanced ions [units charge x mol/L]. That means all charge multiplied cations from hard bases and included soft bases (carbonate and the specific buffer) e.g. mol/L Na that was added in the form of NaOH, NaHCO3 or as part of a Na-buffer salt. Also all charge multiplied (and -) anions from hard acids, e.g. -1 x mol/L Cl that was added as HCl or -2 x mol/L SO4 that was added as H2SO4. 
#' @param temp.C temperature in C
#' @param pKs 
calc_open_system_pH <- function(
  pCO2.bar, `H2CO3*.M` = KH * pCO2.bar, 
  buffer.M = 0, pKa = 7.5, unbalanced_ions.M = 0, temp.C = 25, 
  KH = calc_KH("CO2", temp.C), pK1 = 6.3, pK2 = 10.3, pKw = 14){
  
  if (missing(pCO2.bar) && missing(`H2CO3*.M`)) 
    stop("either pCO2 or H2CO3* must be provided", call. = FALSE)
  if (!missing(pCO2.bar) && !missing(`H2CO3*.M`))
    warning("H2CO3* provided, pCO2 parameter will be ignored", call. = FALSE, immediate. = TRUE)
  if (!missing(buffer.M) && missing(pKa))
    stop("must provide a pKa if buffer is provided", call. = FALSE)
  
  result <- 
    data_frame(`H2CO3*.M`, buffer.M, pKa, unbalanced_ions.M, temp.C, pK1, pK2, pKw) %>% 
    mutate(
      pH = pmap_dbl(
        list(`H2CO3*.M`, buffer.M, pKa, unbalanced_ions.M, temp.C, pK1, pK2, pKw), 
        function(`H2CO3*.M`, buffer.M, pKa, unbalanced_ions.M, temp.C, pK1, pK2, pKw) {
          calc_root <- function(pH) {
            unbalanced_ions.M - calc_open_system_unbalanced_ions(
              pH = pH, `H2CO3*.M` = `H2CO3*.M`, buffer.M = buffer.M, 
              pKa = pKa, temp.C = temp.C, pK1 = pK1, pK2 = pK2, pKw = pKw)
          }
          uniroot(calc_root, c(0, 14))$root
        })
    )
  result$pH
}

#' helper function for open system pH calculations
#' calculate the net unbalanced ions (e.g. Na added as NaOH or Na-Cl added as NaOH and HCl respectively)
#' @param pH the pH of the system
#' @inheritParams calc_open_system_pH
#' @return unbalanced ion concentration in mol/L (M)
calc_open_system_unbalanced_ions <- function(
  pH, pCO2.bar, `H2CO3*.M` = KH * pCO2.bar, 
  buffer.M = 0, pKa = 7.5, temp.C = 25, 
  KH = calc_KH("CO2", temp.C), pK1 = 6.3, pK2 = 10.3, pKw = 14) {
  - 10^(-pH) + 
    1/(1 + 10^(pKa - pH)) * buffer.M + 
    (10^(pH - pK1) + 2 * 10^(2*pH - pK1 - pK2)) * `H2CO3*.M` +
    10^(pH-pKw)
}

#' Calculate pH in open system (i.e. unlimited pCO2)
#' @param CIT.mol total inorganic carbon in system (mol)
#' @param Vl.L volume of liquid in L
#' @param Vg.L volume of gas/headspace in L
#' @inheritParams calc_open_system_pH
#' @param pKs 
calc_closed_system_pH <- function(
  CIT.mol, Vl.L, Vg.L, 
  buffer.M = 0, pKa = 7.5, unbalanced_ions.M = 0, temp.C = 25, pK1 = 6.3, pK2 = 10.3, pKw = 14){
  
  if (!missing(buffer.M) && missing(pKa))
    stop("must provide a pKa if buffer is provided", call. = FALSE)
  
  result <- 
    data_frame(CIT.mol, Vl.L, Vg.L, buffer.M, pKa, unbalanced_ions.M, temp.C, pK1, pK2, pKw) %>% 
    mutate(
      pH = pmap_dbl(
        list(CIT.mol, Vl.L, Vg.L, buffer.M, pKa, unbalanced_ions.M, temp.C, pK1, pK2, pKw), 
        function(CIT.mol, Vl.L, Vg.L, buffer.M, pKa, unbalanced_ions.M, temp.C, pK1, pK2, pKw) {
          calc_root <- function(pH) {
            unbalanced_ions.M - calc_closed_system_unbalanced_ions(
              pH = pH, CIT.mol = CIT.mol, Vl.L = Vl.L, Vg.L = Vg.L, 
              buffer.M = buffer.M, pKa = pKa, temp.C = temp.C, 
              pK1 = pK1, pK2 = pK2, pKw = pKw)
          }
          uniroot(calc_root, c(0, 14))$root
        })
    )
  result$pH
}

#' helper function for closed system pH calculations
#' calculate the net unbalanced ions (e.g. Na added as NaOH or Na-Cl added as NaOH and HCl respectively) considering a fixed amount of inorganic carbon
#' @param pH the pH of the system
#' @inheritParams calc_open_system_pH
#' @return unbalanced ions in mol/L (M)
calc_closed_system_unbalanced_ions <- function(
  pH, CIT.mol, Vl.L, Vg.L, 
  buffer.M = 0, pKa = 7, temp.C = 25, 
  KH = calc_KH("CO2", temp.C),
  pK1 = 6.3, pK2 = 10.3, pKw = 14) {
  
  - 10^(-pH) + 
    1/(1 + 10^(pKa - pH)) * buffer.M +
    (
      (10^(pH-pK1) + 2*10^(2*pH-pK1-pK2)) / 
        ( Vg.L / (KH * R_ideal * convert_to_temp_in_K(temp.C)) + (1 + 10^(pH-pK1) + 10^(2*pH-pK1-pK2)) * Vl.L)
    ) * CIT.mol +
    10^(pH-pKw)
}

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
