# 05. IMPACTS OF CHANGES IN FINAL DEMAND ----
# Reads the scenarios (data/input/scenarios.xlsx), splits each change in final demand into domestic
# output and imports, and computes the effects with the Leontief inverse of the domestic symmetric
# table (settings.yml, impact):
#   type I   dx = (I - A)^-1 dy                         direct and indirect effects
#   type II  households endogenous: compensation of employees per unit of output (row) and household
#            consumption of domestic products per unit of compensation (column)     + induced effects
# Effects on output, gross value added, compensation, employment, net taxes on products and imports.
# Output: Impacts_<source>.xlsx (summary, effects by product for each scenario, multipliers, notes).

if (!exists("HELPERS_LOADED")) source(file.path(MODULE, "R", "helpers.R"))
if (!exists("SIOT")) source(file.path(MODULE, "R", "04_siot.R"))
start_step(5, "impacts of changes in final demand")

IMP <- S$impact; src <- S$analysis_source
r <- SIOT[[paste(src, IMP$model)]]
stop_if(is.null(r), "No symmetric table for ", src, " model ", IMP$model)
n <- length(r$x); codes <- r$labels
coef <- function(v) ifelse(r$x == 0, 0, v / r$x)
A  <- sweep(r$Z, 2, ifelse(r$x == 0, 0, r$x), function(z, x) ifelse(x == 0, 0, z / x))
L  <- solve(diag(n) - A)
cf <- list(gva = coef(r$W["B1G", ]), comp = coef(r$W["D1", ]), taxes = coef(r$W["D21_M_D31", ]),
           imports = coef(r$W["IMPORTS", ]), emp = if (!is.null(r$E)) coef(r$E["EMP", ]) else rep(NA, n))
# type II: close the model for households
hh <- r$Y[, "P3_S14"] / sum(r$W["D1", ])
L2 <- solve(diag(n + 1) - rbind(cbind(A, hh), c(cf$comp, 0)))[1:n, 1:n]

# scenarios ----
f <- file.path(IN, IMP$scenarios_file)
stop_if(!file.exists(f), "Missing scenarios file: ", f, " (Rscript R/00_make_scenarios.R)")
sc <- read.xlsx(f, sheet = "final_demand", sep.names = " ")
unknown <- setdiff(sc$code[rowSums(abs(sc[, -(1:2), drop = FALSE]), na.rm = TRUE) > 0], codes)
stop_if(length(unknown) > 0, "Scenario codes not in the tables: ", paste(unknown, collapse = ", "))
use_tot <- LONG %>% filter(source == src, table == "T1610", induse == IMP$import_split_column)
use_dom <- LONG %>% filter(source == src, table == "T1611", induse == IMP$import_split_column)
dom_share <- use_dom$value[match(codes, use_dom$prod_na)] / use_tot$value[match(codes, use_tot$prod_na)]
dom_share[!is.finite(dom_share)] <- 1

SUMMARY <- list(); BY_PRODUCT <- list()
for (s in names(sc)[-(1:2)]) {
  dy <- coalesce(sc[[s]][match(codes, sc$code)], 0)
  dyd <- dy * dom_share
  x1 <- as.vector(L %*% dyd); x2 <- as.vector(L2 %*% dyd)
  eff <- function(x) c(output = sum(x), gva = sum(cf$gva * x), compensation = sum(cf$comp * x),
                       employment = sum(cf$emp * x), net_taxes_on_products = sum(cf$taxes * x),
                       imports_intermediate = sum(cf$imports * x))
  e1 <- eff(x1); e2 <- eff(x2)
  SUMMARY[[s]] <- tibble(scenario = s, final_demand = sum(dy), of_which_domestic = sum(dyd), direct_imports = sum(dy - dyd),
                         !!!setNames(as.list(e1), paste0(names(e1), "_type1")),
                         !!!setNames(as.list(e2), paste0(names(e2), "_type2")),
                         output_multiplier_type1 = e1["output"] / sum(dyd), output_multiplier_type2 = e2["output"] / sum(dyd))
  BY_PRODUCT[[s]] <- tibble(code = codes, final_demand = dy, domestic = dyd, output_type1 = x1, gva_type1 = cf$gva * x1,
                            employment_type1 = cf$emp * x1, imports_type1 = cf$imports * x1, output_type2 = x2, gva_type2 = cf$gva * x2,
                            employment_type2 = cf$emp * x2)
}
SUMMARY <- bind_rows(SUMMARY)
MULT <- tibble(code = codes, output_type1 = colSums(L), output_type2 = colSums(L2),
               gva_type1 = as.vector(cf$gva %*% L), gva_type2 = as.vector(cf$gva %*% L2),
               employment_per_mil_type1 = 1000 * as.vector(cf$emp %*% L), employment_per_mil_type2 = 1000 * as.vector(cf$emp %*% L2),
               imports_type1 = as.vector(cf$imports %*% L))
NOTES <- tibble(note = c(
  paste0("Tables: ", LAY[[src]]$publisher, ", ", LAY[[src]]$time_period, " (", LAY[[src]]$url, ")."),
  paste0("Symmetric table: domestic, product x product, model ", IMP$model, " (Eurostat Manual of Supply, Use and Input-Output Tables, 2008)."),
  paste0("Final demand changes in thousand HRK at basic prices; import shares from column ", IMP$import_split_column, " of the use tables."),
  "Type I: direct and indirect effects. Type II: plus induced effects of household consumption out of compensation of employees.",
  "Employment: persons (row EMP of the source tables); employment multipliers per million HRK of final demand.",
  "Scenario vectors in data/input/scenarios.xlsx are illustrative."))
sheets <- c(list(Summary = SUMMARY), setNames(BY_PRODUCT, substr(names(BY_PRODUCT), 1, 31)),
            list(Multipliers = MULT, Notes = NOTES))
write_sheets(sheets, file.path(OUT, paste0("Impacts_", src, ".xlsx")))
print(as.data.frame(SUMMARY %>% select(scenario, final_demand, of_which_domestic, output_type1, gva_type1, employment_type1, output_type2, output_multiplier_type1)))
