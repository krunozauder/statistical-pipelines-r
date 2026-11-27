# 03. SDMX OUTPUT ----
# The long table is validated against the data structure (config/dsd.yml) and code lists
# (config/codelists, CBS labels plus documented additions) and written as SDMX-CSV and SDMX-JSON (all dimensions at observation level),
# one file per source. The checks workbook collects the read log, the accounting checks of step 02
# and the validation.

if (!exists("HELPERS_LOADED")) source(file.path(MODULE, "R", "helpers.R"))
start_step(3, "SDMX output and validation")

DSD <- read_yml(file.path(MODULE, "config", "dsd.yml"))
CL  <- lapply(setNames(nm = c("table_identifier", "stk_flow", "prod_na", "induse")),
              function(k) read.csv(file.path(MODULE, "config", "codelists", paste0(k, ".csv")), stringsAsFactors = FALSE))
CL$prod_na <- bind_rows(CL$prod_na, read.csv(file.path(MODULE, "config", "codelists", "prod_na_additions.csv"), stringsAsFactors = FALSE))
DIR_S <- file.path(OUT, "sdmx"); dir.create(DIR_S, showWarnings = FALSE)
FLOW  <- with(DSD$dataflow, paste0(agency, ":", id, "(", version, ")"))

to_sdmx <- function(d) {
  d %>% transmute(DATAFLOW = FLOW, TABLE_IDENTIFIER = table, STK_FLOW = stk_flow, PROD_NA = prod_na, INDUSE = induse,
                  REF_AREA = geo, TIME_PERIOD = time_period, OBS_VALUE = value,
                  UNIT_MEASURE = sub("^THS_", "", unit), UNIT_MULT = ifelse(grepl("^THS_", unit), "3", "0"))
}

# validation ----
VALID <- list()
for (src in unlist(S$sources)) {
  d <- to_sdmx(filter(LONG, source == src))
  for (dm in DSD$dimensions) {
    if (is.null(dm$codelist)) next
    bad <- setdiff(unique(d[[dm$id]]), CL[[dm$codelist]]$code)
    VALID[[length(VALID) + 1]] <- tibble(source = src, check = paste(dm$id, "codes in code list", dm$codelist),
                                         problems = length(bad), detail = paste(head(bad, 15), collapse = ", "))
  }
  key <- d %>% select(TABLE_IDENTIFIER:TIME_PERIOD)
  VALID[[length(VALID) + 1]] <- tibble(source = src, check = "Series keys are unique", problems = sum(duplicated(key)), detail = "")
  VALID[[length(VALID) + 1]] <- tibble(source = src, check = "OBS_VALUE is numeric and finite", problems = sum(!is.finite(d$OBS_VALUE)), detail = "")
  # SDMX-CSV ----
  write.csv(d, file.path(DIR_S, paste0(src, ".csv")), row.names = FALSE)
  # SDMX-JSON, all dimensions at observation level ----
  dims <- sapply(DSD$dimensions, `[[`, "id")
  values <- lapply(setNames(dims, dims), function(k) sort(unique(d[[k]])))
  idx <- sapply(dims, function(k) match(d[[k]], values[[k]]) - 1)
  lab <- function(k, v) {
    cl <- Filter(function(z) z$id == k, DSD$dimensions)[[1]]$codelist
    if (is.null(cl)) return(v)
    l <- CL[[cl]]$label[match(v, CL[[cl]]$code)]; ifelse(is.na(l), v, l)
  }
  units <- unique(d$UNIT_MEASURE); mults <- unique(d$UNIT_MULT)
  obs <- lapply(seq_len(nrow(d)), function(i) list(d$OBS_VALUE[i], match(d$UNIT_MEASURE[i], units) - 1, match(d$UNIT_MULT[i], mults) - 1))
  names(obs) <- apply(idx, 1, paste, collapse = ":")
  msg <- list(
    meta = list(schema = "https://raw.githubusercontent.com/sdmx-twg/sdmx-json/master/data-message/tools/schemas/2.0.0/sdmx-json-data-schema.json",
                id = paste0(DSD$dataflow$id, "_", src), prepared = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                sender = list(id = DSD$dataflow$agency), contentLanguages = list("en")),
    data = list(
      structures = list(list(
        name = DSD$dataflow$name,
        dimensions = list(dataSet = list(), series = list(),
                          observation = lapply(seq_along(dims), function(j) list(id = dims[j], keyPosition = j - 1,
                            values = lapply(values[[j]], function(v) list(id = v, name = lab(dims[j], v)))))),
        attributes = list(dataSet = list(), series = list(),
                          observation = list(list(id = "UNIT_MEASURE", values = lapply(units, function(v) list(id = v))),
                                             list(id = "UNIT_MULT", values = lapply(mults, function(v) list(id = v))))))),
      dataSets = list(list(action = "Information", structure = 0, observations = obs))))
  writeLines(jsonlite::toJSON(msg, auto_unbox = TRUE, digits = NA), file.path(DIR_S, paste0(src, ".json")), useBytes = TRUE)
}
VALID <- bind_rows(VALID) %>% mutate(status = ifelse(problems == 0, "OK", "CHECK"))

write_sheets(list(`Read log` = if (nrow(READ_LOG)) READ_LOG else tibble(note = "no layout issues"),
                  `Accounting checks` = CHECKS_SUT,
                  `Accounting differences` = CHK %>% filter(abs(diff) > S$tolerance$balance) %>% select(source, check, prod_na, induse, value_a, value_b, diff),
                  `SDMX validation` = VALID),
             file.path(OUT, "Checks_tables.xlsx"))
print(as.data.frame(VALID %>% filter(status != "OK")))
cat("SDMX-CSV and SDMX-JSON ->", DIR_S, "\n")
