# 04. SYMMETRIC INPUT-OUTPUT TABLES ----
# Domestic symmetric tables derived from the supply table (T1500) and the use table for domestic
# output (T1611), following the Eurostat Manual of Supply, Use and Input-Output Tables (2008):
#   B  product x product, industry technology:        Z = U g^-1 V,      primary inputs W g^-1 V
#   A  product x product, product technology:          Z = U (V')^-1 q,   primary inputs W (V')^-1 q
#   D  industry x industry, fixed product sales:       Z = V q^-1 U,      final use V q^-1 Y
# V = make matrix (industry x product), U = domestic intermediate use (product x industry),
# g / q = output by industry / product, W = imports and primary inputs by industry.
# Model A needs a square, invertible make matrix and can give negative flows; both are reported.
# For the CBS 2010 tables the result is compared with the published domestic table (T1800).

if (!exists("HELPERS_LOADED")) source(file.path(MODULE, "R", "helpers.R"))
start_step(4, "symmetric input-output tables")

mat <- function(d, rows, cols) {
  m <- matrix(0, length(rows), length(cols), dimnames = list(rows, cols))
  d <- d %>% filter(prod_na %in% rows, induse %in% cols)
  m[cbind(match(d$prod_na, rows), match(d$induse, cols))] <- d$value
  m
}
inv_diag <- function(x) diag(ifelse(x == 0, 0, 1 / x), length(x))

sut_parts <- function(src) {
  d <- LONG %>% filter(source == src)
  s <- d %>% filter(table == "T1500"); cols <- unique(s$induse)
  P <- setdiff(unique(s$prod_na[grepl("^CPA_", s$prod_na)]), "CPA_TOTAL")
  I <- cols[seq_len(match("TOTAL", cols) - 1)]
  stop_if(!setequal(sub("^CPA_", "", P), I), src, ": products and industries do not match one to one")
  I <- sub("^CPA_", "", P)                                               # same order for products and industries
  V <- t(mat(s, P, I))                                                    # industry x product
  U <- mat(filter(d, table == "T1611"), P, I)
  Y <- mat(filter(d, table == "T1611"), P, unlist(S$final_uses))
  W <- rbind(IMPORTS = colSums(mat(filter(d, table == "T1612"), P, I)),
             mat(filter(d, table == "T1610"), unlist(S$primary_inputs), I))
  emp <- d %>% filter(table == "T1610", prod_na == "EMP")
  E <- if (nrow(emp)) mat(emp, "EMP", I) else NULL
  list(P = P, I = I, V = V, U = U, Y = Y, W = W, E = E, g = colSums(t(V)), q = colSums(V))
}

siot <- function(p, model) {
  with(p, {
    if (model == "B") {
      T <- inv_diag(g) %*% V                                             # industry -> product
      list(Z = U %*% T, W = W %*% T, E = if (!is.null(E)) E %*% T, Y = Y, x = q, labels = P)
    } else if (model == "D") {
      T <- V %*% inv_diag(q)                                             # product -> industry
      list(Z = T %*% U, W = W, E = E, Y = T %*% Y, x = g, labels = I)
    } else if (model == "A") {
      k <- which(g != 0 & q != 0)                                       # zero rows/columns cannot be inverted
      Vt <- t(V)[k, k]
      if (abs(det(Vt / mean(Vt[Vt > 0]))) < 1e-12) return(NULL)
      T <- solve(Vt) %*% diag(q[k])
      list(Z = U[k, k] %*% T, W = W[, k] %*% T, E = if (!is.null(E)) E[, k, drop = FALSE] %*% T,
           Y = Y[k, ], x = q[k], labels = P[k])
    }
  })
}

SIOT <- list(); SIOT_CHECK <- list()
for (src in unique(c(S$analysis_source, unlist(S$sources)))) {
  p <- sut_parts(src)
  for (m in unlist(S$iot_models)) {
    r <- siot(p, m)
    if (is.null(r)) { SIOT_CHECK[[length(SIOT_CHECK) + 1]] <- tibble(source = src, model = m, check = "make matrix invertible", value = NA, status = "SKIPPED"); next }
    dimnames(r$Z) <- list(r$labels, r$labels); colnames(r$W) <- r$labels
    SIOT[[paste(src, m)]] <- c(r, source = src, model = m)
    row_bal <- max(abs(rowSums(r$Z) + rowSums(r$Y) - r$x))                # output = intermediate + final use
    col_bal <- max(abs(colSums(r$Z) + colSums(r$W[c("IMPORTS", "D21_M_D31", "B1G"), , drop = FALSE]) - r$x))  # output = inputs
    SIOT_CHECK[[length(SIOT_CHECK) + 1]] <- tibble(source = src, model = m,
      check = c("rows: intermediate + final use = output (max abs. difference)",
                "columns: intermediate + imports + taxes + value added = output (max abs. difference)",
                "negative intermediate flows below -tolerance (count)",
                "total value added unchanged (difference)"),
      value = c(row_bal, col_bal, sum(r$Z < -S$tolerance$balance), sum(r$W["B1G", ]) - sum(p$W["B1G", ])),
      status = ifelse(c(row_bal, col_bal) <= S$tolerance$balance | c(FALSE, FALSE), "OK", "DIFFERENCE") %>%
        c(ifelse(sum(r$Z < -S$tolerance$balance) == 0, "OK", "NEGATIVE"), ifelse(abs(sum(r$W["B1G", ]) - sum(p$W["B1G", ])) <= S$tolerance$balance, "OK", "DIFFERENCE")))
  }
}
SIOT_CHECK <- bind_rows(SIOT_CHECK)

# comparison with the published CBS domestic table ----
COMPARE <- NULL
pub <- LONG %>% filter(source == "dzs_2010", table == "T1800")
if (nrow(pub)) {
  COMPARE <- bind_rows(lapply(intersect(paste("dzs_2010", unlist(S$iot_models)), names(SIOT)), function(k) {
    r <- SIOT[[k]]; if (r$model == "D") return(NULL)
    Zp <- mat(pub %>% mutate(induse = paste0("CPA_", induse)), r$labels, r$labels)
    tibble(model = r$model, published_total = sum(Zp), derived_total = sum(r$Z),
           correlation = cor(as.vector(Zp), as.vector(r$Z)),
           share_of_absolute_difference = sum(abs(r$Z - Zp)) / sum(abs(Zp)), max_abs_cell_difference = max(abs(r$Z - Zp)))
  }))
}

for (k in names(SIOT)) {
  r <- SIOT[[k]]
  wide <- as_tibble(cbind(r$Z, r$Y, TOTAL_OUTPUT = r$x), rownames = "code") %>%
    bind_rows(as_tibble(rbind(r$W, if (!is.null(r$E)) r$E, OUTPUT = r$x), rownames = "code"))
  SIOT[[k]]$table <- wide
}
write_sheets(c(setNames(lapply(SIOT, `[[`, "table"), gsub(" ", "_model_", names(SIOT))),
               list(Checks = SIOT_CHECK), if (!is.null(COMPARE)) list(`vs published T1800` = COMPARE)),
             file.path(OUT, "Symmetric_tables.xlsx"))
print(as.data.frame(SIOT_CHECK)); if (!is.null(COMPARE)) print(as.data.frame(COMPARE))
