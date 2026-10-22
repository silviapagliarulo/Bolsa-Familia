
# TWFE – Bolsa Família → IFDM (Índice Firjan de Desenvolvimento Municipal)
# Metodo: DiD continuo con FE municipio + FE anno (TWFE)
# Dati: panel municipio–anno, 2006–2016


library(tidyverse)
library(readxl)
library(readr)
library(janitor)
library(stringr)
library(fixest)
library(modelsummary)
library(sidrar)


# FUNZIONI AUSILIARIE


pad_ibge <- function(x) {
  x <- as.character(trimws(x))
  out <- rep(NA_character_, length(x))
  ok6 <- !is.na(x) & str_detect(x, "^[0-9]{6}$")
  ok7 <- !is.na(x) & str_detect(x, "^[0-9]{7}$")
  out[ok6] <- paste0("0", x[ok6])
  out[ok7] <- x[ok7]
  as.integer(out)
}

normalizza <- function(x) {
  x %>%
    str_to_lower() %>%
    iconv(to = "ASCII//TRANSLIT") %>%
    str_remove_all("[^a-z0-9 ]") %>%
    str_squish()
}


# 1. POPOLAZIONE MUNICIPALE (IBGE/TCU)


parse_pop <- function(file, sheet = 1, year) {
  read_excel(file, sheet = sheet, col_names = FALSE) %>%
    filter(
      !is.na(...2),
      str_detect(as.character(...3), "^[0-9]{5}$")
    ) %>%
    transmute(
      cod_uf    = as.integer(...2),
      cod_munic = as.integer(...3),
      year      = year,
      pop       = .[[ncol(.)]] %>%
        as.character() %>%
        str_remove_all("\\(.*?\\)") %>%
        str_remove_all("[^0-9]") %>%
        as.numeric()
    ) %>%
    filter(!is.na(pop))
}


# 1a. Anno 2006


bf_lookup_raw <- read_excel("C:/Users/silvi/Downloads/bolsa familia.xls")

lookup <- bf_lookup_raw %>%
  rename_with(~ str_to_lower(iconv(.x, to = "ASCII//TRANSLIT"))) %>%
  transmute(
    cod_munic = pad_ibge(codigo),
    nome_norm = normalizza(municipio)
  ) %>%
  distinct(nome_norm, .keep_all = TRUE)

corr_2006 <- tribble(
  ~nome_norm,                   ~nome_norm_bf,
  "aroeiras do itaim",          "aroeiras do itaim",
  "sao miguel do gostoso",      "sao miguel do gostoso",
  "ilha de itamaraca",          "ilha de itamaraca",
  "barro preto",                "barro preto",
  "itabirinha",                 "itabirinha de mantena",
  "mogi das cruzes",            "mogi das cruzes",
  "moji mirim",                 "mogi mirim",
  "balneario picarras",         "balneario de picarras",
  "presidente castello branco", "presidente castelo branco"
)

lookup_ext <- lookup %>%
  bind_rows(
    corr_2006 %>%
      left_join(lookup, by = c("nome_norm_bf" = "nome_norm")) %>%
      transmute(nome_norm, cod_munic)
  ) %>%
  distinct(nome_norm, .keep_all = TRUE)

pop_2006 <- read_excel(
  "C:/Users/silvi/Downloads/POP2006-TCU.xls",
  col_names = FALSE
) %>%
  filter(
    !is.na(...2), !is.na(...3),
    str_detect(as.character(...2), "^[0-9]{1,2}$"),
    str_detect(as.character(...3), "^[0-9]{1,4}$"),
    !is.na(...5)
  ) %>%
  transmute(
    nome_norm = normalizza(...4),
    year      = 2006L,
    pop       = as.character(...5) %>%
      str_remove_all("[^0-9]") %>%
      as.numeric()
  ) %>%
  filter(!is.na(pop), pop > 0) %>%
  left_join(lookup_ext, by = "nome_norm") %>%
  filter(!is.na(cod_munic)) %>%
  select(cod_munic, year, pop)

cat("2006 — municipios matchati:", nrow(pop_2006), "\n")

# 1b. Anni 2008–2016 + correzione duplicati + interpolazione
# (limitato al 2016 per l'IFDM)


pop_panel_raw <- bind_rows(
  pop_2006,
  parse_pop("C:/Users/silvi/Downloads/POP2008_DOU.xls",                        1,            2008),
  read_excel("C:/Users/silvi/Downloads/UF_Municipio.xls", col_names = FALSE) %>%
    filter(str_detect(as.character(...3), "^[0-9]{5}$")) %>%
    transmute(
      cod_uf    = as.integer(...2),
      cod_munic = as.integer(...3),
      year      = 2009L,
      pop       = as.numeric(str_remove_all(as.character(...5), "\\."))
    ),
  parse_pop("C:/Users/silvi/Downloads/2011.xls",                               1,            2011),
  parse_pop("C:/Users/silvi/Downloads/estimativa_2012_DOU_28_08_2012_xls.xls", 1,            2012),
  parse_pop("C:/Users/silvi/Downloads/estimativa_2013_dou_xls.xls",            1,            2013),
  parse_pop("C:/Users/silvi/Downloads/estimativas_dou_2014_xls.xls",           "Municípios", 2014),
  parse_pop("C:/Users/silvi/Downloads/estimativa_TCU_2015_20170614.xls",       "Municípios", 2015),
  parse_pop("C:/Users/silvi/Downloads/estimativa_TCU_2016_20170614.xls",       "Municípios", 2016)
) %>%
  mutate(cod_munic = if_else(
    !is.na(cod_uf),
    pad_ibge(paste0(str_pad(cod_uf, 2, pad = "0"), str_pad(cod_munic, 5, pad = "0"))),
    cod_munic
  ))

# Deduplicazione
dup_check <- pop_panel_raw %>%
  count(cod_munic, year) %>%
  filter(n > 1)
cat("Coppie cod_munic-year duplicate:", nrow(dup_check), "\n")

pop_panel_raw <- pop_panel_raw %>%
  group_by(cod_munic, year) %>%
  slice_max(pop, n = 1, with_ties = FALSE) %>%
  ungroup()

# Interpolazione
pop_panel <- pop_panel_raw %>%
  group_by(cod_munic) %>%
  complete(year = 2006:2016) %>%
  arrange(year) %>%
  mutate(
    pop = {
      obs <- !is.na(pop)
      if (sum(obs) >= 2) {
        approx(
          x    = year[obs],
          y    = pop[obs],
          xout = year,
          rule = 2
        )$y
      } else if (sum(obs) == 1) {
        rep(pop[obs], length(year))
      } else {
        pop
      }
    }
  ) %>%
  ungroup() %>%
  filter(!is.na(pop)) %>%
  select(cod_munic, year, pop)

cat("Municipios in pop_panel:", n_distinct(pop_panel$cod_munic), "\n")
cat("Anni coperti:",            range(pop_panel$year), "\n")


# 2. PIB MUNICIPALE — IBGE/SIDRA, tabella 5938


pib_sidra <- map_dfr(
  list(2006:2010, 2011:2014, 2015:2016),
  ~ get_sidra(
    x        = 5938,
    variable = 37,
    period   = as.character(.x),
    geo      = "City"
  )
)

pib <- pib_sidra %>%
  transmute(
    cod_munic = pad_ibge(`Município (Código)`),
    year      = as.integer(`Ano (Código)`),
    pib_total = as.numeric(Valor)
  ) %>%
  filter(!is.na(cod_munic), !is.na(pib_total), !is.na(year)) %>%
  group_by(cod_munic, year) %>%
  slice_max(pib_total, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(
    pop_panel %>% select(cod_munic, year, pop),
    by = c("cod_munic", "year")
  ) %>%
  mutate(pib_pc = (pib_total * 1000) / pop) %>%
  filter(!is.na(pib_pc), pib_pc > 0) %>%
  select(cod_munic, year, pib_pc)

cat("Osservazioni PIB:", nrow(pib), "\n")
cat("Municipios PIB:",   n_distinct(pib$cod_munic), "\n")


# 3. BOLSA FAMÍLIA (IPEADATA)


bf_year <- bf_lookup_raw %>%
  rename_with(~ str_to_lower(iconv(.x, to = "ASCII//TRANSLIT"))) %>%
  select(codigo, matches("^\\d{4}$")) %>%
  pivot_longer(
    cols      = -codigo,
    names_to  = "year",
    values_to = "beneficiarios"
  ) %>%
  mutate(
    cod_munic     = pad_ibge(codigo),
    year          = as.integer(year),
    beneficiarios = as.numeric(beneficiarios)
  ) %>%
  filter(!is.na(cod_munic), !is.na(beneficiarios)) %>%
  group_by(cod_munic, year) %>%
  slice_max(beneficiarios, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(cod_munic, year, beneficiarios)


# 4. IFDM (Índice Firjan de Desenvolvimento Municipal)


ifdm_raw <- read_excel(
  "C:/Users/silvi/Downloads/IFDM - Índice Firjan de Desenvolvimento Municipal (2018) - INATIVA.xls"
)

ifdm <- ifdm_raw %>%
  rename_with(~ str_to_lower(iconv(.x, to = "ASCII//TRANSLIT"))) %>%
  select(codigo, matches("^\\d{4}$")) %>%
  pivot_longer(
    cols      = -codigo,
    names_to  = "year",
    values_to = "ifdm"
  ) %>%
  mutate(
    cod_munic = pad_ibge(codigo),
    year      = as.integer(year),
    ifdm      = as.numeric(ifdm)
  ) %>%
  filter(
    !is.na(cod_munic),
    !is.na(ifdm),
    year >= 2006,
    year <= 2016
  ) %>%
  group_by(cod_munic, year) %>%
  slice_max(ifdm, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(cod_munic, year, ifdm)

cat("Osservazioni IFDM:", nrow(ifdm), "\n")
cat("Municipios IFDM:",   n_distinct(ifdm$cod_munic), "\n")
cat("Anni IFDM:",         range(ifdm$year), "\n")

# Diagnostica NA per anno
ifdm_raw %>%
  rename_with(~ str_to_lower(iconv(.x, to = "ASCII//TRANSLIT"))) %>%
  select(matches("^\\d{4}$")) %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "anno", values_to = "n_na") %>%
  print()


# 5. COSTRUZIONE DEL PANEL FINALE


panel_ifdm <- pop_panel %>%
  filter(year >= 2006, year <= 2016) %>%
  left_join(ifdm,    by = c("cod_munic", "year")) %>%
  left_join(bf_year, by = c("cod_munic", "year")) %>%
  left_join(
    pib %>% filter(year <= 2016),
    by = c("cod_munic", "year"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    ln_pop      = log(pop),
    ln_pib_pc   = log(pib_pc),
    coverage    = beneficiarios / pop,
    ln_coverage = log(coverage + 1e-6)
  ) %>%
  filter(
    !is.na(ifdm),
    !is.na(coverage),
    !is.na(ln_pib_pc),
    !is.na(ln_pop)
  ) %>%
  mutate(
    cod_munic = as.factor(cod_munic),
    year      = as.factor(year)
  )

cat("Osservazioni nel panel IFDM:", nrow(panel_ifdm), "\n")
cat("Municipios:",                  n_distinct(panel_ifdm$cod_munic), "\n")
cat("Anni:",                        levels(panel_ifdm$year), "\n")

panel_ifdm %>% summarise(across(
  c(ifdm, coverage, ln_pib_pc, ln_pop),
  ~ sum(is.na(.)),
  .names = "NA_{.col}"
))


# 6. MODELLI TWFE


twfe_ifdm_base <- feols(
  ifdm ~ coverage | cod_munic + year,
  data    = panel_ifdm,
  cluster = ~cod_munic
)

twfe_ifdm_ctrl <- feols(
  ifdm ~ coverage + ln_pib_pc + ln_pop | cod_munic + year,
  data    = panel_ifdm,
  cluster = ~cod_munic
)

twfe_ifdm_log <- feols(
  ifdm ~ ln_coverage + ln_pib_pc + ln_pop | cod_munic + year,
  data    = panel_ifdm,
  cluster = ~cod_munic
)

summary(twfe_ifdm_base)
summary(twfe_ifdm_ctrl)
summary(twfe_ifdm_log)

modelsummary(
  list(
    "Base"      = twfe_ifdm_base,
    "Controlli" = twfe_ifdm_ctrl,
    "Log-Log"   = twfe_ifdm_log
  ),
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_map = c(
    "coverage"    = "Copertura BF",
    "ln_coverage" = "ln(Copertura BF)",
    "ln_pib_pc"   = "ln(PIL p.c.)",
    "ln_pop"      = "ln(Popolazione)"
  ),
  gof_map  = c("nobs", "r.squared", "FE: cod_munic", "FE: year"),
  title    = "TWFE: Effetto di Bolsa Família sull'IFDM (2006–2016)"
)


# 7. EVENT STUDY CORRETTO (intensità fissa al 2006)


intensity_ifdm_2006 <- panel_ifdm %>%
  filter(as.integer(as.character(year)) == 2006) %>%
  select(cod_munic, coverage_base = coverage)

panel_ifdm_es <- panel_ifdm %>%
  mutate(year_num = as.integer(as.character(year))) %>%
  left_join(intensity_ifdm_2006, by = "cod_munic")

event_study_ifdm <- feols(
  ifdm ~ i(year_num, coverage_base, ref = 2006) + ln_pib_pc + ln_pop |
    cod_munic + year,
  data    = panel_ifdm_es,
  cluster = ~cod_munic
)

summary(event_study_ifdm)

iplot(
  event_study_ifdm,
  main     = "Event Study: Bolsa Família → IFDM",
  xlab     = "Anno",
  ylab     = "Coefficiente (95% CI)",
  ref.line = TRUE
)


# 8. ROBUSTEZZA: solo municipios con coverage > 0 almeno un anno


panel_ifdm_nozero <- panel_ifdm %>%
  group_by(cod_munic) %>%
  filter(any(coverage > 0)) %>%
  ungroup()

twfe_ifdm_nozero <- feols(
  ifdm ~ coverage + ln_pib_pc + ln_pop | cod_munic + year,
  data    = panel_ifdm_nozero,
  cluster = ~cod_munic
)

modelsummary(
  list(
    "Baseline"    = twfe_ifdm_ctrl,
    "Solo tratt." = twfe_ifdm_nozero
  ),
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_map = c(
    "coverage"  = "Copertura BF",
    "ln_pib_pc" = "ln(PIL p.c.)",
    "ln_pop"    = "ln(Popolazione)"
  ),
  gof_map = c("nobs", "r.squared"),
  title   = "Robustezza IFDM: esclusione municipios sempre a coverage zero"
)


# 9. DE-TRENDING QUADRATICO CORRETTO (anno centrato)


# Passo 1: stima trend sul periodo pre (2006–2011)
panel_ifdm_pre_c <- panel_ifdm_es %>%
  filter(year_num <= 2011) %>%
  mutate(
    year_c  = year_num - 2008,
    year_c2 = year_c^2
  )

trend_test_ifdm_q <- feols(
  ifdm ~ year_c : coverage_base + year_c2 : coverage_base +
    ln_pib_pc + ln_pop | cod_munic + year,
  data    = panel_ifdm_pre_c,
  cluster = ~cod_munic
)
summary(trend_test_ifdm_q)

# Estrai coefficienti
names(coef(trend_test_ifdm_q))
coef_lin  <- coef(trend_test_ifdm_q)["year_c:coverage_base"]
coef_quad <- coef(trend_test_ifdm_q)["coverage_base:year_c2"]

cat("coef_lin:",  coef_lin,  "\n")
cat("coef_quad:", coef_quad, "\n")
stopifnot(!is.na(coef_lin), !is.na(coef_quad))

# Passo 2: de-trend sull'intero panel
panel_ifdm_detrend_q <- panel_ifdm_es %>%
  mutate(
    year_c        = year_num - 2008,
    year_c2       = year_c^2,
    coverage_safe = replace_na(coverage_base, 0),
    ifdm_dt_q     = ifdm
    - coef_lin  * year_c  * coverage_safe
    - coef_quad * year_c2 * coverage_safe
  )

cat("NA in ifdm_dt_q:", sum(is.na(panel_ifdm_detrend_q$ifdm_dt_q)), "\n")
cat("Valori validi:",   sum(!is.na(panel_ifdm_detrend_q$ifdm_dt_q)), "\n")

# Passo 3: event study de-trended
event_study_ifdm_dt_q <- feols(
  ifdm_dt_q ~ i(year_num, coverage_base, ref = 2006) + ln_pib_pc + ln_pop |
    cod_munic + year,
  data    = panel_ifdm_detrend_q,
  cluster = ~cod_munic
)
summary(event_study_ifdm_dt_q)


# 10. GRAFICI COMPARATIVI EVENT STUDY


par(mfrow = c(1, 2))

iplot(
  event_study_ifdm,
  main     = "Event Study originale",
  xlab     = "Anno",
  ylab     = "Coefficiente (95% CI)",
  ref.line = TRUE
)
abline(v = 2011.5, lty = 2, col = "red")
text(2011.7, par("usr")[4] * 0.9, "Espansione BF",
     col = "red", cex = 0.8, adj = 0)

iplot(
  event_study_ifdm_dt_q,
  main     = "Event Study de-trended (quadratico)",
  xlab     = "Anno",
  ylab     = "",
  ref.line = TRUE
)
abline(v = 2011.5, lty = 2, col = "red")
text(2011.7, par("usr")[4] * 0.9, "Espansione BF",
     col = "red", cex = 0.8, adj = 0)

par(mfrow = c(1, 1))

# Tabella riassuntiva coefficienti pre/post
broom::tidy(event_study_ifdm_dt_q, conf.int = TRUE) %>%
  filter(str_detect(term, "coverage_base")) %>%
  mutate(
    anno    = as.integer(str_extract(term, "\\d{4}")),
    periodo = if_else(anno <= 2011, "Pre (de-trended)", "Post")
  ) %>%
  select(anno, periodo, estimate, conf.low, conf.high, p.value) %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  print(n = 20)

library(patchwork)

es_tidy_dt <- broom::tidy(event_study_ifdm_dt_q, conf.int = TRUE) %>%
  filter(str_detect(term, "coverage_base")) %>%
  mutate(anno = as.integer(str_extract(term, "\\d{4}"))) %>%
  bind_rows(tibble(anno = 2006L, estimate = 0, conf.low = 0, conf.high = 0)) %>%
  arrange(anno)

make_es_plot <- function(df, titolo) {
  ggplot(df, aes(x = anno, y = estimate)) +
    geom_hline(yintercept = 0, linewidth = 0.6) +
    geom_vline(xintercept = 2006, linetype = "dashed", color = "red", linewidth = 0.8) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25, linewidth = 0.7) +
    geom_point(size = 2.2) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.major = element_line(color = "grey85", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", hjust = 0.5, size = 12)
    ) +
    labs(title = titolo, x = "Anno", y = "Coefficiente (95% CI)") +
    scale_x_continuous(breaks = seq(2006, 2016, by = 2))
}

p1 <- make_es_plot(es_tidy,    "Event Study originale")
p2 <- make_es_plot(es_tidy_dt, "Event Study de-trended")

(p1 | p2) +
  plot_annotation(
    caption = "Figure 2: IFDM event study result",
    theme   = theme(plot.caption = element_text(hjust = 0.5))
  )

library(ggplot2)
library(broom)
library(dplyr)
library(stringr)

# --- Estrai coefficienti ---
es_dt_tidy <- broom::tidy(event_study_ifdm_dt_q, conf.int = TRUE) %>%
  filter(str_detect(term, "coverage_base")) %>%
  mutate(anno = as.integer(str_extract(term, "\\d{4}"))) %>%
  bind_rows(tibble(anno = 2006L, estimate = 0, conf.low = 0, conf.high = 0)) %>%
  arrange(anno)

# --- Plot ---
ggplot(es_dt_tidy, aes(x = anno, y = estimate)) +
  geom_hline(yintercept = 0, linewidth = 0.6, color = "black") +
  geom_vline(xintercept = 2011.5, linetype = "dashed",
             color = "red", linewidth = 0.8) +
  annotate("text", x = 2011.7, y = max(es_dt_tidy$conf.high) * 0.95,
           label = "Brasil sem Miséria", color = "red",
           size = 3.2, hjust = 0) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.25, linewidth = 0.7) +
  geom_point(size = 2.2, shape = 19) +
  scale_x_continuous(breaks = seq(2006, 2016, by = 2)) +
  theme_bw(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey85", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    plot.title   = element_text(face = "bold", hjust = 0.5, size = 13),
    axis.title   = element_text(size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10,
                                margin = margin(t = 8))
  ) +
  labs(
    title   = "Event Study (De-trended): Bolsa Família → IFDM",
    x       = "Year",
    y       = "Coefficient (95% CI)",
    caption = "Note: treatment intensity anchored to 2006 baseline coverage.\nPre-existing quadratic trend estimated on 2006–2011 removed."
  )

ggsave("event_study_ifdm_detrended.pdf", width = 7, height = 4.5)