
# REVIEW PDR, first round
# Author: Federica Becca
# Data: 18.02.2026

# REVIEWER 2 PREGUNTA SOBRE EL GRADIENTE EDUCATIVO
# PORQUE LAS MADRES SOLTERAS AUMENTAN MÁS ENTRE LAS MÁS EDUCADAS? SE SEPARAN MÁS?
# CALCULAMOS CUANTAS MADRES SOLTERAS ESTAN NU, SDV, IU POR EDUCACIÓN

# library
library(tidyverse)
library(haven)
library(dplyr)
library(ggrepel)
library(stringr)
library(forcats)

`%notin%` <- Negate(`%in%`)

# load data ------------
load('00_data/DATA_FINAL_IPUMS_JULY.RData')

dfsm <- df_ipums

# USING ALL SAMPLE YEARS

# filter out Uruguay 2006 (design issue)
dfsm <- dfsm %>%
    dplyr::filter(!(COUNTRY2 == "uruguay" & YEAR == 2006))

# --- params ---
keep_c <- c('Argentina','Panama','Uruguay', 'El Salvador', 'Guatemala', 'Costa Rica', 'Chile', 'Mexico', 'Bolivia', 'Paraguay')
# --- your pipeline (integrated) ---

options(scipen = 100)

dfsm <- subset(dfsm, !is.na(dfsm$EDATTAIN2))
dfsm <- subset(dfsm, !is.na(dfsm$MARST2))

df <- dfsm |>
    dplyr::select(COUNTRY2, YEAR, EDATTAIN2, MARST2, PERWT) |>
    dplyr::group_by(COUNTRY2, YEAR, EDATTAIN2) |>
    dplyr::mutate(den = sum(PERWT)) |>
    dplyr::group_by(COUNTRY2, YEAR, EDATTAIN2, MARST2) |>
    dplyr::mutate(num = sum(PERWT)) |>
    dplyr::select(COUNTRY2, YEAR, EDATTAIN2, MARST2, den, num) |>
    dplyr::distinct() |>
    dplyr::ungroup()

df2 <- df |>
    dplyr::mutate(prop_eduxmarst = num / den)

df2$COUNTRY2 <- stringr::str_to_title(df2$COUNTRY2)

df2 <- df2 |>
    dplyr::group_by(COUNTRY2) |>
    dplyr::mutate(
        alpha = dplyr::if_else(COUNTRY2 %in% keep_c, "A", "B"),
        country_hi = dplyr::if_else(COUNTRY2 %in% keep_c, as.character(COUNTRY2), "Other"),
        colorL = dplyr::if_else(COUNTRY2 %in% keep_c & YEAR == min(YEAR), as.character(COUNTRY2), NA_character_)
    ) |>
    dplyr::ungroup()

# --- colors (named mapping is safest) ---
col_map <- c(
    "Uruguay"    = "#8f2d56",
    "Guatemala"  = "#00B4D8",
    "Costa Rica" = "#8358AC",
    "Chile"      = "#FA006E",
    "Mexico"     = "#90be6d",
    "Bolivia"    = "#FFBE0B",
    "El Salvador"= "#218380",
    "Paraguay"   = "#f6a6da",
    "Argentina"  = "#3A86FF",
    "Panama"     = "#f94144",
    "Other"      = "darkgrey"
)

# --- plot: grey lines faint, legend only highlighted ---
PLOT_lab <- ggplot(df2, aes(YEAR, prop_eduxmarst, group = COUNTRY2)) +
    geom_line(aes(colour = country_hi, alpha = alpha), linewidth = .7) +
    # geom_point(aes(colour = country_hi), size = .6) +
    scale_alpha_manual(values = c(A = .9, B = .4), guide = "none") +
    scale_colour_manual(values = col_map, breaks = keep_c, drop = TRUE) +
    facet_grid(MARST2 ~ EDATTAIN2) +
    theme_light(14) +
    labs(y = "Weighted proportion", x = "") +
    theme(legend.position = 'right')+
    guides(colour = guide_legend(title = NULL)); PLOT_lab

ggsave( "02_plots/022026_review/REVIEW_R2_C1.png", 
        plot=PLOT_lab, scale = 1, bg='white',
        dpi = 500, height = 7, width = 9)

# ------------------------------------------------------------------------------

options(scipen = 100)

dfsm <- subset(dfsm, !is.na(dfsm$EDATTAIN2))
dfsm <- subset(dfsm, !is.na(dfsm$MARST2))

df <- dfsm |> 
    select(COUNTRY2, YEAR, EDATTAIN2, MARST2, PERWT) |> 
    group_by(COUNTRY2, YEAR, EDATTAIN2) |> 
    mutate(den =sum(PERWT)) |> 
    group_by(COUNTRY2, YEAR, EDATTAIN2, MARST2) |>
    mutate(num = sum(PERWT)) |> 
    select(COUNTRY2, YEAR, EDATTAIN2, MARST2, den, num) |>  distinct() |> ungroup()
    

df2 <- df |> 
    mutate(prop_eduxmarst = num/den)

ggplot(df2, aes(YEAR, prop_eduxmarst))+
    geom_line(aes(group = COUNTRY2, colour = COUNTRY2))+
    facet_grid(MARST2~EDATTAIN2)+
    theme_light()

# USING ONLY FIRST AND LAST SAMPLE FOR EACH COUNTRY

# --- params ---
keep_c <- c('Argentina','Panama','El Salvador', 'Guatemala', 'Costa Rica', 'Chile', 'Mexico', 'Bolivia', 'Paraguay')

# --- your pipeline (integrated) ---
dfsm <- dfsm |>
    dplyr::group_by(COUNTRY2) |>
    dplyr::filter(YEAR == min(YEAR) | YEAR == max(YEAR)) |>
    dplyr::ungroup()

options(scipen = 100)

dfsm <- subset(dfsm, !is.na(dfsm$EDATTAIN2))
dfsm <- subset(dfsm, !is.na(dfsm$MARST2))

df <- dfsm |>
    dplyr::select(COUNTRY2, YEAR, EDATTAIN2, MARST2, PERWT) |>
    dplyr::group_by(COUNTRY2, YEAR, EDATTAIN2) |>
    dplyr::mutate(den = sum(PERWT)) |>
    dplyr::group_by(COUNTRY2, YEAR, EDATTAIN2, MARST2) |>
    dplyr::mutate(num = sum(PERWT)) |>
    dplyr::select(COUNTRY2, YEAR, EDATTAIN2, MARST2, den, num) |>
    dplyr::distinct() |>
    dplyr::ungroup()

df2 <- df |>
    dplyr::mutate(prop_eduxmarst = num / den)

df2$COUNTRY2 <- stringr::str_to_title(df2$COUNTRY2)

df2 <- df2 |>
    dplyr::group_by(COUNTRY2) |>
    dplyr::mutate(
        alpha = dplyr::if_else(COUNTRY2 %in% keep_c, "A", "B"),
        country_hi = dplyr::if_else(COUNTRY2 %in% keep_c, as.character(COUNTRY2), "Other"),
        colorL = dplyr::if_else(COUNTRY2 %in% keep_c & YEAR == min(YEAR), as.character(COUNTRY2), NA_character_)
    ) |>
    dplyr::ungroup()

# --- colors (named mapping is safest) ---
col_map <- c(
    "Uruguay"    = "#8f2d56",
    "Guatemala"  = "#00B4D8",
    "Costa Rica" = "#8358AC",
    "Chile"      = "#FA006E",
    "Mexico"     = "#90be6d",
    "Bolivia"    = "#FFBE0B",
    "El Salvador"= "#218380",
    "Paraguay"   = "#f6a6da",
    "Argentina"  = "#3A86FF",
    "Panama"     = "#f94144",
    "Other"      = "darkgrey"
)

# --- plot: grey lines faint, legend only highlighted ---
PLOT_lab <- ggplot(df2, aes(YEAR, prop_eduxmarst, group = COUNTRY2)) +
    geom_line(aes(colour = country_hi, alpha = alpha), linewidth = .7) +
    geom_point(aes(colour = country_hi), size = .6) +
    scale_alpha_manual(values = c(A = .9, B = .4), guide = "none") +
    scale_colour_manual(values = col_map, breaks = keep_c, drop = TRUE) +
    facet_grid(MARST2 ~ EDATTAIN2) +
    theme_light(14) +
    labs(y = "Weighted proportion", x = "") +
    theme(legend.position = 'right')+
    guides(colour = guide_legend(title = NULL)); PLOT_lab

ggsave( "02_plots/022026_review/REVIEW_R2_C1.png", 
    plot=PLOT_lab, scale = 1, bg='white',
    dpi = 500, height = 7, width = 9)

