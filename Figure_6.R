
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

la <- dfsm |> 
    group_by(COUNTRY2) |> 
    filter(YEAR == min(YEAR) | YEAR == max(YEAR)) |> 
    mutate(T1 = ifelse(YEAR == min(YEAR), 'Earliest sample', 
        ifelse(YEAR == max(YEAR), 'Latest sample', NA)))

# boxplot with livign arrangements by MARST --------------------------------------------

mar <- dfsm |> 
    group_by(COUNTRY2) |> 
    filter(YEAR == min(YEAR) | YEAR == max(YEAR)) |> 
    mutate(T1 = ifelse(YEAR == min(YEAR), 'Earliest sample', 
        ifelse(YEAR == max(YEAR), 'Latest sample', NA)))

mar2 <- mar |> 
    group_by(SAMPLE, MARST2) |> 
    mutate(den = sum(PERWT),
        num1 = sum(PERWT[LA == 'Only with children']),
        num2 = sum(PERWT[LA == 'Extended with parents']),
        num3 = sum(PERWT[LA == 'Extended without parents']))

mar2 <- subset(mar2, !is.na(mar2$MARST2))

mar3 <- mar2 |> 
    group_by(SAMPLE, MARST2) |> 
    mutate(prop1 = num1/den, 
        prop2 = num2/den, 
        prop3 = num3/den) 

mar3 <- mar3  |> 
    select(COUNTRY2, YEAR, T1, MARST2, prop1, prop2, prop3) |> 
    distinct() |> 
    group_by(COUNTRY2) |> 
    pivot_longer(cols = c(prop1, prop2, prop3),
        names_to = "LA",
        values_to = "values")

mar3$LA <- factor(mar3$LA, levels=c('prop1', 'prop2', 'prop3'))

levels(mar3$LA) <- c('Nuclear', 
    'Extended with parents', 
    'Extended without parents')

mar3$MARST3 <- factor(mar3$MARST2, levels=c("Never married",
                                            "Sep/Div/Wid",
                                            "Partner absent"))

levels(mar3$MARST3) <- c("Never in union",
                         "Sep/Div/Wid" , 
                         "Partner absent")

mar3$T1 <- factor(mar3$T1 , levels = c('Earliest sample',
    'Latest sample'))


palette1 <- c('#f94144', '#fdac42', '#8f2d56')


theme_pdr <- list(
    theme(
        # Title: 9-pt font; 10-pt leading
        plot.title = element_text(
            size = 9,
            lineheight = 10 / 9,
            hjust = 0.5
        ),
        
        # Axes: 8-pt font; 9-pt leading
        axis.title.x = element_text(
            size = 8,
            lineheight = 9 / 8,
            margin = margin(t = 5)
        ),
        axis.title.y = element_text(
            size = 8,
            lineheight = 9 / 8,
            margin = margin(r = 5)
        ),
        axis.text.x = element_text(size = 6, lineheight = 9 / 8),
        axis.text.y = element_text(size = 6, lineheight = 9 / 8),
        
        # Labels (e.g., facet strip text, legend): 6-pt font; 7-pt leading
        strip.text.x = element_text(size = 6, lineheight = 7 / 6),
        strip.text.y = element_text(size = 6, lineheight = 7 / 6),
        legend.text  = element_text(size = 6, lineheight = 7 / 6),
        legend.title = element_text(size = 6, lineheight = 7 / 6),
        
        # # Notes and sources (caption): 7-pt font; 8-pt leading
        # plot.caption = element_text(
        #     size = 7,
        #     lineheight = 8 / 7,
        #     hjust = 0        # left-align captions, typical for notes/sources
        # ),
        
        # Overall font family
        text = element_text(family = "Times New Roman")
    )
)

bymar <- ggplot(mar3, aes(T1, values))+
    geom_boxplot(aes(fill=MARST3), size = .5, show.legend = T,
        outlier.size = 0.4, linewidth=.2, alpha=.8)+
    theme_light()+
    facet_wrap(~LA)+
    scale_fill_manual(values = palette1, name='')+
    scale_y_continuous(breaks=seq(0,1,0.25), limits=c(0,1))+
    labs(y='Proportion', x='')+
    theme(legend.position = 'bottom',
          panel.grid.major = element_line(linetype = 'dashed', linewidth = .3, color = "#EBEBEB"),
        panel.grid.minor = element_line(linetype = 'dashed'),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0))+
    theme_pdr; bymar


#save as svg
ggsave("02_plots\\PDR_format\\Becca_Figure_6.svg",
       plot = bymar,
       device = "svg",
       width = 4.83,   
       height = 3.5,     
       units = "in",
       bg = "white")

ggsave( "02_plots/BOXPLOT_BYMAR_NOVEMBER_IPUMS.png", plot=bymar, scale = 1, bg='white',
    dpi = 500, height = 6, width = 10)
