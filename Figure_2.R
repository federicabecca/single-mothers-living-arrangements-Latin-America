# Liv. Arr. in singmother
#    Federica Becca
###############################

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

# countries 32  68  76 152 170 188 218 222 320 340 484 558 591 600 604 858 862

load('00_data//DATA_FINAL_IPUMS_ALL_MOTHERS_JULY.RData')

dfmom <- df_ipums_all


# PLOT TREND IN SINGLE MOTHERHOOD OVER TIME BY EDUCATION --------------------------------------------

dfmom1 <- dfmom |> 
    filter(!is.na(EDATTAIN2))

dfmom2 <- dfmom1 |> 
    select(COUNTRY2, COUNTRY, YEAR, SAMPLE, EDATTAIN2, MOM2, MOM) |> 
    group_by(SAMPLE, COUNTRY2, YEAR, EDATTAIN2) %>%      
    mutate(
        sm  = sum(MOM == 0),   
        mom = sum(MOM == 1),   
        prop = (sm / (sm + mom)) * 100
    ) %>% 
    select(COUNTRY2, COUNTRY, YEAR, SAMPLE, EDATTAIN2, prop) %>% 
    distinct()

dfmom2$COUNTRY2 <- str_to_title(dfmom2$COUNTRY2)

dfmom2 <- dfmom2 |> 
    group_by(COUNTRY2) |> 
    mutate(color = ifelse(COUNTRY2 %in% c('Ecuador', 'Brazil', 'Costa Rica',
                                          'Chile', 'Mexico', 'Panama', 'Nicaragua'), COUNTRY2, NA),
           alpha = ifelse(COUNTRY2 %in% c('Ecuador', 'Brazil', 'Costa Rica',
                                          'Chile', 'Mexico', 'Panama', 'Nicaragua'), 'A', 'B'),
           colorL = ifelse(COUNTRY2 %in% c('Ecuador', 'Brazil', 'Costa Rica',
                                           'Chile', 'Mexico', 'Panama', 'Nicaragua') & YEAR==min(YEAR), COUNTRY2, NA))

col <- c('#f94144', '#218380', '#8f2d56', '#f6a6da', '#73c2de','#fdac42', '#90be6d', 'darkgrey')

# plot
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
            margin = margin(t = 8)
        ),
        axis.title.y = element_text(
            size = 8,
            lineheight = 9 / 8,
            margin = margin(r = 8)
        ),
        axis.text.x = element_text(size = 8, lineheight = 9 / 8),
        axis.text.y = element_text(size = 8, lineheight = 9 / 8),
        
        # Labels (e.g., facet strip text, legend): 6-pt font; 7-pt leading
        strip.text.x = element_text(size = 4, lineheight = 7 / 6),
        strip.text.y = element_text(size = 4, lineheight = 7 / 6),
        legend.text  = element_text(size = 4, lineheight = 7 / 6),
        legend.title = element_text(size = 4, lineheight = 7 / 6),
        
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

prop.sm.edu <- ggplot(dfmom2, aes(YEAR, prop, group=COUNTRY2))+
    geom_line(aes(col=color, alpha = alpha, linewidth=alpha), show.legend = F)+
    facet_wrap(~EDATTAIN2, nrow=1)+
    scale_alpha_manual(values=c(1, 0.5))+
    scale_linewidth_manual(values=c(0.7, 0.3))+
    scale_color_manual(values = col)+
    scale_x_continuous(breaks = seq(1960,2020,10), limits=c(1960,2022))+
    scale_y_continuous(breaks = seq(0,50,10), limits=c(0,50))+
    geom_label_repel(aes(label = colorL, colour = color, fontface='bold',
                         family="Times New Roman"),
                     nudge_x = 0,nudge_y = -1, size=2,
                     show.legend = F, segment.colour = 'white') +
    theme_bw()+
    labs(y='%', x='')+
    theme(legend.position = 'bottom',
          panel.grid.major = element_line(linetype = 'dashed', linewidth = .5),
          panel.grid.minor = element_line(color =NA))+
    guides(colour = guide_legend(nrow = 2,title = NULL))+
    guides(colour = guide_legend(nrow = 2,title = NULL))+
    theme_pdr; prop.sm.edu

#save as svg
ggsave("02_plots\\PDR_format\\Becca_Figure_2.svg",
       plot = prop.sm.edu,
       device = "svg",
       width = 4.83,     # journal max width
       height = 3,       # adjust based on aspect ratio
       units = "in",
       bg = "white")

ggsave("02_plots\\PDR_format\\Becca_Figure_2.svg",
    plot = prop.sm.edu,
    device = "svg",
    width = 4.83,     # journal max width
    height = 3,       # adjust based on aspect ratio
    units = "in",
    bg = "white"
)

ggsave( "02_plots/TREND_SINGLEMOTHERHOOD_EDU_IPUMS_NOVEMBER.png", 
        plot=prop.sm.edu, scale = 1, bg='white',
        dpi = 500, height = 6, width = 10)
