
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

# Plot living arrangements over time

la <- dfsm |> 
    group_by(COUNTRY2) |> 
    filter(YEAR == min(YEAR) | YEAR == max(YEAR)) |> 
    mutate(T1 = ifelse(YEAR == min(YEAR), 'Earliest sample', 
        ifelse(YEAR == max(YEAR), 'Latest sample', NA)))

la2 <- la |> 
    group_by(SAMPLE) |> 
    mutate(den = sum(PERWT),
        num1 = sum(PERWT[LA == 'Only with children']),
        num2 = sum(PERWT[LA == 'Extended with parents']),
        num3 = sum(PERWT[LA == 'Extended without parents']))

la3 <- la2 |> 
    group_by(SAMPLE) |> 
    mutate(prop1 = num1/den, 
        prop2 = num2/den, 
        prop3 = num3/den) 

la3 <- la3  |> 
    select(COUNTRY2, YEAR, T1, prop1, prop2, prop3) |> 
    distinct() |> 
    group_by(COUNTRY2) |> 
    pivot_longer(cols = c(prop1, prop2, prop3),
        names_to = "LA",
        values_to = "values")

la3$LA <- factor(la3$LA, levels=c('prop1', 'prop2', 'prop3'))

levels(la3$LA) <- c('Nuclear', 
    'Extended with parents', 
    'Extended without parents')


# Create a country label with years
country_years <- la3 %>%
    group_by(COUNTRY2) %>%
    summarise(
        min_year = min(YEAR),
        max_year = max(YEAR),
        .groups = "drop") %>%
    mutate(
        CNTRY_LABEL = paste0(str_to_title(COUNTRY2), " (", min_year, "–", max_year, ")"))

# Join the labels back
la3 <- la3 %>%
    left_join(country_years, by = "COUNTRY2")


# Reorder countries based on 'Only with children' in the oldest sample
ordering <- la3 %>%
    filter(LA == "Extended with parents", T1 == "Latest sample") %>%
    arrange(values) %>%
    pull(CNTRY_LABEL)

# Make CNTRY_LABEL a factor ordered by that
la3 <- la3 %>%
    mutate(CNTRY_LABEL = factor(CNTRY_LABEL, levels = ordering))


#plot
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
        axis.text.x = element_text(size = 8, lineheight = 9 / 8),
        axis.text.y = element_text(size = 8, lineheight = 9 / 8),
        
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

barplot3 <- ggplot(la3, aes(x = CNTRY_LABEL, y = values, color = T1, shape = T1)) +
    geom_point(size = 2.5, alpha = 0.8) +
    theme_light() +
    facet_wrap(~LA) +
    coord_flip() +
    scale_color_manual(
        values = c("Earliest sample" = "black", 
            "Latest sample" = "#8f2d56"),
        name = "") +
    scale_shape_manual(
        values = c("Earliest sample" = 1,  
            "Latest sample" = 16),   
        name = "" ) +
    scale_y_continuous(breaks = seq(0, 0.6, 0.2),
        limits = c(0, 0.62)) +
    labs(x = '', y = 'Proportion') +
    theme(legend.position = 'bottom',
          panel.grid.major = element_line(linetype = 'dashed', linewidth = .3, color = "#EBEBEB"),
          panel.grid.minor = element_line(color =NA),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0))+theme_pdr;barplot3



#savtheme_light()#save as svg
ggsave("02_plots\\PDR_format\\Becca_Figure_4.svg",
       plot = barplot3,
       device = "svg",
       width = 4.83,   
       height = 4,     
       units = "in",
       bg = "white")

ggsave( "02_plots/BARPLOT_LA_NOVEMBER_IPUMS10.png", 
    plot=barplot3, scale = 1, bg='white',
    dpi = 500, height = 7, width = 10)











# ----------TRASH--------------------------------------------------------#
# other options

barplot3 <- ggplot() +
    geom_point(
        data = filter(la3, T1 == "Earliest sample"),
        aes(x = CNTRY_LABEL, y = values), shape=17,
        size = 3.1, col='#fdac42', alpha=.8) +
    geom_point(
        data = filter(la3, T1 == "Latest sample"),
        aes(x = CNTRY_LABEL, y = values), shape=16,
        size = 3.3, col='#8f2d56', alpha=.8) +
    geom_point(
        data = filter(la3, T1 == "Earliest sample"),
        aes(x = CNTRY_LABEL, y = values), shape=2,
        size = 3.3, col='#fdac42') +
    geom_point(
        data = filter(la3, T1 == "Latest sample"),
        aes(x = CNTRY_LABEL, y = values), shape=1,
        size = 3.5, col='#8f2d56') +
    theme_light(base_size = 15) +
    facet_wrap(~LA) +
    coord_flip() +
    scale_y_continuous( breaks = seq(0, 0.6, 0.2),
        limits = c(0, 0.62)) +
    labs(x = '',
        y = 'Proportion' ) +
    theme(legend.position = 'bottom',
        panel.grid.major = element_line(color = '#e9ecef'),
        panel.grid.minor = element_line(color = NA)); barplot3

ggsave( "02_plots/BARPLOT_LA_NOVEMBER_IPUMS7.png", 
    plot=barplot3, scale = 1, bg='white',
    dpi = 500, height = 7, width = 10)

barplot2 <- ggplot(la3, aes(x = CNTRY_LABEL, y = values, col = T1)) +
    geom_col( width = 0.7, position = position_dodge(width = 0.7), col=NA,
        alpha=.8, linewidth=.3) +
    geom_point(aes(x = CNTRY_LABEL, y = values), shape=16,
        size = 3.2, alpha=.6, show.legend = T)+
    geom_point(aes(x = CNTRY_LABEL, y = values),shape=1,
        size = 3.3, alpha=1, , show.legend = F)+
    theme_light(base_size = 15) +
    facet_wrap(~LA) +
    coord_flip() +
    # scale_shape_manual(values=c(1,2))+
    scale_color_manual(values = c("#cdb4db", '#0077B6'),
        name = '') +
    scale_y_continuous( breaks = seq(0, 0.6, 0.2),
        limits = c(0, 0.62)) +
    labs(x = '',
        y = 'Proportion' ) +
    theme(legend.position = 'bottom',
        panel.grid.major = element_line(color = '#e9ecef'),
        panel.grid.minor = element_line(color = '#e9ecef')); barplot2



barplot3 <- ggplot() +
    geom_point(
        data = filter(la3, T1 == "Earliest sample"),
        aes(x = CNTRY_LABEL, y = values), shape=17,
        size = 3.1, col='#fdac42', alpha=.8) +
    geom_point(
        data = filter(la3, T1 == "Latest sample"),
        aes(x = CNTRY_LABEL, y = values), shape=16,
        size = 3.3, col='#8f2d56', alpha=.8) +
    geom_point(
        data = filter(la3, T1 == "Earliest sample"),
        aes(x = CNTRY_LABEL, y = values), shape=2,
        size = 3.3, col='#fdac42') +
    geom_point(
        data = filter(la3, T1 == "Latest sample"),
        aes(x = CNTRY_LABEL, y = values), shape=1,
        size = 3.5, col='#8f2d56') +
    theme_light(base_size = 15) +
    facet_wrap(~LA) +
    coord_flip() +
    scale_y_continuous( breaks = seq(0, 0.6, 0.2),
        limits = c(0, 0.62)) +
    labs(x = '',
        y = 'Proportion' ) +
    theme(legend.position = 'bottom',
        panel.grid.major = element_line(color = '#e9ecef'),
        panel.grid.minor = element_line(color = NA)); barplot3


ggsave( "02_plots/BARPLOT_LA_NOVEMBER_IPUMS7.png", 
    plot=barplot3, scale = 1, bg='white',
    dpi = 500, height = 7, width = 10)


# with bars
# BARBLOT WITH BARS 

la3 <- la3 |>
    mutate(T1 = fct_rev(T1)) 

# Plot
barplot2 <- ggplot(la3, aes(x = CNTRY_LABEL, y = values, fill = T1)) +
    geom_col( width = 0.7, position = position_dodge(width = 0.7), col=NA,
        alpha=.8, linewidth=.3) +
    theme_light(base_size = 15) +
    facet_wrap(~LA) +
    coord_flip() +
    scale_fill_manual(values = c("#cdb4db", '#0077B6'),
    # scale_fill_manual(values = c( "#fcca46", '#eb5e28'),
    #                       breaks = rev(levels(la3$T1)),  # Reverse legend order
    #                       name = '') +
    #         scale_fill_manual(values = c('#e76f51', '#2a9d8f'),
                breaks = rev(levels(la3$T1)),  # Reverse legend order
                name = '') +
                scale_y_continuous( breaks = seq(0, 0.6, 0.2),
                    limits = c(0, 0.62)) +
                labs(x = '',
                    y = 'Proportion' ) +
                theme(legend.position = 'bottom',
                    panel.grid.major = element_line(color = '#e9ecef'),
                    panel.grid.minor = element_line(color = '#e9ecef')); barplot2
            
            ggsave( "02_plots/BARPLOT_LA_NOVEMBER_IPUMS6.png", 
                plot=barplot2, scale = 1, bg='white',
                dpi = 500, height = 7, width = 10)
