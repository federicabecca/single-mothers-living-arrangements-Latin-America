# SAMPLE AVAILABILITY ---------------------------------------------------------
# load data ------------
load('00_data/DATA_FINAL_IPUMS_JULY.RData')

dfsm <- df_ipums

df <- dfsm

sample <- df |>  select(COUNTRY2, YEAR, SAMPLE, DECADE) |> distinct()

levels(sample$COUNTRY2)

library(forcats)
library(stringr)
sample$COUNTRY2 <- fct_rev(str_to_title(sample$COUNTRY2))

col <- c('#d81159', '#218380', '#73c2de','#fdac42', '#90be6d')

colblind <- c('#218380', '#73c2de', '#fdac42')

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

sam <- ggplot(sample, aes(YEAR, COUNTRY2, group = COUNTRY2))+
    geom_line(linewidth=.3)+
    geom_point(aes(colour = DECADE), size=1.8)+
    theme_light(base_size = 16)+
    scale_color_manual(values = col, name='')+
    scale_x_continuous(breaks=seq(1960,2020, 10), limits=c(1960,2020))+
    labs(x='', y='')+
    theme(legend.position = 'bottom')+ theme_pdr; sam

#save as svg
ggsave("02_plots\\PDR_format\\Becca_Figure_3.svg",
       plot = sam,
       device = "svg",
       width = 4.83,   
       height = 4,     
       units = "in",
       bg = "white")

ggsave( "02_plots/SAMPLE_AV_NOVEMBER.png", 
        plot=sam, scale = 1, bg='white',
        dpi = 500, height = 6, width = 8)

