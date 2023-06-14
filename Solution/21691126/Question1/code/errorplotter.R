plotter1<- function(happy){

#wrangle data to only include countrr, region, lifeladder and healthy life expectancy
happy_plotdata1 <- happy %>%
    group_by(`Regional indicator`) %>%
    summarise_at(vars(c(`Ladder score`, upperwhisker, lowerwhisker)), ~median(.))

LE <- happy %>%
    group_by(`Regional indicator`) %>%
    summarise_at(vars(c(`Healthy life expectancy`)), ~median(.))

happy_plotdata1 <- cbind(happy_plotdata1, LE[,2])

order <- happy_plotdata1 %>% arrange(LE) %>% pull(`Regional indicator`)
dfplot <- happy_plotdata1 %>% plot_orderset(., Column = "Regional indicator", Order = order)

FinalPlot1 <- happy_plotdata1 %>%
    ggplot() +
    geom_point(aes(x = `Regional indicator`, y = `Ladder score`, color = `Regional indicator`)) +
    geom_errorbar(aes(x = `Regional indicator`,
                      ymin = lowerwhisker,
                      ymax = upperwhisker, color = `Regional indicator`)) +
    geom_text(aes(x = `Regional indicator`, y = upperwhisker, label = `Healthy life expectancy`), vjust =0 )+
    theme_bw() +
    labs(title = "Happiness Index", subtitle = "Some subtitle", caption = "Data source: World Happiness Index", x = "", y = "Happiness Score") +
    theme(legend.position = "top", legend.title = element_blank()) +
    theme(plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 12),
          axis.text.x = element_text(size = 8, angle = 90, hjust = 1))+
    guides(color = F)

FinalPlot1
}