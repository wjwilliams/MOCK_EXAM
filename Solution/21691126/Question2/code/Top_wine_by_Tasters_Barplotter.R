top_wines_by_tasters <- function(winedf, Country = "South Africa", Tasters = c("Lauren Buzzeo", "Susan Kostrzewa") , Price = 20){
    top_wineries <- winedf %>%
        filter(taster_name %in% Tasters,
               country == Country ,
               price > Price) %>%
        ungroup() %>%
        group_by(taster_name, winery) %>%
        summarise_at(vars(points, price), ~median(.)) %>%
        ungroup() %>%
        group_by(taster_name) %>%
        arrange(points) %>%
        group_by(taster_name) %>%
        slice_head(n = 5) %>%
        ungroup()

    top_wineries <- top_wineries %>%
        group_by(taster_name) %>%
        arrange(desc(points)) %>%
        ungroup()
    order <- top_wineries$winery


    barplot <- top_wineries %>%
        plot_orderset(., Column = "winery", Order = order) %>%
        ggplot() +
        geom_bar(aes(x = winery, y = points, fill = taster_name), stat = "identity") +
        facet_wrap(~taster_name, scales = "free") +
        theme_bw() +
        geom_text(aes(winery, y = points, label = glue::glue("${price}")), vjust = 2.5)+
        labs(title = "Top Wineries by Taster", subtitle = "with average price",
             x = "Winery", y = "Points") +
        theme(legend.position = "top") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(x = "", y = "") + guides(fill = F)

    barplot

}