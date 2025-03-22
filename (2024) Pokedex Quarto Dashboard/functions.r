lollipop_visualization <- function(df) {
  p <- df |> 
    mutate(
      Avg = mean(Total),
      Above = as.factor(if_else(Total > Avg, TRUE, FALSE)),
      name = forcats::fct_reorder(name, Total)
    ) |> 
    ggplot(aes(x = Total, y=name, color = Above)) +
    geom_segment(
      aes(x = Avg, y = name, xend = Total, yend = name), 
      color = "grey50"
    ) +
    geom_point() +
    #scale_x_continuous(sec.axis = dup_axis()) +
    labs(x = "Base Stat Total (BST)") +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white'))
  
  return(ggplotly(p))
}