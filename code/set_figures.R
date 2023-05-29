
# setup -------------------------------------------------------------------

source(here::here("code/library.R"))

# fish density ------------------------------------------------------------

## base numbers
set.seed(10)

lake_size <- 15
n_plot <- 225

plot10 <- foreach(i = seq(1, 15, by = 2), .combine = c) %do% {
  one <- (i - 1) * 15 + 1
  x <- seq(one, (one + 15), by = 2) %>% sample(size = 4)
  
  return(x)
} %>% 
  sample(size = 10)

## data set
df_plot <- expand.grid(x0 = seq(0, 14, length = 15),
                       y0 = seq(0, 14, length = 15)) %>% 
  as_tibble() %>% 
  mutate(x1 = x0 + 1,
         y1 = y0 + 1)

df_fish <- tibble(x = runif(500, 0, lake_size),
                  y = runif(500, 0, lake_size))


df_fish_sub <- foreach(i = seq_len(nrow(df_plot)),
                       .combine = bind_rows) %do% {
                         df_fish %>% 
                           filter(x > df_plot$x0[i],
                                  x < df_plot$x1[i],
                                  y > df_plot$y0[i],
                                  y < df_plot$y1[i]) %>% 
                           mutate(plot = i,
                                  count = nrow(.))
                       } %>% 
  right_join(tibble(plot = 1:n_plot),
             by = "plot") %>% 
  mutate(count = replace_na(count, replace = 0))

## plot
g_lake <- df_fish_sub %>% 
  filter(plot %in% plot10) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_rect(aes(xmin = 0,
                xmax = lake_size,
                ymin = 0,
                ymax = lake_size),
            fill = "steelblue") +
  geom_rect(data = df_plot %>% slice(plot10),
            aes(x = x0,
                y = y0,
                xmin = x0,
                xmax = x1,
                ymin = y0,
                ymax = y1),
            color = "white",
            fill = NA,
            linewidth = 0.1) +
  geom_point(size = 0.5,
             color = "salmon") +
  scale_x_continuous(limits = c(0, lake_size)) +
  scale_y_continuous(limits = c(0, lake_size)) +
  theme_minimal()

ggsave(g_lake,
       filename = "image/figure_lake.pdf",
       width = 5, height = 5)


# dendrogram for probability distributions --------------------------------

## base adjacency matrix
x <- matrix(0, 11, 11)
for (i in 1:5) {
  x[i, (i * 2) :(i * 2 + 1)] <- 1
}
x <- x + t(x)

graph <- graph.adjacency(x, mode = "undirected")
E(graph)$weight <- rep(c("Yes", "No"), 5)
V(graph)$label <- c("Is the variable discrete?",
                    "Are there upper/lower limits?",
                    "Are there upper/lower limits?",
                    "Variance >> Mean?",
                    "Variance >> Mean?",
                    "5",
                    "6",
                    "1",
                    "2",
                    "3",
                    "4")

g_dendro <- graph %>% 
  ggraph(layout = layout_as_tree(.,
                                 flip.y = TRUE,
                                 root = 1)) +
  geom_edge_link(aes(label = weight),
                 color = "gray") +
  geom_node_label(aes(label = label),
                  label.padding = unit(0.5, "lines"),
                  fill = grey(0.95),
                  label.r = unit(0.75, "lines")) +
  theme_void() +
  theme(legend.position = "bottom") +
  guides(color = "none")

ggsave(g_dendro,
       height = 8, width = 10,
       filename = here::here("image/figure_dendro.pdf"))
