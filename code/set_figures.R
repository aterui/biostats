
# setup -------------------------------------------------------------------

source(here::here("code/library.R"))

# fish density ------------------------------------------------------------

## base numbers
set.seed(10)

garden_size <- 15
n_plot <- 225

plot30 <- foreach(i = seq(1, 15, by = 2), .combine = c) %do% {
  one <- (i - 1) * 15 + 1
  x <- seq(one, (one + 15), by = 2) %>% sample(size = 4)
  
  return(x)
} %>% 
  sample(size = 30)

## data set
df_plot <- expand.grid(x0 = seq(0, 14, length = 15),
                       y0 = seq(0, 14, length = 15)) %>% 
  as_tibble() %>% 
  mutate(x1 = x0 + 1,
         y1 = y0 + 1)

## plot
df_garden_sub <- readRDS(here::here("data_raw/data_garden_sub.rds"))

g_garden <- df_garden_sub %>% 
  filter(plot %in% plot30) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_rect(aes(xmin = 0,
                xmax = garden_size,
                ymin = 0,
                ymax = garden_size),
            fill = "seagreen") +
  geom_rect(data = df_plot %>% slice(plot30),
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
  scale_x_continuous(limits = c(0, garden_size)) +
  scale_y_continuous(limits = c(0, garden_size)) +
  theme_minimal()

ggsave(g_garden,
       filename = "image/figure_garden.jpg",
       width = 4, height = 4)


# intercept and slope -----------------------------------------------------

x <- -5:5
y <- 5 + x

g_lm <- tibble(y = y, x = x) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  geom_vline(xintercept = 0,
             linetype = "dotted") +
  geom_segment(x = -0.75,
               xend = -0.1,
               y = 5,
               yend = 5,
               color = "steelblue") +
  geom_text(label = expression(alpha),
            x = -1,
            y = 5) +
  geom_segment(x = 2,
               xend = 3,
               y = 7,
               yend = 7,
               color = "steelblue") +
  geom_segment(x = 3,
               xend = 3,
               y = 7,
               yend = 8,
               color = "steelblue") +
  geom_text(label = expression(beta),
            x = 3.25,
            y = 7.5) +
  geom_text(label = "1",
            x = 2.5,
            y = 6.5) +
  theme_bw()

ggsave(g_lm,
       height = 2.5, width = 3.5,
       filename = here::here("image/figure_lm.png"))

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
                    "Is there an upper bound?",
                    "Is there an upper bound?",
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
       filename = here::here("image/figure_dendro.jpg"))
