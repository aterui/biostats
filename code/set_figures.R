
# setup -------------------------------------------------------------------

pacman::p_load(tidyverse,
               igraph,
               ggraph)


# dendrogram --------------------------------------------------------------

## base adjacency matrix
x <- matrix(0, 11, 11)
for (i in 1:5) {
  x[i, (i * 2) :(i * 2 + 1)] <- 1
}
x <- x + t(x)

graph <- graph.adjacency(x, mode = "undirected")
E(graph)$weight <- rep(c("yes", "no"), 5)
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
  geom_edge_link(aes(color = weight)) +
  geom_node_label(aes(label = label),
                  label.padding = unit(0.5, "lines"),
                  fill = grey(0.95),
                  label.r = unit(0.75, "lines")) +
  scale_edge_color_manual(values = c("yes" = "black", "no" = "gray")) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(edge_color = "Answer")
