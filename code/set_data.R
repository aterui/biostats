
# setup -------------------------------------------------------------------

source(here::here("code/library.R"))


# lake body size data -----------------------------------------------------

set.seed(1)
x <- rnorm(50, 13, sd = 3.5)
y <- rnorm(50, 15, sd = 3.5)

df_fish_length <- tibble(lake = rep(c("a", "b"), each = 50),
                         length = round(c(x, y), 1),
                         unit = "cm")

write_csv(df_fish_length,
          "data_raw/data_fish_length.csv")

# lake sample data --------------------------------------------------------

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

saveRDS(df_fish, "data_raw/data_lake_fish.rds")
saveRDS(df_fish_sub, "data_raw/data_lake_fish_sub.rds")
