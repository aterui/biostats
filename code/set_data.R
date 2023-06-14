
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


# lake body size data II --------------------------------------------------

set.seed(1)
x <- rnorm(50, 13, sd = 3.5)
y <- rnorm(50, 15, sd = 3.5)
z <- rnorm(50, 15, sd = 3.5)

df_fish_anova <- tibble(lake = rep(c("a", "b", "c"), each = 50),
                        length = round(c(x, y, z), 1),
                        unit = "cm")

write_csv(df_fish_anova,
          "data_raw/data_fish_length_anova.csv")


# lm example --------------------------------------------------------------

set.seed(1)

cond <- runif(50, 5, 100)
b <- rnorm(n = length(cond),
           mean = 5 + 0.5 * cond,
           sd = 5)

df_algae <- tibble(biomass = b,
                   conductivity = cond)

write_csv(df_algae, "data_raw/data_algae.csv")

# garden plant density ----------------------------------------------------

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

df_garden <- tibble(x = runif(500, 0, garden_size),
                    y = runif(500, 0, garden_size))


df_garden_sub <- foreach(i = seq_len(nrow(df_plot)),
                         .combine = bind_rows) %do% {
                           df_garden %>% 
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

df_garden_count <- df_garden_sub %>% 
  group_by(plot) %>% 
  summarize(count = unique(count)) %>% 
  filter(plot %in% plot30) %>% 
  mutate(plot = as.numeric(factor(plot)))

saveRDS(df_garden, "data_raw/data_garden.rds")
saveRDS(df_garden_sub, "data_raw/data_garden_sub.rds")
write_csv(df_garden_count, "data_raw/data_garden_count.csv")
