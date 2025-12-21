
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

df_algae <- tibble(biomass = round(b, 2),
                   unit_biomass = "mg_per_m2",
                   conductivity = round(cond, 2),
                   unit_cond = "ms")

write_csv(df_algae, "data_raw/data_algae.csv")

# garden plant density ----------------------------------------------------

set.seed(1)

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
  mutate(plot = as.numeric(factor(plot)),
         nitrate = (log(count + 1) - mean(count + 1)) / rnorm(nrow(.), 0.2, 0.05) + 40,
         nitrate = round(nitrate, 1))

saveRDS(df_garden, "data_raw/data_garden.rds")
saveRDS(df_garden_sub, "data_raw/data_garden_sub.rds")
write_csv(df_garden_count, "data_raw/data_garden_count.csv")


# fertilization rate ------------------------------------------------------

n_sample <- 100
b0 <- -8
b1 <- 0.35
x <- rpois(n = n_sample, lambda = 10 + rnorm(n_sample, 0, 3))
p <- boot::inv.logit(model.matrix(~x) %*% c(b0, b1))
y <- rbinom(n = n_sample, size = 30, prob = p)

df_fert <- dplyr::tibble(n_fertilized = y,
                         n_examined = 30,
                         density = x) %>% 
  mutate(ind_id = row_number()) %>% 
  relocate(ind_id)

df_fert %>% 
  ggplot(aes(x = density,
             y = n_fertilized)) +
  geom_point()

write_csv(df_fert, "data_raw/data_mussel.csv")


# offset ------------------------------------------------------------------

set.seed(123)
df_n <- tibble(area = runif(100, 5, 100),
               nitrate = runif(100, 0, 1) + 3/area) %>% 
  mutate(count = rpois(nrow(.), lambda = exp(-0.1 + 0.5 * nitrate) * area))

write_csv(df_n, "data_raw/data_offset.csv")

# negative binomial -------------------------------------------------------

set.seed(111)
df_tadpole <- tibble(aqveg = runif(100, 0, 1),
                     permanence = round(runif(100, 10, 50)),
                     tadpole = rnbinom(100, mu = exp(-0.5 + 0.05 * permanence), size = 0.5))

write_csv(df_tadpole, "data_raw/data_tadpole.csv")


# messy data --------------------------------------------------------------

messy <- data.frame(
  ID = 1:12,
  
  collector = c(
    "Akira", "akira", "Akira", "Akira",
    "john", "John", "John", "JoHn",
    "Akira", "akira", "Akira", "Akira"
  ),
  
  # Inconsistent capitalization, periods, and whitespace
  species = c(
    "Bluehead chub", "blueHead Chub ", " BLUEHEAD CHUB",
    "green sunfish", " Green SunFish", "green.sunfish",
    " creek chub ", "Creek.Chub", " creek   chub",
    "Gizzard shad", "gizzard Shad.", " gizzard.shad "
  ),
  
  # Mixed numeric formats, commas, text, units
  length_mm = c(
    "55 ", 58, 60, 62, 63, 64,
    "70,0", "72 mm", 80.0, 82, 90, 75
  ),
  
  # Mixed date formats
  sample_date = c(
    "2024-06-01", "2024/06/01", "2024/06/01",
    "June 4 2024", "2024.06.04", "2024-06-04",
    "2024-06-07", "2024-06-07", "07 Jun 2024",
    "2024-06-09", "2024-06-09", " 2024-06-09 "
  ),
  
  # Inconsistent yes/no coding
  recaptured = c(
    "yes", "Yes ", "Y", "no", " NO", "n",
    "yes", " y ", "No.", "N", " YES", " no "
  ),
  
  stringsAsFactors = FALSE
)

write_csv(messy, "data_raw/data_messy.csv")


# gam example -------------------------------------------------------------

# URLs pointing to the raw CSV files hosted on GitHub.
# These use the "raw.githubusercontent.com" domain so that R
# downloads the actual CSV content rather than an HTML webpage.
link_woody <- "https://raw.githubusercontent.com/aterui/public-proj_restore-aqua-complex/master/data_raw/data_src_w_temp/wetland_site1_woody_230321.csv"

link_open <- "https://raw.githubusercontent.com/aterui/public-proj_restore-aqua-complex/master/data_raw/data_src_w_temp/wetland_site2_open_230321.csv"

# Read the woody wetland dataset directly from GitHub.
# A new column, `site`, is added to label observations
# originating from the woody wetland.
df_wt_woody_raw <- read_csv(link_woody, 
                            guess_max = 1E+6) %>% 
  select(1:3) %>% 
  set_names(c("id", "date_time", "temp")) %>% 
  mutate(site = "woody")

# Read the open wetland dataset directly from GitHub.
# Similarly, add a `site` column to indicate the open wetland.
df_wt_open_raw <- read_csv(link_open, 
                           guess_max = 1E+6) %>% 
  select(1:3) %>% 
  set_names(c("id", "date_time", "temp")) %>% 
  mutate(site = "open")

# Combine the two datasets into a single data frame by stacking rows.
# This is possible because both datasets share the same column structure.
# The `site` column preserves information about the original data source.
df_wt_raw <- bind_rows(df_wt_woody_raw,
                       df_wt_open_raw)

write_csv(df_wt_raw,
          "data_raw/data_water_temp.csv")


# path analysis example ---------------------------------------------------

set.seed(123)  # ensure reproducibility

# Define means and SDs
v_mu <- c(1000, 5, 100, 10)
v_s0 <- v_mu * 0.01

# Build covariance matrix
m_vcov <- outer(v_s0, v_s0) * 0.3       # base correlations
diag(m_vcov) <- v_s0^2                  # variances on diagonal
m_vcov[2, 4] <- m_vcov[4, 2] <- (v_s0[2] * v_s0[4]) * 0.001  # small covariances
m_vcov[1, 4] <- m_vcov[4, 1] <- (v_s0[1] * v_s0[4]) * 0.001

# Simulate multivariate normal data
df_fw <- MASS::mvrnorm(n = 100, mu = v_mu, Sigma = m_vcov) %>% 
  as_tibble() %>% 
  set_names(nm = c("mass_plant",
                   "cv_h_plant",
                   "mass_herbiv",
                   "mass_pred")) %>% 
  mutate(plot_id = row_number(), .before = mass_plant)

# Specify SEM model
m <- '
  mass_herbiv ~ mass_plant + cv_h_plant
  mass_pred ~ mass_herbiv
'

# Fit SEM
(fit <- sem(model = m, data = df_fw))

write_csv(df_fw,
          file = "data_raw/data_foodweb.csv")


