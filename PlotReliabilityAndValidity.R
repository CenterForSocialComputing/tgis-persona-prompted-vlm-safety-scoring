library(tidyverse)
library(showtext)
library(ggpubr)
library(ggridges)
library(readxl)
library(patchwork)
library(ggh4x)

font_add("Cons", "constan.ttf")
showtext_auto()

windowsFonts(
  Cons = windowsFont(family = "Constantia")
)


vlm_reliability <- read_csv("vlm_reliability.csv") %>%
  select(item, role, diff)

all_diff <- vlm_reliability %>%
  mutate(role = "all")

roles <- c("all", "male", "female", "local", "non-local", "issue", "non-issue")
rolelabs <- c("In Total", "Male", "Female", "Local", "Non-local", "Issue Experience(s)", "No Issue Experiences")
merged_data <- rbind(vlm_reliability, all_diff) %>%
  mutate(role = factor(role, levels = rev(roles), labels = rev(rolelabs)))


p_reliability <- ggplot(merged_data, aes(diff, role)) +
  geom_density_ridges(alpha = 0.2, fill = "#fca50a", color = "#56106e", linewidth = 0.3) +
  scale_x_continuous(limits = c(0, 11), breaks = c(0, 2, 4, 6, 8, 10)) +
  labs(
    x = "Score Differences between Two Rounds",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.ticks.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Cons",
                            face = "bold",
                            size = 24),
        axis.text.x = element_text(family = "Cons"))

ggsave("./Figures/Reliability.png",
       p_reliability,
       width = 17.21,
       height = 6,
       units = "cm")

vlm_score <- read_csv("vlm_score.csv") %>%
  rename(vlm_score = score)

fe_score <- rbind(
  read_excel("FE.xlsx", sheet = "male") %>% mutate(role = "male"),
  read_excel("FE.xlsx", sheet = "female") %>% mutate(role = "female"),
  read_excel("FE.xlsx", sheet = "local") %>% mutate(role = "local"),
  read_excel("FE.xlsx", sheet = "non-local") %>% mutate(role = "non-local"),
  read_excel("FE.xlsx", sheet = "issue") %>% mutate(role = "issue"),
  read_excel("FE.xlsx", sheet = "non-issue") %>% mutate(role = "non-issue")
) %>%
  select(item, role, score) %>%
  rename(fe_score = score)

mean_score <- read_excel("Data/Human/ScoresByGroups.xlsx", sheet = "mean") %>%
  select(id, male, female, local, nonlocal, issue, noissue) %>%
  pivot_longer(cols = -id, names_to = "role", values_to = "mean_score") %>%
  rename(item = id) %>%
  mutate(role = case_when(role == "nonlocal" ~ "non-local",
                          role == "noissue" ~ "non-issue",
                          .default = role))

mid_score <- read_excel("Data/Human/ScoresByGroups.xlsx", sheet = "median") %>%
  select(id, male, female, local, nonlocal, issue, noissue) %>%
  pivot_longer(cols = -id, names_to = "role", values_to = "median_score") %>%
  rename(item = id) %>%
  mutate(role = case_when(role == "nonlocal" ~ "non-local",
                          role == "noissue" ~ "non-issue",
                          .default = role))

all_score <- vlm_score %>%
  inner_join(fe_score, by = c("item", "role")) %>%
  inner_join(mean_score, by = c("item", "role")) %>%
  inner_join(mid_score, by = c("item", "role"))

cor.test(all_score$vlm_score, all_score$fe_score)
cor.test(all_score$vlm_score, all_score$mean_score)
cor.test(all_score$vlm_score, all_score$median_score)

cor.results <- tibble(
  dataset = character(0),
  role = character(0),
  cor_coef = numeric(0),
  ci_lower = numeric(0),
  ci_upper = numeric(0),
  .rows = 0
)

roles <- c("all", unique(all_score$role))
datasets <- c("fe", "mean", "median")

for (dataset in datasets) {
  variable <- paste0(dataset, "_score")
  for (cur_role in roles) {
    if (cur_role == "all") {
      cor.res <- cor.test(all_score[["vlm_score"]], all_score[[variable]])
    }
    else {
      sub_score <- filter(all_score, role == cur_role)
      cor.res <- cor.test(sub_score[["vlm_score"]], sub_score[[variable]])
    }
    cor.results <- cor.results %>%
      add_row(
        dataset = dataset,
        role = cur_role,
        cor_coef = cor.res[["estimate"]][["cor"]],
        ci_lower = cor.res[["conf.int"]][1],
        ci_upper = cor.res[["conf.int"]][2]
      )
  }
}

cor.pltdata <- cor.results %>%
  filter(str_count(role, "\\*") == 0) %>%
  mutate(role = case_when(role == "all" ~ "In Total",
                          role == "male" ~ "Male",
                          role == "female" ~ "Female",
                          role == "non-local" ~ "Non-local",
                          role == "local" ~ "Local",
                          role == "issue" ~ "Issue Experience(s)",
                          role == "non-issue" ~ "No Issue Experiences",
                          .default = variable),
         role = factor(role, levels = c("In Total",
                                        "Male",
                                        "Female",
                                        "Local",
                                        "Non-local",
                                        "Issue Experience(s)",
                                        "No Issue Experiences")),
         y = as.integer(fct_rev(role)),
         dataset = case_when(dataset == "fe" ~ "Fixed Effects",
                             dataset == "mean" ~ "Mean Value",
                             dataset == "median" ~ "Medium Value"))

p_validity <- ggplot(cor.pltdata) +
  geom_segment(aes(x = ci_lower,
                   xend = ci_upper,
                   y = y,
                   yend = y),
               color = "#fca50a",
               #alpha = 0.5,
               linewidth = 0.3,
               linetype = 1) +
  geom_point(aes(x = cor_coef,
                 y = y),
             color = "#56106e",
             alpha = 0.5,
             size = 1.2) +
  geom_text(aes(x = cor_coef,
                y = y + 0.25,
                label = round(cor_coef, 3)),
            size = 6,
            family = "Cons") +
  facet_grid(. ~ dataset) +
  scale_x_continuous(limits = c(0.5, 0.8), breaks = c(0.5, 0.6, 0.7, 0.8)) +
  scale_y_continuous(breaks = 1:7,
                     labels = levels(fct_rev(cor.pltdata[["role"]]))) +
  labs(x = "Correlation Coefficients with Confidence Intervals",
       y = NULL) +
  theme_minimal() +
  theme(axis.ticks.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Cons",
                            face = "bold",
                            size = 24),
        axis.text.x = element_text(family = "Cons"))

ggsave("./Figures/Validity.png",
       p_validity,
       width = 17.21,
       height = 6,
       units = "cm")

p_reliability / p_validity

ggsave("./Figures/Reliability+Validity.png",
       p_reliability / p_validity,
       width = 17.21,
       height = 12,
       units = "cm")
