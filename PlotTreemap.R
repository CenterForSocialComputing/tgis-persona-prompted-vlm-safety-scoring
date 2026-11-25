library(HistData)
library(treemap)
library(tidyverse)
library(RColorBrewer)
library(showtext)
library(treemapify)

font_add("Cons", "constan.ttf")
showtext_auto()

type_dict <- readxl::read_excel("type_dict.xlsx") %>%
  select(-ends_with("CN"))
type_odr <- unique(type_dict$Type)
subtype_odr <- unique(type_dict$Subtype)
df <- readxl::read_excel("explain.xlsx", sheet = 1) %>%
  select(-item, -role, -round, -score) %>%
  summarise_all(sum) %>%
  data.matrix() %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  mutate(Type = str_split_i(rowname, "\\(", 1),
         Type = str_trim(Type)) %>%
  select(-rowname) %>%
  rename(count = V1) %>%
  group_by(Type) %>%
  summarise(Count = sum(count)) %>%
  ungroup() %>%
  inner_join(type_dict, ., by = c("Key" = "Type")) %>%
  mutate(Type = factor(Type, levels = type_odr),
         Subtype = factor(Subtype, levels = subtype_odr)) %>%
  select(-Key)

palette <- c("#fca50a", "#e07c1c", "#c04a2c", "#a01c3c", "#7c0f5d", 
             "#56106e", "#3d1e7a", "#2a2a7a", "#1a3a8a", "#0a4a8a")
palette <- c("#fca50a", "#ea9415", "#d78420", "#c5732b", "#b26336", 
             "#a05242", "#8d424d", "#7b3158", "#682163", "#56106e")

treemap(df,
        index=c("Type","Subtype"),
        vSize="Count",
        vColor = "Type",
        type = "categorical",
        # formatting options:
        title = "",
        fontfamily.title = "Cons",
        fontfamily.labels = "Cons",
        fontfamily.legend = "Cons",
        palette = palette,
        align.labels=list(
          c("left", "top"), 
          c("right", "bottom")
        ),     
        border.col = "white",
        bg.labels = 255,
        position.legend = "none")

ggplot(df, aes(area=Count, fill=Type, label=Subtype, subgroup=Type))+
  geom_treemap(alpha = 0.6, color="white")+
  geom_treemap_text(place = "bottomright", size = 16, family = "Cons", color="white") +
  scale_fill_discrete(palette = palette) +
  labs(fill = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(family = "Cons", size = 14),
        legend.key.height = unit(0.2, "cm"),
        legend.margin = margin(0, 0, 0, 0, "cm"),
        legend.position = "bottom")

ggsave("./Figures/Treemap.png",
       width = 17.21,
       height = 12,
       units = "cm")
