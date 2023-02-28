# GESTION DE L'ENVIRONNEMENT ----



token  = yaml::read_yaml("secrets.yaml")$api_token

source("script/functions.R", encoding = "UTF-8")


# IMPORT DES DONNEES ----


df <- arrow::read_parquet(
  "individu_reg.parquet",
  col_select = c("region", "aemm", "aged", "anai", "catl", "cs1", "cs2", "cs3", "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp", "trans", "ur")
)


# RETRAITEMENT DES DONNEES


df <- df %>%
  mutate(aged = as.numeric(aged))

df$sexe <- df$sexe %>%
  as.character() %>%
  forcats::fct_recode(Homme = "1", Femme = "2")


df3 <- df %>%
  select(surf, cs1, ur, couple, aged) %>%
  filter(surf != "Z")
df3[, 1] <- factor(df3$surf, ordered = TRUE)
df3[, "cs1"] <- factor(df3$cs1)
df3 %>%
  filter(couple == "2" & aged > 40 & aged < 60)



# STATISTIQUES DESCRIPTIVES ----


summarise(group_by(df, aged), n())

stat_des(df %>% filter(sexe == "Homme") %>% pull(aged))
stat_des(df %>% filter(sexe == "Femme") %>% pull(aged))



stats_age <- df %>%
  group_by(decennie = decennie_a_partir_annee(aged)) %>%
  summarise(n())

table_age <- gt::gt(stats_age) %>%
  gt::tab_header(
    title = "Distribution des âges dans notre population"
  ) %>%
  gt::fmt_number(
    columns = `n()`,
    sep_mark = " ",
    decimals = 0
  ) %>%
  gt::cols_label(
    decennie = "Tranche d'âge",
    `n()` = "Population"
  )

# GRAPHIQUES ----


ggplot(df) +
  geom_histogram(aes(x = 5 * floor(as.numeric(aged) / 5)), stat = "count")

p <- # part d'homme dans chaque cohort
  df %>%
  group_by(aged, sexe) %>%
  summarise(SH_sexe = n()) %>%
  group_by(aged) %>%
  mutate(SH_sexe = SH_sexe / sum(SH_sexe)) %>%
  filter(sexe == 1) %>%
  ggplot() +
  geom_bar(aes(x = aged, y = SH_sexe), stat = "identity") +
  geom_point(aes(x = aged, y = SH_sexe), stat = "identity", color = "red") +
  coord_cartesian(c(0, 100))

ggsave("p.png", p)



# MODELISATION ----

MASS::polr(surf ~ cs1 + factor(ur), df3)
