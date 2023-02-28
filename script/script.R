# GESTION DE L'ENVIRONNEMENT ----


library(ggplot2)
library(dplyr)

api_token <- "trotskitueleski$1917"



# DEFINITION DES FONCTIONS ----


decennie_a_partir_annee <- function(annee) {
  return(annee - annee %% 10)
}

fonction_de_stat_agregee <- function(a, b = "moyenne", ...) {
  if (b == "moyenne") {
    x <- mean(a, na.rm = TRUE, ...)
  } else if (b == "ecart-type" || b == "sd") {
    x <- sd(a, na.rm = TRUE, ...)
  } else if (b == "variance") {
    x <- var(a, na.rm = TRUE, ...)
  }
  return(x)
}



# IMPORT DES DONNEES ----


df <- readr::read_csv2(
  "individu_reg.csv",
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

fonction_de_stat_agregee(df %>% filter(sexe == "Homme") %>% pull(aged))
fonction_de_stat_agregee(df %>% filter(sexe == "Femme") %>% pull(aged))



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
