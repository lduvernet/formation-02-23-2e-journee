library(ggplot2)
library(dplyr)


decennie_a_partir_annee <- function(annee) {
  return(annee - annee %% 10)
}


#' Statistiques descriptives
#' 
#' @description
#' `stat_des` renvoie une statistique descriptive des données
#'
#' @details
#' L'argument `donnees` doit contenir un vecteur de données.
#' L'argument `stat` peut être "moyenne" (par défaut), "ecart-type" ou "variance"
#' 
#' @param donnees Un vecteur.
#' @param stat Un nom de fonction.
#' @return Le nombre résultat de la fonction appliquée au vecteur.
#'  
#' @examples
#' stat_des(rnorm(100))
#' stat_des(rpois(100, lambda = 2), stat = "variance")
stat_des <- function(donnees, stat = "moyenne", ...) {
  if (stat == "moyenne") {
    res <- mean(donnees, na.rm = TRUE, ...)
  } else if (stat == "ecart-type" || stat == "sd") {
    res <- sd(donnees, na.rm = TRUE, ...)
  } else if (stat == "variance") {
    res <- var(donnees, na.rm = TRUE, ...)
  }
  return(res)
}

read_yaml_secret <- function(path, key) {
  return(yaml::read_yaml(path)[[key]])
}
read_from_parquet <- function(path) {
  df <- arrow::read_parquet(
    path,
    col_select  = c(
      "region", "aemm", "aged", "anai", "catl", "cs1", "cs2", "cs3",
      "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp",
      "trans", "ur"
    )
  )
  return(df)
}