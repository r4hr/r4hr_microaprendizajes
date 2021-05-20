
df <- tibble::tibble(
  a = rnorm(10, 5, 1),
  b = rnorm(10, 5, 1),
  c = rnorm(10, 5, 1),
  d = rnorm(10, 5, 1)
)

range(df$a)

#quiero llevar los valores entre 0, 1
df1 <- df

df1$a <- (df$a - min(df$a, na.rm = TRUE)) /
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df1$b <- (df$b - min(df$b, na.rm = TRUE)) /
  (max(df$b, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df1$c <- (df$c - min(df$c, na.rm = TRUE)) /
  (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df1$d <- (df$d - min(df$d, na.rm = TRUE)) /
  (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

for (i in seq_along(df)) {
df[[i]] <- rescale01(df[[i]])
} 


# calcula media de cada columa de df --------------------------------------

output <- vector("double", ncol(df))  # 1. output
for (i in seq_along(df)) {            # 2. secuencia
  output[[i]] <- median(df[[i]])      # 3. cuerpo
}
output


# imprime columnas de df --------------------------------------------------

#resultados <- vector("list", length(df))
#names(resultados) <- names(df)

columnas <- names(df)
for (i in columnas) {
  print(df[, i])
}
# map de paquete purrr ----------------------------------------------------


a <- map_dbl(df, mean)