# función tiene_nombre ----------------------------------------------------


tiene_nombre <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !is.na(nms) & nms != ""
  }
}
# Esta función aprovecha la regla de retorno estándar: 
# una función devuelve el último valor que calculó 

# Prueba 1: vector sin nombre  --------------------------------------------

vector1 <- c("a", "b", "c")
tiene_nombre(vector1)


# Prueba 2: se asignan nombres --------------------------------------------


names(vector1) <- c("ca", "cb", "cc")
names(vector1)

tiene_nombre(vector1)


# Prueba 3: no todos con nombre -------------------------------------------


names(vector1) <- names(vector1)[1:2]
tiene_nombre(vector1)



# for_1 dividiendo directamente -------------------------------

enteros <- seq(1500, 5000, by = 500)
numeros <- vector("double", length = length(enteros))


for (i in seq_along(enteros)){
  numeros[[i]] <- enteros[[i]] /1000
}


# for_2 usando la funcion miles -------------------------------------------

a_miles <- function(x) {
  x / 1000
}


for (i in seq_along(enteros)) {
  numeros[[i]] <- a_miles(enteros[[i]])
}


# for 3 usando dataframe con funcion a-miles ------------------------------


v1= seq(1500, 5000, by = 500)
v2 = seq(1000, 8000, by = 1000)
v3 = seq(7100, 7800, by = 100)

df_1 <- cbind(v1, v2, v3)

for (i in seq_along(df_1)){
  df_1[[i]] <- a_miles(df_1[[i]])
}
