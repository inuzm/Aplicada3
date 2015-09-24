imu <- read.table("pathto/Datos_IMU_Estatal.txt", head = TRUE)
# Se hace el análisis factorial con cinco factores, sin rotación y se piden los
# scores de Bartlett.
y <- factanal(imu[,2:11], factors = 5, rotation = "none", scores = "Bartlett")
m <- 5
N <- dim(imu[, 2:11])
k <- N[2]
# Se estandarizan las variables que se usaron.
imu_est <- scale(imu[,2:11], center = T, scale = T)

# Se obtiene la matriz de pesos de los loadings dados por 'factanal'.
A <- as.double(y$loadings)
A <- matrix(A, k, m)
# Se obtiene la matriz Psi y su inversa.
Psi <- diag(y$uniquenesses)
Psi_inv <- diag(1 / y$uniquenesses)
# Se crea una matriz vacía en la que se guardarán los scores.
Fact <- matrix(nrow = N[1], ncol = m)

# Se crea la matriz A'*Psi_inv*A.
C <- t(A) %*% Psi_inv %*% A
# En un for se calculan los scores de cada una de las 'muestras' resolviendo el
# sistema de ecuaciones que se probó.
for(j in 1:N[1]){
    b <- t(A) %*% Psi_inv %*% matrix(imu_est[j, ], k, 1)
    Fact[j, ] <- t(solve(C, b))
}
# Se imprimen los scores obtenidos por nosotros y el obtenido por 'factanal'.
Fact
y$scores
# Se revisa que en forma matricial ambas matrices sean iguales.
all.equal(Fact, matrix(y$scores, N[1], m))