#Problema
#Unos grandes almacenes encargan a un fabricante pantalones y chamarras 
#deportivas. El fabricante dispone para la confección de 950 m de tejido de algodón 
#y 1320 m de tejido de poliéster. Cada pantalón precisa 1.8 m de algodón y 2.9 m 
#de poliéster. Para cada chaqueta se necesitan 2.3 m de algodón y 1.9 m de poliéster. 
#El precio del pantalón se fija en $350  y el de la chamarra en $700. 
#¿Qué número de pantalones y chamarras debe suministrar el fabricante a los 
#almacenes para que estos consigan un beneficio máximo? “

# Resolución del problema de programación líneal a través del método gráfico

#Paso 1: Identificar las variables básicas
#𝑥1= Cantidad de pantalones a producirse
#𝑥2= Cantidad de chamarras a producirse 

#Paso 2: Identificar las variables “𝒄𝒋”
#𝑐1= $ 350/pantalón
#𝑐2= $ 700/chamarra

#Paso 3: función objetivo
#𝑀𝑎𝑥 𝑧 = ($350/pantalón)(𝑥1pantalon) + ($700/chamarra)((𝑥2chamarra)
#𝑀𝑎𝑥 𝑧 = 350𝑥1 + 700𝑥2 

#Paso 4: Identificar “𝒃𝒋”
#b1= Cantidad de algodón disponible(950m)
#b2= 𝑏1= Cantidad de poliéster disponible(1320m)

#Paso 5: Identificar “𝒂𝒊𝒋”
#𝑏1= Cantidad de algodón disponible(950m)
#*1.8 para producir c/pantalón
#*2.3 para producir c/chamarra
#𝑏2= Cantidad de poliéster disponible(1320m)
#*2.9 para producir c/pantalón
#*1.9 para producir c/chamarra


#Paso 6: Armar restricciones
#(1.8m/pantalón)(𝑋1pantalon)+(2.3m/chaqueta)(𝑋2chaqueta)
#1.8𝑋1 + 2.3𝑋2 ≤ 950
#(2.9m/pantalón)(𝑋1pantalon)+(1.9m/chaqueta)(𝑋2chaqueta)
#2.9𝑋1 + 1.9𝑋2 ≤ 1320


#Paso 7: No negatividad
#𝑥1, 𝑥2 ≥ 0

#Paso 8: Modelo general
#𝑀𝑎𝑥 𝑧 = 350𝑥1 + 700𝑥2  -->	Ganancia
#1.8𝑥1 + 2.3𝑥2 ≤ 950      --> Cantidad de algodón que se utiliza para producir
#2.9𝑋1 + 1.9𝑋2 ≤ 1320    --> Cantidad de poliéster que se utiliza para producir
#𝑥1, 𝑥2 ≥ 0

# Paso 9 Gráficar el sistema.


# Importar la libreria
library(matlib)

# Asignar los coeficientes de las restricciones
#1.8x1 + 2.3𝑥2 ≤ 950        1.8   2.3   (1) 
#2.9x1 + 1.9𝑋2 ≤ 1320       2.9   1.9   (2)

# Definir el número de columnas en 2 y el número de filas en 2
A<-matrix(c(1.8,2.9,2.3,1.9), ncol = 2, nrow = 2)

# Asignar los valores del lado derecho de las desiguldades
#1.8x1 + 2.3𝑥2 ≤ 950        1.8   2.3   (1) 
#2.9x1 + 1.9𝑋2 ≤ 1320       2.9   1.9   (2)

b<- c(950,1320)

# Se grafican las ecuaciones
plotEqn(A, b, xlim=c(0,800), labels=TRUE)

# Intersección A
#1.8x1 + 2.3𝑥2 ≤ 950        1.8   2.3   (1) 
#2.9x1 + 1.9𝑋2 ≤ 1320       2.9   1.9   (2)

# entonces las matrices A y B quedan como:
# [A=
#             1.8     2.3
#              2.9    1.9
# ;B=
#             950
#             1320
# ]

# Definiendo matricialmente las funciones dentro de R, quedaria como

A <- matrix(c(1.8,2.3,2.9,1.9), nrow = 2, ncol = 2, byrow = T)
A

B <- matrix(c(950,1320), nrow = 2, ncol = 1, byrow = F)
B

r <- solve(t(A)%*%A)%*%t(A)%*%B
r


#INTERCCECION B  restriccion 1 con el eje x2
#1.8x1 + 2.3𝑥2 ≤ 950
# x1            =0
#
# entonces las matrices A y B quedan como:
# [A=
#             1.8     2.3
#              1      0
# ;B=
#             950
#             0
# ]

# Definiendo matricialmente las funciones dentro de R, quedaria como

A <- matrix(c(1.8,2.3,1,0), nrow = 2, ncol = 2, byrow = T)
A

B <- matrix(c(950,0), nrow = 2, ncol = 1, byrow = F)
B

r <- solve(t(A)%*%A)%*%t(A)%*%B
r
#
#
#

#INTERCCECION C  restriccion 2 con el eje x1

#2.9x1 + 1.9𝑋2 ≤ 1320
#         x2    =0
#
# entonces las matrices A y B quedan como:
# [A=
#             2.9     1.9
#              0      1
# ;B=
#             1320
#             0
# ]

# Definiendo matricialmente las funciones dentro de R, quedaria como

A <- matrix(c(2.9,1.9,0,1), nrow = 2, ncol = 2, byrow = T)
A

B <- matrix(c(1320,0), nrow = 2, ncol = 1, byrow = F)
B

r <- solve(t(A)%*%A)%*%t(A)%*%B
r
#

# Para ello escribe el codigo como
val<-matrix(c( 378.7692,116.6154, -4.218847, 4.130435,455.1724, 0.0000), nrow=3, ncol = 2, byrow=T)
val

FO<-matrix(c(350,700), nrow=2, ncol=1)
# Mostrar la matriz FO
FO

r=val%*%FO
r

#