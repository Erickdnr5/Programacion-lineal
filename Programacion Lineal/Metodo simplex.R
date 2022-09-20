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



# Paso 9 Resolver el sistema

#
install.packages("lpSolve")

# Importar la libreria
library(lpSolve)


# Se guardan en una matriz los coeficientes de la función objetivo
funcion_objetivo <- c(350, 700)
funcion_objetivo
# Asignar los coeficientes de las desigualdades
#1.8x1 + 2.3𝑥2 ≤ 950        1.8   2.3   (1) 
#2.9x1 + 1.9𝑋2 ≤ 1320       2.9   1.9   (2)

# Definir el número de filas en 2
restriciones_derecho <- matrix(c(1.8,2.3,2.9,1.9), nrow = 2, byrow = T)

# Se muestra el valor de la matriz
restriciones_derecho


# Se guarda en una matriz los simbolos de las desigualdades
restriccion.direccion <- c("<=", "<=", "<=")
restriccion.direccion
# Asignar los valores del lado derecho de las desiguldades
#1.8x1 + 2.3𝑥2 ≤ 950        950   (1) 
#2.9x1 + 1.9𝑋2 ≤ 1320       1320  (2)
lado_derecho.restriccion <- c(950, 1320)

lado_derecho.restriccion
# Se ejecuta la instrucción para mostrar el resultado que maximize la función
lp(direction = "max", objective.in = funcion_objetivo, const.mat =restriciones_derecho, const.dir = restriccion.direccion, const.rhs = lado_derecho.restriccion, int.vec = c(1, 2, 3, 4, 5), all.int = T)

# Se ejecuta la instrucción para mostrar los valores de x1 y x2.
lp(direction = "max", objective.in = funcion_objetivo, const.mat =restriciones_derecho, const.dir = restriccion.direccion, const.rhs = lado_derecho.restriccion, int.vec = c(1, 2, 3, 4, 5), all.int = T)$solution

