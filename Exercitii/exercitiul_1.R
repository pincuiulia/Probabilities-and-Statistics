#LIBRARII

install.packages("ggplot2")
library(ggplot2)

install.packages("knitr")
library(knitr)



# a
install.packages("rje")
library(rje)

frepcomgen <- function(n, m) {
  # Generam o matrice cu repartiția comuna
  matr <- matrix(runif(n * m), n, m)

  # Normalizam matricea pentru a avea sume de 1 pe randuri și coloane
  matr <- matr / sum(matr)

  # Calculam repartitiile marginale
  rep_mg_x <- apply(matr, 1, sum)
  rep_mg_y <- apply(matr, 2, sum)

  # Adaugam repartițiile marginale la matrice
  matr <- cbind(matr, rep_mg_x)
  matr <- rbind(matr, c(rep_mg_y, sum(rep_mg_y)))

  # Adaugam valorile pentru variabilele aleatoare X și Y
  va_x_val <- c(sample(-100:100, n, FALSE), 0)
  va_y_val <- c(0, sample(-100:100, m, FALSE), 0)
  matr <- cbind(va_x_val, matr)
  matr <- rbind(va_y_val, matr)


  # Plasam o singură valoare NA pe fiecare linie
  for (i in 2:(n + 1)) {
    coloana_NA <- sample(2:m+1, 1)
    matr[i, coloana_NA] <- NA
  }
  return(matr)
}


# Exemplu de utilizare
n <- 3
m <- 3
print(frepcomgen(n, m))



# b
# consid matricea de forma:
#  0  vy1 vy2 ... vyn  0
# vx1                 p1
# vx2      rep        p2
# ...    comuna       ...
# vxm                 pm
#  0   q1  q2 ...  qn  1
# m=2, n=3
#  0  vy1 vy2 0
# vx1        px1
# vx2        px2
# vx3        px3
#  0 py1 py2  1
fcomplrepcom <- function(matr) {
  nr <- nrow(matr)
  nc <- ncol(matr)

  for (i in 2:(nr - 1)) {
    for (j in 2:(nc - 1)) {
      if (is.na(matr[i, j])) {
        # Suma valorilor pe linia respectiva, excluzand prima coloana și repartiția marginala
        suma_linie <- sum(matr[i, 2:(nc - 1)], na.rm = TRUE)

        # Valoarea completata este repartiția marginala a liniei minus suma valorilor de pe linie
        matr[i, j] <- matr[i, nc] - suma_linie
      }
    }
  }
  return(matr)
}

# Exemplu de utilizare
matr_ini <- frepcomgen(n, m)
print(matr_ini)
matr_compl <- fcomplrepcom(matr_ini)
print(matr_compl)



# c
frepmarginal <- function(matr, n, m) {
  marg_x <- numeric(n)
  marg_y <- numeric(m)

  # calculcam repartitiile marginale pentru X
  for (i in 2:(n + 1)) {
    marg_x[i - 1] <- matr[i, m + 2]
  }

  # calculam repartitiile marginale pentru Y
  for (j in 2:(m + 1)) {
    marg_y[j - 1] <- matr[n + 2, j]
  }

  # Afișam repartitiile marginale in format de perechi
  print("Repartiția marginală X:")
  for (i in 1:n) {
    print(paste(matr[1, i + 1], ":", marg_x[i]))
  }
  print(" ")
  print("Repartiția marginală Y:")
  for (j in 1:m) {
    print(paste(matr[j + 1, 1], ":", marg_y[j]))
  }
}



frepmarginal (matr_compl,n,m)
print(matr_compl)


# d
# calculeaza media unei variabile aleatoare
fmedie <- function(var){
  medie <- 0
  # parcurge toate valorile și calculeaza suma ponderată
  for(i in 1:dim(var)[2])
    medie <- medie + (var[1,i] * var[2,i])
  return (medie)
}

# calculează varianța unei variabile aleatoare
fvar <- function(X, b=1){
  # ridica la pătrat valorile variabilei aleatoare
  Xpatrat <- matrix(c(X[1,]^2, X[2,]), nrow=2, byrow = TRUE)
  # returneaza varianța ajustată cu factorul b
  return (b * b * (fmedie(Xpatrat) - fmedie(X) ^ 2))
}

# calculeaza covarianta pentru combinații liniare ale variabilelor aleatoare
fpropcov <- function(matr, a=1, b=0, c=0, d=1){
  # Inițializează vectorii pentru valori și probabilități
  val_var <- c()
  prob_var <- c()

  # extragerea valorilor și probabilităților pentru X
  for(i in 2:dim(matr)[1]-1){
    val_var <- c(val_var, matr[i,1])
    prob_var <- c(prob_var, matr[i,dim(matr)[2]])
  }
  X <-  matrix(c(val_var, prob_var), nrow=2, byrow = TRUE)

  # Resetare vectori pentru Y
  val_var <- c()
  prob_var <- c()

  # extragerea valorilor și probabilităților pentru Y
  for(i in 2:dim(matr)[2]-1){
    val_var <- c(val_var, matr[1,i])
    prob_var <- c(prob_var, matr[dim(matr)[1],i])
  }
  Y <-  matrix(c(val_var, prob_var), nrow=2, byrow = TRUE)

  # Verifică dacă X sau Y sunt constante
  if(length(Y) == 1 || length(X) == 1) return (0)

  # Calculul covarianței pentru combinații liniare
  for(i in 1:dim(X)[2])
    for(j in 1:dim(Y)[2]){
      val_var <- c(val_var, X[1,i] * Y[1,j])
      prob_var <- c(prob_var, X[2,i] * Y[2,j])
    }
  prob_var <- tapply(prob_var, val_var, sum)
  XY <- matrix(c(sort(unique(val_var)), prob_var), nrow=2, byrow = TRUE)
  cova <- fmedie(XY)
  covb <- fmedie(X) * fmedie(Y)
  covXY <- trunc((cova - covb) * 10^4) / 10^4
  cov <- a * c * fvar(X) + (a * d + b * c) * covXY + b * d * fvar(Y)
  return (cov)
}

# Exemplu de utilizare
fpropcov(matr_compl, 1, 2, 1, 2)

print(matr_compl)


# e

fPcond <- function(matr, intX=NULL, compX=NULL, intY=NULL, compY=NULL){
  # Calculul P(X|Y) dacă X nu este specificat
  if(is.null(intX)) {
    val_var <- c()  # Initializează vectorul pentru valorile lui X
    prob_var <- c()  # Initializează vectorul pentru probabilitătile conditionate

    # Parcurge valorile lui X și calculează P(X|Y)
    for(i in 2:(dim(matr)[1]-1)){
      val_var <- c(val_var, matr[i,1])
      # Verifică dacă P(Y) este 0 pentru a evita impartirea la zero
      if(fPcomun(matr,intY=intY,compY=compY)==0)
        return ("Nu se poate")

      # Calculeaza probabilitatea conditionată și o adauga la vector
      prob_var <- c(prob_var, fPcomun(matr,matr[i,1],"==",intY,compY)
                    / fPcomun(matr,intY=intY,compY=compY))
    }

    va_noua <- matrix(c(val_var,prob_var), nrow = 2, byrow = TRUE)
    return (va_noua)
  }
  # Calculul P(Y|X) dacă Y nu este specificat
  else if(is.null(intY)) {
    val_var <- c()  # Inițializează vectorul pentru valorile lui Y
    prob_var <- c()  # Inițializează vectorul pentru probabilitățile condiționate
    # Parcurge valorile lui Y și calculează P(Y|X)
    for(i in 2:(dim(matr)[2]-1)){
      val_var <- c(val_var, matr[1,i])
      # Verifica dacă P(X) este 0 pentru a evita imparțirea la zero
      if(fPcomun(matr,intX,compX)==0)
        return("Nu se poate")
      # Calculeaza probabilitatea condiționată și o adaugă la vector
      prob_var <- c(prob_var, fPcomun(matr,intX,compX,matr[1,i],"==")
                    / fPcomun(matr,intX,compX))
    }
    # Creează o matrice cu valorile și probabilitatile condiționate
    va_noua <- matrix(c(val_var,prob_var), nrow = 2, byrow = TRUE)
    return (va_noua)
  }
  # Calculul P(X=x|Y=y) pentru valori specifice ale lui X și Y
  else {
    # Verifică dacă P(Y=y) este 0 pentru a evita împărțirea la zero
    if(fPcomun(matr,intY=intY, compY=compY)==0) return(0)
    # Returnează P(X=x, Y=y) / P(Y=y)
    return (fPcomun(matr,intX, compX, intY, compY) / fPcomun(matr,intY=intY, compY=compY))
  }
}

print(matr_compl)
test <- fPcond(matr_compl,intX=-30:20, compX="<")
print(test)

# f
fPcomun <- function(matr,intX=NULL, compX=NULL, intY=NULL, compY=NULL){
  if(is.null(intX) && is.null(intY))
  {
    return (0)
  }
  if(is.null(intX))
  {
    #daca intX e NULL, atunci se calc doar prob lui Y
    if(length(intY)==1) #daca dim e 1, inseamna ca avem interval nemarginit
    {
      if(is.null(compY))
        return ("Eroare!")
      if(compY=="<")
      {
        sum <- 0
        for(i in 2:(dim(matr)[2]-1))
          if(matr[1,i] < intY) sum <- sum + matr[dim(matr)[1],i]
        return (sum)
      }
      else if(compY==">")
      {
        sum <- 0
        for(i in 2:(dim(matr)[2]-1))
          if(matr[1,i] > intY) sum <- sum + matr[dim(matr)[1],i]
        return (sum)
      }
      else if(compY=="==")
      {
        sum <- 0
        poz <- which(matr[1,2:(dim(matr)[2]-1)]==intY)
        if(length(poz)>0) sum <- sum + matr[dim(matr)[1],poz+1]
        return (sum)
      }
      else return ("Eroare!")
    }
    else #interval marginit
    {
      sum <- 0
      for(i in 2:(dim(matr)[2]-1))
        if(matr[1,i] > min(intY) && matr[1,i] < max(intY)) sum <- sum + matr[dim(matr)[1],i]
      return (sum)
    }
  }
  if(is.null(intY))
  {
    #daca intY e NULL, atunci se calc doar prob lui X
    if(length(intX)==1) #daca dim e 1, inseamna ca avem interval nemarginit
    {
      if(is.null(compX))
        return ("Eroare!")
      if(compX=="<")
      {
        sum <- 0
        for(i in 2:(dim(matr)[1]-1))
          if(matr[i,1] < intX) sum <- sum + matr[i,dim(matr)[2]]
        return (sum)
      }
      else if(compX==">")
      {
        sum <- 0
        for(i in 2:(dim(matr)[1]-1))
          if(matr[i,1] > intX) sum <- sum + matr[i,dim(matr)[2]]
        return (sum)
      }
      else if(compX=="==")
      {
        sum <- 0
        poz <- which(matr[2:(dim(matr)[1]-1),1]==intX)
        #print(poz)
        if(length(poz)>0) sum <- sum + matr[poz+1,dim(matr)[2]]
        return (sum)
      }
      else return ("Eroare!")
    }
    else #interval marginit
    {
      sum <- 0
      for(i in 2:(dim(matr)[1]-1))
        if(matr[i,1] > min(intX) && matr[i,1] < max(intX)) sum <- sum + matr[i,dim(matr)[2]]
      return (sum)
    }
  }

  #cazul in care avem intervale si pt X, si pt Y
  sum <- 0
  for(i in 2:(dim(matr)[1]-1)) #parcurgem matr si cautam celule care indeplinesc ambele conditii
    for(j in 2:(dim(matr)[2]-1))
    {
      if(length(intX)==1 && length(intY)==1)
      { # am format un string de conditie, de ex "matr[i,1] < 2"
        # print ("Both not arrays")
        condX <- paste("matr[i,1]",paste(compX, intX, sep = " "),sep = " ")
        condY <- paste("matr[1,j]",paste(compY, intY, sep = " "),sep = " ")
        if(eval(parse(text = condX)) && eval(parse(text = condY)))
        {
          # print (matr[i,j])
          sum <- sum + matr[i,j]
        }
      }
      else if(length(intX)==1)
      {
        # print ("X not array")
        condX <- paste("matr[i,1]",paste(compX, intX, sep = " "),sep = " ")
        if(eval(parse(text = condX)) && matr[1,j] > min(intY) && matr[1,j] < max(intY))
          sum <- sum + matr[i,j]
      }
      else if(length(intY)==1)
      {
        # print ("Y not array")
        condY <- paste("matr[1,j]",paste(compY, intY, sep = " "),sep = " ")
        if(eval(parse(text = condY)) && matr[i,1] > min(intX) && matr[i,1] < max(intX))
          sum <- sum + matr[i,j]
      }
      else
      {
        # print ("Both arrays")
        if(matr[i,1] > min(intX) && matr[i,1] < max(intX)
           && matr[1,j] > min(intY) && matr[1,j] < max(intY))
          sum <- sum + matr[i,j]
      }
    }
  return (sum)
}


print(matr_compl)
fPcomun(matr_compl,intY=-90:34,compY=">")

# g

fpropcov(matr,5,9,-3,-2)
fPcond(matr,seq(0,0.8,by = 0.1),intY=0.3,compY=">")
fPcomun(matr,0.2,">",1.7,"<")

# h
fverind <- function(matr){
  # initializam vectorii pentru valorile și probabilitățile variabilelor X și Y
  val_var <- c()
  prob_var <- c()

  # extragem valorile și probabilitățile pentru X
  for(i in 2:dim(matr)[1]-1){
    val_var <- c(val_var, matr[i,1])
    prob_var <- c(prob_var, matr[i, dim(matr)[2]])
  }
  X <- matrix(c(val_var, prob_var), nrow = 2, byrow = TRUE)

  # resetare vectori pentru Y
  val_var <- c()
  prob_var <- c()

  # extragem valorile si probabilitatile pentru Y
  for(i in 2:dim(matr)[2]-1){
    val_var <- c(val_var, matr[1, i])
    prob_var <- c(prob_var, matr[dim(matr)[1], i])
  }
  Y <- matrix(c(val_var, prob_var), nrow = 2, byrow = TRUE)

  # verificam daca covarianta este zero (ceea ce indica independenta)
  if(fpropcov(matr) == 0)
    return ("Independente")
  else
    return ("Dependente")
}


fverind(matr_compl)

fvernecor <- function(matr){
  #inițializam vectorii pentru valorile și probabilitatile variabilelor X și Y
  val_var <- c()
  prob_var <- c()

  # extragem valorile și probabilitățile pentru X
  for(i in 2:dim(matr)[1]-1){
    val_var <- c(val_var, matr[i, 1])
    prob_var <- c(prob_var, matr[i, dim(matr)[2]])
  }
  X <- matrix(c(val_var, prob_var), nrow = 2, byrow = TRUE)

  # resetare vectori pentru Y
  val_var <- c()
  prob_var <- c()

  # extragem valorile și probabilitatile pentru Y
  for(i in 2:dim(matr)[2]-1){
    val_var <- c(val_var, matr[1, i])
    prob_var <- c(prob_var, matr[dim(matr)[1], i])
  }
  Y <- matrix(c(val_var, prob_var), nrow = 2, byrow = TRUE)

  # calculeaza coeficientul de corelație și verifică dacă este zero
  coef <- fpropcov(matr) / sqrt(fvar(X) * fvar(Y))
  if(coef == 0)
    return ("Necorelate")
  else
    return (coef)
}

fvernecor(matr_compl)



