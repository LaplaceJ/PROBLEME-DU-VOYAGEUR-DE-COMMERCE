#Chargement du package
library(geosphere)

#Chargement des données
ville = read.table(
  "/home/jordan/Documents/ProjetPS/20villes.txt",
  sep = ";" ,
  dec = ".",
  header = TRUE
)

# Formation de la matrice de distance entre les 22 villes
matrice_distance  = matrix(nrow = length(ville[, 1]) , ncol = length(ville[, 1]))

for (i in 1:length(ville[, 1])) {
  for (j in 1:length(ville[, 1])) {
    matrice_distance[i, j] = distm(c(ville[i, 3] , ville[i, 2]) , c(ville[j, 3] , ville[j, 2]), fun = distGeo)
  }
}

data.distance = as.data.frame(matrice_distance)
colnames(data.distance)  = ville[, 1]
rownames(data.distance)  = ville[, 1]


########## ----------------- QUESTION 1 ----------------- ##########

# somme_distance : permet de calculer la longueur d'une route entre les villes du vecteur vec
# distance : matrice de distance entre les villes du vecteur vec
# vec : vecteur d'entiers qui correspond à l'enchainement des villes à parcourir
somme_distance = function(distance , vec) {
  somme = 0
  
  # de la première ville à la dernière
  for (i in 1:(length(vec) - 1))  {
    somme  = somme + distance[vec[i] , vec[i + 1]]
  }
  
  # retour à la première ville
  somme = somme  + distance[vec[length(vec)] ,  vec[1]]
  
  return (somme)
}


#Algorithme de recherche exhaustive du meilleur chemin
voyageur_de_commerce_exhaustif = function(matrice_distance , lVilles) {
  # si nous avons parcouru toute les villes nous retournons la listeEnCours
  if (length(matrice_distance[, 1]) == length(lVilles)) {
    return (lVilles)
  }
  
  # Nous devons minimiser la valeur de la variable resDistance.
  # Nous prenons donc une valeur de départ élevée.
  resDistance = 99999999999999999
  
  # Meilleur chemin retenu
  lBestRes = c()
  
  
  for (i in 1:length(matrice_distance[, 1])) {
    # si la ville i n'est pas dans la liste alors nous l'ajoutons
    if (!any(i == lVilles)) {
      # Ajout dans la liste lVilles de façon récursive
      lRes = voyageur_de_commerce_exhaustif(matrice_distance, append(lVilles, i))
      
      # Nous prennons le minimun de distance parcouru entre resDistance et
      # notre la nouvelle liste lRes
      if (resDistance >  somme_distance(matrice_distance , lRes)) {
        # Mise à jour de la variable resDistance
        resDistance = somme_distance(matrice_distance , lRes)
        
        #On stock la variable lBestRes
        lBestRes = lRes
      }
    }
  }
  
  return(lBestRes)
}


# pcc = voyageur_de_commerce_exhaustif(data.distance[1:9, 1:9], c())
# somme_distance(data.distance[1:9, 1:9], pcc)


########## ----------------- QUESTION 2 ----------------- ##########


# calcul de  q : si avons 20 villes, nous avons un vecteur de chemin de taille 21 ( retour vers la ville de départ) et donc nous avons une combinaison de chemin voisin
# de "combinaison de 2 parmis 19"

# Métropolis version 1.0
metropolis <- function(start_value, matrice_ville, iteration) {
  # Nombre d'itération de metropolis
  N = iteration
  
  # vecteur des villes de départ
  i = start_value
  
  for (n in 1:N) {
    # tirage de j
    j = i
    ind_ville_permutation = sample(2:(length(i[1, ])), 2)
    tmp = i[ind_ville_permutation[1]]
    j[ind_ville_permutation[1]] = j[ind_ville_permutation[2]]
    j[ind_ville_permutation[2]] = tmp
    
    
    # calcul de p
    Hi = somme_distance(data.distance, j)
    Hj = somme_distance(data.distance, i)
    Pt = exp(-(1 / T)(Hi - Hj))
    
    # changement de la valeur de i en fonction de p et du tirage de U.
    if (p >= 1) {
      i = j
    }
    else{
      U = runif(1, 0, 1)
      if (u <= p) {
        i = j
      }
    }
  }
  return(i)
}


########## ----------------- QUESTION 3.1
# Implémenter l’algorithme du recuit simulé pour le problème
# du voyageur de commerce ----------------- ##########



# Métropolis version 2.0 avec l'implémentation du recuit simulé
metropolis_recuit <-
  function(start_value, matrice_ville, iteration, h) {
    # Nombre d'itération de metropolis
    N = iteration
    
    # vecteur des villes de départ
    i = start_value
    
    for (n in 1:N) {
      # tirage de j
      j = i
      ind_ville_permutation = sample(2:(length(i)), 2)
      tmp = i[ind_ville_permutation[1]]
      j[ind_ville_permutation[1]] = j[ind_ville_permutation[2]]
      j[ind_ville_permutation[2]] = tmp
      
      
      # calcul de p
      T = h / log(n)
      Hi = somme_distance(data.distance, i)
      Hj = somme_distance(data.distance, j)
      Pt = exp(-(1 / T) * (Hj - Hi))
      
      # changement de la valeur de i en fonction de Pt et du tirage de U.
      if (Pt >= 1) {
        i = j
      }
      else{
        u = runif(1, 0, 1)
        if (u <= Pt) {
          i = j
        }
      }
    }
    return(i)
  }

# Métropolis version 2.1 avec l'implémentation du recuit simulé et affichage de ???
metropolis_recuit_affichage <-
  function(start_value, matrice_ville, iteration, h) {
    distance = c()
    
    # Nombre d'itération de metropolis
    N = iteration
    
    # vecteur des villes de départ
    i = start_value
    
    for (n in 1:N) {
      # tirage de j
      j = i
      ind_ville_permutation = sample(2:(length(i)), 2)
      tmp = i[ind_ville_permutation[1]]
      j[ind_ville_permutation[1]] = j[ind_ville_permutation[2]]
      j[ind_ville_permutation[2]] = tmp
      
      
      # calcul de p
      T = h / log(n)
      Hi = somme_distance(data.distance, i)
      Hj = somme_distance(data.distance, j)
      Pt = exp(-(1 / T) * (Hj - Hi))
      
      # changement de la valeur de i en fonction de p et du tirage de U.
      if (Pt >= 1) {
        i = j
        distance = cbind(distance, Hj)
      }
      else{
        u = runif(1, 0, 1)
        if (u <= Pt) {
          i = j
          distance = cbind(distance, Hj)
        } else{
          distance = cbind(distance, Hi)
        }
      }
    }
    plot(1:N, distance, type = "l")
    return(i)
  }


########## ----------------- QUESTION 3.2
#  Proposer des critères d’arrêt ----------------- ##########

#Métropolis version 3.0 avec ( recuit simulé + stockage du minimun )
metropolis_recuitMin <-
  function(start_value, matrice_ville, iteration, h) {
    # distance = c()
    
    # Nombre d'itération de metropolis
    N = iteration
    
    # vecteur des villes de départ
    i = start_value
    Min = start_value
    
    for (n in 1:N) {
      # tirage de j
      j = i
      ind_ville_permutation = sample(2:(length(i)), 2)
      tmp = i[ind_ville_permutation[1]]
      j[ind_ville_permutation[1]] = j[ind_ville_permutation[2]]
      j[ind_ville_permutation[2]] = tmp
      
      
      # recuit simulé
      T = h / log(n)
      
      # calcul de p
      Hi = somme_distance(matrice_ville, i)
      Hj = somme_distance(matrice_ville, j)
      Pt = exp(-(1 / T) * (Hj - Hi))
      
      # Stockage du minimun
      if (somme_distance(matrice_ville , Min) > Hj) {
        Min = j
      }
      
      # changement de la valeur de i en fonction de p et du tirage de U.
      if (Pt >= 1) {
        i = j
        # distance = cbind(distance,Hj)
      }
      else{
        U = runif(1, 0, 1)
        if (U <= Pt) {
          i = j
          # distance = cbind(distance,Hj)
        }
        # distance = cbind(distance,Hi)
      }
    }
    #plot(1:N,distance,type="l")
    return(Min)
  }

#Métropolis version 3.1 avec recuit simulé,  stockage du minimun  et affichage de ???
metropolis_recuitMin_affichage <-
  function(start_value, matrice_ville, iteration, h) {
    distance = c()
    
    # Nombre d'itération de metropolis
    N = iteration
    
    # vecteur des villes de départ
    i = start_value
    Min = start_value
    
    for (n in 1:N) {
      # tirage de j
      j = i
      ind_ville_permutation = sample(2:(length(i)), 2)
      tmp = i[ind_ville_permutation[1]]
      j[ind_ville_permutation[1]] = j[ind_ville_permutation[2]]
      j[ind_ville_permutation[2]] = tmp
      
      
      # recuit simulé
      T = h / log(n)
      
      # calcul de p
      Hi = somme_distance(matrice_ville, i)
      Hj = somme_distance(matrice_ville, j)
      Pt = exp(-(1 / T) * (Hj - Hi))
      
      # Stockage du minimun
      if (somme_distance(matrice_ville , Min) > Hj) {
        Min = j
      }
      
      # changement de la valeur de i en fonction de Pt et du tirage de U.
      if (Pt >= 1) {
        i = j
        distance = cbind(distance, Hj)
      }
      else{
        U = runif(1, 0, 1)
        if (U <= Pt) {
          i = j
          distance = cbind(distance, Hj)
        } else{
          distance = cbind(distance, Hi)
        }
        
      }
    }
    plot(1:N, distance, type = "l")
    return(Min)
  }

# Métropolis version 4.0 avec ( recuit simulé + stockage du minimun + arret )
# L'arret de metropolis à lieu en fonction de la variable compteur. Si ce dernier
# est deux fois supérieur au nombre de ville à parcourir alors l'algorithme s'arrete.
metropolis_recuit_arret <-
  function(start_value, matrice_ville, iteration, h) {
    # Nombre d'itération de metropolis
    N = iteration
    compteur = 0
    
    # vecteur des villes de départ
    i = start_value
    n = 1
    for (n in 1:N) {
      # tirage de j
      j = i
      ind_ville_permutation = sample(2:(length(i)), 2)
      tmp = i[ind_ville_permutation[1]]
      j[ind_ville_permutation[1]] = j[ind_ville_permutation[2]]
      j[ind_ville_permutation[2]] = tmp
      
      
      # calcul de p
      T = h / log(n)
      Hi = somme_distance(matrice_ville, i)
      Hj = somme_distance(matrice_ville, j)
      Pt = exp(-(1 / T) * (Hj - Hi))
      
      if (Hi > Hj) {
        # remise à 0 du compteur
        compteur = 0
      } else{
        compteur = compteur + 1
        
        # si cela fais trop longtemps que nous n'avons pas trouvé un nouveau minimun, alors nous arretons metropolis
        # si le compteur est supérieur au nombre de ville * 2, alors nous arretons metropolis.
        if (compteur > (length(start_value)  * 2))
          return(i)
      }
      
      # changement de la valeur de i en fonction de Pt et du tirage de U.
      if (Pt >= 1) {
        i = j
        
      }
      else{
        U = runif(1, 0, 1)
        if (U <= Pt) {
          i = j
          
        }
        
      }
    }
    
    return(i)
  }

# Métropolis version 4.1 avec ( recuit simulé +  arret )
metropolis_recuit_arret_affichage <-
  function(start_value, matrice_ville, iteration, h) {
    distance = c()
    
    # Nombre d'itération de metropolis
    N = iteration
    compteur = 0
    
    # vecteur des villes de départ
    i = start_value
    n = 1
    for (n in 1:N) {
      # tirage de j
      j = i
      ind_ville_permutation = sample(2:(length(i)), 2)
      tmp = i[ind_ville_permutation[1]]
      j[ind_ville_permutation[1]] = j[ind_ville_permutation[2]]
      j[ind_ville_permutation[2]] = tmp
      
      
      # calcul de p
      T = h / log(n)
      Hi = somme_distance(matrice_ville, i)
      Hj = somme_distance(matrice_ville, j)
      Pt = exp(-(1 / T) * (Hj - Hi))
      
      if (Hi > Hj) {
        # remise à 0 du compteur
        compteur = 0
      } else{
        compteur = compteur + 1
        
        # si cela fais trop longtemps que nous n'avons pas trouvé un nouveau minimun, alors nous arretons metropolis
        # si le compteur est supérieur au nombre de ville * 2, alors nous arretons metropolis.
        if (compteur > (length(start_value)  * 2))
          return(i)
      }
      
      # changement de la valeur de i en fonction de Pt et du tirage de U.
      if (Pt >= 1) {
        i = j
        distance = cbind(distance, Hj)
      }
      else{
        U = runif(1, 0, 1)
        if (U <= Pt) {
          i = j
          distance = cbind(distance, Hj)
        } else{
          distance = cbind(distance, Hi)
        }
        
      }
    }
    plot(1:N, distance, type = "l")
    return(i)
  }

# Métropolis version 5.0 avec ( recuit simulé + stockage du minimun + arret )
# L'arret de metropolis à lieu en fonction de la variable compteur. Si ce dernier
# est deux fois supérieur au nombre de ville à parcourir alors l'algorithme s'arrete.
metropolis_recuit_arret_min <-
  function(start_value, matrice_ville, iteration, h) {
    # Nombre d'itération de metropolis
    N = iteration
    compteur = 0
    
    # vecteur des villes de départ
    i = start_value
    Min = i
    n = 1
    for (n in 1:N) {
      # tirage de j
      j = i
      ind_ville_permutation = sample(2:(length(i)), 2)
      tmp = i[ind_ville_permutation[1]]
      j[ind_ville_permutation[1]] = j[ind_ville_permutation[2]]
      j[ind_ville_permutation[2]] = tmp
      
      
      # calcul de p
      T = h / log(n)
      Hi = somme_distance(matrice_ville, i)
      Hj = somme_distance(matrice_ville, j)
      Pt = exp(-(1 / T) * (Hj - Hi))
      
      if (somme_distance(matrice_ville , Min) > Hj) {
        Min = j
        
        # remise à 0 du compteur
        compteur = 0
      } else{
        compteur = compteur + 1
        
        # si cela fais trop longtemps que nous n'avons pas trouvé un nouveau minimun, alors nous arretons metropolis
        # si le compteur est supérieur au nombre de ville * 2, alors nous arretons metropolis.
        if (compteur > (length(start_value)  * 2))
          return(Min)
      }
      
      # changement de la valeur de i en fonction de Pt et du tirage de U.
      if (Pt >= 1) {
        i = j
        
      }
      else{
        U = runif(1, 0, 1)
        if (U <= Pt) {
          i = j
          
        }
        
      }
    }
    
    return(Min)
  }

# Métropolis version 5.1 avec ( recuit simulé + stockage du minimun + arret )
metropolis_recuit_arret_min_affichage <-
  function(start_value, matrice_ville, iteration, h) {
    distance = c()
    
    # Nombre d'itération de metropolis
    N = iteration
    compteur = 0
    
    # vecteur des villes de départ
    i = start_value
    Min = i
    n = 1
    for (n in 1:N) {
      # tirage de j
      j = i
      ind_ville_permutation = sample(2:(length(i)), 2)
      tmp = i[ind_ville_permutation[1]]
      j[ind_ville_permutation[1]] = j[ind_ville_permutation[2]]
      j[ind_ville_permutation[2]] = tmp
      
      
      # calcul de p
      T = h / log(n)
      Hi = somme_distance(matrice_ville, i)
      Hj = somme_distance(matrice_ville, j)
      Pt = exp(-(1 / T) * (Hj - Hi))
      
      if (somme_distance(matrice_ville , Min) > Hj) {
        Min = j
        
        # remise à 0 du compteur
        compteur = 0
      } else{
        compteur = compteur + 1
        
        # si cela fais trop longtemps que nous n'avons pas trouvé un nouveau minimun, alors nous arretons metropolis
        # si le compteur est supérieur au nombre de ville * 2, alors nous arretons metropolis.
        if (compteur > (length(start_value)  * 2))
          return(Min)
      }
      
      # changement de la valeur de i en fonction de Pt et du tirage de U.
      if (Pt >= 1) {
        i = j
        distance = cbind(distance, Hj)
      }
      else{
        U = runif(1, 0, 1)
        if (U <= Pt) {
          i = j
          distance = cbind(distance, Hj)
        } else{
          distance = cbind(distance, Hi)
        }
        
      }
    }
    plot(1:N, distance, type = "l")
    return(Min)
  }


# it_metropolis version 1. Nous iterons plusieurs fois sur l'algorithme de metropolis avec des paramètre différent
# v1 : nous ré-utilisons le résultat de métropolis comme paramètre ( la liste des villes à parcourir ).
it_metropolis_1 <-
  function (init_ville,
            matrice_ville,
            nIt  ,
            nItMetro,
            h) {
    nbVille = length(init_ville)
    ville_rec = init_ville
    
    for (i in 1:nIt) {
      ville_rec = metropolis_recuit(ville_rec, matrice_ville, nItMetro , h)
      
      dist  = somme_distance(matrice_ville[1:nbVille, 1:nbVille], ville_rec)

    }
    
    return(ville_rec)
  }

it_metropolis_2 <-
  function (init_ville,
            matrice_ville,
            nIt  ,
            nItMetro,
            h) {
    nbVille = length(init_ville)
    ville_rec = init_ville
    vMin = init_ville
    distMin = somme_distance(matrice_ville[1:nbVille, 1:nbVille], vMin)
    
    for (i in 1:nIt) {
      ville_rec = metropolis_recuit(ville_rec, matrice_ville, nItMetro , h)
      
      dist  = somme_distance(matrice_ville[1:nbVille, 1:nbVille], ville_rec)
      
      if (distMin > dist) {
        vMin = ville_rec
        distMin = somme_distance(matrice_ville[1:nbVille, 1:nbVille], vMin)
        
      }
      else  {
        ville_rec = vMin
      }

    }
    
    return(ville_rec)
  }

it_metropolis_3 <-
  function (init_ville,
            matrice_ville,
            nIt  ,
            nItMetro,
            h) {
    nbVille = length(init_ville)
    ville_rec = init_ville
    vMin = init_ville
    distMin = somme_distance(matrice_ville[1:nbVille, 1:nbVille], vMin)
    
    for (i in 1:nIt) {
      ville_rec = metropolis_recuit_arret(ville_rec, matrice_ville, nItMetro , h)
      
      dist  = somme_distance(matrice_ville[1:nbVille, 1:nbVille], ville_rec)
      
      if (distMin > dist) {
        vMin = ville_rec
        distMin = somme_distance(matrice_ville[1:nbVille, 1:nbVille], vMin)
        
      }
      else  {
        ville_rec = vMin
      }


    }
    
    return(vMin)
  }

# TODO comparaison avec le temps d'éxécution pour 8 villes
#Comparaison entre exhaustif et recuit simulé de l'algo de Metropolis + recuit simulé
{
  
  pcc0 = voyageur_de_commerce_exhaustif(data.distance[1:8, 1:8], c())
  somme_distance(data.distance[1:8, 1:8], pcc0)
  
  pcc1 = metropolis_recuit_affichage(c(1, 2, 3, 4, 5, 6, 7, 8), data.distance, 1000, 0.1)
  somme_distance(data.distance[1:8, 1:8], pcc1)
  
  pcc2 = metropolis_recuitMin_affichage(c(1, 2, 3, 4, 5, 6, 7, 8), data.distance, 1000, 0.1)
  somme_distance(data.distance[1:8, 1:8], pcc2)
  
  pcc3 = metropolis_recuit_arret_affichage(c(1, 2, 3, 4, 5, 6, 7, 8), data.distance, 1000, 0.1)
  somme_distance(data.distance[1:8, 1:8], pcc3)
  
  pcc4 = it_metropolis_1(c(1, 2, 3, 4, 5, 6, 7, 8), data.distance, 10, 1000, 0.1)
  somme_distance(data.distance[1:8, 1:8], pcc4)
}



{
  ville22  = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
  ville22 = ind_ville_permutation = sample(1:(length(ville22)), 22)
  
  temps = c(0,0,0,0,0,0)
  performance = c(0,0,0,0,0,0)
  for (i in 1:10) {
    
    ville22 = ind_ville_permutation = sample(1:(length(ville22)), 22)
    
    tmp_act = as.numeric(Sys.time())*1000
    pcc1 = metropolis_recuit_affichage(ville22, data.distance, 600, 2000)
    performance[1] = performance[1] + somme_distance(data.distance, pcc1)
    temps[1] = temps[1] + as.numeric(Sys.time())*1000 - tmp_act
    
    tmp_act = as.numeric(Sys.time())*1000
    
    pcc2 = metropolis_recuitMin_affichage(ville22, data.distance, 600, 2000)
    somme_distance(data.distance, pcc2)
    performance[2] = performance[2] + somme_distance(data.distance, pcc2)
    temps[2] =  temps[2] + as.numeric(Sys.time())*1000 - tmp_act
    
    tmp_act = as.numeric(Sys.time())*1000
    
    pcc3 = metropolis_recuit_arret_affichage(ville22, data.distance, 600, 2000)
    performance[3] = performance[3] + somme_distance(data.distance, pcc3)
    temps[3] = temps[3] + as.numeric(Sys.time())*1000 - tmp_act
    
    tmp_act = as.numeric(Sys.time())*1000
    
    pcc4 = it_metropolis_1(ville22, data.distance, 50, 600, 2000)
    performance[4] = performance[4] + somme_distance(data.distance, pcc4)
    temps[4] = temps[4] + as.numeric(Sys.time())*1000 - tmp_act
    
    # meilleur algo
    tmp_act = as.numeric(Sys.time())*1000
    
    pcc5 = it_metropolis_2(ville22, data.distance, 50, 600, 220)
    somme_distance(data.distance, pcc5)
    performance[5] = performance[5] + somme_distance(data.distance, pcc5)
    temps[5] = temps[5] + as.numeric(Sys.time())*1000 - tmp_act
    
    tmp_act = as.numeric(Sys.time())*1000
    
    pcc6 = it_metropolis_3(ville22, data.distance, 100, 600, 220)
    somme_distance(data.distance, pcc6)
    performance[6] = performance[6] + somme_distance(data.distance, pcc6)
    temps[6] =  temps[6] + as.numeric(Sys.time())*1000 - tmp_act
  }
  temps/1000/10 # 0.3668105  0.5473795  0.1880086 17.3110669 17.3799486  6.0378042
  performance/10 # 4841371 4919956 5644726 4294664 3916153 4176307
  # meilleur score 3 640 152 [1]  1  9 10  8  7  5  6 12 19  2 11  3 14 13 15  4 18 17 16 22 21 20
}


