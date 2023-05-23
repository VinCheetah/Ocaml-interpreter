#!/bin/bash

fichier="test4_2.ml"  # Spécifiez le chemin vers votre fichier ici
executable1="test.sh"  # Spécifiez le chemin vers votre premier exécutable ici
executable2="../fouine"  # Spécifiez le chemin vers votre deuxième exécutable ici

# Exécution du premier exécutable et récupération de sa sortie
sortie_executable1=$(./"$executable1" "$fichier")

# Exécution du deuxième exécutable et récupération de sa sortie
sortie_executable2=$(./"$executable2" "$fichier")

# Comparaison des sorties
if [[ "$sortie_executable1" == "$sortie_executable2" ]]; then
  echo "Les sorties des exécutables sont identiques."
else
  echo "Les sorties des exécutables sont différentes."
fi
