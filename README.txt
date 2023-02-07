Ce répertoire contient un sequelette de départ à partir duquel vous
pouvez programmer votre fouine.

Pour compiler, faites
make
qui aura pour effet de créer l'exécutable appelé fouine.
Faites ensuite
./fouine
pour lancer l'exécutable et saisir une expression au clavier. Par exemple
> ./fouine
3+2*5
Add(3, Mul(2, 5))
TODO

Vous pouvez aussi faire
./fouine tests/basic.ml
pour lancer fouine sur le fichier basic.ml


main.ml : fichier principal
expr.ml : définition des expressions et de l'évaluation
affichage.ml : fonctions d'affichage
lexer.mll : lexèmes, analyse lexicale
parser.mly : règles de grammaire, analyse syntaxique
tests/ : sous-répertoire de tests
Makefile : pour la compilation, à ne pas modifier a priori


