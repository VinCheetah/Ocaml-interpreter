# README  

Fouine de Dziki Yanis et Gardies Vincent (Niveau intermédiaire): 


## <font color = "red" > Description : </font>

- Notre fouine couvre tous les points des tests débutants et intermédiaires, et nous avons pu traiter l'ajout des motifs, des couples, des listes et du pattern matching. 
- Nous n'avons pas rencontré de bugs sur les points que nous avons traités et nous avons pu tester notre code sur les tests fournis. Nous avons globalement avancé à deux en travaillant sur les mêmes fichiers, et nous avons pu nous aider mutuellement pour résoudre les problèmes rencontrés.
- Nous avons rajouté certaines options comme `-tree` qui nous affiche l'évaluation sous forme d'arbre du code par fouine. 
- Une option `-slow` qui correspond à un debug étape par étape de l'évaluation.
- L'option `-trace` qui permet de suivre les états de l'automate durant l'évaluation.
- L'option `-showsrc` qui permet d'afficher le code Caml correspondant à l'expression fouine.
- L'option `-debug` qui permet d'afficher les états de l'environnement durant l'évaluation.

## <font color = "red"> Moulinette de tests : </font> 
- Nous avons utilisé la moulinette de tests fournie et elle ne renvoie pas d'erreur pour les tests débutants et intermédiaires, ainsi que pour les tests avancés qui concernent les motifs, les couples, les listes et le pattern matching et ce que nous avions déjà traité. 



## <font color = "red"> Améliorations possibles : </font> 
- Notre option `-showsrc` renvoie un code Caml fonctionnel mais surparenthésé, nous n'avons pas eu le temps de corriger ce problème.

- Nous nous sommmes rendu compte que nous traitons nous expressions Match .. With et Try .. With de manière complètement autonome et différente alors qu'il pourrait être intéressant de les traiter de la même manière. 
  
- Nous avons rencontré une difficulté de dernière minute sur un test avancé qui se nommait nm2.ml pour lequel notre environnement ce perdait et nous n'avons pas eu le temps de trouver l'origine du problème.


## <font color = "red"> Points notables : </font> 

- Nous avons traité les expressions Match .. With en nous basant sur notre fonction filtre qui nous permet de filtrer les expressions qui correspondent à notre motif. Nous avons donc créé une fonction qui prend en paramètre une expression , un environnement et un motif et qui renvoie un booléen et un nouvel environnement, le booléen permettant de savoir si un cas de matching correspondait. Nous avons ensuite utilisé cette fonction pour traiter les expressions Match .. With.


- Nous avons procédé de manière presque similaire pour Try .. With mais en rajoutant des motifs correspondants aux expressions présentes dans le With et correspondant aux exceptions possibles.

- Les références, les listes et les séquences d'expression sont implémentés en suivant les indications données dans les notes de cours


- Les fonctions récursives diffèrent des fonctions non récursives grâce à un booléen qui est présent lors de la déclaration de la fonction et qui est mis à true si le mot clé `rec` est présent. Les environnements sont alors gérés différemment pour les fonctions récursives pour lesquelles on modifie d'abord l'environnement propre de la fonction pour intégrer le fait que la fonction puisse s'appeler elle même et les fonctions non récursives.
  
  
## <font color = "red"> Corrections suite au pré-rendu : </font>

- Suite aux remarques du pré-rendu, nous avons remarqué et facilement corrigé des erreurs dans l'ordre d'éxecution des expressions dans le cas d'une application, ou lors de l'usage d'un opérateur (booléen, arithmétique,...).

- Nous avons apporté une correction à la priorité du token UMINUS pour résoudre un problème avec un entier négatif dans un couple. De plus nous avons passé notre token REVAL (qui correspond à ":=") de %nonassoc à %right.

- Nous avons aussi ajouté la possibilité de mettre le symbole '_' dans les noms de variables. En effet c'est la raison de l'échec d'un des tests que vous nous avez fait remonté.

- Des modifications ont été apportées au traitement des fonctions et des applications, particulièrement lorsque celles ci sont récursives. Cela a permis de résoudre les problèmes de "perte d'environnement" qui pouvait avoir lieu dans certaines expressions. Ainsi tout les tests de la moulinette (B,I,A) sont correctement effectués par notre fouine.   

- Comme il nous a été demandé dans le pré-rendu, nous avons effectué quelques tests sur les environnements des fonctions appliquées partiellement et nous n'avons pas relevé de problème.


