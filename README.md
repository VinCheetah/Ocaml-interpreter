# README  

## Description :

- Notre fouine couvre tous les points des tests débutants et intermédiaires, et nous avons pu traiter l'ajout des motifs, des couples, des listes et du pattern matching. 
- Nous n'avons pas rencontré de bugs sur les points que nous avons traités et nous avons pu tester notre code sur les tests fournis. Nous avons globalement avancé à deux en travaillant sur les mêmes fichiers, et nous avons pu nous aider mutuellement pour résoudre les problèmes rencontrés.
- Nous avons rajouté certaines options comme -tree qui nous affiche l'évaluation sous forme d'arbre du code par fouine. 
- Une option -slow qui correspond à un debug étape par étape de l'évaluation.
- L'option -trace qui permet de suivre les états de l'automate durant l'évaluation.

## Moulinette de tests : 
- Nous avons utilisé la moulinette de tests fournie et elle ne renvoie pas d'erreur pour les tests débutants et intermédiaires, ainsi que pour les tests avancés qui concernent les motifs, les couples, les listes et le pattern matching et ce que nous avions déjà traité. 



## Améliorations possibles : 
- Notre option -showsrc renvoie un code Caml fonctionnel mais surparenthésé, nous n'avons pas eu le temps de corriger ce problème.

- Nous nous sommmes rendu compte que nous traitons nous expressions Match .. With et Try .. With de manière complètement autonome et différente alors qu'il pourrait être intéressant de les traiter de la même manière. 
  