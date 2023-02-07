# README

J'ai éssayé de compléter jusqu'à l'étape 4 du rendu 0  (je n'ai pas eu beaucoup de temps pour aller plus loin à cause du GALA qui a eu lieu vendredi et dont la mise en place et le rangement se sont étalés sur la semaine et le week-end puisque je faisais parti du comité, j'espère que vous serez compréhensif)

Problèmes rencontrés  : 
- Essai de synthétiser les règles dans le parser en regroupant les opérateurs  booléens cependant cela me créait de nouveaux conflits que je n'arrivais pas à esquiver j'ai donc opté pour l'écriture de chacune des règles concernants les opérateurs booléens (la structure de l'essai est en commentaire dans le fichier parser.mly )

A perfectionner faute de manque de temps : 
- Gérer les cas de matching qui ne sont pas gérés (notamment concernant les valeurs)
- Faire en sorte que les opérateurs && et || fonctionne (je me suis rendu compte trop tard que la version implémentée en commentaire ne convenait pas)

---
## Concernant les tests : 
- il me semble si j'ai bien compris que ce qui est attendu en question 1 et 3 en terme d'affichage est incompatible, il y a donc deux lignes qui sont en commentaires dans le fichier main.ml (<font color = "red" > ligne 30 et 31 ) </font> qu'il faut décommenter afin de pouvoir voir en sorti le code caml qui est généré par le programme, il faut ensuite les recommenter pour le test3_1.ml qui vérifie que la fonction prInt n'affiche bien que ce qu'on lui demande. 

test 
