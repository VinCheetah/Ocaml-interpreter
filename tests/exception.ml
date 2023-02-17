try let x = ref (2 + prInt 3) in raise (E !x) with E 5 -> ref true;;
