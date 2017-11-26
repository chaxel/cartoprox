FILENAME=="masque_"domaine".txt" {
type_recepteur=$1
if ( type_recepteur == "R" ) { code[R_$2]=$2 }
if ( type_recepteur == "B" ) { code[B_$2]=$2 }
#code[$1_$2]=$1_$2
#print $1"-"$2
#le code est sour forme B-xxxxx ou R-xxxxx
}

#si le code sous forme B-xxxxx ou R-xxxxx n'est pas dans le fichier -> ecrit
FILENAME=="recept_"domaine".txt" {
if ( $1 != code[R_$1] ) { print "R"$1" "$2" "$3 }
}

FILENAME=="brin_"domaine".txt" {
if ( $1 != code[B_$1] ) { print "B"$1" "$2" "$3 }
}
