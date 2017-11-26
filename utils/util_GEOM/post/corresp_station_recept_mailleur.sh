case $station in
 ROUSSILLON  ) idrecept=ROUSSILLON 
               grid=AE45 ;;
 ROUSSILLON1 ) idrecept=gr50m_4841 
               grid=AE45 ;;
 ROUSSILLON2 ) idrecept=gr50m_1792 
               grid=AE45 ;;
 ROUSSILLON3 ) idrecept=gr50m_774 
               grid=AE45 ;;
 ROUSSILLON4 ) idrecept=gr50m_4840 
               grid=AE45 ;;
 AZNI          ) idrecept=AZNI
               grid=AE46 ;;
# A7NI1       ) idrecept=
#               grid=AE46 ;;
# A7NI2       ) idrecept= 
#               grid=AE46 ;;
# A7NI3       ) idrecept= 
#               grid=AE46 ;;
# A7NI4       ) idrecept= 
#               grid=AE46 ;;
 * ) echo attention idrecept non defni !! $grid  $station
     exit 1 ;;
esac
 
