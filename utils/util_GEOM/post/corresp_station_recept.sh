case $station in
 ROUSSILLON1 ) idrecept=g30mtt_237_1460 
               grid=AE45 ;;
 ROUSSILLON2 ) idrecept=g30mtt_237_1459 
               grid=AE45 ;;
 A7NI1       ) idrecept=g10mtt_705_4214
               grid=AE46 ;;
 A7NI2       ) idrecept=g10mtt_706_4214 
               grid=AE46 ;;
 A7NI3       ) idrecept=g10mtt_705_4215 
               grid=AE46 ;;
 A7NI4       ) idrecept=g10mtt_706_4215 
               grid=AE46 ;;
 A7NI ) idrecept=AZNI
               grid=AE46 ;;
 * ) echo attention idrecept non defni !! $grid
     exit 1 ;;
esac
 
