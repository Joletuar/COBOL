           SELECT ARCHIVO-CARGOS
             ASSIGN TO "../Archivos/Archivo-CAR.dat"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS CAR-CODIGO
             FILE STATUS IS FL-CAR.
