           SELECT ARCHIVO-CIUDADES
             ASSIGN TO "../Archivos/Archivo-CIU.dat"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS CIU-CODIGO
             FILE STATUS IS FL-CIU.
