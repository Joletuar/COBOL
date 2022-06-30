           SELECT ARCHIVO-PROFESIONES
             ASSIGN TO "../Archivos/Archivo-PRO.dat"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS PRO-CODIGO
             FILE STATUS IS FL-PRO.
