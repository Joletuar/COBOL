           SELECT ARCHIVO-DESCUENTOS
             ASSIGN TO "../Archivos/Archivo-DES.dat"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS DES-CLAVE
             FILE STATUS IS FL-DES.
