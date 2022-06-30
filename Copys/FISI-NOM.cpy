           SELECT ARCHIVO-NOMINAS
             ASSIGN TO "../Archivos/Archivo-NOM.dat"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS NOM-CLAVE
             FILE STATUS IS FL-NOM.
