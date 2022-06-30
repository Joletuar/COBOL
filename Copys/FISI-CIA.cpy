           SELECT ARCHIVO-EMPRESAS
             ASSIGN TO "../Archivos/Archivo-CIA.dat"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS CIA-CODIGO
             FILE STATUS IS FS-CIA.
