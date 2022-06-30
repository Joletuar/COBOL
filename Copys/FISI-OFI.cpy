           SELECT ARCHIVO-OFICINAS
             ASSIGN TO "../Archivos/Archivo-OFI.dat"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS OFI-CLAVE
             FILE STATUS IS FL-OFI.
