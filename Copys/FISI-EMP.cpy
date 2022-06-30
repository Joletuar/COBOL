           SELECT ARCHIVO-EMPLEADOS
             ASSIGN TO "../Archivos/Archivo-EMP.dat"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS EMP-CLAVE
             ALTERNATE RECORD KEY IS EMP-CEDULA
             FILE STATUS IS FL-EMP.
