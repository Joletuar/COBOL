      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CARGA-EMP.
       AUTHOR. JOHAN TUAREZ.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "./Copys/FISI-EMP.cpy".

       DATA DIVISION.
       FILE SECTION.
           COPY "./Copys/LOGI-EMP.cpy".

       WORKING-STORAGE SECTION.

       01  FL-EMP PIC XX.

       01  WS-BANDERA PIC 9.
           88 WS-BANDERA-SI VALUE 1.
           88 WS-BANDERA-NO VALUE 0.

       PROCEDURE DIVISION.

       0000-MAIN.
      *----------
           PERFORM 1000-ABRIR-ARCHIVOS.

           PERFORM 2000-SUBIR-REGISTROS.

           IF WS-BANDERA-SI
             DISPLAY "REGISTROS CARGADOS CORRECTAMENTE"
           ELSE
             DISPLAY "HUBO UN ERROR EN LA CARGA"
             DISPLAY "CODIGO DE ERROR - " FL-EMP
           END-IF.

           PERFORM 1000-CERRAR-ARCHIVOS.

           STOP RUN.


       1000-ABRIR-ARCHIVOS.
      *--------------------
           OPEN OUTPUT ARCHIVO-EMPLEADOS.

       1000-CERRAR-ARCHIVOS.
      *---------------------
           CLOSE ARCHIVO-EMPLEADOS.

       2000-SUBIR-REGISTROS.
      *---------------------
           INITIALIZE EMP-DATOS.

           MOVE 00100001 TO EMP-CLAVE.
           MOVE 1207004928 TO EMP-CEDULA.
           MOVE "JOHAN TUAREZ VEGA" TO EMP-NOMBRES.
           MOVE "URB.RENACER" TO EMP-DIRECCION.
           MOVE 0987869539 TO EMP-CERULAR.
           MOVE 001 TO EMP-COD-OFICINA.
           MOVE 600,00 TO EMP-SUELDO-ACTUAL.
           MOVE 001 TO EMP-COD-CARGO.
           MOVE "A" TO EMP-ESTADO.
           MOVE 001 TO EMP-COD-PROFESION.
           MOVE 001 TO EMP-COD-CIU-RESIDENCIA.

           PERFORM 3000-ESCRIBIR-REGISTROS.

       3000-ESCRIBIR-REGISTROS.
      *------------------------
           WRITE EMP-DATOS.
           MOVE 1 TO WS-BANDERA.

           IF FL-EMP NOT = "00"
             MOVE 0 TO WS-BANDERA
           END-IF.
