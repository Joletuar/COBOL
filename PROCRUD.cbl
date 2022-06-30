      *******************************************************************
      *                                                                 *
      *                                                                 *
      *                                                                 *
      *                                                                 *
      *******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. PROCRUD.
       AUTHOR. JOHAN TUAREZ.
       DATE-WRITTEN. 27/06/2022.

       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       FILE-CONTROL.
            COPY "./Copys/FISI-CIA.cpy".
            COPY "./Copys/FISI-PRO.cpy".
            COPY "./Copys/FISI-CIU.cpy".
            COPY "./Copys/FISI-CAR.cpy".

       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
            COPY "./Copys/LOGI-CIA.cpy".
            COPY "./Copys/LOGI-PRO.cpy".
            COPY "./Copys/LOGI-CIU.cpy".
            COPY "./Copys/LOGI-CAR.cpy".


       WORKING-STORAGE SECTION.
      *-----------------------

       01  WS-ARCHIVO-SELECCIONADO   PIC X(11).

       01  FILE-STATUS.
           03 FS-CIA                 PIC XX.
           03 FS-PRO                 PIC XX.
           03 FS-CIU                 PIC XX.
           03 FS-CAR                 PIC XX.
           03 FS-USADO               PIC XX.

       01  WS-CAMPOS-INGRESADOS.
           03 WS-EMPRESA.
             05 WS-CIA-CODIGO        PIC 9(3).
             05 WS-CIA-DESCRIPCION   PIC X(30).
             05 WS-CIA-ESTADO        PIC X.
             05 WS-CIA-REPRESENTANTE PIC X(30).
             05 WS-CIA-RUC           PIC 9(10).
             05 WS-CIA-DIRECCION     PIC X(40).
             03 WS-CIA-TELEFONO      PIC 9(10).
           03 WS-CARGO.
             05 WS-CAR-CODIGO        PIC 9(3).
             05 WS-CAR-DESCRIPCION   PIC X(30).
             05 WS-CAR-ESTADO        PIC X.
           03 WS-CIUDAD.
             05 WS-CIU-CODIGO        PIC 9(3).
             05 WS-CIU-DESCRIPCION   PIC X(30).
           03 WS-PROFESIONES.
             05 WS-PRO-CODIGO        PIC 9(3).
             05 WS-PRO-DESCRIPCION   PIC X(30).
             05 WS-PRO-ESTADO        PIC X.

       01  WS-MENSAJES.
           03 WS-MSJ-ACTUAL         PIC X(50).
           03 WS-MSJ-OPERACION      PIC X(50).

       01  WS-BANDERAS.
           03 WS-BANDERAS-ARCHIVO    PIC 9.
             88 WS-ERROR-ARCH       VALUE 0.
             88 WS-NO-ERROR-ARCH    VALUE 1.
           03 WS-BANDERAS-CAMPOS.
             05 WS-BANDERA-CIA       PIC 9.
               88 WS-CIA-CORRECTO   VALUE 1.
               88 WS-CIA-INCORRECTO VALUE 0.
             05 WS-BANDERA-CIU       PIC 9.
               88 WS-CIU-CORRECTO   VALUE 1.
               88 WS-CIU-INCORRECTO VALUE 0.
             05 WS-BANDERA-CAR       PIC 9.
               88 WS-CAR-CORRECTO   VALUE 1.
               88 WS-CAR-INCORRECTO VALUE 0.
             05 WS-BANDERA-PRO       PIC 9.
               88 WS-PRO-CORRECTO   VALUE 1.
               88 WS-PRO-INCORRECTO VALUE 0.


       01  WS-CONSTANTES.
           03 WS-ARCH-EXTENSIONES.
             05 WS-EXT-CIA           PIC X(3) VALUE "CIA".
             05 WS-EXT-CAR           PIC X(3) VALUE "CAR".
             05 WS-EXT-CIU           PIC X(3) VALUE "CIU".
             05 WS-EXT-PRO           PIC X(3) VALUE "PRO".
           03 WS-ARCH-NOMBRES.
             05 WS-NOM-CIA           PIC X(11) VALUE "ARCHIVO-CIA".
             05 WS-NOM-CAR           PIC X(11) VALUE "ARCHIVO-CAR".
             05 WS-NOM-CIU           PIC X(11) VALUE "ARCHIVO-CIU".
             05 WS-NOM-PRO           PIC X(11) VALUE "ARCHIVO-PRO".
           03 WS-OPE-EXTENSIONES.
             05 WS-EXT-REGISTRAR     PIC X VALUE "C".
             05 WS-EXT-CONSULTAR     PIC X VALUE "R".
             05 WS-EXT-ACTUALIZAR    PIC X VALUE "U".
             05 WS-EXT-ELIMINAR      PIC X VALUE "D".
           03 WS-DECORADORES.
             05 WS-LINEAS-1          PIC X(25) VALUE ALL "*".


       LINKAGE SECTION.
      *----------------

       01  PARAM-TIPO-ARCHIVO        PIC X(11).
       01  PARAM-OPERACION           PIC X.

       PROCEDURE DIVISION USING PARAM-TIPO-ARCHIVO PARAM-OPERACION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.

       0000-MAIN.
      *----------

           PERFORM 1000-PROCESOS.

           IF WS-ERROR-ARCH
               DISPLAY WS-DECORADORES
               DISPLAY WS-MSJ-OPERACION
               DISPLAY "CODIGO DE ERROR - " FS-USADO
               DISPLAY WS-DECORADORES
           ELSE
               DISPLAY WS-DECORADORES
               DISPLAY WS-MSJ-OPERACION
               DISPLAY WS-DECORADORES
           END-IF.

           GOBACK.


       1000-PROCESOS.
      *--------------
           PERFORM 1000-SELECCION-ARCHIVO.
           PERFORM 4010-MODO-INPUT.

           IF FS-USADO NOT = "00"

               PERFORM 2010-CERRAR-ARCHIVOS
               PERFORM 2000-CREACION-ARCHIVOS

               IF FS-USADO NOT = "00"
                   MOVE 0 TO WS-BANDERAS-ARCHIVO
               END-IF

           ELSE

               PERFORM 2010-CERRAR-ARCHIVOS
               PERFORM 1010-SELECCION-OPERACION

               IF FS-USADO NOT = "00"
                   MOVE 0 TO WS-BANDERAS-ARCHIVO
               ELSE
                   PERFORM 1020-EJECUTAR-OPERACION
               END-IF

           END-IF.

           PERFORM 2010-CERRAR-ARCHIVOS.


       1010-SELECCION-ARCHIVO.
      *-----------------------
           INITIALIZE WS-ARCHIVO-SELECCIONADO.
           INITIALIZE FILE-STATUS.

           EVALUATE TRUE
               WHEN PARAM-TIPO-ARCHIVO = WS-EXT-CIA
                   MOVE WS-NOM-CIA TO WS-ARCHIVO-SELECCIONADO
                   MOVE FS-CIA TO FS-USADO
               WHEN PARAM-TIPO-ARCHIVO = WS-EXT-CAR
                   MOVE WS-NOM-CAR TO WS-ARCHIVO-SELECCIONADO
                   MOVE FS-CAR TO FS-USADO
               WHEN PARAM-TIPO-ARCHIVO = WS-EXT-PRO
                   MOVE WS-NOM-PRO TO WS-ARCHIVO-SELECCIONADO
                   MOVE FS-PRO TO FS-USADO
               WHEN PARAM-TIPO-ARCHIVO = WS-EXT-CIU
                   MOVE WS-NOM-CIU TO WS-ARCHIVO-SELECCIONADO
                   MOVE FS-CIU TO FS-USADO
               WHEN OTHER
                   MOVE 0 TO WS-BANDERAS-ARCHIVO
                   CONTINUE
           END-EVALUATE.


       1010-SELECCION-OPERACION.
      *-------------------------
           INITIALIZE FILE-STATUS.

           EVALUATE TRUE
               WHEN PARAM-OPERACION = WS-EXT-REGISTRAR
               WHEN PARAM-OPERACION = WS-EXT-ACTUALIZAR
               WHEN PARAM-OPERACION = WS-EXT-ELIMINAR
                   PERFORM 4020-MODO-I-O
               WHEN PARAM-OPERACION = WS-EXT-CONSULTAR
                   PERFORM 4010-MODO-INPUT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.


       1020-EJECUTAR-OPERACION.
      *------------------------
           EVALUATE TRUE
               WHEN PARAM-OPERACION = WS-EXT-REGISTRAR
                   PERFORM 2020-INSERTAR-REGISTROS
               WHEN PARAM-OPERACION = WS-EXT-ACTUALIZAR
                   PERFORM 2030-ACTUALIZAR-REGISTROS
               WHEN PARAM-OPERACION = WS-EXT-CONSULTAR
                   PERFORM 2040-CONSULTAR-REGISTROS
               WHEN PARAM-OPERACION = WS-EXT-ELIMINAR
                   PERFORM 2050-ELIMINAR-REGISTROS
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.


       2000-CREACION-ARCHIVOS.
      *-----------------------
           PERFORM 4000-MODO-OUTPUT.


       2010-CERRAR-ARCHIVOS.
      *---------------------
           EVALUATE TRUE
               WHEN WS-ARCHIVO-SELECCIONADO = WS-NOM-CIA
                   CLOSE ARCHIVO-CIA
               WHEN WS-ARCHIVO-SELECCIONADO = WS-NOM-CAR
                   CLOSE ARCHIVO-CAR
               WHEN WS-ARCHIVO-SELECCIONADO = WS-NOM-CIU
                   CLOSE ARCHIVO-CIU
               WHEN WS-ARCHIVO-SELECCIONADO = WS-NOM-PRO
                   CLOSE ARCHIVO-PRO
               WHEN OTHER
                   MOVE 0 TO WS-BANDERAS-ARCHIVO
                   CONTINUE
           END-EVALUATE.


       2020-INSERTAR-REGISTROS.
      *------------------------
           INITIALIZE WS-CAMPOS-INGRESADOS.

           EVALUATE TRUE
               WHEN WS-ARCHIVO-SELECCIONADO = WS-NOM-CIA

                   PERFORM 3000-PEDIR-DATOS-CIA

                   IF WS-CIA-CORRECTO
                      PERFORM 5010-MOVER-DATOS-CIA
                      WRITE CIA-DATOS
                   END-IF

               WHEN WS-ARCHIVO-SELECCIONADO = WS-NOM-CAR

                   PERFORM 3000-PEDIR-DATOS-CAR

                   IF WS-CARGO-CORRECTO
                       PERFORM 5020-MOVER-DATOS-CAR
                       WRITE CAR-DATOS
                   END-IF

               WHEN WS-ARCHIVO-SELECCIONADO = WS-NOM-CIU

                   PERFORM 3000-PEDIR-DATOS-CIU

               WHEN WS-ARCHIVO-SELECCIONADO = WS-NOM-PRO

                   PERFORM 3000-PEDIR-DATOS-PRO

               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

           IF FS-USADO NOT = "00"
              MOVE 0 TO WS-BANDERAS-ARCHIVO
              MOVE "OCURRIO UN ERROR DURANTE EL REGISTRO"
              TO WS-MSJ-OPERACION
           ELSE
               MOVE 1 TO WS-BANDERAS-ARCHIVO
               MOVE "OPERACION DE REGISTRO EXITOSA"
               TO WS-MSJ-OPERACION
           END-IF



       2030-ACTUALIZAR-REGISTROS.
      *--------------------------


       2040-CONSULTAR-REGISTROS.
      *-------------------------


       2050-ELIMINAR-REGISTROS.
      *------------------------


       3000-PEDIR-DATOS SECTION.
      *-------------------------
       3010-PEDIR-DATOS-CIA.
      *---------------------
           PERFORM 9999-MENU-DISPLAY-CIA.

           EVALUATE TRUE
               WHEN WS-CIA-CODIGO IS NOT NUMERIC OR
                    WS-CIA-CODIGO <= ZERO

                   MOVE "EL CODIGO NO ES VALIDO"
                   TO WS-MSJ-ACTUAL
                   MOVE 0 WS-BANDERA-CIA

               WHEN WS-CIA-DESCRIPCION IS SPACES

                   MOVE "LA DESCRIPCION NO PUEDE ESTAR VACIA"
                   TO WS-MSJ-ACTUAL
                   MOVE 0 WS-BANDERA-CIA

               WHEN WS-CIA-REPRESENTANTE IS SPACES

                 MOVE "EL NOMBRE DEL REPRESENTANTE NO PUEDE ESTAR VACIO"
                   TO WS-MSJ-ACTUAL
                   MOVE 0 WS-BANDERA-CIA

               WHEN WS-CIA-RUC IS NOT NUMERIC OR
                    WS-CIA-RUC <= ZERO

                   MOVE "EL RUC INGRESADO NO ES VALIDO"
                   TO WS-MSJ-ACTUAL
                   MOVE 0 WS-BANDERA-CIA

               WHEN WS-CIA-DIRECCION IS SPACES

                 MOVE "EL DIRECCION NO PUEDE ESTAR VACIA"
                   TO WS-MSJ-ACTUAL
                   MOVE 0 WS-BANDERA-CIA

               WHEN WS-CIA-TELEFONO IS NOT NUMERIC OR
                    WS-CIA-TELEFONO <= ZERO

                   MOVE "EL NUMERO DE TELEFONO NO ES VALIDO"
                   TO WS-MSJ-ACTUAL
                   MOVE 0 WS-BANDERA-CIA

               WHEN WS-CIA-ESTADO NOT = "A" OR
                    WS-CIA-ESTADO NOT = "I" OR

                    MOVE "EL ESTADO SOLO PUEDE SER A/I"
                    TO WS-MSJ-ACTUAL
                    MOVE 0 WS-BANDERA-CIA

               WHEN OTHER

                    MOVE 1 TO WS-BANDERA-CIA

           END-EVALUATE.

       3010-PEDIR-DATOS-CAR.
      *---------------------
           PERFORM 9999-MENU-DISPLAY-CAR.

           EVALUATE TRUE
               WHEN WS-CAR-CODIGO IS NOT NUMERIC OR
                    WS-CAR-CODIGO <= ZERO

                   MOVE "EL CODIGO NO ES VALIDO"
                   TO WS-MSJ-ACTUAL
                   MOVE 0 TO WS-BANDERA-CAR

               WHEN WS-PRO-DESCRIPCION IS SPACES

                   MOVE "LA DESCRIPCION NO PUEDE ESTAR VACIA"
                   TO WS-MSJ-ACTUAL
                   MOVE 0 WS-BANDERA-CAR

               WHEN WS-CAR-ESTADO NOT = "A" OR
                    WS-CAR-ESTADO NOT = "I" OR

                    MOVE "EL ESTADO SOLO PUEDE SER A/I"
                    TO WS-MSJ-ACTUAL
                    MOVE 0 WS-BANDERA-CAR

               WHEN OTHER

                    MOVE 1 TO WS-BANDERA-CAR

           END-EVALUATE.


      *---------------------
       3000-PEDIR-DATOS-FIN SECTION.
      *-----------------------------
           EXIT.


       4000-MODO-OUPUT.
      *------------------------
           EVALUATE TRUE
               WHEN WS-ARCHIVO-SELECCIONADO = WS-NOM-CIA
                   OPEN OUTPUT ARCHIVO-CIA
               WHEN WS-ARCHIVO-SELECCIONADO = WS-NOM-CAR
                   OPEN OUTPUT ARCHIVO-CAR
               WHEN WS-ARCHIVO-SELECCIONADO = WS-NOM-CIU
                   OPEN OUTPUT ARCHIVO-CIU
               WHEN WS-ARCHIVO-SELECCIONADO = WS-NOM-PRO
                   OPEN OUTPUT ARCHIVO-PRO
               WHEN OTHER
                   MOVE 0 TO WS-BANDERAS-ARCHIVO
                   CONTINUE
           END-EVALUATE.


       4010-MODO-INPUT.
      *----------------
           EVALUATE TRUE
               WHEN WS-ARCHIVO-SELECCIONADO = WS-NOM-CIA
                   OPEN INPUT ARCHIVO-CIA
               WHEN WS-ARCHIVO-SELECCIONADO = WS-NOM-CAR
                   OPEN INPUT ARCHIVO-CAR
               WHEN WS-ARCHIVO-SELECCIONADO = WS-NOM-CIU
                   OPEN INPUT ARCHIVO-CIU
               WHEN WS-ARCHIVO-SELECCIONADO = WS-NOM-PRO
                   OPEN INPUT ARCHIVO-PRO
               WHEN OTHER
                   MOVE 0 TO WS-BANDERAS-ARCHIVO
                   CONTINUE
           END-EVALUATE.


       4020-MODO-I-O.
      *--------------
           EVALUATE TRUE
               WHEN WS-ARCHIVO-SELECCIONADO = WS-NOM-CIA
                   OPEN I-O ARCHIVO-CIA
               WHEN WS-ARCHIVO-SELECCIONADO = WS-NOM-CAR
                   OPEN I-O ARCHIVO-CAR
               WHEN WS-ARCHIVO-SELECCIONADO = WS-NOM-PRO
                   OPEN I-O ARCHIVO-PRO
               WHEN WS-ARCHIVO-SELECCIONADO = WS-NOM-CIU
                   OPEN I-O ARCHIVO-CIU
               WHEN OTHER
                   MOVE 0 TO WS-BANDERAS-ARCHIVO
                   CONTINUE
           END-EVALUATE.


       5000-MOVER-DATOS SECTION.
      *-------------------------

       5010-MOVER-DATOS-CIA.
      *---------------------
           MOVE WS-CIA-CODIGO      TO CIA-CODIGO.
           MOVE WS-CIA-DESCRIPCION TO CIA-DESCRIPCION.
           MOVE WS-CIA-ESTADO      TO CIA-ESTADO.
           MOVE WS-CIA-RUC         TO CIA-RUC.
           MOVE WS-CIA-DIRECCION   TO CIA-DIRECCION.
           MOVE WS-CIA-TELEFONO    TO CIA-TELE-CONVENCIONAL.

       5020-MOVER-DATOS-CAR.
      *---------------------
           MOVE WS-CAR-CODIGO      TO CAR-CODIGO.
           MOVE WS-CAR-DESCRIPCION TO CAR-DESCRIPCION.
           MOVE WS-CAR-ESTADO      TO CAR-ESTADO.

       5030-MOVER-DATOS-CIU.
      *---------------------
           MOVE WS-CIU-CODIGO      TO CIU-CODIGO.
           MOVE WS-CIU-DESCRIPCION TO CIU-DESCRIPCION.

       5040-MOVER-DATOS-PRO.
      *---------------------
           MOVE WS-PRO-CODIGO      TO PRO-CODIGO.
           MOVE WS-PRO-DESCRIPCION TO PRO-DESCRIPCION.
           MOVE WS-PRO-ESTADO      TO PRO-ESTADO.

       5000-MOVER-DATOS-FIN SECTION.
      *-----------------------------
           EXIT.


       9999-MENUS-DISPLAY SECTION.
      *---------------------------
       9999-MENU-DISPLAY-CIA.
      *------------------
           DISPLAY "INGRESE EL CODIGO DE LA EMPRESA: ".
           ACCEPT WS-CIA-CODIGO.
           DISPLAY "INGRESE EL NOMBRE DE LA EMPRESA: ".
           ACCEPT WS-CIA-DESCRIPCION.
           DISPLAY "INGRESE EL REPRESENTANTE LEGAL DE LA EMPRESA: ".
           ACCEPT WS-CIA-REPRESENTANTE.
           DISPLAY "INGRESE EL RUC DE LA EMPRESA: ".
           ACCEPT WS-CIA-REPRESENTANTE.
           DISPLAY "INGRESE LA DIRECCION DE LA EMPRESA: ".
           ACCEPT WS-CIA-DIRECCION.
           DISPLAY "INGRESE EL TELF. CONVENCIONAL DE LA EMPRESA: ".
           ACCEPT WS-CIA-TELEFONO.
           DISPLAY "INGRESE EL ESTADO DE LA EMPRESA (A/I): ".
           ACCEPT WS-CIA-ESTADO.

       9999-MENU-DISPLAY-CAR.
      *----------------------
           DISPLAY "INGRESE EL CODIGO DEL CARGO: ".
           ACCEPT WS-CAR-CODIGO.
           DISPLAY "INGRESE EL NOMBRE DEL CARGO: ".
           ACCEPT WS-CAR-DESCRIPCION.
           DISPLAY "INGRESE EL ESTADO DEL CARGO: ".
           ACCEPT WS-CAR-ESTADO.

       9999-MENU-DISPLAY-CIU.
      *----------------------
           DISPLAY "INGRESE EL CODIGO DE LA CIUDAD: ".
           ACCEPT WS-CIU-CODIGO.
           DISPLAY "INGRESE EL NOMBRE DE LA CIUDAD: ".
           ACCEPT WS-CIU-DESCRIPCION.

       9999-MENU-DISPLAY-PRO.
      *----------------------
           DISPLAY "INGRESE EL CODIGO DE LA PROFESION: ".
           ACCEPT WS-PRO-CODIGO.
           DISPLAY "INGRESE EL NOMBRE DE LA PROFESION: ".
           ACCEPT WS-PRO-DESCRIPCION.
           DISPLAY "INGRESE ESTADO DE LA PROFESION: ".
           ACCEPT WS-PRO-ESTADO.

       9999-MENUS-DISPLAY SECTION.
      *---------------------------
           EXIT.
