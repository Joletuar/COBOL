      *******************************************************************
      *                                                                 *
      *                                                                 *
      *                                                                 *
      *                                                                 *
      *******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. CRUD-CIA.
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

       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
            COPY "./Copys/LOGI-CIA.cpy".

       WORKING-STORAGE SECTION.

       01  FS-CIA                  PIC XX.

       01  WS-EMPRESA.
           03 WS-CIA-CODIGO        PIC 9(3).
           03 WS-CIA-DESCRIPCION   PIC X(30).
           03 WS-CIA-ESTADO        PIC X.
           03 WS-CIA-REPRESENTANTE PIC X(30).
           03 WS-CIA-RUC           PIC 9(10).
           03 WS-CIA-DIRECCION     PIC X(40).
           03 WS-CIA-TELEFONO      PIC 9(10).

       01  WS-OPCION               PIC S9(2).

       01  WS-BANDERA-CAMPOS       PIC 9.
           88 WS-CAMPOS-CORRECTO    VALUE 1.
           88 WS-CCAMPOS-INCORRECTO VALUE 0.

       01  WS-BANDERA-REGISTRO     PIC 9.
           88 WS-REG-ENCONTRADO      VALUE 1.
           88 WS-REG-NO-ENCONTRADO   VALUE 0.

       77  WS-DECORADOR-1           PIC X(30) VALUE ALL "*-".
       77  WS-DECORADOR-2           PIC X(30) VALUE ALL "-".
       77  WS-ESPACIADO             PIC X(10) VALUE ALL " ".

       LINKAGE SECTION.
      *----------------

       01  PARAMETRO PIC 9.

       PROCEDURE DIVISION USING PARAMETRO.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.

       0000-MAIN.
      *----------
           INITIALIZE WS-OPCION FS-CIA WS-BANDERA-CAMPOS
           WS-BANDERA-REGISTRO CIA-DATOS.

           PERFORM 9999-DISPLAY-MENU-CRUD.

           PERFORM 1000-INICIO.

           IF FS-CIA NOT = "00"
               DISPLAY "EL ARCHIVO NO SE PUDO CREAR"
           ELSE
               ACCEPT WS-OPCION
               PERFORM 1000-PROCESOS
           END-IF

           GOBACK.

       1000-INICIO.
      *------------------------------
           PERFORM 4000-ABRIR-ARCHIVO-INPUT.

           IF FS-CIA NOT = "00"
             PERFORM 4000-CERRAR-ARCHIVO
             PERFORM 4000-ABRIR-ARCHIVO-OUTPUT
           END-IF.

           PERFORM 4000-CERRAR-ARCHIVO.

       1000-PROCESOS.
      *--------------
           PERFORM 1000-SELECCION-MODO.

           EVALUATE TRUE
               WHEN WS-OPCION = 1

                 PERFORM 2000-PEDIR-ID
                 PERFORM 2010-PEDIR-DATOS
                 PERFORM 3000-VALIDAR-ID
                 PERFORM 3000-VALIDACION-DATOS

                 IF WS-REG-NO-ENCONTRADO
                     IF WS-CAMPOS-CORRECTO
                       PERFORM 3000-MOVER-DATOS
                       PERFORM 4000-ESCRIBIR-REGISTRO
                     ELSE
                       DISPLAY "NO SE PUDO INSERTAR EL REGISTRO"
                   END-IF
                 ELSE
                    DISPLAY "YA EXISTE UN REGISTRO CON DICHO CODIGO"
                    DISPLAY "NO SE PUDO INSERTAR EL REGISTRO"
                 END-IF

               WHEN WS-OPCION = 2

                 PERFORM 2000-PEDIR-ID
                 PERFORM 3000-VALIDAR-ID

                 IF WS-CAMPOS-CORRECTO AND WS-REG-ENCONTRADO
                   PERFORM 2010-PEDIR-DATOS
                   PERFORM 3000-VALIDACION-DATOS
                   IF WS-CAMPOS-CORRECTO
                     PERFORM 4000-ACTUALIZAR-REGISTRO
                   ELSE
                     DISPLAY "NO SE PUDO ACTUALIZAR EL REGISTRO"
                 ELSE
                   DISPLAY "NO SE PUDO ACTUALIZAR EL REGISTRO"
                 END-IF

               WHEN WS-OPCION = 3

                 PERFORM 2000-PEDIR-ID
                 PERFORM 3000-VALIDAR-ID

                 IF WS-CAMPOS-CORRECTO AND WS-REG-ENCONTRADO
                   PERFORM 9999-IMPRESION-DATOS
                 ELSE
                   DISPLAY "NO SE ENCONTRO EL REGISTRO"
                 END-IF

               WHEN WS-OPCION = 4

                 PERFORM 2000-PEDIR-ID
                 PERFORM 3000-VALIDAR-ID

                 IF WS-CAMPOS-CORRECTO AND WS-REG-ENCONTRADO
                   PERFORM 4000-BORAR-REGISTRO
                 ELSE
                   DISPLAY "NO SE ENCONTRO EL REGISTRO"
                 END-IF

               WHEN OTHER

                 DISPLAY "OPCION ELEGIDA NO VALIDA"

           END-EVALUATE.

           PERFORM 4000-CERRAR-ARCHIVO.


       1000-SELECCION-MODO.
      *--------------------
           EVALUATE TRUE
               WHEN WS-OPCION = 1
               WHEN WS-OPCION = 2
               WHEN WS-OPCION = 4
                 PERFORM 4000-ABRIR-ARCHIVO-I-O
               WHEN WS-OPCION = 3
                 PERFORM 4000-ABRIR-ARCHIVO-INPUT
               WHEN OTHER
                 CONTINUE
           END-EVALUATE.


       2000-PEDIR-ID.
      *--------------
           PERFORM 9999-MENU-DISPLAY-ID.


       2010-PEDIR-DATOS.
      *-----------------
           PERFORM 9999-MENU-DISPLAY-DATOS.


       3000-VALIDACION-DATOS.
      *----------------------
           EVALUATE TRUE
               WHEN WS-CIA-DESCRIPCION = SPACES
                 MOVE 0 TO WS-BANDERA-CAMPOS
                 DISPLAY "ERROR AL INGRESAR LA DESCRIPCION"
               WHEN WS-CIA-REPRESENTANTE = SPACES
                 MOVE 0 TO WS-BANDERA-CAMPOS
                 DISPLAY "ERROR LA INGRESAR EL REPRESENTANTE "
               WHEN WS-CIA-RUC IS NOT NUMERIC OR
                    WS-CIA-RUC <= ZEROS
                 MOVE 0 TO WS-BANDERA-CAMPOS
                 DISPLAY "ERROR AL INGRESAR EL RUC"
               WHEN WS-CIA-DIRECCION = SPACES
                 MOVE 0 TO WS-BANDERA-CAMPOS
                 DISPLAY "ERROR AL INGRESAR LA DIRECCION"
               WHEN WS-CIA-TELEFONO IS NOT NUMERIC OR
                    WS-CIA-TELEFONO <= ZEROS
                 MOVE 0 TO WS-BANDERA-CAMPOS
                 DISPLAY "ERROR AL INGRESAR EL TELEFONO"
               WHEN WS-CIA-ESTADO = SPACE
                 DISPLAY "ERROR AL INGRESAR EL ESTADO"
                 IF NOT (WS-CIA-ESTADO = "A" OR WS-CIA-ESTADO = "I")
                   DISPLAY "EL ESTADO SOLO PUEDE SER A/I"
                 END-IF
                 MOVE 0 TO WS-BANDERA-CAMPOS
               WHEN OTHER
                 MOVE 1 TO WS-BANDERA-CAMPOS
           END-EVALUATE.

       3000-VALIDAR-ID.
      *----------------
           IF WS-CIA-CODIGO IS NOT NUMERIC OR
                    WS-CIA-CODIGO <= ZERO
               MOVE 0 TO WS-BANDERA-CAMPOS
               DISPLAY "ERROR AL INGRESAR EL CODIGO"
           ELSE
               MOVE 1 TO WS-BANDERA-CAMPOS
               MOVE WS-CIA-CODIGO TO CIA-CODIGO
               PERFORM 4000-LEER-REGISTRO
           END-IF.


       3000-MOVER-DATOS.
      *-----------------
           MOVE WS-CIA-CODIGO      TO CIA-CODIGO.
           MOVE WS-CIA-DESCRIPCION TO CIA-DESCRIPCION.
           MOVE WS-CIA-ESTADO      TO CIA-ESTADO.
           MOVE WS-CIA-RUC         TO CIA-RUC.
           MOVE WS-CIA-DIRECCION   TO CIA-DIRECCION.
           MOVE WS-CIA-TELEFONO    TO CIA-TELE-CONVENCIONAL.


       4000-ABRIR-ARCHIVO-INPUT.
      *-------------------------
           OPEN INPUT ARCHIVO-EMPRESAS.

       4000-ABRIR-ARCHIVO-OUTPUT.
      *--------------------------
           OPEN OUTPUT ARCHIVO-EMPRESAS.

       4000-ABRIR-ARCHIVO-I-O.
      *--------------------------
           OPEN I-O ARCHIVO-EMPRESAS.


       4000-LEER-REGISTRO.
      *-------------------
           READ ARCHIVO-EMPRESAS
           END-READ.

           IF FS-CIA = "00"
                 MOVE 1 TO WS-BANDERA-REGISTRO
           ELSE
                 MOVE 0 TO WS-BANDERA-REGISTRO
           END-IF.

       4000-ESCRIBIR-REGISTRO.
      *-----------------------
           WRITE CIA-DATOS.

           IF FS-CIA = "00"
              DISPLAY "REGISTRO INSERTADO CORRECTAMENTE"
           ELSE
              DISPLAY "ERROR DE ARCHIVO AL REGISTRAR"
              DISPLAY "NO SE PUDO INSERTAR EL REGISTRO"
           END-IF.

       4000-ACTUALIZAR-REGISTRO.
      *-------------------------
           REWRITE CIA-DATOS.

           IF FS-CIA = "00"
              DISPLAY "REGISTRO ACTUALIZADO CORRECTAMENTE"
           ELSE
              DISPLAY"ERROR DE ARCHIVO AL ACTUALIZAR"
              DISPLAY "NO SE PUDO ACTUALIZAR EL REGISTRO"
           END-IF.

       4000-BORAR-REGISTRO.
      *--------------------
           DELETE ARCHIVO-EMPRESAS.

           IF FS-CIA = "00"
              DISPLAY "REGISTRO ELIMINADO CORRECTAMENTE"
           ELSE
              DISPLAY "ERROR DE ARCHIVO AL ELIMINAR"
              DISPLAY "NO SE PUDO INSERTAR EL REGISTRO"
           END-IF.

       4000-CERRAR-ARCHIVO.
      *--------------------
           CLOSE ARCHIVO-EMPRESAS.


       9999-DISPLAY-MENU-CRUD.
      *-----------------------
           DISPLAY WS-ESPACIADO.
           DISPLAY "### PARAMETRIZACION DE: EMPRESA ###".
           DISPLAY WS-ESPACIADO.
           DISPLAY " 1.- REGISTRAR".
           DISPLAY " 2.- ACTUALIZAR".
           DISPLAY " 3.- CONSULTAR".
           DISPLAY " 4.- ELIMINAR".


       9999-MENU-DISPLAY-DATOS.
      *------------------
           DISPLAY "INGRESE LA DESCRIPCION DE LA EMPRESA: ".
           ACCEPT WS-CIA-DESCRIPCION.
           DISPLAY "INGRESE EL REPRESENTANTE LEGAL DE LA EMPRESA: ".
           ACCEPT WS-CIA-REPRESENTANTE.
           DISPLAY "INGRESE EL RUC DE LA EMPRESA: ".
           ACCEPT WS-CIA-RUC.
           DISPLAY "INGRESE LA DIRECCION DE LA EMPRESA: ".
           ACCEPT WS-CIA-DIRECCION.
           DISPLAY "INGRESE EL TELF. CONVENCIONAL DE LA EMPRESA: ".
           ACCEPT WS-CIA-TELEFONO.
           DISPLAY "INGRESE EL ESTADO DE LA EMPRESA (A/I): ".
           ACCEPT WS-CIA-ESTADO.

       9999-MENU-DISPLAY-ID.
      *---------------------
           DISPLAY "INGRESE EL CODIGO DE LA EMPRESA: ".
           ACCEPT WS-CIA-CODIGO.

       9999-IMPRESION-DATOS.
      *---------------------
           DISPLAY WS-DECORADOR-2.
           DISPLAY "CODIGO:             " CIA-CODIGO.
           DISPLAY "DESCRIPCION:        " CIA-DESCRIPCION.
           DISPLAY "RUC:                " CIA-RUC
           DISPLAY "DIRECCION:          " CIA-DIRECCION
           DISPLAY "TELF. CONVENCIONAL: " CIA-TELE-CONVENCIONAL.
           DISPLAY "ESTADO:             " CIA-ESTADO.
           DISPLAY WS-DECORADOR-2.
