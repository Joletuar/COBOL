      *******************************************************************
      *                                                                 *
      *                                                                 *
      *                                                                 *
      *                                                                 *
      *******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. PARAMENU.
       AUTHOR. JOHAN TUAREZ.
       DATE-WRITTEN. 27/06/2022.

       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------

       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
      *-----------------------

      *-----------------------------------------------------------------*
      *                          VARIABLES                              *
      *-----------------------------------------------------------------*

       01  WS-OPCION                             PIC S9(2).
           88 WS-OPC-1 VALUE 1.
           88 WS-OPC-2 VALUE 2.
           88 WS-OPC-3 VALUE 3.
           88 WS-OPC-4 VALUE 4.
           88 WS-OPC-5 VALUE 5.

      *-----------------------------------------------------------------*
      *                        SWITCHES/BANDERA                         *
      *-----------------------------------------------------------------*

       01  WS-SWITCHES.
           03 WS-SWITCH-OPC                      PIC 9.
               88 WS-SWITCH-OPC-SI VALUE 1.
               88 WS-SWITCH-OPC-NO VALUE 0.

      *-----------------------------------------------------------------*
      *                          CONSTANTES                             *
      *-----------------------------------------------------------------*

       77  WS-DECORADOR                        PIC X(30) VALUE ALL "*-".
       77  WS-ESPACIADO                        PIC X(10) VALUE ALL " ".

       LINKAGE SECTION.
      *----------------

       01  PARAMETRO PIC 9.

       PROCEDURE DIVISION USING PARAMETRO.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.

       0000-MAIN.
      *----------
           INITIALIZE WS-OPCION.

           PERFORM 1000-PROCESO UNTIL WS-OPC-5.

           GOBACK.

       1000-PROCESO.
      *-------------
           INITIALIZE WS-SWITCH-OPC.

           PERFORM 2000-INGRESO-OPCION UNTIL WS-SWITCH-OPC-SI.

       2000-INGRESO-OPCION.
      *--------------------
           PERFORM 9999-DISPLAY-MENU-PRINCIPAL.

           MOVE 1 TO WS-SWITCH-OPC.

           INITIALIZE WS-OPCION.

           ACCEPT WS-OPCION.

           EVALUATE TRUE
               WHEN WS-OPC-1
                 CALL "CRUD-CIA" USING BY CONTENT PARAMETRO
               WHEN WS-OPC-2

               WHEN WS-OPC-3

               WHEN WS-OPC-4

               WHEN WS-OPC-5
                   CONTINUE
               WHEN OTHER
                   DISPLAY "OPCION INVALIDA"
                   MOVE 0 TO WS-SWITCH-OPC
           END-EVALUATE.

       9999-DISPLAY-MENU-PRINCIPAL.
      *----------------------------
           DISPLAY WS-ESPACIADO.
           DISPLAY "--- MENU PARAMETRIZACION ---".
           DISPLAY WS-ESPACIADO.
           DISPLAY " 1.- EMPRESA".
           DISPLAY " 2.- PROFESIONES".
           DISPLAY " 3.- CIUDAD".
           DISPLAY " 4.- CARGOS".
           DISPLAY " 5.- MENU ANTERIOR".
