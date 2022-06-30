      *******************************************************************
      *                                                                 *
      *                                                                 *
      *                                                                 *
      *                                                                 *
      *******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. MAINMENU.
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

       01  WS-OPCION                           PIC S9(2).
           88 WS-OPC-1 VALUE 1.
           88 WS-OPC-2 VALUE 2.
           88 WS-OPC-3 VALUE 3.
           88 WS-OPC-4 VALUE 4.

      *-----------------------------------------------------------------*
      *                        SWITCHES/BANDERA                         *
      *-----------------------------------------------------------------*

       01  WS-SWITCHES.
           03 WS-SWITCH-OPC                    PIC 9.
               88 WS-SWITCH-OPC-SI VALUE 1.
               88 WS-SWITCH-OPC-NO VALUE 0.

      *-----------------------------------------------------------------*
      *                          CONSTANTES                             *
      *-----------------------------------------------------------------*

       01  WS-PARAM-CONSTANTES.
           03 WS-PARAM-CONST-1                 PIC 9 VALUE 1.
           03 WS-PARAM-CONST-2                 PIC 9 VALUE 2.
           03 WS-PARAM-CONST-3                 PIC 9 VALUE 3.

       77  WS-DECORADOR                        PIC X(30) VALUE ALL "*-".
       77  WS-ESPACIADO                        PIC X(10) VALUE ALL " ".

       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.

       0000-MAIN.
      *----------
           PERFORM 9999-DISPLAY-BIENVENIDA.

           PERFORM 1000-PROCESO UNTIL WS-OPC-4.

           PERFORM 9999-DISPLAY-DESPEDIDA.

           STOP RUN.

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
                   CALL "PARAMENU" USING BY CONTENT WS-PARAM-CONST-1
               WHEN WS-OPC-2
                   DISPLAY "OPCION 2"
               WHEN WS-OPC-3
                   DISPLAY "OPCION 3"
               WHEN WS-OPC-4
                   CONTINUE
               WHEN OTHER
                   DISPLAY "OPCION INVALIDA"
                   MOVE 0 TO WS-SWITCH-OPC
           END-EVALUATE.

       9999-DISPLAY-BIENVENIDA.
      *------------------------
           DISPLAY WS-DECORADOR.
           DISPLAY "          BIENVENIDO".
           DISPLAY WS-DECORADOR.

       9999-DISPLAY-MENU-PRINCIPAL.
      *----------------------------
           DISPLAY WS-ESPACIADO.
           DISPLAY "--- MENU PRINCIPAL ---".
           DISPLAY WS-ESPACIADO.
           DISPLAY " 1.- PARAMETRIZACION".
           DISPLAY " 2.- NOMINA".
           DISPLAY " 3.- PROCESOS NOMINA".
           DISPLAY " 4.- SALIR".

       9999-DISPLAY-DESPEDIDA.
      *-----------------------
           DISPLAY WS-ESPACIADO.
           DISPLAY WS-DECORADOR.
           DISPLAY "   SALIENDO DEL PROGRAMA...".
           DISPLAY WS-DECORADOR.
