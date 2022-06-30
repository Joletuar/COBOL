       FD  ARCHIVO-HORAS-EXTRAS.
           01 HEX-DATOS.
              03 HEX-CLAVE.
                05 HEX-COD-EMPRESA     PIC 9(5).
                05 HEX-COD-EMPLEADO    PIC 9(5).
                05 HEX-FECHA-NOVEDAD.
                  07 HEX-FEC-NOVE-AA   PIC 9(4).
                  07 HEX-FEC-NOVE-MM   PIC 9(2).
                  07 HEX-FEC-NOVE-DD   PIC 9(2).
                05 TIPO-NOVEDAD        PIC 9(2).
              03 HEX-FECHA-INICIAL.
                05 HEX-FEC-INI-AA      PIC 9(4).
                05 HEX-FEC-INI-MM      PIC 9(2).
                05 HEX-FEC-INI-DD      PIC 9(2).
              03 HEX-HORA-INICIAL.
                05 HEX-HORA-INI-HH     PIC 9(2).
                05 HEX-HORA-INI-MM     PIC 9(2).
                05 HEX-HORA-INI-SS     PIC 9(2).
              03 HEX-FECHA-FINAL.
                05 HEX-FEC-FIN-AA      PIC 9(4).
                05 HEX-FEC-FIN-MM      PIC 9(2).
                05 HEX-FEC-FIN-DD      PIC 9(2).
              03 HEX-HORA-FINAL.
                05 HEX-HORA-FIN-HH     PIC 9(2).
                05 HEX-HORA-FIN-MM     PIC 9(2).
                05 HEX-HORA-FIN-SS     PIC 9(2).
              03 HEX-OBSERVACION       PIC X(50).
              03 HEX-VALOR-HORAS       PIC 9(8)V9(2).
              03 HEX-ESTADO            PIC X(1).
