       FD  ARCHIVO-NOMINAS.
           01 NOM-DATOS.
              03 NOM-CLAVE.
                05 NOM-COD-EMPRESA     PIC 9(3).
                05 NOM-COD-EMPLEADO    PIC 9(5).
                05 NOM-FECHA-NOMINA.
                  07 NOM-FEC-NOMINA-AA PIC 9(4).
                  07 NOM-FEC-NOMINA-MM PIC 9(2).
                05 NOM-TIPO-RUBRO      PIC 9(2).
              03 NOM-VALOR             PIC 9(8)V9(2).
              03 NOM-OBSERVACION       PIC X(50).
              03 NOM-FECHA-PROCESO.
                05 NOM-FEC-PROC-AA     PIC 9(4).
                05 NOM-FEC-PROC-MM     PIC 9(2).
                05 NOM-FEC-PROC-DD     PIC 9(2).
              03 NOM-HORA-PROCESO.
                05 NOM-HORA-PROC-HH    PIC 9(2).
                05 NOM-HORA-PROC-MM    PIC 9(2).
                05 NOM-HORA-PROC-SS    PIC 9(2).
