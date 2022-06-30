       FD  ARCHIVO-DESCUENTOS.
           01 DES-DATOS.
              03 DES-CLAVE.
                 05 DES-COD-EMPRESA      PIC 9(5).
                 05 DES-COD-EMPLEADO     PIC 9(5).
                 05 DES-FECHA-NOVEDAD.
                   07 DES-FEC-NOVE-AA    PIC 9(4).
                   07 DES-FEC-NOVE-MM    PIC 9(2).
                   07 DES-FEC-NOVE-DD    PIC 9(2).
                 05 DES-TIPO-DESCUENTO   PIC 9(2).
              03 DES-MOTIVO              PIC X(50).
              03 DES-VALOR               PIC 9(8)V9(2).
              03 DES-ESTADO              PIC X(1).
