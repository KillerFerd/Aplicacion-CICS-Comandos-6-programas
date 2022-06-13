      ******************************************************************
      * FECHA       : 03/06/2022                                       *
      * PROGRAMADOR : JOSUE DONIS                                      *
      * APLICACION  : SEMILLERO                                        *
      * PROGRAMA    : EDID1CL5                                         *
      * TIPO        : LINEA                                            *
      * DESCRIPCION : MENU DE CONSULTA PARA MAESTRO DE CLIENTES        *
      * ARCHIVOS    : EDM4CL                                           *
      * ACCION (ES) : C=CONSULTAR                                      *
      * PROGRAMA(S) : XTCL                                             *
      * CANAL       : ADMINISTRATIVA                                   *
      * INSTALADO   : 03/06/2022                                       *
      * BPM/RATIONAL:                                                  *
      * NOMBRE      : EDGAR MARTINEZ - INSTRUCOR                       *
      * DESCRIPCION : USER5004                                         *
      ******************************************************************
       ID DIVISION.
       PROGRAM-ID. EDID1CL5.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY DFHBMSCA.
       COPY DFHAID.
       COPY EDCL5.

       COPY EDMACL.


       01 WKS-WORK-FIELDS.
           02 WKS-PAGE-LIMIT            PIC 9(02) VALUE 13.
           02 WKS-END-MSG               PIC X(14)
           VALUE "MUCHAS GRACIAS".

       01 WKS-FILE-STATUS               PIC 99 VALUE ZEROS.
           88 WKS-EDM4CL-NOTOPEN        VALUE 1.
           88 WKS-EDM4CL-NOTFND         VALUE 2.
           88 WKS-EDM4CL-NORMAL         VALUE 3.

       01 WKS-FLAGS.
           02 WKS-OUTPUT-MSG            PIC 9 VALUE ZEROS.
                88 WKS-MSG-NOCOMAND     VALUE 1.
                88 WKS-MSG-UNKERROR     VALUE 2.
                88 WKS-MSG-NOTOPEN      VALUE 3.
           02 WKS-FLAG-FIRST-MATCH      PIC 9 VALUE ZEROS.

       01 WKS-SUBSCRIPTS.
           02 WKS-INDEX                 PIC 99 VALUE ZEROS.

       01 WKS-PROGRAM-SPECS.
           02 WKS-PROGRAM-NAME          PIC X(08) VALUE "EDID1CL5".
           02 WKS-PROGRAM-2             PIC X(08) VALUE "EDID1QLI".
           02 WKS-PROGRAM-8             PIC X(08) VALUE "EDID1CLS".
           02 WKS-COMMAREA.
               03 WKS-COM-LAST-POS      PIC 9(08).
               03 WKS-COM-FIRST-POS     PIC 9(08).
               03 WKS-COM-CO-CLIENTE    PIC X(08) OCCURS 14.
               03 WKS-COM-OPTION        PIC X(01) OCCURS 13.
               03 WKS-COM-MODE          PIC 9(01).
               03 WKS-COM-RETURN        PIC 9(01).
               03 WKS-COM-RETURNPROG    PIC 9(01).
               03 WKS-COM-LAST-POS-A    PIC X(40).
               03 WKS-COM-FIRST-POS-A   PIC X(40).


       01 WKS-EDITED-FIELDS.
           02 WKS-DATE-SIS.
               03 WKS-DATE-DD           PIC 99.
               03 WKS-DATE-MM           PIC /99.
               03 WKS-DATE-YYYY         PIC /9999.
           02 WKS-TIME-SIS.
               03 WKS-TIME-HH           PIC 99.
               03 FILLER                PIC X VALUE ":".
               03 WKS-TIME-MM           PIC 99.
               03 FILLER                PIC X VALUE ":".
               03 WKS-TIME-SS           PIC 99.
           02 WKS-DATE-FORMAT           PIC 99/99/9999.

       LINKAGE SECTION.
       01 DFHCOMMAREA                   PIC X(250).

       PROCEDURE DIVISION.
       000-MAIN-PROCESS.

      *-->MOVER COMMAREA
           IF EIBCALEN NOT = 0
               MOVE DFHCOMMAREA TO WKS-COMMAREA
           END-IF

      *-->EVALUATE PARA LA ACCION DE CADA TECLA
           EVALUATE TRUE
                WHEN EIBCALEN = 0
                    PERFORM 100-ACCION-DEFAULT
                WHEN EIBTRNID = 'EDQI'
                WHEN EIBTRNID = 'EDCS'
                    PERFORM 100-ACCION-DEFAULT
                WHEN EIBAID = DFHPF10
                    PERFORM 200-ACCION-PF10
                WHEN OTHER
                    PERFORM 300-ACCION-OTHER
           END-EVALUATE.

      *-->ACCION DEFAULT
       100-ACCION-DEFAULT.
           PERFORM 801-EXEC-CICS-SEND-ERASE
           PERFORM 110-PROCESS-QUEARY
           PERFORM 802-EXEC-CICS-SEND-DATA
           PERFORM 803-EXEC-CICS-RETURN.

      *-->PROCESO DE CONSULTA
       110-PROCESS-QUEARY.
           MOVE 1 TO WKS-INDEX
           PERFORM UNTIL WKS-INDEX > WKS-PAGE-LIMIT
           OR WKS-FLAG-FIRST-MATCH = 1
                IF WKS-COM-OPTION(WKS-INDEX) = "S"
                    PERFORM 120-PROCESS-MATCH
                END-IF
                ADD 1 TO WKS-INDEX
           END-PERFORM
           MOVE ZEROS TO WKS-COM-MODE

           SET WKS-MSG-UNKERROR TO TRUE.

       120-PROCESS-MATCH.
           MOVE 1 TO WKS-FLAG-FIRST-MATCH
           MOVE SPACES TO WKS-COM-OPTION(WKS-INDEX)
           MOVE WKS-COM-CO-CLIENTE(WKS-INDEX) TO EDMC-LLAVE
           PERFORM 804-EXEC-CICS-READ
           IF WKS-EDM4CL-NOTOPEN
                SET WKS-MSG-NOTOPEN TO TRUE
           ELSE IF WKS-EDM4CL-NOTFND
               SET WKS-MSG-UNKERROR TO TRUE
           ELSE
                PERFORM 130-PROCESS-DATA
           END-IF
           PERFORM 802-EXEC-CICS-SEND-DATA
           PERFORM 803-EXEC-CICS-RETURN.

       130-PROCESS-DATA.
           MOVE EDMC-LLAVE                 TO EDCL5-CO-CLIENTEO
           MOVE EDMC-NOMBRE-CLIENTE        TO EDCL5-NOM-CLIENTEO
           MOVE EDMC-FECHA-NAC-O-CONSTITUC TO WKS-DATE-FORMAT
           MOVE WKS-DATE-FORMAT            TO EDCL5-FECHA-NACO
           MOVE EDMC-NUMERO-TELEFONO       TO EDCL5-TELO
           MOVE EDMC-FECHA-CREACION        TO WKS-DATE-FORMAT
           MOVE WKS-DATE-FORMAT            TO EDCL5-FECHA-ADICO.

      *-->ACCION PARA SALIR AL MENU DE BROWSE
       200-ACCION-PF10.
           MOVE 1 TO WKS-COM-RETURN
           IF WKS-COM-MODE = 1
                PERFORM 110-PROCESS-QUEARY
           END-IF
           IF WKS-COM-RETURNPROG = 1
                PERFORM 805-EXEC-CICS-XCTL-PROGRAM-2
           END-IF
           IF WKS-COM-RETURNPROG = 2
                PERFORM 806-EXEC-CICS-XCTL-PROGRAM-8
           END-IF
           PERFORM 999-END-PROGRAM.

      *-->ACCION QUE DESPLIEGA MENSAJE DE COMANDO NO HABILITADO
       300-ACCION-OTHER.
           SET WKS-MSG-NOCOMAND TO TRUE
           PERFORM 802-EXEC-CICS-SEND-DATA
           PERFORM 803-EXEC-CICS-RETURN.

      *--> COMANDOS CICS
       801-EXEC-CICS-SEND-ERASE.
           EXEC CICS SEND
                MAP('EDCL5')
                MAPSET('EDCL5')
                ERASE
                DEFAULT
           END-EXEC.

       802-EXEC-CICS-SEND-DATA.
           PERFORM 998-PROCESS-DEFAULT-DATA
           PERFORM 997-PROCESS-OUTPUT-MSG
           EXEC CICS SEND
               MAP('EDCL5')
               MAPSET('EDCL5')
               DATAONLY
           END-EXEC.

       803-EXEC-CICS-RETURN.
           EXEC CICS RETURN
               TRANSID('EDC5')
               COMMAREA(WKS-COMMAREA)
           END-EXEC.

       804-EXEC-CICS-READ.
           EXEC CICS READ
               FILE('EDM4CL')
               INTO(REG-EDMACL)
               RIDFLD(EDMC-LLAVE)
               NOHANDLE
           END-EXEC
           PERFORM 899-EVALUATE-DFHRESP.

       805-EXEC-CICS-XCTL-PROGRAM-2.
           EXEC CICS XCTL
               PROGRAM(WKS-PROGRAM-2)
               COMMAREA(WKS-COMMAREA)
           END-EXEC.

       806-EXEC-CICS-XCTL-PROGRAM-8.
           EXEC CICS XCTL
               PROGRAM(WKS-PROGRAM-8)
               COMMAREA(WKS-COMMAREA)
           END-EXEC.

       899-EVALUATE-DFHRESP.
           EVALUATE EIBRESP
                WHEN DFHRESP(NOTOPEN) SET WKS-EDM4CL-NOTOPEN TO TRUE
                WHEN DFHRESP(NORMAL)  SET WKS-EDM4CL-NORMAL  TO TRUE
                WHEN DFHRESP(NOTFND)  SET WKS-EDM4CL-NOTFND  TO TRUE
           END-EVALUATE.

      *--> PROCESAR MENSAJES DE SALIDA
       997-PROCESS-OUTPUT-MSG.
           IF WKS-MSG-NOCOMAND
                MOVE "COMANDO NO ACTIVO"
                TO EDCL5-OUTPUT-MSGO
           ELSE IF WKS-MSG-UNKERROR
                MOVE "ERROR DESCONOCIDO1"
                TO EDCL5-OUTPUT-MSGO
           ELSE IF WKS-MSG-NOTOPEN
                MOVE "ARCHIVO CERRADO"
                TO EDCL5-OUTPUT-MSGO
           ELSE
                MOVE DFHYELLO TO EDCL5-OUTPUT-MSGO
                MOVE SPACES   TO EDCL5-OUTPUT-MSGO
           END-IF
           MOVE ZEROS TO WKS-OUTPUT-MSG.

      *--> PROCESAR DATOS POR DEFECTO
       998-PROCESS-DEFAULT-DATA.
           MOVE FUNCTION CURRENT-DATE(1:4) TO WKS-DATE-YYYY
           MOVE FUNCTION CURRENT-DATE(5:2) TO WKS-DATE-MM
           MOVE FUNCTION CURRENT-DATE(7:2) TO WKS-DATE-DD
           MOVE WKS-DATE-SIS TO EDCL5-DATE-SISO
           MOVE FUNCTION CURRENT-DATE(9:2) TO WKS-TIME-HH
           MOVE FUNCTION CURRENT-DATE(11:2) TO WKS-TIME-MM
           MOVE FUNCTION CURRENT-DATE(13:2) TO WKS-TIME-SS
           MOVE WKS-TIME-SIS TO EDCL5-TIME-SISO.

      *--> TERMINACION DEL PROGRAMA
       999-END-PROGRAM.
           MOVE SPACE TO EDCL5I
           EXEC CICS SEND
               TEXT
               FROM(WKS-END-MSG)
               ERASE
           END-EXEC

           EXEC CICS RETURN
           END-EXEC

           GOBACK.