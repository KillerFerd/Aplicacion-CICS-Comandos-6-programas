      ******************************************************************
      * FECHA       : 03/06/2022                                       *
      * PROGRAMADOR : JOSUE DONIS                                      *
      * APLICACION  : SEMILLERO                                        *
      * PROGRAMA    : EDID1YL5                                         *
      * TIPO        : LINEA                                            *
      * DESCRIPCION : MENU PRINCIPAL PARA MAESTRO DE CLIENTES          *
      * ARCHIVOS    : -                                                *
      * ACCION (ES) : C=CONSULTAR                                      *
      * PROGRAMA(S) : XTCL                                             *
      * CANAL       : ADMINISTRATIVA                                   *
      * INSTALADO   : 03/06/2022                                       *
      * BPM/RATIONAL:                                                  *
      * NOMBRE      : EDGAR MARTINEZ - INSTRUCOR                       *
      * DESCRIPCION : USER5004                                         *
      ******************************************************************
       ID DIVISION.
       PROGRAM-ID. EDID1YL5.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY DFHBMSCA.
       COPY DFHAID.
       COPY EDYL5.

       01 WKS-PROGRAM-SPECS.
           02 WKS-PROGRAM-NAME          PIC X(08) VALUE "EDID1YL5".
           02 WKS-PROGRAM-2             PIC X(08) VALUE "EDID1QLI".
           02 WKS-PROGRAM-4             PIC X(08) VALUE "EDID1ALI".
           02 WKS-PROGRAM-5             PIC X(08) VALUE "EDID1MLI".
           02 WKS-PROGRAM-6             PIC X(08) VALUE "EDID1ELI".
           02 WKS-COMMAREA              PIC X(03) VALUE "123".

       01 WKS-FLAGS.
           02 WKS-OUTPUT-MSG            PIC 9(02) VALUE ZEROS.
                88 WKS-MSG-INVALIDOP    VALUE 1.

       01 WKS-WORK-FIELDS.
           02 WKS-PROGRAM-XCTL          PIC X(08).
           02 WKS-END-MSG               PIC X(14)
           VALUE 'MUCHAS GRACIAS'.

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

       LINKAGE SECTION.
       01 DFHCOMMAREA                   PIC X(03).

       PROCEDURE DIVISION.
       000-MAIN-PROCESS.
           EVALUATE TRUE
               WHEN EIBCALEN = 0
                   PERFORM 801-EXEC-CICS-SEND-ERASE
                   PERFORM 802-EXEC-CICS-SEND-DATA
                   PERFORM 803-EXEC-CICS-RETURN
               WHEN EIBAID = DFHENTER
                   PERFORM 100-ACCION-ENTER
               WHEN EIBAID = DFHPF10
                   PERFORM 200-ACCION-PF10
           END-EVALUATE.

      *--> ACCION PARA INGRESAR OPCIONES
       100-ACCION-ENTER.
           PERFORM 804-EXEC-CICS-RECEIVE
           EVALUATE EDYL5-OPTIONI
               WHEN 1     PERFORM 110-OPTION-1
               WHEN 2     PERFORM 120-OPTION-2
               WHEN 3     PERFORM 130-OPTION-3
               WHEN 4     PERFORM 140-OPTION-4
               WHEN 9     PERFORM 150-OPTION-9
               WHEN OTHER PERFORM 160-OPTION-OTHER
           END-EVALUATE
           PERFORM 802-EXEC-CICS-SEND-DATA
           PERFORM 803-EXEC-CICS-RETURN.

      *--> OPCION 1 >> MENU DE BROWSE
       110-OPTION-1.
           MOVE WKS-PROGRAM-2 TO WKS-PROGRAM-XCTL
           PERFORM 805-EXEC-CICS-XCTL.

      *--> OPCION 2 >> MENU DE ADICION
       120-OPTION-2.
           MOVE WKS-PROGRAM-4 TO WKS-PROGRAM-XCTL
           PERFORM 805-EXEC-CICS-XCTL.

      *--> OPCION 3 >> MENU DE MODIFICACION
       130-OPTION-3.
           MOVE WKS-PROGRAM-5 TO WKS-PROGRAM-XCTL
           PERFORM 805-EXEC-CICS-XCTL.

      *--> OPCION 4 >> MENU DE ELIMINACION
       140-OPTION-4.
           MOVE WKS-PROGRAM-6 TO WKS-PROGRAM-XCTL
           PERFORM 805-EXEC-CICS-XCTL.

      *--> OPCION 9 >> SALIR
       150-OPTION-9.
           PERFORM 999-END-PROGRAM.

      *--> OPCION OTHER
       160-OPTION-OTHER.
           SET WKS-MSG-INVALIDOP TO TRUE.

      *--> ACCION PARA SALIR DEL PROGRAMA
       200-ACCION-PF10.
           PERFORM 999-END-PROGRAM.

      *--> COMANDOS CICS

       801-EXEC-CICS-SEND-ERASE.
           EXEC CICS SEND
               MAP('EDYL5')
               MAPSET('EDYL5')
               ERASE
           END-EXEC.

       802-EXEC-CICS-SEND-DATA.
           PERFORM 998-PROCESS-DEFAULT-DATA
           PERFORM 997-PROCESS-OUTPUT-MSG
           EXEC CICS SEND
               MAP('EDYL5')
               MAPSET('EDYL5')
               DATAONLY
           END-EXEC.

       803-EXEC-CICS-RETURN.
           EXEC CICS RETURN
               TRANSID('EDY5')
               COMMAREA(WKS-COMMAREA)
           END-EXEC.

       804-EXEC-CICS-RECEIVE.
           EXEC CICS RECEIVE
               MAP ('EDYL5')
               MAPSET ('EDYL5')
           END-EXEC.

       805-EXEC-CICS-XCTL.
           EXEC CICS
               XCTL
               PROGRAM(WKS-PROGRAM-XCTL)
           END-EXEC.

      *--> PROCESAR MENSAJES DE SALIDA
       997-PROCESS-OUTPUT-MSG.
           IF WKS-MSG-INVALIDOP
                MOVE "OPCION INVALIDA"
                TO EDYL5-OUTPUT-MSGO
           ELSE
                MOVE DFHYELLO           TO EDYL5-OUTPUT-MSGC
                MOVE SPACES             TO EDYL5-OUTPUT-MSGO
           END-IF

           MOVE ZEROS TO WKS-OUTPUT-MSG.

      *--> PROCESAR DATOS POR DEFECTO
       998-PROCESS-DEFAULT-DATA.
           MOVE FUNCTION CURRENT-DATE(1:4) TO WKS-DATE-YYYY
           MOVE FUNCTION CURRENT-DATE(5:2) TO WKS-DATE-MM
           MOVE FUNCTION CURRENT-DATE(7:2) TO WKS-DATE-DD
           MOVE WKS-DATE-SIS TO EDYL5-DATE-SISO
           MOVE FUNCTION CURRENT-DATE(9:2) TO WKS-TIME-HH
           MOVE FUNCTION CURRENT-DATE(11:2) TO WKS-TIME-MM
           MOVE FUNCTION CURRENT-DATE(13:2) TO WKS-TIME-SS
           MOVE WKS-TIME-SIS TO EDYL5-TIME-SISO.

      *--> TERMINACION DEL PROGRAMA
       999-END-PROGRAM.
           MOVE SPACE TO EDYL5I
           EXEC CICS SEND
               TEXT
               FROM(WKS-END-MSG)
               ERASE
           END-EXEC

           EXEC CICS RETURN
           END-EXEC

           GOBACK.