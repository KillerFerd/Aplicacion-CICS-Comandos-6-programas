      ******************************************************************
      * FECHA       : 03/06/2022                                       *
      * PROGRAMADOR : JOSUE DONIS                                      *
      * APLICACION  : SEMILLERO                                        *
      * PROGRAMA    : EDID1IL5                                         *
      * TIPO        : LINEA                                            *
      * DESCRIPCION : MENU GENERAR JOB BATCH                           *
      * ARCHIVOS    : -                                                *
      * ACCION (ES) : R=Reporte                                        *
      * PROGRAMA(S) : XTCL                                             *
      * CANAL       : ADMINISTRATIVA                                   *
      * INSTALADO   : 03/06/2022                                       *
      * BPM/RATIONAL:                                                  *
      * NOMBRE      : EDGAR MARTINEZ - INSTRUCTOR                      *
      * DESCRIPCION : USER5005                                         *
      ******************************************************************
       ID DIVISION.
       PROGRAM-ID. EDID1IL5.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY DFHBMSCA.
       COPY DFHAID.
       COPY EDIL5.

       01 WKS-PROGRAM-SPECS.
           02 WKS-PROGRAM-NAME          PIC X(08) VALUE "EDID1IL5".
           02 WKS-PROGRAM-1             PIC X(08) VALUE "EDID1YL5".
           02 WKS-COMMAREA              PIC X(03) VALUE "123".

       01 WKS-FILE-STATUS.
           02 WKS-SPOOL-STATUS         PIC 99 VALUE ZEROS.
               88 WKS-SPOOL-NORMAL     VALUE 1.

       01 WKS-FLAGS.
           02 WKS-OUTPUT-MSG            PIC 9(02) VALUE ZEROS.
                88 WKS-MSG-INVALIDOP    VALUE 1.
                88 WKS-MSG-NOCOMAND     VALUE 2.
                88 WKS-MSG-GENERED      VALUE 3.
                88 WKS-MSG-NOGENERED    VALUE 4.

       01 WKS-WORK-FIELDS.
           02 WKS-PROGRAM-XCTL          PIC X(08).
           02 WKS-END-MSG               PIC X(14)
           VALUE "MUCHAS GRACIAS".

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

       01 WKS-JCL.
           02 WKS-TOKEN                 PIC X(08).
           02 WKS-JCL-LINE              PIC X(80).
           02 WKS-JCL-1.
                03 WKS-JCL-1-STRUCT.
                    04 PIC X(56) VALUE
           "//EDID1JB4 JOB EDUC,SEMILLERO,NOTIFY=EJFD               ".
                    04 PIC X(56) VALUE
           "//JOBLIB   DD DSN=USERLIB.BATCH,DISP=SHR                ".
                    04 PIC X(56) VALUE
           "//         DD DSN=RW.V1R6M0.SCXRRUN,DISP=SHR            ".
                    04 PIC X(56) VALUE
           "//EDID1JB4 EXEC PGM=EDID1R01                            ".
                    04 PIC X(56) VALUE
           "//SYSOUT   DD SYSOUT=*                                  ".
                    04 PIC X(56) VALUE
           "//EDM4CL   DD DSN=EDUC.CLIENTES.V1.M.P.MAESTRO.KSDSD.EI,".
                    04 PIC X(56) VALUE
           "// DISP=SHR                                             ".
                    04 PIC X(56) VALUE
           "//EDM4CL1  DD DSN=EDUC.CLIENTES.V1.M.P.MAESTRO.PATHD.EI,".
                    04 PIC X(56) VALUE
           "// DISP=SHR                                             ".
                    04 PIC X(56) VALUE
           "//SYS007   DD SYSOUT=*                                  ".
                03 WKS-JCL-1-TABLE REDEFINES WKS-JCL-1-STRUCT OCCURS 10.
                    04 WKS-JCL-1-LINE   PIC X(56).
                03 WKS-JCL-1-NO-LINES   PIC 9(02) VALUE 10.

       01 WKS-SUBSCRIPTS.
           02 WKS-INDEX                 PIC 99 VALUE ZEROS.

       LINKAGE SECTION.
       01 DFHCOMMAREA                   PIC X(03).

       PROCEDURE DIVISION.
       000-MAIN-PROCESS.
           EVALUATE TRUE
                WHEN EIBCALEN = 0
                    PERFORM 801-EXEC-CICS-SEND-ERASE
                    PERFORM 802-EXEC-CICS-SEND-DATA
                    PERFORM 803-EXEC-CICS-RETURN
                WHEN EIBAID = DFHPF5
                    PERFORM 200-ACCION-PF5
                WHEN EIBAID = DFHPF10
                    PERFORM 300-ACCION-PF10
                WHEN OTHER
                    PERFORM 400-ACCION-OTHER
           END-EVALUATE.

      *--> ACCION QUE MANDA UN REPORTE DE CLIENES
       200-ACCION-PF5.
           PERFORM 805-EXEC-CICS-SPOOLOPEN
           MOVE 1 TO WKS-INDEX
           PERFORM UNTIL WKS-INDEX > WKS-JCL-1-NO-LINES
                MOVE WKS-JCL-1-LINE(WKS-INDEX) TO WKS-JCL-LINE
                PERFORM 806-EXEC-CICS-SPOOLWRITE
                ADD 1 TO WKS-INDEX
           END-PERFORM
           IF WKS-SPOOL-NORMAL
                SET WKS-MSG-GENERED TO TRUE
           ELSE
                SET WKS-MSG-NOGENERED TO TRUE
           END-IF
           PERFORM 807-EXEC-CICS-SPOOLCLOSE
           PERFORM 802-EXEC-CICS-SEND-DATA
           PERFORM 803-EXEC-CICS-RETURN.

      *--> ACCION PARA SALIR DEL PROGRAMA
       300-ACCION-PF10.
           MOVE WKS-PROGRAM-1 TO WKS-PROGRAM-XCTL
           PERFORM 804-EXEC-CICS-XCTL
           PERFORM 999-END-PROGRAM.

      *-->ACCION QUE DESPLIEGA MENSAJE DE COMANDO NO HABILITADO
       400-ACCION-OTHER.
           SET WKS-MSG-NOCOMAND TO TRUE
           PERFORM 802-EXEC-CICS-SEND-DATA
           PERFORM 803-EXEC-CICS-RETURN.

      *--> COMANDOS CICS

       801-EXEC-CICS-SEND-ERASE.
           EXEC CICS SEND
               MAP('EDIL5')
               MAPSET('EDIL5')
               ERASE
           END-EXEC.

       802-EXEC-CICS-SEND-DATA.
           PERFORM 998-PROCESS-DEFAULT-DATA
           PERFORM 997-PROCESS-OUTPUT-MSG
           EXEC CICS SEND
               MAP('EDIL5')
               MAPSET('EDIL5')
               DATAONLY
           END-EXEC.

       803-EXEC-CICS-RETURN.
           EXEC CICS RETURN
               TRANSID('EDI5')
               COMMAREA(WKS-COMMAREA)
           END-EXEC.

       804-EXEC-CICS-XCTL.
           EXEC CICS
               XCTL
               PROGRAM(WKS-PROGRAM-XCTL)
           END-EXEC.

       805-EXEC-CICS-SPOOLOPEN.
           EXEC CICS SPOOLOPEN
                OUTPUT
                NODE ('LOCAL')
                USERID ('INTRDR')
                TOKEN(WKS-TOKEN)
                NOHANDLE
           END-EXEC.

       806-EXEC-CICS-SPOOLWRITE.
           EXEC CICS SPOOLWRITE
                TOKEN(WKS-TOKEN)
                FROM(WKS-JCL-LINE)
                NOHANDLE
           END-EXEC
           PERFORM 899-EVALUATE-DFHRESP.

       807-EXEC-CICS-SPOOLCLOSE.
           EXEC CICS SPOOLCLOSE
                TOKEN(WKS-TOKEN)
                NOHANDLE
           END-EXEC.

       899-EVALUATE-DFHRESP.
           EVALUATE EIBRESP
                WHEN DFHRESP(NORMAL)  SET WKS-SPOOL-NORMAL  TO TRUE
           END-EVALUATE.

      *--> PROCESAR MENSAJES DE SALIDA
       997-PROCESS-OUTPUT-MSG.
           IF WKS-MSG-INVALIDOP
                MOVE "OPCION INVALIDA"
                TO EDIL5-OUTPUT-MSGO
           ELSE IF WKS-MSG-NOCOMAND
                MOVE "COMANDO INACTIVO"
                TO EDIL5-OUTPUT-MSGO
           ELSE IF WKS-MSG-GENERED
                MOVE "GENERADO CORRECTAMENTE"
                TO EDIL5-OUTPUT-MSGO
           ELSE IF WKS-MSG-NOGENERED
                MOVE "ERROR AL GENERAR"
                TO EDIL5-OUTPUT-MSGO
           ELSE
                MOVE DFHYELLO           TO EDIL5-OUTPUT-MSGC
                MOVE SPACES             TO EDIL5-OUTPUT-MSGO
           END-IF

           MOVE ZEROS TO WKS-OUTPUT-MSG.

      *--> PROCESAR DATOS POR DEFECTO
       998-PROCESS-DEFAULT-DATA.
           MOVE FUNCTION CURRENT-DATE(1:4) TO WKS-DATE-YYYY
           MOVE FUNCTION CURRENT-DATE(5:2) TO WKS-DATE-MM
           MOVE FUNCTION CURRENT-DATE(7:2) TO WKS-DATE-DD
           MOVE WKS-DATE-SIS TO EDIL5-DATE-SISO
           MOVE FUNCTION CURRENT-DATE(9:2) TO WKS-TIME-HH
           MOVE FUNCTION CURRENT-DATE(11:2) TO WKS-TIME-MM
           MOVE FUNCTION CURRENT-DATE(13:2) TO WKS-TIME-SS
           MOVE WKS-TIME-SIS TO EDIL5-TIME-SISO.

      *--> TERMINACION DEL PROGRAMA
       999-END-PROGRAM.
           MOVE SPACE TO EDIL5I
           EXEC CICS SEND
               TEXT
               FROM(WKS-END-MSG)
               ERASE
           END-EXEC

           EXEC CICS RETURN
           END-EXEC

           GOBACK.