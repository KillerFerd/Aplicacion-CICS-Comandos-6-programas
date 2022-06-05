      ******************************************************************
      * FECHA       : 03/06/2022                                       *
      * PROGRAMADOR : JOSUE DONIS                                      *
      * APLICACION  : SEMILLERO                                        *
      * PROGRAMA    : EDID1QLI                                         *
      * TIPO        : LINEA                                            *
      * DESCRIPCION : MENU DE BROWSE PARA MAESTRO DE CLIENTES          *
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
       PROGRAM-ID. EDID1QLI.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY DFHBMSCA.
       COPY DFHAID.
       COPY EDQLI.

       COPY EDMACL.

       01 WKS-WORK-FIELDS.
           02 WKS-PAGE-LIMIT            PIC 9(02) VALUE 13.
           02 WKS-SCROLL                PIC 9(03) VALUE 5.
           02 WKS-START-KEY             PIC 9(08) VALUE ZEROS.
           02 WKS-LAST-KEY              PIC X(08).
           02 WKS-PROGRAM-XCTL          PIC X(08).
           02 WKS-END-MSG               PIC X(14)
           VALUE "MUCHAS GRACIAS".

       01 WKS-FILE-STATUS.
           02 WKS-EDM4CL-STATUS         PIC 99 VALUE ZEROS.
               88 WKS-EDM4CL-NOTOPEN    VALUE 1.
               88 WKS-EDM4CL-NORMAL     VALUE 2.
               88 WKS-EDM4CL-ENDFILE    VALUE 3.

       01 WKS-FLAGS.
           02 WKS-OUTPUT-MSG            PIC 9 VALUE ZEROS.
                88 WKS-MSG-NOTOPEN      VALUE 1.
                88 WKS-MSG-ENDFILE      VALUE 2.
                88 WKS-MSG-NOCOMAND     VALUE 3.
                88 WKS-MSG-SELECTION    VALUE 4.
           02 WKS-FLAG-ONE-ITERATION    PIC 9 VALUE ZEROS.
           02 WKS-FLAG-MATCH            PIC 9 VALUE ZEROS.

       01 WKS-SUBCRIPS.
           02 WKS-INDEX                 PIC 99 VALUE ZEROS.

       01 WKS-PROGRAM-SPECS.
           02 WKS-PROGRAM-NAME          PIC X(08) VALUE "EDID1QLI".
           02 WKS-PROGRAM-1             PIC X(08) VALUE "EDID1YL5".
           02 WKS-PROGRAM-3             PIC X(08) VALUE "EDID1CL5".
           02 WKS-COMMAREA.
               03 WKS-COM-LAST-POS      PIC 9(08).
               03 WKS-COM-FIRST-POS     PIC 9(08).
               03 WKS-COM-CO-CLIENTE    PIC X(08) OCCURS 13.
               03 WKS-COM-OPTION        PIC X(01) OCCURS 13.
               03 WKS-COM-MODE          PIC 9(01).
               03 WKS-COM-RETURN        PIC 9(01).

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
       01 DFHCOMMAREA                   PIC X(135).

       PROCEDURE DIVISION.
       000-MAIN-PROCESS.

      *-->MOVER COMMAREA
           IF EIBCALEN NOT = 0
                MOVE DFHCOMMAREA TO WKS-COMMAREA
           END-IF

      *-->VALIDAR RETORNO DEL MENU DE CONSULTA
           IF WKS-COM-RETURN = 1
                PERFORM 010-ACCION-RETURN
           END-IF

      *-->EVALUATE PARA LA ACCION DE CADA TECLA
           EVALUATE TRUE
                WHEN EIBCALEN = 0
                    PERFORM 100-ACCION-DEFAULT
                WHEN EIBAID = DFHENTER
                    PERFORM 200-ACCION-ENTER
                WHEN EIBAID = DFHPF5
                    PERFORM 300-ACCION-PF5
                WHEN EIBAID = DFHPF6
                    PERFORM 400-ACCION-PF6
                WHEN EIBAID = DFHPF7
                    PERFORM 500-ACCION-PF7
                WHEN EIBAID = DFHPF10
                    PERFORM 600-ACCION-PF10
                WHEN OTHER
                    PERFORM 700-ACCION-OTHER
           END-EVALUATE.

      *-->ACCION QUE RESTAURA LA POSICION Y LOS SELECCIONADOS
       010-ACCION-RETURN.
           MOVE ZEROS TO WKS-COM-RETURN
           PERFORM UNTIL WKS-INDEX > WKS-PAGE-LIMIT
                MOVE WKS-COM-OPTION(WKS-INDEX)
                TO EDQLI-OPTIONO(WKS-INDEX)
                ADD 1 TO WKS-INDEX
           END-PERFORM
           PERFORM 801-EXEC-CICS-SEND-ERASE
           MOVE WKS-COM-LAST-POS TO WKS-START-KEY
           PERFORM 804-EXEC-CICS-STARTBR
           PERFORM 807-EXEC-CICS-READPREV
           PERFORM 411-PROCESS-PREV-TABLE-DATA
           PERFORM 802-EXEC-CICS-SEND-DATA
           PERFORM 803-EXEC-CICS-RETURN.

      *-->ACCION DEFAULT
       100-ACCION-DEFAULT.
           PERFORM 801-EXEC-CICS-SEND-ERASE
           PERFORM 110-PROCESS-START-DATA
           PERFORM 802-EXEC-CICS-SEND-DATA
           PERFORM 803-EXEC-CICS-RETURN.

      *-->PROCESO QUE CARGA LA TABLA INICIAL
       110-PROCESS-START-DATA.
           MOVE WKS-COM-FIRST-POS TO WKS-START-KEY
           PERFORM 804-EXEC-CICS-STARTBR

           IF WKS-EDM4CL-NOTOPEN
                SET WKS-MSG-NOTOPEN TO TRUE
           ELSE IF WKS-EDM4CL-NORMAL
                PERFORM 806-EXEC-CICS-READNEXT
                PERFORM 311-PROCESS-NEXT-TABLE-DATA
                PERFORM 808-EXEC-CICS-ENDBR
           END-IF.

      *-->ACCION PARA CONSULTAR UN REGISTRO INDIVIDUAL
       200-ACCION-ENTER.
           PERFORM UNTIL WKS-INDEX > WKS-PAGE-LIMIT
               MOVE SPACES TO WKS-COM-OPTION(WKS-INDEX)
               ADD 1 TO WKS-INDEX
           END-PERFORM
           PERFORM 805-EXEC-CICS-RECEIVE
           INITIALIZE WKS-FLAG-MATCH
           MOVE 1 TO WKS-INDEX

           PERFORM UNTIL WKS-INDEX > WKS-PAGE-LIMIT
               IF EDQLI-OPTIONO(WKS-INDEX) = "S"
                  MOVE EDQLI-OPTIONO(WKS-INDEX)
                    TO WKS-COM-OPTION(WKS-INDEX)
                  MOVE 1 TO WKS-FLAG-MATCH
               END-IF
           ADD 1 TO WKS-INDEX
           END-PERFORM

           IF WKS-FLAG-MATCH = 1
               PERFORM 809-CICS-XCTL-PROGRAM-3
           ELSE
               SET WKS-MSG-SELECTION TO TRUE
               PERFORM 802-EXEC-CICS-SEND-DATA
               PERFORM 803-EXEC-CICS-RETURN
           END-IF.

      *-->ACCION PARA REALIZAR SCROLL HACIA ABAJO
       300-ACCION-PF5.
           PERFORM 310-PROCESS-SCROLL-DOWN
           PERFORM 802-EXEC-CICS-SEND-DATA
           PERFORM 803-EXEC-CICS-RETURN.

       310-PROCESS-SCROLL-DOWN.
           MOVE WKS-COM-FIRST-POS TO WKS-START-KEY
           PERFORM 804-EXEC-CICS-STARTBR
           MOVE 1 TO WKS-INDEX
           PERFORM 806-EXEC-CICS-READNEXT
           PERFORM UNTIL WKS-INDEX > WKS-SCROLL OR WKS-EDM4CL-ENDFILE
                 PERFORM 806-EXEC-CICS-READNEXT
                 ADD 1 TO WKS-INDEX
           END-PERFORM

           IF WKS-EDM4CL-ENDFILE
               SET WKS-MSG-ENDFILE TO TRUE
           ELSE
               PERFORM 311-PROCESS-NEXT-TABLE-DATA
               PERFORM 808-EXEC-CICS-ENDBR
           END-IF.

       311-PROCESS-NEXT-TABLE-DATA.
           MOVE 1 TO WKS-INDEX
           PERFORM UNTIL WKS-INDEX > WKS-PAGE-LIMIT
           OR WKS-EDM4CL-ENDFILE
                MOVE EDMC-LLAVE TO WKS-COM-CO-CLIENTE(WKS-INDEX)
                PERFORM 312-LOAD-TABLE-DATA
                PERFORM 806-EXEC-CICS-READNEXT
                IF WKS-EDM4CL-ENDFILE
                    MOVE WKS-COM-CO-CLIENTE(WKS-INDEX) TO WKS-LAST-KEY
                    SET WKS-MSG-ENDFILE TO TRUE
                END-IF
                ADD 1 TO WKS-INDEX
           END-PERFORM

           IF WKS-EDM4CL-ENDFILE AND WKS-FLAG-ONE-ITERATION = 0
               PERFORM 808-EXEC-CICS-ENDBR
               MOVE WKS-LAST-KEY TO WKS-START-KEY
               PERFORM 804-EXEC-CICS-STARTBR
               PERFORM 807-EXEC-CICS-READPREV
               PERFORM 411-PROCESS-PREV-TABLE-DATA
           ELSE IF WKS-EDM4CL-ENDFILE AND WKS-FLAG-ONE-ITERATION = 1
               MOVE WKS-COM-CO-CLIENTE(1) TO WKS-COM-FIRST-POS
               MOVE WKS-LAST-KEY          TO WKS-COM-LAST-POS
               PERFORM UNTIL WKS-INDEX > WKS-PAGE-LIMIT
                    MOVE SPACES TO WKS-COM-CO-CLIENTE(WKS-INDEX)
                    PERFORM 313-LOAD-TABLE-SPACES
                    ADD 1 TO WKS-INDEX
               END-PERFORM
           ELSE
               MOVE WKS-COM-CO-CLIENTE(1)  TO WKS-COM-FIRST-POS
               MOVE WKS-COM-CO-CLIENTE(13) TO WKS-COM-LAST-POS
           END-IF.

       312-LOAD-TABLE-DATA.
           MOVE EDMC-LLAVE           TO EDQLI-CO-CLIENTEO(WKS-INDEX)
           MOVE EDMC-NOMBRE-CLIENTE  TO EDQLI-NOM-CLIENTEO(WKS-INDEX)
           MOVE EDMC-FECHA-NAC-O-CONSTITUC TO WKS-DATE-FORMAT
           MOVE WKS-DATE-FORMAT      TO EDQLI-FECHA-NACO(WKS-INDEX)
           MOVE EDMC-MARCA-ELIMINADO TO EDQLI-DELETEO(WKS-INDEX).

       313-LOAD-TABLE-SPACES.
           MOVE SPACES TO EDQLI-CO-CLIENTEO(WKS-INDEX)
           MOVE SPACES TO EDQLI-NOM-CLIENTEO(WKS-INDEX)
           MOVE SPACES TO EDQLI-FECHA-NACO(WKS-INDEX)
           MOVE SPACES TO EDQLI-DELETEO(WKS-INDEX).

      *-->ACCION PARA REALIZAR SCROLL HACIA ARRIBA
       400-ACCION-PF6.
           PERFORM 410-PROCESS-PREV-RECORD
           PERFORM 802-EXEC-CICS-SEND-DATA
           PERFORM 803-EXEC-CICS-RETURN.

       410-PROCESS-PREV-RECORD.
           MOVE WKS-COM-LAST-POS TO WKS-START-KEY
           PERFORM 804-EXEC-CICS-STARTBR

           MOVE 1 TO WKS-INDEX
           PERFORM 807-EXEC-CICS-READPREV
           PERFORM UNTIL WKS-INDEX > WKS-SCROLL OR WKS-EDM4CL-ENDFILE
                 PERFORM 807-EXEC-CICS-READPREV
                 ADD 1 TO WKS-INDEX
           END-PERFORM
           IF WKS-EDM4CL-ENDFILE
               SET WKS-MSG-ENDFILE TO TRUE
           ELSE
               PERFORM 411-PROCESS-PREV-TABLE-DATA
               PERFORM 808-EXEC-CICS-ENDBR
           END-IF.

       411-PROCESS-PREV-TABLE-DATA.
           MOVE WKS-PAGE-LIMIT TO WKS-INDEX
           PERFORM UNTIL WKS-INDEX < 1
           OR WKS-EDM4CL-ENDFILE
                MOVE EDMC-LLAVE TO WKS-COM-CO-CLIENTE(WKS-INDEX)
                PERFORM 312-LOAD-TABLE-DATA
                PERFORM 807-EXEC-CICS-READPREV

                IF WKS-EDM4CL-ENDFILE
                    MOVE WKS-COM-CO-CLIENTE(WKS-INDEX) TO WKS-LAST-KEY
                    SET WKS-MSG-ENDFILE TO TRUE
                END-IF

                SUBTRACT 1 FROM WKS-INDEX
           END-PERFORM

           IF WKS-EDM4CL-ENDFILE
               MOVE 1 TO WKS-FLAG-ONE-ITERATION
               PERFORM 808-EXEC-CICS-ENDBR
               MOVE WKS-LAST-KEY TO WKS-START-KEY
               PERFORM 804-EXEC-CICS-STARTBR
               PERFORM 806-EXEC-CICS-READNEXT
               PERFORM 311-PROCESS-NEXT-TABLE-DATA
           ELSE
               MOVE WKS-COM-CO-CLIENTE(1) TO WKS-COM-FIRST-POS
               MOVE WKS-COM-CO-CLIENTE(13) TO WKS-COM-LAST-POS
           END-IF.

      *-->ACCION PARA CONSULTAR VARIOS REGISTROS A LA VEZ
       500-ACCION-PF7.
           MOVE 1 TO WKS-COM-MODE
           PERFORM 200-ACCION-ENTER.

      *-->ACCION PARA SALIR AL MENU PRINCIPAL
       600-ACCION-PF10.
           MOVE WKS-PROGRAM-1 TO WKS-PROGRAM-XCTL
           PERFORM 810-EXEC-CICS-XCTL
           PERFORM 999-END-PROGRAM.

      *-->ACCION QUE DESPLIEGA MENSAJE DE COMANDO NO HABILITADO
       700-ACCION-OTHER.
           SET WKS-MSG-NOCOMAND TO TRUE
           PERFORM 802-EXEC-CICS-SEND-DATA
           PERFORM 803-EXEC-CICS-RETURN.

      *--> COMANDOS CICS
       801-EXEC-CICS-SEND-ERASE.
           EXEC CICS SEND
                MAP('EDQLI')
                MAPSET('EDQLI')
                ERASE
           END-EXEC.

       802-EXEC-CICS-SEND-DATA.
           PERFORM 998-PROCESS-DEFAULT-DATA
           PERFORM 997-PROCESS-OUTPUT-MSG
           EXEC CICS
               SEND MAP('EDQLI')
               MAPSET('EDQLI')
               DATAONLY
           END-EXEC.

       803-EXEC-CICS-RETURN.
           EXEC CICS RETURN
               TRANSID('EDQI')
               COMMAREA(WKS-COMMAREA)
           END-EXEC.

       804-EXEC-CICS-STARTBR.
           EXEC CICS STARTBR
                FILE('EDM4CL')
                RIDFLD(WKS-START-KEY)
                GTEQ
                NOHANDLE
           END-EXEC
           PERFORM 899-EVALUATE-DFHRESP.

       805-EXEC-CICS-RECEIVE.
           EXEC CICS RECEIVE
               MAP ('EDQLI')
               MAPSET ('EDQLI')
           END-EXEC.

       806-EXEC-CICS-READNEXT.
           EXEC CICS READNEXT
                FILE('EDM4CL')
                RIDFLD(WKS-START-KEY)
                INTO(REG-EDMACL)
                NOHANDLE
           END-EXEC
           PERFORM 899-EVALUATE-DFHRESP.

       807-EXEC-CICS-READPREV.
           EXEC CICS READPREV
               FILE('EDM4CL')
               RIDFLD(WKS-START-KEY)
               INTO(REG-EDMACL)
               NOHANDLE
           END-EXEC
           PERFORM 899-EVALUATE-DFHRESP.

       808-EXEC-CICS-ENDBR.
           EXEC CICS ENDBR
               FILE('EDM4CL')
           END-EXEC.

       809-CICS-XCTL-PROGRAM-3.
           EXEC CICS XCTL
               PROGRAM(WKS-PROGRAM-3)
               COMMAREA(WKS-COMMAREA)
           END-EXEC.

       810-EXEC-CICS-XCTL.
           EXEC CICS XCTL
               PROGRAM(WKS-PROGRAM-XCTL)
           END-EXEC.

       899-EVALUATE-DFHRESP.
           EVALUATE EIBRESP
                WHEN DFHRESP(NOTOPEN) SET WKS-EDM4CL-NOTOPEN TO TRUE
                WHEN DFHRESP(NORMAL)  SET WKS-EDM4CL-NORMAL  TO TRUE
                WHEN DFHRESP(ENDFILE) SET WKS-EDM4CL-ENDFILE TO TRUE
           END-EVALUATE.

      *--> PROCESAR MENSAJES DE SALIDA
       997-PROCESS-OUTPUT-MSG.
           IF WKS-MSG-NOTOPEN
                MOVE "ARCHIVO CERRADO"
                TO EDQLI-OUTPUT-MSGO
           ELSE IF WKS-MSG-ENDFILE
                MOVE "FIN DE ARCHIVO"
                TO EDQLI-OUTPUT-MSGO
           ELSE IF WKS-MSG-NOCOMAND
                MOVE "COMANDO INACTIVO"
                TO EDQLI-OUTPUT-MSGO
           ELSE IF WKS-MSG-SELECTION
                MOVE "UTILICE 'S' PARA SELECCIONAR"
                TO EDQLI-OUTPUT-MSGO
           ELSE
                MOVE DFHYELLO TO EDQLI-OUTPUT-MSGC
                MOVE SPACES   TO EDQLI-OUTPUT-MSGO

           END-IF
           MOVE ZEROS TO WKS-OUTPUT-MSG.

      *--> PROCESAR DATOS POR DEFECTO
       998-PROCESS-DEFAULT-DATA.
           MOVE FUNCTION CURRENT-DATE(1:4) TO WKS-DATE-YYYY
           MOVE FUNCTION CURRENT-DATE(5:2) TO WKS-DATE-MM
           MOVE FUNCTION CURRENT-DATE(7:2) TO WKS-DATE-DD
           MOVE WKS-DATE-SIS TO EDQLI-DATE-SISO
           MOVE FUNCTION CURRENT-DATE(9:2) TO WKS-TIME-HH
           MOVE FUNCTION CURRENT-DATE(11:2) TO WKS-TIME-MM
           MOVE FUNCTION CURRENT-DATE(13:2) TO WKS-TIME-SS
           MOVE WKS-TIME-SIS TO EDQLI-TIME-SISO.

      *--> TERMINACION DEL PROGRAMA
       999-END-PROGRAM.
           MOVE SPACE TO EDQLII
           EXEC CICS SEND
               TEXT
               FROM(WKS-END-MSG)
               ERASE
           END-EXEC

           EXEC CICS RETURN
           END-EXEC

           GOBACK.