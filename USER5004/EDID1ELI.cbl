      ******************************************************************
      * FECHA       : HOLA QUE HACE                                    *
      * PROGRAMADOR : JOSUE DONIS                                      *
      * APLICACION  : SEMILLERO                                        *
      * PROGRAMA    : EDID1ELI                                         *
      * TIPO        : LINEA                                            *
      * DESCRIPCION : MENU DE ADICION                                  *
      * ARCHIVOS    : MOMAES, CFCNAT, MOTGEN, CFTGEN                   *
      * ACCION (ES) : C=CONSULTAR                                      *
      * PROGRAMA(S) :                                                  *
      * CANAL       :                                                  *
      * INSTALADO   :                                                  *
      * BPM/RATIONAL:                                                  *
      * NOMBRE      :                                                  *
      * DESCRIPCION :                                                  *
      ******************************************************************
       ID DIVISION.
       PROGRAM-ID. EDID1ELI.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CLASS NUMERO IS '0' '1' '2' '3' '4' '5' '6' '7' '8' '9' ' '.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY DFHBMSCA.
       COPY DFHAID.
       COPY EDELI.

       COPY EDMACL.

       01 WKS-WORK-FIELDS.
           02 WKS-END-MSG               PIC X(14)
           VALUE "MUCHAS GRACIAS".

       01 WKS-FILE-STATUS               PIC 99 VALUE ZEROS.
           88 WKS-EDM4CL-NOTOPEN        VALUE 1.
           88 WKS-EDM4CL-NOTFND         VALUE 2.
           88 WKS-EDM4CL-NORMAL         VALUE 3.
           88 WKS-EDM4CL-ENDFILE        VALUE 4.

       01 WKS-FLAGS.
           02 WKS-OUTPUT-MSG            PIC 99 VALUE ZEROS.
                88 WKS-MSG-NOCOMAND     VALUE 1.
                88 WKS-MSG-UNKERROR     VALUE 2.
                88 WKS-MSG-NOTOPEN      VALUE 3.
                88 WKS-MSG-NONUMERIC    VALUE 4.
                88 WKS-MSG-NOTFND       VALUE 5.
                88 WKS-MSG-FOUND        VALUE 6.
                88 WKS-MSG-CELIMINATED  VALUE 7.
                88 WKS-MSG-CANCELED     VALUE 8.
                88 WKS-MSG-CRESTORE     VALUE 9.
                88 WKS-MSG-ECOMPLETED   VALUE 10.
                88 WKS-MSG-RCOMPLETED   VALUE 11.
           02 WKS-FLAG-INVALID          PIC 9 VALUE ZEROS.

       01 WKS-SUBSCRIPTS.
           02 WKS-INDEX                 PIC 99 VALUE ZEROS.
           02 WKS-AUX                   PIC 99 VALUE ZEROS.

       01 WKS-PROGRAM-SPECS.
           02 WKS-PROGRAM-NAME          PIC X(08) VALUE "EDID1ELI".
           02 WKS-PROGRAM-1             PIC X(08) VALUE "EDID1YL5".

           02 WKS-COMMAREA.
               03 WKS-COM-COMFIRM-MODE  PIC 9.
               03 WKS-COM-CO-CLIENTE    PIC 9(08).
               03 WKS-COM-PROCESS-MODE  PIC 9.

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
           02 WKS-AUX-FECHA             PIC 9(08).
           02 WKS-TEMP-FIELDS.
                03 WKS-CO-CLIENTE       PIC 9(08).
                03 WKS-NOM-CLIENTE      PIC X(40).
                03 WKS-FECHA-NAC        PIC 9(08).
                03 WKS-TEL              PIC 9(08).
                03 WKS-FECHA-ADIC       PIC 9(08).
                02 WKS-MARCA            PIC X(01).

       LINKAGE SECTION.
       01 DFHCOMMAREA                   PIC X(10).

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
                WHEN EIBAID = DFHENTER
                    PERFORM 200-ACCION-ENTER
                WHEN EIBAID = DFHPF2
                    PERFORM 300-ACCION-PF2
                WHEN EIBAID = DFHPF3
                    PERFORM 400-ACCION-PF3
                WHEN EIBAID = DFHPF10
                    PERFORM 500-ACCION-PF10
                WHEN OTHER
                    PERFORM 600-ACCION-OTHER
           END-EVALUATE.

      *-->ACCION DEFAULT
       100-ACCION-DEFAULT.
           PERFORM 801-EXEC-CICS-SEND-ERASE
           PERFORM 802-EXEC-CICS-SEND-DATA
           PERFORM 804-EXEC-CICS-RETURN.

      *-->ACCION PARA VALIDAR EL CODIGO DE CLIENTE
       200-ACCION-ENTER.
           IF WKS-COM-COMFIRM-MODE = 1
                SET WKS-MSG-NOCOMAND TO TRUE
                MOVE -1 TO EDELI-CONFIRL
           ELSE
                INITIALIZE WKS-FLAG-INVALID
                PERFORM 805-EXEC-CICS-RECEIVE
                MOVE DFHTURQ TO EDELI-CO-CLIENTEC
                MOVE -1 TO EDELI-CO-CLIENTEL
                PERFORM 210-VALID-FIELD-1
                IF WKS-FLAG-INVALID = ZEROS
                    PERFORM 220-PROCESS-DATA-TEMP
                    PERFORM 230-SHOW-STATUS
                    PERFORM 240-PROCESS-DATA-OUTPUT
                    SET WKS-MSG-FOUND TO TRUE

                    MOVE WKS-CO-CLIENTE TO WKS-COM-CO-CLIENTE
                END-IF
           END-IF
           PERFORM 803-EXEC-CICS-SEND-DATA-CURSOR
           PERFORM 804-EXEC-CICS-RETURN.

       210-VALID-FIELD-1.
           IF EDELI-CO-CLIENTEI = SPACES
                    SET WKS-MSG-NONUMERIC TO TRUE
                    MOVE 1 TO WKS-FLAG-INVALID
           ELSE IF EDELI-CO-CLIENTEI NOT NUMERO
               SET WKS-MSG-NONUMERIC TO TRUE
               MOVE 1 TO WKS-FLAG-INVALID
           ELSE
               INITIALIZE WKS-CO-CLIENTE
               INITIALIZE WKS-INDEX
               MOVE 9 TO WKS-AUX
               PERFORM VARYING WKS-INDEX FROM 8 BY -1
               UNTIL WKS-INDEX = ZEROS
                   IF EDELI-CO-CLIENTEI(WKS-INDEX:1) NOT EQUAL ' '
                       SUBTRACT 1 FROM WKS-AUX
                       MOVE EDELI-CO-CLIENTEI(WKS-INDEX:1)
                       TO WKS-CO-CLIENTE(WKS-AUX:1)
                   END-IF
               END-PERFORM
               MOVE WKS-CO-CLIENTE TO EDELI-CO-CLIENTEO
           END-IF
           IF WKS-FLAG-INVALID = ZEROS
               MOVE WKS-CO-CLIENTE TO EDMC-LLAVE
               PERFORM 806-EXEC-CICS-READ
               IF WKS-EDM4CL-NOTOPEN
                   SET WKS-MSG-NOTOPEN TO TRUE
                   MOVE 1 TO WKS-FLAG-INVALID
               END-IF
               IF WKS-EDM4CL-NOTFND
                   SET WKS-MSG-NOTFND TO TRUE
                   MOVE 1 TO WKS-FLAG-INVALID
               END-IF
           END-IF

           IF WKS-FLAG-INVALID = 1
               MOVE DFHRED TO EDELI-CO-CLIENTEC
               MOVE DFHNEUTR TO EDELI-ESTADOC
               MOVE SPACES TO EDELI-ESTADOO
           END-IF.

       220-PROCESS-DATA-TEMP.
           MOVE EDMC-LLAVE                  TO WKS-CO-CLIENTE
           MOVE EDMC-NOMBRE-CLIENTE         TO WKS-NOM-CLIENTE
           MOVE EDMC-FECHA-NAC-O-CONSTITUC  TO WKS-AUX-FECHA
           MOVE WKS-AUX-FECHA               TO WKS-FECHA-NAC
           MOVE EDMC-NUMERO-TELEFONO        TO WKS-TEL
           MOVE EDMC-FECHA-CREACION         TO WKS-FECHA-ADIC
           MOVE EDMC-MARCA-ELIMINADO        TO WKS-MARCA.

       230-SHOW-STATUS.
           IF WKS-MARCA = "D"
                MOVE "ELIMINADO" TO EDELI-ESTADOO
                MOVE DFHRED TO EDELI-ESTADOC
           ELSE
                MOVE "INTEGRO" TO EDELI-ESTADOO
                MOVE DFHNEUTR TO EDELI-ESTADOC
           END-IF.

       240-PROCESS-DATA-OUTPUT.
           MOVE WKS-NOM-CLIENTE             TO EDELI-NOM-CLIENTEO
           MOVE EDMC-FECHA-NAC-O-CONSTITUC  TO EDELI-FECHA-NACO
           MOVE WKS-TEL                     TO EDELI-TELO
           MOVE WKS-FECHA-ADIC              TO EDELI-FECHA-ADICO.

      *-->ACCION PARA ELIMINAR UN REGISTRO
       300-ACCION-PF2.
           IF WKS-COM-CO-CLIENTE < SPACES
           OR WKS-COM-CO-CLIENTE = ZEROS
                PERFORM 200-ACCION-ENTER
           ELSE
           IF WKS-COM-PROCESS-MODE = 1
           OR WKS-COM-PROCESS-MODE = ZEROS
                MOVE 1 TO WKS-COM-PROCESS-MODE
                PERFORM 310-PROCESS-CHANGE-MARK
           ELSE
                SET WKS-MSG-NOCOMAND TO TRUE
                MOVE -1 TO EDELI-CONFIRL
           END-IF
           PERFORM 803-EXEC-CICS-SEND-DATA-CURSOR
           PERFORM 804-EXEC-CICS-RETURN
           END-IF.

       310-PROCESS-CHANGE-MARK.
           IF WKS-COM-CO-CLIENTE < SPACES
           OR WKS-COM-CO-CLIENTE = ZEROS
                PERFORM 200-ACCION-ENTER
           ELSE
                PERFORM 805-EXEC-CICS-RECEIVE
                MOVE 1 TO WKS-COM-COMFIRM-MODE
                IF EDELI-CONFIRI = "S"
                    IF WKS-COM-PROCESS-MODE = 1
                        MOVE WKS-COM-CO-CLIENTE TO EDMC-LLAVE
                        PERFORM 807-EXEC-CICS-READ-UPDATE
                        MOVE "D" TO EDMC-MARCA-ELIMINADO
                        PERFORM 808-EXEC-CICS-REWRITE
                        IF WKS-EDM4CL-NORMAL
                            SET WKS-MSG-ECOMPLETED TO TRUE
                        ELSE
                            SET WKS-MSG-UNKERROR TO TRUE
                        END-IF
                    END-IF
                    IF WKS-COM-PROCESS-MODE = 2
                        MOVE WKS-COM-CO-CLIENTE TO EDMC-LLAVE
                        PERFORM 807-EXEC-CICS-READ-UPDATE
                        MOVE SPACES TO EDMC-MARCA-ELIMINADO
                        PERFORM 808-EXEC-CICS-REWRITE
                        IF WKS-EDM4CL-NORMAL
                            SET WKS-MSG-RCOMPLETED TO TRUE
                        ELSE
                            SET WKS-MSG-UNKERROR TO TRUE
                        END-IF
                    END-IF
                    PERFORM 311-PROCESS-RESET-FIELDS
                ELSE IF EDELI-CONFIRI = "N"
                    SET WKS-MSG-CANCELED TO TRUE
                    PERFORM 311-PROCESS-RESET-FIELDS
                ELSE
                    IF WKS-COM-PROCESS-MODE = 1
                    SET WKS-MSG-CELIMINATED TO TRUE
                    END-IF

                    IF WKS-COM-PROCESS-MODE = 2
                    SET WKS-MSG-CRESTORE TO TRUE
                    END-IF

                    MOVE DFHBMUNP TO EDELI-CONFIRA
                    MOVE DFHTURQ TO EDELI-CONFIRC
                    MOVE -1 TO EDELI-CONFIRL
                    MOVE DFHBMPRF TO EDELI-CO-CLIENTEA
                    MOVE DFHNEUTR TO EDELI-CO-CLIENTEC
                    MOVE WKS-COM-CO-CLIENTE TO EDELI-CO-CLIENTEO
                END-IF
           END-IF
           PERFORM 803-EXEC-CICS-SEND-DATA-CURSOR
           PERFORM 804-EXEC-CICS-RETURN.

       311-PROCESS-RESET-FIELDS.
           MOVE DFHBMPRF TO EDELI-CONFIRA
           MOVE DFHNEUTR TO EDELI-CONFIRC
           MOVE SPACES TO EDELI-CONFIRO
           MOVE DFHBMUNN TO EDELI-CO-CLIENTEA
           MOVE DFHTURQ TO EDELI-CO-CLIENTEC
           MOVE -1 TO EDELI-CO-CLIENTEL
           MOVE ZEROS TO WKS-COM-COMFIRM-MODE
           INITIALIZE WKS-COM-CO-CLIENTE
           MOVE SPACES TO EDELI-CO-CLIENTEI
           MOVE SPACES TO EDELI-NOM-CLIENTEI
           MOVE SPACES TO EDELI-FECHA-NACI
           MOVE SPACES TO EDELI-TELI
           MOVE SPACES TO EDELI-FECHA-ADICI
           MOVE SPACES TO EDELI-ESTADOI
           MOVE DFHNEUTR TO EDELI-ESTADOC
           MOVE ZEROS TO WKS-COM-PROCESS-MODE.

      *-->ACCION PARA RESTAURAR UN REGISTRO
       400-ACCION-PF3.
           IF WKS-COM-CO-CLIENTE < SPACES
           OR WKS-COM-CO-CLIENTE = ZEROS
                PERFORM 200-ACCION-ENTER
           ELSE
           IF WKS-COM-PROCESS-MODE = 2
           OR WKS-COM-PROCESS-MODE = ZEROS
                MOVE 2 TO WKS-COM-PROCESS-MODE
                PERFORM 310-PROCESS-CHANGE-MARK
           ELSE
                SET WKS-MSG-NOCOMAND TO TRUE
                MOVE -1 TO EDELI-CONFIRL
           END-IF
           PERFORM 803-EXEC-CICS-SEND-DATA-CURSOR
           PERFORM 804-EXEC-CICS-RETURN
           END-IF.

      *-->ACCION PARA SALIR AL MENU PRINCIPAL
       500-ACCION-PF10.
           PERFORM 809-CICS-XCTL-PROGRAM-1
           PERFORM 999-END-PROGRAM.

      *-->ACCION QUE DESPLIEGA MENSAJE DE COMANDO NO HABILITADO
       600-ACCION-OTHER.
           SET WKS-MSG-NOCOMAND TO TRUE
           PERFORM 802-EXEC-CICS-SEND-DATA
           PERFORM 804-EXEC-CICS-RETURN.

      *--> COMANDOS CICS
       801-EXEC-CICS-SEND-ERASE.
           EXEC CICS SEND
                MAP('EDELI')
                MAPSET('EDELI')
                ERASE
           END-EXEC.

       802-EXEC-CICS-SEND-DATA.
           PERFORM 998-PROCESS-DEFAULT-DATA
           PERFORM 997-PROCESS-OUTPUT-MSG
           EXEC CICS SEND
                MAP('EDELI')
                MAPSET('EDELI')
                DATAONLY
           END-EXEC.

       803-EXEC-CICS-SEND-DATA-CURSOR.
           PERFORM 998-PROCESS-DEFAULT-DATA
           PERFORM 997-PROCESS-OUTPUT-MSG
           EXEC CICS SEND
                MAP('EDELI')
                MAPSET('EDELI')
                DATAONLY
                CURSOR
           END-EXEC.

       804-EXEC-CICS-RETURN.
           EXEC CICS
               RETURN
               TRANSID('EDEI')
               COMMAREA(WKS-COMMAREA)
           END-EXEC.

       805-EXEC-CICS-RECEIVE.
           EXEC CICS RECEIVE
               MAP ('EDELI')
               MAPSET ('EDELI')
           END-EXEC.

       806-EXEC-CICS-READ.
           EXEC CICS READ
               FILE('EDM4CL')
               INTO(REG-EDMACL)
               RIDFLD(EDMC-LLAVE)
               NOHANDLE
           END-EXEC
           PERFORM 899-EVALUATE-DFHRESP.

       807-EXEC-CICS-READ-UPDATE.
           EXEC CICS READ
               FILE('EDM4CL')
               INTO(REG-EDMACL)
               RIDFLD(EDMC-LLAVE)
               NOHANDLE
               UPDATE
           END-EXEC
           PERFORM 899-EVALUATE-DFHRESP.

       808-EXEC-CICS-REWRITE.
           EXEC CICS REWRITE
                FILE ('EDM4CL')
                FROM (REG-EDMACL)
                NOHANDLE
           END-EXEC
           PERFORM 899-EVALUATE-DFHRESP.

       809-CICS-XCTL-PROGRAM-1.
           EXEC CICS XCTL
               PROGRAM(WKS-PROGRAM-1)
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
                MOVE "COMANDO INACTIVO"
                TO EDELI-OUTPUT-MSGO
           ELSE IF WKS-MSG-UNKERROR
                MOVE "ERROR DESCONOCIDO"
                TO EDELI-OUTPUT-MSGO
           ELSE IF WKS-MSG-NOTOPEN
                MOVE "ARCHIVO CERRADO"
                TO EDELI-OUTPUT-MSGO
           ELSE IF WKS-MSG-NONUMERIC
                MOVE "CODIGO DE CLIENTE INCORRECTO"
                TO EDELI-OUTPUT-MSGO
           ELSE IF WKS-MSG-NOTFND
                MOVE "NO SE ENCONTRO"
                TO EDELI-OUTPUT-MSGO
           ELSE IF WKS-MSG-FOUND
                MOVE "BUSQUEDA EXITOSA"
                TO EDELI-OUTPUT-MSGO
           ELSE IF WKS-MSG-CELIMINATED
                MOVE "CONFIRME LA ELIMINACION"
                TO EDELI-OUTPUT-MSGO
           ELSE IF WKS-MSG-CRESTORE
                MOVE "CONFIRME LA RESTAURACION"
                TO EDELI-OUTPUT-MSGO
           ELSE IF WKS-MSG-CANCELED
                MOVE "PROCESO CANCELADO"
                TO EDELI-OUTPUT-MSGO
           ELSE IF WKS-MSG-ECOMPLETED
                MOVE "ELIMINACION COMPLETA"
                TO EDELI-OUTPUT-MSGO
           ELSE IF WKS-MSG-RCOMPLETED
                MOVE "RESTAURACION COMPLETA"
                TO EDELI-OUTPUT-MSGO
           ELSE
                MOVE DFHYELLO TO EDELI-OUTPUT-MSGO
                MOVE SPACES   TO EDELI-OUTPUT-MSGO
           END-IF
           MOVE ZEROS TO WKS-OUTPUT-MSG.

      *--> PROCESAR DATOS POR DEFECTO
       998-PROCESS-DEFAULT-DATA.
           MOVE FUNCTION CURRENT-DATE(1:4) TO WKS-DATE-YYYY
           MOVE FUNCTION CURRENT-DATE(5:2) TO WKS-DATE-MM
           MOVE FUNCTION CURRENT-DATE(7:2) TO WKS-DATE-DD
           MOVE WKS-DATE-SIS TO EDELI-DATE-SISO
           MOVE FUNCTION CURRENT-DATE(9:2) TO WKS-TIME-HH
           MOVE FUNCTION CURRENT-DATE(11:2) TO WKS-TIME-MM
           MOVE FUNCTION CURRENT-DATE(13:2) TO WKS-TIME-SS
           MOVE WKS-TIME-SIS TO EDELI-TIME-SISO.

      *--> TERMINACION DEL PROGRAMA
       999-END-PROGRAM.
           MOVE SPACE TO EDELII
           EXEC CICS SEND
               TEXT
               FROM(WKS-END-MSG)
               ERASE
           END-EXEC

           EXEC CICS RETURN
           END-EXEC

           GOBACK.