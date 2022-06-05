      ******************************************************************
      * FECHA       : 03/06/2022                                       *
      * PROGRAMADOR : JOSUE DONIS                                      *
      * APLICACION  : SEMILLERO                                        *
      * PROGRAMA    : EDID1ALI                                         *
      * TIPO        : LINEA                                            *
      * DESCRIPCION : MENU DE ADICION PARA MAESTRO DE CLIENTES         *
      * ARCHIVOS    : EDM4CL                                           *
      * ACCION (ES) : K=ARCHIVO                                        *
      * PROGRAMA(S) : XTCL                                             *
      * CANAL       : ADMINISTRATIVA                                   *
      * INSTALADO   : 03/06/2022                                       *
      * BPM/RATIONAL:                                                  *
      * NOMBRE      : EDGAR MARTINEZ - INSTRUCOR                       *
      * DESCRIPCION : USER5004                                         *
      ******************************************************************
       ID DIVISION.
       PROGRAM-ID. EDID1ALI.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CLASS NUMERO IS '0' '1' '2' '3' '4' '5' '6' '7' '8' '9' ' '.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY DFHBMSCA.
       COPY DFHAID.
       COPY EDALI.

       COPY EDMACL.

       01 WKS-WORK-FIELDS.
           02 WKS-TEMP-NAME             PIC X(40).
           02 WKS-LIMIT-DAYS            PIC 9(02).
           02 WKS-END-MSG               PIC X(14)
           VALUE "MUCHAS GRACIAS".

       01 WKS-TABLES.
           02 WKS-MOUTHS                PIC X(48)
           VALUE "013102280331043005310630073108310930103111301231".
           02 WKS-MOUTHS-TABLE REDEFINES WKS-MOUTHS OCCURS 12.
                03 WKS-NO-MOUTH         PIC 9(02).
                03 WKS-NO-DAYS          PIC 9(02).

       01 WKS-FILE-STATUS               PIC 99 VALUE ZEROS.
           88 WKS-EDM4CL-NOTOPEN        VALUE 1.
           88 WKS-EDM4CL-NOTFND         VALUE 2.
           88 WKS-EDM4CL-NORMAL         VALUE 3.
           88 WKS-EDM4CL-ENDFILE        VALUE 4.

       01 WKS-FLAGS.
           02 WKS-OUTPUT-MSG            PIC 99 VALUE ZEROS.
                88 WKS-MSG-ADDITION     VALUE 1.
                88 WKS-MSG-NOCOMAND     VALUE 2.
                88 WKS-MSG-UNKERROR     VALUE 3.
                88 WKS-MSG-NOTOPEN      VALUE 4.
                88 WKS-MSG-NONUMERIC    VALUE 5.
                88 WKS-MSG-DUPLICATE    VALUE 6.
                88 WKS-MSG-NOALPHA      VALUE 7.
                88 WKS-MSG-NOYEAR       VALUE 8.
                88 WKS-MSG-NOMOUTH      VALUE 9.
                88 WKS-MSG-NODAY        VALUE 10.
                88 WKS-MSG-NOPHONE      VALUE 11.
           02 WKS-FLAG-INVALID          PIC 9 VALUE ZEROS.
           02 WKS-FLAG-LEAP-YEAR        PIC 9 VALUE ZEROS.

       01 WKS-SUBSCRIPTS.
           02 WKS-INDEX                 PIC 99 VALUE ZEROS.
           02 WKS-AUX                   PIC 99 VALUE ZEROS.

       01 WKS-POINTERS.
           02 WKS-STRING-PTR            PIC 99 VALUE 1.
           02 WKS-UNSTRING-PTR          PIC 99 VALUE 1.
                88 WKS-COMPLETED        VALUE 41.

       01 WKS-PROGRAM-SPECS.
           02 WKS-PROGRAM-NAME          PIC X(08) VALUE "EDID1ALI".
           02 WKS-PROGRAM-1             PIC X(08) VALUE "EDID1YL5".
           02 WKS-COMMAREA              PIC X(03) VALUE "123".

       01 WKS-EDITED-FIELDS.
           02 WKS-DATE-SIS.
                03 WKS-DATE-DD          PIC 99.
                03 WKS-DATE-MM          PIC /99.
                03 WKS-DATE-YYYY        PIC /9999.
           02 WKS-TIME-SIS.
                03 WKS-TIME-HH          PIC 99.
                03 FILLER               PIC X VALUE ":".
                03 WKS-TIME-MM          PIC 99.
                03 FILLER               PIC X VALUE ":".
                03 WKS-TIME-SS          PIC 99.
           02 WKS-DATE-FORMAT           PIC 99/99/9999.
           02 WKS-TEMP-FIELDS.
                03 WKS-CO-CLIENTE       PIC 9(08).
                03 WKS-NOM-CLIENTE      PIC X(40).
                03 WKS-FECHA-NAC.
                    04 WKS-FECHA-NACDD  PIC 99.
                    04 WKS-FECHA-NACMM  PIC 99.
                    04 WKS-FECHA-NACAA  PIC 9999.
                03 WKS-AUX-FECHA        PIC 9(08).

       LINKAGE SECTION.
       01 DFHCOMMAREA                      PIC X(03).

       PROCEDURE DIVISION.
       000-MAIN-PROCESS.

      *-->EVALUATE PARA LA ACCION DE CADA TECLA
           EVALUATE TRUE
                WHEN EIBCALEN = 0
                    PERFORM 100-ACCION-DEFAULT
               WHEN EIBAID = DFHPF1
                    PERFORM 200-ACCION-PF1
               WHEN EIBAID = DFHPF10
                    PERFORM 300-ACCION-PF10
               WHEN OTHER
                    PERFORM 400-ACCION-OTHER
           END-EVALUATE.

      *-->ACCION DEFAULT
       100-ACCION-DEFAULT.
           PERFORM 801-EXEC-CICS-SEND-ERASE
           PERFORM 802-EXEC-CICS-SEND-DATA
           PERFORM 804-EXEC-CICS-RETURN.

      *-->ACCION PARA GUARDAR UN NUEVO REGISTRO
       200-ACCION-PF1.
           PERFORM 805-EXEC-CICS-RECEIVE
           PERFORM 210-PROCESS-VALID-DATA
           IF WKS-FLAG-INVALID = ZEROS
               PERFORM 220-PROCESS-DATA
               PERFORM 807-EXEC-CICS-WRITE
               MOVE -1 TO EDALI-CO-CLIENTEL
               IF WKS-EDM4CL-NORMAL
                   SET WKS-MSG-ADDITION TO TRUE
               ELSE
                   SET WKS-MSG-UNKERROR TO TRUE
               END-IF
           END-IF
           PERFORM 803-EXEC-CICS-SEND-DATA-CURSOR
           PERFORM 804-EXEC-CICS-RETURN.

       210-PROCESS-VALID-DATA.
           INITIALIZE WKS-FLAG-INVALID
           MOVE DFHTURQ TO EDALI-CO-CLIENTEC
           MOVE DFHTURQ TO EDALI-NOM-CLIENTEC
           MOVE DFHTURQ TO EDALI-FECHA-NACAAC
           MOVE DFHTURQ TO EDALI-FECHA-NACMMC
           MOVE DFHTURQ TO EDALI-FECHA-NACDDC
           MOVE DFHTURQ TO EDALI-TELC

           PERFORM 211-VALID-FIELD-1
           IF WKS-FLAG-INVALID = ZEROS
                PERFORM 212-VALID-FIELD-2
           END-IF
           IF WKS-FLAG-INVALID = ZEROS
                PERFORM 213-VALID-FIELD-3
           END-IF
           IF WKS-FLAG-INVALID = ZEROS
                PERFORM 214-VALID-FIELD-4
           END-IF
           IF WKS-FLAG-INVALID = ZEROS
                PERFORM 215-VALID-FIELD-5
           END-IF
           IF WKS-FLAG-INVALID = ZEROS
                PERFORM 217-VALID-FIELD-6
           END-IF.

      *-->VALIDACION DEL CAMPO CODIGO DE CLIENTE
       211-VALID-FIELD-1.
           IF EDALI-CO-CLIENTEI = SPACES
                SET WKS-MSG-NONUMERIC TO TRUE
                MOVE 1 TO WKS-FLAG-INVALID
           ELSE IF EDALI-CO-CLIENTEI NOT NUMERO
                SET WKS-MSG-NONUMERIC TO TRUE
                MOVE 1 TO WKS-FLAG-INVALID
           ELSE
                INITIALIZE WKS-CO-CLIENTE
                INITIALIZE WKS-INDEX
                MOVE 9 TO WKS-AUX
                PERFORM VARYING WKS-INDEX FROM 8 BY -1
                UNTIL WKS-INDEX = ZEROS
                    IF EDALI-CO-CLIENTEI(WKS-INDEX:1) NOT EQUAL ' '
                        SUBTRACT 1 FROM WKS-AUX
                        MOVE EDALI-CO-CLIENTEI(WKS-INDEX:1)
                        TO WKS-CO-CLIENTE(WKS-AUX:1)
                    END-IF
                END-PERFORM
                MOVE WKS-CO-CLIENTE TO EDALI-CO-CLIENTEO
           END-IF

           IF WKS-FLAG-INVALID = ZEROS
                MOVE WKS-CO-CLIENTE TO EDMC-LLAVE
                PERFORM 806-EXEC-CICS-READ
                IF WKS-EDM4CL-NORMAL
                   SET WKS-MSG-DUPLICATE TO TRUE
                   MOVE 1 TO WKS-FLAG-INVALID
                END-IF
                IF WKS-EDM4CL-NOTOPEN
                   SET WKS-MSG-NOTOPEN TO TRUE
                   MOVE 1 TO WKS-FLAG-INVALID
                END-IF
           END-IF

           IF WKS-FLAG-INVALID = 1
                MOVE DFHRED TO EDALI-CO-CLIENTEC
                MOVE -1 TO EDALI-CO-CLIENTEL
           END-IF.

      *-->VALIDACION DEL CAMPO DE NOMBRE
       212-VALID-FIELD-2.
           IF EDALI-NOM-CLIENTEI < SPACES
                SET WKS-MSG-NOALPHA TO TRUE
                MOVE 1 TO WKS-FLAG-INVALID
           ELSE
                UNSTRING EDALI-NOM-CLIENTEI DELIMITED BY ALL SPACES
                    INTO WKS-TEMP-NAME WITH POINTER WKS-UNSTRING-PTR
                END-UNSTRING
                PERFORM UNTIL WKS-COMPLETED
                    STRING WKS-TEMP-NAME DELIMITED BY SPACES
                        " " DELIMITED BY SIZE
                        INTO WKS-NOM-CLIENTE WITH POINTER WKS-STRING-PTR
                    END-STRING
                    UNSTRING EDALI-NOM-CLIENTEI DELIMITED BY ALL SPACES
                        INTO WKS-TEMP-NAME WITH POINTER WKS-UNSTRING-PTR
                    END-UNSTRING
                END-PERFORM
                STRING WKS-TEMP-NAME DELIMITED BY SIZE
                    INTO WKS-NOM-CLIENTE WITH POINTER WKS-STRING-PTR
                END-STRING
                MOVE WKS-NOM-CLIENTE TO EDALI-NOM-CLIENTEO
           END-IF

           IF WKS-FLAG-INVALID = 1
                MOVE DFHRED TO EDALI-NOM-CLIENTEC
                MOVE -1 TO EDALI-NOM-CLIENTEL
           END-IF.

      *-->VALIDACION DEL CAMPO DE ANIO DE NACIMIENTO
       213-VALID-FIELD-3.
           IF EDALI-FECHA-NACAAI < SPACES
                SET WKS-MSG-NOYEAR TO TRUE
                MOVE 1 TO WKS-FLAG-INVALID
           END-IF
           IF EDALI-FECHA-NACAAI NOT NUMERO
                SET WKS-MSG-NOYEAR TO TRUE
                MOVE 1 TO WKS-FLAG-INVALID
           END-IF
           IF EDALI-FECHA-NACAAO < 1900
           OR EDALI-FECHA-NACAAO > FUNCTION CURRENT-DATE(1:4)
                SET WKS-MSG-NOYEAR TO TRUE
                MOVE 1 TO WKS-FLAG-INVALID
           END-IF

           IF WKS-FLAG-INVALID = 1
                MOVE DFHRED TO EDALI-FECHA-NACAAC
                MOVE -1 TO EDALI-FECHA-NACAAL
           ELSE
                MOVE EDALI-FECHA-NACAAO TO WKS-FECHA-NACAA
           END-IF.

      *-->VALIDACION DEL CAMPO DE MES DE NACIMIENTO
       214-VALID-FIELD-4.
           IF EDALI-FECHA-NACMMI < SPACES
               SET WKS-MSG-NOMOUTH TO TRUE
               MOVE 1 TO WKS-FLAG-INVALID
           END-IF
           IF EDALI-FECHA-NACMMI NOT NUMERO
               SET WKS-MSG-NOMOUTH TO TRUE
               MOVE 1 TO WKS-FLAG-INVALID
           END-IF
           IF WKS-FLAG-INVALID = ZEROS
               INITIALIZE WKS-FECHA-NACMM
               INITIALIZE WKS-INDEX
               MOVE 3 TO WKS-AUX
               PERFORM VARYING WKS-INDEX FROM 2 BY -1
               UNTIL WKS-INDEX = ZEROS
                   IF EDALI-FECHA-NACMMI(WKS-INDEX:1) NOT EQUAL ' '
                       SUBTRACT 1 FROM WKS-AUX
                       MOVE EDALI-FECHA-NACMMI(WKS-INDEX:1)
                       TO WKS-FECHA-NACMM(WKS-AUX:1)
                   END-IF
               END-PERFORM
               MOVE WKS-FECHA-NACMM TO EDALI-FECHA-NACMMO
           END-IF

           IF EDALI-FECHA-NACAAO = FUNCTION CURRENT-DATE(1:4)
               IF EDALI-FECHA-NACMMO < 1
               OR EDALI-FECHA-NACMMO > FUNCTION CURRENT-DATE(5:2)
                   SET WKS-MSG-NOMOUTH TO TRUE
                   MOVE 1 TO WKS-FLAG-INVALID
               END-IF
           ELSE
               IF EDALI-FECHA-NACMMO < 1
               OR EDALI-FECHA-NACMMO > 12
                   SET WKS-MSG-NOMOUTH TO TRUE
                   MOVE 1 TO WKS-FLAG-INVALID
               END-IF
           END-IF

           IF WKS-FLAG-INVALID = 1
               MOVE DFHRED TO EDALI-FECHA-NACMMC
               MOVE -1 TO EDALI-FECHA-NACMML
           END-IF.

      *-->VALIDACION DEL CAMPO DE DIA DE NACIMIENTO
       215-VALID-FIELD-5.
           IF EDALI-FECHA-NACDDI < SPACES
               SET WKS-MSG-NODAY TO TRUE
               MOVE 1 TO WKS-FLAG-INVALID
           END-IF
           IF EDALI-FECHA-NACDDI NOT NUMERO
               SET WKS-MSG-NODAY TO TRUE
               MOVE 1 TO WKS-FLAG-INVALID
           END-IF
           IF WKS-FLAG-INVALID = ZEROS
               INITIALIZE WKS-FECHA-NACDD
               INITIALIZE WKS-INDEX
               MOVE 3 TO WKS-AUX
               PERFORM VARYING WKS-INDEX FROM 2 BY -1
               UNTIL WKS-INDEX = ZEROS
                   IF EDALI-FECHA-NACDDI(WKS-INDEX:1) NOT EQUAL ' '
                       SUBTRACT 1 FROM WKS-AUX
                       MOVE EDALI-FECHA-NACDDI(WKS-INDEX:1)
                       TO WKS-FECHA-NACDD(WKS-AUX:1)
                   END-IF
               END-PERFORM
               MOVE WKS-FECHA-NACDD TO EDALI-FECHA-NACDDO
           END-IF

           IF  EDALI-FECHA-NACMMO = FUNCTION CURRENT-DATE(5:2)
           AND EDALI-FECHA-NACAAO = FUNCTION CURRENT-DATE(1:4)
               IF EDALI-FECHA-NACDDO < 1
               OR EDALI-FECHA-NACDDO > FUNCTION CURRENT-DATE(7:2)
                   SET WKS-MSG-NODAY TO TRUE
                   MOVE 1 TO WKS-FLAG-INVALID
               END-IF
           END-IF

           PERFORM 216-PROCESS-LEAP-YEAR
           IF  WKS-FLAG-LEAP-YEAR = 1
           AND EDALI-FECHA-NACMMO = 2
               IF EDALI-FECHA-NACDDO < 1
               OR EDALI-FECHA-NACDDO > 29
                   SET WKS-MSG-NODAY TO TRUE
                   MOVE 1 TO WKS-FLAG-INVALID
               END-IF
           ELSE
               MOVE 1 TO WKS-INDEX
               PERFORM UNTIL WKS-INDEX > 12
                   IF EDALI-FECHA-NACMMO = WKS-NO-MOUTH(WKS-INDEX)
                       MOVE WKS-NO-DAYS(WKS-INDEX) TO WKS-LIMIT-DAYS
                   END-IF
                   ADD 1 TO WKS-INDEX
               END-PERFORM
               IF EDALI-FECHA-NACDDO < 1
               OR EDALI-FECHA-NACDDO > WKS-LIMIT-DAYS
                   SET WKS-MSG-NODAY TO TRUE
                   MOVE 1 TO WKS-FLAG-INVALID
               END-IF
           END-IF

           IF WKS-FLAG-INVALID = 1
               MOVE DFHRED TO EDALI-FECHA-NACDDC
               MOVE -1 TO EDALI-FECHA-NACDDL
           END-IF.

       216-PROCESS-LEAP-YEAR.
           EVALUATE TRUE
           WHEN FUNCTION MOD (EDALI-FECHA-NACAAO 4) NOT ZERO
           WHEN FUNCTION MOD (EDALI-FECHA-NACAAO 100) ZERO
           AND FUNCTION MOD (EDALI-FECHA-NACAAO 400) NOT ZERO
               MOVE 0 TO WKS-FLAG-LEAP-YEAR
           WHEN OTHER
               MOVE 1 TO WKS-FLAG-LEAP-YEAR
           END-EVALUATE.

      *-->VALIDACION DEL CAMPO DE TELEFONO
       217-VALID-FIELD-6.
           IF EDALI-TELI < SPACES
               SET WKS-MSG-NOPHONE TO TRUE
               MOVE 1 TO WKS-FLAG-INVALID
           END-IF
           IF EDALI-TELI NOT NUMERO
               SET WKS-MSG-NOPHONE TO TRUE
               MOVE 1 TO WKS-FLAG-INVALID
           END-IF
           IF EDALI-TELO < 10000000
               SET WKS-MSG-NOPHONE TO TRUE
               MOVE 1 TO WKS-FLAG-INVALID
           END-IF

           IF WKS-FLAG-INVALID = 1
               MOVE DFHRED TO EDALI-TELC
               MOVE -1 TO EDALI-TELL
           END-IF.

       220-PROCESS-DATA.
           MOVE EDALI-CO-CLIENTEO TO EDMC-LLAVE
           MOVE EDALI-NOM-CLIENTEO TO EDMC-NOMBRE-CLIENTE
           MOVE WKS-FECHA-NAC TO WKS-AUX-FECHA
           MOVE WKS-AUX-FECHA TO EDMC-FECHA-NAC-O-CONSTITUC
           MOVE EDALI-TELO TO EDMC-NUMERO-TELEFONO
           MOVE FUNCTION CURRENT-DATE(1:4) TO WKS-FECHA-NACAA
           MOVE FUNCTION CURRENT-DATE(5:2) TO WKS-FECHA-NACMM
           MOVE FUNCTION CURRENT-DATE(7:2) TO WKS-FECHA-NACDD
           MOVE WKS-FECHA-NAC TO WKS-AUX-FECHA
           MOVE WKS-AUX-FECHA TO EDMC-FECHA-CREACION
           MOVE SPACES TO EDMC-MARCA-ELIMINADO
           MOVE SPACES TO EDMC-FILLER.

      *-->ACCION PARA SALIR AL MENU PRINCIPAL
       300-ACCION-PF10.
           PERFORM 808-EXEC-CICS-XCTL-PROGRAM-1
           PERFORM 999-END-PROGRAM.

      *-->ACCION QUE DESPLIEGA MENSAJE DE COMANDO NO HABILITADO
       400-ACCION-OTHER.
           SET WKS-MSG-NOCOMAND TO TRUE
           PERFORM 802-EXEC-CICS-SEND-DATA
           PERFORM 804-EXEC-CICS-RETURN.

      *--> COMANDOS CICS
       801-EXEC-CICS-SEND-ERASE.
           EXEC CICS SEND
                MAP('EDALI')
                MAPSET('EDALI')
                ERASE
           END-EXEC.

       802-EXEC-CICS-SEND-DATA.
           PERFORM 998-PROCESS-DEFAULT-DATA
           PERFORM 997-PROCESS-OUTPUT-MSG
           EXEC CICS SEND
                MAP('EDALI')
                MAPSET('EDALI')
                DATAONLY
           END-EXEC.

       803-EXEC-CICS-SEND-DATA-CURSOR.
           PERFORM 998-PROCESS-DEFAULT-DATA
           PERFORM 997-PROCESS-OUTPUT-MSG
           EXEC CICS SEND
                MAP('EDALI')
                MAPSET('EDALI')
                DATAONLY
                CURSOR
           END-EXEC.

       804-EXEC-CICS-RETURN.
           EXEC CICS RETURN
                TRANSID('EDAI')
                COMMAREA(WKS-COMMAREA)
           END-EXEC.

       805-EXEC-CICS-RECEIVE.
           EXEC CICS RECEIVE
                MAP ('EDALI')
                MAPSET ('EDALI')
           END-EXEC.

       806-EXEC-CICS-READ.
           EXEC CICS READ
                FILE('EDM4CL')
                INTO(REG-EDMACL)
                RIDFLD(EDMC-LLAVE)
                NOHANDLE
           END-EXEC
           PERFORM 899-EVALUATE-DFHRESP.

       807-EXEC-CICS-WRITE.
           EXEC CICS WRITE
               FILE('EDM4CL')
               FROM(REG-EDMACL)
               RIDFLD(EDMC-LLAVE)
           END-EXEC
           PERFORM 899-EVALUATE-DFHRESP.

       808-EXEC-CICS-XCTL-PROGRAM-1.
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
                MOVE "COMANDO NO ACTIVO"
                TO EDALI-OUTPUT-MSGO
           ELSE IF WKS-MSG-UNKERROR
                MOVE "ERROR DESCONOCIDO"
                TO EDALI-OUTPUT-MSGO
           ELSE IF WKS-MSG-NOTOPEN
                MOVE "ARCHIVO CERRADO"
                TO EDALI-OUTPUT-MSGO
           ELSE IF WKS-MSG-NONUMERIC
                MOVE "CODIGO DE CLIENTE INCORRECTO"
                TO EDALI-OUTPUT-MSGO
           ELSE IF WKS-MSG-ADDITION
                MOVE "ADICION COMPLETA"
                TO EDALI-OUTPUT-MSGO
           ELSE IF WKS-MSG-DUPLICATE
                MOVE "CODIGO EXISTENTE"
                TO EDALI-OUTPUT-MSGO
           ELSE IF WKS-MSG-NOALPHA
                MOVE "NOMBRE INCORRECTO"
                TO EDALI-OUTPUT-MSGO
           ELSE IF WKS-MSG-NOYEAR
                MOVE "ANIO INCORRECTO"
                TO EDALI-OUTPUT-MSGO
           ELSE IF WKS-MSG-NOMOUTH
                MOVE "MES INCORRECTO"
                TO EDALI-OUTPUT-MSGO
           ELSE IF WKS-MSG-NODAY
                MOVE "DIA INCORRECTO"
                TO EDALI-OUTPUT-MSGO
           ELSE IF WKS-MSG-NOPHONE
                MOVE "TELEFONO INCORRECTO"
                TO EDALI-OUTPUT-MSGO
           ELSE
                MOVE DFHYELLO TO EDALI-OUTPUT-MSGO
                MOVE SPACES   TO EDALI-OUTPUT-MSGO
           END-IF
           MOVE ZEROS TO WKS-OUTPUT-MSG.

      *--> PROCESAR DATOS POR DEFECTO
       998-PROCESS-DEFAULT-DATA.
           MOVE FUNCTION CURRENT-DATE(1:4) TO WKS-DATE-YYYY
           MOVE FUNCTION CURRENT-DATE(5:2) TO WKS-DATE-MM
           MOVE FUNCTION CURRENT-DATE(7:2) TO WKS-DATE-DD
           MOVE WKS-DATE-SIS TO EDALI-DATE-SISO
           MOVE WKS-DATE-SIS TO EDALI-FECHA-ADICO
           MOVE FUNCTION CURRENT-DATE(9:2) TO WKS-TIME-HH
           MOVE FUNCTION CURRENT-DATE(11:2) TO WKS-TIME-MM
           MOVE FUNCTION CURRENT-DATE(13:2) TO WKS-TIME-SS
           MOVE WKS-TIME-SIS TO EDALI-TIME-SISO.

      *--> TERMINACION DEL PROGRAMA
       999-END-PROGRAM.
           MOVE SPACE TO EDALII
           EXEC CICS SEND
               TEXT
               FROM(WKS-END-MSG)
               ERASE
           END-EXEC

           EXEC CICS RETURN
           END-EXEC

           GOBACK.