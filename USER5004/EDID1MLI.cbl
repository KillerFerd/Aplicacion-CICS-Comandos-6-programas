      ******************************************************************
      * FECHA       : 03/06/2022                                       *
      * PROGRAMADOR : JOSUE DONIS                                      *
      * APLICACION  : SEMILLERO                                        *
      * PROGRAMA    : EDID1MLI                                         *
      * TIPO        : LINEA                                            *
      * DESCRIPCION : MENU DE MODIFICACION PARA MAESTRO DE CLIENTES    *
      * ARCHIVOS    : EDM4CL                                           *
      * ACCION (ES) : A=ACTUALIZA                                      *
      * PROGRAMA(S) : XTCL                                             *
      * CANAL       : ADMINISTRATIVA                                   *
      * INSTALADO   : 03/06/2022                                       *
      * BPM/RATIONAL:                                                  *
      * NOMBRE      : EDGAR MARTINEZ - INSTRUCOR                       *
      * DESCRIPCION : USER5004                                         *
      ******************************************************************
       ID DIVISION.
       PROGRAM-ID. EDID1MLI.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CLASS NUMERO IS '0' '1' '2' '3' '4' '5' '6' '7' '8' '9' ' '.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY DFHBMSCA.
       COPY DFHAID.
       COPY EDMLI.

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
                88 WKS-MSG-NOCOMAND     VALUE 1.
                88 WKS-MSG-UNKERROR     VALUE 3.
                88 WKS-MSG-NOTOPEN      VALUE 4.
                88 WKS-MSG-NONUMERIC    VALUE 5.
                88 WKS-MSG-DUPLICATE    VALUE 6.
                88 WKS-MSG-NOALPHA      VALUE 7.
                88 WKS-MSG-NOYEAR       VALUE 8.
                88 WKS-MSG-NOMOUTH      VALUE 9.
                88 WKS-MSG-NODAY        VALUE 10.
                88 WKS-MSG-NOPHONE      VALUE 11.
                88 WKS-MSG-NOTFND       VALUE 12.
                88 WKS-MSG-PROTECTED    VALUE 13.
                88 WKS-MSG-MOD          VALUE 14.
                88 WKS-MSG-FOUND        VALUE 15.
                88 WKS-MSG-NOCHANGES    VALUE 16.
           02 WKS-FLAG-INVALID          PIC 9 VALUE ZEROS.
           02 WKS-FLAG-LEAP-YEAR        PIC 9 VALUE ZEROS.
           02 WKS-FLAG-NO-CHANGES       PIC 9 VALUE ZEROS.

       01 WKS-SUBSCRIPTS.
           02 WKS-INDEX                 PIC 99 VALUE ZEROS.
           02 WKS-AUX                   PIC 99 VALUE ZEROS.

       01 WKS-POINTERS.
           02 WKS-STRING-PTR            PIC 99 VALUE 1.
           02 WKS-UNSTRING-PTR          PIC 99 VALUE 1.
               88 WKS-COMPLETED         VALUE 41.

       01 WKS-PROGRAM-SPECS.
           02 WKS-PROGRAM-NAME          PIC X(08) VALUE "EDID1MLI".
           02 WKS-PROGRAM-1             PIC X(08) VALUE "EDID1YL5".
           02 WKS-COMMAREA.
               03 WKS-COM-PROTECTED     PIC 9.
               03 WKS-COM-CO-CLIENTE    PIC 9(08).

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
                03 WKS-FECHA-NAC.
                    04 WKS-FECHA-NACDD  PIC 99.
                    04 WKS-FECHA-NACMM  PIC 99.
                    04 WKS-FECHA-NACAA  PIC 9999.
                03 WKS-TEL              PIC 9(08).
                03 WKS-FECHA-ADIC       PIC 9(08).

       LINKAGE SECTION.
       01 DFHCOMMAREA                   PIC X(09).

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
           PERFORM 802-EXEX-CICS-SEND-DATA
           PERFORM 804-EXEC-CICS-RETURN.

      *-->ACCION PARA VALIDAR CODIGO DE CLIENTE INGRESADO
       200-ACCION-ENTER.
           IF WKS-COM-PROTECTED = 1
                SET WKS-MSG-PROTECTED TO TRUE
                MOVE -1 TO EDMLI-NOM-CLIENTEL
           ELSE
                INITIALIZE WKS-FLAG-INVALID
                PERFORM 804-EXEC-CICS-RECEIVE
                PERFORM 210-VALID-FIELD-1
                IF WKS-FLAG-INVALID = ZEROS
                    PERFORM 220-LOAD-DATA-TEMP
                    PERFORM 230-LOAD-DATA-OUTPUT
                    MOVE DFHNEUTR TO EDMLI-CO-CLIENTEC
                    MOVE DFHBMPRF TO EDMLI-CO-CLIENTEA
                    PERFORM 240-UNPROTECTED-FIELDS
                    SET WKS-MSG-FOUND TO TRUE
                    MOVE 1 TO WKS-COM-PROTECTED
                    MOVE WKS-CO-CLIENTE TO WKS-COM-CO-CLIENTE
                ELSE
                    MOVE -1 TO EDMLI-CO-CLIENTEL
                END-IF
           END-IF
           PERFORM 803-EXEX-CICS-SEND-DATA-CURSOR
           PERFORM 804-EXEC-CICS-RETURN.

      *-->VALIDACION DEL CAMPO CODIGO DE CLIENTE
       210-VALID-FIELD-1.
           IF EDMLI-CO-CLIENTEI = SPACES
               SET WKS-MSG-NONUMERIC TO TRUE
               MOVE 1 TO WKS-FLAG-INVALID
           ELSE IF EDMLI-CO-CLIENTEI NOT NUMERO
               SET WKS-MSG-NONUMERIC TO TRUE
               MOVE 1 TO WKS-FLAG-INVALID
           ELSE
               INITIALIZE WKS-CO-CLIENTE
               INITIALIZE WKS-INDEX
               MOVE 9 TO WKS-AUX
               PERFORM VARYING WKS-INDEX FROM 8 BY -1
               UNTIL WKS-INDEX = ZEROS
                   IF EDMLI-CO-CLIENTEI(WKS-INDEX:1) NOT EQUAL ' '
                       SUBTRACT 1 FROM WKS-AUX
                       MOVE EDMLI-CO-CLIENTEI(WKS-INDEX:1)
                       TO WKS-CO-CLIENTE(WKS-AUX:1)
                   END-IF
               END-PERFORM
               MOVE WKS-CO-CLIENTE TO EDMLI-CO-CLIENTEO
           END-IF

           IF WKS-FLAG-INVALID = ZEROS
               MOVE WKS-CO-CLIENTE TO EDMC-LLAVE
               PERFORM 805-EXEC-CICS-READ
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
               MOVE DFHRED TO EDMLI-CO-CLIENTEC
           END-IF.

       220-LOAD-DATA-TEMP.
           MOVE EDMC-LLAVE                  TO WKS-CO-CLIENTE
           MOVE EDMC-NOMBRE-CLIENTE         TO WKS-NOM-CLIENTE
           MOVE EDMC-FECHA-NAC-O-CONSTITUC  TO WKS-AUX-FECHA
           MOVE WKS-AUX-FECHA               TO WKS-FECHA-NAC
           MOVE EDMC-NUMERO-TELEFONO        TO WKS-TEL
           MOVE EDMC-FECHA-CREACION         TO WKS-FECHA-ADIC.

       230-LOAD-DATA-OUTPUT.
           MOVE WKS-NOM-CLIENTE             TO EDMLI-NOM-CLIENTEO
           MOVE WKS-NOM-CLIENTE             TO EDMLI-NOM-CLIENTEI
           MOVE WKS-FECHA-NACDD             TO EDMLI-FECHA-NACDDO
           MOVE WKS-FECHA-NACDD             TO EDMLI-FECHA-NACDDI
           MOVE WKS-FECHA-NACMM             TO EDMLI-FECHA-NACMMO
           MOVE WKS-FECHA-NACMM             TO EDMLI-FECHA-NACMMI
           MOVE WKS-FECHA-NACAA             TO EDMLI-FECHA-NACAAO
           MOVE WKS-FECHA-NACAA             TO EDMLI-FECHA-NACAAI
           MOVE WKS-TEL                     TO EDMLI-TELO
           MOVE WKS-TEL                     TO EDMLI-TELI
           MOVE WKS-FECHA-ADIC              TO EDMLI-FECHA-ADICO.

       240-UNPROTECTED-FIELDS.
           MOVE -1 TO EDMLI-NOM-CLIENTEL
           MOVE DFHTURQ TO EDMLI-NOM-CLIENTEC
           MOVE DFHTURQ TO EDMLI-FECHA-NACAAC
           MOVE DFHTURQ TO EDMLI-FECHA-NACMMC
           MOVE DFHTURQ TO EDMLI-FECHA-NACDDC
           MOVE DFHTURQ TO EDMLI-TELC
           MOVE DFHBMUNP TO EDMLI-NOM-CLIENTEA
           MOVE DFHBMUNP TO EDMLI-FECHA-NACAAA
           MOVE DFHBMUNP TO EDMLI-FECHA-NACMMA
           MOVE DFHBMUNP TO EDMLI-FECHA-NACDDA
           MOVE DFHBMUNP TO EDMLI-TELA.

      *-->ACCION PARA MODIFICAR EL REGISTRO SOLICITADO
       300-ACCION-PF2.
           IF WKS-COM-PROTECTED = 1
                MOVE WKS-COM-CO-CLIENTE TO EDMC-LLAVE
                PERFORM 806-EXEC-CICS-READ-UPDATE
                IF WKS-EDM4CL-NOTOPEN
                     SET WKS-MSG-NOTOPEN TO TRUE
                     MOVE 1 TO WKS-FLAG-INVALID
                END-IF
                IF WKS-EDM4CL-NOTFND
                    SET WKS-MSG-NOTFND TO TRUE
                    MOVE 1 TO WKS-FLAG-INVALID
                END-IF
                PERFORM 220-LOAD-DATA-TEMP
                PERFORM 310-PROCESS-VALID
           ELSE
                PERFORM 200-ACCION-ENTER
           END-IF
           PERFORM 803-EXEX-CICS-SEND-DATA-CURSOR
           PERFORM 804-EXEC-CICS-RETURN.

       310-PROCESS-VALID.
           PERFORM 804-EXEC-CICS-RECEIVE
           PERFORM 311-PROCESS-CHANGES
           IF WKS-FLAG-NO-CHANGES = 1
                SET WKS-MSG-NOCHANGES TO TRUE
                MOVE -1 TO EDMLI-NOM-CLIENTEL
           ELSE
                PERFORM 312-PROCESS-VALID-DATA
                IF WKS-FLAG-INVALID = ZEROS
                    PERFORM 319-PROCESS-REG-DATA
                    PERFORM 807-EXEC-CICS-REWRITE
                    IF WKS-EDM4CL-NORMAL
                        SET WKS-MSG-MOD TO TRUE
                        MOVE -1 TO EDMLI-CO-CLIENTEL
                    ELSE
                        SET WKS-MSG-UNKERROR TO TRUE
                        END-IF
                END-IF
           END-IF
           PERFORM 803-EXEX-CICS-SEND-DATA-CURSOR
           PERFORM 804-EXEC-CICS-RETURN.

       311-PROCESS-CHANGES.
           MOVE 1 TO WKS-FLAG-NO-CHANGES
           IF EDMLI-NOM-CLIENTEI < SPACES
           OR EDMLI-NOM-CLIENTEI = WKS-NOM-CLIENTE
                MOVE WKS-NOM-CLIENTE TO EDMLI-NOM-CLIENTEI
           ELSE
                MOVE ZEROS TO WKS-FLAG-NO-CHANGES
           END-IF
           IF EDMLI-FECHA-NACAAI < SPACES
           OR EDMLI-FECHA-NACAAI = WKS-FECHA-NACAA
               MOVE WKS-FECHA-NACAA TO EDMLI-FECHA-NACAAI
           ELSE
               MOVE ZEROS TO WKS-FLAG-NO-CHANGES
           END-IF
           IF EDMLI-FECHA-NACMMI < SPACES
           OR EDMLI-FECHA-NACMMI = WKS-FECHA-NACMM
               MOVE WKS-FECHA-NACMM TO EDMLI-FECHA-NACMMI
           ELSE
               MOVE ZEROS TO WKS-FLAG-NO-CHANGES
           END-IF
           IF EDMLI-FECHA-NACDDI < SPACES
           OR EDMLI-FECHA-NACDDI = WKS-FECHA-NACDD
               MOVE WKS-FECHA-NACDD TO EDMLI-FECHA-NACDDI
           ELSE
               MOVE ZEROS TO WKS-FLAG-NO-CHANGES
           END-IF
           IF EDMLI-TELI < SPACES
           OR EDMLI-TELI = WKS-TEL
               MOVE WKS-TEL TO EDMLI-TELI
               MOVE WKS-TEL TO EDMLI-TELO
           ELSE
               MOVE ZEROS TO WKS-FLAG-NO-CHANGES
           END-IF.

       312-PROCESS-VALID-DATA.
           INITIALIZE WKS-FLAG-INVALID
           MOVE DFHTURQ TO EDMLI-NOM-CLIENTEC
           MOVE DFHTURQ TO EDMLI-FECHA-NACAAC
           MOVE DFHTURQ TO EDMLI-FECHA-NACMMC
           MOVE DFHTURQ TO EDMLI-FECHA-NACDDC
           MOVE DFHTURQ TO EDMLI-TELC
           PERFORM 313-VALID-FIELD-2
           IF WKS-FLAG-INVALID = ZEROS
               PERFORM 314-VALID-FIELD-3
           END-IF
           IF WKS-FLAG-INVALID = ZEROS
               PERFORM 315-VALID-FIELD-4
           END-IF
           IF WKS-FLAG-INVALID = ZEROS
               PERFORM 316-VALID-FIELD-5
           END-IF
           IF WKS-FLAG-INVALID = ZEROS
               PERFORM 318-VALID-FIELD-6
           END-IF.

      *-->VALIDACION DEL CAMPO DE NOMBRE
       313-VALID-FIELD-2.
           IF EDMLI-NOM-CLIENTEI < SPACES
               SET WKS-MSG-NOALPHA TO TRUE
               MOVE 1 TO WKS-FLAG-INVALID
           ELSE
               UNSTRING EDMLI-NOM-CLIENTEI DELIMITED BY ALL SPACES
                   INTO WKS-TEMP-NAME WITH POINTER WKS-UNSTRING-PTR
               END-UNSTRING
               PERFORM UNTIL WKS-COMPLETED
                   STRING WKS-TEMP-NAME DELIMITED BY SPACES
                          " " DELIMITED BY SIZE
                       INTO WKS-NOM-CLIENTE WITH POINTER WKS-STRING-PTR
                   END-STRING
                   UNSTRING EDMLI-NOM-CLIENTEI DELIMITED BY ALL SPACES
                       INTO WKS-TEMP-NAME WITH POINTER WKS-UNSTRING-PTR
                   END-UNSTRING
               END-PERFORM
               STRING WKS-TEMP-NAME DELIMITED BY SIZE
                   INTO WKS-NOM-CLIENTE WITH POINTER WKS-STRING-PTR
               END-STRING
               MOVE WKS-NOM-CLIENTE TO EDMLI-NOM-CLIENTEO
           END-IF
           IF WKS-FLAG-INVALID = 1
               MOVE DFHRED TO EDMLI-NOM-CLIENTEC
               MOVE -1 TO EDMLI-NOM-CLIENTEL
           END-IF.

      *-->VALIDACION DEL CAMPO DE ANIO DE NACIMIENTO
       314-VALID-FIELD-3.
           IF EDMLI-FECHA-NACAAI < SPACES
               SET WKS-MSG-NOYEAR TO TRUE
               MOVE 1 TO WKS-FLAG-INVALID
           END-IF
           IF EDMLI-FECHA-NACAAI NOT NUMERO
               SET WKS-MSG-NOYEAR TO TRUE
               MOVE 1 TO WKS-FLAG-INVALID
           END-IF
           IF EDMLI-FECHA-NACAAO < 1900
           OR EDMLI-FECHA-NACAAO > FUNCTION CURRENT-DATE(1:4)
               SET WKS-MSG-NOYEAR TO TRUE
               MOVE 1 TO WKS-FLAG-INVALID
           END-IF

           IF WKS-FLAG-INVALID = 1
               MOVE DFHRED TO EDMLI-FECHA-NACAAC
               MOVE -1 TO EDMLI-FECHA-NACAAL
           ELSE
               MOVE EDMLI-FECHA-NACAAO TO WKS-FECHA-NACAA
           END-IF.

      *-->VALIDACION DEL CAMPO DE MES DE NACIMIENTO
       315-VALID-FIELD-4.
           IF EDMLI-FECHA-NACMMI < SPACES
               SET WKS-MSG-NOMOUTH TO TRUE
               MOVE 1 TO WKS-FLAG-INVALID
           END-IF
           IF EDMLI-FECHA-NACMMI NOT NUMERO
               SET WKS-MSG-NOMOUTH TO TRUE
               MOVE 1 TO WKS-FLAG-INVALID
           END-IF
           IF WKS-FLAG-INVALID = ZEROS
               INITIALIZE WKS-FECHA-NACMM
               INITIALIZE WKS-INDEX
               MOVE 3 TO WKS-AUX
               PERFORM VARYING WKS-INDEX FROM 2 BY -1
               UNTIL WKS-INDEX = ZEROS
                   IF EDMLI-FECHA-NACMMI(WKS-INDEX:1) NOT EQUAL ' '
                       SUBTRACT 1 FROM WKS-AUX
                       MOVE EDMLI-FECHA-NACMMI(WKS-INDEX:1)
                       TO WKS-FECHA-NACMM(WKS-AUX:1)
                   END-IF
               END-PERFORM
               MOVE WKS-FECHA-NACMM TO EDMLI-FECHA-NACMMO
           END-IF

           IF EDMLI-FECHA-NACAAO = FUNCTION CURRENT-DATE(1:4)
               IF EDMLI-FECHA-NACMMO < 1
               OR EDMLI-FECHA-NACMMO > FUNCTION CURRENT-DATE(5:2)
                   SET WKS-MSG-NOMOUTH TO TRUE
                   MOVE 1 TO WKS-FLAG-INVALID
               END-IF
           ELSE
               IF EDMLI-FECHA-NACMMO < 1
               OR EDMLI-FECHA-NACMMO > 12
                   SET WKS-MSG-NOMOUTH TO TRUE
                   MOVE 1 TO WKS-FLAG-INVALID
               END-IF
           END-IF

           IF WKS-FLAG-INVALID = 1
               MOVE DFHRED TO EDMLI-FECHA-NACMMC
               MOVE -1 TO EDMLI-FECHA-NACMML
           END-IF.

      *-->VALIDACION DEL CAMPO DE DIA DE NACIMIENTO
       316-VALID-FIELD-5.
           IF EDMLI-FECHA-NACDDI < SPACES
               SET WKS-MSG-NODAY TO TRUE
               MOVE 1 TO WKS-FLAG-INVALID
           END-IF
           IF EDMLI-FECHA-NACDDI NOT NUMERO
               SET WKS-MSG-NODAY TO TRUE
               MOVE 1 TO WKS-FLAG-INVALID
           END-IF
           IF WKS-FLAG-INVALID = ZEROS
               INITIALIZE WKS-FECHA-NACDD
               INITIALIZE WKS-INDEX
               MOVE 3 TO WKS-AUX
               PERFORM VARYING WKS-INDEX FROM 2 BY -1
               UNTIL WKS-INDEX = ZEROS
                   IF EDMLI-FECHA-NACDDI(WKS-INDEX:1) NOT EQUAL ' '
                       SUBTRACT 1 FROM WKS-AUX
                       MOVE EDMLI-FECHA-NACDDI(WKS-INDEX:1)
                       TO WKS-FECHA-NACDD(WKS-AUX:1)
                   END-IF
               END-PERFORM
               MOVE WKS-FECHA-NACDD TO EDMLI-FECHA-NACDDO
           END-IF

           IF  EDMLI-FECHA-NACMMO = FUNCTION CURRENT-DATE(5:2)
           AND EDMLI-FECHA-NACAAO = FUNCTION CURRENT-DATE(1:4)
               IF EDMLI-FECHA-NACDDO < 1
               OR EDMLI-FECHA-NACDDO > FUNCTION CURRENT-DATE(7:2)
                   SET WKS-MSG-NODAY TO TRUE
                   MOVE 1 TO WKS-FLAG-INVALID
               END-IF
           END-IF

           PERFORM 317-PROCESS-LEAP-YEAR
           IF  WKS-FLAG-LEAP-YEAR = 1
           AND EDMLI-FECHA-NACMMO = 2
               IF EDMLI-FECHA-NACDDO < 1
               OR EDMLI-FECHA-NACDDO > 29
                   SET WKS-MSG-NODAY TO TRUE
                   MOVE 1 TO WKS-FLAG-INVALID
               END-IF
           ELSE
               MOVE 1 TO WKS-INDEX
               PERFORM UNTIL WKS-INDEX > 12
                   IF EDMLI-FECHA-NACMMO = WKS-NO-MOUTH(WKS-INDEX)
                       MOVE WKS-NO-DAYS(WKS-INDEX) TO WKS-LIMIT-DAYS
                   END-IF
                   ADD 1 TO WKS-INDEX
               END-PERFORM
               IF EDMLI-FECHA-NACDDO < 1
               OR EDMLI-FECHA-NACDDO > WKS-LIMIT-DAYS
                   SET WKS-MSG-NODAY TO TRUE
                   MOVE 1 TO WKS-FLAG-INVALID
               END-IF
           END-IF

           IF WKS-FLAG-INVALID = 1
               MOVE DFHRED TO EDMLI-FECHA-NACDDC
               MOVE -1 TO EDMLI-FECHA-NACDDL
           END-IF.

       317-PROCESS-LEAP-YEAR.
           EVALUATE TRUE
           WHEN FUNCTION MOD (EDMLI-FECHA-NACAAO 4) NOT ZERO
           WHEN FUNCTION MOD (EDMLI-FECHA-NACAAO 100) ZERO
           AND FUNCTION MOD (EDMLI-FECHA-NACAAO 400) NOT ZERO
               MOVE 0 TO WKS-FLAG-LEAP-YEAR
           WHEN OTHER
               MOVE 1 TO WKS-FLAG-LEAP-YEAR
           END-EVALUATE.

      *-->VALIDACION DEL CAMPO DE TELEFONO
       318-VALID-FIELD-6.
           IF EDMLI-TELI < SPACES
               SET WKS-MSG-NOPHONE TO TRUE
               MOVE 1 TO WKS-FLAG-INVALID
           END-IF
           IF EDMLI-TELI NOT NUMERO
               SET WKS-MSG-NOPHONE TO TRUE
               MOVE 1 TO WKS-FLAG-INVALID
           END-IF
           IF EDMLI-TELO < 10000000
               SET WKS-MSG-NOPHONE TO TRUE
               MOVE 1 TO WKS-FLAG-INVALID
           END-IF

           IF WKS-FLAG-INVALID = 1
               MOVE DFHRED TO EDMLI-TELC
               MOVE -1 TO EDMLI-TELL
           END-IF.

       319-PROCESS-REG-DATA.
           MOVE EDMLI-CO-CLIENTEO  TO EDMC-LLAVE
           MOVE EDMLI-NOM-CLIENTEO TO EDMC-NOMBRE-CLIENTE
           MOVE WKS-FECHA-NAC      TO WKS-AUX-FECHA
           MOVE WKS-AUX-FECHA      TO EDMC-FECHA-NAC-O-CONSTITUC
           MOVE EDMLI-TELO         TO EDMC-NUMERO-TELEFONO
           MOVE FUNCTION CURRENT-DATE(1:4) TO WKS-FECHA-NACAA
           MOVE FUNCTION CURRENT-DATE(5:2) TO WKS-FECHA-NACMM
           MOVE FUNCTION CURRENT-DATE(7:2) TO WKS-FECHA-NACDD
           MOVE WKS-FECHA-NAC      TO WKS-AUX-FECHA
           MOVE WKS-AUX-FECHA      TO EDMC-FECHA-CREACION
           MOVE SPACES             TO EDMC-MARCA-ELIMINADO
           MOVE SPACES             TO EDMC-FILLER.

      *-->ACCION PARA REINICIAR MAPA
       400-ACCION-PF3.
           INITIALIZE WKS-COM-PROTECTED
           INITIALIZE WKS-COM-CO-CLIENTE
           PERFORM 802-EXEX-CICS-SEND-DATA
           PERFORM 801-EXEC-CICS-SEND-ERASE
           PERFORM 804-EXEC-CICS-RETURN.

      *-->ACCION PARA SALIR AL MENU PRINCIPAL
       500-ACCION-PF10.
           PERFORM 808-CICS-XCTL-PROGRAM-1
           PERFORM 999-END-PROGRAM.

      *-->ACCION QUE DESPLIEGA MENSAJE DE COMANDO NO HABILITADO
       600-ACCION-OTHER.
           SET WKS-MSG-NOCOMAND TO TRUE
           PERFORM 802-EXEX-CICS-SEND-DATA
           PERFORM 804-EXEC-CICS-RETURN.

      *--> COMANDOS CICS
       801-EXEC-CICS-SEND-ERASE.
           EXEC CICS SEND
                MAP('EDMLI')
                MAPSET('EDMLI')
                ERASE
           END-EXEC.

       802-EXEX-CICS-SEND-DATA.
           PERFORM 998-PROCESS-DEFAULT-DATA
           PERFORM 997-PROCESS-OUTPUT-MSG
           EXEC CICS SEND
                MAP('EDMLI')
                MAPSET('EDMLI')
                DATAONLY
           END-EXEC.

       803-EXEX-CICS-SEND-DATA-CURSOR.
           PERFORM 998-PROCESS-DEFAULT-DATA
           PERFORM 997-PROCESS-OUTPUT-MSG
           EXEC CICS SEND
                MAP('EDMLI')
                MAPSET('EDMLI')
                DATAONLY
                CURSOR
           END-EXEC.

       804-EXEC-CICS-RETURN.
           EXEC CICS RETURN
               TRANSID('EDMI')
               COMMAREA(WKS-COMMAREA)
           END-EXEC.

       804-EXEC-CICS-RECEIVE.
           EXEC CICS RECEIVE
               MAP ('EDMLI')
               MAPSET ('EDMLI')
           END-EXEC.

       805-EXEC-CICS-READ.
           EXEC CICS READ
               FILE('EDM4CL')
               INTO(REG-EDMACL)
               RIDFLD(EDMC-LLAVE)
               NOHANDLE
           END-EXEC
           PERFORM 899-EVALUATE-DFHRESP.

       806-EXEC-CICS-READ-UPDATE.
           EXEC CICS READ
               FILE('EDM4CL')
               INTO(REG-EDMACL)
               RIDFLD(EDMC-LLAVE)
               NOHANDLE
               UPDATE
           END-EXEC
           PERFORM 899-EVALUATE-DFHRESP.

       807-EXEC-CICS-REWRITE.
           EXEC CICS REWRITE
                FILE ('EDM4CL')
                FROM (REG-EDMACL)
                NOHANDLE
           END-EXEC
           PERFORM 899-EVALUATE-DFHRESP.

       808-CICS-XCTL-PROGRAM-1.
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
                TO EDMLI-OUTPUT-MSGO
           ELSE IF WKS-MSG-UNKERROR
                MOVE "ERROR DESCONOCIDO"
                TO EDMLI-OUTPUT-MSGO
           ELSE IF WKS-MSG-NOTOPEN
                MOVE "ARCHIVO CERRADO"
                TO EDMLI-OUTPUT-MSGO
           ELSE IF WKS-MSG-NONUMERIC
                MOVE "CODIGO DE CLIENTE INCORRECTO"
                TO EDMLI-OUTPUT-MSGO
           ELSE IF WKS-MSG-DUPLICATE
                MOVE "CODIGO EXISTENTE"
                TO EDMLI-OUTPUT-MSGO
           ELSE IF WKS-MSG-NOALPHA
                MOVE "NOMBRE INCORRECTO"
                TO EDMLI-OUTPUT-MSGO
           ELSE IF WKS-MSG-NOYEAR
                MOVE "ANIO INCORRECTO"
                TO EDMLI-OUTPUT-MSGO
           ELSE IF WKS-MSG-NOMOUTH
                MOVE "MES INCORRECTO"
                TO EDMLI-OUTPUT-MSGO
           ELSE IF WKS-MSG-NODAY
                MOVE "DIA INCORRECTO"
                TO EDMLI-OUTPUT-MSGO
           ELSE IF WKS-MSG-NOPHONE
                MOVE "TELEFONO INCORRECTO"
                TO EDMLI-OUTPUT-MSGO
           ELSE IF WKS-MSG-NOTFND
                MOVE "NO SE ENCONTRO"
                TO EDMLI-OUTPUT-MSGO
           ELSE IF WKS-MSG-MOD
                MOVE "MODIFICACION EXITOSA"
                TO EDMLI-OUTPUT-MSGO
           ELSE IF WKS-MSG-PROTECTED
                MOVE "COMANDO INACTIVO"
                TO EDMLI-OUTPUT-MSGO
           ELSE IF WKS-MSG-FOUND
                MOVE "REALIZE SUS MODIFICACIONES"
                TO EDMLI-OUTPUT-MSGO
           ELSE IF WKS-MSG-NOCHANGES
                MOVE "NO SE REALIZARON MODIFICACIONES"
                TO EDMLI-OUTPUT-MSGO
           ELSE
                MOVE DFHYELLO TO EDMLI-OUTPUT-MSGO
                MOVE SPACES   TO EDMLI-OUTPUT-MSGO
           END-IF
           MOVE ZEROS TO WKS-OUTPUT-MSG.

      *--> PROCESAR DATOS POR DEFECTO
       998-PROCESS-DEFAULT-DATA.
           MOVE FUNCTION CURRENT-DATE(1:4) TO WKS-DATE-YYYY
           MOVE FUNCTION CURRENT-DATE(5:2) TO WKS-DATE-MM
           MOVE FUNCTION CURRENT-DATE(7:2) TO WKS-DATE-DD
           MOVE WKS-DATE-SIS TO EDMLI-DATE-SISO
           MOVE FUNCTION CURRENT-DATE(9:2) TO WKS-TIME-HH
           MOVE FUNCTION CURRENT-DATE(11:2) TO WKS-TIME-MM
           MOVE FUNCTION CURRENT-DATE(13:2) TO WKS-TIME-SS
           MOVE WKS-TIME-SIS TO EDMLI-TIME-SISO.

      *--> TERMINACION DEL PROGRAMA
       999-END-PROGRAM.
           MOVE SPACE TO EDMLII
           EXEC CICS SEND
               TEXT
               FROM(WKS-END-MSG)
               ERASE
           END-EXEC

           EXEC CICS RETURN
           END-EXEC

           GOBACK.