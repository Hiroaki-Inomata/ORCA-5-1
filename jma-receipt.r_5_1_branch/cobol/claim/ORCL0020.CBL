      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCL0020.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : 
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  00/12/01    MCC-ƣ��      ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 03.05.00     NACL-¿��Ǽ  07/05/09  ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  LF02-FILE       ASSIGN  FILE-NAME
                                   FILE    STATUS  IS  STS-LF02.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       FD  LF02-FILE.
      *01  OUT-LF02-REC            PIC X(30000).
       01  LF02-REC.        
           COPY    "CPRLF002.INC".
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "COMMON-SPA".
      *
       01  FILE-NAME.
           03                      PIC X(14)  VALUE  "/var/tmp/claim". 
           03  FILE-HOSPNUM        PIC 9(02). 
           03  FILE-PTID           PIC 9(10). 
           03                      PIC X(06)  VALUE  ".txt".
      *
       01  WRK-DATA-REC.
           03  WRK-DATA            PIC X(30000)
                                   OCCURS  10.
      *
      *01  LF02-REC.        
      ***  COPY    "CPRLF002.INC".
      *     
018000*    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-LF02            PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-KBN     PIC  X(01).
               05  WRK-PARA-PTID    PIC  9(10).
               05  WRK-PARA-SRYKA   PIC  X(02).
               05  WRK-PARA-SRYYMD  PIC  X(08).
               05  WRK-PARA-HKNCOMBINUM
                                    PIC  X(04). 
               05  WRK-PARA-HOSPNUM PIC  9(02).
               05  WRK-PARA-DRCD    PIC  X(05).
               05  WRK-PARA-DENPNUM
                                    PIC  X(07).
               05  WRK-PARA-SYORIFLG
                                    PIC  X(01).
      *
           03  WRK-IN               PIC X(01).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
      *
       01  WRK-LF02-REC. 
           COPY    "CPRLF002.INC"    REPLACING  //LF02//
                                     BY         //WRK-LF02//.
      *
      *    �����ƥ��ΰ�
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    ���̥ѥ�᥿
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *PROCEDURE                   DIVISION.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
      *
      *    �����ƥ����ե��å�
           ACCEPT  SYS-YMD         FROM    DATE
           COMPUTE WRK-SYSYY       =   SYS-YY      +   2000
           MOVE    SYS-MM              TO  WRK-SYSMM
           MOVE    SYS-DD              TO  WRK-SYSDD
           MOVE    WRK-SYSYMD          TO  SPA-SYSYMD
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    MOVE    COMMAND-PARAM       TO  WRK-PARA
           UNSTRING COMMAND-PARAM      DELIMITED BY ","
                                       INTO WRK-PARA-KBN
                                            WRK-PARA-PTID
                                            WRK-PARA-SRYKA
                                            WRK-PARA-SRYYMD
                                            WRK-PARA-HKNCOMBINUM
                                            WRK-PARA-HOSPNUM
                                            WRK-PARA-DRCD
                                            WRK-PARA-DENPNUM
                                            WRK-PARA-SYORIFLG
           END-UNSTRING
      *
      *
      *???
           DISPLAY "L0020 DRCD= " WRK-PARA-DRCD "##"
      *???
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM 
           MOVE    WRK-PARA-SRYYMD     TO  SPA-SRYYMD
           MOVE    WRK-PARA-HKNCOMBINUM
                                       TO  SPA-HKNCOMBINUM 
           MOVE    "2"                 TO  SPA-NYUGAIKBN 
           DISPLAY "WRK-PARA-HKNCOMBINUM = " 
                                WRK-PARA-HKNCOMBINUM "##"
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           DISPLAY "WRK-PARA-DENPNUM = " WRK-PARA-DENPNUM
           CALL    "ORCL0021"          USING   WRK-PARA-KBN
                                               WRK-PARA-PTID
                                               WRK-PARA-SRYKA
                                               SPA-HOSPNUM
                                               SPA-SYSYMD
                                               SPA-SRYYMD
                                               SPA-NYUGAIKBN
                                               SPA-HKNCOMBINUM
                                               WRK-PARA-DRCD
                                               WRK-PARA-DENPNUM
                                               WRK-PARA-SYORIFLG
                                               WRK-LF02-REC
      *
           MOVE    WRK-LF02-REC        TO  LF02-REC
      *     
           MOVE    WRK-PARA-HOSPNUM    TO  FILE-HOSPNUM
           MOVE    WRK-PARA-PTID       TO  FILE-PTID
           OPEN    OUTPUT    LF02-FILE
      *     
           MOVE    WRK-LF02-REC        TO  LF02-REC
           WRITE                           LF02-REC
           IF      STS-LF02        NOT =   ZERO
               DISPLAY "*** ORCL0020  ERR="   STS-LF02
           END-IF
      **** MOVE    LF02-REC            TO  WRK-DATA-REC 
      *    PERFORM VARYING     IDX     FROM   1   BY  1
      *            UNTIL       IDX     >      7
      *
      *        MOVE    WRK-DATA (IDX)      TO  OUT-LF02-REC
      *        WRITE                       OUT-LF02-REC
      *        IF      STS-LF02        NOT =   ZERO
      *            DISPLAY "*** ORCL0020  ERR="   STS-LF02
      *            MOVE    10             TO  IDX
      *        END-IF
      *****END-PERFORM
           CLOSE   LF02-FILE
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��λ��������
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "*** ORCL0020 END ***"
      *    ACCEPT  WRK-IN
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *      
      *****************************************************************
      *    �ģ¡������ץ����
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡�������������
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *