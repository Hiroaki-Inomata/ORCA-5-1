      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCL0010.
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
      *  01.00.01    MCC-ƣ��     03.01.28  �ե�����̾�˿��Ųʤ��ɲ� 
      *  03.05.00    NACL-¿��Ǽ  07/05/09  ���롼�׿����б�
      *  04.03.00    NACL-¿��Ǽ  08/06/11  �����Ͽ�������б�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  LF01-FILE       ASSIGN  FILE-NAME
                                   FILE    STATUS  IS  STS-LF01.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       FD  LF01-FILE.
       01  LF01-REC. 
           COPY    "CPRLF001.INC".
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "COMMON-SPA".
      *
       01  FILE-NAME.
           03                      PIC X(14)  VALUE  "/var/tmp/claim". 
           03  FILE-HOSPNUM        PIC 9(02).   
           03  FILE-PTID           PIC 9(10).   
           03  FILE-SRYKA          PIC X(02).   
           03                      PIC X(04)  VALUE  ".txt".   
018000*    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-LF01            PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-KBN     PIC  X(01).
               05  WRK-PARA-PTID    PIC  9(10).
               05  WRK-PARA-SRYKA   PIC  X(02).
               05  WRK-PARA-TIME    PIC  X(06).
               05  WRK-PARA-HOSPNUM PIC  9(02).
               05  WRK-PARA-DRCD    PIC  X(05).
               05  WRK-PARA-HKNCOMBI
                                    PIC  X(04).
               05  WRK-PARA-SINRYONAIYO
                                    PIC  X(16).
               05  WRK-PARA-RENNUM
                                    PIC  X(02).
               05  WRK-PARA-SYSYMD
                                    PIC  X(08).
               05  WRK-PARA-MEMO
                                    PIC  X(02).
      *
           03  WRK-IN               PIC X(01).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
      *
       01  WRK-LF01-REC. 
           COPY    "CPRLF001.INC"    REPLACING  //LF01//
                                     BY         //WRK-LF01//.
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
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(100).
      *PROCEDURE                   DIVISION.
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
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
                                            WRK-PARA-TIME
                                            WRK-PARA-HOSPNUM
                                            WRK-PARA-DRCD
                                            WRK-PARA-HKNCOMBI
                                            WRK-PARA-SINRYONAIYO
                                            WRK-PARA-RENNUM
                                            WRK-PARA-SYSYMD
                                            WRK-PARA-MEMO
           END-UNSTRING
      *
      *???
           DISPLAY "PARA " WRK-PARA "##"
      *???
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           IF      WRK-PARA-SYSYMD     NOT =   SPACE
               MOVE    WRK-PARA-SYSYMD     TO  SPA-SYSYMD
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *???
           DISPLAY "HOSPNUM " SPA-HOSPNUM "##"
      *???
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE    WRK-PARA-SRYKA      TO  SPA-SRYKA
           MOVE    WRK-PARA-PTID       TO  SPA-PTID
           MOVE    WRK-PARA-DRCD       TO  SPA-DRCD
           MOVE    WRK-PARA-HKNCOMBI   TO  SPA-HKNCOMBINUM
      *
      ***  CALL    "ORCL0011"          USING   WRK-PARA-PTID
      *                                        WRK-PARA-SRYKA
      *                                        WRK-PARA-TIME
      *                                        SPA-HOSPID
      *                                        SPA-SYSYMD
      *                                        WRK-PARA-DRCD
      *                                        WRK-PARA-HKNCOMBI
      *                                        WRK-PARA-SINRYONAIYO 
      ***                                      WRK-LF01-REC
           CALL    "ORCL0011"          USING   WRK-PARA-TIME
                                               WRK-PARA-SINRYONAIYO 
                                               WRK-PARA-RENNUM
                                               WRK-PARA-MEMO
                                               SPA-AREA
                                               WRK-LF01-REC
      *
           MOVE    WRK-PARA-HOSPNUM    TO  FILE-HOSPNUM
           MOVE    WRK-PARA-PTID       TO  FILE-PTID
      *-->  01.00.01  ADD START
           MOVE    WRK-PARA-SRYKA      TO  FILE-SRYKA
      *-->  01.00.01  ADD END
           OPEN    OUTPUT    LF01-FILE
           MOVE    WRK-LF01-REC        TO  LF01-REC
           WRITE   LF01-REC
           CLOSE   LF01-FILE
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
           DISPLAY "*** ORCL0010 END ***"
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
       900-DBCLOSE-EXT.
           EXIT.
      *