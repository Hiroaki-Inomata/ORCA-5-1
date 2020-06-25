      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGXG02.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���ףãӣֽ���
      *  ����ݡ��ͥ��̾  : ���ϻؼ�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  09/09/30    NACL-ƣ��     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  04.08.03    NACL-ƣ��    14/08/28  �������ߥɥ��б�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
      *
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    �쥻�ťǡ�������������å��ѥ���ե�����
           SELECT  WRK-FILE        ASSIGN  WRKPARA
                                   FILE    STATUS  IS  STS-WRK.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    �쥻�ťǡ�������������å��ѥ���ե�����
       FD  WRK-FILE.
       01  WRK-R                   PIC X(10).
      *
       WORKING-STORAGE             SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    ���̥ѥ�᥿
           COPY    "XG01COMMON-SPA".
      *
      *    �����ѣӣУ�
           COPY    "XG01SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *
       01  STS-AREA.
           03  STS-WRK             PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX3                PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-MCP-WIDGET      PIC X(64).
      *
           03  WRK-FILENO          PIC 9(03).
      *
           03  WRK-TO-FILENAME.
               05  WRK-TO-FOLDER   PIC X(200).
               05  WRK-TO-DATA1    PIC X(200).
               05  WRK-TO-FILENO   PIC X(03).
               05  WRK-TO-DATA2    PIC X(04).
      *
           03  WRK-COL             PIC 9(03).
           03  WRK-XGIDMSG         PIC X(70).
      *
       01  WRKPARA                 PIC X(100).
      *
       01  WRK-CONS-AREA.
      *    ����ִ����ģ¤Υ�����  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "PUTCSV".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   1.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    ���饤�������¸���ݾ����������
           COPY    "CPORCSCLIENTSV.INC".
      *
      *    �ե��������ѥ�᥿
           COPY    "CPORCSFDEL.INC".
      *
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    MCPAREA.
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA07SCRAREA.INC".
      *
      ******************************************************************
       PROCEDURE                   DIVISION    USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-XG01FREE
           MOVE    SPA-WORK        TO  SPA-XG01KYOUTU 
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
               WHEN    "LINK"      ALSO    ANY
                   PERFORM 100-INIT-SEC
      *        ��������
               WHEN     "PUTG"     ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *        ����
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    �������ܤ��ʤ���������ɽ����
           IF      FLG-END         =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-XG01KYOUTU  TO  SPA-WORK  
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-XG01FREE    TO  SPA-FREE
      *
           EXIT    PROGRAM.
           .
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    ���顼���̤��������
           IF      SPA-MOTOPG      =   "XGERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        ������̽���
               PERFORM 300-SCREEN-SEC
               IF      FLG-END         =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        �����Խ�����
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE           TO  MCP-PUTTYPE
           MOVE    "XG02"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "XGID1"
                   PERFORM 330-XGID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
      *    ���ѽ���Խ�����
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ѽ���Խ�����
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-XG02-AREA
      *
      *    ���饤�������¸���ݾ����������
           INITIALIZE                      ORCSCLIENTSVAREA
           CALL    "ORCSCLIENTSV"  USING   ORCSCLIENTSVAREA
                                           SPA-AREA
           MOVE    SCLIENTSV-CLIENT-SAVE
                                   TO  SPA-XG02-CLIENT-SAVE
           IF      SPA-XG02-CLIENT-SAVE
                                   =   "1"
               MOVE    "1"             TO  SPA-GMN-XG02-FILEL   (1)
               MOVE    "���饤�������¸"
                                       TO  SPA-GMN-XG02-FILEMEIL(1)
               MOVE    1               TO  SPA-GMN-XG02-FILE-MAX
       *
               IF      SPA-MW              =   SPA-GINBEE
                   MOVE    SPA-GMN-XG02-FILELST (1)
                                           TO  SPA-GMN-XG02-FILE-G
               END-IF 
           END-IF
           IF      SPA-NAI-XG04-TBL-KEY
                                       =   "RENKEI1"   OR
                                           "MISRECE1"  OR
                                           "MISJMAMR"
               MOVE    "2"             TO  SPA-GMN-XG02-CDKBNL   (1)
               MOVE    "�ӡݣ���"    TO  SPA-GMN-XG02-CDKBNMEIL(1)
               MOVE    1               TO  SPA-GMN-XG02-CDKBN-MAX
           ELSE
           MOVE    "1"             TO  SPA-GMN-XG02-CDKBNL   (1)
           MOVE    "�ţգ�"        TO  SPA-GMN-XG02-CDKBNMEIL(1)
           MOVE    "2"             TO  SPA-GMN-XG02-CDKBNL   (2)
           MOVE    "�ӡݣ���"    TO  SPA-GMN-XG02-CDKBNMEIL(2)
           MOVE    "3"             TO  SPA-GMN-XG02-CDKBNL   (3)
           MOVE    "�գԣơݣ�"    TO  SPA-GMN-XG02-CDKBNMEIL(3)
           MOVE    3               TO  SPA-GMN-XG02-CDKBN-MAX
           END-IF
           MOVE    SPA-GMN-XG02-CDKBNLST (1)
                                   TO  SPA-GMN-XG02-CDKBN-G
      *
           INITIALIZE              WRK-TO-FILENAME
           IF      SPA-NAI-TTO-FOLDER (SPA-NAI-SELNUM)
                                   =   SPACE
               MOVE    "/tmp/"         TO  WRK-TO-FOLDER
           ELSE
               PERFORM VARYING IDX2    FROM    200 BY  -1
                       UNTIL   IDX2    <       2
                   IF      SPA-NAI-TTO-FOLDER (SPA-NAI-SELNUM) (IDX2:1)
                                           =   "/"
                       COMPUTE IDX3    =   IDX2    -   1
                       MOVE    SPA-NAI-TTO-FOLDER
                                           (SPA-NAI-SELNUM) (1:IDX3)
                                               TO  WRK-TO-FOLDER
                       MOVE    1               TO  IDX2
                   END-IF    
               END-PERFORM
               IF      WRK-TO-FOLDER   =   SPACE
                   MOVE    SPA-NAI-TTO-FOLDER (SPA-NAI-SELNUM)
                                           TO  WRK-TO-FOLDER
               END-IF
           END-IF
      *
           IF      SPA-NAI-TTO-DATA  (SPA-NAI-SELNUM)
                                   =   SPACE
               EVALUATE    SPA-XG01-MOTOPG
                   WHEN    "L01"
                       STRING  SPA-HOSPNUM     DELIMITED   BY  SIZE 
                               "DAILY_"        DELIMITED   BY  SIZE
                                               INTO  WRK-TO-DATA1
                       END-STRING
                   WHEN    "G01"
                       STRING  SPA-HOSPNUM     DELIMITED   BY  SIZE 
                               "MONTHLY_"      DELIMITED   BY  SIZE
                                               INTO  WRK-TO-DATA1
                       END-STRING
               END-EVALUATE
               MOVE    ".CSV"          TO  WRK-TO-DATA2
               MOVE    SPA-NAI-FILENO  TO  WRK-FILENO
               ADD     1               TO  WRK-FILENO
               MOVE    WRK-FILENO      TO  WRK-TO-FILENO
           ELSE
               MOVE    SPA-NAI-TTO-DATA  (SPA-NAI-SELNUM)
                                       TO  WRK-TO-DATA1
           END-IF
      *
           IF      SPA-MW          =   SPA-GINBEE
               STRING   WRK-TO-DATA1       DELIMITED   BY  SPACE
                        WRK-TO-FILENO      DELIMITED   BY  SPACE
                        WRK-TO-DATA2       DELIMITED   BY  SPACE
                                           INTO    SPA-NAI-XG02-FILENAME
               END-STRING
           ELSE
               STRING   WRK-TO-FOLDER      DELIMITED   BY  SPACE
                        "/"                DELIMITED   BY  SIZE
                        WRK-TO-DATA1       DELIMITED   BY  SPACE
                        WRK-TO-FILENO      DELIMITED   BY  SPACE
                        WRK-TO-DATA2       DELIMITED   BY  SPACE
                                           INTO    SPA-NAI-XG02-FILENAME
               END-STRING
           END-IF
           MOVE    SPA-NAI-XG02-FILENAME
                                   TO  SPA-GMN-XG02-FILENAME
      *
           PERFORM VARYING IDX2    FROM   1   BY  1
                   UNTIL   IDX2    >      SPA-GMN-XG02-CDKBN-MAX
               IF      SPA-NAI-TCODE-TYPE (SPA-NAI-SELNUM)
                                   =   SPA-GMN-XG02-CDKBNL (IDX2)
                   MOVE    SPA-GMN-XG02-CDKBNLST (IDX2)
                                           TO  SPA-GMN-XG02-CDKBN-G
                   MOVE    SPA-GMN-XG02-CDKBN-MAX
                                           TO  IDX2
               END-IF
           END-PERFORM        
      *
           MOVE  1                 TO  SPA-GMN-XG02-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ǧ����������
      *****************************************************************
       330-XGID1-SET-SEC            SECTION.
      *
           IF      SPA-XGID1-FLG    =   "OK"
      *        �������
               PERFORM 490-KAKUTEI-SEC
               GO  TO  330-XGID1-SET-EXT
           END-IF
      *
           MOVE    SPACE           TO  SPA-XGID1-FLG
           MOVE    SPACE           TO  SPA-XGIDCD
           .
       330-XGID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *        ���
               WHEN    "CLICKED"   ALSO    "B01"
                   MOVE    SPACE           TO  SPA-XGID1-FLG
                   PERFORM 210-BACK
      *        ����
               WHEN    "CLICKED"   ALSO    "B12"
                   PERFORM 490-KAKUTEI-CHECK-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    ���ý���
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *        ���ʬ
               WHEN    "ACTIVATE"       ALSO    ANY
                   PERFORM 400-ENTRY-SYORI-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ʬ���Ͻ���
      *****************************************************************
       400-ENTRY-SYORI-SEC             SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-XG02-CUR
      *
      *    ���ϥ����å�����
           MOVE    ZERO                TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           .
       400-ENTRY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϥ����å�����
      *****************************************************************
       410-INPUT-CHK-SEC           SECTION.
      *
      *    ���̤�ӣУ��˥��å�
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    ���ܥ����å�����
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           IF      FLG-TOUROKU     =   1
      *        ������
               PERFORM 4103-KAKUTEI-CHK-SEC
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̣ӣУ��Խ�����
      *****************************************************************
       4101-GMN-SPA-SET-SEC        SECTION.
      *
           MOVE    XG02-FILENAME   TO  SPA-GMN-XG02-FILENAME
           MOVE    XG02-FILE       TO  SPA-GMN-XG02-FILE-G
           MOVE    XG02-CDKBN      TO  SPA-GMN-XG02-CDKBN-G
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ܥ����å�����
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
      *    �ե�����̾�����å�
           PERFORM 41021-FILENAME-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    ����������å�
           PERFORM 41022-FILE-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    ʸ�������ɥ����å�
           PERFORM 41023-CDKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ե�����̾�����å�
      *****************************************************************
       41021-FILENAME-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-XG02-FILENAME
                                   =   SPACE
               MOVE    "1001"          TO  SPA-ERRCD
           ELSE
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       100
                       OR      SPA-GMN-XG02-FILENAME (IDX2:1)
                                           =   SPACE
                   IF        SPA-GMN-XG02-FILENAME (IDX2:1)
                                               =   "\" OR  ":" 
                       MOVE    "1004"              TO  SPA-ERRCD
                       MOVE    1                   TO  SPA-GMN-XG02-CUR
                       MOVE    100                 TO  IDX2
                   END-IF
                   IF      SPA-MW          =   SPA-GINBEE
                       IF        SPA-GMN-XG02-FILENAME (IDX2:1)
                                               =   "/"
                           MOVE    "1006"          TO  SPA-ERRCD
                           MOVE    1               TO  SPA-GMN-XG02-CUR
                           MOVE    100             TO  IDX2
                       END-IF
                   END-IF
               END-PERFORM        
           END-IF
           .
       41021-FILENAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����������å�����
      *****************************************************************
       41022-FILE-CHK-SEC          SECTION.
      *
           IF      SPA-MW          =   SPA-GINBEE
               IF      SPA-GMN-XG02-FILE-MAX
                                       =   ZERO
                   MOVE    "1005"          TO  SPA-ERRCD
                   MOVE    2               TO  SPA-GMN-XG02-CUR
                   GO  TO  41022-FILE-CHK-EXT
               END-IF
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >       SPA-GMN-XG02-FILE-MAX
               IF        SPA-GMN-XG02-FILE
                                       =   SPA-GMN-XG02-FILEL (IDX2)
                   MOVE    SPA-GMN-XG02-FILELST (IDX2)
                                           TO  SPA-GMN-XG02-FILE-G
                   MOVE    SPA-GMN-XG02-FILE-MAX
                                           TO  IDX2
               END-IF
           END-PERFORM
      *
           IF      SPA-GMN-XG02-FILEMEI
                                   =   SPACE  
               IF      SPA-MW          =   SPA-GINBEE
                   MOVE    SPA-GMN-XG02-FILELST (1)
                                           TO  SPA-GMN-XG02-FILE-G
               ELSE
                   MOVE    SPACE           TO  SPA-GMN-XG02-FILE-G
               END-IF
           END-IF
           .
       41022-FILE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ʸ�������ɥ����å�����
      *****************************************************************
       41023-CDKBN-CHK-SEC         SECTION.
      *
           PERFORM VARYING IDX2    FROM   1   BY  1
                   UNTIL   IDX2    >      SPA-GMN-XG02-CDKBN-MAX
               IF      SPA-GMN-XG02-CDKBN
                                       =   SPA-GMN-XG02-CDKBNL (IDX2)
                   MOVE    SPA-GMN-XG02-CDKBNLST (IDX2)
                                           TO  SPA-GMN-XG02-CDKBN-G
                   MOVE    SPA-GMN-XG02-CDKBN-MAX
                                           TO  IDX2
               END-IF
           END-PERFORM        
           IF      SPA-GMN-XG02-CDKBNMEI
                                   =   SPACE  
               MOVE    SPA-GMN-XG02-CDKBNLST (1)
                                       TO  SPA-GMN-XG02-CDKBN-G
           END-IF
           .
       41023-CDKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ܥ��󲡲��������å�����
      *****************************************************************
       4103-KAKUTEI-CHK-SEC        SECTION.
      *
      *    �ե�����̾�����å�
           MOVE    SPACE           TO  SPA-NAI-XG02-TO-FOLDER
                                       SPA-NAI-XG02-TO-DATA
      *
           MOVE    ZERO            TO  WRK-COL
           INSPECT SPA-GMN-XG02-FILENAME
                                   TALLYING    WRK-COL
                                   FOR ALL     "/"
            IF     SPA-GMN-XG02-FILE
                                   =   SPACE
                IF      WRK-COL        =   ZERO
                   MOVE    "1003"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-GMN-XG02-CUR
                   GO  TO  4103-KAKUTEI-CHK-EXT
                END-IF
           END-IF
      *
           PERFORM VARYING IDX2    FROM    100 BY  -1
                   UNTIL   IDX2    <       2
               IF      SPA-GMN-XG02-FILENAME (IDX2:1)
                                       =   "/"
                   COMPUTE IDX3    =   IDX2    -   1
                   MOVE    SPA-GMN-XG02-FILENAME (1:IDX3)
                                           TO  SPA-NAI-XG02-TO-FOLDER
                   COMPUTE IDX3    =   IDX2    +   1
                   MOVE    SPA-GMN-XG02-FILENAME (IDX3:)
                                           TO  SPA-NAI-XG02-TO-DATA
                   MOVE    1               TO  IDX2
               END-IF    
           END-PERFORM
      *
      *    ��Ǽ�����꤬�ʤ��Ȥ��ե�����̾�˥��å�
           IF    ( SPA-NAI-XG02-TO-DATA
                                   =   SPACE )
           AND   ( SPA-NAI-XG02-TO-FOLDER
                                   =   SPACE )
               MOVE    SPA-GMN-XG02-FILENAME
                                       TO  SPA-NAI-XG02-TO-DATA
           END-IF
      *
      *    �ե�����̾��̤����
           IF    ( SPA-NAI-XG02-TO-DATA
                                   =   SPACE )
               MOVE    "1002"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-XG02-CUR 
           ELSE
      *        ���饤�������¸�ʳ��ΤȤ��ե����̾��̤����
               IF    ( SPA-GMN-XG02-FILE
                                       =   SPACE )
               AND   ( SPA-NAI-XG02-TO-FOLDER
                                       =   SPACE )
                   MOVE    "1003"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-GMN-XG02-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
           AND   ( SPA-GMN-XG02-FILE
                                   =   SPACE )
               MOVE    SPA-GMN-XG02-FILENAME
                                       TO  WRKPARA
               OPEN    INPUT               WRK-FILE
               IF      STS-WRK             =   "00"
                   CLOSE   WRK-FILE
               ELSE
                   OPEN    OUTPUT              WRK-FILE
                   IF      STS-WRK             =   "00"
                       CLOSE   WRK-FILE
                       MOVE    ZERO                TO  IF-RESULT
                       MOVE    WRKPARA             TO  IF-FILENAME
                       CALL    "orcfiledel"    USING   ORCSFDELAREA
                   ELSE
                       MOVE    1                   TO  SPA-GMN-CUR
                       MOVE    "1001"              TO  SPA-XGIDCD
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD      NOT =   SPACE
               MOVE    SPACE           TO  SPA-NAI-XG02-TO-FOLDER
                                           SPA-NAI-XG02-TO-DATA
           END-IF    
           .
       4103-KAKUTEI-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ������
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPA-MOTOPG      TO  SPA-SAKIPG
           MOVE    "XG02"          TO  SPA-MOTOPG
      *
           MOVE    "JOIN"          TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����������å�����
      *****************************************************************
       490-KAKUTEI-CHECK-SEC       SECTION.
      *
      *    ���ϻ������å�
           MOVE    1               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  490-KAKUTEI-CHECK-EXT
           END-IF
           IF    ( SPA-XGIDCD      NOT =   SPACE )
               GO  TO  490-KAKUTEI-CHECK-EXT
           END-IF
      *
      *    �������
           PERFORM 490-KAKUTEI-SEC
           .
       490-KAKUTEI-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������
      *****************************************************************
       490-KAKUTEI-SEC             SECTION.
      *
           ADD     1               TO  SPA-NAI-FILENO
           MOVE    "OK"            TO  SPA-XGID1-FLG
      *
           PERFORM 210-BACK
      *
           .
       490-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������Խ�����
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD   NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-XGIDCD   NOT =   SPACE
               PERFORM 520-XGIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"       TO  MCP-PUTTYPE
           MOVE    "XG02"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE                  XG02
      *
           MOVE    SPA-GMN-XG02-FILENAME
                                   TO  XG02-FILENAME
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >       SPA-GMN-XG02-FILE-MAX
               MOVE    SPA-GMN-XG02-FILELST (IDX2)
                                           TO  XG02-FILE-ITEM (IDX2)
           END-PERFORM
           MOVE    SPA-GMN-XG02-FILE-MAX   TO  XG02-FILE-COUNT
           MOVE    SPA-GMN-XG02-FILE-G     TO  XG02-FILE
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >       SPA-GMN-XG02-CDKBN-MAX
               MOVE    SPA-GMN-XG02-CDKBNLST (IDX2)
                                           TO  XG02-CDKBN-ITEM (IDX2)
           END-PERFORM
           MOVE    SPA-GMN-XG02-CDKBN-MAX  TO  XG02-CDKBN-COUNT
           MOVE    SPA-GMN-XG02-CDKBN-G    TO  XG02-CDKBN
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̥������륻�åȽ���
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE ) AND
                 ( SPA-GMN-XG02-CUR    =   ZERO  )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-XG02-CUR
               WHEN    1
                   MOVE    "FILENAME"    TO  MCP-WIDGET
               WHEN    2
                   MOVE    "FILE"        TO  MCP-WIDGET
               WHEN    3
                   MOVE    "CDKBN"       TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B12"         TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϸĽ�Υ��åȽ���
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "FILENAME"
                   IF      SPA-XG02-CLIENT-SAVE
                                           =   "1"
                       MOVE    2           TO  SPA-GMN-XG02-CUR
                   ELSE
                       MOVE    3           TO  SPA-GMN-XG02-CUR
                   END-IF
               WHEN   "FILE"
                   MOVE    3           TO  SPA-GMN-XG02-CUR
               WHEN   "CDKBN"
                   MOVE    99          TO  SPA-GMN-XG02-CUR
           END-EVALUATE
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    ���顼��å�����ɽ������
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "1001"
               WHEN    "1002"
                   MOVE    "���ϥե�����̾�����Ϥ���Ƥ��ޤ���"
                                                   TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "��Ǽ��꤬���Ϥ���Ƥ��ޤ���"
                                                   TO  SPA-ERRMSG
               WHEN    "1004"
                   STRING   "���ϥե�����̾��"     DELIMITED   BY  SIZE
                            "�ɡ��ɡ��ɡ�ɤ����Ϥ��Ƥ���ޤ�"
                                                   DELIMITED   BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1005"
                   STRING   "�����ƥ�����Υ��饤�������¸"
                                                   DELIMITED   BY  SIZE
                            "�λ����ͭ���ˤ��Ʋ�����"
                                                   DELIMITED   BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1006"
                   STRING   "���ϥե�����̾�ˡɡ���" DELIMITED  BY  SIZE
                            "���ɡ��ɡ��ɡ�ɤ����Ϥ��Ƥ���ޤ�"
                                                     DELIMITED  BY  SIZE
                                                     INTO  SPA-ERRMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE           TO  XGERR
           MOVE    SPA-ERRCD       TO  XGERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  XGERR-ERRMSG
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "XG02"          TO  SPA-MOTOPG
           MOVE    "XGERR"         TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ǧ��å�����ɽ������
      *****************************************************************
       520-XGIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-XGIDMSG
      *
           EVALUATE    SPA-XGIDCD
               WHEN    "1001"
                   STRING  "�����褬�ޥ���ȤǤ��ޤ��󤬡�"
                                           DELIMITED  BY  SIZE
                           "������³���ޤ���"
                                           DELIMITED  BY  SIZE
                                           INTO  WRK-XGIDMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE           TO  XGID1
           INITIALIZE                  XGID1
           MOVE    SPA-XGIDCD      TO  XGID1-ID1CODE
           MOVE    WRK-XGIDMSG     TO  XGID1-ID1MSG
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "XG02"          TO  SPA-MOTOPG
           MOVE    "XGID1"         TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "XGID1"         TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END              
      *
           .
       520-XGIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �УգԽ���
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE    "PUTWINDOW"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *