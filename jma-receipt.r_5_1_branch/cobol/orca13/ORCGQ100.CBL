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
       PROGRAM-ID.                 ORCGQ100.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���ԾȲ�
      *  ����ݡ��ͥ��̾  : ���ϻؼ����̡ʣѣ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  00/06/11    MCC������     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-����    03/06/10  ��������б�
      *  03.05.00    NACL-����    07.05.23  ���롼�׿����б�
      *  04.01.01    NACL-ƣ��    07/10/01  ���饤�������¸�б�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    �����ѣӣУ�
           COPY    "Q01SCR-SPA".
      *
      *    ���󥹥������
           COPY    "ENUM-VALUE".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-NUM                             PIC 9(09).
           03  WRK-MCP-WIDGET                      PIC X(64).
      *
      *    �������ΰ�
      *01  CONST-AREA.
      *
           03  CONST-JOBID         PIC 9(07)   VALUE   1.
           03  CONST-JOBID2        PIC 9(07)   VALUE   2.
           03  CONST-JOB-SHELLID   PIC X(08)   VALUE   "ORCBQ01".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA13SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *----(01.00.02) LINE ADD START ----------------------------------
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
                                       WRK-AREA
      *----(01.00.02) LINE ADD END   ----------------------------------
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-Q01FREE
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    ��������
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    ����
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    �������ܤ��ʤ���������ɽ����
           IF  ( FLG-END           =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-Q01FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
                                       WRK-AREA
      *
      *    ���顼���̤��
           IF  ( SPA-MOTOPG            =   "QERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        ������̥��å�
               PERFORM 300-SCREEN-SEC
      *        �����Խ�
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "Q100"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-Q100
           INITIALIZE                      SPA-Q100
      *
           MOVE  4                     TO  SPA-GMN-Q100-CUR
      *
           IF  ( SPA-Q02-Q100-PROC-FLG     =   ZERO )
      *        �ãӣֽ��Ͻ���
      *
      *        ���饤�������¸���ݾ����������
               IF      SPA-Q01-CLIENT-SAVE
                                       =   "1"
                   MOVE    "1"             TO  SPA-GMN-Q100-FILEL   (1)
                   MOVE    "���饤�������¸"
                                           TO  SPA-GMN-Q100-FILEMEIL(1)
                   MOVE    1               TO  SPA-GMN-Q100-FILE-MAX
                   MOVE    WIDGET-NORMAL   TO  SPA-GMN-Q100-FILE-STATE
      *
                   MOVE    "1"             TO  SPA-GMN-Q100-CDKBNL   (1)
                   MOVE    "�ţգ�"        TO  SPA-GMN-Q100-CDKBNMEIL(1)
                   MOVE    "2"             TO  SPA-GMN-Q100-CDKBNL   (2)
                   MOVE    "�ӡݣ���"    TO  SPA-GMN-Q100-CDKBNMEIL(2)
                   MOVE    "3"             TO  SPA-GMN-Q100-CDKBNL   (3)
                   MOVE    "�գԣơݣ�"    TO  SPA-GMN-Q100-CDKBNMEIL(3)
                   MOVE    3               TO  SPA-GMN-Q100-CDKBN-MAX
                   MOVE    WIDGET-NORMAL   TO  SPA-GMN-Q100-CDKBN-STATE
      *
                   IF    ( SPA-MW          =   SPA-GINBEE )
                       MOVE    SPA-GMN-Q100-FILELST (1)
                                           TO  SPA-GMN-Q100-FILE-G
                       MOVE    SPA-GMN-Q100-CDKBNLST (1)
                                           TO  SPA-GMN-Q100-CDKBN-G
                   END-IF
      *
               ELSE
                   MOVE    WIDGET-INSENSITIVE
                                           TO  SPA-GMN-Q100-FILE-STATE
                                               SPA-GMN-Q100-CDKBN-STATE
               END-IF
      *
               MOVE    "3001"          TO  SPA-GMN-Q100-CD
               MOVE    "�ãӣֽ��Ϥ򳫻Ϥ��ޤ�"
                                       TO  SPA-GMN-Q100-MSG
               MOVE    "���ϥե�����̾"
                                       TO  SPA-GMN-Q100-LBLFILENAME
               IF    ( SPA-MW          =   SPA-GINBEE )
                   MOVE    "OUTPUT.csv"
                                       TO  SPA-GMN-Q100-FILENAME
               ELSE
                   MOVE    "/tmp/OUTPUT.csv"
                                       TO  SPA-GMN-Q100-FILENAME
               END-IF
               MOVE    WIDGET-NORMAL   TO  SPA-GMN-Q100-FILENAME-STATE
               MOVE    "�������"      TO  SPA-GMN-Q100-LBLNUMSEL
           ELSE
               MOVE    "3002"          TO  SPA-GMN-Q100-CD
               MOVE    "���������򳫻Ϥ��ޤ�"
                                       TO  SPA-GMN-Q100-MSG
               MOVE    SPACE           TO  SPA-GMN-Q100-LBLFILENAME
                                           SPA-GMN-Q100-FILENAME
               MOVE    WIDGET-INSENSITIVE
                                       TO  SPA-GMN-Q100-FILENAME-STATE
               MOVE    "�ڡ�������"    TO  SPA-GMN-Q100-LBLNUMSEL
               MOVE    WIDGET-INSENSITIVE
                                       TO  SPA-GMN-Q100-FILE-STATE
                                           SPA-GMN-Q100-CDKBN-STATE
           END-IF
      *
           MOVE    SPA-Q02-Q100-STNUM      TO  SPA-GMN-Q100-STNUM
           MOVE    SPA-Q02-Q100-EDNUM      TO  SPA-GMN-Q100-EDNUM
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           MOVE    MCP-WIDGET          TO        WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *     ���
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *     ����
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-KAKUTEI
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    ���á�����
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           MOVE    MCP-WIDGET          TO        WRK-MCP-WIDGET
      *
      *    ���ϸĽ�Υ��å�
           PERFORM 400-GMN-INPUT-SEC
      *
      *    ���ϥ����å�����
           PERFORM 410-INPUT-CHK-SEC
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϸĽ�Υ��åȽ���
      *****************************************************************
       400-GMN-INPUT-SEC           SECTION.
      *
           MOVE    ZERO        TO      SPA-GMN-Q100-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *****************************************************************
      *    ���ϥ����å�����
      *****************************************************************
       410-INPUT-CHK-SEC           SECTION.
      *
      *    ���̤�ӣУ��˥��å�
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    �ե�����̾�����å�
           PERFORM 4100-FILENAME-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
      *    ���Ϸ���ʥڡ������˥����å�
           PERFORM 4100-STNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
      *    ��λ����ʥڡ������˥����å�
           PERFORM 4100-EDNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
      *    ����������å�
           PERFORM 4100-FILE-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
      *    ʸ�������ɥ����å�
           PERFORM 4100-CDKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ���̣ӣУ��Խ�����
      *****************************************************************
       4101-GMN-SPA-SET-SEC        SECTION.
      *
      *    �ե�����̾
           MOVE    Q100-FILENAME   TO  SPA-GMN-Q100-FILENAME
      *
      *    ���Ϸ���ʥڡ�������
           MOVE    Q100-STNUM      TO  SPA-GMN-Q100-STNUM
      *
      *    ��λ����ʥڡ�������
           MOVE    Q100-EDNUM      TO  SPA-GMN-Q100-EDNUM
      *
      *    ������
           MOVE    Q100-FILE       TO  SPA-GMN-Q100-FILE-G
      *
      *    ʸ��������
           MOVE    Q100-CDKBN      TO  SPA-GMN-Q100-CDKBN-G
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    �ե�����̾�����å�
      *****************************************************************
       4100-FILENAME-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-Q100-FILENAME   =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       100
                       OR      SPA-GMN-Q100-FILENAME (IDX2:1)
                                           =   SPACE
      *
                   IF    ( SPA-MW      =   SPA-GINBEE )
                       IF        SPA-GMN-Q100-FILENAME (IDX2:1)
                                               =   "\" OR  ":"  OR "/"
                           MOVE    "1006"          TO  SPA-ERRCD
                           MOVE    1               TO  SPA-GMN-Q100-CUR
                           MOVE    100             TO  IDX2
                       END-IF
                   ELSE
                       IF        SPA-GMN-Q100-FILENAME (IDX2:1)
                                               =   "\" OR  ":" 
                           MOVE    "1004"          TO  SPA-ERRCD
                           MOVE    1               TO  SPA-GMN-Q100-CUR
                           MOVE    100             TO  IDX2
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .
       4100-FILENAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ���Ϸ���ʥڡ������˥����å�
      *****************************************************************
       4100-STNUM-CHK-SEC              SECTION.
      *
           MOVE    SPA-GMN-Q100-STNUM
                                   TO  WRK-NUM
      *
      *    ����ʥڡ��������ϰϥ����å�
           PERFORM 4100-NUM-HANI-CHK-SEC
      *
           MOVE    WRK-NUM         TO  SPA-GMN-Q100-STNUM
      *
           .
       4100-STNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����ʥڡ������˥����å�
      *****************************************************************
       4100-EDNUM-CHK-SEC              SECTION.
      *
           MOVE    SPA-GMN-Q100-EDNUM
                                   TO  WRK-NUM
      *
      *    ����ʥڡ��������ϰϥ����å�
           PERFORM 4100-NUM-HANI-CHK-SEC
      *
           MOVE    WRK-NUM         TO  SPA-GMN-Q100-EDNUM
      *
           .
       4100-STNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ����ʥڡ��������ϰϥ����å�
      *****************************************************************
       4100-NUM-HANI-CHK-SEC           SECTION.
      *
           IF    ( WRK-NUM         <   1 )
               MOVE    1           TO  WRK-NUM
           END-IF
      *
           IF    ( WRK-NUM         >   SPA-Q02-Q100-EDNUM )
               MOVE    SPA-Q02-Q100-EDNUM
                                   TO  WRK-NUM
           END-IF
      *
           .
       4100-NUM-HANI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����������å�����
      *****************************************************************
       4100-FILE-CHK-SEC           SECTION.
      *
           IF      SPA-Q01-CLIENT-SAVE    =   "1"
      *
               IF    ( SPA-MW             =   SPA-GINBEE )
                   MOVE    SPA-GMN-Q100-FILELST (1)
                                          TO  SPA-GMN-Q100-FILE-G
               END-IF
      *
               IF      SPA-GMN-Q100-FILE       =   SPACE
                   MOVE    SPACE               TO  SPA-GMN-Q100-CDKBN-G
               ELSE
                   PERFORM VARYING IDX2 FROM    1   BY  1
                           UNTIL   IDX2 >       SPA-GMN-Q100-FILE-MAX
                       IF        SPA-GMN-Q100-FILE
                                       =   SPA-GMN-Q100-FILEL (IDX2)
                           MOVE    SPA-GMN-Q100-FILEMEIL (IDX2)
                                               TO  SPA-GMN-Q100-FILEMEI
                           MOVE    SPA-GMN-Q100-FILE-MAX
                                               TO  IDX2
                       END-IF
                   END-PERFORM        
                   IF      SPA-GMN-Q100-FILEMEI  =   SPACE  
                       MOVE    SPA-GMN-Q100-FILELST (1)
                                               TO  SPA-GMN-Q100-FILE-G
                   END-IF
               END-IF
           END-IF
           .
       4100-FILE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ʸ�������ɥ����å�����
      *****************************************************************
       4100-CDKBN-CHK-SEC           SECTION.
      *
           IF      SPA-Q01-CLIENT-SAVE    =   "1"
               IF      SPA-GMN-Q100-CDKBN      =   SPACE
                   IF      SPA-GMN-Q100-FILE       =   "1"
                       MOVE    SPA-GMN-Q100-CDKBNLST (1)
                                               TO  SPA-GMN-Q100-CDKBN-G
                   END-IF
               ELSE
                   PERFORM VARYING IDX2    FROM   1   BY  1
                           UNTIL   IDX2    >      SPA-GMN-Q100-CDKBN-MAX
                       IF        SPA-GMN-Q100-CDKBN
                                       =   SPA-GMN-Q100-CDKBNL (IDX2)
                           MOVE    SPA-GMN-Q100-CDKBNMEIL (IDX2)
                                               TO  SPA-GMN-Q100-CDKBNMEI
                           MOVE    SPA-GMN-Q100-CDKBN-MAX
                                               TO  IDX2
                       END-IF
                   END-PERFORM        
                   IF      SPA-GMN-Q100-CDKBNMEI   =   SPACE  
                       MOVE    SPA-GMN-Q100-CDKBNLST (1)
                                               TO  SPA-GMN-Q100-CDKBN-G
                   END-IF
               END-IF
           END-IF
           .
       4100-CDKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    ZERO                TO  SPA-Q02-Q100-RTOK-FLG
      *
           MOVE    SPA-Q01-SAKIPG      TO  SPA-SAKIPG
           MOVE    "Q100"              TO  SPA-MOTOPG
      *
      *****MOVE    "BACK"              TO  MCP-PUTTYPE
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ꡡ����
      *****************************************************************
       490-KAKUTEI                 SECTION.
      *
      *    ���ϻ������å�
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  490-KAKUTEI-EXT
           END-IF
      *
      *    ���ϥե�����̤̾���ϥ����å��ʣãӣֽ��ϻ���
           IF  ( SPA-Q02-Q100-PROC-FLG     =   ZERO )
               IF  ( SPA-GMN-Q100-FILENAME
                                       =   SPACE )
                   MOVE    "1001"      TO  SPA-ERRCD
                   MOVE    1           TO  SPA-GMN-Q100-CUR
                   GO  TO  490-KAKUTEI-EXT
               END-IF
      *
               PERFORM VARYING IDX0 FROM 100 BY -1 
                       UNTIL ( IDX0 <    1 )
                        OR   ( SPA-ERRCD NOT =   SPACE )
      *
                   IF    ( SPA-GMN-Q100-FILENAME (IDX0 : 1) =  "/" )
                       IF    ( SPA-ERRCD =  SPACE )
                           IF    ( IDX0      =  100 )
                            OR   ( SPA-GMN-Q100-FILENAME ( IDX0 + 1 : )
                                         = SPACE )
                               MOVE    "1005"      TO  SPA-ERRCD
                               MOVE    1           TO  SPA-GMN-Q100-CUR
                               GO  TO  490-KAKUTEI-EXT
                           END-IF
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
           END-IF
      *
      *    ���(�ڡ������˻�������å�
           IF  ( SPA-GMN-Q100-STNUM    >   SPA-GMN-Q100-EDNUM )
      *
               IF  ( SPA-Q02-Q100-PROC-FLG =   ZERO )
                   MOVE    "1002"      TO  SPA-ERRCD
               ELSE
                   MOVE    "1003"      TO  SPA-ERRCD
               END-IF
      *
               MOVE    2           TO  SPA-GMN-Q100-CUR
               GO  TO  490-KAKUTEI-EXT
           END-IF
      *
      *    �ѣ��������
           MOVE    SPA-GMN-Q100-FILENAME
                                       TO  SPA-Q02-Q100-FILENAME
           IF      SPA-GMN-Q100-FILE   =   SPACE
               MOVE    "0"                 TO  SPA-Q02-Q100-FILE
           ELSE
               MOVE    SPA-GMN-Q100-FILE   TO  SPA-Q02-Q100-FILE
           END-IF
      *
           IF  ( SPA-Q02-Q100-PROC-FLG     =   1 )
      *        ���Ϸ��
               COMPUTE SPA-Q02-Q100-STKNS
                   = ( SPA-GMN-Q100-STNUM  -   1 )
                   *   SPA-Q02-Q100-LINE   +   1
      *        ��λ���
               COMPUTE SPA-Q02-Q100-EDKNS
                   =   SPA-GMN-Q100-EDNUM
                   *   SPA-Q02-Q100-LINE
      *        ���ϥڡ���
               MOVE    SPA-GMN-Q100-STNUM
                                       TO  SPA-Q02-Q100-STPAGE
      *        ��λ�ڡ���
               MOVE    SPA-GMN-Q100-EDNUM
                                       TO  SPA-Q02-Q100-EDPAGE
           ELSE
      *        ���Ϸ��
               MOVE    SPA-GMN-Q100-STNUM
                                       TO  SPA-Q02-Q100-STKNS
      *        ��λ���
               MOVE    SPA-GMN-Q100-EDNUM
                                       TO  SPA-Q02-Q100-EDKNS
      *        ʸ�������ɶ�ʬ
               IF      SPA-GMN-Q100-CDKBN  =   SPACE
                   MOVE    "1"                 TO  SPA-Q02-Q100-CDKBN
               ELSE
                   MOVE    SPA-GMN-Q100-CDKBN  TO  SPA-Q02-Q100-CDKBN
               END-IF
           END-IF
      *
           MOVE    1                   TO  SPA-Q02-Q100-RTOK-FLG
      *
           MOVE    SPA-Q01-SAKIPG      TO  SPA-SAKIPG
           MOVE    "Q100"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-KAKUTEI-EXT.
           EXIT.
     *****************************************************************
      *    �������Խ�����
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Q100"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *    ��å�����������
           MOVE    SPA-GMN-Q100-CD     TO  Q100-ID1CODE
      *
      *    ��å�����
           MOVE    SPA-GMN-Q100-MSG    TO  Q100-ID1MSG
      *
      *    �ե�����̾�ʥ�٥��
           MOVE    SPA-GMN-Q100-LBLFILENAME
                                       TO  Q100-LBLFILENAME
      *
      *    �ե�����̾
           MOVE    SPA-GMN-Q100-FILENAME
                                       TO  Q100-FILENAME
           MOVE    SPA-GMN-Q100-FILENAME-STATE
                                       TO  Q100-FILENAME-STATE
      *
      *    �ե�����̾�ʥ�٥��
           MOVE    SPA-GMN-Q100-LBLNUMSEL
                                       TO  Q100-LBLNUMSEL
      *
      *    ���Ϸ���ʥڡ����˿�
           MOVE    SPA-GMN-Q100-STNUM
                                       TO  Q100-STNUM
      *
      *    ��λ����ʥڡ����˿�
           MOVE    SPA-GMN-Q100-EDNUM
                                       TO  Q100-EDNUM
      *
      *    ������
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >       SPA-GMN-Q100-FILE-MAX
               MOVE    SPA-GMN-Q100-FILELST (IDX2)
                                           TO  Q100-FILE-ITEM (IDX2)
           END-PERFORM
           MOVE    SPA-GMN-Q100-FILE-MAX   TO  Q100-FILE-COUNT
           MOVE    SPA-GMN-Q100-FILE-G     TO  Q100-FILE
           MOVE    SPA-GMN-Q100-FILE-STATE TO  Q100-FILE-STATE
      *
      *    ʸ��������
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >       SPA-GMN-Q100-CDKBN-MAX
               MOVE    SPA-GMN-Q100-CDKBNLST (IDX2)
                                           TO  Q100-CDKBN-ITEM (IDX2)
           END-PERFORM
           MOVE    SPA-GMN-Q100-CDKBN-MAX  TO  Q100-CDKBN-COUNT
           MOVE    SPA-GMN-Q100-CDKBN-G    TO  Q100-CDKBN
           MOVE    SPA-GMN-Q100-CDKBN-STATE
                                           TO  Q100-CDKBN-STATE
           IF    ( SPA-GMN-Q100-FILE       =   SPACE )
               MOVE    WIDGET-INSENSITIVE  TO  Q100-CDKBN-STATE
           END-IF
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
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-Q100-CUR    =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-Q100-CUR
      *
      *    �ե�����̾
           WHEN   001
               MOVE  "FILENAME"    TO  MCP-WIDGET
      *
      *    ���Ϸ���ʥڡ����˿�
           WHEN   002
               MOVE  "STNUM"       TO  MCP-WIDGET
      *
      *    ��λ����ʥڡ����˿�
           WHEN   003
               MOVE  "EDNUM"       TO  MCP-WIDGET
      *
      *    ����ܥ���
           WHEN   004
               MOVE  "B12"         TO  MCP-WIDGET
      *
      *    ������
           WHEN   005
               MOVE  "FILE"        TO  MCP-WIDGET
      *
      *    ʸ��������
           WHEN   006
               MOVE  "CDKBN"       TO  MCP-WIDGET
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
      *
      *    ɽ��
           WHEN    "FILENAME"
               MOVE    001         TO  SPA-GMN-Q100-CUR
      *
      *    ������
           WHEN    "STNUM"
               MOVE    002         TO  SPA-GMN-Q100-CUR
      *
      *    ��ﲼ��
           WHEN    "EDNUM"
               MOVE    003         TO  SPA-GMN-Q100-CUR
      *
      *    ������
           WHEN   "FILE"
               MOVE    005         TO  SPA-GMN-Q100-CUR
      *
      *    ʸ��������
           WHEN   "CDKBN"
               MOVE    006         TO  SPA-GMN-Q100-CUR
      *
           END-EVALUATE
      *
      *    ������������֤�����
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "EDNUM"
               IF      SPA-Q01-CLIENT-SAVE    =   "1"
                   MOVE    005         TO  SPA-GMN-Q100-CUR
               ELSE
                   MOVE    004         TO  SPA-GMN-Q100-CUR
               END-IF
           WHEN    "CDKBN"
               MOVE    004         TO  SPA-GMN-Q100-CUR
           WHEN   "FILE"
               IF      SPA-GMN-Q100-FILE       =   "1"
                   MOVE    006         TO  SPA-GMN-Q100-CUR
               ELSE
                   MOVE    004         TO  SPA-GMN-Q100-CUR
               END-IF
           WHEN    OTHER
               COMPUTE SPA-GMN-Q100-CUR    =   SPA-GMN-Q100-CUR +   1
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "1001"
           WHEN    "1005"
               MOVE    "���ϥե�����̾�����Ϥ���Ƥ��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE    "���Ϸ���佪λ����ȤʤäƤ��ޤ�"
                                       TO  SPA-ERRMSG
           WHEN    "1003"
               MOVE    "���ϥڡ����佪λ�ڡ����ȤʤäƤ��ޤ�"
                                       TO  SPA-ERRMSG
           WHEN    "1004"
               STRING   "���ϥե�����̾��" DELIMITED  BY  SIZE
                        "���郎���Ϥ���Ƥ��ޤ�"
                                             DELIMITED  BY  SIZE
                                             INTO  SPA-ERRMSG
               END-STRING
           WHEN    "1006"
               STRING   "���ϥե�����̾��" DELIMITED  BY  SIZE
                        "��������Ϥ���Ƥ��ޤ�"
               DELIMITED  BY  SIZE
               INTO  SPA-ERRMSG
               END-STRING
           END-EVALUATE
      *
           MOVE    SPACE                TO  QERR
           MOVE    SPA-ERRCD            TO  QERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  QERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "Q100"               TO  SPA-MOTOPG
           MOVE    "QERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "QERR"               TO  MCP-WINDOW
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
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *