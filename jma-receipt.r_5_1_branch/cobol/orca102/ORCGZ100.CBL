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
       PROGRAM-ID.                 ORCGZ100.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �����ޥ�������
      *  ����ݡ��ͥ��̾  : ���ϻؼ����̡ʣڣ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  08/06/02    NACL�ݿ���    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������        ����      ����
      *  04.08.01    NACL-����     14/09/03  GINBEE�б�
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
      *    �����ޥ��������Ѷ��̥ѥ�᥿
           COPY    "Z01COMMON-SPA".
      *    �����̣ӣУ����ꥢ
           COPY    "Z01SCR-SPA".
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
           03  IDX                                 PIC 9(03).
           03  IDX2                                PIC 9(03).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-NUM                             PIC 9(09).
           03  WRK-MCP-WIDGET                      PIC X(64).
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
      *    ���饤�������¸���ݾ����������
           COPY    "CPORCSCLIENTSV.INC".
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
           COPY    "ORCA102SCRAREA.INC".
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
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
                                       WRK-AREA
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE        TO  SPA-Z01FREE
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
           MOVE    SPA-Z01FREE     TO  SPA-FREE
           MOVE    SPA-Z01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
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
           IF  ( SPA-MOTOPG            =   "ZERR" )
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
           MOVE    "Z100"              TO  MCP-WINDOW
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
           MOVE    SPACE               TO  SPA-Z100-AREA
           INITIALIZE                      SPA-Z100-AREA
      *
           MOVE    9                   TO  SPA-Z100-CUR
      *
      *    ���饤�������¸���ݾ����������
           INITIALIZE                      ORCSCLIENTSVAREA
           CALL    "ORCSCLIENTSV"      USING   ORCSCLIENTSVAREA
                                               SPA-AREA
           MOVE    SCLIENTSV-CLIENT-SAVE
                                       TO  SPA-Z100-CLIENT-SAVE
           IF      SPA-Z100-CLIENT-SAVE    =   "1"
               MOVE    "1"                 TO  SPA-Z100-FILEL   (1)
               MOVE    "���饤�������¸"  TO  SPA-Z100-FILEMEIL(1)
               MOVE    1                   TO  SPA-Z100-FILE-MAX
               MOVE    WIDGET-NORMAL       TO  SPA-Z100-FILE-STATE
      *
               MOVE    "1"             TO  SPA-Z100-CDKBNL   (1)
               MOVE    "�ţգ�"        TO  SPA-Z100-CDKBNMEIL(1)
               MOVE    "2"             TO  SPA-Z100-CDKBNL   (2)
               MOVE    "�ӡݣ���"    TO  SPA-Z100-CDKBNMEIL(2)
               MOVE    "3"             TO  SPA-Z100-CDKBNL   (3)
               MOVE    "�գԣơݣ�"    TO  SPA-Z100-CDKBNMEIL(3)
               MOVE    3               TO  SPA-Z100-CDKBN-MAX
               MOVE    WIDGET-NORMAL   TO  SPA-Z100-CDKBN-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  SPA-Z100-FILE-STATE
                                               SPA-Z100-CDKBN-STATE
           END-IF
      *
           MOVE    "3001"                  TO  SPA-Z100-CD
           MOVE    "�ãӣֽ��Ϥ򳫻Ϥ��ޤ�"
                                           TO  SPA-Z100-MSG
      *----(04.08.01)--UPD-START---
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       8
               MOVE    SPACE       TO  SPA-Z100-FILENAME(IDX)
               IF      SPA-CSV-CHK(IDX)   =   "T"
      *            GINBEE�ΤȤ�
                   IF      SPA-MW      =   SPA-GINBEE
                       STRING  "OUTPUT"           DELIMITED SIZE
                               IDX                DELIMITED SIZE
                               ".csv"             DELIMITED SIZE
                                            INTO  SPA-Z100-FILENAME(IDX)
                       END-STRING
                   ELSE
                       STRING  "/tmp/OUTPUT"      DELIMITED SIZE
                               IDX                DELIMITED SIZE
                               ".csv"             DELIMITED SIZE
                                            INTO  SPA-Z100-FILENAME(IDX)
                       END-STRING
                   END-IF
                   MOVE    WIDGET-NORMAL
                                       TO  SPA-Z100-FILENAME-STATE(IDX)
               ELSE
                   MOVE    WIDGET-INSENSITIVE
                                       TO  SPA-Z100-FILENAME-STATE(IDX)
               END-IF
           END-PERFORM
      *----(04.08.01)--UPD-END-----
      *----(04.08.01)--ADD-START---
      *    GINBEE�ΤȤ�
           IF      SPA-MW      =   SPA-GINBEE
               MOVE    SPA-Z100-FILELST(1)     TO  SPA-Z100-FILE-G
               MOVE    SPA-Z100-CDKBNLST(1)    TO  SPA-Z100-CDKBN-G
           END-IF
      *----(04.08.01)--ADD-END-----
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
      *
      *****************************************************************
      *    ���á�����
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
      *    ���ϸĽ�Υ��å�
           PERFORM 400-GMN-INPUT-SEC
      *
      *    ���ϥ����å�����
           PERFORM 410-INPUT-CHK-SEC
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϸĽ�Υ��åȽ���
      *****************************************************************
       400-GMN-INPUT-SEC           SECTION.
      *
           MOVE    ZERO        TO      SPA-Z100-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
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
      *    �ե�����̾�����å�
           PERFORM 4100-FILENAME-CHK-SEC
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
      *
      *****************************************************************
      *    ���̣ӣУ��Խ�����
      *****************************************************************
       4101-GMN-SPA-SET-SEC        SECTION.
      *
      *    �ե�����̾
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       8
               MOVE    Z100-FILENAME(IDX)  TO  SPA-Z100-FILENAME(IDX)
           END-PERFORM
      *
      *    ������
           MOVE    Z100-FILE       TO  SPA-Z100-FILE-G
      *
      *    ʸ��������
           MOVE    Z100-CDKBN      TO  SPA-Z100-CDKBN-G
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *----(04.08.01)--UPD-START---
      *****************************************************************
      *    �ե�����̾�����å�
      *****************************************************************
       4100-FILENAME-CHK-SEC           SECTION.
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       8
                   OR      SPA-ERRCD       NOT =   SPACE
               IF    ( SPA-CSV-CHK(IDX)        =   "T" )
                   IF    ( SPA-Z100-FILENAME(IDX)  =   SPACE )
                       MOVE    "1001"      TO  SPA-ERRCD
                       MOVE    IDX         TO  SPA-Z100-CUR
                   ELSE
                       MOVE    ZERO                TO  WRK-NUM
                       PERFORM VARYING IDX2    FROM    1   BY  1
                               UNTIL   IDX2    >       100
                               OR      SPA-Z100-FILENAME(IDX)(IDX2:1)
                                                       =   SPACE
                               OR      SPA-ERRCD   NOT =   SPACE
                           IF      SPA-Z100-FILENAME(IDX)(IDX2:1)
                                                       =   "\" OR  ":" 
                               MOVE    "1004"          TO  SPA-ERRCD
                               MOVE    IDX             TO  SPA-Z100-CUR
                           ELSE
                               IF      SPA-Z100-FILENAME(IDX)(IDX2:1)
                                                           =   "/"
      *                            GINBEE�ΤȤ�
                                   IF      SPA-MW      =   SPA-GINBEE
                                       MOVE    "1005"  TO  SPA-ERRCD
                                       MOVE    IDX     TO  SPA-Z100-CUR
                                   ELSE
                                       MOVE    IDX2    TO  WRK-NUM
                                   END-IF
                               END-IF
                           END-IF
                       END-PERFORM
                   END-IF
                   IF      SPA-ERRCD       =   SPACE
      *                GINBEE�ΤȤ�
                       IF      SPA-MW      =   SPA-GINBEE
                           MOVE    SPACE   TO  SPA-CSV-DIRNAME(IDX)
                           MOVE    SPA-Z100-FILENAME(IDX)
                                           TO  SPA-CSV-FILENAME(IDX)
                       ELSE
                           IF      WRK-NUM     NOT =   ZERO
                               MOVE    SPA-Z100-FILENAME(IDX)(1:WRK-NUM)
                                               TO  SPA-CSV-DIRNAME(IDX)
                               COMPUTE WRK-NUM =   WRK-NUM +   1
                               MOVE    SPA-Z100-FILENAME(IDX)(WRK-NUM:)
                                               TO  SPA-CSV-FILENAME(IDX)
                           ELSE
                               MOVE    "/home/orca/"
                                               TO  SPA-CSV-DIRNAME(IDX)
                               MOVE    SPA-Z100-FILENAME(IDX)
                                               TO  SPA-CSV-FILENAME(IDX)
                           END-IF
                           MOVE    SPACE   TO  SPA-Z100-FILENAME(IDX)
                           STRING  SPA-CSV-DIRNAME(IDX)
                                                   DELIMITED  BY  SPACE
                                   SPA-CSV-FILENAME(IDX)
                                                   DELIMITED  BY  SPACE
                                   INTO   SPA-Z100-FILENAME(IDX)
                           END-STRING
                       END-IF
                       PERFORM VARYING IDX2    FROM    1   BY  1
                               UNTIL   IDX2    >=      IDX
                               OR      SPA-ERRCD       NOT =   SPACE
                           IF      SPA-Z100-FILENAME(IDX2)
                                       =   SPA-Z100-FILENAME(IDX)
                               MOVE    "1002"      TO  SPA-ERRCD
                               MOVE    IDX         TO  SPA-Z100-CUR
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       4100-FILENAME-CHK-EXT.
           EXIT.
      *----(04.08.01)--UPD-END-----
      *****************************************************************
      *    ����������å�����
      *****************************************************************
       4100-FILE-CHK-SEC           SECTION.
      *
           IF      SPA-Z100-CLIENT-SAVE    =   "1"
               IF      SPA-Z100-FILE       =   SPACE
      *----(04.08.01)--UPD-START---
      *            GINBEE�ΤȤ�
                   IF      SPA-MW      =   SPA-GINBEE
                       MOVE    SPA-Z100-FILELST(1)
                                               TO  SPA-Z100-FILE-G
                   ELSE
                       MOVE    SPACE           TO  SPA-Z100-CDKBN-G
                   END-IF
      *----(04.08.01)--UPD-END-----
               ELSE
                   PERFORM VARYING IDX2 FROM    1   BY  1
                           UNTIL   IDX2 >       SPA-Z100-FILE-MAX
                       IF        SPA-Z100-FILE
                                       =   SPA-Z100-FILEL(IDX2)
                           MOVE    SPA-Z100-FILEMEIL(IDX2)
                                               TO  SPA-Z100-FILEMEI
                           MOVE    SPA-Z100-FILE-MAX
                                               TO  IDX2
                       END-IF
                   END-PERFORM
                   IF      SPA-Z100-FILEMEI  =   SPACE
                       MOVE    SPA-Z100-FILELST(1)
                                               TO  SPA-Z100-FILE-G
                   END-IF
               END-IF
           END-IF
      *
           .
       4100-FILE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ʸ�������ɥ����å�����
      *****************************************************************
       4100-CDKBN-CHK-SEC          SECTION.
      *
           IF      SPA-Z100-CLIENT-SAVE    =   "1"
               IF      SPA-Z100-CDKBN      =   SPACE
                   IF      SPA-Z100-FILE       =   "1"
                       MOVE    SPA-Z100-CDKBNLST(1)
                                               TO  SPA-Z100-CDKBN-G
                   END-IF
               ELSE
                   PERFORM VARYING IDX2    FROM   1   BY  1
                           UNTIL   IDX2    >      SPA-Z100-CDKBN-MAX
                       IF        SPA-Z100-CDKBN
                                       =   SPA-Z100-CDKBNL(IDX2)
                           MOVE    SPA-Z100-CDKBNMEIL(IDX2)
                                               TO  SPA-Z100-CDKBNMEI
                           MOVE    SPA-Z100-CDKBN-MAX
                                               TO  IDX2
                       END-IF
                   END-PERFORM
                   IF      SPA-Z100-CDKBNMEI   =   SPACE
                       MOVE    SPA-Z100-CDKBNLST(1)
                                               TO  SPA-Z100-CDKBN-G
                   END-IF
               END-IF
           END-IF
      *
           .
       4100-CDKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPACE               TO  SPA-ZID1-FLG
           MOVE    "Z91"               TO  SPA-SAKIPG
           MOVE    "Z100"              TO  SPA-MOTOPG
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
      *    ���
           IF      SPA-Z100-FILE   =   SPACE
               MOVE    "0"             TO  SPA-CSV-FILE
           ELSE
               MOVE    SPA-Z100-FILE   TO  SPA-CSV-FILE
           END-IF
      *
      *     ʸ�������ɶ�ʬ
           IF      SPA-Z100-CDKBN  =   SPACE
               MOVE    "0"             TO  SPA-CSV-CDKBN
           ELSE
               MOVE    SPA-Z100-CDKBN  TO  SPA-CSV-CDKBN
           END-IF
      *
           MOVE    "SV"                TO  SPA-ZID1-FLG
      *
           MOVE    "Z91"               TO  SPA-SAKIPG
           MOVE    "Z100"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "Z91"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
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
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Z100"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE              Z100
      *    ��å�����������
           MOVE    SPA-Z100-CD     TO  Z100-ID1CODE
      *
      *    ��å�����
           MOVE    SPA-Z100-MSG    TO  Z100-ID1MSG
      *
      *    �ե�����̾
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       8
               MOVE    SPA-Z100-FILENAME(IDX)
                                           TO  Z100-FILENAME(IDX)
               MOVE    SPA-Z100-FILENAME-STATE(IDX)
                                           TO  Z100-FILENAME-STATE(IDX)
           END-PERFORM
      *
      *    ������
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-Z100-FILE-MAX
               MOVE    SPA-Z100-FILELST(IDX)
                                           TO  Z100-FILE-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-Z100-FILE-MAX       TO  Z100-FILE-COUNT
           MOVE    SPA-Z100-FILE-G         TO  Z100-FILE
           MOVE    SPA-Z100-FILE-STATE     TO  Z100-FILE-STATE
      *
      *    ʸ��������
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-Z100-CDKBN-MAX
               MOVE    SPA-Z100-CDKBNLST(IDX)
                                           TO  Z100-CDKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-Z100-CDKBN-MAX      TO  Z100-CDKBN-COUNT
           MOVE    SPA-Z100-CDKBN-G        TO  Z100-CDKBN
           MOVE    SPA-Z100-CDKBN-STATE    TO  Z100-CDKBN-STATE
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
           IF     (SPA-ERRCD       =   SPACE)  AND
                  (SPA-Z100-CUR    =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-Z100-CUR
           WHEN   001
               MOVE  "FILENAME1"   TO  MCP-WIDGET
           WHEN   002
               MOVE  "FILENAME2"   TO  MCP-WIDGET
           WHEN   003
               MOVE  "FILENAME3"   TO  MCP-WIDGET
           WHEN   004
               MOVE  "FILENAME4"   TO  MCP-WIDGET
           WHEN   005
               MOVE  "FILENAME5"   TO  MCP-WIDGET
           WHEN   006
               MOVE  "FILENAME6"   TO  MCP-WIDGET
           WHEN   007
               MOVE  "FILENAME7"   TO  MCP-WIDGET
           WHEN   008
               MOVE  "FILENAME8"   TO  MCP-WIDGET
           WHEN   009
               MOVE  "B12"         TO  MCP-WIDGET
           WHEN   010
               MOVE  "FILE"        TO  MCP-WIDGET
           WHEN   011
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
           WHEN    "FILENAME1"
               MOVE    002         TO  SPA-Z100-CUR
           WHEN    "FILENAME2"
               MOVE    003         TO  SPA-Z100-CUR
           WHEN    "FILENAME3"
               MOVE    004         TO  SPA-Z100-CUR
           WHEN    "FILENAME4"
               MOVE    005         TO  SPA-Z100-CUR
           WHEN    "FILENAME5"
               MOVE    006         TO  SPA-Z100-CUR
           WHEN    "FILENAME6"
               MOVE    007         TO  SPA-Z100-CUR
           WHEN    "FILENAME7"
               MOVE    008         TO  SPA-Z100-CUR
           WHEN    "FILENAME8"
               MOVE    009         TO  SPA-Z100-CUR
           WHEN   "B12"
               MOVE    010         TO  SPA-Z100-CUR
           WHEN   "FILE"
               MOVE    011         TO  SPA-Z100-CUR
           WHEN   "CDKBN"
               MOVE    009         TO  SPA-Z100-CUR
           END-EVALUATE
      *
           .
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "1001"
               MOVE    "���ϥե�����̾�����Ϥ���Ƥ��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE    "���ϥե�����̾����ʣ���Ƥ��ޤ�"
                                       TO  SPA-ERRMSG
           WHEN    "1004"
               STRING   "���ϥե�����̾��" DELIMITED  BY  SIZE
                        "�ɡ��ɡ��ɡ�ɤ����Ϥ��Ƥ���ޤ�"
                                           DELIMITED  BY  SIZE
                                           INTO  SPA-ERRMSG
               END-STRING
      *----(04.08.01)--ADD-START---
           WHEN    "1005"
               MOVE    "���ϥե�����̾�ˡ������ϤǤ��ޤ���"
                                       TO  SPA-ERRMSG
      *----(04.08.01)--ADD-END-----
           END-EVALUATE
      *
           MOVE    SPACE                TO  ZERR
           MOVE    SPA-ERRCD            TO  ZERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  ZERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "Z100"               TO  SPA-MOTOPG
           MOVE    "ZERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "ZERR"               TO  MCP-WINDOW
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
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *