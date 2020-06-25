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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCGN21FIL.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���޾�������
      *  ����ݡ��ͥ��̾  : �ե����륻�쥯�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  08/06/25    NACL�ݿ���    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������        ����      ����
      * 04.08.01     NACL-����     16/09/27  GINBEE(API)�б�
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
       DATA                DIVISION.
      *
       WORKING-STORAGE         SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    �����ƥ�������������Ѷ��̥ѥ�᥿
           COPY    "N21COMMON-SPA".
      *
           COPY    "N21SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *
      *    �ե�����¸�ߥ����å�
           COPY    "CPORCSFILECHK.INC".
      *    ����ɽ����
           COPY    "ORCA-BLOB".
      *
      *     COPY    "ORCA-DBPATH".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      *    �������᡼�������ѥ�᥿
           COPY    "CPORCSMIAREA.INC".
      *
      *    ���ޥ��
       01  COMMAND                 PIC X(512).
      *
       01  FLG-AREA.
           03  FLG-OK              PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(03).
      *
       01  WRK-AREA.
           03  WRK-PASSMEI         PIC X(256).
           03  WRK-FILEMEI         PIC X(128).
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                 SECTION.
      *
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "N21.INC".
           COPY    "N22.INC".
           COPY    "N23.INC".
           COPY    "N29.INC".
           COPY    "N21ERR.INC".
           COPY    "N21ID1.INC".
           COPY    "N21ID2.INC".
           COPY    "N21FIL.INC".
      *
      ******************************************************************
       PROCEDURE           DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-N21KYOUTU
           MOVE    SPA-FREE        TO  SPA-N21SCRAREA
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *        ��������
               WHEN    "PUTG"          ALSO    "CLICKED"
                   PERFORM 200-GMNSENI
           END-EVALUATE
      *
           MOVE    SPA-N21SCRAREA  TO  SPA-FREE
           MOVE    SPA-N21KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           .
      *
           EXIT    PROGRAM.
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  N21FIL
           MOVE    SPACE           TO  N21FIL-PHOTO
           MOVE    LOW-VALUE       TO  N21FIL-PHOTOAREA
           MOVE    SPACE           TO  SPA-N21FIL-PHOTO
           MOVE    LOW-VALUE       TO  SPA-N21FIL-OBJECT
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        ���
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        ��Ͽ
               WHEN    OTHER
                   PERFORM 210-GAZO
                   PERFORM 210-BACK
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "N21"           TO  SPA-SAKIPG
           MOVE    "N21FIL"        TO  SPA-MOTOPG
      *
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    "JOIN"          TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���åץ����ɡ�����
      *****************************************************************
       210-GAZO                    SECTION.
      *
      *    ���������å�
           IF      N21FIL-PHOTO    =   SPACE
               GO  TO  210-GAZO-EXT
           END-IF
      *
           MOVE    N21FIL-PHOTO    TO  SPA-GMN-PHOTO
      *
      *    ��ĥ�Ҥ��㤨�н���ȴ����
           PERFORM 431-GAZO-TYPE-CHK-SEC
           IF      FLG-OK          =   1
               GO  TO  210-GAZO-EXT
           END-IF
      *
      *    �����ե����뤬����н���ȴ����
           PERFORM 431-GAZO-FILE-CHK
           IF      FLG-OK          =   0
               GO  TO  210-GAZO-EXT
           END-IF
      *
      *    �������᡼���ʤ���н���ȴ����
           IF      N21FIL-PHOTOAREA    =   LOW-VALUE
               GO  TO  210-GAZO-EXT
           END-IF
      *
      *    �����ѥ������å�
           PERFORM 431-GAZO-PATH-CHK-SEC
      *
      *    �����ե������������
           PERFORM 500-GAZO-OBJECT-SEC
      *
      *    �����ǡ���������������å�
           INITIALIZE                  ORCSMIAREA
           MOVE    "misizechk"     TO  ORCSMI-SRYCD
           MOVE    MCP-TEMPDIR     TO  ORCSMI-TEMPDIR
           MOVE    WRK-FILEMEI     TO  ORCSMI-FILENAME
           CALL    "ORCSMISAVE"        USING
                                       ORCSMIAREA
                                       SPA-AREA
           IF      ORCSMI-RETCD    NOT =   ZERO
               MOVE    "SIZEERR"       TO  SPA-N21FIL-PHOTO
           END-IF
           .
       210-GAZO-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ե�����¸�ߥ����å�
      *****************************************************************
       431-GAZO-FILE-CHK           SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
      *    �����ե����뤬�ʤ����Ͻ���ȴ����
           INITIALIZE                  ORCSFILECHKAREA
           MOVE    SPA-GMN-PHOTO   TO  FILECHK-NAME
           CALL    "ORCSFILECHK"       USING
                                       ORCSFILECHKAREA
           IF      FILECHK-RC      NOT =   0
               MOVE    1               TO  FLG-OK
           END-IF
      *
           .
       431-GAZO-FILE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ĥ�ҥ����å�
      *****************************************************************
       431-GAZO-TYPE-CHK-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM VARYING IDX     FROM    256 BY -1 
                   UNTIL   IDX     <       1
               IF      SPA-GMN-PHOTO(IDX:1)    NOT =   SPACE
                   COMPUTE IDX     =   IDX     -   3
                   IF      IDX     >   ZERO
                       EVALUATE    SPA-GMN-PHOTO(IDX:4)
                           WHEN    ".JPG"
                           WHEN    ".GIF"
                           WHEN    ".PNG"
                           WHEN    ".jpg"
                           WHEN    ".gif"
                           WHEN    ".png"
                               CONTINUE
                           WHEN    OTHER
                               MOVE    1           TO  FLG-OK
                       END-EVALUATE
                       MOVE    1               TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       431-GAZO-TYPE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ѥ�̾�Խ�
      *****************************************************************
       431-GAZO-PATH-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-GMN-PHOTO   TO  WRK-PASSMEI
           MOVE    SPACE           TO  WRK-FILEMEI
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM VARYING IDX     FROM    256 BY -1 
                   UNTIL   IDX     <       1
               IF      WRK-PASSMEI(IDX:)   NOT =   SPACE
                   IF    ( WRK-PASSMEI(IDX:1)  =   "/" ) OR
                         ( WRK-PASSMEI(IDX:1)  =   "\" )
                       IF      WRK-PASSMEI(1:IDX)
                                           NOT =   SPA-GMN-FOLDER(1:IDX)
                           MOVE    1           TO  FLG-OK
                       END-IF
                       COMPUTE IDX     =   IDX     +   1
                       MOVE    WRK-PASSMEI(IDX:)
                                           TO  WRK-FILEMEI
                       MOVE    1           TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-FILEMEI     =   SPACE
               MOVE    WRK-PASSMEI     TO  WRK-FILEMEI
           END-IF
      *
           .
       431-GAZO-PATH-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ե������������
      *****************************************************************
       500-GAZO-OBJECT-SEC         SECTION.
      *
           MOVE    N21FIL-PHOTOAREA
                                   TO  ORCA-BLOB-OBJECT
      *
           MOVE    SPACE           TO  ORCA-BLOB-FILE
           STRING  SPA-GMN-FOLDER      DELIMITED  BY  SPACE
                   WRK-FILEMEI         DELIMITED  BY  SPACE
                   INTO                ORCA-BLOB-FILE
           END-STRING
      *
      *    ���᡼���ǡ����ե�����������ޥ��
           MOVE    LOW-VALUE       TO  COMMAND
           STRING  "mkdir -p "         DELIMITED BY SIZE
                   SPA-GMN-FOLDER      DELIMITED BY SIZE
                               INTO    COMMAND
           END-STRING
      *    ���ޥ�ɼ¹�
           CALL    "system"            USING
                                       COMMAND
      *    �����ե���������
           MOVE    "BLOBEXPORT"    TO  MCP-FUNC
           MOVE    "blob"          TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       ORCA-BLOB
                                       SPA-AREA
           .
       500-GAZO-OBJECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *