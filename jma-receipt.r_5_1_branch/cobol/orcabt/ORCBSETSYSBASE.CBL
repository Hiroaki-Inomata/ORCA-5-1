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
       PROGRAM-ID.                 ORCBSETSYSBASE.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : 
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  16/10/18    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������        ����      ����
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-SYSBASE             PIC 9(01).
           03  FLG-OK                  PIC 9(01).
      *
      *    �ѥ�᡼��
       01  PARA-AREA.
           03  PARA-COMMAND            PIC X(03).
           03  PARA-GRPNUM             PIC 9(02).
           03  PARA-HOSPNUM            PIC 9(02).
           03  PARA-KIGEN              PIC X(08).
           03  PARA-HONBUNGRP          PIC 9(02).
           03  PARA-HONBUNKBN          PIC X(01).
           03  PARA-HOSPNAME           PIC X(120).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-PATH                PIC X(64).
           03  WRK-SYSYMD              PIC X(08).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYMD2.
               05  WRK-SYY2            PIC 9(02).
               05  WRK-SMM2            PIC 9(02).
               05  WRK-SDD2            PIC 9(02).
           03  WRK-SYSTIME             PIC X(08).
      *
       01  WRK-PRT-AREA.
           03  WRK-PRT-TITLE1.
               05  FILLER              PIC X(51)   VALUE
               "���롼��|���ŵ���| ��  �� |��ʬ��|��ʬ��|���ŵ���̾".
           03  WRK-PRT-TITLE2.
               05  FILLER              PIC X(41)   VALUE
               "��    ��|�����ֹ�|        |��  ��|��  ʬ|".
               05  FILLER              PIC X(20)   VALUE
               "(���ʡ������ƥ����)".
           03  WRK-PRT-TITLE3.
               05  FILLER              PIC X(41)   VALUE
               "--------+--------+--------+------+------+".
               05  FILLER              PIC X(40)   VALUE
               "----------------------------------------".
           03  WRK-PRT-BODY1.
               05  FILLER                     PIC X(03) VALUE  SPACE.
               05  WRK-PRT-GRPNUM             PIC 9(02).
               05  FILLER                     PIC X(07) VALUE  SPACE.
               05  WRK-PRT-HOSPNUM            PIC 9(02).
               05  FILLER                     PIC X(04) VALUE  SPACE.
               05  WRK-PRT-KIGEN              PIC X(08).
               05  FILLER                     PIC X(03) VALUE  SPACE.
               05  WRK-PRT-HONBUNGRP          PIC 9(02).
               05  FILLER                     PIC X(05) VALUE  SPACE.
               05  WRK-PRT-HONBUNKBN          PIC X(01).
               05  FILLER                     PIC X(04) VALUE  SPACE.
               05  WRK-PRT-HOSPNAME           PIC X(40).
           03  WRK-PRT-BODY2.
               05  FILLER                     PIC X(41) VALUE  SPACE.
               05  WRK-PRT-HOSPNAME2          PIC X(40).
      *
      *****************************************************************
      *    �ե��������
      *****************************************************************
      *
      *    �����ƥ�����ʰ��ŵ��ؾ����
           COPY    "CPSK1001.INC".
      *
      *    �����ƥ����
       01  SYSBASE-REC.
           COPY    "CPSYSBASE.INC".
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    ���̥ѥ�᥿
           COPY    "MCPAREA".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      ****************************************************************
       PROCEDURE               DIVISION
                                   USING
                                   COMMAND-PARAM.
      ****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *    �ѥ�᥿���å�
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                               INTO    PARA-COMMAND
                                       PARA-GRPNUM
                                       PARA-HOSPNUM
                                       PARA-KIGEN
                                       PARA-HONBUNGRP
                                       PARA-HONBUNKBN
                                       PARA-HOSPNAME
           END-UNSTRING
      *
           MOVE    1               TO  SPA-HOSPNUM
      *
           PERFORM 100-MAIN-SEC
      *
           STOP    RUN
           .
      ****************************************************************
      *    �硡������
      *****************************************************************
       100-MAIN-SEC                SECTION.
      *
      *    �ǡ����١��������ץ�
           PERFORM 100-DBOPEN-SEC
      *
      *    �����Խ�����
           PERFORM 800-HIZUKE-SET-SEC
      *
           EVALUATE    PARA-COMMAND
      *        ����ɽ��
               WHEN    "lst"
               WHEN    "LST"
                   PERFORM 210-SYSBASE-LIST-SEC
      *        �ɲ�
               WHEN    "add"
               WHEN    "ADD"
                   PERFORM 220-SYSBASE-ADD-SEC
      *        ����
               WHEN    "mod"
               WHEN    "MOD"
                   PERFORM 230-SYSBASE-MOD-SEC
      *        ���
               WHEN    "del"
               WHEN    "DEL"
                   PERFORM 240-SYSBASE-DEL-SEC
           END-EVALUATE
      *
      *    �ǡ����١�����������
           PERFORM 900-DBDISCONNECT-SEC
           .
       100-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ���ܾ��󡡰���ɽ������
      *****************************************************************
       210-SYSBASE-LIST-SEC        SECTION.
      *
           MOVE    SPACE           TO  WRK-PATH
      *
           IF      PARA-HOSPNUM    =   ZERO
               IF      PARA-HONBUNGRP  =   ZERO
                   MOVE    "all"           TO  WRK-PATH
               ELSE
                   MOVE    "key2"          TO  WRK-PATH
               END-IF
           ELSE
               MOVE    "key"           TO  WRK-PATH
           END-IF
      *
      *    �����ƥ���ܡ��ӣţ̣ţãԽ���
           PERFORM 900-SYSBASE-SELECT-SEC
      *
           IF      FLG-SYSBASE     =   1
               GO  TO  210-SYSBASE-LIST-EXT
           END-IF
      *
      *    ���Ф�����
           PERFORM 211-LIST-PRINT-TITLE-SEC
      *
           PERFORM UNTIL FLG-SYSBASE   =   1
      *        ���ٽ���
               PERFORM 212-LIST-PRINT-BODY-SEC
      *        �����ƥ���ܡ��ƣţԣãȽ���
               PERFORM 900-SYSBASE-FETCH-SEC
           END-PERFORM
      *
      *    �����ƥ���ܡ��ạ̃ϣӣŽ���
           PERFORM 900-SYSBASE-CLOSE-SEC
           .
       210-SYSBASE-LIST-EXT.
           EXIT.
      *****************************************************************
      *    ���Ф�����
      *****************************************************************
       211-LIST-PRINT-TITLE-SEC    SECTION.
      *
           DISPLAY WRK-PRT-TITLE1
           DISPLAY WRK-PRT-TITLE2
           DISPLAY WRK-PRT-TITLE3
           .
       211-LIST-PRINT-TITLE-EXT.
           EXIT.
      *****************************************************************
      *    ���ٽ���
      *****************************************************************
       212-LIST-PRINT-BODY-SEC     SECTION.
      *
           MOVE    ZERO               TO  WRK-PRT-GRPNUM
           MOVE    ZERO               TO  WRK-PRT-HOSPNUM
           MOVE    SPACE              TO  WRK-PRT-KIGEN
           MOVE    ZERO               TO  WRK-PRT-HONBUNGRP
           MOVE    SPACE              TO  WRK-PRT-HONBUNKBN
           MOVE    SPACE              TO  WRK-PRT-HOSPNAME
           MOVE    SPACE              TO  WRK-PRT-HOSPNAME2
      *
           MOVE    SYSBASE-GRPNUM     TO  WRK-PRT-GRPNUM
           MOVE    SYSBASE-HOSPNUM    TO  WRK-PRT-HOSPNUM
           MOVE    SYSBASE-KIGEN      TO  WRK-PRT-KIGEN
           MOVE    SYSBASE-HONBUNGRP  TO  WRK-PRT-HONBUNGRP
           MOVE    SYSBASE-HONBUNKBN  TO  WRK-PRT-HONBUNKBN
           MOVE    SYSBASE-HOSPNAME   TO  WRK-PRT-HOSPNAME
      *
           DISPLAY WRK-PRT-BODY1
      *
      *    �����ƥ���������ɤ߹��߽���
           PERFORM 900-SYS1001-READ-SEC
      *
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    SYS-1001-HOSPNAME   TO  WRK-PRT-HOSPNAME2
           END-IF
      *
           DISPLAY WRK-PRT-BODY2
           .
       212-LIST-PRINT-BODY-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ���ܾ����ɲý���
      *****************************************************************
       220-SYSBASE-ADD-SEC         SECTION.
      *
           IF      PARA-HOSPNUM    =   ZERO
               DISPLAY "���ŵ��ؼ����ֹ����ꤷ�Ƥ���������"
               GO  TO  220-SYSBASE-ADD-EXT
           END-IF
      *
      *    �����ƥ���ܡ��ңţ��Ľ���
           PERFORM 900-SYSBASE-READ-SEC
      *
           IF      FLG-SYSBASE     =   ZERO
               DISPLAY "���ꤵ�줿���ŵ��ؼ����ֹ�ϴ��ˤ���ޤ���"
               GO  TO  220-SYSBASE-ADD-EXT
           END-IF
      *
           INITIALIZE                  SYSBASE-REC
           MOVE    PARA-HOSPNUM    TO  SYSBASE-GRPNUM
           MOVE    PARA-HOSPNUM    TO  SYSBASE-HOSPNUM
           MOVE    "99999999"      TO  SYSBASE-KIGEN
           MOVE    0               TO  SYSBASE-HONBUNGRP
           MOVE    "0"             TO  SYSBASE-HONBUNKBN
           MOVE    "��"            TO  SYSBASE-HOSPNAME
           MOVE    WRK-SYSYMD      TO  SYSBASE-CREYMD
           MOVE    SPACE           TO  SYSBASE-UPYMD
           MOVE    WRK-SYSTIME     TO  SYSBASE-UPHMS
      *
           IF      PARA-GRPNUM     NOT =   ZERO
               MOVE    PARA-GRPNUM     TO  SYSBASE-GRPNUM
           END-IF
           IF      PARA-KIGEN      NOT =   SPACE
               MOVE    PARA-KIGEN      TO  SYSBASE-KIGEN
           END-IF
           IF      PARA-HONBUNGRP  NOT =   ZERO
               MOVE    PARA-HONBUNGRP  TO  SYSBASE-HONBUNGRP
           END-IF
           IF      PARA-HONBUNKBN  NOT =   SPACE
               MOVE    PARA-HONBUNKBN  TO  SYSBASE-HONBUNKBN
           END-IF
           IF      PARA-HOSPNAME   NOT =   SPACE
               MOVE    PARA-HOSPNAME   TO  SYSBASE-HOSPNAME
           END-IF
      *
      *    �ɲý���
           MOVE    SYSBASE-REC     TO  MCPDATA-REC
           MOVE    "DBINSERT"      TO  MCP-FUNC
           MOVE    "tbl_sysbase"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
          .
       220-SYSBASE-ADD-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ���ܾ��󡡹�������
      *****************************************************************
       230-SYSBASE-MOD-SEC         SECTION.
      *
      *    �����ƥ���ܡ��ңţ��Ľ���
           PERFORM 900-SYSBASE-READ-SEC
      *
           IF      FLG-SYSBASE     =   1
               DISPLAY "���ꤵ�줿���ŵ��ؼ����ֹ�Ϥ���ޤ���Ǥ�����"
               GO  TO  230-SYSBASE-MOD-EXT
           END-IF
      *
           IF      PARA-HOSPNUM    NOT =   ZERO
               MOVE    PARA-HOSPNUM    TO  SYSBASE-HOSPNUM
           END-IF
           IF      PARA-KIGEN      NOT =   SPACE
               MOVE    PARA-KIGEN      TO  SYSBASE-KIGEN
           END-IF
           IF      PARA-HONBUNGRP  NOT =   ZERO
               MOVE    PARA-HONBUNGRP  TO  SYSBASE-HONBUNGRP
           END-IF
           IF      PARA-HONBUNKBN  NOT =   SPACE
               MOVE    PARA-HONBUNKBN  TO  SYSBASE-HONBUNKBN
           END-IF
           IF      PARA-HOSPNAME   NOT =   SPACE
               MOVE    PARA-HOSPNAME   TO  SYSBASE-HOSPNAME
           END-IF
           MOVE    WRK-SYSYMD      TO  SYSBASE-UPYMD
           MOVE    WRK-SYSTIME     TO  SYSBASE-UPHMS
      *
      *    ��������
           MOVE    SYSBASE-REC     TO  MCPDATA-REC
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           MOVE    "tbl_sysbase"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
          .
       230-SYSBASE-MOD-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ���ܾ��󡡺������
      *****************************************************************
       240-SYSBASE-DEL-SEC         SECTION.
      *
      *    �����ƥ���ܡ��ңţ��Ľ���
           PERFORM 900-SYSBASE-READ-SEC
      *
           IF      FLG-SYSBASE     =   1
               DISPLAY "���ꤵ�줿���ŵ��ؼ����ֹ�Ϥ���ޤ���Ǥ�����"
               GO  TO  240-SYSBASE-DEL-EXT
           END-IF
      *
      *    �������
           MOVE    SYSBASE-REC     TO  MCPDATA-REC
           MOVE    "DBDELETE"      TO  MCP-FUNC
           MOVE    "tbl_sysbase"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
          .
       240-SYSBASE-DEL-EXT.
           EXIT.
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       800-HIZUKE-SET-SEC         SECTION.
      *
      *    ���ղ��̥����å�����
           ACCEPT  WRK-SYMD2       FROM    DATE
           MOVE    ZERO                TO  WRK-SYMD
           MOVE    WRK-SYMD2           TO  WRK-SYMD(3:)
           ADD     2000                TO  WRK-SYY
           MOVE    WRK-SYMD            TO  WRK-SYSYMD
      *
           ACCEPT  WRK-SYSTIME     FROM    TIME
           .
       800-HIZUKE-SET-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ���ܡ��ңţ��Ľ���
      *****************************************************************
       900-SYSBASE-READ-SEC        SECTION.
      *
           MOVE    "key"           TO  WRK-PATH
           PERFORM 900-SYSBASE-SELECT-SEC
           PERFORM 900-SYSBASE-CLOSE-SEC
           .
       900-SYSBASE-READ-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ���ܡ��ӣţ̣ţãԽ���
      *****************************************************************
       900-SYSBASE-SELECT-SEC      SECTION.
      *
           INITIALIZE                  SYSBASE-REC
           MOVE    PARA-HOSPNUM    TO  SYSBASE-HOSPNUM
           MOVE    PARA-HONBUNGRP  TO  SYSBASE-HONBUNGRP
           MOVE    "tbl_sysbase"   TO  MCP-TABLE
           MOVE    WRK-PATH        TO  MCP-PATHNAME
           MOVE    SYSBASE-REC     TO  MCPDATA-REC
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
      *        �����ƥ���ܡ��ƣţԣãȽ���
               PERFORM 900-SYSBASE-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-SYSBASE
           END-IF
           .
       900-SYSBASE-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ���ܡ��ƣţԣãȽ���
      *****************************************************************
       900-SYSBASE-FETCH-SEC      SECTION.
      *
           MOVE    "tbl_sysbase"   TO  MCP-TABLE
           MOVE    WRK-PATH        TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  SYSBASE-REC
               MOVE    ZERO            TO  FLG-SYSBASE
           ELSE
               MOVE    1               TO  FLG-SYSBASE
           END-IF
           .
       900-SYSBASE-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ���ܡ��ạ̃ϣӣŽ���
      *****************************************************************
       900-SYSBASE-CLOSE-SEC      SECTION.
      *
           MOVE    "tbl_sysbase"   TO  MCP-TABLE
           MOVE    WRK-PATH        TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-SYSBASE-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ���������ɤ߹��߽���
      *****************************************************************
       900-SYS1001-READ-SEC        SECTION.
      *
           MOVE    SPACE           TO  SYS-1001-REC
           MOVE    SYSBASE-HOSPNUM TO  SYS-1001-HOSPNUM
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           MOVE    WRK-SYSYMD      TO  SYS-1001-STYUKYMD
                                       SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1001-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-SYS1001-READ-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£ӣţ̣ţãԽ���
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£ƣţԣãȽ���
      *****************************************************************
       920-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£ạ̃ϣӣŽ���
      *****************************************************************
       990-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¡������ץ����
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¡�������������
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.