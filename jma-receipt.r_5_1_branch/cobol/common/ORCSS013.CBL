      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCSS013.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : テンポラリ適応病名マスタ編集
      *  管理者            :
      *  作成日付    作業者        記述
      *  13/01/28    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *
       WORKING-STORAGE                 SECTION.
      *
       01  IDX-AREA.
           03  IDX1                            PIC 9(05).
      *
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
           03  FLG-CHKSND                      PIC 9(01).
           03  FLG-TEKBYO                      PIC 9(01).
           03  FLG-TEKDIS                      PIC 9(01).
           03  FLG-MATERS                      PIC 9(01).
      *
       01  WRK-AREA.
           03  WRK-BYOMEI                      PIC X(200).
           03  WRK-TEKBYOKBN                   PIC X(01).
           03  WRK-SRYCD                       PIC X(09).
      *
       01  CONST-AREA.
           03  CONST-TEMP                      PIC X(04)   VALUE "TEMP".
           03  CONST-RENNUM                    PIC 9(05)   VALUE 10000.
      *
       01  KEY-AREA.
           03  KEY-SRYCD                       PIC X(09).
      *
       01  TCHKSND-REC.
           COPY    "CPCHKSND.INC"  REPLACING  //CHKSND//
                                   BY         //TCHKSND//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1101.INC".
      *
       01  CHKSND-REC.
           COPY    "CPCHKSND.INC".
      *
       01  TMPSND-REC.
           COPY    "CPCHKSND.INC"  REPLACING  //CHKSND//
                                   BY         //TMPSND//.
      *
       01  TEKIOUBYOMEI-REC.
           COPY    "CPTEKIOUBYOMEI.INC".
      *
       01  TEKIOU-DISEASE-REC.
           COPY    "CPTEKIOU-DISEASE.INC".
      *
       01  MATERIAL-S-REC.
           COPY    "CPMATERIAL-S.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    チェックマスタ病名編集サブ
           COPY    "CPORCSS012.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                         SECTION.
           COPY    "CPORCSS013.INC".
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       PROCEDURE                       DIVISION
           USING
           SS013-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           PERFORM 990-EXIT-PROGRAM-SEC
      *
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  IDX-AREA
                                       FLG-AREA
                                       WRK-AREA
                                       KEY-AREA
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           IF    ( SS013-FUNC          =   "DBCREATE" )
               MOVE    CONST-TEMP      TO  SS013-TEMP-NAME
      *
               MOVE    SMCNDATE-YMD    TO  SS013-TEMP-KEY
               MOVE    SMCNDATE-HMS    TO  SS013-TEMP-KEY(9:)
           END-IF
      *
           IF    ( SS013-CHKKBN    =   "1" OR "2" )
               PERFORM 1001-INIT-SEC
           ELSE
               MOVE    "0"         TO  WRK-TEKBYOKBN
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期　処理
      *****************************************************************
       1001-INIT-SEC                   SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1101"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SS013-KJNYMD        TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1101-REC
      *
           IF    ( SYS-1101-TEKBYOYKZKBN   =   SPACE )
               MOVE    ZERO            TO  SYS-1101-TEKBYOYKZKBN
           END-IF
           IF    ( SYS-1101-TEKBYOSRYKBN   =   SPACE )
               MOVE    ZERO            TO  SYS-1101-TEKBYOSRYKBN
           END-IF
      *
           IF    ( SS013-SRYCD         =   SPACE )
               CONTINUE
           ELSE
               PERFORM 900-CHKSND-KEY27-SEL-SEC
               MOVE    CHKSND-REC         TO   TCHKSND-REC
      *
               MOVE    CHKSND-TEKBYOKBN
                                   TO  WRK-TEKBYOKBN
      *
               IF    ( WRK-TEKBYOKBN   =   SPACE )
                   IF    ( SS013-CHKKBN =  "1" )
                       MOVE    SYS-1101-TEKBYOYKZKBN
                                   TO  WRK-TEKBYOKBN
                   END-IF
      *
                   IF    ( SS013-CHKKBN =  "2" )
                       MOVE    SYS-1101-TEKBYOSRYKBN
                                   TO  WRK-TEKBYOKBN
                   END-IF
               END-IF
           END-IF
      *
           .
       1001-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           EVALUATE    SS013-FUNC
           WHEN    "DBCREATE"
      *
               EVALUATE    SS013-CREATEKBN
               WHEN    "1"
                   INITIALIZE                  CHKSND-REC
                   MOVE    LOW-VALUE       TO  CHKSND-CHKKBN
                   MOVE    CHKSND-REC      TO  MCPDATA-REC
                   MOVE    "tbl_chksnd"    TO  MCP-TABLE
                   MOVE    "tmpkey"        TO  MCP-PATHNAME
                   PERFORM 910-DBCREATE-SEC
      *
                   PERFORM 800-TMPSND-EDIT-SEC
      *
               WHEN    "2"
                   INITIALIZE                  CHKSND-REC
                   MOVE    SPA-HOSPNUM     TO  CHKSND-HOSPNUM
                   MOVE    SS013-CHKKBN    TO  CHKSND-CHKKBN
                   MOVE    SS013-SRYCD     TO  CHKSND-SRYCD
                   MOVE    CHKSND-REC      TO  MCPDATA-REC
                   MOVE    "tbl_chksnd"    TO  MCP-TABLE
                   MOVE    "tmpkey"        TO  MCP-PATHNAME
                   PERFORM 910-DBCREATE-SEC
      *
               WHEN    "9"
      *
                   INITIALIZE                  CHKSND-REC
                   MOVE    LOW-VALUE       TO  CHKSND-CHKKBN
                   MOVE    CHKSND-REC      TO  MCPDATA-REC
                   MOVE    "tbl_chksnd"    TO  MCP-TABLE
                   MOVE    "tmpkey"        TO  MCP-PATHNAME
                   PERFORM 910-DBCREATE-SEC
      *
               WHEN    OTHER
      *
                   INITIALIZE                  CHKSND-REC
                   MOVE    SPA-HOSPNUM     TO  CHKSND-HOSPNUM
                   MOVE    SS013-CHKKBN    TO  CHKSND-CHKKBN
                   MOVE    SS013-SRYCD     TO  CHKSND-SRYCD
                   MOVE    CHKSND-REC      TO  MCPDATA-REC
                   MOVE    "tbl_chksnd"    TO  MCP-TABLE
                   MOVE    "tmpkey"        TO  MCP-PATHNAME
                   PERFORM 910-DBCREATE-SEC
      *
                   PERFORM 800-TMPSND-EDIT-SEC
      *
               END-EVALUATE
           WHEN    "DBINSERT"
               PERFORM 800-TMPSND-EDIT-SEC
           WHEN    "DBDROP"
               MOVE    "tbl_chksnd"    TO  MCP-TABLE
               MOVE    "tmpkey"        TO  MCP-PATHNAME
               PERFORM 910-DBDROP-SEC
      *
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    一時チェックマスタ２編集処理
      *****************************************************************
       800-TMPSND-EDIT-SEC              SECTION.
      *
           MOVE    TCHKSND-REC          TO  CHKSND-REC
      *
           IF    ( SS013-TEKBYOKBN      NOT =   ZERO )
               IF    ( SS013-SRYCD          =   SPACE )
                OR   ( SS013-TEKBYOKBN      =    "1" )
                   PERFORM 8001-TMPSND-EDIT-SEC
               ELSE
                   IF    ( WRK-TEKBYOKBN   =    "1" OR "2"  )
                       IF    ( CHKSND-HOSPNUM   NOT =   ZERO )
                           IF    ( CHKSND-GRPCHKKBN    =   SPACE )
                            AND  ( WRK-TEKBYOKBN       =   "1" )
                               PERFORM 8001-TMPSND-EDIT-SEC
                           END-IF
                       ELSE
                           PERFORM 8001-TMPSND-EDIT-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       800-TMPSND-EDIT-EXT.
            EXIT.
      *****************************************************************
      *    一時チェックマスタ２編集処理
      *****************************************************************
       8001-TMPSND-EDIT-SEC            SECTION.
      *
           EVALUATE    SS013-CHKKBN
           WHEN    "1"
      *
               PERFORM 80011-TEKDIS-TO-CHKSND-SEC
      *
           WHEN    "2"
      *
               PERFORM 80011-TEKBYO-TO-CHKSND-SEC
      *
               PERFORM 80011-MATERS-TO-CHKSND-SEC
      *
           END-EVALUATE
      *
           INITIALIZE                  CHKSND-REC
           MOVE    SPA-HOSPNUM     TO  CHKSND-HOSPNUM
           MOVE    SS013-CHKKBN    TO  CHKSND-CHKKBN
           MOVE    SS013-SRYCD     TO  CHKSND-SRYCD
           MOVE    SS013-KJNYMD    TO  CHKSND-YUKOSTYMD
                                       CHKSND-YUKOEDYMD
           MOVE    CONST-TEMP      TO  CHKSND-TERMID
           MOVE    SS013-TEMP-KEY  TO  CHKSND-OPID
           MOVE    CHKSND-REC      TO  MCPDATA-REC
      *
           MOVE    CHKSND-REC      TO  MCPDATA-REC
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "tmpdel1"       TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           IF    ( SS013-TEKBYOKBN =   "2" )
               PERFORM 80012-TMPSND-EDIT-SEC
           END-IF
      *
           .
       8001-TMPSND-EDIT-EXT.
            EXIT.
      *****************************************************************
      *    一時チェックマスタ２編集処理
      *****************************************************************
       80011-TEKDIS-TO-CHKSND-SEC      SECTION.
      *
           PERFORM 900-TEKIOU-DISEASE-KEY3-SEL-SEC
      *
           PERFORM UNTIL ( FLG-TEKDIS  NOT =   ZERO )
      *
               MOVE    CONST-RENNUM    TO  CHKSND-RENNUM
               MOVE    TEKDIS-SELSRYCD TO  KEY-SRYCD
      *
               PERFORM UNTIL ( FLG-TEKDIS      NOT =   ZERO )
                        OR   ( TEKDIS-SELSRYCD NOT =   KEY-SRYCD )
      *
                   MOVE    TEKDIS-SELBYOMEI
                                       TO  WRK-BYOMEI
      *
                   PERFORM 80011-TMPSND-EDIT-SEC
      *
                   PERFORM 900-TEKIOU-DISEASE-KEY3-FET-SEC
      *
               END-PERFORM
      *
           END-PERFORM
      *
           MOVE    "tbl_tekiou_disease"    TO   MCP-TABLE
           MOVE    "key3"                  TO   MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       80011-TEKDIS-TO-CHKSND-EXT.
            EXIT.
      *****************************************************************
      *    一時チェックマスタ２編集処理
      *****************************************************************
       80011-TEKBYO-TO-CHKSND-SEC      SECTION.
      *
           PERFORM 900-TEKIOUBYOMEI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-TEKBYO  NOT =   ZERO )
      *
               COMPUTE CHKSND-RENNUM = CONST-RENNUM * 2
      *
               MOVE    TEKBYO-SRYCD    TO  KEY-SRYCD
      *
               PERFORM UNTIL ( FLG-TEKBYO      NOT =   ZERO )
                        OR   ( TEKBYO-SRYCD    NOT =   KEY-SRYCD )
      *
                   MOVE    TEKBYO-BYOMEI
                                       TO  WRK-BYOMEI
      *
                   PERFORM 80011-TMPSND-EDIT-SEC
      *
                   PERFORM 900-TEKIOUBYOMEI-KEY2-FET-SEC
      *
               END-PERFORM
      *
           END-PERFORM
      *
           MOVE    "tbl_tekioubyomei"  TO   MCP-TABLE
           MOVE    "key2"              TO   MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       80011-TEKBYO-TO-CHKSND-EXT.
            EXIT.
      *****************************************************************
      *    一時チェックマスタ２編集処理
      *****************************************************************
       80011-MATERS-TO-CHKSND-SEC      SECTION.
      *
           PERFORM 900-MATERIAL-S-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-MATERS  NOT =   ZERO )
      *
               MOVE    CONST-RENNUM    TO  CHKSND-RENNUM
               MOVE    MATERS-SRYCD    TO  KEY-SRYCD
      *
               PERFORM UNTIL ( FLG-MATERS      NOT =   ZERO )
                        OR   ( MATERS-SRYCD    NOT =   KEY-SRYCD )
      *
                   MOVE    MATERS-BYOMEI
                                       TO  WRK-BYOMEI
      *
                   PERFORM 80011-TMPSND-EDIT-SEC
      *
                   PERFORM 900-MATERIAL-S-KEY2-FET-SEC
      *
               END-PERFORM
      *
           END-PERFORM
      *
           MOVE    "tbl_material_s"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       80011-MATERS-TO-CHKSND-EXT.
            EXIT.
      *****************************************************************
      *    一時チェックマスタ２編集処理
      *****************************************************************
       80011-TMPSND-EDIT-SEC           SECTION.
      *
           INITIALIZE              SS012-AREA
           MOVE    SS013-KJNYMD (1:6)
                               TO  SS012-SRYYM
      *
           MOVE    WRK-BYOMEI  TO  SS012-I-BYOMEI
           CALL    "ORCSS012"      USING
                                   SS012-AREA
                                   SPA-AREA
      *
           IF    ( SS012-HAISHI    =   ZERO )
      *
               INITIALIZE                  TMPSND-REC
               MOVE    SPA-HOSPNUM     TO  TMPSND-HOSPNUM
               MOVE    SS013-CHKKBN    TO  TMPSND-CHKKBN
               MOVE    KEY-SRYCD       TO  TMPSND-SRYCD
               MOVE    "00000000"      TO  TMPSND-YUKOSTYMD
               MOVE    "99999999"      TO  TMPSND-YUKOEDYMD
               MOVE    "1"             TO  TMPSND-CDKBN
               MOVE    SPACE           TO  TMPSND-GRPCHKKBN
               MOVE    SPACE           TO  TMPSND-GRPID
               MOVE    SPACE           TO  TMPSND-NYUGAIKBN
      *
               COMPUTE CHKSND-RENNUM   =   CHKSND-RENNUM  +   1
               MOVE    CHKSND-RENNUM   TO  TMPSND-RENNUM
      *
               MOVE    SS012-BYOMEICD-G
                                       TO  TMPSND-CD
      *
               MOVE    SS012-O-BYOMEI  TO  TMPSND-BYOMEI
               MOVE    "1"             TO  TMPSND-COMPKBN
      *
               MOVE    SPACE           TO  TMPSND-HOSOKU
               MOVE    "1"             TO  TMPSND-HOSOKU-COMPKBN
      *
               IF    ( SS013-CHKKBN  =   "1" )
                   MOVE    "0"         TO  TMPSND-UTAGAIKBN
               ELSE
                   MOVE    "1"         TO  TMPSND-UTAGAIKBN
               END-IF
               MOVE    SS012-BYOMEIHENFLG
                                       TO  TMPSND-BYOMEIHENFLG
      *
               MOVE    CONST-TEMP      TO  TMPSND-TERMID
               MOVE    SS013-TEMP-KEY  TO  TMPSND-OPID
      *
               MOVE    TMPSND-REC      TO  MCPDATA-REC
               MOVE    "tbl_chksnd"    TO  MCP-TABLE
               MOVE    "tmpins2"       TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
      *
           END-IF
      *
           .
       80011-TMPSND-EDIT-EXT.
            EXIT.
      *****************************************************************
      *    一時チェックマスタ２編集処理（基本情報レコード作成）
      *****************************************************************
       80012-TMPSND-EDIT-SEC           SECTION.
      *
           PERFORM 900-CHKSND-KEY33-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
      *
               INITIALIZE              TMPSND-REC
      *
               MOVE    CHKSND-HOSPNUM  TO  TMPSND-HOSPNUM
               MOVE    CHKSND-CHKKBN   TO  TMPSND-CHKKBN
               MOVE    CHKSND-SRYCD    TO  TMPSND-SRYCD
               MOVE    "00000000"      TO  TMPSND-YUKOSTYMD
               MOVE    "99999999"      TO  TMPSND-YUKOEDYMD
               MOVE    "0"             TO  TMPSND-CDKBN
               MOVE    SPACE           TO  TMPSND-GRPCHKKBN
               MOVE    SPACE           TO  TMPSND-GRPID
               MOVE    SPACE           TO  TMPSND-NYUGAIKBN
      *
               MOVE    CHKSND-RENNUM   TO  TMPSND-RENNUM
      *
               MOVE    CONST-TEMP      TO  TMPSND-TERMID
               MOVE    SS013-TEMP-KEY  TO  TMPSND-OPID
      *
               MOVE    TMPSND-REC      TO  MCPDATA-REC
               MOVE    "tbl_chksnd"    TO  MCP-TABLE
               MOVE    "tmpins2"       TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
      *
               PERFORM 900-CHKSND-KEY33-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key33"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       80012-TMPSND-EDIT-EXT.
            EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY27-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           INITIALIZE                      CHKSND-REC
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    SS013-CHKKBN        TO  CHKSND-CHKKBN
           MOVE    SS013-SRYCD         TO  CHKSND-SRYCD
           MOVE    SS013-KJNYMD        TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    "0"                 TO  CHKSND-CDKBN
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHKSND-KEY27-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY33-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           INITIALIZE                      CHKSND-REC
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    SS013-CHKKBN        TO  CHKSND-CHKKBN
           MOVE    SS013-SRYCD         TO  CHKSND-SRYCD
           MOVE    SS013-KJNYMD        TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key33"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
       900-CHKSND-KEY33-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY33-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key33"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
       900-CHKSND-KEY33-FET-EXT.
           EXIT.
      *****************************************************************
      *    適応病名マスタ情報検索(KEY2)
      *****************************************************************
       900-TEKIOUBYOMEI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TEKBYO
      *
           INITIALIZE                  TEKIOUBYOMEI-REC
           MOVE    SS013-CHKKBN        TO  TEKBYO-CHKKBN
           IF    ( SS013-SRYCD         =   SPACE )
               MOVE    "%"             TO  TEKBYO-SRYCD
           ELSE
               MOVE    SS013-SRYCD     TO  TEKBYO-SRYCD
           END-IF
           MOVE    TEKIOUBYOMEI-REC    TO  MCPDATA-REC
           MOVE    "tbl_tekioubyomei"  TO   MCP-TABLE
           MOVE    "key2"              TO   MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TEKIOUBYOMEI-REC
           ELSE
               INITIALIZE                  TEKIOUBYOMEI-REC
               MOVE    1               TO  FLG-TEKBYO
           END-IF
           .
       900-TEKIOUBYOMEI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    適応病名マスタ情報FETCH(KEY2)
      *****************************************************************
       900-TEKIOUBYOMEI-KEY2-FET-SEC       SECTION.
      *
           MOVE    "tbl_tekioubyomei"  TO   MCP-TABLE
           MOVE    "key2"              TO   MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TEKIOUBYOMEI-REC
           ELSE
               INITIALIZE              TEKIOUBYOMEI-REC
               MOVE    1               TO  FLG-TEKBYO
           END-IF
           .
      *
       900-TEKIOUBYOMEI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    医薬品傷病名マスタ検索
      *****************************************************************
       900-TEKIOU-DISEASE-KEY3-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-TEKDIS
      *
           INITIALIZE                      TEKIOU-DISEASE-REC
           IF    ( SS013-SRYCD         =   SPACE )
               MOVE    "%"             TO  TEKDIS-SELSRYCD
           ELSE
               MOVE    SS013-SRYCD     TO  TEKDIS-SELSRYCD
           END-IF
           MOVE    TEKIOU-DISEASE-REC  TO  MCPDATA-REC
           MOVE    "tbl_tekiou_disease" TO  MCP-TABLE
           MOVE    "key3"              TO   MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TEKIOU-DISEASE-REC
           ELSE
               INITIALIZE                  TEKIOU-DISEASE-REC
               MOVE    1               TO  FLG-TEKDIS
           END-IF
           .
       900-TEKIOU-DISEASE-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    医薬品傷病名マスタFETCH
      *****************************************************************
       900-TEKIOU-DISEASE-KEY3-FET-SEC       SECTION.
      *
           MOVE    "tbl_tekiou_disease" TO  MCP-TABLE
           MOVE    "key3"              TO   MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TEKIOU-DISEASE-REC
           ELSE
               INITIALIZE                  TEKIOU-DISEASE-REC
               MOVE    1               TO  FLG-TEKDIS
           END-IF
           .
      *
       900-TEKIOU-DISEASE-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    医薬品傷病名マスタ検索
      *****************************************************************
       900-MATERIAL-S-KEY2-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-MATERS
      *
           INITIALIZE                      MATERIAL-S-REC
           IF    ( SS013-SRYCD         =   SPACE )
               MOVE    "%"             TO  MATERS-SRYCD
           ELSE
               MOVE    SS013-SRYCD     TO  MATERS-SRYCD
           END-IF
           MOVE    MATERIAL-S-REC      TO  MCPDATA-REC
           MOVE    "tbl_material_s"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  MATERIAL-S-REC
           ELSE
               INITIALIZE                  MATERIAL-S-REC
               MOVE    1               TO  FLG-MATERS
           END-IF
      *
           .
       900-MATERIAL-S-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    医薬品傷病名マスタFETCH
      *****************************************************************
       900-MATERIAL-S-KEY2-FET-SEC       SECTION.
      *
           MOVE    "tbl_material_s"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  MATERIAL-S-REC
           ELSE
               INITIALIZE                  MATERIAL-S-REC
               MOVE    1               TO  FLG-MATERS
           END-IF
           .
      *
       900-MATERIAL-S-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ作成処理
      *****************************************************************
       910-DBCREATE-SEC                SECTION.
      *
           MOVE    "DBCREATE2"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCREATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＲＯＰ処理
      *****************************************************************
       910-DBDROP-SEC                  SECTION.
      *
           MOVE    "DBDROP"            TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDROP-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                 SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                 SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    EXIT PROGRAM
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
