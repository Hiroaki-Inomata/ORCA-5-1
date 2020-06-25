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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSNOFILES.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : 未配置ファイルチェックサブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/10/18    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
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
       01  FLG-AREA.
           03  FLG-SYSKANRI                PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-KANRICD                 PIC X(04).
           03  WRK-BTPG-G.
               05  WRK-BTPG-ID             PIC 9(03).
           03  WRK-ONPG-G.
               05  WRK-ONPG-STAFFCD        PIC X(05).
               05  WRK-ONPG-FIL            PIC X(01).
               05  WRK-ONPG-ID             PIC 9(02).
           03  WRK-ONPG-MAX                PIC 9(05).
           03  WRK-TYPE                    PIC X(02).
      *
       01  TPG-AREA.
           03  TPG-OCC2                    OCCURS  2.
               05  TPG-OCC                 OCCURS  100.
                   07  TPG-KBNCD           PIC X(08).
                   07  TPG-PGID            PIC X(20).
                   07  TPG-PGNAME          PIC X(50).
                   07  TPG-NOFILE          PIC 9(01).
      *
       01  CONST-AREA.
           03  CONST-BTPG-MAX                  PIC 9(05)   VALUE   100.
           03  CONST-SNOFILES-MAX              PIC 9(05)   VALUE   200.
           03  CONST-ONPG-GAI-G.
               05  CONST-ONPG-GAI-FIL01        PIC X(50)   VALUE
                   "診療録（カルテ１号紙）".
               05  CONST-ONPG-GAI-FIL02        PIC X(50)   VALUE
                   "処方せん（院外）".
               05  CONST-ONPG-GAI-FIL03        PIC X(50)   VALUE
                  "請求書兼領収書".
               05  CONST-ONPG-GAI-FIL04        PIC X(50)   VALUE
                  "薬剤情報提供書".
               05  CONST-ONPG-GAI-FIL05        PIC X(50)   VALUE
                  "診療録（カルテ３号紙）".
               05  CONST-ONPG-GAI-FIL06        PIC X(50)   VALUE
                  "支払証明書（日別）".
               05  CONST-ONPG-GAI-FIL07        PIC X(50)   VALUE
                  "支払証明書（月別）".
               05  CONST-ONPG-GAI-FIL08        PIC X(50)   VALUE
                  "診療費明細書".
               05  CONST-ONPG-GAI-FIL09        PIC X(50)   VALUE
                  "お薬手帳".
               05  CONST-ONPG-GAI-FIL10        PIC X(50)   VALUE
                  "予約票".
               05  CONST-ONPG-GAI-FIL11        PIC X(50)   VALUE
                  "予約一覧".
               05  CONST-ONPG-GAI-FIL12        PIC X(50)   VALUE
                  "予約患者一覧".
           03  CONST-ONPG-GAI-R    REDEFINES   CONST-ONPG-GAI-G.
               05  CONST-ONPG-GAI          PIC X(50)
                                           OCCURS      12.
           03  CONST-ONPG-GAI-MAX              PIC 9(03)
                                           VALUE       12.
           03  CONST-ONPG-NYU-G.
               05  CONST-ONPG-NYU-FIL01        PIC X(50)   VALUE
                   "診療録（カルテ１号紙）".
               05  CONST-ONPG-NYU-FIL02        PIC X(50)   VALUE
                   "退院証明書".
               05  CONST-ONPG-NYU-FIL03        PIC X(50)   VALUE
                  "請求書兼領収書".
               05  CONST-ONPG-NYU-FIL04        PIC X(50)   VALUE
                  "診療録（カルテ３号紙）".
               05  CONST-ONPG-NYU-FIL05        PIC X(50)   VALUE
                  "入院処方箋".
               05  CONST-ONPG-NYU-FIL06        PIC X(50)   VALUE
                  "注射処方箋".
               05  CONST-ONPG-NYU-FIL07        PIC X(50)   VALUE
                  "指示箋".
               05  CONST-ONPG-GAI-FIL08        PIC X(50)   VALUE
                  "診療費明細書".
               05  CONST-ONPG-NYU-FIL09        PIC X(50)   VALUE
                  "薬剤情報提供書".
               05  CONST-ONPG-NYU-FIL10        PIC X(50)   VALUE
                  "お薬手帳".
           03  CONST-ONPG-NYU-R    REDEFINES   CONST-ONPG-NYU-G.
               05  CONST-ONPG-NYU          PIC X(50)
                                           OCCURS      10.
           03  CONST-ONPG-NYU-MAX              PIC 9(03)
                                           VALUE       10.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1032.INC"          REPLACING //1032//
                                           BY        //ONPG//.
           COPY    "CPSK3001.INC"          REPLACING //3001//
                                           BY        //BTPG//.
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ファイル探索サブ
           COPY    "CPORCSACCESS.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "CPORCSNOFILES.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SNOFILES-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SNOFILES-AREA
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           MOVE    "01"            TO  SNOFILES-ONPG-GAI
           MOVE    "02"            TO  SNOFILES-ONPG-NYU
           MOVE    "03"            TO  SNOFILES-NIPPOU
           MOVE    "04"            TO  SNOFILES-GEPPOU
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-SYS1032-CHK-SEC
      *
           PERFORM 2001-SYS1035-CHK-SEC
      *
           PERFORM 2001-SYS3001-CHK-SEC
      *
           PERFORM 2001-SYS3002-CHK-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    帳票プログラム選択情報チェック処理（オンライン）
      *****************************************************************
       2001-SYS1032-CHK-SEC            SECTION.
      *
           MOVE    "1032"              TO  WRK-KANRICD
           MOVE    CONST-ONPG-GAI-MAX  TO  WRK-ONPG-MAX
           MOVE    SNOFILES-ONPG-GAI   TO  WRK-TYPE
           PERFORM 20011-ONLINEPG-CHK-SEC
      *
           .
       2001-SYS1032-CHK-EXT.
           EXIT.
      *****************************************************************
      *    帳票プログラム選択情報チェック処理（オンライン入院）
      *****************************************************************
       2001-SYS1035-CHK-SEC            SECTION.
      *
           MOVE    "1035"              TO  WRK-KANRICD
           MOVE    CONST-ONPG-NYU-MAX  TO  WRK-ONPG-MAX
           MOVE    SNOFILES-ONPG-NYU   TO  WRK-TYPE
           PERFORM 20011-ONLINEPG-CHK-SEC
      *
           .
       2001-SYS1035-CHK-EXT.
           EXIT.
      *****************************************************************
      *    オンライン帳票チェック処理
      *****************************************************************
       20011-ONLINEPG-CHK-SEC       SECTION.
      *
           INITIALIZE                  TPG-AREA
      *
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-ONPG-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    SYS-ONPG-KBNCD  TO  WRK-ONPG-G
      *
               MOVE    ZERO        TO  IDX1
               MOVE    ZERO        TO  IDX2
      *
               IF    ( WRK-ONPG-STAFFCD    =   "00000" )
                   MOVE    1       TO  IDX1
               ELSE
                   IF    ( SPA-STAFFCD =   WRK-ONPG-STAFFCD )
                       MOVE    2   TO  IDX1
                   END-IF
               END-IF
      *
               IF    ( WRK-ONPG-ID IS  NUMERIC )
                AND  ( WRK-ONPG-ID >=  1 )
                AND  ( WRK-ONPG-ID <=  WRK-ONPG-MAX )
                   MOVE    WRK-ONPG-ID
                                   TO  IDX2
               END-IF
      *
               IF    ( IDX1        NOT =   ZERO )
                AND  ( IDX2        NOT =   ZERO )
                   MOVE    SYS-ONPG-KBNCD
                                   TO  TPG-KBNCD (IDX1 IDX2)
                   IF    ( SYS-ONPG-ORCACSTCHK     =   "T" )
                    AND  ( SYS-ONPG-ORCACSTNM  NOT =   SPACE )
                       INITIALIZE              SACCESS-AREA
                       MOVE    "X"             TO  SACCESS-MODE
                       STRING  SYS-ONPG-ORCACSTNM  DELIMITED   BY  SPACE
                               ".so"               DELIMITED   BY  SIZE
                       INTO    SACCESS-IN-FILENAME
                       END-STRING
                       CALL    "ORCSACCESS"    USING
                                               SACCESS-AREA
                       IF    ( SACCESS-RC      NOT =   ZERO )
                           MOVE    SYS-ONPG-ORCACSTNM
                                           TO  TPG-PGID (IDX1 IDX2)
                           MOVE    1       TO  TPG-NOFILE (IDX1 IDX2)
                           IF    ( WRK-TYPE    =  SNOFILES-ONPG-NYU )
                               MOVE    CONST-ONPG-NYU (IDX2)
                                           TO  TPG-PGNAME(IDX1 IDX2)
                           ELSE
                               MOVE    CONST-ONPG-GAI (IDX2)
                                           TO  TPG-PGNAME(IDX1 IDX2)
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-ONPG-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   WRK-ONPG-MAX )
                    OR   ( SNOFILES-MAX    >=  CONST-SNOFILES-MAX )
      *
               MOVE    ZERO        TO  IDX1
               EVALUATE    TRUE
               WHEN  ( TPG-KBNCD (2 IDX2)  NOT =   SPACE )
                   IF    ( TPG-NOFILE (2 IDX2)     =   1 )
                       MOVE    2   TO  IDX1
                   END-IF
               WHEN  ( TPG-KBNCD (1 IDX2)  NOT =   SPACE )
                   IF    ( TPG-NOFILE (1 IDX2)     =   1 )
                       MOVE    1   TO  IDX1
                   END-IF
               END-EVALUATE
      *
               IF    ( IDX1        >   ZERO )
                   COMPUTE SNOFILES-MAX = SNOFILES-MAX + 1
                   MOVE    WRK-TYPE
                                   TO  SNOFILES-TYPE(SNOFILES-MAX)
                   MOVE    IDX2    TO  SNOFILES-ID  (SNOFILES-MAX)
                   MOVE    TPG-PGID  (IDX1 IDX2)
                                   TO  SNOFILES-PGID(SNOFILES-MAX)
                   MOVE    TPG-PGNAME(IDX1 IDX2)
                                   TO  SNOFILES-PGNAME(SNOFILES-MAX)
               END-IF
      *
           END-PERFORM
      *
           .
       20011-ONLINEPG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    帳票プログラム選択情報チェック処理（日報）
      *****************************************************************
       2001-SYS3001-CHK-SEC            SECTION.
      *
           MOVE    "3001"              TO  WRK-KANRICD
           MOVE    SNOFILES-NIPPOU     TO  WRK-TYPE
           PERFORM 20011-BATCHPG-CHK-SEC
      *
           .
       2001-SYS1032-CHK-EXT.
           EXIT.
      *****************************************************************
      *    帳票プログラム選択情報チェック処理（月報）
      *****************************************************************
       2001-SYS3002-CHK-SEC            SECTION.
      *
           MOVE    "3002"              TO  WRK-KANRICD
           MOVE    SNOFILES-GEPPOU     TO  WRK-TYPE
           PERFORM 20011-BATCHPG-CHK-SEC
      *
           .
       2001-SYS3002-CHK-EXT.
           EXIT.
      *****************************************************************
      *    統計帳票チェック処理
      *****************************************************************
       20011-BATCHPG-CHK-SEC           SECTION.
      *
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-BTPG-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( SNOFILES-MAX    >=  CONST-SNOFILES-MAX )
      *
               IF    ( SYS-BTPG-KBNCD  >=  "001" )
                AND  ( SYS-BTPG-KBNCD  <=  "100" )
                   MOVE    SYS-BTPG-KBNCD
                                       TO  WRK-BTPG-G
                   INITIALIZE              SACCESS-AREA
                   MOVE    "X"             TO  SACCESS-MODE
                   STRING  SYS-BTPG-PRGNAME    DELIMITED   BY  SPACE
                           ".so"               DELIMITED   BY  SIZE
                   INTO    SACCESS-IN-FILENAME
                   END-STRING
                   CALL    "ORCSACCESS"    USING
                                           SACCESS-AREA
                   IF    ( SACCESS-RC      NOT =   ZERO )
                       COMPUTE SNOFILES-MAX = SNOFILES-MAX + 1
                       MOVE    WRK-TYPE
                                       TO  SNOFILES-TYPE(SNOFILES-MAX)
                       MOVE    WRK-BTPG-ID
                                       TO  SNOFILES-ID  (SNOFILES-MAX)
                       MOVE    SYS-BTPG-PRGNAME
                                       TO  SNOFILES-PGID(SNOFILES-MAX)
                       MOVE    SYS-BTPG-HCNAME
                                       TO  SNOFILES-PGNAME(SNOFILES-MAX)
                   END-IF
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-BTPG-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-BATCHPG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           PERFORM 800-DISPLAY-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    デバッグＤＩＳＰＬＡＹ
      *****************************************************************
       800-DISPLAY-SEC             SECTION.
      *
           DISPLAY "SNOFILES-MAX[" SNOFILES-MAX "]"
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SNOFILES-MAX )
      *
               DISPLAY IDX1 ":TYPE["  SNOFILES-TYPE (IDX1) "]"
                            "ID["     SNOFILES-ID-G (IDX1) "]"
                            "PGID["   SNOFILES-PGID (IDX1) "]"
                            "PGNAME[" SNOFILES-PGNAME (IDX1) "]"
      *
           END-PERFORM
      *
           .
       800-DISPLAY-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           INITIALIZE              SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    WRK-KANRICD     TO  SYS-KANRICD
           MOVE    SPA-SYSYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  (   MCP-RC              =   ZERO    )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
      *
       900-SYSKANRI-KEY2-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF  (   MCP-RC              =   ZERO    )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
      *
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO      MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
