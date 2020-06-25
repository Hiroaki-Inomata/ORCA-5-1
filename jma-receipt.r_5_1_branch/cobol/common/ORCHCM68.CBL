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
       PROGRAM-ID.             ORCHCM68.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 経過措置医薬品置換マスタ一覧
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/12/18    NACL−太田    新規作成
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
      *    帳票
           COPY    "HCM68.INC".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-TENSU           PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-SRYCDCHG        PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-LOOPEND         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDX-LINE            PIC 9(05).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(03).
           03  CNT-LINE            PIC 9(03).
           03  CNT-MEISAI          PIC 9(03).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD            PIC X(08).
           03  WRK-EDTYMD-G.
               05  WRK-EDTYMD1     PIC X(10).
               05  WRK-EDTYMD3     PIC X(22).
           03  WRK-SAKYMD-PRT      PIC X(22).
           03  WRK-SRYYMD-PRT      PIC X(22).
           03  WRK-ZZZZ9           PIC ZZZZ9.
           03  WRK-TENXX.
               05  WRK-TENZ9       PIC ZZZZZZ9.99.
      *
       01  CONST-AREA.
           03  CONST-LINE-MAX      PIC 9(05)   VALUE   30.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    入力コード
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    老人一般コード変換
       01  SRYCDCHG-REC.
           COPY    "CPSRYCDCHG.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    シェル用領域
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY    "COMMON-SPA".
           COPY    "CPORCSPRT.INC".
           COPY    "CPORCHCM68.INC".
      *
       PROCEDURE                   DIVISION    USING
                                               SPA-AREA
                                               ORCSPRTAREA
                                               ORCHCM68-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           PERFORM 990-EXIT-PROGRAM-SEC
      *
           .
       000-PROC-EXT.
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  ORCHCM68-OT-AREA
      *
           MOVE    SPA-SYSYMD      TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3     TO  WRK-SAKYMD-PRT
      *
           MOVE    ORCHCM68-SRYYMD TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3     TO  WRK-SRYYMD-PRT
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO                TO  CNT-PAGE
           MOVE    ZERO                TO  CNT-MEISAI
      *
           PERFORM 900-TENSU-KEY28-SEL-SEC
      *
           PERFORM UNTIL ( FLG-TENSU   NOT =   ZERO )
      *
               PERFORM 210-EDIT-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key28"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    編集処理
      *****************************************************************
       210-EDIT-SEC                    SECTION.
      *
           MOVE    ZERO            TO  CNT-LINE
      *
           PERFORM 2001-HEAD-SEC
      *
           PERFORM UNTIL ( FLG-TENSU   NOT =   ZERO )
                    OR   ( CNT-LINE       >=   CONST-LINE-MAX )
      *
               PERFORM 2001-BODY-SEC
      *
           END-PERFORM
      *
           PERFORM 2001-PRINT-SEC
      *
           .
       210-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ヘッダ編集処理
      *****************************************************************
       2001-HEAD-SEC                   SECTION.
      *
           INITIALIZE                  HCM68
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
      *
           STRING  WRK-SRYYMD-PRT      DELIMITED   BY  SIZE
                   "経過措置医薬品の置換マスタ一覧"
                                       DELIMITED   BY  SIZE
           INTO    HCM68-TITLE
           END-STRING
      *
      *    MOVE    "廃止前入力コードがある医薬品"
      *                                TO  HCM68-NAIYO
      *
           MOVE    CNT-PAGE            TO  WRK-ZZZZ9
           MOVE    WRK-ZZZZ9           TO  HCM68-PAGE
      *
           MOVE    WRK-SAKYMD-PRT      TO  HCM68-SAKYMD
      *
           .
       2001-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       2001-BODY-SEC                   SECTION.
      *
           COMPUTE CNT-MEISAI  =   CNT-MEISAI  +   1
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
           MOVE    CNT-MEISAI          TO  WRK-ZZZZ9
           MOVE    WRK-ZZZZ9           TO  HCM68-NO (CNT-LINE)
      *
           EVALUATE    TNS-YKZKBN
           WHEN    "1"
               MOVE    "内"            TO  HCM68-YKZKBN (CNT-LINE)
           WHEN    "3"
               MOVE    "他"            TO  HCM68-YKZKBN (CNT-LINE)
           WHEN    "4"
               MOVE    "注"            TO  HCM68-YKZKBN (CNT-LINE)
           WHEN    "6"
               MOVE    "外"            TO  HCM68-YKZKBN (CNT-LINE)
           WHEN    "8"
               MOVE    "歯薬"          TO  HCM68-YKZKBN (CNT-LINE)
           WHEN    "9"
               MOVE    "歯特"          TO  HCM68-YKZKBN (CNT-LINE)
           END-EVALUATE
      *
           MOVE    TNS-SRYCD           TO  HCM68-OLDCD  (CNT-LINE)
           MOVE    TNS-NAME            TO  HCM68-OLDNAME(CNT-LINE)
           MOVE    TNS-TANINAME        TO  HCM68-OLDTANI(CNT-LINE)
           MOVE    TNS-TEN             TO  WRK-TENZ9
           MOVE    WRK-TENZ9           TO  HCM68-OLDTEN (CNT-LINE)
      *    PERFORM 20011-EDIT-TEN-SEC
      *    MOVE    WRK-TENXX           TO  
      *
           PERFORM 900-INPUTCD-KEY4-SEL-SEC
           MOVE    ICD-INPUTCD         TO  HCM68-OLDINPUTCD(CNT-LINE)
      *
           PERFORM 900-SRYCDCHG-KEY2-SEL-SEC
           IF    ( FLG-SRYCDCHG        =   ZERO )
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    TNS-SRYCD       TO  HCM68-NEWCD  (CNT-LINE)
               MOVE    TNS-NAME        TO  HCM68-NEWNAME(CNT-LINE)
               MOVE    TNS-TANINAME    TO  HCM68-NEWTANI(CNT-LINE)
               MOVE    TNS-TEN         TO  WRK-TENZ9
               MOVE    WRK-TENZ9       TO  HCM68-NEWTEN (CNT-LINE)
      *        PERFORM 20011-EDIT-TEN-SEC
      *        MOVE    WRK-TENXX       TO  HCM68-NEWTEN (CNT-LINE)
      *
               PERFORM 900-INPUTCD-KEY4-SEL-SEC
               MOVE    ICD-INPUTCD     TO  HCM68-NEWINPUTCD(CNT-LINE)
           END-IF
      *
           PERFORM 900-TENSU-KEY28-FET-SEC
      *
           .
       2001-BODY-EXT.
           EXIT.
      *****************************************************************
      *    点数変数処理
      *****************************************************************
       20011-EDIT-TEN-SEC              SECTION.
      *
           PERFORM VARYING IDX0    FROM    10  BY  -1
                   UNTIL ( IDX0    <   1 )
      *
               IF    ( WRK-TENXX (IDX0 : 1)    =   "0" )
                       MOVE    SPACE   TO  WRK-TENXX (IDX0 : 1)
               ELSE
                   IF    ( WRK-TENXX (IDX0 : 1)    =   "." )
                       MOVE    SPACE   TO  WRK-TENXX (IDX0 : 1)
                   END-IF
                   MOVE    1           TO  IDX0
               END-IF
      *
           END-PERFORM
      *
           .
       20011-EDIT-TEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       2001-PRINT-SEC                  SECTION.
      *
           MOVE    "HCM68.red"     TO  SPRT-PRTID
           MOVE    HCM68           TO  SPRT-PRTDATA
           MOVE    "1"             TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF    ( SPRT-RETURN         =   ZERO )
               CONTINUE
           ELSE
               DISPLAY "ORCSPRT ERR !!! PTID = " SPA-PTID
               MOVE    -1          TO  ORCHCM68-RC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       2001-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           MOVE    CNT-PAGE    TO  ORCHCM68-OT-CNT
           MOVE    ZERO        TO  ORCHCM68-RC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           INITIALIZE                  WRK-EDTYMD-G
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       900-TENSU-KEY28-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    ORCHCM68-SRYYMD     TO  TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key28"             TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-TENSU-KEY28-FET-SEC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           .
       900-TENSU-KEY28-SEL-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       900-TENSU-KEY28-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-LOOPEND
           MOVE    ZERO            TO  FLG-TENSU
      *
           PERFORM UNTIL ( FLG-TENSU   NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key28"         TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  TNS-TENSU-REC
      *
                   MOVE    1           TO  FLG-LOOPEND
      *
                   IF    ( TNS-SRYCD (1:1) =   "6" )
                       CONTINUE
                   ELSE
                       MOVE    ZERO    TO  FLG-LOOPEND
                   END-IF
      *
               ELSE
                   MOVE    1           TO  FLG-TENSU
                   INITIALIZE              TNS-TENSU-REC
               END-IF
      *
           END-PERFORM
      *
           .
       900-TENSU-KEY28-FET-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    CHG-RJNSRYCD    TO  TNS-SRYCD
           MOVE    SPA-SYSYMD      TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               MOVE    1           TO  FLG-TENSU
               INITIALIZE              TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コード置換検索処理
      *****************************************************************
       900-SRYCDCHG-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYCDCHG
      *
           INITIALIZE                  SRYCDCHG-REC 
           MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
           MOVE    TNS-SRYCD           TO  CHG-IPNSRYCD
           MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
           MOVE    "tbl_srycdchg"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYCDCHG-REC
           ELSE
               INITIALIZE                  SRYCDCHG-REC
               MOVE    1               TO  FLG-SRYCDCHG
           END-IF
      *
           MOVE    "tbl_srycdchg"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SRYCDCHG-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    自院コード検索処理
      *****************************************************************
       900-INPUTCD-KEY4-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-INPUTCD
      *
           INITIALIZE                  INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    TNS-SRYCD           TO  ICD-SRYCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  INPUTCD-REC
           ELSE
               MOVE    1               TO  FLG-INPUTCD
               INITIALIZE                  INPUTCD-REC
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-INPUTCD-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC          SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF        ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       911-DBSELECT-SEC          SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       910-DBFETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           DISPLAY "ORCHCM68-RC [" ORCHCM68-RC "]"
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
