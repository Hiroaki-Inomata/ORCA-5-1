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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCVTSRYKARRK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＤＢ作成処理
      *  コンポーネント名  : ＣＳＶファイルコンバート処理（診療科履歴・
      *                                                    初診算定日）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/01/24    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-森脇    05/07/13  エラーメッセージ、コミット件数
      *  01.00.02    NACL-森脇    05/09/28  構造変更
      *  01.00.03    NACL-森脇    06/07/26  MONFUNC対応
      *  03.05.00    NACL-多々納  07/05/16  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメータファイル
           SELECT  PARA-FILE       ASSIGN
                                   ASS-PARA
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PARA.
      *
      *    入力受診日情報ＣＳＶファイル
           SELECT  CSV-FILE        ASSIGN
                                   ASS-CSV
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-CSV.
      *
      *    出力エラーファイル
           SELECT  ERR-FILE        ASSIGN
                                   ASS-ERR
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-LST.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *    パラメータファイル
       FD  PARA-FILE.
       01  PARA-REC.
           03  PARA-RECKBN             PIC X(01).
           03  PARA-DATKBN             PIC X(04).
           03  FILLER                  PIC X(01).
           03  PARA-DATA               PIC X(100).
      *
      *    入力患者保険情報ＣＳＶファイル
       FD  CSV-FILE.
       01  CSV-REC.
           03  CSV-R                   PIC X(300).
           03  CSV-RR  REDEFINES       CSV-R.
               05  CSV-X               PIC X(01)  OCCURS   300.
      *
      *    出力エラーファイル
       FD  ERR-FILE.
       01  ERR-REC                     PIC X(100).
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    ファイル指定領域
       01  ASS-AREA.
           03  ASS-PARA                PIC X(256).
           03  ASS-CSV                 PIC X(256).
           03  ASS-ERR                 PIC X(256).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA                PIC X(02).
           03  STS-CSV                 PIC X(02).
           03  STS-LST                 PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PARA                PIC 9(01).
           03  FLG-CSV                 PIC 9(01).
      *
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-SRYKARRK            PIC 9(01).
           03  FLG-SANTEI              PIC 9(01).
      *
           03  FLG-ERRARI              PIC 9(01).
           03  FLG-DATAEND             PIC 9(01).
           03  FLG-AREA1.
               05  FLG-ERR             PIC 9(01).
               05  FLG-COMMA           PIC 9(01).
               05  FLG-DELI1           PIC 9(01).
               05  FLG-DELI2           PIC 9(01).
               05  FLG-NODATA          PIC 9(01).
      *
           03  FLG-YMD                 PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PARA                PIC 9(06).
           03  CNT-CSV                 PIC 9(06).
           03  CNT-ERR                 PIC 9(06).
           03  CNT-COMMIT              PIC 9(06).
           03  CNT-SRYKARRK            PIC 9(06).
           03  CNT-SANTEI              PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDE                     PIC 9(04).
           03  IDS                     PIC 9(04).
           03  IDI                     PIC 9(04).
           03  IDJ                     PIC 9(02).
      *
      *    パラメータ保存
       01  PARA-SAVE.
           03  PARA-05-1               PIC X(02).
           03  PARA-09-6               PIC X(06).
           03  PARA-11-6               PIC X(06).
           03  PARA-HOSPNUM            PIC X(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYMD2.
               05  WRK-SYY2            PIC 9(02).
               05  WRK-SMM2            PIC 9(02).
               05  WRK-SDD2            PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).
               05  WRK-SYSDD           PIC 9(02).
      *
           03  WRK-POINT-AREA.
               05  WRK-PO-ST           PIC 9(04).
               05  WRK-PO-ED           PIC 9(04).
           03  WRK-MOVE-AREA.
               05  WRK-MO-ST           PIC 9(04).
               05  WRK-MO-CNT          PIC 9(04).
               05  WRK-WK              PIC 9(04).
           03  WRK-DATA                PIC X(100).
           03  WRK-LEN                 PIC 9(03).
           03  WRK-CNT                 PIC 9(04). 
      *
           03  WRK-RENKETA             PIC 9(02).
           03  WRK-COMMIT              PIC 9(06).
           03  WRK-ERR                 PIC 9(06).
      *
           03  WRK-NUM1                PIC 9(01).
           03  WRK-NUM2                PIC 9(02).
           03  WRK-KOUMOKU-NO          PIC 9(04).
      *
       01  WRK-HEN-AREA.
           03  WRK-HEN-SRYKA           PIC X(02).
           03  WRK-HEN-SRYKA-9         REDEFINES   WRK-HEN-SRYKA
                                       PIC 9(02).
           03  WRK-HEN-LASTYMD         PIC X(08).
           03  WRK-HEN-SYOSINYMD       PIC X(08).
           03  WRK-HEN-PTNUM           PIC X(20).
           03  WRK-HEN-SRYCD           PIC X(09).
      *
       01  TBL-KENSAKU-AREA.
           03  TBL-KENSAKU-TBL     OCCURS   3.
               05  TBL-SRYKA           PIC X(02).
               05  TBL-SRYKA-9         REDEFINES   TBL-SRYKA
                                           PIC 9(02).
               05  TBL-LASTYMD         PIC X(08).
               05  TBL-SYOSINYMD       PIC X(08).
      *
      *    帳票出力領域
       01  MES-AREA.
           03  MES-TITLE1.
               05  FILLER              PIC X(26) VALUE
                                       "【PGID:ORCVTSRYKARRK.CBL】".
               05  MES-TITLE1-YY       PIC X(04) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE ".".
               05  MES-TITLE1-MM       PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE ".".
               05  MES-TITLE1-DD       PIC X(02) VALUE SPACE.
           03  MES-TITLE2.
               05  MES-TITLE2-L1.
                   07  FILLER          PIC X(10) VALUE "LINENO ERR".
                   07  FILLER          PIC X(42) VALUE
                   " [1]必須未入力        [2]項目桁数不正     ".
                   07  FILLER          PIC X(10) VALUE " [3]誤入力".
               05  MES-TITLE2-L2.
                   07  FILLER          PIC X(10) VALUE SPACE.
                   07  FILLER          PIC X(42) VALUE
                   " [5]診療日付未設定    [6]患者番号取得失敗 ".
                   07  FILLER          PIC X(44) VALUE
                   " [7]算定履歴出力失敗  [8]診療科履歴出力失敗".
               05  MES-TITLE2-L3.
                   07  FILLER          PIC X(50) VALUE
                   "--------------------------------------------------".
                   07  FILLER          PIC X(49) VALUE
                   "-------------------------------------------------".
      *
       01  ERRLST-AREA.
      *    エラー番号１・２・３　で使用
           03  ERR-REC1.
               05  ERR1-LINENO         PIC X(06).
               05  FILLER              PIC X(01).
               05  ERR1-NAIYO          PIC X(03).
               05  ERR-OCCURS          OCCURS   4.
                   07  FILLER          PIC X(01).
                   07  ERR1-ERRNO      PIC X(03).
                   07  ERR1-KOMOKU     PIC X(09).
                   07  ERR1-ATAI       PIC X(08).
      *    エラー番号４・５・６　で使用
           03  ERR-REC2.
               05  ERR2-LINENO         PIC X(06).
               05  FILLER              PIC X(01).
               05  ERR2-NAIYO          PIC X(03).
               05  FILLER              PIC X(01).
               05  ERR2-ERRNO          PIC X(03).
               05  ERR2-PTNUMMES       PIC X(09).
               05  ERR2-PTNUM          PIC X(10).
               05  FILLER              PIC X(02).
               05  ERR2-PTIDMES        PIC X(09).
               05  ERR2-PTID           PIC X(10).
               05  FILLER              PIC X(02).
               05  ERR2-SRYKAMES       PIC X(07).
               05  ERR2-SRYKA          PIC X(02).
               05  FILLER              PIC X(01).
               05  ERR2-SYOSINMES      PIC X(07).
               05  ERR2-SYOSINYMD      PIC X(08).
               05  FILLER              PIC X(01).
               05  ERR2-LASTMES        PIC X(07).
               05  ERR2-LASTYMD        PIC X(08).
      *
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
      *    システム管理（医療機関情報）
           COPY    "CPSK1001.INC".
      *    システム管理（患者番号構成管理）
           COPY    "CPSK1009.INC".
      *    診療科履歴
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *    LIKE使用KEY対応
           COPY    "CPORCSADDSIGN.INC".
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
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-CSV     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           MOVE    ZERO            TO  FLG-AREA
           MOVE    ZERO            TO  CNT-AREA
           MOVE    ZERO            TO  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
      *
      *    パラメタセット
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    ASS-PARA
           END-UNSTRING
      *
      *    パラメータ情報取得
           PERFORM 101-PARA-GET-SEC
           IF  FLG-PARA            =   2
               MOVE    1           TO  FLG-CSV
               GO  TO  100-INIT-EXT
           END-IF
      *    医療機関番号
           IF      PARA-HOSPNUM    NUMERIC
               MOVE    PARA-HOSPNUM    TO  SPA-HOSPNUM
           ELSE
               MOVE    1               TO  FLG-CSV
               DISPLAY "*(ORCVTSRYKARRK)* HOSPNUM ERR [" PARA-HOSPNUM"]"
               GO  TO  100-INIT-EXT
           END-IF
      *
           ACCEPT  WRK-SYMD2       FROM    DATE
           MOVE    WRK-SYMD2       TO  WRK-SYSYMD (3:)
           ADD     2000            TO  WRK-SYSYY
           MOVE    WRK-SYSYMD      TO  SPA-SYSYMD
           MOVE    SPA-SYSYMD(1:4) TO  MES-TITLE1-YY
           MOVE    SPA-SYSYMD(5:2) TO  MES-TITLE1-MM
           MOVE    SPA-SYSYMD(7:2) TO  MES-TITLE1-DD
      *    端末ＩＤ固定
           MOVE    "ORCATERM"      TO  SPA-TERMID
      *
      *    データベースオープン
           PERFORM 100-DBOPEN-SEC
      *
      *    システム管理情報　検索処理
           PERFORM 120-CP1001-KENSAKU-SEC
           IF  FLG-CSV             =   1
               GO  TO  100-INIT-EXT
           END-IF
      *
           PERFORM 120-CP1009-KENSAKU-SEC
           IF  FLG-CSV             =   1
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    ＣＳＶ入力ファイル　初期処理
           OPEN    INPUT           CSV-FILE
           IF  STS-CSV         NOT =   ZERO
               MOVE    1           TO  FLG-CSV
               DISPLAY "*(ORCVTSRYKARRK)* PTHKN-FILE OPN STS["
                                       STS-CSV "]"
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    帳票出力　初期処理
           PERFORM 2710-ERRLST-INIT-SEC
           IF  FLG-CSV             =   1
               GO  TO  100-INIT-EXT
           END-IF
      *
           MOVE    PARA-05-1       TO  WRK-RENKETA(1:2)
           MOVE    PARA-09-6       TO  WRK-COMMIT
           MOVE    PARA-11-6       TO  WRK-ERR
      *
      *    ＣＳＶファイル　検索処理
           PERFORM 900-CSV-KENSAKU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメータ情報取得
      *****************************************************************
       101-PARA-GET-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-PARA
           OPEN    INPUT           PARA-FILE
           IF      STS-PARA        NOT =   ZERO
               MOVE    2               TO  FLG-PARA
               DISPLAY "*(ORCVTSRYKARRK)* PARA-FILE OPN STS["
                       STS-PARA "]"
               GO  TO  101-PARA-GET-EXT
           END-IF
      *
           PERFORM 900-PARA-READ-SEC
      *
           PERFORM UNTIL    FLG-PARA  =  1
               IF  PARA-RECKBN         =   "@"
                   PERFORM 101-PARA-GET1-SEC
               END-IF
               PERFORM  900-PARA-READ-SEC
           END-PERFORM
      *
           CLOSE   PARA-FILE
      *
      *    パラメータチェック処理
           IF    ( PARA-05-1           IS  NUMERIC )   AND
                 ( PARA-09-6           IS  NUMERIC )   AND
                 ( PARA-11-6           IS  NUMERIC )
               CONTINUE
           ELSE
               MOVE    1               TO  FLG-CSV
               GO  TO  101-PARA-GET-EXT
           END-IF
      *
           .
       101-PARA-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメータ情報取得（１）
      *****************************************************************
       101-PARA-GET1-SEC           SECTION.
      *
           EVALUATE                PARA-DATKBN
             WHEN  "01-6"
                   MOVE  PARA-DATA TO  ASS-CSV
             WHEN  "02-6"
                   MOVE  PARA-DATA TO  ASS-ERR
             WHEN  "05-1"
                   MOVE  PARA-DATA TO  PARA-05-1
             WHEN  "09-6"
                   MOVE  PARA-DATA TO  PARA-09-6
             WHEN  "11-6"
                   MOVE  PARA-DATA TO  PARA-11-6
               WHEN    "HN"
                   MOVE    PARA-DATA     TO  PARA-HOSPNUM
           END-EVALUATE
      *
           .
       101-PARA-GET1-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理情報　検索処理
      *****************************************************************
       120-CP1001-KENSAKU-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
           MOVE    SPACE           TO  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    WRK-SYSYMD      TO  SYS-1001-STYUKYMD
                                       SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF  FLG-SYSKANRI            =   1
               MOVE    1               TO  FLG-CSV
           ELSE
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               MOVE    SYS-1001-HOSPID     TO  SPA-HOSPID
               MOVE    SYS-1001-HOSPSBT    TO  SPA-HOSPSBT
               MOVE    SYS-1001-ROUPAYKBN  TO  SPA-ROUPAYKBN
           END-IF
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       120-CP1001-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理情報　検索処理
      *****************************************************************
       120-CP1009-KENSAKU-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
           MOVE    SPACE           TO  SYS-1009-REC
           MOVE    "1009"          TO  SYS-1009-KANRICD
           MOVE    "*"             TO  SYS-1009-KBNCD
           MOVE    WRK-SYSYMD      TO  SYS-1009-STYUKYMD
                                       SYS-1009-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF  FLG-SYSKANRI            =   1
               MOVE    1               TO  FLG-CSV
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-1009-REC
           END-IF
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       120-CP1009-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF  CNT-CSV (4:3)       =   ZERO
               DISPLAY "***(ORCVTSRYKARRK)*** CNT-CSV[" CNT-CSV "]"
           END-IF
      *    指定した件数になったらコミット処理を行う
           IF  CNT-COMMIT          =   WRK-COMMIT
               PERFORM 900-DBCOMMIT-SEC
               MOVE    ZERO            TO  CNT-COMMIT
           END-IF
      *    エラーが指定した件数になったら処理を抜ける
           IF    ( WRK-ERR         NOT =   ZERO )  AND
                 ( CNT-ERR             =   WRK-ERR )
               MOVE    1               TO  FLG-CSV
               GO  TO  200-MAIN-EXT
           END-IF
      *
           MOVE    ZERO            TO  WRK-POINT-AREA
           MOVE    ZERO            TO  IDX-AREA
           MOVE    SPACE           TO  SPA-PTNUM
           MOVE    ZERO            TO  SPA-PTID
           MOVE    SPACE           TO  WRK-HEN-AREA
           INITIALIZE                  WRK-HEN-AREA
           INITIALIZE                  TBL-KENSAKU-AREA
      *    対象患者にエラーが存在するかどうか
           MOVE    ZERO            TO  FLG-ERRARI
           MOVE    ZERO            TO  FLG-DATAEND
           MOVE    ZERO            TO  WRK-KOUMOKU-NO
      *
      *    患者保険ＣＳＶファイルに存在する項目数分、繰り返す
      *    またはレコード終了、または項目エラー４個まで
           PERFORM VARYING   IDX   FROM    1   BY  1
                   UNTIL   ( IDX           >   10   )
                   OR      ( WRK-PO-ED     >=  300  )
                   OR      ( IDE           >   4    )
      *
               COMPUTE WRK-PO-ST   =   WRK-PO-ED   +   1
               MOVE    ZERO            TO  FLG-AREA1
      *
               IF  ( CSV-R (WRK-PO-ST:)    =   SPACE )  OR
                   ( FLG-DATAEND           =   1     )
                   MOVE    1           TO  FLG-DATAEND
                   MOVE    WRK-PO-ST   TO  WRK-PO-ED
               ELSE
                   ADD     1           TO  WRK-KOUMOKU-NO
      *            項目終了文字後のカンマ位置を取得(WRK-PO-ED)
                   PERFORM 210-CHAR-END-GET-SEC
               END-IF
      *        項目共通処理
               PERFORM 220-KOUMOKU-KYOTU-SEC
      *        項目別処理
               IF      FLG-DATAEND     =   ZERO
                   PERFORM 240-KOUMOKU-BETU-SEC
               END-IF
           END-PERFORM
      *
           IF  FLG-ERRARI          =   ZERO
      *        マスタ更新処理
      *         PERFORM 220-KOUMOKU-KYOTU-SEC
               PERFORM 250-KOUSIN-SYORI-SEC
               IF  FLG-ERR         NOT =   ZERO
      *            エラーリスト編集/出力処理
                   PERFORM 270-ERRREC-EDIT-SEC
                   MOVE    2           TO  FLG-ERRARI
                   PERFORM 270-ERRLST-OUT-SEC
               END-IF
           ELSE
      *        エラーリスト出力処理
               PERFORM 270-ERRLST-OUT-SEC
           END-IF
      *
      *    ＣＳＶファイル　検索処理
           PERFORM 900-CSV-KENSAKU-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象項目の文字終了位置を取得
      *****************************************************************
       210-CHAR-END-GET-SEC        SECTION.
      *
           MOVE    WRK-PO-ST       TO  WRK-PO-ED
           PERFORM VARYING   IDY   FROM    WRK-PO-ST   BY  1
                   UNTIL   ( IDY           >=  300  )
                   OR      ( FLG-COMMA     =   1    )
      *        カンマの位置
               IF  ( CSV-X (IDY)       =   ","   )  OR
                   ((IDX               =   10   )  AND
                    (CSV-X (IDY)       =   SPACE))
                   MOVE    IDY         TO  WRK-PO-ED
                   MOVE    1           TO  FLG-COMMA
               ELSE
      *            カンマが足りない場合の終了位置決定
                   IF      CSV-X (IDY) NOT =   SPACE
                       MOVE    IDY         TO  WRK-PO-ED
                   END-IF
               END-IF
      *        デリミタ(")存在するかどうか
               IF  CSV-X (IDY)         =   """"
                   IF  FLG-DELI1       =   ZERO
                       MOVE    1       TO  FLG-DELI1
                   ELSE
                       MOVE    1       TO  FLG-DELI2
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       210-CHAR-END-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目共通処理
      *****************************************************************
       220-KOUMOKU-KYOTU-SEC       SECTION.
      *
           MOVE    ZERO            TO  WRK-MOVE-AREA
      *
      *    データが省略されている場合、FLG-NODATAに1をセット
           IF  FLG-DELI1           =   1
      *        デリミタ(")を含んでいる場合
               IF  FLG-DELI2           =   1
                   COMPUTE WRK-WK      =   WRK-PO-ST   +   2
               ELSE
                   COMPUTE WRK-WK      =   WRK-PO-ST   +   1
               END-IF
               IF  WRK-WK              =   WRK-PO-ED
                   MOVE    1           TO  FLG-NODATA
                   GO  TO  220-KOUMOKU-KYOTU-EXT
               END-IF
           ELSE
      *        デリミタ(")を含んでいない場合
               IF  WRK-PO-ST           =   WRK-PO-ED
                   MOVE    1           TO  FLG-NODATA
                   GO  TO  220-KOUMOKU-KYOTU-EXT
               END-IF
           END-IF
      *
      *    転送開始位置・転送桁数を算出
           IF  FLG-DELI1               =   1
      *        デリミタ(")を含んでいる場合
               COMPUTE   WRK-MO-ST     =   WRK-PO-ST   +   1
               IF  FLG-DELI2           =   1
                   COMPUTE WRK-MO-CNT  =   WRK-PO-ED
                                       -  (WRK-PO-ST   +   2)
               ELSE
                   COMPUTE WRK-MO-CNT  =  (WRK-PO-ED   -   1)
                                       -   WRK-PO-ST
               END-IF
           ELSE
      *        デリミタ(")を含んでいない場合
               MOVE    WRK-PO-ST       TO  WRK-MO-ST
               COMPUTE WRK-MO-CNT  =   WRK-PO-ED  -  WRK-PO-ST
           END-IF
      *
           .
       220-KOUMOKU-KYOTU-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目別処理
      *****************************************************************
       240-KOUMOKU-BETU-SEC        SECTION.
      *
           EVALUATE    WRK-KOUMOKU-NO
      *    １：患者番号
           WHEN    1
               PERFORM 2410-PTNUM-SET-SEC
      *    ２：診療科
           WHEN    2
               MOVE    1               TO  IDI
               PERFORM 2410-SRYKA-SET-SEC
      *    ３：前回受診歴１
           WHEN    3
               MOVE    1               TO  IDI
               PERFORM 2410-LASTYMD-SET-SEC
      *    ４：初診算定日
           WHEN    4
               MOVE    1               TO  IDI
               PERFORM 2410-SYOSINYMD-SET-SEC
      *    ５：診療科
           WHEN    5
               MOVE    2               TO  IDI
               PERFORM 2410-SRYKA-SET-SEC
      *    ６：前回受診歴１
           WHEN    6
               MOVE    2               TO  IDI
               PERFORM 2410-LASTYMD-SET-SEC
      *    ７：初診算定日
           WHEN    7
               MOVE    2               TO  IDI
               PERFORM 2410-SYOSINYMD-SET-SEC
      *    ８：診療科
           WHEN    8
               MOVE    3               TO  IDI
               PERFORM 2410-SRYKA-SET-SEC
      *    ９：前回受診歴１
           WHEN    9
               MOVE    3               TO  IDI
               PERFORM 2410-LASTYMD-SET-SEC
      *    10：初診算定日
           WHEN    10
               MOVE    3               TO  IDI
               PERFORM 2410-SYOSINYMD-SET-SEC
           END-EVALUATE
      *
           IF  FLG-ERR         NOT =   ZERO
               MOVE    1           TO  FLG-ERRARI
               PERFORM 270-ERRREC-EDIT-SEC
           END-IF
      *
           .
       240-KOUMOKU-BETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１：患者番号　処理
      *****************************************************************
       2410-PTNUM-SET-SEC          SECTION.
      *
           MOVE    SPACE           TO  SPA-PTNUM
      *
           IF  FLG-NODATA          =   1
               MOVE    1               TO  FLG-ERR
               GO  TO  2410-PTNUM-SET-EXT
           END-IF
      *
      *    空白までを編集する
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  WRK-HEN-PTNUM
           MOVE    ZERO                TO  WRK-CNT
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL ( IDZ >       WRK-MO-CNT )
                   OR    ( WRK-HEN-PTNUM (IDZ:1)  =   SPACE )
               MOVE    IDZ             TO    WRK-CNT
           END-PERFORM
           MOVE    WRK-CNT             TO  WRK-MO-CNT
      *
           IF    ( WRK-MO-CNT          =   0  ) OR
                 ( WRK-MO-CNT          >   20 )
               MOVE    2               TO  FLG-ERR
               GO  TO  2410-PTNUM-SET-EXT
           END-IF
      *
           IF     (( SYS-1009-PTNUMKSIKBN  =   "1" ) AND
                   ( SYS-1009-JIYKSIKBN    =   "2" ))    OR
                  (( SYS-1009-PTNUMKSIKBN  =   "2" ) AND
                   ( SYS-1009-HJNKSIKBN    =   "3" OR "4" ))
               MOVE    ZERO            TO  SPA-PTNUM(1:WRK-RENKETA)
               COMPUTE IDZ             =   WRK-RENKETA
                                       -   WRK-MO-CNT
                                       +   1
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  SPA-PTNUM(IDZ:)
           ELSE
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  SPA-PTNUM
           END-IF
      *
           .
       2410-PTNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２：診療科処理
      *****************************************************************
       2410-SRYKA-SET-SEC          SECTION.
      *
           IF  FLG-NODATA          =   1
               IF      WRK-KOUMOKU-NO  =   2
                   MOVE    1               TO  FLG-ERR
               ELSE
                   MOVE    SPACE           TO  TBL-SRYKA(IDI)
               END-IF
               GO  TO  2410-SRYKA-SET-EXT
           END-IF
      *
           IF     (WRK-MO-CNT      >   2 )
               MOVE    2                   TO  FLG-ERR
               GO  TO  2410-SRYKA-SET-EXT
           END-IF
           IF     (CSV-REC (WRK-MO-ST:WRK-MO-CNT) NOT NUMERIC)
               MOVE    3                   TO  FLG-ERR
               GO  TO  2410-SRYKA-SET-EXT
           END-IF
      *
           IF      WRK-MO-CNT      =   1
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO  WRK-NUM1
               MOVE    WRK-NUM1            TO  TBL-SRYKA-9(IDI)
           ELSE
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO  WRK-NUM2
               MOVE    WRK-NUM2            TO  TBL-SRYKA(IDI)
           END-IF
      *
           .
       2410-SRYKA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目３：最終受診日処理
      *****************************************************************
       2410-LASTYMD-SET-SEC        SECTION.
      *
           IF  FLG-NODATA          =   1
               MOVE    SPACE               TO  TBL-LASTYMD(IDI)
               GO  TO  2410-LASTYMD-SET-EXT
           END-IF
      *
           IF      WRK-MO-CNT      NOT =   8
               MOVE    2                   TO  FLG-ERR
               GO  TO  2410-LASTYMD-SET-EXT
           END-IF
      *
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-SYMD
           IF     (WRK-SYMD        NOT NUMERIC )  OR
                  (WRK-SYMD        =   ALL "9")
               MOVE    3               TO  FLG-ERR
               GO  TO  2410-LASTYMD-SET-EXT
           END-IF
      *
           PERFORM 31012-SEIWA-HEN-SEC
           IF      FLG-YMD         =   1
               MOVE    3               TO  FLG-ERR
           ELSE
               MOVE    WRK-SYMD        TO  TBL-LASTYMD(IDI)
           END-IF
      *
           .
       2410-LASTYMD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目３：初診算定日
      *****************************************************************
       2410-SYOSINYMD-SET-SEC      SECTION.
      *
           IF  FLG-NODATA          =   1
               MOVE    SPACE               TO  TBL-SYOSINYMD(IDI)
               GO  TO  2410-SYOSINYMD-SET-EXT
           END-IF
      *
           IF      WRK-MO-CNT      NOT =   8
               MOVE    2                   TO  FLG-ERR
               GO  TO  2410-SYOSINYMD-SET-EXT
           END-IF
      *
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-SYMD
           IF     (WRK-SYMD        NOT NUMERIC )  OR
                  (WRK-SYMD        =   ALL "9")
               MOVE    3               TO  FLG-ERR
               GO  TO  2410-SYOSINYMD-SET-EXT
           END-IF
      *
           PERFORM 31012-SEIWA-HEN-SEC
           IF      FLG-YMD         =   1
               MOVE    3               TO  FLG-ERR
           ELSE
               MOVE    WRK-SYMD        TO  TBL-SYOSINYMD(IDI)
           END-IF
      *
           .
       2410-SYOSINYMD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    マスタ更新処理
      *****************************************************************
       250-KOUSIN-SYORI-SEC        SECTION.
      *
      *    患者番号より患者ＩＤを求める
           PERFORM 900-PTNUM-READ-SEC
           IF      FLG-PTNUM           =   1
                MOVE    6              TO  FLG-ERR
                GO  TO  250-KOUSIN-SYORI-EXT
           END-IF
      *
           MOVE    PTNUM-PTID      TO  SPA-PTID
      *
           PERFORM VARYING     IDI   FROM    1   BY  1
                   UNTIL       IDI   >   3
               IF      TBL-SRYKA (IDI)   NOT =   SPACE
                   IF      TBL-SYOSINYMD(IDI)    =   SPACE
                       MOVE    TBL-LASTYMD(IDI)
                                           TO  TBL-SYOSINYMD(IDI)
                   END-IF
                   IF      TBL-LASTYMD (IDI)     >   WRK-HEN-LASTYMD
                       MOVE    TBL-SRYKA (IDI)   TO  WRK-HEN-SRYKA
                       MOVE    TBL-LASTYMD(IDI)  TO  WRK-HEN-LASTYMD
                       MOVE    TBL-SYOSINYMD(IDI)
                                                 TO  WRK-HEN-SYOSINYMD
                   END-IF
               END-IF
           END-PERFORM
      *
      *    日付がない時、作成しない
           IF     (WRK-HEN-LASTYMD         =   SPACE )  AND
                  (WRK-HEN-SYOSINYMD       =   SPACE )
               MOVE    5               TO  FLG-ERR
               GO  TO  250-KOUSIN-SYORI-EXT
           END-IF
      *    初診日が最終受診日より後の時
           IF      WRK-HEN-SYOSINYMD       >   WRK-HEN-LASTYMD
               MOVE    WRK-HEN-SYOSINYMD       TO  WRK-HEN-LASTYMD
           END-IF
      *
      *    診療科履歴作成
           PERFORM 260-SRYKARRK-INSERT-SEC
           IF      FLG-ERR         NOT =   ZERO
               GO  TO  250-KOUSIN-SYORI-EXT
           END-IF
      *    初診算定日
           IF      WRK-HEN-SYOSINYMD   NOT =   SPACE
               PERFORM 270-SANTEIRRK-INSERT-SEC
           END-IF
           .
       250-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴作成登録処理
      *****************************************************************
       260-SRYKARRK-INSERT-SEC     SECTION.
      *
      *    診療科履歴読込
           PERFORM 910-SRYKARRK-READ-SEC
           IF      FLG-SRYKARRK        =   ZERO
               MOVE    8               TO  FLG-ERR
               GO  TO  260-SRYKARRK-INSERT-EXT
           END-IF
      *    新規作成
           MOVE    SPACE           TO  SRYKARRK-REC
           INITIALIZE                  SRYKARRK-REC
           MOVE    SPA-HOSPNUM     TO  KARRK-HOSPNUM
           MOVE    SPA-PTID        TO  KARRK-PTID
           MOVE    WRK-HEN-SRYKA   TO  KARRK-SRYKA
      *    最終受診日
           MOVE    WRK-HEN-LASTYMD TO  KARRK-LASTYMD
      *    初診日１
           MOVE    WRK-HEN-SYOSINYMD   TO  KARRK-SYOSINYMD1
      *    初診日２
           MOVE    WRK-HEN-SYOSINYMD   TO  KARRK-SYOSINYMD2
      *
      *    診療科履歴作成
           MOVE    SRYKARRK-REC    TO  MCPDATA-REC
           MOVE    "DBINSERT"      TO  MCP-FUNC
           MOVE    "tbl_srykarrk"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
                MOVE    8              TO  FLG-ERR
                GO  TO  260-SRYKARRK-INSERT-EXT
           END-IF
           ADD     1               TO  CNT-SRYKARRK
      *
           .
       260-SRYKARRK-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診算定履歴登録処理
      *****************************************************************
       270-SANTEIRRK-INSERT-SEC    SECTION.
      *
      *    仮の初診料
           MOVE    "099110001"     TO  WRK-HEN-SRYCD
      *
      *    算定履歴読み込み
           PERFORM 920-SANTEI-READ-SEC
           IF      FLG-SANTEI          =   ZERO
               MOVE    7               TO  FLG-ERR
               GO  TO  270-SANTEIRRK-INSERT-EXT
           END-IF
      *
      *    作成
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-HEN-SYOSINYMD(1:6)  TO  SANTEI-SRYYM
           MOVE    WRK-HEN-SRYCD       TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
      *    初回算定日
           MOVE    WRK-HEN-SYOSINYMD   TO  SANTEI-FSANTEIYMD
           MOVE    WRK-HEN-SYOSINYMD(7:2)  TO  SANTEI-MSANTEID
      *    当月算定歴回数
           MOVE    1                   TO  SANTEI-MSANTEICNT
      *    当日算定日
           MOVE    WRK-HEN-SYOSINYMD(7:2)  TO  IDJ
           MOVE    1                   TO  SANTEI-DAY (IDJ)
      *
           MOVE    SANTEI-REC      TO  MCPDATA-REC
           MOVE    "DBINSERT"      TO  MCP-FUNC
           MOVE    "tbl_santei"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
                MOVE    7              TO  FLG-ERR
                GO  TO  270-SANTEIRRK-INSERT-EXT
           END-IF
           ADD     1               TO  CNT-SANTEI
      *
           .
       270-SANTEIRRK-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーリスト編集処理
      *****************************************************************
       270-ERRREC-EDIT-SEC         SECTION.
      *
           IF  FLG-ERR             <=  3
               ADD     1               TO  IDE
               IF  IDE                 >   4
                   GO  TO  270-ERRREC-EDIT-EXT
               END-IF
               IF  IDE                     =   1
                   ADD     1               TO  CNT-ERR
                   MOVE    SPACE           TO  ERR-REC1
                   MOVE    CNT-CSV         TO  ERR1-LINENO
                   MOVE    "ERR"           TO  ERR1-NAIYO
               END-IF
           END-IF
      *
           EVALUATE    FLG-ERR
      *    必須項目未入力エラー
           WHEN    1
      *    項目桁数エラー
           WHEN    2
      *    誤入力エラー
           WHEN    3
               PERFORM 2910-ERRREC-EDIT-BETU1-SEC
      *    診療日付なしエラー
           WHEN    5
      *    患者番号エラー
           WHEN    6
      *    算定履歴出力エラー
           WHEN    7
      *    診療科履歴出力エラー
           WHEN    8
               PERFORM 2910-ERRREC-EDIT-BETU2-SEC
           END-EVALUATE
      *
           .
       270-ERRREC-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーリスト編集　別１　処理
      *****************************************************************
       2910-ERRREC-EDIT-BETU1-SEC  SECTION.
      *
      *    エラー内容番号転送
           MOVE    "["             TO  ERR1-ERRNO(IDE)(1:1)
           MOVE    FLG-ERR (1:1)   TO  ERR1-ERRNO(IDE)(2:1)
           MOVE    "]"             TO  ERR1-ERRNO(IDE)(3:1)
      *
      *    エラー項目名称転送
           EVALUATE    WRK-KOUMOKU-NO
      *    １：患者番号
           WHEN    1
               MOVE    "患者番号:"     TO  ERR1-KOMOKU(IDE)
      *    ２：診療科１
           WHEN    2
               MOVE    "診療科１:"     TO  ERR1-KOMOKU(IDE)
      *    ３：最終日１
           WHEN    3
               MOVE    "最終日１:"     TO  ERR1-KOMOKU(IDE)
      *    ４：初診日１
           WHEN    4
               MOVE    "初診日１:"     TO  ERR1-KOMOKU(IDE)
      *    ５：診療科２
           WHEN    5
               MOVE    "診療科２:"     TO  ERR1-KOMOKU(IDE)
      *    ６：最終日２
           WHEN    6
               MOVE    "最終日２:"     TO  ERR1-KOMOKU(IDE)
      *    ７：初診日２
           WHEN    7
               MOVE    "初診日２:"     TO  ERR1-KOMOKU(IDE)
      *    ８：診療科３
           WHEN    8
               MOVE    "診療科３:"     TO  ERR1-KOMOKU(IDE)
      *    ９：最終日３
           WHEN    9
               MOVE    "最終日３:"     TO  ERR1-KOMOKU(IDE)
      *    １０：初診日３
           WHEN    10
               MOVE    "初診日３:"     TO  ERR1-KOMOKU(IDE)
           END-EVALUATE
      *
      *    エラー値転送
           IF  WRK-MO-CNT          NOT =   ZERO
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  ERR1-ATAI  (IDE)(1:8)
           END-IF
      *
           .
       2910-ERRREC-EDIT-BETU1-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーリスト編集　別２　処理
      *****************************************************************
       2910-ERRREC-EDIT-BETU2-SEC  SECTION.
      *
           ADD     1               TO  CNT-ERR
           MOVE    SPACE           TO  ERR-REC2
           MOVE    CNT-CSV         TO  ERR2-LINENO
           MOVE    "["             TO  ERR2-ERRNO(1:1)
           MOVE    FLG-ERR(1:1)    TO  ERR2-ERRNO(2:1)
           MOVE    "]"             TO  ERR2-ERRNO(3:1)
           MOVE    "患者番号:"     TO  ERR2-PTNUMMES
           MOVE    "患者ＩＤ:"     TO  ERR2-PTIDMES
           MOVE    "診療科:"       TO  ERR2-SRYKAMES
           MOVE    "初診日:"       TO  ERR2-SYOSINMES
           MOVE    "最終日:"       TO  ERR2-LASTMES
      *
           MOVE    SPA-PTNUM       TO  ERR2-PTNUM
           MOVE    SPA-PTID        TO  ERR2-PTID
           MOVE    WRK-HEN-SRYKA       TO  ERR2-SRYKA
           MOVE    WRK-HEN-LASTYMD     TO  ERR2-LASTYMD
           MOVE    WRK-HEN-SYOSINYMD   TO  ERR2-SYOSINYMD
           MOVE    "ERR"           TO  ERR2-NAIYO
      *
           .
       2910-ERRREC-EDIT-BETU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーリスト出力処理
      *****************************************************************
       270-ERRLST-OUT-SEC          SECTION.
      *
           IF  FLG-ERRARI          =   1
               MOVE    ERR-REC1        TO      ERR-REC
               WRITE   ERR-REC
           ELSE
               MOVE    ERR-REC2        TO      ERR-REC
               WRITE   ERR-REC
           END-IF
      *
           .
       270-ERRLST-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力　初期処理
      *****************************************************************
       2710-ERRLST-INIT-SEC        SECTION.
      *
           OPEN    OUTPUT              ERR-FILE
           IF  STS-LST             NOT =   ZERO
               MOVE    1               TO  FLG-CSV
               DISPLAY "*(ORCVTSRYKARRK)* ERRFILE OPN STS[" STS-LST "]"
               GO  TO  2710-ERRLST-INIT-EXT
           END-IF
      *
           MOVE    MES-TITLE1      TO  ERR-REC
           WRITE   ERR-REC
           MOVE    MES-TITLE2-L1   TO  ERR-REC
           WRITE   ERR-REC
           MOVE    MES-TITLE2-L2   TO  ERR-REC
           WRITE   ERR-REC
           MOVE    MES-TITLE2-L3   TO  ERR-REC
           WRITE   ERR-REC
      *
           .
       2710-ERRLST-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック処理
      *****************************************************************
       31012-SEIWA-HEN-SEC         SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "11"            TO  LNK-DAY1-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY1-AREA
           IF      STS-DAY-RC1     =   ZERO
               MOVE    0               TO  FLG-YMD
           ELSE
               MOVE    1               TO  FLG-YMD
           END-IF
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    帳票出力　終了処理
           CLOSE                       CSV-FILE
           CLOSE                       ERR-FILE
      *
      *    DB DISCONNECT
           PERFORM 900-DBDISCONNECT-SEC
      *
      *
           DISPLAY "*(ORCVTSRYKARRK)* CSV     /I CNT[" CNT-CSV      "]"
           DISPLAY "*(ORCVTSRYKARRK)* SRYKARRK/O CNT[" CNT-SRYKARRK "]"
           DISPLAY "*(ORCVTSRYKARRK)* SANTEI  /O CNT[" CNT-SANTEI   "]"
           DISPLAY "*(ORCVTSRYKARRK)* ERR     /O CNT[" CNT-ERR      "]"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタＦＥＴＣＨ処理
      *****************************************************************
       900-SYSTEM-NEXT-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSTEM-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-HEN-SYOSINYMD(1:6)  TO  SANTEI-SRYYM
           MOVE    WRK-HEN-SRYCD       TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
      *
           MOVE    SANTEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_santei"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  SANTEI-REC
                   MOVE    ZERO            TO  FLG-SANTEI
               ELSE
                   MOVE    1               TO  FLG-SANTEI
                   INITIALIZE                  SANTEI-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-SANTEI
           END-IF
           MOVE    "tbl_santei"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科歴データ主キー検索
      *****************************************************************
       910-SRYKARRK-READ-SEC       SECTION.
      *
           MOVE    SPACE           TO  SRYKARRK-REC
           INITIALIZE                   SRYKARRK-REC
           MOVE    SPA-HOSPNUM     TO  KARRK-HOSPNUM
           MOVE    SPA-PTID        TO  KARRK-PTID
           MOVE    WRK-HEN-SRYKA   TO  KARRK-SRYKA
      *
           MOVE    SRYKARRK-REC    TO  MCPDATA-REC
           MOVE    "tbl_srykarrk"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srykarrk"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  SRYKARRK-REC
                   MOVE    ZERO            TO  FLG-SRYKARRK
               ELSE
                   MOVE    1               TO  FLG-SRYKARRK
                   INITIALIZE                  SRYKARRK-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-SRYKARRK
           END-IF
           MOVE    "tbl_srykarrk"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-SRYKARRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号読み込み
      *****************************************************************
       900-PTNUM-READ-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNUM
           MOVE    SPACE           TO  PTNUM-REC
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    SPA-PTNUM       TO  PTNUM-PTNUM
      *
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnum"     TO  MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  PTNUM-REC
                   MOVE    ZERO            TO  FLG-PTNUM
               ELSE
                   INITIALIZE                  PTNUM-REC
                   MOVE    1               TO  FLG-PTNUM
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTNUM
           END-IF
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険ＣＳＶファイル　検索処理
      *****************************************************************
       900-CSV-KENSAKU-SEC         SECTION.
      *
          MOVE    SPACE            TO  CSV-R
      *
          READ    CSV-FILE
              AT  END
                  MOVE    1            TO  FLG-CSV
              NOT AT  END
                  MOVE    ZERO         TO  FLG-CSV
                  ADD     1            TO  CNT-CSV
                  ADD     1            TO  CNT-COMMIT
          END-READ
      *
          .
       900-CSV-KENSAKU-EXT.
          EXIT.
      *
      *****************************************************************
      *    パラメータファイル読み込み処理
      *****************************************************************
       900-PARA-READ-SEC           SECTION.
      *
          MOVE    SPACE            TO  PARA-REC
      *
          READ    PARA-FILE
              AT  END
                  MOVE    1            TO  FLG-PARA
              NOT AT  END
                  ADD     1            TO  CNT-PARA
          END-READ
      *
          .
       900-PARA-READ-EXT.
          EXIT.
      *      
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
