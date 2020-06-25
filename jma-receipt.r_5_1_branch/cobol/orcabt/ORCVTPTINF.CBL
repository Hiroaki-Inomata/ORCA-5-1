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
       PROGRAM-ID.                 ORCVTPTINF.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＤＢ作成処理
      *  コンポーネント名  : ＣＳＶファイルコンバート処理（患者情報）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  01/04/11    MCC-太田      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/11/01  患者番号の空白までで桁数
      *                                     を編集  
      *  01.00.02    MCC-藤原     01/11/01  氏名・かな氏名を空白まで
      *                                     を編集  
      *  01.00.03    MCC-伊藤     01/12/12  患者番号構成に合わせて
      *                                     前０詰め編集を行う  
      *  01.00.04    MCC-伊藤     01/12/12  ＤＢコールインターフェイス
      *                                     の変更 
      *  01.00.05    MCC-森脇     02/05/01  空白編集の変更  
      *  01.01.01    NACL-森脇    02/11/28  入院履歴番号、枝番号の追加
      *  01.01.02    NACL-多々納  03/10/10  自由構成患者番号編集修正
      *  01.01.03    NACL-多々納  05/09/01  全角チェック変換追加
      *  01.01.04    NACL-森脇    05/07/13  エラーメッセージ、コミット
      *  01.01.05    NACL-森脇    05/09/26  構造変更
      *  01.01.06    NACL-森脇    06/07/26  MONFUNC対応
      *  03.05.00    NACL-多々納  07/05/16  グループ診療対応
      *                                     PTNUM 重複時処理終了の追加など
      *  04.05.00    NACL-多々納  09/12/XX  氏名の拡張漢字対応
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
      *    入力患者情報ＣＳＶファイル
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
      *    入力患者情報ＣＳＶファイル
       FD  CSV-FILE.
       01  CSV-REC.
           03  CSV-R                   PIC X(3000).
           03  CSV-RR  REDEFINES       CSV-R.
               05  CSV-X               PIC X(01)  OCCURS   3000.
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
           03  FLG-PTINF               PIC 9(01).
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
           03  FLG-UPDATE              PIC 9(01).
           03  FLG-YMD                 PIC 9(01).
           03  FLG-PTNUM-ERR           PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PARA                PIC 9(06).
           03  CNT-CSV                 PIC 9(06).
           03  CNT-ERR                 PIC 9(06).
           03  CNT-COMMIT              PIC 9(06).
           03  CNT-PTINF               PIC 9(06).
           03  CNT-PTNUM               PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDS                     PIC 9(04).
           03  IDE                     PIC 9(04).
      *
      *    パラメータ保存
       01  PARA-SAVE.
           03  PARA-03-1               PIC X(01).
           03  PARA-03-2               PIC X(10).
           03  PARA-03-3               PIC X(10).
           03  PARA-04-1               PIC X(10).
           03  PARA-05-1               PIC X(02).
           03  PARA-09-1               PIC X(06).
           03  PARA-11-1               PIC X(06).
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
      *
           03  WRK-POINT-AREA.
               05  WRK-PO-ST           PIC 9(04).
               05  WRK-PO-ED           PIC 9(04).
           03  WRK-MOVE-AREA.
               05  WRK-MO-ST           PIC 9(04).
               05  WRK-MO-CNT          PIC 9(04).
               05  WRK-WK              PIC 9(04).
           03  WRK-DATA                PIC X(200).
           03  WRK-LEN                 PIC 9(03).
           03  WRK-CNT                 PIC 9(04).
      *
           03  WRK-RENKETA             PIC 9(02).
           03  WRK-COMMIT              PIC 9(06).
           03  WRK-ERR                 PIC 9(06).
      *
           03  WRK-KOUMOKU-NO          PIC 9(04).
      *
       01  WRK-HEN-AREA.
           03  WRK-HEN-PTNUM           PIC X(20).
           03  WRK-HEN-WARIKBN         PIC X(01).
      *
      *    帳票出力領域
       01  MES-AREA.
           03  MES-TITLE1.
               05  FILLER              PIC X(23) VALUE
                                       "【PGID:ORCVTPTINF.CBL】".
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
                   " [5]患者ＩＤ取得失敗  [6]患者番号出力失敗 ".
                   07  FILLER          PIC X(20) VALUE
                   " [7]患者情報出力失敗".
               05  MES-TITLE2-L3.
                   07  FILLER          PIC X(50) VALUE
                   "--------------------------------------------------".
                   07  FILLER          PIC X(49) VALUE
                   "-------------------------------------------------".
      *
       01  ERRLST-AREA.
      *    エラー番号１・２・３で使用
           03  ERR-REC1.
               05  ERR1-LINENO         PIC X(06).
               05  FILLER              PIC X(01).
               05  ERR1-NAIYO          PIC X(03).
               05  ERR-OCCURS          OCCURS   4.
                   07  FILLER          PIC X(01).
                   07  ERR1-ERRNO      PIC X(03).
                   07  ERR1-KOMOKU     PIC X(09).
                   07  ERR1-ATAI       PIC X(08).
      *    エラー番号４・５・６で使用
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
               05  ERR2-NAMEMES        PIC X(09).
               05  ERR2-NAME           PIC X(30).
      *
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
      *    システム管理（医療機関情報）
           COPY    "CPSK1001.INC".
      *    システム管理（患者番号構成管理）
           COPY    "CPSK1009.INC".
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
           COPY    "CPORCXKANACONV.INC".
      *    全角チェックパラメタ（拡張漢字）
           COPY    "CPORCSKANACHK2.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *D   COPY    "CPORCMCP.INC".
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
           MOVE    SPACE           TO  ASS-AREA
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
           IF      FLG-PARA            =   2
               MOVE    1               TO  FLG-CSV
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    医療機関番号
           IF      PARA-HOSPNUM    NUMERIC
               MOVE    PARA-HOSPNUM    TO  SPA-HOSPNUM
           ELSE
               MOVE    1               TO  FLG-CSV
               DISPLAY "*(ORCVTPTINF)* HOSPNUM ERR [" PARA-HOSPNUM"]"
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    システム日付セット
           ACCEPT  WRK-SYMD2       FROM    DATE
           MOVE    WRK-SYMD2       TO  WRK-SYMD (3:)
           ADD     2000            TO  WRK-SYY
           MOVE    WRK-SYMD        TO  SPA-SYSYMD
           MOVE    SPA-SYSYMD(1:4) TO  MES-TITLE1-YY
           MOVE    SPA-SYSYMD(5:2) TO  MES-TITLE1-MM
           MOVE    SPA-SYSYMD(7:2) TO  MES-TITLE1-DD
      *
      *    端末ＩＤ固定
           MOVE    "ORCATERM"      TO  SPA-TERMID
      *
      *    帳票ＯＰＥＮ処理
           OPEN    INPUT               CSV-FILE
           IF      STS-CSV         NOT =   ZERO
               MOVE    1               TO  FLG-CSV
               DISPLAY "*(ORCVTPTINF)* PTINF-FILE OPN STS[" STS-CSV "]"
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    帳票出力　初期処理
           PERFORM 2910-ERRLST-INIT-SEC
           IF      FLG-CSV             =   1
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    データベースオープン
           PERFORM 100-DBOPEN-SEC
      *
      *    エンコーディング取得処理
           CALL    "ORCSENCODING"      USING    MCPAREA
                                                SPA-AREA
      *
      *    システム管理情報　検索処理
           PERFORM 110-CP1001-KENSAKU-SEC
           IF      FLG-CSV             =   1
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    患者番号構成クリア処理
           IF      PARA-03-1           =   "T"
               MOVE    9               TO  FLG-UPDATE
               PERFORM 250-PTNUM-GET-SEC
           END-IF
      *
      *    患者番号桁数　取得処理
           MOVE    ZERO            TO  FLG-UPDATE
           PERFORM 250-PTNUM-GET-SEC
           IF      WRK-RENKETA(1:2)    NOT =   PARA-05-1
               MOVE    1               TO  FLG-CSV
               DISPLAY "*(ORCVTPTINF)* PTNUM BYTE UNMATCH!"
               GO  TO  100-INIT-EXT
           END-IF
      *
           MOVE    PARA-05-1       TO  WRK-RENKETA(1:2)
           MOVE    PARA-09-1       TO  WRK-COMMIT
           MOVE    PARA-11-1       TO  WRK-ERR
      *
      *    患者ＣＳＶファイル　検索処理
           MOVE    1               TO  FLG-UPDATE
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
           MOVE    SPACE           TO  PARA-SAVE
           OPEN    INPUT               PARA-FILE
           IF      STS-PARA        NOT =   ZERO
               MOVE    2               TO  FLG-PARA
               DISPLAY "*(ORCVTPTINF)* PARA-FILE OPN STS[" STS-PARA "]"
               GO  TO  101-PARA-GET-EXT
           END-IF
      *
           PERFORM 900-PARA-READ-SEC
      *
           PERFORM UNTIL   FLG-PARA    =   1
               IF      PARA-RECKBN         =   "@"
                   PERFORM 101-PARA-GET1-SEC
               END-IF
               PERFORM 900-PARA-READ-SEC
           END-PERFORM
      *
           CLOSE                       PARA-FILE
      *
      *    パラメータチェック処理
           IF    ( PARA-03-2           IS  NUMERIC )   AND
                 ( PARA-03-3           IS  NUMERIC )   AND
                 ( PARA-04-1           IS  NUMERIC )   AND
                 ( PARA-05-1           IS  NUMERIC )   AND
                 ( PARA-09-1           IS  NUMERIC )   AND
                 ( PARA-11-1           IS  NUMERIC )
               CONTINUE
           ELSE
               MOVE    1               TO  FLG-CSV
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
           EVALUATE    PARA-DATKBN
               WHEN    "01-1"
                   MOVE    PARA-DATA     TO  ASS-CSV
               WHEN    "02-1"
                   MOVE    PARA-DATA     TO  ASS-ERR
               WHEN    "03-1"
                   IF    ( PARA-DATA     =   "YES" ) OR
                         ( PARA-DATA     =   "yes" )
                       MOVE    "T"       TO  PARA-03-1
                   ELSE
                       MOVE    "F"       TO  PARA-03-1
                   END-IF
               WHEN    "03-2"
                   MOVE    PARA-DATA     TO  PARA-03-2
               WHEN    "03-3"
                   MOVE    PARA-DATA     TO  PARA-03-3
               WHEN    "04-1"
                   MOVE    PARA-DATA     TO  PARA-04-1
               WHEN    "05-1"
                   MOVE    PARA-DATA     TO  PARA-05-1
               WHEN    "09-1"
                   MOVE    PARA-DATA     TO  PARA-09-1
               WHEN    "11-1"
                   MOVE    PARA-DATA     TO  PARA-11-1
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
       110-CP1001-KENSAKU-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
           MOVE    SPACE           TO  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    WRK-SYMD        TO  SYS-1001-STYUKYMD
                                       SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   1
               MOVE    1                   TO  FLG-CSV
           ELSE
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               MOVE    SYS-1001-HOSPID     TO  SPA-HOSPID
               MOVE    SYS-1001-HOSPSBT    TO  SPA-HOSPSBT
               MOVE    SYS-1001-ROUPAYKBN  TO  SPA-ROUPAYKBN
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       110-CP1001-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    1000件ごとにメッセージを表示
           IF      CNT-CSV (4:3)       =   ZERO
               DISPLAY "***(ORCVTPTINF)*** CNT-CSV[" CNT-CSV "]"
           END-IF
      *    指定した件数になったらコミット
           IF      CNT-COMMIT          =   WRK-COMMIT
               PERFORM 900-DBCOMMIT-SEC
               MOVE    ZERO            TO  CNT-COMMIT
           END-IF
      *    エラーが指定した件数になったら処理終了
           IF    ( WRK-ERR         NOT =   ZERO )  AND
                 ( CNT-ERR             =   WRK-ERR )
               MOVE    1               TO  FLG-CSV
               GO  TO  200-MAIN-EXT
           END-IF
      *
           MOVE    SPACE           TO  PTINF-REC
           INITIALIZE                  PTINF-REC
           MOVE    SPACE           TO  WRK-HEN-AREA
           INITIALIZE                  WRK-HEN-AREA
           MOVE    ZERO            TO  WRK-POINT-AREA
           MOVE    ZERO            TO  IDX-AREA
           MOVE    ZERO            TO  FLG-ERRARI
           MOVE    ZERO            TO  FLG-DATAEND
           MOVE    ZERO            TO  WRK-KOUMOKU-NO
      *
      *    患者ＣＳＶファイルに存在する項目数分、繰り返す
      *    またはレコード終了、または項目エラー４個まで
           PERFORM VARYING   IDX   FROM    1   BY  1
                   UNTIL   ( IDX           >   49   )
                   OR      ( WRK-PO-ED     >=  3000 )
                   OR      ( IDE           >   4    )
      *
               MOVE    ZERO            TO  FLG-AREA1
               COMPUTE WRK-PO-ST   =   WRK-PO-ED   +   1
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
      *    必須項目なし
           IF      FLG-ERRARI          =   ZERO
               IF     (WRK-KOUMOKU-NO      >   ZERO) AND
                      (WRK-KOUMOKU-NO      <   6   )
                   MOVE    1               TO  FLG-ERR
                   MOVE    1               TO  FLG-ERRARI
                   PERFORM 290-ERRREC-EDIT-SEC
               END-IF
           END-IF
      *    データなし
           IF     (WRK-KOUMOKU-NO      =   ZERO )
               CONTINUE
           ELSE
      *
           IF      FLG-ERRARI          =   ZERO
      *        患者情報セット
               PERFORM 250-PTINF-SET-SEC
               IF      FLG-ERR         NOT =   ZERO
      *            エラーリスト編集/出力処理
                   PERFORM 290-ERRREC-EDIT-SEC
                   MOVE    2           TO  FLG-ERRARI
                   PERFORM 290-ERRLST-OUT-SEC
               END-IF
           ELSE
      *        エラーリスト出力処理
               PERFORM 290-ERRLST-OUT-SEC
           END-IF
      *
           END-IF
      *
      *    患者ＣＳＶファイル　検索処理
           IF      FLG-CSV             =   ZERO
               PERFORM 900-CSV-KENSAKU-SEC
           END-IF
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
           MOVE    WRK-PO-ST           TO  WRK-PO-ED
      *
           PERFORM VARYING   IDY   FROM    WRK-PO-ST   BY  1
                   UNTIL   ( IDY           >=  3000 )
                   OR      ( FLG-COMMA     =   1    )
      *        カンマの位置
               IF    ( CSV-X (IDY)     =   "," )  OR
                     ((IDX             =   49   ) AND
                     ( CSV-X (IDY)     =   SPACE))
                   MOVE    IDY         TO  WRK-PO-ED
                   MOVE    1           TO  FLG-COMMA
               ELSE
      *            カンマが足りない場合の終了位置決定
                   IF      CSV-X (IDY) NOT =   SPACE
                       MOVE    IDY         TO  WRK-PO-ED
                   END-IF
               END-IF
      *        デリミタ(")存在するかどうか
               IF      CSV-X (IDY)         =   """"
                   IF      FLG-DELI1       =   ZERO
                       MOVE    1       TO  FLG-DELI1
                   ELSE
                       MOVE    1       TO  FLG-DELI2
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-COMMA           =   ZERO
               IF      WRK-PO-ST       NOT =   WRK-PO-ED
                   COMPUTE WRK-PO-ED       =   WRK-PO-ED   +   1
               END-IF
           END-IF
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
           IF      FLG-DELI1           =   1
      *        デリミタ(")を含んでいる場合
               IF      FLG-DELI2       =   1
                   COMPUTE WRK-WK      =   WRK-PO-ST   +   2
               ELSE
                   COMPUTE WRK-WK      =   WRK-PO-ST   +   1
               END-IF
               IF      WRK-WK          =   WRK-PO-ED
                   MOVE    1           TO  FLG-NODATA
                   GO  TO  220-KOUMOKU-KYOTU-EXT
               END-IF
           ELSE
      *        デリミタ(")を含んでいない場合
               IF      WRK-PO-ST       =   WRK-PO-ED
                   MOVE    1           TO  FLG-NODATA
                   GO  TO  220-KOUMOKU-KYOTU-EXT
               END-IF
           END-IF
      *
      *    転送開始位置・転送桁数を算出
           IF      FLG-DELI1           =   1
      *        デリミタ(")を含んでいる場合
               COMPUTE WRK-MO-ST       =   WRK-PO-ST   +   1
               IF      FLG-DELI2       =   1
                   COMPUTE WRK-MO-CNT  =   WRK-PO-ED
                                       -  (WRK-PO-ST   +   2)
               ELSE
                   COMPUTE WRK-MO-CNT  =  (WRK-PO-ED   -   1)
                                       -   WRK-PO-ST
               END-IF
           ELSE
      *        デリミタ(")を含んでいない場合
               MOVE    WRK-PO-ST       TO  WRK-MO-ST
               COMPUTE WRK-MO-CNT      =   WRK-PO-ED  -  WRK-PO-ST
           END-IF
      *
           IF    ( WRK-MO-CNT          =   1 ) AND
                 ( CSV-REC (WRK-MO-ST:WRK-MO-CNT) =   X"0D" )
               MOVE  1                 TO  FLG-NODATA
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
      *    ２：カナ氏名
           WHEN    2
               PERFORM 2410-KANANAME-SET-SEC
      *    ３：漢字氏名
           WHEN    3
               PERFORM 2410-NAME-SET-SEC
      *    ４：通称
           WHEN    4
               PERFORM 2410-NICKNAME-SET-SEC
      *    ５：性別
           WHEN    5
               PERFORM 2410-SEX-SET-SEC
      *    ６：生年月日
           WHEN    6
               PERFORM 2410-BIRTHDAY-SET-SEC
      *    ７：死亡区分
           WHEN    7
               PERFORM 2410-DEATHKBN-SET-SEC
      *    ８：自宅−郵便番号
           WHEN    8
               PERFORM 2410-HOME-POST-SET-SEC
      *    ９：自宅−住所
           WHEN    9
               PERFORM 2410-HOME-ADRS-SET-SEC
      *    １０：自宅−番地
           WHEN    10
               PERFORM 2410-HOME-BANTI-SET-SEC
      *    １１：自宅−電話番号（昼）
           WHEN    11
               PERFORM 2410-HOME-TEL1-SET-SEC
      *    １２：自宅−電話番号（夜）
           WHEN    12
               PERFORM 2410-HOME-TEL2-SET-SEC
      *    １３：世帯主
           WHEN    13
               PERFORM 2410-SETAINUSI-SET-SEC
      *    １４：続柄
           WHEN    14
               PERFORM 2410-ZOKUGARA-SET-SEC
      *    １５：職業
           WHEN    15
               PERFORM 2410-JOB-SET-SEC
      *    １６：携帯電話番号
           WHEN    16
               PERFORM 2410-KEITAI-TEL-SET-SEC
      *    １７：ＦＡＸ番号
           WHEN    17
               PERFORM 2410-FAX-SET-SEC
      *    １８：電子メールアドレス
           WHEN    18
               PERFORM 2410-EMAIL-SET-SEC
      *    １９：減免事由
           WHEN    19
               PERFORM 2410-DISCOUNT-KBN-SET-SEC
      *    ２０：割引区分
           WHEN    20
               PERFORM 2410-WARIKBN-SET-SEC
      *    ２１：割引
           WHEN    21
               PERFORM 2410-DISCOUNT-SET-SEC
      *    ２２：コメント１
           WHEN    22
               PERFORM 2410-COMMENT-1-SET-SEC
      *    ２３：コメント２
           WHEN    23
               PERFORM 2410-COMMENT-2-SET-SEC
      *    ２４：禁忌１
           WHEN    24
               PERFORM 2410-TABOO-1-SET-SEC
      *    ２５：禁忌２
           WHEN    25
               PERFORM 2410-TABOO-2-SET-SEC
      *    ２６：アレルギー１
           WHEN    26
               PERFORM 2410-ALLERGY-1-SET-SEC
      *    ２７：アレルギー２
           WHEN    27
               PERFORM 2410-ALLERGY-2-SET-SEC
      *    ２８：感染症１
           WHEN    28
               PERFORM 2410-KANSENSYO-1-SET-SEC
      *    ２９：感染症２
           WHEN    29
               PERFORM 2410-KANSENSYO-2-SET-SEC
      *    ３０：連絡先−名称
           WHEN    30
               PERFORM 2410-RENRAKU-NAME-SET-SEC
      *    ３１：連絡先−続柄
           WHEN    31
               PERFORM 2410-RENRAKU-ZOKUGARA-SET-SEC
      *    ３２：連絡先−郵便番号
           WHEN    32
               PERFORM 2410-RENRAKU-POST-SET-SEC
      *    ３３：連絡先−住所
           WHEN    33
               PERFORM 2410-RENRAKU-ADRS-SET-SEC
      *    ３４：連絡先−番地方書
           WHEN    34
               PERFORM 2410-RENRAKU-BANTI-SET-SEC
      *    ３５：連絡先−電話番号（昼）
           WHEN    35
               PERFORM 2410-RENRAKU-TEL1-SET-SEC
      *    ３６：連絡先−電話番号（夜）
           WHEN    36
               PERFORM 2410-RENRAKU-TEL2-SET-SEC
      *    ３７：勤務先−名称
           WHEN    37
               PERFORM 2410-OFFICE-NAME-SET-SEC
      *    ３８：勤務先−郵便番号
           WHEN    38
               PERFORM 2410-OFFICE-POST-SET-SEC
      *    ３９：勤務先−住所
           WHEN    39
               PERFORM 2410-OFFICE-ADRS-SET-SEC
      *    ４０：勤務先−番地方書
           WHEN    40
               PERFORM 2410-OFFICE-BANTI-SET-SEC
      *    ４１：勤務先−電話番号
           WHEN    41
               PERFORM 2410-OFFICE-TEL-SET-SEC
      *    ４２：帰省先−名称
           WHEN    42
               PERFORM 2410-KISEI-NAME-SET-SEC
      *    ４３：帰省先−郵便番号
           WHEN    43
               PERFORM 2410-KISEI-POST-SET-SEC
      *    ４４：帰省先−住所
           WHEN    44
               PERFORM 2410-KISEI-ADRS-SET-SEC
      *    ４５：帰省先−番地方書
           WHEN    45
               PERFORM 2410-KISEI-BANTI-SET-SEC
      *    ４６：帰省先−電話番号
           WHEN    46
               PERFORM 2410-KISEI-TEL-SET-SEC
      *    ４７：テスト患者フラグ
           WHEN    47
               PERFORM 2410-TSTPTNUMKBN-SET-SEC
      *    ４８：登録日
           WHEN    48
               PERFORM 2410-CREYMD-SET-SEC
      *    ４９：更新日
           WHEN    49
               PERFORM 2410-UPYMD-SET-SEC
           END-EVALUATE
      *
           IF      FLG-ERR             NOT =   ZERO
               MOVE    1               TO  FLG-ERRARI
               PERFORM 290-ERRREC-EDIT-SEC
           END-IF
      *
           .
       240-KOUMOKU-BETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目空白処理
      *****************************************************************
       2401-SPACE-HENSYU-SEC       SECTION.
      *
           MOVE    ZERO            TO  WRK-LEN
      *
           IF      WRK-DATA            =   SPACE
               MOVE    1               TO  WRK-LEN
           ELSE
               PERFORM VARYING IDS FROM    200 BY  -1
                       UNTIL   IDS <       1
                   IF      WRK-DATA (IDS:1)    NOT =   SPACE
                       MOVE    IDS             TO  WRK-LEN
                       MOVE    1               TO  IDS
                   END-IF
               END-PERFORM
      *
      *        後ろの全角空白を削除
               IF      WRK-LEN             >   1
                   PERFORM 24021-KUHAKU-DEL-SEC
               END-IF
      *        半角英数字であること
               IF      WRK-LEN             >   1
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI
                   MOVE    WRK-DATA        TO  KANACHK-MAE-INPUT
                   MOVE    WRK-CNT         TO  KANACHK-MAX-CNT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   IF     (KANACHK-RC          =   ZERO  )  AND
                          (KANACHK-SYUBETU     =   1     )  AND
                          (KANACHK-MAE-INPUT   =   KANACHK-OUT-INPUT)
                       CONTINUE
                   ELSE
      *                全角、半角カナはエラー
                       MOVE    3               TO  FLG-ERR
                   END-IF
               END-IF
           END-IF
           .
       2401-SPACE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角桁数・空白処理
      *****************************************************************
       2402-ZENKAKU-HEN-SEC        SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
      *    後ろの全角空白削除
           MOVE    "1"             TO  KANACHK-SYORIKBN
           MOVE    WRK-DATA        TO  KANACHK-MAE-INPUT
           MOVE    WRK-CNT         TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-DATA
           MOVE    KANACHK-MAX         TO  WRK-LEN
           IF      KANACHK-RC      NOT =   ZERO
      *        全角・半角混在エラ−
               MOVE    3               TO  FLG-ERR
           ELSE
               IF      KANACHK-SYUBETU =   1
      *        半角を全角へ
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI
                   MOVE    "1"             TO  KANACHK-SYORIKBN
                   MOVE    WRK-DATA        TO  KANACHK-MAE-INPUT
                   MOVE    WRK-CNT         TO  KANACHK-MAX-CNT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-MAX         TO  WRK-LEN
                       MOVE    KANACHK-OUT-INPUT   TO  WRK-DATA
                   END-IF
               ELSE
                   MOVE    KANACHK-OUT-INPUT   TO  WRK-DATA
               END-IF
           END-IF
           IF      WRK-LEN         =   ZERO
               MOVE    1               TO  WRK-LEN
           END-IF
      ***  後ろの全角空白を削除
      *    IF      WRK-LEN         >   1
      *        PERFORM 24021-KUHAKU-DEL-SEC
      **** END-IF
           .
       2402-ZENKAKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角空白削除処理
      *****************************************************************
       24021-KUHAKU-DEL-SEC        SECTION.
      *
           COMPUTE IDZ             =   WRK-LEN     -   1
           IF      WRK-DATA (IDZ:2)    =   "　"
               MOVE    1                   TO  WRK-LEN
               PERFORM VARYING IDS     FROM    IDZ BY  -2
                       UNTIL   IDS     <   2
                   IF      WRK-DATA (IDS:2)    NOT =   "　"
                       COMPUTE WRK-LEN         =   IDS     +   1
                       MOVE    2               TO  IDS
                   ELSE
                       IF      IDS             =   3
                           IF      WRK-DATA (1:2)      NOT =   "　"
                               MOVE    2               TO  WRK-LEN
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .
       2402-ZENKAKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角桁数・空白処理（コメント）
      *****************************************************************
       2402-ZENKAKU-HEN2-SEC       SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-DATA        TO  KANACHK-MAE-INPUT
           MOVE    WRK-CNT         TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-DATA
           MOVE    KANACHK-MAX         TO  WRK-LEN
           IF      WRK-LEN             =   ZERO
               MOVE    1               TO  WRK-LEN
           END-IF
      *    後ろの全角空白を削除
           IF      WRK-LEN             >   1
               PERFORM 24021-KUHAKU-DEL-SEC
           END-IF
           .
       2402-ZENKAKU-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角桁数・空白処理（拡張漢字対応）
      *****************************************************************
       2402-ZENKAKU-HEN02-SEC        SECTION.
      *
           IF      SPA-ENCODING        =   2
      *        拡張漢字対応
               PERFORM 24021-ZENKAKU-HEN02C-SEC
           ELSE
      *        拡張漢字不可
               PERFORM 2402-ZENKAKU-HEN-SEC
           END-IF
           .
       2402-ZENKAKU-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角桁数・空白処理（拡張漢字対応）
      *****************************************************************
       24021-ZENKAKU-HEN02C-SEC        SECTION.
      *
           INITIALIZE                  ORCSKANACHK2AREA
           MOVE    "1"             TO  KANACHK2-SYORI
           MOVE    "1"             TO  KANACHK2-SYORIKBN
           MOVE    WRK-DATA        TO  KANACHK2-MAE-INPUT
           MOVE    WRK-CNT         TO  KANACHK2-MAX-CNT
           CALL    "ORCSKANACHK2"      USING
                                       ORCSKANACHK2AREA
           MOVE    KANACHK2-MAE-INPUT  TO  WRK-DATA
           MOVE    KANACHK2-MAX        TO  WRK-LEN
           IF      KANACHK2-RC     NOT =   ZERO
      *        全角・半角混在エラ−
               MOVE    3               TO  FLG-ERR
           ELSE
               IF      KANACHK2-SYUBETU =   1
      *        半角を全角へ
                   INITIALIZE                  ORCSKANACHK2AREA
                   MOVE    "2"             TO  KANACHK2-SYORI
                   MOVE    "1"             TO  KANACHK2-SYORIKBN
                   MOVE    WRK-DATA        TO  KANACHK2-MAE-INPUT
                   MOVE    WRK-CNT         TO  KANACHK2-MAX-CNT
                   CALL    "ORCSKANACHK2"      USING
                                               ORCSKANACHK2AREA
                   IF      KANACHK2-RC         =   ZERO
                       MOVE    KANACHK2-MAX         TO  WRK-LEN
                       MOVE    KANACHK2-OUT-INPUT   TO  WRK-DATA
                   END-IF
               ELSE
                   MOVE    KANACHK2-OUT-INPUT   TO  WRK-DATA
               END-IF
           END-IF
           IF      WRK-LEN         =   ZERO
               MOVE    1               TO  WRK-LEN
           END-IF
           .
       24021-ZENKAKU-HEN02C-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    項目１：患者番号　処理
      *****************************************************************
       2410-PTNUM-SET-SEC          SECTION.
      *
           MOVE    SPACE           TO  SPA-PTNUM
      *
      *    必須未入力
           IF      FLG-NODATA          =   1
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
                   OR    ( WRK-HEN-PTNUM (IDZ:1)   =   SPACE )
               MOVE    IDZ             TO  WRK-CNT
           END-PERFORM
           MOVE    WRK-CNT             TO  WRK-MO-CNT
      *
      *    項目桁数不正
           IF    ( WRK-MO-CNT          =   0  )    OR
                 ( WRK-MO-CNT          >   20 )
               MOVE    2               TO  FLG-ERR
               GO  TO  2410-PTNUM-SET-EXT
           END-IF
      *
      *    患者番号セット
           IF   (( SYS-1009-PTNUMKSIKBN    =   "1" ) AND
                 ( SYS-1009-JIYKSIKBN      =   "2" ))    OR
                (( SYS-1009-PTNUMKSIKBN    =   "2" ) AND
                 ( SYS-1009-HJNKSIKBN      =   "3" OR "4" ))
               MOVE    ZERO                TO  SPA-PTNUM(1:WRK-RENKETA)
               IF      WRK-MO-CNT      >   WRK-RENKETA
                   COMPUTE WRK-MO-ST   =   WRK-MO-ST
                                       +  (WRK-MO-CNT  -   WRK-RENKETA)
                   MOVE  CSV-REC (WRK-MO-ST:WRK-RENKETA)
                                       TO  SPA-PTNUM
               ELSE
                   COMPUTE IDZ         =   WRK-RENKETA
                                       -   WRK-MO-CNT
                                       +   1
                   MOVE  CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  SPA-PTNUM(IDZ:)
               END-IF
           ELSE
               MOVE    1               TO  IDZ
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  SPA-PTNUM(IDZ:)
           END-IF
      *
      *    半角チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    SPA-PTNUM       TO  KANACHK-MAE-INPUT
           MOVE    20              TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC          =   ZERO  )  AND
                  (KANACHK-SYUBETU     =   1     )  AND
                  (KANACHK-RC2         =   ZERO  )
               CONTINUE
           ELSE
      *        半角以外はエラー
               MOVE    3               TO  FLG-ERR
           END-IF
           .
       2410-PTNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２：カナ氏名　処理
      *****************************************************************
       2410-KANANAME-SET-SEC       SECTION.
      *
      *    必須未入力
           IF      FLG-NODATA      =   1
               MOVE    1               TO  FLG-ERR
               GO  TO  2410-KANANAME-SET-EXT
           END-IF
      *
      *    カナ氏名セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    50              TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-KANANAME
           END-IF
      *
           .
       2410-KANANAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目３：漢字氏名　処理
      *****************************************************************
       2410-NAME-SET-SEC           SECTION.
      *
      *    必須未入力
           IF      FLG-NODATA      =   1
      *        カナがなければエラー
               IF  PTINF-KANANAME      =   SPACE
                   MOVE    1               TO  FLG-ERR
               ELSE
                   MOVE    PTINF-KANANAME  TO  PTINF-NAME
               END-IF
               GO  TO  2410-NAME-SET-EXT
           END-IF
      *
      *    漢字氏名セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    50              TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN02-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-NAME
           END-IF
      *
           .
       2410-NAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目４：通称　処理
      *****************************************************************
       2410-NICKNAME-SET-SEC       SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-NICKNAME-SET-EXT
           END-IF
      *
      *    通称セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    50              TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN02-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-NICKNAME
           END-IF
      *
           .
       2410-NICKNAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目５：性別　処理
      *****************************************************************
       2410-SEX-SET-SEC            SECTION.
      *
      *    必須未入力
           IF      FLG-NODATA      =   1
               MOVE    1               TO  FLG-ERR
               GO  TO  2410-SEX-SET-EXT
           END-IF
      *
      *    項目桁数不正
           IF      WRK-MO-CNT      NOT =   1
               MOVE    2               TO  FLG-ERR
               GO  TO  2410-SEX-SET-EXT
           END-IF
      *
      *    性別セット
           EVALUATE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
               WHEN    "1"     THRU    "2"
                   MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  PTINF-SEX
               WHEN    OTHER
                   MOVE    3           TO  FLG-ERR
           END-EVALUATE
      *
           .
       2410-SEX-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目６：生年月日　処理
      *****************************************************************
       2410-BIRTHDAY-SET-SEC       SECTION.
      *
      *    必須未入力
           IF      FLG-NODATA      =   1
               MOVE    1               TO  FLG-ERR
               GO  TO  2410-BIRTHDAY-SET-EXT
           END-IF
      *
      *    項目桁数不正
           IF      WRK-MO-CNT      NOT =   8
               MOVE    2               TO  FLG-ERR
               GO  TO  2410-BIRTHDAY-SET-EXT
           END-IF
      *
      *    生年月日セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           IF      FLG-YMD         =   1
               MOVE    3               TO  FLG-ERR
           ELSE
               MOVE    WRK-SYMD        TO  PTINF-BIRTHDAY
           END-IF
      *
           .
       2410-BIRTHDAY-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目７：死亡区分　処理
      *****************************************************************
       2410-DEATHKBN-SET-SEC       SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               MOVE    "0"             TO  PTINF-DEATHKBN
               GO  TO  2410-DEATHKBN-SET-EXT
           END-IF
      *
      *    項目桁数不正
           IF      WRK-MO-CNT      NOT =   1
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-DEATHKBN-SET-EXT
           END-IF
      *
      *    死亡区分セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  PTINF-DEATHKBN
      *
           .
       2410-DEATHKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目８：自宅−郵便番号　処理
      *****************************************************************
       2410-HOME-POST-SET-SEC      SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-HOME-POST-SET-EXT
           END-IF
      *
      *    項目桁数不正
           IF      WRK-MO-CNT      >   7
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-HOME-POST-SET-EXT
           END-IF
      *
      *    郵便番号セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           PERFORM 2401-SPACE-HENSYU-SEC
           MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-HOME-POST
      *
           .
       2410-HOME-POST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目９：自宅−住所　処理
      *****************************************************************
       2410-HOME-ADRS-SET-SEC      SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-HOME-ADRS-SET-EXT
           END-IF
      *
      *    住所セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    100             TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-HOME-ADRS
           END-IF
      *
           .
       2410-HOME-ADRS-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１０：自宅−番地　処理
      *****************************************************************
       2410-HOME-BANTI-SET-SEC     SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-HOME-BANTI-SET-EXT
           END-IF
      *
      *    番地セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    100             TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-HOME-BANTI
           END-IF
      *
           .
       2410-HOME-BANTI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１１：自宅−電話番号（昼）　処理
      *****************************************************************
       2410-HOME-TEL1-SET-SEC      SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-HOME-TEL1-SET-EXT
           END-IF
      *
      *    電話番号セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           PERFORM 2401-SPACE-HENSYU-SEC
           MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-HOME-TEL1
      *
           .
       2410-HOME-TEL1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１２：自宅−電話番号（夜）　処理
      *****************************************************************
       2410-HOME-TEL2-SET-SEC      SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-HOME-TEL2-SET-EXT
           END-IF
      *
      *    電話番号セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           PERFORM 2401-SPACE-HENSYU-SEC
           MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-HOME-TEL2
      *
           .
       2410-HOME-TEL2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１３：世帯主　処理
      *****************************************************************
       2410-SETAINUSI-SET-SEC      SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-SETAINUSI-SET-EXT
           END-IF
      *
      *    世帯主セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    50              TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN02-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-SETAINUSI
           END-IF
      *
           .
       2410-SETAINUSI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１４：続柄　処理
      *****************************************************************
       2410-ZOKUGARA-SET-SEC       SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-ZOKUGARA-SET-EXT
           END-IF
      *
      *    続柄セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    30              TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-ZOKUGARA
           END-IF
      *
           .
       2410-ZOKUGARA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１５：職業　処理
      *****************************************************************
       2410-JOB-SET-SEC            SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-JOB-SET-EXT
           END-IF
      *
      *    職業セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    20              TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-JOB
           END-IF
      *
           .
       2410-JOB-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１６：携帯電話番号　処理
      *****************************************************************
       2410-KEITAI-TEL-SET-SEC     SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-KEITAI-TEL-SET-EXT
           END-IF
      *
      *    携帯電話番号セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           PERFORM 2401-SPACE-HENSYU-SEC
           MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-KEITAI-TEL
      *
           .
       2410-KEITAI-TEL-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１７：ＦＡＸ番号　処理
      *****************************************************************
       2410-FAX-SET-SEC            SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-FAX-SET-EXT
           END-IF
      *
      *    ＦＡＸ番号セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           PERFORM 2401-SPACE-HENSYU-SEC
           MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-FAX
      *
           .
       2410-FAX-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１８：電子メールアドレス　処理
      *****************************************************************
       2410-EMAIL-SET-SEC          SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-EMAIL-SET-EXT
           END-IF
      *
      *    電子メールアドレス
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           PERFORM 2401-SPACE-HENSYU-SEC
           MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-EMAIL
      *
           .
       2410-EMAIL-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１９：減免事由　処理
      *****************************************************************
       2410-DISCOUNT-KBN-SET-SEC   SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               MOVE    "00"            TO  PTINF-DISCOUNT-KBN
               GO  TO  2410-DISCOUNT-KBN-SET-EXT
           END-IF
      *
      *    減免事由セット
           EVALUATE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
               WHEN    "職員家族通院"
                   MOVE    "02"        TO  PTINF-DISCOUNT-KBN
               WHEN    "職員家族入院"
                   MOVE    "03"        TO  PTINF-DISCOUNT-KBN
               WHEN    OTHER
                   MOVE    "00"        TO  PTINF-DISCOUNT-KBN
           END-EVALUATE
      *
           .
       2410-DISCOUNT-KBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２０：割引区分　処理
      *****************************************************************
       2410-WARIKBN-SET-SEC        SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               MOVE    ZERO            TO  WRK-HEN-WARIKBN
               GO  TO  2410-WARIKBN-SET-EXT
           END-IF
      *
      *    項目桁数不正
           IF      WRK-MO-CNT      NOT =   1
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-WARIKBN-SET-EXT
           END-IF
      *
      *    割引区分セット
           EVALUATE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
               WHEN    "1"     THRU    "2"
                   MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  WRK-HEN-WARIKBN
      *        誤入力
               WHEN    OTHER
                   MOVE    3       TO  FLG-ERR
                   GO  TO  2410-WARIKBN-SET-EXT
          END-EVALUATE
      *
           .
       2410-WARIKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２１：割引　処理
      *****************************************************************
       2410-DISCOUNT-SET-SEC           SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               MOVE    "00"            TO  PTINF-DISCOUNT
               GO  TO  2410-DISCOUNT-SET-EXT
           END-IF
      *
      *    割引セット
           EVALUATE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
      *        例：定率
               WHEN    "20"
                   IF  WRK-HEN-WARIKBN     =   "1"
                       MOVE    "02"    TO  PTINF-DISCOUNT
                   END-IF
      *        例：定額
               WHEN    "1000"
                   IF  WRK-HEN-WARIKBN     =   "2"
                       MOVE    "04"    TO  PTINF-DISCOUNT
                   END-IF
           END-EVALUATE
      *
           IF  PTINF-DISCOUNT      =   SPACE
               MOVE    "00"        TO  PTINF-DISCOUNT
           END-IF
      *
           .
       2410-DISCOUNT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２２：コメント１　処理
      *****************************************************************
       2410-COMMENT-1-SET-SEC      SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-COMMENT-1-SET-EXT
           END-IF
      *
      *    コメント１セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    100             TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN2-SEC
           MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-COMMENT-1
      *
           .
       2410-COMMENT-1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２３：コメント２　処理
      *****************************************************************
       2410-COMMENT-2-SET-SEC      SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-COMMENT-2-SET-EXT
           END-IF
      *
      *    コメント２セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    100             TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN2-SEC
           MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-COMMENT-2
      *
           .
       2410-COMMENT-2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２４：禁忌１　処理
      *****************************************************************
       2410-TABOO-1-SET-SEC        SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-TABOO-1-SET-EXT
           END-IF
      *
      *    禁忌１セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    100             TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-TABOO-1
           END-IF
      *
           .
       2410-TABOO-1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２５：禁忌２　処理
      *****************************************************************
       2410-TABOO-2-SET-SEC        SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-TABOO-2-SET-EXT
           END-IF
      *
      *    禁忌２セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    100             TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-TABOO-2
           END-IF
      *
           .
       2410-TABOO-2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２６：アレルギー１　処理
      *****************************************************************
       2410-ALLERGY-1-SET-SEC      SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-ALLERGY-1-SET-EXT
           END-IF
      *
      *    アレルギー１セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    100             TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-ALLERGY-1
           END-IF
      *
           .
       2410-ALLERGY-1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２７：アレルギー２　処理
      *****************************************************************
       2410-ALLERGY-2-SET-SEC       SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-ALLERGY-2-SET-EXT
           END-IF
      *
      *    アレルギー２セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    100             TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-ALLERGY-2
           END-IF
      *
           .
       2410-ALLERGY-2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２８：感染症１　処理
      *****************************************************************
       2410-KANSENSYO-1-SET-SEC    SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-KANSENSYO-1-SET-EXT
           END-IF
      *
      *    感染症１セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    100             TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-KANSENSYO-1
           END-IF
      *
           .
       2410-KANSENSYO-1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目２９：感染症２　処理
      *****************************************************************
       2410-KANSENSYO-2-SET-SEC    SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-KANSENSYO-2-SET-EXT
           END-IF
      *
      *    感染症２セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    100             TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-KANSENSYO-2
           END-IF
      *
           .
       2410-KANSENSYO-2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目３０：連絡先−名称　処理
      *****************************************************************
       2410-RENRAKU-NAME-SET-SEC   SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-RENRAKU-NAME-SET-EXT
           END-IF
      *
      *    連絡先−名称セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    50              TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-RENRAKU-NAME
           END-IF
      *
           .
       2410-RENRAKU-NAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目３１：連絡先−続柄　処理
      *****************************************************************
       2410-RENRAKU-ZOKUGARA-SET-SEC   SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-RENRAKU-ZOKUGARA-SET-EXT
           END-IF
      *
      *    連絡先−続柄セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                  TO  WRK-DATA
           MOVE    30             TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                  TO  PTINF-RENRAKU-ZOKUGARA
           END-IF
      *
           .
       2410-RENRAKU-ZOKUGARA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目３２：連絡先−郵便番号　処理
      *****************************************************************
       2410-RENRAKU-POST-SET-SEC   SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-RENRAKU-POST-SET-EXT
           END-IF
      *
      *    項目桁数不正
           IF      WRK-MO-CNT      >  7
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-RENRAKU-POST-SET-EXT
           END-IF
      *
      *    連絡先−郵便番号セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           PERFORM 2401-SPACE-HENSYU-SEC
           MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-RENRAKU-POST
      *
           .
       2410-RENRAKU-POST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目３３：連絡先−住所　処理
      *****************************************************************
       2410-RENRAKU-ADRS-SET-SEC   SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-RENRAKU-ADRS-SET-EXT
           END-IF
      *
      *    連絡先−住所セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    100             TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-RENRAKU-ADRS
           END-IF
      *
           .
       2410-RENRAKU-ADRS-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目３４：連絡先−番地方書　処理
      *****************************************************************
       2410-RENRAKU-BANTI-SET-SEC  SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-RENRAKU-BANTI-SET-EXT
           END-IF
      *
      *    連絡先−番地方書セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    100             TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-RENRAKU-BANTI
           END-IF
      *
           .
       2410-RENRAKU-BANTI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目３５：連絡先−電話番号（昼）　処理
      *****************************************************************
       2410-RENRAKU-TEL1-SET-SEC   SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-RENRAKU-TEL1-SET-EXT
           END-IF
      *
      *    連絡先−電話番号セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                  TO  WRK-DATA
           PERFORM 2401-SPACE-HENSYU-SEC
           MOVE    WRK-DATA (1:WRK-LEN)
                                  TO  PTINF-RENRAKU-TEL1
      *
           .
       2410-RENRAKU-TEL1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目３６：連絡先−電話番号（夜）　処理
      *****************************************************************
       2410-RENRAKU-TEL2-SET-SEC   SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-RENRAKU-TEL2-SET-EXT
           END-IF
      *
      *    連絡先−電話番号セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           PERFORM 2401-SPACE-HENSYU-SEC
           MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-RENRAKU-TEL2
      *
           .
       2410-RENRAKU-TEL2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目３７：勤務先−名称　処理
      *****************************************************************
       2410-OFFICE-NAME-SET-SEC    SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-OFFICE-NAME-SET-EXT
           END-IF
      *
      *    勤務先−名称セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    50              TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-OFFICE-NAME
           END-IF
      *
           .
       2410-OFFICE-NAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目３８：勤務先−郵便番号　処理
      *****************************************************************
       2410-OFFICE-POST-SET-SEC    SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-OFFICE-POST-SET-EXT
           END-IF
      *
      *    項目桁数不正
           IF      WRK-MO-CNT      >   7
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-OFFICE-POST-SET-EXT
           END-IF
      *
      *    勤務先−郵便番号セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           PERFORM 2401-SPACE-HENSYU-SEC
           MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-OFFICE-POST
      *
           .
       2410-OFFICE-POST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目３９：勤務先−住所　処理
      *****************************************************************
       2410-OFFICE-ADRS-SET-SEC    SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-OFFICE-ADRS-SET-EXT
           END-IF
      *
      *    勤務先−住所セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    100             TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-OFFICE-ADRS
           END-IF
      *
           .
       2410-OFFICE-ADRS-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目４０：勤務先−番地方書　処理
      *****************************************************************
       2410-OFFICE-BANTI-SET-SEC   SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-OFFICE-BANTI-SET-EXT
           END-IF
      *
      *    勤務先−番地方書セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    100             TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-OFFICE-BANTI
           END-IF
      *
           .
       2410-OFFICE-BANTI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目４１：勤務先−電話番号　処理
      *****************************************************************
       2410-OFFICE-TEL-SET-SEC     SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-OFFICE-TEL-SET-EXT
           END-IF
      *
      *    勤務先−電話番号セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           PERFORM 2401-SPACE-HENSYU-SEC
           MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-OFFICE-TEL
      *
           .
       2410-OFFICE-TEL-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目４２：帰省先−名称　処理
      *****************************************************************
       2410-KISEI-NAME-SET-SEC     SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-KISEI-NAME-SET-EXT
           END-IF
      *
      *    帰省先−名称セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    50              TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-KISEI-NAME
           END-IF
      *
           .
       2410-KISEI-NAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目４３：帰省先−郵便番号　処理
      *****************************************************************
       2410-KISEI-POST-SET-SEC     SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-KISEI-POST-SET-EXT
           END-IF
      *
      *    項目桁数不正
           IF      WRK-MO-CNT      >   7
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-KISEI-POST-SET-EXT
           END-IF
      *
      *    帰省先−郵便番号セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           PERFORM 2401-SPACE-HENSYU-SEC
           MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-KISEI-POST
      *
           .
       2410-KISEI-POST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目４４：帰省先−住所　処理
      *****************************************************************
       2410-KISEI-ADRS-SET-SEC     SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-KISEI-ADRS-SET-EXT
           END-IF
      *
      *    帰省先−住所セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    100             TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-KISEI-ADRS
           END-IF
      *
           .
       2410-KISEI-ADRS-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目４５：帰省先−番地方書　処理
      *****************************************************************
       2410-KISEI-BANTI-SET-SEC    SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-KISEI-BANTI-SET-EXT
           END-IF
      *
      *    帰省先−番地方書セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           MOVE    100             TO  WRK-CNT
           PERFORM 2402-ZENKAKU-HEN-SEC
           IF      FLG-ERR         =   ZERO
               MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-KISEI-BANTI
           END-IF
      *
           .
       2410-KISEI-BANTI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目４６：帰省先−電話番号　処理
      *****************************************************************
       2410-KISEI-TEL-SET-SEC      SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-KISEI-TEL-SET-EXT
           END-IF
      *
      *    帰省先−電話番号セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-DATA
           PERFORM 2401-SPACE-HENSYU-SEC
           MOVE    WRK-DATA (1:WRK-LEN)
                                   TO  PTINF-KISEI-TEL
      *
           .
       2410-KISEI-TEL-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目４７：テスト患者フラグ　処理
      *****************************************************************
       2410-TSTPTNUMKBN-SET-SEC    SECTION.
      *
      *    必須未入力
           IF      FLG-NODATA      =   1
               MOVE    1           TO  FLG-ERR
               GO  TO  2410-TSTPTNUMKBN-SET-EXT
           END-IF
      *
      *    項目桁数不正
           IF  WRK-MO-CNT          NOT =   1
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-TSTPTNUMKBN-SET-EXT
           END-IF
      *
      *    テスト患者フラグ
           EVALUATE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
               WHEN    "0"
               WHEN    "1"
                   MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  PTINF-TSTPTNUMKBN
               WHEN    OTHER
                   MOVE    3           TO  FLG-ERR
           END-EVALUATE
      *
           .
       2410-TSTPTNUMKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目４８：登録日　処理
      *****************************************************************
       2410-CREYMD-SET-SEC         SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-CREYMD-SET-EXT
           END-IF
      *
      *    項目桁数不正
           IF      WRK-MO-CNT      NOT =   8
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-CREYMD-SET-EXT
           END-IF
      *
      *    登録日セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           IF      FLG-YMD         =   1
               MOVE    3               TO  FLG-ERR
           ELSE
               MOVE    WRK-SYMD        TO  PTINF-CREYMD
           END-IF
      *
           .
       2410-CREYMD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目４９：更新日　処理
      *****************************************************************
       2410-UPYMD-SET-SEC             SECTION.
      *
      *    未入力
           IF      FLG-NODATA      =   1
               GO  TO  2410-UPYMD-SET-EXT
           END-IF
      *
      *    項目桁数不正
           IF  WRK-MO-CNT          NOT =   8
               MOVE    2           TO  FLG-ERR
               GO  TO  2410-UPYMD-SET-EXT
           END-IF
      *
      *    更新日セット
           MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                   TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           IF      FLG-YMD         =   1
               MOVE    3               TO  FLG-ERR
           ELSE
               MOVE    WRK-SYMD        TO  PTINF-UPYMD
           END-IF
      *
           .
       2410-UPYMD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報処理
      *****************************************************************
       250-PTINF-SET-SEC           SECTION.
      *
      *    患者ＩＤ取得処理
           IF  FLG-ERR                 =   ZERO
               PERFORM 250-PTNUM-GET-SEC
           END-IF
      *    患者番号変換マスタ　更新処理
           IF  FLG-ERR                 =   ZERO
               PERFORM 260-PTNUM-INSERT-SEC
           END-IF
      *    患者マスタ　更新処理
           IF  FLG-ERR                 =   ZERO
               PERFORM 270-PTINF-INSERT-SEC
           END-IF
      *
           .
       250-PTINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者ＩＤ取得処理
      *****************************************************************
       250-PTNUM-GET-SEC           SECTION.
      *
      *    患者番号採番検索
           PERFORM 2510-PTGETNUM-GET-SEC
           IF  FLG-SYSKANRI            =   1
               MOVE    5               TO  FLG-ERR
               GO  TO  250-PTNUM-GET-EXT
           END-IF
      *
      *****MOVE    MCPDATA-REC     TO  SYS-1009-REC
           EVALUATE    FLG-UPDATE
           WHEN    ZERO
      *        患者番号桁数算出処理
               PERFORM 2520-PTNUM-KETA-SEC
               GO  TO  250-PTNUM-GET-EXT
           WHEN    1
               COMPUTE SYS-1009-PTID   =   SYS-1009-PTID    +   1
               MOVE    SYS-1009-PTID   TO  SPA-PTID
           WHEN    2
               MOVE    PARA-04-1       TO  SYS-1009-RENNUM(1:10)
           WHEN    9
               MOVE    PARA-03-2       TO  SYS-1009-RENNUM(1:10)
               MOVE    PARA-03-3       TO  SYS-1009-PTID(1:10)
           END-EVALUATE
      *
      *    患者番号採番  更新
           MOVE    SPA-HOSPNUM     TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC    TO  MCPDATA-REC
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    5               TO  FLG-ERR
           END-IF
      *
           .
       250-PTNUM-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号採番検索
      *****************************************************************
       2510-PTGETNUM-GET-SEC       SECTION.
      *
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"          TO  SYS-1009-KANRICD
           MOVE    "*"             TO  SYS-1009-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1009-STYUKYMD
                                       SYS-1009-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1009-HOSPNUM
      *
           MOVE    SYS-1009-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-NEXT-SEC
               IF      FLG-SYSKANRI    =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1009-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2510-PTGETNUM-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号桁数算出処理
      *****************************************************************
       2520-PTNUM-KETA-SEC         SECTION.
      *
           EVALUATE    SYS-1009-PTNUMKSIKBN
           WHEN    "1"
               MOVE    SYS-1009-JIYKSIKETA
                                       TO  WRK-RENKETA
           WHEN    "2"
               MOVE    SYS-1009-HJNKSIRENNUMKETA
                                       TO  WRK-RENKETA
               IF  SYS-1009-HJNKSIKBN  =   "1"  OR  "2"
                   EVALUATE    SYS-1009-HJNKSINENKBN
                   WHEN    "1"
                       ADD     3           TO  WRK-RENKETA
                   WHEN    "2"
                       ADD     4           TO  WRK-RENKETA
                   END-EVALUATE
               END-IF
               IF  SYS-1009-HJNKSIKBN   =   "1"  OR  "3"
                   ADD     1            TO  WRK-RENKETA
               END-IF
           WHEN    "3"
               EVALUATE    SYS-1009-KKCKSIKBN
               WHEN    "1"
                   COMPUTE WRK-RENKETA  =   SYS-1009-KKCKSIMAEKETA
                                        +   SYS-1009-KKCKSIRENNUMKETA
                                        +   SYS-1009-KKCKSIATOKETA
               WHEN    "2"
                   COMPUTE WRK-RENKETA  =   SYS-1009-KKCKSIMAEKETA
                                        +   SYS-1009-KKCKSIRENNUMKETA
               WHEN    "3"
                   COMPUTE WRK-RENKETA  =   SYS-1009-KKCKSIRENNUMKETA
                                        +   SYS-1009-KKCKSIATOKETA
               END-EVALUATE
           END-EVALUATE
           .
      *
       2520-PTNUM-KETA-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号変換マスタ　更新処理
      *****************************************************************
       260-PTNUM-INSERT-SEC        SECTION.
      *
      *    存在チェック
           PERFORM 900-PTNUM-READ-SEC
           IF      FLG-PTNUM-ERR   NOT =   ZERO
               MOVE    6               TO  FLG-ERR
               GO  TO  260-PTNUM-INSERT-EXT
           END-IF
      *
           MOVE    SPACE               TO  PTNUM-REC
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    SPA-PTID            TO  PTNUM-PTID
           MOVE    SPA-PTNUM           TO  PTNUM-PTNUM
           MOVE    PTINF-TSTPTNUMKBN   TO  PTNUM-TSTPTNUMKBN
           MOVE    SPA-TERMID          TO  PTNUM-TERMID
           MOVE    PTINF-CREYMD        TO  PTNUM-CREYMD
           MOVE    PTINF-UPYMD         TO  PTNUM-UPYMD
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC         NOT =   ZERO
               MOVE    6              TO  FLG-ERR
               GO  TO  260-PTNUM-INSERT-EXT
           END-IF
           ADD     1               TO  CNT-PTNUM
      *
           .
       260-PTNUM-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ　登録処理
      *****************************************************************
       270-PTINF-INSERT-SEC        SECTION.
      *
      *    存在チェック
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       =   ZERO
               MOVE    7               TO  FLG-ERR
               GO  TO  270-PTINF-INSERT-EXT
           END-IF
      *
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    SPA-PTID        TO  PTINF-PTID
           MOVE    ZERO            TO  PTINF-CONDITION-1
           MOVE    ZERO            TO  PTINF-CONDITION-2
           MOVE    ZERO            TO  PTINF-CONDITION-3
           MOVE    ZERO            TO  PTINF-MAXZAINUM
           MOVE    ZERO            TO  PTINF-RRKNUM
           MOVE    ZERO            TO  PTINF-RRKEDANUM
      *    テスト患者区分が未設定の時は、０ とする
           IF      PTINF-TSTPTNUMKBN   =   SPACE
               MOVE    "0"             TO  PTINF-TSTPTNUMKBN
           END-IF
      *
           MOVE    SPA-TERMID      TO  PTINF-TERMID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "DBINSERT"      TO  MCP-FUNC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
                MOVE    7              TO  FLG-ERR
                GO  TO  270-PTINF-INSERT-EXT
           END-IF
           ADD     1               TO  CNT-PTINF
      *
           .
       270-PTINF-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーリスト編集処理
      *****************************************************************
       290-ERRREC-EDIT-SEC         SECTION.
      *
           IF      FLG-ERR         <=  3
               ADD     1               TO  IDE
               IF      IDE             >   4
                   GO  TO  290-ERRREC-EDIT-EXT
               END-IF
               IF      IDE                 =   1
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
      *    患者ＩＤ取得エラー
           WHEN    5
      *    患者番号出力エラー
           WHEN    6
      *    患者情報出力エラー
           WHEN    7
               PERFORM 2910-ERRREC-EDIT-BETU2-SEC
           END-EVALUATE
      *
           .
       290-ERRREC-EDIT-EXT.
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
      *    ２：カナ氏名
           WHEN    2
               MOVE    "カナ氏名:"     TO  ERR1-KOMOKU(IDE)
      *    ３：漢字氏名
           WHEN    3
               MOVE    "漢字氏名:"     TO  ERR1-KOMOKU(IDE)
      *    ４：通称
           WHEN    4
               MOVE    "通称    :"     TO  ERR1-KOMOKU(IDE)
      *    ５：性別
           WHEN    5
               MOVE    "性別    :"     TO  ERR1-KOMOKU(IDE)
      *    ６：生年月日
           WHEN    6
               MOVE    "生年月日:"     TO  ERR1-KOMOKU(IDE)
      *    ７：死亡区分
           WHEN    7
               MOVE    "死亡区分:"     TO  ERR1-KOMOKU(IDE)
      *    ８：自宅−郵便番号
           WHEN    8
               MOVE    "自宅郵便:"     TO  ERR1-KOMOKU(IDE)
      *    ９：自宅−住所
           WHEN    9
               MOVE    "自宅住所:"     TO  ERR1-KOMOKU(IDE)
      *    １０：自宅−番地
           WHEN    10
               MOVE    "自宅番地:"     TO  ERR1-KOMOKU(IDE)
      *    １１：自宅−電話番号（昼）
           WHEN    11
               MOVE    "自宅電昼:"     TO  ERR1-KOMOKU(IDE)
      *    １２：自宅−電話番号（夜）
           WHEN    12
               MOVE    "自宅電夜:"     TO  ERR1-KOMOKU(IDE)
      *    １３：世帯主
           WHEN    13
               MOVE    "世帯主  :"     TO  ERR1-KOMOKU(IDE)
      *    １４：続柄
           WHEN    14
               MOVE    "続柄    :"     TO  ERR1-KOMOKU(IDE)
      *    １５：職業
           WHEN    15
               MOVE    "職業    :"     TO  ERR1-KOMOKU(IDE)
      *    １６：携帯電話番号
           WHEN    16
               MOVE    "携帯    :"     TO  ERR1-KOMOKU(IDE)
      *    １７：ＦＡＸ番号
           WHEN    17
               MOVE    "ＦＡＸ  :"     TO  ERR1-KOMOKU(IDE)
      *    １８：電子メールアドレス
           WHEN    18
               MOVE    "メール  :"     TO  ERR1-KOMOKU(IDE)
      *    １９：減免事由
           WHEN    19
               MOVE    "減免事由:"     TO  ERR1-KOMOKU(IDE)
      *    ２０：割引区分
           WHEN    20
               MOVE    "割引区分:"     TO  ERR1-KOMOKU(IDE)
      *    ２１：割引
           WHEN    21
               MOVE    "割引    :"     TO  ERR1-KOMOKU(IDE)
      *    ２２：コメント１
           WHEN    22
               MOVE    "コメ１  :"     TO  ERR1-KOMOKU(IDE)
      *    ２３：コメント２
           WHEN    23
               MOVE    "コメ２  :"     TO  ERR1-KOMOKU(IDE)
      *    ２４：禁忌１
           WHEN    24
               MOVE    "禁忌１  :"     TO  ERR1-KOMOKU(IDE)
      *    ２５：禁忌２
           WHEN    25
               MOVE    "禁忌２  :"     TO  ERR1-KOMOKU(IDE)
      *    ２６：アレルギー１
           WHEN    26
               MOVE    "アレル１:"     TO  ERR1-KOMOKU(IDE)
      *    ２７：アレルギー２
           WHEN    27
               MOVE    "アレル２"      TO  ERR1-KOMOKU(IDE)
      *    ２８：感染症１
           WHEN    28
               MOVE    "感染症１:"     TO  ERR1-KOMOKU(IDE)
      *    ２９：感染症２
           WHEN    29
               MOVE    "感染症２:"     TO  ERR1-KOMOKU(IDE)
      *    ３０：連絡先−名称
           WHEN    30
               MOVE    "連絡名称:"     TO  ERR1-KOMOKU(IDE)
      *    ３１：連絡先−続柄
           WHEN    31
               MOVE    "連絡続柄:"     TO  ERR1-KOMOKU(IDE)
      *    ３２：連絡先−郵便番号
           WHEN    32
               MOVE    "連絡郵便:"     TO  ERR1-KOMOKU(IDE)
      *    ３３：連絡先−住所
           WHEN    33
               MOVE    "連絡住所:"     TO  ERR1-KOMOKU(IDE)
      *    ３４：連絡先−番地方書
           WHEN    34
               MOVE    "連絡番地:"     TO  ERR1-KOMOKU(IDE)
      *    ３５：連絡先−電話番号（昼）
           WHEN    35
               MOVE    "連絡電昼:"     TO  ERR1-KOMOKU(IDE)
      *    ３６：連絡先−電話番号（夜）
           WHEN    36
               MOVE    "連絡電夜:"     TO  ERR1-KOMOKU(IDE)
      *    ３７：勤務先−名称
           WHEN    37
               MOVE    "勤務名称:"     TO  ERR1-KOMOKU(IDE)
      *    ３８：勤務先−郵便番号
           WHEN    38
               MOVE    "勤務郵便:"     TO  ERR1-KOMOKU(IDE)
      *    ３９：勤務先−住所
           WHEN    39
               MOVE    "勤務住所:"     TO  ERR1-KOMOKU(IDE)
      *    ４０：勤務先−番地方書
           WHEN    40
               MOVE    "勤務番地:"     TO  ERR1-KOMOKU(IDE)
      *    ４１：勤務先−電話番号
           WHEN    41
               MOVE    "勤務電話:"     TO  ERR1-KOMOKU(IDE)
      *    ４２：帰省先−名称
           WHEN    42
               MOVE    "帰省名称:"     TO  ERR1-KOMOKU(IDE)
      *    ４３：帰省先−郵便番号
           WHEN    43
               MOVE    "帰省郵便:"     TO  ERR1-KOMOKU(IDE)
      *    ４４：帰省先−住所
           WHEN    44
               MOVE    "帰省住所:"     TO  ERR1-KOMOKU(IDE)
      *    ４５：帰省先−番地方書
           WHEN    45
               MOVE    "帰省番地:"     TO  ERR1-KOMOKU(IDE)
      *    ４６：帰省先−電話番号
           WHEN    46
               MOVE    "帰省電話:"     TO  ERR1-KOMOKU(IDE)
      *    ４７：テスト患者フラグ
           WHEN    47
               MOVE    "テスト患:"     TO  ERR1-KOMOKU(IDE)
      *    ４８：登録日
           WHEN    48
               MOVE    "登録日  :"     TO  ERR1-KOMOKU(IDE)
      *    ４９：更新日
           WHEN    49
               MOVE    "更新日  :"     TO  ERR1-KOMOKU(IDE)
           END-EVALUATE
      *
      *    エラー値転送
           IF      WRK-MO-CNT          NOT =   ZERO
               MOVE    CSV-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  ERR1-ATAI(IDE)(1:8)
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
      *    エラー内容番号転送
           MOVE    CNT-CSV         TO  ERR2-LINENO
           MOVE    "["             TO  ERR2-ERRNO(1:1)
           MOVE    FLG-ERR(1:1)    TO  ERR2-ERRNO(2:1)
           MOVE    "]"             TO  ERR2-ERRNO(3:1)
           MOVE    "患者番号:"     TO  ERR2-PTNUMMES
           MOVE    "患者ＩＤ:"     TO  ERR2-PTIDMES
           MOVE    "患者氏名:"     TO  ERR2-NAMEMES
           MOVE    "ERR"           TO  ERR2-NAIYO
      *
           MOVE    SPA-PTNUM       TO  ERR2-PTNUM
           MOVE    SPA-PTID        TO  ERR2-PTID
           MOVE    PTINF-NAME      TO  ERR2-NAME
      *
           .
       2910-ERRREC-EDIT-BETU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーリスト出力処理
      *****************************************************************
       290-ERRLST-OUT-SEC          SECTION.
      *
           IF      FLG-ERRARI      =   1
               MOVE    ERR-REC1        TO  ERR-REC
               WRITE   ERR-REC
           ELSE
               MOVE    ERR-REC2        TO  ERR-REC
               WRITE   ERR-REC
           END-IF
      *
           .
       290-ERRLST-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力　初期処理
      *****************************************************************
       2910-ERRLST-INIT-SEC        SECTION.
      *
           OPEN    OUTPUT              ERR-FILE
           IF      STS-LST             NOT =   ZERO
               MOVE    1               TO  FLG-CSV
               DISPLAY "*(ORCVTPTINF)* ERRFILE OPN STS[" STS-LST "]"
               GO  TO  2910-ERRLST-INIT-EXT
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
       2910-ERRLST-INIT-EXT.
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
           MOVE    2               TO  FLG-UPDATE
           PERFORM 250-PTNUM-GET-SEC
      *
      *    DB DISCONNECT
           PERFORM 900-DBDISCONNECT-SEC
      *
      *    帳票出力　終了処理
           CLOSE                       CSV-FILE
           CLOSE                       ERR-FILE
      *
           DISPLAY "*(ORCVTPTINF)* CSV     /I CNT[" CNT-CSV      "]"
           DISPLAY "*(ORCVTPTINF)* PTINF   /O CNT[" CNT-PTINF    "]"
           DISPLAY "*(ORCVTPTINF)* ERR     /O CNT[" CNT-ERR      "]"
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
           IF      MCP-RC              =   ZERO
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
      *    患者番号読み込み　処理
      *****************************************************************
       900-PTNUM-READ-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNUM-ERR
      *
           MOVE    ZERO            TO  FLG-PTNUM
           MOVE    SPACE           TO  PTNUM-REC
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    SPA-PTID        TO  PTNUM-PTID
      *
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF          MCP-RC          =   ZERO
               MOVE    "tbl_ptnum"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF          MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-PTNUM
               ELSE
                   MOVE    1               TO  FLG-PTNUM
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    患者ＩＤが存在する時、エラー
           IF      FLG-PTNUM       =   ZERO
               MOVE    1               TO  FLG-PTNUM-ERR
               DISPLAY "*(ORCVTPTINF)* PTID  ERR [" SPA-PTID  "]"
               MOVE    1               TO  FLG-CSV
           ELSE
      *        患者番号存在チェック
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
               IF          MCP-RC          =   ZERO
                   MOVE    "tbl_ptnum"     TO  MCP-TABLE
                   MOVE    "key4"          TO  MCP-PATHNAME
                   PERFORM 920-DBFETCH-SEC
                   IF          MCP-RC          =   ZERO
                       MOVE    ZERO            TO  FLG-PTNUM
                   ELSE
                       MOVE    1               TO  FLG-PTNUM
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-PTNUM
               END-IF
      *
               MOVE    "tbl_ptnum"     TO  MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *        患者番号が存在する時、エラー
               IF      FLG-PTNUM       =   ZERO
                   MOVE    2               TO  FLG-PTNUM-ERR
                   DISPLAY "*(ORCVTPTINF)* PTNUM ERR [" SPA-PTNUM "]"
                   MOVE    1               TO  FLG-CSV
               END-IF
           END-IF
      *
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報読み込み　処理
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    SPA-PTID        TO  PTINF-PTID
      *
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF          MCP-RC          =   ZERO
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF          MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-PTINF
               ELSE
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者ＣＳＶファイル検索処理
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
