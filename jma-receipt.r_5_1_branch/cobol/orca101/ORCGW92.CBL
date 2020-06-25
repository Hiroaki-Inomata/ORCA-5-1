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
       IDENTIFICATION                DIVISION.
       PROGRAM-ID.                   ORCGW92.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : ＥＦファイル作成設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  14/04/22    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-伊藤    15/08/19  GINBEE対応
      *  04.08.01    NACL-藤原    15/08/24  公費のみの診療（医療保険のな
      *                                     い診療）のＥＦ出力可否対応
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *
       FILE-CONTROL.
      *    ＥＦファイル用デイレクトリ出力先チェック用ワークファイル
           SELECT  WRK-FILE        ASSIGN  WRKPARA
                                   FILE    STATUS  IS  STS-WRK.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *    ＥＦファイル用デイレクトリ出力先チェック用ワークファイル
       FD  WRK-FILE.
       01  WRK-R                       PIC X(10).
      *
       WORKING-STORAGE             SECTION.
      *
      *    ＥＦファイル用デイレクトリ出力先チェック用ワークファイル
       01  STS-AREA.
           03  STS-WRK                 PIC X(02).
       01  WRKPARA                     PIC X(210).
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    コンスタント値
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
       01  SPA-W92.
           03  SPA-GMN-CUR                 PIC 9(03).
           03  SPA-GMN-MODE                PIC 9(01).
           03  SPA-GMN-AREA.
               05  SPA-GMN-EFOUT-G.
                   07  SPA-GMN-EFOUT       PIC X(01).
                   07  SPA-GMN-EFOUT-F1    PIC X(01).
                   07  SPA-GMN-EFOUT-MEI   PIC X(18).
               05  SPA-GMN-EFOUT-LST-G.
                   07  SPA-GMN-EFOUT-LST   PIC X(20)
                                               OCCURS   10.
               05  SPA-GMN-EFOUT-MAX       PIC 9(02).
      *
               05  SPA-GMN-FILE-DIR        PIC X(200).
      *
               05  SPA-GMN-SRYCD-G.
                   07  SPA-GMN-SRYCD-OCC   OCCURS   2. 
                       09  SPA-GMN-SRYCD   PIC X(09).
                       09  SPA-GMN-NAME    PIC X(100).
               05  SPA-GMN-NUMBER-G.
                   07  SPA-GMN-NUMBER       PIC X(01).
                   07  SPA-GMN-NUMBER-F1    PIC X(01).
                   07  SPA-GMN-NUMBER-MEI   PIC X(18).
               05  SPA-GMN-NUMBER-LST-G.
                   07  SPA-GMN-NUMBER-LST   PIC X(20)
                                               OCCURS   5.
               05  SPA-GMN-NUMBER-MAX       PIC 9(02).
               05  SPA-GMN-KOH-RECEIPT-G.
                   07  SPA-GMN-KOH-RECEIPT     PIC X(01).
                   07  SPA-GMN-KOH-RECEIPT-F1  PIC X(01).
                   07  SPA-GMN-KOH-RECEIPT-MEI PIC X(18).
               05  SPA-GMN-KOH-RECEIPT-LST-G.
                   07  SPA-GMN-KOH-RECEIPT-LST PIC X(20)
                                               OCCURS   5.
               05  SPA-GMN-KOH-RECEIPT-MAX     PIC 9(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
      *
           03  FLG-CHK                     PIC 9(01).
           03  FLG-OK                      PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(05).
           03  IDY                         PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
      *    入力項目名
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-WIDMSG                  PIC X(100).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    ＥＦファイルデータ
           COPY    "CPSK9102.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA101SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
                                               MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W92
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"         ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W92         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "WERR" )
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF 
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "W92    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC                  SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
                   MOVE    SPACE           TO  SPA-WID1-FLG
                   MOVE    SPACE           TO  SPA-WIDCD
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-W92
      *
           MOVE    "0 データ作成しない"
                                       TO  SPA-GMN-EFOUT-LST(1)
           MOVE    "1 データ作成する"
                                       TO  SPA-GMN-EFOUT-LST(2)
           MOVE    2                   TO  SPA-GMN-EFOUT-MAX
      *
           MOVE    "0 患者番号を記録する"
                                       TO  SPA-GMN-NUMBER-LST(1)
           MOVE    "1 患者ＩＤを記録する"
                                       TO  SPA-GMN-NUMBER-LST(2)
           MOVE    2                   TO  SPA-GMN-NUMBER-MAX
      *
           MOVE    "0 ＥＦ記録する"    TO  SPA-GMN-KOH-RECEIPT-LST(1)
           MOVE    "1 ＥＦ記録しない"  TO  SPA-GMN-KOH-RECEIPT-LST(2)
           MOVE    2                   TO  SPA-GMN-KOH-RECEIPT-MAX
      *
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "9102"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-9102-REC
               MOVE    SYS-9102-EFOUT      TO  SPA-GMN-EFOUT
               MOVE    SYS-9102-FILE-DIR   TO  SPA-GMN-FILE-DIR
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       2
                       OR     SYS-9102-SRYCD (IDX) =   SPACE
                   MOVE    SYS-9102-SRYCD (IDX)
                                               TO  SPA-GMN-SRYCD (IDX)
                   INITIALIZE                      TNS-TENSU-REC
                   MOVE    SPA-GMN-SRYCD (IDX) TO  TNS-SRYCD
                   PERFORM 900-TENSU-READ-SEC
                   MOVE    TNS-NAME            TO  SPA-GMN-NAME  (IDX)
               END-PERFORM
               MOVE    SYS-9102-NUMBER     TO  SPA-GMN-NUMBER
               MOVE    SYS-9102-KOH-RECEIPT
                                           TO  SPA-GMN-KOH-RECEIPT
           END-IF
      *
           IF      SPA-GMN-EFOUT       =   SPACE
               MOVE    "0"                 TO  SPA-GMN-EFOUT
           END-IF
           PERFORM 4102-EFOUT-CHK-SEC
           IF      SPA-GMN-NUMBER      =   SPACE
               MOVE    "0"                 TO  SPA-GMN-NUMBER
           END-IF
           PERFORM 4105-NUMBER-CHK-SEC
           IF      SPA-GMN-KOH-RECEIPT =   SPACE
               MOVE    "0"                 TO  SPA-GMN-KOH-RECEIPT
           END-IF
           PERFORM 4106-KOH-RECEIPT-CHK-SEC
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＷＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-WIDCD
               WHEN "1001"
      *        登録処理
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 4801-TOUROKU-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
               WHEN "1003"
      *        削除処理
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 4301-DELETE-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
           END-EVALUATE
      *
           .
       330-WID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM  210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 420-CLEAR-SEC
      *    削除
           WHEN    "CLICKED"       ALSO    "B03"
               PERFORM 430-DELETE-SEC
      *    登録
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 480-TOUROKU-SEC
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話処理
      *****************************************************************
       200-ENTRY                             SECTION.
      *
      *    入力個所のセット
           PERFORM 400-GMN-INPUT-SEC
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           .
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC           SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    W92-EFOUT           TO  SPA-GMN-EFOUT-G
           MOVE    W92-FILEDIR         TO  SPA-GMN-FILE-DIR
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       2
               MOVE    W92-SRYCD (IDX)    TO  SPA-GMN-SRYCD (IDX)
           END-PERFORM
           MOVE    W92-NUMBER          TO  SPA-GMN-NUMBER-G
           MOVE    W92-KOH-RECEIPT     TO  SPA-GMN-KOH-RECEIPT-G
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    ＥＦファイル用データ作成区分
           PERFORM 4102-EFOUT-CHK-SEC
      *
      *    連携基盤ディレクトリ（PANDA環境のみ）
           IF     (SPA-MW              =   SPA-PANDA)  AND
                  (SPA-ERRCD           =   SPACE)
               PERFORM 4103-FILEDIR-CHK-SEC
           END-IF
      *
      *    退院時処方指示
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4104-SRYCD-CHK-SEC
           END-IF
      *
      *    データ識別番号記録
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4105-NUMBER-CHK-SEC
           END-IF
      *
      *    公費のみ診療出力可否
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4106-KOH-RECEIPT-CHK-SEC
           END-IF
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＥＦファイル用データ作成区分チェック処理
      *****************************************************************
       4102-EFOUT-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
           IF      SPA-GMN-EFOUT  =   SPACE
               MOVE    SPA-GMN-EFOUT-LST (1)
                                       TO  SPA-GMN-EFOUT-G
               MOVE    1               TO  FLG-OK
           ELSE
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       SPA-GMN-EFOUT-MAX
                   IF      SPA-GMN-EFOUT   =   SPA-GMN-EFOUT-LST
                                                         (IDX)(1:1)
                       MOVE    SPA-GMN-EFOUT-LST (IDX)
                                               TO  SPA-GMN-EFOUT-G
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       4102-EFOUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ファイルデイレクトリチェック処理
      *****************************************************************
       4103-FILEDIR-CHK-SEC        SECTION.
      *
           IF      SPA-GMN-FILE-DIR    =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    200                 TO  KANACHK-MAX-CNT
               MOVE    SPA-GMN-FILE-DIR    TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-RC          =   ZERO  )  AND
                      (KANACHK-SYUBETU     =   1     )
      *            １桁目と最終桁のチェック
                   IF     (SPA-GMN-FILE-DIR(1:1)   NOT =   "/" )
                       MOVE    "0002"          TO  SPA-ERRCD
                   END-IF
                   IF     (SPA-GMN-FILE-DIR(KANACHK-MAX:1)
                                                   =   "/" )
                    AND   (SPA-ERRCD               =   SPACE)
                       MOVE    "0003"          TO  SPA-ERRCD
                   END-IF
               ELSE
      *            半角以外はエラー
                   MOVE    "0004"          TO  SPA-ERRCD
               END-IF
      *
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 41031-FILE-CHK-SEC
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    2               TO  SPA-GMN-CUR
           END-IF
      *
           .
       4103-FILEDIR-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ディレクトリ位置チェック処理
      *****************************************************************
       41031-FILE-CHK-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRKPARA
           STRING  SPA-GMN-FILE-DIR    DELIMITED   BY  SPACE
                   "/W92.txt"          DELIMITED   BY  SIZE
                                       INTO    WRKPARA
           END-STRING
      *
           OPEN    OUTPUT              WRK-FILE
           IF      STS-WRK             =   "00"
               CLOSE   WRK-FILE
               MOVE    ZERO                TO  IF-RESULT
               MOVE    WRKPARA             TO  IF-FILENAME
               CALL    "orcfiledel"    USING   ORCSFDELAREA
           ELSE
               MOVE    "0005"          TO  SPA-ERRCD
           END-IF
           .
       41031-FILE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    退院時処方指示チェック処理
      *****************************************************************
       4104-SRYCD-CHK-SEC          SECTION.
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       2
               MOVE    SPACE               TO  SPA-GMN-NAME  (IDX)
               IF      SPA-GMN-SRYCD  (IDX)    =  SPACE
                   CONTINUE
               ELSE 
                   INITIALIZE                      TNS-TENSU-REC
                   MOVE    SPA-GMN-SRYCD (IDX) TO  TNS-SRYCD
                   PERFORM 900-TENSU-READ-SEC
                   IF      FLG-TENSU           =   ZERO
                       MOVE    TNS-NAME            TO  SPA-GMN-NAME(IDX)
                   ELSE
                       MOVE    "0006"              TO  SPA-ERRCD
                       COMPUTE SPA-GMN-CUR =   2   +   IDX
                       MOVE    2                   TO  IDX                       
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       4104-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ識別番号記録チェック処理
      *****************************************************************
       4105-NUMBER-CHK-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
           IF      SPA-GMN-NUMBER  =   SPACE
               MOVE    SPA-GMN-NUMBER-LST (1)
                                       TO  SPA-GMN-NUMBER-G
               MOVE    1               TO  FLG-OK
           ELSE
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       SPA-GMN-NUMBER-MAX
                   IF      SPA-GMN-NUMBER  =   SPA-GMN-NUMBER-LST
                                                         (IDX)(1:1)
                       MOVE    SPA-GMN-NUMBER-LST (IDX)
                                               TO  SPA-GMN-NUMBER-G
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0007"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       4105-NUMBER-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費のみのレセプトチェック処理
      *****************************************************************
       4106-KOH-RECEIPT-CHK-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
           IF      SPA-GMN-KOH-RECEIPT  =   SPACE
               MOVE    SPA-GMN-KOH-RECEIPT-LST (1)
                                       TO  SPA-GMN-KOH-RECEIPT-G
               MOVE    1               TO  FLG-OK
           ELSE
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       SPA-GMN-KOH-RECEIPT-MAX
                   IF      SPA-GMN-KOH-RECEIPT
                                       =   SPA-GMN-KOH-RECEIPT-LST
                                                         (IDX)(1:1)
                       MOVE    SPA-GMN-KOH-RECEIPT-LST (IDX)
                                               TO  SPA-GMN-KOH-RECEIPT-G
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       4106-KOH-RECEIPT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W92"               TO  SPA-MOTOPG
      *
           MOVE    "W01"               TO  MCP-WINDOW
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
      *    条件クリア処理
      *****************************************************************
       420-CLEAR-SEC               SECTION.
      *
           PERFORM 310-SPASET-SEC
           MOVE    SPACE               TO  SPA-ERRCD
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       430-DELETE-SEC              SECTION.
      *
           MOVE    "1003"              TO  SPA-WIDCD
      *
           .
       430-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       4301-DELETE-SYORI-SEC              SECTION.
      *
      *    システム管理検索
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "9102"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               IF      MCP-RC              NOT =   ZERO 
                   MOVE    "9003"              TO  SPA-ERRCD
                   DISPLAY "W92 UPD ERR SYSKANRI-KEY =>" SYS-KEY
                           ",MCP-RC:" MCP-RC 
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE   "JOIN"           TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
      *
           .
       4301-DELETE-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       480-TOUROKU-SEC                 SECTION.
      *
      *    入力個所のチェック
           PERFORM 410-INPUT-CHK-SEC
      *
      *    関連チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4801-KANRENCHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "1001"          TO  SPA-WIDCD
           END-IF
      *
           .
       480-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    共通関連チェック
      *****************************************************************
       4801-KANRENCHK-SEC              SECTION.
      *
           IF      SPA-GMN-EFOUT    =   "1"
               IF     (SPA-MW              =   SPA-PANDA)  AND
                      (SPA-GMN-FILE-DIR    =   SPACE)
                   MOVE    "0010"          TO  SPA-ERRCD
                   MOVE    2               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF      SPA-ERRCD         =   SPACE
               IF      SPA-GMN-SRYCD (1) =   SPACE
                   CONTINUE 
               ELSE
                   IF      SPA-GMN-SRYCD (1) =   SPA-GMN-SRYCD (2)
                       MOVE    "0011"          TO  SPA-ERRCD
                       MOVE    3               TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
           .
      *
       4801-KANRENCHK-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       4801-TOUROKU-SEC                SECTION.
      *
      *    システム管理検索
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "9102"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-9102-REC
           ELSE
               MOVE    SPACE               TO  SYS-9102-REC
               INITIALIZE                      SYS-9102-REC
               MOVE    "9102"              TO  SYS-9102-KANRICD
               MOVE    "*"                 TO  SYS-9102-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD    TO  SYS-9102-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-9102-EDYUKYMD
           MOVE    SPA-GMN-EFOUT       TO  SYS-9102-EFOUT
           MOVE    SPA-GMN-FILE-DIR    TO  SYS-9102-FILE-DIR
           INITIALIZE                      SYS-9102-SRYCD-TBL
           MOVE    ZERO                TO  IDY
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       2
               IF      SPA-GMN-SRYCD (IDX) =   SPACE
                   CONTINUE
               ELSE
                   ADD     1                   TO  IDY
                   MOVE    SPA-GMN-SRYCD (IDX) TO  SYS-9102-SRYCD (IDY)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-NUMBER      TO  SYS-9102-NUMBER
           MOVE    SPA-GMN-KOH-RECEIPT TO  SYS-9102-KOH-RECEIPT
      *
           MOVE    SYS-9102-REC        TO  SYSKANRI-REC
      *    ＤＢ更新処理
           PERFORM 4910-DBKOUSIN-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE   "JOIN"           TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
      *
           .
       4801-TOUROKU-EXT.
           EXIT.
      *
      ******************************************************************
      *       登録処理
      *****************************************************************
       4910-DBKOUSIN-SEC        SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SPA-OPID                TO  SYS-OPID
               MOVE    SMCNDATE-YMD            TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
      *
               MOVE    SYS-KANRICD             TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD               TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    SPA-HOSPNUM             TO  SYSUP-HOSPNUM
               MOVE    SPA-HOSPNUM             TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC            TO  MCPDATA-REC
      *
               MOVE    "DBUPDATE"              TO  MCP-FUNC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "upd1"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC            NOT =   ZERO 
                   MOVE    "9002"                  TO  SPA-ERRCD
                   DISPLAY "W92 UPD ERR SYSKANRI-KEY =>" SYS-KEY
                           ",MCP-RC:" MCP-RC 
               END-IF 
           ELSE
               MOVE    SPA-OPID                TO  SYS-OPID
               MOVE    SMCNDATE-YMD            TO  SYS-CREYMD
                                                   SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
               MOVE    SPA-HOSPNUM             TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC            TO  MCPDATA-REC
      *
               MOVE    "DBINSERT"              TO  MCP-FUNC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC            NOT =   ZERO 
                   MOVE    "9001"                  TO  SPA-ERRCD
                   DISPLAY "W92 INS ERR SYSKANRI-KEY =>" SYS-KEY
                           ",MCP-RC:" MCP-RC 
               END-IF 
           END-IF
      *
           .
       4910-DBKOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "W92    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  W92
           INITIALIZE                      W92
      *
           MOVE    SPA-GMN-EFOUT-G  TO  W92-EFOUT
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL   IDX         >   SPA-GMN-EFOUT-MAX
               MOVE    SPA-GMN-EFOUT-LST(IDX)
                                           TO  W92-EFOUT-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-EFOUT-MAX   TO  W92-EFOUT-COUNT
      *
           MOVE    SPA-GMN-FILE-DIR    TO  W92-FILEDIR
      *
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL   IDX         >       2
               MOVE    SPA-GMN-SRYCD (IDX) TO  W92-SRYCD (IDX)
               MOVE    SPA-GMN-NAME  (IDX) TO  W92-NAME  (IDX)
           END-PERFORM
      *
           MOVE    SPA-GMN-NUMBER-G    TO  W92-NUMBER
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL   IDX         >   SPA-GMN-NUMBER-MAX
               MOVE    SPA-GMN-NUMBER-LST(IDX)
                                           TO  W92-NUMBER-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-NUMBER-MAX
                                       TO  W92-NUMBER-COUNT
      *
           MOVE    SPA-GMN-KOH-RECEIPT-G
                                       TO  W92-KOH-RECEIPT
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL   IDX         >   SPA-GMN-KOH-RECEIPT-MAX
               MOVE    SPA-GMN-KOH-RECEIPT-LST(IDX)
                                           TO  W92-KOH-RECEIPT-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-KOH-RECEIPT-MAX
                                       TO  W92-KOH-RECEIPT-COUNT
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
           AND   ( SPA-GMN-CUR         =   ZERO  )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE    "EFOUT"             TO  MCP-WIDGET
               WHEN    2
                   MOVE    "FILEDIR"           TO  MCP-WIDGET
               WHEN    3
                   MOVE    "SRYCD1"            TO  MCP-WIDGET
               WHEN    4
                   MOVE    "SRYCD2"            TO  MCP-WIDGET
               WHEN    5
                   MOVE    "NUMBER"            TO  MCP-WIDGET
               WHEN    6
                   MOVE    "KOH_RECEIPT"       TO  MCP-WIDGET
               WHEN    90
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "EFOUT"
                   MOVE    2               TO  SPA-GMN-CUR
               WHEN    "FILEDIR"
                   MOVE    3               TO  SPA-GMN-CUR
               WHEN    "SRYCD1"
                   MOVE    4               TO  SPA-GMN-CUR
               WHEN    "SRYCD2"
                   MOVE    5               TO  SPA-GMN-CUR
               WHEN    "NUMBER"
                   MOVE    6              TO  SPA-GMN-CUR
               WHEN    "KOH_RECEIPT"
                   MOVE    90              TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "ＥＦファイル用データ作成区分入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   STRING  "ＥＦファイル用ディレクトリの先頭は/から"
                           "指定して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0003"
                   STRING  "ＥＦファイル用ディレクトリの末尾は/を省略"
                           "して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0004"
                   STRING  "ＥＦファイル用ディレクトリはすべて半角で"
                           "入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0005"
                   STRING  "ＥＦファイル用ディレクトリが存在しないか"
                           "又は権限がありません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0006"
                   MOVE    "退院時処方指示入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "データ識別番号記録入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "公費のみのレセプト入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "ＥＦファイル用ディレクトリを入力して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "退院時処方指示が重複して入力してあります"
                                       TO  SPA-ERRMSG
               WHEN    "9001"
                   MOVE    "登録処理に失敗しました"
                                       TO  SPA-ERRMSG
               WHEN    "9002"
                   MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
               WHEN    "9003"
                   MOVE    "削除処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W92"               TO  SPA-MOTOPG
           MOVE    "WERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "登録します。よろしいですか"
                                            TO  WRK-WIDMSG
               WHEN    "1003"
                   MOVE    "削除します。よろしいですか"
                                            TO  WRK-WIDMSG
           END-EVALUATE
      *
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W92"                TO  SPA-MOTOPG
           MOVE    "WID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読込
      *****************************************************************
       900-SYSKANRI-INV-SEC        SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読込
      *****************************************************************
       900-TENSU-READ-SEC               SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    SPA-W01-STYUKYMD    TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM   920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                      TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                      TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
