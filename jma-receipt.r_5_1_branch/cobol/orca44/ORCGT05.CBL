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
       PROGRAM-ID.             ORCGT05.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 総括表・公費請求書
      *  コンポーネント名  : オンライン請求結果データ取込
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/09/05    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.04.01    NACL-藤原    08/12/10  返戻対応
      *  04.04.02    NACL-藤原    10/12/10  返戻ファイルの保存用ファイル名
      *                                     の変更
      *
      *  04.05.01    NACL-藤原    09/07/23  返戻ファイルの小文字対応
      *
      *  04.06.01    NACL-藤原    10/12/20  保険者からの返戻対応
      *  04.06.02    NACL-藤原    11/02/10  登録済エラー時の情報追加
      *  04.06.03    NACL-藤原    11/02/14  OpenCOBOL 1.0.0 対応
      *  04.06.04    NACL-藤原    12/06/19  取り込み時のレコード長変更
      *
      *  04.07.01    NACL-藤原    12/08/01  レセ電データチェック機能対応
      *  04.07.02    NACL-藤原    13/10/07  労災レセ電対応
      *  04.07.03    NACL-藤原    15/05/11  ファイルの文字コードチェック
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.02    NACL-藤原    16/06/27  医療機関コード変更時のレセ電
      *                                     データ等の作成対応
      *                                      （１日からの変更のみ）
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  ERR-FILE        ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-ERR.
      *
      *    パラメータファイル
           SELECT  PARA-FILE       ASSIGN  ASS-PARA
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  ERR-FILE.
       01  ERR-REC             PIC X(200).
      *
      *    パラメータファイル
       FD  PARA-FILE.
       01  PARA-REC            PIC X(3000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING   //TEMPFLPARA//
                                   BY          //RECEERR//.
      *
           COPY    "CPERRFL.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    レセプト作成指示共通パラメタ
           COPY    "T01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "T01SCR-SPA".
           COPY    "CPORCSPRTLNK.INC"
                                   REPLACING  //LNK-//
                                   BY         //SPA-LNK-//.
      *
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA                PIC X(02).
           03  STS-ERR                 PIC X(02).
      *
       01  WRK-AREA.
           03  WRK-SELNUM              PIC S9(05).
      *
           03  WRK-T05-DATALST-ROW     PIC S9(09).
      *
           03  WRK-GMN-CUR             PIC 9(02).
           03  WRK-MCP-WIDGET          PIC X(64).
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-POINT-AREA.
               05  WRK-PO-ST           PIC 9(04).
               05  WRK-PO-ED           PIC 9(04).
           03  WRK-KOUMOKU-NO          PIC 9(04).
      *
           03  WRK-MOVE-AREA.
               05  WRK-MO-ST           PIC 9(04).
               05  WRK-MO-CNT          PIC 9(04).
               05  WRK-WK              PIC 9(04).
           03  WRK-FILENM.
               05  WRK-FILENMSS        PIC X(01)  OCCURS   50.
      *
           03  WRK-NUM                 PIC ZZ.
           03  WRK-NUM3                PIC ZZZ.
           03  WRK-NUM5                PIC ZZZZZ.
      *
           03  WK-HEAD-LINE            PIC 9(03).
           03  WK-BODY-LINE            PIC 9(03).
           03  WRK-TITLE               PIC X(100).
           03  WRK-ERRMSG1             PIC 9(02).
           03  WRK-ERRMSG2             PIC 9(02).
      *
           03  WRK-PREFNUM             PIC X(02).
           03  WRK-HOSPCD              PIC X(07).
           03  WRK-TEISYUTUSAKI        PIC X(01).
           03 WRK-SRYYM                PIC X(06).
      *
           03  WRK-INDEX.
               05  WRK-INDEX-9         PIC 9(02).
      *
           03  WRK-SRYYM-HOSPCD        PIC 9(07).
           03  WRK-SYORIYM-HOSPCD      PIC 9(07).
      *
           03  WRK-ERRMSG-HENSYU-AREA.
               05  WRK-ERRMSG-TEISYUTUSAKI
                                       PIC X(04).
               05  WRK-ERRMSG-SRYYM    PIC X(06).
               05  WRK-ERRMSG-HKNJA    PIC X(20).
               05  WRK-ERRMSG-HOSPCD   PIC X(02).
      *
           03  WRK-SEIKYU-DATA.
               05  WRK-SEIKYU-SKYYM    PIC X(05).
               05  WRK-SEIKYU-SRYYM    PIC X(05).
               05  WRK-SEIKYU-CREYMD   PIC X(08).
               05  WRK-SEIKYU-CREHMS   PIC X(08).
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //ASS-PARA//.
      *    カウンタ
       01  CNT-AREA.
           03  CNT-PARA                PIC 9(05).
           03  MOJI-CNT                PIC 9(03).
           03  SPILIT-CNT              PIC 9(03).
           03  ERR-CNT                 PIC 9(04).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(03).
           03  IDY                     PIC 9(02).
           03  IDZ                     PIC 9(03).
           03  IDYY                    PIC 9(05).
           03  IDXX                    PIC 9(05).
           03  IDXY                    PIC 9(05).
      *
      *    カウンタ
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PARA                PIC 9(01).
           03  FLG-ONRECE-K            PIC 9(01).
           03  FLG-HENREI-K            PIC 9(01).
           03  FLG-ONRECE-B            PIC 9(01).
           03  FLG-HENREI-B            PIC 9(01).
      * 
           03  FLG-TOUROKU             PIC 9(01).
           03  FLG-ROW                 PIC 9(01).
      *
           03  FLG-CHK                 PIC 9(01).
           03  FLG-DATAEND             PIC 9(01).
           03  FLG-AREA1.
               05  FLG-COMMA           PIC 9(01).
               05  FLG-DELI1           PIC 9(01).
               05  FLG-DELI2           PIC 9(01).
               05  FLG-NODATA          PIC 9(01).
           03  FLG-HG-CHK              PIC 9(01).
           03  FLG-DB-INSERT           PIC 9(01).
      *
       01  WRK-PARA-REC.
           03  PARA-DATA.
               05  CSV-X           PIC X(01)  OCCURS   3000.
      *
       01  TABLE-SAVE-AREA.
           03  TBL-SAVE-MAX            PIC 9(02).
           03  TBL-SAVE-NAME-OCC       OCCURS  50.
               05  TBL-SAVE-NAME       PIC X(200).
               05  TBL-SAVE-HOSPCD     PIC X(07).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMDG3        PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
      *
           COPY    "CPSHELLTBL.INC".
      *                             
       01  WRK-SHELL-NAME         PIC X(50).
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-AREA.
           03  TBL-SYS1001-OCC             OCCURS  10.
               05  TBL-SYS1001-REC     PIC X(610).
           03  TBL-SYS1001-MAX         PIC 9(02).
      *
       01  WRK-CONS-AREA.
      *
      *    システム管理ＤＢ（３００３）のキー値  
           03  WRK-CONS-3003-KBNCD PIC X(08)   VALUE   "044".
      *    印刷管理ＤＢのキー値  
           03  WRK-CONS-PRTKANRI-TBL-KEY
                                   PIC X(08)   VALUE   "SOKATU".
           03  WRK-CONS-PRTKANRI-TBL-GROUP
                                   PIC X(14)   VALUE   SPACE.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "SOKATU".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   ZERO.
      *     
      *    ジョブ管理ＤＢのキー値（レセプト）  
           03  WRK-CONS-JOB-SHELLID-RECEPT1
                                   PIC X(08)   VALUE   "RECEPT1".
           03  WRK-CONS-JOB-JOBID-RECEPT1
                                   PIC 9(07)   VALUE   1.
      *     
      *    ジョブ管理ＤＢのキー値（データ収集）  
           03  WRK-CONS-JOB-SHELLID-COLLECT
                                  PIC X(08)    VALUE   "COLLECT".
           03  WRK-CONS-JOB-JOBID-COLLECT
                                   PIC 9(07)   VALUE   1.
      *
           03  WRK-CONS-DATALST-MAX
                                   PIC 9(02)   VALUE   15.
      *
      *    エラーファイルＩＤ
           03  WRK-CONS-FILE-ID    PIC X(08)   VALUE   "OR044ERR".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK3003.INC".
      *
       01  ONRECE-K-REC.
           COPY    "CPONRECE-KANRI.INC".
      *
       01  ONRECE-B-REC.
           COPY    "CPONRECE-BODY.INC".
      *
       01  HENREI-K-REC.
           COPY    "CPHENREI-KANRI.INC".
      *
       01  HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSZENHANCNV.INC".
      *
      *    コード変換サブ
           COPY    "CPORCSJISTOEUC.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "ORCA-BLOB".
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
           COPY    "ORCA44SCRAREA.INC".
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
           INITIALIZE                  WRK-AREA
                                       STS-AREA
                                       IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-T01FREE
           MOVE    SPA-WORK        TO  SPA-T01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-T01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-T01FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "TERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "T05"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "TID1"
                   PERFORM 330-TID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "T99"
                   PERFORM 320-T99-SET-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＴＩＤ１）戻り処理
      *****************************************************************
       330-TID1-SET-SEC            SECTION.
      *
           IF      SPA-TID1-FLG    =   "OK"
               EVALUATE    SPA-TIDCD
                   WHEN    "6666"
                       MOVE    "JOIN"          TO  MCP-PUTTYPE
                       PERFORM 210-BACK-SYORI-SEC
                       GO  TO  330-TID1-SET-EXT
                   WHEN    "1001"
      *                ＤＢ登録
                       MOVE    SPACE           TO  SPA-TIDCD
                                                   SPA-TID1-FLG
                       PERFORM 4501-JOBKANRI-CHK-SEC
                       IF      SPA-ERRCD       =   SPACE
                           PERFORM 210-TOUROKU-SEC
                           GO  TO  330-TID1-SET-EXT
                       END-IF
                  WHEN    "1002"
                       MOVE    SPACE           TO  SPA-TIDCD
                                                   SPA-TID1-FLG
                       MOVE    "JOIN"          TO  MCP-PUTTYPE
                       PERFORM 210-BACK-SYORI-SEC
                       GO  TO  330-TID1-SET-EXT
                   WHEN    "0101"
                       PERFORM 220-DEL-SEC
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-TIDCD
                                       SPA-TID1-FLG
      *
           MOVE    1               TO  SPA-GMN-T05-CUR
      *
           INITIALIZE                  SPA-T01KYOUTU
           .
       330-TID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理結果画面（Ｔ９９）戻り処理
      *****************************************************************
       320-T99-SET-SEC            SECTION.
      *
      *    登録済フラグ設定
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-T05-DATALST-MAX
               MOVE    "1"             TO  SPA-NAI-T05-TDB (IDX)
           END-PERFORM
           .
       320-T99-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-T05-AREA
      *
           MOVE    1               TO  SPA-GMN-T05-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           MOVE    T05-ENTRY1          TO  SPA-GMN-T05-ENTRY1
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    "JOIN"          TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *        取り込みファイル削除処理
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM  220-DEL-GID-SEC
      *        UPLOAD(サーバへのデータ保存)処理
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 700-EXPORT-SEC
      *        ＤＢ登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 210-TOUROKU-GID-SEC
               END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    T05-ENTRY1          TO  SPA-GMN-T05-ENTRY1
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        番号選択入力
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 201-SELNUM-CHK-SEC
      *        行選択
               WHEN    ANY             ALSO    "DATALST"
                   PERFORM 203-DATALST-SELECT-SEC
      *        一括分
               WHEN    "ACTIVATE"       ALSO   ANY
                   PERFORM 400-ENTRY-SYORI-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括分入力処理
      *****************************************************************
       400-ENTRY-SYORI-SEC             SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-T05-CUR
      *
      *    入力チェック処理
           MOVE    ZERO                TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           .
       400-ENTRY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
           IF      FLG-TOUROKU         =   1
      *        更新前
               PERFORM 4101-KAKUTEI-CHK-SEC
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定ボタン押下時チェック処理
      *****************************************************************
       4101-KAKUTEI-CHK-SEC        SECTION.
      *
           .
       4101-KAKUTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       201-SELNUM-CHK-SEC              SECTION.
      *
           MOVE    1                   TO  SPA-GMN-T05-CUR
      *
           INITIALIZE                      SPA-GMN-T05-KOUMOKU
                                           SPA-NAI-T05-KOUMOKU
      *
           MOVE    T05-SELNUM         TO  SPA-GMN-T05-SELNUM
      *
           IF      SPA-GMN-T05-SELNUM  =   ZERO
               MOVE    99                  TO  SPA-GMN-T05-CUR
           ELSE
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-T05-DATALST-MAX
                       OR      FLG-CHK     =   1
                   IF      SPA-GMN-T05-SELNUM =
                                       SPA-GMN-T05-TRENNUM (IDX)
                       MOVE    IDX             TO  SPA-NAI-T05-SELNUM
                       MOVE    SPA-GMN-T05-SELNUM
                                               TO  T05-DATALST-ROW
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      FLG-CHK             =   ZERO
                   MOVE    "0001"          TO  SPA-ERRCD
               ELSE
                   PERFORM 2011-DATALST-SELECT-SEC
               END-IF
           END-IF
           .
       201-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象データ選択処理
      *****************************************************************
       2011-DATALST-SELECT-SEC          SECTION.
      *
           MOVE    SPA-GMN-T05-TNAIYO (SPA-NAI-T05-SELNUM)
                                       TO  SPA-GMN-T05-SELNAME1
           MOVE    SPA-NAI-T05-ERR-MSG (SPA-NAI-T05-SELNUM)
                                       TO  SPA-GMN-T05-ERRDETAIL
           EVALUATE    SPA-GMN-T05-TFILENM(SPA-NAI-T05-SELNUM)(1:6)
               WHEN    "RIhasp"
               WHEN    "rihasp"
               WHEN    "RIuasp"
               WHEN    "riuasp"
                   IF      SPA-GMN-T05-TUKAISU (SPA-NAI-T05-SELNUM)
                                           >   ZERO
                       MOVE    SPA-GMN-T05-TUKAISU (SPA-NAI-T05-SELNUM)   
                                           TO  WRK-NUM3
                       MOVE    WRK-NUM3    TO  SPA-GMN-T05-SELNAME2(1:3)
                       MOVE    "回"        TO  SPA-GMN-T05-SELNAME2(4:)
                   END-IF
               WHEN    "RIfixf"
               WHEN    "rifixf"
                   IF    SPA-GMN-T05-TSKAISU (SPA-NAI-T05-SELNUM)
                                           >   ZERO
                       MOVE    SPA-GMN-T05-TSKAISU (SPA-NAI-T05-SELNUM)
                                           TO  WRK-NUM3
                       MOVE    WRK-NUM3    TO  SPA-GMN-T05-SELNAME2(1:3)
                       MOVE    "回"        TO  SPA-GMN-T05-SELNAME2(4:)
                 END-IF
           END-EVALUATE
      *
           .
       2011-DATALST-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象データ選択処理
      *****************************************************************
       203-DATALST-SELECT-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-T05-DATALST-MAX
               IF      T05-DATALST-SELECT (IDX)    =   "T"
                   IF      IDX             =   SPA-NAI-T05-SELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX                 TO  WRK-SELNUM
                       MOVE    SPA-GMN-T05-DATALST-MAX
                                                   TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               INITIALIZE                      SPA-GMN-T05-KOUMOKU
                                               SPA-NAI-T05-KOUMOKU
      *
               MOVE    WRK-SELNUM          TO  SPA-NAI-T05-SELNUM
               MOVE    SPA-GMN-T05-TRENNUM (SPA-NAI-T05-SELNUM)
                                           TO  SPA-GMN-T05-SELNUM
      *
               PERFORM 2011-DATALST-SELECT-SEC
           END-IF
           .
       203-DATALST-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     UPLOAD(サーバへのデータ保存)処理
      *****************************************************************
       700-EXPORT-SEC          SECTION.
      *
           INITIALIZE                      SPA-GMN-T05-KOUMOKU
                                           SPA-NAI-T05-KOUMOKU
           MOVE    T05-ENTRY1          TO  SPA-GMN-T05-ENTRY1
           MOVE    ZERO                TO  WRK-ERRMSG1
                                           WRK-ERRMSG2
                                           ERR-CNT
      *
      *    ファイル名取得
           MOVE    SPACE               TO  WRK-FILENM
           IF      T05-ENTRY1          =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDX FROM    200 BY  -1
                       UNTIL   IDX <       1
                   IF      T05-ENTRY1 (IDX:1)  =   "/" OR  "\"
                       COMPUTE IDY     =   IDX      +   1
                       MOVE  T05-ENTRY1 (IDY:)     TO  WRK-FILENM
                       MOVE  1                     TO  IDX
                  END-IF
               END-PERFORM
               IF      WRK-FILENM             =   SPACE
                   MOVE    T05-ENTRY1          TO  WRK-FILENM
               END-IF
           END-IF
      *
      *    ファイル名チェック
           PERFORM 7001-FILENM-CHECK-SEC
           IF      WRK-ERRMSG1         >   ZERO
               GO  TO  700-EXPORT-EXT
           END-IF
      *
           COMPUTE IDY     =   SPA-GMN-T05-DATALST-MAX +   1
           IF      IDY                 >   WRK-CONS-DATALST-MAX
               MOVE    99                  TO  WRK-ERRMSG1
               GO  TO  700-EXPORT-EXT
           END-IF
      *
      *    保存用ファイル名
           COMPUTE IDY     =   SPA-GMN-T05-DATALST-MAX +   1
           IF      IDY                 >   WRK-CONS-DATALST-MAX
               MOVE    99                  TO  WRK-ERRMSG1
           END-IF
      * 
           IF      WRK-FILENM(1:6)     =   "RECEIP"    OR
                                           "receip"    OR
                                           "RRECEC"    OR
                                           "rrecec"
               EVALUATE    WRK-FILENM
                   WHEN    "receiptc.hen"
                   WHEN    "RECEIPTC.HEN"
                       MOVE    "RECEIPTC.HEN"      TO  WRK-FILENM 
                   WHEN    "receiptc.sah"
                   WHEN    "RECEIPTC.SAH"
                       MOVE    "RECEIPTC.SAH"      TO  WRK-FILENM
                   WHEN    "rrecec.hen"
                   WHEN    "RRECEC.HEN"
                       MOVE    "RRECEC.HEN"        TO  WRK-FILENM 
               END-EVALUATE
               MOVE    IDY                 TO  WRK-INDEX-9
               STRING  SPA-HOSPNUM         DELIMITED   BY  SIZE
                       WRK-INDEX           DELIMITED   BY  SIZE
                       WRK-FILENM          DELIMITED   BY  SPACE
                                   INTO    SPA-NAI-T05-SAVE-BASENAME
                                                                 (IDY)
               END-STRING
           ELSE
               STRING  "RI"                DELIMITED   BY  SIZE
                       WRK-FILENM (3:)     DELIMITED   BY  SPACE
                                   INTO    SPA-NAI-T05-SAVE-BASENAME
                                                                 (IDY)
               END-STRING
           END-IF
      *
           STRING  MCP-TEMPDIR         DELIMITED   BY  SPACE
                   "/"                 DELIMITED   BY  SIZE
                   SPA-NAI-T05-SAVE-BASENAME (IDY)
                                       DELIMITED   BY  SPACE
                               INTO    SPA-NAI-T05-SAVE-NAME (IDY)
           END-STRING
      *??
           DISPLAY "NAME=" IDY "#" SPA-NAI-T05-SAVE-NAME (IDY)
      *??
      *  
      *    BLOBEXPORT  ---> サーバへの保存用FUNCTION
           INITIALIZE                      ORCA-BLOB
           MOVE    T05-OBJECTDATA      TO  ORCA-BLOB-OBJECT
           MOVE    SPA-NAI-T05-SAVE-NAME(IDY)
                                       TO  ORCA-BLOB-FILE
           MOVE    "BLOBEXPORT"        TO  MCP-FUNC
           MOVE    "blob"              TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       ORCA-BLOB
                                       SPA-AREA
      *
      *    取り込みファイルチェック処理
           PERFORM 7002-FILE-CHECK-SEC
           IF      WRK-ERRMSG1         >   ZERO
               INITIALIZE              WRK-ERRMSG-HENSYU-AREA
               EVALUATE    SPA-NAI-T05-TFLG3 (IDY)
                   WHEN    "1"
                       MOVE    "社保"          TO
                                               WRK-ERRMSG-TEISYUTUSAKI
                   WHEN    "2"
                       MOVE    "国保"          TO
                                               WRK-ERRMSG-TEISYUTUSAKI
                   WHEN    "3"
                       MOVE    "労災"          TO
                                               WRK-ERRMSG-TEISYUTUSAKI
               END-EVALUATE
               MOVE    SPA-GMN-T05-TSRYYM(IDY) TO  WRK-ERRMSG-SRYYM
               EVALUATE    SPA-NAI-T05-TFLG2 (IDY)
                   WHEN    "0"
                       CONTINUE
                   WHEN    "1"
                       MOVE    "（保険者からの返戻）"
                                               TO  WRK-ERRMSG-HKNJA
               END-EVALUATE
               MOVE    SPA-GMN-T05-THOSPCD (IDY)
                                       TO  WRK-ERRMSG-HOSPCD
      *
               INITIALIZE                  SPA-GMN-T05-DATALST (IDY)
                                           SPA-NAI-T05-DATALST (IDY)
               EVALUATE    WRK-ERRMSG1
                   WHEN    1
                       MOVE    "ファイルが空です"
                                               TO  SPA-GMN-T05-ERRMSG1
                   WHEN    2
                       MOVE    "ファイル名を確認してください"
                                               TO  SPA-GMN-T05-ERRMSG1
                       MOVE    "都道府県番号、医療機関コードエラー"
                                               TO  SPA-GMN-T05-ERRMSG2
                   WHEN    3
                       MOVE    "既に取り込み済のファイルです"
                                               TO  SPA-GMN-T05-ERRMSG1
                   WHEN    4
                       IF      WRK-ERRMSG-HOSPCD   =   SPACE
                           STRING
                               "既にＤＢ登録済のファイルです　提出先："
                                               DELIMITED  BY  SIZE
                                WRK-ERRMSG-TEISYUTUSAKI
                                               DELIMITED  BY  SIZE
                                "　処理年月：" DELIMITED  BY  SIZE
                                WRK-ERRMSG-SRYYM
                                               DELIMITED  BY  SIZE
                                WRK-ERRMSG-HKNJA
                                               DELIMITED  BY  SPACE
                                               INTO  SPA-GMN-T05-ERRMSG1
                           END-STRING
                       ELSE
                           STRING
                               "既にＤＢ登録済のファイルです　（"
                                               DELIMITED  BY  SIZE
                                WRK-ERRMSG-HOSPCD
                                               DELIMITED  BY  SIZE
                               "）　提出先："  DELIMITED  BY  SIZE
                                WRK-ERRMSG-TEISYUTUSAKI
                                               DELIMITED  BY  SIZE
                                "　処理年月：" DELIMITED  BY  SIZE
                                WRK-ERRMSG-SRYYM
                                               DELIMITED  BY  SIZE
                                WRK-ERRMSG-HKNJA
                                               DELIMITED  BY  SPACE
                                               INTO  SPA-GMN-T05-ERRMSG1
                           END-STRING
                       END-IF
                   WHEN    5
                       MOVE    "取込みデータに不正な文字が存在します"
                                               TO  SPA-GMN-T05-ERRMSG1
                   WHEN    6
                       MOVE    "医療機関コードの取得ができませんでした"
                                               TO  SPA-GMN-T05-ERRMSG1
                   WHEN    9
                       MOVE    "ファイルがオープンできません"
                                               TO  SPA-GMN-T05-ERRMSG1
                   WHEN    18
                       MOVE    "存在しないレコードがあります"
                                               TO  SPA-GMN-T05-ERRMSG1
                       MOVE    "返戻医療機関レコードエラー"
                                               TO  SPA-GMN-T05-ERRMSG2
                   WHEN    98
                       MOVE    "エラー取り込みは１０００件までです"
                                               TO  SPA-GMN-T05-ERRMSG1
                   WHEN    99
                       MOVE    "これ以上取り込めません"
                                               TO  SPA-GMN-T05-ERRMSG1
                   WHEN    OTHER
                       MOVE    "変換不能のデータがあります"
                                               TO  SPA-GMN-T05-ERRMSG1
                       EVALUATE    WRK-ERRMSG1
                           WHEN    10
                               MOVE    "送信回エラー"
                                               TO  SPA-GMN-T05-ERRMSG2
                           WHEN    11
                               MOVE    "請求確定回エラー"
                                               TO  SPA-GMN-T05-ERRMSG2
                           WHEN    12
                               MOVE    "診療年月エラー"
                                               TO  SPA-GMN-T05-ERRMSG2
                           WHEN    13
                               MOVE    "送信年月日エラー"
                                               TO  SPA-GMN-T05-ERRMSG2
                           WHEN    14
                               MOVE    "請求年月エラー"
                                               TO  SPA-GMN-T05-ERRMSG2
                           WHEN    15
                               MOVE    "請求確定年月日エラー"
                                               TO  SPA-GMN-T05-ERRMSG2
                           WHEN    16
                               MOVE    "審査支払機関コードエラー"
                                               TO  SPA-GMN-T05-ERRMSG2
                           WHEN    17
                               MOVE    "送信回エラー"
                                               TO  SPA-GMN-T05-ERRMSG2
                           WHEN    19
                               MOVE
                                   "都道府県番号、医療機関コードエラー"
                                               TO  SPA-GMN-T05-ERRMSG2
                           WHEN    20
                               MOVE    "点数表エラー"
                                               TO  SPA-GMN-T05-ERRMSG2
                           WHEN    21
                               MOVE    "マルチボリューム識別情報エラー"
                                               TO  SPA-GMN-T05-ERRMSG2
                           WHEN    22
                               MOVE
                                   "都道府県番号、医療機関コードエラー"
                                               TO  SPA-GMN-T05-ERRMSG2
                       END-EVALUATE
               END-EVALUATE
               GO  TO  700-EXPORT-EXT
           END-IF
      *
      *    返戻合計レコードチェック
           IF      WRK-FILENM          =  "RECEIPTC.HEN"   OR
                                          "receiptc.hen"   OR
                                          "RECEIPTC.SAH"   OR
                                          "receiptc.sah"
               IF      FLG-HG-CHK          =   ZERO
                   MOVE    "存在しないレコードがあります"
                                               TO  SPA-GMN-T05-ERRMSG1
                   MOVE    "返戻合計レコードエラー"
                                               TO  SPA-GMN-T05-ERRMSG2
                   GO  TO  700-EXPORT-EXT
               END-IF
           END-IF
      *
      *    送信回数チェック
           IF      WRK-FILENM(1:6)     =  "RIhasp" OR "RIuasp" OR
                                          "rihasp" OR "riuasp"
               PERFORM VARYING IDX     FROM    1   BY  1
                     UNTIL     IDX     >       IDY
                   IF      SPA-GMN-T05-TFILENM(IDX)(1:6)
                                               =  "RIfixf" OR "rifixf"
                       PERFORM 7003-SEND-CHECK-SEC
                   END-IF
               END-PERFORM
           END-IF
           IF      WRK-FILENM(1:6)     =   "RIfixf" OR "rifixf"   
               PERFORM VARYING IDX     FROM    1   BY  1
                     UNTIL     IDX     >       IDY
                   IF      SPA-GMN-T05-TFILENM(IDX)(1:6)
                                               =   "RIhasp" OR "RIuasp"
                                               OR  "rihasp" OR "riuasp"
                       PERFORM 7003-SEND-CHECK-SEC
                   END-IF
               END-PERFORM
           END-IF
      *
      *    診療年月チェック
           IF      IDY                 >   1
               IF      SPA-NAI-T05-TSRYYM (1)
                                       NOT =   SPA-NAI-T05-TSRYYM(IDY)
                   MOVE  "対象年月（診療年月）を確認してください"
                                           TO  SPA-GMN-T05-ERRMSG2
                                               SPA-NAI-T05-ERR-MSG(IDY)
                   MOVE  "○"              TO  SPA-GMN-T05-TFLG4  (IDY)
               END-IF
           END-IF
      *
           MOVE   IDY                  TO  SPA-GMN-T05-TRENNUM(IDY)
           MOVE   WRK-FILENM           TO  SPA-GMN-T05-TFILENM(IDY)
           MOVE   T05-ENTRY1           TO  SPA-NAI-T05-TFILENM(IDY)
      *
           MOVE   IDY                  TO  SPA-GMN-T05-DATALST-MAX
           .
       700-EXPORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル名チェック処理
      *****************************************************************
       7001-FILENM-CHECK-SEC            SECTION.
      *
      *    ファイル名桁数
           MOVE    ZERO                TO  MOJI-CNT
           PERFORM VARYING IDX FROM    50  BY  -1
                   UNTIL   IDX <       1
               IF    WRK-FILENMSS (IDX)    NOT =  SPACE
                   MOVE    IDX             TO  MOJI-CNT
                   MOVE    1               TO  IDX
              END-IF
           END-PERFORM
      *
           EVALUATE    WRK-FILENM(1:6)
               WHEN    "RIhasp"
               WHEN    "rihasp"
               WHEN    "RIuasp"
               WHEN    "riuasp"
      *            受付事務点検ファイル
                   EVALUATE    MOJI-CNT
                       WHEN    26
                       WHEN    35
      *                    審査支払機関コード
                           IF      WRK-FILENM (7:1)    =   "1" OR  "2"
                               CONTINUE
                           ELSE
                               MOVE    1               TO  WRK-ERRMSG1
                               MOVE    2               TO  WRK-ERRMSG2
                           END-IF
      *                    点数表区分
                           IF      WRK-FILENM (10:1)   =   "1"
                               CONTINUE
                           ELSE
                               MOVE    1               TO  WRK-ERRMSG1
                               MOVE    3               TO  WRK-ERRMSG2
                           END-IF
      *                    送信回
                           IF      MOJI-CNT            =   26
                               MOVE    WRK-FILENM (20:3)   TO  SNUM-INX
                               CALL    "ORCSNUM"           USING
                                                           ORCSNUMAREA
                               IF    ( SNUM-RC             =   ZERO  )
                               AND   ( SNUM-MINKBN         =   SPACE )
                               AND   ( SNUM-SYOKBN         =   SPACE )
                               AND   ( SNUM-SEISUU         <=  3     )
                               AND   ( SNUM-OUTNUM         >   ZERO  )
                                   CONTINUE
                               ELSE
                                   MOVE    1           TO  WRK-ERRMSG1
                                   MOVE    4           TO  WRK-ERRMSG2
                               END-IF
                           END-IF
      *                    拡張子
                           IF      MOJI-CNT            =   26
                               IF      WRK-FILENM (23:4)   =   ".csv"
                                   CONTINUE
                               ELSE
                                   MOVE    1           TO  WRK-ERRMSG1
                                   MOVE    5           TO  WRK-ERRMSG2
                               END-IF
                           ELSE    
                               IF      WRK-FILENM (32:4)   =   ".csv"
                                   CONTINUE
                               ELSE
                                   MOVE    1           TO  WRK-ERRMSG1
                                   MOVE    5           TO  WRK-ERRMSG2
                               END-IF
                           END-IF
                       WHEN    OTHER
                           MOVE    1                   TO  WRK-ERRMSG1
                           MOVE    1                   TO  WRK-ERRMSG2
                   END-EVALUATE
      **           MOVE 11                 TO  SPILIT-CNT
                   MOVE 9                  TO  SPILIT-CNT
               WHEN    "RIfixf"
               WHEN    "rifixf"
      *            請求確定状況ファイル
                   EVALUATE    MOJI-CNT
                       WHEN    27
                       WHEN    29
                       WHEN    35
      *                    審査支払機関コード
                           IF      WRK-FILENM (7:1)    =   "1" OR  "2"
                               CONTINUE
                           ELSE
                               MOVE    1               TO  WRK-ERRMSG1
                               MOVE    2               TO  WRK-ERRMSG2
                           END-IF
      *                    点数表区分
                           IF      WRK-FILENM (10:1)   =   "1"
                               CONTINUE
                           ELSE
                               MOVE    1               TO  WRK-ERRMSG1
                               MOVE    3               TO  WRK-ERRMSG2
                           END-IF
      *                    送信回
                           IF      MOJI-CNT            =   27  OR  29
                               MOVE    WRK-FILENM (20:3)   TO  SNUM-INX
                               CALL    "ORCSNUM"           USING
                                                           ORCSNUMAREA
                               IF    ( SNUM-RC             =   ZERO  )
                               AND   ( SNUM-MINKBN         =   SPACE )
                               AND   ( SNUM-SYOKBN         =   SPACE )
                               AND   ( SNUM-SEISUU         <=  3     )
                               AND   ( SNUM-OUTNUM         >   ZERO  )
                                   CONTINUE
                               ELSE
                                   MOVE    1           TO  WRK-ERRMSG1
                                   MOVE    4           TO  WRK-ERRMSG2
                               END-IF
                           END-IF
      *                    請求確定回
                           IF      MOJI-CNT            =   27
                               IF    ( WRK-FILENM (23:1)   >=   "1" )
                               AND   ( WRK-FILENM (23:1)   <=   "9" )
                                   CONTINUE
                               ELSE
                                   MOVE    1           TO  WRK-ERRMSG1
                                   MOVE    6           TO  WRK-ERRMSG2
                               END-IF
                           END-IF
                           IF      MOJI-CNT            =   29
                               MOVE    WRK-FILENM (23:3)   TO  SNUM-INX
                               CALL    "ORCSNUM"           USING
                                                           ORCSNUMAREA
                               IF    ( SNUM-RC             =   ZERO  )
                               AND   ( SNUM-MINKBN         =   SPACE )
                               AND   ( SNUM-SYOKBN         =   SPACE )
                               AND   ( SNUM-SEISUU         <=  3     )
                               AND   ( SNUM-OUTNUM         >   ZERO  )
                                   CONTINUE
                               ELSE
                                   MOVE    1           TO  WRK-ERRMSG1
                                   MOVE    6           TO  WRK-ERRMSG2
                               END-IF
                           END-IF
      *                    拡張子
                           IF      MOJI-CNT            =   27
                               IF      WRK-FILENM (24:4)   =   ".csv"
                                   CONTINUE
                               ELSE
                                   MOVE    1           TO  WRK-ERRMSG1
                                   MOVE    5           TO  WRK-ERRMSG2
                               END-IF
                           ELSE    
                               IF      MOJI-CNT            =   29
                                   IF      WRK-FILENM (26:4)   =  ".csv"
                                       CONTINUE
                                   ELSE
                                       MOVE    1       TO  WRK-ERRMSG1
                                       MOVE    5       TO  WRK-ERRMSG2
                                   END-IF
                               ELSE    
                                   IF      WRK-FILENM (32:4)   =  ".csv"
                                       CONTINUE
                                   ELSE
                                       MOVE    1       TO  WRK-ERRMSG1
                                       MOVE    5       TO  WRK-ERRMSG2
                                   END-IF
                               END-IF
                           END-IF
                       WHEN    OTHER
                           MOVE    1                   TO  WRK-ERRMSG1
                           MOVE    1                   TO  WRK-ERRMSG2
                   END-EVALUATE
      *
                   MOVE    10              TO  SPILIT-CNT
               WHEN    "RECEIP"
               WHEN    "receip"
      *            返戻データ
                   EVALUATE    MOJI-CNT
                       WHEN    12
      *                    ファイル名
                           IF      WRK-FILENM      =   "RECEIPTC.HEN" 
                                                   OR  "receiptc.hen"
                                                   OR  "RECEIPTC.SAH" 
                                                   OR  "receiptc.sah"
                               CONTINUE
                           ELSE
                               MOVE    1               TO  WRK-ERRMSG1
                           END-IF
                       WHEN    OTHER
                           MOVE    1                   TO  WRK-ERRMSG1
                           MOVE    1                   TO  WRK-ERRMSG2
                   END-EVALUATE
      *
                   MOVE    8               TO  SPILIT-CNT
               WHEN    "RRECEC"
               WHEN    "rrecec"
      *            返戻データ（労災）
                   EVALUATE    MOJI-CNT
                       WHEN    10
      *                    ファイル名
                           IF      WRK-FILENM      =   "RRECEC.HEN" 
                                                   OR  "rrecec.hen"
                               CONTINUE
                           ELSE
                               MOVE    1               TO  WRK-ERRMSG1
                           END-IF
                       WHEN    OTHER
                           MOVE    1                   TO  WRK-ERRMSG1
                           MOVE    1                   TO  WRK-ERRMSG2
                   END-EVALUATE
      *
                   MOVE    21              TO  SPILIT-CNT
               WHEN    OTHER
                   MOVE    1               TO  WRK-ERRMSG1
           END-EVALUATE
      *
           IF      WRK-ERRMSG1         =   1
               MOVE    "ファイル名を確認してください"
                                           TO  SPA-GMN-T05-ERRMSG1
           END-IF
           EVALUATE    WRK-ERRMSG2
               WHEN    1
                   MOVE    "ファイル名の長さが異なります"
                                           TO  SPA-GMN-T05-ERRMSG2
               WHEN    2
                   MOVE    "審査支払機関コードエラー"
                                           TO  SPA-GMN-T05-ERRMSG2
               WHEN    3
                   MOVE    "点数表区分エラー"
                                           TO  SPA-GMN-T05-ERRMSG2
               WHEN    4
                   MOVE    "送信回エラー"
                                           TO  SPA-GMN-T05-ERRMSG2
               WHEN    5
                   MOVE    "拡張子エラー"
                                           TO  SPA-GMN-T05-ERRMSG2
               WHEN    6
                   MOVE    "請求確定回エラー"
                                           TO  SPA-GMN-T05-ERRMSG2
               WHEN    OTHER
                   MOVE    SPACE           TO  SPA-GMN-T05-ERRMSG2
           END-EVALUATE
           .
       7001-FILENM-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    取り込みファイルチェック処理
      *****************************************************************
       7002-FILE-CHECK-SEC          SECTION.
      *
           MOVE    ZERO                TO  CNT-PARA
                                           FLG-HG-CHK
      *
           MOVE    SPA-NAI-T05-SAVE-NAME (IDY)
                                       TO  ASS-PARA
           OPEN    INPUT                   PARA-FILE
      *
           IF      STS-PARA        NOT =   ZERO
               MOVE    9                   TO  WRK-ERRMSG1
               CALL "coblog" USING   "file open err sts=" STS-PARA
               GO  TO  7002-FILE-CHECK-EXT
           END-IF
      *
           PERFORM 900-PARA-READ-SEC
      *
           PERFORM             UNTIL   FLG-PARA    =   1
                               OR      WRK-ERRMSG1 >   ZERO
      *
               MOVE    PARA-REC        TO   SJISTOEUC-INDATA
               PERFORM 7002-FILE-CONVERT-SEC
               IF      SJISTOEUC-RETURN    =   0
                 MOVE    SJISTOEUC-OUTDATA
                                       TO  PARA-DATA
                 IF       PARA-DATA(1:1) =   X"0D"
                   MOVE    1               TO  WRK-ERRMSG1
                 ELSE
                   IF      CNT-PARA            =   1
                       EVALUATE    WRK-FILENM(1:6)
                           WHEN    "RIhasp"
                           WHEN    "rihasp"
                           WHEN    "RIuasp"
                           WHEN    "riuasp"
                               MOVE    SJISTOEUC-OUTDATA
                                                       TO  WRK-TITLE
                               MOVE    2               TO  WK-HEAD-LINE
                               MOVE    4               TO  WK-BODY-LINE
                           WHEN    "RIfixf"
                           WHEN    "rifixf"
                               MOVE    "オンライン請求確定状況ファイル"
                                                       TO  WRK-TITLE
                               MOVE    1               TO  WK-HEAD-LINE
                               MOVE    2               TO  WK-BODY-LINE
                           WHEN    "RECEIP"
                           WHEN    "receip"
                               EVALUATE    WRK-FILENM
                                   WHEN    "receiptc.hen"
                                   WHEN    "RECEIPTC.HEN"
                                       MOVE    "オンライン返戻ファイル"
                                                           TO  WRK-TITLE
                                   WHEN    "receiptc.sah"
                                   WHEN    "RECEIPTC.SAH"
                                       MOVE
                                      "オンライン返戻ファイル（保険者）"
                                                           TO  WRK-TITLE
                               END-EVALUATE
                               MOVE    1               TO  WK-HEAD-LINE
                               MOVE    2               TO  WK-BODY-LINE
                           WHEN    "RRECEC"
                           WHEN    "rrecec"
                                       MOVE
                                       "オンライン返戻ファイル（労災）"
                                                       TO  WRK-TITLE
                               MOVE    1               TO  WK-HEAD-LINE
                               MOVE    2               TO  WK-BODY-LINE
                       END-EVALUATE
                   END-IF
                   IF      CNT-PARA            =   WK-HEAD-LINE
      *                ヘッダ項目取得チェック処理
                       PERFORM 70021-HEAD-CHECK-SEC
                   END-IF
                   IF      CNT-PARA            >=  WK-BODY-LINE
      *                明細項目取得チェック処理
                       PERFORM 70022-BODY-CHECK-SEC
                   END-IF
      *
                   PERFORM 900-PARA-READ-SEC
                 END-IF
               ELSE
                 MOVE    5               TO  WRK-ERRMSG1
               END-IF
           END-PERFORM
      *
           CLOSE   PARA-FILE
      *
           IF      CNT-PARA            =   ZERO
               MOVE    1                   TO  WRK-ERRMSG1
           END-IF
           IF      WRK-ERRMSG1         >   ZERO
               GO  TO  7002-FILE-CHECK-EXT
           END-IF
      *
      *    ファイル名（都道府県コード・医療機関コード）チェック
           INITIALIZE                      TBL-SYS1001-AREA
      *
           INITIALIZE                      SYS-1001-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
      **** MOVE    SPA-NAI-T05-TSRYYM (1)
      ****                             TO  SYS-1001-STYUKYMD (1:6)
           EVALUATE    WRK-FILENM(1:6)
               WHEN    "RRECEC"
               WHEN    "rrecec"
                   MOVE    SPA-NAI-T05-TSRYYM (IDY)
                                           TO  SYS-1001-STYUKYMD (1:6)
                                               SYS-1001-EDYUKYMD (1:6)
               WHEN    OTHER
                   MOVE    SPA-NAI-T05-TSRYYM-B (IDY)
                                           TO  SYS-1001-STYUKYMD (1:6)
                                               SYS-1001-EDYUKYMD (1:6)
           END-EVALUATE
           MOVE    "31"                TO  SYS-1001-STYUKYMD (7:2)
           MOVE    "01"                TO  SYS-1001-EDYUKYMD (7:2)
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               PERFORM  UNTIL    ( FLG-SYSKANRI    =    1  )
                        OR       ( TBL-SYS1001-MAX >=   10 )
      *
                    MOVE    MCPDATA-REC    TO  SYS-1001-REC
                    ADD     1              TO  TBL-SYS1001-MAX
                    MOVE    SYS-1001-REC   TO  TBL-SYS1001-REC 
                                                  (TBL-SYS1001-MAX)
      *
                    MOVE    "tbl_syskanri" TO  MCP-TABLE
                    MOVE    "key10"        TO  MCP-PATHNAME
                    PERFORM 900-SYSKANRI-READ-SEC
                END-PERFORM
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      WRK-FILENM      =   "RECEIPTC.HEN"  OR
                                       "receiptc.hen"  OR
                                       "RECEIPTC.SAH"  OR
                                       "receiptc.sah"
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       TBL-SYS1001-MAX
                   MOVE    TBL-SYS1001-REC (IDX)   TO  SYS-1001-REC
                   IF    ( WRK-PREFNUM         =   SYS-1001-PREFNUM )
                   AND   ( WRK-HOSPCD          =   SYS-1001-HOSPCD  )
                       MOVE    ZERO                    TO  WRK-ERRMSG1
                       MOVE    TBL-SYS1001-MAX         TO  IDX
                   ELSE
                       MOVE    2                   TO  WRK-ERRMSG1
                   END-IF
               END-PERFORM    
               IF      WRK-ERRMSG1         >   ZERO
                   GO  TO  7002-FILE-CHECK-EXT
               END-IF
           ELSE
               IF      WRK-FILENM      =   "RRECEC.HEN"  OR
                                           "rrecec.hen"
                   CONTINUE
               ELSE
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       TBL-SYS1001-MAX
                       MOVE    TBL-SYS1001-REC (IDX)   TO  SYS-1001-REC
                       IF    ( WRK-FILENM (8:2)    =  SYS-1001-PREFNUM )
                       AND   ( WRK-FILENM (11:7)   =  SYS-1001-HOSPCD  )
                           MOVE    ZERO                TO  WRK-ERRMSG1
                           MOVE    TBL-SYS1001-MAX     TO  IDX
                       ELSE
                           MOVE    2                   TO  WRK-ERRMSG1
                       END-IF
                   END-PERFORM    
                   IF      WRK-ERRMSG1         >   ZERO
                       GO  TO  7002-FILE-CHECK-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    ファイル名
           MOVE    WRK-TITLE           TO  SPA-GMN-T05-TNAIYO(IDY)
      *    返戻元
           EVALUATE    WRK-FILENM
               WHEN    "RECEIPTC.HEN"
               WHEN    "receiptc.hen"
               WHEN    "RRECEC.HEN"
               WHEN    "rrecec.hen"
                   MOVE    "0"             TO  SPA-NAI-T05-TFLG2 (IDY)
               WHEN    "RECEIPTC.SAH"
               WHEN    "receiptc.sah" 
                   MOVE    "1"             TO  SPA-NAI-T05-TFLG2 (IDY)
               WHEN    OTHER
                   MOVE    SPACE           TO  SPA-NAI-T05-TFLG2 (IDY)
           END-EVALUATE
      *
      *    審査支払機関名称
           IF      WRK-FILENM      =   "RECEIPTC.HEN"  OR
                                       "receiptc.hen"  OR
                                       "RECEIPTC.SAH"  OR
                                       "receiptc.sah" 
               MOVE    WRK-TEISYUTUSAKI    TO  SPA-NAI-T05-TFLG3 (IDY)
           ELSE
               IF      WRK-FILENM      =   "RRECEC.HEN"  OR
                                           "rrecec.hen"
                   MOVE    "3"             TO  SPA-NAI-T05-TFLG3 (IDY)
               ELSE
                   MOVE    WRK-FILENM (7:1)
                                           TO  SPA-NAI-T05-TFLG3 (IDY)
               END-IF
           END-IF
      *    医療機関コード（診療年月に該当する医療機関コードを取得）
           EVALUATE    WRK-FILENM(1:6)
               WHEN    "RIhasp"
               WHEN    "rihasp"
               WHEN    "RIuasp"
               WHEN    "riuasp"
               WHEN    "RIfixf"
               WHEN    "rifixf"
      *            ファイル名の医療機関コードを取得
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       100
                           OR      SPA-NAI-1001-TSTYUKYMD  (IDX)
                                       =       SPACE
                       IF    ( SPA-NAI-T05-TSRYYM (IDY)
                                       >=  SPA-NAI-1001-TSTYUKYMD (IDX)
                                                            (1:6) )
                       AND   ( SPA-NAI-T05-TSRYYM (IDY)
                                       <=  SPA-NAI-1001-TEDYUKYMD (IDX)
                                                            (1:6) )
                           IF      SPA-NAI-1001-THOSPCD (IDX)
                                           NOT =   WRK-FILENM (11:7)
                                MOVE    "旧"       TO
                                               SPA-GMN-T05-THOSPCD (IDY)
                           END-IF
                           MOVE    WRK-FILENM (11:7)
                                                   TO
                                               SPA-NAI-T05-THOSPCD (IDY)
                           MOVE    100            TO  IDX
                       END-IF
                   END-PERFORM
                   IF      SPA-NAI-T05-THOSPCD (IDY)
                                              =   SPACE
                       MOVE    6                  TO  WRK-ERRMSG1
                   END-IF
               WHEN    "RECEIP"
               WHEN    "receip"
      *            診療年月に該当する医療機関コードを取得
                   MOVE    WRK-HOSPCD      TO  SPA-NAI-T05-THOSPCD (IDY)
      *
      *            請求年月の前月の医療機関コードを取得とチェック
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"            TO  LNK-DAY6-IRAI
                   MOVE    SPA-NAI-T05-TSRYYM (IDY)
                                           TO  LNK-DAY6-KIJUN (1:6)
                   MOVE    "01"            TO  LNK-DAY6-KIJUN (7:2)
                   MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
                   MOVE    -1              TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                  MOVE    LNK-DAY6-KEISAN(1:6)
                                           TO  WRK-SRYYM
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       100
                           OR      SPA-NAI-1001-TSTYUKYMD  (IDX)
                                       =       SPACE
                       IF    ( WRK-SRYYM
                                       >=  SPA-NAI-1001-TSTYUKYMD (IDX)
                                                            (1:6) )
                       AND   ( WRK-SRYYM
                                       <=  SPA-NAI-1001-TEDYUKYMD (IDX)
                                                            (1:6) )
                           IF      SPA-NAI-1001-THOSPCD (IDX)
                                           NOT =   WRK-HOSPCD
                                MOVE    "旧"       TO
                                               SPA-GMN-T05-THOSPCD (IDY)
                           END-IF
                           MOVE    100            TO  IDX
                       END-IF
                   END-PERFORM
               WHEN    "RRECEC"
               WHEN    "rrecec"
                   INITIALIZE                  WRK-SRYYM-HOSPCD
                                               WRK-SYORIYM-HOSPCD
      *??
           DISPLAY "SRYYM=" SPA-NAI-T05-TSRYYM (IDY) 
                        "#" SPA-NAI-T05-TSRYYM-B (IDY) "#"
      *??
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       100
                           OR      SPA-NAI-1001-TSTYUKYMD  (IDX)
                                       =       SPACE
                       IF    ( SPA-NAI-T05-TSRYYM (IDY)
                                       >=  SPA-NAI-1001-TSTYUKYMD (IDX)
                                                            (1:6) )
                       AND   ( SPA-NAI-T05-TSRYYM (IDY)
                                       <=  SPA-NAI-1001-TEDYUKYMD (IDX)
                                                            (1:6) )
                           MOVE    SPA-NAI-1001-THOSPCD (IDX)
                                               TO  WRK-SYORIYM-HOSPCD
                       END-IF
                       IF    ( SPA-NAI-T05-TSRYYM-B (IDY)
                                       >=  SPA-NAI-1001-TSTYUKYMD (IDX)
                                                            (1:6) )
                       AND   ( SPA-NAI-T05-TSRYYM-B (IDY)
                                       <=  SPA-NAI-1001-TEDYUKYMD (IDX)
                                                            (1:6) )
                           MOVE    SPA-NAI-1001-THOSPCD (IDX)
                                               TO  WRK-SRYYM-HOSPCD
                       END-IF
                       IF    ( WRK-SRYYM-HOSPCD    >   ZERO )
                       AND   ( WRK-SYORIYM-HOSPCD  >   ZERO )
                           MOVE    100             TO  IDX
                       END-IF
                   END-PERFORM
                   IF    ( WRK-SRYYM-HOSPCD        >   ZERO )
                   AND   ( WRK-SYORIYM-HOSPCD      >   ZERO )
                       IF      WRK-SRYYM-HOSPCD
                                           NOT =   WRK-SYORIYM-HOSPCD
                                MOVE    "旧"       TO
                                               SPA-GMN-T05-THOSPCD (IDY)
                       END-IF
                       MOVE    WRK-SRYYM-HOSPCD    TO
                                               SPA-NAI-T05-THOSPCD (IDY)
                   ELSE
                       MOVE    6                  TO  WRK-ERRMSG1
                   END-IF
      *??
           DISPLAY "HOSPCD=" WRK-SRYYM-HOSPCD "#" WRK-SYORIYM-HOSPCD
                        "#" SPA-NAI-T05-THOSPCD (IDY) "#"
      *??
           END-EVALUATE
      *??
           DISPLAY "SPA-NAI-T05-THOSPCD=" SPA-NAI-T05-THOSPCD (IDY) "#"
      *??
      *
      *    取り込み済のファイルかどうかの確認
           IF      IDY                 >   1
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-T05-DATALST-MAX
                   IF    ( SPA-NAI-T05-TFLG3   (IDX)
                                           =   SPA-NAI-T05-TFLG3 (IDY) )
                   AND   ( SPA-NAI-T05-TFLG2   (IDX)
                                           =   SPA-NAI-T05-TFLG2 (IDY) )
                   AND   ( SPA-NAI-T05-THOSPCD (IDX)
                                           =   SPA-NAI-T05-THOSPCD
                                                                 (IDY) )
                       IF      SPA-GMN-T05-TFILENM (IDX)
                                                   =   WRK-FILENM
                           MOVE    3               TO  WRK-ERRMSG1
                           GO  TO  7002-FILE-CHECK-EXT
                       END-IF
                       IF    ( SPA-GMN-T05-TFILENM (IDX) (1:2)
                                               =   "RI"    OR  "ri" )
                       AND   ( WRK-FILENM (1:2)
                                               =   "RI"    OR  "ri" )
                       AND   ( SPA-GMN-T05-TFILENM (IDX) (3:)
                                               =   WRK-FILENM (3:)  )
                           MOVE    3               TO  WRK-ERRMSG1
                           GO  TO  7002-FILE-CHECK-EXT
                       END-IF
                       IF    ( SPA-GMN-T05-TFILENM (IDX)
                                               =   "RECEIPTC.HEN"
                                               OR  "receiptc.hen" )
                       AND   ( WRK-FILENM      =   "RECEIPTC.HEN"
                                               OR  "receiptc.hen" )
                           MOVE    3               TO  WRK-ERRMSG1
                           GO  TO  7002-FILE-CHECK-EXT
                       END-IF
                       IF    ( SPA-GMN-T05-TFILENM (IDX)
                                               =   "RECEIPTC.SAH"
                                               OR  "receiptc.sah" )
                       AND   ( WRK-FILENM      =   "RECEIPTC.SAH"
                                               OR  "receiptc.sah" )
                           MOVE    3               TO  WRK-ERRMSG1
                           GO  TO  7002-FILE-CHECK-EXT
                       END-IF
                       IF    ( SPA-GMN-T05-TFILENM (IDX)
                                               =   "RRECEC.HEN"
                                               OR  "rrecec.hen" )
                       AND   ( WRK-FILENM      =   "RRECEC.HEN"
                                               OR  "rrecec.hen" )
                           MOVE    3               TO  WRK-ERRMSG1
                           GO  TO  7002-FILE-CHECK-EXT
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    ＤＢ登録済のファイルかどうかの確認
           EVALUATE    WRK-FILENM(1:6)
               WHEN    "RIhasp"
               WHEN    "rihasp"
               WHEN    "RIuasp"
               WHEN    "riuasp"
               WHEN    "RIfixf"
               WHEN    "rifixf"
                   INITIALIZE                  ONRECE-K-REC
                   MOVE   SPA-HOSPNUM      TO  ONRECE-K-HOSPNUM
                   MOVE   SPA-NAI-T05-TFLG3 (IDY)
                                           TO  ONRECE-K-TEISYUTUSAKI
                   MOVE   SPA-NAI-T05-TSRYYM(IDY)
                                           TO  ONRECE-K-SRYYM
                   MOVE   SPA-NAI-T05-THOSPCD (IDY)
                                           TO  ONRECE-K-HOSPCD
                   MOVE    SPA-GMN-T05-TUKAISU(IDY) 
                                           TO  ONRECE-K-UCNT
                   EVALUATE    WRK-FILENM(1:6)
                       WHEN    "RIhasp"
                       WHEN    "rihasp"
                       WHEN    "RIuasp"
                       WHEN    "riuasp"
                           MOVE    "key7"          TO  WRK-MCP-PATHNAME
                       WHEN    "RIfixf"
                       WHEN    "rifixf"
                           MOVE    SPA-GMN-T05-TSKAISU(IDY)
                                                   TO  ONRECE-K-SCNT
                           MOVE    "key8"          TO  WRK-MCP-PATHNAME
                   END-EVALUATE
                   PERFORM 900-ONRECE-KANRI-READ-SEC
                   IF      FLG-ONRECE-K    =   ZERO
                       EVALUATE    WRK-FILENM(1:6)
                           WHEN    "RIhasp"
                           WHEN    "rihasp"
                           WHEN    "RIuasp"
                           WHEN    "riuasp"
                               IF      ONRECE-K-RECV-FLG
                                                       =   1
                                   MOVE    4           TO  WRK-ERRMSG1
                                   GO  TO  7002-FILE-CHECK-EXT
                               END-IF
                           WHEN    "RIfixf"
                           WHEN    "rifixf"
                               MOVE    4               TO  WRK-ERRMSG1
                               GO  TO  7002-FILE-CHECK-EXT
                       END-EVALUATE
                   ELSE
      *                医療機関コードが空白の時オンラインレセ電管理の診療年月で確認
                       INITIALIZE                  ONRECE-K-REC
                       MOVE   SPA-HOSPNUM      TO  ONRECE-K-HOSPNUM
                       MOVE   SPA-NAI-T05-TFLG3 (IDY)
                                               TO  ONRECE-K-TEISYUTUSAKI
                       MOVE   SPA-NAI-T05-TSRYYM(IDY)
                                               TO  ONRECE-K-SRYYM
                       MOVE    SPACE           TO  ONRECE-K-HOSPCD
                       MOVE    SPA-GMN-T05-TUKAISU(IDY) 
                                               TO  ONRECE-K-UCNT
                       EVALUATE    WRK-FILENM(1:6)
                           WHEN    "RIhasp"
                           WHEN    "rihasp"
                           WHEN    "RIuasp"
                           WHEN    "riuasp"
                               MOVE    "key7"      TO  WRK-MCP-PATHNAME
                           WHEN    "RIfixf"
                           WHEN    "rifixf"
                               MOVE    SPA-GMN-T05-TSKAISU(IDY)
                                                   TO  ONRECE-K-SCNT
                               MOVE    "key8"      TO  WRK-MCP-PATHNAME
                       END-EVALUATE
                       PERFORM 900-ONRECE-KANRI-READ-SEC
                       IF      FLG-ONRECE-K    =   ZERO
                           INITIALIZE                  ONRECE-B-REC
                           MOVE    SPA-HOSPNUM     TO  ONRECE-B-HOSPNUM
                           MOVE    ONRECE-K-TEISYUTUSAKI
                                                   TO
                                                   ONRECE-B-TEISYUTUSAKI
                           MOVE    ONRECE-K-SRYYM  TO  ONRECE-B-SRYYM
                           MOVE    SPACE           TO  ONRECE-B-HOSPCD
                           MOVE    ONRECE-K-UCNT   TO  ONRECE-B-UCNT
                           MOVE    ONRECE-K-SCNT   TO  ONRECE-B-SCNT
                           MOVE    ZERO            TO  ONRECE-B-RENNUM
                           PERFORM 900-ONRECE-BODY-READ-SEC
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX >       100
                                   OR      SPA-NAI-1001-TSTYUKYMD (IDX)
                                               =       SPACE
                               IF    ( ONRECE-B-SRYYM-B
                                       >=  SPA-NAI-1001-TSTYUKYMD (IDX)
                                                            (1:6) )
                               AND   ( ONRECE-B-SRYYM-B
                                       <=  SPA-NAI-1001-TEDYUKYMD (IDX)
                                                            (1:6) )
                                   IF      SPA-NAI-1001-THOSPCD (IDX)
                                           =   SPA-NAI-T05-THOSPCD (IDY)
                                       MOVE    4       TO  WRK-ERRMSG1
                                       GO  TO  7002-FILE-CHECK-EXT
                                   END-IF
                                   MOVE    100             TO  IDX
                               END-IF
                           END-PERFORM
                       END-IF
                   END-IF
               WHEN    "RECEIP"
               WHEN    "receip"
               WHEN    "RRECEC"
               WHEN    "rrecec"
                   INITIALIZE                  HENREI-K-REC
                   MOVE   SPA-HOSPNUM      TO  HENREI-K-HOSPNUM
                   MOVE   SPA-NAI-T05-TFLG3 (IDY)
                                           TO  HENREI-K-TEISYUTUSAKI
                   MOVE   SPA-NAI-T05-TFLG2 (IDY)
                                           TO  HENREI-K-TEISYUTUSAKI2
                   MOVE   SPA-NAI-T05-TSRYYM(IDY)
                                           TO  HENREI-K-SYORIYM
                   MOVE   SPA-NAI-T05-THOSPCD (IDY)
                                           TO  HENREI-K-HOSPCD
                   PERFORM 900-HENREI-KANRI-READ-SEC
                   IF      FLG-HENREI-K    =   ZERO
                       MOVE    4               TO  WRK-ERRMSG1
                       GO  TO  7002-FILE-CHECK-EXT
                   END-IF
      *            医療機関コードが空白の時返戻明細の診療年月で確認
                   INITIALIZE                  HENREI-K-REC
                   MOVE   SPA-HOSPNUM      TO  HENREI-K-HOSPNUM
                   MOVE   SPA-NAI-T05-TFLG3 (IDY)
                                           TO  HENREI-K-TEISYUTUSAKI
                   MOVE   SPA-NAI-T05-TFLG2 (IDY)
                                           TO  HENREI-K-TEISYUTUSAKI2
                   MOVE   SPA-NAI-T05-TSRYYM(IDY)
                                           TO  HENREI-K-SYORIYM
                   MOVE   SPACE            TO  HENREI-K-HOSPCD
                   PERFORM 900-HENREI-KANRI-READ-SEC
                   IF      FLG-HENREI-K    =   ZERO
                       INITIALIZE                  HENREI-B-REC
                       MOVE    SPA-HOSPNUM     TO  HENREI-B-HOSPNUM
                       MOVE    HENREI-K-SYORIYM
                                               TO  HENREI-B-SYORIYM
                       MOVE    HENREI-K-TEISYUTUSAKI
                                               TO  HENREI-B-TEISYUTUSAKI
                       MOVE    HENREI-K-TEISYUTUSAKI2
                                               TO HENREI-B-TEISYUTUSAKI2
                       MOVE    HENREI-K-HOSPCD TO  HENREI-B-HOSPCD
                       PERFORM 900-HENREI-BODY-READ-SEC
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       100
                               OR      SPA-NAI-1001-TSTYUKYMD  (IDX)
                                           =       SPACE
                           IF    ( HENREI-B-SRYYM
                                       >=  SPA-NAI-1001-TSTYUKYMD (IDX)
                                                            (1:6) )
                           AND   ( HENREI-B-SRYYM
                                       <=  SPA-NAI-1001-TEDYUKYMD (IDX)
                                                            (1:6) )
                               IF      SPA-NAI-1001-THOSPCD (IDX)
                                           =   SPA-NAI-T05-THOSPCD (IDY)
                                   MOVE    4       TO  WRK-ERRMSG1
                                   GO  TO  7002-FILE-CHECK-EXT
                               END-IF
                               MOVE    100             TO  IDX
                           END-IF
                       END-PERFORM
                   END-IF
               WHEN    "RRECEC"
               WHEN    "rrecec"
                   INITIALIZE                  HENREI-K-REC
                   MOVE   SPA-HOSPNUM      TO  HENREI-K-HOSPNUM
                   MOVE   SPA-NAI-T05-TFLG3 (IDY)
                                           TO  HENREI-K-TEISYUTUSAKI
                   MOVE   SPA-NAI-T05-TFLG2 (IDY)
                                           TO  HENREI-K-TEISYUTUSAKI2
                   MOVE   SPA-NAI-T05-TSRYYM(IDY)
                                           TO  HENREI-K-SYORIYM
                   MOVE   SPA-NAI-T05-THOSPCD (IDY)
                                           TO  HENREI-K-HOSPCD
                   PERFORM 900-HENREI-KANRI-READ-SEC
                   IF      FLG-HENREI-K    =   ZERO
                       MOVE    4               TO  WRK-ERRMSG1
                       GO  TO  7002-FILE-CHECK-EXT
                   END-IF
           END-EVALUATE
      *
      *    エラー件数のチェック（１０００件超えた場合は取り込み不可）
           IF      ERR-CNT                 >   1000
               MOVE    98                      TO  WRK-ERRMSG1
           END-IF
           .
       7002-FILE-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ヘッダ項目取得処理
      *****************************************************************
       70021-HEAD-CHECK-SEC          SECTION.
      *
           EVALUATE    WRK-FILENM(1:6)
               WHEN    "RECEIP"
               WHEN    "receip"
                   ADD     1               TO  SPA-GMN-T05-TCNT(IDY)
               WHEN    "RRECEC"
               WHEN    "rrecec"
                   ADD     1               TO  SPA-GMN-T05-TCNT(IDY)
           END-EVALUATE
      *
           MOVE    ZERO            TO  WRK-POINT-AREA
           MOVE    ZERO            TO  FLG-DATAEND
           MOVE    ZERO            TO  WRK-KOUMOKU-NO
      *
      *    患者ＣＳＶファイルに存在する項目数分、繰り返す
      *    またはレコード終了まで
           PERFORM VARYING   IDX   FROM    1   BY  1
                   UNTIL   ( IDX           >   SPILIT-CNT)
                   OR      ( WRK-PO-ED     >=  3000 )
      *
               MOVE    ZERO            TO  FLG-AREA1
               COMPUTE WRK-PO-ST   =   WRK-PO-ED   +   1
      *
               IF  ( PARA-DATA (WRK-PO-ST:)    =   SPACE )  OR
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
      *
               IF      FLG-DATAEND     =   ZERO
                IF     CNT-PARA        =   WK-HEAD-LINE
      *            ヘッダ項目別編集処理
                   PERFORM 700211-HEAD-HENSYU-SEC
                ELSE
      *            ボディ項目別編集処理
                   PERFORM 700211-BODY-HENSYU-SEC
                END-IF
               END-IF
           END-PERFORM
      *
           .
       70021-HEAD-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ヘッダ項目別編集処理
      *****************************************************************
       700211-HEAD-HENSYU-SEC        SECTION.
      *
           EVALUATE    WRK-FILENM(1:6)
               WHEN    "RIhasp"
               WHEN    "rihasp"
               WHEN    "RIuasp"
               WHEN    "riuasp"
                   EVALUATE    WRK-KOUMOKU-NO
      *                １：診療年月
                       WHEN    1
                           PERFORM 2401-UKE-SRYYM-SET-SEC
      *                ２：送信年月日
                       WHEN    2
                           PERFORM 2402-UKE-SENDYMD-SET-SEC
      *                ６：医療機関コード
                       WHEN    6
                           MOVE    SPACE               TO  WRK-HOSPCD
                           IF      WRK-MO-CNT          =   14
                               INITIALIZE              ORCSZENHANCNVAREA
                               MOVE    PARA-DATA (WRK-MO-ST:WRK-MO-CNT)
                                               TO  LNK-ZENHANCNV-IN-DATA
                               MOVE    14      TO  LNK-ZENHANCNV-IN-LEN
                               MOVE    WRK-MO-CNT
                                               TO  LNK-ZENHANCNV-IN-LEN1
                               MOVE    "2"     TO  LNK-ZENHANCNV-SYORI
                               CALL    "ORCSZENHANCNV"
                                               USING   ORCSZENHANCNVAREA
                               IF      LNK-ZENHANCNV-RC    =   ZERO
                                   MOVE    LNK-ZENHANCNV-OT-DATA (1:7)
                                                   TO  WRK-HOSPCD
                               ELSE
                                   MOVE    22      TO  WRK-ERRMSG1
                               END-IF                 
                           ELSE
                               MOVE    22      TO  WRK-ERRMSG1
                           END-IF
      *??
               DISPLAY "HASP WRK-HOSPCD=" WRK-HOSPCD "#" 
      *??
      *                ９：送信回数
                       WHEN    9
                           PERFORM 2404-UKE-SENDNUM-SET-SEC
                   END-EVALUATE
               WHEN    "RIfixf"
               WHEN    "rifixf"
                   EVALUATE    WRK-KOUMOKU-NO
      *                ２：請求年月
                       WHEN    2
                           PERFORM 2410S-SKYYM-SET-SEC
      *                ３：請求確定年月日
                       WHEN    3
                           PERFORM 2410S-SKYYMD-SET-SEC
      *                ４：審査支払機関コード
                       WHEN    4
                           EVALUATE    PARA-DATA (WRK-MO-ST:WRK-MO-CNT)
                               WHEN    "1"
                               WHEN    "2"
                                   CONTINUE
                               WHEN    OTHER
                                   MOVE    16          TO  WRK-ERRMSG1
                           END-EVALUATE
      *                ７：医療機関コード
                       WHEN    7
                           MOVE    SPACE               TO  WRK-HOSPCD
                           IF      WRK-MO-CNT          =   7
                               MOVE    PARA-DATA (WRK-MO-ST:WRK-MO-CNT)
                                                       TO  WRK-HOSPCD
                           ELSE
                               MOVE    22              TO  WRK-ERRMSG1
                           END-IF
      *??
               DISPLAY "FIX WRK-HOSPCD=" WRK-HOSPCD "#" 
      *??
      *                ９：送信回
                       WHEN    9
                           INITIALIZE                  ORCSNUMAREA
                           MOVE    PARA-DATA (WRK-MO-ST:WRK-MO-CNT)
                                                       TO  SNUM-INX
                           CALL    "ORCSNUM"           USING
                                                       ORCSNUMAREA
                           IF    ( SNUM-RC             =   ZERO  )
                           AND   ( SNUM-MINKBN         =   SPACE )
                           AND   ( SNUM-SYOKBN         =   SPACE )
                           AND   ( SNUM-SEISUU         <=  3     )
                           AND   ( SNUM-OUTNUM         >   ZERO  )
                               MOVE    SNUM-OUTNUM     TO
                                               SPA-GMN-T05-TUKAISU(IDY)
                           ELSE
                               MOVE    10              TO  WRK-ERRMSG1
                           END-IF
      *                １０：請求確定回
                       WHEN    10
                           INITIALIZE                  ORCSNUMAREA
                           MOVE    PARA-DATA (WRK-MO-ST:WRK-MO-CNT)
                                                       TO  SNUM-INX
                           CALL    "ORCSNUM"           USING
                                                       ORCSNUMAREA
                           IF    ( SNUM-RC             =   ZERO  )
                           AND   ( SNUM-MINKBN         =   SPACE )
                           AND   ( SNUM-SYOKBN         =   SPACE )
                           AND   ( SNUM-SEISUU         >=  1   AND
                                                       <=  3     )
                           AND   ( SNUM-OUTNUM         >   ZERO  )
                               MOVE    SNUM-OUTNUM     TO
                                               SPA-GMN-T05-TSKAISU(IDY)
                           ELSE
                               MOVE    11              TO  WRK-ERRMSG1
                           END-IF
                   END-EVALUATE
               WHEN    "RECEIP"
               WHEN    "receip"
                   EVALUATE    WRK-KOUMOKU-NO
      *                レコード識別情報
                       WHEN    1
                           IF      PARA-DATA (WRK-MO-ST:WRK-MO-CNT)
                                                   NOT =   "HI"
                               MOVE    18              TO  WRK-ERRMSG1
                           END-IF
      *                審査支払機関コード
                       WHEN    2
                           EVALUATE    PARA-DATA (WRK-MO-ST:WRK-MO-CNT)
                               WHEN    "1"
                               WHEN    "2"
                                   MOVE
                                       PARA-DATA (WRK-MO-ST:WRK-MO-CNT)
                                                       TO
                                                       WRK-TEISYUTUSAKI
                               WHEN    OTHER
                                   MOVE    16          TO  WRK-ERRMSG1
                           END-EVALUATE
      *                請求年月
                       WHEN    3
                           INITIALIZE                      ORCSGDAYAREA
                           MOVE    "1"                 TO  SGDAY-INTYPE
                           MOVE    PARA-DATA (WRK-MO-ST:WRK-MO-CNT)
                                                       TO  SGDAY-INDAY
                           CALL    "ORCSGDAY"          USING
                                                       ORCSGDAYAREA
                           IF      SGDAY-RC            =   ZERO
                               MOVE    SGDAY-OTDAY(1:6)    TO
                                               SPA-GMN-T05-TSRYYM (IDY)
                               MOVE    SGDAY-SDAY (1:6)    TO
                                               SPA-NAI-T05-TSRYYM (IDY)
                           ELSE
                               MOVE    14              TO  WRK-ERRMSG1
                           END-IF
      *                都道府県
                       WHEN    4
                           MOVE    SPACE               TO  WRK-PREFNUM
                           IF      WRK-MO-CNT          =   2
                               MOVE    PARA-DATA (WRK-MO-ST:WRK-MO-CNT)
                                                       TO  WRK-PREFNUM
                           ELSE
                               MOVE    19              TO  WRK-ERRMSG1
                           END-IF
      *                点数表
                       WHEN    5
                           IF      PARA-DATA (WRK-MO-ST:WRK-MO-CNT)
                                                   NOT =   "1"
                               MOVE    20              TO  WRK-ERRMSG1
                           END-IF
      *                医療機関コード
                       WHEN    6
                           MOVE    SPACE               TO  WRK-HOSPCD
                           IF      WRK-MO-CNT          =   7
                               MOVE    PARA-DATA (WRK-MO-ST:WRK-MO-CNT)
                                                       TO  WRK-HOSPCD
                           ELSE
                               MOVE    22              TO  WRK-ERRMSG1
                           END-IF
      *??
               DISPLAY "RECE WRK-HOSPCD=" WRK-HOSPCD "#" 
      *??
                       WHEN    8
                           IF      PARA-DATA (WRK-MO-ST:WRK-MO-CNT)
                                                   NOT =   "00"
                               MOVE    21              TO  WRK-ERRMSG1
                           END-IF
                   END-EVALUATE
               WHEN    "RRECEC"
               WHEN    "rrecec"
                   EVALUATE    WRK-KOUMOKU-NO
      *                レコード識別情報
                       WHEN    1
                           IF      PARA-DATA (WRK-MO-ST:WRK-MO-CNT)
                                                   NOT =   "RE"
                               MOVE    18              TO  WRK-ERRMSG1
                           END-IF
      *                請求年月
                       WHEN    21
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO  WRK-SEIKYU-DATA
      *??
            DISPLAY "SEIKYU=" WRK-SEIKYU-SKYYM "#" WRK-SEIKYU-SRYYM "#"
      *??
                           INITIALIZE                  ORCSGDAYAREA
                           MOVE    "1"             TO  SGDAY-INTYPE
                           MOVE    WRK-SEIKYU-SKYYM
                                                   TO  SGDAY-INDAY
                           CALL    "ORCSGDAY"      USING
                                                   ORCSGDAYAREA
                           IF      SGDAY-RC        =   ZERO
                               MOVE    SGDAY-OTDAY(1:6)    TO
                                               SPA-GMN-T05-TSRYYM (IDY)
                               MOVE    SGDAY-SDAY (1:6)    TO
                                               SPA-NAI-T05-TSRYYM (IDY)
                           ELSE
                               MOVE    14              TO  WRK-ERRMSG1
                           END-IF
      *                    診療年月
                           INITIALIZE              ORCSGDAYAREA
                           MOVE    "1"             TO  SGDAY-INTYPE
                           MOVE    WRK-SEIKYU-SRYYM
                                                   TO  SGDAY-INDAY
                           CALL    "ORCSGDAY"      USING
                                                   ORCSGDAYAREA
                           IF      SGDAY-RC        =   ZERO
                               MOVE    SGDAY-SDAY (1:6)    TO
                                               SPA-NAI-T05-TSRYYM-B(IDY)
                           ELSE
                               MOVE    12          TO  WRK-ERRMSG1
                           END-IF
                   END-EVALUATE
           END-EVALUATE
      *
           .
       700211-HEAD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ボディ項目別編集処理
      *****************************************************************
       700211-BODY-HENSYU-SEC        SECTION.
      *
           EVALUATE    WRK-KOUMOKU-NO
      *    ２９：確定区分
               WHEN    29
                   IF  PARA-DATA (WRK-MO-ST:WRK-MO-CNT) = "2"
                       ADD   1         TO  ERR-CNT
                   END-IF
           END-EVALUATE
      *
           .
       700211-BODY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月（受付）変換処理
      *****************************************************************
       2401-UKE-SRYYM-SET-SEC        SECTION.
      *
      *
           INITIALIZE                      ORCSZENHANCNVAREA
           MOVE    PARA-DATA (WRK-MO-ST:WRK-MO-CNT)
                                       TO  LNK-ZENHANCNV-IN-DATA
           MOVE    16                  TO  LNK-ZENHANCNV-IN-LEN
           MOVE    WRK-MO-CNT          TO  LNK-ZENHANCNV-IN-LEN1
           MOVE    "1"                 TO  LNK-ZENHANCNV-SYORI
           CALL    "ORCSZENHANCNV"     USING   ORCSZENHANCNVAREA
           IF      LNK-ZENHANCNV-RC    =   ZERO
               MOVE    LNK-ZENHANCNV-OT-GYMD (1:6)
                                           TO  SPA-GMN-T05-TSRYYM(IDY)
               MOVE    LNK-ZENHANCNV-OT-SYMD (1:6)
                                           TO  SPA-NAI-T05-TSRYYM(IDY)
           ELSE
               MOVE    12                  TO  WRK-ERRMSG1
           END-IF                 
           .
       2401-UKE-SRYYM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    送信年月日（受付）変換処理
      *****************************************************************
       2402-UKE-SENDYMD-SET-SEC        SECTION.
      *
      *
           INITIALIZE                      ORCSZENHANCNVAREA
           MOVE    PARA-DATA (WRK-MO-ST:WRK-MO-CNT)
                                       TO  LNK-ZENHANCNV-IN-DATA
           MOVE    22                  TO  LNK-ZENHANCNV-IN-LEN
           MOVE    WRK-MO-CNT          TO  LNK-ZENHANCNV-IN-LEN1
           MOVE    "1"                 TO  LNK-ZENHANCNV-SYORI
           CALL    "ORCSZENHANCNV"     USING   ORCSZENHANCNVAREA
           IF      LNK-ZENHANCNV-RC    =   ZERO
               MOVE    LNK-ZENHANCNV-OT-GYMD
                                           TO  SPA-GMN-T05-TSENDYMD(IDY)
           ELSE
               MOVE    13                  TO  WRK-ERRMSG1
           END-IF                 
           .
       2402-UKE-SENDYMD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    送信回数（受付）変換処理
      *****************************************************************
       2404-UKE-SENDNUM-SET-SEC        SECTION.
      *
      *
           INITIALIZE                      ORCSZENHANCNVAREA
           MOVE    PARA-DATA (WRK-MO-ST:WRK-MO-CNT)
                                       TO  LNK-ZENHANCNV-IN-DATA
           MOVE    6                   TO  LNK-ZENHANCNV-IN-LEN
           MOVE    WRK-MO-CNT          TO  LNK-ZENHANCNV-IN-LEN1
           MOVE    "2"                 TO  LNK-ZENHANCNV-SYORI
           CALL    "ORCSZENHANCNV"     USING   ORCSZENHANCNVAREA
           IF      LNK-ZENHANCNV-RC    =   ZERO
               IF      LNK-ZENHANCNV-OT-NUM    =   ZERO
                   MOVE    17              TO  WRK-ERRMSG1
               ELSE
                   MOVE    LNK-ZENHANCNV-OT-NUM
                                           TO  SPA-GMN-T05-TUKAISU(IDY)
               END-IF                 
           ELSE
               MOVE    17                  TO  WRK-ERRMSG1
           END-IF                 
           .
       2404-UKE-SENDNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求年月（請求確定）変換処理
      *****************************************************************
       2410S-SKYYM-SET-SEC       SECTION.
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    "1"                 TO  SGDAY-INTYPE
           MOVE    PARA-DATA (WRK-MO-ST:WRK-MO-CNT)
                                       TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    SGDAY-SDAY (1:6)    TO  LNK-DAY6-KIJUN (1:6)
               MOVE    "01"                TO  LNK-DAY6-KIJUN (7:2)
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    -1                  TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN(1:6)
                                           TO  SPA-NAI-T05-TSRYYM (IDY)
               INITIALIZE                      ORCSGDAYAREA
               MOVE    "1"                 TO  SGDAY-INTYPE
               MOVE    LNK-DAY6-KEISAN(1:6)
                                           TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN-T05-TSRYYM (IDY)
           ELSE
               MOVE    14                  TO  WRK-ERRMSG1
           END-IF
           .
       2410S-SKYYM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求年月日（請求確定）変換処理
      *****************************************************************
       2410S-SKYYMD-SET-SEC      SECTION.
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    PARA-DATA (WRK-MO-ST:WRK-MO-CNT)
                                       TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO
                                           SPA-GMN-T05-TSENDYMD (IDY)
           ELSE
               MOVE    15                  TO  WRK-ERRMSG1
           END-IF                 
             .
       2410S-SKYYMD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細チェック処理
      *****************************************************************
       70022-BODY-CHECK-SEC             SECTION.
      *
           EVALUATE    WRK-FILENM(1:6)
               WHEN    "RIhasp"
               WHEN    "rihasp"
               WHEN    "RIuasp"
               WHEN    "riuasp"
                   ADD     1               TO  SPA-GMN-T05-TCNT(IDY)
                                               ERR-CNT
               WHEN    "RIfixf"
               WHEN    "rifixf"
                   IF      PARA-DATA (1:1) =   "2"
                       ADD     1               TO  SPA-GMN-T05-TCNT(IDY)
                       MOVE    30              TO  SPILIT-CNT
                       PERFORM 70021-HEAD-CHECK-SEC
                   END-IF
               WHEN    "RECEIP"
               WHEN    "receip"
                   ADD     1               TO  SPA-GMN-T05-TCNT(IDY)
                   IF      PARA-DATA (1:2) =   "HG"
                       MOVE    1               TO  FLG-HG-CHK
                   END-IF
               WHEN    "RRECEC"
               WHEN    "rrecec"
                   IF       PARA-DATA(1:1) =   X"1A"
                       CONTINUE
                   ELSE
                       ADD     1               TO  SPA-GMN-T05-TCNT(IDY)
                   END-IF
           END-EVALUATE
      *
           EVALUATE    WRK-FILENM(1:6)
               WHEN    "RIhasp"
               WHEN    "rihasp"
               WHEN    "RIuasp"
               WHEN    "riuasp"
               WHEN    "RIfixf"
               WHEN    "rifixf"
                   IF      SPA-GMN-T05-TCNT (IDY)  =   1
      *                明細編集処理（診療年月）
                       PERFORM 700221-BODY-HENSYU-SEC
                   END-IF
               WHEN    "RECEIP"
               WHEN    "receip"
                   IF      SPA-GMN-T05-TCNT (IDY)  =   2
      *                明細編集処理（診療年月）
                       PERFORM 700221-BODY-HENSYU-SEC
                   END-IF
           END-EVALUATE
      *
           .
       70022-BODY-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細編集処理（診療年月）
      *****************************************************************
       700221-BODY-HENSYU-SEC           SECTION.
      *
           MOVE    ZERO            TO  WRK-POINT-AREA
           MOVE    ZERO            TO  FLG-DATAEND
           MOVE    ZERO            TO  WRK-KOUMOKU-NO
      *
      *    患者ＣＳＶファイルに存在する項目数分、繰り返す
      *    またはレコード終了まで
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX             >   SPILIT-CNT
                   OR      WRK-PO-ED       >=  3000
               MOVE    ZERO            TO  FLG-AREA1
               COMPUTE WRK-PO-ST   =   WRK-PO-ED   +   1
      *
               IF    ( PARA-DATA (WRK-PO-ST:)    =   SPACE )  OR
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
      *
               IF      FLG-DATAEND     =   ZERO
                   EVALUATE    WRK-FILENM(1:6)
                       WHEN    "RECEIP"
                       WHEN    "receip"
                           IF      WRK-KOUMOKU-NO  =   4
      *                        診療年月
                               MOVE    "1"             TO  SGDAY-INTYPE
                               MOVE    PARA-DATA (WRK-MO-ST:WRK-MO-CNT)
                                                       TO  SGDAY-INDAY
                               CALL    "ORCSGDAY"          USING
                                                       ORCSGDAYAREA
                               IF      SGDAY-RC        =   ZERO
                                   MOVE    SGDAY-SDAY (1:6)    TO
                                               SPA-NAI-T05-TSRYYM-B(IDY)
                               ELSE
                                   MOVE    12          TO  WRK-ERRMSG1
                               END-IF
                           END-IF
                       WHEN    "RIhasp"
                       WHEN    "rihasp"
                       WHEN    "RIuasp"
                       WHEN    "riuasp"
                           IF      WRK-KOUMOKU-NO  =   6
      *                        診療年月
                               MOVE    "1"             TO  SGDAY-INTYPE
                           INITIALIZE          ORCSZENHANCNVAREA
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  LNK-ZENHANCNV-IN-DATA
                           MOVE    14          TO  LNK-ZENHANCNV-IN-LEN
                           MOVE    WRK-MO-CNT  TO  LNK-ZENHANCNV-IN-LEN1
                           MOVE    "1"         TO  LNK-ZENHANCNV-SYORI
                           CALL    "ORCSZENHANCNV" USING
                                                   ORCSZENHANCNVAREA
                           IF      LNK-ZENHANCNV-RC    =   ZERO
                               MOVE    LNK-ZENHANCNV-OT-SYMD (1:6) TO
                                               SPA-NAI-T05-TSRYYM-B(IDY)
                               ELSE
                                   MOVE    12          TO  WRK-ERRMSG1
                               END-IF
                           END-IF
                       WHEN    "RIfixf"
                       WHEN    "rifixf"
                           IF      WRK-KOUMOKU-NO  =   4
      *                        診療年月
                               MOVE    "1"             TO  SGDAY-INTYPE
                               MOVE    PARA-DATA (WRK-MO-ST:WRK-MO-CNT)
                                                       TO  SGDAY-INDAY
                               CALL    "ORCSGDAY"          USING
                                                       ORCSGDAYAREA
                               IF      SGDAY-RC        =   ZERO
                                   MOVE    SGDAY-SDAY (1:6)    TO
                                               SPA-NAI-T05-TSRYYM-B(IDY)
                               ELSE
                                   MOVE    12          TO  WRK-ERRMSG1
                               END-IF
                           END-IF
                   END-EVALUATE
               END-IF
           END-PERFORM
      *??
           DISPLAY "SRYYM-B=" WRK-FILENM(1:6) "#"
                           SPA-NAI-T05-TSRYYM-B(IDY) "#"
      *??
      *
           .
       700221-BODY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    送信回数チェック
      *****************************************************************
       7003-SEND-CHECK-SEC           SECTION.
      *
           IF    ( SPA-NAI-T05-TFLG3(IDY)
                                       =   SPA-NAI-T05-TFLG3(IDX) )
           AND   ( SPA-NAI-T05-TFLG2(IDY)
                                       =   SPA-NAI-T05-TFLG2(IDX) )
               IF      SPA-GMN-T05-TUKAISU(IDY)
                                       NOT =   SPA-GMN-T05-TUKAISU(IDX)
                   MOVE    "受付の送信回数が異なります"
                                           TO  SPA-GMN-T05-ERRMSG2
                                               SPA-NAI-T05-ERR-MSG(IDY)
                   MOVE  "○"              TO  SPA-GMN-T05-TFLG4  (IDY)
               END-IF
           END-IF
           .
       7003-SEND-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除ガイドメッセージ処理
      *****************************************************************
       220-DEL-GID-SEC                 SECTION.
      *
           IF      SPA-NAI-T05-SELNUM  =   ZERO
               MOVE    "0004"              TO  SPA-ERRCD
               GO  TO  220-DEL-GID-EXT
           END-IF
      *
           MOVE    "0101"             TO  SPA-TIDCD
          .
       220-DEL-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       220-DEL-SEC                 SECTION.
      *
      *    取り込みファイル削除
           MOVE    ZERO                TO  IF-RESULT
           MOVE    SPA-NAI-T05-SAVE-NAME (SPA-NAI-T05-SELNUM)
                                       TO  IF-FILENAME
           CALL    "orcfiledel"    USING   ORCSFDELAREA
      *
      *    選択行削除
           MOVE    SPA-NAI-T05-SELNUM  TO  IDX
           INITIALIZE                  SPA-GMN-T05-DATALST (IDX)
           INITIALIZE                  SPA-NAI-T05-DATALST (IDX)
           IF      SPA-GMN-T05-DATALST-MAX
                                       =   1
               MOVE    ZERO                TO  SPA-GMN-T05-DATALST-MAX
           ELSE
               PERFORM VARYING IDXX    FROM    IDX     BY  1
                       UNTIL   IDXX    >       SPA-GMN-T05-DATALST-MAX
                   IF      IDXX            =   SPA-GMN-T05-DATALST-MAX
                       INITIALIZE          SPA-GMN-T05-DATALST (IDXX)
                       INITIALIZE          SPA-NAI-T05-DATALST (IDXX)
                   ELSE 
                       COMPUTE IDXY    =   IDXX    +   1
                       MOVE    SPA-GMN-T05-DATALST (IDXY)
                                           TO  SPA-GMN-T05-DATALST(IDXX)
                       MOVE    SPA-NAI-T05-DATALST (IDXY)
                                           TO  SPA-NAI-T05-DATALST(IDXX)
                       MOVE    IDXX        TO  SPA-GMN-T05-TRENNUM(IDXX)
                   END-IF
               END-PERFORM
               COMPUTE SPA-GMN-T05-DATALST-MAX
                           =   SPA-GMN-T05-DATALST-MAX    -   1
           END-IF    
      *
           IF      SPA-GMN-T05-DATALST-MAX
                                       =   ZERO
               MOVE    ZERO                TO  FLG-ROW
           ELSE
               COMPUTE IDX     =   SPA-NAI-T05-SELNUM  -   1
               MOVE    IDX                 TO  T05-DATALST-ROW
           END-IF
      *
           INITIALIZE                  SPA-GMN-T05-KOUMOKU
                                       SPA-NAI-T05-KOUMOKU
      *
           MOVE    4                   TO  SPA-GMN-T05-CUR
      *
           .
       220-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角ＪＩＳ→ＥＵＣ変換処理
      *****************************************************************
       7002-FILE-CONVERT-SEC       SECTION.
      *
           INITIALIZE                  SJISTOEUC-OUTDATA
           MOVE    10000                TO  SJISTOEUC-INLEN
           MOVE    10000                TO  SJISTOEUC-OUTLEN
           CALL    "orcsjistoeuc"      USING
                                       ORCSJISTOEUC
      *
           .
       7002-FILE-CONVERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ登録ガイドメッセージ処理
      *****************************************************************
       210-TOUROKU-GID-SEC                 SECTION.
      *
           INITIALIZE                      SPA-GMN-T05-KOUMOKU
                                           SPA-NAI-T05-KOUMOKU
           MOVE    T05-ENTRY1          TO  SPA-GMN-T05-ENTRY1
           MOVE    ZERO                TO  WRK-ERRMSG1
                                           WRK-ERRMSG2
           MOVE    SPACE               TO  SPA-TIDCD 
      *
           IF      SPA-GMN-T05-DATALST-MAX =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               GO  TO  210-TOUROKU-GID-EXT
           END-IF
      *
      *    ＤＢ未登録ファイルがあるか
           MOVE    ZERO                TO  FLG-DB-INSERT
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-T05-DATALST-MAX
                   OR      FLG-DB-INSERT   =   1  
               IF      SPA-NAI-T05-TDB (IDX)
                                       =   SPACE
                   MOVE    1               TO  FLG-DB-INSERT
               END-IF
           END-PERFORM
           IF      FLG-DB-INSERT       =   ZERO
               MOVE   "0003"               TO  SPA-ERRCD
               GO  TO  210-TOUROKU-GID-EXT
           END-IF
      *
      *    ジョブ管理チェック処理
           IF      SPA-ERRCD       =   SPACE
               PERFORM 4501-JOBKANRI-CHK-SEC
               IF      SPA-ERRCD       =   SPACE
                   MOVE    "1001"             TO  SPA-TIDCD
               END-IF
           END-IF
          .
       210-TOUROKU-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ登録処理
      *****************************************************************
       210-TOUROKU-SEC                 SECTION.
      * 
      *    ジョブ管理削除処理
           MOVE    "ALL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN    =   ZERO
               CONTINUE
           ELSE
               MOVE    "1004"              TO  SPA-ERRCD
               GO  TO  210-TOUROKU-EXT
           END-IF
      *
      *    未登録ファイルのみ抜きだし
           INITIALIZE                  TABLE-SAVE-AREA
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-T05-DATALST-MAX
               IF      SPA-NAI-T05-TDB (IDX)
                                       =   SPACE
                   ADD     1           TO  TBL-SAVE-MAX
                   MOVE    SPA-NAI-T05-SAVE-BASENAME(IDX)
                                       TO  TBL-SAVE-NAME (TBL-SAVE-MAX)
                   MOVE    SPA-NAI-T05-THOSPCD      (IDX)
                                       TO  TBL-SAVE-HOSPCD
                                                         (TBL-SAVE-MAX)
               END-IF
           END-PERFORM
      *
      *    エラーファイル名設定     
           MOVE    WRK-CONS-FILE-ID    TO  ERRFLPARA-FILE-ID
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    SPACE               TO  RECEERR
           STRING  MCP-TEMPDIR         DELIMITED   BY  SPACE
                   "/"                 DELIMITED   BY  SIZE
                   ERRFLPARA-BASENAME  DELIMITED   BY  SPACE
                                       INTO        RECEERR
           END-STRING
      *
      *    ジョブ開始セット処理
           MOVE    ZERO            TO  WRK-CONS-JOB-JOBID     
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *             
           MOVE    SPACE           TO  SHELLTBL
           MOVE    "receipt_export.sh"
                                   TO  SHELLTBL-NAME
      *                                 
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG1
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG2
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG3
           MOVE    ERRFLPARA-BASENAME
                                   TO  SHELLTBL-ARG4
           MOVE    TBL-SAVE-NAME (1)
                                   TO  SHELLTBL-ARG5
           MOVE    TBL-SAVE-NAME (2)
                                   TO  SHELLTBL-ARG6
           MOVE    TBL-SAVE-NAME (3)
                                   TO  SHELLTBL-ARG7
           MOVE    TBL-SAVE-NAME (4)
                                   TO  SHELLTBL-ARG8
           MOVE    TBL-SAVE-NAME (5)
                                   TO  SHELLTBL-ARG9
           MOVE    TBL-SAVE-NAME (6)
                                   TO  SHELLTBL-ARG10
           MOVE    TBL-SAVE-NAME (7)
                                   TO  SHELLTBL-ARG11
           MOVE    TBL-SAVE-NAME (8)
                                   TO  SHELLTBL-ARG12
           MOVE    TBL-SAVE-NAME (9)
                                   TO  SHELLTBL-ARG13
           MOVE    TBL-SAVE-NAME (10)
                                   TO  SHELLTBL-ARG14
           MOVE    TBL-SAVE-NAME (11)
                                   TO  SHELLTBL-ARG15
           MOVE    TBL-SAVE-NAME (12)
                                   TO  SHELLTBL-ARG16
           MOVE    TBL-SAVE-NAME (13)
                                   TO  SHELLTBL-ARG17
           MOVE    TBL-SAVE-NAME (14)
                                   TO  SHELLTBL-ARG18
           MOVE    TBL-SAVE-NAME (15)
                                   TO  SHELLTBL-ARG19
           MOVE    TBL-SAVE-HOSPCD (1)
                                   TO  SHELLTBL-ARG20
           MOVE    TBL-SAVE-HOSPCD (2)
                                   TO  SHELLTBL-ARG21
           MOVE    TBL-SAVE-HOSPCD (3)
                                   TO  SHELLTBL-ARG22
           MOVE    TBL-SAVE-HOSPCD (4)
                                   TO  SHELLTBL-ARG23
           MOVE    TBL-SAVE-HOSPCD (5)
                                   TO  SHELLTBL-ARG24
           MOVE    TBL-SAVE-HOSPCD (6)
                                   TO  SHELLTBL-ARG25
           MOVE    TBL-SAVE-HOSPCD (7)
                                   TO  SHELLTBL-ARG26
           MOVE    TBL-SAVE-HOSPCD (8)
                                   TO  SHELLTBL-ARG27
           MOVE    TBL-SAVE-HOSPCD (9)
                                   TO  SHELLTBL-ARG28
           MOVE    TBL-SAVE-HOSPCD (10)
                                   TO  SHELLTBL-ARG29
           MOVE    TBL-SAVE-HOSPCD (11)
                                   TO  SHELLTBL-ARG30
           MOVE    TBL-SAVE-HOSPCD (12)
                                   TO  SHELLTBL-ARG31
           MOVE    TBL-SAVE-HOSPCD (13)
                                   TO  SHELLTBL-ARG32
           MOVE    TBL-SAVE-HOSPCD (14)
                                   TO  SHELLTBL-ARG33
           MOVE    TBL-SAVE-HOSPCD (15)
                                   TO  SHELLTBL-ARG34
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "monthly"       TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
      *    ＪＯＢＩＤ更新処理
           PERFORM 45012-JOBID-UPD-SEC
      *     
      *    情報確認画面を表示                                                   
           PERFORM 490-KAKUNIN-SEC
            .
       210-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       4501-JOBKANRI-CHK-SEC       SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRCD
      *
      *    ジョブ管理チェック        
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-3003-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-3003-STYUKYMD (7:2)
           MOVE    SYS-3003-STYUKYMD   TO  SYS-3003-EDYUKYMD 
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SYS-3003-JOBID
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
                   IF      SJOBKANRI-RETURN
                                           =   ZERO
                       IF      JOB-ENDYMD      =   SPACE
                           MOVE    "7777"          TO  SPA-ERRCD
                           MOVE    SPACE           TO  WRK-RECEERR
                           STRING  "処理中です【"  DELIMITED  BY  SIZE
                                   JOB-SHELLMSG    DELIMITED  BY  SPACE
                                   "】"            DELIMITED  BY  SIZE      
                                                   INTO  WRK-RECEERR
                           END-STRING
                           MOVE    SYS-3003-JOBID  TO  IDX
                       END-IF
                   END-IF    
               END-PERFORM
           END-IF
      *
      *
      *    ジョブ管理チェック（レセプト）
           IF      SPA-ERRCD           =   SPACE         
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-CONS-JOB-JOBID-RECEPT1
                                       TO  JOB-JOBID
               MOVE    WRK-CONS-JOB-SHELLID-RECEPT1
                                       TO  JOB-SHELLID
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
              IF      SJOBKANRI-RETURN
                                       =   ZERO
                   IF      JOB-ENDYMD      =   SPACE
                       MOVE    "7777"          TO  SPA-ERRCD
                       MOVE    SPACE             TO  WRK-RECEERR
                       STRING  "レセプト作成処理中です【"
                                                 DELIMITED  BY  SIZE
                               JOB-SHELLMSG      DELIMITED  BY  SPACE
                               "】"              DELIMITED  BY  SIZE      
                                                 INTO  WRK-RECEERR
                       END-STRING
                   END-IF
               END-IF    
           END-IF    
           .
       4501-JOBKANRI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ開始セット処理
      *****************************************************************
       45011-JOBKANRI-HENSYU-SEC       SECTION.
      *
           ADD     1               TO  WRK-CONS-JOB-JOBID
      *       
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           .
       45011-JOBKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＪＯＢＩＤ更新処理
      *****************************************************************
       45012-JOBID-UPD-SEC    SECTION.
      *
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-3003-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-3003-STYUKYMD (7:2)
           MOVE    SYS-3003-STYUKYMD   TO  SYS-3003-EDYUKYMD 
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC
           ELSE       
               MOVE    SPACE               TO  SYS-3003-REC 
               INITIALIZE                      SYS-3003-REC
               MOVE    "3003"              TO  SYS-3003-KANRICD
               MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
               MOVE    ZERO                TO  SYS-3003-STYUKYMD
               MOVE    ALL "9"             TO  SYS-3003-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           END-IF    
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SYS-3003-TBL-KEY
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                       TO  SYS-3003-TBL-GROUP
           MOVE    WRK-CONS-JOB-JOBID  TO  SYS-3003-JOBID                                      
      *
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF      FLG-SYSKANRI        =   ZERO     
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           ELSE
               MOVE    "DBINSERT"          TO  MCP-FUNC
           END-IF         
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "SYSKANRI 3003  ERR KEY = ["
                                  MCP-RC  " " SYS-3003-KEY  "]"   
      *****    MOVE    "1010"          TO  SPA-ERRCD
           END-IF
      *
           .
       45012-JOBID-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る前チェック処理
      *****************************************************************
       210-BACK                    SECTION.
      *        
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-3003-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-3003-STYUKYMD (7:2)
           MOVE    SYS-3003-STYUKYMD   TO  SYS-3003-EDYUKYMD 
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SYS-3003-JOBID
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
                   PERFORM 900-CALL-ORCSJOB-SEC
                   IF      SJOBKANRI-RETURN
                                           =   ZERO
                       IF      JOB-ENDYMD      =   SPACE
                           MOVE    "6666"          TO  SPA-TIDCD
                           GO  TO  210-BACK-EXT
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    ＤＢ未登録ファイルがあるとき
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-T05-DATALST-MAX
               IF      SPA-NAI-T05-TDB (IDX)
                                       =   SPACE
                   MOVE    "1002"          TO  SPA-TIDCD
                   GO  TO  210-BACK-EXT
               END-IF
           END-PERFORM
      *
           PERFORM 210-BACK-SYORI-SEC         
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK-SYORI-SEC              SECTION.
      *
           MOVE    "T01"               TO  SPA-SAKIPG
           MOVE    "T05"               TO  SPA-MOTOPG
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
           INITIALIZE                      T99
           MOVE    ZERO                TO  T99-COUNT
           MOVE    "black"             TO  T99-MSG-STYLE
      *
           MOVE    5                   TO  T99-DURATION
           MOVE    WIDGET-INSENSITIVE  TO  T99-B12-STATE
      *        
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-3003-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-3003-STYUKYMD (7:2)
           MOVE    SYS-3003-STYUKYMD   TO  SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SYS-3003-JOBID
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
                   IF      SJOBKANRI-RETURN    =   ZERO
                       MOVE    JOB-JOBID       TO  T99-RENNUM   (IDX)
                       MOVE    JOB-SHELLMSG    TO  T99-SHORINM  (IDX)
                       MOVE    JOB-STARTTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-STARTTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-STARTTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  T99-STARTTIME(IDX)
                       MOVE    JOB-ENDTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-ENDTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-ENDTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  T99-ENDTIME  (IDX)
      ****             MOVE    JOB-UPDCNT      TO  WRK-PAGE
      ****             MOVE    WRK-PAGE        TO  T99-PAGE     (IDX)
                       MOVE    JOB-YOBI        TO  T99-ERRMSG   (IDX)
                       IF      JOB-ENDYMD      =   SPACE
                           MOVE    "処理中です"    TO  T99-MSG
                       ELSE
                           IF      JOB-ERRCD       =   SPACE
                               CONTINUE
                           ELSE
                               MOVE    SPACE       TO  T99-MSG
                               IF      T99-MSG     =   SPACE
                                 STRING  JOB-YOBI    DELIMITED BY SPACE
                                         "【"        DELIMITED BY SIZE
                                        JOB-SHELLMSG DELIMITED BY SPACE
                                        "】"         DELIMITED BY SIZE
                                                     INTO  T99-MSG
                                 END-STRING   
                               END-IF
                           END-IF                          
                       END-IF    
                       MOVE    IDX                     TO  T99-COUNT    
                   END-IF
               END-PERFORM
           END-IF
           IF      T99-MSG              =   SPACE
               MOVE    "B01"                TO  MCP-WIDGET
               IF      T99-COUNT            >   ZERO    
                   MOVE    "処理は正常に終了しました"
                                                TO  T99-MSG
               ELSE
                   MOVE    "該当する処理はありません"
                                                TO  T99-MSG
               END-IF                                 
               MOVE    WIDGET-NORMAL       TO  T99-B01-STATE
           ELSE                                        
               MOVE    "B11"               TO  MCP-WIDGET
               MOVE    WIDGET-NORMAL       TO  T99-B01-STATE
           END-IF                                 
      *
      *
           MOVE    "T05"                TO  SPA-MOTOPG
           MOVE    "T99"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "T99"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
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
           IF      SPA-TIDCD       NOT =   SPACE
               PERFORM 520-TIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "T05    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           IF      FLG-ROW             =   1
               MOVE    ZERO                TO  WRK-T05-DATALST-ROW
           ELSE
               MOVE    T05-DATALST-ROW     TO  WRK-T05-DATALST-ROW
           END-IF
      *
           INITIALIZE                      T05
           MOVE   LOW-VALUE            TO  T05-OBJECTDATA
      **   MOVE    SPACE               TO  T05-ENTRY1
      *
           MOVE    SPA-GMN-T05-SELNUM  TO  T05-SELNUM
           MOVE    SPA-GMN-T05-SELNAME1
                                       TO  T05-SELNAME1
           MOVE    SPA-GMN-T05-SELNAME2
                                       TO  T05-SELNAME2
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-GMN-T05-DATALST-MAX
               MOVE    SPA-GMN-T05-TRENNUM (IDX)
                                       TO  WRK-NUM
               MOVE    WRK-NUM         TO  T05-DATALST-TRENNUM (IDX)
               MOVE  SPA-GMN-T05-TFILENM (IDX)
                                       TO  T05-DATALST-TFILENM (IDX)
               MOVE  SPA-GMN-T05-TSRYYM (IDX)
                                       TO  T05-DATALST-TSRYYM  (IDX)
               MOVE  SPA-GMN-T05-TSENDYMD (IDX)
                                       TO  T05-DATALST-TSENDYM (IDX)
               MOVE  SPA-GMN-T05-TUKAISU (IDX)
                                       TO  WRK-NUM3
               MOVE    WRK-NUM3        TO  T05-DATALST-TUKAISU (IDX)
               MOVE  SPA-GMN-T05-TSKAISU (IDX)
                                       TO  WRK-NUM3
               MOVE    WRK-NUM3        TO  T05-DATALST-TSKAISU (IDX)
               MOVE  SPA-GMN-T05-TNAIYO (IDX)
                                       TO  T05-DATALST-TDATA   (IDX)
               EVALUATE    SPA-NAI-T05-TFLG3 (IDX)
                   WHEN    "1"
                       MOVE    "社"        TO  T05-DATALST-TFLG3   (IDX)
                   WHEN    "2"
                       MOVE    "国"        TO  T05-DATALST-TFLG3   (IDX)
               END-EVALUATE
               EVALUATE    SPA-NAI-T05-TFLG2 (IDX)
                   WHEN    "0"
                       MOVE    "　"        TO  T05-DATALST-TFLG2   (IDX)
                   WHEN    "1"
                       MOVE    "保"        TO  T05-DATALST-TFLG2   (IDX)
               END-EVALUATE
               MOVE  SPA-GMN-T05-TFLG4 (IDX)
                                       TO  T05-DATALST-TFLG4   (IDX)
               MOVE  SPA-GMN-T05-TCNT (IDX)
                                       TO  WRK-NUM5
               MOVE    WRK-NUM5        TO  T05-DATALST-TCNT    (IDX)
               MOVE    SPA-GMN-T05-THOSPCD (IDX)
                                       TO  T05-DATALST-THOSPCD (IDX)
               IF      IDX             =   SPA-NAI-T05-SELNUM
                   MOVE    "T"             TO  T05-DATALST-SELECT (IDX)
               ELSE
                   MOVE    "F"             TO  T05-DATALST-SELECT (IDX)
               END-IF
      *
               IF      SPA-GMN-T05-THOSPCD (IDX)
                                       NOT =   SPACE
                   MOVE    "#FFCC99"       TO  T05-DATALST-BGCOLOR (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-T05-DATALST-MAX
                                       TO  T05-DATALST-COUNT
           MOVE    ZERO                TO  T05-DATALST-ROW
           IF      WRK-T05-DATALST-ROW >   ZERO
               MOVE    WRK-T05-DATALST-ROW TO  T05-DATALST-ROW
           END-IF
      *
           MOVE    "red"               TO  T05-ERRMSG1-STYLE
           MOVE    SPA-GMN-T05-ERRMSG1 TO  T05-ERRMSG1-VALUE
           IF      SPA-GMN-T05-ERRMSG1 NOT =   SPACE
               MOVE    SPA-GMN-T05-ENTRY1      TO  T05-ENTRY1
           END-IF
      *
           MOVE    "red"               TO  T05-ERRMSG2-STYLE
           MOVE    SPA-GMN-T05-ERRMSG2 TO  T05-ERRMSG2-VALUE
           MOVE    "red"               TO  T05-ERRDETAIL-STYLE
           MOVE    SPA-GMN-T05-ERRDETAIL 
                                       TO  T05-ERRDETAIL-VALUE
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
           AND   ( SPA-GMN-T05-CUR     =   ZERO  )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
         EVALUATE    SPA-GMN-T05-CUR
             WHEN    1
                 MOVE    "SELNUM"            TO  MCP-WIDGET
             WHEN    99
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
       50011-INPUT-CUR-SEC             SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SELNUM"
                   MOVE    2               TO  SPA-GMN-T05-CUR
               WHEN    OTHER
                   MOVE    99              TO  SPA-GMN-T05-CUR
           END-EVALUATE
           .
      *
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号入力エラー" 
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "読み込みファイルがありません" 
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "既にＤＢ登録済です"
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "対象のデータを選択してありません"
                                               TO  SPA-ERRMSG
               WHEN    "7777"
                   MOVE    WRK-RECEERR         TO  SPA-ERRMSG
               WHEN    "9999"
                   MOVE    WRK-RECEERR         TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  TERR
           MOVE    SPA-ERRCD            TO  TERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  TERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "T05"                TO  SPA-MOTOPG
           MOVE    "TERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "TERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示処理
      *****************************************************************
       520-TIDSET-SEC              SECTION.
      *
           MOVE    SPACE                TO  TID1
           INITIALIZE                       TID1
           MOVE    SPA-TIDCD            TO  TID1-ID1CODE
           EVALUATE    SPA-TIDCD
                WHEN    "1001"
                    MOVE    "登録処理をします。よろしいですか"
                                               TO  TID1-ID1MSG
                    MOVE    "B12"               TO  MCP-WIDGET
              WHEN    "1002"
                   STRING  "ＤＢ未登録のファイルがあります。"
                                               DELIMITED   BY  SIZE
                           "処理を終了してよろしいですか"
                                               DELIMITED   BY  SIZE
                                               INTO  TID1-ID1MSG
                   END-STRING
                   MOVE    "B01"               TO  MCP-WIDGET
               WHEN    "0101"
                   MOVE    "選択したファイルを削除しますか？"
                                               TO  TID1-ID1MSG
                   MOVE    "B01"               TO  MCP-WIDGET
               WHEN    "6666"
                   MOVE    "処理中です"        TO  TID1-ID1MSG
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "戻る"           TO  TID1-B01-LABEL
           MOVE    "ＯＫ"           TO  TID1-B12-LABEL
      *
           MOVE    "T05"                TO  SPA-MOTOPG
           MOVE    "TID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "TID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-TIDSET-EXT.
           EXIT.
      *****************************************************************
      *    対象項目の文字終了位置を取得
      *****************************************************************
       210-CHAR-END-GET-SEC        SECTION.
      *
           MOVE    WRK-PO-ST           TO  WRK-PO-ED
      *
           PERFORM VARYING   IDYY   FROM    WRK-PO-ST   BY  1
                   UNTIL   ( IDYY           >=  3000 )
                   OR      ( FLG-COMMA     =   1    )
      *        カンマの位置
               IF    ( CSV-X (IDYY)    =   ","              )  OR
                   ( ( IDX             =   SPILIT-CNT ) AND
                     ( CSV-X (IDYY)    =   SPACE      )     )
                   MOVE    IDYY            TO  WRK-PO-ED
                   MOVE    1               TO  FLG-COMMA
               ELSE
      *            カンマが足りない場合の終了位置決定
                   IF      CSV-X (IDYY) NOT =   SPACE
                       MOVE    IDYY         TO  WRK-PO-ED
                   END-IF
               END-IF
      *        デリミタ(")存在するかどうか
               IF      CSV-X (IDYY)         =   """"
                   IF      FLG-DELI1       =   ZERO
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
                 ( PARA-DATA (WRK-MO-ST:WRK-MO-CNT) =   X"0D" )
               MOVE  1                 TO  FLG-NODATA
           END-IF
      *
           IF    ( WRK-MO-CNT          =   1 ) AND
                 ( PARA-DATA (WRK-MO-ST:WRK-MO-CNT) =   X"0A" )
               MOVE  1                 TO  FLG-NODATA
           END-IF
      *
           .
       220-KOUMOKU-KYOTU-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメータファイル読み込み処理
      *****************************************************************
       900-PARA-READ-SEC           SECTION.
      *
          READ    PARA-FILE
              AT  END
                  MOVE    1            TO  FLG-PARA
              NOT AT  END
                  ADD     1            TO  CNT-PARA
                  MOVE    ZERO         TO  FLG-PARA
          END-READ
      *
          .
       900-PARA-READ-EXT.
          EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理ファイル読み込み処理
      *****************************************************************
       900-ONRECE-KANRI-READ-SEC     SECTION.
      *
           MOVE    ONRECE-K-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_onrece_kanri"  TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_onrece_kanri"  TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE   ZERO                 TO  FLG-ONRECE-K
                   MOVE   MCPDATA-REC          TO  ONRECE-K-REC
               ELSE
                   INITIALIZE                      ONRECE-K-REC
                   MOVE   1                    TO  FLG-ONRECE-K
               END-IF
           ELSE
               MOVE    1                   TO  FLG-ONRECE-K
               INITIALIZE                      ONRECE-K-REC
           END-IF
      *
           MOVE    "tbl_onrece_kanri"  TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-ONRECE-KANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細ファイル読込処理
      *****************************************************************
       900-ONRECE-BODY-READ-SEC        SECTION.
      *
           MOVE    ONRECE-B-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_onrece_body"   TO  MCP-TABLE
           MOVE    "key1"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_onrece_body"   TO  MCP-TABLE
               MOVE    "key1"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE  MCPDATA-REC           TO  ONRECE-B-REC
                   MOVE  ZERO                  TO  FLG-ONRECE-B
               ELSE
                   INITIALIZE                      ONRECE-B-REC
                   MOVE  1                     TO  FLG-ONRECE-B
               END-IF
           ELSE
               INITIALIZE                      ONRECE-B-REC
               MOVE  1                     TO  FLG-ONRECE-B
           END-IF
      *
           MOVE    "tbl_onrece_body"   TO  MCP-TABLE
           MOVE    "key1"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-ONRECE-BODY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン返戻管理読み込み処理
      *****************************************************************
       900-HENREI-KANRI-READ-SEC     SECTION.
      *
           MOVE    HENREI-K-REC    TO  MCPDATA-REC
      *
           MOVE    "tbl_henrei_kanri"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_henrei_kanri"
                                       TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE   ZERO             TO  FLG-HENREI-K
                   MOVE   MCPDATA-REC      TO  HENREI-K-REC
               ELSE
                   INITIALIZE                  HENREI-K-REC
                   MOVE   1                TO  FLG-HENREI-K
               END-IF
           ELSE
               MOVE    1               TO  FLG-HENREI-K
               INITIALIZE                  HENREI-K-REC
           END-IF
      * 
           MOVE    "tbl_henrei_kanri"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-HENREI-KANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン返戻明細読込処理
      *****************************************************************
       900-HENREI-BODY-READ-SEC        SECTION.
      *
           MOVE    HENREI-B-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_henrei_body"   TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE  MCPDATA-REC           TO  HENREI-B-REC
                   MOVE  ZERO                  TO  FLG-HENREI-B
               ELSE
                   INITIALIZE                      HENREI-B-REC
                   MOVE  1                     TO  FLG-HENREI-B
               END-IF
           ELSE
               INITIALIZE                      HENREI-B-REC
               MOVE  1                     TO  FLG-HENREI-B
           END-IF
      *
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-HENREI-BODY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
               CALL "coblog" USING "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
