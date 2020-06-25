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
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGZ01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 点数マスタ設定
      *  コンポーネント名  : 点数マスタ検索画面（Ｚ０１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/04/20  クリアボタン追加
      *  01.00.02    MCC-多々納   01/05/23  表示キー追加
      *  01.00.03    MCC-多々納   01/07/03  日付入力チェックサブ追加
      *  01.00.04    MCC-多々納   01/07/04  老人一般コード変換追加
      *  01.01.01    MCC-藤原     01/09/26  JOINの修正      
      *  01.01.02    MCC-多々納   01/11/02  全角入力可能へ  
      *  01.01.03    MCC-藤原     01/12/19  選択番号の入力範囲のチェック追加  
      *  01.02.01    MCC-森脇     02/01/30  リストの追加  
      *  01.02.02    MCC-多々納   02/04/04  診療行為一覧へのパラメタ修正
      *  01.02.03    MCC-多々納   02/05/02  全角チェックサブ追加
      *  01.02.04    MCC-多々納   02/07/18  服用方法画面追加
      *  01.02.05    NACL-多々納  03/05/14  更新前チェック追加
      *                                     選択番号未選択時修正
      *  02.00.00    NACL-多々納  03/06/13  一括入力へ変更
      *  02.00.01    NACL-多々納  05/12/09  登録コードチェック追加
      *                                     MONFUNC対応
      *  03.05.00    NACL-多々納  07/05/07  グループ診療対応
      *  04.01.00    NACL-多々納  07/11/XX  器材商品名対応
      *  04.06.00    NACL-多々納  12/08/16  期間変更警告追加
      *  05.01.00    NACL-多々納  17/02/XX  点数マスタ分離対応
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    点数マスタ設定用共通パラメタ
           COPY    "Z01COMMON-SPA".
      *
      *    画面ＳＰＡエリア
           COPY    "Z01SCR-SPA".
      *
         01  SPA-Z01.
      *    第一画面ＳＰＡエリア
           03  SPA-Z01-AREA.
               05  SPA-GMN-CUR                    PIC 9(02).
      *
      *********05  SPA-Z01GMN-CUR                 PIC X(30).
               05  SPA-Z01GMN-MAX                 PIC 9(04).
               05  SPA-Z01GMN-PAGE                PIC 9(02).
               05  SPA-Z01GMN-AREA.
                   07  SPA-Z01GMN-SRYCD               PIC X(09).
                   07  SPA-Z01GMN-BS1VALUE            PIC X.
                   07  SPA-Z01GMN-BS1LABEL            PIC X(10).
                   07  SPA-Z01GMN-SELTBL.
                       09  SPA-Z01GMN-SELTBLT         OCCURS  30 TIMES.
                           11  SPA-Z01GMN-TRENNUM     PIC X(3).
                           11  SPA-Z01GMN-TYUKOSTYMD  PIC X(9).
                           11  SPA-Z01GMN-TYUKOEDYMD  PIC X(9).
                   07  SPA-Z01GMN-KOUMOKU.
                       09  SPA-Z01GMN-NAME            PIC X(100).
                       09  SPA-Z01GMN-KOUMOKU1.
                           11  SPA-Z01GMN-SELNUM      PIC X(03).
                           11  SPA-Z01GMN-YUKOSTYMD   PIC X(10).
                           11  SPA-Z01GMN-YUKOEDYMD   PIC X(10).
      *H29.1
                   07  SPA-Z01NAI-MASTER-CLASS    PIC 9(01).
      *
               05  SPA-Z01-NAIB-AREA.
                   07  SPA-Z01NAI-KOUMOKU.
                       09  SPA-Z01NAI-SELNUM          PIC 9(03).
                       09  SPA-Z01NAI-YUKOSTYMD       PIC X(08).
                       09  SPA-Z01NAI-YUKOEDYMD       PIC X(08).
                   07  SPA-Z01NAI-SELTBL.
                       09  SPA-Z01NAI-SELTBLT         OCCURS  30 TIMES.
                           11  SPA-Z01NAI-TYUKOSTYMD  PIC X(8).
                           11  SPA-Z01NAI-TYUKOEDYMD  PIC X(8).
                           11  SPA-Z01NAI-NAME        PIC X(100).
                           11  SPA-Z01NAI-TCHOFUKU    PIC 9(01).
                       09  SPA-Z01NAI-COUNT           PIC 9(04).
      *
                   07  SPA-Z01NAI-KEIFLG          PIC 9(01).
                   07  SPA-Z01NAI-SRYCD-HZN       PIC X(09).
      *H24.8
                   07  SPA-Z01NAI-KEIFLG2         PIC 9(01).
      *H29.1
                   07  SPA-Z01NAI-B08-CNT         PIC 9(01).
                   07  SPA-Z01NAI-UPD-OK          PIC 9(01).
                   07  SPA-Z01NAI-UPDCD           PIC 9(01).
      *
      ******* 05  SPA-NAI-SELNO                   PIC 9(03). 
      *
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                           PIC 9(01).
           03  FLG-TENSU                         PIC 9(01).
           03  FLG-INPUTCD                       PIC 9(01).
           03  FLG-SRYCDCHG                      PIC 9(01).
      *
           03  FLG-TOUROKU                       PIC 9(01).
           03  FLG-ERR                           PIC 9(01).
      *
           03  FLG-B08SYORI                      PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR                           PIC 9(06).
           03  CNT-PAGE                          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDXX                              PIC 9(02).
           03  IDXX2                             PIC 9(04).
           03  IDXY                              PIC 9(02).
           03  IDXZ                              PIC 9(02).
           03  IDX                               PIC 9(04).
      *
      *    ワーク領域
       01  WRK-AREA.
           03  WRK-ZZZ                           PIC ZZZ.
           03  WRK-SELNO                         PIC 9(03).
           03  WRK-SNUM-INX                      PIC X(26).
           03  WRK-LNKYMD                        PIC X(10).
           03  WRK-WYMD.
               05  WRK-WGO                       PIC X(01).
               05  WRK-WYY                       PIC 9(02).
               05  WRK-WMM                       PIC 9(02).
               05  WRK-WDD                       PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-YMD             PIC X(08).
           03  WRK-OTYMD           PIC X(08).
      *    老人適用区分
           03  WRK-ROUTEKKBN           PIC 9(01).
      *    パス名
           03  WRK-PATH                PIC X(64).
      *
           03  WRK-MCP-TABLE           PIC X(64).
      *
           03  WRK-SRYCD               PIC X(09).
           03  WRK-Z08-SRYCD           PIC X(09).
      *
           03  WRK-INPUTCD             PIC X(09).
           03  WRK-CD-MCNT             PIC 9(02).
           03  FLG-KANJI               PIC 9(01).
           03  FLG-KENSAKU             PIC 9(01).
      *
           03  WRK-MCP-WIDGET          PIC X(64).
           03  WRK-ZIDMSG              PIC X(80).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    入力コード
        01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *    老人一般コード変換
        01  SRYCDCHG-REC.
            COPY    "CPSRYCDCHG.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *----(01.02.03) LINE ADD START ----------------------------------
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *----(01.02.03) LINE ADD END   ----------------------------------
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
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
           COPY    "ORCA102SCRAREA.INC".
      *    COPY    "Z01.INC".
      *    COPY    "Z02.INC".
      *    COPY    "Z03.INC".
      *    COPY    "Z04.INC".
      *    COPY    "Z05.INC".
      *    COPY    "Z06.INC".
      *    COPY    "Z07.INC".
      *    COPY    "Z08.INC".
      *    COPY    "Z90.INC".
      *    COPY    "Z91.INC".
      *    COPY    "Z97.INC".
      *    COPY    "Z98.INC".
      *    COPY    "Z99.INC".
      *    COPY    "ZERR.INC".
      *    COPY    "ZID1.INC".
      *    COPY    "ZID2.INC".
      *    COPY    "ZID3.INC".
      *    COPY    "Z100.INC".
      *    COPY    "CPAPIMOD.INC".
      *    COPY    "CPAPIMODV2.INC".
      *
       PROCEDURE                                 DIVISION    USING
                                                 MCPAREA
                                                 SPAAREA
                                                 LINKAREA
                                                 SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                              SECTION.
      *
           INITIALIZE                            FLG-AREA
           INITIALIZE                            CNT-AREA
           INITIALIZE                            WRK-AREA
      *
           MOVE    SPA-COMMON                    TO  SPA-AREA
           MOVE    SPA-WORK                      TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE                      TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA                 TO  SPA-Z01
      *
           MOVE    SPACE                         TO  SPA-ERRCD.
           MOVE    ZERO                          TO  FLG-END.
      *
           EVALUATE    MCP-STATUS       ALSO     MCP-EVENT
               WHEN    "LINK"           ALSO     ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI-SEC
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY-SEC
           END-EVALUATE
      *
      *    B08以外はクリア
           IF      FLG-B08SYORI        =   ZERO
               MOVE    ZERO                TO  SPA-Z01NAI-B08-CNT
           END-IF
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END                       =   ZERO
               PERFORM 500-SET-SCREEN-SEC
           END-IF.
      *
           MOVE    SPA-Z01KYOUTU                 TO  SPA-WORK
           MOVE    SPA-AREA                      TO  SPA-COMMON
           MOVE    SPA-Z01                       TO  SPA-Z01SONOTA
           MOVE    SPA-Z01FREE                   TO  SPA-FREE
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                              SECTION.
      *
           INITIALIZE                            FLG-AREA
           INITIALIZE                            CNT-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "ZERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
               MOVE    SPACE               TO  MCP-PUTTYPE
               MOVE    "Z01    "           TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *
           IF  LNK-KYOUTU                   NOT  =  SPACE
               MOVE   LNK-KYOUTU                 TO  SPA-AREA
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "Z98"
      *            検索より
                   PERFORM 320-Z98SPASET-SEC
      *    更新処理
               WHEN    "ZID1"
                   PERFORM 330-ZID1-SET-SEC
      *
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-ZID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-ZIDCD
               WHEN    "0001"
      *            一括削除
                   IF      SPA-ZID1-FLG    =   "OK"
                       PERFORM 4091-USER-DELETE-SEC
                   END-IF
               WHEN    "0002"
      *            ユーザー複写確認
                   IF      SPA-ZID1-FLG    =   "OK"
                       MOVE    "0003"      TO  SPA-ZIDCD
                       PERFORM 520-ZIDSET-SEC
                       MOVE    SPACE           TO  SPA-ZID1-FLG
                       GO  TO  330-ZID1-SET-EXT
                   END-IF
               WHEN    "0003"
      *            ユーザー複写
                   IF      SPA-ZID1-FLG    =   "OK"
                       PERFORM 4092-MASTER-USER-SEC
                   END-IF
           END-EVALUATE
      *
           IF      SPA-ERRCD       NOT =  SPACE
               PERFORM 520-ERRSET-SEC
           END-IF
           MOVE    SPACE           TO  SPA-ZID1-FLG
           MOVE    SPACE           TO  SPA-ZIDCD
           .
       330-ZID1-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
           MOVE    SPACE               TO  WRK-Z08-SRYCD
           IF      SPA-MOTOPG      NOT =   "M02"
               MOVE    SPA-MAE-SRYCD       TO  WRK-SRYCD
               MOVE    SPA-Z08MAE-SRYCD    TO  WRK-Z08-SRYCD
           END-IF
           INITIALIZE                      SPA-Z01KYOUTU
           MOVE    WRK-SRYCD           TO  SPA-MAE-SRYCD
           MOVE    WRK-Z08-SRYCD       TO  SPA-Z08MAE-SRYCD
      *
           INITIALIZE                      SPA-Z01-AREA
      *
           MOVE    "通　常"            TO  SPA-Z01GMN-BS1LABEL
           MOVE    "F"                 TO  SPA-Z01GMN-BS1VALUE
           MOVE    1                   TO  SPA-Z01-SYORIKBN
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為コードセット処理
      *****************************************************************
       320-Z98SPASET-SEC          SECTION.
      *
           IF      SPA-Z98-SRYCD   NOT =   SPACE    
               MOVE    SPA-Z98-SRYCD       TO  Z01-SRYCD
               PERFORM 501-SRYCD-CHK-SEC
           END-IF 
           .
       320-Z98SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI-SEC                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO      MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM  401-BACK-SEC
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 493-CLEAR-SEC
      *        前回コード
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 430-ZEN-SRYCD-SEC
      *---- (01.02.01) LINE ADD START --------------------------------
      *        リスト
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 402-LIST-SEC
      *---- (01.02.01) LINE ADD END ----------------------------------
      *        検索
               WHEN    "CLICKED"       ALSO    "B09"
                   MOVE    SPACE       TO  WRK-INPUTCD
                   PERFORM  409-KENSAKU-SEC
      *        確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM  412-KAKUTEI-SEC
      *        処理切り替えトグルボタン
               WHEN    "CLICKED"       ALSO    "BS1"
                   PERFORM  413-SYORI-SEC
      *        期限切れ
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM  407-Z07SYORI-SEC
      *H29.1
      *        メンテナンスモード設定
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM  408-B08-SYORI-SEC
      *        ユーザー複写・一括削除
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM  409-B10-SYORI-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY-SEC                             SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        診療行為コード
               WHEN    "ACTIVATE"       ALSO    "SRYCD"
                   PERFORM  501-SRYCD-CHK-SEC
      *        選択番号
               WHEN    "ACTIVATE"       ALSO    "SELNUM"
                   PERFORM  502-SELNUM-CHK-SEC
      *        行選択
               WHEN    ANY             ALSO    "TENSU_LIST"
                   PERFORM 41015-LIST1SEL-SEC
      *        一括入力分
               WHEN    OTHER
                   PERFORM 5000-ENTRY-SYORI-SEC
           END-EVALUATE.
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *  　診療行為コードチェック
      *****************************************************************
       501-SRYCD-CHK-SEC                         SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           INITIALIZE                      SPA-Z01NAI-SELTBL
           INITIALIZE                      SPA-Z01NAI-SELTBL
           INITIALIZE                      SPA-Z01GMN-SELTBL
           INITIALIZE                      SPA-Z01GMN-KOUMOKU
           MOVE    ZERO                TO  SPA-Z01NAI-COUNT
           MOVE    ZERO                TO  SPA-Z01NAI-SELNUM
      *
           IF      Z01-SRYCD       NOT =    SPA-Z01GMN-SRYCD
               MOVE    ZERO                TO  SPA-Z01NAI-KEIFLG
               MOVE    ZERO                TO  SPA-Z01NAI-KEIFLG2
           END-IF
      *    全角入力の時、４文字まで
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    Z01-SRYCD           TO  KANACHK-MAE-INPUT
           MOVE    9                   TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-Z01GMN-SRYCD
      **** IF     (KANACHK-RC          =   ZERO )  AND
      *           (KANACHK-SYUBETU     =   2    )
      *        MOVE  Z01-SRYCD(1:8)        TO  SPA-Z01GMN-SRYCD
      *    ELSE
      *        MOVE  Z01-SRYCD             TO  SPA-Z01GMN-SRYCD
      *****END-IF
      *
      *    "//" 入力の時、検索画面へ
           IF      SPA-Z01GMN-SRYCD(1:2)   =  "//"
               MOVE    SPACE           TO  WRK-INPUTCD
               PERFORM 409-KENSAKU-SEC
               GO      TO      501-SRYCD-CHK-EXT
           END-IF
      *
      *    入力コードチェック
           PERFORM 4010-SRYCD-CHK-SEC
           IF      FLG-KENSAKU         =   1
               GO      TO      501-SRYCD-CHK-EXT
           END-IF
      *
           IF      SPA-Z01GMN-SRYCD    NOT =  SPACE
               EVALUATE  SPA-Z01GMN-SRYCD(1:1)
                   WHEN  "0"
                       IF  SPA-Z01GMN-SRYCD(2:2) = "00"
                           MOVE    "0003"    TO  SPA-ERRCD
                       END-IF
                   WHEN  "1"
                   WHEN  "6"
                   WHEN  "7"
                   WHEN  "8"
                       CONTINUE
                   WHEN  OTHER
                       MOVE    "0003"        TO  SPA-ERRCD
               END-EVALUATE
               IF      SPA-ERRCD               =   SPACE
                   MOVE  SPA-Z01GMN-SRYCD          TO  SPA-MAE-SRYCD
      *            点数マスタ該当データ読み出し
                   PERFORM 5011-TENSU-HEN-SEC
               END-IF
           END-IF
      *
           .
       501-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　点数マスタ編集処理
      *****************************************************************
       5011-TENSU-HEN-SEC                        SECTION.
      *
      *    点数マスタ該当データ読み出し
           INITIALIZE                      SPA-Z01GMN-SELTBL
           MOVE    ZERO                TO  SPA-Z01NAI-MASTER-CLASS
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-Z01GMN-SRYCD        TO  TNS-SRYCD
           MOVE    SPA-HOSPNUM             TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC           TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-TENSU-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
           MOVE    ZERO                TO  IDXX
           PERFORM UNTIL    (FLG-TENSU     =   1  )  OR
                            (IDXX          >=  30 )
               ADD     1                   TO  IDXX
               MOVE    IDXX                TO  SPA-Z01NAI-COUNT
               MOVE    TNS-NAME            TO  SPA-Z01GMN-NAME
      *
               MOVE    IDXX                TO  SPA-Z01GMN-TRENNUM
                                                              (IDXX)
               MOVE    TNS-NAME            TO  SPA-Z01NAI-NAME
                                                              (IDXX)
               MOVE    TNS-YUKOSTYMD       TO  SPA-Z01NAI-TYUKOSTYMD
                                                              (IDXX)
               MOVE    TNS-YUKOEDYMD       TO  SPA-Z01NAI-TYUKOEDYMD
                                                               (IDXX)
               MOVE    TNS-YUKOSTYMD       TO  WRK-LNKYMD
               PERFORM  832-YMDCONV21-SEC
               MOVE  WRK-HENYMDG           TO  SPA-Z01GMN-TYUKOSTYMD
                                                                (IDXX)
      *
               MOVE    TNS-YUKOEDYMD       TO  WRK-LNKYMD
               PERFORM  832-YMDCONV21-SEC
               MOVE    WRK-HENYMDG         TO  SPA-Z01GMN-TYUKOEDYMD
                                                                (IDXX)
      *H29.1
      *        ユーザー登録分
               IF      TNS-MASTER-CLASS    =   2
                   MOVE    2               TO  SPA-Z01NAI-MASTER-CLASS
               END-IF
      *
               PERFORM VARYING IDXY    FROM    1   BY  1
                       UNTIL ( IDXY    >=  IDXX )
                   IF    ( SPA-Z01NAI-TYUKOEDYMD (IDXY)
                               <  SPA-Z01NAI-TYUKOSTYMD (IDXX))
                   OR    ( SPA-Z01NAI-TYUKOSTYMD (IDXY)
                               >  SPA-Z01NAI-TYUKOEDYMD (IDXX))
                       CONTINUE
                   ELSE
                       MOVE    1   TO  SPA-Z01NAI-TCHOFUKU (IDXX)
                       MOVE    1   TO  SPA-Z01NAI-TCHOFUKU (IDXY)
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-TENSU-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
      *
      *H29.11
      *    金額テーブルを反映しない為、それぞれのテーブルで検索する。
           IF      SPA-Z01GMN-SRYCD(1:1)   =   "7"
               IF      SPA-Z01NAI-MASTER-CLASS   =   2
                   MOVE    "tbl_tensu_user"    TO  WRK-MCP-TABLE
               ELSE
                   MOVE    "tbl_tensu_master"  TO  WRK-MCP-TABLE
               END-IF
               PERFORM 50119-TENSU-HEN7-SEC
           END-IF
      *
      *
           IF     (SPA-Z01GMN-SRYCD(1:1)   =   "1"
                                           OR  "6"
                                           OR  "7" )
      *        ユーザー対応
               IF     (SPA-Z01NAI-COUNT        >   ZERO )
                 AND  (SPA-Z01NAI-MASTER-CLASS =   ZERO )
                   MOVE    1                   TO  SPA-Z01NAI-UPDCD
               END-IF
           END-IF
      *
      *    カーソル位置の指定
           EVALUATE    SPA-Z01NAI-COUNT
               WHEN    ZERO
                   IF      SPA-Z01NAI-KEIFLG       =   ZERO
      *               コード内容判定
                      PERFORM 50111-SRYCD-CHK-SEC
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
      *                有効開始日へ
                       MOVE    3                   TO  SPA-GMN-CUR
                   ELSE
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
               WHEN    1
      *            １件目を対象とする
                   MOVE    1                   TO  SPA-Z01NAI-SELNUM
                   MOVE    SPA-Z01NAI-SELNUM   TO  WRK-ZZZ
                   MOVE    WRK-ZZZ             TO  SPA-Z01GMN-SELNUM
                   MOVE    SPA-Z01GMN-TYUKOSTYMD(1)
                                               TO  SPA-Z01GMN-YUKOSTYMD
                   MOVE    SPA-Z01NAI-TYUKOSTYMD(1)
                                               TO  SPA-Z01NAI-YUKOSTYMD
                   MOVE    SPA-Z01GMN-TYUKOEDYMD(1)
                                               TO  SPA-Z01GMN-YUKOEDYMD
                   MOVE    SPA-Z01NAI-TYUKOEDYMD(1)
                                               TO  SPA-Z01NAI-YUKOEDYMD
                   MOVE    12                  TO  SPA-GMN-CUR
               WHEN    OTHER
                   MOVE    2                   TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       5011-TENSU-HEN-EXT.
           EXIT.
      *
      *H29.11
      *****************************************************************
      *  コード内容判定処理
      *****************************************************************
       50119-TENSU-HEN7-SEC                       SECTION.
      *
           INITIALIZE                      SPA-Z01GMN-SELTBL
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-Z01GMN-SRYCD        TO  TNS-SRYCD
           MOVE    SPA-HOSPNUM             TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC           TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-TENSU-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
           MOVE    ZERO                TO  IDXX
           PERFORM UNTIL    (FLG-TENSU     =   1  )  OR
                            (IDXX          >=  30 )
               ADD     1                   TO  IDXX
               MOVE    IDXX                TO  SPA-Z01NAI-COUNT
               MOVE    TNS-NAME            TO  SPA-Z01GMN-NAME
      *
               MOVE    IDXX                TO  SPA-Z01GMN-TRENNUM
                                                              (IDXX)
               MOVE    TNS-NAME            TO  SPA-Z01NAI-NAME
                                                              (IDXX)
               MOVE    TNS-YUKOSTYMD       TO  SPA-Z01NAI-TYUKOSTYMD
                                                              (IDXX)
               MOVE    TNS-YUKOEDYMD       TO  SPA-Z01NAI-TYUKOEDYMD
                                                               (IDXX)
               MOVE    TNS-YUKOSTYMD       TO  WRK-LNKYMD
               PERFORM  832-YMDCONV21-SEC
               MOVE  WRK-HENYMDG           TO  SPA-Z01GMN-TYUKOSTYMD
                                                                (IDXX)
      *
               MOVE    TNS-YUKOEDYMD       TO  WRK-LNKYMD
               PERFORM  832-YMDCONV21-SEC
               MOVE    WRK-HENYMDG         TO  SPA-Z01GMN-TYUKOEDYMD
                                                                (IDXX)
      *
               PERFORM VARYING IDXY    FROM    1   BY  1
                       UNTIL ( IDXY    >=  IDXX )
                   IF    ( SPA-Z01NAI-TYUKOEDYMD (IDXY)
                               <  SPA-Z01NAI-TYUKOSTYMD (IDXX))
                   OR    ( SPA-Z01NAI-TYUKOSTYMD (IDXY)
                               >  SPA-Z01NAI-TYUKOEDYMD (IDXX))
                       CONTINUE
                   ELSE
                       MOVE    1   TO  SPA-Z01NAI-TCHOFUKU (IDXX)
                       MOVE    1   TO  SPA-Z01NAI-TCHOFUKU (IDXY)
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-TENSU-NEXT-SEC
           END-PERFORM
      *    MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
           .
       50119-TENSU-HEN7-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *  コード内容判定処理
      *****************************************************************
       50111-SRYCD-CHK-SEC                        SECTION.
      *
           MOVE    1                   TO  SPA-Z01NAI-KEIFLG
      *    ユーザ登録範囲であること
           IF     (SPA-Z01GMN-SRYCD (1:3)  =   "001"
                                           OR  "002"
                                           OR  "059"
      *           器材商品名
                                           OR  "058"
                                           OR  "095"
                                           OR  "096" )  OR
                  (SPA-Z01GMN-SRYCD (1:4)  =   "0082"
                                           OR  "0083"
                                           OR  "0084"
                                           OR  "0085"
                                           OR  "0086")
               CONTINUE
           ELSE
               IF      SPA-Z01GMN-SRYCD (1:1)  =   "1"
                                               OR  "6"
                                               OR  "7"
                                               OR  "8"
                   MOVE    "K001"              TO  SPA-ERRCD
               ELSE
                   MOVE    "K002"              TO  SPA-ERRCD
               END-IF
      *H29.1
      *        メンテナンスモード以外はエラー
               IF      SPA-Z01NAI-UPD-OK   =   ZERO
                   MOVE    "0052"          TO  SPA-ERRCD
                   MOVE    ZERO            TO  SPA-Z01NAI-KEIFLG
               END-IF
           END-IF
      *
           .
       50111-SRYCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *  　入力コードチェック処理
      *****************************************************************
       4010-SRYCD-CHK-SEC                        SECTION.
      *
           MOVE    ZERO               TO  FLG-KENSAKU
      *
           MOVE    ZERO               TO  WRK-CD-MCNT
           MOVE    ZERO               TO  FLG-KANJI
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-Z01GMN-SRYCD    TO  KANACHK-MAE-INPUT
           MOVE    9                   TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-Z01GMN-SRYCD
           IF      KANACHK-RC      NOT =   ZERO
      *        全角半角混在エラー
               MOVE    "0003"              TO  SPA-ERRCD
               GO      TO    4010-SRYCD-CHK-EXT
           ELSE
               MOVE    KANACHK-MAX         TO  WRK-CD-MCNT
      *        全角
               IF      KANACHK-SYUBETU     =   2
                   MOVE    1               TO  FLG-KANJI
               ELSE
                   MOVE    ZERO            TO  FLG-KANJI
               END-IF
           END-IF
      *****PERFORM VARYING IDX    FROM    1   BY  1
      *            UNTIL   IDX        >   9
      *       IF       SPA-Z01GMN-SRYCD(IDX:1) NOT =   SPACE
      *            ADD     1              TO  WRK-CD-MCNT
      *           IF       SPA-Z01GMN-SRYCD(IDX:1) >=  X"A1"
      *                MOVE    1               TO  FLG-KANJI
      *           END-IF
      *       END-IF
      *****END-PERFORM
      *
      *    半角で９文字の時、点数マスタチェックへ
           IF     (WRK-CD-MCNT         =   9    )  AND
                  (FLG-KANJI           =   ZERO )  AND
                  (SPA-Z01GMN-SRYCD    NUMERIC  )
               CONTINUE
           ELSE
      *        入力コード検索
               MOVE    1                   TO  FLG-KENSAKU
               MOVE    SPA-Z01GMN-SRYCD    TO  WRK-INPUTCD
               PERFORM 409-KENSAKU-SEC
           END-IF
      *
           .
       4010-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　選択番号チェック
      *****************************************************************
       502-SELNUM-CHK-SEC                        SECTION.
      *
           MOVE    2                   TO  SPA-GMN-CUR
           INITIALIZE                      SPA-Z01GMN-KOUMOKU1
           INITIALIZE                      SPA-Z01NAI-KOUMOKU
           MOVE    ZERO                TO  SPA-Z01NAI-SELNUM
      *
           MOVE    Z01-SELNUM          TO  SPA-Z01GMN-SELNUM
      *
           IF      SPA-Z01GMN-SELNUM   =   SPACE
      *        未入力で新規以外の場合、最終番号とする
               IF       SPA-Z01NAI-COUNT   >   ZERO
                    MOVE    SPA-Z01NAI-COUNT   TO  SPA-Z01NAI-SELNUM
                    MOVE    SPA-Z01NAI-SELNUM  TO  SPA-Z01GMN-SELNUM
               END-IF
           END-IF
      *    新規とする
           IF      SPA-Z01GMN-SELNUM       =   SPACE
               MOVE    3                   TO  SPA-GMN-CUR
               GO      TO      502-SELNUM-CHK-EXT
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA.
           MOVE    SPA-Z01GMN-SELNUM   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT           =  ZERO  )   OR
                  (SNUM-MINKBN     NOT           =  SPACE )   OR
                  (SNUM-SYOKBN     NOT           =  SPACE )
               MOVE    "0002"                    TO  SPA-ERRCD
               GO      TO      502-SELNUM-CHK-EXT
           ELSE
               MOVE    SNUM-OUTEDT         TO  SPA-Z01NAI-SELNUM
               MOVE    SPA-Z01NAI-SELNUM   TO  WRK-ZZZ
               MOVE    WRK-ZZZ             TO  SPA-Z01GMN-SELNUM
           END-IF.
      *
           IF      (SPA-Z01NAI-SELNUM  <   1   )  OR
                   (SPA-Z01NAI-SELNUM  >   SPA-Z01NAI-COUNT)
  	       MOVE    "0001"              TO  SPA-ERRCD
               GO      TO      502-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    SPA-Z01NAI-NAME  (SPA-Z01NAI-SELNUM)
                                       TO  SPA-Z01GMN-NAME
           MOVE    SPA-Z01GMN-TYUKOSTYMD(SPA-Z01NAI-SELNUM)
                                       TO  SPA-Z01GMN-YUKOSTYMD
           MOVE    SPA-Z01NAI-TYUKOSTYMD(SPA-Z01NAI-SELNUM)
                                       TO  SPA-Z01NAI-YUKOSTYMD
           MOVE    SPA-Z01GMN-TYUKOEDYMD(SPA-Z01NAI-SELNUM)
                                       TO  SPA-Z01GMN-YUKOEDYMD
           MOVE    SPA-Z01NAI-TYUKOEDYMD(SPA-Z01NAI-SELNUM)
                                       TO  SPA-Z01NAI-YUKOEDYMD
      *
           MOVE    12                  TO  SPA-GMN-CUR
           .
       502-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41015-LIST1SEL-SEC       SECTION.
      *    行選択
           MOVE    ZERO                TO  WRK-SELNO
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-Z01NAI-COUNT
               IF      Z01-SELECT (IDX)    =   "T"
                   IF      IDX             =   SPA-Z01NAI-SELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX                 TO  WRK-SELNO
                       MOVE    SPA-Z01NAI-COUNT    TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNO           >   ZERO
               MOVE    WRK-SELNO           TO  SPA-Z01NAI-SELNUM
           END-IF
      *
           IF      SPA-Z01NAI-SELNUM   >   ZERO
               MOVE    SPA-Z01NAI-SELNUM   TO  Z01-SELNUM
               PERFORM 502-SELNUM-CHK-SEC
           END-IF
           .
       41015-LIST1SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    その他一括処理
      *****************************************************************
       5000-ENTRY-SYORI-SEC         SECTION.
      *
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
       5000-ENTRY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
           IF      FLG-TOUROKU         =   1
      *        登録前チェック
               PERFORM 4103-KANREI-SEC
           END-IF
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
      *H24.8
      *    警告区分クリア
           IF     (Z01-YUKOSTYMD   NOT =   SPA-Z01GMN-YUKOSTYMD)
             OR   (Z01-YUKOSTYMD   NOT =   SPA-Z01GMN-YUKOSTYMD)
               MOVE    ZERO            TO  SPA-Z01NAI-KEIFLG2
           END-IF
      *
           MOVE    Z01-YUKOSTYMD       TO  SPA-Z01GMN-YUKOSTYMD
           MOVE    Z01-YUKOEDYMD       TO  SPA-Z01GMN-YUKOEDYMD
      *
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
      *    有効開始年月日
           PERFORM  503-YUKOSTYMD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    有効終了年月日
           PERFORM  504-YUKOEDYMD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　有効開始年月日チェック
      *****************************************************************
       503-YUKOSTYMD-CHK-SEC                     SECTION.
      *
           IF      SPA-Z01GMN-YUKOSTYMD    =   SPACE
               MOVE    "0000000"           TO  SPA-Z01GMN-YUKOSTYMD
           END-IF
      *
           MOVE    SPA-Z01GMN-YUKOSTYMD    TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-LNKYMD          TO  SPA-Z01GMN-YUKOSTYMD
               MOVE    WRK-SYMD            TO  SPA-Z01NAI-YUKOSTYMD
           ELSE
               MOVE    SPACE               TO  SPA-Z01NAI-YUKOSTYMD
               MOVE    "0004"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
               GO      TO      503-YUKOSTYMD-CHK-EXT
           END-IF
      *
           .
       503-YUKOSTYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　有効終了年月日チェック
      *****************************************************************
       504-YUKOEDYMD-CHK-SEC                     SECTION.
      *
           IF      SPA-Z01GMN-YUKOEDYMD    =  SPACE
               IF      (FLG-TOUROKU        =   1   )  OR
                       (WRK-MCP-WIDGET     =   "YUKOEDYMD" )
                   MOVE   "9999999"            TO  SPA-Z01GMN-YUKOEDYMD
               ELSE
                   GO      TO      504-YUKOEDYMD-CHK-EXT
               END-IF
           END-IF.
      *
           MOVE    SPA-Z01GMN-YUKOEDYMD    TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-LNKYMD          TO  SPA-Z01GMN-YUKOEDYMD
               MOVE    WRK-SYMD            TO  SPA-Z01NAI-YUKOEDYMD
           ELSE
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-CUR
               GO      TO      504-YUKOEDYMD-CHK-EXT
           END-IF
      *
           .
       504-YUKOEDYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       401-BACK-SEC                              SECTION.
      *
           MOVE  "M02"                 TO  SPA-SAKIPG.
      *
           MOVE  "Z01"                 TO  SPA-MOTOPG.
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE  "CHANGE"                        TO  MCP-PUTTYPE.
           MOVE  SPA-SAKIPG                      TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                             TO  FLG-END.
       401-BACK-EXT.
           EXIT.
      *---- (01.02.01) LINE ADD START ---------------------------------
      *
      *****************************************************************
      *    リスト　処理
      *****************************************************************
       402-LIST-SEC                              SECTION.
      *
           MOVE  "Z91"                 TO  SPA-SAKIPG.
      *
           MOVE  "Z01"                 TO  SPA-MOTOPG.
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE  "CHANGE"                        TO  MCP-PUTTYPE.
           MOVE  SPA-SAKIPG                      TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                             TO  FLG-END.
       402-LIST-EXT.
           EXIT.
      *---- (01.02.01) LINE ADD END -----------------------------------
      *
      *****************************************************************
      *    項目クリア
      *****************************************************************
       493-CLEAR-SEC                   SECTION.
      *
           PERFORM 310-SPASET-SEC
           .
       491-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    前回コード処理
      *****************************************************************
       430-ZEN-SRYCD-SEC                   SECTION.
      *
           IF      SPA-MAE-SRYCD   NOT =   SPACE
               MOVE    SPACE               TO  SPA-Z01GMN-SRYCD
               MOVE    SPA-MAE-SRYCD       TO  Z01-SRYCD
               PERFORM 501-SRYCD-CHK-SEC
           END-IF
           .
       430-ZEN-SRYCD-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索処理
      *****************************************************************
       409-KENSAKU-SEC             SECTION.
      *
      *    共通ＳＰＡ編集
           INITIALIZE                  SPA-Z98-AREA
           MOVE    "2"                 TO  SPA-Z98-TYPE
      *
           INITIALIZE                  SPA-Z98GMN-AREA
           IF      WRK-INPUTCD         NOT =   SPACE
               MOVE    "3"                 TO  SPA-Z98-GMN-SYORI
               MOVE    WRK-INPUTCD         TO  SPA-Z98-INPUTCD
               MOVE    "1"                 TO  SPA-Z98-TYPE
               IF      FLG-KANJI           =   1
                   MOVE    "J"             TO  SPA-Z98-CDSYUBETU
               ELSE
      ****         IF      WRK-INPUTCD(1:1)    NUMERIC
                   IF      WRK-INPUTCD(1:WRK-CD-MCNT)    NUMERIC
                       EVALUATE    WRK-CD-MCNT
                           WHEN    3
                           WHEN    6
                               MOVE    "6"     TO  SPA-Z98-CDSYUBETU
                           WHEN    4
                               MOVE    "4"     TO  SPA-Z98-CDSYUBETU
                           WHEN    5
                               MOVE    "5"     TO  SPA-Z98-CDSYUBETU
                       END-EVALUATE
                   ELSE
                       MOVE    "A"             TO  SPA-Z98-CDSYUBETU
                   END-IF
               END-IF
      *        画面編集
               PERFORM 41011-Z98-SET-SEC
      *        点数マスタ編集で、対象データなしの時、診療行為で検索する
               IF     (SPA-Z98-GMN-MAX     =   ZERO )  AND
                      (SPA-Z98-CDSYUBETU   =   "J"  )
                   INITIALIZE                  SPA-Z98GMN-AREA
                   MOVE    "8"                 TO  SPA-Z98-GMN-SYORI
      *            画面編集
                   PERFORM 41011-Z98-SET-SEC
               END-IF
           ELSE
      *        画面編集
               PERFORM 41011-Z98-SET-SEC
           END-IF
      *
      *
           MOVE    "Z01"               TO  SPA-MOTOPG
           MOVE    "Z98"               TO  SPA-SAKIPG
           MOVE    "Z01"               TO  SPA-Z98-MOTOPG
      *
           IF      SPA-Z98-GMN-MAX     =   ZERO
               MOVE    "INPUT"             TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM1"           TO  MCP-WIDGET
           END-IF
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "Z98    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       409-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索画面編集処理
      *****************************************************************
       41011-Z98-SET-SEC                           SECTION.
      *
      *
           MOVE    SPA-Z01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-Z01             TO  SPA-Z01SONOTA
           MOVE    SPA-Z01FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGZ98"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
      *
           MOVE    SPA-COMMON                    TO  SPA-AREA
           MOVE    SPA-WORK                      TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE                      TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA                 TO  SPA-Z01
      *
           .
       41011-Z98-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    期限切れ画面遷移処理
      *****************************************************************
       407-Z07SYORI-SEC                SECTION.
      *
           MOVE    "Z07"               TO  SPA-SAKIPG
      *
           MOVE    "Z01"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "Z07"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       407-Z07SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録前チェック処理
      *****************************************************************
       4103-KANREI-SEC              SECTION.
      *
      *
           IF      SPA-Z01GMN-SRYCD    =   SPACE
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      4103-KANREI-EXT
           END-IF
      *    すべて数字であること
           IF      SPA-Z01GMN-SRYCD    NOT NUMERIC
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      4103-KANREI-EXT
           END-IF
      *
           IF    ( SPA-Z01GMN-YUKOSTYMD    =  SPACE ) OR
                 ( SPA-Z01GMN-YUKOEDYMD    =  SPACE )
               MOVE    "0007"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
               GO      TO      4103-KANREI-EXT
           END-IF
      *
           IF      SPA-Z01NAI-YUKOSTYMD    >=  SPA-Z01NAI-YUKOEDYMD
               MOVE    "0011"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
               GO      TO      4103-KANREI-EXT
           END-IF
      *
           IF      SPA-Z01-SYORIKBN    =   1
      *        新規分で対象がある時、更新とする
               IF      SPA-Z01NAI-COUNT    NOT =   ZERO
                   MOVE    2               TO  SPA-Z01-SYORIKBN
               END-IF
           END-IF
      *
      *    新規のコードエラーチェック
           IF    ( SPA-Z01NAI-COUNT    =   ZERO )
             AND ( SPA-Z01NAI-KEIFLG   =   ZERO )
      *       コード内容判定
               PERFORM 50111-SRYCD-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    1                   TO  SPA-GMN-CUR
                   GO      TO      4103-KANREI-EXT
               END-IF
           END-IF
      *
           EVALUATE    SPA-Z01-SYORIKBN
      *       改正処理
               WHEN    3
      *            新規でないこと
                   IF      SPA-Z01NAI-COUNT    =   ZERO
                       MOVE    1                   TO  SPA-GMN-CUR
                       MOVE    "0006"              TO  SPA-ERRCD
                       GO      TO      4103-KANREI-EXT
                   END-IF
                   IF      SPA-Z01NAI-YUKOEDYMD    NOT =   99999999
                       MOVE    4                   TO  SPA-GMN-CUR
                       MOVE    "0009"              TO  SPA-ERRCD
                       GO      TO      4103-KANREI-EXT
                   END-IF
      *            履歴選択があること
                   IF     (SPA-Z01NAI-SELNUM       =   ZERO            )
                   OR     (SPA-Z01NAI-SELNUM       >   SPA-Z01NAI-COUNT)
                       MOVE    2                   TO  SPA-GMN-CUR
                       MOVE    "0012"              TO  SPA-ERRCD
                       GO      TO      4103-KANREI-EXT
                   END-IF
      *H24.8
      *            ユーザ登録分であること
                   IF     (SPA-Z01GMN-SRYCD (1:1)  =   "1"
                                                   OR  "6"
                                                   OR  "7"
                                                   OR  "8" )
                      OR  (SPA-Z01GMN-SRYCD (1:3)  =   "099")
      *!!
      *                更新不可
                       IF      (SPA-Z01NAI-MASTER-CLASS =  2)
                          OR   (SPA-Z01NAI-UPD-OK       =  1)
                           CONTINUE
                       ELSE
                           MOVE    2               TO  SPA-GMN-CUR
                           MOVE    "0050"          TO  SPA-ERRCD
                           GO      TO      4103-KANREI-EXT
                       END-IF
                       IF       SPA-Z01NAI-KEIFLG2  =   ZERO
                           MOVE    1           TO  SPA-Z01NAI-KEIFLG2
                           MOVE    "K003"          TO  SPA-ERRCD
                           MOVE    1               TO  SPA-GMN-CUR
                           GO      TO      4103-KANREI-EXT
                       END-IF
                   END-IF
      *
      *        通常修正
               WHEN    2
      *            通常修正有効期限範囲チェック
                   PERFORM 41031-YUKOHANI-CHK-SEC
           END-EVALUATE
      *
           .
       4103-KANREI-EXT.
           EXIT.
      *
      *****************************************************************
      *    通常修正有効期限範囲チェック処理
      *****************************************************************
       41031-YUKOHANI-CHK-SEC                           SECTION.
      *
           IF    ( SPA-Z01NAI-SELNUM  >=  1 )
            AND  ( SPA-Z01NAI-SELNUM  <=  SPA-Z01NAI-COUNT )
            AND  ( SPA-Z01NAI-TCHOFUKU (SPA-Z01NAI-SELNUM)
                                       =  1 )
                   CONTINUE
           ELSE
      *
      *        前チェック
               IF      SPA-Z01NAI-SELNUM   >   1
                   MOVE    SPA-Z01NAI-TYUKOEDYMD(SPA-Z01NAI-SELNUM - 1)
                                           TO  WRK-YMD
      *            対象日＋１日
                   PERFORM 800-DAYSET-SEC
      *********IF      SPA-Z01NAI-YUKOSTYMD  NOT =   WRK-OTYMD
      *            重複しないこと
                   IF      SPA-Z01NAI-YUKOSTYMD    <     WRK-OTYMD
                       MOVE    3                   TO  SPA-GMN-CUR
                       MOVE    "0010"              TO  SPA-ERRCD
                       GO      TO      41031-YUKOHANI-CHK-EXT
                   END-IF
               END-IF
      *        後チェック
               IF      SPA-Z01NAI-SELNUM   <   SPA-Z01NAI-COUNT
                   MOVE    SPA-Z01NAI-YUKOEDYMD
                                           TO  WRK-YMD
      *            対象日＋１日
                   PERFORM 800-DAYSET-SEC
      *******  IF      SPA-Z01NAI-TYUKOSTYMD(SPA-Z01NAI-SELNUM + 1)
      *****                                NOT =   WRK-OTYMD
      *            重複しないこと
                   IF      SPA-Z01NAI-TYUKOSTYMD(SPA-Z01NAI-SELNUM + 1)
                                               <   WRK-OTYMD
                       MOVE    4                   TO  SPA-GMN-CUR
                       MOVE    "0010"              TO  SPA-ERRCD
                       GO      TO      41031-YUKOHANI-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *H24.8
      *    ユーザ登録分以外の期間変更
           IF    ((SPA-Z01GMN-SRYCD (1:1)  =   "1"
                                           OR  "6"
                                           OR  "7"
                                           OR  "8" )  OR
                  (SPA-Z01GMN-SRYCD (1:3)  =   "099" ))
           AND   (SPA-Z01NAI-KEIFLG2   =   ZERO   )
           AND   (SPA-Z01NAI-SELNUM    >=  1      )
               IF     (SPA-Z01NAI-YUKOSTYMD    =
                               SPA-Z01NAI-TYUKOSTYMD(SPA-Z01NAI-SELNUM))
               AND    (SPA-Z01NAI-YUKOEDYMD    =
                               SPA-Z01NAI-TYUKOEDYMD(SPA-Z01NAI-SELNUM))
                   CONTINUE
               ELSE
                   MOVE    1               TO  SPA-Z01NAI-KEIFLG2
                   MOVE    "K004"          TO  SPA-ERRCD
                   MOVE    2               TO  SPA-GMN-CUR
      *!!
      *            更新不可
                   IF      (SPA-Z01NAI-MASTER-CLASS =  2)
                      OR   (SPA-Z01NAI-UPD-OK       =  1)
                       CONTINUE
                   ELSE
                       MOVE    "0051"          TO  SPA-ERRCD
                       MOVE    ZERO            TO  SPA-Z01NAI-KEIFLG2
                   END-IF
               END-IF
           END-IF
      *
      *
           .
       41031-YUKOHANI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定処理
      *****************************************************************
       412-KAKUTEI-SEC                           SECTION.
      *
      *    入力個所のセット
           MOVE    SPACE               TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
           MOVE    1                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      412-KAKUTEI-EXT
           END-IF
      *
      *    入力コードマスタ読み込み 
           INITIALIZE                  SPA-Z99-AREA
      *
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    SPA-Z01GMN-SRYCD    TO  ICD-SRYCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM                     920-INPUTCD-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           PERFORM VARYING     IDXX    FROM    1  BY  1
                   UNTIL      (IDXX    >   10  )  OR
                              (FLG-INPUTCD     =   1)
               IF      IDXX                =   1
                   MOVE    "◎"        TO  SPA-Z99-HYOUJI (IDXX)
               END-IF
               MOVE    ICD-INPUTCD         TO  SPA-Z99-INPUTCD(IDXX)
               MOVE    ICD-DSPNAME         TO  SPA-Z99-DSPNAME(IDXX)
               MOVE    ICD-CDSYU           TO  SPA-Z99-CDSYU(IDXX)
      *
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM                     920-INPUTCD-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
      *    老人一般コード変換マスタ読み込み 
           INITIALIZE                  SPA-Z90-AREA
           INITIALIZE                  SRYCDCHG-REC
      *    点数マスタ検索
           INITIALIZE                  TNS-TENSU-REC
           MOVE    SPA-Z01GMN-SRYCD    TO  TNS-SRYCD
           PERFORM 900-TENSU-READ-SEC
           MOVE    TNS-ROUTEKKBN       TO  WRK-ROUTEKKBN
      *
      *    老人適用区分チェック
           EVALUATE    TNS-ROUTEKKBN
               WHEN    0
                   CONTINUE
               WHEN    1
      *            一般
                   MOVE    SPA-Z01GMN-SRYCD    TO  CHG-IPNSRYCD
                   MOVE    "key2"              TO  WRK-PATH
                   PERFORM 4121-SRYCDCHG-SET-SEC
               WHEN    2
      *            老人
                   MOVE    SPA-Z01GMN-SRYCD    TO  CHG-RJNSRYCD
                   MOVE    "key3"              TO  WRK-PATH
                   PERFORM 4121-SRYCDCHG-SET-SEC
           END-EVALUATE
      *
      *    共通見出し用編集
           INITIALIZE                  SPA-Z01-KYOUTU-AREA
           MOVE    SPA-Z01GMN-SRYCD              TO  SPA-Z01-SRYCD
           MOVE    SPA-Z01GMN-NAME               TO  SPA-Z01-NAMEH
           MOVE    SPA-Z01GMN-YUKOSTYMD          TO  SPA-Z01-YUKOSTYMDH
           MOVE    SPA-Z01GMN-YUKOEDYMD          TO  SPA-Z01-YUKOEDYMDH
           MOVE    SPA-Z01NAI-YUKOSTYMD          TO  SPA-Z01-YUKOSTYMD
           MOVE    SPA-Z01NAI-YUKOEDYMD          TO  SPA-Z01-YUKOEDYMD
      *    Ｚ０１画面保存
           INITIALIZE                   SPA-Z01-GMNKYOUTU
           MOVE    SPA-Z01NAI-SELNUM   TO  SPA-Z01-SELNUM
           MOVE    ZERO                TO  SPA-Z01-MAX
           PERFORM VARYING IDXX        FROM    1   BY  1
                   UNTIL  (IDXX        >   SPA-Z01NAI-COUNT  )   OR
                          (SPA-Z01NAI-TYUKOSTYMD(IDXX) =   SPACE   AND
                           SPA-Z01NAI-TYUKOEDYMD(IDXX) =   SPACE )
               MOVE    SPA-Z01NAI-TYUKOSTYMD(IDXX)
                                           TO  SPA-Z01-TYUKOSTYMD(IDXX)
               MOVE    SPA-Z01NAI-TYUKOEDYMD(IDXX)
                                           TO  SPA-Z01-TYUKOEDYMD(IDXX)
               MOVE    IDXX                TO  SPA-Z01-MAX
           END-PERFORM
      *
           MOVE    "Z01"                         TO  SPA-MOTOPG
      *
           MOVE   "CHANGE"                       TO  MCP-PUTTYPE.
           EVALUATE  SPA-Z01GMN-SRYCD(1:1)
              WHEN   "0"
      ************IF      SPA-Z01GMN-SRYCD (1:3)     =   "099"
      *               MOVE   "Z03"                   TO  SPA-SAKIPG
      *           ELSE 
      *               MOVE   "Z02"                   TO  SPA-SAKIPG
      *********** END-IF
                  EVALUATE    SPA-Z01GMN-SRYCD (1:3)
                      WHEN    "099"
                          MOVE   "Z03"                   TO  SPA-SAKIPG
      *
      *H30.4
      *                   処方せん分割用
                          IF      SPA-Z01GMN-SRYCD(1:7)  =   "0992081"
      *                       コメントへ
                              MOVE   "Z02"               TO  SPA-SAKIPG
                          END-IF
      *               特定器材
                      WHEN    "059"
                          MOVE   "Z05"                   TO  SPA-SAKIPG
      *               特定器材商品名
                      WHEN    "058"
                          MOVE   "Z08"                   TO  SPA-SAKIPG
      *               用法
                      WHEN    "001"
                          MOVE   "Z06"                   TO  SPA-SAKIPG
                      WHEN    OTHER
                          MOVE   "Z02"                   TO  SPA-SAKIPG
                  END-EVALUATE
              WHEN   "1"
                  MOVE   "Z03"                   TO  SPA-SAKIPG
              WHEN   "6"
                  MOVE   "Z04"                   TO  SPA-SAKIPG
              WHEN   "7"
                  MOVE   "Z05"                   TO  SPA-SAKIPG
              WHEN   "8"
                  MOVE   "Z02"                   TO  SPA-SAKIPG
              WHEN  OTHER
                   MOVE    "0003"                TO  SPA-ERRCD
                   GO      TO      412-KAKUTEI-EXT
           END-EVALUATE
      *H29.1
      *    ユーザー指定
           MOVE    ZERO            TO  SPA-MASTER-CLASS
           IF      SPA-Z01NAI-MASTER-CLASS =   2
               MOVE    2               TO  SPA-MASTER-CLASS
           END-IF
      *    メンテナンス
           MOVE    ZERO            TO  SPA-MENTE-OK
           IF    (SPA-Z01GMN-SRYCD(1:1)    =   "1"
                                           OR  "6"
                                           OR  "7"
                                           OR  "8" )
             OR  (SPA-Z01GMN-SRYCD(1:3)    =   "099")
               IF     (SPA-Z01NAI-UPD-OK       =   1 )
                 OR   (SPA-MASTER-CLASS        =   2 )
                   MOVE    1               TO  SPA-MENTE-OK
               ELSE
                   MOVE    ZERO            TO  SPA-MENTE-OK
               END-IF
           ELSE
               MOVE    1               TO  SPA-MENTE-OK
               MOVE    2               TO  SPA-MASTER-CLASS
           END-IF
      *
           MOVE   SPA-SAKIPG                     TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
           MOVE  1                               TO  FLG-END
      *
           .
       412-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *   老人一般コード振替コードセット
      *****************************************************************
       4121-SRYCDCHG-SET-SEC           SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
           MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
           MOVE    "tbl_srycdchg"      TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srycdchg"      TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM                     930-SRYCDCHG-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYCDCHG
           END-IF
      *
           MOVE    ZERO                TO  IDXX
           MOVE    ZERO                TO  IDXX2
           PERFORM 
                   UNTIL      (IDXX            >=  5 )  OR
                              (FLG-SRYCDCHG    =   1 )
               EVALUATE    WRK-ROUTEKKBN
                   WHEN    1
      *                点数マスタ検索
                       INITIALIZE                  TNS-TENSU-REC
                       MOVE    CHG-RJNSRYCD        TO  TNS-SRYCD
                       PERFORM 900-TENSU-READ-SEC
                       IF      FLG-TENSU           =   ZERO
                           ADD     1               TO  IDXX
                           MOVE    TNS-NAME        TO
                                             SPA-Z90-SRYNAME  (IDXX) 
                           MOVE    CHG-RJNSRYCD    TO  
                                             SPA-Z90-SRYCDCHG (IDXX)
                       END-IF
                   WHEN    2
      *                点数マスタ検索
                       INITIALIZE                  TNS-TENSU-REC
                       MOVE    CHG-IPNSRYCD        TO  TNS-SRYCD
                       PERFORM 900-TENSU-READ-SEC
                       IF      FLG-TENSU           =   ZERO
                           ADD     1               TO  IDXX
                           MOVE    TNS-NAME        TO  SPA-Z90-SRYNAME
                                                               (IDXX)
                           MOVE    CHG-IPNSRYCD    TO  SPA-Z90-SRYCDCHG
                                                               (IDXX)
                       END-IF
               END-EVALUATE
      *
               ADD     1                   TO  IDXX2
               MOVE    CHG-IPNSRYCD        TO  SPA-Z90-IPNSRYCD(IDXX2)
               MOVE    CHG-RJNSRYCD        TO  SPA-Z90-RJNSRYCD(IDXX2)
      *
               MOVE    "tbl_srycdchg"      TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM                     930-SRYCDCHG-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_srycdchg"      TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
           .
       4121-SRYCDCHG-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   処理切り替えトグルボタン（通常、改正）
      *****************************************************************
       413-SYORI-SEC                             SECTION.
      *
           EVALUATE  SPA-Z01-SYORIKBN
             WHEN    3
                 MOVE  "通　常"              TO  SPA-Z01GMN-BS1LABEL
                 MOVE  "F"                   TO  SPA-Z01GMN-BS1VALUE
                 MOVE  1                     TO  SPA-Z01-SYORIKBN
             WHEN   1
                 MOVE  "改　正"              TO  SPA-Z01GMN-BS1LABEL
                 MOVE  "T"                   TO  SPA-Z01GMN-BS1VALUE
                 MOVE  3                     TO  SPA-Z01-SYORIKBN
           END-EVALUATE
           .
       413-SYORI-EXT.
           EXIT.
      *
      *H29.1
      *****************************************************************
      *    メンテナンスモード設定処理
      *****************************************************************
       408-B08-SYORI-SEC                SECTION.
      *
           ADD     1                   TO  SPA-Z01NAI-B08-CNT
           MOVE    1                   TO  FLG-B08SYORI
           IF      SPA-Z01NAI-B08-CNT  >=  3
               MOVE    1               TO  SPA-Z01NAI-UPD-OK
           END-IF
           .
       408-B08-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ユーザー複写・一括削除処理
      *****************************************************************
       409-B10-SYORI-SEC                SECTION.
      *
           MOVE    ZERO                TO  SPA-Z01NAI-UPD-OK
           IF      SPA-Z01NAI-COUNT    =   ZERO
               GO      TO      409-B10-SYORI-EXT
           END-IF
      *
           IF      SPA-Z01GMN-SRYCD(1:1)   =   "1"
                                           OR  "6"
                                           OR  "7"
               CONTINUE
           ELSE
               MOVE    "0053"              TO  SPA-ERRCD
               GO      TO      409-B10-SYORI-EXT
           END-IF
      *
           IF      SPA-Z01NAI-MASTER-CLASS =   2
      *        ユーザー指定　一括削除
               MOVE    "0001"          TO  SPA-ZIDCD
           ELSE
      *        ユーザー指定へ変更
               MOVE    "0002"          TO  SPA-ZIDCD
           END-IF
           .
       409-B10-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザー指定一括削除処理
      *****************************************************************
       4091-USER-DELETE-SEC             SECTION.
      *
      *    点数マスタ削除
           MOVE    SPACE               TO  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    SPA-Z01GMN-SRYCD    TO  TNS-SRYCD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "del01"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "Z01 TENSU USER DEL ERR!!"   MCP-RC
                       ",KEY:"   TNS-KEY
               MOVE    "9001"                TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 310-SPASET-SEC
           END-IF
           .
       4091-USER-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザー指定へ変更処理
      *****************************************************************
       4092-MASTER-USER-SEC              SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-Z01GMN-SRYCD    TO  TNS-SRYCD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu_master"  TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu_master"  TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-TENSU-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
      *
           PERFORM UNTIL  (FLG-TENSU       =   1 )
                       OR (SPA-ERRCD   NOT =   SPACE )
      *        更新日付設定
               MOVE    SPA-OPID            TO  TNS-OPID
               MOVE    SMCNDATE-YMD        TO  TNS-UPYMD
               MOVE    SMCNDATE-HMS        TO  TNS-UPHMS
      *
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_tensu_user"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "9003"              TO  SPA-ERRCD
                   DISPLAY "Z01 TENSU USER INS ERR:" MCP-RC
                           ",KEY:" TNS-KEY
               END-IF
               MOVE    "tbl_tensu_master"  TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-TENSU-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_tensu_master"  TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-Z01GMN-SRYCD    TO  SPA-MAE-SRYCD
               PERFORM 430-ZEN-SRYCD-SEC
           END-IF
           .
       4092-MASTER-USER-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN-SEC                        SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *    エラー処理
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 520-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-ZIDCD       NOT =   SPACE
               PERFORM 520-ZIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE  SPACE                           TO  LINKAREA
      *
           MOVE  "CURRENT"                       TO  MCP-PUTTYPE.
           MOVE  "Z01    "                       TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面セット処理
      *****************************************************************
       500-GMNHEN-SEC                            SECTION.
      *
      *     画面ワークエリアにセット
            INITIALIZE                           Z01.
      *
            MOVE    SPA-Z01GMN-BS1LABEL    TO  Z01-BS1LABEL.
            MOVE    SPA-Z01GMN-BS1VALUE    TO  Z01-BS1VALUE.
      *
            MOVE    SPA-Z01GMN-SRYCD       TO  Z01-SRYCD.
            MOVE    SPA-Z01GMN-NAME        TO  Z01-NAME.
            MOVE    SPA-Z01GMN-SELNUM      TO  Z01-SELNUM.
            MOVE    SPA-Z01GMN-YUKOSTYMD   TO  Z01-YUKOSTYMD.
            MOVE    SPA-Z01GMN-YUKOEDYMD   TO  Z01-YUKOEDYMD
      *
            PERFORM VARYING IDXX   FROM    1   BY  1
                    UNTIL   IDXX       >   SPA-Z01NAI-COUNT
               MOVE  SPA-Z01GMN-TRENNUM(IDXX)      TO  Z01-TRENNUM 
                                                              (IDXX)
               MOVE  SPA-Z01GMN-TYUKOSTYMD(IDXX)   TO  Z01-TYUKOSTYMD
                                                              (IDXX)
               MOVE  SPA-Z01GMN-TYUKOEDYMD(IDXX)   TO  Z01-TYUKOEDYMD
                                                              (IDXX)
               IF      IDXX            =   SPA-Z01NAI-SELNUM
                   MOVE    "T"             TO  Z01-SELECT (IDXX)
               ELSE
                   MOVE    "F"             TO  Z01-SELECT (IDXX)
               END-IF
            END-PERFORM.
            MOVE  SPA-Z01NAI-COUNT                  TO  Z01-COUNT.
      *H29.1
      *    ユーザー
           IF      SPA-Z01NAI-MASTER-CLASS =   2
               MOVE    "【ユーザー管理】"  TO  Z01-MIDASHI-DATA
               MOVE    "red"               TO  Z01-MIDASHI-STYLE
           END-IF
      *
           MOVE    WIDGET-INSENSITIVE      TO  Z01-B10-STATE
      *
           IF      SPA-Z01GMN-SRYCD(1:1)   =   "1"
                                           OR  "6"
                                           OR  "7"
           IF      SPA-Z01NAI-MASTER-CLASS =   2
               MOVE    "一括削除"          TO  Z01-B10-LABEL
               MOVE    WIDGET-NORMAL       TO  Z01-B10-STATE
           ELSE
               IF      SPA-Z01NAI-UPDCD    =   1
                   MOVE    "ユーザー"          TO  Z01-B10-LABEL
                   MOVE    WIDGET-NORMAL       TO  Z01-B10-STATE
               END-IF
           END-IF
      *
           END-IF
      *
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
           IF      SPA-GMN-CUR         =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN   1
                   MOVE    "SRYCD"          TO  MCP-WIDGET
               WHEN   2
                   MOVE    "SELNUM"         TO  MCP-WIDGET
               WHEN   3
                   MOVE    "YUKOSTYMD"      TO  MCP-WIDGET
               WHEN   4
                   MOVE    "YUKOEDYMD"      TO  MCP-WIDGET
               WHEN   12
                   MOVE    "B12"            TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル指定処理
      *****************************************************************
       50011-CUR-SET-SEC               SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "YUKOSTYMD"
                   MOVE    4                   TO  SPA-GMN-CUR
               WHEN    "YUKOEDYMD"
                   MOVE    12                  TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       520-ERRSET-SEC                            SECTION.
           MOVE    SPACE                         TO  SPA-ERRMSG.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力された番号は選択できません"
                                                 TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "数字のみ入力可能です"
                                                 TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "診療コード入力規則にあっていません"
                                                 TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "有効開始日に誤りがあります"
                                                 TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "有効終了日に誤りがあります"
                                                 TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "有効開始日＜有効終了日で入力して下さい。"
                                                 TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "新規の為、改正処理は行えません"
                                                 TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "有効年月日を入力してください"
                                                 TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "有効年月日が重複しています"
                                                 TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "改正の終了年月日はＡＬＬ９のみ入力できます"
                                                 TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "有効年月日の範囲が違います"
                                                 TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "選択番号を入力して下さい。"
                                                 TO  SPA-ERRMSG
               WHEN    "K001"
                   STRING    "警告！厚生労働省から提供される"
                                                   DELIMITED BY  SIZE
                             "コードの範囲です。"
                                                   DELIMITED BY  SIZE
                             "新規登録はしないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K002"
                   STRING    "警告！ユーザ登録できる範囲のコード"
                                                   DELIMITED BY  SIZE
                             "ではありません。"
                                                   DELIMITED BY  SIZE
                             "新規登録はしないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
      *H24.8
               WHEN    "K003"
                   STRING    "警告！"
                             "厚生労働省から提供される範囲のコードです"
                             "。改正処理はしないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K004"
                   STRING    "警告！"
                             "厚生労働省から提供される範囲のコードです"
                             "。有効年月日の変更はしないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
      *H29.1
               WHEN    "0050"
                   STRING    "厚生労働省から提供される範囲のコードです"
                             "。改正処理はできません。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0051"
                   STRING    "厚生労働省から提供される範囲のコードです"
                             "。有効年月日の変更はできません。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0052"
                   STRING    "厚生労働省から提供される"
                             "コードの範囲です。"
                             "新規登録はしないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0053"
                   STRING    "一括削除できません。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
           END-EVALUATE.
      *
           MOVE    SPACE                         TO  ZERR
           MOVE    SPA-ERRCD                     TO  ZERR-ERRCODE
           MOVE    SPA-ERRMSG                    TO  ZERR-ERRMSG
      *
           MOVE    "Z01"                         TO  SPA-MOTOPG
           MOVE    "ZERR"                        TO  SPA-SAKIPG
      *
           MOVE    "NEW"                         TO  MCP-PUTTYPE
           MOVE    "ZERR"                        TO  MCP-WINDOW
           MOVE    "B01"                         TO  MCP-WIDGET
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                             TO  FLG-END
           .
      *
       520-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-ZIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-ZIDMSG
      *
           EVALUATE    SPA-ZIDCD
               WHEN    "0001"
                   MOVE    "一括削除します。よろしいですか？"
                                                       TO  WRK-ZIDMSG
               WHEN    "0002"
                   STRING    "マスターをユーザーへ複写します。"
                             "よろしいですか？"
                                                   DELIMITED BY  SIZE
                                               INTO    WRK-ZIDMSG
                   END-STRING
               WHEN    "0003"
                   STRING    "ユーザー管理となりますので、"
                             "マスター更新の対象外となります。"
                             "よろしいですか？"
                                                   DELIMITED BY  SIZE
                                               INTO    WRK-ZIDMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE                TO  ZID1
           INITIALIZE                       ZID1
           MOVE    SPA-ZIDCD            TO  ZID1-ID1CODE
           MOVE    WRK-ZIDMSG           TO  ZID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
           EVALUATE    SPA-ZIDCD
               WHEN    "0002"
               WHEN    "0003"
                   MOVE    "B01"                TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "Z01"                TO  SPA-MOTOPG
           MOVE    "ZID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "ZID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-ZIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *     対象日＋１日計算処理
      *****************************************************************
       800-DAYSET-SEC                  SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           EVALUATE    WRK-YMD
               WHEN    ZERO
                   MOVE    ZERO            TO  WRK-OTYMD
               WHEN    "99999999"
                   MOVE    ALL "9"         TO  WRK-OTYMD
               WHEN    OTHER
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE    1                   TO  LNK-DAY6-ZOGEN
                   CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                  MOVE    LNK-DAY6-KEISAN      TO  WRK-OTYMD
           END-EVALUATE
      *
           .
       800-DAYSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　処理
      *****************************************************************
      *
      *****************************************************************
      *    点数マスタ検索処理（診療行為コード）
      *****************************************************************
       900-TENSU-READ-SEC               SECTION.
      *
           MOVE    SPA-Z01NAI-YUKOSTYMD    TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01NAI-YUKOSTYMD    TO  TNS-YUKOEDYMD
           IF      SPA-Z01NAI-YUKOSTYMD    =   SPACE
               MOVE   ZERO                 TO  TNS-YUKOSTYMD
               MOVE   "99999999"           TO  TNS-YUKOEDYMD
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
           .
      *
       900-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       910-TENSU-NEXT-SEC     SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               MOVE    ZERO                TO  FLG-TENSU
           ELSE
               INITIALIZE                      TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           .
       910-TENSU-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       920-INPUTCD-READ-SEC     SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTCD-REC
               MOVE    ZERO                TO  FLG-INPUTCD
           ELSE
               INITIALIZE                      INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           .
       920-INPUTCD-READ-EXT.
           EXIT.
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       930-SRYCDCHG-READ-SEC     SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYCDCHG-REC
               MOVE    ZERO                TO  FLG-SRYCDCHG
           ELSE
               INITIALIZE                      SRYCDCHG-REC
               MOVE    1                   TO  FLG-SRYCDCHG
           END-IF
      *
           .
       930-SRYCDCHG-READ-EXT.
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
      *****************************************************************
      *   ＤＢクローズ処理
      *****************************************************************
       990-DBLOSEC-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBLOSEC-EXT.
           EXIT.
      *****************************************************************
      *   和暦年月日→西暦年月日（機能１２）
      *****************************************************************
       831-YMDCONV12-SEC                         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
      *
           INITIALIZE                            STS-AREA-DAY
           EVALUATE  WRK-LNKYMD(1:7)
               WHEN  ZERO
                   MOVE  ZERO                    TO  WRK-SYMD
               WHEN  "9999999"
                   MOVE  ALL "9"                 TO  WRK-SYMD
               WHEN  OTHER
      *
      *************IF  WRK-LNKYMD(1:7)           NOT NUMERIC
      *                MOVE    WRK-LNKYMD(1:1)   TO  WRK-WGO
      *                MOVE    WRK-LNKYMD(2:2)   TO  WRK-WYY
      *                MOVE    WRK-LNKYMD(5:2)   TO  WRK-WMM
      *                MOVE    WRK-LNKYMD(8:2)   TO  WRK-WDD
      *                INSPECT  WRK-WYMD  REPLACING   ALL " " BY  "0"
      *            ELSE
      *                MOVE    WRK-LNKYMD(1:7)   TO  WRK-WYMD
      *            END-IF
      *            INITIALIZE                    LNK-DAY1-AREA
      *            MOVE  "12"                    TO  LNK-DAY1-IRAI
      *            MOVE  ZERO                    TO  LNK-DAY1-YMD
      *            MOVE  WRK-WYMD                TO  LNK-DAY1-YMD(2:7)
      *            CALL  "ORCSDAY"               USING   STS-AREA-DAY
      *                                          LNK-DAY1-AREA
      *            IF      STS-DAY-RC1           =   ZERO
      *                MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
      *************END-IF
      *----(01.00.01) LINE ADD START --------------------------------
      *            日付画面チェックサブ
                   INITIALIZE                      ORCSGDAYAREA
                   MOVE    WRK-LNKYMD          TO  SGDAY-INDAY
                   CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
                   IF      SGDAY-RC            =   ZERO
                       MOVE    SGDAY-OTDAY         TO  WRK-LNKYMD
                       MOVE    SGDAY-SDAY          TO  WRK-SYMD
                   ELSE
                       MOVE    1                   TO  FLG-ERR
                   END-IF
      *----(01.00.01) LINE ADD END   --------------------------------
           END-EVALUATE
           .
      *
       831-YMDCONV12-EXT.
           EXIT.
      *
      *****************************************************************
      *   西暦→和暦（機能２１）
      *****************************************************************
       832-YMDCONV21-SEC                          SECTION.
      *
      *
           INITIALIZE                            STS-AREA-DAY
           EVALUATE  WRK-LNKYMD(1:8)
               WHEN  ZERO 
               WHEN  "99999999"
                   MOVE    WRK-LNKYMD            TO  WRK-HENYMDG
               WHEN  OTHER
                  INITIALIZE                     STS-AREA-DAY
                  INITIALIZE                     LNK-DAY2-AREA
                  MOVE  "21"                     TO  LNK-DAY2-IRAI
                  MOVE  WRK-LNKYMD(1:8)          TO  LNK-DAY2-YMD
                  CALL  "ORCSDAY"                USING   STS-AREA-DAY
                                                 LNK-DAY2-AREA
                  MOVE    LNK-DAY2-EDTYMD1       TO  WRK-HENYMDG
           END-EVALUATE
           .
      *
       832-YMDCONV21-EXT.
           EXIT.
      *
      *****************************************************************
      *   西暦→日数１日マイナス（機能６１）
      *****************************************************************
       833-YMDCONV61-SEC                          SECTION.
      *
           INITIALIZE                            STS-AREA-DAY
           EVALUATE  WRK-LNKYMD(1:8)
               WHEN  ZERO
                   MOVE  ZERO                    TO  WRK-SYMD
               WHEN  "99999999"
                   MOVE  ALL "9"                 TO  WRK-SYMD
               WHEN  OTHER
                  INITIALIZE                     STS-AREA-DAY
                  INITIALIZE                     LNK-DAY6-AREA
                  MOVE  "61"                     TO  LNK-DAY6-IRAI
                  MOVE  WRK-LNKYMD(1:8)          TO  LNK-DAY6-KIJUN
                  MOVE  "1"                      TO  LNK-DAY6-ZOGENPTN
                  MOVE  -1                       TO  LNK-DAY6-ZOGEN
                  CALL  "ORCSDAY"                USING   STS-AREA-DAY
                                                 LNK-DAY6-AREA
                  MOVE    LNK-DAY6-KEISAN        TO  WRK-SYMD
           END-EVALUATE
           .
      *
       833-YMDCONV61-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                            SECTION.
      *
           MOVE   "PUTWINDOW"                    TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
