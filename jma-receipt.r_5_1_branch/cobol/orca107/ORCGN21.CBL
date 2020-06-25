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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCGN21.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 薬剤情報設定
      *  コンポーネント名  : 薬剤情報（Ｎ２１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/07/25    NACL−森脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      * 00.01.01     NACL-森脇     02/08/28  検索開始文字数の変更
      *                                      検索結果表示の変更
      * 00.01.02     NACL-森脇     02/08/29  初期表示追加
      * 00.01.03     NACL-森脇     02/10/11  ーボタン追加
      * 00.01.04     NACL-森脇     02/10/11  登録時のクリア処理変更
      * 00.01.05     NACL-森脇     02/11/07  記号の半角全角混在チェック
      * 01.01.01     NACL-森脇     02/12/09  検索文字変更
      * 01.01.02     NACL-森脇     02/12/12  検索文字引数クリア
      * 01.01.03     NACL-森脇     03/01/08  画面入力文字数変更
      * 01.01.04     NACL-森脇     03/05/30  初期値変更
      * 02.01.01     NACL-森脇     03/06/13  一括更新
      * 02.01.02     NACL-森脇     04/10/05  半角全角チェック追加
      * 02.01.03     NACL-森脇     04/10/22  CLOSE処理
      * 02.02.01     NACL-森脇     05/12/13  薬剤情報参照追加
      * 02.02.02     NACL-森脇     06/05/11  薬剤情報リスト追加
      * 02.02.03     NACL-森脇     06/08/29  薬剤情報文字数チェック追加
      * 02.02.04     NACL-森脇     06/09/21  参照処理前スパセット
      * 02.02.05     NACL-森脇     06/09/29  登録後内容スパクリア
      * 02.02.06     NACL-森脇     06/10/06  薬剤名クリア処理修正
      * 03.03.01     NACL-森脇     06/10/10  画面リスト位置固定処理
      * 03.03.02     NACL-森脇     06/10/13  テキストエリア有効文字チェック
      * 03.03.03     NACL-森脇     06/10/23  画像パス名省略処理
      * 03.04.01     NACL-森脇     06/11/13  半角記号文字数チェック
      * 04.01.01     NACL-森脇     07/12/26  薬剤情報提供書文字数変更
      *                                      形状８行以上の時確認表示
      * 04.03.01     NACL-森脇     08/06/25  画像選択機能追加
      * 04.03.02     NACL-森脇     08/12/04  薬剤情報複写機能追加
      * 04.07.01     NACL-森脇     14/04/02  画像表示方法変更
      * 04.08.01     NACL-森脇     14/08/07  画像イメージ作成機能追加
      * 04.08.02     NACL-森脇     16/09/27  GINBEE(API)対応
      * 05.00.00     NACL-森脇     17/03/06  自費薬剤登録対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "N21COMMON-SPA".
           COPY    "N21SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-TENSU           PIC 9(01).
           03  FLG-YAKUJYO         PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(03).
           03  IDY                 PIC 9(03).
           03  IDZ                 PIC 9(03).
      *
           03  IDX-GYO             PIC 9(03).
      *
           03  IDX-STR             PIC 9(04).
           03  IDX-STR1            PIC 9(04).
           03  IDX-KET             PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD             PIC X(08).
           03  SYS-TIME            PIC X(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYCD           PIC X(9).
           03  WRK-NAME            PIC X(26).
           03  WRK-CDSYU           PIC X(01).
      *
           03  WRK-IPCPATH         PIC X(64).
           03  WRK-TNSPATH         PIC X(64).
      *
           03  WRK-N21IDMSG        PIC X(70).
      *
           03  WRK-ROW             PIC S9(9)   BINARY.
      *
           03  WRK-ZZZZ9           PIC ZZZZ9.
      *
       01  WRK-FILE-AREA.
           03  WRK-PASSMEI         PIC X(256).
           03  WRK-FILEMEI         PIC X(128).
      *
      *    全角を行毎に編集
       01  WRK-HENSYU-AREA.
           03  WRK-NAIYOU          PIC X(4000).
           03  GYO-MAX             PIC 9(04).
           03  KOU-MAX             PIC 9(04).
      *
      *    カナ検索用テーブル
       01  PIN-TBL-AREA.
           03  KANA-TBL.
               05  FILLER          PIC X(10)   VALUE "アイウエオ".
               05  FILLER          PIC X(10)   VALUE "カキクケコ".
               05  FILLER          PIC X(10)   VALUE "サシスセソ".
               05  FILLER          PIC X(10)   VALUE "タチツテト".
               05  FILLER          PIC X(10)   VALUE "ナニヌネノ".
               05  FILLER          PIC X(10)   VALUE "ハヒフヘホ".
               05  FILLER          PIC X(10)   VALUE "マミムメモ".
               05  FILLER          PIC X(06)   VALUE "ヤユヨ".
               05  FILLER          PIC X(10)   VALUE "ラリルレロ".
               05  FILLER          PIC X(10)   VALUE "ワヲン゜゛".
               05  FILLER          PIC X(08)   VALUE "ーＳＢＣ".
           03  KANA-TBL-R          REDEFINES   KANA-TBL.
               05  PKANA           PIC X(02)   OCCURS  52.
      *
       01  WRK-CONS-AREA.
      *    定型文
           03  WRK-CONS-CAUTION1.
               05  FILLER          PIC X(32)   VALUE
                   "医師の指示通りに服用して下さい。".
           03  WRK-CONS-CAUTION11.
               05  FILLER          PIC X(32)   VALUE
                   "医師の指示通りに使用して下さい。".
           03  WRK-CONS-CAUTION2.
               05  FILLER          PIC X(40)   VALUE
                   "発疹・かゆみ等の過敏症が現れた時は服用を".
               05  FILLER          PIC X(30)   VALUE
                   "中止し、医師に相談して下さい。".
           03  WRK-CONS-CAUTION21.
               05  FILLER          PIC X(40)   VALUE
                   "発疹・かゆみ等の過敏症が現れた時は使用を".
               05  FILLER          PIC X(30)   VALUE
                   "中止し、医師に相談して下さい。".
      *
      *    画像ファイルフォルダ名
           03  WRK-CONS-MEDIMGDIR  PIC X(08)   VALUE   "medimage".
      *
      *    文字数多い場合の変換領域
       01  WRK-KANACONV-AREA.
           03  WRK-KANACONV-LEN        PIC S9(9) BINARY.
           03  WRK-KANACONV-RETURN     PIC S9(9) BINARY.
           03  WRK-KANACONV-MAXLEN     PIC S9(9) BINARY.
           03  WRK-KANACONV-CONV-FLAG  PIC S9(9) BINARY.
           03  WRK-KANACONV-CHAR-TYPE  PIC S9(9) BINARY.
           03  WRK-KANACONV-STLEN      PIC S9(9) BINARY.
           03  WRK-KANACONV-INDATA     PIC X(4000).
           03  WRK-KANACONV-OUTDATA    PIC X(4000).
      *
      *    画像イメージ作成パラメタ
           COPY    "CPORCSMIAREA.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    システム管理
           COPY    "CPSK1030.INC".
      *
      *    薬剤情報マスタ
           COPY    "CPYAKUJYO.INC".
      *    薬剤情報参照用ＤＢ
           COPY    "CPYAKUJYOTEMP.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    画像イメージ
           COPY    "CPMEDIMAGE.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    全角半角チェック
           COPY    "CPORCSKANACHK.INC".
      *
      *    ファイル存在チェック
           COPY    "CPORCSFILECHK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *    LIKE使用KEY対応
           COPY    "CPORCSADDSIGN.INC".
      *
      *    COPY    "ORCA-DBPATH".
           COPY    "ORCA-BLOB".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
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
       PROCEDURE               DIVISION
                                   USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA.
      *
      *****************************************************************
      *    主　　処理
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
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
               WHEN    "LINK"      ALSO    ANY
                   PERFORM 100-INIT-SEC
      *        画面遷移
               WHEN    "PUTG"      ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *        入力
               WHEN    OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END         =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-N21SCRAREA  TO  SPA-FREE
           MOVE    SPA-N21KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG      =   "N21ERR"
               MOVE    SPACE       TO  SPA-MOTOPG
               PERFORM 530-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END         =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD   NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
      *
      *    ＰＵＴ処理
           MOVE    SPACE           TO  MCP-PUTTYPE
           MOVE    "N21"           TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1               TO  FLG-END
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"   ALSO    "B01"
                   PERFORM 460-BACK-SEC
      *        クリア
               WHEN    "CLICKED"   ALSO    "B02"
                   PERFORM 400-CLEAR-SEC
      *        削除
               WHEN    "CLICKED"   ALSO    "B03"
                   PERFORM 470-DEL-SEC
      *        リスト
               WHEN    "CLICKED"   ALSO    "B04"
                   PERFORM 450-LSTPRT-SEC
      *        複写
               WHEN    "CLICKED"   ALSO    "B08"
                   PERFORM 400-COPY-SEC
      *        画像
               WHEN    "CLICKED"   ALSO    "B09"
                   PERFORM 440-GAZOU-SEC
      *        入力
               WHEN    "CLICKED"   ALSO    "B10"
                   PERFORM 430-TEXTCHK-SEC
      *        参照
               WHEN    "CLICKED"   ALSO    "B11"
                   PERFORM 440-SANSYO-SEC
      *        登録
               WHEN    "CLICKED"   ALSO    "B12"
                   PERFORM 480-TOUROKU-SEC
      *        定型文１
               WHEN    "CLICKED"   ALSO    "TEIKEI1"
                   PERFORM 430-TEIKEI1-SEC
      *        定型文２
               WHEN    "CLICKED"   ALSO    "TEIKEI2"
                   PERFORM 430-TEIKEI2-SEC
      *        カナ入力
               WHEN    "CLICKED"   ALSO    ANY
                   PERFORM 490-KANA-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *        コード検索
               WHEN    "ACTIVATE"  ALSO    "KENCD"
                   PERFORM 410-KENCD-SEC
      *        選択番号入力
               WHEN    "ACTIVATE"  ALSO    "SELNUM"
                   PERFORM 420-SELNUM-SEC
      *        リストから選択
               WHEN    ANY         ALSO    "KEKKALST"
                   PERFORM 420-KEKKALST-SEC
      *        薬剤情報入力
               WHEN    OTHER
                   PERFORM 430-TEXTCHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU  TO  SPA-KYOUTU
               MOVE    SPACE           TO  SPA-N21FIL-PHOTO
               MOVE    LOW-VALUE       TO  SPA-N21FIL-OBJECT
           END-IF
      *
           EVALUATE    SPA-MOTOPG
      *        確認画面ＯＫ処理
               WHEN    "N21ID1"
                   PERFORM 320-N21ID1-SET-SEC
                   GO  TO  300-SCREEN-EXT
      *        参照画面処理
               WHEN    "N22"
                   PERFORM 320-N22-SET-SEC
                   GO  TO  300-SCREEN-EXT
      *        画像選択画面処理
               WHEN    "N21FIL"
                   IF      SPA-N21FIL-PHOTO    =   "SIZEERR"
                       MOVE    "0022"      TO  SPA-ERRCD
                       MOVE    10          TO  SPA-GMN-CUR
                   ELSE
                       PERFORM 430-GAZOCHK-HENSYU-SEC
                   END-IF
                   GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-N21ID1-FLG
           MOVE    SPACE           TO  SPA-N21IDCD
      *
      *    スパ初期編集処理
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-N21-AREA
           INITIALIZE                  SPA-NAI-PIN-AREA
      *
           MOVE    LOW-VALUE       TO  SPA-GMN-OBJECT
                                       SPA-NAI-OBJECT
      *
           MOVE    2               TO  SPA-GMN-CUR
      *
      *    画像ファイルフォルダ
           MOVE    SPACE               TO  SPA-GMN-FOLDER
           STRING  MCP-TEMPDIR         DELIMITED BY SPACE
                   "/"                 DELIMITED BY SIZE
                   WRK-CONS-MEDIMGDIR  DELIMITED BY SIZE
                   "/"                 DELIMITED BY SIZE
                   INTO    SPA-GMN-FOLDER
           END-STRING
           . 
      *
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    参照画面（Ｎ２２）ＯＫ処理
      *****************************************************************
       320-N22-SET-SEC             SECTION.
      *
           IF      SPA-N22-FLG     =   "OK"
               MOVE    10              TO  SPA-GMN-CUR
      *        写真があればセット
               PERFORM 430-GAZOCHK-HENSYU-SEC
               MOVE    SPACE       TO  SPA-ERRCD
           END-IF
      *
           .
       320-N22-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（Ｎ２１ＩＤ１）ＯＫ処理
      *****************************************************************
       320-N21ID1-SET-SEC          SECTION.
      *
           IF      SPA-N21ID1-FLG    =   "OK"
               EVALUATE      SPA-N21IDCD
      *            更新処理
                   WHEN    "1001"
                       PERFORM 481-KOUSHIN-SEC
      *            削除処理
                   WHEN    "1002"
                       PERFORM 471-DEL-SEC
      *            編集データキャンセル処理
                   WHEN    "1003"
                       PERFORM 400-CANCEL-SEC
      *            警告確認了解の場合は更新確認メッセージを表示する
                   WHEN    "1004"
                       MOVE    SPACE           TO  SPA-N21ID1-FLG
                       MOVE    "1001"          TO  SPA-N21IDCD
                       PERFORM 520-N21IDSET-SEC
                       GO  TO  320-N21ID1-SET-EXT
               END-EVALUATE
           ELSE
      *        キャンセルしない場合戻す
               IF      SPA-N21IDCD     =   "1003"
                   MOVE    SPA-NAI-SRYCD       TO  SPA-GMN-KENCD
                   MOVE    SPA-NAI-SELNUM      TO  SPA-GMN-SELNUM
                   MOVE    SPA-NAI-KENNAIYO    TO  SPA-GMN-KENNAIYO
                   MOVE    4           TO  SPA-GMN-CUR
               ELSE
                   MOVE    2           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  SPA-N21ID1-FLG
           MOVE    SPACE           TO  SPA-N21IDCD
      *
           .
       320-N21ID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    編集データキャンセル処理
      *****************************************************************
       400-CANCEL-SEC              SECTION.
      *
      *    次の処理を行う
           EVALUATE    SPA-GMN-EVENT
               WHEN    "1"
                   PERFORM 410-KENCDLIST-SET-SEC
               WHEN    "2"
                   PERFORM 420-LISTSET-SEC
               WHEN    "3"
                   PERFORM 490-KANAMOJI-SET-SEC
               WHEN    "4"
                   PERFORM 460-BACK-SYORI-SEC
           END-EVALUATE
      *
           .
       400-CANCEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       400-CLEAR-SEC                SECTION.
      *
           IF      SPA-GMN-JYOTAIFLG    =   ZERO
               PERFORM 310-SPASET-SEC
           ELSE
               PERFORM 401-CLEAR-ENTRY-SEC
               IF      SPA-GMN-SELNUM   NOT =   ZERO
                   MOVE    SPA-GMN-KENNAME     TO  SPA-GMN-NAME
               END-IF
           END-IF
      *
           .
       400-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア編集領域処理
      *****************************************************************
       401-CLEAR-AREA-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-AREA
           INITIALIZE                  SPA-NAI-AREA
           MOVE    LOW-VALUE       TO  SPA-GMN-OBJECT
                                       SPA-NAI-OBJECT
           MOVE    SPACE           TO  SPA-GMN-EVENT
      *
           .
       401-CLEAR-AREA-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア入力領域処理
      *****************************************************************
       401-CLEAR-ENTRY-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-ENTRY
           INITIALIZE                  SPA-NAI-ENTRY
           MOVE    LOW-VALUE       TO  SPA-GMN-OBJECT
                                       SPA-NAI-OBJECT
      *
           .
       401-CLEAR-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    複写処理
      *****************************************************************
       400-COPY-SEC                SECTION.
      *
           IF      SPA-GMN-JYOTAIFLG   =   ZERO
               INITIALIZE                      SPA-COPY-AREA
               MOVE    SPA-NAI-YAKKAKJNCD  TO  SPA-COPY-YAKKAKJNCD
               MOVE    SPA-NAI-YKZKBN      TO  SPA-COPY-YKZKBN
               MOVE    SPA-NAI-SRYCD       TO  SPA-COPY-SRYCD
               MOVE    SPA-NAI-KENCD       TO  SPA-COPY-KENCD
               MOVE    SPA-NAI-SELNUM      TO  SPA-COPY-SELNUM
               MOVE    SPA-NAI-EFFECT      TO  SPA-COPY-EFFECT
               MOVE    SPA-NAI-CAUTION     TO  SPA-COPY-CAUTION
               PERFORM 800-COPYLIST-SET-SEC
           ELSE
               PERFORM 401-CLEAR-AREA-SEC
               MOVE    ZERO                TO  SPA-GMN-JYOTAIFLG
               IF      SPA-COPY-SELNUM     =   ZERO
                   MOVE    SPA-COPY-SRYCD  TO  SPA-GMN-KENCD
                   PERFORM 410-KENCDLIST-SET-SEC
               ELSE
                   IF      SPA-COPY-KENCD   NOT =   SPACE
                       MOVE    SPA-COPY-KENCD  TO  SPA-GMN-KENCD
                       PERFORM 410-KENCDLIST-SET-SEC
                   ELSE
                       MOVE    SPA-NAI-PED         TO  SPA-GMN-KENNAIYO
                                                   WRK-NAME
                       PERFORM 800-TENSULIST-SET-SEC
                   END-IF
                   MOVE    SPA-COPY-SELNUM TO  SPA-GMN-SELNUM
                   PERFORM 420-LISTSET-SEC
               END-IF
               INITIALIZE                      SPA-COPY-AREA
           END-IF
      *
           .
       400-COPY-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索コード処理
      *****************************************************************
       410-KENCD-SEC               SECTION.
      *
      *    複写モードならば処理抜ける
           IF      SPA-GMN-JYOTAIFLG   =   1
               GO  TO  410-KENCD-EXT
           END-IF
      *
      *    編集中の場合、保存するか確認する
           IF      SPA-GMN-ENTRY   NOT =   SPA-NAI-ENTRY
               MOVE    "1003"          TO  SPA-N21IDCD
               MOVE    "1"             TO  SPA-GMN-EVENT
               MOVE    N21-KENCD       TO  SPA-GMN-KENCD
               GO  TO  410-KENCD-EXT
           END-IF
      *
           PERFORM 310-SPASET-SEC
      *
           MOVE    N21-KENCD       TO  SPA-GMN-KENCD
           PERFORM 410-KENCDLIST-SET-SEC
      *
           .
       410-KENCD-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索コードリストセット
      *****************************************************************
       410-KENCDLIST-SET-SEC       SECTION.
      *
           IF      SPA-GMN-KENCD   =   SPACE
               GO  TO  410-KENCDLIST-SET-EXT
           END-IF
      *
      *    検索コード種類判定
           PERFORM 411-KENCD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  410-KENCDLIST-SET-EXT
           END-IF
      *
           INITIALIZE                  SPA-NAI-PIN-AREA
           MOVE    SPA-GMN-KENCD   TO  WRK-NAME
           PERFORM 401-CLEAR-AREA-SEC
           MOVE    WRK-NAME        TO  SPA-GMN-KENCD
                                       SPA-NAI-KENCD
      *
      *    入力コード読み込み
           PERFORM 900-INPUTCD-SELECT-SEC
           MOVE    ZERO            TO  IDX
           PERFORM UNTIL ( IDX     =   200 )
                   OR    ( FLG-INPUTCD =   1 )
               MOVE    ICD-SRYCD   TO  WRK-SRYCD
      *        点数マスタ読み込み
               PERFORM 900-TENSU-READ-SEC
               IF      FLG-TENSU   =   ZERO
                    PERFORM 800-LIST-TENSU-SET-SEC
               END-IF
               PERFORM 900-INPUTCD-FETCH-SEC
           END-PERFORM
      *
           PERFORM 900-INPUTCD-CLOSE-SEC
      *
      *    件数がない場合点数マスタから
           IF      IDX         =   ZERO
               IF          WRK-CDSYU   =   "J"
                   MOVE    SPA-GMN-KENCD   TO  WRK-NAME
                   PERFORM 800-TENSULIST-SET-SEC
               ELSE
      *----(05.00.00)--UPD-START---
      *             IF    ( KANACHK-MAX     =   9 ) AND
      *                   ( SPA-GMN-KENCD(1:1)  =   "6" )
                   IF    ( KANACHK-MAX     =   9 )
      *----(05.00.00)--UPD-END-----
                       MOVE    SPA-GMN-KENCD   TO  WRK-SRYCD
                       PERFORM 900-TENSU-READ-SEC
                       IF      FLG-TENSU       =   ZERO
                           MOVE    TNS-NAME    TO  SPA-GMN-KENNAME
                           MOVE    TNS-SRYCD   TO  SPA-GMN-KENCD
                                                   SPA-NAI-SRYCD
                           MOVE    TNS-YAKKAKJNCD
                                               TO  SPA-NAI-YAKKAKJNCD
                           MOVE    TNS-YKZKBN  TO  SPA-NAI-YKZKBN
                           PERFORM 800-YAKUJYO-SET-SEC
                       ELSE
                           PERFORM 800-LIST-KENSU-SET-SEC
                       END-IF
                   ELSE
                       PERFORM 800-LIST-KENSU-SET-SEC
                   END-IF
               END-IF
           ELSE
               PERFORM 800-LIST-KENSU-SET-SEC
           END-IF
      *
           .
       410-KENCDLIST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     入力コード種類判定
      *****************************************************************
       411-KENCD-CHK-SEC           SECTION.
      *
      *    漢字入力チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    SPA-GMN-KENCD   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-KENCD
      *    全角半角混在エラー
           IF    ( KANACHK-RC      NOT =   ZERO )  OR
                 ( KANACHK-RC2         =   7    )
               MOVE    "0005"          TO  SPA-ERRCD
               GO  TO  411-KENCD-CHK-EXT
           END-IF
      *
      *    種別判定
           EVALUATE    KANACHK-SYUBETU
      *        全角
               WHEN    2
                   IF      KANACHK-MAX     >   20
                       MOVE    "0004"      TO  SPA-ERRCD
                   ELSE
                       MOVE    "J"         TO  WRK-CDSYU
                       MOVE    "key"       TO  WRK-IPCPATH
                   END-IF
      *        半角
               WHEN    1
      *            コード種別を桁数で決定
                   IF     (SPA-GMN-KENCD(1:KANACHK-MAX)    NUMERIC)
                       EVALUATE    KANACHK-MAX
                           WHEN    4
                               MOVE    "4"         TO  WRK-CDSYU
                           WHEN    5
                               MOVE    "5"         TO  WRK-CDSYU
                           WHEN    6
                               MOVE    "6"         TO  WRK-CDSYU
                           WHEN    OTHER
                               MOVE    "6"         TO  WRK-CDSYU
                       END-EVALUATE
                       MOVE    "key2"              TO  WRK-IPCPATH
                   ELSE
                       IF      SPA-GMN-KENCD(1:1)  =   "P"  OR  "S"
                           MOVE    "0007"      TO  SPA-ERRCD
                       ELSE
                           MOVE    "A"             TO  WRK-CDSYU
                           MOVE    "key"           TO  WRK-IPCPATH
                       END-IF
                   END-IF
           END-EVALUATE
      *
        .
       411-KENCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号処理
      *****************************************************************
       420-SELNUM-SEC              SECTION.
      *
      *    該当がない場合処理抜ける
           IF        ( SPA-GMN-MAX     =   1 )
               AND   ( SPA-GMN-JYOTAIFLG   =   ZERO )
               MOVE    2           TO  SPA-GMN-CUR
               MOVE    ZERO        TO  SPA-GMN-SELNUM
               GO  TO  420-SELNUM-EXT
           END-IF
      *
           MOVE    N21-SELNUM      TO  SPA-GMN-SELNUM
      *
           IF      SPA-GMN-SELNUM  =   ZERO
               MOVE    3               TO  SPA-GMN-CUR
               GO  TO  420-SELNUM-EXT
           END-IF
      *
      *    番号チェック
           IF      SPA-GMN-SELNUM  >   SPA-GMN-MAX
               MOVE    3               TO  SPA-GMN-CUR
               MOVE    "0008"          TO  SPA-ERRCD
               GO  TO  420-SELNUM-EXT
           END-IF
      *
      *    編集中の場合、保存するか確認する
           IF      SPA-GMN-ENTRY   NOT =   SPA-NAI-ENTRY
               MOVE    "1003"          TO  SPA-N21IDCD
               MOVE    "2"             TO  SPA-GMN-EVENT
               GO  TO  420-SELNUM-EXT
           END-IF
      *
           PERFORM 420-LISTSET-SEC
           .
      *
       420-SELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    結果リスト選択処理
      *****************************************************************
       420-KEKKALST-SEC            SECTION.
      *
      *    該当がない場合処理抜ける
           IF        ( SPA-GMN-MAX     =   1 )
               AND   ( SPA-GMN-JYOTAIFLG   =   ZERO )
               MOVE    2           TO  SPA-GMN-CUR
               MOVE    ZERO        TO  SPA-GMN-SELNUM
               GO  TO  420-KEKKALST-EXT
           END-IF
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-GMN-MAX
               IF      N21-SELECT (IDX)    =   "T"
                   IF      IDX         NOT =   SPA-GMN-SELNUM
                       MOVE    IDX             TO  SPA-GMN-SELNUM
                       MOVE    SPA-GMN-MAX     TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
      *    編集中の場合、保存するか確認する
           IF      SPA-GMN-ENTRY   NOT =   SPA-NAI-ENTRY
               MOVE    "1003"          TO  SPA-N21IDCD
               MOVE    "2"             TO  SPA-GMN-EVENT
               GO  TO  420-KEKKALST-EXT
           END-IF
      *
           PERFORM 420-LISTSET-SEC
      *
           .
       420-KEKKALST-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択薬剤セット処理
      *****************************************************************
       420-LISTSET-SEC             SECTION.
      *
           MOVE    SPA-GMN-SELNUM  TO  SPA-NAI-SELNUM
      *
           MOVE    SPA-GMN-TNAME(SPA-GMN-SELNUM)
                                   TO  SPA-GMN-KENNAME
           MOVE    SPA-GMN-TCD(SPA-GMN-SELNUM)
                                   TO  SPA-GMN-KENCD
                                       SPA-NAI-SRYCD
           MOVE    SPA-GMN-TKJNCD(SPA-GMN-SELNUM)
                                   TO  SPA-NAI-YAKKAKJNCD
           MOVE    SPA-GMN-TYKZKBN(SPA-GMN-SELNUM)
                                   TO  SPA-NAI-YKZKBN
      *
      *    薬剤情報マスタ読み込み
           PERFORM 800-YAKUJYO-SET-SEC
      *
           .
       420-LISTSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    定型文１
      *****************************************************************
       430-TEIKEI1-SEC             SECTION.
      *
      *    薬剤が選択されているかチェック
           IF      SPA-NAI-SRYCD   =   SPACE
                GO  TO  430-TEIKEI1-EXT
           END-IF
      *
           IF      SPA-NAI-YKZKBN  =   1
               STRING  N21-CAUTION         DELIMITED BY SPACE
                       WRK-CONS-CAUTION1   DELIMITED BY SPACE
                                     INTO  SPA-GMN-CAUTION
               END-STRING
           ELSE
               STRING  N21-CAUTION         DELIMITED BY SPACE
                       WRK-CONS-CAUTION11   DELIMITED BY SPACE
                                     INTO  SPA-GMN-CAUTION
               END-STRING
           END-IF
      *
           .
       430-TEIKEI1-EXT.
           EXIT.
      *
      *****************************************************************
      *    定型文２
      *****************************************************************
       430-TEIKEI2-SEC             SECTION.
      *
      *    薬剤が選択されているかチェック
           IF      SPA-NAI-SRYCD   =   SPACE
                GO  TO  430-TEIKEI2-EXT
           END-IF
      *
           IF      SPA-NAI-YKZKBN  =   1
               STRING  N21-CAUTION         DELIMITED BY SPACE
                       WRK-CONS-CAUTION2   DELIMITED BY SPACE
                                     INTO  SPA-GMN-CAUTION
               END-STRING
           ELSE
               STRING  N21-CAUTION         DELIMITED BY SPACE
                       WRK-CONS-CAUTION21  DELIMITED BY SPACE
                                     INTO  SPA-GMN-CAUTION
               END-STRING
           END-IF
      *
           .
       430-TEIKEI2-EXT.
           EXIT.
      *
      *****************************************************************
      *    テキストチェック
      *****************************************************************
       430-TEXTCHK-SEC             SECTION.
      *
      *    薬剤が選択されているかチェック
           IF      SPA-NAI-SRYCD   =   SPACE
               MOVE    "0011"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
               GO  TO  430-TEXTCHK-EXT
           END-IF
      *
           INITIALIZE                  SPA-GMN-ENTRY
           MOVE    N21-NAME        TO  SPA-GMN-NAME
           MOVE    N21-EFFECT      TO  SPA-GMN-EFFECT
           MOVE    N21-COLOR       TO  SPA-GMN-COLOR
           MOVE    N21-SHAPE       TO  SPA-GMN-SHAPE
           MOVE    N21-MARK        TO  SPA-GMN-MARK
           MOVE    N21-CAUTION     TO  SPA-GMN-CAUTION
           MOVE    N21-PHOTO       TO  SPA-GMN-PHOTO
      *
           INITIALIZE                  WRK-HENSYU-AREA
      *
      *    薬剤画像チェック
           PERFORM 430-GAZOCHK-HENSYU-SEC
      *
      *    薬剤名称チェック
           PERFORM 431-NAME-SET-SEC
      *
      *    効能効果チェック
           PERFORM 431-EFFECT-SET-SEC
      *
      *    注意事項チェック
           PERFORM 431-CAUTION-SET-SEC
      *
      *    形状色チェック
           PERFORM 431-COLOR-SET-SEC
      *
      *    形状形チェック
           PERFORM 431-SHAPE-SET-SEC
      *
      *    形状記号チェック
           PERFORM 431-MARK-SET-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  430-TEXTCHK-EXT
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    10
                   MOVE    99          TO  SPA-GMN-CUR
               WHEN    99
                   MOVE    2           TO  SPA-GMN-CUR
               WHEN    OTHER
                   COMPUTE SPA-GMN-CUR =   SPA-GMN-CUR +   1
           END-EVALUATE
      *
           .
       430-TEXTCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    文字数チェック編集処理
      *****************************************************************
       430-MOJICHK-HENSYU-SEC      SECTION.
      *
           MOVE    1               TO  IDX-STR
           MOVE    1               TO  IDX-KET
      *
           PERFORM UNTIL  (IDX-STR     >   4000 )
                   OR     (IDX-GYO     >   GYO-MAX )
                   OR     (WRK-NAIYOU(IDX-STR:)    =   SPACE)
      *        改行なら最大字セット
               IF      WRK-NAIYOU(IDX-STR:1)   =   X"0A"
                   MOVE    KOU-MAX         TO  IDX-KET
                   ADD     1               TO  IDX-STR
               ELSE
      *            全角セット
                   IF      WRK-NAIYOU(IDX-STR:1)    >=  X"A1"
                       ADD     2               TO  IDX-STR
                       ADD     2               TO  IDX-KET
      *            半角セット
                   ELSE
                       ADD     1               TO  IDX-STR
                       ADD     1               TO  IDX-KET
                   END-IF
               END-IF
      *        最大字で改行
               IF         (IDX-KET         >=  KOU-MAX)
                   OR    ((IDX-STR         <=  4000)
                   AND    (WRK-NAIYOU(IDX-STR:)    =   SPACE))
                   ADD     1               TO  IDX-GYO
                   MOVE    1               TO  IDX-KET
      *            改行なら次へ
                   IF      WRK-NAIYOU(IDX-STR:1)   =   X"0A"
                       COMPUTE IDX-STR =   IDX-STR     +   1
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       430-MOJICHK-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    カナチェック編集処理
      *****************************************************************
       430-KANACHK-HENSYU-SEC      SECTION.
      *
      *    500バイト以上ならサイズの大きいカナチェックを行う
           IF      WRK-NAIYOU(500:)    NOT =   SPACE
               PERFORM 430-KANACHK-HENSYU-BIG-SEC
               GO  TO  430-KANACHK-HENSYU-EXT
           END-IF
      *
      *    カナチェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    1000            TO  KANACHK-MAX-CNT
           MOVE    WRK-NAIYOU      TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
      *
           IF      KANACHK-OUT-INPUT   =   SPACE
               GO  TO  430-KANACHK-HENSYU-EXT
           END-IF
      *
           MOVE    SPACE           TO  WRK-NAIYOU
           MOVE    1               TO  IDX-STR
           MOVE    1               TO  IDX-STR1
           MOVE    KANACHK-MAX     TO  IDX-KET
      *
      *    末尾空白削除
           PERFORM UNTIL   IDX-KET <       1
               IF    ( KANACHK-OUT-INPUT(IDX-KET:1) =   SPACE ) OR
                     ( KANACHK-OUT-INPUT(IDX-KET:1) =   X"0A" )
                   COMPUTE IDX-KET =   IDX-KET -   1
               ELSE
                   IF      KANACHK-OUT-INPUT(IDX-KET:2) =   "　"
                       COMPUTE IDX-KET =   IDX-KET -   2
                   ELSE
                       MOVE    KANACHK-OUT-INPUT(1:IDX-KET)
                                    TO  WRK-NAIYOU(IDX-STR1:IDX-KET)
                       COMPUTE IDX-STR1 = IDX-STR1 +  IDX-KET
                       MOVE    ZERO                TO IDX-KET
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       430-KANACHK-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目カナチェック編集処理（大きい）
      *****************************************************************
       430-KANACHK-HENSYU-BIG-SEC  SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           INITIALIZE                  WRK-KANACONV-AREA
           MOVE    4000            TO  WRK-KANACONV-LEN
           MOVE    4000            TO  WRK-KANACONV-STLEN
           MOVE    0               TO  WRK-KANACONV-CONV-FLAG
           MOVE    15              TO  WRK-KANACONV-CHAR-TYPE
           MOVE    WRK-NAIYOU      TO  WRK-KANACONV-INDATA
           CALL    "kanaconv"          USING
                                       WRK-KANACONV-AREA
           IF      WRK-KANACONV-RETURN =   ZERO
      *        全角のみ
               INITIALIZE                  WRK-KANACONV-AREA
               MOVE    4000            TO  WRK-KANACONV-LEN
               MOVE    4000            TO  WRK-KANACONV-STLEN
               MOVE    1               TO  WRK-KANACONV-CONV-FLAG
               MOVE    15              TO  WRK-KANACONV-CHAR-TYPE
               MOVE    WRK-NAIYOU      TO  WRK-KANACONV-INDATA
               CALL    "kanaconv"          USING
                                           WRK-KANACONV-AREA
               MOVE    2               TO  KANACHK-SYUBETU
               MOVE    WRK-KANACONV-MAXLEN TO  KANACHK-MAX
           ELSE
      *        混在エラー
               MOVE    1               TO  KANACHK-RC
               MOVE    WRK-KANACONV-MAXLEN TO  KANACHK-MAX
           END-IF
      *
           IF      WRK-KANACONV-OUTDATA    =   SPACE
               GO  TO  430-KANACHK-HENSYU-BIG-EXT
           END-IF
      *
           MOVE    SPACE           TO  WRK-NAIYOU
           MOVE    1               TO  IDX-STR
           MOVE    1               TO  IDX-STR1
           MOVE    KANACHK-MAX     TO  IDX-KET
      *
      *    末尾空白削除
           PERFORM UNTIL   IDX-KET <   1
               IF    ( WRK-KANACONV-OUTDATA(IDX-KET:1) =   SPACE ) OR
                     ( WRK-KANACONV-OUTDATA(IDX-KET:1) =   X"0A" )
                   COMPUTE IDX-KET =   IDX-KET -   1
               ELSE
                   IF      WRK-KANACONV-OUTDATA(IDX-KET:2) =   "　"
                       COMPUTE IDX-KET =   IDX-KET -   2
                   ELSE
                       MOVE    WRK-KANACONV-OUTDATA(1:IDX-KET)
                                    TO  WRK-NAIYOU(IDX-STR1:IDX-KET)
                       COMPUTE IDX-STR1 = IDX-STR1 +  IDX-KET
                       MOVE    ZERO                TO IDX-KET
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       430-KANACHK-HENSYU-BIG-EXT.
           EXIT.
      *
      *****************************************************************
      *    画像ファイルチェック
      *****************************************************************
       430-GAZOCHK-HENSYU-SEC      SECTION.
      *
           MOVE    SPACE           TO  SPA-N21FIL-PHOTO
           MOVE    LOW-VALUE       TO  N21-PHOTOAREA
      *
           IF      SPA-GMN-PHOTO   =   SPACE
               MOVE    LOW-VALUE   TO  SPA-GMN-OBJECT
               GO  TO  430-GAZOCHK-HENSYU-EXT
           END-IF
      *
      *    末尾文字編集
           MOVE    SPA-GMN-PHOTO   TO  WRK-NAIYOU
           PERFORM 430-KANACHK-HENSYU-SEC
           MOVE    WRK-NAIYOU      TO  SPA-GMN-PHOTO
      *
      *    カナチェック
           IF    ( KANACHK-SYUBETU     NOT =   1    )  OR
                 ( KANACHK-RC          NOT =   ZERO ) 
               MOVE    "0003"      TO  SPA-ERRCD
               MOVE    10          TO  SPA-GMN-CUR
               GO  TO  430-GAZOCHK-HENSYU-EXT
           END-IF
      *
      *    拡張子が違えば処理抜ける
           PERFORM 431-GAZO-TYPE-CHK-SEC
           IF      FLG-OK          =   1
               MOVE    "0015"      TO  SPA-ERRCD
               MOVE    10          TO  SPA-GMN-CUR
               MOVE    LOW-VALUE   TO  SPA-GMN-OBJECT
               GO  TO  430-GAZOCHK-HENSYU-EXT
           END-IF
      *
      *    パス名をフォルダ名とファイル名に分ける
           PERFORM 431-GAZO-PATH-CHK-SEC
      *
      *    ファイル存在チェック
           PERFORM 431-GAZO-FILE-CHK
           IF      FLG-OK    =   1
               MOVE    "0014"      TO  SPA-ERRCD
               MOVE    10          TO  SPA-GMN-CUR
               MOVE    LOW-VALUE   TO  SPA-GMN-OBJECT
               GO  TO  430-GAZOCHK-HENSYU-EXT
           END-IF
      *
      *    画像データサイズをチェック
           INITIALIZE                  ORCSMIAREA
           MOVE    "misizechk"     TO  ORCSMI-SRYCD
           MOVE    MCP-TEMPDIR     TO  ORCSMI-TEMPDIR
           MOVE    WRK-FILEMEI     TO  ORCSMI-FILENAME
           CALL    "ORCSMISAVE"        USING
                                       ORCSMIAREA
                                       SPA-AREA
           IF      ORCSMI-RETCD    NOT =   ZERO
               MOVE    "0022"          TO  SPA-ERRCD
               MOVE    10              TO  SPA-GMN-CUR
               MOVE    LOW-VALUE       TO  SPA-GMN-OBJECT
               GO  TO  430-GAZOCHK-HENSYU-EXT
           END-IF
      *
      *    画像ファイルを表示
           PERFORM 500-GAZO-OBJECT-SEC
           .
       430-GAZOCHK-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    画像ファイルチェック
      *****************************************************************
       431-GAZOCHK-SYOKI-SEC       SECTION.
      *
           MOVE    LOW-VALUE       TO  N21-PHOTOAREA
      *
           IF      SPA-GMN-PHOTO   =   SPACE
               MOVE    LOW-VALUE   TO  SPA-GMN-OBJECT
               GO  TO  431-GAZOCHK-SYOKI-EXT
           END-IF
      *
      *    パス名をフォルダ名とファイル名に分ける
           PERFORM 431-GAZO-PATH-CHK-SEC
      *
      *    データからイメージファイル作成
           INITIALIZE                  ORCSMIAREA
           MOVE    SPA-NAI-SRYCD   TO  ORCSMI-SRYCD
           MOVE    MCP-TEMPDIR     TO  ORCSMI-TEMPDIR
           MOVE    WRK-FILEMEI     TO  ORCSMI-FILENAME
           CALL    "ORCSMILOAD"        USING
                                       ORCSMIAREA
                                       SPA-AREA
           .
       431-GAZOCHK-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画像ファイル存在チェック
      *****************************************************************
       431-GAZO-FILE-CHK           SECTION.
      *
           MOVE    SPACE           TO  ORCA-BLOB-FILE
           STRING  MCP-TEMPDIR         DELIMITED BY SPACE
                   "/"                 DELIMITED BY SIZE
                   WRK-CONS-MEDIMGDIR  DELIMITED BY SPACE
                   "/"                 DELIMITED BY SIZE
                   WRK-FILEMEI         DELIMITED BY SPACE
                               INTO    ORCA-BLOB-FILE
           END-STRING
      *
           MOVE    ZERO            TO  FLG-OK
      *
      *    画像ファイルがない場合は処理抜ける
           INITIALIZE                  ORCSFILECHKAREA
           MOVE    ORCA-BLOB-FILE  TO  FILECHK-NAME
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
      *    拡張子チェック
      *****************************************************************
       431-GAZO-TYPE-CHK-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM VARYING IDZ     FROM    256 BY -1 
                   UNTIL   IDZ     <       1
               IF      SPA-GMN-PHOTO(IDZ:1)    NOT =   SPACE
                   COMPUTE IDZ     =   IDZ     -   3
                   IF      IDZ     >   ZERO
                       EVALUATE    SPA-GMN-PHOTO(IDZ:4)
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
                       MOVE    1               TO  IDZ
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       431-GAZO-TYPE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画像パス名編集
      *****************************************************************
       431-GAZO-PATH-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-FILE-AREA
      *
           MOVE    SPA-GMN-PHOTO   TO  WRK-PASSMEI
           MOVE    SPACE           TO  WRK-FILEMEI
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM VARYING IDZ     FROM    256 BY -1 
                   UNTIL   IDZ     <       1
               IF      WRK-PASSMEI(IDZ:)   NOT =   SPACE
                   IF    ( WRK-PASSMEI(IDZ:1)  =   "/" ) OR
                         ( WRK-PASSMEI(IDZ:1)  =   "\" )
                       IF      WRK-PASSMEI(1:IDZ)
                                           NOT =   SPA-GMN-FOLDER(1:IDZ)
                           MOVE    1           TO  FLG-OK
                       END-IF
                       COMPUTE IDZ     =   IDZ     +   1
                       MOVE    WRK-PASSMEI(IDZ:)
                                           TO  WRK-FILEMEI
                       MOVE    1           TO  IDZ
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
      *   薬剤名称データセット処理
      *****************************************************************
       431-NAME-SET-SEC            SECTION.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  431-NAME-SET-EXT
           END-IF
      *
           IF      SPA-GMN-NAME    NOT =   SPACE
      *        末尾文字編集
               MOVE    SPA-GMN-NAME    TO  WRK-NAIYOU
               PERFORM 430-KANACHK-HENSYU-SEC
               MOVE    WRK-NAIYOU      TO  SPA-GMN-NAME
      *        カナチェック
               IF    ( KANACHK-SYUBETU     NOT =   2    )  OR
                     ( KANACHK-RC          NOT =   ZERO )  OR
                     ( KANACHK-RC2             =   7    )
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE    4           TO  SPA-GMN-CUR
                   GO  TO  431-NAME-SET-EXT
               END-IF
      *        文字数チェック
               MOVE    1               TO  IDX-GYO
               MOVE    3               TO  GYO-MAX
               MOVE    34              TO  KOU-MAX
               PERFORM 430-MOJICHK-HENSYU-SEC
               IF      WRK-NAIYOU(IDX-STR:)    NOT =   SPACE
                   MOVE    "0016"      TO  SPA-ERRCD
                   MOVE    4           TO  SPA-GMN-CUR
                   GO  TO  431-NAME-SET-EXT
               END-IF
           END-IF
      *
      *    薬剤名が空白ならセット
           IF      SPA-GMN-NAME    =   SPACE
               MOVE    SPA-GMN-KENNAME     TO  SPA-GMN-NAME
           END-IF
      *
           .
       431-NAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   効能効果データセット処理
      *****************************************************************
       431-EFFECT-SET-SEC          SECTION.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  431-EFFECT-SET-EXT
           END-IF
      *
           IF      SPA-GMN-EFFECT    NOT =   SPACE
      *        末尾文字編集
               MOVE    SPA-GMN-EFFECT  TO  WRK-NAIYOU
               PERFORM 430-KANACHK-HENSYU-SEC
               MOVE    WRK-NAIYOU      TO  SPA-GMN-EFFECT
      *        カナチェック
               IF    ( KANACHK-SYUBETU     NOT =   2    )  OR
                     ( KANACHK-RC          NOT =   ZERO )  OR
                     ( KANACHK-RC2             =   7    )
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE    5           TO  SPA-GMN-CUR
                   GO  TO  431-EFFECT-SET-EXT
               END-IF
               IF      WRK-NAIYOU(1001:)   NOT =   SPACE
                   COMPUTE IDX-KET    =   IDX-STR1 / 2
                   MOVE    "0017"      TO  SPA-ERRCD
                   MOVE    5           TO  SPA-GMN-CUR
                   GO  TO  431-EFFECT-SET-EXT
               END-IF
           END-IF
      *
           .
       431-EFFECT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   注意事項データセット処理
      *****************************************************************
       431-CAUTION-SET-SEC         SECTION.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  431-CAUTION-SET-EXT
           END-IF
      *
           IF      SPA-GMN-CAUTION NOT =   SPACE
      *        末尾文字編集
               MOVE    SPA-GMN-CAUTION TO  WRK-NAIYOU
               PERFORM 430-KANACHK-HENSYU-SEC
               MOVE    WRK-NAIYOU      TO  SPA-GMN-CAUTION
      *        カナチェック
               IF    ( KANACHK-SYUBETU     NOT =   2    )  OR
                     ( KANACHK-RC          NOT =   ZERO )  OR
                     ( KANACHK-RC2             =   7    )
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE    9           TO  SPA-GMN-CUR
                   GO  TO  431-CAUTION-SET-EXT
               END-IF
      *        文字数オーバーチェック
               IF      WRK-NAIYOU(2001:)    NOT = SPACE
                   COMPUTE IDX-KET    =   IDX-STR1 / 2
                   MOVE    "0018"      TO  SPA-ERRCD
                   MOVE    9           TO  SPA-GMN-CUR
                   GO  TO  431-CAUTION-SET-EXT
               END-IF
           END-IF
      *
           .
       431-CAUTION-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   薬剤色データセット処理
      *****************************************************************
       431-COLOR-SET-SEC           SECTION.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  431-COLOR-SET-EXT
           END-IF
      *
           IF      SPA-GMN-COLOR   NOT =   SPACE
      *        末尾文字編集
               MOVE    SPA-GMN-COLOR   TO  WRK-NAIYOU
               PERFORM 430-KANACHK-HENSYU-SEC
               MOVE    WRK-NAIYOU      TO  SPA-GMN-COLOR
      *        カナチェック
               IF    ( KANACHK-SYUBETU     NOT =   2    )  OR
                     ( KANACHK-RC          NOT =   ZERO )  OR
                     ( KANACHK-RC2             =   7    )
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE    6           TO  SPA-GMN-CUR
                   GO  TO  431-COLOR-SET-EXT
               END-IF
               IF      WRK-NAIYOU(181:)    NOT = SPACE
                   COMPUTE IDX-KET    =   IDX-STR1 / 2
                   MOVE    "0019"      TO  SPA-ERRCD
                   MOVE    6           TO  SPA-GMN-CUR
                   GO  TO  431-COLOR-SET-EXT
               END-IF
           END-IF
      *
           .
       431-COLOR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   薬剤形データセット処理
      *****************************************************************
       431-SHAPE-SET-SEC           SECTION.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  431-SHAPE-SET-EXT
           END-IF
      *
           IF      SPA-GMN-SHAPE   NOT =   SPACE
      *        末尾文字編集
               MOVE    SPA-GMN-SHAPE   TO  WRK-NAIYOU
               PERFORM 430-KANACHK-HENSYU-SEC
               MOVE    WRK-NAIYOU      TO  SPA-GMN-SHAPE
      *        カナチェック
               IF    ( KANACHK-SYUBETU     NOT =   2    )  OR
                     ( KANACHK-RC          NOT =   ZERO )  OR
                     ( KANACHK-RC2             =   7    )
                    MOVE    "0002"      TO  SPA-ERRCD
                    MOVE    7           TO  SPA-GMN-CUR
                    GO  TO  431-SHAPE-SET-EXT
               END-IF
               IF      WRK-NAIYOU(181:)    NOT = SPACE
                   COMPUTE IDX-KET    =   IDX-STR1 / 2
                   MOVE    "0020"      TO  SPA-ERRCD
                   MOVE    7           TO  SPA-GMN-CUR
                   GO  TO  431-SHAPE-SET-EXT
               END-IF
           END-IF
      *
           .
       431-SHAPE-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   薬剤記号データセット処理
      *****************************************************************
       431-MARK-SET-SEC            SECTION.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  431-MARK-SET-EXT
           END-IF
      *
           IF      SPA-GMN-MARK    NOT =   SPACE
      *        末尾文字編集
               MOVE    SPA-GMN-MARK    TO  WRK-NAIYOU
               PERFORM 430-KANACHK-HENSYU-SEC
               MOVE    WRK-NAIYOU      TO  SPA-GMN-MARK
      *        カナチェック
               IF    ( KANACHK-SYUBETU         =   ZERO )  OR
                     ( KANACHK-RC          NOT =   ZERO )  OR
                     ( KANACHK-RC2             =   7    )
                   MOVE    "0005"      TO  SPA-ERRCD
                   MOVE    8           TO  SPA-GMN-CUR
                   GO  TO  431-MARK-SET-EXT
               END-IF
               IF      WRK-NAIYOU(181:)    NOT = SPACE
                   COMPUTE IDX-KET    =   IDX-STR1 / 2
                   MOVE    "0021"      TO  SPA-ERRCD
                   MOVE    8           TO  SPA-GMN-CUR
                   GO  TO  431-MARK-SET-EXT
               END-IF
           END-IF
      *
           .
       431-MARK-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   薬剤形状データセット処理
      *****************************************************************
       431-KEIJYO-SET-SEC          SECTION.
      *
           IF    ( SPA-GMN-COLOR       =   SPACE ) AND
                 ( SPA-GMN-SHAPE       =   SPACE ) AND
                 ( SPA-GMN-MARK        =   SPACE )
               GO  TO  431-KEIJYO-SET-EXT
           END-IF
      *
           MOVE    1               TO  IDX-GYO
           MOVE    8               TO  GYO-MAX
           MOVE    20              TO  KOU-MAX
      *
      *    形状色チェック
           IF      SPA-GMN-COLOR   NOT =   SPACE
               MOVE    SPA-GMN-COLOR   TO  WRK-NAIYOU
      *        文字数チェック
               PERFORM 430-MOJICHK-HENSYU-SEC
               IF      WRK-NAIYOU(IDX-STR:)    NOT =   SPACE
                   MOVE    "1004"      TO  SPA-N21IDCD
                   GO  TO  431-KEIJYO-SET-EXT
               END-IF
           END-IF
      *
      *    形状形チェック
           IF      SPA-GMN-SHAPE   NOT =   SPACE
               MOVE    SPA-GMN-SHAPE    TO  WRK-NAIYOU
      *        文字数チェック
               PERFORM 430-MOJICHK-HENSYU-SEC
               IF      WRK-NAIYOU(IDX-STR:)    NOT =   SPACE
                   MOVE    "1004"      TO  SPA-N21IDCD
                   GO  TO  431-KEIJYO-SET-EXT
               END-IF
           END-IF
      *
      *    形状記号チェック
           IF      SPA-GMN-MARK   NOT =   SPACE
               MOVE    SPA-GMN-MARK    TO  WRK-NAIYOU
               PERFORM 430-KANACHK-HENSYU-SEC
      *        文字数チェック
               IF      KANACHK-SYUBETU =   1
                   MOVE    16              TO  KOU-MAX
               END-IF
               PERFORM 430-MOJICHK-HENSYU-SEC
               IF      WRK-NAIYOU(IDX-STR:)    NOT =   SPACE
                   MOVE    "1004"      TO  SPA-N21IDCD
                   GO  TO  431-KEIJYO-SET-EXT
               END-IF
           END-IF
      *
           .
       431-KEIJYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    参照処理
      *****************************************************************
       440-SANSYO-SEC              SECTION.
      *
           PERFORM 430-TEXTCHK-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               IF      SPA-ERRCD   =   "0014"
                   MOVE    SPACE       TO  SPA-ERRCD
               ELSE
                   GO  TO  440-SANSYO-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  SPA-MOTOPG
           MOVE    SPA-N21KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-N21SCRAREA  TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGN22"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-N21SCRAREA
           MOVE    SPA-WORK        TO  SPA-N21KYOUTU
      *
           MOVE    "SELNUM"        TO  MCP-WIDGET
      *
           MOVE    "N21"           TO  SPA-MOTOPG
           MOVE    "N22"           TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "N22"           TO  MCP-WINDOW
      *
      *    ＰＵＴ処理
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1               TO  FLG-END
      *
           .
       440-SANSYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    画像処理
      *****************************************************************
       440-GAZOU-SEC               SECTION.
      *
           PERFORM 430-TEXTCHK-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               IF      SPA-ERRCD   =   "0014"
                   MOVE    SPACE       TO  SPA-ERRCD
               ELSE
                   GO  TO  440-GAZOU-EXT
               END-IF
           END-IF
      *
           MOVE    SPA-N21KYOUTU   TO  SPA-WORK
           MOVE    SPA-N21SCRAREA  TO  SPA-FREE
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGN21FIL"        USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-N21SCRAREA
           MOVE    SPA-WORK        TO  SPA-N21KYOUTU
      *
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "N21"           TO  SPA-MOTOPG
           MOVE    "N21FIL"        TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "N21FIL"        TO  MCP-WINDOW
      *
      *    ＰＵＴ処理
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1               TO  FLG-END
      *
           .
       440-GAZOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    リスト印刷処理
      *****************************************************************
       450-LSTPRT-SEC              SECTION.
      *
           MOVE    SPACE           TO  LINKAREA
           MOVE    SPA-N21KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           MOVE    "N23"           TO  SPA-SAKIPG
           MOVE    "N21"           TO  SPA-MOTOPG
      *
           MOVE   "CHANGE"         TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG       TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       450-LSTPRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       460-BACK-SEC                SECTION.
      *
      *    編集中の場合、保存するか確認する
           IF      SPA-GMN-ENTRY   NOT =   SPA-NAI-ENTRY
               MOVE    "1003"          TO  SPA-N21IDCD
               MOVE    "4"             TO  SPA-GMN-EVENT
               GO  TO  460-BACK-EXT
           END-IF
      *
      *    画像添付情報削除
           INITIALIZE                  TEMP-REC
           MOVE    SPA-TERMID      TO  TEMP-TERMID
           MOVE    SPA-HOSPNUM     TO  TEMP-HOSPNUM
           MOVE    TEMP-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"      TO  MCP-FUNC
           MOVE    "tbl_yakujyo_temp"
                                   TO  MCP-TABLE
           MOVE    "termid"        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "M02"           TO  SPA-SAKIPG
           MOVE    "N21"           TO  SPA-MOTOPG
      *
           MOVE    SPACE           TO  LINKAREA
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU      TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"         TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
      *    ＰＵＴ処理
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       460-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       460-BACK-SYORI-SEC          SECTION.
      *
      *    画像添付情報削除
           INITIALIZE                  TEMP-REC
           MOVE    SPA-TERMID      TO  TEMP-TERMID
           MOVE    SPA-HOSPNUM     TO  TEMP-HOSPNUM
           MOVE    TEMP-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"      TO  MCP-FUNC
           MOVE    "tbl_yakujyo_temp"
                                   TO  MCP-TABLE
           MOVE    "termid"        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "M02"           TO  SPA-SAKIPG
           MOVE    "N21"           TO  SPA-MOTOPG
      *
           MOVE    SPACE           TO  LINKAREA
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU      TO  LNK-KYOUTU
      *
           MOVE   "JOIN"           TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
      *    ＰＵＴ処理
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       460-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       470-DEL-SEC                 SECTION.
      *
           IF      SPA-NAI-SRYCD   =   SPACE
               MOVE    "0011"      TO  SPA-ERRCD
           ELSE
               MOVE    "1002"      TO  SPA-N21IDCD
           END-IF
      *
           .
       470-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       471-DEL-SEC                 SECTION.
      *
           INITIALIZE                  YAKUJYO-REC
      *
           MOVE    SPA-HOSPNUM     TO  YAKUJYO-HOSPNUM
           MOVE    SPA-NAI-SRYCD   TO  YAKUJYO-SRYCD
           MOVE    1               TO  YAKUJYO-TEKNUM
      *
           MOVE    YAKUJYO-REC     TO  MCPDATA-REC
           MOVE    "DBDELETE"      TO  MCP-FUNC
           MOVE    "tbl_yakujyo"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1002"      TO  SPA-ERRCD
               GO  TO  471-DEL-EXT
           END-IF
      *
      *    画像削除
           PERFORM 482-GAZODEL-SEC
      *
           PERFORM 490-SYORILIST-SET-SEC
      *
           .
       471-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録前処理
      *****************************************************************
       480-TOUROKU-SEC             SECTION.
      *
      *    入力チェック
           PERFORM 430-TEXTCHK-SEC
           MOVE    SPACE               TO  SPA-N22-FLG
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           IF      SPA-GMN-ENTRY   =   SPACE
               MOVE    "0010"          TO  SPA-ERRCD
               MOVE    4               TO  SPA-GMN-CUR
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
      *    形状チェック
           PERFORM 431-KEIJYO-SET-SEC
           IF      SPA-N21IDCD      =   SPACE
               MOVE    "1001"       TO  SPA-N21IDCD
           END-IF
      *
           .
       480-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       481-KOUSHIN-SEC             SECTION.
      *
           MOVE    SPA-NAI-SRYCD       TO  WRK-SRYCD
      *    薬剤情報マスタ読み込み
           PERFORM 900-YAKUJYO-READ-SEC
      *
           IF      FLG-YAKUJYO     =   ZERO
               ACCEPT  SYS-TIME            FROM    TIME
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    SYS-TIME        TO  YAKUJYO-UPHMS
               MOVE    SPA-SYSYMD      TO  YAKUJYO-UPYMD
           ELSE
               INITIALIZE                  YAKUJYO-REC
               MOVE    "DBINSERT"      TO  MCP-FUNC
               MOVE    SPA-SYSYMD      TO  YAKUJYO-CREYMD
               MOVE    SYS-TIME        TO  YAKUJYO-UPHMS
               MOVE    SPA-SYSYMD      TO  YAKUJYO-UPYMD
           END-IF
      *
           MOVE    SPA-HOSPNUM     TO  YAKUJYO-HOSPNUM
           MOVE    SPA-NAI-SRYCD   TO  YAKUJYO-SRYCD
           MOVE    1               TO  YAKUJYO-TEKNUM
      *
           IF    ( SPA-GMN-NAME    =   SPACE ) OR
                 ( SPA-GMN-NAME    =   SPA-GMN-KENNAME )
               MOVE    SPACE           TO  YAKUJYO-NAME
           ELSE
               MOVE    SPA-GMN-NAME    TO  YAKUJYO-NAME
           END-IF
      *
           MOVE    SPA-GMN-EFFECT      TO  YAKUJYO-EFFECT
           MOVE    SPA-GMN-CAUTION     TO  YAKUJYO-CAUTION
           MOVE    SPA-GMN-PHOTO       TO  YAKUJYO-PHOTO
           MOVE    SPA-GMN-COLOR       TO  YAKUJYO-COLOR
           MOVE    SPA-GMN-SHAPE       TO  YAKUJYO-SHAPE
           MOVE    SPA-GMN-MARK        TO  YAKUJYO-MARK
      *
           MOVE    YAKUJYO-REC     TO  MCPDATA-REC
           MOVE    "tbl_yakujyo"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1001"          TO  SPA-ERRCD
               GO  TO  481-KOUSHIN-EXT
           END-IF
      *
           IF      SPA-GMN-PHOTO   =   SPACE
      *        画像削除
               PERFORM 482-GAZODEL-SEC
           ELSE
      *        画像コピー
               PERFORM 482-GAZOCOPY-SEC
           END-IF
      *
           PERFORM 490-SYORILIST-SET-SEC
      *
           .
       481-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画像コピー
      *****************************************************************
       482-GAZOCOPY-SEC            SECTION.
      *
      *    画像ファイルチェック
           PERFORM 430-GAZOCHK-HENSYU-SEC
      *
           INITIALIZE                  ORCSMIAREA
           MOVE    SPA-NAI-SRYCD   TO  ORCSMI-SRYCD
           MOVE    SPA-N22-GAZO    TO  ORCSMI-IMAGEKBN
           MOVE    MCP-TEMPDIR     TO  ORCSMI-TEMPDIR
           MOVE    WRK-FILEMEI     TO  ORCSMI-FILENAME
           CALL    "ORCSMISAVE"        USING
                                       ORCSMIAREA
                                       SPA-AREA
           .
       482-GAZOCOPY-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    画像削除
      *****************************************************************
       482-GAZODEL-SEC             SECTION.
      *
           INITIALIZE                  MEDIMAGE-REC
           MOVE    SPA-HOSPNUM     TO  MEDIMG-HOSPNUM
           MOVE    SPA-NAI-SRYCD   TO  MEDIMG-SRYCD
      *
           MOVE    MEDIMAGE-REC    TO  MCPDATA-REC
           MOVE    "DBDELETE"      TO  MCP-FUNC
           MOVE    "tbl_med_image" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MEDIMAGE-REC
                                       SPA-AREA
           .
       482-GAZODEL-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理後リストセット
      *****************************************************************
       490-SYORILIST-SET-SEC       SECTION.
      *
           IF      SPA-GMN-JYOTAIFLG   =   ZERO
               IF      SPA-GMN-SELNUM  =   ZERO
      *            画面初期化
                   PERFORM 310-SPASET-SEC
               ELSE
                   MOVE    SPA-NAI-KENCD       TO  SPA-GMN-KENCD
                   MOVE    SPA-NAI-KENNAIYO    TO  SPA-GMN-KENNAIYO
                   MOVE    ZERO                TO  SPA-GMN-SELNUM
                                                   SPA-NAI-SELNUM
                   MOVE    SPACE               TO  SPA-GMN-KENNAME
                   MOVE    3                   TO  SPA-GMN-CUR
                   PERFORM 401-CLEAR-ENTRY-SEC
               END-IF
           ELSE
               PERFORM 800-COPYLIST-SET-SEC
           END-IF
      *
           .
       490-SYORILIST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    カナボタン検索処理
      *****************************************************************
       490-KANA-SEC                SECTION.
      *
      *    複写モード、カナ以外は処理抜ける
           IF        ( SPA-GMN-JYOTAIFLG   =   1 )
               OR    ( MCP-WIDGET(1:3) NOT =   "KEN" )
               GO  TO  490-KANA-EXT
           END-IF
      *
           MOVE    ZERO            TO  SPA-NAI-PIN
           IF      MCP-WIDGET(5:)  =   SPACE
               MOVE    MCP-WIDGET(4:1) TO  SPA-NAI-PIN(2:1)
           ELSE
               MOVE    MCP-WIDGET(4:2) TO  SPA-NAI-PIN(1:2)
           END-IF
      *
           MOVE    PKANA(SPA-NAI-PIN)  TO  SPA-NAI-PKANA
      *
      *    編集中の場合確認
           IF      SPA-GMN-ENTRY   NOT =   SPA-NAI-ENTRY
               MOVE    "1003"          TO  SPA-N21IDCD
               MOVE    "3"             TO  SPA-GMN-EVENT
               GO  TO  490-KANA-EXT
           END-IF
      *
           PERFORM 490-KANAMOJI-SET-SEC
      *
           .
       490-KANA-EXT.
           EXIT.
      *
      *****************************************************************
      *    カナセット処理
      *****************************************************************
       490-KANAMOJI-SET-SEC        SECTION.
      *
      *    カナ変換
           EVALUATE    SPA-NAI-PKANA
      *        半濁音変換
               WHEN    "゜"
                   PERFORM 491-KANA-HANDAKUTEN-SEC
      *        濁音変換
               WHEN    "゛" 
                   PERFORM 491-KANA-DAKUTEN-SEC
      *        小書き変換
               WHEN    "Ｓ"
                   PERFORM 491-KANA-KOGAKI-SEC
      *        一文字後退
               WHEN    "Ｂ"
                   IF      SPA-NAI-PMAX        >   0
                       MOVE    SPACE     TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
                       COMPUTE SPA-NAI-PMAX    =   SPA-NAI-PMAX    -   1
                   END-IF
      *        クリア
               WHEN    "Ｃ"
                   MOVE    SPACE           TO  SPA-GMN-KENNAIYO
                   INITIALIZE                  SPA-NAI-PIN-AREA
                   GO  TO  490-KANAMOJI-SET-EXT
      *        通常
               WHEN    OTHER
                   COMPUTE SPA-NAI-PMAX    =   SPA-NAI-PMAX    +   1
                   MOVE    SPA-NAI-PKANA TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
           END-EVALUATE
      *
      *    １３文字以上ならエラー
           IF      SPA-NAI-PMAX        >   13
               MOVE    "0009"          TO  SPA-ERRCD
               GO  TO  490-KANAMOJI-SET-EXT
           END-IF
      *
           MOVE    SPA-NAI-PED             TO  WRK-NAME
           PERFORM 401-CLEAR-AREA-SEC
           MOVE    WRK-NAME            TO  SPA-GMN-KENNAIYO
                                           SPA-NAI-KENNAIYO
      *
      *    ２文字以上なら検索
           IF      SPA-NAI-PMAX        <   2
               GO  TO  490-KANAMOJI-SET-EXT
           END-IF
      *
           PERFORM 800-TENSULIST-SET-SEC
      *
           .
       490-KANAMOJI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    半濁点変換処理（゜）
      *****************************************************************
       491-KANA-HANDAKUTEN-SEC SECTION.
      *
      *    ゜編集
           IF      SPA-NAI-PMAX        =   0
               GO  TO  491-KANA-HANDAKUTEN-EXT
           END-IF
      *
           EVALUATE    SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "ハ"
                   MOVE    "パ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "ヒ"
                   MOVE    "ピ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "フ"
                   MOVE    "プ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "ヘ"
                   MOVE    "ペ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "ホ"
                   MOVE    "ポ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
           END-EVALUATE
      *
           .
       491-KANA-HANDAKUTEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    濁点変換処理（゛）
      *****************************************************************
       491-KANA-DAKUTEN-SEC    SECTION.
      *
      *    ゛編集
           IF      SPA-NAI-PMAX        =   0
               GO  TO  491-KANA-DAKUTEN-EXT
           END-IF
      *
           EVALUATE    SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "ウ"
                   MOVE    "ヴ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "カ"
                   MOVE    "ガ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "キ"
                   MOVE    "ギ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "ク"
                   MOVE    "グ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "ケ"
                   MOVE    "ゲ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "コ"
                   MOVE    "ゴ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "サ"
                   MOVE    "ザ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "シ"
                   MOVE    "ジ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "ス"
                   MOVE    "ズ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "セ"
                   MOVE    "ゼ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "ソ"
                   MOVE    "ゾ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "タ"
                   MOVE    "ダ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "チ"
                   MOVE    "ヂ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "ツ"
                   MOVE    "ヅ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "テ"
                   MOVE    "デ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "ト"
                   MOVE    "ド"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "ハ"
                   MOVE    "バ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "ヒ"
                   MOVE    "ビ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "フ"
                   MOVE    "ブ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "ヘ"
                   MOVE    "ベ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "ホ"
                   MOVE    "ボ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
           END-EVALUATE
      *
           .
       491-KANA-DAKUTEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    小書き変換処理
      *****************************************************************
       491-KANA-KOGAKI-SEC     SECTION.
      *
           IF      SPA-NAI-PMAX        =   0
               GO  TO  491-KANA-KOGAKI-EXT
           END-IF
      *
           EVALUATE    SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "ア"
                   MOVE    "ァ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "イ"
                   MOVE    "ィ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "ウ"
                   MOVE    "ゥ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "エ"
                   MOVE    "ェ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "オ"
                   MOVE    "ォ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "カ"
                   MOVE    "ヵ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "ツ"
                   MOVE    "ッ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "ヤ"
                   MOVE    "ャ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "ユ"
                   MOVE    "ュ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
               WHEN    "ヨ"
                   MOVE    "ョ"        TO  SPA-NAI-PEDX(SPA-NAI-PMAX)
           END-EVALUATE
      *
           .
       491-KANA-KOGAKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
      *    画面編集処理
           PERFORM 500-GMNHEN-SEC
      *
      *    エラーメッセージ表示処理
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
      *    確認メッセージ表示処理
           IF      SPA-N21IDCD     NOT =   SPACE
               PERFORM 520-N21IDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    "CURRENT"       TO  MCP-PUTTYPE.
           MOVE    "N21    "       TO  MCP-WINDOW.
      *
      *    ＰＵＴ処理
           PERFORM 900-PUT-WINDOW.
      *
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    N21-ROW         TO  WRK-ROW
      *
           MOVE    SPACE           TO  N21
      *
      *    スクロール位置セット
           INITIALIZE                  N21
           MOVE    WRK-ROW         TO  N21-ROW
           MOVE    ZERO            TO  N21-ROWATTR
      *
      *    リストセット
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-MAX
               MOVE    SPA-GMN-TNO(IDX)    TO  N21-TNO(IDX)
               MOVE    SPA-GMN-TSEL(IDX)   TO  N21-TSEL(IDX)
               MOVE    SPA-GMN-TNAME(IDX)  TO  N21-TNAME(IDX)
               IF      IDX         =   SPA-GMN-SELNUM
                   MOVE    "T"             TO  N21-SELECT(IDX)
               ELSE
                   MOVE    "F"             TO  N21-SELECT(IDX)
               END-IF
           END-PERFORM
      *
           MOVE    SPA-GMN-MAX         TO  N21-COUNT
      *
           MOVE    SPA-GMN-KENCD       TO  N21-KENCD
           MOVE    SPA-GMN-KENNAIYO    TO  N21-KENNAIYO
           MOVE    SPA-GMN-KENNAME     TO  N21-KENNAME
           MOVE    SPA-GMN-SELNUM      TO  N21-SELNUM
      *
           MOVE    SPA-GMN-NAME        TO  N21-NAME
           MOVE    SPA-GMN-EFFECT      TO  N21-EFFECT
           MOVE    SPA-GMN-COLOR       TO  N21-COLOR
           MOVE    SPA-GMN-SHAPE       TO  N21-SHAPE
           MOVE    SPA-GMN-MARK        TO  N21-MARK
           MOVE    SPA-GMN-CAUTION     TO  N21-CAUTION
      *
           MOVE    LOW-VALUE           TO  N21-PHOTOAREA
      *
      *    画像パス編集
           PERFORM 431-GAZO-PATH-CHK-SEC
           IF      FLG-OK      =   1
               MOVE    WRK-PASSMEI     TO  N21-PHOTO
           ELSE
               MOVE    WRK-FILEMEI     TO  N21-PHOTO
           END-IF
      *
      *    ファイル存在チェック
           PERFORM 431-GAZO-FILE-CHK
           IF      FLG-OK    =   0
      *        画像イメージを表示
               PERFORM 500-GAZO-OBJECT-SEC
           END-IF
      *
      *    参照ボタン非活性
           IF        ( SPA-GMN-FOLDER  =   SPACE )
               OR    ( SPA-NAI-SRYCD   =   SPACE )
               MOVE    WIDGET-INSENSITIVE  TO  N21-B09-STATE
               MOVE    WIDGET-INSENSITIVE  TO  N21-B11-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  N21-B09-STATE
               MOVE    WIDGET-NORMAL       TO  N21-B11-STATE
           END-IF
      *
      *----(05.00.00)--ADD-START---
           IF        ( SPA-NAI-YAKKAKJNCD  =   SPACE )
               MOVE    WIDGET-INSENSITIVE  TO  N21-B11-STATE
           END-IF
      *----(05.00.00)--ADD-END-----
      *
      *    複写ボタン非活性
           IF        ( SPA-GMN-JYOTAIFLG   =   ZERO )
               MOVE    SPACE               TO  N21-JYOTAI
               MOVE    "black"             TO  N21-JYOTAI-STYLE
               MOVE    "複写"              TO  N21-B08-LABEL
               IF        ( SPA-NAI-YAKUJYO     =   1 )
                   OR    ( SPA-NAI-YAKKAKJNCD  =   SPACE )
                   OR    ( SPA-NAI-ENTRY   NOT =   SPA-GMN-ENTRY )
                   MOVE    WIDGET-INSENSITIVE  TO  N21-B08-STATE
               ELSE
                   MOVE    WIDGET-NORMAL       TO  N21-B08-STATE
               END-IF
               MOVE    WIDGET-NORMAL       TO  N21-B04-STATE
           ELSE
               MOVE    "複写"              TO  N21-JYOTAI
               MOVE    "blue"              TO  N21-JYOTAI-STYLE
               MOVE    "通常"              TO  N21-B08-LABEL
               MOVE    WIDGET-NORMAL       TO  N21-B08-STATE
               MOVE    WIDGET-INSENSITIVE  TO  N21-B04-STATE
           END-IF
      *
      *    カーソルセット
           PERFORM 530-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画像表示処理
      *****************************************************************
       500-GAZO-OBJECT-SEC         SECTION.
      *
      *    画像イメージを表示
           MOVE    "BLOBIMPORT"    TO  MCP-FUNC
           MOVE    "blob"          TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       ORCA-BLOB
                                       SPA-AREA
           MOVE    ORCA-BLOB-OBJECT    TO  N21-PHOTOAREA
           .
       500-GAZO-OBJECT-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラー"    TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "全て全角で入力して下さい"  
                                           TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "全て半角で入力して下さい"  
                                           TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "全角のコードは１０文字以内で入力して下さい"
                                           TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "半角と全角の混在はできません"
                                           TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "数字のコードは４桁から６桁で入力して下さい"
                                           TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "始めがＳ、Ｐ以外のコードを入力して下さい"
                                           TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "範囲外の番号が入力されています"
                                           TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "文字数が多すぎます"
                                           TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "データを入力して下さい"
                                           TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "薬剤を指定して下さい"
                                           TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "同名の画像ファイルが存在します"
                                           TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "画像ファイルのデータが存在しません"
                                           TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "表示可能な画像ファイルではありません"
                                           TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "枠内に納まるよう編集して下さい"
                                           TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    SPACE           TO  SPA-ERRMSG
                   STRING   "効能効果：文字数超過（" DELIMITED BY SIZE
                            IDX-KET                  DELIMITED BY SIZE
                            "/0500）"                DELIMITED BY SIZE
                                           INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0018"
                   MOVE    SPACE           TO  SPA-ERRMSG
                   STRING   "注意事項：文字数超過（" DELIMITED BY SIZE
                            IDX-KET                  DELIMITED BY SIZE
                            "/1000）"                DELIMITED BY SIZE
                                           INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0019"
                   MOVE    SPACE           TO  SPA-ERRMSG
                   STRING   "色：文字数超過（"       DELIMITED BY SIZE
                            IDX-KET                  DELIMITED BY SIZE
                            "/0090）"                DELIMITED BY SIZE
                                           INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0020"
                   MOVE    SPACE           TO  SPA-ERRMSG
                   STRING   "形：文字数超過（"       DELIMITED BY SIZE
                            IDX-KET                  DELIMITED BY SIZE
                            "/0090）"                DELIMITED BY SIZE
                                           INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0021"
                   MOVE    SPACE           TO  SPA-ERRMSG
                   STRING   "記号：文字数超過（"     DELIMITED BY SIZE
                            IDX-KET                  DELIMITED BY SIZE
                            "/0090）"                DELIMITED BY SIZE
                                           INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0022"
                   MOVE    "画像ファイルのサイズが超過しています"
                                           TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "登録できませんでした"
                                           TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除できませんでした"
                                           TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  N21ERR
           MOVE    SPA-ERRCD       TO  N21ERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  N21ERR-ERRMSG
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "N21"           TO  SPA-MOTOPG
           MOVE    "N21ERR"        TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "N21ERR"        TO  MCP-WINDOW
      *
      *    ＰＵＴ処理
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示処理
      *****************************************************************
       520-N21IDSET-SEC            SECTION.
      *
           MOVE    SPACE           TO  WRK-N21IDMSG
      *
           EVALUATE    SPA-N21IDCD
               WHEN    "1001"
                   MOVE    "薬剤情報を登録します"
                                           TO  WRK-N21IDMSG
               WHEN    "1002"
                   MOVE    "薬剤情報を削除します"
                                           TO  WRK-N21IDMSG
               WHEN    "1003"
                   MOVE    "編集中ですが中止しますか？"
                                           TO  WRK-N21IDMSG
               WHEN    "1004"
                   MOVE    "色、形、記号が全部で８行を超えています"
                                           TO  WRK-N21IDMSG
               WHEN    "1005"
                   MOVE    "複写先のデータが存在しません"
                                           TO  WRK-N21IDMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  N21ID1
           INITIALIZE                  N21ID1
           MOVE    SPA-N21IDCD     TO  N21ID1-ID1CODE
           MOVE    WRK-N21IDMSG    TO  N21ID1-ID1MSG
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "N21"           TO  SPA-MOTOPG
           MOVE    "N21ID1"        TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "N21ID1"        TO  MCP-WINDOW
      *
      *    ＰＵＴ処理
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       520-N21IDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       530-MAPCUR-SEC              SECTION.
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    2
                   MOVE  "KENCD"           TO  MCP-WIDGET
               WHEN    3
                   MOVE  "SELNUM"          TO  MCP-WIDGET
               WHEN    4
                   MOVE  "NAME"            TO  MCP-WIDGET
               WHEN    5
                   MOVE  "EFFECT"          TO  MCP-WIDGET
               WHEN    6
                   MOVE  "COLOR"           TO  MCP-WIDGET
               WHEN    7
                   MOVE  "SHAPE"           TO  MCP-WIDGET
               WHEN    8
                   MOVE  "MARK"            TO  MCP-WIDGET
               WHEN    9
                   MOVE  "CAUTION"         TO  MCP-WIDGET
               WHEN    10
                   MOVE  "PHOTO"           TO  MCP-WIDGET
               WHEN    11
                   MOVE  "TEIKEI1"         TO  MCP-WIDGET
               WHEN    12
                   MOVE  "TEIKEI2"         TO  MCP-WIDGET
               WHEN    99
                   MOVE  "B12"             TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       530-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    コピー先リストセット処理
      *****************************************************************
       800-COPYLIST-SET-SEC        SECTION.
      *
      *    点数マスタ読み込み
           INITIALIZE                  TNS-TENSU-REC
           INITIALIZE                  ORCSADDSIGNAREA
           MOVE    SPA-COPY-YAKKAKJNCD(1:8)
                                   TO  SADDSIGN-STR
           CALL    "ORCSADDSIGN"       USING
                                       ORCSADDSIGNAREA
           MOVE    SADDSIGN-STR    TO  TNS-YAKKAKJNCD
           MOVE    SPA-COPY-SRYCD  TO  TNS-SRYCD
           MOVE    "key94"         TO  WRK-TNSPATH
           PERFORM 900-TENSU-SELECT-SEC
      *
      *    該当データがない場合は処理抜ける
           IF      FLG-TENSU       =   1
               MOVE    "1005"      TO  SPA-N21IDCD
               GO  TO  800-COPYLIST-SET-EXT
           END-IF
      *
           PERFORM 401-CLEAR-AREA-SEC
      *
      *    リストセット
           MOVE    ZERO            TO  IDX
           PERFORM UNTIL ( IDX     =    200 )
                   OR    ( FLG-TENSU    =   1 )
               PERFORM 800-LIST-TENSU-SET-SEC
               MOVE    TNS-SRYCD       TO  WRK-SRYCD
               PERFORM 900-YAKUJYO-READ-SEC
               IF      FLG-YAKUJYO     =   ZERO
                   MOVE    "○"        TO  SPA-GMN-TSEL(IDX)
               END-IF
               PERFORM 900-TENSU-FETCH-SEC
           END-PERFORM
      *
           PERFORM 900-TENSU-CLOSE-SEC
      *
           MOVE    IDX             TO  SPA-GMN-MAX
           MOVE    3               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-GMN-JYOTAIFLG
      *
           .
       800-COPYLIST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    リストセット処理
      *****************************************************************
       800-TENSULIST-SET-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-TBL
           MOVE    ZERO            TO  SPA-GMN-MAX
      *
      *    点数マスタ読み込み
           INITIALIZE                  TNS-TENSU-REC
           INITIALIZE                  ORCSADDSIGNAREA
           MOVE    WRK-NAME        TO  SADDSIGN-STR
           CALL    "ORCSADDSIGN"       USING
                                       ORCSADDSIGNAREA
           MOVE    SADDSIGN-STR    TO  TNS-NAME
                                       TNS-KANANAME
      *----(05.00.00)--UPD-START---
      *     MOVE    "key5"          TO  WRK-TNSPATH
           MOVE    "key2"          TO  WRK-TNSPATH
      *----(05.00.00)--UPD-END-----
           PERFORM 900-TENSU-SELECT-SEC
      *
      *    リストセット
           MOVE    ZERO            TO  IDX
           PERFORM UNTIL ( IDX     =    200 )
                   OR    ( FLG-TENSU    =   1 )
      *----(05.00.00)--ADD-START---
               EVALUATE    TNS-SRYCD
                   WHEN    "600000000" THRU    "699999999"
                   WHEN    "095210001" THRU    "095279999"
                   WHEN    "096210001" THRU    "096279999"
                       PERFORM 800-LIST-TENSU-SET-SEC
               END-EVALUATE
      *----(05.00.00)--ADD-END-----
      *----(05.00.00)--UPD-START---
      *         PERFORM 800-LIST-TENSU-SET-SEC
      *----(05.00.00)--UPD-END-----
               PERFORM 900-TENSU-FETCH-SEC
           END-PERFORM
      *
           PERFORM 900-TENSU-CLOSE-SEC
      *
           PERFORM 800-LIST-KENSU-SET-SEC
      *
           .
       800-TENSULIST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    リスト内容処理
      *****************************************************************
       800-LIST-TENSU-SET-SEC      SECTION.
      *
           COMPUTE IDX     =   IDX     +   1
           MOVE    IDX             TO  WRK-ZZZZ9
           MOVE    WRK-ZZZZ9       TO  SPA-GMN-TNO(IDX)
           MOVE    "　"            TO  SPA-GMN-TSEL(IDX)
           MOVE    TNS-NAME        TO  SPA-GMN-TNAME(IDX)
           MOVE    TNS-SRYCD       TO  SPA-GMN-TCD(IDX)
           MOVE    TNS-YAKKAKJNCD  TO  SPA-GMN-TKJNCD(IDX)
           MOVE    TNS-YKZKBN      TO  SPA-GMN-TYKZKBN(IDX)
      *
           .
       800-LIST-TENSU--SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    リスト件数処理
      *****************************************************************
       800-LIST-KENSU-SET-SEC      SECTION.
      *
           EVALUATE    IDX
               WHEN    0
                   MOVE    "該当データがありません"
                                               TO  SPA-GMN-TNAME(1)
                   MOVE    1                   TO  SPA-GMN-MAX
                   MOVE    2                   TO  SPA-GMN-CUR
               WHEN    1
                   MOVE    SPA-GMN-TNAME(IDX)  TO  SPA-GMN-KENNAME
                   MOVE    SPA-GMN-TCD(IDX)    TO  SPA-GMN-KENCD
                                                   SPA-NAI-SRYCD
                   MOVE    SPA-GMN-TKJNCD(IDX) TO  SPA-NAI-YAKKAKJNCD
                   MOVE    SPA-GMN-TYKZKBN(IDX) TO  SPA-NAI-YKZKBN
                   PERFORM 800-YAKUJYO-SET-SEC
                   INITIALIZE                      SPA-GMN-TBL
                   MOVE    ZERO                TO  SPA-GMN-MAX
               WHEN    OTHER
                   MOVE    IDX                 TO  SPA-GMN-MAX
                   MOVE    3                   TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       800-LIST-KENSU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報セット処理
      *****************************************************************
       800-YAKUJYO-SET-SEC         SECTION.
      *
           PERFORM 401-CLEAR-ENTRY-SEC
      *
           MOVE    SPA-NAI-SRYCD   TO  WRK-SRYCD
      *    薬剤情報マスタ読み込み
           PERFORM 900-YAKUJYO-READ-SEC
      *
           MOVE    FLG-YAKUJYO         TO  SPA-NAI-YAKUJYO
      *    薬剤情報セット
           IF      FLG-YAKUJYO     =   1
               MOVE    SPA-GMN-KENNAME     TO  SPA-GMN-NAME
           ELSE
               IF      YAKUJYO-NAME    =   SPACE
                   MOVE    SPA-GMN-KENNAME TO  SPA-GMN-NAME
               ELSE
                   MOVE    YAKUJYO-NAME    TO  SPA-GMN-NAME
               END-IF
               MOVE    YAKUJYO-EFFECT  TO  SPA-GMN-EFFECT
               MOVE    YAKUJYO-CAUTION TO  SPA-GMN-CAUTION
               MOVE    YAKUJYO-PHOTO   TO  SPA-GMN-PHOTO
               MOVE    YAKUJYO-COLOR   TO  SPA-GMN-COLOR
               MOVE    YAKUJYO-SHAPE   TO  SPA-GMN-SHAPE
               MOVE    YAKUJYO-MARK    TO  SPA-GMN-MARK
               PERFORM 431-GAZOCHK-SYOKI-SEC
           END-IF
      *
           IF      SPA-GMN-JYOTAIFLG   NOT =   ZERO
               MOVE    SPA-COPY-EFFECT     TO  SPA-GMN-EFFECT
               MOVE    SPA-COPY-CAUTION    TO  SPA-GMN-CAUTION
           END-IF
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    4               TO  SPA-GMN-CUR
      *
           MOVE    SPA-GMN-ENTRY   TO  SPA-NAI-ENTRY
      *
           .
       800-YAKUJYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスタ読み込み（キー）
      *****************************************************************
       900-INPUTCD-SELECT-SEC      SECTION.
      *
           INITIALIZE                  INPUTCD-REC
           MOVE    SPA-HOSPNUM     TO  ICD-HOSPNUM
           MOVE    WRK-CDSYU       TO  ICD-CDSYU
           IF      WRK-IPCPATH     =   "key"
               INITIALIZE                  ORCSADDSIGNAREA
               MOVE    SPA-GMN-KENCD   TO  SADDSIGN-STR
               MOVE    "1"             TO  SADDSIGN-KBN
               CALL    "ORCSADDSIGN"       USING
                                           ORCSADDSIGNAREA
               MOVE    SADDSIGN-STR    TO  ICD-INPUTCD
           ELSE
               MOVE    SPA-GMN-KENCD   TO  ICD-INPUTCD
           END-IF
           MOVE    INPUTCD-REC     TO  MCPDATA-REC
           MOVE    "tbl_inputcd"   TO  MCP-TABLE
           MOVE    WRK-IPCPATH     TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               PERFORM 900-INPUTCD-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-INPUTCD
           END-IF
      *
           .
       900-INPUTCD-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスタ読み込み（キー）
      *****************************************************************
       900-INPUTCD-FETCH-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM UNTIL   FLG-OK  =   1
               MOVE    "tbl_inputcd"   TO  MCP-TABLE
               MOVE    WRK-IPCPATH     TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
      *
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-INPUTCD
                   MOVE    MCPDATA-REC     TO  INPUTCD-REC
      *----(05.00.00)--UPD-START---
      *             IF      ICD-SRYCD(1:1)  =   "6"
                   EVALUATE    ICD-SRYCD
                   WHEN    "600000000" THRU    "699999999"
                   WHEN    "095210001" THRU    "095279999"
                   WHEN    "096210001" THRU    "096279999"
                       MOVE    1               TO  FLG-OK
      *                入力コード同一コードチェック
                       PERFORM VARYING IDY     FROM    1   BY  1
                               UNTIL ( IDY     >   200 )
                               OR    ( SPA-GMN-TCD(IDY)    =   SPACE )
                           IF      ICD-SRYCD       =   SPA-GMN-TCD(IDY)
                               MOVE    ZERO        TO  FLG-OK
                           END-IF
                       END-PERFORM
                   END-EVALUATE
      *----(05.00.00)--UPD-END-----
               ELSE
                   MOVE    1               TO  FLG-INPUTCD
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           .
       900-INPUTCD-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスタ読み込み（キー）
      *****************************************************************
       900-INPUTCD-CLOSE-SEC       SECTION.
      *
           MOVE    "tbl_inputcd"   TO  MCP-TABLE
           MOVE    WRK-IPCPATH     TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       900-INPUTCD-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読み込み（キー）
      *****************************************************************
       900-TENSU-SELECT-SEC        SECTION.
      *
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    SPA-SYSYMD      TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    WRK-TNSPATH     TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               PERFORM 900-TENSU-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           .
       900-TENSU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読み込み（キー）
      *****************************************************************
       900-TENSU-FETCH-SEC         SECTION.
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    WRK-TNSPATH     TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-TENSU
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           .
       900-TENSU-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読み込み（キー）
      *****************************************************************
       900-TENSU-CLOSE-SEC         SECTION.
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    WRK-TNSPATH     TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       900-TENSU-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読み込み処理
      *****************************************************************
       900-TENSU-READ-SEC          SECTION.
      *
      *----(05.00.00)--ADD-START---
           EVALUATE    WRK-SRYCD
               WHEN    "600000000" THRU    "699999999"
               WHEN    "095210001" THRU    "095279999"
               WHEN    "096210001" THRU    "096279999"
                   CONTINUE
               WHEN    OTHER
                   GO  TO  900-TENSU-READ-EXT
           END-EVALUATE
      *----(05.00.00)--ADD-END-----
      *
      *    点数マスタ読み込み
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-SYSYMD      TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-TENSU
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               ELSE
                   MOVE    1               TO  FLG-TENSU
               END-IF
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬情マスタ読み込み
      *****************************************************************
       900-YAKUJYO-READ-SEC        SECTION.
      *
           INITIALIZE                  YAKUJYO-REC
           MOVE    SPA-HOSPNUM     TO  YAKUJYO-HOSPNUM
           MOVE    WRK-SRYCD       TO  YAKUJYO-SRYCD
           MOVE    1               TO  YAKUJYO-TEKNUM
           MOVE    YAKUJYO-REC     TO  MCPDATA-REC
      *
           MOVE    "tbl_yakujyo"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_yakujyo"   TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-YAKUJYO
                   MOVE    MCPDATA-REC     TO  YAKUJYO-REC
               ELSE
                   MOVE    1               TO  FLG-YAKUJYO
               END-IF
           ELSE
               MOVE    1               TO  FLG-YAKUJYO
           END-IF
      *
           MOVE    "tbl_yakujyo"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
            .
       900-YAKUJYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYS1030-READ-SEC        SECTION.
      *
           INITIALIZE                  SYS-1030-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1030-HOSPNUM
           MOVE    "1030"          TO  SYS-1030-KANRICD
           MOVE    "*"             TO  SYS-1030-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1030-STYUKYMD
                                       SYS-1030-EDYUKYMD
           MOVE    SYS-1030-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC     TO  SYS-1030-REC
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       900-SYS1030-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読込開始処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読込検索処理
      *****************************************************************
       920-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読込終了処理
      *****************************************************************
       930-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       930-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE    "PUTWINDOW"     TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
