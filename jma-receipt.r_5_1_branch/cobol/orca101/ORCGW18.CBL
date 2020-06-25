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
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGW18.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 医療機関編集情報（Ｗ１８）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/09/03    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  02.06.01    NACL-藤原    05/08/22  区分コードの更新前チェック追加
      *  03.04.01    NACL-森脇    07/01/16  住所６→１３
      *  03.05.00    NACL-多々納  07/05/02  グループ診療対応
      *  04.01.00    NACL-森脇    07/12/21  公害レセプト追加
      *  04.02.00    NACL-森脇    08/03/17  広域連合追加、お薬手帳追加
      *  04.03.01    NACL-森脇    08/06/24  レセプト電算、症状詳記追加
      *  04.04.00    NACL-森脇    08/11/21  光ディスク等送付書追加
      *  04.06.00    NACL-多々納  10/06/28  予約票追加
      *  04.06.01    NACL-森脇    11/09/12  削除ボタン追加
      *  04.06.02    NACL-森脇    11/09/28  診療報酬請求書（労災）追加
      *
      *  04.07.01    NACL-藤原    12/05/28  タブラベル対応
      *  04.07.02    NACL-藤原    15/08/18  開設者名追加対応
      *  04.08.01    NACL-太田    16/08/04  支払証明書、自費内訳明細書追加
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
           COPY    "W01COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-W18.
           03  SPA-W18-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(03).
               05  SPA-GMN-CUR         PIC 9(03).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-STYUKYMD       PIC X(09).
                   07  SPA-GMN-EDYUKYMD       PIC X(09).
                   07  SPA-GMN-HHOSPNAME      PIC X(60).
                   07  SPA-GMN-HKAISETUNAME   PIC X(60).
                   07  SPA-GMN-HPOST          PIC X(07).
                   07  SPA-GMN-HADRS          PIC X(100).
                   07  SPA-GMN-HTEL           PIC X(15).
                   07  SPA-GMN-HFAX           PIC X(15).
                   07  SPA-GMN-PRTKBN-MAX     PIC 9(02).
      *
                   07  SPA-GMN-KBNCD-G.
                       09  SPA-GMN-KBNCD      PIC X(02).
                       09  SPA-GMN-F1         PIC X(01).
                       09  SPA-GMN-KBNCDMEI   PIC X(18).
                   07  SPA-GMN-KOUMOKU1.
                       09  SPA-GMN-HOSPNAME-G.
                           11  SPA-GMN-HOSPNAME1
                                              PIC X(80).
                           11  SPA-GMN-HOSPNAME2
                                              PIC X(80).
                           11  SPA-GMN-HOSPNAME3
                                              PIC X(80).
                       09  SPA-GMN-KAISETUNAME
                                              PIC X(80).
                       09  SPA-GMN-POST       PIC X(07).
                       09  SPA-GMN-ADRS1      PIC X(76).
                       09  SPA-GMN-ADRS2      PIC X(76).
                       09  SPA-GMN-ADRS3      PIC X(76).
                       09  SPA-GMN-TEL        PIC X(15).
                       09  SPA-GMN-FAX        PIC X(15).
      *
      *----(04.06.02)--UPD-START---
                   07  SPA-GMN-KOUMOKU2.
                       09  SPA-GMN-PRTKBN-G   OCCURS  25.
                           11  SPA-GMN-PRTKBN
                                              PIC X(02).
                           11  SPA-GMN-F1     PIC X(01).
                           11  SPA-GMN-PRTKBNMEI
                                              PIC X(18).
      *----(04.06.02)--UPD-END-----
      *                                        
                   07  SPA-GMN-KBNCDLST-G.
                       09  SPA-GMN-KBNCDLST   OCCURS  99.
                           11  SPA-GMN-KBNCDL PIC X(02).
                           11  SPA-GMN-F1L    PIC X(01).
                           11  SPA-GMN-KBNCDMEIL
                                              PIC X(18).
                       09  SPA-GMN-KBNCD-MAX  PIC 9(02).    
      *
               05  SPA-NAIB-AREA.
                   07  SPA-NAI-STYUKYMD       PIC 9(08).
                   07  SPA-NAI-EDYUKYMD       PIC 9(08).
                   07  SPA-NAI-KOUMOKU.
                       09  SPA-NAI-HOSPNAME1MOJI  PIC 9(02).
                       09  SPA-NAI-ADRS1MOJI      PIC 9(02).
                       09  SPA-NAI-KBNCD          PIC 9(02).
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYS-1901        PIC 9(01).
           03  FLG-SYS-1903        PIC 9(01).
           03  FLG-SYS-1902        PIC 9(01).
      *
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
      *    添字領域
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDXX                PIC 9(04).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-WIDMSG          PIC X(70). 
           03  WRK-YMD             PIC X(09). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-KBNCD.
               05  WRK-KBNCD9      PIC 9(02).
           03  WRK-KBNCDMEI        PIC X(18).
      *
           03  WRK-MOJISUX.
               05  WRK-MOJISU      PIC 9(02).
           03  WRK-MOJI1           PIC X(04).
           03  WRK-MOJI2           PIC X(04).
      *
           03  WRK-PAGENO          PIC S9(9)   BINARY.
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
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK1002.INC".
      *
           COPY    "CPSK1900.INC".
      *
           COPY    "CPSK1901.INC".
      *
           COPY    "CPSK1902.INC".
      *
           COPY    "CPSK1903.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
      *
           COPY    "CPORCSDAY.INC".
      *
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
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
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W18
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
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
           MOVE    SPA-W18         TO  SPA-FREE
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "WERR"
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
      *
               IF      SPA-WIDCD2      NOT =   SPACE
                   PERFORM 530-WID2SET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      * 
           MOVE    SPACE                TO  MCP-PUTTYPE
           MOVE    "W18"                TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                    TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "WID2"
                   PERFORM 330-WID2-SET-SEC
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
      *    確認画面（ＷＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID-SET-SEC            SECTION.
      *
           IF      SPA-WID1-FLG    =   "OK"
               EVALUATE    SPA-WIDCD
                   WHEN    "1005"
                       PERFORM 490-SAKUJYO-SEC
                   WHEN    "1003"
                   WHEN    "1001"
      *                登録時の医療機関名未入力の警告
                       IF      SPA-GMN-HOSPNAME-G  =   SPACE
                           MOVE    "1006"              TO  SPA-WIDCD2
                           MOVE    SPACE               TO  SPA-WID1-FLG
                           MOVE    SPACE               TO  SPA-WIDCD
                           GO  TO  330-WID-SET-EXT
                       ELSE
                           PERFORM 490-KOUSHIN-SEC
                       END-IF
                   WHEN    OTHER
                       PERFORM 490-KOUSHIN-SEC
               END-EVALUATE
      *
               EVALUATE    SPA-WIDCD
                   WHEN    "1003"
                   WHEN    "1004"
                       MOVE    "JOIN"           TO  MCP-PUTTYPE
                       PERFORM 210-BACK
                       GO  TO  330-WID-SET-EXT 
               END-EVALUATE
           ELSE
               EVALUATE    SPA-WIDCD
                   WHEN    "1001"
                   WHEN    "1003"
                   WHEN    "1005"
                       MOVE    2            TO  SPA-GMN-CUR
                   WHEN    "1002"
                   WHEN    "1004"
                       MOVE    101          TO  SPA-GMN-CUR
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD
      *
           .
       330-WID-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面２（ＷＩＤ２）ＯＫ処理
      *****************************************************************
       330-WID2-SET-SEC            SECTION.
      *
           IF      SPA-WID2-FLG    =   "OK"
               EVALUATE    SPA-WIDCD2
                   WHEN    "1003"
      *                登録時の医療機関名未入力の警告
                       IF      SPA-GMN-HOSPNAME-G  =   SPACE
                           MOVE    "1007"              TO  SPA-WIDCD2
                           MOVE    SPACE               TO  SPA-WID2-FLG
                           GO  TO  330-WID2-SET-EXT
                       ELSE
                           PERFORM 490-KOUSHIN-SEC
                       END-IF
                   WHEN    OTHER
                       PERFORM 490-KOUSHIN-SEC
               END-EVALUATE
      *
               EVALUATE    SPA-WIDCD2
                   WHEN    "1003"
                   WHEN    "1007"
                       MOVE    101          TO  SPA-GMN-CUR
                   WHEN    "1004"
                   WHEN    "1006"
                       MOVE    1            TO  SPA-GMN-CUR
               END-EVALUATE
           ELSE
               EVALUATE    SPA-WIDCD2
                   WHEN    "1003"
                       MOVE    101          TO  SPA-GMN-CUR
                   WHEN    "1004"
                   WHEN    "1006"
                   WHEN    "1007"
                       MOVE    1            TO  SPA-GMN-CUR
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID2-FLG
           MOVE    SPACE           TO  SPA-WIDCD2
           .
       330-WID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE   SPA-W18-AREA
      *
      *----(04.06.02)--UPD-START---
           MOVE    25                  TO  SPA-GMN-PRTKBN-MAX
      *----(04.06.02)--UPD-END-----
      *
           MOVE    SPA-W01-STYUKYMDH   TO  SPA-GMN-STYUKYMD
           MOVE    SPA-W01-EDYUKYMDH   TO  SPA-GMN-EDYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SPA-NAI-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SPA-NAI-EDYUKYMD
      * 
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1001-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           ELSE
               INITIALIZE                      SYS-1001-REC
           END-IF
           MOVE    SYS-1001-HOSPNAME   TO  SPA-GMN-HHOSPNAME
           MOVE    SYS-1001-KAISETUNAME
                                       TO  SPA-GMN-HKAISETUNAME
      * 
           MOVE    SPACE               TO  SYS-1002-REC
           INITIALIZE                      SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1002-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1002-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1002-REC
           ELSE
               INITIALIZE                      SYS-1002-REC
           END-IF
           MOVE    SYS-1002-POST       TO  SPA-GMN-HPOST
           MOVE    SYS-1002-ADRS       TO  SPA-GMN-HADRS
           MOVE    SYS-1002-TEL        TO  SPA-GMN-HTEL
           MOVE    SYS-1002-FAX        TO  SPA-GMN-HFAX
      *
      *    区分コード編集処理
           PERFORM 500-KBNCD-HENSYU-SEC
      * 
           MOVE    SPACE               TO  SYS-1900-REC
           INITIALIZE                      SYS-1900-REC
           MOVE    "1900"              TO  SYS-1900-KANRICD
           MOVE    "*"                 TO  SYS-1900-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1900-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1900-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1900-HOSPNUM
           MOVE    SYS-1900-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1900-REC
           ELSE
               INITIALIZE                      SYS-1900-REC
           END-IF
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-PRTKBN-MAX
               IF      SYS-1900-PRTKBN (IDX)   NOT =   SPACE
                   MOVE    SYS-1900-PRTKBN (IDX)
                                           TO  SPA-GMN-PRTKBN    (IDX)
                                               WRK-KBNCD
                   PERFORM 500-PRTKBN-HENSYU-SEC
                   MOVE    WRK-KBNCDMEI    TO  SPA-GMN-PRTKBNMEI (IDX)
               END-IF 
           END-PERFORM
      *
           MOVE    1                   TO  SPA-GMN-CUR
           . 
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 420-CLEAR-SEC
      *----(04.06.01)--ADD-START---
      *    削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 430-DEL-SEC
      *----(04.06.01)--ADD-END-----
      *    タグ切替
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 480-TAG-CHANGE-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-KOUSHIN-MAE-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        区分コード
               WHEN    "ACTIVATE"       ALSO    "KBNCD"
                   PERFORM 4100-KBNCD-CHK-SEC
      *        タグ
               WHEN    "SWITCH"        ALSO    "note3"
                   PERFORM 440-TAG-CHANGE-SEC
      *        一括分
               WHEN    "ACTIVATE"       ALSO    ANY
                   PERFORM 400-ENTRY-SYORI-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    区分コード入力処理
      *****************************************************************
       4100-KBNCD-CHK-SEC       SECTION.
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           INITIALIZE                  SPA-GMN-KBNCD-G
      *
           MOVE    W18-KBNCD       TO  SPA-GMN-KBNCD-G
      *         
           IF      SPA-GMN-KBNCD   =   SPACE
               INITIALIZE                  SPA-GMN-KOUMOKU1
                                           SPA-NAI-KOUMOKU
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE    SPA-GMN-KBNCD       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "0001"                  TO  SPA-ERRCD
                   GO  TO  4100-KBNCD-CHK-EXT
               ELSE
                   MOVE    SPACE           TO  SPA-GMN-KBNCD-G
                   MOVE    SNUM-OUTEDT(14:2)
                                           TO  SPA-GMN-KBNCD
                                               SPA-NAI-KBNCD
               END-IF
      *
               MOVE    SPA-GMN-KBNCD       TO  WRK-KBNCD
               PERFORM 500-PRTKBN-HENSYU-SEC
               MOVE    WRK-KBNCDMEI          TO  SPA-GMN-KBNCDMEI
               IF      WRK-KBNCDMEI      NOT =   SPACE
                   MOVE    SPACE               TO  SYS-1901-REC
                   INITIALIZE                      SYS-1901-REC
                   MOVE    "1901"              TO  SYS-1901-KANRICD
                   MOVE    SPA-GMN-KBNCD       TO  SYS-1901-KBNCD
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-1901-STYUKYMD
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-1901-EDYUKYMD
                   MOVE    SPA-HOSPNUM         TO  SYS-1901-HOSPNUM
                   MOVE    SYS-1901-REC        TO  MCPDATA-REC
                   PERFORM 900-SYSKANRI-INV-SEC
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC         TO  SYS-1901-REC
                   ELSE
                       INITIALIZE                      SYS-1901-REC
                   END-IF
      *
                   MOVE    SPACE               TO  SYS-1902-REC
                   INITIALIZE                      SYS-1902-REC
                   MOVE    "1902"              TO  SYS-1902-KANRICD
                   MOVE    SPA-GMN-KBNCD       TO  SYS-1902-KBNCD
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-1902-STYUKYMD
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-1902-EDYUKYMD
                   MOVE    SPA-HOSPNUM         TO  SYS-1902-HOSPNUM
                   MOVE    SYS-1902-REC        TO  MCPDATA-REC
                   PERFORM 900-SYSKANRI-INV-SEC
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC         TO  SYS-1902-REC
                   ELSE
                       INITIALIZE                      SYS-1902-REC
                   END-IF
      *
                   MOVE    SPACE               TO  SYS-1903-REC
                   INITIALIZE                      SYS-1903-REC
                   MOVE    "1903"              TO  SYS-1903-KANRICD
                   MOVE    SPA-GMN-KBNCD       TO  SYS-1903-KBNCD
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-1903-STYUKYMD
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-1903-EDYUKYMD
                   MOVE    SPA-HOSPNUM         TO  SYS-1903-HOSPNUM
                   MOVE    SYS-1903-REC        TO  MCPDATA-REC
                   PERFORM 900-SYSKANRI-INV-SEC
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC         TO  SYS-1903-REC
                   ELSE
                       INITIALIZE                      SYS-1903-REC
                   END-IF
      *
                   MOVE    SYS-1901-HOSPNAME1  TO  SPA-GMN-HOSPNAME1
                   MOVE    SYS-1901-HOSPNAME2  TO  SPA-GMN-HOSPNAME2
                   MOVE    SYS-1901-HOSPNAME3  TO  SPA-GMN-HOSPNAME3
                   MOVE    SYS-1901-HOSPNAME1MOJI
                                               TO  SPA-NAI-HOSPNAME1MOJI
                   MOVE    SYS-1902-POST       TO  SPA-GMN-POST
                   MOVE    SYS-1902-ADRS1      TO  SPA-GMN-ADRS1
                   MOVE    SYS-1902-ADRS2      TO  SPA-GMN-ADRS2
                   MOVE    SYS-1902-ADRS3      TO  SPA-GMN-ADRS3
                   MOVE    SYS-1902-TEL        TO  SPA-GMN-TEL
                   MOVE    SYS-1902-FAX        TO  SPA-GMN-FAX
                   MOVE    SYS-1902-ADRS1MOJI  TO  SPA-NAI-ADRS1MOJI
                   MOVE    SYS-1903-KAISETUNAME
                                               TO  SPA-GMN-KAISETUNAME
               END-IF
           END-IF
      *
           MOVE    2                       TO  SPA-GMN-CUR
           .
       4100-KBNCD-CHK-EXT.
      *
      *****************************************************************
      *    一括分　入力処理
      *****************************************************************
       400-ENTRY-SYORI-SEC       SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
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
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
      *    更新前チェック処理
           IF      FLG-TOUROKU         =   1   OR  2
               PERFORM 4103-KAKUTEI-CHK-SEC
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
           MOVE    W18-HOSPNAME1       TO  SPA-GMN-HOSPNAME1
           MOVE    W18-HOSPNAME2       TO  SPA-GMN-HOSPNAME2
           MOVE    W18-HOSPNAME3       TO  SPA-GMN-HOSPNAME3
           MOVE    W18-KAISETUNAME     TO  SPA-GMN-KAISETUNAME
           MOVE    W18-POST            TO  SPA-GMN-POST
           MOVE    W18-ADRS1           TO  SPA-GMN-ADRS1
           MOVE    W18-ADRS2           TO  SPA-GMN-ADRS2
           MOVE    W18-ADRS3           TO  SPA-GMN-ADRS3
           MOVE    W18-TEL             TO  SPA-GMN-TEL
           MOVE    W18-FAX             TO  SPA-GMN-FAX
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-PRTKBN-MAX
               MOVE    W18-PRTKBN (IDX)    TO  SPA-GMN-PRTKBN-G (IDX)
           END-PERFORM
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
           PERFORM 41021-HOSPNAME1-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41022-HOSPNAME2-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41023-HOSPNAME3-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41024-KAISETUNAME-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41025-ADRS1-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41026-ADRS2-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41027-ADRS3-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *----(03.04.02)--ADD-START---
           PERFORM 41028-PRTKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *----(03.04.02)--ADD-END-----
      *
      *----(04.03.01)--ADD-START---
           PERFORM 41029-RECEDEN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *----(04.03.01)--ADD-END-----
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    名称１チェック
      *****************************************************************
       41021-HOSPNAME1-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-HOSPNAME1MOJI
      *
           IF      SPA-GMN-HOSPNAME1   =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-HOSPNAME1   TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-RC      NOT =   ZERO  )  OR
                      (KANACHK-SYUBETU     =   1     )
                   MOVE    "0002"          TO  SPA-ERRCD
                   MOVE    2               TO  SPA-GMN-CUR
               ELSE
                   MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                           TO  SPA-GMN-HOSPNAME1
                   COMPUTE SPA-NAI-HOSPNAME1MOJI
                                           =   KANACHK-MAX /   2 
               END-IF
           END-IF 
      *
      *----(04.03.01)--ADD-START---
           IF    ( SPA-NAI-HOSPNAME1MOJI   =   ZERO )  OR
                 ( SPA-NAI-HOSPNAME1MOJI   >   20 )
               IF    ( SPA-GMN-PRTKBN(19)  NOT =   SPACE ) AND
                     ( SPA-GMN-PRTKBN(19)  =   SPA-GMN-KBNCD )
                   MOVE    "0011"          TO  SPA-ERRCD
                   MOVE    2               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *----(04.03.01)--ADD-END-----
      *
           .
       41021-HOSPNAME1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    名称２チェック
      *****************************************************************
       41022-HOSPNAME2-CHK-SEC         SECTION.
      *
           IF      SPA-GMN-HOSPNAME2   =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-HOSPNAME2   TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-RC      NOT =   ZERO  )  OR
                      (KANACHK-SYUBETU     =   1     )
                   MOVE    "0003"          TO  SPA-ERRCD
                   MOVE    3               TO  SPA-GMN-CUR
               ELSE
                   MOVE    KANACHK-OUT-INPUT
                                           TO  SPA-GMN-HOSPNAME2
               END-IF
           END-IF 
      *
           .
       41022-HOSPNAME2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    名称３チェック
      *****************************************************************
       41023-HOSPNAME3-CHK-SEC         SECTION.
      *
           IF      SPA-GMN-HOSPNAME3   =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-HOSPNAME3   TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-RC      NOT =   ZERO  )  OR
                      (KANACHK-SYUBETU     =   1     )
                   MOVE    "0004"          TO  SPA-ERRCD
                   MOVE    4               TO  SPA-GMN-CUR
               ELSE
                   MOVE    KANACHK-OUT-INPUT
                                           TO  SPA-GMN-HOSPNAME3
               END-IF
           END-IF 
      *
           .
       41023-HOSPNAME3-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    開設者名チェック
      *****************************************************************
       41024-KAISETUNAME-CHK-SEC         SECTION.
      *
           IF      SPA-GMN-KAISETUNAME =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-KAISETUNAME TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-RC      NOT =   ZERO  )  OR
                      (KANACHK-SYUBETU     =   1     )
                   MOVE    "0008"              TO  SPA-ERRCD
                   MOVE    11                  TO  SPA-GMN-CUR
               ELSE
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-KAISETUNAME
               END-IF
           END-IF 
      *
           .
       41024-KAISETUNAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所１チェック処理
      *****************************************************************
       41025-ADRS1-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-ADRS1MOJI
      *
           IF      SPA-GMN-ADRS1       =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-ADRS1       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-RC      NOT =   ZERO  )  OR
                      (KANACHK-SYUBETU     =   1     )
                   MOVE    "0005"          TO  SPA-ERRCD
                   MOVE    5               TO  SPA-GMN-CUR
               ELSE
                   MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                           TO  SPA-GMN-ADRS1
                   COMPUTE SPA-NAI-ADRS1MOJI
                                           =   KANACHK-MAX /   2
               END-IF
           END-IF    
           . 
       41025-ADRS1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所２チェック処理
      *****************************************************************
       41026-ADRS2-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-ADRS2       =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-ADRS2       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-RC      NOT =   ZERO  )  OR
                      (KANACHK-SYUBETU     =   1     )
                   MOVE    "0006"          TO  SPA-ERRCD
                   MOVE    6               TO  SPA-GMN-CUR
               ELSE
                   MOVE    KANACHK-OUT-INPUT
                                           TO  SPA-GMN-ADRS2
               END-IF
           END-IF    
     *
           . 
       41026-ADRS2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所３チェック処理
      *****************************************************************
       41027-ADRS3-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-ADRS3       =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-ADRS3       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-RC      NOT =   ZERO  )  OR
                      (KANACHK-SYUBETU     =   1     )
                   MOVE    "0007"          TO  SPA-ERRCD
                   MOVE    7               TO  SPA-GMN-CUR
               ELSE
                   MOVE    KANACHK-OUT-INPUT
                                           TO  SPA-GMN-ADRS3
               END-IF
           END-IF    
     *
           . 
       41027-ADRS3-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票別指定チェック
      *****************************************************************
       41028-PRTKBN-CHK-SEC       SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-PRTKBN-MAX
                   OR      SPA-ERRCD   NOT =   SPACE
               IF      SPA-GMN-PRTKBN (IDX)    =   SPACE
                   MOVE    SPACE           TO  SPA-GMN-PRTKBNMEI (IDX)
               ELSE
                   MOVE    SPA-GMN-PRTKBN (IDX)    TO  WRK-KBNCD
                   PERFORM 500-PRTKBN-HENSYU-SEC
                   MOVE    WRK-KBNCDMEI    TO  SPA-GMN-PRTKBNMEI (IDX)
      *     
                   IF      SPA-GMN-PRTKBNMEI (IDX) =   SPACE
                       COMPUTE SPA-GMN-CUR =   100 +   IDX    
                       MOVE    "0010"                  TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM   
      *
           .
       41028-PRTKBN-CHK-EXT.
           EXIT.
      *
      *----(04.03.01)--ADD-START---
      *****************************************************************
      *    レセ電チェック
      *****************************************************************
       41029-RECEDEN-CHK-SEC       SECTION.
      *
          IF      SPA-GMN-PRTKBN(19)   NOT =   SPACE
               MOVE    SPACE               TO  SYS-1901-REC
               INITIALIZE                      SYS-1901-REC
               MOVE    "1901"              TO  SYS-1901-KANRICD
               MOVE    SPA-GMN-PRTKBN(19)  TO  SYS-1901-KBNCD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-1901-STYUKYMD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-1901-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-1901-HOSPNUM
               MOVE    SYS-1901-REC        TO  MCPDATA-REC
               PERFORM 900-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1901-REC
               ELSE
                   INITIALIZE                      SYS-1901-REC
               END-IF
               IF    ( SYS-1901-HOSPNAME1MOJI  =   0 ) OR
                     ( SYS-1901-HOSPNAME1MOJI  >   20 )
                   COMPUTE SPA-GMN-CUR =   100 +   19
                   MOVE    "0011"                  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       41029-RECEDEN-CHK-EXT.
           EXIT.
      *
      *----(04.03.01)--ADD-END-----
      *****************************************************************
      *    更新前チェック処理
      *****************************************************************
       4103-KAKUTEI-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-KBNCD       =   SPACE )
           AND   ( SPA-GMN-KOUMOKU1
                                   NOT =   SPACE )
               MOVE    "0100"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR 
           END-IF           
      *
           IF      SPA-ERRCD           =   SPACE
               IF    ( SPA-GMN-KBNCD   NOT =   SPACE )
               AND   ( SPA-NAI-KBNCD       =   ZERO  )
                   MOVE    "0001"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR 
               END-IF           
           END-IF
           .
       4103-KAKUTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    タグ編集処理
      *****************************************************************
       440-TAG-CHANGE-SEC            SECTION.
      *
           MOVE   W18-PAGENO           TO  WRK-PAGENO
      *
      *    タグが変わったとき項目チェック
      *****IF      IDX              NOT =   W18-PAGENO
               MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
               MOVE    ZERO                TO  SPA-GMN-CUR
      *
      *        入力チェック処理
               MOVE    2                   TO  FLG-TOUROKU
               PERFORM 410-INPUT-CHK-SEC
               IF      SPA-ERRCD            =   SPACE
                   EVALUATE    WRK-PAGENO
                       WHEN    0
                           IF      SPA-GMN-KOUMOKU2 =   SPACE
                               MOVE    1            TO  SPA-GMN-CUR
                           ELSE
                               IF  (   SPA-W01-SELNUM
                                                    >   ZERO           )
                               AND ( ( SPA-W01-STYUKYMD
                                        NOT =   SPA-W01-SEL-STYUKYMD )
                                  OR ( SPA-W01-EDYUKYMD
                                        NOT =   SPA-W01-SEL-EDYUKYMD ) )
                                   MOVE    "1004"       TO  SPA-WIDCD
                               ELSE
                                   MOVE    "1004"       TO  SPA-WIDCD2
                               END-IF
                           END-IF
                       WHEN    1
                           IF      SPA-GMN-KBNCD    =   SPACE
                               MOVE    101          TO  SPA-GMN-CUR
                           ELSE 
                               IF  (   SPA-W01-SELNUM
                                                    >   ZERO           )
                               AND ( ( SPA-W01-STYUKYMD
                                        NOT =   SPA-W01-SEL-STYUKYMD )
                                  OR ( SPA-W01-EDYUKYMD
                                        NOT =   SPA-W01-SEL-EDYUKYMD ) )
                                   MOVE    "1003"       TO  SPA-WIDCD
                               ELSE
                                   MOVE    "1003"       TO  SPA-WIDCD2
                               END-IF
                           END-IF 
                   END-EVALUATE
               END-IF
      *****END-IF        
           .
       440-TAG-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W18"               TO  SPA-MOTOPG
      *
      *     INITIALIZE                  SPA-W01KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
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
      *    クリア処理
      *****************************************************************
       420-CLEAR-SEC                   SECTION.
      *
           INITIALIZE                  SPA-GMN-KOUMOKU1
                                       SPA-GMN-KBNCD-G
                                       SPA-NAI-KOUMOKU
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *
      *----(04.06.01)--ADD-START---
      *****************************************************************
      *    削除処理
      *****************************************************************
       430-DEL-SEC                 SECTION.
      *
           IF    ( SPA-GMN-KBNCD   =   SPACE )
               MOVE    "0102"          TO  SPA-ERRCD
           ELSE
               MOVE    "1005"          TO  SPA-WIDCD
           END-IF                      
      *
           .
       430-DEL-EXT.
           EXIT.
      *
      *----(04.06.01)--ADD-END-----
      *****************************************************************
      *    タグ切替処理
      *****************************************************************
       480-TAG-CHANGE-SEC              SECTION.
      *
      *    入力チェック処理
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    2                   TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD            =   SPACE
               EVALUATE    W18-PAGENO
                   WHEN    1
                       IF      SPA-GMN-KOUMOKU2 =   SPACE
                           MOVE    1            TO  SPA-GMN-CUR
                       ELSE 
                           IF  (   SPA-W01-SELNUM   >   ZERO           )
                           AND ( ( SPA-W01-STYUKYMD
                                        NOT =   SPA-W01-SEL-STYUKYMD )
                              OR ( SPA-W01-EDYUKYMD
                                        NOT =   SPA-W01-SEL-EDYUKYMD ) )
                               MOVE    "1004"       TO  SPA-WIDCD
                           ELSE
                               MOVE    "1004"       TO  SPA-WIDCD2
                           END-IF
                       END-IF    
                   WHEN    0
                       IF      SPA-GMN-KBNCD    =   SPACE
                           MOVE    101          TO  SPA-GMN-CUR
                       ELSE 
                           IF  (   SPA-W01-SELNUM   >   ZERO           )
                           AND ( ( SPA-W01-STYUKYMD
                                        NOT =   SPA-W01-SEL-STYUKYMD )
                              OR ( SPA-W01-EDYUKYMD
                                        NOT =   SPA-W01-SEL-EDYUKYMD ) )
                               MOVE    "1003"       TO  SPA-WIDCD
                           ELSE
                               MOVE    "1003"       TO  SPA-WIDCD2
                           END-IF
                       END-IF 
               END-EVALUATE
           END-IF        
      *
           .
       480-TAG-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
       490-KOUSHIN-MAE-SEC              SECTION.
      *
      *    入力チェック処理
           MOVE    1                   TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
              GO      TO      490-KOUSHIN-MAE-EXT
           END-IF
      *
           EVALUATE   W18-PAGENO
               WHEN   0
                   IF    ( SPA-GMN-KBNCD   =   SPACE )
                   AND   ( SPA-GMN-KOUMOKU1 
                                           =   SPACE )
                       MOVE    "0101"          TO  SPA-ERRCD
                   END-IF                      
           END-EVALUATE
           IF      SPA-ERRCD       NOT =   SPACE
              GO      TO      490-KOUSHIN-MAE-EXT
           END-IF
      *
           IF  (   SPA-W01-SELNUM          >   ZERO                   )
           AND ( ( SPA-W01-STYUKYMD    NOT =   SPA-W01-SEL-STYUKYMD )
              OR ( SPA-W01-EDYUKYMD    NOT =   SPA-W01-SEL-EDYUKYMD ) ) 
               EVALUATE   W18-PAGENO
                   WHEN   0
                       MOVE    "1003"              TO  SPA-WIDCD
                   WHEN   1
                       MOVE    "1004"              TO  SPA-WIDCD
               END-EVALUATE
           ELSE
               EVALUATE   W18-PAGENO
                   WHEN   0
                       MOVE    "1001"              TO  SPA-WIDCD
                   WHEN   1
                       MOVE    "1002"              TO  SPA-WIDCD
               END-EVALUATE
           END-IF 
           .
       490-KOUSHIN-MAE-EXT.
           EXIT.
      *
      *----(04.06.01)--ADD-START---
      *****************************************************************
      *       削除処理
      *****************************************************************
       490-SAKUJYO-SEC             SECTION.
      *
      *    医療機関編集情報−基本情報
           MOVE    SPACE               TO  SYS-1901-REC
           INITIALIZE                      SYS-1901-REC
           MOVE    "1901"              TO  SYS-1901-KANRICD
           MOVE    SPA-GMN-KBNCD       TO  SYS-1901-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1901-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1901-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1004"              TO  SPA-ERRCD
               GO  TO  490-SAKUJYO-EXT
           END-IF
      *
      *    医療機関編集情報−基本情報（２）
           MOVE    SPACE               TO  SYS-1903-REC
           INITIALIZE                      SYS-1903-REC
           MOVE    "1903"              TO  SYS-1903-KANRICD
           MOVE    SPA-GMN-KBNCD       TO  SYS-1903-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1903-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1903-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1903-HOSPNUM
           MOVE    SYS-1903-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1004"              TO  SPA-ERRCD
               GO  TO  490-SAKUJYO-EXT
           END-IF
      *
      *    医療機関編集情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1902-REC
           INITIALIZE                      SYS-1902-REC
           MOVE    "1902"              TO  SYS-1902-KANRICD
           MOVE    SPA-GMN-KBNCD       TO  SYS-1902-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1902-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1006"              TO  SPA-ERRCD
               GO  TO  490-SAKUJYO-EXT
           END-IF
      *
      *    使用状況チェック
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-PRTKBN-MAX
               IF      SPA-GMN-PRTKBN (IDX)    =   SPA-GMN-KBNCD
                   MOVE    1               TO  FLG-CHK
                   MOVE    SPACE           TO  SPA-GMN-PRTKBN (IDX)
               END-IF
           END-PERFORM
      *
          IF      FLG-CHK              NOT =   ZERO
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-PRTKBN-MAX
               IF      SPA-GMN-PRTKBN (IDX)    NOT =   SPACE
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
      *
      *    帳票別指定情報
           MOVE    SPACE               TO  SYS-1900-REC
           INITIALIZE                      SYS-1900-REC
           MOVE    "1900"              TO  SYS-1900-KANRICD
           MOVE    "*"                 TO  SYS-1900-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1900-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1900-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1900-HOSPNUM
           MOVE    SYS-1900-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1900-REC
               IF      FLG-CHK             =   ZERO
                   MOVE    SYS-1900-REC        TO  SYSKANRI-REC
                   MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                   MOVE    SYSKANRI-REC        TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC              NOT =   ZERO 
                       MOVE    "1002"              TO  SPA-ERRCD
                       GO  TO  490-SAKUJYO-EXT
                   END-IF
               ELSE
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       SPA-GMN-PRTKBN-MAX
                       MOVE    SPA-GMN-PRTKBN (IDX)
                                               TO  SYS-1900-PRTKBN (IDX)
                   END-PERFORM
                   MOVE    SYS-1900-REC        TO  SYSKANRI-REC
                   MOVE    SMCNDATE-YMD        TO  SYS-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
                   MOVE    SYS-KANRICD         TO  SYSUP-KANRICD
                   MOVE    SYS-KBNCD           TO  SYSUP-KBNCD
                   MOVE    SPA-W01-STYUKYMD    TO  SYSUP-STYUKYMD
                   MOVE    SPA-W01-EDYUKYMD    TO  SYSUP-EDYUKYMD
                   MOVE    SPA-HOSPNUM         TO  SYSUP-HOSPNUM
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "upd1"              TO  MCP-PATHNAME
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                   MOVE    SYSKANRI-REC        TO  MCPDATA-REC
                   CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC              NOT =   ZERO
                       MOVE    "1001"              TO  SPA-ERRCD
                       GO  TO  490-SAKUJYO-EXT
                   END-IF
               END-IF
           END-IF
           END-IF
      *
      *    区分コード編集処理
           PERFORM 500-KBNCD-HENSYU-SEC
      *
      *    項目クリア
           INITIALIZE                  SPA-GMN-KOUMOKU1
                                       SPA-GMN-KBNCD-G
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       490-SAKUJYO-EXT.
           EXIT.
      *----(04.06.01)--ADD-END-----
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
           EVALUATE    SPA-WIDCD    ALSO    SPA-WIDCD2
               WHEN    "1003"       ALSO    ANY
               WHEN    "1001"       ALSO    ANY
               WHEN    ANY          ALSO    "1003"
               WHEN    ANY          ALSO    "1006"
               WHEN    ANY          ALSO    "1007"
      *            医療機関編集情報
                   IF      SPA-GMN-KBNCD       =   SPACE
                       CONTINUE
                   ELSE 
      *                医療機関編集情報−基本情報
                       MOVE    SPACE               TO  SYS-1901-REC
                       INITIALIZE                      SYS-1901-REC
                       MOVE    "1901"              TO  SYS-1901-KANRICD
                       MOVE    SPA-GMN-KBNCD       TO  SYS-1901-KBNCD
                       MOVE    SPA-W01-STYUKYMD
                                                   TO  SYS-1901-STYUKYMD
                       MOVE    SPA-W01-STYUKYMD
                                                   TO  SYS-1901-EDYUKYMD
                       MOVE    SPA-HOSPNUM         TO  SYS-1901-HOSPNUM
                       MOVE    SYS-1901-REC        TO  MCPDATA-REC
                       PERFORM 900-SYSKANRI-INV-SEC
                       IF      FLG-SYSKANRI        =   ZERO
                           MOVE    MCPDATA-REC     TO  SYS-1901-REC
                       ELSE
                           MOVE    SPACE           TO  SYS-1901-REC
                           INITIALIZE                  SYS-1901-REC
                           MOVE    "1901"          TO  SYS-1901-KANRICD
                           MOVE    SPA-GMN-KBNCD   TO  SYS-1901-KBNCD
                       END-IF
      *
                       MOVE    SPA-W01-STYUKYMD    TO  SYS-1901-STYUKYMD
                       MOVE    SPA-W01-EDYUKYMD    TO  SYS-1901-EDYUKYMD
                       MOVE    SPA-GMN-HOSPNAME1   TO
                                                   SYS-1901-HOSPNAME1
                       MOVE    SPA-GMN-HOSPNAME2   TO
                                                   SYS-1901-HOSPNAME2
                       MOVE    SPA-GMN-HOSPNAME3   TO
                                                   SYS-1901-HOSPNAME3
                       MOVE    SPA-NAI-HOSPNAME1MOJI   TO
                                               SYS-1901-HOSPNAME1MOJI
                       MOVE    SYS-1901-REC        TO  SYSKANRI-REC
                       PERFORM 800-MCNDATE-SEC
                       IF      FLG-SYSKANRI        =   ZERO
                           MOVE    SMCNDATE-YMD    TO  SYS-UPYMD
                           MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
                           MOVE    SYS-KANRICD     TO  SYSUP-KANRICD
                           MOVE    SYS-KBNCD       TO  SYSUP-KBNCD
                           MOVE    SPA-W01-STYUKYMD
                                                   TO  SYSUP-STYUKYMD
                           MOVE    SPA-W01-EDYUKYMD
                                                   TO  SYSUP-EDYUKYMD
                           MOVE    SPA-HOSPNUM     TO  SYSUP-HOSPNUM
                           MOVE    "tbl_syskanri"  TO  MCP-TABLE
                           MOVE    "upd1"          TO  MCP-PATHNAME
                           MOVE    "DBUPDATE"      TO  MCP-FUNC
                       ELSE
                           MOVE    SMCNDATE-YMD    TO  SYS-CREYMD
                                                       SYS-UPYMD
                           MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
                           MOVE    "tbl_syskanri"  TO  MCP-TABLE
                           MOVE    "key"           TO  MCP-PATHNAME
                           MOVE    "DBINSERT"      TO  MCP-FUNC
                       END-IF
                       MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                       MOVE    SYSKANRI-REC        TO  MCPDATA-REC
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                       IF      MCP-RC              NOT =   ZERO 
                           MOVE    "1003"              TO  SPA-ERRCD
                           GO  TO  490-KOUSHIN-EXT
                       END-IF
      *
      *                医療機関編集情報−基本情報（２）
                       MOVE    SPACE               TO  SYS-1903-REC
                       INITIALIZE                      SYS-1903-REC
                       MOVE    "1903"              TO  SYS-1903-KANRICD
                       MOVE    SPA-GMN-KBNCD       TO  SYS-1903-KBNCD
                       MOVE    SPA-W01-STYUKYMD
                                                   TO  SYS-1903-STYUKYMD
                       MOVE    SPA-W01-STYUKYMD
                                                   TO  SYS-1903-EDYUKYMD
                       MOVE    SPA-HOSPNUM         TO  SYS-1903-HOSPNUM
                       MOVE    SYS-1903-REC        TO  MCPDATA-REC
                       PERFORM 900-SYSKANRI-INV-SEC
                       IF      FLG-SYSKANRI        =   ZERO
                           MOVE    MCPDATA-REC     TO  SYS-1903-REC
                       ELSE
                           MOVE    SPACE           TO  SYS-1903-REC
                           INITIALIZE                  SYS-1903-REC
                           MOVE    "1903"          TO  SYS-1903-KANRICD
                           MOVE    SPA-GMN-KBNCD   TO  SYS-1903-KBNCD
                       END-IF
      *
                       MOVE    ZERO                TO  FLG-CHK
                       IF      SPA-GMN-KAISETUNAME NOT =   SPACE
                           MOVE    1                   TO  FLG-CHK
                       END-IF
      *  
                       IF      FLG-CHK             =   ZERO
                           IF      FLG-SYSKANRI        =   ZERO
                               MOVE    SYS-1903-REC    TO  MCPDATA-REC
                               MOVE    "DBDELETE"      TO  MCP-FUNC
                               MOVE    "tbl_syskanri"  TO  MCP-TABLE
                               MOVE    "key"           TO  MCP-PATHNAME
                               CALL    "ORCDBMAIN"     USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                               IF      MCP-RC          NOT =   ZERO 
                                   MOVE    "1006"          TO  SPA-ERRCD
                                   GO  TO  490-KOUSHIN-EXT
                                END-IF
                           END-IF    
                       ELSE
                           MOVE    SPA-W01-STYUKYMD
                                                   TO  SYS-1903-STYUKYMD
                           MOVE    SPA-W01-EDYUKYMD
                                                   TO  SYS-1903-EDYUKYMD
                           MOVE    SPA-GMN-KAISETUNAME
                                                   TO
                                                   SYS-1903-KAISETUNAME
                           MOVE    SYS-1903-REC    TO  SYSKANRI-REC
                           PERFORM 800-MCNDATE-SEC
                           IF      FLG-SYSKANRI        =   ZERO
                               MOVE    SMCNDATE-YMD    TO  SYS-UPYMD
                               MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
                               MOVE    SYS-KANRICD     TO  SYSUP-KANRICD
                               MOVE    SYS-KBNCD       TO  SYSUP-KBNCD
                               MOVE    SPA-W01-STYUKYMD
                                                       TO SYSUP-STYUKYMD
                               MOVE    SPA-W01-EDYUKYMD
                                                       TO SYSUP-EDYUKYMD
                               MOVE    SPA-HOSPNUM     TO  SYSUP-HOSPNUM
                               MOVE    "tbl_syskanri"  TO  MCP-TABLE
                               MOVE    "upd1"          TO  MCP-PATHNAME
                               MOVE    "DBUPDATE"      TO  MCP-FUNC
                           ELSE
                               MOVE    SMCNDATE-YMD    TO  SYS-CREYMD
                                                           SYS-UPYMD
                               MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
                               MOVE    "tbl_syskanri"  TO  MCP-TABLE
                               MOVE    "key"           TO  MCP-PATHNAME
                               MOVE    "DBINSERT"      TO  MCP-FUNC
                           END-IF
                           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
grpsys                     CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                           IF      MCP-RC              NOT =   ZERO 
                               MOVE    "1005"              TO  SPA-ERRCD
                               GO  TO  490-KOUSHIN-EXT
                           END-IF
                       END-IF
      *
      *                医療機関編集情報−所在地、連絡先
                       MOVE    SPACE               TO  SYS-1902-REC
                       INITIALIZE                      SYS-1902-REC
                       MOVE    "1902"              TO  SYS-1902-KANRICD
                       MOVE    SPA-GMN-KBNCD       TO  SYS-1902-KBNCD
                       MOVE    SPA-W01-STYUKYMD
                                                   TO  SYS-1902-STYUKYMD
                       MOVE    SPA-W01-STYUKYMD
                                                   TO  SYS-1902-EDYUKYMD
                       MOVE    SPA-HOSPNUM         TO  SYS-1902-HOSPNUM
                       MOVE    SYS-1902-REC        TO  MCPDATA-REC
                       PERFORM 900-SYSKANRI-INV-SEC
                       IF      FLG-SYSKANRI        =   ZERO
                           MOVE    MCPDATA-REC     TO  SYS-1902-REC
                       ELSE
                           MOVE    SPACE           TO  SYS-1902-REC
                           INITIALIZE                  SYS-1902-REC
                           MOVE    "1902"          TO  SYS-1902-KANRICD
                           MOVE    SPA-GMN-KBNCD   TO  SYS-1902-KBNCD
                       END-IF
      *
                       MOVE    ZERO                TO  FLG-CHK
                       IF    ( SPA-GMN-POST        NOT =   SPACE )
                       OR    ( SPA-GMN-ADRS1       NOT =   SPACE )
                       OR    ( SPA-GMN-ADRS2       NOT =   SPACE )
                       OR    ( SPA-GMN-ADRS3       NOT =   SPACE )
                       OR    ( SPA-GMN-TEL         NOT =   SPACE )
                       OR    ( SPA-GMN-FAX         NOT =   SPACE )
                           MOVE    1                   TO  FLG-CHK
                       END-IF
      *  
                       IF      FLG-CHK             =   ZERO
                           IF      FLG-SYSKANRI        =   ZERO
                               MOVE    SYS-1902-REC    TO  MCPDATA-REC
                               MOVE    "DBDELETE"      TO  MCP-FUNC
                               MOVE    "tbl_syskanri"  TO  MCP-TABLE
                               MOVE    "key"           TO  MCP-PATHNAME
grpsys                         CALL    "ORCDBMAIN"     USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                               IF      MCP-RC          NOT =   ZERO 
                                   MOVE    "1006"          TO  SPA-ERRCD
                                   GO  TO  490-KOUSHIN-EXT
                                END-IF
                           END-IF    
                       ELSE
                           MOVE    SPA-W01-STYUKYMD
                                                   TO  SYS-1902-STYUKYMD
                           MOVE    SPA-W01-EDYUKYMD
                                                   TO  SYS-1902-EDYUKYMD
                           MOVE    SPA-GMN-POST    TO  SYS-1902-POST
                           MOVE    SPA-GMN-ADRS1   TO  SYS-1902-ADRS1
                           MOVE    SPA-GMN-ADRS2   TO  SYS-1902-ADRS2
                           MOVE    SPA-GMN-ADRS3   TO  SYS-1902-ADRS3
                           MOVE    SPA-GMN-TEL     TO  SYS-1902-TEL
                           MOVE    SPA-GMN-FAX     TO  SYS-1902-FAX
                           MOVE    SPA-NAI-ADRS1MOJI   TO
                                                   SYS-1902-ADRS1MOJI 
                           MOVE    SYS-1902-REC    TO  SYSKANRI-REC
                           PERFORM 800-MCNDATE-SEC
                           IF      FLG-SYSKANRI        =   ZERO
                               MOVE    SMCNDATE-YMD    TO  SYS-UPYMD
                               MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
                               MOVE    SYS-KANRICD     TO  SYSUP-KANRICD
                               MOVE    SYS-KBNCD       TO  SYSUP-KBNCD
                               MOVE    SPA-W01-STYUKYMD
                                                   TO  SYSUP-STYUKYMD
                               MOVE    SPA-W01-EDYUKYMD
                                                   TO  SYSUP-EDYUKYMD
                           MOVE    SPA-HOSPNUM     TO  SYSUP-HOSPNUM
                               MOVE    "tbl_syskanri"  TO  MCP-TABLE
                               MOVE    "upd1"          TO  MCP-PATHNAME
                               MOVE    "DBUPDATE"      TO  MCP-FUNC
                           ELSE
                               MOVE    SMCNDATE-YMD    TO  SYS-CREYMD
                                                           SYS-UPYMD
                               MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
                               MOVE    "tbl_syskanri"  TO  MCP-TABLE
                               MOVE    "key"           TO  MCP-PATHNAME
                               MOVE    "DBINSERT"      TO  MCP-FUNC
                           END-IF
                           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
grpsys                     CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                           IF      MCP-RC              NOT =   ZERO 
                               MOVE    "1005"              TO  SPA-ERRCD
                               GO  TO  490-KOUSHIN-EXT
                           END-IF
                       END-IF
                   END-IF      
      *
      *            区分コード編集処理
                   PERFORM 500-KBNCD-HENSYU-SEC
      *
      *            項目クリア
                   INITIALIZE                  SPA-GMN-KOUMOKU1
                                               SPA-GMN-KBNCD-G
                   MOVE    1                   TO  SPA-GMN-CUR
               WHEN    OTHER
      *            帳票別指定情報
                   MOVE    SPACE               TO  SYS-1900-REC
                   INITIALIZE                      SYS-1900-REC
                   MOVE    "1900"              TO  SYS-1900-KANRICD
                   MOVE    "*"                 TO  SYS-1900-KBNCD
                   MOVE    SPA-W01-STYUKYMD
                                               TO  SYS-1900-STYUKYMD
                   MOVE    SPA-W01-STYUKYMD
                                               TO  SYS-1900-EDYUKYMD
                   MOVE    SPA-HOSPNUM         TO  SYS-1900-HOSPNUM
                   MOVE    SYS-1900-REC        TO  MCPDATA-REC
                   PERFORM 900-SYSKANRI-INV-SEC
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC         TO  SYS-1900-REC
                   ELSE
                       MOVE    SPACE               TO  SYS-1900-REC
                       INITIALIZE                      SYS-1900-REC
                       MOVE    "1900"              TO  SYS-1900-KANRICD
                       MOVE    "*"                 TO  SYS-1900-KBNCD
                   END-IF
      *
                   MOVE    ZERO                TO  FLG-CHK
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       SPA-GMN-PRTKBN-MAX
                       IF      SPA-GMN-PRTKBN (IDX)    NOT =   SPACE
                           MOVE    1                   TO  FLG-CHK
                           MOVE    SPA-GMN-PRTKBN-MAX  TO  IDX
                       END-IF
                   END-PERFORM
      *  
                   IF      FLG-CHK             =   ZERO
                       IF      FLG-SYSKANRI        =   ZERO
                           MOVE    SYS-1900-REC        TO  SYSKANRI-REC
                           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
                           MOVE    "DBDELETE"          TO  MCP-FUNC
                           MOVE    "tbl_syskanri"      TO  MCP-TABLE
                           MOVE    "key"               TO  MCP-PATHNAME
grpsys                     CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                           IF      MCP-RC              NOT =   ZERO 
                               MOVE    "1002"              TO  SPA-ERRCD
                               GO  TO  490-KOUSHIN-EXT
                           END-IF
                       END-IF    
                   ELSE
                       MOVE    SPA-W01-STYUKYMD    TO  SYS-1900-STYUKYMD
                       MOVE    SPA-W01-EDYUKYMD    TO  SYS-1900-EDYUKYMD
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       SPA-GMN-PRTKBN-MAX
                           MOVE    SPA-GMN-PRTKBN (IDX)
                                           TO  SYS-1900-PRTKBN (IDX)
                       END-PERFORM
                       MOVE    SYS-1900-REC        TO  SYSKANRI-REC
                       PERFORM 800-MCNDATE-SEC
                       IF      FLG-SYSKANRI        =   ZERO
                           MOVE    SMCNDATE-YMD    TO  SYS-UPYMD
                           MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
                           MOVE    SYS-KANRICD     TO  SYSUP-KANRICD
                           MOVE    SYS-KBNCD       TO  SYSUP-KBNCD
                           MOVE    SPA-W01-STYUKYMD
                                                   TO  SYSUP-STYUKYMD
                           MOVE    SPA-W01-EDYUKYMD
                                                   TO  SYSUP-EDYUKYMD
                           MOVE    SPA-HOSPNUM     TO  SYSUP-HOSPNUM
                           MOVE    "tbl_syskanri"  TO  MCP-TABLE
                           MOVE    "upd1"          TO  MCP-PATHNAME
                           MOVE    "DBUPDATE"      TO  MCP-FUNC
                       ELSE
                           MOVE    SMCNDATE-YMD    TO  SYS-CREYMD
                                                       SYS-UPYMD
                           MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
                           MOVE    "tbl_syskanri"  TO  MCP-TABLE
                           MOVE    "key"           TO  MCP-PATHNAME
                           MOVE    "DBINSERT"      TO  MCP-FUNC
                       END-IF
                       MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                       MOVE    SYSKANRI-REC        TO  MCPDATA-REC
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                       IF      MCP-RC              NOT =   ZERO 
                           MOVE    "1001"              TO  SPA-ERRCD
                           GO  TO  490-KOUSHIN-EXT
                       END-IF
                   END-IF
      *
                   MOVE    101                 TO  SPA-GMN-CUR
           END-EVALUATE
      *
           EVALUATE    SPA-WIDCD    ALSO    SPA-WIDCD2
               WHEN    "1003"       ALSO    ANY
               WHEN    "1004"       ALSO    ANY
               WHEN    ANY          ALSO    "1006"
      *            医療機関編集情報
                   MOVE    SPACE               TO  SYSKANRI-REC
                   INITIALIZE                      SYSKANRI-REC
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
                   MOVE    SPA-W01-EDYUKYMD    TO  SYS-EDYUKYMD
                   MOVE    SMCNDATE-YMD        TO  SYS-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
                   MOVE    "1901"              TO  SYSUP-KANRICD
                   MOVE    SPA-W01-SEL-STYUKYMD
                                               TO  SYSUP-STYUKYMD
                   MOVE    SPA-W01-SEL-EDYUKYMD
                                               TO  SYSUP-EDYUKYMD
                   MOVE    SPA-HOSPNUM         TO  SYSUP-HOSPNUM
                   MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                   MOVE    SYSKANRI-REC        TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "upd2"              TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       MOVE    "1003"              TO  SPA-ERRCD
                       GO  TO  490-KOUSHIN-EXT
                   END-IF
      *
                   MOVE    SPACE               TO  SYSKANRI-REC
                   INITIALIZE                      SYSKANRI-REC
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
                   MOVE    SPA-W01-EDYUKYMD    TO  SYS-EDYUKYMD
                   MOVE    SMCNDATE-YMD        TO  SYS-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
                   MOVE    "1903"              TO  SYSUP-KANRICD
                   MOVE    SPA-W01-SEL-STYUKYMD
                                               TO  SYSUP-STYUKYMD
                   MOVE    SPA-W01-SEL-EDYUKYMD
                                               TO  SYSUP-EDYUKYMD
                   MOVE    SPA-HOSPNUM         TO  SYSUP-HOSPNUM
                   MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                   MOVE    SYSKANRI-REC        TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "upd2"              TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       MOVE    "1003"              TO  SPA-ERRCD
                       GO  TO  490-KOUSHIN-EXT
                   END-IF
      *
                   MOVE    SPACE               TO  SYSKANRI-REC
                   INITIALIZE                      SYSKANRI-REC
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
                   MOVE    SPA-W01-EDYUKYMD    TO  SYS-EDYUKYMD
                   MOVE    SMCNDATE-YMD        TO  SYS-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
                   MOVE    "1902"              TO  SYSUP-KANRICD
                   MOVE    SPA-W01-SEL-STYUKYMD
                                               TO  SYSUP-STYUKYMD
                   MOVE    SPA-W01-SEL-EDYUKYMD
                                               TO  SYSUP-EDYUKYMD
                   MOVE    SPA-HOSPNUM         TO  SYSUP-HOSPNUM
                   MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                   MOVE    SYSKANRI-REC        TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "upd2"              TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       MOVE    "1005"              TO  SPA-ERRCD
                       GO  TO  490-KOUSHIN-EXT
                   END-IF
      *
                   MOVE    SPACE               TO  SYSKANRI-REC
                   INITIALIZE                      SYSKANRI-REC
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
                   MOVE    SPA-W01-EDYUKYMD    TO  SYS-EDYUKYMD
                   MOVE    SMCNDATE-YMD        TO  SYS-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
                   MOVE    "1900"              TO  SYSUP-KANRICD
                   MOVE    SPA-W01-SEL-STYUKYMD
                                               TO  SYSUP-STYUKYMD
                   MOVE    SPA-W01-SEL-EDYUKYMD
                                               TO  SYSUP-EDYUKYMD
                   MOVE    SPA-HOSPNUM         TO  SYSUP-HOSPNUM
                   MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                   MOVE    SYSKANRI-REC        TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "upd2"              TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       MOVE    "1001"              TO  SPA-ERRCD
                       GO  TO  490-KOUSHIN-EXT
                   END-IF
           END-EVALUATE
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    区分コード編集処理
      *****************************************************************
       500-KBNCD-HENSYU-SEC            SECTION.
      *
           INITIALIZE                      SPA-GMN-KBNCDLST-G
      *    システム管理読み込み
           INITIALIZE                      SYS-1901-REC
           MOVE    "1901"              TO  SYS-1901-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1901-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1901-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM             UNTIL   FLG-SYSKANRI   =   1
               MOVE    MCPDATA-REC        TO  SYS-1901-REC
      *
               INITIALIZE                      SYS-1902-REC
               MOVE    "1902"              TO  SYS-1902-KANRICD
               MOVE    SYS-1901-KBNCD      TO  SYS-1902-KBNCD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-1902-STYUKYMD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-1902-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-1902-HOSPNUM
               MOVE    SYS-1902-REC        TO  MCPDATA-REC
               PERFORM 900-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1902-REC
               ELSE
                   INITIALIZE                      SYS-1902-REC
               END-IF
      *
               MOVE    SPACE               TO  WRK-MOJI1
                                               WRK-MOJI2
               MOVE    SYS-1901-HOSPNAME1MOJI
                                           TO  WRK-MOJISU
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI
               MOVE    WRK-MOJISUX         TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
                   MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                               TO  WRK-MOJI1
               END-IF
               MOVE    SYS-1902-ADRS1MOJI  TO  WRK-MOJISU
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI
               MOVE    WRK-MOJISUX         TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
                   MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                               TO  WRK-MOJI2
               END-IF
               ADD     1                   TO  SPA-GMN-KBNCD-MAX
               MOVE    SPA-GMN-KBNCD-MAX   TO  IDY
               MOVE    SYS-1901-KBNCD      TO  SPA-GMN-KBNCDL (IDY)
               STRING  "名称"                  DELIMITED  BY SIZE
                       WRK-MOJI1               DELIMITED  BY  SPACE
                       "所在地"                DELIMITED  BY SIZE
                       WRK-MOJI2               DELIMITED  BY  SPACE
                       INTO                SPA-GMN-KBNCDMEIL (IDY)
               END-STRING
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       500-KBNCD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票別指定区分編集処理
      *****************************************************************
       500-PRTKBN-HENSYU-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-KBNCDMEI
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       SPA-GMN-KBNCD-MAX
               IF      WRK-KBNCD       =   SPA-GMN-KBNCDL (IDXX)
                   MOVE    SPA-GMN-KBNCDMEIL (IDXX)
                                           TO  WRK-KBNCDMEI
                   MOVE    SPA-GMN-KBNCD-MAX
                                           TO  IDY
               END-IF                                     
           END-PERFORM
           .
       500-PRTKBN-HENSYU-EXT.
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
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD2      NOT =   SPACE
               PERFORM 530-WID2SET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "W18    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPA-GMN-STYUKYMD         TO  W18-YUKOSTYMD
           MOVE    SPA-GMN-EDYUKYMD         TO  W18-YUKOEDYMD
           MOVE    SPA-GMN-HHOSPNAME        TO  W18-HHOSPNAME
           MOVE    SPA-GMN-HKAISETUNAME      TO W18-HKAISETUNAME
           MOVE    SPA-GMN-HPOST            TO  W18-HPOST
           MOVE    SPA-GMN-HADRS            TO  W18-HADRS
           MOVE    SPA-GMN-HTEL             TO  W18-HTEL
           MOVE    SPA-GMN-HFAX             TO  W18-HFAX
      *
           MOVE    SPA-GMN-KBNCD-G          TO  W18-KBNCD
           MOVE    SPA-GMN-HOSPNAME1        TO  W18-HOSPNAME1
           MOVE    SPA-GMN-HOSPNAME2        TO  W18-HOSPNAME2
           MOVE    SPA-GMN-HOSPNAME3        TO  W18-HOSPNAME3
           MOVE    SPA-GMN-KAISETUNAME      TO  W18-KAISETUNAME
           MOVE    SPA-GMN-POST             TO  W18-POST
           MOVE    SPA-GMN-ADRS1            TO  W18-ADRS1
           MOVE    SPA-GMN-ADRS2            TO  W18-ADRS2
           MOVE    SPA-GMN-ADRS3            TO  W18-ADRS3
           MOVE    SPA-GMN-TEL              TO  W18-TEL
           MOVE    SPA-GMN-FAX              TO  W18-FAX
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-PRTKBN-MAX
               MOVE    SPA-GMN-PRTKBN-G (IDY)
                                       TO  W18-PRTKBN (IDY)
           END-PERFORM
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-KBNCD-MAX
               MOVE    SPA-GMN-KBNCDLST (IDY)
                                       TO  W18-KBNCD-ITEM  (IDY)
               PERFORM VARYING IDZ     FROM    1   BY  1
                       UNTIL   IDZ     >       SPA-GMN-PRTKBN-MAX
                   MOVE    SPA-GMN-KBNCDLST (IDY)
                                           TO  W18-PRTKBN-ITEM (IDZ IDY)
               END-PERFORM    
           END-PERFORM
           MOVE    SPA-GMN-KBNCD-MAX   TO  W18-KBNCD-COUNT
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL   IDZ     >       SPA-GMN-PRTKBN-MAX
               MOVE    SPA-GMN-KBNCD-MAX   TO  
                                           W18-PRTKBN-COUNT (IDZ)
           END-PERFORM
      *
           PERFORM 5001-MAPCUR-SEC
      *
      *----(04.06.01)--UPD-START---
           IF      W18-PAGENO          =   0
               MOVE    WIDGET-NORMAL       TO  W18-B02-STATE
               MOVE    WIDGET-NORMAL       TO  W18-B03-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  W18-B02-STATE
               MOVE    WIDGET-INSENSITIVE  TO  W18-B03-STATE
           END-IF
      *----(04.06.01)--UPD-END-----
      *
           .
       500-GMNHEN-EXT.
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
                   MOVE    "区分コード入力エラー"        TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "名称（１段目）は全角で入力して下さい"
                                                         TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "名称（２段目）は全角で入力して下さい"
                                                         TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "名称（３段目）は全角で入力して下さい"
                                                         TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "所在地（１段目）は全角で入力して下さい"
                                                         TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "所在地（２段目）は全角で入力して下さい"
                                                         TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "所在地（３段目）は全角で入力して下さい"
                                                         TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "開設者名は全角で入力して下さい"
                                                         TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "帳票別指定入力エラー"        TO  SPA-ERRMSG
      *----(04.03.01)--ADD-START---
               WHEN    "0011"
                   MOVE
              "レセプト電算の医療機関名称は１〜２０文字以内にして下さい"
                                                         TO  SPA-ERRMSG
      *----(04.03.01)--ADD-END-----
               WHEN    "0100"
                   MOVE
                   "入力項目があるのに区分コードが入力されていません"
                                                         TO  SPA-ERRMSG
               WHEN    "0101"
                   MOVE
                       "項目が未入力なので更新できません"
                                                         TO  SPA-ERRMSG
      *----(04.06.01)--ADD-START---
               WHEN    "0102"
                   MOVE
                       "区分コードが選択されていないので削除できません"
                                                         TO  SPA-ERRMSG
      *----(04.06.01)--ADD-END-----
               WHEN    "1001"
                   MOVE    "帳票別指定情報の更新ができませんでした"
                                                         TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "帳票別指定情報の削除ができませんでした"
                                                         TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "医療機関名称情報の更新ができませんでした"
                                                         TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE    "医療機関名称情報の削除ができませんでした"
                                                         TO  SPA-ERRMSG
               WHEN    "1005"
                   MOVE    "所在地情報の更新ができませんでした"
                                                         TO  SPA-ERRMSG
               WHEN    "1006"
                   MOVE    "所在地情報の削除ができませんでした"
                                                         TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W18"                TO  SPA-MOTOPG
           MOVE    "WERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
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
           MOVE    SPACE               TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "医療機関編集情報を登録します"
                                                TO  WRK-WIDMSG
               WHEN    "1002"
                   MOVE    "帳票別指定情報を登録します"
                                                TO  WRK-WIDMSG
               WHEN    "1003"
               WHEN    "1004"
                   MOVE
                   "適用日を変更しますので、登録後選択画面へ戻ります"
                                                TO  WRK-WIDMSG
               WHEN    "1005"
                   MOVE
                   "医療機関編集情報（帳票別指定含む）を削除します。"
                                                TO  WRK-WIDMSG(1:48)
                   MOVE    "よろしいですか？"   TO  WRK-WIDMSG(49:16)
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W18"                TO  SPA-MOTOPG
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
      *    ガイドメッセージ表示
      *****************************************************************
       530-WID2SET-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD2
               WHEN    "1003"
                   MOVE
                   "画面を切替えます。医療機関編集情報を登録しますか？"
                                                TO  WRK-WIDMSG
               WHEN    "1004"
                   MOVE
                   "画面を切替えます。帳票別指定情報を登録しますか？"
                                                TO  WRK-WIDMSG
               WHEN    "1006"
               WHEN    "1007"
                   MOVE
                   "医療機関名称が未入力ですが、このまま登録しますか？"
                                                TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID2
           INITIALIZE                       WID2
           MOVE    SPA-WIDCD2           TO  WID2-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID2-ID1MSG
           MOVE    "いいえ"             TO  WID2-B01 
           MOVE    "は  い"             TO  WID2-B12 
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W18"                TO  SPA-MOTOPG
           MOVE    "WID2"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WID2"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       530-WID2SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理    
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
          IF      SPA-GMN-CUR          =   ZERO
               PERFORM 50011-CUR-SET-SEC
          END-IF
      *
          EVALUATE    SPA-GMN-CUR
                WHEN    1
                    MOVE  "KBNCD"            TO   MCP-WIDGET
                WHEN    2
                    MOVE  "HOSPNAME1"        TO   MCP-WIDGET
                WHEN    3
                    MOVE  "HOSPNAME2"        TO   MCP-WIDGET
                WHEN    4
                    MOVE  "HOSPNAME3"        TO   MCP-WIDGET
                WHEN    5
                    MOVE  "POST"             TO   MCP-WIDGET
                WHEN    6
                    MOVE  "ADRS1"            TO   MCP-WIDGET
                WHEN    7
                    MOVE  "ADRS2"            TO   MCP-WIDGET
                WHEN    8
                    MOVE  "ADRS3"            TO   MCP-WIDGET
                WHEN    9
                    MOVE  "TEL"              TO   MCP-WIDGET
                WHEN    10
                    MOVE  "FAX"              TO   MCP-WIDGET
                WHEN    11
                    MOVE  "KAISETUNAME"      TO   MCP-WIDGET
                WHEN    101
                    MOVE  "PRTKBN1"          TO   MCP-WIDGET
                WHEN    102
                    MOVE  "PRTKBN2"          TO   MCP-WIDGET
                WHEN    103
                    MOVE  "PRTKBN3"          TO   MCP-WIDGET
                WHEN    104
                    MOVE  "PRTKBN4"          TO   MCP-WIDGET
                WHEN    105
                    MOVE  "PRTKBN5"          TO   MCP-WIDGET
                WHEN    106
                    MOVE  "PRTKBN6"          TO   MCP-WIDGET
      *----(03.04.01)--ADD-START---
                WHEN    107
                    MOVE  "PRTKBN7"          TO   MCP-WIDGET
                WHEN    108
                    MOVE  "PRTKBN8"          TO   MCP-WIDGET
                WHEN    109
                    MOVE  "PRTKBN9"          TO   MCP-WIDGET
                WHEN    110
                    MOVE  "PRTKBN10"          TO   MCP-WIDGET
                WHEN    111
                    MOVE  "PRTKBN11"          TO   MCP-WIDGET
                WHEN    112
                    MOVE  "PRTKBN12"          TO   MCP-WIDGET
                WHEN    113
                    MOVE  "PRTKBN13"          TO   MCP-WIDGET
      *----(03.04.01)--ADD-END-----
      *----(04.01.00)--ADD-START---
                WHEN    114
                    MOVE  "PRTKBN14"          TO   MCP-WIDGET
                WHEN    115
                    MOVE  "PRTKBN15"          TO   MCP-WIDGET
                WHEN    116
                    MOVE  "PRTKBN16"          TO   MCP-WIDGET
      *----(04.01.00)--ADD-END-----
      *----(04.02.00)--ADD-START---
                WHEN    117
                    MOVE  "PRTKBN17"          TO   MCP-WIDGET
                WHEN    118
                    MOVE  "PRTKBN18"          TO   MCP-WIDGET
      *----(04.02.00)--ADD-END-----
      *----(04.03.01)--ADD-START---
                WHEN    119
                    MOVE  "PRTKBN19"          TO   MCP-WIDGET
                WHEN    120
                    MOVE  "PRTKBN20"          TO   MCP-WIDGET
      *----(04.03.01)--ADD-END-----
      *----(04.04.00)--ADD-START---
                WHEN    121
                    MOVE  "PRTKBN21"          TO   MCP-WIDGET
      *----(04.04.00)--ADD-END-----
                WHEN    122
                    MOVE  "PRTKBN22"          TO   MCP-WIDGET
      *----(04.06.02)--ADD-START---
                WHEN    123
                    MOVE  "PRTKBN23"          TO   MCP-WIDGET
      *----(04.06.02)--ADD-END-----
                WHEN    124
                    MOVE  "PRTKBN24"          TO   MCP-WIDGET
                WHEN    125
                    MOVE  "PRTKBN25"          TO   MCP-WIDGET
                WHEN    200
                    MOVE  "B12"              TO   MCP-WIDGET
            END-EVALUATE
      *
            EVALUATE    SPA-GMN-CUR
                WHEN    1   THRU    99
                    MOVE    0                TO  W18-PAGENO
                WHEN    100 THRU    199
                    MOVE    1                TO  W18-PAGENO
            END-EVALUATE
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "KBNCD"
                   MOVE    2           TO  SPA-GMN-CUR
               WHEN    "HOSPNAME1"
                   MOVE    3           TO  SPA-GMN-CUR
               WHEN    "HOSPNAME2"
                   MOVE    4           TO  SPA-GMN-CUR
               WHEN    "HOSPNAME3"
                   MOVE    11          TO  SPA-GMN-CUR
               WHEN    "KAISETUNAME"
                   MOVE    5           TO  SPA-GMN-CUR
               WHEN    "POST"
                   MOVE    6           TO  SPA-GMN-CUR
               WHEN    "ADRS1"
                   MOVE    7           TO  SPA-GMN-CUR
               WHEN    "ADRS2"
                   MOVE    8           TO  SPA-GMN-CUR
               WHEN    "ADRS3"
                   MOVE    9           TO  SPA-GMN-CUR
               WHEN    "TEL"
                   MOVE    10          TO  SPA-GMN-CUR
               WHEN    "FAX"
                   MOVE    200         TO  SPA-GMN-CUR
      *----(04.06.02)--UPD-START---
               WHEN    "PRTKBN1"
                   MOVE    102         TO  SPA-GMN-CUR
               WHEN    "PRTKBN2"
                   MOVE    103         TO  SPA-GMN-CUR
               WHEN    "PRTKBN3"
                   MOVE    114         TO  SPA-GMN-CUR
               WHEN    "PRTKBN14"
                   MOVE    104         TO  SPA-GMN-CUR
               WHEN    "PRTKBN4"
                   MOVE    105         TO  SPA-GMN-CUR
               WHEN    "PRTKBN5"
                   MOVE    106         TO  SPA-GMN-CUR
               WHEN    "PRTKBN6"
                   MOVE    115         TO  SPA-GMN-CUR
               WHEN    "PRTKBN15"
                   MOVE    119         TO  SPA-GMN-CUR
               WHEN    "PRTKBN19"
                   MOVE    120         TO  SPA-GMN-CUR
               WHEN    "PRTKBN20"
                   MOVE    121         TO  SPA-GMN-CUR
               WHEN    "PRTKBN21"
                   MOVE    107         TO  SPA-GMN-CUR
               WHEN    "PRTKBN7"
                   MOVE    108         TO  SPA-GMN-CUR
               WHEN    "PRTKBN8"
                   MOVE    116         TO  SPA-GMN-CUR
               WHEN    "PRTKBN16"
                   MOVE    117         TO  SPA-GMN-CUR
               WHEN    "PRTKBN17"
                   MOVE    123         TO  SPA-GMN-CUR
               WHEN    "PRTKBN9"
                   MOVE    110         TO  SPA-GMN-CUR
               WHEN    "PRTKBN10"
                   MOVE    111         TO  SPA-GMN-CUR
               WHEN    "PRTKBN11"
                   MOVE    112         TO  SPA-GMN-CUR
               WHEN    "PRTKBN12"
                   MOVE    124         TO  SPA-GMN-CUR
               WHEN    "PRTKBN13"
                   MOVE    118         TO  SPA-GMN-CUR
               WHEN    "PRTKBN18"
      ********     MOVE    200         TO  SPA-GMN-CUR
                   MOVE    122         TO  SPA-GMN-CUR
               WHEN    "PRTKBN22"
                   MOVE    200         TO  SPA-GMN-CUR
               WHEN    "PRTKBN23"
                   MOVE    109         TO  SPA-GMN-CUR
      *----(04.06.02)--UPD-END-----
               WHEN    "PRTKBN24"
                   MOVE    125         TO  SPA-GMN-CUR
               WHEN    "PRTKBN25"
                   MOVE    113         TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           IF      WRK-YMD  (1:7)  NOT NUMERIC
               INSPECT     WRK-YMD      REPLACING   ALL " " BY  "0"
               MOVE    WRK-YMD(1:1)     TO  WRK-WGO
               MOVE    WRK-YMD(2:2)     TO  WRK-WYY
               MOVE    WRK-YMD(5:2)     TO  WRK-WMM
               MOVE    WRK-YMD(8:2)     TO  WRK-WDD
           ELSE
               MOVE    WRK-YMD(1:7)     TO  WRK-WYMD
           END-IF
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "12"                TO  LNK-DAY1-IRAI
           MOVE    ZERO                TO  LNK-DAY1-YMD
           MOVE    WRK-WYMD            TO  LNK-DAY1-YMD(2:7)
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE
               MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
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
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
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
      *    ＤＢクロース処理
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
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
