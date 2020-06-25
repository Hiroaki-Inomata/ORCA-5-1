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
       PROGRAM-ID.             ORCGI49.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  コンポーネント名  : Ｐカ入力（Ｉ４９）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02.09.19    ひさなが      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-多々納   04/05/07  会計照会と同じ機能とする
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
      *
       DATA                    DIVISION.
       WORKING-STORAGE             SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "I4COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "I44SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(02).
           03  IDX1                PIC 9(03).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ERRMSG          PIC X(40).
           03  WRK-PIN.
               05  PIN9            PIC 9(02).
               05  PIN9-R          REDEFINES  PIN9.
                   07  PIN10       PIC 9(01).
                   07  PIN1        PIC 9(01).
           03  WRK-PED.
               05  PEDX            PIC X(02)  OCCURS  10.
           03  WRK-PKANA           PIC X(02).
      *
       01  PIN-TBL-AREA.
           03  KANA-TBL.
               05 FILLER PIC X(20) VALUE "アイウエオ　　　　　".
               05 FILLER PIC X(20) VALUE "カキクケコ　　　　　".
               05 FILLER PIC X(20) VALUE "サシスセソ　　　　　".
               05 FILLER PIC X(20) VALUE "タチツテト　　　　　".
               05 FILLER PIC X(20) VALUE "ナニヌネノ　　　　　".
               05 FILLER PIC X(20) VALUE "ハヒフヘホ　　　　　".
               05 FILLER PIC X(20) VALUE "マミムメモ　　　　　".
               05 FILLER PIC X(20) VALUE "ヤ　ユ　ヨ　　　　　".
               05 FILLER PIC X(20) VALUE "ラリルレロ１２３４５".
               05 FILLER PIC X(20) VALUE "ワヲン゛゜６７８９０".
           03  KANA-TBL-R          REDEFINES  KANA-TBL.
               05  PKANA           PIC X(02)  OCCURS  100.
      *      
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
           COPY    "ORCA32SCRAREA.INC".
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-I44
           MOVE    SPA-WORK            TO  SPA-I4KYOUTU
      *
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
           MOVE    SPA-I4KYOUTU        TO  SPA-WORK.
           MOVE    SPA-AREA            TO  SPA-COMMON.
           MOVE    SPA-I44             TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *    INITIALIZE                  SPA-I49-DATA
      *
      *    初期画面編集
      *    PERFORM 300-SCREEN-SEC
      *
      *    画面編集
           PERFORM 500-GMNHEN-SEC
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "I49"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *
           PERFORM 310-SPASET-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
           .
       3OO-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-I49GMN-AREA
           .
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  I49-PIN
           MOVE    SPA-I49-PED         TO  I49-PED
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
           MOVE    "PIN"               TO  MCP-WIDGET
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT
               WHEN    "CLICKED"
                   IF      MCP-WIDGET(2:2)     NUMERIC
                       MOVE    MCP-WIDGET(2:2)     TO  PIN9
                       PERFORM 410111-PIN-SET-SEC
                   ELSE
                       EVALUATE    MCP-WIDGET
                           WHEN    "BSH"
                               PERFORM 410112-SHIFT-SEC
      *----(01.00.01) LINE ADD START ----------------------------------
      *                    後退
                           WHEN     "BAK"
                               PERFORM 410113-BACK-SEC
      *                    終了
                           WHEN     "BEND"
                               PERFORM 210-BACK
                       END-EVALUATE
      *----(01.00.01) LINE ADD END   ----------------------------------
                   END-IF
           END-EVALUATE
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        番号入力
               WHEN    "ACTIVATE"      ALSO    "PIN"
                   PERFORM 41011-PIN-CHK-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力　処理
      *****************************************************************
       41011-PIN-CHK-SEC            SECTION.
      *
           IF       I49-PIN            =   SPACE
      *        空白入力時、終了
               PERFORM 210-BACK
           ELSE
               EVALUATE    TRUE
      *        前の桁を削除
                   WHEN   (I49-PIN(1:2)       =   "--") AND
                          (SPA-I49-IDX        >   0   )
                       MOVE    SPACE          TO  SPA-I49-PEDX
                                                         (SPA-I49-IDX)
                       COMPUTE SPA-I49-IDX    =   SPA-I49-IDX   -   1
      *        空白 
                   WHEN   (I49-PIN(1:2)        =   "..") AND
                          (SPA-I49-IDX         >   0)
                       ADD    1               TO  SPA-I49-IDX
                       MOVE   SPACE           TO  SPA-I49-PEDX
                                                        (SPA-I49-IDX)
      *        小文字変換
                   WHEN   (I49-PIN(1:1)        =   "+")  AND
                          (SPA-I49-IDX         >   0  )
                       PERFORM 410112-SHIFT-SEC
      *        漢字変換
                   WHEN    OTHER
                       MOVE    I49-PIN         TO  WRK-PIN
                       IF     (WRK-PIN(1:1)    =   SPACE) OR
                              (WRK-PIN(2:1)    =   SPACE) OR
                              (PIN10       NOT NUMERIC   ) OR
                              (PIN1        NOT NUMERIC   ) OR
                              (WRK-PIN         =   "82" OR "84") OR
                             ((PIN10           >=  1) AND
                              (PIN10           <=  8) AND
                              (PIN1            >   5))
                            CONTINUE
                       ELSE
                           IF      SPA-I49-IDX    >=  10
                               MOVE    "0002"          TO  SPA-ERRCD
                               GO      TO      41011-PIN-CHK-EXT
                           END-IF
      *                    漢字セット
                           PERFORM 410111-PIN-SET-SEC
                       END-IF
      *
               END-EVALUATE
           END-IF
           .
       41011-PIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    漢字セット　処理
      *****************************************************************
       410111-PIN-SET-SEC              SECTION.
      *
           EVALUATE    PIN9
               WHEN    0
                   MOVE    100         TO  IDX1
               WHEN    90
                   MOVE    90          TO  IDX1
               WHEN    OTHER
                   IF      PIN10       =   0
                       MOVE    9           TO  PIN10
                       MOVE    PIN9        TO  IDX1
                   ELSE
                       COMPUTE IDX1        =   PIN9    -   10
                   END-IF
           END-EVALUATE
           MOVE    PKANA(IDX1)         TO  WRK-PKANA
           IF     (WRK-PKANA           =   "゜"  ) AND
                  (SPA-I49-IDX         >   0     )
               EVALUATE    SPA-I49-PEDX (SPA-I49-IDX)
                   WHEN    "ハ"
                       MOVE    "パ"            TO  WRK-PKANA
                   WHEN    "ヒ"
                       MOVE    "ピ"            TO  WRK-PKANA
                   WHEN    "フ"
                       MOVE    "プ"            TO  WRK-PKANA
                   WHEN    "ヘ"
                       MOVE    "ペ"            TO  WRK-PKANA
                   WHEN    "ホ"
                       MOVE    "ポ"            TO  WRK-PKANA
                   WHEN    OTHER
                       ADD    1                TO  SPA-I49-IDX
               END-EVALUATE
           ELSE
               IF    (WRK-PKANA        =   "゛" ) AND
                     (SPA-I49-IDX      >   0    )
                   EVALUATE    SPA-I49-PEDX (SPA-I49-IDX)
                       WHEN    "ウ"
                           MOVE    "ヴ"            TO  WRK-PKANA
                       WHEN    "カ"
                           MOVE    "ガ"            TO  WRK-PKANA
                       WHEN    "キ"
                           MOVE    "ギ"            TO  WRK-PKANA
                       WHEN    "ク"
                           MOVE    "グ"            TO  WRK-PKANA
                       WHEN    "ケ"
                           MOVE    "ゲ"            TO  WRK-PKANA
                       WHEN    "コ"
                           MOVE    "ゴ"            TO  WRK-PKANA
                       WHEN    "サ"
                           MOVE    "ザ"            TO  WRK-PKANA
                       WHEN    "シ"
                           MOVE    "ジ"            TO  WRK-PKANA
                       WHEN    "ス"
                           MOVE    "ズ"            TO  WRK-PKANA
                       WHEN    "セ"
                           MOVE    "ゼ"            TO  WRK-PKANA
                       WHEN    "ソ"
                           MOVE    "ゾ"            TO  WRK-PKANA
                       WHEN    "タ"
                           MOVE    "ダ"            TO  WRK-PKANA
                       WHEN    "チ"
                           MOVE    "ヂ"            TO  WRK-PKANA
                       WHEN    "ツ"
                           MOVE    "ヅ"            TO  WRK-PKANA
                       WHEN    "テ"
                           MOVE    "デ"            TO  WRK-PKANA
                       WHEN    "ト"
                           MOVE    "ド"            TO  WRK-PKANA
                       WHEN    "ハ"
                           MOVE    "バ"            TO  WRK-PKANA
                       WHEN    "ヒ"
                           MOVE    "ビ"            TO  WRK-PKANA
                       WHEN    "フ"
                           MOVE    "ブ"            TO  WRK-PKANA
                       WHEN    "ヘ"
                           MOVE    "ベ"            TO  WRK-PKANA
                       WHEN    "ホ"
                           MOVE    "ボ"            TO  WRK-PKANA
                       WHEN    OTHER
                           ADD    1                TO  SPA-I49-IDX
                   END-EVALUATE
               ELSE
                   ADD     1               TO  SPA-I49-IDX
               END-IF
           END-IF
      *
           MOVE    WRK-PKANA           TO  SPA-I49-PEDX
                                                (SPA-I49-IDX)
           .
       410111-PIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    小文字変換　処理
      *****************************************************************
       410112-SHIFT-SEC                    SECTION.
      *
           IF      SPA-I49-IDX         =   0
               GO      TO      410112-SHIFT-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-PKANA
           EVALUATE    SPA-I49-PEDX (SPA-I49-IDX)
               WHEN    "ア"
                   MOVE    "ァ"            TO  WRK-PKANA
               WHEN    "イ"
                   MOVE    "ィ"            TO  WRK-PKANA
               WHEN    "ウ"
                   MOVE    "ゥ"            TO  WRK-PKANA
               WHEN    "エ"
                   MOVE    "ェ"            TO  WRK-PKANA
               WHEN    "オ"
                   MOVE    "ォ"            TO  WRK-PKANA
               WHEN    "カ"
                   MOVE    "ヵ"            TO  WRK-PKANA
               WHEN    "ツ"
                   MOVE    "ッ"            TO  WRK-PKANA
               WHEN    "ヤ"
                   MOVE    "ャ"            TO  WRK-PKANA
               WHEN    "ユ"
                   MOVE    "ュ"            TO  WRK-PKANA
               WHEN    "ヨ"
                   MOVE    "ョ"            TO  WRK-PKANA
           END-EVALUATE
      *
           IF      WRK-PKANA       NOT =   SPACE
               MOVE    WRK-PKANA           TO  SPA-I49-PEDX
                                                (SPA-I49-IDX)
           END-IF
           .
       410112-SHIFT-EXT.
           EXIT.
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *****************************************************************
      *    後退　処理
      *****************************************************************
       410113-BACK-SEC                    SECTION.
      *
      *    前の桁を削除
           IF      SPA-I49-IDX        >   0
               MOVE    SPACE          TO  SPA-I49-PEDX(SPA-I49-IDX)
               COMPUTE SPA-I49-IDX    =   SPA-I49-IDX   -   1
           END-IF
           .
       410113-BACK-EXT.
           EXIT.
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPA-I49-PED         TO  SPA-I49-DATA
      *
           MOVE    "I44"               TO  SPA-SAKIPG
           MOVE    "I49"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
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
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "I49"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラー"        TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "１０文字までです"  TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  I4ERR
           MOVE    SPA-ERRMSG          TO  I4ERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "I49"               TO  SPA-MOTOPG
           MOVE    "I4ERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "I4ERR"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL   "ORCDBMAIN"   USING   MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA.
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
