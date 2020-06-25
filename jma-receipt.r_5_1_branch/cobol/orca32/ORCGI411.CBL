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
       PROGRAM-ID.                 ORCGI411.
      ********************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院会計照会 
      *  コンポーネント名  : プレビュー保険組合せ選択サブ（Ｉ４１１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  12/11/09    NACL−多々納  新規作成
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
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    会計照会共通パラメタ
           COPY    "I4COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "I4SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-ERR             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
           03  CNT-ENT             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDH                 PIC 9(04).
           03  IDK                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-HKNCOMBINUM     PIC  X(04).
      *
           03  FLG-GMN-INIT        PIC  9(01).
      *
       01  WRK-HEN-AREA.
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
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
      *    COPY    "CPORCSDAY.INC".
      *    COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    全角チェックパラメタ
      *    COPY    "CPORCSKANACHK.INC".
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
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-I41
           MOVE    SPA-WORK            TO  SPA-I4KYOUTU
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-END
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
           MOVE    SPA-I4KYOUTU        TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-I41             TO  SPA-FREE
          .
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA.
           INITIALIZE                  CNT-AREA.
           INITIALIZE                  WRK-AREA.
      *    エラー画面より
           IF      SPA-MOTOPG          =   "I4ERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE
           MOVE   "I411"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
           PERFORM                     310-SPA-SET-SEC
           .
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                 SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
      *
           INITIALIZE                      SPA-I411-AREA
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX     >   SPA-NAI-MAX-41
               IF      SPA-NAI-TZAISKBKBN-41 (IDX) =   "5"
      *            保険判定
                   PERFORM VARYING IDK     FROM    1   BY  1
                           UNTIL   IDK     >   SPA-NAI-SRYMATU
                       IF      SPA-NAI-DAY-41 (IDX IDK)    NOT =   ZERO
                           MOVE    ZERO        TO  WRK-HKNCOMBINUM
                           MOVE    SPA-NAI-DAY-41 (IDX IDK)
                                               TO  WRK-HKNCOMBINUM(2:3)
                           PERFORM VARYING     IDY     FROM    1   BY  1
                                   UNTIL       IDY     >   30
                               IF      SPA-I411-THKNCOMBI (IDY)
                                                       =   SPACE
                                   PERFORM 3101-HKNCOMBI-SET-SEC
                               END-IF
                               IF      SPA-I411-THKNCOMBI (IDY)
                                                   =   WRK-HKNCOMBINUM
                                   MOVE    30              TO  IDY
                               END-IF
                           END-PERFORM
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           MOVE    1                   TO  SPA-I411-GMN-CUR
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       3101-HKNCOMBI-SET-SEC                  SECTION.
      *
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL       IDH     >   SPA-HKNCOMBI-MAX
               IF      SPA-I41-THKNCOMBI (IDH) =   WRK-HKNCOMBINUM
                   MOVE    SPA-I41-THKNCOMBI (IDH)
                                           TO  SPA-I411-THKNCOMBI (IDY)
                   MOVE    SPA-I41-THKNCOMBIMEI (IDH)
                                           TO  SPA-I411-THKNCOMBIMEI
                                                                 (IDY)
                   MOVE    SPA-I41-HKNNUM (IDH)
                                           TO  SPA-I411-THKNNUM  (IDY)
                   MOVE    SPA-I41-HKNKBN (IDH)
                                           TO  SPA-I411-THKNKBN  (IDY)
                   MOVE    SPA-I41-RSI-HKNKBN (IDH)
                                           TO  SPA-I411-TRSI-HKNKBN(IDY)
                   MOVE    SPA-I41-SISIKBN (IDH)
                                           TO  SPA-I411-TSISIKBN (IDY)
                   MOVE    IDY             TO  SPA-I411-HKNCOMBI-MAX
                   MOVE    SPA-HKNCOMBI-MAX    TO  IDH
               END-IF
           END-PERFORM
           .
       3101-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  I411
           INITIALIZE                      I411
      *
           MOVE    SPA-I411-SELNUM    TO  I411-SELNUM
      *
           PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL     IDX       >   SPA-I411-HKNCOMBI-MAX
               MOVE    SPA-I411-THKNCOMBI (IDX)
                                           TO  I411-HKNCOMBI (IDX)
               MOVE    SPA-I411-THKNCOMBIMEI(IDX)
                                           TO  I411-HKNCOMBIMEI (IDX)
           END-PERFORM
           MOVE    SPA-I411-HKNCOMBI-MAX   TO  I411-COUNT
      *
           PERFORM 5001-MAPCUR-SEC.
      *
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-I411-GMN-CUR
               WHEN    01
                   MOVE    "SELNUM"        TO  MCP-WIDGET
           END-EVALUATE.
      *
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    SPACE               TO  SPA-I411-SELNUM
                   PERFORM 210-BACK
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        番号入力
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 41011-SELNUM-CHK-SEC
      *        行選択
               WHEN    ANY             ALSO    "HKNLIST"
                   PERFORM 41012-LISTSEL-SEC
           END-EVALUATE.
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号選択　処理
      *****************************************************************
       41011-SELNUM-CHK-SEC                SECTION.
      *
           MOVE    1                   TO  SPA-I411-GMN-CUR
      *
           MOVE    I411-SELNUM         TO  SPA-I411-SELNUM
      *    選択番号入力
           INITIALIZE                      ORCSNUMAREA.
           MOVE    SPA-I411-SELNUM     TO  SNUM-INX.
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA.
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )   OR
                  (SNUM-SYOKBN         =   "1"   )
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  SPA-I411-SELNUM
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-I411-HKNCOMBI-MAX )
                           OR (FLG-OK      =   1      )
               IF      SPA-I411-THKNCOMBI(IDX) =   SPA-I411-SELNUM
                   MOVE    1               TO  FLG-OK
                   PERFORM 41012-HKNCOMBI-SELHEN-SEC
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
           ELSE
                PERFORM 210-BACK
           END-IF
           .
       41011-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ内容編集処理
      *****************************************************************
       41012-HKNCOMBI-SELHEN-SEC       SECTION.
      *
      *     保険判定
            EVALUATE    SPA-I411-THKNNUM (IDX)
               WHEN    "971"
      *            労災
                   MOVE    "1"                 TO  SPA-XC01-HKNKBN
                   MOVE    SPA-I411-TRSI-HKNKBN (IDX)
                                               TO  SPA-XC01-RSI-HKNKBN
               WHEN    "973"
      *            自賠責
                   MOVE    "2"                 TO  SPA-XC01-HKNKBN
                   MOVE    SPA-I411-TRSI-HKNKBN (IDX)
                                               TO  SPA-XC01-RSI-HKNKBN
               WHEN    "975"
      *            公害
                   MOVE    "3"             TO  SPA-XC01-HKNKBN
                   MOVE    SPACE           TO  SPA-XC01-RSI-HKNKBN
               WHEN    OTHER
      *            その他
                   MOVE    "0"             TO  SPA-XC01-HKNKBN
                   MOVE    SPACE           TO  SPA-XC01-RSI-HKNKBN
           END-EVALUATE
           .
       41012-HKNCOMBI-SELHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41012-LISTSEL-SEC       SECTION.
      *
      *    行選択
           MOVE    SPACE               TO  SPA-I411-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-I411-HKNCOMBI-MAX
               IF      I411-SELECT (IDX)   =   "T"
                   MOVE    SPA-I411-THKNCOMBI(IDX)  TO   SPA-I411-SELNUM
                   MOVE    SPA-I411-HKNCOMBI-MAX    TO   IDX
               END-IF
           END-PERFORM
      *
           IF      SPA-I411-SELNUM     =   SPACE
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
      *        保険組合せチェック処理
               MOVE    SPA-I411-SELNUM     TO  I411-SELNUM
               PERFORM 41011-SELNUM-CHK-SEC
           END-IF
           .
      *
       41012-LISTSEL-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "I41"               TO  SPA-SAKIPG
           MOVE    "I411"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "I41"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "I411"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "保険組合せ入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPA-ERRCD
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  I4ERR.
           MOVE    SPA-ERRCD           TO  I4ERR-ERRCODE.
           MOVE    SPA-ERRMSG          TO  I4ERR-ERRMSG.
           MOVE    "B01"               TO  MCP-WIDGET.
      *
           MOVE    "I411"              TO  SPA-MOTOPG.
           MOVE    "I4ERR"             TO  SPA-SAKIPG.
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "I4ERR"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
      *
       510-ERRSET-EXT.
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
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
