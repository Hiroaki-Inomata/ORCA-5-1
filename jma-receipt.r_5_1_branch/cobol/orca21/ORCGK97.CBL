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
       PROGRAM-ID.                 ORCGK97.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 複数科・保険組合せ・ドクター選択
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/07/23    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  05/11/17  MONFUNC 対応
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
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
      *
       WORKING-STORAGE             SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    会計照会共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "K02SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-CHK2            PIC 9(01).
      *
           03  FLG-UKETUKE         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDGET              PIC X(64).
      *
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-DRCD                PIC X(04).
           03  WRK-DRCDMEI             PIC X(50).
      *
      *    コラムリスト位置
           03  WRK-K97-HKNCOMBI-ROW    PIC S9(09).
           03  WRK-K97-SRYKA-ROW       PIC S9(09).
           03  WRK-K97-DRCD-ROW        PIC S9(09).
      *
           03  FLG-GMN-INIT            PIC  9(01).
           03  FLG-GMN-INIT2           PIC  9(01).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    受診内容
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
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
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ドクター編集処理パラメタ
           COPY    "CPORCSDRSET.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
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
      *
       01  SCRAREA.
           COPY    "ORCA21SCRAREA.INC".
      *!!!     COPY    "K01.INC".
      *!!!     COPY    "K02.INC".
      *!!!     COPY    "K02N.INC".
      *!!!     COPY    "K021.INC".
      *!!!     COPY    "K023.INC".
      *!!!     COPY    "K03.INC".
      *!!!     COPY    "K04.INC".
      *!!!     COPY    "K05.INC".
      *!!!     COPY    "K051.INC".
      *!!!     COPY    "K052.INC".
      *!!!     COPY    "K06.INC".
      *!!!     COPY    "K061.INC".
      *!!!     COPY    "K062.INC".
      *!!!     COPY    "K07.INC".
      *!!!     COPY    "K08.INC".
      *!!!     COPY    "K081.INC".
      *!!!     COPY    "K09.INC".
      *!!!     COPY    "K10.INC".
      *!!!     COPY    "K97.INC".
      *!!!     COPY    "K98.INC".
      *!!!     COPY    "K99.INC".
      *!!!     COPY    "K03X.INC".
      *!!!     COPY    "KERR.INC".
      *!!!     COPY    "KERR2.INC".
      *!!!     COPY    "KID1.INC".
      *!!!     COPY    "KID2.INC".
      *!!!     COPY    "KHELP.INC".
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
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
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
           MOVE    SPA-K01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-K02         TO  SPA-FREE
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "KERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "K97"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
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
      *****INITIALIZE                  SPA-K97-AREA
      *
           MOVE    1                   TO  FLG-GMN-INIT
           PERFORM                     310-SPA-SET-SEC
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT2
           INITIALIZE                  SPA-K97-HKNCOMBI-LISTG
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-HKN-MAX ) OR
                              (SPA-HKNCOMBINUM     =   "9999") OR
                              (SPA-HKNKBN          =   1     )
                       OR     (SPA-HKNKBN          =   2     )
      *******  IF     (SPA-GMN-THKNCOMBINUM(IDX)   =   "9999") OR
               IF     (SPA-NAI-RSI-HKNKBN(IDX) NOT =   SPACE )  OR
                     ((SPA-GMN-THKNCOMBINUM(IDX)   =   "9999") AND
                      (SPA-SRYYMD                  <   "20060401"))
      *           公害は対象外
                  OR  (SPA-NAI-HKNNUM (IDX)        =   "975"  )
      *           入院中は自費のみ
                  OR ((SPA-NAI-NYUIN               =   1    )  AND
                      (SPA-NAI-HKNNUM (IDX)(1:2)   NOT =   "98" ))
                   CONTINUE
               ELSE
                   ADD     1               TO  IDY
                   MOVE    SPA-GMN-THKNCOMBINUM(IDX)
                                           TO  SPA-K97-THKNCOMBINUM(IDY)
                   MOVE    SPA-GMN-THKNCOMBIMEI(IDX)
                                           TO  SPA-K97-THKNCOMBIMEI(IDY)
      *            地方公費のみ
                   IF     (SPA-NAI-HKNNUM (IDX)    =   SPACE )  AND
                          (SPA-NAI-KOH1HKNNUM(IDX) >=  "100" AND
                                                   <=  "899"  ) AND
                         ((SPA-NAI-KOH2HKNNUM(IDX) =   SPACE ) OR
                          (SPA-NAI-KOH2HKNNUM(IDX) >=  "100" AND
                                                   <=  "899" )) AND
                         ((SPA-NAI-KOH3HKNNUM(IDX) =   SPACE ) OR
                          (SPA-NAI-KOH3HKNNUM(IDX) >=  "100" AND
                                                   <=  "899" )) AND
                         ((SPA-NAI-KOH4HKNNUM(IDX) =   SPACE ) OR
                          (SPA-NAI-KOH4HKNNUM(IDX) >=  "100" AND
                                                   <=  "899"  ))
                       MOVE    1           TO  SPA-K97-TKOHONLY (IDY)
                   END-IF
                   MOVE    IDY             TO  SPA-K97-HKN-MAX
               END-IF
           END-PERFORM
      *
      *    ドクター編集
           PERFORM 3101-DRCD-SPASET-SEC
      *
           IF      SPA-K97-SRYKA       =   SPACE  OR  ZERO
               MOVE    SPA-GMN-SRYKA       TO  SPA-K97-SRYKA
           ELSE
               MOVE    SPA-K97-SRYKA       TO  SPA-K97-SELNUM2
               PERFORM 22031-UKETUKE-CHK-SEC
           END-IF
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター編集処理
      *****************************************************************
       3101-DRCD-SPASET-SEC            SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
      *
           INITIALIZE                  ORCSDRSETAREA
           MOVE    "1"                 TO  ORCSDR-KBN
           MOVE    SPA-SRYYMD          TO  ORCSDR-SYSYMD
           IF      SPA-K97-SRYKA       =   SPACE
               MOVE    SPA-GMN-SRYKA       TO  ORCSDR-SRYKA
           ELSE
               MOVE    SPA-K97-SRYKA       TO  ORCSDR-SRYKA
           END-IF
           MOVE    SPA-K97-DRCD        TO  ORCSDR-IN-DRCD
           CALL    "ORCSDRSET"         USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-K97-DRCDLST-G
           MOVE    ORCSDR-DRCD-MAX     TO  SPA-K97-DRCD-MAX
      *
           .
       3101-DRCD-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *    コラムリスト位置保存
           IF      FLG-GMN-INIT2       =   1
               MOVE    ZERO                TO  WRK-K97-SRYKA-ROW
               MOVE    ZERO                TO  WRK-K97-HKNCOMBI-ROW
           ELSE
               MOVE    K97-SRYKA-ROW       TO  WRK-K97-SRYKA-ROW
               MOVE    K97-HKNCOMBI-ROW
                                           TO  WRK-K97-HKNCOMBI-ROW
           END-IF
           IF      FLG-GMN-INIT        =   1
               MOVE    ZERO                TO  WRK-K97-DRCD-ROW
           ELSE
               MOVE    K97-DRCD-ROW        TO  WRK-K97-DRCD-ROW
           END-IF
      *
           MOVE    SPACE               TO  K97
      *    コラムリストの位置指定
           INITIALIZE                      K97
           MOVE    ZERO                TO  K97-HKNCOMBI-ROW
           MOVE    ZERO                TO  K97-SRYKA-ROW
           MOVE    ZERO                TO  K97-DRCD-ROW
           MOVE    ZERO                TO  K97-HKNCOMBI-ROWATTR
           MOVE    ZERO                TO  K97-SRYKA-ROWATTR
           MOVE    ZERO                TO  K97-DRCD-ROWATTR
           IF      WRK-K97-HKNCOMBI-ROW    NOT =   ZERO
               MOVE    WRK-K97-HKNCOMBI-ROW
                                       TO  K97-HKNCOMBI-ROW
           END-IF
           IF      WRK-K97-SRYKA-ROW    NOT =   ZERO
               MOVE    WRK-K97-SRYKA-ROW
                                       TO  K97-SRYKA-ROW
           END-IF
           IF      WRK-K97-DRCD-ROW    NOT =   ZERO
               MOVE    WRK-K97-DRCD-ROW
                                       TO  K97-DRCD-ROW
           END-IF
      *
      *    保険組合せ
           MOVE    SPA-K97-SELNUM1     TO  K97-SELNUM1
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-K97-HKN-MAX
               MOVE    SPA-K97-THKNCOMBINUM(IDX)
                                           TO  K97-HKNCOMBI    (IDX)
               MOVE    SPA-K97-THKNCOMBIMEI(IDX)
                                           TO  K97-HKNCOMBIMEI (IDX)
               IF      SPA-K97-SELNUM1     =   K97-HKNCOMBI    (IDX)
                   MOVE    "T"             TO  K97-HKNCOMBI-SELECT
                                                               (IDX)
                   MOVE    IDX             TO  K97-HKNCOMBI-ROW
                   MOVE    2               TO  K97-HKNCOMBI-ROWATTR
               ELSE
                   MOVE    "F"             TO  K97-HKNCOMBI-SELECT
                                                               (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-K97-HKN-MAX     TO  K97-HKNCOMBI-COUNT
      *    診療科
           MOVE    SPA-K97-SELNUM2     TO  K97-SELNUM2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               MOVE    SPA-GMN-SRYKAL(IDX) TO  K97-SRYKA   (IDX)
               MOVE    SPA-GMN-SRYKAMEIL(IDX)  TO  K97-SRYKAMEI (IDX)
               IF      SPA-K97-SELNUM2     =   K97-SRYKA (IDX)
                   MOVE    "T"             TO  K97-SRYKA-SELECT (IDX)
                   MOVE    IDX             TO  K97-SRYKA-ROW
                   MOVE    2               TO  K97-SRYKA-ROWATTR
               ELSE
                  MOVE    "F"              TO  K97-SRYKA-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-SRYKA-MAX        TO  K97-SRYKA-COUNT
      *    ドクター
           MOVE    SPA-K97-SELNUM3     TO  K97-SELNUM3
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-K97-DRCD-MAX
               MOVE    SPA-K97-DRCDL(IDX)      TO  K97-DRCD    (IDX)
               MOVE    SPA-K97-DRCDMEIL(IDX)   TO  K97-DRCDMEI (IDX)
               IF      SPA-K97-SELNUM3     =   K97-DRCD (IDX)
                   MOVE    "T"             TO  K97-DRCD-SELECT (IDX)
                   MOVE    IDX             TO  K97-DRCD-ROW
                   MOVE    2               TO  K97-DRCD-ROWATTR
               ELSE
                  MOVE    "F"              TO  K97-DRCD-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-K97-DRCD-MAX        TO  K97-DRCD-COUNT
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-K97-CUR        =   ZERO
               EVALUATE    WRK-WIDGET
                   WHEN    "SELNUM1"
                   WHEN    "HKNCOMBILST"
                       MOVE    2                   TO  SPA-K97-CUR
                   WHEN    "SELNUM2"
                   WHEN    "SRYKALIST"
                       MOVE    3                   TO  SPA-K97-CUR
                   WHEN    "SELNUM3"
                   WHEN    "DRCDLIST"
                       MOVE    4                   TO  SPA-K97-CUR
               END-EVALUATE
           END-IF
      *
           EVALUATE    SPA-K97-CUR
               WHEN    1
                   MOVE    "SELNUM1"           TO  MCP-WIDGET
               WHEN    2
                   MOVE    "SELNUM2"           TO  MCP-WIDGET
               WHEN    3
                   MOVE    "SELNUM3"           TO  MCP-WIDGET
               WHEN    4
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    ZERO                TO  SPA-K97-SYORI
                   PERFORM 210-BACK
      *        主科ドクター修正
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 230-SYUDRCD-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 220-KAKUTEI-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力個所のセット
           PERFORM 210-GMN-INPUT-SEC
      *
           EVALUATE    WRK-WIDGET
               WHEN    "HKNCOMBILST"
                   PERFORM 2001-HKNCOMBI-LIST-SEC
               WHEN    "SRYKALIST"
                   PERFORM 2002-SRYKA-LIST-SEC
               WHEN    "DRCDLIST"
                   PERFORM 2003-DRCD-LIST-SEC
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 220-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       210-GMN-INPUT-SEC               SECTION.
      *
           MOVE    ZERO                TO  SPA-K97-CUR
      *    入力項目の決定
           MOVE    MCP-WIDGET          TO  WRK-WIDGET
      *
           .
       210-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       220-INPUT-CHK-SEC               SECTION.
      *
      *    画面をＳＰＡへ編集
           PERFORM 2201-GMNSPA-SET-SEC
      *
           PERFORM 2202-SELNUM1-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2203-SELNUM2-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2204-SELNUM3-CHK-SEC
           END-IF
      *
           .
       220-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ選択処理
      *****************************************************************
       2001-HKNCOMBI-LIST-SEC               SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBI
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-K97-HKN-MAX
              IF      K97-HKNCOMBI-SELECT (IDX)        =  "T"
                  IF       K97-HKNCOMBI(IDX)   =   SPA-K97-SELNUM1
                      CONTINUE
                  ELSE
                      MOVE    K97-HKNCOMBI(IDX)    TO  WRK-HKNCOMBI
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-HKNCOMBI    NOT =   SPACE
               MOVE    WRK-HKNCOMBI        TO  SPA-K97-SELNUM1
               PERFORM 2202-SELNUM1-CHK-SEC
           END-IF
      *
           .
       2001-HKNCOMBI-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科選択処理
      *****************************************************************
       2002-SRYKA-LIST-SEC               SECTION.
      *
           MOVE    SPACE                TO  WRK-SRYKA
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-SRYKA-MAX
              IF      K97-SRYKA-SELECT (IDX)        =  "T"
                  IF       K97-SRYKA(IDX)      =   SPA-K97-SELNUM2
                      CONTINUE
                  ELSE
                      MOVE    K97-SRYKA (IDX)      TO  WRK-SRYKA
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-SRYKA    NOT =   SPACE
               MOVE    WRK-SRYKA        TO  SPA-K97-SELNUM2
               PERFORM 2203-SELNUM2-CHK-SEC
           END-IF
      *
           .
       2002-SRYKA-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科選択処理
      *****************************************************************
       2003-DRCD-LIST-SEC               SECTION.
      *
           MOVE    SPACE                TO  WRK-DRCD
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-K97-DRCD-MAX
              IF      K97-DRCD-SELECT (IDX)        =  "T"
                  IF       K97-DRCD(IDX)      =   SPA-K97-SELNUM3
                      CONTINUE
                  ELSE
                      MOVE    K97-DRCD (IDX)      TO  WRK-DRCD
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-DRCD    NOT =   SPACE
               MOVE    WRK-DRCD        TO  SPA-K97-SELNUM3
               PERFORM 2204-SELNUM3-CHK-SEC
           END-IF
      *
           .
       2003-DRCD-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面項目→ＳＰＡ編集処理
      *****************************************************************
       2201-GMNSPA-SET-SEC               SECTION.
      *
           MOVE    K97-SELNUM1         TO  SPA-K97-SELNUM1
           MOVE    K97-SELNUM2         TO  SPA-K97-SELNUM2
           MOVE    K97-SELNUM3         TO  SPA-K97-SELNUM3
      *
           .
       2201-GMNSPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       2202-SELNUM1-CHK-SEC            SECTION.
      *
           IF      SPA-K97-SELNUM1     =   SPACE
               MOVE   ZERO             TO  SPA-K97-SELNUM1
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-K97-SELNUM1     TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-K97-CUR
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  SPA-K97-SELNUM1
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-K97-HKN-MAX )  OR
                              (FLG-CHK     =   1           )  OR
                              (SPA-K97-SELNUM1 =   ZERO  )    OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-K97-SELNUM1     =   SPA-K97-THKNCOMBINUM(IDX)
                   IF     (SPA-K97-TKOHONLY (IDX)  =   1  ) AND
                          (SPA-K97-TKOHONLY-CHK(IDX)   =   ZERO )
                       MOVE    "K001"          TO  SPA-ERRCD
                       MOVE    1               TO  SPA-K97-TKOHONLY-CHK
                                                                   (IDX)
                   END-IF
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF     (SPA-K97-SELNUM1 NOT =   ZERO ) AND
                  (FLG-CHK             =   ZERO )
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-K97-CUR
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-K97-SELNUM1     =   ZERO
                   MOVE    SPACE               TO  SPA-K97-HKNCOMBI
                   MOVE    SPACE               TO  SPA-K97-SELNUM1
               ELSE
                   MOVE    SPA-K97-SELNUM1     TO  SPA-K97-HKNCOMBI
               END-IF
           END-IF
      *
           .
       2202-SELNUM1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療科チェック処理
      *****************************************************************
       2203-SELNUM2-CHK-SEC                SECTION.
      *
           IF      SPA-K97-SELNUM2     =   SPACE
               MOVE   ZERO             TO  SPA-K97-SELNUM2
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-K97-SELNUM2     TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-K97-CUR
           ELSE
               MOVE    SNUM-OUTEDT(14:2)   TO  SPA-K97-SELNUM2
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-SRYKA-MAX )  OR
                              (FLG-CHK     =   1           )  OR
                              (SPA-K97-SELNUM2 =   ZERO    )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-K97-SELNUM2     =   SPA-GMN-SRYKAL(IDX)
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF     (SPA-K97-SELNUM2 NOT =   ZERO ) AND
                  (FLG-CHK             =   ZERO )
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-K97-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-K97-SELNUM2 NOT =   ZERO  )  AND
                      (SPA-K97-SELNUM2 NOT =   SPA-K97-SRYKA)
                   MOVE    SPA-K97-SELNUM2     TO  SPA-K97-SRYKA
      *            ドクター編集
                   PERFORM 3101-DRCD-SPASET-SEC
                   PERFORM 22031-UKETUKE-CHK-SEC
               END-IF
               IF      SPA-K97-SELNUM2     =   ZERO
                   MOVE    SPACE               TO  SPA-K97-SRYKA
                   MOVE    SPACE               TO  SPA-K97-SELNUM2
               ELSE
                   MOVE    SPA-K97-SELNUM2     TO  SPA-K97-SRYKA
               END-IF
           END-IF
      *
           .
       2203-SELNUM2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付ドクターチェック処理
      *****************************************************************
       22031-UKETUKE-CHK-SEC                SECTION.
      *
      *    受診内容ファイルを特定する
           INITIALIZE                      UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    SPA-PTID            TO  UKE-PTID
      *
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-UKETUKE-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-UKETUKE
           END-IF
           MOVE    SPACE               TO  WRK-DRCD
           MOVE    SPACE               TO  WRK-HKNCOMBI
           PERFORM
                   UNTIL       FLG-UKETUKE     =    1
               IF      (UKE-SRYKA          =   SPA-K97-SRYKA  ) AND
                       (UKE-UKEID      NOT =   ZERO     )
                   IF      UKE-KAIKEITIME      =   ZERO
                       MOVE    UKE-DRCD(2:4)       TO  WRK-DRCD
                       IF      UKE-HKNCOMBI    NOT =   ZERO
                           MOVE    UKE-HKNCOMBI        TO  WRK-HKNCOMBI
                       END-IF
                       MOVE    1                   TO  FLG-UKETUKE
                   END-IF
               END-IF
      *
               IF      FLG-UKETUKE         =   ZERO
                   MOVE    "tbl_uketuke"       TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-UKETUKE-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    主科の時、受付は反映しない
           IF     (SPA-K97-SRYKA       =   SPA-GMN-SRYKA ) AND
                  (WRK-DRCD            =   SPA-DRCD )
               MOVE    SPACE               TO  WRK-DRCD
           END-IF
           IF     (SPA-K97-SRYKA       =   SPA-GMN-SRYKA ) AND
                  (WRK-HKNCOMBI        =   SPA-GMN-HKNCOMBINUM)
               MOVE    SPACE               TO  WRK-HKNCOMBI
           END-IF
      *
           MOVE    SPACE               TO  SPA-K97-SELNUM3
           MOVE    ZERO                TO  FLG-CHK2
           IF      WRK-DRCD            =   SPACE
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX         >   SPA-GMN-MAX1)  OR
                                  (FLG-CHK     =   1           )
                   IF      SPA-NAI-RRK-SRYKA(IDX)  =   SPA-K97-SRYKA
                       MOVE    SPA-NAI-RRK-DRCD (IDX)(2:4)
                                               TO  WRK-DRCD
                       MOVE    1               TO  FLG-CHK
                       MOVE    1               TO  FLG-CHK2
                   END-IF
               END-PERFORM
           END-IF
           IF      WRK-DRCD        NOT =   SPACE
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX         >   SPA-K97-DRCD-MAX) OR
                                  (FLG-CHK     =   1           )
                   IF      WRK-DRCD            =   SPA-K97-DRCDL(IDX)
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      FLG-CHK             =   1
                   MOVE    WRK-DRCD            TO  SPA-K97-SELNUM3
               ELSE
                   MOVE    WRK-DRCD            TO  SPA-K97-SELNUM3
                   PERFORM 22041-DRCD-CHK-SEC
               END-IF
           END-IF
      *
      *    保険組合せ
           IF      WRK-HKNCOMBI    NOT =   SPACE
               MOVE    WRK-HKNCOMBI        TO  SPA-K97-SELNUM1
               PERFORM 2202-SELNUM1-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    SPACE               TO  SPA-ERRCD
                   MOVE    3                   TO  SPA-K97-CUR
                   MOVE    SPA-K97-HKNCOMBI    TO  SPA-K97-SELNUM1
               END-IF
           END-IF
      *
           .
       22031-UKETUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクターチェック処理
      *****************************************************************
       2204-SELNUM3-CHK-SEC                SECTION.
      *
           IF      SPA-K97-SELNUM3     =   SPACE
               MOVE   ZERO             TO  SPA-K97-SELNUM3
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-K97-SELNUM3     TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-K97-CUR
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  SPA-K97-SELNUM3
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-K97-DRCD-MAX) OR
                              (FLG-CHK     =   1           )  OR
                              (SPA-K97-SELNUM3 =   ZERO  )    OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-K97-SELNUM3     =   SPA-K97-DRCDL(IDX)
                   MOVE    SPA-K97-DRCDMEIL(IDX)
                                           TO  WRK-DRCDMEI
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF     (SPA-K97-SELNUM3 NOT =   ZERO ) AND
                  (FLG-CHK             =   ZERO )
               MOVE    ZERO                TO  FLG-CHK2
               PERFORM 22041-DRCD-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-K97-SELNUM3     =   ZERO
                   MOVE    SPACE               TO  SPA-K97-DRCD
                   MOVE    SPACE               TO  SPA-K97-SELNUM3
               ELSE
                   MOVE    SPA-K97-SELNUM3     TO  SPA-K97-DRCD
               END-IF
           END-IF
      *
           .
       2204-SELNUM3-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクターチェック処理
      *****************************************************************
       22041-DRCD-CHK-SEC                   SECTION.
      *
      *    ドクター編集
           INITIALIZE                  ORCSDRSETAREA
           IF      FLG-CHK2            =   ZERO
               MOVE    "2"                 TO  ORCSDR-KBN
           ELSE
      *        前回受診のドクターを判定
               MOVE    "3"                 TO  ORCSDR-KBN
           END-IF
           MOVE    SPA-SRYYMD          TO  ORCSDR-SYSYMD
           MOVE    SPA-K97-SRYKA       TO  ORCSDR-SRYKA
           MOVE    SPA-K97-SELNUM3     TO  ORCSDR-IN-DRCD
           MOVE    SPA-K97-DRCDLST-G   TO  ORCSDR-DRCD-TBL-G
           MOVE    SPA-K97-DRCD-MAX    TO  ORCSDR-DRCD-MAX
           CALL    "ORCSDRSET"         USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           IF      ORCSDR-ERRCD        =   ZERO
               MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-K97-DRCDLST-G
               MOVE    ORCSDR-DRCD-MAX     TO  SPA-K97-DRCD-MAX
               MOVE    SPA-K97-DRCDMEIL(SPA-K97-DRCD-MAX)
                                           TO  WRK-DRCDMEI
           ELSE
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-K97-CUR
           END-IF
      *
           .
       22041-DRCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       230-SYUDRCD-SEC                   SECTION.
      *
           MOVE    K97-SELNUM3         TO  SPA-K97-SELNUM3
           IF      SPA-K97-SELNUM3     =   ZERO  OR  SPACE
               CONTINUE
           ELSE
               PERFORM 2204-SELNUM3-CHK-SEC
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "1"             TO  SPA-K97-SYU-DRCD(1:1)
                   MOVE    SPA-K97-DRCD    TO  SPA-K97-SYU-DRCD(2:4)
                   MOVE    SPA-K97-DRCD    TO  SPA-K97-SYU-DRNAME(1:4)
                   MOVE    WRK-DRCDMEI     TO  SPA-K97-SYU-DRNAME
                                                                (6:)
                   MOVE    SPACE           TO  SPA-K97-DRCD
                   MOVE    SPACE           TO  SPA-K97-SRYKA
                   MOVE    SPACE           TO  SPA-K97-HKNCOMBI
                   MOVE    1                   TO  SPA-K97-SYORI
      *            元の画面へ
                   PERFORM 210-BACK
               END-IF
           END-IF
           .
       230-SYUDRCD-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       220-KAKUTEI-SEC                   SECTION.
      *
      *    入力チェック処理
           PERFORM 220-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      220-KAKUTEI-EXT
           END-IF
      *
           MOVE    1                   TO  SPA-K97-SYORI
      *    元の画面へ
           PERFORM 210-BACK
      *
           .
       220-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "K02"               TO  SPA-SAKIPG
           MOVE    "K97"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    "K02"               TO  MCP-WINDOW
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "K97"               TO  MCP-WINDOW.
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
                   MOVE    "保険組合せがありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "診療科がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "ドクターコードがありません。"
                                       TO  SPA-ERRMSG
               WHEN    "K001"
                   MOVE    "警告！地方公費のみの保険組合せです。"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KERR
           MOVE    SPA-ERRCD           TO  KERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  KERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K97"               TO  SPA-MOTOPG
           MOVE    "KERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診内容データ読み込み
      *****************************************************************
       900-UKETUKE-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  UKETUKE-REC 
               MOVE    ZERO                TO  FLG-UKETUKE
           ELSE
               MOVE    1                   TO  FLG-UKETUKE
           END-IF
      *
           .
       900-UKETUKE-READ-EXT.
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
