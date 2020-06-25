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
       PROGRAM-ID.             ORCGT09.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 総括表・公費請求書
      *  コンポーネント名  : オンライン請求−レセ電対象患者一覧
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/09/05    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.04.01    NACL-藤原    09/12/10  返戻対応
      *  04.04.02    NACL-藤原    09/03/31  主科設定時の主科未設定データ
      *                                     の有無チェック
      *
      *  04.05.01    NACL-藤原    12/03/27  算定日情報に係る記録条件仕様
      *                                     の変更に伴う月後れ分のチェック
      *
      *  04.06.01    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *                                     receipt_kbn 追加 
      *
      *  04.07.01    NACL-藤原    12/01/13  健保組合への直接請求と審査
      *                                     支払機関請求対応
      *  04.07.02    NACL-藤原    12/08/01  レセ電データチェック機能対応
      *  04.07.03    NACL-藤原    13/10/07  労災レセ電対応
      *
      *  04.08.04    NACL-藤原    16/08/08  災害該当分（非減免対象と減免対象）
      *                                     レセプトの分割対応
      *
      *  05.00.01    NACL-藤原    17/12/19  ＨＡＯＲＩ対応
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "COMMON-SPA".
      *
           COPY    "T01COMMON-SPA".
      *
           COPY    "T06SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-ONRECE-B        PIC 9(01).
      *
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-SET             PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDXX                PIC 9(04).
           03  IDYY                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SELNO           PIC 9(02).
           03  WRK-RECE-MAX        PIC 9(04).
      *
           03  WRK-SEL-IDX         PIC 9(02).
           03  WRK-ERR-IDX         PIC 9(02).
      *
           03  WRK-Z9              PIC Z9.
           03  WRK-ZZ9             PIC ZZ9.
      *
      *    SPA-GMN-T09-SELNUM-OCCにあわせる
           03  WRK-GKN-T09.
               05  WRK-GMN-T09-SELNUM-OCC.
                   07  WRK-GMN-T09-SELNUM
                                   PIC 9(02)   OCCURS  5.
               05  WRK-GMN-T09-ERR PIC 9(01)   OCCURS  5.
      *
      *    入力項目名
           03  WRK-MCP-WIDGET      PIC X(64).
      *
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
       01  WRK-CONS-AREA.
      *    個別指示一覧最大行
           03  WRK-CONS-LINE-MAX   PIC 9(04)   VALUE   1000.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPRCF010.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
       01  ONRECE-B-REC.
           COPY    "CPONRECE-BODY.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
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
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-T06
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
           MOVE    SPA-T06         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
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
               IF      SPA-TIDCD       NOT =   SPACE
                   EVALUATE    SPA-TIDCD
                       WHEN    "1002"
                           PERFORM 530-TID2SET-SEC
                           GO  TO  100-INIT-EXT
                       WHEN    "1006"
                       WHEN    "1011"
                       WHEN    "1012"
                           PERFORM 520-TIDSET-SEC
                           GO  TO  100-INIT-EXT
                   END-EVALUATE
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "T09"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "TID1"
                   PERFORM 330-TID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "TID2"
                   PERFORM 340-TID2-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "T07"
                   IF      SPA-T07-FLG     =   "1"
                       MOVE    "1002"          TO  SPA-TIDCD
                   ELSE
                       INITIALIZE              SPA-T01KYOUTU
      ****             MOVE    ZERO            TO  SPA-GMN-T09-SELNUM
      ****                                         (SPA-NAI-T09-SELNUM)
      ****             MOVE    ZERO            TO  SPA-NAI-T09-SELNUM
                       MOVE    4               TO  SPA-GMN-T09-CUR
                   END-IF    
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           MOVE    1                   TO  SPA-GMN-T09-CUR
           .
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面戻り処理
      *****************************************************************
       330-TID1-SET-SEC            SECTION.
      *
           IF      SPA-TID1-FLG    =   "OK"
               EVALUATE    SPA-TIDCD
                   WHEN    "1006"
      *                返戻年月未入力のときの返戻データ存在チェック
                       IF      SPA-NAI-T09-THENREIYM
                                               (SPA-NAI-T09-SELNUM)
                                           =   "1" OR  "2"
                           EVALUATE    SPA-NAI-T09-THENREIYM
                                               (SPA-NAI-T09-SELNUM)
                               WHEN    "1"
                                   MOVE    "1011"      TO  SPA-TIDCD
                               WHEN    "2"
                                   MOVE    "1012"      TO  SPA-TIDCD
                           END-EVALUATE
                           MOVE    SPACE           TO  SPA-TID1-FLG
                           GO  TO  330-TID1-SET-EXT
                       END-IF          
                   WHEN    "1011"
                   WHEN    "1012"
                       CONTINUE
               END-EVALUATE 
           ELSE
               MOVE    ZERO    TO  SPA-GMN-T09-SELNUM
                                               (SPA-NAI-T09-SELNUM)
           END-IF
      *
           MOVE    ZERO            TO  SPA-NAI-T09-SELNUM
           MOVE    SPACE           TO  SPA-TID1-FLG
           MOVE    SPACE           TO  SPA-TIDCD      
      *
           .
       330-TID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＴＩＤ２）ＯＫ処理
      *****************************************************************
       340-TID2-SET-SEC            SECTION.
      *
           IF      SPA-TID1-FLG    =   "OK"
               EVALUATE    SPA-TIDCD
                   WHEN    "1002"
      *                作成日のチェック
                       IF      SPA-NAI-T09-TCREYMD (SPA-NAI-T09-SELNUM)
                                           <   SPA-NAI-T09-TUKEYMD
                                                   (SPA-NAI-T09-SELNUM)
                           MOVE    "1006"             TO  SPA-TIDCD
                           MOVE    SPACE              TO  SPA-TID1-FLG
                           GO  TO  340-TID2-SET-EXT
                       ELSE    
      *                    返戻年月未入力のときの返戻データ存在チェック
                           IF      SPA-NAI-T09-THENREIYM
                                                   (SPA-NAI-T09-SELNUM)
                                               =   "1" OR  "2"
                               EVALUATE    SPA-NAI-T09-THENREIYM
                                                   (SPA-NAI-T09-SELNUM)
                                   WHEN    "1"
                                       MOVE    "1011"      TO  SPA-TIDCD
                                   WHEN    "2"
                                       MOVE    "1012"      TO  SPA-TIDCD
                               END-EVALUATE
                               MOVE    SPACE           TO  SPA-TID1-FLG
                               GO  TO  340-TID2-SET-EXT
                           ELSE 
      *                        追加処理
                               MOVE    ZERO    TO  SPA-NAI-T09-SELNUM
                           END-IF
                       END-IF          
               END-EVALUATE 
           ELSE
               IF      SPA-TID1-FLG    =   "NO"
                   EVALUATE    SPA-TIDCD
                       WHEN    "1002"
      *                    個人履歴表示
                           MOVE    "1"                TO  SPA-T07-FLG
                           PERFORM 400-T07-SYORI-SEC
                           MOVE    SPACE              TO  SPA-TID1-FLG
                           GO  TO  340-TID2-SET-EXT
                   END-EVALUATE 
               END-IF
           END-IF
      *
           INITIALIZE                  SPA-T01KYOUTU
      *
           MOVE    ZERO            TO  SPA-GMN-T09-SELNUM
                                               (SPA-NAI-T09-SELNUM)
           MOVE    ZERO            TO  SPA-NAI-T09-SELNUM
      *
           MOVE    4               TO  SPA-GMN-T09-CUR
           .
       340-TID2-SET-EXT.
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
                   MOVE    SPACE       TO  SPA-TID1-FLG
                   PERFORM 210-BACK
      *     確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-KAKUTEI-SEC
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
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    ANY             ALSO    "RECELIST"
                   PERFORM 4105-RECESEL-SEC
      *        一括分
               WHEN    "ACTIVATE"       ALSO   ANY
                   PERFORM 400-ENTRY-SYORI-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括入力処理
      *****************************************************************
       400-ENTRY-SYORI-SEC       SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-T09-CUR
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
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  410-INPUT-CHK-EXT
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
           MOVE    T09-SELNUM (1)  TO  SPA-GMN-T09-SELNUM (1)
           MOVE    T09-SELNUM (2)  TO  SPA-GMN-T09-SELNUM (2)
           MOVE    T09-SELNUM (3)  TO  SPA-GMN-T09-SELNUM (3)
           MOVE    T09-SELNUM (4)  TO  SPA-GMN-T09-SELNUM (4)
           MOVE    T09-SELNUM (5)  TO  SPA-GMN-T09-SELNUM (5)
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
           PERFORM 41021-SELNUM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
                GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
        41021-SELNUM-CHK-SEC          SECTION.
      *
           MOVE    ZERO               TO  WRK-SEL-IDX
                                          WRK-ERR-IDX
      *
      *    重複するときはクリア（入力時のみ）
           IF      FLG-TOUROKU         =   ZERO
               EVALUATE    MCP-WIDGET
                   WHEN    "SELNUM01"
                       MOVE    1               TO  WRK-SEL-IDX
                   WHEN    "SELNUM02"
                       MOVE    2               TO  WRK-SEL-IDX
                   WHEN    "SELNUM03"
                       MOVE    3               TO  WRK-SEL-IDX
                   WHEN    "SELNUM04"
                       MOVE    4               TO  WRK-SEL-IDX
                   WHEN    "SELNUM05"
                       MOVE    5               TO  WRK-SEL-IDX
               END-EVALUATE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       5
                       OR      SPA-ERRCD   NOT =   SPACE
                   IF    ( SPA-GMN-T09-SELNUM (IDY)
                                       =   ZERO        ) 
                   OR    ( IDY         =   WRK-SEL-IDX )
                       CONTINUE
                   ELSE
                       IF      SPA-GMN-T09-SELNUM (WRK-SEL-IDX)
                                       =   SPA-GMN-T09-SELNUM (IDY)
                           MOVE    ZERO            TO
                                       SPA-GMN-T09-SELNUM (WRK-SEL-IDX)
                           MOVE    5               TO  IDY
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    範囲チェック
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       5
                   OR      SPA-ERRCD   NOT =   SPACE
               IF      SPA-GMN-T09-SELNUM (IDX)    =   ZERO
                   CONTINUE
               ELSE
                   IF      SPA-GMN-T09-SELNUM (IDX)
                                               >   SPA-GMN-T09-MAX
                       MOVE    "0001"              TO  SPA-ERRCD
                       MOVE    ZERO                TO
                                       SPA-GMN-T09-SELNUM (IDX)
                       MOVE    IDX                 TO  WRK-ERR-IDX
                   END-IF
               END-IF
           END-PERFORM
      *
      *    選択行チェック
           IF      SPA-ERRCD           =   SPACE
             IF      FLG-TOUROKU         =   ZERO
                   IF      SPA-GMN-T09-SELNUM (WRK-SEL-IDX)    =   ZERO
                       CONTINUE
                   ELSE
                       MOVE    SPA-GMN-T09-SELNUM (WRK-SEL-IDX)
                                                       TO  WRK-SELNO
                       MOVE    WRK-SEL-IDX             TO  IDX
                       PERFORM 410211-SELNUM-HENSYU-SEC
                       IF      SPA-ERRCD   NOT =   SPACE
                           MOVE    IDX                 TO  WRK-ERR-IDX
                       END-IF
                   END-IF
             END-IF
           END-IF
      *
      *    空欄をつめる
           MOVE    SPA-GMN-T09-SELNUM-OCC
                                       TO  WRK-GMN-T09-SELNUM-OCC
           INITIALIZE                      SPA-GMN-T09-SELNUM-OCC
           MOVE    ZERO                TO  IDX
      *
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       5
               IF      WRK-GMN-T09-SELNUM (IDY)    =   ZERO
                   CONTINUE
               ELSE
                   ADD     1                   TO  IDX
                   MOVE    WRK-GMN-T09-SELNUM (IDY)
                                               TO  SPA-GMN-T09-SELNUM
                                                               (IDX)
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD   NOT =   SPACE
               MOVE    WRK-ERR-IDX     TO  SPA-GMN-T09-CUR
           ELSE
               IF      IDX             =   5
                   MOVE    99              TO  SPA-GMN-T09-CUR
               ELSE
                   COMPUTE SPA-GMN-T09-CUR =  IDX +   1
               END-IF    
           END-IF    
            .
        41021-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択行チェック処理
      *****************************************************************
        410211-SELNUM-HENSYU-SEC               SECTION.
      *
      *    患者番号重複チェック
           IF      SPA-NAI-T06-BODY-SELNUM   >   ZERO      
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       5
                       OR      SPA-ERRCD   NOT =   SPACE
                   IF    ( IDY                 NOT =   IDX  )
                   AND   ( SPA-GMN-T09-SELNUM (IDY)
                                               >   ZERO     )
                       MOVE    SPA-GMN-T09-SELNUM (IDY)
                                                     TO  IDZ
                       IF      SPA-GMN-T09-TPTNUM (WRK-SELNO)
                                       NOT =   SPA-GMN-T09-TPTNUM (IDZ)
                           MOVE    "0003"          TO  SPA-ERRCD
                           MOVE    ZERO            TO
                                               SPA-GMN-T09-SELNUM (IDX)
                           GO  TO  410211-SELNUM-HENSYU-EXT
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      SPA-GMN-T09-TCHK (WRK-SELNO)
                                   =   "1"
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    ZERO            TO  SPA-GMN-T09-SELNUM (IDX)
               GO  TO  410211-SELNUM-HENSYU-EXT
           END-IF
      *
      *    直接請求する保険者とのチェック（社保）
           IF    ( SPA-GMN-T06-TEISYUTUSAKI1
                                       =   "T"   )
           AND   ( SPA-NAI-T06-DIRECT-HKNJANUM
                                   NOT =   SPACE )
               EVALUATE    SPA-NAI-T06-DIRECT-HKNJANUM
                   WHEN    "00000000"
                       PERFORM VARYING IDYY    FROM    1   BY  1
                               UNTIL   IDYY    >       10
                               OR      SPA-NAI-T06-DIRECT-HKNJANUML
                                                               (IDYY)
                                               =   SPACE
                           IF      SPA-NAI-T09-THKNJANUM-S (WRK-SELNO)
                                           =
                                       SPA-NAI-T06-DIRECT-HKNJANUML
                                                               (IDYY)
                               MOVE    "0027"              TO  SPA-ERRCD
                               MOVE    ZERO                TO
                                               SPA-GMN-T09-SELNUM (IDX)
                               GO  TO  410211-SELNUM-HENSYU-EXT
                           END-IF
                       END-PERFORM
                   WHEN    OTHER
                       IF      SPA-NAI-T09-THKNJANUM-S (WRK-SELNO)
                                           =
                                           SPA-NAI-T06-DIRECT-HKNJANUM
                           CONTINUE
                       ELSE
                           MOVE    "0027"              TO  SPA-ERRCD
                           MOVE    ZERO                TO
                                               SPA-GMN-T09-SELNUM (IDX)
                           GO  TO  410211-SELNUM-HENSYU-EXT
                       END-IF
               END-EVALUATE
           END-IF
      *
      *    レセ電データ存在チェック
           IF      SPA-NAI-T09-TRECEDEN (WRK-SELNO)
                                   =   "1"
               MOVE    "0010"              TO  SPA-ERRCD
               MOVE    ZERO                TO  SPA-GMN-T09-SELNUM (IDX)
               GO  TO  410211-SELNUM-HENSYU-EXT
           END-IF
      *
      *    平成２４年３月以前の月後れ分が改正パッチ後に再作成されているか
           IF      SPA-NAI-T09-TRECEDEN (WRK-SELNO)
                                       =   "9"
               MOVE    "0020"              TO  SPA-ERRCD
               MOVE    ZERO                TO  SPA-GMN-T09-SELNUM (IDX)
               GO  TO  410211-SELNUM-HENSYU-EXT
           END-IF
      *
      *    返戻指示設定時に返戻データに設定されているか
           IF      SPA-NAI-T09-THENREI (WRK-SELNO)
                                       =   "1"
               MOVE    "0011"              TO  SPA-ERRCD
               MOVE    ZERO                TO  SPA-GMN-T09-SELNUM (IDX)
               GO  TO  410211-SELNUM-HENSYU-EXT
           END-IF
      *
      *    主科未設定データ存在チェック
           IF      SPA-NAI-T09-TSYUKA (WRK-SELNO)
                                       =   "1"
               MOVE    "0018"              TO  SPA-ERRCD
               MOVE    ZERO                TO  SPA-GMN-T09-SELNUM (IDX)
               GO  TO  410211-SELNUM-HENSYU-EXT
           END-IF
      *
      *    既に請求済かどうか
           IF      SPA-NAI-T09-TSEIKYU (WRK-SELNO)
                                   =   "1"
               MOVE    "1002"          TO  SPA-TIDCD
               MOVE    IDX             TO  SPA-NAI-T09-SELNUM
               GO  TO  410211-SELNUM-HENSYU-EXT
           END-IF
      *
      *    作成日のチェック
           IF      SPA-NAI-T09-TCREYMD (WRK-SELNO)
                                   <   SPA-NAI-T09-TUKEYMD (WRK-SELNO)      
               MOVE    "1006"          TO  SPA-TIDCD
               MOVE    IDX             TO  SPA-NAI-T09-SELNUM
               GO  TO  410211-SELNUM-HENSYU-EXT
           END-IF 
      *
      *    返戻年月未入力のときの返戻データ存在チェック
           IF      SPA-NAI-T09-THENREIYM (WRK-SELNO)
                                       =   "1"
               MOVE    "1011"              TO  SPA-TIDCD
               MOVE    IDX             TO  SPA-NAI-T09-SELNUM
               GO  TO  410211-SELNUM-HENSYU-EXT
           END-IF
           IF      SPA-NAI-T09-THENREIYM (WRK-SELNO)
                                       =   "2"
               MOVE    "1012"              TO  SPA-TIDCD
               MOVE    IDX             TO  SPA-NAI-T09-SELNUM
               GO  TO  410211-SELNUM-HENSYU-EXT
           END-IF
      *
          .
        410211-SELNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       4105-RECESEL-SEC               SECTION.
      *     
           PERFORM VARYING     IDXX    FROM    1   BY  1
                   UNTIL       IDXX    >       SPA-GMN-T09-MAX
               MOVE    ZERO                TO  WRK-SELNO
               IF    ( T09-SELECT (IDXX)   =   "T" )
               AND   ( SPA-NAI-T09-TSELECT (IDXX)
                                           =   "F" )
                   MOVE    IDXX            TO  WRK-SELNO
               ELSE
                   IF    ( T09-SELECT (IDXX)   =   "F" )
                   AND   ( SPA-NAI-T09-TSELECT (IDXX)
                                               =   "T" )
                       MOVE    IDXX             TO  WRK-SELNO
                   END-IF
               END-IF
      *
               IF      WRK-SELNO           >   ZERO
                   IF      T09-SELECT (IDXX)   =   "T"
                       MOVE    ZERO                TO  FLG-SET
      *                空欄があれば追加
                       PERFORM VARYING     IDX     FROM    1   BY  1
                               UNTIL       IDX     >       5
                               OR          FLG-SET >       ZERO
                           IF      SPA-GMN-T09-SELNUM (IDX)    =   ZERO
                               MOVE    WRK-SELNO   TO
                                                   SPA-GMN-T09-SELNUM
                                                               (IDX)
                               MOVE    IDX         TO  FLG-SET
                           END-IF
                       END-PERFORM
      *
      *                追加できないとき
                       IF      FLG-SET             =   ZERO
                           MOVE    "0004"          TO  SPA-ERRCD
                           MOVE    99              TO  SPA-GMN-T09-CUR 
                           GO  TO  4105-RECESEL-EXT
                       END-IF
      *
      *                選択行チェック
                       MOVE    FLG-SET             TO  IDX  
                       PERFORM 410211-SELNUM-HENSYU-SEC
                       IF      SPA-ERRCD   NOT =   SPACE
                           MOVE    FLG-SET         TO  SPA-GMN-T09-CUR
                       ELSE
                           IF      FLG-SET         =   5
                               MOVE    99          TO  SPA-GMN-T09-CUR
                           ELSE
                               COMPUTE SPA-GMN-T09-CUR =   FLG-SET +   1
                          END-IF    
                       END-IF
                   END-IF     
                   IF      T09-SELECT (IDXX)   =   "F"
                       PERFORM VARYING     IDX     FROM    1   BY  1
                               UNTIL       IDX     >       5
                           IF      SPA-GMN-T09-SELNUM (IDX)
                                                   =   WRK-SELNO
                               MOVE    ZERO    TO  SPA-GMN-T09-SELNUM
                                                                  (IDX)
                               MOVE    5       TO  IDX 
                           END-IF
                       END-PERFORM
                   END-IF     
               END-IF
           END-PERFORM
           .
       4105-RECESEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定ボタン押下時チェック処理
      *****************************************************************
       4101-KAKUTEI-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-T06-RECE-MAX    TO  WRK-RECE-MAX
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       5
               IF      SPA-GMN-T09-SELNUM (IDX)    >   ZERO
                   ADD     1                       TO   WRK-RECE-MAX
               END-IF
           END-PERFORM
      *    選択項目があるか
           IF      WRK-RECE-MAX    =   SPA-GMN-T06-RECE-MAX
               MOVE    "0005"          TO  SPA-ERRCD
           ELSE
      *        選択項目がすべて一覧に追加できるか
               IF      SPA-GMN-T06-SELNUM  >   ZERO
                   COMPUTE WRK-RECE-MAX    =   WRK-RECE-MAX    -   1
               END-IF
               IF      WRK-RECE-MAX    >   WRK-CONS-LINE-MAX
                   MOVE    "0099"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4101-KAKUTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK                    SECTION.
      *   
           MOVE    "T06"               TO  SPA-SAKIPG
           MOVE    "T09"               TO  SPA-MOTOPG
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
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
      *    確定処理
      *****************************************************************
       450-KAKUTEI-SEC                SECTION.
      *
           MOVE    1                  TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  450-KAKUTEI-EXT
           END-IF
      *
           MOVE    "OK"            TO  SPA-TID1-FLG
      *
           PERFORM 210-BACK
           .
       450-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    個人履歴表示
      *****************************************************************
       400-T07-SYORI-SEC           SECTION.
      *
           INITIALIZE                      T07
           MOVE    SPA-GMN-T09-TPTNUM (SPA-NAI-T09-SELNUM)
                                           TO  T07-PTNUM
           MOVE    SPA-GMN-T09-TNAME  (SPA-NAI-T09-SELNUM)
                                           TO  T07-NAME
           MOVE    SPA-GMN-T06-SRYYM       TO  T07-SRYYM
           IF      SPA-GMN-T06-TEISYUTUSAKI1
                                       =   "T"
               MOVE    "社保"              TO  T07-TEISYUTUSAKI
           ELSE
               IF      SPA-GMN-T06-TEISYUTUSAKI4
                                           =   "T"
                   MOVE    "労災"              TO  T07-TEISYUTUSAKI
               ELSE
                   MOVE    "国保"              TO  T07-TEISYUTUSAKI
               END-IF
           END-IF
      *
           MOVE    ZERO            TO  IDX
           INITIALIZE                  ONRECE-B-REC
           MOVE    SPA-HOSPNUM     TO  ONRECE-B-HOSPNUM
           MOVE    SPA-NAI-T06-SRYYM-H
                                   TO  ONRECE-B-SRYYM
           MOVE    SPA-NAI-T09-RECE10-TSRYYM (SPA-NAI-T09-SELNUM)
                                   TO  ONRECE-B-SRYYM-B
           MOVE    SPA-NAI-T09-RECE10-TPTID (SPA-NAI-T09-SELNUM)
                                   TO  ONRECE-B-PTID
           IF      SPA-GMN-T06-TEISYUTUSAKI1
                                       =   "T"
               MOVE    "1"                 TO  ONRECE-B-TEISYUTUSAKI
                                               ONRECE-B-XXTEISYUTUSAKI
           ELSE
               IF      SPA-GMN-T06-TEISYUTUSAKI4
                                           =   "T"
                   MOVE    "3"             TO  ONRECE-B-TEISYUTUSAKI
                                               ONRECE-B-XXTEISYUTUSAKI
               ELSE
                   MOVE    "6"             TO  ONRECE-B-TEISYUTUSAKI
                   MOVE    "2"             TO  ONRECE-B-XXTEISYUTUSAKI
               END-IF
           END-IF
           MOVE    SPA-NAI-T09-RECE10-TNYUGAIKBN (SPA-NAI-T09-SELNUM)
                                   TO  ONRECE-B-NYUGAIKBN
      *
           MOVE    "tbl_onrece_body"
                                   TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 900-ONRECE-BODY-SELECT-SEC
      *
           PERFORM         UNTIL   FLG-ONRECE-B    =   1
               ADD     1               TO  IDX
               MOVE    IDX             TO  WRK-ZZ9
               MOVE    WRK-ZZ9         TO  T07-TRENNUM (IDX)
               IF      ONRECE-B-KAKUTEI-KBN
                                       =   "1"
                   MOVE    "済"            TO  T07-TSEIKYU (IDX)
               END-IF
               MOVE    ONRECE-B-SKYYMD TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG     TO  T07-TSKYYMD (IDX)
               MOVE    ONRECE-B-SCNT   TO  WRK-Z9
               MOVE    WRK-Z9          TO  T07-TSCNT   (IDX)
               MOVE    ONRECE-B-UKEYMD TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG     TO  T07-TUKEYMD (IDX)
               MOVE    ONRECE-B-UCNT   TO  WRK-ZZ9
               MOVE    WRK-ZZ9         TO  T07-TUCNT   (IDX)
               EVALUATE    ONRECE-B-NYUGAIKBN
                   WHEN    "1"
                       MOVE    "入"        TO  T07-TNYUGAIKBN (IDX)
                   WHEN    "2"
                       MOVE    "外"        TO  T07-TNYUGAIKBN (IDX)
               END-EVALUATE
               MOVE    ONRECE-B-RECESYUBETU
                                       TO  T07-TRECESYUBETU   (IDX)
               MOVE    ONRECE-B-HKNJANUM
                                       TO  T07-THKNJANUM      (IDX)
               IF      ONRECE-B-RECESYUBETU    =   "1212"  OR  "1211"
                   STRING  ONRECE-B-FTNNUM (1) DELIMITED   BY  SPACE
                           "　"                DELIMITED   BY  SIZE
                           ONRECE-B-JKYNUM (1) DELIMITED   BY  SPACE
                                               INTO    T07-TKIGO (IDX)
                   END-STRING
               ELSE
                   STRING  ONRECE-B-KIGO   DELIMITED   BY  SPACE
                           "　"            DELIMITED   BY  SIZE
                           ONRECE-B-NUM    DELIMITED   BY  SPACE
                                           INTO    T07-TKIGO (IDX)
                   END-STRING
               END-IF
      * 
               MOVE    IDX             TO  T07-LIST-COUNT
      * 
               MOVE    "tbl_onrece_body"
                                       TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 900-ONRECE-BODY-FETCH-SEC
           END-PERFORM
      * 
           MOVE    "tbl_onrece_body"
                                   TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      IDX             =   ZERO
               GO  TO  400-T07-SYORI-EXT
           END-IF
      *
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "T09"                TO  SPA-MOTOPG
           MOVE    "T07"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
       400-T07-SYORI-EXT.
           EXIT.
      *
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
               EVALUATE    SPA-TIDCD
                   WHEN    "1002"
                       PERFORM 530-TID2SET-SEC
                       GO  TO  500-SET-SCREEN-EXT
                   WHEN    OTHER
                       PERFORM 520-TIDSET-SEC
                       GO  TO  500-SET-SCREEN-EXT
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "T09"               TO  MCP-WINDOW
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
           INITIALIZE                   T09
      *
           MOVE    SPA-GMN-T06-PTNUM   TO  T09-PTNUM
           MOVE    SPA-GMN-T06-NAME    TO  T09-NAME
           MOVE    SPA-GMN-T06-SRYYM   TO  T09-SRYYM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       5
               MOVE    SPA-GMN-T09-SELNUM  (IDX)
                                           TO  WRK-Z9
               MOVE    WRK-Z9              TO  T09-SELNUM (IDX)
           END-PERFORM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-T09-MAX
               MOVE    SPA-GMN-T09-TRENNUM      (IDX)
                                       TO  WRK-Z9
               MOVE    WRK-Z9          TO  T09-TRENNUM      (IDX)
               MOVE    SPA-GMN-T09-TPTNUM       (IDX)
                                       TO  T09-TPTNUM       (IDX)
               IF      SPA-NAI-T06-BODY-SELNUM =   ZERO
                   MOVE    SPA-GMN-T09-TNAME       (IDX)
                                           TO  T09-TNAME        (IDX)
               ELSE
                   MOVE    SPA-NAI-T09-TKANANAME   (IDX)
                                           TO  T09-TNAME        (IDX)
               END-IF
      *
      *        同じ患者番号のとき表示しない
               IF      IDX             >   1
                   COMPUTE IDY     =   IDX     -   1
                   IF      SPA-GMN-T09-TPTNUM  (IDX)
                                       =   SPA-GMN-T09-TPTNUM (IDY)
                       MOVE    SPACE           TO  T09-TPTNUM (IDX)
                                                   T09-TNAME  (IDX)
                   END-IF
               END-IF
      *
               MOVE    SPA-GMN-T09-TNYUGAIKBN   (IDX)
                                       TO  T09-TNYUGAIKBN   (IDX)
               MOVE    SPA-GMN-T09-TRECESYUBETU (IDX)
                                       TO  T09-TRECESYUBETU (IDX)
               MOVE    SPA-GMN-T09-THKNJANUM    (IDX)
                                       TO  T09-THKNJANUM    (IDX)
               MOVE    SPA-GMN-T09-THOJOKBN     (IDX)
                                       TO  T09-THOJOKBN     (IDX)
               MOVE    SPA-GMN-T09-TKIGO        (IDX)
                                       TO  T09-TKIGO        (IDX)
               MOVE    SPA-GMN-T09-TRECEIPT-KBN (IDX)
                                       TO  T09-TRECEIPT-KBN (IDX)
               MOVE    SPA-GMN-T09-TDISASTER (IDX)
                                       TO  T09-TDISASTER    (IDX)
               MOVE    SPA-GMN-T09-TCREYMD      (IDX)
                                       TO  T09-TCREYMD      (IDX)
               IF      SPA-GMN-T09-TCHK (IDX)  =   "1"
                   MOVE    "*"             TO  T09-TCHK     (IDX)
               END-IF
               MOVE    "F"                 TO   T09-SELECT (IDX)
               IF      IDX                 =   SPA-GMN-T09-SELNUM (1)
                                           OR  SPA-GMN-T09-SELNUM (2)
                                           OR  SPA-GMN-T09-SELNUM (3)
                                           OR  SPA-GMN-T09-SELNUM (4)
                                           OR  SPA-GMN-T09-SELNUM (5)
                   MOVE    "T"                 TO  T09-SELECT (IDX)
               END-IF
               MOVE    T09-SELECT (IDX)
                                   TO  SPA-NAI-T09-TSELECT (IDX)
           END-PERFORM
           MOVE    IDX             TO  T09-COUNT
      *
           IF      SPA-GMN-T06-TEISYUTUSAKI4
                                       =   "T"
               MOVE    "年金証書番号等"    TO  T09-HYODAI1
           ELSE
               MOVE    "記号・番号"        TO  T09-HYODAI1
           END-IF
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
      *****AND   ( SPA-GMN-T09-CUR     =   ZERO  )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
         EVALUATE    SPA-GMN-T09-CUR
             WHEN    1
                    MOVE  "SELNUM01"     TO  MCP-WIDGET
             WHEN    2
                    MOVE  "SELNUM02"     TO  MCP-WIDGET
             WHEN    3
                    MOVE  "SELNUM03"     TO  MCP-WIDGET
             WHEN    4
                    MOVE  "SELNUM04"     TO  MCP-WIDGET
             WHEN    5
                    MOVE  "SELNUM05"     TO  MCP-WIDGET
             WHEN    99
                    MOVE  "B12"          TO  MCP-WIDGET   
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
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       5
               IF      SPA-GMN-T09-SELNUM (IDXX)
                                       =   ZERO
                   MOVE    IDXX            TO  SPA-GMN-T09-CUR
                   GO  TO  50011-INPUT-CUR-EXT
               END-IF
           END-PERFORM
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SELNUM01"
                   MOVE    2               TO  SPA-GMN-T09-CUR
               WHEN    "SELNUM02"
                   MOVE    3               TO  SPA-GMN-T09-CUR
               WHEN    "SELNUM03"
                   MOVE    4               TO  SPA-GMN-T09-CUR
               WHEN    "SELNUM04"
                   MOVE    5               TO  SPA-GMN-T09-CUR
               WHEN    "SELNUM05"
                   MOVE    99              TO  SPA-GMN-T09-CUR
               WHEN    OTHER
                   MOVE    99              TO  SPA-GMN-T09-CUR
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
           MOVE    SPACE                TO  TERR
           MOVE    SPA-ERRCD            TO  TERR-ERRCODE
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号入力エラー"        TO  TERR-ERRMSG
               WHEN    "0002"
                   MOVE    "選択番号が重複しています"  TO  TERR-ERRMSG
               WHEN    "0003"
                   MOVE    "複数の患者は、同時に選択できません。"
                                                       TO  TERR-ERRMSG
               WHEN    "0004"
                   MOVE    "これ以上データの追加はできません"
                                                       TO  TERR-ERRMSG
               WHEN    "0005"
                   MOVE    "データが選択してありません"
                                                       TO  TERR-ERRMSG
               WHEN    "0008"
                   MOVE    "個別指示一覧に選択済です"  TO  TERR-ERRMSG
               WHEN    "0010"
                   MOVE    "対象のレセ電データがありません"
                                                       TO  TERR-ERRMSG
               WHEN    "0011"
                   MOVE    "返戻データが設定されていません"
                                                       TO  TERR-ERRMSG
               WHEN    "0018"
                   MOVE    "主科未設定データです"      TO  TERR-ERRMSG
               WHEN    "0020"
                   STRING  "改正パッチ提供以降に平成２４年３月以前の"
                                                   DELIMITED  BY  SIZE
                     "月遅れ分についてレセプト再処理が行われていません"
                                                   DELIMITED  BY  SIZE
                                                   INTO    TERR-ERRMSG
                   END-STRING
               WHEN    "0027"
                   MOVE    "選択した保険者と違います"  TO  TERR-ERRMSG
               WHEN    "0099"
                   MOVE    "選択したデータを全て追加できません"
                                                       TO  TERR-ERRMSG
      *
               WHEN    "9999"
                   MOVE    "他端末で使用中です。"
                                               TO  TERR-ERRMSG
                   MOVE    SLOCK-MSG           TO  TERR-ERRMSG(21:)
           END-EVALUATE
      *
           MOVE    TERR-ERRMSG          TO  SPA-ERRMSG
      *
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "T09"                TO  SPA-MOTOPG
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
      *    エラーメッセージ表示理
      *****************************************************************
       520-TIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  TID1
           INITIALIZE                      TID1
           MOVE    SPA-TIDCD           TO  TID1-ID1CODE
      *
           EVALUATE    SPA-TIDCD
               WHEN    "1006"
                   STRING   "請求データの作成日がレセ送信日以前ですが"
                                       DELIMITED  BY  SIZE
                            "個別指示の対象としますか（"
                                       DELIMITED  BY  SIZE
                            SPA-GMN-T09-TCREYMD (SPA-NAI-T09-SELNUM)
                                       DELIMITED  BY  SIZE
                            "）"       DELIMITED  BY  SIZE
                                       INTO    TID1-ID1MSG
                   END-STRING
               WHEN    "1011"
                   STRING   "返戻データが存在します（返戻設定無し）。"
                                       DELIMITED  BY  SIZE
                            "対象としますか"
                                       DELIMITED  BY  SIZE
                                       INTO    TID1-ID1MSG
                   END-STRING
               WHEN    "1012"
                   STRING   "保険情報が一致する返戻データが存在します"
                                       DELIMITED  BY  SIZE
                            "（返戻設定無し）。対象としますか"
                                       DELIMITED  BY  SIZE
                                       INTO    TID1-ID1MSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    "いいえ"         TO  TID1-B01-LABEL
           MOVE    "は  い"         TO  TID1-B12-LABEL
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "T09"               TO  SPA-MOTOPG
           MOVE    "TID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "TID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END              
      *
           .
       520-TIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示処理
      *****************************************************************
       530-TID2SET-SEC              SECTION.
      *
           MOVE    SPACE                TO  TID2
           INITIALIZE                       TID2
           MOVE    SPA-TIDCD            TO  TID2-ID1CODE
      *
           EVALUATE    SPA-TIDCD
               WHEN    "1002"
                   MOVE    "既に請求済ですが個別指示の対象としますか"
                                           TO  TID2-ID1MSG
           END-EVALUATE
      *
           MOVE    "F"                  TO  TID2-B10-VISIBLE
           MOVE    "T"                  TO  TID2-B11-VISIBLE
           MOVE    "F11:個人履歴"       TO  TID2-B11-LABEL
           MOVE    "F12:作成する"       TO  TID2-B12-LABEL
           MOVE    "blue"               TO  TID2-LABEL-STYLE
           MOVE    "black"              TO  TID2-ID1MSG-STYLE
      *
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "T09"                TO  SPA-MOTOPG
           MOVE    "TID2"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "TID2"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       530-TID2SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD (1:6)          =   ZERO    OR  SPACE
               MOVE    SPACE                   TO  WRK-HENYMDG
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
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SEIKYU
           ELSE
               MOVE    1               TO  FLG-SEIKYU
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                      PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-N-SEC       SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTINF-REC
               MOVE    ZERO                TO  FLG-PTINF
           ELSE
               MOVE    1                   TO  FLG-PTINF
                INITIALIZE                  PTINF-REC
           END-IF
      *
           .
       900-PTINF-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細ファイルSELECT処理
      *****************************************************************
       900-ONRECE-BODY-SELECT-SEC     SECTION.
      *
           MOVE    ONRECE-B-REC        TO  MCPDATA-REC
      *
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-ONRECE-BODY-FETCH-SEC
           ELSE
               INITIALIZE                      ONRECE-B-REC
               MOVE  1                     TO  FLG-ONRECE-B
           END-IF
           .
       900-ONRECE-BODY-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細ファイルFETCH処理
      *****************************************************************
       900-ONRECE-BODY-FETCH-SEC      SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE  MCPDATA-REC           TO  ONRECE-B-REC
               MOVE  ZERO                  TO  FLG-ONRECE-B
           ELSE
               INITIALIZE                      ONRECE-B-REC
               MOVE  1                     TO  FLG-ONRECE-B
           END-IF
           .
       900-ONRECE-BODY-FETCH-EXT.
           EXIT.
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
