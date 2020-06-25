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
       PROGRAM-ID.                               ORCGZ99.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 点数マスタ設定
      *  コンポーネント名  : 点数マスタ用入力コード設定画面（Ｚ９９）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-多々納   01/05/23  表示キー追加
      * 01.01.01     MCC-藤原     01/09/26  JOINの修正      
      * 01.00.03     MCC-多々納   02/07/18  インターフェースの変更
      * 01.00.04     NACL-多々納  05/12/12  MONFUNC対応
      * 05.00.00     NACL-多々納  18/10/XX  登録済みエラ―表示対応
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                                      DIVISION.
       FILE                                      SECTION.
      *
       WORKING-STORAGE                           SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    点数マスタ設定用共通パラメタ
           COPY    "Z01COMMON-SPA".
      *
      *    第一画面ＳＰＡエリア
           COPY    "Z01SCR-SPA".
      *
      *    画面用ＳＰＡ
      *01  SPA-Z99.
      *    03  SPA-Z99-AREA.
      *        05  SPA-GMN-PAGE        PIC 9(02).
      *        05  SPA-GMN-MAX         PIC 9(04).
      *        05  SPA-GMN-IDX         PIC 9(04).
      *        05  SPA-GMN-CUR         PIC 9(02).
      *        05  SPA-GMN-MOTOPG      PIC X(08).
      *        05  SPA-GMN-AREA.
      *            07  SPA-GMN-SRYCDH            PIC X(20).
      *            07  SPA-GMN-NAMEH             PIC X(100).
      *            07  SPA-GMN-TBL.
      *                09  SPA-GMN-TBL-G         OCCURS  23. 
      *                    11  SPA-GMN-HYOUJI    PIC X(04).
      *                    11  SPA-GMN-INPUTCD   PIC X(20).
      *                    11  SPA-GMN-DSPNAME   PIC X(100).
      *********************11  SPA-NAI-CDSYU     PIC X(01).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                           PIC 9(01).
      *
           03  FLG-INPUTCD                       PIC 9(01).
           03  FLG-SYORI                         PIC 9(01).
           03  FLG-KANJI                         PIC 9(01).
           03  FLG-KANA                          PIC 9(01).
           03  FLG-SUJI                          PIC 9(01).
      *
           03  FLG-TOUROKU                       PIC 9(01).
      *****03  FLG-SYORI                         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR                           PIC 9(06).
           03  CNT-PAGE                          PIC 9(02).
           03  CNT-ERRCHK                        PIC 9(03).
           03  CNT-STRING                        PIC 9(03).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDXX                              PIC 9(02).
           03  IDXY                              PIC 9(02).
           03  IDXZ                              PIC 9(02).
      *
           03  IDX                               PIC 9(04).
           03  IDY                               PIC 9(04).
           03  IDX-C                             PIC 9(04).
      *
      *    ワーク領域
       01  WRK-AREA.
      *    入力項目名編集用 
           03  WRK-IDX2            PIC 9(02).
           03  WRK-WIDGET          PIC X(20).
           03  WRK-PATH            PIC X(64).
           03  WRK-MCP-WIDGET      PIC X(20).
      *
           03  WRK-STRINGTBL.
               05  WRK-STRING                    PIC X(01)  OCCURS 20.
      *
           03  WRK-CDSYU               PIC X(01).
      *
           03  WRK-IDX                 PIC 9(04).
      *
      *    削除用ワーク
       01  WRK-GMN-REC.
           03  WRK-GMN-TBL.
               05  WRK-GMN-TBL-G         OCCURS  10. 
                   07  WRK-GMN-HYOUJI    PIC X(04).
                   07  WRK-GMN-INPUTCD   PIC X(20).
                   07  WRK-GMN-DSPNAME   PIC X(100).
                   07  WRK-NAI-CDSYU     PIC X(01).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    入力コード
        01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
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
           MOVE    SPA-COMMON                    TO  SPA-AREA.
           MOVE    SPA-WORK                      TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE                      TO  SPA-Z01FREE
      *****MOVE    SPA-Z01SONOTA                 TO  SPA-Z99
      *
           MOVE    SPACE                         TO  SPA-ERRCD.
           MOVE    ZERO                          TO  FLG-END.
      *
           EVALUATE    MCP-STATUS       ALSO    MCP-EVENT
               WHEN    "LINK"           ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI-SEC
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY-SEC
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END                       =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF.
      *    
           MOVE    SPA-Z01KYOUTU                 TO  SPA-WORK
           MOVE    SPA-AREA                      TO  SPA-COMMON
      *****MOVE    SPA-Z99                       TO  SPA-Z01SONOTA 
           MOVE    SPA-Z01FREE                   TO  SPA-FREE
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                              SECTION.
      *
      *    DISPLAY "*** ORCSNPL    START  ***"
      *
           INITIALIZE                            FLG-AREA.
           INITIALIZE                            CNT-AREA.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "ZERR"
               PERFORM 5001-MAPCUR-SEC
               MOVE    SPACE               TO  SPA-MOTOPG
           ELSE
      *
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO      TO      100-INIT-EXT
               END-IF
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
      ***  MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    SPACE                TO  MCP-PUTTYPE
           MOVE   "Z99"                TO  MCP-WINDOW.
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
       300-SCREEN-SEC             SECTION.
      *
           MOVE    SPA-MOTOPG      TO  SPA-Z99GMN-MOTOPG
      *
      *    画面用スパ編集処理
           PERFORM                 310-SPA-SET-SEC
      *
           MOVE    1               TO  SPA-Z99GMN-PAGE
           MOVE    1               TO  SPA-Z99GMN-CUR
           MOVE    1               TO  SPA-Z99GMN-IDX
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    共通ＳＰＡ情報をＳＰＡにセット
      *****************************************************************
       310-SPA-SET-SEC             SECTION.
      *
           INITIALIZE                  SPA-Z99GMN-TBL
      *
           MOVE  SPA-Z01-SRYCD         TO  SPA-Z99GMN-SRYCDH
           MOVE  SPA-Z01-NAMEH         TO  SPA-Z99GMN-NAMEH 
      *
           PERFORM  VARYING  IDXX      FROM  1  BY  1
                    UNTIL    IDXX      >  10
               MOVE  SPA-Z99-HYOUJI (IDXX) TO  SPA-Z99GMN-HYOUJI (IDXX)
               MOVE  SPA-Z99-INPUTCD(IDXX) TO  SPA-Z99GMN-INPUTCD(IDXX)
      *********MOVE  SPA-Z99-DSPNAME(IDXX) TO  SPA-Z99GMN-DSPNAME(IDXX)
               MOVE  SPA-Z99-CDSYU(IDXX)   TO  SPA-Z99NAI-CDSYU  (IDXX)
           END-PERFORM
           .
      *
        310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI-SEC                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    "CHANGE"        TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *        確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM  412-KAKUTEI-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY-SEC                             SECTION.
      *
      *    入力項目の決定
           IF      MCP-EVENT           =   "ACTIVATE"
               PERFORM 2001-INPUT-SET-SEC
           END-IF
      *
           MOVE    ZERO                TO  SPA-Z99GMN-CUR
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目の決定
      *****************************************************************
       2001-INPUT-SET-SEC               SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *    入力行NOの設定
           MOVE    ZERO                TO  WRK-IDX2
           MOVE    SPACE               TO  WRK-WIDGET
           IF      MCP-WIDGET(1:07)    =   "INPUTCD"
               MOVE    MCP-WIDGET(8:2)     TO  WRK-IDX2
               MOVE    MCP-WIDGET(1:7)     TO  WRK-WIDGET
           END-IF
           IF      MCP-WIDGET(1:6)     =   "HYOUJI"
               MOVE    MCP-WIDGET(7:2)     TO  WRK-IDX2
               MOVE    MCP-WIDGET(1:6)     TO  WRK-WIDGET
           END-IF
      *
      *    画面は、１ページとする
      *****MOVE    WRK-IDX2            TO  SPA-Z99GMN-IDX
      *
           .
       2001-INPUT-SET-EXT.
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
      *    登録時、必須入力チェック
           IF      FLG-TOUROKU         =   1
               PERFORM 4109-TOUROKU-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      410-INPUT-CHK-EXT
               END-IF
           END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録時力チェック処理
      *****************************************************************
       4109-TOUROKU-CHK-SEC          SECTION.
      *
           IF     (SPA-Z99GMN-INPUTCD(1)   NOT =   SPACE )  AND
                  (SPA-Z99GMN-INPUTCD(1)(3:)   =   SPACE )  AND
                  (SPA-Z99NAI-CDSYU  (1)       =   "J"   )
      *        表示のキーが全角１文字数字はエラー
               IF      SPA-Z99GMN-INPUTCD(1)(1:2)  >=  "０"  AND
                                                   <=  "９"
                   MOVE    "0011"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-Z99GMN-CUR
                   MOVE    01              TO  SPA-Z99GMN-IDX
               END-IF
           END-IF
      *    画面表示コードが１０文字以上をエラーとする
           IF     (SPA-Z99GMN-INPUTCD(1)   NOT =   SPACE )  AND
                  (SPA-Z99GMN-INPUTCD(1)(11:)
                                           NOT =   SPACE )
               MOVE    "0013"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-Z99GMN-CUR
               MOVE    01                  TO  SPA-Z99GMN-IDX
           END-IF
      *
           .
      *
       4109-TOUROKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
               MOVE    Z99-INPUTCD  (IDX)
                                       TO  SPA-Z99GMN-INPUTCD (IDX)
               MOVE    Z99-HYOUJI   (IDX)
                                       TO  SPA-Z99GMN-HYOUJI  (IDX)
           END-PERFORM
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC           SECTION.
      *
      *    空白行削除
           PERFORM 420-SYOKYO-SEC
      *    入力コード
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   10  )   OR
                              (SPA-Z99GMN-INPUTCD(IDX)
                                               =   SPACE )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               PERFORM 41011-INPUTCD-HEN-SEC
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *    表示キー
           PERFORM 41023-HYOUJI-HEN-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       41011-INPUTCD-HEN-SEC           SECTION.
      *
           MOVE    IDX                 TO  SPA-Z99GMN-IDX
      *    入力コードチェック
           PERFORM 410111-INPUTCD-CHK-SEC
      *
      *    エラー行入力へ
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    1                   TO  SPA-Z99GMN-CUR
           END-IF
      *
           .
       41011-INPUTCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入力コードチェック処理
      *****************************************************************
       410111-INPUTCD-CHK-SEC           SECTION.
      *
      *    漢字入力チェック
           MOVE    ZERO               TO  FLG-KANJI
           MOVE    ZERO               TO  FLG-KANA
           MOVE    ZERO               TO  FLG-SUJI
           MOVE    ZERO               TO  CNT-STRING
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-Z99GMN-INPUTCD(SPA-Z99GMN-IDX)
                                       TO  KANACHK-MAE-INPUT
           MOVE    20                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-Z99GMN-INPUTCD
                                               (SPA-Z99GMN-IDX)
           IF      KANACHK-RC      NOT =   ZERO
      *        全角半角混在エラー
               MOVE    "0007"          TO  SPA-ERRCD
           ELSE
               MOVE    KANACHK-MAX     TO  CNT-STRING
      *        間に空白がある時、エラー
               IF      KANACHK-RC2     NOT =   ZERO
                   MOVE    "0012"          TO  SPA-ERRCD
                   GO      TO      410111-INPUTCD-CHK-EXT
               END-IF
      *
      *        全角
               IF      KANACHK-SYUBETU     =   2
                   MOVE    1               TO  FLG-KANJI
                   IF      KANACHK-MAX         >   10
                       MOVE    "0006"              TO  SPA-ERRCD
                   END-IF
      *            全角空白はエラー
                   IF     KANACHK-RC3      NOT =   ZERO
                       MOVE    "0014"      TO  SPA-ERRCD
                   END-IF
               END-IF
      *        半角
               IF      KANACHK-SYUBETU     =   1
                   IF      SPA-Z99GMN-INPUTCD(SPA-Z99GMN-IDX)
                                                  (1:CNT-STRING)
                                                   NUMERIC
      *                数字桁数チェック
                       IF      KANACHK-MAX         <   4   OR  >  6
                           MOVE    "0002"              TO  SPA-ERRCD
                       END-IF
                   ELSE
      *                S,Pチェック
                       IF      SPA-Z99GMN-INPUTCD(SPA-Z99GMN-IDX)(1:1)
                                                   =   "S"   OR  "P"
                           MOVE    "0008"      TO  SPA-ERRCD
                       END-IF
      *                #,$チェック
                       IF      SPA-Z99GMN-INPUTCD(SPA-Z99GMN-IDX)(1:1)
                                                   =   "#"   OR  "$"
                           MOVE    "0010"      TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410111-INPUTCD-CHK-EXT
           END-IF
      *    記号は入力不可とする
           IF      FLG-KANJI           =   1
               MOVE    2                   TO  IDX-C
           ELSE
               MOVE    1                   TO  IDX-C
           END-IF
           PERFORM VARYING     IDY     FROM    1   BY  IDX-C
                   UNTIL      (IDY         >   20  )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      IDX-C               =   1
                   IF       SPA-Z99GMN-INPUTCD(SPA-Z99GMN-IDX)(IDY:1)
                                               =   "+"   OR  "-"
                                               OR  "/"   OR  "!"
                                               OR  "."   OR  "*"
                                               OR  "_"
                       MOVE    "0010"      TO  SPA-ERRCD
                   END-IF
               ELSE
                   IF     SPA-Z99GMN-INPUTCD(SPA-Z99GMN-IDX) (IDY:2)
                                               =   "＋"   OR  "−"
                                               OR  "／"   OR  "！"
                                               OR  "．"   OR  "＊"
                       MOVE    "0010"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
      *
      *    セットコード存在チェック
           PERFORM 410112-DB-INPUTCD-CHK-SEC
      *
      *    同一コードチェック
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY         >   10  )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-Z99GMN-IDX         NOT =   IDY
                   IF     (SPA-Z99GMN-INPUTCD(SPA-Z99GMN-IDX)
                                           NOT =   SPACE    )  AND
                          (SPA-Z99GMN-INPUTCD(SPA-Z99GMN-IDX)
                                           =   SPA-Z99GMN-INPUTCD(IDY))
                       MOVE    "0005"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
      *
      *    ＯＫで、１行目の時、表示キーとする
      *D   IF      SPA-ERRCD           =   SPACE
      *D       IF      SPA-Z99GMN-IDX         =   1
      *D           MOVE    "◎"        TO  SPA-Z99GMN-HYOUJI
      *D                                               (SPA-Z99GMN-IDX)
      ***          表示キーは最大９桁とする
      *            IF      CNT-STRING          >   8
      *                MOVE    "0009"          TO  SPA-ERRCD
      **           END-IF
      *D       END-IF
      *D   END-IF
      *
           .
       410111-INPUTCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    セットコード存在チェック
      *****************************************************************
       410112-DB-INPUTCD-CHK-SEC        SECTION.
      *
      *    入力コード使用チェック
           INITIALIZE                  INPUTCD-REC
      *****MOVE    SPA-HOSPID          TO  ICD-HOSPID
           MOVE    SPA-Z99GMN-INPUTCD(SPA-Z99GMN-IDX)
                                       TO  ICD-INPUTCD
           IF      FLG-KANJI           =   1
               MOVE    "J"                 TO  ICD-CDSYU
           ELSE
      *        全桁数字は、コード種別を桁数で決定
               IF      SPA-Z99GMN-INPUTCD(SPA-Z99GMN-IDX)(1:CNT-STRING)
                                           NUMERIC
                   EVALUATE    CNT-STRING
                       WHEN    4
                           MOVE    "4"                 TO  ICD-CDSYU
                       WHEN    5
                           MOVE    "5"                 TO  ICD-CDSYU
                       WHEN    6
                           MOVE    "6"                 TO  ICD-CDSYU
                   END-EVALUATE
               ELSE
                   MOVE    "A"                 TO  ICD-CDSYU
               END-IF
           END-IF
      *
           MOVE    ICD-CDSYU           TO  WRK-CDSYU
      *
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM                     920-INPUTCD-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
           IF     (FLG-INPUTCD         =   ZERO  )  AND
                  (SPA-Z99GMN-INPUTCD(SPA-Z99GMN-IDX)
                                       =   ICD-INPUTCD )
               IF      ICD-SRYCD           NOT =   SPA-Z01-SRYCD
                   MOVE    "0003"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        コード種別編集
               MOVE    WRK-CDSYU       TO
                                       SPA-Z99NAI-CDSYU (SPA-Z99GMN-IDX)
           END-IF
           .
       410112-DB-INPUTCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為名入力処理
      *****************************************************************
      *41012-DSPNAME-HEN-SEC        SECTION.
      *
      *    MOVE    Z99-DSPNAME(SPA-Z99GMN-IDX)
      *                                TO  SPA-Z99GMN-DSPNAME
      *                                                (SPA-Z99GMN-IDX)
      *    IF   (SPA-Z99GMN-INPUTCD (SPA-Z99GMN-IDX)       =   SPACE )
      *    AND  (SPA-Z99GMN-DSPNAME (SPA-Z99GMN-IDX)   NOT =   SPACE )
      *        MOVE    "0004"              TO  SPA-ERRCD
      *        GO      TO      41012-DSPNAME-HEN-EXT
      *    END-IF
      *
      *    MOVE    1                   TO  SPA-Z99GMN-CUR
      *    ADD     1                   TO  SPA-Z99GMN-IDX
      *
      *    .
      *41012-DSPNAME-HEN-EXT.
      *    EXIT.
      *****************************************************************
      *    表示キー入力処理
      *****************************************************************
       41023-HYOUJI-HEN-SEC        SECTION.
      *    ◎以外の入力を求める（最初を表示キーとする)
           MOVE    ZERO                TO  WRK-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   10  )   OR
                              (WRK-IDX     NOT =   ZERO )
               IF      SPA-Z99GMN-HYOUJI (IDX)     =   SPACE
                   CONTINUE
               ELSE
                   IF      IDX                     =   1
                       IF      SPA-Z99GMN-INPUTCD (IDX)
                                                   =   SPACE
                           MOVE    SPACE           TO  SPA-Z99GMN-HYOUJI
                                                         (IDX)
                       END-IF
                   ELSE
                       MOVE    IDX             TO  WRK-IDX
                       MOVE    "◎"            TO  SPA-Z99GMN-HYOUJI
                                                         (IDX)
                   END-IF
               END-IF
           END-PERFORM
      *
      *    対象がない時、最初の行を表示キーとする
           IF      WRK-IDX             =   ZERO
               MOVE    1               TO  WRK-IDX
           END-IF
      *
      *    キー並び替え
           PERFORM 410131-HYOUJI-HENKAN-SEC
      *
      *    表示キー設定後、削除
           PERFORM 420-SYOKYO-SEC
      *
           .
       41023-HYOUJI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    表示キー入力処理
      *****************************************************************
       41013-HYOUJI-HEN-SEC        SECTION.
      *
      *
           IF      SPA-Z99GMN-HYOUJI (SPA-Z99GMN-IDX)   =   SPACE
               MOVE    1                   TO  SPA-Z99GMN-CUR
               IF      SPA-Z99GMN-IDX         =   1
                   MOVE    "◎"                TO  SPA-Z99GMN-HYOUJI
                                                      (SPA-Z99GMN-IDX)
               END-IF
               PERFORM 410132-HOUJI-CHK-SEC
               GO      TO      41013-HYOUJI-HEN-EXT
           ELSE
      *        入力のあった時、先頭へ
               IF      SPA-Z99GMN-IDX     NOT =   1
                   PERFORM 410131-HYOUJI-HENKAN-SEC
               END-IF
               MOVE    "◎"                TO  SPA-Z99GMN-HYOUJI(01)
               IF      SPA-Z99GMN-INPUTCD (01)    =   SPACE
                   MOVE    ZERO                TO  SPA-Z99GMN-IDX
               END-IF
               PERFORM 410132-HOUJI-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      41013-HYOUJI-HEN-EXT
               END-IF
           END-IF
      *
           MOVE    1                   TO  SPA-Z99GMN-CUR
           ADD     1                   TO  SPA-Z99GMN-IDX
      *
           .
       41013-HYOUJI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    表示キー桁数チェック処理
      *****************************************************************
       410132-HOUJI-CHK-SEC              SECTION.
      *
           MOVE    ZERO               TO  CNT-STRING
           PERFORM VARYING IDX    FROM    1   BY  1
                   UNTIL   IDX        >   20
      *
               IF      SPA-Z99GMN-INPUTCD(01)(IDX:1)
                                          =    SPACE
                   CONTINUE
               ELSE
                   MOVE    IDX                 TO  CNT-STRING
               END-IF
           END-PERFORM
      *
      *****IF      CNT-STRING          >   8
      *        MOVE    "0009"              TO  SPA-ERRCD
      *        MOVE    1                   TO  SPA-Z99GMN-CUR
      *        MOVE    1                   TO  SPA-Z99GMN-IDX
      *****END-IF
      *
           .
       410132-HOUJI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    表示キー並び替え処理
      *****************************************************************
       410131-HYOUJI-HENKAN-SEC              SECTION.
      *
           MOVE    SPA-Z99GMN-TBL         TO  WRK-GMN-TBL
           INITIALIZE                      SPA-Z99GMN-TBL
           MOVE    1                   TO  IDY
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >  10
      *********IF      IDX             =   SPA-Z99GMN-IDX
               IF      IDX             =   WRK-IDX
                   MOVE    WRK-GMN-TBL-G (IDX)
                                       TO  SPA-Z99GMN-TBL-G (01)
               ELSE
                   ADD     1           TO  IDY
                   MOVE    WRK-GMN-TBL-G (IDX)
                                       TO  SPA-Z99GMN-TBL-G (IDY)
                   MOVE    SPACE       TO  SPA-Z99GMN-HYOUJI(IDY)
               END-IF
           END-PERFORM
      *
           .
       410131-HYOUJI-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    １行削除処理
      *****************************************************************
       420-SYOKYO-SEC              SECTION.
      *
           MOVE    SPA-Z99GMN-TBL         TO  WRK-GMN-TBL
           INITIALIZE                      SPA-Z99GMN-TBL
           MOVE    ZERO                TO  SPA-Z99GMN-MAX
           MOVE    ZERO                TO  FLG-SYORI
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >  10
               IF     (WRK-GMN-HYOUJI (IDX)    =   SPACE )  AND
                      (WRK-GMN-INPUTCD(IDX)    =   SPACE )
                   CONTINUE
               ELSE
                   ADD     1               TO  SPA-Z99GMN-MAX
                   MOVE    WRK-GMN-TBL-G   (IDX) 
                                           TO  SPA-Z99GMN-TBL-G
                                                       (SPA-Z99GMN-MAX)
                   IF      SPA-Z99GMN-MAX     NOT =   IDX
                       MOVE    1               TO  FLG-SYORI
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    "◎"                TO  SPA-Z99GMN-HYOUJI(01)
      *
      **** 削除の時、画面の最終行の次へ
      *    IF      FLG-SYORI           =   1
      *        COMPUTE SPA-Z99GMN-IDX     =   SPA-Z99GMN-MAX   +   1
      **** END-IF
      *
           .
       420-SYOKYO-EXT.
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
           MOVE    "Z99"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  Z99
           INITIALIZE                      Z99
      *
      *    MOVE    SPA-Z99GMN-SRYCDH      TO  Z99-SRYCDH
      *    MOVE    SPA-Z99GMN-NAMEH       TO  Z99-NAMEH
      *
           PERFORM VARYING  IDX    FROM  1  BY  1  
                   UNTIL    IDX        >  10
               MOVE    SPA-Z99GMN-HYOUJI (IDX)    TO  Z99-HYOUJI (IDX)
               MOVE    SPA-Z99GMN-INPUTCD(IDX)    TO  Z99-INPUTCD(IDX)
      *********MOVE    SPA-Z99GMN-DSPNAME(IDX)    TO  Z99-DSPNAME(IDX)
           END-PERFORM
      *
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
           IF     (SPA-MOTOPG      NOT =   "ZERR" ) AND
                  (SPA-ERRCD           =   SPACE  )
               IF      WRK-WIDGET(1:07)    =   "INPUTCD"
                   ADD     1                   TO  WRK-IDX2
                   IF      SPA-Z99GMN-INPUTCD (WRK-IDX2)
                                               =   SPACE
                       MOVE    ZERO            TO  WRK-IDX2
                   END-IF
               ELSE
                   MOVE    ZERO            TO  WRK-IDX2
               END-IF
      *        最後の空白行へカーソルセット
               IF      WRK-IDX2            =   ZERO
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL       IDX     >   10
                       IF      SPA-Z99GMN-INPUTCD (IDX)
                                                   =   SPACE
                           MOVE    IDX             TO  WRK-IDX2
                           MOVE    10              TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
               IF      WRK-IDX2       =   ZERO
                   MOVE    00              TO  SPA-Z99GMN-CUR
               ELSE
                   MOVE    01              TO  SPA-Z99GMN-CUR
               END-IF
               MOVE    WRK-IDX2            TO  SPA-Z99GMN-IDX
           ELSE
               MOVE    SPA-Z99GMN-IDX      TO  WRK-IDX2
           END-IF
      *
      **   MOVE    SPA-Z99GMN-IDX         TO  WRK-IDX2
      *    IF      WRK-IDX2            =   ZERO
      *        MOVE    1               TO  WRK-IDX2
      *    END-IF
      *
      *    IF      WRK-IDX2            >   10
      *        MOVE    00              TO  SPA-Z99GMN-CUR
      *    ELSE
      *        MOVE    01              TO  SPA-Z99GMN-CUR
      *****END-IF
      *
           EVALUATE    SPA-Z99GMN-CUR
               WHEN    01
                   MOVE    "INPUTCD"       TO  MCP-WIDGET
                   MOVE    WRK-IDX2        TO  MCP-WIDGET(8:2)
               WHEN    03
                   MOVE    "HYOUJI"        TO  MCP-WIDGET
                   MOVE    WRK-IDX2        TO  MCP-WIDGET(7:2)
               WHEN    00
                   MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE                         TO  SPA-ERRMSG.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "名称を入力してください"
                                                 TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "入力コードは４桁から６桁で入力してください"
                                                 TO  SPA-ERRMSG
               WHEN    "0003"
      *            MOVE
      *            "この入力コードは既に別の診療コードで使われています"
      *                                          TO  SPA-ERRMSG
      *H30.10
                   STRING  "この入力コードは既に別の診療コード"
                           "（"
                           ICD-SRYCD
                           "）"
                           "で使われています。"
                                                DELIMITED  BY  SIZE 
                                                INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0004"
                   MOVE    "入力時はコードを入力してください"
                                                 TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "入力コードが重複しています"
                                                 TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "漢字コードは５文字までです"
                                                 TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "漢字との混在はできません"
                                                 TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "Ｐ，Ｓは使用できません"
                                                 TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "表示キーは９文字までです"
                                                 TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE
                   "入力コードには使用できない記号があります。"
                                                 TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE
                   "表示入力コードに全角１文字の数字は使用できません"
                                                 TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "文字の間に空白は入力できません。"
                                                 TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "表示入力コードは半角１０文字、"
                                                 TO  SPA-ERRMSG
                   MOVE    "全角５文字までにして下さい。"
                                                 TO  SPA-ERRMSG(31:)
               WHEN    "0014"
                   STRING  "全角空白があります。"
                           "全角空白は入力できません。"
                                        DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
           END-EVALUATE.
      *
           MOVE    SPACE               TO  ZERR
           MOVE    SPA-ERRCD           TO  ZERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  ZERR-ERRMSG
      *
           MOVE    "Z99"               TO  SPA-MOTOPG
           MOVE    "ZERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "ZERR"              TO  MCP-WINDOW
           MOVE    "B01"               TO  MCP-WIDGET
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
        510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPA-Z99GMN-MOTOPG        TO  SPA-SAKIPG.
      *
           MOVE    "Z99"                 TO  SPA-MOTOPG.
      *
      ***  MOVE    "CHANGE"              TO  MCP-PUTTYPE.
           MOVE    "JOIN"                TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                             TO  FLG-END
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認処理
      *****************************************************************
       412-KAKUTEI-SEC                           SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF     SPA-ERRCD        NOT =   SPACE
               GO      TO      412-KAKUTEI-EXT
           END-IF
      *
      *    削除行取消
      *    PERFORM 420-SYOKYO-SEC
      *
      *    エラーチェック
      *    PERFORM VARYING     SPA-Z99GMN-IDX FROM    1  BY  1
      *            UNTIL      (SPA-Z99GMN-IDX >   10  )  OR
      *                       (SPA-ERRCD   NOT =   SPACE )
      *        PERFORM 410111-INPUTCD-CHK-SEC
      *    END-PERFORM
      *
      *    IF     SPA-ERRCD        NOT =   SPACE
      *        COMPUTE SPA-Z99GMN-IDX     =   SPA-Z99GMN-IDX    -   1
      *        GO      TO      412-KAKUTEI-EXT
      *    END-IF
      *
      *    入力内容をＰＲＧ共通ＳＰＡにセット
           PERFORM  VARYING  IDX       FROM  1  BY  1
                    UNTIL    IDX       >  10
               MOVE    SPA-Z99GMN-HYOUJI (IDX)
                                      TO  SPA-Z99-HYOUJI (IDX)
               MOVE    SPA-Z99GMN-INPUTCD(IDX)
                                      TO  SPA-Z99-INPUTCD(IDX)
               MOVE    SPA-Z99NAI-CDSYU  (IDX)
                                      TO  SPA-Z99-CDSYU  (IDX)
           END-PERFORM
      *
      *    前画面に戻る
           MOVE  "JOIN"               TO  MCP-PUTTYPE
           PERFORM  210-BACK
      *
           .
       412-KAKUTEI-EXT.
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
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBLOSEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                            SECTION.
      *
           MOVE   "PUTWINDOW"                    TO  MCP-FUNC.
           CALL   "ORCDBMAIN"                    USING
                                                     MCPAREA
                                                     MCPDATA-REC
                                                     SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
