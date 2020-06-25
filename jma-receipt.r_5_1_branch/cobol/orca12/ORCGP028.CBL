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
       PROGRAM-ID.             ORCGP028.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 患者個別設定（Ｐ０２８）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  13/07/22    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.00    NACL-多々納  13/11/26  患者個別設定追加対応
      *  04.07.00    NACL-多々納  14/02/XX  患者介護情報追加対応
      *  04.07.00    NACL-多々納  14/03/XX  地域包括診療対象疾病対応
      *  05.00.00    NACL-多々納  16/12/15  患者個人番号対応
      *  04.08.00    NACL-多々納  16/12/XX  指導料等自動算定対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *    患者画面スパ領域
           COPY    "P02SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PARA            PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-HKNJAINFCARE    PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-INPUT           PIC 9(01).
           03  FLG-KANJI           PIC 9(01).
           03  FLG-TENSU-OK        PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-ARI             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDH1                PIC 9(01).
      *
           03  IDK                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-SELNUM          PIC 9(04).
      *
      *H28.12
           03  WRK-SRYCD               PIC X(09).
           03  WRK-INPUTCD             PIC X(22).
           03  WRK-CD-MCNT             PIC 9(02).
           03  WRK-ICD-CDSYU           PIC X(01).
           03  WRK-SRYCD-NAME          PIC X(80).
           03  WRK-KEI-IDX             PIC 9(02).
           03  WRK-KEICD               PIC X(04).
      *
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT       PIC X(400).
           03  WRK-ATO-INPUT       PIC X(400).
           03  WRK-MAX             PIC 9(04).
      *
      *    指導料対象外テーブル
       01  TBL-SINSATU-AREA.
      *    小児科外来診察料(院外処方 初診)
           03  FILLER              PIC X(09)   VALUE   "113003510".
      *    小児科外来診察料(院内処方 初診)
           03  FILLER              PIC X(09)   VALUE   "113003710".
      *    小児科外来診察料(院外処方 再診)
           03  FILLER              PIC X(09)   VALUE   "113003610".
      *    小児科外来診察料(院内処方 再診)
           03  FILLER              PIC X(09)   VALUE   "113003810".
      *    小児科かかりつけ診察料(院外処方 初診)
           03  FILLER              PIC X(09)   VALUE   "113019910".
      *    小児科かかりつけ診察料(院内処方 初診)
           03  FILLER              PIC X(09)   VALUE   "113019710".
      *    小児科かかりつけ診察料(院外処方 再診)
           03  FILLER              PIC X(09)   VALUE   "113020010".
      *    小児科かかりつけ診察料(院内処方 再診)
           03  FILLER              PIC X(09)   VALUE   "113019810".
       01  TBL-SINSATU-AREAR       REDEFINES   TBL-SINSATU-AREA.
           03  TBL-SINSATU-OCC         OCCURS  8     INDEXED  TBL-IDX.
      *        診療コード
               05  TBL-SRYCD           PIC X(09).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    介護保険者号マスタ
       01  HKNJAINFCARE-REC.
           COPY    "CPHKNJAINFCARE.INC".
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    保存点数マスタ−
      *01  TNSWK-TENSU-REC.
      **** COPY    "CPTENSUWK.INC".
           COPY    "CPTENSU.INC"      REPLACING
                                     //TNS-// BY //TNSWK-//.
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
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
           COPY    "LINKAREA".
      *
      *
       01  SCRAREA.
           COPY    "ORCA12SCRAREA.INC".
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
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  SPA-FLG-END
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
           IF      SPA-FLG-END         =   ZERO
               PERFORM 5001-MAPCUR-SEC
      *********PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA  TO  SPA-FREE
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
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
      *    初期画面編集
           PERFORM 300-SCREEN-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    パラメタファイル検索処理
      **** PERFORM 320-PARAFILE-KENSAKU-SEC
      *
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           EVALUATE    SPA-MOTOPG
      *        検索画面より
               WHEN    "P02Y1"
                   PERFORM 3002-P02Y1-INIT-SEC
      *
               WHEN    OTHER
      *        初期画面編集
                   PERFORM 310-SPASET-SEC
      *
                   MOVE    1                   TO  SPA-GMN28-CUR
           END-EVALUATE
      *
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索一覧画面より
      *****************************************************************
       3002-P02Y1-INIT-SEC            SECTION.
      *
           MOVE    SPA-GMN28-AUTOIDX   TO  IDY
           IF      IDY                 =   ZERO
               MOVE    1               TO  IDY
           END-IF
           IF      SPA-P02Y1-SRYCD-G   =   SPACE
               MOVE    SPACE           TO  SPA-GMN28-AUTOSRYCD(IDY)
               EVALUATE    IDY
                   WHEN    1
                       MOVE    46          TO  SPA-GMN28-CUR
                   WHEN    2
                       MOVE    47          TO  SPA-GMN28-CUR
                   WHEN    3
                       MOVE    48          TO  SPA-GMN28-CUR
               END-EVALUATE
           ELSE
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   10   )
                          OR ( IDY     >   3    )
               IF      SPA-P02Y1-SRYCD (IDX)   NOT =   SPACE
                   MOVE    SPA-P02Y1-SRYCD (IDX)
                                       TO  SPA-GMN28-AUTOSRYCD(IDY)
                   EVALUATE    IDY
                   WHEN    1
                       MOVE    46          TO  SPA-GMN28-CUR
                   WHEN    2
                       MOVE    47          TO  SPA-GMN28-CUR
                   WHEN    3
                       MOVE    48          TO  SPA-GMN28-CUR
                   END-EVALUATE
                   ADD     1           TO  IDY
               END-IF
           END-PERFORM
           END-IF
           INITIALIZE                  SPA-P02Y1-AREA
           INITIALIZE                  SPA-P02Y1GMN-AREA 
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    1                   TO  FLG-CHK
           PERFORM 41012-AUTOSRYCD-CHK-SEC
      *
           .
       3002-P02Y1-INIT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
      *    介護保険情報編集
           IF     (SPA-GMN28-CRHKN-MAX >   ZERO )
              AND (SPA-GMN28-CRHSELNUM =   ZERO )
      *        システム日で有効な保険情報を記載する
               PERFORM 3101-CARAHKN-INIT-SEC
           END-IF
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    介護保険情報編集初期編集処理
      *****************************************************************
       3101-CARAHKN-INIT-SEC              SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN28-CRHKN-MAX
               IF     (SPA-NAI28-TCRHSTRYMD (IDX)  <=  SPA-SYSYMD)
                  AND (SPA-NAI28-TCRHENDYMD (IDX)  >=  SPA-SYSYMD)
                   IF      SPA-GMN28-TCRHKNJANUM (IDX) NOT =   SPACE
                       MOVE    SPA-GMN28-TCRHKNJANUM (IDX)
                                           TO  SPA-GMN28-CR-HKNJANUM
                       PERFORM 41012-CR-HKNJANUM-CHK-SEC
      ***              STRING  "保険者番号　"
      *                        SPA-GMN28-CR-HKNJANUM
      *                                    DELIMITED  BY  SPACE
      *                                    INTO   SPA-P028-CAREHKNMEI
      ***              END-STRING
                   END-IF
                   MOVE    SPACE               TO  SPA-GMN28-CR-HKNJANUM
                   MOVE    SPA-GMN28-CRHKN-MAX TO  IDX
               END-IF
           END-PERFORM
           .
       3101-CARAHKN-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
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
      *    カーソル指定のない時、入力した項目の次ぎへ
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN28-CUR       =   ZERO )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN28-CUR
               WHEN    01
                   MOVE    "P028_PATIENTID1"    TO  MCP-WIDGET
               WHEN    02
                   MOVE    "P028_AGREEMENT"     TO  MCP-WIDGET
               WHEN    03
                   MOVE    "P028_MNOTE_QRKBN"   TO  MCP-WIDGET
               WHEN    04
                   MOVE    "P028_MNOTE_CSV"     TO  MCP-WIDGET
               WHEN    05
                   MOVE    "P028_JMAMRVER"      TO  MCP-WIDGET
      *
               WHEN    10
                   MOVE    "P028_CRHSELNUM"     TO  MCP-WIDGET
               WHEN    11
                   MOVE    "P028_CRHKNJANUM"    TO  MCP-WIDGET
               WHEN    12
                   MOVE    "P028_CRHIHKNJANUM"  TO  MCP-WIDGET
               WHEN    13
                   MOVE    "P028_CHTEKSTYMD"   TO  MCP-WIDGET
               WHEN    14
                   MOVE    "P028_CHTEKEDYMD"   TO  MCP-WIDGET
               WHEN    15
                   MOVE    "BCRH2"              TO  MCP-WIDGET
               WHEN    20
                   MOVE    "P028_CRSELNUM"      TO  MCP-WIDGET
               WHEN    21
                   MOVE    "P028_CAREJOTAI"     TO  MCP-WIDGET
               WHEN    22
                   MOVE    "P028_CRNINYMD"      TO  MCP-WIDGET
               WHEN    23
                   MOVE    "P028_CRTEKSTYMD"    TO  MCP-WIDGET
               WHEN    24
                   MOVE    "P028_CRTEKEDYMD"    TO  MCP-WIDGET
               WHEN    25
                   MOVE    "BCRN2"              TO  MCP-WIDGET
               WHEN    30
                   MOVE    "P028_CHKHKTBYO1"    TO  MCP-WIDGET
               WHEN    31
                   MOVE    "P028_CHKHKTBYO2"    TO  MCP-WIDGET
               WHEN    32
                   MOVE    "P028_CHKHKTBYO3"    TO  MCP-WIDGET
               WHEN    33
                   MOVE    "P028_CHKHKTBYO4"    TO  MCP-WIDGET
      *H28.4
               WHEN    34
                   MOVE    "P028_NNCHIKBN"     TO  MCP-WIDGET
               WHEN    35
                   MOVE    "P028_SYOUNIKBN"    TO  MCP-WIDGET
      *
      *H28.12
               WHEN    40
                   MOVE    "P028_MYSELNUM"     TO  MCP-WIDGET
               WHEN    41
                   MOVE    "P028_MYIDKEY"      TO  MCP-WIDGET
               WHEN    42
                   MOVE    "P028_MY_NUMBER"    TO  MCP-WIDGET
               WHEN    43
                   MOVE    "P028_MY_DESCRIP"   TO  MCP-WIDGET
               WHEN    44
                   MOVE    "BCMY2"             TO  MCP-WIDGET
      *H28.12
               WHEN    46
                   MOVE    "P028_AUTOSRYCD1"   TO  MCP-WIDGET
               WHEN    47
                   MOVE    "P028_AUTOSRYCD2"   TO  MCP-WIDGET
               WHEN    48
                   MOVE    "P028_AUTOSRYCD3"   TO  MCP-WIDGET
      *H30.4   妊婦区分
               WHEN    49
                   MOVE    "P028_NINPUKBN"      TO  MCP-WIDGET
      *
               WHEN    90
                   MOVE    "B08"                TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "P028_PATIENTID1"
                   MOVE    2           TO  SPA-GMN28-CUR
               WHEN    "P028_AGREEMENT"
                   MOVE    3           TO  SPA-GMN28-CUR
               WHEN    "P028_MNOTE_QRKBN"
                   MOVE    4           TO  SPA-GMN28-CUR
               WHEN    "P028_MNOTE_CSV"
                   MOVE    5           TO  SPA-GMN28-CUR
               WHEN    "P028_JMAMRVER"
      *******      MOVE    90          TO  SPA-GMN28-CUR
      *H28.12      MOVE    10          TO  SPA-GMN28-CUR
                   MOVE    40          TO  SPA-GMN28-CUR
      *
               WHEN    "P028_CRHSELNUM"
                   MOVE    11          TO  SPA-GMN28-CUR
               WHEN    "P028_CRHKNJANUM"
                   MOVE    12          TO  SPA-GMN28-CUR
               WHEN    "P028_CRHIHKNJANUM"
                   MOVE    13          TO  SPA-GMN28-CUR
               WHEN    "P028_CHTEKSTYMD"
                   MOVE    14          TO  SPA-GMN28-CUR
               WHEN    "P028_CHTEKEDYMD"
                   MOVE    15          TO  SPA-GMN28-CUR
                   IF     (SPA-GMN28-CR-HKNJANUM   =   SPACE )
                      AND (SPA-GMN28-CR-HIHKNJANUM =   SPACE )
                      AND (SPA-GMN28-CHTEKSTYMD    =   SPACE )
                      AND (SPA-GMN28-CHTEKEDYMD    =   SPACE )
                       MOVE    20          TO  SPA-GMN28-CUR
                   END-IF
      *
               WHEN    "P028_CRSELNUM"
                   MOVE    21          TO  SPA-GMN28-CUR
               WHEN    "P028_CAREJOTAI"
                   MOVE    22          TO  SPA-GMN28-CUR
               WHEN    "P028_CRNINYMD"
                   MOVE    23          TO  SPA-GMN28-CUR
               WHEN    "P028_CRTEKSTYMD"
                   MOVE    24          TO  SPA-GMN28-CUR
               WHEN    "P028_CRTEKEDYMD"
                   IF     (SPA-GMN28-CAREJOTAI     =   SPACE )
                     AND  (SPA-GMN28-CRNINYMD      =   SPACE )
                     AND  (SPA-GMN28-CRTEKSTYMD    =   SPACE )
                     AND  (SPA-GMN28-CRTEKEDYMD    =   SPACE )
      *********        MOVE    90          TO  SPA-GMN28-CUR
                       MOVE    30          TO  SPA-GMN28-CUR
      *H30.4
      *                女性で１６歳以上は妊婦区分へ
                       IF     (SPA-GMN-SEX         =   "2" )
                         AND  (SPA-NAI-NENREI-YY   >=  16  )
                           MOVE    49          TO  SPA-GMN28-CUR
                       END-IF
                   ELSE
                       MOVE    25          TO  SPA-GMN28-CUR
                   END-IF
      *H30.4   妊婦区分
               WHEN    "P028_NINPUKBN"
                   MOVE    30              TO  SPA-GMN28-CUR
      *地域包括対象疾病
               WHEN    "P028_CHKHKTBYO1"
                   MOVE    31              TO  SPA-GMN28-CUR
               WHEN    "P028_CHKHKTBYO2"
                   MOVE    32              TO  SPA-GMN28-CUR
               WHEN    "P028_CHKHKTBYO3"
                   MOVE    33              TO  SPA-GMN28-CUR
               WHEN    "P028_CHKHKTBYO4"
                   MOVE    34              TO  SPA-GMN28-CUR
      *H28.4
               WHEN    "P028_NNCHIKBN"
                   MOVE    35              TO  SPA-GMN28-CUR
               WHEN    "P028_SYOUNIKBN"
      *!!!         MOVE    90              TO  SPA-GMN28-CUR
                   MOVE    46              TO  SPA-GMN28-CUR
      *H28.12
               WHEN    "P028_MYSELNUM"
                   MOVE    41              TO  SPA-GMN28-CUR
               WHEN    "P028_MYIDKEY"
                   MOVE    42              TO  SPA-GMN28-CUR
               WHEN    "P028_MY_NUMBER"
                   MOVE    43              TO  SPA-GMN28-CUR
               WHEN    "P028_MY_DESCRIP"
                   IF     (SPA-GMN28-MYIDKEY   =   SPACE )
                      AND (SPA-GMN28-MYNUMBER  =   SPACE )
                      AND (SPA-GMN28-MYDESCRIP =   SPACE )
                       MOVE    10              TO  SPA-GMN28-CUR
                   ELSE
                       MOVE    44              TO  SPA-GMN28-CUR
                   END-IF
      *H28.12
               WHEN    "P028_AUTOSRYCD1"
                   MOVE    47              TO  SPA-GMN28-CUR
               WHEN    "P028_AUTOSRYCD2"
                   MOVE    48              TO  SPA-GMN28-CUR
               WHEN    "P028_AUTOSRYCD3"
                   MOVE    90              TO  SPA-GMN28-CUR
           END-EVALUATE
      *
           .
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    登録処理
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *
      *H26.1
      *    介護保険者登録
               WHEN    "CLICKED"       ALSO    "BCRH2"
                   PERFORM 4801-CRHKN-TOUROKU-SEC
      *    介護保険者削除
               WHEN    "CLICKED"       ALSO    "BCRH1"
                   PERFORM 4802-CRHKN-DELETE-SEC
      *    介護認定登録
               WHEN    "CLICKED"       ALSO    "BCRN2"
                   PERFORM 4801-CRNIN-TOUROKU-SEC
      *    介護認定削除
               WHEN    "CLICKED"       ALSO    "BCRN1"
                   PERFORM 4802-CRNIN-DELETE-SEC
      *
      *    地域包括診療対象疾病
               WHEN    "CLICKED"       ALSO    "P028_CHKHKTBYO1"
               WHEN    "CLICKED"       ALSO    "P028_CHKHKTBYO2"
               WHEN    "CLICKED"       ALSO    "P028_CHKHKTBYO3"
               WHEN    "CLICKED"       ALSO    "P028_CHKHKTBYO4"
                   PERFORM 4802-CHKHTKBYO-SEC
      *H28.4
      *    認知症地域包括診療対象
               WHEN    "CLICKED"       ALSO    "P028_NNCHIKBN"
                   PERFORM 4802-NNCHIKBN-SEC
      *    小児かかりつけ診療対象
               WHEN    "CLICKED"       ALSO    "P028_SYOUNIKBN"
                   PERFORM 4802-SYOUNIKBN-SEC
      *H30.4
      *    妊婦区分
               WHEN    "CLICKED"       ALSO    "P028_NINPUKBN"
                   PERFORM 4802-NINPUKBN-SEC
      *H28.12
      *    患者個人番号登録
               WHEN    "CLICKED"       ALSO    "BCMY2"
                   PERFORM 4803-MYID-TOUROKU-SEC
      *    患者個人番号削除
               WHEN    "CLICKED"       ALSO    "BCMY1"
                   PERFORM 4802-MYID-DELETE-SEC
      *     以外はエラー
               WHEN    "CLICKED"       ALSO     ANY
                   MOVE    "0110"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-GMN28-CUR
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       490-TOUROKU-SEC             SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力個所のセット
           PERFORM 400-GMN-INPUT-SEC
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        介護保険者リスト選択
               WHEN    ANY             ALSO    "P028_CAREHKNLST"
                   PERFORM 4101-CAREHKNLST-SYORI-SEC
      *        介護リスト選択
               WHEN    ANY             ALSO    "P028_CARENINLST"
                   PERFORM 4101-CARELST-SYORI-SEC
      *H28.12
      *        患者個人番号リスト選択
               WHEN    ANY             ALSO    "P028_MYLIST"
                   PERFORM 4101-MYLIST-SYORI-SEC
               WHEN    OTHER
                   MOVE    ZERO                TO  FLG-TOUROKU
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    介護保険者リスト選択処理
      *****************************************************************
       4101-CAREHKNLST-SYORI-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN28-CRHKN-MAX
               IF      P02-P028-CRHKNLST-SELECT (IDX)  =   "T"
                   IF      IDX               =   SPA-GMN28-CRHSELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX               TO  WRK-SELNUM
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-GMN28-CRHSELNUM
           END-IF
      *
           IF      SPA-GMN28-CRHSELNUM NOT =   ZERO
               MOVE    SPA-GMN28-CRHSELNUM TO  P02-P028-CRHSELNUM
               PERFORM 41012-CRHSELNUM-CHK-SEC
               MOVE    11                  TO  SPA-GMN28-CUR
           END-IF
           .
       4101-CAREHKNLST-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    介護リスト選択処理
      *****************************************************************
       4101-CARELST-SYORI-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN28-CRNIN-MAX
               IF      P02-P028-CARELST-SELECT (IDX)  =   "T"
                   IF      IDX               =   SPA-GMN28-CRSELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX               TO  WRK-SELNUM
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-GMN28-CRSELNUM
           END-IF
      *
           IF      SPA-GMN28-CRSELNUM  NOT =   ZERO
               MOVE    SPA-GMN28-CRSELNUM  TO  P02-P028-CRSELNUM
               PERFORM 41012-CRSELNUM-CHK-SEC
               MOVE    21                  TO  SPA-GMN28-CUR
           END-IF
           .
       4101-CARELST-SYORI-EXT.
           EXIT.
      *
      *H28.12
      *****************************************************************
      *    患者個人番号リスト選択処理
      *****************************************************************
       4101-MYLIST-SYORI-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN28-MYID-MAX
               IF      P02-P028-MYLIST-SELECT (IDX)  =   "T"
                   IF      IDX               =   SPA-GMN28-MYSELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX               TO  WRK-SELNUM
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-GMN28-MYSELNUM
           END-IF
      *
           IF      SPA-GMN28-MYSELNUM  NOT =   ZERO
               MOVE    SPA-GMN28-MYSELNUM  TO  P02-P028-MYSELNUM
               PERFORM 41012-MYSELNUM-CHK-SEC
               MOVE    41                  TO  SPA-GMN28-CUR
           END-IF
           .
       4101-MYLIST-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    地域包括診療対象疾病処理
      *****************************************************************
       4802-CHKHTKBYO-SEC          SECTION.
      *
           MOVE    MCP-WIDGET(15:1)    TO  IDH1
           EVALUATE    SPA-GMN28-CHKHKTBYO (IDH1)
               WHEN    "1"
                   MOVE    "0"         TO  SPA-GMN28-CHKHKTBYO (IDH1)
               WHEN    "0"
               WHEN    SPACE
                   MOVE    "1"         TO  SPA-GMN28-CHKHKTBYO (IDH1)
           END-EVALUATE
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               EVALUATE     IDH1
                   WHEN     1
                       MOVE    31              TO  SPA-GMN28-CUR
                   WHEN     2
                       MOVE    32              TO  SPA-GMN28-CUR
                   WHEN     3
                       MOVE    33              TO  SPA-GMN28-CUR
                   WHEN     4
      *H28.4           MOVE    90              TO  SPA-GMN28-CUR
                       MOVE    34              TO  SPA-GMN28-CUR
               END-EVALUATE
           END-IF
          .
       4802-CHKHTKBYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    認知症地域包括診療対象処理
      *****************************************************************
       4802-NNCHIKBN-SEC        SECTION.
      *
           EVALUATE    SPA-GMN28-NNCHIKBN
               WHEN    "1"
                   MOVE    "0"         TO  SPA-GMN28-NNCHIKBN
               WHEN    "0"
               WHEN    SPACE
                   MOVE    "1"         TO  SPA-GMN28-NNCHIKBN
           END-EVALUATE
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    35              TO  SPA-GMN28-CUR
           END-IF
          .
       4802-NNCHIKBN-EXT.
           EXIT.
      *****************************************************************
      *    小児かかりつけ診療対象処理
      *****************************************************************
       4802-SYOUNIKBN-SEC          SECTION.
      *
           EVALUATE    SPA-GMN28-SYOUNIKBN
               WHEN    "1"
                   MOVE    "0"         TO  SPA-GMN28-SYOUNIKBN
               WHEN    "0"
               WHEN    SPACE
                   MOVE    "1"         TO  SPA-GMN28-SYOUNIKBN
           END-EVALUATE
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
      *H28.12  MOVE    90              TO  SPA-GMN28-CUR
               MOVE    46              TO  SPA-GMN28-CUR
           END-IF
          .
       4802-SYOUNIKBN-EXT.
           EXIT.
      *
      *H30.4
      *****************************************************************
      *    妊婦区分対象処理
      *****************************************************************
       4802-NINPUKBN-SEC        SECTION.
      *
           EVALUATE    SPA-GMN28-NINPUKBN
               WHEN    "1"
                   MOVE    "0"         TO  SPA-GMN28-NINPUKBN
               WHEN    "0"
               WHEN    SPACE
                   MOVE    "1"         TO  SPA-GMN28-NINPUKBN
           END-EVALUATE
      *
           IF      SPA-GMN28-NINPUKBN  =   "1"
      *        男性はエラー
               IF      SPA-GMN-SEX         =   "1"
                   MOVE    "0"             TO  SPA-GMN28-NINPUKBN
                   MOVE    "2853"          TO  SPA-ERRCD
                   MOVE    30              TO  SPA-GMN28-CUR
               END-IF
      *        １６歳未満は警告
               IF      SPA-NAI-NENREI-YY   <   16
                   IF     (SPA-NAI28-KEININP   =   ZERO )
                     AND  (SPA-ERRCD           =   SPACE )
                       MOVE    "K287"          TO  SPA-ERRCD
                       MOVE    1               TO  SPA-NAI28-KEININP
                       MOVE    49              TO  SPA-GMN28-CUR
                   END-IF
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    30              TO  SPA-GMN28-CUR
           END-IF
           .
       4802-NINPUKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN28-CUR
      *
           .
       400-GMN-INPUT-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
      *
      *    登録チェック
           IF      FLG-TOUROKU         =   1
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 4109-TOUROKU-CHK-SEC
               END-IF
           END-IF
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    P02-P028-MNOTE-QRKBN
                                       TO  SPA-GMN28-MNOTE-QRKBN-G
           MOVE    P02-P028-MNOTE-CSV
                                       TO  SPA-GMN28-MNOTE-CSV-G
      *    地域連携ＩＤ
           MOVE    P02-P028-PATIENTID1 TO  SPA-GMN28-PATIENTID1
      *    情報提供同意有無
           MOVE    P02-P028-AGREEMENT
                                       TO  SPA-GMN28-AGREEMENT-G
      *    日医データフォーマット区分
           MOVE    P02-P028-JMAMRVER
                                       TO  SPA-GMN28-JMAMRVER-G
      *
      *    介護保険情報
           MOVE    P02-P028-CRHSELNUM
                                       TO  SPA-GMN28-CRHSELNUM
           MOVE    P02-P028-CRHKNJANUM
                                       TO  SPA-GMN28-CR-HKNJANUM
           MOVE    P02-P028-CRHIHKNJANUM
                                       TO  SPA-GMN28-CR-HIHKNJANUM
           MOVE    P02-P028-CHTEKSTYMD
                                       TO  SPA-GMN28-CHTEKSTYMD
           MOVE    P02-P028-CHTEKEDYMD
                                       TO  SPA-GMN28-CHTEKEDYMD
      *    介護認定者情報
           MOVE    P02-P028-CRSELNUM
                                       TO  SPA-GMN28-CRSELNUM
           MOVE    P02-P028-CAREJOTAI
                                       TO  SPA-GMN28-CAREJOTAICD-G
           MOVE    P02-P028-CRNINYMD   
                                       TO  SPA-GMN28-CRNINYMD
           MOVE    P02-P028-CRTEKSTYMD
                                       TO  SPA-GMN28-CRTEKSTYMD
           MOVE    P02-P028-CRTEKEDYMD
                                       TO  SPA-GMN28-CRTEKEDYMD
      *H28.12
      *    患者個人番号情報
           MOVE    P02-P028-MYSELNUM   TO  SPA-GMN28-MYSELNUM
           MOVE    P02-P028-MYIDKEY    TO  SPA-GMN28-MYIDKEY
           MOVE    P02-P028-MY-NUMBER  TO  SPA-GMN28-MYNUMBER
           MOVE    P02-P028-MY-DESCRIP TO  SPA-GMN28-MYDESCRIP
      *H28.12
      *    指導料等自動算定
           MOVE    P02-P028-AUTOSRYCD1 TO  SPA-GMN28-AUTOSRYCD(1)
           MOVE    P02-P028-AUTOSRYCD2 TO  SPA-GMN28-AUTOSRYCD(2)
           MOVE    P02-P028-AUTOSRYCD3 TO  SPA-GMN28-AUTOSRYCD(3)
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェックと別画面処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    地域連携ＩＤ
           IF     (SPA-ERRCD           =   SPACE )
               PERFORM 4101-PATIENTID1-CHK-SEC
           END-IF
      *    情報提供同意有無
           IF     (SPA-ERRCD           =   SPACE )
               PERFORM 41012-AGREEMENT-CHK-SEC
           END-IF
      *    お薬手帳ＱＲ区分
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-MNOTE-QRKBN-CHK-SEC
           END-IF
      *    お薬手帳ＣＳＶ区分
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-MNOTE-CSV-CHK-SEC
           END-IF
      *    日医データフォーマット区分
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-JMAMRVER-CHK-SEC
           END-IF
      *H28.12
      *    患者個人番号
      *    選択番号
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-MYSELNUM-CHK-SEC
           END-IF
      *    ＩＤ識別
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-MYIDKEY-CHK-SEC
           END-IF
      *    ＩＤ番号
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-MYNUMBER-CHK-SEC
           END-IF
      *    備考
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-MYDESCRIP-CHK-SEC
           END-IF
      *
      *    介護保険情報
      *    選択番号
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-CRHSELNUM-CHK-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-CR-HKNJANUM-CHK-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-CR-HIHKNJANUM-CHK-SEC
           END-IF
      *    開始日付
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-CHTEKSTYMD-CHK-SEC
           END-IF
      *    終了日付
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-CHTEKEDYMD-CHK-SEC
           END-IF
      *    介護認定者情報
      *    選択番号
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-CRSELNUM-CHK-SEC
           END-IF
      *    要介護状態区分
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-CAREJOTAICD-CHK-SEC
           END-IF
      *    認定日付
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-CRNINYMD-CHK-SEC
           END-IF
      *    開始日付
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-CRTEKSTYMD-CHK-SEC
           END-IF
      *    終了日付
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-CRTEKEDYMD-CHK-SEC
           END-IF
      *H28.12
      *    指導料等自動算定
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-AUTOSRYCD-CHK-SEC
           END-IF
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    地域連携ＩＤチェック処理
      *****************************************************************
       4101-PATIENTID1-CHK-SEC          SECTION.
      *
           IF      SPA-GMN28-PATIENTID1    NOT =   SPACE
               MOVE    SPA-GMN28-PATIENTID1    TO  WRK-MAE-INPUT
               MOVE    20                      TO  WRK-MAX
               PERFORM 891-HALF-CHK-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN28-PATIENTID1
               IF      FLG-ERR         NOT =   ZERO
                   MOVE    "2801"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN28-CUR
               END-IF
           END-IF
      *
           .
       4101-PATIENTID1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    お薬手帳ＱＲ区分チェック処理
      *****************************************************************
       41012-AGREEMENT-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-AGREEMENT =   SPACE
               MOVE    SPACE               TO  SPA-GMN28-AGREEMENT-G
           ELSE
      *
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-GMN28-AGREEMENT-MAX)
                              OR  (FLG-OK  =   1                )
                   IF      SPA-GMN28-AGREEMENT
                                       =   SPA-GMN28-AGREEMENT-LST
                                                      (IDX) (1:1)
                       MOVE    SPA-GMN28-AGREEMENT-LST (IDX)
                                           TO  SPA-GMN28-AGREEMENT-G
                       MOVE    1           TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      FLG-OK              =   ZERO
                   MOVE    "2802"          TO  SPA-ERRCD
                   MOVE    2               TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       41012-AGREEMENT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    お薬手帳ＱＲ区分チェック処理
      *****************************************************************
       41012-MNOTE-QRKBN-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-MNOTE-QRKBN   =   SPACE
               MOVE    SPACE           TO  SPA-GMN28-MNOTE-QRKBN-G
           ELSE
      *
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-GMN28-QRKBN-MAX)
                              OR  (FLG-OK  =   1                )
                   IF      SPA-GMN28-MNOTE-QRKBN
                                       =   SPA-GMN28-MNOTE-QRKBN-LST
                                                      (IDX) (1:1)
                       MOVE    SPA-GMN28-MNOTE-QRKBN-LST (IDX)
                                           TO  SPA-GMN28-MNOTE-QRKBN-G
                       MOVE    1           TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      FLG-OK              =   ZERO
                   MOVE    "2803"          TO  SPA-ERRCD
                   MOVE    3               TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       41012-MNOTE-QRKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    お薬手帳ＱＲ区分チェック処理
      *****************************************************************
       41012-MNOTE-CSV-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-MNOTE-CSV   =   SPACE
               MOVE    SPACE           TO  SPA-GMN28-MNOTE-CSV-G
           ELSE
      *
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-GMN28-CSV-MAX)
                              OR  (FLG-OK  =   1                )
                   IF      SPA-GMN28-MNOTE-CSV
                                       =   SPA-GMN28-MNOTE-CSV-LST
                                                      (IDX) (1:1)
                       MOVE    SPA-GMN28-MNOTE-CSV-LST (IDX)
                                           TO  SPA-GMN28-MNOTE-CSV-G
                       MOVE    1           TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      FLG-OK              =   ZERO
                   MOVE    "2804"          TO  SPA-ERRCD
                   MOVE    4               TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       41012-MNOTE-CSV-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日医データフォーマット区分チェック処理
      *****************************************************************
       41012-JMAMRVER-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-JMAMRVER  =   SPACE
               MOVE    SPACE           TO  SPA-GMN28-JMAMRVER-G
           ELSE
      *
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-GMN28-JMAMRVER-MAX)
                              OR  (FLG-OK  =   1                )
                   IF      SPA-GMN28-JMAMRVER
                                       =   SPA-GMN28-JMAMRVER-LST
                                                      (IDX) (1:1)
                       MOVE    SPA-GMN28-JMAMRVER-LST (IDX)
                                           TO  SPA-GMN28-JMAMRVER-G
                       MOVE    1           TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      FLG-OK              =   ZERO
                   MOVE    "2805"          TO  SPA-ERRCD
                   MOVE    5               TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       41012-JMAMRVER-CHK-EXT.
           EXIT.
      *
      *H28.12
      *
      *****************************************************************
      *    患者個人番号番号チェック処理
      *****************************************************************
       41012-MYSELNUM-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-MYSELNUM  NOT =   SPA-MAE28-MYSELNUM
               IF      SPA-GMN28-MYSELNUM  =   ZERO
                   MOVE    SPA-GMN28-MYSELNUM  TO  SPA-MAE28-MYSELNUM
                   INITIALIZE                  SPA-GMN28-MYID-ING
               END-IF
           END-IF
      *
           IF     (SPA-GMN28-MYSELNUM  NOT =   ZERO   )
             AND  (SPA-GMN28-MYSELNUM  NOT =   SPA-MAE28-MYSELNUM)
               IF      SPA-GMN28-MYSELNUM  >   SPA-GMN28-MYID-MAX
                   MOVE    "2840"          TO  SPA-ERRCD
                   MOVE    40              TO  SPA-GMN28-CUR
               ELSE
                   PERFORM 410121-MYNUMBER-HEN-SEC
               END-IF
           END-IF
           .
       41012-MYSELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者個人番号情報編集処理
      *****************************************************************
       410121-MYNUMBER-HEN-SEC           SECTION.
      *
           MOVE    SPA-GMN28-MYSELNUM  TO  SPA-MAE28-MYSELNUM
           MOVE    SPA-GMN28-MYSELNUM  TO  IDX
           MOVE    SPA-GMN28-TMYIDKEY (IDX)
                                       TO  SPA-GMN28-MYIDKEY
           MOVE    SPA-GMN28-TMYNUMBER (IDX)
                                       TO  SPA-GMN28-MYNUMBER
           MOVE    SPA-GMN28-TMYDESCRIP (IDX)
                                       TO  SPA-GMN28-MYDESCRIP
           .
       410121-MYNUMBER-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者個人番号ＩＤ識別チェック処理
      *****************************************************************
       41012-MYIDKEY-CHK-SEC           SECTION.
      *
      *    半角チェック
           IF      SPA-GMN28-MYIDKEY   NOT =   SPACE
               MOVE    SPA-GMN28-MYIDKEY   TO  WRK-MAE-INPUT
               MOVE    10                  TO  WRK-MAX
               PERFORM 891-HALF-CHK-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN28-MYIDKEY
               IF      FLG-ERR         NOT =   ZERO
                   MOVE    "2841"          TO  SPA-ERRCD
                   MOVE    41              TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       41012-MYIDKEY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者個人番号ＩＤ番号チェック処理
      *****************************************************************
       41012-MYNUMBER-CHK-SEC           SECTION.
      *
      *    外字チェック
           IF      SPA-GMN28-MYNUMBER  NOT =   SPACE
               MOVE    SPA-GMN28-MYNUMBER  TO  WRK-MAE-INPUT
               MOVE    20                  TO  WRK-MAX
               PERFORM 891-HALF-CHK02-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN28-MYNUMBER
      *        IF      FLG-ERR         NOT =   ZERO
      *            MOVE    "2842"          TO  SPA-ERRCD
      *            MOVE    42              TO  SPA-GMN28-CUR
      *        END-IF
           END-IF
           .
       41012-MYNUMBER-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者個人番号備考チェック処理
      *****************************************************************
       41012-MYDESCRIP-CHK-SEC           SECTION.
      *
      *    外字チェック
           IF      SPA-GMN28-MYDESCRIP  NOT =   SPACE
               MOVE    SPA-GMN28-MYDESCRIP  TO  WRK-MAE-INPUT
               MOVE    100                  TO  WRK-MAX
               PERFORM 891-HALF-CHK02-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN28-MYDESCRIP
      *        IF      FLG-ERR         NOT =   ZERO
      *            MOVE    "2843"          TO  SPA-ERRCD
      *            MOVE    43              TO  SPA-GMN28-CUR
      *        END-IF
           END-IF
           .
       41012-MYDESCRIP-CHK-EXT.
           EXIT.
      *H28.12
      *
      *****************************************************************
      *    介護選択番号チェック処理
      *****************************************************************
       41012-CRHSELNUM-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-CRHSELNUM  NOT =   SPA-MAE28-CRHSELNUM
               IF      SPA-GMN28-CRHSELNUM  =   ZERO
                   MOVE    SPA-GMN28-CRHSELNUM  TO  SPA-MAE28-CRHSELNUM
                   INITIALIZE                  SPA-GMN28-CRHKN-ING
      *
                   IF     (SPA-GMN28-CRHKN-MAX >   ZERO )
      *                  システム日で有効な保険情報を記載する
                         PERFORM 3101-CARAHKN-INIT-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF     (SPA-GMN28-CRHSELNUM NOT =   ZERO   )
             AND  (SPA-GMN28-CRHSELNUM NOT =   SPA-MAE28-CRHSELNUM)
               IF      SPA-GMN28-CRHSELNUM >   SPA-GMN28-CRHKN-MAX
                   MOVE    "2810"          TO  SPA-ERRCD
                   MOVE    10              TO  SPA-GMN28-CUR
               ELSE
                   PERFORM 410121-CRHKNINF-HEN-SEC
               END-IF
           END-IF
           .
       41012-CRHSELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者介護保険者情報編集処理
      *****************************************************************
       410121-CRHKNINF-HEN-SEC           SECTION.
      *
           MOVE    SPA-GMN28-CRHSELNUM TO  SPA-MAE28-CRHSELNUM
           MOVE    SPA-GMN28-CRHSELNUM TO  IDX
      *
           MOVE    SPA-GMN28-TCRHKNJANUM (IDX)
                                       TO  SPA-GMN28-CR-HKNJANUM
           MOVE    SPA-GMN28-TCRHIHKNJANUM (IDX)
                                       TO  SPA-GMN28-CR-HIHKNJANUM
           MOVE    SPA-GMN28-TCRHSTRYMD (IDX)
                                       TO  SPA-GMN28-CHTEKSTYMD
           MOVE    SPA-GMN28-TCRHENDYMD (IDX)
                                       TO  SPA-GMN28-CHTEKEDYMD
           MOVE    SPA-NAI28-TCRHSTRYMD (IDX)
                                       TO  SPA-NAI28-CHTEKSTYMD
           MOVE    SPA-NAI28-TCRHENDYMD (IDX)
                                       TO  SPA-NAI28-CHTEKEDYMD
      *
           MOVE    SPA-NAI28-TCHKNSKBN (IDX)
                                       TO  SPA-NAI28-CHKNSKBN
      *
           PERFORM 41012-CR-HKNJANUM-CHK-SEC
           .
       410121-CRHKNINF-HEN-EXT.
           EXIT.
      *****************************************************************
      *    介護保険情報チェック処理
      *****************************************************************
       41012-CR-HKNJANUM-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-CR-HKNJANUM   =   SPACE
               GO      TO      41012-CR-HKNJANUM-CHK-EXT
           END-IF
      *
      **** MOVE    SPACE               TO  SPA-P028-CAREHKNMEI
           MOVE    SPACE               TO  SPA-P028-CRHKNJANAME
           MOVE    SPACE               TO  SPA-P028-CRHKNJAADRS
           MOVE    SPACE               TO  SPA-P028-CRHKNJAADRS2
           MOVE    SPACE               TO  SPA-P028-CRHKNJATEL
           IF      SPA-GMN28-CR-HKNJANUM   NOT =   SPACE
               MOVE    SPA-GMN28-CR-HKNJANUM   TO  WRK-MAE-INPUT
               MOVE    6                       TO  WRK-MAX
               PERFORM 891-HALF-CHK-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN28-CR-HKNJANUM
               IF      FLG-ERR         NOT =   ZERO
                   MOVE    "2811"          TO  SPA-ERRCD
                   MOVE    11              TO  SPA-GMN28-CUR
                   GO      TO      41012-CR-HKNJANUM-CHK-EXT
               END-IF
           END-IF
           IF      SPA-GMN28-CR-HKNJANUM   =   SPACE
               GO      TO      41012-CR-HKNJANUM-CHK-EXT
           END-IF
      *    保険者番号　検索チェック
           INITIALIZE                    HKNJAINFCARE-REC
      **** MOVE    SPA-HOSPNUM           TO  HKNJAC-HKNJANUM
           MOVE    SPA-GMN28-CR-HKNJANUM TO  HKNJAC-HKNJANUM
      *
           MOVE    HKNJAINFCARE-REC      TO  MCPDATA-REC
           MOVE    "tbl_hknjainf_care"   TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf_care" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-HKNJAINFCARE-READ-SEC
           ELSE
               INITIALIZE                  HKNJAINFCARE-REC
               MOVE    1               TO  FLG-HKNJAINFCARE
           END-IF
           MOVE    "tbl_hknjainf_care" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    保険者番号あり
           IF      FLG-HKNJAINFCARE    =   ZERO
               MOVE    HKNJAC-HKNJANAME    TO  SPA-P028-CRHKNJANAME
               MOVE    HKNJAC-ADRS         TO  SPA-P028-CRHKNJAADRS
               MOVE    HKNJAC-BANTI        TO  SPA-P028-CRHKNJAADRS2
               MOVE    HKNJAC-TEL          TO  SPA-P028-CRHKNJATEL
           END-IF
           .
       41012-CR-HKNJANUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護被保険情報チェック処理
      *****************************************************************
       41012-CR-HIHKNJANUM-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-CR-HIHKNJANUM   NOT =   SPACE
               MOVE    SPA-GMN28-CR-HIHKNJANUM TO  WRK-MAE-INPUT
               MOVE    10                      TO  WRK-MAX
               PERFORM 891-HALF-CHK-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN28-CR-HIHKNJANUM
               IF      FLG-ERR         NOT =   ZERO
                   MOVE    "2812"          TO  SPA-ERRCD
                   MOVE    12              TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       41012-CR-HIHKNJANUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護保険開始日付チェック処理
      *****************************************************************
       41012-CHTEKSTYMD-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-CHTEKSTYMD    =   SPACE
               MOVE    SPACE               TO  SPA-NAI28-CHTEKSTYMD
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN28-CHTEKSTYMD    TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN28-CHTEKSTYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI28-CHTEKSTYMD
               ELSE
                   MOVE    "2815"          TO  SPA-ERRCD
                   MOVE    13              TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       41012-CRTEKSTYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護保険終了日付チェック処理
      *****************************************************************
       41012-CHTEKEDYMD-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-CHTEKEDYMD    =   SPACE
               MOVE    SPACE               TO  SPA-NAI28-CHTEKEDYMD
           ELSE
      *
               IF      SPA-GMN28-CHTEKEDYMD    =   "9999999"
                   MOVE    ALL "9"         TO  SPA-NAI28-CHTEKEDYMD
                   GO      TO      41012-CHTEKEDYMD-CHK-EXT
               END-IF
      *
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN28-CHTEKEDYMD    TO  SGDAY-INDAY
               CALL    "ORCSGDAY"              USING
                                               ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN28-CHTEKEDYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI28-CHTEKEDYMD
               ELSE
                   MOVE    "2816"          TO  SPA-ERRCD
                   MOVE    14              TO  SPA-GMN28-CUR
                   GO      TO      41012-CHTEKEDYMD-CHK-EXT
               END-IF
           END-IF
      *
           IF      SPA-GMN28-CHTEKSTYMD    NOT =   SPACE
               IF      SPA-GMN28-CHTEKEDYMD    =   SPACE
                   IF     WRK-MCP-WIDGET       =   "P028_CHTEKEDYMD"
                       MOVE    ALL "9"         TO  SPA-NAI28-CHTEKEDYMD
                       MOVE    "9999999"       TO  SPA-GMN28-CHTEKEDYMD
                   END-IF
               END-IF
           END-IF
      *
           IF     (SPA-NAI28-CHTEKSTYMD    NOT =   SPACE )  AND
                  (SPA-NAI28-CHTEKEDYMD    NOT =   SPACE )
      *        開始日＞終了日はエラー
               IF      SPA-NAI28-CHTEKSTYMD    >   SPA-NAI28-CHTEKEDYMD
                   MOVE    "2817"              TO  SPA-ERRCD
                   MOVE    14                  TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       41012-CHTEKEDYMD-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      **    介護保険者情報登録処理
      ****************************************************************
       4802-CRHKN-DELETE-SEC             SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN28-CRHSELNUM  =   ZERO
                   MOVE    "2824"          TO  SPA-ERRCD
                   MOVE    10              TO  SPA-GMN28-CUR
               ELSE
      *            削除処理
                   PERFORM 48021-CRHKN-TBLDEL-SEC
               END-IF
           END-IF
           .
       4802-CRHKN-DELETE-EXT.
           EXIT.
      *
      ****************************************************************
      **    介護保険者情報削除処理
      ****************************************************************
       48021-CRHKN-TBLDEL-SEC             SECTION.
      *
           PERFORM VARYING    IDX      FROM   1   BY  1
                   UNTIL     (IDX      >   SPA-GMN28-CRHKN-MAX)
               IF     (SPA-GMN28-CRHSELNUM =   IDX )
      *H29.12
                   IF      IDX                 =   10
                       INITIALIZE              SPA-GMN28-CR-HKNLST-OCC
                                                              (IDX)
                   ELSE
                       MOVE    SPA-GMN28-CR-HKNLST-OCC (IDX + 1)
                                       TO  SPA-GMN28-CR-HKNLST-OCC(IDX)
                       INITIALIZE              SPA-GMN28-CR-HKNLST-OCC
                                                              (IDX + 1)
                   END-IF
               ELSE
                   IF     (SPA-GMN28-TCHNUM (IDX)  =   ZERO )
                      AND (IDX                 <   10   )
                       MOVE    SPA-GMN28-CR-HKNLST-OCC (IDX + 1)
                                       TO  SPA-GMN28-CR-HKNLST-OCC(IDX)
                       INITIALIZE          SPA-GMN28-CR-HKNLST-OCC
                                                              (IDX + 1)
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  SPA-GMN28-CRHKN-MAX
           PERFORM VARYING    IDX      FROM   1   BY  1
                   UNTIL     (IDX      >   10   )
               IF     (SPA-GMN28-TCRHKNJANUM (IDX) =   SPACE)
                AND   (SPA-GMN28-TCRHIHKNJANUM(IDX) =  SPACE)
                   INITIALIZE          SPA-GMN28-CR-HKNLST-OCC (IDX)
               ELSE
                   MOVE    IDX             TO  SPA-GMN28-TCHNUM (IDX)
                   ADD     1               TO  SPA-GMN28-CRHKN-MAX
               END-IF
           END-PERFORM
      *
           INITIALIZE                  SPA-GMN28-CRHKN-ING
           MOVE    ZERO                TO  SPA-GMN28-CRHSELNUM
           MOVE    ZERO                TO  SPA-MAE28-CRHSELNUM
           MOVE    10                  TO  SPA-GMN28-CUR
      *
           IF     (SPA-GMN28-CRHKN-MAX >   ZERO )
      *        システム日で有効な保険情報を記載する
               PERFORM 3101-CARAHKN-INIT-SEC
           END-IF
      *
           .
       48021-CRHKN-TBLDEL-EXT.
           EXIT.
      *
      ****************************************************************
      **    介護保険者情報登録処理
      ****************************************************************
       4801-CRHKN-TOUROKU-SEC             SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4801-CRHKN-TOUROKU-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4802-CRHKN-TOUROKU-SEC
           END-IF
           .
       4801-CRHKN-TOUROKU-EXT.
           EXIT.
      *
      ****************************************************************
      **    介護保険者情報登録前関連チェック処理
      ****************************************************************
       4801-CRHKN-TOUROKU-CHK-SEC      SECTION.
      *
      *    必須チェック
           IF     (SPA-GMN28-CR-HKNJANUM   =   SPACE )
             AND  (SPA-GMN28-CR-HIHKNJANUM =   SPACE )
             AND  (SPA-ERRCD               =   SPACE )
               MOVE    "2813"          TO  SPA-ERRCD
               MOVE    11              TO  SPA-GMN28-CUR
           END-IF
           IF     (SPA-NAI28-CHTEKSTYMD =   SPACE)
             AND  (SPA-ERRCD           =   SPACE )
               MOVE    "2818"          TO  SPA-ERRCD
               MOVE    13              TO  SPA-GMN28-CUR
           END-IF
           IF     (SPA-NAI28-CHTEKEDYMD =   SPACE)
             AND  (SPA-ERRCD            =   SPACE )
               MOVE    "2819"          TO  SPA-ERRCD
               MOVE    14              TO  SPA-GMN28-CUR
           END-IF
      *
      *    期間重複チェック
           PERFORM VARYING    IDY      FROM   1   BY  1
                   UNTIL     (IDY      >   SPA-GMN28-CRHKN-MAX)
                       OR    (SPA-ERRCD    NOT =   SPACE )
               IF     (SPA-GMN28-CRHSELNUM     NOT =   IDY )
                   IF      (SPA-NAI28-TCRHENDYMD (IDY)
                                               <   SPA-NAI28-CHTEKSTYMD)
                     OR    (SPA-NAI28-TCRHSTRYMD (IDY)
                                               >   SPA-NAI28-CHTEKEDYMD)
                       CONTINUE
                   ELSE
                       MOVE    "2820"          TO  SPA-ERRCD
                       MOVE    13              TO  SPA-GMN28-CUR
                   END-IF
               END-IF
           END-PERFORM
      *
           IF     (SPA-GMN28-CRHSELNUM =   ZERO  )
             AND  (SPA-ERRCD           =   SPACE )
      *        新規追加
               IF      SPA-GMN28-CRHKN-MAX >=  10
                   MOVE    "2814"          TO  SPA-ERRCD
                   MOVE    10              TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       4801-CRHKN-TOUROKU-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      **    介護保険者情報登録処理
      ****************************************************************
       4802-CRHKN-TOUROKU-SEC             SECTION.
      *
           IF      SPA-GMN28-CRHSELNUM  =   ZERO
               ADD     1                   TO  SPA-GMN28-CRHKN-MAX
               MOVE    SPA-GMN28-CRHKN-MAX TO  IDX
           ELSE
               MOVE    SPA-GMN28-CRHSELNUM TO  IDX
               IF      SPA-NAI28-CHKNSKBN  NOT =   SPACE
                   MOVE    "2"             TO  SPA-NAI28-CHKNSKBN
               END-IF
           END-IF
      *
           INITIALIZE                  SPA-GMN28-CR-HKNLST-OCC (IDX)
           MOVE    IDX                 TO  SPA-GMN28-TCHNUM (IDX)
      *
           MOVE    SPA-GMN28-CHTEKSTYMD
                                       TO  SPA-GMN28-TCRHSTRYMD (IDX)
           MOVE    SPA-NAI28-CHTEKSTYMD
                                       TO  SPA-NAI28-TCRHSTRYMD (IDX)
           MOVE    SPA-GMN28-CHTEKEDYMD
                                       TO  SPA-GMN28-TCRHENDYMD (IDX)
           MOVE    SPA-NAI28-CHTEKEDYMD
                                       TO  SPA-NAI28-TCRHENDYMD (IDX)
      *
           MOVE    SPA-GMN28-CR-HKNJANUM
                                       TO  SPA-GMN28-TCRHKNJANUM (IDX)
           MOVE    SPA-GMN28-CR-HIHKNJANUM
                                       TO  SPA-GMN28-TCRHIHKNJANUM (IDX)
      *
           MOVE    SPA-NAI28-CHKNSKBN  TO  SPA-NAI28-TCHKNSKBN (IDX)
      *
           INITIALIZE                  SPA-GMN28-CRHKN-ING
           MOVE    ZERO                TO  SPA-GMN28-CRHSELNUM
           MOVE    ZERO                TO  SPA-MAE28-CRHSELNUM
           MOVE    10                  TO  SPA-GMN28-CUR
      *
           IF     (SPA-GMN28-CRHKN-MAX >   ZERO )
      *        システム日で有効な保険情報を記載する
               PERFORM 3101-CARAHKN-INIT-SEC
           END-IF
      *
           .
       4802-CRHKN-TOUROKU-EXT.
           EXIT.
      *
      *
      *
      *****************************************************************
      *    介護選択番号チェック処理
      *****************************************************************
       41012-CRSELNUM-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-CRSELNUM  NOT =   SPA-MAE28-CRSELNUM
               IF      SPA-GMN28-CRSELNUM  =   ZERO
                   MOVE    SPA-GMN28-CRSELNUM  TO  SPA-MAE28-CRSELNUM
                   INITIALIZE                  SPA-GMN28-CRNIN-ING
               END-IF
           END-IF
      *
           IF     (SPA-GMN28-CRSELNUM  NOT =   ZERO   )
             AND  (SPA-GMN28-CRSELNUM  NOT =   SPA-MAE28-CRSELNUM)
               IF      SPA-GMN28-CRSELNUM  >   SPA-GMN28-CRNIN-MAX
                   MOVE    "2825"          TO  SPA-ERRCD
                   MOVE    20              TO  SPA-GMN28-CUR
               ELSE
                   PERFORM 410121-CRNINTEI-HEN-SEC
               END-IF
           END-IF
           .
       41012-CRSELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者介護認定者編集処理
      *****************************************************************
       410121-CRNINTEI-HEN-SEC           SECTION.
      *
           MOVE    SPA-GMN28-CRSELNUM  TO  SPA-MAE28-CRSELNUM
           MOVE    SPA-GMN28-CRSELNUM  TO  IDX
      *
           MOVE    SPA-NAI28-TCAREJOTAI (IDX)
                                       TO  SPA-GMN28-CAREJOTAI
           MOVE    SPA-GMN28-TCRNINYMD (IDX)
                                       TO  SPA-GMN28-CRNINYMD
           MOVE    SPA-GMN28-TCRSTRYMD (IDX)
                                       TO  SPA-GMN28-CRTEKSTYMD
           MOVE    SPA-GMN28-TCRENDYMD (IDX)
                                       TO  SPA-GMN28-CRTEKEDYMD
           MOVE    SPA-NAI28-TCRNINYMD (IDX)
                                       TO  SPA-NAI28-CRNINYMD
           MOVE    SPA-NAI28-TCRSTRYMD (IDX)
                                       TO  SPA-NAI28-CRTEKSTYMD
           MOVE    SPA-NAI28-TCRENDYMD (IDX)
                                       TO  SPA-NAI28-CRTEKEDYMD
           MOVE    SPA-NAI28-TINSKBN (IDX)
                                       TO  SPA-NAI28-INSKBN
      *
           PERFORM 41012-CAREJOTAICD-CHK-SEC
           .
       410121-CRNINTEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    要介護状態区分チェック処理
      *****************************************************************
       41012-CAREJOTAICD-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-CAREJOTAI =   SPACE
               MOVE    SPACE           TO  SPA-GMN28-CAREJOTAICD-G
           ELSE
      *
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-GMN28-CAREJOTAI-MAX)
                              OR  (FLG-OK  =   1                )
                   IF      SPA-GMN28-CAREJOTAI
                                       =   SPA-GMN28-CAREJOTAI-LST
                                                      (IDX) (1:2)
                       MOVE    SPA-GMN28-CAREJOTAI-LST (IDX)
                                           TO  SPA-GMN28-CAREJOTAICD-G
                       MOVE    1           TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      FLG-OK              =   ZERO
                   MOVE    "2821"          TO  SPA-ERRCD
                   MOVE    21              TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       41012-CAREJOTAICD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    認定日付チェック処理
      *****************************************************************
       41012-CRNINYMD-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-CRNINYMD  =   SPACE
               MOVE    SPACE               TO  SPA-NAI28-CRNINYMD
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN28-CRNINYMD      TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN28-CRNINYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI28-CRNINYMD
               ELSE
                   MOVE    "2822"          TO  SPA-ERRCD
                   MOVE    22              TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       41012-CRNINYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護開始日付チェック処理
      *****************************************************************
       41012-CRTEKSTYMD-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-CRTEKSTYMD    =   SPACE
               MOVE    SPACE               TO  SPA-NAI28-CRTEKSTYMD
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN28-CRTEKSTYMD    TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN28-CRTEKSTYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI28-CRTEKSTYMD
               ELSE
                   MOVE    "2815"          TO  SPA-ERRCD
                   MOVE    23              TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       41012-CRTEKSTYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護終了日付チェック処理
      *****************************************************************
       41012-CRTEKEDYMD-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-CRTEKEDYMD    =   SPACE
               MOVE    SPACE               TO  SPA-NAI28-CRTEKEDYMD
           ELSE
      *
               IF      SPA-GMN28-CRTEKEDYMD    =   "9999999"
                   MOVE    ALL "9"         TO  SPA-NAI28-CRTEKEDYMD
                   GO      TO      41012-CRTEKEDYMD-CHK-EXT
               END-IF
      *
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN28-CRTEKEDYMD    TO  SGDAY-INDAY
               CALL    "ORCSGDAY"              USING
                                               ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN28-CRTEKEDYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI28-CRTEKEDYMD
               ELSE
                   MOVE    "2816"          TO  SPA-ERRCD
                   MOVE    24              TO  SPA-GMN28-CUR
                   GO      TO      41012-CRTEKEDYMD-CHK-EXT
               END-IF
           END-IF
      *
           IF      SPA-GMN28-CRTEKSTYMD    NOT =   SPACE
               IF      SPA-GMN28-CRTEKEDYMD    =   SPACE
                   IF     WRK-MCP-WIDGET       =   "P028_CRTEKEDYMD"
                       MOVE    ALL "9"         TO  SPA-NAI28-CRTEKEDYMD
                       MOVE    "9999999"       TO  SPA-GMN28-CRTEKEDYMD
                   END-IF
               END-IF
           END-IF
      *
           IF     (SPA-NAI28-CRTEKSTYMD    NOT =   SPACE )  AND
                  (SPA-NAI28-CRTEKEDYMD    NOT =   SPACE )
      *        開始日＞終了日はエラー
               IF      SPA-NAI28-CRTEKSTYMD    >   SPA-NAI28-CRTEKEDYMD
                   MOVE    "2817"              TO  SPA-ERRCD
                   MOVE    24                  TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       41012-CRTEKEDYMD-CHK-EXT.
           EXIT.
      *H28.12
      *
      ****************************************************************
      **    指導料等自動算定チェック処理
      ****************************************************************
       41012-AUTOSRYCD-CHK-SEC             SECTION.
      *
      *    指導料等自動算定
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL    ( IDX             >   3 )
               MOVE    SPACE               TO  SPA-GMN28-AUTONAME
                                                             (IDX)
           END-PERFORM
           MOVE    SPACE               TO  WRK-KEICD
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL    ( IDX             >   3 )
                         OR  ( SPA-ERRCD   NOT =   SPACE )
                         OR  ( FLG-END         =   1     )
               IF      SPA-GMN28-AUTOSRYCD (IDX)   NOT =   SPACE
                   MOVE    SPA-GMN28-AUTOSRYCD (IDX)
                                               TO  WRK-INPUTCD
                   PERFORM 410121-INPUTCD-CHK-SEC
                   IF      FLG-END             =   1 
                       CONTINUE
                   ELSE
                       IF      WRK-SRYCD           =   SPACE
                           IF      SPA-ERRCD       =   SPACE
                               MOVE    "2850"          TO  SPA-ERRCD
                           END-IF
                       ELSE
      *                    コードチェック
                           PERFORM 410122-TENSU-CHEK-SEC
                       END-IF
                   END-IF
      *
                   IF      SPA-ERRCD       NOT =   SPACE
                       EVALUATE    IDX
                           WHEN    1
                               MOVE    46          TO  SPA-GMN28-CUR
                           WHEN    2
                               MOVE    47          TO  SPA-GMN28-CUR
                           WHEN    3
                               MOVE    48          TO  SPA-GMN28-CUR
                       END-EVALUATE
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      (SPA-ERRCD          =   SPACE )
              AND  (FLG-END            =   ZERO  )
              AND  (WRK-KEICD      NOT =   SPACE )
              AND  (SPA-NAI28-KEIFLG   =   ZERO  )
                MOVE    WRK-KEICD          TO  SPA-ERRCD
                MOVE    1                  TO  SPA-NAI28-KEIFLG
                EVALUATE    WRK-KEI-IDX
                    WHEN    1
                        MOVE    46          TO  SPA-GMN28-CUR
                    WHEN    2
                        MOVE    47          TO  SPA-GMN28-CUR
                    WHEN    3
                       MOVE    48          TO  SPA-GMN28-CUR
               END-EVALUATE
           END-IF
           .
       41012-AUTOSRYCD-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      **    点数マスタチェック処理
      ****************************************************************
       410122-TENSU-CHEK-SEC             SECTION.
      *
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU-OK       =   ZERO
      *        マスタなし
               MOVE    "2850"          TO  SPA-ERRCD
               GO      TO      410122-TENSU-CHEK-EXT
           END-IF
      *
           MOVE    WRK-SRYCD-NAME      TO  SPA-GMN28-AUTONAME
                                                             (IDX)
           MOVE    WRK-SRYCD           TO  SPA-GMN28-AUTOSRYCD
                                                             (IDX)
      *    小児科外来診療料・小児かかりつけ対象外検索
           SET     TBL-IDX             TO  1
           SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
               AT   END
                   MOVE    ZERO            TO  FLG-ARI
               WHEN   TNS-SRYCD        =   TBL-SRYCD (TBL-IDX)
                   MOVE    1               TO  FLG-ARI
           END-SEARCH
      *
           IF     (TNS-SRYKBN          =   "13" )
            AND   (TNS-DATAKBN         =   "1"  )
            AND   (FLG-ARI             =   ZERO )
      *        入院はエラー
               IF      TNS-NYUGAITEKKBN    =   1
                   MOVE    "2851"          TO  SPA-ERRCD
               END-IF
      *        労災コードはエラー
               IF     (WRK-SRYCD (1:3)     =   "101")
                   MOVE    "2851"          TO  SPA-ERRCD
               END-IF
      *
               IF     (FLG-TENSU-OK        =   2 )
                 AND  (SPA-ERRCD           =   SPACE )
      *            当日期間外
                   MOVE    "K285"          TO  WRK-KEICD
                   MOVE    IDX             TO  WRK-KEI-IDX
               END-IF
           ELSE
               MOVE    "2851"          TO  SPA-ERRCD
           END-IF
      *
           .
       410122-TENSU-CHEK-EXT.
           EXIT.
      *
      ****************************************************************
      **    指導料等入力ＣＤチェック処理
      ****************************************************************
       410121-INPUTCD-CHK-SEC             SECTION.
      *
           MOVE    SPACE              TO  WRK-SRYCD
           MOVE    ZERO               TO  WRK-CD-MCNT
           MOVE    ZERO               TO  FLG-KANJI
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-INPUTCD         TO  KANACHK-MAE-INPUT
           MOVE    9                   TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-INPUTCD
           IF      KANACHK-RC      NOT =   ZERO
      *        全角半角混在エラー
               MOVE    "2851"              TO  SPA-ERRCD
           ELSE
               MOVE    KANACHK-MAX         TO  WRK-CD-MCNT
      *        全角
               IF      KANACHK-SYUBETU     =   2
                   MOVE    1               TO  FLG-KANJI
               ELSE
                   MOVE    ZERO            TO  FLG-KANJI
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *    半角で９文字の時、点数マスタチェックへ
               IF     (WRK-CD-MCNT         =   9    )  AND
                      (FLG-KANJI           =   ZERO )  AND
                      (WRK-INPUTCD(1:WRK-CD-MCNT)
                                               NUMERIC  )
                   MOVE    WRK-INPUTCD(1:WRK-CD-MCNT)
                                               TO  WRK-SRYCD
               ELSE
                   PERFORM 400211-INPUTCD-CHK-SEC
                   IF     (FLG-INPUT       =   ZERO ) AND
                          (SPA-ERRCD       =   SPACE )
                    AND   (FLG-CHK         =   ZERO  )
      *                入力コード検索
                       PERFORM 408-P02Y1-SYORI-SEC
                   END-IF
               END-IF
           END-IF
           .
       410121-INPUTCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力ＣＤ処理
      *****************************************************************
       400211-INPUTCD-CHK-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-INPUT
           MOVE    SPACE               TO  WRK-ICD-CDSYU
           IF      FLG-KANJI           =   1
      *        全角のコードチェック
               MOVE    "J"             TO  WRK-ICD-CDSYU
           ELSE
      *        診療コードチェック
               IF      WRK-INPUTCD(1:1)        =   "P"  OR  "S"
                   MOVE    "2851"              TO  SPA-ERRCD
               ELSE
                   IF     (WRK-CD-MCNT     >   1   AND  <=  9 )  AND
                          (WRK-INPUTCD(1:WRK-CD-MCNT)   NUMERIC    )
                       EVALUATE    WRK-CD-MCNT
                           WHEN    9
      *                    診療行為コード入力
                               MOVE    1           TO  FLG-INPUT
      *                    短縮３桁
                           WHEN    3
                               MOVE    "6"         TO  WRK-ICD-CDSYU
      *                    ４桁
                           WHEN    4
                               MOVE    "4"         TO  WRK-ICD-CDSYU
      *                    ５桁
                           WHEN    5
                               MOVE    "5"         TO  WRK-ICD-CDSYU
      *                    ６桁
                           WHEN    6
                               MOVE    "6"         TO  WRK-ICD-CDSYU
      *
                           WHEN    OTHER
                               MOVE    "A"         TO  WRK-ICD-CDSYU
                       END-EVALUATE
                   ELSE
                       MOVE    "A"         TO  WRK-ICD-CDSYU
                   END-IF
               END-IF
           END-IF
      *
           IF      WRK-ICD-CDSYU   NOT =   SPACE
               PERFORM 4002110-INPUTCD-HEN-SEC
           END-IF
           .
       400211-INPUTCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力コード処理
      *****************************************************************
       4002110-INPUTCD-HEN-SEC         SECTION.
      *
           INITIALIZE                  INPUTCD-REC
           MOVE    SPACE               TO  INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    WRK-ICD-CDSYU       TO  ICD-CDSYU
           MOVE    WRK-INPUTCD         TO  ICD-INPUTCD
           COMPUTE IDK                 =   WRK-CD-MCNT  +  1
           MOVE    "%"                 TO  ICD-INPUTCD(IDK:1)
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 930-INPUTCD-READ-SEC
           ELSE
               MOVE    1               TO  FLG-INPUTCD
           END-IF
           IF     (FLG-INPUTCD         =   ZERO  )  AND
                  (WRK-INPUTCD         =   ICD-INPUTCD )
      *        点数コード決定
               MOVE    ICD-SRYCD           TO  WRK-SRYCD
               MOVE    1                   TO  FLG-INPUT
           END-IF
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       4002110-INPUTCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    検索処理
      *****************************************************************
       408-P02Y1-SYORI-SEC             SECTION.
      *
      *    共通ＳＰＡ編集
           INITIALIZE                  SPA-P02Y1-AREA
           INITIALIZE                  SPA-P02Y1GMN-AREA 
           MOVE    "2"                 TO  SPA-P02Y1-TYPE
           MOVE    "13"                TO  SPA-P02Y1-DATA
           MOVE    IDX                 TO  SPA-GMN28-AUTOIDX
           MOVE    "P02"               TO  SPA-MOTOPG
      *
           IF      WRK-INPUTCD         NOT =   SPACE
      *        診療行為
               MOVE    "8"                 TO  SPA-P02Y1-GMN-SYORI
               MOVE    WRK-INPUTCD         TO  SPA-P02Y1-INPUTCD
               MOVE    "1"                 TO  SPA-P02Y1-TYPE
               IF      FLG-KANJI           =   1
                   MOVE    "J"             TO  SPA-P02Y1-CDSYUBETU
               ELSE
                   IF      WRK-INPUTCD(1:1)    NUMERIC
                       EVALUATE    WRK-CD-MCNT
                           WHEN    3
                           WHEN    6
                               MOVE    "6"     TO  SPA-P02Y1-CDSYUBETU
                           WHEN    4
                               MOVE    "4"     TO  SPA-P02Y1-CDSYUBETU
                           WHEN    5
                               MOVE    "5"     TO  SPA-P02Y1-CDSYUBETU
                       END-EVALUATE
                   ELSE
                       MOVE    "A"             TO  SPA-P02Y1-CDSYUBETU
                   END-IF
               END-IF
      *        画面編集
               PERFORM 41011-P02Y1-SET-SEC
           ELSE
      *        画面編集
               PERFORM 41011-P02Y1-SET-SEC
           END-IF
      *
           MOVE    SPACE               TO  SPA-ERRCD
      *
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    "P02Y1"             TO  SPA-SAKIPG
      *
           IF      SPA-P02Y1-GMN-MAX   =   ZERO
               MOVE    "INPUT"             TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM01"          TO  MCP-WIDGET
           END-IF
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE
           MOVE   "P02Y1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           MOVE    1                   TO  SPA-FLG-END
      *
           .
       408-P02Y1-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    検索画面編集処理
      *****************************************************************
       41011-P02Y1-SET-SEC             SECTION.
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP02Y1"         USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA.
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
           .
       41011-P02Y1-SET-EXT.
           EXIT.
      *
      ****************************************************************
      **    介護認定登録処理
      ****************************************************************
       4802-CRNIN-DELETE-SEC             SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN28-CRSELNUM  =   ZERO
                   MOVE    "2824"          TO  SPA-ERRCD
                   MOVE    20              TO  SPA-GMN28-CUR
               ELSE
      *            削除処理
                   PERFORM 48021-CRNIN-TBLDEL-SEC
               END-IF
           END-IF
           .
       4802-CRNIN-DELETE-EXT.
           EXIT.
      *
      ****************************************************************
      **    介護認定者削除処理
      ****************************************************************
       48021-CRNIN-TBLDEL-SEC             SECTION.
      *
           PERFORM VARYING    IDX      FROM   1   BY  1
                   UNTIL     (IDX      >   SPA-GMN28-CRNIN-MAX)
               IF     (SPA-GMN28-CRSELNUM  =   IDX )
      *H29.12
                   IF      IDX             =   50
                       INITIALIZE          SPA-GMN28-CR-NINLST-OCC
                                                              (IDX)
                   ELSE
                       MOVE    SPA-GMN28-CR-NINLST-OCC (IDX + 1)
                                       TO  SPA-GMN28-CR-NINLST-OCC(IDX)
                       INITIALIZE              SPA-GMN28-CR-NINLST-OCC
                                                              (IDX + 1)
                   END-IF
               ELSE
                   IF     (SPA-GMN28-TCRNUM (IDX)  =   ZERO )
                      AND (IDX                     <   50   )
                       MOVE    SPA-GMN28-CR-NINLST-OCC (IDX + 1)
                                       TO  SPA-GMN28-CR-NINLST-OCC(IDX)
                       INITIALIZE          SPA-GMN28-CR-NINLST-OCC
                                                              (IDX + 1)
                   END-IF
               END-IF
           END-PERFORM
      *
      *
           MOVE    ZERO                TO  SPA-GMN28-CRNIN-MAX
           PERFORM VARYING    IDX      FROM   1   BY  1
                   UNTIL     (IDX      >   50     )
               IF      SPA-NAI28-TCAREJOTAI(IDX) NOT =   SPACE
                   MOVE    IDX             TO  SPA-GMN28-TCRNUM (IDX)
                   ADD     1               TO  SPA-GMN28-CRNIN-MAX
               END-IF
           END-PERFORM
      *
           INITIALIZE                  SPA-GMN28-CRNIN-ING
           MOVE    ZERO                TO  SPA-GMN28-CRSELNUM
           MOVE    ZERO                TO  SPA-MAE28-CRSELNUM
           MOVE    20                  TO  SPA-GMN28-CUR
      *
           .
       48021-CRNIN-TBLDEL-EXT.
           EXIT.
      *
      ****************************************************************
      **    介護認定登録処理
      ****************************************************************
       4801-CRNIN-TOUROKU-SEC            SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4801-CRNIN-TOUROKU-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4802-CRNIN-TOUROKU-SEC
           END-IF
           .
       4801-CRNIN-TOUROKU-EXT.
           EXIT.
      *
      ****************************************************************
      **    介護認定者登録前関連チェック処理
      ****************************************************************
       4801-CRNIN-TOUROKU-CHK-SEC             SECTION.
      *
      *    必須チェック
           IF     (SPA-GMN28-CAREJOTAI =   SPACE )
             AND  (SPA-ERRCD           =   SPACE )
               MOVE    "2821"          TO  SPA-ERRCD
               MOVE    21              TO  SPA-GMN28-CUR
           END-IF
           IF     (SPA-GMN28-CRTEKSTYMD =   SPACE)
             AND  (SPA-ERRCD           =   SPACE )
               MOVE    "2818"          TO  SPA-ERRCD
               MOVE    23              TO  SPA-GMN28-CUR
           END-IF
           IF     (SPA-GMN28-CRTEKEDYMD =   SPACE)
             AND  (SPA-ERRCD            =   SPACE )
               MOVE    "2819"          TO  SPA-ERRCD
               MOVE    24              TO  SPA-GMN28-CUR
           END-IF
      *
      *    期間重複チェック
           PERFORM VARYING    IDY      FROM   1   BY  1
                   UNTIL     (IDY      >   SPA-GMN28-CRNIN-MAX)
                       OR    (SPA-ERRCD    NOT =   SPACE )
               IF     (SPA-GMN28-CRSELNUM      NOT =   IDY )
                   IF      (SPA-NAI28-TCRENDYMD (IDY)
                                               <   SPA-NAI28-CRTEKSTYMD)
                     OR    (SPA-NAI28-TCRSTRYMD (IDY)
                                               >   SPA-NAI28-CRTEKEDYMD)
                       CONTINUE
                   ELSE
                       MOVE    "2820"          TO  SPA-ERRCD
                       MOVE    23              TO  SPA-GMN28-CUR
                   END-IF
               END-IF
           END-PERFORM
      *
           IF     (SPA-GMN28-CRSELNUM  =   ZERO  )
             AND  (SPA-ERRCD           =   SPACE )
      *        新規追加
               IF      SPA-GMN28-CRNIN-MAX >=  50
                   MOVE    "2823"          TO  SPA-ERRCD
                   MOVE    20              TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       4801-CRNIN-TOUROKU-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      **    介護認定者登録処理
      ****************************************************************
       4802-CRNIN-TOUROKU-SEC             SECTION.
      *
           IF      SPA-GMN28-CRSELNUM  =   ZERO
               ADD     1                   TO  SPA-GMN28-CRNIN-MAX
               MOVE    SPA-GMN28-CRNIN-MAX TO  IDX
           ELSE
               MOVE    SPA-GMN28-CRSELNUM  TO  IDX
               IF      SPA-NAI28-INSKBN    NOT =   SPACE
                   MOVE    "2"             TO  SPA-NAI28-INSKBN
               END-IF
           END-IF
      *
           INITIALIZE                  SPA-GMN28-CR-NINLST-OCC (IDX)
           MOVE    IDX                 TO  SPA-GMN28-TCRNUM (IDX)
      *
           MOVE    SPA-GMN28-CRTEKSTYMD
                                       TO  SPA-GMN28-TCRSTRYMD (IDX)
           MOVE    SPA-NAI28-CRTEKSTYMD
                                       TO  SPA-NAI28-TCRSTRYMD (IDX)
           MOVE    SPA-GMN28-CRTEKEDYMD
                                       TO  SPA-GMN28-TCRENDYMD (IDX)
           MOVE    SPA-NAI28-CRTEKEDYMD
                                       TO  SPA-NAI28-TCRENDYMD (IDX)
           MOVE    SPA-GMN28-CRNINYMD  TO  SPA-GMN28-TCRNINYMD (IDX)
           MOVE    SPA-NAI28-CRNINYMD  TO  SPA-NAI28-TCRNINYMD (IDX)
      *
           MOVE    SPA-GMN28-CAREJOTAICD-G
                                       TO  SPA-GMN28-TCAREJOTAI (IDX)
           MOVE    SPA-GMN28-CAREJOTAI TO  SPA-NAI28-TCAREJOTAI (IDX)
      *
           MOVE    SPA-NAI28-INSKBN    TO  SPA-NAI28-TINSKBN (IDX)
      *
           INITIALIZE                  SPA-GMN28-CRNIN-ING
           MOVE    ZERO                TO  SPA-GMN28-CRSELNUM
           MOVE    ZERO                TO  SPA-MAE28-CRSELNUM
           MOVE    20                  TO  SPA-GMN28-CUR
      *
           .
       4802-CRNIN-TOUROKU-EXT.
           EXIT.
      *
      *H28.12
      ****************************************************************
      **    患者個人番号削除処理
      ****************************************************************
       4802-MYID-DELETE-SEC             SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN28-MYSELNUM  =   ZERO
                   MOVE    "2840"          TO  SPA-ERRCD
                   MOVE    40              TO  SPA-GMN28-CUR
               ELSE
      *            削除処理
                   PERFORM 48021-MYID-TBLDEL-SEC
               END-IF
           END-IF
           .
       4802-MYID-DELETE-EXT.
           EXIT.
      *
      ****************************************************************
      **    患者個人番号削除処理
      ****************************************************************
       48021-MYID-TBLDEL-SEC             SECTION.
      *
           PERFORM VARYING    IDX      FROM   1   BY  1
                   UNTIL     (IDX      >   SPA-GMN28-MYID-MAX)
               IF     (SPA-GMN28-MYSELNUM  =   IDX )
      *H29.12
                   IF      IDX             =   20
                       INITIALIZE              SPA-GMN28-MYLIST-OCC
                                                            (IDX)
                   ELSE
                       MOVE    SPA-GMN28-MYLIST-OCC (IDX + 1)
                                       TO  SPA-GMN28-MYLIST-OCC(IDX)
                       INITIALIZE              SPA-GMN28-MYLIST-OCC
                                                            (IDX + 1)
                   END-IF
               ELSE
                   IF     (SPA-GMN28-TMYNUM (IDX)  =   ZERO )
                      AND (IDX                     <   20   )
                       MOVE    SPA-GMN28-MYLIST-OCC (IDX + 1)
                                       TO  SPA-GMN28-MYLIST-OCC(IDX)
                       INITIALIZE          SPA-GMN28-MYLIST-OCC
                                                              (IDX + 1)
                   END-IF
               END-IF
           END-PERFORM
      *
      *
           MOVE    ZERO                TO  SPA-GMN28-MYID-MAX
           PERFORM VARYING    IDX      FROM   1   BY  1
                   UNTIL     (IDX      >   20     )
               IF      SPA-GMN28-TMYIDKEY(IDX) NOT =   SPACE
                   MOVE    IDX             TO  SPA-GMN28-TMYNUM (IDX)
                   ADD     1               TO  SPA-GMN28-MYID-MAX
               END-IF
           END-PERFORM
      *
           INITIALIZE                  SPA-GMN28-MYID-ING
           MOVE    ZERO                TO  SPA-GMN28-MYSELNUM
           MOVE    ZERO                TO  SPA-MAE28-MYSELNUM
           MOVE    40                  TO  SPA-GMN28-CUR
      *
           .
       48021-MYID-TBLDEL-EXT.
           EXIT.
      *
      ****************************************************************
      **    患者個人番号登録処理
      ****************************************************************
       4803-MYID-TOUROKU-SEC            SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 48031-MYID-TOUROKU-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 48032-MIYD-TOUROKU-SEC
           END-IF
           .
       4803-MYID-TOUROKU-EXT.
           EXIT.
      *
      ****************************************************************
      **    患者個人番号登録前関連チェック処理
      ****************************************************************
       48031-MYID-TOUROKU-CHK-SEC             SECTION.
      *
      *    必須チェック
           IF     (SPA-GMN28-MYIDKEY   =   SPACE )
             AND  (SPA-ERRCD           =   SPACE )
               MOVE    "2844"          TO  SPA-ERRCD
               MOVE    41              TO  SPA-GMN28-CUR
           END-IF
           IF     (SPA-GMN28-MYNUMBER  =   SPACE)
             AND  (SPA-ERRCD           =   SPACE )
               MOVE    "2845"          TO  SPA-ERRCD
               MOVE    42              TO  SPA-GMN28-CUR
           END-IF
      *
      *    ＩＤ識別重複チェック
           PERFORM VARYING    IDY      FROM   1   BY  1
                   UNTIL     (IDY      >   SPA-GMN28-MYID-MAX)
                       OR    (SPA-ERRCD    NOT =   SPACE )
               IF     (SPA-GMN28-MYSELNUM      NOT =   IDY )
                   IF      SPA-GMN28-MYIDKEY   =   SPA-GMN28-TMYIDKEY
                                                           (IDY)
                       MOVE    "2846"          TO  SPA-ERRCD
                       MOVE    41              TO  SPA-GMN28-CUR
                   END-IF
               END-IF
           END-PERFORM
      *
           IF     (SPA-GMN28-MYSELNUM  =   ZERO  )
             AND  (SPA-ERRCD           =   SPACE )
      *        新規追加
               IF      SPA-GMN28-MYID-MAX  >=  20
                   MOVE    "2847"          TO  SPA-ERRCD
                   MOVE    40              TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       48031-MYID-TOUROKU-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      **    患者個人番号登録処理
      ****************************************************************
       48032-MIYD-TOUROKU-SEC             SECTION.
      *
           IF      SPA-GMN28-MYSELNUM  =   ZERO
               ADD     1                   TO  SPA-GMN28-MYID-MAX
               MOVE    SPA-GMN28-MYID-MAX  TO  IDX
           ELSE
               MOVE    SPA-GMN28-MYSELNUM  TO  IDX
           END-IF
      *
           INITIALIZE                  SPA-GMN28-MYLIST-OCC (IDX)
           MOVE    IDX                 TO  SPA-GMN28-TMYNUM (IDX)
      *
           MOVE    SPA-GMN28-MYIDKEY
                                       TO  SPA-GMN28-TMYIDKEY (IDX)
           MOVE    SPA-GMN28-MYNUMBER
                                       TO  SPA-GMN28-TMYNUMBER (IDX)
           MOVE    SPA-GMN28-MYDESCRIP
                                       TO  SPA-GMN28-TMYDESCRIP(IDX)
      *
           INITIALIZE                  SPA-GMN28-MYID-ING
           MOVE    ZERO                TO  SPA-GMN28-MYSELNUM
           MOVE    ZERO                TO  SPA-MAE28-MYSELNUM
           MOVE    40                  TO  SPA-GMN28-CUR
      *
           .
       48032-MIYD-TOUROKU-EXT.
           EXIT.
      *
      ****************************************************************
      **    登録前関連チェック処理
      ****************************************************************
       4109-TOUROKU-CHK-SEC             SECTION.
      *
      *
           .
       4109-TOUROKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角チェック処理
      *****************************************************************
       891-HALF-CHK-SEC         SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-MAX             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           IF     (KANACHK-RC          =   ZERO  )  AND
                  (KANACHK-SYUBETU     =   1     )
      *        半角
               MOVE    ZERO                TO  FLG-ERR
           ELSE
               MOVE    1                   TO  FLG-ERR
           END-IF
      *
           .
       891-HALF-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    全角チェック処理
      *****************************************************************
       891-HALF-CHK02-SEC         SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    SPACE               TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-MAX             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           IF     (KANACHK-RC2         =   ZERO  )
      *        半角
               MOVE    ZERO                TO  FLG-ERR
           ELSE
               MOVE    1                   TO  FLG-ERR
           END-IF
      *
           .
       891-HALF-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号マスタ次検索処理
      *****************************************************************
       900-HKNJAINFCARE-READ-SEC              SECTION.
      *
           MOVE    "tbl_hknjainf_care"    TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNJAINFCARE-REC
               MOVE    ZERO                TO  FLG-HKNJAINFCARE
           ELSE
               INITIALIZE                  HKNJAINFCARE-REC
               MOVE    1                   TO  FLG-HKNJAINFCARE
           END-IF
           .
       900-HKNJAINFCARE-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    SPA-SYSYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SYSYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
      *H3.10   開始日の降順に検索
      ***  MOVE    "key4"               TO  MCP-PATHNAME
           MOVE    "key24"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM 9110-TENSU-READ-SEC
           END-IF
           MOVE    ZERO                TO  FLG-TENSU-OK
           MOVE    SPACE               TO  WRK-SRYCD-NAME
           PERFORM UNTIL  (FLG-TENSU       =   1 ) OR
                          (FLG-TENSU-OK    =   1 )
               IF     (TNS-YUKOSTYMD       <=  SPA-SYSYMD ) AND
                      (TNS-YUKOEDYMD       >=  SPA-SYSYMD )
                   MOVE    1                   TO  FLG-TENSU-OK
                   MOVE    1                   TO  FLG-TENSU
                   MOVE    TNS-NAME            TO  WRK-SRYCD-NAME
               ELSE
      *H30.10
                   IF      TNSWK-TENSU-REC     =   SPACE
      *                最終履歴を編集 
                       MOVE    2                   TO  FLG-TENSU-OK
                       MOVE    TNS-TENSU-REC       TO  TNSWK-TENSU-REC
                   END-IF
               END-IF
               IF      FLG-TENSU           =   ZERO
                   MOVE    "tbl_tensu"         TO  MCP-TABLE
                   MOVE    "key24"             TO  MCP-PATHNAME
                   PERFORM 9110-TENSU-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    当日期間外
           IF      FLG-TENSU-OK        =   2
               MOVE    TNSWK-TENSU-REC     TO  TNS-TENSU-REC
               MOVE    SPACE               TO  WRK-SRYCD-NAME
               STRING  "【期間外】"
                      TNS-NAME
                                       DELIMITED  BY  SPACE
                                   INTO    WRK-SRYCD-NAME
               END-STRING
           END-IF
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ読込処理
      *****************************************************************
       9110-TENSU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               MOVE    ZERO                TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
           .
       9110-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       930-INPUTCD-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTCD-REC
               MOVE    ZERO                TO  FLG-INPUTCD
           ELSE
               INITIALIZE                  INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           .
       930-INPUTCD-READ-EXT.
           EXIT.
      *
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
       900-PUT-WINDOW           SECTION.
      *
           MOVE    "PUTWINDOW"      TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
