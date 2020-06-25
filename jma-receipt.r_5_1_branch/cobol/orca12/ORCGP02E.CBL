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
       PROGRAM-ID.             ORCGP02E.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 患者複写入力画面（Ｐ０２Ｅ）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/07/08    MCC-多々納    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  02.00.01    NACL-多々納  02/07/31  パラメタファイルをＤＢへ
      *  02.00.02    NACL-多々納  02/11/01  保険・公費は有効のものを対象
      *  02.00.03    NACL-多々納  05/11/11  MONFUNC 対応 他
      *  03.05.00    NACL-多々納  07/05/XX  グループ診療対応
      *  03.05.01    NACL-多々納  07/08/28  公費期限切れ区分追加
      *  04.04.00    NACL-多々納  09/01/28  本院分院対応
      *  04.05.00    NACL-多々納  09/06/05  公費複写なし対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    画面非表示
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *    患者画面スパ領域
           COPY    "P02SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-PTNUMO          PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-KYUSEIRRK       PIC 9(01).
           03  FLG-PTGETNUM        PIC 9(01).
           03  FLG-TSYRRK          PIC 9(01).
           03  FLG-TNKRRK          PIC 9(01).
           03  FLG-PTSAME          PIC 9(01).
      *
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNNUM-OK       PIC 9(01).
           03  FLG-KOHCOPY         PIC 9(01).
      *
      *
           03  FLG-OK              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-PIDMSG              PIC X(80).
           03  WRK-KBNCD               PIC X(02).
      *
           03  WRK-RATE-AREA.
               05  WRK-RATE-KBN        PIC X(02).
               05  WRK-RATE-SP         PIC X(01).
               05  WRK-RATE-EDIT.
                   07  WRK-RATE-EDITZ  PIC ZZZZZZZ.
                   07  WRK-RATE-TANI   PIC X(04).
               05  WRK-RATE-EDIT-R  REDEFINES    WRK-RATE-EDIT.
                   07  WRK-RATE-EDITR  PIC X(11).
      *    保険公費種別
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-PAYKBN              PIC X(02).
           03  WRK-HKNKOHSBTKBN        PIC X(01).
           03  WRK-TANSEIDONAME        PIC X(20).
           03  WRK-TEKSTYMD            PIC X(08).
           03  WRK-TEKEDYMD            PIC X(08).
      *
           03  WRK-MCP-WIDGET          PIC X(64).
      *
      *   年齢表示
       01  WRK-NENREI-AREA.
           03  WRK-YMDS1.
               05  WRK-YYS1        PIC 9(04).
               05  WRK-MMS1        PIC 9(02).
               05  WRK-DDS1        PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2        PIC 9(04).
               05  WRK-MMS2        PIC 9(02).
               05  WRK-DDS2        PIC 9(02).
           03  WRK-NENREI.
               05  WRK-NENREI-YY       PIC 9(04).
               05  WRK-NENREI-MM       PIC 9(02).
               05  WRK-NENREI-DD       PIC 9(02).
       01  WRK-NENREI-G.
           03  WRK-NENREI-Z        PIC ZZ9.
           03  WRK-NENREI-MEI      PIC X(04).
       01  WRK-NENREI-GR           REDEFINES   WRK-NENREI-G.
           03  WRK-NENREI-ZN       PIC Z9.
           03  WRK-NENREI-MEIN     PIC X(08).
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT       PIC X(100).
           03  WRK-ATO-INPUT       PIC X(100).
           03  WRK-MAX             PIC 9(04).
      *
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号変換マスタ
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *    受付
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *    同一患者識別テーブル
       01  PTSAME-REC.
           COPY    "CPPTSAME.INC".
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
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *   患者番号発生サブ
           COPY    "CPORCSP01.INC".
      *   画面編集サブ
           COPY    "CPORCSP02.INC".
      *
      *    患者複写パラメタ
           COPY    "CPORCSP04.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
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
           COPY    "ORCA12SCRAREA.INC".
      *    COPY    "P02.INC".
      *    COPY    "P02A.INC".
      *    COPY    "P02D.INC".
      *    COPY    "P02E.INC".
      *    COPY    "P02F.INC".
      *    COPY    "P014.INC".
      *    COPY    "P015.INC".
      *    COPY    "P98.INC".
      *    COPY    "P981.INC".
      *    COPY    "P0201.INC".
      *    COPY    "P982.INC".
      *    COPY    "P100.INC".
      *    COPY    "P02X.INC".
      *    COPY    "P02S.INC".
      *    COPY    "P02Q.INC".
      *    COPY    "P02Q1.INC".
      *    COPY    "P02Y.INC".
      *    COPY    "P02Y1.INC".
      *    COPY    "P02G.INC".
      *    COPY    "P02G2.INC".
      *    COPY    "P02K.INC".
      *    COPY    "P02H.INC".
      *    COPY    "P02T.INC".
      *    COPY    "PERR.INC".
      *    COPY    "PID1.INC".
      *    COPY    "PDMY01.INC".
      *    COPY    "CPAPIMOD.INC".
      *
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
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
      *    画面編集後、表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-P02KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA  TO  SPA-FREE
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "PERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   ZERO
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      *
               MOVE    "NEW"               TO  MCP-PUTTYPE
               MOVE    "P02E"              TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
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
           MOVE    ZERO                    TO  FLG-END
      *
           EVALUATE    SPA-MOTOPG
      *        患者選択画面より
               WHEN    "P02F"
                   IF      SPA-GMN2E-PTNUM         =   SPACE
                       MOVE    1               TO  SPA-GMN2E-CUR
                   ELSE
      *                画面編集
                       PERFORM 41011-PTNUM-SYORI-SEC
                   END-IF
               WHEN    OTHER
                   PERFORM 3000-SCREEN-INIT-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       3000-SCREEN-INIT-SEC            SECTION.
      *
      *    ＳＰＡ編集
           INITIALIZE                      SPA-GMN2E-AREA
      *
           PERFORM 300-SPA-INIT-SEC
      *
           MOVE    1                  TO  SPA-GMN2E-CUR
           .
       3000-SCREEN-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       300-SPA-INIT-SEC                SECTION.
      *
           IF      SPA-GMN2E-SYORI     =   1
      *        グループ医療機関情報
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   9   )   OR
                                  (SPA-GRPHOSPNUM (IDX)    =   ZERO )
                   MOVE    SPA-GRPHOSPNUM (IDX)
                                      TO  SPA-GMN2E-GRPHOSPNUM-LST
                                                     (IDX)(1:2)
                   MOVE    SPA-GRPHOSPNAME (IDX)
                                      TO  SPA-GMN2E-GRPHOSPNUM-LST
                                                     (IDX)(4:)
                   MOVE    IDX            TO  SPA-GMN2E-GRPMAX
               END-PERFORM
               MOVE    SPA-GMN2E-GRPHOSPNUM-LST(1)
                                           TO  SPA-GMN2E-GRPHOSPNUM-G
           END-IF
           .
       3000-SCREEN-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  P02E
           INITIALIZE                      P02E
      *
           MOVE    SPA-GMN2E-PTNUM     TO  P02E-PTNUM
      *
           MOVE    SPA-GMN2E-NAME      TO  P02E-NAME
           MOVE    SPA-GMN2E-ADRS1     TO  P02E-ADRS1
           MOVE    SPA-GMN2E-ADRS2     TO  P02E-ADRS2
           MOVE    SPA-GMN2E-BIRTHDAY  TO  P02E-BIRTHDAY
           MOVE    SPA-GMN2E-NENREI    TO  P02E-NENREI
           MOVE    SPA-GMN2E-SEX       TO  P02E-SEX
      *
           MOVE    SPA-GMN2E-GRPHOSPNUM-G  TO  P02E-GRPHOSPNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN2E-GRPMAX )
               MOVE    SPA-GMN2E-GRPHOSPNUM-LST(IDX)
                                       TO  P02E-GRPHOSPNUM-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN2E-GRPMAX    TO  P02E-GRPHOSPNUM-COUNT
      *
      *    患者紐付け
           IF      SPA-GRPHOSPKBN      =   ZERO
               MOVE    WIDGET-INSENSITIVE  TO  P02E-B03-STATE
               MOVE    SPACE               TO  P02E-B03-LABEL
           ELSE
               MOVE    WIDGET-NORMAL       TO  P02E-B03-STATE
               IF      SPA-GMN2E-SYORI     =   ZERO
                   MOVE    "F3 患者紐付複写"   TO  P02E-B03-LABEL
               ELSE
                   MOVE    "F3 患者複写  "     TO  P02E-B03-LABEL
               END-IF
           END-IF
           MOVE    SPA-GMN2E-MIDASI    TO  P02E-MIDASI-VALUE
           IF      SPA-GMN2E-SYORI     =   ZERO
               MOVE    "black"             TO  P02E-MIDASI-STYLE
           ELSE
               MOVE    "red"               TO  P02E-MIDASI-STYLE
           END-IF
      *    保険情報
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   5   )   OR
                              (SPA-GMN2E-THKNMEI (IDX) =   SPACE )
               MOVE    SPA-GMN2E-THKNMEI (IDX) TO  P02E-THKNMEI(IDX)
               MOVE    SPA-GMN2E-THKNJANUM (IDX)
                                               TO  P02E-THKNJANUM (IDX)
               MOVE    SPA-GMN2E-TKIGO (IDX)   TO  P02E-TKIGO  (IDX)
               MOVE    SPA-GMN2E-TNUM  (IDX)   TO  P02E-TNUM   (IDX)
               MOVE    IDX                     TO  P02E-HKNLIST-COUNT
           END-PERFORM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   10  )   OR
                              (SPA-GMN2E-TKOHMEI (IDX) =   SPACE )
               MOVE    SPA-GMN2E-TKOHMEI (IDX)     TO  P02E-TKOHMEI(IDX)
               MOVE    SPA-GMN2E-TFTNJANUM (IDX)   TO  P02E-TFTNJANUM
                                                                   (IDX)
               MOVE    SPA-GMN2E-TJKYSNUM (IDX)    TO  P02E-TJKYSNUM
                                                                   (IDX)
               MOVE    IDX                     TO  P02E-KOHLIST-COUNT
           END-PERFORM
           IF      SPA-GMN2E-SYORI     =   ZERO
               MOVE    WIDGET-NORMAL       TO  P02E-B10-STATE
               MOVE    "F10 公費複写なし"  TO  P02E-B10-LABEL
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  P02E-B10-STATE
               MOVE    SPACE               TO  P02E-B10-LABEL
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
           EVALUATE    SPA-GMN2E-CUR
               WHEN    01
                   MOVE    "PTNUM"            TO  MCP-WIDGET
               WHEN    03
                   MOVE    "GRPHOSPNUM"       TO  MCP-WIDGET
               WHEN    02
                   MOVE    "B12"              TO  MCP-WIDGET
           END-EVALUATE
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
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 420-MODORU-SEC
      *    患者紐付け
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 430-GRPSYORI-SEC
      *    患者一覧
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 430-ICHIRAN-SYORI-SEC
      *    複写
               WHEN    "CLICKED"       ALSO    "B12"
                   MOVE    ZERO            TO  FLG-KOHCOPY
                   PERFORM 490-TOROKU-SEC
      *    複写（公費複写なし）
               WHEN    "CLICKED"       ALSO    "B10"
                   MOVE    1               TO  FLG-KOHCOPY
                   PERFORM 490-TOROKU-SEC
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
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN2E-CUR
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
       200-ENTRY-EXT.
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
           .
      *
       410-INPUT-CHK-EXT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    P02E-PTNUM          TO  SPA-GMN2E-PTNUM
           MOVE    P02E-GRPHOSPNUM     TO  SPA-GMN2E-GRPHOSPNUM-G
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェックと別画面処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    医療機関番号
           PERFORM 41021-GRPHOSPNUM-CHK-SEC
      *    患者番号
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN2E-PTNUM     NOT =   SPACE
                   PERFORM 41022-PTNUM-CHK-SEC
               END-IF
           END-IF
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
      *        画面編集
               IF     (SPA-GMN2E-PTNUM     NOT =   SPACE )
                   PERFORM 41011-PTNUM-SYORI-SEC
               END-IF
           END-IF
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    医療機関番号処理
      *****************************************************************
       41021-GRPHOSPNUM-CHK-SEC                 SECTION.
      *
           IF      SPA-GMN2E-GRPMAX    =   ZERO
               MOVE    SPACE               TO  SPA-GMN2E-GRPHOSPNUM-G
           END-IF
           IF      SPA-GMN2E-GRPHOSPNUM    NOT =   SPACE
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN2E-GRPMAX
                   IF      SPA-GMN2E-GRPHOSPNUM-LST (IDX)(1:2)
                                           =   SPA-GMN2E-GRPHOSPNUM
                       MOVE    SPA-GMN2E-GRPHOSPNUM-LST (IDX)
                                           TO  SPA-GMN2E-GRPHOSPNUM-G
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      FLG-OK              =   ZERO
                   MOVE    "0003"              TO  SPA-ERRCD
                   MOVE    3                   TO  SPA-GMN2E-CUR
               END-IF
           END-IF
           .
       41021-GRPHOSPNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       41022-PTNUM-CHK-SEC         SECTION.
      *
           INITIALIZE                      ORCSPTNUMAREA
           MOVE    SPA-GMN2E-PTNUM     TO  SPTNUM-PTNUM
           MOVE    SPA-GMN2E-GRPHOSPNUM
                                      TO  SPTNUM-GRPHOSPNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           EVALUATE    SPTNUM-RC
               WHEN    00
      *            患者あり
                   MOVE    SPTNUM-PTID         TO  SPA-GMN2E-PTID
                   MOVE    SPTNUM-PTNUM        TO  SPA-GMN2E-PTNUM
      *            画面編集
      ********     PERFORM 41011-PTNUM-SYORI-SEC
               WHEN    20
      *            患者一覧へ
                   INITIALIZE                      SPA-GMN2F-AREA
                   MOVE    SPA-GMN2E-PTNUM     TO  SPA-GMN2F-KNJKEY
                   PERFORM 41012-P02F-SYORI-SEC
               WHEN    OTHER
      *            エラー
                   INITIALIZE                  SPA-GMN2E-OUTAREA
                   MOVE    "0001"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN2E-CUR
           END-EVALUATE
           .
       41022-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報編集　処理
      *****************************************************************
       41011-PTNUM-SYORI-SEC           SECTION.
      *
      *    患者マスター存在チェック
           IF      SPA-GMN2E-GRPHOSPNUM    =   SPACE
               MOVE    SPA-HOSPNUM             TO  PTINF-HOSPNUM
           ELSE
               MOVE    SPA-GMN2E-GRPHOSPNUM    TO  PTINF-HOSPNUM
           END-IF
           MOVE    SPA-GMN2E-PTID      TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF      NOT =   ZERO
               INITIALIZE                  SPA-GMN2E-OUTAREA
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN2E-CUR
               GO      TO      41011-PTNUM-SYORI-EXT
           END-IF
      *
           MOVE    PTINF-NAME          TO  SPA-GMN2E-NAME
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"                TO  SPA-GMN2E-SEX
               WHEN    "2"
                   MOVE    "女"                TO  SPA-GMN2E-SEX
           END-EVALUATE
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-GMN2E-BIRTHDAY
      *
           MOVE    PTINF-HOME-ADRS     TO  SPA-GMN2E-ADRS1
           MOVE    PTINF-HOME-BANTI    TO  SPA-GMN2E-ADRS2
      *    年齢
           PERFORM 41012-NENREI-SET-SEC
      *    保険情報
           PERFORM 41013-KHNKOH-SET-SEC
      *
      *    患者紐付け判定
           IF      SPA-GMN2E-GRPHOSPNUM    NOT =   SPACE
               PERFORM 410111-PTSAME-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    2                   TO  SPA-GMN2E-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN2E-CUR
           END-IF
           .
       41011-PTNUM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者紐付け判定処理
      *****************************************************************
       410111-PTSAME-CHK-SEC           SECTION.
      *
           INITIALIZE                      PTSAME-REC
           MOVE    PTINF-HOSPNUM       TO  PTSAME-HOSPNUM
           MOVE    PTINF-PTID          TO  PTSAME-PTID
           MOVE    PTSAME-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptsame"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-PTSAME-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTSAME
           END-IF
           MOVE    "tbl_ptsame"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTSAME          =   ZERO
      *        同一医療機関で登録済み
               MOVE    PTSAME-REC          TO  MCPDATA-REC
               MOVE    "tbl_ptsame"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptsame"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-PTSAME-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-PTSAME
               END-IF
               PERFORM UNTIL   (FLG-PTSAME     =   1 )
                          OR   (SPA-ERRCD  NOT =   SPACE )
                   IF     (PTSAME-HOSPNUM      =   SPA-HOSPNUM   )
                       MOVE    "0004"              TO  SPA-ERRCD
                   END-IF
      *
                   MOVE    "tbl_ptsame"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-PTSAME-READ-SEC
               END-PERFORM
               MOVE    "tbl_ptsame"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
           .
       410111-PTSAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険情報編集処理
      *****************************************************************
       41013-KHNKOH-SET-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN2E-HKNLIST-G
           INITIALIZE                  SPA-GMN2E-KOHLIST-G
      *    保険者編集
           INITIALIZE                  PTHKNINF-REC
           MOVE    PTINF-HOSPNUM       TO  PTHKN-HOSPNUM
           MOVE    SPA-GMN2E-PTID      TO  PTHKN-PTID
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM     UNTIL   FLG-PTHKNINF    =   1
               IF     (PTHKN-SAKJOKBN      =   SPACE )  AND
                      (PTHKN-HKNNUM    NOT =   "971" AND
                                               "973" )  AND
                      (PTHKN-TEKSTYMD      <=  SPA-SYSYMD AND
                       PTHKN-TEKEDYMD      >=  SPA-SYSYMD )
                   PERFORM 410130-HKNNUM-CHK-SEC
      *            患者紐付けは、期間エラーも複写する
                   IF     (FLG-HKNNUM-OK       =   ZERO )
                      AND (SPA-GMN2E-SYORI     =   1    )
                       MOVE    PTHKN-HKNNUM        TO  WRK-TANSEIDONAME
                       MOVE    1                   TO  FLG-HKNNUM-OK
                   END-IF
                   IF      FLG-HKNNUM-OK       =   1
                       ADD     1               TO  IDX
                       IF      IDX             <=  5
                           PERFORM 410131-PTHKNINF-HEN-SEC
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    公費編集
           INITIALIZE                  PTKOHINF-REC
           MOVE    PTINF-HOSPNUM       TO  PTKOH-HOSPNUM
           MOVE    SPA-GMN2E-PTID      TO  PTKOH-PTID
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-PTKOHINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM     UNTIL     FLG-PTKOHINF  =   1
               IF     (PTKOH-SAKJOKBN        =   SPACE    )  AND
                      (PTKOH-TEKSTYMD      <=  SPA-SYSYMD AND
                       PTKOH-TEKEDYMD      >=  SPA-SYSYMD )
      *            第三者行為対象外
                  AND (PTKOH-KOHNUM    NOT =   "970"      )
      *
                   PERFORM 410130-KOHNUM-CHK-SEC
      *            患者紐付けは、期間エラーも複写する
                   IF     (FLG-HKNNUM-OK       =   ZERO )
                      AND (SPA-GMN2E-SYORI     =   1    )
                       MOVE    PTKOH-KOHNUM        TO  WRK-TANSEIDONAME
                       MOVE    1                   TO  FLG-HKNNUM-OK
                   END-IF
                   IF      FLG-HKNNUM-OK       =   1
                       ADD     1               TO  IDX
                       IF      IDX             <=  10
                           PERFORM 410132-PTKOHINF-HEN-SEC
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-PTKOHINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       41013-KHNKOH-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険情報保険番号チェック処理
      *****************************************************************
       410130-HKNNUM-CHK-SEC           SECTION.
      *
      *    保険公費区分編集
           MOVE    PTHKN-HKNNUM        TO  WRK-HKNNUM
           MOVE    ZERO                TO  WRK-PAYKBN
           MOVE    PTHKN-TEKSTYMD      TO  WRK-TEKSTYMD
           MOVE    PTHKN-TEKEDYMD      TO  WRK-TEKEDYMD
           PERFORM 1002211-HKNNUM-SET-SEC
      *
           .
       410130-HKNNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費情報保険番号チェック処理
      *****************************************************************
       410130-KOHNUM-CHK-SEC           SECTION.
      *
      *    保険公費区分編集
           MOVE    PTKOH-KOHNUM        TO  WRK-HKNNUM
           MOVE    PTKOH-TEKSTYMD      TO  WRK-TEKSTYMD
           MOVE    PTKOH-TEKEDYMD      TO  WRK-TEKEDYMD
           MOVE    PTKOH-PAYKBN        TO  WRK-PAYKBN
           IF     (WRK-HKNNUM          =   "027"  )  AND
                  (WRK-PAYKBN          =   ZERO  OR  SPACE )
               MOVE    SPA-NAI-HOSPSBT     TO  WRK-PAYKBN(1:1)
               MOVE    SPA-NAI-ROUPAYKBN   TO  WRK-PAYKBN(2:1)
           END-IF
           PERFORM 1002211-HKNNUM-SET-SEC
           IF      FLG-HKNNUM-OK       =   1
               IF     (PTKOH-KOHNUM        =   "027" )  AND
                      (PTKOH-PAYKBN        =   ZERO  )
                   MOVE    "老人保健"          TO  WRK-TANSEIDONAME
               END-IF
           END-IF
      *
           .
       410130-KOHNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報編集処理
      *****************************************************************
       410131-PTHKNINF-HEN-SEC           SECTION.
      *
           MOVE    PTHKN-HKNJANUM      TO  SPA-GMN2E-THKNJANUM(IDX)
           MOVE    PTHKN-KIGO          TO  SPA-GMN2E-TKIGO    (IDX)
           MOVE    PTHKN-NUM           TO  SPA-GMN2E-TNUM    (IDX)
      *    保険公費区分編集
           MOVE    WRK-TANSEIDONAME    TO  SPA-GMN2E-THKNMEI (IDX)
      *
           .
       410131-PTHKNINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報編集処理
      *****************************************************************
       410132-PTKOHINF-HEN-SEC           SECTION.
      *
           MOVE    PTKOH-FTNJANUM      TO  SPA-GMN2E-TFTNJANUM (IDX)
           MOVE    PTKOH-JKYSNUM       TO  SPA-GMN2E-TJKYSNUM  (IDX)
           MOVE    WRK-TANSEIDONAME    TO  SPA-GMN2E-TKOHMEI   (IDX)
      *
           .
       410132-PTKOHINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者一覧処理
      *****************************************************************
       430-ICHIRAN-SYORI-SEC           SECTION.
      *
           INITIALIZE                      SPA-GMN2F-AREA
           MOVE    SPACE               TO  SPA-GMN2F-KNJKEY
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP02F"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           MOVE    "KNJKEY"            TO  MCP-WIDGET
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
      *
           MOVE    "P02F"              TO  SPA-SAKIPG
           MOVE    "P02E"              TO  SPA-MOTOPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P02F"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       430-ICHIRAN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者一覧処理
      *****************************************************************
       41012-P02F-SYORI-SEC            SECTION.
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP02F"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
      *    １件の時、対象
           IF      SPA-GMN2F-MAX       =   1
      *        画面編集
               PERFORM 41011-PTNUM-SYORI-SEC
               GO  TO  41012-P02F-SYORI-EXT
           END-IF
      *
      *    患者一覧画面へ
           IF      SPA-GMN2F-MAX       =   ZERO
               MOVE    "KNJKEY"            TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM"            TO  MCP-WIDGET
           END-IF
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
      *
      *    一覧へ
           MOVE    "P02F"              TO  SPA-SAKIPG
           MOVE    "P02E"              TO  SPA-MOTOPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P02F"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
          .
       41012-P02F-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    取り消し処理
      *****************************************************************
       420-MODORU-SEC             SECTION.
      *
           INITIALIZE                      SPA-GMN2E-AREA
      *
           PERFORM 210-BACK
      *
           .
       420-MODORU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者紐付処理
      *****************************************************************
       430-GRPSYORI-SEC             SECTION.
      *
           IF      SPA-GMN2E-SYORI     =   ZERO
               INITIALIZE                  SPA-GMN2E-AREA
               MOVE    1               TO  SPA-GMN2E-SYORI
               MOVE    "＜患者紐付け複写＞"
                                       TO  SPA-GMN2E-MIDASI
           ELSE
               INITIALIZE                  SPA-GMN2E-AREA
               MOVE    ZERO            TO  SPA-GMN2E-SYORI
           END-IF
      *
           PERFORM 300-SPA-INIT-SEC
           MOVE    1                   TO  SPA-GMN2E-CUR
           .
       430-GRPSYORI-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       490-TOROKU-SEC             SECTION.
      *
      *    必須入力等のチェック
           PERFORM 410-INPUT-CHK-SEC 
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (FLG-END         NOT =   ZERO  )
               GO  TO                  490-TOROKU-EXT
           END-IF
      *
           IF      SPA-GMN2E-GRPHOSPNUM    =   SPACE
      *        自院複写
               PERFORM 4901-COPY-SYORI-SEC
           ELSE
      *        他院複写
               PERFORM 4902-GRPCOPY-SYORI-SEC
           END-IF
      *
      *    終了へ
           PERFORM 210-BACK
           .
       490-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *     自院複写処理
      *****************************************************************
       4901-COPY-SYORI-SEC         SECTION.
      *
      *    パラメタ削除
           PERFORM 4903-PARA-DEL-SEC
      *    患者情報複写
           PERFORM 4901-PTINF-SET-SEC
      *    他の情報複写
           PERFORM 4902-PARA-SET-SEC
           .
       4901-COPY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     他院複写処理
      *****************************************************************
       4902-GRPCOPY-SYORI-SEC         SECTION.
      *
           INITIALIZE                      ORCSP04AREA
           MOVE    SPA-GMN2E-GRPHOSPNUM
                                       TO  ORCSP04-HOSPNUM
           MOVE    SPA-GMN2E-PTID      TO  ORCSP04-PTID
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           CALL    "ORCSP04"           USING
                                       SPA-AREA
                                       ORCSP04AREA
                                       SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
           .
       4902-GRPCOPY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     保険・公費・保険組合せのパラメタ削除処理
      *****************************************************************
       4903-PARA-DEL-SEC         SECTION.
      *
      *    パラメタ削除
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       49021-PARA-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報複写
      *****************************************************************
       4901-PTINF-SET-SEC         SECTION.
      *
      *    氏名は入力があれば入力
           IF      SPA-GMN-NAME    NOT =   SPACE
               MOVE    SPA-GMN-NAME        TO  PTINF-NAME
               MOVE    SPA-GMN-KANANAME    TO  PTINF-KANANAME
           END-IF
      * 複写しない項目をクリアする
      *    生年月日
           MOVE    SPACE               TO  PTINF-BIRTHDAY
      *    性別
           MOVE    SPACE               TO  PTINF-SEX
      *    死亡
           MOVE    SPACE               TO  PTINF-DEATHKBN
      *    続柄
           MOVE    SPACE               TO  PTINF-ZOKUGARA
      *    状態
           MOVE    SPACE               TO  PTINF-CONDITION-1
           MOVE    SPACE               TO  PTINF-CONDITION-2
           MOVE    SPACE               TO  PTINF-CONDITION-3
      *    コメント等
           MOVE    SPACE               TO  PTINF-COMMENT-1
           MOVE    SPACE               TO  PTINF-COMMENT-2
           MOVE    SPACE               TO  PTINF-TABOO-1
           MOVE    SPACE               TO  PTINF-TABOO-2
           MOVE    SPACE               TO  PTINF-ALLERGY-1
           MOVE    SPACE               TO  PTINF-ALLERGY-2
           MOVE    SPACE               TO  PTINF-KANSENSYO-1
           MOVE    SPACE               TO  PTINF-KANSENSYO-2
      *
      *    画面編集
           MOVE    PTINF-NAME          TO  SPA-GMN-NAMEH
           MOVE    SPACE               TO  SPA-GMN-SEXH
           MOVE    SPACE               TO  SPA-GMN-BIRTHDAYH
           MOVE    SPACE               TO  SPA-GMN-BIRTHDAYW
           MOVE    PTINF-BIRTHDAY      TO  SPA-NAI-BIRTHDAY
           MOVE    SPACE               TO  SPA-GMN-NENREI
      *
           MOVE    PTINF-KANANAME      TO  SPA-GMN-KANANAME
           MOVE    PTINF-NAME          TO  SPA-GMN-NAME
      *    変更確認用氏名保存
           MOVE    SPA-GMN-KANANAME    TO  SPA-GMN-OLD-KANANAME
           MOVE    SPA-GMN-NAME        TO  SPA-GMN-OLD-NAME
      *    旧姓
           MOVE    PTINF-NAME          TO  SPA-OLD-NAME
           MOVE    PTINF-KANANAME      TO  SPA-OLD-KANANAME
           MOVE    PTINF-NICKNAME      TO  SPA-OLD-NICKNAME
      *
      *    性別
           MOVE    SPACE               TO  SPA-GMN-SEX-G
           MOVE    PTINF-SEX           TO  SPA-GMN-SEX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-SEX     =   SPA-GMN-SEX-LST(IDX)(1:1)
                   MOVE    SPA-GMN-SEX-LST (IDX)   TO  SPA-GMN-SEX-G
                   MOVE    2                       TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    PTINF-NICKNAME      TO  SPA-GMN-NICKNAME
           MOVE    PTINF-DEATHKBN      TO  SPA-GMN-DEATHKBN
      *    テスト患者
           MOVE    PTINF-TSTPTNUMKBN   TO  SPA-GMN-TSTPTNUMKBN
           IF      SPA-GMN-TSTPTNUMKBN =   SPACE
               MOVE    ZERO                TO  SPA-GMN-TSTPTNUMKBN
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-TSTPTNUMKBN =   SPA-GMN-TSTPTNUMKBN-LST
                                                        (IDX)(1:1)
                   MOVE    SPA-GMN-TSTPTNUMKBN-LST (IDX)
                                           TO  SPA-GMN-TSTPTNUMKBN-G
                   MOVE    2               TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    PTINF-HOME-POST     TO  SPA-GMN-POST
           MOVE    PTINF-HOME-ADRS     TO  SPA-GMN-EDITADRS-NAME
           MOVE    PTINF-HOME-BANTI    TO  SPA-GMN-BANTI
           MOVE    PTINF-HOME-TEL1     TO  SPA-GMN-TEL1
           MOVE    PTINF-HOME-TEL2     TO  SPA-GMN-TEL2
           MOVE    PTINF-SETAINUSI     TO  SPA-GMN-SETAINUSI
           MOVE    PTINF-ZOKUGARA      TO  SPA-GMN-ZOKUGARA
           MOVE    PTINF-COMMENT-1     TO  SPA-GMN-COMMENT-1
           MOVE    PTINF-COMMENT-2     TO  SPA-GMN-COMMENT-2
      *    減免事由
           MOVE    PTINF-DISCOUNT-KBN  TO  SPA-GMN-DISCOUNTKBN
      *    割引率
           MOVE    PTINF-DISCOUNT      TO  SPA-GMN-DISCOUNT
      *
      *    連絡先
           MOVE    PTINF-FAX           TO  SPA-GMN21-FAX
           MOVE    PTINF-KEITAI-TEL    TO  SPA-GMN21-KEITAI-TEL
           MOVE    PTINF-EMAIL         TO  SPA-GMN21-EMAIL
           MOVE    PTINF-JOB           TO  SPA-GMN21-JOB
           MOVE    PTINF-RENRAKU-NAME  TO  SPA-GMN21-RENRAKU-NAME
           MOVE    PTINF-RENRAKU-ZOKUGARA
                                       TO  SPA-GMN21-RENRAKU-ZOKUGARA
           MOVE    PTINF-RENRAKU-POST  TO  SPA-GMN21-RENRAKU-POST
           MOVE    PTINF-RENRAKU-ADRS  TO  SPA-GMN21-RENRAKU-ADRS
           MOVE    PTINF-RENRAKU-BANTI TO  SPA-GMN21-RENRAKU-BANTI
           MOVE    PTINF-RENRAKU-TEL1  TO  SPA-GMN21-RENRAKU-TEL1
           MOVE    PTINF-RENRAKU-TEL2  TO  SPA-GMN21-RENRAKU-TEL2
           MOVE    PTINF-OFFICE-NAME   TO  SPA-GMN21-OFFICE-NAME
           MOVE    PTINF-OFFICE-POST   TO  SPA-GMN21-OFFICE-POST
           MOVE    PTINF-OFFICE-ADRS   TO  SPA-GMN21-OFFICE-ADRS
           MOVE    PTINF-OFFICE-BANTI  TO  SPA-GMN21-OFFICE-BANTI
           MOVE    PTINF-OFFICE-TEL    TO  SPA-GMN21-OFFICE-TEL
           MOVE    PTINF-KISEI-NAME    TO  SPA-GMN21-KISEI-NAME
           MOVE    PTINF-KISEI-POST    TO  SPA-GMN21-KISEI-POST
           MOVE    PTINF-KISEI-ADRS    TO  SPA-GMN21-KISEI-ADRS
           MOVE    PTINF-KISEI-BANTI   TO  SPA-GMN21-KISEI-BANTI
           MOVE    PTINF-KISEI-TEL     TO  SPA-GMN21-KISEI-TEL
      *
      *    禁忌情報
           MOVE    PTINF-TABOO-1       TO  SPA-GMN-TABOO-1
           MOVE    PTINF-TABOO-2       TO  SPA-GMN-TABOO-2
           MOVE    PTINF-ALLERGY-1     TO  SPA-GMN-ALLERGY-1
           MOVE    PTINF-ALLERGY-2     TO  SPA-GMN-ALLERGY-2
           MOVE    PTINF-KANSENSYO-1   TO  SPA-GMN-KANSENSYO-1
           MOVE    PTINF-KANSENSYO-2   TO  SPA-GMN-KANSENSYO-2
      *
           IF  SPA-GMN-DISCOUNTKBN     =   SPACE
               MOVE    ZERO               TO  SPA-GMN-DISCOUNTKBN
           END-IF
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL   ( IDX       >   SPA-DISCOUNTKBN-MAX )
                     OR    ( SPA-GMN-DISCOUNTKBN-KBNCD(IDX) = SPACE )
               IF  SPA-GMN-DISCOUNTKBN
                                     =   SPA-GMN-DISCOUNTKBN-KBNCD(IDX)
                   MOVE    SPA-GMN-DISCOUNTKBN-BODY (IDX)
                                     TO  SPA-GMN-DISCOUNTKBN-MEI
                   MOVE    SPA-DISCOUNTKBN-MAX     TO  IDX
               END-IF
           END-PERFORM
      *    割引率
           MOVE    PTINF-DISCOUNT      TO  SPA-GMN-DISCOUNT
           IF  SPA-GMN-DISCOUNT (1:2)      =   SPACE
               MOVE    ZERO                TO  SPA-GMN-DISCOUNT (1:2)
           END-IF
           MOVE    SPACE                   TO  WRK-RATE-AREA
           MOVE    SPA-GMN-DISCOUNT        TO  WRK-RATE-KBN
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL   ( IDX         >   SPA-DISCOUNT-MAX )
                     OR      ( SPA-GMN-DISCOUNT-KBNCD(IDX) = SPACE )
               IF  WRK-RATE-KBN        =   SPA-GMN-DISCOUNT-KBNCD(IDX)
                   MOVE    SPA-GMN-DISCOUNT-RITU (IDX)
                                           TO  WRK-RATE-EDITZ
                   EVALUATE    SPA-GMN-DISCOUNT-TEIRITU (IDX)
                   WHEN    "0"
                       MOVE    "該当なし"  TO  WRK-RATE-EDIT-R
                   WHEN    "1"
                       MOVE    "(%)"       TO  WRK-RATE-TANI
                   WHEN    "2"
                       MOVE    "(円)"      TO  WRK-RATE-TANI
                   END-EVALUATE
                   MOVE    10              TO  IDX
               END-IF
           END-PERFORM
           MOVE    WRK-RATE-AREA       TO  SPA-GMN-DISCOUNT
      *    状態１〜３
           MOVE    PTINF-CONDITION-1   TO  SPA-GMN-CONDITION(1)
           IF      SPA-GMN-CONDITION(1)(1:2)  =   SPACE
               MOVE    ZERO            TO  SPA-GMN-CONDITION(1)(1:2)
           END-IF
           MOVE    1                   TO  IDX
           PERFORM 31110-CONDITIONH-SPASET-SEC
      *
           MOVE    PTINF-CONDITION-2   TO  SPA-GMN-CONDITION(2)
           IF      SPA-GMN-CONDITION(2)(1:2)   =   SPACE
               MOVE    ZERO            TO  SPA-GMN-CONDITION(2)(1:2)
           END-IF
           MOVE    2                   TO  IDX
           PERFORM 31110-CONDITIONH-SPASET-SEC
      *
           MOVE    PTINF-CONDITION-3   TO  SPA-GMN-CONDITION(3)
           IF      SPA-GMN-CONDITION(3)(1:2)   =   SPACE
               MOVE    ZERO            TO  SPA-GMN-CONDITION(3)(1:2)
           END-IF
           MOVE    3                   TO  IDX
           PERFORM 31110-CONDITIONH-SPASET-SEC
      *
      *    入力の有無判定用
           MOVE    SPA-GMN-POST        TO  SPA-MAE-POST
           MOVE    SPA-GMN-KANANAME    TO  SPA-MAE-KANANAME
           MOVE    SPA-GMN-NAME        TO  SPA-MAE-NAME
           MOVE    SPA-GMN-SEX         TO  SPA-MAE-SEX
           MOVE    SPA-GMN-BIRTHDAYW   TO  SPA-MAE-BIRTHDAY
           MOVE    SPA-GMN-DISCOUNTKBN TO  SPA-MAE-DISCOUNTKBN
      *
           .
       10021-PTINF-SET-EXT.
           EXIT.
      ****************************************************************
      *    状態名称スパセット処理
      ****************************************************************
       31110-CONDITIONH-SPASET-SEC         SECTION.
      *
           MOVE    SPA-GMN-CONDITION (IDX)(1:2)    TO  WRK-KBNCD
      *
           PERFORM   VARYING   IDY     FROM    1   BY  1
                     UNTIL   ( IDY     >   SPA-CONDITION-MAX(IDX) )
                     OR      ( SPA-GMN-CONDITION-KBNCD (IDX IDY)
                                           =   SPACE )
               IF      WRK-KBNCD       =   SPA-GMN-CONDITION-KBNCD
                                                           (IDX IDY)
                   MOVE    SPA-GMN-CONDITION-BODY (IDX IDY)
                                         TO  SPA-GMN-CONDITION(IDX)(4:)
                   MOVE    SPA-GMN-CONDITION-WORD (IDX IDY)
                                         TO  SPA-GMN-CONDITIONH (IDX)
                   MOVE    SPA-CONDITION-MAX(IDX)  TO  IDY
               END-IF
           END-PERFORM
      *
           .
       31110-CONDITIONH-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    年齢計算処理
      *****************************************************************
       41012-NENREI-SET-SEC             SECTION.
      *
      *
           MOVE    PTINF-BIRTHDAY      TO  WRK-YMDS1
           MOVE    SPA-SYSYMD          TO  WRK-YMDS2
           COMPUTE WRK-NENREI-YY       =   WRK-YYS2
                                       -   WRK-YYS1
           IF      WRK-YMDS1(5:4)      >   WRK-YMDS2(5:4)
               COMPUTE WRK-NENREI-YY   =   WRK-NENREI-YY
                                           -   1
               COMPUTE WRK-NENREI-MM   =  (WRK-MMS2  +  12 )
                                           -   WRK-MMS1
           ELSE
               COMPUTE WRK-NENREI-MM   =   WRK-MMS2
                                           -   WRK-MMS1
           END-IF
           IF      WRK-DDS1            >   WRK-DDS2
               COMPUTE WRK-NENREI-MM   =   WRK-NENREI-MM
                                           -   1
           END-IF
      *
      *    画面表示用
           IF      WRK-NENREI-YY       =   ZERO
               MOVE    WRK-NENREI-MM       TO  WRK-NENREI-ZN
               MOVE    "ヶ月"              TO  WRK-NENREI-MEIN
           ELSE
               MOVE    WRK-NENREI-YY       TO  WRK-NENREI-Z
               MOVE    "才"                TO  WRK-NENREI-MEI
           END-IF
           MOVE    WRK-NENREI-G        TO  SPA-GMN2E-NENREI
      *
           .
       41012-NENREI-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *     他の情報編集処理
      *****************************************************************
       4902-PARA-SET-SEC         SECTION.
      *
      *    保険者編集
           INITIALIZE                  SPA-PTHKNINF-AREA
      *    保険者編集
           INITIALIZE                  PTHKNINF-REC
           MOVE    PTINF-HOSPNUM       TO  PTHKN-HOSPNUM
           MOVE    SPA-GMN2E-PTID      TO  PTHKN-PTID
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM     UNTIL   FLG-PTHKNINF    =   1
               IF     (PTHKN-SAKJOKBN      =   SPACE )  AND
                      (PTHKN-HKNNUM    NOT =   "971" AND
                                               "973" )  AND
                      (PTHKN-TEKSTYMD      <=  SPA-SYSYMD AND
                       PTHKN-TEKEDYMD      >=  SPA-SYSYMD )
      *            保険番号マスタ登録チェック
                   PERFORM 410130-HKNNUM-CHK-SEC
                   IF      FLG-HKNNUM-OK       =   1
      *                内容の変更
                       MOVE    SPA-GMN-PTID        TO  PTHKN-PTID
      *
                       ADD     1                   TO  IDX
                       MOVE    SPACE               TO  PARA-REC
                       INITIALIZE                      PARA-REC
                       MOVE    "PTHKNINF"          TO  PARA-FILEMEI
                       MOVE    IDX                 TO  PARA-EDANUM
                       MOVE    PTHKNINF-REC        TO  PARA-DATA-REC
                       ADD     1                   TO  SPA-PTNUM-HKNID
                       MOVE    SPA-PTNUM-HKNID     TO  PTHKN-HKNID
                       PERFORM 400-PARA-INSERT-SEC
      *                ＳＰＡにも編集する
                       IF      IDX               >  40
                           CONTINUE
                       ELSE
                           PERFORM 100221-PTHKNINF-SPA-SEC
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           INITIALIZE                  SPA-PTKOHINF-AREA
           IF      FLG-KOHCOPY         =   ZERO
      *        公費編集
               PERFORM 100221-KOHCOPY-SYORI-SEC
           END-IF
      *    受付
           IF      SPA-P02-UKEID   NOT =   ZERO
               PERFORM 100229-UKETUKE-SET-SEC
           END-IF
           .
       10022-PARA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付作成処理
      *****************************************************************
       100221-KOHCOPY-SYORI-SEC           SECTION.
      *
      *    公費編集
           INITIALIZE                  PTKOHINF-REC
           MOVE    PTINF-HOSPNUM       TO  PTKOH-HOSPNUM
           MOVE    SPA-GMN2E-PTID      TO  PTKOH-PTID
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-PTKOHINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM     UNTIL     FLG-PTKOHINF  =   1
               IF     (PTKOH-SAKJOKBN        =   SPACE    )  AND
                      (PTKOH-TEKSTYMD      <=  SPA-SYSYMD AND
                       PTKOH-TEKEDYMD      >=  SPA-SYSYMD )
      *H25.11
      *            第三者行為対象外
                  AND (PTKOH-KOHNUM    NOT =   "970"      )
      *            保険番号マスタ登録チェック
                   PERFORM 410130-KOHNUM-CHK-SEC
                   IF      FLG-HKNNUM-OK       =   1
                       ADD     1                   TO  IDX
      *                内容の変更
                       MOVE    SPA-GMN-PTID        TO  PTKOH-PTID
                       MOVE    SPACE               TO  PTKOH-JKYSNUM
      *
                       MOVE    SPACE               TO  PARA-REC
                       INITIALIZE                      PARA-REC
                       MOVE    "PTKOHINF"          TO  PARA-FILEMEI
                       MOVE    IDX                 TO  PARA-EDANUM
                       MOVE    PTKOHINF-REC        TO  PARA-DATA-REC
                       ADD     1                   TO  SPA-PTNUM-KOHID
                       MOVE    SPA-PTNUM-KOHID     TO  PTKOH-KOHID
                       PERFORM 400-PARA-INSERT-SEC
      **               ＳＰＡにも編集する
                       IF      IDX               >  60
                           CONTINUE
                       ELSE
                           PERFORM 100222-PTKOHINF-SPA-SEC
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-PTKOHINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       100221-KOHCOPY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付作成処理
      *****************************************************************
       100229-UKETUKE-SET-SEC           SECTION.
      *
      *    パラメタ登録
           MOVE    SPACE               TO  UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    SPA-P02-UKEID       TO  UKE-UKEID
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC                  =   ZERO
                   MOVE    MCPDATA-REC         TO  UKETUKE-REC
      *
                   MOVE    SPACE               TO  PARA-REC
                   INITIALIZE                      PARA-REC
                   MOVE    "UKETUKE"           TO  PARA-FILEMEI
                   MOVE    1                   TO  PARA-EDANUM
                   MOVE    UKETUKE-REC         TO  PARA-DATA-REC
                   PERFORM 400-PARA-INSERT-SEC
               ELSE
                   MOVE    SPACE               TO  UKETUKE-REC
               END-IF
           END-IF
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       10022-PARA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタファイル作成処理
      *****************************************************************
       400-PARA-INSERT-SEC           SECTION.
      *
      *    パラメタ登録
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PARA            =   1
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02 PARA INSERT ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY(1:42)
               END-IF
           ELSE
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02 PARA UPDATE ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY(1:42)
               END-IF
           END-IF
      *
           .
       400-PARA-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報ＳＰＡ編集処理
      *****************************************************************
       100221-PTHKNINF-SPA-SEC           SECTION.
      *
           MOVE    PTHKN-HKNID         TO  SPA-PTHKN-HKNID      (IDX)
           MOVE    PTHKN-HKNNUM        TO  SPA-PTHKN-HKNNUM     (IDX)
           MOVE    PTHKN-HKNJANUM      TO  SPA-PTHKN-HKNJANUM   (IDX)
           MOVE    PTHKN-HIHKNJANAME   TO  SPA-PTHKN-HIHKNJANAME(IDX)
           MOVE    PTHKN-HONKZKKBN     TO  SPA-PTHKN-HONKZKKBN  (IDX)
           MOVE    PTHKN-HOJOKBN       TO  SPA-PTHKN-HOJOKBN    (IDX)
           MOVE    PTHKN-CONTKBN       TO  SPA-PTHKN-CONTKBN    (IDX)
           MOVE    PTHKN-KIGO          TO  SPA-PTHKN-KIGO       (IDX)
           MOVE    PTHKN-NUM           TO  SPA-PTHKN-NUM        (IDX)
           MOVE    PTHKN-SKKGETYMD     TO  SPA-PTHKN-SKKGETYMD  (IDX)
           MOVE    PTHKN-TEKSTYMD      TO  SPA-PTHKN-TEKSTYMD   (IDX)
           MOVE    PTHKN-TEKEDYMD      TO  SPA-PTHKN-TEKEDYMD   (IDX)
           MOVE    PTHKN-KAKUNINYMD    TO  SPA-PTHKN-KAKUNINYMD (IDX)
           MOVE    PTHKN-SAKJOKBN      TO  SPA-PTHKN-SAKJOKBN   (IDX)
      *
           MOVE    PTHKN-SKKGETYMD     TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTHKN-SKKGETYMDW (IDX)
      *
           MOVE    PTHKN-TEKSTYMD      TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTHKN-TEKSTYMDW (IDX)
      *
           MOVE    PTHKN-TEKEDYMD      TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTHKN-TEKEDYMDW (IDX)
      *
           MOVE    PTHKN-KAKUNINYMD    TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTHKN-KAKUNINYMDW (IDX)
      *
      *    保険公費区分編集
           MOVE    WRK-HKNKOHSBTKBN    TO  SPA-PTHKN-HKN-SBTKBN (IDX)
           MOVE    WRK-TANSEIDONAME    TO  SPA-PTHKN-HKNNUM-MEI (IDX)
      *    新規分
           MOVE    "1"                 TO  SPA-PTHKN-SINKIKBN (IDX)
      *
           MOVE    IDX                 TO  SPA-PTHKN-MAX
      *
           .
       100221-PTHKNINF-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報ＳＰＡ編集処理
      *****************************************************************
       100222-PTKOHINF-SPA-SEC           SECTION.
      *
           MOVE    PTKOH-KOHID         TO  SPA-PTKOH-KOHID      (IDX)
           MOVE    PTKOH-KOHNUM        TO  SPA-PTKOH-KOHNUM     (IDX)
           MOVE    PTKOH-FTNJANUM      TO  SPA-PTKOH-FTNJANUM   (IDX)
           MOVE    PTKOH-JKYSNUM       TO  SPA-PTKOH-JKYSNUM    (IDX)
           MOVE    PTKOH-TEKSTYMD      TO  SPA-PTKOH-TEKSTYMD   (IDX)
           MOVE    PTKOH-TEKEDYMD      TO  SPA-PTKOH-TEKEDYMD   (IDX)
           MOVE    PTKOH-KAKUNINYMD    TO  SPA-PTKOH-KAKUNINYMD (IDX)
           MOVE    PTKOH-SAKJOKBN      TO  SPA-PTKOH-SAKJOKBN   (IDX)
           MOVE    PTKOH-TEKEDCHKKBN   TO  SPA-PTKOH-TEKEDCHKKBN (IDX)
      *
           MOVE    PTKOH-TEKSTYMD      TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTKOH-TEKSTYMDW (IDX)
      *
           MOVE    PTKOH-TEKEDYMD      TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTKOH-TEKEDYMDW (IDX)
      *
           MOVE    PTKOH-KAKUNINYMD    TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTKOH-KAKUNINYMDW (IDX)
      *
      *    保険公費区分編集
           MOVE    WRK-HKNKOHSBTKBN    TO  SPA-PTKOH-HKN-SBTKBN (IDX)
           MOVE    WRK-TANSEIDONAME    TO  SPA-PTKOH-KOHNUM-MEI (IDX)
      *    支払区分
           MOVE    PTKOH-PAYKBN        TO  SPA-PTKOH-PAYKBN     (IDX)
      *
      *    新規分
           MOVE    "1"                 TO  SPA-PTKOH-SINKIKBN (IDX)
      *
           MOVE    IDX                 TO  SPA-PTKOH-MAX
      *
           .
       100222-PTKOHINF-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険公費区分編集処理
      *****************************************************************
       1002211-HKNNUM-SET-SEC           SECTION.
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    WRK-HKNNUM          TO  HKN-HKNNUM
           IF      WRK-PAYKBN          =   SPACE
               MOVE    ZERO                TO  HKN-PAYKBN
           ELSE
               MOVE    WRK-PAYKBN          TO  HKN-PAYKBN
           END-IF
           MOVE    WRK-TEKSTYMD        TO  HKN-TEKSTYMD
           MOVE    WRK-TEKEDYMD        TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 921-HKNMST-NEXT-SEC
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           MOVE    SPACE               TO  WRK-HKNKOHSBTKBN
           MOVE    SPACE               TO  WRK-TANSEIDONAME
           MOVE    ZERO                TO  FLG-HKNNUM-OK
           PERFORM
               UNTIL   FLG-HKNNUM      =   1
               IF     (WRK-TEKSTYMD        >=  HKN-TEKSTYMD )  AND
                      (WRK-TEKEDYMD        <=  HKN-TEKEDYMD )
                   MOVE    HKN-HKNKOHSBTKBN    TO  WRK-HKNKOHSBTKBN
                   MOVE    HKN-TANSEIDONAME    TO  WRK-TANSEIDONAME
                   MOVE    1                   TO  FLG-HKNNUM
                   MOVE    1                   TO  FLG-HKNNUM-OK
               ELSE
                   MOVE    "tbl_hknnum"        TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 921-HKNMST-NEXT-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       1002211-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "P02E"              TO  SPA-MOTOPG
           MOVE    "P02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "P02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           MOVE    ZERO                TO  SPA-FLG-END
           MOVE    "P02"               TO  SPA-P02-GAMEN
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    西暦→和暦FORMAT日付編集
      *****************************************************************
       4001-SEIWA-YMDEDIT-SEC           SECTION.
      *
           EVALUATE    WRK-SYMD
               WHEN    SPACE
                   MOVE    SPACE           TO  WRK-HENYMD
               WHEN    ZERO
                   MOVE    "0000000"       TO  WRK-HENYMD
               WHEN    "99999999"
                   MOVE    "9999999"       TO  WRK-HENYMD
               WHEN    OTHER
      *
                   MOVE    SPACE               TO  STS-AREA-DAY
                   INITIALIZE                      STS-AREA-DAY
                   MOVE    SPACE               TO  LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                   IF      STS-DAY-RC1         =   ZERO
                       MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
                   ELSE
                       MOVE    SPACE               TO  WRK-HENYMD
                   END-IF
           END-EVALUATE
           .
       4001-SEIWA-YMDEDIT-EXT.
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
           IF      SPA-PIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "P02E"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
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
                   MOVE    "該当の患者は存在しません"
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "医療機関番号がありません"
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   STRING    "既に紐付けされている患者です。"
                             "紐付けできません。"
                                               INTO  SPA-ERRMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P02E"              TO  SPA-MOTOPG
           MOVE    "PERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
      *    EVALUATE    SPA-PIDCD
      *        WHEN    OTHER
                   MOVE    SPA-PIDCD
                                       TO  WRK-PIDMSG
      *!!  END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PID1-FLG
      *
           MOVE    SPACE               TO  PID1
           INITIALIZE                      PID1
           MOVE    SPA-PIDCD           TO  PID1-ID1CODE
           MOVE    WRK-PIDMSG          TO  PID1-ID1MSG
           MOVE    "戻る"              TO  PID1-B01
           MOVE    "ＯＫ"              TO  PID1-B12
      ***  EVALUATE    SPA-PIDCD
      *        WHEN    "0103"
      *            MOVE    "B01"               TO  MCP-WIDGET
      ***      WHEN    OTHER
                   MOVE    "B12"               TO  MCP-WIDGET
      *****END-EVALUATE
      *
           MOVE    "P02E"              TO  SPA-MOTOPG
           MOVE    "PID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-JIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    同一患者識別マスター読込処理
      *****************************************************************
       900-PTSAME-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTSAME
               MOVE    MCPDATA-REC         TO  PTSAME-REC
           ELSE
               MOVE    1                   TO  FLG-PTSAME
           END-IF
      *
           .
       900-PTSAME-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       910-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_pthkninf"     TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費履歴読み込み
      *****************************************************************
       920-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       920-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスタ次検索処理
      *****************************************************************
       921-HKNMST-NEXT-SEC              SECTION.
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
       921-HKNMST-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
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
      *
