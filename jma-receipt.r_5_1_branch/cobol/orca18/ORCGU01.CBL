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
       PROGRAM-ID.                 ORCGU01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 受付登録
      *  コンポーネント名  : 受付一覧（Ｕ０１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/05/24  患者番号ゼロ詰め処理
      *  01.01.01    MCC-藤原     01/09/21  JOINの修正      
      *  01.01.02    NACL-多々納  03/06/16  確認画面追加
      *  01.01.03    NACL-森脇    03/06/24  印刷処理追加
      *  01.01.04    NACL-多々納  03/06/25  受付一覧更新追加
      *  01.01.05    NACL-多々納  03/06/27  状態編集追加
      *  01.01.06    NACL-多々納  05/12/07  送受信フラグ追加
      *                                     MONFUNC対応
      *  03.04.00    NACL−多々納 07/01/12  受付一覧年齢追加
      *  03.05.00    NACL-多々納  07/05/07  グループ診療対応
      *  04.05.00    NACL-多々納  09/06/03  400行対応
      *  04.08.00    NACL-多々納  14/08/12  クライアント印刷対応
      *  04.08.00    NACL-多々納  16/12/XX  受付順変更表示対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    画面共通ＳＰＡ
           COPY    "U01COMMON-SPA".
      *
       01  SPA-U01.
           03  SPA-GMN-AREA.
               05  SPA-GMN-PAGE              PIC 9(02).
               05  SPA-GMN-CUR               PIC 9(02).
      *********05  SPA-GMN-DATE              PIC X(30).
               05  SPA-GMN-ICHIRAN.
               07  SPA-GMN-TBL.
                   09  SPA-GMN-TBL2      OCCURS  2.
                       11  SPA-GMN-TBLREC    OCCURS  400.
                           13  SPA-GMN-NO          PIC 9(04).
                           13  SPA-GMN-PTNUM       PIC X(20).
                           13  SPA-GMN-PTID        PIC 9(10).
                           13  SPA-GMN-NAME        PIC X(50).
                           13  SPA-GMN-NENREI      PIC X(06).
                           13  SPA-GMN-NICKNAME    PIC X(50).
                           13  SPA-GMN-CONDITION   PIC X(06).
                           13  SPA-GMN-SRYKAMEI    PIC X(06).
                           13  SPA-GMN-SRYKA       PIC X(02).
                           13  SPA-GMN-SRYNAIYOMEI PIC X(08).
                           13  SPA-GMN-SRYNAIYO    PIC X(02).
                           13  SPA-GMN-DRCD        PIC X(05).
                           13  SPA-GMN-DRCDMEI     PIC X(24).
                           13  SPA-GMN-HKNCOMBI    PIC X(04).
                           13  SPA-GMN-HKNCOMBIMEI PIC X(50).
                           13  SPA-GMN-UKETIMEH    PIC X(08).
                           13  SPA-GMN-YYKTIMEH    PIC X(08).
                           13  SPA-GMN-UKETIME     PIC X(06).
                           13  SPA-GMN-YYKTIME     PIC X(04).
                           13  SPA-GMN-UKEID       PIC 9(05).
      *                    送受信フラグ
                           13  SPA-GMN-SRFLG       PIC X(04).
      *H28.12
      *                    変更フラグ
                           13  SPA-GMN-HENFLG      PIC X(01).
      *
                   09  SPA-GMN-SELNUM          PIC 9(04) OCCURS  2.
                   09  SPA-GMN-MAX             PIC 9(03) OCCURS  2.
      *
           03  SPA-NAI-SELNO               PIC 9(04)     OCCURS  2.
      *
           03  SPA-NAI-AREA.
               05  SPA-NAI-SELFLG          PIC 9(01).
               05  SPA-NAI-SELNUM          PIC 9(04).
               05  SPA-NAI-UPDOWN          PIC 9(01).
      ****     05  SPA-NAI-IRYID           PIC X(10).
      ****     05  SPA-NAI-MOTOPG          PIC X(08).
      *
      *        画面遷移用パラメタに患者番号あり
      *        05  SPA-NAI-LNKFLG          PIC X(01).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-PTRSIINF            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(01).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SELNO           PIC 9(04).
      *    システム日付西暦
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
      *    システム日付和暦編集済
           03  WRK-WSYSYMDH        PIC X(09).
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
      *                            選択された明細インデックス
           03  WRK-IDX             PIC 9(003).
      *                            明細行変更のため入替対象インデックス
           03  WRK-IDY             PIC 9(003).
      *
           03  WRK-SYORI           PIC X(02).
      *
           03  WRK-ZZZ9            PIC ZZZ9.
      *
           03  WRK-HKNCOMBI        PIC X(100).
           03  WRK-HKNKBN-MEI      PIC X(02).
      *
           03  WRK-UKETIME         PIC X(10).
      *    確認メッセージ
           03  WRK-U1IDMSG         PIC X(80).
      *
      *    コラムリスト位置
           03  WRK-U01-SI-ROW          PIC S9(09).
           03  WRK-U01-SZ-ROW          PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
      *
      *   年齢表示
       01  WRK-NENREI-G.
           03  WRK-NENREI-Z        PIC ZZ9.
           03  WRK-NENREI-MEI      PIC X(02).
           03  WRK-NENREI-B        PIC X(01).
       01  WRK-NENREI-GR           REDEFINES   WRK-NENREI-G.
           03  WRK-NENREI-ZN       PIC Z9.
           03  WRK-NENREI-MEIN     PIC X(04).
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
           03  WRK-HENYMDN.
               05  WRK-HENNYMD     PIC X(22).
               05  FILLER          PIC X(02)   VALUE  "（".
               05  WRK-HENNYOBI    PIC X(02).
               05  FILLER          PIC X(02)   VALUE  "）".
      *
      *    一覧表最大行数
       01  LINE-MAX                    PIC 9(03)   VALUE   400.
      *
      *    ＳＰＡソート用ワーク
       01  WRK-SORT-GMN.
           03  SPA-WRK-TBL.
               05  SPA-WRK-TBLREC    OCCURS  400.
                   07  SPA-WRK-NO          PIC 9(04).
                   07  SPA-WRK-PTNUM       PIC X(20).
                   07  SPA-WRK-PTID        PIC 9(10).
                   07  SPA-WRK-NAME        PIC X(50).
                   07  SPA-WRK-NENREI      PIC X(06).
                   07  SPA-WRK-NICKNAME    PIC X(50).
                   07  SPA-WRK-CONDITION   PIC X(06).
                   07  SPA-WRK-SRYKAMEI    PIC X(06).
                   07  SPA-WRK-SRYKA       PIC X(02).
                   07  SPA-WRK-SRYNAIYOMEI PIC X(08).
                   07  SPA-WRK-SRYNAIYO    PIC X(02).
                   07  SPA-WRK-DRCD        PIC X(05).
                   07  SPA-WRK-DRCDMEI     PIC X(24).
                   07  SPA-WRK-HKNCOMBI    PIC X(04).
                   07  SPA-WRK-HKNCOMBIMEI PIC X(50).
                   07  SPA-WRK-UKETIMEH    PIC X(08).
                   07  SPA-WRK-YYKTIMEH    PIC X(08).
                   07  SPA-WRK-UKETIME     PIC X(06).
                   07  SPA-WRK-YYKTIME     PIC X(04).
                   07  SPA-WRK-UKEID       PIC 9(05).
      *            送受信フラグ
                   07  SPA-WRK-SRFLG       PIC X(04).
      *H28.12
      *            変更フラグ
                   07  SPA-WRK-HENFLG      PIC X(01).
      *
      *   受付内容編集領域
        01  WORK-SORT-TBL.
            03  WORK-SORT-TBLO                OCCURS   2.
                05  WORK-SORT-TBLREC          OCCURS   400.
                   07  WORK-SORT-PTID         PIC X(10).
                   07  WORK-SORT-NAME         PIC X(50).
                   07  WORK-SORT-NICKNAME     PIC X(50).
                   07  WORK-SORT-CONDITION    PIC X(06).
                   07  WORK-SORT-SRYKA        PIC X(02).
                   07  WORK-SORT-SRYNAIYO     PIC X(02).
                   07  WORK-SORT-DRCD         PIC X(05).
                   07  WORK-SORT-HKNCOMBI     PIC X(04).
                   07  WORK-SORT-UKETIME      PIC X(06).
                   07  WORK-SORT-YYKTIME      PIC X(04).
                   07  WORK-SORT-KEY          PIC X(10).
                   07  WORK-SORT-UKEID        PIC 9(05).
      *            送受信フラグ
                   07  WORK-SORT-SRFLG        PIC X(02).
      *H28.12
      *            変更フラグ
                   07  WORK-SORT-HENFLG       PIC X(01).
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"      REPLACING
                                     //SPA-// BY //WRK-SPA-//.
      *
      *
      *****************************************************************
      *    ファイルレイアウト領域
      *****************************************************************
      *
      *    受付
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    診療科情報
           COPY  "CPSK1005.INC".
      *    診療内容情報
           COPY  "CPSK1012.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *    状態情報
           COPY    "CPSK1018.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *
      *****************************************************************
      *    サブプロ　領域
      *****************************************************************
      *    日付サブルーチン領域
           COPY  "CPORCSDAY.INC".
           COPY  "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY  "CPORCSNUM.INC".
      *
      *    患者番号変換
           COPY  "CPORCSPTID.INC".
      *
      *    保険年齢・負担割合算定サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡      領域
      *****************************************************************
      *
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "U01.INC".
           COPY    "U012.INC".
           COPY    "UDR.INC".
           COPY    "U1ERR.INC". 
           COPY    "U1ID1.INC". 
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
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-WORK        TO  SPA-U01KYOUTU
           MOVE    SPA-FREE        TO  SPA-U01
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN     "LINK"         ALSO    ANY
                   PERFORM 100-INIT
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
           MOVE    SPA-U01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-U01         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM.
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "U1ERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
      *D   MOVE    SPACE               TO  LINKAREA
      *
      ***  MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   SPACE                TO  MCP-PUTTYPE.
           MOVE   "U01"                TO  MCP-WINDOW.
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
           EVALUATE    SPA-MOTOPG
      *        確認画面より
               WHEN    "U1ID1"
                   PERFORM 330-U1ID1-SET-SEC
      *
      *        WHEN    "U012"
      *            受付情報表示
      *            PERFORM 340-U012-HEN-SEC
      *
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-U01
      *
      *    画面遷移後、ＬＮＫをセット
           IF      LNK-KYOUTU      NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               IF      SPA-MOTOPG     NOT  =   "U012"
                    INITIALIZE                  SPA-U01KYOUTU
                    INITIALIZE                  SPA-U01
                    MOVE    SPA-MOTOPG          TO  SPA-NAI-MOTOPG
               END-IF
           END-IF
      *
      *    INITIALIZE                  SPA-U01
      *    MOVE    SPA-MOTOPG          TO  SPA-NAI-MOTOPG
      *
      *    曜日変換
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    SPA-SYSYMD          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING  STS-AREA-DAY
                                              LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE  LNK-DAY2-EDTYMD3     TO  WRK-HENNYMD
           END-IF
           INITIALIZE                  LNK-DAY3-AREA
           MOVE    "31"                TO  LNK-DAY3-IRAI
           MOVE    SPA-SYSYMD          TO  LNK-DAY3-YMD
           CALL    "ORCSDAY"           USING  STS-AREA-DAY
                                              LNK-DAY3-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE  LNK-DAY3-YOBIMEI  TO  WRK-HENNYOBI
           END-IF
           MOVE    WRK-HENYMDN         TO  SPA-GMN-DATE
      *    受付情報表示
           PERFORM                     5001-U010-HEN-SEC
      *
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    SPACE           TO  WRK-SYORI
      *
           .
       310-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    選択一覧画面より
      *****************************************************************
       340-U012-HEN-SEC               SECTION.
      *
           INITIALIZE                  SPA-U01
      *    受付情報表示
           PERFORM 5001-U010-HEN-SEC
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    1               TO  SPA-GMN-CUR
           .
       340-U012-HEN-EXT.
           EXIT.
      *****************************************************************
      *    確認画面より
      *****************************************************************
       330-U1ID1-SET-SEC               SECTION.
      *
           EVALUATE    SPA-U1IDCD
      *        印刷確認
               WHEN    "0001"
                   IF      SPA-U1ID1-FLG    =   "OK"
      *                印刷処理
                       PERFORM 800-PRINT-SYORI-SEC
                   END-IF
           END-EVALUATE
           MOVE    SPACE               TO  SPA-U1IDCD
           MOVE    SPACE               TO  SPA-U1ID1-FLG
      *
           .
       330-U1ID1-SET-EXT.
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
                   PERFORM 210-BACK
      *        確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-KAKUTEI-SEC
      *           ＵＰボタン選択
               WHEN    "CLICKED"       ALSO    "BUP"
                   PERFORM 41011-UPSYORI-SEC
      *            ＤＷボタン選択
               WHEN    "CLICKED"       ALSO    "BDOWN"
                   PERFORM 41012-DWNSYORI-SEC
      *        印刷処理
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 4010-PRINT-SEC
      *        受付一覧更新
               WHEN    "CLICKED"       ALSO    "pandatimer1"
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 4011-UKELIST-SEC
      *        選択一覧
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 490-U012-SYORI-SEC
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
      *        受付選択入力
               WHEN    "ACTIVATE"      ALSO    "SELNUM1"
                   PERFORM 41013-BANSELI-SEC
      *        会計済選択入力
               WHEN    "ACTIVATE"      ALSO    "SELNUM2"
                   PERFORM 41014-BANSELZ-SEC
      *        行選択
               WHEN    ANY             ALSO    "LIST1"
                   PERFORM 41015-LIST1SEL-SEC
      *        行選択
               WHEN    ANY             ALSO    "LIST2"
                   PERFORM 41016-LIST2SEL-SEC
           END-EVALUATE
      *
      *
           .
       200-ENTRY-EXT.
           EXIT.
      ***************************************************************
      *    ＵＰボタン選択 処理
      ***************************************************************
       41011-UPSYORI-SEC               SECTION.
      *
           PERFORM 40112-SELNUM-HEN-SEC
      *
           MOVE    SPA-GMN-SELNUM(1)    TO  WRK-IDX
           IF     (WRK-IDX             >   ZERO )  AND
                  (SPA-GMN-MAX (1)     >   1    )  AND
                  (WRK-IDX         NOT =   1    )
      *       １行上と入替表示
               PERFORM 410112-UPEDIT-SEC
               MOVE    WRK-IDX      TO  SPA-GMN-SELNUM(1)
           END-IF
      *
           .
      *
       41011-UPSYORI-EXT.
           EXIT.
      ***************************************************************
      *    選択番号　変換処理
      ***************************************************************
       40112-SELNUM-HEN-SEC               SECTION.
      *
           MOVE    ZERO                 TO  SPA-NAI-SELNO (1)
      *     
           IF      U01-SI-SELNUM        =   SPACE
               MOVE    ZERO                TO  SPA-GMN-SELNUM (1)
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    U01-SI-SELNUM        TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SYOKBN         =   "1"   )
                   MOVE    "0001"              TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM     TO  SPA-GMN-SELNUM (1)
               END-IF
           END-IF
           .
       40112-SELNUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細ＵＰ処理
      *****************************************************************
       410112-UPEDIT-SEC              SECTION.
      *
           IF  WRK-IDX                <   2
               GO  TO                 410112-UPEDIT-EXT
           END-IF
      *                                   入替対象インデックス取得
           COMPUTE     WRK-IDY        =   WRK-IDX  -   1
      *
           MOVE    SPA-GMN-TBL2(1)    TO  SPA-WRK-TBL
      *H28.12
           MOVE    "1"                TO  SPA-GMN-HENFLG(1 WRK-IDX)
      *
           MOVE    SPA-GMN-TBLREC (1 WRK-IDX)
                                      TO  SPA-WRK-TBLREC (WRK-IDY)
           MOVE    SPA-GMN-TBLREC (1 WRK-IDY)
                                      TO  SPA-WRK-TBLREC (WRK-IDX)
           MOVE    WRK-IDX            TO  SPA-WRK-NO     (WRK-IDX)
           MOVE    WRK-IDY            TO  SPA-WRK-NO     (WRK-IDY)
           MOVE    SPA-WRK-TBL        TO  SPA-GMN-TBL2   (1)
      *
           MOVE    1                  TO  SPA-NAI-UPDOWN
      *
           IF  WRK-IDX                >   1
               COMPUTE     WRK-IDX    =   WRK-IDX  -   1
           END-IF
           .
       410112-UPEDIT-EXT.
           EXIT.
      ***************************************************************
      *    ＤＷボタン選択 処理
      ***************************************************************
       41012-DWNSYORI-SEC               SECTION.
      *
           PERFORM 40112-SELNUM-HEN-SEC
      *
           MOVE    SPA-GMN-SELNUM(1)    TO  WRK-IDX
      *
           IF     (WRK-IDX             >   ZERO )   AND
                  (SPA-GMN-MAX(1)      >   1    )   AND
                  (WRK-IDX    NOT      =   SPA-GMN-MAX(1) )
      *        １行下と入替表示
                PERFORM 410121-DOWNEDIT-SEC
                MOVE    WRK-IDX      TO  SPA-GMN-SELNUM(1)
           END-IF
           .
      *
       41012-DWNSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細ＤＯＷＮ処理
      *****************************************************************
       410121-DOWNEDIT-SEC               SECTION.
      *
           IF  WRK-IDX                >=   SPA-GMN-MAX(1)
               GO  TO                 410121-DOWNEDIT-EXT
           END-IF
      *                                   入替対象インデックス取得
           COMPUTE     WRK-IDY        =   WRK-IDX  +   1
      *
           MOVE    SPA-GMN-TBL2  (1)  TO  SPA-WRK-TBL
      *H28.12
           MOVE    "1"                TO  SPA-GMN-HENFLG(1 WRK-IDX)
      *
           MOVE    SPA-GMN-TBLREC(1 WRK-IDX)
                                      TO  SPA-WRK-TBLREC (WRK-IDY)
           MOVE    SPA-GMN-TBLREC(1 WRK-IDY)
                                      TO  SPA-WRK-TBLREC (WRK-IDX)
           MOVE    WRK-IDX            TO  SPA-WRK-NO     (WRK-IDX)
           MOVE    WRK-IDY            TO  SPA-WRK-NO     (WRK-IDY)
           MOVE    SPA-WRK-TBL        TO  SPA-GMN-TBL2   (1)
      *
           MOVE    1                  TO  SPA-NAI-UPDOWN
      *
           COMPUTE     WRK-IDX        =   WRK-IDX  +   1
           .
       410121-DOWNEDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号選択処理
      *****************************************************************
       41013-BANSELI-SEC        SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           MOVE    ZERO                 TO  SPA-NAI-SELNO (1)
                                            SPA-NAI-SELNUM
      *
           PERFORM 40112-SELNUM-HEN-SEC
      *    選択番号チェック
           IF      SPA-GMN-SELNUM(1)  =   ZERO
               MOVE    SPA-GMN-SELNUM(1)  TO  SPA-NAI-SELNUM
               MOVE    ZERO               TO  SPA-NAI-SELFLG
               GO      TO            41013-BANSELI-EXT
           END-IF
           IF      SPA-GMN-SELNUM(1)     >   SPA-GMN-MAX(1)
               MOVE    "0001"        TO  SPA-ERRCD
               GO      TO            41013-BANSELI-EXT
           END-IF
      *
           MOVE    SPA-GMN-SELNUM(1) TO  SPA-NAI-SELNUM
                                         SPA-NAI-SELNO (1)
           MOVE    1                 TO  SPA-NAI-SELFLG
      *
           MOVE    2                 TO  SPA-GMN-CUR
      *
           .
       41013-BANSELI-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号選択処理
      *****************************************************************
       41014-BANSELZ-SEC        SECTION.
      *
           MOVE    3                    TO  SPA-GMN-CUR
      *     
           MOVE    ZERO                 TO  SPA-NAI-SELNO (2)
                                            SPA-NAI-SELNUM
      *
           PERFORM 40142-SELNUM2-HEN-SEC
      *    選択番号チェック
           IF      SPA-GMN-SELNUM(2)  =   ZERO
               MOVE    SPA-GMN-SELNUM(2) TO  SPA-NAI-SELNUM
               MOVE    ZERO              TO  SPA-NAI-SELFLG
               GO      TO            41014-BANSELZ-EXT
           END-IF
           IF      SPA-GMN-SELNUM(2)  >   SPA-GMN-MAX(2)
               MOVE    "0001"        TO  SPA-ERRCD
               GO      TO            41014-BANSELZ-EXT
           END-IF
      *
           MOVE    SPA-GMN-SELNUM(2) TO  SPA-NAI-SELNUM
                                         SPA-NAI-SELNO (2)
           MOVE    2                 TO  SPA-NAI-SELFLG
      *
           MOVE     2                TO  SPA-GMN-CUR
      *
           .
       41014-BANSELZ-EXT.
           EXIT.
      ***************************************************************
      *    選択番号　変換処理
      ***************************************************************
       40142-SELNUM2-HEN-SEC               SECTION.
      *     
           IF      U01-SZ-SELNUM        =   SPACE
               MOVE    ZERO                TO  SPA-GMN-SELNUM (2)
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    U01-SZ-SELNUM        TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SYOKBN         =   "1"   )
                   MOVE    "0001"              TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM     TO  SPA-GMN-SELNUM (2)
               END-IF
           END-IF
           .
       40142-SELNUM2-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41015-LIST1SEL-SEC       SECTION.
      *        行選択
           MOVE    ZERO                TO  WRK-SELNO
      *     
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       LINE-MAX
               IF      U01-SI-SELECT (IDX)     =   "T"
                   IF      IDX                 =   SPA-NAI-SELNO (1)
                       CONTINUE
                   ELSE
                       MOVE    IDX             TO  WRK-SELNO
                       MOVE    LINE-MAX        TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
           IF      WRK-SELNO           >   ZERO
               MOVE    WRK-SELNO           TO  SPA-NAI-SELNO (1)
           END-IF
      *
           MOVE    SPA-NAI-SELNO (1) TO  SPA-GMN-SELNUM (1)
      *
           MOVE    SPA-GMN-SELNUM(1) TO  SPA-NAI-SELNUM
           MOVE    1                 TO  SPA-NAI-SELFLG
      *
           MOVE     2                TO  SPA-GMN-CUR
           .
       41015-LIST1SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41016-LIST2SEL-SEC       SECTION.
      *        行選択
           MOVE    ZERO                TO  WRK-SELNO
      *     
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       LINE-MAX
               IF      U01-SZ-SELECT (IDX)     =   "T"
                   MOVE    IDX                 TO  WRK-SELNO
                   MOVE    LINE-MAX            TO  IDX
               END-IF    
           END-PERFORM
      *
           IF      WRK-SELNO           >   ZERO
               MOVE    WRK-SELNO           TO  SPA-NAI-SELNO (2)
           END-IF
      *
           MOVE    SPA-NAI-SELNO (2) TO  SPA-GMN-SELNUM (2)
      *
           MOVE    SPA-GMN-SELNUM(2) TO  SPA-NAI-SELNUM
           MOVE    2                 TO  SPA-NAI-SELFLG
      *
           MOVE     2                TO  SPA-GMN-CUR
      *
           .
       41016-LIST2SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       490-KAKUTEI-SEC                SECTION.
      *
      *    前の画面で患者番号が決定されている時、選択エラーとする
           IF     (SPA-NAI-SELNUM  NOT =   ZERO )  AND
                  (SPA-PTNUM       NOT =   SPACE)
               MOVE    "0011"          TO  SPA-ERRCD
               GO      TO      490-KAKUTEI-EXT
           END-IF
      *
           IF      SPA-NAI-SELNUM   NOT =   ZERO
               IF      SPA-MOTOPG(1:1)     =   "U"   OR
                                               "P"   OR
                                               "Q"
                   CONTINUE
               ELSE
                   IF      SPA-GMN-PTID (SPA-NAI-SELFLG SPA-NAI-SELNUM)
                                               =   ZERO
                       MOVE    "0010"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *        ＯＫの時、ＳＰＡ確定後、元の画面へ
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4901-LNKPARA-SET-SEC
               PERFORM 210-BACK
           END-IF
          .
       490-KAKUTEI-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移用パラメタ作成処理
      *****************************************************************
       4901-LNKPARA-SET-SEC            SECTION.
      *
           IF       SPA-NAI-SELNUM      >   ZERO
      *            対象がある時、患者情報をセットする
               PERFORM 49011-KNJMST-HEN-SEC
           END-IF
      *
           .
       4901-LNKPARA-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移用パラメタ患者情報編集処理
      *****************************************************************
       49011-KNJMST-HEN-SEC            SECTION.
      *
           MOVE    SPA-GMN-PTNUM (SPA-NAI-SELFLG SPA-NAI-SELNUM)
                                       TO  SPA-PTNUM
           MOVE    SPA-GMN-PTID (SPA-NAI-SELFLG SPA-NAI-SELNUM)
                                       TO  SPA-PTID
           MOVE    SPA-GMN-NAME  (SPA-NAI-SELFLG SPA-NAI-SELNUM)
                                       TO  SPA-NAME
           MOVE    SPA-GMN-SRYKA (SPA-NAI-SELFLG SPA-NAI-SELNUM)
                                       TO  SPA-SRYKA
           MOVE    SPA-GMN-SRYKAMEI (SPA-NAI-SELFLG SPA-NAI-SELNUM)
                                       TO  SPA-SRYKAMEI
      ***  MOVE    SPA-GMN-UKETIME(SPA-NAI-SELFLG SPA-NAI-SELNUM)
      ***                              TO  SPA-UKETIME
           MOVE    SPA-GMN-DRCD   (SPA-NAI-SELFLG SPA-NAI-SELNUM)
                                       TO  SPA-DRCD
      *
           MOVE    SPA-GMN-HKNCOMBI(SPA-NAI-SELFLG SPA-NAI-SELNUM)
                                       TO  SPA-HKNCOMBINUM
      *
           MOVE    SPA-GMN-UKEID  (SPA-NAI-SELFLG SPA-NAI-SELNUM)
                                       TO  SPA-P02-UKEID
      *
           IF      SPA-PTID            =   ZERO
               CONTINUE
           ELSE
               INITIALIZE                      PTINF-REC
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    SPA-PTID            TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
      *
               MOVE    PTINF-KANANAME      TO  SPA-KANANAME
               MOVE    PTINF-NAME          TO  SPA-NAME
               EVALUATE    PTINF-SEX
                   WHEN    "1"
                       MOVE    "男"                TO  SPA-SEX
                   WHEN    "2"
                       MOVE    "女"                TO  SPA-SEX
               END-EVALUATE
               MOVE    PTINF-BIRTHDAY      TO  SPA-BIRTHDAY
      *
               MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               IF     (STS-DAY-RC1         =   0 )    AND
                      (STS-DAY-RC2         =   0 ) 
                   MOVE    LNK-DAY2-EDTYMD1    TO  SPA-BIRTHDAYW
               ELSE
                   MOVE    SPACE               TO  SPA-BIRTHDAYW
               END-IF
           END-IF
      *
           .
       49011-KNJMST-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       4010-PRINT-SEC              SECTION.
      *    印刷処理
           IF     (SPA-GMN-MAX (1)     =   ZERO )  AND
                  (SPA-GMN-MAX (2)     =   ZERO )
               MOVE    "0012"              TO  SPA-ERRCD
           ELSE
               MOVE    "0001"              TO  SPA-U1IDCD
           END-IF
      *
           .
       4010-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       800-PRINT-SYORI-SEC              SECTION.
      *
      *
           IF     (SPA-CLIENT-PRT-FLG  =   2 )
             OR   (SPA-PRTCONF         =   "1" )
      *        クライアント印刷制御情報取得処理
               PERFORM 8001-CLIENT-PRT-SEC
           END-IF
           MOVE    "01"                TO  SPA-PRT-FLG
      *
           CALL    "ORCHCM16"          USING
                                       SPA-AREA
      *
      *    クライアント印刷
           IF     (SPA-CLIENT-PRT-FLG   =   1  )
      *        印刷ダミー処理
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
      *
           .
       800-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    クライアント印刷前　処理
      *****************************************************************
       8001-CLIENT-PRT-SEC         SECTION.
      *
           CALL    "ORCSONPRTSET"      USING   SPA-AREA
      *
      *    クライアント印刷制御情報取得サブ
           INITIALIZE                  ORCSPRTCTRLAREA
           CALL    "ORCSPRTCTRL"       USING   SPA-AREA
                                               ORCSPRTCTRLAREA
                                               MCPAREA
      *
           .
       8001-CLIENT-PRT-EXT.
           EXIT.
      *****************************************************************
      *    受付一覧更新処理
      *****************************************************************
       4011-UKELIST-SEC              SECTION.
      *
      *    選択中でないこと
           IF      SPA-GMN-SELNUM(1)   NOT =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      4011-UKELIST-EXT
           END-IF
           IF      SPA-GMN-SELNUM(2)   NOT =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
               GO      TO      4011-UKELIST-EXT
           END-IF
      *
      *    受付情報表示
           PERFORM                     5001-U010-HEN-SEC
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       4011-UKELIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    受付ファイルｓｏｒｔｋｅｙセット
           IF      SPA-NAI-UPDOWN      =   1
               PERFORM 503-JYUSIN-RWT-SEC
           END-IF
      *
      *    予約からの時、内容の保存
           IF      SPA-NAI-MOTOPG      =   "Y01"
               MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
               MOVE    WRK-SPA-WORK        TO  SPA-WORK
           END-IF
      *
           MOVE    SPA-NAI-MOTOPG      TO  SPA-SAKIPG
           MOVE    "U01"               TO  SPA-MOTOPG
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           IF      SPA-SAKIPG          =   SPACE  OR  "U01"
               MOVE   "M01"            TO  SPA-SAKIPG
           END-IF
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    選択一覧処理
      *****************************************************************
       490-U012-SYORI-SEC             SECTION.
      *
      *    受付ファイルｓｏｒｔｋｅｙセット
           IF      SPA-NAI-UPDOWN      =   1
               PERFORM 503-JYUSIN-RWT-SEC
           END-IF
      *
           INITIALIZE                      SPA-U012-AREA
      *
           MOVE    "U01"               TO  SPA-U012-MOTOPG
           MOVE    SPA-U01KYOUTU       TO  SPA-WORK
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "U01"               TO  SPA-MOTOPG
           MOVE    "U012"              TO  SPA-SAKIPG
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "U012"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-U012-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       430-MAEGMN-SEC             SECTION.
      *
           .
       430-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       440-ATOGMN-SEC             SECTION.
      *
           .
       440-ATOGMN-EXT.
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
      *    確認画面
           IF      SPA-U1IDCD      NOT =   SPACE
               PERFORM 520-U1ID1-SET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
      *D   MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "U01    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           IF     (FLG-GMN-INIT        =   1    )
               MOVE    ZERO                TO  WRK-U01-SI-ROW
               MOVE    ZERO                TO  WRK-U01-SZ-ROW
           ELSE
               MOVE    ZERO                TO  WRK-U01-SI-ROW
               MOVE    ZERO                TO  WRK-U01-SZ-ROW
               IF      SPA-GMN-MAX (1)     >   ZERO
                   MOVE    U01-SI-ROW          TO  WRK-U01-SI-ROW
               END-IF
               IF      SPA-GMN-MAX (2)     >   ZERO
                   MOVE    U01-SZ-ROW          TO  WRK-U01-SZ-ROW
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  U01
           INITIALIZE                      U01
      *    コラムリストの位置指定
           MOVE    WRK-U01-SI-ROW      TO  U01-SI-ROW 
           MOVE    WRK-U01-SZ-ROW      TO  U01-SZ-ROW
      *
           MOVE    SPA-GMN-DATE        TO  U01-SRYYMD.
           IF      SPA-GMN-SELNUM(1)   NOT =   ZERO
               MOVE    SPA-GMN-SELNUM(1)   TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9            TO  U01-SI-SELNUM
           END-IF
           IF      SPA-GMN-SELNUM(2)   NOT =   ZERO
               MOVE    SPA-GMN-SELNUM(2)   TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9            TO  U01-SZ-SELNUM
           END-IF
      *
           MOVE    ZERO                 TO  U01-SI-COUNT
           MOVE    ZERO                 TO  U01-SZ-COUNT
      *    受付一覧
           MOVE    1                   TO  IDX
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY             >   LINE-MAX  )   OR
                              (SPA-GMN-NO (IDX IDY) =   ZERO)
               MOVE    SPA-GMN-NO    (IDX IDY)  TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9                 TO  U01-SI-NO  (IDY)
      *
               MOVE    SPA-GMN-PTNUM (IDX IDY)  TO  U01-SI-PTNUM(IDY)
               MOVE    SPA-GMN-NAME  (IDX IDY)  TO  U01-SI-NAME (IDY)
               MOVE    SPA-GMN-NENREI(IDX IDY)  TO  U01-SI-NENREI(IDY)
               MOVE    SPA-GMN-NICKNAME (IDX IDY)
                                           TO  U01-SI-NICKNAME(IDY)
               MOVE    SPA-GMN-CONDITION (IDX IDY)
                                           TO  U01-SI-CONDITION (IDY)
      *
               MOVE    SPA-GMN-SRYKAMEI (IDX IDY)
                                           TO  U01-SI-SRYKAMEI(IDY)
               MOVE    SPA-GMN-SRYNAIYOMEI  (IDX IDY)
                                           TO  U01-SI-SRYNAIYOMEI(IDY)
               MOVE    SPA-GMN-DRCDMEI (IDX IDY)
                                           TO  U01-SI-DRCD (IDY)
               MOVE    SPA-GMN-HKNCOMBIMEI (IDX IDY)
                                           TO  U01-SI-HKNCOMBI (IDY)
               MOVE    SPA-GMN-UKETIMEH (IDX IDY)
                                           TO  U01-SI-UKETIME (IDY)
               MOVE    SPA-GMN-YYKTIMEH(IDX IDY)
                                           TO  U01-SI-YYKTIME (IDY)
      *        送受信フラグ
               MOVE    SPA-GMN-SRFLG(IDX IDY)
                                           TO  U01-SI-SRFLG (IDY)
      *H28.12
               IF      SPA-GMN-HENFLG (IDX IDY) =   "1"
      *        変更済み
                   MOVE    "*"           TO  U01-SI-UKETIME(IDY)
                                                    (8:1)
               END-IF
      *
               MOVE    IDY                 TO  U01-SI-COUNT
      *
               IF      SPA-GMN-NO(IDX IDY) =   SPA-GMN-SELNUM(1)
      *****    IF      IDY             =   SPA-NAI-SELNO (1)
                   MOVE    "T"             TO  U01-SI-SELECT (IDY)
      *             MOVE    IDY             TO  U01-SI-ROW
      *             MOVE    2               TO  U01-SI-ROWATTR
               ELSE
                   MOVE    "F"             TO  U01-SI-SELECT (IDY)
               END-IF
           END-PERFORM
      *
      *    会計済一覧
           MOVE    2                   TO  IDX
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY             >   LINE-MAX  )   OR
                              (SPA-GMN-NO (IDX IDY) =   ZERO) 
               MOVE    SPA-GMN-NO    (IDX IDY)  TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9                 TO  U01-SZ-NO  (IDY)
      *
               MOVE    SPA-GMN-PTNUM (IDX IDY)  TO  U01-SZ-PTNUM(IDY)
               MOVE    SPA-GMN-NAME  (IDX IDY)  TO  U01-SZ-NAME (IDY)
               MOVE    SPA-GMN-NENREI(IDX IDY)  TO  U01-SZ-NENREI(IDY)
               MOVE    SPA-GMN-NICKNAME (IDX IDY)
                                           TO  U01-SZ-NICKNAME(IDY)
      *
               MOVE    SPA-GMN-CONDITION (IDX IDY)
                                           TO  U01-SZ-CONDITION (IDY)
      *
               MOVE    SPA-GMN-SRYKAMEI (IDX IDY)
                                           TO  U01-SZ-SRYKAMEI (IDY)
               MOVE    SPA-GMN-SRYNAIYOMEI (IDX IDY)
                                           TO  U01-SZ-SRYNAIYOMEI(IDY)
               MOVE    SPA-GMN-DRCDMEI(IDX IDY)
                                           TO  U01-SZ-DRCD    (IDY)
               MOVE    SPA-GMN-HKNCOMBIMEI (IDX IDY)
                                           TO  U01-SZ-HKNCOMBI (IDY)
               MOVE    SPA-GMN-UKETIMEH(IDX IDY)
                                           TO  U01-SZ-UKETIME (IDY)
               MOVE    SPA-GMN-YYKTIMEH(IDX IDY)
                                           TO  U01-SZ-YYKTIME (IDY)
      *        送受信フラグ
               MOVE    SPA-GMN-SRFLG(IDX IDY)
                                           TO  U01-SZ-SRFLG (IDY)
              MOVE    IDY                  TO  U01-SZ-COUNT
      *
      ******   IF      SPA-GMN-NO (IDX IDY)    =   SPA-GMN-SELNUM(2)
               IF      IDY             =   SPA-NAI-SELNO (2)
                   MOVE    "T"             TO  U01-SZ-SELECT (IDY)
               ELSE
                   MOVE    "F"             TO  U01-SZ-SELECT (IDY)
               END-IF
           END-PERFORM
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
           EVALUATE    SPA-GMN-CUR
               WHEN    01
                   MOVE    "SELNUM1"       TO  MCP-WIDGET
               WHEN    03
                   MOVE    "SELNUM2"       TO  MCP-WIDGET
               WHEN    02
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
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号が有りません"  
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "選択中です。受付一覧の更新はできません"
                                       TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "新患は選択できません"
                                       TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "選択できません"
                                       TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "受付データがありません。印刷できません"
                                       TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPACE       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  U1ERR
           MOVE    SPA-ERRCD            TO  U1ERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  U1ERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "U01"                TO  SPA-MOTOPG
           MOVE    "U1ERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "U1ERR"              TO  MCP-WINDOW
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
      *    確認メッセージ表示理
      *****************************************************************
       520-U1ID1-SET-SEC              SECTION.
      *
           EVALUATE    SPA-U1IDCD
               WHEN    "0001"
                   MOVE    "一覧表を印刷します。よろしいですか？"
                                       TO  WRK-U1IDMSG
               WHEN    OTHER
                   MOVE    SPACE       TO  WRK-U1IDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  U1ID1
           INITIALIZE                       U1ID1
           MOVE    SPA-U1IDCD           TO  U1ID1-ID1CODE
           MOVE    WRK-U1IDMSG          TO  U1ID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "U01"                TO  SPA-MOTOPG
           MOVE    "U1ID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "U1ID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-U1ID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    受付検索処理
      *****************************************************************
       5001-U010-HEN-SEC        SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
      *
           MOVE    ZERO                TO  SPA-GMN-MAX(1)
                                           SPA-GMN-MAX(2)
           MOVE    ZERO                TO  SPA-GMN-PAGE
           MOVE    ZERO                TO  SPA-NAI-UPDOWN
      *
           MOVE   ZERO                 TO  IDX
           MOVE   ZERO                 TO  IDX2
           MOVE   ZERO                 TO  IDX3
           INITIALIZE                 WORK-SORT-TBL
           INITIALIZE                 SPA-GMN-TBL
      *
      *    受付マスタよりドクターコード設定
           INITIALIZE                  UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
      *
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-UKETUKE-READ-SEC
           ELSE
               INITIALIZE                  UKETUKE-REC
               MOVE    1               TO  FLG-UKETUKE
           END-IF
           PERFORM
               UNTIL   FLG-UKETUKE        =   1
      *        受付ＩＤ
               IF      UKE-UKEID          =   ZERO
                   CONTINUE
               ELSE
                   MOVE    ZERO                TO  IDX4
                   IF      UKE-KAIKEITIME      =   ZERO
                       MOVE    1                   TO  IDX1
                       ADD     1                   TO  IDX2
                       MOVE    IDX2                TO  IDX4
                   ELSE
                       MOVE    2                   TO  IDX1
                       ADD     1                   TO  IDX3
                       MOVE    IDX3                TO  IDX4
                   END-IF
                   IF      IDX4            <=  LINE-MAX
                       PERFORM 50012-UKETUKE-SET-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-UKETUKE-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    ＳＰＡ領域へＳＯＲＴ編集
           INITIALIZE                  SPA-GMN-TBL
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               PERFORM  5002-SORT-SEC
           END-PERFORM
      *
      *    スパ内部コード変換
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2    >   SPA-GMN-MAX(IDX)
                   PERFORM 5003-SPA-HEN-SEC
               END-PERFORM
           END-PERFORM
           .
      *
       5001-U010-HEN-EXT.
           EXIT.
      *****************************************************************
      *    受付状況並べ替え
      *****************************************************************
       50012-UKETUKE-SET-SEC           SECTION.
      *
           MOVE    UKE-UKEID           TO  WORK-SORT-UKEID (IDX1 IDX4)
           MOVE    UKE-PTID            TO  WORK-SORT-PTID  (IDX1 IDX4)
           MOVE    UKE-NAME            TO  WORK-SORT-NAME  (IDX1 IDX4)
           MOVE    UKE-SRYKA           TO  WORK-SORT-SRYKA  (IDX1 IDX4)
           MOVE    UKE-SRYNAIYO        TO  WORK-SORT-SRYNAIYO
                                                           (IDX1 IDX4)
           MOVE    UKE-DRCD            TO  WORK-SORT-DRCD  (IDX1 IDX4)
      *
           IF      UKE-HKNCOMBI        =   ZERO
               MOVE    SPACE           TO  WORK-SORT-HKNCOMBI(IDX1 IDX4)
           ELSE
               MOVE    UKE-HKNCOMBI    TO  WORK-SORT-HKNCOMBI(IDX1 IDX4)
           END-IF
           MOVE    UKE-UKETIME         TO  WORK-SORT-UKETIME(IDX1 IDX4)
      *    受付分
           IF      IDX1                =   1
               MOVE    UKE-YYKTIME     TO  WORK-SORT-YYKTIME
                                                           (IDX1 IDX4)
               IF      UKE-HYOJIJUN    =   ZERO
                   MOVE    ALL "9"         TO  WORK-SORT-KEY(IDX1 IDX4)
                                                                  (1:4)
               ELSE
                   MOVE    UKE-HYOJIJUN    TO  WORK-SORT-KEY(IDX1 IDX4)
                                                                  (1:4)
               END-IF
               MOVE    UKE-UKETIME         TO  WORK-SORT-KEY(IDX1 IDX4)
                                                                  (5:6)
           ELSE
               MOVE    UKE-KAIKEITIME      TO  WORK-SORT-YYKTIME 
                                                            (IDX1 IDX4)
               MOVE    UKE-KAIKEITIME      TO  WORK-SORT-KEY(IDX1 IDX4)
                                                                  (5:6)
           END-IF
      *    送受信フラグ
           MOVE    UKE-SRFLG           TO  WORK-SORT-SRFLG (IDX1 IDX4)
      *H28.12
      *    変更フラグ
           IF      UKE-KAKREN (1:1)    =   "1"
               MOVE    "1"             TO  WORK-SORT-HENFLG(IDX1 IDX4)
           END-IF
      *
           .
      *
       50012-UKETUKE-SET-EXT.
           EXIT.
      *****************************************************************
      *    スパ内部コード変換
      *****************************************************************
       5003-SPA-HEN-SEC           SECTION.
      *
           MOVE    IDX2               TO  SPA-GMN-NO (IDX IDX2)
      *    患者マスタ読み出し
           IF      SPA-GMN-PTID(IDX IDX2) NOT  =  ZERO
               INITIALIZE                      PTINF-REC
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    SPA-GMN-PTID(IDX IDX2)  TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
                   MOVE    PTINF-NAME          TO  SPA-GMN-NAME
                                                             (IDX IDX2)
                   MOVE    PTINF-NICKNAME      TO  SPA-GMN-NICKNAME
                                                             (IDX IDX2)
      *            年齢
                   PERFORM 50032-SPA-NENREI-SEC
      *            状態１
                   PERFORM 50030-SPA-CONDITION-SEC
               END-IF
      *
      *        患者番号取得
               INITIALIZE                      ORCSPTIDAREA
               MOVE    SPA-HOSPNUM          TO  SPTID-HOSPNUM
               MOVE    SPA-GMN-PTID(IDX IDX2)
                                           TO  SPTID-PTID
               CALL    "ORCSPTID"          USING   ORCSPTIDAREA
      *----(01.01.01) LINE ADD START ----------------------------------
                                                   SPA-AREA
      *----(01.01.01) LINE ADD END   ----------------------------------
               IF      SPTID-RC            =   ZERO
                   MOVE    SPTID-PTNUM         TO  SPA-GMN-PTNUM
                                                             (IDX IDX2)
               END-IF
           END-IF
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-GMN-SRYKA(IDX IDX2)
                                       TO  SYS-1005-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC    TO  SYS-1005-REC 
               MOVE    SYS-1005-SRYKANAME1 TO  SPA-GMN-SRYKAMEI
                                              (IDX IDX2)
           END-IF
      *
      *    職員情報をＳＰＡにセット
           MOVE    SPACE               TO  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-GMN-DRCD (IDX IDX2)
                                       TO  SYS-1010-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               MOVE    SYS-1010-NAME       TO  SPA-GMN-DRCDMEI 
                                                 (IDX IDX2)
           END-IF
      *
      *    診療内容をＳＰＡにセット
           MOVE    SPACE               TO  SYS-1012-REC
           MOVE    "1012"              TO  SYS-1012-KANRICD
           MOVE    SPA-GMN-SRYNAIYO (IDX IDX2)
                                       TO  SYS-1012-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1012-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1012-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1012-HOSPNUM
           MOVE    SYS-1012-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-1012-REC
               MOVE    SYS-1012-SRYNAIYODSP    TO  SPA-GMN-SRYNAIYOMEI
                                                          (IDX IDX2)
           END-IF
      *    保険組合せ設定
           IF      SPA-GMN-HKNCOMBI(IDX IDX2)  =   SPACE  OR  ZERO
               MOVE    SPACE           TO  SPA-GMN-HKNCOMBIMEI(IDX IDX2)
           ELSE
               PERFORM 50031-HKNCOMBI-HEN-SEC
               MOVE    WRK-HKNCOMBI    TO  SPA-GMN-HKNCOMBIMEI(IDX IDX2)
           END-IF
           . 
      *
       5003-SPA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    状態１編集処理
      *****************************************************************
       50030-SPA-CONDITION-SEC              SECTION.
     *
           MOVE    SPACE               TO  SYS-1018-REC
           MOVE    "1018"              TO  SYS-1018-KANRICD
           MOVE    PTINF-CONDITION-1   TO  SYS-1018-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1018-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1018-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1018-HOSPNUM
           MOVE    SYS-1018-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1018-REC
               IF      PTINF-CONDITION-1   NOT =   ZERO
                   MOVE    SYS-1018-CONDITION-WORD
                                           TO  SPA-GMN-CONDITION 
                                                 (IDX IDX2)
               END-IF
           END-IF
      *
           . 
      *
       50030-SPA-CONDITION-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ設定処理
      *****************************************************************
       50031-HKNCOMBI-HEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBI
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-GMN-PTID(IDX IDX2)
                                       TO  COMB-PTID
           MOVE    SPA-GMN-HKNCOMBI(IDX IDX2)
                                       TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-HKNCOMBI-READ-SEC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      COMB-DELKBN     NOT =   SPACE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
           IF      FLG-HKNCOMBI        =   ZERO
               PERFORM 500311-HKNCOMBIMEI-HEN-SEC
           END-IF
      *
           .
       50031-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       500311-HKNCOMBIMEI-HEN-SEC      SECTION.
      *
      *
           STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBI
           END-STRING
      *    主保険がない時
           IF      COMB-SYU-TANSEIDONAME   =   SPACE
               MOVE    SPACE               TO  WRK-HKNCOMBI
               STRING  COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBI
               END-STRING
           END-IF
      *
      *    労災の時、コメントの追加
           IF      COMB-HKNNUM         =   "971"  OR  "973"
      *        患者保険情報検索
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
               MOVE    SPA-PTID            TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
               MOVE    PTRSIINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 993-PTRSIINF-NEXT-SEC
               ELSE
                   INITIALIZE                      PTRSIINF-REC
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
               MOVE    SPACE               TO  WRK-HKNKBN-MEI
               EVALUATE    PTRSI-HKNKBN
                   WHEN    "1"
                       MOVE    "短"        TO  WRK-HKNKBN-MEI
                   WHEN    "2"
                       MOVE    "傷"        TO  WRK-HKNKBN-MEI
                   WHEN    "3"
                       MOVE    "ア"        TO  WRK-HKNKBN-MEI
                   WHEN    "5"
                       MOVE    "公"        TO  WRK-HKNKBN-MEI
               END-EVALUATE
               IF      PTRSI-COMMENT       =   SPACE
                   MOVE    SPACE               TO  WRK-HKNCOMBI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           INTO              WRK-HKNCOMBI
                   END-STRING
               ELSE
                   MOVE    SPACE               TO  WRK-HKNCOMBI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           "（"                    DELIMITED  BY  SIZE
                           PTRSI-COMMENT(1:24)     DELIMITED  BY  SPACE
                           "）"                    DELIMITED  BY  SIZE
                           INTO              WRK-HKNCOMBI
                   END-STRING
               END-IF
           END-IF
      *
           .
       500311-HKNCOMBIMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    年齢計算処理
      *****************************************************************
       50032-SPA-NENREI-SEC             SECTION.
      *
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    PTINF-PTID          TO  AGECHK-PTID
           MOVE    SPA-SYSYMD          TO  AGECHK-SRYYMD
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           MOVE    "2"                 TO  AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
      *    画面表示用
           MOVE    SPACE               TO  WRK-NENREI-G
           IF      AGECHK-NENREI2-YY   =   ZERO
               MOVE    AGECHK-NENREI2-MM   TO  WRK-NENREI-ZN
               MOVE    "ヶ月"              TO  WRK-NENREI-MEIN
           ELSE
               MOVE    AGECHK-NENREI2-YY   TO  WRK-NENREI-Z
               MOVE    "才"                TO  WRK-NENREI-MEI
           END-IF
           MOVE    WRK-NENREI-G        TO  SPA-GMN-NENREI(IDX IDX2)
      *
           .
       50032-SPA-NENREI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付状況並べ替え
      *****************************************************************
       5002-SORT-SEC              SECTION.
      *
           MOVE    ZERO                   TO  IDX2  
      *    受付時間順にＳＯＲＴ
           PERFORM VARYING   IDX2      FROM    1   BY  1
                   UNTIL     IDX2          >   LINE-MAX
               IF  IDX  =  1
                   MOVE    HIGH-VALUE      TO  WRK-UKETIME
               ELSE
                   MOVE    LOW-VALUE       TO  WRK-UKETIME
               END-IF
               MOVE    ZERO            TO  WRK-IDX
               PERFORM VARYING   IDX3      FROM    1   BY  1
                       UNTIL     IDX3      >  LINE-MAX
                   IF      IDX             =  1
                       IF     (WORK-SORT-UKETIME (IDX IDX3)
                                                   NOT =   SPACE)  AND
                              (WORK-SORT-KEY     (IDX IDX3)
                                                       <   WRK-UKETIME)
                           MOVE    IDX3            TO  WRK-IDX
                           MOVE    WORK-SORT-KEY(IDX IDX3)
                                                   TO  WRK-UKETIME
                       END-IF
                   ELSE
                       IF     (WORK-SORT-UKETIME(IDX IDX3)
                                                   NOT =   SPACE)  AND
                              (WORK-SORT-KEY    (IDX IDX3)
                                                       >   WRK-UKETIME)
                           MOVE    IDX3            TO  WRK-IDX
                           MOVE    WORK-SORT-KEY(IDX IDX3)
                                                   TO  WRK-UKETIME
                       END-IF
                   END-IF
              END-PERFORM
      *
              IF      WRK-IDX          >   ZERO
                   MOVE    WRK-IDX         TO  IDX3
                   PERFORM 50021-SPA-HEN-SEC
                   MOVE    SPACE           TO  WORK-SORT-TBLREC
                                                        (IDX IDX3)
              END-IF
           END-PERFORM
           .
      *
       5002-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ編集処理
      *****************************************************************
       50021-SPA-HEN-SEC              SECTION.
      *
           ADD     1                   TO  SPA-GMN-MAX(IDX)
           MOVE    WORK-SORT-UKEID (IDX IDX3)
                                       TO  SPA-GMN-UKEID(IDX IDX2)
           MOVE    WORK-SORT-PTID(IDX IDX3)
                                       TO  SPA-GMN-PTID(IDX IDX2)
           MOVE    WORK-SORT-NAME (IDX IDX3)
                                       TO  SPA-GMN-NAME(IDX IDX2)
           MOVE    WORK-SORT-SRYKA(IDX IDX3)
                                       TO  SPA-GMN-SRYKA(IDX IDX2)
           MOVE    WORK-SORT-SRYNAIYO(IDX IDX3)
                                       TO  SPA-GMN-SRYNAIYO(IDX IDX2)
           MOVE    WORK-SORT-DRCD   (IDX IDX3)
                                       TO  SPA-GMN-DRCD   (IDX IDX2)
           MOVE    WORK-SORT-HKNCOMBI (IDX IDX3)
                                       TO  SPA-GMN-HKNCOMBI (IDX IDX2)
           MOVE    WORK-SORT-UKETIME(IDX IDX3)
                                       TO  SPA-GMN-UKETIME(IDX IDX2)
           MOVE    WORK-SORT-YYKTIME(IDX IDX3)
                                       TO  SPA-GMN-YYKTIME(IDX IDX2)
           MOVE    WORK-SORT-UKETIME(IDX IDX3)(1:2)
                                       TO  SPA-GMN-UKETIMEH(IDX IDX2)
                                                                (2:2)
           MOVE    ":"                 TO  SPA-GMN-UKETIMEH(IDX IDX2)
                                                                (4:1)
           MOVE    WORK-SORT-UKETIME(IDX IDX3)(3:2)
                                       TO  SPA-GMN-UKETIMEH(IDX IDX2)
                                                                (5:2)
           IF      WORK-SORT-YYKTIME(IDX IDX3)   =   ZERO
               CONTINUE
           ELSE
               MOVE    WORK-SORT-YYKTIME(IDX IDX3)(1:2)
                                       TO  SPA-GMN-YYKTIMEH(IDX IDX2)
                                                                (2:2)
               MOVE    ":"             TO  SPA-GMN-YYKTIMEH(IDX IDX2)
                                                                (4:1)
               MOVE    WORK-SORT-YYKTIME(IDX IDX3)(3:2)
                                       TO  SPA-GMN-YYKTIMEH(IDX IDX2)
                                                                (5:2)
           END-IF
      *    送受信フラグ
           EVALUATE    WORK-SORT-SRFLG (IDX IDX3)
               WHEN    "00"
                   MOVE    "送済"          TO  SPA-GMN-SRFLG (IDX IDX2)
               WHEN    "01"
                   MOVE    "閉塞"          TO  SPA-GMN-SRFLG (IDX IDX2)
               WHEN    "02"
                   MOVE    "送誤"          TO  SPA-GMN-SRFLG (IDX IDX2)
               WHEN    "03"
                   MOVE    "会待"          TO  SPA-GMN-SRFLG (IDX IDX2)
           END-EVALUATE
      *H28.12
      *    変更フラグ
           IF      WORK-SORT-HENFLG(IDX IDX3)  =   "1"
               MOVE    "1"             TO  SPA-GMN-HENFLG(IDX IDX2)
           END-IF
           .
      *
       50021-SPA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付ファイルｓｏｒｔｋｅｙセット
      *****************************************************************
       503-JYUSIN-RWT-SEC              SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    ZERO                TO  IDY
           PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL     IDX       >   SPA-GMN-MAX(1)
               INITIALIZE                  UKETUKE-REC
               MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
               MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
               MOVE    SPA-GMN-UKEID(1 IDX)
                                           TO  UKE-UKEID
      *
               MOVE    UKETUKE-REC         TO  MCPDATA-REC
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_uketuke"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 900-UKETUKE-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-UKETUKE
               END-IF
      *
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               IF      FLG-UKETUKE         =   ZERO
                   IF      UKE-KAIKEITIME      =   ZERO
                       ADD     1                   TO  IDY
                       MOVE    IDY                 TO  UKE-HYOJIJUN
      *H28.12
      *                変更フラグ
                       IF  SPA-GMN-HENFLG(1 IDX)   =   "1"
                          MOVE    "1"            TO  UKE-KAKREN
                       END-IF
      *
                       MOVE    SMCNDATE-YMD        TO  UKE-UPYMD
                       MOVE    SMCNDATE-HMS        TO  UKE-UPHMS
      *
                       MOVE    UKETUKE-REC         TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "tbl_uketuke"       TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                   END-IF
               END-IF
           END-PERFORM
           .
       503-JYUSIN-RWT-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付マスタ読み込み処理
      *****************************************************************
       900-UKETUKE-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  UKETUKE-REC
               MOVE    ZERO            TO  FLG-UKETUKE
           ELSE
               INITIALIZE                  UKETUKE-REC
               MOVE    1               TO  FLG-UKETUKE
           END-IF
      *
           .
       900-UKETUKE-READ-EXT.
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
               MOVE    "tbl_ptinf"        TO  MCP-TABLE
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
           MOVE    "tbl_ptinf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
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
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       900-HKNCOMBI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           .
       993-PTRSIINF-NEXT-EXT.
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
      *     *
