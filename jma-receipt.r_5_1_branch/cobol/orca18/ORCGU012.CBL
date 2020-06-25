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
       PROGRAM-ID.                 ORCGU012.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 受付登録
      *  コンポーネント名  : 受付選択一覧（Ｕ０１２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/06/XX    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-多々納  15/06/16  診療科表示件数対応
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
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    画面共通ＳＰＡ
           COPY    "U01COMMON-SPA".
      *
       01  SPA-U012.
           03  SPA-GMN-AREA.
               05  SPA-GMN-PAGE                PIC 9(02).
               05  SPA-GMN-NEXT                PIC 9(01).
               05  SPA-GMN-CUR                 PIC 9(02).
      *
               05  SPA-GMN-MIDASI              PIC X(10).
               05  SPA-NAI-SYORI               PIC 9(01).
               05  SPA-NAI-SYORI-2             PIC 9(01).
      *
      *        処理日
               05  SPA-U012-SYSYMD              PIC X(08).
               05  SPA-U012-DATE                PIC X(30).
      *        選択
           03  SPA-GMN-HEN-AREA.
      *        診療科
               05  SPA-GMN-SRYKA-LIST-G.
                   07  SPA-GMN-SRYKA-LIST      OCCURS  100.
                       09  SPA-GMN-SRYKAL          PIC X(02).
                       09  SPA-GMN-SRYKAF1L        PIC X(01).
                       09  SPA-GMN-SRYKANAMEL      PIC X(16).
                   07  SPA-SRYKA-MAX           PIC 9(04).
                   07  SPA-NAI-SRYKA-LIST-G.
                       09  SPA-NAI-SRYKACHK        PIC X(01)
                                                   OCCURS  100.
               05  SPA-GMN-SRYKA-G.
                   07  SPA-GMN-SRYKA              PIC X(02).
                   07  SPA-GMN-SRYKA1             PIC X(01).
                   07  SPA-GMN-SRYKANAME          PIC X(16).
      *        ドクター
               05  SPA-GMN-DRCD-LIST-G.
                   07  SPA-GMN-DRCD-LIST          OCCURS  100.
                       09  SPA-GMN-DRCDL              PIC X(04).
                       09  SPA-GMN-DRCDF1L            PIC X(01).
                       09  SPA-GMN-DRNAMEL            PIC X(24).
                   07  SPA-DRCD-MAX               PIC 9(04).
                   07  SPA-NAI-DRCD-LIST-G.
                       09  SPA-NAI-DRCHK              PIC X(01)
                                                      OCCURS  100.
               05  SPA-GMN-DRCD-G.
                   07  SPA-GMN-DRCD               PIC X(04).
                   07  SPA-GMN-DRCD1              PIC X(01).
                   07  SPA-GMN-DRNAME             PIC X(24).
      *        診療内容
               05  SPA-GMN-SRYNAIYO-LIST-G.
                   07  SPA-GMN-SRYNAIYO-LIST      OCCURS  100.
                       09  SPA-GMN-SRYNAIYOCDL        PIC X(02).
                       09  SPA-GMN-SRYNAIYO1L         PIC X(01).
                       09  SPA-GMN-SRYNAIYODSPL       PIC X(16).
                   07  SPA-SRYNAI-MAX             PIC 9(04).
               05  SPA-GMN-SRYNAIYOCD-G.
                   07  SPA-GMN-SRYNAIYOCD         PIC X(02).
                   07  SPA-GMN-SRYNAIYO1          PIC X(01).
                   07  SPA-GMN-SRYNAIYODSP        PIC X(16).
      *
               05  SPA-GMN-TBL.
                   07  SPA-GMN-TBLREC    OCCURS  400.
                       09  SPA-GMN-NO              PIC 9(04).
                       09  SPA-GMN-PTNUM           PIC X(20).
                       09  SPA-GMN-PTID            PIC 9(10).
                       09  SPA-GMN-NAME            PIC X(50).
                       09  SPA-GMN-NENREI          PIC X(06).
                       09  SPA-GMN-NICKNAME        PIC X(50).
                       09  SPA-GMN-CONDITION       PIC X(06).
                       09  SPA-GMN-SRYKAMEI        PIC X(06).
                       09  SPA-GMN-SRYNAIYOMEI     PIC X(08).
                       09  SPA-GMN-DRCDMEI         PIC X(24).
                       09  SPA-GMN-HKNCOMBIMEI     PIC X(50).
                       09  SPA-GMN-UKETIMEH        PIC X(06).
                       09  SPA-GMN-YYKTIMEH        PIC X(06).
                       09  SPA-GMN-KAIKIMEH        PIC X(06).
                       09  SPA-GMN-SRFLG           PIC X(04).
      *
                       09  SPA-NAI-UKEID           PIC 9(05).
                       09  SPA-NAI-UKETIME         PIC X(06).
                       09  SPA-NAI-HKNCOMBI        PIC X(04).
                       09  SPA-NAI-DRCD            PIC X(05).
                       09  SPA-NAI-SRYNAIYO        PIC X(02).
                       09  SPA-NAI-SRYKA           PIC X(02).
                       09  SPA-NAI-KAIKEITIME      PIC 9(04).
                       09  SPA-NAI-YYKTIME         PIC 9(04).
                       09  SPA-NAI-HYOJIJUN        PIC X(04).
      *                ドクター追加
                       09  SPA-NAI-DRCDADD         PIC X(01).
      *                送受信フラグ
                       09  SPA-NAI-SRFLG           PIC X(01).
      *H28.12
      *                変更フラグ
                       09  SPA-GMN-HENFLG          PIC X(01).
                   07  SPA-GMN-MAX             PIC 9(03).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-UKETUKE             PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-PTRSIINF            PIC 9(01).
      *
           03  FLG-NO                  PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-CHK-END             PIC 9(01).
      *    ドクター追加あり
           03  FLG-DRCD-ADD            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-STR             PIC 9(04).
           03  IDX-D               PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYKA           PIC X(02).
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
               05  WRK-WDD         PIC 9(02).
      *
           03  WRK-ZZZ9            PIC ZZZ9.
      *
           03  WRK-HKNCOMBI-MEI    PIC X(100).
           03  WRK-HKNKBN-MEI      PIC X(02).
      *    確認メッセージ
           03  WRK-U1IDMSG         PIC X(80).
      *
           03  WRK-PATHNAME        PIC X(60).
      *
           03  WRK-KEY.
               05  WRK-HYOJIJUN         PIC X(04).
               05  WRK-UKETIME         PIC X(06).
           03  WRK-IDX                 PIC 9(04).
      *    ドクター一覧ワーク
           03  WRK-DRCD-HENSYU-AREA.
               05  WRK-GMN-DRCD-LIST-G.
                   07  WRK-GMN-DRCD-LIST          OCCURS  100.
                       09  WRK-GMN-DRCDL              PIC X(04).
                       09  WRK-GMN-DRCDF1L            PIC X(01).
                       09  WRK-GMN-DRNAMEL            PIC X(24).
                   07  WRK-DRCD-MAX               PIC 9(04).
                   07  WRK-NAI-DRCD-LIST-G.
                       09  WRK-NAI-DRCHK              PIC X(01)
                                                      OCCURS  100.
      *
      *    ＳＯＲＴ用
       01  WRK-SORT-AREA.
               03  WRK-GMN-TBL.
                   07  WRK-GMN-TBLREC    OCCURS  400.
                       09  WRK-GMN-NO              PIC 9(04).
                       09  WRK-GMN-PTNUM           PIC X(20).
                       09  WRK-GMN-PTID            PIC 9(10).
                       09  WRK-GMN-NAME            PIC X(50).
                       09  WRK-GMN-NENREI          PIC X(06).
                       09  WRK-GMN-NICKNAME        PIC X(50).
                       09  WRK-GMN-CONDITION       PIC X(06).
                       09  WRK-GMN-SRYKAMEI        PIC X(06).
                       09  WRK-GMN-SRYNAIYOMEI     PIC X(08).
                       09  WRK-GMN-DRCDMEI         PIC X(24).
                       09  WRK-GMN-HKNCOMBIMEI     PIC X(50).
                       09  WRK-GMN-UKETIMEH        PIC X(06).
                       09  WRK-GMN-YYKTIMEH        PIC X(06).
                       09  WRK-GMN-KAIKIMEH        PIC X(06).
                       09  WRK-GMN-SRFLG           PIC X(04).
      *
                       09  WRK-NAI-UKEID           PIC 9(05).
                       09  WRK-NAI-UKETIME         PIC X(06).
                       09  WRK-NAI-HKNCOMBI        PIC X(04).
                       09  WRK-NAI-DRCD            PIC X(05).
                       09  WRK-NAI-SRYNAIYO        PIC X(02).
                       09  WRK-NAI-SRYKA           PIC X(02).
                       09  WRK-NAI-KAIKEITIME      PIC 9(04).
                       09  WRK-NAI-YYKTIME         PIC 9(04).
                       09  WRK-NAI-HYOJIJUN        PIC X(04).
      *                ドクター追加
                       09  WRK-NAI-DRCDADD         PIC X(01).
      *                送受信フラグ
                       09  WRK-NAI-SRFLG           PIC X(01).
      *H28.12
      *                変更フラグ
                       09  WRK-NAI-HENFLG          PIC X(01).
      *    一覧表最大行数
       01  LINE-MAX                    PIC 9(03)   VALUE   400.
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
      *    ドクターセット領域
           COPY    "CPORCSDRSET.INC".
      *
           COPY    "MCPDATA.INC".
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-WORK        TO  SPA-U01KYOUTU
           MOVE    SPA-FREE        TO  SPA-U012
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
           MOVE    SPA-U012        TO  SPA-FREE
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
           INITIALIZE                  IDX-AREA
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
           MOVE   SPACE                TO  MCP-PUTTYPE
           MOVE   "U012"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
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
               WHEN    OTHER
                   PERFORM 310-INIT-SYORI-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-INIT-SYORI-SEC              SECTION.
      *
      *    画面遷移後、ＬＮＫをセット
           IF      LNK-KYOUTU      NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               IF      SPA-MOTOPG          =   "U02"
                   INITIALIZE                  SPA-U01KYOUTU
                   MOVE    SPA-MOTOPG          TO  SPA-U012-MOTOPG
               ELSE
                   MOVE    "U01"               TO  SPA-U012-MOTOPG
               END-IF
           END-IF
      *
           INITIALIZE                  SPA-U012
      *
           MOVE    SPA-SYSYMD          TO  SPA-U012-SYSYMD
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-NAI-SYORI
           PERFORM 310-SPASET-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       310-INIT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-HEN-AREA
      *
      *    曜日変換
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    SPA-U012-SYSYMD     TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING  STS-AREA-DAY
                                              LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE  LNK-DAY2-EDTYMD3      TO  WRK-HENNYMD
           END-IF
           INITIALIZE                  LNK-DAY3-AREA
           MOVE    "31"                TO  LNK-DAY3-IRAI
           MOVE    SPA-U012-SYSYMD     TO  LNK-DAY3-YMD
           CALL    "ORCSDAY"           USING  STS-AREA-DAY
                                              LNK-DAY3-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE  LNK-DAY3-YOBIMEI  TO  WRK-HENNYOBI
           END-IF
           MOVE    WRK-HENYMDN         TO  SPA-U012-DATE
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-U012-SYSYMD     TO  SYS-1005-STYUKYMD
           MOVE    SPA-U012-SYSYMD     TO  SYS-1005-EDYUKYMD
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
      *    システム管理検索
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    1                   TO  IDX
           MOVE    "00 全科"           TO  SPA-GMN-SRYKA-LIST (IDX)
           PERFORM 
                   UNTIL       (IDX            >=  100  )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC           TO  SYS-1005-REC
               ADD     1                     TO  IDX
               MOVE    SYS-1005-KBNCD (1:2)  TO  SPA-GMN-SRYKAL (IDX)
               MOVE    SYS-1005-SRYKANAME1   TO  SPA-GMN-SRYKANAMEL(IDX)
               MOVE    IDX                   TO  SPA-SRYKA-MAX
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPA-GMN-SRYKA-LIST(01)
                                       TO  SPA-GMN-SRYKA-G
      *
      *    職員情報をＳＰＡにセット
           PERFORM 3101-DRCD-SPASET-SEC
      *    初期表示は指定なし
           MOVE    SPA-GMN-DRCD-LIST(01)   TO  SPA-GMN-DRCD-G
      *
      *    診療内容をＳＰＡにセット
           MOVE    SPACE               TO  SYS-1012-REC
           MOVE    "1012"              TO  SYS-1012-KANRICD
           MOVE    SPA-U012-SYSYMD     TO  SYS-1012-STYUKYMD
           MOVE    SPA-U012-SYSYMD     TO  SYS-1012-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1012-HOSPNUM
           MOVE    SYS-1012-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1012-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    1                   TO  IDX
           MOVE    1                   TO  SPA-SRYNAI-MAX
           MOVE    "00 全体"           TO  SPA-GMN-SRYNAIYO-LIST
                                                   (SPA-SRYNAI-MAX)
           PERFORM
                   UNTIL      (IDX             >=  100 )   OR
                              (FLG-SYSKANRI    =   1   )
               MOVE    MCPDATA-REC       TO  SYS-1012-REC
               MOVE    SYS-1012-KBNCD    TO  SPA-GMN-SRYNAIYOCD
               MOVE    SPACE             TO  SPA-GMN-SRYNAIYO1
               MOVE    SYS-1012-SRYNAIYODSP
                                         TO  SPA-GMN-SRYNAIYODSP
      *
               ADD     1                 TO  IDX
               MOVE    SPA-GMN-SRYNAIYOCD-G
                                         TO  SPA-GMN-SRYNAIYO-LIST(IDX)
               MOVE    IDX               TO  SPA-SRYNAI-MAX
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPA-GMN-SRYNAIYO-LIST(01)
                                       TO  SPA-GMN-SRYNAIYOCD-G
      *
           MOVE    1                   TO  SPA-GMN-PAGE
      *    一覧表示
           PERFORM 5001-UKETUKE-HEN-SEC
           .
       310-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    職員情報編集処理
      *****************************************************************
       3101-DRCD-SPASET-SEC               SECTION.
      *
           INITIALIZE                      SPA-GMN-DRCD-LIST-G
      *    ドクター １件目を指定無しに
           MOVE    1                   TO  SPA-DRCD-MAX
           MOVE    "9999"              TO  SPA-GMN-DRCDL (SPA-DRCD-MAX)
           MOVE    "指定なし"          TO  SPA-GMN-DRNAMEL(SPA-DRCD-MAX)
      *    全ドクター
           INITIALIZE                  ORCSDRSETAREA
           MOVE    SPA-U012-SYSYMD     TO  ORCSDR-SYSYMD
           MOVE    "1"                 TO  ORCSDR-KBN
           MOVE    SPACE               TO  ORCSDR-SRYKA
           CALL    "ORCSDRSET"         USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   ORCSDR-DRCD-MAX )
               ADD     1               TO  SPA-DRCD-MAX
               MOVE    ORCSDR-DRCD-TBL (IDX-D)
                                       TO  SPA-GMN-DRCD-LIST
                                                   (SPA-DRCD-MAX)
           END-PERFORM
           .
       3101-DRCD-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    受付一覧編集処理
      *****************************************************************
       5001-UKETUKE-HEN-SEC               SECTION.
      *
      *    開始行
           COMPUTE IDX-STR             =   (SPA-GMN-PAGE  -  1 )
                                       *    LINE-MAX
                                       +    1
           MOVE    ZERO                TO  SPA-GMN-NEXT
           MOVE    ZERO                TO  IDX4
           MOVE    ZERO                TO  IDX
      *
           INITIALIZE                  SPA-GMN-TBL
           MOVE    ZERO                TO  FLG-DRCD-ADD
      *
           INITIALIZE                  UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-U012-SYSYMD     TO  UKE-UKEYMD
      *
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           IF      SPA-NAI-SYORI       =   1
      *        受付中
               MOVE    "key7"              TO  WRK-PATHNAME
           ELSE
      *        受診済み
               MOVE    "key8"              TO  WRK-PATHNAME
           END-IF
           MOVE    WRK-PATHNAME         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 900-UKETUKE-READ-SEC
           ELSE
               INITIALIZE                  UKETUKE-REC
               MOVE    1               TO  FLG-UKETUKE
           END-IF
           PERFORM
               UNTIL  (FLG-UKETUKE        =   1  )
               OR     (SPA-GMN-NEXT       =   1  )
               MOVE    ZERO               TO  FLG-NO
      *        診療科
               IF     (SPA-GMN-SRYKA   NOT =   "00" )
                  AND (SPA-GMN-SRYKA   NOT =   UKE-SRYKA )
                   MOVE    1                   TO  FLG-NO
               END-IF
      *        ドクター
               IF     (SPA-GMN-DRCD    NOT =   "9999" )
                  AND (SPA-GMN-DRCD    NOT =   UKE-DRCD(2:4))
                   MOVE    1                   TO  FLG-NO
               END-IF
      *        診療内容
               IF     (SPA-GMN-SRYNAIYOCD  NOT =   "00" )
                  AND (SPA-GMN-SRYNAIYOCD  NOT =   UKE-SRYNAIYO )
                   MOVE    1                   TO  FLG-NO
               END-IF
      *
               IF      FLG-NO              =   ZERO
                   ADD     1                   TO  IDX4
                   IF      IDX-STR             <=  IDX4
                       ADD     1               TO  IDX
                       IF      IDX             >   LINE-MAX
                           MOVE    1               TO  SPA-GMN-NEXT
                       ELSE
                           PERFORM 50012-UKETUKE-SET-SEC
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 900-UKETUKE-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    ドクター追加あり
           IF      FLG-DRCD-ADD        =   1
               PERFORM 50013-DRCD-HENSYU-SEC
           END-IF
      *
           IF      SPA-NAI-SYORI       =   1
      *        受付中は表示順によりＳＯＲＴする
               PERFORM 50021-UKETUKE-SROT-SEC
           END-IF
           .
       5001-UKETUKE-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ドクター追加処理
      *****************************************************************
       50013-DRCD-HENSYU-SEC               SECTION.
      *
      *    一覧表のドクターのみ表示とする
           MOVE    SPACE               TO  WRK-GMN-DRCD-LIST-G
           INITIALIZE                      WRK-GMN-DRCD-LIST-G
           MOVE    ZERO                TO  IDX3
           PERFORM VARYING   IDX2      FROM    1   BY  1
                   UNTIL    (IDX2      >   SPA-DRCD-MAX)
               IF     (SPA-NAI-DRCHK (IDX2)    =   "1" )
                  OR  (IDX2                    =   1   )
                   ADD     1               TO  IDX3
                   MOVE    SPA-GMN-DRCD-LIST(IDX2)
                                           TO  WRK-GMN-DRCD-LIST(IDX3)
                   MOVE    SPA-NAI-DRCHK (IDX2)
                                           TO  WRK-NAI-DRCHK    (IDX3)
                   MOVE    IDX3            TO  WRK-DRCD-MAX
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-GMN-MAX
               IF      SPA-NAI-DRCDADD (IDX)   =   "1"
                   MOVE    ZERO                TO  FLG-OK
                   PERFORM VARYING   IDX3      FROM    1   BY  1
                           UNTIL    (IDX3      >   WRK-DRCD-MAX)
                                 OR (FLG-OK    =   1    )
                       IF      WRK-GMN-DRCDL (IDX3)
                                               =   SPA-NAI-DRCD (IDX)
                                                                (2:4)
                           MOVE    1               TO  FLG-OK
                       END-IF
                   END-PERFORM
                   IF      FLG-OK          =   ZERO
                       ADD     1               TO  WRK-DRCD-MAX
                       IF      WRK-DRCD-MAX    >   100
                           MOVE    100             TO  WRK-DRCD-MAX
                       ELSE
                           MOVE    WRK-DRCD-MAX    TO  IDX3
                           MOVE    SPA-NAI-DRCD(IDX)(2:4)
                                           TO  WRK-GMN-DRCDL (IDX3)
                           MOVE    SPA-GMN-DRCDMEI(IDX)
                                           TO  WRK-GMN-DRNAMEL(IDX3)
                           MOVE    "1"     TO  WRK-NAI-DRCHK  (IDX3)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    WRK-GMN-DRCD-LIST-G TO  SPA-GMN-DRCD-LIST-G
           .
       50013-DRCD-HENSYU-EXT.
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
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 420-CLEAR-SEC
      *        受付切替
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 450-LIST-CHANGE-SEC
      *        前日
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 450-MAE-DAY-SEC
      *        後日
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 450-ATO-DAY-SEC
      *        前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 460-MAE-PAGE-SEC
      *        次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 470-ATO-PAGE-SEC
      *        診療科
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 480-SRYKA-SEL-SEC
      *        ドクター
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 490-DRCD-SEL-SEC
      *        診療内容
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 4100-SRYNAIYO-SEL-SEC
      *        印刷処理
      *        WHEN    "CLICKED"       ALSO    "B04"
      *            PERFORM 4010-PRINT-SEC
      *        受付一覧更新
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 4011-UKELIST-SEC
      *        受付一覧自動更新
               WHEN    "CLICKED"       ALSO    "pandatimer1"
                   PERFORM 4011-AUTOUKELIST-SEC
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
      *        診療科入力
               WHEN    "ACTIVATE"      ALSO    "SRYKA"
                   PERFORM 4101-SRYKA-SYORI-SEC
      *        ドクター入力
               WHEN    "ACTIVATE"      ALSO    "DRCD"
                   PERFORM 4102-DRCD-SYORI-SEC
      *        診療内容
               WHEN    "ACTIVATE"      ALSO    "SRYNAIYO"
                   PERFORM 4102-SRYNAIYO-SYORI-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      ***************************************************************
      *    受付表示きり変え処理
      ***************************************************************
       450-LIST-CHANGE-SEC               SECTION.
      *
           IF      SPA-NAI-SYORI       =   1
               MOVE    2               TO  SPA-NAI-SYORI
           ELSE
               MOVE    1               TO  SPA-NAI-SYORI
           END-IF
           INITIALIZE                  SPA-NAI-DRCD-LIST-G
           INITIALIZE                  SPA-NAI-SRYKA-LIST-G
           MOVE    SPA-GMN-SRYKA-LIST(01)  TO  SPA-GMN-SRYKA-G
           MOVE    SPA-GMN-DRCD-LIST (01)  TO  SPA-GMN-DRCD-G
           MOVE    SPA-GMN-SRYNAIYO-LIST (01)
                                           TO  SPA-GMN-SRYNAIYOCD-G
           MOVE    1                   TO  SPA-GMN-PAGE
      *    一覧表示
           PERFORM 5001-UKETUKE-HEN-SEC
           MOVE    1                   TO  SPA-GMN-CUR
           .
       450-LIST-CHANGE-EXT.
           EXIT.
      ***************************************************************
      *    前日処理
      ***************************************************************
       450-MAE-DAY-SEC               SECTION.
      *
      *    前日
           INITIALIZE                     STS-AREA-DAY
           INITIALIZE                     LNK-DAY6-AREA
           MOVE    "61"                   TO  LNK-DAY6-IRAI
           MOVE    SPA-U012-SYSYMD        TO  LNK-DAY6-KIJUN
           MOVE    "1"                    TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                     TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"              USING   STS-AREA-DAY
                                                  LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN        TO  SPA-U012-SYSYMD
      *
           PERFORM 310-SPASET-SEC
           MOVE    1                   TO  SPA-GMN-CUR
           .
       450-MAE-DAY-EXT.
           EXIT.
      ***************************************************************
      *    次日処理
      ***************************************************************
       450-ATO-DAY-SEC               SECTION.
      *
      *    次日
           INITIALIZE                     STS-AREA-DAY
           INITIALIZE                     LNK-DAY6-AREA
           MOVE    "61"                   TO  LNK-DAY6-IRAI
           MOVE    SPA-U012-SYSYMD        TO  LNK-DAY6-KIJUN
           MOVE    "1"                    TO  LNK-DAY6-ZOGENPTN
           MOVE    1                      TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"              USING   STS-AREA-DAY
                                                  LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN        TO  SPA-U012-SYSYMD
      *
           PERFORM 310-SPASET-SEC
           MOVE    1                   TO  SPA-GMN-CUR
           .
       450-ATO-DAY-EXT.
           EXIT.
      ***************************************************************
      *    診療科きり変え処理
      ***************************************************************
       480-SRYKA-SEL-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-CHK-END
           PERFORM VARYING   IDX2      FROM    1   BY  1
                   UNTIL    (IDX2      >   SPA-SRYKA-MAX)
                         OR (FLG-CHK-END   =   1    )
               IF      FLG-OK          =   1
                   IF      SPA-NAI-SRYKACHK (IDX2) =   "1"
                       MOVE    SPA-GMN-SRYKA-LIST(IDX2)
                                           TO  SPA-GMN-SRYKA-G
                       MOVE    1           TO  FLG-CHK-END
                   END-IF
                   IF      IDX2            =   SPA-SRYKA-MAX
                       MOVE    SPA-GMN-SRYKA-LIST(01)
                                           TO  SPA-GMN-SRYKA-G
                       MOVE    1           TO  FLG-CHK-END
                   END-IF
               END-IF
               IF      SPA-GMN-SRYKAL (IDX2)
                                           =   SPA-GMN-SRYKA
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           MOVE    1                   TO  SPA-GMN-PAGE
      *    一覧表示
           PERFORM 5001-UKETUKE-HEN-SEC
           MOVE    1                   TO  SPA-GMN-CUR
           .
       480-SRYKA-SEL-EXT.
           EXIT.
      ***************************************************************
      *    ドクター切替処理
      ***************************************************************
       490-DRCD-SEL-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-CHK-END
           PERFORM VARYING   IDX2      FROM    1   BY  1
                   UNTIL    (IDX2      >   SPA-DRCD-MAX)
                       OR   (FLG-CHK-END   =   1       )
               IF      FLG-OK          =   1
                   IF      SPA-NAI-DRCHK (IDX2)    =   "1"
                       MOVE    SPA-GMN-DRCD-LIST(IDX2)
                                           TO  SPA-GMN-DRCD-G
                       MOVE    1           TO  FLG-CHK-END
                   END-IF
               END-IF
               IF      SPA-GMN-DRCDL (IDX2)
                                           =   SPA-GMN-DRCD
                   MOVE    1               TO  FLG-OK
               END-IF
               IF      IDX2            =   SPA-DRCD-MAX
                 AND  (FLG-CHK-END     =   ZERO   )
                   MOVE    SPA-GMN-DRCD-LIST(01)
                                           TO  SPA-GMN-DRCD-G
                   MOVE    1           TO  FLG-CHK-END
                END-IF
           END-PERFORM
           MOVE    1                   TO  SPA-GMN-PAGE
      *    一覧表示
           PERFORM 5001-UKETUKE-HEN-SEC
           MOVE    2                   TO  SPA-GMN-CUR
           .
       490-DRCD-SEL-EXT.
           EXIT.
      ***************************************************************
      *    診療内容切替処理
      ***************************************************************
       4100-SRYNAIYO-SEL-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING   IDX2      FROM    1   BY  1
                   UNTIL    (IDX2      >   SPA-SRYNAI-MAX)
                         OR (FLG-OK    =   1    )
               IF      SPA-GMN-SRYNAIYOCDL (IDX2)
                                           =   SPA-GMN-SRYNAIYOCD
                   IF      IDX2            =   SPA-SRYNAI-MAX
                       MOVE    SPA-GMN-SRYNAIYO-LIST(01)
                                           TO  SPA-GMN-SRYNAIYOCD-G
                   ELSE
                       MOVE    SPA-GMN-SRYNAIYO-LIST(IDX2 + 1)
                                           TO  SPA-GMN-SRYNAIYOCD-G
                   END-IF
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           MOVE    1                   TO  SPA-GMN-PAGE
      *    一覧表示
           PERFORM 5001-UKETUKE-HEN-SEC
           MOVE    3                   TO  SPA-GMN-CUR
           .
       4100-SRYNAIYO-SEL-EXT.
           EXIT.
      ***************************************************************
      *    初期表示へ処理
      ***************************************************************
       420-CLEAR-SEC               SECTION.
      *
           PERFORM 310-INIT-SYORI-SEC
      *
           .
       420-CLEAR-SEL-EXT.
           EXIT.
      ***************************************************************
      *    診療科入力処理
      ***************************************************************
       4101-SRYKA-SYORI-SEC               SECTION.
      *
           MOVE    SPA-GMN-SRYKA       TO  WRK-SRYKA
      *
           MOVE    U012-SRYKA          TO  SPA-GMN-SRYKA-G
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING   IDX2      FROM    1   BY  1
                   UNTIL    (IDX2      >   SPA-SRYKA-MAX)
                         OR (FLG-OK    =   1    )
               IF      SPA-GMN-SRYKAL (IDX2)
                                           =   SPA-GMN-SRYKA
                   MOVE    SPA-GMN-SRYKA-LIST(IDX2)
                                           TO  SPA-GMN-SRYKA-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               MOVE    1                   TO  SPA-GMN-PAGE
      *        一覧表示
               PERFORM 5001-UKETUKE-HEN-SEC
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
           .
       4101-SRYKA-SYORI-EXT.
           EXIT.
      ***************************************************************
      *    ドクター力処理
      ***************************************************************
       4102-DRCD-SYORI-SEC               SECTION.
      *
           MOVE    U012-DRCD           TO  SPA-GMN-DRCD-G
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING   IDX2      FROM    1   BY  1
                   UNTIL    (IDX2      >   SPA-DRCD-MAX)
                         OR (FLG-OK    =   1    )
               IF      SPA-GMN-DRCDL (IDX2)
                                           =   SPA-GMN-DRCD
                   MOVE    SPA-GMN-DRCD-LIST(IDX2)
                                           TO  SPA-GMN-DRCD-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN-PAGE
      *        一覧表示
               PERFORM 5001-UKETUKE-HEN-SEC
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
           .
       4102-DRCD-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療内容　　入力処理
      *****************************************************************
       4102-SRYNAIYO-SYORI-SEC     SECTION.
      *
           MOVE    U012-SRYNAIYO       TO  SPA-GMN-SRYNAIYOCD-G
      *
      *    診療内容
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   SPA-SRYNAI-MAX )
                           OR (FLG-OK    =   1    )
               IF     SPA-GMN-SRYNAIYOCD   =   SPA-GMN-SRYNAIYOCDL
                                                              (IDX2)
                   MOVE    SPA-GMN-SRYNAIYO-LIST (IDX2) 
                                               TO  SPA-GMN-SRYNAIYOCD-G
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN-PAGE
      *        一覧表示
               PERFORM 5001-UKETUKE-HEN-SEC
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF
          .
       4102-SRYNAIYO-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       4010-PRINT-SEC              SECTION.
      *
           .
       4010-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       800-PRINT-SYORI-SEC              SECTION.
      *
      *    CALL    "ORCHCM16"          USING
      *                                SPA-AREA
      *
           .
       800-PRINT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    受付一覧更新処理
      *****************************************************************
       4011-UKELIST-SEC              SECTION.
      *
      *    受付情報再表示
           MOVE    1                   TO  SPA-GMN-PAGE
           PERFORM 5001-UKETUKE-HEN-SEC
      *
           .
       4011-UKELIST-EXT.
           EXIT.
      *****************************************************************
      *    受付一覧自動更新処理
      *****************************************************************
       4011-AUTOUKELIST-SEC              SECTION.
      *
           IF     (SPA-SYSYMD          =   SPA-U012-SYSYMD )
      *    受付情報再表示
               MOVE    1                   TO  SPA-GMN-PAGE
               PERFORM 5001-UKETUKE-HEN-SEC
           END-IF
      *
           .
       4011-AUTOUKELIST-EXT.
           EXIT.
      *****************************************************************
      *    受付編集処理
      *****************************************************************
       50012-UKETUKE-SET-SEC           SECTION.
      *
           MOVE    IDX                 TO  SPA-GMN-MAX
      *
           MOVE    IDX4                TO  SPA-GMN-NO   (IDX)
           MOVE    UKE-UKEID           TO  SPA-NAI-UKEID (IDX)
           MOVE    UKE-PTID            TO  SPA-GMN-PTID  (IDX)
           MOVE    UKE-NAME            TO  SPA-GMN-NAME  (IDX)
           MOVE    UKE-SRYKA           TO  SPA-NAI-SRYKA (IDX)
           MOVE    UKE-SRYNAIYO        TO  SPA-NAI-SRYNAIYO (IDX)
           MOVE    UKE-DRCD            TO  SPA-NAI-DRCD  (IDX)
           IF      UKE-HKNCOMBI        =   ZERO
               MOVE    SPACE           TO  SPA-NAI-HKNCOMBI(IDX)
           ELSE
               MOVE    UKE-HKNCOMBI    TO  SPA-NAI-HKNCOMBI(IDX)
           END-IF
           MOVE    UKE-UKETIME         TO  SPA-NAI-UKETIME (IDX)
      *    時間
           MOVE    UKE-YYKTIME         TO  SPA-NAI-YYKTIME (IDX)
           MOVE    UKE-UKETIME         TO  SPA-NAI-UKETIME (IDX)
           MOVE    UKE-KAIKEITIME      TO  SPA-NAI-KAIKEITIME (IDX)
           IF      UKE-HYOJIJUN        =   ZERO
                MOVE    "9999"              TO  SPA-NAI-HYOJIJUN (IDX)
           ELSE
                MOVE    UKE-HYOJIJUN        TO  SPA-NAI-HYOJIJUN (IDX)
           END-IF
      *    送受信フラグ
           MOVE    UKE-SRFLG           TO  SPA-NAI-SRFLG (IDX)
      *H28.12
      *    変更フラグ
           IF      UKE-KAKREN (1:1)    =   "1"
               MOVE    "1"                 TO  SPA-GMN-HENFLG(IDX)
           END-IF
      *    時間編集
           MOVE    SPA-NAI-UKETIME(IDX)(1:2)
                                       TO  SPA-GMN-UKETIMEH(IDX)(2:2)
           MOVE    ":"                 TO  SPA-GMN-UKETIMEH(IDX)(4:1)
           MOVE    SPA-NAI-UKETIME(IDX)(3:2)
                                       TO  SPA-GMN-UKETIMEH(IDX)(5:2)
           IF      SPA-NAI-YYKTIME(IDX)    =   ZERO
                                           OR  SPACE
               CONTINUE
           ELSE
               MOVE    SPA-NAI-YYKTIME(IDX)(1:2)
                                           TO  SPA-GMN-YYKTIMEH(IDX)
                                                                (2:2)
               MOVE    ":"                 TO  SPA-GMN-YYKTIMEH(IDX)
                                                                (4:1)
               MOVE    SPA-NAI-YYKTIME(IDX)(3:2)
                                           TO  SPA-GMN-YYKTIMEH(IDX)
                                                                (5:2)
           END-IF
           IF      SPA-NAI-KAIKEITIME(IDX) =   ZERO
                                           OR  SPACE
               CONTINUE
           ELSE
               MOVE    SPA-NAI-KAIKEITIME(IDX)(1:2)
                                           TO  SPA-GMN-KAIKIMEH(IDX)
                                                                (2:2)
               MOVE    ":"                 TO  SPA-GMN-KAIKIMEH(IDX)
                                                                (4:1)
               MOVE    SPA-NAI-KAIKEITIME(IDX)(3:2)
                                           TO  SPA-GMN-KAIKIMEH(IDX)
                                                                (5:2)
           END-IF
      *    送受信フラグ
           EVALUATE    SPA-NAI-SRFLG (IDX)
               WHEN    "00"
                   MOVE    "送済"          TO  SPA-GMN-SRFLG (IDX)
               WHEN    "01"
                   MOVE    "閉塞"          TO  SPA-GMN-SRFLG (IDX)
               WHEN    "02"
                   MOVE    "送誤"          TO  SPA-GMN-SRFLG (IDX)
               WHEN    "03"
                   MOVE    "会待"          TO  SPA-GMN-SRFLG (IDX)
           END-EVALUATE
      *
      *    患者マスタ読み出し
           IF      SPA-GMN-PTID(IDX)   NOT =   ZERO
               INITIALIZE                      PTINF-REC
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    SPA-GMN-PTID(IDX)   TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
                   MOVE    PTINF-NAME          TO  SPA-GMN-NAME (IDX)
                   MOVE    PTINF-NICKNAME      TO  SPA-GMN-NICKNAME
                                                                (IDX)
      *            年齢
                   PERFORM 50032-SPA-NENREI-SEC
      *            状態１
                   PERFORM 50030-SPA-CONDITION-SEC
               END-IF
      *        患者番号取得
               INITIALIZE                      ORCSPTIDAREA
               MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
               MOVE    SPA-GMN-PTID(IDX)   TO  SPTID-PTID
               CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                   SPA-AREA
               IF      SPTID-RC            =   ZERO
                   MOVE    SPTID-PTNUM         TO  SPA-GMN-PTNUM (IDX)
               END-IF
           END-IF
      *
      *    診療科
           PERFORM VARYING   IDX2      FROM    1   BY  1
                   UNTIL     IDX2      >   SPA-SRYKA-MAX
               IF      SPA-GMN-SRYKAL (IDX2)
                                           =   SPA-NAI-SRYKA (IDX)
                   MOVE    SPA-GMN-SRYKANAMEL(IDX2)
                                           TO  SPA-GMN-SRYKAMEI(IDX)
      *            受付有り
                   MOVE    "1"             TO  SPA-NAI-SRYKACHK (IDX2)
                   MOVE    SPA-SRYKA-MAX   TO  IDX2
               END-IF
           END-PERFORM
      *
      *    ドクター名
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING   IDX2      FROM    1   BY  1
                   UNTIL     IDX2      >   SPA-DRCD-MAX
               IF      SPA-GMN-DRCDL (IDX2)
                                           =   SPA-NAI-DRCD (IDX)
                                                        (2:4)
                   MOVE    SPA-GMN-DRNAMEL (IDX2)
                                           TO  SPA-GMN-DRCDMEI(IDX)
      *            受付有り
                   MOVE    "1"             TO  SPA-NAI-DRCHK (IDX2)
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-DRCD-MAX    TO  IDX2
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               INITIALIZE                  ORCSDRSETAREA
               MOVE    SPA-SYSYMD          TO  ORCSDR-SYSYMD
               MOVE    "2"                 TO  ORCSDR-KBN
               MOVE    SPA-NAI-DRCD (IDX)(2:4)
                                           TO  ORCSDR-IN-DRCD
               CALL    "ORCSDRSET"         USING
                                           SPA-AREA
                                           ORCSDRSETAREA
               MOVE    ORCSDR-OUT-DRCDMEI  TO  SPA-GMN-DRCDMEI(IDX)
               MOVE    "1"                 TO  SPA-NAI-DRCDADD (IDX)
               MOVE    1                   TO  FLG-DRCD-ADD
           END-IF
      *
      *    診療内容
           PERFORM VARYING   IDX2      FROM    1   BY  1
                   UNTIL     IDX2      >   SPA-SRYNAI-MAX
               IF      SPA-GMN-SRYNAIYOCDL (IDX2)
                                           =   SPA-NAI-SRYNAIYO (IDX)
                   MOVE    SPA-GMN-SRYNAIYODSPL (IDX2)
                                               TO  SPA-GMN-SRYNAIYOMEI
                                                               (IDX)
                   MOVE    SPA-SRYNAI-MAX      TO  IDX2
               END-IF
           END-PERFORM
      *    保険組合せ設定
           IF      SPA-NAI-HKNCOMBI(IDX)   =   SPACE  OR  ZERO
               MOVE    SPACE               TO  SPA-GMN-HKNCOMBIMEI(IDX)
           ELSE
               PERFORM 50031-HKNCOMBI-HEN-SEC
               MOVE    WRK-HKNCOMBI-MEI    TO  SPA-GMN-HKNCOMBIMEI(IDX)
           END-IF
           . 
       50012-UKETUKE-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    状態１編集処理
      *****************************************************************
       50030-SPA-CONDITION-SEC              SECTION.
     *
           MOVE    SPACE               TO  SYS-1018-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1018-HOSPNUM
           MOVE    "1018"              TO  SYS-1018-KANRICD
           MOVE    PTINF-CONDITION-1   TO  SYS-1018-KBNCD
           MOVE    SPA-U012-SYSYMD     TO  SYS-1018-STYUKYMD
           MOVE    SPA-U012-SYSYMD     TO  SYS-1018-EDYUKYMD
      *
           MOVE    SYS-1018-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1018-REC
               IF      PTINF-CONDITION-1   NOT =   ZERO
                   MOVE    SYS-1018-CONDITION-WORD
                                           TO  SPA-GMN-CONDITION 
                                                           (IDX)
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
           MOVE    SPACE               TO  WRK-HKNCOMBI-MEI
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-GMN-PTID   (IDX)
                                       TO  COMB-PTID
           MOVE    SPA-NAI-HKNCOMBI(IDX)
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
           STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBI-MEI
           END-STRING
      *    主保険がない時
           IF      COMB-SYU-TANSEIDONAME   =   SPACE
               MOVE    SPACE               TO  WRK-HKNCOMBI-MEI
               STRING  COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBI-MEI
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
                   MOVE    SPACE               TO  WRK-HKNCOMBI-MEI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           INTO              WRK-HKNCOMBI-MEI
                   END-STRING
               ELSE
                   MOVE    SPACE               TO  WRK-HKNCOMBI-MEI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           "（"                    DELIMITED  BY  SIZE
                           PTRSI-COMMENT(1:24)     DELIMITED  BY  SPACE
                           "）"                    DELIMITED  BY  SIZE
                           INTO              WRK-HKNCOMBI-MEI
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
           MOVE    SPA-U012-SYSYMD     TO  AGECHK-SRYYMD
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
           MOVE    WRK-NENREI-G        TO  SPA-GMN-NENREI(IDX)
      *
           .
       50032-SPA-NENREI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付ＳＯＲＴ処理
      *****************************************************************
       50021-UKETUKE-SROT-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-SORT-AREA
           MOVE    SPA-GMN-TBL         TO  WRK-GMN-TBL
           INITIALIZE                      SPA-GMN-TBL
      *    受付時間順にＳＯＲＴ
           PERFORM VARYING   IDX2      FROM    1   BY  1
                   UNTIL     IDX2          >   LINE-MAX
               MOVE    HIGH-VALUE      TO  WRK-UKETIME
               MOVE    HIGH-VALUE      TO  WRK-HYOJIJUN
               MOVE    ZERO            TO  WRK-IDX
               PERFORM VARYING   IDX3      FROM    1   BY  1
                       UNTIL     IDX3      >  LINE-MAX
                   IF      WRK-NAI-HYOJIJUN (IDX3)
                                           NOT =   SPACE
                       IF     (WRK-NAI-HYOJIJUN (IDX3)
                                               <   WRK-HYOJIJUN )
                       OR    ((WRK-NAI-HYOJIJUN (IDX3)
                                               =   WRK-HYOJIJUN )
                         AND  (WRK-NAI-UKETIME (IDX3)
                                               <   WRK-UKETIME  ))
                           MOVE    IDX3            TO  WRK-IDX
                           MOVE    WRK-NAI-HYOJIJUN (IDX3)
                                                   TO  WRK-HYOJIJUN
                           MOVE    WRK-NAI-UKETIME (IDX3)
                                                   TO  WRK-UKETIME
                       END-IF
                   END-IF
              END-PERFORM
      *
              IF      WRK-IDX          >   ZERO
                   MOVE    WRK-IDX         TO  IDX3
                   MOVE    WRK-GMN-TBLREC (IDX3)
                                           TO  SPA-GMN-TBLREC (IDX2)
                   MOVE    IDX2            TO  SPA-GMN-NO     (IDX2)
                   MOVE    SPACE           TO  WRK-GMN-TBLREC (IDX3)
                   ADD     1               TO  SPA-GMN-MAX
              END-IF
           END-PERFORM
           .
       50021-UKETUKE-SROT-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      **** MOVE    "U01"               TO  SPA-SAKIPG
           MOVE    "U012"              TO  SPA-MOTOPG
           MOVE    SPA-U012-MOTOPG     TO  SPA-SAKIPG
      *
           IF      SPA-SAKIPG      NOT =   "U01"
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           END-IF
      *
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
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       460-MAE-PAGE-SEC             SECTION.
      *
           IF      SPA-GMN-PAGE        >   1
               COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE   -   1
               PERFORM 5001-UKETUKE-HEN-SEC
           END-IF
           .
       460-MAE-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       470-ATO-PAGE-SEC             SECTION.
      *
           IF     SPA-GMN-NEXT         =   1
               ADD     1               TO  SPA-GMN-PAGE
               PERFORM 5001-UKETUKE-HEN-SEC
           END-IF
           .
       470-ATO-PAGE-EXT.
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
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "U012"              TO  MCP-WINDOW
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
           MOVE    SPACE               TO  U012
           INITIALIZE                      U012
      *
           MOVE    SPA-U012-DATE       TO  U012-SRYYMD
      *    処理内容
           EVALUATE    SPA-NAI-SYORI
               WHEN    1
                   MOVE    "＜受付＞"          TO  U012-MIDASI
               WHEN    2
                   MOVE    "＜受診済＞"        TO  U012-MIDASI
           END-EVALUATE
      *    診療科
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               MOVE    SPA-GMN-SRYKA-LIST (IDX)
                                       TO  U012-SRYKA-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-SRYKA-MAX       TO  U012-SRYKA-COUNT
           MOVE    SPA-GMN-SRYKA-G     TO  U012-SRYKA
      *    ドクター
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-DRCD-MAX
               MOVE    SPA-GMN-DRCD-LIST (IDX)
                                       TO  U012-DRCD-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-DRCD-MAX        TO  U012-DRCD-COUNT
           MOVE    SPA-GMN-DRCD-G      TO  U012-DRCD
      *    診療内容
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYNAI-MAX
               MOVE    SPA-GMN-SRYNAIYO-LIST (IDX)
                                       TO  U012-SRYNAIYO-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-SRYNAI-MAX      TO  U012-SRYNAIYO-COUNT
           MOVE    SPA-GMN-SRYNAIYOCD-G     TO  U012-SRYNAIYO
      *
      *    受付一覧
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-GMN-MAX
               MOVE    SPA-GMN-NO    (IDX) TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9            TO  U012-LNO  (IDX)
      *
               MOVE    SPA-GMN-PTNUM (IDX) TO  U012-LPTNUM(IDX)
               MOVE    SPA-GMN-NAME  (IDX) TO  U012-LNAME (IDX)
               MOVE    SPA-GMN-NENREI(IDX) TO  U012-LNENREI(IDX)
               MOVE    SPA-GMN-NICKNAME (IDX)
                                           TO  U012-LNICKNAME(IDX)
               MOVE    SPA-GMN-CONDITION (IDX)
                                           TO  U012-LCONDITION (IDX)
      *
               MOVE    SPA-GMN-SRYKAMEI (IDX)
                                           TO  U012-LSRYKAMEI(IDX)
               MOVE    SPA-GMN-SRYNAIYOMEI (IDX)
                                           TO  U012-LSRYNAIYOMEI(IDX)
               MOVE    SPA-GMN-DRCDMEI (IDX)
                                           TO  U012-LDRCD (IDX)
               MOVE    SPA-GMN-HKNCOMBIMEI (IDX)
                                           TO  U012-LHKNCOMBI (IDX)
               MOVE    SPA-GMN-UKETIMEH (IDX)
                                           TO  U012-LUKETIME (IDX)
               MOVE    SPA-GMN-YYKTIMEH(IDX)
                                           TO  U012-LYYKTIME (IDX)
               MOVE    SPA-GMN-KAIKIMEH(IDX)
                                           TO  U012-LKAITIME (IDX)
      *H28.12
               IF      SPA-GMN-HENFLG (IDX)    =   "1"
      *        変更済み
                   MOVE    "*"             TO  U012-LUKETIME (IDX)
                                                    (8:1)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-MAX         TO  U012-LIST-COUNT
      *    次頁あり
           IF      SPA-GMN-NEXT        =   1
               IF      SPA-GMN-PAGE        =   1
                   MOVE    "次頁有り"          TO  U012-NEXT-VALUE
               ELSE
                   MOVE    "前頁・次頁有り"    TO  U012-NEXT-VALUE
               END-IF
               MOVE    "red"               TO  U012-NEXT-STYLE
           ELSE
               IF      SPA-GMN-PAGE        >   1
                   MOVE    "前頁有り"          TO  U012-NEXT-VALUE
                   MOVE    "red"               TO  U012-NEXT-STYLE
               ELSE
                   MOVE    SPACE               TO  U012-NEXT-VALUE
                   MOVE    "black"             TO  U012-NEXT-STYLE
               END-IF
           END-IF
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
                   MOVE    "SRYKA"         TO  MCP-WIDGET
               WHEN    02
                   MOVE    "DRCD"          TO  MCP-WIDGET
               WHEN    03
                   MOVE    "SRYNAIYO"      TO  MCP-WIDGET
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
                   MOVE    "診療科が有りません"  
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "ドクターが有りません。"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "診療内容が有りません。"
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
           MOVE    "U012"               TO  SPA-MOTOPG
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
           MOVE    "U012"               TO  SPA-MOTOPG
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
      *    システム管理読み込み処理
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
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
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
