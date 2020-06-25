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
       PROGRAM-ID.                 ORCGUDR.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 受付登録
      *  コンポーネント名  : 診療科別担当医登録（ＵＤＲ）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  15/10/XX    NACL−多々納  新規作成
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
       01  SPA-UDR.
           03  SPA-GMN-AREA.
               05  SPA-GMN-PAGE                PIC 9(02).
               05  SPA-GMN-NEXT                PIC 9(01).
               05  SPA-GMN-CUR                 PIC 9(02).
      *
               05  SPA-GMN-MIDASI              PIC X(50).
               05  SPA-GMN-MIDASI3             PIC X(100).
               05  SPA-NAI-SYORI               PIC 9(01).
               05  SPA-NAI-SYORI-2             PIC 9(01).
      *
               05  SPA-NAI-LISTKBN             PIC 9(01).
               05  SPA-NAI-LISTKBN2            PIC 9(01).
      *        ドクターリストから遷移
               05  SPA-NAI-MAE                 PIC 9(01).
               05  SPA-SEL-DRCD                PIC 9(05).
      *
           03  SPA-GMN-HEN-AREA.
      *        診療科
               05  SPA-GMN-SRYKA-LIST-G.
                   07  SPA-GMN-SRYKA-LIST      OCCURS  100.
                       09  SPA-GMN-SRYKAL          PIC X(02).
                       09  SPA-GMN-SRYKAF1L        PIC X(01).
                       09  SPA-GMN-SRYKANAMEL      PIC X(16).
                   07  SPA-SRYKA-MAX           PIC 9(04).
                   07  SPA-NAI-SRYKANAME2          PIC X(12)
                                               OCCURS   100.
                   07  SPA-NAI-SRYKA-LIST-G.
                       09  SPA-NAI-SRYKACHK        PIC X(01)
                                                   OCCURS  100.
               05  SPA-GMN-SRYKA-G.
                   07  SPA-GMN-SRYKA              PIC X(02).
                   07  SPA-GMN-SRYKA1             PIC X(01).
                   07  SPA-GMN-SRYKANAME          PIC X(16).
      *
               05  SPA-GMN-TBL.
                   07  SPA-GMN-TBLREC    OCCURS  200.
                       09  SPA-GMN-TNO              PIC 9(03).
                       09  SPA-GMN-THNUM            PIC 9(03).
                       09  SPA-GMN-TDRCD            PIC X(05).
                       09  SPA-GMN-TDRCD-NAME       PIC X(80).
                       09  SPA-GMN-TDRCD-KANA       PIC X(80).
                       09  SPA-GMN-TSRYKA-G.
                           11  SPA-GMN-TSRYKA      PIC X(20)
                                                   OCCURS   5.
                       09  SPA-GMN-THYOUJI         PIC X(10).
                       09  SPA-GMN-TSELECT         PIC X(01).
      *
                       09  SPA-NAI-TDBKBN          PIC X(01).
                       09  SPA-NAI-THYOUJIKBN      PIC X(01).
                       09  SPA-NAI-THIHYOJI        PIC X(01).
                   07  SPA-GMN-MAX             PIC 9(03).
      *        選択
               05  SPA-GMN-SELNUM          PIC 9(03).
               05  SPA-NAI-SELNUM          PIC 9(03).
      *
               05  SPA-KEI-FLG             PIC 9(01).
      *
       01  WRK-GMN-AREA.
      *
           03  WRK-GMN-HENSYU.
      *        診療科毎
               05  WRK-LSDOCTOR-TBL.
                   07  WRK-LSDOCTOR-TBLREC        OCCURS  400.
                       09  WRK-LSDOCTOR-TRENNUM     PIC 9(03).
                       09  WRK-LSDOCTOR-TDRCD       PIC X(05).
                       09  WRK-LSDOCTOR-TDRCD-NAME  PIC X(80).
                       09  WRK-LSDOCTOR-TDRCD-KANA  PIC X(80).
                       09  WRK-LSDOCTOR-TSRYKA-G.
                           11  WRK-LSDOCTOR-TSRYKA  PIC X(20)
                                                   OCCURS   5.
                       09  WRK-LSDOCTOR-THYOUJIKBN  PIC X(01).
                       09  WRK-LSDOCTOR-THIHYOJI    PIC X(01).
                       09  WRK-LSDOCTOR-THYOUJIMEI  PIC X(10).
                   07  WRK-LSDOCTOR-MAX             PIC 9(03).
      *        システム管理（当日有効）
               05  WRK-SYSKANRI-TBL.
                   07  WRK-SYSKANRI-TBLREC        OCCURS  999.
                       09  WRK-SYSKANRI-TRENNUM     PIC 9(03).
                       09  WRK-SYSKANRI-TDRCD       PIC X(05).
                       09  WRK-SYSKANRI-TDRCD-NAME  PIC X(80).
                       09  WRK-SYSKANRI-TDRCD-KANA  PIC X(80).
                       09  WRK-SYSKANRI-TSRYKA-G.
                           11  WRK-SYSKANRI-TSRYKA  PIC X(20)
                                                   OCCURS   5.
                       09  WRK-SYSKANRI-THYOUJIKBN  PIC X(01).
                       09  WRK-SYSKANRI-THIHYOJI    PIC X(01).
                       09  WRK-SYSKANRI-THYOUJIMEI  PIC X(10).
                   07  WRK-SYSKANRI-MAX             PIC 9(03).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-LSDOCTOR             PIC 9(01).
      *
           03  FLG-NO                  PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
           03  FLG-CHK2                PIC 9(01).
           03  FLG-CHK3                PIC 9(01).
           03  FLG-CHK-END             PIC 9(01).
      *
           03  FLG-SORT-END            PIC 9(01).
           03  FLG-SYS1011             PIC 9(01).
           03  FLG-OK2                 PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-STR             PIC 9(04).
           03  IDX-D               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDXS1               PIC 9(04).
           03  IDXS2               PIC 9(04).
           03  IDXW                PIC 9(04).
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
           03  WRK-CHK-SRYKA       PIC X(02).
           03  WRK-CHK-DRCD        PIC X(05).
      *
           03  WRK-DRCD            PIC X(05).
           03  WRK-DRCD-AREA.
               05  WRK-DRCD-NAME       PIC X(80).
               05  WRK-DRCD-KANANAME   PIC X(80).
               05  WRK-DRCD-HYOUJI     PIC X(02).
               05  WRK-DRCD-HYOUJIKBN  PIC X(01).
               05  WRK-DRCD-HIHYOJI    PIC X(01).
               05  WRK-DRCD-HYOJIMEI   PIC X(10).
               05  WRK-DR-SRYKA-G.
                   07  WRK-DR-SRYKA-OCC        OCCURS   5.
                       09  WRK-DR-SRYKA        PIC X(02).
                       09  WRK-DR-SRYKAYOBI    PIC X(01).
                       09  WRK-DR-SRYAKNAME2   PIC X(12).
      *
           03  WRK-HYOJIMEI1       PIC X(02).
           03  WRK-HYOJIMEI2       PIC X(06).
           03  WRK-MIDASI2         PIC X(20).
      *
           03  WRK-ZZ9             PIC ZZZ.
           03  WRK-ZZZ9            PIC ZZZ9.
      *
           03  WRK-SELNUM          PIC 9(03).
           03  WRK-RENNUM          PIC 9(03).
           03  WRK-HNUM            PIC 9(03).
      *
           03  WRK-HZN-HNUM1       PIC 9(03).
           03  WRK-HZN-HNUM2       PIC 9(03).
      *    SORT用
           03  WRK-SRT-DRCD        PIC X(05).
           03  WRK-SRT-KANA        PIC X(80).
      *
           03  WRK-UDR-ROW         PIC S9(09).
           03  FLG-GMN-INIT        PIC  9(01).
      *
           03  WRK-GMN-MAX         PIC 9(03).
           03  WRK-MAX-LINE        PIC 9(03).
      *
      *    確認メッセージ
           03  WRK-U1IDMSG             PIC X(80).
      *
           03  WRK-PATHNAME            PIC X(60).
           03  WRK-IDX                 PIC 9(04).
      *
      *    ＳＯＲＴ用
       01  WRK-HZN-AREA.
           03  WRK-HZN-TELREC          PIC X(300).
      *    ＳＯＲＴ用
       01  WRK-SORT-AREA.
           03  WRK-SORT-TBL.
               05  WRK-SORT-TBLREC    OCCURS  999.
                   09  WRK-SORT-TNO              PIC 9(03).
                   09  WRK-SORT-THNUM            PIC 9(03).
                   09  WRK-SORT-TDRCD            PIC X(05).
                   09  WRK-SORT-TDRCD-NAME       PIC X(80).
                   09  WRK-SORT-TKANANAME        PIC X(80).
                   09  WRK-SORT-TSRYKA-G.
                       11  WRK-SORT-TSRYKA      PIC X(20)
                                                   OCCURS   5.
                   09  WRK-SORT-THYOUJI         PIC X(10).
                   09  WRK-SORT-TSELECT         PIC X(01).
      *
                   09  WRK-SORT-TDBKBN          PIC X(01).
                   09  WRK-SORT-THYOUJIKBN      PIC X(01).
                   09  WRK-SORT-THIHYOJI        PIC X(01).
      *
      *    システム管理（当日有効）
           03  WRK-SORT2-TBL.
               07  WRK-SORT2-TBLREC        OCCURS  999.
                       09  WRK-SORT2-TRENNUM     PIC 9(03).
                       09  WRK-SORT2-TDRCD       PIC X(05).
                       09  WRK-SORT2-TDRCD-NAME  PIC X(80).
                       09  WRK-SORT2-TDRCD-KANA  PIC X(80).
                       09  WRK-SORT2-TSRYKA-G.
                           11  WRK-SORT2-TSRYKA  PIC X(20)
                                                   OCCURS   5.
                       09  WRK-SORT2-THYOUJIKBN  PIC X(01).
                       09  WRK-SORT2-THIHYOJI    PIC X(01).
                       09  WRK-SORT2-THYOUJIMEI  PIC X(10).
                   07  WRK-SORT2-MAX             PIC 9(03).
      *
      *    一覧表最大行数
       01  LINE-MAX                    PIC 9(03)   VALUE   200.
      *
      *    ドクターリスト件数
       01  DRLST-MAX                   PIC 9(03)   VALUE   99.
      *
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
      *    診療科別ドクター
       01  LIST-DOCTOR-REC.
           COPY    "CPLIST_DOCTOR.INC".
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    診療科情報
           COPY  "CPSK1005.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *    当日担当医
           COPY  "CPSK1011.INC".
      *
      *    職員情報
           COPY  "CPSK1010.INC"       REPLACING
                                 //SYS-1010-// BY //WRK-SYS-1010-//.
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
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ドクターセット領域
      *    COPY    "CPORCSDRSET.INC".
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
           MOVE    SPA-FREE        TO  SPA-UDR
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
           MOVE    SPA-UDR         TO  SPA-FREE
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
               IF      FLG-END             =   ZERO
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE   SPACE                TO  MCP-PUTTYPE
               MOVE   "UDR"                TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
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
                                           OR  "K02"
                   INITIALIZE                  SPA-U01KYOUTU
                   MOVE    SPA-MOTOPG          TO  SPA-U012-MOTOPG
      *            処理途中
                   IF      SPA-NAME        NOT =   SPACE
                       MOVE    SPA-SRYKA           TO  WRK-CHK-SRYKA
                   END-IF
               ELSE
                   MOVE    "U01"               TO  SPA-U012-MOTOPG
               END-IF
           END-IF
      *
           INITIALIZE                  SPA-UDR
      *
           PERFORM 310-SPASET-SEC
      *
           MOVE    1                   TO  SPA-GMN-CUR
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
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-NAI-SYORI
      *    受付中の遷移
           IF      WRK-CHK-SRYKA   NOT =   SPACE
               MOVE    1                   TO  SPA-NAI-MAE
           END-IF
      *
           INITIALIZE                  SPA-GMN-HEN-AREA
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
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
           MOVE    ZERO                TO  IDX
           MOVE    1                   TO  IDX-S
           PERFORM 
                   UNTIL       (IDX            >=  100  )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    SYSKANRI-REC          TO  SYS-1005-REC
               ADD     1                     TO  IDX
               MOVE    SYS-1005-KBNCD (1:2)  TO  SPA-GMN-SRYKAL (IDX)
               MOVE    SYS-1005-SRYKANAME1   TO  SPA-GMN-SRYKANAMEL(IDX)
               MOVE    SYS-1005-SRYKANAME2   TO  SPA-NAI-SRYKANAME2(IDX)
               MOVE    IDX                   TO  SPA-SRYKA-MAX
      *
               IF     (WRK-CHK-SRYKA   NOT =   SPACE)
                  AND (WRK-CHK-SRYKA       =   SPA-GMN-SRYKAL (IDX))
                   MOVE    IDX             TO  IDX-S
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPA-GMN-SRYKA-LIST(IDX-S)
                                       TO  SPA-GMN-SRYKA-G
      *
      *    医師編集
           PERFORM 3101-TBL-HENSYU-SEC
      *
      *    ドクターリストからの遷移は、医師選択
           IF      SPA-NAI-MAE         =   1
               MOVE    1                   TO  SPA-NAI-SYORI
           END-IF
      *
           .
       310-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    診療科別医師表示編集処理
      *****************************************************************
       3101-TBL-HENSYU-SEC               SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN-MIDASI3
           MOVE    ZERO                TO  SPA-NAI-SYORI-2
           MOVE    ZERO                TO  SPA-NAI-LISTKBN
           MOVE    ZERO                TO  SPA-NAI-LISTKBN2
      *
           INITIALIZE                  WRK-GMN-AREA
      ***  INITIALIZE                  SPA-NAI-AREA
      *    診療科別医師表示
           PERFORM 3101-LSDOCTOR-HEN-SEC
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           IF      WRK-LSDOCTOR-MAX     >   ZERO
      *        表示順を優先
               MOVE    2                   TO  SPA-NAI-SYORI
           ELSE
               MOVE    1                   TO  SPA-NAI-SYORI
           END-IF
      *    診療科別医師画面編集
           PERFORM 3102-GMNSPA-HEN-SEC
      *
      *    システム管理当日医師表示
           PERFORM 3101-SYSKANRI-HEN-SEC
      *
           IF      SPA-NAI-LISTKBN     =   ZERO
      *        カナ氏名順
               PERFORM 45012-KANA-SORT2-SEC
           END-IF
      *
      *    切替え
           MOVE    SPA-GMN-MAX         TO  IDX
           IF     (IDX                 <   LINE-MAX )
              AND (IDX                 >   ZERO     )
              AND (WRK-SYSKANRI-MAX    >   ZERO     )
               ADD     1               TO  IDX
               MOVE    ALL "-"         TO  SPA-GMN-TDRCD (IDX)
               MOVE    "--"            TO  SPA-GMN-THYOUJI(IDX)
               MOVE    IDX             TO  SPA-GMN-MAX
           END-IF
      *
      *    システム管理当日画面編集
           PERFORM 3103-GMNSPA-HEN02-SEC
      *
      *    当日担当医設定あり
           PERFORM 3103-SYS1011-CHK-SEC
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       3101-TBL-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    職員情報編集処理
      *****************************************************************
       3101-LSDOCTOR-HEN-SEC               SECTION.
      *
           INITIALIZE                  WRK-LSDOCTOR-TBL
           MOVE    ZERO                TO  IDX
      *
           INITIALIZE                  LIST-DOCTOR-REC
           MOVE    SPA-HOSPNUM         TO  LSDOCTOR-HOSPNUM
           MOVE    SPA-GMN-SRYKA       TO  LSDOCTOR-SRYKA
      *
           MOVE    LIST-DOCTOR-REC     TO  MCPDATA-REC
           MOVE    "tbl_list_doctor"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_list_doctor"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-LSDOCTOR-READ-SEC
           ELSE
               INITIALIZE                  LIST-DOCTOR-REC
               MOVE    1               TO  FLG-LSDOCTOR
           END-IF
           PERFORM
               UNTIL  (FLG-LSDOCTOR     =   1  )
      *
               ADD     1               TO  IDX
               IF      IDX             >   LINE-MAX
                   MOVE    1               TO  FLG-LSDOCTOR
               ELSE
                   PERFORM 31011-LSDOCTOR-SET-SEC
               END-IF
      *
               IF      FLG-LSDOCTOR         =   ZERO
                   MOVE    "tbl_list_doctor"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-LSDOCTOR-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_list_doctor"        TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       3101-LSDOCTOR-HEN-EXT.
           EXIT.
      *****************************************************************
      *    職員情報編集処理
      *****************************************************************
       31011-LSDOCTOR-SET-SEC          SECTION.
      *
           MOVE    LSDOCTOR-RENNUM      TO  WRK-LSDOCTOR-TRENNUM (IDX)
           MOVE    LSDOCTOR-DRCD        TO  WRK-LSDOCTOR-TDRCD (IDX)
           MOVE    LSDOCTOR-DRCD        TO  WRK-DRCD
           PERFORM 31011-DRCD-NAME-SEC
           MOVE    WRK-DRCD-NAME       TO  WRK-LSDOCTOR-TDRCD-NAME
                                                              (IDX)
           MOVE    WRK-DRCD-KANANAME   TO  WRK-LSDOCTOR-TDRCD-KANA
                                                              (IDX)
           MOVE    WRK-DRCD-HYOUJIKBN  TO  WRK-LSDOCTOR-THYOUJIKBN
                                                             (IDX)
           MOVE    WRK-DRCD-HIHYOJI    TO  WRK-LSDOCTOR-THIHYOJI
                                                             (IDX)
           MOVE    WRK-DRCD-HYOJIMEI   TO  WRK-LSDOCTOR-THYOUJIMEI
                                                             (IDX)
           PERFORM VARYING     IDXS    FROM    1   BY  1
                   UNTIL       IDXS    >   5
               MOVE    WRK-DR-SRYKA-OCC (IDXS)
                                       TO  WRK-LSDOCTOR-TSRYKA
                                                        (IDX IDXS)
           END-PERFORM
      *
           MOVE    IDX                 TO  WRK-LSDOCTOR-MAX
      *
           .
       31011-LSDOCTOR-SET-EXT.
           EXIT.
      *****************************************************************
      *    職員情報編集処理
      *****************************************************************
       31011-DRCD-NAME-SEC               SECTION.
      *
           MOVE    SPACE               TO  WRK-SYS-1010-REC
           INITIALIZE                  WRK-DRCD-AREA
      *
           MOVE    SPACE               TO  SYS-1010-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WRK-DRCD            TO  SYS-1010-KBNCD
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
      *    システム管理検索
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM UNTIL       FLG-SYSKANRI    =   1
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
               IF      WRK-SYS-1010-REC    =   SPACE
                   MOVE    SYS-1010-REC        TO  WRK-SYS-1010-REC
                   MOVE    "1"                 TO  WRK-DRCD-HYOUJIKBN
               END-IF
               IF     (SYS-1010-STYUKYMD   <=  SPA-SYSYMD )
                 AND  (SYS-1010-EDYUKYMD   >=  SPA-SYSYMD )
                   MOVE    SYS-1010-REC        TO  WRK-SYS-1010-REC
                   MOVE    SPACE               TO  WRK-DRCD-HYOUJIKBN
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
               IF      FLG-SYSKANRI    =   ZERO
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      WRK-SYS-1010-REC    =   SPACE
               MOVE    "9"                 TO  WRK-DRCD-HYOUJIKBN
               MOVE    "削"                TO  WRK-DRCD-HYOUJI
           ELSE
               MOVE    WRK-SYS-1010-REC    TO  SYS-1010-REC
               IF      WRK-DRCD-HYOUJIKBN      =   "1"
      *            期間外
                   MOVE    "外"                TO  WRK-DRCD-HYOUJI
               END-IF
               MOVE    WRK-SYS-1010-REC    TO  SYS-1010-REC
               PERFORM 310111-DRCD-WKHENSYU-SEC
           END-IF
      *    表示区分
           PERFORM 31021-HIHYOJI-HEN-SEC
           .
       31011-DRCD-NAME-EXT.
           EXIT.
      *****************************************************************
      *    職員情報編集処理
      *****************************************************************
       310111-DRCD-WKHENSYU-SEC               SECTION.
      *
      *    医師名
           MOVE    SYS-1010-NAME       TO  WRK-DRCD-NAME
           MOVE    SYS-1010-KANANAME   TO  WRK-DRCD-KANANAME
      *    非表示区分
           MOVE    SYS-1010-HYOJIFLG   TO  WRK-DRCD-HIHYOJI
      *
      *    専門科コード
           PERFORM VARYING     IDXS    FROM    1   BY  1
                   UNTIL       IDXS    >   5
               IF      SYS-1010-SEMONKACD-X (IDXS) NOT =   SPACE
                   MOVE    SYS-1010-SEMONKACD-X (IDXS)
                                           TO  WRK-DR-SRYKA (IDXS)
                   PERFORM VARYING   IDX2      FROM    1   BY  1
                           UNTIL    (IDX2      >   SPA-SRYKA-MAX)
                       IF      WRK-DR-SRYKA (IDXS)
                                               =   SPA-GMN-SRYKAL(IDX2)
                           MOVE    SPA-NAI-SRYKANAME2 (IDX2)
                                               TO  WRK-DR-SRYAKNAME2
                                                            (IDXS)
                           MOVE    SPA-SRYKA-MAX   TO  IDX2
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       310111-DRCD-WKHENSYU-EXT.
           EXIT.
      *****************************************************************
      *    表示区分編集
      *****************************************************************
       31021-HIHYOJI-HEN-SEC               SECTION.
      *
           MOVE    SPACE               TO  WRK-HYOJIMEI1
           MOVE    SPACE               TO  WRK-HYOJIMEI2
      *    非表示区分
           IF      WRK-DRCD-HIHYOJI    =   "1"
               MOVE    "×"                TO  WRK-HYOJIMEI1
           END-IF
           EVALUATE    WRK-DRCD-HYOUJIKBN
               WHEN    "1"
      *            期間外
                   MOVE    "【外】"            TO  WRK-HYOJIMEI2
               WHEN    "9"
      *            削除
                   MOVE    "【削】"            TO  WRK-HYOJIMEI2
           END-EVALUATE
           STRING    WRK-HYOJIMEI1
                     WRK-HYOJIMEI2         DELIMITED  BY  SPACE
                                       INTO    WRK-DRCD-HYOJIMEI
           END-STRING
           .
       31021-HIHYOJI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    システム管理当日医師表示編集処理
      *****************************************************************
       3101-SYSKANRI-HEN-SEC               SECTION.
      *
     ***** INITIALIZE                  SPA-SYSKANRI-TBL
           INITIALIZE                  WRK-SYSKANRI-TBL
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDXW
      *
           MOVE    SPACE               TO  SYS-1010-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    "1%"                TO  SYS-1010-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
      *    システム管理検索
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM UNTIL      (FLG-SYSKANRI    =   1 )
      *
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
               IF      SYS-1010-KBNCD(1:1) =   "1"
                   MOVE    ZERO                TO  FLG-OK
               ELSE
                   MOVE    1                   TO  FLG-OK
               END-IF
      *
      *        診療科担当医にないこと
               MOVE    ZERO                TO  FLG-OK2
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   SPA-GMN-MAX )
                               OR (FLG-OK2     =   1    )
                   IF      SYS-1010-KBNCD(1:5)
                                           =   SPA-GMN-TDRCD(IDX2)
                       MOVE    1               TO  FLG-OK2
                   END-IF
               END-PERFORM
               IF     (FLG-OK              =   ZERO )
                  AND (FLG-OK2             =   ZERO )
                   EVALUATE    SPA-NAI-LISTKBN2
                   WHEN    1
      *            対象診療科のみ
                       PERFORM 31012-SRYKA-CHEK-SEC
                   WHEN    2
      *            非表示なし
                       IF      SYS-1010-HYOJIFLG   =  "1"
                           MOVE    1               TO  FLG-OK
                       END-IF
                   END-EVALUATE
               END-IF
      *
               IF     (FLG-OK              =   ZERO )
                  AND (FLG-OK2             =   ZERO )
      *            編集
                   ADD     1                   TO  IDX
                   IF      IDX             >   999
                       MOVE    1               TO  FLG-SYSKANRI
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-IF
               IF     (FLG-OK              =   ZERO )
                  AND (FLG-OK2             =   ZERO )
                   MOVE    IDX                 TO  WRK-SYSKANRI-MAX
      *
                   MOVE    SYS-1010-KBNCD(1:5)
                                           TO  WRK-SYSKANRI-TDRCD(IDX)
                   INITIALIZE                  WRK-DRCD-AREA
                   PERFORM 310111-DRCD-WKHENSYU-SEC
      *            表示区分
                   PERFORM 31021-HIHYOJI-HEN-SEC
                   MOVE    WRK-DRCD-NAME   TO  WRK-SYSKANRI-TDRCD-NAME
                                                              (IDX)
                   MOVE    WRK-DRCD-HYOUJIKBN
                                           TO  WRK-SYSKANRI-THYOUJIKBN
                                                             (IDX)
                   MOVE    WRK-DRCD-KANANAME
                                           TO  WRK-SYSKANRI-TDRCD-KANA
                                                             (IDX)
                   MOVE    WRK-DRCD-HIHYOJI
                                           TO  WRK-SYSKANRI-THIHYOJI
                                                             (IDX)
                  MOVE    WRK-DRCD-HYOJIMEI
                                           TO  WRK-SYSKANRI-THYOUJIMEI
                                                             (IDX)
                   PERFORM VARYING     IDXS    FROM    1   BY  1
                           UNTIL       IDXS    >   5
                       MOVE    WRK-DR-SRYKA-OCC (IDXS)
                                       TO  WRK-SYSKANRI-TSRYKA
                                                        (IDX IDXS)
                   END-PERFORM
      *
               END-IF
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    "key9"              TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3101-SYSKANRI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    表示診療科チェック処理
      *****************************************************************
       31012-SRYKA-CHEK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK3
           MOVE    ZERO                TO  FLG-CHK2
           PERFORM VARYING    IDK      FROM    1  BY  1
                   UNTIL     (IDK      >   5   )
                        OR   (FLG-CHK3     =   1   )
               IF     (SYS-1010-SEMONKACD-X (IDK)
                                       NOT =   SPACE  )
                  MOVE    1                   TO  FLG-CHK2
                  IF      (SYS-1010-SEMONKACD-X (IDK)
                                           =   SPA-GMN-SRYKA )
                       MOVE    1           TO  FLG-CHK3
                   END-IF
               END-IF
           END-PERFORM
           IF     (FLG-CHK3            =   1    )
             OR   (FLG-CHK2            =   ZERO )
               CONTINUE
           ELSE
      *        対象外
               MOVE    1               TO  FLG-OK
           END-IF
           .
       31012-SRYKA-CHEK-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       3102-GMNSPA-HEN-SEC               SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
      *
           INITIALIZE                  SPA-GMN-TBL
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  WRK-IDX
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   WRK-LSDOCTOR-MAX)
               ADD     1               TO  IDX
               MOVE    IDX             TO  SPA-GMN-TNO (IDX)
               MOVE    WRK-LSDOCTOR-TRENNUM (IDX2)
                                       TO  SPA-GMN-THNUM (IDX)
               MOVE    WRK-LSDOCTOR-TDRCD (IDX2)
                                       TO  SPA-GMN-TDRCD (IDX)
               MOVE    WRK-LSDOCTOR-TDRCD-NAME (IDX2)
                                       TO  SPA-GMN-TDRCD-NAME (IDX)
               MOVE    WRK-LSDOCTOR-TDRCD-KANA (IDX2)
                                       TO  SPA-GMN-TDRCD-KANA (IDX)
               MOVE    WRK-LSDOCTOR-TSRYKA-G (IDX2)
                                       TO  SPA-GMN-TSRYKA-G (IDX)
               MOVE    WRK-LSDOCTOR-THYOUJIKBN (IDX2)
                                       TO  SPA-NAI-THYOUJIKBN (IDX)
               MOVE    WRK-LSDOCTOR-THIHYOJI   (IDX2)
                                       TO  SPA-NAI-THIHYOJI (IDX)
               MOVE    WRK-LSDOCTOR-THYOUJIMEI  (IDX2)
                                       TO  SPA-GMN-THYOUJI (IDX)
               MOVE    "1"             TO  SPA-NAI-TDBKBN  (IDX)
               MOVE    "1"             TO  SPA-GMN-TSELECT (IDX)
           END-PERFORM
           MOVE    IDX                 TO  SPA-GMN-MAX
           MOVE    IDX                 TO  WRK-IDX
      *
      *
           .
       3102-GMNSPA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    システム管理画面編集処理
      *****************************************************************
       3103-GMNSPA-HEN02-SEC               SECTION.
      *
           MOVE    SPA-GMN-MAX         TO  IDX
      *****MOVE    ZERO                TO  WRK-IDX
           MOVE    ZERO                TO  IDX4
      *    最大表示件数
           COMPUTE WRK-MAX-LINE        =   LINE-MAX
                                       -   SPA-GMN-MAX
      *
           COMPUTE IDX-STR             =   (SPA-GMN-PAGE   -   1 )
                                       *   WRK-MAX-LINE
                                       +   1
           MOVE    ZERO                TO  SPA-GMN-NEXT
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   WRK-SYSKANRI-MAX)
                          OR  (SPA-GMN-NEXT    =   1    )
              ADD     1                TO  IDX4
              IF      IDX4             >=  IDX-STR
                   ADD     1               TO  IDX
                   IF      IDX             >   LINE-MAX
                       MOVE    1               TO  SPA-GMN-NEXT
                   END-IF
                   IF      SPA-GMN-NEXT    =   ZERO
                       ADD     1               TO  WRK-IDX
                       MOVE    WRK-IDX         TO  SPA-GMN-TNO (IDX)
                       MOVE    IDX             TO  SPA-GMN-MAX
                       MOVE    WRK-SYSKANRI-TDRCD (IDX2)
                                               TO  SPA-GMN-TDRCD (IDX)
                       MOVE    WRK-SYSKANRI-TDRCD-NAME (IDX2)
                                               TO  SPA-GMN-TDRCD-NAME
                                                                 (IDX)
                       MOVE    WRK-SYSKANRI-TDRCD-KANA (IDX2)
                                               TO  SPA-GMN-TDRCD-KANA
                                                                 (IDX)
                       MOVE    WRK-SYSKANRI-TSRYKA-G (IDX2)
                                               TO  SPA-GMN-TSRYKA-G
                                                                 (IDX)
                       MOVE    WRK-SYSKANRI-THYOUJIKBN (IDX2)
                                               TO  SPA-NAI-THYOUJIKBN
                                                                  (IDX)
                       MOVE    WRK-SYSKANRI-THIHYOJI   (IDX2)
                                               TO  SPA-NAI-THIHYOJI
                                                                  (IDX)
                       MOVE    WRK-SYSKANRI-THYOUJIMEI  (IDX2)
                                               TO  SPA-GMN-THYOUJI(IDX)
                       MOVE    "2"             TO  SPA-NAI-TDBKBN (IDX)
                   END-IF
               END-IF
           END-PERFORM
           .
       3103-GMNSPA-HEN02-EXT.
           EXIT.
      *****************************************************************
      *    当日担当医設定あり
      *****************************************************************
       3103-SYS1011-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-SYS1011
      *
           MOVE    SPACE               TO  SYS-1011-REC
           INITIALIZE                  SYS-1011-REC
           MOVE    "1011"              TO  SYS-1011-KANRICD
           MOVE    SPA-GMN-SRYKA       TO  SYS-1011-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1011-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1011-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1011-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1011-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    SYSKANRI-REC        TO  SYS-1011-REC
                   IF      SYS-1011-DRCD(01)   NOT =   SPACE
                       MOVE    1               TO  FLG-SYS1011
                   END-IF
               END-IF
           ELSE
               INITIALIZE                  SYS-1011-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYS1011         =   1
               MOVE    "[当日担当医設定より優先します]"
                                       TO  SPA-GMN-MIDASI3
           END-IF
           .
       3103-SYS1011-CHK-EXT.
           EXIT.
      *****************************************************************
      *    確認画面より
      *****************************************************************
       330-U1ID1-SET-SEC               SECTION.
      *
           EVALUATE    SPA-U1IDCD
      *        登録確認
               WHEN    "0101"
                   IF      SPA-U1ID1-FLG    =   "OK"
      *                登録処理
                       PERFORM 4030-TOUROKU-SYORI-SEC
                   END-IF
      *        削除確認
               WHEN    "0103"
                   IF      SPA-U1ID1-FLG    =   "OK"
      *                削除処理
                       PERFORM 4401-DELETE-SYORI-SEC
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
      *        削除処理
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 440-ALL-CLEAR-SEC
      *        表示切替
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 450-LIST-CHANGE-SEC
      *        表示順選択
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 440-UPDWN-SEL-SEC
      *           ＵＰボタン選択
      *        WHEN    "CLICKED"       ALSO    "BUP"
      *            PERFORM 41011-UPSYORI-SEC
      *            ＤＷボタン選択
      *        WHEN    "CLICKED"       ALSO    "BDW"
      *            PERFORM 41012-DWNSYORI-SEC
      *        ＵＰボタン選択
               WHEN    "CLICKED"       ALSO    "B06S"
                   PERFORM 41011-UPSYORI-SEC
      *        ＤＷボタン選択
               WHEN    "CLICKED"       ALSO    "B07S"
                   PERFORM 41012-DWNSYORI-SEC
      *        診療科
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 480-SRYKA-SEL-SEC
      *        非表示
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 490-HIHYOUJI-SEC
      *        確定
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 4011-KAKUNIN-SEC
      *        登録更新
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 4012-TOUROKU-SEC
      *        選択のみ
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 4013-SEL-SYORI-SEC
      *        前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 460-MAE-PAGE-SEC
      *        次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 470-ATO-PAGE-SEC
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
      *        診療内容
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 4102-SELNUM-SYORI-SEC
      *        行選択
               WHEN    ANY             ALSO    "DRLIST"
                   PERFORM 4103-LISTSEL-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      ***************************************************************
      *    表示きり変え処理
      ***************************************************************
       450-LIST-CHANGE-SEC               SECTION.
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    ZERO                TO  SPA-GMN-NEXT
      *
           IF      SPA-NAI-LISTKBN     =   ZERO
               MOVE    1                   TO  SPA-NAI-LISTKBN
           ELSE
               MOVE    ZERO                TO  SPA-NAI-LISTKBN
           END-IF
      *    選択表示分
           PERFORM 4501-LISTCHNGE-HEN-SEC
      *
           MOVE    ZERO                TO  SPA-GMN-SELNUM
           MOVE    ZERO                TO  SPA-NAI-SELNUM
           MOVE    1                   TO  FLG-GMN-INIT
      *
           .
       450-LIST-CHANGE-EXT.
           EXIT.
      ***************************************************************
      *    診療科きり変え処理
      ***************************************************************
       480-SRYKA-SEL-SEC               SECTION.
      *
           IF      SPA-NAI-SYORI-2     =   1
               MOVE    "K003"              TO  SPA-ERRCD
               MOVE    99                  TO  SPA-GMN-CUR
               MOVE    ZERO                TO  SPA-NAI-SYORI-2
               GO      TO      480-SRYKA-SEL-EXT
           END-IF
      *
           IF      SPA-NAI-MAE         =   1
               MOVE    "0007"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      480-SRYKA-SEL-EXT
           END-IF
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-CHK-END
           PERFORM VARYING   IDX2      FROM    1   BY  1
                   UNTIL    (IDX2      >   SPA-SRYKA-MAX)
                         OR (FLG-CHK-END   =   1    )
               IF      FLG-OK          =   1
                   MOVE    SPA-GMN-SRYKA-LIST(IDX2)
                                           TO  SPA-GMN-SRYKA-G
                   MOVE    1               TO  FLG-CHK-END
               END-IF
               IF     (SPA-GMN-SRYKAL (IDX2)
                                           =   SPA-GMN-SRYKA)
                  AND (FLG-CHK-END         =   ZERO    )
                   MOVE    1               TO  FLG-OK
                   IF      IDX2            =   SPA-SRYKA-MAX
                       MOVE    SPA-GMN-SRYKA-LIST(01)
                                           TO  SPA-GMN-SRYKA-G
                       MOVE    1           TO  FLG-CHK-END
                   END-IF
               END-IF
           END-PERFORM
      *
      *    医師画面編集
           PERFORM 3101-TBL-HENSYU-SEC
           MOVE    1                   TO  SPA-GMN-CUR
           .
       480-SRYKA-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号選択処理
      *****************************************************************
       4102-SELNUM-SYORI-SEC        SECTION.
      *
           MOVE    UDR-SELNUM          TO  SPA-GMN-SELNUM
      *
           MOVE    ZERO                TO  SPA-NAI-SELNUM
           IF      SPA-GMN-SELNUM      =   ZERO
               GO      TO      4102-SELNUM-SYORI-EXT
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
               IF      SPA-GMN-SELNUM      =   SPA-GMN-TNO (IDX)
                   MOVE    IDX             TO  SPA-NAI-SELNUM
                   MOVE    SPA-GMN-MAX     TO  IDX
               END-IF
           END-PERFORM
           IF      SPA-NAI-SELNUM      =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      4102-SELNUM-SYORI-EXT
           END-IF
      *
           PERFORM 41021-SELNUM-HENSYU-SEC
      *
           MOVE    1                 TO  SPA-GMN-CUR
      *
           .
       4102-SELNUM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41021-SELNUM-HENSYU-SEC                SECTION.
      *
           IF      SPA-NAI-SYORI   NOT =   1
      *        選択医師のみ
               IF      SPA-GMN-TSELECT (SPA-NAI-SELNUM)    =   SPACE
      *            選択へ
                   MOVE    1                   TO  SPA-NAI-SYORI
               END-IF
           END-IF
      *
           IF      SPA-NAI-SYORI       =   1
      *        対象選択
               IF      SPA-GMN-TSELECT (SPA-NAI-SELNUM)    =   "1"
                   MOVE    SPACE           TO  SPA-GMN-TSELECT
                                                     (SPA-NAI-SELNUM)
               ELSE
                   MOVE    "1"             TO  SPA-GMN-TSELECT
                                                     (SPA-NAI-SELNUM)
      *            削除は選択不可
                   IF      SPA-NAI-THYOUJIKBN(SPA-NAI-SELNUM)
                                               =   "9"
                       MOVE    "0003"          TO  SPA-ERRCD
                       MOVE    SPACE           TO  SPA-GMN-TSELECT
                                                     (SPA-NAI-SELNUM)
                   ELSE
      *                期間外は警告
                       IF      SPA-NAI-THYOUJIKBN(SPA-NAI-SELNUM)
                                                   =   "1"
                           MOVE    "K004"          TO  SPA-ERRCD
                       END-IF
                       MOVE    SPA-GMN-TDRCD(SPA-NAI-SELNUM)
                                               TO  SPA-SEL-DRCD
                   END-IF
               END-IF
      *        表示番号設定
               PERFORM 41021-RENUM-HEN-SEC
               MOVE    ZERO                TO  SPA-NAI-SELNUM
               MOVE    ZERO                TO  SPA-GMN-SELNUM
      ***  ELSE
      *        選択医師のみ
      *        IF      SPA-GMN-TSELECT (SPA-NAI-SELNUM)    =   SPACE
      *            MOVE    "0006"          TO  SPA-ERRCD
      ***      END-IF
           END-IF
           MOVE    1                   TO  SPA-NAI-SYORI-2
      *
           .
       41021-SELNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    最大表示番号設定処理
      *****************************************************************
       41021-RENUM-HEN-SEC                SECTION.
      *
           MOVE    ZERO                TO  WRK-RENNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
               IF      SPA-GMN-TSELECT (IDX)   =   "1"
                   IF     SPA-GMN-THNUM (IDX)  >   WRK-RENNUM
                       MOVE    SPA-GMN-THNUM (IDX) TO  WRK-RENNUM
                   END-IF
               ELSE
                   MOVE    ZERO            TO  SPA-GMN-THNUM (IDX)
               END-IF
           END-PERFORM
      *
           IF     (SPA-GMN-THNUM (SPA-NAI-SELNUM)  =   ZERO )
             AND  (SPA-GMN-TSELECT(SPA-NAI-SELNUM) =   "1"  )
               ADD     1               TO  WRK-RENNUM
               MOVE    WRK-RENNUM      TO  SPA-GMN-THNUM
                                                    (SPA-NAI-SELNUM)
           END-IF
           .
       41021-RENUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       4103-LISTSEL-SEC                SECTION.
      *
      *    行選択
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-GMN-MAX
               IF      UDR-DRLIST-SELECTDATA (IDX)   =   "T"
                   IF      UDR-TNO  (IDX)(1:1)     =   "="
                      MOVE    ZERO             TO  WRK-SELNUM
                   ELSE
                       MOVE    IDX             TO  WRK-SELNUM
                   END-IF
                   MOVE    SPA-GMN-MAX     TO  IDX
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               MOVE    WRK-SELNUM          TO  SPA-NAI-SELNUM
               MOVE    SPA-GMN-TNO (SPA-NAI-SELNUM)
                                           TO  SPA-GMN-SELNUM
               PERFORM 41021-SELNUM-HENSYU-SEC
           END-IF
      *
           MOVE     1                TO  SPA-GMN-CUR
           .
       4103-LISTSEL-EXT.
           EXIT.
      ***************************************************************
      *    初期表示へ処理
      ***************************************************************
       420-CLEAR-SEC               SECTION.
      *
           IF      SPA-GMN-SELNUM      =   ZERO
               PERFORM 310-INIT-SYORI-SEC
           ELSE
               MOVE    ZERO            TO  SPA-GMN-SELNUM
               MOVE    ZERO            TO  SPA-NAI-SELNUM
           END-IF
      *
           .
       420-CLEAR-SEL-EXT.
           EXIT.
      ***************************************************************
      *    全選択削除処理
      ***************************************************************
       440-ALL-CLEAR-SEC               SECTION.
      *
           MOVE    "0103"              TO  SPA-U1IDCD
      *
           .
       440-ALL-CLEAR-EXT.
           EXIT.
      ***************************************************************
      *    削除処理
      ***************************************************************
       4401-DELETE-SYORI-SEC               SECTION.
      *
      *
      *    診療科担当医削除
           INITIALIZE                  LIST-DOCTOR-REC
           MOVE    SPA-HOSPNUM         TO  LSDOCTOR-HOSPNUM
           MOVE    SPA-GMN-SRYKA       TO  LSDOCTOR-SRYKA
      *
           MOVE    LIST-DOCTOR-REC         TO  MCPDATA-REC
           MOVE    "tbl_list_doctor"      TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    医師編集
           PERFORM 3101-TBL-HENSYU-SEC
      *
           MOVE    ZERO                TO  SPA-NAI-SELNUM
           MOVE    ZERO                TO  SPA-GMN-SELNUM
      *
           .
       4401-DELETE-SYORI-EXT.
           EXIT.
      ***************************************************************
      *    診療科入力処理
      ***************************************************************
       4101-SRYKA-SYORI-SEC               SECTION.
      *
           MOVE    SPA-GMN-SRYKA       TO  WRK-SRYKA
      *
           IF      SPA-NAI-SYORI-2     =   1
               MOVE    "K003"              TO  SPA-ERRCD
               MOVE    99                  TO  SPA-GMN-CUR
               MOVE    ZERO                TO  SPA-NAI-SYORI-2
               GO      TO      4101-SRYKA-SYORI-EXT
           END-IF
      *
           IF      SPA-NAI-MAE         =   1
               MOVE    "0007"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      4101-SRYKA-SYORI-EXT
           END-IF
      *
           MOVE    UDR-SRYKA           TO  SPA-GMN-SRYKA-G
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
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        医師編集
               PERFORM 3101-TBL-HENSYU-SEC
      *
               MOVE    ZERO                TO  SPA-NAI-SELNUM
               MOVE    ZERO                TO  SPA-GMN-SELNUM
           END-IF
           .
       4101-SRYKA-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    表示順選択
      *****************************************************************
       440-UPDWN-SEL-SEC               SECTION.
      *
           IF      SPA-NAI-SYORI       =   1
      *        確定処理
               PERFORM 4100-LISTHENSYU-SEC
               MOVE    2               TO  SPA-NAI-SYORI
           ELSE
               MOVE    1               TO  SPA-NAI-SYORI
           END-IF
           MOVE    ZERO                TO  SPA-GMN-SELNUM
           MOVE    ZERO                TO  SPA-NAI-SELNUM
           MOVE    1                   TO  SPA-GMN-CUR
           .
       440-UPDWN-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＵＰボタン選択
      *****************************************************************
       41011-UPSYORI-SEC              SECTION.
      *
           IF      SPA-GMN-SELNUM      =   ZERO
               MOVE    "0005"          TO  SPA-ERRCD
           ELSE
               MOVE    1                   TO  SPA-NAI-SYORI-2
               IF      SPA-NAI-SELNUM      =   1
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-SELNUM      TO  IDX2
                   MOVE    SPA-NAI-SELNUM      TO  IDX
                   PERFORM 410111-UPSYORI-HEN-SEC
               END-IF
           END-IF
           MOVE    1                   TO  SPA-GMN-CUR
           .
       41011-UPSYORI-EXT.
           EXIT.
      *****************************************************************
      *    ＵＰ　表示順変更処理
      *****************************************************************
       410111-UPSYORI-HEN-SEC              SECTION.
      *
           MOVE    SPA-GMN-TBLREC (IDX)    TO  WRK-HZN-TELREC
           MOVE    SPA-GMN-THNUM (IDX)     TO  WRK-HZN-HNUM1
           MOVE    SPA-GMN-THNUM (IDX - 1) TO  WRK-HZN-HNUM2
           MOVE    SPA-GMN-TBLREC(IDX - 1)
                                           TO  SPA-GMN-TBLREC (IDX)
           MOVE    IDX2                TO  SPA-GMN-TNO (IDX)
           IF     (WRK-HZN-HNUM2       >   ZERO )
             AND  (WRK-HZN-HNUM1       >   ZERO )
               IF      WRK-HZN-HNUM1       >   WRK-HZN-HNUM2
                   MOVE    WRK-HZN-HNUM1       TO  SPA-GMN-THNUM (IDX)
               END-IF
           END-IF
      *
           COMPUTE IDX                 =   IDX     -   1
           COMPUTE IDX2                =   IDX2    -   1
           MOVE    WRK-HZN-TELREC      TO  SPA-GMN-TBLREC (IDX)
           MOVE    IDX2                TO  SPA-GMN-TNO    (IDX)
           IF     (WRK-HZN-HNUM2       >   ZERO )
             AND  (WRK-HZN-HNUM1       >   ZERO )
               IF      WRK-HZN-HNUM1       >   WRK-HZN-HNUM2
                   MOVE    WRK-HZN-HNUM2       TO  SPA-GMN-THNUM (IDX)
               END-IF
           END-IF
           MOVE    IDX2                TO  SPA-GMN-SELNUM
           MOVE    IDX                 TO  SPA-NAI-SELNUM
           .
       410111-UPSYORI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ＤＷボタン選択
      *****************************************************************
       41012-DWNSYORI-SEC              SECTION.
      *
           IF      SPA-GMN-SELNUM      =   ZERO
               MOVE    "0005"          TO  SPA-ERRCD
           ELSE
               MOVE    1                   TO  SPA-NAI-SYORI-2
               IF      SPA-NAI-SELNUM      =   SPA-GMN-MAX 
                OR    (SPA-GMN-THNUM (SPA-NAI-SELNUM  + 1)
                                           =   ZERO  )
                 CONTINUE
               ELSE
                   MOVE    SPA-GMN-SELNUM      TO  IDX2
                   MOVE    SPA-NAI-SELNUM      TO  IDX
                   PERFORM 410121-DWNSYORI-HEN-SEC
               END-IF
           END-IF
           MOVE    1                   TO  SPA-GMN-CUR
           .
       41012-DWNSYORI-EXT.
           EXIT.
      *****************************************************************
      *    ＵＰ　表示順変更処理
      *****************************************************************
       410121-DWNSYORI-HEN-SEC              SECTION.
      *
           MOVE    SPA-GMN-TBLREC (IDX)    TO  WRK-HZN-TELREC
           MOVE    SPA-GMN-THNUM (IDX)     TO  WRK-HZN-HNUM1
           MOVE    SPA-GMN-THNUM (IDX + 1) TO  WRK-HZN-HNUM2
           MOVE    SPA-GMN-TBLREC(IDX + 1)
                                           TO  SPA-GMN-TBLREC (IDX)
           MOVE    IDX2                TO  SPA-GMN-TNO (IDX)
           IF     (WRK-HZN-HNUM2       >   ZERO )
             AND  (WRK-HZN-HNUM1       >   ZERO )
               IF      WRK-HZN-HNUM1       <   WRK-HZN-HNUM2
                   MOVE    WRK-HZN-HNUM1       TO  SPA-GMN-THNUM (IDX)
               END-IF
           END-IF
      *
           COMPUTE IDX                 =   IDX     +   1
           COMPUTE IDX2                =   IDX2    +   1
           MOVE    WRK-HZN-TELREC      TO  SPA-GMN-TBLREC (IDX)
           MOVE    IDX2                TO  SPA-GMN-TNO    (IDX)
           IF     (WRK-HZN-HNUM2       >   ZERO )
             AND  (WRK-HZN-HNUM1       >   ZERO )
               IF      WRK-HZN-HNUM1       <   WRK-HZN-HNUM2
                   MOVE    WRK-HZN-HNUM2       TO  SPA-GMN-THNUM (IDX)
               END-IF
           END-IF
           MOVE    IDX2                TO  SPA-GMN-SELNUM
           MOVE    IDX                 TO  SPA-NAI-SELNUM
           .
       410121-DWNSYORI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定処理
      *****************************************************************
       4011-KAKUNIN-SEC              SECTION.
      *
      *    表示内容確定
           MOVE    ZERO                TO  SPA-NAI-LISTKBN
           PERFORM 4100-LISTHENSYU-SEC
           MOVE    1                   TO  FLG-GMN-INIT
      *
           IF      WRK-RENNUM          >   DRLST-MAX
      *********MOVE    "K010"          TO  SPA-ERRCD
               MOVE    "0010"          TO  SPA-ERRCD
           END-IF
      *
           MOVE    ZERO                TO  SPA-NAI-SELNUM
           MOVE    ZERO                TO  SPA-GMN-SELNUM
      *
           MOVE    1                   TO  SPA-GMN-CUR
           . 
       4011-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面表示内容編集処理
      *****************************************************************
       4100-LISTHENSYU-SEC              SECTION.
      *
      *    表示順にSORT
           PERFORM 4100-HYOJI-SORT-SEC
      *
           IF      SPA-NAI-LISTKBN     =   1
      *        ＤＲＣＤ順
               PERFORM 41001-DRCD-SORT-SEC
           ELSE
      *        カナ氏名順
               PERFORM 41002-KANA-SORT-SEC
           END-IF
      *
           MOVE    IDX3                TO  SPA-GMN-MAX
           .
       4100-LISTHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面表示内容編集処理
      *****************************************************************
       4501-LISTCHNGE-HEN-SEC              SECTION.
      *
      *    表示順にSORT
           PERFORM 4100-HYOJI-SORT-SEC
      *
      *    システム管理当日医師表示（ＣＤ順)
           PERFORM 3101-SYSKANRI-HEN-SEC
      *
           IF      SPA-NAI-LISTKBN     =   ZERO
      *        カナ氏名順
               PERFORM 45012-KANA-SORT2-SEC
           END-IF
      *    システム管理当日画面編集
           PERFORM 3103-GMNSPA-HEN02-SEC
      *
           MOVE    ZERO            TO  SPA-GMN-SELNUM
           MOVE    ZERO            TO  SPA-NAI-SELNUM
           .
       4501-LISTCHNGE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    表示順編集処理
      *****************************************************************
       4100-HYOJI-SORT-SEC              SECTION.
      *
           INITIALIZE                  WRK-SORT-AREA
           MOVE    SPA-GMN-TBL         TO  WRK-SORT-AREA
           MOVE    SPA-GMN-MAX         TO  WRK-GMN-MAX
           INITIALIZE                  SPA-GMN-TBL
      *    表示
           MOVE    ZERO                TO  WRK-RENNUM
           MOVE    ZERO                TO  IDX3
           MOVE    ZERO                TO  FLG-SORT-END
      *    表示順にSORT
           PERFORM
                   UNTIL       FLG-SORT-END    =   1
               MOVE    999                 TO  WRK-HNUM
               MOVE    ZERO                TO  IDX2
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   WRK-GMN-MAX
                   IF     (WRK-SORT-TSELECT (IDX)   =   "1" )
                      AND (WRK-SORT-TDRCD (IDX) NOT =   ZERO )
      *               削除は対象外
                      AND  (WRK-SORT-THYOUJIKBN(IDX) NOT =   "9" )
                       IF    (WRK-SORT-THNUM (IDX)   <   WRK-HNUM )
                         OR  (WRK-SORT-THNUM (IDX)   =   999      )
                           MOVE    WRK-SORT-THNUM(IDX) TO  WRK-HNUM
                           MOVE    IDX                 TO  IDX2
                       END-IF
                   END-IF
               END-PERFORM
               IF     (IDX2                >   ZERO     )
                  AND (IDX3                <   LINE-MAX )
                   ADD     1               TO  WRK-RENNUM
                   ADD     1               TO  IDX3
                   MOVE    WRK-SORT-TBLREC (IDX2)
                                           TO  SPA-GMN-TBLREC (IDX3)
                   MOVE    IDX3            TO  SPA-GMN-TNO    (IDX3)
                   MOVE    WRK-RENNUM      TO  SPA-GMN-THNUM (IDX3)
                   MOVE    ZERO            TO  WRK-SORT-TDRCD (IDX2)
               ELSE
      *            対象終了
                   MOVE    1               TO  FLG-SORT-END
               END-IF
           END-PERFORM
      *
           MOVE    IDX3                TO  WRK-IDX
           MOVE    IDX3                TO  IDX4
      *    切替え
           IF     (IDX3                <   LINE-MAX )
              AND (IDX3                <   WRK-GMN-MAX )
              AND (IDX3                >   ZERO     )
               ADD     1               TO  IDX3
               MOVE    ALL "-"         TO  SPA-GMN-TDRCD (IDX3)
               MOVE    "--"            TO  SPA-GMN-THYOUJI(IDX3)
           END-IF
      *
           MOVE    IDX3                TO  SPA-GMN-MAX
      *
           .
       4100-HYOJI-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＲＣＤ順処理
      *****************************************************************
       41001-DRCD-SORT-SEC              SECTION.
      *
      *    対象外をドクターＣＤ順にＳＯＲＴ
           MOVE    ZERO                TO  FLG-SORT-END
      *    表示順にSORT
           PERFORM
                   UNTIL       FLG-SORT-END    =   1
               MOVE    "99999"             TO  WRK-SRT-DRCD
               MOVE    ZERO                TO  IDX2
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   WRK-GMN-MAX
                   IF     (WRK-SORT-TDRCD (IDX) NOT =   ZERO )
                     AND  (WRK-SORT-TDRCD (IDX) <   WRK-SRT-DRCD )
                     AND  (WRK-SORT-TDRCD (IDX) NUMERIC  )
                     AND  (WRK-SORT-THYOUJIKBN(IDX) NOT =   "9" )
                       MOVE    WRK-SORT-TDRCD(IDX) TO  WRK-SRT-DRCD
                       MOVE    IDX                 TO  IDX2
                   END-IF
               END-PERFORM
               IF     (IDX2                >   ZERO     )
                  AND (IDX3                <   LINE-MAX )
                   ADD     1                   TO  IDX3
                   ADD     1                   TO  IDX4
                   MOVE    WRK-SORT-TBLREC (IDX2)
                                               TO  SPA-GMN-TBLREC
                                                               (IDX3)
                   MOVE    IDX4                TO  SPA-GMN-TNO (IDX3)
                   MOVE    ZERO                TO  SPA-GMN-THNUM
                                                               (IDX3)
                   MOVE    SPACE               TO  SPA-GMN-TSELECT
                                                               (IDX3)
                   MOVE    ZERO                TO  WRK-SORT-TDRCD
                                                               (IDX2)
               ELSE
      *            対象終了
                   MOVE    1               TO  FLG-SORT-END
               END-IF
           END-PERFORM
      *
           .
       41001-DRCD-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    カナ氏名順処理
      *****************************************************************
       41002-KANA-SORT-SEC              SECTION.
      *
      *    対象外をドクターＣＤ順にＳＯＲＴ
           MOVE    ZERO                TO  FLG-SORT-END
      *    表示順にSORT
           PERFORM
                   UNTIL       FLG-SORT-END    =   1
               MOVE    HIGH-VALUE          TO  WRK-SRT-KANA
               MOVE    ZERO                TO  IDX2
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   WRK-GMN-MAX
                   IF     (WRK-SORT-TDRCD     (IDX) NOT =   ZERO )
                     AND  (WRK-SORT-TDRCD     (IDX) NUMERIC      )
                     AND  (WRK-SORT-TKANANAME (IDX) <   WRK-SRT-KANA)
                     AND  (WRK-SORT-THYOUJIKBN(IDX) NOT =   "9" )
                       MOVE    WRK-SORT-TKANANAME (IDX)
                                                   TO  WRK-SRT-KANA
                       MOVE    IDX                 TO  IDX2
                   END-IF
               END-PERFORM
               IF     (IDX2                >   ZERO     )
                  AND (IDX3                <   LINE-MAX )
                   ADD     1                   TO  IDX3
                   ADD     1                   TO  IDX4
                   MOVE    WRK-SORT-TBLREC (IDX2)
                                               TO  SPA-GMN-TBLREC
                                                               (IDX3)
                   MOVE    IDX4                TO  SPA-GMN-TNO (IDX3)
                   MOVE    ZERO                TO  SPA-GMN-THNUM
                                                               (IDX3)
                   MOVE    SPACE               TO  SPA-GMN-TSELECT
                                                               (IDX3)
                   MOVE    ZERO                TO  WRK-SORT-TDRCD
                                                               (IDX2)
               ELSE
      *            対象終了
                   MOVE    1               TO  FLG-SORT-END
               END-IF
           END-PERFORM
      *
           . 
       41002-KANA-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    カナ氏名順処理
      *****************************************************************
       45012-KANA-SORT2-SEC              SECTION.
      *
      *    対象外をドクターＣＤ順にＳＯＲＴ
           MOVE    ZERO                TO  FLG-SORT-END
           INITIALIZE                  WRK-SORT2-TBL
           MOVE    WRK-SYSKANRI-TBL    TO  WRK-SORT2-TBL
           MOVE    WRK-SYSKANRI-MAX    TO  WRK-GMN-MAX
           INITIALIZE                  WRK-SYSKANRI-TBL
           MOVE    ZERO                TO  IDX3
           MOVE    ZERO                TO  IDX4
      *    表示順にSORT
           PERFORM
                   UNTIL       FLG-SORT-END    =   1
               MOVE    HIGH-VALUE          TO  WRK-SRT-KANA
               MOVE    ZERO                TO  IDX2
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   WRK-GMN-MAX
                   IF     (WRK-SORT2-TDRCD     (IDX) NOT =   ZERO )
                     AND  (WRK-SORT2-TDRCD     (IDX) NUMERIC      )
                     AND  (WRK-SORT2-TDRCD-KANA (IDX) <   WRK-SRT-KANA)
                       MOVE    WRK-SORT2-TDRCD-KANA (IDX)
                                                   TO  WRK-SRT-KANA
                       MOVE    IDX                 TO  IDX2
                   END-IF
               END-PERFORM
               IF     (IDX2                >   ZERO     )
                  AND (IDX3                <   999      )
                   ADD     1                   TO  IDX3
                   MOVE    WRK-SORT2-TBLREC (IDX2)
                                               TO  WRK-SYSKANRI-TBLREC
                                                               (IDX3)
                   MOVE    ZERO                TO  WRK-SORT2-TDRCD
                                                               (IDX2)
               ELSE
      *            対象終了
                   MOVE    1               TO  FLG-SORT-END
               END-IF
           END-PERFORM
           MOVE    IDX3                TO  WRK-SYSKANRI-MAX
      *
           . 
       45012-KANA-SORT2-EXT.
           EXIT.
      *****************************************************************
      *    表示診療科チェック処理
      *****************************************************************
       4500-SRYKA-CHEK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK3
           MOVE    ZERO                TO  FLG-CHK2
           PERFORM VARYING    IDK      FROM    1  BY  1
                   UNTIL     (IDK      >   5   )
                        OR   (FLG-CHK3     =   1   )
               IF     (WRK-SORT-TSRYKA (IDX IDK)
                                       NOT =   SPACE  )
                  MOVE    1                   TO  FLG-CHK2
                  IF      (WRK-SORT-TSRYKA (IDX IDK)(1:2)
                                           =   SPA-GMN-SRYKA )
                       MOVE    1           TO  FLG-CHK3
                   END-IF
               END-IF
           END-PERFORM
           IF     (FLG-CHK3            =   1    )
             OR   (FLG-CHK2            =   ZERO )
               CONTINUE
           ELSE
               MOVE    1               TO  FLG-CHK
           END-IF
           .
       4500-SRYKA-CHEK-EXT.
           EXIT.
      *****************************************************************
      *    非表示処理
      *****************************************************************
       490-HIHYOUJI-SEC             SECTION.
      *
           EVALUATE    SPA-NAI-LISTKBN2
               WHEN    0
                   MOVE    1           TO  SPA-NAI-LISTKBN2
               WHEN    1
                   MOVE    2           TO  SPA-NAI-LISTKBN2
               WHEN    2
                   MOVE    0           TO  SPA-NAI-LISTKBN2
           END-EVALUATE
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    ZERO                TO  SPA-GMN-NEXT
      *
      *    選択表示分
           PERFORM 4501-LISTCHNGE-HEN-SEC
      *
           MOVE    ZERO                TO  SPA-GMN-SELNUM
           MOVE    ZERO                TO  SPA-NAI-SELNUM
           MOVE    1                   TO  FLG-GMN-INIT
           . 
       490-HIHYOUJI-EXT.
           EXIT.
      *****************************************************************
      *    登録更新処理
      *****************************************************************
       4012-TOUROKU-SEC             SECTION.
      *
           PERFORM 4011-KAKUNIN-SEC
      *    IF      SPA-KEI-FLG         =   1
      *        MOVE    SPACE           TO  SPA-ERRCD
      *    ELSE
      *        MOVE    1               TO  SPA-KEI-FLG
      *    END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0101"              TO  SPA-U1IDCD
           END-IF
      *
           .
       4012-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       4030-TOUROKU-SYORI-SEC           SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    診療科担当医削除
           INITIALIZE                  LIST-DOCTOR-REC
           MOVE    SPA-HOSPNUM         TO  LSDOCTOR-HOSPNUM
           MOVE    SPA-GMN-SRYKA       TO  LSDOCTOR-SRYKA
      *
           MOVE    LIST-DOCTOR-REC     TO  MCPDATA-REC
           MOVE    "tbl_list_doctor"   TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-MAX  )
                         OR   (SPA-ERRCD   NOT =   SPACE )
               IF     (SPA-GMN-TSELECT (IDX)   =   "1" )
                 AND  (SPA-NAI-THYOUJIKBN(IDX) NOT =   "9" )
                   PERFORM 40301-LSDOCTOR-INS-SEC
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-MAE         =   1
                   MOVE    SPA-SEL-DRCD        TO  SPA-DRCD
                   PERFORM 210-BACK
                   GO  TO  4030-TOUROKU-SYORI-EXT
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
              AND (FLG-END             =   ZERO  )
      *        医師編集
               PERFORM 3101-TBL-HENSYU-SEC
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
           .
       4030-TOUROKU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       40301-LSDOCTOR-INS-SEC           SECTION.
      *
           INITIALIZE                  LIST-DOCTOR-REC
           MOVE    SPA-HOSPNUM         TO  LSDOCTOR-HOSPNUM
           MOVE    SPA-GMN-SRYKA       TO  LSDOCTOR-SRYKA
           MOVE    SPA-GMN-THNUM (IDX) TO  LSDOCTOR-RENNUM
           MOVE    SPA-GMN-TDRCD (IDX) TO  LSDOCTOR-DRCD
      *
           MOVE    SMCNDATE-YMD        TO  LSDOCTOR-CREYMD
           MOVE    SMCNDATE-YMD        TO  LSDOCTOR-UPYMD
           MOVE    SMCNDATE-HMS        TO  LSDOCTOR-UPHMS
           MOVE    SPA-OPID            TO  LSDOCTOR-OPID
      *
           MOVE    LIST-DOCTOR-REC     TO  MCPDATA-REC
           MOVE    "tbl_list_doctor"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "900"               TO  SPA-ERRCD
               DISPLAY "SRYKADR INSERT ERR:"  MCP-RC
                       ",KEY:" LSDOCTOR-KEY
           END-IF
           .
       40301-LSDOCTOR-INS-EXT.
           EXIT.
      *****************************************************************
      *    登録更新処理
      *****************************************************************
       4013-SEL-SYORI-SEC             SECTION.
      *
           IF     (SPA-NAI-MAE         =   ZERO )
               GO      TO     4013-SEL-SYORI-EXT
           END-IF
           IF     (SPA-SEL-DRCD        =   ZERO )
               MOVE    "0013"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-MAE         =   1
                   MOVE    SPA-SEL-DRCD        TO  SPA-DRCD
                   PERFORM 210-BACK
               END-IF
           END-IF
      *
           .
       4013-SEL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "UDR"               TO  SPA-MOTOPG
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
      *        選択表示分
               PERFORM 4501-LISTCHNGE-HEN-SEC
           ELSE
               MOVE    "0011"          TO  SPA-ERRCD
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
           IF      SPA-GMN-NEXT        =   1
               COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE   +   1
      *        選択表示分
               PERFORM 4501-LISTCHNGE-HEN-SEC
           ELSE
               MOVE    "0012"          TO  SPA-ERRCD
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
           MOVE    "UDR"               TO  MCP-WINDOW
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
           IF     (FLG-GMN-INIT        =   1   )  OR
                  (SPA-GMN-MAX         =   ZERO )
               MOVE    ZERO                TO  WRK-UDR-ROW
           ELSE
               MOVE    UDR-DRLIST-ROW      TO  WRK-UDR-ROW
           END-IF
      *
      *
           MOVE    SPACE               TO  UDR
           INITIALIZE                      UDR
      *
           IF      WRK-UDR-ROW     NOT =   ZERO
               MOVE    WRK-UDR-ROW         TO  UDR-DRLIST-ROW
           END-IF
      *
      *    処理内容
           EVALUATE    SPA-NAI-SYORI
               WHEN    1
                   MOVE    "＜医師選択＞"          TO  UDR-MIDASI-DATA
                   MOVE    "black"                 TO  UDR-MIDASI-STYLE
                   MOVE    "表示順変更"            TO  UDR-B05-LABEL
               WHEN    2
                   MOVE    "＜表示順変更＞"        TO  UDR-MIDASI-DATA
                   MOVE    "blue"                  TO  UDR-MIDASI-STYLE
                   MOVE    "医師選択"              TO  UDR-B05-LABEL
           END-EVALUATE
      *
           EVALUATE    SPA-NAI-LISTKBN2
               WHEN    0
                   MOVE    "（全体）"              TO  WRK-MIDASI2
                   MOVE    "専門科"                TO  UDR-B09-LABEL
               WHEN    1
                   MOVE    "（専門科）"            TO  WRK-MIDASI2
                   MOVE    "表示分"                TO  UDR-B09-LABEL
               WHEN    2
                   MOVE    "（非表示なし）"        TO  WRK-MIDASI2
                   MOVE    "全体"                  TO  UDR-B09-LABEL
           END-EVALUATE
      *
           EVALUATE    SPA-NAI-LISTKBN
               WHEN    1
                   STRING  "【コード順】"
                          WRK-MIDASI2          DELIMITED  BY  SIZE
                                               INTO    UDR-MIDASI2-DATA
                   END-STRING
                   MOVE    "black"                 TO  UDR-MIDASI2-STYLE
                   MOVE    "カナ氏名順"            TO  UDR-B03-LABEL
               WHEN    0
                   STRING  "【カナ氏名順】"
                          WRK-MIDASI2          DELIMITED  BY  SIZE
                                               INTO    UDR-MIDASI2-DATA
                   END-STRING
                   MOVE    "black"                 TO  UDR-MIDASI2-STYLE
                   MOVE    "コード順"              TO  UDR-B03-LABEL
           END-EVALUATE
      *
           IF      SPA-GMN-MIDASI3 NOT =   SPACE
               MOVE    SPA-GMN-MIDASI3     TO  UDR-MIDASI3-DATA
               MOVE    "blue"              TO  UDR-MIDASI3-STYLE
           END-IF
      *
      *    診療科
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               MOVE    SPA-GMN-SRYKA-LIST (IDX)
                                       TO  UDR-SRYKA-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-SRYKA-MAX       TO  UDR-SRYKA-COUNT
           MOVE    SPA-GMN-SRYKA-G     TO  UDR-SRYKA
      *
      *    一覧
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   SPA-GMN-MAX)
                           OR (IDX             >   LINE-MAX   )
      *
           IF      SPA-GMN-TDRCD (IDX)     =   "-----"
               MOVE    ALL "="             TO  UDR-TNO  (IDX)
               MOVE    ALL "="             TO  UDR-THNUM  (IDX)
               MOVE    ALL "="             TO  UDR-TDRCD  (IDX)
               MOVE    "＝＝＝　診療科担当医未選択　＝＝＝"
                                           TO  UDR-TDRCDNAME (IDX)
           ELSE
      *        表示
               MOVE    SPA-GMN-TNO   (IDX) TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  UDR-TNO  (IDX)
      *
               MOVE    SPA-GMN-THNUM (IDX) TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  UDR-THNUM  (IDX)
      *
               MOVE    SPA-GMN-TDRCD (IDX)(2:4)
                                           TO  UDR-TDRCD  (IDX)
               MOVE    SPA-GMN-TDRCD-NAME  (IDX)
                                           TO  UDR-TDRCDNAME (IDX)
               MOVE    SPA-GMN-TSRYKA (IDX 1)
                                           TO  UDR-TSRYKA1 (IDX)
               MOVE    SPA-GMN-TSRYKA (IDX 2)
                                           TO  UDR-TSRYKA2 (IDX)
               MOVE    SPA-GMN-TSRYKA (IDX 3)
                                           TO  UDR-TSRYKA3 (IDX)
               MOVE    SPA-GMN-TSRYKA (IDX 4)
                                           TO  UDR-TSRYKA4 (IDX)
               MOVE    SPA-GMN-TSRYKA (IDX 5)
                                           TO  UDR-TSRYKA5 (IDX)
               MOVE    SPA-GMN-THYOUJI (IDX)
                                           TO  UDR-THYOUJI (IDX)
      *
               IF     (SPA-GMN-SELNUM      >   ZERO )
                  AND (SPA-GMN-SELNUM      =   SPA-GMN-TNO (IDX) )
      *            MOVE    IDX             TO  UDR-DRLIST-ROW
      *            MOVE    2               TO  UDR-DRLIST-ROWATTR
                   MOVE    "T"             TO  UDR-DRLIST-SELECTDATA
                                                                (IDX)
               ELSE
                   MOVE    "F"             TO  UDR-DRLIST-SELECTDATA
                                                                (IDX)
               END-IF
      *        背景色
               IF     (SPA-GMN-TSELECT (IDX)   =   "1"  )
                AND   (SPA-GMN-THNUM (IDX)     <=  DRLST-MAX )
      *            対象医師
                   MOVE    "#E5FFD1"       TO  UDR-DRLIST-BGCOLOR
                                                               (IDX)
               ELSE
                   MOVE    "#FFFFFF"       TO  UDR-DRLIST-BGCOLOR
                                                               (IDX)
               END-IF
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-MAX         TO  UDR-DRLIST-COUNT
      *
           MOVE    SPA-GMN-SELNUM      TO  UDR-SELNUM
      *
           IF     (SPA-NAI-MAE         =   1    )
               MOVE    "今回選択"          TO  UDR-B11-LABEL
           ELSE
               MOVE    SPACE               TO  UDR-B11-LABEL
           END-IF
      *    次頁あり
           IF      SPA-GMN-NEXT        =   1
               IF      SPA-GMN-PAGE        =   1
                   MOVE    "次頁有り"          TO  UDR-NEXT-DATA
               ELSE
                   MOVE    "前頁・次頁有り"    TO  UDR-NEXT-DATA
               END-IF
               MOVE    "red"               TO  UDR-NEXT-STYLE
           ELSE
               IF      SPA-GMN-PAGE        >   1
                   MOVE    "前頁有り"          TO  UDR-NEXT-DATA
                   MOVE    "red"               TO  UDR-NEXT-STYLE
               ELSE
                   MOVE    SPACE               TO  UDR-NEXT-DATA
                   MOVE    "black"             TO  UDR-NEXT-STYLE
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
                   MOVE    "SELNUM"        TO  MCP-WIDGET
               WHEN    02
                   MOVE    "SRYKA"         TO  MCP-WIDGET
               WHEN    99
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
           MOVE    SPACE               TO  SPA-ERRMSG
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "診療科が有りません"  
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "選択番号が有りません。"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   STRING  "医師コードが有効ではありません。"
                           "選択できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0005"
                   STRING  "番号を選択して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K004"
                   STRING  "警告！"
                           "医師コードが有効ではありません。"
                           "選択できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0006"
                   STRING  "担当医ではありません。"
                           "表示順の変更はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0007"
                   STRING  "患者選択中です。"
                           "診療科の変更はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K003"
                   STRING  "警告！"
                           "変更内容が破棄されます。"
                           "登録して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K010"
                   STRING  "警告！"
                           "診療科担当医が９９件以上となります。"
                           "ドクターリストに表示できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0010"
                   STRING  "診療科担当医が９９件以上となります。"
                           "これ以上登録できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0011"
                   STRING  "前頁はありません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0012"
                   STRING  "次頁はありません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0013"
                   STRING  "ドクターの選択がありません"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPACE       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  U1ERR
           MOVE    SPA-ERRCD            TO  U1ERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  U1ERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "UDR"                TO  SPA-MOTOPG
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
           MOVE    SPACE               TO  WRK-U1IDMSG
           EVALUATE    SPA-U1IDCD
               WHEN    "0101"
                   MOVE    "担当医を登録します。よろしいですか？"
                                       TO  WRK-U1IDMSG
               WHEN    "0103"
                   STRING  "登録してある担当医を削除します。"
                           "復元はできません。よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-U1IDMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPACE       TO  WRK-U1IDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  U1ID1
           INITIALIZE                       U1ID1
           MOVE    SPA-U1IDCD           TO  U1ID1-ID1CODE
           MOVE    WRK-U1IDMSG          TO  U1ID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
           IF      SPA-U1IDCD           =   "0103"
               MOVE    "B01"                TO  MCP-WIDGET
           END-IF
      *
           MOVE    "UDR"                TO  SPA-MOTOPG
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
      *    担当医マスタ読み込み処理
      *****************************************************************
       900-LSDOCTOR-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  LIST-DOCTOR-REC
               MOVE    ZERO            TO  FLG-LSDOCTOR
           ELSE
               INITIALIZE                  LIST-DOCTOR-REC
               MOVE    1               TO  FLG-LSDOCTOR
           END-IF
      *
           .
       900-LSDOCTOR-READ-EXT.
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
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               MOVE    SPACE           TO  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
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
