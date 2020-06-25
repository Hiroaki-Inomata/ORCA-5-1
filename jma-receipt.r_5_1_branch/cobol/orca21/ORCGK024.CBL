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
       PROGRAM-ID.                 ORCGK024.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 電子お薬手帳連携表示（Ｋ０２４）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/12/XX    NACL−多々納  新規作成
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
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面用共通スパ領域
           COPY    "K02SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-SPASCR          PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-OKUKAN          PIC 9(01). 
           03  FLG-OKUDAT          PIC 9(01).
      *    03  FLG-PTINF           PIC 9(01).
      *    03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-HENSYU          PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-KETA5           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
           03  IDXG                PIC 9(04).
           03  IDX-C               PIC 9(04).
           03  IDM                 PIC 9(04).
           03  IDX-MAX             PIC 9(04).
      *
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
      *
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WFIL1       PIC X(01).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WFIL2       PIC X(01).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-SELNUM          PIC 9(03).
      *
           03  WRK-KIDMSG          PIC X(80).
      *
           03  WRK-RENNUM          PIC 9(03).
      *
           03  WRK-ZZ              PIC ZZZ.
           03  WRK-Z9              PIC ZZ9.
           03  WRK-CNT             PIC 9(03).
      *
           03  WRK-STR                 PIC 9(03).
      *
      *    コラムリスト位置
           03  WRK-K024-ROW            PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
           03  FLG-GMN-INIT2           PIC  9(01).
      *
           03  WRK-KEY-AREA.
               05  WRK-KEY-RP          PIC X(03).
           03  WRK-NAI-SEX             PIC X(01).
      *    数量表示用
           03  WRK-SURYO           PIC 9(05)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(05).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZZ.
               05  WRK-SURYO-Z1-9  REDEFINES   WRK-SURYO-Z1
                                   PIC ZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZZZ.
           03  WRK-SURYO-X2        PIC X(09).
           03  WRK-SURYO-J-G.
               05  WRK-SURYO-J         PIC X(18).
               05  WRK-SURYO-J3        PIC X(04).
      *
       01  WRK-MEISAI-AREA.
           03  WRK-MEISAI-TBL          OCCURS   500.
               05  WRK-MEI-NUM         PIC X(04).
               05  WRK-MEI-DATA        PIC X(80).
           03  WRK-MEI-MAX             PIC 9(03).
      *
      *
       01  WRK-DATA-AREA.
           03  WRK-DATA-TBL          OCCURS   500.
               05  WRK-DAT-NUM         PIC X(04).
               05  WRK-DAT-DATA        PIC X(80).
      *
               05  WRK-DAT-DATA-R1     REDEFINES
                                       WRK-DAT-DATA.
                   07  WRK-DAT-MEI01       PIC X(42).
                   07  WRK-DAT-SURYO       PIC X(18).
                   07  WRK-DAT-TANNI       PIC X(20).
               05  WRK-DAT-DATA-R2     REDEFINES
                                       WRK-DAT-DATA.
                   07  WRK-DAT-MEI02       PIC X(38).
                   07  WRK-DAT-SURYO2      PIC X(22).
                   07  WRK-DAT-TANNI2      PIC X(20).
               05  WRK-DAT-DATA-R3     REDEFINES
                                       WRK-DAT-DATA.
                   07  WRK-DAT-MEI03       PIC X(54).
                   07  WRK-DAT-KAISU       PIC X(06).
                   07  WRK-DAT-TANNI3      PIC X(20).
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
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
      *    COPY    "CPSYSKANRI.INC".
      *
      *    COPY    "CPSK1010.INC".
      *
      *    お薬手帳管理データ
       01  OKUSURIKANRI-REC.
           COPY    "CPOKUSURIKANRI.INC".
      *
      *    お薬手帳薬剤データ
       01  OKUSURIDATA-REC.
           COPY    "CPOKUSURIDATA.INC".
      *
      *    お薬手帳管理データ
       01  HZN-OKUSURIKANRI-REC.
           COPY    "CPOKUSURIKANRI.INC"  REPLACING
                                       //OKUKAN-// BY //HZN-OKUKAN-//.
      *    患者マスタ−
      *01  PTINF-REC.
      *    COPY    "CPPTINF.INC".
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
      *    患者番号変換サブ
      *    COPY    "CPORCSPTID.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    排他制御サブパラメタ
      **   COPY    "CPORCSLOCK.INC".
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
           COPY    "ORCA21SCRAREA.INC".
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
           INITIALIZE                  WRK-MEISAI-AREA
           INITIALIZE                  WRK-DATA-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
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
           INITIALIZE                  STS-AREA
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
               IF      FLG-END         =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE   "NEW"                TO  MCP-PUTTYPE
               MOVE   "K024"               TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
           END-IF
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
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "KID1"
      *            確認画面より
                   PERFORM 30010-KID1-SET-SEC
                   MOVE    SPACE               TO  SPA-KIDCD
               WHEN    "K0241"
      *            未設定一覧画面より
                   MOVE    ZERO                TO  SPA-K024-PAGE
                   PERFORM 3101-KANRILIST-HEN-SEC
      *            未登録分判定
                   PERFORM 3102-OKUSURI-KANRI-CHK-SEC
                   MOVE    1                   TO  SPA-K024-CUR
      *
               WHEN    OTHER
                   INITIALIZE                  SPA-K024-AREA
                   PERFORM                     310-SPA-SET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＫＩＤ１）ＯＫ処理
      *****************************************************************
       30010-KID1-SET-SEC          SECTION.
      *
           EVALUATE    SPA-KIDCD
               WHEN    "0101"
                   IF      SPA-KID1-FLG        =   "OK"
      *                登録処理
                       PERFORM 4401-KAIJO-UPDATE-SEC
                       MOVE    1               TO  SPA-K024-CUR
                   ELSE
                       MOVE    ZERO            TO  SPA-K024-SELNUM
                       MOVE    1               TO  SPA-K024-CUR
                   END-IF
           END-EVALUATE
           .
       30010-KID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-K024-AREA
      *    性別コード変換
           EVALUATE    SPA-GMN-SEX
               WHEN    "男"
                   MOVE    "1"             TO  SPA-K024-SEX
               WHEN    "女"
                   MOVE    "2"             TO  SPA-K024-SEX
           END-EVALUATE
      *
           MOVE    ZERO                TO  SPA-K024-PAGE
           PERFORM 3101-KANRILIST-HEN-SEC
      *
      *    未登録分判定
           PERFORM 3102-OKUSURI-KANRI-CHK-SEC
      *
           MOVE    1                   TO  SPA-K024-CUR
           .
       310-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    管理画面編集処理
      *****************************************************************
       3101-KANRILIST-HEN-SEC             SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
           MOVE    ZERO                TO  SPA-K024-NEXT
           INITIALIZE                  SPA-K024-LIST
           MOVE    ZERO                TO  SPA-K024-MAX
           MOVE    ZERO                TO  SPA-K024-SELNUM
      *
           IF     (SPA-K024-PAGE       =   ZERO )
               MOVE    1                   TO  SPA-K024-PAGE
           END-IF
           COMPUTE WRK-STR             =  (SPA-K024-PAGE   -   1  )
                                       *   100
                                       +   1
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  FLG-HENSYU
      *
           INITIALIZE                  OKUSURIKANRI-REC
           MOVE    SPA-HOSPNUM         TO  OKUKAN-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  OKUKAN-PTID
      *
           MOVE    OKUSURIKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_okusuri_kanri" TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_okusuri_kanri" TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-OKUSURIKANRI-READ-SEC
           ELSE
               INITIALIZE                  OKUSURIKANRI-REC
               MOVE    1               TO  FLG-OKUKAN
           END-IF
           PERFORM
               UNTIL   (FLG-OKUKAN     =   1   )  OR
                       (FLG-HENSYU     =   1   )
      *        性別・生年月日の一致のみ
      *        IF     (WRK-NAI-SEX         =   OKUKAN-SEX)
      *           AND (SPA-NAI-BIRTHDAY    =   OKUKAN-BIRTHDAY)
                   ADD     1                   TO  IDY
                   IF      IDY                 <   WRK-STR
                       CONTINUE
                   ELSE
                       ADD     1               TO  IDX
                       IF      IDX             >   100
                           MOVE    1               TO  SPA-K024-NEXT
                           MOVE    1               TO  FLG-HENSYU
                       ELSE
                           PERFORM 31011-TBL-HENSYU-SEC
                       END-IF
                   END-IF
      **       END-IF
      *
               MOVE    "tbl_okusuri_kanri" TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-OKUSURIKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_okusuri_kanri" TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       3101-KANRILIST-HEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集用処理
      *****************************************************************
       31011-TBL-HENSYU-SEC             SECTION.
      *
           MOVE    IDX                 TO  SPA-K024-MAX
           MOVE    IDX                 TO  SPA-K024G-TNUM   (IDX)
      *    調剤日
           MOVE    OKUKAN-SRYYMD       TO  SPA-K024-TSRYYMD(IDX)
      *    医療機関コード
           MOVE    OKUKAN-HOSPCD       TO  SPA-K024-THOSPCD(IDX)
      *    データキー
           MOVE    OKUKAN-DATA-KEY     TO  SPA-K024-DATA-KEY (IDX)
      *    医療機関名
           MOVE    OKUKAN-HOSPNAME     TO  SPA-K024G-THOSPNAME(IDX)
      *    連番
           MOVE    OKUKAN-RENNUM       TO  SPA-K024-TRENNUM  (IDX)
      *    調剤日
           MOVE    OKUKAN-SRYYMD       TO  WRK-SYMD
           PERFORM 520-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-K024G-TSRYYMD (IDX)
           IF      OKUKAN-RENNUM       >   1
               MOVE    SPACE           TO  SPA-K024G-TSRYYMD (IDX)
               STRING  WRK-HENYMDG
                       "("
                       OKUKAN-RENNUM
                       ")"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-K024G-TSRYYMD (IDX)
               END-STRING
           END-IF
      *
           .
       31011-TBL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    未登録分判定処理
      *****************************************************************
       3102-OKUSURI-KANRI-CHK-SEC      SECTION.
      *
           INITIALIZE                  OKUSURIKANRI-REC
           MOVE    SPA-HOSPNUM         TO  OKUKAN-HOSPNUM
           MOVE    ZERO                TO  OKUKAN-PTID
           MOVE    SPA-K024-SEX        TO  OKUKAN-SEX
           MOVE    SPA-NAI-BIRTHDAY    TO  OKUKAN-BIRTHDAY
      *
           MOVE    OKUSURIKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_okusuri_kanri" TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_okusuri_kanri" TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-OKUSURIKANRI-READ-SEC
           ELSE
               INITIALIZE                  OKUSURIKANRI-REC
               MOVE    1               TO  FLG-OKUKAN
           END-IF
           MOVE    "tbl_okusuri_kanri" TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-OKUKAN          =   ZERO
               MOVE    1               TO  SPA-K024-OKSR-ARI
           ELSE
               MOVE    ZERO            TO  SPA-K024-OKSR-ARI
           END-IF
      *
           .
       3102-OKUSURI-KANRI-CHK-EXT.
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
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 220-CLREA-SEC
      *        連携解除へ
               WHEN    "CLICKED"       ALSO    "B04"
                    PERFORM 440-KAIJO-SYORI-SEC
      *        一覧へ
               WHEN    "CLICKED"       ALSO    "B10"
                    PERFORM 4100-K0241-SYORI-SEC
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
      *        選択番号
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 410-SELNUM-CHK-SEC
      *        診療日選択
               WHEN    ANY             ALSO    "KANRILIST"
                   PERFORM 420-LIST-CHK-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日選択番号入力チェック処理
      *****************************************************************
       410-SELNUM-CHK-SEC              SECTION.
      *
           MOVE    1                   TO  SPA-K024-CUR
      *
           MOVE    K024-SELNUM         TO  SPA-K024-SELNUM
      *
           IF      SPA-K024-SELNUM     <   01   OR
                                       >   SPA-K024-MAX
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               PERFORM 4101-MEISAI-HENSYU-SEC
               MOVE    2                   TO  SPA-K024-CUR
           END-IF
      *
           .
       410-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日選択処理
      *****************************************************************
       420-LIST-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-K024-MAX
              IF      K024-KANRILIST-SELECT (IDX)
                                           =  "T"
                  IF      IDX              =   SPA-K024-SELNUM
                      CONTINUE
                  ELSE
                      MOVE    SPA-K024G-TNUM (IDX)     TO  WRK-SELNUM
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               MOVE    WRK-SELNUM          TO  SPA-K024-SELNUM
           END-IF
      *
           IF      SPA-K024-SELNUM     <   01   OR
                                       >   SPA-K024-MAX
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               PERFORM 4101-MEISAI-HENSYU-SEC
               MOVE    2                   TO  SPA-K024-CUR
           END-IF
      *
           .
       420-LIST-CHK-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       4101-MEISAI-HENSYU-SEC          SECTION.
      *
           INITIALIZE                  WRK-MEISAI-AREA
           INITIALIZE                  WRK-DATA-AREA
           MOVE    ZERO                TO  IDXG
           MOVE    ZERO                TO  IDX-C
           MOVE    ZERO                TO  WRK-CNT 
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-K024-MAX
              IF      SPA-K024-SELNUM      =   SPA-K024G-TNUM(IDX)
                  MOVE    IDX                  TO  IDX-C
                  MOVE    SPA-K024-MAX         TO  IDX
              END-IF
           END-PERFORM
           IF      IDX-C               =   ZERO
               GO      TO      4101-MEISAI-HENSYU-EXT
           END-IF
      *
           INITIALIZE                  OKUSURIDATA-REC
           MOVE    SPA-K024-DATA-KEY (IDX-C)
                                       TO  OKUDAT-DATA-KEY
      *
           MOVE    OKUSURIDATA-REC     TO  MCPDATA-REC
           MOVE    "tbl_okusuri_data"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_okusuri_data"  TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-OKUSURIDATA-READ-SEC
           ELSE
               INITIALIZE                  OKUSURIKANRI-REC
               MOVE    1               TO  FLG-OKUDAT
           END-IF
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  WRK-KEY-RP
           PERFORM
               UNTIL   (FLG-OKUDAT     =   1   )
      *
               IF      (OKUDAT-RP  NOT =   WRK-KEY-RP )
                   AND (IDX                 >   1     )
                   PERFORM 41012-GMN-HENSYU-SEC
                   MOVE    ZERO            TO  IDX
                   INITIALIZE                  WRK-DATA-AREA
               END-IF
      *
               PERFORM 41011-OKUSURIDATA-HENSYU-SEC
      *
               MOVE    OKUDAT-RP           TO  WRK-KEY-RP
      *
               MOVE    "tbl_okusuri_data"  TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-OKUSURIDATA-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_okusuri_data"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX                 >   1
               PERFORM 41012-GMN-HENSYU-SEC
           END-IF
           .
       4101-MEISAI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       41012-GMN-HENSYU-SEC            SECTION.
      *
      *
           PERFORM VARYING     IDX     FROM    1    BY  1
                   UNTIL      (IDX     >   500  )
                          OR  (WRK-DAT-DATA(IDX)    =   SPACE)
                          OR  (IDXG    >=  500 )
               ADD     1                   TO  IDXG
               MOVE    WRK-DAT-NUM  (IDX)  TO  WRK-MEI-NUM  (IDXG)
               MOVE    WRK-DAT-DATA (IDX)  TO  WRK-MEI-DATA (IDXG)
               ADD     1                   TO  WRK-MEI-MAX
           END-PERFORM
      *    連番毎
           IF      (IDXG               <   500  )
               ADD     1                   TO  IDXG
               MOVE    ALL "-"             TO  WRK-MEI-DATA (IDXG)
               MOVE    ALL "-"             TO  WRK-MEI-NUM  (IDXG)
               ADD     1                   TO  WRK-MEI-MAX
           END-IF
           .
       41012-GMN-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       41011-OKUSURIDATA-HENSYU-SEC    SECTION.
      *
           ADD     1                   TO  IDX
           IF      IDX                 >   500
               GO      TO      41011-OKUSURIDATA-HENSYU-EXT
           END-IF
      *    １件目
           IF      IDX                  =   1
               ADD     1                TO  WRK-CNT 
               MOVE    WRK-CNT          TO  WRK-Z9
               MOVE    WRK-Z9           TO  WRK-DAT-NUM  (IDX)
           END-IF
      *
      *    名称
           MOVE    ZERO                TO  IDX-MAX
           PERFORM VARYING     IDM     FROM    1    BY  1
                   UNTIL      (IDM     >   200  )
                          OR  (OKUDAT-NAME(IDM:1)  =   SPACE)
               MOVE    IDM                 TO  IDX-MAX
           END-PERFORM
           MOVE    OKUDAT-NAME         TO  WRK-DAT-DATA(IDX)
           IF      IDX-MAX             >   80
               ADD     1                   TO  IDX
               IF      IDX                 >   500
                   GO      TO      41011-OKUSURIDATA-HENSYU-EXT
               END-IF
               MOVE    OKUDAT-NAME(81:)   TO  WRK-DAT-DATA (IDX)
           END-IF
      *
      *    薬品レコード
           IF      OKUDAT-DATA-CLASS   =   "201"
      *        数量変換
               MOVE    OKUDAT-SURYO        TO  WRK-SURYO
               PERFORM 310121-SURYO-HEN-SEC
               IF      FLG-KETA5           =   1
                   IF      WRK-DAT-SURYO2(IDX) NOT =   SPACE
                       ADD     1               TO  IDX
                       IF      IDX                 >   500
                           GO      TO  41011-OKUSURIDATA-HENSYU-EXT
                       END-IF
                   END-IF
                   MOVE    WRK-SURYO-J-G   TO  WRK-DAT-SURYO2(IDX)
                   MOVE    OKUDAT-TANINAME TO  WRK-DAT-TANNI2 (IDX)
               ELSE
                   IF      WRK-DAT-SURYO (IDX) NOT =   SPACE
                       ADD     1               TO  IDX
                       IF      IDX                 >   500
                           GO      TO  41011-OKUSURIDATA-HENSYU-EXT
                       END-IF
                   END-IF
                   MOVE    WRK-SURYO-J    TO  WRK-DAT-SURYO (IDX)
                   MOVE    OKUDAT-TANINAME    TO  WRK-DAT-TANNI (IDX)
               END-IF
           END-IF
      *    用法レコード
           IF      OKUDAT-DATA-CLASS   =   "301"
               IF      WRK-DAT-KAISU (IDX) NOT =   SPACE
                   ADD     1                   TO  IDX
                   IF      IDX                 >   500
                       GO      TO  41011-OKUSURIDATA-HENSYU-EXT
                   END-IF
               END-IF
      *        回数変換
               MOVE    OKUDAT-SURYO        TO  WRK-Z9
      *        日本語変換
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI
               MOVE    WRK-Z9              TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT   TO  WRK-DAT-KAISU (IDX)
               MOVE    OKUDAT-TANINAME     TO  WRK-DAT-TANNI3(IDX)
           END-IF
      *
           .
       41011-OKUSURIDATA-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    数量表示用編集処理
      *****************************************************************
       310121-SURYO-HEN-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
           MOVE    ZERO                TO  FLG-SP
           MOVE    ZERO                TO  FLG-KETA5
           PERFORM VARYING     IDM     FROM    5  BY  -1
                   UNTIL       IDM     <   1
               IF      WRK-SURYO-S2(IDM:1) NOT =   ZERO
                   MOVE    1               TO  FLG-SP
                   IF      IDM             >   3
                       MOVE    1               TO  FLG-KETA5
                   END-IF
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDM:1) TO  WRK-SURYO-Z3(IDM:1)
               END-IF
           END-PERFORM
      *
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
               MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1-9
           END-IF
      *    小数部３桁以上の時、前の空白削除
           IF      FLG-KETA5           =   1
               IF      WRK-SURYO-Z1(1:2)  =   SPACE
                   MOVE    WRK-SURYO-X(3:9)   TO  WRK-SURYO-X2
                   MOVE    SPACE              TO  WRK-SURYO-X
                   MOVE    WRK-SURYO-X2       TO  WRK-SURYO-X
                   MOVE    ZERO               TO  FLG-KETA5
               END-IF
           END-IF
      *    日本語変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-SURYO-X         TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      FLG-KETA5           =   0
               MOVE    KANACHK-OUT-INPUT   TO  WRK-SURYO-J
               INSPECT WRK-SURYO-J         REPLACING
                                           ALL "Ｘ"  BY  "×"
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  WRK-SURYO-J-G
               INSPECT WRK-SURYO-J-G       REPLACING
                                           ALL "Ｘ"  BY  "×"
           END-IF
      *
           .
       310121-SURYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    更新　処理
      *****************************************************************
       440-KAIJO-SYORI-SEC           SECTION.
      *
           IF      SPA-K024-SELNUM     >=  01     AND
                                       <=  SPA-K024-MAX
               MOVE    "0101"              TO  SPA-KIDCD
               PERFORM 4101-MEISAI-HENSYU-SEC
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-K024-CUR
           END-IF
           .
       440-KAIJO-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    連携解除処理
      *****************************************************************
       4401-KAIJO-UPDATE-SEC             SECTION.
      *
           MOVE    SPA-K024-SELNUM     TO  IDX
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                  OKUSURIKANRI-REC
           MOVE    SPA-HOSPNUM         TO  OKUKAN-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  OKUKAN-PTID
           MOVE    SPA-K024-TSRYYMD(IDX)
                                       TO  OKUKAN-SRYYMD
           MOVE    SPA-K024-THOSPCD(IDX)
                                       TO  OKUKAN-HOSPCD
           MOVE    SPA-K024-TRENNUM(IDX)
                                       TO  OKUKAN-RENNUM
      *
           MOVE    OKUSURIKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_okusuri_kanri" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_okusuri_kanri" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-OKUSURIKANRI-READ-SEC
           ELSE
               INITIALIZE                  OKUSURIKANRI-REC
               MOVE    1               TO  FLG-OKUKAN
           END-IF
           MOVE    "tbl_okusuri_kanri" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-OKUKAN          =   1
               MOVE    "0010"              TO  SPA-ERRCD
               GO      TO   4401-KAIJO-UPDATE-EXT
           END-IF
      *
           MOVE    OKUSURIKANRI-REC    TO  HZN-OKUSURIKANRI-REC
      *    削除
           MOVE    OKUSURIKANRI-REC    TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_okusuri_kanri" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "0010"              TO  SPA-ERRCD
               DISPLAY "K024  DEL ERR:"  MCP-RC
                  ",KEY:"  OKUKAN-KEY
               GO      TO   4401-KAIJO-UPDATE-EXT
           END-IF
      *    最終連番取得
           PERFORM 41202-KANRI-RENNUM-SEC
      *
      *    患者ＩＤをゼロへ
           MOVE    HZN-OKUSURIKANRI-REC    TO  OKUSURIKANRI-REC
           MOVE    ZERO                TO  OKUKAN-PTID
           ADD     1                   TO  WRK-RENNUM
           MOVE    WRK-RENNUM          TO  OKUKAN-RENNUM
      *
           MOVE    SMCNDATE-YMD        TO  OKUKAN-CREYMD
           MOVE    SMCNDATE-YMD        TO  OKUKAN-UPYMD
           MOVE    SMCNDATE-HMS        TO  OKUKAN-UPHMS
           MOVE    SPA-OPID            TO  OKUKAN-OPID
      *
           MOVE    OKUSURIKANRI-REC    TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_okusuri_kanri" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "0010"              TO  SPA-ERRCD
               DISPLAY "K024 INS ERR:"  MCP-RC
                  ",KEY:"  OKUKAN-KEY
               GO      TO   4401-KAIJO-UPDATE-EXT
           END-IF
      *
      *    管理画面編集処理
           PERFORM 3101-KANRILIST-HEN-SEC
      *    未登録分判定
           PERFORM 3102-OKUSURI-KANRI-CHK-SEC
           .
       4401-KAIJO-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    最終連番取得処理
      *****************************************************************
       41202-KANRI-RENNUM-SEC           SECTION.
      *
           INITIALIZE                  OKUSURIKANRI-REC
           MOVE    HZN-OKUSURIKANRI-REC    TO  OKUSURIKANRI-REC
           MOVE    ZERO                TO  OKUKAN-PTID
      *
           MOVE    OKUSURIKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_okusuri_kanri" TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_okusuri_kanri" TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-OKUSURIKANRI-READ-SEC
           ELSE
               INITIALIZE                  OKUSURIKANRI-REC
               MOVE    1               TO  FLG-OKUKAN
           END-IF
           MOVE    "tbl_okusuri_kanri" TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    OKUKAN-RENNUM       TO  WRK-RENNUM
           .
       41202-KANRI-RENNUM-EXT.
           EXIT.
      *****************************************************************
      *    クリア　処理
      *****************************************************************
       220-CLREA-SEC                    SECTION.
      *
           MOVE    ZERO                TO  SPA-K024-SELNUM
           MOVE    1                   TO  SPA-K024-CUR
           .
       220-CLREA-EXT.
           EXIT.
      *****************************************************************
      *    一覧へ　処理
      *****************************************************************
       4100-K0241-SYORI-SEC            SECTION.
      *
      *    初期画面セット
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K02             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK0241"         USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           MOVE    "K0241"             TO  SPA-SAKIPG
           MOVE    "K024"              TO  SPA-MOTOPG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K0241"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       4100-K0241-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       460-MAE-PAGE-SEC            SECTION.
      *
           IF     SPA-K024-PAGE        >   1
              COMPUTE SPA-K024-PAGE    =   SPA-K024-PAGE
                                       -   1
               PERFORM 3101-KANRILIST-HEN-SEC
           END-IF
           .
       460-MAE-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       470-ATO-PAGE-SEC             SECTION.
      *
           IF     SPA-K024-NEXT        =   1
              COMPUTE SPA-K024-PAGE    =   SPA-K024-PAGE
                                       +   1
               PERFORM 3101-KANRILIST-HEN-SEC
           END-IF
           .
       470-ATO-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
           MOVE    "K024"              TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       520-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       520-SEIWA-HEN-EXT.
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
           IF      SPA-KIDCD       NOT =   SPACE
               PERFORM 520-KIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "K024"              TO  MCP-WINDOW.
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
           IF     (FLG-GMN-INIT        =   1   )  OR
                  (SPA-K024-MAX         =   ZERO)
               MOVE    ZERO                TO  WRK-K024-ROW
           ELSE
               MOVE    K024-KANRILIST-ROW  TO  WRK-K024-ROW
           END-IF
      *
      *
           MOVE    SPACE               TO  K024
           INITIALIZE                      K024
      *
      *    コラムリストの位置指定
      *    MOVE    ZERO                TO  K024-ROW
      *    MOVE    ZERO                TO  K024-ROWATTR
           IF      WRK-K024-ROW     NOT =   ZERO
               MOVE    WRK-K024-ROW         TO  K024-KANRILIST-ROW
           END-IF
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-K024-MAX
               MOVE    SPA-K024G-TNUM (IDX)    TO  WRK-ZZ
               MOVE    WRK-ZZ                  TO  K024-TNUM (IDX)
               MOVE    SPA-K024G-TSRYYMD(IDX)
                                               TO  K024-TTOUYYMD (IDX)
               MOVE    SPA-K024G-THOSPNAME(IDX)
                                               TO  K024-THOSPNAME(IDX)
               IF      SPA-K024G-TNUM(IDX)     =   SPA-K024-SELNUM
                   MOVE    "T"             TO  K024-KANRILIST-SELECT
                                                              (IDX)
               ELSE
                   MOVE    "F"             TO   K024-KANRILIST-SELECT
                                                              (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-K024-MAX         TO  K024-KANRILIST-COUNT
      *
           MOVE    SPA-K024-SELNUM      TO  K024-SELNUM
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   WRK-MEI-MAX
               IF      WRK-MEI-NUM (IDX)   =   "-"
                   MOVE    ALL "-"             TO  K024-TBANGO(IDX)
                   MOVE    ALL "―"            TO  K024-TMEISAI(IDX)
               ELSE
                   MOVE    WRK-MEI-NUM (IDX)   TO  K024-TBANGO(IDX)
                   MOVE    WRK-MEI-DATA(IDX)   TO  K024-TMEISAI(IDX)
               END-IF
               MOVE    IDX                 TO  K024-MEILIST-COUNT
           END-PERFORM
      *
           IF      SPA-K024-OKSR-ARI   =   1
               MOVE    "＜未設定対象あり＞"
                                       TO  K024-MIDASI-TEXTDATA
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
           EVALUATE    SPA-K024-CUR
               WHEN    01
                   MOVE    "SELNUM"          TO  MCP-WIDGET
               WHEN    02
                   MOVE    "B01"             TO  MCP-WIDGET
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
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "番号選択入力エラー"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KERR
           MOVE    SPA-ERRCD           TO  KERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  KERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K024"               TO  SPA-MOTOPG
           MOVE    "KERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-KIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-KIDCD
               WHEN    "0101"
                   MOVE
                   "対象のデータを解除します。よろしいですか？"
                                       TO  WRK-KIDMSG
               WHEN    OTHER
                   MOVE    SPA-KIDCD
                                       TO  WRK-KIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KID1
           INITIALIZE                      KID1
           MOVE    SPA-KIDCD           TO  KID1-ID1CODE
           MOVE    WRK-KIDMSG          TO  KID1-ID1MSG
      *
           MOVE    "戻る"              TO  KID1-B01
           MOVE    "ＯＫ"              TO  KID1-B12
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "K024"              TO  SPA-MOTOPG
           MOVE    "KID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-KIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    お薬手帳データ連携　管理マスター読込
      *****************************************************************
       910-OKUSURIKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  OKUSURIKANRI-REC
               MOVE    ZERO            TO  FLG-OKUKAN
           ELSE
               INITIALIZE                  OKUSURIKANRI-REC
               MOVE    1               TO  FLG-OKUKAN
           END-IF
      *
           .
       910-OKUSURIKANRI-EXT.
           EXIT.
      *****************************************************************
      *    お薬手帳データ連携読込
      *****************************************************************
       910-OKUSURIDATA-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  OKUSURIDATA-REC
               MOVE    ZERO            TO  FLG-OKUDAT
           ELSE
               INITIALIZE                  OKUSURIDATA-REC
               MOVE    1               TO  FLG-OKUDAT
           END-IF
      *
           .
       910-OKUSURIDATA-EXT.
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
      *
      *****************************************************************
      *    パラメタデータ順読み
      *****************************************************************
      *980-PARA-READ-SEC         SECTION.
      *
      *    READ    PARA-FILE 
      *        AT  END
      *            MOVE    1           TO  FLG-PARA
      *        NOT AT  END
      *            MOVE    ZERO        TO  FLG-PARA
      *    END-READ
      *
      *    .
      *980-PARA-READ-EXT.
      *    EXIT.
      *
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
