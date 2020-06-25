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
       PROGRAM-ID.             ORCGM95.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 業務選択
      *  コンポーネント名  : 日付セット（Ｍ９５）
      *  管理者            : 
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.01.01    MCC-多々納   01/09/20  JOINの修正      
      *  01.01.02    MCC-藤原     01/09/27  MVERの追加  
      *  01.02.01    MCC-藤原     01/11/21  ORCMCPSUBへ変更  
      *  01.02.02    MCC-多々納   02/02/22  MID1の追加
      *  01.02.03    NACL-多々納  04/02/02  外来時間外区分追加
      *  01.02.04    NACL-伊藤    04/02/05  M97の追加
      *  01.02.05    NACL-伊藤    06/02/22  MONFUNC 対応
      *  02.00.00    NACL-多々納  06/08/09  担当医設定追加
      *  04.04.00    NACL-森脇    09/01/30  他院切替機能追加、MID2の追加
      *  04.05.00    NACL-多々納  09/07/15  小児科特例時間外区分追加
      *  04.07.00    NACL-多々納  12/01/26  背景色変更追加
      *  04.07.00    NACL-多々納  12/05/28  タブラベル対応
      *  04.08.00    NACL-多々納  18/03/XX  平成３０年４月改正対応
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
      *    スパ領域
           COPY    "COMMON-SPA".
      *    スパ領域
           COPY    "M00COMMON-SPA".
      *
      *    Ｍ００スパ領域
      **** COPY    "M95SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *
      *    担当医設定画面、他院切替画面
       01  SPA-M95.
           03  SPA-M95PAGE2-AREA.
               05  SPA-M95GMN2-AREA.
                   07  SPA-ACCTSYSYMD          PIC X(08).
      *
                   07  SPA-M95GMN-SYORIYMD     PIC X(10).
                   07  SPA-M95NAI-SYORIYMD     PIC X(08).
                   07  SPA-M95GMN-LIST-G.
                       09  SPA-M95GMN-LIST      OCCURS   100.
                           11  SPA-M95GMN-TSRYKA    PIC X(02).
                           11  SPA-M95GMN-TSRYKAMEI PIC X(20).
                           11  SPA-M95GMN-TRENNUM   PIC X(04).
                           11  SPA-M95GMN-TDRCD-G.
                               13  SPA-M95GMN-TDRCD     PIC X(04).
                               13  SPA-M95GMN-TF1       PIC X(01).
                               13  SPA-M95GMN-TDRCDMEI  PIC X(50).
      *
                           11  SPA-M95NAI-TSRYKA    PIC X(02).
                       09  SPA-M95GMN-MAX           PIC 9(03).
      *            入力
                   07  SPA-M95GMN-IN-AREA.
                       09  SPA-M95GMN-SRYKA         PIC X(02).
                       09  SPA-M95GMN-DRCDLST-G.
                           11  SPA-M95GMN-DRCDLST     OCCURS  99.
                               13  SPA-M95GMN-DRCDL       PIC X(04).
                               13  SPA-M95GMN-F1L         PIC X(01).
                               13  SPA-M95GMN-DRCDMEIL    PIC X(24).
                       09  SPA-M95GMN-DRCD-MAX            PIC 9(04).
                       09  SPA-MAE-SRYKA        PIC X(02).
                       09  SPA-M95GMN-SRYKAMEI  PIC X(20).
                       09  SPA-M95-KOUSIN       PIC 9(01).
      *H29.10
                       09  SPA-M95-DELONL       PIC 9(01).
      *
                       09  SPA-M95GMN-INPUT-G.
                           11  SPA-M95GMN-RENNUM       PIC X(04).
                           11  SPA-M95GMN-DRCD-G.
                               13  SPA-M95GMN-DRCD     PIC X(04).
                               13  SPA-M95GMN-DRCDF1   PIC X(01).
                               13  SPA-M95GMN-DRCDMEI  PIC X(24).
      *        診療科
               05  SPA-NAI-SRYKA-LST.
                   07  SPA-NAI-SRYKA-OCC      OCCURS   40.
                       09  SPA-NAI-SRYKA      PIC X(02).
                       09  SPA-NAI-SRYKAMEI   PIC X(20).
                   07  SPA-SRYKA-MAX          PIC 9(03).
           03  SPA-M95PAGE3-AREA.
               05  SPA-M95GMN-TAINLIST-G.
                   07  SPA-M95GMN-TAINLIST    OCCURS   9.
                       09  SPA-M95GMN-TAINNO    PIC X(01).
                       09  SPA-M95GMN-TAINNAME-G.
                           11  SPA-M95GMN-TAINCD   PIC 9(2).
                           11  SPA-M95GMN-TAINF1   PIC X(01).
                           11  SPA-M95GMN-TAINNAME PIC X(10).
                   07  SPA-M95GMN-TAIN-MAX      PIC 9(01).
               05  SPA-M95GMN-TAINNUM         PIC X(01).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-HEN             PIC 9(01).
           03  FLG-INITAUTO        PIC 9(01).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDX2                    PIC 9(04).
           03  IDX3                    PIC 9(04).
           03  IDK                     PIC 9(04).
           03  IDK2                    PIC 9(04).
           03  IDX-S                   PIC 9(04).
           03  IDX-T                   PIC 9(04).
           03  IDX-H                   PIC 9(04).
           03  IDX-D                   PIC 9(04).
      *
           03  IDJ                     PIC 9(01).
      *
      *    一時領域
       01  WRK-AREA.
      *    選択日付
           03  WRK-CAL-YMD.
               05  WRK-CAL-YEAR        PIC 9(04).
               05  WRK-CAL-MONTH       PIC 9(02).
               05  WRK-CAL-DAY         PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-WSYSYMDH          PIC X(09).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
      *
           03  WRK-NUM-1-X.
               05  WRK-NUM-1           PIC 9(01).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKAMEI            PIC X(20).
           03  WRK-RENNUM              PIC X(04).
           03  WRK-DRCD-G.
               05  WRK-DRCD            PIC X(04).
               05  WRK-DRCD-F1         PIC X(01).
               05  WRK-DRCDMEI         PIC X(24).
           03  WRK-SYU-DRCD            PIC X(05).
           03  WRK-MCP-WIDGET          PIC X(10).
           03  WRK-PAGENO              PIC 9(03).
           03  WRK-YAKANKBN            PIC 9(01).
           03  WRK-JIKANTOKU-NYU       PIC 9(01).
      *
           03  WRK-HOSPNUM             PIC 9(02).
           03  WRK-GRPHOSPUSERID       PIC X(64).
           03  WRK-MOTOPG              PIC X(08).
      *
           03  TBL-DRCD-AREA.
               05  TBL-DRCD-OCC        OCCURS   300.
                    07  TBL-DRCD           PIC X(05).
                    07  TBL-DRCDNAME       PIC X(50).
                    07  TBL-SENMONKACD-G.
                        09  TBL-SENMONKACD     PIC X(02)
                                                 OCCURS   5.
      *    SORT用
           03  WRK-SORT-AREA.
               07  WRK-M95GMN-LIST-G.
                   09  WRK-M95GMN-LIST      OCCURS   100.
                       11  WRK-M95GMN-TSRYKA    PIC X(02).
                       11  WRK-M95GMN-TSRYKAMEI PIC X(20).
                       11  WRK-M95GMN-TRENNUM   PIC X(04).
                       11  WRK-M95GMN-TDRCD-G.
                               13  WRK-M95GMN-TDRCD     PIC X(04).
                               13  WRK-M95GMN-TF1       PIC X(01).
                               13  WRK-M95GMN-TDRCDMEI  PIC X(50).
      *
                       11  WRK-M95NAI-TSRYKA    PIC X(02).
                   09  WRK-M95GMN-MAX           PIC 9(03).
      *
      *    コラムリスト位置
           03  WRK-M95-SYSLIST-ROW     PIC S9(09).
           03  FLG-GMN-INIT            PIC 9(01).
      *
      *    担当医一覧ＭＡＸ
       01  COM-TANMAX              PIC 9(03)  VALUE   100.
      *
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *    診療科情報
           COPY  "CPSK1005.INC".
      *    担当医設定 
           COPY  "CPSK1011.INC".
      *    施設規準 
           COPY  "CPSK1006.INC".
      *    チェック機能制御情報
           COPY  "CPSK1008.INC".
      *    診療行為一括入金管理情報
           COPY  "CPSK1038.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ドクター編集変換サブ
           COPY    "CPORCSDRSET.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *    COMMON-SPA 初期編集
           COPY    "CPORCSCOMMON.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *****COPY    "MCPAREA.INC".
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "M00.INC".
           COPY    "M01.INC".
           COPY    "M01N.INC".
           COPY    "M02.INC". 
           COPY    "M95.INC".
           COPY    "MERR.INC".
           COPY    "MID1.INC".
           COPY    "MVER.INC".
           COPY    "M98.INC".
           COPY    "MUID.INC".
           COPY    "M99.INC".
           COPY    "MID2.INC".
           COPY    "MDAS.INC".
           COPY    "MLOG.INC".
           COPY    "MINF.INC".
           COPY    "MPRT.INC".
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-M00KYOUTU
           MOVE    SPA-FREE        TO  SPA-M95
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO MCP-EVENT
               WHEN    "LINK"      ALSO ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"      ALSO "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-M00KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-M95             TO  SPA-FREE
      *
           .
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "MERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END         =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
      *     IF     (SPA-ERRCD           =   SPACE )  OR
      *            (FLG-END             =   ZERO  )
               MOVE    SPACE               TO  LINKAREA
               MOVE    "NEW"               TO  MCP-PUTTYPE
               MOVE    "M95    "           TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
      *     END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "MID1"
      *            確認画面より
                   PERFORM 30010-MID1-SET-SEC
               WHEN    "MID2"
      *            確認画面より
                   PERFORM 30010-MID2-SET-SEC
               WHEN    OTHER
                   MOVE    SPACE               TO  SPA-MIDCD
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
           INITIALIZE                  SPA-M95GMN-AREA
      *
           MOVE    SPA-SYSYMD      TO  SPA-NAI-SYSYMD
           MOVE    SPA-SYSYMDW     TO  SPA-NAI-SYSYMDW
           MOVE    SPA-SYSYMDWH    TO  SPA-GMN-SYSYMD
      *
      *    時間外区分
           MOVE    SPA-GAI-JIKANKBN    TO  SPA-GMN-JIKANKBN
           PERFORM 3101-JIKANGAI-SET-SEC
      *
           MOVE    1               TO  SPA-M95-CUR
           MOVE    1               TO  SPA-M95-PAGE
      *
      *    担当医設定画面
           INITIALIZE                  SPA-M95PAGE2-AREA
      *    他院切替画面
           INITIALIZE                  SPA-M95PAGE3-AREA
      *ver.4.7
      *    背景色設定
           PERFORM 3102-BGCOLORS-INIT-SEC
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    背景色設定処理
      *****************************************************************
       3102-BGCOLORS-INIT-SEC       SECTION.
      *
      *    システム日付決定
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD        TO  SPA-ACCTSYSYMD
      *
           MOVE    SPACE               TO  SYS-1038-REC
           INITIALIZE                      SYS-1038-REC
           MOVE    "1038"              TO  SYS-1038-KANRICD
           MOVE    "*"                 TO  SYS-1038-KBNCD
           MOVE    SPA-ACCTSYSYMD      TO  SYS-1038-STYUKYMD
           MOVE    SPA-ACCTSYSYMD      TO  SYS-1038-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1038-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1038-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    SYSKANRI-REC        TO  SYS-1038-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1038-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
      *        背景色設定
               PERFORM 31021-BGCOLORS-HEN-SEC
           END-IF
           .
       3102-BGCOLORS-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    背景色設定処理
      *****************************************************************
       31021-BGCOLORS-HEN-SEC       SECTION.
      *
      *    時間内（標準
           MOVE    SYS-1038-BGCOLORSJ0 TO  SPA-M95-BGCOLORS
      *    日付変更
           IF      SPA-ACCTSYSYMD  NOT =   SPA-NAI-SYSYMD
               MOVE    SYS-1038-BGCOLORSDAY    TO  SPA-M95-BGCOLORS
               GO      TO      31021-BGCOLORS-HEN-EXT
           END-IF
      *    時間外
           IF      SPA-GMN-JIKANKBN    NOT =   "0"
               MOVE    SPA-GMN-JIKANKBN        TO  IDJ
               IF     (IDJ                 >=  1   AND   <=  8 )
                   MOVE    SYS-1038-BGCOLORSJ (IDJ)
                                               TO  SPA-M95-BGCOLORS
               END-IF
           END-IF
           .
       31021-BGCOLORS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＭＩＤ１）ＯＫ処理
      *****************************************************************
       3101-JIKANGAI-SET-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-YAKANKBN
           MOVE    ZERO                TO  WRK-JIKANTOKU-NYU
           IF     (SPA-NAI-SYSYMD      >=  "20080401" )
             AND  (SPA-HOSPSBT         =   2          )
      *        夜間・早朝加算施設規準
               PERFORM 31011-KIJUN-CHK-SEC
           END-IF
      *    時間外加算特例乳幼児
           IF     (SPA-NAI-SYSYMD      >=  "20080401" )
               PERFORM 31012-JIKANTOKU-CHK-SEC
           END-IF
      *    時間外区分
           MOVE    "0 時間内"          TO  SPA-GMN-JIKANKBN-LST(1)
           MOVE    "1 時間外"          TO  SPA-GMN-JIKANKBN-LST(2)
           MOVE    "2 休日"            TO  SPA-GMN-JIKANKBN-LST(3)
           MOVE    "3 深夜"            TO  SPA-GMN-JIKANKBN-LST(4)
           MOVE    "4 時間外特例"      TO  SPA-GMN-JIKANKBN-LST(5)
           MOVE    5                   TO  SPA-JIKANKBN-MAX
      *
           IF      WRK-JIKANTOKU-NYU   =   1
                                       OR  2
               MOVE    "5 小児科特例夜間"  TO  SPA-GMN-JIKANKBN-LST(6)
               MOVE    "6 小児科特例休日"  TO  SPA-GMN-JIKANKBN-LST(7)
               MOVE    "7 小児科特例深夜"  TO  SPA-GMN-JIKANKBN-LST(8)
               ADD     3                   TO  SPA-JIKANKBN-MAX
           END-IF
      *
      *H30.4
      *    産科・産婦人科
           IF     (SPA-NAI-SYSYMD      >=  "20180401" )
               IF      WRK-JIKANTOKU-NYU   =   1
                                           OR  2
                                           OR  3
                   MOVE    "5 小児・産・産婦人科特例夜間"
                                           TO  SPA-GMN-JIKANKBN-LST(6)
                   MOVE    "6 小児・産・産婦人科特例休日"
                                           TO  SPA-GMN-JIKANKBN-LST(7)
                   MOVE    "7 小児・産・産婦人科特例深夜"
                                           TO  SPA-GMN-JIKANKBN-LST(8)
                   MOVE    8               TO  SPA-JIKANKBN-MAX
               END-IF
           END-IF
      *
           IF      WRK-YAKANKBN        =   1
               ADD     1                   TO  SPA-JIKANKBN-MAX
               MOVE    "8 夜間・早朝"      TO  SPA-GMN-JIKANKBN-LST
                                                    (SPA-JIKANKBN-MAX)
           END-IF
           IF      FLG-INITAUTO        =   1
               MOVE    1               TO  SPA-JIKANKBN-MAX
           END-IF
      *
           MOVE    SPACE               TO  SPA-GMN-JIKANKBN-MEI
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-JIKANKBN-MAX
               IF      SPA-GMN-JIKANKBN    =   SPA-GMN-JIKANKBN-LST
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-JIKANKBN-LST (IDX)
                                           TO  SPA-GMN-JIKANKBN-G
                   MOVE   SPA-JIKANKBN-MAX TO  IDX
               END-IF
           END-PERFORM
           .
       3101-JIKANGAI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    夜間・早朝加算施設規準処理
      *****************************************************************
       31011-KIJUN-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  SYS-1006-REC
           INITIALIZE                      SYS-1006-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1006-HOSPNUM
           MOVE    "1006"              TO  SYS-1006-KANRICD
           MOVE    SPA-NAI-SYSYMD      TO  SYS-1006-STYUKYMD
           MOVE    SPA-NAI-SYSYMD      TO  SYS-1006-EDYUKYMD
      *    システム管理検索（施設基準）
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1006-REC
               INITIALIZE                  SYS-1006-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM UNTIL   FLG-SYSKANRI    =   1
               MOVE    SYSKANRI-REC        TO  SYS-1006-REC
               IF      SYS-1006-KBNCD      =   "2"
      *            夜間・早朝加算
                   IF      SYS-1006-SSTKIJUN (172) =   1
                       MOVE    1               TO  WRK-YAKANKBN
                   ELSE
                       MOVE    0               TO  WRK-YAKANKBN
                   END-IF
               END-IF
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       31011-KIJUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    時間外加算特例乳幼児処理
      *****************************************************************
       31012-JIKANTOKU-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  WRK-JIKANTOKU-NYU
      *
           MOVE    SPACE               TO  SYS-1008-REC
           INITIALIZE                      SYS-1008-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1008-HOSPNUM
           MOVE    "1008"              TO  SYS-1008-KANRICD
           MOVE    SPA-NAI-SYSYMD      TO  SYS-1008-STYUKYMD
           MOVE    SPA-NAI-SYSYMD      TO  SYS-1008-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1008-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1008-REC
               INITIALIZE                  SYS-1008-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM UNTIL   FLG-SYSKANRI    =   1
               MOVE    SYSKANRI-REC        TO  SYS-1008-REC
      *        時間外加算特例乳幼児
               IF      SYS-1008-JIKANTOKU-NYU-FLG  =   SPACE
                   MOVE    ZERO            TO  WRK-JIKANTOKU-NYU
               ELSE
                   MOVE    SYS-1008-JIKANTOKU-NYU-FLG
                                           TO  WRK-JIKANTOKU-NYU
               END-IF
      *        診察料自動発生なし
               IF      SYS-1008-INITAUTO-FLG   =   "0"
                   MOVE    1               TO  FLG-INITAUTO
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       31012-JIKANTOKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＭＩＤ１）ＯＫ処理
      *****************************************************************
       30010-MID1-SET-SEC          SECTION.
      *
           EVALUATE    SPA-MIDCD
               WHEN    "0102"
      *            削除処理
                   IF      SPA-MID1-FLG        =   "OK"
                       PERFORM 2021-DELETE-SYORI-SEC
                   END-IF
                   MOVE    10              TO  SPA-M95-CUR
               WHEN    "0103"
      *            更新後、全体
                   IF      SPA-MID1-FLG        =   "OK"
                       PERFORM 4020-PAGE2-TOROKU-SEC
                   END-IF
                   MOVE    10              TO  SPA-M95-CUR
               WHEN    "0104"
      *            更新後、切替え
                   IF      SPA-MID1-FLG        =   "OK"
                       PERFORM 4020-PAGE2-TOROKU-SEC
                       MOVE    1               TO  SPA-M95-PAGE
                   END-IF
                   MOVE    10              TO  SPA-M95-CUR
               WHEN    "0105"
      *            他院切替え
                   IF      SPA-MID1-FLG        =   "OK"
                       PERFORM 4310-TAIN-SYORI-SEC
                   END-IF
                   MOVE    31              TO  SPA-M95-CUR
                   IF      SPA-MIDCD       =   "0106"
                       MOVE    SPACE           TO  SPA-MID1-FLG
                       PERFORM 520-MIDSET2-SEC
                       GO  TO  30010-MID1-SET-EXT
                   END-IF
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-MID1-FLG
           MOVE    SPACE           TO  SPA-MIDCD
           .
       30010-MID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＭＩＤ２）ＯＫ処理
      *****************************************************************
       30010-MID2-SET-SEC          SECTION.
      *
           MOVE    31              TO  SPA-M95-CUR
           MOVE    SPACE           TO  SPA-MID1-FLG
           MOVE    SPACE           TO  SPA-MIDCD
           .
       30010-MID2-SET-EXT.
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
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 400-TOROKU-SEC
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 202-CLREA-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 202-DELETE-SEC
      *        全体
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 205-HYOUJUN-SEC
      *        更新
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 210-KOUSIN-SEC
      *        タグ切り替え
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 208-BETUGAMEN-SEC
      *        タグ切り替え
      *        WHEN    "CLICKED"       ALSO    "N01"
      *        WHEN    "CLICKED"       ALSO    "N02"
      *        WHEN    "CLICKED"       ALSO    "N03"
      *            PERFORM 430-TAGUCHG-SEC
      *        カレンダー選択
               WHEN    "CLICKED"       ALSO    "C01"
               WHEN    "CLICKED"       ALSO    "CALENDAR1"
                   PERFORM 4102-CALENDAR-SEC
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
               WHEN    "ACTIVATE"      ALSO    "SYSYMD"
                   PERFORM 4101-SYSYMD-SEC
               WHEN    "ACTIVATE"      ALSO    "JIKANKBN"
                   PERFORM 4103-JIKANKBN-SEC
      *        担当医設定画面
               WHEN    "ACTIVATE"      ALSO    "SRYKA"
                   PERFORM 4110-SRYKA-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "DRCD"
                   PERFORM 4120-DRCD-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "RENNUM"
                   PERFORM 4120-RENNUM-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "TAINNUM"
                   PERFORM 4210-TAINNUM-CHK-SEC
               WHEN    ANY             ALSO    "SYSLIST"
                   PERFORM 4130-SYSLIST-SEC
               WHEN    ANY             ALSO    "TAINLIST"
                   PERFORM 4130-TAINLIST-SEC
               WHEN    "SWITCH"       ALSO     "nt1"
                   PERFORM 430-TAGUCHG-SEC
          END-EVALUATE
      *
          .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力　処理
      *****************************************************************
       4101-SYSYMD-SEC            SECTION.
      *
           MOVE    M95-SYSYMD      TO  SPA-GMN-SYSYMD
      *
           .
       4101-SYSYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力　処理
      *****************************************************************
       4102-CALENDAR-SEC            SECTION.
      *
           MOVE    1                   TO  SPA-M95-CUR
      *
      *    画面入力の年月＝ＳＰＡの年月のチェック
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    M95-YEAR            TO  WRK-CAL-YEAR
           MOVE    M95-MONTH           TO  WRK-CAL-MONTH
           MOVE    M95-DAY             TO  WRK-CAL-DAY
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-CAL-YMD         TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"      USING    STS-AREA-DAY
                                           LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
               MOVE    WRK-HENYMDG         TO  SPA-GMN-SYSYMD
               MOVE    WRK-CAL-YMD         TO  SPA-NAI-SYSYMD
               MOVE    3                   TO  SPA-M95-CUR
      *
      *        時間外区分
               MOVE    SPA-GAI-JIKANKBN    TO  SPA-GMN-JIKANKBN
               PERFORM 3101-JIKANGAI-SET-SEC
      *        背景色設定
               PERFORM 3102-BGCOLORS-INIT-SEC
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
           END-IF
      *
           .
       4102-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *     時間外登録  処理
      *****************************************************************
       4103-JIKANKBN-SEC                 SECTION.
      *
           MOVE    M95-JIKANKBN        TO  SPA-GMN-JIKANKBN-G
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-JIKANKBN-MAX
               IF      SPA-GMN-JIKANKBN    =   SPA-GMN-JIKANKBN-LST
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-JIKANKBN-LST (IDX)
                                           TO  SPA-GMN-JIKANKBN-G
                   MOVE    1               TO  FLG-CHK
                   MOVE    SPA-JIKANKBN-MAX    TO  IDX
               END-IF
           END-PERFORM
      *    背景色設定
           IF      FLG-CHK             =   1
               PERFORM 3102-BGCOLORS-INIT-SEC
           END-IF
      *
           IF      FLG-CHK             =   ZERO
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-M95-CUR
           END-IF
      *
           MOVE    3                   TO  SPA-M95-CUR
      *
           .
       4103-JIKANKBN-EXT.
           EXIT.
      *****************************************************************
      *     診療科入力チェック処理
      *****************************************************************
       4110-SRYKA-CHK-SEC              SECTION.
      *
           MOVE    M95-SRYKA           TO  SPA-M95GMN-SRYKA
      *H29.9
      *    数値変換
           INITIALIZE                      ORCSNUMAREA
           MOVE    M95-SRYKA           TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0101"              TO  SPA-ERRCD
               MOVE    10                  TO  SPA-M95-CUR
               GO      TO      4110-SRYKA-CHK-EXT
           ELSE
               MOVE    SNUM-OUTEDT(14:2)   TO  SPA-M95GMN-SRYKA
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
      *
      *H29.10
      *    １桁の診療科登録チェック
           MOVE    ZERO            TO  SPA-M95-DELONL
           IF     (M95-SRYKA       NOT =   SPA-M95GMN-SRYKA)
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-M95GMN-MAX )
                               OR (FLG-CHK     =   1          )
                   IF      M95-SRYKA       =   SPA-M95GMN-TSRYKA
                                                          (IDX)
                       MOVE    SPA-M95GMN-TSRYKAMEI (IDX)
                                               TO  WRK-SRYKAMEI
                       MOVE    IDX             TO  IDX-S
                       MOVE    1               TO  FLG-CHK
                       MOVE    1               TO  SPA-M95-DELONL
                       MOVE    M95-SRYKA       TO  SPA-M95GMN-SRYKA
                   END-IF
               END-PERFORM
           END-IF
      *
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-M95GMN-MAX )
                          OR  (FLG-CHK =   1    )
               IF      SPA-M95GMN-SRYKA    =   SPA-M95GMN-TSRYKA
                                                          (IDX)
                   MOVE    SPA-M95GMN-TSRYKAMEI (IDX)
                                           TO  WRK-SRYKAMEI
                   MOVE    IDX             TO  IDX-S
                   MOVE    1               TO  FLG-CHK
                   MOVE    SPA-M95GMN-MAX  TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1       BY  1
                   UNTIL      (IDX     >   SPA-SRYKA-MAX )
                          OR  (FLG-CHK =   1    )
               IF      SPA-NAI-SRYKA (IDX) =   SPA-M95GMN-SRYKA
                   MOVE    SPA-NAI-SRYKAMEI (IDX)
                                           TO  WRK-SRYKAMEI
                   MOVE    1               TO  FLG-CHK
                   MOVE    IDX             TO  IDX-S
               END-IF
           END-PERFORM
      *
      *
           IF      SPA-M95-SYORI       =   2
               IF      SPA-M95GMN-SRYKA    NOT =   SPA-MAE-SRYKA
                   MOVE    "0106"          TO  SPA-ERRCD
                   MOVE    11              TO  SPA-M95-CUR
                   MOVE    SPA-MAE-SRYKA   TO  SPA-M95GMN-SRYKA
               END-IF
           ELSE
      *
           IF      FLG-CHK         =   ZERO
               MOVE    "0101"          TO  SPA-ERRCD
               MOVE    10              TO  SPA-M95-CUR
               MOVE    SPACE           TO  SPA-MAE-SRYKA
               MOVE    SPACE           TO  SPA-M95GMN-RENNUM
               MOVE    SPACE           TO  SPA-M95GMN-DRCDLST-G
               MOVE    ZERO            TO  SPA-M95GMN-DRCD-MAX
               MOVE    SPACE           TO  SPA-M95GMN-DRCD-G
           ELSE
               IF      SPA-M95GMN-SRYKA    =   SPA-MAE-SRYKA
                   CONTINUE
               ELSE
                   MOVE    SPA-M95GMN-SRYKA    TO  SPA-MAE-SRYKA
      ************ MOVE    SPA-M95GMN-TSRYKAMEI (IDX-S)
                   MOVE    WRK-SRYKAMEI        TO  SPA-M95GMN-SRYKAMEI
      *            担当医一覧表示
                   MOVE    2                   TO  SPA-M95-SYORI
                   PERFORM 42001-SYSLIST-INIT-SEC
      *            ドクターリスト編集
                   MOVE    SPACE               TO  SPA-M95GMN-DRCD-G
                   INITIALIZE                  ORCSDRSETAREA
                   MOVE    SPA-ACCTSYSYMD      TO  ORCSDR-SYSYMD
                   MOVE    "4"                 TO  ORCSDR-KBN
                   MOVE    SPA-M95GMN-SRYKA    TO  ORCSDR-SRYKA
                   MOVE    SPA-M95GMN-DRCD-G   TO  ORCSDR-IN-DRCD-G
                   CALL    "ORCSDRSET"         USING
                                               SPA-AREA
                                               ORCSDRSETAREA
                   MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-M95GMN-DRCDLST-G
                   MOVE    ORCSDR-DRCD-MAX     TO  SPA-M95GMN-DRCD-MAX
                   MOVE    ORCSDR-IN-DRCD-G    TO  SPA-M95GMN-DRCD-G 
      *
                   MOVE    11                  TO  SPA-M95-CUR
      *H29.10
                   IF      SPA-M95-DELONL      =   1
                       MOVE    21                  TO  SPA-M95-CUR
                   END-IF
               END-IF
           END-IF
      *
           END-IF
      *
           .
       4110-SRYKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *     一覧表選択処理
      *****************************************************************
       4130-SYSLIST-SEC                 SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYKA
           MOVE    SPACE               TO  WRK-RENNUM
           MOVE    SPACE               TO  WRK-DRCD-G
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-M95GMN-MAX
              IF      M95-SYSLIST-SELECT (IDX)   =  "T"
                  MOVE    SPA-M95NAI-TSRYKA (IDX)
                                              TO  WRK-SRYKA
                  MOVE    SPA-M95GMN-TRENNUM(IDX)
                                              TO  WRK-RENNUM
                  MOVE    SPA-M95GMN-TDRCD-G(IDX)
                                              TO  WRK-DRCD-G
              END-IF
           END-PERFORM
      *
           IF      SPA-M95-SYORI       =   1
               IF      WRK-SRYKA       NOT =   SPACE
                   MOVE    WRK-SRYKA       TO  M95-SRYKA
                   PERFORM 4110-SRYKA-CHK-SEC
               END-IF
           ELSE
               IF      WRK-RENNUM      NOT =   SPACE
                   MOVE    WRK-RENNUM      TO  M95-RENNUM
                   PERFORM 4120-RENNUM-CHK-SEC
               END-IF
           END-IF
           .
       4130-SYSLIST-EXT.
           EXIT.
      *****************************************************************
      *     一覧表選択処理
      *****************************************************************
       4130-TAINLIST-SEC           SECTION.
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-M95GMN-TAIN-MAX
              IF      M95-TAINLIST-SELECT (IDX)   =  "T"
                  MOVE    SPA-M95GMN-TAINNO (IDX)
                                              TO  SPA-M95GMN-TAINNUM
                  MOVE    SPA-M95GMN-TAIN-MAX TO  IDX
              END-IF
           END-PERFORM
      *
           MOVE    "0105"          TO  SPA-MIDCD
           .
       4130-TAINLIST-EXT.
           EXIT.
      *****************************************************************
      *     診療科入力チェック処理
      *****************************************************************
       4120-RENNUM-CHK-SEC                 SECTION.
      *
      *H29.10
           IF      SPA-M95-DELONL      =   1
               MOVE    21                  TO  SPA-M95-CUR
               MOVE    "0107"              TO  SPA-ERRCD
               MOVE    SPACE               TO  SPA-M95GMN-RENNUM
               GO      TO      4120-RENNUM-CHK-EXT
           END-IF
      *
           MOVE    M95-RENNUM          TO  SPA-M95GMN-RENNUM
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-M95GMN-MAX
               IF      SPA-M95GMN-RENNUM   =   SPA-M95GMN-TRENNUM
                                                          (IDX)
                   MOVE    SPA-M95GMN-TDRCD-G(IDX)
                                           TO  WRK-DRCD-G
                   MOVE    1               TO  FLG-CHK
                   MOVE    SPA-M95GMN-MAX  TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   1
      *        ドクター編集
               MOVE    WRK-DRCD-G          TO  M95-DRCD
               PERFORM 4120-DRCD-CHK-SEC
           END-IF
           MOVE    12                  TO  SPA-M95-CUR
           .
       4120-RENNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     ドクターチェック処理
      *****************************************************************
       4120-DRCD-CHK-SEC                 SECTION.
      *
           MOVE    M95-DRCD            TO  SPA-M95GMN-DRCD-G 
      *
           INITIALIZE                  ORCSDRSETAREA
           MOVE    SPA-ACCTSYSYMD      TO  ORCSDR-SYSYMD
           MOVE    "2"                 TO  ORCSDR-KBN
           MOVE    SPA-M95GMN-SRYKA    TO  ORCSDR-SRYKA
           MOVE    SPA-M95GMN-DRCDLST-G   TO  ORCSDR-DRCD-TBL-G
           MOVE    SPA-M95GMN-DRCD-MAX    TO  ORCSDR-DRCD-MAX
           MOVE    SPA-M95GMN-DRCD-G   TO  ORCSDR-IN-DRCD-G
           CALL    "ORCSDRSET"         USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-M95GMN-DRCDLST-G
           MOVE    ORCSDR-DRCD-MAX     TO  SPA-M95GMN-DRCD-MAX
           MOVE    ORCSDR-IN-DRCD-G    TO  SPA-M95GMN-DRCD-G
           IF      ORCSDR-ERRCD    NOT =   ZERO
               MOVE    "0102"              TO  SPA-ERRCD
               MOVE    12                  TO  SPA-M95-CUR
           ELSE
               MOVE    20                  TO  SPA-M95-CUR
           END-IF
      *
           .
       4120-DRCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *     他院切替番号入力チェック処理
      *****************************************************************
       4210-TAINNUM-CHK-SEC          SECTION.
      *
           MOVE    M95-TAINNUM         TO  SPA-M95GMN-TAINNUM
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-M95GMN-TAIN-MAX
               IF      SPA-M95GMN-TAINNUM  =   SPA-M95GMN-TAINNO (IDX)
                   MOVE    1               TO  FLG-CHK
                   MOVE    SPA-M95GMN-TAIN-MAX TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    31              TO  SPA-M95-CUR
           ELSE
               MOVE    "0105"          TO  SPA-MIDCD
           END-IF
           .
       4210-TAINNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *     クリア処理
      *****************************************************************
       202-CLREA-SEC                 SECTION.
      *
           IF      SPA-M95-PAGE        =   2
               IF      SPA-M95-SYORI       =   2
                   IF     (SPA-M95GMN-RENNUM   =   SPACE ) AND
                          (SPA-M95GMN-DRCD-G   =   SPACE )
      *                担当医一覧表示
                       MOVE    1                   TO  SPA-M95-SYORI
                       PERFORM 42001-SYSLIST-INIT-SEC
                       INITIALIZE                  SPA-M95GMN-IN-AREA
                       MOVE    10                  TO  SPA-M95-CUR
                   ELSE
                       INITIALIZE                  SPA-M95GMN-RENNUM
                       INITIALIZE                  SPA-M95GMN-DRCD-G
                       MOVE    11              TO  SPA-M95-CUR
                   END-IF
               END-IF
           END-IF
           .
       202-CLREA-EXT.
           EXIT.
      *****************************************************************
      *     削除処理
      *****************************************************************
       202-DELETE-SEC                 SECTION.
      *
           IF      SPA-M95-PAGE        =   2
               IF      SPA-M95-SYORI       =   2
                   IF      SPA-M95GMN-RENNUM   =   SPACE
                       MOVE    "0102"          TO  SPA-MIDCD
                   ELSE
                       PERFORM 2021-ONLY-DELETE-SEC
                   END-IF
               ELSE
                   MOVE    "0103"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       202-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    １行削除処理
      *****************************************************************
       2021-ONLY-DELETE-SEC                 SECTION.
      *
           MOVE    SPACE               TO  M95-DRCD
           PERFORM 210-KOUSIN-SEC
           .
       2021-ONLY-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    全体表示へ処理
      *****************************************************************
       205-HYOUJUN-SEC                 SECTION.
      *
           IF      SPA-M95-PAGE        =   2
               IF      SPA-M95-KOUSIN      =   1
                   MOVE    "0103"          TO  SPA-MIDCD
               ELSE
      *            担当医一覧表示
                   MOVE    1                   TO  SPA-M95-SYORI
                   PERFORM 42001-SYSLIST-INIT-SEC
                   INITIALIZE                  SPA-M95GMN-IN-AREA
                   MOVE    10                  TO  SPA-M95-CUR
               END-IF
           END-IF
      *
           .
       205-HYOUJUN-EXT.
           EXIT.
      *****************************************************************
      *     タグ切り替え処理
      *****************************************************************
       208-BETUGAMEN-SEC                 SECTION.
      *
           EVALUATE    SPA-M95-PAGE
               WHEN    1
                   PERFORM 4103-JIKANKBN-SEC
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    2               TO  SPA-M95-PAGE
                       IF      SPA-M95-INIT    =   ZERO
                           PERFORM 4200-DRCDMGN-INIT-SEC
                       END-IF
                       MOVE    10              TO  SPA-M95-CUR
                   END-IF
               WHEN    2
                   IF     (SPA-M95-SYORI       =   2 )  AND
                          (SPA-M95-KOUSIN      =   1 )
                       PERFORM 4201-DRCDGMN-CHK-SEC
                   END-IF
                   IF      SPA-ERRCD       =   SPACE
                       IF      SPA-GRPHOSPKBN      =   1
                           MOVE    3               TO  SPA-M95-PAGE
                           IF      SPA-M95-INIT-P3 =   ZERO
                               PERFORM 4300-TAIN-SET-SEC
                           END-IF
                           MOVE    31              TO  SPA-M95-CUR
                       ELSE
                           MOVE    1               TO  SPA-M95-PAGE
                           MOVE    2               TO  SPA-M95-CUR
                       END-IF
                   END-IF
               WHEN    3
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    1               TO  SPA-M95-PAGE
                       MOVE    2               TO  SPA-M95-CUR
                   END-IF
           END-EVALUATE
      *
           .
       208-BETUGAMEN-EXT.
           EXIT.
      *****************************************************************
      *     タグ切り替え処理
      *****************************************************************
       430-TAGUCHG-SEC                 SECTION.
      *
           MOVE    M95-PAGENO          TO  WRK-PAGENO
      *
           EVALUATE    SPA-M95-PAGE
               WHEN    1
                   PERFORM 4103-JIKANKBN-SEC
               WHEN    2
                   IF     (SPA-M95-SYORI       =   2 )  AND
                          (SPA-M95-KOUSIN      =   1 )
                       PERFORM 4201-DRCDGMN-CHK-SEC
                   END-IF
               WHEN    3
                   CONTINUE
           END-EVALUATE
           IF      SPA-ERRCD           =   SPACE
               IF      WRK-PAGENO          =   0
                   MOVE    1               TO  SPA-M95-PAGE
                   MOVE    1               TO  SPA-M95-CUR
               ELSE
                   IF      WRK-PAGENO          =   1
                   MOVE    2               TO  SPA-M95-PAGE
                   IF      SPA-M95-INIT    =   ZERO
                       PERFORM 4200-DRCDMGN-INIT-SEC
                   END-IF
                   MOVE    10              TO  SPA-M95-CUR
                 ELSE
                   IF    ( SPA-GRPHOSPKBN      =   1 )
                       MOVE    3               TO  SPA-M95-PAGE
                       IF      SPA-M95-INIT-P3 =   ZERO
                           PERFORM 4300-TAIN-SET-SEC
                       END-IF
                       MOVE    31              TO  SPA-M95-CUR
                   ELSE
                       MOVE    "0110"          TO  SPA-ERRCD
                   END-IF
                 END-IF
               END-IF
           END-IF
           .
       430-TAGUCHG-EXT.
           EXIT.
      *****************************************************************
      *     担当医画面切替え前処理
      *****************************************************************
       4201-DRCDGMN-CHK-SEC                 SECTION.
      *
           PERFORM 210-KOUSIN-SEC
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-M95-SYORI       =   2
                   MOVE    "0104"          TO  SPA-MIDCD
               END-IF
           END-IF
           .
       4201-DRCDGMN-CHK-EXT.
           EXIT.
      *****************************************************************
      *     担当医設定画面更新処理
      *****************************************************************
       210-KOUSIN-SEC                 SECTION.
      *
           IF      SPA-M95-PAGE        =   2
               IF      SPA-M95-SYORI       =    2
                   IF      (SPA-M95GMN-RENNUM   =   SPACE )
                       AND (SPA-M95GMN-DRCD-G   =   SPACE )
                       MOVE    11               TO  SPA-M95-CUR
                   ELSE
                       PERFORM 4120-DRCD-CHK-SEC
                       IF     (SPA-ERRCD       =   SPACE)
                           PERFORM 2101-GMN-KOUSIN-SEC
                           MOVE    1               TO  SPA-M95-KOUSIN
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
       210-KOUSIN-EXT.
           EXIT.
      *****************************************************************
      *     画面更新処理
      *****************************************************************
       2101-GMN-KOUSIN-SEC                 SECTION.
      *
           IF      SPA-M95GMN-RENNUM   =   SPACE
               MOVE    "0105"          TO  SPA-ERRCD
               MOVE    11              TO  SPA-M95-CUR
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-M95GMN-MAX )  OR
                              (SPA-ERRCD   NOT =   SPACE  )
               IF      SPA-M95GMN-RENNUM   =   SPA-M95GMN-TRENNUM (IDX)
                   MOVE    SPA-M95GMN-DRCD-G
                                       TO  SPA-M95GMN-TDRCD-G (IDX)
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-M95GMN-MAX  TO  IDX
               END-IF
           END-PERFORM
           IF     (SPA-ERRCD           =   SPACE )
              AND (FLG-OK              =   ZERO  )
               ADD     1                   TO  SPA-M95GMN-MAX
               IF      SPA-M95GMN-MAX      >   20
                   MOVE    "0109"          TO  SPA-ERRCD
               ELSE
                   MOVE    SPA-M95GMN-MAX      TO  IDX
                   MOVE    SPA-M95GMN-RENNUM   TO  SPA-M95GMN-TRENNUM
                                                                (IDX)
                   MOVE    SPA-M95GMN-DRCD-G   TO  SPA-M95GMN-TDRCD-G
                                                               (IDX)
                   IF      IDX                 =   1
                       MOVE    SPA-M95GMN-SRYKA    TO  SPA-M95GMN-TSRYKA
                                                                (IDX)
                       MOVE    SPA-M95GMN-SRYKAMEI
                                               TO  SPA-M95GMN-TSRYKAMEI
                                                                (IDX)
                   END-IF
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
      *        連番号毎のソート
               PERFORM 21011-GMN-SORT-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               INITIALIZE                  SPA-M95GMN-INPUT-G
               MOVE    11                  TO  SPA-M95-CUR
           END-IF
           .
       2101-GMN-KOUSIN-EXT.
           EXIT.
      *****************************************************************
      *     連番号毎のソート処理
      *****************************************************************
       21011-GMN-SORT-SEC                 SECTION.
      *
           INITIALIZE                      WRK-SORT-AREA
           MOVE    SPA-M95GMN-LIST-G   TO  WRK-M95GMN-LIST-G
           MOVE    SPA-M95GMN-TSRYKA(1)    TO  WRK-SRYKA
           MOVE    SPA-M95GMN-TSRYKAMEI(1) TO  WRK-SRYKAMEI
           INITIALIZE                  SPA-M95GMN-LIST-G
           MOVE    ZERO                TO  IDX-T
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   100     )
               MOVE    HIGH-VALUE          TO  WRK-RENNUM
               MOVE    ZERO                TO  IDX2 
               PERFORM VARYING     IDX3    FROM    1   BY  1
                       UNTIL      (IDX3    >   100     )
                   IF     (WRK-M95GMN-TRENNUM(IDX3) NOT =   SPACE )
                   AND    (WRK-M95GMN-TDRCD  (IDX3) NOT =   SPACE )
                       IF      WRK-M95GMN-TRENNUM(IDX3)
                                               <=  WRK-RENNUM
                           MOVE    IDX3        TO  IDX2
                           MOVE    WRK-M95GMN-TRENNUM(IDX3)
                                               TO  WRK-RENNUM
                       END-IF
                   END-IF
               END-PERFORM
               IF      IDX2                >   ZERO
                   ADD     1               TO  IDX-T
                   MOVE    WRK-M95GMN-LIST (IDX2)
                                           TO  SPA-M95GMN-LIST
                                                             (IDX-T)
                   MOVE    SPACE           TO  SPA-M95GMN-TSRYKA
                                                             (IDX-T)
                   MOVE    SPACE           TO  SPA-M95GMN-TSRYKAMEI
                                                             (IDX-T)
                   MOVE    SPACE           TO  WRK-M95GMN-LIST(IDX2)
               END-IF
           END-PERFORM
           MOVE    IDX-T               TO  SPA-M95GMN-MAX
           IF      IDX-T               >   ZERO
               MOVE    WRK-SRYKA       TO  SPA-M95GMN-TSRYKA(1)
               MOVE    WRK-SRYKA       TO  SPA-M95NAI-TSRYKA(1)
               MOVE    WRK-SRYKAMEI    TO  SPA-M95GMN-TSRYKAMEI(1)
           END-IF
           .
       21011-GMN-SORT-EXT.
           EXIT.
      *****************************************************************
      *     担当医設定画面初期設定処理
      *****************************************************************
       4200-DRCDMGN-INIT-SEC                 SECTION.
      *
      *    システム日付決定
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD        TO  SPA-ACCTSYSYMD
      *
      *    診療科テーブル編集
           INITIALIZE                      SPA-NAI-SRYKA-LST
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-ACCTSYSYMD      TO  SYS-1005-STYUKYMD
           MOVE    SPA-ACCTSYSYMD      TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
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
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX            >   40   )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    SYSKANRI-REC          TO  SYS-1005-REC
               MOVE    SYS-1005-KBNCD (1:2)  TO  SPA-NAI-SRYKA (IDX)
               MOVE    SYS-1005-SRYKANAME1   TO  SPA-NAI-SRYKAMEI (IDX)
               MOVE    IDX                   TO  SPA-SRYKA-MAX
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           MOVE    1                   TO  SPA-M95-INIT
      *
      *    担当医一覧表示
           MOVE    1                   TO  SPA-M95-SYORI
           PERFORM 42001-SYSLIST-INIT-SEC
           INITIALIZE                  SPA-M95GMN-IN-AREA
           .
       4200-DRCDMGN-INIT-EXT.
           EXIT.
      *****************************************************************
      *     担当医初期画面編集処理
      *****************************************************************
       42001-SYSLIST-INIT-SEC          SECTION.
      *
      *    コラムリスト位置
           MOVE    1                   TO  FLG-GMN-INIT
      *    ドクターテーブル編集
           PERFORM 42000-DRCD-TBL-SEC
      *    担当医設定
           INITIALIZE                  SPA-M95GMN-LIST-G
      *
           MOVE    SPACE               TO  SYS-1011-REC
           MOVE    "1011"              TO  SYS-1011-KANRICD
           MOVE    SPA-ACCTSYSYMD      TO  SYS-1011-STYUKYMD
           MOVE    SPA-ACCTSYSYMD      TO  SYS-1011-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1011-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1011-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1011-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    1                   TO  IDX-S
           MOVE    ZERO                TO  IDX-H
           PERFORM UNTIL       FLG-SYSKANRI    =   1
               MOVE    SYSKANRI-REC            TO  SYS-1011-REC
               IF      SPA-M95-SYORI         =   1
                   MOVE    SYS-1011-KBNCD(1:2)   TO  WRK-SRYKA
                   PERFORM 42002-ALL-SYSLIST-HEN-SEC
               ELSE
                   IF      SYS-1011-KBNCD(1:2)   =   SPA-M95GMN-SRYKA
                       MOVE    SYS-1011-KBNCD(1:2)   TO  WRK-SRYKA
                       PERFORM 42003-ONLY-SYSLIST-HEN-SEC
                       MOVE    1               TO  FLG-SYSKANRI
                   END-IF
               END-IF
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-IF
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      SPA-M95-SYORI         =   1
               IF      IDX-S               <=  SPA-SRYKA-MAX
                   MOVE    SPACE           TO  WRK-SRYKA
                   PERFORM 42003-MAX-SYSLIST-HEN-SEC
               END-IF
           END-IF
           MOVE    IDX-H               TO  SPA-M95GMN-MAX
           .
       42001-SYSLIST-INIT-EXT.
           EXIT.
      *****************************************************************
      *     担当医設定画面編集処理
      *****************************************************************
       42003-MAX-SYSLIST-HEN-SEC          SECTION.
      *
           PERFORM VARYING     IDX     FROM    IDX-S   BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
                          OR  (IDX-H   >=  COM-TANMAX    )
               PERFORM 4200211-SRYKA-HEN-SEC
           END-PERFORM
           .
       42003-MAX-SYSLIST-HEN-EXT.
           EXIT.
      *****************************************************************
      *     担当医設定画面編集処理
      *****************************************************************
       42002-ALL-SYSLIST-HEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-HEN
           PERFORM VARYING     IDX     FROM    IDX-S   BY  1
                   UNTIL      (IDX     >   SPA-SRYKA-MAX )
                          OR  (IDX-H   >=  COM-TANMAX    )
               IF      SPA-NAI-SRYKA (IDX) =   WRK-SRYKA
                   PERFORM 4200211-SRYKA-HEN-SEC
                   PERFORM 420021-DRDELIST-HEN-SEC
                   ADD     1               TO  IDX-S
                   MOVE    SPA-SRYKA-MAX   TO  IDX
                   MOVE    1               TO  FLG-HEN
               ELSE
                   IF      SPA-NAI-SRYKA (IDX) <   WRK-SRYKA
                       PERFORM 4200211-SRYKA-HEN-SEC
                       ADD     1               TO  IDX-S
                   ELSE
                       MOVE    SPA-SRYKA-MAX   TO  IDX
                   END-IF
               END-IF
           END-PERFORM
           IF     (FLG-HEN             =   ZERO  )
              AND (WRK-SRYKA       NOT =   SPACE )
              AND (IDX-H               <   COM-TANMAX )
      *        診療科なし
               ADD     1               TO  IDX-H
               MOVE    WRK-SRYKA       TO  SPA-M95GMN-TSRYKA  (IDX-H)
               MOVE    WRK-SRYKA       TO  SPA-M95NAI-TSRYKA  (IDX-H)
               MOVE    "★削除★"      TO  SPA-M95GMN-TSRYKAMEI
                                                             (IDX-H)
               PERFORM 420021-DRDELIST-HEN-SEC
           END-IF
           .
       42002-ALL-SYSLIST-HEN-EXT.
           EXIT.
      *****************************************************************
      *     診療科編集処理
      *****************************************************************
       42003-ONLY-SYSLIST-HEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-HEN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-SRYKA-MAX )
                         OR   (IDX-H   >=  COM-TANMAX    )
               IF      SPA-NAI-SRYKA (IDX) =   WRK-SRYKA
                   PERFORM 4200211-SRYKA-HEN-SEC
                   PERFORM 420021-DRDELIST-HEN-SEC
                   MOVE    SPA-SRYKA-MAX   TO  IDX
                   MOVE    1               TO  FLG-HEN
               END-IF
           END-PERFORM
      *
           IF     (FLG-HEN             =   ZERO )
             AND  (IDX-H               <   COM-TANMAX )
      *        診療科なし
               ADD     1               TO  IDX-H
               MOVE    WRK-SRYKA       TO  SPA-M95GMN-TSRYKA  (IDX-H)
               MOVE    WRK-SRYKA       TO  SPA-M95NAI-TSRYKA  (IDX-H)
               MOVE    "★削除★"      TO  SPA-M95GMN-TSRYKAMEI
                                                             (IDX-H)
               PERFORM 420021-DRDELIST-HEN-SEC
           END-IF
           .
       42003-ONLY-SYSLIST-HEN-EXT.
           EXIT.
      *****************************************************************
      *     診療科編集処理
      *****************************************************************
       4200211-SRYKA-HEN-SEC          SECTION.
      *
           ADD     1                   TO  IDX-H
           MOVE    SPA-NAI-SRYKA (IDX) TO  SPA-M95GMN-TSRYKA (IDX-H)
           MOVE    SPA-NAI-SRYKAMEI (IDX)  TO  SPA-M95GMN-TSRYKAMEI
                                                            (IDX-H)
           MOVE    SPA-NAI-SRYKA (IDX) TO  SPA-M95NAI-TSRYKA (IDX-H)
           .
       4200211-SRYKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *     ドクター編集処理
      *****************************************************************
       420021-DRDELIST-HEN-SEC          SECTION.
      *
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   20  )  OR
                              (SYS-1011-DRCD (IDX-D)   =   SPACE)
                          OR  (IDX-H   >=  COM-TANMAX  )
               IF      IDX-D           >   1
                   ADD     1                   TO  IDX-H
               END-IF
               MOVE    SYS-1011-RENNUM (IDX-D)
                                           TO  SPA-M95GMN-TRENNUM(IDX-H)
               MOVE    SYS-1011-DRCD(IDX-D)(2:4)
                                           TO  SPA-M95GMN-TDRCD (IDX-H)
               MOVE    WRK-SRYKA           TO  SPA-M95NAI-TSRYKA (IDX-H)
               PERFORM VARYING     IDK     FROM    1   BY  1
                       UNTIL      (IDK     >   300 )  OR
                                  (TBL-DRCD (IDK)  =   SPACE )
                   IF      SYS-1011-DRCD(IDX-D)
                                           =   TBL-DRCD (IDK)
                       MOVE    TBL-DRCDNAME (IDK)
                                           TO  SPA-M95GMN-TDRCDMEI
                                                           (IDX-H)
                       MOVE    300         TO  IDK
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       420021-DRDELIST-HEN-EXT.
           EXIT.
      *****************************************************************
      *     担当医標準初期処理
      *****************************************************************
       42000-DRCD-TBL-SEC                 SECTION.
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-ACCTSYSYMD      TO  SYS-1010-STYUKYMD
           MOVE    SPA-ACCTSYSYMD      TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           INITIALIZE                  TBL-DRCD-AREA
           MOVE    ZERO                TO  IDX
           MOVE    SPACE               TO  WRK-SYU-DRCD
           PERFORM
                   UNTIL       (FLG-SYSKANRI   =   1    )
                           OR  (IDX            >=  300  )
               MOVE    SYSKANRI-REC            TO  SYS-1010-REC
               IF      SYS-1010-KBNCD(1:1)     =   "1"
                   ADD     1                   TO  IDX
                   MOVE    SYS-1010-KBNCD      TO  TBL-DRCD (IDX)
                   MOVE    SYS-1010-NAME       TO  TBL-DRCDNAME(IDX)
      *            専門科コード
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   5
                       MOVE    SYS-1010-SEMONKACD(IDX2)
                                           TO  TBL-SENMONKACD(IDX IDX2)
                   END-PERFORM
                   IF     (TBL-SENMONKACD-G (IDX)  =   SPACE ) AND
                          (WRK-SYU-DRCD            =   SPACE )
                       MOVE    SYS-1010-KBNCD      TO  WRK-SYU-DRCD
                   END-IF
               END-IF
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      WRK-SYU-DRCD        =   SPACE
               MOVE    TBL-DRCD (1)    TO  WRK-SYU-DRCD
           END-IF
           .
       42000-DRCD-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *     他院切替画面初期設定処理
      *****************************************************************
       4300-TAIN-SET-SEC            SECTION.
      *
           MOVE    ZERO                TO  SPA-M95GMN-TAIN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX            >   9    )
               IF      SPA-GRPHOSPNUM (IDX)  NOT =   ZERO
                   COMPUTE SPA-M95GMN-TAIN-MAX   =
                           SPA-M95GMN-TAIN-MAX   +   1
                   MOVE    IDX                 TO  WRK-NUM-1
                   MOVE    WRK-NUM-1-X         TO
                                               SPA-M95GMN-TAINNO (IDX)
                   MOVE    SPA-GRPHOSPNUM (IDX)    TO
                                               SPA-M95GMN-TAINCD (IDX)
                   MOVE    SPA-GRPHOSPNAME (IDX)   TO
                                               SPA-M95GMN-TAINNAME (IDX)
               END-IF
           END-PERFORM
           MOVE    1                   TO  SPA-M95-INIT-P3
           .
       4300-TAIN-SET-EXT.
           EXIT.
      *****************************************************************
      *     他院切替処理
      *****************************************************************
       4310-TAIN-SYORI-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-GRPHOSPUSERID
           MOVE    ZERO                TO  WRK-HOSPNUM
           MOVE    SPA-M95GMN-TAINNUM  TO  WRK-NUM-1-X
      *
      *    医療機関識別番号セット SPA-HOSPNUM
           IF      SPA-GRPHOSPUSERID(WRK-NUM-1)  =  SPACE
               DISPLAY "GRPHOSPUSERID IS EMPTY!!"
               MOVE    "0106"          TO  SPA-MIDCD
               GO  TO  4310-TAIN-SYORI-EXT
           END-IF
           MOVE    SPA-GRPHOSPNUM (WRK-NUM-1)
                                       TO  WRK-HOSPNUM
           MOVE    SPA-GRPHOSPUSERID (WRK-NUM-1)
                                       TO  WRK-GRPHOSPUSERID
      *!!
      *    ＳＰＡ共通設定
           MOVE    SPA-M95-MOTOPG      TO  WRK-MOTOPG
           INITIALIZE                  SPA-AREA
           MOVE    WRK-GRPHOSPUSERID   TO  SPA-OPID
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M95"               TO  ORCSCOMMON-PG
           MOVE    WRK-HOSPNUM         TO  ORCSCOMMON-HOSPNUM
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
      *
           IF      WRK-MOTOPG          =   "M00"
               MOVE    "M00"           TO  SPA-M95-MOTOPG
           ELSE
               IF      SPA-BEDFLG          =   ZERO
                   MOVE    "M01"           TO  SPA-M95-MOTOPG
               ELSE
                   MOVE    "M01N"          TO  SPA-M95-MOTOPG
               END-IF
           END-IF
      *
           PERFORM 210-BACK
           .
       4310-TAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     担当医削除処理
      *****************************************************************
       2021-DELETE-SYORI-SEC                 SECTION.
      *
      *    システム管理検索
           MOVE    SPACE               TO  SYS-1011-REC
           MOVE    "1011"              TO  SYS-1011-KANRICD
           MOVE    SPA-M95GMN-SRYKA    TO  SYS-1011-KBNCD(1:2)
           MOVE    "00000000"          TO  SYS-1011-STYUKYMD
           MOVE    "99999999"          TO  SYS-1011-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1011-HOSPNUM
      *
           MOVE    SYS-1011-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1011-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1011-REC
               PERFORM 420091-SYS1011-DEL-SEC
           END-IF
      *
      *    担当医一覧表示
           MOVE    1                   TO  SPA-M95-SYORI
           PERFORM 42001-SYSLIST-INIT-SEC
           INITIALIZE                  SPA-M95GMN-IN-AREA
      *
           .
       2021-DELETE-SYORI-EXT.
           EXIT.
      *****************************************************************
      *     担当医レコード削除処理
      *****************************************************************
       420091-SYS1011-DEL-SEC           SECTION.
      *
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    SYS-1011-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "M95 SYSKANRI DEL ERR:" MCP-RC
                           ",KEY:"    SYS-1011-KEY
               MOVE    "9000"              TO  SPA-ERRCD
           END-IF
           .
       420091-SYS1011-DEL-EXT.
           EXIT.
      *****************************************************************
      *     登録  処理
      *****************************************************************
       400-TOROKU-SEC                 SECTION.
      *
           IF      SPA-M95-PAGE        =   1
               PERFORM 4010-PAGE1-TOROKU-SEC
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 210-BACK
               END-IF
           END-IF
      *
           IF      SPA-M95-PAGE        =   2
               PERFORM 4201-DRCDGMN-CHK-SEC
               IF      SPA-ERRCD           =   SPACE
                   MOVE    SPACE               TO  SPA-MIDCD
                   PERFORM 4020-PAGE2-TOROKU-SEC
               END-IF
      *
           END-IF
      *
           .
       400-TOROKU-EXT.
           EXIT.
      *****************************************************************
      *    システム日付の登録処理
      *****************************************************************
       4010-PAGE1-TOROKU-SEC            SECTION.
      *
      *    画面入力の和暦変換
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "11"                TO  LNK-DAY1-IRAI
           MOVE    SPA-NAI-SYSYMD      TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"      USING    STS-AREA-DAY
                                           LNK-DAY1-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY1-WYMD       TO  SPA-NAI-SYSYMDW
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO      4010-PAGE1-TOROKU-EXT
           END-IF
      *    時間外区分チェック
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-JIKANKBN-MAX
               IF      SPA-GMN-JIKANKBN    =   SPA-GMN-JIKANKBN-LST
                                                          (IDX)(1:1)
                   MOVE    1               TO  FLG-CHK
                   MOVE    SPA-JIKANKBN-MAX    TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-M95-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 40101-PAGE1-HENSYU-SEC
           END-IF
      *ver.4.7
      *    背景色設定
           IF      SPA-ERRCD           =   SPACE
               PERFORM 3102-BGCOLORS-INIT-SEC
               MOVE    SPA-M95-BGCOLORS    TO  SPA-M00-BGCOLORS
           END-IF
      *
           .
       4010-PAGE1-TOROKU-EXT.
           EXIT.
      *****************************************************************
      *    システム日付の登録処理
      *****************************************************************
       40101-PAGE1-HENSYU-SEC            SECTION.
      *
      *    曜日指定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY3-AREA
           MOVE    "31"                TO  LNK-DAY3-IRAI
           MOVE    SPA-NAI-SYSYMD      TO  LNK-DAY3-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY3-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY3-YOBI       TO  SPA-SYSYMD-YOBI
           END-IF
      *
           MOVE    SPA-GMN-SYSYMD      TO  SPA-SYSYMDWH
           MOVE    SPA-NAI-SYSYMD      TO  SPA-SYSYMD
           MOVE    SPA-NAI-SYSYMDW     TO  SPA-SYSYMDW
      *
           MOVE    SPA-GMN-JIKANKBN    TO  SPA-GAI-JIKANKBN
      *
           .
       40101-PAGE1-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    担当医の登録処理
      *****************************************************************
       4020-PAGE2-TOROKU-SEC            SECTION.
      *
           IF      SPA-M95-SYORI       =   2
      *H29.9   ドクター登録タブのみ登録
               PERFORM 4021-SYS1011-UPDATE-SEC
           END-IF
      *
      *    担当医一覧表示
           IF      SPA-ERRCD           =   SPACE
               MOVE    1                   TO  SPA-M95-SYORI
               PERFORM 42001-SYSLIST-INIT-SEC
               INITIALIZE                  SPA-M95GMN-IN-AREA
               INITIALIZE                  SPA-M95GMN-INPUT-G
               MOVE    10                  TO  SPA-M95-CUR
           END-IF
           .
       4020-PAGE2-TOROKU-EXT.
           EXIT.
      *****************************************************************
      *     担当医レコード更新処理
      *****************************************************************
       4021-SYS1011-UPDATE-SEC                 SECTION.
      *
      *    システム日付決定
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *    システム管理検索
           MOVE    SPACE               TO  SYS-1011-REC
           MOVE    "1011"              TO  SYS-1011-KANRICD
           MOVE    SPA-M95GMN-SRYKA    TO  SYS-1011-KBNCD(1:2)
           MOVE    "00000000"          TO  SYS-1011-STYUKYMD
           MOVE    "99999999"          TO  SYS-1011-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1011-HOSPNUM
      *
           MOVE    SYS-1011-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1011-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1011-REC
               PERFORM 4021-SYS1011-HEN-SEC
      *
               MOVE    SMCNDATE-YMD        TO  SYS-1011-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-1011-UPHMS
      *
grpsys*        MOVE SPA-HOSPNUM TO -HOSPNUM
               MOVE    SYS-1011-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "M95 SYSKANRI UPD ERR:" MCP-RC
                               ",KEY:"    SYS-1011-KEY
                   MOVE    "9001"              TO  SPA-ERRCD
               END-IF
           ELSE
      *        新規
               MOVE    SPACE               TO  SYS-1011-REC
               MOVE    "1011"              TO  SYS-1011-KANRICD
               MOVE    SPA-M95GMN-SRYKA    TO  SYS-1011-KBNCD(1:2)
               MOVE    "00000000"          TO  SYS-1011-STYUKYMD
               MOVE    "99999999"          TO  SYS-1011-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-1011-HOSPNUM
      *
               PERFORM 4021-SYS1011-HEN-SEC
      *
               MOVE    SMCNDATE-YMD        TO  SYS-1011-CREYMD
               MOVE    SMCNDATE-YMD        TO  SYS-1011-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-1011-UPHMS
      *
               MOVE    SYS-1011-REC        TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "M95 SYSKANRI INS ERR:" MCP-RC
                               ",KEY:"    SYS-1011-KEY
                   MOVE    "9001"              TO  SPA-ERRCD
               END-IF
           END-IF
           .
       4021-SYS1011-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    担当医編集処理
      *****************************************************************
       4021-SYS1011-HEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX2
           MOVE    SPACE               TO  SYS-1011-TBL
           INITIALIZE                      SYS-1011-TBL
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-M95GMN-MAX
               IF      SPA-M95GMN-TDRCD (IDX)  =   SPACE
                   CONTINUE
               ELSE
                   ADD     1               TO  IDX2
                   MOVE    SPA-M95GMN-TRENNUM(IDX)
                                           TO  SYS-1011-RENNUM(IDX2)
                   MOVE    "1"             TO  SYS-1011-DRCD(IDX2)(1:1)
                   MOVE    SPA-M95GMN-TDRCD (IDX)
                                           TO  SYS-1011-DRCD(IDX2)(2:4)
               END-IF
           END-PERFORM
      *
           .
       4021-SYS1011-HEN-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPA-M95-MOTOPG      TO  SPA-SAKIPG
           MOVE    "M95"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
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
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-MIDCD       NOT =   SPACE )
               PERFORM 520-MIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "M95    "           TO  MCP-WINDOW
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
      *    コラムリスト位置保存
           IF      FLG-GMN-INIT        =   1
               MOVE    ZERO                TO  WRK-M95-SYSLIST-ROW
           ELSE
               MOVE    M95-SYSLIST-ROW     TO  WRK-M95-SYSLIST-ROW
           END-IF
      *
           INITIALIZE                  M95
      *    背景色設定
           MOVE    SPA-M95-BGCOLORS        TO  M95-BGCOLORS
      *
           IF      SPA-GRPHOSPKBN      =   1
               MOVE    WIDGET-NORMAL       TO  M95-N03-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  M95-N03-STATE
           END-IF
      *
      *    コラムリストの位置指定
           MOVE    ZERO                TO  M95-SYSLIST-ROW
           MOVE    ZERO                TO  M95-SYSLIST-ROWATTR
           IF      WRK-M95-SYSLIST-ROW    NOT =   ZERO
               MOVE    WRK-M95-SYSLIST-ROW
                                           TO  M95-SYSLIST-ROW
               MOVE    2                   TO  M95-SYSLIST-ROWATTR
           END-IF
      *
           MOVE    SPA-GMN-SYSYMD      TO  M95-SYSYMD
           MOVE    SPA-NAI-SYSYMD      TO  WRK-SYMD
           MOVE    WRK-SYY             TO  M95-YEAR
           MOVE    WRK-SMM             TO  M95-MONTH
           MOVE    WRK-SDD             TO  M95-DAY
      *
           MOVE    SPA-GMN-JIKANKBN-G  TO  M95-JIKANKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-JIKANKBN-MAX
               MOVE    SPA-GMN-JIKANKBN-LST (IDX)
                                           TO  M95-JIKANKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-JIKANKBN-MAX    TO  M95-JIKANKBN-COUNT
      *
           IF     (SPA-M95-INIT    =   ZERO ) AND
                 ((SPA-M95-PAGE    =   1    ) OR
                  (SPA-M95-PAGE    =   3    ))
               INITIALIZE                  SPA-M95GMN-DRCD-MAX
               INITIALIZE                  SPA-M95GMN-MAX
           END-IF
      *
           MOVE    SPA-M95GMN-SRYKA    TO  M95-SRYKA
           MOVE    SPA-M95GMN-RENNUM   TO  M95-RENNUM
           MOVE    SPA-M95GMN-DRCD-G   TO  M95-DRCD
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-M95GMN-DRCD-MAX
               MOVE    SPA-M95GMN-DRCDLST (IDX)
                                           TO  M95-DRCD-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-M95GMN-DRCD-MAX TO  M95-DRCD-COUNT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-M95GMN-MAX
               MOVE    SPA-M95GMN-TSRYKA (IDX)
                                           TO  M95-TSRYKA (IDX)
               MOVE    SPA-M95GMN-TSRYKAMEI (IDX)
                                           TO  M95-TSRYKAMEI (IDX)
               MOVE    SPA-M95GMN-TRENNUM   (IDX)
                                           TO  M95-TRENNUM   (IDX)
               MOVE    SPA-M95GMN-TDRCD-G (IDX)
                                           TO  M95-TDRCDMEI (IDX)
               MOVE    "F"                 TO  M95-SYSLIST-SELECT(IDX)
               IF      SPA-M95-SYORI       =   2
                   IF     (SPA-M95GMN-TRENNUM (IDX)
                                           =   SPA-M95GMN-RENNUM )
                      AND (SPA-M95GMN-RENNUM   NOT =   SPACE     )
                   MOVE    "T"             TO  M95-SYSLIST-SELECT(IDX)
                   MOVE    IDX             TO  M95-SYSLIST-ROW
                   MOVE    2               TO  M95-SYSLIST-ROWATTR
                   END-IF
               END-IF
           END-PERFORM
           MOVE    SPA-M95GMN-MAX      TO  M95-SYSLIST-COUNT
      *
           IF     (SPA-M95-INIT-P3 =   ZERO ) AND
                 ((SPA-M95-PAGE    =   1    ) OR
                  (SPA-M95-PAGE    =   2    ))
               MOVE    ZERO            TO  SPA-M95GMN-TAIN-MAX
           END-IF
      *
           MOVE    SPA-M95GMN-TAINNUM      TO  M95-TAINNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-M95GMN-TAIN-MAX
               MOVE    SPA-M95GMN-TAINNO (IDX)
                                           TO  M95-TAINNO (IDX)
               MOVE    SPA-M95GMN-TAINNAME-G (IDX)
                                           TO  M95-TAINNAME (IDX)
               MOVE    "F"                 TO  M95-TAINLIST-SELECT(IDX)
           END-PERFORM
           MOVE    SPA-M95GMN-TAIN-MAX     TO  M95-TAINLIST-COUNT
      *
      *    処理
           IF      SPA-M95-PAGE        =   2
               IF      SPA-M95-SYORI       =   1
                   MOVE    "＜表示＞"        TO  M95-SYORIMEI-VALUE
                   MOVE    "blue"              TO  M95-SYORIMEI-STYLE
               ELSE
      ****         MOVE    "＜更新＞"        TO  M95-SYORIMEI-VALUE
                   STRING  "＜"                DELIMITED  BY  SIZE
                           SPA-M95GMN-SRYKAMEI DELIMITED  BY  SPACE
                           "＞"                DELIMITED  BY  SIZE
                                           INTO    M95-SYORIMEI-VALUE
                   END-STRING
                   MOVE    "red"               TO  M95-SYORIMEI-STYLE
               END-IF
           END-IF
      *
           COMPUTE M95-PAGENO          =   SPA-M95-PAGE  -  1
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
           EVALUATE    SPA-M95-CUR
      *        環境設定
               WHEN    1
      *********    MOVE  "SYSYMD"    TO  MCP-WIDGET
               WHEN    2
                   MOVE  "JIKANKBN"  TO  MCP-WIDGET
               WHEN    3
                   MOVE  "B12"       TO  MCP-WIDGET
      *        システム設定
               WHEN    10
                   MOVE  "SRYKA"     TO  MCP-WIDGET
               WHEN    11
                   MOVE  "RENNUM"    TO  MCP-WIDGET
               WHEN    12
                   MOVE  "DRCD"      TO  MCP-WIDGET
               WHEN    20
                   MOVE  "B10"       TO  MCP-WIDGET
               WHEN    21
                   MOVE  "B04"       TO  MCP-WIDGET
      *        他院切替
               WHEN    31
                   MOVE  "TAINNUM"   TO  MCP-WIDGET
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
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラー"        TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "時間外区分エラー"  TO  SPA-ERRMSG
               WHEN    "0101"
                   MOVE    "診療科入力エラー"  TO  SPA-ERRMSG
               WHEN    "0102"
                   MOVE    "ドクターコード入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0103"
                   MOVE    "全体表示中です。削除はできません。"
                                               TO  SPA-ERRMSG
               WHEN    "0105"
                   MOVE    "番号は必須入力です。"
                                               TO  SPA-ERRMSG
               WHEN    "0106"
                   MOVE    "診療科は変更できません。"
                                               TO  SPA-ERRMSG
               WHEN    "0107"
                   STRING  "診療科が２桁ではありません。"
                           "削除して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0109"
                   STRING  "２０件以上になります。"
                           "これ以上登録できません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0110"
                   MOVE    "本院・分院機能が有効な場合に設定可能です。"
                                               TO  SPA-ERRMSG
               WHEN    "9000"
                   MOVE    "ＤＢ削除エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "9001"
                   MOVE    "ＤＢ更新エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPA-ERRCD           TO  SPA-ERRMSG
           END-EVALUATE
      *
      *
           MOVE    SPACE                TO  MERR
           MOVE    SPA-ERRCD            TO  MERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  MERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "M95"                TO  SPA-MOTOPG
           MOVE    "MERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "MERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-MIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  MID1
           INITIALIZE                      MID1
           EVALUATE    SPA-MIDCD
               WHEN    "0102"
                   STRING  "表示されているすべての担当医を削除します。"
                           "よろしいですか？"
                                       INTO  MID1-ID1MSG
                   END-STRING
               WHEN    "0103"
                   STRING  "表示されている診療科の担当医を登録します。"
                           "よろしいですか？"
                                       INTO  MID1-ID1MSG
                   END-STRING
               WHEN    "0104"
                   STRING  "表示されている診療科の担当医を登録します。"
                           "よろしいですか？"
                                       INTO  MID1-ID1MSG
                   END-STRING
               WHEN    "0105"
                   MOVE    SPA-M95GMN-TAINNUM  TO  WRK-NUM-1-X
                   STRING  SPA-M95GMN-TAINNAME(WRK-NUM-1)
                                               DELIMITED   BY  SPACE
                           "へ切り替えを行います。よろしいですか？"
                                               DELIMITED   BY  SIZE
                                               INTO  MID1-ID1MSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-MIDCD
                                       TO  MID1-ID1MSG
           END-EVALUATE
      *
           MOVE    SPA-MIDCD           TO  MID1-ID1CODE
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "M95"               TO  SPA-MOTOPG
           MOVE    "MID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "MID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-MIDSET-EXT.
           EXIT.
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-MIDSET2-SEC             SECTION.
      *
           MOVE    SPACE               TO  MID2
           INITIALIZE                      MID2
           EVALUATE    SPA-MIDCD
               WHEN    "0106"
                   MOVE    SPA-M95GMN-TAINNUM  TO  WRK-NUM-1-X
                   STRING  SPA-M95GMN-TAINNAME(WRK-NUM-1)
                                               DELIMITED   BY  SPACE
                           "へログインするためのユーザーが"
                                               DELIMITED   BY  SIZE
                           "設定されていません"
                                               DELIMITED   BY  SIZE
                                               INTO  MID2-ID1MSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-MIDCD
                                       TO  MID2-ID1MSG
           END-EVALUATE
      *
           MOVE    SPA-MIDCD           TO  MID2-ID1CODE
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "M95"               TO  SPA-MOTOPG
           MOVE    "MID2"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "MID2"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-MIDSET2-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    SPACE           TO  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
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
           MOVE    "PUTWINDOW"         TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
