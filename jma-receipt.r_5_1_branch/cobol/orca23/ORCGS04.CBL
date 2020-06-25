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
       PROGRAM-ID.                 ORCGS04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 収納登録
      *  コンポーネント名  : 支払証明書発行（Ｓ０４）
      *  管理者            : 
      *  02/12/19    NACL−森脇   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.01.01     NACL-森脇    04/11/15  設定画面変更
      * 03.04.00     NACL-森脇    06/12/15  画面変更
      * 03.04.01     NACL-森脇    06/12/19  プレビュー表示
      * 03.05.00     NACL-太田    07/05/20  グループ診療対応
      * 04.03.00     NACL-太田    08/06/18  システム管理より初期値設定
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
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "S01COMMON-SPA".
      *
      *    支払証明書パラメータ
           COPY    "CPORCHCM31.INC".
      *
      *    画面活性非活性
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
       01  SPA-S04.
           03  SPA-GMN-CUR                 PIC 9(03).
           03  SPA-GMN-AREA.
               05  SPA-GMN-SIHARAIKBN-G.
                   07  SPA-GMN-SIHARAIKBN  PIC X(01).
                   07  SPA-GMN-FL01        PIC X(01).
                   07  SPA-GMN-SIHARAIMEI  PIC X(10).
      *
               05  SPA-GMN-NYUGAIKBN-G.
                   07  SPA-GMN-NYUGAIKBN   PIC X(01).
                   07  SPA-GMN-FL02        PIC X(01).
                   07  SPA-GMN-NYUGAIMEI   PIC X(04).
      *
               05  SPA-GMN-SRYKBN-G.
                   07  SPA-GMN-SRYKBN      PIC X(01).
                   07  SPA-GMN-FL03        PIC X(01).
                   07  SPA-GMN-SRYMEI      PIC X(06).
      *
               05  SPA-GMN-STRSRYYMD       PIC X(10).
               05  SPA-GMN-ENDSRYYMD       PIC X(10).
      *
               05  SPA-GMN-KIKANKBN-G.
                   07  SPA-GMN-KIKANKBN    PIC X(01).
                   07  SPA-GMN-FL04        PIC X(01).
                   07  SPA-GMN-KIKANMEI    PIC X(08).
      *
               05  SPA-GMN-DSPKBN-G.
                   07  SPA-GMN-DSPKBN      PIC X(01).
                   07  SPA-GMN-FL05        PIC X(01).
                   07  SPA-GMN-DSPMEI      PIC X(18).
      *
               05  SPA-GMN-SUMKBN-G.
                   07  SPA-GMN-SUMKBN      PIC X(01).
                   07  SPA-GMN-FL06        PIC X(01).
                   07  SPA-GMN-SUMMEI      PIC X(16).
      *
               05  SPA-GMN-SYORIKBN-G.
                   07  SPA-GMN-SYORIKBN    PIC X(01).
                   07  SPA-GMN-FL07        PIC X(01).
                   07  SPA-GMN-SYORIMEI    PIC X(16).
      *
               05  SPA-GMN-JIHIKBN-G.
                   07  SPA-GMN-JIHIKBN     PIC X(01).
                   07  SPA-GMN-FL08        PIC X(01).
                   07  SPA-GMN-JIHIMEI     PIC X(10).
      *
               05  SPA-GMN-SIHARAILIST-G.
                   07  SPA-GMN-SIHARAILIST-MAX PIC 9(05).
                   07  SPA-GMN-SIHARAILIST         OCCURS  3.
                       09  SPA-GMN-SIHARAIKBNL PIC X(01).
                       09  SPA-GMN-FL09        PIC X(01).
                       09  SPA-GMN-SIHARAIMEIL PIC X(10).
      *
               05  SPA-GMN-NYUGAILIST-G.
                   07  SPA-GMN-NYUGAILIST-MAX  PIC 9(05).
                   07  SPA-GMN-NYUGAILIST          OCCURS  3.
                       09  SPA-GMN-NYUGAIKBNL  PIC X(01).
                       09  SPA-GMN-FL10        PIC X(01).
                       09  SPA-GMN-NYUGAIMEIL  PIC X(04).
      *
               05  SPA-GMN-SRYLIST-G.
                   07  SPA-GMN-SRYLIST-MAX     PIC 9(05).
                   07  SPA-GMN-SRYLIST             OCCURS  3.
                       09  SPA-GMN-SRYKBNL     PIC X(01).
                       09  SPA-GMN-FL11        PIC X(01).
                       09  SPA-GMN-SRYMEIL     PIC X(06).
      *
               05  SPA-GMN-KIKANLIST-G.
                   07  SPA-GMN-KIKANLIST-MAX   PIC 9(05).
                   07  SPA-GMN-KIKANLIST           OCCURS  2.
                       09  SPA-GMN-KIKANKBNL   PIC X(01).
                       09  SPA-GMN-FL12        PIC X(01).
                       09  SPA-GMN-KIKANMEIL   PIC X(08).
      *
               05  SPA-GMN-DSPLIST-G.
                   07  SPA-GMN-DSPLIST-MAX     PIC 9(05).
                   07  SPA-GMN-DSPLIST             OCCURS  4.
                       09  SPA-GMN-DSPKBNL     PIC X(01).
                       09  SPA-GMN-FL13        PIC X(01).
                       09  SPA-GMN-DSPMEIL     PIC X(18).
      *
               05  SPA-GMN-SUMKBNLIST-G.
                   07  SPA-GMN-SUMKBNLIST-MAX  PIC 9(05).
                   07  SPA-GMN-SUMKBNLIST      OCCURS  2.
                       09  SPA-GMN-SUMKBNL     PIC X(01).
                       09  SPA-GMN-FL14        PIC X(01).
                       09  SPA-GMN-SUMKBNMEIL  PIC X(16).
      *
               05  SPA-GMN-SYORILIST-G.
                   07  SPA-GMN-SYORILIST-MAX   PIC 9(05).
                   07  SPA-GMN-SYORILIST           OCCURS  2.
                       09  SPA-GMN-SYORIKBNL   PIC X(01).
                       09  SPA-GMN-FL15        PIC X(01).
                       09  SPA-GMN-SYORIMEIL   PIC X(16).
      *
               05  SPA-GMN-JIHILIST-G.
                   07  SPA-GMN-JIHILIST-MAX    PIC 9(05).
                   07  SPA-GMN-JIHILIST            OCCURS  3.
                       09  SPA-GMN-JIHIKBNL    PIC X(01).
                       09  SPA-GMN-FL16        PIC X(01).
                       09  SPA-GMN-JIHIMEIL    PIC X(10).
      *
           03  SPA-NAI-AREA.
               05  SPA-NAI-HCM31API            PIC X(01).
               05  SPA-NAI-HCM32API            PIC X(01).
               05  SPA-NAI-STRSRYYMD.
                   07  SPA-NAI-STRYY       PIC 9(04).
                   07  SPA-NAI-STRMM       PIC 9(02).
                   07  SPA-NAI-STRDD       PIC 9(02).
               05  SPA-NAI-ENDSRYYMD.
                   07  SPA-NAI-ENDYY       PIC 9(04).
                   07  SPA-NAI-ENDMM       PIC 9(02).
                   07  SPA-NAI-ENDDD       PIC 9(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-INIT            PIC 9(01).
           03  FLG-API             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(05).
      *
      *    作業領域
       01  WRK-AREA.
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-LNKYMD          PIC X(10).
           03  WRK-SRYYMD          PIC X(10).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-YMD.
               05  WRK-YY          PIC S9(04).
               05  WRK-MM          PIC S9(02).
      *
           03  WRK-SYORI-PG        PIC X(20).
           03  WRK-SIDMSG          PIC X(70).
           03  WRK-ERRMSG          PIC X(80).
           03  WRK-PRTPG           PIC X(20).
           03  WRK-KBNCD           PIC X(08).
      *
       01  CONST-AREA.
      *
      *    システム管理ＤＢ（３００３）のキー値
           03  CONST-3003-KBNCD            PIC X(08)   VALUE "023".
      *
      *    印刷管理ＤＢのキー値
           03  CONST-PRTKANRI-TBL-KEY      PIC X(08)   VALUE "ORCBS01".
           03  CONST-PRTKANRI-TBL-GROUP    PIC X(14)   VALUE SPACE.
      *
      *    ジョブ管理ＤＢのキー値
           03  CONST-JOB-SHELLID           PIC X(08)   VALUE "ORCBS01".
           03  CONST-JOB-JOBID             PIC 9(07)   VALUE ZERO.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1039.INC".
           COPY    "CPSK3003.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
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
           COPY    "ORCA23SCRAREA.INC".
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
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-S01KYOUTU
           MOVE    SPA-FREE2       TO  SPA-S04
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
               WHEN    "LINK"      ALSO    ANY
                   PERFORM 100-INIT-SEC
      *        画面遷移
               WHEN     "PUTG"     ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *        入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-S01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-S04         TO  SPA-FREE2
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
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "SERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
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
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    SPACE           TO  MCP-PUTTYPE.
           MOVE    "S04    "       TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1               TO  FLG-END
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
      *----(03.04.01)--UPD-START---
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"   ALSO    "B01"
               INITIALIZE              SPA-S01-SHIHARAI-PARA
               MOVE    SPACE       TO  SPA-S01-SHR-ORDER
               PERFORM 4101-GMN-SPA-SET-SEC
               PERFORM 210-BACK
           WHEN    "CLICKED"   ALSO    "B02"
               INITIALIZE              SPA-S04-AREA
               PERFORM 310-SPASET-SEC
      *    プレビュー
           WHEN    "CLICKED"   ALSO    "B10"
               PERFORM 410-PRINT-MAE-CHK-SEC
               IF    ( SPA-ERRCD   =   SPACE )
                   INITIALIZE              SPA-S01-SHIHARAI-PARA
                   MOVE    "PV"        TO  SPA-S01-SHR-ORDER
                   PERFORM 400-PRINT-SEC
               END-IF
      *    登録
           WHEN    "CLICKED"   ALSO    "B12"
               PERFORM 410-PRINT-MAE-CHK-SEC
               IF    ( SPA-ERRCD   =   SPACE )
                   INITIALIZE              SPA-S01-SHIHARAI-PARA
                   MOVE    "OK"        TO  SPA-S01-SHR-ORDER
                   PERFORM 400-PRINT-SEC
               END-IF
           END-EVALUATE
      *----(03.04.01)--UPD-END-----
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
      *    EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    WHEN    "ACTIVATE"      ALSO    "XXXX"
      *    WHEN    OTHER
      *
               MOVE    ZERO        TO  SPA-GMN-CUR
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
      *
      *    END-EVALUATE
      *
           .
       200-ENTRY-EXT.
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
           IF    ( SPA-ERRCD       =   SPACE )
      *        基本チェック処理
               PERFORM 4102-KIHON-CHK-SEC
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    S04-SIHARAIKBN  TO  SPA-GMN-SIHARAIKBN-G
           MOVE    S04-NYUGAIKBN   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    S04-SRYKBN      TO  SPA-GMN-SRYKBN-G
           MOVE    S04-STRSRYYMD   TO  SPA-GMN-STRSRYYMD
           MOVE    S04-ENDSRYYMD   TO  SPA-GMN-ENDSRYYMD
           MOVE    S04-KIKANKBN    TO  SPA-GMN-KIKANKBN-G
           MOVE    S04-DSPKBN      TO  SPA-GMN-DSPKBN-G
           MOVE    S04-SUMKBN      TO  SPA-GMN-SUMKBN-G
           MOVE    S04-SYORIKBN    TO  SPA-GMN-SYORIKBN-G
           MOVE    S04-JIHIKBN     TO  SPA-GMN-JIHIKBN-G
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4001-SIHARAIKBN-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4001-NYUGAIKBN-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4001-SRYKBN-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4001-STRSRYYMD-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4001-ENDSRYYMD-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4001-KIKANKBN-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4001-DSPKBN-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4001-SUMKBN-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4001-SYORIKBN-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4001-JIHIKBN-SEC
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           INITIALIZE                      SPA-S04-AREA
           MOVE    "1"                 TO  SPA-S04-SAVED
      *
           PERFORM 3101-SPASET-SEC
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPA-GMN-SIHARAIKBN  TO  SPA-S04-SIHARAIKBN
           MOVE    SPA-GMN-NYUGAIKBN   TO  SPA-S04-NYUGAIKBN
           MOVE    SPA-GMN-SRYKBN      TO  SPA-S04-SRYKBN
           MOVE    SPA-NAI-STRSRYYMD   TO  SPA-S04-STRSRYYMD
           MOVE    SPA-NAI-ENDSRYYMD   TO  SPA-S04-ENDSRYYMD
           MOVE    SPA-GMN-KIKANKBN    TO  SPA-S04-KIKANKBN
           MOVE    SPA-GMN-DSPKBN      TO  SPA-S04-DSPKBN
           MOVE    SPA-GMN-SUMKBN      TO  SPA-S04-SUMKBN
           MOVE    SPA-GMN-SYORIKBN    TO  SPA-S04-SYORIKBN
           MOVE    SPA-GMN-JIHIKBN     TO  SPA-S04-JIHIKBN
      *
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "S02"           TO  SPA-SAKIPG
           MOVE    "S04"           TO  SPA-MOTOPG
      *
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE   "JOIN"           TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *************************************************************
      *    初期画面処理
      *************************************************************
       300-SCREEN-SEC              SECTION.
      *
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
           END-IF
      *
      *     EVALUATE    SPA-MOTOPG
      *     WHEN    "SID2"
      *         PERFORM 330-SID2-SET-SEC
      *         GO  TO  300-SCREEN-EXT
      *     END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *----(03.04.01)--UPD-START---
      *****************************************************************
      *    確認画面（ＳＩＤ２）ＯＫ処理
      *****************************************************************
      * 330-SID2-SET-SEC            SECTION.
      *
      *     IF      SPA-SID1-FLG    =   "OK" OR "NO"
      *         IF      SPA-SIDCD   =   "1001"
      *             PERFORM 4900-JOBKANRICHK-SEC
      *             IF    ( SPA-ERRCD   =    SPACE )
      *                 PERFORM 400-PRINT-SEC
      *             END-IF
      *         END-IF
      *     END-IF
      *
      *     MOVE    SPACE           TO  SPA-SID1-FLG
      *     MOVE    SPACE           TO  SPA-SIDCD
      *
      *     .
      * 330-SID2-SET-EXT.
      *     EXIT.
      *
      *----(03.04.01)--UPD-END-----
      *************************************************************
      *    画面編集処理
      *************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-AREA
                                       SPA-NAI-AREA
      *
           MOVE    1                   TO  FLG-INIT
      *
           PERFORM 3101-COMBO-SYOKI-SEC
      *
           INITIALIZE                      SYS-1039-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1039-HOSPNUM
           MOVE    "1039"              TO  SYS-1039-KANRICD
           MOVE    "*"                 TO  SYS-1039-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1039-STYUKYMD
                                           SYS-1039-EDYUKYMD
           MOVE    SYS-1039-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-1039-REC
           ELSE
               INITIALIZE                  SYS-1039-REC
           END-IF
      *
           IF    ( SPA-S04-SAVED       =   "1" )
               MOVE    SPA-S04-SIHARAIKBN  TO  SPA-GMN-SIHARAIKBN
               MOVE    SPA-S04-NYUGAIKBN   TO  SPA-GMN-NYUGAIKBN
               MOVE    SPA-S04-SRYKBN      TO  SPA-GMN-SRYKBN
               MOVE    SPA-S04-STRSRYYMD   TO  SPA-GMN-STRSRYYMD
               MOVE    SPA-S04-ENDSRYYMD   TO  SPA-GMN-ENDSRYYMD
               MOVE    SPA-S04-KIKANKBN    TO  SPA-GMN-KIKANKBN
               MOVE    SPA-S04-DSPKBN      TO  SPA-GMN-DSPKBN
               MOVE    SPA-S04-SUMKBN      TO  SPA-GMN-SUMKBN
               MOVE    SPA-S04-SYORIKBN    TO  SPA-GMN-SYORIKBN
               MOVE    SPA-S04-JIHIKBN     TO  SPA-GMN-JIHIKBN

           ELSE
               MOVE    SYS-1039-S04SHIHARAIKBN TO  SPA-GMN-SIHARAIKBN
               MOVE    SYS-1039-S04NYUGAIKBN   TO  SPA-GMN-NYUGAIKBN
               MOVE    SYS-1039-S04SRYKBN      TO  SPA-GMN-SRYKBN
               MOVE    SYS-1039-S04KIKANKBN    TO  SPA-GMN-KIKANKBN
               MOVE    SYS-1039-S04DSPKBN      TO  SPA-GMN-DSPKBN
               MOVE    SYS-1039-S04SUMKBN      TO  SPA-GMN-SUMKBN
               MOVE    SYS-1039-S04SYORIKBN    TO  SPA-GMN-SYORIKBN
               MOVE    SYS-1039-S04JIHIPRTKBN  TO  SPA-GMN-JIHIKBN
           END-IF
      *
           MOVE    "00000006"      TO  WRK-KBNCD
           MOVE    "ORCHCM31"      TO  WRK-PRTPG
           PERFORM 3101-APIPRINT-CHK-SEC
           IF    ( FLG-API         =   1 )
               MOVE    "1"         TO  SPA-NAI-HCM31API
           END-IF
      *
           MOVE    "00000007"      TO  WRK-KBNCD
           MOVE    "ORCHCM32"      TO  WRK-PRTPG
           PERFORM 3101-APIPRINT-CHK-SEC
           IF    ( FLG-API         =   1 )
               MOVE    "1"         TO  SPA-NAI-HCM32API
           END-IF
      *
           PERFORM 3101-SPASET-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *************************************************************
      *    画面編集処理
      *************************************************************
       3101-SPASET-SEC                     SECTION.
      *
           PERFORM 4001-SIHARAIKBN-SEC
           PERFORM 4001-NYUGAIKBN-SEC
           PERFORM 4001-SRYKBN-SEC
      *
           IF    ( SPA-S04-SAVED       =   SPACE )
               PERFORM 4002-SRYKBN-SET-SEC
           END-IF
      *
           PERFORM 4001-STRSRYYMD-SEC
           PERFORM 4001-ENDSRYYMD-SEC
           PERFORM 4001-KIKANKBN-SEC
           PERFORM 4001-DSPKBN-SEC
           PERFORM 4001-SUMKBN-SEC
           PERFORM 4001-SYORIKBN-SEC
           PERFORM 4001-JIHIKBN-SEC
      *
           .
       3101-SPASET-EXT.
           EXIT.
      *************************************************************
      *    ＡＰＩ印刷チェック
      *************************************************************
       3101-APIPRINT-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-API
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    WRK-KBNCD           TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYSKANRI-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              CONTINUE
           ELSE
              MOVE    WRK-PRTPG        TO  ORCSPRTNM-PRTPG
           END-IF
      *
           IF    ( WRK-PRTPG   =   ORCSPRTNM-PRTPG )
               INITIALIZE                  ORCSPRGOPTIONAREA
               MOVE    WRK-PRTPG       TO  ORCSPRGOPTION-PRGID
               MOVE    "API_DATA"      TO  ORCSPRGOPTION-IN-DATA
               CALL    "ORCSPRGOPTION"     USING
                                           SPA-AREA
                                           ORCSPRGOPTIONAREA
               IF    ( ORCSPRGOPTION-OT-PARA
                                       =    "1" )
                   MOVE    1           TO  FLG-API
               END-IF
           END-IF
      *
           .
       3101-APIPRINT-CHK-EXT.
           EXIT.
      *************************************************************
      *    コンボ初期処理
      *************************************************************
       3101-COMBO-SYOKI-SEC        SECTION.
      *
           PERFORM 3101-SIHARAILIST-SET-SEC
      *
           PERFORM 3101-NYUGAILIST-SET-SEC
      *
           PERFORM 3101-SRYLIST-SET-SEC
      *
           PERFORM 3101-KIKANLIST-SET-SEC
      *
           PERFORM 3101-DSPLIST-SET-SEC
      *
           PERFORM 3101-SUMKBNLIST-SET-SEC
      *
           PERFORM 3101-SYORILIST-SET-SEC
      *
           PERFORM 3101-JIHILIST-SET-SEC
      *
           .
       3101-COMBO-SYOKI-EXT.
           EXIT.
      *************************************************************
      *    帳票指定区分コンボ設定処理
      *************************************************************
       3101-SIHARAILIST-SET-SEC    SECTION.
      *
           INITIALIZE                  SPA-GMN-SIHARAILIST-G
      *
           MOVE    "0 発行しない"  TO  SPA-GMN-SIHARAILIST(1)
           MOVE    "1 日別証明書"  TO  SPA-GMN-SIHARAILIST(2)
           MOVE    "2 月別証明書"  TO  SPA-GMN-SIHARAILIST(3)
      *
           MOVE    3               TO  SPA-GMN-SIHARAILIST-MAX
      *
           .
       3101-SIHARAILIST-SET-EXT.
           EXIT.
      *************************************************************
      *    入外区分コンボ設定処理
      *************************************************************
       3101-NYUGAILIST-SET-SEC     SECTION.
      *
           INITIALIZE                  SPA-GMN-NYUGAILIST-G
      *
           MOVE    "0 全て"        TO  SPA-GMN-NYUGAILIST(1)
           MOVE    "1 入院"        TO  SPA-GMN-NYUGAILIST(2)
           MOVE    "2 外来"        TO  SPA-GMN-NYUGAILIST(3)
      *
           MOVE    3               TO  SPA-GMN-NYUGAILIST-MAX
      *
           .
       3101-NYUGAILIST-SET-EXT.
           EXIT.
      *************************************************************
      *    期間指定区分コンボ設定処理
      *************************************************************
       3101-SRYLIST-SET-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-SRYLIST-G
      *
           MOVE    "1 年月日"      TO  SPA-GMN-SRYLIST(1)
           MOVE    "2 年"          TO  SPA-GMN-SRYLIST(2)
           MOVE    "3 年度"        TO  SPA-GMN-SRYLIST(3)
      *
           MOVE    3               TO  SPA-GMN-SRYLIST-MAX
      *
           .
       3101-SRYLIST-SET-EXT.
           EXIT.
      *************************************************************
      *    期間区分コンボ設定処理
      *************************************************************
       3101-KIKANLIST-SET-SEC      SECTION.
      *
           INITIALIZE                  SPA-GMN-KIKANLIST-G
      *
           MOVE    "1 診療期間"    TO  SPA-GMN-KIKANLIST(1)
           MOVE    "2 支払期間"    TO  SPA-GMN-KIKANLIST(2)
      *
           MOVE    2               TO  SPA-GMN-KIKANLIST-MAX
      *
           .
       3101-KIKANLIST-SET-EXT.
           EXIT.
      *************************************************************
      *    印字区分設定処理
      *************************************************************
       3101-DSPLIST-SET-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-DSPLIST-G
      *
           MOVE    "1 印字する"            TO  SPA-GMN-DSPLIST(1)
           MOVE    "2 印字しない"          TO  SPA-GMN-DSPLIST(2)
           MOVE    "3 医保分のみ印字する"  TO  SPA-GMN-DSPLIST(3)
           MOVE    "4 入金分のみ印字する"  TO  SPA-GMN-DSPLIST(4)
      *
           MOVE    4               TO  SPA-GMN-DSPLIST-MAX
      *
           .
       3101-DSPLIST-SET-EXT.
           EXIT.
      *************************************************************
      *    期間開始前診療分の支払い
      *************************************************************
       3101-SUMKBNLIST-SET-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-SUMKBNLIST-G
      *
           MOVE    "1 支払額に含める"      TO  SPA-GMN-SUMKBNLIST(1)
      *
           MOVE    "2 支払額に含めない"    TO  SPA-GMN-SUMKBNLIST(2)
      *
           MOVE    2               TO  SPA-GMN-SUMKBNLIST-MAX
      *
           .
       3101-SUMKBNLIST-SET-EXT.
           EXIT.
      *************************************************************
      *    処理区分設定処理
      *************************************************************
       3101-SYORILIST-SET-SEC      SECTION.
      *
           INITIALIZE                  SPA-GMN-SYORILIST-G
      *
           MOVE    "1 支払額に含める"      TO  SPA-GMN-SYORILIST(1)
      *
           MOVE    "2 支払額に含めない"    TO  SPA-GMN-SYORILIST(2)
      *
           MOVE    2               TO  SPA-GMN-SYORILIST-MAX
      *
           .
       3101-SYORILIST-SET-EXT.
           EXIT.
      *
      *************************************************************
      *    自費発行区分コンボ設定処理
      *************************************************************
       3101-JIHILIST-SET-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-JIHILIST-G
      *
           MOVE    "0 発行しない"  TO  SPA-GMN-JIHILIST(1)
           MOVE    "1 日別明細書"  TO  SPA-GMN-JIHILIST(2)
           MOVE    "2 月別明細書"  TO  SPA-GMN-JIHILIST(3)
      *
           MOVE    3               TO  SPA-GMN-JIHILIST-MAX
      *
           .
       3101-JIHILIST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票設定区分
      *****************************************************************
       4001-SIHARAIKBN-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM VARYING IDX     FROM   1   BY  1
                   UNTIL ( IDX     >   SPA-GMN-SIHARAILIST-MAX )
                    OR   ( FLG-OK  NOT =   ZERO )
               IF    ( SPA-GMN-SIHARAIKBN  =   SPA-GMN-SIHARAIKBNL(IDX))
                   MOVE    SPA-GMN-SIHARAILIST(IDX)
                                              TO  SPA-GMN-SIHARAIKBN-G
                   MOVE    1                  TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF    ( FLG-OK          =   ZERO )
               MOVE    SPA-GMN-SIHARAILIST(2)
                                   TO  SPA-GMN-SIHARAIKBN-G
           END-IF
      *
           .
       4001-SIHARAIKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分
      *****************************************************************
       4001-NYUGAIKBN-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM VARYING IDX     FROM   1   BY  1
                   UNTIL ( IDX     >   SPA-GMN-NYUGAILIST-MAX )
                    OR   ( FLG-OK  NOT =   ZERO )
               IF    ( SPA-GMN-NYUGAIKBN  =   SPA-GMN-NYUGAIKBNL(IDX))
                   MOVE    SPA-GMN-NYUGAILIST(IDX)
                                              TO  SPA-GMN-NYUGAIKBN-G
                   MOVE    1                  TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF    ( FLG-OK          =   ZERO )
                   MOVE    SPA-GMN-NYUGAILIST(1)
                                              TO  SPA-GMN-NYUGAIKBN-G
           END-IF
      *
           .
       4001-NYUGAIKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    期間指定区分
      *****************************************************************
       4001-SRYKBN-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM VARYING IDX     FROM   1   BY  1
                   UNTIL ( IDX     >   SPA-GMN-SRYLIST-MAX )
                    OR   ( FLG-OK  NOT =   ZERO )
               IF    ( SPA-GMN-SRYKBN  =   SPA-GMN-SRYKBNL(IDX) )
                   MOVE    SPA-GMN-SRYLIST(IDX)   TO  SPA-GMN-SRYKBN-G
                   MOVE    1                      TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF    ( FLG-OK          =   ZERO )
                   MOVE    SPA-GMN-SRYLIST(1)     TO  SPA-GMN-SRYKBN-G
           END-IF
      *
           IF    ( WRK-MCP-WIDGET  =   "SRYKBN" )
               PERFORM 4002-SRYKBN-SET-SEC
           END-IF
      *
           .
       4001-SRYKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    期間指定区分セット
      *****************************************************************
       4002-SRYKBN-SET-SEC         SECTION.
      *
           MOVE    SPACE           TO  SPA-GMN-STRSRYYMD
                                       SPA-NAI-STRSRYYMD
                                       SPA-GMN-ENDSRYYMD
                                       SPA-NAI-ENDSRYYMD
      *
           EVALUATE    SPA-GMN-SRYKBN
               WHEN    "1"
                   MOVE    SPA-SYSYMDWH    TO  SPA-GMN-STRSRYYMD
                                               SPA-GMN-ENDSRYYMD
                   MOVE    SPA-SYSYMD      TO  SPA-NAI-STRSRYYMD
                                               SPA-NAI-ENDSRYYMD
               WHEN    "2"
                   MOVE    SPA-SYSYMDWH(1:3)   TO  SPA-GMN-STRSRYYMD
                   MOVE    SPA-SYSYMD(1:4)     TO  SPA-NAI-STRSRYYMD
               WHEN    "3"
                   IF      SPA-SYSYMD(5:4)     <   "0401"
                       MOVE    SPA-SYSYMD      TO  WRK-SYMD
                       COMPUTE WRK-SYY     =   WRK-SYY -   1
      *                MOVE    "0401"      TO  WRK-SYMD
                       MOVE    WRK-SYMD    TO  WRK-LNKYMD
                       PERFORM 800-ORCSGDAY-SEC
                       IF    ( SPA-ERRCD           =   SPACE )
                           MOVE    WRK-LNKYMD(1:3) TO  SPA-GMN-STRSRYYMD
                           MOVE    WRK-SYMD(1:4)   TO  SPA-NAI-STRSRYYMD
                       END-IF
                   ELSE
                       MOVE    SPA-SYSYMDWH(1:3)   TO  SPA-GMN-STRSRYYMD
                       MOVE    SPA-SYSYMD(1:4)     TO  SPA-NAI-STRSRYYMD
                   END-IF
           END-EVALUATE
      *
           .
       4002-SRYKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    開始診療年月日
      *****************************************************************
       4001-STRSRYYMD-SEC          SECTION.
      *
           MOVE    SPACE           TO  SPA-NAI-STRSRYYMD
      *
           IF    ( SPA-GMN-STRSRYYMD   =   SPACE )
               IF    ( SPA-GMN-SRYKBN  =   "1" )
                   MOVE    SPA-SYSYMDWH    TO  SPA-GMN-STRSRYYMD
                   MOVE    SPA-SYSYMD      TO  SPA-NAI-STRSRYYMD
               ELSE
                   MOVE    SPA-SYSYMDWH(1:3)
                                           TO  SPA-GMN-STRSRYYMD
                   MOVE    SPA-SYSYMD(1:4) TO  SPA-NAI-STRSRYYMD
               END-IF
           ELSE
      *
               MOVE    SPA-GMN-STRSRYYMD   TO  WRK-LNKYMD
               IF    ( SPA-GMN-SRYKBN  =   "1" )
                   PERFORM 831-YMDCONV12-SEC
               ELSE
                   PERFORM 800-ORCSGDAY-SEC
               END-IF
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( SPA-GMN-SRYKBN  =   "1" )
                       MOVE    WRK-LNKYMD      TO  SPA-GMN-STRSRYYMD
                       MOVE    WRK-SYMD        TO  SPA-NAI-STRSRYYMD
                   ELSE
                       MOVE    WRK-LNKYMD(1:3) TO  SPA-GMN-STRSRYYMD
                       MOVE    WRK-SYMD(1:4)   TO  SPA-NAI-STRSRYYMD
                   END-IF
               ELSE
                   MOVE    "0004"      TO  SPA-ERRCD
                   MOVE    4           TO  SPA-GMN-CUR
               END-IF
      *
           END-IF
      *
           .
       4001-STRSRYYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了診療年月日
      *****************************************************************
       4001-ENDSRYYMD-SEC          SECTION.
      *
           MOVE    SPACE           TO  SPA-NAI-ENDSRYYMD
      *
           IF    ( SPA-GMN-SRYKBN  =   "1" )
      *
               IF    ( SPA-GMN-ENDSRYYMD   =   SPACE )
                   MOVE    SPA-SYSYMDWH    TO  SPA-GMN-ENDSRYYMD
                   MOVE    SPA-SYSYMD      TO  SPA-NAI-ENDSRYYMD
               ELSE
      *
                   MOVE    SPA-GMN-ENDSRYYMD   TO  WRK-LNKYMD
                   PERFORM 831-YMDCONV12-SEC
                   IF    ( SPA-ERRCD           =   SPACE )
                       MOVE    WRK-LNKYMD  TO  SPA-GMN-ENDSRYYMD
                       MOVE    WRK-SYMD    TO  SPA-NAI-ENDSRYYMD
                   ELSE
                       MOVE    "0005"      TO  SPA-ERRCD
                       MOVE    5           TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
       4001-ENDSRYYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    期間区分
      *****************************************************************
       4001-KIKANKBN-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL ( IDX     >   SPA-GMN-KIKANLIST-MAX )
                    OR   ( FLG-OK  NOT =   ZERO )
               IF    ( SPA-GMN-KIKANKBN   =   SPA-GMN-KIKANKBNL(IDX) )
                   MOVE    SPA-GMN-KIKANLIST(IDX) TO  SPA-GMN-KIKANKBN-G
                   MOVE    1                      TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF    ( FLG-OK          =   ZERO )
                   MOVE    SPA-GMN-KIKANLIST(1)   TO  SPA-GMN-KIKANKBN-G
           END-IF
      *
           IF    ( SPA-GMN-KIKANKBN    =   "1" )
               IF    ( FLG-INIT        =   ZERO )
                AND  ( S04-LBLSUMKBN   =   SPACE )
                   MOVE    SPA-GMN-DSPLIST(1)
                                       TO  SPA-GMN-DSPKBN-G
                   MOVE    SPA-GMN-SUMKBNLIST (2)
                                       TO  SPA-GMN-SUMKBN-G
                   MOVE    SPA-GMN-SYORILIST (1)
                                       TO  SPA-GMN-SYORIKBN-G
               END-IF
           ELSE
               MOVE    SPACE           TO  SPA-GMN-SYORIKBN-G
                                           SPA-GMN-SUMKBN-G
               MOVE    SPA-GMN-DSPLIST(2)
                                       TO  SPA-GMN-DSPKBN-G
               MOVE    SPA-GMN-JIHILIST(1)
                                       TO  SPA-GMN-JIHIKBN-G
           END-IF
      *
           .
       4001-KIKANKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    印字区分
      *****************************************************************
       4001-DSPKBN-SEC             SECTION.
      *
           IF    ( SPA-GMN-KIKANKBN    =   "1" )
      *
               MOVE    ZERO            TO  FLG-OK
      *
               PERFORM VARYING IDX     FROM   1   BY  1
                       UNTIL ( IDX     >   SPA-GMN-DSPLIST-MAX )
                        OR   ( FLG-OK  NOT =   ZERO )
                   IF    ( SPA-GMN-DSPKBN  =   SPA-GMN-DSPKBNL(IDX) )
                       MOVE    SPA-GMN-DSPLIST(IDX)
                                               TO  SPA-GMN-DSPKBN-G
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
      *
               IF    ( FLG-OK          =   ZERO )
                   MOVE    SPA-GMN-DSPLIST(1)  TO  SPA-GMN-DSPKBN-G
               END-IF
      *
           END-IF
      *
           .
       4001-DSPKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    期間開始診療分の支払い
      *****************************************************************
       4001-SUMKBN-SEC           SECTION.
      *
           IF    ( SPA-GMN-KIKANKBN    =   "1" )
      *
               MOVE    ZERO            TO  FLG-OK
      *
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL ( IDX     >   SPA-GMN-SUMKBNLIST-MAX )
                        OR   ( FLG-OK
                                   NOT =   ZERO )
                   IF    ( SPA-GMN-SUMKBN
                                       =   SPA-GMN-SUMKBNL(IDX) )
                       MOVE    SPA-GMN-SUMKBNLIST(IDX)
                                       TO  SPA-GMN-SUMKBN-G
                       MOVE    1       TO  FLG-OK
                   END-IF
               END-PERFORM
      *
               IF    ( FLG-OK          =   ZERO )
                   MOVE    SPA-GMN-SUMKBNLIST(2)
                                       TO  SPA-GMN-SUMKBN-G
               END-IF
           END-IF
      *
           .
       4001-SUMKBN-EXT.
           EXIT.
      *****************************************************************
      *    処理区分
      *****************************************************************
       4001-SYORIKBN-SEC           SECTION.
      *
           IF    ( SPA-GMN-KIKANKBN    =   "1" )
      *
               MOVE    ZERO            TO  FLG-OK
      *
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL ( IDX     >   SPA-GMN-SYORILIST-MAX )
                        OR   ( FLG-OK
                                   NOT =   ZERO )
                   IF    ( SPA-GMN-SYORIKBN
                                       =   SPA-GMN-SYORIKBNL(IDX) )
                       MOVE    SPA-GMN-SYORILIST(IDX)
                                       TO  SPA-GMN-SYORIKBN-G
                       MOVE    1       TO  FLG-OK
                   END-IF
               END-PERFORM
      *
               IF    ( FLG-OK          =   ZERO )
                   MOVE    SPA-GMN-SYORILIST(1)
                                       TO  SPA-GMN-SYORIKBN-G
               END-IF
           END-IF
      *
           .
       4001-SYORIKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費区分
      *****************************************************************
       4001-JIHIKBN-SEC            SECTION.
      *
           IF    ( SPA-GMN-KIKANKBN    =   "1" )
      *
               MOVE    ZERO            TO  FLG-OK
      *
               PERFORM VARYING IDX     FROM   1   BY  1
                       UNTIL ( IDX     >   SPA-GMN-JIHILIST-MAX )
                        OR   ( FLG-OK  NOT =   ZERO )
                   IF    ( SPA-GMN-JIHIKBN  =   SPA-GMN-JIHIKBNL(IDX))
                       MOVE    SPA-GMN-JIHILIST(IDX)
                                           TO  SPA-GMN-JIHIKBN-G
                       MOVE    1           TO  FLG-OK
                   END-IF
               END-PERFORM
      *
               IF    ( FLG-OK          =   ZERO )
                   MOVE    SPA-GMN-JIHILIST(1)
                                              TO  SPA-GMN-JIHIKBN-G
               END-IF
           END-IF
      *
           .
       4001-JIHIKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷前チェック
      *****************************************************************
       410-PRINT-MAE-CHK-SEC       SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  410-PRINT-MAE-CHK-EXT
           END-IF
      *
      *    期間範囲大小チェック
           IF    ( SPA-GMN-SRYKBN       =   "1" )
               IF    ( SPA-NAI-STRSRYYMD   >   SPA-NAI-ENDSRYYMD )
                   MOVE    "0021"          TO  SPA-ERRCD
                   MOVE    4               TO  SPA-GMN-CUR
                   GO  TO  410-PRINT-MAE-CHK-EXT
               END-IF
      *
      *        一年オーバーチェック
               COMPUTE WRK-YY      =   SPA-NAI-ENDYY   -   SPA-NAI-STRYY
               COMPUTE WRK-MM      =   SPA-NAI-ENDMM   -   SPA-NAI-STRMM
      *
               IF    ( WRK-YY          <=  1 )
                   IF    ( WRK-YY      =   1 ) AND   ( WRK-MM  >=  0 )
                       MOVE    "0022"      TO  SPA-ERRCD
                       MOVE    4           TO  SPA-GMN-CUR
                       GO  TO  410-PRINT-MAE-CHK-EXT
                   END-IF
               ELSE
                   MOVE    "0022"          TO  SPA-ERRCD
                   MOVE    4               TO  SPA-GMN-CUR
                   GO  TO  410-PRINT-MAE-CHK-EXT
               END-IF
           END-IF
      *
           PERFORM 4900-JOBKANRICHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  410-PRINT-MAE-CHK-EXT
           END-IF
      *
      *----(03.04.01)--UPD-START---
      *     MOVE    "1001"              TO  SPA-SIDCD
      *----(03.04.01)--UPD-END-----
      *
           .
       410-PRINT-MAE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       400-PRINT-SEC               SECTION.
      *
      *----(03.04.01)--UPD-START---
      *     INITIALIZE                  SPA-S01-SHIHARAI-PARA
      *
      *     MOVE    SPA-SID1-FLG        TO  SPA-S01-SHR-ORDER
      *----(03.04.01)--UPD-END-----
      *
           MOVE    SPA-GMN-NYUGAIKBN   TO  SPA-S01-SHR-NYUGAIKBN
           MOVE    SPA-GMN-KIKANKBN    TO  SPA-S01-SHR-KIKANKBN
           MOVE    SPA-GMN-DSPKBN      TO  SPA-S01-SHR-DSPKBN
           MOVE    SPA-GMN-SUMKBN      TO  SPA-S01-SHR-SUMKBN
           MOVE    SPA-GMN-SYORIKBN    TO  SPA-S01-SHR-SYORIKBN
      *
           EVALUATE    SPA-GMN-SRYKBN
               WHEN    "1"
                   MOVE    SPA-NAI-STRSRYYMD
                                       TO  SPA-S01-SHR-STYMD
                   MOVE    SPA-NAI-ENDSRYYMD
                                       TO  SPA-S01-SHR-EDYMD
               WHEN    "2"
                   MOVE    SPA-NAI-STRSRYYMD
                                       TO  SPA-S01-SHR-STYMD
                                           SPA-S01-SHR-EDYMD
                   MOVE    "0101"      TO  SPA-S01-SHR-STYMD (5:)
                   MOVE    "1231"      TO  SPA-S01-SHR-EDYMD (5:)
               WHEN    "3"
                   MOVE    SPA-NAI-STRSRYYMD
                                       TO  SPA-S01-SHR-STYMD
                   MOVE    SPA-NAI-STRSRYYMD
                                       TO  WRK-SYMD
                   COMPUTE WRK-SYY     =   WRK-SYY +   1
                   MOVE    WRK-SYMD    TO  SPA-S01-SHR-EDYMD
                   MOVE    "0401"      TO  SPA-S01-SHR-STYMD (5:)
                   MOVE    "0331"      TO  SPA-S01-SHR-EDYMD (5:)
           END-EVALUATE
      *
      *    支払証明書フラグ
           EVALUATE    SPA-GMN-SIHARAIKBN
               WHEN    "0"
                   MOVE    0           TO  SPA-S01-SHR-HCM31FLG
                   MOVE    0           TO  SPA-S01-SHR-HCM32FLG
               WHEN    "1"
                   MOVE    1           TO  SPA-S01-SHR-HCM31FLG
                   MOVE    0           TO  SPA-S01-SHR-HCM32FLG
               WHEN    "2"
                   MOVE    0           TO  SPA-S01-SHR-HCM31FLG
                   MOVE    1           TO  SPA-S01-SHR-HCM32FLG
           END-EVALUATE
      *
           IF  ( SPA-S01-SHR-ORDER     =   "PV" )
               PERFORM 4001-APIPV-CHK-SEC
               IF    ( FLG-API         =   1 )
                   MOVE    0           TO  SPA-S01-SHR-HCM31FLG
                   MOVE    0           TO  SPA-S01-SHR-HCM32FLG
               END-IF
           END-IF
      *
      *    自費内訳明細書フラグ
           EVALUATE    SPA-GMN-JIHIKBN
               WHEN    "0"
                   MOVE    0           TO  SPA-S01-SHR-HCM50FLG
                   MOVE    "0"         TO  SPA-S01-SHR-JIHSUMKBN
               WHEN    "1"
                   MOVE    1           TO  SPA-S01-SHR-HCM50FLG
                   MOVE    "0"         TO  SPA-S01-SHR-JIHSUMKBN
               WHEN    "2"
                   MOVE    1           TO  SPA-S01-SHR-HCM50FLG
                   MOVE    "1"         TO  SPA-S01-SHR-JIHSUMKBN
           END-EVALUATE
      *
           PERFORM 210-BACK
      *
           .
       400-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    API印刷時プレビュ選択ーチェック処理
      *****************************************************************
       4001-APIPV-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-API
      *
           IF    ( SPA-GMN-SIHARAIKBN     =   "1" )
            AND  ( SPA-NAI-HCM31API       =   "1" )
               MOVE    1               TO  FLG-API
           END-IF
      *
           IF    ( SPA-GMN-SIHARAIKBN     =   "2" )
            AND  ( SPA-NAI-HCM32API       =   "1" )
               MOVE    1               TO  FLG-API
           END-IF
      *
           .
       4001-APIPV-CHK-EXT.
           EXIT.
      *----(03.04.01)--UPD-START---
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       4900-JOBKANRICHK-SEC            SECTION.
      *
      *    ジョブ管理チェック
           MOVE    SPACE               TO  WRK-ERRMSG
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    CONST-3003-KBNCD    TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL ( IDX >       SYS-3003-JOBID )
                        OR   ( WRK-ERRMSG  NOT =   SPACE )
      *
      *            ジョブ管理チェック処理
                   MOVE    "CHK"               TO  SJOBKANRI-MODE
                   INITIALIZE                      JOBKANRI-REC
                   MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
                   MOVE    IDX                 TO  JOB-JOBID
                   MOVE    CONST-JOB-SHELLID   TO  JOB-SHELLID
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
                   IF    ( SJOBKANRI-RETURN    =   ZERO )
                       IF    ( JOB-ENDYMD      =   SPACE )
                           MOVE    "7777"      TO  SPA-ERRCD
                           STRING  "処理中です【"  DELIMITED  BY  SIZE
                                   JOB-SHELLMSG    DELIMITED  BY  SPACE
                                   "】"            DELIMITED  BY  SIZE
                                                   INTO  WRK-ERRMSG
                           END-STRING
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       4900-JOBKANRICHK-EXT.
           EXIT.
      *
      *----(03.04.01)--UPD-END-----
      *************************************************************
      *    自画面編集処理
      *************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF.
      *----(03.04.01)--UPD-START---
      *     IF      SPA-SIDCD       NOT =   SPACE
      *         PERFORM 520-SIDSET-SEC
      *         GO  TO  500-SET-SCREEN-EXT
      *     END-IF
      *----(03.04.01)--UPD-END-----
           MOVE    SPACE          TO  LNK-KYOUTU
      *
           MOVE    "CURRENT"      TO  MCP-PUTTYPE
           MOVE    "S04    "      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *************************************************************
      *    画面編集処理
      *************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE                  S04
      *
      *    帳票指定区分コンボ
           MOVE    SPA-GMN-SIHARAIKBN-G    TO  S04-SIHARAIKBN
           MOVE    SPA-GMN-SIHARAILIST-MAX TO  S04-SIHARAI-COUNT
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL ( IDX     >   SPA-GMN-SIHARAILIST-MAX )
               MOVE    SPA-GMN-SIHARAILIST(IDX)
                                       TO  S04-SIHARAI-LIST(IDX)
           END-PERFORM
      *
      *    自費区分コンボ
           MOVE    SPA-GMN-JIHIKBN-G       TO  S04-JIHIKBN
           MOVE    SPA-GMN-JIHILIST-MAX    TO  S04-JIHI-COUNT
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL ( IDX     >   SPA-GMN-JIHILIST-MAX )
               MOVE    SPA-GMN-JIHILIST(IDX)
                                       TO  S04-JIHI-LIST(IDX)
           END-PERFORM
      *
      *    入外区分コンボ
           MOVE    SPA-GMN-NYUGAIKBN-G     TO  S04-NYUGAIKBN
           MOVE    SPA-GMN-NYUGAILIST-MAX  TO  S04-NYUGAI-COUNT
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL ( IDX     >   SPA-GMN-NYUGAILIST-MAX )
               MOVE    SPA-GMN-NYUGAILIST(IDX)
                                       TO  S04-NYUGAI-LIST(IDX)
           END-PERFORM
      *
      *    期間指定区分コンボ
           MOVE    SPA-GMN-SRYKBN-G        TO  S04-SRYKBN
           MOVE    SPA-GMN-SRYLIST-MAX     TO  S04-SRY-COUNT
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL ( IDX     >   SPA-GMN-SRYLIST-MAX )
               MOVE    SPA-GMN-SRYLIST(IDX)
                                       TO  S04-SRY-LIST(IDX)
           END-PERFORM
      *
           MOVE    SPA-GMN-STRSRYYMD   TO  S04-STRSRYYMD
           MOVE    SPA-GMN-ENDSRYYMD   TO  S04-ENDSRYYMD
      *
      *    期間区分コンボ
           MOVE    SPA-GMN-KIKANKBN-G      TO  S04-KIKANKBN
           MOVE    SPA-GMN-KIKANLIST-MAX   TO  S04-KIKAN-COUNT
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL ( IDX     >   SPA-GMN-KIKANLIST-MAX )
               MOVE    SPA-GMN-KIKANLIST(IDX)
                                       TO  S04-KIKAN-LIST(IDX)
           END-PERFORM
      *
      *    印字区分コンボ
           MOVE    SPA-GMN-DSPKBN-G        TO  S04-DSPKBN
           MOVE    SPA-GMN-DSPLIST-MAX     TO  S04-DSP-COUNT
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL ( IDX     >   SPA-GMN-DSPLIST-MAX )
               MOVE    SPA-GMN-DSPLIST(IDX)
                                       TO  S04-DSP-LIST(IDX)
           END-PERFORM
      *
      *    期間開始診療分の支払い
           MOVE    SPA-GMN-SUMKBN-G      TO  S04-SUMKBN
           MOVE    SPA-GMN-SUMKBNLIST-MAX   TO  S04-SUMKBN-COUNT
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL ( IDX     >   SPA-GMN-SUMKBNLIST-MAX )
               MOVE    SPA-GMN-SUMKBNLIST(IDX)
                                       TO  S04-SUMKBN-LIST(IDX)
           END-PERFORM
      *
      *    処理区分コンボ
           MOVE    SPA-GMN-SYORIKBN-G      TO  S04-SYORIKBN
           MOVE    SPA-GMN-SYORILIST-MAX   TO  S04-SYORI-COUNT
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL ( IDX     >   SPA-GMN-SYORILIST-MAX )
               MOVE    SPA-GMN-SYORILIST(IDX)
                                       TO  S04-SYORI-LIST(IDX)
           END-PERFORM
      *
           IF    ( SPA-GMN-SRYKBN   =   "1" )
               MOVE    "〜"                TO  S04-LBLKARA
               MOVE    WIDGET-NORMAL       TO  S04-ENDSRYYMD-STATE
           ELSE
               MOVE    SPACE               TO  S04-LBLKARA
               MOVE    WIDGET-INSENSITIVE  TO  S04-ENDSRYYMD-STATE
           END-IF
      *
           IF    ( SPA-GMN-KIKANKBN =   "2" )
               MOVE    SPACE               TO  S04-LBLSYORIKBN
                                               S04-LBLSUMKBN
               MOVE    WIDGET-INSENSITIVE  TO  S04-SYORIKBN-STATE
                                               S04-SYORI-LIST-STATE
                                               S04-SUMKBN-STATE
                                               S04-SUMKBN-LIST-STATE
                                               S04-DSPKBN-STATE
                                               S04-DSP-LIST-STATE
                                               S04-JIHIKBN-STATE
                                               S04-JIHI-LIST-STATE
           ELSE
               MOVE    "期間開始日前診療分の支払い"
                                           TO  S04-LBLSUMKBN
               MOVE    "期間終了日後の支払い"
                                           TO  S04-LBLSYORIKBN
               MOVE    WIDGET-NORMAL       TO  S04-SYORIKBN-STATE
                                               S04-SYORI-LIST-STATE
                                               S04-SUMKBN-STATE
                                               S04-SUMKBN-LIST-STATE
                                               S04-DSPKBN-STATE
                                               S04-DSP-LIST-STATE
                                               S04-JIHIKBN-STATE
                                               S04-JIHI-LIST-STATE
           END-IF
      *
           MOVE    WIDGET-NORMAL           TO  S04-B10-STATE
                                               S04-B12-STATE
      *
           IF    ( SPA-GMN-SIHARAIKBN =   "0" )
            AND  ( SPA-GMN-JIHIKBN    =   "0" )
               MOVE    WIDGET-INSENSITIVE  TO  S04-B10-STATE
                                               S04-B12-STATE
           END-IF
      *
           IF    ( SPA-GMN-JIHIKBN    =   "0" )
               PERFORM 4001-APIPV-CHK-SEC
               IF    ( FLG-API        =   1 )
                   MOVE    WIDGET-INSENSITIVE TO  S04-B10-STATE
               END-IF
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *----(03.04.01)--UPD-START---
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
      * 520-SIDSET-SEC              SECTION.
      *
      *     MOVE    SPACE               TO  WRK-SIDMSG
      *
      *     EVALUATE    SPA-SIDCD
      *     WHEN    "1001"
      *         MOVE    "印刷を行います"
      *                                  TO  WRK-SIDMSG
      *     END-EVALUATE
      *
      *     MOVE    SPACE                TO  SID2
      *     MOVE    SPA-SIDCD            TO  SID2-ID1CODE
      *     MOVE    WRK-SIDMSG           TO  SID2-ID1MSG
      *     MOVE    "B12"                TO  MCP-WIDGET
      *
      *     MOVE    "S04"                TO  SPA-MOTOPG
      *     MOVE    "SID2"               TO  SPA-SAKIPG
      *
      *     MOVE    "NEW"                TO  MCP-PUTTYPE
      *     MOVE    "SID2"               TO  MCP-WINDOW
      *
      *     PERFORM 900-PUT-WINDOW
      *
      *     MOVE    1                    TO  FLG-END
      *
      *     .
      * 520-SIDSET-EXT.
      *     EXIT.
      *----(03.04.01)--UPD-END-----
      *************************************************************
      *    画面カーソルセット処理
      *************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF        ( SPA-ERRCD       =   SPACE )
               AND   ( SPA-GMN-CUR     =   ZERO )
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
           WHEN   1
               MOVE    "SIHARAIKBN"    TO  MCP-WIDGET
           WHEN   2
               MOVE    "NYUGAIKBN"     TO  MCP-WIDGET
           WHEN   3
               MOVE    "SRYKBN"        TO  MCP-WIDGET
           WHEN   4
               MOVE    "STRSRYYMD"     TO  MCP-WIDGET
           WHEN   5
               MOVE    "ENDSRYYMD"     TO  MCP-WIDGET
           WHEN   6
               MOVE    "KIKANKBN"      TO  MCP-WIDGET
           WHEN   7
               MOVE    "DSPKBN"        TO  MCP-WIDGET
           WHEN   10
               MOVE    "SUMKBN"        TO  MCP-WIDGET
           WHEN   8
               MOVE    "SYORIKBN"      TO  MCP-WIDGET
           WHEN   9
               MOVE    "JIHIKBN"       TO  MCP-WIDGET
           WHEN   99
               MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理（入力位置より）
      *****************************************************************
       50011-CUR-SET-SEC           SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "SIHARAIKBN"
               MOVE    2           TO  SPA-GMN-CUR
           WHEN    "NYUGAIKBN"
               MOVE    3           TO  SPA-GMN-CUR
           WHEN    "SRYKBN"
               MOVE    4           TO  SPA-GMN-CUR
           WHEN    "STRSRYYMD"
               IF      SPA-GMN-SRYKBN      =   "1"
                   MOVE    5           TO  SPA-GMN-CUR
               ELSE
                   MOVE    6           TO  SPA-GMN-CUR
               END-IF
           WHEN    "ENDSRYYMD"
               MOVE    6           TO  SPA-GMN-CUR
           WHEN    "KIKANKBN"
              IF       SPA-GMN-KIKANKBN   =   "1"
                   MOVE    7           TO  SPA-GMN-CUR
               ELSE
                   MOVE    99          TO  SPA-GMN-CUR
               END-IF
           WHEN    "DSPKBN"
               IF      SPA-GMN-KIKANKBN    =   "1"
                   MOVE    10          TO  SPA-GMN-CUR
               ELSE
                   MOVE    99          TO  SPA-GMN-CUR
               END-IF
           WHEN    "SUMKBN"
               IF      SPA-GMN-KIKANKBN    =   "1"
                   MOVE    8           TO  SPA-GMN-CUR
               ELSE
                   MOVE    99          TO  SPA-GMN-CUR
               END-IF
           WHEN    "SYORIKBN"
               IF      SPA-GMN-KIKANKBN    =   "1"
                   MOVE    9           TO  SPA-GMN-CUR
               ELSE
                   MOVE    99          TO  SPA-GMN-CUR
               END-IF
           WHEN    "JIHIKBN"
               MOVE    99          TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    エラーメッセージ表示理
      **************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "帳票指定区分入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "入外区分入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "期間指定区分入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "期間開始入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "期間終了入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "期間区分入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "請求内容印字区分入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "支払区分入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0009"
               MOVE    "自費分発行指定区分入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0021"
               MOVE    "期間を正しく入力してください"
                                           TO  SPA-ERRMSG
           WHEN    "0022"
               MOVE    "指定された期間が１２ヶ月を超えています"
                                           TO  SPA-ERRMSG
           WHEN    "7777"
               MOVE    WRK-ERRMSG          TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  SERR
           MOVE    SPA-ERRCD       TO  SERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  SERR-ERRMSG
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "S04"           TO  SPA-MOTOPG
           MOVE    "SERR"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "SERR"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *   和暦年月日→西暦年月日（機能１２）
      *****************************************************************
       831-YMDCONV12-SEC           SECTION.
      *
           IF      WRK-LNKYMD      NOT =   SPACE
      *        日付画面チェックサブ
               INITIALIZE                  ORCSGDAYAREA
               MOVE    WRK-LNKYMD      TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY     TO  WRK-LNKYMD
                   MOVE    SGDAY-SDAY      TO  WRK-SYMD
               ELSE
                   MOVE    "0002"          TO  SPA-ERRCD
               END-IF
           END-IF
           .
      *
       831-YMDCONV12-EXT.
           EXIT.
      *
      *****************************************************************
      *   画面日付編集処理
      *****************************************************************
       800-ORCSGDAY-SEC            SECTION.
      *
      *    日付画面チェックサブ
           INITIALIZE                  ORCSGDAYAREA
           MOVE    "2"             TO  SGDAY-INTYPE
           MOVE    WRK-LNKYMD      TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF    ( SGDAY-RC            =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-LNKYMD
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
               MOVE    "0002"          TO  SPA-ERRCD
           END-IF
           .
      *
       800-ORCSGDAY-EXT.
           EXIT.
      *
      *----(03.04.01)--ADD-START---
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC  SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC              =   ZERO )
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF    ( MCP-RC              =   ZERO )
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
                   MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *
      *----(03.04.01)--ADD-END-----
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE    "PUTWINDOW"     TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
