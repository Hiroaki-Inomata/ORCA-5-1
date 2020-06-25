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
       PROGRAM-ID.             ORCGY03.
1     *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 予約登録
      *  コンポーネント名  : 予約一覧、未院一覧（Ｙ０３）　
      *                     （Ｙ０１のサブ）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−    　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01.05.18  表題の修正
      *                                     医療機関ＩＤをスパよりセット
      *  01.00.02    MCC-藤原     01/05/24  患者番号ゼロ詰め処理
      *  01.00.03    NACL-多々納  03/05/19  診療内容・ドクター選択追加
      *  01.00.04    NACL-多々納  03/12/02  前頁・次頁追加
      *  01.00.05    NACL-多々納  06/05/12  MONFUNC 対応
      *  03.05.00    NACL-多々納  07/05/07  グループ診療対応
      *  04.01.00    NACL-多々納  08/01/30  予約内容追加
      *  04.05.00    NACL-多々納  09/06/10  予約一覧カスタマイズ帳票対応
      *  04.08.00    NACL-森脇    13/10/10  クライアント印刷対応
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
      *
      *    予約用ワークスパ領域
           COPY    "Y01COMMON-SPA".
      *    スパ領域
       01  SPA-Y03.
           03  SPA-STS                 PIC 9(04).
      *    03  SPA-ERRCD               PIC X(04).
           03  SPA-CTLSEQ              PIC 9(02).
      *
           03  SPA-Y03-AREA.
               05  SPA-GMN-PAGE            PIC 9(02).
               05  SPA-GMN-LINE            PIC 9(04).
               05  SPA-GMN-MAX             PIC 9(04).
               05  SPA-GMN-CUR             PIC 9(02).
      *
               05  SPA-NAI-NEXT            PIC 9(01).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-SINYMD      PIC X(20).
                   07  SPA-GMN-MIDASI      PIC X(12).
      *
                   07  SPA-GMN-SINRYO.
                       09  SPA-GMN-SINRYOCD    PIC X(02).
                       09  SPA-GMN-SK1         PIC X(01).
                       09  SPA-GMN-SINRYOMEI   PIC X(24).
                   07  SPA-GMN-SINRYOLST-G.
                       09  SPA-GMN-SINRYOLST   PIC X(27)   OCCURS  100.
                   07  SPA-SINRYO-MAX          PIC 9(04).
      *
                   07  SPA-GMN-DRNAME.
                       09  SPA-GMN-DRCD        PIC X(04).
                       09  SPA-GMN-D1          PIC X(01).
                       09  SPA-GMN-DRCDMEI     PIC X(24).
                   07  SPA-GMN-DRNAMELST-G.
                       09  SPA-GMN-DRNAMELST       OCCURS  100.
                           11  SPA-GMN-DRNCDL      PIC X(04).
                           11  SPA-GMN-D1L         PIC X(01).
                           11  SPA-GMN-DRNAMEMEIL  PIC X(24).
                       09  SPA-DRCD-MAX            PIC 9(04).
      *
                   07  SPA-NAI-DRNAMELST-G.
                       09  SPA-NAI-DRNAMELST       OCCURS  100.
                           11  SPA-NAI-DRCDFLG     PIC X(01).
      *
                   07  SPA-GMN-TBL.
                       09  SPA-GMN-TBLO    OCCURS  200.
                           11  SPA-GMN-TBANGO      PIC 9(03).
                           11  SPA-GMN-TYYKTIME    PIC X(04).
                           11  SPA-GMN-TSRYKA      PIC X(20).
                           11  SPA-GMN-TSINRYO     PIC X(24).
                           11  SPA-GMN-TDRNAME     PIC X(24).
                           11  SPA-GMN-TKNJNAME    PIC X(30).
                           11  SPA-GMN-TPTNUM      PIC X(20).
                           11  SPA-GMN-TYYKKBN     PIC X(10).
                           11  SPA-GMN-TRENRAKU    PIC X(10).
                           11  SPA-GMN-TRAIIN      PIC X(04).
                   07  SPA-GMN-SELBAN          PIC 9(04).
      *
               05  SPA-NAIB-AREA.
                   07  SPA-NAI-SINYMD          PIC X(08).
                   07  SPA-NAI-DRNCD           PIC X(05).
                   07  SPA-NAI-TBL.
                       09  SPA-NAI-TBLO    OCCURS  200.
                           11  SPA-NAI-TSRYKA      PIC X(02).
                           11  SPA-NAI-TKYYKTIME   PIC X(04). 
                           11  SPA-NAI-TSINRYO     PIC X(02).
                           11  SPA-NAI-TDRNCD      PIC X(05).
                           11  SPA-NAI-TRAIIN      PIC X(01).
                           11  SPA-NAI-TYYKID      PIC 9(02).
                           11  SPA-NAI-TYYKKBN     PIC X(02).
                           11  SPA-NAI-TRENRAKU    PIC X(01).
                           11  SPA-NAI-TPTID       PIC 9(10).
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-YYK             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-DRCD            PIC 9(01).
      *----(04.08.00)--ADD-START---
      *    クライアント印刷有
           03  FLG-CLIENT-OK       PIC 9(01).
      *----(04.08.00)--ADD-END-----
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
           03  IDX-D               PIC 9(04).
           03  IDY                 PIC 9(04).
      *
           03  IDX-G               PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-PTNUM           PIC X(20).
      *
           03  WRK-ERRMSG          PIC X(40).
      *
           03  WRK-PATH                PIC X(64).
      *
           03  WRK-STR                 PIC 9(04).
      *    予約一覧ＰＧ名
           03  WRK-YYKICH-PG           PIC X(20).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG-G.
               05  WRK-HENYMDG         PIC X(09).
               05  FILLER              PIC X(01)   VALUE   "(".
               05  WRK-HENNYOBI        PIC X(02).
               05  FILLER              PIC X(01)   VALUE   ")".
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
           03  WRK-HENTIME.
               05                  PIC X(01)   VALUE   SPACE.
               05  WRK-HTIME1      PIC X(02).
               05  WRK-HK1         PIC X(01).
               05  WRK-HTIME2      PIC X(02).
               05                  PIC X(01)   VALUE   SPACE.
      *    テーブルソート用ワーク
       01  WRK-SORT-AREA.
           03  WRK-YYKTIME         PIC X(04).
           03  WRK-YYKID           PIC X(02).
           03  WRK-IDX             PIC 9(04).
           03  WRK-GMN-TBL.
               05  WRK-GMN-TBLO    OCCURS  200.
                   07  WRK-GMN-TBANGO      PIC 9(03).
                   07  WRK-GMN-TYYKTIME    PIC X(04).
                   07  WRK-GMN-TSRYKA      PIC X(20).
                   07  WRK-GMN-TSINRYO     PIC X(24).
                   07  WRK-GMN-TDRNAME     PIC X(24).
                   07  WRK-GMN-TKNJNAME    PIC X(30).
                   07  WRK-GMN-TKNJNO      PIC X(20).
                   07  WRK-GMN-TYYKKBN     PIC X(10).
                   07  WRK-GMN-TRENRAKU    PIC X(10).
                   07  WRK-GMN-TRAIIN      PIC X(04).
      *
           03  WRK-NAI-TBL.
               05  WRK-NAI-TBLO    OCCURS  200.
                   07  WRK-NAI-TSRYKA      PIC X(02).
                   07  WRK-NAI-TKYYKTIME   PIC X(04).  
                   07  WRK-NAI-TSINRYO     PIC X(02).
                   07  WRK-NAI-TDRNCD      PIC X(05).
                   07  WRK-NAI-TRAIIN      PIC X(01).
                   07  WRK-NAI-TYYKID      PIC 9(02).
                   07  WRK-NAI-TYYKKBN     PIC X(02).
                   07  WRK-NAI-TRENRAKU    PIC X(01).
                   07  WRK-NAI-TKNJID      PIC X(10).
      *
      *    診療科テーブル
       01  TBL-SRYKA-AREA.
           03  TBL-SRYKA-G             OCCURS   99.
               05  TBL-SRYKA-CD        PIC X(02).
               05  TBL-SRYKANAME       PIC X(20).
      *    連絡先テーブル
       01  TBL-RENRAK-AREA.
           03  TBL-RENRAK-G            OCCURS   9.
               05  TBL-RENRAK-CD       PIC X(01).
               05  TBL-YYKKAKNAME      PIC X(10).
      *    予約内容テーブル
       01  TBL-YYKKBN-AREA.
           03  TBL-YYKKBN-G            OCCURS   99.
               05  TBL-YYKKBN-CD       PIC X(02).
               05  TBL-YYKKBNNAME1     PIC X(10).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    診療内容情報
           COPY    "CPSK1012.INC".
      *    診療科
           COPY    "CPSK1005.INC".
      *    職員情報
           COPY    "CPSK1010.INC".
      *    予約確認
           COPY    "CPSK0051.INC".
      *    予約内容
           COPY    "CPSK1028.INC".
      *    帳票ＰＧ
           COPY  "CPSK1032.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    予約マスター
       01  YYK-REC.
           COPY    "CPYYK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
      *
           COPY  "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    予約一覧パラメタ領域
           COPY    "CPORCHC14.INC".
      *    オンライン帳票共通パラメタ
           COPY    "CPONPRTDATA.INC".
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *----(04.08.00)--ADD-START---
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *----(04.08.00)--ADD-END-----
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
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
           COPY    "Y01.INC".
           COPY    "Y011.INC".
           COPY    "Y012.INC".
           COPY    "Y03.INC".
           COPY    "Y04.INC".
           COPY    "Y05.INC".
           COPY    "YERR.INC".
           COPY    "YID1.INC".
           COPY    "CPAPIMODV2.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
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
           MOVE    SPA-FREE        TO  SPA-Y03
           MOVE    SPA-WORK        TO  SPA-Y01KYOUTU  
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"           ALSO   "CLICKED"
                   PERFORM 210-GMNSENI
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
           MOVE    SPA-Y01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-Y03         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "YERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
      *    画面編集
           MOVE    SPACE                TO  LNK-KYOUTU
      *
      *****MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    SPACE                TO  MCP-PUTTYPE
           MOVE    "Y03"                TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                    TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           INITIALIZE              SPA-Y03-AREA
      *
           IF      LNK-KYOUTU         NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
           MOVE    SPA-WORK                TO  SPA-Y01KYOUTU
      *
           MOVE    ZERO                TO  SPA-GMN-SELBAN
      *    MOVE    ZERO                TO  SPA-NAI-SELBAN
      *
      *
           MOVE    SPA-Y01-YYKYMDS     TO  SPA-NAI-SINYMD
           MOVE    SPA-NAI-SINYMD      TO  WRK-SYMD
           PERFORM 500-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG-G       TO  SPA-GMN-SINYMD
      *
           IF      SPA-Y01-YYKID   NOT =   ZERO
               MOVE    "（未院分）"    TO  SPA-GMN-MIDASI
           END-IF
      *
           PERFORM 301-INIT-SPATBL-SEC
      *
           IF      SPA-Y01-YYKINIT-FLG =   "1"
      *        全体
               MOVE    SPA-GMN-SINRYOLST (1)   TO  SPA-GMN-SINRYO
               MOVE    SPA-GMN-DRNAMELST (1)   TO  SPA-GMN-DRNAME
               MOVE    "1"                     TO  SPA-NAI-DRNCD(1:1)
               MOVE    SPA-GMN-DRCD            TO  SPA-NAI-DRNCD(2:4)
           ELSE
               MOVE    SPA-Y01-SRYNAIYOG   TO  SPA-GMN-SINRYO
               MOVE    SPA-Y01-DRNAME      TO  SPA-GMN-DRNAME
               MOVE    SPA-Y01-DRCD        TO  SPA-NAI-DRNCD
           END-IF
      *
           MOVE    1               TO  SPA-GMN-PAGE
           PERFORM 310-SPASET-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル初期編集処理
      *****************************************************************
       301-INIT-SPATBL-SEC           SECTION.
      *
      *    診療内容をＳＰＡにセット
           INITIALIZE                   SPA-GMN-SINRYOLST-G
      *    １件目を全体
           MOVE    1                   TO  SPA-SINRYO-MAX
           MOVE    "00 全体"           TO  SPA-GMN-SINRYOLST
                                                   (SPA-SINRYO-MAX)
      *
           MOVE    SPACE               TO  SYS-1012-REC
           MOVE    "1012"              TO  SYS-1012-KANRICD
           MOVE    SPA-NAI-SINYMD      TO  SYS-1012-STYUKYMD
           MOVE    SPA-NAI-SINYMD      TO  SYS-1012-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1012-HOSPNUM
           MOVE    SYS-1012-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1012-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM
                   UNTIL   (SPA-SINRYO-MAX     >=  100 )   OR
                           (FLG-SYSKANRI       =   1   )
      *
               MOVE    MCPDATA-REC       TO  SYS-1012-REC
               ADD      1                TO  SPA-SINRYO-MAX
               MOVE    SYS-1012-KBNCD    TO  SPA-GMN-SINRYOLST
                                                  (SPA-SINRYO-MAX)(1:2)
               MOVE    SYS-1012-SRYNAIYODSP
                                         TO  SPA-GMN-SINRYOLST
                                                  (SPA-SINRYO-MAX)(4:)
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           INITIALIZE                  SPA-GMN-DRNAMELST-G
           INITIALIZE                  SPA-NAI-DRNAMELST-G
      *    ドクター １件目を指定無しに
           MOVE    1                   TO  SPA-DRCD-MAX
           MOVE    "9999"              TO  SPA-GMN-DRNAMELST
                                                (SPA-DRCD-MAX)(1:4)
           MOVE    SPACE               TO  SPA-GMN-DRNAMELST
                                                (SPA-DRCD-MAX)(5:1)
           MOVE    "指定なし"          TO  SPA-GMN-DRNAMELST
                                                (SPA-DRCD-MAX)(6:)
      *    職員情報をＳＰＡにセット
           MOVE    SPACE               TO  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-NAI-SINYMD      TO  SYS-1010-STYUKYMD
           MOVE    SPA-NAI-SINYMD      TO  SYS-1010-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM
                   UNTIL      (SPA-DRCD-MAX      >=  100 )   OR
                              (FLG-SYSKANRI      =   1   )
               MOVE    MCPDATA-REC      TO  SYS-1010-REC
               IF      SYS-1010-KBNCD(1:1) =   "1"
      *H27.12  非表示
                AND   (SYS-1010-HYOJIFLG   NOT =   "1" )
                   ADD     1                   TO  SPA-DRCD-MAX
                   MOVE    SYS-1010-KBNCD(2:4) TO  SPA-GMN-DRNAMELST
                                                       (SPA-DRCD-MAX)
                                                       (1:4)
                   MOVE    SYS-1010-NAME       TO  SPA-GMN-DRNAMELST
                                                       (SPA-DRCD-MAX)
                                                       (6:)
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      **** MOVE    SPA-Y01-SRYNAIYOG   TO  SPA-GMN-SINRYO
      *    MOVE    SPA-Y01-DRNAME      TO  SPA-GMN-DRNAME
      **** MOVE    SPA-Y01-DRCD        TO  SPA-NAI-DRNCD
      *
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   SPA-SINRYO-MAX )
               IF     SPA-GMN-SINRYOCD     =   SPA-GMN-SINRYOLST(IDX-D)
                                                            (1:2)
                   MOVE    SPA-GMN-SINRYOLST(IDX-D) 
                                               TO  SPA-GMN-SINRYO
                   MOVE    SPA-SINRYO-MAX      TO  IDX-D
                   MOVE    1                   TO  FLG-CHK
               END-IF
           END-PERFORM
     *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   SPA-DRCD-MAX )
               IF      SPA-NAI-DRNCD(2:4)  =   SPA-GMN-DRNCDL(IDX-D)
                   MOVE    SPA-GMN-DRNAMELST(IDX-D)
                                           TO  SPA-GMN-DRNAME
                   MOVE    SPA-DRCD-MAX    TO  IDX-D
                   MOVE    1                   TO  FLG-CHK
               END-IF
           END-PERFORM
           .
       301-INIT-SPATBL-EXT.
           EXIT.
      *****************************************************************
      *    予約マスタ編集処理
      *****************************************************************
       310-SPASET-SEC           SECTION.
      *
      *    開始行設定
           COMPUTE WRK-STR             =   (SPA-GMN-PAGE  -  1)
                                       *    200
                                       +    1
      *
           INITIALIZE                  SPA-GMN-TBL
           INITIALIZE                  SPA-NAI-TBL
      *
      *    予約一覧編集
           INITIALIZE                  YYK-KEY
           INITIALIZE                  YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
     ***** MOVE    SPA-Y01-YYKYMDS     TO  YYK-YYKYMD
           MOVE    SPA-NAI-SINYMD      TO  YYK-YYKYMD
           MOVE    SPA-GMN-SINRYOCD    TO  YYK-SRYNAIYO
           MOVE    SPA-NAI-DRNCD       TO  YYK-DRCD
           IF     (SPA-GMN-SINRYOCD    =   "00"  )  OR
                  (SPA-NAI-DRNCD(2:4)  =   "9999")
               MOVE    "key5"              TO  WRK-PATH
           ELSE
               MOVE    "key3"              TO  WRK-PATH
           END-IF
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 900-YOYAKU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-YYK
           END-IF
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX-G
           MOVE    ZERO                TO  SPA-NAI-NEXT
           MOVE    ZERO                TO  SPA-GMN-MAX
           PERFORM
                   UNTIL      (FLG-YYK     =   1   )
                         OR   (IDX         >   200 )
               MOVE    ZERO                TO  FLG-SYORI
               IF     (SPA-GMN-SINRYOCD    =   "00"  )  OR
                      (SPA-NAI-DRNCD(2:4)   =   "9999")
                   IF      SPA-GMN-SINRYOCD    NOT =   "00"
                       IF      SPA-GMN-SINRYOCD    =  YYK-SRYNAIYO
                           MOVE    1           TO  FLG-SYORI
                       END-IF
                   END-IF
                   IF      SPA-NAI-DRNCD(2:4)   NOT =   "9999"
                       IF      SPA-NAI-DRNCD        =  YYK-DRCD
                           MOVE    1           TO  FLG-SYORI
                       END-IF
                   END-IF
                   IF     (SPA-GMN-SINRYOCD    =   "00"  )  AND
                          (SPA-NAI-DRNCD(2:4)   =   "9999")
                       MOVE    1           TO  FLG-SYORI
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-SYORI
               END-IF
               IF      FLG-SYORI           =   1
      *
                   IF     (YYK-YYKID       >   ZERO           )  AND
                          ((SPA-Y01-YYKID      =   ZERO  )  OR
                          ((SPA-Y01-YYKID  NOT =   ZERO  ) AND
                           (YYK-RAIINZUMI      =   SPACE )) )
                       PERFORM 3101-YOYAKU-HEN-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 900-YOYAKU-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           PERFORM 3001-TBL-SORT-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスタ編集処理
      *****************************************************************
       3101-YOYAKU-HEN-SEC           SECTION.
      *
      *    開始行読み飛ばし
           ADD     1               TO  IDX-G
           IF      IDX-G           <   WRK-STR
               CONTINUE
           ELSE
               ADD     1               TO  IDX
               IF      IDX             >   200
                   MOVE    1               TO  SPA-NAI-NEXT
               ELSE
                   IF      IDX             =   1
      *                システム管理テーブル編集
                       PERFORM 3011-SYSKANRI-TBL-SEC
                       MOVE    1               TO  IDX
                   END-IF
      *            テーブル編集
                   PERFORM 31011-YOYAKU-TBLHEN-SEC
               END-IF
           END-IF
           .
       3101-YOYAKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスタテーブル編集処理
      *****************************************************************
       31011-YOYAKU-TBLHEN-SEC           SECTION.
      *
           MOVE    IDX             TO  SPA-GMN-MAX
           MOVE    IDX-G           TO  SPA-GMN-TBANGO  (IDX)
           MOVE    YYK-KEYYYKTIME  TO  SPA-NAI-TKYYKTIME (IDX)
           MOVE    YYK-YYKTIME     TO  SPA-GMN-TYYKTIME(IDX)
           MOVE    YYK-SRYKA       TO  SPA-NAI-TSRYKA  (IDX)
           MOVE    YYK-SRYNAIYO    TO  SPA-NAI-TSINRYO (IDX)
           MOVE    YYK-DRCD        TO  SPA-NAI-TDRNCD  (IDX)
           MOVE    YYK-NAME        TO  SPA-GMN-TKNJNAME(IDX)
           MOVE    YYK-PTID        TO  SPA-NAI-TPTID   (IDX)
      *
           PERFORM 3001-PTNUM-HENSYU-SEC
           MOVE    WRK-PTNUM       TO  SPA-GMN-TPTNUM (IDX)
      *
           MOVE    YYK-YYKKBN      TO  SPA-NAI-TYYKKBN (IDX)
           MOVE    YYK-KAKREN      TO  SPA-NAI-TRENRAKU(IDX)
           MOVE    YYK-RAIINZUMI   TO  SPA-NAI-TRAIIN  (IDX)
           MOVE    YYK-YYKID       TO  SPA-NAI-TYYKID  (IDX)
           IF      YYK-RAIINZUMI       =   "1"
               MOVE    "来院"          TO  SPA-GMN-TRAIIN(IDX)
           END-IF
      *
      *    ドクター
           MOVE    ZERO                TO  IDX-D
           MOVE    ZERO                TO  FLG-DRCD
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   SPA-DRCD-MAX )
               IF     YYK-DRCD(2:4)        =   SPA-GMN-DRNCDL(IDX-D)
                   MOVE    SPA-GMN-DRNAMEMEIL(IDX-D)
                                           TO  SPA-GMN-TDRNAME (IDX)
                   MOVE    "1"             TO  SPA-NAI-DRCDFLG
                                                              (IDX-D)
                   MOVE    SPA-DRCD-MAX    TO  IDX-D
                   MOVE    1               TO  FLG-DRCD
               END-IF
           END-PERFORM
      *H27.11
      *    対象なし
           IF      FLG-DRCD            =   ZERO
               PERFORM 310112-DRCD-CHK-SEC
           END-IF
      *
      *    診療内容
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   SPA-SINRYO-MAX )
               IF     YYK-SRYNAIYO         =   SPA-GMN-SINRYOLST(IDX-D)
                                                            (1:2)
                   MOVE    SPA-GMN-SINRYOLST(IDX-D) (4:)
                                           TO  SPA-GMN-TSINRYO(IDX)
                   MOVE    SPA-SINRYO-MAX  TO  IDX-D
               END-IF
           END-PERFORM
      *
      *    診療科
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   99  )
                          OR  (TBL-SRYKA-CD (IDX-D)    =   SPACE)
               IF     YYK-SRYKA            =   TBL-SRYKA-CD (IDX-D)
                   MOVE    TBL-SRYKANAME (IDX-D)
                                           TO  SPA-GMN-TSRYKA(IDX)
                   MOVE    99              TO  IDX-D
               END-IF
           END-PERFORM
      *
      *    予約確認連絡コード
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   9   )
                          OR  (TBL-RENRAK-CD (IDX-D)   =   SPACE)
               IF     YYK-KAKREN            =   TBL-RENRAK-CD (IDX-D)
                   MOVE    TBL-YYKKAKNAME (IDX-D)
                                           TO  SPA-GMN-TRENRAKU(IDX)
                   MOVE    9               TO  IDX-D
               END-IF
           END-PERFORM
      *
      *    予約内容コード
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   99   )
                          OR  (TBL-YYKKBN-CD (IDX-D)   =   SPACE)
               IF     YYK-YYKKBN            =   TBL-YYKKBN-CD (IDX-D)
                   MOVE    TBL-YYKKBNNAME1 (IDX-D)
                                           TO  SPA-GMN-TYYKKBN(IDX)
                   MOVE    99              TO  IDX-D
               END-IF
           END-PERFORM
           .
       31011-YOYAKU-TBLHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ編集処理
      *****************************************************************
       3001-PTNUM-HENSYU-SEC           SECTION.
      *
      *    患者番号変換より、患者IDより患者NOを取得する
      *
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    YYK-PTID            TO  SPTID-PTID
      *
           CALL    "ORCSPTID"          USING  ORCSPTIDAREA
                                              SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
               MOVE    SPACE           TO  WRK-PTNUM
           ELSE
               MOVE    SPTID-PTNUM     TO  WRK-PTNUM
           END-IF
      *
           .
       3001-PTNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理テーブル編集処理
      *****************************************************************
       310112-DRCD-CHK-SEC           SECTION.
      *
      *    職員情報（ドクター）
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    YYK-DRCD            TO  SYS-1010-KBNCD
           MOVE    SPA-NAI-SINYMD      TO  SYS-1010-STYUKYMD
           MOVE    SPA-NAI-SINYMD      TO  SYS-1010-EDYUKYMD
      *
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
      *        最後の行へ編集する
               IF      SPA-DRCD-MAX        >=  100
                   MOVE    100             TO  SPA-DRCD-MAX
               ELSE
                   ADD     1               TO  SPA-DRCD-MAX
               END-IF
               MOVE    SPACE               TO  SPA-GMN-DRNAMELST
                                                      (SPA-DRCD-MAX)
               MOVE    SYS-1010-KBNCD(2:4) TO  SPA-GMN-DRNCDL
                                                      (SPA-DRCD-MAX)
               MOVE    SYS-1010-NAME       TO  SPA-GMN-DRNAMEMEIL
                                                      (SPA-DRCD-MAX)
               MOVE    "1"                 TO  SPA-NAI-DRCDFLG
                                                      (SPA-DRCD-MAX)
      *
               MOVE    SPA-GMN-DRNAMEMEIL(SPA-DRCD-MAX)
                                           TO  SPA-GMN-TDRNAME (IDX)
           END-IF
      *
           .
       310112-DRCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理テーブル編集処理
      *****************************************************************
       3011-SYSKANRI-TBL-SEC           SECTION.
      *
           INITIALIZE                  TBL-SRYKA-AREA
           INITIALIZE                  TBL-RENRAK-AREA
           INITIALIZE                  TBL-YYKKBN-AREA
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-NAI-SINYMD      TO  SYS-1005-STYUKYMD
           MOVE    SPA-NAI-SINYMD      TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL   (FLG-SYSKANRI   =   1   )
                       OR  (IDX            >=  99  )
               ADD     1                   TO  IDX
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
               MOVE    SYS-1005-KBNCD(1:2) TO  TBL-SRYKA-CD  (IDX)
               MOVE    SYS-1005-SRYKANAME2 TO  TBL-SRYKANAME (IDX)
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    予約確認連絡コード
           MOVE    SPACE               TO  SYS-0051-REC
           MOVE    "0051"              TO  SYS-0051-KANRICD
           MOVE    SPA-NAI-SINYMD      TO  SYS-0051-STYUKYMD
           MOVE    SPA-NAI-SINYMD      TO  SYS-0051-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-0051-HOSPNUM
           MOVE    SYS-0051-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL   (FLG-SYSKANRI   =   1   )
                       OR  (IDX            >=  9   )
               ADD     1                   TO  IDX
               MOVE    MCPDATA-REC         TO  SYS-0051-REC
               MOVE    SYS-0051-KBNCD(1:2) TO  TBL-RENRAK-CD (IDX)
               MOVE    SYS-0051-YYKKAKNAME TO  TBL-YYKKAKNAME (IDX)
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    予約内容コード
           MOVE    SPACE               TO  SYS-1028-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1028-HOSPNUM
           MOVE    "1028"              TO  SYS-1028-KANRICD
           MOVE    SPA-NAI-SINYMD      TO  SYS-1028-STYUKYMD
           MOVE    SPA-NAI-SINYMD      TO  SYS-1028-EDYUKYMD
      *
           MOVE    SYS-1028-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL   (FLG-SYSKANRI   =   1   )
                       OR  (IDX            >=  99  )
               ADD     1                   TO  IDX
               MOVE    MCPDATA-REC         TO  SYS-1028-REC
               MOVE    SYS-1028-KBNCD(1:2) TO  TBL-YYKKBN-CD (IDX)
               MOVE    SYS-1028-YYKKBNNAME1
                                           TO  TBL-YYKKBNNAME1(IDX)
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3011-SYSKANRI-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＯＲＴ処理
      *****************************************************************
       3001-TBL-SORT-SEC              SECTION.
      *
           MOVE    SPA-GMN-TBANGO (01) TO  IDX-G
           COMPUTE IDX-G               =   IDX-G   -   1
      *
           INITIALIZE                  WRK-SORT-AREA
           MOVE    SPA-GMN-TBL         TO  WRK-GMN-TBL
           MOVE    SPA-NAI-TBL         TO  WRK-NAI-TBL
           INITIALIZE                  SPA-GMN-TBL
           INITIALIZE                  SPA-NAI-TBL
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
               MOVE    HIGH-VALUE          TO  WRK-YYKTIME
               MOVE    HIGH-VALUE          TO  WRK-YYKID
               MOVE    ZERO                TO  WRK-IDX
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   SPA-GMN-MAX
                   IF      WRK-GMN-TBANGO (IDY)  =   ZERO
                       CONTINUE
                   ELSE
                       IF      WRK-GMN-TYYKTIME (IDY)  <  WRK-YYKTIME
                           MOVE    WRK-GMN-TYYKTIME (IDY)
                                               TO  WRK-YYKTIME
                           MOVE    WRK-NAI-TYYKID   (IDY)
                                               TO  WRK-YYKID
                           MOVE    IDY         TO  WRK-IDX
                       END-IF
                   END-IF
               END-PERFORM
               IF      WRK-IDX         >   ZERO
                   MOVE    WRK-GMN-TBLO (WRK-IDX)
                                           TO  SPA-GMN-TBLO(IDX)
                   MOVE    WRK-NAI-TBLO (WRK-IDX)
                                           TO  SPA-NAI-TBLO(IDX)
                   ADD     1               TO  IDX-G
                   MOVE    IDX-G           TO  SPA-GMN-TBANGO (IDX)
                   INITIALIZE              WRK-GMN-TBLO (WRK-IDX)
                   INITIALIZE              WRK-NAI-TBLO (WRK-IDX)
               END-IF
           END-PERFORM
      *
           .
       3001-TBL-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  Y03
           INITIALIZE                      Y03
      *
           MOVE    SPA-GMN-SINYMD      TO  Y03-SRYMD
      *----(01.00.01) LINE ADD START ----------------------------------
           MOVE    SPA-GMN-MIDASI      TO  Y03-HYODAI
      *
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-SINRYO-MAX
               MOVE    SPA-GMN-SINRYOLST (IDX)
                                           TO  Y03-SRYNAIY-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-SINRYO-MAX      TO  Y03-SRYNAIY-COUNT
           MOVE    SPA-GMN-SINRYO      TO  Y03-SRYNAIYO
      *
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >  SPA-DRCD-MAX
               ADD     1               TO IDY
               MOVE    SPA-GMN-DRNAMELST (IDX)
                                       TO  Y03-DRNAME-ITEM (IDY)
           END-PERFORM
           MOVE    SPA-DRCD-MAX        TO  Y03-DRNAME-COUNT
           MOVE    SPA-GMN-DRNAME      TO  Y03-DRNAME
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-GMN-MAX
               IF  SPA-GMN-TBANGO  (IDX)       NOT = ZERO
                   MOVE    SPA-GMN-TBANGO  (IDX)
                                               TO  Y03-TNUM (IDX)
               END-IF
               IF      SPA-GMN-TYYKTIME(IDX)   NOT =   SPACE
                   MOVE    SPA-GMN-TYYKTIME(IDX)(1:2)
                                           TO  Y03-TYYKTIME(IDX)(1:2)
                   MOVE    ":"             TO  Y03-TYYKTIME(IDX)(3:1)
                   MOVE    SPA-GMN-TYYKTIME(IDX)(3:2)
                                           TO  Y03-TYYKTIME(IDX)(4:2)
               END-IF
               MOVE    SPA-GMN-TSRYKA  (IDX)
                                               TO  Y03-TSRYKA  (IDX)
               MOVE    SPA-GMN-TSINRYO (IDX)
                                               TO  Y03-TSRYNAIYO (IDX)
               MOVE    SPA-GMN-TDRNAME (IDX)
                                               TO  Y03-TDRNAME (IDX)
               MOVE    SPA-GMN-TKNJNAME(IDX)   TO  Y03-TNAME(IDX)
               MOVE    SPA-GMN-TPTNUM  (IDX)   TO  Y03-TPTNUM  (IDX)
               MOVE    SPA-GMN-TYYKKBN (IDX)
                                               TO  Y03-TYYKKBN (IDX)
               MOVE    SPA-GMN-TRENRAKU(IDX)
                                               TO  Y03-TRENRAKU(IDX)
               MOVE    SPA-GMN-TRAIIN  (IDX)
                                               TO  Y03-TRAIIN  (IDX)
           END-PERFORM
           MOVE    SPA-GMN-MAX         TO  Y03-COUNT
      *
           PERFORM 5001-MAPCUR-SEC
      *
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       210-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    一覧印刷
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 460-YYKICHI-SEC
      *    前ページ表示
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 430-MAEGMN-SEC
      *    次ページ表示
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 440-ATOGMN-SEC
      *    前日
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 450-MAE-DAY-SEC
      *    翌日
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 480-NEXT-DAY-SEC
      *    全体
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 4100-ALL-HEN-SEC
           END-EVALUATE
      *
           .
       210-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "Y03"               TO  SPA-MOTOPG
           MOVE    "Y01"               TO  SPA-SAKIPG
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "Y01"               TO  MCP-WINDOW
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
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Y03    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        診療内容
               WHEN    "ACTIVATE"      ALSO    "SRYNAIYO"
                   PERFORM 201-SRYNAIYO-HEN-SEC
      *        ドクタ−入力
               WHEN    "ACTIVATE"      ALSO    "DRNAME"
                   PERFORM 202-DRNAME-HEN-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    診療内容　　入力処理
      *****************************************************************
       201-SRYNAIYO-HEN-SEC       SECTION.
      *
           MOVE    Y03-SRYNAIYO        TO  SPA-GMN-SINRYO
      *
      *    診療内容
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   SPA-SINRYO-MAX )
               IF     SPA-GMN-SINRYOCD     =   SPA-GMN-SINRYOLST(IDX-D)
                                                            (1:2)
                   MOVE    SPA-GMN-SINRYOLST(IDX-D) 
                                               TO  SPA-GMN-SINRYO
                   MOVE    SPA-SINRYO-MAX      TO  IDX-D
                   MOVE    1                   TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
      *        画面再編集
               MOVE    1                   TO  SPA-GMN-PAGE
               PERFORM 310-SPASET-SEC
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
          .
       201-SRYNAIYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ドクター　入力処理
      *****************************************************************
       202-DRNAME-HEN-SEC       SECTION.
      *
           MOVE    Y03-DRNAME          TO  SPA-GMN-DRNAME
      *
      *    コード変換
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-DRCD        TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   4     )   OR
                  (SNUM-SYOKBN         =   "1"   )   OR
                  (SNUM-MINKBN         =   "1"   )
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    02                  TO  SPA-GMN-CUR
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  SPA-GMN-DRCD
           END-IF
      *
      *    診療内容
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   SPA-DRCD-MAX )
               IF      SPA-GMN-DRCD        =   SPA-GMN-DRNCDL(IDX-D)
                   MOVE    SPA-GMN-DRNAMELST(IDX-D)
                                           TO  SPA-GMN-DRNAME
                   MOVE    SPA-DRCD-MAX    TO  IDX-D
                   MOVE    1                   TO  FLG-CHK
               END-IF
           END-PERFORM
      *H29.12
      *    対象なし
           IF      FLG-CHK             =   ZERO
               PERFORM 2021-DRCD-CHK-SEC
           END-IF
      *
           IF      FLG-CHK             =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    "1"                 TO  SPA-NAI-DRNCD(1:1)
               MOVE    SPA-GMN-DRCD        TO  SPA-NAI-DRNCD(2:)
      *        画面再編集
               MOVE    1                   TO  SPA-GMN-PAGE
               PERFORM 310-SPASET-SEC
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF
           .
       202-DRNAME-HEN-EXT.
           EXIT.
      *
      *H29.12
      *****************************************************************
      *    ドクターチェック　処理
      *****************************************************************
       2021-DRCD-CHK-SEC           SECTION.
      *
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    "1"                 TO  SYS-1010-KBNCD(1:1)
           MOVE    SPA-GMN-DRCD        TO  SYS-1010-KBNCD(2:4)
      *
           MOVE    SPA-NAI-SINYMD      TO  SYS-1010-STYUKYMD
           MOVE    SPA-NAI-SINYMD      TO  SYS-1010-EDYUKYMD
      *
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
      *        最後の行へ編集する
               IF      SPA-DRCD-MAX        >=  100
                   CONTINUE
               ELSE
                   ADD     1               TO  SPA-DRCD-MAX
               END-IF
               MOVE    SPACE               TO  SPA-GMN-DRNAMELST
                                                      (SPA-DRCD-MAX)
               MOVE    SYS-1010-KBNCD(2:4) TO  SPA-GMN-DRNCDL
                                                      (SPA-DRCD-MAX)
               MOVE    SYS-1010-NAME       TO  SPA-GMN-DRNAMEMEIL
                                                      (SPA-DRCD-MAX)
               MOVE    "1"                 TO  SPA-NAI-DRCDFLG
                                                      (SPA-DRCD-MAX)
      *
               MOVE    SPA-GMN-DRNAMELST(SPA-DRCD-MAX)
                                           TO  SPA-GMN-DRNAME
      *
               MOVE    1                   TO  FLG-CHK
           END-IF
           .
       2021-DRCD-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    一覧印刷　処理
      *****************************************************************
       460-YYKICHI-SEC           SECTION.
      *
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"      USING   SPA-AREA
           MOVE    2                   TO  SPA-CLIENT-TEMP-FLG
      *
           IF     (SPA-CLIENT-PRT-FLG  =   2 )
             OR   (SPA-PRTCONF         =   "1" )
      *        クライアント印刷制御情報取得サブ
               PERFORM 4901-CLIENT-PRT-SEC
           END-IF
      *
      *    予約・未院一覧出力
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000011"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1032-REC
           IF      ORCSPRTNM-RC        =   ZERO
               MOVE    ORCSPRTNM-PRTPG     TO  WRK-YYKICH-PG
           END-IF
      *
           INITIALIZE                  ORCHC14AREA
           IF      SPA-Y01-YYKID       =   ZERO
               MOVE    "1"                 TO  ORCHC14-KBN
           ELSE
               MOVE    "2"                 TO  ORCHC14-KBN
           END-IF
           MOVE     SPA-GMN-SINRYOCD   TO  ORCHC14-SRYNAIYO
           MOVE     SPA-NAI-DRNCD      TO  ORCHC14-DRCD-G
           MOVE     SPA-NAI-SINYMD     TO  ORCHC14-YYKYMD
      *
           IF      WRK-YYKICH-PG   NOT =   SPACE
      *
      *        クライアント印刷
               MOVE    "01"            TO  SPA-PRT-FLG
      *
               MOVE    ORCHC14AREA     TO  ONPRTDATA-DATA
               CALL    WRK-YYKICH-PG       USING
                                           SPA-AREA
                                           ONPRTDATA
           END-IF
      *
           IF      (SPA-CLIENT-PRT-FLG =   1  )
      *        印刷ダミー処理
               MOVE    "99"            TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"        USING
                                           SPA-AREA
           END-IF
           MOVE    ZERO                TO  SPA-CLIENT-TEMP-FLG
      *
           .
       460-YYKICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    クライアント印刷前　処理
      *****************************************************************
       4901-CLIENT-PRT-SEC         SECTION.
      *
      *    クライアント印刷制御情報取得サブ
           INITIALIZE                  ORCSPRTCTRLAREA
           CALL    "ORCSPRTCTRL"       USING
                                       SPA-AREA
                                       ORCSPRTCTRLAREA
                                       MCPAREA
      *
      *
           .
       4901-CLIENT-PRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       430-MAEGMN-SEC             SECTION.
      *
           IF      SPA-GMN-PAGE        =   1
               GO      TO      430-MAEGMN-EXT
           END-IF
      *
           COMPUTE SPA-GMN-PAGE        =   SPA-GMN-PAGE   -   1
           PERFORM 310-SPASET-SEC
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
           IF      SPA-NAI-NEXT        =   ZERO
               MOVE    "0003"          TO  SPA-ERRCD
               GO      TO      440-ATOGMN-EXT
           END-IF
      *
           COMPUTE SPA-GMN-PAGE        =   SPA-GMN-PAGE   +   1
           PERFORM 310-SPASET-SEC
           .
       440-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    前日　処理
      *****************************************************************
       450-MAE-DAY-SEC             SECTION.
      *
      *    前日計算
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE     SPA-NAI-SINYMD     TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN 
           CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  SPA-NAI-SINYMD
           MOVE    SPA-NAI-SINYMD      TO  WRK-SYMD
           PERFORM 500-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG-G       TO  SPA-GMN-SINYMD
      *
           PERFORM 301-INIT-SPATBL-SEC
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           PERFORM 310-SPASET-SEC
           .
       450-MAE-DAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    翌日　処理
      *****************************************************************
       480-NEXT-DAY-SEC             SECTION.
      *
      *    翌日計算
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE     SPA-NAI-SINYMD     TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    1                   TO  LNK-DAY6-ZOGEN 
           CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  SPA-NAI-SINYMD
           MOVE    SPA-NAI-SINYMD      TO  WRK-SYMD
           PERFORM 500-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG-G       TO  SPA-GMN-SINYMD
      *
           PERFORM 301-INIT-SPATBL-SEC
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           PERFORM 310-SPASET-SEC
           .
       480-NEXT-DAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    全体表示処理
      *****************************************************************
       4100-ALL-HEN-SEC             SECTION.
      *
           IF     (SPA-GMN-SINRYOCD    =   "00"  )  AND
                  (SPA-GMN-DRCD        =   "9999")
               MOVE    SPA-Y01-SRYNAIYOG   TO  SPA-GMN-SINRYO
               MOVE    SPA-Y01-DRNAME      TO  SPA-GMN-DRNAME
               MOVE    SPA-Y01-DRCD        TO  SPA-NAI-DRNCD
           ELSE
               MOVE    SPA-GMN-SINRYOLST (1)
                                       TO  SPA-GMN-SINRYO
               MOVE    SPA-GMN-DRNAMELST (1)
                                       TO  SPA-GMN-DRNAME
               MOVE    "1"             TO  SPA-NAI-DRNCD(1:1)
               MOVE    SPA-GMN-DRCD    TO  SPA-NAI-DRNCD(2:4)
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           PERFORM 310-SPASET-SEC
           MOVE    1                   TO  SPA-GMN-CUR
           .
       4100-ALL-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       500-SEIWA-HEN-SEC             SECTION.
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
      *
      *    曜日
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY3-AREA
           MOVE    "31"                TO  LNK-DAY3-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY3-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY3-AREA
           MOVE  LNK-DAY3-YOBIMEI      TO  WRK-HENNYOBI
      *
           .
       3101-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
          EVALUATE    SPA-GMN-CUR
                WHEN    01
                    MOVE  "SRYNAIYO"    TO   MCP-WIDGET
                WHEN    02
                    MOVE  "DRNAME"      TO   MCP-WIDGET
                WHEN    OTHER
                    MOVE  "B01"         TO   MCP-WIDGET
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
                   MOVE    "診療内容が有りません"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "ドクターが有りません"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "次ページは有りません"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  YERR
           MOVE    SPA-ERRCD            TO  YERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  YERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "Y03"                TO  SPA-MOTOPG
           MOVE    "YERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "YERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスター読込
      *****************************************************************
       900-YOYAKU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-YYK
               MOVE    MCPDATA-REC         TO  YYK-REC
           ELSE
               MOVE    1                   TO  FLG-YYK
           END-IF
           .
      *
       900-YOYAKU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
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
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
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
           MOVE    "PUTWINDOW"         TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
