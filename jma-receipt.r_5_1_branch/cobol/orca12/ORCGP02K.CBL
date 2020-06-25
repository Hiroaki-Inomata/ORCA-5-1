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
       PROGRAM-ID.             ORCGP02K.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 他一部負担金入力画面（Ｐ０２Ｋ）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/07/XX    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
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
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-PTKOHFTNETC     PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-FTNMONEY        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-TBL             PIC 9(04).
           03  IDX4                PIC 9(04).
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
           03  WRK-FTNMONEY            PIC 9(07).
           03  WRK-FTNSTYMD            PIC X(08).
      *
           03  WRK-DAYCNT              PIC 9(03).
      *
           03  WRK-KEY-AREA.
               05  WRK-KEY-SRYYMD          PIC X(08).
               05  WRK-KEY-JITAKBN         PIC X(01).
               05  WRK-KEY-NYUGAIKBN       PIC X(01).
      *
           03  WRK-GOUKEI              PIC 9(08).
           03  WRK-ZZZ9                PIC ZZ,ZZZ,ZZ9.
           03  WRK-Z9                  PIC ZZ9.
      *
           03  WRK-MAX-EDANUM          PIC 9(03).
           03  WRK-RENNUM              PIC 9(02).
      *
           03  WRK-MCP-WIDGET          PIC X(64).
      *
           03  WRK-LIST-ROW            PIC S9(09).
           03  WRK-LIST-INIT           PIC 9(01).
      *
       01  TBL-HENSYU-AREA.
           03  TBL-HENSYU-OCC          OCCURS   100.
               05  TBL-SRYYMD          PIC X(08).
               05  TBL-NYUGAIKBN       PIC X(01).
               05  TBL-FTNMONEY        PIC 9(07).
               05  TBL-JITAKBN         PIC X(01).
               05  TBL-SYORIKBN        PIC X(01).
      *
       01  TBL-HENSYU-AREA2.
           03  TBL-HENSYU-OCC2         OCCURS   100.
               05  TBL2-SRYYMD          PIC X(08).
               05  TBL2-NYUGAIKBN       PIC X(01).
               05  TBL2-FTNMONEY        PIC 9(07).
               05  TBL2-JITAKBN         PIC X(01).
               05  TBL2-SYORIKBN        PIC X(01).
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
      *    患者公費負担額
       01  PTKOHFTNETC-REC.
           COPY    "CPPTKOHFTNETC.INC".
      *    データ区分
           03  FTNETC-SYORIKBN         PIC X(01).
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
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
           INITIALIZE                  IDX-AREA
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
               MOVE    "P02K"              TO  MCP-WINDOW
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
      *
           EVALUATE    SPA-MOTOPG
      *        確認メッセージ
               WHEN    "PID1"
                   PERFORM 3001-PID1-SYORI-SEC
      *
               WHEN    OTHER
      *        ＳＰＡ編集
                   INITIALIZE                      SPA-P02K-NAI-AREA
                   PERFORM 300-SPA-INIT-SEC
                   MOVE    2                   TO  SPA-P02K-CUR
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       300-SPA-INIT-SEC                SECTION.
      *
           INITIALIZE                  TBL-HENSYU-AREA
           MOVE    ZERO                TO  IDX-TBL
      *    収納テーブル検索
           IF      SPA-NAI24-KOHID  (SPA-NAI24-FTNSELNUM)
                                    NOT =   ZERO
               PERFORM 3001-SYUNOU-HENSYU-SEC
           END-IF
      *    公費負担額テーブル検索
           EVALUATE    SPA-NAI24-TFTNETCKBN (SPA-NAI24-FTNSELNUM2)
      *        公費負担額テーブル検索
               WHEN    "1"
                   PERFORM 30021-KOHFTNETC-HENSYU-SEC
      *        パラメタ検索
               WHEN    "2"
                   PERFORM 30022-PARA-KOHFTNETC-SEC
           END-EVALUATE
      *    テーブルまとめ
           PERFORM 3003-TBL-HENSYU-SEC
      *
           MOVE    "1 入院"            TO  SPA-P02K-NYUGAIKBN-LIST (1)
           MOVE    "2 外来"            TO  SPA-P02K-NYUGAIKBN-LIST (2)
           MOVE    SPA-P02K-NYUGAIKBN-LIST (2)
                                       TO  SPA-P02K-NYUGAIKBN-G
      *    期間編集
           STRING  "期間："
                   SPA-P02K-GMN-SRYYMD
                   "　〜　"
                   SPA-P02K-GMN-ENDYMD DELIMITED  BY  SIZE
                                       INTO    SPA-P02K-MSG05
           END-STRING
      *    負担額表示編集
           MOVE    SPA-P02K-GAIJGNGAKKEI
                                       TO  WRK-ZZZ9
           STRING  "月上限額："
                   WRK-ZZZ9            DELIMITED  BY  SIZE
                                       INTO    SPA-P02K-MSG01
           END-STRING
      *
           MOVE    SPA-P02K-JIGOKEI    TO  WRK-ZZZ9
           STRING  "自院累計："
                   WRK-ZZZ9            DELIMITED  BY  SIZE
                                       INTO    SPA-P02K-MSG02
           END-STRING
      *
      *    他院累計等編集処理
           PERFORM 440-TBLHEN-GOKEI-SEC
      *
           .
       300-SPA-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納テーブル検索編集処理
      *****************************************************************
       3001-SYUNOU-HENSYU-SEC                SECTION.
      *
           MOVE    SPACE               TO  SYUNOU-REC
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-P02K-NAI-SRYYM  TO  SYU-SRYYMD(1:6)
           MOVE    "%"                 TO  SYU-SRYYMD(7:1)
           MOVE    SPA-NAI24-KOHID  (SPA-NAI24-FTNSELNUM)
                                       TO  SYU-SELKOHID
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key70"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key70"             TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYUNOU-REC
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
           PERFORM UNTIL    FLG-SYUNOU     =   1
               MOVE    ZERO                TO  WRK-FTNMONEY
               MOVE    ZERO                TO  FLG-FTNMONEY
               IF      SYU-KOH1HKNNUM          =   SPA-P02K-KOHNUM
                   MOVE    SYU-KOH1HKNFTNMONEY     TO  WRK-FTNMONEY
                   MOVE    1                       TO  FLG-FTNMONEY
               END-IF
               IF      SYU-KOH2HKNNUM          =   SPA-P02K-KOHNUM
                   MOVE    SYU-KOH2HKNFTNMONEY     TO  WRK-FTNMONEY
                   MOVE    1                       TO  FLG-FTNMONEY
               END-IF
               IF      SYU-KOH3HKNNUM          =   SPA-P02K-KOHNUM
                   MOVE    SYU-KOH3HKNFTNMONEY     TO  WRK-FTNMONEY
                   MOVE    1                       TO  FLG-FTNMONEY
               END-IF
               IF      SYU-KOH4HKNNUM          =   SPA-P02K-KOHNUM
                   MOVE    SYU-KOH4HKNFTNMONEY     TO  WRK-FTNMONEY
                   MOVE    1                       TO  FLG-FTNMONEY
               END-IF
      *
               IF      FLG-FTNMONEY        =   1
                   ADD     1                   TO  IDX-TBL
                   MOVE    SYU-SRYYMD          TO  TBL-SRYYMD (IDX-TBL)
                   MOVE    SYU-NYUGAIKBN       TO  TBL-NYUGAIKBN
                                                             (IDX-TBL)
                   MOVE    WRK-FTNMONEY        TO  TBL-FTNMONEY
                                                             (IDX-TBL)
                   MOVE    "1"                 TO  TBL-JITAKBN(IDX-TBL)
               END-IF
      *
               MOVE    "key70"             TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           END-PERFORM
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key70"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3001-SYUNOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担額テーブル検索編集処理
      *****************************************************************
       30021-KOHFTNETC-HENSYU-SEC                SECTION.
      *
           INITIALIZE                      PTKOHFTNETC-REC
           MOVE    SPA-HOSPNUM         TO  PTKOHFTNETC-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTKOHFTNETC-PTID
           MOVE    SPA-P02K-KOHID      TO  PTKOHFTNETC-KOHID
           MOVE    SPA-P02K-NAI-SRYYM
                                       TO  PTKOHFTNETC-SRYYMD(1:6)
           MOVE    "%"                 TO  PTKOHFTNETC-SRYYMD(7:1)
      *
           MOVE    PTKOHFTNETC-REC     TO  MCPDATA-REC
           MOVE    "tbl_ptkohftnetc"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohftnetc"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-PTKOHFTNETC-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHFTNETC
           END-IF
           PERFORM UNTIL    FLG-PTKOHFTNETC    =   1
               ADD     1                   TO  IDX-TBL
               MOVE    PTKOHFTNETC-SRYYMD  TO  TBL-SRYYMD (IDX-TBL)
               MOVE    PTKOHFTNETC-NYUGAIKBN
                                           TO  TBL-NYUGAIKBN (IDX-TBL)
               MOVE    PTKOHFTNETC-FTNMONEY
                                           TO  TBL-FTNMONEY  (IDX-TBL)
               MOVE    "2"                 TO  TBL-JITAKBN(IDX-TBL)
      *
               MOVE    "tbl_ptkohftnetc"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-PTKOHFTNETC-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptkohftnetc"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       30021-KOHFTNETC-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ公費負担額テーブル検索編集処理
      *****************************************************************
       30022-PARA-KOHFTNETC-SEC                SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTKOHFTNETC"       TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           PERFORM
               UNTIL   FLG-PARA            =   1
               MOVE    PARA-DATA-REC       TO  PTKOHFTNETC-REC
               IF     (PTKOHFTNETC-KOHID   =   SPA-P02K-KOHID )  AND
                      (PTKOHFTNETC-SRYYMD(1:6)
                                           =   SPA-P02K-NAI-SRYYM )
      *
                   ADD     1                   TO  IDX-TBL
                   MOVE    PTKOHFTNETC-SRYYMD  TO  TBL-SRYYMD (IDX-TBL)
                   MOVE    PTKOHFTNETC-NYUGAIKBN
                                           TO  TBL-NYUGAIKBN (IDX-TBL)
                   MOVE    PTKOHFTNETC-FTNMONEY
                                           TO  TBL-FTNMONEY  (IDX-TBL)
                   MOVE    FTNETC-SYORIKBN TO  TBL-SYORIKBN  (IDX-TBL)
                   MOVE    "2"             TO  TBL-JITAKBN(IDX-TBL)
      *
               END-IF
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       30022-PARA-KOHFTNETC-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担額テーブルＳＯＲＴ処理
      *****************************************************************
       3003-TBL-HENSYU-SEC                SECTION.
      *
           INITIALIZE                  TBL-HENSYU-AREA2
           MOVE    ZERO                TO  IDX
      *    日付、自他、入外の昇順にＳＯＲＴ
           PERFORM VARYING     IDX     FROM    1   BY   1
                   UNTIL       IDX     >   IDX-TBL
               MOVE    "99999999"          TO  WRK-KEY-SRYYMD
               MOVE    "9"                 TO  WRK-KEY-JITAKBN
               MOVE    "9"                 TO  WRK-KEY-NYUGAIKBN
               MOVE    ZERO                TO  IDX-K
               PERFORM VARYING     IDY     FROM    1   BY   1
                       UNTIL       IDY     >   IDX-TBL
                   IF      TBL-SRYYMD (IDY)    NOT =   SPACE
                       IF      TBL-SRYYMD (IDY)    <   WRK-KEY-SRYYMD
                           MOVE    TBL-SRYYMD (IDY)
                                                   TO  WRK-KEY-SRYYMD
                           MOVE    TBL-JITAKBN (IDY)
                                                   TO  WRK-KEY-JITAKBN
                           MOVE    TBL-NYUGAIKBN (IDY)
                                                   TO  WRK-KEY-NYUGAIKBN
                           MOVE    IDY             TO  IDX-K
                       END-IF
                       IF     (TBL-SRYYMD (IDY)    =   WRK-KEY-SRYYMD )
                       AND    (TBL-JITAKBN (IDY)   <   WRK-KEY-JITAKBN)
                           MOVE    TBL-JITAKBN (IDY)
                                                   TO  WRK-KEY-JITAKBN
                           MOVE    TBL-NYUGAIKBN (IDY)
                                                   TO  WRK-KEY-NYUGAIKBN
                           MOVE    IDY             TO  IDX-K
                       END-IF
                       IF     (TBL-SRYYMD  (IDY)   =   WRK-KEY-SRYYMD )
                       AND    (TBL-JITAKBN (IDY)   =   WRK-KEY-JITAKBN)
                       AND    (TBL-NYUGAIKBN(IDY)  <
                                                   WRK-KEY-NYUGAIKBN)
                           MOVE    TBL-NYUGAIKBN (IDY)
                                                   TO  WRK-KEY-NYUGAIKBN
                           MOVE    IDY             TO  IDX-K
                       END-IF
                   END-IF
               END-PERFORM
               MOVE    TBL-HENSYU-OCC (IDX-K)  TO  TBL-HENSYU-OCC2 (IDX)
               INITIALIZE                      TBL-HENSYU-OCC (IDX-K)
           END-PERFORM
      *
      *    ＳＰＡ編集
           INITIALIZE                  SPA-P02K-JIGOKEI 
           INITIALIZE                  SPA-P02K-TAGOKEI
           MOVE    ZERO                TO  SPA-P02K-MAX
           MOVE    1                   TO  WRK-LIST-INIT
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY   1
                   UNTIL      (IDX     >   100 )
                         OR   (TBL2-SRYYMD (IDX)   =   SPACE )
               ADD     1                   TO  SPA-P02K-MAX
      *        削除以外の連番
               IF      TBL2-SYORIKBN (IDX)     =   "3"
                   CONTINUE
               ELSE
                   ADD     1                   TO  IDY
                   MOVE    IDY                 TO  SPA-P02K-TNUM (IDX)
      *            負担金合計
                   IF      TBL2-JITAKBN (IDX)  =   "1"
                       ADD     TBL2-FTNMONEY (IDX) TO  SPA-P02K-JIGOKEI
                   ELSE
                       ADD     TBL2-FTNMONEY (IDX) TO  SPA-P02K-TAGOKEI
                   END-IF
               END-IF
               MOVE    TBL2-SRYYMD   (IDX) TO  SPA-P02K-TSRYYMD (IDX)
               MOVE    TBL2-JITAKBN  (IDX) TO  SPA-P02K-TJITAKBN (IDX)
               MOVE    TBL2-NYUGAIKBN(IDX) TO  SPA-P02K-TNYUGAIKBN (IDX)
               MOVE    TBL2-FTNMONEY (IDX) TO  SPA-P02K-TFTNMONEY (IDX)
               MOVE    TBL2-SYORIKBN (IDX) TO  SPA-P02K-TSYORIKBN (IDX)
      *
               PERFORM 30031-TBL-MEIHENKAN-SEC
           END-PERFORM
           .
       3003-TBL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集用処理
      *****************************************************************
       30031-TBL-MEIHENKAN-SEC             SECTION.
      *
           MOVE    SPA-P02K-TSRYYMD (IDX)  TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD              TO  SPA-P02K-TGSRYYMD (IDX)
      *
           MOVE    SPA-P02K-TFTNMONEY(IDX) TO  WRK-ZZZ9
           MOVE    WRK-ZZZ9                TO  SPA-P02K-TGFTNMONEY(IDX)
      *
           IF      SPA-P02K-TJITAKBN (IDX) =   "1"
               MOVE    "自院　　"          TO  SPA-P02K-TGJITAKBN
                                                                 (IDX)
           ELSE
               MOVE    "　　　他院"        TO  SPA-P02K-TGJITAKBN
                                                                 (IDX)
           END-IF
      *
           IF      SPA-P02K-TNYUGAIKBN (IDX)   =   "1"
               MOVE    "入"                TO  SPA-P02K-TGNYUGAIKBN
                                                                 (IDX)
           ELSE
               MOVE    "外"                TO  SPA-P02K-TGNYUGAIKBN
                                                                 (IDX)
           END-IF
           .
       30031-TBL-MEIHENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面より処理
      *****************************************************************
       3001-PID1-SYORI-SEC              SECTION.
      *
           EVALUATE    SPA-PIDCD
               WHEN    "2401"
                   IF      SPA-PID1-FLG        =   "OK"
                       PERFORM 210-BACK
                   ELSE
                       MOVE    12              TO  SPA-P02K-CUR
                   END-IF
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PIDCD
           MOVE    SPACE               TO  SPA-PID1-FLG
           .
       3001-PID1-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           IF     (WRK-LIST-INIT       =   1  )  OR
                  (SPA-P02K-MAX        =   ZERO )
               MOVE    ZERO                TO  WRK-LIST-ROW
           ELSE
               MOVE    P02K-FTNLIST-ROW    TO  WRK-LIST-ROW
           END-IF
      *
           MOVE    SPACE               TO  P02K
           INITIALIZE                      P02K
      *
           MOVE    WRK-LIST-ROW        TO  P02K-FTNLIST-ROW
      *
           MOVE    SPA-P02K-GMN-SRYYM  TO  P02K-SRYYM
      *
           MOVE    SPA-P02K-MSG01      TO  P02K-MSG01-VALUE
           MOVE    "black"             TO  P02K-MSG01-STYLE
           MOVE    SPA-P02K-MSG02      TO  P02K-MSG02
      *    他
           IF      SPA-P02K-FTNRUIKBN  =   "9"
               MOVE    "blue"              TO  P02K-MSG03-STYLE
               MOVE    SPA-P02K-MSG07      TO  P02K-MSG03-VALUE
           ELSE
               MOVE    "black"             TO  P02K-MSG03-STYLE
               MOVE    SPA-P02K-MSG03      TO  P02K-MSG03-VALUE
           END-IF
           MOVE    SPA-P02K-MSG04      TO  P02K-MSG04-VALUE
           MOVE    SPA-P02K-MSG05      TO  P02K-MSG05
           IF     (SPA-P02K-JIGOKEI    +   SPA-P02K-TAGOKEI
                                       +   SPA-P02K-TNYUJGNGAK )
                                       >   SPA-P02K-GAIJGNGAKKEI
      *        上限額オーバー
               MOVE    "red"               TO  P02K-MSG04-STYLE
           ELSE
               MOVE    "black"             TO  P02K-MSG04-STYLE
           END-IF
           MOVE    SPA-P02K-MSG06      TO  P02K-MSG06
      *    入力不可
           IF      SPA-P02K-FTNRUIKBN  =   "9"
               MOVE    "red"               TO  P02K-MSG07-STYLE
               MOVE    "＜入力不可＞"      TO  P02K-MSG07-VALUE
           END-IF
      *
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-P02K-MAX   )
                          OR  (IDY     >=  50             )
               IF      SPA-P02K-TSYORIKBN (IDX)    =   "3"
                   CONTINUE
               ELSE
                    ADD     1                  TO  IDY
                    MOVE    SPA-P02K-TNUM(IDX) TO  WRK-Z9
                    MOVE    WRK-Z9             TO  P02K-TRENNUM (IDY)
                    MOVE    SPA-P02K-TGSRYYMD (IDX)
                                               TO  P02K-TSRYYMD (IDY)
                    MOVE    SPA-P02K-TGJITAKBN (IDX)
                                               TO  P02K-TJITAKBN(IDY)
                    MOVE    SPA-P02K-TGNYUGAIKBN (IDX)
                                               TO  P02K-TNYUGAIKBN(IDY)
                    MOVE    SPA-P02K-TGFTNMONEY(IDX)
                                               TO  P02K-TFTNMONEY(IDY)
                    IF      IDY                =   SPA-P02K-SELNUM
                        MOVE    "T"            TO  P02K-FTNLIST-SELECT
                                                                  (IDY)
                        MOVE    IDY            TO  P02K-FTNLIST-ROW
                    ELSE
                        MOVE    "F"            TO  P02K-FTNLIST-SELECT
                                                                  (IDY)
                    END-IF
               END-IF
           END-PERFORM
           MOVE    IDY                 TO  P02K-FTNLIST-COUNT
           IF     (WRK-LIST-INIT       =   2  ) 
               MOVE    IDY                 TO  P02K-FTNLIST-ROW
           END-IF
      *
           MOVE    SPA-P02K-SELNUM     TO  P02K-SELNUM
      *
           MOVE    SPA-P02K-NYUGAIKBN-G    TO  P02K-NYUGAIKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   2   )
               MOVE    SPA-P02K-NYUGAIKBN-LIST (IDX)
                                       TO  P02K-NYUGAIKBN-ITEM (IDX)
           END-PERFORM
           MOVE    2                   TO  P02K-NYUGAIKBN-COUNT
           MOVE    SPA-P02K-SRYDD      TO  P02K-SRYDD
           MOVE    SPA-P02K-FTNMONEY   TO  P02K-FTNMONEY
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
           IF      SPA-P02K-CUR        =   ZERO
               EVALUATE    WRK-MCP-WIDGET
                   WHEN    "SELNUM"
                       MOVE    2                   TO  SPA-P02K-CUR
                   WHEN    "NYUGAIKBN"
                       MOVE    3                   TO  SPA-P02K-CUR
                   WHEN    "SRYDD"
                       MOVE    4                   TO  SPA-P02K-CUR
                   WHEN    "FTNMONEY"
                       MOVE    10                  TO  SPA-P02K-CUR
               END-EVALUATE
           END-IF
           EVALUATE    SPA-P02K-CUR
               WHEN    01
                   MOVE    "SELNUM"           TO  MCP-WIDGET
               WHEN    02
                   MOVE    "NYUGAIKBN"        TO  MCP-WIDGET
               WHEN    03
                   MOVE    "SRYDD"            TO  MCP-WIDGET
               WHEN    04
                   MOVE    "FTNMONEY"         TO  MCP-WIDGET
               WHEN    10
                   MOVE    "B10"              TO  MCP-WIDGET
               WHEN    12
                   MOVE    "B12"              TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B01"              TO  MCP-WIDGET
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
                   PERFORM 210-BACK-SYORI-SEC
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 420-CLREA-SEC
      *    削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 430-DELETE-SEC
      *    更新
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 410-KOUSIN-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
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
           MOVE    ZERO                TO  SPA-P02K-CUR
      *
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        選択
               WHEN    "ACTIVATE"       ALSO    "SELNUM"
                   PERFORM  420-SELNUM-CHK-SEC
      *        選択
               WHEN    ANY              ALSO    "KOHFTNLIST"
                   PERFORM  430-FTNLIST-CHK-SEC
      *        その他
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE.
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    行選択処理
      *****************************************************************
       420-SELNUM-CHK-SEC       SECTION.
      *
           MOVE    P02K-SELNUM         TO  SPA-P02K-SELNUM
           MOVE    1                   TO  SPA-P02K-CUR
      *
           MOVE    ZERO                TO  SPA-P02K-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-P02K-MAX
               IF     (SPA-P02K-TNUM (IDX) NOT =   ZERO    )  AND
                      (SPA-P02K-TNUM (IDX)     =   SPA-P02K-SELNUM)
                   MOVE    IDX                 TO  SPA-P02K-IDX
                   MOVE    SPA-P02K-MAX        TO  IDX
               END-IF
           END-PERFORM
           IF      SPA-P02K-IDX        =   ZERO
               IF      SPA-P02K-SELNUM     NOT =   ZERO
                   MOVE    "0001"              TO  SPA-ERRCD
               END-IF
           ELSE
               IF      SPA-P02K-TJITAKBN (SPA-P02K-IDX) =   "1"
      *            自院はエラー
                   MOVE    "0002"              TO  SPA-ERRCD
                   MOVE    ZERO                TO  SPA-P02K-IDX
                   MOVE    ZERO                TO  SPA-P02K-SELNUM
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *        選択
               INITIALIZE              SPA-P02K-INPUT-AREA
               MOVE    SPA-P02K-NYUGAIKBN-LIST (2)
                                       TO  SPA-P02K-NYUGAIKBN-G
               IF      SPA-P02K-IDX        >   ZERO
                   PERFORM 4201-INPUT-HEN-SEC
               END-IF
               MOVE    2                   TO  SPA-P02K-CUR
           END-IF
           .
       420-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    行編集処理
      *****************************************************************
       4201-INPUT-HEN-SEC       SECTION.
      *
           MOVE    SPA-P02K-TNYUGAIKBN (SPA-P02K-IDX)
                                       TO  SPA-P02K-NYUGAIKBN
           MOVE    SPA-P02K-TFTNMONEY (SPA-P02K-IDX)
                                       TO  SPA-P02K-FTNMONEY
           MOVE    SPA-P02K-TSRYYMD  (SPA-P02K-IDX) (7:2)
                                       TO  SPA-P02K-SRYDD
           MOVE    SPA-P02K-SRYDD      TO  SPA-P02K-MAEDD
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-P02K-NYUGAIKBN  =   SPA-P02K-NYUGAIKBN-LIST
                                                         (IDX)(1:1)
                   MOVE    SPA-P02K-NYUGAIKBN-LIST (IDX)
                                               TO  SPA-P02K-NYUGAIKBN-G
               END-IF
           END-PERFORM
           .
       4201-INPUT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    行選択処理
      *****************************************************************
       430-FTNLIST-CHK-SEC       SECTION.
      *
      *    行選択
           MOVE    ZERO                TO  SPA-P02K-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   50
               IF      P02K-FTNLIST-SELECT  (IDX)    =   "T"
                   MOVE    IDX              TO  SPA-P02K-SELNUM
                   MOVE    100              TO  IDX
               END-IF
           END-PERFORM
      *
           IF      SPA-P02K-SELNUM     >   ZERO
               MOVE    SPA-P02K-SELNUM     TO  P02K-SELNUM
               PERFORM 420-SELNUM-CHK-SEC
           END-IF
           .
       430-FTNLIST-CHK-EXT.
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
           MOVE    P02K-NYUGAIKBN      TO  SPA-P02K-NYUGAIKBN-G
           MOVE    P02K-SRYDD          TO  SPA-P02K-SRYDD
           MOVE    P02K-FTNMONEY       TO  SPA-P02K-FTNMONEY
      *
           IF      SPA-P02K-SRYDD  NOT =   SPA-P02K-MAEDD
               MOVE    ZERO                TO  SPA-P02K-KEIFLG
           END-IF
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェックと別画面処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    入外区分
           PERFORM 41021-NYUGAIKBN-CHK-SEC
      *    日付
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-P02K-SRYDD  NOT =   ZERO
                   PERFORM 41022-SRYDD-CHK-SEC
               END-IF
           END-IF
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入外区分チェック処理
      *****************************************************************
       41021-NYUGAIKBN-CHK-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   2   )
                           OR (FLG-OK  =   1   )
               IF      SPA-P02K-NYUGAIKBN  =   SPA-P02K-NYUGAIKBN-LIST
                                                         (IDX)(1:1)
                   MOVE    SPA-P02K-NYUGAIKBN-LIST (IDX)
                                           TO  SPA-P02K-NYUGAIKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
      *        エラーは外来
               MOVE    SPA-P02K-NYUGAIKBN-LIST (2)
                                           TO  SPA-P02K-NYUGAIKBN-G
           END-IF
           .
       41021-NYUGAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック処理
      *****************************************************************
       41022-SRYDD-CHK-SEC         SECTION.
      *
           IF      SPA-P02K-SRYDD      >   SPA-P02K-NAIMATU
               MOVE    "0003"              TO  SPA-ERRCD
           END-IF
      *    公費開始日チェック
           IF     (SPA-NAI24-TKOHSTYMD (SPA-NAI24-FTNSELNUM)(1:6)
                                       =   SPA-P02K-NAI-SRYYMD(1:6))
             AND  (SPA-NAI24-TKOHSTYMD (SPA-NAI24-FTNSELNUM)(7:2)
                                       >   SPA-P02K-SRYDD     )
             AND  (SPA-ERRCD           =   SPACE              )
               MOVE    "0006"              TO  SPA-ERRCD
           END-IF
      *    公費終了日チェック
           IF     (SPA-NAI24-TKOHEDYMD (SPA-NAI24-FTNSELNUM)(1:6)
                                       =   SPA-P02K-NAI-SRYYMD(1:6))
             AND  (SPA-NAI24-TKOHEDYMD (SPA-NAI24-FTNSELNUM)(7:2)
                                       <   SPA-P02K-SRYDD     )
             AND  (SPA-ERRCD           =   SPACE              )
               MOVE    "0006"              TO  SPA-ERRCD
           END-IF
      *    履歴期間
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-P02K-SRYDD      <   SPA-P02K-NAI-SRYDD )
                   OR (SPA-P02K-SRYDD      >   SPA-P02K-NAI-ENDDD )
                   IF      SPA-P02K-KEIFLG     =   ZERO
                       MOVE    "K004"              TO  SPA-ERRCD
                       MOVE    1                   TO  SPA-P02K-KEIFLG
                   END-IF
               END-IF
               MOVE    SPA-P02K-SRYDD      TO  SPA-P02K-MAEDD
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    3               TO  SPA-P02K-CUR
           END-IF
           .
       41022-SRYDD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     クリア処理
      *****************************************************************
       420-CLREA-SEC                    SECTION.
      *
           INITIALIZE                  SPA-P02K-INPUT-AREA
           MOVE    ZERO                TO  SPA-P02K-SELNUM
           MOVE    ZERO                TO  SPA-P02K-IDX
           MOVE    SPA-P02K-NYUGAIKBN-LIST (2)
                                       TO  SPA-P02K-NYUGAIKBN-G
           MOVE    1                   TO  SPA-P02K-CUR
           IF      SPA-P02K-FTNRUIKBN  =   "9"
               MOVE    99                  TO  SPA-P02K-CUR
           END-IF
           .
       420-CLREA-EXT.
           EXIT.
      *
      *****************************************************************
      *     削除処理
      *****************************************************************
       430-DELETE-SEC                    SECTION.
      *
           IF      SPA-P02K-SELNUM     =   ZERO
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P02K-CUR
           ELSE
               MOVE    "3"                 TO  SPA-P02K-TSYORIKBN
                                                         (SPA-P02K-IDX)
               PERFORM 420-CLREA-SEC
      *        テーブル再編集処理
               PERFORM 440-TBLHEN-GOKEI-SEC
               MOVE    1                   TO  SPA-P02K-SYORI
           END-IF
           .
       430-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *     テーブル再編集処理
      *****************************************************************
       440-TBLHEN-GOKEI-SEC            SECTION.
      *
           INITIALIZE                  SPA-P02K-TAGOKEI
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY   1
                   UNTIL      (IDX     >   SPA-P02K-MAX )
      *        削除以外の連番
               IF      SPA-P02K-TSYORIKBN (IDX)    =   "3"
                   MOVE    ZERO                TO  SPA-P02K-TNUM (IDX)
               ELSE
      *            連番採番
                   ADD     1                   TO  IDY
                   MOVE    IDY                 TO  SPA-P02K-TNUM (IDX)
      *            他院負担金合計
                   IF      SPA-P02K-TJITAKBN (IDX) =   "2"
                       ADD     SPA-P02K-TFTNMONEY (IDX)
                                                   TO  SPA-P02K-TAGOKEI
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    SPA-P02K-TAGOKEI    TO  WRK-ZZZ9
           STRING  "　　　他院累計："
                   WRK-ZZZ9            DELIMITED  BY  SIZE
                                       INTO    SPA-P02K-MSG03
           END-STRING
           COMPUTE WRK-GOUKEI          =   SPA-P02K-TAGOKEI
                                       +   SPA-P02K-JIGOKEI
                                       +   SPA-P02K-TNYUJGNGAK
           MOVE    WRK-GOUKEI          TO  WRK-ZZZ9
           STRING  "合　　計："
                   WRK-ZZZ9            DELIMITED  BY  SIZE
                                       INTO    SPA-P02K-MSG04
           END-STRING
           .
       440-TBLHEN-GOKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *     更新処理
      *****************************************************************
       410-KOUSIN-SEC         SECTION.
      *
      *    負担累計入力ありは更新不可
           IF      SPA-P02K-FTNRUIKBN  =   "9"
               MOVE    "0012"              TO  SPA-ERRCD
               MOVE    99                  TO  SPA-P02K-CUR
               GO      TO      410-KOUSIN-EXT
           END-IF
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4100-KANREN-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4101-TBL-KOUSIN-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               PERFORM 420-CLREA-SEC
               MOVE    1               TO  SPA-P02K-SYORI
               MOVE    2               TO  WRK-LIST-INIT
           END-IF
           .
       410-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *     関連チェック処理
      *****************************************************************
       4100-KANREN-CHK-SEC         SECTION.
      *
           IF      SPA-P02K-SRYDD      =   ZERO
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-P02K-CUR
           END-IF
           .
       4100-KANREN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     テーブル更新処理
      *****************************************************************
       4101-TBL-KOUSIN-SEC         SECTION.
      *
           IF      SPA-P02K-IDX        =   ZERO
      *        新規追加
               IF      SPA-P02K-MAX        >=  50
                   MOVE    "0010"              TO  SPA-ERRCD
               ELSE
                   ADD     1                   TO  SPA-P02K-MAX
                   MOVE    SPA-P02K-MAX        TO  IDX
                   MOVE    "1"                 TO  SPA-P02K-TSYORIKBN
                                                              (IDX)
               END-IF
           ELSE
      *        更新
               MOVE    SPA-P02K-IDX        TO  IDX
               IF      SPA-P02K-TSYORIKBN (IDX) =   SPACE
                   MOVE    "2"             TO  SPA-P02K-TSYORIKBN (IDX)
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *        同一日は９９件まで
               PERFORM 41011-TBL-DAYCHK-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    "2"                 TO  SPA-P02K-TJITAKBN   (IDX)
               MOVE    SPA-P02K-NYUGAIKBN  TO  SPA-P02K-TNYUGAIKBN (IDX)
               MOVE    SPA-P02K-NAI-SRYYM  TO  SPA-P02K-TSRYYMD    (IDX)
                                                               (1:6)
               MOVE    SPA-P02K-SRYDD      TO  SPA-P02K-TSRYYMD    (IDX)
                                                               (7:2)
               MOVE    SPA-P02K-FTNMONEY   TO  SPA-P02K-TFTNMONEY (IDX)
      *
               PERFORM 30031-TBL-MEIHENKAN-SEC
      *        テーブル再編集処理
               PERFORM 440-TBLHEN-GOKEI-SEC
           END-IF
           .
       4101-TBL-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *     同一日チェック処理
      *****************************************************************
       41011-TBL-DAYCHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  WRK-DAYCNT
           PERFORM VARYING     IDZ     FROM    1   BY   1
                   UNTIL      (IDZ     >   SPA-P02K-MAX )
              IF      (SPA-P02K-TJITAKBN   (IDZ)   =   "2" )
                 AND  (SPA-P02K-TSRYYMD (IDZ)(7:2) =   SPA-P02K-SRYDD )
                   ADD     1               TO  WRK-DAYCNT
              END-IF
           END-PERFORM
           IF      WRK-DAYCNT          >   99
               MOVE    "0011"          TO  SPA-ERRCD
           END-IF
           .
       41011-TBL-DAYCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     登録処理
      *****************************************************************
       490-TOROKU-SEC         SECTION.
      *
      *    負担累計入力ありは更新不可
           IF      SPA-P02K-FTNRUIKBN  =   "9"
               MOVE    "0012"              TO  SPA-ERRCD
               MOVE    99                  TO  SPA-P02K-CUR
               GO      TO      490-TOROKU-EXT
           END-IF
      *
      *    更新処理
           IF     (SPA-P02K-SRYDD      =   ZERO )  AND
                  (SPA-P02K-FTNMONEY   =   ZERO )
               CONTINUE
           ELSE
               PERFORM 410-KOUSIN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                    GO      TO      490-TOROKU-EXT
               END-IF
           END-IF
      *
      *    パラメタ削除
           PERFORM 4901-PARA-DELETE-SEC
      *
           INITIALIZE                      PTKOHFTNETC-REC
           MOVE    ZERO                TO  WRK-RENNUM
           PERFORM VARYING     IDX     FROM    1   BY   1
                   UNTIL      (IDX     >   SPA-P02K-MAX  )
                           OR (SPA-ERRCD   NOT =   SPACE )
               MOVE    "99999999"          TO  WRK-KEY-SRYYMD
               MOVE    ZERO                TO  IDX-K
               PERFORM VARYING     IDY     FROM    1   BY   1
                       UNTIL      (IDY     >   SPA-P02K-MAX )
                   IF     (SPA-P02K-TSRYYMD (IDY)  =   SPACE ) OR
                          (SPA-P02K-TJITAKBN (IDY) =   "1" )   OR
                          (SPA-P02K-TSYORIKBN(IDY) =   "3" )
                       CONTINUE
                   ELSE
                       IF      SPA-P02K-TSRYYMD (IDY)
                                                   <   WRK-KEY-SRYYMD
                           MOVE    IDY             TO  IDX-K
                           MOVE    SPA-P02K-TSRYYMD (IDY)
                                                   TO  WRK-KEY-SRYYMD
                       END-IF
                   END-IF
               END-PERFORM
      *
               IF      IDX-K           >   ZERO
                   PERFORM 4901-PTKOHFTNETC-INSERT-SEC
                   PERFORM 400-PARA-INSERT-SEC
                   MOVE    SPACE           TO  SPA-P02K-TSRYYMD
                                                              (IDX-K)
              END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
      *        公費テーブル編集
               PERFORM 4902-FTNTBL-HEN-SEC
      *        更新対象編集
               PERFORM VARYING     IDX     FROM    1   BY   1
                       UNTIL      (IDX     >   100  )
                   IF      SPA-NAI24-KOHFTN-ID (IDX)   =   ZERO
                       MOVE    SPA-P02K-KOHID  TO  SPA-NAI24-KOHFTN-ID
                                                                (IDX)
                       MOVE    SPA-P02K-NAI-SRYYM
                                               TO  SPA-NAI24-KOHFTN-YM
                                                                (IDX)
                   END-IF
                   IF     (SPA-P02K-KOHID  =   SPA-NAI24-KOHFTN-ID
                                                                (IDX))
                      AND (SPA-P02K-NAI-SRYYM
                                           =  SPA-NAI24-KOHFTN-YM
                                                                (IDX))
                       MOVE    100            TO  IDX
                   END-IF
               END-PERFORM
      *
               INITIALIZE                  SPA-GMN24-FTN-AREA2
               MOVE    "1"                 TO  SPA-UPDFLG
               PERFORM 210-BACK
           END-IF
           .
       490-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費テーブル編集処理
      *****************************************************************
       4902-FTNTBL-HEN-SEC         SECTION.
      *
           IF     SPA-P02K-FUKUFLG     =   ZERO
               IF     WRK-RENNUM       >   ZERO
                   MOVE   "2"              TO  SPA-NAI24-TFTNETCKBN
                                                 (SPA-NAI24-FTNSELNUM2)
               ELSE
                   MOVE   "0"              TO  SPA-NAI24-TFTNETCKBN
                                                 (SPA-NAI24-FTNSELNUM2)
               END-IF
      *        他院累計額を負担累計へ
               MOVE    SPA-P02K-TAGOKEI    TO  SPA-GMN24-TNYUJGNGAK
                                                 (SPA-NAI24-FTNSELNUM2)
           ELSE
               MOVE    "99999999"          TO  WRK-FTNSTYMD
               MOVE    ZERO                TO  IDX4
               PERFORM VARYING     IDX     FROM    1   BY   1
                       UNTIL      (IDX     >   SPA-FTN2-MAX )
                   IF      SPA-NAI24-TFTNSTYMD (IDX)(1:6)
                                           =   SPA-P02K-NAI-SRYYMD(1:6)
                       IF     WRK-RENNUM       >   ZERO
                           MOVE   "2"          TO  SPA-NAI24-TFTNETCKBN
                                                                 (IDX)
                       ELSE
                           MOVE   "0"          TO  SPA-NAI24-TFTNETCKBN
                                                                 (IDX)
                       END-IF
      *                日付判定
                       IF      SPA-NAI24-TFTNSTYMD (IDX)
                                                   <   WRK-FTNSTYMD
                           MOVE    SPA-NAI24-TFTNSTYMD (IDX)
                                                   TO  WRK-FTNSTYMD
                           MOVE    IDX             TO  IDX4
                       END-IF
                       MOVE    ZERO            TO  SPA-GMN24-TNYUJGNGAK
                                                               (IDX)
                   END-IF
               END-PERFORM
      *        他院累計額を負担累計へ
               MOVE    SPA-P02K-TAGOKEI    TO  SPA-GMN24-TNYUJGNGAK
                                                           (IDX4)
           END-IF
           .
       4902-FTNTBL-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ削除
      *****************************************************************
       4901-PARA-DELETE-SEC         SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTKOHFTNETC"       TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           MOVE    ZERO                TO  WRK-MAX-EDANUM
           PERFORM
               UNTIL   FLG-PARA            =   1
               MOVE    PARA-DATA-REC       TO  PTKOHFTNETC-REC
               IF     (PTKOHFTNETC-KOHID   =   SPA-P02K-KOHID )  AND
                      (PTKOHFTNETC-SRYYMD(1:6)
                                           =   SPA-P02K-NAI-SRYYM )
      *            削除
                   MOVE    PARA-REC            TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_para"          TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                                   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               END-IF
               MOVE    PARA-EDANUM         TO  WRK-MAX-EDANUM
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       4901-PARA-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担額テーブル編集処理
      *****************************************************************
       4901-PTKOHFTNETC-INSERT-SEC           SECTION.
      *
           IF      PTKOHFTNETC-SRYYMD  =   SPA-P02K-TSRYYMD (IDX-K)
               ADD     1                   TO  WRK-RENNUM
           ELSE
               MOVE    1                   TO  WRK-RENNUM
           END-IF
      *
           INITIALIZE                      PTKOHFTNETC-REC
           MOVE    SPA-HOSPNUM         TO  PTKOHFTNETC-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTKOHFTNETC-PTID
           MOVE    SPA-P02K-KOHID      TO  PTKOHFTNETC-KOHID
           MOVE    SPA-P02K-KOHNUM     TO  PTKOHFTNETC-KOHNUM
           MOVE    SPA-P02K-TSRYYMD (IDX-K)
                                       TO  PTKOHFTNETC-SRYYMD
           MOVE    WRK-RENNUM          TO  PTKOHFTNETC-RENNUM
           MOVE    SPA-P02K-TNYUGAIKBN (IDX-K)
                                       TO  PTKOHFTNETC-NYUGAIKBN
           MOVE    SPA-P02K-TFTNMONEY (IDX-K)
                                       TO  PTKOHFTNETC-FTNMONEY
      *
           MOVE    SPA-P02K-TSYORIKBN (IDX-K)
                                       TO  FTNETC-SYORIKBN
           .
       4901-PTKOHFTNETC-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタファイル作成処理
      *****************************************************************
       400-PARA-INSERT-SEC           SECTION.
      *
           INITIALIZE                  PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTKOHFTNETC"       TO  PARA-FILEMEI
           ADD     1                   TO  WRK-MAX-EDANUM
           MOVE    WRK-MAX-EDANUM      TO  PARA-EDANUM
      *
           MOVE    PTKOHFTNETC-REC     TO  PARA-DATA-REC
      *
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
               MOVE    PTKOHFTNETC-REC     TO  PARA-DATA-REC
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02 PARA INSERT ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY(1:42)
               END-IF
           ELSE
               MOVE    PTKOHFTNETC-REC     TO  PARA-DATA-REC
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
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
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SYORI-SEC          SECTION.
      *
           IF      SPA-P02K-SYORI      =   1
               MOVE   "2401"               TO  SPA-PIDCD
           ELSE
               PERFORM 210-BACK
           END-IF
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "P02K"              TO  SPA-MOTOPG
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
           MOVE    "P024"              TO  SPA-P02-GAMEN
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
           MOVE    "P02K"              TO  MCP-WINDOW
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
                   MOVE    "選択番号エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "他院分ではありません。選択できません。"
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "暦日エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "K004"
                   STRING    "警告！！"
                             "選択中の上限額履歴の期間外の日です。"
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0005"
                   MOVE    "選択番号を入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   STRING  "公費適用期間外です。日を確認してください。"
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0010"
                   STRING  "明細が５０件以上となります。"
                           "これ以上登録できません。"
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0010"
                   STRING  "確定件数が５０件以上となります。"
                           "登録後、再度入力してください。"
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0011"
                   MOVE    "同一日が９９件以上です。"
                                               TO  SPA-ERRMSG
               WHEN    "0012"
                   STRING  "他一部負担累計をクリアしてから"
                           "入力してください。"
                                               INTO  SPA-ERRMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P02K"              TO  SPA-MOTOPG
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
           MOVE    SPACE               TO  PID1
           INITIALIZE                      PID1
           EVALUATE    SPA-PIDCD
               WHEN    "2401"
                   STRING  "今回入力分は反映されません。"
                           "よろしいですか？"
                                             DELIMITED  BY  SIZE
                                       INTO  PID1-ID1MSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-PIDCD
                                       TO  PID1-ID1MSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PID1-FLG
      *
           MOVE    SPA-PIDCD           TO  PID1-ID1CODE
           MOVE    "戻る"              TO  PID1-B01
           MOVE    "ＯＫ"              TO  PID1-B12
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "P02K"              TO  SPA-MOTOPG
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
      *    収納データ読み込み処理
      *****************************************************************
       980-SYUNOU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYUNOU
               MOVE    MCPDATA-REC         TO  SYUNOU-REC
           ELSE
               MOVE    1                   TO  FLG-SYUNOU
               INITIALIZE                      SYUNOU-REC
           END-IF
      *
           .
       980-SYUNOU-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者公費負担読込処理
      *****************************************************************
       990-PTKOHFTNETC-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTKOHFTNETC
               MOVE    MCPDATA-REC         TO  PTKOHFTNETC-REC
           ELSE
               MOVE    1                   TO  FLG-PTKOHFTNETC
               INITIALIZE                      PTKOHFTNETC-REC
           END-IF
      *
           .
       990-PTKOHFTNETC-READ-EXT.
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
               MOVE    ZERO                TO  FLG-PARA
               MOVE    MCPDATA-REC         TO  PARA-REC
           ELSE
               MOVE    1                   TO  FLG-PARA
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
