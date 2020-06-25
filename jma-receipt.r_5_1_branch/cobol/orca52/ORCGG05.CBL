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
      *
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGG05.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次統計
      *  コンポーネント名  : 個別指示
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07.02.22    太田          新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/06/01  グループ診療対応
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
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    月次統計共通パラメタ
           COPY    "G01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "G01SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-PTNUM                   PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-BTPARA                  PIC 9(01).
           03  FLG-HIZUKE-ERR              PIC 9(01).
           03  FLG-PTLST-ROW               PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RENNUM                  PIC 9(04).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX-LST                     PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
      *
      *    作業領域
       01  WRK-AREA.
           03  WRK-CMB-CD                  PIC X(10).
           03  WRK-CMB-ITM-G.
               07  WRK-CMB-ITM             PIC X(200).
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-YMD                     PIC X(10).
           03  WRK-SYMD                    PIC X(08).
           03  WRK-EDTYMD1                 PIC X(10).
           03  WRK-EDTYMD3                 PIC X(22).
           03  WRK-HENYMDG                 PIC X(09).
           03  WRK-GIDMSG                  PIC X(200).
           03  WRK-PTID                    PIC 9(10).
           03  WRK-PTNUM                   PIC X(20).
           03  WRK-ZZZZ9                   PIC ZZZZ9.
           03  WRK-PTLST-SEL               PIC 9(05).
           03  WRK-MOTOPG                  PIC X(05).
           03  WRK-NAME                    PIC X(100).
           03  WRK-RENNUM                  PIC 9(04).
           03  WRK-DSP-RENNUM              PIC 9(03).
           03  WRK-SELLIMIT                PIC 9(03).
      *
       01  WRK-ROW-AREA.
           03  WRK-PTLST-ROW               PIC S9(9).
      *
       01  CONST-AREA.
           03  CONST-PTLST-MAX             PIC 9(05)   VALUE   200.
           03  CONST-BTPARA-SHELLID        PIC X(08)
                                           VALUE   "ORCBQL01".
           03  CONST-BTPARA-JOBID          PIC 9(07)
                                           VALUE   2.
           03  CONST-BTPARA-DSP-RENNUM     PIC 9(03)
                                           VALUE   3.
      *
           COPY "COMMON-SPA"       REPLACING //SPA-//
                                   BY        //WKSPA-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
           COPY    "CPQTOANY.INC".
     *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
        LINKAGE                     SECTION.
      *
      *     テーブルアクセス
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA51SCRAREA.INC".
      *
       PROCEDURE                       DIVISION    USING
                                                   MCPAREA
                                                   SPAAREA
                                                   LINKAREA
                                                   SCRAREA.
      *****************************************************************
      *    主    処    理
      *****************************************************************
      *
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                      FLG-AREA
                                           IDX-AREA
                                           WRK-AREA
                                           CNT-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-G01FREE
           MOVE    SPA-WORK        TO  SPA-G01KYOUTU
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-END
      *
           PERFORM 800-MCNDATE-SEC
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN    "PUTG"          ALSO    "CLICKED"
               PERFORM 200-GMNSENI
      *    入力
           WHEN    OTHER
               PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END             =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-G01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-G01FREE     TO  SPA-FREE
           .
      *
       000-PROC-SEC-EXT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "GERR" )
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
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "G05    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
      *
               EVALUATE    SPA-MOTOPG
               WHEN    "P97"
                   MOVE    LNK-COMMON  TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA  TO  SPA-G01FREE
                   MOVE    SPA-WORK    TO  SPA-G01KYOUTU
                   PERFORM 330-P97-SET-SEC
                   GO  TO  300-SCREEN-EXT
               END-EVALUATE
      *
           END-IF
      *
           PERFORM 310-SPASET-SEC.
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           MOVE    CONST-BTPARA-DSP-RENNUM
                                      TO   WRK-DSP-RENNUM
           PERFORM 900-BTPARA-DEL4-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *
               MOVE    2                  TO   WRK-DSP-RENNUM
               MOVE    CONST-BTPARA-DSP-RENNUM
                                      TO   WRK-SELLIMIT
               PERFORM 900-BTPARA-INS1-SEC
           END-IF
      *
           PERFORM 3101-SPASET-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       3101-SPASET-SEC                 SECTION.
      *
           INITIALIZE                  SPA-G05-AREA
      *
           MOVE    1               TO  SPA-GMN-G05-PTLST-PAGE
           PERFORM 3101-EDIT-PTLST-SEC
      *
           .
       3101-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    個別患者リスト編集処理
      *****************************************************************
       3101-EDIT-PTLST-SEC            SECTION.
      *
           MOVE    1               TO  FLG-PTLST-ROW
      *
           INITIALIZE                  SPA-GMN-G05-PTLST-AREA
      *
                                       SPA-NAI-G05-PTLST-AREA
           PERFORM 900-BTPARA-KEY9-SEL-SEC
           MOVE    BTPARA-INFO-PARA
                                   TO  QTOANY-AREA
      *
           PERFORM UNTIL ( FLG-BTPARA  NOT =   ZERO )
                    OR   ( SPA-GMN-G05-PTLST-MAX
                                   >=  CONST-PTLST-MAX )
      *
               COMPUTE SPA-GMN-G05-PTLST-MAX
                   =   SPA-GMN-G05-PTLST-MAX
                   +   1
      *
               MOVE    SPA-GMN-G05-PTLST-MAX  TO  IDX-LST
      *
               MOVE    QTOANY-PTID     TO  WRK-PTID
      *
               PERFORM 900-PTINF-KEY-SEL-SEC
               PERFORM 900-PTNUM-KEY-SEL-SEC
      *
               COMPUTE SPA-GMN-G05-TNUM(IDX-LST)
                   = ( SPA-GMN-G05-PTLST-PAGE -   1 )
                   *   CONST-PTLST-MAX
                   +   IDX-LST
      *
               MOVE    PTINF-TSTPTNUMKBN
                                       TO  SPA-GMN-G05-TTESTPT(IDX-LST)
      *
               MOVE    QTOANY-PTID     TO  SPA-NAI-G05-TPTID  (IDX-LST)
               MOVE    BTPARA-RENNUM   TO  SPA-NAI-G05-TRENNUM(IDX-LST)
               MOVE    BTPARA-UPYMD    TO  SPA-NAI-G05-TUPYMD (IDX-LST)
               MOVE    BTPARA-UPHMS    TO  SPA-NAI-G05-TUPHMS (IDX-LST)
      *
               MOVE    PTNUM-PTNUM     TO  SPA-GMN-G05-TPTNUM (IDX-LST)
      *
               MOVE    PTINF-NAME      TO  SPA-GMN-G05-TNAME  (IDX-LST)
      *
               PERFORM 900-BTPARA-KEY9-FET-SEC
               MOVE    BTPARA-INFO-PARA
                                       TO  QTOANY-AREA
      *
           END-PERFORM
      *
           MOVE    "tbl_btpara"    TO  MCP-TABLE
           MOVE    "key9"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-BTPARA      =   ZERO )
               MOVE    1           TO  SPA-GMN-G05-PTLST-NEXT
           END-IF
      *
           MOVE    1               TO  SPA-GMN-G05-CUR
      *
           .
       3101-EDIT-PTLST-EXT.
           EXIT.
      *****************************************************************
      *    氏名検索より戻り時の処理
      *****************************************************************
       330-P97-SET-SEC             SECTION.
      *
           MOVE    LNK-KYOUTU      TO  WKSPA-KYOUTU
      *
           IF    ( WKSPA-PTID    =   ZERO )
             OR  ( WKSPA-PTID    =   SPA-NAI-G05-PTID )
               MOVE    2           TO  SPA-GMN-G05-CUR
           ELSE
               MOVE    WKSPA-PTNUM TO  G05-PTNUM
               PERFORM 4100-PTNUM-CHK-SEC
               MOVE    SPACE       TO  SPA-ERRCD
               MOVE    2           TO  SPA-GMN-G05-CUR
           END-IF
      *
           .
       330-P97-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM 210-BACK
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 220-CLEAR-SEC
           WHEN    "CLICKED"       ALSO    "B04"
               PERFORM 240-DEL-SELPT-SEC
           WHEN    "CLICKED"       ALSO    "B05"
               MOVE    3           TO  SPA-GMN-G05-CUR
               PERFORM 260-PREVPAGE-SEC
           WHEN    "CLICKED"       ALSO    "B06"
               PERFORM 260-PREVPAGE-SEC
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 270-NEXTPAGE-SEC
           WHEN    "CLICKED"       ALSO    "B09"
               MOVE    SPACE       TO      WRK-NAME
               PERFORM 490-KENSAKU-SEC
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 412-KAKUTEI-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
           WHEN    "ACTIVATE"      ALSO    "SELNUM"
               PERFORM 4100-SELNUM-CHK-SEC
           WHEN    "ACTIVATE"      ALSO    "PTNUM"
               PERFORM 4100-PTNUM-CHK-SEC
           WHEN    ANY             ALSO    "PTLST"
               PERFORM 4100-PTLST-CHK-SEC
           WHEN    OTHER
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC           SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-G05-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
             OR  ( FLG-END     NOT =   ZERO  )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SELNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-PTNUM-CHK-SEC
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-G05-CUR
      *
           MOVE    ZERO                TO  SPA-GMN-G05-PTLST-SEL
           INITIALIZE                      SPA-GMN-G05-INPUT-AREA
                                           SPA-NAI-G05-INPUT-AREA
      *
           MOVE    G05-SELNUM          TO  SPA-GMN-G05-SELNUM
      *
           PERFORM 4101-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( WRK-PTLST-SEL  >   ZERO )
                   MOVE    WRK-PTLST-SEL  TO  SPA-GMN-G05-PTLST-SEL
                   PERFORM 41011-EDIT-INPUT-AREA-SEC
               END-IF
           END-IF
      *
           .
      *
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       4100-PTNUM-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-G05-CUR
      *
           MOVE    SPACE               TO  SPA-GMN-G05-NAME
           MOVE    ZERO                TO  SPA-NAI-G05-PTID
      *
           MOVE    G05-PTNUM           TO  SPA-GMN-G05-PTNUM
      *
           PERFORM 4101-PTNUM-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
            AND  ( SPA-GMN-G05-PTNUM  NOT  =   SPACE )
            AND  ( FLG-END             =   ZERO  )
               IF    ( SPA-GMN-G05-PTLST-SEL  =   ZERO )
      *
                   PERFORM 900-BTPARA-KEY10-SEL-SEC
                   COMPUTE CNT-RENNUM
                       =   BTPARA-RENNUM  +   1
                   PERFORM 41001-BTPARA-COMMON-SEC
                   MOVE    CNT-RENNUM   TO BTPARA-RENNUM
                   MOVE    BTPARA-REC   TO MCPDATA-REC
                   MOVE    "tbl_btpara" TO MCP-TABLE
                   MOVE    "key"        TO MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF    ( MCP-RC          NOT =   ZERO )
                       MOVE    "4002"  TO  SPA-ERRCD
                   END-IF
      *
                   IF    ( SPA-ERRCD           =   SPACE )
                       INITIALIZE          SPA-GMN-G05-INPUT-AREA
                                           SPA-NAI-G05-INPUT-AREA
                       PERFORM 3101-EDIT-PTLST-SEC
                       MOVE    2       TO  SPA-GMN-G05-CUR
                   END-IF
      *
               ELSE
                   MOVE    SPA-GMN-G05-PTLST-SEL
                                       TO  IDX-LST
                   PERFORM 41001-BTPARA-COMMON-SEC
                   MOVE    SPA-NAI-G05-TRENNUM(IDX-LST)
                                       TO  BTPARA-RENNUM
                   MOVE    SPA-NAI-G05-TUPYMD(IDX-LST)
                                       TO  BTPARA-UPYMD
                   MOVE    SPA-NAI-G05-TUPHMS(IDX-LST)
                                       TO  BTPARA-UPHMS
                   MOVE    BTPARA-REC  TO  MCPDATA-REC
                   MOVE    "tbl_btpara" TO  MCP-TABLE
                   MOVE    "key"        TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       MOVE    "4003"  TO  SPA-ERRCD
                   END-IF
      *
                   IF    ( SPA-ERRCD           =   SPACE )
                       INITIALIZE          SPA-GMN-G05-INPUT-AREA
                                           SPA-NAI-G05-INPUT-AREA
                       PERFORM 3101-EDIT-PTLST-SEC
                   END-IF
      *
               END-IF
           END-IF
      *
           .
      *
       4100-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    バッチパラメタ共通項目編集処理
      *****************************************************************
       41001-BTPARA-COMMON-SEC        SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           INITIALIZE                  BTPARA-REC
           MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    CONST-BTPARA-SHELLID
                                       TO  BTPARA-SHELLID
           MOVE    SPA-OPID            TO  BTPARA-RUN-OPID
           MOVE    CONST-BTPARA-JOBID  TO  BTPARA-JOBID
           MOVE    SPACE               TO  BTPARA-SCRIPTID
           MOVE    SPA-TERMID          TO  BTPARA-TERMID
           MOVE    SMCNDATE-YMD        TO  BTPARA-CREYMD
                                           BTPARA-UPYMD
           MOVE    SMCNDATE-HMS        TO  BTPARA-UPHMS
      *
           MOVE    CONST-BTPARA-DSP-RENNUM
                                       TO  BTPARA-DSP-RENNUM
      *
           INITIALIZE                  QTOANY-AREA
           MOVE    SPA-NAI-G05-PTID    TO  QTOANY-PTID
           MOVE    SPA-GMN-G05-PTNUM   TO  QTOANY-PTNUM
           MOVE    QTOANY-AREA         TO  BTPARA-INFO-PARA
      *
           .
       41001-BTPARA-COMMON-EXT.
           EXIT.
      *****************************************************************
      *    個別患者リストチェック処理
      *****************************************************************
       4100-PTLST-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-G05-PTLST-SEL
                                           SPA-GMN-G05-SELNUM
           INITIALIZE                      SPA-GMN-G05-INPUT-AREA
                                           SPA-NAI-G05-INPUT-AREA
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST >   SPA-GMN-G05-PTLST-MAX )
                    OR   ( SPA-GMN-G05-PTLST-SEL  >   ZERO )
      *
               IF    ( G05-PTLST-SELECT (IDX-LST) =   "T")
                   MOVE    IDX-LST     TO  SPA-GMN-G05-PTLST-SEL
                   MOVE    SPA-GMN-G05-TNUM (IDX-LST)
                                       TO  SPA-GMN-G05-SELNUM
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-GMN-G05-PTLST-SEL  >   ZERO )
               PERFORM 41011-EDIT-INPUT-AREA-SEC
           END-IF
      *
           .
      *
       4100-PTLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4101-SELNUM-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-G05-SELNUM  >   ZERO )
               MOVE    ZERO            TO  WRK-PTLST-SEL
               PERFORM VARYING IDX-LST     FROM    1   BY  1
                       UNTIL ( IDX-LST     >   SPA-GMN-G05-PTLST-MAX )
                        OR   ( WRK-PTLST-SEL  >   ZERO )
      *
                   IF    ( SPA-GMN-G05-SELNUM
                                       =   SPA-GMN-G05-TNUM (IDX-LST))
                       MOVE    IDX-LST
                                       TO  WRK-PTLST-SEL
                   END-IF
      *
               END-PERFORM
      *
               IF    ( WRK-PTLST-SEL  =   ZERO )
                   MOVE    "0001"      TO  SPA-ERRCD
                   MOVE    1           TO  SPA-GMN-G05-CUR
               END-IF
           END-IF
      *
           .
      *
       4101-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       4101-PTNUM-CHK-SEC              SECTION.
      *
           IF    ( SPA-GMN-G05-PTNUM       =   SPACE )
            AND  ( SPA-GMN-G05-PTLST-SEL  =   ZERO )
               GO  TO  4101-PTNUM-CHK-EXT
           END-IF
      *
           INITIALIZE                  ORCSPTNUMAREA
           MOVE    SPA-GMN-G05-PTNUM   TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"     USING   ORCSPTNUMAREA
                                           SPA-AREA
           MOVE    SPTNUM-PTNUM    TO  SPA-GMN-G05-PTNUM
                                       WRK-PTNUM
           IF    ( SPTNUM-RC       NOT =   ZERO )
               IF    ( SPTNUM-RC       =   20 )
                   MOVE    SPA-GMN-G05-PTNUM   TO  WRK-NAME
                   PERFORM 490-KENSAKU-SEC
                   GO  TO  4101-PTNUM-CHK-EXT
               ELSE
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE    2           TO  SPA-GMN-G05-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    SPTNUM-PTID     TO  SPA-NAI-G05-PTID
      *
               MOVE    SPA-NAI-G05-PTID    TO  WRK-PTID
               PERFORM 900-PTINF-KEY-SEL-SEC
               MOVE    PTINF-NAME      TO  SPA-GMN-G05-NAME
      *
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               PERFORM 900-BTPARA-KEY8-SEL-SEC
               MOVE    BTPARA-INFO-PARA
                                       TO  QTOANY-AREA
      *
               MOVE    ZERO            TO  WRK-PTID
               PERFORM UNTIL ( FLG-BTPARA  NOT =   ZERO )
                        OR   ( WRK-PTID    NOT =   ZERO )
      *
                   IF    ( SPA-NAI-G05-PTID
                                       =   QTOANY-PTID )
                       MOVE    QTOANY-PTID
                                       TO  WRK-PTID
                   END-IF
                   PERFORM 900-BTPARA-KEY8-FET-SEC
                   MOVE    BTPARA-INFO-PARA
                                       TO  QTOANY-AREA
               END-PERFORM
      *
               MOVE    "tbl_btpara"    TO  MCP-TABLE
               MOVE    "key8"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               IF    ( WRK-PTID        =   ZERO )
                   CONTINUE
               ELSE
                       MOVE    "0004"  TO  SPA-ERRCD
                       MOVE    2       TO  SPA-GMN-G05-CUR
               END-IF
           END-IF
      *
           .
      *
       4101-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力領域編集処理
      *****************************************************************
       41011-EDIT-INPUT-AREA-SEC       SECTION.
      *
           INITIALIZE                      SPA-GMN-G05-INPUT-AREA
                                           SPA-NAI-G05-INPUT-AREA
      *
           MOVE    SPA-GMN-G05-PTLST-SEL  TO  IDX-LST
      *
           MOVE    SPA-GMN-G05-TNUM (IDX-LST)
                                       TO  SPA-GMN-G05-SELNUM
      *
           MOVE    SPA-GMN-G05-TPTNUM (IDX-LST)
                                       TO  SPA-GMN-G05-PTNUM
      *
           MOVE    SPA-GMN-G05-TNAME  (IDX-LST)
                                       TO  SPA-GMN-G05-NAME
      *
           MOVE    SPA-NAI-G05-TPTID  (IDX-LST)
                                       TO  SPA-NAI-G05-PTID
      *
           MOVE    2                   TO  SPA-GMN-G05-CUR
      *
           .
      *
       41011-EDIT-INPUT-AREA-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "G01"               TO  SPA-SAKIPG
           MOVE    "G05"               TO  SPA-MOTOPG
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
      *
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       220-CLEAR-SEC                   SECTION.
      *
           IF    ( SPA-GMN-G05-SELNUM  NOT =   ZERO )
            OR   ( SPA-GMN-G05-PTNUM   NOT =   SPACE )
               INITIALIZE              SPA-GMN-G05-INPUT-AREA
                                       SPA-NAI-G05-INPUT-AREA
                                       SPA-GMN-G05-PTLST-SEL
               MOVE    2               TO   SPA-GMN-G05-CUR
           ELSE
               PERFORM 3101-SPASET-SEC
      *
               MOVE    CONST-BTPARA-DSP-RENNUM
                                      TO   WRK-DSP-RENNUM
               PERFORM 900-BTPARA-DEL4-SEC
               IF    ( SPA-ERRCD       =   SPACE )
                  PERFORM 3101-EDIT-PTLST-SEC
                END-IF
           END-IF
      *
           .
      *
       220-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       240-DEL-SELPT-SEC               SECTION.
      *
           IF    ( SPA-GMN-G05-PTLST-SEL  =   ZERO )
               MOVE    "0005"      TO  SPA-ERRCD
           ELSE
      *
               PERFORM 41001-BTPARA-COMMON-SEC
               MOVE    SPA-NAI-G05-TRENNUM(SPA-GMN-G05-PTLST-SEL)
                                       TO  BTPARA-RENNUM
               MOVE    BTPARA-REC      TO  MCPDATA-REC
               MOVE    "tbl_btpara"    TO  MCP-TABLE
               MOVE    "del5"          TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
               IF    ( MCP-RC      =   ZERO )
                   INITIALIZE          SPA-GMN-G05-INPUT-AREA
                                       SPA-NAI-G05-INPUT-AREA
                   PERFORM 3101-EDIT-PTLST-SEC
                   MOVE    ZERO    TO  FLG-PTLST-ROW
               ELSE
                   MOVE    "4007"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
      *
       240-DEL-SELPT-EXT.
           EXIT.
      *****************************************************************
      *    前頁処理
      *****************************************************************
       260-PREVPAGE-SEC                SECTION.
      *
           IF    ( SPA-GMN-G05-PTLST-PAGE >   1 )
      *
               INITIALIZE              SPA-GMN-G05-INPUT-AREA
                                       SPA-NAI-G05-INPUT-AREA
      *
               COMPUTE SPA-GMN-G05-PTLST-PAGE
                   =   SPA-GMN-G05-PTLST-PAGE     -   1
      *
               PERFORM 3101-EDIT-PTLST-SEC
           END-IF
      *
           .
       260-PREVPAGE-EXT.
           EXIT.
      *****************************************************************
      *    次頁処理
      *****************************************************************
       270-NEXTPAGE-SEC                SECTION.
      *
           IF    ( SPA-GMN-G05-PTLST-NEXT  =   1 )
      *
               INITIALIZE              SPA-GMN-G05-INPUT-AREA
                                       SPA-NAI-G05-INPUT-AREA
      *
               COMPUTE SPA-GMN-G05-PTLST-PAGE
                   =   SPA-GMN-G05-PTLST-PAGE     +   1
      *
               PERFORM 3101-EDIT-PTLST-SEC
           END-IF
      *
           .
       270-NEXTPAGE-EXT.
           EXIT.
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       490-KENSAKU-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-G01FREE         TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "G05"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *
           MOVE    WRK-NAME            TO  SPA-NAME
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "P97"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-KENSAKU-EXT.
           EXIT.
      *****************************************************************
      *    確定
      *****************************************************************
       412-KAKUTEI-SEC                 SECTION.
      *
      *    入力チェック
           PERFORM 410-INPUT-CHK-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4900-KANRENCHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM  4121-KAKUTEI-SEC
           END-IF
      *
           .
      *
       412-KAKUTEI-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
           IF    ( SPA-GMN-G05-SELNUM  NOT =   ZERO )
            OR   ( SPA-GMN-G05-PTNUM   NOT =   SPACE )
               MOVE    "1001"      TO  SPA-ERRCD
               MOVE    2           TO  SPA-GMN-G05-CUR
           END-IF
      *
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *****************************************************************
      *    確定処理
      *****************************************************************
       4121-KAKUTEI-SEC                SECTION.
      *
           MOVE    2               TO  WRK-DSP-RENNUM
           PERFORM 900-BTPARA-DEL4-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    CONST-BTPARA-DSP-RENNUM
                                   TO  WRK-DSP-RENNUM
               MOVE    2           TO  WRK-SELLIMIT
               PERFORM 900-BTPARA-INS1-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    CONST-BTPARA-DSP-RENNUM
                                   TO  WRK-DSP-RENNUM
               PERFORM 900-BTPARA-DEL4-SEC
           END-IF
      *
           MOVE   "CHANGE"         TO  MCP-PUTTYPE
           PERFORM 210-BACK
      *
           .
       4121-KAKUTEI-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "G05    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE   G05-PTLST-ROW        TO  WRK-PTLST-ROW
      *
           MOVE   SPACE                TO  G05
           INITIALIZE                      G05
      *
           IF    ( FLG-PTLST-ROW       =   1 )
               MOVE    ZERO            TO  G05-PTLST-ROW
                                           G05-PTLST-ROWATTR
           ELSE
               MOVE    WRK-PTLST-ROW   TO  G05-PTLST-ROW
               MOVE    ZERO            TO  G05-PTLST-ROWATTR
           END-IF
      *
           MOVE    SPA-GMN-G05-SELNUM      TO  G05-SELNUM
           MOVE    SPA-GMN-G05-PTNUM       TO  G05-PTNUM
           MOVE    SPA-GMN-G05-NAME        TO  G05-NAME
           MOVE    SPA-GMN-G05-PTLST-MAX   TO  G05-PTLST-COUNT
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST >   CONST-PTLST-MAX  )
      *
               MOVE    SPA-GMN-G05-TNUM (IDX-LST)
                                       TO  WRK-ZZZZ9
               MOVE    WRK-ZZZZ9       TO  G05-PTLST-TNUM (IDX-LST)
      *
               IF    ( SPA-GMN-G05-TTESTPT(IDX-LST) =   "1" )
                   MOVE    "テ"        TO  G05-PTLST-TTESTPT(IDX-LST)
               END-IF
      *
               MOVE    SPA-GMN-G05-TPTNUM (IDX-LST)
                                       TO  G05-PTLST-TPTNUM(IDX-LST)
               MOVE    SPA-GMN-G05-TNAME (IDX-LST)
                                       TO  G05-PTLST-TNAME (IDX-LST)
      *
               IF    ( SPA-GMN-G05-PTLST-SEL
                                       =    IDX-LST )
                   MOVE    "T"         TO   G05-PTLST-SELECT (IDX-LST)
               ELSE
                   MOVE    "F"         TO   G05-PTLST-SELECT (IDX-LST)
               END-IF
      *
           END-PERFORM
      *
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
      *    カーソル位置
           PERFORM 5001-MAPCUR-SEC
           .
      *
       500-GMNHEN-EXT.
           EXIT.
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
           MOVE    WIDGET-NORMAL       TO  G05-B06-STATE
                                           G05-B07-STATE
      *
           IF    ( SPA-GMN-G05-PTLST-PAGE <=  1 )
               MOVE    WIDGET-INSENSITIVE
                                       TO  G05-B06-STATE
           END-IF
      *
           IF    ( SPA-GMN-G05-PTLST-NEXT  =  ZERO )
               MOVE    WIDGET-INSENSITIVE
                                       TO  G05-B07-STATE
           END-IF
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    カーソル位置処理
      *****************************************************************
       5001-MAPCUR-SEC                 SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE)
             AND ( SPA-GMN-G05-CUR         =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5011-INPUT-CUR-SKIP-SEC
      *
           EVALUATE    SPA-GMN-G05-CUR
           WHEN    001
               MOVE    "SELNUM"        TO  MCP-WIDGET
           WHEN    002
               MOVE    "PTNUM"         TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "SELNUM"
               MOVE    002             TO  SPA-GMN-G05-CUR
           WHEN    "PTNUM"
               MOVE    001             TO  SPA-GMN-G05-CUR
           WHEN    "TAISYO"
               MOVE    001             TO  SPA-GMN-G05-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5011-INPUT-CUR-SKIP-SEC         SECTION.
      *
           IF    ( SPA-GMN-G05-CUR         =   1 )
               IF    ( SPA-GMN-G05-PTLST-MAX  =   ZERO )
                   MOVE    2           TO  SPA-GMN-G05-CUR
               END-IF
           END-IF
      *
           .
      *
       5011-INPUT-CUR-SKIP-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC                  SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "選択番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "該当の患者が存在しません"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "該当患者は既に登録済みです"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "対象のデータが選択してありません"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "入力の途中です"
                                       TO  SPA-ERRMSG
           WHEN    "4001"  THRU    "4007"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  GERR
           MOVE    SPA-ERRCD           TO  GERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  GERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "G05"               TO  SPA-MOTOPG
           MOVE    "GERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "GERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索(KEY)
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM      TO  PTINF-HOSPNUM
           MOVE    WRK-PTID        TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
           ELSE
               MOVE    1           TO  FLG-PTINF
               INITIALIZE              PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者番号情報検索処理(KEY)
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM          TO  PTNUM-HOSPNUM
           MOVE    WRK-PTID            TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               INITIALIZE                  PTNUM-REC
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    バッチパラメタ検索処理
      *****************************************************************
       900-BTPARA-KEY9-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BTPARA
      *
           INITIALIZE                  BTPARA-REC
           MOVE    SPA-HOSPNUM     TO  BTPARA-HOSPNUM
           MOVE    CONST-BTPARA-SHELLID
                                   TO  BTPARA-SHELLID
           MOVE    CONST-BTPARA-JOBID
                                   TO  BTPARA-JOBID
           MOVE    CONST-BTPARA-DSP-RENNUM
                                   TO  BTPARA-DSP-RENNUM
      *
           COMPUTE BTPARA-SELLIMIT
               =   CONST-PTLST-MAX    +   1
           COMPUTE BTPARA-SELOFFSET
               = ( SPA-GMN-G05-PTLST-PAGE -   1 )
               *   CONST-PTLST-MAX
      *
           MOVE    BTPARA-REC      TO  MCPDATA-REC
           MOVE    "tbl_btpara"    TO  MCP-TABLE
           MOVE    "key9"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BTPARA-REC
           ELSE
               INITIALIZE              BTPARA-REC
               MOVE    1           TO  FLG-BTPARA
           END-IF
      *
           .
       900-BTPARA-KEY9-SEL-EXT.
           EXIT.
      *****************************************************************
      *    バッチパラメタFETCH処理
      *****************************************************************
       900-BTPARA-KEY9-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BTPARA
      *
           MOVE    "tbl_btpara"    TO  MCP-TABLE
           MOVE    "key9"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BTPARA-REC
           ELSE
               INITIALIZE              BTPARA-REC
               MOVE    1           TO  FLG-BTPARA
           END-IF
      *
           .
       900-BTPARA-KEY9-FET-EXT.
           EXIT.
      *****************************************************************
      *    バッチパラメタ検索処理
      *****************************************************************
       900-BTPARA-KEY8-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BTPARA
      *
           INITIALIZE                  BTPARA-REC
           MOVE    SPA-HOSPNUM     TO  BTPARA-HOSPNUM
           MOVE    CONST-BTPARA-SHELLID
                                   TO  BTPARA-SHELLID
           MOVE    CONST-BTPARA-JOBID
                                   TO  BTPARA-JOBID
           MOVE    CONST-BTPARA-DSP-RENNUM
                                   TO  BTPARA-DSP-RENNUM
      *
           MOVE    BTPARA-REC      TO  MCPDATA-REC
           MOVE    "tbl_btpara"    TO  MCP-TABLE
           MOVE    "key8"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BTPARA-REC
           ELSE
               INITIALIZE              BTPARA-REC
               MOVE    1           TO  FLG-BTPARA
           END-IF
      *
           .
       900-BTPARA-KEY8-SEL-EXT.
           EXIT.
      *****************************************************************
      *    バッチパラメタ検索処理
      *****************************************************************
       900-BTPARA-KEY10-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-BTPARA
      *
           INITIALIZE                  BTPARA-REC
           MOVE    SPA-HOSPNUM     TO  BTPARA-HOSPNUM
           MOVE    CONST-BTPARA-SHELLID
                                   TO  BTPARA-SHELLID
           MOVE    CONST-BTPARA-JOBID
                                   TO  BTPARA-JOBID
           MOVE    CONST-BTPARA-DSP-RENNUM
                                   TO  BTPARA-DSP-RENNUM
      *
           MOVE    BTPARA-REC      TO  MCPDATA-REC
           MOVE    "tbl_btpara"    TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BTPARA-REC
           ELSE
               INITIALIZE              BTPARA-REC
               MOVE    1           TO  FLG-BTPARA
           END-IF
      *
           MOVE    "tbl_btpara"    TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-BTPARA-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    バッチパラメタ削除処理
      *****************************************************************
       900-BTPARA-DEL4-SEC             SECTION.
      *
           INITIALIZE                      BTPARA-REC
           MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    CONST-BTPARA-SHELLID
                                       TO  BTPARA-SHELLID
           MOVE    CONST-BTPARA-JOBID  TO  BTPARA-JOBID
           MOVE    WRK-DSP-RENNUM      TO  BTPARA-DSP-RENNUM
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "del4"              TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "4005"      TO  SPA-ERRCD
           END-IF
      *
           .
       900-BTPARA-DEL4-EXT.
           EXIT.
      *****************************************************************
      *    バッチパラメタ追加処理
      *****************************************************************
       900-BTPARA-INS1-SEC             SECTION.
      *
           INITIALIZE                      BTPARA-REC
           MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    CONST-BTPARA-SHELLID
                                       TO  BTPARA-SHELLID
           MOVE    CONST-BTPARA-JOBID  TO  BTPARA-JOBID
           MOVE    WRK-DSP-RENNUM      TO  BTPARA-DSP-RENNUM
           MOVE    WRK-SELLIMIT        TO  BTPARA-SELLIMIT
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "ins1"              TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4006"  TO  SPA-ERRCD
           END-IF
      *
           .
       900-BTPARA-INS1-EXT.
           EXIT.
      *****************************************************************
      *    バッチパラメタFETCH処理
      *****************************************************************
       900-BTPARA-KEY8-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BTPARA
      *
           MOVE    "tbl_btpara"    TO  MCP-TABLE
           MOVE    "key8"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BTPARA-REC
           ELSE
               INITIALIZE              BTPARA-REC
               MOVE    1           TO  FLG-BTPARA
           END-IF
      *
           .
       900-BTPARA-KEY8-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
