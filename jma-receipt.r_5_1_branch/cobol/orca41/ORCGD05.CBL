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
       PROGRAM-ID.                 ORCGD05.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェック
      *  コンポーネント名  : 例外登録一覧画面
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07.07.20    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
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
      *    データチェック共通パラメタ
           COPY    "D01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "D04SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-CHKREIGAI               PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-PTNUM                   PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-HIZUKE-ERR              PIC 9(01).
           03  FLG-REIGAILST-ROW           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
           03  IDXE                        PIC 9(05).
           03  IDXL                        PIC 9(05).
      *
      *    作業領域
       01  WRK-AREA.
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-CMB-CD                  PIC X(10).
           03  WRK-CMB-ITM-G.
               07  WRK-CMB-ITM             PIC X(200).
           03  WRK-YMD                     PIC X(10).
           03  WRK-SYMD                    PIC X(08).
           03  WRK-HENYMDG                 PIC X(09).
           03  WRK-SRYYM-BF                PIC X(06).
           03  WRK-NYUGAIKBN-BF            PIC X(01).
           03  WRK-REIGAILST-SEL           PIC 9(05).
           03  WRK-DIDMSG                  PIC X(200).
           03  WRK-LISTNUM                 PIC ZZZZ9.
           03  WRK-CHKNAME                 PIC X(100).
           03  WRK-TEKNAME                 PIC X(100).
           03  WRK-TNS-SRYCD               PIC X(09).
           03  WRK-EDIT-COMPKBN            PIC X(01).
           03  WRK-EDIT-COMPKBN-MEI        PIC X(04).
           03  WRK-CHKNAIYO                PIC X(222).
           03  WRK-ERRMSG                  PIC X(200).
           03  WRK-LTRIM                   PIC X(500).
           03  WRK-LTRIM-Z                 REDEFINES WRK-LTRIM.
               05  WRK-LTRIM-ZX99          PIC Z(9)9.
               05  WRK-LTRIM-FL            PIC X(490).
           03  WRK-LTRIM-TMP               PIC X(500).
           03  WRK-KANACHK-STR             PIC X(500).
      *
       01  KEY-AREA.
           03  KEY-PTID                    PIC 9(10).
           03  KEY-PTNUM                   PIC X(20).
      *
       01  WRK-ROW-AREA.
           03  WRK-REIGAILST-ROW           PIC S9(9).
      *
       01  CONST-AREA.
           03  CONST-REIGAILST-MAX         PIC 9(05)   VALUE 100.
      *
      *    データチェック確認項目
           COPY    "CPDTCHKKOMOKU.INC".
      *    データチェックエラーメッセージ
           COPY    "CPDTCHKERRMES.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPCHKREIGAI.INC".
      *
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
           COPY    "CPTENSU.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
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
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA41SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
                                               MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA.
      *****************************************************************
      *    主    処    理
      *****************************************************************
      *
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA.
           MOVE    SPA-WORK            TO  SPA-D01KYOUTU
           MOVE    SPA-FREE            TO  SPA-D04FREE
      *
           MOVE    SPACE           TO  SPA-ERRCD.
           MOVE    ZERO            TO  FLG-END.
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF  FLG-END   =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF.
      *
           MOVE    SPA-D01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-D04FREE         TO  SPA-FREE
           .
      *
       000-PROC-SEC-EXT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                   SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "DERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF 
      *
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
           MOVE    "D05"               TO  MCP-WINDOW
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
           EVALUATE    SPA-MOTOPG
           WHEN    "DID1"
               PERFORM 330-DID1-SET-SEC
           WHEN    OTHER
               PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-D05
      *
      *    コンボボックス編集処理
           PERFORM 3101-COMBO-EDIT-SEC
      *
           MOVE    SPA-D04-SRYYM       TO  WRK-YMD
           PERFORM 800-YM-CHK-SEC
           IF    ( FLG-HIZUKE-ERR      =   1 )
            OR   ( SPA-D04-SRYYM       =   SPACE )
               MOVE    SPA-SYSYMDWH(1:6)   TO  SPA-GMN-D05-SRYYM
               MOVE    SPA-SYSYMD  (1:6)   TO  SPA-NAI-D05-SRYYM
           ELSE
               MOVE    WRK-HENYMDG (1:6)   TO  SPA-GMN-D05-SRYYM
               MOVE    WRK-SYMD    (1:6)   TO  SPA-NAI-D05-SRYYM
           END-IF
      *
           MOVE    SPA-D04-NYUGAIKBN   TO  WRK-CMB-CD
           PERFORM 4200-NYUGAIKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-D05-NYUGAIKBNLST (2)
                                   TO  SPA-GMN-D05-NYUGAIKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-D05-NYUGAIKBN-G
           END-IF
      *
           MOVE    1               TO  SPA-GMN-D05-REIGAILST-PAGE
           PERFORM 3101-REIGAILST-SEC
      *
           MOVE    1               TO  SPA-GMN-D05-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コンボボックス編集処理
      *****************************************************************
       3101-COMBO-EDIT-SEC             SECTION.
      *
           PERFORM 3101-NYUGAIKBNLST-SEC
      *
           .
       3101-COMBO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入外区分コンボ編集処理
      *****************************************************************
       3101-NYUGAIKBNLST-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-D05-NYUGAIKBNLST-G
           MOVE    "1"             TO  SPA-GMN-D05-NYUGAIKBNL (1)
           MOVE    "入院"          TO  SPA-GMN-D05-NYUGAIKBNMEIL (1)
           MOVE    "2"             TO  SPA-GMN-D05-NYUGAIKBNL (2)
           MOVE    "外来"          TO  SPA-GMN-D05-NYUGAIKBNMEIL (2)
           MOVE    2               TO  SPA-GMN-D05-NYUGAIKBN-MAX
      *
           .
       3101-NYUGAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    例外一覧リスト編集処理
      *****************************************************************
       3101-REIGAILST-SEC                  SECTION.
      *
           MOVE    ZERO            TO  IDXL
      *
           INITIALIZE                      SPA-GMN-D05-REIGAILST-AREA
                                           SPA-NAI-D05-REIGAILST-AREA
      *
           PERFORM 900-CHKREIGAI-KEY3-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKREIGAI   NOT  =  ZERO )
                    OR   ( SPA-GMN-D05-REIGAILST-MAX
                                          >=  CONST-REIGAILST-MAX )
      *
               COMPUTE IDXL  =  IDXL     +   1
      *
               COMPUTE SPA-GMN-D05-TNUM (IDXL)
                   = ( SPA-GMN-D05-REIGAILST-PAGE - 1 )
                   *   CONST-REIGAILST-MAX
                   +   IDXL
      *
               MOVE    IDXL  TO SPA-GMN-D05-REIGAILST-MAX
      *
               PERFORM 900-PTNUM-KEY-SEL-SEC
               PERFORM 900-PTINF-KEY-SEL-SEC
      *
               MOVE    PTNUM-PTNUM     TO  SPA-GMN-D05-TPTNUM  (IDXL)
               MOVE    PTINF-NAME      TO  SPA-GMN-D05-TNAME   (IDXL)
      *
               EVALUATE    CHKREIGAI-CHKNUM
               WHEN    "99"
                   MOVE    "その他"    TO  SPA-GMN-D05-TCHKNUMNAME(IDXL)
               WHEN    "A1"
                   MOVE    "［薬剤と病名］未登録"
                                       TO  SPA-GMN-D05-TCHKNUMNAME(IDXL)
               WHEN    OTHER
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   DTCHKKOMOKU-MAX )
                            OR   ( SPA-GMN-D05-TCHKNUMNAME(IDXL)
                                       NOT =   SPACE )
      *
                       IF    ( DTCHKKOMOKU-CHKNUM (IDX0)
                                       =   CHKREIGAI-CHKNUM )
                           MOVE    DTCHKKOMOKU-CHKKOMOKU (IDX0)
                                       TO  SPA-GMN-D05-TCHKNUMNAME(IDXL)
                       END-IF
      *
                   END-PERFORM
               END-EVALUATE
      *
               MOVE    SPACE           TO  WRK-ERRMSG
               MOVE    SPACE           TO  WRK-CHKNAIYO
      *
               IF    ( CHKREIGAI-CHKKBN    =   SPACE )
                   EVALUATE    TRUE
                   WHEN  ( CHKREIGAI-CHKNUM    =   "20" )
                    AND  ( CHKREIGAI-ERRCD     =   "200001" )
                       MOVE    CHKREIGAI-SRYCD
                                           TO  WRK-TNS-SRYCD
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       STRING "［"                 DELIMITED BY SIZE
                            CHKREIGAI-TANINAME     DELIMITED BY SPACE
                           "］："                  DELIMITED BY SIZE
                           CHKREIGAI-SRYCD         DELIMITED BY SIZE
                           " "                     DELIMITED BY SIZE
                           TNS-NAME                DELIMITED BY SPACE
                       INTO    WRK-CHKNAIYO
                       END-STRING
                   WHEN  ( CHKREIGAI-CHKNUM    =   "21" )
                       MOVE    CHKREIGAI-SRYCD
                                           TO  WRK-TNS-SRYCD
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       STRING
                           CHKREIGAI-SRYCD         DELIMITED BY SIZE
                           " "                     DELIMITED BY SIZE
                           TNS-NAME                DELIMITED BY SPACE
                       INTO    WRK-CHKNAIYO
                       END-STRING
                   WHEN    OTHER
                       PERFORM VARYING IDXE    FROM    1   BY  1
                               UNTIL ( IDXE    >   ERRMES-MAX )
                                OR   ( SPA-GMN-D05-TCHKNAIYO (IDXL)
                                           NOT =   SPACE )
      *
                           IF    ( CHKREIGAI-ERRCD =   ERRMES-CD (IDXE))
                               MOVE    ERRMES-MESSAGE (IDXE)
                                       TO  WRK-ERRMSG
                           END-IF
      *
                       END-PERFORM
                       IF    ( WRK-ERRMSG  =   SPACE )
                           MOVE    CHKREIGAI-ERRMSG
                                       TO  WRK-ERRMSG
                       END-IF
      *
                       STRING  CHKREIGAI-ERRCD DELIMITED BY SIZE
                               " "             DELIMITED BY SIZE
                               WRK-ERRMSG      DELIMITED BY SPACE
                       INTO    WRK-CHKNAIYO
                       END-STRING
                   END-EVALUATE
      *
               ELSE
                   EVALUATE    CHKREIGAI-CHKKBN
                   WHEN    "8"
                   WHEN    "9"
                       MOVE    CHKREIGAI-BYOMEI
                                           TO  WRK-CHKNAIYO
                   WHEN    "4"
                   WHEN    "5"
                   WHEN    "A"
                   WHEN    "B"
                       MOVE    CHKREIGAI-SRYCD
                                           TO  WRK-TNS-SRYCD
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       MOVE    TNS-NAME    TO  WRK-CHKNAME
      *
                       MOVE    CHKREIGAI-TEKCD
                                           TO  WRK-TNS-SRYCD
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       MOVE    TNS-NAME    TO  WRK-TEKNAME
      *
                       STRING  CHKREIGAI-SRYCD DELIMITED BY SIZE
                               " "             DELIMITED BY SIZE
                               WRK-CHKNAME     DELIMITED BY SPACE
                               "／"            DELIMITED BY SIZE
                               CHKREIGAI-TEKCD DELIMITED BY SIZE
                               " "             DELIMITED BY SIZE
                               WRK-TEKNAME     DELIMITED BY SPACE
                       INTO    WRK-CHKNAIYO
                       END-STRING
                   WHEN    "6"
                       MOVE    CHKREIGAI-SRYCD
                                           TO  WRK-TNS-SRYCD
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       MOVE    TNS-NAME    TO  WRK-CHKNAME
      *
                       MOVE    CHKREIGAI-TEKCOMPKBN
                                               TO  WRK-EDIT-COMPKBN
                       PERFORM 800-EDIT-COMPKBN-MEI-SEC
      *
                       STRING  CHKREIGAI-SRYCD DELIMITED BY SIZE
                               " "             DELIMITED BY SIZE
                               WRK-CHKNAME     DELIMITED BY SPACE
                               "／"            DELIMITED BY SIZE
                               CHKREIGAI-TEKBYOMEI
                                               DELIMITED BY SPACE
                               "［"            DELIMITED BY SIZE
                               WRK-EDIT-COMPKBN-MEI
                                               DELIMITED BY SPACE
                               "］"            DELIMITED BY SIZE
                       INTO    WRK-CHKNAIYO
                       END-STRING
                   WHEN    OTHER
                       MOVE    CHKREIGAI-SRYCD
                                           TO  WRK-TNS-SRYCD
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       MOVE    TNS-NAME    TO  WRK-CHKNAME
      *
                       STRING  CHKREIGAI-SRYCD DELIMITED BY SIZE
                               " "             DELIMITED BY SIZE
                               WRK-CHKNAME     DELIMITED BY SPACE
                       INTO    WRK-CHKNAIYO
                       END-STRING
                   END-EVALUATE
               END-IF
      *
               MOVE    WRK-CHKNAIYO    TO  SPA-GMN-D05-TCHKNAIYO  (IDXL)
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    SPA-GMN-D05-TCHKNAIYO  (IDXL)
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT
                                       TO  SPA-GMN-D05-TCHKNAIYO  (IDXL)
      *
               IF    ( CHKREIGAI-EDYUKYMD  =   "99999999" )
                   MOVE    1               TO  SPA-GMN-D05-TKIKAN
                                                               (IDXL)
               END-IF
      *
               MOVE    CHKREIGAI-PTID      TO  SPA-NAI-D05-TPTID
                                                               (IDXL)
               MOVE    CHKREIGAI-RENNUM    TO  SPA-NAI-D05-TRENNUM
                                                               (IDXL)
      *
               PERFORM 900-CHKREIGAI-KEY3-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chkreigai"         TO  MCP-TABLE
           MOVE    "key3"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-CHKREIGAI       =   ZERO )
               MOVE    1               TO  SPA-GMN-D05-REIGAILST-NEXT
           END-IF
      *
           .
       3101-REIGAILST-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＤＩＤ１）ＯＫ処理
      *****************************************************************
       330-DID1-SET-SEC            SECTION.
      *
           IF    ( SPA-DID1-FLG    =   "OK" )
               EVALUATE    SPA-DIDCD
               WHEN    "2001"
                   PERFORM 2301-DEL-SEC
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE               TO  SPA-DID1-FLG
           MOVE    SPACE               TO  SPA-DIDCD
      *
           .
       330-DID1-SET-EXT.
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
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 210-BACK
           WHEN    "CLICKED"       ALSO    "B03"
               PERFORM 230-DEL-SEC
           WHEN    "CLICKED"       ALSO    "B06"
               PERFORM 260-ZENGETU-SEC
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 270-JIGETU-SEC
           WHEN    "CLICKED"       ALSO    "B06S"
               PERFORM 260-PREVPAGE-SEC
           WHEN    "CLICKED"       ALSO    "B07S"
               PERFORM 270-NEXTPAGE-SEC
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
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
           WHEN    ANY                 ALSO    "REIGAILST"
               PERFORM 4100-REIGAILST-CHK-SEC
           WHEN    "ACTIVATE"          ALSO    "SELNUM"
               PERFORM 4100-SELNUM-CHK-SEC
           WHEN    "ACTIVATE"          ALSO    "SRYYM"
               PERFORM 4100-SRYYM-CHK-SEC
           WHEN    "ACTIVATE"          ALSO    "NYUGAIKBN"
               PERFORM 4100-NYUGAIKBN-CHK-SEC
      *    モード変更
           WHEN    OTHER
      *
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
      *
           END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           MOVE    ZERO        TO      SPA-GMN-D05-CUR
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
           PERFORM 4101-SELNUM-CHK-SEC
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療年月入力チェック
      *****************************************************************
       4100-SRYYM-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-D05-CUR
      *
           MOVE    SPA-NAI-D05-SRYYM   TO  WRK-SRYYM-BF
      *
           IF    ( D05-SRYYM           =   SPACE )
               CONTINUE
           ELSE
               MOVE    D05-SRYYM       TO  WRK-YMD
               PERFORM 800-YM-CHK-SEC
               IF    ( FLG-HIZUKE-ERR      =   ZERO )
                   MOVE    WRK-HENYMDG  (1:6)
                                           TO  SPA-GMN-D05-SRYYM
                   MOVE    WRK-SYMD  (1:6) TO  SPA-NAI-D05-SRYYM
               ELSE
                   MOVE    "0001"      TO  SPA-ERRCD
                   MOVE    1           TO  SPA-GMN-D05-CUR
               END-IF
               IF    ( WRK-SRYYM-BF    =   SPA-NAI-D05-SRYYM )
                   CONTINUE
               ELSE
                   MOVE    1           TO  SPA-GMN-D05-REIGAILST-PAGE
                   PERFORM 3101-REIGAILST-SEC
               END-IF
           END-IF
      *
           .
      *
       4100-SRYYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入外区分入力チェック
      *****************************************************************
       4100-NYUGAIKBN-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-D05-CUR
      *
           MOVE    SPA-GMN-D05-NYUGAIKBN
                                       TO  WRK-NYUGAIKBN-BF
      *
           IF    ( D05-NYUGAIKBN       =   SPACE )
               CONTINUE
           ELSE
               MOVE    D05-NYUGAIKBN (1:1) TO  WRK-CMB-CD
               PERFORM 4200-NYUGAIKBNLST-CHK-SEC
               IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE    2           TO  SPA-GMN-D05-CUR
               ELSE
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-D05-NYUGAIKBN-G
               END-IF
               IF    ( WRK-NYUGAIKBN-BF    =   SPA-GMN-D05-NYUGAIKBN )
                   CONTINUE
               ELSE
                   MOVE    1           TO  SPA-GMN-D05-REIGAILST-PAGE
                   PERFORM 3101-REIGAILST-SEC
               END-IF
           END-IF
      *
           .
      *
       4100-NYUGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-D05-CUR
      *
           MOVE    ZERO                TO  SPA-GMN-D05-REIGAILST-SEL
      *
           MOVE    D05-SELNUM          TO  SPA-GMN-D05-REIGAILST-SELNUM
      *
           PERFORM 4101-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( WRK-REIGAILST-SEL  >   ZERO )
                   MOVE    WRK-REIGAILST-SEL 
                                       TO  SPA-GMN-D05-REIGAILST-SEL
               END-IF
           END-IF
      *
           .
      *
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4101-SELNUM-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-D05-REIGAILST-SELNUM    >   ZERO )
               MOVE    ZERO            TO  WRK-REIGAILST-SEL
               PERFORM VARYING IDXL     FROM    1   BY  1
                       UNTIL ( IDXL >   SPA-GMN-D05-REIGAILST-MAX )
                        OR   ( WRK-REIGAILST-SEL  >   ZERO )
      *
                   IF    ( SPA-GMN-D05-REIGAILST-SELNUM
                                       =   SPA-GMN-D05-TNUM (IDXL))
                       MOVE    IDXL
                                       TO  WRK-REIGAILST-SEL
                   END-IF
      *
               END-PERFORM
      *
               IF    ( WRK-REIGAILST-SEL  =   ZERO )
                   MOVE    "0003"      TO  SPA-ERRCD
                   MOVE    3           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       4101-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    例外一覧リストチェック処理
      *****************************************************************
       4100-REIGAILST-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-D05-REIGAILST-SEL
      *
           PERFORM VARYING IDXL FROM    1  BY  1
                   UNTIL ( IDXL >   SPA-GMN-D05-REIGAILST-MAX )
                    OR   ( SPA-GMN-D05-REIGAILST-SEL  >   ZERO )
      *
               IF    ( D05-REIGAILST-SELECT (IDXL)    =   "T")
                   MOVE    IDXL        TO  SPA-GMN-D05-REIGAILST-SEL
                   MOVE    SPA-GMN-D05-TNUM (IDXL)
                                       TO  D05-SELNUM
                   MOVE    3           TO  SPA-GMN-D05-CUR
                   PERFORM 4100-SELNUM-CHK-SEC
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4100-REIGAILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       4900-KANREN-CHK-SEC             SECTION.
      *
      *
           .
       4900-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "D04"               TO  SPA-SAKIPG
           MOVE    "D05"               TO  SPA-MOTOPG
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       230-DEL-SEC                     SECTION.
      *
           IF    ( SPA-GMN-D05-REIGAILST-SEL   >   ZERO )
               MOVE    "2001"          TO  SPA-DIDCD
           END-IF
      *
           .
      *
       230-DEL-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       2301-DEL-SEC                     SECTION.
      *
           MOVE    SPA-GMN-D05-REIGAILST-SEL   TO  IDXL
           INITIALIZE                              CHKREIGAI-REC
           MOVE    SPA-HOSPNUM                 TO  CHKREIGAI-HOSPNUM
           MOVE    SPA-NAI-D05-TPTID (IDXL) TO  CHKREIGAI-PTID
           MOVE    SPA-NAI-D05-TRENNUM (IDXL)
                                               TO  CHKREIGAI-RENNUM
           MOVE    CHKREIGAI-REC               TO  MCPDATA-REC
           MOVE    "tbl_chkreigai"             TO  MCP-TABLE
           MOVE    "key"                       TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 3101-REIGAILST-SEC
           ELSE
               MOVE    "4001"                  TO  SPA-ERRCD
           END-IF
      *
           .
      *
       2301-DEL-EXT.
           EXIT.
      **************************************************************
      *    前月表示処理
      **************************************************************
       260-ZENGETU-SEC                 SECTION.
      *
      *    前月の月算出
           MOVE    "61"            TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-D05-SRYYM   TO  LNK-DAY6-KIJUN (1:6)
           MOVE    "01"            TO  LNK-DAY6-KIJUN (7:2)
           MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
           MOVE     -1             TO  LNK-DAY6-ZOGEN 
           CALL    "ORCSDAY"       USING
                                       STS-AREA-DAY
                                       LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN (1:6)
                                   TO  D05-SRYYM
           PERFORM 4100-SRYYM-CHK-SEC
      *
           .
       260-ZENGETU-EXT.
           EXIT.
      **************************************************************
      *    次月表示処理
      **************************************************************
       270-JIGETU-SEC                 SECTION.
      *
      *    次月の月算出
           MOVE    "61"            TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-D05-SRYYM   TO  LNK-DAY6-KIJUN (1:6)
           MOVE    "01"            TO  LNK-DAY6-KIJUN (7:2)
           MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
           MOVE     1              TO  LNK-DAY6-ZOGEN 
           CALL    "ORCSDAY"       USING
                                       STS-AREA-DAY
                                       LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN (1:6)
                                   TO  D05-SRYYM
           PERFORM 4100-SRYYM-CHK-SEC
      *
           .
       270-JIGETU-EXT.
           EXIT.
      *****************************************************************
      *    前頁処理
      *****************************************************************
       260-PREVPAGE-SEC                SECTION.
      *
           IF    ( SPA-GMN-D05-REIGAILST-PAGE >   1 )
      *
               INITIALIZE              SPA-GMN-D05-REIGAILST-AREA
                                       SPA-NAI-D05-REIGAILST-AREA
      *
               COMPUTE SPA-GMN-D05-REIGAILST-PAGE
                   =   SPA-GMN-D05-REIGAILST-PAGE     -   1
      *
               PERFORM 3101-REIGAILST-SEC
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
           IF    ( SPA-GMN-D05-REIGAILST-NEXT  =   1 )
      *
               INITIALIZE              SPA-GMN-D05-REIGAILST-AREA
                                       SPA-NAI-D05-REIGAILST-AREA
      *
               COMPUTE SPA-GMN-D05-REIGAILST-PAGE
                   =   SPA-GMN-D05-REIGAILST-PAGE     +   1
      *
               PERFORM 3101-REIGAILST-SEC
           END-IF
      *
           .
       270-NEXTPAGE-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       220-TOUROKU-SEC                 SECTION.
      *
      *
           .
      *
       220-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    入外区分コンボチェック処理
      *****************************************************************
       4200-NYUGAIKBNLST-CHK-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-D05-NYUGAIKBN-MAX )
                OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
                IF   ( WRK-CMB-CD  =   SPA-GMN-D05-NYUGAIKBNL (IDX-CMB))
                   MOVE    SPA-GMN-D05-NYUGAIKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                END-IF
      *
           END-PERFORM
      *
           .
       4200-NYUGAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-DIDCD       NOT =   SPACE )
               PERFORM 520-DID1SET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "D05    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE   D05-REIGAILST-ROW    TO  WRK-REIGAILST-ROW
      *
           MOVE   SPACE            TO  D05
           INITIALIZE                  D05
      *
           IF    ( FLG-REIGAILST-ROW      =   1 )
               MOVE    ZERO            TO  D05-REIGAILST-ROW
                                           D05-REIGAILST-ROWATTR
           ELSE
               MOVE    WRK-REIGAILST-ROW   TO  D05-REIGAILST-ROW
               MOVE    ZERO                TO  D05-REIGAILST-ROWATTR
           END-IF
      *
           MOVE    SPA-GMN-D05-REIGAILST-SELNUM
                                           TO  D05-SELNUM
           MOVE    SPA-GMN-D05-SRYYM       TO  D05-SRYYM
           MOVE    SPA-GMN-D05-NYUGAIKBN-G TO  D05-NYUGAIKBN
           MOVE    SPA-GMN-D05-NYUGAIKBN-MAX
                                           TO  D05-NYUGAIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   D05-NYUGAIKBN-COUNT )
               MOVE    SPA-GMN-D05-NYUGAIKBNLST (IDX-CMB)
                                       TO  D05-NYUGAIKBN-ITEM  (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-D05-REIGAILST-MAX   TO  D05-REIGAILST-COUNT
           PERFORM VARYING IDXL FROM    1   BY  1
                   UNTIL ( IDXL >   SPA-GMN-D05-REIGAILST-MAX )
               MOVE    SPA-GMN-D05-TNUM   (IDXL)
                                       TO  WRK-LISTNUM
               MOVE    WRK-LISTNUM     TO  D05-TNUM   (IDXL)
      *
               MOVE    SPA-GMN-D05-TPTNUM (IDXL)
                                       TO  D05-TPTNUM (IDXL)
               MOVE    SPA-GMN-D05-TNAME  (IDXL)
                                       TO  D05-TNAME  (IDXL)
      *
               MOVE    SPA-GMN-D05-TCHKNUMNAME (IDXL)
                                       TO  D05-TCHKNUMNAME (IDXL)
               MOVE    SPA-GMN-D05-TCHKNAIYO (IDXL)
                                       TO  D05-TCHKNAIYO (IDXL)
               IF    ( SPA-GMN-D05-TKIKAN (IDXL)    =   ZERO )
                   MOVE    "当月のみ"  TO  D05-TKIKAN    (IDXL)
               ELSE
                   MOVE    "指定なし"  TO  D05-TKIKAN    (IDXL)
               END-IF
      *
               IF    ( IDXL     =   SPA-GMN-D05-REIGAILST-SEL )
                   MOVE    "T"         TO  D05-REIGAILST-SELECT(IDXL)
               ELSE
                   MOVE    "F"         TO  D05-REIGAILST-SELECT(IDXL)
               END-IF
           END-PERFORM
      *
      *    項目状態編集
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
           MOVE    WIDGET-NORMAL       TO  D05-B06S-STATE
                                           D05-B07S-STATE
      *
           IF    ( SPA-GMN-D05-REIGAILST-PAGE      <=  1 )
               MOVE    WIDGET-INSENSITIVE  TO  D05-B06S-STATE
           END-IF
      *
           IF    ( SPA-GMN-D05-REIGAILST-NEXT       =  ZERO )
               MOVE    WIDGET-INSENSITIVE  TO  D05-B07S-STATE
           END-IF
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC              SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-D05-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-D05-CUR
           WHEN    001
               MOVE    "SRYYM"         TO  MCP-WIDGET
           WHEN    002
               MOVE    "NYUGAIKBN"     TO  MCP-WIDGET
           WHEN    003
               MOVE    "SELNUM"        TO  MCP-WIDGET
           END-EVALUATE
      *
           .
      *
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "SRYYM"
               MOVE    002             TO  SPA-GMN-D05-CUR
           WHEN    "NYUGAIKBN"
               MOVE    003             TO  SPA-GMN-D05-CUR
           WHEN    "SELNUM"
               MOVE    003             TO  SPA-GMN-D05-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "診療年月入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "入外区分入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "選択番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "4001"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  DERR
           MOVE    SPA-ERRCD            TO  DERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  DERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "D05"                TO  SPA-MOTOPG
           MOVE    "DERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "DERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-DID1SET-SEC                 SECTION.
      *
           MOVE    SPACE               TO  WRK-DIDMSG
      *
           EVALUATE    SPA-DIDCD
           WHEN    "2001"
               MOVE    "例外設定を解除します。よろしいですか"
                                       TO  WRK-DIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  DID1
           INITIALIZE                      DID1
           MOVE    SPA-DIDCD           TO  DID1-ID1CODE
           MOVE    WRK-DIDMSG          TO  DID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "D05"               TO  SPA-MOTOPG
           MOVE    "DID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "DID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-DID1SET-EXT.
           EXIT.
      *****************************************************************
      *    年月チェック・編集処理
      *****************************************************************
       800-YM-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-HIZUKE-ERR
      *
           MOVE    SPACE               TO  WRK-HENYMDG
                                           WRK-SYMD
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    "1"                 TO  SGDAY-INTYPE
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF    ( SGDAY-RC            =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
               MOVE    1               TO  FLG-HIZUKE-ERR
           END-IF
      *
           .
       800-YM-CHK-EXT.
           EXIT.
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
      *    一致条件名称編集処理
      *****************************************************************
       800-EDIT-COMPKBN-MEI-SEC        SECTION.
      *
           MOVE    SPACE           TO  WRK-EDIT-COMPKBN-MEI
      *
           EVALUATE    WRK-EDIT-COMPKBN
           WHEN    "1"
               MOVE    "部分"      TO  WRK-EDIT-COMPKBN-MEI
           WHEN    "2"
               MOVE    "完全"      TO  WRK-EDIT-COMPKBN-MEI
           WHEN    "3"
               MOVE    "前方"      TO  WRK-EDIT-COMPKBN-MEI
           WHEN    "4"
               MOVE    "後方"      TO  WRK-EDIT-COMPKBN-MEI
           END-EVALUATE
      *
           .
       800-EDIT-COMPKBN-MEI-EXT.
            EXIT.
      *****************************************************************
      *    文字列左詰処理
      *****************************************************************
       800-LTRIM-SEC               SECTION.
      *
           MOVE    SPACE           TO  WRK-LTRIM-FL
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >       500 )
      *
               IF    ( WRK-LTRIM (IDX1 : 1)
                                       NOT =   SPACE )
                   MOVE    WRK-LTRIM (IDX1:)
                                           TO  WRK-LTRIM-TMP
                   MOVE    WRK-LTRIM-TMP   TO  WRK-LTRIM
                   MOVE    500             TO  IDX1
               END-IF
      *
           END-PERFORM
      *
           .
       800-LTRIM-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-SEC                 SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE   "2"              TO  KANACHK-SYORI
           MOVE  500               TO  KANACHK-MAX-CNT
           MOVE  WRK-KANACHK-STR   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-STR
      *
           .
       800-ZENKAKU-EXT.
           EXIT.
      *****************************************************************
      *    チェック例外マスタ検索処理
      *****************************************************************
       900-CHKREIGAI-KEY3-SEL-SEC      SECTION.
      *
           MOVE    ZERO                    TO  FLG-CHKREIGAI
      *
           INITIALIZE                          CHKREIGAI-REC
           MOVE    SPA-HOSPNUM             TO  CHKREIGAI-HOSPNUM
           MOVE    SPA-GMN-D05-NYUGAIKBN   TO  CHKREIGAI-NYUGAIKBN
           MOVE    SPA-NAI-D05-SRYYM       TO  CHKREIGAI-STYUKYMD
           MOVE    "01"                    TO  CHKREIGAI-STYUKYMD(7:2)
           MOVE    CHKREIGAI-STYUKYMD      TO  CHKREIGAI-EDYUKYMD
           COMPUTE CHKREIGAI-SELLIMIT  =   CONST-REIGAILST-MAX +   1
           COMPUTE CHKREIGAI-SELOFFSET
               = ( SPA-GMN-D05-REIGAILST-PAGE  -   1 )
               *   CONST-REIGAILST-MAX
           MOVE    CHKREIGAI-REC           TO  MCPDATA-REC
           MOVE    "tbl_chkreigai"         TO  MCP-TABLE
           MOVE    "key3"                  TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHKREIGAI-REC
           ELSE
               INITIALIZE              CHKREIGAI-REC
               MOVE    1           TO  FLG-CHKREIGAI
           END-IF
      *
           .
       900-CHKREIGAI-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェック例外マスタFETCH処理
      *****************************************************************
       900-CHKREIGAI-KEY3-FET-SEC      SECTION.
      *
           MOVE    "tbl_chkreigai"         TO  MCP-TABLE
           MOVE    "key3"                  TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHKREIGAI-REC
           ELSE
               INITIALIZE              CHKREIGAI-REC
               MOVE    1           TO  FLG-CHKREIGAI
           END-IF
      *
           .
       900-CHKREIGAI-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索(KEY)
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    CHKREIGAI-PTID  TO  PTINF-PTID
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
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    CHKREIGAI-PTID      TO  PTNUM-PTID
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
      *    点数マスター検索（KEY）
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-TNS-SRYCD   TO  TNS-SRYCD
           MOVE    SPA-NAI-D05-SRYYM   TO  TNS-YUKOSTYMD
           MOVE    "01"                TO  TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD       TO  TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ追加処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

