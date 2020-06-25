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
       PROGRAM-ID.             ORAPI001R4V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : カルテ１号紙（外来）印刷
      *  管理者            :
      *  作成日付    作業者        記述
      *  18/07/25    NACL−太田　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-ORCSNUM             PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SNUM-INX            PIC X(26).
           03  WRK-SNUM-OUTNUM         PIC 9(12).
      *
       01  CONST-AREA.
           03  CONST-TRUE              PIC X(05)   VALUE "True".
           03  CONST-FALSE             PIC X(05)   VALUE "False".
      *
           COPY    "CPRPKEY.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    オンライン帳票ＩＤ連絡
           COPY    "CPORCSRPLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    カルテ１号紙（外来）
           COPY    "CPORCHC01.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPKARTE1V2REQ.INC".
      *
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *    一時ディレクトリ作成作成
           COPY    "CPORCSTEMPDIR.INC".
      *
      *    API用帳票データレスポンス作成
           COPY    "CPORCSAPIFM.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得
           COPY    "CPORCSPRTNM.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1005.INC".
           COPY    "CPSK1010.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
       01  SCRAREA.
           COPY     "API01RV2SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "*******************"
           DISPLAY   "* karte_no1 start *"
           DISPLAY   "*******************"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  KARTE1REQ-AREA
      *
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           PERFORM 200-MAIN-SEC
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           DISPLAY   "*****************"
           DISPLAY   "* karte_no1 end *"
           DISPLAY   "*****************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
      *    医療機関識別番号セット
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE   "0099"       TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
      *        ＳＰＡ共通設定
               INITIALIZE              SYS-1010-REC
               INITIALIZE              ORCSCOMMONAREA
               MOVE    "M00"       TO  ORCSCOMMON-PG
               CALL    "ORCSCOMMON"    USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                           SYS-1010-REC
      *        IF    ( SPA-ERRCD   NOT =   SPACE )
      *            MOVE   "0089"   TO  SPA-ERRCD
      *        END-IF
           END-IF
      *
           INITIALIZE                  STEMPDIR-AREA
           CALL    "ORCSTEMPDIR"       USING
                                       STEMPDIR-AREA
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        XML情報取り出し
               PERFORM 900-XML-READ-SEC
               MOVE    LOW-VALUE   TO  APILST39-BODY
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-REQUEST-CHK-SEC
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 2001-PRINT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    リクエスト項目処理
      *****************************************************************
       2001-REQUEST-CHK-SEC            SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( KARTE1REQ-REQNUM    =   "01" OR "02" )
                   CONTINUE
               ELSE
                   MOVE    "0101"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               EVALUATE    KARTE1REQ-REQNUM
               WHEN    "01"
                   MOVE    FUNCTION UPPER-CASE(KARTE1REQ-PRINT-MODE)
                                       TO  KARTE1REQ-PRINT-MODE
                   EVALUATE    KARTE1REQ-PRINT-MODE
                   WHEN    SPACE
                       CONTINUE
                   WHEN    "PDF"
                       INITIALIZE                  SRPLNK-AREA
                       MOVE    "CHK_MONBLOB"   TO  SRPLNK-MODE
                       CALL    "ORCSRPLNK"         USING
                                                   SRPLNK-AREA
                                                   SPA-AREA
                       IF    ( SRPLNK-RC       NOT =   ZERO )
                           MOVE    "0128"      TO  SPA-ERRCD
                       END-IF
                   WHEN    OTHER
                       MOVE    "0127"      TO  SPA-ERRCD
                   END-EVALUATE
      *
               END-EVALUATE
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( KARTE1REQ-HAKKOKBN  =   SPACE )
                   MOVE    "1"             TO  KARTE1REQ-HAKKOKBN
               END-IF
               IF    ( KARTE1REQ-HAKKOKBN  =   "1" OR "2" OR "3" )
                   CONTINUE
               ELSE
                   MOVE    "0107"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( KARTE1REQ-PTNUM     =   SPACE )
                   MOVE    "0102"      TO  SPA-ERRCD
               ELSE
                   INITIALIZE                  ORCSPTIDAREA
                   MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
                   MOVE    KARTE1REQ-PTNUM     TO  SPTID-PTNUM
                   CALL    "ORCSPTID"      USING
                                           ORCSPTIDAREA
                                           SPA-AREA
                   IF    ( SPTID-RC        NOT =   ZERO )
                       MOVE    "0103"      TO  SPA-ERRCD
                   END-IF
      *
                   MOVE    SPTID-PTNUM     TO  SPA-PTNUM
                   MOVE    SPTID-PTID      TO  SPA-PTID
      *
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               PERFORM 900-PTINF-KEY-SEL-SEC
               IF    ( FLG-PTINF       NOT =   ZERO )
                   MOVE    "0103"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( KARTE1REQ-SRYKA     =   SPACE )
                   CONTINUE
               ELSE
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
                   MOVE    "1005"          TO  SYS-KANRICD
                   MOVE    KARTE1REQ-SRYKA TO  SYS-KBNCD
                   MOVE    SPA-SYSYMD      TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   IF    ( FLG-SYSKANRI    =   ZERO )
                       MOVE    SYSKANRI-REC TO SYS-1005-REC
                       MOVE    KARTE1REQ-SRYKA
                                           TO  SPA-SRYKA
                       MOVE    SYS-1005-SRYKANAME1
                                           TO  SPA-SRYKAMEI
                   ELSE
                       MOVE    "0108"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD                   =   SPACE )
               IF    ( KARTE1REQ-HKNCOMBINUM   =   SPACE )
                   CONTINUE
               ELSE
                   IF    ( FUNCTION TRIM(KARTE1REQ-HKNCOMBINUM)
                                           IS  NOT NUMERIC )
                       MOVE    "0109"      TO  SPA-ERRCD
                   END-IF
      *
                   IF    ( SPA-ERRCD       =   SPACE )
                       PERFORM 900-HKNCOMBI-KEY-SEL-SEC
                       IF    ( FLG-HKNCOMBI    =   ZERO )
                           MOVE    COMB-HKNCOMBINUM
                                           TO  SPA-HKNCOMBINUM
                       ELSE
                           MOVE    "0109"  TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       2001-REQUEST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    印刷・データ取得処理
      *****************************************************************
       2001-PRINT-SEC                  SECTION.
      *
           PERFORM 20011-PRE-PRINT-SEC
      *
      *    カルテ印刷
           INITIALIZE                      ORCHC01AREA
           IF    ( KARTE1REQ-HAKKOKBN  =   "3" )
               MOVE    "4"                 TO  ORCHC01-SYORIKBN
           ELSE
               MOVE    KARTE1REQ-HAKKOKBN  TO  ORCHC01-SYORIKBN
           END-IF
      *
      *    クライアント印刷
           MOVE    "01"                TO  SPA-PRT-FLG
           INITIALIZE                  SRPLNK-AREA
           MOVE    "INS"           TO  SRPLNK-MODE
           MOVE    RPKEY-KARTE-NO1
                                   TO  SRPLNK-VAL
           CALL    "ORCSRPLNK"         USING
                                       SRPLNK-AREA
                                       SPA-AREA
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "00000001"          TO  ORCSPRTNM-KBNCD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYSKANRI-REC
      *
           CALL    ORCSPRTNM-PRTPG     USING
                                       SPA-AREA
                                       ORCHC01AREA
      *
           MOVE    SPACE               TO  SPA-PRT-FLG
      *
           .
       2001-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    印刷・データ取得処理
      *****************************************************************
       20011-PRE-PRINT-SEC             SECTION.
      *
           IF    ( KARTE1REQ-REQNUM    =   "01" )
               MOVE    2           TO  SPA-API-FLG
               IF    ( KARTE1REQ-PRINT-MODE    =   "PDF" )
                   MOVE    "3"     TO  SPA-PRTCONF
               END-IF
           ELSE
               MOVE    3           TO  SPA-API-FLG
           END-IF
      *
           CALL    "ORCSONPRTSET"      USING
                                       SPA-AREA
      *
           INITIALIZE                  ORCSPRTCTRLAREA
           CALL    "ORCSPRTCTRL"       USING
                                       SPA-AREA
                                       ORCSPRTCTRLAREA
                                       MCPAREA
      *
           .
       20011-PRE-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF    ( SPA-ERRCD    =  "0099" )
               MOVE    404             TO  APILST39-HTTPSTATUS
           ELSE
               INITIALIZE                  SAPIFM-AREA
               MOVE    SPA-ERRCD       TO  SAPIFM-ERRCD
               CALL    "ORCSAPIFM"         USING
                                           SAPIFM-AREA
                                           SPA-AREA
               IF    ( SAPIFM-OBJECT   NOT =   LOW-VALUE )
                   MOVE    SAPIFM-OBJECT
                                       TO  APILST39-BODY
                   MOVE    "application/json"
                                       TO  APILST39-CONTENT-TYPE
               END-IF
           END-IF
      *
           INITIALIZE                  STEMPDIR-AREA
           MOVE    "DEL"           TO  STEMPDIR-MODE
           CALL    "ORCSTEMPDIR"       USING
                                       STEMPDIR-AREA
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    数値変換処理
      *****************************************************************
       800-ORCSNUM-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-ORCSNUM
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-SNUM-INX        TO  SNUM-INX
           CALL    "ORCSNUM"           USING   ORCSNUMAREA
           IF    ( SNUM-RC         NOT =   ZERO  )
            OR   ( SNUM-MINKBN     NOT =   SPACE )
            OR   ( SNUM-SYOKBN     NOT =   SPACE )
               MOVE    1               TO  FLG-ORCSNUM
           ELSE
               MOVE    SNUM-OUTNUM     TO  WRK-SNUM-OUTNUM
           END-IF
      *
           .
      *
       800-ORCSNUM-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE  PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF     ( MCP-RC             =  ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ検索処理
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    FUNCTION NUMVAL(KARTE1REQ-HKNCOMBINUM)
                                       TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNCOMBI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       900-XML-READ-SEC             SECTION.
      *
           IF    ( APILST39-BODY    NOT =   LOW-VALUE )
               DISPLAY "karte_no1 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    "xml_karte_no1v2req"
                                           TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    APILST39-BODY           TO  APIREQ-OBJECT
           MOVE    ZERO                    TO  APIREQ-MODE
           MOVE    "karte_no1v2req"        TO  APIREQ-RECORDNAME
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC              =   ZERO  OR  1 )
               PERFORM 9001-XML-FILTER-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "0098"            TO  SPA-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE            TO  APILST39-BODY
      *
           .
       900-XML-READ-EXT.
           EXIT.
      *****************************************************************
      *    XMLフィルタリング処理
      *****************************************************************
       9001-XML-FILTER-SEC             SECTION.
      *
           MOVE    APIREQ-PATIENTINFOREQ
                                       TO  KARTE1REQ-DATA-AREA
      *
           .
       9001-XML-FILTER-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
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
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FETCHはしない）
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
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
      *    ＸＭＬ読込処理
      *****************************************************************
       910-XMLREAD-SEC                 SECTION.
      *
           MOVE    "XMLREAD"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLREAD-EXT.
           EXIT.
