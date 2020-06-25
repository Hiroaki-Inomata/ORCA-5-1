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
       PROGRAM-ID.             ORAPI001R8V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 外来請求書兼領収書
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/05/18    NACL−太田　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                             DIVISION.
       CONFIGURATION                           SECTION.
       INPUT-OUTPUT                            SECTION.
       FILE-CONTROL.
      *
      *
       DATA                                    DIVISION.
       FILE                                    SECTION.
      *
      *
       WORKING-STORAGE                         SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    収納業務共通パラメタ
           COPY    "S01COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-PTINF                       PIC 9(01).
           03  FLG-SYUNOU                      PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                            PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-DENPNUM                     PIC 9(7).
           03  WRK-FILEMEI.
               05  WRK-FILEMEI-YMD             PIC X(08).
               05  WRK-FILEMEI-HMS             PIC X(06).
               05  WRK-FILEMEI-CD              PIC X(06).
      *
       01  CONST-AREA.
           03  CONST-MODE-G.
               05  CONST-MODE-REPRINT          PIC X(02)   VALUE "09".
               05  CONST-MODE-MONTHLY-PRINT    PIC X(02)   VALUE "10".
      *
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
      *    オンライン帳票名・出力先プリンタ名取得
           COPY    "CPORCSPRTNM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    請求書・明細書印刷サブ
           COPY    "CPSSUB03.INC".
      *
      *    印刷リクエストチェック
           COPY    "CPSSUB06.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPIRV2REQ.INC".
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
       01  UIDIO-AREA.
           03  C-RET                   PIC X(2).
           03  C-UID                   PIC X(36).
       01  UIDIO-ST                    PIC 9(2).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSK1010.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
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
           DISPLAY   "*************************"
           DISPLAY   "* invoice_receipt start *"
           DISPLAY   "*************************"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  SPA-S01KYOUTU
           INITIALIZE                  PTINF-REC
           INITIALIZE                  SSUB03-AREA
           INITIALIZE                  SSUB06-AREA
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
           DISPLAY   "*************************"
           DISPLAY   "* invoice_receipt end   *"
           DISPLAY   "*************************"
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
               IF    ( IRREQ-REQNUM    =   "01" OR "02" )
                   CONTINUE
               ELSE
                   MOVE    "0101"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               EVALUATE    IRREQ-REQNUM
               WHEN    "01"
                   MOVE    FUNCTION UPPER-CASE(IRREQ-PRINT-MODE)
                                       TO  IRREQ-PRINT-MODE
                   EVALUATE    IRREQ-PRINT-MODE
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
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( IRREQ-PTNUM     =   SPACE )
                   MOVE    "0102"      TO  SPA-ERRCD
               ELSE
                   INITIALIZE                  ORCSPTIDAREA
                   MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
                   MOVE    IRREQ-PTNUM     TO  SPTID-PTNUM
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
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 900-PTINF-KEY-SEL-SEC
               IF    ( FLG-PTINF       NOT =   ZERO )
                   MOVE    "0103"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               INITIALIZE                  SSUB06-AREA
               MOVE    CONST-MODE-REPRINT
                                       TO  SSUB06-REQMODE
               MOVE    "O"             TO  SSUB06-NYUGAIKBN
               MOVE    IRREQ-DENPNUM   TO  SSUB06-DENPNUM
               MOVE    "1"             TO  SSUB06-SKYPRT
               CALL    "ORCGSSUB06"        USING
                                           SSUB06-AREA
                                           SPA-AREA
      D        DISPLAY "SPA-ERRCD["    SPA-ERRCD "]"
      *
      *        0033 診療年月の入力に誤りがあります
      *        0034 入外区分の入力に誤りがあります
      * *      0085 請求情報が存在しません
      *        0086 入外区分を入力してください
      * *      0100 伝票番号を入力してください
      * *      0101 伝票番号の入力に誤りがあります
      *        0116 明細情報が存在しません
      *        0121 請求書兼領収書印刷区分の入力に誤りがあります。
      *        0122 診療費明細書印刷区分の入力に誤りがあります
      *        0123 帳票の印刷指示がありません
      *        0125 期間指定開始日の入力に誤りがあります
      *        0126 期間指定終了日の入力に誤りがあります
      *        0127 期間指定開始日＞期間指定終了日となっています
      *        0128 印刷対象の請求情報が存在しません
      *        0129 外来請求書兼領収書発行方法の入力に誤りがあります
      *        0130 診療年月を入力してください
      *        3005 明細番号の入力に誤りがあります
      *
               EVALUATE    SPA-ERRCD
               WHEN    SPACE
                   CONTINUE
               WHEN    "0033"
                   MOVE    "0115"      TO  SPA-ERRCD
               WHEN    "0034"
                   MOVE    "0122"      TO  SPA-ERRCD
               WHEN    "0085"
                   MOVE    "0123"      TO  SPA-ERRCD
               WHEN    "0086"
                   MOVE    "0124"      TO  SPA-ERRCD
               WHEN    "0100"
                   MOVE    "0104"      TO  SPA-ERRCD
               WHEN    "0101"
                   MOVE    "0126"      TO  SPA-ERRCD
               WHEN    "0116"
                   MOVE    "0125"      TO  SPA-ERRCD
               WHEN    "3005"
                   MOVE    "0126"      TO  SPA-ERRCD
      *        WHEN    "0121"
      *        WHEN    "0122"
      *        WHEN    "0123"
      *        WHEN    "0125"
      *        WHEN    "0126"
      *        WHEN    "0127"
      *        WHEN    "0128"
      *        WHEN    "0129"
      *        WHEN    "0130"
               WHEN    OTHER
                   MOVE    "0123"      TO  SPA-ERRCD
               END-EVALUATE
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
           PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
           INITIALIZE                      WRK-FILEMEI
           MOVE    SMCNDATE-YMD        TO  WRK-FILEMEI-YMD
           MOVE    SMCNDATE-HMS        TO  WRK-FILEMEI-HMS
      *
           INITIALIZE                      SPA-S01KYOUTU
           MOVE    SYU-DENPNUM         TO  SPA-S01-DENPNUM
           MOVE    "API"               TO  SPA-S01-GYOUMUID
           MOVE    SYU-SYUHKNNUM       TO  SPA-S01-HKNNUM
           MOVE    PTINF-KANANAME      TO  SPA-S01-KANANAME
           MOVE    PTINF-NAME          TO  SPA-S01-NAME
           MOVE    SPA-PTID            TO  SPA-S01-PTID
           MOVE    SPA-PTNUM           TO  SPA-S01-PTNUM
           MOVE    SPA-RSI-HKNKBN      TO  SPA-S01-RSI-HKNKBN
           MOVE    SYU-SRYYMD          TO  SPA-S01-SRYYMD
           PERFORM 800-ORCUIDSET-SEC
           MOVE    C-UID               TO  SPA-S01-UID
           MOVE    WRK-FILEMEI         TO  SPA-S01-FILEMEI
      *
           MOVE    "2"                 TO  SPA-S01-NYUGAIKBN
      *
           INITIALIZE                      SSUB03-AREA
           MOVE    3                   TO  SSUB03-REQNO
           MOVE    "1"                 TO  SSUB03-SKYPRT
           CALL    "ORCGSSUB03"        USING
                                       SPA-AREA
                                       SPA-S01KYOUTU
                                       SSUB03-AREA
      D    DISPLAY "SPA-ERRCD[" SPA-ERRCD "]"
      *
           .
       2001-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    印刷・データ取得処理
      *****************************************************************
       20011-PRE-PRINT-SEC             SECTION.
      *
           IF    ( IRREQ-REQNUM    =   "01" )
               MOVE    2           TO  SPA-API-FLG
               IF    ( IRREQ-PRINT-MODE    =   "PDF" )
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
               MOVE    404             TO  APILST44-HTTPSTATUS
           ELSE
               INITIALIZE                  SAPIFM-AREA
               MOVE    SPA-ERRCD       TO  SAPIFM-ERRCD
               CALL    "ORCSAPIFM"         USING
                                           SAPIFM-AREA
                                           SPA-AREA
               IF    ( SAPIFM-OBJECT   NOT =   LOW-VALUE )
                   MOVE    SAPIFM-OBJECT
                                       TO  APILST44-BODY
                   MOVE    "application/json"
                                       TO  APILST44-CONTENT-TYPE
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
      *    UID取得処理
      *****************************************************************
       800-ORCUIDSET-SEC               SECTION.
      *
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       UIDIO-ST
                                       UIDIO-AREA
      *
           .
       800-ORCUIDSET-EXT.
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
      *************************************************************
      *    収納マスター検索処理
      *************************************************************
       900-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
      *
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    "2"                 TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    SSUB06-DENPNUM      TO  SYU-DENPNUM
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
               INITIALIZE                  SYUNOU-REC
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       900-XML-READ-SEC             SECTION.
      *
           IF    ( APILST44-BODY    NOT =   LOW-VALUE )
               DISPLAY "invoice_receipt object is not low_value"
           END-IF
      *
           INITIALIZE                          XML-APIREQ
           MOVE    "xml_invoice_receiptv2req"
                                           TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    APILST44-BODY           TO  APIREQ-OBJECT
           MOVE    ZERO                    TO  APIREQ-MODE
           MOVE    "invoice_receiptv2req"
                                           TO  APIREQ-RECORDNAME
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC              =   ZERO  OR  1 )
               PERFORM 9001-XML-FILTER-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "0098"            TO  SPA-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE            TO  APILST44-BODY
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
                                       TO  IRREQ-DATA-AREA
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
