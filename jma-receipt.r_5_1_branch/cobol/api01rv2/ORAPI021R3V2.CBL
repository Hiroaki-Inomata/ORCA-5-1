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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORAPI021R3V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 中途データ一覧  (xml2)
      *  管理者            :
      *  作成日付    作業者        記述
      *  13/09/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.08.00     NACL-多々納  17/07/XX  同日再入院区分追加
      *                                     HAORI展開中対応
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
       DATA                DIVISION.
       FILE                    SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-TEND                PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-WKSRYACT            PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDX-H               PIC 9(04).
           03  TBL-MAX             PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SRYCNT              PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(04).
               05  WRK-SMM             PIC X(02).
               05  WRK-SDD             PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *    時間編集
           03  WRK-THMS.
               05  WRK-THH             PIC X(02).
               05  WRK-TMM             PIC X(02).
               05  WRK-TSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
      *    エラーコード
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-KEICD               PIC X(02).
      *
           03  WRK-MCP-PATHNAME        PIC X(62).
           03  WRK-MCP-WINDOW          PIC X(62).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYM           PIC X(06).
               05  WRK-SYSDD           PIC X(02).
      *
           03  WRK-KEY-AREA.
               05  WRK-KEY-PTID        PIC 9(10).
               05  WRK-KEY-SRYKA       PIC X(02).
               05  WRK-KEY-HKNCOMBI    PIC X(04).
               05  WRK-KEY-DOUJITSUKBN    PIC X(01).
      *
           03  WRK-PTID                PIC 9(10).
           03  WRK-PTNUM               PIC X(20).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-NAME          PIC X(80).
           03  WRK-DRCD                PIC X(05).
           03  WRK-DRCD-NAME           PIC X(80).
      *
       01  WRK-XML-AREA.
           03  WRK-PERFORM-DATE.
               05  WRK-PERFORM-DATE1       PIC X(07).
               05  WRK-PERFORM-DATE2       PIC X(03).
           03  WRK-PERFORM-YMD.
               05  WRK-PERFORM-YM          PIC X(06).
               05  WRK-PERFORM-DD          PIC X(02).
           03  WRK-IN-PTNUM               PIC X(20).
           03  WRK-IN-SRYKA               PIC X(02).
           03  WRK-IN-SRYKA-NAME          PIC X(80).
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(100).
           03  WRK-ATO-INPUT           PIC X(100).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "CPORCSKANACHK.INC".
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPTMDCGETV2REQ.INC".
      *    API XML書き込み用定義体
           COPY    "CPTMDCGETV2RES.INC".
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
      *    （職員情報）
           COPY    "CPSK1010.INC".
      *    （診療科情報）
           COPY    "CPSK1005.INC".
      *    （医療機関情報）
           COPY    "CPSK1001.INC".
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "ORCA-BLOB".
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
       01  SCRAREA.
           COPY    "API01RV2SCRAREA.INC".
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
           DISPLAY   "********************"
           DISPLAY   "* tmedicalget start*"
           DISPLAY   "********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
           INITIALIZE                      CNT-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *
           IF      WRK-ERRCD           =   "99"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST13-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-APTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "*******************"
           DISPLAY   "* tmedicalget end *"
           DISPLAY   "*******************"
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
           INITIALIZE                      WRK-XML-AREA
           INITIALIZE                      XML-TMEDICALGETRES
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Medical Info"       TO  TMDCRES-RESKEY
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TMDCRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  TMDCRES-INFORMATION-TIME
      *
      *    医療機関識別番号セット 
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "99"             TO  WRK-ERRCD
               GO  TO         100-INIT-EXT
           END-IF
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "89"             TO  WRK-ERRCD
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    XML情報取り出し
           PERFORM 1000-APTXMLREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD
      *
           MOVE    ZERO                TO  FLG-SYORI
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
      *        患者情報内容編集
               PERFORM 20021-TMDCRES-HEN-SEC
           END-IF
      *
      *    中途データ情報
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2002-SYORI-01-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-APTXMLREAD-SEC   SECTION.
      *
           IF      APILST13-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_tmedicalgetv2req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST13-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "tmedicalgetreq"    TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
               MOVE   "97"             TO  WRK-ERRCD
               GO     TO   1000-APTXMLREAD-EXT
           END-IF
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_tmedicalgetv2req"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST13-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "tmedicalgetreq"    TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ          TO  XML-TMEDICALGETREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           .
       1000-APTXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-APTXMLMAKE-SEC   SECTION.
      *
      *    エラーメッセージ編集
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-KEICD           TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD           =   SPACE
               MOVE    ZERO                TO  TMDCRES-API-RESULT
               MOVE    "処理終了"          TO  TMDCRES-API-RESULT-MSG
           ELSE
               IF      WRK-ERRMSG          =   SPACE
      *        エラー・警告メッセージ
                   PERFORM 890-ERRCD-MSG-SEC
               END-IF
               MOVE    WRK-ERRCD           TO  TMDCRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  TMDCRES-API-RESULT-MSG
           END-IF
      *
           PERFORM 3001-TMDCXMLMAKE-SEC
      *
          .
       300-APTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       3001-TMDCXMLMAKE-SEC        SECTION.
      *
           IF      APILST13-BODY   NOT =   LOW-VALUE
               DISPLAY "TMDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_tmedicalgetv2res"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  TMDCRES-MODE
           MOVE    "tmedicalgetres"    TO  TMDCRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-TMEDICALGETRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_tmedicalgetv2res"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  TMDCRES-MODE
           MOVE    "tmedicalgetres"    TO  TMDCRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-TMEDICALGETRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    TMDCRES-OBJECT      TO  APILST13-BODY 
               MOVE    "application/xml"   TO  APILST13-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       3001-TMDCXMLMAKE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
           IF      TMDCREQ-PATIENTID   =   LOW-VALUE
               MOVE  SPACE             TO  TMDCREQ-PATIENTID
           END-IF
           IF     TMDCREQ-PERFORM-DATE =   LOW-VALUE
               MOVE  SPACE             TO  TMDCREQ-PERFORM-DATE
           END-IF
           IF      TMDCREQ-DEP-CODE    =   LOW-VALUE
               MOVE  SPACE             TO  TMDCREQ-DEP-CODE
           END-IF
      *
           .
       10001-XML-REMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報設定内容チェック
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
      *    編集処理
           PERFORM 20010-INPUT-HEN-SEC
      *
      *    診療日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PERFORM-CHK-SEC
           END-IF
      *    患者番号チェック
           IF     (WRK-ERRCD           =   SPACE )
           AND    (WRK-PTNUM       NOT =   SPACE )
               PERFORM 20011-PTNUM-CHK-SEC
           END-IF
      *    診療科コードチェック
           IF     (WRK-IN-SRYKA    NOT =   SPACE  )
           AND    (WRK-ERRCD           =   SPACE  )
               MOVE    WRK-IN-SRYKA        TO  WRK-SRYKA
               PERFORM 20011-SRYKA-CHK-SEC
               MOVE    WRK-SRYKA-NAME      TO  WRK-IN-SRYKA-NAME
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目変換編集処理
      *****************************************************************
       20010-INPUT-HEN-SEC     SECTION.
      *
      *    診療日付
           MOVE    TMDCREQ-PERFORM-DATE    TO  WRK-HEN-DATE
           MOVE    SPACE                   TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-PERFORM-YMD
      *    未設定時はシステム情報設定
           IF      WRK-PERFORM-YMD         =   SPACE
               MOVE    SMCNDATE-YMD        TO  WRK-PERFORM-YMD
               MOVE    TMDCRES-INFORMATION-DATE
                                           TO  TMDCREQ-PERFORM-DATE
               MOVE    "K1"                TO  WRK-KEICD
           END-IF
      *    システム日付とする
           MOVE    WRK-PERFORM-YMD     TO  SPA-SYSYMD
      *
      *    入外区分
           IF      TMDCREQ-OUTPATIENT-CLASS    =   "1"
      *        入院
               MOVE    "1"                 TO  SPA-NYUGAIKBN
           ELSE
      *        外来
               MOVE    "2"                 TO  SPA-NYUGAIKBN
           END-IF
      *    患者番号
           MOVE    TMDCREQ-PATIENTID       TO  WRK-PTNUM
      *    診療科
           MOVE    TMDCREQ-DEP-CODE        TO  WRK-IN-SRYKA
           .
       20010-INPUT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日付チェック
      *****************************************************************
       20011-PERFORM-CHK-SEC    SECTION.
      *
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-PERFORM-YMD     TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "11"                TO  WRK-ERRCD
           END-IF
           MOVE    WRK-PERFORM-YMD     TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  WRK-PERFORM-DATE
      *
           .
       20011-PERFORM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック
      *****************************************************************
       20011-PTNUM-CHK-SEC     SECTION.
      *
           INITIALIZE                      PTINF-REC
      *    患者番号取得
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    WRK-PTNUM           TO  SPTID-PTNUM
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
               MOVE    "10"                TO  WRK-ERRCD
               GO      TO                  20011-PTNUM-CHK-EXT
           END-IF
      *
           MOVE    SPTID-PTNUM         TO  WRK-PTNUM
           MOVE    SPTID-PTID          TO  SPA-PTID
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPTID-PTID          TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    "10"                TO  WRK-ERRCD
           END-IF
           .
       20011-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科コードチェック
      *****************************************************************
       20011-SRYKA-CHK-SEC         SECTION.
      *
      *    診療科の存在チェック
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA           TO  SYS-1005-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME1 TO  WRK-SRYKA-NAME
           ELSE
               MOVE    "13"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴編集処理
      *****************************************************************
       2002-SYORI-01-SEC           SECTION.
      *
      *    患者情報内容編集
           PERFORM 20021-TMDCRES-HEN-SEC
      *
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-SYSYMD          TO  WKSRY-SRYYMD
           MOVE    SPA-PTID            TO  WKSRY-PTID
           IF      SPA-PTID            =   ZERO
               MOVE    "key14"             TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "key15"             TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           INITIALIZE                  WRK-KEY-AREA
           MOVE    ZERO                TO  TBL-MAX
           MOVE    ZERO                TO  IDX
           PERFORM
               UNTIL   (FLG-WKSRYACT   =   1 )  OR
                       (WRK-ERRCD      NOT =   SPACE)
      *
               MOVE    ZERO            TO  FLG-CHK
      *
               IF     (WRK-IN-SRYKA    NOT =   SPACE )
                 AND  (WRK-IN-SRYKA    NOT =   WKSRY-SRYKA )
                   MOVE    1               TO  FLG-CHK
               END-IF
      *
               IF     (WRK-KEY-PTID        =   WKSRY-PTID)
                 AND  (WRK-KEY-SRYKA       =   WKSRY-SRYKA)
                 AND  (WRK-KEY-HKNCOMBI    =   WKSRY-HKNCOMBI)
      *H29.7
                 AND  (WRK-KEY-DOUJITSUKBN =   WKSRY-DOUJITSUKBN)
                   MOVE    1               TO  FLG-CHK
               END-IF
      *
               IF      FLG-CHK             =   ZERO
                   ADD     1               TO  TBL-MAX
                   IF      TBL-MAX         >   500
                       MOVE    "14"            TO  WRK-ERRCD
                   ELSE
      *                明細項目編集
                       PERFORM 20021-WKSRYACT-HENSYU-SEC
      *
                       MOVE    WKSRY-PTID      TO  WRK-KEY-PTID
                       MOVE    WKSRY-SRYKA     TO  WRK-KEY-SRYKA
                       MOVE    WKSRY-HKNCOMBI  TO  WRK-KEY-HKNCOMBI
      *H29.7
                       MOVE    WKSRY-DOUJITSUKBN
                                               TO  WRK-KEY-DOUJITSUKBN
                   END-IF
               END-IF
      *
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-WKSRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      TBL-MAX             =   ZERO
      *        対象なし
               MOVE    "15"                TO  WRK-ERRCD
           END-IF
           .
       2002-SYORI-01-EXT.
           EXIT.
      *****************************************************************
      *    取得情報編集処理
      *****************************************************************
       20021-TMDCRES-HEN-SEC     SECTION.
      *
           MOVE    WRK-PERFORM-DATE    TO  TMDCRES-PERFORM-DATE
      *    入外区分
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    "1"             TO  TMDCRES-OUTPATIENT-CLASS
           ELSE
               MOVE    "2"             TO  TMDCRES-OUTPATIENT-CLASS
           END-IF
      *
           MOVE    1                   TO  IDX
           IF      WRK-IN-SRYKA    NOT =   SPACE
               MOVE    WRK-IN-SRYKA       TO  TMDCRES-DEP-CODE (IDX)
               MOVE    WRK-IN-SRYKA-NAME  TO  TMDCRES-DEP-NAME (IDX)
           END-IF
      *
           MOVE    WRK-PTNUM           TO  TMDCRES-PATIENTID (IDX)
      *    患者番号あり
           IF     (WRK-PTNUM       NOT =   SPACE )
              AND (SPA-PTID        NOT =   ZERO  )
               PERFORM 200211-PTINF-HEN-SEC
           END-IF
           .
       20021-TMDCRES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者情報出力情報編集処理
      *****************************************************************
       200211-PTINF-HEN-SEC     SECTION.
      *
      *    患者情報
           MOVE    WRK-PTNUM           TO  TMDCRES-PATIENTID (IDX)
      *
           MOVE    PTINF-NAME          TO  TMDCRES-NAME     (IDX)
           MOVE    PTINF-KANANAME      TO  TMDCRES-KANANAME (IDX)
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TMDCRES-BIRTHDAY  (IDX)
           MOVE    PTINF-SEX           TO  TMDCRES-SEX       (IDX)
           .
       200211-PTINF-HEN-EXT.
           EXIT.
      *****************************************************************
      *    中途終了データ情報編集処理
      *****************************************************************
       20021-WKSRYACT-HENSYU-SEC     SECTION.
      *
           ADD    1                    TO  IDX
      *
      *    患者情報検索
           IF      WKSRY-PTID      NOT =   WRK-KEY-PTID
               PERFORM 200211-PTNUM-HEN-SEC
           END-IF
      *    患者情報編集
           PERFORM 200211-PTINF-HEN-SEC
      *    診療科
           PERFORM 200212-SRYKAMEI-HEN-SEC
           MOVE    WKSRY-SRYKA         TO  TMDCRES-DEP-CODE (IDX)
           MOVE    WRK-SRYKA-NAME      TO  TMDCRES-DEP-NAME (IDX)
      *    ドクター
           IF      WKSRY-DRCD      NOT =   ZERO
               PERFORM 200213-DRCD-HEN-SEC
               MOVE    WKSRY-DRCD          TO  TMDCRES-DR-CODE (IDX)
               MOVE    WRK-DRCD-NAME       TO  TMDCRES-DR-NAME (IDX)
           END-IF
      *    保険組合せ
           MOVE    WKSRY-HKNCOMBI      TO  TMDCRES-COMBINATION-NUMBER
                                                               (IDX)
           PERFORM 200214-HKNCOMBI-HEN-SEC
      *
      *    更新時間
           MOVE    SPACE               TO  WRK-SYMD
           MOVE    WKSRY-UPHMS         TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-TIME        TO  TMDCRES-UPHMS  (IDX)
      *    更新中フラグ
      **   IF      WKSRY-MOD-FLG   NOT =   ZERO
           EVALUATE    WKSRY-MOD-FLG
           WHEN    1
           WHEN    9
      *        展開中（オルカ）
               MOVE    "1"              TO  TMDCRES-MODE-FLG (IDX)
           WHEN    5
      *        展開中（HAORI）
               MOVE    "5"              TO  TMDCRES-MODE-FLG (IDX)
           WHEN    0
               MOVE    "0"              TO  TMDCRES-MODE-FLG (IDX)
           END-EVALUATE
      *
      *    電子カルテフラグ
           IF      WKSRY-KARTE-FLG      =   1
      *        中途終了登録
               MOVE    "1"              TO  TMDCRES-KARTE-FLG (IDX)
           ELSE
      *        中途終了登録以外
               MOVE    "0"              TO  TMDCRES-KARTE-FLG (IDX)
           END-IF
      *    電子カルテキー（ＵＩＤ）
           MOVE    WKSRY-KARTE-KEY      TO  TMDCRES-MEDICAL-UID (IDX)
      *H29.7
      *    同日再入院区分
           MOVE    WKSRY-DOUJITSUKBN    TO  TMDCRES-DOUJITSUKBN(IDX)
      *
           .
       20021-WKSRYACT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック
      *****************************************************************
       200211-PTNUM-HEN-SEC     SECTION.
      *
      *    患者番号取得
           INITIALIZE                  ORCSPTIDAREA
           MOVE    WKSRY-HOSPNUM       TO  SPTID-HOSPNUM
           MOVE    WKSRY-PTID          TO  SPTID-PTID
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           MOVE    SPTID-PTNUM         TO  WRK-PTNUM
           IF      SPTID-RC        NOT =   ZERO
      *        患者番号エラー
               MOVE    ALL "*"             TO  WRK-PTNUM
               MOVE    1                   TO  FLG-ERR
           END-IF
      *
           INITIALIZE                      PTINF-REC
           MOVE    WKSRY-HOSPNUM       TO  PTINF-HOSPNUM
           MOVE    WKSRY-PTID          TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    1                   TO  FLG-ERR
           END-IF
           .
       200211-PTNUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科名称編集処理
      *****************************************************************
       200212-SRYKAMEI-HEN-SEC        SECTION.
      *
           MOVE    ZERO                TO  IDX-H
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >=  IDX
               IF     (WKSRY-SRYKA     =   TMDCRES-DEP-CODE (IDY))
                   MOVE    IDY            TO  IDX-H
                   MOVE    IDX            TO  IDY
               END-IF
           END-PERFORM
           IF       IDX-H              =   ZERO
      *        診療科検索
               MOVE    WKSRY-SRYKA         TO   WRK-SRYKA
               PERFORM 20011-SRYKA-CHK-SEC
           ELSE
               MOVE    TMDCRES-DEP-NAME (IDX-H)
                                           TO  WRK-SRYKA-NAME
           END-IF
           .
       200212-SRYKAMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター名称編集処理
      *****************************************************************
       200213-DRCD-HEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDX-H
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >=  IDX
               IF     (WKSRY-DRCD      =  TMDCRES-DR-CODE (IDY))
                   MOVE    IDY            TO  IDX-H
                   MOVE    IDX            TO  IDY
               END-IF
           END-PERFORM
           IF       IDX-H              =   ZERO
      *        ドクター検索
               MOVE    WKSRY-DRCD          TO   WRK-DRCD
               PERFORM 2002131-DRCD-CHK-SEC
           ELSE
               MOVE    TMDCRES-DR-NAME (IDX-H)
                                           TO  WRK-DRCD-NAME
           END-IF
           .
       200213-DRCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター編集処理
      *****************************************************************
       2002131-DRCD-CHK-SEC         SECTION.
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WRK-DRCD            TO  SYS-1010-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
               MOVE    SYS-1010-NAME       TO  WRK-DRCD-NAME
           ELSE
               MOVE    SPACE               TO  WRK-DRCD-NAME
           END-IF
      *
           .
       2002131-DRCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       200214-HKNCOMBI-HEN-SEC     SECTION.
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    WKSRY-HOSPNUM       TO  COMB-HOSPNUM
           MOVE    WKSRY-PTID          TO  COMB-PTID
           MOVE    WKSRY-HKNCOMBI      TO  COMB-HKNCOMBINUM
           PERFORM 920-HKNCOMBI-READ-SEC
      *
      *    主保険−保険番号
           MOVE    COMB-HKNNUM         TO  TMDCRES-MAIN-INSURANCE-CLASS
                                                            (IDX)
      *    主保険−短縮制度名
           MOVE    COMB-SYU-TANSEIDONAME
                                       TO  TMDCRES-MAIN-INSURANCE-NAME
                                                            (IDX)
           IF      COMB-HKNID      NOT =   ZERO
      *        保険情報編集
               PERFORM 930-PTHKNINF-READ-SEC
      *        保険者番号
               MOVE    PTHKN-HKNJANUM  TO
                                       TMDCRES-MAIN-INSURANCE-NUMBER
                                                            (IDX)
      *        記号・番号
               MOVE    PTHKN-KIGO      TO  TMDCRES-MAIN-MARK  (IDX)
               MOVE    PTHKN-NUM       TO  TMDCRES-MAIN-NUMBER(IDX)
           END-IF
      *    公費情報
           PERFORM VARYING    IDK      FROM    1   BY  1
                   UNTIL      IDK      >   4
               IF      COMB-KOHID (IDK)    NOT =   ZERO
      *            公費の種類
                   MOVE    COMB-KOHHKNNUM (IDK)
                                       TO  TMDCRES-INSURANCE-CLASS
                                                            (IDX IDK)
                   MOVE    COMB-KOH-TANSEIDONAME (IDK)
                                       TO  TMDCRES-INSURANCE-NAME
                                                            (IDX IDK)
      *            公費情報編集
                   PERFORM 940-PTKOHINF-READ-SEC
      *            負担者番号
                   MOVE    PTKOH-FTNJANUM  TO  TMDCRES-PROVIDER-NUMBER
                                                            (IDX IDK)
                   MOVE    PTKOH-JKYSNUM   TO  TMDCRES-RECIPIENT-NUMBER
                                                            (IDX IDK)
               END-IF
           END-PERFORM
           .
       200214-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
           ELSE
               INITIALIZE                  WRK-HEN-DATE
               MOVE    WRK-SYY             TO  WRK-HEN-YY
               MOVE    WRK-SMM             TO  WRK-HEN-MM
               MOVE    WRK-SDD             TO  WRK-HEN-DD
               MOVE    "-"                 TO  WRK-HEN-H1
               MOVE    "-"                 TO  WRK-HEN-H2
               IF      WRK-SYMD            =   "99999999"
                   MOVE    "12"                TO  WRK-HEN-MM
                   MOVE    "31"                TO  WRK-HEN-DD
               END-IF
           END-IF
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THH             TO  WRK-HEN-THH
           MOVE    WRK-TMM             TO  WRK-HEN-TMM
           MOVE    WRK-TSS             TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *****************************************************************
      *    日付変換編集処理
      *****************************************************************
       802-DAYHEN02-SEC                SECTION.
      *
           INITIALIZE                  WRK-SYMD
           MOVE    WRK-HEN-YY          TO  WRK-SYY
           MOVE    WRK-HEN-MM          TO  WRK-SMM
           MOVE    WRK-HEN-DD          TO  WRK-SDD
           IF      WRK-SYMD            =   "99991231"
               MOVE    "99"                TO  WRK-SMM
               MOVE    "99"                TO  WRK-SDD
           END-IF
      *
           INITIALIZE                  WRK-THMS
           MOVE    WRK-HEN-THH         TO  WRK-THH
           MOVE    WRK-HEN-TMM         TO  WRK-TMM
           MOVE    WRK-HEN-TSS         TO  WRK-TSS
           .
       802-DAYHEN02-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
           WHEN    "10"
      ******** MOVE    "該当患者番号なし"
               MOVE    "患者番号に該当する患者が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "診療日が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE    "診療科が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "14"
               MOVE    "対象が５００件以上存在します。"
                                               TO  WRK-ERRMSG
           WHEN    "15"
               MOVE    "対象がありません"
                                               TO  WRK-ERRMSG
      *警告
           WHEN    "K1"
               MOVE    "診療日を設定しました"
                                               TO  WRK-ERRMSG
      *共通エラー
           WHEN    "89"
      *        ORCSCOMMON のエラー
               EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "職員情報が取得できません"
                                               TO  WRK-ERRMSG
               WHEN    "1002"
                   MOVE    "医療機関情報が取得できません"
                                               TO  WRK-ERRMSG
               WHEN    "1003"
                   MOVE    "システム日付が取得できません"
                                               TO  WRK-ERRMSG
               WHEN    "1005"
                   MOVE    "患者番号構成情報が取得できません"
                                               TO  WRK-ERRMSG
               WHEN    "1015"
                   STRING  "グループ医療機関が不整合です。"
                           "処理を終了して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    "システム項目が設定できません"
                                               TO  WRK-ERRMSG
               END-EVALUATE
           WHEN    "90"
               MOVE    "他端末使用中"          TO  WRK-ERRMSG
           WHEN    "91"
               MOVE    "処理区分未設定"        TO  WRK-ERRMSG
           WHEN    "97"
               MOVE   "送信内容に誤りがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "98"
               MOVE   "送信内容の読込ができませんでした"
                                               TO  WRK-ERRMSG
           WHEN    "99"
               MOVE    "ユーザＩＤ未登録。"
                                               TO  WRK-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報取得
      *****************************************************************
       900-PTINF-READ-SEC              SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    中途終了データ読込
      *****************************************************************
       910-WKSRYACT-READ-SEC         SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  WKSRYACT-REC
               MOVE    ZERO            TO  FLG-WKSRYACT
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
      *
           .
       910-WKSRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       920-HKNCOMBI-READ-SEC              SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   MOVE    ZERO                TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                  HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       920-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報取得処理
      *****************************************************************
       930-PTHKNINF-READ-SEC              SECTION.
      *
           INITIALIZE                  PTHKNINF-REC
           MOVE    COMB-HOSPNUM        TO  PTHKN-HOSPNUM
           MOVE    COMB-PTID           TO  PTHKN-PTID
           MOVE    COMB-HKNID          TO  PTHKN-HKNID 
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
                   MOVE    ZERO                TO  FLG-PTHKNINF
               ELSE
                   MOVE    1                   TO  FLG-PTHKNINF
                   INITIALIZE                  PTHKNINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
               INITIALIZE                  PTHKNINF-REC
           END-IF
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       930-PTHKNINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報取得処理
      *****************************************************************
       940-PTKOHINF-READ-SEC              SECTION.
      *
           INITIALIZE                  PTKOHINF-REC
           MOVE    COMB-HOSPNUM        TO  PTKOH-HOSPNUM
           MOVE    COMB-PTID           TO  PTKOH-PTID
           MOVE    COMB-KOHID (IDK)    TO  PTKOH-KOHID
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTKOHINF-REC
                   MOVE    ZERO                TO  FLG-PTKOHINF
               ELSE
                   MOVE    1                   TO  FLG-PTKOHINF
                   INITIALIZE                  PTKOHINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       940-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-SEL-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI         =   ZERO
                   MOVE    MCPDATA-REC      TO  SYSKANRI-REC
               END-IF
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-SYSKANRI-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
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
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
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
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *
