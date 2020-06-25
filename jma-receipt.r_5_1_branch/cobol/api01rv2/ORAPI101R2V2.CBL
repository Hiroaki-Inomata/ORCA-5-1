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
       PROGRAM-ID.         ORAPI101R2V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : システム管理　患者登録・診療行為設定情報取得
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/11/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT           DIVISION.
       CONFIGURATION         SECTION.
       INPUT-OUTPUT          SECTION.
      *
       FILE-CONTROL.
       DATA                DIVISION.
      *FILE                    SECTION.
      *
       WORKING-STORAGE             SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
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
           03  WRK-MCP-PATHNAME        PIC X(64).
           03  WRK-ZZ9                 PIC ZZZ9.
      *
           03  WRK-CNT                 PIC 9(04).
      *    リクエスト番号
           03  WRK-REQUEST-NUMBER      PIC X(02).
      *
       01  WRK-XML-AREA.
           03  WRK-IN-DATE                PIC X(10).
      *
           03  WRK-IN-YMD                 PIC X(08).
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPAPISYSDAY01V2REQ.INC".
      *    API XML書き込み用定義体
           COPY    "CPAPISYSDAY01V2RES.INC".
      *
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    診療科情報
           COPY    "CPSK1005.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK1010.INC".
      *
           COPY    "CPSK1017.INC".
      *
           COPY    "CPSK1007.INC".
      *
           COPY    "CPSK1008.INC".
      *
           COPY    "CPSK1038.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "ORCA-BLOB".
      *
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
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "************************"
           DISPLAY   "* system01_manage start*"
           DISPLAY   "************************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
      ***  INITIALIZE                      CNT-AREA
           INITIALIZE                      SPA-AREA
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
               MOVE   404                  TO  APILST35-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-PTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "***********************"
           DISPLAY   "* system01_manage end *"
           DISPLAY   "***********************"
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
           INITIALIZE                      XML-SYSTEMDAILYRES
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Patient Info"       TO  SYSDAYRES-RESKEY
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  SYSDAYRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  SYSDAYRES-INFORMATION-TIME
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
           PERFORM 1000-APTLSTREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           MOVE    ZERO                TO  SYSDAYRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD
      *
           MOVE    ZERO                TO  FLG-SYORI
      *
           EVALUATE    SYSDAYREQ-REQUEST-NUMBER
           WHEN    "01"
      *        システム管理設定
               MOVE    1                   TO  FLG-SYORI
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
           END-EVALUATE
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
      *    一覧処理
           IF      WRK-ERRCD           =   SPACE
               EVALUATE    SYSDAYREQ-REQUEST-NUMBER
               WHEN    "01"
      *            システム管理設定
                   PERFORM 2002-SYSKANRI-HEN-SEC
               END-EVALUATE
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-APTLSTREAD-SEC   SECTION.
      *
           IF      APILST35-BODY        NOT =   LOW-VALUE
               DISPLAY "SYS01REQ-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-SYSTEMDAILYREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_system01_dailyv2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST35-BODY        TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "system01_dailyreq" TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
               MOVE   "97"             TO  WRK-ERRCD
               GO     TO      1000-APTLSTREAD-EXT
           END-IF
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_system01_dailyv2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST35-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "system01_dailyreq" TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ          TO  XML-SYSTEMDAILYREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               MOVE   "98"             TO  WRK-ERRCD
               DISPLAY "XMLREAD failure = " MCP-RC
           END-IF
      *
           .
       1000-APTLSTREAD-EXT.
           EXIT.
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
           .
       10001-XML-REMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    設定内容チェック
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
           MOVE    SYSDAYREQ-BASE-DATE TO  WRK-IN-DATE
      *    処理日
           MOVE    SYSDAYREQ-BASE-DATE TO  WRK-HEN-DATE
           MOVE    SPACE               TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD            TO  WRK-IN-YMD
      *    開始日未設定はシステム日付
           IF      WRK-IN-DATE         =   SPACE
               MOVE    SPA-SYSYMD          TO  WRK-IN-YMD
               MOVE    WRK-IN-YMD          TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  WRK-IN-DATE
           END-IF
           MOVE    WRK-IN-DATE         TO  SYSDAYRES-BASE-DATE
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理設定情報処理
      *****************************************************************
       2002-SYSKANRI-HEN-SEC             SECTION.
      *
      *    患者登録情報
           PERFORM 20021-SYS1017-SET-SEC
      *
      *    診療行為情報
      *    1001 医療機関情報
           PERFORM 20022-SYS1001-SET-SEC
      *    1007 自動算定・チェック機能情報
           PERFORM 20023-SYS1007-SET-SEC
      *    1038 診療行為機能情報
           PERFORM 20024-SYS1038-SET-SEC
           .
       2002-SYSKANRI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者登録情報処理
      *****************************************************************
       20021-SYS1017-SET-SEC             SECTION.
      *
      *
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "1017"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-IN-YMD          TO  SYS-STYUKYMD
           MOVE    WRK-IN-YMD          TO  SYS-EDYUKYMD
      *    システム管理検索
           PERFORM 9001-SYSKANRI-KEYREAD-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC    TO  SYS-1017-REC
           ELSE
               INITIALIZE                  SYS-1017-REC
           END-IF
      *    二重登録疑い区分
           IF      SYS-1017-JYUFUKU-CHKKBN =   SPACE
               MOVE    "0"             TO  SYS-1017-JYUFUKU-CHKKBN
           END-IF
           MOVE    SYS-1017-JYUFUKU-CHKKBN
                                       TO  SYSDAYRES-JYUFUKU-CHKKBN
           EVALUATE    SYS-1017-JYUFUKU-CHKKBN
               WHEN    "0"
                   MOVE    "カナ氏名＋性別＋生年月日" 
                                       TO  SYSDAYRES-JYUFUKU-CHK-MEI
               WHEN    "1"
                   MOVE    "カナ氏名＋漢字氏名＋性別＋生年月日" 
                                       TO  SYSDAYRES-JYUFUKU-CHK-MEI
               WHEN    "2"
                   MOVE    "性別＋生年月日" 
                                       TO  SYSDAYRES-JYUFUKU-CHK-MEI
           END-EVALUATE
      *    郵便番号自動記載区分
           IF      SYS-1017-POSTKBN-FLG    =   SPACE
               MOVE    "1"             TO  SYS-1017-POSTKBN-FLG
           END-IF
           MOVE    SYS-1017-POSTKBN-FLG
                                       TO  SYSDAYRES-POSTKBN-FLG
           EVALUATE    SYS-1017-POSTKBN-FLG
               WHEN    "0"
                   MOVE    "自動記載しない" 
                                       TO  SYSDAYRES-POSTKBN-MEI
               WHEN    "1"
                   MOVE    "自動記載する" 
                                       TO  SYSDAYRES-POSTKBN-MEI
               WHEN    "2"
                   MOVE    "自動記載する（郵便番号未設定）" 
                                       TO  SYSDAYRES-POSTKBN-MEI
           END-EVALUATE
      *    都道府県名記載区分
           IF      SYS-1017-KENMEIKBN-FLG  =   SPACE
               MOVE    "1"             TO  SYS-1017-KENMEIKBN-FLG
           END-IF
           MOVE    SYS-1017-KENMEIKBN-FLG
                                       TO  SYSDAYRES-KENMEIKBN-FLG
           EVALUATE    SYS-1017-KENMEIKBN-FLG
               WHEN    "0"
                   MOVE    "記載しない" 
                                       TO  SYSDAYRES-KENMEIKBN-MEI
               WHEN    "1"
                   MOVE    "記載する" 
                                       TO  SYSDAYRES-KENMEIKBN-MEI
           END-EVALUATE
           .
       20021-SYS1017-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報情報処理
      *****************************************************************
       20022-SYS1001-SET-SEC             SECTION.
      *
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "1001"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-IN-YMD          TO  SYS-STYUKYMD
           MOVE    WRK-IN-YMD          TO  SYS-EDYUKYMD
      *    システム管理検索
           PERFORM 9001-SYSKANRI-KEYREAD-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC    TO  SYS-1001-REC
           ELSE
               INITIALIZE                  SYS-1001-REC
           END-IF
      *    処方区分
           MOVE    SYS-1001-SYOHOKBN   TO  SYSDAYRES-SYOHOKBN
           EVALUATE    SYS-1001-SYOHOKBN
               WHEN    "0"
                   MOVE    "院内"      TO  SYSDAYRES-SYOHOKBN-MEI
               WHEN    "1"
                   MOVE    "院外"      TO  SYSDAYRES-SYOHOKBN-MEI
           END-EVALUATE
      *    消費税端数設定
           MOVE    SYS-1001-TAX-HASU-KBN   TO  SYSDAYRES-TAX-HASU-KBN
           EVALUATE    SYS-1001-TAX-HASU-KBN
               WHEN    "1"
                   MOVE    "１円未満四捨五入"
                                           TO  SYSDAYRES-TAX-HASU-MEI
               WHEN    "3"
                   MOVE    "１円未満切り上げ"
                                           TO  SYSDAYRES-TAX-HASU-MEI
               WHEN    OTHER
                   MOVE    "2"             TO  SYSDAYRES-TAX-HASU-KBN
                   MOVE    "１円未満切り捨て"
                                           TO  SYSDAYRES-TAX-HASU-MEI
           END-EVALUATE
      *    自費コード端数設定
           IF      WRK-IN-YMD          <   "20170901"
               MOVE    "2"                 TO  SYS-1001-JIHI-HASU-KBN
           END-IF
           MOVE    SYS-1001-JIHI-HASU-KBN  TO  SYSDAYRES-JIHI-HASU-KBN
           EVALUATE    SYS-1001-JIHI-HASU-KBN
               WHEN    "1"
                   MOVE    "１円未満四捨五入"
                                           TO  SYSDAYRES-JIHI-HASU-MEI
               WHEN    "3"
                   MOVE    "１円未満切り上げ"
                                           TO  SYSDAYRES-JIHI-HASU-MEI
               WHEN    OTHER
      *            （端数切り捨てが初期）
                   MOVE    "2"             TO  SYSDAYRES-JIHI-HASU-KBN
                   MOVE    "１円未満切り捨て"
                                           TO  SYSDAYRES-JIHI-HASU-MEI
           END-EVALUATE
           .
       20022-SYS1001-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動算定・チェック機能情報処理
      *****************************************************************
       20023-SYS1007-SET-SEC             SECTION.
      *
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "1007"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-IN-YMD          TO  SYS-STYUKYMD
           MOVE    WRK-IN-YMD          TO  SYS-EDYUKYMD
      *    システム管理検索
           PERFORM 9001-SYSKANRI-KEYREAD-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC    TO  SYS-1007-REC
           ELSE
               INITIALIZE                  SYS-1007-REC
           END-IF
      *    調剤技術基本料
           IF      SYS-1007-SANTEI (1:1)   =   "1"
               MOVE    "1"                 TO  SYSDAYRES-CHOKI-FLG
               MOVE    "算定する"          TO  SYSDAYRES-CHOKI-MEI
           ELSE
               MOVE    "0"                 TO  SYSDAYRES-CHOKI-FLG
               MOVE    "算定しない"        TO  SYSDAYRES-CHOKI-MEI
           END-IF
      *    薬剤情報提供料（一般）
           EVALUATE    SYS-1007-SANTEI (2:1)
           WHEN   "1"
               MOVE    "1"                 TO  SYSDAYRES-YAKJYO1-FLG
               MOVE    "算定する（月１回）"
                                           TO  SYSDAYRES-YAKJYO1-MEI
           WHEN   "2"
               MOVE    "2"                 TO  SYSDAYRES-YAKJYO1-FLG
               MOVE    "算定する（毎回）"  TO  SYSDAYRES-YAKJYO1-MEI
           WHEN    OTHER
               MOVE    "0"                 TO  SYSDAYRES-YAKJYO1-FLG
               MOVE    "算定しない"        TO  SYSDAYRES-YAKJYO1-MEI
           END-EVALUATE
      *    薬剤情報提供料（老人）
      *    （どちらかにのみ設定がある）
           IF      SYS-1007-SANTEI (3:1)   =   SPACE
                                           OR  "0"
               IF      SYS-1007-SANTEI (4:1)   =   SPACE
                                               OR  "0"
                   MOVE    "0"             TO  SYSDAYRES-YAKJYO2-FLG
               ELSE
                   MOVE    SYS-1007-SANTEI (4:1)
                                           TO  SYSDAYRES-YAKJYO2-FLG
               END-IF
           ELSE
               MOVE    SYS-1007-SANTEI (3:1)
                                           TO  SYSDAYRES-YAKJYO2-FLG
           END-IF
           EVALUATE    SYSDAYRES-YAKJYO2-FLG
           WHEN    "0"
               MOVE    "算定しない"        TO  SYSDAYRES-YAKJYO2-MEI
           WHEN   "1"
               MOVE    "算定する（月１回）"
                                           TO  SYSDAYRES-YAKJYO2-MEI
           WHEN   "2"
               MOVE    "算定する（毎回）"  TO  SYSDAYRES-YAKJYO2-MEI
           END-EVALUATE
      *    療養担当手当
           IF      SYS-1007-SANTEI (5:1)   =   "1"
               MOVE    "1"                 TO  SYSDAYRES-RYOYOU-FLG
               MOVE    "算定する"          TO  SYSDAYRES-RYOYOU-MEI
           ELSE
               MOVE    "0"                 TO  SYSDAYRES-RYOYOU-FLG
               MOVE    "算定しない"        TO  SYSDAYRES-RYOYOU-MEI
           END-IF
      *    特定薬剤治療管理料有無
           IF      SYS-1007-SANTEI (6:1)   =   "1"
               MOVE    "1"                 TO  SYSDAYRES-TOKUTEI-FLG
               MOVE    "算定する"          TO  SYSDAYRES-TOKUTEI-MEI
           ELSE
               MOVE    "0"                 TO  SYSDAYRES-TOKUTEI-FLG
               MOVE    "算定しない"        TO  SYSDAYRES-TOKUTEI-MEI
           END-IF
      *
      *    システム管理検索（チェック機能制御情報）
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "1008"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-IN-YMD          TO  SYS-STYUKYMD
           MOVE    WRK-IN-YMD          TO  SYS-EDYUKYMD
      *    システム管理検索
           PERFORM 9001-SYSKANRI-KEYREAD-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC    TO  SYS-1008-REC
           ELSE
               INITIALIZE                  SYS-1008-REC
           END-IF
      *
      *    外来管理加算チェック
           IF      SYS-1008-GAIKANRIKBN-CHK    =   SPACE
               MOVE    "1"             TO  SYS-1008-GAIKANRIKBN-CHK
           END-IF
           MOVE    SYS-1008-GAIKANRIKBN-CHK
                                       TO  SYSDAYRES-GAIKANRIKBN-CHK
           EVALUATE    SYS-1008-GAIKANRIKBN-CHK
           WHEN    "0"
               MOVE    "チェックなし"      TO  SYSDAYRES-GAIKANRIKBN-MEI
           WHEN    "1"
               MOVE    "チェックあり"      TO  SYSDAYRES-GAIKANRIKBN-MEI
           WHEN    "2"
               MOVE    "削除後の自動発生なし"
                                           TO  SYSDAYRES-GAIKANRIKBN-MEI
           WHEN    "3"
               MOVE    "自動発生なし"      TO  SYSDAYRES-GAIKANRIKBN-MEI
           WHEN    "4"
               MOVE    "削除後の自動発生・確認なし"
                                           TO  SYSDAYRES-GAIKANRIKBN-MEI
           END-EVALUATE
      *
      *    育児栄養加算自動算定
           IF      SYS-1008-IKUJIEIYO-FLG  =   SPACE
               MOVE    "0"                 TO  SYS-1008-IKUJIEIYO-FLG
           END-IF
           MOVE    SYS-1008-IKUJIEIYO-FLG
                                           TO  SYSDAYRES-IKUJIEIYO-FLG
           EVALUATE    SYS-1008-IKUJIEIYO-FLG
           WHEN    "0"
               MOVE    "算定しない"        TO  SYSDAYRES-IKUJIEIYO-MEI
           WHEN    "1"
               MOVE    "算定する"          TO  SYSDAYRES-IKUJIEIYO-MEI
           END-EVALUATE
      *    育児栄養加算自動算定科
           MOVE    SYS-1008-IKUJIEIYO-SRYKA
                                       TO  SYSDAYRES-IKUJIEIYO-SRYKA
           IF      SYS-1008-IKUJIEIYO-SRYKA    =   "00"
               MOVE    "全科"          TO  SYSDAYRES-IKUJIEIYO-KAMEI
           ELSE
      *        診療科名
               MOVE    SPACE               TO  SYSKANRI-REC 
               INITIALIZE                      SYSKANRI-REC
               MOVE    "1005"              TO  SYS-KANRICD
               MOVE    SYS-1008-IKUJIEIYO-SRYKA    TO  SYS-KBNCD
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    WRK-IN-YMD          TO  SYS-STYUKYMD
               MOVE    WRK-IN-YMD          TO  SYS-EDYUKYMD
      *       システム管理検索
               PERFORM 9001-SYSKANRI-KEYREAD-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    SYSKANRI-REC        TO  SYS-1005-REC
                   MOVE    SYS-1005-SRYKANAME1
                                           TO  SYSDAYRES-IKUJIEIYO-KAMEI
               END-IF
           END-IF
      *
      *    時間外加算（特例乳幼児）
           IF      SYS-1008-JIKANTOKU-NYU-FLG  =   SPACE
               MOVE    "0"             TO  SYS-1008-JIKANTOKU-NYU-FLG
           END-IF
           MOVE    SYS-1008-JIKANTOKU-NYU-FLG
                                       TO  SYSDAYRES-JIKANTOKU-NYU-FLG
           EVALUATE    SYS-1008-JIKANTOKU-NYU-FLG
           WHEN    "0"
               MOVE    "算定しない"    TO  SYSDAYRES-JIKANTOKU-NYU-MEI
           WHEN    "1"
               MOVE    "算定する"      TO  SYSDAYRES-JIKANTOKU-NYU-MEI
           END-EVALUATE
      *
      *    画像診断管理加算１（写真診断）
           IF      SYS-1008-GAZOUKSN1-FLG =   SPACE
               MOVE    "1"                 TO  SYS-1008-GAZOUKSN1-FLG
           END-IF
           MOVE    SYS-1008-GAZOUKSN1-FLG  TO  SYSDAYRES-GAZOUKSN1-FLG
           EVALUATE    SYS-1008-GAZOUKSN1-FLG
           WHEN    "0"
               MOVE    "算定しない"        TO  SYSDAYRES-GAZOUKSN1-MEI
           WHEN    "1"
               MOVE    "算定する"          TO  SYSDAYRES-GAZOUKSN1-MEI
           END-EVALUATE
      *
      *    最終来院日からの期間
           IF      SYS-1008-LAST-KIKAN-X   =   SPACE
               MOVE    ZERO                TO  SYSDAYRES-LAST-KIKAN
           ELSE
               MOVE    SYS-1008-LAST-KIKAN
                                           TO  SYSDAYRES-LAST-KIKAN
           END-IF
      *
      *    薬剤情報提供料チェック
           IF      SYS-1008-YAKJYOCHK-FLG =   SPACE
               MOVE    "1"                 TO  SYS-1008-YAKJYOCHK-FLG
           END-IF
           MOVE    SYS-1008-YAKJYOCHK-FLG TO  SYSDAYRES-YAKJYOCHK-FLG
           EVALUATE    SYS-1008-YAKJYOCHK-FLG
           WHEN    "0"
               MOVE    "チェックしない"   TO  SYSDAYRES-YAKJYOCHK-MEI
           WHEN    "1"
               MOVE    "チェックする"     TO  SYSDAYRES-YAKJYOCHK-MEI
           END-EVALUATE
      *    訂正時の自動発生
      *    外来
           IF      SYS-1008-TEISEIAUTO-FLG =   SPACE
               MOVE    "1"                 TO  SYS-1008-TEISEIAUTO-FLG
           END-IF
           MOVE    SYS-1008-TEISEIAUTO-FLG TO  SYSDAYRES-TEISEIAUTO-FLG
           EVALUATE    SYS-1008-TEISEIAUTO-FLG
           WHEN    "0"
               MOVE    "算定しない"        TO  SYSDAYRES-TEISEIAUTO-MEI
           WHEN    "1"
               MOVE    "算定する"          TO  SYSDAYRES-TEISEIAUTO-MEI
           END-EVALUATE
      *
      *    訂正時の自動発生（特定疾患処方管理）
           IF      SYS-1008-TEISEITOKU-FLG =   SPACE
               MOVE    "0"                 TO  SYS-1008-TEISEITOKU-FLG
           END-IF
           MOVE    SYS-1008-TEISEITOKU-FLG TO  SYSDAYRES-TEISEITOKU-FLG
           EVALUATE    SYS-1008-TEISEITOKU-FLG
           WHEN    "0"
               MOVE    "算定しない"        TO  SYSDAYRES-TEISEITOKU-MEI
           WHEN    "1"
               MOVE    "算定する"          TO  SYSDAYRES-TEISEITOKU-MEI
           END-EVALUATE
      *
      *    検体検査加算自動発生
           IF      SYS-1008-KENSAAUTO-FLG   =   SPACE
               MOVE    "0"                 TO  SYS-1008-KENSAAUTO-FLG
           END-IF
           MOVE    SYS-1008-KENSAAUTO-FLG  TO  SYSDAYRES-KENSAAUTO-FLG
           EVALUATE    SYS-1008-KENSAAUTO-FLG
           WHEN    "0"
               MOVE    "算定しない"        TO  SYSDAYRES-KENSAAUTO-MEI
           WHEN    "1"
               MOVE    "算定する"          TO  SYSDAYRES-KENSAAUTO-MEI
           WHEN    "2"
               MOVE    "算定する（設定）"  TO  SYSDAYRES-KENSAAUTO-MEI
           END-EVALUATE
      *
      *    前回保険組合せ相違チェック
           IF      SYS-1008-ZENHKNCHK-FLG   =   SPACE
              MOVE    "0"                  TO  SYS-1008-ZENHKNCHK-FLG
           END-IF
           MOVE    SYS-1008-ZENHKNCHK-FLG  TO  SYSDAYRES-ZENHKNCHK-FLG
           EVALUATE    SYS-1008-ZENHKNCHK-FLG
           WHEN    "0"
               MOVE    "チェックしない"    TO  SYSDAYRES-ZENHKNCHK-MEI
           WHEN    "1"
               MOVE    "チェックする"      TO  SYSDAYRES-ZENHKNCHK-MEI
           WHEN    "2"
               MOVE    "チェックする（すべて）"
                                           TO  SYSDAYRES-ZENHKNCHK-MEI
           END-EVALUATE
      *
      *    病名疾患区分からの自動発生
           IF      SYS-1008-BYOMEIAUTO-FLG =   SPACE
      *        未設定は算定する
               MOVE    "1"                 TO  SYS-1008-BYOMEIAUTO-FLG
           END-IF
           MOVE    SYS-1008-BYOMEIAUTO-FLG TO  SYSDAYRES-BYOMEIAUTO-FLG
           EVALUATE    SYS-1008-BYOMEIAUTO-FLG
           WHEN    "0"
               MOVE    "算定しない"        TO  SYSDAYRES-BYOMEIAUTO-MEI
           WHEN    "1"
               MOVE    "算定する"          TO  SYSDAYRES-BYOMEIAUTO-MEI
           END-EVALUATE
      *    診察料自動発生
           IF      SYS-1008-INITAUTO-FLG   =   SPACE
               MOVE    "1"                 TO  SYS-1008-INITAUTO-FLG
           END-IF
           MOVE    SYS-1008-INITAUTO-FLG   TO  SYSDAYRES-INITAUTO-FLG
           EVALUATE    SYS-1008-INITAUTO-FLG
           WHEN    "0"
               MOVE    "算定しない"        TO  SYSDAYRES-INITAUTO-MEI
           WHEN    "1"
               MOVE    "算定する"          TO  SYSDAYRES-INITAUTO-MEI
           END-EVALUATE
      *    手帳加算確認チェック
           IF      SYS-1008-TECHOKBN-FLG   =   SPACE
               MOVE    "0"                 TO  SYS-1008-TECHOKBN-FLG
           END-IF
           MOVE    SYS-1008-TECHOKBN-FLG   TO  SYSDAYRES-TECHOKBN-FLG
           EVALUATE    SYS-1008-TECHOKBN-FLG
           WHEN    "0"
               MOVE    "チェックしない"    TO  SYSDAYRES-TECHOKBN-MEI
           WHEN    "1"
               MOVE    "チェックする"      TO  SYSDAYRES-TECHOKBN-MEI
           END-EVALUATE
      *    時間外緊急院内チェック
           IF      SYS-1008-JGAIKEIKBN-FLG =   SPACE
               MOVE    "1"                 TO  SYS-1008-JGAIKEIKBN-FLG
           END-IF
           MOVE    SYS-1008-JGAIKEIKBN-FLG TO  SYSDAYRES-JGAIKEIKBN-FLG
           EVALUATE    SYS-1008-JGAIKEIKBN-FLG
           WHEN    "0"
               MOVE    "チェックしない"    TO  SYSDAYRES-JGAIKEIKBN-MEI
           WHEN    "1"
               MOVE    "チェックする"      TO  SYSDAYRES-JGAIKEIKBN-MEI
           END-EVALUATE
      *
      *    薬剤投与量チェック
           IF      SYS-1008-TOUYOCHK-FLG   =   SPACE
               MOVE    "0"                 TO  SYS-1008-TOUYOCHK-FLG
           END-IF
           MOVE    SYS-1008-TOUYOCHK-FLG   TO  SYSDAYRES-TOUYOCHK-FLG
           EVALUATE    SYS-1008-TOUYOCHK-FLG
           WHEN    "0"
               MOVE    "在宅・投薬・注射を対象"
                                           TO  SYSDAYRES-TOUYOCHK-MEI
           WHEN    "1"
               MOVE    "すべてを対象"      TO  SYSDAYRES-TOUYOCHK-MEI
           END-EVALUATE
      *
      *    投薬３０日超チェック
           IF      SYS-1008-SHOHOU30CHK    =   SPACE
               MOVE    "0"                 TO  SYS-1008-SHOHOU30CHK
           END-IF
           MOVE    SYS-1008-SHOHOU30CHK    TO  SYSDAYRES-SHOHOU30CHK
           EVALUATE    SYS-1008-SHOHOU30CHK
           WHEN    "0"
               MOVE    "チェックしない"    TO  SYSDAYRES-SHOHOU30CHK-MEI
           WHEN    "1"
               MOVE    "チェックする"      TO  SYSDAYRES-SHOHOU30CHK-MEI
           END-EVALUATE
           .
       20023-SYS1007-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為機能情報処理
      *****************************************************************
       20024-SYS1038-SET-SEC             SECTION.
      *
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "1038"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-IN-YMD          TO  SYS-STYUKYMD
           MOVE    WRK-IN-YMD          TO  SYS-EDYUKYMD
      *    システム管理検索
           PERFORM 9001-SYSKANRI-KEYREAD-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC    TO  SYS-1038-REC
           ELSE
               INITIALIZE                  SYS-1038-REC
           END-IF
      *
      *    皮下筋肉注射料変換選択
           IF      SYS-1038-CHG310FLG  =   SPACE
               MOVE    "0"                 TO  SYS-1038-CHG310FLG
           END-IF
           MOVE    SYS-1038-CHG310FLG      TO  SYSDAYRES-CHG310FLG
           EVALUATE    SYS-1038-CHG310FLG
           WHEN    "0"
               MOVE    "変換する"          TO  SYSDAYRES-CHG310FLG-MEI
           WHEN    "1"
               MOVE    "変換しない"        TO  SYSDAYRES-CHG310FLG-MEI
           END-EVALUATE
      *
      *    投薬診療区分変換選択
           IF      SYS-1038-CHGSRYSYU-FLG  =   SPACE
               MOVE    "0"                 TO  SYS-1038-CHGSRYSYU-FLG
           END-IF
           MOVE    SYS-1038-CHGSRYSYU-FLG  TO  SYSDAYRES-CHGSRYSYU-FLG
           EVALUATE    SYS-1038-CHGSRYSYU-FLG
           WHEN    "0"
               MOVE    "変換する"          TO  SYSDAYRES-CHGSRYSYU-MEI
           WHEN    "1"
               MOVE    "変換しない"        TO  SYSDAYRES-CHGSRYSYU-MEI
           END-EVALUATE
      *
      *    残量廃棄区分
           IF      SYS-1038-HAIKIKBN    =   SPACE
               MOVE    "1"                 TO  SYS-1038-HAIKIKBN
           END-IF
           MOVE    SYS-1038-HAIKIKBN       TO  SYSDAYRES-HAIKIKBN
           EVALUATE    SYS-1038-HAIKIKBN
           WHEN    "1"
               MOVE    "注射のみ"          TO  SYSDAYRES-HAIKIKBN-MEI
           WHEN    "2"
               MOVE    "すべての診療区分"  TO  SYSDAYRES-HAIKIKBN-MEI
           END-EVALUATE
      *
      *    精神療法２０未満加算区分
           IF      SYS-1038-SEI20KSNKBN    =   SPACE
               MOVE    "0"                 TO  SYS-1038-SEI20KSNKBN
           END-IF
           MOVE    SYS-1038-SEI20KSNKBN    TO  SYSDAYRES-SEI20KSNKBN
           EVALUATE    SYS-1038-SEI20KSNKBN
           WHEN    "0"
               MOVE    "自動算定する"      TO  SYSDAYRES-SEI20KSNKBN-MEI
           WHEN    "1"
               MOVE    "自動算定しない"    TO  SYSDAYRES-SEI20KSNKBN-MEI
           END-EVALUATE
      *
      *    心身療法２０未満加算区分
           IF      SYS-1038-SIN20KSNKBN    =   SPACE
               MOVE    "0"                 TO  SYS-1038-SIN20KSNKBN
           END-IF
           MOVE    SYS-1038-SIN20KSNKBN    TO  SYSDAYRES-SIN20KSNKBN
           EVALUATE    SYS-1038-SIN20KSNKBN
           WHEN    "0"
               MOVE    "自動算定する"      TO  SYSDAYRES-SIN20KSNKBN-MEI
           WHEN    "1"
               MOVE    "自動算定しない"    TO  SYSDAYRES-SIN20KSNKBN-MEI
           END-EVALUATE
      *
      *    セット展開区分
           IF      SYS-1038-SETALLKBN      =   SPACE
               MOVE    "0"                 TO  SYS-1038-SETALLKBN
           END-IF
           MOVE    SYS-1038-SETALLKBN      TO  SYSDAYRES-SETALLKBN
           EVALUATE    SYS-1038-SETALLKBN
           WHEN    "0"
               MOVE    "通常展開"          TO  SYSDAYRES-SETALLKBN-MEI
           WHEN    "1"
               MOVE    "全展開"            TO  SYSDAYRES-SETALLKBN-MEI
           END-EVALUATE
      *
      *
      *    入金の取扱い
           IF      SYS-1038-NYKNKNRKBN     =   SPACE
               MOVE    "1"                 TO  SYS-1038-NYKNKNRKBN
           END-IF
           MOVE    SYS-1038-NYKNKNRKBN     TO  SYSDAYRES-NYKNKNRKBN
           EVALUATE    SYS-1038-NYKNKNRKBN
           WHEN    "1"
               MOVE    "今回請求分のみ入金"
                                           TO  SYSDAYRES-NYKNKNRKBN-MEI
           WHEN    "2"
               MOVE    "今回分・伝票の古い未収順に入金"
                                           TO  SYSDAYRES-NYKNKNRKBN-MEI
           WHEN    "3"
               MOVE    "今回分・伝票の新しい未収順に入金"
                                           TO  SYSDAYRES-NYKNKNRKBN-MEI
           WHEN    "4"
               MOVE    "伝票の古い未収順に入金"
                                           TO  SYSDAYRES-NYKNKNRKBN-MEI
           WHEN    "5"
               MOVE    "伝票の新しい未収順に入金"
                                           TO  SYSDAYRES-NYKNKNRKBN-MEI
           END-EVALUATE
      *
      *    訂正の請求書金額
           IF      SYS-1038-TEIKINKBN      =   SPACE
               MOVE    "0"                 TO  SYS-1038-TEIKINKBN
           END-IF
           MOVE    SYS-1038-TEIKINKBN      TO  SYSDAYRES-TEIKINKBN
           EVALUATE    SYS-1038-TEIKINKBN
           WHEN    "0"
               MOVE    "訂正分（差額）"    TO  SYSDAYRES-TEIKINKBN-MEI
           WHEN    "1"
               MOVE    "合計"              TO  SYSDAYRES-TEIKINKBN-MEI
           END-EVALUATE
      *    入金・返金額設定区分
           IF      SYS-1038-NYKHNKKBN      =   SPACE
               MOVE    "0"                 TO  SYS-1038-NYKHNKKBN
           END-IF
           MOVE    SYS-1038-NYKHNKKBN      TO  SYSDAYRES-NYKHNKKBN
           EVALUATE    SYS-1038-NYKHNKKBN
           WHEN    "0"
               MOVE    "今回請求額のみ"    TO  SYSDAYRES-NYKHNKKBN-MEI
           WHEN    "1"
               MOVE    "一括入金反映"      TO  SYSDAYRES-NYKHNKKBN-MEI
           WHEN    "2"
               MOVE    "一括返金反映"      TO  SYSDAYRES-NYKHNKKBN-MEI
           WHEN    "3"
               MOVE    "一括入金・返金反映"
                                           TO  SYSDAYRES-NYKHNKKBN-MEI
           END-EVALUATE
           .
       20024-SYS1038-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-PTXMLMAKE-SEC               SECTION.
      *
           EVALUATE    FLG-SYORI
               WHEN    1
               WHEN    0
      *            システム管理設定
                   PERFORM 3001-SYSTEMDAY01-MAKE-SEC
           END-EVALUATE
      *
           .
       300-PTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理（システム管理設定）
      *****************************************************************
       3001-SYSTEMDAY01-MAKE-SEC       SECTION.
      *
      *    エラーメッセージ編集
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-KEICD           TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               MOVE    "処理終了"          TO  SYSDAYRES-API-RESULT-MSG
           ELSE
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD           TO  SYSDAYRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  SYSDAYRES-API-RESULT-MSG
           END-IF
      *
           IF      APILST35-BODY     NOT =   LOW-VALUE
               DISPLAY "SYSDAYRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_system01_dailyv2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  SYSDAYRES-MODE
           MOVE    "system01_dailyres" TO  SYSDAYRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-SYSTEMDAILYRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_system01_dailyv2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  SYSDAYRES-MODE
           MOVE    "system01_dailyres" TO  SYSDAYRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-SYSTEMDAILYRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               DISPLAY "XMLWRITE OK      = " MCP-RC
               MOVE    SYSDAYRES-OBJECT      TO  APILST35-BODY 
               MOVE    "application/xml"   TO  APILST35-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       3001-SYSTEMDAY01-MAKE-EXT.
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
               MOVE    "リクエスト番号がありません"
                                               TO  WRK-ERRMSG
           WHEN    "97"
               MOVE    "送信内容に誤りがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "98"
               MOVE    "送信内容の読込ができませんでした"
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
      *    システム管理読込処理
      *****************************************************************
       9001-SYSKANRI-KEYREAD-SEC            SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       9001-SYSKANRI-KEYREAD-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    システム管理読込処理
      *****************************************************************
       900-SYSKANRI-READ-SEC            SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC         TO  SYSKANRI-REC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
               INITIALIZE                      SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
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
