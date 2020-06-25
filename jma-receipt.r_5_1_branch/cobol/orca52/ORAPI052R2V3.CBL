      ******************************************************************
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
      ******************************************************************
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORAPI052R2V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 点数マスタ返却  (xml2)
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/06/XX    NACL−藤原　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.01    NACL-藤原    18/09/05  ＣＳＶ対応
      *                                     抽出条件の追加
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
      *    エラーファイル 名称領域
           COPY    "CPERRFL.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
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
           03  WRK-ERRCD               PIC X(03).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-KEICD               PIC X(03).
      *
           03  WRK-TEMPDIR-ROOT        PIC X(1024).
      *
           03  WRK-UPDCNT-X.
               05  WRK-UPDCNT          PIC 9(10).
      *
           03  WRK-MONBLOB-ID          PIC X(37).
      *
       01  WRK-LNK-AREA.
           COPY    "CPORCSPRTLNK.INC".
      *
       01  WRK-XML-AREA.
           03  WRK-BASE-DATE           PIC X(10).
           03  WRK-BASE-YMD            PIC X(08).
      *
       01  WRK-CONS-AREA.
      *    システム管理ＤＢ（３００３）のキー値  
           03  WRK-CONS-3003-KBNCD     PIC X(08)   VALUE   "052".
      *    ジョブ管理ＤＢの値
           03  WRK-CONS-JOB-SHELLID    PIC X(08)   VALUE   "MONTHLY".
           03  WRK-CONS-JOB-JOBID      PIC 9(07)   VALUE   1.
      *
      *    印刷管理ＤＢの値  
           03  WRK-CONS-PRTKANRI-TBL-KEY
                                       PIC X(08)   VALUE   "MONTHLY".
           03  WRK-CONS-PRTKANRI-TBL-GROUP
                                       PIC X(14)   VALUE   SPACE.
      *
      *    エラーファイルＩＤ
           03  WRK-CONS-FILE-ID        PIC X(08)   VALUE   "OR052ERR".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
           COPY    "CPORCSCOMMON.INC".
      *
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
           COPY    "CPMEDICATIONMASTERLSTV3REQ.INC".
      *    API XML書き込み用定義体
           COPY    "CPMEDICATIONMASTERLSTV3RES.INC".
      *
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *    一時ディレクトリ作成作成
           COPY    "CPORCSTEMPDIR.INC".
      *
           COPY    "MONBLOB".
      *
      *    ＵＩＤ取得用エリア
       01  UIDIO-AREA.
           03  C-RET               PIC X(2).
           03  C-UID               PIC X(36).
       01  ST                      PIC 9(2).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
      *    （職員情報）
           COPY    "CPSK1010.INC".
      *    （医療機関情報）
      *    COPY    "CPSK1001.INC".
      *
           COPY    "CPSK3003.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "ORCA-BLOB".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA51SCRAREA.INC".
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
           DISPLAY   "*****************************"
           DISPLAY   "* medicationmasterlst start *"
           DISPLAY   "*****************************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   "E99"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST02-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-APTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "****************************"
           DISPLAY   "* medicationmasterlst end  *"
           DISPLAY   "****************************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      WRK-XML-AREA
           INITIALIZE                      XML-LSTRES
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "medication master"  TO  LSTRES-RESKEY
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  LSTRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  LSTRES-INFORMATION-TIME
      *
      *    医療機関識別番号セット 
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "E99"                TO  WRK-ERRCD
               GO  TO 100-INIT-EXT
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
               MOVE   "E89"                TO  WRK-ERRCD
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    XML情報取り出し
           PERFORM 1000-APTXMLREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE   LSTREQ-REQUEST-NUMBER
           WHEN    "01"
               MOVE    1                   TO  FLG-SYORI
           WHEN    "02"
               MOVE    2                   TO  FLG-SYORI
           WHEN    OTHER
               MOVE    "E91"               TO  WRK-ERRCD
           END-EVALUATE
      *
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           EVALUATE   LSTREQ-REQUEST-NUMBER
           WHEN    "01"
      *        処理実施
               PERFORM 2002-SYORI-01-SEC
           WHEN    "02"
      *        結果情報返却
               PERFORM 2003-SYORI-02-SEC
           END-EVALUATE
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-APTXMLREAD-SEC   SECTION.
      *
           IF      APILST02-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-LSTREQ
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_medication_masterlstv3req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST02-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medication_masterlstv3req"
                                       TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ          TO  XML-LSTREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "E98"             TO  WRK-ERRCD
           END-IF
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
               MOVE    "02"                TO  LSTRES-RESPONSE-NUMBER
               MOVE    ZERO                TO  LSTRES-API-RESULT
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "処理実施終了"
                                           TO  LSTRES-API-RESULT-MSG
                   WHEN    2
                       MOVE    "処理確認終了"
                                           TO  LSTRES-API-RESULT-MSG
               END-EVALUATE
           ELSE
               MOVE    "01"                TO  LSTRES-RESPONSE-NUMBER
               IF      WRK-ERRMSG          =   SPACE
      *        エラー・警告メッセージ
                   PERFORM 890-ERRCD-MSG-SEC
               END-IF
               MOVE    WRK-ERRCD           TO  LSTRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  LSTRES-API-RESULT-MSG
           END-IF
      *
           MOVE   LSTREQ-KARTE-UID     TO  LSTRES-KARTE-UID
           IF      FLG-SYORI           =   1
               IF      WRK-ERRCD           =   SPACE
                   MOVE   LSTREQ-ORCA-UID      TO  LSTRES-ORCA-UID
               END-IF
               MOVE    WRK-MONBLOB-ID          TO  LSTRES-DATA-ID (1)
           ELSE
               MOVE   LSTREQ-ORCA-UID  TO  LSTRES-ORCA-UID
           END-IF
      *
           PERFORM 3001-LSTRES-SEC
           .
       300-APTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       3001-LSTRES-SEC        SECTION.
      *
           IF      APILST02-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_medication_masterlstv3res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  LSTRES-MODE
           MOVE    "medication_masterlstv3res"
                                       TO  LSTRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-LSTRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    LSTRES-OBJECT       TO  APILST02-BODY 
               MOVE    "application/xml"   TO  APILST02-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       3001-LSTRES-EXT.
           EXIT.
      *
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
      *
           .
       10001-XML-REMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報設定内容チェック
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
      *    編集処理
           PERFORM 20010-INPUT-HEN-SEC
      *
      *    カルテUID
           IF     LSTREQ-KARTE-UID     =   SPACE
               MOVE    "E04"               TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    オルカUID
           IF     LSTREQ-REQUEST-NUMBER
                                       =   "01"
               CONTINUE
           ELSE
               IF     LSTREQ-ORCA-UID      =   SPACE
                   MOVE    "E05"               TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF
           END-IF
      *
      *    基準日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PERFORM-CHK-SEC
           END-IF
      *
      *    データフォーマットチェック
           IF      WRK-ERRCD           =   SPACE
               EVALUATE    LSTREQ-DATA-FORMAT
                   WHEN    SPACE
                       MOVE    "xml"           TO  LSTREQ-DATA-FORMAT
                   WHEN    "xml"
                   WHEN    "csv"
                       CONTINUE
                   WHEN    OTHER
                       MOVE    "E18"           TO  WRK-ERRCD
               END-EVALUATE
           END-IF
      *
      *    有効モードチェック
           IF      WRK-ERRCD           =   SPACE
               EVALUATE    LSTREQ-EFFECTIVE-MODE
                   WHEN    SPACE
                   WHEN    "True"
                   WHEN    "False"
                       CONTINUE
                   WHEN    OTHER
                       MOVE    "E19"           TO  WRK-ERRCD
               END-EVALUATE
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目変換編集処理
      *****************************************************************
       20010-INPUT-HEN-SEC             SECTION.
      *
      *    基準日
           MOVE   LSTREQ-BASE-DATE     TO  LSTRES-BASE-DATE
      *
           MOVE   LSTREQ-BASE-DATE     TO  WRK-HEN-DATE
           MOVE    SPACE               TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD            TO  WRK-BASE-YMD
      *
           MOVE   LSTREQ-REQUEST-NUMBER
                                       TO  LSTRES-REQUEST-NUMBER
      *
           EVALUATE    LSTREQ-DATA-FORMAT
               WHEN    SPACE
                   MOVE    "xml"           TO  LSTREQ-DATA-FORMAT
           END-EVALUATE
           MOVE   LSTREQ-DATA-FORMAT   TO  LSTRES-DATA-FORMAT
      *
           MOVE   LSTREQ-EFFECTIVE-MODE
                                       TO  LSTRES-EFFECTIVE-MODE
      *
           .
       20010-INPUT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    基準日チェック
      *****************************************************************
       20011-PERFORM-CHK-SEC           SECTION.
      *
      *    日付チェックサブ
           IF      WRK-BASE-YMD        =   SPACE
               CONTINUE
           ELSE
               MOVE    SPACE               TO  STS-AREA-DAY
               INITIALIZE                      STS-AREA-DAY
               MOVE    SPACE               TO  LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-BASE-YMD        TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY2-AREA
               IF      STS-DAY-RC1     NOT =   ZERO
                   MOVE    "E11"               TO  WRK-ERRCD
               END-IF
           END-IF
      *
           .
       20011-PERFORM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理実施処理
      *****************************************************************
       2002-SYORI-01-SEC           SECTION.
      *
      *    ジョブ管理チェック処理
           PERFORM 800-JOBKANRI-CHK-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  2002-SYORI-01-EXT
           END-IF
      *
      *    UIDコード取得処理
           PERFORM 20021-UID-GET-SEC
           MOVE    C-UID           TO  LSTREQ-ORCA-UID
      *
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"           TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
      *
      *    一時ディレクトリのルートディレクトリ取得
           ACCEPT  WRK-TEMPDIR-ROOT
                               FROM    ENVIRONMENT "MCP_TEMPDIR_ROOT"
      *    一時ディレクトリ名組み立て
           STRING  WRK-TEMPDIR-ROOT    DELIMITED   BY  SPACE
                   "/"                 DELIMITED   BY  SIZE
                   SPA-TERMID          DELIMITED   BY  SPACE
                   LOW-VALUE           DELIMITED   BY  SIZE
                                       INTO        MCP-TEMPDIR
           END-STRING
      *
      *    環境変数［MCP_TEMPDIR］を生成（MCP-TEMPDIRの値を設定）
           SET ENVIRONMENT "MCP_TEMPDIR"
                                   TO  MCP-TEMPDIR
      *    環境変数［ORCA_UID］を生成（SPA-TERMID値を設定）
           SET ENVIRONMENT "ORCA_UID"
                                   TO  SPA-TERMID
      *
      *    一時ディレクトリ作成
           CALL    "CBL_CREATE_DIR"    USING   MCP-TEMPDIR
           IF    ( RETURN-CODE     NOT =   ZERO )
               MOVE    "E80"               TO  WRK-ERRCD
               GO  TO  2002-SYORI-01-EXT
           END-IF
      *
      *    Ｄａｔａ＿Ｉｄ取得
           MOVE    "MONBLOBNEW"        TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "monblob"           TO  MCP-TABLE
           CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MONBLOB
           IF      MCP-RC          NOT =   ZERO
               MOVE    "E88"               TO  WRK-ERRCD
               GO  TO  2002-SYORI-01-EXT
           END-IF
           MOVE    MONBLOB-ID          TO  WRK-MONBLOB-ID
      *
      *    マシン日付取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    ＪＯＢＩＤ削除処理
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   CONTINUE
               ELSE
                   MOVE    "E22"               TO  WRK-ERRCD
                   GO  TO  2002-SYORI-01-EXT
               END-IF
           END-IF
      *
      *    ジョブ管理処理
           MOVE    "ALL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN    =   ZERO
               CONTINUE
           ELSE
               MOVE    "E20"               TO  WRK-ERRCD
               GO  TO  2002-SYORI-01-EXT
           END-IF
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    LSTREQ-KARTE-UID
                                   TO  JOB-KARTE-UID
           MOVE    LSTREQ-ORCA-UID TO  JOB-ORCA-UID
      *
           PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN    =   ZERO
               CONTINUE
           ELSE
               MOVE    "E21"               TO  WRK-ERRCD
               GO  TO  2002-SYORI-01-EXT
           END-IF
      *
      *    ＪＯＢＩＤ更新処理
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    ZERO                TO  SYS-3003-STYUKYMD
           MOVE    ALL "9"             TO  SYS-3003-EDYUKYMD
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SYS-3003-TBL-KEY
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                       TO  SYS-3003-TBL-GROUP
           MOVE    WRK-CONS-JOB-JOBID  TO  SYS-3003-JOBID
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "E23"               TO  WRK-ERRCD
               GO  TO  2002-SYORI-01-EXT
           END-IF
      *
      *    エラーファイル名
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    WRK-CONS-FILE-ID
                                       TO  ERRFLPARA-FILE-ID
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
      *
      *    処理スクリプト
           MOVE    SPACE               TO  WRK-CONS-PRTKANRI-TBL-GROUP
           STRING  SMCNDATE-YMD    DELIMITED  BY  SIZE
                   SMCNDATE-HMS    DELIMITED  BY  SIZE
                                   INTO    WRK-CONS-PRTKANRI-TBL-GROUP
           END-STRING
      *
           MOVE    SPACE               TO  LNK-PRTKANRI-AREA
           INITIALIZE                      LNK-PRTKANRI-AREA
           MOVE    1                   TO  LNK-PRTKANRI-RENNUM
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  LNK-PRTKANRI-TBL-KEY
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                       TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    "000000"            TO  LNK-PRTKANRI-SRYYM
           MOVE    SPA-SYSYMD          TO  LNK-PRTKANRI-SKYYMD
           MOVE    "medication_masterlst"
                                       TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO                TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID          TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID            TO  LNK-PRTKANRI-OPID
      **** MOVE    SYS-1031-PRTNM (6)  TO  LNK-PRTKANRI-PRTNM
      *
           MOVE    SPACE               TO  SHELLTBL
           MOVE    "medication_masterlst.sh"
                                       TO  SHELLTBL-NAME
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID  TO  SHELLTBL-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SHELLTBL-SHELLID
           MOVE    LNK-PRTKANRI-RENNUM TO  SHELLTBL-ARG1
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SHELLTBL-ARG2
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SHELLTBL-ARG3
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SHELLTBL-ARG4
           MOVE    LNK-PRTKANRI-SRYYM  TO  SHELLTBL-ARG5
           MOVE    LNK-PRTKANRI-SKYYMD TO  SHELLTBL-ARG6
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SHELLTBL-ARG7
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SHELLTBL-ARG8
           MOVE    LNK-PRTKANRI-TERMID TO  SHELLTBL-ARG9
           MOVE    LNK-PRTKANRI-OPID   TO  SHELLTBL-ARG10
           MOVE    LNK-PRTKANRI-PRTNM  TO  SHELLTBL-ARG11
           MOVE    WRK-CONS-JOB-JOBID  TO  SHELLTBL-ARG12
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SHELLTBL-ARG13
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG14
           MOVE    ERRFLPARA-BASENAME
                                       TO  SHELLTBL-ARG15
           MOVE    WRK-BASE-YMD        TO  SHELLTBL-ARG16
      *
           MOVE    WRK-MONBLOB-ID      TO  SHELLTBL-ARG17
      *
           MOVE    "medicalpoints_master_make"
                                       TO  SHELLTBL-ARG18
           MOVE    LSTREQ-DATA-FORMAT
                                       TO  SHELLTBL-ARG19
           MOVE    LSTREQ-EFFECTIVE-MODE
                                       TO  SHELLTBL-ARG20
      *
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "allways"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       2002-SYORI-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    UIDコード取得処理
      *****************************************************************
       20021-UID-GET-SEC           SECTION.
      *
      *    UIDコード取得処理
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       ST
                                       UIDIO-AREA
      *
           .
       20021-UID-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    結果情報返却処理
      *****************************************************************
       2003-SYORI-02-SEC           SECTION.
      *
      *    ジョブ管理チェック処理
           MOVE    "CH2"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF    ( LSTREQ-KARTE-UID
                                   NOT =   JOB-KARTE-UID )
               OR    ( LSTREQ-ORCA-UID
                                   NOT =   JOB-ORCA-UID  )
                   MOVE    "E41"           TO  WRK-ERRCD
                   GO  TO  2003-SYORI-02-EXT
               END-IF
      *
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "E70"           TO  WRK-ERRCD
                   STRING  "処理中です【"  DELIMITED  BY  SIZE
                           JOB-SHELLMSG    DELIMITED  BY  SPACE
                           "】"            DELIMITED  BY  SIZE
                                           INTO  WRK-ERRMSG
                   END-STRING
               ELSE
                   IF      JOB-UPDCNT      >   ZERO
                       MOVE    JOB-UPDCNT      TO  WRK-UPDCNT
                       MOVE    WRK-UPDCNT-X    TO  LSTRES-OUTPUT-COUNT
                   ELSE
                       IF    ( JOB-ERRCD   =   SPACE                )
                       OR    ( JOB-YOBI    =
                               "処理対象のデータがありませんでした" )
                           MOVE    JOB-UPDCNT      TO  WRK-UPDCNT
                           MOVE    WRK-UPDCNT-X    TO
                                                   LSTRES-OUTPUT-COUNT
                       ELSE
                           MOVE    "E30"           TO  WRK-ERRCD
                           STRING  JOB-YOBI        DELIMITED  BY  SPACE
                                   "【"            DELIMITED  BY  SIZE
                                  JOB-SHELLMSG     DELIMITED  BY  SPACE
                                  "】"             DELIMITED  BY  SIZE
                                                   INTO  WRK-ERRMSG
                           END-STRING
                       END-IF
                   END-IF
               END-IF
           ELSE
               MOVE    "E40"           TO  WRK-ERRCD
           END-IF
           .
       2003-SYORI-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       800-JOBKANRI-CHK-SEC       SECTION.
      *
      *    ジョブ管理チェック
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC         TO  SYS-3003-REC
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL ( IDX >       SYS-3003-JOBID )
      *            ジョブ管理チェック処理
                   MOVE    "CH2"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
                   PERFORM 800-ORCSJOB-SEC
                   IF      SJOBKANRI-RETURN
                                           =   ZERO
                       IF      JOB-ENDYMD      =   SPACE
                           MOVE    "E70"           TO  WRK-ERRCD
                           MOVE    SPACE           TO  WRK-ERRMSG
                           STRING  "処理中です【"  DELIMITED  BY  SIZE
                                   JOB-SHELLMSG    DELIMITED  BY  SPACE
                                   "】"            DELIMITED  BY  SIZE
                                                   INTO  WRK-ERRMSG
                           END-STRING
                           GO  TO  800-JOBKANRI-CHK-EXT
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .
       800-JOBKANRI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
       800-ORCSJOB-EXT.
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
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           EVALUATE    WRK-ERRCD
               WHEN    "E30"
               WHEN    "E70"
                   GO  TO  890-ERRCD-MSG-EXT
           END-EVALUATE
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
           WHEN    "E04"
               MOVE    "電子カルテＵＩＤが未設定です。"
                                               TO  WRK-ERRMSG
           WHEN    "E05"
               MOVE    "オルカＵＩＤが未設定です。"
                                               TO  WRK-ERRMSG
           WHEN    "E11"
               MOVE    "基準日が暦日ではありません。"
                                               TO  WRK-ERRMSG
           WHEN    "E18"
               MOVE    "データフォーマットの設定に誤りがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "E19"
               MOVE    "有効モードの設定に誤りがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "E20"
               MOVE    "ジョブ管理が削除できませんでした。"
                                               TO  WRK-ERRMSG
           WHEN    "E21"
               MOVE    "ジョブ管理が更新できませんでした。"
                                               TO  WRK-ERRMSG
           WHEN    "E22"
               MOVE    "システム管理が削除できませんでした。"
                                               TO  WRK-ERRMSG
           WHEN    "E23"
               MOVE    "システム管理が更新できませんでした。"
                                               TO  WRK-ERRMSG
      *
           WHEN    "E40"
               MOVE    "対象のジョブ管理がありません。"
                                               TO  WRK-ERRMSG
           WHEN    "E41"
               MOVE    "対象の処理はありません。"
                                               TO  WRK-ERRMSG
      *
           WHEN    "E80"
               MOVE    "一時ディレクトリが取得できません。"
                                               TO  WRK-ERRMSG
      *
           WHEN    "E88"
               MOVE    "Ｄａｔａ＿Ｉｄが取得できません。"
                                               TO  WRK-ERRMSG 
      *共通エラー
           WHEN    "E89"
      *        ORCSCOMMON のエラー
               EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "職員情報が取得できません。"
                                               TO  WRK-ERRMSG
               WHEN    "1002"
                   MOVE    "医療機関情報が取得できません。"
                                               TO  WRK-ERRMSG
               WHEN    "1003"
                   MOVE    "システム日付が取得できません。"
                                               TO  WRK-ERRMSG
               WHEN    "1005"
                   MOVE    "患者番号構成情報が取得できません。"
                                               TO  WRK-ERRMSG
               WHEN    "1015"
                   STRING  "グループ医療機関が不整合です。"
                           "処理を終了して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    "システム項目が設定できません。"
                                               TO  WRK-ERRMSG
               END-EVALUATE
           WHEN    "E90"
                   MOVE    "他端末で使用中です。"
                                               TO  WRK-ERRMSG
           WHEN    "E91"
                   MOVE    "リクエスト番号が不正です。"
                                               TO  WRK-ERRMSG
           WHEN    "E97"
                   MOVE   "送信内容に誤りがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "E98"
                   MOVE   "送信内容の読込ができませんでした。"
                                               TO  WRK-ERRMSG
           WHEN    "E99"
                   MOVE    "ユーザＩＤが未登録です。"
                                               TO  WRK-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               INITIALIZE                      SYSKANRI-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
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
