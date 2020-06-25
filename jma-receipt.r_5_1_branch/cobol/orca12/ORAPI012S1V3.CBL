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
       PROGRAM-ID.         ORAPI012S1V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 患者基本情報設定（登録・削除）(V3)
      *  管理者            :
      *  作成日付    作業者        記述
      *  15/10/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.00    NACL-多々納  19/08/XX  検索処理追加
      *                                     改行チェック追加
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
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *    患者画面スパ領域
           COPY    "P02SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-PARA                PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-PTBYOMEI            PIC 9(01).
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-KYUSEIRRK           PIC 9(01).
      *
           03  FLG-ERR                 PIC 9(01).
           03  FLG-ERR2                PIC 9(01).
           03  FLG-ERR3                PIC 9(01).
           03  FLG-KYUSEI              PIC 9(01).
           03  FLG-SYORI               PIC 9(01).
           03  FLG-PTINF-JFK           PIC 9(01).
           03  FLG-CHK02               PIC 9(01).
           03  FLG-CHKARI              PIC 9(01).
      *
           03  FLG-P02A-SYORI          PIC 9(01).
           03  FLG-DATA                PIC 9(01).
           03  FLG-DELETE              PIC 9(01).
           03  FLG-CONTKBN             PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
           03  FLG-ERR01               PIC 9(01).
           03  FLG-KANA                PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDY2                    PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDY1                    PIC 9(04).
           03  IDX1                    PIC 9(04).
           03  TBL-MAX                 PIC 9(04).
           03  IDX-W                   PIC 9(04).
      *
           03  IDX-ERR                 PIC 9(04).
           03  IDX-JF                  PIC 9(04).
           03  IDS                     PIC 9(04).
           03  IDX-DS                  PIC 9(04).
      *
      *    カウント領域
      *01  CNT-AREA.
      *
      *    一時領域
      *
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
      *
      *    住所変換
           03  WRK-MAE-POST            PIC X(07).
           03  WRK-POST                PIC X(07).
           03  WRK-ADRS                PIC X(100).
      *    減免
           03  WRK-DISCOUNTKBN-NAME    PIC X(20).
           03  WRK-DISCOUNT-NAME       PIC X(20).
      *    エラーコード
           03  WRK-ERRCD-G.
               05  WRK-ERRKBN              PIC X(01).
               05  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-ERRCD-2             PIC X(02).
           03  WRK-ERRMSG-1            PIC X(100).
           03  WRK-WERRCD              PIC X(03).
      *    ワーニングコード
           03  WRK-KEICD-TBL.
               05  WRK-KEICD-OCC       OCCURS   50.
                   07  WRK-KEIERRCD.
                       09  WRK-KEICD-K         PIC X(01).
                       09  WRK-KEICD           PIC X(03).
      *
           03  WRK-OKMSG               PIC X(100).
      *
      *    オルカUID
           03  WRK-ORCA-UID            PIC X(36).
           03  WRK-OPID                PIC X(16).
           03  WRK-SPA-OPID            PIC X(16).
      *
           03  WRK-ENDKBN              PIC X(01).
      *
           03  WRK-MCP-PATHNAME        PIC X(62).
           03  WRK-MCP-WINDOW          PIC X(62).
      *    MCP-TERMID
           03  WRK-MCP-TERM            PIC X(64).
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(100).
           03  WRK-ATO-INPUT           PIC X(100).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *
      *    年齢計算
       01  WRK-NENREI-AREA.
           03  WRK-YMDS1.
               05  WRK-YYS1            PIC 9(04).
               05  WRK-MMS1            PIC 9(02).
               05  WRK-DDS1            PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2            PIC 9(04).
               05  WRK-MMS2            PIC 9(02).
               05  WRK-DDS2            PIC 9(02).
           03  WRK-NENREI.
               05  WRK-NENREI-YY       PIC 9(04).
               05  WRK-NENREI-MM       PIC 9(02).
               05  WRK-NENREI-DD       PIC 9(02).
      *    基準日
           03  WRK-BASEYMD.
               05  WRK-BYY             PIC 9(04).
               05  WRK-BMM             PIC 9(02).
               05  WRK-BDD             PIC 9(02).
      *
       01  WRK-XML-AREA.
           03  WRK-PTNUM               PIC X(20).
           03  WRK-BIRTHDAY-YMD        PIC X(08).
      **** 03  WRK-RJN-RATE            PIC X(01).
      *
      *    共通パラメタ内容
       01  WRK-APIV3DATA-AREA.
      *    電子カルテUID
           03  WRK-KARTE-UID           PIC X(36).
      *    患者番号
           03  WRK-PARA-PTNUM          PIC X(20).
           03  WRK-APIV3DATA-REC.
      *        レスポンス番号
               05  WRK-RESPONSE-NUMBER     PIC X(02).
      *        処理ＰＧ
               05  WRK-PARA-SYORIPG        PIC X(02).
      *        処理モード
               05  WRK-PATIENT-MODE        PIC X(06).
      *        確認コード
               05  WRK-SELECT-CODE         PIC X(04).
      *        登録終了
               05  WRK-PARA-ENDKBN         PIC X(01).
      *
      *    ＵＩＤ取得用エリア
       01  UIDIO-AREA.
           03  C-RET               PIC X(2).
           03  C-UID               PIC X(36).
       01  ST                      PIC 9(2).
      *v4.8
      *    画面非表示
      *    COPY    "ENUM-VALUE".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ファイルデイレクトリ位置指定サブ
      ***  COPY    "CPMKPASS.INC".
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    患者番号発生サブ
           COPY    "CPORCSP01.INC".
      *    画面編集サブ
           COPY    "CPORCSP02.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *    全角チェックパラメタ（拡張漢字）
           COPY    "CPORCSKANACHK2.INC".
      *
           COPY    "CPORCXKANACONV.INC".
      *
      *    拡張文字対応
           COPY    "CPORCSSTRING.INC".
      *
      *    日付変換サブ
           COPY     "CPORCSGDAY.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    API XML読込用定義体
           COPY    "CPPATIENTMODV3REQ1.INC".
      *    API XML出力用定義体
           COPY    "CPPATIENTMODV3RES1.INC".
      *
      *    処理終了パラメタ
           COPY    "CPORAPIV3SUB01.INC".
      *
      *    保険組合せ決定関数サブ
      **** COPY    "CPORCSAPIHKN.INC".
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    （診療科情報）
      *    COPY    "CPSK1005.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *
           COPY  "CPSK1009.INC".
      *    患者マスタ−
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    旧姓履歴   *
       01  KYUSEIRRK-REC.
           COPY  "CPKYUSEIRRK.INC".
      *
      *    保険組み合わせ
      *01  HKNCOMBI-REC.
      *    COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
      *01  PTHKNINF-REC.
      *    COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
      *01  PTKOHINF-REC.
      *    COPY    "CPPTKOHINF.INC".
      *
      *    保険者号マスタ
      *01  HKNJAINF-REC.
      *    COPY    "CPHKNJAINF.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *    パラメタテーブル
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "ORCA-BLOB".
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA12SCRAREA.INC".
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
           DISPLAY   "*********************"
           DISPLAY   "* patient res start *"
           DISPLAY   "*********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
      *
           MOVE    MCP-TERM            TO  WRK-MCP-TERM
           MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   "99"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST03-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
      *
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
           MOVE    WRK-MCP-TERM        TO  MCP-TERM
      *
           DISPLAY   "*******************"
           DISPLAY   "* patient res end *"
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
           INITIALIZE                      XML-PATIENTMODRES
           INITIALIZE                      WRK-APIV3DATA-AREA
           INITIALIZE                      WRK-NENREI-AREA
      *
           INITIALIZE                      SPA-P02SCRAREA
           INITIALIZE                      SPA-P02KYOUTU
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Acceptance_Info"    TO  PTMODRES-RESKEY
      *
           STRING  "A+"
                   MCP-USER            DELIMITED  BY  SIZE
                                       INTO    WRK-OPID
           END-STRING
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PTMODRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  PTMODRES-INFORMATION-TIME
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
      *
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
           PERFORM 1000-RGTXMLREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           MOVE    ZERO                TO  PTMODRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
           INITIALIZE                      WRK-KEICD-TBL
           MOVE    ZERO                TO  IDX-W
           MOVE    ZERO                TO  IDX-ERR
      *
           MOVE    PTMODREQ-REQUEST-NUMBER
                                       TO  PTMODRES-REQUEST-NUMBER
           MOVE    PTMODREQ-REQUEST-NUMBER
                                       TO  PTMODRES-RESPONSE-NUMBER
      *    オルカＵＩＤ
           IF      PTMODREQ-ORCA-UID   NOT =   SPACE
      *R01.7
             AND  (PTMODREQ-REQUEST-NUMBER NOT =   "00" )
               PERFORM 2009-PARA-INIT-SEC
           END-IF
      *
           MOVE    ZERO                TO  FLG-SYORI
           MOVE    ZERO                TO  FLG-ERR01
      *
           EVALUATE    PTMODREQ-REQUEST-NUMBER
           WHEN    "01"
               CONTINUE
           WHEN    "02"
               MOVE    1                   TO  FLG-CHKARI
           WHEN    "99"
      *        強制処理終了
               IF      WRK-ERRCD           =   SPACE
                   PERFORM 2099-CANCEL-SEC
                   MOVE    "00"            TO  PTMODRES-RESPONSE-NUMBER
                   GO      TO      200-MAIN-EXT
               END-IF
      *
      *R01.7
           WHEN    "00"
      *        検索処理
               PERFORM 20010-PTINF-SELECT-SEC
               MOVE    "00"            TO  PTMODRES-RESPONSE-NUMBER
               GO      TO      200-MAIN-EXT
      *
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
               MOVE    "01"                TO  PTMODRES-RESPONSE-NUMBER
               MOVE    WRK-PARA-PTNUM      TO  WRK-PTNUM
           END-EVALUATE
      *
           EVALUATE    PTMODREQ-PATIENT-MODE
           WHEN    "New"
      *        新規登録
               MOVE    1                   TO  FLG-SYORI
               MOVE    1                   TO  FLG-CHKARI
           WHEN    "Modify"
      *        更新
               MOVE    2                   TO  FLG-SYORI
           WHEN    "Delete"
      *        削除
               MOVE    3                   TO  FLG-SYORI
           WHEN    OTHER
               IF      WRK-ERRCD           =   SPACE
                   MOVE    "09"                TO  WRK-ERRCD
               END-IF
               MOVE    "01"                TO  PTMODRES-RESPONSE-NUMBER
           END-EVALUATE
      *
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               PERFORM 890-ERRCD-MSG-SEC
      *        編集処理
               PERFORM 20010-INPUT-HEN-SEC
      *        初回から
               IF      FLG-ERR01           =   1
                   MOVE    "01"            TO  PTMODRES-RESPONSE-NUMBER
               END-IF
      *
               IF     (WRK-ERRCD           =   "08" )
                  OR  (PTMODREQ-ORCA-UID   =   SPACE)
                   CONTINUE
               ELSE
                   PERFORM 2901-PARA-INS-SEC
               END-IF
               GO      TO      200-MAIN-EXT
           END-IF
      *
           EVALUATE    PTMODREQ-PATIENT-MODE
           WHEN    "New"
      *        新規登録
               PERFORM 2002-PTINF-ADD-SEC
           WHEN    "Modify"
      *        更新処理（基本情報）
               PERFORM 2003-PTINF-UPDATE-SEC
           WHEN    "Delete"
      *        削除処理
               PERFORM 2004-PTINF-DELETE-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    強制処理終了
      *****************************************************************
       2099-CANCEL-SEC   SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    PTMODREQ-PATIENTID  TO  WRK-PTNUM
           IF      WRK-PTNUM           =   "*"
                                       OR  SPACE
               CONTINUE
           ELSE
               INITIALIZE                  ORCSPTIDAREA
               MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
               MOVE    WRK-PTNUM           TO  SPTID-PTNUM
               CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                   SPA-AREA
               MOVE    SPTID-PTNUM         TO  WRK-PTNUM
           END-IF
      *
           INITIALIZE                  ORAPIV3SUB01AREA
           MOVE    "9"                 TO  APIV3SUB01-SYORI
           IF      PTMODREQ-ORCA-UID   NOT =   SPACE
               MOVE    SPA-TERMID          TO  APIV3SUB01-TERMID
           END-IF
           MOVE    PTMODREQ-KARTE-UID  TO  APIV3SUB01-KARTE-UID
           MOVE    "AP12"              TO  APIV3SUB01-API-GYOUMUID
           MOVE    "API12V3"           TO  APIV3SUB01-API-FILEMEI
           MOVE    WRK-PTNUM           TO  APIV3SUB01-PTNUM
           MOVE    "P02"               TO  APIV3SUB01-PARA-GYOUMUID
           CALL    "ORAPIV3SUB01"      USING
                                       MCPAREA
                                       SPA-AREA
                                       ORAPIV3SUB01AREA
           IF      APIV3SUB01-ERRCD    NOT =   SPACE
               MOVE    "81"                TO  WRK-ERRCD
               MOVE    APIV3SUB01-ERRMSG   TO  WRK-ERRMSG
           END-IF
      *    編集処理
           PERFORM 20010-INPUT-HEN-SEC
           MOVE    WRK-PTNUM           TO  PTMODRES-PATIENTID
      *
           MOVE    9                   TO  FLG-SYORI
      *
           .
       2099-CANCEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-RGTXMLREAD-SEC   SECTION.
      *
           IF      APILST03-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-PATIENTMODREQ
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_patientmodv3req1"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST03-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "patientmodv3req1"  TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
               MOVE    "97"             TO  WRK-ERRCD
               GO      TO      1000-RGTXMLREAD-EXT
           END-IF
      *
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_patientmodv3req1"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST03-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "patientmodv3req1"  TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-PATIENTMODREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE           TO  APILST03-BODY
      *
           .
       1000-RGTXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-RGTXMLMAKE-SEC   SECTION.
      *
           MOVE    SPACE               TO  WRK-OKMSG
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "登録終了"
                                           TO  WRK-OKMSG
                   WHEN    2
                       IF     (PTMODRES-RESPONSE-NUMBER    =  "00" )
                         OR   (WRK-ENDKBN          =   "1"  )
                           MOVE    "更新終了"
                                           TO  WRK-OKMSG
                       ELSE
                           MOVE    "検索処理終了"
                                           TO  WRK-OKMSG
                       END-IF
                   WHEN    3
                       IF      PTMODRES-RESPONSE-NUMBER    =  "00"
                           MOVE    "削除終了"
                                           TO  WRK-OKMSG
                       ELSE
                           MOVE    "検索処理終了"
                                           TO  WRK-OKMSG
                       END-IF
                   WHEN    9
                       MOVE    "解除処理終了"
                                           TO  WRK-OKMSG
      *R01.7
                   WHEN    0
                       MOVE    "検索処理終了"
                                           TO  WRK-OKMSG
               END-EVALUATE
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               MOVE    ZERO            TO  WRK-ERRCD
               IF      WRK-ERRKBN          =   SPACE
                   MOVE    ZERO            TO  WRK-ERRKBN
               END-IF
               MOVE    WRK-ERRCD-G         TO  PTMODRES-API-RESULT
               MOVE    WRK-OKMSG           TO  PTMODRES-API-RESULT-MSG
           ELSE
      *        エラーメッセージ
               IF      WRK-ERRMSG          =   SPACE
                   PERFORM 890-ERRCD-MSG-SEC
               END-IF
               IF      WRK-ERRKBN          =   SPACE
                   MOVE    "E"                 TO  WRK-ERRKBN
               END-IF
               MOVE    WRK-ERRCD-G         TO  PTMODRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  PTMODRES-API-RESULT-MSG
           END-IF
      *
           MOVE    ZERO                TO  IDX
      *    ワーニングメッセージ編集
           IF      WRK-ERRCD           =   ZERO
               IF      IDX-W           >   ZERO
                   MOVE    "W"             TO  WRK-ERRKBN
                   MOVE    "00"            TO  WRK-ERRCD
                   MOVE    WRK-ERRCD-G     TO  PTMODRES-API-RESULT
               END-IF
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   20  )
                         OR   (IDX     >   IDX-W)
                         OR   (WRK-KEIERRCD(IDX)   =   SPACE)
               MOVE    SPACE               TO  WRK-ERRCD
               MOVE    SPACE               TO  SPA-ERRCD
               IF      WRK-KEICD-K (IDX)   =   SPACE
                   MOVE    WRK-KEICD (IDX)     TO  WRK-ERRCD
               ELSE
                   MOVE    WRK-KEIERRCD(IDX)   TO  SPA-ERRCD
                   MOVE    "K2"                TO  WRK-ERRCD
               END-IF
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-KEIERRCD(IDX)   TO  PTMODRES-WARNING
                                                             (IDX)
               MOVE    WRK-ERRMSG      TO  PTMODRES-WARNING-MSG
                                                             (IDX)
           END-PERFORM
      *
           IF      APILST03-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_patientmodv3res1"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  PTMODRES-MODE
           MOVE    "patientmodv3res1"  TO  PTMODRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-PATIENTMODRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"           TO  MCP-FUNC
           MOVE    "xml_patientmodv3res1"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  PTMODRES-MODE
           MOVE    "patientmodv3res1"  TO  PTMODRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-PATIENTMODRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               MOVE    PTMODRES-OBJECT     TO  APILST03-BODY
               MOVE    "patientmod/xml"    TO  APILST03-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       300-RGTXMLMAKE-EXT.
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
      *****************************************************************
      *    ORCA-UID 初期処理
      *****************************************************************
       2009-PARA-INIT-SEC            SECTION.
      *
      *    UIDコードをTERMID
           MOVE    PTMODREQ-ORCA-UID   TO  WRK-ORCA-UID
           MOVE    SPACE               TO  SPA-TERMID
           STRING  "API+"
                   WRK-ORCA-UID        DELIMITED  BY  SPACE
                                       INTO    SPA-TERMID
           END-STRING
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "AP12"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "API12V3"           TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
           PERFORM 9010-APIPARA-KEYREAD-SEC
      *
           IF      FLG-APIPARA          =   ZERO
               MOVE    APIPARA-DATA-REC    TO  WRK-APIV3DATA-REC
               MOVE    APIPARA-KARTE-UID   TO  WRK-KARTE-UID
               MOVE    APIPARA-PTNUM       TO  WRK-PARA-PTNUM
           ELSE
               MOVE    SPACE               TO  WRK-APIV3DATA-AREA
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
      *
           IF     (WRK-ERRCD               =   SPACE )
             AND  (PTMODREQ-REQUEST-NUMBER =   "01"  )
      *        リクエスト０１　は排他解除
               PERFORM 990-LOCK-OUT-SEC
      *        パラメタ削除処理
               PERFORM 2902-PARA-ALLDEL-SEC
           END-IF
      *
           IF     (WRK-ERRCD               =   SPACE )
             AND  (PTMODREQ-REQUEST-NUMBER NOT =   "01"  )
      *        リクエスト０１以外は、同じ処理の続きであること
               IF      WRK-PARA-SYORIPG    NOT =   "01"
                   MOVE    "08"                TO  WRK-ERRCD
               END-IF
           END-IF
           .
       2009-PARA-INIT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    患者基本情報設定内容チェック
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
      *    編集処理
           PERFORM 20010-INPUT-HEN-SEC
      *    必須入力チェック
           PERFORM 20010-INIT-CHECK-SEC
      *
      *    電子カルテＵＩＤチェック
           IF     (PTMODREQ-ORCA-UID   NOT =   SPACE  )
             AND  (WRK-ERRCD           =   SPACE  )
               IF      PTMODREQ-KARTE-UID  NOT =   WRK-KARTE-UID
      *            他端末使用中
                   MOVE    "90"            TO  WRK-ERRCD
                   MOVE    1                   TO  FLG-ERR01
               END-IF
           END-IF
      *
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PTNUM-CHK-SEC
           END-IF
      *
      *    新規とリクエスト＝０２のみ
           IF     (FLG-SYORI           =   1   )
              OR  (FLG-CHKARI          =   1   )
               IF      WRK-ERRCD           =   SPACE
      *            患者基本チェック処理
                   PERFORM 200211-KIHON-CHK-SEC
               END-IF
           END-IF
      *
      *    継続区分
           IF      PTMODREQ-CONT-MODE  =   "True"
               MOVE    1               TO  FLG-CONTKBN
           ELSE
               MOVE    ZERO            TO  FLG-CONTKBN
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
      *    患者番号
           MOVE    PTMODREQ-PATIENTID      TO  PTMODRES-PATIENTID
      *    患者情報
           MOVE    PTMODREQ-NAME           TO  PTMODRES-NAME
           MOVE    PTMODREQ-KANANAME       TO  PTMODRES-KANANAME
           MOVE    PTMODREQ-BIRTHDAY       TO  PTMODRES-BIRTHDAY
           MOVE    PTMODREQ-SEX            TO  PTMODRES-SEX
           MOVE    PTMODREQ-SETAI-NAME     TO  PTMODRES-SETAI-NAME
           MOVE    PTMODREQ-ZOKUGARA       TO  PTMODRES-ZOKUGARA
           MOVE    PTMODREQ-JOB            TO  PTMODRES-JOB
           MOVE    PTMODREQ-NICKNAME       TO  PTMODRES-NICKNAME
           MOVE    PTMODREQ-KEITAI-TEL     TO  PTMODRES-KEITAI-TEL
           MOVE    PTMODREQ-FAX            TO  PTMODRES-FAX
           MOVE    PTMODREQ-EMAIL          TO  PTMODRES-EMAIL
      *    生年月日変換
           MOVE    PTMODREQ-BIRTHDAY       TO  PTMODRES-BIRTHDAY
           MOVE    PTMODREQ-BIRTHDAY       TO  WRK-HEN-DATE
           MOVE    SPACE                   TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-BIRTHDAY-YMD
      *    自宅情報
           MOVE    PTMODREQ-ADDRESS-INFO   TO  PTMODRES-ADDRESS-INFO
      *    勤務先情報
           MOVE    PTMODREQ-OFFICE-INFO    TO  PTMODRES-OFFICE-INFO
      *    連絡先情報
           MOVE    PTMODREQ-RENRAKU-INFO   TO  PTMODRES-RENRAKU-INFO
      *    帰省先情報
           MOVE    PTMODREQ-KISEI-INFO     TO  PTMODRES-KISEI-INFO
      *    禁忌
           MOVE    PTMODREQ-TABOO-1        TO  PTMODRES-TABOO-1
           MOVE    PTMODREQ-TABOO-2        TO  PTMODRES-TABOO-2
      *    アレルギー
           MOVE    PTMODREQ-ALLERGY-1      TO  PTMODRES-ALLERGY-1
           MOVE    PTMODREQ-ALLERGY-2      TO  PTMODRES-ALLERGY-2
      *    感染症
           MOVE    PTMODREQ-KANSENSYO-1    TO  PTMODRES-KANSENSYO-1
           MOVE    PTMODREQ-KANSENSYO-2    TO  PTMODRES-KANSENSYO-2
      *    コメント
           MOVE    PTMODREQ-COMMENT-1      TO  PTMODRES-COMMENT-1
           MOVE    PTMODREQ-COMMENT-2      TO  PTMODRES-COMMENT-2
      *    テスト患者区分
           MOVE    PTMODREQ-TEST-PATIENT   TO  PTMODRES-TEST-PATIENT
      *    死亡区分
           MOVE    PTMODREQ-DEATHKBN       TO  PTMODRES-DEATHKBN
      *    減免事由
           MOVE    PTMODREQ-DISCOUNTKBN    TO  PTMODRES-DISCOUNTKBN
      *    割引率
           MOVE    PTMODREQ-DISCOUNT       TO  PTMODRES-DISCOUNT
      *    状態１から３
           MOVE    PTMODREQ-CONDITION-KBN(1)
                                           TO  PTMODRES-CONDITION-KBN(1)
           MOVE    PTMODREQ-CONDITION-KBN(2)
                                           TO  PTMODRES-CONDITION-KBN(2)
           MOVE    PTMODREQ-CONDITION-KBN(3)
                                           TO  PTMODRES-CONDITION-KBN(3)
      *
           MOVE    PTMODREQ-KARTE-UID      TO  PTMODRES-KARTE-UID
           MOVE    PTMODREQ-ORCA-UID       TO  PTMODRES-ORCA-UID
      *
           MOVE    PTMODREQ-CONT-MODE      TO  PTMODRES-CONT-MODE
      *
           .
       20010-INPUT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    必須入力チェック処理
      *****************************************************************
       20010-INIT-CHECK-SEC    SECTION.
      *
           MOVE    PTMODREQ-PATIENTID      TO  WRK-PTNUM
      *    患者番号
           IF      WRK-PTNUM           =   SPACE
               MOVE    "01"                TO  WRK-ERRCD
               MOVE    1                   TO  FLG-ERR01
           END-IF
      *    患者氏名
           IF      PTMODREQ-NAME       =   SPACE
               IF     (WRK-ERRCD           =   SPACE  )
                 AND  (FLG-CHKARI          =   1      )
                   MOVE    "02"                TO  WRK-ERRCD
               END-IF
           END-IF
      *    カナ氏名
           IF      PTMODREQ-KANANAME    =   SPACE
               IF     (WRK-ERRCD           =   SPACE  )
                 AND  (FLG-CHKARI          =   1      )
                   MOVE    "03"                TO  WRK-ERRCD
               END-IF
           END-IF
      *    生年月日
           IF      WRK-BIRTHDAY-YMD    =   SPACE
               IF     (WRK-ERRCD           =   SPACE  )
                 AND  (FLG-CHKARI          =   1      )
                   MOVE    "04"                TO  WRK-ERRCD
               END-IF
           END-IF
      *    性別
           IF      PTMODREQ-SEX        =   SPACE
               IF     (WRK-ERRCD           =   SPACE  )
                 AND  (FLG-CHKARI          =   1      )
                   MOVE    "05"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    UID（電子カルテ）
           IF      PTMODREQ-KARTE-UID  =   SPACE
               IF     (WRK-ERRCD           =   SPACE  )
                   MOVE    "06"                TO  WRK-ERRCD
                   MOVE    1                   TO  FLG-ERR01
               END-IF
           END-IF
      *
      *    オルカＵＩＤ
           IF      PTMODREQ-ORCA-UID   =   SPACE
               IF      PTMODREQ-REQUEST-NUMBER NOT =   "01"
                   MOVE    "07"                TO  WRK-ERRCD
                   MOVE    1                   TO  FLG-ERR01
               END-IF
           END-IF
      *    処理モード
           IF     (PTMODREQ-REQUEST-NUMBER NOT =   "01" )
             AND  (PTMODREQ-ORCA-UID       NOT =   SPACE)
               IF      PTMODREQ-PATIENT-MODE   =   WRK-PATIENT-MODE
                   CONTINUE
               ELSE
                   MOVE    "19"                TO  WRK-ERRCD
                   MOVE    1                   TO  FLG-ERR01
               END-IF
           END-IF
           .
       20010-INIT-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック
      *****************************************************************
       20011-PTNUM-CHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  SPA-PTID
           MOVE    SPACE               TO  SPA-PTNUM
           IF     (FLG-SYORI           =   1   )
               IF      WRK-PTNUM           =   "*"
      *            自動採番
                   IF      SPA-1009-PTNUMKSIKBN    NOT =   "2"
      *                標準構成以外は、自動採番なし
                       MOVE    "11"                TO  WRK-ERRCD
                   END-IF
               ELSE
      *            患者番号検証
                   PERFORM 200111-PTNUM-KEN-SEC
               END-IF
           ELSE
      *        患者番号取得
               INITIALIZE                  ORCSPTIDAREA
               MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
               MOVE    WRK-PTNUM           TO  SPTID-PTNUM
               CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                   SPA-AREA
               IF      SPTID-RC            =   ZERO
                   MOVE    SPTID-PTNUM         TO  WRK-PTNUM
                   MOVE    SPTID-PTNUM         TO  SPA-PTNUM
                   MOVE    SPTID-PTID          TO  SPA-PTID
               ELSE
                   MOVE    "10"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
           MOVE    WRK-PTNUM           TO  PTMODRES-PATIENTID
           .
       20011-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号検証処理
      *****************************************************************
       200111-PTNUM-KEN-SEC    SECTION.
      *
           INITIALIZE                      ORCSPTNUMAREA
           MOVE    WRK-PTNUM           TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           MOVE    SPTNUM-PTNUM        TO  WRK-PTNUM
           EVALUATE    SPTNUM-RC
               WHEN    00
      *            患者ありエラー
                   MOVE    "13"                TO  WRK-ERRCD
               WHEN    20
      *            全角エラー
                   MOVE    "12"                TO  WRK-ERRCD
               WHEN    10
      *            新規患者検索へ
                   IF      SPTNUM-SYUBETU       =   1
                       CONTINUE
                   ELSE
                       MOVE    "12"                TO  WRK-ERRCD
                   END-IF
               WHEN    OTHER
      *            エラー
                   MOVE    "12"                TO  WRK-ERRCD
           END-EVALUATE
      *
           IF      WRK-ERRCD           =   SPACE
      *        桁数チェック
               INITIALIZE                  ORCSP01AREA
               MOVE    "2"                 TO  ORCSP01-SYORI
               MOVE    WRK-PTNUM           TO  ORCSP01-PTNUM
               CALL    "ORCSP01"           USING
                                           ORCSP01AREA
                                           SPA-AREA
               IF      SPA-ERRCD           =   SPACE
                   MOVE    ORCSP01-PTNUM       TO  SPA-GMN-PTNUM
                   MOVE    ORCSP01-PTNUM       TO  SPA-PTNUM
                   MOVE    SPA-PTNUM           TO  WRK-PTNUM
               ELSE
                   MOVE    "P1"                TO  WRK-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
                   MOVE    "18"                TO  WRK-ERRCD
               END-IF
           END-IF
           .
       200111-PTNUM-KEN-EXT.
           EXIT.
      *****************************************************************
      *    患者氏名チェック処理
      *****************************************************************
       20011-NAME-CHK-SEC    SECTION.
      *
      *    半角空白を全角空白へ
           MOVE    PTMODREQ-NAME       TO  WRK-MAE-INPUT
           MOVE    50                  TO  WRK-LEN
           PERFORM 891-HALF-ZEN2-SEC
           MOVE    WRK-ATO-INPUT       TO  PTMODREQ-NAME
           IF      FLG-ERR             =   1
               MOVE    "14"                TO  WRK-ERRCD
           ELSE
               MOVE    PTMODREQ-NAME       TO  PTMODRES-NAME
               MOVE    PTMODRES-NAME       TO  SPA-GMN-NAME
           END-IF
           .
       20011-NAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    カナ氏名チェック処理
      *****************************************************************
       20011-KANANAME-CHK-SEC    SECTION.
      *
      *    半角を全角へ
           MOVE    PTMODREQ-KANANAME   TO  WRK-MAE-INPUT
           MOVE    50                  TO  WRK-LEN
           PERFORM 880-HALF-ZENCNG-SEC
           MOVE    WRK-ATO-INPUT       TO  PTMODREQ-KANANAME
           IF      FLG-ERR             =   1
               MOVE    "15"                TO  WRK-ERRCD
           ELSE
               MOVE    PTMODREQ-KANANAME   TO  PTMODRES-KANANAME
               MOVE    PTMODRES-KANANAME   TO  SPA-GMN-KANANAME
           END-IF
           .
       20011-KANANAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    性別チェック処理
      *****************************************************************
       20011-SEX-CHK-SEC    SECTION.
      *
           EVALUATE    PTMODREQ-SEX
               WHEN    "1"
               WHEN    "2"
                   MOVE    PTMODREQ-SEX        TO  PTMODRES-SEX
                   MOVE    PTMODREQ-SEX        TO  SPA-GMN-SEX
               WHEN    OTHER
                   MOVE    "16"                TO  WRK-ERRCD
           END-EVALUATE
           .
       20011-SEX-CHK-EXT.
           EXIT.
      *****************************************************************
      *    生年月日チェック処理
      *****************************************************************
       20011-BIRTHDAY-CHK-SEC    SECTION.
      *
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-BIRTHDAY-YMD    TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE     "17"               TO  WRK-ERRCD
           ELSE
               MOVE    WRK-BIRTHDAY-YMD    TO  SPA-NAI-BIRTHDAY
      ***      MOVE    WRK-BIRTHDAY-YMD    TO  SPA-GMN-BIRTHDAY
           END-IF
      *
           .
       20011-BIRTHDAY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号新規取得処理
      *****************************************************************
       200111-PTNUM-GET-SEC    SECTION.
      *
           IF      SPA-1009-PTNUMKSIKBN    =   "2"
      *        標準構成
               INITIALIZE                  ORCSP01AREA
               MOVE    "1"                 TO  ORCSP01-SYORI
               CALL    "ORCSP01"           USING
                                           ORCSP01AREA
                                           SPA-AREA
               IF      SPA-ERRCD           =   "8000"
                   ADD     1                   TO  IDX-W
                   MOVE    "K1"                TO  WRK-KEICD(IDX-W)
                   MOVE    SPACE               TO  SPA-ERRCD
               END-IF
               IF      SPA-ERRCD           =   SPACE  OR  "8000"
                   MOVE    ORCSP01-PTNUM       TO  SPA-GMN-PTNUM
                   MOVE    ORCSP01-PTNUM       TO  SPA-PTNUM
                   MOVE    ORCSP01-PTID        TO  SPA-PTID
                   MOVE    ORCSP01-PTID        TO  SPA-GMN-PTID
                   MOVE    SPA-PTNUM           TO  WRK-PTNUM
               ELSE
                   MOVE    "P1"                TO  WRK-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
                   MOVE    "18"                TO  WRK-ERRCD
               END-IF
           END-IF
           .
       200111-PTNUM-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＵＩＤコード取得処理
      *****************************************************************
       20020-ORCAUID-SYORI-SEC        SECTION.
      *
      *    UIDコード処理
           IF      PTMODREQ-ORCA-UID   =   SPACE
      *        UIDコード取得処理
               PERFORM 1001-UID-GET-SEC
               MOVE    C-UID               TO  WRK-ORCA-UID
           ELSE
               MOVE    PTMODREQ-ORCA-UID   TO  WRK-ORCA-UID
           END-IF
      *    UIDコードをTERMID
           MOVE    SPACE               TO  SPA-TERMID
           STRING  "API+"
                   WRK-ORCA-UID        DELIMITED  BY  SPACE
                                           INTO    SPA-TERMID
           END-STRING
      *
           MOVE    WRK-ORCA-UID        TO  PTMODRES-ORCA-UID
      *
           .
       20020-ORCAUID-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    UIDコード取得処理
      *****************************************************************
       1001-UID-GET-SEC     SECTION.
      *
      *    UIDコード取得処理
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       ST
                                       UIDIO-AREA
      *
           .
       1001-UID-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者登録処理
      *****************************************************************
       2002-PTINF-ADD-SEC        SECTION.
      *
      *    オルカＵＩＤ処理
           PERFORM 20020-ORCAUID-SYORI-SEC
      *    継続でないこと
      *    IF      PTMODREQ-ORCA-UID   NOT =   SPACE
      *        IF      WRK-PARA-ENDKBN     =   "1"
      *            MOVE    "44"            TO  WRK-ERRCD
      *            終了処理
      *            PERFORM 2909-END-SYORI-SEC
      *            MOVE    SPACE               TO  PTMODRES-CONT-MODE
      *            GO      TO      2002-PTINF-ADD-EXT
      *        END-IF
      *    END-IF
      *
           EVALUATE    PTMODREQ-REQUEST-NUMBER
           WHEN    "01"
               MOVE    "02"            TO  PTMODRES-RESPONSE-NUMBER
           WHEN    "02"
               MOVE    "02"            TO  PTMODRES-RESPONSE-NUMBER
      *        患者番号判定
               IF     (WRK-PTNUM       NOT =   WRK-PARA-PTNUM )
                   MOVE    "22"            TO  WRK-ERRCD
                   MOVE    "01"            TO  PTMODRES-RESPONSE-NUMBER
               END-IF
           WHEN    OTHER
               MOVE    "91"            TO  WRK-ERRCD
               MOVE    "01"            TO  PTMODRES-RESPONSE-NUMBER
           END-EVALUATE
      *
      *    基本情報チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200211-PTINF-CHK-SEC
           END-IF
      *
      *    二重登録チェック
           MOVE    ZERO                TO  FLG-PTINF-JFK
           IF     (PTMODREQ-REQUEST-NUMBER =   "02" )
               IF      WRK-SELECT-CODE     =   SPACE
                   MOVE    1               TO  FLG-PTINF-JFK
               ELSE
                   EVALUATE    PTMODREQ-SELECT-ANSWER
                   WHEN    "OK"
                   WHEN    "Ok"
                       MOVE    1               TO  FLG-PTINF-JFK
                   WHEN    "No"
                   WHEN    "NO"
      *                処理終了
                       MOVE    1               TO  FLG-PTINF-JFK
                       MOVE    "55"            TO  WRK-ERRCD
                       MOVE    ZERO            TO  FLG-CONTKBN
                   WHEN    OTHER
                       IF      WRK-ERRCD       =   SPACE
                           MOVE    "21"        TO  WRK-ERRCD
                       END-IF
                   END-EVALUATE
               END-IF
           END-IF
           IF    ((PTMODREQ-REQUEST-NUMBER =   "01" )
              OR  (FLG-PTINF-JFK           =   ZERO ))
             AND  (WRK-ERRCD               =   SPACE )
               MOVE    2                   TO  FLG-PTINF-JFK
               PERFORM 20022-JUFUKU-HEN-SEC
               IF      IDX-JF          >   ZERO
      *            二重登録疑いあり
                   IF      WRK-ERRCD       =   SPACE
                       MOVE    "S"             TO  WRK-ERRKBN
                       MOVE    "20"            TO  WRK-ERRCD
                   END-IF
      *            確認メッセージ
                   MOVE    "0101"              TO  PTMODRES-SELECT-CODE
                   PERFORM 8902-SELECTMSG-SEC
               END-IF
           END-IF
      *
      *    患者登録処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20023-PTINFINS-SYORI-SEC
           END-IF
      *    同姓同名チェック
           IF     WRK-ERRCD            =   SPACE
               MOVE    1                   TO  FLG-PTINF-JFK
               PERFORM 20022-JUFUKU-HEN-SEC
               MOVE    SPA-PTID            TO  PTINF-PTID
           END-IF
      *
           IF     (WRK-ERRCD           =   SPACE )
             OR  ( WRK-ERRCD           =   "55"  )
      *        継続
               IF      FLG-CONTKBN         =   1
      *            保険登録へ
                   MOVE    "01"            TO  PTMODRES-RESPONSE-NUMBER
      *            継続区分終了
                   MOVE    "1"             TO  WRK-ENDKBN
      *            パラメタ登録
                   PERFORM 2901-PARA-INS-SEC
      *            排他チェック & ロック処理
                   PERFORM 990-LOCK-IN-SEC
                   IF      SLOCK-RETURN    NOT =   ZERO
                       MOVE    "90"        TO  WRK-ERRCD
                       MOVE    "01"        TO  PTMODRES-RESPONSE-NUMBER
                   END-IF
               ELSE
      *            パラメタ削除処理
                   PERFORM 2902-PARA-ALLDEL-SEC
                   MOVE    "00"            TO  PTMODRES-RESPONSE-NUMBER
               END-IF
           ELSE
      *        パラメタ登録
      *R02.1
               IF     (WRK-SELECT-CODE NOT =   SPACE )
                 AND  (PTMODRES-SELECT-CODE    =   SPACE )
                   MOVE    WRK-SELECT-CODE     TO  PTMODRES-SELECT-CODE
               END-IF
      *
               MOVE    SPACE           TO  WRK-ENDKBN
               PERFORM 2901-PARA-INS-SEC
           END-IF
      *
           .
       2002-PTINF-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ登録処理
      *****************************************************************
       2901-PARA-INS-SEC        SECTION.
      *
      *    パラメタ削除処理
           PERFORM 2902-PARA-ALLDEL-SEC
      *
      *    登録
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "AP12"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "API12V3"           TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
      *    電子カルテUID
           MOVE    PTMODREQ-KARTE-UID  TO  APIPARA-KARTE-UID
           MOVE    WRK-PTNUM           TO  APIPARA-PTNUM
      *    レスポンス番号
           MOVE    SPACE               TO  WRK-APIV3DATA-REC
           MOVE    PTMODRES-RESPONSE-NUMBER
                                       TO  WRK-RESPONSE-NUMBER
           MOVE    PTMODREQ-PATIENT-MODE
                                       TO  WRK-PATIENT-MODE
           MOVE    PTMODRES-SELECT-CODE
                                       TO  WRK-SELECT-CODE
      *    継続区分
           MOVE    WRK-ENDKBN          TO  WRK-PARA-ENDKBN
      *    処理ＰＧ
           MOVE    "01"                TO  WRK-PARA-SYORIPG
      *
           MOVE    WRK-APIV3DATA-REC   TO  APIPARA-DATA-REC
      *
           MOVE    SMCNDATE-YMD        TO  APIPARA-UPYMD
           MOVE    SMCNDATE-HMS        TO  APIPARA-UPHMS
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    "80"                TO  WRK-ERRCD
               DISPLAY "API PARA INSERT ERR:"  MCP-RC
                       ",KEY:" APIPARA-KEY
           END-IF
           .
       2901-PARA-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ削除処理
      *****************************************************************
       2902-PARA-ALLDEL-SEC          SECTION.
      *
      *    パラメタ削除
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "AP12"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       2902-PARA-ALLDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本チェック処理
      *****************************************************************
       200211-KIHON-CHK-SEC        SECTION.
      *
      *    氏名チェック
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 20011-NAME-CHK-SEC
           END-IF
      *    カナ氏名チェック
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 20011-KANANAME-CHK-SEC
           END-IF
      *    性別チェック
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 20011-SEX-CHK-SEC
           END-IF
      *    生年月日暦日チェック
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 20011-BIRTHDAY-CHK-SEC
           END-IF
      *
           .
       200211-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報処理
      *****************************************************************
       200211-PTINF-CHK-SEC        SECTION.
      *
      *    ＳＰＡ患者情報のセット
           PERFORM 200210-ORCSP02-INT-SEC
      *
      *    ＳＰＡ編集
      *    氏名
           MOVE    PTMODRES-NAME       TO  SPA-GMN-NAME
      *    カナ氏名
           MOVE    PTMODRES-KANANAME   TO  SPA-GMN-KANANAME
      *    生年月日
           MOVE    WRK-BIRTHDAY-YMD    TO  SPA-NAI-BIRTHDAY
      *    性別
           MOVE    PTMODREQ-SEX        TO  SPA-GMN-SEX
      *    世帯主名
           MOVE    PTMODREQ-SETAI-NAME TO  SPA-GMN-SETAINUSI
      *    通称名
           MOVE    PTMODREQ-NICKNAME   TO  SPA-GMN-NICKNAME
      *    続柄
           MOVE    PTMODREQ-ZOKUGARA   TO  SPA-GMN-ZOKUGARA
      *    職業
           MOVE    PTMODREQ-JOB        TO  SPA-GMN21-JOB
      *    ＦＡＸ番号
           MOVE    PTMODREQ-FAX        TO  SPA-GMN21-FAX
      *    携帯
           MOVE    PTMODREQ-KEITAI-TEL TO  SPA-GMN21-KEITAI-TEL
      *    メール
           MOVE    PTMODREQ-EMAIL      TO  SPA-GMN21-EMAIL
      *    患者自宅
           MOVE    PTMODREQ-HOME-ZIP-CODE  TO  SPA-GMN-POST
           MOVE    PTMODREQ-HOME-ADDRESSES1
                                           TO  SPA-GMN-EDITADRS-NAME
           MOVE    PTMODREQ-HOME-ADDRESSES2
                                           TO  SPA-GMN-BANTI
           MOVE    PTMODREQ-HOME-HPONE1    TO  SPA-GMN-TEL1
           MOVE    PTMODREQ-HOME-HPONE2    TO  SPA-GMN-TEL2
      *    勤務先情報
           MOVE    PTMODREQ-OFFICE-NAME    TO  SPA-GMN21-OFFICE-NAME
           MOVE    PTMODREQ-OFFICE-ZIP-CODE
                                           TO  SPA-GMN21-OFFICE-POST
           MOVE    PTMODREQ-OFFICE-ADDRESSES1
                                           TO  SPA-GMN21-OFFICE-ADRS
           MOVE    PTMODREQ-OFFICE-ADDRESSES2
                                           TO  SPA-GMN21-OFFICE-BANTI
           MOVE    PTMODREQ-OFFICE-HPONE   TO  SPA-GMN21-OFFICE-TEL
      *    連絡先情報
           MOVE    PTMODREQ-RENRAKU-NAME   TO  SPA-GMN21-RENRAKU-NAME
           MOVE    PTMODREQ-RENRAKU-ZOKUGARA
                                       TO  SPA-GMN21-RENRAKU-ZOKUGARA
           MOVE    PTMODREQ-RENRAKU-ZIP-CODE
                                           TO  SPA-GMN21-RENRAKU-POST
           MOVE    PTMODREQ-RENRAKU-ADDRESSES1
                                           TO  SPA-GMN21-RENRAKU-ADRS
           MOVE    PTMODREQ-RENRAKU-ADDRESSES2
                                           TO  SPA-GMN21-RENRAKU-BANTI
           MOVE    PTMODREQ-RENRAKU-HPONE1 TO  SPA-GMN21-RENRAKU-TEL1
           MOVE    PTMODREQ-RENRAKU-HPONE2 TO  SPA-GMN21-RENRAKU-TEL2
      *    帰省先情報
           MOVE    PTMODREQ-KISEI-NAME     TO  SPA-GMN21-KISEI-NAME
           MOVE    PTMODREQ-KISEI-ZIP-CODE
                                           TO  SPA-GMN21-KISEI-POST
           MOVE    PTMODREQ-KISEI-ADDRESSES1
                                           TO  SPA-GMN21-KISEI-ADRS
           MOVE    PTMODREQ-KISEI-ADDRESSES2
                                           TO  SPA-GMN21-KISEI-BANTI
           MOVE    PTMODREQ-KISEI-HPONE    TO  SPA-GMN21-KISEI-TEL
      *    禁忌
           MOVE    PTMODREQ-TABOO-1    TO  SPA-GMN-TABOO-1
           MOVE    PTMODREQ-TABOO-2    TO  SPA-GMN-TABOO-2
      *    アレルギー
           MOVE    PTMODREQ-ALLERGY-1  TO  SPA-GMN-ALLERGY-1
           MOVE    PTMODREQ-ALLERGY-2  TO  SPA-GMN-ALLERGY-2
      *    感染症
           MOVE    PTMODREQ-KANSENSYO-1    TO  SPA-GMN-KANSENSYO-1
           MOVE    PTMODREQ-KANSENSYO-2    TO  SPA-GMN-KANSENSYO-2
      *    コメント
           MOVE    PTMODREQ-COMMENT-1  TO  SPA-GMN-COMMENT-1
           MOVE    PTMODREQ-COMMENT-2  TO  SPA-GMN-COMMENT-2
      *    テスト患者区分
           MOVE    PTMODREQ-TEST-PATIENT   TO  SPA-GMN-TSTPTNUMKBN
      *    死亡区分
           MOVE    PTMODREQ-DEATHKBN   TO  SPA-GMN-DEATHKBN
      *    減免事由
           MOVE    PTMODREQ-DISCOUNTKBN    TO  SPA-GMN-DISCOUNTKBN
      *    割引率
           MOVE    PTMODREQ-DISCOUNT   TO  SPA-GMN-DISCOUNT-CD
      *    状態１〜３
           MOVE    PTMODREQ-CONDITION-KBN(1)
                                       TO  SPA-GMN-CONDITION-CD(1)
           MOVE    PTMODREQ-CONDITION-KBN(2)
                                       TO  SPA-GMN-CONDITION-CD(2)
           MOVE    PTMODREQ-CONDITION-KBN(3)
                                       TO  SPA-GMN-CONDITION-CD(3)
      *
      *    カナ氏名チェック（警告）
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-KANANAME-KEICHK-SEC
           END-IF
      *    生年月日
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-BIRTHDAY-CHK-SEC
           END-IF
      *    郵便番号
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-POST-CHK-SEC
           END-IF
      *    住所
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-ADRS-CHK-SEC
           END-IF
      *    番地方書
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-BANTI-CHK-SEC
           END-IF
      *    自宅電話番号
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-TEL1-CHK-SEC
           END-IF
      *    電話番号
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-TEL2-CHK-SEC
           END-IF
      *    世帯主
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-SETAINUSI-CHK-SEC
           END-IF
      *    続柄
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-ZOKUGARA-CHK-SEC
           END-IF
      *    通称名
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-NICKNAME-CHK-SEC
           END-IF
      *    禁忌他
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-COMMENT-CHK-SEC
           END-IF
      *
      *    仕事
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-JOB-CHK-SEC
           END-IF
      *    携帯電話番号
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-KEITAITEL-CHK-SEC
           END-IF
      *    ＦＡＸ番号
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-FAX-CHK-SEC
           END-IF
      *    電子メールアドレス
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-EMAIL-CHK-SEC
           END-IF
      *
      *    勤務先−名前
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-OFFNAME-CHK-SEC
           END-IF
      *
      *    郵便番号
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-OFFPOST-CHK-SEC
           END-IF
      *
      *    勤務先−住所
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-OFFADRS-CHK-SEC
           END-IF
      *    勤務先−番地
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-OFFBANTI-CHK-SEC
           END-IF
      *    勤務先−電話
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-OFFICETEL-CHK-SEC
           END-IF
      *
      *連絡先情報
      *    連絡先名
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-RENRAKU-NAME-SEC
           END-IF
      *    連絡先続柄
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-RENRAKU-ZOKUGARA-SEC
           END-IF
      *
      *    郵便番号
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-RENRAKU-POST-SEC
           END-IF
      *    住所
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-RENRAKU-ADRS-SEC
           END-IF
      *    番地
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-RENRAKU-BANTI-SEC
           END-IF
      *    電話
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-RENRAKU-TEL-SEC
           END-IF
      *帰省先
      *    帰省先名
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-KISNAME-CHK-SEC
           END-IF
      *
      *    郵便番号
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-KISPOST-CHK-SEC
           END-IF
      *    住所
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-KISADRS-CHK-SEC
           END-IF
      *    番地
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-KISBANTI-CHK-SEC
           END-IF
      *    電話
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-KISTEL-CHK-SEC
           END-IF
      *
      *    テスト患者番号区分
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-TSTPTNUMKBN-CHK-SEC
           END-IF
      *
      *    死亡区分
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-DEATHKBN-CHK-SEC
           END-IF
      *
      *    減免事由
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-DISCOUNTKBN-CHK-SEC
           END-IF
      *
      *    割引率
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-DISCOUNT-CHK-SEC
           END-IF
      *
      *    状態
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-CONDITION-CHK-SEC
           END-IF
      *
           IF     (WRK-ERRCD           =   SPACE )
               IF      IDX-ERR         >   ZERO
      *        エラー有
                   MOVE    "50"            TO  WRK-ERRCD
                   MOVE    "E"             TO  WRK-ERRKBN
                   MOVE    "送信内容にエラーがあります"
                                           TO  WRK-ERRMSG
               END-IF
           END-IF
           .
       200211-PTINF-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    エラーメッセージ編集処理
      ****************************************************************
       200210-ORCSP02-INT-SEC           SECTION.
      *
           MOVE    SPACE               TO  P02
           INITIALIZE                      P02
      *
           MOVE    SPA-GMN-NAME        TO  SPA-NAME
           MOVE    ZERO                TO  SPA-P02-UKEID
           MOVE    1                   TO  SPA-P02-SYORI
           MOVE    "API"               TO  ORCSP02-SYORI
           CALL    "ORCSP02"           USING   ORCSP02AREA
                                               SPA-AREA
                                               SPA-P02KYOUTU
                                               SPA-P02SCRAREA
                                               P02
           MOVE    SPACE               TO  SPA-ERRCD
      *    公費法別一覧編集区分なし
           MOVE    "0"                 TO  SPA-NAI-KOHHBTKBN-FLG
           .
       200210-ORCSP02-INT-EXT.
           EXIT.
      *
      ****************************************************************
      *    エラーメッセージ編集処理
      ****************************************************************
       290-ERROR-HEN-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           IF      SPA-ERRCD(1:1)      =   "A"
               PERFORM 8901-PTERRCD-SETAPI-SEC
           ELSE
               PERFORM 8901-PTERRCD-SET-SEC
           END-IF
           IF      WRK-ERRCD           =   SPACE
               MOVE    "P1"                TO  WRK-ERRCD
           END-IF
           ADD     1                   TO  IDX-ERR
           IF      IDX-ERR             <=  50
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "AP"            TO  PTMODRES-ERROR
                                                       (IDX-ERR)(1:2)
                   MOVE    WRK-ERRCD       TO  PTMODRES-ERROR
                                                       (IDX-ERR)(3:2)
               ELSE
                   MOVE    SPA-ERRCD       TO  PTMODRES-ERROR
                                                           (IDX-ERR)
               END-IF
               MOVE    WRK-ERRMSG          TO  PTMODRES-ERROR-MSG
                                                           (IDX-ERR)
           END-IF
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-ERRMSG
           MOVE    SPACE               TO  SPA-ERRCD
           .
       290-ERROR-HEN-EXT.
           EXIT.
      *
      ****************************************************************
      *    カナ氏名チェック（警告）処理
      ****************************************************************
       200211-KANANAME-KEICHK-SEC           SECTION.
      *
           MOVE    SPA-GMN-KANANAME    TO  SPA-MAE-KANANAME
      *    カタカナのみの判定
           MOVE    ZERO                TO  FLG-KANA
           PERFORM VARYING     IDX     FROM    1   BY  2
                   UNTIL      (IDX     >=   50  )  OR
                              (SPA-GMN-KANANAME(IDX:1) =  SPACE)
               IF     (SPA-GMN-KANANAME (IDX:2)
                                       =   "　" 
                                       OR  "ー" 
                                       OR  "−"  )  OR
                      (SPA-GMN-KANANAME (IDX:2)
                                       >=  "ァ"  AND
                                       <=  "ヶ"   )  OR
                      (SPA-GMN-KANANAME (IDX:2)
                                       >=  "０"  AND
                                       <=  "ｚ"   )
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-KANA
               END-IF
           END-PERFORM
           IF      FLG-KANA            =   1
               ADD     1                   TO  IDX-W
               MOVE    "K4"                TO  WRK-KEICD(IDX-W)
           END-IF
           .
       200211-KANANAME-KEICHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    生年月日チェック処理
      ****************************************************************
       200211-BIRTHDAY-CHK-SEC           SECTION.
      *
      *    システム日付とのチェック
           IF      SPA-NAI-BIRTHDAY    >   SPA-SYSYMD
               MOVE    "0020"          TO  SPA-ERRCD
               PERFORM 290-ERROR-HEN-SEC
               GO      TO      200211-BIRTHDAY-CHK-EXT
           END-IF
      *    年齢計算
           MOVE    SPA-NAI-BIRTHDAY    TO  WRK-YMDS1
           MOVE    SPA-SYSYMD          TO  WRK-YMDS2
           COMPUTE WRK-NENREI-YY       =   WRK-YYS2
                                       -   WRK-YYS1
           IF      WRK-YMDS1(5:4)      >   WRK-YMDS2(5:4)
               COMPUTE WRK-NENREI-YY   =   WRK-NENREI-YY
                                           -   1
               COMPUTE WRK-NENREI-MM   =  (WRK-MMS2  +  12 )
                                           -   WRK-MMS1
           ELSE
               COMPUTE WRK-NENREI-MM   =   WRK-MMS2
                                           -   WRK-MMS1
           END-IF
           IF      WRK-DDS1            >   WRK-DDS2
               COMPUTE WRK-NENREI-MM   =   WRK-NENREI-MM
                                           -   1
           END-IF
      *
           MOVE    WRK-NENREI-YY       TO  SPA-NAI-NENREI-YY
           MOVE    WRK-NENREI-MM       TO  SPA-NAI-NENREI-MM
      *
      *    ７０歳到達日
      *    生年月日から基準日を求める
      *    １日ならば生年月日、１日以外ならば生年月日の翌月１日
           MOVE    SPA-NAI-BIRTHDAY    TO  WRK-BASEYMD
           IF      WRK-BDD         NOT =   01
               COMPUTE WRK-BMM         =   WRK-BMM     +   1
               IF      WRK-BMM         >   12
                   COMPUTE WRK-BYY     =   WRK-BYY     +   1
                   MOVE    01              TO  WRK-BMM
               END-IF
               MOVE    01                  TO  WRK-BDD
           END-IF
      *    ６５歳を迎える日
           MOVE    WRK-BASEYMD         TO  SPA-BYMD-65
           COMPUTE SPA-B65-YY          =   SPA-B65-YY  +   65
      *    ７０歳を迎える日
           MOVE    WRK-BASEYMD         TO  SPA-BYMD-70
           COMPUTE SPA-B70-YY          =   SPA-B70-YY  +   70
      *    ７５歳を迎える日
           MOVE    WRK-BASEYMD         TO  SPA-BYMD-75
           COMPUTE SPA-B75-YY          =   SPA-B75-YY  +   75
      *
      *    ７５歳の誕生日（Ｈ２０．４から）
           MOVE    SPA-NAI-BIRTHDAY    TO  WRK-BASEYMD
           MOVE    WRK-BASEYMD         TO  SPA-BYMD-75-2
           COMPUTE SPA-B75-YY2         =   SPA-B75-YY2  +   75
           IF      WRK-BASEYMD(5:4)    =   "0229"
               MOVE    "0301"              TO  SPA-BYMD-75-2(5:4)
           END-IF
      *
           .
       200211-BIRTHDAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    郵便番号 チェック処理
      *****************************************************************
       200211-POST-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-POST        =   SPACE
               GO      TO      200211-POST-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-POST        TO  WRK-MAE-POST
      *    郵便番号より、住所一覧を表示する
           PERFORM 200212-POST-SYORI-SEC
           IF      WRK-ERRCD           =   "P1"
               MOVE    SPACE               TO  WRK-ERRCD
               MOVE    "0131"              TO  SPA-ERRCD
               PERFORM 290-ERROR-HEN-SEC
               GO  TO  200211-POST-CHK-EXT
           END-IF
      *
      *    対象なし（警告とする）
           IF     (SPA-GMN982-MAX      =   ZERO  )
             AND  (WRK-ERRCD           =   SPACE )
               ADD     1                   TO  IDX-W
               MOVE    "0031"              TO  WRK-KEIERRCD (IDX-W)
               GO  TO  200211-POST-CHK-EXT
           END-IF
      *    １件で住所なしは編集
           IF     (SPA-GMN982-MAX      =   1  )
             AND  (SPA-GMN-EDITADRS-NAME   =   SPACE )
             AND  (WRK-ERRCD           =   SPACE )
      *        住所検索編集
               PERFORM 30011-P982SET-SEC
           END-IF
      *
      *    IF      WRK-ERRCD       NOT =   SPACE
      *        PERFORM 290-ERROR-HEN-SEC
      *    END-IF
      *
           .
       200211-POST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    住所確認編集処理
      *****************************************************************
       200212-POST-SYORI-SEC              SECTION.
      *
           INITIALIZE                  SPA-P982-AREA
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-POST        TO  KANACHK-MAE-INPUT
           MOVE    7                   TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-MAE-INPUT
           IF     (KANACHK-RC          =   ZERO  )  AND
                  (KANACHK-SYUBETU     =   1     )
      *R01.8
            AND   (KANACHK-RC4         =   ZERO  )
      *        半角であること
               CONTINUE
           ELSE
               MOVE    "P1"                TO  WRK-ERRCD
               GO      TO      200212-POST-SYORI-EXT
           END-IF
      *    郵便番号より、住所一覧を表示する
           INITIALIZE                  SPA-P982-AREA
           MOVE    WRK-MAE-POST        TO  SPA-GMN982-POST
           MOVE    "P02"               TO  SPA-GMN982-MOTOPG
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP982"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           .
       200212-POST-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    住所確認編集処理
      *****************************************************************
       30011-P982SET-SEC              SECTION.
      *
           IF      SPA-GMN982-POST     NOT =   SPACE
               MOVE    SPA-GMN982-POST     TO  SPA-GMN-POST
           END-IF
      *
           IF      SPA-GMN982-TOWNNAME NOT =   SPACE
               IF      SPA-NAI-KENMEIKBN-FLG   =   "0"
      *        都道府県名なし
                   MOVE    SPA-GMN982-CITYNAMET
                                           TO  SPA-GMN-EDITADRS-NAME
               ELSE
      *        都道府県名あり
                   MOVE    SPA-GMN982-TOWNNAME
                                           TO  SPA-GMN-EDITADRS-NAME
               END-IF
           END-IF
           MOVE    SPA-GMN-EDITADRS-NAME   TO  SPA-MAE-ADRS
           MOVE    SPA-GMN-POST            TO  SPA-MAE-POST
           .
       30011-P982SET-EXT.
           EXIT.
      *****************************************************************
      *    住所チェック処理
      *****************************************************************
       200211-ADRS-CHK-SEC         SECTION.
      *
           IF      SPA-GMN-EDITADRS-NAME       =   SPACE
               MOVE    SPA-GMN-EDITADRS-NAME   TO  SPA-MAE-ADRS
           ELSE
      *        半角を全角へ
               MOVE    SPA-GMN-EDITADRS-NAME   TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-EDITADRS-NAME
               IF      FLG-ERR             =   1
                   MOVE    "0032"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               ELSE
      *            郵便番号自動設定（郵便番号未設定のみ）
                   IF     (SPA-GMN-POST        =   SPACE  )
                    AND   (SPA-NAI-POSTKBN-FLG =   "1"
                                               OR  "2"    )
                       MOVE    SPA-GMN-EDITADRS-NAME   TO  WRK-ADRS
                       PERFORM 410121-ADRS-POST-SEC
                       MOVE    WRK-POST            TO  SPA-GMN-POST
                   END-IF
               END-IF
           END-IF
           .
       200211-ADRS-CHK-EXT.
           EXIT.
      *****************************************************************
      *    住所から郵便番号編集処理
      *****************************************************************
       410121-ADRS-POST-SEC         SECTION.
      *
      *    住所より郵便番号を検索
           INITIALIZE                  SPA-P982-AREA
           MOVE    2                   TO  SPA-GMN982-KBN
           MOVE    WRK-ADRS            TO  SPA-GMN982-TOWNNAME
           MOVE    "P02"               TO  SPA-GMN982-MOTOPG
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP982"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
           MOVE    SPACE               TO  WRK-POST
      *    １件の時、対象
           IF      SPA-GMN982-MAX      =   1
               MOVE    SPA-GMN982-POST     TO  WRK-POST
           END-IF
      *
           .
       410121-ADRS-POST-EXT.
           EXIT.
      *****************************************************************
      *    番地チェック処理
      *****************************************************************
       200211-BANTI-CHK-SEC         SECTION.
      *
           IF      SPA-GMN-BANTI   NOT =   SPACE
      *        半角を全角へ
               MOVE    SPA-GMN-BANTI       TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-BANTI
               IF      FLG-ERR             =   1
                   MOVE    "0032"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           .
       200211-BANTI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    電話番号チェック処理
      *****************************************************************
       200211-TEL1-CHK-SEC         SECTION.
      *
           IF      SPA-GMN-TEL1    NOT =   SPACE
      *        半角であること
               MOVE    SPA-GMN-TEL1        TO  WRK-MAE-INPUT
               MOVE    15                  TO  WRK-MAX
               PERFORM 891-HALF-CHK-SEC
               IF      FLG-ERR         NOT =   ZERO
                   MOVE    "A020"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           .
       200211-TEL1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    電話番号チェック処理
      *****************************************************************
       200211-TEL2-CHK-SEC         SECTION.
      *
           IF      SPA-GMN-TEL2    NOT =   SPACE
      *        半角であること
               MOVE    SPA-GMN-TEL2        TO  WRK-MAE-INPUT
               MOVE    15                  TO  WRK-MAX
               PERFORM 891-HALF-CHK-SEC
               IF      FLG-ERR         NOT =   ZERO
                   MOVE    "A021"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           .
       200211-TEL2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    世帯主チェック処理
      *****************************************************************
       200211-SETAINUSI-CHK-SEC         SECTION.
      *
           IF      SPA-GMN-SETAINUSI   NOT =   SPACE
      *        半角空白を全角空白へ
               MOVE    SPA-GMN-SETAINUSI   TO  WRK-MAE-INPUT
               MOVE    50                  TO  WRK-LEN
               PERFORM 891-HALF-ZEN2-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-SETAINUSI
               IF      FLG-ERR             =   1
                   MOVE    "0033"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           .
       200211-SETAINUSI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    続柄チェック処理
      *****************************************************************
       200211-ZOKUGARA-CHK-SEC         SECTION.
      *
           IF      SPA-GMN-ZOKUGARA    NOT =   SPACE
      *        半角を全角へ
               MOVE    SPA-GMN-ZOKUGARA    TO  WRK-MAE-INPUT
               MOVE    30                  TO  WRK-LEN
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-ZOKUGARA
               IF      FLG-ERR             =   1
                   MOVE    "0034"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           .
       200211-ZOKUGARA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    職業チェック処理
      *****************************************************************
       200211-JOB-CHK-SEC         SECTION.
      *
           IF      SPA-GMN21-JOB    NOT =   SPACE
      *        拡張文字チェック
               MOVE    SPA-GMN21-JOB       TO  WRK-MAE-INPUT
               MOVE    40                  TO  WRK-LEN
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN21-JOB
               IF      FLG-ERR             =   1
      ***      IF      FLG-ERR2            =   1
      ****         拡張文字エラー
                   MOVE    "A022"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           .
       200211-JOB-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    通称名チェック処理
      *****************************************************************
       200211-NICKNAME-CHK-SEC         SECTION.
      *
           IF      SPA-GMN-NICKNAME    NOT =   SPACE
      *        半角空白を全角空白へ
               MOVE    SPA-GMN-NICKNAME    TO  WRK-MAE-INPUT
               MOVE    50                  TO  WRK-LEN
               PERFORM 891-HALF-ZEN2-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-NICKNAME
               IF      FLG-ERR             =   1
                   MOVE    "0027"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           .
       200211-NICKNAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    携帯電話番号チェック処理
      *****************************************************************
       200211-KEITAITEL-CHK-SEC         SECTION.
      *
           IF      SPA-GMN21-KEITAI-TEL  NOT =   SPACE
      *        半角であること
               MOVE    SPA-GMN21-KEITAI-TEL    TO  WRK-MAE-INPUT
               MOVE    15                  TO  WRK-MAX
               PERFORM 891-HALF-CHK-SEC
               IF      FLG-ERR         NOT =   ZERO
                   MOVE    "2118"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           .
       200211-KEITAITEL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ＦＡＸ番号チェック処理
      *****************************************************************
       200211-FAX-CHK-SEC         SECTION.
      *
           IF      SPA-GMN21-FAX       NOT =   SPACE
      *        半角であること
               MOVE    SPA-GMN21-FAX       TO  WRK-MAE-INPUT
               MOVE    15                  TO  WRK-MAX
               PERFORM 891-HALF-CHK-SEC
               IF      FLG-ERR         NOT =   ZERO
                   MOVE    "2117"          TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           .
       200211-FAX-CHK-EXT.
           EXIT.
      *****************************************************************
      *    電子メールアドレスチェック処理
      *****************************************************************
       200211-EMAIL-CHK-SEC         SECTION.
      *
      **画面チェックなし
           IF      SPA-GMN21-EMAIL     NOT =   SPACE
      *        拡張文字変換と改行チェック
               MOVE    SPA-GMN21-EMAIL     TO  WRK-MAE-INPUT
               MOVE    50                  TO  WRK-MAX
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN21-EMAIL
               IF      FLG-ERR3            =   1
                   MOVE    "A023"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           .
       200211-EMAIL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌他チェック処理
      *****************************************************************
       200211-COMMENT-CHK-SEC          SECTION.
      *
      *    すべて半角を全角へ
      *    禁忌
           IF      SPA-GMN-TABOO-1     NOT =   SPACE
               MOVE    SPA-GMN-TABOO-1     TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-TABOO-1
               IF      FLG-ERR             =   1
                   MOVE    "A024"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           IF     (SPA-GMN-TABOO-2     NOT =   SPACE ) AND
                  (WRK-ERRCD               =   SPACE )
               MOVE    SPA-GMN-TABOO-2     TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-TABOO-2
               IF      FLG-ERR             =   1
                   MOVE    "A025"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
      *    アレルギー
           IF     (SPA-GMN-ALLERGY-1     NOT =   SPACE ) AND
                  (WRK-ERRCD               =   SPACE )
               MOVE    SPA-GMN-ALLERGY-1   TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-ALLERGY-1
               IF      FLG-ERR             =   1
                   MOVE    "A026"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           IF     (SPA-GMN-ALLERGY-2     NOT =   SPACE ) AND
                  (WRK-ERRCD               =   SPACE )
               MOVE    SPA-GMN-ALLERGY-2   TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-ALLERGY-2
               IF      FLG-ERR             =   1
                   MOVE    "A027"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
      *    感染症
           IF     (SPA-GMN-KANSENSYO-1 NOT =   SPACE ) AND
                  (WRK-ERRCD               =   SPACE )
               MOVE    SPA-GMN-KANSENSYO-1 TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-KANSENSYO-1
               IF      FLG-ERR             =   1
                   MOVE    "A028"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           IF     (SPA-GMN-KANSENSYO-2 NOT =   SPACE ) AND
                  (WRK-ERRCD               =   SPACE )
               MOVE    SPA-GMN-KANSENSYO-2 TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-KANSENSYO-2
               IF      FLG-ERR             =   1
                   MOVE    "A029"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
      *    コメント
           IF     (SPA-GMN-COMMENT-1   NOT =   SPACE ) AND
                  (WRK-ERRCD               =   SPACE )
               MOVE    SPA-GMN-COMMENT-1   TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
      ****     PERFORM 880-HALF-ZENCNG-SEC
      ****     MOVE    WRK-ATO-INPUT       TO  SPA-GMN-COMMENT-1
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-COMMENT-1
               IF      FLG-ERR3            =   1
                   MOVE    "A030"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           IF     (SPA-GMN-COMMENT-2   NOT =   SPACE ) AND
                  (WRK-ERRCD               =   SPACE )
               MOVE    SPA-GMN-COMMENT-2   TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
      ****     PERFORM 880-HALF-ZENCNG-SEC
      ****     MOVE    WRK-ATO-INPUT       TO  SPA-GMN-COMMENT-2
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-COMMENT-2
               IF      FLG-ERR3            =   1
                   MOVE    "A031"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           .
       200211-COMMENT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    勤務先−名前チェック処理
      *****************************************************************
       200211-OFFNAME-CHK-SEC          SECTION.
      *
      *    半角を全角へ
           IF      SPA-GMN21-OFFICE-NAME   NOT =   SPACE
               MOVE    SPA-GMN21-OFFICE-NAME   TO  WRK-MAE-INPUT
               MOVE    50                  TO  WRK-MAX
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN21-OFFICE-NAME
               IF      FLG-ERR             =   ZERO
                   MOVE    WRK-ATO-INPUT       TO  SPA-GMN21-OFFICE-NAME
               ELSE
                   MOVE    "2105"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           .
      *
       200211-OFFNAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    勤務先−郵便番号チェック処理
      *****************************************************************
       200211-OFFPOST-CHK-SEC          SECTION.
      *
           IF      SPA-GMN21-OFFICE-POST  =   SPACE
               GO      TO      200211-OFFPOST-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN21-OFFICE-POST  TO  WRK-MAE-POST
      *
           PERFORM 200212-POST-SYORI-SEC
           IF      WRK-ERRCD           =   "P1"
               MOVE    SPACE               TO  WRK-ERRCD
               MOVE    "2112"              TO  SPA-ERRCD
               PERFORM 290-ERROR-HEN-SEC
               GO  TO  200211-OFFPOST-CHK-EXT
           END-IF
      *    対象なし
           IF     (SPA-GMN982-MAX      =   ZERO  )
             AND  (WRK-ERRCD           =   SPACE )
               ADD     1                   TO  IDX-W
               MOVE    "2106"              TO  WRK-KEIERRCD(IDX-W)
               GO  TO  200211-OFFPOST-CHK-EXT
           END-IF
      *    １件で住所なしは編集
           IF     (SPA-GMN982-MAX      =   1  )
             AND  (SPA-GMN21-OFFICE-ADRS   =   SPACE )
             AND  (WRK-ERRCD           =   SPACE )
      *        住所編集
               MOVE    SPA-GMN982-TOWNNAME TO  SPA-GMN21-OFFICE-ADRS
           END-IF
           .
       200211-OFFPOST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    勤務先−住所チェック処理
      *****************************************************************
       200211-OFFADRS-CHK-SEC          SECTION.
      *
           IF      SPA-GMN21-OFFICE-ADRS   =   SPACE
               MOVE    SPA-GMN21-OFFICE-ADRS   TO  SPA-MAE-OFFICE-ADRS
           ELSE
      *        半角を全角へ
               MOVE    SPA-GMN21-OFFICE-ADRS   TO  WRK-MAE-INPUT
               MOVE    100                     TO  WRK-MAX
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN21-OFFICE-ADRS
               IF      FLG-ERR             =   ZERO
                   MOVE    WRK-ATO-INPUT   TO  SPA-GMN21-OFFICE-ADRS
      *            郵便番号自動設定（郵便番号未設定のみ）
                   IF     (SPA-GMN21-OFFICE-POST   =   SPACE  )
                    AND   (SPA-NAI-POSTKBN-FLG     =   "1"
                                                   OR  "2"    )
                       MOVE    SPA-GMN21-OFFICE-ADRS   TO  WRK-ADRS
                       PERFORM 410121-ADRS-POST-SEC
                       MOVE    WRK-POST        TO  SPA-GMN21-OFFICE-POST
                   END-IF
               ELSE
                   MOVE    "2107"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           .
       200211-OFFADRS-CHK-EXT.
           EXIT.
      *****************************************************************
      *    勤務先−住所チェック処理
      *****************************************************************
       200211-OFFBANTI-CHK-SEC          SECTION.
      *
      *    半角を全角へ
           MOVE    SPA-GMN21-OFFICE-BANTI  TO  WRK-MAE-INPUT
           MOVE    100                 TO  WRK-MAX
           PERFORM 880-HALF-ZENCNG-SEC
           MOVE    WRK-ATO-INPUT       TO  SPA-GMN21-OFFICE-BANTI
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN21-OFFICE-BANTI
           ELSE
               MOVE    "2107"              TO  SPA-ERRCD
               PERFORM 290-ERROR-HEN-SEC
           END-IF
           .
       200211-OFFBANTI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    電話番号チェック処理
      *****************************************************************
       200211-OFFICETEL-CHK-SEC         SECTION.
      *
           IF      SPA-GMN21-OFFICE-TEL    NOT =   SPACE
      *        半角であること
               MOVE    SPA-GMN21-OFFICE-TEL    TO  WRK-MAE-INPUT
               MOVE    15                  TO  WRK-MAX
               PERFORM 891-HALF-CHK-SEC
               IF      FLG-ERR         NOT =   ZERO
                   MOVE    "2115"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           .
       200211-OFFICETEL-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    連絡先情報−名前チェック処理
      *****************************************************************
       200211-RENRAKU-NAME-SEC          SECTION.
      *
      *    半角を全角へ
           IF      SPA-GMN21-RENRAKU-NAME   NOT =   SPACE
               MOVE    SPA-GMN21-RENRAKU-NAME  TO  WRK-MAE-INPUT
               MOVE    50                  TO  WRK-MAX
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN21-RENRAKU-NAME
               IF      FLG-ERR             =   ZERO
                   MOVE    WRK-ATO-INPUT   TO  SPA-GMN21-RENRAKU-NAME
               ELSE
                   MOVE    "2101"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           .
      *
       200211-RENRAKU-NAME-EXT.
           EXIT.
      *
      *****************************************************************
      *    連絡先情報−続柄チェック処理
      *****************************************************************
       200211-RENRAKU-ZOKUGARA-SEC          SECTION.
      *
      *    半角を全角へ
           IF      SPA-GMN21-RENRAKU-ZOKUGARA  NOT =   SPACE
               MOVE    SPA-GMN21-RENRAKU-ZOKUGARA  TO  WRK-MAE-INPUT
               MOVE    50                  TO  WRK-MAX
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO
                                           SPA-GMN21-RENRAKU-ZOKUGARA
               IF      FLG-ERR             =   ZERO
                   MOVE    WRK-ATO-INPUT   TO
                                           SPA-GMN21-RENRAKU-ZOKUGARA
               ELSE
                   MOVE    "2102"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           .
      *
       200211-RENRAKU-ZOKUGARA-EXT.
           EXIT.
      *
      *****************************************************************
      *    連絡先情報−郵便番号チェック処理
      *****************************************************************
       200211-RENRAKU-POST-SEC          SECTION.
      *
           IF      SPA-GMN21-RENRAKU-POST  =   SPACE
               GO      TO      200211-RENRAKU-POST-EXT
           END-IF
      *
           MOVE    SPA-GMN21-RENRAKU-POST  TO  WRK-MAE-POST
      *
           PERFORM 200212-POST-SYORI-SEC
           IF      WRK-ERRCD           =   "P1"
               MOVE    SPACE               TO  WRK-ERRCD
               MOVE    "2111"          TO  SPA-ERRCD
               PERFORM 290-ERROR-HEN-SEC
               GO      TO      200211-RENRAKU-POST-EXT
           END-IF
      *    対象なし
           IF     (SPA-GMN982-MAX      =   ZERO  )
             AND  (WRK-ERRCD           =   SPACE )
               ADD     1                   TO  IDX-W
               MOVE    "2103"              TO  WRK-KEIERRCD(IDX-W)
               GO      TO      200211-RENRAKU-POST-EXT
           END-IF
      *    １件で住所なしは編集
           IF     (SPA-GMN982-MAX      =   1  )
             AND  (SPA-GMN21-RENRAKU-ADRS  =   SPACE )
             AND  (WRK-ERRCD           =   SPACE )
      *        住所編集
               MOVE    SPA-GMN982-TOWNNAME TO  SPA-GMN21-RENRAKU-ADRS
           END-IF
           .
       200211-RENRAKU-POST-EXT.
           EXIT.
      *****************************************************************
      *    連絡先−住所チェック処理
      *****************************************************************
       200211-RENRAKU-ADRS-SEC          SECTION.
      *
           IF      SPA-GMN21-RENRAKU-ADRS  =   SPACE
               MOVE    SPA-GMN21-RENRAKU-ADRS  TO  SPA-MAE-RENRAKU-ADRS
           ELSE
      *        半角を全角へ
               MOVE    SPA-GMN21-RENRAKU-ADRS  TO  WRK-MAE-INPUT
               MOVE    100                     TO  WRK-MAX
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN21-RENRAKU-ADRS
               IF      FLG-ERR             =   ZERO
                   MOVE    WRK-ATO-INPUT   TO  SPA-GMN21-RENRAKU-ADRS
      *            郵便番号自動設定（郵便番号未設定のみ）
                   IF     (SPA-GMN21-RENRAKU-POST  =   SPACE  )
                    AND   (SPA-NAI-POSTKBN-FLG     =   "1"
                                                   OR  "2"    )
                       MOVE    SPA-GMN21-RENRAKU-ADRS  TO  WRK-ADRS
                       PERFORM 410121-ADRS-POST-SEC
                       MOVE    WRK-POST    TO  SPA-GMN21-RENRAKU-POST
                   END-IF
               ELSE
                   MOVE    "2104"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           .
       200211-RENRAKU-ADRS-EXT.
           EXIT.
      *****************************************************************
      *    連絡先−住所チェック処理
      *****************************************************************
       200211-RENRAKU-BANTI-SEC          SECTION.
      *
      *    半角を全角へ
           MOVE    SPA-GMN21-RENRAKU-BANTI  TO  WRK-MAE-INPUT
           MOVE    100                 TO  WRK-MAX
           PERFORM 880-HALF-ZENCNG-SEC
           MOVE    WRK-ATO-INPUT       TO  SPA-GMN21-RENRAKU-BANTI
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN21-RENRAKU-BANTI
           ELSE
               MOVE    "2104"              TO  SPA-ERRCD
               PERFORM 290-ERROR-HEN-SEC
           END-IF
           .
       200211-RENRAKU-BANTI-EXT.
           EXIT.
      *****************************************************************
      *    電話番号チェック処理
      *****************************************************************
       200211-RENRAKU-TEL-SEC         SECTION.
      *
           IF      SPA-GMN21-RENRAKU-TEL1  NOT =   SPACE
      *        半角であること
               MOVE    SPA-GMN21-RENRAKU-TEL1  TO  WRK-MAE-INPUT
               MOVE    15                  TO  WRK-MAX
               PERFORM 891-HALF-CHK-SEC
               IF      FLG-ERR         NOT =   ZERO
                   MOVE    "2114"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
      *
           IF      SPA-GMN21-RENRAKU-TEL2  NOT =   SPACE
      *        半角であること
               MOVE    SPA-GMN21-RENRAKU-TEL2  TO  WRK-MAE-INPUT
               MOVE    15                  TO  WRK-MAX
               PERFORM 891-HALF-CHK-SEC
               IF      FLG-ERR         NOT =   ZERO
                   MOVE    "2114"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           .
       200211-RENRAKU-TEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    帰省先−名前チェック処理
      *****************************************************************
       200211-KISNAME-CHK-SEC          SECTION.
      *
           IF      SPA-GMN21-KISEI-NAME    NOT =   SPACE
      *    半角空白を全角空白へ
               MOVE    SPA-GMN21-KISEI-NAME    TO  WRK-MAE-INPUT
               MOVE    50                  TO  WRK-MAX
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN21-KISEI-NAME
               IF      FLG-ERR             =   ZERO
                   MOVE    WRK-ATO-INPUT       TO  SPA-GMN21-KISEI-NAME
               ELSE
                   MOVE    "2108"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           .
      *
       4101-KISNAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    帰省先−郵便番号チェック処理
      *****************************************************************
       200211-KISPOST-CHK-SEC          SECTION.
      *
           IF      SPA-GMN21-KISEI-POST  =   SPACE
               GO      TO      200211-KISPOST-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN21-KISEI-POST  TO  WRK-MAE-POST
           PERFORM 200212-POST-SYORI-SEC
           IF      WRK-ERRCD           =   "P1"
               MOVE    SPACE               TO  WRK-ERRCD
               MOVE    "2113"          TO  SPA-ERRCD
               PERFORM 290-ERROR-HEN-SEC
               GO      TO      200211-KISPOST-CHK-EXT
           END-IF
      *    対象なし
           IF     (SPA-GMN982-MAX      =   ZERO  )
             AND  (WRK-ERRCD           =   SPACE )
               ADD     1                   TO  IDX-W
               MOVE    "2109"              TO  WRK-KEIERRCD(IDX-W)
               GO      TO      200211-KISPOST-CHK-EXT
           END-IF
      *    １件で住所なしは編集
           IF     (SPA-GMN982-MAX      =   1  )
             AND  (SPA-GMN21-KISEI-ADRS    =   SPACE )
             AND  (WRK-ERRCD           =   SPACE )
      *        住所編集
               MOVE    SPA-GMN982-TOWNNAME TO  SPA-GMN21-KISEI-ADRS
           END-IF
           .
       200211-KISPOST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    帰省先−住所チェック処理
      *****************************************************************
       200211-KISADRS-CHK-SEC          SECTION.
      *
           IF      SPA-GMN21-KISEI-ADRS    =   SPACE
               MOVE    SPA-GMN21-KISEI-ADRS    TO  SPA-MAE-KISEI-ADRS
           ELSE
      *        半角を全角へ
               MOVE    SPA-GMN21-KISEI-ADRS    TO  WRK-MAE-INPUT
               MOVE    100                     TO  WRK-MAX
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN21-KISEI-ADRS
               IF      FLG-ERR             =   ZERO
                   MOVE    WRK-ATO-INPUT   TO  SPA-GMN21-KISEI-ADRS
      *            郵便番号自動設定
                   IF     (SPA-GMN21-KISEI-POST    =   SPACE  )
                    AND   (SPA-NAI-POSTKBN-FLG     =   "1"
                                                   OR  "2"    )
                       MOVE    SPA-GMN21-KISEI-ADRS   TO  WRK-ADRS
                       PERFORM 410121-ADRS-POST-SEC
                       MOVE    WRK-POST        TO  SPA-GMN21-KISEI-POST
                   END-IF
               ELSE
                   MOVE    "2110"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           .
      *
       200211-KISADRS-CHK-EXT.
           EXIT.
      *****************************************************************
      *    帰省先−住所チェック処理
      *****************************************************************
       200211-KISBANTI-CHK-SEC          SECTION.
      *
           IF      SPA-GMN21-KISEI-BANTI   NOT =   SPACE
      *        半角を全角へ
               MOVE    SPA-GMN21-KISEI-BANTI   TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-MAX
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN21-KISEI-BANTI
               IF      FLG-ERR             =   ZERO
                   MOVE    WRK-ATO-INPUT       TO  SPA-GMN21-KISEI-BANTI
               ELSE
                   MOVE    "2110"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           .
      *
       200211-KISBANTI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    帰省先−電話チェック処理
      *****************************************************************
       200211-KISTEL-CHK-SEC          SECTION.
      *
           IF      SPA-GMN21-KISEI-TEL NOT =   SPACE
               MOVE    SPA-GMN21-KISEI-TEL TO  WRK-MAE-INPUT
               MOVE    15                  TO  WRK-MAX
               PERFORM 891-HALF-CHK-SEC
               IF      FLG-ERR         NOT =   ZERO
                   MOVE    "2116"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
      *
           .
       200211-KISTEL-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    テスト患者番号区分チェック処理
      *****************************************************************
       200211-TSTPTNUMKBN-CHK-SEC     SECTION.
      *
      *    テスト患者
           IF      SPA-GMN-TSTPTNUMKBN =   SPACE
               MOVE    "0"             TO  SPA-GMN-TSTPTNUMKBN
           END-IF
           IF      SPA-GMN-TSTPTNUMKBN =   "0"  OR  "1"
               CONTINUE
           ELSE
               MOVE    "A032"              TO  SPA-ERRCD
               PERFORM 290-ERROR-HEN-SEC
           END-IF
           .
       200211-TSTPTNUMKBN-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    死亡区分チェック処理
      *****************************************************************
       200211-DEATHKBN-CHK-SEC     SECTION.
      *
           IF      SPA-GMN-DEATHKBN    =   "0"
               MOVE    SPACE           TO  SPA-GMN-DEATHKBN
           END-IF
           IF      SPA-GMN-DEATHKBN =   " "  OR  "1"
               CONTINUE
           ELSE
               MOVE    "A033"              TO  SPA-ERRCD
               PERFORM 290-ERROR-HEN-SEC
           END-IF
           .
       200211-DEATHKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    減免事由チェック処理
      *****************************************************************
       200211-DISCOUNTKBN-CHK-SEC     SECTION.
      *
           IF      SPA-GMN-DISCOUNTKBN =   SPACE
               MOVE    "00"            TO  SPA-GMN-DISCOUNTKBN
               MOVE    "00"            TO  PTMODRES-DISCOUNTKBN
           END-IF
           MOVE    ZERO                TO  FLG-CHK
           MOVE    SPACE               TO  WRK-DISCOUNTKBN-NAME
           PERFORM VARYING   IDX   FROM    1   BY  1
                   UNTIL   ( IDX       >   SPA-DISCOUNTKBN-MAX )
                   OR      ( SPA-GMN-DISCOUNTKBN-KBNCD (IDX)
                                             =   SPACE )
               IF      SPA-GMN-DISCOUNTKBN
                                   =   SPA-GMN-DISCOUNTKBN-KBNCD (IDX)
                   MOVE    1               TO  FLG-CHK
                   MOVE    SPA-GMN-DISCOUNTKBN-BODY (IDX)
                                           TO  WRK-DISCOUNTKBN-NAME
                   MOVE    SPA-DISCOUNTKBN-MAX TO  IDX
               END-IF
           END-PERFORM
           MOVE    WRK-DISCOUNTKBN-NAME    TO  PTMODRES-DISCOUNTKBN-NAME
           IF      FLG-CHK             =   ZERO
               MOVE    "0019"              TO  SPA-ERRCD
               PERFORM 290-ERROR-HEN-SEC
           END-IF
           .
       200211-DISCOUNTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    割引率チェック処理
      *****************************************************************
       200211-DISCOUNT-CHK-SEC     SECTION.
      *
           IF      SPA-GMN-DISCOUNT-CD =   SPACE
               MOVE    SPACE           TO  SPA-GMN-DISCOUNT
               MOVE    "00"            TO  SPA-GMN-DISCOUNT-CD
               MOVE    "00"            TO  PTMODRES-DISCOUNT
           END-IF
           MOVE    SPACE               TO  WRK-DISCOUNT-NAME
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX   FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-DISCOUNT-MAX)
                         OR   (SPA-GMN-DISCOUNT-KBNCD(IDX)
                                           =   SPACE  )
                         OR   (FLG-CHK     =   1      )
               IF      SPA-GMN-DISCOUNT-CD
                                           =   SPA-GMN-DISCOUNT-KBNCD
                                                              (IDX)
                   MOVE    SPA-GMN-DISCOUNT-EDIT(IDX)(3:)
                                               TO  WRK-DISCOUNT-NAME
                   MOVE    1                   TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0035"              TO  SPA-ERRCD
               PERFORM 290-ERROR-HEN-SEC
           ELSE
      *        前空白削除編集
               PERFORM VARYING    IDS      FROM    1   BY  1
                       UNTIL     (IDS      >   14  )
                   IF      WRK-DISCOUNT-NAME   (IDS:1)
                                           NOT =   SPACE
                       MOVE    WRK-DISCOUNT-NAME   (IDS:)
                                           TO  PTMODRES-DISCOUNT-NAME
                       MOVE    14          TO  IDS
                   END-IF
               END-PERFORM
           END-IF
           .
       200211-DISCOUNT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    状態チェック処理
      *****************************************************************
       200211-CONDITION-CHK-SEC     SECTION.
      *
           PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL    (IDX       >  3        )
               MOVE    ZERO                TO  FLG-CHK
               IF      SPA-GMN-CONDITION-CD (IDX)  =   SPACE
                   MOVE    "00"            TO  SPA-GMN-CONDITION-CD(IDX)
                   MOVE    "00"            TO  PTMODRES-CONDITION-KBN
                                                                (IDX)
               END-IF
               PERFORM VARYING   IDY       FROM    1   BY  1
                       UNTIL   ( IDY       >   SPA-CONDITION-MAX(IDX))
                       OR      ( SPA-GMN-CONDITION-KBNCD (IDX IDY)
                                           =   SPACE )
                       OR      (FLG-CHK    =   1     )
                   IF      SPA-GMN-CONDITION-CD (IDX)
                                           =   SPA-GMN-CONDITION-KBNCD
                                                            (IDX IDY)
                       MOVE    SPA-GMN-CONDITION-BODY (IDX IDY)
                                           TO  PTMODRES-CONDITION-NAME
                                                            (IDX)
                       MOVE    1           TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      FLG-CHK         =   ZERO
                   EVALUATE    IDX
                   WHEN    1
                       MOVE    "A034"              TO  SPA-ERRCD
                   WHEN    2
                       MOVE    "A035"              TO  SPA-ERRCD
                   WHEN    3
                       MOVE    "A036"              TO  SPA-ERRCD
                   END-EVALUATE
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-PERFORM
      *
           .
       200211-CONDITION-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報更新処理
      *****************************************************************
       20023-PTINFINS-SYORI-SEC     SECTION.
      *
      *    患者番号取得処理
           IF     (SPA-PTNUM           =   SPACE )
               PERFORM 200111-PTNUM-GET-SEC
           END-IF
      *    患者ＩＤ取得・PTNUM初期編集
           IF     WRK-ERRCD            =   SPACE
               PERFORM 200231-PTNUM-INIT-SEC
           END-IF
           IF     WRK-ERRCD        NOT =   SPACE
               GO     TO      20023-PTINFINS-SYORI-EXT
           END-IF
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   ZERO
               MOVE    "43"                TO  WRK-ERRCD
           END-IF
      *    患者情報編集処理
           IF     WRK-ERRCD            =   SPACE
      *        登録
               PERFORM 200232-PTINF-INSERT-SEC
      *        患者情報出力内容編集処理
               PERFORM 20021-PTMODRES-HEN-SEC
           END-IF
      *
           .
       20023-PTINFINS-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    同姓同名チェック処理
      *****************************************************************
       20022-JUFUKU-HEN-SEC        SECTION.
      *
           IF      FLG-PTINF-JFK       =   1
      *        同姓同名
               MOVE    1               TO  FLG-P02A-SYORI
           ELSE
      *        二重登録疑い
               MOVE    2               TO  FLG-P02A-SYORI
           END-IF
      *
           INITIALIZE                  SPA-P02A-AREA
           MOVE    FLG-P02A-SYORI      TO  SPA-P02A-SYORI
      *
           MOVE    SPACE               TO  SPA-MOTOPG
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP02A"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           MOVE    ZERO            TO  IDX-JF
      *    対象あり
           PERFORM     VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   SPA-GMN2A-MAX
               MOVE    SPA-GMN2A-PTNUM(IDX)
                                           TO  PTMODRES-JFK-PATIENTID
                                                             (IDX)
               INITIALIZE                      PTINF-REC
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    SPA-GMN2A-PTID (IDX)    TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               MOVE    PTINF-NAME          TO  PTMODRES-JFK-NAME
                                                             (IDX)
               MOVE    PTINF-KANANAME      TO  PTMODRES-JFK-KANANAME
                                                             (IDX)
               MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  PTMODRES-JFK-BIRTHDAY
                                                             (IDX)
               MOVE    PTINF-SEX           TO  PTMODRES-JFK-SEX
                                                             (IDX)
               MOVE    PTINF-HOME-ADRS     TO  PTMODRES-JFK-HOME-ADRS
                                                             (IDX)
               MOVE    PTINF-HOME-BANTI    TO  PTMODRES-JFK-HOME-BANTI
                                                             (IDX)
               MOVE    IDX                 TO  IDX-JF
           END-PERFORM
           .
       20022-JUFUKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者登録処理
      *****************************************************************
       200232-PTINF-INSERT-SEC        SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
      *    明細編集
           PERFORM 20023-PTINF-HENSYU-SEC
      *
           MOVE    SMCNDATE-YMD        TO  PTINF-CREYMD
           MOVE    SMCNDATE-YMD        TO  PTINF-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTINF-UPHMS
           MOVE    WRK-OPID            TO  PTINF-OPID
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "API PTINF INS ERR:" MCP-RC
                           ",KEY:" PTINF-KEY
               MOVE    "44"             TO  WRK-ERRCD
           END-IF
      *
           .
       200232-PTINF-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号変換（管理）マスター処理
      *****************************************************************
       200231-PTNUM-INIT-SEC        SECTION.
      *
      *    患者番号存在チェック
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    SPA-PTNUM           TO  PTNUM-PTID
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-PTNUM-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTNUM
           END-IF
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-PTNUM           =   ZERO
               MOVE    "43"                TO  WRK-ERRCD
               GO      TO      200231-PTNUM-INIT-EXT
           END-IF
      *
      *    患者番号採番検索
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1009-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1009-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1009-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSKANRI        =   1
               MOVE    "40"            TO  WRK-ERRCD
               DISPLAY "API SYS1009(PTGETNUM)ERR:" MCP-RC  "."
               GO      TO      200231-PTNUM-INIT-EXT
           END-IF
           IF      SYS-1009-PTID       =   ALL "9"
      *???     MOVE    "47"                TO  WRK-ERRCD
               MOVE    ZERO                TO  SYS-1009-PTID
           END-IF
           COMPUTE SYS-1009-PTID       =   SYS-1009-PTID  +   1
           MOVE    SYS-1009-PTID       TO  SPA-GMN-PTID
           MOVE    SYS-1009-PTID       TO  SPA-PTID
      *
           MOVE    SMCNDATE-YMD        TO  SYS-1009-UPYMD
           MOVE    SMCNDATE-HMS        TO  SYS-1009-UPHMS
           MOVE    WRK-OPID            TO  SYS-1009-OPID
      *    患者番号採番  更新
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC         NOT  =   ZERO
               MOVE    "41"                TO  WRK-ERRCD
               DISPLAY "ARI  SYSKANRI UPDATE:" MCP-RC "."
               GO      TO      200231-PTNUM-INIT-EXT
           END-IF
      *
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    SPA-PTID            TO  PTNUM-PTID
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-PTNUM-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTNUM
           END-IF
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTNUM           =   1
               MOVE    SPACE               TO  PTNUM-REC
               INITIALIZE                      PTNUM-REC
               MOVE    SPA-HOSPNUM             TO  PTNUM-HOSPNUM
               MOVE    SPA-PTID                TO  PTNUM-PTID
               MOVE    SPA-PTNUM               TO  PTNUM-PTNUM
               MOVE    SPA-PTNUM-HKNID         TO  PTNUM-HKNID
               MOVE    SPA-PTNUM-KOHID         TO  PTNUM-KOHID
               MOVE    SPA-PTNUM-AUTOCOMBINUM  TO  PTNUM-AUTOCOMBINUM
               MOVE    SPA-PTNUM-MANUCOMBINUM  TO  PTNUM-MANUCOMBINUM
      *
               MOVE    SMCNDATE-YMD        TO  PTNUM-CREYMD
               MOVE    SMCNDATE-YMD        TO  PTNUM-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTNUM-UPHMS
      *
               MOVE    WRK-OPID            TO  PTNUM-OPID
      *
               MOVE    PTNUM-REC           TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P029 PTNUM INS ERR:" MCP-RC
                          ",KEY:" PTNUM-KEY   "."
                   MOVE    "42"            TO  WRK-ERRCD
               END-IF
           ELSE
               MOVE    "43"                TO  WRK-ERRCD
           END-IF
           .
       200231-PTNUM-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者更新処理
      *****************************************************************
       2003-PTINF-UPDATE-SEC        SECTION.
      *
      *
           EVALUATE    PTMODREQ-REQUEST-NUMBER
           WHEN    "01"
               MOVE    "02"            TO  PTMODRES-RESPONSE-NUMBER
      *        患者情報返却
               PERFORM 20031-PTINF-UPDINIT-SEC
           WHEN    "02"
      *        患者情報更新
               MOVE    "02"            TO  PTMODRES-RESPONSE-NUMBER
               PERFORM 20032-PTINF-UPDSYORI-SEC
           WHEN    OTHER
               MOVE    "91"            TO  WRK-ERRCD
               MOVE    "01"            TO  PTMODRES-RESPONSE-NUMBER
           END-EVALUATE
      *
           .
       2003-PTINF-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報更新処理
      *****************************************************************
       20031-PTINF-UPDINIT-SEC         SECTION.
      *
      *    患者情報検索処理
           PERFORM 20030-PTNUM-INIT-SEC
      *
      *    パラメタ登録
           IF      WRK-ERRCD           =   SPACE
      *        パラメタ登録
               MOVE    SPACE           TO  WRK-ENDKBN
               PERFORM 2901-PARA-INS-SEC
           ELSE
      *        エラー
               MOVE    "01"                TO  PTMODRES-RESPONSE-NUMBER
               MOVE    PTMODREQ-ORCA-UID   TO  PTMODRES-ORCA-UID
           END-IF
      *
           .
       20031-PTINF-UPDINIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報検索初期処理
      *****************************************************************
       20030-PTNUM-INIT-SEC         SECTION.
      *
      *    オルカＵＩＤ処理
           PERFORM 20020-ORCAUID-SYORI-SEC
      *
      *    患者情報検索
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   ZERO
      *        排他チェック & ロック処理
               PERFORM 990-LOCK-IN-SEC
               IF      SLOCK-RETURN    NOT =   ZERO
                   MOVE    "90"            TO  WRK-ERRCD
               END-IF
           ELSE
      *        患者なし
               MOVE    "10"                TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *         ＳＰＡセット
                PERFORM 200210-ORCSP02-INT-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        患者情報出力内容編集処理
               PERFORM 20021-PTMODRES-HEN-SEC
           END-IF
      *
           .
       20030-PTNUM-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報更新処理
      *****************************************************************
       20032-PTINF-UPDSYORI-SEC        SECTION.
      *
      *    患者番号判定
           PERFORM 20039-PTINF-INITCHK-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      20032-PTINF-UPDSYORI-EXT
           END-IF
      *
           MOVE    SPA-PTID            TO  SPA-GMN-PTID
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
      *    基本情報
           PERFORM 200211-PTINF-CHK-SEC
      *
      *    旧姓履歴チェック
           IF     (WRK-ERRCD           =   SPACE    )
               PERFORM 20031-KYUSEI-HEN-SEC
           END-IF
      *
      *    同姓同名リスト編集判定
           IF     (PTINF-NAME      =   SPA-GMN-NAME    )  AND
                  (PTINF-KANANAME  =   SPA-GMN-KANANAME)  AND
                  (PTINF-SEX       =   SPA-GMN-SEX     )  AND
                  (PTINF-BIRTHDAY  =   SPA-NAI-BIRTHDAY)
      *        氏名から生年月日に変更あり
               MOVE    1               TO  FLG-CHK02
           ELSE
               MOVE    ZERO            TO  FLG-CHK02
           END-IF
      *
      *    更新処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20023-PTINFUPD-SYORI-SEC
      *        患者情報出力内容編集処理
               PERFORM 20021-PTMODRES-HEN-SEC
           END-IF
      *
      *    同姓同名チェック
           IF    (WRK-ERRCD            =   SPACE )
      *R02.2 AND (FLG-CHK02            =   ZERO  )
               MOVE    1                   TO  FLG-PTINF-JFK
               PERFORM 20022-JUFUKU-HEN-SEC
           END-IF
      *
      *
           IF      WRK-ERRCD           =   SPACE
      *        継続
               IF      FLG-CONTKBN         =   1
      *            保険登録へ
                   MOVE    "01"            TO  PTMODRES-RESPONSE-NUMBER
      *            継続区分終了
                   MOVE    "1"             TO  WRK-ENDKBN
      *            パラメタ登録
                   PERFORM 2901-PARA-INS-SEC
               ELSE
      *            終了処理
                   PERFORM 2909-END-SYORI-SEC
                   MOVE    "00"            TO  PTMODRES-RESPONSE-NUMBER
               END-IF
           ELSE
               MOVE    SPACE           TO  WRK-ENDKBN
      *        パラメタ登録
               PERFORM 2901-PARA-INS-SEC
           END-IF
           .
       20032-PTINF-UPDSYORI-EXT.
           EXIT.
      *
      ****************************************************************
      *    患者番号判定共通処理
      ****************************************************************
       20039-PTINF-INITCHK-SEC      SECTION.
      *
      *    患者番号判定
           IF     (SPA-PTNUM       NOT =   WRK-PARA-PTNUM )
               MOVE    "22"                TO  WRK-ERRCD
               MOVE    "01"                TO  PTMODRES-RESPONSE-NUMBER
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        患者情報検索
               INITIALIZE                      PTINF-REC
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    SPA-PTID            TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF       NOT =   ZERO
                   MOVE    "10"            TO  WRK-ERRCD
                   MOVE    "01"            TO  PTMODRES-RESPONSE-NUMBER
               END-IF
           END-IF
      *    排他チェック & ロック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 990-LOCK-IN-SEC
               IF      SLOCK-RETURN    NOT =   ZERO
                   MOVE    "90"            TO  WRK-ERRCD
                   MOVE    "01"            TO  PTMODRES-RESPONSE-NUMBER
               END-IF
           END-IF
           .
       20039-PTINF-INITCHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    旧姓履歴の更新処理
      ****************************************************************
       20031-KYUSEI-HEN-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-KYUSEI
           IF     (SPA-GMN-NAME        =   PTINF-NAME    )
             AND  (SPA-GMN-KANANAME    =   PTINF-KANANAME)
               CONTINUE
           ELSE
               MOVE    1                   TO  FLG-KYUSEI
           END-IF
           .
       20031-KYUSEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者情報更新処理
      *****************************************************************
       20023-PTINFUPD-SYORI-SEC     SECTION.
      *
      *    旧姓履歴登録
           IF     (FLG-KYUSEI          =   1 )
             AND  (PTMODREQ-KYUSEI-MODE    =   "True" )
               PERFORM 200232-KYUSEI-INS-SEC
           END-IF
      *    患者情報編集処理
           PERFORM 20023-PTINF-HENSYU-SEC
      *
      *    更新
           MOVE    SMCNDATE-YMD        TO  PTINF-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTINF-UPHMS
           MOVE    WRK-OPID            TO  PTINF-OPID
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "API PTINF UPD ERR:" MCP-RC
                           ",KEY:" PTINF-KEY
               MOVE    "51"             TO  WRK-ERRCD
           END-IF
      *
           .
       20023-PTINFUPD-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    患者情報出力編集処理
      *****************************************************************
       20023-PTINF-HENSYU-SEC     SECTION.
      *
           MOVE    SPA-GMN-NAME            TO  PTINF-NAME
           MOVE    SPA-GMN-KANANAME        TO  PTINF-KANANAME
           MOVE    SPA-GMN-SEX             TO  PTINF-SEX
           MOVE    SPA-NAI-BIRTHDAY        TO  PTINF-BIRTHDAY
           MOVE    SPA-GMN-POST            TO  PTINF-HOME-POST
           MOVE    SPA-GMN-EDITADRS-NAME   TO  PTINF-HOME-ADRS
           MOVE    SPA-GMN-BANTI           TO  PTINF-HOME-BANTI
           MOVE    SPA-GMN-TEL1            TO  PTINF-HOME-TEL1
           MOVE    SPA-GMN-TEL2            TO  PTINF-HOME-TEL2
           MOVE    SPA-GMN-SETAINUSI       TO  PTINF-SETAINUSI
           MOVE    SPA-GMN-ZOKUGARA        TO  PTINF-ZOKUGARA
           MOVE    SPA-GMN-NICKNAME        TO  PTINF-NICKNAME
           MOVE    SPA-GMN-TABOO-1         TO  PTINF-TABOO-1
           MOVE    SPA-GMN-TABOO-2         TO  PTINF-TABOO-2
           MOVE    SPA-GMN-ALLERGY-1       TO  PTINF-ALLERGY-1
           MOVE    SPA-GMN-ALLERGY-2       TO  PTINF-ALLERGY-2
           MOVE    SPA-GMN-KANSENSYO-1     TO  PTINF-KANSENSYO-1
           MOVE    SPA-GMN-KANSENSYO-2     TO  PTINF-KANSENSYO-2
           MOVE    SPA-GMN-COMMENT-1       TO  PTINF-COMMENT-1
           MOVE    SPA-GMN-COMMENT-2       TO  PTINF-COMMENT-2
      *
           MOVE    SPA-GMN21-JOB           TO  PTINF-JOB
           MOVE    SPA-GMN21-KEITAI-TEL    TO  PTINF-KEITAI-TEL
           MOVE    SPA-GMN21-FAX           TO  PTINF-FAX
           MOVE    SPA-GMN21-EMAIL         TO  PTINF-EMAIL
      *    勤務先
           MOVE    SPA-GMN21-OFFICE-NAME   TO  PTINF-OFFICE-NAME
           MOVE    SPA-GMN21-OFFICE-POST   TO  PTINF-OFFICE-POST
           MOVE    SPA-GMN21-OFFICE-ADRS   TO  PTINF-OFFICE-ADRS
           MOVE    SPA-GMN21-OFFICE-BANTI  TO  PTINF-OFFICE-BANTI
           MOVE    SPA-GMN21-OFFICE-TEL    TO  PTINF-OFFICE-TEL
      *    連絡先
           MOVE    SPA-GMN21-RENRAKU-NAME  TO  PTINF-RENRAKU-NAME
           MOVE    SPA-GMN21-RENRAKU-ZOKUGARA
                                           TO  PTINF-RENRAKU-ZOKUGARA
           MOVE    SPA-GMN21-RENRAKU-POST  TO  PTINF-RENRAKU-POST
           MOVE    SPA-GMN21-RENRAKU-ADRS  TO  PTINF-RENRAKU-ADRS
           MOVE    SPA-GMN21-RENRAKU-BANTI TO  PTINF-RENRAKU-BANTI
           MOVE    SPA-GMN21-RENRAKU-TEL1  TO  PTINF-RENRAKU-TEL1
           MOVE    SPA-GMN21-RENRAKU-TEL2  TO  PTINF-RENRAKU-TEL2
      *    帰省先
           MOVE    SPA-GMN21-KISEI-NAME    TO  PTINF-KISEI-NAME
           MOVE    SPA-GMN21-KISEI-POST    TO  PTINF-KISEI-POST
           MOVE    SPA-GMN21-KISEI-ADRS    TO  PTINF-KISEI-ADRS
           MOVE    SPA-GMN21-KISEI-BANTI   TO  PTINF-KISEI-BANTI
           MOVE    SPA-GMN21-KISEI-TEL     TO  PTINF-KISEI-TEL
      *    テスト患者番号区分
           MOVE    SPA-GMN-TSTPTNUMKBN     TO  PTINF-TSTPTNUMKBN
      *    死亡区分
           MOVE    SPA-GMN-DEATHKBN        TO  PTINF-DEATHKBN
      *    減免事由
           MOVE    SPA-GMN-DISCOUNTKBN     TO  PTINF-DISCOUNT-KBN
      *    割引率
           MOVE    SPA-GMN-DISCOUNT-CD     TO  PTINF-DISCOUNT
      *    状態１から３
           MOVE    SPA-GMN-CONDITION-CD(1) TO  PTINF-CONDITION-1
           MOVE    SPA-GMN-CONDITION-CD(2) TO  PTINF-CONDITION-2
           MOVE    SPA-GMN-CONDITION-CD(3) TO  PTINF-CONDITION-3
      *
           .
       20023-PTINF-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    旧姓履歴登録処理
      *****************************************************************
       200232-KYUSEI-INS-SEC     SECTION.
      *
      *    当日の旧姓履歴登録済みは登録なし
           INITIALIZE                      KYUSEIRRK-REC
           MOVE    SPA-HOSPNUM         TO  KYUSEI-HOSPNUM
           MOVE    SPA-PTID            TO  KYUSEI-PTID
           MOVE    SPA-SYSYMD          TO  KYUSEI-CHGYMD
           PERFORM 903-KYUSEIRRK-READ-SEC
           IF      FLG-KYUSEIRRK       =   ZERO
               GO      TO      200232-KYUSEI-INS-EXT
           END-IF
      *
           INITIALIZE                      KYUSEIRRK-REC
           MOVE    SPA-HOSPNUM         TO  KYUSEI-HOSPNUM
           MOVE    SPA-PTID            TO  KYUSEI-PTID
           MOVE    SPA-SYSYMD          TO  KYUSEI-CHGYMD
           MOVE    PTINF-NAME          TO  KYUSEI-NAME
           MOVE    PTINF-KANANAME      TO  KYUSEI-KANANAME
           MOVE    PTINF-NICKNAME      TO  KYUSEI-NICKNAME
      *
           MOVE    SMCNDATE-YMD        TO  KYUSEI-CREYMD
           MOVE    SMCNDATE-YMD        TO  KYUSEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  KYUSEI-UPHMS
      *
           MOVE    WRK-OPID            TO  KYUSEI-OPID
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    KYUSEIRRK-REC       TO  MCPDATA-REC
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "API KYUSEIRRK INS ERR:" MCP-RC
                             ",KEY:" KYUSEI-KEY
               MOVE    "52"                TO  WRK-ERRCD
           END-IF
      *
           IF     WRK-ERRCD            =   SPACE
               ADD     1                   TO  IDX-W
               MOVE    "K3"                TO  WRK-KEICD(IDX-W)
           END-IF
           .
       200232-KYUSEI-INS-EXT.
           EXIT.
      *****************************************************************
      *    患者情報出力内容編集処理
      *****************************************************************
       20021-PTMODRES-HEN-SEC     SECTION.
      *
      *    患者番号
           MOVE    SPA-PTNUM           TO  PTMODRES-PATIENTID
      *    患者情報
           MOVE    PTINF-NAME          TO  PTMODRES-NAME
           MOVE    PTINF-KANANAME      TO  PTMODRES-KANANAME
           MOVE    PTINF-SEX           TO  PTMODRES-SEX
      *
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PTMODRES-BIRTHDAY
      *
           MOVE    PTINF-SETAINUSI     TO  PTMODRES-SETAI-NAME
           MOVE    PTINF-ZOKUGARA      TO  PTMODRES-ZOKUGARA
           MOVE    PTINF-JOB           TO  PTMODRES-JOB
      *
           MOVE    PTINF-NICKNAME      TO  PTMODRES-NICKNAME
           MOVE    PTINF-KEITAI-TEL    TO  PTMODRES-KEITAI-TEL
           MOVE    PTINF-FAX           TO  PTMODRES-FAX
           MOVE    PTINF-EMAIL         TO  PTMODRES-EMAIL
      *    自宅情報
           MOVE    PTINF-HOME-POST     TO  PTMODRES-HOME-ZIP-CODE
           MOVE    PTINF-HOME-ADRS     TO  PTMODRES-HOME-ADDRESSES1
           MOVE    PTINF-HOME-BANTI    TO  PTMODRES-HOME-ADDRESSES2
           MOVE    PTINF-HOME-TEL1     TO  PTMODRES-HOME-HPONE1
           MOVE    PTINF-HOME-TEL2     TO  PTMODRES-HOME-HPONE2
      *    勤務先情報
           MOVE    PTINF-OFFICE-NAME   TO  PTMODRES-OFFICE-NAME
           MOVE    PTINF-OFFICE-POST   TO  PTMODRES-OFFICE-ZIP-CODE
           MOVE    PTINF-OFFICE-ADRS   TO  PTMODRES-OFFICE-ADDRESSES1
           MOVE    PTINF-OFFICE-BANTI  TO  PTMODRES-OFFICE-ADDRESSES2
           MOVE    PTINF-OFFICE-TEL    TO  PTMODRES-OFFICE-HPONE
      *    連絡先情報
           MOVE    PTINF-RENRAKU-NAME  TO  PTMODRES-RENRAKU-NAME
           MOVE    PTINF-RENRAKU-ZOKUGARA
                                       TO  PTMODRES-RENRAKU-ZOKUGARA
           MOVE    PTINF-RENRAKU-POST  TO  PTMODRES-RENRAKU-ZIP-CODE
           MOVE    PTINF-RENRAKU-ADRS  TO  PTMODRES-RENRAKU-ADDRESSES1
           MOVE    PTINF-RENRAKU-BANTI TO  PTMODRES-RENRAKU-ADDRESSES2
           MOVE    PTINF-RENRAKU-TEL1  TO  PTMODRES-RENRAKU-HPONE1
           MOVE    PTINF-RENRAKU-TEL2  TO  PTMODRES-RENRAKU-HPONE2
      *    帰省先
           MOVE    PTINF-KISEI-NAME    TO  PTMODRES-KISEI-NAME
           MOVE    PTINF-KISEI-POST    TO  PTMODRES-KISEI-ZIP-CODE
           MOVE    PTINF-KISEI-ADRS    TO  PTMODRES-KISEI-ADDRESSES1
           MOVE    PTINF-KISEI-BANTI   TO  PTMODRES-KISEI-ADDRESSES2
           MOVE    PTINF-KISEI-TEL     TO  PTMODRES-KISEI-HPONE
      *    コメント
           MOVE    PTINF-COMMENT-1     TO  PTMODRES-COMMENT-1
           MOVE    PTINF-COMMENT-2     TO  PTMODRES-COMMENT-2
           MOVE    PTINF-TABOO-1       TO  PTMODRES-TABOO-1
           MOVE    PTINF-TABOO-2       TO  PTMODRES-TABOO-2
           MOVE    PTINF-ALLERGY-1     TO  PTMODRES-ALLERGY-1
           MOVE    PTINF-ALLERGY-2     TO  PTMODRES-ALLERGY-2
           MOVE    PTINF-KANSENSYO-1   TO  PTMODRES-KANSENSYO-1
           MOVE    PTINF-KANSENSYO-2   TO  PTMODRES-KANSENSYO-2
      *    テスト患者番号区分
           MOVE    PTINF-TSTPTNUMKBN   TO  PTMODRES-TEST-PATIENT
      *    死亡区分
           MOVE    PTINF-DEATHKBN      TO  PTMODRES-DEATHKBN
      *    減免事由
           MOVE    PTINF-DISCOUNT-KBN  TO  PTMODRES-DISCOUNTKBN
           PERFORM VARYING   IDX   FROM    1   BY  1
                   UNTIL   ( IDX       >   SPA-DISCOUNTKBN-MAX )
               IF      PTINF-DISCOUNT-KBN
                                   =   SPA-GMN-DISCOUNTKBN-KBNCD (IDX)
                   MOVE    SPA-GMN-DISCOUNTKBN-BODY (IDX)
                                           TO  PTMODRES-DISCOUNTKBN-NAME
                   MOVE    SPA-DISCOUNTKBN-MAX TO  IDX
               END-IF
           END-PERFORM
      *    割引率
           MOVE    PTINF-DISCOUNT      TO  PTMODRES-DISCOUNT
           MOVE    SPACE               TO  WRK-DISCOUNT-NAME
           PERFORM VARYING     IDX   FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-DISCOUNT-MAX)
               IF      PTINF-DISCOUNT      =   SPA-GMN-DISCOUNT-KBNCD
                                                              (IDX)
                   MOVE    SPA-GMN-DISCOUNT-EDIT(IDX)(3:)
                                               TO  WRK-DISCOUNT-NAME
                   MOVE    SPA-DISCOUNT-MAX    TO  IDX
               END-IF
           END-PERFORM
      *    前空白削除編集
           PERFORM VARYING    IDS      FROM    1   BY  1
                   UNTIL     (IDS      >   14  )
               IF      WRK-DISCOUNT-NAME   (IDS:1)
                                           NOT =   SPACE
                   MOVE    WRK-DISCOUNT-NAME   (IDS:)
                                           TO  PTMODRES-DISCOUNT-NAME
                   MOVE    14              TO  IDS
               END-IF
           END-PERFORM
      *    状態１から３
           MOVE    PTINF-CONDITION-1   TO  PTMODRES-CONDITION-KBN (1)
           MOVE    PTINF-CONDITION-2   TO  PTMODRES-CONDITION-KBN (2)
           MOVE    PTINF-CONDITION-3   TO  PTMODRES-CONDITION-KBN (3)
      *
           PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL    (IDX       >  3        )
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING   IDY       FROM    1   BY  1
                       UNTIL   ( IDY       >   SPA-CONDITION-MAX(IDX))
                   IF      PTMODRES-CONDITION-KBN (IDX)
                                           =   SPA-GMN-CONDITION-KBNCD
                                                            (IDX IDY)
                       MOVE    SPA-GMN-CONDITION-BODY (IDX IDY)
                                           TO  PTMODRES-CONDITION-NAME
                                                            (IDX)
                       MOVE    SPA-CONDITION-MAX(IDX)
                                           TO  IDY
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           .
       20021-PTMODRES-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       2004-PTINF-DELETE-SEC     SECTION.
      *
           EVALUATE    PTMODREQ-REQUEST-NUMBER
           WHEN    "01"
               MOVE    "02"            TO  PTMODRES-RESPONSE-NUMBER
      *        患者情報返却
               PERFORM 20041-PTINF-DELINIT-SEC
           WHEN    "02"
      *        患者情報更新
               MOVE    "02"            TO  PTMODRES-RESPONSE-NUMBER
               PERFORM 20042-PTINF-DELSYORI-SEC
           WHEN    OTHER
               MOVE    "91"            TO  WRK-ERRCD
               MOVE    "01"            TO  PTMODRES-RESPONSE-NUMBER
           END-EVALUATE
      *
           .
       2004-PTINF-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       20041-PTINF-DELINIT-SEC     SECTION.
      *
      *    患者情報検索処理
           PERFORM 20030-PTNUM-INIT-SEC
      *
      *    IF      WRK-ERRCD           =   SPACE
      *        MOVE    "0103"              TO  PTMODRES-SELECT-CODE
      *        PERFORM 8902-SELECTMSG-SEC
      *        MOVE    "S"                 TO  WRK-ERRKBN
      *    END-IF
      *    パラメタ登録
           IF      WRK-ERRCD           =   SPACE
               MOVE    SPACE           TO  WRK-ENDKBN
               PERFORM 2901-PARA-INS-SEC
           ELSE
      *        エラー
               MOVE    "01"                TO  PTMODRES-RESPONSE-NUMBER
               MOVE    PTMODREQ-ORCA-UID   TO  PTMODRES-ORCA-UID
           END-IF
      *
           .
       20041-PTINF-DELINIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報更新処理
      *****************************************************************
       20042-PTINF-DELSYORI-SEC        SECTION.
      *
      *    患者番号判定
           PERFORM 20039-PTINF-INITCHK-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      20042-PTINF-DELSYORI-EXT
           END-IF
      *
      *    性別・生年月日が一致すること
           IF     (PTINF-SEX           =   SPA-GMN-SEX )  AND
                  (PTINF-BIRTHDAY      =   SPA-NAI-BIRTHDAY ) AND
                  (PTINF-NAME          =   SPA-GMN-NAME )
               CONTINUE
           ELSE
               MOVE    "70"                TO  WRK-ERRCD
               MOVE    "01"                TO  PTMODRES-RESPONSE-NUMBER
               MOVE    SPACE               TO  WRK-SELECT-CODE
           END-IF
      *
      *    ＳＰＡ患者情報のセット
           PERFORM 200210-ORCSP02-INT-SEC
      *    患者情報出力内容編集処理
           PERFORM 20021-PTMODRES-HEN-SEC
           IF      WRK-ERRCD           =   SPACE
               IF      WRK-SELECT-CODE     =   SPACE
                   MOVE    "0103"              TO  PTMODRES-SELECT-CODE
                   PERFORM 8902-SELECTMSG-SEC
                   MOVE    "20"                TO  WRK-ERRCD
                   MOVE    "S"                 TO  WRK-ERRKBN
               END-IF
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
               MOVE    ZERO                TO  FLG-DELETE
               EVALUATE    PTMODREQ-SELECT-ANSWER
               WHEN    "OK"
               WHEN    "Ok"
                   EVALUATE    WRK-SELECT-CODE
                   WHEN    "0103"
                       IF      WRK-ERRCD       =   SPACE
      *                    患者情報チェック
                           PERFORM 20031-DELETE-CHKSYORI-SEC
                       END-IF
                   WHEN    "0113"
                       IF      WRK-ERRCD       =   SPACE
                           MOVE    1               TO  FLG-DELETE
                       END-IF
                   WHEN    OTHER
                       MOVE    "71"            TO  WRK-ERRCD
                   END-EVALUATE
               WHEN    "NO"
               WHEN    "No"
      *            削除処理終了
                   MOVE    "00"            TO  PTMODRES-RESPONSE-NUMBER
                   MOVE    "73"            TO  WRK-ERRCD
               WHEN    OTHER
                   MOVE    "01"            TO  PTMODRES-RESPONSE-NUMBER
                   MOVE    "21"            TO  WRK-ERRCD
               END-EVALUATE
      *
           END-IF
      *
           IF     (WRK-ERRCD               =   SPACE )
             AND  (FLG-DELETE              =   1     )
      *R01.10
               MOVE    SPA-OPID            TO  WRK-SPA-OPID
               MOVE    WRK-OPID            TO  SPA-OPID
      *        患者番号削除処理
               CALL    "ORCSP05DL"         USING
                                           SPA-AREA
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    "75"            TO  WRK-ERRCD
               END-IF
               MOVE    WRK-SPA-OPID        TO  SPA-OPID
           END-IF
      *
           IF    ((WRK-ERRCD               =   SPACE )
             AND  (FLG-DELETE              =   1     ))
             OR  ( WRK-ERRCD               =   "73"   )
      *        終了処理
               PERFORM 2909-END-SYORI-SEC
               MOVE    "00"            TO  PTMODRES-RESPONSE-NUMBER
      *        削除処理キャンセル
               IF      WRK-ERRCD           =   "73"
                   MOVE    "0"                 TO  WRK-ERRKBN
               END-IF
               MOVE    SPACE                   TO  PTMODRES-CONT-MODE
           ELSE
               IF     (WRK-SELECT-CODE NOT =   SPACE )
                 AND  (PTMODRES-SELECT-CODE    =   SPACE )
                   MOVE    WRK-SELECT-CODE     TO  PTMODRES-SELECT-CODE
               END-IF
               MOVE    SPACE           TO  WRK-ENDKBN
               PERFORM 2901-PARA-INS-SEC
           END-IF
           .
       20042-PTINF-DELSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除チェック処理
      *****************************************************************
       20031-DELETE-CHKSYORI-SEC               SECTION.
      *
      *    患者情報チェック
           PERFORM 20031-DELETE-CHK-SEC
           IF      WRK-ERRCD           =   SPACE
               MOVE    1               TO  FLG-DELETE
           ELSE
      *        削除確認へ
               MOVE    "0113"              TO  PTMODRES-SELECT-CODE
               PERFORM 8902-SELECTMSG-SEC
               MOVE    "20"                TO  WRK-ERRCD
               MOVE    "S"                 TO  WRK-ERRKBN
           END-IF
           .
       20031-DELETE-CHKSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除チェック処理
      *****************************************************************
       20031-DELETE-CHK-SEC               SECTION.
      *
      *    受診履歴の登録がないこと
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key8"          TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-JYURRK
               ELSE
                   MOVE    1               TO  FLG-JYURRK
               END-IF
           ELSE
               INITIALIZE              JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-JYURRK          =   ZERO
               MOVE    "82"                TO  WRK-ERRCD
               GO      TO      20031-DELETE-CHK-EXT
           END-IF
      *    病名の登録がないこと
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-PTID            TO  PTBYO-PTID
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-PTBYOMEI
               ELSE
                   MOVE    1               TO  FLG-PTBYOMEI
               END-IF
           ELSE
               INITIALIZE              PTBYOMEI-REC
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-PTBYOMEI        =   ZERO
               MOVE    "82"                TO  WRK-ERRCD
               GO      TO      20031-DELETE-CHK-EXT
           END-IF
      *    入院の登録がないこと
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-PTNYUINRRK
               ELSE
                   MOVE    1               TO  FLG-PTNYUINRRK
               END-IF
           ELSE
               INITIALIZE              PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-PTNYUINRRK         =   ZERO
               MOVE    "82"                TO  WRK-ERRCD
           END-IF
           .
       20031-DELETE-CHK-EXT.
           EXIT.
      *
      *R01.7
      *****************************************************************
      *    検索処理
      *****************************************************************
       20010-PTINF-SELECT-SEC          SECTION.
      *
           MOVE    ZERO                    TO  FLG-SYORI
      *    患者番号
           MOVE    PTMODREQ-PATIENTID      TO  PTMODRES-PATIENTID
           MOVE    PTMODREQ-PATIENTID      TO  WRK-PTNUM
      *    患者番号
           IF      WRK-PTNUM           =   SPACE
               MOVE    "01"                TO  WRK-ERRCD
           END-IF
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PTNUM-CHK-SEC
           END-IF
      *    患者情報検索
           IF      WRK-ERRCD           =   SPACE
               INITIALIZE                      PTINF-REC
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    SPA-PTID            TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF       NOT =   ZERO
      *            患者なし
                   MOVE    "10"                TO  WRK-ERRCD
               END-IF
           END-IF
      *    ＳＰＡ患者情報のセット
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200210-ORCSP02-INT-SEC
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        患者情報出力内容編集処理
               PERFORM 20021-PTMODRES-HEN-SEC
           END-IF
           .
       20010-PTINF-SELECT-EXT.
           EXIT.
      *!!
      *
      *****************************************************************
      *    正常終了処理
      *****************************************************************
       2909-END-SYORI-SEC   SECTION.
      *
           INITIALIZE                  ORAPIV3SUB01AREA
           MOVE    "1"                 TO  APIV3SUB01-SYORI
           MOVE    SPA-TERMID          TO  APIV3SUB01-TERMID
           MOVE    PTMODREQ-KARTE-UID  TO  APIV3SUB01-KARTE-UID
           MOVE    "AP12"              TO  APIV3SUB01-API-GYOUMUID
           MOVE    "API12V3"           TO  APIV3SUB01-API-FILEMEI
           MOVE    WRK-PTNUM           TO  APIV3SUB01-PTNUM
           MOVE    "P02"               TO  APIV3SUB01-PARA-GYOUMUID
           CALL    "ORAPIV3SUB01"      USING
                                       MCPAREA
                                       SPA-AREA
                                       ORAPIV3SUB01AREA
           IF      APIV3SUB01-ERRCD    NOT =   SPACE
               MOVE    "60"                TO  WRK-ERRCD
               MOVE    APIV3SUB01-ERRMSG   TO  WRK-ERRMSG
           END-IF
           .
       2909-END-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
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
      *    半角空白を全角空白へ変換処理
      *****************************************************************
       890-HALF-ZEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
           MOVE    ZERO                TO  FLG-ERR2
           MOVE    ZERO                TO  FLG-ERR3
      *    拡張漢字有無
           INITIALIZE                  ORCSSTRINGAREA
           MOVE    "check"             TO  ORCSSTR-COMMAND
           MOVE    WRK-MAE-INPUT       TO  ORCSSTR-STRING1
           CALL    "orcsstring"        USING
                                       ORCSSTRINGAREA
           IF      ORCSSTR-NUM2    NOT =   ZERO
               MOVE    1                   TO  FLG-ERR2
      **       MOVE    1                   TO  FLG-ERR
           END-IF
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-MAE-INPUT
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
      *2019.07  改行コード
              OR  (KANACHK-RC4     NOT =   ZERO  )
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
      *        改行コードのみエラー
               IF      KANACHK-RC4     NOT =   ZERO
                   MOVE    1               TO  FLG-ERR3
               END-IF
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT
           END-IF
      *
           .
       890-HALF-ZEN-EXT.
           EXIT.
      *****************************************************************
      *    半角空白を全角空白へ変換処理（外字あり）
      *****************************************************************
       891-HALF-ZEN2-SEC         SECTION.
      *
           IF     SPA-ENCODING         =   2
      *        拡張漢字対応
               PERFORM 8910-HALF-ZENC2-SEC
           ELSE
      *        拡張漢字不可
               PERFORM 890-HALF-ZEN-SEC
           END-IF
      *
           .
       891-HALF-ZEN2-EXT.
           EXIT.
      *****************************************************************
      *    半角空白を全角空白へ変換処理（外字あり）
      *****************************************************************
       8910-HALF-ZENC2-SEC         SECTION.
      *
           INITIALIZE                  ORCSKANACHK2AREA
           MOVE    "1"                 TO  KANACHK2-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK2-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK2-MAX-CNT
           CALL    "ORCSKANACHK2"      USING
                                       ORCSKANACHK2AREA
           MOVE    KANACHK2-MAE-INPUT  TO  WRK-MAE-INPUT
           IF     (KANACHK2-RC     NOT =   ZERO  )  OR
                  (KANACHK2-SYUBETU    =   1     )
      *2019.07  改行コード
              OR  (KANACHK2-RC5    NOT =   ZERO  )
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK2-MAE-INPUT  TO  WRK-ATO-INPUT
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK2-OUT-INPUT  TO  WRK-ATO-INPUT
           END-IF
      *
           .
       8910-HALF-ZENC2-EXT.
           EXIT.
      *****************************************************************
      *    半角を全角へ変換処理
      *****************************************************************
       880-HALF-ZENCNG-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
           MOVE    ZERO                TO  FLG-ERR2
      *    拡張漢字有無
           INITIALIZE                  ORCSSTRINGAREA
           MOVE    "check"             TO  ORCSSTR-COMMAND
           MOVE    WRK-MAE-INPUT       TO  ORCSSTR-STRING1
           CALL    "orcsstring"        USING
                                       ORCSSTRINGAREA
           IF      ORCSSTR-NUM2    NOT =   ZERO
               MOVE    1                   TO  FLG-ERR2
           END-IF
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-MAE-INPUT
           IF     (KANACHK-RC      NOT =   ZERO  )
      *2019.07  改行コード
              OR  (KANACHK-RC4     NOT =   ZERO  )
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT
           END-IF
      *
           .
       880-HALF-ZENCNG-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角チェック処理
      *****************************************************************
       891-HALF-CHK-SEC         SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-MAX             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           IF     (KANACHK-RC          =   ZERO  )  AND
                  (KANACHK-SYUBETU     =   1     )
      *R01.8  改行チェック
            AND   (KANACHK-RC4         =   ZERO  )
      *        半角
               MOVE    ZERO                TO  FLG-ERR
           ELSE
               MOVE    1                   TO  FLG-ERR
           END-IF
      *
           .
       891-HALF-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
           WHEN    "01"
               MOVE    "患者番号が未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "02"
               MOVE    "患者氏名が未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "03"
               MOVE    "患者カナ氏名が未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "04"
               MOVE    "生年月日が未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "05"
               MOVE    "性別が未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "06"
               MOVE    "電子カルテＵＩＤが未設定です。"
                                               TO  WRK-ERRMSG
           WHEN    "07"
               MOVE    "オルカＵＩＤが未設定です。"
                                               TO  WRK-ERRMSG
           WHEN    "08"
               STRING  "初回接続が完了していません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "09"
               STRING  "処理区分がありません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "10"
               MOVE    "患者番号に該当する患者が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "患者番号の自動採番はできません。"
                                               TO  WRK-ERRMSG
           WHEN    "12"
               MOVE    "患者番号構成誤り"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE    "患者番号登録済み"
                                               TO  WRK-ERRMSG
           WHEN    "14"
               MOVE    "漢字氏名は全角で入力して下さい"
                                               TO  WRK-ERRMSG
           WHEN    "15"
               MOVE    "カナ氏名にエラーがあります"
                                               TO  WRK-ERRMSG
           WHEN    "16"
               MOVE    "性別は１：男２：女で入力して下さい"
                                               TO  WRK-ERRMSG
           WHEN    "17"
               MOVE    "生年月日暦日エラー"
                                               TO  WRK-ERRMSG
           WHEN    "18"
      ****     MOVE    "患者番号採番エラー"
               MOVE    "患者番号にエラーがあります"
                                               TO  WRK-ERRMSG
           WHEN    "19"
               STRING  "前回と処理区分が違います。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "20"
               STRING  "選択項目があります。"
                       "選択結果を返却してください。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "21"
               STRING  "選択結果エラーです。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "22"
               MOVE    "前回と患者番号が違います。"
                                               TO  WRK-ERRMSG
      *エラー
           WHEN    "P1"
           WHEN    "K2"
               PERFORM 8901-PTERRCD-SET-SEC
      *新規
           WHEN    "40"
               MOVE    "患者番号採番ができません。"
                                               TO  WRK-ERRMSG
           WHEN    "41"
               MOVE    "システム管理更新エラー"
                                               TO  WRK-ERRMSG
           WHEN    "42"
               MOVE    "患者番号登録エラー"
                                               TO  WRK-ERRMSG
           WHEN    "43"
               MOVE    "患者番号登録済みエラー"
                                               TO  WRK-ERRMSG
           WHEN    "44"
               MOVE    "患者情報登録エラー"
                                               TO  WRK-ERRMSG
      *    WHEN    "44"
      *        STRING  "新規登録は、初回接続から行って下さい。"
      *                                        DELIMITED  BY  SIZE
      *                                        INTO    WRK-ERRMSG
      *        END-STRING
      *                                        TO  WRK-ERRMSG
      *更新
           WHEN    "51"
               MOVE    "患者情報更新エラー"
                                               TO  WRK-ERRMSG
           WHEN    "52"
               MOVE    "旧姓履歴登録エラー"
                                               TO  WRK-ERRMSG
           WHEN    "55"
               MOVE    "登録処理をキャンセルしました。"
                                               TO  WRK-ERRMSG
      *
      *削除
           WHEN    "70"
               STRING  "送信内容と患者登録内容が一致しません。"
                       "削除できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "71"
               STRING  "削除の処理順が違います"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "73"
               STRING  "削除処理をキャンセルしました。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "75"
               MOVE    "削除処理エラー"
                                               TO  WRK-ERRMSG
      *
           WHEN    "80"
               STRING  "一時データ出力エラー"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "81"
               STRING  "解除対象がありません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *警告
           WHEN    "K1"
               STRING  "警告！自動採番の患者番号が最大値です。"
                       "初期番号から採番しました。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "K3"
               MOVE    "旧姓履歴を登録しました。"
                                               TO  WRK-ERRMSG
           WHEN    "K4"
               STRING  "警告！カナ氏名にカナ以外の文字があります。"
                       "確認して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
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
               MOVE    "リクエスト番号不正"    TO  WRK-ERRMSG
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
      *    患者登録エラーメッセージ編集処理
      *****************************************************************
       8901-PTERRCD-SET-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
      *    エラーメッセージ編集（ＡＰＩと共通）
           CALL    "ORCSP90"           USING   SPA-AREA
                                               SPA-P02KYOUTU
                                               SPA-P02SCRAREA
           MOVE    SPA-ERRMSG          TO  WRK-ERRMSG
      *
           .
        8901-PTERRCD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報エラーメッセージ
      *****************************************************************
       8901-PTERRCD-SETAPI-SEC              SECTION.
      *
           EVALUATE    SPA-ERRCD
           WHEN    "A020"
               MOVE    "自宅電話番号は半角で入力して下さい"
                                               TO  WRK-ERRMSG
           WHEN    "A021"
               MOVE    "連絡電話番号は半角で入力して下さい"
                                               TO  WRK-ERRMSG
           WHEN    "A022"
               MOVE    "職業エラー"
                                               TO  WRK-ERRMSG
           WHEN    "A023"
               MOVE    "電子メールアドレスエラー"
                                               TO  WRK-ERRMSG
           WHEN    "A024"
               MOVE    "禁忌１エラー"
                                               TO  WRK-ERRMSG
           WHEN    "A025"
               MOVE    "禁忌２エラー"
                                               TO  WRK-ERRMSG
           WHEN    "A026"
               MOVE    "アレルギー１エラー"
                                               TO  WRK-ERRMSG
           WHEN    "A027"
               MOVE    "アレルギー２エラー"
                                               TO  WRK-ERRMSG
           WHEN    "A028"
               MOVE    "感染症１エラー"
                                               TO  WRK-ERRMSG
           WHEN    "A029"
               MOVE    "感染症２エラー"
                                               TO  WRK-ERRMSG
           WHEN    "A030"
               MOVE    "コメント１エラー"
                                               TO  WRK-ERRMSG
           WHEN    "A031"
               MOVE    "コメント２エラー"
                                               TO  WRK-ERRMSG
           WHEN    "A032"
               MOVE    "テスト患者区分エラー"
                                               TO  WRK-ERRMSG
           WHEN    "A033"
               MOVE    "死亡区分エラー"
                                               TO  WRK-ERRMSG
           WHEN    "A034"
               MOVE    "状態１エラー"
                                               TO  WRK-ERRMSG
           WHEN    "A035"
               MOVE    "状態２エラー"
                                               TO  WRK-ERRMSG
           WHEN    "A036"
               MOVE    "状態３エラー"
                                               TO  WRK-ERRMSG
           END-EVALUATE
            .
       8901-PTERRCD-SETAPI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者登録確認メッセージ編集処理
      *****************************************************************
       8902-SELECTMSG-SEC              SECTION.
      *
           EVALUATE    PTMODRES-SELECT-CODE
               WHEN    "0101"
                   STRING  "二重登録疑いの患者が存在します。"
                           "登録しますか？"
                                           DELIMITED   BY  SIZE
                                           INTO    PTMODRES-SELECT-MSG
                   END-STRING
               WHEN    "0103"
                   STRING  "該当患者のデータをすべて削除します。"
                           "復元はできません。"
                                           DELIMITED   BY  SIZE
                                           INTO    PTMODRES-SELECT-MSG
                   END-STRING
               WHEN    "0113"
                   STRING  "受診のある患者です。"
                           "削除後に復元はできません。"
                                           DELIMITED   BY  SIZE
                                           INTO    PTMODRES-SELECT-MSG
                   END-STRING
           END-EVALUATE
            .
       8902-SELECTMSG-SEC-EXT.
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
               PERFORM 910-PTINF-NEXT-SEC
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
      *
      *****************************************************************
      *    患者番号マスター読込処理
      *****************************************************************
       910-PTINF-NEXT-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTINF
               MOVE    MCPDATA-REC         TO  PTINF-REC
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           .
       910-PTINF-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号マスター読込処理
      *****************************************************************
       910-PTNUM-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTNUM
               MOVE    MCPDATA-REC         TO  PTNUM-REC
           ELSE
               MOVE    1                   TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           .
       910-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
      *    MOVE    MCP-TERM            TO  WRK-MCP-TERM
      *    MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
           MOVE    "P02"               TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    1                   TO  SLOCK-MODE
           MOVE    PTINF-PTID          TO  SLOCK-PTID
      *    電子カルテキー
           MOVE    PTMODREQ-KARTE-UID  TO  SLOCK-KARTE-UID
      *    API+オルカＵＩＤ をTERMID
           MOVE    SPA-TERMID          TO  MCP-TERM
      *
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF
      *
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
           MOVE    WRK-MCP-TERM        TO  MCP-TERM
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理(削除)
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    排他制御解除処理
           MOVE    "P02"               TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    ZERO                TO  SLOCK-PTID
           MOVE    3                   TO  SLOCK-MODE
      *    API+オルカＵＩＤ をTERMID
           MOVE    SPA-TERMID          TO  MCP-TERM
      *
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           MOVE    WRK-MCP-TERM        TO  MCP-TERM
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
           .
       990-LOCK-OUT-EXT.
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
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    旧姓履歴情報取得
      *****************************************************************
       903-KYUSEIRRK-READ-SEC              SECTION.
      *
           MOVE    KYUSEIRRK-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-KYUSEIRRK
                   MOVE    MCPDATA-REC         TO  KYUSEIRRK-REC
               ELSE
                   MOVE    1                   TO  FLG-KYUSEIRRK
                   INITIALIZE                  KYUSEIRRK-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-KYUSEIRRK
               INITIALIZE                  KYUSEIRRK-REC
           END-IF
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       903-KYUSEIRRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＡＲＡデータ検索処理
      *****************************************************************
       9010-APIPARA-KEYREAD-SEC         SECTION.
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_api_para"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  APIPARA-REC
                   MOVE    ZERO            TO  FLG-APIPARA
               ELSE
                   INITIALIZE                  APIPARA-REC
                   MOVE    1               TO  FLG-APIPARA
               END-IF
           ELSE
               INITIALIZE                  APIPARA-REC
               MOVE    1               TO  FLG-APIPARA
           END-IF
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9010-APIPARA-KEYREAD-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    テーブルCALL処理
      *****************************************************************
       900-ORCDBMAIN-SEC                SECTION.
      *
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-ORCDBMAIN-EXT.
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
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       920-DBFETCH-EXT.
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
