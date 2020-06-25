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
       PROGRAM-ID.         ORAPI012S6V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 患者　介護保険情報・介護認定情報等設定
      *                                               （登録・削除）(V3)
      *  管理者            :
      *  作成日付    作業者        記述
      *  16/10/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.00    NACL-多々納  19/07/XX  検索処理追加
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
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-PTCARE-HKNINF       PIC 9(01).
           03  FLG-PTCARE-NINTEI       PIC 9(01).
           03  FLG-SPATMP              PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
      *
           03  FLG-ERR                 PIC 9(01).
           03  FLG-ERR2                PIC 9(01).
           03  FLG-DAYERR              PIC 9(01).
           03  FLG-SYORI               PIC 9(01).
           03  FLG-HKNEND              PIC 9(01).
           03  FLG-SYORIEND            PIC 9(01).
           03  FLG-CONTKBN             PIC 9(01).
           03  FLG-ERR-01              PIC 9(01).
           03  FLG-OK                  PIC 9(01).
      *
      *    03  FLG-P02A-SYORI          PIC 9(01).
      *    03  FLG-DATA                PIC 9(01).
      **** 03  FLG-DELETE              PIC 9(01).
           03  FLG-CHKERR              PIC 9(01).
           03  FLG-HENSYU              PIC 9(01).
           03  FLG-CANSEL              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDY2                    PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDH                     PIC 9(04).
           03  IDY1                    PIC 9(04).
           03  IDX1                    PIC 9(04).
           03  IDX2                    PIC 9(04).
           03  IDR                     PIC 9(04).
           03  IDM                     PIC 9(04).
           03  IDX-R                   PIC 9(04).
      *
           03  IDX-ERR                 PIC 9(04).
           03  IDX-WAR                 PIC 9(04).
      *
           03  IDD                     PIC 9(04).
           03  IDD2                    PIC 9(04).
           03  IDF                     PIC 9(04).
           03  IDX3                     PIC 9(04).
           03  IDX-T                    PIC 9(04).
           03  IDX-N                    PIC 9(04).
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
           03  WRK-ENDYMD.
               05  WRK-ENDYY           PIC 9(04).
               05  WRK-ENDMM           PIC 9(02).
               05  WRK-ENDDD           PIC 9(02).
      *
      *    エラーコード
           03  WRK-ERRCD-G.
               05  WRK-ERRKBN              PIC X(01).
               05  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(200).
           03  WRK-ERRCD-2             PIC X(02).
           03  WRK-ERRMSG-1            PIC X(200).
           03  WRK-WERRCD              PIC X(03).
      *    エラー種類
           03  WRK-ERR-AREA.
               05  WRK-ERR-NUM             PIC X(03).
               05  WRK-ERR-NUMMEI          PIC X(20).
               05  WRK-ERR-NUMMEI2         PIC X(20).
               05  WRK-ERR-YMD             PIC X(22).
               05  WRK-ERR-CNT             PIC X(01).
      *
      *    ワーニングコード
           03  WRK-KEICD               PIC X(03).
           03  WRK-OKMSG               PIC X(100).
      *    項目毎のエラー
           03  WRK-ERRCD-TBLG.
               05  WRK-ERRCD-TBL       OCCURS   100.
                   07  WRK-ERRCD-T         PIC X(02).
      *
      *    要介護状態
           03  WRK-CAREJOTAICD         PIC X(02).
      *
      *    オルカUID
           03  WRK-ORCA-UID            PIC X(36).
           03  WRK-OPID                PIC X(16).
      *
           03  HZN-SPA-OPID            PIC X(16).
      *
           03  WRK-ENDKBN              PIC X(01).
      *
           03  WRK-MCP-PATHNAME        PIC X(62).
           03  WRK-MCP-WINDOW          PIC X(62).
      *    MCP-TERMID
           03  WRK-MCP-TERM            PIC X(64).
      *
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(100).
           03  WRK-ATO-INPUT           PIC X(100).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *
      *
       01  WRK-XML-AREA.
           03  WRK-PTNUM               PIC X(20).
           03  WRK-BIRTHDAY-YMD        PIC X(08).
      *    基準日
           03  WRK-BASE-YMD            PIC X(08).
           03  WRK-BASE-YMD-R      REDEFINES  WRK-BASE-YMD.
               05  WRK-BYY             PIC 9(04).
               05  WRK-BMM             PIC 9(02).
               05  WRK-BDD             PIC 9(02).
      *
           03  WRK-WEIGHT              PIC 9(04).
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
      *        登録終了
               05  WRK-PARA-ENDKBN         PIC X(01).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *    全角
           03  WRK-HENYMD-Z        PIC X(22).
      *
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
      *APIV3
      *    患者画面スパ領域の保存
           COPY    "P02SCR-SPA"
                              REPLACING  //SPA-// BY //APIS-//.
      *
      *    患者介護保険情報テーブル
       01  TBL-PTCARE-HKNINF-AREA.
           02  TBL-PTCARE-HKNINF-REC    OCCURS   10.
           COPY    "CPPTCARE-HKNINF.INC"   REPLACING
                                     //PTCRHKN-// BY //TBL-PTCRHKN-//.
      *    患者介護認定者情報テーブル
       01  TBL-PTCARE-NINTEI-AREA.
           02  TBL-PTCARE-NINTEI-REC   OCCURS   50.
           COPY    "CPPTCARE-NINTEI.INC"   REPLACING
                                    //PTCRNIN-// BY //TBL-PTCRNIN-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ファイルデイレクトリ位置指定サブ
      **   COPY    "CPMKPASS.INC".
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
      *    COPY    "CPORCSP01.INC".
      *    画面編集サブ
           COPY    "CPORCSP02.INC".
      *    チェックデジット
      *    COPY    "CPORCCHKDGT.INC".
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
           COPY    "CPPATIENTMODV3REQ6.INC".
      *    API XML出力用定義体
           COPY    "CPPATIENTMODV3RES6.INC".
      *
      *    保険組合せ決定関数サブ
      *    COPY    "CPORCSAPIHKN.INC".
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *    補助区分名称編集パラメタ
      **** COPY    "CPORAPI012SUB1.INC".
      *    処理終了パラメタ
           COPY    "CPORAPIV3SUB01.INC".
      *
      *    保険組合せ編集パラメタ
      **   COPY    "CPORAPI012SUB2V3.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム基本
      *01  SYSBASE-REC.
      *    COPY    "CPSYSBASE.INC".
      *
      *    システムユーザー
      *01  SYSUSER-REC.
      *    COPY    "CPSYSUSER.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    （診療科情報）
      *    COPY    "CPSK1005.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *
           COPY  "CPSK4002.INC".
      *
           COPY  "CPSK1009.INC".
      *    患者マスタ−
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者介護保険情報テーブル
       01  PTCARE-HKNINF-REC.
           COPY    "CPPTCARE-HKNINF.INC".
      *    患者介護認定者情報テーブル
       01  PTCARE-NINTEI-REC.
           COPY    "CPPTCARE-NINTEI.INC".
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *    ＳＰＡ一時テーブル
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
      *    APIパラメタテーブル
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
      *
           COPY    "MCPDATA2.INC".
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
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   "99"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST08-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
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
           INITIALIZE                      TBL-PTCARE-HKNINF-AREA
           INITIALIZE                      TBL-PTCARE-NINTEI-AREA
           INITIALIZE                      WRK-APIV3DATA-AREA
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
           MOVE    ZERO                TO  IDX-WAR
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
      *
           EVALUATE    PTMODREQ-REQUEST-NUMBER
           WHEN    "01"
               MOVE    1                   TO  FLG-SYORI
           WHEN    "02"
               MOVE    2                   TO  FLG-SYORI
           WHEN    "99"
      *        強制処理終了
               PERFORM 2099-CANCEL-SEC
               MOVE    "00"                TO  PTMODRES-RESPONSE-NUMBER
               GO      TO      200-MAIN-EXT
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
           END-EVALUATE
      *
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               PERFORM 890-ERRCD-MSG-SEC
               IF      WRK-ERRCD           =   "08"
                   MOVE    "01"            TO  PTMODRES-RESPONSE-NUMBER
               END-IF
      *        編集処理
               PERFORM 20010-INPUT-HEN-SEC
      *        パラメタ削除・排他解除処理
               IF     (PTMODREQ-ORCA-UID   NOT =   SPACE  )
                 AND  (FLG-SYORIEND            =   1      )
                   PERFORM 2900-SYORI-END-SEC
                   MOVE    "01"            TO  PTMODRES-RESPONSE-NUMBER
                   MOVE    SPACE           TO  PTMODRES-CONT-MODE
               END-IF
               GO      TO      200-MAIN-EXT
           END-IF
      *
           EVALUATE    PTMODREQ-REQUEST-NUMBER
           WHEN    "01"
      *        初期処理
               PERFORM 2001-PTINF-SYORI-SEC
           WHEN    "02"
      *        確認処理
               PERFORM 2002-UPDATE-SYORI-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    強制処理終了
      *****************************************************************
       2099-CANCEL-SEC                 SECTION.
      *
           MOVE    PTMODREQ-PATIENTID  TO  WRK-PTNUM
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    WRK-PTNUM           TO  SPTID-PTNUM
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF      SPTID-RC            =   ZERO
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
           MOVE    9                   TO  FLG-SYORI
      *
      *    編集処理
           PERFORM 20010-INPUT-HEN-SEC
           .
       2099-CANCEL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-RGTXMLREAD-SEC   SECTION.
      *
           IF      APILST08-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-PATIENTMODREQ
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_patientmodv3req6"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST08-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "patientmodv3req6"     TO  APIREQ-RECORDNAME
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
           MOVE    "xml_patientmodv3req6"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST08-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "patientmodv3req6"  TO  APIREQ-RECORDNAME
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
                       MOVE    "検索処理終了"
                                           TO  WRK-OKMSG
                   WHEN    2
                       IF      PTMODRES-RESPONSE-NUMBER    =  "00"
                           MOVE    "登録処理終了"
                                           TO  WRK-OKMSG
                       ELSE
                           MOVE    "登録処理終了"
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
           IF     (IDX-ERR             >   ZERO )
               PERFORM 290-ERROR-HEN-SEC
           END-IF
      *
           IF      APILST08-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_patientmodv3res6"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  PTMODRES-MODE
           MOVE    "patientmodv3res6"  TO  PTMODRES-RECORDNAME
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
           MOVE    "xml_patientmodv3res6"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  PTMODRES-MODE
           MOVE    "patientmodv3res6"  TO  PTMODRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-PATIENTMODRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               MOVE    PTMODRES-OBJECT     TO  APILST08-BODY
               MOVE    "patientmod/xml"    TO  APILST08-CONTENT-TYPE
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
           END-IF
      *
           IF     (WRK-ERRCD               =   SPACE )
             AND  (PTMODREQ-REQUEST-NUMBER NOT =   "01"  )
      *        リクエスト０１以外は、同じ処理の続きであること
               IF      WRK-PARA-SYORIPG    NOT =   "06"
                   MOVE    "08"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
           .
       2009-PARA-INIT-EXT.
           EXIT.
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
               END-IF
           END-IF
      *
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PTNUM-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    1               TO  FLG-SYORIEND
           END-IF
      *    継続区分
           IF      PTMODREQ-CONT-MODE  =   "True"
               MOVE    1               TO  FLG-CONTKBN
           ELSE
               MOVE    ZERO            TO  FLG-CONTKBN
           END-IF
      *
      *    患者基本チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200211-KIHON-CHK-SEC
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    オルカＵＩＤ変換編集処理
      *****************************************************************
       2001-TERMID-HEN-SEC     SECTION.
      *
           MOVE    PTMODREQ-ORCA-UID   TO  WRK-ORCA-UID
           MOVE    SPACE               TO  SPA-TERMID
           STRING  "API+"
                   WRK-ORCA-UID        DELIMITED  BY  SPACE
                                           INTO    SPA-TERMID
           END-STRING
      *
           .
       2001-TERMID-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目変換編集処理
      *****************************************************************
       20010-INPUT-HEN-SEC     SECTION.
      *
      *    患者番号
           MOVE    PTMODREQ-PATIENTID      TO  PTMODRES-PATIENTID
           MOVE    PTMODREQ-PATIENTID      TO  WRK-PTNUM
      *    基準日
      *    MOVE    PTMODREQ-BASE-DATE      TO  PTMODRES-BASE-DATE
      *    MOVE    PTMODREQ-BASE-DATE      TO  WRK-HEN-DATE
      *    MOVE    SPACE                   TO  WRK-HEN-TIME
      *    PERFORM 802-DAYHEN02-SEC
      *    MOVE    WRK-SYMD                TO  WRK-BASE-YMD
      *    未設定時はシステム情報設定
      *    IF      WRK-BASE-YMD         =   SPACE
      *        MOVE    SMCNDATE-YMD        TO  WRK-BASE-YMD
      *    END-IF
      *    基準日をシステム日付へ
      *    MOVE    WRK-BASE-YMD        TO  SPA-SYSYMD
      *    画面表示編集
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    SPA-SYSYMD          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO 
               MOVE    LNK-DAY2-EDTYMD1    TO  SPA-SYSYMDWH
           ELSE
               MOVE    SPA-SYSYMD          TO  SPA-SYSYMDWH
           END-IF
      *
      *    患者情報
           MOVE    PTMODREQ-NAME           TO  PTMODRES-NAME
           MOVE    PTMODREQ-KANANAME       TO  PTMODRES-KANANAME
           MOVE    PTMODREQ-BIRTHDAY       TO  PTMODRES-BIRTHDAY
           MOVE    PTMODREQ-BIRTHDAY       TO  WRK-HEN-DATE
           MOVE    SPACE                   TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-BIRTHDAY-YMD
      *
           MOVE    PTMODREQ-SEX            TO  PTMODRES-SEX
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
      *    患者番号
           IF      WRK-PTNUM           =   SPACE
               MOVE    "01"                TO  WRK-ERRCD
           END-IF
      *
      *    UID（電子カルテ）
           IF      PTMODREQ-KARTE-UID  =   SPACE
               IF     (WRK-ERRCD           =   SPACE  )
                   MOVE    "06"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    オルカＵＩＤ
           IF      PTMODREQ-ORCA-UID   =   SPACE
               IF      PTMODREQ-REQUEST-NUMBER NOT =   "01"
                   MOVE    "07"                TO  WRK-ERRCD
               END-IF
           END-IF
           .
       20010-INIT-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック
      *****************************************************************
       20011-PTNUM-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  SPA-PTID
           MOVE    SPACE               TO  SPA-PTNUM
      *    患者番号取得
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
      *
           MOVE    WRK-PTNUM           TO  PTMODRES-PATIENTID
           .
       20011-PTNUM-CHK-EXT.
           EXIT.
      *
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
      *    患者・保険情報取得処理
      *****************************************************************
       2001-PTINF-SYORI-SEC        SECTION.
      *
           INITIALIZE                      APIS-P02SCRAREA
      *
      *    オルカＵＩＤ処理
           PERFORM 20020-ORCAUID-SYORI-SEC
      *
      *    患者番号判定
           PERFORM 20011-PTINF-INITCHK-SEC
      *
           IF      PTMODREQ-ORCA-UID   NOT =   SPACE
               PERFORM 2902-PARA-ALLDEL-SEC
               PERFORM 2009-SPATMP-DEL-SEC
           END-IF
      *
      *    基本情報処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20012-PTINF-HEN-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        介護保険情報編集処理
               PERFORM 20013-PTCARE-HKNINF-HENSYU-SEC
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        介護認定者情報編集処理
               PERFORM 20014-PTCARE-NINTEI-HENSYU-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
               MOVE    "02"            TO  PTMODRES-RESPONSE-NUMBER
           ELSE
               MOVE    "01"            TO  PTMODRES-RESPONSE-NUMBER
               IF      FLG-SYORIEND    =   1
                   MOVE    PTMODREQ-ORCA-UID   TO  PTMODRES-ORCA-UID
               END-IF
           END-IF
      *    エラー判定
           IF      WRK-ERRCD           =   SPACE
               IF      IDX-ERR             >   ZERO
                   MOVE    "60"               TO  WRK-ERRCD
               END-IF
           END-IF
      *    パラメタ登録
           IF      FLG-SYORIEND    =   1
               IF      PTMODRES-ORCA-UID   NOT =   SPACE
                   PERFORM 2902-APIPARA-INS-SEC
               END-IF
           ELSE
               PERFORM 2901-PARA-INS-SEC
           END-IF
      *
           .
       2001-PTINF-SYORI-EXT.
           EXIT.
      ****************************************************************
      *    患者番号判定共通処理
      ****************************************************************
       20011-PTINF-INITCHK-SEC      SECTION.
      *
      *    患者番号判定
           IF     (FLG-SYORI       NOT =   1              )
             AND  (SPA-PTNUM       NOT =   WRK-PARA-PTNUM )
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
      *            排他制御なし
                   MOVE    1               TO  FLG-SYORIEND
               END-IF
           END-IF
      *    排他チェック & ロック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 990-LOCK-IN-SEC
               IF      SLOCK-RETURN    NOT =   ZERO
                   MOVE    "90"            TO  WRK-ERRCD
                   MOVE    "01"            TO  PTMODRES-RESPONSE-NUMBER
      *            排他制御なし
                   MOVE    1               TO  FLG-SYORIEND
               END-IF
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        患者情報出力内容編集処理
               PERFORM 20021-PTMODRES-HEN-SEC
           END-IF
      *
      ***  IF     (FLG-SYORI       NOT =   1    )
           IF     (WRK-ERRCD           =   SPACE)
      *        氏名・性別・生年月日の一致
               IF     ((PTMODREQ-NAME  NOT =    SPACE        )  AND
                       (PTMODREQ-NAME  NOT =    PTMODRES-NAME))
                  OR  ((PTMODREQ-SEX   NOT =    SPACE        )  AND
                       (PTMODREQ-SEX   NOT =    PTMODRES-SEX ))
                  OR  ((PTMODREQ-BIRTHDAY  NOT =   SPACE     )  AND
                       (PTMODREQ-BIRTHDAY  NOT =   PTMODRES-BIRTHDAY))
                   MOVE    "18"            TO  WRK-ERRCD
                   MOVE    "01"            TO  PTMODRES-RESPONSE-NUMBER
               END-IF
           END-IF
           .
       20011-PTINF-INITCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報出力内容編集処理
      *****************************************************************
       20021-PTMODRES-HEN-SEC        SECTION.
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
      *    自宅情報
      *    MOVE    PTINF-HOME-ADRS     TO  PTMODRES-HOME-ADDRESSES1
      *    MOVE    PTINF-HOME-BANTI    TO  PTMODRES-HOME-ADDRESSES2
           .
       20021-PTMODRES-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ登録処理
      *****************************************************************
       2901-PARA-INS-SEC        SECTION.
      *
      *    APIパラメタ登録
           PERFORM 2902-APIPARA-INS-SEC
      *
      *    パラメタ削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "AP12"              TO  PARA-GYOUMUID
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
      *    SPA保存
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "AP12"              TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "P02KYOUTU"         TO  PARA-FILEMEI
           MOVE    1                   TO  PARA-EDANUM
      *
           MOVE    SPA-P02KYOUTU       TO  PARA-DATA-REC
      *
           MOVE    SMCNDATE-YMD        TO  PARA-UPYMD
           MOVE    SMCNDATE-HMS        TO  PARA-UPHMS
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    "80"                TO  WRK-ERRCD
               DISPLAY "API PARA INSERT ERR:"  MCP-RC
                       ",KEY:" PARA-KEY
           END-IF
           .
       2901-PARA-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    APIパラメタ登録処理
      *****************************************************************
       2902-APIPARA-INS-SEC        SECTION.
      *
      *    パラメタ削除
           INITIALIZE                      APIPARA-REC
           MOVE    "AP12"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
      *    登録
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "AP12"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "API12V3"           TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
      *    レスポンス番号・電子カルテUID保存
           MOVE    PTMODREQ-KARTE-UID  TO  APIPARA-KARTE-UID
           MOVE    WRK-PTNUM           TO  APIPARA-PTNUM
      *
           MOVE    SPACE               TO  WRK-APIV3DATA-REC
           MOVE    PTMODRES-RESPONSE-NUMBER
                                       TO  WRK-RESPONSE-NUMBER
           MOVE    WRK-ENDKBN          TO  WRK-PARA-ENDKBN
      *    処理ＰＧ
           MOVE    "06"                TO  WRK-PARA-SYORIPG
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
                       ",KEY:" PARA-KEY
           END-IF
           .
       2902-APIPARA-INS-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    患者基本チェック処理
      *****************************************************************
       200211-KIHON-CHK-SEC        SECTION.
      *
      *    氏名チェック
           IF     (WRK-ERRCD           =   SPACE )
           AND    (PTMODREQ-NAME   NOT =   SPACE )
               PERFORM 20011-NAME-CHK-SEC
           END-IF
      *    カナ氏名チェック
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (PTMODREQ-KANANAME   NOT =   SPACE )
               PERFORM 20011-KANANAME-CHK-SEC
           END-IF
      *    性別チェック
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (PTMODREQ-SEX    NOT =   SPACE )
               PERFORM 20011-SEX-CHK-SEC
           END-IF
      *    生年月日暦日チェック
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (WRK-BIRTHDAY-YMD    NOT =   SPACE )
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
       20012-PTINF-HEN-SEC        SECTION.
      *
           MOVE    SPA-PTID            TO  SPA-GMN-PTID
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
      *    パラメタファイル、患者情報のセット
           MOVE    SPACE               TO  P02
           INITIALIZE                      P02
      *
           MOVE    PTINF-NAME          TO  SPA-NAME
           MOVE    ZERO                TO  SPA-P02-UKEID
           MOVE    ZERO                TO  SPA-P02-SYORI
           MOVE    "API"               TO  ORCSP02-SYORI
           CALL    "ORCSP02"           USING   ORCSP02AREA
                                               SPA-AREA
                                               SPA-P02KYOUTU
                                               SPA-P02SCRAREA
                                               P02
           MOVE    SPACE               TO  SPA-ERRCD
      *    公費法別一覧編集区分なし
           MOVE    "0"                 TO  SPA-NAI-KOHHBTKBN-FLG
      *
           .
       20012-PTINF-HEN-EXT.
           EXIT.
      *
      ****************************************************************
      *    介護保険情報情報編集処理
      ****************************************************************
       20013-PTCARE-HKNINF-HENSYU-SEC           SECTION.
      *
           INITIALIZE                  PTMODRES-CARE-INFO-G
      *    患者介護保険情報
           MOVE    SPACE               TO  PTCARE-HKNINF-REC
           INITIALIZE                  PTCARE-HKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTCRHKN-HOSPNUM
           MOVE    PTINF-PTID          TO  PTCRHKN-PTID
      *
           MOVE    PTCARE-HKNINF-REC   TO  MCPDATA-REC
           MOVE    "tbl_ptcare_hkninf" TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcare_hkninf" TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-PTCAREHKNINF-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTCARE-HKNINF
               INITIALIZE                  PTCARE-HKNINF-REC
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL      (FLG-PTCARE-HKNINF   =   1  )
                          OR  (IDX                 >=  10 )
               ADD     1                       TO  IDX
      *        保険者番号
               MOVE    PTCRHKN-HKNJANUM    TO
                                       PTMODRES-CARE-INSURANCE-NUMBER
                                                             (IDX)
      *        被保険者番号
               MOVE    PTCRHKN-HIHKNJANUM  TO
                                           PTMODRES-CARE-POLICYHOLDER
                                                             (IDX)
      *        開始日
               MOVE    PTCRHKN-TEKSTYMD    TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  PTMODRES-CARE-START-DATE
                                                             (IDX)
      *        終了日
               MOVE    PTCRHKN-TEKEDYMD    TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  PTMODRES-CARE-END-DATE
                                                             (IDX)
      *
               MOVE    "tbl_ptcare_hkninf" TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-PTCAREHKNINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptcare_hkninf" TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       20013-PTCARE-HKNINF-HENSYU-EXT.
           EXIT.
      *
      ****************************************************************
      *    介護認定者情報編集処理
      **************************************************************
       20014-PTCARE-NINTEI-HENSYU-SEC           SECTION.
      *
           INITIALIZE                  PTMODRES-NEED-INFO-G
      *    患者介護保険情報
           INITIALIZE                  PTCARE-NINTEI-REC
           MOVE    SPA-HOSPNUM         TO  PTCRNIN-HOSPNUM
           MOVE    PTINF-PTID          TO  PTCRNIN-PTID
      *
           MOVE    PTCARE-NINTEI-REC   TO  MCPDATA-REC
           MOVE    "tbl_ptcare_nintei" TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcare_nintei" TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-PTCARENINTEI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTCARE-NINTEI
               INITIALIZE                  PTCARE-NINTEI-REC
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL      (FLG-PTCARE-NINTEI   =   1  )
                          OR  (IDX                 >=  50 )
               ADD     1                   TO  IDX
      *        要介護状態
               MOVE    PTCRNIN-CAREJOTAICD TO
                                           PTMODRES-NEED-CARE-STATE-CODE
                                                             (IDX)
      *        要介護状態名称編集
               MOVE    PTCRNIN-CAREJOTAICD TO  WRK-CAREJOTAICD
               PERFORM 200221-PTCARE-CARESTATE-SEC
      *        認定年月日
               MOVE    PTCRNIN-NINTEIYMD   TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  PTMODRES-NEED-DATE
                                                             (IDX)
      *        開始日
               MOVE    PTCRNIN-TEKSTYMD    TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  PTMODRES-NEED-START-DATE
                                                             (IDX)
      *        終了日
               MOVE    PTCRNIN-TEKEDYMD    TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  PTMODRES-NEED-END-DATE
                                                             (IDX)
      *
               MOVE    "tbl_ptcare_nintei" TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-PTCARENINTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptcare_nintei" TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       20014-PTCARE-NINTEI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    要介護状態名称編集処理
      *****************************************************************
       200221-PTCARE-CARESTATE-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKERR
      *    要介護状態区分
           EVALUATE    WRK-CAREJOTAICD
               WHEN    "01"
                   MOVE    "自立"      TO  PTMODRES-NEED-CARE-STATE(IDX)
               WHEN    "11"
                   MOVE    "要支援"    TO  PTMODRES-NEED-CARE-STATE(IDX)
               WHEN    "12"
                   MOVE    "要支援１"  TO  PTMODRES-NEED-CARE-STATE(IDX)
               WHEN    "13"
                   MOVE    "要支援２"  TO  PTMODRES-NEED-CARE-STATE(IDX)
               WHEN    "21"
                   MOVE    "要介護１"  TO  PTMODRES-NEED-CARE-STATE(IDX)
               WHEN    "22"
                   MOVE    "要介護２"  TO  PTMODRES-NEED-CARE-STATE(IDX)
               WHEN    "23"
                   MOVE    "要介護３"  TO  PTMODRES-NEED-CARE-STATE(IDX)
               WHEN    "24"
                   MOVE    "要介護４"  TO  PTMODRES-NEED-CARE-STATE(IDX)
               WHEN    "25"
                   MOVE    "要介護５"  TO  PTMODRES-NEED-CARE-STATE(IDX)
      *H30.7
               WHEN    "31"
                   MOVE    "事業対象者"
                                       TO  PTMODRES-NEED-CARE-STATE(IDX)
               WHEN    OTHER
                   MOVE    1           TO  FLG-CHKERR
           END-EVALUATE
           .
       200221-PTCARE-CARESTATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新確認処理
      *****************************************************************
       2002-UPDATE-SYORI-SEC     SECTION.
      *
           MOVE    ZERO                TO  IDX-ERR
      *
      *    患者番号判定
           PERFORM 20011-PTINF-INITCHK-SEC
      * 
      *    基本情報処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20012-PTINF-HEN-SEC
           END-IF
      *    項目チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20021-KOUSIN-CHK-SEC
           END-IF
      *    更新処理
           IF     (WRK-ERRCD           =   SPACE )
              AND (IDX-ERR             =   ZERO  )
               PERFORM 20022-KOUSIN-SYORI-SEC
           END-IF
      *    エラー判定
           IF      WRK-ERRCD           =   SPACE
               IF      IDX-ERR             >   ZERO
                   MOVE    "60"               TO  WRK-ERRCD
               END-IF
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        継続
               IF      FLG-CONTKBN         =   1
      *            パラメタ削除
                   PERFORM 2902-PARA-ALLDEL-SEC
                   PERFORM 2009-SPATMP-DEL-SEC
      *            保険登録へ
                   MOVE    "01"            TO  PTMODRES-RESPONSE-NUMBER
      *            継続区分終了
                   MOVE    "1"             TO  WRK-ENDKBN
      *            APIパラメタ登録
                   PERFORM 2902-APIPARA-INS-SEC
               ELSE
                   MOVE    "00"            TO  PTMODRES-RESPONSE-NUMBER
      *            パラメタ削除・排他解除処理
                   PERFORM 2900-SYORI-END-SEC
               END-IF
           ELSE
               IF      PTMODRES-RESPONSE-NUMBER    =   SPACE
                   MOVE    "02"            TO  PTMODRES-RESPONSE-NUMBER
               END-IF
               IF      FLG-CANSEL      =   ZERO
      *            SPA再編集
                   PERFORM 2901-PARA-INS-SEC
                   PERFORM 2008-SPATMP-INS-SEC
               END-IF
           END-IF
      *
           .
       2002-UPDATE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新チェック処理
      *****************************************************************
       20021-KOUSIN-CHK-SEC         SECTION.
      *
      *    介護保険情報
           EVALUATE    PTMODREQ-CARE-MODE
               WHEN    "DELETE"
               WHEN    "Delete"
                   CONTINUE
               WHEN    "MODIFY"
               WHEN    "Modify"
                   PERFORM 200212-PTCARE-HKNINF-CHK-SEC
               WHEN    SPACE
                   CONTINUE
               WHEN    OTHER
                   ADD     1               TO  IDX-ERR
                   MOVE    "20"            TO  WRK-ERRCD-T (IDX-ERR)
           END-EVALUATE
      *
      *    要介護認定情報
           EVALUATE    PTMODREQ-NEED-MODE
               WHEN    "DELETE"
               WHEN    "Delete"
                   CONTINUE
               WHEN    "MODIFY"
               WHEN    "Modify"
                   PERFORM 200222-PTCARE-NINTEI-CHK-SEC
               WHEN    SPACE
                   CONTINUE
               WHEN    OTHER
                   ADD     1               TO  IDX-ERR
                   MOVE    "30"            TO  WRK-ERRCD-T (IDX-ERR)
           END-EVALUATE
      *
           .
       20021-KOUSIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       20022-KOUSIN-SYORI-SEC         SECTION.
      *
      *    介護保険情報
           EVALUATE    PTMODREQ-CARE-MODE
               WHEN    "DELETE"
               WHEN    "Delete"
                   PERFORM 200221-HKNINF-DELETE-SEC
               WHEN    "MODIFY"
               WHEN    "Modify"
                   PERFORM 200221-HKNINF-UPDATE-SEC
           END-EVALUATE
      *
      *    要介護認定情報
           EVALUATE    PTMODREQ-NEED-MODE
               WHEN    "DELETE"
               WHEN    "Delete"
                   PERFORM 200223-NINTEI-DELETE-SEC
               WHEN    "MODIFY"
               WHEN    "Modify"
                   PERFORM 200224-NINTEI-UPDATE-SEC
           END-EVALUATE
      *
           .
       20022-KOUSIN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    介護保険情報チェック処理
      *****************************************************************
       200212-PTCARE-HKNINF-CHK-SEC         SECTION.
      *
           INITIALIZE                  TBL-PTCARE-HKNINF-AREA
           MOVE    ZERO                TO  FLG-ERR-01
      *    送信内容チェック
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   10   )
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    PTMODREQ-CARE-INSURANCE-NUMBER (IDX)
                                   TO  PTMODRES-CARE-INSURANCE-NUMBER
                                                                (IDX)
               MOVE    PTMODREQ-CARE-POLICYHOLDER (IDX)
                                       TO  PTMODRES-CARE-POLICYHOLDER
                                                                (IDX)
               MOVE    PTMODREQ-CARE-START-DATE (IDX)
                                       TO  PTMODRES-CARE-START-DATE
                                                                (IDX)
               MOVE    PTMODREQ-CARE-END-DATE (IDX)
                                       TO  PTMODRES-CARE-END-DATE
                                                                (IDX)
      *        保険者番号
               IF       PTMODREQ-CARE-INSURANCE-NUMBER (IDX)
                                       NOT =   SPACE
      *            半角６文字
                   MOVE     PTMODREQ-CARE-INSURANCE-NUMBER (IDX)
                                                   TO  WRK-MAE-INPUT
                   MOVE    6                       TO  WRK-LEN
                   PERFORM 891-HALF-CHK-SEC
                   IF      FLG-ERR             =   ZERO
                       MOVE     PTMODREQ-CARE-INSURANCE-NUMBER (IDX)
                                               TO  TBL-PTCRHKN-HKNJANUM
                                                                (IDX)
                   ELSE
                       MOVE    "2811"          TO  SPA-ERRCD
                   END-IF
               END-IF
      *        被保険者番号
               IF       PTMODREQ-CARE-POLICYHOLDER (IDX)
                                       NOT =   SPACE
      *            半角１０文字
                   MOVE     PTMODREQ-CARE-POLICYHOLDER (IDX)
                                                   TO  WRK-MAE-INPUT
                   MOVE    10                      TO  WRK-LEN
                   PERFORM 891-HALF-CHK-SEC
                   IF      FLG-ERR             =   ZERO
                       MOVE     PTMODREQ-CARE-POLICYHOLDER (IDX)
                                           TO  TBL-PTCRHKN-HIHKNJANUM
                                                                (IDX)
                   ELSE
                       IF      SPA-ERRCD       =   SPACE
                          MOVE    "2812"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
      *        開始年月日
               IF      PTMODREQ-CARE-START-DATE (IDX)  =   SPACE
                   CONTINUE
               ELSE
                   MOVE    PTMODREQ-CARE-START-DATE(IDX)
                                           TO  WRK-HEN-DATE
                   MOVE    SPACE           TO  WRK-HEN-TIME
                   PERFORM 802-DAYHEN02-SEC
                   MOVE    WRK-SYMD        TO  TBL-PTCRHKN-TEKSTYMD
                                                         (IDX)
      *            暦日チェック
                   MOVE    TBL-PTCRHKN-TEKSTYMD (IDX)  TO  WRK-SYMD
                   PERFORM 803-ORCSDAY-CHEK-SEC
                   IF     (FLG-DAYERR          =   1    )
                     AND  (SPA-ERRCD           =   SPACE)
                       MOVE    "2815"          TO  SPA-ERRCD
                   END-IF
               END-IF
      *        終了年月日
               IF      PTMODREQ-CARE-END-DATE(IDX)  =   SPACE
                   CONTINUE
               ELSE
                   MOVE    PTMODREQ-CARE-END-DATE(IDX)
                                           TO  WRK-HEN-DATE
                   MOVE    SPACE           TO  WRK-HEN-TIME
                   PERFORM 802-DAYHEN02-SEC
                   MOVE    WRK-SYMD        TO  TBL-PTCRHKN-TEKEDYMD
                                                         (IDX)
      *            暦日チェック
                   MOVE    TBL-PTCRHKN-TEKEDYMD (IDX)  TO  WRK-SYMD
                   PERFORM 803-ORCSDAY-CHEK-SEC
                   IF     (FLG-DAYERR          =   1    )
                     AND  (SPA-ERRCD           =   SPACE)
                       MOVE    "2816"          TO  SPA-ERRCD
                   END-IF
               END-IF
      *        必須項目チェック（開始年月日）
               IF      PTMODREQ-CARE-START-DATE (IDX)  =   SPACE
      *            他項目は空白であること
                   IF     (PTMODREQ-CARE-INSURANCE-NUMBER (IDX)
                                                       =   SPACE )
                      AND (PTMODREQ-CARE-POLICYHOLDER (IDX)
                                                       =   SPACE )
                      AND (PTMODREQ-CARE-END-DATE (IDX)
                                                       =   SPACE )
                       CONTINUE
                   ELSE
                       IF      SPA-ERRCD       =   SPACE
                           MOVE    "2818"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
      *
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    SPA-ERRCD           TO  PTMODRES-CARE-ERRCD
                                                         (IDX)
                   MOVE    1                   TO  FLG-ERR-01
               END-IF
           END-PERFORM
      *
      *    基本チェック
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   10   )
                        OR    (TBL-PTCRHKN-TEKSTYMD (IDX)   =   SPACE )
               MOVE    SPACE               TO  SPA-ERRCD
      *        終了年月日
               IF      TBL-PTCRHKN-TEKEDYMD (IDX)  =   SPACE
                   MOVE    "99999999"          TO  TBL-PTCRHKN-TEKEDYMD
                                                                 (IDX)
                   MOVE    TBL-PTCRHKN-TEKEDYMD (IDX)
                                               TO  WRK-SYMD
                   MOVE    ZERO                TO  WRK-THMS
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE    TO  PTMODRES-CARE-END-DATE
                                                                (IDX)
               END-IF
      *        開始日＞終了日　エラー
               IF      TBL-PTCRHKN-TEKSTYMD (IDX)
                                           >  TBL-PTCRHKN-TEKEDYMD (IDX)
                   IF      SPA-ERRCD          =   SPACE
                       MOVE    "2817"              TO  SPA-ERRCD
                   END-IF
               END-IF
      *        保険者番号・被保険者番号どちらか必須
               IF    (PTMODREQ-CARE-INSURANCE-NUMBER (IDX)
                                           =   SPACE )
                 AND (PTMODREQ-CARE-POLICYHOLDER (IDX)
                                           =   SPACE )
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "2813"          TO  SPA-ERRCD
                   END-IF
               END-IF
      *
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    SPA-ERRCD           TO  PTMODRES-CARE-ERRCD
                                                         (IDX)
                   MOVE    1                   TO  FLG-ERR-01
                END-IF
           END-PERFORM
      *
      *    期間重複チェック
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   10   )
                        OR    (FLG-ERR-01  NOT =   ZERO  )
                        OR    (TBL-PTCRHKN-TEKSTYMD (IDX)   =   SPACE )
               COMPUTE IDX2            =   IDX     +  1
               PERFORM VARYING     IDX3    FROM    IDX2    BY  1
                           UNTIL  (IDX3        >   10   )
                            OR    (FLG-ERR-01  NOT =   ZERO  )
                            OR    (TBL-PTCRHKN-TEKSTYMD (IDX3)
                                                   =   SPACE )
                   IF      (TBL-PTCRHKN-TEKEDYMD (IDX3)
                                               <   TBL-PTCRHKN-TEKSTYMD
                                                             (IDX) )
                      OR   (TBL-PTCRHKN-TEKSTYMD (IDX3)
                                               >   TBL-PTCRHKN-TEKEDYMD
                                                             (IDX) )
                       CONTINUE
                   ELSE
                       MOVE    "2820"          TO  PTMODRES-CARE-ERRCD
                                                                 (IDX)
                       MOVE    1               TO  FLG-ERR-01
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    エラーメッセージ編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                      UNTIL   (IDX         >   10   )
               IF      PTMODRES-CARE-ERRCD (IDX) NOT =  SPACE
                   MOVE    PTMODRES-CARE-ERRCD (IDX) TO SPA-ERRCD
                   PERFORM 8901-PTERRCD-SET-SEC
                   MOVE    WRK-ERRMSG     TO  PTMODRES-CARE-ERRCD-MSG
                                                            (IDX)
                   MOVE    SPACE          TO  WRK-ERRMSG
                   MOVE    SPACE          TO  SPA-ERRCD
               END-IF
           END-PERFORM
      *
           IF      FLG-ERR-01      NOT =   ZERO
               ADD    1                TO  IDX-ERR
               MOVE    "21"            TO  WRK-ERRCD-T (IDX-ERR)
           END-IF
      *    対象なし
           IF     (TBL-PTCRHKN-TEKSTYMD (1) =   SPACE )
             AND  (FLG-ERR-01              =   ZERO  )
               ADD     1               TO  IDX-ERR
               MOVE    "22"            TO  WRK-ERRCD-T(IDX-ERR)
           END-IF
           .
       200212-PTCARE-HKNINF-CHK-EXT.
           EXIT.
      *****************************************************************
      *    要介護認定情報チェック処理
      *****************************************************************
       200222-PTCARE-NINTEI-CHK-SEC         SECTION.
      *
           INITIALIZE                  TBL-PTCARE-NINTEI-AREA
           MOVE    ZERO                TO  FLG-ERR-01
      *    送信内容チェック
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   50   )
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    PTMODREQ-NEED-CARE-STATE-CODE (IDX)
                                       TO  PTMODRES-NEED-CARE-STATE-CODE
                                                                (IDX)
               MOVE    PTMODREQ-NEED-DATE (IDX)
                                       TO  PTMODRES-NEED-DATE
                                                                (IDX)
               MOVE    PTMODREQ-NEED-START-DATE (IDX)
                                       TO  PTMODRES-NEED-START-DATE
                                                                (IDX)
               MOVE    PTMODREQ-NEED-END-DATE (IDX)
                                       TO  PTMODRES-NEED-END-DATE
                                                                (IDX)
      *        要介護状態区分
               IF       PTMODREQ-NEED-CARE-STATE-CODE (IDX)
                                       NOT =   SPACE
      *            要介護状態名称編集
                   MOVE    PTMODREQ-NEED-CARE-STATE-CODE (IDX)
                                               TO  WRK-CAREJOTAICD
                   PERFORM 200221-PTCARE-CARESTATE-SEC
                   IF      FLG-CHKERR          =   1
                       MOVE    "2813"          TO  SPA-ERRCD
                   ELSE
                       MOVE    PTMODREQ-NEED-CARE-STATE-CODE (IDX)
                                           TO  TBL-PTCRNIN-CAREJOTAICD
                                                                (IDX)
                   END-IF
               END-IF
      *        認定年月日
               IF      PTMODREQ-NEED-DATE (IDX)  =   SPACE
                   CONTINUE
               ELSE
                   MOVE    PTMODREQ-NEED-DATE  (IDX)
                                           TO  WRK-HEN-DATE
                   MOVE    SPACE           TO  WRK-HEN-TIME
                   PERFORM 802-DAYHEN02-SEC
                   MOVE    WRK-SYMD        TO  TBL-PTCRNIN-NINTEIYMD
                                                         (IDX)
      *            暦日チェック
                   MOVE    TBL-PTCRNIN-NINTEIYMD (IDX) TO  WRK-SYMD
                   PERFORM 803-ORCSDAY-CHEK-SEC
                   IF     (FLG-DAYERR          =   1    )
                     AND  (SPA-ERRCD           =   SPACE)
                       MOVE    "2822"          TO  SPA-ERRCD
                   END-IF
               END-IF
      *        開始年月日
               IF      PTMODREQ-NEED-START-DATE (IDX)  =   SPACE
                   CONTINUE
               ELSE
                   MOVE    PTMODREQ-NEED-START-DATE(IDX)
                                           TO  WRK-HEN-DATE
                   MOVE    SPACE           TO  WRK-HEN-TIME
                   PERFORM 802-DAYHEN02-SEC
                   MOVE    WRK-SYMD        TO  TBL-PTCRNIN-TEKSTYMD
                                                         (IDX)
      *            暦日チェック
                   MOVE    TBL-PTCRNIN-TEKSTYMD (IDX)  TO  WRK-SYMD
                   PERFORM 803-ORCSDAY-CHEK-SEC
                   IF     (FLG-DAYERR          =   1    )
                     AND  (SPA-ERRCD           =   SPACE)
                       MOVE    "2815"          TO  SPA-ERRCD
                   END-IF
               END-IF
      *        終了年月日
               IF      PTMODREQ-NEED-END-DATE(IDX)  =   SPACE
                   CONTINUE
               ELSE
                   MOVE    PTMODREQ-NEED-END-DATE(IDX)
                                           TO  WRK-HEN-DATE
                   MOVE    SPACE           TO  WRK-HEN-TIME
                   PERFORM 802-DAYHEN02-SEC
                   MOVE    WRK-SYMD        TO  TBL-PTCRNIN-TEKEDYMD
                                                         (IDX)
      *            暦日チェック
                   MOVE    TBL-PTCRNIN-TEKEDYMD (IDX)  TO  WRK-SYMD
                   PERFORM 803-ORCSDAY-CHEK-SEC
                   IF     (FLG-DAYERR          =   1    )
                     AND  (SPA-ERRCD           =   SPACE)
                       MOVE    "2816"          TO  SPA-ERRCD
                   END-IF
               END-IF
      *        必須項目チェック（開始年月日）
               IF      PTMODREQ-NEED-START-DATE (IDX)  =   SPACE
      *            他項目は空白であること
                   IF     (PTMODREQ-NEED-CARE-STATE-CODE (IDX)
                                                       =   SPACE )
                      AND (PTMODREQ-NEED-DATE (IDX)
                                                       =   SPACE )
                      AND (PTMODREQ-NEED-END-DATE (IDX)
                                                       =   SPACE )
                       CONTINUE
                   ELSE
                       IF      SPA-ERRCD       =   SPACE
                           MOVE    "2818"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
      *
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    SPA-ERRCD           TO  PTMODRES-NEED-ERRCD
                                                         (IDX)
                   MOVE    1                   TO  FLG-ERR-01
                END-IF
           END-PERFORM
      *
      *    基本チェック
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   50   )
                        OR    (TBL-PTCRNIN-TEKSTYMD (IDX)   =   SPACE )
               MOVE    SPACE               TO  SPA-ERRCD
      *        終了年月日
               IF      TBL-PTCRNIN-TEKEDYMD (IDX)  =   SPACE
                   MOVE    "99999999"          TO  TBL-PTCRNIN-TEKEDYMD
                                                                 (IDX)
                   MOVE    TBL-PTCRNIN-TEKEDYMD (IDX)
                                               TO  WRK-SYMD
                   MOVE    ZERO                TO  WRK-THMS
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE    TO  PTMODRES-NEED-END-DATE
                                                                (IDX)
               END-IF
      *        開始日＞終了日　エラー
               IF      TBL-PTCRNIN-TEKSTYMD (IDX)
                                           >  TBL-PTCRNIN-TEKEDYMD (IDX)
                   IF      SPA-ERRCD          =   SPACE
                       MOVE    "2817"              TO  SPA-ERRCD
                   END-IF
               END-IF
      *        介護状態必須
               IF    (TBL-PTCRNIN-CAREJOTAICD (IDX)
                                           =   SPACE )
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "2821"          TO  SPA-ERRCD
                   END-IF
               END-IF
      *
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    SPA-ERRCD           TO  PTMODRES-NEED-ERRCD
                                                         (IDX)
                   MOVE    1                   TO  FLG-ERR-01
                END-IF
           END-PERFORM
      *
      *    期間重複チェック
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   50   )
                        OR    (FLG-ERR-01  NOT =   ZERO  )
                        OR    (TBL-PTCRNIN-TEKSTYMD (IDX)   =   SPACE )
               COMPUTE IDX2            =   IDX     +  1
               PERFORM VARYING     IDX3    FROM    IDX2    BY  1
                           UNTIL  (IDX3        >   50   )
                            OR    (FLG-ERR-01  NOT =   ZERO  )
                            OR    (TBL-PTCRNIN-TEKSTYMD (IDX3)
                                                   =   SPACE )
                   IF      (TBL-PTCRNIN-TEKEDYMD (IDX3)
                                               <   TBL-PTCRNIN-TEKSTYMD
                                                             (IDX) )
                      OR   (TBL-PTCRNIN-TEKSTYMD (IDX3)
                                               >   TBL-PTCRNIN-TEKEDYMD
                                                             (IDX) )
                       CONTINUE
                   ELSE
                       MOVE    "2820"          TO  PTMODRES-NEED-ERRCD
                                                                 (IDX)
                       MOVE    1               TO  FLG-ERR-01
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    エラーメッセージ編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                      UNTIL   (IDX         >   50   )
               IF      PTMODRES-NEED-ERRCD (IDX) NOT =  SPACE
                   MOVE    PTMODRES-NEED-ERRCD (IDX) TO SPA-ERRCD
                   PERFORM 8901-PTERRCD-SET-SEC
                   MOVE    WRK-ERRMSG     TO  PTMODRES-NEED-ERRCD-MSG
                                                            (IDX)
                   MOVE    SPACE          TO  WRK-ERRMSG
                   MOVE    SPACE          TO  SPA-ERRCD
               END-IF
           END-PERFORM
      *
           IF      FLG-ERR-01      NOT =   ZERO
               ADD    1                TO  IDX-ERR
               MOVE    "31"            TO  WRK-ERRCD-T (IDX-ERR)
           END-IF
      *    対象なし
           IF     (TBL-PTCRNIN-TEKSTYMD (1) =   SPACE )
             AND  (FLG-ERR-01              =   ZERO  )
               ADD     1               TO  IDX-ERR
               MOVE    "32"            TO  WRK-ERRCD-T(IDX-ERR)
           END-IF
           .
       200222-PTCARE-NINTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    介護保険情報削除処理
      *****************************************************************
       200221-HKNINF-DELETE-SEC         SECTION.
      *
      *    介護保険情報　削除
           INITIALIZE                      PTCARE-HKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTCRHKN-HOSPNUM
           MOVE    SPA-PTID            TO  PTCRHKN-PTID
           MOVE    PTCARE-HKNINF-REC   TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptcare_hkninf" TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE   "62"             TO  WRK-ERRCD
           END-IF
      *
           .
       200221-HKNINF-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    介護保険情報更新処理
      *****************************************************************
       200221-HKNINF-UPDATE-SEC         SECTION.
      *
      *    介護保険情報削除処理
           PERFORM 200221-HKNINF-DELETE-SEC
      *    追加処理
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   10   )
                        OR    (TBL-PTCRHKN-TEKSTYMD (IDX)  =   SPACE )
                        OR    (WRK-ERRCD   NOT =   SPACE )
               MOVE    TBL-PTCARE-HKNINF-REC (IDX)
                                           TO  PTCARE-HKNINF-REC
               MOVE    SPA-HOSPNUM         TO  PTCRHKN-HOSPNUM
               MOVE    SPA-PTID            TO  PTCRHKN-PTID
      *
               MOVE    SMCNDATE-YMD        TO  PTCRHKN-CREYMD
               MOVE    SMCNDATE-YMD        TO  PTCRHKN-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTCRHKN-UPHMS
      *
               MOVE    WRK-OPID            TO  PTCRHKN-OPID
      *
               MOVE    PTCARE-HKNINF-REC   TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_ptcare_hkninf"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "61"                TO  WRK-ERRCD
               END-IF
           END-PERFORM
      *
           .
       200221-HKNINF-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    要介護認定情報削除処理
      *****************************************************************
       200223-NINTEI-DELETE-SEC         SECTION.
      *
      *    要介護認定　削除
           INITIALIZE                      PTCARE-NINTEI-REC
           MOVE    SPA-HOSPNUM         TO  PTCRNIN-HOSPNUM
           MOVE    SPA-PTID            TO  PTCRNIN-PTID
           MOVE    PTCARE-NINTEI-REC   TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptcare_nintei" TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE   "64"             TO  WRK-ERRCD
           END-IF
      *
           .
       200223-NINTEI-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    要介護認定情報更新処理
      *****************************************************************
       200224-NINTEI-UPDATE-SEC         SECTION.
      *
      *    要介護認定情報削除処理
           PERFORM 200223-NINTEI-DELETE-SEC
      *    追加処理
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   50   )
                        OR    (TBL-PTCRNIN-TEKSTYMD (IDX)  =   SPACE )
                        OR    (WRK-ERRCD   NOT =   SPACE )
               MOVE    TBL-PTCARE-NINTEI-REC (IDX)
                                           TO  PTCARE-NINTEI-REC
               MOVE    SPA-HOSPNUM         TO  PTCRNIN-HOSPNUM
               MOVE    SPA-PTID            TO  PTCRNIN-PTID
      *
               MOVE    SMCNDATE-YMD        TO  PTCRNIN-CREYMD
               MOVE    SMCNDATE-YMD        TO  PTCRNIN-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTCRNIN-UPHMS
      *
               MOVE    WRK-OPID            TO  PTCRNIN-OPID
      *
      *        追加
               MOVE    PTCARE-NINTEI-REC   TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_ptcare_nintei" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "63"                TO  WRK-ERRCD
               END-IF
           END-PERFORM
      *
           .
       200224-NINTEI-UPDATE-EXT.
           EXIT.
      *
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
      *
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
      *
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200101-SELECT-HENSYU-SEC
           END-IF
           .
       20010-PTINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索編集処理
      *****************************************************************
       200101-SELECT-HENSYU-SEC        SECTION.
      *
           INITIALIZE                      APIS-P02SCRAREA
      *
      *    オルカＵＩＤ処理
           MOVE    SPACE               TO  PTMODREQ-ORCA-UID
           PERFORM 20020-ORCAUID-SYORI-SEC
           MOVE    SPACE               TO  PTMODRES-ORCA-UID
      *
           IF      WRK-ERRCD           =   SPACE
      *        患者情報出力内容編集処理
               PERFORM 20021-PTMODRES-HEN-SEC
           END-IF
      *    基本情報処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20012-PTINF-HEN-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        介護保険情報編集処理
               PERFORM 20013-PTCARE-HKNINF-HENSYU-SEC
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        介護認定者情報編集処理
               PERFORM 20014-PTCARE-NINTEI-HENSYU-SEC
           END-IF
      *
           PERFORM 2902-PARA-ALLDEL-SEC
           .
       200101-SELECT-HENSYU-EXT.
           EXIT.
      *R01.7
      *
      *
      ****************************************************************
      *    ＰＡＲＡのＳＰＡ編集処理
      ****************************************************************
       2031-SPA-PARA-SEC           SECTION.
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP12"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPA-P02"           TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
           PERFORM 9100-SPATMP-KEYREAD-SEC
           IF      FLG-SPATMP          =   ZERO
               MOVE    SPATMP-DATA-REC     TO  SPA-P02SCRAREA
           ELSE
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "AP12"              TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "P02KYOUTU"         TO  PARA-FILEMEI
           MOVE    1                   TO  PARA-EDANUM
           PERFORM 9010-PARA-KEYREAD-SEC
      *
           IF      FLG-PARA            =   ZERO
               MOVE    PARA-DATA-REC       TO  SPA-P02KYOUTU
           ELSE
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
           .
       2031-SPA-PARA-EXT.
           EXIT.
      *
      ****************************************************************
      *    エラーメッセージ編集処理
      ****************************************************************
       290-ERROR-HEN-SEC           SECTION.
      *
           PERFORM VARYING   IDX       FROM   1   BY  1
                   UNTIL    (IDX       >   50     )
                      OR    (WRK-ERRCD-T (IDX)    =   SPACE )
               MOVE    WRK-ERRCD-T (IDX)   TO  WRK-ERRCD
               PERFORM 8901-PTERRCD-SETAPI-SEC
               MOVE    "AP"                TO  SPA-ERRCD(1:2)
               MOVE    WRK-ERRCD-T (IDX)   TO  SPA-ERRCD(3:2)
               MOVE    SPA-ERRCD           TO  PTMODRES-ERROR
                                                           (IDX)
               MOVE    WRK-ERRMSG          TO  PTMODRES-ERROR-MSG
                                                           (IDX)
           END-PERFORM
           .
       290-ERROR-HEN-EXT.
           EXIT.
      *****************************************************************
      *    確認メッセージ編集処理
      *****************************************************************
      *292-SELECT-HEN-SEC            SECTION.
      *
      *    MOVE    SPA-PIDCD           TO  PTMODRES-SELECT-CODE
      *    PERFORM 8902-PTPIDCD-SET-SEC
      *    MOVE    WRK-ERRMSG          TO  PTMODRES-SELECT-MSG
      *    MOVE    SPACE               TO  WRK-ERRMSG
      *    .
      *292-SELECT-HEN-EXT.
      *    EXIT.
      *****************************************************************
      *    終了処理（PARA削除・排他解除）
      *****************************************************************
       2900-SYORI-END-SEC            SECTION.
      *
           PERFORM 2902-PARA-ALLDEL-SEC
           PERFORM 2009-SPATMP-DEL-SEC
      *    排他制御解除
           PERFORM 990-LOCK-OUT-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               DISPLAY "ロック解除失敗"
               MOVE    "81"            TO  WRK-ERRCD
           END-IF
           .
       2900-SYORI-END-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ削除処理
      *****************************************************************
       2902-PARA-ALLDEL-SEC          SECTION.
      *
      *    APIパラメタ削除
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "AP12"              TO  APIPARA-GYOUMUID
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
      *    パラメタ削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "AP12"              TO  PARA-GYOUMUID
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "P02"               TO  PARA-GYOUMUID
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       2902-PARA-ALLDEL-EXT.
           EXIT.
      *****************************************************************
      *     SPATMPの登録処理
      *****************************************************************
       2008-SPATMP-INS-SEC            SECTION.
      *
      *    削除
           PERFORM 2009-SPATMP-DEL-SEC
      *    TBL-SPATMP保存
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP12"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPA-P02"           TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
      *
           MOVE    SPA-P02SCRAREA      TO  SPATMP-DATA-REC
           PERFORM 20081-SPATMP-INSERT-SEC
      *
           .
       2008-SPATMP-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡ一時保存テーブル追加
      *****************************************************************
       20081-SPATMP-INSERT-SEC                SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  SPATMP-UPYMD
           MOVE    SMCNDATE-HMS        TO  SPATMP-UPHMS
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "SPATMP INSERT ERR:"  MCP-RC
                           ",KEY:" SPATMP-KEY
               MOVE    "80"                TO  WRK-ERRCD
           END-IF
           .
       20081-SPATMP-INSERT-EXT.
           EXIT.
      *****************************************************************
      *     SPATMPの削除処理
      *****************************************************************
       2009-SPATMP-DEL-SEC            SECTION.
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP12"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "SPATMP DELETE ERR:"  MCP-RC
                           ",KEY:" SPATMP-KEY
           END-IF
      *
           .
       2009-SPATMP-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ削除処理
      *****************************************************************
       4904-PARA-DEL-SEC          SECTION.
      *
      *    パラメタ削除
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       4904-PARA-DEL-EXT.
           EXIT.
      *****************************************************************
      *    日付暦日チェック
      *****************************************************************
       803-ORCSDAY-CHEK-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-DAYERR
           EVALUATE    WRK-SYMD
               WHEN    SPACE
                   MOVE    SPACE               TO  WRK-HENYMD
               WHEN    ZERO
                   MOVE    "00000000"          TO  WRK-HENYMD
               WHEN    "99999999"
                   MOVE    "99999999"          TO  WRK-HENYMD
               WHEN    OTHER
      *            日付チェックサブ
                   MOVE    SPACE               TO  STS-AREA-DAY
                   INITIALIZE                      STS-AREA-DAY
                   MOVE    SPACE               TO  LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                   IF      STS-DAY-RC1     NOT =   ZERO
                       MOVE    1                   TO  FLG-DAYERR
                   ELSE
                       MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
                   END-IF
           END-EVALUATE
      *
           .
       803-ORCSDAY-CHEK-EXT.
           EXIT.
      *****************************************************************
      *    日付暦日編集処理
      *****************************************************************
       803-ORCSDAY-CHEK02-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-DAYERR
           MOVE    SPACE               TO  WRK-HENYMD-Z
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    1                   TO  FLG-DAYERR
           ELSE
      *        全角空白削除
               MOVE    1                   TO  IDD2
               PERFORM VARYING     IDD     FROM    1   BY  2
                       UNTIL       IDD     >   22
                   IF      LNK-DAY2-EDTYMD3(IDD:2) =   "　"
                       CONTINUE
                   ELSE
                       MOVE    LNK-DAY2-EDTYMD3(IDD:2)
                                           TO  WRK-HENYMD-Z(IDD2:2)
                       ADD     2           TO  IDD2
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       803-ORCSDAY-CHEK02-EXT.
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
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
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
      *    拡張漢字有無
           INITIALIZE                  ORCSSTRINGAREA
           MOVE    "check"             TO  ORCSSTR-COMMAND
           MOVE    WRK-MAE-INPUT       TO  ORCSSTR-STRING1
           CALL    "orcsstring"        USING
                                       ORCSSTRINGAREA
           IF      ORCSSTR-NUM2    NOT =   ZERO
               MOVE    1                   TO  FLG-ERR
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
      *R01.8
              OR  (KANACHK-RC4     NOT =   ZERO  )
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
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
      *R01.8
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
      *R01.8
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
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           IF     (KANACHK-RC          =   ZERO  )  AND
                  (KANACHK-SYUBETU     =   1     )
      *R01.8
             AND  (KANACHK-RC4         =   ZERO  )
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
      *    WHEN    "02"
      *        MOVE    "患者氏名未設定"
      *                                        TO  WRK-ERRMSG
      *    WHEN    "03"
      *        MOVE    "患者カナ氏名未設定"
      *                                        TO  WRK-ERRMSG
      *    WHEN    "04"
      *        MOVE    "生年月日未設定"
      *                                        TO  WRK-ERRMSG
      *    WHEN    "05"
      *        MOVE    "性別未設定"
      *                                        TO  WRK-ERRMSG
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
           WHEN    "10"
               MOVE    "患者番号に該当する患者が存在しません"
                                               TO  WRK-ERRMSG
      **   WHEN    "11"
      *        MOVE    "基準日の暦日エラー"
      *                                        TO  WRK-ERRMSG
      *    WHEN    "12"
      *        MOVE    "基準日は平成２７年４月１日からです。"
      *                                        TO  WRK-ERRMSG
      *    WHEN    "13"
      *        MOVE    "患者番号登録済み"
      *                                        TO  WRK-ERRMSG
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
               STRING  "患者氏名・性別・生年月日が送信内容と違います。"
                       "患者を確認して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *
           WHEN    "22"
               MOVE    "前回と患者番号が違います。"
                                               TO  WRK-ERRMSG
      *
           WHEN    "60"
               STRING  "エラーがあります。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "61"
               MOVE    "介護保険情報更新エラー"
                                               TO  WRK-ERRMSG
           WHEN    "62"
               MOVE    "介護保険情報削除エラー"
                                               TO  WRK-ERRMSG
           WHEN    "63"
               MOVE    "要介護認定情報更新エラー"
                                               TO  WRK-ERRMSG
           WHEN    "64"
               MOVE    "要介護認定情報削除エラー"
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
      ******   MOVE    "処理区分未設定"        TO  WRK-ERRMSG
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
      *    個別エラーメッセージ
      *****************************************************************
       8901-PTERRCD-SETAPI-SEC              SECTION.
      *
           EVALUATE    WRK-ERRCD
           WHEN    "20"
               MOVE    "介護保険情報更新区分エラー"
                                               TO  WRK-ERRMSG
           WHEN    "21"
               MOVE    "介護保険情報の項目にエラーがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "22"
               MOVE    "介護保険情報の更新対象がありません。"
                                               TO  WRK-ERRMSG
      *
           WHEN    "30"
               MOVE    "要介護認定情報更新区分エラー"
                                               TO  WRK-ERRMSG
           WHEN    "31"
               MOVE    "要介護認定情報にエラーがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "32"
               MOVE    "要介護認定情報の更新対象がありません。"
                                               TO  WRK-ERRMSG
           END-EVALUATE
            .
       8901-PTERRCD-SETAPI-EXT.
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
           MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
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
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEYREAD-SEC      SECTION.
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
      *
      *****************************************************************
      *    患者介護保険情報データデータ読込処理
      *****************************************************************
       930-PTCAREHKNINF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTCARE-HKNINF
               MOVE    MCPDATA-REC         TO  PTCARE-HKNINF-REC
           ELSE
               MOVE    1                   TO  FLG-PTCARE-HKNINF
               INITIALIZE                  PTCARE-HKNINF-REC
           END-IF
      *
           .
       930-PTCAREHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者介護者認定情報データ読込処理
      *****************************************************************
       930-PTCARENINTEI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTCARE-NINTEI
               MOVE    MCPDATA-REC         TO  PTCARE-NINTEI-REC
           ELSE
               MOVE    1                   TO  FLG-PTCARE-NINTEI
               INITIALIZE                 PTCARE-NINTEI-REC
           END-IF
      *
           .
       930-PTCARENINTEI-READ-EXT.
           EXIT.
      *
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
      *****************************************************************
      *    ＰＡＲＡデータ検索処理
      *****************************************************************
       9010-PARA-KEYREAD-SEC         SECTION.
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9010-PARA-KEYREAD-EXT.
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
               MOVE    "DBFETCH"           TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
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
      *****************************************************************
      *    一時保存テーブル読み込み
      *****************************************************************
       9100-SPATMP-KEYREAD-SEC        SECTION.
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBFETCH"           TO  MCP-FUNC
               PERFORM 910-SPATMP-CALL-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA2-REC    TO  SPATMP-REC
                   MOVE    ZERO            TO  FLG-SPATMP
               ELSE
                   INITIALIZE                  SPATMP-REC
                   MOVE    1               TO  FLG-SPATMP
               END-IF
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
      *
           .
       9100-SPATMP-KEYREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブル検索処理
      *****************************************************************
       910-SPATMP-CALL-SEC                SECTION.
      *
           MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
      *
           CALL    "ORCDBSPATMP"       USING   MCPAREA
                                               MCPDATA2-REC
                                               SPA-AREA
      *
           .
       910-SPATMP-CALL-EXT.
           EXIT.
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
