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
       PROGRAM-ID.         ORAPI012S5V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 患者　公費負担額情報等設定
      *                                               （登録・削除）(V3)
      *  管理者            :
      *  作成日付    作業者        記述
      *  16/09/XX    NACL−多々納　新規作成
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
           03  FLG-PTKOHFTN            PIC 9(01).
           03  FLG-PTKOHFTNETC         PIC 9(01).
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
           03  FLG-ERR-02              PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
      *
      *    03  FLG-P02A-SYORI          PIC 9(01).
      *    03  FLG-DATA                PIC 9(01).
      **** 03  FLG-DELETE              PIC 9(01).
           03  FLG-HKNKOHERR           PIC 9(01).
           03  FLG-HENSYU              PIC 9(01).
           03  FLG-CANSEL              PIC 9(01).
           03  FLG-MODE                PIC 9(01).
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
      *    03  IDX-ERR                 PIC 9(04).
      *    03  IDX-WAR                 PIC 9(04).
      *
           03  IDF                     PIC 9(04).
           03  IDD                     PIC 9(04).
           03  IDD2                    PIC 9(04).
           03  IDX-TBL                 PIC 9(04).
           03  IDX3                    PIC 9(04).
           03  IDX-T                   PIC 9(04).
           03  IDX-K                   PIC 9(04).
           03  TBL-IDX                 PIC 9(04).
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
           03  WRK-RENNUM              PIC 9(03).
           03  WRK-DAYCNT              PIC 9(03).
      *    他一部負担累計
           03  WRK-NYUJGNGAK           PIC 9(07).
      *
           03  WRK-KEY-SRYYMD          PIC X(08).
           03  WRK-SRYYM               PIC X(06).
      *
           03  TBL-FTN-MAX             PIC 9(04).
           03  TBL-ETC-MAX             PIC 9(04).
      *
           03  WRK-MONEY               PIC 9(07).
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
      *    公費ＩＤ
           03  WRK-KOHID               PIC 9(10).
      *    公費開始日
           03   WRK-KOH-TEKSTYMD       PIC X(08).
      *
      *    他一部負担
           03   WRK-SEL-RENNUM         PIC 9(03).
           03   WRK-SEL-TEKSTYMD       PIC X(08).
      *
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
      *        公費ＩＤ
               05  WRK-PARA-KOHID          PIC X(10).
      *        選択番号
               05  WRK-PARA-SELNUM         PIC 9(03).
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
      *    患者公費負担
       01  TBL-PTKOHFTN-AREA.
           02  TBL-PTKOHFTN-REC        OCCURS   300.
           COPY    "CPPTKOHFTN.INC"     REPLACING
                                   //PTKOHFTN-// BY //TBL-PTKOHFTN-//.
      *    他一部有
           03  TBL-PTKOHFTN-ETCKBN      PIC X(01).
      *    削除区分
           03  TBL-PTKOHFTN-DELKBN      PIC X(01).
           03  TBL-SPA-IDX              PIC 9(03).
      *
      *    患者公費他一部負担
       01  TBL-PTKOHFTNETC-AREA.
           02  TBL-PTKOHFTNETC-REC         OCCURS   100.
           COPY    "CPPTKOHFTNETC.INC"     REPLACING
                             //PTKOHFTNETC-// BY //TBL-PTKOHFTNETC-//.
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
           COPY    "CPPATIENTMODV3REQ5.INC".
      *    API XML出力用定義体
           COPY    "CPPATIENTMODV3RES5.INC".
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
      *    患者公費負担
       01  PTKOHFTN-REC.
           COPY    "CPPTKOHFTN.INC".
      *
      *    他一部負担
       01  PTKOHFTNETC-REC.
           COPY    "CPPTKOHFTNETC.INC".
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
               MOVE   404                  TO  APILST07-HTTPSTATUS
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
           INITIALIZE                      TBL-PTKOHFTN-AREA
           INITIALIZE                      TBL-PTKOHFTNETC-AREA
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
           WHEN    "03"
               MOVE    3                   TO  FLG-SYORI
           WHEN    "04"
               MOVE    4                   TO  FLG-SYORI
           WHEN    "05"
               MOVE    5                   TO  FLG-SYORI
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
      *        公費負担確認処理
               PERFORM 2002-PTKOHFTN-SYORI-SEC
           WHEN    "03"
      *        公費負担更新処理
               PERFORM 2003-PTKOHFTN-UPDATE-SEC
           WHEN    "04"
      *        他一部負担額確認処理
               PERFORM 2004-KOHETC-SYORI-SEC
           WHEN    "05"
      *        他一部負担額更新処理
               PERFORM 2005-KOHETC-UPDATE-SEC
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
           IF      APILST07-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-PATIENTMODREQ
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_patientmodv3req5"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST07-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "patientmodv3req5"  TO  APIREQ-RECORDNAME
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
           MOVE    "xml_patientmodv3req5"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST07-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "patientmodv3req5"  TO  APIREQ-RECORDNAME
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
                   WHEN    2
                   WHEN    4
                       MOVE    "検索処理終了"
                                           TO  WRK-OKMSG
                   WHEN    3
                   WHEN    5
                       EVALUATE    FLG-MODE
                       WHEN    ZERO
                           MOVE    "処理終了"
                                           TO  WRK-OKMSG
                       WHEN    1
                           MOVE    "登録処理終了"
                                           TO  WRK-OKMSG
                       WHEN    2
                           MOVE    "削除処理終了"
                                           TO  WRK-OKMSG
                       END-EVALUATE
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
           IF      APILST07-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_patientmodv3res5"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  PTMODRES-MODE
           MOVE    "patientmodv3res5"  TO  PTMODRES-RECORDNAME
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
           MOVE    "xml_patientmodv3res5"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  PTMODRES-MODE
           MOVE    "patientmodv3res5"  TO  PTMODRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-PATIENTMODRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               MOVE    PTMODRES-OBJECT     TO  APILST07-BODY
               MOVE    "patientmod/xml"    TO  APILST07-CONTENT-TYPE
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
               IF      WRK-PARA-SYORIPG    NOT =   "05"
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
      *        公費負担対象一覧編集処理
               PERFORM 20013-PTKOH-HENSYU-SEC
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
      *    パラメタ登録
           IF      FLG-SYORIEND    =   1
               IF      PTMODRES-ORCA-UID   NOT =   SPACE
                   PERFORM 2902-APIPARA-INS-SEC
               END-IF
           ELSE
               PERFORM 2901-PARA-INS-SEC
               PERFORM 2008-SPATMP-INS-SEC
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
      **** IF     (FLG-SYORI       NOT =   1    )
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
           MOVE    "05"                TO  WRK-PARA-SYORIPG
      *    公費ＩＤ
           MOVE    WRK-KOHID           TO  WRK-PARA-KOHID
      *    選択番号
           MOVE    WRK-SEL-RENNUM      TO  WRK-PARA-SELNUM
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
      *    公費負担対象一覧編集処理
      ****************************************************************
       20013-PTKOH-HENSYU-SEC           SECTION.
      *
           INITIALIZE                  PTMODRES-PUBLIC-INSURANCE-INFG
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *    公費負担対象
           MOVE    "CLICKED"           TO  MCP-EVENT
           MOVE    "APIV3D"            TO  MCP-WIDGET
           MOVE    "PUTG"              TO  MCP-STATUS
           CALL    "ORCGP02W1"     USING   MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
      *
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL   (IDX2       >   60 )
                         OR    (IDX2       >   SPA-FTN1-MAX)
               IF      SPA-NAI24-FTNDELKBN (IDX2)  =   SPACE
                   ADD     1                   TO  IDX
                   PERFORM 200131-PTKOH-PTMODRES-SEC
               END-IF
           END-PERFORM
      *    対象なし
           IF      IDX                 =   ZERO
               MOVE   "30"             TO  WRK-ERRCD
           END-IF
           .
       20013-PTKOH-HENSYU-EXT.
           EXIT.
      *
      ****************************************************************
      *    公費編集処理
      **************************************************************
       200131-PTKOH-PTMODRES-SEC           SECTION.
      *
      *    公費ＩＤ
           MOVE    SPA-NAI24-KOHID (IDX2)  TO  PTMODRES-INSURANCE-ID
                                                                  (IDX)
      *    公費の種類
           MOVE    SPA-NAI24-KOHNUM(IDX2)  TO  PTMODRES-INSURANCE-CLASS
                                                                  (IDX)
      *    公費の種類名称
           MOVE    SPA-GMN24-TFTNKOHMEI(IDX2)
                                           TO  PTMODRES-INSURANCE-NAME
                                                                  (IDX)
      *    負担者番号
      *    MOVE    SPA-GMN24-TXXXXXXXXI(IDX2)
      *                                    TO  PTMODRES-PROVIDER-NUMBER
      *                                                           (IDX)
      *    受給者番号
           MOVE    SPA-NAI24-TJKYSNUM(IDX2)
                                           TO  PTMODRES-RECIPIENT-NUMBER
                                                                  (IDX)
      *    開始年月日
           MOVE    SPA-NAI24-TKOHSTYMD(IDX2)   TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PTMODRES-INSURANCE-START-DATE
                                                                  (IDX)
      *    終了年月日
           MOVE    SPA-NAI24-TKOHEDYMD(IDX2)   TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PTMODRES-INSURANCE-END-DATE
                                                                  (IDX)
      *
           .
       200131-PTKOH-PTMODRES-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担確認処理
      *****************************************************************
       2002-PTKOHFTN-SYORI-SEC     SECTION.
      *
      *    患者番号判定
           PERFORM 20011-PTINF-INITCHK-SEC
      *
           IF      WRK-ERRCD           =   SPACE
      *        ＰＡＲＡのＳＰＡ編集処理
               PERFORM 2031-SPA-PARA-SEC
           END-IF
      *    項目チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20021-PTKOHFTN-CHK-SEC
           END-IF
      *    公費負担一覧編集処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20022-PTKOHFTN-HENSYU-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
               MOVE    "03"            TO  PTMODRES-RESPONSE-NUMBER
           ELSE
               IF      PTMODRES-RESPONSE-NUMBER    =   SPACE
                   MOVE    "02"            TO  PTMODRES-RESPONSE-NUMBER
               END-IF
           END-IF
      *    パラメタ登録
           PERFORM 2901-PARA-INS-SEC
      *    SPAの保存
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2008-SPATMP-INS-SEC
           END-IF
      *
           .
       2002-PTKOHFTN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費ＩＤ選択チェック処理
      *****************************************************************
       200211-KOHID-SEL-SEC     SECTION.
      *
      *    レスポンス編集
           MOVE    PTMODREQ-INSURANCE-ID
                                       TO  PTMODRES-SEL-INSURANCE-ID
           MOVE    PTMODREQ-INSURANCE-CLASS
                                       TO  PTMODRES-SEL-INSURANCE-CLASS
           MOVE    PTMODREQ-RECIPIENT-NUMBER
                                       TO  PTMODRES-SEL-RECIPIENT-NUMBER
           MOVE    PTMODREQ-INSURANCE-START-DATE
                                       TO  PTMODRES-SEL-INSURANCE-START
      *
           MOVE    ZERO                TO  WRK-KOHID
           MOVE    SPACE               TO  WRK-KOH-TEKSTYMD
      *    公費ＩＤチェック
           INITIALIZE                      ORCSNUMAREA
           MOVE    PTMODREQ-INSURANCE-ID   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF      SNUM-RC         NOT =   ZERO
               MOVE    "31"                TO  WRK-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-KOHID
               MOVE    WRK-KOHID           TO  PTMODRES-SEL-INSURANCE-ID
           END-IF
      *
           IF      PTMODREQ-INSURANCE-START-DATE   NOT =   SPACE
               MOVE    PTMODREQ-INSURANCE-START-DATE
                                           TO  WRK-HEN-DATE
               MOVE    SPACE               TO  WRK-HEN-TIME
               PERFORM 802-DAYHEN02-SEC
               MOVE    WRK-SYMD            TO  WRK-KOH-TEKSTYMD
           END-IF
           .
       200211-KOHID-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目チェック処理
      *****************************************************************
       20021-PTKOHFTN-CHK-SEC     SECTION.
      *
      *    公費ＩＤ選択チェック
           PERFORM 200211-KOHID-SEL-SEC
      *
           MOVE    ZERO                TO  SPA-GMN24-FTNSELNUM
           MOVE    ZERO                TO  SPA-NAI24-FTNSELNUM
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL   (IDX        >   60 )
                         OR    (IDX        >   SPA-FTN1-MAX)
                         OR    (WRK-ERRCD  NOT =   SPACE  )
                         OR    (FLG-OK     =   1   )
               IF     (WRK-KOHID           =   SPA-NAI24-KOHID (IDX))
                 AND  (SPA-NAI24-FTNDELKBN (IDX)  =   SPACE )
                   PERFORM 2001211-PTKOHTBL-CHK-SEC
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               IF      WRK-ERRCD           =   SPACE
                   MOVE    "32"                TO  WRK-ERRCD
               END-IF
           END-IF
           .
       20021-PTKOHFTN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費チェック処理
      *****************************************************************
       2001211-PTKOHTBL-CHK-SEC     SECTION.
      *
           IF    ((PTMODREQ-INSURANCE-CLASS    NOT =   SPACE)
              AND (PTMODREQ-INSURANCE-CLASS
                                       NOT =   SPA-NAI24-KOHNUM(IDX)))
           OR    ((PTMODREQ-RECIPIENT-NUMBER   NOT =   SPACE )
              AND (PTMODREQ-RECIPIENT-NUMBER
                                       NOT =   SPA-NAI24-TJKYSNUM(IDX)))
           OR    ((WRK-KOH-TEKSTYMD    NOT =   SPACE )
              AND (WRK-KOH-TEKSTYMD
                                       NOT =   SPA-NAI24-TKOHSTYMD
                                                                (IDX)))
               MOVE    "32"                TO  WRK-ERRCD
           ELSE
      *        公費選択
               MOVE    IDX             TO  SPA-GMN24-FTNSELNUM
               MOVE    IDX             TO  SPA-NAI24-FTNSELNUM
      *        公費負担額編集
               MOVE    FLG-SYORI           TO  FLG-HENSYU
               PERFORM 2001212-PTKOHFTN-SELHENSYU-SEC
           END-IF
           .
       2001211-PTKOHTBL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象の公費編集処理
      *****************************************************************
       2001212-PTKOHFTN-SELHENSYU-SEC     SECTION.
      *
           MOVE    SPA-NAI24-FTNSELNUM TO  IDF
      *
           INITIALIZE                  PTMODRES-KOHFTN-INFO-G
      *    公費ＩＤ
           MOVE    SPA-NAI24-KOHID(IDF)
                                       TO  PTMODRES-SEL-INSURANCE-ID
           MOVE    SPA-NAI24-KOHNUM(IDF)
                                       TO  PTMODRES-SEL-INSURANCE-CLASS
           MOVE    SPA-GMN24-TFTNKOHMEI(IDF)
                                       TO  PTMODRES-SEL-INSURANCE-NAME
           MOVE    SPA-NAI24-TJKYSNUM(IDF)
                                       TO  PTMODRES-SEL-RECIPIENT-NUMBER
      *    開始年月日
           MOVE    SPA-NAI24-TKOHSTYMD(IDF)    TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PTMODRES-SEL-INSURANCE-START
      *    終了年月日
           MOVE    SPA-NAI24-TKOHEDYMD(IDF)    TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PTMODRES-SEL-INSURANCE-END
      *
      *    公費選択時のみ
           IF      FLG-HENSYU           =   2
      *        対象の公費負担のみ検索
               PERFORM 2001212-PTKOHFTN-PARASET-SEC
      *        公費負担リスト編集
               MOVE    "02"                TO  SPA-MCP-WIDGET
           ELSE
      *        見出しのみ編集
               MOVE    "01"                TO  SPA-MCP-WIDGET
           END-IF
           PERFORM 2001211-P024-SYORI-SEC
      *    見出編集
           MOVE    SPA-GMN24-FTNMSG1   TO  PTMODRES-SEL-KOHFTN-NAME1
           MOVE    SPA-GMN24-FTNMSG2   TO  PTMODRES-SEL-KOHFTN-NAME2
      *    公費種類
           MOVE    SPA-NAI24-FTNSYORI  TO  PTMODRES-SEL-KOHFTN-CLASS
           .
       2001212-PTKOHFTN-SELHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象の公費負担のみ編集処理
      *****************************************************************
       2001212-PTKOHFTN-PARASET-SEC     SECTION.
      *
      *    PARA削除
      *    パラメタ削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTKOHFTN"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           INITIALIZE                      PTKOHFTN-REC
           MOVE    SPA-HOSPNUM         TO  PTKOHFTN-HOSPNUM
           MOVE    SPA-PTID            TO  PTKOHFTN-PTID
           MOVE    WRK-KOHID           TO  PTKOHFTN-KOHID
           MOVE    PTKOHFTN-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-PTKOHFTN-READ-SEC
           ELSE 
               MOVE    1               TO  FLG-PTKOHFTN
           END-IF
      *
           MOVE    ZERO                TO  IDH
           PERFORM
               UNTIL    FLG-PTKOHFTN       =   1
               ADD     1                   TO  IDH
               MOVE    SPACE               TO  PARA-REC
               INITIALIZE                      PARA-REC
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    "P02"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "PTKOHFTN"          TO  PARA-FILEMEI
               MOVE    IDH                 TO  PARA-EDANUM
      *        他一部負担金判定の為、クリア
               MOVE    SPACE               TO  PTKOHFTN-TERMID
               MOVE    PTKOHFTN-REC        TO  PARA-DATA-REC
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
      *
               MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-PTKOHFTN-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2001212-PTKOHFTN-PARASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    負担額見出し編集処理
      *****************************************************************
       2001211-P024-SYORI-SEC     SECTION.
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "CLICKED"           TO  MCP-EVENT
           MOVE    "APIV3"             TO  MCP-WIDGET
           MOVE    "PUTG"              TO  MCP-STATUS
           CALL    "ORCGP024"      USING   MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
      *
           .
       2001211-P024-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担一覧編集処理
      *****************************************************************
       20022-PTKOHFTN-HENSYU-SEC     SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-FTN2-MAX
      **       MOVE    SPA-NAI24-TRENNUM  (IDX)
      **                               TO  PTMODRES-PTKOHFTN-RENNUM
      **                                                       (IDX)
               MOVE    IDX             TO  WRK-RENNUM
               MOVE    WRK-RENNUM      TO  PTMODRES-PTKOHFTN-RENNUM
                                                               (IDX)
      *        開始日
               MOVE    SPA-NAI24-TFTNSTYMD(IDX)    TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE    TO  PTMODRES-PTKOHFTN-TEKSTYMD
                                                               (IDX)
      *        終了日
               MOVE    SPA-NAI24-TFTNEDYMD(IDX)    TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE    TO  PTMODRES-PTKOHFTN-TEKEDYMD
                                                               (IDX)
      *        外来上限額
               MOVE    SPA-GMN24-TGAIJGNGAK (IDX)
                                       TO  PTMODRES-PTKOHFTN-GAIJGNGAK
                                                               (IDX)
               MOVE    SPA-GMN24-TNYUJGNGAK (IDX)
                                       TO  PTMODRES-PTKOHFTN-NYUJGNGAK
                                                               (IDX)
      *
      *        公費他一部負担額設定有
               IF      SPA-NAI24-TFTNETCKBN (IDX)  =   "1"
                   MOVE    "1"         TO  PTMODRES-PTKOHFTN-ETC-KBN
                                                               (IDX)
               END-IF
           END-PERFORM
      *
           .
       20022-PTKOHFTN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担更新処理
      *****************************************************************
       2003-PTKOHFTN-UPDATE-SEC     SECTION.
      *
      *    患者番号判定
           PERFORM 20011-PTINF-INITCHK-SEC
      *
           IF      WRK-RESPONSE-NUMBER NOT =   "03"
               MOVE    "19"                TO  WRK-ERRCD
               MOVE    WRK-RESPONSE-NUMBER
                                           TO  PTMODRES-RESPONSE-NUMBER
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        ＰＡＲＡのＳＰＡ編集処理
               PERFORM 2031-SPA-PARA-SEC
           END-IF
      *    項目チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20031-PTKOHFTN-UPD-CHK-SEC
           END-IF
      *    更新処理
           IF     (WRK-ERRCD           =   SPACE )
              AND (FLG-MODE        NOT =   ZERO  )
               PERFORM 20032-PTKOHFTN-UPDATE-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        継続
               IF      FLG-CONTKBN         =   1
      *            パラメタ削除
                   PERFORM 2902-PARA-ALLDEL-SEC
                   PERFORM 2009-SPATMP-DEL-SEC
      *            公費選択へ
                   MOVE    "02"            TO  PTMODRES-RESPONSE-NUMBER
      *            継続区分終了
                   MOVE    "1"             TO  WRK-ENDKBN
      *            APIパラメタ登録
                   PERFORM 2901-PARA-INS-SEC
                   PERFORM 2008-SPATMP-INS-SEC
               ELSE
                   MOVE    "00"            TO  PTMODRES-RESPONSE-NUMBER
      *            パラメタ削除・排他解除処理
                   PERFORM 2900-SYORI-END-SEC
               END-IF
           ELSE
               IF      PTMODRES-RESPONSE-NUMBER    =   SPACE
                   MOVE    "03"            TO  PTMODRES-RESPONSE-NUMBER
               END-IF
               IF      FLG-CANSEL      =   ZERO
      *            SPA再編集
                   PERFORM 2901-PARA-INS-SEC
      **********   PERFORM 2008-SPATMP-INS-SEC
               END-IF
           END-IF
      *
           .
       2003-PTKOHFTN-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新チェック処理
      *****************************************************************
       20031-PTKOHFTN-UPD-CHK-SEC         SECTION.
      *
      *    公費ＩＤ選択チェック
           PERFORM 20031-KOHIDPARA-CHK-SEC
      *
           IF      WRK-ERRCD           =   SPACE
      *        公費負担額更新チェック
               EVALUATE    PTMODREQ-PTKOHFTN-MODE
               WHEN    "DELETE"
               WHEN    "Delete"
                   MOVE    2               TO  FLG-MODE
               WHEN    "MODIFY"
               WHEN    "Modify"
                   MOVE    1               TO  FLG-MODE
                   PERFORM 200311-PTKOHFTN-CHK-SEC
               WHEN    SPACE
                   MOVE    ZERO            TO  FLG-MODE
               WHEN    OTHER
                   MOVE    "30"            TO  WRK-ERRCD
               END-EVALUATE
           END-IF
      *
           .
       20031-PTKOHFTN-UPD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＡＲＡチェック処理
      *****************************************************************
       20031-KOHIDPARA-CHK-SEC         SECTION.
      *
      *    公費ＩＤ選択チェック
           PERFORM 200211-KOHID-SEL-SEC
      *
           IF     (WRK-KOHID       NOT =   WRK-PARA-KOHID)
               MOVE    "33"                TO  WRK-ERRCD
           END-IF
           IF      SPA-NAI24-FTNSELNUM NOT =   ZERO
               IF      SPA-NAI24-KOHID (SPA-NAI24-FTNSELNUM)
                                       NOT =   WRK-KOHID
                   MOVE    "33"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    "02"            TO  PTMODRES-RESPONSE-NUMBER
           END-IF
           IF     (SPA-NAI24-FTNSELNUM NOT =   ZERO ) 
              AND (WRK-ERRCD           =   SPACE )
      *        公費負担額編集
               MOVE    FLG-SYORI           TO  FLG-HENSYU
               PERFORM 2001212-PTKOHFTN-SELHENSYU-SEC
           END-IF
           .
       220031-PARA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新チェック処理
      *****************************************************************
       200311-PTKOHFTN-CHK-SEC         SECTION.
      *
           INITIALIZE                  TBL-PTKOHFTN-AREA
           MOVE    ZERO                TO  FLG-ERR-01
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   300 )
               MOVE    PTMODREQ-PTKOHFTN-LMODE  (IDX)
                                       TO  PTMODRES-PTKOHFTN-LMODE
                                                               (IDX)
               MOVE    PTMODREQ-PTKOHFTN-RENNUM  (IDX)
                                       TO  PTMODRES-PTKOHFTN-RENNUM
                                                               (IDX)
               MOVE    PTMODREQ-PTKOHFTN-TEKSTYMD (IDX)
                                       TO  PTMODRES-PTKOHFTN-TEKSTYMD
                                                               (IDX)
               MOVE    PTMODREQ-PTKOHFTN-TEKEDYMD (IDX)
                                       TO  PTMODRES-PTKOHFTN-TEKEDYMD
                                                               (IDX)
               MOVE    PTMODREQ-PTKOHFTN-GAIJGNGAK (IDX)
                                       TO  PTMODRES-PTKOHFTN-GAIJGNGAK
                                                               (IDX)
               MOVE    PTMODREQ-PTKOHFTN-NYUJGNGAK (IDX)
                                       TO  PTMODRES-PTKOHFTN-NYUJGNGAK
                                                               (IDX)
      *
               MOVE    SPACE               TO  WRK-ERRCD
               MOVE    SPACE               TO  SPA-ERRCD
               EVALUATE    PTMODREQ-PTKOHFTN-LMODE (IDX)
                   WHEN    "Modify"
                   WHEN    "New"
                   WHEN    SPACE
                       CONTINUE
                   WHEN    "Delete"
                       MOVE    "1"             TO  TBL-PTKOHFTN-DELKBN
                                                               (IDX)
                   WHEN    OTHER
                       MOVE    "AP34"          TO  SPA-ERRCD
               END-EVALUATE
      *
      *        連番　数値チェック
               MOVE    ZERO                TO  WRK-RENNUM
               INITIALIZE                      ORCSNUMAREA
               MOVE    PTMODREQ-PTKOHFTN-RENNUM  (IDX)
                                           TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF      SNUM-RC             NOT =   ZERO
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "AP35"                TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-RENNUM
               END-IF
               MOVE    WRK-RENNUM      TO  TBL-PTKOHFTN-RENNUM(IDX)
      *        認定開始年月日
               IF      PTMODREQ-PTKOHFTN-TEKSTYMD(IDX)  =   SPACE
                   CONTINUE
               ELSE
                   MOVE    PTMODREQ-PTKOHFTN-TEKSTYMD(IDX)
                                           TO  WRK-HEN-DATE
                   MOVE    SPACE           TO  WRK-HEN-TIME
                   PERFORM 802-DAYHEN02-SEC
                   MOVE    WRK-SYMD        TO  TBL-PTKOHFTN-TEKSTYMD
                                                         (IDX)
      *            暦日チェック
                   PERFORM 803-ORCSDAY-CHEK-SEC
                   IF      FLG-DAYERR          =   1
                       IF     (SPA-ERRCD           =   SPACE )
                           MOVE    "2432"              TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
      *        終了日
               IF      PTMODREQ-PTKOHFTN-TEKEDYMD(IDX)  =   SPACE
                   CONTINUE
               ELSE
                   MOVE    PTMODREQ-PTKOHFTN-TEKEDYMD(IDX)
                                           TO  WRK-HEN-DATE
                   MOVE    SPACE           TO  WRK-HEN-TIME
                   PERFORM 802-DAYHEN02-SEC
                   MOVE    WRK-SYMD        TO  TBL-PTKOHFTN-TEKEDYMD
                                                         (IDX)
      *            暦日チェック
                   PERFORM 803-ORCSDAY-CHEK-SEC
                   IF      FLG-DAYERR          =   1
                       IF     (SPA-ERRCD           =   SPACE )
                         AND  (WRK-ERRCD           =   SPACE )
                           MOVE    "2433"              TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
      *        外来上限額
               INITIALIZE                      ORCSNUMAREA
               MOVE    PTMODREQ-PTKOHFTN-GAIJGNGAK  (IDX)
                                           TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
      *            整数であること
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "AP36"              TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    SNUM-OUTNUM     TO  TBL-PTKOHFTN-GAIJGNGAK
                                                              (IDX)
               END-IF
      *        入院上限額
               INITIALIZE                      ORCSNUMAREA
               MOVE    PTMODREQ-PTKOHFTN-NYUJGNGAK  (IDX)
                                           TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
      *            整数であること
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "AP37"                TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    SNUM-OUTNUM     TO  TBL-PTKOHFTN-NYUJGNGAK
                                                              (IDX)
               END-IF
      *        必須チェック
               IF     (TBL-PTKOHFTN-RENNUM(IDX)    NOT =   ZERO )
                   IF      (PTMODREQ-PTKOHFTN-TEKSTYMD(IDX)  =   SPACE)
                       IF      SPA-ERRCD           =   SPACE
                           MOVE    "AP38"                TO  SPA-ERRCD
                       END-IF
                   END-IF
               ELSE
                   IF      (PTMODREQ-PTKOHFTN-LMODE (IDX)  =   "New"  )
                       AND (PTMODREQ-PTKOHFTN-TEKSTYMD(IDX)  =   SPACE)
                       IF      SPA-ERRCD           =   SPACE
                           MOVE    "AP38"              TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
      *
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 2003110-ERRCDMSG-HEN-SEC
                   MOVE    1                   TO  FLG-ERR-01
               END-IF
           END-PERFORM
      *
      *    関連チェック
           MOVE    ZERO                TO  FLG-ERR-02
           MOVE    ZERO                TO  TBL-FTN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   300 )
                       OR     (FLG-ERR-01  =   1    )
                       OR    ((TBL-PTKOHFTN-RENNUM(IDX)    =   ZERO )
                          AND (TBL-PTKOHFTN-TEKSTYMD(IDX)  =   SPACE))
               ADD     1                   TO  TBL-FTN-MAX
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    SPACE               TO  WRK-ERRCD
      *        連番チェック
               IF      TBL-PTKOHFTN-RENNUM(IDX)    NOT =   ZERO
                   PERFORM 2003111-RENNUM-CHK-SEC
               END-IF
      *        開始日チェック
               IF     (TBL-PTKOHFTN-TEKSTYMD(IDX)  NOT =   SPACE )
                 AND  (WRK-ERRCD           =   SPACE  )
      *            終了日未設定
                   IF      TBL-PTKOHFTN-TEKEDYMD (IDX) =   SPACE
                       MOVE    "99999999"      TO  TBL-PTKOHFTN-TEKEDYMD
                                                                 (IDX)
                       MOVE    TBL-PTKOHFTN-TEKEDYMD (IDX)
                                               TO  WRK-SYMD
                       MOVE    ZERO                TO  WRK-THMS
                       PERFORM 801-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE    TO
                                           PTMODRES-PTKOHFTN-TEKEDYMD
                                                                (IDX)
                   END-IF
      *            項目チェック
                   IF     TBL-PTKOHFTN-DELKBN (IDX)    =   "1"
      *                削除
                       IF    (TBL-PTKOHFTN-TEKSTYMD(IDX)
                                           NOT =   SPA-NAI24-TFTNSTYMD
                                                          (IDX-TBL))
                        AND  (TBL-PTKOHFTN-ETCKBN (IDX)
                                                =   "1"    )
                        AND  (SPA-ERRCD         =   SPACE  )
      *                    開始日変更はエラー
                           MOVE    "AP39"            TO  SPA-ERRCD
                       END-IF
                   ELSE
                       PERFORM 2003112-PTKOHFTN-CHK-SEC
                   END-IF
               END-IF
      *
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 2003110-ERRCDMSG-HEN-SEC
                   MOVE    1               TO  FLG-ERR-02
               END-IF
      *
           END-PERFORM
      *
           IF      (FLG-ERR-02         =   1   )
               OR  (FLG-ERR-01         =   1   )
               MOVE    "35"                TO  WRK-ERRCD
           END-IF
      *
      *    ＳＰＡの連番がすべて返却されていること
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   SPA-FTN2-MAX)
                          OR  (WRK-ERRCD   NOT =   SPACE )
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX         >   TBL-FTN-MAX )
                               OR (FLG-CHK     =   1     )
                   IF      IDX2            =   TBL-PTKOHFTN-RENNUM(IDX)
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      FLG-CHK         =   ZERO
                   MOVE    "34"            TO  WRK-ERRCD
               END-IF
           END-PERFORM
           .
       200311-PTKOHFTN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    連番チェック処理
      *****************************************************************
       2003111-RENNUM-CHK-SEC         SECTION.
      *
      *    同じ番号がないこと
           MOVE    SPACE               TO  SPA-ERRCD
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   TBL-FTN-MAX )
                          OR  (SPA-ERRCD   NOT =   SPACE )
               IF     (IDX         NOT =   IDX2      )
                  AND (TBL-PTKOHFTN-RENNUM(IDX)
                                       =   TBL-PTKOHFTN-RENNUM(IDX2))
                   MOVE    "AP40"              TO  SPA-ERRCD
               END-IF
           END-PERFORM
      *    ＳＰＡに存在していること
           MOVE    ZERO                TO  FLG-CHK
           MOVE    ZERO                TO  TBL-IDX
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   SPA-FTN2-MAX)
                          OR  (FLG-CHK     =   1     )
                          OR  (SPA-ERRCD   NOT =   SPACE )
               IF      IDX2            =   TBL-PTKOHFTN-RENNUM(IDX)
      *            他一部有
                   IF      SPA-NAI24-TFTNETCKBN (IDX2)  =   "1"
                       MOVE    "1"         TO  TBL-PTKOHFTN-ETCKBN
                                                             (IDX)
                   END-IF
                   MOVE    IDX2            TO  TBL-SPA-IDX (IDX)
                   MOVE    IDX2            TO  TBL-IDX
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
               IF      PTMODREQ-PTKOHFTN-LMODE (IDX)  =   "New"
                   IF      FLG-CHK         NOT =   ZERO
                       MOVE    "AP41"              TO  SPA-ERRCD
                   END-IF
               ELSE
                   IF      FLG-CHK             =   ZERO
                       MOVE    "AP42"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
           .
       2003111-RENNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担項目エラーメッセージ処理
      *****************************************************************
       2003110-ERRCDMSG-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           IF      SPA-ERRCD(1:1)      =   "A"
               PERFORM 8901-PTERRCD-SETAPI-SEC
           ELSE
               PERFORM 8901-PTERRCD-SET-SEC
           END-IF
           MOVE    SPA-ERRCD           TO  PTMODRES-PTKOHFTN-ERRCD
                                                                (IDX)
           MOVE    WRK-ERRMSG          TO  PTMODRES-PTKOHFTN-ERRCD-MSG
                                                                (IDX)
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  WRK-ERRMSG
           MOVE    SPACE               TO  WRK-ERRCD
           .
       2003110-ERRCDMSG-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担項目チェック処理
      *****************************************************************
       2003112-PTKOHFTN-CHK-SEC         SECTION.
      *
      *    選択行
           IF      TBL-IDX             =   ZERO
               MOVE    ZERO            TO  SPA-GMN24-FTNSELNUM2
               MOVE    ZERO            TO  SPA-NAI24-FTNSELNUM2
           ELSE
               MOVE    TBL-IDX         TO  SPA-GMN24-FTNSELNUM2
               MOVE    TBL-IDX         TO  SPA-NAI24-FTNSELNUM2
           END-IF
      *    対象のデータを編集する
           INITIALIZE                  SPA-GMN24-FTN-AREA2
           MOVE    TBL-PTKOHFTN-TEKSTYMD(IDX)  TO  SPA-NAI24-FTNSTYMD
           MOVE    TBL-PTKOHFTN-TEKEDYMD(IDX)  TO  SPA-NAI24-FTNEDYMD
           MOVE    TBL-PTKOHFTN-GAIJGNGAK(IDX) TO  SPA-NAI24-GAIJGNGAK
           MOVE    TBL-PTKOHFTN-NYUJGNGAK(IDX) TO  SPA-NAI24-NYUJGNGAK
           MOVE    TBL-PTKOHFTN-ETCKBN  (IDX)  TO  SPA-NAI24-FTNETCKBN
      *
           MOVE    TBL-PTKOHFTN-TEKSTYMD(IDX)  TO  SPA-GMN24-FTNSTYMD
           MOVE    TBL-PTKOHFTN-TEKEDYMD(IDX)  TO  SPA-GMN24-FTNEDYMD
           MOVE    SPA-NAI24-GAIJGNGAK         TO  SPA-GMN24-GAIJGNGAK
           MOVE    SPA-NAI24-NYUJGNGAK         TO  SPA-GMN24-NYUJGNGAK
      *
      *    リクエスト番号
           MOVE    "03"                TO  SPA-MCP-WIDGET
           PERFORM 2001211-P024-SYORI-SEC
      *H30.11
      *    肝がん治療対応
           MOVE    SPA-NAI24-GAIJGNGAK TO  TBL-PTKOHFTN-GAIJGNGAK(IDX)
           MOVE    SPA-NAI24-NYUJGNGAK TO  TBL-PTKOHFTN-NYUJGNGAK(IDX)
      *
      *    重複等チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2003112-KOHFTN-CHK-SEC
           END-IF
      *
           .
       2003112-PTKOHFTN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担情報更新処理
      *****************************************************************
       2003112-KOHFTN-CHK-SEC         SECTION.
      *
      *    期間が重複しないこと
           PERFORM VARYING     IDX3    FROM    1  BY  1
                   UNTIL      (IDX3    >   TBL-FTN-MAX   )
                         OR   (SPA-ERRCD   NOT =   SPACE )
               IF     (IDX                 NOT =   IDX3  )
                 AND  (TBL-PTKOHFTN-DELKBN (IDX3)  =   SPACE )
                   IF     (TBL-PTKOHFTN-TEKEDYMD(IDX3)
                                           <   SPA-NAI24-FTNSTYMD )
                     OR   (TBL-PTKOHFTN-TEKSTYMD(IDX3)
                                           >   SPA-NAI24-FTNEDYMD )
                       CONTINUE
                   ELSE
                       MOVE    "2434"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
      *    期間変更時の他一部負担履歴チェック
           IF      TBL-IDX             =   ZERO
      *        新規の時、同一開始年月で他一部履歴がある時
               PERFORM VARYING     IDX3    FROM    1  BY  1
                       UNTIL      (IDX3    >   TBL-FTN-MAX   )
                             OR   (SPA-ERRCD   NOT =   SPACE )
                             OR   (WRK-ERRCD   NOT =   SPACE )
                   IF     (IDX                 NOT =   IDX3  )
                     AND  (TBL-PTKOHFTN-DELKBN (IDX3)  =   SPACE )
                       IF     (TBL-PTKOHFTN-TEKSTYMD(IDX3)(1:6)
                                           =   SPA-NAI24-FTNSTYMD(1:6))
                          AND (TBL-PTKOHFTN-ETCKBN (IDX3)
                                           =   "1"               )
                           IF    TBL-PTKOHFTN-NYUJGNGAK(IDX)
                                               >   ZERO
                               MOVE    "AP45"            TO  SPA-ERRCD
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    他一部負担履歴あり
           IF     (TBL-PTKOHFTN-ETCKBN (IDX)   =   "1" )
             AND  (WRK-ERRCD           =   SPACE )
             AND  (SPA-ERRCD           =   SPACE )
      *        開始年月変更、他負担額累計変更不可
               IF     (SPA-NAI24-FTNSTYMD(1:6)
                                       NOT =   SPA-NAI24-TFTNSTYMD
                                                   (TBL-IDX) (1:6))
                   MOVE    "2447"          TO  SPA-ERRCD
               ELSE
                   IF    (SPA-NAI24-NYUJGNGAK
                                       NOT =   SPA-GMN24-TNYUJGNGAK
                                                      (TBL-IDX))
                       MOVE    "AP45"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
           .
       2003112-KOHFTN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担情報更新処理
      *****************************************************************
       20032-PTKOHFTN-UPDATE-SEC         SECTION.
      *
      *    公費負担額
           EVALUATE    PTMODREQ-PTKOHFTN-MODE
               WHEN    "DELETE"
               WHEN    "Delete"
                   PERFORM 200321-PTKOHFTN-DELETE-SEC
      *            他一部負担一括削除
                   IF      WRK-ERRCD       =   SPACE
                       PERFORM 200321-PTKOHETC-ALLDEL-SEC
                   END-IF
               WHEN    "MODIFY"
               WHEN    "Modify"
                   PERFORM 200322-PTKOHFTN-UPDATE-SEC
           END-EVALUATE
      *
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      20032-PTKOHFTN-UPDATE-EXT
           END-IF
      *
      *    更新後のテーブルから再度表示（連番を変更する為）
      *    公費負担更新編集処理
           PERFORM 200322-PTKOHFTN-SAIHENSYU-SEC
      *
           .
       20032-PTKOHFTN-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担情報削除処理
      *****************************************************************
       200321-PTKOHFTN-DELETE-SEC      SECTION.
      *
      *    公費ＩＤで削除
           INITIALIZE                      PTKOHFTN-REC
           MOVE    SPA-HOSPNUM         TO  PTKOHFTN-HOSPNUM
           MOVE    SPA-PTID            TO  PTKOHFTN-PTID
           MOVE    WRK-KOHID           TO  PTKOHFTN-KOHID
      *
           MOVE    PTKOHFTN-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       200321-PTKOHFTN-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担情報削除処理
      *****************************************************************
       200321-PTKOHETC-ALLDEL-SEC      SECTION.
      *
      *    公費ＩＤで削除
           INITIALIZE                      PTKOHFTNETC-REC
           MOVE    SPA-HOSPNUM         TO  PTKOHFTNETC-HOSPNUM
           MOVE    SPA-PTID            TO  PTKOHFTNETC-PTID
           MOVE    WRK-KOHID           TO  PTKOHFTNETC-KOHID
      *
           MOVE    PTKOHFTNETC-REC     TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptkohftnetc"   TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE   "61"             TO  WRK-ERRCD
           END-IF
           .
       200321-PTKOHETC-ALLDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担情報更新処理
      *****************************************************************
       200322-PTKOHFTN-UPDATE-SEC         SECTION.
      *
      *    公費負担情報削除処理
           PERFORM 200321-PTKOHFTN-DELETE-SEC
      *    削除分の他一部負担額累計変更
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   TBL-FTN-MAX )
                           OR (FLG-CHK     =   1    )
               IF     (TBL-PTKOHFTN-DELKBN(IDX)    =   "1" )
                 AND  (TBL-PTKOHFTN-ETCKBN(IDX)    =   "1" )
                 AND  (TBL-PTKOHFTN-NYUJGNGAK(IDX) >   ZERO)
                   PERFORM VARYING     IDZ     FROM    1     BY  1
                               UNTIL  (IDZ         >   TBL-FTN-MAX )
                                   OR (FLG-CHK     =   1    )
                       IF     (IDX             NOT =   IDZ)
                          AND (TBL-PTKOHFTN-DELKBN(IDZ)  =   SPACE)
                          AND (TBL-PTKOHFTN-TEKSTYMD(IDX)(1:6)
                                               =   TBL-PTKOHFTN-TEKSTYMD
                                                             (IDZ)(1:6))
                          AND (TBL-PTKOHFTN-ETCKBN(IDZ)
                                               =   "1"        )
                           MOVE    TBL-PTKOHFTN-NYUJGNGAK(IDX)
                                           TO  TBL-PTKOHFTN-NYUJGNGAK
                                                             (IDZ)
                           MOVE    1       TO  FLG-CHK
                        END-IF
                   END-PERFORM
      *            他一部負担削除なし
                   IF      FLG-CHK         =   1
                       MOVE    "2"         TO  TBL-PTKOHFTN-DELKBN(IDX)
                   END-IF
               END-IF
           END-PERFORM
      *
      *    追加処理
           MOVE    ZERO                TO  WRK-RENNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   300   )
                        OR    (TBL-PTKOHFTN-TEKSTYMD (IDX)
                                           =   SPACE )
                        OR    (WRK-ERRCD   NOT =   SPACE )
               IF      TBL-PTKOHFTN-DELKBN(IDX)   =   SPACE
      *
                   PERFORM 2003221-PTKOHFTN-INS-SEC
               ELSE
      *            他一部負担削除
                   IF     (TBL-PTKOHFTN-DELKBN(IDX)    =   "1")
                      AND (TBL-PTKOHFTN-ETCKBN(IDX)    =   "1" )
                       MOVE    TBL-PTKOHFTN-TEKSTYMD (IDX)(1:6)
                                                   TO  WRK-SRYYM
                       PERFORM 2003221-PTKOHETC-DEL-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       200322-PTKOHFTN-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担情報追加処理
      *****************************************************************
       2003221-PTKOHFTN-INS-SEC         SECTION.
      *
           MOVE    TBL-PTKOHFTN-REC (IDX)  TO  PTKOHFTN-REC
      *    1から連番
           ADD     1                   TO  WRK-RENNUM
           MOVE    WRK-RENNUM          TO  PTKOHFTN-RENNUM
      *
           MOVE    SPA-HOSPNUM         TO  PTKOHFTN-HOSPNUM
           MOVE    SPA-PTID            TO  PTKOHFTN-PTID
           MOVE    WRK-KOHID           TO  PTKOHFTN-KOHID
      *
           MOVE    SPA-NAI24-KOHNUM  (SPA-NAI24-FTNSELNUM)
                                       TO  PTKOHFTN-KOHNUM
           MOVE    SPA-NAI24-PAYKBN  (SPA-NAI24-FTNSELNUM)
                                       TO  PTKOHFTN-PAYKBN
      *
           MOVE    SMCNDATE-YMD        TO  PTKOHFTN-CREYMD
           MOVE    SMCNDATE-YMD        TO  PTKOHFTN-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTKOHFTN-UPHMS
      *
           MOVE    WRK-OPID            TO  PTKOHFTN-OPID
      *
           MOVE    PTKOHFTN-REC        TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "62"                TO  WRK-ERRCD
           END-IF
      *
           .
       2003221-PTKOHFTN-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担情報削除処理
      *****************************************************************
       2003221-PTKOHETC-DEL-SEC      SECTION.
      *
      *    公費ＩＤの開始年月で削除
           INITIALIZE                      PTKOHFTNETC-REC
           MOVE    SPA-HOSPNUM         TO  PTKOHFTNETC-HOSPNUM
           MOVE    SPA-PTID            TO  PTKOHFTNETC-PTID
           MOVE    WRK-KOHID           TO  PTKOHFTNETC-KOHID
           MOVE    WRK-SRYYM           TO  PTKOHFTNETC-SRYYMD(1:6)
           MOVE    "%"                 TO  PTKOHFTNETC-SRYYMD(7:1)
      *
           MOVE    PTKOHFTNETC-REC     TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptkohftnetc"   TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE   "63"             TO  WRK-ERRCD
           END-IF
           .
       2003221-PTKOHETC-DEL-EXT.
           EXIT.
      *
      ****************************************************************
      *    公費負担更新編集処理
      ****************************************************************
       200322-PTKOHFTN-SAIHENSYU-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN24-KOHFTNTBL2
           MOVE    ZERO                TO  SPA-GMN24-FTNSELNUM
           MOVE    ZERO                TO  SPA-NAI24-FTNSELNUM
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   60 )
                     OR    (IDX        >   SPA-FTN1-MAX)
                     OR    (FLG-OK     =   1   )
               IF     (WRK-KOHID           =   SPA-NAI24-KOHID (IDX))
                 AND  (SPA-NAI24-FTNDELKBN (IDX)  =   SPACE )
      *            公費選択
                   MOVE    IDX             TO  SPA-GMN24-FTNSELNUM
                   MOVE    IDX             TO  SPA-NAI24-FTNSELNUM
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      SPA-NAI24-FTNSELNUM NOT =   ZERO
      *        公費負担額編集
               MOVE    2                   TO  FLG-HENSYU
               PERFORM 2001212-PTKOHFTN-SELHENSYU-SEC
           END-IF
      *    公費負担一覧編集処理
           PERFORM 20022-PTKOHFTN-HENSYU-SEC
           .
       200322-PTKOHFTN-SAIHENSYU-EXT.
           EXIT.
      *
      ****************************************************************
      *    他一部負担額確認処理
      ****************************************************************
       2004-KOHETC-SYORI-SEC           SECTION.
      *
      *    患者番号判定
           PERFORM 20011-PTINF-INITCHK-SEC
      *
      *    公費選択後のみ
           IF      WRK-RESPONSE-NUMBER     =   "03"
                                           OR  "04"
                                           OR  "05"
               CONTINUE
           ELSE
               MOVE    "19"                TO  WRK-ERRCD
               MOVE    WRK-RESPONSE-NUMBER
                                           TO  PTMODRES-RESPONSE-NUMBER
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        ＰＡＲＡのＳＰＡ編集処理
               PERFORM 2031-SPA-PARA-SEC
           END-IF
      *    項目チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20041-PTKOHFTNETC-CHK-SEC
           END-IF
      *    公費負担一覧編集処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200412-PTMODRES-FTNETC-HEN-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
               MOVE    "05"            TO  PTMODRES-RESPONSE-NUMBER
           ELSE
               IF      PTMODRES-RESPONSE-NUMBER    =   SPACE
                   MOVE    "04"            TO  PTMODRES-RESPONSE-NUMBER
               END-IF
           END-IF
      *    パラメタ登録
           PERFORM 2901-PARA-INS-SEC
      *    SPAの保存
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2008-SPATMP-INS-SEC
           END-IF
           .
       2004-KOHETC-SYORI-EXT.
           EXIT.
      *
      ****************************************************************
      *    他一部負担額項目チェック処理
      ****************************************************************
       20041-PTKOHFTNETC-CHK-SEC           SECTION.
      *
      *    PARA公費ＩＤとSPA公費ＩＤに変更がないこと
           PERFORM 20031-KOHIDPARA-CHK-SEC
      *
           IF      WRK-ERRCD           =   SPACE
      *        他一部負担額対象外
               IF      SPA-NAI24-FTNSYORI  NOT =   1
                   MOVE    "40"                TO  WRK-ERRCD
      ******       MOVE    "2445"              TO  SPA-ERRCD
                   MOVE    "02"            TO  PTMODRES-RESPONSE-NUMBER
               END-IF
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      20041-PTKOHFTNETC-CHK-EXT
           END-IF
      *
      *    選択項目チェック
           PERFORM 200410-FTNETC-SELCHK-SEC
      *
           MOVE    ZERO                TO  TBL-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-FTN2-MAX )
                           OR (WRK-ERRCD   NOT =   SPACE )
               IF     (IDX                         =   WRK-SEL-RENNUM)
                  AND (SPA-NAI24-TFTNSTYMD(IDX)    =   WRK-SEL-TEKSTYMD)
                   MOVE    IDX             TO   SPA-NAI24-FTNSELNUM2
                   MOVE    IDX                 TO  TBL-IDX
                   MOVE    SPA-FTN2-MAX        TO  IDX
               END-IF
           END-PERFORM
           IF      (WRK-ERRCD          =   SPACE )
              AND  (TBL-IDX            =   ZERO  )
               MOVE    "43"                TO  WRK-ERRCD
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200411-PTKOHFTNETC-HEN-SEC
               IF      SPA-P02K-FTNRUIKBN      =   "9"
      *            他一部負担額をゼロにすること
                   MOVE    "44"                TO  WRK-ERRCD
                   MOVE    "03"            TO  PTMODRES-RESPONSE-NUMBER
               END-IF
           END-IF
           .
       20041-PTKOHFTNETC-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新チェック処理
      *****************************************************************
       200410-FTNETC-SELCHK-SEC      SECTION.
      *
      *    選択連番
           MOVE    ZERO                TO  WRK-SEL-RENNUM
           MOVE    SPACE               TO  WRK-SEL-TEKSTYMD
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    PTMODREQ-FTN-SEL-RENNUM TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF      SNUM-RC         NOT =   ZERO
               MOVE    "41"                TO  WRK-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-SEL-RENNUM
               MOVE    WRK-SEL-RENNUM      TO  PTMODRES-FTN-SEL-RENNUM
           END-IF
      *    開始日
           MOVE    PTMODREQ-FTN-SEL-TEKSTYMD
                                       TO  PTMODRES-FTN-SEL-TEKSTYMD
           IF      PTMODREQ-FTN-SEL-TEKSTYMD   =   SPACE
               MOVE    "42"                TO  WRK-ERRCD
           ELSE
               MOVE    PTMODREQ-FTN-SEL-TEKSTYMD
                                           TO  WRK-HEN-DATE
               MOVE    SPACE               TO  WRK-HEN-TIME
               PERFORM 802-DAYHEN02-SEC
               MOVE    WRK-SYMD            TO  WRK-SEL-TEKSTYMD
           END-IF
      *
           .
       200410-FTNETC-SELCHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    他一部負担額編集処理
      ****************************************************************
       200411-PTKOHFTNETC-HEN-SEC           SECTION.
      *
      *    リクエスト番号
           MOVE    "04"                TO  SPA-MCP-WIDGET
           PERFORM 2001211-P024-SYORI-SEC
      *
           PERFORM 2004111-FTNETC-PTMODRES-SEC
           IF      SPA-P02K-KOHID  NOT =   WRK-KOHID
               MOVE    "23"                TO  WRK-ERRCD
           END-IF
           .
       200411-PTKOHFTNETC-HEN-EXT.
           EXIT.
      *
      ****************************************************************
      *    他一部負担額編集処理
      ****************************************************************
       2004111-FTNETC-PTMODRES-SEC           SECTION.
      *
      *    開始
           MOVE    SPA-P02K-NAI-SRYYMD TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PTMODRES-FTNETC-STRYMD
      *    終了日
           MOVE    SPA-P02K-NAI-ENDYMD TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PTMODRES-FTNETC-ENDYMD
      *    上限額（同月分を集計）
           MOVE    SPA-P02K-GAIJGNGAKKEI   TO  WRK-MONEY
           MOVE    WRK-MONEY           TO  PTMODRES-FTNETC-GAIJGNGAK
      *    他一部負担累計
           IF      SPA-P02K-FTNRUIKBN      =   "9"
               MOVE    SPA-P02K-TNYUJGNGAK
                                       TO  WRK-MONEY
           ELSE
               MOVE    SPA-P02K-TAGOKEI
                                       TO  WRK-MONEY
           END-IF
           MOVE    WRK-MONEY           TO  PTMODRES-FTNETC-NYUJGNGAK
      *    自院集計額（収納合計額）
           MOVE    SPA-P02K-JIGOKEI    TO  WRK-MONEY
           MOVE    WRK-MONEY           TO  PTMODRES-FTNETC-SYUTOTAL
      *
           .
       2004111-FTNETC-PTMODRES-EXT.
           EXIT.
      *
      ****************************************************************
      *    他一部負担額編集処理
      ****************************************************************
       200412-PTMODRES-FTNETC-HEN-SEC           SECTION.
      *
           INITIALIZE                      PTKOHFTNETC-REC
           MOVE    SPA-HOSPNUM         TO  PTKOHFTNETC-HOSPNUM
           MOVE    SPA-PTID            TO  PTKOHFTNETC-PTID
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
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL    FLG-PTKOHFTNETC    =   1
               ADD     1                   TO  IDX
               MOVE    PTKOHFTNETC-SRYYMD  TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  PTMODRES-FTNETC-SRYYMD
                                                                (IDX)
               MOVE    PTKOHFTNETC-NYUGAIKBN
                                           TO  PTMODRES-FTNETC-NYUGAIKBN
                                                                (IDX)
               MOVE    PTKOHFTNETC-FTNMONEY
                                           TO  PTMODRES-FTNETC-FTNMONEY
                                                                (IDX)
      *
               MOVE    "tbl_ptkohftnetc"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-PTKOHFTNETC-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptkohftnetc"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       200412-PTMODRES-FTNETC-HEN-EXT.
           EXIT.
      *
      ****************************************************************
      *    他一部負担額更新処理
      ****************************************************************
       2005-KOHETC-UPDATE-SEC           SECTION.
      *
      *    患者番号判定
           PERFORM 20011-PTINF-INITCHK-SEC
      *
      *    公費選択後のみ
           IF      WRK-RESPONSE-NUMBER     =   "05"
               CONTINUE
           ELSE
               MOVE    "19"                TO  WRK-ERRCD
               MOVE    WRK-RESPONSE-NUMBER
                                           TO  PTMODRES-RESPONSE-NUMBER
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        ＰＡＲＡのＳＰＡ編集処理
               PERFORM 2031-SPA-PARA-SEC
           END-IF
      *    項目チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20051-PTKOHFTNETC-UPDCHK-SEC
           END-IF
      *    更新処理
           IF     (WRK-ERRCD           =   SPACE )
              AND (FLG-MODE        NOT =   ZERO  )
               PERFORM 20052-PTKOHFTNETC-UPDATE-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        継続
               IF      FLG-CONTKBN         =   1
      *            パラメタ削除
                   PERFORM 2902-PARA-ALLDEL-SEC
                   PERFORM 2009-SPATMP-DEL-SEC
      *            公費選択へ
                   MOVE    "02"            TO  PTMODRES-RESPONSE-NUMBER
      *            継続区分終了
                   MOVE    "1"             TO  WRK-ENDKBN
      *            APIパラメタ登録
                   PERFORM 2901-PARA-INS-SEC
                   PERFORM 2008-SPATMP-INS-SEC
               ELSE
                   MOVE    "00"            TO  PTMODRES-RESPONSE-NUMBER
      *            パラメタ削除・排他解除処理
                   PERFORM 2900-SYORI-END-SEC
               END-IF
           ELSE
               IF      PTMODRES-RESPONSE-NUMBER    =   SPACE
                   MOVE    "05"            TO  PTMODRES-RESPONSE-NUMBER
               END-IF
               IF      FLG-CANSEL      =   ZERO
      *            SPA再編集
                   PERFORM 2901-PARA-INS-SEC
      **********   PERFORM 2008-SPATMP-INS-SEC
               END-IF
           END-IF
           .
       2005-KOHETC-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新チェック処理
      *****************************************************************
       20051-PTKOHFTNETC-UPDCHK-SEC     SECTION.
      *
      *    PARA公費ＩＤとSPA公費ＩＤに変更がないこと
           PERFORM 20031-KOHIDPARA-CHK-SEC
      *
           IF      WRK-ERRCD           =   SPACE
      *        選択番号変更なし
               PERFORM 200410-FTNETC-SELCHK-SEC
      *        前回と一致
               IF      WRK-SEL-RENNUM      NOT =   WRK-PARA-SELNUM
                   MOVE    "24"            TO  WRK-ERRCD
                   MOVE    "04"            TO  PTMODRES-RESPONSE-NUMBER
               END-IF
      *
               IF      WRK-ERRCD           =   SPACE
                   PERFORM 2004111-FTNETC-PTMODRES-SEC
               END-IF
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        公費負担額更新チェック
               EVALUATE    PTMODREQ-FTNETC-MODE
               WHEN    "DELETE"
               WHEN    "Delete"
                   MOVE    2               TO  FLG-MODE
               WHEN    "MODIFY"
               WHEN    "Modify"
                   PERFORM 200511-PTKOHFTNETC-CHK-SEC
                   MOVE    1               TO  FLG-MODE
               WHEN    SPACE
                   MOVE    ZERO            TO  FLG-MODE
               WHEN    OTHER
                   MOVE    "50"            TO  WRK-ERRCD
               END-EVALUATE
           END-IF
      *
           .
       20051-PTKOHFTNETC-UPDCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新チェック処理
      *****************************************************************
       200511-PTKOHFTNETC-CHK-SEC      SECTION.
      *
           INITIALIZE                  TBL-PTKOHFTNETC-AREA
           MOVE    ZERO                TO  FLG-ERR-01
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   100 )
               MOVE    PTMODREQ-FTNETC-SRYYMD (IDX)
                                       TO  PTMODRES-FTNETC-SRYYMD
                                                               (IDX)
               MOVE    PTMODREQ-FTNETC-NYUGAIKBN (IDX)
                                       TO  PTMODRES-FTNETC-NYUGAIKBN
                                                               (IDX)
               MOVE    PTMODREQ-FTNETC-FTNMONEY (IDX)
                                       TO  PTMODRES-FTNETC-FTNMONEY
                                                               (IDX)
               MOVE    SPACE               TO  SPA-ERRCD
      *        年月日
               IF      PTMODREQ-FTNETC-SRYYMD(IDX)  =   SPACE
                   CONTINUE
               ELSE
                   MOVE    PTMODREQ-FTNETC-SRYYMD(IDX)
                                           TO  WRK-HEN-DATE
                   MOVE    SPACE           TO  WRK-HEN-TIME
                   PERFORM 802-DAYHEN02-SEC
                   MOVE    WRK-SYMD        TO  TBL-PTKOHFTNETC-SRYYMD
                                                         (IDX)
      *            暦日チェック
                   PERFORM 803-ORCSDAY-CHEK-SEC
                   IF      FLG-DAYERR          =   1
                       MOVE    "AP51"              TO  SPA-ERRCD
                   END-IF
               END-IF
      *        入外区分
               MOVE    PTMODREQ-FTNETC-NYUGAIKBN (IDX)
                                           TO  TBL-PTKOHFTNETC-NYUGAIKBN
                                                         (IDX)
               IF      PTMODREQ-FTNETC-NYUGAIKBN (IDX)
                                           =   "1"
                                           OR  "2"
                                           OR  SPACE
                   CONTINUE
               ELSE
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "AP52"              TO  SPA-ERRCD
                   END-IF
               END-IF
      *        負担額
               IF      PTMODREQ-FTNETC-FTNMONEY  (IDX)
                                       NOT =   SPACE
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    PTMODREQ-FTNETC-FTNMONEY  (IDX)
                                               TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF     (SNUM-RC         NOT =   ZERO  )   OR
                          (SNUM-MINKBN     NOT =   SPACE )   OR
                          (SNUM-SYOKBN     NOT =   SPACE )
      *                整数であること
                       IF      SPA-ERRCD           =   SPACE
                           MOVE    "AP53"                TO  SPA-ERRCD
                       END-IF
                   ELSE
                       MOVE    SNUM-OUTNUM
                                           TO  TBL-PTKOHFTNETC-FTNMONEY
                                                              (IDX)
                   END-IF
               END-IF
      *        必須チェック
               IF      TBL-PTKOHFTNETC-SRYYMD (IDX)  =   SPACE
                   IF      (PTMODREQ-FTNETC-NYUGAIKBN (IDX)  =   SPACE)
                       AND (PTMODREQ-FTNETC-FTNMONEY  (IDX)  =   SPACE)
                       CONTINUE
                   ELSE
                       IF      SPA-ERRCD           =   SPACE
                           MOVE    "AP54"                TO  SPA-ERRCD
                       END-IF
                   END-IF
               ELSE
                   IF      (PTMODREQ-FTNETC-NYUGAIKBN (IDX)  =   SPACE)
                       IF      SPA-ERRCD           =   SPACE
                           MOVE    "AP52"              TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
      *
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 8901-PTERRCD-SETAPI-SEC
                   MOVE    SPA-ERRCD       TO  PTMODRES-FTNETC-ERRCD
                                                                (IDX)
                   MOVE    WRK-ERRMSG      TO
                                           PTMODRES-FTNETC-ERRCD-MSG
                                                                (IDX)
                   MOVE    1               TO  FLG-ERR-01
                   MOVE    SPACE           TO  SPA-ERRCD
                   MOVE    SPACE           TO  WRK-ERRMSG
               END-IF
           END-PERFORM
      *
      *    関連チェック
           MOVE    ZERO                TO  TBL-ETC-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   100 )
                       OR     (FLG-ERR-01  =   1    )
                       OR     (TBL-PTKOHFTNETC-SRYYMD(IDX) =   SPACE)
               MOVE    IDX                 TO  TBL-ETC-MAX
      *
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    SPACE               TO  WRK-ERRCD
      *        年月日チェック
               PERFORM 2005111-FTNETC-SRYYMD-SEC
      *
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 8901-PTERRCD-SETAPI-SEC
                   MOVE    SPA-ERRCD       TO  PTMODRES-FTNETC-ERRCD
                                                                (IDX)
                   MOVE    WRK-ERRMSG      TO
                                           PTMODRES-FTNETC-ERRCD-MSG
                                                                (IDX)
                   MOVE    1               TO  FLG-ERR-02
                   MOVE    SPACE           TO  SPA-ERRCD
                   MOVE    SPACE           TO  WRK-ERRMSG
               END-IF
           END-PERFORM
      *
           IF      (FLG-ERR-02         =   1   )
               OR  (FLG-ERR-01         =   1   )
               MOVE    "52"                TO  WRK-ERRCD
           END-IF
      *
           .
       200511-PTKOHFTNETC-CHK-EXT.
           EXIT.
      *
      *
      ****************************************************************
      *    年月日チェック処理
      ****************************************************************
       2005111-FTNETC-SRYYMD-SEC           SECTION.
      *
      *    公費期間チェック
           IF     (SPA-NAI24-TKOHSTYMD (SPA-NAI24-FTNSELNUM)
                                       >   TBL-PTKOHFTNETC-SRYYMD
                                                       (IDX))
             OR   (SPA-NAI24-TKOHEDYMD (SPA-NAI24-FTNSELNUM)
                                       <   TBL-PTKOHFTNETC-SRYYMD
                                                       (IDX))
      ***      MOVE    "0006"              TO  SPA-ERRCD
               MOVE    "AP55"              TO  SPA-ERRCD
           END-IF
      *    履歴期間
           IF      SPA-ERRCD           =   SPACE
               IF     (TBL-PTKOHFTNETC-SRYYMD (IDX)(1:6)
                                           =   SPA-P02K-NAI-SRYYMD
                                                             (1:6))
                   IF     (TBL-PTKOHFTNETC-SRYYMD (IDX)
                                           <   SPA-P02K-NAI-SRYYMD)
                     OR   (TBL-PTKOHFTNETC-SRYYMD (IDX)
                                           >   SPA-P02K-NAI-ENDYMD)
      *                同月内で期間外
      *****            MOVE    "K004"              TO  SPA-ERRCD
                       MOVE    "AP56"              TO  SPA-ERRCD
                   END-IF
               ELSE
      *            期間外
                   MOVE    "AP56"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *    同一日は９９件まで
           MOVE    ZERO                TO  WRK-DAYCNT
           PERFORM VARYING     IDZ     FROM    1   BY   1
                   UNTIL      (IDZ     >   100  )
                          OR  (TBL-PTKOHFTNETC-SRYYMD(IDZ) =   SPACE)
              IF      (TBL-PTKOHFTNETC-SRYYMD (IDX)
                                           =   TBL-PTKOHFTNETC-SRYYMD
                                                               (IDZ))
                   ADD     1               TO  WRK-DAYCNT
              END-IF
           END-PERFORM
           IF      WRK-DAYCNT          >   99
      ****     MOVE    "0011"          TO  SPA-ERRCD
               MOVE    "AP57"          TO  SPA-ERRCD
           END-IF
      *
           .
       2005111-FTNETC-SRYYMD-EXT.
           EXIT.
      *
      ****************************************************************
      *    他一部負担額更新処理
      ****************************************************************
       20052-PTKOHFTNETC-UPDATE-SEC           SECTION.
      *
           MOVE    ZERO                TO  SPA-P02K-TAGOKEI
      *
           EVALUATE    PTMODREQ-FTNETC-MODE
               WHEN    "DELETE"
               WHEN    "Delete"
                   MOVE    SPA-P02K-NAI-SRYYMD(1:6)
                                                   TO  WRK-SRYYM
                   PERFORM 2003221-PTKOHETC-DEL-SEC
               WHEN    "MODIFY"
               WHEN    "Modify"
                   PERFORM 200522-PTKOHFTNETC-INS-SEC
           END-EVALUATE
      *
      *    公費負担額の他一部負担額更新
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200523-PTKOHFTN-MONEYUPD-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        公費負担更新編集処理
               PERFORM 200322-PTKOHFTN-SAIHENSYU-SEC
           END-IF
           .
       20052-PTKOHFTNETC-UPDATE-EXT.
           EXIT.
      *
      ****************************************************************
      *    他一部負担額更新処理
      ****************************************************************
       200522-PTKOHFTNETC-INS-SEC           SECTION.
      *
      *     一括削除
           MOVE    SPA-P02K-NAI-SRYYMD(1:6)   TO  WRK-SRYYM
           PERFORM 2003221-PTKOHETC-DEL-SEC
      *
           INITIALIZE                      PTKOHFTNETC-REC
           MOVE    ZERO                TO  WRK-RENNUM
           PERFORM VARYING     IDX     FROM    1   BY   1
                   UNTIL      (IDX     >   TBL-ETC-MAX  )
                           OR (WRK-ERRCD   NOT =   SPACE )
      *        年月日順で登録
               MOVE    "99999999"          TO  WRK-KEY-SRYYMD
               MOVE    ZERO                TO  IDX-K
               PERFORM VARYING     IDY     FROM    1   BY   1
                       UNTIL      (IDY     >   TBL-ETC-MAX   )
                   IF     (TBL-PTKOHFTNETC-SRYYMD (IDY) =   SPACE )
                       CONTINUE
                   ELSE
                       IF      TBL-PTKOHFTNETC-SRYYMD (IDY)
                                                   <   WRK-KEY-SRYYMD
                           MOVE    IDY             TO  IDX-K
                           MOVE    TBL-PTKOHFTNETC-SRYYMD (IDY)
                                                   TO  WRK-KEY-SRYYMD
                       END-IF
                   END-IF
               END-PERFORM
      *
               IF      IDX-K           >   ZERO
                   PERFORM 200521-PTKOHFTNETC-INSERT-SEC
                   MOVE    SPACE           TO  TBL-PTKOHFTNETC-SRYYMD
                                                              (IDX-K)
               END-IF
           END-PERFORM
           .
       20052-PTKOHFTNETC-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    公費負担額テーブル編集処理
      *****************************************************************
       200521-PTKOHFTNETC-INSERT-SEC           SECTION.
      *
           IF      PTKOHFTNETC-SRYYMD  =   TBL-PTKOHFTNETC-SRYYMD
                                                               (IDX-K)
               ADD     1                   TO  WRK-RENNUM
           ELSE
               MOVE    1                   TO  WRK-RENNUM
           END-IF
      *
           INITIALIZE                      PTKOHFTNETC-REC
           MOVE    SPA-HOSPNUM         TO  PTKOHFTNETC-HOSPNUM
           MOVE    SPA-PTID            TO  PTKOHFTNETC-PTID
           MOVE    SPA-P02K-KOHID      TO  PTKOHFTNETC-KOHID
           MOVE    SPA-P02K-KOHNUM     TO  PTKOHFTNETC-KOHNUM
           MOVE    TBL-PTKOHFTNETC-SRYYMD (IDX-K)
                                       TO  PTKOHFTNETC-SRYYMD
           MOVE    WRK-RENNUM          TO  PTKOHFTNETC-RENNUM
           MOVE    TBL-PTKOHFTNETC-NYUGAIKBN (IDX-K)
                                       TO  PTKOHFTNETC-NYUGAIKBN
           MOVE    TBL-PTKOHFTNETC-FTNMONEY (IDX-K)
                                       TO  PTKOHFTNETC-FTNMONEY
      *    他一部負担額集計
           ADD     TBL-PTKOHFTNETC-FTNMONEY (IDX-K)
                                       TO  SPA-P02K-TAGOKEI
      *
           MOVE    SMCNDATE-YMD        TO  PTKOHFTNETC-CREYMD
           MOVE    SMCNDATE-YMD        TO  PTKOHFTNETC-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTKOHFTNETC-UPHMS
      *
           MOVE    WRK-OPID            TO  PTKOHFTNETC-OPID
      *
           MOVE    PTKOHFTNETC-REC     TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_ptkohftnetc"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "64"                TO  WRK-ERRCD
           END-IF
      *
           .
       200521-PTKOHFTNETC-INSERT-EXT.
           EXIT.
      *
      ****************************************************************
      *    公費負担額の他一部負担額更新処理
      ****************************************************************
       200523-PTKOHFTN-MONEYUPD-SEC           SECTION.
      *
           INITIALIZE                      PTKOHFTN-REC
           MOVE    SPA-HOSPNUM         TO  PTKOHFTN-HOSPNUM
           MOVE    SPA-PTID            TO  PTKOHFTN-PTID
           MOVE    WRK-KOHID           TO  PTKOHFTN-KOHID
           MOVE    SPA-P02K-NAI-SRYYMD(1:6)
                                       TO  PTKOHFTN-TEKSTYMD(1:6)
           MOVE    "%"                 TO  PTKOHFTN-TEKSTYMD(7:1)
           MOVE    PTKOHFTN-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 910-PTKOHFTN-READ-SEC
           ELSE 
               MOVE    1                   TO  FLG-PTKOHFTN
           END-IF
      *
           MOVE    SPA-P02K-TAGOKEI        TO  WRK-NYUJGNGAK
           PERFORM
               UNTIL    FLG-PTKOHFTN       =   1
      *        最初の開始日に他一部負担額を設定する
               MOVE    WRK-NYUJGNGAK       TO  PTKOHFTN-NYUJGNGAK
      *
               MOVE    ZERO                TO  WRK-NYUJGNGAK
      *
               MOVE    SMCNDATE-YMD        TO  PTKOHFTN-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTKOHFTN-UPHMS
               MOVE    WRK-OPID            TO  PTKOHFTN-OPID
      *
               MOVE    PTKOHFTN-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                    MOVE    "65"               TO  WRK-ERRCD
               END-IF
      *
               MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 910-PTKOHFTN-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       200523-PTKOHFTN-MONEYUPD-EXT.
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
      *        公費負担対象一覧編集処理
               PERFORM 20013-PTKOH-HENSYU-SEC
            END-IF
      *
      *    公費負担一覧編集処理
           IF      WRK-ERRCD           =   SPACE
               IF      PTMODREQ-INSURANCE-ID   =   SPACE
      *            １件目を対象
                   MOVE    1                   TO  SPA-GMN24-FTNSELNUM
                   MOVE    1                   TO  SPA-NAI24-FTNSELNUM
                   MOVE    SPA-NAI24-KOHID(1)  TO  WRK-KOHID
      *            公費負担額編集
                   MOVE    2                   TO  FLG-HENSYU
                   PERFORM 2001212-PTKOHFTN-SELHENSYU-SEC
               ELSE
                   MOVE    2                   TO  FLG-SYORI
                   PERFORM 20021-PTKOHFTN-CHK-SEC
               END-IF
               IF      WRK-ERRCD           =   SPACE
      *            公費負担対象一覧編集処理
                   PERFORM 20022-PTKOHFTN-HENSYU-SEC
               END-IF
           END-IF
           MOVE    ZERO                    TO  FLG-SYORI
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
               MOVE    "01"                TO  PTMODRES-RESPONSE-NUMBER
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
               MOVE    "01"                TO  PTMODRES-RESPONSE-NUMBER
           END-IF
           .
       2031-SPA-PARA-EXT.
           EXIT.
      *
      ****************************************************************
      *    エラーメッセージ編集処理
      ****************************************************************
      *290-ERROR-HEN-SEC           SECTION.
      *
      *    PERFORM VARYING   IDX       FROM   1   BY  1
      *            UNTIL    (IDX       >   50     )
      *               OR    (WRK-ERRCD-T (IDX)    =   SPACE )
      *        MOVE    WRK-ERRCD-T (IDX)   TO  WRK-ERRCD
      *        PERFORM 890-ERRCD-MSG-SEC
      *        MOVE    WRK-ERRCD-T (IDX)   TO  PTMODRES-ERROR
      *                                                    (IDX)
      *        MOVE    WRK-ERRMSG          TO  PTMODRES-ERROR-MSG
      *                                                    (IDX)
      *    END-PERFORM
      *    .
      *290-ERROR-HEN-EXT.
      *    EXIT.
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
      *    WHEN    "11"
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
           WHEN    "19"
               MOVE    "処理順が違います。"
                                               TO  WRK-ERRMSG
           WHEN    "22"
               MOVE    "前回と患者番号が違います。"
                                               TO  WRK-ERRMSG
           WHEN    "23"
               MOVE    "前回と公費ＩＤが違います。"
                                               TO  WRK-ERRMSG
           WHEN    "24"
               MOVE    "前回と選択連番が違います。"
                                               TO  WRK-ERRMSG
      *
           WHEN    "30"
               MOVE    "公費負担額の対象はありません。"
                                               TO  WRK-ERRMSG
      *
           WHEN    "31"
               MOVE    "公費ＩＤ数値エラー"
                                               TO  WRK-ERRMSG
           WHEN    "32"
               MOVE    "公費選択対象がありません。"
                                               TO  WRK-ERRMSG
           WHEN    "33"
               STRING   "公費ＩＤが一致しません。"
                        "再度公費選択を行って下さい。"
                                               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "34"
               MOVE    "送信されていない連番があります。"
                                               TO  WRK-ERRMSG
           WHEN    "35"
               MOVE    "公費負担額にエラーがあります"
                                               TO  WRK-ERRMSG
      *
           WHEN    "40"
               MOVE    "他一部負担入力対象外公費です。"
                                               TO  WRK-ERRMSG
           WHEN    "41"
               MOVE    "選択連番エラー"
                                               TO  WRK-ERRMSG
           WHEN    "42"
               MOVE    "適応開始日エラー"
                                               TO  WRK-ERRMSG
           WHEN    "43"
               MOVE    "対象の公費負担額がありません。"
                                               TO  WRK-ERRMSG
           WHEN    "44"
               STRING  "対象の他一部負担累計は公費負担額履歴で"
                       "登録されています。"
                       "他一部負担累計をゼロにして下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-ERRMSG
               END-STRING
      *
           WHEN    "50"
               MOVE    "他一部負担更新区分エラー"
                                               TO  WRK-ERRMSG
           WHEN    "52"
               MOVE    "他一部負担額にエラーがあります"
                                               TO  WRK-ERRMSG
      *
           WHEN    "61"
               MOVE    "公費負担額削除エラー"
                                               TO  WRK-ERRMSG
           WHEN    "62"
               MOVE    "公費負担額登録エラー"
                                               TO  WRK-ERRMSG
           WHEN    "63"
               MOVE    "他一部負担額削除エラー"
                                               TO  WRK-ERRMSG
           WHEN    "64"
               MOVE    "他一部負担額登録エラー"
                                               TO  WRK-ERRMSG
           WHEN    "65"
               MOVE    "公費負担額更新エラー"
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
           EVALUATE    SPA-ERRCD
      *
           WHEN    "AP34"
               MOVE    "公費負担額更新区分エラー"
                                               TO  WRK-ERRMSG
           WHEN    "AP35"
               MOVE    "連番数値エラー"
                                               TO  WRK-ERRMSG
           WHEN    "AP36"
               MOVE    "上限額１数値エラー"
                                               TO  WRK-ERRMSG
           WHEN    "AP37"
               MOVE    "上限額２数値エラー"
                                               TO  WRK-ERRMSG
           WHEN    "AP38"
               MOVE    "開始日付未入力"
                                               TO  WRK-ERRMSG
           WHEN    "AP39"
               MOVE    "開始日付は変更できません。"
                                               TO  WRK-ERRMSG
           WHEN    "AP40"
               MOVE    "同じ連番があります。"
                                               TO  WRK-ERRMSG
           WHEN    "AP41"
               MOVE    "登録済みの連番です。"
                                               TO  WRK-ERRMSG
           WHEN    "AP42"
               MOVE    "連番がありません"
                                               TO  WRK-ERRMSG
           WHEN    "AP45"
               STRING  "同一開始年月で他一部負担履歴が"
                       "登録されています。"
                       "他一部負担累計は他一部負担履歴を"
                       "更新して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-ERRMSG
               END-STRING
      *
           WHEN    "AP51"
               MOVE    "年月日暦日エラー"
                                               TO  WRK-ERRMSG
           WHEN    "AP52"
               MOVE    "入外区分エラー"
                                               TO  WRK-ERRMSG
           WHEN    "AP53"
               MOVE    "負担額数値エラー"
                                               TO  WRK-ERRMSG
           WHEN    "AP54"
               MOVE    "年月日が未入力です"
                                               TO  WRK-ERRMSG
           WHEN    "AP55"
               STRING  "公費適用期間外です。"
                                               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "AP56"
               STRING  "選択中の上限額履歴の期間外の日です。"
                                               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "AP57"
               MOVE    "同一日が９９件以上です。"
                                               TO  WRK-ERRMSG
           END-EVALUATE
           .
       8901-PTERRCD-SETAPI-EXT.
           EXIT.
      *
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
      *    患者公費負担テーブル読み込み
      *****************************************************************
       910-PTKOHFTN-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHFTN-REC
               MOVE    ZERO            TO  FLG-PTKOHFTN
           ELSE
               INITIALIZE                  PTKOHFTN-REC
               MOVE    1               TO  FLG-PTKOHFTN
           END-IF
      *
           .
       910-PTKOHFTN-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費負担テーブル読み込み
      *****************************************************************
       990-PTKOHFTNETC-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHFTNETC-REC
               MOVE    ZERO            TO  FLG-PTKOHFTNETC
           ELSE
               INITIALIZE                  PTKOHFTNETC-REC
               MOVE    1               TO  FLG-PTKOHFTNETC
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
