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
       PROGRAM-ID.         ORAPI012S4V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 患者　所得者情報・特記事項・個別情報等設定
      *                                               （登録・削除）(V3)
      *  管理者            :
      *  作成日付    作業者        記述
      *  16/09/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-多々納  17/05/XX  境界層該当対応
      *  05.00.00    NACL-多々納  18/03/XX  妊婦設定対応
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
           03  FLG-KYUSEIRRK           PIC 9(01).
           03  FLG-TSYRRK              PIC 9(01).
           03  FLG-TNKRRK              PIC 9(01).
           03  FLG-PTTOKKI             PIC 9(01).
           03  FLG-PTCONF              PIC 9(01).
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
           03  FLG-HKNKOHERR           PIC 9(01).
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
           03  IDX3                    PIC 9(04).
           03  IDX-T                   PIC 9(04).
           03  IDX-N                   PIC 9(04).
      *
           03  IDXE                    PIC 9(04).
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
           03  WRK-HEN-DATE-R          REDEFINES  WRK-HEN-DATE.
               05  WRK-HEN-DATE-1      PIC X(07).
               05  WRK-HEN-H3-1        PIC X(03).
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
           03  WRK-KEICD-TBLG.
               05  WRK-KEICD-TBL       OCCURS   10.
                   07  WRK-KEICD-T         PIC X(02).
      *
      *
      *    認定範囲名称
           03  WRK-HKNTEKKBN-NAME-G.
               05  WRK-HKNTEKKBN-F1        PIC X(02).
               05  WRK-HKNTEKKBN-NAME      PIC X(28).
           03  WRK-HKNTEKKBN           PIC X(01).
      *
      *    患者個別設定　設定ＫＥＹ
           03  WRK-CONF-CKEY           PIC  X(25).
      *                  設定データ
           03  WRK-CONF-CDATA          PIC  X(10).
      *    地域包括診療対象疾病
           03  WRK-CHKBYO-G.
               05  WRK-CHKBYO          PIC X(01)
                                       OCCURS   4.
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
      *H29.6
      *    境界層該当区分
           03  WRK-KYOUKAI             PIC X(01).
           03  WRK-KYOUKAI-MEI         PIC X(20).
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
      *    低所得
       01  TBL-TSYRRK-AREA.
           02  TBL-TSYRRK-REC        OCCURS   20.
           COPY    "CPTSYRRK.INC"    REPLACING
                                     //TSYRRK-// BY //TBL-TSYRRK-//.
           03  TBL-TSYRRK-ERRCD             PIC X(04).
      *
      *    年金
       01  TBL-TNKRRK-AREA.
           02  TBL-TNKRRK-REC        OCCURS   20.
           COPY    "CPTNKRRK.INC"     REPLACING
                                     //TNKRRK-// BY //TBL-TNKRRK-//.
      *
      *    旧姓履歴
       01  TBL-KYUSEIRRK-AREA.
           02  TBL-KYUSEIRRK-REC        OCCURS   20.
           COPY    "CPKYUSEIRRK.INC"     REPLACING
                                        //KYUSEI-// BY //TBL-KYUSEI-//.
      *
      *    患者特記事項マスタ
       01  TBL-PTTOKKI-AREA.
           02  TBL-PTTOKKI-REC         OCCURS   50.
           COPY    "CPPTTOKKI.INC"     REPLACING
                                       //PTTOKKI-// BY //TBL-PTTOKKI-//.
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
           COPY    "CPPATIENTMODV3REQ4.INC".
      *    API XML出力用定義体
           COPY    "CPPATIENTMODV3RES4.INC".
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
      *    旧姓履歴
       01  KYUSEIRRK-REC.
           COPY    "CPKYUSEIRRK.INC".
      *
      *    低所得
       01  TSYRRK-REC.
           COPY    "CPTSYRRK.INC".
      *
      *    年金
       01  TNKRRK-REC.
           COPY    "CPTNKRRK.INC".
      *
      *    患者特記事項マスタ
       01  PTTOKKI-REC.
           COPY    "CPPTTOKKI.INC".
      *
      *    患者個別設定テーブル
       01  PTCONF-REC.
           COPY    "CPPTCONF.INC".
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
               MOVE   404                  TO  APILST06-HTTPSTATUS
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
           INITIALIZE                      TBL-TSYRRK-AREA
           INITIALIZE                      TBL-TNKRRK-AREA
           INITIALIZE                      TBL-KYUSEIRRK-AREA
           INITIALIZE                      TBL-PTTOKKI-AREA
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
      *        確認更新処理
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
           IF      APILST06-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-PATIENTMODREQ
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_patientmodv3req4"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST06-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "patientmodv3req4"     TO  APIREQ-RECORDNAME
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
           MOVE    "xml_patientmodv3req4"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST06-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "patientmodv3req4"  TO  APIREQ-RECORDNAME
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
      *H30.4
           IF     (WRK-ERRCD           =   ZERO )
              AND (IDX-WAR             >   ZERO )
      *        ワーニング有
               MOVE    "W"                 TO  WRK-ERRKBN
               IF      FLG-SYORI           =   2
                   MOVE    SPACE               TO  WRK-OKMSG
                   STRING  "登録処理終了。"
                           "警告メッセージがあります。"
                           "確認して下さい。"
                                           DELIMITED  BY  SIZE
                                           INTO  WRK-OKMSG
                   END-STRING
               END-IF
               MOVE    WRK-ERRCD-G         TO  PTMODRES-API-RESULT
               MOVE    WRK-OKMSG           TO  PTMODRES-API-RESULT-MSG
           END-IF
      *
           IF     (IDX-ERR             >   ZERO )
      *H30.4
              OR  (IDX-WAR             >   ZERO )
               PERFORM 290-ERROR-HEN-SEC
           END-IF
      *
      *
           IF      APILST06-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_patientmodv3res4"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  PTMODRES-MODE
           MOVE    "patientmodv3res4"  TO  PTMODRES-RECORDNAME
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
           MOVE    "xml_patientmodv3res4"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  PTMODRES-MODE
           MOVE    "patientmodv3res4"  TO  PTMODRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-PATIENTMODRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               MOVE    PTMODRES-OBJECT     TO  APILST06-BODY
               MOVE    "patientmod/xml"    TO  APILST06-CONTENT-TYPE
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
               IF      WRK-PARA-SYORIPG    NOT =   "04"
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
      *        低所得者２（低所得者履歴）情報編集処理
               PERFORM 20013-TSYRRK-HENSYU-SEC
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        低所得者１情報（年金履歴）情報編集処理
               PERFORM 20014-TNKRRK-HENSYU-SEC
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        旧姓履歴編集処理
               PERFORM 20015-KYUSEIRRK-HENSYU-SEC
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        特記事項編集処理
               PERFORM 20016-PTTOKKI-HENSYU-SEC
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        患者個別情報編集処理
               PERFORM 20017-PTCONF-HENSYU-SEC
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
           MOVE    "04"                TO  WRK-PARA-SYORIPG
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
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *    特記事項初期設定
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
           CALL    "ORCGP026"          USING   MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
      *
           .
       20012-PTINF-HEN-EXT.
           EXIT.
      *
      ****************************************************************
      *    低所得者２（低所得者履歴）情報編集処理
      ****************************************************************
       20013-TSYRRK-HENSYU-SEC           SECTION.
      *
           INITIALIZE                  PTMODRES-TSYRRK-INFO-G
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "TSYRRK"            TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL   (FLG-PARA       =   1 )
                       OR  (IDX            >=  20)
               MOVE    PARA-DATA-REC       TO  TSYRRK-REC
      *
               ADD     1                   TO  IDX
               PERFORM 20013-TSYRRK-PTMODRES-SEC
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       20013-TSYRRK-HENSYU-EXT.
           EXIT.
      *
      ****************************************************************
      *    低所得者２（低所得者履歴）編集処理
      **************************************************************
       20013-TSYRRK-PTMODRES-SEC           SECTION.
      *
      *    認定開始年月日
           MOVE    TSYRRK-NINSTYMD     TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PTMODRES-TSYRRK-NINSTYMD
                                                                  (IDX)
      *    認定終了年月日
           MOVE    TSYRRK-NINEDYMD     TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PTMODRES-TSYRRK-NINEDYMD
                                                                  (IDX)
      *    食事標準負担額減額開始日
           IF      TSYRRK-SKJNINSTYMD  NOT =   SPACE
               MOVE    TSYRRK-SKJNINSTYMD  TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE    TO  PTMODRES-TSYRRK-SKJNINSTYMD
                                                                  (IDX)
           END-IF
      *    長期入院食事減額開始日
           IF      TSYRRK-SKJGNSTYMD  NOT =   SPACE
               MOVE    TSYRRK-SKJGNSTYMD  TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE    TO  PTMODRES-TSYRRK-SKJGNSTYMD
                                                                  (IDX)
           END-IF
      *    保険適用区分
           MOVE    TSYRRK-HKNTEKKBN    TO  PTMODRES-TSYRRK-HKNTEKKBN
                                                                  (IDX)
      *    低所得者２　認定範囲（画面）
           MOVE    TSYRRK-HKNTEKKBN    TO  WRK-HKNTEKKBN
           PERFORM 200212-TSYRRK-HKNTEKKBN-SEC
           IF      FLG-ERR                 =   ZERO
               MOVE    WRK-HKNTEKKBN-NAME
                                        TO  PTMODRES-TSYRRK-HKNTEK-NAME
                                                                  (IDX)
           END-IF
      *H29.6
      *    境界層該当区分
           IF      TSYRRK-KYOUKAISOUKBN    =   SPACE
               MOVE    "0"                 TO  TSYRRK-KYOUKAISOUKBN
           END-IF
           MOVE    TSYRRK-KYOUKAISOUKBN
                                       TO  PTMODRES-TSYRRK-KYOUKAISOU
                                                                  (IDX)
           MOVE    TSYRRK-KYOUKAISOUKBN    TO  WRK-KYOUKAI
           PERFORM 41003-KYOUKAI-HEN-SEC
           MOVE    WRK-KYOUKAI-MEI     TO  PTMODRES-TSYRRK-KYOUKAI-NAME
                                                                  (IDX)
      *
      *
           .
       20013-TSYRRK-PTMODRES-EXT.
           EXIT.
      *
      ****************************************************************
      *    低所得者１（年金履歴）情報編集処理
      ****************************************************************
       20014-TNKRRK-HENSYU-SEC           SECTION.
      *
           INITIALIZE                  PTMODRES-TNKRRK-INFO-G
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "TNKRRK"            TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL   (FLG-PARA       =   1 )
                       OR  (IDX            >=  20)
               MOVE    PARA-DATA-REC       TO  TNKRRK-REC
      *
               ADD     1                   TO  IDX
               PERFORM 20014-TNKRRK-PTMODRES-SEC
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       20014-TNKRRK-HENSYU-EXT.
           EXIT.
      *
      ****************************************************************
      *    低所得者１（年金履歴）編集処理
      **************************************************************
       20014-TNKRRK-PTMODRES-SEC           SECTION.
      *
      *    認定開始年月日
           MOVE    TNKRRK-STYMD        TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PTMODRES-TNKRRK-STYMD
                                                                  (IDX)
      *    認定終了年月日
           MOVE    TNKRRK-EDYMD        TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PTMODRES-TNKRRK-EDYMD
                                                                  (IDX)
      *    標準負担額減額開始日
           IF      TNKRRK-SKJNINSTYMD  NOT =   SPACE
               MOVE    TNKRRK-SKJNINSTYMD  TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE    TO  PTMODRES-TNKRRK-SKJNINSTYMD
                                                                  (IDX)
           END-IF
      *    老齢福祉年金受給者証区分
           MOVE    TNKRRK-RRI-FUKUSHI-KBN
                                       TO  PTMODRES-TNKRRK-FUKUSHI-KBN
                                                                  (IDX)
      *    認定範囲区分
           MOVE    TNKRRK-HKNTEKKBN    TO  PTMODRES-TNKRRK-HKNTEKKBN
                                                                  (IDX)
      *    認定範囲区分
      *    低所得者１　認定範囲（画面）
           MOVE    TNKRRK-HKNTEKKBN    TO  WRK-HKNTEKKBN
           PERFORM 200222-TNKRRK-HKNTEKKBN-SEC
           IF      FLG-ERR                 =   ZERO
               MOVE    WRK-HKNTEKKBN-NAME
                                        TO  PTMODRES-TNKRRK-HKNTEK-NAME
                                                                  (IDX)
           END-IF
      *H29.6
      *    境界層該当区分
           IF      TNKRRK-KYOUKAISOUKBN    =   SPACE
               MOVE    "0"             TO  TNKRRK-KYOUKAISOUKBN
           END-IF
           MOVE    TNKRRK-KYOUKAISOUKBN
                                       TO  PTMODRES-TNKRRK-KYOUKAISOU
                                                                  (IDX)
           MOVE    TNKRRK-KYOUKAISOUKBN    TO  WRK-KYOUKAI
           PERFORM 41003-KYOUKAI-HEN-SEC
           MOVE    WRK-KYOUKAI-MEI     TO  PTMODRES-TNKRRK-KYOUKAI-NAME
                                                                  (IDX)
      *
           .
       20014-TNKRRK-PTMODRES-EXT.
           EXIT.
      *
      *****************************************************************
      *    境界層該当区分編集処理
      *****************************************************************
       41003-KYOUKAI-HEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-KYOUKAI-MEI
           EVALUATE    WRK-KYOUKAI
               WHEN    "0"
                   MOVE    "境界層非該当"  TO  WRK-KYOUKAI-MEI
               WHEN    "1"
                   MOVE    "境界層該当"    TO  WRK-KYOUKAI-MEI
           END-EVALUATE
           .
       41003-KYOUKAI-HEN-EXT.
           EXIT.
      *
      ****************************************************************
      *    旧姓履歴編集処理
      ****************************************************************
       20015-KYUSEIRRK-HENSYU-SEC           SECTION.
      *
           INITIALIZE                  PTMODRES-KYUSEIRRK-INFO-G
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   20   )
                          OR  (IDX         >   SPA-NAI25-KYU-MAX )
               MOVE    SPA-GMN25-KYU-CHGYMD(IDX)
                                           TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  PTMODRES-KYUSEI-CHGYMD
                                                                  (IDX)
               MOVE    SPA-GMN25-KYU-KANANAME (IDX)
                                           TO  PTMODRES-KYUSEI-KANANAME
                                                                (IDX)
               MOVE    SPA-GMN25-KYU-NAME (IDX)
                                           TO  PTMODRES-KYUSEI-NAME
                                                                (IDX)
               MOVE    SPA-GMN25-KYU-NICKNAME (IDX)
                                           TO  PTMODRES-KYUSEI-NICKNAME
                                                                (IDX)
           END-PERFORM
      *
           .
       20015-KYUSEIRRK-HENSYU-EXT.
           EXIT.
      *
      ****************************************************************
      *    特記事項情報編集処理
      ****************************************************************
       20016-PTTOKKI-HENSYU-SEC           SECTION.
      *
           INITIALIZE                  PTMODRES-PTTOKKI-INFO-G
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTTOKKI"           TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL   (FLG-PARA       =   1 )
                       OR  (IDX            >=  50)
               MOVE    PARA-DATA-REC       TO  PTTOKKI-REC
      *
               ADD     1                   TO  IDX
               PERFORM 20016-PTTOKKI-PTMODRES-SEC
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       20016-PTTOKKI-HENSYU-EXT.
           EXIT.
      *
      ****************************************************************
      *    特記事項編集処理
      **************************************************************
       20016-PTTOKKI-PTMODRES-SEC           SECTION.
      *
      *    連番
      *    MOVE    PTTOKKI-RENNUM      TO  PTMODRES-PTTOKKI-RENNUM
      *                                                          (IDX)
      *    入外区分
           MOVE    PTTOKKI-NYUGAIKBN   TO  PTMODRES-PTTOKKI-NYUGAIKBN
                                                                 (IDX)
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX-N   FROM    1   BY  1
                   UNTIL      (IDX-N   >   SPA-GMN26-NYUMAX )
                          OR  (FLG-OK  =   1                )
               IF      PTTOKKI-NYUGAIKBN
                                       =   SPA-GMN26-NYUGAI-LST
                                                      (IDX-N) (1:1)
                   MOVE    SPA-GMN26-NYUGAI-LST (IDX-N)(3:)
                                       TO  PTMODRES-PTTOKKI-NYUGAI-NAME
                                                           (IDX)
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
      *
      *    レセ電区分
           MOVE    PTTOKKI-CD          TO  PTMODRES-PTTOKKI-CD (IDX)
      *    紙レセプト（特記区分内容）
           MOVE    PTTOKKI-NAIYO       TO  PTMODRES-PTTOKKI-NAIYO
                                                                 (IDX)
      *    開始年月
           MOVE    PTTOKKI-STYM        TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SDD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE-1      TO  PTMODRES-PTTOKKI-STYM (IDX)
      *    終了年月
           MOVE    PTTOKKI-EDYM        TO  WRK-SYMD(1:6)
           MOVE    "99"                TO  WRK-SDD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE-1      TO  PTMODRES-PTTOKKI-EDYM (IDX)
      *
           .
       20016-PTTOKKI-PTMODRES-EXT.
           EXIT.
      *****************************************************************
      *    患者個別情報編集処理
      *****************************************************************
       20017-PTCONF-HENSYU-SEC             SECTION.
      *
      *    出生時体重
           IF      SPA-GMN-BIRTHWEIGHT-X   =   SPACE
                                           OR  ZERO
               MOVE    SPACE                   TO  PTMODRES-BIRTHWEIGHT
           ELSE
               MOVE    SPA-GMN-BIRTHWEIGHT-X   TO  PTMODRES-BIRTHWEIGHT
           END-IF
      *    地域包括診療対象疾病
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
               IF      SPA-GMN28-CHKHKTBYO(IDX)    =   "1"
                   MOVE    "True"          TO  PTMODRES-CHKHKTBYO
                                                              (IDX)
               ELSE
                   MOVE    "False"         TO  PTMODRES-CHKHKTBYO
                                                              (IDX)
               END-IF
           END-PERFORM
      *    認知症地域包括診療
           IF     SPA-GMN28-NNCHIKBN   =   "1"
               MOVE    "True"          TO  PTMODRES-NINTIHOKATU
           ELSE
               MOVE    "False"         TO  PTMODRES-NINTIHOKATU
           END-IF
      *    小児かかりつけ診療
           IF     SPA-GMN28-SYOUNIKBN  =   "1"
               MOVE    "True"          TO  PTMODRES-SYOUNIKAKARI
           ELSE
               MOVE    "False"         TO  PTMODRES-SYOUNIKAKARI
           END-IF
      *
      *H30.4
      *    妊婦設定
           IF     SPA-GMN28-NINPUKBN   =   "1"
               MOVE    "True"          TO  PTMODRES-NINPUKBN
           ELSE
               MOVE    "False"         TO  PTMODRES-NINPUKBN
           END-IF
      *
           .
       20017-PTCONF-HENSYU-EXT.
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
      *    低所得者２情報（低所得者履歴）
           EVALUATE    PTMODREQ-TSYRRK-MODE
               WHEN    "DELETE"
               WHEN    "Delete"
                   CONTINUE
               WHEN    "MODIFY"
               WHEN    "Modify"
                   PERFORM 200212-TSYRRK-CHK-SEC
               WHEN    SPACE
                   CONTINUE
               WHEN    OTHER
                   ADD     1               TO  IDX-ERR
                   MOVE    "20"            TO  WRK-ERRCD-T (IDX-ERR)
           END-EVALUATE
      *
      *    低所得者１情報（年金履歴）
           EVALUATE    PTMODREQ-TNKRRK-MODE
               WHEN    "DELETE"
               WHEN    "Delete"
                   CONTINUE
               WHEN    "MODIFY"
               WHEN    "Modify"
                   PERFORM 200222-TNKRRK-CHK-SEC
               WHEN    SPACE
                   CONTINUE
               WHEN    OTHER
                   ADD     1               TO  IDX-ERR
                   MOVE    "30"            TO  WRK-ERRCD-T (IDX-ERR)
           END-EVALUATE
      *
      *    旧姓履歴処理
           EVALUATE    PTMODREQ-KYUSEI-MODE
               WHEN    "DELETE"
               WHEN    "Delete"
                   CONTINUE
               WHEN    "MODIFY"
               WHEN    "Modify"
                   PERFORM 200232-KYUSEI-CHK-SEC
               WHEN    SPACE
                   CONTINUE
               WHEN    OTHER
                   ADD     1               TO  IDX-ERR
                   MOVE    "40"            TO  WRK-ERRCD-T (IDX-ERR)
           END-EVALUATE
      *
      *    特記事項
           EVALUATE    PTMODREQ-PTTOKKI-MODE
               WHEN    "DELETE"
               WHEN    "Delete"
                   CONTINUE
               WHEN    "MODIFY"
               WHEN    "Modify"
                   PERFORM 200242-PTTOKKI-CHK-SEC
               WHEN    SPACE
                   CONTINUE
               WHEN    OTHER
                   ADD     1               TO  IDX-ERR
                   MOVE    "50"            TO  WRK-ERRCD-T (IDX-ERR)
           END-EVALUATE
      *
      *    患者個別情報
           EVALUATE    PTMODREQ-PTCONF-MODE
               WHEN    "DELETE"
               WHEN    "Delete"
                   CONTINUE
               WHEN    "MODIFY"
               WHEN    "Modify"
                   PERFORM 200252-PTCONF-CHK-SEC
               WHEN    SPACE
                   CONTINUE
               WHEN    OTHER
                   ADD     1               TO  IDX-ERR
                   MOVE    "60"            TO  WRK-ERRCD-T (IDX-ERR)
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
      *    低所得者２情報（低所得者履歴）
           EVALUATE    PTMODREQ-TSYRRK-MODE
               WHEN    "DELETE"
               WHEN    "Delete"
                   PERFORM 200221-TSYRRK-DELETE-SEC
               WHEN    "MODIFY"
               WHEN    "Modify"
                   PERFORM 200221-TSYRRK-UPDATE-SEC
           END-EVALUATE
      *
      *    低所得者１情報（年金履歴）
           EVALUATE    PTMODREQ-TNKRRK-MODE
               WHEN    "DELETE"
               WHEN    "Delete"
                   PERFORM 200223-TNKRRK-DELETE-SEC
               WHEN    "MODIFY"
               WHEN    "Modify"
                   PERFORM 200224-TNKRRK-UPDATE-SEC
           END-EVALUATE
      *
      *    旧姓履歴処理
           EVALUATE    PTMODREQ-KYUSEI-MODE
               WHEN    "DELETE"
               WHEN    "Delete"
                   PERFORM 200225-KYUSEI-DELETE-SEC
               WHEN    "MODIFY"
               WHEN    "Modify"
                   PERFORM 200226-KYUSEI-UPDATE-SEC
           END-EVALUATE
      *
      *    特記事項
           EVALUATE    PTMODREQ-PTTOKKI-MODE
               WHEN    "DELETE"
               WHEN    "Delete"
                   PERFORM 200227-PTTOKKI-DELETE-SEC
               WHEN    "MODIFY"
               WHEN    "Modify"
                   PERFORM 200228-PTTOKKI-UPDATE-SEC
           END-EVALUATE
      *
      *    患者個別情報
           EVALUATE    PTMODREQ-PTCONF-MODE
               WHEN    "DELETE"
               WHEN    "Delete"
                   PERFORM 200252-PTCONF-DELETE-SEC
               WHEN    "MODIFY"
               WHEN    "Modify"
                   PERFORM 200252-PTCONF-UPDATE-SEC
           END-EVALUATE
      *
           .
       20022-KOUSIN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    低所得者２情報（低所得者履歴）チェック処理
      *****************************************************************
       200212-TSYRRK-CHK-SEC         SECTION.
      *
           INITIALIZE                  TBL-TSYRRK-AREA
           MOVE    ZERO                TO  FLG-ERR-01
      *    送信内容チェック
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   20   )
               MOVE    PTMODREQ-TSYRRK-NINSTYMD (IDX)
                                       TO  PTMODRES-TSYRRK-NINSTYMD
                                                                (IDX)
               MOVE    PTMODREQ-TSYRRK-NINEDYMD (IDX)
                                       TO  PTMODRES-TSYRRK-NINEDYMD
                                                                (IDX)
               MOVE    PTMODREQ-TSYRRK-SKJNINSTYMD (IDX)
                                       TO  PTMODRES-TSYRRK-SKJNINSTYMD
                                                                (IDX)
               MOVE    PTMODREQ-TSYRRK-SKJGNSTYMD (IDX)
                                       TO  PTMODRES-TSYRRK-SKJGNSTYMD
                                                                (IDX)
               MOVE    PTMODREQ-TSYRRK-HKNTEKKBN (IDX)
                                       TO  PTMODRES-TSYRRK-HKNTEKKBN
                                                                (IDX)
               MOVE    PTMODREQ-TSYRRK-KYOUKAISOU (IDX)
                                       TO  PTMODRES-TSYRRK-KYOUKAISOU
                                                                (IDX)
      *        認定開始年月日
               IF      PTMODREQ-TSYRRK-NINSTYMD (IDX)  =   SPACE
                   CONTINUE
               ELSE
                   MOVE    PTMODREQ-TSYRRK-NINSTYMD(IDX)
                                           TO  WRK-HEN-DATE
                   MOVE    SPACE           TO  WRK-HEN-TIME
                   PERFORM 802-DAYHEN02-SEC
                   MOVE    WRK-SYMD        TO  TBL-TSYRRK-NINSTYMD
                                                         (IDX)
               END-IF
      *        認定終了年月日
               IF      PTMODREQ-TSYRRK-NINEDYMD (IDX)  =   SPACE
                   CONTINUE
               ELSE
                   MOVE    PTMODREQ-TSYRRK-NINEDYMD(IDX)
                                           TO  WRK-HEN-DATE
                   MOVE    SPACE           TO  WRK-HEN-TIME
                   PERFORM 802-DAYHEN02-SEC
                   MOVE    WRK-SYMD        TO  TBL-TSYRRK-NINEDYMD
                                                         (IDX)
               END-IF
      *        食事標準負担額減額開始日
               IF      PTMODREQ-TSYRRK-SKJNINSTYMD (IDX)  =   SPACE
                   CONTINUE
               ELSE
                   MOVE    PTMODREQ-TSYRRK-SKJNINSTYMD(IDX)
                                           TO  WRK-HEN-DATE
                   MOVE    SPACE           TO  WRK-HEN-TIME
                   PERFORM 802-DAYHEN02-SEC
                   MOVE    WRK-SYMD        TO  TBL-TSYRRK-SKJNINSTYMD
                                                         (IDX)
               END-IF
      *        長期入院食事減額開始日
               IF      PTMODREQ-TSYRRK-SKJGNSTYMD (IDX)  =   SPACE
                   CONTINUE
               ELSE
                   MOVE    PTMODREQ-TSYRRK-SKJGNSTYMD(IDX)
                                           TO  WRK-HEN-DATE
                   MOVE    SPACE           TO  WRK-HEN-TIME
                   PERFORM 802-DAYHEN02-SEC
                   MOVE    WRK-SYMD        TO  TBL-TSYRRK-SKJGNSTYMD
                                                         (IDX)
               END-IF
      *        認定範囲区分
               MOVE    PTMODREQ-TSYRRK-HKNTEKKBN(IDX)
                                           TO  TBL-TSYRRK-HKNTEKKBN(IDX)
      *H29.6   境界層該当区分
               MOVE    PTMODREQ-TSYRRK-KYOUKAISOU(IDX)
                                           TO  TBL-TSYRRK-KYOUKAISOUKBN
                                                                (IDX)
      *        必須項目チェック（認定開始年月日）
               IF      PTMODREQ-TSYRRK-NINSTYMD (IDX)  =   SPACE
      *            他項目は空白であること
                   IF     (PTMODREQ-TSYRRK-NINEDYMD (IDX)  =   SPACE )
                      AND (PTMODREQ-TSYRRK-SKJNINSTYMD (IDX)
                                                           =   SPACE )
                      AND (PTMODREQ-TSYRRK-SKJGNSTYMD (IDX)
                                                           =   SPACE )
                      AND (PTMODREQ-TSYRRK-HKNTEKKBN (IDX)
                                                           =   SPACE )
                       CONTINUE
                   ELSE
                       MOVE    "2411"          TO  PTMODRES-TSYRRK-ERRCD
                                                                   (IDX)
                       MOVE    1               TO  FLG-ERR-01
                   END-IF
                END-IF
           END-PERFORM
      *
      *    基本チェック
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   20   )
                        OR    (TBL-TSYRRK-NINSTYMD (IDX)   =   SPACE )
      *        開始日
               MOVE    TBL-TSYRRK-NINSTYMD (IDX)   TO  WRK-SYMD
               PERFORM 803-ORCSDAY-CHEK-SEC
               IF     (FLG-DAYERR          =   1   )
                 AND  (PTMODRES-TSYRRK-ERRCD (IDX) =   SPACE )
                   MOVE    "2402"          TO  PTMODRES-TSYRRK-ERRCD
                                                                (IDX)
                   MOVE    1               TO  FLG-ERR-01
               END-IF
      *        認定終了年月日
               IF      TBL-TSYRRK-NINEDYMD (IDX)   =   SPACE
                   MOVE    "99999999"          TO  TBL-TSYRRK-NINEDYMD
                                                                 (IDX)
                   MOVE    TBL-TSYRRK-NINEDYMD (IDX)
                                               TO  WRK-SYMD
                   MOVE    ZERO                TO  WRK-THMS
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE    TO  PTMODRES-TSYRRK-NINEDYMD
                                                                (IDX)
               ELSE
                   MOVE    TBL-TSYRRK-NINEDYMD (IDX)   TO  WRK-SYMD
                   PERFORM 803-ORCSDAY-CHEK-SEC
                   IF     (FLG-DAYERR          =   1   )
                     AND  (PTMODRES-TSYRRK-ERRCD (IDX) =   SPACE )
                       MOVE    "2403"          TO  PTMODRES-TSYRRK-ERRCD
                                                                (IDX)
                       MOVE    1               TO  FLG-ERR-01
                   END-IF
               END-IF
      *        開始日＞終了日　エラー
               IF      TBL-TSYRRK-NINSTYMD (IDX)
                                           >  TBL-TSYRRK-NINEDYMD (IDX)
                   IF      (PTMODRES-TSYRRK-ERRCD (IDX) =   SPACE )
                       MOVE    "2404"          TO  PTMODRES-TSYRRK-ERRCD
                                                                (IDX)
                       MOVE    1               TO  FLG-ERR-01
                   END-IF
               END-IF
      *        標準負担額減額開始日
               IF      TBL-TSYRRK-SKJNINSTYMD (IDX)  =   SPACE
                   CONTINUE
               ELSE
                   MOVE    TBL-TSYRRK-SKJNINSTYMD (IDX)  TO  WRK-SYMD
                   PERFORM 803-ORCSDAY-CHEK-SEC
                   IF     (FLG-DAYERR          =   1   )
                     AND  (PTMODRES-TSYRRK-ERRCD (IDX) =   SPACE )
                       MOVE    "2418"          TO  PTMODRES-TSYRRK-ERRCD
                                                                 (IDX)
                       MOVE    1               TO  FLG-ERR-01
                   END-IF
      *            期間内チェック
                   IF     (FLG-DAYERR          =    ZERO   )
                     AND  (TBL-TSYRRK-NINSTYMD (IDX)
                                       <=  TBL-TSYRRK-SKJNINSTYMD(IDX))
                     AND  (TBL-TSYRRK-NINEDYMD (IDX)
                                       >=  TBL-TSYRRK-SKJNINSTYMD(IDX))
                       CONTINUE
                   ELSE
                       IF     (PTMODRES-TSYRRK-ERRCD (IDX) =   SPACE )
                           MOVE    "2419"      TO  PTMODRES-TSYRRK-ERRCD
                                                                 (IDX)
                           MOVE    1               TO  FLG-ERR-01
                       END-IF
                   END-IF
               END-IF
      *        長期入院該当年月日
               IF      TBL-TSYRRK-SKJGNSTYMD (IDX)  =   SPACE
                   CONTINUE
               ELSE
                   MOVE    TBL-TSYRRK-SKJGNSTYMD(IDX)  TO  WRK-SYMD
                   PERFORM 803-ORCSDAY-CHEK-SEC
                   IF     (FLG-DAYERR          =   1   )
                     AND  (PTMODRES-TSYRRK-ERRCD (IDX) =   SPACE )
                       MOVE    "2413"          TO  PTMODRES-TSYRRK-ERRCD
                                                                 (IDX)
                       MOVE    1               TO  FLG-ERR-01
                   END-IF
      *            期間内チェック
                   IF     (FLG-DAYERR          =    ZERO   )
                     AND  (TBL-TSYRRK-NINSTYMD (IDX)
                                       <=  TBL-TSYRRK-SKJGNSTYMD(IDX))
                     AND  (TBL-TSYRRK-NINEDYMD (IDX)
                                       >=  TBL-TSYRRK-SKJGNSTYMD(IDX))
                       CONTINUE
                   ELSE
                       IF     (PTMODRES-TSYRRK-ERRCD (IDX) =   SPACE )
                           MOVE    "2414"      TO  PTMODRES-TSYRRK-ERRCD
                                                                 (IDX)
                           MOVE    1               TO  FLG-ERR-01
                       END-IF
                   END-IF
                   IF     (PTMODRES-TSYRRK-ERRCD (IDX) =   SPACE )
      *                長期入院該当日＜減額開始日はエラー
                       IF     (TBL-TSYRRK-SKJGNSTYMD(IDX)
                                               <  TBL-TSYRRK-SKJNINSTYMD
                                                               (IDX))
                         OR   (TBL-TSYRRK-SKJNINSTYMD (IDX)
                                               =   SPACE   )
                           MOVE    "2423"      TO  PTMODRES-TSYRRK-ERRCD
                                                                 (IDX)
                           MOVE    1           TO  FLG-ERR-01
                       END-IF
                   END-IF
               END-IF
      *        認定範囲区分
               MOVE    TBL-TSYRRK-HKNTEKKBN(IDX)   TO  WRK-HKNTEKKBN
               PERFORM 200212-TSYRRK-HKNTEKKBN-SEC
               IF      FLG-ERR                 =   ZERO
                   MOVE    WRK-HKNTEKKBN-NAME
                                       TO  PTMODRES-TSYRRK-HKNTEK-NAME
                                                                 (IDX)
               ELSE
                   IF      PTMODRES-TSYRRK-ERRCD (IDX) =   SPACE
                       MOVE    "2417"          TO  PTMODRES-TSYRRK-ERRCD
                                                                 (IDX)
                       MOVE    1               TO  FLG-ERR-01
                   END-IF
               END-IF
      *H29.6   境界層該当区分
               IF      TBL-TSYRRK-KYOUKAISOUKBN(IDX)   =   SPACE
                   MOVE    "0"             TO  TBL-TSYRRK-KYOUKAISOUKBN
                                                                 (IDX)
                   MOVE    "0"         TO  PTMODRES-TSYRRK-KYOUKAISOU
                                                                  (IDX)
               END-IF
               MOVE    TBL-TSYRRK-KYOUKAISOUKBN(IDX)   TO  WRK-KYOUKAI
               PERFORM 41003-KYOUKAI-HEN-SEC
               IF      WRK-KYOUKAI-MEI     =   SPACE
      *            エラー
                   IF      PTMODRES-TSYRRK-ERRCD (IDX) =   SPACE
                       MOVE    "2424"          TO  PTMODRES-TSYRRK-ERRCD
                                                                 (IDX)
                       MOVE    1               TO  FLG-ERR-01
                   END-IF
               ELSE
                   MOVE    WRK-KYOUKAI-MEI     TO
                                           PTMODRES-TSYRRK-KYOUKAI-NAME
                                                                 (IDX)
               END-IF
           END-PERFORM
      *
      *    期間重複チェック
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   20   )
                        OR    (FLG-ERR-01  NOT =   ZERO  )
                        OR    (TBL-TSYRRK-NINSTYMD (IDX)   =   SPACE )
               COMPUTE IDX2            =   IDX     +  1
               PERFORM VARYING     IDX3    FROM    IDX2    BY  1
                           UNTIL  (IDX3        >   20   )
                            OR    (FLG-ERR-01  NOT =   ZERO  )
                            OR    (TBL-TSYRRK-NINSTYMD (IDX3)
                                                   =   SPACE )
                   IF      (TBL-TSYRRK-NINEDYMD (IDX3)
                                               <   TBL-TSYRRK-NINSTYMD
                                                             (IDX) )
                      OR   (TBL-TSYRRK-NINSTYMD (IDX3)
                                               >   TBL-TSYRRK-NINEDYMD
                                                             (IDX) )
                       CONTINUE
                   ELSE
                       MOVE    "2405"          TO  PTMODRES-TSYRRK-ERRCD
                                                                 (IDX)
                       MOVE    1               TO  FLG-ERR-01
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    エラーメッセージ編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                      UNTIL  (IDX         >   20   )
               IF      PTMODRES-TSYRRK-ERRCD (IDX) NOT =  SPACE
                   MOVE    PTMODRES-TSYRRK-ERRCD (IDX) TO SPA-ERRCD
                   PERFORM 8901-PTERRCD-SET-SEC
                   MOVE    WRK-ERRMSG     TO  PTMODRES-TSYRRK-ERRCD-MSG
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
           IF     (TBL-TSYRRK-NINSTYMD (1) =   SPACE )
             AND  (FLG-ERR-01              =   ZERO  )
               ADD     1               TO  IDX-ERR
               MOVE    "22"            TO  WRK-ERRCD-T(IDX-ERR)
           END-IF
           .
       200212-TSYRRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    認定範囲区分編集処理
      *****************************************************************
       200212-TSYRRK-HKNTEKKBN-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-ERR
           MOVE    SPACE               TO  WRK-HKNTEKKBN-NAME-G
           PERFORM VARYING     IDF     FROM    1   BY  1
                   UNTIL       IDF     >   SPA-GMN24-TSYTEKKBN-MAX
               IF      WRK-HKNTEKKBN   =   SPA-GMN24-TSYTEKKBN-LST(IDF)
                                                              (1:1)
                   MOVE    SPA-GMN24-TSYTEKKBN-LST(IDF)
                                            TO  WRK-HKNTEKKBN-NAME-G
                   MOVE    1                TO  FLG-OK
                   MOVE    SPA-GMN24-TSYTEKKBN-MAX   TO  IDF
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    1               TO  FLG-ERR
           END-IF
           .
       200212-TSYRRK-HKNTEKKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    認定範囲区分編集処理
      *****************************************************************
       200222-TNKRRK-HKNTEKKBN-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-ERR
           MOVE    SPACE               TO  WRK-HKNTEKKBN-NAME-G
           PERFORM VARYING     IDF     FROM    1   BY  1
                   UNTIL       IDF     >   SPA-GMN24-TNKTEKKBN-MAX
               IF      WRK-HKNTEKKBN   =   SPA-GMN24-TNKTEKKBN-LST(IDF)
                                                              (1:1)
                   MOVE    SPA-GMN24-TNKTEKKBN-LST(IDF)
                                            TO  WRK-HKNTEKKBN-NAME-G
                   MOVE    1                TO  FLG-OK
                   MOVE    SPA-GMN24-TNKTEKKBN-MAX   TO  IDF
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    1               TO  FLG-ERR
           END-IF
           .
       200222-TNKRRK-HKNTEKKBN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    低所得者２情報（低所得者履歴）削除処理
      *****************************************************************
       200221-TSYRRK-DELETE-SEC         SECTION.
      *
      *    低所得者　削除
           INITIALIZE                      TSYRRK-REC
           MOVE    SPA-HOSPNUM         TO  TSYRRK-HOSPNUM
           MOVE    SPA-PTID            TO  TSYRRK-PTID
           MOVE    TSYRRK-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE   "62"             TO  WRK-ERRCD
           END-IF
      *
           .
       200221-TSYRRK-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得者２情報（低所得者履歴）更新処理
      *****************************************************************
       200221-TSYRRK-UPDATE-SEC         SECTION.
      *
      *    低所得者２情報（低所得者履歴）削除処理
           PERFORM 200221-TSYRRK-DELETE-SEC
      *    追加処理
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   20   )
                        OR    (TBL-TSYRRK-NINSTYMD (IDX)   =   SPACE )
                        OR    (WRK-ERRCD   NOT =   SPACE )
               MOVE    TBL-TSYRRK-REC (IDX)    TO  TSYRRK-REC
               MOVE    SPA-HOSPNUM         TO  TSYRRK-HOSPNUM
               MOVE    SPA-PTID            TO  TSYRRK-PTID
      *
               MOVE    SMCNDATE-YMD        TO  TSYRRK-CREYMD
               MOVE    SMCNDATE-YMD        TO  TSYRRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  TSYRRK-UPHMS
      *
               MOVE    WRK-OPID            TO  TSYRRK-OPID
      *
               MOVE    TSYRRK-REC          TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
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
       200221-TSYRRK-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得者１情報（年金履歴）チェック処理
      *****************************************************************
       200222-TNKRRK-CHK-SEC         SECTION.
      *
           INITIALIZE                  TBL-TNKRRK-AREA
           MOVE    ZERO                TO  FLG-ERR-01
      *    送信内容チェック
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   20   )
               MOVE    PTMODREQ-TNKRRK-STYMD (IDX)
                                       TO  PTMODRES-TNKRRK-STYMD
                                                                (IDX)
               MOVE    PTMODREQ-TNKRRK-EDYMD (IDX)
                                       TO  PTMODRES-TNKRRK-EDYMD
                                                                (IDX)
               MOVE    PTMODREQ-TNKRRK-SKJNINSTYMD (IDX)
                                       TO  PTMODRES-TNKRRK-SKJNINSTYMD
                                                                (IDX)
               MOVE    PTMODREQ-TNKRRK-FUKUSHI-KBN (IDX)
                                       TO  PTMODRES-TNKRRK-FUKUSHI-KBN
                                                                (IDX)
               MOVE    PTMODREQ-TNKRRK-HKNTEKKBN (IDX)
                                       TO  PTMODRES-TNKRRK-HKNTEKKBN
                                                                (IDX)
               MOVE    PTMODREQ-TNKRRK-KYOUKAISOU (IDX)
                                       TO  PTMODRES-TNKRRK-KYOUKAISOU
                                                                (IDX)
      *        認定開始年月日
               IF      PTMODREQ-TNKRRK-STYMD (IDX)  =   SPACE
                   CONTINUE
               ELSE
                   MOVE    PTMODREQ-TNKRRK-STYMD(IDX)
                                           TO  WRK-HEN-DATE
                   MOVE    SPACE           TO  WRK-HEN-TIME
                   PERFORM 802-DAYHEN02-SEC
                   MOVE    WRK-SYMD        TO  TBL-TNKRRK-STYMD
                                                         (IDX)
               END-IF
      *        認定終了年月日
               IF      PTMODREQ-TNKRRK-EDYMD (IDX)  =   SPACE
                   CONTINUE
               ELSE
                   MOVE    PTMODREQ-TNKRRK-EDYMD(IDX)
                                           TO  WRK-HEN-DATE
                   MOVE    SPACE           TO  WRK-HEN-TIME
                   PERFORM 802-DAYHEN02-SEC
                   MOVE    WRK-SYMD        TO  TBL-TNKRRK-EDYMD
                                                         (IDX)
               END-IF
      *        食事標準負担額減額開始日
               IF      PTMODREQ-TNKRRK-SKJNINSTYMD (IDX)  =   SPACE
                   CONTINUE
               ELSE
                   MOVE    PTMODREQ-TNKRRK-SKJNINSTYMD(IDX)
                                           TO  WRK-HEN-DATE
                   MOVE    SPACE           TO  WRK-HEN-TIME
                   PERFORM 802-DAYHEN02-SEC
                   MOVE    WRK-SYMD        TO  TBL-TNKRRK-SKJNINSTYMD
                                                         (IDX)
               END-IF
      *        老齢福祉年金受給者証区分
               MOVE    PTMODREQ-TNKRRK-FUKUSHI-KBN (IDX) 
                                       TO  TBL-TNKRRK-RRI-FUKUSHI-KBN
                                                         (IDX)
      *        認定範囲区分
               MOVE    PTMODREQ-TNKRRK-HKNTEKKBN(IDX)
                                           TO  TBL-TNKRRK-HKNTEKKBN(IDX)
      *H29.6   境界層該当区分
               MOVE    PTMODREQ-TNKRRK-KYOUKAISOU(IDX)
                                           TO  TBL-TNKRRK-KYOUKAISOUKBN
                                                                 (IDX)
      *        必須項目チェック（認定開始年月日）
               IF      PTMODREQ-TNKRRK-STYMD (IDX)  =   SPACE
      *            他項目は空白であること
                   IF     (PTMODREQ-TNKRRK-EDYMD (IDX)  =   SPACE )
                      AND (PTMODREQ-TNKRRK-SKJNINSTYMD (IDX)
                                                           =   SPACE )
                      AND (PTMODREQ-TNKRRK-FUKUSHI-KBN (IDX)
                                                           =   SPACE )
                      AND (PTMODREQ-TNKRRK-HKNTEKKBN (IDX)
                                                           =   SPACE )
                       CONTINUE
                   ELSE
                       MOVE    "2412"          TO  PTMODRES-TNKRRK-ERRCD
                                                                   (IDX)
                       MOVE    1               TO  FLG-ERR-01
                   END-IF
                END-IF
           END-PERFORM
      *
      *    基本チェック
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   20   )
                        OR    (TBL-TNKRRK-STYMD (IDX)   =   SPACE )
      *        開始日
               MOVE    TBL-TNKRRK-STYMD (IDX)   TO  WRK-SYMD
               PERFORM 803-ORCSDAY-CHEK-SEC
               IF     (FLG-DAYERR          =   1   )
                 AND  (PTMODRES-TNKRRK-ERRCD (IDX) =   SPACE )
                   MOVE    "2407"          TO  PTMODRES-TNKRRK-ERRCD
                                                                (IDX)
                   MOVE    1               TO  FLG-ERR-01
               END-IF
      *        認定終了年月日
               IF      TBL-TNKRRK-EDYMD (IDX)   =   SPACE
                   MOVE    "99999999"          TO  TBL-TNKRRK-EDYMD
                                                                 (IDX)
                   MOVE    TBL-TNKRRK-EDYMD(IDX)
                                               TO  WRK-SYMD
                   MOVE    ZERO                TO  WRK-THMS
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE        TO  PTMODRES-TNKRRK-EDYMD
                                                                (IDX)
               ELSE
                   MOVE    TBL-TNKRRK-EDYMD (IDX)   TO  WRK-SYMD
                   PERFORM 803-ORCSDAY-CHEK-SEC
                   IF     (FLG-DAYERR          =   1   )
                     AND  (PTMODRES-TNKRRK-ERRCD (IDX) =   SPACE )
                       MOVE    "2408"          TO  PTMODRES-TNKRRK-ERRCD
                                                                (IDX)
                       MOVE    1               TO  FLG-ERR-01
                   END-IF
               END-IF
      *        開始日＞終了日　エラー
               IF      TBL-TNKRRK-STYMD (IDX)
                                           >  TBL-TNKRRK-EDYMD (IDX)
                   IF      (PTMODRES-TNKRRK-ERRCD (IDX) =   SPACE )
                       MOVE    "2409"          TO  PTMODRES-TNKRRK-ERRCD
                                                                (IDX)
                       MOVE    1               TO  FLG-ERR-01
                   END-IF
               END-IF
      *        標準負担額減額開始日
               IF      TBL-TNKRRK-SKJNINSTYMD (IDX)  =   SPACE
                   CONTINUE
               ELSE
                   MOVE    TBL-TNKRRK-SKJNINSTYMD (IDX)  TO  WRK-SYMD
                   PERFORM 803-ORCSDAY-CHEK-SEC
                   IF     (FLG-DAYERR          =   1   )
                     AND  (PTMODRES-TNKRRK-ERRCD (IDX) =   SPACE )
                       MOVE    "2421"          TO  PTMODRES-TNKRRK-ERRCD
                                                                 (IDX)
                       MOVE    1               TO  FLG-ERR-01
                   END-IF
      *            期間内チェック
                   IF     (FLG-DAYERR          =    ZERO   )
                     AND  (TBL-TNKRRK-STYMD (IDX)
                                       <=  TBL-TNKRRK-SKJNINSTYMD(IDX))
                     AND  (TBL-TNKRRK-EDYMD (IDX)
                                       >=  TBL-TNKRRK-SKJNINSTYMD(IDX))
                       CONTINUE
                   ELSE
                       IF     (PTMODRES-TNKRRK-ERRCD (IDX) =   SPACE )
                           MOVE    "2422"      TO  PTMODRES-TNKRRK-ERRCD
                                                                 (IDX)
                           MOVE    1           TO  FLG-ERR-01
                       END-IF
                   END-IF
               END-IF
      *        老齢福祉年金受給者証区分
               EVALUATE    TBL-TNKRRK-RRI-FUKUSHI-KBN (IDX)
                   WHEN    "0"
                   WHEN    "1"
                       CONTINUE
                   WHEN    OTHER
                       IF     (PTMODRES-TNKRRK-ERRCD (IDX) =   SPACE )
                           MOVE    "2416"      TO  PTMODRES-TNKRRK-ERRCD
                                                                 (IDX)
                           MOVE    1               TO  FLG-ERR-01
                       END-IF
               END-EVALUATE
      *        認定範囲区分
               MOVE    TBL-TNKRRK-HKNTEKKBN(IDX)   TO  WRK-HKNTEKKBN
               PERFORM 200222-TNKRRK-HKNTEKKBN-SEC
               IF      FLG-ERR                 =   ZERO
                   MOVE    WRK-HKNTEKKBN-NAME
                                       TO  PTMODRES-TNKRRK-HKNTEK-NAME
                                                                 (IDX)
               ELSE
                   IF      PTMODRES-TNKRRK-ERRCD (IDX) =   SPACE
                       MOVE    "2420"          TO  PTMODRES-TNKRRK-ERRCD
                                                                 (IDX)
                       MOVE    1               TO  FLG-ERR-01
                   END-IF
               END-IF
      *H29.6   境界層該当区分
               IF      TBL-TNKRRK-KYOUKAISOUKBN(IDX)   =   SPACE
                   MOVE    "0"             TO  TBL-TNKRRK-KYOUKAISOUKBN
                                                                 (IDX)
                   MOVE    "0"         TO  PTMODRES-TNKRRK-KYOUKAISOU
                                                                  (IDX)
               END-IF
               MOVE    TBL-TNKRRK-KYOUKAISOUKBN(IDX)   TO  WRK-KYOUKAI
               PERFORM 41003-KYOUKAI-HEN-SEC
               IF      WRK-KYOUKAI-MEI     =   SPACE
      *            エラー
                   IF      PTMODRES-TNKRRK-ERRCD (IDX) =   SPACE
                       MOVE    "2425"          TO  PTMODRES-TNKRRK-ERRCD
                                                                 (IDX)
                       MOVE    1               TO  FLG-ERR-01
                   END-IF
               ELSE
                   MOVE    WRK-KYOUKAI-MEI     TO
                                           PTMODRES-TNKRRK-KYOUKAI-NAME
                                                                 (IDX)
               END-IF
           END-PERFORM
      *
      *    期間重複チェック
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   20   )
                        OR    (FLG-ERR-01  NOT =   ZERO  )
                        OR    (TBL-TNKRRK-STYMD (IDX)   =   SPACE )
               COMPUTE IDX2            =   IDX     +  1
               PERFORM VARYING     IDX3    FROM    IDX2    BY  1
                           UNTIL  (IDX3        >   20   )
                            OR    (FLG-ERR-01  NOT =   ZERO  )
                            OR    (TBL-TNKRRK-STYMD (IDX3)
                                                   =   SPACE )
                   IF      (TBL-TNKRRK-EDYMD (IDX3)
                                               <   TBL-TNKRRK-STYMD
                                                             (IDX) )  OR
                           (TBL-TNKRRK-STYMD (IDX3)
                                               >   TBL-TNKRRK-EDYMD
                                                             (IDX) )
                       CONTINUE
                   ELSE
                       MOVE    "2410"          TO  PTMODRES-TNKRRK-ERRCD
                                                                 (IDX)
                       MOVE    1               TO  FLG-ERR-01
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    エラーメッセージ編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                      UNTIL   (IDX         >   20   )
              IF      PTMODRES-TNKRRK-ERRCD (IDX) NOT =  SPACE
                   MOVE    PTMODRES-TNKRRK-ERRCD (IDX) TO SPA-ERRCD
                   PERFORM 8901-PTERRCD-SET-SEC
                   MOVE    WRK-ERRMSG     TO  PTMODRES-TNKRRK-ERRCD-MSG
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
           IF     (TBL-TNKRRK-STYMD (1)    =   SPACE )
             AND  (FLG-ERR-01              =   ZERO  )
               ADD     1               TO  IDX-ERR
               MOVE    "32"            TO  WRK-ERRCD-T(IDX-ERR)
           END-IF
           .
       200222-TNKRRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得者１情報（年金履歴）削除処理
      *****************************************************************
       200223-TNKRRK-DELETE-SEC         SECTION.
      *
      *    低所得者　削除
           INITIALIZE                      TNKRRK-REC
           MOVE    SPA-HOSPNUM         TO  TNKRRK-HOSPNUM
           MOVE    SPA-PTID            TO  TNKRRK-PTID
           MOVE    TNKRRK-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE   "64"             TO  WRK-ERRCD
           END-IF
      *
           .
       200223-TNKRRK-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得者１情報（年金履歴）更新処理
      *****************************************************************
       200224-TNKRRK-UPDATE-SEC         SECTION.
      *
      *    低所得者１情報（年金履歴）削除処理
           PERFORM 200223-TNKRRK-DELETE-SEC
      *    追加処理
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   20   )
                        OR    (TBL-TNKRRK-STYMD (IDX)  =   SPACE )
                        OR    (WRK-ERRCD   NOT =   SPACE )
               MOVE    TBL-TNKRRK-REC (IDX)    TO  TNKRRK-REC
               MOVE    SPA-HOSPNUM         TO  TNKRRK-HOSPNUM
               MOVE    SPA-PTID            TO  TNKRRK-PTID
      *
               MOVE    SMCNDATE-YMD        TO  TNKRRK-CREYMD
               MOVE    SMCNDATE-YMD        TO  TNKRRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  TNKRRK-UPHMS
      *
               MOVE    WRK-OPID            TO  TNKRRK-OPID
      *
               MOVE    TNKRRK-REC          TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
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
       200224-TNKRRK-UPDATE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    旧姓履歴チェック処理
      *****************************************************************
       200232-KYUSEI-CHK-SEC         SECTION.
      *
           INITIALIZE                  TBL-KYUSEIRRK-AREA
           MOVE    ZERO                TO  FLG-ERR-01
      *    送信内容チェック
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   20   )
               MOVE    PTMODREQ-KYUSEI-CHGYMD (IDX)
                                       TO  PTMODRES-KYUSEI-CHGYMD
                                                                (IDX)
               MOVE    PTMODREQ-KYUSEI-KANANAME (IDX)
                                       TO  PTMODRES-KYUSEI-KANANAME
                                                                (IDX)
               MOVE    PTMODREQ-KYUSEI-NAME (IDX)
                                       TO  PTMODRES-KYUSEI-NAME
                                                                (IDX)
               MOVE    PTMODREQ-KYUSEI-NICKNAME (IDX)
                                       TO  PTMODRES-KYUSEI-NICKNAME
                                                                (IDX)
      *        変更年月日
               IF      PTMODREQ-KYUSEI-CHGYMD (IDX)  =   SPACE
                   CONTINUE
               ELSE
                   MOVE    PTMODREQ-KYUSEI-CHGYMD(IDX)
                                           TO  WRK-HEN-DATE
                   MOVE    SPACE           TO  WRK-HEN-TIME
                   PERFORM 802-DAYHEN02-SEC
                   MOVE    WRK-SYMD        TO  TBL-KYUSEI-CHGYMD
                                                         (IDX)
               END-IF
      *        旧姓カナ氏名
               MOVE    PTMODREQ-KYUSEI-KANANAME (IDX)
                                           TO  TBL-KYUSEI-KANANAME
                                                            (IDX)
      *        旧姓氏名
               MOVE    PTMODREQ-KYUSEI-NAME (IDX)
                                           TO  TBL-KYUSEI-NAME
                                                            (IDX)
      *        旧姓通称名
               MOVE    PTMODREQ-KYUSEI-NICKNAME (IDX)
                                           TO  TBL-KYUSEI-NICKNAME
                                                            (IDX)
      *        必須項目チェック
               IF      PTMODREQ-KYUSEI-CHGYMD (IDX)    =   SPACE
      *            他項目は空白であること
                   IF     (PTMODREQ-KYUSEI-KANANAME (IDX)  =   SPACE )
                      AND (PTMODREQ-KYUSEI-NAME     (IDX)  =   SPACE )
                       CONTINUE
                   ELSE
                       MOVE    "2503"          TO  PTMODRES-KYUSEI-ERRCD
                                                                   (IDX)
                       MOVE    1               TO  FLG-ERR-01
                   END-IF
                END-IF
           END-PERFORM
      *
      *    基本チェック
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   20   )
                        OR    (TBL-KYUSEI-CHGYMD (IDX)   =   SPACE )
      *        開始日
               MOVE    TBL-KYUSEI-CHGYMD (IDX)   TO  WRK-SYMD
               PERFORM 803-ORCSDAY-CHEK-SEC
               IF     (FLG-DAYERR          =   1   )
                 AND  (PTMODRES-KYUSEI-ERRCD (IDX) =   SPACE )
                   MOVE    "2503"          TO  PTMODRES-KYUSEI-ERRCD
                                                                (IDX)
                   MOVE    1               TO  FLG-ERR-01
               END-IF
      *        旧姓カナ氏名
               MOVE    TBL-KYUSEI-KANANAME (IDX)
                                           TO  WRK-MAE-INPUT
               MOVE    50                  TO  WRK-LEN
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  TBL-KYUSEI-KANANAME
                                                             (IDX)
               IF     (FLG-ERR             =   1  )
                 AND  (PTMODRES-KYUSEI-ERRCD (IDX) =   SPACE )
                   MOVE    "2530"          TO  PTMODRES-KYUSEI-ERRCD
                                                                   (IDX)
                   MOVE    1               TO  FLG-ERR-01
               ELSE
                   MOVE    TBL-KYUSEI-KANANAME (IDX)
                                           TO  PTMODRES-KYUSEI-KANANAME
                                                                   (IDX)
               END-IF
      *        旧姓氏名
               MOVE    TBL-KYUSEI-NAME (IDX)
                                           TO  WRK-MAE-INPUT
               MOVE    50                  TO  WRK-LEN
               PERFORM 891-HALF-ZEN2-SEC
               MOVE    WRK-ATO-INPUT       TO  TBL-KYUSEI-NAME
                                                             (IDX)
               IF     (FLG-ERR             =   1  )
                 AND  (PTMODRES-KYUSEI-ERRCD (IDX) =   SPACE )
                   MOVE    "2530"          TO  PTMODRES-KYUSEI-ERRCD
                                                                   (IDX)
                   MOVE    1               TO  FLG-ERR-01
               ELSE
                   MOVE    TBL-KYUSEI-NAME (IDX)
                                           TO  PTMODRES-KYUSEI-NAME
                                                                   (IDX)
               END-IF
      *        必須チェック
               IF    ((TBL-KYUSEI-KANANAME (IDX)   =   SPACE ) OR
                      (TBL-KYUSEI-NAME (IDX)       =   SPACE ))
                 AND  (PTMODRES-KYUSEI-ERRCD (IDX) =   SPACE )
                   MOVE    "2507"          TO  PTMODRES-KYUSEI-ERRCD
                                                                   (IDX)
                   MOVE    1               TO  FLG-ERR-01
               END-IF
      *        旧姓通称名
               IF      TBL-KYUSEI-NICKNAME (IDX)   NOT =   SPACE
                   MOVE    TBL-KYUSEI-NICKNAME (IDX)
                                           TO  WRK-MAE-INPUT
                   MOVE    50                  TO  WRK-LEN
                   PERFORM 891-HALF-ZEN2-SEC
                   MOVE    WRK-ATO-INPUT       TO  TBL-KYUSEI-NICKNAME
                                                             (IDX)
                   IF      FLG-ERR             =   1
                       MOVE    "2530"          TO  PTMODRES-KYUSEI-ERRCD
                                                                   (IDX)
                       MOVE    1               TO  FLG-ERR-01
                   ELSE
                       MOVE    TBL-KYUSEI-NICKNAME (IDX)
                                           TO  PTMODRES-KYUSEI-NICKNAME
                                                                   (IDX)
                   END-IF
               END-IF
           END-PERFORM
      *
      *    期間重複チェック
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   20   )
                        OR    (FLG-ERR-01  NOT =   ZERO  )
                        OR    (TBL-KYUSEI-CHGYMD (IDX)   =   SPACE )
               COMPUTE IDX2            =   IDX     +  1
               PERFORM VARYING     IDX3    FROM    IDX2    BY  1
                           UNTIL  (IDX3        >   20   )
                            OR    (FLG-ERR-01  NOT =   ZERO  )
                            OR    (TBL-KYUSEI-CHGYMD (IDX3)
                                                   =   SPACE )
      *            同じ日付がないこと
                   IF      (TBL-KYUSEI-CHGYMD  (IDX3)
                                               =   TBL-KYUSEI-CHGYMD
                                                             (IDX) )
                       MOVE    "2517"          TO  PTMODRES-KYUSEI-ERRCD
                                                                 (IDX)
                       MOVE    1               TO  FLG-ERR-01
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    エラーメッセージ編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                      UNTIL  (IDX         >   20   )
               IF      PTMODRES-KYUSEI-ERRCD (IDX) NOT =  SPACE
                   MOVE    PTMODRES-KYUSEI-ERRCD (IDX) TO SPA-ERRCD
                   PERFORM 8901-PTERRCD-SET-SEC
                   MOVE    WRK-ERRMSG     TO  PTMODRES-KYUSEI-ERRCD-MSG
                                                            (IDX)
                   MOVE    SPACE          TO  WRK-ERRMSG
                   MOVE    SPACE          TO  SPA-ERRCD
               END-IF
           END-PERFORM
      *
           IF      FLG-ERR-01      NOT =   ZERO
               ADD    1                TO  IDX-ERR
               MOVE    "41"            TO  WRK-ERRCD-T (IDX-ERR)
           END-IF
      *    対象なし
           IF     (TBL-KYUSEI-CHGYMD (1)   =   SPACE )
             AND  (FLG-ERR-01              =   ZERO  )
               ADD     1               TO  IDX-ERR
               MOVE    "42"            TO  WRK-ERRCD-T(IDX-ERR)
           END-IF
           .
       200232-KYUSEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    旧姓履歴削除処理
      *****************************************************************
       200225-KYUSEI-DELETE-SEC         SECTION.
      *
      *    旧姓履　削除
           INITIALIZE                      KYUSEIRRK-REC
           MOVE    SPA-HOSPNUM         TO  KYUSEI-HOSPNUM
           MOVE    SPA-PTID            TO  KYUSEI-PTID
           MOVE    KYUSEIRRK-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE   "66"             TO  WRK-ERRCD
           END-IF
      *
           .
       200225-KYUSEI-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    旧姓履歴更新処理
      *****************************************************************
       200226-KYUSEI-UPDATE-SEC         SECTION.
      *
      *    旧姓履歴削除処理
           PERFORM 200225-KYUSEI-DELETE-SEC
      *    追加処理
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   20   )
                        OR    (TBL-KYUSEI-CHGYMD (IDX) =   SPACE )
                        OR    (WRK-ERRCD   NOT =   SPACE )
               MOVE    TBL-KYUSEIRRK-REC (IDX) TO  KYUSEIRRK-REC
               MOVE    SPA-HOSPNUM         TO  KYUSEI-HOSPNUM
               MOVE    SPA-PTID            TO  KYUSEI-PTID
      *
               MOVE    SMCNDATE-YMD        TO  KYUSEI-CREYMD
               MOVE    SMCNDATE-YMD        TO  KYUSEI-UPYMD
               MOVE    SMCNDATE-HMS        TO  KYUSEI-UPHMS
      *
               MOVE    WRK-OPID            TO  KYUSEI-OPID
      *
               MOVE    KYUSEIRRK-REC       TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "65"                TO  WRK-ERRCD
               END-IF
           END-PERFORM
      *
           .
       200226-KYUSEI-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    特記事項チェック処理
      *****************************************************************
       200242-PTTOKKI-CHK-SEC         SECTION.
      *
           INITIALIZE                  TBL-PTTOKKI-AREA
           MOVE    ZERO                TO  FLG-ERR-01
      *    送信内容チェック
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   50   )
               MOVE    PTMODREQ-PTTOKKI-NYUGAIKBN (IDX)
                                       TO  PTMODRES-PTTOKKI-NYUGAIKBN
                                                                (IDX)
               MOVE    PTMODREQ-PTTOKKI-CD (IDX)
                                       TO  PTMODRES-PTTOKKI-CD
                                                                (IDX)
               MOVE    PTMODREQ-PTTOKKI-NAIYO (IDX)
                                       TO  PTMODRES-PTTOKKI-NAIYO
                                                                (IDX)
               MOVE    PTMODREQ-PTTOKKI-STYM (IDX)
                                       TO  PTMODRES-PTTOKKI-STYM
                                                                (IDX)
               MOVE    PTMODREQ-PTTOKKI-EDYM (IDX)
                                       TO  PTMODRES-PTTOKKI-EDYM
                                                                (IDX)
      *        開始年月
               IF      PTMODREQ-PTTOKKI-STYM (IDX)  =   SPACE
                   CONTINUE
               ELSE
                   MOVE    SPACE           TO  WRK-HEN-DATE
                   MOVE    PTMODREQ-PTTOKKI-STYM(IDX)
                                           TO  WRK-HEN-DATE-1
                   PERFORM 802-DAYHEN02-SEC
                   MOVE    WRK-SYMD(1:6)   TO  TBL-PTTOKKI-STYM
                                                         (IDX)
               END-IF
      *        終了年月
               IF      PTMODREQ-PTTOKKI-EDYM (IDX)  =   SPACE
                   CONTINUE
               ELSE
                   MOVE    SPACE           TO  WRK-HEN-DATE
                   MOVE    PTMODREQ-PTTOKKI-EDYM(IDX)
                                           TO  WRK-HEN-DATE-1
                   PERFORM 802-DAYHEN02-SEC
                   MOVE    WRK-SYMD(1:6)   TO  TBL-PTTOKKI-EDYM
                                                         (IDX)
               END-IF
      *        入外区分
               MOVE    PTMODREQ-PTTOKKI-NYUGAIKBN (IDX)
                                       TO  TBL-PTTOKKI-NYUGAIKBN
                                                                (IDX)
      *        レセ電区分
               MOVE    PTMODREQ-PTTOKKI-CD (IDX)
                                       TO  TBL-PTTOKKI-CD      (IDX)
      *        紙レセプト（特記区分内容）
               MOVE    PTMODREQ-PTTOKKI-NAIYO (IDX)
                                       TO  TBL-PTTOKKI-NAIYO
                                                                (IDX)
      *        必須項目チェック（開始年月）
               IF      PTMODREQ-PTTOKKI-STYM (IDX)  =   SPACE
      *            他項目は空白であること
                   IF     (PTMODREQ-PTTOKKI-NYUGAIKBN (IDX)  =   SPACE )
                      AND (PTMODREQ-PTTOKKI-CD (IDX)
                                                           =   SPACE )
                      AND (PTMODREQ-PTTOKKI-NAIYO (IDX)
                                                           =   SPACE )
                      AND (PTMODREQ-PTTOKKI-EDYM(IDX)
                                                           =   SPACE )
                       CONTINUE
                   ELSE
                       MOVE    "2609"      TO  PTMODRES-PTTOKKI-ERRCD
                                                                  (IDX)
                       MOVE    1               TO  FLG-ERR-01
                   END-IF
                END-IF
           END-PERFORM
      *
      *    基本チェック
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   50   )
                        OR    (TBL-PTTOKKI-STYM (IDX)   =   SPACE )
      *        開始日
               MOVE    TBL-PTTOKKI-STYM (IDX)  TO  WRK-SYMD(1:6)
               MOVE    "01"                    TO  WRK-SDD
               PERFORM 803-ORCSDAY-CHEK-SEC
               IF     (FLG-DAYERR          =   1   )
                 AND  (PTMODRES-PTTOKKI-ERRCD (IDX) =   SPACE )
                   MOVE    "2605"          TO  PTMODRES-PTTOKKI-ERRCD
                                                                (IDX)
                   MOVE    1               TO  FLG-ERR-01
               END-IF
      *        終了年月
               IF      TBL-PTTOKKI-EDYM(IDX)   =   SPACE
                                               OR  "999999"
                   MOVE    "999999"            TO  TBL-PTTOKKI-EDYM(IDX)
                   MOVE    "99999999"          TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE-1      TO  PTMODRES-PTTOKKI-EDYM
                                                                (IDX)
               ELSE
                   MOVE    TBL-PTTOKKI-EDYM(IDX)   TO  WRK-SYMD(1:6)
                   MOVE    "01"                    TO  WRK-SDD
                   PERFORM 803-ORCSDAY-CHEK-SEC
                   IF     (FLG-DAYERR          =   1   )
                    AND  (PTMODRES-PTTOKKI-ERRCD (IDX) =   SPACE )
                       MOVE    "2606"      TO  PTMODRES-PTTOKKI-ERRCD
                                                                (IDX)
                       MOVE    1               TO  FLG-ERR-01
                   END-IF
               END-IF
      *        開始日＞終了日　エラー
               IF      TBL-PTTOKKI-STYM (IDX)
                                           >  TBL-PTTOKKI-EDYM (IDX)
                   IF      (PTMODRES-PTTOKKI-ERRCD (IDX) =   SPACE )
                       MOVE    "2610"      TO  PTMODRES-PTTOKKI-ERRCD
                                                                (IDX)
                       MOVE    1               TO  FLG-ERR-01
                   END-IF
               END-IF
      *        入外区分
               PERFORM 2002421-PTTOKKI-NYUGAIKBN-SEC
      *
               IF      PTMODREQ-PTTOKKI-CD (IDX)   =   SPACE
      *            紙レセプト
                   PERFORM 2002421-PTTOKKI-NAIYO-SEC
               ELSE
      *            レセ電区分
                   PERFORM 2002421-PTTOKKI-CD-SEC
               END-IF
           END-PERFORM
      *
      *    期間重複チェック
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   50   )
                        OR    (FLG-ERR-01  NOT =   ZERO  )
                        OR    (TBL-PTTOKKI-STYM (IDX)   =   SPACE )
               COMPUTE IDX2            =   IDX     +  1
               PERFORM VARYING     IDX3    FROM    IDX2    BY  1
                           UNTIL  (IDX3        >   20   )
                            OR    (FLG-ERR-01  NOT =   ZERO  )
                            OR    (TBL-PTTOKKI-STYM (IDX3)
                                                   =   SPACE )
               IF      (TBL-PTTOKKI-CD (IDX)   NOT =   SPACE )
                  AND  (TBL-PTTOKKI-CD (IDX)   =
                                          TBL-PTTOKKI-CD(IDX3))
                   IF      (TBL-PTTOKKI-EDYM (IDX3)
                                               <   TBL-PTTOKKI-STYM
                                                             (IDX) )
                     OR    (TBL-PTTOKKI-STYM (IDX3)
                                               >   TBL-PTTOKKI-EDYM
                                                             (IDX) )
                       CONTINUE
                   ELSE
                       MOVE    "2611"      TO  PTMODRES-PTTOKKI-ERRCD
                                                                 (IDX)
                       MOVE    1           TO  FLG-ERR-01
                   END-IF
               END-IF
               END-PERFORM
           END-PERFORM
      *
      *    エラーメッセージ編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                      UNTIL  (IDX         >   50   )
               IF      PTMODRES-PTTOKKI-ERRCD (IDX) NOT =  SPACE
                   MOVE    PTMODRES-PTTOKKI-ERRCD (IDX) TO SPA-ERRCD
                   PERFORM 8901-PTERRCD-SET-SEC
                   MOVE    WRK-ERRMSG     TO  PTMODRES-PTTOKKI-ERRCD-MSG
                                                            (IDX)
                   MOVE    SPACE          TO  SPA-ERRCD
                   MOVE    SPACE          TO  WRK-ERRMSG
               END-IF
           END-PERFORM
      *
           IF      FLG-ERR-01      NOT =   ZERO
               ADD    1                TO  IDX-ERR
               MOVE    "51"            TO  WRK-ERRCD-T (IDX-ERR)
           END-IF
      *    対象なし
           IF     (TBL-PTTOKKI-STYM (1)    =   SPACE )
             AND  (FLG-ERR-01              =   ZERO  )
               ADD     1               TO  IDX-ERR
               MOVE    "52"            TO  WRK-ERRCD-T(IDX-ERR)
           END-IF
           .
       200242-PTTOKKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    特記事項入外区分処理
      *****************************************************************
       2002421-PTTOKKI-NYUGAIKBN-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX-N   FROM    1   BY  1
                   UNTIL      (IDX-N   >   SPA-GMN26-NYUMAX )
                          OR  (FLG-OK  =   1                )
               IF      TBL-PTTOKKI-NYUGAIKBN(IDX)
                                       =   SPA-GMN26-NYUGAI-LST
                                                      (IDX-N) (1:1)
                   MOVE    SPA-GMN26-NYUGAI-LST (IDX-N)(3:)
                                       TO  PTMODRES-PTTOKKI-NYUGAI-NAME
                                                           (IDX)
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF     (FLG-OK              =   ZERO )
              AND (PTMODRES-PTTOKKI-ERRCD(IDX) =   SPACE )
               MOVE    "2602"          TO  PTMODRES-PTTOKKI-ERRCD
                                                                  (IDX)
               MOVE    1               TO  FLG-ERR-01
           END-IF
           .
       2002421-PTTOKKI-NYUGAIKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    特記事項レセ電区分処理
      *****************************************************************
       2002421-PTTOKKI-CD-SEC         SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRCD
      *    数値であること
           INITIALIZE                      ORCSNUMAREA
           MOVE    TBL-PTTOKKI-CD (IDX)    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               IF     (PTMODRES-PTTOKKI-ERRCD(IDX) =   SPACE )
                   MOVE    "2603"          TO  SPA-ERRCD
                   MOVE    "2603"          TO  PTMODRES-PTTOKKI-ERRCD
                                                                  (IDX)
                   MOVE    1               TO  FLG-ERR-01
               END-IF
           ELSE
               MOVE    SNUM-OUTEDT(14:2)   TO  TBL-PTTOKKI-CD (IDX)
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX-T   FROM    1   BY  1
                   UNTIL      (IDX-T       >   SPA-GMN26-TOKMAX )
                          OR  (FLG-OK      =   1                )
               IF      TBL-PTTOKKI-CD (IDX)
                                           =   SPA-GMN26-TOKCDL (IDX-T)
                   MOVE    SPA-GMN26-NAIYOL (IDX-T)
                                               TO  TBL-PTTOKKI-NAIYO
                                                               (IDX)
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
      *    返却編集
           MOVE    TBL-PTTOKKI-CD (IDX)    TO  PTMODRES-PTTOKKI-CD
                                                               (IDX)
           MOVE    TBL-PTTOKKI-NAIYO(IDX)  TO  PTMODRES-PTTOKKI-NAIYO
                                                               (IDX)
      *
           IF     (FLG-OK              =   ZERO )
      *        紙レセプト
               PERFORM 2002421-PTTOKKI-NAIYO-SEC
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    SPACE               TO  SPA-ERRCD
               GO      TO      2002421-PTTOKKI-CD-EXT
           END-IF
      *
      *    開始日チェック
           IF     (TBL-PTTOKKI-CD(IDX) =   "22"
                                       OR  "23"
                                       OR  "24"  )
               IF      TBL-PTTOKKI-STYM (IDX)      <   "200905"
                   MOVE    "2612"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF     (TBL-PTTOKKI-CD(IDX) =   "25"  )
               IF      TBL-PTTOKKI-STYM (IDX)      <   "200910"
                   MOVE    "2613"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *H26.12
      *    区アから多オ　平成２７年１月から
           IF     (TBL-PTTOKKI-CD(IDX) >=  "26"  AND
                                       <=  "35"  )
               IF      TBL-PTTOKKI-STYM (IDX)  <   "201501"
                   MOVE    "2614"              TO  SPA-ERRCD
                   MOVE    05                  TO  SPA-GMN26-CUR
               END-IF
           END-IF
      *    加治　平成２６年１１月から
           IF     (TBL-PTTOKKI-CD(IDX) =  "36"  )
               IF      TBL-PTTOKKI-STYM (IDX)  <   "201411"
                   MOVE    "2615"              TO  SPA-ERRCD
               END-IF
           END-IF
      *    申出　平成２８年４月から
           IF     (TBL-PTTOKKI-CD(IDX) =  "37"  )
               IF      TBL-PTTOKKI-STYM (IDX)  <   "201604"
                   MOVE    "2616"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD       NOT =   SPACE         )
              AND (PTMODRES-PTTOKKI-ERRCD(IDX) =   SPACE )
               MOVE    SPA-ERRCD       TO  PTMODRES-PTTOKKI-ERRCD
                                                                  (IDX)
               MOVE    1               TO  FLG-ERR-01
           END-IF
           .
       2002421-PTTOKKI-CD-EXT.
           EXIT.
      *
      *****************************************************************
      *    特記事項レセ電区分処理
      *****************************************************************
       2002421-PTTOKKI-NAIYO-SEC         SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRCD
           IF      TBL-PTTOKKI-NAIYO (IDX) =   SPACE
               MOVE    "2604"              TO  SPA-ERRCD
           ELSE
               MOVE    TBL-PTTOKKI-NAIYO (IDX) TO  WRK-MAE-INPUT
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  TBL-PTTOKKI-NAIYO
                                                            (IDX)
               IF      FLG-ERR             =   1
                   MOVE    "2604"              TO  SPA-ERRCD
               ELSE
                   MOVE    TBL-PTTOKKI-NAIYO(IDX)
                                       TO  PTMODRES-PTTOKKI-NAIYO
                                                            (IDX)
               END-IF
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE         )
              AND (PTMODRES-PTTOKKI-ERRCD(IDX) =   SPACE )
               MOVE    SPA-ERRCD       TO  PTMODRES-PTTOKKI-ERRCD
                                                                  (IDX)
               MOVE    1               TO  FLG-ERR-01
           END-IF
           .
       2002421-PTTOKKI-NAIYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    特記事項削除処理
      *****************************************************************
       200227-PTTOKKI-DELETE-SEC         SECTION.
      *
      *    特記事項　削除
           INITIALIZE                      PTTOKKI-REC
           MOVE    SPA-HOSPNUM         TO  PTTOKKI-HOSPNUM
           MOVE    SPA-PTID            TO  PTTOKKI-PTID
           MOVE    PTTOKKI-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_pttokki"       TO  MCP-TABLE
           MOVE    "del"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE   "68"             TO  WRK-ERRCD
           END-IF
      *
           .
       200227-PTTOKKI-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    特記事項更新処理
      *****************************************************************
       200228-PTTOKKI-UPDATE-SEC         SECTION.
      *
      *    特記事項削除処理
           PERFORM 200227-PTTOKKI-DELETE-SEC
      *    追加処理
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL  (IDX         >   50   )
                        OR    (TBL-PTTOKKI-STYM (IDX)   =   SPACE )
                        OR    (WRK-ERRCD   NOT =   SPACE )
               MOVE    TBL-PTTOKKI-REC (IDX)   TO  PTTOKKI-REC
      *        連番
               MOVE    IDX                 TO  PTTOKKI-RENNUM
               MOVE    SPA-HOSPNUM         TO  PTTOKKI-HOSPNUM
               MOVE    SPA-PTID            TO  PTTOKKI-PTID
      *
               MOVE    SMCNDATE-YMD        TO  PTTOKKI-CREYMD
               MOVE    SMCNDATE-YMD        TO  PTTOKKI-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTTOKKI-UPHMS
      *
               MOVE    WRK-OPID            TO  PTTOKKI-OPID
      *
               MOVE    PTTOKKI-REC          TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_pttokki"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "67"                TO  WRK-ERRCD
               END-IF
           END-PERFORM
      *
           .
       200228-PTTOKKI-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者個別情報チェック処理
      *****************************************************************
       200252-PTCONF-CHK-SEC         SECTION.
      *
           MOVE    PTMODREQ-BIRTHWEIGHT    TO  PTMODRES-BIRTHWEIGHT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
               MOVE    PTMODREQ-CHKHKTBYO(IDX) TO  PTMODRES-CHKHKTBYO
                                                               (IDX)
           END-PERFORM
           MOVE    PTMODREQ-NINTIHOKATU    TO  PTMODRES-NINTIHOKATU
           MOVE    PTMODREQ-SYOUNIKAKARI   TO  PTMODRES-SYOUNIKAKARI
      *H30.4
      *    妊婦設定
           MOVE    PTMODREQ-NINPUKBN   TO  PTMODRES-NINPUKBN
      *
      *    出生時体重
           MOVE    ZERO                TO  WRK-WEIGHT
           IF      PTMODREQ-BIRTHWEIGHT    NOT =   SPACE
      *        数値チェック
               INITIALIZE                      ORCSNUMAREA
               MOVE    PTMODREQ-BIRTHWEIGHT    TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   IF      IDX-ERR             <   100
                       ADD     1               TO  IDX-ERR
                       MOVE    "61"            TO  WRK-ERRCD-T(IDX-ERR)
                   END-IF
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-WEIGHT
                   MOVE    WRK-WEIGHT          TO  PTMODRES-BIRTHWEIGHT
               END-IF
           END-IF
      *
      *    地域包括診療対象疾病
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
               EVALUATE    PTMODREQ-CHKHKTBYO(IDX)
                   WHEN    "True"
                   WHEN    "False"
                       CONTINUE
                   WHEN    OTHER
                       IF      IDX-ERR             <   100
                           ADD     1               TO  IDX-ERR
                           MOVE    "62"            TO  WRK-ERRCD-T
                                                            (IDX-ERR)
                       END-IF
                       MOVE    4           TO  IDX
               END-EVALUATE
           END-PERFORM
      *    認知症地域包括診療
           EVALUATE    PTMODREQ-NINTIHOKATU
               WHEN    "True"
               WHEN    "False"
                   CONTINUE
               WHEN    OTHER
                   IF      IDX-ERR             <   100
                       ADD     1               TO  IDX-ERR
                       MOVE    "63"            TO  WRK-ERRCD-T
                                                            (IDX-ERR)
                   END-IF
           END-EVALUATE
      *    小児かかりつけ診療
           EVALUATE    PTMODREQ-SYOUNIKAKARI
               WHEN    "True"
               WHEN    "False"
                   CONTINUE
               WHEN    OTHER
                   IF      IDX-ERR             <   100
                       ADD     1               TO  IDX-ERR
                       MOVE    "64"            TO  WRK-ERRCD-T
                                                            (IDX-ERR)
                   END-IF
           END-EVALUATE
      *H30.4
      *    妊婦設定
           EVALUATE    PTMODREQ-NINPUKBN
               WHEN    "True"
      *            妊婦
                   IF      SPA-GMN-SEX         =   "1"
      *                男性はエラー
                       IF      IDX-ERR             <   100
                           ADD     1               TO  IDX-ERR
                           MOVE    "66"            TO  WRK-ERRCD-T
                                                            (IDX-ERR)
                       END-IF
                   END-IF
      *           １６歳未満は警告
                   IF     (SPA-NAI-NENREI-YY   <   16 )
                     AND  (SPA-GMN-SEX         =   "2")
                       ADD     1               TO  IDX-WAR
                       MOVE    "K6"            TO  WRK-KEICD-T (IDX-WAR)
                   END-IF
               WHEN    "False"
                   CONTINUE
               WHEN    OTHER
                   IF      IDX-ERR             <   100
                       ADD     1               TO  IDX-ERR
                       MOVE    "65"            TO  WRK-ERRCD-T
                                                            (IDX-ERR)
                   END-IF
           END-EVALUATE
      *
           .
       200252-PTCONF-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者個別情報　削除処理
      *****************************************************************
       200252-PTCONF-DELETE-SEC         SECTION.
      *
      *    出生時体重削除
           MOVE    SPACE               TO  WRK-CONF-CDATA
           MOVE    "BIRTHWEIGHT"       TO  WRK-CONF-CKEY
           PERFORM 2002521-PTCONF-DBSYORI-SEC
      *    地域包括診療対象疾病
           MOVE    SPACE               TO  WRK-CONF-CDATA
           MOVE    "TIIKIHOKATU"       TO  WRK-CONF-CKEY
           PERFORM 2002521-PTCONF-DBSYORI-SEC
      *    認知症地域包括診療
           MOVE    SPACE               TO  WRK-CONF-CDATA
           MOVE    "NINTIHOKATU"       TO  WRK-CONF-CKEY
           PERFORM 2002521-PTCONF-DBSYORI-SEC
      *    小児かかりつけ診療
           MOVE    SPACE               TO  WRK-CONF-CDATA
           MOVE    "SYOUNIKAKARI"      TO  WRK-CONF-CKEY
           PERFORM 2002521-PTCONF-DBSYORI-SEC
      *H30.4
      *    妊婦設定
           MOVE    SPACE               TO  WRK-CONF-CDATA
           MOVE    "NINPUKBN"          TO  WRK-CONF-CKEY
           PERFORM 2002521-PTCONF-DBSYORI-SEC
      *
           .
       200252-PTCONF-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者個別情報　更新処理
      *****************************************************************
       200252-PTCONF-UPDATE-SEC         SECTION.
      *
      *    出生時体重
           IF      WRK-WEIGHT          =   ZERO
               MOVE    SPACE               TO  WRK-CONF-CDATA
           ELSE
               MOVE    WRK-WEIGHT          TO  WRK-CONF-CDATA
           END-IF
           MOVE    "BIRTHWEIGHT"           TO  WRK-CONF-CKEY
           PERFORM 2002521-PTCONF-DBSYORI-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200252-PTCONF-UPDATE-EXT
           END-IF
      *
      *
      *    地域包括診療対象疾病
           MOVE    SPACE               TO  WRK-CHKBYO-G
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
               IF      PTMODREQ-CHKHKTBYO(IDX)  =   "True"
                   MOVE    "1"             TO  WRK-CHKBYO (IDX)
               ELSE
                   MOVE    "0"             TO  WRK-CHKBYO (IDX)
               END-IF
           END-PERFORM
           IF      WRK-CHKBYO-G        =   ZERO
      *        すべてなし
               MOVE    SPACE               TO  WRK-CONF-CDATA
           ELSE
               MOVE    WRK-CHKBYO-G        TO  WRK-CONF-CDATA
           END-IF
           MOVE    "TIIKIHOKATU"           TO  WRK-CONF-CKEY
           PERFORM 2002521-PTCONF-DBSYORI-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200252-PTCONF-UPDATE-EXT
           END-IF
      *    認知症地域包括診療
           IF      PTMODREQ-NINTIHOKATU    =   "True"
               MOVE    "1"                 TO  WRK-CONF-CDATA
           ELSE
               MOVE    SPACE               TO  WRK-CONF-CDATA
           END-IF
           MOVE    "NINTIHOKATU"           TO  WRK-CONF-CKEY
           PERFORM 2002521-PTCONF-DBSYORI-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200252-PTCONF-UPDATE-EXT
           END-IF
      *    小児かかりつけ診療
           IF      PTMODREQ-SYOUNIKAKARI   =   "True"
               MOVE    "1"                 TO  WRK-CONF-CDATA
           ELSE
               MOVE    SPACE               TO  WRK-CONF-CDATA
           END-IF
           MOVE    "SYOUNIKAKARI"          TO  WRK-CONF-CKEY
           PERFORM 2002521-PTCONF-DBSYORI-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200252-PTCONF-UPDATE-EXT
           END-IF
      *H30.4
      *    妊婦設定
           IF      PTMODREQ-NINPUKBN       =   "True"
               MOVE    "1"                 TO  WRK-CONF-CDATA
           ELSE
               MOVE    SPACE               TO  WRK-CONF-CDATA
           END-IF
           MOVE    "NINPUKBN"          TO  WRK-CONF-CKEY
           PERFORM 2002521-PTCONF-DBSYORI-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200252-PTCONF-UPDATE-EXT
           END-IF
           .
       200252-PTCONF-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者個別設定登録処理
      *****************************************************************
       2002521-PTCONF-DBSYORI-SEC        SECTION.
      *
      *
           INITIALIZE                  PTCONF-REC
           MOVE    SPA-HOSPNUM         TO  PTCONF-HOSPNUM
           MOVE    SPA-PTID            TO  PTCONF-PTID
           MOVE    WRK-CONF-CKEY       TO  PTCONF-CKEY
      *
           MOVE    PTCONF-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptconf"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 930-PTCONF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTCONF
               INITIALIZE                  PTCONF-REC
           END-IF
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    ＤＢあり
           IF      FLG-PTCONF          =   ZERO
               IF      WRK-CONF-CDATA      =   SPACE
      *            ＤＢ削除
                   MOVE    PTCONF-REC          TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_ptconf"        TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       MOVE    "70"            TO  WRK-ERRCD
                   END-IF
               ELSE
      *            ＤＢ更新
                   PERFORM 21111-PTCONF-UPS-SEC
               END-IF
           ELSE
               IF      WRK-CONF-CDATA  NOT =   SPACE
      *            登録
                   PERFORM 21112-PTCONF-INS-SEC
               END-IF
           END-IF
      *
           .
       2002521-PTCONF-DBSYORI-EXT.
           EXIT.
      *****************************************************************
      *    患者個別設定データ登録処理
      *****************************************************************
       21112-PTCONF-INS-SEC        SECTION.
      *
           INITIALIZE                      PTCONF-REC
           MOVE    SPA-HOSPNUM         TO  PTCONF-HOSPNUM
           MOVE    SPA-PTID            TO  PTCONF-PTID
           MOVE    WRK-CONF-CKEY       TO  PTCONF-CKEY
      *    入力値編集
           MOVE    WRK-CONF-CDATA      TO  PTCONF-CDATA
      *    作成年月日
           MOVE    SMCNDATE-YMD        TO  PTCONF-CREYMD
           MOVE    SMCNDATE-YMD        TO  PTCONF-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTCONF-UPHMS
           MOVE    WRK-OPID            TO  PTCONF-OPID
      *
           MOVE    PTCONF-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "69"            TO  WRK-ERRCD
           END-IF
           .
       21112-PTCONF-INS-EXT.
           EXIT.
      *****************************************************************
      *    患者個別設定データ更新処理
      *****************************************************************
       21111-PTCONF-UPS-SEC        SECTION.
      *
      *    入力値編集
           MOVE    WRK-CONF-CDATA      TO  PTCONF-CDATA
      *    更新年月日
           MOVE    SMCNDATE-YMD        TO  PTCONF-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTCONF-UPHMS
           MOVE    WRK-OPID            TO  PTCONF-OPID
      *
           MOVE    PTCONF-REC          TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "69"            TO  WRK-ERRCD
           END-IF
           .
       21111-PTCONF-UPS-EXT.
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
      *        低所得者２（低所得者履歴）情報編集処理
               PERFORM 20013-TSYRRK-HENSYU-SEC
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        低所得者１情報（年金履歴）情報編集処理
               PERFORM 20014-TNKRRK-HENSYU-SEC
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        旧姓履歴編集処理
               PERFORM 20015-KYUSEIRRK-HENSYU-SEC
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        特記事項編集処理
               PERFORM 20016-PTTOKKI-HENSYU-SEC
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        患者個別情報編集処理
               PERFORM 20017-PTCONF-HENSYU-SEC
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
           MOVE    ZERO                TO  IDXE
           PERFORM VARYING   IDX       FROM   1   BY  1
                   UNTIL    (IDX       >   50     )
                      OR    (WRK-ERRCD-T (IDX)    =   SPACE )
               MOVE    WRK-ERRCD-T (IDX)   TO  WRK-ERRCD
               PERFORM 8901-PTERRCD-SETAPI-SEC
               MOVE    "AP"                TO  SPA-ERRCD(1:2)
               MOVE    WRK-ERRCD-T (IDX)   TO  SPA-ERRCD(3:2)
               ADD     1                   TO  IDXE
               MOVE    SPA-ERRCD           TO  PTMODRES-ERROR
                                                           (IDXE)
               MOVE    WRK-ERRMSG          TO  PTMODRES-ERROR-MSG
                                                           (IDXE)
           END-PERFORM
      *
      *    ワーニング
           PERFORM VARYING   IDX       FROM   1   BY  1
                   UNTIL    (IDX       >   50     )
                      OR    (WRK-KEICD-T (IDX)    =   SPACE )
               MOVE    WRK-KEICD-T (IDX)   TO  WRK-ERRCD
               PERFORM 8901-PTERRCD-SETAPI-SEC
               MOVE    "AP"                TO  SPA-ERRCD(1:2)
               MOVE    WRK-KEICD-T (IDX)   TO  SPA-ERRCD(3:2)
               ADD     1                   TO  IDXE
               MOVE    SPA-ERRCD           TO  PTMODRES-ERROR
                                                           (IDXE)
               MOVE    WRK-ERRMSG          TO  PTMODRES-ERROR-MSG
                                                           (IDXE)
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
               MOVE    "低所得者２更新エラー"
                                               TO  WRK-ERRMSG
           WHEN    "62"
               MOVE    "低所得者２削除エラー"
                                               TO  WRK-ERRMSG
           WHEN    "63"
               MOVE    "低所得者１更新エラー"
                                               TO  WRK-ERRMSG
           WHEN    "64"
               MOVE    "低所得者１削除エラー"
                                               TO  WRK-ERRMSG
           WHEN    "65"
               MOVE    "旧姓履歴更新エラー"
                                               TO  WRK-ERRMSG
           WHEN    "66"
               MOVE    "旧姓履歴削除エラー"
                                               TO  WRK-ERRMSG
           WHEN    "67"
               MOVE    "特記事項更新エラー"
                                               TO  WRK-ERRMSG
           WHEN    "68"
               MOVE    "特記事項削除エラー"
                                               TO  WRK-ERRMSG
           WHEN    "69"
               MOVE    "患者個別情報更新エラー"
                                               TO  WRK-ERRMSG
           WHEN    "70"
               MOVE    "患者個別情報削除エラー"
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
               MOVE    "低所得者２更新区分エラー"
                                               TO  WRK-ERRMSG
           WHEN    "21"
               MOVE    "低所得者２の項目にエラーがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "22"
               MOVE    "低所得者２の更新対象がありません。"
                                               TO  WRK-ERRMSG
      *
           WHEN    "30"
               MOVE    "低所得者１更新区分エラー"
                                               TO  WRK-ERRMSG
           WHEN    "31"
               MOVE    "低所得者１にエラーがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "32"
               MOVE    "低所得者１の更新対象がありません。"
                                               TO  WRK-ERRMSG
      *
           WHEN    "40"
               MOVE    "旧姓履歴更新区分エラー"
                                               TO  WRK-ERRMSG
           WHEN    "41"
               MOVE    "旧姓履歴にエラーがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "42"
               MOVE    "旧姓履歴の更新対象がありません。"
                                               TO  WRK-ERRMSG
      *
           WHEN    "50"
               MOVE    "特記事項更新区分エラー"
                                               TO  WRK-ERRMSG
           WHEN    "51"
               MOVE    "特記事項にエラーがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "52"
               MOVE    "特記事項の更新対象がありません。"
                                               TO  WRK-ERRMSG
      *
           WHEN    "60"
               MOVE    "患者個別情報更新区分エラー"
                                               TO  WRK-ERRMSG
           WHEN    "61"
               MOVE    "出生時体重数値エラー"
                                               TO  WRK-ERRMSG
           WHEN    "62"
               MOVE    "地域包括診療対象疾病区分エラー"
                                               TO  WRK-ERRMSG
           WHEN    "63"
               MOVE    "認知症地域包括診療区分エラー"
                                               TO  WRK-ERRMSG
           WHEN    "64"
               MOVE    "小児かかりつけ診療区分エラー"
                                               TO  WRK-ERRMSG
      *H30.4
           WHEN    "65"
               MOVE    "妊婦設定エラー"
                                               TO  WRK-ERRMSG
           WHEN    "66"
               MOVE    "性別が男性です。妊婦設定はできません。"
                                               TO  WRK-ERRMSG
           WHEN    "K6"
               MOVE    "警告！１６歳未満です。"
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
      *    患者個別設定テーブル読み込み
      *****************************************************************
       930-PTCONF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTCONF-REC
               MOVE    ZERO            TO  FLG-PTCONF
           ELSE
               INITIALIZE                  PTCONF-REC
               MOVE    1               TO  FLG-PTCONF
           END-IF
      *
           .
       930-PTCONF-READ-EXT.
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
