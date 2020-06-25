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
       PROGRAM-ID.         ORAPI012S3V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 患者労災・自賠責設定（登録・削除）(V3)
      *  管理者            :
      *  作成日付    作業者        記述
      *  15/10/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-多々納  17/05/XX  自賠責点数算定区分対応
      *  05.00.00    NACL-多々納  19/07/XX  検索処理追加
      *  05.00.00    NACL-多々納  20/04/XX  被保険者証枝番対応
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
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-LABOR-SIO           PIC 9(01).
      *    03  FLG-PTHKNINF            PIC 9(01).
      *    03  FLG-PTKOHINF            PIC 9(01).
      *    03  FLG-HKNJAINF            PIC 9(01).
           03  FLG-HKNNUM              PIC 9(01).
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
      *
           03  WRK-CHK-HKNKBN          PIC X(01).
           03  WRK-CHK-HKNNUM          PIC X(03).
           03  WRK-CHK-KOHNUM          PIC X(03).
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-HOJOKBN             PIC X(01).
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
      *    負担率・額編集
           03  WRK-FTNRATE             PIC 9(02)V99.
           03  WRK-Z9                  PIC 9.ZZ.
           03  WRK-FTNRATEMEI          PIC X(04).
           03  WRK-ZZ9                 PIC ZZZZZ9.
      *
      *    患者保険情報（最大４０件とする）共通で使用
       01  WRK-PTRSIINF-AREA.
           03  WRK-PTHKNINF-TBL        OCCURS   40.
               05  WRK-PTRSI-MODE          PIC X(01).
               05  WRK-PTRSI-HKNID         PIC 9(10).
               05  WRK-PTRSI-KOHID         PIC 9(10).
      *        労災保険区分
               05  WRK-PTRSI-HKNKBN        PIC  X(01).
      *        保険番号
               05  WRK-PTRSI-HKNNUM        PIC  X(03).
               05  WRK-PTRSI-COMMENT       PIC  X(100).
               05  WRK-PTRSI-HKNNUM-NAME   PIC  X(20).
               05  WRK-PTRSI-JIGYOUNAME    PIC  X(80).
               05  WRK-PTRSI-HOKENNAME     PIC  X(80).
      *        傷病年月日
               05  WRK-PTRSI-SHOBYOYMD     PIC  X(08).
      *療養開始年月日
               05  WRK-PTRSI-RYOSTYMD      PIC  X(08).
      *療養終了年月日
               05  WRK-PTRSI-RYOEDYMD      PIC  X(08).
      *アフター療養開始年月日
               05  WRK-PTRSI-SINSATUYMD    PIC  X(08).
      *アフター療養終了年月日
               05  WRK-PTRSI-SINSATUEND    PIC  X(08).
      *労災レセ回数記載：基準年月
               05  WRK-PTRSI-KIJYUNYM      PIC  X(06).
               05  WRK-PTRSI-KAISU         PIC  9(03).
      *
               05  WRK-IN-IDX              PIC 9(04).
      *
               05  WRK-PTRSI-IDX           PIC 9(04).
           03  WRK-PTRSI-MAX           PIC 9(04).
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
           03  WRK-RJN-RATE            PIC X(01).
      *    基準日
           03  WRK-BASE-YMD            PIC X(08).
           03  WRK-BASE-YMD-R      REDEFINES  WRK-BASE-YMD.
               05  WRK-BYY             PIC 9(04).
               05  WRK-BMM             PIC 9(02).
               05  WRK-BDD             PIC 9(02).
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
      *        確認コード
               05  WRK-SELECT-CODE         PIC X(04).
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
       01  CONS-AREA.
           03  TBL-MAX-KOH             PIC 9(02)  VALUE    60.
           03  TBL-MAX-HKN             PIC 9(02)  VALUE    40.
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
      *    保険組合せの保存
       01  HZN-HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC"
                              REPLACING  //COMB-// BY //HZN-//.
      *    患者労災保険情報
       01  TBL-PTRSIINF-AREA.
           02  TBL-PTRSIINF-REC        OCCURS   50.
           COPY    "CPPTRSIINF.INC"
                              REPLACING  //PTRSI-// BY //TBL-PTRSI-//.
      *
               03  TBL-PARA-EDANUM     PIC 9(04).
               03  TBL-PTRSI-MODE      PIC X(01).
      *
           02  TBL-PARA-MAX        PIC 9(04).
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
      *    ユーザプログラム起動チェックサブパラメタ
      **    COPY    "CPORCSUSERCHK.INC".
      *
      *    API XML読込用定義体
           COPY    "CPPATIENTMODV3REQ3.INC".
      *    API XML出力用定義体
           COPY    "CPPATIENTMODV3RES3.INC".
      *
      *    保険組合せ決定関数サブ
           COPY    "CPORCSAPIHKN.INC".
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
           COPY    "CPORAPI012SUB2V3.INC".
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
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    労働基準監督署マスタ
       01  LABOR-SIO-REC.
           COPY    "CPLABOR-SIO.INC".
      *
      *
      *    患者公費情報
      *01  PTKOHINF-REC.
      *    COPY    "CPPTKOHINF.INC".
      *
      *    保険者号マスタ
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    受診履歴
      *01  JYURRK-REC.
      *    COPY    "CPJYURRK.INC".
      *
      *    患者入院履歴
      *01  PTNYUINRRK-REC.
      *    COPY    "CPPTNYUINRRK.INC".
      *
      *    患者病名
      *01  PTBYOMEI-REC.
      *    COPY    "CPPTBYOMEI.INC".
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
               MOVE   404                  TO  APILST05-HTTPSTATUS
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
           INITIALIZE                      WRK-PTRSIINF-AREA
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
           WHEN    "03"
               MOVE    3                   TO  FLG-SYORI
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
               PERFORM 2002-CHECK-SYORI-SEC
           WHEN    "03"
      *        更新処理
               PERFORM 2003-UPDATE-SYORI-SEC
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
           IF      APILST05-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-PATIENTMODREQ
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_patientmodv3req3"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST05-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "patientmodv3req3"  TO  APIREQ-RECORDNAME
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
           MOVE    "xml_patientmodv3req3"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST05-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "patientmodv3req3"  TO  APIREQ-RECORDNAME
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
           MOVE    LOW-VALUE           TO  APILST05-BODY
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
                       MOVE    "処理終了"
                                           TO  WRK-OKMSG
                   WHEN    3
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
           IF     (WRK-ERRCD           =   ZERO )
              AND (IDX-WAR             >   ZERO )
      *        ワーニング有
               MOVE    "W"                 TO  WRK-ERRKBN
               IF      WRK-KEICD           =   SPACE
                   IF      FLG-SYORI           =   3
                       MOVE    SPACE               TO  WRK-OKMSG
                       STRING  "登録処理終了。"
                               "警告メッセージがあります。"
                               "確認して下さい。"
                                           DELIMITED  BY  SIZE
                                           INTO  WRK-OKMSG
                       END-STRING
                   END-IF
               END-IF
               MOVE    WRK-ERRCD-G         TO  PTMODRES-API-RESULT
               MOVE    WRK-OKMSG           TO  PTMODRES-API-RESULT-MSG
           END-IF
      *
           IF      APILST05-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_patientmodv3res3"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  PTMODRES-MODE
           MOVE    "patientmodv3res3"  TO  PTMODRES-RECORDNAME
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
           MOVE    "xml_patientmodv3res3"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  PTMODRES-MODE
           MOVE    "patientmodv3res3"  TO  PTMODRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-PATIENTMODRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               MOVE    PTMODRES-OBJECT     TO  APILST05-BODY
               MOVE    "patientmod/xml"    TO  APILST05-CONTENT-TYPE
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
               IF      WRK-PARA-SYORIPG    NOT =   "03"
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
      *    基準日チェック
      *    IF      WRK-ERRCD           =   SPACE
      *        PERFORM 20011-BASEYMD-CHK-SEC
      *    END-IF
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
      *    基準日付チェック
      *****************************************************************
       20011-BASEYMD-CHK-SEC    SECTION.
      *
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-BASE-YMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "11"                TO  WRK-ERRCD
           END-IF
      *    H27.4.1からとする
           IF      WRK-BASE-YMD        <   "20150401"
               MOVE    "12"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-BASEYMD-CHK-EXT.
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
      *    労災保険情報編集
           IF      WRK-ERRCD           =   SPACE
               MOVE    ZERO            TO  FLG-HENSYU
               PERFORM 20013-RSIINF-HENSYU-SEC
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
                   MOVE    "50"               TO  WRK-ERRCD
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
      **   IF     (FLG-SYORI       NOT =   1    )
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
           MOVE    PTINF-HOME-ADRS     TO  PTMODRES-HOME-ADDRESSES1
           MOVE    PTINF-HOME-BANTI    TO  PTMODRES-HOME-ADDRESSES2
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
           MOVE    PTMODRES-SELECT-CODE    TO  WRK-SELECT-CODE
           MOVE    WRK-ENDKBN          TO  WRK-PARA-ENDKBN
      *    処理ＰＧ
           MOVE    "03"                TO  WRK-PARA-SYORIPG
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
      *    労災か自賠責が登録されていること
           PERFORM 200121-HKNNUM-SET-SEC
      *
           .
       20012-PTINF-HEN-EXT.
           EXIT.
      *
      ****************************************************************
      *    労災保険情報編集処理
      ****************************************************************
       200121-HKNNUM-SET-SEC           SECTION.
      *
      *    労災か自賠責が登録されていること
           INITIALIZE                  HKNNUM-REC
           MOVE    "971"               TO  HKN-HKNNUM
           PERFORM 9002-HKNNUM-KEYREAD-SEC
           IF      FLG-HKNNUM          =   ZERO
               MOVE    1               TO  SPA-ROSAI-FLG
           ELSE
               MOVE    ZERO            TO  SPA-ROSAI-FLG
           END-IF
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    "973"               TO  HKN-HKNNUM
           PERFORM 9002-HKNNUM-KEYREAD-SEC
           IF      FLG-HKNNUM          =   ZERO
               MOVE    1               TO  SPA-JIBAI-FLG
           ELSE
               MOVE    ZERO            TO  SPA-JIBAI-FLG
           END-IF
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    "970"               TO  HKN-HKNNUM
           PERFORM 9002-HKNNUM-KEYREAD-SEC
           IF      FLG-HKNNUM          =   ZERO
               MOVE    1               TO  SPA-DAISAN-FLG
           ELSE
               MOVE    ZERO            TO  SPA-DAISAN-FLG
           END-IF
      *
           .
       200121-HKNNUM-SET-EXT.
           EXIT.
      *
      ****************************************************************
      *    労災保険情報編集処理
      ****************************************************************
       20013-RSIINF-HENSYU-SEC           SECTION.
      *
           INITIALIZE                  PTMODRES-ACCIDENT-INFO-G
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTRSIINF"          TO  PARA-FILEMEI
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
                       OR  (IDX            >=  40)
               MOVE    PARA-DATA-REC       TO  PTRSIINF-REC
      *
               ADD     1                   TO  IDX
               PERFORM 20013-RSIINF-PTMODRES-SEC
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    保険組合せ編集処理
           PERFORM 20013-HKNCOMBI-HENSYU-SEC
           .
       20013-RSIINF-HENSYU-EXT.
           EXIT.
      *
      ****************************************************************
      *    労災保険情報編集処理
      **************************************************************
       20013-RSIINF-PTMODRES-SEC           SECTION.
      *
      *    保険ＩＤ
           MOVE    PTRSI-HKNID         TO  PTMODRES-MAIN-INSURANCE-ID
                                                                 (IDX)
      *    公費ＩＤ
           MOVE    PTRSI-KOHID         TO  PTMODRES-INSURANCE-ID (IDX)
      *
      *    処理モード
           IF      FLG-HENSYU          =   1
               MOVE    PARA-EDANUM         TO  IDR
      *        新規
               EVALUATE    TBL-PTRSI-MODE (IDR)
                   WHEN    "1"
                       MOVE    "New"           TO  PTMODRES-MAIN-MODE
                                                                 (IDX)
      *                保険・公費ＩＤはゼロとする
                       MOVE    ZERO       TO  PTMODRES-MAIN-INSURANCE-ID
                                                                 (IDX)
                       MOVE    ZERO       TO  PTMODRES-INSURANCE-ID
                                                                 (IDX)
                   WHEN    "2"
                       MOVE    "Modify"        TO  PTMODRES-MAIN-MODE
                                                                 (IDX)
                   WHEN    "3"
                       MOVE    "Delete"        TO  PTMODRES-MAIN-MODE
                                                                 (IDX)
               END-EVALUATE
           END-IF
      *    保険番号
           MOVE    PTRSI-HKNNUM        TO  PTMODRES-INSURANCE-CLASS
                                                                (IDX)
      *    保険種類名称
           INITIALIZE                  HKNNUM-REC
           MOVE    PTRSI-HKNNUM        TO  HKN-HKNNUM
           PERFORM 9002-HKNNUM-KEYREAD-SEC
           IF      FLG-HKNNUM          =   ZERO
               MOVE    HKN-TANSEIDONAME    TO   PTMODRES-INSURANCE-NAME
                                                                 (IDX)
           END-IF
      *    労災保険名称
           MOVE    PTRSI-HKNKBN        TO  PTMODRES-ACCIDENT  (IDX)
           EVALUATE    PTRSI-HKNKBN
               WHEN    "1"
                   MOVE    "短期給付"      TO  PTMODRES-ACCIDENT-NAME
                                                                  (IDX)
               WHEN    "2"
                   MOVE    "傷病年金"      TO  PTMODRES-ACCIDENT-NAME
                                                                  (IDX)
               WHEN    "3"
                   MOVE    "アフターケア"  TO  PTMODRES-ACCIDENT-NAME
                                                                  (IDX)
               WHEN    "4"
                   MOVE    "自賠責保険"    TO  PTMODRES-ACCIDENT-NAME
                                                                  (IDX)
               WHEN    "5"
                   MOVE    "公務災害"      TO  PTMODRES-ACCIDENT-NAME
                                                                  (IDX)
               WHEN    "6"
                   MOVE    "第三者行為"    TO  PTMODRES-ACCIDENT-NAME
                                                                  (IDX)
           END-EVALUATE
      *    傷病の部位
           MOVE    PTRSI-COMMENT       TO  PTMODRES-DISEASE-LOCATION
                                                                  (IDX)
      *    傷病年月日
           IF      PTRSI-SHOBYOYMD NOT =   SPACE
               MOVE    PTRSI-SHOBYOYMD     TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  PTMODRES-DISEASE-DATE
                                                                  (IDX)
           END-IF
      *    アフターケア
           IF      PTRSI-HKNKBN        =   "3"
      *        療養開始年月日
               MOVE    PTRSI-SINSATUYMD    TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  PTMODRES-START-DATE(IDX)
      *        療養終了年月日
               MOVE    PTRSI-SINSATUEND    TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  PTMODRES-END-DATE (IDX)
           ELSE
      *        療養開始年月日
               MOVE    PTRSI-RYOSTYMD      TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  PTMODRES-START-DATE(IDX)
      *        療養終了年月日
               MOVE    PTRSI-RYOEDYMD      TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  PTMODRES-END-DATE (IDX)
           END-IF
      *
           EVALUATE    PTRSI-HKNKBN
               WHEN    "1"
      *    労働保険番号
                   MOVE    PTRSI-KOFUNUM   TO  PTMODRES-ACCIDENT-NUMBER
                                                                (IDX)
               WHEN    "2"
      *    年金証書番号
                   MOVE    PTRSI-KOFUNUM   TO
                                           PTMODRES-PENSIONCERTIFICATE
                                                                (IDX)
               WHEN    "3"
      *    健康管理手帳番号
                   MOVE    PTRSI-KOFUNUM   TO
                                           PTMODRES-KANRITCHO-NUMBER
                                                                (IDX)
           END-EVALUATE
      *    災害区分（名称）
           EVALUATE    PTRSI-SAIGAIKBN
               WHEN    "1"
                   MOVE    PTRSI-SAIGAIKBN
                                           TO  PTMODRES-ACCIDENT-CLASS
                                                                (IDX)
                   MOVE    "業務中の災害"
                                       TO  PTMODRES-ACCIDENT-CLASS-NAME
                                                                (IDX)
               WHEN    "2"
                   MOVE    PTRSI-SAIGAIKBN
                                           TO  PTMODRES-ACCIDENT-CLASS
                                                                (IDX)
                   MOVE    "通勤途上の災害"
                                       TO  PTMODRES-ACCIDENT-CLASS-NAME
                                                                (IDX)
           END-EVALUATE
      *    労働基準監督署コード
           MOVE    PTRSI-SYOCD         TO  PTMODRES-LABOR-ST-CODE(IDX)
           IF      PTRSI-SYOCD     NOT =   SPACE
               PERFORM 20021012-SYOCD-HEN-SEC
           END-IF
      *    新継再別区分
           MOVE    PTRSI-SINKEI        TO  PTMODRES-SINKEI (IDX)
           EVALUATE    PTRSI-SINKEI
               WHEN    "1"
                   MOVE    "初診"      TO  PTMODRES-SINKEI-NAME(IDX)
               WHEN    "3"
                   MOVE    "転医始診"  TO  PTMODRES-SINKEI-NAME(IDX)
               WHEN    "5"
                   MOVE    "継続"      TO  PTMODRES-SINKEI-NAME(IDX)
               WHEN    "7"
                   MOVE    "再発"      TO  PTMODRES-SINKEI-NAME(IDX)
           END-EVALUATE
      *    転帰事由
           MOVE    PTRSI-TENKI         TO  PTMODRES-TENKI (IDX)
           EVALUATE    PTRSI-TENKI
               WHEN    "1"
                   MOVE    "治癒"      TO  PTMODRES-TENKI-NAME(IDX)
               WHEN    "3"
                   MOVE    "継続"      TO  PTMODRES-TENKI-NAME(IDX)
               WHEN    "5"
                   MOVE    "転医"      TO  PTMODRES-TENKI-NAME(IDX)
               WHEN    "7"
                   MOVE    "中止"      TO  PTMODRES-TENKI-NAME(IDX)
               WHEN    "9"
                   MOVE    "死亡"      TO  PTMODRES-TENKI-NAME(IDX)
           END-EVALUATE
      *    四肢特例区分
           MOVE    PTRSI-SISIKBN       TO  PTMODRES-SISIKBN (IDX)
           EVALUATE    PTRSI-SISIKBN
               WHEN    "0"
                   MOVE    "なし"      TO  PTMODRES-SISIKBN-NAME(IDX)
               WHEN    "1"
                   MOVE    "四肢"      TO  PTMODRES-SISIKBN-NAME(IDX)
               WHEN    "2"
                   MOVE    "手指"      TO  PTMODRES-SISIKBN-NAME(IDX)
           END-EVALUATE
      *
      *    事業所名称
           MOVE    PTRSI-JIGYOUNAME    TO  PTMODRES-JIGYOUNAME (IDX)
      *    所在地都道府県名
           MOVE    PTRSI-PREFNAME      TO  PTMODRES-PREFNAME (IDX)
      *    所在地都道府県区分
           MOVE    PTRSI-PREFKBN       TO  PTMODRES-PREFKBN (IDX)
           EVALUATE    PTRSI-PREFKBN
               WHEN    "1"
                   MOVE    "都"        TO  PTMODRES-PREFKBN-NAME (IDX)
               WHEN    "2"
                   MOVE    "道"        TO  PTMODRES-PREFKBN-NAME (IDX)
               WHEN    "3"
                   MOVE    "府"        TO  PTMODRES-PREFKBN-NAME (IDX)
               WHEN    "4"
                   MOVE    "県"        TO  PTMODRES-PREFKBN-NAME (IDX)
           END-EVALUATE
      *    所在地郡市区名
           MOVE    PTRSI-CITYNAME      TO  PTMODRES-CITYNAME (IDX)
      *    所在地郡市区区分
           MOVE    PTRSI-CITYKBN       TO  PTMODRES-CITYKBN (IDX)
           EVALUATE    PTRSI-CITYKBN
               WHEN    "1"
                   MOVE    "郡"        TO  PTMODRES-CITYKBN-NAME (IDX)
               WHEN    "2"
                   MOVE    "市"        TO  PTMODRES-CITYKBN-NAME (IDX)
               WHEN    "3"
                   MOVE    "区"        TO  PTMODRES-CITYKBN-NAME (IDX)
           END-EVALUATE
      *    労災レセ回数記載 基準年月
           IF      PTRSI-KIJYUNYM  NOT =   SPACE
               MOVE    PTRSI-KIJYUNYM      TO  WRK-SYMD(1:6)
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE(1:7)   TO  PTMODRES-KIJYUNYM
                                                                  (IDX)
           END-IF
      *    労災レセ回数記載 回数
           IF      PTRSI-KAISU     NOT =   ZERO
               MOVE    PTRSI-KAISU     TO  PTMODRES-RESEPUTO-COUNT(IDX)
           END-IF
      *自賠責
      *    自賠責保険会社名
           MOVE    PTRSI-HOKENNAME     TO  PTMODRES-HOKENNAME (IDX)
      *    自賠責患者請求区分
           MOVE    PTRSI-JIBAISEIKBN   TO  PTMODRES-JIBAISEIKBN (IDX)
           EVALUATE    PTRSI-JIBAISEIKBN
               WHEN    "1"
                   MOVE    "患者請求する(100%)"
                                       TO  PTMODRES-JIBAISEIKBN-NAME
                                                            (IDX)
               WHEN    "2"
                   MOVE    "患者請求しない(0%)"
                                       TO  PTMODRES-JIBAISEIKBN-NAME
                                                            (IDX)
           END-EVALUATE
      *    自賠責担当医コード
           IF      PTRSI-DRCD      NOT =   SPACE
               MOVE    PTRSI-DRCD      TO  PTMODRES-RSI-DRCD (IDX)
               PERFORM 20021013-RSI-DRCD-HEN-SEC
           END-IF
      *
      *    アフターケア
           EVALUATE    PTRSI-HKNKBN
           WHEN    "3"
      *        損傷区分
               STRING  PTRSI-SONSHOUKBN
                       PTRSI-SONSHOUKBN-EDANUM
                                           DELIMITED  BY  SIZE
                                           INTO   PTMODRES-DAMAGE-CLASS
                                                                (IDX)
               END-STRING
      *        損傷区分名称取得
               MOVE    SPACE               TO  SYSKANRI-REC
               INITIALIZE                  SYSKANRI-REC
               MOVE    "4002"              TO  SYS-KANRICD
               MOVE    PTRSI-SINSATUYMD    TO  SYS-STYUKYMD
               MOVE    PTRSI-SINSATUYMD    TO  SYS-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    PTMODRES-DAMAGE-CLASS (IDX)
                                           TO  SYS-KBNCD
               PERFORM 900-SYSKANRI-KEYREAD-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    SYSKANRI-REC        TO  SYS-4002-REC
                   MOVE    SYS-4002-SONSHOUNAME
                                       TO  PTMODRES-DAMAGE-CLASS-NAME
                                                                (IDX)
               END-IF
      *H29.5
      *    自賠責
           WHEN    "4"
      *        自賠責点数区分
               IF      PTRSI-JIBAIKBN      =   SPACE
                   MOVE    "0"             TO  PTRSI-JIBAIKBN
               END-IF
               MOVE    PTRSI-JIBAIKBN      TO  PTMODRES-JIBAIKBN (IDX)
               EVALUATE    PTRSI-JIBAIKBN
                   WHEN    "0"
                       MOVE    "労災自賠医療機関情報に準拠"
                                           TO  PTMODRES-JIBAIKBN-NAME
                                                               (IDX)
                   WHEN    "1"
                       MOVE    "労災準拠"
                                           TO  PTMODRES-JIBAIKBN-NAME
                                                               (IDX)
                   WHEN    "2"
                       MOVE    "健保準拠"
                                           TO  PTMODRES-JIBAIKBN-NAME
                                                               (IDX)
               END-EVALUATE
      *
      *    第三者行為
           WHEN    "6"
      *        現物支給区分
               MOVE    PTRSI-DAISAN-GENBTKBN
                                       TO  PTMODRES-DS-GENBTKBN (IDX)
               EVALUATE    PTRSI-DAISAN-GENBTKBN
               WHEN    "1"
                   MOVE    "対象外"
                                   TO  PTMODRES-DS-GENBTKBN-NAME (IDX)
               WHEN    "2"
                   MOVE    "対象"
                                   TO  PTMODRES-DS-GENBTKBN-NAME (IDX)
               END-EVALUATE
      *        特記事項区分
               MOVE    PTRSI-DAISAN-TOKKIKBN
                                       TO  PTMODRES-DS-TOKKIKBN (IDX)
               EVALUATE    PTRSI-DAISAN-TOKKIKBN
               WHEN    "1"
                   MOVE    "「１０第三」記載有"
                                   TO  PTMODRES-DS-TOKKIKBN-NAME (IDX)
               WHEN    "2"
                   MOVE    "「１０第三」記載無"
                                   TO  PTMODRES-DS-TOKKIKBN-NAME (IDX)
               END-EVALUATE
           END-EVALUATE
      *
           .
       20013-RSIINF-PTMODRES-EXT.
           EXIT.
      *****************************************************************
      *    労働基準監督署名称編集処理
      *****************************************************************
       20021012-SYOCD-HEN-SEC             SECTION.
      *
           INITIALIZE                      LABOR-SIO-REC
           MOVE    PTRSI-SYOCD         TO  LABOR-SIO-SYOCD
           MOVE    PTRSI-RYOSTYMD      TO  LABOR-SIO-YUKOSTYMD
           MOVE    PTRSI-RYOSTYMD      TO  LABOR-SIO-YUKOEDYMD
      *
           MOVE    LABOR-SIO-REC       TO  MCPDATA-REC
           MOVE    "tbl_labor_sio"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_labor_sio"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-LABOR-SIO-READ-SEC
           ELSE
               INITIALIZE                  LABOR-SIO-REC
               MOVE    1               TO  FLG-LABOR-SIO
           END-IF
           MOVE    "tbl_labor_sio"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-LABOR-SIO       =   ZERO
               MOVE    LABOR-SIO-NAME      TO  PTMODRES-LABOR-ST-NAME
                                                        (IDX)
           ELSE
               MOVE    SPACE               TO  PTMODRES-LABOR-ST-NAME
                                                        (IDX)
           END-IF
           .
       20021012-SYOCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    自賠責担当医コード名称編集処理
      *****************************************************************
       20021013-RSI-DRCD-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1010"              TO  SYS-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    PTRSI-DRCD          TO  SYS-KBNCD
           PERFORM 900-SYSKANRI-KEYREAD-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
               MOVE    SYS-1010-NAME       TO  PTMODRES-RSI-DRCD-NAME
                                                                (IDX)
           END-IF
           .
       20021013-RSI-DRCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       20013-HKNCOMBI-HENSYU-SEC             SECTION.
      *
      *
           INITIALIZE                  ORAPI012SUB2V3AREA
           MOVE    SPA-SYSYMD          TO  AP12SUB2V3-SYSYMD
           IF      FLG-HENSYU          =   1
               MOVE    "2"             TO  AP12SUB2V3-SYORIKBN
           ELSE
               MOVE    "1"             TO  AP12SUB2V3-SYORIKBN
           END-IF
           CALL    "ORAPI012SUB2V3"    USING
                                       ORAPI012SUB2V3AREA
                                       SPA-AREA
                                       SPA-P02SCRAREA
      *
           INITIALIZE                  PTMODRES-COMBINATION-INF-G
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   AP12SUB2V3-MAX )
               PERFORM 200131-COMBINATION-HEN-SEC
           END-PERFORM
      *
           IF      AP12SUB2V3-ERRCD    NOT =   SPACE
      *        警告
               MOVE    AP12SUB2V3-ERRCD    TO  WRK-ERRCD
      *        警告
               STRING  "AP"
                       AP12SUB2V3-ERRCD    DELIMITED  BY  SIZE
                                       INTO    SPA-ERRCD
               END-STRING
      ******   MOVE    AP12SUB2V3-ERRCD    TO  WRK-ERRCD
               MOVE    SPACE               TO  WRK-ERRCD
               PERFORM 8901-PTERRCD-SETAPI-SEC
               PERFORM 290-WARMSG-HEN-SEC
           END-IF
      *
           .
       20013-HKNCOMBI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ出力領域保険情報編集処理
      *****************************************************************
       200131-COMBINATION-HEN-SEC     SECTION.
      *
      *    処理区分（新規・削除のみ）
           MOVE    AP12SUB2V3-COMB-MODE (IDX)
                                   TO  PTMODRES-COMB-MODE (IDX)
      *    保険組合せ番号
           MOVE    AP12SUB2V3-COMBINATION-NUMBER (IDX)
                                   TO  PTMODRES-COMBINATION-NUMBER
                                                           (IDX)
      *    保険組合せ　負担割合（入院）
           MOVE    AP12SUB2V3-COMBINATION-RATE-ADM (IDX)
                                   TO  PTMODRES-COMBINATION-RATE-ADM
                                                           (IDX)
      *    保険組合せ　負担割合（外来）
           MOVE    AP12SUB2V3-COMBINATION-RATE-OUT (IDX)
                                   TO  PTMODRES-COMBINATION-RATE-OUT
                                                           (IDX)
      *    保険組合せの有効開始日
           MOVE    AP12SUB2V3-COMB-START-DATE (IDX)
                                   TO  PTMODRES-COMB-START-DATE
                                                           (IDX)
      *    保険組合せの有効終了日
           MOVE    AP12SUB2V3-COMB-END-DATE (IDX)
                                   TO  PTMODRES-COMB-END-DATE
                                                           (IDX)
      *
      *    保険ＩＤ
           MOVE    AP12SUB2V3-COMB-MAIN-ID  (IDX)
                                   TO  PTMODRES-COMB-MAIN-ID (IDX)
      *    保険の種類
           MOVE    AP12SUB2V3-COMB-MAIN-CLASS  (IDX)
                                   TO  PTMODRES-COMB-MAIN-CLASS (IDX)
      *    保険者番号
           MOVE    AP12SUB2V3-COMB-MAIN-NUMBER  (IDX)
                                   TO  PTMODRES-COMB-MAIN-NUMBER
                                                             (IDX)
      *    保険の種類名称
           MOVE    AP12SUB2V3-COMB-MAIN-NAME  (IDX)
                                   TO  PTMODRES-COMB-MAIN-NAME (IDX)
      *    記号
           MOVE    AP12SUB2V3-COMB-MARK  (IDX)
                                   TO  PTMODRES-COMB-MARK   (IDX)
           MOVE    AP12SUB2V3-COMB-NUMBER  (IDX)
                                   TO  PTMODRES-COMB-NUMBER  (IDX)
      *    枝番
           MOVE    AP12SUB2V3-COMB-EDABAN  (IDX)
                                   TO  PTMODRES-COMB-EDABAN  (IDX)
      *    継続区分
           MOVE    AP12SUB2V3-COMB-CONTINUATION  (IDX)
                                   TO  PTMODRES-COMB-CONTINUATION(IDX)
      *    補助区分
           MOVE    AP12SUB2V3-COMB-ASSISTANCE  (IDX)
                                   TO  PTMODRES-COMB-ASSISTANCE (IDX)
           MOVE    AP12SUB2V3-COMB-ASSI-NAME  (IDX)
                                   TO  PTMODRES-COMB-ASSI-NAME (IDX)
      *    本人家族区分
           MOVE    AP12SUB2V3-COMB-FAMILY  (IDX)
                                   TO  PTMODRES-COMB-FAMILY  (IDX)
      *
      *    公費情報
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   4      )
                          OR  (AP12SUB2V3-INSURANCE-CLASS (IDX IDY)
                                           =   SPACE)
      *        公費ＩＤ
               MOVE    AP12SUB2V3-INSURANCE-ID (IDX IDY)
                                       TO  PTMODRES-COMB-INSURANCE-ID
                                                             (IDX IDY)
      *        公費の種類
               MOVE    AP12SUB2V3-INSURANCE-CLASS(IDX IDY)
                                       TO  PTMODRES-COMB-INSURANCE-CLASS
                                                             (IDX IDY)
      *        公費の種類名称
               MOVE    AP12SUB2V3-INSURANCE-NAME(IDX IDY)
                                       TO  PTMODRES-COMB-INSURANCE-NAME
                                                             (IDX IDY)
      *        負担者番号
               MOVE    AP12SUB2V3-PROVIDER-NUMBER (IDX IDY)
                                       TO  PTMODRES-COMB-PROVIDER-NUMBER
                                                             (IDX IDY)
      *        受給者番号
               MOVE    AP12SUB2V3-RECIPIENT-NUMBER (IDX IDY)
                                     TO  PTMODRES-COMB-RECIPIENT-NUMBER
                                                             (IDX IDY)
      *        入院−負担率（割）
               MOVE    AP12SUB2V3-RATE-ADMISSION (IDX IDY)
                                     TO  PTMODRES-RATE-ADMISSION
                                                             (IDX IDY)
      *        入院−固定額
               MOVE    AP12SUB2V3-MONEY-ADMISSION (IDX IDY)
                                     TO  PTMODRES-MONEY-ADMISSION
                                                             (IDX IDY)
      *        外来−負担率（割）
               MOVE    AP12SUB2V3-RATE-OUTPATIENT (IDX IDY)
                                     TO  PTMODRES-RATE-OUTPATIENT
                                                             (IDX IDY)
      *        外来−固定額
               MOVE    AP12SUB2V3-MONEY-OUTPATIENT (IDX IDY)
                                     TO  PTMODRES-MONEY-OUTPATIENT
                                                             (IDX IDY)
           END-PERFORM
           .
       200131-COMBINATION-HEN-EXT.
           EXIT.
      *****************************************************************
      *    補助区分名称編集
      *****************************************************************
      *200131-HOJOKBN-NAME-HEN-SEC             SECTION.
      *
      *    INITIALIZE                  ORAPI012SUB1AREA
      *    MOVE    WRK-HKNNUM          TO  API012SUB1-HKNNUM
      *    MOVE    WRK-HOJOKBN         TO  API012SUB1-HOJOKBN
      *    MOVE    SPA-SYSYMD          TO  API012SUB1-SYSYMD
      *    CALL    "ORAPI012SUB1"          USING
      *                                    ORAPI012SUB1AREA
      *                                    SPA-AREA
      *
      *    .
      *200131-HOJOKBN-NAME-HEN-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    患者保険確認処理
      *****************************************************************
       2002-CHECK-SYORI-SEC     SECTION.
      *
      *    患者番号判定
           PERFORM 20011-PTINF-INITCHK-SEC
      *    保険情報編集
           PERFORM 20021-INPUT-HKNHEN-SEC
      * 
      *    基本情報処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20012-PTINF-HEN-SEC
           END-IF
      *    労災情報チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20021-RSIHKN-CHK-SEC
           END-IF
      *    保険組合せ作成
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20022-HKNCOMBI-CHKSYORI-SEC
           END-IF
      *    情報編集
           IF      WRK-ERRCD           =   SPACE
               MOVE    1               TO  FLG-HENSYU
               PERFORM 20013-RSIINF-HENSYU-SEC
           END-IF
      *    エラー判定
           IF      WRK-ERRCD           =   SPACE
               IF      IDX-ERR             >   ZERO
                   MOVE    "50"               TO  WRK-ERRCD
               END-IF
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
       2002-CHECK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険情報編集処理
      *****************************************************************
       20021-INPUT-HKNHEN-SEC         SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   40
      *        保険の種類・保険者番号なし対象外
               IF     (PTMODREQ-INSURANCE-CLASS (IDX)  =   SPACE)
                  AND (PTMODREQ-ACCIDENT        (IDX)  =   SPACE)
                   CONTINUE
               ELSE
      *
               MOVE    PTMODREQ-MAIN-MODE (IDX)
                                       TO  PTMODRES-MAIN-MODE   (IDX)
               MOVE    PTMODREQ-MAIN-INSURANCE-ID (IDX)
                                       TO  PTMODRES-MAIN-INSURANCE-ID
                                                                 (IDX)
               MOVE    PTMODREQ-INSURANCE-ID (IDX)
                                       TO  PTMODRES-INSURANCE-ID (IDX)
               MOVE    PTMODREQ-INSURANCE-CLASS (IDX)
                                       TO  PTMODRES-INSURANCE-CLASS(IDX)
               MOVE    PTMODREQ-INSURANCE-NAME (IDX)
                                       TO  PTMODRES-INSURANCE-NAME(IDX)
      *    労災情報
               MOVE    PTMODREQ-ACCIDENT       (IDX)
                                       TO  PTMODRES-ACCIDENT     (IDX)
               MOVE    PTMODREQ-ACCIDENT-NAME (IDX)
                                       TO  PTMODRES-ACCIDENT-NAME(IDX)
      *        傷病の部位
               MOVE    PTMODREQ-DISEASE-LOCATION (IDX)
                                       TO  PTMODRES-DISEASE-LOCATION
                                                                 (IDX)
               MOVE    PTMODREQ-DISEASE-DATE (IDX)
                                       TO  PTMODRES-DISEASE-DATE (IDX)
               MOVE    PTMODREQ-START-DATE (IDX)
                                       TO  PTMODRES-START-DATE (IDX)
               MOVE    PTMODREQ-END-DATE (IDX)
                                       TO  PTMODRES-END-DATE (IDX)
      *労災
               MOVE    PTMODREQ-ACCIDENT-NUMBER(IDX)
                                       TO  PTMODRES-ACCIDENT-NUMBER
                                                               (IDX)
               MOVE    PTMODREQ-PENSIONCERTIFICATE(IDX)
                                       TO  PTMODRES-PENSIONCERTIFICATE
                                                                (IDX)
      *        災害区分名称
               MOVE    PTMODREQ-ACCIDENT-CLASS (IDX)
                                       TO  PTMODRES-ACCIDENT-CLASS
                                                              (IDX)
      *        労働基準監督署コード
               MOVE    PTMODREQ-LABOR-ST-CODE (IDX)
                                       TO  PTMODRES-LABOR-ST-CODE
                                                              (IDX)
      *        新継再別区分
               MOVE    PTMODREQ-SINKEI (IDX)
                                       TO  PTMODRES-SINKEI   (IDX)
      *        転帰事由
               MOVE    PTMODREQ-TENKI (IDX)
                                       TO  PTMODRES-TENKI    (IDX)
      *        四肢特例区分
               MOVE    PTMODREQ-SISIKBN (IDX)
                                       TO  PTMODRES-SISIKBN  (IDX)
      *
      *        事業所名称
               MOVE    PTMODREQ-JIGYOUNAME (IDX)
                                       TO  PTMODRES-JIGYOUNAME (IDX)
               MOVE    PTMODREQ-PREFNAME (IDX)
                                       TO  PTMODRES-PREFNAME (IDX)
      *        所在地都道府県区分
               MOVE    PTMODREQ-PREFKBN (IDX)
                                       TO  PTMODRES-PREFKBN (IDX)
      ****     MOVE    PTMODREQ-PREFKBN-NAME (IDX)
      ****                             TO  PTMODRES-PREFKBN-NAME(IDX)
               MOVE    PTMODREQ-CITYNAME (IDX)
                                       TO  PTMODRES-CITYNAME (IDX)
      *        所在地郡市区区分
               MOVE    PTMODREQ-CITYKBN (IDX)
                                       TO  PTMODRES-CITYKBN (IDX)
      *        労災レセ回数記載 基準年月
               MOVE    PTMODREQ-KIJYUNYM (IDX)
                                       TO  PTMODRES-KIJYUNYM (IDX)
               MOVE    PTMODREQ-RESEPUTO-COUNT (IDX)
                                       TO  PTMODRES-RESEPUTO-COUNT(IDX)
      *自賠責
      *        自賠責請求区分
               MOVE    PTMODREQ-JIBAISEIKBN (IDX)
                                       TO  PTMODRES-JIBAISEIKBN (IDX)
               MOVE    PTMODREQ-HOKENNAME (IDX)
                                       TO  PTMODRES-HOKENNAME (IDX)
               MOVE    PTMODREQ-RSI-DRCD (IDX)
                                       TO  PTMODRES-RSI-DRCD (IDX)
      *H29.6
      *        自賠責点数算定区分
               MOVE    PTMODREQ-JIBAIKBN (IDX)
                                       TO  PTMODRES-JIBAIKBN (IDX)
      *アフターケア
               MOVE    PTMODREQ-KANRITCHO-NUMBER(IDX)
                                       TO  PTMODRES-KANRITCHO-NUMBER
                                                                 (IDX)
               MOVE    PTMODREQ-DAMAGE-CLASS (IDX)
                                       TO  PTMODRES-DAMAGE-CLASS
                                                                 (IDX)
      *第三者行為
               MOVE    PTMODREQ-DS-GENBTKBN (IDX)
                                       TO  PTMODRES-DS-GENBTKBN
                                                                 (IDX)
               MOVE    PTMODREQ-DS-TOKKIKBN (IDX)
                                       TO  PTMODRES-DS-TOKKIKBN
                                                                 (IDX)
               END-IF
           END-PERFORM
      *
           .
       20021-INPUT-HKNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災保険情報チェック処理
      *****************************************************************
       20021-RSIHKN-CHK-SEC         SECTION.
      *
      *    PARAから編集
           PERFORM 200211-PARA-RSIINF-SEC
      *
           MOVE    SPA-P02SCRAREA      TO  APIS-P02SCRAREA
      *
           INITIALIZE                  WRK-PTRSIINF-AREA
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   40
      *        保険の種類・保険者番号なし対象外
               IF     (PTMODRES-INSURANCE-CLASS (IDX)  =   SPACE)
                  AND (PTMODRES-ACCIDENT        (IDX)  =   SPACE)
                   CONTINUE
               ELSE
      *            保険基本チェック
                   PERFORM 200211-RSIHKN-INIT-CHK-SEC
               END-IF
           END-PERFORM
      *    基本エラー有
           IF      IDX-ERR             >   ZERO
               MOVE    "50"            TO  WRK-ERRCD
               GO      TO      20021-RSIHKN-CHK-EXT
           END-IF
      *    保険ＳＰＡ編集
      *    更新分
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   WRK-PTRSI-MAX
               IF      (WRK-PTRSI-MODE  (IDY)  =   "2"   )
                   AND (WRK-PTRSI-IDX(IDY) NOT =   ZERO  )
                   MOVE    WRK-PTRSI-IDX  (IDY)    TO  IDH
                   PERFORM 200213-RSIINF-SYORI-SEC
               END-IF
               IF      (WRK-PTRSI-MODE  (IDY)  =   "1"   )
                   MOVE    ZERO                TO  IDH
                   PERFORM 200213-RSIINF-SYORI-SEC
               END-IF
               IF      (WRK-PTRSI-MODE  (IDY)  =   "3"   )
                   AND (WRK-PTRSI-IDX(IDY) NOT =   ZERO  )
                   MOVE    WRK-PTRSI-IDX  (IDY)    TO  IDH
      *            削除処理
                   PERFORM 200213-RSIINF-DELETE-SEC
               END-IF
           END-PERFORM
      *    エラー有
           IF      IDX-ERR             >   ZERO
               MOVE    "50"            TO  WRK-ERRCD
               GO      TO      20021-RSIHKN-CHK-EXT
           END-IF
      *
      *    ＳＰＡをパラメタへ
           PERFORM 200219-PARAINS-RSIINF-SEC
           .
       20021-RSIHKN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災保険情報パラメタ編集処理
      *****************************************************************
       200211-PARA-RSIINF-SEC      SECTION.
      *
           MOVE    ZERO                TO  IDR
           INITIALIZE                  TBL-PTRSIINF-AREA
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTRSIINF"          TO  PARA-FILEMEI
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
           PERFORM UNTIL   (FLG-PARA       =   1 )
                       OR  (IDR            >=  50)
               MOVE    PARA-DATA-REC       TO  PTRSIINF-REC
      *
               ADD     1                   TO  IDR
               MOVE    PARA-DATA-REC       TO  TBL-PTRSIINF-REC(IDR)
               MOVE    PARA-EDANUM         TO  TBL-PARA-EDANUM (IDR)
               MOVE    IDR                 TO  TBL-PARA-MAX
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *
           .
       200211-PARA-RSIINF-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災保険情報パラメタ作成処理
      *****************************************************************
       200219-PARAINS-RSIINF-SEC      SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTRSIINF"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           PERFORM VARYING     IDR     FROM    1   BY   1
                       UNTIL  (IDR         >   TBL-PARA-MAX )
               INITIALIZE                      PARA-REC
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    "P02"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "PTRSIINF"          TO  PARA-FILEMEI
               MOVE    IDR                 TO  PARA-EDANUM
      *
               MOVE    PARA-EDANUM         TO  TBL-PARA-EDANUM (IDR)
               MOVE    TBL-PTRSIINF-REC(IDR)
                                           TO  PARA-DATA-REC
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
           END-PERFORM
      *
           .
       200219-PARAINS-RSIINF-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険情報ＳＰＡ編集処理
      *****************************************************************
       200213-RSIINF-SPAHEN-SEC      SECTION.
      *
           IF      IDH                 =   ZERO
      *        新規分
               MOVE    SPACE               TO  PTRSIINF-REC
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPNUM             TO  PTRSI-HOSPNUM
               MOVE    SPA-GMN-PTID            TO  PTRSI-PTID
               MOVE    WRK-PTRSI-HKNKBN(IDY)   TO  PTRSI-HKNKBN
               MOVE    WRK-PTRSI-HKNNUM(IDY)   TO  PTRSI-HKNNUM
           ELSE
               MOVE    TBL-PTRSIINF-REC (IDH)  TO  PTRSIINF-REC
           END-IF
      *
           MOVE    WRK-PTRSI-SHOBYOYMD(IDY)
                                       TO  PTRSI-SHOBYOYMD
      *    療養開始年月日
           IF      WRK-PTRSI-HKNKBN (IDY)  =   "3"
      *        アフターケア
               MOVE    WRK-PTRSI-RYOSTYMD (IDY)
                                           TO  PTRSI-SINSATUYMD
               MOVE    WRK-PTRSI-RYOEDYMD (IDY)
                                           TO  PTRSI-SINSATUEND
           ELSE
               MOVE    WRK-PTRSI-RYOSTYMD (IDY)
                                           TO  PTRSI-RYOSTYMD
               MOVE    WRK-PTRSI-RYOEDYMD (IDY)
                                           TO  PTRSI-RYOEDYMD
           END-IF
      *
           MOVE    WRK-IN-IDX (IDY)    TO  IDM
      *    患者交付番号
           EVALUATE    WRK-PTRSI-HKNKBN (IDY)
               WHEN    "1"
      *    労働保険番号
                   MOVE    PTMODRES-ACCIDENT-NUMBER (IDM)
                                           TO  PTRSI-KOFUNUM
               WHEN    "2"
      *    年金証書番号
                   MOVE    PTMODRES-PENSIONCERTIFICATE (IDM)
                                           TO  PTRSI-KOFUNUM
               WHEN    "3"
      *    健康管理手帳番号
                   MOVE    PTMODRES-KANRITCHO-NUMBER (IDM)
                                           TO  PTRSI-KOFUNUM
           END-EVALUATE
      *    被保険者名（患者氏名）
           IF      PTRSI-HIHKNJANAME       =   SPACE
               MOVE    SPA-GMN-NAME        TO  PTRSI-HIHKNJANAME
           END-IF
      *    事業所名称
           MOVE    PTMODRES-JIGYOUNAME (IDM)
                                           TO  PTRSI-JIGYOUNAME
      *    所在地都道府県名
           MOVE    PTMODRES-PREFNAME (IDM)
                                           TO  PTRSI-PREFNAME
      *    所在地都道府県区分
           MOVE    PTMODRES-PREFKBN (IDM)
                                           TO  PTRSI-PREFKBN
      *    所在地郡市区名
           MOVE    PTMODRES-CITYNAME (IDM)
                                           TO  PTRSI-CITYNAME
      *    所在地郡市区区分
           MOVE    PTMODRES-CITYKBN (IDM)
                                           TO  PTRSI-CITYKBN
      *    新継再別
           MOVE    PTMODRES-SINKEI (IDM)
                                           TO  PTRSI-SINKEI
      *    転帰事由
           MOVE    PTMODRES-TENKI (IDM)
                                           TO  PTRSI-TENKI
      *    四肢特例区分
           MOVE    PTMODRES-SISIKBN (IDM)
                                           TO  PTRSI-SISIKBN
      *    損傷区分
           MOVE    PTMODRES-DAMAGE-CLASS(IDM)(1:2)
                                           TO  PTRSI-SONSHOUKBN
      *    損傷区分−枝番
           MOVE    PTMODRES-DAMAGE-CLASS (IDM)(3:1)
                                           TO  PTRSI-SONSHOUKBN-EDANUM
      *    傷病の部位
           MOVE    PTMODRES-DISEASE-LOCATION (IDM)
                                           TO  PTRSI-COMMENT
      *    災害区分
           MOVE    PTMODRES-ACCIDENT-CLASS (IDM)
                                           TO  PTRSI-SAIGAIKBN
      *    自賠責患者請求区分
           MOVE    PTMODRES-JIBAISEIKBN (IDM)
                                           TO  PTRSI-JIBAISEIKBN
      *H29.6
      *    自賠責点数算定区分
           MOVE    PTMODRES-JIBAIKBN (IDM)
                                           TO  PTRSI-JIBAIKBN
      *    自賠責担当医コード
           MOVE    PTMODRES-RSI-DRCD (IDM)
                                           TO  PTRSI-DRCD
      *    自賠責保険会社名
           MOVE    PTMODRES-HOKENNAME (IDM)
                                           TO  PTRSI-HOKENNAME
      *    労災レセ回数記載：基準年月
           MOVE    WRK-PTRSI-KIJYUNYM(IDY)
                                           TO  PTRSI-KIJYUNYM
      *    労災レセ回数記載：回数
           MOVE    WRK-PTRSI-KAISU(IDY)    TO  PTRSI-KAISU
      *    労働基準監督署コード
           MOVE    PTMODRES-LABOR-ST-CODE (IDM)
                                           TO  PTRSI-SYOCD
      *
      *    第三者　現物給付計算区分
           MOVE    PTMODRES-DS-GENBTKBN (IDM)
                                           TO  PTRSI-DAISAN-GENBTKBN
      *    第三者　特記事項区分
           MOVE    PTMODRES-DS-TOKKIKBN (IDM)
                                           TO  PTRSI-DAISAN-TOKKIKBN
      *
      *    パラメタ登録
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "AP12"              TO  PARA-GYOUMUID
           MOVE    "PTRSIINF"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "AP12"              TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTRSIINF"          TO  PARA-FILEMEI
           MOVE    1                   TO  PARA-EDANUM
      *
           MOVE    PTRSIINF-REC        TO  PARA-DATA-REC
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
       200213-RSIINF-SPAHEN-EXT.
           EXIT.
      *****************************************************************
      *    保険情報基本チェック処理
      *****************************************************************
       200211-RSIHKN-INIT-CHK-SEC     SECTION.
      *
      *    エラー用編集
      *    労災区分
           MOVE    PTMODREQ-ACCIDENT (IDX)
                                       TO  WRK-ERR-NUM
           MOVE    PTMODREQ-ACCIDENT-NAME (IDX)
                                       TO  WRK-ERR-NUMMEI
           MOVE    SPACE               TO  WRK-CHK-HKNNUM
      *
           ADD     1                   TO  IDY
      *
      *    処理区分
           EVALUATE    PTMODREQ-MAIN-MODE (IDX)
               WHEN    "New"
      *        新規
                   MOVE    "1"                 TO  WRK-PTRSI-MODE (IDY)
               WHEN    "Modify"
      *        修正
                   MOVE    "2"                 TO  WRK-PTRSI-MODE (IDY)
               WHEN    "Delete"
      *        削除
                   MOVE    "3"                 TO  WRK-PTRSI-MODE (IDY)
               WHEN    SPACE
      *        変更なし
                   MOVE    "0"                 TO  WRK-PTRSI-MODE (IDY)
               WHEN    OTHER
                   MOVE    "AP19"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
           END-EVALUATE
      *    労災区分
      *    労災保険区分
           MOVE    PTMODREQ-ACCIDENT (IDX)
                                       TO  WRK-PTRSI-HKNKBN(IDY)
           EVALUATE    PTMODREQ-ACCIDENT (IDX)
               WHEN    "1"
                   MOVE    "短期給付"      TO  PTMODRES-ACCIDENT-NAME
                                                                  (IDX)
                   MOVE    "971"           TO  WRK-PTRSI-HKNNUM   (IDY)
               WHEN    "2"
                   MOVE    "傷病年金"      TO  PTMODRES-ACCIDENT-NAME
                                                                  (IDX)
                   MOVE    "971"           TO  WRK-PTRSI-HKNNUM   (IDY)
               WHEN    "3"
                   MOVE    "アフターケア"  TO  PTMODRES-ACCIDENT-NAME
                                                                  (IDX)
                   MOVE    "971"           TO  WRK-PTRSI-HKNNUM   (IDY)
               WHEN    "4"
                   MOVE    "自賠責保険"    TO  PTMODRES-ACCIDENT-NAME
                                                                  (IDX)
                   MOVE    "973"           TO  WRK-PTRSI-HKNNUM   (IDY)
               WHEN    "5"
                   MOVE    "公務災害"      TO  PTMODRES-ACCIDENT-NAME
                                                                  (IDX)
                   MOVE    "971"           TO  WRK-PTRSI-HKNNUM   (IDY)
               WHEN    "6"
                   MOVE    "第三者行為"    TO  PTMODRES-ACCIDENT-NAME
                                                                  (IDX)
                   MOVE    "970"           TO  WRK-PTRSI-HKNNUM   (IDY)
               WHEN    OTHER
                   MOVE    "AP20"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
           END-EVALUATE
           MOVE    PTMODRES-ACCIDENT-NAME (IDX)
                                       TO  WRK-ERR-NUMMEI
      *
      *    保険種類
           IF      WRK-PTRSI-HKNNUM  (IDY)  NOT =   SPACE
               INITIALIZE                  HKNNUM-REC
               MOVE    WRK-PTRSI-HKNNUM  (IDY) TO  HKN-HKNNUM
               PERFORM 9002-HKNNUM-KEYREAD-SEC
               IF      FLG-HKNNUM          =   ZERO
                   MOVE    HKN-TANSEIDONAME    TO  WRK-PTRSI-HKNNUM-NAME
                                                                 (IDY)
               ELSE
                   MOVE    WRK-PTRSI-HKNNUM  (IDY)
                                               TO  WRK-ERR-NUM
      **           MOVE    "32"                TO  WRK-ERRCD
                   MOVE    "AP32"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           IF     (WRK-PTRSI-HKNNUM (IDY)  =   PTMODREQ-INSURANCE-CLASS
                                                               (IDX))
              OR  (PTMODREQ-INSURANCE-CLASS (IDX)  =   SPACE )
               MOVE    WRK-PTRSI-HKNNUM (IDY)
                                           TO  PTMODRES-INSURANCE-CLASS
                                                                  (IDX)
               MOVE    WRK-PTRSI-HKNNUM-NAME (IDY)
                                           TO  PTMODRES-INSURANCE-NAME
                                                                  (IDX)
           ELSE
               MOVE    WRK-PTRSI-HKNNUM (IDY)
                                           TO  PTMODRES-INSURANCE-CLASS
                                                                  (IDX)
               MOVE    WRK-PTRSI-HKNNUM-NAME (IDY)
                                           TO  PTMODRES-INSURANCE-NAME
                                                                  (IDX)
      *        保険種類警告
               MOVE    "K3"                TO  WRK-ERRCD
               PERFORM 290-WARMSG-HEN-SEC
           END-IF
      *
      *    保険ＩＤ数値チェック
           IF      PTMODREQ-MAIN-INSURANCE-ID (IDX)  =   SPACE
               MOVE    ZERO                TO  WRK-PTRSI-HKNID (IDY)
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    PTMODREQ-MAIN-INSURANCE-ID (IDX)
                                           TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF      SNUM-RC         NOT =   ZERO
                   MOVE    "AP21"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-PTRSI-HKNID(IDY)
               END-IF
           END-IF
      *    公費ＩＤ数値チェック
           IF      PTMODREQ-INSURANCE-ID (IDX)  =   SPACE
               MOVE    ZERO                TO  WRK-PTRSI-KOHID (IDY)
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    PTMODREQ-INSURANCE-ID (IDX)
                                           TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF      SNUM-RC         NOT =   ZERO
                   MOVE    "AP22"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-PTRSI-KOHID(IDY)
               END-IF
           END-IF
      *
      *    保険ＩＤ存在チェック
           IF     (WRK-PTRSI-HKNID(IDY)    =   ZERO )
             AND  (WRK-PTRSI-KOHID(IDY)    =   ZERO )
      *        新規以外はエラー
               IF      WRK-PTRSI-MODE (IDY)    NOT =   "1"
                   MOVE    "AP23"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           ELSE
      *        新規はエラー
               IF      WRK-PTRSI-MODE (IDY)        =   "1"
                   MOVE    "AP24"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               ELSE
                   PERFORM VARYING     IDH     FROM    1   BY  1
                           UNTIL      (IDH     >   TBL-PARA-MAX )
                       IF    (TBL-PTRSI-HKNID (IDH)
                                           =   WRK-PTRSI-HKNID(IDY))
                         AND (TBL-PTRSI-KOHID (IDH)
                                           =   WRK-PTRSI-KOHID(IDY))
                           MOVE    IDH             TO  WRK-PTRSI-IDX
                                                             (IDY)
                           MOVE    TBL-PTRSI-HKNNUM (IDH)
                                                   TO  WRK-CHK-HKNNUM
                           MOVE    TBL-PTRSI-HKNKBN (IDH)
                                                   TO  WRK-CHK-HKNKBN
                           MOVE    TBL-PARA-MAX    TO  IDH
                       END-IF
                   END-PERFORM
                   IF      WRK-PTRSI-IDX  (IDY)    =   ZERO
                       MOVE    "AP23"              TO  SPA-ERRCD
                       PERFORM 290-ERROR-HEN-SEC
                  END-IF
               END-IF
           END-IF
      *
           IF      WRK-PTRSI-MODE (IDY)    =   "2"
                                           OR  "3"
      *        保険変更はエラー
               IF     (WRK-PTRSI-HKNKBN(IDY)   =   WRK-CHK-HKNKBN )
                  AND (WRK-PTRSI-HKNNUM(IDY)   =   WRK-CHK-HKNNUM )
                   CONTINUE
               ELSE
      ***          MOVE    "25"                TO  WRK-ERRCD
                   MOVE    "AP25"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
      *    傷病の部位
           IF      PTMODREQ-DISEASE-LOCATION(IDX) NOT =   SPACE
      *        半角を全角へ
               MOVE    PTMODREQ-DISEASE-LOCATION(IDX)
                                           TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  WRK-PTRSI-COMMENT (IDY)
               IF      FLG-ERR             =   1
      ***          MOVE    "35"                TO  WRK-ERRCD
                   MOVE    "AP35"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               ELSE
                   MOVE    WRK-PTRSI-COMMENT(IDY)
                                           TO  PTMODRES-DISEASE-LOCATION
                                                              (IDX)
               END-IF
           END-IF
      *
      *    事業所名称
           IF      PTMODREQ-JIGYOUNAME(IDX) NOT =   SPACE
      *        半角を全角へ
               MOVE    PTMODREQ-JIGYOUNAME(IDX)
                                           TO  WRK-MAE-INPUT
               MOVE    80                  TO  WRK-LEN
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  WRK-PTRSI-JIGYOUNAME(IDY)
               IF      FLG-ERR             =   1
                   MOVE    "AP26"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               ELSE
                   MOVE    WRK-PTRSI-JIGYOUNAME(IDY)
                                               TO  PTMODRES-JIGYOUNAME
                                                              (IDX)
               END-IF
           END-IF
      *
      *    所在地郡市区名
           IF      PTMODREQ-CITYNAME(IDX) NOT =   SPACE
      *        半角を全角へ
               MOVE    PTMODREQ-CITYNAME(IDX)
                                           TO  WRK-MAE-INPUT
               MOVE    80                  TO  WRK-LEN
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  PTMODRES-CITYNAME(IDY)
               IF      FLG-ERR             =   1
                   MOVE    "AP36"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
      *    保険会社名称
           IF      PTMODREQ-HOKENNAME(IDX) NOT =   SPACE
      *        半角を全角へ
               MOVE    PTMODREQ-HOKENNAME(IDX)
                                           TO  WRK-MAE-INPUT
               MOVE    80                  TO  WRK-LEN
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  WRK-PTRSI-HOKENNAME(IDY)
               IF      FLG-ERR             =   1
                   MOVE    "AP34"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               ELSE
                   MOVE    WRK-PTRSI-HOKENNAME(IDY)
                                               TO  PTMODRES-HOKENNAME
                                                              (IDX)
               END-IF
           END-IF
      *    傷病年月日
           MOVE    PTMODREQ-DISEASE-DATE(IDX)  TO  WRK-HEN-DATE
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD            TO  WRK-PTRSI-SHOBYOYMD(IDY)
           IF      WRK-SYMD        NOT =   SPACE
               PERFORM 803-ORCSDAY-CHEK-SEC
               IF      FLG-DAYERR          =   1
                   MOVE    "AP27"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
      *    療養開始年月日
           MOVE    PTMODREQ-START-DATE(IDX)    TO  WRK-HEN-DATE
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD            TO  WRK-PTRSI-RYOSTYMD (IDY)
           IF      WRK-SYMD        NOT =   SPACE
               PERFORM 803-ORCSDAY-CHEK-SEC
               IF      FLG-DAYERR          =   1
                   MOVE    "AP28"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
           MOVE    PTMODREQ-END-DATE(IDX)  TO  WRK-HEN-DATE
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD            TO  WRK-PTRSI-RYOEDYMD (IDY)
           IF      WRK-SYMD        NOT =   SPACE
               PERFORM 803-ORCSDAY-CHEK-SEC
               IF      FLG-DAYERR          =   1
                   MOVE    "AP29"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
      *    労災レセ回数記載 基準年月
           IF      PTMODREQ-KIJYUNYM(IDX)  =   SPACE
               MOVE    SPACE               TO  WRK-PTRSI-KIJYUNYM (IDY)
           ELSE
               MOVE    PTMODREQ-KIJYUNYM(IDX)  TO  WRK-HEN-DATE(1:7)
               PERFORM 802-DAYHEN02-SEC
               MOVE    WRK-SYMD(1:6)       TO  WRK-PTRSI-KIJYUNYM (IDY)
               MOVE    "01"                TO  WRK-SDD
               PERFORM 803-ORCSDAY-CHEK-SEC
               IF      FLG-DAYERR          =   1
                   MOVE    "AP30"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               END-IF
           END-IF
      *    労災レセ回数記載：回数
           IF      PTMODREQ-RESEPUTO-COUNT (IDX)    =   SPACE
               MOVE    ZERO            TO  WRK-PTRSI-KAISU (IDY)
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    PTMODREQ-RESEPUTO-COUNT (IDX)
                                           TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF      SNUM-RC         NOT =   ZERO
                   MOVE    "AP31"              TO  SPA-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-PTRSI-KAISU(IDY)
               END-IF
           END-IF
      *
           MOVE    IDX                 TO  WRK-IN-IDX (IDY)
           MOVE    IDY                 TO  WRK-PTRSI-MAX
      *
           .
       200211-RSIHKN-INIT-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    保険情報ＳＰＡ編集処理
      *****************************************************************
       200213-RSIINF-SYORI-SEC      SECTION.
      *
           PERFORM 200213-RSIINF-SPAHEN-SEC
      *
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    ZERO                TO  FLG-HKNEND
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPACE               TO  SPA-P02D-AREA
           INITIALIZE                      SPA-P02D-AREA
           PERFORM UNTIL    (FLG-HKNEND    =   1   )
      *
               MOVE    SPACE               TO  SPA-PIDCD
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    SPACE               TO  SPA-ERRMSG
      *        労災画面初期表示
               MOVE    SPA-P02KYOUTU       TO  SPA-WORK
               MOVE    SPA-AREA            TO  SPA-COMMON
               MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
               MOVE    "PUTG"              TO  MCP-STATUS
               MOVE    "CLICKED"           TO  MCP-EVENT
               MOVE    "APIV3"             TO  MCP-WIDGET
      *
               CALL    "ORCGP02D"          USING
                                           MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *
               MOVE    SPA-COMMON      TO  SPA-AREA
               MOVE    SPA-FREE        TO  SPA-P02SCRAREA
               MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
               IF     (SPA-ERRCD           =   SPACE )
                  AND (SPA-PIDCD           =   SPACE )
                   MOVE    1               TO  FLG-HKNEND
               END-IF
      *
               IF      SPA-ERRCD       NOT =   SPACE
      *            エラー用  労災区分
                   MOVE    SPA-GMN2D-HKNKBN
                                           TO  WRK-ERR-NUM
                   MOVE    SPA-GMN2D-HKNKBN-G(3:)
                                           TO  WRK-ERR-NUMMEI
      *            エラーメッセージ
                   MOVE    SPA-ERRMSG      TO  WRK-ERRMSG
      ********     PERFORM 8901-PTERRCD-SET-SEC
      *            警告は続行
                   IF     (SPA-ERRCD(1:1)      =   "K" )
                       OR (WRK-ERRMSG(1:4)     =   "警告")
                       PERFORM 290-WARMSG-HEN-SEC
                       MOVE    SPACE               TO  SPA-ERRCD
      *                続行
                       MOVE    "1"                 TO  SPA-RSIAPI-FLG
                   END-IF
               END-IF
               IF     (SPA-ERRCD       NOT =   SPACE )
                   MOVE    "H1"            TO  WRK-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
                   MOVE    1               TO  FLG-HKNEND
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD       NOT =   SPACE
      *        画面内容から編集
      ***      PERFORM 2002131-GMNHENSYU-SEC
               GO      TO      200213-RSIINF-SYORI-EXT
           END-IF
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "AP12"              TO  PARA-GYOUMUID
           MOVE    "PTRSIINF"          TO  PARA-FILEMEI
           MOVE    1                   TO  PARA-EDANUM
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PARA        NOT =   ZERO
               DISPLAY "PARA READ ERR:"  FLG-PARA
               GO      TO      200213-RSIINF-SYORI-EXT
           END-IF
           MOVE    PARA-DATA-REC   TO  PTRSIINF-REC
           IF      IDH             =   ZERO
      *        新規分追加
               ADD     1               TO  TBL-PARA-MAX
               MOVE    TBL-PARA-MAX    TO  IDH
      *        TBL位置
               MOVE    IDH             TO  WRK-PTRSI-IDX(IDY)
           END-IF
           MOVE    PTRSIINF-REC    TO  TBL-PTRSIINF-REC (IDH)
           MOVE    WRK-PTRSI-MODE (IDY)
                                   TO  TBL-PTRSI-MODE (IDH)
      *    ＳＰＡ編集
           IF      PTRSI-HKNKBN        =   "6"
      *        第三者行為
               PERFORM 2002132-SPAKOH-HEN-SEC
           ELSE
      *        労災・自賠責
               PERFORM 2002131-SPAHKN-HEN-SEC
           END-IF
           .
       200213-RSIINF-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ＳＰＡ保険編集処理
      *****************************************************************
       2002131-SPAHKN-HEN-SEC         SECTION.
      *
           IF      WRK-PTRSI-MODE (IDY)  =   "1"
      *        新規
               IF      (SPA-PTHKN-MAX      >=  TBL-MAX-HKN )
                   MOVE    "0107"          TO  SPA-ERRCD
                   GO      TO      2002131-SPAHKN-HEN-EXT
               ELSE
                   ADD     1               TO  SPA-PTHKN-MAX
                   MOVE    SPA-PTHKN-MAX   TO  IDX
                   INITIALIZE                  SPA-PTHKNINF-TBL (IDX)
               END-IF
           ELSE
               PERFORM VARYING IDX-R       FROM    1   BY  1
                       UNTIL   IDX-R       >   SPA-PTHKN-MAX
                  IF      (SPA-PTHKN-HKNNUM (IDX-R)  =   "971"  OR
                                                         "973" )
                     AND  (SPA-PTHKN-HKNID  (IDX-R)
                                               =   PTRSI-HKNID )
                      MOVE    IDX-R            TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    SPA-NAI-HKNID       TO  SPA-PTHKN-HKNID (IDX)
           MOVE    SPA-GMN-HKNNUM      TO  SPA-PTHKN-HKNNUM (IDX)
           MOVE    SPA-GMN-KIGO        TO  SPA-PTHKN-KIGO    (IDX)
           MOVE    SPA-GMN-NUM         TO  SPA-PTHKN-NUM     (IDX)
           MOVE    SPA-NAI-HTEKSTYMD   TO  SPA-PTHKN-TEKSTYMD (IDX)
           MOVE    SPA-NAI-HTEKEDYMD   TO  SPA-PTHKN-TEKEDYMD (IDX)
      *
           MOVE    SPA-NAI-SAKJOKBN    TO  SPA-PTHKN-SAKJOKBN (IDX)
      *
           MOVE    SPA-GMN-HKNNUMMEI   TO  SPA-PTHKN-HKNNUM-MEI(IDX)
      *
      *    処理区分
           MOVE    TBL-PTRSI-MODE (IDH)    TO  SPA-PTHKN-MODE (IDX)
      *
           .
       2002131-SPAHKN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ＳＰＡ保険編集処理
      *****************************************************************
       2002132-SPAKOH-HEN-SEC         SECTION.
      *
      *    新規
           IF      WRK-PTRSI-MODE (IDY)  =   "1"
      *        新規
               IF      (SPA-PTKOH-MAX      >=  TBL-MAX-KOH )
                   MOVE    "0107"          TO  SPA-ERRCD
                   GO      TO      2002132-SPAKOH-HEN-EXT
               ELSE
                   ADD     1               TO  SPA-PTKOH-MAX
                   MOVE    SPA-PTKOH-MAX   TO  IDX
                   INITIALIZE                  SPA-PTKOHINF-TBL (IDX)
               END-IF
           ELSE
               PERFORM VARYING IDX-R       FROM    1   BY  1
                       UNTIL   IDX-R       >   SPA-PTKOH-MAX
                  IF      (SPA-PTKOH-KOHNUM (IDX-R)  =   "970" )
                     AND  (SPA-PTKOH-KOHID  (IDX-R)
                                               =   PTRSI-KOHID )
                      MOVE   IDX-R             TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    SPA-NAI2D-KOHID     TO  SPA-PTKOH-KOHID  (IDX)
           MOVE    SPA-NAI2D-HKNNUM    TO  SPA-PTKOH-KOHNUM (IDX)
      *    部位を受給者番号へ
           MOVE    SPA-GMN2D-COMMENT   TO  SPA-PTKOH-JKYSNUM (IDX)
           MOVE    SPA-NAI2D-RYOSTYMD  TO  SPA-PTKOH-TEKSTYMD (IDX)
           MOVE    SPA-NAI2D-RYOEDYMD  TO  SPA-PTKOH-TEKEDYMD (IDX)
      *
           MOVE    SPA-NAI-SAKJOKBN    TO  SPA-PTKOH-SAKJOKBN (IDX)
      *
      *    処理区分
           MOVE    TBL-PTRSI-MODE (IDH)    TO  SPA-PTKOH-MODE (IDX)
      *
           .
       2002132-SPAKOH-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    削除チェック処理
      *****************************************************************
       200213-RSIINF-DELETE-SEC         SECTION.
      *
           MOVE    "1"                 TO  TBL-PTRSI-SAKUJOKBN(IDH)
           MOVE    WRK-PTRSI-MODE (IDY)
                                       TO  TBL-PTRSI-MODE (IDH)
      *
           MOVE    TBL-PTRSIINF-REC (IDH)  TO  PTRSIINF-REC
      *    ＳＰＡ編集
           IF      PTRSI-HKNKBN        =   "6"
      *        第三者
               PERFORM VARYING IDX-R       FROM    1   BY  1
                       UNTIL   IDX-R       >   SPA-PTKOH-MAX
                  IF      (SPA-PTKOH-KOHNUM (IDX-R)  =   "970" )
                     AND  (SPA-PTKOH-SAKJOKBN(IDX-R) =   SPACE )
                     AND  (SPA-PTKOH-KOHID  (IDX-R)
                                               =   PTRSI-KOHID )
                      MOVE    "1"              TO  SPA-PTKOH-SAKJOKBN
                                                           (IDX-R)
                   END-IF
               END-PERFORM
           ELSE
               PERFORM VARYING IDX-R       FROM    1   BY  1
                       UNTIL   IDX-R       >   SPA-PTHKN-MAX
                  IF      (SPA-PTHKN-HKNNUM (IDX-R)  =   "971"  OR
                                                         "973" )
                     AND  (SPA-PTHKN-SAKJOKBN(IDX-R) =   SPACE )
                     AND  (SPA-PTHKN-HKNID  (IDX-R)
                                               =   PTRSI-HKNID )
                      MOVE    "1"              TO  SPA-PTHKN-SAKJOKBN
                                                           (IDX-R)
                   END-IF
               END-PERFORM
           END-IF
           .
       200213-RSIINF-DELETE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       20022-HKNCOMBI-CHKSYORI-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    ZERO                TO  FLG-HKNEND
           MOVE    SPACE               TO  SPA-MOTOPG
      *    被保険者名の自動変更はしない
           MOVE    1                   TO  SPA-NAI-HIHKNFLG
      *    本人・家族区分の自動設定なし
           MOVE    "1"                 TO  SPA-NAI-HONKZKKBN-FLG
      *    世帯主名の自動設定なし
           MOVE    "0"                 TO  SPA-NAI-SETAIKBN-FLG
      *
      *    LNK-SCRSPA にAPI内容を登録
      **** MOVE    APIS-P02SCRAREA     TO  LNK-SCRSPA
      *
           MOVE    ZERO                TO  SPA-NAI-HKNSTR
           MOVE    ZERO                TO  SPA-NAI-KOHSTR
      *
           PERFORM UNTIL    (FLG-HKNEND    =   1   )
               MOVE    SPA-P02SCRAREA      TO  SPA-FREE
               MOVE    SPA-P02KYOUTU       TO  SPA-WORK
               MOVE    SPA-AREA            TO  SPA-COMMON
      *        基本情報チェック
               MOVE    "CLICKED"           TO  MCP-EVENT
               MOVE    "APIV3D"            TO  MCP-WIDGET
               MOVE    "PUTG"              TO  MCP-STATUS
               CALL    "ORCGP02W1"     USING   MCPAREA
                                               SPAAREA
                                                LINKAREA
                                                SCRAREA
               MOVE    SPA-COMMON          TO  SPA-AREA
               MOVE    SPA-FREE            TO  SPA-P02SCRAREA
               MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
               MOVE    SPACE               TO  SPA-MOTOPG
               MOVE    ZERO                TO  SPA-FLG-END
               MOVE    SPACE               TO  WRK-ERR-AREA
      *
               IF     (SPA-ERRCD           =   SPACE )
                  AND (SPA-PIDCD           =   SPACE )
                   MOVE    1               TO  FLG-HKNEND
               END-IF
      *
               IF      SPA-ERRCD       NOT =   SPACE
      *            保険エラー
                   IF      SPA-GMN-CUR         =   98
                       MOVE    SPA-GMN-HKNNUM  TO  WRK-ERR-NUM
                       MOVE    SPA-GMN-HKNNUMMEI
                                               TO  WRK-ERR-NUMMEI
                       MOVE    SPACE           TO  WRK-ERR-NUMMEI2
                   END-IF
                   IF     (SPA-GMN-CUR         =   99   )
                     AND  (SPA-NAI-KOHSTR  NOT =   ZERO )
                     AND  (SPA-KOH-ERRIDX  NOT =   ZERO )
      *                公費エラー
                       MOVE    SPA-GMN-KOHNUM (SPA-KOH-ERRIDX)
                                               TO  WRK-ERR-NUM
                       MOVE    SPA-GMN-KOHNUMMEI (SPA-KOH-ERRIDX)
                                               TO  WRK-ERR-NUMMEI
                       IF      SPA-GMN-JKYSNUM(SPA-KOH-ERRIDX)
                                               =   SPACE
                           MOVE    SPA-GMN-FTNJANUM (SPA-KOH-ERRIDX)
                                               TO  WRK-ERR-NUMMEI2
                       ELSE
                           MOVE    SPA-GMN-JKYSNUM (SPA-KOH-ERRIDX)
                                               TO  WRK-ERR-NUMMEI2
                       END-IF
                   END-IF
      *            エラーメッセージ
                   PERFORM 8901-PTERRCD-SET-SEC
      *            警告は続行
                   IF     (SPA-ERRCD(1:1)      =   "K" )
                       OR (WRK-ERRMSG(1:4)     =   "警告")
                       PERFORM 290-WARMSG-HEN-SEC
                       MOVE    SPACE               TO  SPA-ERRCD
                   END-IF
               END-IF
               IF     (SPA-ERRCD       NOT =   SPACE )
                   MOVE    "H1"            TO  WRK-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
                   MOVE    1               TO  FLG-HKNEND
               END-IF
               IF     (SPA-PIDCD       NOT =   SPACE )
                   PERFORM 200221-PIDCDMSG-SEC
                   MOVE    "P2"            TO  WRK-ERRCD
                   PERFORM 290-ERROR-HEN-SEC
                   MOVE    1               TO  FLG-HKNEND
               END-IF
           END-PERFORM
      *
           IF      SPA-GMN-CUR         =   90
               MOVE    ZERO            TO  FLG-HKNKOHERR
           ELSE
      *        保険組合せなし
               MOVE    1               TO  FLG-HKNKOHERR
           END-IF
           MOVE    SPACE               TO  LNK-SCRSPA
      *
           .
       20022-HKNCOMBI-CHKSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ編集処理
      *****************************************************************
       200221-PIDCDMSG-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-ERR-AREA
      *    保険切替え
           IF      SPA-HKN-IDX1    NOT =   ZERO
               MOVE    SPA-GMN-HKNNUM      TO  WRK-ERR-NUM
               MOVE    SPA-GMN-HKNNUMMEI   TO  WRK-ERR-NUMMEI
               MOVE    SPACE               TO  WRK-ERR-NUMMEI2
               IF      SPA-STR-TEKSTYMD    NOT =   SPACE
                   MOVE    SPA-STR-TEKSTYMD    TO  WRK-SYMD
                   PERFORM 803-ORCSDAY-CHEK02-SEC
                   MOVE    WRK-HENYMD-Z        TO  WRK-ERR-YMD
                   MOVE    "1"                 TO  WRK-ERR-CNT
               ELSE
                   IF      SPA-END-TEKEDYMD    NOT =   SPACE
                       MOVE    SPA-END-TEKEDYMD    TO  WRK-SYMD
                       PERFORM 803-ORCSDAY-CHEK02-SEC
                       MOVE    WRK-HENYMD-Z        TO  WRK-ERR-YMD
                       MOVE    "2"                 TO  WRK-ERR-CNT
                   END-IF
               END-IF
           END-IF
      *    公費切替え
           IF      SPA-KOH-IDX1    NOT =   ZERO
               MOVE    SPA-GMN-KOHNUM (SPA-KOH-IDX1)
                                               TO  WRK-ERR-NUM
               MOVE    SPA-GMN-KOHNUMMEI (SPA-KOH-IDX1)
                                               TO  WRK-ERR-NUMMEI
               IF      SPA-GMN-JKYSNUM(SPA-KOH-IDX1)
                                               =   SPACE
                   MOVE    SPA-GMN-FTNJANUM (SPA-KOH-IDX1)
                                               TO  WRK-ERR-NUMMEI2
               ELSE
                   MOVE    SPA-GMN-JKYSNUM (SPA-KOH-IDX1)
                                               TO  WRK-ERR-NUMMEI2
               END-IF
               IF      SPA-HKN2-TEKSTYMD   NOT =   SPACE
                   MOVE    SPA-HKN2-TEKSTYMD   TO  WRK-SYMD
                   PERFORM 803-ORCSDAY-CHEK02-SEC
                   MOVE    WRK-HENYMD-Z        TO  WRK-ERR-YMD
                   MOVE    "1"                 TO  WRK-ERR-CNT
               ELSE
                   IF      SPA-HKN-TEKEDYMD    NOT =   SPACE
                       MOVE    SPA-HKN-TEKEDYMD    TO  WRK-SYMD
                       PERFORM 803-ORCSDAY-CHEK02-SEC
                       MOVE    WRK-HENYMD-Z        TO  WRK-ERR-YMD
                       MOVE    "2"                 TO  WRK-ERR-CNT
                   END-IF
               END-IF
           END-IF
      *
           .
       200221-PIDCDMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険登録処理
      *****************************************************************
       2003-UPDATE-SYORI-SEC     SECTION.
      *
      *    患者番号判定
           PERFORM 20011-PTINF-INITCHK-SEC
      *
           IF      WRK-RESPONSE-NUMBER NOT =   "03"
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        SPA-FREE
               PERFORM 2031-SPA-PARA-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               IF      PTMODRES-RESPONSE-NUMBER    =   SPACE
                   MOVE    "02"            TO  PTMODRES-RESPONSE-NUMBER
               END-IF
               GO      TO      2003-UPDATE-SYORI-EXT
           END-IF
      *
      *    確認メッセージあり
           IF      WRK-SELECT-CODE NOT =   SPACE
               EVALUATE    PTMODREQ-SELECT-ANSWER
               WHEN    "Ok"
               WHEN    "OK"
                   CONTINUE
               WHEN    "No"
               WHEN    "NO"
                   MOVE    "02"        TO  PTMODRES-RESPONSE-NUMBER
                   PERFORM 2901-PARA-INS-SEC
                   MOVE    "61"        TO  WRK-ERRCD
               WHEN    OTHER
                   MOVE    "03"        TO  PTMODRES-RESPONSE-NUMBER
                   MOVE    "21"        TO  WRK-ERRCD
               END-EVALUATE
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      2003-UPDATE-SYORI-EXT
           END-IF
      *    登録処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20031-HKNCOMBI-UPDATE-SEC
           END-IF
      *    エラー判定
           IF      WRK-ERRCD           =   SPACE
               IF      IDX-ERR             >   ZERO
                   MOVE    "50"               TO  WRK-ERRCD
               END-IF
           END-IF
      *    情報編集
           IF      WRK-ERRCD           =   SPACE
      *        登録内容の再編集
               PERFORM 20012-PTINF-HEN-SEC
      *
               MOVE    2               TO  FLG-HENSYU
               PERFORM 20013-RSIINF-HENSYU-SEC
           ELSE
               MOVE    1               TO  FLG-HENSYU
               PERFORM 20013-RSIINF-HENSYU-SEC
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
               IF      FLG-CANSEL      =   ZERO
      *            SPA再編集
                   PERFORM 2901-PARA-INS-SEC
                   PERFORM 2008-SPATMP-INS-SEC
               END-IF
               MOVE    "03"            TO  PTMODRES-RESPONSE-NUMBER
           END-IF
      *
           .
       2003-UPDATE-SYORI-EXT.
           EXIT.
      *
      ****************************************************************
      *    登録処理
      ****************************************************************
       20031-HKNCOMBI-UPDATE-SEC           SECTION.
      *
      *    保険組合せの変更による履歴チェック
           MOVE    ZERO                TO  SPA-P02-SYORI
           MOVE    "APICHK"            TO  SPA-P02-GAMEN
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
           MOVE    SPA-AREA            TO  SPA-COMMON
      *    更新サブへ
           CALL    "ORCGP029"          USING   MCPAREA
                                               SPAAREA
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           IF     (SPA-PIDCD       NOT =   SPACE )
      *        確認メッセージとする
               PERFORM 292-SELECT-HEN-SEC
               MOVE    "20"                TO  WRK-ERRCD
               MOVE    "S"                 TO  WRK-ERRKBN
               MOVE    SPACE               TO  SPA-PIDCD
           END-IF
           IF     (WRK-ERRCD           =   SPACE )
      *        保険登録
               PERFORM 200233-PTHKN-INSERT-SEC
               IF    WRK-ERRCD           =   SPACE
      *            保険・公費ＩＤ登録
                   PERFORM 20042-PTNUM-UPDATE-SEC
               END-IF
               IF     (WRK-ERRCD       NOT =   SPACE )
      *            ＤＢ更新キャンセル
                   MOVE    SPACE               TO  MCPDATA-REC
                   MOVE    "DBSELECT"          TO  MCP-FUNC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "abort"             TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
      *            一時データ登録なし
                   MOVE    1                   TO  FLG-CANSEL
               END-IF
           END-IF
      *
           .
       20031-HKNCOMBI-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険・公費・保険組合せ更新処理
      *****************************************************************
       200233-PTHKN-INSERT-SEC        SECTION.
      *
           MOVE    SPA-OPID            TO  HZN-SPA-OPID
           MOVE    WRK-OPID            TO  SPA-OPID
           MOVE    SPA-PTID            TO  SPA-GMN-PTID
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
           MOVE    "API"               TO  SPA-P02-GAMEN
           MOVE    "1"                 TO  SPA-RSIAPI-FLG
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
           MOVE    SPA-AREA            TO  SPA-COMMON
      *    更新サブへ
           CALL    "ORCGP029"      USING   MCPAREA
                                           SPAAREA
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
      *
           MOVE    HZN-SPA-OPID        TO  SPA-OPID
      *
      *    警告メッセージ
           IF     (SPA-ERRCD           =   SPACE  )
             AND  (SPA-KEI-ERRCD   NOT =   SPACE  )
               MOVE    SPA-KEI-ERRCD       TO  SPA-ERRCD
               PERFORM 8901-PTERRCD-SET-SEC
               PERFORM 290-WARMSG-HEN-SEC
           END-IF
           .
       200233-PTHKN-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険・公費ＩＤ登録処理
      *****************************************************************
       20042-PTNUM-UPDATE-SEC               SECTION.
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
           IF      FLG-PTNUM           =   ZERO
               IF      SPA-PTNUM       NOT =   PTNUM-PTNUM
                   MOVE    "10"                TO  WRK-ERRCD
                   GO      TO  20042-PTNUM-UPDATE-EXT
               END-IF
               MOVE    SPA-PTNUM-HKNID         TO  PTNUM-HKNID
               MOVE    SPA-PTNUM-KOHID         TO  PTNUM-KOHID
               MOVE    SPA-PTNUM-AUTOCOMBINUM  TO  PTNUM-AUTOCOMBINUM
               MOVE    SPA-PTNUM-MANUCOMBINUM  TO  PTNUM-MANUCOMBINUM
      *
               MOVE    SMCNDATE-YMD        TO  PTNUM-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTNUM-UPHMS
      *
               MOVE    WRK-OPID            TO  PTNUM-OPID
      *
               MOVE    PTNUM-REC           TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "API PTNUM UPD ERR:" MCP-RC
                          ",KEY:" PTNUM-KEY   "."
                   MOVE    "51"            TO  WRK-ERRCD
               END-IF
           ELSE
               MOVE    "10"                TO  WRK-ERRCD
           END-IF
           .
       20042-PTNUM-UPDATE-EXT.
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
      *    労災保険情報編集
           IF      WRK-ERRCD           =   SPACE
               MOVE    ZERO            TO  FLG-HENSYU
               PERFORM 20013-RSIINF-HENSYU-SEC
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
      *    ワーニングメッセージ編集処理
      ****************************************************************
       290-WARMSG-HEN-SEC           SECTION.
      *
           IF      WRK-ERRMSG          =   SPACE
               PERFORM 890-ERRCD-MSG-SEC
           END-IF
           ADD     1                   TO  IDX-WAR
           IF      IDX-WAR             <=  50
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "AP"            TO  PTMODRES-WARNING
                                                       (IDX-WAR)(1:2)
                   MOVE    WRK-ERRCD       TO  PTMODRES-WARNING
                                                       (IDX-WAR)(3:2)
               ELSE
                   MOVE    SPA-ERRCD       TO  PTMODRES-WARNING
                                                           (IDX-WAR)
               END-IF
               IF      WRK-ERR-NUM         =   SPACE
                   MOVE    WRK-ERRMSG          TO  PTMODRES-WARNING-MSG
                                                           (IDX-WAR)
               ELSE
               IF      WRK-ERR-NUMMEI2     =   SPACE
                   STRING  WRK-ERR-NUM         DELIMITED  BY  SIZE
                           " "                 DELIMITED  BY  SIZE
                           WRK-ERR-NUMMEI      DELIMITED  BY  SPACE
                           " "                 DELIMITED  BY  SIZE
                           WRK-ERRMSG          DELIMITED  BY  SIZE
                                       INTO    PTMODRES-WARNING-MSG
                                                           (IDX-WAR)
                   END-STRING
               ELSE
                   STRING  WRK-ERR-NUM         DELIMITED  BY  SIZE
                           " "                 DELIMITED  BY  SIZE
                           WRK-ERR-NUMMEI      DELIMITED  BY  SPACE
                           "("                 DELIMITED  BY  SIZE
                           WRK-ERR-NUMMEI2     DELIMITED  BY  SPACE
                           ")"                 DELIMITED  BY  SIZE
                           WRK-ERRMSG          DELIMITED  BY  SIZE
                                       INTO    PTMODRES-WARNING-MSG
                                                           (IDX-WAR)
                   END-STRING
               END-IF
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-ERRMSG
           MOVE    SPACE               TO  SPA-ERRCD
           .
       290-WARMSG-HEN-EXT.
           EXIT.
      ****************************************************************
      *    エラーメッセージ編集処理
      ****************************************************************
       290-ERROR-HEN-SEC           SECTION.
      *
      *    IF      WRK-ERRCD           =   SPACE
      *        MOVE    "P1"                TO  WRK-ERRCD
      *    END-IF
      *    IF      WRK-ERRMSG          =   SPACE
      *        PERFORM 890-ERRCD-MSG-SEC
      *    END-IF
      *
           IF      WRK-ERRMSG          =   SPACE
               IF      SPA-ERRCD(1:1)      =   "A"
                   PERFORM 8901-PTERRCD-SETAPI-SEC
                   MOVE    SPACE               TO  WRK-ERRCD
               ELSE
                   PERFORM 8901-PTERRCD-SET-SEC
               END-IF
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
               IF      WRK-ERR-NUM         =   SPACE
                   MOVE    WRK-ERRMSG          TO  PTMODRES-ERROR-MSG
                                                           (IDX-ERR)
               ELSE
               IF      WRK-ERR-NUMMEI2     =   SPACE
                   STRING  WRK-ERR-NUM         DELIMITED  BY  SPACE
                           " "                 DELIMITED  BY  SIZE
                           WRK-ERR-NUMMEI      DELIMITED  BY  SPACE
                           " "                 DELIMITED  BY  SIZE
                           WRK-ERRMSG          DELIMITED  BY  SIZE
                                       INTO    PTMODRES-ERROR-MSG
                                                           (IDX-ERR)
                   END-STRING
               ELSE
                   STRING  WRK-ERR-NUM         DELIMITED  BY  SPACE
                           " "                 DELIMITED  BY  SIZE
                           WRK-ERR-NUMMEI      DELIMITED  BY  SPACE
                           "("                 DELIMITED  BY  SIZE
                           WRK-ERR-NUMMEI2     DELIMITED  BY  SPACE
                           ")"                 DELIMITED  BY  SIZE
                           WRK-ERRMSG          DELIMITED  BY  SIZE
                                       INTO    PTMODRES-ERROR-MSG
                                                           (IDX-ERR)
                   END-STRING
               END-IF
               END-IF
           END-IF
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-ERRMSG
           MOVE    SPACE               TO  SPA-ERRCD
           .
       290-ERROR-HEN-EXT.
           EXIT.
      *****************************************************************
      *    確認メッセージ編集処理
      *****************************************************************
       292-SELECT-HEN-SEC            SECTION.
      *
           MOVE    SPA-PIDCD           TO  PTMODRES-SELECT-CODE
           PERFORM 8902-PTPIDCD-SET-SEC
           MOVE    WRK-ERRMSG          TO  PTMODRES-SELECT-MSG
           MOVE    SPACE               TO  WRK-ERRMSG
           .
       292-SELECT-HEN-EXT.
           EXIT.
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
               MOVE    "71"            TO  WRK-ERRCD
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
               MOVE    WRK-MAE-INPUT       TO  WRK-ATO-INPUT
               GO      TO      890-HALF-ZEN-EXT
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
           WHEN    "09"
               STRING  "処理区分がありません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "10"
               MOVE    "患者番号に該当する患者が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "基準日の暦日エラー"
                                               TO  WRK-ERRMSG
           WHEN    "12"
               MOVE    "基準日は平成２７年４月１日からです。"
                                               TO  WRK-ERRMSG
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
           WHEN    "H1"
           WHEN    "K2"
               PERFORM 8901-PTERRCD-SET-SEC
           WHEN    "P2"
               PERFORM 8902-PTPIDCD-SET-SEC
      *更新
           WHEN    "50"
               MOVE    "保険・公費にエラーがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "51"
               MOVE    "患者番号更新エラー"
                                               TO  WRK-ERRMSG
           WHEN    "52"
               MOVE    "患者番号と患者ＩＤが一致しません。"
                                               TO  WRK-ERRMSG
      *
           WHEN    "61"
               STRING  "保険公費情報の送信から行って下さい"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *
           WHEN    "80"
               STRING  "一時データ出力エラーです。"
                       "強制終了して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "81"
               STRING  "解除対象がありません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *警告
           WHEN    "K3"
               STRING  "保険の種類を変更しました。"
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
      *    患者基本情報エラーメッセージ
      *****************************************************************
       8901-PTERRCD-SETAPI-SEC              SECTION.
      *
           EVALUATE    SPA-ERRCD
           WHEN    "AP19"
               MOVE    "保険処理区分が違います"
                                               TO  WRK-ERRMSG
           WHEN    "AP20"
               MOVE    "労災保険区分が違います"
                                               TO  WRK-ERRMSG
           WHEN    "AP21"
               MOVE    "保険ＩＤエラー"
                                               TO  WRK-ERRMSG
           WHEN    "AP22"
               MOVE    "公費ＩＤエラー"
                                               TO  WRK-ERRMSG
           WHEN    "AP24"
               MOVE    "新規に保険ＩＤ・公費ＩＤは存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "AP23"
               MOVE    "保険ＩＤ・公費ＩＤが存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "AP25"
               MOVE    "保険区分の変更はできません。"
                                               TO  WRK-ERRMSG
           WHEN    "AP26"
               MOVE    "事業所名全角エラー"
                                               TO  WRK-ERRMSG
           WHEN    "AP27"
               MOVE    "傷病年月日暦日エラー"
                                               TO  WRK-ERRMSG
           WHEN    "AP28"
               MOVE    "療養開始日暦日エラー"
                                               TO  WRK-ERRMSG
           WHEN    "AP29"
               MOVE    "療養終了日暦日エラー"
                                               TO  WRK-ERRMSG
           WHEN    "AP30"
               MOVE    "労災レセ回数記載基準年月暦日エラー"
                                               TO  WRK-ERRMSG
           WHEN    "AP31"
               MOVE    "労災レセ回数記載回数数値エラー"
                                               TO  WRK-ERRMSG
           WHEN    "AP32"
               MOVE    "保険番号マスタに登録がありません。"
                                               TO  WRK-ERRMSG
           WHEN    "AP33"
               MOVE    "保険番号と保険区分が一致しません。"
                                               TO  WRK-ERRMSG
           WHEN    "AP34"
               MOVE    "自賠責保険会社名にエラーがあります"
                                               TO  WRK-ERRMSG
           WHEN    "AP35"
               MOVE    "傷病の部位にエラーがあります"
                                               TO  WRK-ERRMSG
           WHEN    "AP36"
               MOVE    "郡市区名にエラーがあります"
                                               TO  WRK-ERRMSG
      *
           WHEN    "AP73"
               STRING  "保険組合せが２００件以上存在します。"
                       "３年前に終了となった保険組合せは返却しません"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "AP74"
               STRING  "保険組合せが２００件以上存在します。"
                       "２年前に終了となった保険組合せは返却しません"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "AP75"
               STRING  "保険組合せが２００件以上存在します。"
                       "１年前に終了となった保険組合せは返却しません"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "AP76"
               STRING  "患者氏名・性別・生年月日が送信内容と違います。"
                       "患者を確認して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
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
      *    患者登録確認メッセージ編集処理
      *****************************************************************
       8902-PTPIDCD-SET-SEC              SECTION.
      *
      *
           EVALUATE    SPA-PIDCD
               WHEN    "0104"
      **           STRING    SPA-PIDMSG    DELIMITED  BY  SPACE
      *                      "を老人負担割合へ追加変更します。"
      *                      "よろしいですか？"
      *                                    DELIMITED  BY  SIZE
      *                                    INTO    WRK-PIDMSG
      ***          END-STRING
                   STRING    SPA-PIDMSG    DELIMITED  BY  SPACE
                             "　開始日が前期高齢者の適応開始日以前"
                             "です。"
                                           DELIMITED  BY  SIZE
                                           INTO    WRK-ERRMSG
                   END-STRING
               WHEN    "0105"
      ****         STRING     "保険の期間が重複します。前の"
      *                                    DELIMITED  BY  SIZE
      *                       SPA-PIDMSG   DELIMITED  BY  SPACE
      *                       "を終了させます。よろしいですか？"
      *                                    DELIMITED  BY  SIZE
      *                                INTO    WRK-PIDMSG
      ****         END-STRING
                   STRING     SPA-PIDMSG   DELIMITED  BY  SPACE
                              "と保険の期間が重複します。"
                                           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
               WHEN    "0106"
      **********   STRING  "公費の期間が重複します。"
      *                    "前の公費を終了させます。よろしいですか？"
      *                                        DELIMITED  BY  SIZE
      *                                        INTO    WRK-ERRMSG
      ************ END-STRING
                   STRING  "公費の期間が重複します。"
                           "前の公費を終了して下さい。"
                                               DELIMITED  BY  SPACE
                                       INTO    WRK-ERRMSG
                   END-STRING
               WHEN    "0107"
      ****         MOVE    SPA-PIDMSG      TO  WRK-PIDMSG
                   STRING  "保険番号マスタとの期間が違います。"
                           "終了日を変更して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   IF      WRK-ERR-YMD     NOT =   SPACE
                       MOVE    SPACE               TO  WRK-ERRMSG
                       IF      WRK-ERR-CNT         =   "1"
                           STRING  "保険番号マスタとの期間が違います。"
                                   WRK-ERR-YMD
                                   "で切り替えて下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                           END-STRING
                       ELSE
                           STRING  "保険番号マスタとの期間が違います。"
                                   "終了日を"
                                   WRK-ERR-YMD
                                   "に変更して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                           END-STRING
                       END-IF
                   END-IF
      *        Ｈ１５．４改正から
               WHEN    "0201"
      *            STRING  "保険が終了しています。終了日を変更します。"
      *                    "よろしいですか？"
      *                                        DELIMITED  BY  SIZE
      *                                INTO    WRK-ERRMSG
      *            END-STRING
                   STRING  "保険が終了しています。"
                           "終了日を変更して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   IF      WRK-ERR-YMD     NOT =   SPACE
                       MOVE    SPACE               TO  WRK-ERRMSG
                       STRING  "保険が終了しています。"
                               "終了日を"
                               WRK-ERR-YMD
                               "に変更して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                       END-STRING
                   END-IF
               WHEN    "0202"
               WHEN    "0204"
      ***          MOVE    "保険を切り替えます。よろしいですか？"
      ***                              TO  WRK-PIDMSG
                   STRING  "保険番号マスタとの期間が違います。"
                           "終了日を変更して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   IF      WRK-ERR-YMD     NOT =   SPACE
                       MOVE    SPACE               TO  WRK-ERRMSG
                       IF      WRK-ERR-CNT         =   "1"
                           STRING  "保険番号マスタとの期間が違います。"
                                   WRK-ERR-YMD
                                   "で切り替えて下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                           END-STRING
                       ELSE
                           STRING  "保険番号マスタとの期間が違います。"
                                   "終了日を"
                                   WRK-ERR-YMD
                                   "に変更して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                           END-STRING
                       END-IF
                   END-IF
               WHEN    "0203"
                   STRING  "継続は平成１５年３月末で終了しています。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                   END-STRING
      ****     WHEN    "0204"
      *            STRING  "任継を切り替えます。"
      *                    "切り替え後の終了日を確認して下さい。"
      *                                        INTO    WRK-ERRMSG
      ***          END-STRING
               WHEN    "0110"
      **           STRING  SPA-PIDMSG    DELIMITED  BY  SPACE
      *                    "の前期高齢者２割を平成１８年１０月から"
      *                    "３割に変更します。"
      *                    "よろしいですか？"
      *                                DELIMITED  BY  SIZE
      *                                INTO  WRK-PIDMSG
      **           END-STRING
                   STRING  SPA-PIDMSG    DELIMITED  BY  SPACE
                           "前期高齢者２割を平成１８年１０月から"
                           "３割に変更して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
                   END-STRING
               WHEN    "0111"
     **            STRING  "老人公費の２割を平成１８年１０月から"
      *                    "３割に変更します。"
      *                    "よろしいですか？"
      *                                DELIMITED  BY  SIZE
      *                                INTO  WRK-PIDMSG
      *            END-STRING
                   STRING  "老人公費の２割は平成１８年１０月から"
                           "３割です。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
                   END-STRING
      *
               WHEN    "K910"
      *!!          STRING  "警告！"
                   STRING  "保険組合せ更新で期間外の診療が"
                           "発生します。"
                           "更新内容を確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
                   END-STRING
               WHEN    "K911"
      ****         STRING  "警告！"
                   STRING  "保険組合せ更新で期間外の病名が"
                           "発生します。"
                           "更新内容を確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
                   END-STRING
               WHEN    "K912"
      ****         STRING  "警告！"
                   STRING  "保険組合せ更新で期間外の診療と病名が"
                           "発生します。"
                           "更新内容を確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-PIDCD
                                       TO  WRK-ERRMSG
           END-EVALUATE
            .
       8902-PTPIDCD-SET-EXT.
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
      *    保険番号マスタ取得
      *****************************************************************
       9002-HKNNUM-KEYREAD-SEC              SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    SPA-SYSYMD          TO  HKN-TEKSTYMD
           MOVE    SPA-SYSYMD          TO  HKN-TEKEDYMD
           MOVE    ZERO                TO  HKN-PAYKBN
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 980-HKNNUM-READ-SEC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       9002-HKNNUM-KEYREAD-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    保険番号マスタ次検索処理
      *****************************************************************
       980-HKNNUM-READ-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNNUM-REC
               MOVE    ZERO                TO  FLG-HKNNUM
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
       980-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    労働基準監督署マスタ読み込み
      *****************************************************************
       900-LABOR-SIO-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  LABOR-SIO-REC
               MOVE    ZERO            TO  FLG-LABOR-SIO
           ELSE
               INITIALIZE                  LABOR-SIO-REC
               MOVE    1               TO  FLG-LABOR-SIO
           END-IF
      *
           .
       900-LABOR-SIO-READ-EXT.
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
