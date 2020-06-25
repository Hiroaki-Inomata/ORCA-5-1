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
       PROGRAM-ID.         ORAPI012S1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 患者基本情報設定（登録・削除）(xml2)
      *  管理者            :
      *  作成日付    作業者        記述
      *  12/07/06    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.07.00     NACL-多々納  13/10/21  PARA削除追加
      * 04.07.00     NACL-多々納  13/11/01  API保険・公費追加対応
      *                                     保険組合せ番号追加他
      * 04.07.00     NACL-多々納  15/01/XX  性別・生年月日変更対応
      * 04.08.00     NACL-多々納  18/03/XX  妊婦設定対応
      * 05.00.00     NACL-多々納  19/06/XX  減免事由など追加対応
      * 05.00.00     NACL-多々納  20/04/XX  被保険者証枝番追加
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
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-PTBYOMEI            PIC 9(01).
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-HKNJAINF            PIC 9(01).
           03  FLG-KYUSEIRRK           PIC 9(01).
           03  FLG-PTCONF              PIC 9(01).
      *
           03  FLG-ERR                 PIC 9(01).
           03  FLG-HKNEND              PIC 9(01).
           03  FLG-KYUSEI              PIC 9(01).
           03  FLG-CLAIM-MULTIHOST     PIC 9(01).
           03  FLG-CLAIM               PIC 9(01).
           03  FLG-SOUSIN              PIC 9(01).
           03  FLG-SYORI               PIC 9(01).
           03  FLG-PTINF-JYU           PIC 9(01).
      *    ユーザプログラム起動
           03  FLG-GYOUMU-ARI          PIC 9(01).
           03  FLG-USERPG              PIC 9(01).
      *
           03  FLG-DATA                PIC 9(01).
      *R01.6
           03  FLG-CHK                 PIC 9(01).
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
      *R01.6
           03  IDS                     PIC 9(04).
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
      *    エラーコード
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-ERRMSG-2            PIC X(100).
           03  WRK-KEIERRCD            PIC X(04).
      *    ワーニングコード
           03  WRK-KEICD-G.
               05  WRK-KEICD           PIC X(02)   OCCURS   5.
           03  WRK-OKMSG               PIC X(20).
      *    エラー公費種類
           03  WRK-ERR-KOHNUM          PIC X(03).
           03  WRK-ERR-KOHNUMMEI       PIC X(20).
      *
      *    補助区分
           03  WRK-PTHKN-HOJOKBN       PIC X(01).
      *    ユーザプログラム
           03  WRK-PTID                PIC X(10).
           03  WRK-RENNUM              PIC 9(02).
           03  WRK-GMN-SELNUM          PIC 9(02).
           03  WRK-SYORI-FLG           PIC 9(01).
      *    ＣＬＡＩＭ受付時間
           03  WRK-UKETIME             PIC X(06).
      *
           03  WRK-MCP-PATHNAME        PIC X(62).
           03  WRK-MCP-WINDOW          PIC X(62).
      *
           03  WRK-OPID                PIC X(16).
           03  HZN-SPA-OPID            PIC X(16).
      *H30.4
      *    患者個別設定　設定ＫＥＹ
           03  WRK-CONF-CKEY           PIC  X(25).
      *                  設定データ
           03  WRK-CONF-CDATA          PIC  X(10).
      *
      *R01.6
      *    減免
           03  WRK-DISCOUNTKBN-NAME    PIC X(20).
           03  WRK-DISCOUNT-NAME       PIC X(20).
           03  WRK-IC-CODE-NAME        PIC X(20).
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
           03  WRK-RJN-RATE            PIC X(01).
      *
      *    ＣＬＡＩＭファイル名
       01  CLAIM-FILE.
           03  FILLER              PIC X(30)   VALUE
               "scripts/claim/HL01.sh".
      *
      *v4.8
      *    画面非表示
           COPY    "ENUM-VALUE".
      *
      * 保険・公費最大件数
       01  CONS-AREA.
           03  TBL-MAX-KOH             PIC 9(02)  VALUE    60.
           03  TBL-MAX-HKN             PIC 9(02)  VALUE    40.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
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
      *    チェックデジット
           COPY    "CPORCCHKDGT.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *    全角チェックパラメタ（拡張漢字）
           COPY    "CPORCSKANACHK2.INC".
      *
           COPY    "CPORCXKANACONV.INC".
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
           COPY    "CPORCSUSERCHK.INC".
      *
      *    API XML読込用定義体
           COPY    "CPPATIENTMODV2REQ.INC".
      *    API XML出力用定義体
           COPY    "CPPATIENTMODV2RES.INC".
      *
      *    保険組合せ決定関数サブ
           COPY    "CPORCSAPIHKN.INC".
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム基本
       01  SYSBASE-REC.
           COPY    "CPSYSBASE.INC".
      *
      *    システムユーザー
       01  SYSUSER-REC.
           COPY    "CPSYSUSER.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    （診療科情報）
           COPY    "CPSK1005.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *    診療内容情報
           COPY  "CPSK1012.INC".
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
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *    旧姓履歴   *
       01  KYUSEIRRK-REC.
           COPY  "CPKYUSEIRRK.INC".
      *
      *    保険者号マスタ
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
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
      *H30.4
      *    患者個別設定テーブル
       01  PTCONF-REC.
           COPY    "CPPTCONF.INC".
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *   （ＣＬＡＩＭ接続）
           COPY    "CPSK9000.INC".
           COPY    "CPSK9001.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
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
               MOVE   404                  TO  APIMOD-HTTPSTATUS
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
      *
           INITIALIZE                      SPA-P02SCRAREA
           INITIALIZE                      SPA-P02KYOUTU
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *    ＣＬＡＩＭ受付時間
           MOVE    SMCNDATE-HMS        TO  WRK-UKETIME
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
           INITIALIZE                      WRK-KEICD-G
           MOVE    ZERO                TO  IDX-W
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    APIMOD-CLASS
           WHEN    "01"
      *        新規登録
               MOVE    1                   TO  FLG-SYORI
           WHEN    "02"
      *        更新
               MOVE    2                   TO  FLG-SYORI
           WHEN    "03"
      *        削除
               MOVE    3                   TO  FLG-SYORI
      *H25.10
           WHEN    "04"
      *        保険・公費追加
               MOVE    4                   TO  FLG-SYORI
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
           END-EVALUATE
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               PERFORM 890-ERRCD-MSG-SEC
      *TEST!!
      *        DISPLAY "必須項目チェックエラー "    WRK-ERRCD
      *                " " WRK-ERRMSG
      *TEST!!
               GO      TO      200-MAIN-EXT
           END-IF
      *
           EVALUATE    APIMOD-CLASS
           WHEN    "01"
      *        登録処理
               PERFORM 2002-PTINF-ADD-SEC
           WHEN    "02"
      *        更新処理（基本情報）
               PERFORM 2003-PTINF-UPDATE-SEC
           WHEN    "03"
      *        削除処理
               PERFORM 2003-PTINF-DELETE-SEC
      *H25.10
           WHEN    "04"
      *        保険・公費追加
               PERFORM 2004-PTHKNKOH-INS-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-RGTXMLREAD-SEC   SECTION.
      *
           IF      APIMOD-BODY        NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-PATIENTMODREQ
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_patientmodv2req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APIMOD-BODY         TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "patientmodreq"     TO  APIREQ-RECORDNAME
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
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_patientmodv2req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APIMOD-BODY         TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "patientmodreq"     TO  APIREQ-RECORDNAME
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
                       MOVE    "登録終了"
                                           TO  WRK-OKMSG
                   WHEN    2
                       MOVE    "更新終了"
                                           TO  WRK-OKMSG
                   WHEN    3
                       MOVE    "削除終了"
                                           TO  WRK-OKMSG
                   WHEN    4
                       MOVE    "追加終了"
                                           TO  WRK-OKMSG
               END-EVALUATE
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               MOVE    WRK-OKMSG           TO  PTMODRES-API-RESULT-MSG
           ELSE
      *        エラーメッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD           TO  PTMODRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  PTMODRES-API-RESULT-MSG
           END-IF
      *
           MOVE    ZERO                TO  IDX
      *    ワーニングメッセージ編集
           IF      WRK-ERRCD           =   SPACE
               IF      WRK-KEIERRCD    NOT =   SPACE
                   MOVE    WRK-KEIERRCD    TO  SPA-ERRCD
                   MOVE    "K2"            TO  WRK-ERRCD
                   PERFORM 890-ERRCD-MSG-SEC
                   MOVE    WRK-ERRCD       TO  PTMODRES-API-RESULT
                   ADD     1               TO  IDX
                   MOVE    WRK-ERRMSG      TO  PTMODRES-API-WARNING-MSG
                                                             (IDX)
               END-IF
               PERFORM VARYING     IDX-W   FROM    1   BY  1
                       UNTIL      (IDX-W   >   5   )
                               OR (WRK-KEICD (IDX-W)   =   SPACE)
                               OR (IDX     >=  5   )
                   MOVE    WRK-KEICD (IDX-W)   TO  WRK-ERRCD
                   PERFORM 890-ERRCD-MSG-SEC
                   ADD     1                   TO  IDX
                   IF      IDX                 =   1
                       MOVE    WRK-ERRCD       TO  PTMODRES-API-RESULT
                   END-IF
                   MOVE    WRK-ERRMSG      TO  PTMODRES-API-WARNING-MSG
                                                             (IDX)
               END-PERFORM
           END-IF
      *
           IF      APIMOD-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_patientmodv2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  PTMODRES-MODE
           MOVE    "patientmodres"     TO  PTMODRES-RECORDNAME
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
           MOVE    "xml_patientmodv2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  PTMODRES-MODE
           MOVE    "patientmodres"     TO  PTMODRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-PATIENTMODRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               MOVE    PTMODRES-OBJECT     TO  APIMOD-BODY
               MOVE    "patientmod/xml"    TO  APIMOD-CONTENT-TYPE
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
      *
      *****************************************************************
      *    患者基本情報設定内容チェック
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
      *    編集処理
           PERFORM 20010-INPUT-HEN-SEC
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PTNUM-CHK-SEC
           END-IF
      *    氏名チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-NAME-CHK-SEC
           END-IF
      *    カナ氏名チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-KANANAME-CHK-SEC
           END-IF
      *    性別チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-SEX-CHK-SEC
           END-IF
      *    生年月日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-BIRTHDAY-CHK-SEC
           END-IF
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
           MOVE    PTMODREQ-PATIENTID      TO  WRK-PTNUM
      *    患者情報
           MOVE    PTMODREQ-NAME           TO  PTMODRES-NAME
           MOVE    PTMODREQ-KANANAME       TO  PTMODRES-KANANAME
           MOVE    PTMODREQ-BIRTHDAY       TO  PTMODRES-BIRTHDAY
           MOVE    PTMODREQ-SEX            TO  PTMODRES-SEX
           MOVE    PTMODREQ-SETAI-NAME     TO  PTMODRES-SETAI-NAME
           MOVE    PTMODREQ-ZOKUGARA       TO  PTMODRES-ZOKUGARA
           MOVE    PTMODREQ-JOB            TO  PTMODRES-JOB
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
      *    妊婦設定
           MOVE    PTMODREQ-NINPUKBN       TO  PTMODRES-NINPUKBN
      *R01.6
      *    減免事由
           MOVE    PTMODREQ-DISCOUNTKBN    TO  PTMODRES-DISCOUNTKBN
      *    割引率
           MOVE    PTMODREQ-DISCOUNT       TO  PTMODRES-DISCOUNT
      *    支払区分（入金方法）
           MOVE    PTMODREQ-IC-CODE        TO  PTMODRES-IC-CODE
      *
      *    保険情報（登録のみ）
           IF      FLG-SYORI           =   1
                                       OR  4
               PERFORM 200101-INHKN-HEN-SEC
           END-IF
      *
      *    必須入力チェック
      *    患者番号
           IF      WRK-PTNUM           =   SPACE
               MOVE    "01"                TO  WRK-ERRCD
           END-IF
      *    患者氏名
           IF      PTMODREQ-NAME       =   SPACE
               IF     (WRK-ERRCD           =   SPACE  )
                   MOVE    "02"                TO  WRK-ERRCD
               END-IF
           END-IF
      *    カナ氏名
           IF      PTMODREQ-KANANAME    =   SPACE
               IF     (WRK-ERRCD           =   SPACE  )
                   MOVE    "03"                TO  WRK-ERRCD
               END-IF
           END-IF
      *    生年月日
           IF      WRK-BIRTHDAY-YMD    =   SPACE
               IF     (WRK-ERRCD           =   SPACE  )
                   MOVE    "04"                TO  WRK-ERRCD
               END-IF
           END-IF
      *    性別
           IF      PTMODREQ-SEX        =   SPACE
               IF     (WRK-ERRCD           =   SPACE  )
                   MOVE    "05"                TO  WRK-ERRCD
               END-IF
           END-IF
           .
       20010-INPUT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力保険情報編集処理
      *****************************************************************
       200101-INHKN-HEN-SEC    SECTION.
      *
           MOVE    1                   TO  IDX
           MOVE    PTMODREQ-MAIN-INSURANCE-CLASS
                                       TO  PTMODRES-MAIN-INSURANCE-CLASS
                                                               (IDX)
           MOVE    PTMODREQ-MAIN-INSURANCE-NUMBER
                                   TO  PTMODRES-MAIN-INSURANCE-NUMBER
                                                               (IDX)
           MOVE    PTMODREQ-MAIN-INSURANCE-NAME
                                       TO  PTMODRES-MAIN-INSURANCE-NAME
                                                               (IDX)
           MOVE    PTMODREQ-MAIN-MARK
                                       TO  PTMODRES-MAIN-MARK
                                                               (IDX)
           MOVE    PTMODREQ-MAIN-NUMBER
                                       TO  PTMODRES-MAIN-NUMBER
                                                               (IDX)
      *    枝番
           MOVE    PTMODREQ-MAIN-EDABAN
                                       TO  PTMODRES-MAIN-EDABAN
                                                               (IDX)
           MOVE    PTMODREQ-MAIN-CONTINUATION
                                       TO  PTMODRES-MAIN-CONTINUATION
                                                               (IDX)
           MOVE    PTMODREQ-MAIN-ASSISTANCE
                                       TO  PTMODRES-MAIN-ASSISTANCE
                                                               (IDX)
           MOVE    PTMODREQ-MAIN-FAMILY
                                       TO  PTMODRES-MAIN-FAMILY
                                                               (IDX)
           MOVE    PTMODREQ-MAIN-POLICYHOLDER
                                       TO  PTMODRES-MAIN-POLICYHOLDER
                                                               (IDX)
           MOVE    PTMODREQ-MAIN-START-DATE
                                       TO  PTMODRES-MAIN-START-DATE
                                                               (IDX)
           MOVE    PTMODREQ-MAIN-GET-DATE
                                       TO  PTMODRES-MAIN-GET-DATE
                                                               (IDX)
      *    公費情報
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   4
      *             公費の種類
               MOVE    PTMODREQ-INSURANCE-CLASS (IDY)
                                       TO  PTMODRES-INSURANCE-CLASS
                                                           (IDX IDY)
               MOVE    PTMODREQ-INSURANCE-NAME  (IDY)
                                       TO  PTMODRES-INSURANCE-NAME
                                                           (IDX IDY)
               MOVE    PTMODREQ-PROVIDER-NUMBER  (IDY)
                                       TO  PTMODRES-PROVIDER-NUMBER
                                                           (IDX IDY)
               MOVE    PTMODREQ-RECIPIENT-NUMBER  (IDY)
                                       TO  PTMODRES-RECIPIENT-NUMBER
                                                           (IDX IDY)
               MOVE    PTMODREQ-INSURANCE-START-DATE (IDY)
                                       TO  PTMODRES-INSURANCE-START-DATE
                                                           (IDX IDY)
               MOVE    PTMODREQ-INSURANCE-END-DATE (IDY)
                                       TO  PTMODRES-INSURANCE-END-DATE
                                                           (IDX IDY)
           END-PERFORM
           .
       200101-INHKN-HEN-EXT.
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
      *    患者氏名チェック処理
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
               ELSE
                   MOVE    "P1"                TO  WRK-ERRCD
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
               ELSE
                   MOVE    "P1"                TO  WRK-ERRCD
               END-IF
           END-IF
           .
       200111-PTNUM-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者登録処理
      *****************************************************************
       2002-PTINF-ADD-SEC        SECTION.
      *
           MOVE    SPA-PTID            TO  SPA-GMN-PTID
      *    患者情報内容編集
           PERFORM 20021-INSERT-CHK-SEC
      *
      *    患者登録処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20023-PTINFINS-SYORI-SEC
           END-IF
      *
      *    パラメタ削除処理（保険情報）
           PERFORM 4904-PARA-DEL-SEC
      *
      *    保険組合せ編集処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20023-HKNCOMBI-HEN-SEC
           END-IF
      *    CLAIM送信チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 4905-CLAIM-CHK-SEC
           END-IF
      *
      *    ユーザ起動プログラム起動チェック
           MOVE    1                   TO  FLG-USERPG
           IF      WRK-ERRCD           =   SPACE
               PERFORM 4907-USERPG-SEC
           END-IF
           .
       2002-PTINF-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報内容編集・チェック処理
      *****************************************************************
       20021-INSERT-CHK-SEC        SECTION.
      *
      *    基本情報
           PERFORM 200211-PTINF-CHK-SEC
      *    保険情報チェック
           IF      WRK-ERRCD           =   SPACE
               MOVE    ZERO                TO  SPA-PTHKN-MAX
               MOVE    ZERO                TO  SPA-PTNUM-HKNID
               MOVE    ZERO                TO  IDX
               PERFORM 200212-PTHKNINF-CHK-SEC
           END-IF
      *    公費情報チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200213-PTHKOHNF-CHK-SEC
           END-IF
      *    保険組合せ作成チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200214-HKNCOMBI-SYORI-SEC
           END-IF
           .
       20021-INSERT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報処理
      *****************************************************************
       200211-PTINF-CHK-SEC        SECTION.
      *
      *    パラメタファイル、患者情報のセット
           MOVE    SPACE               TO  P02
           INITIALIZE                      P02
      *
           MOVE    SPA-GMN-NAME        TO  SPA-NAME
           MOVE    ZERO                TO  SPA-P02-UKEID
           MOVE    1                   TO  SPA-P02-SYORI
      *R01.6
      *    更新
           IF      FLG-SYORI           =   2
               MOVE    2                   TO  SPA-P02-SYORI
           END-IF
      *
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
      *
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
      *H30.4
      *    妊婦設定
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-NINPUKBN-CHK-SEC
           END-IF
      *R01.6
      *    減免事由
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-DISCOUNTKBN-CHK-SEC
           END-IF
      *    割引率
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-DISCOUNT-CHK-SEC
           END-IF
      *    支払区分（入金方法）
           IF     (WRK-ERRCD           =   SPACE ) 
           AND    (SPA-ERRCD           =   SPACE )
               PERFORM 200211-IC-CODE-CHK-SEC
           END-IF
      *
           IF     (WRK-ERRCD           =   SPACE ) 
               IF      (SPA-ERRCD      NOT =   SPACE )
                   MOVE    "P1"            TO  WRK-ERRCD
               END-IF
           END-IF
           .
       200211-PTINF-CHK-EXT.
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
      *
      *    対象なし（警告とする）
           IF     (SPA-GMN982-MAX      =   ZERO  )
             AND  (WRK-ERRCD           =   SPACE )
               MOVE    "0031"              TO  WRK-KEIERRCD
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
      *H29.10
                AND   (KANACHK-RC4         =   ZERO  )
      *        半角であること
               CONTINUE
           ELSE
               MOVE    "20"                TO  WRK-ERRCD
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
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-TEL1        TO  KANACHK-MAE-INPUT
               MOVE    15                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  WRK-MAE-INPUT
               IF     (KANACHK-RC          =   ZERO  )  AND
                      (KANACHK-SYUBETU     =   1     )
      *H29.10
                AND   (KANACHK-RC4         =   ZERO  )
                   CONTINUE
               ELSE
                   MOVE    "22"                TO  WRK-ERRCD
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
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-TEL2        TO  KANACHK-MAE-INPUT
               MOVE    15                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  WRK-MAE-INPUT
               IF     (KANACHK-RC          =   ZERO  )  AND
                      (KANACHK-SYUBETU     =   1     )
      *H29.10
                AND   (KANACHK-RC4         =   ZERO  )
                   CONTINUE
               ELSE
                   MOVE    "23"                TO  WRK-ERRCD
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
      *        半角空白を全角空白へ
               MOVE    SPA-GMN-ZOKUGARA    TO  WRK-MAE-INPUT
               MOVE    30                  TO  WRK-LEN
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-ZOKUGARA
               IF      FLG-ERR             =   1
                   MOVE    "0034"              TO  SPA-ERRCD
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
      *        半角空白を全角空白へ
               MOVE    SPA-GMN21-JOB       TO  WRK-MAE-INPUT
               MOVE    40                  TO  WRK-LEN
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN21-JOB
               IF      FLG-ERR             =   1
                   MOVE    "28"                TO  WRK-ERRCD
               END-IF
           END-IF
           .
       200211-JOB-CHK-EXT.
           EXIT.
      *****************************************************************
      *    携帯電話番号チェック処理
      *****************************************************************
       200211-KEITAITEL-CHK-SEC         SECTION.
      *
           IF      SPA-GMN21-KEITAI-TEL  NOT =   SPACE
      *        半角であること
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN21-KEITAI-TEL
                                           TO  KANACHK-MAE-INPUT
               MOVE    15                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  WRK-MAE-INPUT
               IF     (KANACHK-RC          =   ZERO  )  AND
                      (KANACHK-SYUBETU     =   1     )
      *H29.10
                AND   (KANACHK-RC4         =   ZERO  )
                   CONTINUE
               ELSE
                   MOVE    "25"                TO  WRK-ERRCD
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
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN21-FAX       TO  KANACHK-MAE-INPUT
               MOVE    15                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  WRK-MAE-INPUT
               IF     (KANACHK-RC          =   ZERO  )  AND
                      (KANACHK-SYUBETU     =   1     )
      *H29.10
                AND   (KANACHK-RC4         =   ZERO  )
                   CONTINUE
               ELSE
                   MOVE    "26"                TO  WRK-ERRCD
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
           IF      SPA-GMN21-EMAIL     NOT =   SPACE
      *        半角であること
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN21-EMAIL     TO  KANACHK-MAE-INPUT
               MOVE    50                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  WRK-MAE-INPUT
               IF     (KANACHK-RC          =   ZERO  )  AND
                      (KANACHK-SYUBETU     =   1     )
      *H29.10
                AND   (KANACHK-RC4         =   ZERO  )
                   CONTINUE
               ELSE
                   MOVE    "27"                TO  WRK-ERRCD
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
                   MOVE    "29"                TO  WRK-ERRCD
               END-IF
           END-IF
           IF     (SPA-GMN-TABOO-2     NOT =   SPACE ) AND
                  (WRK-ERRCD               =   SPACE )
               MOVE    SPA-GMN-TABOO-2     TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-TABOO-2
               IF      FLG-ERR             =   1
                   MOVE    "29"                TO  WRK-ERRCD
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
                   MOVE    "30"                TO  WRK-ERRCD
               END-IF
           END-IF
           IF     (SPA-GMN-ALLERGY-2     NOT =   SPACE ) AND
                  (WRK-ERRCD               =   SPACE )
               MOVE    SPA-GMN-ALLERGY-2   TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-ALLERGY-2
               IF      FLG-ERR             =   1
                   MOVE    "30"                TO  WRK-ERRCD
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
                   MOVE    "31"                TO  WRK-ERRCD
               END-IF
           END-IF
           IF     (SPA-GMN-KANSENSYO-2 NOT =   SPACE ) AND
                  (WRK-ERRCD               =   SPACE )
               MOVE    SPA-GMN-KANSENSYO-2 TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-KANSENSYO-2
               IF      FLG-ERR             =   1
                   MOVE    "31"                TO  WRK-ERRCD
               END-IF
           END-IF
      *    コメント
           IF     (SPA-GMN-COMMENT-1   NOT =   SPACE ) AND
                  (WRK-ERRCD               =   SPACE )
               MOVE    SPA-GMN-COMMENT-1   TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-COMMENT-1
               IF      FLG-ERR             =   1
                   MOVE    "35"                TO  WRK-ERRCD
               END-IF
           END-IF
           IF     (SPA-GMN-COMMENT-2   NOT =   SPACE ) AND
                  (WRK-ERRCD               =   SPACE )
               MOVE    SPA-GMN-COMMENT-2   TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
               PERFORM 880-HALF-ZENCNG-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-COMMENT-2
               IF      FLG-ERR             =   1
                   MOVE    "35"                TO  WRK-ERRCD
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
           IF      WRK-ERRCD           =   "20"
               MOVE    "21"            TO  WRK-ERRCD
           END-IF
      *    対象なし
           IF     (SPA-GMN982-MAX      =   ZERO  )
             AND  (WRK-ERRCD           =   SPACE )
               MOVE    "2106"              TO  WRK-KEIERRCD
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
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN21-OFFICE-TEL    TO  KANACHK-MAE-INPUT
               MOVE    15                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  WRK-MAE-INPUT
               IF     (KANACHK-RC          =   ZERO  )  AND
                      (KANACHK-SYUBETU     =   1     )
      *H29.10
                AND   (KANACHK-RC4         =   ZERO  )
                   CONTINUE
               ELSE
                   MOVE    "24"                TO  WRK-ERRCD
               END-IF
           END-IF
           .
       200211-OFFICETEL-CHK-EXT.
           EXIT.
      *
      *H30.4
      *****************************************************************
      *    妊婦設定チェック処理
      *****************************************************************
       200211-NINPUKBN-CHK-SEC        SECTION.
      *
      *    妊婦設定
           EVALUATE    PTMODREQ-NINPUKBN
               WHEN    "True"
      *            妊婦
                   MOVE    "1"             TO  SPA-GMN28-NINPUKBN
                   IF      SPA-GMN-SEX         =   "1"
      *                男性はエラー
                       MOVE    "34"            TO  WRK-ERRCD
                   END-IF
      *           １６歳未満は警告
                   IF     (SPA-NAI-NENREI-YY   <   16 )
                     AND  (WRK-ERRCD           =   SPACE )
                       ADD     1               TO  IDX-W
                       MOVE    "K5"            TO  WRK-KEICD (IDX-W)
                   END-IF
               WHEN    OTHER
      *            妊婦解除
                   MOVE    "0"             TO  SPA-GMN28-NINPUKBN
           END-EVALUATE
           .
       200211-NINPUKBN-CHK-EXT.
           EXIT.
      *R01.6
      *****************************************************************
      *    減免事由チェック処理
      *****************************************************************
       200211-DISCOUNTKBN-CHK-SEC        SECTION.
      *
      *    減免事由
           IF      PTMODREQ-DISCOUNTKBN    NOT =   SPACE
               MOVE    PTMODREQ-DISCOUNTKBN    TO  SPA-GMN-DISCOUNTKBN
           END-IF
           IF      SPA-GMN-DISCOUNTKBN =   SPACE
               MOVE    "00"            TO  SPA-GMN-DISCOUNTKBN
           END-IF
      *
           MOVE    SPA-GMN-DISCOUNTKBN TO  PTMODRES-DISCOUNTKBN
      *
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
               MOVE    "36"                TO  WRK-ERRCD
           END-IF
           .
       200211-DISCOUNTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    割引率チェック処理
      *****************************************************************
       200211-DISCOUNT-CHK-SEC        SECTION.
      *
      *    割引率
           IF      PTMODREQ-DISCOUNT   NOT =   SPACE
               MOVE    PTMODREQ-DISCOUNT   TO  SPA-GMN-DISCOUNT-CD
           END-IF
           IF      SPA-GMN-DISCOUNT-CD =   SPACE
               MOVE    SPACE           TO  SPA-GMN-DISCOUNT
               MOVE    "00"            TO  SPA-GMN-DISCOUNT-CD
           END-IF
           MOVE    SPA-GMN-DISCOUNT-CD TO  PTMODRES-DISCOUNT
      *
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
               MOVE    "37"                TO  WRK-ERRCD
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
      *    支払区分（入金方法）チェック処理
      *****************************************************************
       200211-IC-CODE-CHK-SEC        SECTION.
      *
           IF      PTMODREQ-IC-CODE   NOT =   SPACE
               MOVE    PTMODREQ-IC-CODE   TO  SPA-GMN-NYUKIN-HOHO
           END-IF
      *
           IF      SPA-GMN-NYUKIN-HOHO =   SPACE
      *        リストの１件目とする
               MOVE    SPA-GMN-NYUKIN-LST (1)
                                           TO  SPA-GMN-NYUKIN-HOHO-G
           END-IF
           MOVE    ZERO                TO  FLG-CHK
           MOVE    SPACE               TO  WRK-IC-CODE-NAME
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-NYUKIN-MAX)
                          OR  (FLG-CHK     =   1   )
               IF      SPA-GMN-NYUKIN-HOHO
                                           =   SPA-GMN-NYUKIN-HOHOL
                                                              (IDX)
                   MOVE    SPA-GMN-NYUKIN-MEIL(IDX)
                                           TO  WRK-IC-CODE-NAME
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-NYUKIN-HOHO TO  PTMODRES-IC-CODE
           MOVE    WRK-IC-CODE-NAME    TO  PTMODRES-IC-CODE-NAME
           IF      FLG-CHK             =   ZERO
               MOVE    "38"                TO  WRK-ERRCD
           END-IF
           .
       200211-IC-CODE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険情報更新処理
      *****************************************************************
       200212-PTHKNINF-CHK-SEC        SECTION.
      *
           IF      PTMODREQ-INSURANCE-INFO-G   =   SPACE
      *??      MOVE    ZERO                TO  SPA-PTHKN-MAX
      *        MOVE    ZERO                TO  SPA-PTNUM-HKNID
      *???     MOVE    ZERO                TO  IDX
               GO      TO    200212-PTHKNINF-CHK-EXT
           END-IF
           MOVE    1                   TO  FLG-DATA
      *H29.11
           IF      SPA-PTHKN-MAX       >=  TBL-MAX-HKN
               MOVE    "64"            TO  WRK-ERRCD
               GO      TO    200212-PTHKNINF-CHK-EXT
           END-IF
      *!!
      *
           ADD     1                   TO  SPA-PTHKN-MAX
           ADD     1                   TO  SPA-PTNUM-HKNID
           MOVE    SPA-PTHKN-MAX       TO  IDX
      *    保険情報
      *    保険ＩＤ
           MOVE    SPA-PTNUM-HKNID     TO  SPA-PTHKN-HKNID (IDX)
      *    新規登録
           MOVE    "1"                 TO  SPA-PTHKN-SINKIKBN (IDX)
      *    保険の種類
           MOVE    PTMODREQ-MAIN-INSURANCE-CLASS
                                       TO  SPA-PTHKN-HKNNUM (IDX)
      *    保険者番号
           MOVE    PTMODREQ-MAIN-INSURANCE-NUMBER
                                       TO  SPA-PTHKN-HKNJANUM (IDX)
      *    記号
           MOVE    PTMODREQ-MAIN-MARK  TO  WRK-MAE-INPUT
           MOVE    40                  TO  WRK-LEN
           PERFORM 880-HALF-ZENCNG-SEC
           MOVE    WRK-ATO-INPUT       TO  PTMODREQ-MAIN-MARK
           IF      FLG-ERR             =   1
               MOVE    "32"                TO  WRK-ERRCD
           END-IF
           MOVE    PTMODREQ-MAIN-MARK      TO  SPA-PTHKN-KIGO (IDX)
      *    番号
           MOVE    PTMODREQ-MAIN-NUMBER    TO  WRK-MAE-INPUT
           MOVE    40                      TO  WRK-LEN
           PERFORM 880-HALF-ZENCNG-SEC
           MOVE    WRK-ATO-INPUT           TO  PTMODREQ-MAIN-NUMBER
           IF     (FLG-ERR             =   1     )
             AND  (WRK-ERRCD           =   SPACE )
               MOVE    "33"                TO  WRK-ERRCD
           END-IF
           MOVE    PTMODREQ-MAIN-NUMBER    TO  SPA-PTHKN-NUM  (IDX)
      *R02.4
      *    枝番
           MOVE    PTMODREQ-MAIN-EDABAN    TO  SPA-PTHKN-EDABAN(IDX)
      *    継続区分
           MOVE    PTMODREQ-MAIN-CONTINUATION
                                           TO  SPA-PTHKN-CONTKBN (IDX)
      *    補助区分
           MOVE    PTMODREQ-MAIN-ASSISTANCE
                                           TO  SPA-PTHKN-HOJOKBN (IDX)
      *    本人家族区分
           MOVE    PTMODREQ-MAIN-FAMILY    TO  SPA-PTHKN-HONKZKKBN (IDX)
      *    被保険者名
           MOVE    PTMODREQ-MAIN-POLICYHOLDER
                                           TO  SPA-PTHKN-HIHKNJANAME
                                                                  (IDX)
      *    有効開始日
           MOVE    PTMODREQ-MAIN-START-DATE
                                           TO  WRK-HEN-DATE
           PERFORM 802-DAYHEN02-SEC
           IF      WRK-SYMD                =   SPACE
                                           OR  ZERO
               MOVE    SPA-SYSYMD          TO  SPA-PTHKN-TEKSTYMD (IDX)
           ELSE
               MOVE    WRK-SYMD            TO  SPA-PTHKN-TEKSTYMD (IDX)
           END-IF
      *    有効終了日
           MOVE    PTMODREQ-MAIN-END-DATE  TO  WRK-HEN-DATE
           PERFORM 802-DAYHEN02-SEC
           IF      WRK-SYMD                =   SPACE
                                           OR  ZERO
               MOVE    "99999999"          TO  SPA-PTHKN-TEKEDYMD (IDX)
           ELSE
               MOVE    WRK-SYMD            TO  SPA-PTHKN-TEKEDYMD (IDX)
           END-IF
      *    資格取得日
           MOVE    PTMODREQ-MAIN-GET-DATE  TO  WRK-HEN-DATE
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD            TO  SPA-PTHKN-SKKGETYMD (IDX)
      *    確認年月日
           MOVE    SPA-SYSYMD          TO  SPA-PTHKN-KAKUNINYMD (IDX)
      *
           MOVE    SPA-PTHKN-SKKGETYMD (IDX)
                                       TO  SPA-PTHKN-SKKGETYMDW(IDX)
           MOVE    SPA-PTHKN-TEKSTYMD (IDX)
                                       TO  SPA-PTHKN-TEKSTYMDW(IDX)
           MOVE    SPA-PTHKN-TEKEDYMD (IDX)
                                       TO  SPA-PTHKN-TEKEDYMDW(IDX)
           MOVE    SPA-PTHKN-KAKUNINYMD (IDX)
                                       TO  SPA-PTHKN-KAKUNINYMDW(IDX)
      *    労災保険はエラー
           IF      PTMODREQ-MAIN-INSURANCE-CLASS
                                       =   "971"
                                       OR  "973"
               MOVE    "39"                TO  WRK-ERRCD
           END-IF
      *
           IF      SPA-PTHKN-HKNJANUM (IDX)    NOT =   SPACE
      *        保険者番号から保険の種類と補助区分設定
               PERFORM 2002121-HKNJANUM-CHK-SEC
           END-IF
           .
       200212-PTHKNINF-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険者番号から保険の種類と補助区分設定処理
      *****************************************************************
       2002121-HKNJANUM-CHK-SEC     SECTION.
      *
           INITIALIZE                    HKNJAINF-REC
           MOVE    SPA-HOSPNUM           TO  HKNJA-HOSPNUM
           MOVE    SPA-PTHKN-HKNJANUM (IDX)
                                         TO  HKNJA-HKNJANUM
           MOVE    HKNJAINF-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknjainf"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"        TO  MCP-TABLE
               MOVE    "key"                 TO  MCP-PATHNAME
               PERFORM 900-HKNJAINF-READ-SEC
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    保険者番号あり
           IF      FLG-HKNJAINF        =   ZERO
               IF    ((HKNJA-HKNNUM            =   "060" ) AND
                      (SPA-PTHKN-HKNNUM (IDX)  =   "068" ))
                 OR  ((HKNJA-HKNNUM            =   "067" ) AND
                      (SPA-PTHKN-HKNNUM (IDX)  =   "069" ))
                 OR  ((HKNJA-HKNNUM            =   "039" ) AND
                      (SPA-PTHKN-HKNNUM (IDX)  =   "040" ))
      *            保険変更なし
                   CONTINUE
               ELSE
      *            保険種類変更
                   MOVE    HKNJA-HKNNUM    TO  SPA-PTHKN-HKNNUM (IDX)
               END-IF
           END-IF
      *    高齢者、国保の補助区分設定
           IF     (SPA-PTHKN-HKNNUM (IDX)  =   SPACE    )  OR
                  (SPA-PTHKN-HKNNUM (IDX)(1:1) =   "9"  )
               GO      TO      2002121-HKNJANUM-CHK-EXT
           END-IF
           IF     (SPA-PTHKN-TEKSTYMD(IDX) >=  SPA-BYMD-70 )
              OR  (SPA-PTHKN-HKNNUM (IDX)  =   "039"
                                           OR  "040"      )
      *        高齢者補助区分設定
               PERFORM 2002121-HOJOKBN-ROU-SEC
           END-IF
           .
       2002121-HKNJANUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    高齢者補助区分編集処理
      *****************************************************************
       2002121-HOJOKBN-ROU-SEC     SECTION.
      *
           MOVE    PTMODREQ-RATE-CLASS TO  WRK-RJN-RATE
      *
           MOVE    SPACE               TO  WRK-PTHKN-HOJOKBN
           EVALUATE    SPA-PTHKN-HKNNUM (IDX)
      *    船員
           WHEN    "002"
               EVALUATE    WRK-RJN-RATE    ALSO  SPA-PTHKN-HOJOKBN(IDX)
      *        負担割合（１０％）
                   WHEN    "1"     ALSO    SPACE
                   WHEN    "1"     ALSO    "0"
                       MOVE    "9"             TO  WRK-PTHKN-HOJOKBN
                   WHEN    "1"     ALSO    "1"
                       MOVE    "A"             TO  WRK-PTHKN-HOJOKBN
                   WHEN    "1"     ALSO    "2"
                       MOVE    "B"             TO  WRK-PTHKN-HOJOKBN
                   WHEN    "1"     ALSO    "3"
                       MOVE    "C"             TO  WRK-PTHKN-HOJOKBN
      *        負担割合（２０％）
                   WHEN    "2"     ALSO    SPACE
                   WHEN    "2"     ALSO    "0"
                       MOVE    "8"             TO  WRK-PTHKN-HOJOKBN
                   WHEN    "2"     ALSO    "1"
                       MOVE    "D"             TO  WRK-PTHKN-HOJOKBN
                   WHEN    "2"     ALSO    "2"
                       MOVE    "E"             TO  WRK-PTHKN-HOJOKBN
                   WHEN    "2"     ALSO    "3"
                       MOVE    "F"             TO  WRK-PTHKN-HOJOKBN
      *        負担割合（３０％）
                   WHEN    "3"     ALSO    SPACE
                   WHEN    "3"     ALSO    "0"
                       MOVE    "7"             TO  WRK-PTHKN-HOJOKBN
                   WHEN    "3"     ALSO    "1"
                       MOVE    "G"             TO  WRK-PTHKN-HOJOKBN
                   WHEN    "3"     ALSO    "2"
                       MOVE    "H"             TO  WRK-PTHKN-HOJOKBN
                   WHEN    "3"     ALSO    "3"
                       MOVE    "I"             TO  WRK-PTHKN-HOJOKBN
               END-EVALUATE
      *    共済
           WHEN    "031"   THRU    "034"
               EVALUATE    WRK-RJN-RATE  ALSO  SPA-PTHKN-HOJOKBN(IDX)
                   WHEN    "1"     ALSO    "2"
                       MOVE    "B"             TO  WRK-PTHKN-HOJOKBN
                   WHEN    "2"     ALSO    "2"
                       MOVE    "E"             TO  WRK-PTHKN-HOJOKBN
                   WHEN    "1"     ALSO    ANY
                       MOVE    "9"             TO  WRK-PTHKN-HOJOKBN
                   WHEN    "2"     ALSO    ANY
                       MOVE    "8"             TO  WRK-PTHKN-HOJOKBN
      *            ３割
                   WHEN    "3"     ALSO    "2"
                       MOVE    "H"             TO  WRK-PTHKN-HOJOKBN
                   WHEN    "3"     ALSO    ANY
                       MOVE    "7"             TO  WRK-PTHKN-HOJOKBN
               END-EVALUATE
      *    国保
           WHEN    "060"
      *    国保（特別療養費）
           WHEN    "068"
               EVALUATE    WRK-RJN-RATE
      *        負担割合１
                   WHEN    "1"
                       MOVE    "1"         TO  WRK-PTHKN-HOJOKBN
      *        負担割合２
                   WHEN    "2"
                       MOVE    "2"         TO  WRK-PTHKN-HOJOKBN
      *        負担割合３
                   WHEN    "3"
                       MOVE    "3"         TO  WRK-PTHKN-HOJOKBN
               END-EVALUATE
      *    後期高齢者
           WHEN    "039"
           WHEN    "040"
               EVALUATE    WRK-RJN-RATE
      *        負担割合１
                   WHEN    "1"
                       MOVE    "1"             TO  WRK-PTHKN-HOJOKBN
      *        負担割合３
                   WHEN    "3"
                       MOVE    "3"             TO  WRK-PTHKN-HOJOKBN
               END-EVALUATE
           WHEN    OTHER
      *    通常の場合
               EVALUATE    WRK-RJN-RATE
      *        負担割合１
                   WHEN    "1"
                       MOVE    "9"             TO  WRK-PTHKN-HOJOKBN
      *        負担割合２
                   WHEN    "2"
                       MOVE    "8"             TO  WRK-PTHKN-HOJOKBN
      *        負担割合３
                   WHEN    "3"
                       MOVE    "7"             TO  WRK-PTHKN-HOJOKBN
               END-EVALUATE
           END-EVALUATE
           IF      WRK-PTHKN-HOJOKBN   NOT =   SPACE
               MOVE    WRK-PTHKN-HOJOKBN   TO  SPA-PTHKN-HOJOKBN(IDX)
           END-IF
           .
       2002121-HOJOKBN-ROU-EXT.
           EXIT.
      *****************************************************************
      *    患者公費情報更新処理
      *****************************************************************
       200213-PTHKOHNF-CHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   4   )
                          OR  (WRK-ERRCD   NOT =   SPACE )
               IF     (PTMODREQ-INSURANCE-CLASS(IDX)   =   SPACE)
                  AND (PTMODREQ-PROVIDER-NUMBER(IDX)   =   SPACE)
                  AND (PTMODREQ-RECIPIENT-NUMBER(IDX)  =   SPACE)
                  AND (PTMODREQ-INSURANCE-START-DATE(IDX)  =   SPACE)
                  AND (PTMODREQ-INSURANCE-END-DATE(IDX)    =   SPACE)
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-DATA
      *            公費編集
      *H29.11
                   IF      SPA-PTKOH-MAX       >=  TBL-MAX-KOH
                       MOVE    "65"            TO  WRK-ERRCD
                   ELSE
      *!!
      *
                   ADD     1               TO  SPA-PTKOH-MAX
                   MOVE    SPA-PTKOH-MAX   TO  IDY
      *            新規の時公費ＩＤ取得
                   ADD     1               TO  SPA-PTNUM-KOHID
                   MOVE    SPA-PTNUM-KOHID TO  SPA-PTKOH-KOHID (IDY)
      *
                   MOVE    "1"             TO  SPA-PTKOH-SINKIKBN (IDY)
      *            公費の種類
                   MOVE    PTMODREQ-INSURANCE-CLASS(IDX)
                                           TO  SPA-PTKOH-KOHNUM (IDY)
      *            負担者番号
                   MOVE    PTMODREQ-PROVIDER-NUMBER(IDX)
                                           TO  SPA-PTKOH-FTNJANUM (IDY)
      *            受給者番号
                   MOVE    PTMODREQ-RECIPIENT-NUMBER (IDX)
                                           TO  SPA-PTKOH-JKYSNUM (IDY)
      *            有効開始日
                   MOVE    PTMODREQ-INSURANCE-START-DATE(IDX)
                                               TO  WRK-HEN-DATE
                   PERFORM 802-DAYHEN02-SEC
                   IF      WRK-SYMD            =   SPACE
                                               OR  ZERO
                       MOVE    SPA-SYSYMD  TO  SPA-PTKOH-TEKSTYMD (IDY)
                   ELSE
                       MOVE    WRK-SYMD    TO  SPA-PTKOH-TEKSTYMD (IDY)
                   END-IF
                   MOVE    SPA-PTKOH-TEKSTYMD (IDY)
                                           TO  SPA-PTKOH-TEKSTYMDW(IDY)
      *            有効終了日
                   MOVE    PTMODREQ-INSURANCE-END-DATE (IDX)
                                               TO  WRK-HEN-DATE
                   MOVE    SPACE               TO  WRK-HEN-TIME
                   PERFORM 802-DAYHEN02-SEC
                   IF      WRK-SYMD            =   SPACE
                                               OR  ZERO
                       MOVE    "99999999"  TO  SPA-PTKOH-TEKEDYMD (IDY)
                   ELSE
                       MOVE    WRK-SYMD    TO  SPA-PTKOH-TEKEDYMD (IDY)
                   END-IF
                   MOVE    SPA-PTKOH-TEKEDYMD (IDY)
                                           TO  SPA-PTKOH-TEKEDYMDW(IDY)
      *            確認年月日
                   MOVE    SPA-SYSYMD      TO  SPA-PTKOH-KAKUNINYMD(IDY)
                   MOVE    SPA-SYSYMD      TO  SPA-PTKOH-KAKUNINYMDW
                                                                   (IDY)
      *            支払区分
                   MOVE    "00"            TO  SPA-PTKOH-PAYKBN (IDY)
      *            第三者行為はエラー
                   IF      PTMODREQ-INSURANCE-CLASS(IDX)
                                               =   "970"
                       MOVE    "39"                TO  WRK-ERRCD
                   END-IF
      *
               END-IF
      *
               END-IF
           END-PERFORM
           .
       200213-PTHKOHNF-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ作成処理
      *****************************************************************
       200214-HKNCOMBI-SYORI-SEC     SECTION.
      *
      **   MOVE    SPACE               TO  WRK-KEIERRCD
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    ZERO                TO  FLG-HKNEND
           MOVE    SPACE               TO  SPA-MOTOPG
      *
           PERFORM UNTIL    (FLG-HKNEND    =   1   )
               MOVE    SPA-P02SCRAREA      TO  SPA-FREE
               MOVE    SPA-P02KYOUTU       TO  SPA-WORK
               MOVE    SPA-AREA            TO  SPA-COMMON
      *        基本情報チェック
               MOVE    "CLICKED"           TO  MCP-EVENT
               MOVE    "API"               TO  MCP-WIDGET
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
               MOVE    SPACE               TO  WRK-ERR-KOHNUM
               MOVE    SPACE               TO  WRK-ERR-KOHNUMMEI
               IF      SPA-ERRCD       NOT =   SPACE
                   IF      SPA-KOH-ERRIDX      =   ZERO
                       MOVE    SPA-GMN-HKNNUM  TO  WRK-ERR-KOHNUM
                       MOVE    SPA-GMN-HKNNUMMEI
                                               TO  WRK-ERR-KOHNUMMEI
                   ELSE
      *                公費エラー
                       MOVE    SPA-GMN-KOHNUM (SPA-KOH-ERRIDX)
                                               TO  WRK-ERR-KOHNUM
                       MOVE    SPA-GMN-KOHNUMMEI (SPA-KOH-ERRIDX)
                                               TO  WRK-ERR-KOHNUMMEI
                   END-IF
               END-IF
               IF     (SPA-ERRCD           =   SPACE )
                  AND (SPA-PIDCD           =   SPACE )
                   MOVE    1               TO  FLG-HKNEND
               END-IF
               IF     (SPA-ERRCD(1:1)      =   "K"   )
                   IF      WRK-KEIERRCD        =   SPACE
                       MOVE    SPA-ERRCD       TO  WRK-KEIERRCD
                   END-IF
                   MOVE    SPACE           TO  SPA-ERRCD
               END-IF
               IF     (SPA-ERRCD       NOT =   SPACE )
                   MOVE    "H1"            TO  WRK-ERRCD
                   MOVE    1               TO  FLG-HKNEND
               END-IF
               IF     (SPA-PIDCD       NOT =   SPACE )
                   MOVE    "P2"            TO  WRK-ERRCD
                   MOVE    1               TO  FLG-HKNEND
               END-IF
           END-PERFORM
      *
      *    保険種類の変更チェック
           IF      WRK-ERRCD           =   SPACE
               IF     (PTMODREQ-MAIN-INSURANCE-CLASS
                                       NOT =   SPACE)
                 AND  (PTMODREQ-MAIN-INSURANCE-CLASS
                                       NOT =   SPA-PTHKN-HKNNUM (1))
                   ADD     1                   TO  IDX-W
                   MOVE    "K3"                TO  WRK-KEICD (IDX-W)
               END-IF
           END-IF
           .
       200214-HKNCOMBI-SYORI-EXT.
           EXIT.
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
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   ZERO
               MOVE    "82"                TO  WRK-ERRCD
           END-IF
      *    重複登録チェック
           IF     WRK-ERRCD            =   SPACE
               PERFORM 200234-JYUFUKU-CHK-SEC
           END-IF
      *    患者情報編集処理
           IF     WRK-ERRCD            =   SPACE
      *        登録
               PERFORM 200232-PTINF-INSERT-SEC
           END-IF
      *H30.4
      *    妊婦設定処理
           IF     WRK-ERRCD            =   SPACE
      *        登録
               IF      SPA-GMN28-NINPUKBN   =   "1"
                   PERFORM 200234-NINPUKBN-SYORI-SEC
               END-IF
           END-IF
      *
      *    保険情報編集処理
           IF     WRK-ERRCD            =   SPACE
      *        登録
               PERFORM 200233-PTHKN-INSERT-SEC
           END-IF
      *
           .
       20023-PTINFINS-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者重複登録チェック処理
      *****************************************************************
       200234-JYUFUKU-CHK-SEC        SECTION.
      *
           MOVE    SPACE               TO  PTINF-REC
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-GMN-KANANAME    TO  PTINF-KANANAME
           MOVE    SPA-NAI-BIRTHDAY    TO  PTINF-BIRTHDAY
           MOVE    SPA-GMN-SEX         TO  PTINF-SEX
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 910-PTINF-NEXT-SEC
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    ZERO                TO  FLG-PTINF-JYU
           PERFORM   UNTIL  ( FLG-PTINF    =   1  )
                       OR   ( FLG-PTINF-JYU    =   1  )
      *        氏名・漢字氏名・性別・生年月日一致
               IF     (PTINF-KANANAME      =   SPA-GMN-KANANAME)
                 AND  (PTINF-NAME          =   SPA-GMN-NAME    )
                 AND  (PTINF-SEX           =   SPA-GMN-SEX     )
                 AND  (PTINF-BIRTHDAY      =   SPA-NAI-BIRTHDAY)
                   MOVE    1                   TO  FLG-PTINF-JYU
                   ADD     1                   TO  IDX-W
                   MOVE    "K0"                TO  WRK-KEICD(IDX-W)
               END-IF
      *
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 910-PTINF-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *
      *    同じ内容の公費チェック
           IF     (SPA-PTKOH-MAX       >   ZERO )
             AND  (WRK-ERRCD           =   SPACE)
               PERFORM VARYING    IDX      FROM   1   BY  1
                       UNTIL     (IDX          >   SPA-PTKOH-MAX )
                             OR  (WRK-ERRCD    NOT =   SPACE )
                   IF      SPA-PTKOH-SINKIKBN (IDX)    =   "1"
                       PERFORM 2004141-KOHJYUCHK-SEC
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       200234-JYUFUKU-CHK-EXT.
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
      *H26.8
      *    テスト患者番号区分
           MOVE    "0"                 TO  PTINF-TSTPTNUMKBN
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
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
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
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
      *    患者保険・公費・保険組合せ更新処理
      *****************************************************************
       200233-PTHKN-INSERT-SEC        SECTION.
      *
           MOVE    SPA-OPID            TO  HZN-SPA-OPID
           MOVE    WRK-OPID            TO  SPA-OPID
           MOVE    SPA-PTID            TO  SPA-GMN-PTID
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
           MOVE    "API"               TO  SPA-P02-GAMEN
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
      *TEST
      *     DISPLAY "ORCGP029 END:"  SPA-ERRCD
      *
           MOVE    HZN-SPA-OPID        TO  SPA-OPID
      *
           .
       200233-PTHKN-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者更新処理
      *****************************************************************
       2003-PTINF-UPDATE-SEC        SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    "10"                TO  WRK-ERRCD
               GO      TO              2003-PTINF-UPDATE-EXT
           END-IF
      *H27.01
      *    処理区分
           EVALUATE    PTMODREQ-MOD-KEY
      *        氏名変更
               WHEN    "1"
               WHEN    " "
      *            性別・生年月日が一致すること
                   IF     (PTINF-SEX       =   SPA-GMN-SEX )  AND
                          (PTINF-BIRTHDAY  =   SPA-NAI-BIRTHDAY )
                       CONTINUE
                   ELSE
                       MOVE    "50"                TO  WRK-ERRCD
                   END-IF
      *        性別・生年月日変更
               WHEN    "2"
      *            氏名が一致すること
                   IF     (PTINF-NAME      =   SPA-GMN-NAME   )  AND
                          (PTINF-KANANAME  =   SPA-GMN-KANANAME)
                       CONTINUE
                   ELSE
                       MOVE    "50"                TO  WRK-ERRCD
                   END-IF
               WHEN    OTHER
                   MOVE    "53"            TO  WRK-ERRCD
           END-EVALUATE
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO              2003-PTINF-UPDATE-EXT
           END-IF
      *
      *    排他チェック & ロック処理
           PERFORM 990-LOCK-IN-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "90"            TO  WRK-ERRCD
               GO      TO              2003-PTINF-UPDATE-EXT
           END-IF
      *
      *    患者情報内容編集
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20031-UPDATE-CHK-SEC
           END-IF
      *
      *    更新処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20023-PTINFUPD-SYORI-SEC
           END-IF
      *H30.4
      *    妊婦設定処理
           IF     WRK-ERRCD            =   SPACE
               PERFORM 200234-NINPUKBN-SYORI-SEC
           END-IF
      *
      *    CLAIM送信チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 4905-CLAIM-CHK-SEC
           END-IF
      *
      *    ユーザ起動プログラム起動チェック
           MOVE    2                   TO  FLG-USERPG
           IF      WRK-ERRCD           =   SPACE
               PERFORM 4907-USERPG-SEC
           END-IF
      *
           PERFORM 990-LOCK-OUT-SEC
           IF      SLOCK-RETURN   NOT =   ZERO
               DISPLAY "ロック解除失敗"
           END-IF
           .
       2003-PTINF-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報内容編集・チェック処理
      *****************************************************************
       20031-UPDATE-CHK-SEC        SECTION.
      *
           MOVE    SPA-PTID            TO  SPA-GMN-PTID
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
      *    基本情報
           PERFORM 200211-PTINF-CHK-SEC
      *    旧姓履歴チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20031-KYUSEI-HEN-SEC
           END-IF
           .
       20031-UPDATE-CHK-EXT.
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
           IF      FLG-KYUSEI          =   1
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
      *R01.6
      *    減免事由
           MOVE    SPA-GMN-DISCOUNTKBN     TO  PTINF-DISCOUNT-KBN
      *    割引率
           MOVE    SPA-GMN-DISCOUNT-CD     TO  PTINF-DISCOUNT
      *    入金方法
           MOVE    SPA-GMN-NYUKIN-HOHO     TO  PTINF-NYUKIN-HOHO
      *
      *    患者情報出力内容編集処理
           PERFORM 20021-PTMODRES-HEN-SEC
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "API KYUSEIRRK INS ERR:" MCP-RC
                             ",KEY:" KYUSEI-KEY
               MOVE    "52"                TO  WRK-ERRCD
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
           MOVE    PTINF-SETAINUSI     TO  PTMODRES-SETAI-NAME
           MOVE    PTINF-ZOKUGARA      TO  PTMODRES-ZOKUGARA
           MOVE    PTINF-JOB           TO  PTMODRES-JOB
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
      *    コメント
           MOVE    PTINF-COMMENT-1     TO  PTMODRES-COMMENT-1
           MOVE    PTINF-COMMENT-2     TO  PTMODRES-COMMENT-2
           MOVE    PTINF-TABOO-1       TO  PTMODRES-TABOO-1
           MOVE    PTINF-TABOO-2       TO  PTMODRES-TABOO-2
           MOVE    PTINF-ALLERGY-1     TO  PTMODRES-ALLERGY-1
           MOVE    PTINF-ALLERGY-2     TO  PTMODRES-ALLERGY-2
           MOVE    PTINF-KANSENSYO-1   TO  PTMODRES-KANSENSYO-1
           MOVE    PTINF-KANSENSYO-2   TO  PTMODRES-KANSENSYO-2
           .
       20021-PTMODRES-HEN-EXT.
           EXIT.
      *
      *H30.4
      *****************************************************************
      *    妊婦設定処理
      *****************************************************************
       200234-NINPUKBN-SYORI-SEC     SECTION.
      *
      *    妊婦区分
           IF      SPA-GMN28-NINPUKBN   =   ZERO
               MOVE    SPACE           TO  SPA-GMN28-NINPUKBN
           END-IF
           MOVE    SPA-GMN28-NINPUKBN  TO  WRK-CONF-CDATA
           MOVE    "NINPUKBN"          TO  WRK-CONF-CKEY
           PERFORM 2002341-PTCONF-DBSYORI-SEC
           .
       200234-NINPUKBN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    患者個別設定登録処理
      *****************************************************************
       2002341-PTCONF-DBSYORI-SEC        SECTION.
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
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTCONF-REC
                   MOVE    ZERO            TO  FLG-PTCONF
               ELSE
                   MOVE    1               TO  FLG-PTCONF
                   INITIALIZE                  PTCONF-REC
               END-IF
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
                       DISPLAY "API PTCONF DEL ERR:" MCP-RC
                               ",KEY:" PTCONF-KEY
                       MOVE    "83"             TO  WRK-ERRCD
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
       2111-PTCONF-DBSYORI-EXT.
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
               DISPLAY "API PTCONF INS ERR:" MCP-RC
                           ",KEY:" PTCONF-KEY
               MOVE    "45"             TO  WRK-ERRCD
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "API PTCONF UPD ERR:" MCP-RC
                           ",KEY:" PTCONF-KEY
               MOVE    "54"             TO  WRK-ERRCD
           END-IF
           .
       21111-PTCONF-UPS-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       20023-HKNCOMBI-HEN-SEC     SECTION.
      *
      *    保険組合せ編集処理
           INITIALIZE                      ORCSAPIHKNAREA
      *    すべてを対象
           MOVE    "1"                 TO  ORCSAPIHKN-KBN2
           CALL    "ORCSAPIHKN"        USING
                                       SPA-AREA
                                       ORCSAPIHKNAREA
      *
           IF      ORCSAPIHKN-ERRCD    =   9
               ADD     1                   TO  IDX-W
               MOVE    "K4"                TO  WRK-KEICD (IDX-W)
           END-IF
      *
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   20  )
                         OR   (ORCSAPIHKN-THKNCOMBI (IDX)
                                       =   SPACE  )
               PERFORM 200231-HKNHEN-TBL-SEC
           END-PERFORM
           .
       20023-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力領域保険情報編集処理
      *****************************************************************
       200231-HKNHEN-TBL-SEC              SECTION.
      *
           ADD     1                   TO  IDY
      *    保険組合せ番号
           MOVE    ORCSAPIHKN-THKNCOMBI (IDX)
                                   TO  PTMODRES-COMBINATION-NUMBER
                                                                 (IDY)
      *    保険の種類
           MOVE    ORCSAPIHKN-HKNNUM (IDX)
                                   TO  PTMODRES-MAIN-INSURANCE-CLASS
                                                                 (IDY)
      *    保険者番号
           MOVE    ORCSAPIHKN-HKNJANUM (IDX)
                                   TO  PTMODRES-MAIN-INSURANCE-NUMBER
                                                                 (IDY)
      *    保険の名称
           MOVE    ORCSAPIHKN-HKNNUM-NAME (IDX)
                                   TO  PTMODRES-MAIN-INSURANCE-NAME
                                                                 (IDY)
      *    記号
           MOVE    ORCSAPIHKN-KIGO  (IDX)
                                   TO  PTMODRES-MAIN-MARK       (IDY)
      *    番号
           MOVE    ORCSAPIHKN-NUM  (IDX)
                                   TO  PTMODRES-MAIN-NUMBER     (IDY)
      *R02.4
      *    枝番
           MOVE    ORCSAPIHKN-EDABAN (IDX)
                                   TO  PTMODRES-MAIN-EDABAN     (IDY)
      *    継続区分
           MOVE    ORCSAPIHKN-CONTKBN  (IDX)
                                   TO  PTMODRES-MAIN-CONTINUATION(IDY)
      *    補助区分
           MOVE    ORCSAPIHKN-HOJOKBN  (IDX)
                                   TO  PTMODRES-MAIN-ASSISTANCE (IDY)
      *    本人家族区分
           MOVE    ORCSAPIHKN-HONKZKKBN  (IDX)
                                   TO  PTMODRES-MAIN-FAMILY     (IDY)
      *    被保険者名
           MOVE    ORCSAPIHKN-HIHKNJANAME  (IDX)
                                   TO  PTMODRES-MAIN-POLICYHOLDER(IDY)
      *    有効開始日
           MOVE    ORCSAPIHKN-TEKSTYMD  (IDX)
                                   TO  PTMODRES-MAIN-START-DATE  (IDY)
      *    有効終了日
           MOVE    ORCSAPIHKN-TEKEDYMD  (IDX)
                                   TO  PTMODRES-MAIN-END-DATE  (IDY)
      *    資格取得日
           MOVE    ORCSAPIHKN-SKKGETYMD  (IDX)
                                   TO  PTMODRES-MAIN-GET-DATE  (IDY)
      *    公費情報
           PERFORM VARYING     IDY2    FROM    1   BY  1
                   UNTIL       IDY2    >   4
      *        公費の種類
               MOVE    ORCSAPIHKN-KOHNUM (IDX IDY2)
                                       TO  PTMODRES-INSURANCE-CLASS
                                                             (IDY IDY2)
      *        公費の種類名称
               MOVE    ORCSAPIHKN-KOHNUM-NAME (IDX IDY2)
                                       TO  PTMODRES-INSURANCE-NAME
                                                             (IDY IDY2)
      *        負担者番号
               MOVE    ORCSAPIHKN-FTNJANUM (IDX IDY2)
                                       TO  PTMODRES-PROVIDER-NUMBER
                                                             (IDY IDY2)
      *        受給者番号
               MOVE    ORCSAPIHKN-JKYSNUM (IDX IDY2)
                                       TO  PTMODRES-RECIPIENT-NUMBER
                                                             (IDY IDY2)
      *        有効開始日
               MOVE    ORCSAPIHKN-KOH-TEKSTYMD (IDX IDY2)
                                       TO  PTMODRES-INSURANCE-START-DATE
                                                             (IDY IDY2)
      *        有効終了日
               MOVE    ORCSAPIHKN-KOH-TEKEDYMD (IDX IDY2)
                                       TO  PTMODRES-INSURANCE-END-DATE
                                                             (IDY IDY2)
      *        入院−負担率（割）
               MOVE    ORCSAPIHKN-RATE-ADMISSION (IDX IDY2)
                                       TO  PTMODRES-RATE-ADMISSION
                                                             (IDY IDY2)
      *        入院−固定額
               MOVE    ORCSAPIHKN-MONEY-ADMISSION (IDX IDY2)
                                       TO  PTMODRES-MONEY-ADMISSION
                                                             (IDY IDY2)
      *        外来−負担率（割）
               MOVE    ORCSAPIHKN-RATE-OUTPATIENT (IDX IDY2)
                                       TO  PTMODRES-RATE-OUTPATIENT
                                                             (IDY IDY2)
      *        外来−固定額
               MOVE    ORCSAPIHKN-MONEY-OUTPATIENT (IDX IDY2)
                                       TO  PTMODRES-MONEY-OUTPATIENT
                                                             (IDY IDY2)
           END-PERFORM
      *
           .
       20023-HKNHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       2003-PTINF-DELETE-SEC     SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    "10"                TO  WRK-ERRCD
               GO      TO              2003-PTINF-DELETE-EXT
           END-IF
      *
      *    排他チェック & ロック処理
           PERFORM 990-LOCK-IN-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "90"            TO  WRK-ERRCD
               GO      TO              2003-PTINF-DELETE-EXT
           END-IF
      *    性別・生年月日が一致すること
           IF     (PTINF-SEX           =   SPA-GMN-SEX )  AND
                  (PTINF-BIRTHDAY      =   SPA-NAI-BIRTHDAY ) AND
                  (PTINF-NAME          =   SPA-GMN-NAME )
               CONTINUE
           ELSE
               MOVE    "81"                TO  WRK-ERRCD
           END-IF
      *    患者情報内容編集
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20031-DELETE-CHK-SEC
           END-IF
      *
      *    患者番号削除処理
           IF      WRK-ERRCD           =   SPACE
      *R01.10
               MOVE    SPA-GMN-NAME        TO  SPA-NAME
               MOVE    SPA-OPID            TO  HZN-SPA-OPID
               MOVE    WRK-OPID            TO  SPA-OPID
               CALL    "ORCSP05DL"         USING
                                           SPA-AREA
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    "83"            TO  WRK-ERRCD
               END-IF
               MOVE    HZN-SPA-OPID        TO  SPA-OPID
           END-IF
      *
      *    ユーザ起動プログラム起動チェック
           MOVE    3                   TO  FLG-USERPG
           IF      WRK-ERRCD           =   SPACE
               PERFORM 4907-USERPG-SEC
           END-IF
      *
           PERFORM 990-LOCK-OUT-SEC
           IF      SLOCK-RETURN   NOT =   ZERO
               DISPLAY "ロック解除失敗"
           END-IF
           .
       2003-PTINF-DELETE-EXT.
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
               GO      TO      20031-DELETE-CHK-EXT
           END-IF
           .
       20031-DELETE-CHK-EXT.
           EXIT.
      *
      *H25.10
      *****************************************************************
      *    保険・公費追加処理
      *****************************************************************
       2004-PTHKNKOH-INS-SEC        SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    "10"                TO  WRK-ERRCD
               GO      TO              2004-PTHKNKOH-INS-EXT
           END-IF
      *    性別・生年月日が一致すること
           IF     (PTINF-SEX           =   SPA-GMN-SEX )  AND
                  (PTINF-BIRTHDAY      =   SPA-NAI-BIRTHDAY ) AND
                  (PTINF-NAME          =   SPA-GMN-NAME )
               CONTINUE
           ELSE
               MOVE    "60"                TO  WRK-ERRCD
               GO      TO      2004-PTHKNKOH-INS-EXT
           END-IF
      *
      *    排他チェック & ロック処理
           PERFORM 990-LOCK-IN-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "90"            TO  WRK-ERRCD
               GO      TO      2004-PTHKNKOH-INS-EXT
           END-IF
      *
      *    患者情報出力内容編集処理
           PERFORM 20041-PTMODRES-HEN-SEC
      *
      *    患者情報内容編集
      *    パラメタファイル、患者情報のセット
           MOVE    SPACE               TO  P02
           INITIALIZE                      P02
      *
           MOVE    SPA-PTID            TO  SPA-GMN-PTID
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
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
      *    生年月日から年齢編集処理
           PERFORM 200211-BIRTHDAY-CHK-SEC
      *
           MOVE    ZERO                TO  FLG-DATA
      *    保険情報チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200212-PTHKNINF-CHK-SEC
           END-IF
      *    公費情報チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200213-PTHKOHNF-CHK-SEC
           END-IF
      *
           IF      FLG-DATA            =   ZERO
               MOVE    "61"                TO  WRK-ERRCD
      *
               PERFORM 990-LOCK-OUT-SEC
               GO      TO      2004-PTHKNKOH-INS-EXT
           END-IF
      *
      *    保険組合せ作成チェック
           IF     (WRK-ERRCD           =   SPACE )
               PERFORM 200414-HKNCOMBI-INS-SYORI-SEC
           END-IF
      *    保険情報編集処理
           IF     (WRK-ERRCD           =   SPACE )
      *        保険組合せの変更による履歴チェック
               MOVE    SPA-P02KYOUTU   TO  SPA-WORK
               MOVE    SPA-P02SCRAREA  TO  SPA-FREE
               MOVE    SPA-AREA        TO  SPA-COMMON
      *        更新サブへ
               CALL    "ORCGP029"      USING   MCPAREA
                                               SPAAREA
               MOVE    SPA-COMMON      TO  SPA-AREA
               MOVE    SPA-WORK        TO  SPA-P02KYOUTU
               MOVE    SPA-FREE        TO  SPA-P02SCRAREA
               IF     (SPA-PIDCD       NOT =   SPACE )
                   MOVE    "P2"            TO  WRK-ERRCD
                   MOVE    1               TO  FLG-HKNEND
               END-IF
               IF     (WRK-ERRCD           =   SPACE )
      *            保険登録
                   PERFORM 200233-PTHKN-INSERT-SEC
                   IF    WRK-ERRCD           =   SPACE
      *                保険・公費ＩＤ登録
                       PERFORM 20042-PTNUM-UPDATE-SEC
                   END-IF
                   IF     (WRK-ERRCD       NOT =   SPACE )
      *                ＤＢ更新キャンセル
                       MOVE    SPACE               TO  MCPDATA-REC
                       MOVE    "DBSELECT"          TO  MCP-FUNC
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "abort"             TO  MCP-PATHNAME
                       CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   END-IF
               END-IF
           END-IF
      *
      *    パラメタ削除処理（保険情報）
           PERFORM 4904-PARA-DEL-SEC
      *
      *    保険組合せ編集処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20023-HKNCOMBI-HEN-SEC
           END-IF
      *
      *    CLAIM送信チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 4905-CLAIM-CHK-SEC
           END-IF
      *
      *    ユーザ起動プログラム起動チェック
           MOVE    2                   TO  FLG-USERPG
           IF      WRK-ERRCD           =   SPACE
               PERFORM 4907-USERPG-SEC
           END-IF
      *
           PERFORM 990-LOCK-OUT-SEC
           IF      SLOCK-RETURN   NOT =   ZERO
               DISPLAY "ロック解除失敗"
           END-IF
           .
       2004-PTHKNKOH-INS-EXT.
           EXIT.
      *****************************************************************
      *    患者情報出力内容編集処理
      *****************************************************************
       20041-PTMODRES-HEN-SEC     SECTION.
      *
      *    患者情報
           MOVE    PTINF-NAME          TO  PTMODRES-NAME
           MOVE    PTINF-KANANAME      TO  PTMODRES-KANANAME
           MOVE    PTINF-SEX           TO  PTMODRES-SEX
           MOVE    SPACE               TO  PTMODRES-SETAI-NAME
           MOVE    SPACE               TO  PTMODRES-ZOKUGARA
           MOVE    SPACE               TO  PTMODRES-JOB
           MOVE    SPACE               TO  PTMODRES-KEITAI-TEL
           MOVE    SPACE               TO  PTMODRES-FAX
           MOVE    SPACE               TO  PTMODRES-EMAIL
      *    自宅情報
           MOVE    PTINF-HOME-POST     TO  PTMODRES-HOME-ZIP-CODE
           MOVE    PTINF-HOME-ADRS     TO  PTMODRES-HOME-ADDRESSES1
           MOVE    PTINF-HOME-BANTI    TO  PTMODRES-HOME-ADDRESSES2
           MOVE    PTINF-HOME-TEL1     TO  PTMODRES-HOME-HPONE1
           MOVE    PTINF-HOME-TEL2     TO  PTMODRES-HOME-HPONE2
      *    勤務先情報
           MOVE    SPACE               TO  PTMODRES-OFFICE-INFO
      *    コメント
           MOVE    SPACE               TO  PTMODRES-COMMENT-1
           MOVE    SPACE               TO  PTMODRES-COMMENT-2
           MOVE    SPACE               TO  PTMODRES-TABOO-1
           MOVE    SPACE               TO  PTMODRES-TABOO-2
           MOVE    SPACE               TO  PTMODRES-ALLERGY-1
           MOVE    SPACE               TO  PTMODRES-ALLERGY-2
           MOVE    SPACE               TO  PTMODRES-KANSENSYO-1
           MOVE    SPACE               TO  PTMODRES-KANSENSYO-2
           .
       20041-PTMODRES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ作成処理
      *****************************************************************
       200414-HKNCOMBI-INS-SYORI-SEC     SECTION.
      *
      **   MOVE    SPACE               TO  WRK-KEIERRCD
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    ZERO                TO  FLG-HKNEND
           MOVE    SPACE               TO  SPA-MOTOPG
      *
           PERFORM UNTIL    (FLG-HKNEND    =   1   )
               MOVE    SPA-P02SCRAREA      TO  SPA-FREE
               MOVE    SPA-P02KYOUTU       TO  SPA-WORK
               MOVE    SPA-AREA            TO  SPA-COMMON
      *        基本情報チェック
               MOVE    "CLICKED"           TO  MCP-EVENT
               MOVE    "API4"              TO  MCP-WIDGET
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
               MOVE    SPACE               TO  WRK-ERR-KOHNUM
               MOVE    SPACE               TO  WRK-ERR-KOHNUMMEI
               IF      SPA-ERRCD       NOT =   SPACE
                   IF      SPA-KOH-ERRIDX      =   ZERO
      *                保険エラー
                       MOVE    SPA-GMN-HKNNUM  TO  WRK-ERR-KOHNUM
                       MOVE    SPA-GMN-HKNNUMMEI
                                               TO  WRK-ERR-KOHNUMMEI
                   ELSE
      *                公費エラー
                       MOVE    SPA-GMN-KOHNUM (SPA-KOH-ERRIDX)
                                               TO  WRK-ERR-KOHNUM
                       MOVE    SPA-GMN-KOHNUMMEI (SPA-KOH-ERRIDX)
                                               TO  WRK-ERR-KOHNUMMEI
                   END-IF
               END-IF
               IF      SPA-PIDCD       NOT =   SPACE
                   IF     (SPA-KOH-ERRIDX      =   ZERO)
                      AND (SPA-KOH-IDX1        =   ZERO)
      *                保険エラー
                       MOVE    SPA-GMN-HKNNUM  TO  WRK-ERR-KOHNUM
                       MOVE    SPA-GMN-HKNNUMMEI
                                               TO  WRK-ERR-KOHNUMMEI
                   ELSE
      *                公費エラー
                       IF      SPA-KOH-ERRIDX  NOT =   ZERO
                           MOVE    SPA-GMN-KOHNUM (SPA-KOH-ERRIDX)
                                               TO  WRK-ERR-KOHNUM
                           MOVE    SPA-GMN-KOHNUMMEI (SPA-KOH-ERRIDX)
                                               TO  WRK-ERR-KOHNUMMEI
                       END-IF
                       IF      SPA-KOH-IDX1  NOT =   ZERO
                           MOVE    SPA-GMN-KOHNUM (SPA-KOH-IDX1)
                                               TO  WRK-ERR-KOHNUM
                           MOVE    SPA-GMN-KOHNUMMEI (SPA-KOH-IDX1)
                                               TO  WRK-ERR-KOHNUMMEI
                       END-IF
                   END-IF
               END-IF
               IF     (SPA-ERRCD           =   SPACE )
                  AND (SPA-PIDCD           =   SPACE )
                   MOVE    1               TO  FLG-HKNEND
               END-IF
               IF     (SPA-ERRCD(1:1)      =   "K"   )
                   IF      WRK-KEIERRCD        =   SPACE
                       MOVE    SPA-ERRCD       TO  WRK-KEIERRCD
                   END-IF
                   MOVE    SPACE           TO  SPA-ERRCD
               END-IF
               IF     (SPA-ERRCD       NOT =   SPACE )
                   MOVE    "H1"            TO  WRK-ERRCD
                   MOVE    1               TO  FLG-HKNEND
               END-IF
               IF     (SPA-PIDCD       NOT =   SPACE )
                   MOVE    "P2"            TO  WRK-ERRCD
                   MOVE    1               TO  FLG-HKNEND
               END-IF
           END-PERFORM
      *
      *    保険種類の変更チェック
           IF      WRK-ERRCD           =   SPACE
               IF     (PTMODREQ-MAIN-INSURANCE-CLASS
                                       NOT =   SPACE)
                 AND  (PTMODREQ-MAIN-INSURANCE-CLASS
                                       NOT =   SPA-PTHKN-HKNNUM
                                                    (SPA-PTHKN-MAX))
                   ADD     1                   TO  IDX-W
                   MOVE    "K3"                TO  WRK-KEICD(IDX-W)
               END-IF
           END-IF
      *    同じ内容の保険チェック
           IF     (SPA-PTHKN-MAX       >   ZERO )
             AND  (SPA-PTHKN-SINKIKBN (SPA-PTHKN-MAX)  =   "1" )
               PERFORM VARYING    IDX      FROM   1   BY  1
                       UNTIL     (IDX          >=  SPA-PTHKN-MAX )
                             OR  (WRK-ERRCD    NOT =   SPACE )
                   IF      (SPA-PTHKN-HKNNUM (IDX)
                                       =   SPA-PTHKN-HKNNUM
                                                 (SPA-PTHKN-MAX))
                   AND     (SPA-PTHKN-HKNJANUM(IDX)
                                       =   SPA-PTHKN-HKNJANUM
                                                 (SPA-PTHKN-MAX))
                   AND     (SPA-PTHKN-KIGO(IDX)
                                       =   SPA-PTHKN-KIGO
                                                 (SPA-PTHKN-MAX))
                   AND     (SPA-PTHKN-NUM(IDX)
                                       =   SPA-PTHKN-NUM
                                                 (SPA-PTHKN-MAX))
                   AND     (SPA-PTHKN-HOJOKBN(IDX)
                                       =   SPA-PTHKN-HOJOKBN
                                                 (SPA-PTHKN-MAX))
                   AND     (SPA-PTHKN-HONKZKKBN(IDX)
                                       =   SPA-PTHKN-HONKZKKBN
                                                 (SPA-PTHKN-MAX))
                   AND     (SPA-PTHKN-CONTKBN(IDX)
                                       =   SPA-PTHKN-CONTKBN
                                                 (SPA-PTHKN-MAX))
                   AND     (SPA-PTHKN-HIHKNJANAME(IDX)
                                       =   SPA-PTHKN-HIHKNJANAME
                                                 (SPA-PTHKN-MAX))
                       MOVE   "62"             TO  WRK-ERRCD
                   END-IF
               END-PERFORM
           END-IF
      *    同じ内容の公費チェック
           IF     (SPA-PTKOH-MAX       >   ZERO )
             AND  (WRK-ERRCD           =   SPACE)
             AND  (SPA-PTKOH-SINKIKBN (SPA-PTKOH-MAX)  =   "1" )
               PERFORM VARYING    IDX      FROM   1   BY  1
                       UNTIL     (IDX          >   SPA-PTKOH-MAX )
                             OR  (WRK-ERRCD    NOT =   SPACE )
                   IF      SPA-PTKOH-SINKIKBN (IDX)    =   "1"
                       PERFORM 2004141-KOHJYUCHK-SEC
                   END-IF
               END-PERFORM
           END-IF
           .
       200414-HKNCOMBI-INS-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    同じ内容の公費チェック処理
      *****************************************************************
       2004141-KOHJYUCHK-SEC               SECTION.
      *
           PERFORM VARYING    IDY      FROM   1   BY  1
                   UNTIL     (IDY      >   SPA-PTKOH-MAX)
                         OR  (WRK-ERRCD    NOT =   SPACE )
               IF      (IDX        NOT =   IDY )
               AND     (SPA-PTKOH-KOHNUM (IDX)
                                       =   SPA-PTKOH-KOHNUM (IDY))
               AND     (SPA-PTKOH-FTNJANUM (IDX)
                                       =   SPA-PTKOH-FTNJANUM (IDY))
               AND     (SPA-PTKOH-JKYSNUM (IDX)
                                       =   SPA-PTKOH-JKYSNUM (IDY))
               AND     (SPA-PTKOH-TEKSTYMD (IDX)
                                       =   SPA-PTKOH-TEKSTYMD (IDY))
               AND     (SPA-PTKOH-TEKEDYMD (IDX)
                                       =   SPA-PTKOH-TEKEDYMD (IDY))
                   MOVE   "63"             TO  WRK-ERRCD
                   MOVE   SPA-PTKOH-KOHNUM(IDX)
                                           TO  WRK-ERR-KOHNUM
                   MOVE   SPA-PTKOH-KOHNUM-MEI(IDX)
                                           TO  WRK-ERR-KOHNUMMEI
               END-IF
           END-PERFORM
           .
       2004141-KOHJYUCHK-EXT.
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
               IF     (SPA-MAE-HKNID       =   PTNUM-HKNID)
                  AND (SPA-MAE-KOHID       =   PTNUM-KOHID)
                  AND (SPA-MAE-AUTOCOMBINUM  =   PTNUM-AUTOCOMBINUM)
                  AND (SPA-MAE-MANUCOMBINUM  =   PTNUM-MANUCOMBINUM)
                   CONTINUE
               ELSE
                   MOVE    "51"            TO  WRK-ERRCD
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
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       4904-PARA-DEL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ＣＬＡＩＭ接続情報取得処理
      *****************************************************************
       4905-CLAIM-CHK-SEC               SECTION.
      *
      *v4.8
      *    ginbee対応
           IF      SPA-MW              =   SPA-GINBEE
               GO      TO      4905-CLAIM-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-CLAIM
           MOVE    ZERO                TO  FLG-CLAIM-MULTIHOST
      *    ＣＬＡＩＭ接続判定
           MOVE    SPACE               TO  SYS-9000-REC
           INITIALIZE                  SYS-9000-REC
           MOVE    "9000"              TO  SYS-9000-KANRICD
           MOVE    "*"                 TO  SYS-9000-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-9000-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-9000-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-9000-HOSPNUM
           MOVE    SYS-9000-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-9000-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYSKANRI        =   1
               CONTINUE
           ELSE
               MOVE    MCPDATA-REC         TO  SYS-9000-REC
      *        マルチホストフラグ
               IF      SYS-9000-MULTIHOST-FLG  =   "1"
                    MOVE    1                   TO  FLG-CLAIM-MULTIHOST
               END-IF
               IF     (SYS-9000-SETUZOKU   =   ZERO )  OR
                      (SYS-9000-PT-FLG NOT =   "1"  )
                   CONTINUE
               ELSE
                   IF      SYS-9000-KANBUTTON (02) =   "T"
                       MOVE    2               TO  FLG-CLAIM
                   ELSE
                       MOVE    1               TO  FLG-CLAIM
                   END-IF
               END-IF
           END-IF
      *
      *    送信時ポップアップなしの時のみ送信
           IF      FLG-CLAIM           =   1
      *        ＣＬＡＩＭ接続情報取得処理
               PERFORM 4906-CLAIM-SOUSIN-SEC
           END-IF
           .
       4905-CLAIM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＬＡＩＭ接続情報取得処理
      *****************************************************************
       4906-CLAIM-SOUSIN-SEC               SECTION.
      *
           MOVE    SPACE               TO  SYS-9001-REC
           INITIALIZE                  SYS-9001-REC
           MOVE    "9001"              TO  SYS-9001-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-9001-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-9001-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-9001-HOSPNUM
           MOVE    SYS-9001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-9001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  WRK-RENNUM
           PERFORM
                   UNTIL       (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC         TO  SYS-9001-REC
               MOVE    ZERO                TO  FLG-SOUSIN
               IF      FLG-CLAIM-MULTIHOST =   1
      *            複数送信処理
                   MOVE    1                   TO  FLG-SOUSIN
               ELSE
                   MOVE    SYS-9001-KBNCD (1:2)  TO  WRK-GMN-SELNUM
                   IF      WRK-GMN-SELNUM        =   1
                       MOVE    1                   TO  FLG-SOUSIN
                       MOVE    1                   TO  FLG-SYSKANRI
                   END-IF
               END-IF
               IF      FLG-SOUSIN          =   1
                   PERFORM 49061-CLAIM-SOUSINSYOSI-SEC
               END-IF
      *
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-IF
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       4906-CLAIM-SOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＬＡＩＭ接続情報取得処理
      *****************************************************************
       49061-CLAIM-SOUSINSYOSI-SEC               SECTION.
      *
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    CLAIM-FILE          TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
      *
           MOVE    MKPASS-OT-01        TO  SHELLTBL-NAME
      *
      **** MOVE    "1"                 TO  SHELLTBL-ARG1
           MOVE    SYS-9000-CODE       TO  SHELLTBL-ARG1
           MOVE    SPA-PTID            TO  WRK-PTID
           MOVE    WRK-PTID            TO  SHELLTBL-ARG2
           MOVE    "XX"                TO  SHELLTBL-ARG3
           MOVE    WRK-UKETIME         TO  SHELLTBL-ARG4
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG5
      *
           MOVE    SYS-9001-ADDRESS    TO  SHELLTBL-ARG6
           MOVE    SYS-9001-PORT1      TO  SHELLTBL-ARG7
      *    ドクター
           MOVE    "@@@@@"             TO  SHELLTBL-ARG8
      *    保険組合せ
           MOVE    "@@@@"              TO  SHELLTBL-ARG9
      *    診療内容
           MOVE    "XX"                TO  SHELLTBL-ARG10
      *    連番
           ADD     1                   TO  WRK-RENNUM
           MOVE    WRK-RENNUM          TO  SHELLTBL-ARG11 (1:2)
      *    システム日付
           MOVE    SPA-SYSYMD          TO  SHELLTBL-ARG12
      *
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "shell"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       49061-CLAIM-SOUSINSYOSI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザプログラム起動処理
      *****************************************************************
       4907-USERPG-SEC                 SECTION.
      *
      *v4.8
      *    ginbee対応
      *    IF      SPA-MW              =   SPA-GINBEE
      *        GO      TO      4907-USERPG-EXT
      *    END-IF
      *
      *
           EVALUATE    FLG-USERPG
               WHEN    1
      *        追加
                   MOVE    1               TO  WRK-SYORI-FLG
               WHEN    2
               WHEN    4
      *        更新
                   MOVE    2               TO  WRK-SYORI-FLG
               WHEN    3
      *        削除
                   MOVE    3               TO  WRK-SYORI-FLG
           END-EVALUATE
      *
      *    ユーザプログラム起動処理
           INITIALIZE                      ORCSUSERCHKAREA
           MOVE    "P02"               TO  ORCSUSERCHK-GMNPG
           MOVE    "2"                 TO  ORCSUSERCHK-SYORI
           MOVE    SPA-SYSYMD          TO  ORCSUSERCHK-SYSYMD
           MOVE    SPA-SYSYMD          TO  ORCSUSERCHK-SRYYMD
           MOVE    SPA-PTID            TO  ORCSUSERCHK-PTID
           MOVE    WRK-PTNUM           TO  ORCSUSERCHK-PTNUM
           MOVE    SPACE               TO  ORCSUSERCHK-SRYKA
           MOVE    SPACE               TO  ORCSUSERCHK-HKNCOMBI
           MOVE    SPACE               TO  ORCSUSERCHK-DRCD
           MOVE    WRK-SYORI-FLG       TO  ORCSUSERCHK-SYORI-FLG
           CALL    "ORCSUSERCHK"       USING
                                       ORCSUSERCHKAREA
                                       SPA-AREA
      *
           .
       4907-USERPG-EXT.
           EXIT.
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
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-MAE-INPUT
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
      *H29.10
              OR  (KANACHK-RC4     NOT =   ZERO  )
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           ELSE
               MOVE    ZERO                TO  FLG-ERR
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
      *H29.10
              OR  (KANACHK2-RC5     NOT =   ZERO  )
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
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-MAE-INPUT
           IF     (KANACHK-RC      NOT =   ZERO  )
      *H29.10
              OR  (KANACHK-RC4     NOT =   ZERO  )
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT
           END-IF
      *
           .
       880-HALF-ZEN-EXT.
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
               MOVE    "患者番号未設定"
                                               TO  WRK-ERRMSG
           WHEN    "02"
               MOVE    "患者氏名未設定"
                                               TO  WRK-ERRMSG
           WHEN    "03"
               MOVE    "患者カナ氏名未設定"
                                               TO  WRK-ERRMSG
           WHEN    "04"
               MOVE    "生年月日未設定"
                                               TO  WRK-ERRMSG
           WHEN    "05"
               MOVE    "性別未設定"
                                               TO  WRK-ERRMSG
           WHEN    "10"
               MOVE    "該当患者番号なし"
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
               MOVE    "カナ氏名は全角で入力して下さい"
                                               TO  WRK-ERRMSG
           WHEN    "16"
               MOVE    "性別は１：男２：女で入力して下さい"
                                               TO  WRK-ERRMSG
           WHEN    "17"
               MOVE    "生年月日暦日エラー"
                                               TO  WRK-ERRMSG
           WHEN    "18"
               MOVE    "患者番号採番エラー"
                                               TO  WRK-ERRMSG
           WHEN    "20"
               MOVE    "郵便番号エラー"
                                               TO  WRK-ERRMSG
           WHEN    "21"
               MOVE    "勤務先郵便番号エラー"
                                               TO  WRK-ERRMSG
           WHEN    "22"
               MOVE    "自宅電話番号エラー"
                                               TO  WRK-ERRMSG
           WHEN    "23"
               MOVE    "連絡先電話番号エラー"
                                               TO  WRK-ERRMSG
           WHEN    "24"
               MOVE    "勤務先電話番号エラー"
                                               TO  WRK-ERRMSG
           WHEN    "25"
               MOVE    "携帯電話番号エラー"
                                               TO  WRK-ERRMSG
           WHEN    "26"
               MOVE    "ＦＡＸ番号エラー"
                                               TO  WRK-ERRMSG
           WHEN    "27"
               MOVE    "電子メールアドレスエラー"
                                               TO  WRK-ERRMSG
           WHEN    "28"
               MOVE    "職業エラー"
                                               TO  WRK-ERRMSG
           WHEN    "29"
               MOVE    "禁忌全角エラー"
                                               TO  WRK-ERRMSG
           WHEN    "30"
               MOVE    "アレルギー全角エラー"
                                               TO  WRK-ERRMSG
           WHEN    "31"
               MOVE    "感染症全角エラー"
                                               TO  WRK-ERRMSG
           WHEN    "32"
               MOVE    "記号全角エラー"
                                               TO  WRK-ERRMSG
           WHEN    "33"
               MOVE    "番号全角エラー"
                                               TO  WRK-ERRMSG
      *H30.4
           WHEN    "34"
               MOVE    "妊婦設定はできません。"
                                               TO  WRK-ERRMSG
      *H31.4
           WHEN    "35"
               MOVE    "コメント文字エラー"
                                               TO  WRK-ERRMSG
      *R01.6
           WHEN    "36"
               MOVE    "減免事由入力エラー"
                                               TO  WRK-ERRMSG
           WHEN    "37"
               MOVE    "割引率入力エラー"
                                               TO  WRK-ERRMSG
           WHEN    "38"
               MOVE    "入金方法入力エラー"
                                               TO  WRK-ERRMSG
      *
           WHEN    "39"
               MOVE    "労災・自賠責保険は登録できません。"
                                               TO  WRK-ERRMSG
      *保険エラー
           WHEN    "P1"
           WHEN    "H1"
           WHEN    "K2"
               PERFORM 8901-PTERRCD-SET-SEC
           WHEN    "P2"
               PERFORM 8902-PTPIDCD-SET-SEC
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
      *H30.4
           WHEN    "44"
               MOVE    "患者情報登録エラー"
                                               TO  WRK-ERRMSG
           WHEN    "45"
               MOVE    "患者個別情報登録エラー"
                                               TO  WRK-ERRMSG
      *更新
           WHEN    "50"
               STRING  "患者登録内容が一致しません。"
                       "更新できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "51"
               MOVE    "患者情報更新エラー"
                                               TO  WRK-ERRMSG
           WHEN    "52"
               MOVE    "旧姓履歴登録エラー"
                                               TO  WRK-ERRMSG
           WHEN    "53"
               MOVE    "更新区分エラー"
                                               TO  WRK-ERRMSG
           WHEN    "54"
               MOVE    "患者個別情報更新エラー"
                                               TO  WRK-ERRMSG
      *削除
           WHEN    "81"
               STRING  "患者登録内容が一致しません。"
                       "削除できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "82"
               STRING  "患者情報以外のデータが登録済みです。"
                       "削除できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "83"
               MOVE    "削除処理エラー"
                                               TO  WRK-ERRMSG
      *更新
           WHEN    "60"
               STRING  "患者登録内容が一致しません。"
                       "処理できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "61"
               MOVE    "保険・公費の設定がありません"
                                               TO  WRK-ERRMSG
           WHEN    "62"
               STRING  "同じ内容の保険が登録済みです。"
                       "処理できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "63"
               STRING  WRK-ERR-KOHNUM
                       " "               DELIMITED  BY  SIZE
                       WRK-ERR-KOHNUMMEI DELIMITED  BY  SPACE
                       " "               DELIMITED  BY  SIZE
                       "同じ内容の公費があります。"
                       "処理できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "64"
               MOVE    "保険が４０件以上となります。登録できません。"
                                               TO  WRK-ERRMSG
           WHEN    "65"
               MOVE    "公費が６０件以上となります。登録できません。"
                                               TO  WRK-ERRMSG
      *警告
           WHEN    "K1"
               STRING  "警告！自動採番の患者番号が最大値です。"
                       "初期番号から採番しました。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "K3"
               STRING  "警告！保険の種類を変更しました。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "K0"
               STRING  "警告！同一患者の登録があります。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "K4"
               STRING  "保険組合せが２０件以上存在します。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *H30.4
           WHEN    "K5"
               STRING  "警告！"
                       "１６歳未満です。"
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
           IF      WRK-ERR-KOHNUM  NOT =   SPACE
               MOVE    SPACE               TO  WRK-ERRMSG
               STRING    WRK-ERR-KOHNUM
                         " "               DELIMITED  BY  SIZE
                         WRK-ERR-KOHNUMMEI DELIMITED  BY  SPACE
                         " "               DELIMITED  BY  SIZE
                         SPA-ERRMSG        DELIMITED  BY  SPACE
                                       INTO    WRK-ERRMSG
               END-STRING
           END-IF
           .
        8901-PTERRCD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者登録確認メッセージ編集処理
      *****************************************************************
       8902-PTPIDCD-SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
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
      ********     STRING     "保険の期間が重複します。前の"
      *                                    DELIMITED  BY  SIZE
      *                       SPA-PIDMSG   DELIMITED  BY  SPACE
      *                       "を終了して下さい。"
      *                                    DELIMITED  BY  SIZE
      *                                INTO    WRK-ERRMSG
      *******      END-STRING
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
                   STRING    WRK-ERR-KOHNUM
                             " "               DELIMITED  BY  SIZE
                             WRK-ERR-KOHNUMMEI DELIMITED  BY  SPACE
                             " "               DELIMITED  BY  SIZE
                             "公費の期間が重複します。"
                                               DELIMITED  BY  SPACE
                                       INTO    WRK-ERRMSG
                   END-STRING
               WHEN    "0107"
                   STRING    WRK-ERR-KOHNUM
                             " "               DELIMITED  BY  SIZE
                             WRK-ERR-KOHNUMMEI DELIMITED  BY  SPACE
                             " "               DELIMITED  BY  SIZE
                             "保険番号マスタとの期間が違います。"
                                               DELIMITED  BY  SPACE
                                       INTO    WRK-ERRMSG
                   END-STRING
      *        Ｈ１５．４改正から
               WHEN    "0201"
                   STRING  "保険が終了しています。"
                           "終了日を設定して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                   END-STRING
               WHEN    "0202"
      *!!!         MOVE    "保険を切り替えます。よろしいですか？"
                   MOVE    "保険番号マスタとの期間が違います。"
                                               TO  WRK-ERRMSG
               WHEN    "0203"
                   STRING  "継続は平成１５年３月末で終了しています。"
      ***********          "終了日を変更します。よろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                   END-STRING
               WHEN    "0204"
      ********     STRING  "任継を切り替えます。"
      *                    "切り替え後の終了日を確認して下さい。"
      *                                        DELIMITED  BY  SIZE
      *                                        INTO    WRK-ERRMSG
      ********     END-STRING
                   MOVE    "任継の終了日が違います。"
                                               TO  WRK-ERRMSG
      *        Ｈ１８．１０改正から
               WHEN    "0110"
      *            STRING  SPA-PIDMSG    DELIMITED  BY  SPACE
      *                    "の前期高齢者２割を平成１８年１０月から"
      *                    "３割に変更します。"
      *                    "よろしいですか？"
      *                                DELIMITED  BY  SIZE
      *                                INTO  WRK-PIDMSG
      *            END-STRING
                   MOVE    "前期高齢者は２割ではありません。"
                                               TO  WRK-ERRMSG
      *        WHEN    "0111"
      *            STRING  "老人公費の２割を平成１８年１０月から"
      *                    "３割に変更します。"
      *                    "よろしいですか？"
      *                                DELIMITED  BY  SIZE
      *                                INTO  WRK-PIDMSG
      *            END-STRING
      *
               WHEN    "K910"
      *********    STRING  "警告！"
                   STRING  "保険組合せ更新で期間外の診療が"
                           "発生します。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
                   END-STRING
               WHEN    "K911"
     ************  STRING  "警告！"
                   STRING  "保険組合せ更新で期間外の病名が"
                           "発生します。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
                   END-STRING
               WHEN    "K912"
      ************ STRING  "警告！"
                   STRING  "保険組合せ更新で期間外の診療と病名が"
                           "発生します。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    "保険・公費がエラーです。"
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
      *    DISPLAY "mcp-window = "    MCP-WINDOW 
      *
           MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
           MOVE    "P02"               TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    1                   TO  SLOCK-MODE
           MOVE    PTINF-PTID          TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               DISPLAY "排他エラー = " PTINF-PTID
               DISPLAY "TERMID     = " SPA-TERMID
               DISPLAY "RC         = " SLOCK-RETURN
           END-IF
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
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
      *    MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
      *    MOVE    "U02"               TO  SPA-MOTOPG
      *                                    MCP-WINDOW
           MOVE    ZERO                TO  SLOCK-PTID
           MOVE    3                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
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
      *    保険者番号マスタ次検索処理
      *****************************************************************
       900-HKNJAINF-READ-SEC              SECTION.
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               MOVE    ZERO                TO  FLG-HKNJAINF
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1                   TO  FLG-HKNJAINF
           END-IF
           .
       900-HKNJAINF-READ-EXT.
           EXIT.
      *
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
      *
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
