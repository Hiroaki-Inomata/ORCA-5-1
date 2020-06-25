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
       PROGRAM-ID.         ORAPI021S1V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 診療行為　診察料返却　（入力一体化）
      *  管理者            :
      *  作成日付    作業者        記述
      *  15/02/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.00    NACL-多々納  17/08/XX  リクエスト＝００追加
      *  05.00.00    NACL-多々納  18/08/XX  処方箋印刷独立機能対応
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
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-WKSRYACT            PIC 9(01).
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-HKNNUM              PIC 9(01).
           03  FLG-SPATMP              PIC 9(01).
           03  FLG-PARA                PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-SYOERR              PIC 9(01).
           03  FLG-TASRYKA             PIC 9(01).
           03  FLG-TAHOKEN             PIC 9(01).
           03  FLG-HKN-ARI             PIC 9(01).
           03  FLG-UIDOK               PIC 9(01).
           03  FLG-NYUINDAY            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
      *    03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDY2                PIC 9(04).
      *
           03  IDM1                PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDX-ZAI             PIC 9(04).
           03  IDX-ZAI2            PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX-HKN             PIC 9(04).
           03  IDX-ERR             PIC 9(04).
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
      *    保険組合せ
           03  WRK-HKNCOMBI            PIC X(04).
      *    エラーコード
           03  WRK-ERRCD-G.
               05  WRK-ERRKBN              PIC X(01).
               05  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-ERRCD-1             PIC X(02).
           03  WRK-ERRMSG-1            PIC X(100).
           03  WRK-WERRCD              PIC X(03).
           03  WRK-KEICD-G.
               05  WRK-KEICD-OCC       OCCURS   20.
                   07  WRK-KEI-SRYCD       PIC X(09).
                   07  WRK-KEICD           PIC X(03).
                   07  WRK-KEIMSG          PIC X(100).
      *
           03  WRK-MCP-WINDOW          PIC X(64).
      *
           03  WRK-OPID                PIC X(16).
           03  WRK-ORCA-UID            PIC X(64).
      *    MCP-TERMID
           03  WRK-MCP-TERM            PIC X(64).
      *
      *    保険組合せ内容
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-RSI-HKNKBN          PIC X(01).
      *
           03  WRK-SINSATU             PIC X(01).
      *
           03  WRK-SRYSYUKBN           PIC X(03).
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-SRYSYUKBNMEI        PIC X(40).
           03  WRK-SRYCD               PIC X(09).
           03  WRK-TIMEFLG             PIC X(01).
      *
       01  WRK-XML-AREA.
           03  WRK-PERFORM-DATE        PIC X(10).
           03  WRK-PERFORM-TIME8       PIC X(08).
      *
           03  WRK-PERFORM-YMD         PIC X(08).
           03  WRK-PERFORM-TIME.
               05  WRK-PERFORM-THH     PIC X(02).
               05  WRK-PERFORM-TMM     PIC X(02).
               05  WRK-PERFORM-TSS     PIC X(02).
           03  WRK-PTNUM               PIC X(20).
           03  WRK-PTID                PIC 9(10).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-NAME          PIC X(80).
           03  WRK-DRCD                PIC X(05).
           03  WRK-DRCD-NAME           PIC X(80).
      *
      *    入院日
           03  WRK-NYUIN-DATE          PIC X(10).
           03  WRK-NYUIN-YMD           PIC X(08).
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(100).
           03  WRK-ATO-INPUT           PIC X(100).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
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
      *
               05  WRK-SELECT-CODE         PIC X(04).
      *        処理区分
               05  WRK-PARA-MODE           PIC X(03).
      *
      *処理最大行
       01  MAX-LINE-VALUE          PIC 9(04)   VALUE   400.
      *
      *    全業務解除
       01  CONS-ORCA-UID       PIC X(64)   VALUE
                   "267c5110-71b6-431e-baac-0989ab98a505".
      *
      *
      *    ワーク診療行為マスタ−
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC          OCCURS   400.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
      *    附加情報を追加
           03  LNK-WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC"  REPLACING
                                     //WKSRYP-// BY //LNK-WKSRYP-//.
      *    API用剤削除連番
           03  LNK-WKSRYAPI-RENNUM         PIC 9(02).
      *    剤削除区分
           03  LNK-WKSRYAPI-DELKBN         PIC 9(01).
      *
       01  LNK-WKSRYACT-MAX           PIC 9(04).
      *
      *    診療内容編集サブ領域
           COPY    "CPORAPI021SUB2V3.INC".
      *
      *    処理終了パラメタ
           COPY    "CPORAPIV3SUB01.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
      *****COPY    "CPORCSPTNUM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    初期ＳＰＡ画面編集パラメタ
           COPY    "CPORCSC95.INC".
      *    初期ＳＰＡ画面編集パラメタ
           COPY    "CPORCSC92.INC".
      *    パラメタ
           COPY    "CPORCSC10S.INC".
      *    診療行為内容編集・計算
           COPY    "CPORCSC100.INC".
      *
      *    エラーメッセージ編集パラメタ
           COPY    "CPORCSC96.INC".
      *
      *    ＳＰＡ画面パラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "K02SCR-SPA".
      *
      *
      *    API XML読み込み用定義体
           COPY    "CPMDCXMLV3REQ1.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPMDCXMLV3RES1.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
      *    保険年齢・負担割合算定サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *    ＵＩＤ取得用エリア
       01  UIDIO-AREA.
           03  C-RET               PIC X(2).
           03  C-UID               PIC X(36).
       01  ST                      PIC 9(2).
      *
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
      *    診療科情報
           COPY    "CPSK1005.INC".
      *    職員情報
           COPY    "CPSK1010.INC".
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    ＳＰＡ一時テーブル
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
      *     パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *    APIパラメタテーブル
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPDATA2.INC".
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
           COPY    "CPAPILSTV2.INC".
      *V3  一体化
           COPY    "API21V3SCRAREA.INC".
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
           DISPLAY   "* medicalreqv3 start*"
           DISPLAY   "*********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
           INITIALIZE                      CNT-AREA
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
               PERFORM 300-APTXMLMAKE-SEC
           END-IF
      *
           MOVE    WRK-MCP-TERM        TO  MCP-TERM
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
      *
           DISPLAY   "*********************"
           DISPLAY   "* medicalreqv3 end  *"
           DISPLAY   "*********************"
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
           INITIALIZE                      XML-MEDICALV3RES1
           MOVE    SPACE               TO  WRK-APIV3DATA-AREA
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Medical Info"       TO  MDCRES-RESKEY
      *
           STRING  "A+"
                   MCP-USER            DELIMITED  BY  SIZE
                                       INTO    WRK-OPID
           END-STRING
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MDCRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  MDCRES-INFORMATION-TIME
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
      *    XML情報取り出し
           PERFORM 1000-APTXMLREAD-SEC
      *
      *    ＳＰＡ共通設定
           IF      WRK-ERRCD           =   SPACE
               PERFORM 1001-ORCSCOMMON-SEC
           END-IF
      *
      *    テーブル最大行
           MOVE    MAX-LINE-VALUE      TO  SPA-MAX-LINE
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡ共通設定処理
      *****************************************************************
       1001-ORCSCOMMON-SEC           SECTION.
      *
      *    診療日をシステム日付へ
           IF      MDCREQ-PERFORM-DATE NOT =   SPACE
               MOVE    MDCREQ-PERFORM-DATE TO  WRK-HEN-DATE
               MOVE    SPACE               TO  WRK-HEN-TIME
               PERFORM 802-DAYHEN02-SEC
               MOVE    WRK-SYMD            TO  WRK-PERFORM-YMD
               PERFORM 20011-PERFORM-CHK-SEC
      *        診療日をシステム日付へ
               IF      WRK-ERRCD           =   SPACE
                   MOVE    WRK-PERFORM-YMD     TO  SPA-SYSYMD
               END-IF
               MOVE    SPACE               TO  WRK-ERRCD
           END-IF
      *
           MOVE    "API"               TO  SPA-MOTOPG
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
           MOVE    SPACE               TO  SPA-MOTOPG
           .
       1001-ORCSCOMMON-EXT.
           EXIT.
      *
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
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *
           MOVE    ZERO                TO  MDCRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD-G
           MOVE    SPACE               TO  WRK-WERRCD
           MOVE    SPACE               TO  WRK-KEICD-G
           MOVE    ZERO                TO  FLG-UIDOK
      *
      *    全業務解除
           IF     (MDCREQ-ORCA-UID         =   CONS-ORCA-UID)
            AND   (MDCREQ-REQUEST-NUMBER   =   "99"      )
               MOVE    MDCREQ-ORCA-UID     TO  SPA-TERMID
           ELSE
      *!!
      *    UIDチェック
           IF      MDCREQ-ORCA-UID NOT =   SPACE
            AND   (MDCREQ-REQUEST-NUMBER   NOT =   "00" )
               PERFORM 2009-PARA-INIT-SEC
           END-IF
      *
           END-IF
      *
           MOVE    "01"                TO  MDCRES-RESPONSE-NUMBER
      *
           MOVE    MDCREQ-REQUEST-NUMBER   TO  MDCRES-REQUEST-NUMBER
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    MDCREQ-REQUEST-NUMBER
           WHEN    "01"
      *        診察料返却
               MOVE    1                   TO  FLG-SYORI
      *H29.8
           WHEN    "00"
      *        API 保険組合せ等返却
               MOVE    9                   TO  FLG-SYORI
               PERFORM 400-SYORI-00-SEC
               GO      TO      200-MAIN-EXT
      *
      *ロック解除
           WHEN    "99"
      *        強制処理終了
               PERFORM 2099-CANCEL-SEC
               GO      TO      200-MAIN-EXT
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
           END-EVALUATE
      *
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           ELSE
      *        編集処理
               PERFORM 20010-INPUT-HEN-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    SPACE               TO  WRK-KEICD-G
      *        API処理登録
               IF     (WRK-ORCA-UID    NOT =   SPACE )
                 AND  (FLG-UIDOK           =   1     )
                   PERFORM 2008-APIPARAINS-SEC
               ELSE
                   MOVE    SPACE           TO  MDCRES-ORCA-UID
               END-IF
               GO      TO      200-MAIN-EXT
           END-IF
      *
           EVALUATE    FLG-SYORI
           WHEN    1
      *        診察料返却処理
               PERFORM 2002-SYORI-01-SEC
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
           MOVE    2                   TO  FLG-SYORI
      *
           IF      MDCREQ-PATIENTID    =   SPACE
               MOVE    SPACE               TO  WRK-PTNUM
           ELSE
               MOVE    MDCREQ-PATIENTID    TO  WRK-PTNUM
               INITIALIZE                  ORCSPTIDAREA
               MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
               MOVE    WRK-PTNUM           TO  SPTID-PTNUM
               CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                   SPA-AREA
               IF      SPTID-RC            =   ZERO
                   MOVE    SPTID-PTNUM         TO  WRK-PTNUM
               END-IF
           END-IF
      *
           INITIALIZE                  ORAPIV3SUB01AREA
           MOVE    "9"                 TO  APIV3SUB01-SYORI
           IF      MDCREQ-ORCA-UID     NOT =   SPACE
               MOVE    SPA-TERMID          TO  APIV3SUB01-TERMID
           END-IF
           MOVE    MDCREQ-KARTE-UID    TO  APIV3SUB01-KARTE-UID
           MOVE    "AP21"              TO  APIV3SUB01-API-GYOUMUID
           MOVE    "API21V3"           TO  APIV3SUB01-API-FILEMEI
           MOVE    WRK-PTNUM           TO  APIV3SUB01-PTNUM
           MOVE    "K02"               TO  APIV3SUB01-PARA-GYOUMUID
           CALL    "ORAPIV3SUB01"      USING
                                       MCPAREA
                                       SPA-AREA
                                       ORAPIV3SUB01AREA
           IF      APIV3SUB01-ERRCD    NOT =   SPACE
               MOVE    "60"                TO  WRK-ERRCD
               MOVE    APIV3SUB01-ERRMSG   TO  WRK-ERRMSG
           END-IF
      *    編集処理
           PERFORM 20010-INPUT-HEN-SEC
           MOVE    SPACE               TO  WRK-KEICD-G
           .
       2099-CANCEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-APTXMLREAD-SEC   SECTION.
      *
           IF      APILST03-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-MEDICALV3REQ1
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalv3req1" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST03-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalv3req1"     TO  APIREQ-RECORDNAME
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
           MOVE    SPACE               TO  APIREQ-PATIENTINFOREQ
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_medicalv3req1" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST03-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalv3req1"     TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-MEDICALV3REQ1
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE           TO  APILST03-BODY
           .
       1000-APTXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-APTXMLMAKE-SEC   SECTION.
      *
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "処理終了"
                                           TO  MDCRES-API-RESULT-MSG
                   WHEN    2
                       MOVE    "解除処理終了"
                                           TO  MDCRES-API-RESULT-MSG
                   WHEN    9
                       MOVE    "処理終了"
                                           TO  MDCRES-API-RESULT-MSG
               END-EVALUATE
      *        警告あり
               IF      WRK-KEICD (1)   NOT =   SPACE
                   MOVE    "W00"           TO  MDCRES-API-RESULT
               END-IF
           ELSE
               IF      WRK-ERRMSG          =   SPACE
      *        エラー・警告メッセージ
                   PERFORM 890-ERRCD-MSG-SEC
               END-IF
               IF      WRK-ERRKBN          =   SPACE
                   MOVE    "E"                 TO  WRK-ERRKBN
               END-IF
               MOVE    WRK-ERRCD-G         TO  MDCRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  MDCRES-API-RESULT-MSG
           END-IF
      *
      *    ワーニング編集
           PERFORM 3001-WAINGMSG-HEN-SEC
      *
      *
           IF      APILST03-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalv3res1" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalv3res1"     TO  MDCRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALV3RES1
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_medicalv3res1" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalv3res1"     TO  MDCRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALV3RES1
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE  MDCRES-OBJECT     TO  APILST03-BODY 
               MOVE  "application/xml" TO  APILST03-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       300-APTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    各エラー、ワーニング編集
      *****************************************************************
       3001-WAINGMSG-HEN-SEC        SECTION.
      *
           IF      WRK-ERRCD-1      NOT =   SPACE
               MOVE    SPACE               TO  MDCRES-MEDI-RESULT
               STRING  "E"
                       WRK-ERRCD-1     DELIMITED  BY  SIZE
                                       INTO    MDCRES-MEDI-RESULT
               END-STRING
               MOVE    WRK-ERRMSG-1        TO  MDCRES-MEDI-RESULT-MSG
           END-IF
           MOVE    ZERO                TO  IDM1
           PERFORM VARYING     IDK     FROM    1   BY  1
                   UNTIL      (IDK     >   10   )   OR
                              (WRK-KEICD (IDK) =   SPACE)
               MOVE    WRK-KEICD (IDK)     TO  WRK-WERRCD
               IF      WRK-KEIMSG(IDK)     =   SPACE
      *            エラー・警告メッセージ
                   PERFORM 891-WARNING-MSG-SEC
                   MOVE    WRK-ERRMSG          TO  WRK-KEIMSG(IDK)
               END-IF
               ADD     1                   TO  IDM1
               MOVE    WRK-KEIMSG(IDK)     TO  MDCRES-MEDI-WARNING-MSG
                                                               (IDM1)
               MOVE    WRK-WERRCD          TO  MDCRES-MEDI-WARNING
                                                               (IDM1)
               MOVE    WRK-KEI-SRYCD(IDK)  TO  MDCRES-MEDI-CODE (IDM1)
           END-PERFORM
      *
           .
       3001-WAINGMSG-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
      *    IF      MDCREQ-OUTPATIENT-CLASS    =   LOW-VALUE
      *        MOVE  SPACE             TO  MDCREQ-OUTPATIENT-CLASS
      *    END-IF
           IF      MDCREQ-PATIENTID    =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-PATIENTID
           END-IF
           IF     MDCREQ-PERFORM-DATE  =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-PERFORM-DATE
           END-IF
           IF     MDCREQ-PERFORM-TIME  =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-PERFORM-TIME
           END-IF
           IF      MDCREQ-APPOINT-DEP-CODE =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-APPOINT-DEP-CODE
           END-IF
           IF      MDCREQ-APPOINT-DR-CODE  =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-APPOINT-DR-CODE
           END-IF
           IF  MDCREQ-MAIN-INSURANCE-CLASS  =   LOW-VALUE
               MOVE  SPACE             TO  
                                       MDCREQ-MAIN-INSURANCE-CLASS
           END-IF
           IF  MDCREQ-MAIN-INSURANCE-NUMBER =   LOW-VALUE
               MOVE  SPACE             TO  
                                       MDCREQ-MAIN-INSURANCE-NUMBER
           END-IF
           IF  MDCREQ-MAIN-INSURANCE-NAME   =   LOW-VALUE
               MOVE  SPACE             TO  
                                       MDCREQ-MAIN-INSURANCE-NAME
           END-IF
           IF  MDCREQ-MAIN-MARK      =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-MARK
           END-IF
           IF  MDCREQ-MAIN-NUMBER    =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-NUMBER
           END-IF
           IF  MDCREQ-MAIN-CONTINUATION     =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-CONTINUATION
           END-IF
           IF  MDCREQ-MAIN-ASSISTANCE =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-ASSISTANCE
           END-IF
           IF  MDCREQ-MAIN-FAMILY     =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-FAMILY
           END-IF
           IF  MDCREQ-MAIN-POLICYHOLDER     =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-POLICYHOLDER
           END-IF
           IF  MDCREQ-MAIN-START-DATE =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-START-DATE
           END-IF
           IF  MDCREQ-MAIN-END-DATE   =   LOW-VALUE
               MOVE  SPACE             TO  MDCREQ-MAIN-END-DATE
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
               IF      MDCREQ-INSURANCE-CLASS(IDX)  =   LOW-VALUE
                   MOVE  SPACE             TO  MDCREQ-INSURANCE-CLASS
                                                                  (IDX)
               END-IF
               IF      MDCREQ-INSURANCE-NAME(IDX)    =   LOW-VALUE
                   MOVE  SPACE             TO  
                                       MDCREQ-INSURANCE-NAME(IDX)
               END-IF
               IF      MDCREQ-PROVIDER-NUMBER(IDX)   =   LOW-VALUE
                   MOVE  SPACE             TO  
                                       MDCREQ-PROVIDER-NUMBER(IDX)
               END-IF
               IF      MDCREQ-RECIPIENT-NUMBER(IDX)  =   LOW-VALUE
                   MOVE  SPACE             TO  
                                       MDCREQ-RECIPIENT-NUMBER(IDX)
               END-IF
               IF      MDCREQ-INSURANCE-START-DATE(IDX) =   LOW-VALUE
                   MOVE    SPACE      TO   MDCREQ-INSURANCE-START-DATE
                                                                 (IDX)
               END-IF
               IF      MDCREQ-INSURANCE-END-DATE(IDX)   =   LOW-VALUE
                   MOVE    SPACE       TO  MDCREQ-INSURANCE-END-DATE
                                                                (IDX)
               END-IF
           END-PERFORM 
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
      *    必須チェック
           PERFORM 20010-INIT-CHECK-SEC
      *
      *    診療日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PERFORM-CHK-SEC
           END-IF
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PTNUM-CHK-SEC
      *        排他制御処理
               IF      WRK-ERRCD           =   SPACE
                   PERFORM 20019-LOCK-SYORI-SEC
                   IF      WRK-ERRCD           =   SPACE
      *                パラメタ登録
                       MOVE    1               TO  FLG-UIDOK
                   END-IF
               END-IF
           END-IF
      *
      *    診療科コードチェック
           IF     (WRK-SRYKA       NOT =   SPACE  )
           AND    (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-SRYKA-CHK-SEC
           END-IF
      *    ドクターコードチェック
           IF     (WRK-DRCD        NOT =   SPACE  )
           AND    (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-DRCD-CHK-SEC
           END-IF
      *
      *    時間外区分チェック
           IF     (MDCREQ-OFFTIME  NOT =   SPACE  )
           AND    (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-OFFTIME-CHK-SEC
           END-IF
      *
      *    診察料区分チェック
           IF     (MDCREQ-DOCTORS-FEE  NOT =   SPACE  )
           AND    (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-DOCTORS-CHK-SEC
           END-IF
      *
      *    処理区分チェック
           IF     (MDCREQ-DOCTORS-FEE  NOT =   SPACE  )
           AND    (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-PATIENT-MODE-CHK-SEC
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
           MOVE    MDCREQ-PATIENTID        TO  MDCRES-PATIENTID
           MOVE    MDCREQ-PATIENTID        TO  WRK-PTNUM
      *    診療科
           MOVE    MDCREQ-APPOINT-DEP-CODE
                                           TO  MDCRES-APPOINT-DEP-CODE
           MOVE    MDCREQ-APPOINT-DEP-CODE
                                           TO  WRK-SRYKA
      *    ドクター
           MOVE    MDCREQ-APPOINT-DR-CODE
                                           TO  MDCRES-APPOINT-DR-CODE
           MOVE    MDCREQ-APPOINT-DR-CODE
                                           TO  WRK-DRCD
      *    診療日・時間
           MOVE    MDCREQ-PERFORM-DATE     TO  MDCRES-PERFORM-DATE
           MOVE    MDCREQ-PERFORM-TIME     TO  MDCRES-PERFORM-TIME
      *    数字のみに編集
           MOVE    MDCREQ-PERFORM-DATE     TO  WRK-HEN-DATE
           MOVE    MDCREQ-PERFORM-TIME     TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-PERFORM-YMD
           MOVE    WRK-THMS                TO  WRK-PERFORM-TIME
      *    未設定時はシステム情報設定
           IF      WRK-PERFORM-YMD         =   SPACE
               MOVE    SMCNDATE-YMD        TO  WRK-PERFORM-YMD
               MOVE    MDCRES-INFORMATION-DATE
                                           TO  MDCRES-PERFORM-DATE
               ADD     1                   TO  IDK
               MOVE    "W01"               TO  WRK-KEICD(IDK)
           END-IF
      *    診療日をシステム日付・診療日付へ
           MOVE    WRK-PERFORM-YMD     TO  SPA-SYSYMD
      *
           MOVE    WRK-PERFORM-YMD     TO  SPA-SRYYMD
      *    入外区分（外来）
           MOVE    "2"                 TO  SPA-NYUGAIKBN
      *H29.5
      *    HAORI処理
           MOVE    1                   TO  SPA-API-FLG
      *
      *    入力UID
           MOVE    MDCREQ-ORCA-UID     TO  MDCRES-ORCA-UID
           MOVE    MDCREQ-KARTE-UID    TO  MDCRES-KARTE-UID
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
      *    診療科
           IF      WRK-SRYKA           =   SPACE
               IF     (WRK-ERRCD           =   SPACE  )
                   MOVE    "02"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    UID（電子カルテ）
           IF      MDCREQ-KARTE-UID    =   SPACE
               MOVE    "04"                TO  WRK-ERRCD
           END-IF
      *    電子カルテＵＩＤチェック
           IF      WRK-ERRCD           =   SPACE
               IF     (WRK-KARTE-UID       NOT =   SPACE  )
                 AND  (MDCREQ-KARTE-UID    NOT =   WRK-KARTE-UID )
      *            他端末使用中
                   MOVE    "90"            TO  WRK-ERRCD
                   MOVE    ZERO            TO  FLG-UIDOK
               END-IF
           END-IF
      *
           .
       20010-INIT-CHECK-EXT.
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
      *    H27.4.1からとする
           IF      WRK-PERFORM-YMD     <   "20150401"
               MOVE    "12"                TO  WRK-ERRCD
           END-IF
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
           MOVE    SPTID-PTNUM         TO  MDCRES-PATIENTID
           MOVE    SPTID-PTNUM         TO  WRK-PTNUM
           MOVE    SPTID-PTID          TO  WRK-PTID
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
           MOVE    SPA-SRYYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1005-EDYUKYMD
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
      *    ドクタコードチェック
      *****************************************************************
       20011-DRCD-CHK-SEC          SECTION.
      *
      *    ドクタの存在チェック
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WRK-DRCD            TO  SYS-1010-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
               MOVE    SYS-1010-NAME       TO  WRK-DRCD-NAME
           ELSE
               MOVE    "14"                TO  WRK-ERRCD
           END-IF
      *
           IF      WRK-DRCD (1:1)  NOT =   "1"
               MOVE    "14"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-DRCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間外区分チェック処理
      *****************************************************************
       20011-OFFTIME-CHK-SEC        SECTION.
      *
           IF      MDCREQ-OFFTIME      =   SPACE
               CONTINUE
           ELSE
               IF      MDCREQ-OFFTIME      >=  "0"  AND
                                           <=  "8"
                   CONTINUE
               ELSE
                   MOVE    "18"                TO  WRK-ERRCD
               END-IF
           END-IF
           .
       20011-OFFTIME-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    診察料区分チェック処理
      *****************************************************************
       20011-DOCTORS-CHK-SEC        SECTION.
      *
           EVALUATE    MDCREQ-DOCTORS-FEE
               WHEN    "01"
      *            初診
                   MOVE    "1"                 TO  WRK-SINSATU
               WHEN    "02"
      *            再診
                   MOVE    "2"                 TO  WRK-SINSATU
               WHEN    "03"
      *            電話再診
                   MOVE    "T"                 TO  WRK-SINSATU
      *保留    WHEN    "10"
      *            指定なし
      *!!          MOVE    "A"                 TO  WRK-SINSATU
               WHEN    "09"
      *            診察料なし
                   MOVE    "9"                 TO  WRK-SINSATU
               WHEN    OTHER
                   MOVE    SPACE               TO  WRK-SINSATU
                   MOVE    "19"                TO  WRK-ERRCD
           END-EVALUATE
           .
       20011-DOCTORS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理区分チェック処理
      *****************************************************************
       20011-PATIENT-MODE-CHK-SEC        SECTION.
      *
      *    印刷のみ
           EVALUATE    MDCREQ-PATIENT-MODE
               WHEN    "Print"
               WHEN    SPACE
                   CONTINUE
               WHEN    OTHER
                   MOVE    "26"                TO  WRK-ERRCD
           END-EVALUATE
           .
       20011-PATIENT-MODE-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御処理処理
      *****************************************************************
       20019-LOCK-SYORI-SEC        SECTION.
      *
      *    ＵＩＤコード取得処理
           PERFORM 20020-ORCAUID-SYORI-SEC
      *
      *    排他チェック & ロック処理
           PERFORM 990-LOCK-IN-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "90"                TO  WRK-ERRCD
               MOVE    SPACE               TO  WRK-KEICD-G
           ELSE
               MOVE    WRK-ORCA-UID        TO  MDCRES-ORCA-UID
           END-IF
      *
           .
       20019-LOCK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＵＩＤコード取得処理
      *****************************************************************
       20020-ORCAUID-SYORI-SEC        SECTION.
      *
      *    UIDコード処理
           IF      MDCREQ-ORCA-UID     =   SPACE
      *        UIDコード取得処理
               PERFORM 1001-UID-GET-SEC
               MOVE    C-UID               TO  WRK-ORCA-UID
           ELSE
               MOVE    MDCREQ-ORCA-UID     TO  WRK-ORCA-UID
           END-IF
      *    UIDコードをTERMID
           MOVE    SPACE               TO  SPA-TERMID
           STRING  "API+"
                   WRK-ORCA-UID        DELIMITED  BY  SPACE
                                           INTO    SPA-TERMID
           END-STRING
           .
       20020-ORCAUID-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診察料返却処理
      *****************************************************************
       2002-SYORI-01-SEC        SECTION.
      *
      *    患者情報内容編集
           PERFORM 20021-MDCRES-HEN-SEC
      *
      *    診察料登録処理
           PERFORM 20021-HENSYU-01-SEC
      *
      *    返却レスポンス番号
           IF     (WRK-ERRCD           =   SPACE )
             AND  (WRK-ERRCD-1         =   SPACE )
      *        エラーなしは次へ
               MOVE    "02"            TO  MDCRES-RESPONSE-NUMBER
      *
      *        SPAの登録
               PERFORM 2008-SPATMP-INS-SEC
           END-IF
      *    API処理登録
           PERFORM 2008-APIPARAINS-SEC
      *
           .
       2002-SYORI-01-EXT.
           EXIT.
      *****************************************************************
      *    患者情報出力情報編集処理
      *****************************************************************
       20021-MDCRES-HEN-SEC     SECTION.
      *
           MOVE    PTINF-NAME          TO  MDCRES-NAME
           MOVE    PTINF-KANANAME      TO  MDCRES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MDCRES-BIRTHDAY
           MOVE    PTINF-SEX           TO  MDCRES-SEX
      *
           MOVE    WRK-SRYKA-NAME      TO  MDCRES-APPOINT-DEP-NAME
           MOVE    WRK-DRCD-NAME       TO  MDCRES-APPOINT-DR-NAME
      *
           MOVE    WRK-ORCA-UID        TO  MDCRES-ORCA-UID
      *
      *    保険情報
           PERFORM 200211-HKNINF-HEN-SEC
      *
           .
       20021-MDCRES-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険入力情報編集処理
      *****************************************************************
       200211-HKNINF-HEN-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-HKN-ARI
      *
      *    保険組合せ番号
           MOVE    MDCREQ-COMBINATION-NUMBER
                                       TO  MDCRES-COMBINATION-NUMBER
      *    保険種類
           MOVE    MDCREQ-MAIN-INSURANCE-CLASS 
                                       TO  MDCRES-MAIN-INSURANCE-CLASS
      *    保険者番号
           MOVE    MDCREQ-MAIN-INSURANCE-NUMBER
                                       TO  MDCRES-MAIN-INSURANCE-NUMBER
      *    保険の名称
           MOVE    MDCREQ-MAIN-INSURANCE-NAME
                                       TO  MDCRES-MAIN-INSURANCE-NAME
      *    記号
           MOVE    MDCREQ-MAIN-MARK    TO  MDCRES-MAIN-MARK
      *    番号
           MOVE    MDCREQ-MAIN-NUMBER  TO  MDCRES-MAIN-NUMBER
      *    継続区分
           MOVE    MDCREQ-MAIN-CONTINUATION
                                       TO  MDCRES-MAIN-CONTINUATION
           MOVE    MDCREQ-MAIN-ASSISTANCE
                                       TO  MDCRES-MAIN-ASSISTANCE
           MOVE    MDCREQ-MAIN-FAMILY  TO  MDCRES-MAIN-FAMILY
           MOVE    MDCREQ-MAIN-POLICYHOLDER
                                       TO  MDCRES-MAIN-POLICYHOLDER
           MOVE    MDCREQ-MAIN-START-DATE
                                       TO  MDCRES-MAIN-START-DATE
           MOVE    MDCREQ-MAIN-END-DATE
                                       TO  MDCRES-MAIN-END-DATE
      *    公費情報
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
      *        公費種類
               MOVE    MDCREQ-INSURANCE-CLASS (IDX)
                                       TO  MDCRES-INSURANCE-CLASS(IDX)
      *        公費名称
               MOVE    MDCREQ-INSURANCE-NAME  (IDX)
                                       TO  MDCRES-INSURANCE-NAME (IDX)
      *        負担者番号
               MOVE    MDCREQ-PROVIDER-NUMBER(IDX)
                                       TO  MDCRES-PROVIDER-NUMBER(IDX)
      *        受給者番号
               MOVE    MDCREQ-RECIPIENT-NUMBER (IDX)
                                       TO  MDCRES-RECIPIENT-NUMBER(IDX)
      *        有効開始日
               MOVE    MDCREQ-INSURANCE-START-DATE (IDX)
                                       TO  MDCRES-INSURANCE-START-DATE
                                                                (IDX)
      *        有効終了日
               MOVE    MDCREQ-INSURANCE-END-DATE (IDX)
                                       TO  MDCRES-INSURANCE-END-DATE
                                                                (IDX)
               IF     (MDCREQ-INSURANCE-CLASS (IDX)
                                           =   SPACE )
                AND   (MDCREQ-INSURANCE-NAME  (IDX)
                                           =   SPACE )
                AND   (MDCREQ-PROVIDER-NUMBER (IDX)
                                           =   SPACE )
                AND   (MDCREQ-RECIPIENT-NUMBER (IDX)
                                           =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-HKN-ARI
               END-IF
           END-PERFORM
      *
           IF      (MDCREQ-COMBINATION-NUMBER  =   SPACE)
               AND (MDCREQ-MAIN-INSURANCE-CLASS    =   SPACE)
               AND (MDCREQ-MAIN-INSURANCE-NUMBER   =   SPACE)
               AND (MDCREQ-MAIN-INSURANCE-NAME     =   SPACE)
               AND (FLG-HKN-ARI                =   ZERO  )
               MOVE    ZERO                TO  FLG-HKN-ARI
           ELSE
               MOVE    1                   TO  FLG-HKN-ARI
           END-IF
           .
       200211-HKNINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為登録処理
      *****************************************************************
       20021-HENSYU-01-SEC        SECTION.
      *
           INITIALIZE                  SPA-K01KYOUTU
           INITIALIZE                  SPA-K02
      *
           IF      (FLG-HKN-ARI        =   ZERO)
      *        保険設定なし
               MOVE    ZERO                TO  WRK-HKNCOMBI
               IF     (WRK-SINSATU     NOT =   "9" )
                   MOVE    "15"            TO  WRK-ERRCD
                   MOVE    WRK-HKNCOMBI    TO  MDCRES-COMBINATION-NUMBER
               END-IF
           ELSE
      *        保険組合せの決定
               PERFORM 200215-HKNCOMBI-SET-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      20021-HENSYU-01-EXT
           END-IF
      *
      *    ＳＰＡ初期設定
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    "1"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
      *    SPA患者情報
           MOVE    WRK-PTID            TO  SPA-PTID
           MOVE    WRK-PTNUM           TO  SPA-PTNUM
           MOVE    WRK-SRYKA           TO  SPA-SRYKA
           MOVE    WRK-HKNCOMBI        TO  SPA-HKNCOMBINUM
      *
      *    APIの患者決定
           INITIALIZE                  ORCSC95AREA
           MOVE    "@"                 TO  LNK-SC95-KBN
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    "K10"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
           MOVE    SPA-BIRTHDAY        TO  SPA-NAI-BIRTHDAY
           MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
      *
      *    保険組合せ判定
           IF     (SPA-HKN-ERR     NOT =   ZERO )  OR
                  (SPA-RJN-ERR     NOT =   ZERO )
               PERFORM 20021-HKN-CHEK-SEC
           END-IF
           IF     (LNK-SC95-LAST-RENNUM    =   9     )
             AND  (WRK-ERRCD               =   SPACE )
               MOVE    "0220"              TO  SPA-ERRCD
               PERFORM 200232-ERR-HEN-SEC
               MOVE    "31"            TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      20021-HENSYU-01-EXT
           END-IF
      *
      *    年齢計算
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
           MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
           MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
      **** MOVE    SPA-HKNCOMBINUM     TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           MOVE    AGECHK-NENREI2-YY   TO  SPA-NENREI-YY
           MOVE    AGECHK-NENREI2-MM   TO  SPA-NENREI-MM
           MOVE    AGECHK-NENREI2-DD   TO  SPA-NENREI-DD
      *H28.4
      *    未就学前
           MOVE    AGECHK-SYUGAKUKBN   TO  SPA-SYUGAKUKBN
      *
      *    入外区分判定
           PERFORM 200211-NYUGAIKBN-CHK-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      20021-HENSYU-01-EXT
           END-IF
      *
      *    中途データ有無判定
           PERFORM 200212-WKSRYACT-CHK-SEC
      *
           IF     (SPA-SYOYMD          =   SPACE  )
      *        初診算定日なし
               MOVE    "False"         TO  MDCRES-DOCTORS-FEE-INITIAL
           END-IF
      *    最終来院日
           IF      SPA-LSTYMD      NOT =   SPACE
               MOVE    SPA-LSTYMD          TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  MDCRES-LAST-DATE
           END-IF
      *
      *    診察料なし
           IF      (WRK-HKNCOMBI       =   "9999" )
            OR     (WRK-SINSATU        =   "9"    )
               GO      TO      20021-HENSYU-01-EXT
           END-IF
      *
      *    治験対象外
           IF      SPA-CHIKENKBN       =   "1"
               ADD     1                   TO  IDK
               MOVE    "W17"               TO  WRK-KEICD (IDK)
               GO      TO      20021-HENSYU-01-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
           IF      WRK-SINSATU         =   SPACE
      *        設定診療コード処理
               PERFORM 20023-INPUT-SYORI-SEC
           ELSE
      *        自動表示処理
               PERFORM 20024-AUTO-SYORI-SEC
           END-IF
      *
           IF      FLG-SYOERR          =   1
      *        初診算定日なし
               MOVE    "False"         TO  MDCRES-DOCTORS-FEE-INITIAL
           END-IF
           .
       20021-HENSYU-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ判定処理
      *****************************************************************
       20021-HKN-CHEK-SEC        SECTION.
      *
           EVALUATE    SPA-RJN-ERR
      *        老人保険編集エラーがある時、エラーとする
               WHEN    1
                   MOVE    "1027"              TO  SPA-ERRCD
      *        老人保険編集エラーがある時、エラーとする
               WHEN    2
                   MOVE    "1070"              TO  SPA-ERRCD
      *        後期高齢者の保険がない時
               WHEN    3
                   MOVE    "1076"              TO  SPA-ERRCD
      *        退職国保で６５歳以上
               WHEN    9
                   MOVE    "1077"              TO  SPA-ERRCD
           END-EVALUATE
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    SPA-HKN-ERR
      *            保険エラーがある時、エラーとする
                   WHEN    1
                       MOVE    "1028"              TO  SPA-ERRCD
                   WHEN    2
                       MOVE    "1032"              TO  SPA-ERRCD
      *            入院中保険エラー
                   WHEN    6
                       MOVE    "9036"              TO  SPA-ERRCD
               END-EVALUATE
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 200232-ERR-HEN-SEC
               MOVE    "30"            TO  WRK-ERRCD
           END-IF
           .
       20021-HKN-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動表示処理得処理
      *****************************************************************
       20024-AUTO-SYORI-SEC        SECTION.
      *
           IF      WRK-SINSATU         =   "2"
                                       OR  "T"
      *        再診で初診算定日なし
               IF      SPA-SYOYMD          =   SPACE
                   MOVE    1                   TO  FLG-SYOERR
                   MOVE    "20"                TO  WRK-ERRCD
                   GO      TO      20024-AUTO-SYORI-EXT
               ELSE
      *            初診算定を再診へ
                   IF      SPA-SYOSIN          =   1
                       MOVE    ZERO            TO  SPA-SYOSIN
                   END-IF
               END-IF
           END-IF
           IF      WRK-SINSATU         =   "1"
      *        初診算定へ
               IF      SPA-SYOSIN          =   ZERO
                   MOVE    1               TO  SPA-SYOSIN
               END-IF
           END-IF
      *    初診算定日なしのクリア
           MOVE    SPACE               TO  MDCRES-DOCTORS-FEE-INITIAL
      *
      *    時間外加算
           IF      MDCREQ-OFFTIME      =   SPACE
               MOVE    ZERO                TO  SPA-GAI-JIKANKBN
           ELSE
               MOVE    MDCREQ-OFFTIME      TO  SPA-GAI-JIKANKBN
           END-IF
           MOVE    MDCREQ-OFFTIME      TO  MDCRES-OFFTIME
      *
           MOVE    ZERO                TO  SPA-TEISEIFLG
           MOVE    ZERO                TO  SPA-UKETUKE-ARI
           MOVE    ZERO                TO  IDX-ZAI
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "1"                 TO  LNK-SC10S-SYORI
      *    電話再診
           IF      WRK-SINSATU         =   "T"
               MOVE    "@"                 TO  LNK-SC10S-SYORI
               MOVE    1                   TO  SPA-TELSAI-ARI
           END-IF
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
      *    他科で算定済み
           IF      SPA-KIDCD           =   "0109"
               MOVE    1                   TO  FLG-TASRYKA
           ELSE
               MOVE    ZERO                TO  FLG-TASRYKA
           END-IF
           IF      LNK-SC10S-IDX2      >   ZERO
               PERFORM 20041-ZAIHENSYU-SEC
           END-IF
      *
      *    地域包括診療料算定時、ダミー
           IF      (SPA-CHKHKT-ARI2     =   1      )
               IF      SPA-COM-INPUTCD(1)(1:1) NOT =   "1"
                   ADD     1                   TO  IDK
                   MOVE    "W11"               TO  WRK-KEICD (IDK)
               END-IF
           END-IF
      *    施設入所・外来リハビリ算定中のダミー
           IF      (SPA-PTTOKK-KBN  NOT =   SPACE )
               ADD     1                   TO  IDK
               MOVE    "W13"               TO  WRK-KEICD (IDK)
           END-IF
           IF      (SPA-KOURESRY-ARI2  =   2   OR  3 )
               ADD     1                   TO  IDK
               MOVE    "W12"               TO  WRK-KEICD (IDK)
           END-IF
      *    小児科外来診療料の同日算定済み
           IF     (LNK-SC10S-IDX2      =   ZERO )
              AND (SPA-SYONIKA-ARI2    =   1    )
               ADD     1                   TO  IDK
               MOVE    "W14"               TO  WRK-KEICD (IDK)
           END-IF
      *H28.5
      *    地域包括診療料算定時、ダミー
           IF      (SPA-NNCHKT-ARI2     =   1      )
               IF      SPA-COM-INPUTCD(1)(1:1) NOT =   "1"
                   ADD     1                   TO  IDK
                   MOVE    "W22"               TO  WRK-KEICD (IDK)
               END-IF
           END-IF
      *H28.5
      *    小児かかりつけ診療料の同日算定済み
           IF     (LNK-SC10S-IDX2      =   ZERO )
              AND (SPA-SYONIKKR-ARI2   =   1    )
               ADD     1                   TO  IDK
               MOVE    "W23"               TO  WRK-KEICD (IDK)
           END-IF
      *
      *    初診料の併用エラー
           IF      SPA-SYOSIN-KEI      =   1
      *        当月前に併用算定がある時、病名がすべて治癒である
               ADD     1                   TO  IDK
               MOVE    "W15"               TO  WRK-KEICD (IDK)
           END-IF
           IF     (SPA-SYOSIN-KEI      =   2     )
               ADD     1                   TO  IDK
               MOVE    "W15"               TO  WRK-KEICD (IDK)
           END-IF
      *
      *    他保険で算定済みのチェック
           MOVE    ZERO                TO  FLG-TAHOKEN
           PERFORM VARYING     IDX-HKN     FROM    1   BY  1
                   UNTIL      (IDX-HKN     >   10  )  OR
                              (SPA-TA-HKNCOMBI (IDX-HKN)
                                           =   ZERO )
               IF     SPA-TA-HKNCOMBI (IDX-HKN)
                                           NOT =   SPA-GMN-HKNCOMBINUM
                   MOVE    1               TO  FLG-TAHOKEN
               END-IF
           END-PERFORM
      *
           IF      FLG-TASRYKA          =   1
      *        同日再診料
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
               INITIALIZE                      ORCSC10SAREA
               MOVE    "2"                 TO  LNK-SC10S-SYORI
               CALL    "ORCSC10S"          USING
                                           ORCSC10SAREA
                                           SPA-AREA
                                           SPA-SCR-REC
                                           SPA-K02
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
               PERFORM 20041-ZAIHENSYU-SEC
      *
           END-IF
      *
           IF      FLG-TAHOKEN          =   1
      *        他保険ありの時
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
               INITIALIZE                      ORCSC10SAREA
               MOVE    "H"                 TO  LNK-SC10S-SYORI
               CALL    "ORCSC10S"          USING
                                           ORCSC10SAREA
                                           SPA-AREA
                                           SPA-SCR-REC
                                           SPA-K02
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
               PERFORM 20041-ZAIHENSYU-SEC
      *
           END-IF
      *    複数返却あり
           IF      IDX-ZAI             >   1
               MOVE    "True"          TO  MDCRES-MEDICAL-SELECT-FLAG
           END-IF
      *
           .
       20024-AUTO-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    診療コード編集処理
      *****************************************************************
       20041-ZAIHENSYU-SEC        SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-KIDCD
           MOVE    SPACE               TO  SPA-ERRMSG2
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                  ORCSC100AREA
           MOVE    "1"                 TO  ORCSC100-KBN
           MOVE    SPACE               TO  ORCSC100-TOROKU
           MOVE    ZERO                TO  ORCSC100-INIT
           MOVE    ZERO                TO  ORCSC100-KENSADEL
           MOVE    ZERO                TO  ORCSC100-RIHTEI
           CALL    "ORCSC100"          USING
                                           ORCSC100AREA
                                           SPA-AREA
                                           SPA-K02
                                           SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *    初診エラー
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-KIDCD       NOT =   SPACE )
               IF      WRK-SINSATU         =   "1"
                   MOVE    "24"            TO  WRK-ERRCD
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  FLG-ERR
      *
           ADD     1                   TO  IDX-ZAI
           MOVE    ZERO                TO  IDX-ZAI2
      *
           MOVE    SPACE               TO  WRK-TIMEFLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE )
                         OR  ((SPA-COM-INPUTCD (IDX)   =   SPACE)
                          AND (SPA-GMN-INPUTCD (IDX)   =   SPACE))
                         OR   (FLG-ERR     =   1   )
      *        診療種別で剤分離する
               IF     (IDX             >   1       )
                  AND (SPA-COM-SRYSYUKBN(IDX)
                                   NOT =   SPA-COM-SRYSYUKBN(IDX - 1))
                   MOVE    ZERO            TO  IDX2
               END-IF
               IF      SPA-COM-DATAKBN (IDX)   =   "1"
                   MOVE    ZERO            TO  IDX2
               END-IF
      *
               IF      IDX2            =   ZERO
                   ADD     1               TO  IDX-ZAI2
                   IF      IDX-ZAI2        >   10
                       MOVE    1               TO  FLG-ERR
                   ELSE
                       IF      SPA-COM-SRYSYUKBN (IDX)
                                               =   SPACE
                           MOVE    SPA-COM-SRYKBN (IDX)
                                               TO  WRK-SRYSYUKBN(1:2)
                           MOVE    "0"         TO  WRK-SRYSYUKBN(3:1)
                       ELSE
                           MOVE    SPA-COM-SRYSYUKBN (IDX)
                                               TO  WRK-SRYSYUKBN
                       END-IF
                       MOVE    WRK-SRYSYUKBN   TO  MDCRES-MDC-CLASS
                                                    (IDX-ZAI IDX-ZAI2)
      *                診療種別区分名称チェック
                       PERFORM 510-SRYKBN-MEI-SEC
                       MOVE    WRK-SRYSYUKBNMEI
                                           TO  MDCRES-MDC-CLASS-NAME
                                                    (IDX-ZAI IDX-ZAI2)
                       MOVE    "1"         TO  MDCRES-MDC-CLASS-NUMBER
                                                    (IDX-ZAI IDX-ZAI2)
                   END-IF
               END-IF
               IF      FLG-ERR             =   ZERO
                   ADD     1               TO  IDX2
                   MOVE    SPA-COM-INPUTCD (IDX)
                                       TO  MDCRES-PRSCRPT-CODE
                                               (IDX-ZAI IDX-ZAI2 IDX2)
                   MOVE    SPA-COM-NAME (IDX)
                                       TO  MDCRES-PRSCRPT-NAME
                                               (IDX-ZAI IDX-ZAI2 IDX2)
      *            他科受診の診療科
                   IF     (SPA-COM-INPUTCD (IDX)(1:2)
                                           =   "83" )  AND
                          (SPA-COM-ATAI1 (IDX) NOT =   SPACE)
                       MOVE    SPA-GMN-MEISYO (IDX)
                                       TO  MDCRES-PRSCRPT-NAME
                                               (IDX-ZAI IDX-ZAI2 IDX2)
      **********       MOVE    SPA-COM-ATAI1 (IDX)
      *                                    TO  MDCRES-DEP-CODE 
      **********                               (IDX-ZAI IDX-ZAI2 IDX2)
                   END-IF
      *            時間外チェック
                   IF     (SPA-COM-INPUTCD(IDX)(1:1)   =   "1"   )
                      AND (SPA-COM-TIMEFLG(IDX)    NOT =   ZERO  )
                      AND (SPA-COM-DATAKBN(IDX)        =   "1"   )
                       MOVE    SPA-COM-TIMEFLG(IDX)    TO  WRK-TIMEFLG
                   END-IF
      *
      *            エラー
                   IF     (SPA-ERRCD       NOT =   SPACE )
                      AND (SPA-COM-CUR (IDX)   =   "1"   )
      *                エラー編集
                       MOVE    SPA-COM-INPUTCD (IDX) TO  WRK-SRYCD
                       PERFORM 200232-ERR-HEN-SEC
                   END-IF
               END-IF
           END-PERFORM
      *    時間外
           IF     (SPA-GAI-JIKANKBN    NOT =   ZERO  )
             AND  (WRK-TIMEFLG         NOT =   SPACE )
               IF      SPA-GAI-JIKANKBN    NOT =   WRK-TIMEFLG
                   ADD     1                   TO  IDK
                   MOVE    "W19"               TO  WRK-KEICD (IDK)
                   MOVE   WRK-TIMEFLG          TO  MDCRES-OFFTIME
               END-IF
           END-IF
      *
           IF      IDX-ZAI2            =   ZERO
               COMPUTE IDX-ZAI         =   IDX-ZAI    -   1
           END-IF
           .
       20041-ZAIHENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    設定診療コード処理
      *****************************************************************
       20023-INPUT-SYORI-SEC        SECTION.
      *
      *    送信設定診察料
      *    診療種別区分
           MOVE    MDCREQ-MDC-CLASS    TO  MDCRES-IN-MDC-CLASS
           MOVE    MDCREQ-MDC-CLASS-NAME
                                       TO  MDCRES-IN-MDC-CLASS-NAME
      *    診療コード
           MOVE    MDCREQ-PRSCRPT-CODE TO  MDCRES-IN-PRSCRPT-CODE
           MOVE    MDCREQ-PRSCRPT-NAME TO  MDCRES-IN-PRSCRPT-NAME
      *
      *    時間外加算
           MOVE    MDCREQ-OFFTIME      TO  SPA-GAI-JIKANKBN
      *    診療情報設定
           MOVE    MDCREQ-MDC-CLASS    TO  WRK-SRYSYUKBN
           MOVE    MDCREQ-MDC-CLASS    TO  WRK-SRYKBN
           MOVE    MDCREQ-PRSCRPT-CODE
                                       TO  WRK-SRYCD
      *    診療コードなし
           IF      MDCREQ-PRSCRPT-CODE     =   SPACE
               MOVE    "23"                TO  WRK-ERRCD
           ELSE
      *        剤内容
               PERFORM 200231-INPUT-HEN-SEC
           END-IF
      *
           .
       20023-INPUT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       200231-INPUT-HEN-SEC     SECTION.
      *
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "1"                 TO  LNK-SC92-KBN
           MOVE    "K02"               TO  LNK-SC92-PG
      *    警告のチェックなし
           MOVE    SPACE               TO  LNK-SC92-KEIFLG
           MOVE    1                   TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *    初診料の併用エラー
           IF      SPA-COM-SRYKBN (1)  =   "11"
               IF      LNK-SC92-SYOKBN     =   "1"
      *            前に併用算定がある時、病名がすべて治癒であること
                   IF      SPA-BYOMEI-STR      =   ZERO
                       ADD     1                   TO  IDK
                       MOVE    "W15"               TO  WRK-KEICD (IDK)
                   ELSE
                       ADD     1                   TO  IDK
                       MOVE    "W15"               TO  WRK-KEICD (IDK)
                   END-IF
               END-IF
           END-IF
      *
      *    エラー編集
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 200232-ERR-HEN-SEC
           END-IF
           PERFORM VARYING     IDX-ERR FROM    1  BY  1
                   UNTIL      (IDX-ERR     >   10     )  OR
                              (SPA-ERRCD   NOT =   SPACE)
               IF      LNK-SC92-ERRCD(IDX-ERR) NOT =   SPACE
                   MOVE    LNK-SC92-ERRCD(IDX-ERR)     TO  SPA-ERRCD
                   MOVE    LNK-SC92-ERRMSG             TO  SPA-ERRMSG2
                   PERFORM 200232-ERR-HEN-SEC
               END-IF
           END-PERFORM
      *
      *    診療名称
           IF     SPA-COM-NAME (1) NOT =   SPACE
               MOVE    SPA-COM-NAME (1)    TO  MDCRES-IN-PRSCRPT-NAME
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      200231-INPUT-HEN-EXT
           END-IF
      *
           MOVE    ZERO                TO  SPA-TEISEIFLG
           MOVE    ZERO                TO  SPA-UKETUKE-ARI
           MOVE    ZERO                TO  IDX-ZAI
      *
      *    同日算定回数
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "3"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-KIDCD
           MOVE    SPACE               TO  SPA-ERRMSG2
      *TEST
      *    DISPLAY "SPA-RRK-LSTYM:"  SPA-RRK-LSTYMD
      *           ",SPA-TAKA-SRYKA-R:" SPA-TAKA-SRYKA-R
      *TEST
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                  ORCSC100AREA
           MOVE    "1"                 TO  ORCSC100-KBN
           MOVE    SPACE               TO  ORCSC100-TOROKU
           MOVE    ZERO                TO  ORCSC100-INIT
           MOVE    ZERO                TO  ORCSC100-KENSADEL
           MOVE    ZERO                TO  ORCSC100-RIHTEI
           CALL    "ORCSC100"          USING
                                           ORCSC100AREA
                                           SPA-AREA
                                           SPA-K02
                                           SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           IF      SPA-KIDCD           =   "0101"
      *        診察料算定済み
               MOVE    "25"                TO  WRK-ERRCD
           END-IF
      *    エラー
           IF     (SPA-ERRCD       NOT =   SPACE )
      *        エラー編集
               MOVE    SPA-COM-INPUTCD(1)  TO  WRK-SRYCD
               PERFORM 200232-ERR-HEN-SEC
           END-IF
      *
           .
       200231-INPUT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       200232-ERR-HEN-SEC     SECTION.
      *
           IF      SPA-ERRMSG2         =   SPACE
               MOVE    "K02"               TO  LNK-SC96-PG
               CALL    "ORCSC96"           USING    SPA-AREA
                                                    ORCSC96AREA
               MOVE    SPA-ERRMSG          TO  SPA-ERRMSG2
           END-IF
      *
           IF      SPA-ERRCD(1:1)      =   "K"
               ADD     1                   TO  IDK
               MOVE    "W10"               TO  WRK-KEICD (IDK)
               MOVE    WRK-SRYCD           TO  WRK-KEI-SRYCD(IDK)
               MOVE    SPA-ERRMSG2         TO  WRK-KEIMSG(IDK)
           ELSE
               IF      WRK-ERRCD           =   SPACE
                   MOVE    "40"                TO  WRK-ERRCD
                   MOVE    SPA-ERRMSG2         TO  WRK-ERRMSG
               ELSE
                   MOVE    "40"                TO  WRK-ERRCD-1
                   MOVE    SPA-ERRMSG2         TO  WRK-ERRMSG-1
               END-IF
           END-IF
           MOVE    SPACE               TO  SPA-ERRMSG2
           MOVE    SPACE               TO  SPA-ERRMSG
           .
       200232-ERR-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せの決定処理
      *****************************************************************
       200215-HKNCOMBI-SET-SEC     SECTION.
      *
      *    包括分入力
           IF     (MDCREQ-MAIN-INSURANCE-CLASS =   "999" )
               OR (MDCREQ-COMBINATION-NUMBER   =   "9999")
               INITIALIZE              MDCRES-INSURANCE-G
               MOVE    "9999"          TO  WRK-HKNCOMBI
               MOVE    "包括分入力"    TO  MDCRES-MAIN-INSURANCE-NAME
           ELSE
               PERFORM 2002151-HKNCOMBI-CALL-SEC
           END-IF
      *    保険組合せ番号
           MOVE    WRK-HKNCOMBI        TO  MDCRES-COMBINATION-NUMBER
           .
       200215-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険種類変換処理
      *****************************************************************
       2002151-HKNCOMBI-CALL-SEC     SECTION.
      *
           MOVE    ZERO                TO  WRK-HKNCOMBI
      *
           IF      MDCREQ-COMBINATION-NUMBER   =   SPACE
      *        保険組合せ編集処理
               INITIALIZE                      ORCSAPIHKNAREA
      *        対象なし有
               MOVE    "2"                 TO  ORCSAPIHKN-KBN
      *        当日
               MOVE    SPACE               TO  ORCSAPIHKN-KBN2
      *        診療科
               MOVE    WRK-SRYKA           TO  ORCSAPIHKN-SRYKA
      *        保険種類
               MOVE    MDCREQ-MAIN-INSURANCE-CLASS 
                                           TO  ORCSAPIHKN-IN-HKNNUM
      *        保険者番号
               MOVE    MDCREQ-MAIN-INSURANCE-NUMBER
                                           TO  ORCSAPIHKN-IN-HKNJANUM
      *        保険種類
               MOVE    MDCREQ-MAIN-INSURANCE-NAME 
                                           TO  ORCSAPIHKN-IN-HKNNUM-NAME
      *        記号
               MOVE    MDCREQ-MAIN-MARK    TO  ORCSAPIHKN-IN-MARK
      *        番号
               MOVE    MDCREQ-MAIN-NUMBER  TO  ORCSAPIHKN-IN-NUMBER
      *        公費情報
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   4
      *            公費種類
                   MOVE    MDCREQ-INSURANCE-CLASS (IDX)
                                       TO  ORCSAPIHKN-IN-KOHNUM (IDX)
      *            公費名称
                   MOVE    MDCREQ-INSURANCE-NAME  (IDX)
                                       TO  ORCSAPIHKN-IN-KOHNUM-NAME
                                                                 (IDX)
      *            負担者番号
                   MOVE    MDCREQ-PROVIDER-NUMBER(IDX)
                                       TO  ORCSAPIHKN-IN-FTNJANUM (IDX)
      *            受給者番号
                   MOVE    MDCREQ-RECIPIENT-NUMBER (IDX)
                                       TO  ORCSAPIHKN-IN-JKYSNUM  (IDX)
               END-PERFORM
           ELSE
      *        保険組合せチェック
               PERFORM 200311-HKNCOMBI-NUM-SEC
      *        保険組合せ編集処理
               INITIALIZE                      ORCSAPIHKNAREA
               MOVE    "2"                 TO  ORCSAPIHKN-KBN
               MOVE    SPACE               TO  ORCSAPIHKN-KBN2
      *        診療科
               MOVE    WRK-SRYKA           TO  ORCSAPIHKN-SRYKA
               MOVE    WRK-HKNCOMBI        TO  ORCSAPIHKN-IN-HKNCOMBI
           END-IF
           MOVE    SPA-SRYYMD          TO  ORCSAPIHKN-SRYYMD
           CALL    "ORCSAPIHKN"        USING
                                       SPA-AREA
                                       ORCSAPIHKNAREA
           IF      ORCSAPIHKN-OT-HKNCOMBI  =   SPACE
               IF      MDCREQ-COMBINATION-NUMBER   =   SPACE
                   MOVE    "17"                    TO  WRK-ERRCD
               ELSE
                   MOVE    "16"                    TO  WRK-ERRCD
               END-IF
           ELSE
               MOVE    ORCSAPIHKN-OT-HKNCOMBI  TO  WRK-HKNCOMBI
      *
      *        出力領域保険情報編集
               MOVE    1                   TO  IDX
               PERFORM 2002101-HKNHEN-TBL-SEC
           END-IF
           .
       2002151-HKNCOMBI-CALL-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ番号数値チェック
      *****************************************************************
       200311-HKNCOMBI-NUM-SEC          SECTION.
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    MDCREQ-COMBINATION-NUMBER   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "16"                TO  WRK-ERRCD
               MOVE    MDCREQ-COMBINATION-NUMBER   TO  WRK-HKNCOMBI
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  WRK-HKNCOMBI
           END-IF
      *
           .
       200311-HKNCOMBI-NUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力領域保険情報編集処理
      *****************************************************************
       2002101-HKNHEN-TBL-SEC              SECTION.
      *
           IF      ORCSAPIHKN-THKNCOMBI (IDX)
                                   NOT =   ORCSAPIHKN-OT-HKNCOMBI
      *        保険組合せ番号なし
               MOVE    "16"                TO  WRK-ERRCD
               GO      TO      2002101-HKNHEN-TBL-EXT
           END-IF
      *
           INITIALIZE              MDCRES-INSURANCE-G
      *    保険の種類
           MOVE    ORCSAPIHKN-HKNNUM (IDX)
                                   TO  MDCRES-MAIN-INSURANCE-CLASS
      *    保険者番号
           MOVE    ORCSAPIHKN-HKNJANUM (IDX)
                                   TO  MDCRES-MAIN-INSURANCE-NUMBER
      *    保険の名称
           MOVE    ORCSAPIHKN-HKNNUM-NAME (IDX)
                                   TO  MDCRES-MAIN-INSURANCE-NAME
      *    記号
           MOVE    ORCSAPIHKN-KIGO  (IDX)
                                   TO  MDCRES-MAIN-MARK
      *    番号
           MOVE    ORCSAPIHKN-NUM  (IDX)
                                   TO  MDCRES-MAIN-NUMBER
      *    継続区分
           MOVE    ORCSAPIHKN-CONTKBN  (IDX)
                                   TO  MDCRES-MAIN-CONTINUATION
      *    補助区分
           MOVE    ORCSAPIHKN-HOJOKBN  (IDX)
                                   TO  MDCRES-MAIN-ASSISTANCE
           MOVE    ORCSAPIHKN-HOJOKBN-NAME  (IDX)
                                   TO  MDCRES-MAIN-ASSI-NAME
      *    本人家族区分
           MOVE    ORCSAPIHKN-HONKZKKBN  (IDX)
                                   TO  MDCRES-MAIN-FAMILY
      *    被保険者名
           MOVE    ORCSAPIHKN-HIHKNJANAME  (IDX)
                                   TO  MDCRES-MAIN-POLICYHOLDER
      *    有効開始日
           MOVE    ORCSAPIHKN-TEKSTYMD  (IDX)
                                   TO  MDCRES-MAIN-START-DATE
      *    有効終了日
           MOVE    ORCSAPIHKN-TEKEDYMD  (IDX)
                                   TO  MDCRES-MAIN-END-DATE
      *H30.8
      *    非表示区分判定
           IF      ORCSAPIHKN-HYOJIKBN (IDX)   =   "1"
                                               OR  "3"
      *        非表示分
               MOVE    "27"                    TO  WRK-ERRCD
           END-IF
      *
      *    公費情報
           PERFORM VARYING     IDY2    FROM    1   BY  1
                   UNTIL       IDY2    >   4
      *        公費の種類
               MOVE    ORCSAPIHKN-KOHNUM (IDX IDY2)
                                       TO  MDCRES-INSURANCE-CLASS
                                                             (IDY2)
      *        公費の種類名称
               MOVE    ORCSAPIHKN-KOHNUM-NAME (IDX IDY2)
                                       TO  MDCRES-INSURANCE-NAME
                                                             (IDY2)
      *        負担者番号
               MOVE    ORCSAPIHKN-FTNJANUM (IDX IDY2)
                                       TO  MDCRES-PROVIDER-NUMBER
                                                             (IDY2)
      *        受給者番号
               MOVE    ORCSAPIHKN-JKYSNUM (IDX IDY2)
                                       TO  MDCRES-RECIPIENT-NUMBER
                                                             (IDY2)
      *        有効開始日
               MOVE    ORCSAPIHKN-KOH-TEKSTYMD (IDX IDY2)
                                       TO  MDCRES-INSURANCE-START-DATE
                                                             (IDY2)
      *        有効終了日
               MOVE    ORCSAPIHKN-KOH-TEKEDYMD (IDX IDY2)
                                       TO  MDCRES-INSURANCE-END-DATE
                                                             (IDY2)
      *        入院−負担率（割）
               MOVE    ORCSAPIHKN-RATE-ADMISSION (IDX IDY2)
                                       TO  MDCRES-RATE-ADMISSION
                                                             (IDY2)
      *        入院−固定額
               MOVE    ORCSAPIHKN-MONEY-ADMISSION (IDX IDY2)
                                       TO  MDCRES-MONEY-ADMISSION
                                                             (IDY2)
      *        外来−負担率（割）
               MOVE    ORCSAPIHKN-RATE-OUTPATIENT (IDX IDY2)
                                       TO  MDCRES-RATE-OUTPATIENT
                                                             (IDY2)
      *        外来−固定額
               MOVE    ORCSAPIHKN-MONEY-OUTPATIENT (IDX IDY2)
                                       TO  MDCRES-MONEY-OUTPATIENT
                                                             (IDY2)
           END-PERFORM
      *
      *    労災区分
           MOVE    ORCSAPIHKN-RSI-HKNKBN (IDX) TO  WRK-RSI-HKNKBN
           MOVE    ORCSAPIHKN-HKNNUM (IDX)     TO  WRK-HKNNUM
           .
       2002101-HKNHEN-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分判定処理
      *****************************************************************
       200211-NYUGAIKBN-CHK-SEC        SECTION.
      *
      *    入院履歴検索
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    SPA-SRYYMD          TO  PTNYUINRRK-NYUINYMD
           MOVE    SPA-SRYYMD          TO  PTNYUINRRK-TAIINYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
                   MOVE    ZERO            TO  FLG-PTNYUINRRK
               ELSE
                   INITIALIZE                  PTNYUINRRK-REC
                   MOVE    1               TO  FLG-PTNYUINRRK
               END-IF
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  FLG-NYUINDAY
           IF      FLG-PTNYUINRRK      =   ZERO
      *        入院中
               IF     (SPA-NYUGAIKBN       =   "2" )
                 AND  (SPA-SRYYMD      NOT =   PTNYUINRRK-NYUINYMD)
                 AND  (SPA-SRYYMD      NOT =   PTNYUINRRK-TAIINYMD)
      *            外来は、自費・アフターケア以外をエラー
                   IF     (WRK-HKNNUM (1:2)    =   "98" )
                    OR   ((WRK-HKNNUM          =   "971")  AND
                          (WRK-RSI-HKNKBN      =   "3"  ))
                    OR    (WRK-HKNCOMBI        =   ZERO )
      *                入院中は診察料算定なし
                       ADD     1                   TO  IDK
                       MOVE    "W21"               TO  WRK-KEICD (IDK)
                   ELSE
                       MOVE    "21"                TO  WRK-ERRCD
                   END-IF
               END-IF
      *        入院日
               IF     (SPA-SRYYMD      =   PTNYUINRRK-NYUINYMD)
                   MOVE    1               TO  FLG-NYUINDAY
               END-IF
           END-IF
           .
       200211-NYUGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    中途終了データ有無処理
      *****************************************************************
       200212-WKSRYACT-CHK-SEC        SECTION.
      *
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    WRK-PERFORM-YMD     TO  WKSRY-SRYYMD
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key15"             TO  MCP-PATHNAME
               PERFORM 940-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-WKSRYACT        =   ZERO
      *        中途データあり
               MOVE    "True"              TO  MDCRES-TEMP-MEDICAL-FLAG
      *        対象を返却
      *********PERFORM 200212-WKSRYACT-HENSYU-SEC
      *
           ELSE
      *        中途データなし
               MOVE    "False"             TO  MDCRES-TEMP-MEDICAL-FLAG
           END-IF
           .
       200212-WKSRYACT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    中途データ編集処理
      *****************************************************************
       200212-WKSRYACT-HENSYU-SEC            SECTION.
      *
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    WRK-PERFORM-YMD     TO  WKSRY-SRYYMD
           MOVE    WRK-SRYKA           TO  WKSRY-SRYKA
           MOVE    WRK-HKNCOMBI        TO  WKSRY-HKNCOMBI
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 940-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           INITIALIZE                  LNK-WKSRYACT-AREA
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
           PERFORM UNTIL   (FLG-WKSRYACT    =   1 )
                       OR  (LNK-WKSRYACT-MAX    >=  400)
               ADD     1                   TO  LNK-WKSRYACT-MAX
               MOVE    WKSRYACT-REC        TO  LNK-WKSRYACT-REC
                                                    (LNK-WKSRYACT-MAX)
      *
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 940-WKSRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      LNK-WKSRYACT-MAX      =   ZERO
               GO      TO      200212-WKSRYACT-HENSYU-EXT
           END-IF
      *    診療内容編集サブ
           INITIALIZE                  ORAPI021SUB2V3AREA
           MOVE    "2"                 TO  ORAPISUB2-SYORIKBN
           MOVE    LNK-WKSRYACT-MAX    TO  ORAPISUB2-WKSRYACT-MAX
           CALL    "ORAPI021SUB2V3"    USING
                                       MCPAREA
                                       SPA-AREA
                                       ORAPI021SUB2V3AREA
                                       LNK-WKSRYACT-AREA
      *
           INITIALIZE                  MDCRES-WKMEDICAL-INF-G
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX            >   110   )
                       OR  (ORAPISUB2-MDC-CLASS-KBN (IDX)
                                           =   SPACE )
               MOVE    ORAPISUB2-MDC-CLASS-KBN (IDX)
                                           TO  MDCRES-WKMDC-CLASS-KBN
                                                                 (IDX)
               MOVE    ORAPISUB2-MDC-CLASS (IDX)
                                           TO  MDCRES-WKMDC-CLASS (IDX)
               MOVE    ORAPISUB2-MDC-CLASS-NAME (IDX)
                                           TO  MDCRES-WKMDC-CLASS-NAME
                                                               (IDX)
               MOVE    ORAPISUB2-MDC-CLASS-NUMBER (IDX)
                                           TO  MDCRES-WKMDC-CLASS-NUMBER
                                                               (IDX)
      *        剤内容
               PERFORM VARYING IDY     FROM    1   BY  1
                       UNTIL   (IDY            >   50    )
                           OR  (ORAPISUB2-PRSCRPT-CODE (IDX IDY)
                                               =   SPACE )
                   MOVE    ORAPISUB2-PRSCRPT-CODE (IDX IDY)
                                       TO  MDCRES-WKPRSCRPT-CODE
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-NAME (IDX IDY)
                                       TO  MDCRES-WKPRSCRPT-NAME
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-NUMBER (IDX IDY)
                                       TO  MDCRES-WKPRSCRPT-NUMBER
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-MONEY (IDX IDY)
                                       TO  MDCRES-WKPRSCRPT-MONEY
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-ATAI-G (IDX IDY)
                                       TO  MDCRES-WKPRSCRPT-ATAI-G
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-CONTKBN (IDX IDY)
                                       TO  MDCRES-WKPRSCRPT-CONTKBN
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-NAIFKBN (IDX IDY)
                                       TO  MDCRES-WKPRSCRPT-NAIFKBN
                                                            (IDX IDY)
               END-PERFORM
           END-PERFORM
           .
       200212-WKSRYACT-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *     SPATMPの登録処理
      *****************************************************************
       2008-SPATMP-INS-SEC            SECTION.
      *
      *    TBL-SPATMP保存
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPA-AREA"          TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPATMP-DATA-REC
           PERFORM 20081-SPATMP-INSERT-SEC
      *
           .
       2008-SPATMP-INS-EXT.
           EXIT.
      *****************************************************************
      *     API処理登録処理
      *****************************************************************
       2008-APIPARAINS-SEC            SECTION.
      *
      *    登録
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "AP21"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "API21V3"           TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
      *    レスポンス番号・電子カルテUID保存
           MOVE    SPACE               TO  WRK-APIV3DATA-REC
           MOVE    MDCRES-RESPONSE-NUMBER
                                       TO  WRK-RESPONSE-NUMBER
      *    処理ＰＧ
           MOVE    "01"                TO  WRK-PARA-SYORIPG
      *!!!
      *    印刷のみ
           IF      MDCREQ-PATIENT-MODE =   "Print"
               MOVE    "PRT"           TO  WRK-PARA-MODE
           END-IF
      *
           MOVE    WRK-APIV3DATA-REC   TO  APIPARA-DATA-REC
      *
           MOVE    MDCRES-KARTE-UID    TO  APIPARA-KARTE-UID
           MOVE    WRK-PTNUM           TO  APIPARA-PTNUM
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
      *
           .
       2008-APIPARAINS-EXT.
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
      *    ORCA-UID 初期処理
      *****************************************************************
       2009-PARA-INIT-SEC            SECTION.
      *
      *    UIDコードをTERMID
           MOVE    MDCREQ-ORCA-UID     TO  WRK-ORCA-UID
           MOVE    SPACE               TO  SPA-TERMID
           STRING  "API+"
                   WRK-ORCA-UID        DELIMITED  BY  SPACE
                                       INTO    SPA-TERMID
           END-STRING
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "AP21"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "API21V3"           TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
           PERFORM 9010-APIPARA-KEYREAD-SEC
      *
           IF      FLG-APIPARA          =   ZERO
               MOVE    APIPARA-DATA-REC    TO  WRK-APIV3DATA-REC
               MOVE    APIPARA-KARTE-UID   TO  WRK-KARTE-UID
               MOVE    APIPARA-PTNUM       TO  WRK-PARA-PTNUM
               MOVE    1                   TO  FLG-UIDOK
           ELSE
               MOVE    SPACE               TO  WRK-APIV3DATA-AREA
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
      *
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
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
      *    一時保存ファイル削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "AP21"              TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-PARA-CALL-SEC
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "AP21"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           PERFORM 900-ORCDBMAIN-SEC
      *
      *    前回の排他制御解除
           PERFORM 990-LOCK-OUT-SEC
      *
           .
       2009-PARA-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    API 保険組合せ番号返却、診察料返却処理
      *****************************************************************
       400-SYORI-00-SEC            SECTION.
      *
           MOVE    "00"                TO  MDCRES-RESPONSE-NUMBER
           MOVE    "00"                TO  MDCRES-REQUEST-NUMBER
      *
      *    入力項目チェック処理
           PERFORM 4001-INPUT-CHK-SEC
           IF      WRK-ERRCD       NOT =   SPACE
      *        排他制御解除
               PERFORM 990-LOCK-OUT-SEC
               GO      TO      400-SYORI-00-EXT
           END-IF
      *    患者情報内容編集
           PERFORM 40012-MDCRES-HEN-SEC
      *    入外区分判定
           PERFORM 200211-NYUGAIKBN-CHK-SEC
      *    入院中は自動設定なしとする
           IF     (WRK-ERRCD           =   "21" )
              OR ((IDK                 >   ZERO ) AND
                  (WRK-KEICD (IDK)     =   "W21"))
              OR  (FLG-NYUINDAY        =   1    )
               MOVE    "70"                TO  WRK-ERRCD
               MOVE    SPACE               TO  WRK-KEICD (IDK)
               MOVE    ZERO                TO  IDK
      *        排他制御解除
               PERFORM 990-LOCK-OUT-SEC
               GO      TO      400-SYORI-00-EXT
           END-IF
      *
           INITIALIZE                  SPA-K01KYOUTU
           INITIALIZE                  SPA-K02
      *    診察料自動算定
           PERFORM 4003-SINSATU-SET-SEC
      *
      *    排他制御解除
           PERFORM 990-LOCK-OUT-SEC
           .
       400-SYORI-00-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報設定内容チェック
      *****************************************************************
       4001-INPUT-CHK-SEC      SECTION.
      *
      *    編集処理
           PERFORM 20010-INPUT-HEN-SEC
      *    必須チェック
      *    患者番号
           IF      WRK-PTNUM           =   SPACE
               MOVE    "01"                TO  WRK-ERRCD
           END-IF
      *    診療科
           IF      WRK-SRYKA           =   SPACE
               IF     (WRK-ERRCD           =   SPACE  )
                   MOVE    "02"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    診療日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PERFORM-CHK-SEC
           END-IF
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PTNUM-CHK-SEC
      *        排他制御処理
               IF      WRK-ERRCD           =   SPACE
                   PERFORM 20019-LOCK-SYORI-SEC
               END-IF
           END-IF
      *
      *    診療科コードチェック
           IF     (WRK-SRYKA       NOT =   SPACE  )
           AND    (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-SRYKA-CHK-SEC
           END-IF
      *
      *    診察料区分チェック
           IF     (MDCREQ-DOCTORS-FEE  NOT =   SPACE  )
           AND    (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-DOCTORS-CHK-SEC
           END-IF
      *
           .
       4001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報出力情報編集処理
      *****************************************************************
       40012-MDCRES-HEN-SEC     SECTION.
      *
           MOVE    PTINF-NAME          TO  MDCRES-NAME
           MOVE    PTINF-KANANAME      TO  MDCRES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MDCRES-BIRTHDAY
           MOVE    PTINF-SEX           TO  MDCRES-SEX
      *
           MOVE    WRK-SRYKA-NAME      TO  MDCRES-APPOINT-DEP-NAME
           MOVE    WRK-DRCD-NAME       TO  MDCRES-APPOINT-DR-NAME
      *
           MOVE    SPACE               TO  MDCRES-ORCA-UID
      *    時間外区分なし
           MOVE    SPACE               TO  MDCREQ-OFFTIME
           MOVE    SPACE               TO  MDCRES-OFFTIME
      *
           .
       40012-MDCRES-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     診察料　自動編集処理
      *****************************************************************
       4003-SINSATU-SET-SEC            SECTION.
      *
      *    ＳＰＡ初期設定
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    "1"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
      *    SPA患者情報
           MOVE    WRK-PTID            TO  SPA-PTID
           MOVE    WRK-PTNUM           TO  SPA-PTNUM
           MOVE    WRK-SRYKA           TO  SPA-SRYKA
      *****MOVE    WRK-HKNCOMBI        TO  SPA-HKNCOMBINUM
      *
      *    APIの患者決定
           INITIALIZE                  ORCSC95AREA
           MOVE    "@"                 TO  LNK-SC95-KBN
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
      *    前回の保険組合せが不可の時
           MOVE    SPACE               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
           MOVE    SPA-BIRTHDAY        TO  SPA-NAI-BIRTHDAY
           MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
           MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBI
      *
      *    保険組合せ判定
           IF     (SPA-HKN-ERR     NOT =   ZERO )  OR
                  (SPA-RJN-ERR     NOT =   ZERO )
               PERFORM 20021-HKN-CHEK-SEC
           END-IF
      *
      *    前回保険組合せの判定チェック
           IF     (SPA-HKNCHK-FLG  NOT =   ZERO   )
               ADD     1                   TO  IDK
               IF      SPA-HKNCHK-FLG      =   2
      ****         MOVE    "K173"              TO  SPA-ERRCD
                   MOVE    "W24"               TO  WRK-KEICD (IDK)
               ELSE
      ***          MOVE    "K172"              TO  SPA-ERRCD
                   MOVE    "W25"               TO  WRK-KEICD (IDK)
               END-IF
           END-IF
      *
      *    保険組合せ編集
           PERFORM 4002-HKNCOMBI-SET-SEC
      *    IF     (LNK-SC95-LAST-RENNUM    =   9     )
      *      AND  (WRK-ERRCD               =   SPACE )
      *        MOVE    "0220"              TO  SPA-ERRCD
      *        PERFORM 200232-ERR-HEN-SEC
      *        MOVE    "31"            TO  WRK-ERRCD
      *    END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      4003-SINSATU-SET-EXT
           END-IF
      *
      *    年齢計算
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
           MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
           MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
      **** MOVE    SPA-HKNCOMBINUM     TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           MOVE    AGECHK-NENREI2-YY   TO  SPA-NENREI-YY
           MOVE    AGECHK-NENREI2-MM   TO  SPA-NENREI-MM
           MOVE    AGECHK-NENREI2-DD   TO  SPA-NENREI-DD
      *    未就学前
           MOVE    AGECHK-SYUGAKUKBN   TO  SPA-SYUGAKUKBN
      *
      *    診察料なし
           IF      (WRK-HKNCOMBI       =   "9999" )
            OR     (WRK-SINSATU        =   "9"    )
               GO      TO      4003-SINSATU-SET-EXT
           END-IF
      *
      *    治験対象外
           IF      SPA-CHIKENKBN       =   "1"
               ADD     1                   TO  IDK
               MOVE    "W17"               TO  WRK-KEICD (IDK)
               GO      TO      4003-SINSATU-SET-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
      *    自動表示処理
           PERFORM 20024-AUTO-SYORI-SEC
      *
           IF      SPA-SYOYMD          =   SPACE
      *        初診算定日なし
               MOVE    "False"         TO  MDCRES-DOCTORS-FEE-INITIAL
           END-IF
           .
       4003-SINSATU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療種別区分名称、診療行為区分セット
      *****************************************************************
       4002-HKNCOMBI-SET-SEC            SECTION.
      *
      *    保険組合せ編集処理
           INITIALIZE                      ORCSAPIHKNAREA
           MOVE    "1"                 TO  ORCSAPIHKN-KBN
           MOVE    SPACE               TO  ORCSAPIHKN-KBN2
      *    診療科
           MOVE    WRK-SRYKA           TO  ORCSAPIHKN-SRYKA
           MOVE    WRK-HKNCOMBI        TO  ORCSAPIHKN-IN-HKNCOMBI
           MOVE    SPA-SRYYMD          TO  ORCSAPIHKN-SRYYMD
           CALL    "ORCSAPIHKN"        USING
                                       SPA-AREA
                                       ORCSAPIHKNAREA
           IF      ORCSAPIHKN-OT-HKNCOMBI  =   SPACE
               MOVE    "17"                    TO  WRK-ERRCD
               MOVE    ZERO                    TO  WRK-HKNCOMBI
           ELSE
               MOVE    ORCSAPIHKN-OT-HKNCOMBI  TO  WRK-HKNCOMBI
      *
               PERFORM VARYING    IDX      FROM    1   BY  1
                       UNTIL     (IDX      >   ORCSAPIHKN-OT-MAX)
                   IF      ORCSAPIHKN-THKNCOMBI (IDX)
                                           =   ORCSAPIHKN-OT-HKNCOMBI
      *                出力領域保険情報編集
                       PERFORM 2002101-HKNHEN-TBL-SEC
                   END-IF
               END-PERFORM
           END-IF
           MOVE    WRK-HKNCOMBI        TO  MDCRES-COMBINATION-NUMBER
           .
       4002-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *     診療種別区分名称、診療行為区分セット
      *****************************************************************
       510-SRYKBN-MEI-SEC            SECTION.
      *
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYKBN
                       MOVE    SPACE           TO  WRK-SRYSYUKBNMEI
                       MOVE    "0006"          TO  SPA-ERRCD
                   WHEN    WRK-SRYSYUKBN       =   MSYU-SRYSYUKBN 
                                                           (MSYU-IDX)
                       MOVE    MSYU-SRYKBN  (MSYU-IDX)
                                                TO  WRK-SRYKBN
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  WRK-SRYSYUKBNMEI
           END-SEARCH 
      *
           .
       510-SRYKBN-MEI-EXT.
           EXIT.
      *
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
           WHEN    "01"
               MOVE    "患者番号が未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "02"
               MOVE    "診療科が未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "03"
               MOVE    "ドクターが未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "04"
               MOVE    "電子カルテＵＩＤが未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "05"
               MOVE    "オルカＵＩＤが未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "08"
               STRING  "初回接続が完了していません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "10"
               MOVE    "患者番号に該当する患者が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "診療日が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "12"
               MOVE    "診療日が平成２７年４月１日以降ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE    "診療科が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "14"
               MOVE    "ドクターが存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "15"
               STRING  "保険組合せ情報を設定して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "16"
               STRING  "保険組合せ番号が存在しません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "17"
               STRING  "保険組合せが存在しません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "18"
               MOVE     "時間外区分は、０から８で設定して下さい"
                                               TO  WRK-ERRMSG
           WHEN    "19"
               MOVE     "診察料区分がありません。"
                                               TO  WRK-ERRMSG
           WHEN    "20"
               STRING  "初診算定日の登録がありません。"
                       "初診料を登録して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "21"
               STRING  "入院中です。"
                       "外来で算定できる保険ではありません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "23"
               STRING  "診察料区分・診療コードの設定がありません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "24"
               STRING  "すでに当日に診察料を算定しています。"
                       "初診料の算定はできません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "25"
               STRING  "すでに当日に診察料を算定しています。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *H30.8
           WHEN    "26"
               MOVE    "処理区分エラー"        TO  WRK-ERRMSG 
           WHEN    "27"
               STRING  "保険組合せは外来非表示となっています。"
                       "外来では使用できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *!
           WHEN    "60"
               STRING  "解除対象がありません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *API
           WHEN    "70"
               STRING  "入院中です。処理できません。"
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
      *
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
      ******** MOVE    "処理区分未設定"        TO  WRK-ERRMSG
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
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       891-WARNING-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-WERRCD
      *警告
           WHEN    "W01"
               MOVE    "診療日を設定しました。"
                                               TO  WRK-ERRMSG
      *    WHEN    "W02"
      *        MOVE    "保険組合せをゼロで登録しました。"
      *                                        TO  WRK-ERRMSG
      *    WHEN    "W04"
      *        STRING  "入院期間中です。"
      *                "外来で展開できない保険組合せです。"
      *                                        DELIMITED  BY  SIZE
      *                                        INTO    WRK-ERRMSG
      *        END-STRING
           WHEN    "W15"
               STRING  "警告！！"
                       "当月に初診料と併用算定できないコードが登録"
                       "されています。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "W11"
               STRING  "地域包括診療料が算定済みです。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "W12"
               STRING  "外来リハビリテーション料の算定中です。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "W13"
               STRING  "施設入所中です。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "W14"
               STRING  "当日に小児科外来診療料を算定しています。"
                       "診察料は算定できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *H28.5
           WHEN    "W22"
               STRING  "認知症地域包括診療料が算定済みです。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "W23"
               STRING  "当日に小児かかりつけ診療料を算定しています。"
                       "診察料は算定できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *
           WHEN    "W17"
               STRING  "保険組合せが診察料の自動算定の対象外です。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "W19"
               STRING  "時間外区分を変更しました。"
                       "送信した時間外区分に変更して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "W21"
               STRING  "入院中です。外来で診察料は算定できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *H29.8 API
           WHEN    "W24"
               STRING  "前回の保険組合せは削除になっています。"
                       "保険組合せを確認して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "W25"
               STRING  "前回と保険組合せが違います。"
                       "確認して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           END-EVALUATE
           .
       891-WARNING-MSG-EXT.
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
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
      *    MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
           MOVE    "K02"               TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    1                   TO  SLOCK-MODE
           MOVE    PTINF-PTID          TO  SLOCK-PTID
      *    電子カルテキー
           MOVE    MDCREQ-KARTE-UID    TO  SLOCK-KARTE-UID
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
      *    MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
      *
           MOVE    "K02"               TO  SPA-MOTOPG
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
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
           MOVE    WRK-MCP-TERM        TO  MCP-TERM
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
      *    ワーク診療行為データ読込
      *****************************************************************
       940-WKSRYACT-READ-SEC          SECTION.
      *
           PERFORM  910-DBFETCH-SEC 
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  WKSRYACT-REC
               MOVE    ZERO            TO  FLG-WKSRYACT
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
      *
           .
       940-WKSRYACT-READ-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           PERFORM 910-DBFETCH-SEC
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
               PERFORM 910-DBFETCH-SEC
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
      *    パラメータテーブル検索処理
      *****************************************************************
       900-PARA-CALL-SEC                SECTION.
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    PARA-REC            TO  MCPDATA-REC
      *
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PARA-CALL-EXT.
           EXIT.
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
