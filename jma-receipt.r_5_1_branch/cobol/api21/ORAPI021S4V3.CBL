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
       PROGRAM-ID.         ORAPI021S4V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 診療行為　削除　（入力一体化）
      *  管理者            :
      *  作成日付    作業者        記述
      *  15/12/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.00    NACL-多々納  19/01/XX  複数科・保険対応
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
           03  FLG-JYURRK              PIC 9(01).
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
           03  IDXF                PIC 9(04).
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
           03  WRK-ERRMSG-02           PIC X(100).
      *
           03  WRK-ERRCD-1             PIC X(02).
           03  WRK-ERRMSG-1            PIC X(100).
           03  WRK-WERRCD              PIC X(03).
           03  WRK-KEICD-G.
               05  WRK-KEICD-OCC       OCCURS   40.
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
           03  WRK-SYORI-MODE          PIC X(03).
      *
           03  WRK-SINSATU             PIC X(01).
      *
           03  WRK-SRYSYUKBN           PIC X(03).
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-SRYSYUKBNMEI        PIC X(40).
           03  WRK-SRYCD               PIC X(09).
           03  WRK-TIMEFLG             PIC X(01).
      *
           03  WRK-SELECT-CD           PIC X(04).
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
      *
           03  WRK-IN-DENPNUM          PIC 9(07).
      *
           03  WRK-DENPNUM             PIC 9(07).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-NAME          PIC X(80).
           03  WRK-DRCD                PIC X(05).
           03  WRK-DRCD-NAME           PIC X(80).
           03  WRK-RENNUM              PIC 9(01).
      *    負担率・額編集
           03  WRK-FTNRATE             PIC 9(02)V99.
           03  WRK-Z9                  PIC 9.ZZ.
           03  WRK-FTNRATEMEI          PIC X(04).
      *
      *    入院日
      *    03  WRK-NYUIN-DATE          PIC X(10).
      *    03  WRK-NYUIN-YMD           PIC X(08).
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
      *        伝票番号
               05  WRK-PARA-DENPNUM        PIC 9(07).
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
           COPY    "CPMDCXMLV3REQ4.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPMDCXMLV3RES4.INC".
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
      *    診療行為外来更新パラメタ領域
           COPY    "CPORCSCGAIRAI.INC".
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
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *    受診履歴
       01  HZN-JYURRK-REC.
           COPY    "CPJYURRK.INC"    REPLACING
                                     //JYURRK-// BY //HZN-JYURRK-//.
      *
      *    入院履歴
      *01  PTNYUINRRK-REC.
      *    COPY    "CPPTNYUINRRK.INC".
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
           INITIALIZE                      SPA-K01KYOUTU
           INITIALIZE                      SPA-K02
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
               MOVE   404                  TO  APILST06-HTTPSTATUS
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
           INITIALIZE                      XML-MEDICALV3RES4
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
      *    システム日付で検索
      ***  診療日をシステム日付へ
      *    IF      MDCREQ-PERFORM-DATE NOT =   SPACE
      *        MOVE    MDCREQ-PERFORM-DATE TO  WRK-HEN-DATE
      *        MOVE    SPACE               TO  WRK-HEN-TIME
      *        PERFORM 802-DAYHEN02-SEC
      *        MOVE    WRK-SYMD            TO  WRK-PERFORM-YMD
      *        PERFORM 20011-PERFORM-CHK-SEC
      *        診療日をシステム日付へ
      *        IF      WRK-ERRCD           =   SPACE
      *            MOVE    WRK-PERFORM-YMD     TO  SPA-SYSYMD
      *        END-IF
      *        MOVE    SPACE               TO  WRK-ERRCD
      *****END-IF
      *
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
      *
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
      *        UIDチェック
               IF      MDCREQ-ORCA-UID NOT =   SPACE
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
      *        確認処理
               MOVE    1                   TO  FLG-SYORI
               IF      MDCREQ-ORCA-UID NOT =   SPACE
                   PERFORM 20019-PARA-ALLDELDEL-SEC
               END-IF
           WHEN    "02"
      *        削除処理
               MOVE    2                   TO  FLG-SYORI
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
      *        診療内容返却処理
               PERFORM 2002-SYORI-01-SEC
           WHEN    2
      *        削除処理
               PERFORM 2003-SYORI-02-SEC
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
           .
       2099-CANCEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-APTXMLREAD-SEC   SECTION.
      *
           IF      APILST06-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-MEDICALV3REQ4
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalv3req4" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST06-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalv3req4"     TO  APIREQ-RECORDNAME
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
           MOVE    "xml_medicalv3req4" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST06-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalv3req4"     TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-MEDICALV3REQ4
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE           TO  APILST06-BODY
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
                       MOVE    "確認処理終了"
                                           TO  MDCRES-API-RESULT-MSG
                   WHEN    2
                       MOVE    "削除処理終了"
                                           TO  MDCRES-API-RESULT-MSG
               END-EVALUATE
      *        警告あり
               IF      WRK-KEICD (1)   NOT =   SPACE
                AND    WRK-ERRKBN          =   SPACE
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
      *
               IF      WRK-ERRMSG-02   NOT =   SPACE
                   MOVE    SPACE           TO  MDCRES-API-RESULT-MSG
                   STRING  WRK-ERRMSG
                           WRK-ERRMSG-02   DELIMITED  BY  SPACE
                                           INTO  MDCRES-API-RESULT-MSG
                   END-STRING
               END-IF
           END-IF
      *
      *    ワーニング編集
           PERFORM 3001-WAINGMSG-HEN-SEC
      *
      *
           IF      APILST06-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalv3res4" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalv3res4"     TO  MDCRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALV3RES4
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_medicalv3res4" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalv3res4"     TO  MDCRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALV3RES4
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE  MDCRES-OBJECT     TO  APILST06-BODY 
               MOVE  "application/xml" TO  APILST06-CONTENT-TYPE
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
           MOVE    ZERO                TO  IDM1
           PERFORM VARYING     IDK     FROM    1   BY  1
                   UNTIL      (IDK     >   40   )   OR
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
      *    処理区分
           EVALUATE    MDCREQ-PATIENT-MODE
      *        削除
               WHEN    "Delete"
                   MOVE    "DEL"           TO  WRK-SYORI-MODE
      *        訂正
               WHEN    "Modify"
                   MOVE    "UPD"           TO  WRK-SYORI-MODE
               WHEN    OTHER
                   MOVE    "02"            TO  WRK-ERRCD
           END-EVALUATE
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
      *    受診履歴チェック
           IF     (WRK-ERRCD           =   SPACE  )
               IF      MDCREQ-INVOICD-NUMBER   =   SPACE
      *            診療科コードチェック
                   PERFORM 20011-SRYKA-CHK-SEC
      *            保険組合せチェックチェック
                   PERFORM 20011-HKNCOMBI-CHK-SEC
                   IF      WRK-ERRCD           =   SPACE
                       PERFORM 20013-JYURRK-CHK-SEC
                   END-IF
               ELSE
      *            伝票番号チェック
                   PERFORM 20011-DENPNUM-CHK-SEC
               END-IF
      *        エラー時患者情報編集
               IF     (WRK-ERRCD       NOT =   SPACE  )
      *            患者情報内容編集
                   PERFORM 20021-MDCRES-HEN-SEC
               END-IF
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
      *    伝票番号
           MOVE    MDCREQ-INVOICD-NUMBER   TO  MDCRES-INVOICD-NUMBER
      *    診療日・時間
           MOVE    MDCREQ-PERFORM-DATE     TO  MDCRES-PERFORM-DATE
      ***  MOVE    MDCREQ-PERFORM-TIME     TO  MDCRES-PERFORM-TIME
      *    数字のみに編集
           MOVE    MDCREQ-PERFORM-DATE     TO  WRK-HEN-DATE
           MOVE    SPACE                   TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-PERFORM-YMD
           MOVE    WRK-THMS                TO  WRK-PERFORM-TIME
      *    未設定時はシステム情報設定
           IF      WRK-PERFORM-YMD         =   SPACE
               MOVE    SMCNDATE-YMD        TO  WRK-PERFORM-YMD
               MOVE    MDCRES-INFORMATION-DATE
                                           TO  MDCRES-PERFORM-DATE
           END-IF
      *
      *    診療日をシステム日付・診療日付へ
           MOVE    WRK-PERFORM-YMD     TO  SPA-SYSYMD
      *
           MOVE    WRK-PERFORM-YMD     TO  SPA-SRYYMD
      *    診療科
           MOVE    MDCREQ-APPOINT-DEP-CODE
                                       TO  MDCRES-APPOINT-DEP-CODE
           MOVE    MDCREQ-APPOINT-DEP-CODE
                                       TO  WRK-SRYKA
      *    保険組合せ
           MOVE    MDCREQ-COMBINATION-NUMBER
                                       TO  MDCRES-COMBINATION-NUMBER
           MOVE    MDCREQ-COMBINATION-NUMBER
                                       TO  WRK-HKNCOMBI
           MOVE    MDCREQ-NUMBER       TO  MDCRES-NUMBER
      *    伝票番号
           MOVE    MDCREQ-INVOICD-NUMBER
                                       TO  MDCRES-INVOICD-NUMBER
      *    入外区分（外来）
           MOVE    "2"                 TO  SPA-NYUGAIKBN
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
      *    伝票番号チェック処理
      *****************************************************************
       20011-DENPNUM-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  WRK-DENPNUM
           MOVE    ZERO                TO  WRK-IN-DENPNUM
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    MDCREQ-INVOICD-NUMBER   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   7     )   OR
                  (SNUM-SYOSUU         >   0     )
      *        数値７桁
               MOVE    "13"                TO  WRK-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-DENPNUM
               MOVE    SNUM-OUTNUM         TO  WRK-IN-DENPNUM
           END-IF
      *
           IF      WRK-DENPNUM         =   ZERO
               MOVE    "13"                TO  WRK-ERRCD
               GO      TO      20011-DENPNUM-CHK-EXT
           END-IF
      *
           INITIALIZE                   HZN-JYURRK-REC
      *
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    WRK-DENPNUM         TO  JYURRK-DENPNUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 910-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-JYURRK          =   ZERO
               IF      JYURRK-SRYYMD   NOT =   SPA-SRYYMD
                   MOVE    "14"                TO  WRK-ERRCD
               END-IF
      *2019.1
               IF      WRK-ERRCD           =   SPACE
      *   複数科保険対応
                   IF      JYURRK-GRP-DENPNUM  NOT =   ZERO
      *            複数科保険は返却できない
      *****        MOVE    "15"                TO  WRK-ERRCD
                       IF      JYURRK-GRP-RENNUM   >   1
      *                    主たる受診履歴へ変更
                           PERFORM 20014-GRP-JYURRK-SEC
                       END-IF
                   END-IF
               END-IF
               IF      WRK-ERRCD           =   SPACE
                   MOVE    JYURRK-REC          TO  HZN-JYURRK-REC
                   MOVE    JYURRK-DENPNUM      TO  WRK-DENPNUM
     ******        MOVE    JYURRK-DENPNUM      TO  MDCRES-INVOICD-NUMBER
               END-IF
           ELSE
               MOVE    "22"                TO  WRK-ERRCD
           END-IF
           .
       20011-DENPNUM-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    主たる受診履歴へ変更
      *****************************************************************
       20014-GRP-JYURRK-SEC         SECTION.
      *
           MOVE    JYURRK-REC          TO  HZN-JYURRK-REC
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key26"             TO  MCP-PATHNAME
               PERFORM 910-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-JYURRK          =   1
               MOVE    HZN-JYURRK-REC      TO  JYURRK-REC
      *        受診履歴不整合
               ADD     1                   TO  IDK
               MOVE    "W05"               TO  WRK-KEICD(IDK)
           ELSE
      *        主たる受診履歴へ変更
               ADD     1                   TO  IDK
               MOVE    "W04"               TO  WRK-KEICD(IDK)
           END-IF
           .
       20014-GRP-JYURRK-EXT.
           EXIT.
      *
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
      *********MOVE    "13"                TO  WRK-ERRCD
               MOVE    SPACE               TO  WRK-SRYKA-NAME
           END-IF
           MOVE    WRK-SRYKA-NAME      TO  MDCRES-APPOINT-DEP-NAME
      *
           .
       20011-SRYKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ番号数値チェック
      *****************************************************************
       20011-HKNCOMBI-CHK-SEC          SECTION.
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    MDCREQ-COMBINATION-NUMBER   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "18"                TO  WRK-ERRCD
               MOVE    MDCREQ-COMBINATION-NUMBER   TO  WRK-HKNCOMBI
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  WRK-HKNCOMBI
           END-IF
      *    連番は数値であること
           IF      MDCREQ-NUMBER       =   SPACE
               MOVE    ZERO            TO  WRK-RENNUM
           ELSE
               IF      MDCREQ-NUMBER   NUMERIC
                   MOVE    MDCREQ-NUMBER   TO  WRK-RENNUM
               ELSE
                   MOVE    "23"            TO  WRK-ERRCD
               END-IF
           END-IF
           IF      WRK-RENNUM          =   ZERO
               MOVE    1                   TO  WRK-RENNUM
           END-IF
      *
           .
       20011-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴チェック処理
      *****************************************************************
       20013-JYURRK-CHK-SEC        SECTION.
      *
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
           MOVE    WRK-SRYKA           TO  JYURRK-SRYKA
           MOVE    WRK-HKNCOMBI        TO  JYURRK-HKNCOMBINUM
           MOVE    WRK-RENNUM          TO  JYURRK-RENNUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key19"             TO  MCP-PATHNAME
               PERFORM 910-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-JYURRK          =   ZERO
               IF      JYURRK-GRP-DENPNUM  NOT =   ZERO
      *            複数科保険は返却できない
      *****        MOVE    "15"                TO  WRK-ERRCD
                   IF      JYURRK-GRP-RENNUM   >   1
      *                 主たる受診履歴へ変更
                        PERFORM 20014-GRP-JYURRK-SEC
                   END-IF
               END-IF
               IF      WRK-ERRCD           =   SPACE
                   MOVE    JYURRK-REC          TO  HZN-JYURRK-REC
                   MOVE    JYURRK-DENPNUM      TO  WRK-DENPNUM
      ********     MOVE    JYURRK-DENPNUM      TO  MDCRES-INVOICD-NUMBER
               END-IF
           ELSE
               MOVE    "22"                TO  WRK-ERRCD
           END-IF
           .
       20013-JYURRK-CHK-EXT.
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
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20021-HENSYU-01-SEC
           END-IF
      *
           IF     (WRK-ERRCD           =   SPACE )
             AND  (WRK-SYORI-MODE      =   "DEL" )
      *        確認メッセージ
               MOVE    "0001"              TO  WRK-SELECT-CD
               IF      SPA-TEI-DENPNUM (1) >   ZERO
      *            複数科保険あり
                   MOVE    "0002"              TO  WRK-SELECT-CD
               END-IF
               PERFORM 20029-SELECT-MSG-SEC
      *
               MOVE    "S"                 TO  WRK-ERRKBN
               MOVE    "30"                TO  WRK-ERRCD
      *        確認あり
               MOVE    "True"          TO  MDCRES-MEDICAL-SELECT-FLAG
           END-IF
      *
      *    返却レスポンス番号
           IF     (WRK-ERRCD           =   SPACE )
             OR   (WRK-ERRCD           =   "30"  )
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
           MOVE    WRK-ORCA-UID        TO  MDCRES-ORCA-UID
      *
           MOVE    WRK-DENPNUM         TO  MDCRES-INVOICD-NUMBER
           MOVE    WRK-HKNCOMBI        TO  MDCRES-COMBINATION-NUMBER
      *
           .
       20021-MDCRES-HEN-EXT.
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
           MOVE    PTINF-BIRTHDAY      TO  SPA-BIRTHDAY
           MOVE    PTINF-BIRTHDAY      TO  SPA-NAI-BIRTHDAY
      *
           MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
      *
           MOVE    HZN-JYURRK-REC      TO  JYURRK-REC
      *    診療日付
           MOVE    JYURRK-SRYYMD       TO  SPA-SRYYMD
           MOVE    JYURRK-RENNUM       TO  SPA-RENNUM
           MOVE    JYURRK-DOUJI-RENNUM TO  SPA-DOUJI-RENNUM
           MOVE    JYURRK-DRCD         TO  SPA-DRCD
           MOVE    JYURRK-SRYKA        TO  SPA-SRYKA
           MOVE    JYURRK-SRYKA        TO  SPA-OLD-SRYKA
      *    保険組合せ
           MOVE    JYURRK-HKNCOMBINUM  TO  SPA-HKNCOMBINUM
           MOVE    JYURRK-HKNCOMBINUM  TO  SPA-OLD-HKNCOMBI
      *
           MOVE    JYURRK-DENPNUM      TO  SPA-DENPNUM
      *    伝票番号
           MOVE    SPA-DENPNUM         TO  MDCRES-INVOICD-NUMBER
      *
           MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
           MOVE    SPA-SRYYMDW         TO  SPA-GMN-SRYYMD
      *    訂正処理
           MOVE    1                   TO  SPA-SYORIFLG
      *H29.5
      *    HAORI処理
           MOVE    1                   TO  SPA-API-FLG
      *    初診算定日は変更しない
      **** MOVE    SPA-GMN-SYOSINYMD   TO  WRK-SPA-GMN-SYOSINYMD
      *
      *    中途データパラ作成
           INITIALIZE                  SPA-SCR-REC
           INITIALIZE                  ORCSC95AREA
     ***** MOVE    "4"                 TO  LNK-SC95-KBN
           MOVE    "R"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           MOVE    SPA-SRYYMD          TO  LNK-SC95-RRKYMD
           MOVE    SPA-RENNUM          TO  LNK-SC95-RENNUM
           MOVE    SPA-DOUJI-RENNUM    TO  LNK-SC95I-DOUJI-RENNUM
           MOVE    JYURRK-GRP-DENPNUM  TO  LNK-SC95-GRP-DENPNUM
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
           MOVE    SPA-OLD-HKNCOMBI    TO  SPA-HKNCOMBINUM
           MOVE    SPA-OLD-SRYKA       TO  SPA-SRYKA
      *    中途データ編集
           PERFORM 200212-WKSRYACT-HENSYU-SEC
      *H28.12
      *    院内・院外区分
           EVALUATE    ORAPISUB2-INGAIKBN
               WHEN    "1"
      *            院外
                   MOVE    "True"          TO  MDCRES-OUTSIDE-LASS
               WHEN    "2"
      *            院内
                   MOVE    "False"         TO  MDCRES-OUTSIDE-LASS
           END-EVALUATE
      *
      *    診療科編集
           MOVE    SPA-SRYKA           TO  MDCRES-APPOINT-DEP-CODE
           MOVE    SPA-DRCD            TO  MDCRES-APPOINT-DR-CODE
           MOVE    SPA-SRYKAMEI        TO  MDCRES-APPOINT-DEP-NAME
           MOVE    SPA-GMN-DRCDNAME    TO  MDCRES-APPOINT-DR-NAME
           MOVE    SPA-RENNUM          TO  MDCRES-NUMBER
      *    点滴の変更前保存
           IF      LNK-SC95-TENTEKI    =   1
               MOVE    "XXXX"          TO  SPA-ACCT2-SRYCD
           END-IF
           MOVE    LNK-SC95-CHUSYA     TO  SPA-ACCT2-CSYYOURYO
      *
      *    保険組合せ編集
           INITIALIZE                   MDCRES-INSURANCE-G
           MOVE    SPA-HKNCOMBINUM     TO  MDCRES-COMBINATION-NUMBER
      *
      *    包括分入力
           IF     (SPA-HKNCOMBINUM     =   "9999" )
               MOVE    "9999"          TO  WRK-HKNCOMBI
               MOVE    "包括分入力"    TO  MDCRES-MAIN-INSURANCE-NAME
           ELSE
      *        保険組合せ編集処理
               PERFORM 2002101-HKNHEN-TBL-SEC
      *
      *        保険負担割合
               INITIALIZE                  ORCSAGECHKAREA
               MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
               MOVE    SPA-PTID            TO  AGECHK-PTID
               MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
               MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
               MOVE    SPA-HKNCOMBINUM     TO  AGECHK-HKNCOMBINUM
               MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
               CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
               IF      AGECHK-FTNRATE  NOT =   ZERO
      *           負担率
                   COMPUTE WRK-FTNRATE         =   AGECHK-FTNRATE
                                               /   100
                   MOVE    WRK-FTNRATE         TO  WRK-Z9
                   MOVE    WRK-Z9              TO  WRK-FTNRATEMEI
                   MOVE    WRK-FTNRATEMEI
                                       TO  MDCRES-COMBINATION-RATE
               ELSE
                   MOVE    "0"         TO  MDCRES-COMBINATION-RATE
               END-IF
           END-IF
      *
      *2019.1
      *    複数科保険
           IF      SPA-TEI-DENPNUM (1) >   ZERO
               PERFORM 200213-GRP-DENPNUM-SEC
           END-IF
      *
           .
       20021-HENSYU-01-EXT.
           EXIT.
      *****************************************************************
      *    中途データ編集処理
      *****************************************************************
       200212-WKSRYACT-HENSYU-SEC      SECTION.
      *
      *
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
      *
           MOVE    SPACE               TO  PARA-REC
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           PERFORM
               UNTIL  (FLG-PARA            =   1     )
               ADD     1                   TO  LNK-WKSRYACT-MAX
               IF      LNK-WKSRYACT-MAX    >   SPA-MAX-LINE
                   DISPLAY "K02 LNK-WKSRYACT-MAX 400 OVR"
                   ADD     1                   TO  IDK
                   MOVE    "W02"               TO  WRK-KEICD(IDK)
               ELSE
                   MOVE    PARA-DATA-REC   TO  LNK-WKSRYACT-REC
                                                      (LNK-WKSRYACT-MAX)
               END-IF
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    一時保存ファイル削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del3"              TO  MCP-PATHNAME
           PERFORM 900-PARA-CALL-SEC
      *
      *
           IF      LNK-WKSRYACT-MAX    =   ZERO
               ADD     1                   TO  IDK
               MOVE    "W03"               TO  WRK-KEICD(IDK)
               GO      TO      200212-WKSRYACT-HENSYU-EXT
           END-IF
      *
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
      *
           INITIALIZE                  MDCRES-MEDICAL-INF-G
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX            >   120   )
      ****             OR  (ORAPISUB2-MDC-CLASS-KBN (IDX)
      ****                                 =   SPACE )
                       OR ((ORAPISUB2-MDC-CLASS-KBN (IDX)
                                           =   SPACE )   AND
                           (ORAPISUB2-MDC-DATA-KBN (IDX)
                                           =   SPACE ))
      *!!!
      *2019.1
      *        複数科・保険項目
               MOVE    ORAPISUB2-MDC-DATA-KBN (IDX)
                                           TO  MDCRES-MDC-DATA-KBN(IDX)
               MOVE    ORAPISUB2-MDC-COMBINATION (IDX)
                                       TO  MDCRES-MDC-COMBINATION-NUMBER
                                                               (IDX)
               MOVE    ORAPISUB2-MDC-MAIN-INS-NAME (IDX)
                                       TO  MDCRES-MDC-MAIN-INS-NAME
                                                               (IDX)
               MOVE    ORAPISUB2-PUBLIC-INSURANCE-INFO (IDX)
                                       TO  MDCRES-MDC-PUBLIC-INS-INFO
                                                               (IDX)
               MOVE    ORAPISUB2-MDC-DEP-CODE (IDX)
                                       TO  MDCRES-MDC-DEP-CODE
                                                               (IDX)
               MOVE    ORAPISUB2-MDC-DEP-CODE-NAME (IDX)
                                       TO  MDCRES-MDC-DEP-CODE-NAME
                                                               (IDX)
               MOVE    ORAPISUB2-MDC-DR-CODE (IDX)
                                       TO  MDCRES-MDC-DR-CODE
                                                               (IDX)
               MOVE    ORAPISUB2-MDC-DR-CODE-NAME (IDX)
                                       TO  MDCRES-MDC-DR-CODE-NAME
                                                               (IDX)
      *!!!
               MOVE    ORAPISUB2-MDC-CLASS-KBN (IDX)
                                           TO  MDCRES-MDC-CLASS-KBN(IDX)
               MOVE    ORAPISUB2-MDC-CLASS (IDX)
                                           TO  MDCRES-MDC-CLASS (IDX)
               MOVE    ORAPISUB2-MDC-CLASS-NAME (IDX)
                                           TO  MDCRES-MDC-CLASS-NAME
                                                               (IDX)
               MOVE    ORAPISUB2-MDC-CLASS-NUMBER (IDX)
                                           TO  MDCRES-MDC-CLASS-NUMBER
                                                               (IDX)
               MOVE    ORAPISUB2-MDC-POINT (IDX)
                                           TO  MDCRES-MDC-POINT (IDX)
               MOVE    ORAPISUB2-MDC-MONEY (IDX)
                                           TO  MDCRES-MDC-MONEY (IDX)
      *        剤区分（包括分）
               IF       ORAPISUB2-MDC-CODE (IDX)  =   "1"
                    MOVE    "True"         TO  MDCRES-INCLUSION-CLASS
                                                                (IDX)
               END-IF
      *H29.5
      *        包括検査の検査数
               IF      ORAPISUB2-HKTKNSA-CNT (IDX) >   ZERO
                   MOVE    ORAPISUB2-HKTKNSA-CNT (IDX)
                                               TO  MDCRES-HKTKNSA-CNT
                                                                (IDX)
               END-IF
      *        剤内容
               PERFORM VARYING IDY     FROM    1   BY  1
                       UNTIL   (IDY            >   50    )
                           OR  (ORAPISUB2-PRSCRPT-CODE (IDX IDY)
                                               =   SPACE )
                   MOVE    ORAPISUB2-PRSCRPT-CODE (IDX IDY)
                                       TO  MDCRES-PRSCRPT-CODE
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-NAME (IDX IDY)
                                       TO  MDCRES-PRSCRPT-NAME
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-NUMBER (IDX IDY)
                                       TO  MDCRES-PRSCRPT-NUMBER
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-MONEY (IDX IDY)
                                       TO  MDCRES-PRSCRPT-MONEY
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-ATAI-G (IDX IDY)
                                       TO  MDCRES-PRSCRPT-ATAI-G
                                                            (IDX IDY)
      *****        MOVE    ORAPISUB2-PRSCRPT-GENERIC (IDX IDY)
      *                                TO  MDCRES-PRSCRPT-GENERIC
      *                                                     (IDX IDY)
      *            MOVE    ORAPISUB2-PRSCRPT-GENNAME (IDX IDY)
      *                                TO  MDCRES-PRSCRPT-GENNAME
      *                                                     (IDX IDY)
      *            一般名称があれば、名称は削除
      *            IF      ORAPISUB2-PRSCRPT-GENNAME (IDX IDY)
      *                                    NOT =   SPACE
      *                MOVE    SPACE   TO  MDCRES-PRSCRPT-NAME
      *                                                     (IDX IDY)
      *****        END-IF
                   MOVE    ORAPISUB2-PRSCRPT-CONTKBN (IDX IDY)
                                       TO  MDCRES-PRSCRPT-CONTKBN
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-NAIFKBN (IDX IDY)
                                       TO  MDCRES-PRSCRPT-NAIFKBN
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-AUTOKBN (IDX IDY)
                                       TO  MDCRES-PRSCRPT-AUTOKBN
                                                            (IDX IDY)
      *H29.5
      *            単位コード
                   MOVE    ORAPISUB2-PRSCRPT-TANICD (IDX IDY)
                                       TO  MDCRES-PRSCRPT-TANICD
                                                            (IDX IDY)
                   MOVE    ORAPISUB2-PRSCRPT-TANICD-NAME (IDX IDY)
                                       TO  MDCRES-PRSCRPT-TANICD-NAME
                                                            (IDX IDY)
      *            フィルム分画数
                   MOVE    ORAPISUB2-PRSCRPT-FILMNUM (IDX IDY)
                                       TO  MDCRES-PRSCRPT-FILMNUM
                                                            (IDX IDY)
      *            加算自動算定なし
                   MOVE    ORAPISUB2-PRSCRPT-NOAUTO (IDX IDY)
                                       TO  MDCRES-PRSCRPT-NOAUTO
                                                            (IDX IDY)
      *
               END-PERFORM
           END-PERFORM
           IF     (ORAPISUB2-ERRCD NOT =   SPACE )
             AND  (WRK-ERRCD           =   SPACE )
      *        剤数オーバー
               ADD     1                   TO  IDK
               MOVE    "W02"               TO  WRK-KEICD(IDK)
               MOVE    ORAPISUB2-ERRMSG    TO  WRK-KEIMSG(IDK)
           END-IF
           .
       200212-WKSRYACT-HENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    出力領域保険情報編集処理
      *****************************************************************
       2002101-HKNHEN-TBL-SEC              SECTION.
      *
           INITIALIZE                      ORCSAPIHKNAREA
           MOVE    "2"                 TO  ORCSAPIHKN-KBN
           MOVE    SPACE               TO  ORCSAPIHKN-KBN2
      *    診療科
           MOVE    SPA-SRYKA           TO  ORCSAPIHKN-SRYKA
           MOVE    SPA-HKNCOMBINUM     TO  ORCSAPIHKN-IN-HKNCOMBI
           CALL    "ORCSAPIHKN"        USING
                                       SPA-AREA
                                       ORCSAPIHKNAREA
           MOVE    1                   TO  IDX
           IF     (ORCSAPIHKN-ERRCD    NOT =   ZERO  )
             OR   (ORCSAPIHKN-THKNCOMBI (IDX)
                                   NOT =   SPA-HKNCOMBINUM )
      *        保険組合せ番号なし
               ADD     1                       TO  IDK
               MOVE    "W01"                   TO  WRK-KEICD(IDK)
               GO      TO      2002101-HKNHEN-TBL-EXT
           END-IF
      *
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
      *    MOVE    ORCSAPIHKN-HIHKNJANAME  (IDX)
      *                            TO  MDCRES-MAIN-POLICYHOLDER
      *    有効開始日
      *    MOVE    ORCSAPIHKN-TEKSTYMD  (IDX)
      *                            TO  MDCRES-MAIN-START-DATE
      *    有効終了日
      *    MOVE    ORCSAPIHKN-TEKEDYMD  (IDX)
      *                            TO  MDCRES-MAIN-END-DATE
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
      *        MOVE    ORCSAPIHKN-KOH-TEKSTYMD (IDX IDY2)
      *                                TO  MDCRES-INSURANCE-START-DATE
      *                                                      (IDY2)
      *        有効終了日
      *        MOVE    ORCSAPIHKN-KOH-TEKEDYMD (IDX IDY2)
      *                                TO  MDCRES-INSURANCE-END-DATE
      *                                                      (IDY2)
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
      *!!!
      *2019.1
      *****************************************************************
      *    複数科保険編集処理
      *****************************************************************
       200213-GRP-DENPNUM-SEC        SECTION.
      *
           PERFORM VARYING     IDXF    FROM    1   BY  1
                   UNTIL      (IDXF        >   15 )
                         OR   (SPA-TEI-DENPNUM (IDXF)
                                           =   ZERO )
               MOVE    SPA-TEI-DENPNUM (IDXF)
                                       TO  MDCRES-SUB-INVOICD-NUMBER
                                                               (IDXF)
      *        診療科
               MOVE    SPA-TEI-SRYKA (IDXF)
                                       TO  MDCRES-SUB-DEP-CODE
                                                               (IDXF)
      *        保険組合せ番号
               MOVE    SPA-TEI-HKNCOMBI (IDXF)
                                       TO  MDCRES-SUB-COMBINATION-NUMBER
                                                               (IDXF)
      *        連番
               MOVE    SPA-TEI-RENNUM (IDXF)
                                       TO  MDCRES-SUB-NUMBER (IDXF)
           END-PERFORM
           .
       200213-GRP-DENPNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ編集処理
      *****************************************************************
       20029-SELECT-MSG-SEC        SECTION.
      *
           MOVE    SPACE               TO  MDCRES-SELECT-MSG
           MOVE    WRK-SELECT-CD       TO  MDCRES-SELECT-CODE
           EVALUATE    MDCRES-SELECT-CODE
               WHEN    "0001"
                   MOVE    "受診履歴を削除します。よろしいですか？"
                                           TO  MDCRES-SELECT-MSG
               WHEN    "0002"
                   STRING  "複数科保険組合せの受診履歴です。"
                           "すべての受診履歴を削除します。"
                           "よろしいですか？"
                                           DELIMITED  BY  SPACE
                                           INTO  MDCRES-SELECT-MSG
                   END-STRING
           END-EVALUATE
      *
           .
       20029-SELECT-MSG-EXT.
           EXIT.
      *!!!
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       2003-SYORI-02-SEC        SECTION.
      *
      *    前回の続きであること
           IF     (WRK-SYORI-MODE      =   "DEL" )
              AND (WRK-PARA-MODE       =   "DEL" )
              AND (WRK-PARA-DENPNUM    =   WRK-DENPNUM)
              AND (WRK-PARA-PTNUM      =   WRK-PTNUM  )
               CONTINUE
           ELSE
               MOVE    "20"                TO  WRK-ERRCD
           END-IF
      *
      *    患者情報内容編集
           PERFORM 20021-MDCRES-HEN-SEC
      *
           IF      WRK-ERRCD           =   SPACE
               EVALUATE    MDCREQ-SELECT-ANSWER
               WHEN    "Ok"
                   PERFORM 20031-DELETE-SYORI-SEC
               WHEN    "No"
                   MOVE    "25"                TO  WRK-ERRCD
               WHEN    OTHER
                   MOVE    "21"                TO  WRK-ERRCD
               END-EVALUATE
           END-IF
      *
      *    返却レスポンス番号
           IF      WRK-ERRCD           =   "20"
                                       OR  "21"
               MOVE    "02"                TO  MDCRES-RESPONSE-NUMBER
           ELSE
      *        更新エラー時も解除
      *        パラメタ削除・排他解除処理
               PERFORM 20019-PARA-ALLDELDEL-SEC
               MOVE    "00"                TO  MDCRES-RESPONSE-NUMBER
           END-IF
      *
           .
       2003-SYORI-02-EXT.
           EXIT.
      *****************************************************************
      *    診療行為　削除処理
      *****************************************************************
       20031-DELETE-SYORI-SEC        SECTION.
      *
      *    TBL-SPATMP
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPA-AREA"          TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
           PERFORM 9100-SPATMP-KEYREAD-SEC
           IF      FLG-SPATMP          =   ZERO
               MOVE    SPATMP-DATA-REC     TO  SPA-AREA
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           ELSE
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
               IF     (SPA-DENPNUM         =   WRK-DENPNUM)
                  AND (SPA-SRYYMD          =   WRK-PERFORM-YMD )
                  AND (SPA-OLD-SRYKA   NOT =   SPACE       )
                   CONTINUE
               ELSE
                   MOVE    "08"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      20031-DELETE-SYORI-EXT
           END-IF
      *
      *    外来更新処理
           MOVE    WRK-OPID            TO  SPA-OPID
           INITIALIZE                  SPA-TEI-AREA
           INITIALIZE                  ORCSCGAIRAIAREA
           MOVE    "2"                 TO  ORCSCGAIRAI-KBN
           CALL    "ORCSCGAIRAI"       USING
                                       SPA-AREA
                                       ORCSCGAIRAIAREA
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
      *    警告エラー
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-ERRMSG2     NOT =   SPACE)
                   ADD     1                   TO  IDK
                   MOVE    "W30"               TO  WRK-KEICD(IDK)
                   MOVE    SPA-ERRMSG2         TO  WRK-KEIMSG(IDK)
               END-IF
           ELSE
      *        エラー
               PERFORM 200329-ERRCD-MSG-SEC
               IF      WRK-ERRMSG-02   NOT =   SPACE
                   MOVE    SPA-ERRMSG2         TO  WRK-ERRMSG-02
               END-IF
               IF      SPA-ERRCD(1:1)      =   "K"
                   MOVE    SPACE               TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       20031-DELETE-SYORI-EXT.
           EXIT.
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
      *    IF      SPA-ERRCD       NOT =   SPACE
      *        PERFORM 200232-ERR-HEN-SEC
      *        MOVE    "30"            TO  WRK-ERRCD
      *    END-IF
           .
       20021-HKN-CHEK-EXT.
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
      *    TBL-SPATMP保存
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "AP21"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPA-K02"           TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
      *
           MOVE    SPA-K02             TO  SPATMP-DATA-REC
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
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "AP21"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           PERFORM 900-ORCDBMAIN-SEC
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
           MOVE    WRK-DENPNUM         TO  WRK-PARA-DENPNUM
           MOVE    WRK-SYORI-MODE      TO  WRK-PARA-MODE
           MOVE    MDCRES-SELECT-CODE
                                       TO  WRK-SELECT-CODE
      *
           MOVE    WRK-APIV3DATA-REC   TO  APIPARA-DATA-REC
      *
           MOVE    MDCRES-KARTE-UID    TO  APIPARA-KARTE-UID
           MOVE    WRK-PTNUM           TO  APIPARA-PTNUM
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
           IF     (WRK-ERRCD               =   SPACE )
             AND  (MDCREQ-REQUEST-NUMBER   NOT =   "01"  )
      *        リクエスト０１以外は、同じ処理の続きであること
      *        診療行為登録の処理の続きであること
               IF      WRK-PARA-SYORIPG    NOT =   "01"
                   MOVE    "08"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
           .
       2009-PARA-INIT-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ削除処理
      *****************************************************************
       20019-PARA-ALLDELDEL-SEC        SECTION.
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
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
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
       20019-PARAINIT-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       200329-ERRCD-MSG-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    SPA-ERRCD
               WHEN    "0002"
                   STRING  "患者情報が検索できませんでした。"
                           "患者情報テーブルを確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   MOVE    "40"            TO  WRK-ERRCD
               WHEN    "0003"
                   STRING  "伝票番号が取得できませんでした。"
                           "システム管理テーブルを確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   MOVE    "41"            TO  WRK-ERRCD
               WHEN    "0005"
                   STRING  "ＤＢ更新がエラーになりました。"
                           "ＳＥに連絡して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   MOVE    "42"            TO  WRK-ERRCD
               WHEN    "0105"
                   STRING  "この診療科の当日の受診が９回登録済です。"
                           "これ以上登録できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   MOVE    "43"            TO  WRK-ERRCD
               WHEN    "9000"
                   MOVE    "算定履歴の更新ができませんでした"
                                           TO  WRK-ERRMSG
                   MOVE    "44"            TO  WRK-ERRCD
               WHEN    "0051"
                   STRING  "中間データに不整合が発生しました。"
                           "強制終了後、"
                           "初回接続からおこなってください。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
                   END-STRING
                   MOVE    "45"            TO  WRK-ERRCD
               WHEN    "0052"
                   STRING  "外用薬剤の回数が１回以上になります。"
                           "同じ剤は回数をまとめて入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-ERRMSG
                   END-STRING
                   MOVE    "46"            TO  WRK-ERRCD
               WHEN    OTHER
                   MOVE    SPA-ERRCD       TO  WRK-ERRMSG
                   MOVE    "47"            TO  WRK-ERRCD
           END-EVALUATE
           .
       200329-ERRCD-MSG-EXT.
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
           WHEN    "01"
               MOVE    "患者番号が未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "02"
               MOVE    "処理区分エラー"
                                               TO  WRK-ERRMSG
      *    WHEN    "03"
      *        MOVE    "ドクターが未設定です"
      *                                        TO  WRK-ERRMSG
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
               MOVE    "伝票番号エラーです"
                                               TO  WRK-ERRMSG
           WHEN    "14"
               MOVE    "伝票番号の受診履歴が送信内容と一致しません"
                                               TO  WRK-ERRMSG
           WHEN    "15"
               STRING  "複数科保険組合せの受診履歴です。"
                       "処理できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "18"
               STRING  "保険組合せ番号が存在しません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "20"
               STRING  "処理順が違います。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "21"
               STRING  "選択結果エラーです。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "22"
               MOVE    "該当の受診履歴が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "23"
               MOVE    "連番数値エラーです"
                                               TO  WRK-ERRMSG
           WHEN    "25"
               MOVE    "削除処理をキャンセルしました。"
                                               TO  WRK-ERRMSG
           WHEN    "30"
               STRING  "選択項目があります。"
                       "選択結果を返却してください。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "60"
               STRING  "解除対象がありません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
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
               MOVE    "保険組合せが存在しません。"
                                               TO  WRK-ERRMSG
           WHEN    "W02"
               MOVE    "最大件数以上の登録があります。"
                                               TO  WRK-ERRMSG
           WHEN    "W03"
               STRING  "診療行為内容が存在しません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *
      *2019.1
           WHEN    "W04"
               STRING  "複数科保険組合せの受診履歴です。"
                       "主たる受診履歴の情報から取得しました。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "W05"
               STRING  "複数科保険組合せの受診履歴に不具合があります。"
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
      *    受診履歴マスター読込
      *****************************************************************
       910-JYURRK-READ-SEC         SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       910-JYURRK-READ-EXT.
           EXIT.
      *
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
      *****************************************************************
      *    一時保存テーブル読み込み
      *****************************************************************
       9100-SPATMP-KEYREAD-SEC        SECTION.
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBSELECT"      TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-SPATMP-READ-SEC
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
      *****************************************************************
      *    一時保存テーブル読み込み
      *****************************************************************
       990-SPATMP-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA2-REC    TO  SPATMP-REC
               MOVE    ZERO            TO  FLG-SPATMP
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
      *
           .
       990-SPATMP-READ-EXT.
           EXIT.
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
