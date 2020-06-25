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
       PROGRAM-ID.         ORAPI023R1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 請求額シュミレーション
      *  管理者            :
      *  作成日付    作業者        記述
      *  12/12/04    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-多々納  14/05/XX  一時ファイル変更
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
      *    パラメタデータ
      *    SELECT  PARA-FILE       ASSIGN  FILE-NAME2
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-PARA.
      *
      *    パラメタデータ
      *    SELECT  PARAK02-FILE    ASSIGN  FILE-NAME22
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-PARA2.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    パラメタデータ
      *FD  PARA-FILE.
      *01  PARA-REC.
      *    03  PARA-KEY.
      *        05  PARA-FILEMEI         PIC X(08).
      *        05  PARA-EDANUM          PIC 9(03).
      *    03  PARA-DATA-REC            PIC X(60000).
      *
      *    画面スパ領域パラメタデータ
      *FD  PARAK02-FILE.
      *    画面スパ領域
      *    COPY    "K02SCR-SPA"      REPLACING
      *                              //SPA-// BY //PARA-SPA-//.
      *
       WORKING-STORAGE             SECTION.
      *
      *    パラメタファイル名
      *01  FILE-NAME2.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(07)   VALUE   "K01PARA".
      *    03  FILE-HOSPNUM2       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID2        PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *    パラメタファイル名
      *01  FILE-NAME22.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(08)   VALUE   "K01PARA2".
      *    03  FILE-HOSPNUM22      PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID22       PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
      *    フラグ領域
      *01  STS-AREA.
      *    03  STS-PARA            PIC X(02).
      *    03  STS-PARA2           PIC X(02).
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "K02SCR-SPA".
      *
      *    ＳＰＡパラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-WKSRYACT            PIC 9(01).
           03  FLG-PARA                PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-END-SYORI           PIC 9(01).
           03  FLG-END-SYORI2          PIC 9(01).
      *
           03  FLG-RSIFLG              PIC 9(01).
           03  FLG-SYOSIN-OK           PIC 9(01).
           03  FLG-SYU40               PIC 9(01).
           03  FLG-KENSADEL            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDY1                PIC 9(04).
           03  IDY2                PIC 9(04).
      *
           03  IDX-ERR             PIC 9(04).
           03  IDX-S               PIC 9(04).
      *
           03  IDX-I               PIC 9(04).
           03  IDX-I1              PIC 9(04).
           03  IDX-I2              PIC 9(04).
      *
           03  IDS                 PIC 9(04).
           03  IDX-S1              PIC 9(04).
      *    福岡県対応用
           03  IDX-SYU             PIC 9(04).
           03  IDX-SYU2            PIC 9(04).
           03  IDX-Y               PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SRYCNT              PIC 9(04).
           03  CNT-ERR                 PIC 9(04).
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
      *    保険組合せ
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-IN-HKNCOMBI         PIC X(04).
      *    エラーコード
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-ERRMSG2             PIC X(100).
           03  WRK-KEICD               PIC X(02).
           03  WRK-ERR-INPUTCD         PIC X(09).
      *
           03  WRK-MCP-PATHNAME        PIC X(62).
           03  WRK-MCP-WINDOW          PIC X(62).
      *
           03  WRK-OPID                PIC X(16).
           03  WRK-TERMID              PIC X(64).
      *
      *     一部負担金計算用
           03  WRK-SYUTEN1             PIC 9(08).
           03  WRK-TEN                 PIC 9(08).
      *     一部負担金計算用
           03  WRK-SYUTEN1-KEI         PIC S9(08).
           03  WRK-LNK-S01-SYUTEN      PIC S9(08).
           03  WRK-SINDAN              PIC S9(07).
      *    粉砕判定用
           03  WRK-FUNSA-AREA.
               05  FLG-FUNSAI          PIC 9(01).
               05  WRK-FUNSAI-CNT      PIC 9(04).
      *    院内、院外判定用
           03  WRK-CHK-SRYSYUKBN       PIC X(03).
      *
      *    金額編集
           03  WRK-KINGAKU             PIC 9(10).
           03  WRK-ZZZ9                PIC ZZZZZZZZ9.
           03  WRK-ZZZZ                PIC ZZZZZZZZZ.
           03  WRK-ZZZ7                PIC ZZZZZZ9.
           03  WRK-ZZ9                 PIC ZZ9.
      *
       01  WRK-XML-AREA.
           03  WRK-PERFORM-DATE        PIC X(10).
           03  WRK-PERFORM-TIME        PIC X(08).
      *
           03  WRK-PERFORM-YMD         PIC X(08).
           03  WRK-PTNUM               PIC X(20).
           03  WRK-PTID                PIC 9(10).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-NAME          PIC X(80).
           03  WRK-TIMEFLG             PIC X(01).
      *
      *    警告メッセージ
       01  TBL-KEIMSG-AREA.
           03  TBL-KEIMSG-G            OCCURS   50.
               05  TBL-KEIERRCD            PIC X(04).
               05  TBL-KEIMSG              PIC X(80).
           03  IDX-KEI                     PIC 9(03).
      *
      *    請求点数名称
       01  TBL-HKNTEN-MEI-AREA.
           03  FILLER              PIC X(30)   VALUE   "初・再診料".
           03  FILLER              PIC X(30)   VALUE   "医学管理等".
           03  FILLER              PIC X(30)   VALUE   "在宅療養".
           03  FILLER              PIC X(30)   VALUE   "投薬".
           03  FILLER              PIC X(30)   VALUE   "注射".
           03  FILLER              PIC X(30)   VALUE   "処置".
           03  FILLER              PIC X(30)   VALUE   "手術".
           03  FILLER              PIC X(30)   VALUE   "麻酔".
           03  FILLER              PIC X(30)   VALUE   "検査".
           03  FILLER              PIC X(30)   VALUE   "画像診断".
           03  FILLER              PIC X(30)   VALUE   "リハビリ".
           03  FILLER              PIC X(30)   VALUE   "精神科専門".
           03  FILLER              PIC X(30)   VALUE   "放射線治療".
           03  FILLER              PIC X(30)   VALUE   "病理診断".
           03  FILLER              PIC X(30)   VALUE   "入院料".
           03  FILLER              PIC X(30)   VALUE   "療養担当手当".
       01  TBL-HKNTEN-MEI-RES      REDEFINES   TBL-HKNTEN-MEI-AREA.
           03  TBL-HKNTEN-MEI      PIC X(30)   OCCURS   16.
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(100).
           03  WRK-ATO-INPUT           PIC X(100).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *
      *    ワーク診療行為マスタ−テーブル
       01  TBL-WKSRYACT-AREA.
           02  TBL-WKSRYACT-REC      OCCURS   40.
           COPY    "CPWKSRYACT.INC"  REPLACING  //WKSRY//
                                     BY         //TBL-WKSRY//.
      *
      *収納計算用
      *    収納マスタワーク
       01  LNK-SYUNOU-AREA.
           02  LNK-SYUNOU-REC      OCCURS   15.
           COPY    "CPSYUNOU.INC"      REPLACING
                                       //SYU-// BY //LNK-SYU-//.
       01  LNK-SYUNOU-MAX              PIC 9(04).
      *
      *    収納マスタワーク２
       01  LNK-SYUNOU2-AREA.
           02  LNK-SYUNOU2-REC      OCCURS   15.
           COPY    "CPSYUNOU.INC"      REPLACING
                                       //SYU-// BY //LNK-SYU2-//.
       01  LNK-SYUNOU2-MAX              PIC 9(04).
      *
      *    診療行為マスタワーク
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC      OCCURS   600.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
      *    附加情報を追加
           03  LNK-WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC"  REPLACING
                                     //WKSRYP-// BY //LNK-WKSRYP-//.
      *
           03  LNK-SORT-SRYKBN        PIC X(02).
       01  LNK-WKSRYACT-MAX           PIC 9(04).
      *
      *処理最大行
       01  MAX-LINE-VALUE          PIC 9(04)   VALUE   400.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
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
      *
           COPY    "CPORCXKANACONV.INC".
      *
           COPY    "CPORCSKANACHK.INC".
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号発生サブ
      *    COPY    "CPORCSP01.INC".
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *    ユーザプログラム起動チェックサブパラメタ
      **** COPY    "CPORCSUSERCHK.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPACSIMULATEV2REQ.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPACSIMULATEV2RES.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
      *    ワーク診療行為データ編集領域
           COPY    "CPORAPISUBWKSRY.INC".
      *
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
      *    点数マスタ編集パラメタ
           COPY    "CPORCSC92.INC".
      *
      *    初期ＳＰＡ画面編集パラメタ
           COPY    "CPORCSC95.INC".
      *
      *    エラーメッセージ編集パラメタ
           COPY    "CPORCSC96.INC".
      *
      *    診療行為内容編集・計算
           COPY    "CPORCSC100.INC".
      *
      *    包括診療チェックパラメタ
           COPY    "CPORCSCHKTCHK.INC".
      *
      *    ワーク診療行為展開・作成
           COPY    "CPORCSCWKSRY.INC".
      *
      *    診察料自動発生パラメタ
      *    COPY    "CPORCSC10S.INC".
      *
      *    負担金額計算用パラメタ
           COPY    "CPORCS01.INC".
      *
      *    保険年齢・負担割合算定サブ
           COPY    "CPORCSAGECHK.INC".
      *福岡県対応用
      *    給付対象外点数編集サブパラメタ
           COPY    "CPORCSSYU40.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    ＵＩＤ取得用エリア
       01  UIDIO-AREA.
           03  C-RET               PIC X(02).
           03  C-UID               PIC X(36).
       01  ST                      PIC 9(2).
      *
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
           COPY  "CPSK1010.INC".
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    収納マスタワーク
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *     パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *    ＳＰＡ一時テーブル
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
           COPY    "MCPDATA2.INC".
      *
      *    入院履歴
      *01  PTNYUINRRK-REC.
      *    COPY    "CPPTNYUINRRK.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "ORCA-BLOB".
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
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "*******************"
           DISPLAY   "* acsimulate start*"
           DISPLAY   "*******************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
           INITIALIZE                      CNT-AREA
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
               MOVE   404                  TO  APILST9-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-APTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "*******************"
           DISPLAY   "* acsimulate end  *"
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
           INITIALIZE                      XML-ACSIMULATERES
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Medical Info"       TO  ACSRES-RESKEY
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
           MOVE    WRK-HEN-DATE        TO  ACSRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  ACSRES-INFORMATION-TIME
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
      *    UIDコード取得処理
           PERFORM 1001-UID-GET-SEC
      *    UIDコードをTERMID
           MOVE    C-UID               TO  WRK-TERMID
           MOVE    SPACE               TO  SPA-TERMID
           STRING  "API+"
                   WRK-TERMID          DELIMITED  BY  SIZE
                                       INTO    SPA-TERMID
           END-STRING
      *
           .
       100-INIT-EXT.
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
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    XML情報取り出し
           PERFORM 1000-APTXMLREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           MOVE    ZERO                TO  ACSRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    APILST9-CLASS
           WHEN    "01"
               MOVE    1                   TO  FLG-SYORI
           END-EVALUATE
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
      *TEST!!
      *        PERFORM 890-ERRCD-MSG-SEC
      *        DISPLAY "必須項目チェックエラー "    WRK-ERRCD
      *                " " WRK-ERRMSG
      *TEST!!
               GO      TO      200-MAIN-EXT
           END-IF
      *
           EVALUATE    APILST9-CLASS
           WHEN    "01"
      *        処理
               PERFORM 2002-WKSRYACT-ADD-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-APTXMLREAD-SEC   SECTION.
      *
           IF      APILST9-BODY     NOT =   LOW-VALUE
               DISPLAY "ACSRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-ACSIMULATEREQ
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_acsimulatev2req"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST9-BODY        TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "acsimulatereq"     TO  APIREQ-RECORDNAME
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
           MOVE    "xml_acsimulatev2req"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST9-BODY        TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "acsimulatereq"     TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-ACSIMULATEREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
      *
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
               IF      FLG-SYORI           =   1
                   MOVE    WRK-KEICD           TO  WRK-ERRCD
               END-IF
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "処理終了"
                                           TO  ACSRES-API-RESULT-MSG
               END-EVALUATE
           ELSE
               IF      WRK-ERRMSG          =   SPACE
      *        エラー・警告メッセージ
                   PERFORM 890-ERRCD-MSG-SEC
               END-IF
               MOVE    WRK-ERRCD           TO  ACSRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  ACSRES-API-RESULT-MSG
           END-IF
      *
           IF      APILST9-BODY     NOT =   LOW-VALUE
               DISPLAY "ACSRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_acsimulatev2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  ACSRES-MODE
           MOVE    "acsimulateres"     TO  ACSRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-ACSIMULATERES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_acsimulatev2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-ACSIMULATERES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE  ACSRES-OBJECT     TO  APILST9-BODY 
               MOVE  "application/xml" TO  APILST9-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       300-APTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
           IF      ACSREQ-PATIENTID    =   LOW-VALUE
               MOVE  SPACE             TO  ACSREQ-PATIENTID
           END-IF
           IF     ACSREQ-PERFORM-DATE  =   LOW-VALUE
               MOVE  SPACE             TO  ACSREQ-PERFORM-DATE
           END-IF
           IF      ACSREQ-APPOINT-DEP-CODE =   LOW-VALUE
               MOVE  SPACE             TO  ACSREQ-APPOINT-DEP-CODE
           END-IF
           IF  ACSREQ-MAIN-INSURANCE-CLASS  =   LOW-VALUE
               MOVE  SPACE             TO  
                                       ACSREQ-MAIN-INSURANCE-CLASS
           END-IF
           IF  ACSREQ-MAIN-INSURANCE-NUMBER =   LOW-VALUE
               MOVE  SPACE             TO  
                                       ACSREQ-MAIN-INSURANCE-NUMBER
           END-IF
           IF  ACSREQ-MAIN-INSURANCE-NAME   =   LOW-VALUE
               MOVE  SPACE             TO  
                                       ACSREQ-MAIN-INSURANCE-NAME
           END-IF
           IF  ACSREQ-MAIN-MARK      =   LOW-VALUE
               MOVE  SPACE             TO  ACSREQ-MAIN-MARK
           END-IF
           IF  ACSREQ-MAIN-NUMBER    =   LOW-VALUE
               MOVE  SPACE             TO  ACSREQ-MAIN-NUMBER
           END-IF
           IF  ACSREQ-MAIN-CONTINUATION     =   LOW-VALUE
               MOVE  SPACE             TO  ACSREQ-MAIN-CONTINUATION
           END-IF
           IF  ACSREQ-MAIN-ASSISTANCE =   LOW-VALUE
               MOVE  SPACE             TO  ACSREQ-MAIN-ASSISTANCE
           END-IF
           IF  ACSREQ-MAIN-FAMILY     =   LOW-VALUE
               MOVE  SPACE             TO  ACSREQ-MAIN-FAMILY
           END-IF
           IF  ACSREQ-MAIN-POLICYHOLDER     =   LOW-VALUE
               MOVE  SPACE             TO  ACSREQ-MAIN-POLICYHOLDER
           END-IF
           IF  ACSREQ-MAIN-START-DATE =   LOW-VALUE
               MOVE  SPACE             TO  ACSREQ-MAIN-START-DATE
           END-IF
           IF  ACSREQ-MAIN-END-DATE   =   LOW-VALUE
               MOVE  SPACE             TO  ACSREQ-MAIN-END-DATE
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
               IF      ACSREQ-INSURANCE-CLASS(IDX)  =   LOW-VALUE
                   MOVE  SPACE             TO  ACSREQ-INSURANCE-CLASS
                                                                  (IDX)
               END-IF
               IF      ACSREQ-INSURANCE-NAME(IDX)    =   LOW-VALUE
                   MOVE  SPACE             TO  
                                       ACSREQ-INSURANCE-NAME(IDX)
               END-IF
               IF      ACSREQ-PROVIDER-NUMBER(IDX)   =   LOW-VALUE
                   MOVE  SPACE             TO  
                                       ACSREQ-PROVIDER-NUMBER(IDX)
               END-IF
               IF      ACSREQ-RECIPIENT-NUMBER(IDX)  =   LOW-VALUE
                   MOVE  SPACE             TO  
                                       ACSREQ-RECIPIENT-NUMBER(IDX)
               END-IF
               IF      ACSREQ-INSURANCE-START-DATE(IDX) =   LOW-VALUE
                   MOVE    SPACE      TO   ACSREQ-INSURANCE-START-DATE
                                                                 (IDX)
               END-IF
               IF      ACSREQ-INSURANCE-END-DATE(IDX)   =   LOW-VALUE
                   MOVE    SPACE       TO  ACSREQ-INSURANCE-END-DATE
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
      *
      *    診療日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PERFORM-CHK-SEC
           END-IF
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PTNUM-CHK-SEC
           END-IF
      *    診療科コードチェック
           IF     (WRK-SRYKA       NOT =   SPACE  )
           AND    (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-SRYKA-CHK-SEC
           END-IF
      *    時間外区分
           IF     (ACSREQ-TIME-CLASS   NOT =   SPACE  )
           AND    (WRK-ERRCD           =   SPACE  )
               IF      ACSREQ-TIME-CLASS   >=  "0"   AND  <=  "8"
                   MOVE    ACSREQ-TIME-CLASS   TO  WRK-TIMEFLG
               ELSE
                   MOVE    "03"                TO  WRK-ERRCD
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
           MOVE    ACSREQ-PATIENTID        TO  ACSRES-PATIENTID
           MOVE    ACSREQ-PATIENTID        TO  WRK-PTNUM
      *    診療科
           MOVE    ACSREQ-APPOINT-DEP-CODE
                                           TO  ACSRES-APPOINT-DEP-CODE
           MOVE    ACSREQ-APPOINT-DEP-CODE
                                           TO  WRK-SRYKA
      *    診療日・時間
           MOVE    ACSREQ-PERFORM-DATE     TO  ACSRES-PERFORM-DATE
      *    数字のみに編集
           MOVE    ACSREQ-PERFORM-DATE     TO  WRK-HEN-DATE
           MOVE    SPACE                   TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-PERFORM-YMD
           MOVE    WRK-THMS                TO  WRK-PERFORM-TIME
      *    未設定時はシステム情報設定
           IF      WRK-PERFORM-YMD         =   SPACE
               MOVE    SMCNDATE-YMD        TO  WRK-PERFORM-YMD
               MOVE    ACSRES-INFORMATION-DATE
                                           TO  ACSRES-PERFORM-DATE
               MOVE    "K1"                TO  WRK-KEICD
           END-IF
      *    診療日を診療日付へ
           MOVE    WRK-PERFORM-YMD     TO  SPA-SRYYMD
           MOVE    WRK-PERFORM-YMD     TO  SPA-SYSYMD
      *    外来のみ
           MOVE    "2"                 TO  SPA-NYUGAIKBN
      *    時間外区分
           MOVE    ACSREQ-TIME-CLASS   TO  ACSRES-TIME-CLASS
      *    必須入力チェック
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
           .
       20010-INPUT-HEN-EXT.
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
               DISPLAY "Accept Ymd =  " WRK-PERFORM-YMD
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
           MOVE    SPTID-PTNUM         TO  ACSRES-PATIENTID
           MOVE    SPTID-PTNUM         TO  WRK-PTNUM
           MOVE    SPTID-PTID          TO  SPA-PTID
           MOVE    SPTID-PTID          TO  WRK-PTID
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
           MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
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
      *    中途終了データ登録処理
      *****************************************************************
       2002-WKSRYACT-ADD-SEC        SECTION.
      *
      *    患者情報内容編集
           PERFORM 20021-ACSRES-HEN-SEC
      *
      *    保険組合せ編集処理
           PERFORM 20023-HKNCOMBI-HEN-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      2002-WKSRYACT-ADD-EXT
           END-IF
      *
      *??  排他チェック & ロック処理
      *    PERFORM 990-LOCK-IN-SEC
      *    IF      SLOCK-RETURN    NOT =   ZERO
      *        MOVE    "90"            TO  WRK-ERRCD
      *        GO      TO      2002-WKSRYACT-ADD-EXT
      *??  END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        診療行為処理
               PERFORM 20021-MAIN-SYORI-SEC
           END-IF
      *
      *    PERFORM 990-LOCK-OUT-SEC
      *    IF      SLOCK-RETURN    NOT =   ZERO
      *        DISPLAY "ロック解除失敗"
      *    END-IF
           .
       2002-WKSRYACT-ADD-EXT.
           EXIT.
      *****************************************************************
      *    患者情報出力情報編集処理
      *****************************************************************
       20021-ACSRES-HEN-SEC     SECTION.
      *
           MOVE    PTINF-NAME          TO  ACSRES-NAME
           MOVE    PTINF-KANANAME      TO  ACSRES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  ACSRES-BIRTHDAY
           MOVE    PTINF-SEX           TO  ACSRES-SEX
      *
           MOVE    WRK-SRYKA-NAME      TO  ACSRES-APPOINT-DEP-NAME
           .
       20021-ACSRES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       20023-HKNCOMBI-HEN-SEC     SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBI
           MOVE    SPACE               TO  WRK-IN-HKNCOMBI
           IF      ACSRES-COMBINATION-NUMBER   NOT =   SPACE
      *        保険組合せチェック
               PERFORM 200311-HKNCOMBI-NUM-SEC
           END-IF
      *    保険組合せサブＣＡＬＬ処理
           PERFORM 200311-ORCSAPIHKN-SYORI-SEC
      *
      *    出力領域保険情報編集
           PERFORM 20023-HKNHENSYU-SEC
           .
       20023-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せサブＣＡＬＬ処理
      *****************************************************************
       200311-ORCSAPIHKN-SYORI-SEC        SECTION.
      *
      *    保険組合せ編集処理
           INITIALIZE                      ORCSAPIHKNAREA
      *    保険組合せなしは、前回分
           MOVE    "1"                 TO  ORCSAPIHKN-KBN
      *    処理区分
      **** MOVE    "1"                 TO  ORCSAPIHKN-KBN2
      *    診療科
           MOVE    WRK-SRYKA           TO  ORCSAPIHKN-SRYKA
      *    保険組合せ
           MOVE    WRK-IN-HKNCOMBI     TO  ORCSAPIHKN-IN-HKNCOMBI
      *    保険種類
           MOVE    ACSREQ-MAIN-INSURANCE-CLASS 
                                       TO  ORCSAPIHKN-IN-HKNNUM
      *    保険者番号
           MOVE    ACSREQ-MAIN-INSURANCE-NUMBER
                                       TO  ORCSAPIHKN-IN-HKNJANUM
      *    保険種類
           MOVE    ACSREQ-MAIN-INSURANCE-NAME 
                                       TO  ORCSAPIHKN-IN-HKNNUM-NAME
      *    記号
           MOVE    ACSREQ-MAIN-MARK    TO  ORCSAPIHKN-IN-MARK
      *    番号
           MOVE    ACSREQ-MAIN-NUMBER
                                       TO  ORCSAPIHKN-IN-NUMBER
      *    有効期間
           MOVE    ACSREQ-MAIN-START-DATE
                                       TO  WRK-HEN-DATE
           MOVE    SPACE               TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD            TO  ORCSAPIHKN-IN-TEKSTYMD
           MOVE    ACSREQ-MAIN-END-DATE
                                       TO  WRK-HEN-DATE
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD            TO  ORCSAPIHKN-IN-TEKEDYMD
      *    公費情報
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
      *        公費種類
               MOVE    ACSREQ-INSURANCE-CLASS (IDX)
                                       TO  ORCSAPIHKN-IN-KOHNUM (IDX)
      *        公費名称
               MOVE    ACSREQ-INSURANCE-NAME  (IDX)
                                       TO  ORCSAPIHKN-IN-KOHNUM-NAME
                                                                 (IDX)
      *        負担者番号
               MOVE    ACSREQ-PROVIDER-NUMBER(IDX)
                                       TO  ORCSAPIHKN-IN-FTNJANUM (IDX)
      *        受給者番号
               MOVE    ACSREQ-RECIPIENT-NUMBER (IDX)
                                       TO  ORCSAPIHKN-IN-JKYSNUM  (IDX)
           END-PERFORM
           CALL    "ORCSAPIHKN"        USING
                                       SPA-AREA
                                       ORCSAPIHKNAREA
           IF      ORCSAPIHKN-OT-HKNCOMBI  =   SPACE
               MOVE    ZERO                    TO  WRK-HKNCOMBI
               MOVE    "12"                    TO  WRK-ERRCD
           ELSE
               MOVE    ORCSAPIHKN-OT-HKNCOMBI  TO  WRK-HKNCOMBI
           END-IF
           .
       200311-ORCSAPIHKN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ番号数値チェック
      *****************************************************************
       200311-HKNCOMBI-NUM-SEC          SECTION.
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    ACSREQ-COMBINATION-NUMBER   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "12"                TO  WRK-ERRCD
               MOVE    ACSREQ-COMBINATION-NUMBER
                                           TO  WRK-HKNCOMBI
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  WRK-HKNCOMBI
           END-IF
           MOVE    WRK-HKNCOMBI        TO  WRK-IN-HKNCOMBI
      *
           .
       200311-HKNCOMBI-NUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力領域保険情報編集処理
      *****************************************************************
       20023-HKNHENSYU-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           IF      WRK-HKNCOMBI    NOT =   ZERO
      *        対象の保険組合せの内容編集
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   20  )
                             OR   (ORCSAPIHKN-THKNCOMBI (IDX)
                                           =   SPACE  )
                   IF      ORCSAPIHKN-THKNCOMBI (IDX)  =   WRK-HKNCOMBI
                       PERFORM 200231-HKNHEN-TBL-SEC
                       MOVE    1                   TO  FLG-OK
                       MOVE    20                  TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *    保険組合せ
           MOVE    WRK-HKNCOMBI        TO  ACSRES-COMBINATION-NUMBER
           .
       20023-HKNHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力領域保険情報編集処理
      *****************************************************************
       200231-HKNHEN-TBL-SEC              SECTION.
      *
      *    保険の種類
           MOVE    ORCSAPIHKN-HKNNUM (IDX)
                                   TO  ACSRES-MAIN-INSURANCE-CLASS
      *    保険者番号
           MOVE    ORCSAPIHKN-HKNJANUM (IDX)
                                   TO  ACSRES-MAIN-INSURANCE-NUMBER
      *    保険の名称
           MOVE    ORCSAPIHKN-HKNNUM-NAME (IDX)
                                   TO  ACSRES-MAIN-INSURANCE-NAME
      *    記号
           MOVE    ORCSAPIHKN-KIGO  (IDX)
                                   TO  ACSRES-MAIN-MARK 
      *    番号
           MOVE    ORCSAPIHKN-NUM  (IDX)
                                   TO  ACSRES-MAIN-NUMBER
      *    継続区分
           MOVE    ORCSAPIHKN-CONTKBN  (IDX)
                                   TO  ACSRES-MAIN-CONTINUATION
      *    補助区分
           MOVE    ORCSAPIHKN-HOJOKBN  (IDX)
                                   TO  ACSRES-MAIN-ASSISTANCE
      *    補助区分名称
           MOVE    ORCSAPIHKN-HOJOKBN-NAME  (IDX)
                                   TO  ACSRES-MAIN-ASSISTANCE-NAME
      *    本人家族区分
           MOVE    ORCSAPIHKN-HONKZKKBN  (IDX)
                                   TO  ACSRES-MAIN-FAMILY
      *    被保険者名
           MOVE    ORCSAPIHKN-HIHKNJANAME  (IDX)
                                   TO  ACSRES-MAIN-POLICYHOLDER
      *    有効開始日
           MOVE    ORCSAPIHKN-TEKSTYMD  (IDX)
                                   TO  ACSRES-MAIN-START-DATE
      *    有効終了日
           MOVE    ORCSAPIHKN-TEKEDYMD  (IDX)
                                   TO  ACSRES-MAIN-END-DATE
      *    労災区分
      *    MOVE    ORCSAPIHKN-RSI-HKNKBN  (IDX)
      *                            TO  WRK-RSI-HKNKBN
      *    公費情報
           PERFORM VARYING     IDY2    FROM    1   BY  1
                   UNTIL       IDY2    >   4
      *        公費の種類
               MOVE    ORCSAPIHKN-KOHNUM (IDX IDY2)
                                       TO  ACSRES-INSURANCE-CLASS
                                                             (IDY2)
      *        公費の種類名称
               MOVE    ORCSAPIHKN-KOHNUM-NAME (IDX IDY2)
                                       TO  ACSRES-INSURANCE-NAME
                                                             (IDY2)
      *        負担者番号
               MOVE    ORCSAPIHKN-FTNJANUM (IDX IDY2)
                                       TO  ACSRES-PROVIDER-NUMBER
                                                             (IDY2)
      *        受給者番号
               MOVE    ORCSAPIHKN-JKYSNUM (IDX IDY2)
                                       TO  ACSRES-RECIPIENT-NUMBER
                                                             (IDY2)
      *        有効開始日
               MOVE    ORCSAPIHKN-KOH-TEKSTYMD (IDX IDY2)
                                   TO  ACSRES-INSURANCE-START-DATE
                                                             (IDY2)
      *        有効終了日
               MOVE    ORCSAPIHKN-KOH-TEKEDYMD (IDX IDY2)
                                       TO  ACSRES-INSURANCE-END-DATE
                                                             (IDY2)
      *        入院−負担率（割）
               MOVE    ORCSAPIHKN-RATE-ADMISSION (IDX IDY2)
                                       TO  ACSRES-RATE-ADMISSION
                                                             (IDY2)
      *        入院−固定額
               MOVE    ORCSAPIHKN-MONEY-ADMISSION (IDX IDY2)
                                       TO  ACSRES-MONEY-ADMISSION
                                                             (IDY2)
      *        外来−負担率（割）
               MOVE    ORCSAPIHKN-RATE-OUTPATIENT (IDX IDY2)
                                       TO  ACSRES-RATE-OUTPATIENT
                                                             (IDY2)
      *        外来−固定額
               MOVE    ORCSAPIHKN-MONEY-OUTPATIENT (IDX IDY2)
                                       TO  ACSRES-MONEY-OUTPATIENT
                                                             (IDY2)
           END-PERFORM
      *
           .
       200231-HKNHEN-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為登録処理
      *****************************************************************
       20021-MAIN-SYORI-SEC        SECTION.
      *
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
      *    ワーク診療行為データ作成
           PERFORM 20021-WKSRYACT-SYORI-SEC
      *
      *    対象なし
           IF      ORAPIWKSRY-ERRCD    NOT =   SPACE
               MOVE    "51"                TO  WRK-ERRCD
               MOVE    ORAPIWKSRY-ERRMSG   TO  WRK-ERRMSG
               GO      TO      20021-MAIN-SYORI-EXT
           END-IF
      *    中途データ展開・点数計算チェックなど
           PERFORM 20011-INPUTSYORI-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 200212-ERRMSG-HEN-SEC
           END-IF
           .
       20021-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為登録処理
      *****************************************************************
       20021-WKSRYACT-SYORI-SEC        SECTION.
      *
           INITIALIZE                  ORAPISUBWKSRYAREA
           MOVE    WRK-HKNCOMBI        TO  ORAPIWKSRY-HKNCOMBI
      *
           PERFORM VARYING     IDX     FROM    1   BY    1
                   UNTIL       IDX     >   40
               MOVE    ACSREQ-MDC-CLASS (IDX)
                                       TO  ORAPIWKSRY-MDC-CLASS
                                                                (IDX)
               MOVE    ACSREQ-MDC-CLASS-NAME (IDX)
                                       TO  ORAPIWKSRY-MDC-CLASS-NAME
                                                                (IDX)
               MOVE    ACSREQ-MDC-CLASS-NUMBER (IDX)
                                       TO  ORAPIWKSRY-MDC-CLASS-NUMBER
                                                                (IDX)
      *        診療内容（レイアウトが同じこと）
               PERFORM VARYING     IDY     FROM    1   BY    1
                       UNTIL       IDY     >   40
                   MOVE    ACSREQ-PRESCRIPTION-INFO (IDX IDY)
                                       TO  ORAPIWKSRY-PRESCRIPTION-INFO
                                                           (IDX IDY)
               END-PERFORM
           END-PERFORM
      *
           MOVE    WRK-TIMEFLG         TO  ORAPIWKSRY-TIMEFLG
           MOVE    "1"                 TO  ORAPIWKSRY-SYORIKBN
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORAPISUBWKSRY"     USING
                                       MCPAREA
                                       SPA-AREA
                                       ORAPISUBWKSRYAREA
      *
           .
       20021-WKSRYACT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    画面用スパ編集処理
      *****************************************************************
       20011-INPUTSYORI-SEC           SECTION.
      *
      *    テーブル最大行
           MOVE    MAX-LINE-VALUE      TO  SPA-MAX-LINE
      *
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-GMN-IDX
           MOVE    ZERO                TO  SPA-NAI-TIMEFLG
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  SPA-ERRCD
      *    診療行為処理
           INITIALIZE                  SPA-K02
           INITIALIZE                  SPA-SCR-REC
      *
           MOVE    WRK-HKNCOMBI        TO  SPA-HKNCOMBINUM
           MOVE    WRK-SRYKA           TO  SPA-SRYKA
           MOVE    WRK-PTID            TO  SPA-PTID
           MOVE    WRK-PTNUM           TO  SPA-PTNUM
      *
      *    中途データ展開処理
           PERFORM 3103-SAIHEN-HEN-SEC
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
                    OR     (SPA-ERRCD          NOT =   SPACE )
               EVALUATE    SPA-GMN-INPUTCD (IDX)(1:1)
                   WHEN    "."
                       CONTINUE
      **           WHEN    "*"
      *                入院期間判定
      **               PERFORM 3101-NYUKIKANCHK-SEC
                   WHEN    OTHER
      *                点数マスタチェック（再表示編集時にエラー）
                       IF      SPA-COM-CUR (IDX)   NOT =   SPACE
                           PERFORM 3100-KIHONCHK-SEC
                       END-IF
      *                電話再診の加算処理なし
                       IF     (SPA-COM-SRYKBN(IDX) =   "12"  )
                         AND  (SPA-COM-INPUTCD(IDX)(1:1) =  "1")
                           MOVE    "1"         TO  SPA-COM-KSNAUTO(IDX)
                       END-IF
               END-EVALUATE
           END-PERFORM
      *
      *    診療行為、計計算処理、編集
           PERFORM 510-TBLHEN-SYORI-SEC
      *
      *    包括の判定
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-KIDCD           =   SPACE )  AND
                  (SPA-HKNCOMBINUM NOT =   "9999")
      *            治験以外
             AND  (SPA-CHIKENKBN   NOT =   "1"   )
               IF     (SPA-HKTSANTEI-FLG   =   1     )  OR
                      (SPA-HKTSANTEI-08    =   1     )  OR
                      (SPA-NYUHKTCHK       =   1     )
                   PERFORM 5109-HKTSANTEI-CHK-SEC
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
      *        更新前自動算定・更新追加
               PERFORM 530-TBLCHI-SYORI-SEC
      *        更新処理
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 600-KOUSIN-SYORI-SEC
              END-IF
           END-IF
      *    データ削除処理
           PERFORM 700-FILE-DELETE-SEC
           .
       20011-INPUTSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   画面ＳＰＡ編集処理
      *****************************************************************
       3103-SAIHEN-HEN-SEC                  SECTION.
      *
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
      *    保険組合せ詳細編集
           INITIALIZE                  ORCSC95AREA
           MOVE    "H"                 TO  LNK-SC95-KBN
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
      *    年齢計算
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
           MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
           MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
           MOVE    SPA-HKNCOMBINUM     TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           MOVE    AGECHK-NENREI2-YY   TO  SPA-NENREI-YY
           MOVE    AGECHK-NENREI2-MM   TO  SPA-NENREI-MM
           MOVE    AGECHK-NENREI2-DD   TO  SPA-NENREI-DD
      *
           INITIALIZE                  ORCSCWKSRYAREA
           MOVE    "1"                 TO  ORCSCWKSRY-KBN
           MOVE    "K02"               TO  ORCSCWKSRY-PG
      *    処理区分（１：中途終了展開）
           MOVE    1                   TO  ORCSCWKSRY-SYORIFLG
      *    追加開始位置
           MOVE    1                   TO  ORCSCWKSRY-STRIDX
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORCSCWKSRY"        USING
                                       ORCSCWKSRYAREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           IF      ORCSCWKSRY-ERRCD    NOT =   SPACE
               MOVE    ORCSCWKSRY-ERRCD    TO  SPA-ERRCD
           END-IF
      *
           .
       3103-SAIHEN-HEN-EXT.
           EXIT.
      *****************************************************************
      *   点数マスタチェック処理
      *****************************************************************
       3100-KIHONCHK-SEC                  SECTION.
      *
      *    点数マスタ編集
           INITIALIZE                  ORCSC92AREA
           MOVE    SPACE               TO  LNK-SC92-KBN
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    "K02"               TO  LNK-SC92-PG
           MOVE    IDX                 TO  LNK-SC92-GMN-IDX
           MOVE    SPA-COM-INPUTCD(IDX)
                                       TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           PERFORM VARYING     IDX-ERR FROM    1  BY  1
                   UNTIL      (IDX-ERR     >   10     )  OR
                              (SPA-ERRCD   NOT =   SPACE)
               IF      LNK-SC92-ERRCD(IDX-ERR) NOT =   SPACE
                   MOVE    LNK-SC92-ERRCD(IDX-ERR)
                                                   TO  SPA-ERRCD
               END-IF
           END-PERFORM
      *    警告は登録対象
           IF      SPA-ERRCD(1:1)      =   "K"
               MOVE    ZERO                TO  FLG-ERR
               PERFORM 30010-KEIMSGSYORI-SEC
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
           IF     (SPA-ERRCD           NOT =   SPACE )
               MOVE    "1"             TO  SPA-COM-CUR (IDX)
           END-IF
           .
       3100-KIHONCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為、計計算処理、編集処理
      *****************************************************************
       510-TBLHEN-SYORI-SEC            SECTION.
      *
      *    腫瘍マーカー一覧展開しないとする
           MOVE    ZERO                TO  SPA-AKUSEIHYOFLG
      *    診療行為、計計算処理、編集
           MOVE    ZERO                TO  FLG-END-SYORI
           MOVE    SPACE               TO  SPA-ERRMSG2
           MOVE    ZERO                TO  FLG-KENSADEL
           PERFORM
               UNTIL   (FLG-END-SYORI  =   1  )
               OR      (SPA-ERRCD      NOT =   SPACE )
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    SPACE               TO  SPA-KIDCD
               MOVE    SPACE               TO  SPA-ERRMSG2
      *
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
               INITIALIZE                  ORCSC100AREA
               MOVE    "1"                 TO  ORCSC100-KBN
               MOVE    SPACE               TO  ORCSC100-TOROKU
               MOVE    ZERO                TO  ORCSC100-INIT
               MOVE    FLG-KENSADEL        TO  ORCSC100-KENSADEL
               MOVE    ZERO                TO  ORCSC100-RIHTEI
               CALL    "ORCSC100"          USING
                                           ORCSC100AREA
                                           SPA-AREA
                                           SPA-K02
                                           SPA-SCR-REC
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
               MOVE    ZERO                TO  FLG-KENSADEL
      *        確認メッセージはすべてOKとする
               IF      SPA-KIDCD        NOT =   SPACE
                   MOVE    "OK"             TO  SPA-KID1-FLG
                   PERFORM 5301-KIDCD-CHK-SEC
               ELSE
      *            初診算定日なしは、継続する
                   IF     (SPA-ERRCD           =   "1058" )
                     AND  (SPA-SYOYMD          =   SPACE  )
                       MOVE    SPA-SYSYMD      TO  SPA-SYOYMD
                       MOVE    SPACE           TO  SPA-ERRCD
                   END-IF
      *            警告は登録対象
                   IF      SPA-ERRCD(1:1)      =   "K"
                       MOVE    ZERO                TO  FLG-ERR
                       PERFORM 30010-KEIMSGSYORI-SEC
                       MOVE    ZERO            TO  FLG-END-SYORI
                   ELSE
                       MOVE    1               TO  FLG-END-SYORI
                   END-IF
               END-IF
           END-PERFORM
           .
       510-TBLHEN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療行為自動算定・算定チェック処理
      *****************************************************************
       530-TBLCHI-SYORI-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-END-SYORI2
           PERFORM
               UNTIL    FLG-END-SYORI2 =   1
      *
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
               INITIALIZE                  ORCSC100AREA
               MOVE    "2"                 TO  ORCSC100-KBN
               MOVE    "1"                 TO  ORCSC100-TOROKU
               MOVE    ZERO                TO  ORCSC100-INIT
               MOVE    ZERO                TO  ORCSC100-KENSADEL
               MOVE    ZERO                TO  ORCSC100-RIHTEI
               CALL    "ORCSC100"          USING
                                           ORCSC100AREA
                                           SPA-AREA
                                           SPA-K02
                                           SPA-SCR-REC
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *        確認メッセージはすべてOKとする
               IF      SPA-KIDCD        NOT =   SPACE
                   MOVE    "OK"             TO  SPA-KID1-FLG
                   PERFORM 5301-KIDCD-CHK-SEC
               ELSE
      *        警告は登録対象
                   IF      SPA-ERRCD(1:1)      =   "K"
                       MOVE    ZERO                TO  FLG-ERR
                       PERFORM 30010-KEIMSGSYORI-SEC
                       MOVE    ZERO                TO  FLG-END-SYORI2
                   ELSE
                       MOVE    1                   TO  FLG-END-SYORI2
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       530-TBLCHI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    確認メッセージチェック処理 (K02と同じ処理）
      *****************************************************************
       5301-KIDCD-CHK-SEC              SECTION.
      *
           EVALUATE    SPA-KIDCD
      *        特定疾患処方管理加算の算定確認
               WHEN    "0113"
               WHEN    "0213"
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    2                   TO  SPA-TOKUTEIFLG
                   ELSE
                       MOVE    1                   TO  SPA-TOKUTEIFLG
                   END-IF
      *        長期投与加算の算定確認
               WHEN    "0130"
               WHEN    "0230"
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    2                   TO  SPA-CHOKITOFLG
                   ELSE
                       MOVE    1                   TO  SPA-CHOKITOFLG
                   END-IF
      *        手帳加算の算定確認
               WHEN    "2003"
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    2                   TO  SPA-TECHOKBN-CHK
                   ELSE
                       MOVE    1                   TO  SPA-TECHOKBN-CHK
                   END-IF
      *        同一検査削除へ
               WHEN    "0160"
                   MOVE    1                   TO  FLG-KENSADEL
      *        内服７種類以上逓減確認
               WHEN    "2001"
               WHEN    "2002"
               WHEN    "2004"
               WHEN    "2005"
                   IF      SPA-KID1-FLG        =   "OK"
                      MOVE    1                TO  SPA-NAIFUKU-OK
                   ELSE
                      MOVE    ZERO             TO  SPA-NAIFUKU-OK
                   END-IF
      *H26.10
      *        向精神薬剤多剤投与
               WHEN    "2006"
                   IF      SPA-KID1-FLG        =   "OK"
                      MOVE    1                TO  SPA-PSYCHO-OK
                   ELSE
                      MOVE    ZERO             TO  SPA-PSYCHO-OK
                   END-IF
      *H27.4
      *        低紹介率３０日以上投与
               WHEN    "2007"
                   IF      SPA-KID1-FLG        =   "OK"
                      MOVE    1                TO  SPA-TEIGEN30-OK
                   ELSE
                      MOVE    ZERO             TO  SPA-TEIGEN30-OK
                   END-IF
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-KIDCD
           MOVE    SPACE               TO  SPA-KID1-FLG
           .
       5301-KIDCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    包括診療チェック処理
      *****************************************************************
       5109-HKTSANTEI-CHK-SEC              SECTION.
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                  ORCSCHKTCHKAREA
           MOVE    SPA-NYUHKTCHK       TO  ORCSCHKTCHK-NYUHKTCHK
           MOVE    SPA-NYUHKTCD        TO  ORCSCHKTCHK-NYUHKTCD
           MOVE    SPA-NYUHKTSRYCD     TO  ORCSCHKTCHK-NYUSRYCD
           MOVE    SPA-NYUHKT-G        TO  ORCSCHKTCHK-NYUHKT-G
           MOVE    "6"                 TO  ORCSCHKTCHK-SYORI
           MOVE    "K02"               TO  ORCSCHKTCHK-PG
           CALL    "ORCSCHKTCHK"       USING
                                       ORCSCHKTCHKAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       ORCSC00AREA
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           IF      ORCSCHKTCHK-HKTFLG  =   1
               MOVE    1               TO  SPA-HKTSANTEI-CHK
           ELSE
               MOVE    ZERO            TO  SPA-HKTSANTEI-CHK
           END-IF
           IF      ORCSCHKTCHK-HKTERR  =   1
      *        包括をエラーとする削除処理
               PERFORM 51091-HKTSANTEI-DEL-SEC
           END-IF
           .
       5109-HKTSANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括診療エラー削除処理
      *****************************************************************
       51091-HKTSANTEI-DEL-SEC              SECTION.
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                  ORCSCHKTCHKAREA
           MOVE    SPA-NYUHKTCHK       TO  ORCSCHKTCHK-NYUHKTCHK
           MOVE    SPA-NYUHKTCD        TO  ORCSCHKTCHK-NYUHKTCD
           MOVE    SPA-NYUHKTSRYCD     TO  ORCSCHKTCHK-NYUSRYCD
           MOVE    SPA-NYUHKT-G        TO  ORCSCHKTCHK-NYUHKT-G
           MOVE    "3"                 TO  ORCSCHKTCHK-SYORI
           MOVE    "K02"               TO  ORCSCHKTCHK-PG
           CALL    "ORCSCHKTCHK"       USING
                                       ORCSCHKTCHKAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       ORCSC00AREA
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *    診療行為、計計算処理、編集
           PERFORM 510-TBLHEN-SYORI-SEC
           .
       51091-HKTSANTEI-DEL-EXT.
           EXIT.
      *****************************************************************
      *    収納編集処理
      *****************************************************************
       600-KOUSIN-SYORI-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA
      *
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
           INITIALIZE                  LNK-WKSRYACT-AREA
           MOVE    ZERO                TO  IDX
      *
           MOVE    SPACE               TO  LNK-SYUNOU2-AREA
           INITIALIZE                  LNK-SYUNOU2-AREA
           INITIALIZE                  LNK-SYUNOU2-MAX
      *
      **** MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *    OPEN    INPUT               PARA-FILE
      *
      *    IF      STS-PARA           =  ZERO
      *        PERFORM 980-PARA-READ-SEC
      *    ELSE
      *        MOVE    1               TO  FLG-PARA 
      *    END-IF
      *    PERFORM
      *            UNTIL      (FLG-PARA    =    1   )  OR
      *                       (IDX         >=   600 )
      *        ワーク診療行
      *        IF      PARA-FILEMEI    =   "WKSRYACT"
      *            ADD     1                   TO  IDX
      *            MOVE    PARA-DATA-REC       TO  LNK-WKSRYACT-REC
      *                                                         (IDX)
      *            MOVE    IDX                 TO  LNK-WKSRYACT-MAX
      *        END-IF
      *        収納
      *        IF      PARA-FILEMEI        =   "SYUNOU"
      *            ADD     1                   TO  LNK-SYUNOU2-MAX
      *            MOVE    PARA-DATA-REC       TO  LNK-SYUNOU2-REC
      *                                            (LNK-SYUNOU2-MAX)
      *        END-IF
      *
      *        PERFORM 980-PARA-READ-SEC
      *    END-PERFORM
      *
      **** CLOSE                       PARA-FILE
      *
      *ver.4.8
           MOVE    SPACE               TO  PARA-REC
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                      PARA-REC
               MOVE    1                   TO  FLG-PARA
           END-IF
           PERFORM
               UNTIL  (FLG-PARA            =   1     )
      *        ワーク診療行
               IF     (PARA-FILEMEI    =   "WKSRYACT")
               AND    (IDX             <   600       )
                   ADD     1                   TO  IDX
                   MOVE    PARA-DATA-REC       TO  LNK-WKSRYACT-REC
                                                                (IDX)
                   MOVE    IDX                 TO  LNK-WKSRYACT-MAX
               END-IF
      *        収納
               IF      PARA-FILEMEI        =   "SYUNOU"
                   ADD     1                   TO  LNK-SYUNOU2-MAX
                   MOVE    PARA-DATA-REC       TO  LNK-SYUNOU2-REC
                                                   (LNK-SYUNOU2-MAX)
               END-IF
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *負担金計算
      *    収納は１件
           MOVE    LNK-SYUNOU2-REC (LNK-SYUNOU2-MAX)
                                       TO  SYUNOU-REC
      *    初期設定処理
           PERFORM 450121-GAIFTN-SYOKI-SEC
      *
      *    薬剤一部負担金計算処理
           PERFORM 4505-YAKFTN-KEI-SEC
      *    福岡公費用
           MOVE    ZERO                TO  FLG-SYU40
      *
      *    複数科まとめ集計フラグ
           MOVE    SPA-KAGRPFLG        TO  LNK-S01-KAGRPFLG
      *    負担金計算処理
           CALL    "ORCSGAIFTN"        USING
                                       MCPAREA
                                       LNK-ORCS01-REC
                                       LNK-SYUNOU2-AREA
                                       SPA-AREA
      *
      *    収納は１件
           MOVE    LNK-SYUNOU2-REC (1)
                                       TO  SYUNOU-REC
      *    収納情報出力編集
           PERFORM 6002-ACSRES-SYUNOU-HEN-SEC
      *
          .
       600-KOUSIN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    収納情報出力編集処理
      *****************************************************************
       6002-ACSRES-SYUNOU-HEN-SEC             SECTION.
      *
      *    割引率
           IF      SYU-DISCOUNT-RATE   NOT =   ZERO
               EVALUATE    SYU-DISCOUNT-TEIRITU
                   WHEN    "1"
      *                定率
                       MOVE    SYU-DISCOUNT-RATE   TO  WRK-ZZ9
                       STRING  WRK-ZZ9
                               "％"            DELIMITED  BY  SIZE
                                               INTO  ACSRES-RATE
                       END-STRING
                   WHEN    "2"
      *                定額
                       MOVE    SYU-DISCOUNT-RATE   TO  WRK-ZZZ7
                       STRING  WRK-ZZZ7
                               "円"            DELIMITED  BY  SIZE
                                               INTO  ACSRES-RATE
                       END-STRING
               END-EVALUATE
           END-IF
      *
      *    請求金額
           MOVE    SYU-SKYMONEY        TO  WRK-ZZZ9
           MOVE    WRK-ZZZ9            TO  ACSRES-SKYMONEY
      *    保険適用金額
           MOVE    SYU-HKNTEKMONEY     TO  WRK-ZZZ9
           MOVE    WRK-ZZZ9            TO  ACSRES-HKNTEKMONEY
      *    自費合計金額
           COMPUTE WRK-KINGAKU         =   SYU-JIHI-TOTAL-TAX
                                       +   SYU-JIHI-TOTAL
      *                                保険適応外
                                       +   SYU-TGMONEY (17)
                                       +   SYU-TGMONEY-TAX (17)
           MOVE    WRK-KINGAKU         TO  WRK-ZZZ9
           MOVE    WRK-ZZZ9            TO  ACSRES-JIHIMONEY
      *    老人一部負担金
           MOVE    SYU-RJN-FTN         TO  WRK-ZZZZ
           MOVE    WRK-ZZZZ            TO  ACSRES-RJN-FTN
      *    公費一部負担金
           MOVE    SYU-KOH-FTN         TO  WRK-ZZZZ
           MOVE    WRK-ZZZZ            TO  ACSRES-KOH-FTN
      *    薬剤一部負担金
           MOVE    SYU-YKZ-FTN         TO  WRK-ZZZZ
           MOVE    WRK-ZZZZ            TO  ACSRES-YKZ-FTN
      *    保険合計点数
           MOVE    SYU-TOTAL-HKNTEN    TO  WRK-ZZZ9
           MOVE    WRK-ZZZ9            TO  ACSRES-TOTAL-HKNTEN
      *    労災・自賠責金額
           COMPUTE WRK-KINGAKU         =   SYU-RSISHOSHIN-MONEY
                                       +   SYU-RSISAISHIN-MONEY
                                       +   SYU-RSISDO-MONEY
                                       +   SYU-RSIETC-MONEY
           MOVE    WRK-KINGAKU         TO  WRK-ZZZZ
           MOVE    WRK-ZZZZ            TO  ACSRES-RSI-TOTALMONEY
      *    保険点数明細
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   16
      *        手術料・療養担当手当は、点数ありのみ
               IF     (IDX             =   15   OR  16  )
                  AND (SYU-HKNTEN (IDX)    =   ZERO     )
                   CONTINUE
               ELSE
      *
                   MOVE    TBL-HKNTEN-MEI(IDX)
                                           TO  ACSRES-HKNTEN-MEI (IDX)
                   MOVE    SYU-HKNTEN(IDX) TO  WRK-ZZZ9
                   MOVE    WRK-ZZZ9        TO  ACSRES-HKNTEN (IDX)
               END-IF
           END-PERFORM
          .
       6002-ACSRES-SYUNOU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    負担金計算初期設定処理
      *****************************************************************
       450121-GAIFTN-SYOKI-SEC             SECTION.
      *
           INITIALIZE                  LNK-ORCS01-REC
           MOVE    ZERO                TO  IDX-Y
      *
      *
           MOVE    ZERO                TO  IDY
           ADD     1                   TO  IDX-Y
      *
           MOVE    ZERO                TO  FLG-FUNSAI
           MOVE    ZERO                TO  WRK-FUNSAI-CNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-WKSRYACT-MAX
      *        剤毎にクリアする
               IF     (IDX                 >   1    )  AND
                      (LNK-WKSRY-ZAINUM (IDX) NOT =
                                           LNK-WKSRY-ZAINUM (IDX - 1))
                   MOVE    ZERO            TO  FLG-FUNSAI
                   MOVE    ZERO            TO  WRK-FUNSAI-CNT
               END-IF
      *
               IF     (LNK-WKSRY-HKNCOMBI (IDX)    =   "9999")
                   OR (LNK-WKSRY-CONTKBN  (IDX)    =   "H"   )
      *           薬評以外
                   OR (LNK-WKSRY-ZAIKBN   (IDX)    =   1    )
                   CONTINUE
               ELSE
      *            診療内容編集（薬剤と服用方法)（未使用）
                   PERFORM VARYING     IDS     FROM    1   BY  1
                           UNTIL      (IDS         >   5   )  OR
                                      (LNK-WKSRY-SRYCD(IDX IDS)
                                                   =   SPACE )
                       PERFORM 45051-ORCS01-SET-SEC
      *                特定公費（診察料未算定扱い）判定
                       IF      LNK-WKSRY-SRYCD(IDX IDS)
                                                   =   "099409905"
                           MOVE    "1"         TO  LNK-S01-SINSATUFLG2
                       END-IF
      *                特定公費（時間外受診）判定
                       IF      LNK-WKSRY-SRYCD(IDX IDS)
                                                   =   "099409907"
                           MOVE    "1"         TO  LNK-S01-JIKANGAI
                       END-IF
      *H30.1
      *                第三者行為分計算優先フラグ
                       IF      LNK-WKSRY-SRYCD(IDX IDS)
                                                   =   "099999931"
                           MOVE    "1"     TO  LNK-S01-DAISANYUSENFLG
                       END-IF
      *H30.11
      *                初診料算定フラグ２（地方公費用）
                       IF      LNK-WKSRY-SRYCD(IDX IDS)
                                                   =   "099409906"
                           MOVE    "1"     TO  LNK-S01-SYOSINFLG2
                       END-IF
      *
      *                初診料算定判定
                       PERFORM 450112-SYOSIN-CHK-SEC
                   END-PERFORM
               END-IF
      *        診察料算定フラグ（地方公費用）
               IF      LNK-WKSRY-SRYKBN (IDX)  =   "11"
                                               OR  "12"
                                               OR  "13"
                                               OR  "14"
                   MOVE    "1"                 TO  LNK-S01-SINSATUFLG
               END-IF
           END-PERFORM
      *
      *Ｈ１４．１０以降
      *    高齢者（在宅総合・在宅末期）算定患者区分
           IF     ( SPA-SRYYMD         <   "20021001")  OR
                  ((SPA-NETAKIRI-ARI   =   ZERO )  AND
                   (SPA-NETAKIRI-ARI2  =   ZERO )  AND
                   (SPA-ZAIMATU-ARI    =   ZERO )  AND
                   (SPA-ZAIMATU-ARI2   =   ZERO )  AND
      *            Ｈ１８．４追加
                   (SPA-ZAIKANRI-ARI   =   ZERO )  AND
                   (SPA-ZAIKANRI-ARI2  =   ZERO )
      *            Ｈ２０．４追加（特定施設入居時医学総合管理料）
              AND  (SPA-IGAKUKAN-ARI   =   ZERO )
              AND  (SPA-IGAKUKAN-ARI2  =   ZERO )  )
               CONTINUE
           ELSE
      *        Ｈ１９．４から年令に関係なくすべて対象（以前の処理なし）
      ***      IF     ( SPA-SRYYMD         >=  "20070401")
                   MOVE    "1"             TO
                                           LNK-S01-KOUREI-ZAITAKU-KBN
      *****    END-IF
           END-IF
      *    初診料算定フラグ（地方公費用）
           IF      FLG-SYOSIN-OK       =   1
               MOVE    "1"             TO  LNK-S01-SYOSINFLG
           END-IF
      *
           .
       450121-GAIFTN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    初診料算定判定処理
      *****************************************************************
       450112-SYOSIN-CHK-SEC             SECTION.
      *
           IF      LNK-WKSRY-SRYKBN(IDX)   =   "11"  OR  "13"
               EVALUATE    LNK-WKSRY-SRYCD  (IDX IDS)
      *                初診料
                       WHEN    "111000110"
                       WHEN    "111003610"
                       WHEN    "111700110"
                       WHEN    "111700210"
                       WHEN    "113003510"
                       WHEN    "113003710"
                       WHEN    "101110010"
      *                ダミー追加
                       WHEN    "099110001"
      *                紹介状なし追加
                       WHEN    "111012510"
      *H27.1           妥結率５割以下
                       WHEN    "111012710"
      *R02.4
      *               初診料（新型コロナウイルス感染症・臨時）
                       WHEN    "111013850"
                           MOVE    1             TO  FLG-SYOSIN-OK
                   END-EVALUATE
           END-IF
           .
       450112-SYOSIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一部負担額計算用セット処理
      *****************************************************************
       45051-ORCS01-SET-SEC            SECTION.
      *
      *    粉砕の時最初の１剤のみとする
           IF      LNK-WKSRY-SRYCD(IDX IDS)    =   "099209901"
               ADD     1                   TO  WRK-FUNSAI-CNT
               MOVE    1                   TO  FLG-FUNSAI
               GO      TO      45051-ORCS01-SET-EXT
           END-IF
           IF     (FLG-FUNSAI          =   1   )  AND
                  (WRK-FUNSAI-CNT      >   1   )
               MOVE    ZERO                TO  FLG-FUNSAI
               GO      TO      45051-ORCS01-SET-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-FUNSAI
           IF     (LNK-WKSRY-SRYCD(IDX IDS)(1:1) =   "6"  ) OR
                  (LNK-WKSRY-SRYCD(IDX IDS)(1:3) =   "001")
      *        院内薬剤のみとする
               MOVE    LNK-WKSRY-SRYSYUKBN (IDX) TO
                                                   WRK-CHK-SRYSYUKBN
               IF      WRK-CHK-SRYSYUKBN       =   SPACE
                   EVALUATE    LNK-WKSRY-SRYKBN (IDX)
                       WHEN    "21"
                           MOVE    "210"     TO  WRK-CHK-SRYSYUKBN
                       WHEN    "22"
                           MOVE    "220"     TO  WRK-CHK-SRYSYUKBN
                       WHEN    "23"
                           MOVE    "230"     TO  WRK-CHK-SRYSYUKBN
                   END-EVALUATE
               END-IF
               IF     (WRK-CHK-SRYSYUKBN           =   "211"  OR
                                                       "221"  OR
                                                       "231"  OR
                                                       "291" )  OR
                     ((SPA-INGAIKBN            NOT =   "1"   ) AND
                      (WRK-CHK-SRYSYUKBN           =   "210"  OR
                                                       "220"  OR
                                                       "230"  OR
                                                       "290" ))
                   ADD     1                   TO  IDY
                   IF      IDY                 >   40
                       GO      TO      45051-ORCS01-SET-EXT
                   END-IF
                   MOVE    LNK-WKSRY-SRYKBN(IDX)
                                                   TO  LNK-S01-SRYKBN
                                                          (IDX-Y IDY)
                   MOVE    LNK-WKSRY-ZAINUM (IDX)
                                                   TO  LNK-S01-ZAIBAN
                                                          (IDX-Y IDY)
                   MOVE    LNK-WKSRY-SRYCD (IDX IDS)
                                                   TO  LNK-S01-SRYCD
                                                          (IDX-Y IDY)
                   MOVE    LNK-WKSRY-SRYSURYO(IDX IDS)
                                                   TO  LNK-S01-SURYOU
                                                          (IDX-Y IDY)
                   MOVE    LNK-WKSRY-ZAIKAIKEI(IDX)
                                                   TO  LNK-S01-KAISUU
                                                          (IDX-Y IDY)
                   MOVE    LNK-WKSRY-ZAITENKEI(IDX)
                                                   TO  LNK-S01-TENSUU
                                                          (IDX-Y IDY)
               END-IF
           END-IF
      *
           .
       45051-ORCS01-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤一部負担金計算処理
      *****************************************************************
       4505-YAKFTN-KEI-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-LNK-S01-SYUTEN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-WKSRYACT-MAX
      *        包括剤対象外
               IF      LNK-WKSRY-CONTKBN (IDX) =   "H"
                   CONTINUE
               ELSE
      *
      *            自賠責の時、技術点を集計する
                   IF      (SPA-HKNKBN         =   1   )  AND
                           (SPA-RSI-HKNKBN     =   "4"
                                               OR  "5"
      *                    第三者行為追加
                                               OR  "6"  ) 
                       PERFORM 45052-JIBAI-SYORI-SEC
                   END-IF
      *            公害保険の時、技術点を集計する
                   IF      (SPA-HKNKBN         =   2   )  AND
                           (LNK-WKSRY-SYUTEN1 (IDX)    NOT =  ZERO )
                       PERFORM 45052-KOGAI-SYORI-SEC
                   END-IF
               END-IF
           END-PERFORM
           MOVE    WRK-LNK-S01-SYUTEN  TO  LNK-S01-SYUTEN
                                                   (LNK-SYUNOU2-MAX)
      *
      *    船員保険で初診料算定時
           IF     (SYU-SYUHKNNUM       =   "002" )  AND
                  (SYU-HKNTEN (01)     >   ZERO  )  AND
                  (FLG-SYOSIN-OK       =   1     )
               PERFORM 45053-SENINFLG-SET-SEC
           END-IF
      *
      *福岡県公費対応
      *    福岡県で、一般保険算定時
           IF     (SPA-PREFNUM         =   40   )  AND
                  (SPA-HKNKBN          =   ZERO )
               PERFORM 45054-PREFNUM-40-SEC
           END-IF
      *
           .
       4505-YAKFTN-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責の時、技術点を集計処理
      *****************************************************************
       45052-JIBAI-SYORI-SEC          SECTION.
      *
      *    診察料と診断料等の時、診療コードを判定する
           MOVE    ZERO                TO  WRK-SYUTEN1
           IF      LNK-WKSRY-SRYKBN (IDX)  =   "11"  OR  "12"  OR
                                               "13"  OR  "80"
               PERFORM VARYING     IDS     FROM    1   BY  1
                       UNTIL      (IDS         >   5   )  OR
                                  (LNK-WKSRY-SRYCD(IDX IDS)
                                              =   SPACE )
      *
                   MOVE    ZERO                TO  FLG-RSIFLG
                   MOVE    LNK-WKSRY-SRYCD (IDX IDS)   TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
      *            労災の金額算定分の時、点数をゼロとする
                   IF     (LNK-WKSRY-SRYCD (IDX IDS)(1:3)
                                               =   "101" ) AND
                          (TNS-TENSIKIBETU     =   1    )
                       MOVE    1                   TO  FLG-RSIFLG
                   END-IF
      *            自賠責で証明料等の時、点数をゼロとする
                   IF     (LNK-WKSRY-SRYKBN(IDX)       =   "80")  AND
                          (LNK-WKSRY-SRYCD (IDX IDS)(1:5)
                                                       =   "09591" OR
                                                           "09592" OR
                                                           "09593" OR
                                                           "09594" )
                       MOVE    1                   TO  FLG-RSIFLG
                   END-IF
      *            自賠責金額入力
                   IF      LNK-WKSRY-SRYSYUKBN(IDX)    =   "809"
                       MOVE    LNK-WKSRY-JIHIMONEY(IDX IDS)
                                                   TO  TNS-TEN
                   END-IF
      *
                   IF      FLG-RSIFLG          =   1
                       COMPUTE WRK-TEN         =   TNS-TEN   /   10
                       ADD     WRK-TEN         TO  WRK-SYUTEN1
                   END-IF
      *            自賠責金額集計
                   IF      LNK-WKSRY-SRYKBN(IDX)       =   "80"
      *                自賠責診断書料集計
                       IF      LNK-WKSRY-SRYCD (IDX IDS)(1:5)
                                                       =   "09591"
                           COMPUTE WRK-SINDAN  =   TNS-TEN
                                               *   LNK-WKSRY-ZAIKAIKEI
                                                               (IDX)
                           ADD     WRK-SINDAN      TO  LNK-S01-SINDAN
                                                      (LNK-SYUNOU2-MAX)
                       END-IF
      *                自賠責明細書料集計
                       IF      LNK-WKSRY-SRYCD (IDX IDS)(1:5)
                                                       =   "09592"
                           COMPUTE WRK-SINDAN  =   TNS-TEN
                                               *   LNK-WKSRY-ZAIKAIKEI
                                                               (IDX)
                           ADD     WRK-SINDAN      TO  LNK-S01-MEISAI
                                                      (LNK-SYUNOU2-MAX)
                       END-IF
      *                自賠責その他のコード編集
                       IF      LNK-WKSRY-SRYCD (IDX IDS)(1:5)
                                                       =   "09591"
                                                       OR  "09592"
                                                       OR  "09593"
                           ADD     1               TO  IDX-S1
                           IF      IDX-S1          <=  20
                               MOVE    LNK-WKSRY-SRYCD (IDX IDS)
                                           TO   LNK-S01-SONOTA-SRYCD
                                                (LNK-SYUNOU2-MAX IDX-S1)
                               MOVE    LNK-WKSRY-ZAIKAIKEI (IDX)
                                           TO   LNK-S01-SONOTA-ZAIKAISU
                                                (LNK-SYUNOU2-MAX IDX-S1)
                               MOVE    TNS-TEN
                                           TO   LNK-S01-SONOTA-TANKA
                                                (LNK-SYUNOU2-MAX IDX-S1)
                           END-IF
                       END-IF
                   END-IF
      *            療養担当手当の点数をゼロとする
                   IF     (LNK-WKSRY-SRYKBN(IDX)       =   "80")  AND
                          (LNK-WKSRY-SRYCD (IDX IDS)   =   "199000610")
                       ADD     TNS-TEN         TO  WRK-SYUTEN1
      *                自賠責療養担当手当点数
                       COMPUTE WRK-SINDAN      =   TNS-TEN
                                               *   LNK-WKSRY-ZAIKAIKEI
                                                               (IDX)
                       ADD     WRK-SINDAN      TO  LNK-S01-RYOYO
                                                      (LNK-SYUNOU2-MAX)
                   END-IF
      *H25.7
      *            救急医療管理加算点数
                   IF     (SPA-SRYYMD                  >=  "20130701")
                     AND  (LNK-WKSRY-SRYKBN(IDX)       =   "80"      )
                     AND  (LNK-WKSRY-SRYCD (IDX IDS)   =   "101800890")
      *                自賠責療養担当手当点数
                       COMPUTE WRK-SINDAN      =   TNS-TEN
                                               *   LNK-WKSRY-ZAIKAIKEI
                                                               (IDX)
                       MOVE    WRK-SINDAN      TO  LNK-S01-KYUKYU
                                                      (LNK-SYUNOU2-MAX)
                   END-IF
      *
      *
               END-PERFORM
           END-IF
      *
      *    薬剤料逓減は対象外
           IF      LNK-WKSRY-SRYCD (IDX 01)    =   "630010002"
               GO      TO      45052-JIBAI-SYORI-EXT
           END-IF
      *
      *    画像診断の時、フィルム点数を対象外とする
           IF      LNK-WKSRY-SRYKBN (IDX)  =   "70"
               MOVE    LNK-WKSRY-SYUTEN2 (IDX)     TO  WRK-SYUTEN1
           END-IF
      *
      *    腰部固定帯加算を器材点数として対象外とする
      *    自賠責固定帯加算等取扱を判定する
           IF     (SYS-4001-KOTEIKBN    =   "2"  OR   SPACE )  AND
                  (LNK-WKSRY-SRYKBN(IDX)(1:1)  NOT =   "9"  )
               PERFORM VARYING     IDS     FROM    1   BY  1
                       UNTIL      (IDS         >   5   )  OR
                                  (LNK-WKSRY-SRYCD(IDX IDS)
                                              =   SPACE )
                   IF     LNK-WKSRY-SRYCD(IDX IDS)     =   "140037490"
                                                       OR  "140040110"
                                                       OR  "150266970"
      *                    頸部固定帯加算
                                                       OR  "140052610"
                       MOVE    LNK-WKSRY-SRYCD (IDX IDS)   TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                       ADD     TNS-TEN         TO  WRK-SYUTEN1
                   END-IF
               END-PERFORM
           END-IF
      *
      *    自賠責手技点合計
           IF      LNK-WKSRY-SYUTEN1 (IDX) <   WRK-SYUTEN1
               CONTINUE
           ELSE
               COMPUTE WRK-SYUTEN1-KEI     =  (LNK-WKSRY-SYUTEN1 (IDX)
                                           -   WRK-SYUTEN1  )
                                           *   LNK-WKSRY-ZAIKAIKEI
                                                             (IDX)
               IF      LNK-WKSRY-ZAIKBN (IDX) =   2
      *            減点分
                   COMPUTE WRK-SYUTEN1-KEI     =   WRK-SYUTEN1-KEI
                                               *   -1
               END-IF
               COMPUTE WRK-LNK-S01-SYUTEN  =   WRK-LNK-S01-SYUTEN
                                           +   WRK-SYUTEN1-KEI
           END-IF
      *
           .
       45052-JIBAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公害保険の時、技術点を集計処理
      *****************************************************************
       45052-KOGAI-SYORI-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-SYUTEN1
      *
      *    薬剤料逓減は対象外
           IF      LNK-WKSRY-SRYCD (IDX 01)    =   "630010002"
               GO      TO      45052-KOGAI-SYORI-EXT
           END-IF
      *
      *    画像診断の時、フィルム点数を対象外とする
           IF      LNK-WKSRY-SRYKBN (IDX)  =   "70"
               MOVE    LNK-WKSRY-SYUTEN2 (IDX)     TO  WRK-SYUTEN1
           END-IF
      *
      *    公害固有のコードを対象外とする
           IF     (LNK-WKSRY-SRYKBN(IDX)(1:1)  NOT =   "9"  )
               PERFORM VARYING     IDS     FROM    1   BY  1
                       UNTIL      (IDS         >   5   )  OR
                                  (LNK-WKSRY-SRYCD(IDX IDS)
                                              =   SPACE )
                   IF     LNK-WKSRY-SRYCD(IDX IDS)(1:3)
                                                   =   "102"
                       MOVE    LNK-WKSRY-SRYCD (IDX IDS)   TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                       ADD     TNS-TEN         TO  WRK-SYUTEN1
                   END-IF
               END-PERFORM
           END-IF
      *    自賠責手技点合計
           IF      LNK-WKSRY-SYUTEN1 (IDX) <   WRK-SYUTEN1
               CONTINUE
           ELSE
               COMPUTE WRK-SYUTEN1-KEI     =  (LNK-WKSRY-SYUTEN1 (IDX)
                                           -   WRK-SYUTEN1  )
                                           *   LNK-WKSRY-ZAIKAIKEI
                                                             (IDX)
               IF      LNK-WKSRY-ZAIKBN (IDX) =   2
      *            減点分
                   COMPUTE WRK-SYUTEN1-KEI     =   WRK-SYUTEN1-KEI
                                               *   -1
               END-IF
               COMPUTE WRK-LNK-S01-SYUTEN  =   WRK-LNK-S01-SYUTEN
                                           +   WRK-SYUTEN1-KEI
           END-IF
      *
           .
       45052-KOGAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    船員保険で初診料算定時セット処理
      *****************************************************************
       45053-SENINFLG-SET-SEC            SECTION.
      *
      *    本人で通勤災害
           IF     (ACSRES-MAIN-FAMILY      =   "1"  )  AND
                  (ACSRES-MAIN-ASSISTANCE  =   "3"  OR
                                               "C"  OR
                                               "F"  OR
                                               "I"   )
               MOVE    "1"             TO  LNK-S01-SENINFLG
                                                      (LNK-SYUNOU2-MAX)
           END-IF
           .
       45053-SENINFLG-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    福岡県公費対応処理
      *****************************************************************
       45054-PREFNUM-40-SEC            SECTION.
      *
           MOVE    ZERO                TO  IDX-SYU
           MOVE    ZERO                TO  IDX-SYU2
           INITIALIZE                  ORCSSYU40AREA
           MOVE    "1"                 TO  ORCS40-KBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   LNK-WKSRYACT-MAX )  OR
                              (IDX-SYU >=  5                )
      *
      *    IF     (LNK-WKSRY-SRYYMD (IDX)      =   SPACE ) OR
      *           (LNK-WKSRY-HKNCOMBI(IDX) NOT =   SYU-HKNCOMBINUM)
      *       OR  ((SYU-SRYKA              NOT =   ZERO  )  AND
      *           (LNK-WKSRY-SRYKA   (IDX) NOT =   SYU-SRYKA  ))
      *        CONTINUE
      *****ELSE
      *        剤毎にクリアする
               IF     (IDX                 >   1    )  AND
                      (LNK-WKSRY-ZAINUM (IDX) NOT =
                                           LNK-WKSRY-ZAINUM (IDX - 1))
                   MOVE    ZERO            TO  IDX-SYU2
               END-IF
      *
      *        初診・往診、小児科外来診察料の初診時
               IF     (LNK-WKSRY-SRYKBN (IDX)  =   "11"
                                               OR  "14" )  OR
                     ((SPA-SYONIKA-ARI         =   1    )  AND
                      (SPA-SYOSIN              =   1    )  AND
                      (LNK-WKSRY-SRYKBN (IDX)  =   "13"
                                               OR  "99"  ))
      *            連番が１の時、テーブル編集
                   IF      LNK-WKSRY-RENNUM (IDX)  =   1
                       ADD     1                   TO  IDX-SYU
                       MOVE    LNK-WKSRY-SRYKBN (IDX)  TO  ORCS40-SRYKBN
                                                           (IDX-SYU)
                   END-IF
                   MOVE    LNK-WKSRY-ZAITENKEI (IDX)   TO  ORCS40-ZAITEN
                                                           (IDX-SYU)
      *            診療内容編集
                   PERFORM VARYING     IDS     FROM    1   BY  1
                           UNTIL      (IDS         >   5   )  OR
                                      (LNK-WKSRY-SRYCD(IDX IDS)
                                                   =   SPACE )  OR
                                      (IDX-SYU2    >=  5     )
                       IF     (LNK-WKSRY-SRYCD(IDX IDS)(1:1) =   "1") OR
                              (LNK-WKSRY-SRYCD(IDX IDS) =   "099114001")
                            ADD     1              TO  IDX-SYU2
                            MOVE    LNK-WKSRY-SRYCD(IDX IDS)
                                                   TO  ORCS40-SRYCD
                                                     (IDX-SYU IDX-SYU2)
                       END-IF
                   END-PERFORM
      *
      ******** END-IF
      *
               END-IF
           END-PERFORM
      *
      *    対象があった場合
           IF      IDX-SYU             >   ZERO
      *        給付対象外点数編集
               CALL    "ORCSSYU40"     USING
                                       SPA-AREA
                                       ORCSSYU40AREA
                                       SYUNOU-REC
      *        初診料と同日初診がある時、初診料のみ
               IF      FLG-SYU40           =   1
                   COMPUTE SYU-KYUFUGAI-GOKEI-TEN
                                           =   SYU-KYUFUGAI-GOKEI-TEN
                                           -   SYU-KYUFUGAI-SHOSHIN-TEN
                   MOVE    ZERO            TO  SYU-KYUFUGAI-SHOSHIN-TEN
               END-IF
               IF      SYU-KYUFUGAI-SHOSHIN-TEN    >   ZERO
                   MOVE    1               TO  FLG-SYU40
               END-IF
           ELSE
      *        診療区分別給付外点数クリア
               INITIALIZE              SYU-KYUFUGAI
           END-IF
      *
           .
       45054-PREFNUM-40-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       200212-ERRMSG-HEN-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-ERR-INPUTCD
           MOVE    SPACE               TO  WRK-ERRMSG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
      *
               IF      SPA-COM-CUR (IDX)   =   "1"   OR  "2"
                   MOVE    SPA-COM-INPUTCD (IDX)
                                           TO  WRK-ERR-INPUTCD
      *            診療種別がエラー
                   IF      SPA-GMN-INPUTCD (IDX)(1:1)
                                           =   "."
                       MOVE   SPA-GMN-MEISYO(IDX)
                                           TO  WRK-ERR-INPUTCD
                   END-IF
                   MOVE    SPA-MAX-LINE    TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    "K02"               TO  LNK-SC96-PG
           CALL    "ORCSC96"           USING    SPA-AREA
                                                ORCSC96AREA
           MOVE    "50"                TO  WRK-ERRCD
           IF      WRK-ERR-INPUTCD     =   SPACE
               STRING  SPA-ERRMSG          DELIMITED  BY  SPACE
                       " "                 DELIMITED  BY  SIZE
                       SPA-ERRMSG2         DELIMITED  BY  SPACE
                                           INTO    WRK-ERRMSG
               END-STRING
           ELSE
               STRING  "コード："
                       WRK-ERR-INPUTCD     DELIMITED  BY  SIZE
                       "　"                DELIMITED  BY  SIZE
                       SPA-ERRMSG          DELIMITED  BY  SPACE
                       " "                 DELIMITED  BY  SIZE
                       SPA-ERRMSG2         DELIMITED  BY  SPACE
                                           INTO    WRK-ERRMSG
               END-STRING
           END-IF
      *
           .
       200212-ERRMSG-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ削除処理
      *****************************************************************
       700-FILE-DELETE-SEC          SECTION.
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *
      *    MOVE    ZERO                TO  IF-RESULT
      *    MOVE    FILE-NAME2          TO  IF-FILENAME
      *    CALL    "orcfiledel"        USING
      *                                ORCSFDELAREA
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID22
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM22
      *    MOVE    ZERO                TO  IF-RESULT
      *    MOVE    FILE-NAME22         TO  IF-FILENAME
      *    CALL    "orcfiledel"        USING
      ***                              ORCSFDELAREA
      *
      *    一時保存ファイル削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "K02"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
      *
           .
       700-FILE-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    警告メッセージ処理
      *****************************************************************
       30010-KEIMSGSYORI-SEC          SECTION.
      *
           IF      FLG-ERR             =   2
               MOVE    SPA-ERRMSG2         TO  SPA-ERRMSG
               MOVE    SPACE               TO  SPA-ERRMSG2
           ELSE
               MOVE    "K02"               TO  LNK-SC96-PG
               CALL    "ORCSC96"           USING    SPA-AREA
                                                    ORCSC96AREA
           END-IF
           ADD     1                   TO  IDX-KEI
           IF      IDX-KEI             <=  50
               MOVE    SPA-ERRCD           TO  TBL-KEIERRCD (IDX-KEI)
               IF      SPA-ERRMSG2     NOT =   SPACE
                   MOVE    SPA-ERRMSG2         TO  TBL-KEIMSG (IDX-KEI)
                   ADD     1                   TO  IDX-KEI
               END-IF
               IF      IDX-KEI             <=  50
                   MOVE    SPA-ERRMSG          TO  TBL-KEIMSG (IDX-KEI)
               END-IF
           END-IF
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-ERRMSG
           MOVE    SPACE               TO  SPA-ERRMSG2
           .
       30010-KEIMSGSYORI-EXIT.
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
               MOVE    "患者番号の設定がありません"
                                               TO  WRK-ERRMSG
           WHEN    "02"
               MOVE    "診療科の設定がありません"
                                               TO  WRK-ERRMSG
           WHEN    "03"
               MOVE    "時間外加算区分が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "10"
               MOVE    "患者番号に該当する患者が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "診療日が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "12"
      ******   MOVE    "保険組合せ誤り"
               MOVE    "該当する保険組合せがありません"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE    "診療科が存在しません"
                                               TO  WRK-ERRMSG
      *
      *警告
           WHEN    "K1"
               MOVE    "診療日を設定しました"
                                               TO  WRK-ERRMSG
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
           MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
           MOVE    "K02"               TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    1                   TO  SLOCK-MODE
           MOVE    PTINF-PTID          TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
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
           MOVE    "K02"               TO  SPA-MOTOPG
                                           MCP-WINDOW
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
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタデータ順読み
      *****************************************************************
      *980-PARA-READ-SEC         SECTION.
      *
      *    READ    PARA-FILE 
      *        AT  END
      *            MOVE    1           TO  FLG-PARA
      *        NOT AT  END
      *            MOVE    ZERO        TO  FLG-PARA
      *    END-READ
      *
      *    .
      *980-PARA-READ-EXT.
      **** EXIT.
      *
      *v.4.8
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
