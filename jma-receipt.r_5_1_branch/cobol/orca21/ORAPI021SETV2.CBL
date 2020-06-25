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
       PROGRAM-ID.         ORAPI021SETV2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 診療行為　セット登録　（xml2）
      *  管理者            :
      *  作成日付    作業者        記述
      *  15/11/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
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
           03  FLG-PARA                PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-INPUTCD             PIC 9(01).
           03  FLG-INPUTSET            PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-ERR2                PIC 9(01).
           03  FLG-KEIMSG              PIC 9(01).
           03  FLG-SET-ARI             PIC 9(01).
           03  FLG-SET-OK              PIC 9(01).
           03  FLG-HENCHK              PIC 9(01).
           03  FLG-TAISYO              PIC 9(01).
           03  FLG-TOROKU              PIC 9(01).
           03  FLG-KAISU               PIC 9(01).
           03  FLG-SP                  PIC 9(01).
           03  FLG-AUTOKBN2            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
           03  IDK                 PIC 9(04).
           03  IDXM                PIC 9(04).
           03  IDXR                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-A               PIC 9(04).
           03  IDX-SET             PIC 9(04).
           03  IDX-ERR             PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SETCNT              PIC 9(04).
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
           03  WRK-ERRCD-G.
               05  WRK-ERRKBN              PIC X(01).
               05  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-ERRMSG1             PIC X(100).
           03  WRK-ERRMSG2             PIC X(100).
           03  WRK-ERRMSG-02           PIC X(100).
      *
      *    内容エラー
           03  WRK-MD-ERRCD            PIC X(04).
           03  WRK-MD-ERRMSG.
               05  WRK-MD-ERRMSG1          PIC X(100).
               05  WRK-MD-ERRMSG2          PIC X(100).
           03  WRK-MD-ERR-LINE         PIC X(03).
           03  WRK-MD-ERR-IDX          PIC X(02).
           03  WRK-MD-SRYCD            PIC X(09).
      *    警告メッセージ
           03  WRK-KEIMSG.
               05  WRK-KEIMSG1             PIC X(100).
               05  WRK-KEIMSG2             PIC X(100).
      *
           03  WRK-ERR-IDX             PIC 9(03).
      *
           03  WRK-MCP-PATHNAME        PIC X(62).
           03  WRK-MCP-WINDOW          PIC X(62).
      *
           03  WRK-OPID                PIC X(16).
      *    MCP-TERMID
           03  WRK-TERMID              PIC X(64).
      *
           03  WRK-KAISU           PIC 9(04).
      *
           03  WRK-SRYSYUKBN           PIC X(03).
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-SRYSYUKBNMEI        PIC X(40).
           03  WRK-SRYCD               PIC X(09).
           03  WRK-ERR-SRYCD           PIC X(09).
           03  WRK-ERR-INPUTCD         PIC X(09).
      *    剤内容
           03  WRK-KINGAK              PIC 9(08).
      *
           03  WRK-TOROKU              PIC X(01).
           03  WRK-SEL-YUKOSTYMD       PIC X(08).
           03  WRK-SEL-YUKOEDYMD       PIC X(08).
           03  WRK-SET-RRK             PIC 9(03).
      *
      *    数値変換用
           03  WRK-SURYO               PIC S9(08)V9(05).
           03  WRK-SURYO-S             PIC 9(08)V9(05).
           03  WRK-SURYO-R             REDEFINES   WRK-SURYO-S.
               05  WRK-SURYO-S1        PIC 9(08).
               05  WRK-SURYO-S2        PIC 9(05).
           03  WRK-SURYO2-S1           PIC S9(08).
      *
           03  WRK-SURYO-XX            PIC X(15).
           03  WRK-SURYO-X             REDEFINES    WRK-SURYO-XX.
               05  WRK-SURYO-Z1        PIC -(08)9.
               05  WRK-SURYO-Z2        PIC X(01).
               05  WRK-SURYO-Z3        PIC ZZZZZ.
           03  WRK-SURYO-HX            PIC X(15).
           03  WRK-S-MAX               PIC 9(02).
      *
           03  WRK-MAX-ZAINUM          PIC 9(08).
      *
           03  WRK-H-LINE              PIC X(03).
           03  WRK-H-IDX               PIC X(02).
      *
       01  WRK-XML-AREA.
           03  WRK-BASE-DATE           PIC X(10).
           03  WRK-BASE-YMD            PIC X(08).
      *
           03  WRK-START-DATE          PIC X(10).
           03  WRK-END-DATE            PIC X(10).
      *    有効期間
           03  WRK-YUKOSTYMD           PIC X(08).
           03  WRK-YUKOEDYMD           PIC X(08).
      *
      *    エラーメッセージ
       01  TBL-ERRMSG-AREA.
           03  TBL-ERRMSG-G            OCCURS  400.
               05  TBL-ERRERRCD            PIC X(04).
               05  TBL-ERRMSG.
                   07  TBL-ERRMSG1             PIC X(100).
                   07  TBL-ERRMSG2             PIC X(100).
               05  TBL-ERR-SRYCD           PIC X(09).
               05  TBL-ERR-LINE            PIC X(03).
               05  TBL-ERR-IDX             PIC X(02).
           03  IDX-MDE                     PIC 9(03).
      *
      *
      *    警告メッセージ
       01  TBL-KEIMSG-AREA.
           03  TBL-KEIMSG-G            OCCURS  400.
               05  TBL-KEIERRCD            PIC X(04).
               05  TBL-KEIMSG.
                   07  TBL-KEIMSG1             PIC X(100).
                   07  TBL-KEIMSG2             PIC X(100).
               05  TBL-KEI-SRYCD           PIC X(09).
               05  TBL-KEI-LINE            PIC X(03).
               05  TBL-KEI-IDX             PIC X(02).
           03  IDX-KEI                     PIC 9(03).
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(200).
           03  WRK-ATO-INPUT           PIC X(200).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *
      *処理最大行
       01  MAX-LINE-VALUE          PIC 9(04)   VALUE   400.
      *受取剤数
       01  COB-ZAI-INMAX           PIC 9(04)   VALUE   40.
      *返却剤数
       01  COB-ZAI-OUTMAX          PIC 9(04)   VALUE   40.
      *明細数
       01  COB-MEI-MAX             PIC 9(04)   VALUE   40.
      *
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
      *    画面ＳＰＡ領域ワーク
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *
      *    保存ＳＰＡ領域ワーク
       01  WRK-HZN-SCR-AREA.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-HZN-//.
        01  WRK-HOZ-IDX                PIC 9(04).
      *
      *    中途データ
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC          OCCURS   400.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
       01  LNK-WKSRYACT-MAX           PIC 9(04).
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
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "CPORCXKANACONV.INC".
      *
           COPY    "CPORCSKANACHK.INC".
      *    全角チェックパラメタ（拡張漢字）
      ***  COPY    "CPORCSKANACHK2.INC".
      *
      *    拡張文字対応
           COPY    "CPORCSSTRING.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPMDCSETV2REQ.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPMDCSETV2RES.INC".
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
      *    点数マスタ編集パラメタ
           COPY    "CPORCSC92.INC".
      *
      *    点数マスタ編集パラメタ
           COPY    "CPORCSC93.INC".
      *
      *    初期ＳＰＡ画面編集パラメタ
           COPY    "CPORCSC95.INC".
           COPY    "CPORCSC94.INC".
      *
      *    エラーメッセージ編集パラメタ
           COPY    "CPORCSC96.INC".
      *
      *    診療行為内容編集・計算
      *    COPY    "CPORCSC100.INC".
      *
      *    ワーク診療行為データ編集領域
           COPY    "CPORAPI021SUB1V3.INC".
      *
      *    診療内容編集サブ領域
           COPY    "CPORAPI021SUB2V3.INC".
      *
      *    ワーク診療行為展開・作成
           COPY    "CPORCSCWKSRY.INC".
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
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    COPY    "CPSYSKANRI.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    入力セットコード
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
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
           COPY    "ORCA21SCRAREA.INC".
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
           DISPLAY   "* medicalsetv2 start*"
           DISPLAY   "*********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
           INITIALIZE                      CNT-AREA
           INITIALIZE                      TBL-ERRMSG-AREA
           INITIALIZE                      TBL-KEIMSG-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *
           IF      WRK-ERRCD           =   "99"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-APTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "*********************"
           DISPLAY   "* medicalsetv2 end  *"
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
           INITIALIZE                      XML-MEDICALSETRES
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
           MOVE    SPACE               TO  SPA-MOTOPG
      *
      *    XML情報取り出し
           PERFORM 1000-APTXMLREAD-SEC
      *
      *    ＳＰＡ共通設定
           IF      WRK-ERRCD           =   SPACE
               PERFORM 1001-ORCSCOMMON-SEC
           END-IF
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
      *    診療日チェック
           IF      MDCREQ-BASE-DATE    NOT =   SPACE
               MOVE    MDCREQ-BASE-DATE    TO  WRK-HEN-DATE
               MOVE    SPACE               TO  WRK-HEN-TIME
               PERFORM 802-DAYHEN02-SEC
               MOVE    WRK-SYMD            TO  WRK-BASE-YMD
      *        診療日チェック
               PERFORM 20011-BASE-DATE-CHK-SEC
      *        診療日をシステム日付へ
               IF      WRK-ERRCD           =   SPACE
                   MOVE    WRK-BASE-YMD        TO  SPA-SYSYMD
               END-IF
               MOVE    SPACE               TO  WRK-ERRCD
           END-IF
      *
           MOVE    SPACE               TO  SPA-MOTOPG
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
           .
       1001-ORCSCOMMON-EXT.
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
           MOVE    ZERO                TO  MDCRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    MDCREQ-REQUEST-NUMBER
           WHEN    "01"
      *        登録
               MOVE    1                   TO  FLG-SYORI
           WHEN    "02"
      *        削除
               MOVE    2                   TO  FLG-SYORI
           WHEN    "03"
      *        終了日変更
               MOVE    3                   TO  FLG-SYORI
           WHEN    "04"
      *        セット内容返却
               MOVE    4                   TO  FLG-SYORI
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
           END-EVALUATE
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
      *        診療行為内容編集
               PERFORM 200101-MDCHENSYU-SEC
               GO      TO      200-MAIN-EXT
           END-IF
      *
           EVALUATE    MDCREQ-REQUEST-NUMBER
           WHEN    "01"
      *        登録処理
               PERFORM 2002-INPUTSET-ADD-SEC
           WHEN    "02"
      *        削除処理
               PERFORM 2003-INPUTSET-DEL-SEC
           WHEN    "03"
      *        終了日変更
               PERFORM 2004-INPUTSET-ENDUPD-SEC
           WHEN    "04"
      *        内容取得
               PERFORM 2005-INPUTSET-HENKAN-SEC
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
           IF      APILST-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-MEDICALSETREQ
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalsetv2req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST-BODY         TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalsetreq"     TO  APIREQ-RECORDNAME
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
           MOVE    "xml_medicalsetv2req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST-BODY         TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalsetreq"     TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-MEDICALSETREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
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
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "登録処理終了"
                                           TO  MDCRES-API-RESULT-MSG
                   WHEN    2
                       MOVE    "削除処理終了"
                                           TO  MDCRES-API-RESULT-MSG
                   WHEN    3
                       MOVE    "終了日更新処理終了"
                                           TO  MDCRES-API-RESULT-MSG
                   WHEN    4
                       MOVE    "処理終了"
                                           TO  MDCRES-API-RESULT-MSG
               END-EVALUATE
               IF      FLG-KEIMSG          =   1
      *            警告メッセージあり
                   MOVE    "W"                 TO  WRK-ERRKBN
                   MOVE    ZERO                TO  WRK-ERRCD
                   MOVE    WRK-ERRCD-G         TO  MDCRES-API-RESULT
               END-IF
           ELSE
      *        エラーメッセージ
               IF      WRK-ERRMSG          =   SPACE
                   PERFORM 890-ERRCD-MSG-SEC
               END-IF
               IF      WRK-ERRKBN          =   SPACE
                   MOVE    "E"             TO  WRK-ERRKBN
               END-IF
      *
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
      *    診療内容エラー・警告編集
           PERFORM 3001-WAINGMSG-HEN-SEC
      *
           IF      APILST-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalsetv2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalsetres"     TO  MDCRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALSETRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_medicalsetv2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalsetres"     TO  MDCRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALSETRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE  MDCRES-OBJECT     TO  APILST-BODY 
               MOVE  "application/xml" TO  APILST-CONTENT-TYPE
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
           MOVE    ZERO                TO  IDX
      *
           IF      WRK-MD-ERRCD    NOT =   SPACE
               MOVE    1                   TO  IDX
               MOVE    WRK-MD-ERRCD        TO  MDCRES-MEDI-RESULT
                                                            (IDX)
               MOVE    WRK-MD-ERRMSG       TO  MDCRES-MEDI-RESULT-MSG
                                                            (IDX)
               MOVE    WRK-MD-ERR-LINE     TO  MDCRES-MEDI-POSITION
                                                            (IDX)
               MOVE    WRK-MD-ERR-IDX      TO  MDCRES-MEDI-ITEM-POSITION
                                                            (IDX)
               MOVE    WRK-MD-SRYCD        TO  MDCRES-MEDI-CODE
                                                            (IDX)
           END-IF
           PERFORM VARYING     IDK     FROM    1   BY  1
                   UNTIL      (IDK     >   50     )   OR
                              (TBL-ERRERRCD (IDK)  =   SPACE)
               ADD     1                   TO  IDX
               MOVE    TBL-ERRERRCD (IDK)  TO  MDCRES-MEDI-RESULT(IDX)
               MOVE    TBL-ERRMSG   (IDK)  TO  MDCRES-MEDI-RESULT-MSG
                                                               (IDX)
               MOVE    TBL-ERR-LINE (IDK)  TO  MDCRES-MEDI-POSITION
                                                               (IDX)
               MOVE    TBL-ERR-IDX  (IDK)  TO  MDCRES-MEDI-ITEM-POSITION
                                                               (IDX)
               MOVE    TBL-ERR-SRYCD(IDK)  TO  MDCRES-MEDI-CODE (IDX)
           END-PERFORM
      *
           PERFORM VARYING     IDK     FROM    1   BY  1
                   UNTIL      (IDK     >   50     )   OR
                              (TBL-KEIERRCD (IDK)  =   SPACE)
      *        ワーニングコード
               MOVE    TBL-KEIERRCD (IDK)  TO  MDCRES-MEDI-WARNING (IDK)
               MOVE    TBL-KEIMSG   (IDK)  TO  MDCRES-WARNING-MSG  (IDK)
               MOVE    TBL-KEI-LINE (IDK)  TO  MDCRES-WRG-POSITION (IDK)
               MOVE    TBL-KEI-IDX  (IDK)  TO  MDCRES-WRG-ITEM-POSITION
                                                                 (IDK)
               MOVE    TBL-KEI-SRYCD(IDK)  TO  MDCRES-WRG-MEDI-CODE(IDK)
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
      *    編集処理
           PERFORM 20010-INPUT-HEN-SEC
      *
      *    基準日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-BASE-DATE-CHK-SEC
           END-IF
      *    セットＣＤチェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-SETCD-CHK-SEC
           END-IF
      *    セット名称チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-SETNAME-CHK-SEC
           END-IF
      *    開始日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-START-DATE-CHK-SEC
           END-IF
      *    終了日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-END-DATE-CHK-SEC
           END-IF
      *
           MOVE    WRK-BASE-YMD        TO  SPA-SYSYMD
           MOVE    WRK-BASE-YMD        TO  SPA-SRYYMD
      *    開始日がシステム日付より後の時、開始日を診療日
           IF      WRK-YUKOSTYMD       >   SPA-SYSYMD
               MOVE    WRK-YUKOSTYMD       TO  SPA-SRYYMD
           END-IF
      *    終了日がシステム日付より前の時、終了日を診療日
           IF      WRK-YUKOEDYMD       <   SPA-SYSYMD
               MOVE    WRK-YUKOEDYMD       TO  SPA-SRYYMD
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
      *    基準日チェック
           MOVE    MDCREQ-BASE-DATE     TO  WRK-HEN-DATE
           MOVE    SPACE                TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD             TO  WRK-BASE-YMD
      *    未設定時はシステム情報設定
           IF      WRK-BASE-YMD         =  SPACE
               MOVE    SMCNDATE-YMD        TO  WRK-BASE-YMD
           END-IF
           MOVE    WRK-BASE-YMD        TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MDCRES-BASE-DATE
      *
      *    セットＣＤ
           MOVE    MDCREQ-SETCD        TO  MDCRES-SETCD
           MOVE    MDCREQ-SETCD-NAME   TO  MDCRES-SETCD-NAME
      *    開始日
           MOVE    MDCREQ-START-DATE   TO  MDCRES-START-DATE
      *    数字編集
           IF      MDCREQ-START-DATE   =   SPACE
               MOVE    ALL "0"             TO  WRK-YUKOSTYMD
               MOVE    WRK-YUKOSTYMD       TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  MDCRES-START-DATE
           ELSE
               MOVE    MDCREQ-START-DATE   TO  WRK-HEN-DATE
               MOVE    ZERO                TO  WRK-HEN-TIME
               PERFORM 802-DAYHEN02-SEC
               MOVE    WRK-SYMD            TO  WRK-YUKOSTYMD
           END-IF
      *    終了日
           MOVE    MDCREQ-END-DATE     TO  MDCRES-END-DATE
           IF      MDCREQ-END-DATE     =   SPACE
               MOVE    ALL "9"             TO  WRK-YUKOEDYMD
               MOVE    WRK-YUKOEDYMD       TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  MDCRES-END-DATE
           ELSE
               MOVE    MDCREQ-END-DATE     TO  WRK-HEN-DATE
               MOVE    ZERO                TO  WRK-HEN-TIME
               PERFORM 802-DAYHEN02-SEC
               MOVE    WRK-SYMD            TO  WRK-YUKOEDYMD
           END-IF
      *    入外区分
           IF      MDCREQ-OUTPATIENT-CLASS =  "I"
               MOVE    "1"                 TO  SPA-NYUGAIKBN
           ELSE
               MOVE    "2"                 TO  SPA-NYUGAIKBN
           END-IF
      *
      *    必須入力チェック
      *    セットＣＤ
           IF      MDCREQ-SETCD        =   SPACE
               MOVE    "01"                TO  WRK-ERRCD
           END-IF
           .
       20010-INPUT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    基準日付チェック
      *****************************************************************
       20011-BASE-DATE-CHK-SEC          SECTION.
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
               MOVE    "02"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-BASE-DATE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    セットＣＤチェック
      *****************************************************************
       20011-SETCD-CHK-SEC          SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    MDCREQ-SETCD        TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC          =   ZERO  )  AND
                  (KANACHK-SYUBETU     =   1     )  AND
                  (KANACHK-RC2         =   ZERO  )  AND
                  (KANACHK-MAX         =   6     )
      *        半角6文字であること
               CONTINUE
           ELSE
               MOVE    "03"                TO  WRK-ERRCD
           END-IF
           IF      MDCREQ-SETCD(1:1)       =   "P"
               CONTINUE
           ELSE
               IF      MDCREQ-SETCD(1:1)       =   "S"
                   IF     (FLG-SYORI               =   1
                                                   OR  3   )
                       MOVE    "09"                TO  WRK-ERRCD
                   END-IF
               ELSE
                   MOVE    "03"                TO  WRK-ERRCD
               END-IF
           END-IF
           IF     (MDCREQ-SETCD(4:3)   NOT NUMERIC )
             AND  (WRK-ERRCD               =   SPACE )
               MOVE    "03"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-SETCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット名称チェック
      *****************************************************************
       20011-SETNAME-CHK-SEC          SECTION.
      *
           IF      MDCREQ-SETCD-NAME   =   SPACE
      *        登録は必須
               IF      FLG-SYORI           =   1
                   MOVE    "04"                TO  WRK-ERRCD
               END-IF 
           ELSE
               MOVE    MDCREQ-SETCD-NAME   TO  WRK-MAE-INPUT
               MOVE    80                  TO  WRK-LEN
               PERFORM 880-HALF-ZENCNG-SEC
      *        拡張漢字エラー
               IF      FLG-ERR             =   1
                   MOVE    "05"                TO  WRK-ERRCD
               ELSE
                   MOVE    WRK-ATO-INPUT       TO  MDCRES-SETCD-NAME
               END-IF
           END-IF
      *
           .
       20011-SETNAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始日付チェック
      *****************************************************************
       20011-START-DATE-CHK-SEC          SECTION.
      *
           IF      WRK-YUKOSTYMD       =   ZERO
               CONTINUE
           ELSE
      *        日付チェックサブ
               MOVE    SPACE               TO  STS-AREA-DAY
               INITIALIZE                      STS-AREA-DAY
               MOVE    SPACE               TO  LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-YUKOSTYMD       TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY2-AREA
               IF      STS-DAY-RC1     NOT =   ZERO
                   MOVE    "06"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
           .
       20011-START-DATE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了日付チェック
      *****************************************************************
       20011-END-DATE-CHK-SEC          SECTION.
      *
           IF      WRK-YUKOEDYMD       =   "99999999"
               CONTINUE
           ELSE
      *        日付チェックサブ
               MOVE    SPACE               TO  STS-AREA-DAY
               INITIALIZE                      STS-AREA-DAY
               MOVE    SPACE               TO  LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-YUKOEDYMD       TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY2-AREA
               IF      STS-DAY-RC1     NOT =   ZERO
                   MOVE    "07"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
           IF     (WRK-YUKOSTYMD       >=  WRK-YUKOEDYMD)
              AND (WRK-ERRCD           =   SPACE    )
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
           .
       20011-END-DATE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット登録処理
      *****************************************************************
       2002-INPUTSET-ADD-SEC        SECTION.
      *
      *    診療行為内容編集
           PERFORM 200101-MDCHENSYU-SEC
      *    追加初期チェック
           PERFORM 20021-SETADD-INIT-SEC
      *
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      2002-INPUTSET-ADD-EXT
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
      *    セット内容返却初期
           PERFORM 20040-MEDICAL-INIT-SEC
      *
           INITIALIZE                  SPA-K02
      *
      *    ワーク診療行為データレイアウト作成
           PERFORM 20021-WKSRYACT-SYORI-SEC
      *    画面展開・計算
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20022-INPUT-SYORI-SEC
           END-IF
      *    パラメタ削除
           PERFORM 2009-PARA-DELETE-SEC
           .
       2002-INPUTSET-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    リクエスト内容をレスポンス編集処理
      *****************************************************************
       200101-MDCHENSYU-SEC        SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY    1
                   UNTIL      (IDX     >   COB-ZAI-INMAX )
                       OR     (MDCREQ-MDC-CLASS (IDX) =   SPACE )
               MOVE    MDCREQ-MDC-CLASS (IDX)
                                       TO  MDCRES-MDC-CLASS     (IDX)
               MOVE    MDCREQ-MDC-CLASS-NAME (IDX)
                                       TO  MDCRES-MDC-CLASS-NAME
                                                                (IDX)
               MOVE    MDCREQ-MDC-CLASS-NUMBER (IDX)
                                       TO  MDCRES-MDC-CLASS-NUMBER
                                                                (IDX)
      *        診療内容
               PERFORM VARYING     IDY     FROM    1   BY    1
                       UNTIL      (IDY     >   COB-MEI-MAX  )
                           OR     (MDCREQ-PRSCRPT-CODE (IDX IDY)
                                           =   SPACE )
                   MOVE    MDCREQ-PRSCRPT-CODE (IDX IDY)
                                       TO  MDCRES-PRSCRPT-CODE
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-NAME (IDX IDY)
                                       TO  MDCRES-PRSCRPT-NAME
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-NUMBER (IDX IDY)
                                       TO  MDCRES-PRSCRPT-NUMBER
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-MONEY (IDX IDY)
                                       TO  MDCRES-PRSCRPT-MONEY
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-ATAI-G (IDX IDY)
                                       TO  MDCRES-PRSCRPT-ATAI-G
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-CONTKBN (IDX IDY)
                                       TO  MDCRES-PRSCRPT-CONTKBN
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-NAIFKBN (IDX IDY)
                                       TO  MDCRES-PRSCRPT-NAIFKBN
                                                           (IDX IDY)
               END-PERFORM
           END-PERFORM
      *
           .
       200101-MDCHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加初期チェック処理
      *****************************************************************
       20021-SETADD-INIT-SEC        SECTION.
      *
      *    セット登録がないこと
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
           MOVE    MDCREQ-SETCD        TO  ISET-SETCD
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
           MOVE    ZERO                TO  FLG-SET-ARI
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL    (FLG-INPUTSET  =   1   )
               ADD     1                   TO  IDX
               IF     (ISET-YUKOSTYMD      <=  WRK-YUKOSTYMD)
                 AND  (ISET-YUKOEDYMD      >=  WRK-YUKOSTYMD)
                   MOVE    1               TO  FLG-SET-ARI
               END-IF
               IF     (ISET-YUKOSTYMD      >=  WRK-YUKOEDYMD)
                 AND  (ISET-YUKOEDYMD      <=  WRK-YUKOEDYMD)
                   MOVE    1               TO  FLG-SET-ARI
               END-IF
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-INPUTSET-READ-SEC
           END-PERFORM
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    履歴は最大１０件
           IF      IDX                 =   10
               MOVE    "12"            TO  WRK-ERRCD
           END-IF
      *
           IF      FLG-SET-ARI         =   1
               MOVE    "10"            TO  WRK-ERRCD
           END-IF
      *
      *    入力ＣＤチェック
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    "6"                 TO  ICD-CDSYU
           MOVE    MDCREQ-SETCD        TO  ICD-INPUTCD
           PERFORM 900-INPUTCD-KEYREAD-SEC
      *
      *    履歴なしの時
           IF      IDX                 =   ZERO
      *        登録済み
               IF      FLG-INPUTCD         =   ZERO
                   MOVE    "11"            TO  WRK-ERRCD
               END-IF
           ELSE
      *        履歴あり
               IF      FLG-INPUTCD         =   1
                   MOVE    "13"            TO  WRK-ERRCD
               END-IF
               MOVE    IDX                 TO  WRK-SET-RRK
           END-IF
           .
       20021-SETADD-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為データ作成処理
      *****************************************************************
       20021-WKSRYACT-SYORI-SEC        SECTION.
      *
           INITIALIZE                  ORAPI021SUB1V3AREA
      *
           PERFORM VARYING     IDX     FROM    1   BY    1
                   UNTIL       IDX     >   COB-ZAI-INMAX
               MOVE    MDCREQ-MDC-CLASS (IDX)
                                       TO  ORAPISUB01-MDC-CLASS
                                                                (IDX)
               MOVE    MDCREQ-MDC-CLASS-NAME (IDX)
                                       TO  ORAPISUB01-MDC-CLASS-NAME
                                                                (IDX)
               MOVE    MDCREQ-MDC-CLASS-NUMBER (IDX)
                                       TO  ORAPISUB01-MDC-CLASS-NUMBER
                                                                (IDX)
      *        診療内容
               PERFORM VARYING     IDY     FROM    1   BY    1
                       UNTIL       IDY     >   COB-MEI-MAX
                   MOVE    MDCREQ-PRSCRPT-CODE (IDX IDY)
                                       TO  ORAPISUB01-PRSCRPT-CODE
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-NAME (IDX IDY)
                                       TO  ORAPISUB01-PRSCRPT-NAME
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-NUMBER (IDX IDY)
                                       TO  ORAPISUB01-PRSCRPT-NUMBER
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-MONEY (IDX IDY)
                                       TO  ORAPISUB01-PRSCRPT-MONEY
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-ATAI-G (IDX IDY)
                                       TO  ORAPISUB01-ATAI-G
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-CONTKBN (IDX IDY)
                                       TO  ORAPISUB01-CONTKBN
                                                           (IDX IDY)
                   MOVE    MDCREQ-PRSCRPT-NAIFKBN (IDX IDY)
                                       TO  ORAPISUB01-NAIFKBN
                                                           (IDX IDY)
               END-PERFORM
           END-PERFORM
      *
           MOVE    "SET"               TO  ORAPISUB01-SYORIKBN
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORAPI021SUB1V3"   USING
                                       MCPAREA
                                       SPA-AREA
                                       ORAPI021SUB1V3AREA
      *
      *    エラー処理
           IF      ORAPISUB01-ERRCD    NOT =   SPACE
               MOVE    ORAPISUB01-ERRCD    TO  WRK-ERRCD
               MOVE    ORAPISUB01-ERRMSG   TO  WRK-ERRMSG
           END-IF
           PERFORM VARYING     IDX    FROM    1   BY  1
                   UNTIL       (IDX        >   40  )
                           OR  (ORAPISUB01-IN-ERR (IDX)
                                           =   SPACE )
      *        自動算定対象外はエラーとする
               IF      ORAPISUB01-IN-ERR (IDX) =   "W13"
                   MOVE    "セット登録対象外です。"
                                               TO  ORAPISUB01-IN-ERRMSG
                                                               (IDX)
                   MOVE    "013"               TO  ORAPISUB01-IN-ERR
                                                               (IDX)
               END-IF
               MOVE    ORAPISUB01-IN-ERR (IDX)
                                       TO  MDCRES-MEDI-RESULT (IDX)
               MOVE    ORAPISUB01-IN-ERRMSG (IDX)
                                       TO  MDCRES-MEDI-RESULT-MSG (IDX)
               MOVE    ORAPISUB01-IN-POS (IDX)
                                       TO  MDCRES-MEDI-POSITION (IDX)
               MOVE    ORAPISUB01-IN-LINE (IDX)
                                       TO  MDCRES-MEDI-ITEM-POSITION
                                                                (IDX)
               MOVE    ORAPISUB01-IN-SRYCD (IDX)
                                       TO  MDCRES-MEDI-CODE (IDX)
               IF      WRK-ERRCD       =   SPACE
                   MOVE    "21"            TO  WRK-ERRCD
               END-IF
           END-PERFORM
           .
       20021-WKSRYACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    中途データ展開・編集処理
      *****************************************************************
       20022-INPUT-SYORI-SEC     SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-TIMEFLG
           MOVE    ZERO                TO  SPA-AKUSEI-FLG
      *
      *    中途データセット展開
           MOVE    "API"               TO  SPA-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K02             TO  SPA-FREE
      *
           MOVE    "API"               TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK05"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *    画面ＳＰＡ読み込み
           PERFORM 1002-K05SPA-SET-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 200211-ERRCD-HEN-SEC
               MOVE    "21"            TO  WRK-ERRCD
           END-IF
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
                    OR     (SPA-ERRCD          NOT =   SPACE )
                    OR     (WRK-ERRCD          NOT =   SPACE )
               EVALUATE    SPA-GMN-INPUTCD (IDX)(1:1)
                   WHEN    "."
                   WHEN    "*"
                       CONTINUE
                   WHEN    OTHER
      *                点数マスタチェック
                       PERFORM 200222-KIHONCHK-SEC
                       IF      SPA-ERRCD       NOT =   SPACE
                            MOVE    IDX            TO  WRK-ERR-IDX
                            PERFORM 200232-ERR-HEN-SEC
                       END-IF
               END-EVALUATE
           END-PERFORM
      *
      *    診療行為、計計算処理、編集
           IF     (SPA-ERRCD           =   SPACE )
             AND  (WRK-ERRCD           =   SPACE )
               PERFORM 20225-TBLHEN-SYORI-SEC
           END-IF
      *
      *    エラーメッセージ編集処理
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 200232-ERR-HEN-SEC
           END-IF
      *
      *    登録内容処理
           PERFORM 200222-MEDICAL-HEN-SEC
      *
           IF     (IDX-MDE             >   ZERO  )
             AND  (WRK-ERRCD           =   SPACE )
               MOVE    "21"            TO  WRK-ERRCD
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
             AND  (WRK-ERRCD           =   SPACE )
      *        登録処理
               PERFORM 200222-INPUTSET-TOROKU-SEC
           END-IF
      *
      *    エラーメッセージ編集処理
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 200211-ERRCD-HEN-SEC
               MOVE    "21"            TO  WRK-ERRCD
           END-IF
           .
       20022-INPUT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡデータ読み込み処理
      *****************************************************************
       1002-K05SPA-SET-SEC             SECTION.
      *
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                      SPA-SCR-REC
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
      *    TBL_PARA を使用する
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K05"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SPAGMN"            TO  PARA-FILEMEI
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
           PERFORM
               UNTIL  (FLG-PARA            =   1 )
                 OR   (IDX                 >=  SPA-MAX-LINE)
               ADD    1                    TO  IDX
               MOVE   PARA-DATA-REC        TO  SPA-GMN-TBL (IDX)
      *
               IF      (SPA-COM-INPUTCD(IDX)   =   SPACE ) AND
                       (SPA-GMN-INPUTCD(IDX)   =   SPACE )
                   CONTINUE
               ELSE
                   ADD     1               TO  SPA-GMN-MAX
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
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K05"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SPAGMN"            TO  PARA-FILEMEI
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
      *
           .
       1002-K05SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   点数マスタチェック処理
      *****************************************************************
       200222-KIHONCHK-SEC                 SECTION.
      *
      *    点数マスタ編集
           INITIALIZE                  ORCSC92AREA
           MOVE    SPACE               TO  LNK-SC92-KBN
           MOVE    "K05"               TO  LNK-SC92-PG
           MOVE    IDX                 TO  LNK-SC92-GMN-IDX
           MOVE    SPA-COM-INPUTCD(IDX)
                                       TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
      *    MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
      **** MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
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
           MOVE    LNK-SC92-ERRMSG     TO  SPA-ERRMSG2
      *    警告は登録対象
           IF      SPA-ERRCD(1:1)      =   "K"
               MOVE    ZERO                TO  FLG-ERR
               PERFORM 3001-KEIMSGSYORI-SEC
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
           .
       200222-KIHONCHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為、計計算処理、編集処理
      *****************************************************************
       20225-TBLHEN-SYORI-SEC           SECTION.
      *
      *    空白行をなくす
      *    PERFORM 420-SYOKYO-SEC
      *
      *    剤分離処理
           INITIALIZE                  ORCSC93AREA
           MOVE    SPA-GMN-SETCODE     TO  LNK-SC93-SETCODE
           MOVE    "K05"               TO  LNK-SC93-PG
      *    残量廃棄区分
           MOVE    SPA-HAIKIKBN-FLG    TO  LNK-SC93-HAIKIKBN-FLG
           CALL    "ORCSC93"           USING
                                       MCPAREA
                                       SPA-SCR-REC
                                       SPA-AREA
                                       ORCSC93AREA
                                       MSYU-DATA-REC
           IF     (SPA-ERRCD       NOT =   SPACE )
               OR (IDX-MDE             >   ZERO  )
               GO      TO      20225-TBLHEN-SYORI-EXT
           END-IF
      *
      *    約束セットは薬評・減点を入力不可とする
      *    IF      (SPA-GMN-SETCODE(1:1)   =   "S"    ) 
      *        PERFORM 5009-SETCHEK-SEC
      *        IF      SPA-ERRCD       NOT =   SPACE
      *            GO      TO      510-TBLHEN-EXT
      *        END-IF
      *    END-IF
      *
           MOVE    1                   TO  FLG-TOROKU
      *    剤別初回編集
           INITIALIZE                  ORCSC00AREA
      *    診療種別件数クリア
           INITIALIZE                  SPA-SRYKBN-AREA
      *    剤ごとの処理
           MOVE    ZERO                TO  IDX-Z
           MOVE    ZERO                TO  WRK-KAISU
           INITIALIZE                  WRK-SCR-REC
           INITIALIZE                  WRK-HZN-SCR-AREA
           INITIALIZE                  WRK-HOZ-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )   OR
                              (SPA-GMN-INPUTCD (IDX)
                                                   =   SPACE )
                         OR   (SPA-ERRCD   NOT =   SPACE )
                         OR   (WRK-ERRCD   NOT =   SPACE )
      *        回数の入力があるかのチェック
      **       PERFORM 51011-KAISU-CHK-SEC
      *        剤ごとの算定処理で変更される加算データは対象としない
      *          但し、コメントの自動追加分は、そのまま
      *              （注射の廃棄コメントははずす）
               MOVE    ZERO                TO  FLG-TAISYO
      *AUTOKBNはないのでこのまま
               IF      SPA-COM-AUTOKBN (IDX)    =   "2"  OR  "3"
                   IF     ( SPA-COM-INPUTCD (IDX)(1:1)
                                               NOT =   "8"   )   OR
                          ((SPA-COM-SRYKBN  (IDX)  =   "31" OR
                                                       "32" OR
                                                       "33")   AND
                           (SPA-COM-INPUTCD (IDX)  =   "099309901" ))
                        MOVE    1               TO  FLG-TAISYO
                   END-IF
      *            薬剤の自動追加コメントは対象外
                   IF     ( SPA-COM-INPUTCD (IDX)(1:1)   =  "8" ) AND
                          ( SPA-COM-SRYKBN  (IDX)    =   "21"
                                                     OR  "22"
                                                     OR  "33"  )
                        MOVE    1               TO  FLG-TAISYO
                   END-IF
      *            自動追加コメントで入力値ありの時対象外
                   IF      (SPA-COM-INPUTCD (IDX)(1:2)   =  "84") OR
                           (SPA-COM-INPUTCD (IDX)(1:4)   =  "0084")
                        MOVE    ZERO            TO  FLG-TAISYO
                   END-IF
               END-IF
      *
               IF      FLG-TAISYO              =   ZERO
                   ADD     1               TO  IDX-Z
                   MOVE    SPA-COM-TBLREC (IDX)
                                           TO  WRK-COM-TBLREC(IDX-Z)
                   MOVE    SPA-GMN-TBLREC (IDX)
                                           TO  WRK-GMN-TBLREC(IDX-Z)
               END-IF
      *        回数編集
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "."
                   CONTINUE
               ELSE
                   IF      SPA-COM-KAIFLG (IDX)    =   "1"
                        MOVE    SPA-COM-KAISU (IDX) TO  WRK-KAISU
      *                 外用薬の時、入力回数を
                        IF     (SPA-COM-SRYKBN(IDX) =  "23"  )  AND
                               (SPA-COM-KAISU2(IDX) >   ZERO ) AND
                               (SPA-COM-KAISU (IDX) =   1    )
                            MOVE    SPA-COM-KAISU2 (IDX) TO  WRK-KAISU
                        END-IF
                   END-IF
               END-IF
      *
               IF     (IDX                 =   SPA-GMN-MAX   )  OR
                      (SPA-COM-ZAIEND (IDX)
                                           =   "1"           )
      *            剤ごとの処理
                   PERFORM 5102-ZAIBETU-SEC
                   INITIALIZE                  WRK-SCR-REC
                   MOVE    ZERO                TO  IDX-Z
                   MOVE    ZERO                TO  WRK-KAISU
               END-IF
      *        エラー
               IF      SPA-ERRCD(1:1)      =   "K"
                   MOVE    ZERO                TO  FLG-ERR
                   PERFORM 3001-KEIMSGSYORI-SEC
                   MOVE    SPACE               TO  SPA-ERRCD
               END-IF
           END-PERFORM
      *
      *    スパ領域へ戻す
           MOVE    ZERO                TO  SPA-GMN-MAX
           INITIALIZE                  SPA-GMN-REC
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF     (WRK-HZN-GMN-INPUTCD (IDX)   =   SPACE)  AND
                      (WRK-HZN-COM-INPUTCD (IDX)   =   SPACE)
                   CONTINUE
               ELSE
                   ADD     1                   TO  SPA-GMN-MAX
                   MOVE    WRK-HZN-GMN-TBLREC (IDX)
                                               TO  SPA-GMN-TBLREC
                                                         (SPA-GMN-MAX)
                   MOVE    WRK-HZN-COM-TBLREC (IDX)
                                               TO  SPA-COM-TBLREC
                                                         (SPA-GMN-MAX)
               END-IF
           END-PERFORM
      *
      *    登録前の全体のチェックを行う
           IF     (WRK-ERRCD           =   SPACE )
             AND  (SPA-ERRCD           =   SPACE )
               INITIALIZE                  ORCSC00AREA
               MOVE    "2"                 TO  LNK-ORCSC-KBN
               PERFORM 520-ALL-CHEK-SEC
           END-IF
      *
           .
       20225-TBLHEN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤ごとの処理
      *****************************************************************
       5102-ZAIBETU-SEC             SECTION.
      *
           IF      WRK-KAISU           =   ZERO
               MOVE    1               TO  WRK-KAISU
           END-IF
      *
           INITIALIZE                  LNK-ORCSZAI-AREA
           MOVE    "1"                 TO  LNK-ORCSC-KBN
           MOVE    "K05"               TO  LNK-ORCSC-PG
           MOVE    WRK-KAISU           TO  LNK-ORCSC-KAISU
           MOVE    SPA-NAI-ROUJIN      TO  LNK-ORCSC-ROUJIN 
           MOVE    SPA-NENREI          TO  LNK-ORCSC-NENREI 
           MOVE    FLG-TOROKU          TO  LNK-ORCSC-TOROKU
      *    時間外特例区分
           MOVE    SPA-JIKANTOKU       TO  LNK-ORCSC-JIKANTOKU
      *    皮下筋肉注射料変換選択
           MOVE    SPA-CHG310FLG       TO  LNK-ORCSC-CHG310FLG
      *    腫瘍マーカー一覧展開選択
      **   MOVE    SPA-AKUSEIHYOFLG    TO  LNK-ORCSC-AKUSEIHYOFLG
      *    残量廃棄区分
           MOVE    SPA-HAIKIKBN-FLG    TO  LNK-ORCSC-HAIKIKBN-FLG
      *
           EVALUATE    WRK-COM-SRYKBN (01)
      *        診療
               WHEN    "11"
               WHEN    "12"
               WHEN    "13"
               WHEN    "14"
                   CALL    "ORCSC10"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (01)
      *            薬剤
               WHEN    "21"
               WHEN    "22"
               WHEN    "23"
                   CALL    "ORCSC20"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (02)
      *            注射
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   CALL    "ORCSC30"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (03)
      *            処置
               WHEN    "40"
                   CALL    "ORCSC40"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (04)
      *            手術
               WHEN    "50"
                   CALL    "ORCSC50"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (05)
      *            麻酔
               WHEN    "54"
                   CALL    "ORCSC54"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (06)
      *            検査
               WHEN    "60"
               WHEN    "64"
                   CALL    "ORCSC60"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (07)
      *            画像診断
               WHEN    "70"
                   CALL    "ORCSC70"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (08)
      *            その他
               WHEN    "80"
                   CALL    "ORCSC80"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (09)
      *            コメント等
      *********WHEN    "90"
               WHEN    "93"
               WHEN    "99"
      *****        PERFORM 510211-SRYKBN-90-SEC
                   CONTINUE
      *            自費
               WHEN    "95"
               WHEN    "96"
                   CALL    "ORCSCJIH"      USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
      *            入院（食事）
               WHEN    "97"
                   CALL    "ORCSCN97"      USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (10)
           END-EVALUATE
      *
      *    編集後内容セット
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *        診区
               IF      IDX-Z               =   1
                   MOVE    WRK-COM-SRYKBN (IDX-Z)
                                           TO  WRK-GMN-SRYKBN(IDX-Z)
               ELSE
                   MOVE    SPACE           TO  WRK-GMN-SRYKBN(IDX-Z)
                   MOVE    WRK-COM-SRYSYUKBN (01)
                                           TO  WRK-COM-SRYSYUKBN(IDX-Z)
               END-IF
      *        名称（最初の行に’＊’）
               IF     (WRK-COM-INPUTCD(IDX-Z)(1:1) =   "8" )  OR
                      (WRK-COM-INPUTCD(IDX-Z)(1:3) =   "008")
                   CONTINUE
               ELSE
                   IF      IDX-Z               =   1
                       MOVE    "* "            TO  WRK-GMN-MEIH (IDX-Z)
                   ELSE
                       MOVE    SPACE           TO  WRK-GMN-MEIH (IDX-Z)
                   END-IF
               END-IF
      *
      *        最終行を剤終了とする
               IF      (IDX-Z              =   SPA-MAX-LINE )  OR
                       (WRK-COM-INPUTCD(IDX-Z + 1) =   SPACE  AND
                        WRK-GMN-INPUTCD(IDX-Z + 1) =   SPACE )
                   MOVE    "1"             TO  WRK-COM-ZAIEND(IDX-Z)
               ELSE
                   MOVE    SPACE           TO  WRK-COM-ZAIEND(IDX-Z)
               END-IF
      *        回数
               IF      LNK-ORCSC-KAISU NOT =   WRK-COM-KAISU (IDX-Z)
                   MOVE    LNK-ORCSC-KAISU     TO  WRK-COM-KAISU(IDX-Z)
               END-IF
      *        最終行に回数入力（＊）が有った時、最終行を回数入力
      *        IF      FLG-KAISU           NOT =   ZERO
      *            IF      WRK-COM-ZAIEND (IDX-Z)  =   SPACE
      *                IF      WRK-COM-KAIFLG (IDX-Z)  =   "1"
      *                    MOVE    SPACE           TO  WRK-COM-KAIFLG
      *                                                        (IDX-Z)
      *                    PERFORM 51021-KAISU-SAIHEN-SEC
      *                END-IF
      *            ELSE
      *                最終行
      *                IF      WRK-COM-KAIFLG (IDX-Z)  =   SPACE
      *                    PERFORM 51021-KAISU-SAIHEN-SEC
      *                    MOVE    "1"             TO  WRK-COM-KAIFLG
      *                                                        (IDX-Z)
      *                END-IF
      *            END-IF
      **       END-IF
      *
      *        約束セット行の編集
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1) =   "S"
                   MOVE    LNK-ORCSC-ZAITEN        TO  WRK-COM-TENSU
                                                                (IDX-Z)
                   MOVE    LNK-ORCSC-ZAIKEI        TO  WRK-COM-KEI
                                                               (IDX-Z)
                   MOVE    LNK-ORCSC-KAISU         TO  WRK-COM-KAISU
                                                               (IDX-Z)
               END-IF
      *        画面再編集・基本チェック
               INITIALIZE                  ORCSC94AREA
               MOVE    "2"                 TO  LNK-SC94-KBN
               MOVE    "K05"               TO  LNK-SC94-PG
               MOVE    IDX-Z               TO  LNK-SC94-IDX
               CALL    "ORCSC94"           USING
                                           ORCSC94AREA
                                           WRK-SCR-REC
                                           SPA-AREA
      *
               ADD     1                   TO  WRK-HOZ-IDX
               MOVE    WRK-COM-TBLREC (IDX-Z)
                                           TO  WRK-HZN-COM-TBLREC
                                                       (WRK-HOZ-IDX)
               MOVE    WRK-GMN-TBLREC (IDX-Z)
                                           TO  WRK-HZN-GMN-TBLREC
                                                       (WRK-HOZ-IDX)
      *
           END-PERFORM
      *
           .
       5102-ZAIBETU-EXT.
           EXIT.
      *****************************************************************
      *    各診療行為チェックへ
      *****************************************************************
       520-ALL-CHEK-SEC                SECTION.
      *
           MOVE    SPA-NAI-ROUJIN      TO  LNK-ORCSC-ROUJIN 
           MOVE    SPA-NENREI          TO  LNK-ORCSC-NENREI 
           MOVE    FLG-TOROKU          TO  LNK-ORCSC-TOROKU
      *    皮下筋肉注射料変換選択
           MOVE    SPA-CHG310FLG       TO  LNK-ORCSC-CHG310FLG
      *    腫瘍マーカー一覧展開選択
           MOVE    SPA-AKUSEIHYOFLG    TO  LNK-ORCSC-AKUSEIHYOFLG
      *    検査の重複チェックのみ行う
           IF      SPA-SRYKBN-CNT (07) >   ZERO
               CALL    "ORCSC60"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           END-IF
      *
           .
       520-ALL-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       200232-ERR-HEN-SEC     SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
           MOVE    "K05"               TO  LNK-SC96-PG
           CALL    "ORCSC96"           USING    SPA-AREA
                                                ORCSC96AREA
      *
      *    全角変換
           MOVE    SPA-ERRMSG          TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKUHEN-SEC
           MOVE    WRK-MAE-INPUT       TO  WRK-ERRMSG1
      *
           MOVE    SPACE               TO  WRK-MD-ERRMSG
           IF      SPA-ERRMSG2         =   SPACE
               MOVE    WRK-ERRMSG1         TO  WRK-MD-ERRMSG1
           ELSE
      *        全角変換
               MOVE    SPA-ERRMSG2         TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKUHEN-SEC
               MOVE    WRK-MAE-INPUT       TO  WRK-ERRMSG2
      *
               MOVE    WRK-ERRMSG2         TO  WRK-MD-ERRMSG1
               MOVE    WRK-ERRMSG1         TO  WRK-MD-ERRMSG2
           END-IF
           MOVE    SPA-ERRCD           TO  WRK-MD-ERRCD
      *
      *    行判定
           MOVE    SPACE           TO  WRK-ERR-SRYCD
           IF      WRK-ERR-IDX     >   ZERO
               MOVE    WRK-ERR-IDX         TO  IDX2
               MOVE    SPA-COM-INPUTCD (IDX2)   TO  WRK-ERR-SRYCD
               IF      SPA-COM-INPUTCD (IDX2)   =   SPACE
                   MOVE    SPA-GMN-INPUTCD (IDX2)   TO  WRK-ERR-SRYCD
               END-IF
           ELSE
      *
           MOVE    ZERO            TO  IDX2
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
                   OR     ( WRK-ERR-SRYCD      NOT =   SPACE  )
               IF     (SPA-COM-CUR    (IDX)    =   "1" )
                  AND (SPA-COM-KZMFLG (IDX)    =   SPACE )
                  AND (SPA-COM-KZMFLG2(IDX)    =   SPACE )
                  AND (SPA-COM-KZMFLG3(IDX)    =   SPACE )
                  AND (SPA-COM-KZMFLG4(IDX)    =   SPACE )
                  AND (SPA-COM-KZMFLG5(IDX)    =   SPACE )
                  AND (SPA-COM-KZMFLG6(IDX)    =   SPACE )
                  AND (SPA-COM-KINKIFLG(IDX)   =   SPACE )
                  AND (SPA-COM-TUKIJGNFLG(IDX) =   SPACE )
      *
                   MOVE    SPA-COM-INPUTCD (IDX)   TO  WRK-ERR-SRYCD
                   IF      SPA-COM-INPUTCD (IDX)   =   SPACE
                       MOVE    SPA-GMN-INPUTCD (IDX)   TO  WRK-ERR-SRYCD
                   END-IF
                   MOVE    IDX             TO  IDX2
               END-IF
           END-PERFORM
      *
           END-IF
      *
           IF      WRK-ERR-SRYCD   NOT =   SPACE
               ADD     1                   TO  IDX-MDE
               IF      IDX-MDE             <=  400
                   MOVE    SPA-ERRCD       TO  TBL-ERRERRCD (IDX-MDE)
                   MOVE    WRK-MD-ERRMSG   TO  TBL-ERRMSG   (IDX-MDE)
                   MOVE    WRK-ERR-SRYCD   TO  TBL-ERR-SRYCD(IDX-MDE)
      *            位置
                   MOVE    IDX-MDE         TO  SPA-COM-GYOU (IDX2)
               END-IF
           END-IF
      *
      *
           MOVE    SPACE               TO  WRK-MD-ERRMSG
           MOVE    SPACE               TO  WRK-MD-ERRCD
           MOVE    SPACE               TO  SPA-ERRMSG2
           MOVE    SPACE               TO  SPA-ERRMSG
           MOVE    SPACE               TO  SPA-ERRCD 
           MOVE    ZERO                TO  WRK-ERR-IDX
           .
       200232-ERR-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       200222-MEDICAL-HEN-SEC          SECTION.
      *
           INITIALIZE                  MDCRES-MDC-INFO-G
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDZ
           MOVE    ZERO                TO  FLG-HENCHK
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX            >   SPA-MAX-LINE )
                       OR ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
                       OR  (FLG-HENCHK NOT =   ZERO    )
      *
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "."
                   MOVE    SPA-COM-SRYKBN (IDX)    TO  WRK-SRYKBN
                   MOVE    SPA-COM-SRYSYUKBN(IDX)  TO  WRK-SRYSYUKBN
                   MOVE    ZERO                    TO  WRK-KINGAK
      *            エラー・警告
                   IF     (SPA-COM-CUR (IDX)   =   "1"  )
                     OR   (SPA-COM-GYOU(IDX)   >   ZERO )
                       PERFORM 2002222-LINE-HEN-SEC
                   END-IF
               ELSE
      *
                   IF      IDZ                 =   ZERO
                       ADD     1                   TO  IDY
                       IF      IDY                 >   COB-ZAI-OUTMAX
                           MOVE    1               TO  FLG-HENCHK
                       END-IF
                       MOVE    SPA-COM-SRYKBN (IDX)    TO  WRK-SRYKBN
                       MOVE    SPA-COM-SRYSYUKBN(IDX)  TO  WRK-SRYSYUKBN
                       IF      WRK-SRYSYUKBN        =   SPACE
                            MOVE    WRK-SRYKBN      TO  WRK-SRYSYUKBN
                                                                  (1:2)
                            MOVE    "0"             TO  WRK-SRYSYUKBN
                                                                  (3:1)
                            MOVE    WRK-SRYSYUKBN   TO SPA-COM-SRYSYUKBN
                                                                  (IDX) 
                       END-IF
                       MOVE    ZERO                    TO  WRK-KINGAK
                   END-IF
                   ADD     1                   TO  IDZ
                   IF      IDZ                 >   COB-ZAI-OUTMAX
                       MOVE    2               TO  FLG-HENCHK
                   END-IF
      *
                   IF      FLG-HENCHK          =   ZERO
                       PERFORM 2002221-MEISAI-HEN-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           IF     (FLG-HENCHK          =   1     )
              AND (WRK-ERRCD           =   SPACE )
      *        剤数オーバー
               MOVE    "24"                TO  WRK-ERRCD
           END-IF
           IF     (FLG-HENCHK          =   2     )
              AND (WRK-ERRCD           =   SPACE )
      *        剤数オーバー
               MOVE    "25"                TO  WRK-ERRCD
           END-IF
      *!!!
      *    明細なし
           IF     (IDY                 =   ZERO  )
             AND  (WRK-ERRCD           =   SPACE )
               MOVE    "20"            TO  WRK-ERRCD
           END-IF
           .
       200222-MEDICAL-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為自動算定・算定チェック処理
      *****************************************************************
       2002221-MEISAI-HEN-SEC          SECTION.
      *
      *    コード
           MOVE    SPA-COM-INPUTCD(IDX)    TO MDCRES-PRSCRPT-CODE
                                                             (IDY IDZ)
      *    名称
           IF     (SPA-COM-INPUTCD(IDX)(1:1) =   "8")
             OR   (SPA-COM-INPUTCD(IDX)(1:3) =   "008")
             OR   (SPA-COM-INPUTCD(IDX)(1:3) =   "001")
               IF     (SPA-COM-INPUTCD(IDX)  =   "810000001"
                                             OR  "008500000"
                                             OR  "008600000")
      *            フリーコード
                   MOVE    SPA-GMN-MEISYO-G (IDX)
                                               TO  MDCRES-PRSCRPT-NAME
                                                             (IDY IDZ)
               ELSE
                   MOVE    SPA-GMN-MEISYO (IDX)
                                               TO  MDCRES-PRSCRPT-NAME
                                                             (IDY IDZ)
               END-IF
           ELSE
      *        点数マスタの名称
               MOVE    SPA-COM-NAME (IDX)  TO  MDCRES-PRSCRPT-NAME
                                                             (IDY IDZ)
           END-IF
      *
      *    約束セット（登録済みセット内容のみ）
           IF     SPA-GMN-INPUTCD(IDX)(1:1)    =   "S"
               MOVE    SPA-GMN-INPUTCD(IDX)    TO  MDCRES-PRSCRPT-CODE
                                                             (IDY IDZ)
               MOVE    SPA-GMN-MEISYO (IDX)    TO  MDCRES-PRSCRPT-NAME
                                                             (IDY IDZ)
           END-IF
      *
      *    数量
           IF     (SPA-COM-INPUTCD(IDX)(1:3)   =   "058"
                                               OR  "001"
                                               OR  "002"
                                               OR  "099")
      *        数量入力のできないコード
               IF      SPA-COM-INPUTCD(IDX)(1:7)   =   "0992000"
                   CONTINUE
               ELSE
                   MOVE    1               TO  WRK-SURYO
                   MOVE    SPACE           TO  SPA-COM-SURFLG (IDX)
               END-IF
           END-IF
      *
      *    数量
           MOVE    SPA-COM-SURYO(IDX)      TO  WRK-SURYO
           IF      WRK-SURYO               =   ZERO
               IF      SPA-COM-SURFLG (IDX)    =   SPACE
                   MOVE    1                   TO  WRK-SURYO
               END-IF
           END-IF
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)   TO  MDCRES-PRSCRPT-NUMBER
                                                             (IDY IDZ)
      *
      *    自費金額
           IF     (SPA-COM-SRYKBN (IDX)    =   "96"
                                           OR  "95"   )
      *        自賠責金額
           OR     (SPA-COM-SRYSYUKBN (IDX)  =   "809" )
           OR     (SPA-COM-JIHIMONEY (IDX)  >   ZERO  )
               MOVE    SPA-COM-JIHIMONEY (IDX) TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                               TO  MDCRES-PRSCRPT-MONEY
                                                             (IDY IDZ)
           END-IF
      *    器材の商品名入力がある時
           IF     (SPA-COM-INPUTCD (IDX)(1:1)   =   "7"   )
             AND  (SPA-COM-AUTOKBN2(IDX)    NOT =   SPACE )
               MOVE    "2"        TO  MDCRES-PRSCRPT-CONTKBN
                                                             (IDY IDZ)
           END-IF
      *                                                      (IDY IDZ)
      *    継続コメント
           IF      SPA-COM-INPUTCONT (IDX)     =   "1"
               MOVE    "1"        TO  MDCRES-PRSCRPT-CONTKBN
                                                             (IDY IDZ)
           END-IF
      *    内服１種類区分
           IF      SPA-COM-INPUTNAIF (IDX)     =   "1"
               MOVE    "1"        TO  MDCRES-PRSCRPT-NAIFKBN
                                                             (IDY IDZ)
           END-IF
      *    埋め込み入力値
           IF     (SPA-COM-INPUTCD(IDX)(1:2)   =   "84" )
              OR  (SPA-COM-INPUTCD(IDX)(1:4)   =   "0084")
      *       用法コメント
              OR ((SPA-COM-INPUTCD(IDX)(1:3)   =   "001") AND
                  (SPA-COM-YCOMKBN (IDX)       =   "1" ))
      *R02.4
              OR  (SPA-COM-INPUTCD(IDX)(1:2)   =   "85" )
              OR  (SPA-COM-INPUTCD(IDX)(1:3)   =   "831" ) 
               PERFORM VARYING     IDX-A   FROM    1   BY  1
                       UNTIL      (IDX-A       >   5   )
                   IF      SPA-COM-ATAI (IDX IDX-A)    NOT =   SPACE
                       MOVE    SPA-COM-ATAI (IDX IDX-A)
                                       TO  MDCRES-PRSCRPT-ATAI
                                                     (IDY IDZ IDX-A)
                   END-IF
               END-PERFORM
      *        数量はなし
               MOVE    SPACE           TO  MDCRES-PRSCRPT-NUMBER
                                                             (IDY IDZ)
           END-IF
      *
           IF      SPA-COM-KEIFLG (IDX)   =   "1"  OR  "2"
               ADD      SPA-COM-JIHIMONEY (IDX)    TO  WRK-KINGAK
           END-IF
      *
           IF     (IDX                     <   SPA-GMN-MAX) AND
                  (SPA-GMN-INPUTCD (IDX + 1)(1:1)
                                            =   "."    )
               MOVE    "1"             TO  SPA-COM-ZAIEND (IDX)
           END-IF
      *
      *    エラー・警告
           IF     (SPA-COM-CUR (IDX)   =   "1"  )
             OR   (SPA-COM-GYOU(IDX)   >   ZERO )
               PERFORM 2002222-LINE-HEN-SEC
           END-IF
      *
           IF      SPA-COM-ZAIEND (IDX)    =   "1"
      *        剤毎の編集
               PERFORM 2002221-ZAIHENSYU-SEC
               MOVE    ZERO                    TO  IDZ
           END-IF
      *
           .
       2002221-MEISAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    剤編集処理
      *****************************************************************
       2002221-ZAIHENSYU-SEC          SECTION.
      *
      *    回数
           IF      SPA-COM-KAISU (IDX) >   ZERO
               MOVE    SPA-COM-KAISU (IDX) TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-MDC-CLASS-NUMBER(IDY)
           END-IF
      *    診療区分
      **** MOVE    WRK-SRYKBN          TO  MDCRES-MDC-CLASS-KBN (IDY)
           MOVE    WRK-SRYSYUKBN       TO  MDCRES-MDC-CLASS     (IDY)
           PERFORM 510-SRYKBN-MEI-SEC
           MOVE    WRK-SRYSYUKBNMEI    TO  MDCRES-MDC-CLASS-NAME(IDY)
           .
       2002221-ZAIHENSYU-EXT.
           EXIT.
      *****************************************************************
      *    エラー・警告行位置編集処理
      *****************************************************************
       2002222-LINE-HEN-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-H-LINE
           MOVE    SPACE               TO  WRK-H-IDX
           IF      IDY                 >   ZERO
               MOVE    IDY             TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                           TO  WRK-H-LINE
           END-IF
           IF      IDZ                 >   ZERO
               MOVE    IDZ             TO  WRK-SURYO
               PERFORM 810-NUMHENKAN-SEC
               MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  WRK-H-IDX
           END-IF
      *
           IF      SPA-COM-GYOU(IDX)   >   ZERO
               MOVE    SPA-COM-GYOU(IDX)   TO  IDK
               IF      TBL-ERR-SRYCD (IDK) =   SPA-COM-INPUTCD (IDX)
                                           OR  SPA-GMN-INPUTCD (IDX)
      *            エラー行
                   MOVE    WRK-H-LINE          TO  TBL-ERR-LINE (IDK)
                   MOVE    WRK-H-IDX           TO  TBL-ERR-IDX  (IDK)
               ELSE
      *            警告行
                   MOVE    WRK-H-LINE          TO  TBL-KEI-LINE (IDK)
                   MOVE    WRK-H-IDX           TO  TBL-KEI-IDX  (IDK)
               END-IF
           END-IF
           IF     (SPA-COM-CUR (IDX)   =   "1" )
              AND (SPA-COM-GYOU(IDX)   =   ZERO)
               IF     (SPA-GMN-INPUTCD (IDX)   =   WRK-ERR-INPUTCD)
                  OR  (SPA-COM-INPUTCD (IDX)   =   WRK-ERR-SRYCD  )
                   MOVE    WRK-H-LINE          TO  WRK-MD-ERR-LINE
                   MOVE    WRK-H-IDX           TO  WRK-MD-ERR-IDX
               END-IF
           END-IF
           .
       2002222-LINE-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       200211-ERRCD-HEN-SEC     SECTION.
      *
           IF      SPA-ERRMSG2         =   SPACE
               MOVE    "K05"               TO  LNK-SC96-PG
               CALL    "ORCSC96"           USING    SPA-AREA
                                                    ORCSC96AREA
               MOVE    SPA-ERRMSG          TO  SPA-ERRMSG2
           END-IF
      *    全角変換
           MOVE    SPA-ERRMSG2         TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKUHEN-SEC
           MOVE    WRK-MAE-INPUT       TO  WRK-ERRMSG
           MOVE    SPACE               TO  SPA-ERRMSG2
           MOVE    SPACE               TO  SPA-ERRMSG
           .
       200211-ERRCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    警告メッセージ処理
      *****************************************************************
       3001-KEIMSGSYORI-SEC          SECTION.
      *
           MOVE    "K05"               TO  LNK-SC96-PG
           CALL    "ORCSC96"           USING    SPA-AREA
                                                ORCSC96AREA
      *    全角変換
           MOVE    SPA-ERRMSG          TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKUHEN-SEC
           MOVE    WRK-MAE-INPUT       TO  WRK-ERRMSG1
      *
           MOVE    SPACE               TO  WRK-KEIMSG
           IF      SPA-ERRMSG2         =   SPACE
               MOVE    SPACE               TO  WRK-ERRMSG2
               MOVE    WRK-ERRMSG1         TO  WRK-KEIMSG1
           ELSE
               MOVE    SPA-ERRMSG2         TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKUHEN-SEC
               MOVE    WRK-MAE-INPUT       TO  WRK-ERRMSG2
      *
               MOVE    WRK-ERRMSG2         TO  WRK-KEIMSG1
               MOVE    WRK-ERRMSG1         TO  WRK-KEIMSG2
           END-IF
      *
      *    警告行判定
           MOVE    SPACE               TO  WRK-ERR-SRYCD
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
                   OR     ( WRK-ERR-SRYCD      NOT =   SPACE  )
               IF     (SPA-COM-CUR    (IDX)    =   "2"
                                               OR  "1" )
                   PERFORM 30011-KEIMSGHEN-SEC
                   MOVE    SPA-COM-INPUTCD (IDX)   TO  WRK-ERR-SRYCD
                   MOVE    SPACE                   TO  SPA-COM-CUR(IDX)
               END-IF
           END-PERFORM
      *
           IF      WRK-ERR-SRYCD       =   SPACE
               ADD     1                   TO  IDX-KEI
               IF      IDX-KEI             <=  400
                   MOVE    SPA-ERRCD       TO  TBL-KEIERRCD (IDX-KEI)
                   MOVE    WRK-KEIMSG      TO  TBL-KEIMSG   (IDX-KEI)
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-ERRMSG
           MOVE    SPACE               TO  SPA-ERRMSG2
           MOVE    SPACE               TO  WRK-ERR-SRYCD
           MOVE    SPACE               TO  WRK-ERRMSG
      *
           MOVE    1                   TO  FLG-KEIMSG
           .
       3001-KEIMSGSYORI-EXT.
           EXIT.
      *****************************************************************
      *    警告メッセージ編集処理
      *****************************************************************
       30011-KEIMSGHEN-SEC     SECTION.
      *
           ADD     1                   TO  IDX-KEI
           IF      IDX-KEI             <=  400
               MOVE    SPA-ERRCD           TO  TBL-KEIERRCD (IDX-KEI)
               MOVE    WRK-KEIMSG          TO  TBL-KEIMSG   (IDX-KEI)
               MOVE    SPA-COM-INPUTCD (IDX)
                                           TO  TBL-KEI-SRYCD(IDX-KEI)
               IF      SPA-COM-INPUTCD (IDX)   =   SPACE
                   MOVE    SPA-GMN-INPUTCD (IDX)(1:9)
                                           TO  TBL-KEI-SRYCD(IDX-KEI)
               END-IF
      *        警告位置
               MOVE    IDX-KEI             TO  SPA-COM-GYOU (IDX)
           END-IF
           .
       30011-KEIMSGHEN-EXT.
           EXIT.
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
      *****************************************************************
      *    セット登録処理
      *****************************************************************
       200222-INPUTSET-TOROKU-SEC        SECTION.
      *
      *    追加初期チェック
           PERFORM 20021-SETADD-INIT-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO    TO  200222-INPUTSET-TOROKU-EXT
           END-IF
      *
      *    セットコード登録
           IF      WRK-SET-RRK         =   ZERO
               PERFORM 2002221-INPUTCD-INS-SEC
           END-IF
      *    セット内容登録
           PERFORM 2002222-INPUTSET-INS-SEC
      *
           .
       200222-INPUTSET-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード追加処理
      *****************************************************************
       2002221-INPUTCD-INS-SEC         SECTION.
      *
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    "6"                 TO  ICD-CDSYU
           MOVE    ZERO                TO  ICD-DSPSEQ
           MOVE    MDCREQ-SETCD        TO  ICD-INPUTCD
           MOVE    MDCRES-SETCD-NAME   TO  ICD-DSPNAME
           MOVE    MDCREQ-SETCD        TO  ICD-SRYCD
      *
           MOVE    SMCNDATE-YMD        TO  ICD-CREYMD
           MOVE    SMCNDATE-YMD        TO  ICD-UPYMD
           MOVE    SMCNDATE-HMS        TO  ICD-UPHMS
      *
           MOVE    WRK-OPID            TO  ICD-OPID
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "APISET INPUTCD INS ERR:"  MCP-RC
               MOVE    "28"            TO  WRK-ERRCD
           END-IF
           .
       2002221-INPUTCD-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力セット追加作成処理
      *****************************************************************
       2002222-INPUTSET-INS-SEC        SECTION.
      *
           MOVE    ZERO                TO  IDX-SET
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-MAX   )  OR
                              (WRK-ERRCD   NOT =   SPACE )
      *      画面表示なしの時、出力しない
             IF      SPA-COM-SET(IDX)      =   SPACE
      *
               ADD     1                   TO  IDX-SET
               INITIALIZE                      INPUTSET-REC
               MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
               MOVE    MDCREQ-SETCD        TO  ISET-SETCD
               MOVE    IDX-SET             TO  ISET-SETSEQ
               MOVE    WRK-YUKOSTYMD       TO  ISET-YUKOSTYMD
               MOVE    WRK-YUKOEDYMD       TO  ISET-YUKOEDYMD
      *        入力セット内容編集処理
               PERFORM 2002222-INPUTSET-HEN-SEC
      *
               MOVE    SMCNDATE-YMD        TO  ISET-CREYMD
               MOVE    SMCNDATE-YMD        TO  ISET-UPYMD
               MOVE    SMCNDATE-HMS        TO  ISET-UPHMS
      *
               MOVE    WRK-OPID            TO  ISET-OPID
      *
               MOVE    INPUTSET-REC        TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "K05 INPUTSET   INS ERR:"  MCP-RC
                   MOVE    "29"            TO  WRK-ERRCD
               END-IF
             END-IF
           END-PERFORM
      *
           .
       2002222-INPUTSET-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力セット内容編集処理
      *****************************************************************
       2002222-INPUTSET-HEN-SEC              SECTION.
      *
           IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "."  OR  "S"
               MOVE    SPA-GMN-INPUTCD(IDX)(1:6)   TO  ISET-INPUTCD
           ELSE
               MOVE    SPA-COM-INPUTCD(IDX)    TO  ISET-INPUTCD
           END-IF
      *    数量
           IF      SPA-COM-KEIFLG (IDX)    =   SPACE  OR  "2"
      *        数量
               MOVE    SPA-COM-SURYO  (IDX)    TO  ISET-SURYO1
               IF     (SPA-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (SPA-COM-SURFLG (IDX)    =   SPACE   )  AND
                      (SPA-COM-SURYO (IDX)     >   1      )
                   MOVE    1                   TO  ISET-SURYO1
               END-IF
           ELSE
      *        保険外金額
               MOVE    SPA-COM-JIHIMONEY (IDX)     TO  ISET-SURYO1
           END-IF
           MOVE    SPA-COM-BUNGSU (IDX)    TO  ISET-SURYO2
      *
      *    回数
           IF     (SPA-COM-KAIFLG (IDX)    =   "1"    )
           OR     (SPA-GMN-INPUTCD(IDX)(1:1)   =   "S")
               MOVE    SPA-COM-KAISU  (IDX)    TO  ISET-KAISU
           END-IF
      *    入力コメント番号
           IF     (SPA-COM-INPUTCD(IDX)(1:1)   =   "8" )  OR
                  (SPA-COM-INPUTCD(IDX)(1:3)   =   "008")
             OR   (SPA-COM-INPUTCD(IDX)(1:3)   =   "001")
      *        入力コメント
               IF      SPA-COM-INPUTCD (IDX)    =   "810000001"
                                                OR  "008500000"
                                                OR  "008600000"
                   MOVE    SPA-GMN-MEISYO-G (IDX)
                                               TO  ISET-COMMENT
               ELSE
                   IF    ((SPA-COM-INPUTCD (IDX)(1:1)  =   "8"  ) AND
                          (SPA-COM-INPUTCHI-G(IDX) NOT =   SPACE))
                   OR    ((SPA-COM-INPUTCD (IDX)(1:4)  =   "0084") AND
                          (SPA-COM-INPUTCHI-G(IDX) NOT =   SPACE))
                   OR     (SPA-COM-INPUTCD (IDX)(1:2)  =   "83" )
                   OR     (SPA-COM-INPUTCD (IDX)(1:4)  =   "0083")
                   OR     (SPA-COM-INPUTCD (IDX)(1:4)  =   "0085")
                   OR     (SPA-COM-INPUTCD (IDX)(1:4)  =   "0086")
                   OR    ((SPA-COM-INPUTCD (IDX)(1:3)  =   "001" ) AND
                          (SPA-COM-INPUTCHI-G(IDX) NOT =   SPACE))
                       MOVE    SPA-GMN-MEISYO(IDX)
                                               TO  ISET-COMMENT
                   END-IF
               END-IF
      *        入力値
               MOVE    SPA-COM-ATAI1(IDX)  TO  ISET-ATAI1
               MOVE    SPA-COM-ATAI2(IDX)  TO  ISET-ATAI2
               MOVE    SPA-COM-ATAI3(IDX)  TO  ISET-ATAI3
               MOVE    SPA-COM-ATAI4(IDX)  TO  ISET-ATAI4
               MOVE    SPA-COM-ATAI5(IDX)  TO  ISET-ATAI5
           END-IF
      *    換算値
           MOVE    SPA-COM-KANSURYO (IDX)  TO  ISET-KANSURYO
      *    関係コメント指示
           IF      SPA-COM-INPUTCONT (IDX) =   "1"
               MOVE    "1"                 TO  ISET-INPUTKBN
           END-IF
      *    内服１種類区分
           IF      SPA-COM-INPUTNAIF (IDX)     =   "1"
               MOVE    "2"                 TO  ISET-INPUTKBN
           END-IF
           .
       2002222-INPUTSET-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    PARA削除処理
      *****************************************************************
       2009-PARA-DELETE-SEC        SECTION.
      *
      *    パラメタ削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           .
       2009-PARA-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       2003-INPUTSET-DEL-SEC        SECTION.
      *
      *    初期チェック
           PERFORM 20031-SETDEL-INIT-SEC
      *    セットなし
           IF     (WRK-YUKOSTYMD       =   "00000000" )
             AND  (WRK-YUKOEDYMD       =   "99999999" )
               IF      FLG-SET-ARI         =   ZERO
                   MOVE    "30"            TO  WRK-ERRCD
               END-IF
           ELSE
               IF      FLG-SET-OK          =   ZERO
                   MOVE    "30"            TO  WRK-ERRCD
               END-IF
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      2003-INPUTSET-DEL-EXT
           END-IF
      *
           IF     (WRK-YUKOSTYMD       =   "00000000" )
             AND  (WRK-YUKOEDYMD       =   "99999999" )
      *        全履歴削除
               PERFORM 200311-DELETE-ALL-SEC
           ELSE
      *        該当セット削除
               PERFORM 200312-DELETE-SET-SEC
           END-IF
      *
           .
       2003-INPUTSET-DEL-EXT.
           EXIT.
      *****************************************************************
      *    初期チェック処理
      *****************************************************************
       20031-SETDEL-INIT-SEC        SECTION.
      *
      *    セット存在チェック
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
           MOVE    MDCREQ-SETCD        TO  ISET-SETCD
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
           MOVE    ZERO                TO  FLG-SET-ARI
           MOVE    ZERO                TO  FLG-SET-OK
           PERFORM UNTIL    (FLG-INPUTSET  =   1   )
                        OR  (FLG-SET-OK    =   1   )
               IF     (ISET-YUKOSTYMD      =   WRK-YUKOSTYMD)
                 AND  (ISET-YUKOEDYMD      =   WRK-YUKOEDYMD)
                   MOVE    1               TO  FLG-SET-OK
               END-IF
               MOVE    1                   TO  FLG-SET-ARI
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-INPUTSET-READ-SEC
           END-PERFORM
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       20031-SETDEL-INIT-EXT.
           EXIT.
      *****************************************************************
      *    セット全履歴削除処理
      *****************************************************************
       200311-DELETE-ALL-SEC        SECTION.
      *
      *    セット削除
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
           MOVE    MDCREQ-SETCD        TO  ISET-SETCD
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "INPUTSET   DEL ERR:"  MCP-RC
               MOVE    "31"            TO  WRK-ERRCD
               GO      TO      200311-DELETE-ALL-EXT
           END-IF
      *
      *    入力ＣＤ削除
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    MDCREQ-SETCD        TO  ICD-SRYCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "del22"             TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "INPUTCD    DEL ERR:"  MCP-RC
               MOVE    "31"            TO  WRK-ERRCD
           END-IF
      *
           .
       200311-DELETE-ALL-EXT.
           EXIT.
      *****************************************************************
      *    該当セット削除処理
      *****************************************************************
       200312-DELETE-SET-SEC        SECTION.
      *
      *    セット削除
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
           MOVE    MDCREQ-SETCD        TO  ISET-SETCD
           MOVE    WRK-YUKOSTYMD       TO  ISET-YUKOSTYMD
           MOVE    WRK-YUKOEDYMD       TO  ISET-YUKOEDYMD
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "INPUTSET   DEL ERR:"  MCP-RC
               MOVE    "31"            TO  WRK-ERRCD
               GO      TO      2003-INPUTSET-DEL-EXT
           END-IF
      *
      *    セット存在チェック
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
           MOVE    MDCREQ-SETCD        TO  ISET-SETCD
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    入力ＣＤ削除
           IF      FLG-INPUTSET        =   1
               INITIALIZE                      INPUTCD-REC
               MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
               MOVE    MDCREQ-SETCD        TO  ICD-SRYCD
      *
               MOVE    INPUTCD-REC         TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "del22"             TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "INPUTCD    DEL ERR:"  MCP-RC
                   MOVE    "31"            TO  WRK-ERRCD
               END-IF
           END-IF
      *
           .
       200312-DELETE-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了日変更処理
      *****************************************************************
       2004-INPUTSET-ENDUPD-SEC        SECTION.
      *
      *    初期チェック
           PERFORM 20041-SETUPD-INIT-SEC
      *    セットなし
           IF      FLG-SET-OK          =   ZERO
               MOVE    "40"            TO  WRK-ERRCD
           END-IF
      *    最終履歴以外はエラー
           IF      FLG-SET-ARI         =   1
               MOVE    "41"            TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      2004-INPUTSET-ENDUPD-EXT
           END-IF
      *
      *    セット内容返却初期
           PERFORM 20040-MEDICAL-INIT-SEC
           MOVE    ZERO                TO  IDX
      *    セット終了日更新
           PERFORM 20041-INPUTSET-UPDATE-SEC
      *    ＤＢ更新エラー
           IF      FLG-ERR2            =   1
               GO      TO      2004-INPUTSET-ENDUPD-EXT
           END-IF
      *
      *    エラーメッセージ編集処理
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 200211-ERRCD-HEN-SEC
           END-IF
           PERFORM 200415-SETMEDICAL-HEN-SEC
           .
       2004-INPUTSET-ENDUPD-EXT.
           EXIT.
      *****************************************************************
      *    初期チェック処理
      *****************************************************************
       20041-SETUPD-INIT-SEC        SECTION.
      *
      *    セット存在チェック
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
           MOVE    MDCREQ-SETCD        TO  ISET-SETCD
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
           MOVE    ZERO                TO  FLG-SET-ARI
           MOVE    ZERO                TO  FLG-SET-OK
           MOVE    SPACE               TO  WRK-SEL-YUKOEDYMD
           PERFORM UNTIL    (FLG-INPUTSET  =   1   )
               IF     (ISET-YUKOSTYMD      =   WRK-YUKOSTYMD)
                   MOVE    ISET-YUKOEDYMD  TO  WRK-SEL-YUKOEDYMD
                   MOVE    1               TO  FLG-SET-OK
               END-IF
               IF     (ISET-YUKOSTYMD      >   WRK-YUKOSTYMD)
                   MOVE    1                   TO  FLG-SET-ARI
               END-IF
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-INPUTSET-READ-SEC
           END-PERFORM
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    入力ＣＤチェック
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    "6"                 TO  ICD-CDSYU
           MOVE    MDCREQ-SETCD        TO  ICD-INPUTCD
           PERFORM 900-INPUTCD-KEYREAD-SEC
           IF      FLG-INPUTCD         =   ZERO
               MOVE    ICD-DSPNAME         TO  MDCRES-SETCD-NAME
           ELSE
               MOVE    "13"            TO  WRK-ERRCD
           END-IF
           .
       20041-SETUPD-INIT-EXT.
           EXIT.
      *****************************************************************
      *    セット内容返却初期処理
      *****************************************************************
       20040-MEDICAL-INIT-SEC        SECTION.
      *
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *    ＳＰＡ初期設定
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           IF      SPA-NYUGAIKBN       =   "2"
      *        外来
               MOVE    "1"                 TO  LNK-SC95-KBN
           ELSE
      *        入院
               MOVE    "9"                 TO  LNK-SC95-KBN
           END-IF
           MOVE    "K05"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
      *    テーブル最大行
           MOVE    MAX-LINE-VALUE      TO  SPA-MAX-LINE
      *
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-GMN-IDX
      *    年齢
           MOVE    50                  TO  SPA-NENREI-YY
      *
           .
       20040-MEDICAL-INIT-EXT.
           EXIT.
      *****************************************************************
      *    セット終了日更新処理
      *****************************************************************
       20041-INPUTSET-UPDATE-SEC        SECTION.
      *
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
           MOVE    MDCREQ-SETCD        TO  ISET-SETCD
           MOVE    WRK-YUKOSTYMD       TO  ISET-YUKOSTYMD
           MOVE    WRK-SEL-YUKOEDYMD   TO  ISET-YUKOEDYMD
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
           PERFORM UNTIL    (FLG-INPUTSET  =   1   )
               IF     (ISET-YUKOSTYMD      =   WRK-YUKOSTYMD)
                 AND  (ISET-YUKOEDYMD      =   WRK-SEL-YUKOEDYMD)
      *            削除
                   MOVE    INPUTSET-REC        TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_inputset"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "K05 INPUTSET   INS ERR:"  MCP-RC
                       MOVE    "43"            TO  WRK-ERRCD
                       MOVE    1               TO  FLG-ERR2
                   END-IF
      *
                   MOVE    WRK-YUKOEDYMD       TO  ISET-YUKOEDYMD
                   MOVE    SMCNDATE-YMD        TO  ISET-UPYMD
                   MOVE    SMCNDATE-HMS        TO  ISET-UPHMS
      *
                   MOVE    WRK-OPID            TO  ISET-OPID
      *
                   MOVE    INPUTSET-REC        TO  MCPDATA-REC
                   MOVE    "DBINSERT"          TO  MCP-FUNC
                   MOVE    "tbl_inputset"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "K05 INPUTSET   INS ERR:"  MCP-RC
                       MOVE    "29"            TO  WRK-ERRCD
                       MOVE    1               TO  FLG-ERR2
                   END-IF
      *            セット内容ＳＰＡ展開
                   PERFORM 20045-INPUTSET-SPAHEN-SEC
      *
               END-IF
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-INPUTSET-READ-SEC
           END-PERFORM
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       20041-INPUTSET-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット内容ＳＰＡ展開処理
      *****************************************************************
       20045-INPUTSET-SPAHEN-SEC       SECTION.
      *
           ADD     1               TO  SPA-GMN-IDX
           IF      SPA-GMN-IDX     >   SPA-MAX-LINE
               GO      TO      20045-INPUTSET-SPAHEN-EXT
           END-IF
      *
      *    器材商品コード対応
      *    1行前が器材コードの自動発生の時、置換え
           MOVE    ZERO            TO  FLG-AUTOKBN2
           IF     (SPA-GMN-IDX         >   1    )    AND
                  (ISET-INPUTCD (1:1)  =   "7"  )    AND
                  (SPA-COM-AUTOKBN2(SPA-GMN-IDX - 1)
                                       =   "1"   )   AND
                  (ISET-INPUTCD        =   SPA-COM-INPUTCD
                                                  (SPA-GMN-IDX - 1 ))
               MOVE    1               TO  FLG-AUTOKBN2
               COMPUTE SPA-GMN-IDX     =   SPA-GMN-IDX   -  1
           END-IF
      *!!
      *
           MOVE    ISET-INPUTCD    TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
      *
      *    約束セットの時
           IF      ISET-INPUTCD(1:1)       =   "S"
               MOVE    ISET-INPUTCD(1:6)   TO  SPA-GMN-INPUTCD
                                                         (SPA-GMN-IDX)
           END-IF
      *
           MOVE    ISET-SURYO1         TO  SPA-COM-SURYO (SPA-GMN-IDX)
      *    １以上の時、数量入力とする
           IF     (ISET-INPUTCD(1:1)   =   "1"
                                       OR  "6"
                                       OR  "7" )   AND
                  (ISET-SURYO1         >   1   )
               MOVE    "1"             TO  SPA-COM-SURFLG (SPA-GMN-IDX)
           END-IF
      *
      *    換算値
           MOVE    ISET-KANSURYO       TO  SPA-COM-KANSURYO
                                                           (SPA-GMN-IDX)
      *    関係コメント指示
           IF      ISET-INPUTKBN       =   "1"
                MOVE    "1"            TO  SPA-COM-INPUTCONT
                                                           (SPA-GMN-IDX)
           END-IF
      *    内服１種類区分
           IF      ISET-INPUTKBN       =   "2"
                MOVE    "1"            TO  SPA-COM-INPUTNAIF
                                                           (SPA-GMN-IDX)
           END-IF
      *
           MOVE    SPA-COM-SURYO (SPA-GMN-IDX)
                                       TO  SPA-COM-SURYO2(SPA-GMN-IDX)
           IF      ISET-SURYO2     =   ZERO
               MOVE    1           TO  SPA-COM-BUNGSU(SPA-GMN-IDX)
           ELSE
               MOVE    ISET-SURYO2 TO  SPA-COM-BUNGSU(SPA-GMN-IDX)
           END-IF
           IF      ISET-KAISU      =   ZERO
               MOVE    1           TO  SPA-COM-KAISU  (SPA-GMN-IDX)
           ELSE
               MOVE    ISET-KAISU  TO  SPA-COM-KAISU  (SPA-GMN-IDX)
               MOVE    "1"         TO  SPA-COM-KAIFLG (SPA-GMN-IDX)
           END-IF
           MOVE    SPA-COM-KAISU (SPA-GMN-IDX)
                                       TO  SPA-COM-KAISU2(SPA-GMN-IDX)
      *
           IF      ISET-INPUTCD        =   "810000001"
                                       OR  "008500000"
                                       OR  "008600000"
               MOVE    ISET-COMMENT    TO  SPA-GMN-MEISYO-G
                                                          (SPA-GMN-IDX)
           ELSE
               MOVE    ISET-COMMENT    TO  SPA-GMN-MEISYO
                                                          (SPA-GMN-IDX)
           END-IF
      *
           MOVE    ISET-ATAI (01)  TO  SPA-COM-ATAI1 (SPA-GMN-IDX)
           MOVE    ISET-ATAI (02)  TO  SPA-COM-ATAI2 (SPA-GMN-IDX)
           MOVE    ISET-ATAI (03)  TO  SPA-COM-ATAI3 (SPA-GMN-IDX)
           MOVE    ISET-ATAI (04)  TO  SPA-COM-ATAI4 (SPA-GMN-IDX)
           MOVE    ISET-ATAI (05)  TO  SPA-COM-ATAI5 (SPA-GMN-IDX)
      *
      *    約束処方の時、画面表示なし
      *    IF      SPA-GMN-INPUTCD(WRK-GMN-IDX)(1:1)   =   "S"
      *        画面表示なし
      *        MOVE    "1"             TO  SPA-COM-SET(SPA-GMN-IDX)
      *    END-IF
      *
           IF       SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)  =   "." 
      *        診療種別区分名称チェック
               MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:3)
                                               TO  WRK-SRYSYUKBN
               PERFORM 510-SRYKBN-MEI-SEC
               MOVE    WRK-SRYSYUKBN           TO  SPA-COM-SRYSYUKBN
                                                          (SPA-GMN-IDX)
               MOVE    WRK-SRYKBN              TO  SPA-COM-SRYKBN
                                                          (SPA-GMN-IDX)
               MOVE    WRK-SRYKBN              TO  SPA-GMN-SRYKBN
                                                          (SPA-GMN-IDX)
           ELSE
               MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:9)
                                               TO  SPA-COM-INPUTCD
                                                          (SPA-GMN-IDX)
      *
      *        セット内に約束セットがある時
               IF       SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)  =   "S"
      *            約束名称編集
                   INITIALIZE                      INPUTCD-REC
                   MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
                   MOVE    "6"                 TO  ICD-CDSYU
                   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:6)
                                               TO  ICD-INPUTCD
                   PERFORM 900-INPUTCD-KEYREAD-SEC
                   MOVE    ICD-DSPNAME         TO  SPA-GMN-MEISYO
                                                          (SPA-GMN-IDX)
      *
                   MOVE    SPACE               TO  SPA-COM-INPUTCD
                                                          (SPA-GMN-IDX)
                ELSE
      *             基本情報編集チェック
                    MOVE    SPA-GMN-IDX         TO  IDX
                    PERFORM 200222-KIHONCHK-SEC
               END-IF
           END-IF
      *
           IF      SPA-COM-KAIFLG (SPA-GMN-IDX)    =   "1"
               MOVE    SPACE               TO  WRK-SRYKBN
           END-IF
      *
           IF      FLG-AUTOKBN2        =   1
               MOVE    "1"             TO  SPA-COM-AUTOKBN2 
                                                      (SPA-GMN-IDX)
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    SPA-GMN-IDX         TO  WRK-ERR-IDX
               PERFORM 200232-ERR-HEN-SEC
           END-IF
      *
           IF     (SPA-ERRCD        NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA NOT =   SPACE )
             OR   (FLG-ERR              =   1     )
               MOVE    SPACE               TO  SPA-ERRCD
           ELSE
               MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
           END-IF
      *
           .
       20045-INPUTSET-SPAHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット編集処理
      *****************************************************************
       200415-SETMEDICAL-HEN-SEC                SECTION.
      *
      *    診療行為、計計算処理、編集
           PERFORM 20225-TBLHEN-SYORI-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 200232-ERR-HEN-SEC
           END-IF
      *
      *    登録内容処理
           PERFORM 200222-MEDICAL-HEN-SEC
           .
       200415-SETMEDICAL-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット内容取得処理
      *****************************************************************
       2005-INPUTSET-HENKAN-SEC        SECTION.
      *
      *    セット存在チェック
           IF     (WRK-YUKOSTYMD       =   "00000000" )  AND
                  (WRK-YUKOEDYMD       =   "99999999" )
               PERFORM 20051-SETINIT-SEC
           ELSE
               MOVE    WRK-YUKOSTYMD       TO  WRK-SEL-YUKOSTYMD
               MOVE    WRK-YUKOEDYMD       TO  WRK-SEL-YUKOEDYMD
           END-IF
           IF      WRK-SEL-YUKOSTYMD   =   SPACE
      *        対象なし
               MOVE    "42"                TO  WRK-ERRCD
           ELSE
      *        開始日がシステム日付より後の時、開始日を診療日
               IF      WRK-SEL-YUKOSTYMD   >   SPA-SYSYMD
                  MOVE    WRK-SEL-YUKOSTYMD    TO  SPA-SRYYMD
               END-IF
      *        終了日がシステム日付より前の時、終了日を診療日
               IF      WRK-SEL-YUKOEDYMD       <   SPA-SYSYMD
                   MOVE    WRK-SEL-YUKOEDYMD   TO  SPA-SRYYMD
               END-IF
           END-IF
      *
           IF     WRK-ERRCD            =   SPACE
               PERFORM 20052-INPUTSET-HENSYU-SEC
           END-IF
      *
      *    編集期間
           MOVE    WRK-YUKOSTYMD       TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MDCRES-START-DATE
      *
           MOVE    WRK-YUKOEDYMD       TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MDCRES-END-DATE
      *    入力ＣＤチェック
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    "6"                 TO  ICD-CDSYU
           MOVE    MDCREQ-SETCD        TO  ICD-INPUTCD
           PERFORM 900-INPUTCD-KEYREAD-SEC
           IF      FLG-INPUTCD         =   ZERO
               MOVE    ICD-DSPNAME         TO  MDCRES-SETCD-NAME
           ELSE
               MOVE    "13"                TO  WRK-ERRCD
           END-IF
      *
           IF       SPA-GMN-IDX        =   ZERO
      *        対象なし
               IF      WRK-ERRCD           =   SPACE
                   MOVE    "43"                TO  WRK-ERRCD
               END-IF
           ELSE
               PERFORM 200415-SETMEDICAL-HEN-SEC
           END-IF
      *    エラーメッセージ編集処理
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 200211-ERRCD-HEN-SEC
           END-IF
           .
       2005-INPUTSET-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期チェック処理
      *****************************************************************
       20051-SETINIT-SEC        SECTION.
      *
      *    セット存在チェック
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
           MOVE    MDCREQ-SETCD        TO  ISET-SETCD
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
           MOVE    ZERO                TO  FLG-SET-OK
           MOVE    SPACE               TO  WRK-SEL-YUKOSTYMD
           MOVE    SPACE               TO  WRK-SEL-YUKOEDYMD
           PERFORM UNTIL    (FLG-INPUTSET  =   1   )
                        OR  (FLG-SET-OK    =   1   )
               IF     (ISET-YUKOSTYMD      <=   SPA-SRYYMD)
                 AND  (ISET-YUKOEDYMD      >=   SPA-SRYYMD)
                   MOVE    ISET-YUKOSTYMD  TO  WRK-SEL-YUKOSTYMD
                   MOVE    ISET-YUKOEDYMD  TO  WRK-SEL-YUKOEDYMD
                   MOVE    1               TO  FLG-SET-OK
               END-IF
               IF     (ISET-YUKOSTYMD      >   WRK-SEL-YUKOSTYMD)
                   MOVE    ISET-YUKOSTYMD  TO  WRK-SEL-YUKOSTYMD
                   MOVE    ISET-YUKOEDYMD  TO  WRK-SEL-YUKOEDYMD
               END-IF
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-INPUTSET-READ-SEC
           END-PERFORM
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       20051-SETINIT-EXT.
           EXIT.
      *****************************************************************
      *    セット内容返却編集処理
      *****************************************************************
       20052-INPUTSET-HENSYU-SEC                SECTION.
      *
      *    セット内容返却初期
           PERFORM 20040-MEDICAL-INIT-SEC
           MOVE    ZERO                TO  IDX
      *
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
           MOVE    MDCREQ-SETCD        TO  ISET-SETCD
           MOVE    WRK-SEL-YUKOSTYMD   TO  ISET-YUKOSTYMD
           MOVE    WRK-SEL-YUKOEDYMD   TO  ISET-YUKOEDYMD
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
           PERFORM UNTIL    (FLG-INPUTSET  =   1   )
      *
               MOVE    ISET-YUKOSTYMD      TO  WRK-YUKOSTYMD
               MOVE    ISET-YUKOEDYMD      TO  WRK-YUKOEDYMD
      *        セット内容ＳＰＡ展開
               PERFORM 20045-INPUTSET-SPAHEN-SEC
      *
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-INPUTSET-READ-SEC
           END-PERFORM
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       20052-INPUTSET-HENSYU-EXT.
           EXIT.
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
      *    半角を全角へ変換処理
      *****************************************************************
       880-HALF-ZENCNG-SEC         SECTION.
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
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-MAE-INPUT
           IF     (KANACHK-RC      NOT =   ZERO  )
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
      *    全角変換処理
      *****************************************************************
       800-ZENKAKUHEN-SEC          SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-MAE-INPUT
           END-IF
           .
       800-ZENKAKUHEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    数値変換編集処理
      *****************************************************************
       810-NUMHENKAN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           IF      WRK-SURYO           <   ZERO
               COMPUTE WRK-SURYO-S         =   WRK-SURYO      *  -1
               COMPUTE WRK-SURYO2-S1       =   WRK-SURYO-S1   *  -1
               MOVE    WRK-SURYO2-S1       TO  WRK-SURYO-Z1
           ELSE
               MOVE    WRK-SURYO           TO  WRK-SURYO-S
               MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
           END-IF
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDXM    FROM    5  BY  -1
                   UNTIL       IDXM    <   1
               IF      WRK-SURYO-S2(IDXM:1)    NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDXM:1)    TO  WRK-SURYO-Z3
                                                             (IDXM:1)
               END-IF
           END-PERFORM
           MOVE    SPACE               TO  WRK-SURYO-Z2
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
           END-IF
      *    左詰
           MOVE    SPACE              TO  WRK-SURYO-HX
           MOVE    1                  TO  IDXR
           MOVE    ZERO               TO  WRK-S-MAX
           PERFORM VARYING     IDXM    FROM    1    BY  1
                   UNTIL       IDXM    >   15
               IF      WRK-SURYO-X (IDXM:1)    NOT =   SPACE
                   MOVE    WRK-SURYO-X (IDXM:1)    TO  WRK-SURYO-HX
                                                       (IDXR:1)
                   COMPUTE IDXR            =   IDXR    +   1
      *            桁数
                   ADD     1               TO  WRK-S-MAX
               END-IF
           END-PERFORM
           .
       810-NUMHENKAN-EXT.
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
               MOVE    "セットコードが未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "02"
               MOVE    "基準日の暦日エラーです"
                                               TO  WRK-ERRMSG
           WHEN    "03"
               STRING "セットコードの構成が違います。"
                                           DELIMITED  BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
           WHEN    "09"
               STRING "約束セットは処理できません。"
                                           DELIMITED  BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
           WHEN    "04"
               MOVE    "セット名称が未設定です。"
                                               TO  WRK-ERRMSG
           WHEN    "05"
               MOVE    "セット名称エラーです"
                                               TO  WRK-ERRMSG
           WHEN    "06"
               MOVE    "開始日が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "07"
               MOVE    "終了日が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "08"
               MOVE    "開始日≧終了日です。"
                                               TO  WRK-ERRMSG
           WHEN    "10"
               STRING  "セットコード登録済みです。"
                                           DELIMITED  BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
           WHEN    "11"
               STRING  "セットコードが入力コードに登録済みです。"
                       "このセットコードは登録できません。"
                                           DELIMITED  BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
           WHEN    "12"
               STRING  "履歴が１０件登録済みです。"
                       "これ以上登録できません。"
                                           DELIMITED  BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
           WHEN    "13"
               STRING  "セットコードが入力コードに登録されていません。"
                       "このセットコードは処理できません。"
                                           DELIMITED  BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
           WHEN    "20"
               MOVE    "セット内容がありません。"
                                               TO  WRK-ERRMSG
           WHEN    "21"
               MOVE    "セット内容に誤りがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "24"
               MOVE    "剤数が４０件以上です。"
                                               TO  WRK-ERRMSG
           WHEN    "25"
               MOVE    "剤明細数が４０件以上です。"
                                               TO  WRK-ERRMSG
      *
           WHEN    "28"
               MOVE    "セットコード登録エラー"
                                               TO  WRK-ERRMSG
           WHEN    "29"
               MOVE    "セット内容登録エラー"
                                               TO  WRK-ERRMSG
      *削除
           WHEN    "30"
               MOVE    "削除対象のセットがありません"
                                               TO  WRK-ERRMSG
           WHEN    "31"
               MOVE    "削除処理エラー"
                                               TO  WRK-ERRMSG
      *変更
           WHEN    "40"
               MOVE    "変更対象のセットがありません"
                                               TO  WRK-ERRMSG
           WHEN    "41"
               MOVE    "最終履歴ではありません。"
                                               TO  WRK-ERRMSG
           WHEN    "42"
               MOVE    "セットコードが存在しません。"
                                               TO  WRK-ERRMSG
           WHEN    "43"
               MOVE    "対象期間のセットが存在しません。"
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
      *    入力コードマスター読込
      *****************************************************************
       900-INPUTCD-KEYREAD-SEC         SECTION.
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-INPUTCD-READ-SEC
           ELSE
               INITIALIZE                  INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-INPUTCD-KEYREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット読込処理
      *****************************************************************
       900-INPUTSET-READ-SEC          SECTION.
      *
           PERFORM  910-DBFETCH-SEC 
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  INPUTSET-REC
               MOVE    ZERO            TO  FLG-INPUTSET
           ELSE
               INITIALIZE                  INPUTSET-REC
               MOVE    1               TO  FLG-INPUTSET
           END-IF
      *
           .
       900-INPUTSET-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力ＣＤ読込処理
      *****************************************************************
       910-INPUTCD-READ-SEC          SECTION.
      *
           PERFORM  910-DBFETCH-SEC 
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  INPUTCD-REC
               MOVE    ZERO            TO  FLG-INPUTCD
           ELSE
               INITIALIZE                  INPUTCD-REC
               MOVE    1               TO  FLG-INPUTCD
           END-IF
      *
           .
       910-INPUTCD-READ-EXT.
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
