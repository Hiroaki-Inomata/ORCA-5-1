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
       PROGRAM-ID.         ORAPI102S1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : ユーザ点数マスタ登録（xml）
      *  管理者            :
      *  作成日付    作業者        記述
      *  12/07/19    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.00    NACL-多々納  11/12/20  商品名コード金額単位対応
      *  05.01.00    NACL-多々納  17/02/XX  点数マスタ分離対応
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
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-TENSUPLUS           PIC 9(01).
      *
           03  FLG-ERR                 PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-SYORI               PIC 9(01).
           03  FLG-TENSU-ARI           PIC 9(01).
           03  FLG-TENSU-ARI2          PIC 9(01).
           03  FLG-TENSUPLUS-ARI       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDY2                    PIC 9(04).
      *
      *    カウント領域
      *01  CNT-AREA.
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
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-KEICD               PIC X(02).
      *
           03  WRK-MCP-PATHNAME        PIC X(62).
           03  WRK-MCP-WINDOW          PIC X(62).
      *
           03  WRK-OPID                PIC X(16).
      *
           03  WRK-TENS-CNT            PIC 9(02).
           03  WRK-MAE-YUKOEDYMD       PIC X(08).
      *    金額種別・単位
           03  WRK-IDO-TENSIKIBETU     PIC X(01).
           03  WRK-IDO-TANINAME        PIC X(12).
      *
       01  WRK-XML-AREA.
           03  WRK-YUKOSTYMD           PIC X(08).
           03  WRK-YUKOEDYMD           PIC X(08).
           03  WRK-SRYCD               PIC X(09).
           03  WRK-TENSU               PIC 9(09)V9(2).
           03  WRK-GAITENTTLKBN        PIC 9(03).
           03  WRK-NYUTENTTLKBN        PIC 9(03).
      *
           03  WRK-COLUMN-AREA.
               05  WRK-COLUMN-TBL          OCCURS   4.
                   07  WRK-POSITION        PIC 9(03).
                   07  WRK-NUMBER          PIC 9(03).
      *
           03  WRK-MEDICATION-AREA.
               05  WRK-MEDICATION-TBL      OCCURS   5.
                   07  WRK-POINT           PIC 9(01).
                   07  WRK-YPOSITION       PIC 9(03).
      *
           03  WRK-TANICD              PIC X(03).
           03  WRK-TANINAME            PIC X(12).
           03  WRK-H-TEN               PIC Z(8)9.
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(200).
           03  WRK-ATO-INPUT           PIC X(200).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *    文字数
           03  WRK-STRINGTBL           PIC X(200).
           03  WRK-COUNT               PIC 9(03).
      *
      *    単位の項目順テーブル
           COPY    "CMTANICD.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *    ファイルデイレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
           COPY    "CPORCXKANACONV.INC".
      *
           COPY    "CPORCSKANACHK.INC".
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK2.INC".
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPMEDICATIONV2REQ.INC".
      *    API XML読み込み用定義体
           COPY    "CPMEDICATIONV2RES.INC".
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    入力コード
        01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    ワーク点数マスタ
        01  WRK-TNS-TENSU-REC          PIC X(1000).
      *
      *    ワー点数マスタ付加コード
       01  HZN-TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC"   REPLACING
                                       //TNSP-// BY //HZN-TNSP-//.
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    職員情報
           COPY  "CPSK1010.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "ORCA-BLOB".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA102SCRAREA.INC".
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
           DISPLAY   "* medication start*"
           DISPLAY   "*******************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
      ***  INITIALIZE                      CNT-AREA
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
               PERFORM 300-MEDIXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "*******************"
           DISPLAY   "* medication end  *"
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
           INITIALIZE                      XML-MEDICATIONRES
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Patient Info"       TO  MEDIRES-RESKEY
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
           MOVE    WRK-HEN-DATE        TO  MEDIRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  MEDIRES-INFORMATION-TIME
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
           PERFORM 1000-MEDIXMLREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           MOVE    ZERO                TO  MEDIRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    APIMOD-CLASS
           WHEN    "01"
      *        登録
               MOVE    1                   TO  FLG-SYORI
           WHEN    "02"
      *        削除
               MOVE    2                   TO  FLG-SYORI
           WHEN    "03"
      *        終了
               MOVE    3                   TO  FLG-SYORI
           WHEN    "04"
      *        期間変更
               MOVE    4                   TO  FLG-SYORI
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
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
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           EVALUATE    APIMOD-CLASS
           WHEN    "01"
      *        マスタ登録処理
               PERFORM 2002-TENSU-ADD-SEC
           WHEN    "02"
      *        マスタ削除処理
               PERFORM 2003-TENSU-DEL-SEC
           WHEN    "03"
      *        マスタ終了処理
               PERFORM 2004-TENSU-END-SEC
           WHEN    "04"
      *        マスタ変更・登録
               PERFORM 2005-TENSU-UPD-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-MEDIXMLREAD-SEC   SECTION.
      *
      *    IF      MEDIRES-OBJECT      NOT =   LOW-VALUE
      *        DISPLAY "MEDIRES-OBJECT not low_value"
      *    END-IF
           INITIALIZE                      XML-MEDICATIONREQ
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE   "XMLOPEN"            TO  MCP-FUNC
           MOVE    "xml_medicationv2req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APIMOD-BODY         TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicationreq"     TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
               MOVE   "97"             TO  WRK-ERRCD
           END-IF
      *
      *
           MOVE   "XMLREAD"            TO  MCP-FUNC
           MOVE    "xml_medicationv2req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "medicationreq"     TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-MEDICATIONREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           MOVE    "XMLCLOSE"          TO  MCP-FUNC
           MOVE    "xml_medicationv2req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           END-IF
      *
           .
       1000-MEDIXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-MEDIXMLMAKE-SEC   SECTION.
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
                       MOVE    "点数マスタ登録終了"
                                           TO  MEDIRES-API-RESULT-MSG
                   WHEN    2
                       MOVE    "点数マスタ削除終了"
                                           TO  MEDIRES-API-RESULT-MSG
                   WHEN    3
                       MOVE    "点数マスタ終了日更新終了"
                                           TO  MEDIRES-API-RESULT-MSG
                   WHEN    4
                       MOVE    "点数マスタ変更・登録終了"
                                           TO  MEDIRES-API-RESULT-MSG
               END-EVALUATE
           ELSE
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD           TO  MEDIRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  MEDIRES-API-RESULT-MSG
           END-IF
      *
           IF      APIMOD-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicationv2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MEDIRES-MODE
           MOVE    "medicationres"     TO  MEDIRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICATIONRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_medicationv2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "medicationres"     TO  MEDIRES-RECORDNAME
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       XML-MEDICATIONRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               DISPLAY "XMLWRITE OK      = " MCP-RC
               CONTINUE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLCLOSE"          TO  MCP-FUNC
           MOVE    "xml_medicationv2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       XML-MEDICATIONRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               DISPLAY "XMLCLOSE OK     = " MCP-RC
               MOVE    MEDIRES-OBJECT      TO  APIMOD-BODY 
               MOVE    "application/xml"   TO  APIMOD-CONTENT-TYPE 
           ELSE
               DISPLAY "XMLCLOSE failure = " MCP-RC
               DISPLAY "appoint xml write failure "
      * CALL    "coblog"    USING   WRK-ERRMSG
           END-IF
      *
          .
       300-MEDIXMLMAKE-EXT.
           EXIT.
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
      *    コードチェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-SRYCD-CHK-SEC
           END-IF
      *    開始日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-YUKOSTYMD-CHK-SEC
           END-IF
      *    終了日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-YUKOEDYMD-CHK-SEC
           END-IF
      *    期間チェック
           IF      WRK-ERRCD           =   SPACE
               IF      WRK-YUKOSTYMD       >=  WRK-YUKOEDYMD
                   MOVE    "30"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    削除は以下チェックなし
           IF      FLG-SYORI           =   2
               GO      TO      2001-INPUT-CHK-EXT
           END-IF
      *
      *    名称チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-NAME-CHK-SEC
           END-IF
      *    カナ名称チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-KANANAME-CHK-SEC
           END-IF
      *    自費金額
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-JIHIMONEY-CHK-SEC
           END-IF
      *    金額内容
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-CONTENT-MONEY-CHK-SEC
           END-IF
      *    集計先（外来）
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-TOTAL-OUT-CHK-SEC
           END-IF
      *    集計先（入院）
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-TOTAL-IN-CHK-SEC
           END-IF
      *    自賠責集計先（外来）
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-JIBAI-OUT-CHK-SEC
           END-IF
      *    自賠責集計先（入院）
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-JIBAI-IN-CHK-SEC
           END-IF
      *    部位区分
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-BUIKBN-CHK-SEC
           END-IF
      *    コメントコード
           IF      WRK-ERRCD           =   SPACE
               IF      WRK-SRYCD(1:4)      =   "0084"
                   PERFORM 20011-COMMENT-CHK-SEC
               END-IF
           END-IF
      *    用法コード
           IF      WRK-ERRCD           =   SPACE
               IF      WRK-SRYCD(1:3)      =   "001"
                   PERFORM 20011-MEDICATION-CHK-SEC
               END-IF
           END-IF
      *    単位コード
           IF      WRK-ERRCD           =   SPACE
               IF      WRK-SRYCD(1:3)      =   "059"
                                           OR  "058"
                   PERFORM 20011-TANICD-CHK-SEC
               END-IF
           END-IF
      *    データ区分
           IF      WRK-ERRCD           =   SPACE
               IF      WRK-SRYCD(1:3)      =   "059"
                   PERFORM 20011-DATAKBN-CHK-SEC
               END-IF
           END-IF
      *    商品名称
           IF      WRK-ERRCD           =   SPACE
               IF      WRK-SRYCD(1:3)      =   "058"
                   PERFORM 20011-COMMERCIALNAME-CHK-SEC
               END-IF
           END-IF
      *    特定器材コード
           IF      WRK-ERRCD           =   SPACE
               IF      WRK-SRYCD(1:3)      =   "058"
                   PERFORM 20011-EQUIPMENT-CHK-SEC
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
      *    コード
           MOVE    MEDIREQ-MEDICATION-CODE TO  MEDIRES-MEDICATION-CODE
           MOVE    MEDIREQ-MEDICATION-CODE TO  WRK-SRYCD
      *    漢字名称
           MOVE    MEDIREQ-MEDICATION-NAME
                                           TO  MEDIRES-MEDICATION-NAME
      *    カナ名称
           MOVE    MEDIREQ-MEDICATION-KNAME
                                           TO  MEDIRES-MEDICATION-KNAME
      *    開始日
           MOVE    MEDIREQ-START-DATE      TO  MEDIRES-START-DATE
      *    数字編集
           IF      MEDIREQ-START-DATE      =   SPACE
               MOVE    ALL "0"                 TO  WRK-YUKOSTYMD
               MOVE    WRK-YUKOSTYMD           TO  WRK-SYMD
               MOVE    ZERO                    TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE            TO  MEDIRES-START-DATE
           ELSE
               MOVE    MEDIREQ-START-DATE      TO  WRK-HEN-DATE
               MOVE    ZERO                    TO  WRK-HEN-TIME
               PERFORM 802-DAYHEN02-SEC
               MOVE    WRK-SYMD                TO  WRK-YUKOSTYMD
           END-IF
      *    終了日
           MOVE    MEDIREQ-END-DATE        TO  MEDIRES-END-DATE
      *    数字編集
           IF      MEDIREQ-END-DATE        =   SPACE
               MOVE    ALL "9"                 TO  WRK-YUKOEDYMD
               MOVE    WRK-YUKOEDYMD           TO  WRK-SYMD
               MOVE    ZERO                    TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE            TO  MEDIRES-END-DATE
           ELSE
               MOVE    MEDIREQ-END-DATE        TO  WRK-HEN-DATE
               MOVE    ZERO                    TO  WRK-HEN-TIME
               PERFORM 802-DAYHEN02-SEC
               MOVE    WRK-SYMD                TO  WRK-YUKOEDYMD
           END-IF
      *    自費コード領域
      *    自費金額
           MOVE   MEDIREQ-AMOUNT-MONEY     TO  MEDIRES-AMOUNT-MONEY
      *    金額内容（4:税込み）
           MOVE   MEDIREQ-CONTENT-MONEY    TO  MEDIRES-CONTENT-MONEY
      *    集計先（外来）
           MOVE   MEDIREQ-TOTAL-OUT        TO  MEDIRES-TOTAL-OUT
      *    集計先（入院）
           MOVE   MEDIREQ-TOTAL-IN         TO  MEDIRES-TOTAL-IN
      *    自賠責集計先（外来）
           MOVE   MEDIREQ-LIABILITY-TOTAL-OUT
                                       TO  MEDIRES-LIABILITY-TOTAL-OUT
      *    自賠責集計先（入院）
           MOVE   MEDIREQ-LIABILITY-TOTAL-IN
                                       TO  MEDIRES-LIABILITY-TOTAL-IN
      *    部位区分
           MOVE   MEDIREQ-LOCATION-CATEGORY
                                           TO  MEDIRES-LOCATION-CATEGORY
      *    コメントコード領域
           MOVE    MEDIREQ-COMMENT-AREA    TO  MEDIRES-COMMENT-AREA
      *    用法コード領域
           MOVE    MEDIREQ-MEDICATION-AREA TO  MEDIRES-MEDICATION-AREA
      *    特定器材コード領域
      *    単位コード
           MOVE    MEDIREQ-UNIT-CODE       TO  MEDIRES-UNIT-CODE
      *    データ区分
           MOVE    MEDIREQ-DATA-CATEGORY   TO  MEDIRES-DATA-CATEGORY
      *    商品名称
           MOVE    MEDIREQ-COMMERCIALNAME  TO  MEDIRES-COMMERCIALNAME
      *    特定器材コード
           MOVE    MEDIREQ-EQUIPMENT-CODE  TO  MEDIRES-EQUIPMENT-CODE
      *
      *    必須入力チェック
      *    診療コード
           IF     (WRK-SRYCD           =   SPACE  )
               MOVE    "01"                TO  WRK-ERRCD
           END-IF
           .
       20010-INPUT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    コードチェック
      *****************************************************************
       20011-SRYCD-CHK-SEC    SECTION.
      *
      *    用法・部位・自費・特定器材・商品名・ユーザコメントのみ
           IF     (WRK-SRYCD(1:3)      =   "001"
                                       OR  "002"
                                       OR  "095"
                                       OR  "096"
                                       OR  "058"
                                       OR  "059" )
             OR   (WRK-SRYCD(1:4)      =   "0082"
                                       OR  "0083"
                                       OR  "0084"
                                       OR  "0085"
                                       OR  "0086" )
               IF      WRK-SRYCD       NOT NUMERIC
                   MOVE    "11"            TO  WRK-ERRCD
               END-IF
           ELSE
               MOVE    "11"            TO  WRK-ERRCD
           END-IF
           .
       20011-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    漢字名称チェック
      *****************************************************************
       20011-NAME-CHK-SEC    SECTION.
      *
      *    漢字名称
           IF     (MEDIREQ-MEDICATION-NAME =   SPACE  )
               MOVE    "12"                TO  WRK-ERRCD
           END-IF
      *
           MOVE    MEDIREQ-MEDICATION-NAME TO  WRK-MAE-INPUT
           MOVE    100                 TO  WRK-LEN
           PERFORM 890-HALF-ZEN-SEC
           MOVE    WRK-ATO-INPUT       TO  MEDIRES-MEDICATION-NAME
           IF      FLG-ERR             =   1
               MOVE    "12"                TO  WRK-ERRCD
           END-IF
           .
       20011-NAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    カナ漢字名称チェック
      *****************************************************************
       20011-KANANAME-CHK-SEC    SECTION.
      *
           MOVE    MEDIREQ-MEDICATION-KNAME TO  WRK-MAE-INPUT
           MOVE    100                 TO  WRK-LEN
           PERFORM 890-HALF-ZEN-SEC
           MOVE    WRK-ATO-INPUT       TO  MEDIRES-MEDICATION-KNAME
           IF      FLG-ERR             =   1
               MOVE    "13"                TO  WRK-ERRCD
           END-IF
           .
       20011-KANANAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    開始日チェック
      *****************************************************************
       20011-YUKOSTYMD-CHK-SEC    SECTION.
      *
            IF      WRK-YUKOSTYMD       =   "00000000"
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
                   MOVE    "14"                TO  WRK-ERRCD
                   DISPLAY "Start Ymd =  " WRK-YUKOSTYMD
               END-IF
           END-IF
      *
           .
       20011-YUKOSTYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了日チェック
      *****************************************************************
       20011-YUKOEDYMD-CHK-SEC    SECTION.
      *
           IF      WRK-YUKOEDYMD       =   "99999999"
               CONTINUE
           ELSE
      *    日付チェックサブ
               MOVE    SPACE               TO  STS-AREA-DAY
               INITIALIZE                      STS-AREA-DAY
               MOVE    SPACE               TO  LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-YUKOEDYMD       TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY2-AREA
               IF      STS-DAY-RC1     NOT =   ZERO
                   MOVE    "15"                TO  WRK-ERRCD
                   DISPLAY "End  Ymd =  " WRK-YUKOEDYMD
               END-IF
           END-IF
      *
           .
       20011-YUKOEDYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費金額・点数チェック処理
      *****************************************************************
       20011-JIHIMONEY-CHK-SEC         SECTION.
      *
           IF      MEDIRES-AMOUNT-MONEY    =   SPACE
               MOVE    ZERO                TO  WRK-TENSU
           ELSE
              IF     (WRK-SRYCD(1:3)       =   "095"
                                           OR  "096" )
      *            自費金額
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    MEDIRES-AMOUNT-MONEY    TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF     (SNUM-RC         NOT =   ZERO  )   OR
                          (SNUM-SEISUU         >   9     )   OR
                          (SNUM-SYOKBN         =   "1"   )   OR
                          (SNUM-MINKBN         =   "1"   )
                       MOVE    "16"                TO  WRK-ERRCD
                   ELSE
                       MOVE    SNUM-OUTNUM         TO  WRK-TENSU
                   END-IF
               ELSE
      *            点数
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    MEDIRES-AMOUNT-MONEY    TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF     (SNUM-RC         NOT =   ZERO  )   OR
                          (SNUM-SEISUU         >   8     )   OR
                          (SNUM-SYOSUU         >   2     )   OR
                          (SNUM-MINKBN         =   "1"   )
                       MOVE    "29"                TO  WRK-ERRCD
                   ELSE
                       MOVE    SNUM-OUTNUM         TO  WRK-TENSU
                   END-IF
               END-IF
           END-IF
      *
           .
       20011-JIHIMONEY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費金額内容チェック処理
      *****************************************************************
       20011-CONTENT-MONEY-CHK-SEC         SECTION.
      *
           EVALUATE    MEDIRES-CONTENT-MONEY
               WHEN    SPACE
               WHEN    "0"
                   CONTINUE
               WHEN    "4"
                   IF     (WRK-YUKOSTYMD       >=  "20070401")  AND
                          (WRK-SRYCD(1:3)      =   "096"     )
                       CONTINUE
                   ELSE
                       MOVE    "17"                TO  WRK-ERRCD
                   END-IF
               WHEN    OTHER
                   MOVE    "17"                TO  WRK-ERRCD
           END-EVALUATE
           .
       20011-CONTENT-MONEY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計先（外来）チェック処理
      *****************************************************************
       20011-TOTAL-OUT-CHK-SEC         SECTION.
      *
           IF      MEDIRES-TOTAL-OUT   =   SPACE
               MOVE    ZERO                TO  WRK-GAITENTTLKBN
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    MEDIRES-TOTAL-OUT   TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-SEISUU         >   3     )   OR
                      (SNUM-SYOKBN         =   "1"   )   OR
                      (SNUM-MINKBN         =   "1"   )
                   MOVE    "18"                TO  WRK-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-GAITENTTLKBN
               END-IF
           END-IF
           .
       20011-TOTAL-OUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    集計先（外来）チェック処理
      *****************************************************************
       20011-TOTAL-IN-CHK-SEC         SECTION.
      *
           IF      MEDIRES-TOTAL-IN    =   SPACE
               MOVE    ZERO                TO  WRK-NYUTENTTLKBN
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    MEDIRES-TOTAL-IN    TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-SEISUU         >   3     )   OR
                      (SNUM-SYOKBN         =   "1"   )   OR
                      (SNUM-MINKBN         =   "1"   )
                   MOVE    "19"                TO  WRK-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-NYUTENTTLKBN
               END-IF
           END-IF
           .
       20011-TOTAL-IN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    自賠責集計先（外来）チェック処理
      *****************************************************************
       20011-JIBAI-OUT-CHK-SEC         SECTION.
      *
           EVALUATE    MEDIRES-LIABILITY-TOTAL-OUT
               WHEN    SPACE
               WHEN    "1"
               WHEN    "2"
               WHEN    "3"
               WHEN    "4"
               WHEN    "5"
                   CONTINUE
               WHEN    OTHER
                   MOVE    "20"                TO  WRK-ERRCD
           END-EVALUATE
           .
       20011-JIBAI-OUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    自賠責集計先（外来）チェック処理
      *****************************************************************
       20011-JIBAI-IN-CHK-SEC         SECTION.
      *
           EVALUATE    MEDIRES-LIABILITY-TOTAL-IN
               WHEN    SPACE
               WHEN    "1"
               WHEN    "2"
               WHEN    "3"
               WHEN    "4"
               WHEN    "5"
                   CONTINUE
               WHEN    OTHER
                   MOVE    "21"                TO  WRK-ERRCD
           END-EVALUATE
           .
       20011-JIBAI-IN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    部位区分コードチェック
      *****************************************************************
       20011-BUIKBN-CHK-SEC         SECTION.
      *
           EVALUATE    MEDIRES-LOCATION-CATEGORY
               WHEN    "0"
               WHEN    "1"
               WHEN    "2"
               WHEN    "3"
               WHEN    "4"
               WHEN    "5"
               WHEN    "6"
               WHEN    "7"
               WHEN    "8"
                   CONTINUE
               WHEN    SPACE
                   IF      WRK-SRYCD(1:3)      =   "002"
                       MOVE    "0"         TO  MEDIRES-LOCATION-CATEGORY
                   END-IF
               WHEN    OTHER
                   MOVE    "22"                TO  WRK-ERRCD
           END-EVALUATE
      *
           .
       20011-BUIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    コメントコードチェック
      *****************************************************************
       20011-COMMENT-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDY
           INITIALIZE                      WRK-COLUMN-AREA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   4    )
                           OR (WRK-ERRCD   NOT =   SPACE )
      *        開始位置
               INITIALIZE                      ORCSNUMAREA
               MOVE    MEDIRES-COLUMN-POSITION (IDX)   TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-SEISUU         >   3     )   OR
                      (SNUM-SYOKBN         =   "1"   )   OR
                      (SNUM-MINKBN         =   "1"   )
                   MOVE    "23"                TO  WRK-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-POSITION (IDX)
               END-IF
      *        桁数
               INITIALIZE                      ORCSNUMAREA
               MOVE    MEDIRES-DIGIT-NUMBER (IDX)  TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-SEISUU         >   3     )   OR
                      (SNUM-SYOKBN         =   "1"   )   OR
                      (SNUM-MINKBN         =   "1"   )
                   MOVE    "23"                TO  WRK-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-NUMBER (IDX)
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   4    )
                           OR (WRK-ERRCD   NOT =   SPACE )
               IF     (WRK-POSITION (IDX)  =   ZERO )
                  AND (WRK-NUMBER   (IDX)  =   ZERO )
                   CONTINUE
               ELSE
                   IF     (WRK-POSITION (IDX)  =   ZERO )
      *                位置は必須
                       MOVE    "23"            TO  WRK-ERRCD
                   ELSE
      *                埋め込みに空きがないこと
                       ADD     1               TO  IDY
                       IF      IDY         NOT =   IDX
                           MOVE    "23"            TO  WRK-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *    コメント位置は４までとする
           IF      (MEDIRES-COLUMN-POSITION(5) =   SPACE
                                               OR  ZERO  )
              AND  (MEDIRES-DIGIT-NUMBER (5)   =   SPACE
                                               OR  ZERO  )
               CONTINUE
           ELSE
               MOVE    "23"                TO  WRK-ERRCD
           END-IF
           .
       20011-COMMENT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    用法コードチェック
      *****************************************************************
       20011-MEDICATION-CHK-SEC         SECTION.
      *
           INITIALIZE                      WRK-MEDICATION-AREA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   5    )
      *        服用時点
               IF      MEDIRES-MEDICATION-POINT(IDX)
                                               =   "0"
                                               OR  "1"
                                               OR  SPACE
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    MEDIRES-MEDICATION-POINT(IDX)
                                               TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   MOVE    SNUM-OUTNUM         TO  WRK-POINT (IDX)
               ELSE
                    MOVE    "24"           TO  WRK-ERRCD
               END-IF
           END-PERFORM
      *    カラム位置
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   5    )
                           OR (WRK-ERRCD   NOT =   SPACE )
      *        開始位置
               INITIALIZE                      ORCSNUMAREA
               MOVE    MEDIRES-COLUMN-POSITION (IDX)   TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-SEISUU         >   3     )   OR
                      (SNUM-SYOKBN         =   "1"   )   OR
                      (SNUM-MINKBN         =   "1"   )
                   MOVE    "23"                TO  WRK-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-YPOSITION(IDX)
               END-IF
           END-PERFORM
      *    埋め込みに空きがないこと
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   5    )
                           OR (WRK-ERRCD   NOT =   SPACE )
               IF     (WRK-YPOSITION (IDX)  =   ZERO )
                   CONTINUE
               ELSE
                   ADD     1               TO  IDY
                   IF      IDY         NOT =   IDX
                       MOVE    "23"            TO  WRK-ERRCD
                   END-IF
               END-IF
           END-PERFORM
      *
      *    コメント区分
           IF      MEDIRES-MEDICATION-CATEGORY =   SPACE
               MOVE    "0"             TO  MEDIRES-MEDICATION-CATEGORY
           END-IF
           EVALUATE    MEDIRES-MEDICATION-CATEGORY
               WHEN    "0"
               WHEN    "1"
               WHEN    "2"
                   CONTINUE
               WHEN    OTHER
                   IF      WRK-ERRCD           =   SPACE
                       MOVE    "36"                TO  WRK-ERRCD
                   END-IF
           END-EVALUATE
           .
       20011-MEDICATION-CHK-EXT.
           EXIT.
      *****************************************************************
      *    単位コードチェック
      *****************************************************************
       20011-TANICD-CHK-SEC         SECTION.
      *
           IF      MEDIRES-UNIT-CODE   =   SPACE
               MOVE    ZERO            TO  MEDIRES-UNIT-CODE
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    MEDIRES-UNIT-CODE   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   3     )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "25"                TO  WRK-ERRCD
           ELSE
               MOVE    SNUM-OUTEDT(13:3)   TO  MEDIRES-UNIT-CODE
               MOVE    MEDIRES-UNIT-CODE   TO  WRK-TANICD
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    SPACE               TO  WRK-TANINAME
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   MTANI-MAX )   OR
                              (FLG-OK  =   1   )
               IF      WRK-TANICD          =   MTANI-TANICD (IDX)
                   MOVE    MTANI-TANINAME(IDX)
                                               TO  WRK-TANINAME
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "25"                TO  WRK-ERRCD
           END-IF
           .
       20011-TANICD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ区分チェック
      *****************************************************************
       20011-DATAKBN-CHK-SEC         SECTION.
      *
           IF      MEDIRES-DATA-CATEGORY   =   SPACE
                                           OR  "0"
                                           OR  "3"
               CONTINUE
           ELSE
               MOVE    "26"                TO  WRK-ERRCD
           END-IF
           .
       20011-DATAKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    商品名称チェック
      *****************************************************************
       20011-COMMERCIALNAME-CHK-SEC         SECTION.
      *
           IF      MEDIREQ-COMMERCIALNAME  =   SPACE
               MOVE    "27"                TO  WRK-ERRCD
           END-IF
      *
           MOVE    MEDIREQ-COMMERCIALNAME  TO  WRK-MAE-INPUT
           MOVE    200                     TO  WRK-LEN
           PERFORM 890-HALF-ZEN-SEC
           MOVE    WRK-ATO-INPUT       TO  MEDIRES-COMMERCIALNAME
           IF      FLG-ERR             =   1
               MOVE    "27"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-COMMERCIALNAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定器材コードチェック
      *****************************************************************
       20011-EQUIPMENT-CHK-SEC         SECTION.
      *
           IF     (MEDIREQ-EQUIPMENT-CODE  =   SPACE )
              OR  (MEDIREQ-EQUIPMENT-CODE(1:1) NOT =   "7" )
               MOVE    "28"                TO  WRK-ERRCD
           ELSE
      *        点数マスタ存在チェック
               INITIALIZE                      TNS-TENSU-REC
               MOVE    MEDIREQ-EQUIPMENT-CODE  TO  TNS-SRYCD
               PERFORM 900-TENSU-READKEY-SEC 
               IF      FLG-TENSU           =   1
                   MOVE    "28"                TO  WRK-ERRCD
               END-IF
      *        金額種別・単位
               MOVE    TNS-TENSIKIBETU     TO  WRK-IDO-TENSIKIBETU
               MOVE    TNS-TANINAME        TO  WRK-IDO-TANINAME
           END-IF
      *
           .
       20011-EQUIPMENT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ登録処理
      *****************************************************************
       2002-TENSU-ADD-SEC       SECTION.
      *
      *    点数マスタ存在チェック
           PERFORM 20020-TENSU-CHECK-SEC
           IF     (FLG-TENSU-ARI       =   1   )  OR
                  (FLG-TENSU-ARI2      =   1   )
              MOVE    "31"                 TO  WRK-ERRCD
           END-IF
      *
      *    マスタ内容編集
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20021-MEDIRES-HEN-SEC
           END-IF
      *
      *    登録処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20022-TENSU-INS-SEC
           END-IF
           .
       2002-TENSU-ADD-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ存在チェック処理
      *****************************************************************
       20020-TENSU-CHECK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU-ARI
           MOVE    ZERO                TO  FLG-TENSU-ARI2
           MOVE    SPACE               TO  WRK-TNS-TENSU-REC
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
           PERFORM UNTIL      FLG-TENSU    =   1
               IF     ((TNS-YUKOSTYMD      <=  WRK-YUKOSTYMD )  AND
                       (TNS-YUKOEDYMD      >=  WRK-YUKOSTYMD ))
               AND    ((TNS-YUKOSTYMD      <=  WRK-YUKOEDYMD )  AND
                       (TNS-YUKOEDYMD      >=  WRK-YUKOEDYMD ))
                   MOVE    1                   TO  FLG-TENSU-ARI
                   MOVE    TNS-TENSU-REC   TO  WRK-TNS-TENSU-REC
               ELSE
                   IF      (TNS-YUKOSTYMD      <=  WRK-YUKOEDYMD )
                      AND  (TNS-YUKOEDYMD      >=  WRK-YUKOEDYMD )
                       MOVE    1                   TO  FLG-TENSU-ARI2
                   END-IF
                   IF      (TNS-YUKOSTYMD      <=  WRK-YUKOSTYMD )
                      AND  (TNS-YUKOEDYMD      >=  WRK-YUKOSTYMD )
                       MOVE    1                   TO  FLG-TENSU-ARI2
                   END-IF
               END-IF
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           END-PERFORM
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       20020-TENSU-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    マスタ情報内容編集処理
      *****************************************************************
       20021-MEDIRES-HEN-SEC        SECTION.
      *
           MOVE    SPACE               TO  TNS-TENSU-REC
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-YUKOSTYMD       TO  TNS-YUKOSTYMD
           MOVE    WRK-YUKOEDYMD       TO  TNS-YUKOEDYMD
      *    カナ名称
           MOVE    MEDIRES-MEDICATION-KNAME
                                       TO  TNS-KANANAME
           MOVE    MEDIRES-MEDICATION-KNAME
                                       TO  WRK-STRINGTBL
           MOVE    100                 TO  WRK-LEN
           PERFORM 890-MOJICOUNT-SEC
           MOVE    WRK-COUNT           TO  TNS-KANAYUKOKETA
      *    漢字名称
           MOVE    MEDIRES-MEDICATION-NAME TO  TNS-NAME
           MOVE    MEDIRES-MEDICATION-NAME
                                       TO  WRK-STRINGTBL
           MOVE    100                 TO  WRK-LEN
           PERFORM 890-MOJICOUNT-SEC
           MOVE    WRK-COUNT           TO  TNS-YUKOKETA
      *    コメント埋め込み位置
           IF      WRK-SRYCD (1:4)     =   "0084"
               MOVE    WRK-POSITION (1)    TO  TNS-SSTKIJUNCD(1)
               MOVE    WRK-NUMBER   (1)    TO  TNS-SSTKIJUNCD(2)
               MOVE    WRK-POSITION (2)    TO  TNS-SSTKIJUNCD(3)
               MOVE    WRK-NUMBER   (2)    TO  TNS-SSTKIJUNCD(4)
               MOVE    WRK-POSITION (3)    TO  TNS-SSTKIJUNCD(5)
               MOVE    WRK-NUMBER   (3)    TO  TNS-SSTKIJUNCD(6)
               MOVE    WRK-POSITION (4)    TO  TNS-SSTKIJUNCD(7)
               MOVE    WRK-NUMBER   (4)    TO  TNS-SSTKIJUNCD(8)
           END-IF
      *    自費コード
           IF      WRK-SRYCD (1:3)     =   "095"
                                       OR  "096"
      *        金額
               MOVE    WRK-TENSU           TO  TNS-TEN
      *        保険適用外
               MOVE    2                   TO  TNS-HKNTEKKBN
      *        集計先
               MOVE    WRK-GAITENTTLKBN    TO  TNS-GAITENTTLKBN
               MOVE    WRK-NYUTENTTLKBN    TO  TNS-NYUTENTTLKBN
      *        金額内容（４：税込）
               IF      MEDIRES-CONTENT-MONEY   =   SPACE
                   MOVE    "0"             TO  MEDIRES-CONTENT-MONEY
               END-IF
               MOVE    MEDIRES-CONTENT-MONEY   TO  TNS-TENSIKIBETU
      *        自賠責集計先
               IF      WRK-SRYCD (1:5)     =   "09593"
                   IF     MEDIRES-LIABILITY-TOTAL-OUT  =   SPACE
                       MOVE    "5"     TO  MEDIRES-LIABILITY-TOTAL-OUT
                   END-IF
                   IF     MEDIRES-LIABILITY-TOTAL-IN   =   SPACE
                       MOVE    "5"     TO  MEDIRES-LIABILITY-TOTAL-IN
                   END-IF
                   MOVE   MEDIRES-LIABILITY-TOTAL-OUT
                                               TO  TNS-NAMECHGKBN
                   MOVE   MEDIRES-LIABILITY-TOTAL-IN
                                               TO  TNS-KANANAMECHGKBN
               END-IF
           END-IF
      *    部位コード
           IF      WRK-SRYCD (1:3)     =   "002"
      *        部位区分
               MOVE  MEDIRES-LOCATION-CATEGORY     TO  TNS-BUIKBN
           END-IF
      *    用法コード
           IF      WRK-SRYCD (1:3)     =   "001"
      *        起床
               MOVE    WRK-POINT  (1)      TO  TNS-SSTKIJUNCD(5)
      *        朝
               MOVE    WRK-POINT  (2)      TO  TNS-SSTKIJUNCD(1)
      *        昼
               MOVE    WRK-POINT  (3)      TO  TNS-SSTKIJUNCD(2)
      *        夕
               MOVE    WRK-POINT  (4)      TO  TNS-SSTKIJUNCD(3)
      *        寝前
               MOVE    WRK-POINT  (5)      TO  TNS-SSTKIJUNCD(4)
      *        カラム位置
               MOVE    WRK-YPOSITION (1)   TO  TNS-SSTKIJUNCD(6)
               MOVE    WRK-YPOSITION (2)   TO  TNS-SSTKIJUNCD(7)
               MOVE    WRK-YPOSITION (3)   TO  TNS-SSTKIJUNCD(8)
               MOVE    WRK-YPOSITION (4)   TO  TNS-SSTKIJUNCD(9)
               MOVE    WRK-YPOSITION (5)   TO  TNS-SSTKIJUNCD(10)
      *        コメント区分
               MOVE    MEDIREQ-MEDICATION-CATEGORY
                                           TO  TNS-KENTAICOMMENT
           END-IF
      *    器材コード
           IF      WRK-SRYCD (1:3)     =   "059"
      *        金額
               MOVE    WRK-TENSU           TO  TNS-TEN
      *        金額種別（金額）
               MOVE    1                   TO  TNS-TENSIKIBETU
      *        データ区分
               IF      MEDIRES-DATA-CATEGORY   =   "3"
                   MOVE    3               TO  TNS-DATAKBN
               END-IF
      *        単位区分
               MOVE    WRK-TANICD          TO  TNS-TANICD
      *        単位名称
               MOVE    WRK-TANINAME        TO  TNS-TANINAME
               MOVE    WRK-TANINAME        TO  WRK-STRINGTBL
               MOVE    12                  TO  WRK-LEN
               PERFORM 890-MOJICOUNT-SEC
               MOVE    WRK-COUNT           TO  TNS-TANIMOJISU
      *        上限・下限年齢
               MOVE    "00"                TO  TNS-KGNAGE
               MOVE    "00"                TO  TNS-JGNAGE
           END-IF
      *    器材商品名コード
           IF      WRK-SRYCD (1:3)     =   "058"
      *        商品名称
               MOVE    MEDIREQ-COMMERCIALNAME  TO  TNS-FORMALNAME
               MOVE    MEDIREQ-COMMERCIALNAME  TO  WRK-STRINGTBL
               MOVE    200                     TO  WRK-LEN
               PERFORM 890-MOJICOUNT-SEC
               MOVE    WRK-COUNT               TO  TNS-FORMALYUKOKETA
      *
               MOVE    MEDIREQ-EQUIPMENT-CODE  TO  TNS-IDOKANREN
      *        金額
               IF     (WRK-IDO-TENSIKIBETU     =   "2"  )
                  AND (WRK-TENSU           NOT =   ZERO )
      *            金額
                   MOVE    WRK-TENSU           TO  TNS-TEN
      *            金額種別（金額）
                   MOVE    1                   TO  TNS-TENSIKIBETU
               ELSE
                   MOVE    SPACE               TO  MEDIRES-AMOUNT-MONEY
               END-IF
      *        単位区分
               IF     (WRK-IDO-TANINAME        =   SPACE )
                 AND  (WRK-TANINAME        NOT =   SPACE )
                   MOVE    WRK-TANICD          TO  TNS-TANICD
      *            単位名称
                   MOVE    WRK-TANINAME        TO  TNS-TANINAME
                   MOVE    WRK-TANINAME        TO  WRK-STRINGTBL
                   MOVE    12                  TO  WRK-LEN
                   PERFORM 890-MOJICOUNT-SEC
                   MOVE    WRK-COUNT           TO  TNS-TANIMOJISU
               ELSE
                   MOVE    SPACE               TO  MEDIRES-UNIT-CODE
               END-IF
           END-IF
      *
           .
       20021-MEDIRES-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ追加処理
      *****************************************************************
       20022-TENSU-INS-SEC              SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-OPID            TO  TNS-OPID
      *
           MOVE    SMCNDATE-YMD        TO  TNS-CREYMD
           MOVE    SMCNDATE-YMD        TO  TNS-UPYMD
           MOVE    SMCNDATE-HMS        TO  TNS-UPHMS
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "51"                TO  WRK-ERRCD
               DISPLAY "TENSU INS ERR:" MCP-RC
                       ",KEY:" TNS-KEY
           END-IF
      *
           .
       20022-TENSU-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ削除処理
      *****************************************************************
       2003-TENSU-DEL-SEC       SECTION.
      *
      *    点数マスタ存在チェック
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-TENSU-READ-SEC
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
           MOVE    ZERO                TO  WRK-TENS-CNT
           MOVE    SPACE               TO  WRK-TNS-TENSU-REC
           MOVE    ZERO                TO  FLG-TENSU-ARI
           PERFORM UNTIL      FLG-TENSU    =   1
               IF     (TNS-YUKOSTYMD       <=  WRK-YUKOSTYMD )
               AND    (TNS-YUKOEDYMD       >=  WRK-YUKOEDYMD )
                   MOVE    1               TO  FLG-TENSU-ARI
                   MOVE    TNS-TENSU-REC   TO  WRK-TNS-TENSU-REC
               ELSE
                   ADD     1               TO  WRK-TENS-CNT
               END-IF
               PERFORM 900-TENSU-READ-SEC
           END-PERFORM
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-TENSU-ARI       =   ZERO
               MOVE    "32"                TO  WRK-ERRCD
           ELSE
               MOVE    WRK-TNS-TENSU-REC   TO  TNS-TENSU-REC
           END-IF
      *    点数マスタ・点数付加情報削除
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20031-TENSU-DELETE-SEC
               IF      WRK-ERRCD           =   SPACE
      *            削除内容送信
                   PERFORM 20032-DEL-XMHEN-SEC
               END-IF
           END-IF
      *    入力コード削除
           IF     (WRK-ERRCD           =   SPACE )
              AND (WRK-TENS-CNT        =   ZERO  )
               PERFORM 20033-INPUTCD-DELETE-SEC
           END-IF
           .
       2003-TENSU-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除内容送信処理
      *****************************************************************
       20032-DEL-XMHEN-SEC           SECTION.
      *
      *    漢字名称
           MOVE    TNS-NAME                TO  MEDIRES-MEDICATION-NAME
      *    カナ名称
           MOVE    TNS-KANANAME            TO  MEDIRES-MEDICATION-KNAME
      *    開始日
           MOVE    TNS-YUKOSTYMD           TO  WRK-SYMD
           MOVE    ZERO                    TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE            TO  MEDIRES-START-DATE
      *    終了日
           MOVE    TNS-YUKOEDYMD           TO  WRK-SYMD
           MOVE    ZERO                    TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE            TO  MEDIRES-END-DATE
      *    金額
           MOVE    TNS-TEN                 TO  WRK-H-TEN
           MOVE    WRK-H-TEN               TO  MEDIRES-AMOUNT-MONEY
           .
       20032-DEL-XMHEN-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ削除処理
      *****************************************************************
       20031-TENSU-DELETE-SEC           SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF     MCP-RC           NOT =   ZERO
               DISPLAY "TENSU DEL ERR!!"     MCP-RC
                       ",KEY:" TNS-KEY
               MOVE    "53"                 TO  WRK-ERRCD
           END-IF
      *
      *    点数付加情報削除
      *    INITIALIZE                      TENSUPLUS-REC
      *    MOVE    TNS-SRYCD           TO  TNSP-SRYCD
      *    MOVE    TNS-YUKOSTYMD       TO  TNSP-YUKOSTYMD
      *    MOVE    TNS-YUKOEDYMD       TO  TNSP-YUKOEDYMD
      *    MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
      *    MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
      *    MOVE    "DBDELETE"          TO  MCP-FUNC
      *    MOVE    "tbl_tensuplus"     TO  MCP-TABLE
      *    MOVE    "key"               TO  MCP-PATHNAME
      *    CALL    "ORCDBMAIN"         USING   MCPAREA
      *                                        MCPDATA-REC
      *                                        SPA-AREA
      *    IF      MCP-RC          NOT =   ZERO
      *        DISPLAY "TENSUPLUS DEL ERR!!"   MCP-RC
      **               ",KEY:"   TNS-KEY
      *        MOVE    "53"                 TO  WRK-ERRCD
      *    END-IF
      *
           .
       20031-TENSU-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード削除
      *****************************************************************
       20033-INPUTCD-DELETE-SEC     SECTION.
      *
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    WRK-SRYCD           TO  ICD-SRYCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "del22"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    点数付加情報削除
           INITIALIZE                  TENSUPLUS-REC
           MOVE    WRK-SRYCD           TO  TNSP-SRYCD
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       20033-INPUTCD-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    マスタ終了登録処理
      *****************************************************************
       2004-TENSU-END-SEC             SECTION.
      *
      *    点数マスタ存在チェック
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    WRK-YUKOSTYMD       TO  TNS-YUKOSTYMD
           MOVE    WRK-YUKOSTYMD       TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-TENSU-READ-SEC
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-TENSU           =   ZERO
               IF      TNS-YUKOEDYMD   NOT =   "99999999"
                   MOVE    "33"            TO  WRK-ERRCD
               END-IF
               IF      TNS-YUKOSTYMD   NOT =   WRK-YUKOSTYMD
                   MOVE    "33"            TO  WRK-ERRCD
               END-IF
           ELSE
               MOVE    "34"                TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        終了日設定
               MOVE    TNS-SRYCD               TO  TNSUP-SRYCD
               MOVE    TNS-YUKOSTYMD           TO  TNSUP-YUKOSTYMD
               MOVE    TNS-YUKOEDYMD           TO  TNSUP-YUKOEDYMD
               MOVE    WRK-YUKOEDYMD           TO  TNS-YUKOEDYMD
      *
               PERFORM 20041-TENSU-UPDATE-SEC
      *v5.1
      *付加情報登録なし
      *        付加情報更新
      *        IF      WRK-ERRCD           =   SPACE
      *            PERFORM 20042-TENSUPLUS-UPDATE-SEC
      *****    END-IF
           END-IF
           .
       2004-TENSU-END-EXT.
           EXIT.
      *****************************************************************
      *    マスタ改正登録処理
      *****************************************************************
       20041-TENSU-UPDATE-SEC             SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  TNS-UPYMD
           MOVE    SMCNDATE-HMS        TO  TNS-UPHMS
           MOVE    WRK-OPID            TO  TNS-OPID
      *
           MOVE    SPA-HOSPNUM         TO  TNSUP-HOSPNUM
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
      *****MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              NOT =   ZERO
               DISPLAY "Z08 TENSU UPD ERR:" MCP-RC
                      ",KEY:" TNS-KEY
               MOVE    "52"                TO  WRK-ERRCD
           END-IF
           .
       20041-TENSU-UPDATE-EXT.
           EXIT.
      *
      *v5.1
      *廃止
      *****************************************************************
      *    マスタ改正登録処理
      *****************************************************************
       20042-TENSUPLUS-UPDATE-SEC             SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           MOVE    WRK-SRYCD           TO  TNSP-SRYCD
           MOVE    TNSUP-YUKOSTYMD     TO  TNSP-YUKOSTYMD
           MOVE    TNSUP-YUKOEDYMD     TO  TNSP-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
      *
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-TENSUPLUS-READ-SEC
           ELSE
               MOVE    1               TO  FLG-TENSUPLUS
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPACE               TO  HZN-TENSUPLUS-REC
           MOVE    ZERO                TO  FLG-TENSUPLUS-ARI
           IF      FLG-TENSUPLUS       =   ZERO
               MOVE    TENSUPLUS-REC       TO  HZN-TENSUPLUS-REC
               MOVE    1                   TO  FLG-TENSUPLUS-ARI
      *        終了日設定
               MOVE    WRK-SRYCD           TO  TNSP-UP-SRYCD
               MOVE    TNSP-YUKOSTYMD      TO  TNSP-UP-YUKOSTYMD
               MOVE    TNSP-YUKOEDYMD      TO  TNSP-UP-YUKOEDYMD
               MOVE    TNSP-HOSPNUM        TO  TNSP-UP-HOSPNUM
      *
               MOVE    TNS-YUKOEDYMD       TO  TNSP-YUKOEDYMD
      *
               MOVE    WRK-OPID            TO  TNSP-OPID
               MOVE    SMCNDATE-YMD        TO  TNSP-UPYMD
               MOVE    SMCNDATE-HMS        TO  TNSP-UPHMS
      *
               MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "52"                TO  WRK-ERRCD
                   DISPLAY "TENSUPLUS UPD ERR:" MCP-RC
                                   ",KEY:" TNSP-KEY
               END-IF
           END-IF
           .
       20042-TENSUPLUS-UPDATE-EXT.
           EXIT.
      *!!!
      *****************************************************************
      *    マスタ改正登録処理
      *****************************************************************
       2005-TENSU-UPD-SEC             SECTION.
      *
      *    今回の有効開始日の前日とする
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YUKOSTYMD       TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY6-KEISAN     TO  WRK-MAE-YUKOEDYMD
           ELSE
               MOVE    "35"                TO  WRK-ERRCD
               GO      TO      2005-TENSU-UPD-EXT
           END-IF
      *    点数マスタ存在チェック
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-TENSU-READ-SEC
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
           MOVE    ZERO                TO  FLG-TENSU-ARI
           PERFORM UNTIL     (FLG-TENSU        =   1  )
                        OR   (FLG-TENSU-ARI    =   1  )
               IF     (TNS-YUKOEDYMD       =   "99999999"    )
                   MOVE    1               TO  FLG-TENSU-ARI
               ELSE
                   PERFORM 900-TENSU-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (FLG-TENSU-ARI       =   1   )
              AND (TNS-YUKOSTYMD       <   WRK-MAE-YUKOEDYMD )
      *        期間終了日
               MOVE    TNS-SRYCD               TO  TNSUP-SRYCD
               MOVE    TNS-YUKOSTYMD           TO  TNSUP-YUKOSTYMD
               MOVE    TNS-YUKOEDYMD           TO  TNSUP-YUKOEDYMD
               MOVE    WRK-MAE-YUKOEDYMD       TO  TNS-YUKOEDYMD
      *
               PERFORM 20041-TENSU-UPDATE-SEC
      *v5.1
      *        付加情報更新
      *        IF      WRK-ERRCD           =   SPACE
      *            PERFORM 20042-TENSUPLUS-UPDATE-SEC
      ***      END-IF
           ELSE
               MOVE    "34"                TO  WRK-ERRCD
           END-IF
      *
      *    マスタ内容編集
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20021-MEDIRES-HEN-SEC
           END-IF
      *
      *    登録処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20022-TENSU-INS-SEC
      *v5.1
      *****    付加情報
      *        IF     (WRK-ERRCD           =   SPACE )
      *           AND (FLG-TENSUPLUS-ARI   =   1     )
      *            PERFORM 20023-TENSUPLUS-INS-SEC
      *******  END-IF
           END-IF
           .
       2005-TENSU-UPD-EXT.
           EXIT.
      *****************************************************************
      *    点数付加情報登録処理
      *****************************************************************
       20023-TENSUPLUS-INS-SEC          SECTION.
      *
           MOVE    HZN-TENSUPLUS-REC   TO  TENSUPLUS-REC
      *
           MOVE    TNS-YUKOSTYMD       TO  TNSP-YUKOSTYMD
           MOVE    TNS-YUKOEDYMD       TO  TNSP-YUKOEDYMD
      *
           MOVE    WRK-OPID            TO  TNSP-OPID
           MOVE    SMCNDATE-YMD        TO  TNSP-CREYMD
           MOVE    SMCNDATE-YMD        TO  TNSP-UPYMD
           MOVE    SMCNDATE-HMS        TO  TNSP-UPHMS
      *
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              NOT =   ZERO
               MOVE    "51"                TO  WRK-ERRCD
               DISPLAY "Z02 TENSUPLUS INS ERR:" MCP-RC
                      ",KEY:" TNSP-KEY
           END-IF
           .
       20023-TENSUPLUS-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       890-HALF-ZEN-SEC            SECTION.
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
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT
           END-IF
      *
           .
       890-HALF-ZEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角文字数処理
      *****************************************************************
       890-MOJICOUNT-SEC            SECTION.
      *
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    WRK-LEN BY  -1
                   UNTIL       IDX     <   1
               IF      WRK-STRINGTBL(IDX:1)    NOT =   SPACE
                   MOVE    IDX             TO  IDY
                   MOVE    1               TO  IDX
               END-IF
           END-PERFORM
           IF      IDY                 >   1
               COMPUTE WRK-COUNT       =   IDY     /   2
           ELSE
               MOVE    ZERO            TO  WRK-COUNT
           END-IF
        
           .
       890-MOJICOUNT-HEN-EXT.
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
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
           WHEN    "01"
               MOVE    "コード未設定"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "登録できるコードではありません。"
                                               TO  WRK-ERRMSG
           WHEN    "12"
               MOVE    "漢字名称がエラーです"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE    "カナ名称がエラーです"
                                               TO  WRK-ERRMSG
           WHEN    "14"
               MOVE    "開始日暦日エラー"
                                               TO  WRK-ERRMSG
           WHEN    "15"
               MOVE    "終了日暦日エラー"
                                               TO  WRK-ERRMSG
           WHEN    "16"
               MOVE    "金額は整数９桁です。"
                                               TO  WRK-ERRMSG
           WHEN    "29"
               MOVE    "点数は整数８桁・小数２桁です"
                                               TO  WRK-ERRMSG
           WHEN    "17"
               MOVE    "自費金額内容区分設定誤り"
                                               TO  WRK-ERRMSG
           WHEN    "18"
               MOVE    "点数欄集計先識別（外来）設定誤り"
                                               TO  WRK-ERRMSG
           WHEN    "19"
               MOVE    "点数欄集計先識別（入院）設定誤り"
                                               TO  WRK-ERRMSG
           WHEN    "20"
               MOVE    "自賠責集計等取扱（外来）設定誤り"
                                               TO  WRK-ERRMSG
           WHEN    "21"
               MOVE    "自賠責集計等取扱（入院）設定誤り"
                                               TO  WRK-ERRMSG
           WHEN    "22"
               MOVE    "部位コード設定誤り"
                                               TO  WRK-ERRMSG
           WHEN    "23"
               MOVE    "コメントのカラム位置・桁数設定誤り"
                                               TO  WRK-ERRMSG
           WHEN    "24"
               MOVE    "服用時点区分設定誤り"
                                               TO  WRK-ERRMSG
           WHEN    "36"
               MOVE    "服用コメント区分設定誤り"
                                               TO  WRK-ERRMSG
           WHEN    "25"
               MOVE    "単位区分設定誤り"
                                               TO  WRK-ERRMSG
           WHEN    "26"
               MOVE    "データ区分設定誤り"
                                               TO  WRK-ERRMSG
           WHEN    "27"
               MOVE    "商品名称設定誤り"
                                               TO  WRK-ERRMSG
           WHEN    "28"
               MOVE    "算定特定器材コード設定誤り"
                                               TO  WRK-ERRMSG
           WHEN    "30"
               MOVE    "開始日≧終了日です。"
                                               TO  WRK-ERRMSG
           WHEN    "31"
               MOVE    "点数マスタが登録済みです。"
                                               TO  WRK-ERRMSG
           WHEN    "32"
               MOVE    "削除対象の点数マスタがありません。"
                                               TO  WRK-ERRMSG
           WHEN    "33"
               MOVE    "終了日設定対象の点数マスタがありません"
                                               TO  WRK-ERRMSG
           WHEN    "34"
               MOVE    "更新対象の点数マスタがありません。"
                                               TO  WRK-ERRMSG
           WHEN    "35"
               MOVE    "開始日＋１日の設定ができません。"
                                               TO  WRK-ERRMSG
           WHEN    "51"
               MOVE    "点数マスタ登録エラー"
                                               TO  WRK-ERRMSG
           WHEN    "52"
               MOVE    "点数マスタ更新エラー"
                                               TO  WRK-ERRMSG
           WHEN    "53"
               MOVE    "点数マスタ削除エラー"
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
       900-TENSU-READKEY-SEC              SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-YUKOSTYMD       TO  TNS-YUKOSTYMD
           MOVE    WRK-YUKOSTYMD       TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-TENSU-READ-SEC
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-TENSU-READKEY-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ読込処理
      *****************************************************************
       900-TENSU-READ-SEC              SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               MOVE    ZERO            TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
           .
      *
       900-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ読込処理
      *****************************************************************
       990-TENSUPLUS-READ-SEC              SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TENSUPLUS-REC
               MOVE    ZERO            TO  FLG-TENSUPLUS
           ELSE
               INITIALIZE                  TENSUPLUS-REC
               MOVE    1               TO  FLG-TENSUPLUS
           END-IF
           .
      *
       990-TENSUPLUS-READ-EXT.
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
