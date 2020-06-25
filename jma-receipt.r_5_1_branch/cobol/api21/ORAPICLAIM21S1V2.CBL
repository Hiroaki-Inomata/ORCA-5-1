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
       PROGRAM-ID.         ORAPICLAIM21S1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : ＣＬＡＩＭ　診療行為　登録　（xml2）
      *  管理者            :
      *  作成日付    作業者        記述
      *  16/07/XX    NACL−多々納　新規作成
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
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDY1                PIC 9(04).
           03  IDY2                PIC 9(04).
      *
           03  IDK                 PIC 9(04).
           03  IDM1                PIC 9(04).
           03  IDM2                PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SRYCNT              PIC 9(04).
           03  CNT-BYOCNT              PIC 9(04).
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
           03  WRK-WERRCD              PIC X(03).
           03  WRK-KEICD-G.
               05  WRK-KEICD           PIC X(03)   OCCURS   10.
      *
           03  WRK-ERRCD-1             PIC X(02).
           03  WRK-ERRMSG-1            PIC X(100).
           03  WRK-ERRCD-2             PIC X(02).
           03  WRK-ERRMSG-2            PIC X(100).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).
           03  WRK-MCP-WINDOW          PIC X(64).
      *
           03  WRK-OPID                PIC X(16).
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
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-NAME          PIC X(80).
           03  WRK-DRCD                PIC X(05).
           03  WRK-DRCD-NAME           PIC X(80).
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(100).
           03  WRK-ATO-INPUT           PIC X(100).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *
      *    claim連携サブリターンエリア
           COPY    "CPORCL0031.INC".
      *!
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    数字変換領域
      *    COPY    "CPORCSNUM.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPCLAIMRECEIVEV2REQ.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPCLAIMRECEIVEV2RES.INC".
      *
      *    保険組合せセット領域
      *    COPY    "CPORCSAPIHKN.INC".
      *
       01  LF04-REC.
           COPY    "CPRLF004.INC".
      *
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
      *    （診療科情報）
      *    COPY    "CPSK1005.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *    （医療機関情報）
           COPY    "CPSK1001.INC".
      *
      *    患者基本情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ワーク診療行為マスタ−
      *01  WKSRYACT-REC.
      *    COPY    "CPWKSRYACT.INC".
      *
      *    入院履歴
      *01  PTNYUINRRK-REC.
      *    COPY    "CPPTNYUINRRK.INC".
      *
      *    保険番号
      *01  HKNNUM-REC.
      *    COPY    "CPHKNNUM.INC".
      *
      *　　（ＣＬＡＩＭ接続）
      *    COPY    "CPSK9000.INC".
      *    COPY    "CPSK9001.INC".
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
           COPY    "CPAPILSTV2.INC".
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
           DISPLAY   "***************"
           DISPLAY   "* accept start*"
           DISPLAY   "***************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
           INITIALIZE                      CNT-AREA
      *
           MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
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
               MOVE   404                  TO  APILST07-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-APTXMLMAKE-SEC
           END-IF
      *
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
      *
           DISPLAY   "***************"
           DISPLAY   "* accept end  *"
           DISPLAY   "***************"
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
           INITIALIZE                      XML-CLAIM-RECEIVEV2REQ
           INITIALIZE                      XML-CLAIM-RECEIVEV2RES
      *
           INITIALIZE                  LNK-ORCL0031AREA
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *****MOVE   "Medical Info"       TO  CLRERES-RESKEY
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
           MOVE    WRK-HEN-DATE        TO  CLRERES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  CLRERES-INFORMATION-TIME
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
           IF      CLREREQ-PERFORM-TIME    =   SPACE
               MOVE    CLREREQ-ORDER-TIME      TO  WRK-HEN-DATE
           ELSE
               MOVE    CLREREQ-PERFORM-TIME    TO  WRK-HEN-DATE
           END-IF
           MOVE    SPACE                   TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-PERFORM-YMD
           MOVE    WRK-THMS                TO  WRK-PERFORM-TIME
      *    未設定時はシステム情報設定
           IF      WRK-PERFORM-YMD         =   SPACE
               MOVE    SMCNDATE-YMD        TO  WRK-PERFORM-YMD
           END-IF
      *    診療日チェック
           PERFORM 20011-PERFORM-CHK-SEC
      *    診療日をシステム日付へ
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-PERFORM-YMD     TO  SPA-SYSYMD
           ELSE
      *        エラーは他項目と同時
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
      *    ＳＰＡ共通設定
           PERFORM 1001-ORCSCOMMON-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           MOVE    ZERO                TO  CLRERES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-WERRCD
           MOVE    SPACE               TO  WRK-KEICD-G
      *
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
      *    登録処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2002-CLAIM-SYORI-SEC
           END-IF
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
           IF      APILST07-BODY     NOT =   LOW-VALUE
               DISPLAY "CLRERES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-CLAIM-RECEIVEV2REQ
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_claim_receivev2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST07-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "claim_receivev2req"
                                       TO  APIREQ-RECORDNAME
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
           MOVE    "xml_claim_receivev2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST07-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "claim_receivev2req"
                                       TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-CLAIM-RECEIVEV2REQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE           TO  APILST07-BODY
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
               MOVE    "登録処理終了"      TO  CLRERES-API-RESULT-MSG
               IF     (LNK-OUT-CNT1        =   ZERO )
                 AND  (LNK-OUT-CNT2        >   ZERO )
                   MOVE    "病名登録処理終了"
                                           TO  CLRERES-API-RESULT-MSG
               END-IF
           ELSE
               IF      WRK-ERRMSG          =   SPACE
      *        エラー・警告メッセージ
                   PERFORM 890-ERRCD-MSG-SEC
               END-IF
               MOVE    WRK-ERRCD           TO  CLRERES-API-RESULT
               MOVE    WRK-ERRMSG          TO  CLRERES-API-RESULT-MSG
           END-IF
      *!!!!
      *    各エラー、ワーニング編集
           PERFORM 3001-WAINGMSG-HEN-SEC
      *!!!! 
      *
           IF      APILST07-BODY     NOT =   LOW-VALUE
               DISPLAY "CLRERES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_claim_receivev2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  CLRERES-MODE
           MOVE    "claim_receivev2res"
                                       TO  CLRERES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-CLAIM-RECEIVEV2RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_claim_receivev2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  CLRERES-MODE
           MOVE    "claim_receivev2res"
                                       TO  CLRERES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-CLAIM-RECEIVEV2RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE  CLRERES-OBJECT     TO  APILST07-BODY 
               MOVE  "application/xml" TO  APILST07-CONTENT-TYPE
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
      *    中途データ
           IF      WRK-ERRCD-1      NOT =   SPACE
               ADD     1                   TO  IDM1
               MOVE    WRK-ERRCD-1         TO  CLRERES-WARNING
                                                               (IDM1)
               MOVE    WRK-ERRMSG-1        TO  CLRERES-WARNING-MSG
                                                               (IDM1)
           END-IF
           PERFORM VARYING     IDK     FROM    1   BY  1
                   UNTIL      (IDK     >   10   )   OR
                              (WRK-KEICD (IDK) =   SPACE)
               MOVE    WRK-KEICD (IDK)     TO  WRK-WERRCD
      *        エラー・警告メッセージ
               PERFORM 891-WARNING-MSG-SEC
               ADD     1                   TO  IDM1
               MOVE    WRK-WERRCD          TO  CLRERES-WARNING
                                                               (IDM1)
               MOVE    WRK-ERRMSG          TO  CLRERES-WARNING-MSG
                                                               (IDM1)
           END-PERFORM
      *
      *    病名
           IF      WRK-ERRCD-2      NOT =   SPACE
               ADD     1                   TO  IDM1
               MOVE    WRK-ERRCD-2         TO  CLRERES-WARNING
                                                               (IDM1)
               MOVE    WRK-ERRMSG-2        TO  CLRERES-WARNING-MSG
                                                               (IDM1)
           END-IF
          .
       3001-WAINGMSG-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
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
      *    医療機関ＩＤチェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-HOSPNUM-CHK-SEC
           END-IF
      *
      *    診療日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PERFORM-CHK-SEC
           END-IF
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PTNUM-CHK-SEC
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
           MOVE    CLREREQ-PTID1       TO  CLRERES-PATIENTID
           MOVE    CLREREQ-PTID1       TO  WRK-PTNUM
      *    診療日チェック
           IF      CLREREQ-PERFORM-TIME    =   SPACE
               MOVE    CLREREQ-ORDER-TIME      TO  WRK-HEN-DATE
           ELSE
               MOVE    CLREREQ-PERFORM-TIME    TO  WRK-HEN-DATE
           END-IF
           MOVE    SPACE                   TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-PERFORM-YMD
      *    未設定時はシステム情報設定
           IF      WRK-PERFORM-YMD         =   SPACE
               MOVE    SMCNDATE-YMD        TO  WRK-PERFORM-YMD
           END-IF
      *    診療日・時間
           MOVE    WRK-PERFORM-YMD     TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  CLRERES-PERFORM-DATE
      *    必須入力チェック
      *    患者番号
           IF      WRK-PTNUM           =   SPACE
               MOVE    "01"                TO  WRK-ERRCD
           END-IF
           .
       20010-INPUT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関ＩＤチェック
      *****************************************************************
       20011-HOSPNUM-CHK-SEC    SECTION.
      *
      *    作成者 施設ID
           IF      CLREREQ-CRE001-FACILITYID   NOT =   SPA-HOSPID
               MOVE    "02"                TO  WRK-ERRCD
           END-IF
           .
       20010-HOSPNUM-HEN-EXT.
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
           MOVE    SPTID-PTNUM         TO  CLRERES-PATIENTID
           MOVE    SPTID-PTNUM         TO  WRK-PTNUM
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
      *    中途終了データ登録処理
      *****************************************************************
       2002-CLAIM-SYORI-SEC        SECTION.
      *
      *    患者情報内容編集
           PERFORM 20021-CLRERES-HEN-SEC
      *
      *    排他チェック & ロック処理
           PERFORM 990-LOCK-IN-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               ADD     1                   TO  IDK
               MOVE    "W01"               TO  WRK-KEICD(IDK)
           END-IF
      *
      *    診療行為登録処理
           PERFORM 20021-CLAIM-CALL-SEC
      *
           PERFORM 990-LOCK-OUT-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               DISPLAY "ロック解除失敗"
           END-IF
           .
       2002-CLAIM-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    患者情報出力情報編集処理
      *****************************************************************
       20021-CLRERES-HEN-SEC     SECTION.
      *
           .
       20021-CLRERES-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為登録処理
      *****************************************************************
       20021-CLAIM-CALL-SEC        SECTION.
      *
           MOVE    SPACE               TO  LF04-REC
           INITIALIZE                  LF04-REC
      *
      * 作成者 施設ID
           MOVE    CLREREQ-CRE001-FACILITYID
                                       TO  LF04-CRE001-FACILITYID
      * 患者主ID
           MOVE    WRK-PTNUM
                                       TO  LF04-PTID1
      * オーダー発行日時 (claim:orderTime)
           MOVE    CLREREQ-ORDER-TIME
                                       TO  LF04-ORDER-TIME
      *実施日時 (claim:performTime)
           MOVE    CLREREQ-PERFORM-TIME
                                       TO  LF04-PERFORM-TIME
      * 作成者 診療科ID
           MOVE    CLREREQ-CRE001-DPERTID
                                       TO  LF04-CRE001-DPERTID
      *
      * 健康保険種別
           MOVE    CLREREQ-HKN-HKNSBT
                                       TO  LF04-HKN-HKNSBT (1)
      * 保険種別コード (mmlHi:ClassCode)
           MOVE    CLREREQ-HKN-HKNSBTCODE
                                       TO  LF04-HKN-HKNSBTCODE (1)
      * 健康保険番号
           MOVE    CLREREQ-HKN-HKNJANUM
                                       TO  LF04-HKN-HKNJANUM (1)
      * 被保険者記号
           MOVE    CLREREQ-HKN-KIGO
                                       TO  LF04-HKN-KIGO (1)
      * 被保険者番号
           MOVE    CLREREQ-HKN-NUM
                                       TO  LF04-HKN-NUM (1)
      *    公費負担医療情報
           PERFORM     VARYING   IDY   FROM    1   BY  1
                       UNTIL    (IDY           >   4)
      * 公費負担名称
               MOVE    CLREREQ-HKNKOH-PROVIDER (IDY)
                                       TO  LF04-HKNKOH-PROVIDER (1 IDY)
      * 負担者番号
               MOVE    CLREREQ-HKNKOH-FTNJANUM (IDY)
                                       TO  LF04-HKNKOH-FTNJANUM (1 IDY)
      * 受給者番号
               MOVE    CLREREQ-HKNKOH-JKYSNUM (IDY)
                                       TO  LF04-HKNKOH-JKYSNUM (1 IDY)
           END-PERFORM
      *
      * 病名
           PERFORM     VARYING   IDX   FROM    1   BY  1
                       UNTIL    (IDX           >   50)
      * 疾患コード (mmlRd:code)
               MOVE    CLREREQ-SND-SPICOD1 (IDX)
                                           TO  LF04-SND-SPICOD1 (IDX)
      * 疾患名
               MOVE    CLREREQ-SND-SPINAME1 (IDX)
                                           TO  LF04-SND-SPINAME1(IDX)
      *
               IF     (CLREREQ-SND-SPICOD1  (IDX)
                                               NOT =   SPACE )
                 OR   (CLREREQ-SND-SPINAME1 (IDX)
                                               NOT =   SPACE )
                   ADD     1                   TO  CNT-BYOCNT
               END-IF
      *
      * <疾患要素>
               PERFORM VARYING   IDY   FROM    1   BY  1
                       UNTIL    (IDY           >   6 )
      * 疾患コード (mmlRd:code)
                   MOVE    CLREREQ-SNDSPITBL-SPICOD2 (IDX IDY)
                                           TO  LF04-SNDSPITBL-SPICOD2
                                                           (IDX IDY)
      * 疾患名
                   MOVE    CLREREQ-SNDSPITBL-SPINAME2 (IDX IDY)
                                           TO  LF04-SNDSPITBL-SPINAME2
                                                           (IDX IDY)
      *
                   IF     (CLREREQ-SNDSPITBL-SPICOD2 (IDX IDY)
                                               NOT =   SPACE )
                     OR   (CLREREQ-SNDSPITBL-SPINAME2 (IDX IDY)
                                               NOT =   SPACE )
                       ADD     1                   TO  CNT-BYOCNT
                   END-IF
               END-PERFORM
      * <分類>
               PERFORM VARYING   IDY   FROM    1   BY  1
                       UNTIL    (IDY           >   4 )
      * 分類の種類コード (mmlRd:tableId)
                   MOVE    CLREREQ-SNDBNRTBL-BNRTBL1 (IDX IDY)
                                           TO  LF04-SNDBNRTBL-BNRTBL1
                                                           (IDX IDY)
      * 分類名
                   MOVE    CLREREQ-SNDBNRTBL-BNRNAME1 (IDX IDY)
                                           TO  LF04-SNDBNRTBL-BNRNAME1
                                                           (IDX IDY)
               END-PERFORM
      * 疾患開始日
               MOVE    CLREREQ-SND-SKNSTARTDATE (IDX)
                                           TO  LF04-SND-SKNSTARTDATE
                                                                  (IDX)
      * 疾患終了日
               MOVE    CLREREQ-SND-SKNENDDATE (IDX)
                                           TO  LF04-SND-SKNENDDATE(IDX)
      * 転帰
               MOVE    CLREREQ-SND-TENKI (IDX)
                                           TO  LF04-SND-TENKI  (IDX)
      * 入外区分 (mmlRd:InOutPatient)
               MOVE    CLREREQ-SND-NYUGAIKBN (IDX)
                                           TO  LF04-SND-NYUGAIKBN(IDX)
           END-PERFORM
      *
      * 文書ユニークID
           MOVE    CLREREQ-UID8        TO  LF04-UID8
      * ドクターコード
           MOVE    CLREREQ-DOCCD       TO  LF04-DOCCD
      * 入院外来区分 (true OR false) (claim:admitFlag)
           MOVE    CLREREQ-NYUGAIKBN   TO  LF04-NYUGAIKBN
      * 時間外区分 (claim:timeClass)
           MOVE    CLREREQ-JIKANNGAIKBN    TO  LF04-JIKANNGAIKBN
      * 使用保険ＵＩＤ (claim:insuranceUid)
           MOVE    CLREREQ-HOKEN-UID   TO  LF04-HOKEN-UID
      *
           PERFORM     VARYING   IDX   FROM    1   BY  1
                       UNTIL    (IDX           >   40)
      * 診療行為区分コード (claim:classCode)
               MOVE    CLREREQ-SRYCD (IDX)
                                           TO  LF04-SRYCD (IDX)
      * 診療行為区分コードのテーブル名 (claim:classCodeId) (Claim002)
               MOVE    CLREREQ-SRYCDTBL (IDX)
                                           TO  LF04-SRYCDTBL (IDX)
      * 用法コード (claim:adminCode)
               MOVE    CLREREQ-ADMINCODE (IDX)
                                           TO  LF04-ADMINCODE (IDX)
      * 用法コメント
               MOVE    CLREREQ-ADMMEMO (IDX)
                                           TO  LF04-ADMMEMO (IDX)
      * 回数もしくは日数(Z編集)
               MOVE    CLREREQ-KAISU (IDX)
                                           TO  LF04-KAISU (IDX)
      *
               PERFORM VARYING   IDY   FROM    1   BY  1
                       UNTIL    (IDY           >   40 )
      * 医事請求コード(S + 診療行為コード) (claim:code)
                   MOVE    CLREREQ-IJICD (IDX IDY)
                                           TO  LF04-IJICD (IDX IDY)
      * 医事請求コードのテーブル名 (claim:tableId)
                   MOVE    CLREREQ-TBLID (IDX IDY)
                                           TO  LF04-TBLID (IDX IDY)
      * 医事請求名称
                   MOVE    CLREREQ-ALSNAME (IDX IDY)
                                           TO  LF04-ALSNAME (IDX IDY)
      * 数量(Z編集)
                   MOVE    CLREREQ-SURYO (IDX IDY)
                                           TO  LF04-SURYO (IDX IDY)
      * 数量コード (claim:numberCode)
                   MOVE    CLREREQ-SURYOCD (IDX IDY)
                                           TO  LF04-SURYOCD (IDX IDY)
      * 診療項目メモ
                   MOVE    CLREREQ-SRYMEMO (IDX IDY)
                                           TO  LF04-SRYMEMO (IDX IDY)
      *
                   IF      CLREREQ-IJICD (IDX IDY) NOT =   SPACE
                       ADD     1               TO  CNT-SRYCNT
                   END-IF
               END-PERFORM
      * 診療括りメモ
               MOVE    CLREREQ-MEMO (IDX)
                                           TO  LF04-MEMO (IDX)
           END-PERFORM
      *
           INITIALIZE                  LNK-ORCL0031AREA
           MOVE    "1"                 TO  LNK-KBN-CODE
           CALL    "ORCL0031"          USING LF04-REC
                                             LNK-ORCL0031AREA
      *
      *    中途データエラー
           IF      LNK-ERR1-CODE       =   "02"
      *        「内容を置き換えました」は警告へ
               ADD     1                   TO  IDK
               MOVE    "W03"               TO  WRK-KEICD(IDK)
               MOVE    SPACE               TO  LNK-ERR1-CODE
               MOVE    SPACE               TO  LNK-ERR1-MSG
           END-IF
           IF      LNK-ERR1-CODE   NOT =   SPACE
               IF      CNT-BYOCNT          =   ZERO
                   MOVE    LNK-ERR1-CODE       TO  WRK-ERRCD
                   MOVE    LNK-ERR1-MSG        TO  WRK-ERRMSG
               ELSE
                   MOVE    LNK-ERR1-CODE       TO  WRK-ERRCD-1
                   MOVE    LNK-ERR1-MSG        TO  WRK-ERRMSG-1
               END-IF
           END-IF
           IF      LNK-ERR2-CODE   NOT =   SPACE
               IF      (CNT-SRYCNT         =   ZERO)
                   MOVE    LNK-ERR2-CODE       TO  WRK-ERRCD
                   MOVE    LNK-ERR2-MSG        TO  WRK-ERRMSG
               ELSE
                   MOVE    LNK-ERR2-CODE       TO  WRK-ERRCD-2
                   MOVE    LNK-ERR2-MSG        TO  WRK-ERRMSG-2
               END-IF
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        中途データ・病名
               IF      (CNT-SRYCNT         >   ZERO )
                 AND   (CNT-BYOCNT         >   ZERO )
                   IF      (LNK-OUT-CNT1       >   ZERO)
                       AND (LNK-OUT-CNT2       =   ZERO)
      *                中途データ登録
                       MOVE    "21"            TO  WRK-ERRCD
                       STRING  "中途データ登録処理終了。"
                               "病名の登録ができませんでした。"
                                               INTO  WRK-ERRMSG
                       END-STRING
                   END-IF
                   IF      (LNK-ERR1-CODE  NOT =   SPACE)
                       AND (LNK-OUT-CNT2       >   ZERO)
      *                中途データ登録
                       STRING  "病名登録処理終了。"
                               "中途データが登録できませんでした。"
                                               INTO  WRK-ERRMSG
                       END-STRING
                       MOVE    "20"            TO  WRK-ERRCD
                   END-IF
               END-IF
               IF     (LNK-OUT-HKNCOMBI    =   ZERO
                                           OR  SPACE )
               AND    (LNK-OUT-CNT1        >   ZERO )
                   ADD     1                   TO  IDK
                   MOVE    "W02"               TO  WRK-KEICD(IDK)
               END-IF
           END-IF
      *
           .
       20021-CLAIM-CALL-EXT.
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
               MOVE    "医療機関番号が一致しません。"
                                               TO  WRK-ERRMSG
           WHEN    "10"
               MOVE    "患者番号に該当する患者が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "診療日が暦日ではありません"
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
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       891-WARNING-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-WERRCD
      *警告
           WHEN    "W01"
               MOVE    "他端末で処理中の患者です。"
                                               TO  WRK-ERRMSG
           WHEN    "W02"
               MOVE    "保険組合せをゼロで登録しました。"
                                               TO  WRK-ERRMSG
           WHEN    "W03"
               STRING  "診療内容を置き換えました。"
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
