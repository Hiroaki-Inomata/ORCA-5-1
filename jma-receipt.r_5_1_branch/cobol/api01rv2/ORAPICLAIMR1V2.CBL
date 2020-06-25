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
       PROGRAM-ID.         ORAPICLAIMR1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : ＣＬＡＩＭ受付送信(xml2)
      *  管理者            :
      *  作成日付    作業者        記述
      *  16/05/XX    NACL−多々納　新規作成
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
           03  FLG-TEND                PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
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
      *    エラーコード
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-KEICD               PIC X(02).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYM           PIC X(06).
               05  WRK-SYSDD           PIC X(02).
      *
           03  WRK-PTID                PIC 9(10).
           03  WRK-PTNUM               PIC X(20).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-NAME          PIC X(80).
           03  WRK-DRCD                PIC X(05).
           03  WRK-DRCD-NAME           PIC X(80).
      *
       01  WRK-XML-AREA.
           03  WRK-ACCEPT-DATE         PIC X(10).
      *
           03  WRK-ACCEPT-YMD          PIC X(08).
           03  WRK-ACCEPT-TIME         PIC X(06).
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(100).
           03  WRK-ATO-INPUT           PIC X(100).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *
      *
      *    CLAIMパラメタ
       01  LF01-REC. 
           COPY    "CPRLF001.INC".
      *
       01  WRK-CLAIM-PARA.
           03  WRK-PARA.
               05  WRK-PARA-PTID       PIC  9(10).
               05  WRK-PARA-SRYKA      PIC  X(02).
               05  WRK-PARA-DRCD       PIC  X(05).
               05  WRK-PARA-HKNCOMBI   PIC  X(04).
               05  WRK-PARA-SYSYMD     PIC  X(08).
               05  WRK-PARA-TIME       PIC  X(06).
               05  WRK-PARA-SINRYONAIYO
                                       PIC  X(16).
               05  WRK-PARA-RENNUM     PIC  X(02).
               05  WRK-PARA-MEMO       PIC  X(02).
               05  WRK-PARA-RC         PIC  X(02).
               05  WRK-PARA-ENDRC      PIC  X(02).
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "CPORCSKANACHK.INC".
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
      *    COPY    "CPORCSNUM.INC".
      *
      *
      *    API XML読み込み用定義体
           COPY    "CPCLAIMFRONTV2REQ.INC".
      *    API XML書き込み用定義体
           COPY    "CPCLAIMFRONTV2RES.INC".
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
      *    （職員情報）
           COPY    "CPSK1010.INC".
      *    （診療科情報）
           COPY    "CPSK1005.INC".
      *    （医療機関情報）
           COPY    "CPSK1001.INC".
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
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
           DISPLAY   "********************"
           DISPLAY   "* claim_front start*"
           DISPLAY   "********************"
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
      *
           IF      WRK-ERRCD           =   "99"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST23-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-APTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "*******************"
           DISPLAY   "* claim_front end *"
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
           INITIALIZE                      XML-CLAIM-FRONTV2REQ
           INITIALIZE                      XML-CLAIM-FRONTV2RES
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *****MOVE   "Medical Info"       TO  CLFRRES-RESKEY
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  CLFRRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  CLFRRES-INFORMATION-TIME
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
           PERFORM 1000-APTXMLREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD
      *
           MOVE    ZERO                TO  FLG-SYORI
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
      *    送信内容編集
           PERFORM 2003-CLAIMHEN-SEC
      *
           IF      WRK-ERRCD           =   SPACE
               EVALUATE    CLFRREQ-REQUEST-NUMBER
                   WHEN    "01"
      *                ＣＬＡＩＭデータ情報編集
                       PERFORM 2002-SYORI-01-SEC
                   WHEN    "02"
      *                送信済み更新
                       PERFORM 2004-SYORI-02-SEC
                   WHEN    OTHER
                       MOVE    "15"              TO  WRK-ERRCD
               END-EVALUATE
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
           IF      APILST23-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_claim_frontv2req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST23-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "claim_frontv2req"  TO  APIREQ-RECORDNAME
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
           MOVE    "xml_claim_frontv2req"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST23-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "claim_frontv2req"  TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ          TO  XML-CLAIM-FRONTV2REQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           MOVE    "XMLCLOSE"          TO  MCP-FUNC
           MOVE    "xml_claim_frontv2req"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF (MCP-RC = ZERO OR 1)
               CONTINUE
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
      *    エラーメッセージ編集
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-KEICD           TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD           =   SPACE
               MOVE    ZERO                TO  CLFRRES-API-RESULT
               MOVE    "処理終了"          TO  CLFRRES-API-RESULT-MSG
           ELSE
               IF      WRK-ERRMSG          =   SPACE
      *        エラー・警告メッセージ
                   PERFORM 890-ERRCD-MSG-SEC
               END-IF
               MOVE    WRK-ERRCD           TO  CLFRRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  CLFRRES-API-RESULT-MSG
           END-IF
      *
           PERFORM 3001-TMDCXMLMAKE-SEC
      *
          .
       300-APTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       3001-TMDCXMLMAKE-SEC        SECTION.
      *
           IF      APILST23-BODY   NOT =   LOW-VALUE
               DISPLAY "CLFRRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_claim_frontv2res"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  CLFRRES-MODE
           MOVE    "claim_frontv2res"  TO  CLFRRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-CLAIM-FRONTV2RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_claim_frontv2res"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  CLFRRES-MODE
           MOVE    "claim_frontv2res"  TO  CLFRRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-CLAIM-FRONTV2RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    CLFRRES-OBJECT      TO  APILST23-BODY 
               MOVE    "application/xml"   TO  APILST23-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       3001-TMDCXMLMAKE-EXT.
           EXIT.
      *
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
      *    受付日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-ACCEPT-DATE-SEC
           END-IF
      *    受付時間チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-ACCEPT-TIME-SEC
           END-IF
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PTNUM-CHK-SEC
           END-IF
      *    診療科コードチェック
           IF     (WRK-SRYKA           =   SPACE
                                       OR  "XX"  )
               CONTINUE
           ELSE
               IF     (WRK-ERRCD           =   SPACE  )
                   PERFORM 20011-SRYKA-CHK-SEC
               END-IF
           END-IF
      *    ドクターコードチェック
           IF     (WRK-DRCD            =   SPACE
                                       OR  "@@@@@"  )
               CONTINUE
           ELSE
               IF     (WRK-ERRCD           =   SPACE  )
                   PERFORM 20011-DRCD-CHK-SEC
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
      *    受付診療日付
           MOVE    CLFRREQ-ACCEPT-DATE     TO  WRK-HEN-DATE
           MOVE    SPACE                   TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-ACCEPT-YMD
      *    未設定時はシステム情報設定
           IF      WRK-ACCEPT-YMD          =   SPACE
               MOVE    SMCNDATE-YMD        TO  WRK-ACCEPT-YMD
               MOVE    "K1"                TO  WRK-KEICD
           END-IF
      *    システム日付とする
           MOVE    WRK-ACCEPT-YMD      TO  SPA-SYSYMD
      *
      *    患者番号
           MOVE    CLFRREQ-PATIENTID   TO  WRK-PTNUM
      *    診療科
           MOVE    CLFRREQ-DEP-CODE    TO  WRK-SRYKA
      *    ドクター
           MOVE    CLFRREQ-DR-CODE     TO  WRK-DRCD
           .
       20010-INPUT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付日チェック
      *****************************************************************
       20011-ACCEPT-DATE-SEC    SECTION.
      *
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-ACCEPT-YMD      TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "11"                TO  WRK-ERRCD
           END-IF
           MOVE    WRK-ACCEPT-YMD      TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  WRK-ACCEPT-DATE
      *
           .
       20011-ACCEPT-DATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付時間チェック
      *****************************************************************
       20011-ACCEPT-TIME-SEC    SECTION.
      *
           MOVE    SPACE                   TO  WRK-HEN-DATE
           MOVE    CLFRREQ-ACCEPT-TIME     TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           IF      WRK-THMS            NUMERIC
               MOVE    WRK-THMS            TO  WRK-ACCEPT-TIME
           ELSE
               MOVE    "12"                TO  WRK-ERRCD
           END-IF
           .
       20011-ACCEPT-TIME-EXT.
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
      *    ドクターコードチェック
      *****************************************************************
       20011-DRCD-CHK-SEC         SECTION.
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WRK-DRCD            TO  SYS-1010-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD
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
           .
       20011-DRCD-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    送信内容編集処理
      *****************************************************************
       2003-CLAIMHEN-SEC        SECTION.
      *
           MOVE    WRK-PTNUM           TO  CLFRRES-PATIENTID
      *    実施日(CCYY-MM-DDTHH:MM:SS形式)
           STRING  CLFRRES-INFORMATION-DATE
                   "T"
                   CLFRRES-INFORMATION-TIME
                                        DELIMITED  BY  SIZE
                                   INTO    CLFRRES-INFORMATION-HEN
           END-STRING
      *
           .
       2003-CLAIMHEN-EXT.
           EXIT.
      *****************************************************************
      *    データ情報編集処理
      *****************************************************************
       2002-SYORI-01-SEC           SECTION.
      *
           INITIALIZE                  WRK-CLAIM-PARA
           MOVE    WRK-ACCEPT-TIME     TO  WRK-PARA-TIME
      *    診療内容名称
           MOVE    CLFRREQ-MEDICAL-NAME
                                       TO  WRK-PARA-SINRYONAIYO
      *    メモ送信区分
           MOVE    CLFRREQ-MEMO-CODE   TO  WRK-PARA-MEMO
      *    送信連番
           MOVE    "01"                TO  WRK-PARA-RENNUM
      *
           MOVE    WRK-SRYKA           TO  SPA-SRYKA
           MOVE    WRK-PTNUM           TO  SPA-PTNUM
           MOVE    WRK-DRCD            TO  SPA-DRCD
           MOVE    CLFRREQ-COMBINATION-NUMBER
                                       TO  SPA-HKNCOMBINUM
      *
           CALL    "ORCL0011"          USING   WRK-PARA-TIME
                                               WRK-PARA-SINRYONAIYO
                                               WRK-PARA-RENNUM
                                               WRK-PARA-MEMO
                                               SPA-AREA
                                               LF01-REC
      *
      *    患者情報内容編集
           PERFORM 20021-CLFRRES-HEN-SEC
      *
           .
       2002-SYORI-01-EXT.
           EXIT.
      *****************************************************************
      *    取得情報編集処理
      *****************************************************************
       20021-CLFRRES-HEN-SEC     SECTION.
      *
      * 患者主ID
           MOVE    LF01-PTID1          TO  CLFRRES-PTID1
      * IDの種類コード
           MOVE    LF01-IDTYPE1        TO  CLFRRES-IDTYPE1
      * 作成者 個人ID [必須]
           MOVE    LF01-CRE001-PERSONALID
                                       TO  CLFRRES-CRE001-PERSONALID
      * 作成者 個人フルネーム [条件付き必須]
           MOVE    LF01-CRE001-FULLNAME
                                       TO  CLFRRES-CRE001-FULLNAME
      * 作成者 個人姓 [条件付き必須]
           MOVE    LF01-CRE001-FAMILYNAME
                                       TO  CLFRRES-CRE001-FAMILYNAME
      * 作成者 個人名 [条件付き必須]
           MOVE    LF01-CRE001-GIVENNAME
                                       TO  CLFRRES-CRE001-GIVENNAME
      * 作成者 ミドルネーム
           MOVE    LF01-CRE001-FAMILYNAME
                                       TO  CLFRRES-CRE001-FAMILYNAME
      * 作成者 肩書きなど
           MOVE    LF01-CRE001-PREFIX
                                       TO  CLFRRES-CRE001-PREFIX
      * 作成者 学位
           MOVE    LF01-CRE001-DEGREE
                                       TO  CLFRRES-CRE001-DEGREE
      * 作成者 施設情報名
           MOVE    LF01-CRE001-FACILITYNAME
                                       TO  CLFRRES-CRE001-FACILITYNAME
      * 作成者 施設ID
           MOVE    LF01-CRE001-FACILITYID
                                       TO  CLFRRES-CRE001-FACILITYID
      * 作成者 診療科名称
           MOVE    LF01-CRE001-DPERTNAME
                                       TO  CLFRRES-CRE001-DPERTNAME
      * 作成者 診療科ID
           MOVE    LF01-CRE001-DPERTID
                                       TO  CLFRRES-CRE001-DPERTID
      * 作成者 住所 [条件付き必須]
           MOVE    LF01-CRE001-ADRFULLNAME
                                       TO  CLFRRES-CRE001-ADRFULLNAME
      * 作成者 住所の種類
           MOVE    LF01-CRE001-ADRCLASS
                                       TO  CLFRRES-CRE001-ADRCLASS
      * 作成者 都道府県 [条件付き必須]
           MOVE    LF01-CRE001-ADRPREF
                                       TO  CLFRRES-CRE001-ADRPREF
      * 作成者 市、区、郡 [条件付き必須]
           MOVE    LF01-CRE001-ADRCITY
                                       TO  CLFRRES-CRE001-ADRCITY
      * 作成者 町、村 [条件付き必須]
           MOVE    LF01-CRE001-ADRTOWN
                                       TO  CLFRRES-CRE001-ADRTOWN
      * 作成者 番地、丁目等 [条件付き必須]
           MOVE    LF01-CRE001-ADRHOMENUM
                                       TO  CLFRRES-CRE001-ADRHOMENUM
      * 作成者 郵便番号
           MOVE    LF01-CRE001-ZIP
                                       TO  CLFRRES-CRE001-ZIP
      * 作成者 国コード
           MOVE    LF01-CRE001-COUNTRY
                                       TO  CLFRRES-CRE001-COUNTRY
      * 作成者 メールアドレス
           MOVE    LF01-CRE001-EMAIL
                                       TO  CLFRRES-CRE001-EMAIL
      * 作成者 市外局番 [必須]
           MOVE    LF01-CRE001-PHONEAREA
                                       TO  CLFRRES-CRE001-PHONEAREA
      * 作成者 局番 [必須]
           MOVE    LF01-CRE001-PHONECITY
                                       TO  CLFRRES-CRE001-PHONECITY
      * 作成者 加入番号 [必須]
           MOVE    LF01-CRE001-PHONENUM
                                       TO  CLFRRES-CRE001-PHONENUM
      * 作成者 内線番号
           MOVE    LF01-CRE001-PHONEEXT
                                       TO  CLFRRES-CRE001-PHONEEXT
      * 作成者 国番号
           MOVE    LF01-CRE001-PHONECOUNTRY
                                       TO  CLFRRES-CRE001-PHONECOUNTRY
      * 作成者 使用時間帯などを記載
           MOVE    LF01-CRE001-PHONEMEMO
                                       TO  CLFRRES-CRE001-PHONEMEMO
      * 作成者 装置の種類コード
           MOVE    LF01-CRE001-PHONEEQUIP
                                       TO  CLFRRES-CRE001-PHONEEQUIP
      * 作成者 生成者の資格 [必須]
           MOVE    LF01-CRE001-LICENSE
                                       TO  CLFRRES-CRE001-LICENSE
      *##
      * コンスタント値 (<extRefs />)
           MOVE    "<extRefs />"       TO  CLFRRES-M-RX001-I1
      * コンスタント値  (<accessRight permit="all" />) 
           MOVE    '<accessRight permit="all" />'
                                       TO  CLFRRES-M-RI101-ACC1
      * コンスタント値 (Data of Regist) 
           MOVE    "Data of Regist"
                                       TO  CLFRRES-M-RI101-TITLE
      *##
      *
      * [患者情報]
      * 文書ユニークID
           MOVE    LF01-UID1           TO  CLFRRES-UID1
      * カルテ電子保存確定日
           MOVE    LF01-CONFIRMDATE1
                                       TO  CLFRRES-CONFIRMDATE1
      * 作成者 個人ID [必須]
           MOVE    LF01-CRE101-PERSONALID
                                       TO  CLFRRES-CRE101-PERSONALID
      * 作成者 個人フルネーム [条件付き必須]
           MOVE    LF01-CRE101-FULLNAME
                                       TO  CLFRRES-CRE101-FULLNAME
      * 作成者 個人姓 [条件付き必須]
           MOVE    LF01-CRE101-FAMILYNAME
                                       TO  CLFRRES-CRE101-FAMILYNAME
      * 作成者 個人名 [条件付き必須]
           MOVE    LF01-CRE101-GIVENNAME
                                       TO  CLFRRES-CRE101-GIVENNAME
      * 作成者 ミドルネーム
           MOVE    LF01-CRE101-MIDDLENAME
                                       TO  CLFRRES-CRE101-MIDDLENAME
      * 作成者 肩書きなど
           MOVE    LF01-CRE101-PREFIX
                                       TO  CLFRRES-CRE101-PREFIX
      * 作成者 学位
           MOVE    LF01-CRE101-DEGREE
                                       TO  CLFRRES-CRE101-DEGREE
      * 作成者 施設情報名
           MOVE    LF01-CRE101-FACILITYNAME
                                       TO  CLFRRES-CRE101-FACILITYNAME
      * 作成者 施設ID
           MOVE    LF01-CRE101-FACILITYID
                                       TO  CLFRRES-CRE101-FACILITYID
      * 作成者 診療科名称
           MOVE    LF01-CRE101-DPERTNAME
                                       TO  CLFRRES-CRE101-DPERTNAME
      * 作成者 診療科ID
           MOVE    LF01-CRE101-DPERTID
                                       TO  CLFRRES-CRE101-DPERTID
      * 作成者 住所 [条件付き必須]
           MOVE    LF01-CRE101-ADRFULLNAME
                                       TO  CLFRRES-CRE101-ADRFULLNAME
      * 作成者 住所の種類
           MOVE    LF01-CRE101-ADRCLASS
                                       TO  CLFRRES-CRE101-ADRCLASS
      * 作成者 都道府県 [条件付き必須]
           MOVE    LF01-CRE101-ADRPREF
                                       TO  CLFRRES-CRE101-ADRPREF
      * 作成者 市、区、郡 [条件付き必須]
           MOVE    LF01-CRE101-ADRCITY
                                       TO  CLFRRES-CRE101-ADRCITY
      * 作成者 町、村 [条件付き必須]
           MOVE    LF01-CRE101-ADRTOWN
                                       TO  CLFRRES-CRE101-ADRTOWN
      * 作成者 番地、丁目等 [条件付き必須]
           MOVE    LF01-CRE101-ADRHOMENUM
                                       TO  CLFRRES-CRE101-ADRHOMENUM
      * 作成者 郵便番号
           MOVE    LF01-CRE101-ZIP
                                       TO  CLFRRES-CRE101-ZIP
      * 作成者 国コード
           MOVE    LF01-CRE101-COUNTRY
                                       TO  CLFRRES-CRE101-COUNTRY
      * 作成者 メールアドレス
           MOVE    LF01-CRE101-EMAIL
                                       TO  CLFRRES-CRE101-EMAIL
      * 作成者 市外局番 [必須]
           MOVE    LF01-CRE101-PHONEAREA
                                       TO  CLFRRES-CRE101-PHONEAREA
      * 作成者 局番 [必須]
           MOVE    LF01-CRE101-PHONECITY
                                       TO  CLFRRES-CRE101-PHONECITY
      * 作成者 加入番号 [必須]
           MOVE    LF01-CRE101-PHONENUM
                                       TO  CLFRRES-CRE101-PHONENUM
      * 作成者 内線番号
           MOVE    LF01-CRE101-PHONEEXT
                                       TO  CLFRRES-CRE101-PHONEEXT
      * 作成者 国番号
           MOVE    LF01-CRE101-PHONECOUNTRY
                                       TO  CLFRRES-CRE101-PHONECOUNTRY
      * 作成者 使用時間帯などを記載
           MOVE    LF01-CRE101-PHONEMEMO
                                       TO  CLFRRES-CRE101-PHONEMEMO
      * 作成者 装置の種類コード
           MOVE    LF01-CRE101-PHONEEQUIP
                                       TO  CLFRRES-CRE101-PHONEEQUIP
      * 作成者 生成者の資格
           MOVE    LF01-CRE101-LICENSE
                                       TO  CLFRRES-CRE101-LICENSE
      *##
      * コンスタント値 (<extRefs />) 
           MOVE    "<extRefs />"       TO  CLFRRES-M-RX101-I1
      *##
      * 患者主ID
           MOVE    LF01-PTID2          TO  CLFRRES-PTID2
      * 患者主IDのテーブル名
           MOVE    LF01-IDTYPE2        TO  CLFRRES-IDTYPE2
      * 氏名(カナ)の表記法
           MOVE    LF01-KANANAME-REPCODE
                                       TO  CLFRRES-KANANAME-REPCODE
      * 氏名(カナ)のフルネーム
           MOVE    LF01-KANANAME
                                       TO  CLFRRES-KANANAME
      * 氏名(漢字)の表記法
           MOVE    LF01-NAME-REPCODE
                                       TO  CLFRRES-NAME-REPCODE
      * 氏名(漢字)のフルネーム
           MOVE    LF01-NAME           TO  CLFRRES-NAME
      * 生年月日
           MOVE    LF01-BIRTHDAY
                                       TO  CLFRRES-BIRTHDAY
      * 性別
           MOVE    LF01-SEX            TO  CLFRRES-SEX
      * 住所(自宅)
           MOVE    LF01-HOME-ADRS
                                       TO  CLFRRES-HOME-ADRS
      * 住所(自宅) 種類コード
           MOVE    LF01-HOME-KREPCODE
                                       TO  CLFRRES-HOME-KREPCODE
      * 住所(自宅) 郵便番号
           MOVE    LF01-HOME-POST
                                       TO  CLFRRES-HOME-POST
      * 住所(連絡先)
           MOVE    LF01-RENRAKU-ADRS
                                       TO  CLFRRES-RENRAKU-ADRS
      * 住所(連絡先) 種類コード
           MOVE    LF01-RENRAKU-KREPCODE
                                       TO  CLFRRES-RENRAKU-KREPCODE
      * 住所(連絡先) 郵便番号
           MOVE    LF01-RENRAKU-POST
                                       TO  CLFRRES-RENRAKU-POST
      * 住所(勤務先)
           MOVE    LF01-OFFICE-ADRS
                                       TO  CLFRRES-OFFICE-ADRS
      * 住所(勤務先) 種類コード
           MOVE    LF01-OFFICE-KREPCODE
                                       TO  CLFRRES-OFFICE-KREPCODE
      * 住所(勤務先) 郵便番号
           MOVE    LF01-OFFICE-POST
                                       TO  CLFRRES-OFFICE-POST
      * 住所(帰省先)
           MOVE    LF01-KISEI-ADRS
                                       TO  CLFRRES-KISEI-ADRS
      * 住所(帰省先) 種類コード
           MOVE    LF01-KISEI-KREPCODE
                                       TO  CLFRRES-KISEI-KREPCODE
      * 住所(帰省先) 郵便番号
           MOVE    LF01-KISEI-POST
                                       TO  CLFRRES-KISEI-POST
      * 市外局番
           MOVE    LF01-PHONE-AREA
                                       TO  CLFRRES-PHONE-AREA
      * 局番
           MOVE    LF01-PHONE-CITY
                                       TO  CLFRRES-PHONE-CITY
      * 加入番号
           MOVE    LF01-PHONE-NUM
                                       TO  CLFRRES-PHONE-NUM
      * 内線番号
           MOVE    LF01-PHONE-EXT
                                       TO  CLFRRES-PHONE-EXT
      * 国番号
           MOVE    LF01-PHONE-COUNTRY
                                       TO  CLFRRES-PHONE-COUNTRY
      * 使用時間帯などを記載
           MOVE    LF01-PHONE-MEMO
                                       TO  CLFRRES-PHONE-MEMO
      * 装置の種類コード [省略可能・属性] (PH)
           MOVE    LF01-PHONE-EQUIP
                                       TO  CLFRRES-PHONE-EQUIP
      * 死亡フラグ
           MOVE    LF01-DEATHKBN
                                       TO  CLFRRES-DEATHKBN
      * [健康保険情報]
           PERFORM VARYING     IDX     FROM    1   BY   1
                   UNTIL      (IDX     >   20 )
                    OR        (LF01-UID2(IDX)  =   SPACE )
      *##
      * コンスタント値 (<accessRight permit="all" />)
               MOVE    '<accessRight permit="all" />'
                                       TO  CLFRRES-M-RI2II-ACC1(IDX)
      * コンスタント値 (Data of Regist)
               MOVE    "Data of Regist"
                                       TO  CLFRRES-M-RI2II-TITLE(IDX)
      *##
      * 文書ユニークID
               MOVE    LF01-UID2 (IDX)
                                       TO  CLFRRES-UID2  (IDX)
      * カルテ電子保存の確定日
               MOVE    LF01-CONFIRMDATE2 (IDX)
                                       TO  CLFRRES-CONFIRMDATE2 (IDX)
      * 作成者 個人ID [必須]
               MOVE    LF01-CRE201-PERSONALID (IDX)
                                       TO  CLFRRES-CRE201-PERSONALID
                                                               (IDX)
      * 作成者 個人フルネーム [条件付き必須]
               MOVE    LF01-CRE201-FULLNAME (IDX)
                                       TO  CLFRRES-CRE201-FULLNAME
                                                               (IDX)
      * 作成者 個人姓 [条件付き必須]
               MOVE    LF01-CRE201-FAMILYNAME (IDX)
                                       TO  CLFRRES-CRE201-FAMILYNAME
                                                               (IDX)
      * 作成者 個人名 [条件付き必須]
               MOVE    LF01-CRE201-GIVENNAME (IDX)
                                       TO  CLFRRES-CRE201-GIVENNAME
                                                               (IDX)
      * 作成者 ミドルネーム
               MOVE    LF01-CRE201-MIDDLENAME (IDX)
                                       TO  CLFRRES-CRE201-MIDDLENAME
                                                               (IDX)
      * 作成者 肩書きなど
               MOVE    LF01-CRE201-PREFIX (IDX)
                                       TO  CLFRRES-CRE201-PREFIX
                                                               (IDX)
      * 作成者 学位
               MOVE    LF01-CRE201-DEGREE (IDX)
                                       TO  CLFRRES-CRE201-DEGREE
                                                               (IDX)
      * 作成者 施設情報名
               MOVE    LF01-CRE201-FACILITYNAME (IDX)
                                       TO  CLFRRES-CRE201-FACILITYNAME
                                                               (IDX)
      * 作成者 施設ID
               MOVE    LF01-CRE201-FACILITYID (IDX)
                                       TO  CLFRRES-CRE201-FACILITYID
                                                               (IDX)
      * 作成者 診療科名称
               MOVE    LF01-CRE201-DPERTNAME (IDX)
                                       TO  CLFRRES-CRE201-DPERTNAME
                                                               (IDX)
      * 作成者 診療科ID
               MOVE    LF01-CRE201-DPERTID (IDX)
                                       TO  CLFRRES-CRE201-DPERTID
                                                               (IDX)
      * 作成者 住所 [条件付き必須]
               MOVE    LF01-CRE201-ADRFULLNAME (IDX)
                                       TO  CLFRRES-CRE201-ADRFULLNAME
                                                               (IDX)
      * 作成者 住所の種類
               MOVE    LF01-CRE201-ADRCLASS (IDX)
                                       TO  CLFRRES-CRE201-ADRCLASS
                                                               (IDX)
      * 作成者 都道府県 [条件付き必須]
               MOVE    LF01-CRE201-ADRPREF (IDX)
                                       TO  CLFRRES-CRE201-ADRPREF
                                                               (IDX)
      * 作成者 市、区、郡 [条件付き必須]
               MOVE    LF01-CRE201-ADRCITY (IDX)
                                       TO  CLFRRES-CRE201-ADRCITY
                                                               (IDX)
      * 作成者 町、村 [条件付き必須]
               MOVE    LF01-CRE201-ADRTOWN (IDX)
                                       TO  CLFRRES-CRE201-ADRTOWN
                                                               (IDX)
      * 作成者 番地、丁目等 [条件付き必須]
               MOVE    LF01-CRE201-ADRHOMENUM (IDX)
                                       TO  CLFRRES-CRE201-ADRHOMENUM
                                                               (IDX)
      * 作成者 郵便番号
               MOVE    LF01-CRE201-ZIP (IDX)
                                       TO  CLFRRES-CRE201-ZIP
                                                               (IDX)
      * 作成者 国コード
               MOVE    LF01-CRE201-COUNTRY (IDX)
                                       TO  CLFRRES-CRE201-COUNTRY
                                                               (IDX)
      * 作成者 メールアドレス
               MOVE    LF01-CRE201-EMAIL (IDX)
                                       TO  CLFRRES-CRE201-EMAIL
                                                               (IDX)
      * 作成者 市外局番 [必須]
               MOVE    LF01-CRE201-PHONEAREA (IDX)
                                       TO  CLFRRES-CRE201-PHONEAREA
                                                               (IDX)
      * 作成者 局番 [必須]
               MOVE    LF01-CRE201-PHONECITY (IDX)
                                       TO  CLFRRES-CRE201-PHONECITY
                                                               (IDX)
      * 作成者 加入番号 [必須]
               MOVE    LF01-CRE201-PHONENUM (IDX)
                                       TO  CLFRRES-CRE201-PHONENUM
                                                               (IDX)
      * 作成者 内線番号
               MOVE    LF01-CRE201-PHONEEXT (IDX)
                                       TO  CLFRRES-CRE201-PHONEEXT
                                                               (IDX)
      * 作成者 国番号
               MOVE    LF01-CRE201-PHONECOUNTRY (IDX)
                                       TO  CLFRRES-CRE201-PHONECOUNTRY
                                                               (IDX)
      * 作成者 使用時間帯などを記載
               MOVE    LF01-CRE201-PHONEMEMO (IDX)
                                       TO  CLFRRES-CRE201-PHONEMEMO
                                                               (IDX)
      * 作成者 装置の種類コード
               MOVE    LF01-CRE201-PHONEEQUIP (IDX)
                                       TO  CLFRRES-CRE201-PHONEEQUIP
                                                               (IDX)
      * 作成者 生成者の資格 [必須]
               MOVE    LF01-CRE201-LICENSE (IDX)
                                       TO  CLFRRES-CRE201-LICENSE
                                                               (IDX)
      *##
      * コンスタント値  (<extRefs />)
               MOVE    "<extRefs />"
                                       TO  CLFRRES-M-RX2II-I1
                                                               (IDX)
      *##
      * 健康保険種別名称
               MOVE    LF01-HKNSBT (IDX)
                                       TO  CLFRRES-HKNSBT    (IDX)
      * 健康保険種別コード
               MOVE    LF01-HKNSBTCODE (IDX)
                                       TO  CLFRRES-HKNSBTCODE (IDX)
      * 健康保険種別コードテーブル名
               MOVE    LF01-HKNSBTTBL (IDX)
                                       TO  CLFRRES-HKNSBTTBL (IDX)
      *保険者番号
               MOVE    LF01-HKNJANUM (IDX)
                                       TO  CLFRRES-HKNJANUM  (IDX)
      *記号
               MOVE    LF01-KIGO (IDX)
                                       TO  CLFRRES-KIGO    (IDX)
      *番号
               MOVE    LF01-NUM (IDX)
                                       TO  CLFRRES-NUM    (IDX)
      *本人家族区分
               MOVE    LF01-HONKZKKBN (IDX)
                                       TO  CLFRRES-HONKZKKBN (IDX)
      *適用開始年月日
               MOVE    LF01-TEKSTYMD (IDX)
                                       TO  CLFRRES-TEKSTYMD (IDX)
      *適用終了年月日
               MOVE    LF01-TEKEDYMD (IDX)
                                       TO  CLFRRES-TEKEDYMD (IDX)
      * 入院時の負担率
               MOVE    LF01-INRATIO (IDX)
                                       TO  CLFRRES-INRATIO  (IDX)
      * 外来時の負担率
               MOVE    LF01-OUTRATIO (IDX)
                                       TO  CLFRRES-OUTRATIO (IDX)
      *公費情報
               PERFORM VARYING     IDY     FROM    1   BY   1
                       UNTIL      (IDY     >   6 )
      *優先順位
                   MOVE    LF01-PRIORITY (IDX IDY)
                                       TO  CLFRRES-PRIORITY
                                                        (IDX IDY)
      *公費名称
                   MOVE    LF01-KOHINAME (IDX IDY)
                                       TO  CLFRRES-KOHINAME
                                                        (IDX IDY)
      *負担者番号
                   MOVE    LF01-FTNJANUM (IDX IDY)
                                       TO  CLFRRES-FTNJANUM
                                                        (IDX IDY)
      *受給者番号
                   MOVE    LF01-JKYSNUM (IDX IDY)
                                       TO  CLFRRES-JKYSNUM
                                                        (IDX IDY)
      *適用開始年月日
                   MOVE    LF01-TTEKSTYMD (IDX IDY)
                                       TO  CLFRRES-TTEKSTYMD
                                                        (IDX IDY)
      *適用終了年月日
                   MOVE    LF01-TTEKEDYMD (IDX IDY)
                                       TO  CLFRRES-TTEKEDYMD
                                                        (IDX IDY)
      * 負担金または負担率
                   MOVE    LF01-RATIO-CODE (IDX IDY)
                                       TO  CLFRRES-RATIO-CODE
                                                        (IDX IDY)
                   MOVE    LF01-RATIO (IDX IDY)
                                       TO  CLFRRES-RATIO
                                                        (IDX IDY)
               END-PERFORM
           END-PERFORM
      *
      * コンスタント値 (<accessRight permit="all" />)
           MOVE    '<accessRight permit="all" />'
                                       TO  CLFRRES-M-RI301-ACC1
      * コンスタント値 (Data of Regist) 
           MOVE    "Data of Regist"
                                       TO  CLFRRES-M-RI301-TITLE
      *##
      *[CLAIM]
      * 文書ユニークID
           MOVE    LF01-UID3
                                       TO  CLFRRES-UID3
      * カルテ電子保存の確定日
           MOVE    LF01-CONFIRMDATE3
                                       TO  CLFRRES-CONFIRMDATE3
      * 作成者 個人ID [必須]
           MOVE    LF01-CRE301-PERSONALID
                                       TO  CLFRRES-CRE301-PERSONALID
      * 作成者 個人フルネーム [条件付き必須]
           MOVE    LF01-CRE301-FULLNAME
                                       TO  CLFRRES-CRE301-FULLNAME
      * 作成者 個人姓 [条件付き必須]
           MOVE    LF01-CRE301-FAMILYNAME
                                       TO  CLFRRES-CRE301-FAMILYNAME
      * 作成者 個人名 [条件付き必須]
           MOVE    LF01-CRE301-GIVENNAME
                                       TO  CLFRRES-CRE301-GIVENNAME
      * 作成者 ミドルネーム
           MOVE    LF01-CRE301-MIDDLENAME
                                       TO  CLFRRES-CRE301-MIDDLENAME
      * 作成者 肩書きなど
           MOVE    LF01-CRE301-PREFIX
                                       TO  CLFRRES-CRE301-PREFIX
      * 作成者 学位
           MOVE    LF01-CRE301-DEGREE
                                       TO  CLFRRES-CRE301-DEGREE
      * 作成者 施設情報名
           MOVE    LF01-CRE301-FACILITYNAME
                                       TO  CLFRRES-CRE301-FACILITYNAME
      * 作成者 施設ID
           MOVE    LF01-CRE301-FACILITYID
                                       TO  CLFRRES-CRE301-FACILITYID
      * 作成者 診療科名称
           MOVE    LF01-CRE301-DPERTNAME
                                       TO  CLFRRES-CRE301-DPERTNAME
      * 作成者 診療科ID
           MOVE    LF01-CRE301-DPERTID
                                       TO  CLFRRES-CRE301-DPERTID
      * 作成者 住所 [条件付き必須]
           MOVE    LF01-CRE301-ADRFULLNAME
                                       TO  CLFRRES-CRE301-ADRFULLNAME
      * 作成者 住所の種類
           MOVE    LF01-CRE301-ADRCLASS
                                       TO  CLFRRES-CRE301-ADRCLASS
      * 作成者 都道府県 [条件付き必須]
           MOVE    LF01-CRE301-ADRPREF
                                       TO  CLFRRES-CRE301-ADRPREF
      * 作成者 市、区、郡 [条件付き必須]
           MOVE    LF01-CRE301-ADRCITY
                                       TO  CLFRRES-CRE301-ADRCITY
      * 作成者 町、村 [条件付き必須]
           MOVE    LF01-CRE301-ADRTOWN
                                       TO  CLFRRES-CRE301-ADRTOWN
      * 作成者 番地、丁目等 [条件付き必須]
           MOVE    LF01-CRE301-ADRHOMENUM
                                       TO  CLFRRES-CRE301-ADRHOMENUM
      * 作成者 郵便番号
           MOVE    LF01-CRE301-ZIP
                                       TO  CLFRRES-CRE301-ZIP
      * 作成者 国コード
           MOVE    LF01-CRE301-COUNTRY
                                       TO  CLFRRES-CRE301-COUNTRY
      * 作成者 メールアドレス
           MOVE    LF01-CRE301-EMAIL
                                       TO  CLFRRES-CRE301-EMAIL
      * 作成者 市外局番 [必須]
           MOVE    LF01-CRE301-PHONEAREA
                                       TO  CLFRRES-CRE301-PHONEAREA
      * 作成者 局番 [必須]
           MOVE    LF01-CRE301-PHONECITY
                                       TO  CLFRRES-CRE301-PHONECITY
      * 作成者 加入番号 [必須]
           MOVE    LF01-CRE301-PHONENUM
                                       TO  CLFRRES-CRE301-PHONENUM
      * 作成者 内線番号
           MOVE    LF01-CRE301-PHONEEXT
                                       TO  CLFRRES-CRE301-PHONEEXT
      * 作成者 国番号
           MOVE    LF01-CRE301-PHONECOUNTRY
                                       TO  CLFRRES-CRE301-PHONECOUNTRY
      * 作成者 使用時間帯などを記載
           MOVE    LF01-CRE301-PHONEMEMO
                                       TO  CLFRRES-CRE301-PHONEMEMO
      * 作成者 装置の種類コード
           MOVE    LF01-CRE301-PHONEEQUIP
                                       TO  CLFRRES-CRE301-PHONEEQUIP
      * 作成者 生成者の資格 [必須]
           MOVE    LF01-CRE301-LICENSE
                                       TO  CLFRRES-CRE301-LICENSE
      *##
      * コンスタント値 (<extRefs />)                  const
           MOVE    "<extRefs />"       TO  CLFRRES-M-RX301-I1
      * 予約請求モジュールの状態 (claim:status)
           MOVE    LF01-UKEYMD-KREPCODE
                                       TO  CLFRRES-UKEYMD-KREPCODE
      * オーダー発行日時 (claim:oderTime)
           MOVE    LF01-CONFIRMDATE4
                                       TO  CLFRRES-CONFIRMDATE4
      * 受付日時 (claim:registTime)
           MOVE    LF01-UKEYMD
                                       TO  CLFRRES-UKEYMD
      * 入院外来区分 (claim:admitFlag)
           MOVE    LF01-NYUGAIKBN
                                       TO  CLFRRES-NYUGAIKBN
      * 使用保険 (claim:insuranceUid)
           MOVE    LF01-FIL1
                                       TO  CLFRRES-FIL1
      * 共通医事請求コード体系
           MOVE    LF01-IJISKYCOD
                                       TO  CLFRRES-IJISKYCOD
      * 予約
           MOVE    LF01-YYK
                                       TO  CLFRRES-YYK
      * 予約コード (claim:appCode)
           MOVE    LF01-YYKCOD
                                       TO  CLFRRES-YYKCOD
      *##
      * コンスタント値 (claim:appCodeId="Claim009"")
           MOVE    'claim:appCodeId="Claim009"'
                                       TO  CLFRRES-C-B3
      * 予約メモ
           MOVE    LF01-YYKMEMO
                                       TO  CLFRRES-YYKMEMO
      *診療科名称
           MOVE    LF01-SRYNAME
                                       TO  CLFRRES-SRYNAME
      *診療科表記
           MOVE    LF01-SRYREP
                                       TO  CLFRRES-SRYREP
      * 診療科ID
           MOVE    LF01-SRYID
                                       TO  CLFRRES-SRYID
      * 診療科IDのテーブル名
           MOVE    LF01-SRYIDTBL
                                       TO  CLFRRES-SRYIDTBL
      * 病棟診療科名称
           MOVE    LF01-SNRNAME
                                       TO  CLFRRES-SNRNAME
      * 病棟診療科 表記法
           MOVE    LF01-SNRREP
                                       TO  CLFRRES-SNRREP
      * 病棟診療科名称
           MOVE    LF01-SNRID
                                       TO  CLFRRES-SNRID
      * 病棟診療科 IDの種類コード
           MOVE    LF01-SNRIDTBL
                                       TO  CLFRRES-SNRIDTBL
      * 診療種別コード (claim:classCode)
           MOVE    LF01-BUNDLE-CLASSCODE
                                       TO  CLFRRES-BUNDLE-CLASSCODE
      *医事請求コード  
           MOVE    LF01-IJICD          TO  CLFRRES-IJICD
      * 医事請求コード体系名
           MOVE    LF01-ID             TO  CLFRRES-ID
      * 医事請求名称
           MOVE    LF01-HOSPSBT        TO  CLFRRES-HOSPSBT
      * 受付メモ
           MOVE    LF01-UKE-MEMO       TO  CLFRRES-UKE-MEMO
      *
           .
       20021-CLFRRES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    受付送信済み更新処理
      *****************************************************************
       2004-SYORI-02-SEC           SECTION.
      *
      *
           INITIALIZE                  WRK-CLAIM-PARA
           MOVE    WRK-ACCEPT-TIME     TO  WRK-PARA-TIME
      *
           MOVE    SPA-PTID            TO  WRK-PARA-PTID
           MOVE    WRK-SRYKA           TO  WRK-PARA-SRYKA
           MOVE    WRK-DRCD            TO  WRK-PARA-DRCD
           MOVE    CLFRREQ-COMBINATION-NUMBER
                                       TO  WRK-PARA-HKNCOMBI
           MOVE    CLFRREQ-SEND-STATUS TO  WRK-PARA-RC
      *
           CALL    "ORCL0012"          USING   WRK-PARA-PTID
                                               WRK-PARA-SRYKA
                                               WRK-PARA-TIME
                                               SPA-HOSPNUM
                                               WRK-PARA-DRCD
                                               WRK-PARA-HKNCOMBI
                                               WRK-PARA-RC
                                               SPA-SYSYMD
                                               WRK-PARA-ENDRC
           EVALUATE    WRK-PARA-ENDRC
               WHEN    "01"
                   MOVE    "20"            TO  WRK-ERRCD
               WHEN    "99"
                   MOVE    "29"            TO  WRK-ERRCD
           END-EVALUATE
      *
           .
       2004-SYORI-02-EXT.
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
           WHEN    "10"
               MOVE    "患者番号に該当する患者が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "受付日が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "12"
               MOVE    "受付時間が数値ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE    "診療科が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "14"
               MOVE    "ドクターが存在しません。"
                                               TO  WRK-ERRMSG
           WHEN    "15"
               MOVE    "リクエスト番号が違います"
                                               TO  WRK-ERRMSG
           WHEN    "20"
               MOVE    "更新対象がありません。"
                                               TO  WRK-ERRMSG
           WHEN    "21"
               MOVE    "受付更新エラーです。"
                                               TO  WRK-ERRMSG
      *警告
           WHEN    "K1"
               MOVE    "受付日を設定しました"
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
      *
