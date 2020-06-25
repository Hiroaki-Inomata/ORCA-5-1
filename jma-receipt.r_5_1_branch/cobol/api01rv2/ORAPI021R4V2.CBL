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
       PROGRAM-ID.         ORAPI021R4V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 併用禁忌薬剤情報返却  (xml2)
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
           03  FLG-TEND                PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-SSKIJYO             PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-TENSUPLUS           PIC 9(01).
           03  FLG-INTERACT           PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-ARI                 PIC 9(01).
           03  FLG-ARI2                PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
           03  FLG-CHK02               PIC 9(01).
           03  FLG-CHKMST              PIC 9(01).
           03  FLG-SP                  PIC 9(01).
      *
           03  FLG-KINKI-ARI           PIC 9(01).
           03  FLG-KINKI-CHK           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-K1              PIC 9(04).
           03  IDX-K2              PIC 9(04).
           03  IDX-K3              PIC 9(04).
           03  IDX-K4              PIC 9(04).
           03  IDX-C               PIC 9(04).
           03  IDX-C1              PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-S2              PIC 9(04).
      *
           03  IDXR                PIC 9(02).
           03  IDXM                PIC 9(02).
      *    診療行為用
      *    03  IDX-ACT             PIC 9(02).
      *    03  IDX-REN             PIC 9(04).
      *    03  IDXA                PIC 9(04).
      *    03  IDX-S               PIC 9(04).
      *    03  IDX-T               PIC 9(04).
      *    03  IDX-TBL             PIC 9(04).
      *
      *    03  IDX-D               PIC 9(02).
      *
      *    03  TBL-MAX             PIC 9(04).
           03  IDX-MAX             PIC 9(04).
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
           03  WRK-KEICD               PIC X(02).
      *
           03  WRK-MCP-PATHNAME        PIC X(62).
           03  WRK-MCP-WINDOW          PIC X(62).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYM           PIC X(06).
               05  WRK-SYSDD           PIC X(02).
      *
           03  WRK-SRYCD               PIC X(09).
           03  WRK-KNK-SRYCD           PIC X(09).
           03  WRK-SYOJYOUCD           PIC X(07).
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
       01  WRK-XML-AREA.
           03  WRK-PERFORM-DATE.
               05  WRK-PERFORM-DATE1       PIC X(07).
               05  WRK-PERFORM-DATE2       PIC X(03).
           03  WRK-PERFORM-YMD.
               05  WRK-PERFORM-YM          PIC X(06).
               05  WRK-PERFORM-DD          PIC X(02).
           03  WRK-PTNUM               PIC X(20).
           03  WRK-PTID                PIC 9(10).
           03  WRK-KIKAN               PIC 9(02).
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(100).
           03  WRK-ATO-INPUT           PIC X(100).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *
      *処理最大行
       01  MAX-LINE-VALUE          PIC 9(04)   VALUE   400.
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
           COPY    "CPORCSNUM.INC".
      *
      *
      *    パラメタ
           COPY    "CPORCSCKNKCHK.INC".
      *
      *    初期ＳＰＡ編集処理パラメタ
           COPY    "CPORCSC95.INC".
      *
      *    画面スパ領域
           COPY    "K01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "K02SCR-SPA".
      *
      *    ＳＰＡパラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPCONTRACHECKV2REQ.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPCONTRACHECKV2RES.INC".
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
      ***  COPY    "CPSK1005.INC".
      *    （医療機関情報）
           COPY    "CPSK1001.INC".
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    症状処置機序マスタ
       01  SSKIJYO-REC.
           COPY    "CPSSKIJYO.INC".
      *
      *    相互作用レコード
       01  INTERACT-REC.
           COPY    "CPINTERACT.INC".
      *
      *    チェックマスタ
       01  CHK-REC.
           COPY    "CPCHK.INC".
      *
      *    受診履歴
      *01  JYURRK-REC.
      *    COPY    "CPJYURRK.INC".
      *
      *    保険組み合わせ
      *01  HKNCOMBI-REC.
      *    COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
      *01  PTHKNINF-REC.
      *    COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
      *01  PTKOHINF-REC.
      *    COPY    "CPPTKOHINF.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    診療行為マスタ−
      *01  SRYACT-REC.
      *    COPY    "CPSRYACT.INC".
      *    コメント
      *01  PTCOM-REC.
      *    COPY    "CPPTCOM.INC".
      *
      *    診療会計マスタ−
      *01  SRYACCT-REC.
      *    COPY    "CPSRYACCT.INC".
      *    診療会計附加情報マスタ−
      *01  SRYACCTPLUS-REC.
      *    COPY    "CPSRYACCTPLUS.INC".
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
           DISPLAY   "*******************"
           DISPLAY   "* medicalget start*"
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
               MOVE   404                  TO  APILST26-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-APTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "*******************"
           DISPLAY   "* medicalget end  *"
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
           INITIALIZE                      XML-CONTRA-CHECKV2REQ
           INITIALIZE                      XML-CONTRA-CHECKV2RES
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Medical Info"       TO  CHKRES-RESKEY
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  CHKRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  CHKRES-INFORMATION-TIME
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
           EVALUATE    CHKREQ-REQUEST-NUMBER
           WHEN    "01"
      *        薬剤禁忌情報
               MOVE    1                   TO  FLG-SYORI
           WHEN    "02"
      *        症状詳記明細
               MOVE    2                   TO  FLG-SYORI
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
           END-EVALUATE
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           EVALUATE    CHKREQ-REQUEST-NUMBER
           WHEN    "01"
      *        薬剤禁忌情報
               PERFORM 2002-SYORI-01-SEC
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
           IF      APILST26-BODY     NOT =   LOW-VALUE
               DISPLAY "CHKRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_contraindication_checkv2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST26-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "contraindication_checkreq"
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
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_contraindication_checkv2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST26-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "contraindication_checkreq"
                                       TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ          TO  XML-CONTRA-CHECKV2REQ
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
      *    エラーメッセージ編集
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-KEICD           TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD           =   SPACE
               MOVE    ZERO                TO  CHKRES-API-RESULT
               MOVE    "処理終了"          TO  CHKRES-API-RESULT-MSG
           ELSE
               IF      WRK-ERRMSG          =   SPACE
      *        エラー・警告メッセージ
                   PERFORM 890-ERRCD-MSG-SEC
               END-IF
               MOVE    WRK-ERRCD           TO  CHKRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  CHKRES-API-RESULT-MSG
           END-IF
      *
           PERFORM 3001-MDC01XMLMAKE-SEC
      *
           .
       300-APTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理（受診履歴内容）
      *****************************************************************
       3001-MDC01XMLMAKE-SEC        SECTION.
      *
           IF      APILST26-BODY     NOT =   LOW-VALUE
               DISPLAY "CHKRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_contraindication_checkv2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  CHKRES-MODE
           MOVE    "contraindication_checkres"
                                       TO  CHKRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-CONTRA-CHECKV2RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_contraindication_checkv2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  CHKRES-MODE
           MOVE    "contraindication_checkres"
                                       TO  CHKRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-CONTRA-CHECKV2RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    CHKRES-OBJECT       TO  APILST26-BODY 
               MOVE    "application/xml"   TO  APILST26-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       3001-MDC01XMLMAKE-EXT.
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
      *    診療日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PERFORM-CHK-SEC
           END-IF
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PTNUM-CHK-SEC
           END-IF
      *    禁忌期間チェック
           IF     (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-KIKAN-CHK-SEC
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
           MOVE    CHKREQ-PATIENTID        TO  WRK-PTNUM
      *    診療日付
           MOVE    SPACE                   TO  WRK-HEN-DATE
           MOVE    CHKREQ-PERFORM-YM       TO  WRK-HEN-DATE(1:7)
           MOVE    SPACE                   TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-PERFORM-YMD
      *    未設定時はシステム情報設定
           IF      WRK-PERFORM-YMD         =   SPACE
               MOVE    SMCNDATE-YMD        TO  WRK-PERFORM-YMD
           END-IF
           MOVE    "01"                    TO  WRK-PERFORM-DD
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
           MOVE    WRK-PERFORM-YMD     TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  WRK-PERFORM-DATE
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
           MOVE    SPTID-PTNUM         TO  WRK-PTNUM
           MOVE    SPTID-PTID          TO  SPA-PTID
           MOVE    SPTID-PTID          TO  WRK-PTID
           MOVE    WRK-PTNUM           TO  CHKRES-PATIENTID
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
      *    対象期間月数コードチェック
      *****************************************************************
       20011-KIKAN-CHK-SEC         SECTION.
      *
      *    対象期間月数
           IF      CHKREQ-CHECK-TUKI    =   SPACE
               MOVE    ZERO                TO  WRK-KIKAN
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    CHKREQ-CHECK-TUKI   TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
      *            数値エラー
                   MOVE    "12"                TO  WRK-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-KIKAN
               END-IF
           END-IF
      *
           .
       20011-KIKAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴編集処理
      *****************************************************************
       2002-SYORI-01-SEC           SECTION.
      *
      *    患者情報内容編集
           PERFORM 20021-CHKRES-HEN-SEC
      *
           MOVE    WRK-PERFORM-YMD     TO  SPA-SRYYMD
      *
           INITIALIZE                  SPA-K02
           INITIALIZE                  SPA-SCR-REC
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
      *    SPA患者情報
           MOVE    WRK-PTID            TO  SPA-PTID
           MOVE    WRK-PTNUM           TO  SPA-PTNUM
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
      *    入院とする
           MOVE    "1"                 TO  SPA-NYUGAIKBN
      *    相互作用チェック期間
           IF      WRK-KIKAN       NOT =   ZERO
               MOVE    WRK-KIKAN           TO  SPA-INTERACT-KIKAN
           END-IF
           IF      SPA-INTERACT-KIKAN  =   ZERO
               MOVE    "13"                TO  WRK-ERRCD
               GO      TO      2002-SYORI-01-EXT
           END-IF
      *    薬剤コード編集
           PERFORM 20021-INPUTCD-HEN-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      2002-SYORI-01-EXT
           END-IF
      *    相互作用チェック期間の投薬編集
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                  ORCSCKNKCHKAREA
           MOVE    "4"                 TO  ORCSCKNKCHK-KBN 
           CALL    "ORCSCKNKCHK"       USING
                                       ORCSCKNKCHKAREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
      *    併用禁忌薬剤チェック
           PERFORM 20022-KINKI-HEN-SEC
           .
       2002-SYORI-01-EXT.
           EXIT.
      *****************************************************************
      *    患者情報出力情報編集処理
      *****************************************************************
       20021-CHKRES-HEN-SEC     SECTION.
      *
           MOVE    WRK-PTNUM           TO  CHKRES-PATIENTID
      *
           MOVE    PTINF-NAME          TO  CHKRES-NAME
           MOVE    PTINF-KANANAME      TO  CHKRES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  CHKRES-BIRTHDAY
           MOVE    PTINF-SEX           TO  CHKRES-SEX
      *
           MOVE    WRK-PERFORM-DATE1   TO  CHKRES-PERFORM-YM
      *
           .
       20021-CHKRES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    薬剤コード編集処理
      *****************************************************************
       20021-INPUTCD-HEN-SEC          SECTION.
      *
      *    テーブル最大行
           MOVE    MAX-LINE-VALUE      TO  SPA-MAX-LINE
      *
           INITIALIZE                  SPA-SCR-REC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   30  )
                           OR (CHKREQ-PRSCRPT-CODE (IDX)   =   SPACE)
               MOVE    CHKREQ-PRSCRPT-CODE (IDX)   TO
                                               CHKRES-PRSCRPT-CODE(IDX)
      *        点数マスタ検索
               MOVE    CHKREQ-PRSCRPT-CODE (IDX)   TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF     (FLG-TENSU           =   ZERO )
                  AND (CHKREQ-PRSCRPT-CODE (IDX)(1:1)  =   "6" )
                   ADD     1               TO  IDY
                   MOVE    CHKREQ-PRSCRPT-CODE (IDX)
                                           TO  SPA-COM-INPUTCD(IDY)
                   MOVE    TNS-NAME        TO  SPA-COM-NAME   (IDY)
                   MOVE    TNS-NAME        TO  CHKRES-PRSCRPT-NAME
                                                              (IDX)
               ELSE
                   MOVE    "20"            TO  WRK-ERRCD
                   PERFORM 890-ERRCD-MSG-SEC
                   MOVE    WRK-ERRCD       TO  CHKRES-PRSCRPT-ERRCD 
                                                              (IDX)
                   MOVE    WRK-ERRMSG      TO  CHKRES-PRSCRPT-ERRMSG
                                                              (IDX)
                   MOVE    SPACE           TO  WRK-ERRCD
               END-IF
           END-PERFORM
      *
           IF      IDY                 =   ZERO
               MOVE    "14"            TO  WRK-ERRCD
           END-IF
           .
       20021-INPUTCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    併用禁忌薬剤チェック処理
      *****************************************************************
       20022-KINKI-HEN-SEC        SECTION.
      *
           MOVE    ZERO                TO  IDX-Z
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   30  )
                           OR (CHKRES-PRSCRPT-CODE (IDX)   =   SPACE)
               IF      CHKRES-PRSCRPT-ERRCD (IDX)  =   SPACE
      *
                   ADD     1                   TO  IDX-Z
      *
                   IF     (SPA-COM-INPUTCD(IDX-Z)  =
                                               CHKRES-PRSCRPT-CODE(IDX))
                       PERFORM 2001-KNKYAK-CHK-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
      *    症状詳記内容編集
           PERFORM 2002-SAYOKIJYO-HEN-SEC
           .
       20022-KINKI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌編集処理
      *****************************************************************
       2001-KNKYAK-CHK-SEC        SECTION.
      *
      *    相互作用レコードより、禁忌有無のチェックを行う
           INITIALIZE                  INTERACT-REC
           MOVE    SPA-COM-INPUTCD (IDX-Z)
                                       TO  INTERACT-DRUGCD
      *
           MOVE    ZERO                TO  FLG-INTERACT
           MOVE    INTERACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_interact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-INTERACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INTERACT
           END-IF
      *
      *    禁忌チェック
           MOVE    ZERO                TO  IDX-K2
           ADD     1                   TO  IDX-K1
           MOVE    ZERO                TO  FLG-ARI2
           PERFORM
                   UNTIL      FLG-INTERACT     =   1
               PERFORM 20011-KNKCHK-SYORI-SEC
      *
               MOVE    "tbl_interact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-INTERACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_interact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    チェックマスタとのチェック
           PERFORM 2009-KNKYKCHKMST-CHK-SEC
           .
       2001-KNKYAK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    禁忌薬剤チェック処理
      *****************************************************************
       20011-KNKCHK-SYORI-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-KINKI-ARI
           MOVE    ZERO                TO  FLG-KINKI-CHK
      *    診療会計マスタとのチェック
           PERFORM VARYING     IDX-S       FROM    1   BY  1
                   UNTIL      (IDX-S           >   SPA-KNK-MAX )  OR
                              (FLG-KINKI-ARI   =   1   ) OR
                              (IDX-K2          >   30  )  OR
                              (SPA-KNK-SRYCD (IDX-S)   =   SPACE )
               MOVE    ZERO                    TO  FLG-KINKI-CHK
               IF    ( SPA-COM-INPUTCD (IDX-Z)
                                           NOT =   SPA-KNK-SRYCD(IDX-S))
      *            前
                   IF     (SPA-KNK-SRYCD (IDX-S)   =   INTERACT-DRUGCD2)
                       MOVE    1                   TO  FLG-KINKI-CHK
                   END-IF
      *            後
                   IF     (SPA-KNK-SRYCD (IDX-S)   =   INTERACT-DRUGCD)
                       MOVE    2                   TO  FLG-KINKI-CHK
                   END-IF
      *            今回入力分は対象外
                   PERFORM 200111-INPUT-CHK-SEC
               END-IF
               IF      FLG-KINKI-CHK       NOT =   ZERO
                   ADD     1                   TO  IDX-K2
      *
                   MOVE    1                   TO  FLG-KINKI-ARI
                   MOVE    SPA-KNK-SRYCD (IDX-S)   TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
                   IF      FLG-TENSU           =   1
                       MOVE    SPA-KNK-SRYCD (IDX-S)   TO  TNS-SRYCD
                       PERFORM 919-TENSU-READ2-SEC
                   END-IF
      *            禁忌薬名称
                   MOVE    TNS-NAME            TO  CHKRES-DRUGCD-NAME
                                                        (IDX IDX-K2)
      *            禁忌薬コード
                   MOVE    SPA-KNK-SRYCD (IDX-S)
                                               TO  CHKRES-DRUGCD-CODE
                                                        (IDX IDX-K2)
      *            症状詳記区分
                   MOVE    INTERACT-SYOJYOUCD  TO  CHKRES-INTERACT-CODE
                                                        (IDX IDX-K2)
      *            投与日
                   MOVE    SPA-KNK-TOUYMD (IDX-S)  TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE        TO  CHKRES-ADMINISTE-DATE
                                                        (IDX IDX-K2)
      *            方向性
                   IF      FLG-KINKI-CHK       =   2
      *                "←"
                       MOVE    "after"         TO  CHKRES-CONTEXT-CLASS
                                                        (IDX IDX-K2)
                   ELSE
      *                "→"
                       MOVE    "before"        TO  CHKRES-CONTEXT-CLASS
                                                        (IDX IDX-K2)
                   END-IF
               END-IF
           END-PERFORM
      *
      *    画面入力分とのチェック
           PERFORM VARYING     IDX-S       FROM    1   BY  1
                   UNTIL      (IDX-S           >   SPA-MAX-LINE)  OR
                              (FLG-KINKI-ARI   =   1    ) OR
                              (IDX-K2          >   30  )  OR
                              (SPA-GMN-INPUTCD(IDX-S)  =   SPACE  AND
                               SPA-COM-INPUTCD(IDX-S)  =   SPACE )
               IF     (SPA-COM-INPUTCD (IDX-S) =   INTERACT-DRUGCD2)
                 AND  (SPA-COM-INPUTCD (IDX-S)
                                           NOT =   SPA-COM-INPUTCD
                                                           (IDX-Z))
                 AND  (IDX-S               NOT =   IDX-Z          )
                   MOVE    ZERO                TO  FLG-ARI
                   IF      IDX-K2              >   ZERO
                       MOVE    SPA-COM-INPUTCD (IDX-S) TO  WRK-SRYCD
                       PERFORM 200111-TBL-CHEK-SEC
                   ELSE
                       ADD     1                   TO  IDX-K2
                   END-IF
      *
                   IF      FLG-ARI             =   ZERO
      *                禁忌薬名称
                       MOVE    SPA-COM-NAME   (IDX-S)
                                               TO  CHKRES-DRUGCD-NAME
                                                        (IDX IDX-K2)
      *                禁忌薬コード
                       MOVE    SPA-COM-INPUTCD (IDX-S)
                                               TO  CHKRES-DRUGCD-CODE
                                                        (IDX IDX-K2)
      *                症状詳記区分
                       MOVE    INTERACT-SYOJYOUCD
                                               TO  CHKRES-INTERACT-CODE
                                                        (IDX IDX-K2)
      *                今回入力分
                       MOVE    "1"             TO  CHKRES-ADMINISTE-KBN
                                                        (IDX IDX-K2)
      *                方向性
      *                "→"
                       MOVE    "before"        TO  CHKRES-CONTEXT-CLASS
                                                        (IDX IDX-K2)
      *
                   ELSE
      *            今回入力を優先とする
      *                投与日は削除
                       MOVE    SPACE           TO  CHKRES-ADMINISTE-DATE
                                                        (IDX IDX-K2)
      *                今回入力分
                       MOVE    "1"             TO  CHKRES-ADMINISTE-KBN
                                                        (IDX IDX-K2)
      *
                   END-IF
                   MOVE    1                       TO  FLG-KINKI-ARI
               END-IF
           END-PERFORM
      *
           .
       20011-KNKCHK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    重複チェック処理
      *****************************************************************
       200111-TBL-CHEK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-ARI
           PERFORM VARYING     IDX-C1      FROM    1   BY  1
                   UNTIL      (IDX-C1          >   30  )
                          OR  (FLG-ARI         =   1   )
               IF      CHKRES-INTERACT-CODE (IDX IDX-C1)   =   SPACE
                   MOVE    30                  TO  IDX-C1
               ELSE
                   MOVE    IDX-C1              TO  IDX-K2
               END-IF
               IF     (CHKRES-INTERACT-CODE (IDX IDX-C1)
                                               =   WRK-SRYCD )
                  AND (CHKRES-INTERACT-CODE (IDX IDX-C1)
                                               =   INTERACT-SYOJYOUCD)
                   MOVE    IDX-C1              TO  IDX-K2
                   MOVE    1                   TO  FLG-ARI
               END-IF
           END-PERFORM
           IF      FLG-ARI             =   ZERO
               ADD     1               TO  IDX-K2
           END-IF
      *
           .
       200111-TBL-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    今回入力分チェック処理
      *****************************************************************
       200111-INPUT-CHK-SEC              SECTION.
      *
           PERFORM VARYING     IDX-S2      FROM    1   BY  1
                   UNTIL      (IDX-S2          >   SPA-MAX-LINE )  OR
                              (SPA-GMN-INPUTCD(IDX-S2) =   SPACE  AND
                               SPA-COM-INPUTCD(IDX-S2) =   SPACE )
               IF     (SPA-COM-INPUTCD (IDX-S2)
                                            =   SPA-KNK-SRYCD (IDX-S))
                 AND  (SPA-COM-INPUTCD (IDX-S2)
                                        NOT =   SPA-COM-INPUTCD
                                                           (IDX-Z))
                   MOVE    ZERO             TO  FLG-KINKI-CHK
               END-IF
           END-PERFORM
           .
       200111-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェックマスタとのチェック処理
      *****************************************************************
       2009-KNKYKCHKMST-CHK-SEC              SECTION.
      *
           INITIALIZE                  CHK-REC
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    "4"                 TO  CHK-CHKKBN
           MOVE    SPA-COM-INPUTCD (IDX-Z)
                                       TO  CHK-SRYCD
           MOVE    "2"                 TO  CHK-CDKBN
           MOVE    ZERO                TO  CHK-RENNUM
           MOVE    SPA-SRYYMD          TO  CHK-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  CHK-YUKOEDYMD
      *
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_chk"           TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 950-CHK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-CHKMST
           END-IF
           PERFORM
                   UNTIL      FLG-CHKMST       =   1
               MOVE    ZERO                    TO  FLG-ARI
               PERFORM VARYING     IDX-C   FROM    1   BY  1
                       UNTIL      (IDX-C       >   30  )  OR 
                                  (CHKRES-DRUGCD-CODE (IDX IDX-C)
                                               =   SPACE)
                   IF      CHKRES-DRUGCD-CODE (IDX IDX-C)  =   CHK-CD
                                                         OR  CHK-SRYCD
                        MOVE    1              TO  FLG-ARI
                        MOVE    30             TO  IDX-C
                   END-IF
               END-PERFORM
               IF      FLG-ARI             =   ZERO
                   MOVE    CHK-SRYCD           TO  INTERACT-DRUGCD
                   MOVE    CHK-CD              TO  INTERACT-DRUGCD2
                   MOVE    ALL "*"             TO  INTERACT-SYOJYOUCD
                   PERFORM 20011-KNKCHK-SYORI-SEC
               END-IF
      *
               MOVE    "tbl_chk"           TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 950-CHK-READ-SEC
           END-PERFORM
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2009-KNKYKCHKMST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記内容編集処理
      *****************************************************************
       2002-SAYOKIJYO-HEN-SEC              SECTION.
      *
           INITIALIZE                      CHKRES-SSKIJYO-INF-G
      *
           PERFORM VARYING     IDX         FROM    1   BY  1
                   UNTIL      (IDX             >   30  )
                       OR     (CHKRES-PRSCRPT-CODE (IDX) =   SPACE)
               PERFORM VARYING     IDX-S       FROM    1   BY  1
                       UNTIL      (IDX-S           >   30  )
                           OR     (CHKRES-INTERACT-CODE(IDX IDX-S)
                                                   =   SPACE )
                   IF      CHKRES-INTERACT-CODE (IDX IDX-S)  =   SPACE
                                                         OR  ALL "*"
                       CONTINUE
                   ELSE
                       MOVE    CHKRES-INTERACT-CODE (IDX IDX-S)
                                                    TO  WRK-SYOJYOUCD
                       PERFORM 20021-SYOJYOUCD-SYORI-SEC
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       2002-SAYOKIJYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記内容編集処理
      *****************************************************************
       20021-SYOJYOUCD-SYORI-SEC              SECTION.
      *
           MOVE    ZERO                 TO  FLG-CHK
           PERFORM VARYING     IDY          FROM    1   BY  1
                   UNTIL      (IDY             >   50  )
               IF      CHKRES-SYOJYOUCD (IDY)  =   SPACE
                   MOVE    WRK-SYOJYOUCD       TO  CHKRES-SYOJYOUCD
                                                                (IDY)
      *            症状処置機序検索
                   INITIALIZE                  SSKIJYO-REC
                   MOVE    WRK-SYOJYOUCD       TO  SSKIJYO-SYOJYOUCD
                   PERFORM 910-SSKIJYO-READ-SEC
                   IF      FLG-SSKIJYO         =   ZERO
                       MOVE    SSKIJYO-SYOJYOU     TO  CHKRES-SYOJYOU
                                                                (IDY)
                       MOVE    SSKIJYO-SAYOKIJYO   TO  CHKRES-SAYOKIJYO
                                                                (IDY)
                   END-IF
               END-IF
               IF      CHKRES-SYOJYOUCD (IDY)  =   WRK-SYOJYOUCD
                   MOVE    1                   TO  FLG-CHK
                   MOVE    50                  TO  IDY
               END-IF
           END-PERFORM
      *
           IF      FLG-CHK             =   ZERO
      *        症状機序オーバー
               MOVE    "K1"                TO  WRK-KEICD
           END-IF
      *
           .
       20021-SYOJYOUCD-SYORI-EXT.
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
               MOVE    "患者番号の設定がありません"
                                               TO  WRK-ERRMSG
           WHEN    "10"
               MOVE    "患者番号に該当する患者が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "診療日が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "12"
               MOVE    "対象期間月数が数値ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE    "対象期間月数がゼロです。チェックできません"
                                               TO  WRK-ERRMSG
           WHEN    "14"
               MOVE    "対象がありません"
                                               TO  WRK-ERRMSG
           WHEN    "20"
               MOVE    "薬剤コードが存在しません。"
                                               TO  WRK-ERRMSG
      *警告
           WHEN    "K1"
               STRING  "警告！"
                       "症状コードが５０件以上存在します。"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-ERRMSG
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
               PERFORM 920-DBFETCH-SEC
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
      *    症状処置機序取得処理
      *****************************************************************
       910-SSKIJYO-READ-SEC            SECTION.
      *
           MOVE    SSKIJYO-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sskijyo"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SSKIJYO-REC
                   MOVE    ZERO                TO  FLG-SSKIJYO
               ELSE
                   MOVE    1                   TO  FLG-SSKIJYO
                   INITIALIZE                  SSKIJYO-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SSKIJYO
               INITIALIZE                  SSKIJYO-REC
           END-IF
           MOVE    "tbl_sskijyo"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-SSKIJYO-READ-EXT.
           EXIT.
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
               PERFORM 920-DBFETCH-SEC
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
      *    点数附加情報検索
      *    IF     (FLG-TENSU           =   ZERO)
      *        PERFORM 9101-TENSUPLUS-READ-SEC
      *    ELSE
      *        INITIALIZE                      TENSUPLUS-REC
      *    END-IF
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       919-TENSU-READ2-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
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
           PERFORM UNTIL     FLG-TENSU     =   1
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
      *
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-TENSU
               ELSE
                   MOVE    1               TO  FLG-TENSU
               END-IF
           END-PERFORM
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       919-TENSU-READ2-EXT.
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
      *    相互作用マスタ読込
      *****************************************************************
       990-INTERACT-READ-SEC         SECTION.
      *
           MOVE    "tbl_interact"      TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  INTERACT-REC
               MOVE    ZERO            TO  FLG-INTERACT
           ELSE
               INITIALIZE                  INTERACT-REC
               MOVE    1               TO  FLG-INTERACT
           END-IF
      *
           .
       990-INTERACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェックマスタ読込
      *****************************************************************
       950-CHK-READ-SEC         SECTION.
      *
           MOVE    "tbl_chk"           TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  CHK-REC
               MOVE    ZERO            TO  FLG-CHKMST
           ELSE
               INITIALIZE                  CHK-REC
               MOVE    1               TO  FLG-CHKMST
           END-IF
      *
           .
       950-CHK-READ-EXT.
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
      *
