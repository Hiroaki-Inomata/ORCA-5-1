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
       PROGRAM-ID.         ORAPICLAIMR2V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : ＣＬＡＩＭ請求確認送信(xml2)
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
           03  IDY2                PIC 9(04).
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
           03  WRK-DENPNUM             PIC 9(07).
      *
       01  WRK-XML-AREA.
           03  WRK-PERFORM-DATE         PIC X(10).
      *
           03  WRK-PERFORM-YMD          PIC X(08).
           03  WRK-PERFORM-TIME         PIC X(06).
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
       01  LF02-REC. 
           COPY    "CPRLF002.INC".
      *
       01  WRK-CLAIM-PARA.
           03  WRK-PARA.
               05  WRK-PARA-CODE       PIC  X(01).
               05  WRK-PARA-PTID       PIC  X(10).
               05  WRK-PARA-SRYKA      PIC  X(02).
               05  WRK-PARA-HOSPNUM    PIC  X(02).
               05  WRK-PARA-SYSYMD     PIC  X(08).
               05  WRK-PARA-SRYYMD     PIC  X(08).
               05  WRK-PARA-NYUGAIKBN  PIC  X(01).
               05  WRK-PARA-HKNCOMBI   PIC  X(04).
               05  WRK-PARA-DRCD       PIC  X(05).
               05  WRK-PARA-DENPNUM    PIC  X(07).
               05  WRK-PARA-SYORIFLG   PIC  X(01).
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
      *    API XML読み込み用定義体
           COPY    "CPCLAIMACCOUNTV2REQ.INC".
      *    API XML書き込み用定義体
           COPY    "CPCLAIMACCOUNTV2RES.INC".
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
           DISPLAY   "**********************"
           DISPLAY   "* claim_account start*"
           DISPLAY   "**********************"
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
               MOVE   404                  TO  APILST24-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-APTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "*********************"
           DISPLAY   "* claim_account end *"
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
           INITIALIZE                      XML-CLAIM-ACCOUNTV2REQ
           INITIALIZE                      XML-CLAIM-ACCOUNTV2RES
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *****MOVE   "Medical Info"       TO  CLACRES-RESKEY
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  CLACRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  CLACRES-INFORMATION-TIME
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
               EVALUATE    CLACREQ-REQUEST-NUMBER
                   WHEN    "01"
      *                ＣＬＡＩＭデータ情報編集
                       PERFORM 2002-SYORI-01-SEC
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
           IF      APILST24-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_claim_accountv2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST24-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "claim_accountv2req"    TO  APIREQ-RECORDNAME
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
           MOVE    "xml_claim_accountv2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST24-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "claim_accountv2req"    TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ          TO  XML-CLAIM-ACCOUNTV2REQ
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
               MOVE    ZERO                TO  CLACRES-API-RESULT
               MOVE    "処理終了"          TO  CLACRES-API-RESULT-MSG
           ELSE
               IF      WRK-ERRMSG          =   SPACE
      *        エラー・警告メッセージ
                   PERFORM 890-ERRCD-MSG-SEC
               END-IF
               MOVE    WRK-ERRCD           TO  CLACRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  CLACRES-API-RESULT-MSG
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
           IF      APILST24-BODY   NOT =   LOW-VALUE
               DISPLAY "CLACRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_claim_accountv2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  CLACRES-MODE
           MOVE    "claim_accountv2res"    TO  CLACRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-CLAIM-ACCOUNTV2RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_claim_accountv2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  CLACRES-MODE
           MOVE    "claim_accountv2res"    TO  CLACRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-CLAIM-ACCOUNTV2RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    CLACRES-OBJECT      TO  APILST24-BODY 
               MOVE    "application/xml"   TO  APILST24-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
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
      *    診療日付チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PERFORM-DATE-SEC
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
      *    ドクターコードチェック
           IF     (WRK-DRCD        NOT =   SPACE  )
           AND    (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-DRCD-CHK-SEC
           END-IF
      *    伝票番号チェック
           IF     (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-DENPNUM-CHK-SEC
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
      *    診療日付
           MOVE    CLACREQ-PERFORM-DATE    TO  WRK-HEN-DATE
           MOVE    SPACE                   TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-PERFORM-YMD
      *    未設定時はシステム情報設定
           IF      WRK-PERFORM-YMD         =   SPACE
               MOVE    SMCNDATE-YMD        TO  WRK-PERFORM-YMD
               MOVE    "K1"                TO  WRK-KEICD
           END-IF
      *    診療日付とする
           MOVE    WRK-PERFORM-YMD     TO  SPA-SRYYMD
      *
      *    患者番号
           MOVE    CLACREQ-PATIENTID   TO  WRK-PTNUM
      *    診療科
           MOVE    CLACREQ-DEP-CODE    TO  WRK-SRYKA
      *    ドクター
           MOVE    CLACREQ-DR-CODE     TO  WRK-DRCD
           .
       20010-INPUT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日付チェック
      *****************************************************************
       20011-PERFORM-DATE-SEC    SECTION.
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
       20011-PERFORM-DATE-EXT.
           EXIT.
      *
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
      *****************************************************************
      *    伝票番号チェック
      *****************************************************************
       20011-DENPNUM-CHK-SEC         SECTION.
      *
      *    数値チェック
           INITIALIZE                      ORCSNUMAREA
           MOVE    CLACREQ-INVOICE-NUMBER  TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   7     )   OR
                  (SNUM-SYOSUU         >   0     )
      *        数値７桁
               MOVE    "16"                TO  WRK-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-DENPNUM
           END-IF
           .
       20011-DENPNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    送信内容編集処理
      *****************************************************************
       2003-CLAIMHEN-SEC        SECTION.
      *
           MOVE    WRK-PTNUM           TO  CLACRES-PATIENTID
      *    実施日(CCYY-MM-DDTHH:MM:SS形式)
           STRING  CLACRES-INFORMATION-DATE
                   "T"
                   CLACRES-INFORMATION-TIME
                                        DELIMITED  BY  SIZE
                                   INTO    CLACRES-INFORMATION-HEN
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
           MOVE    CLACREQ-SEND-CHARACTER-CODE
                                       TO  WRK-PARA-CODE
           MOVE    SPA-HOSPNUM         TO  WRK-PARA-HOSPNUM
           MOVE    SPA-PTID            TO  WRK-PARA-PTID
           MOVE    SPA-SRYYMD          TO  WRK-PARA-SRYYMD
           MOVE    SPA-SYSYMD          TO  WRK-PARA-SYSYMD
           MOVE    WRK-SRYKA           TO  WRK-PARA-SRYKA
           MOVE    "2"                 TO  WRK-PARA-NYUGAIKBN
           MOVE    WRK-DRCD            TO  WRK-PARA-DRCD
           MOVE    WRK-DENPNUM         TO  WRK-PARA-DENPNUM
           MOVE    CLACREQ-COMBINATION-NUMBER
                                       TO  WRK-PARA-HKNCOMBI
           MOVE    CLACREQ-UPDATE-CODE
                                       TO  WRK-PARA-SYORIFLG
           IF      WRK-PARA-SYORIFLG   =   SPACE
               MOVE    "0"             TO  WRK-PARA-SYORIFLG
           END-IF
      *
      *
           CALL    "ORCL0021"          USING   WRK-PARA-CODE
                                               WRK-PARA-PTID
                                               WRK-PARA-SRYKA
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-SYSYMD
                                               WRK-PARA-SRYYMD
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-HKNCOMBI
                                               WRK-PARA-DRCD
                                               WRK-PARA-DENPNUM
                                               WRK-PARA-SYORIFLG
                                               LF02-REC
      *
      *    患者情報内容編集
           PERFORM 20021-CLACRES-HEN-SEC
      *
           .
       2002-SYORI-01-EXT.
           EXIT.
      *****************************************************************
      *    取得情報編集処理
      *****************************************************************
       20021-CLACRES-HEN-SEC     SECTION.
      *
      * 患者主ID
           MOVE    LF02-PTID1          TO  CLACRES-PTID1
      * 患者主IDのテーブル名
           MOVE    LF02-IDTYPE1        TO  CLACRES-IDTYPE1
      * 作成者 個人ID [必須]
           MOVE    LF02-CRE001-PERSONALID
                                       TO  CLACRES-CRE001-PERSONALID
      * 作成者 個人フルネーム [条件付き必須]
           MOVE    LF02-CRE001-FULLNAME
                                       TO  CLACRES-CRE001-FULLNAME
      * 作成者 個人姓 [条件付き必須]
           MOVE    LF02-CRE001-FAMILYNAME
                                       TO  CLACRES-CRE001-FAMILYNAME
      * 作成者 個人名 [条件付き必須]
           MOVE    LF02-CRE001-GIVENNAME
                                       TO  CLACRES-CRE001-GIVENNAME
      * 作成者 ミドルネーム
           MOVE    LF02-CRE001-MIDDLENAME
                                       TO  CLACRES-CRE001-MIDDLENAME
      * 作成者 肩書きなど
           MOVE    LF02-CRE001-PREFIX
                                       TO  CLACRES-CRE001-PREFIX
      * 作成者 学位
           MOVE    LF02-CRE001-DEGREE
                                       TO  CLACRES-CRE001-DEGREE
      * 作成者 施設情報名
           MOVE    LF02-CRE001-FACILITYNAME
                                       TO  CLACRES-CRE001-FACILITYNAME
      * 作成者 施設ID
           MOVE    LF02-CRE001-FACILITYID
                                       TO  CLACRES-CRE001-FACILITYID
      * 作成者 診療科名称
           MOVE    LF02-CRE001-DPERTNAME
                                       TO  CLACRES-CRE001-DPERTNAME
      * 作成者 診療科ID
           MOVE    LF02-CRE001-DPERTID
                                       TO  CLACRES-CRE001-DPERTID
      * 作成者 住所
           MOVE    LF02-CRE001-ADRFULLNAME
                                       TO  CLACRES-CRE001-ADRFULLNAME
      * 作成者 住所の種類
           MOVE    LF02-CRE001-ADRCLASS
                                       TO  CLACRES-CRE001-ADRCLASS
      * 作成者 都道府県
           MOVE    LF02-CRE001-ADRPREF
                                       TO  CLACRES-CRE001-ADRPREF
      * 作成者 市、区、郡
           MOVE    LF02-CRE001-ADRCITY
                                       TO  CLACRES-CRE001-ADRCITY
      * 作成者 町、村
           MOVE    LF02-CRE001-ADRTOWN
                                       TO  CLACRES-CRE001-ADRTOWN
      * 作成者 番地、丁目等
           MOVE    LF02-CRE001-ADRHOMENUM
                                       TO  CLACRES-CRE001-ADRHOMENUM
      * 作成者 郵便番号
           MOVE    LF02-CRE001-ZIP
                                       TO  CLACRES-CRE001-ZIP
      * 作成者 国コード
           MOVE    LF02-CRE001-COUNTRY
                                       TO  CLACRES-CRE001-COUNTRY
      * 作成者 メールアドレス
           MOVE    LF02-CRE001-EMAIL
                                       TO  CLACRES-CRE001-EMAIL
      * 作成者 市外局番
           MOVE    LF02-CRE001-PHONEAREA
                                       TO  CLACRES-CRE001-PHONEAREA
      * 作成者 局番
           MOVE    LF02-CRE001-PHONECITY
                                       TO  CLACRES-CRE001-PHONECITY
      * 作成者 加入番号
           MOVE    LF02-CRE001-PHONENUM
                                       TO  CLACRES-CRE001-PHONENUM
      * 作成者 内線番号
           MOVE    LF02-CRE001-PHONEEXT
                                       TO  CLACRES-CRE001-PHONEEXT
      * 作成者 国番号
           MOVE    LF02-CRE001-PHONECOUNTRY
                                       TO  CLACRES-CRE001-PHONECOUNTRY
      * 作成者 使用時間帯などを記載
           MOVE    LF02-CRE001-PHONEMEMO
                                       TO  CLACRES-CRE001-PHONEMEMO
      * 作成者 装置の種類コード
           MOVE    LF02-CRE001-PHONEEQUIP
                                       TO  CLACRES-CRE001-PHONEEQUIP
      * 作成者 生成者の資格 [必須]
           MOVE    LF02-CRE001-LICENSE
                                       TO  CLACRES-CRE001-LICENSE
      *## コンスタント値 (<accessRight permit="all" />) const
           MOVE    '<accessRight permit="all" />'
                                       TO  CLACRES-M-RI401-ACC1
      *## コンスタント値 (Data of Amount) const
           MOVE    'Data of Amount'
                                       TO  CLACRES-M-RI401-TITLE
      *
      * [点数金額モジュール]
      * 文書ユニークID
           MOVE    LF02-UID1           TO  CLACRES-UID1
      * カルテ電子保存の確定日
           MOVE    LF02-CONFIRMDATE1   TO  CLACRES-CONFIRMDATE1
      * 作成者 個人ID [必須]
           MOVE    LF02-CRE401-PERSONALID
                                       TO  CLACRES-CRE401-PERSONALID
      * 作成者 個人フルネーム [条件付き必須]
           MOVE    LF02-CRE401-FULLNAME
                                       TO  CLACRES-CRE401-FULLNAME
      * 作成者 個人姓 [条件付き必須]
           MOVE    LF02-CRE401-FAMILYNAME
                                       TO  CLACRES-CRE401-FAMILYNAME
      * 作成者 個人名 [条件付き必須]
           MOVE    LF02-CRE401-GIVENNAME
                                       TO  CLACRES-CRE401-GIVENNAME
      * 作成者 ミドルネーム
           MOVE    LF02-CRE401-MIDDLENAME
                                       TO  CLACRES-CRE401-MIDDLENAME
      * 作成者 肩書きなど
           MOVE    LF02-CRE401-PREFIX
                                       TO  CLACRES-CRE401-PREFIX
      * 作成者 学位
           MOVE    LF02-CRE401-DEGREE
                                       TO  CLACRES-CRE401-DEGREE
      * 作成者 施設情報名
           MOVE    LF02-CRE401-FACILITYNAME
                                       TO  CLACRES-CRE401-FACILITYNAME
      * 作成者 施設ID
           MOVE    LF02-CRE401-FACILITYID
                                       TO  CLACRES-CRE401-FACILITYID
      * 作成者 診療科名称
           MOVE    LF02-CRE401-DPERTNAME
                                       TO  CLACRES-CRE401-DPERTNAME
      * 作成者 診療科ID
           MOVE    LF02-CRE401-DPERTID
                                       TO  CLACRES-CRE401-DPERTID
      * 作成者 住所
           MOVE    LF02-CRE401-ADRFULLNAME
                                       TO  CLACRES-CRE401-ADRFULLNAME
      * 作成者 住所の種類
           MOVE    LF02-CRE401-ADRCLASS
                                       TO  CLACRES-CRE401-ADRCLASS
      * 作成者 都道府県
           MOVE    LF02-CRE401-ADRPREF
                                       TO  CLACRES-CRE401-ADRPREF
      * 作成者 市、区、郡
           MOVE    LF02-CRE401-ADRCITY
                                       TO  CLACRES-CRE401-ADRCITY
      * 作成者 町、村
           MOVE    LF02-CRE401-ADRTOWN
                                       TO  CLACRES-CRE401-ADRTOWN
      * 作成者 番地、丁目等
           MOVE    LF02-CRE401-ADRHOMENUM
                                       TO  CLACRES-CRE401-ADRHOMENUM
      * 作成者 郵便番号
           MOVE    LF02-CRE401-ZIP
                                       TO  CLACRES-CRE401-ZIP
      * 作成者 国コード
           MOVE    LF02-CRE401-COUNTRY
                                       TO  CLACRES-CRE401-COUNTRY
      * 作成者 メールアドレス
           MOVE    LF02-CRE401-EMAIL
                                       TO  CLACRES-CRE401-EMAIL
      * 作成者 市外局番
           MOVE    LF02-CRE401-PHONEAREA
                                       TO  CLACRES-CRE401-PHONEAREA
      * 作成者 局番
           MOVE    LF02-CRE401-PHONECITY
                                       TO  CLACRES-CRE401-PHONECITY
      * 作成者 加入番号
           MOVE    LF02-CRE401-PHONENUM
                                       TO  CLACRES-CRE401-PHONENUM
      * 作成者 内線番号
           MOVE    LF02-CRE401-PHONEEXT
                                       TO  CLACRES-CRE401-PHONEEXT
      * 作成者 国番号
           MOVE    LF02-CRE401-PHONECOUNTRY
                                       TO  CLACRES-CRE401-PHONECOUNTRY
      * 作成者 使用時間帯などを記載
           MOVE    LF02-CRE401-PHONEMEMO
                                       TO  CLACRES-CRE401-PHONEMEMO
      * 作成者 装置の種類コード
           MOVE    LF02-CRE401-PHONEEQUIP
                                       TO  CLACRES-CRE401-PHONEEQUIP
      * 作成者 生成者の資格 [必須]
           MOVE    LF02-CRE401-LICENSE
                                       TO  CLACRES-CRE401-LICENSE
      *### コンスタント値 (<extRefs />)                const
           MOVE    "<extRefs />"       TO  CLACRES-M-RX401-I1
      *
      * 点数金額モジュールの状態(claimA:status) (accountを設定)
           MOVE    LF02-STATUS         TO  CLACRES-STATUS
      *オーダー発行日時
           MOVE    LF02-ORDER-TIME
                                       TO  CLACRES-ORDER-TIME
      *実施予定日時
           MOVE    LF02-APPOINT-TIME
                                       TO  CLACRES-APPOINT-TIME
      *受付日時
           MOVE    LF02-REGIST-TIME
                                       TO  CLACRES-REGIST-TIME
      *実施日時
           MOVE    LF02-PERFORM-TIME
                                       TO  CLACRES-PERFORM-TIME
      *会計終了日時
           MOVE    LF02-ACCOUNT-TIME
                                       TO  CLACRES-ACCOUNT-TIME
      *入外区分 (true OR false)
           MOVE    LF02-NYUGAIKBN
                                       TO  CLACRES-NYUGAIKBN
      *時間外区分
           MOVE    LF02-JIKANNGAIKBN
                                       TO  CLACRES-JIKANNGAIKBN
      *使用保険ＵＩＤ
           MOVE    LF02-HOKEN-UID
                                       TO  CLACRES-HOKEN-UID
      *共通医事請求コード体系名
           MOVE    LF02-DEFAULT-TABLED
                                       TO  CLACRES-DEFAULT-TABLED
      *診療科名称
           MOVE    LF02-SRYKA-NAME
                                       TO  CLACRES-SRYKA-NAME
      * 診療科名称の表記法 (mmlDp:repCode)
           MOVE    LF02-SRYKA-REPCODE
                                       TO  CLACRES-SRYKA-REPCODE
      * 診療科ID
           MOVE    LF02-SRYKA-ID
                                       TO  CLACRES-SRYKA-ID
      * 診療科IDの種類 (mmlCm:type)
           MOVE    LF02-SRYKA-IDCODE
                                       TO  CLACRES-SRYKA-IDCODE
      * 診療病棟名称
           MOVE    LF02-SNRBT-NAME
                                       TO  CLACRES-SNRBT-NAME
      * 診療病棟名称の表記法 (mmlDp:repCode)
           MOVE    LF02-SNRBT-REPCODE
                                       TO  CLACRES-SNRBT-REPCODE
      * 診療病棟ID
           MOVE    LF02-SNRBT-ID
                                       TO  CLACRES-SNRBT-ID
      * 診療病棟IDの種類 (mmlCm:type)
           MOVE    LF02-SNRBT-IDCODE
                                       TO  CLACRES-SNRBT-IDCODE
      * 健康保険種別
           MOVE    LF02-HKNSBT
                                       TO  CLACRES-HKNSBT
      * 健康保険種別コード
           MOVE    LF02-HKNSBTCODE
                                       TO  CLACRES-HKNSBTCODE
      * 健康保険種別コードテーブル名
           MOVE    LF02-HKNSBTTBL
                                       TO  CLACRES-HKNSBTTBL
      *
      *診療行為区分コード (claimA:classCode)
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   30 )
                        OR    (LF02-SRYCD(IDX) =   SPACE)
               MOVE    LF02-SRYCD (IDX)
                                       TO  CLACRES-SRYCD (IDX)
      * 診療行為区分コードのテーブル名 (claimA:classCodeId)
               MOVE    LF02-SRYCDTBL (IDX)
                                       TO  CLACRES-SRYCDTBL (IDX)
      *診療行為区分名
               MOVE    LF02-SRYNAME (IDX)
                                       TO  CLACRES-SRYNAME (IDX)
      *合計請求点数(Z編集)
               MOVE    LF02-TENNSUALL (IDX)
                                       TO  CLACRES-TENNSUALL (IDX)
      *合計請求金額(Z編集)
               MOVE    LF02-GINGAKUALL (IDX)
                                       TO  CLACRES-GINGAKUALL (IDX)
      *用法
               MOVE    LF02-ADMINISTRATION (IDX)
                                       TO  CLACRES-ADMINISTRATION (IDX)
      *用法コード
               MOVE    LF02-ADMINCODE (IDX)
                                       TO  CLACRES-ADMINCODE (IDX)
      * 用法コードのテーブル名 (claimA:adminCodeId)
               MOVE    LF02-ADMINCODETBL (IDX)
                                       TO  CLACRES-ADMINCODETBL (IDX)
      *用法コメント
               MOVE    LF02-ADMMEMO (IDX)
                                       TO  CLACRES-ADMMEMO (IDX)
      *回数又は日数(Z編集)
               MOVE    LF02-KAISU (IDX)
                                       TO  CLACRES-KAISU (IDX)
      *手技料合計点数(Z編集)
               MOVE    LF02-SYUGI (IDX)
                                       TO  CLACRES-SYUGI (IDX)
      *材料料合計点数(Z編集)
               MOVE    LF02-ZAIRYOU (IDX)
                                       TO  CLACRES-ZAIRYOU (IDX)
      *薬剤料合計点数(Z編集)
               MOVE    LF02-YAKUZAI (IDX)
                                       TO  CLACRES-YAKUZAI (IDX)
      *包括区分包括フラグ(true OR false)
               MOVE    LF02-PPSFLG (IDX)
                                       TO  CLACRES-PPSFLG (IDX)
      *明細テーブル
      *H30.10
               MOVE    ZERO            TO  IDY2
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY     >   30 )
      ***                    OR   (LF02-SRYSYUBETSU(IDX IDY)
      ***                                          =   SPACE )
      *H30.10
                 IF      LF02-SRYSYUBETSU(IDX IDY) NOT =   SPACE
                   ADD     1                   TO  IDY2
      *
      *診療種別区分コード(0:手技、1:材料,2:薬剤)
                   MOVE    LF02-SRYSYUBETSU (IDX IDY)
                                       TO  CLACRES-SRYSYUBETSU
                                                       (IDX IDY2)
      * 診療種別区分コードのテーブル名 (claimA:subclassCodeId)
                   MOVE    LF02-SRYSYUBETSUTBL (IDX IDY)
                                       TO  CLACRES-SRYSYUBETSUTBL
                                                       (IDX IDY2)
      *医事請求コード(S + 診療行為コード)
                   MOVE    LF02-IJICD (IDX IDY)
                                       TO  CLACRES-IJICD
                                                       (IDX IDY2)
      *医事請求コード体系名
                   MOVE    LF02-TBLID (IDX IDY)
                                       TO  CLACRES-TBLID
                                                       (IDX IDY2)
      *医事請求コード
                   MOVE    LF02-ALSCODE (IDX IDY)
                                       TO  CLACRES-ALSCODE
                                                       (IDX IDY2)
      *医事請求コード体系名
                   MOVE    LF02-ALSTBLID (IDX IDY)
                                       TO  CLACRES-ALSTBLID
                                                       (IDX IDY2)
      *医事請求コード名称
                   MOVE    LF02-ALSNAME (IDX IDY)
                                       TO  CLACRES-ALSNAME
                                                       (IDX IDY2)
      *数量(Z編集)
                   MOVE    LF02-SURYO (IDX IDY)
                                       TO  CLACRES-SURYO
                                                       (IDX IDY2)
      *数量コード
                   MOVE    LF02-SURYOCD (IDX IDY)
                                       TO  CLACRES-SURYOCD
                                                       (IDX IDY2)
      * 数量コードのテーブル名 (claimA:numberCodeId)
                   MOVE    LF02-SURYOCDTBL (IDX IDY)
                                       TO  CLACRES-SURYOCDTBL
                                                       (IDX IDY2)
      *単位
                   MOVE    LF02-TANI (IDX IDY)
                                       TO  CLACRES-TANI
                                                       (IDX IDY2)
      *請求点数(Z編集)
                   MOVE    LF02-SEIKYUTEN (IDX IDY)
                                       TO  CLACRES-SEIKYUTEN
                                                       (IDX IDY2)
      *明細単価(Z編集)
                   MOVE    LF02-MEISAITANKA (IDX IDY)
                                       TO  CLACRES-MEISAITANKA
                                                       (IDX IDY2)
      *所要時間(PTnHnM,5時間23分→PT5H23M)
                   MOVE    LF02-DURATION (IDX IDY)
                                       TO  CLACRES-DURATION
                                                       (IDX IDY2)
      *部位
                   MOVE    LF02-LOCATION (IDX IDY)
                                       TO  CLACRES-LOCATION
                                                       (IDX IDY2)
      *フィルム
      *フィルムサイズ
                   MOVE    LF02-FILMSIZE (IDX IDY 1 )
                                       TO  CLACRES-FILMSIZE
                                                       (IDX IDY2)
      *フィルムサイズコード
                   MOVE    LF02-FILMSIZECD (IDX IDY 1 )
                                       TO  CLACRES-FILMSIZECD
                                                       (IDX IDY2)
      * フィルムサイズコードのテーブル名 (claimA:numberCodeId)
                   MOVE    LF02-FILMSIZECDTBL (IDX IDY 1 )
                                       TO  CLACRES-FILMSIZECDTBL
                                                       (IDX IDY2)
      *フィルム分割
                   MOVE    LF02-FILMDEV (IDX IDY 1 )
                                       TO  CLACRES-FILMDEV
                                                       (IDX IDY2)
      *フィルム枚数(Z編集)
                   MOVE    LF02-FILMNUM (IDX IDY 1 )
                                       TO  CLACRES-FILMNUM
                                                       (IDX IDY2)
      *関連手技
                   MOVE    LF02-EVENT (IDX IDY)
                                       TO  CLACRES-EVENT
                                                       (IDX IDY2)
      *開始日
                   MOVE    LF02-EVENTSTART (IDX IDY)
                                       TO  CLACRES-EVENTSTART
                                                       (IDX IDY2)
      *終了日
                   MOVE    LF02-EVENTEND (IDX IDY)
                                       TO  CLACRES-EVENTEND
                                                       (IDX IDY2)
      *メモ
                   MOVE    LF02-SRYMEMO (IDX IDY)
                                       TO  CLACRES-SRYMEMO
                                                       (IDX IDY2)
      *H30.10
                 END-IF
               END-PERFORM
      *
      *括りメモ
               MOVE    LF02-MEMO (IDX)     TO  CLACRES-MEMO (IDX)
           END-PERFORM
      *
           .
       20021-CLACRES-HEN-EXT.
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
               MOVE    "診療日が暦日ではありません"
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
           WHEN    "16"
               MOVE    "伝票番号が数値ではありません"
                                               TO  WRK-ERRMSG
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
