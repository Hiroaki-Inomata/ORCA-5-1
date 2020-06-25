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
       PROGRAM-ID.         ORAPI014R1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 予約一覧  (xml2）
      *  管理者            :
      *  作成日付    作業者        記述
      *  12/06/27    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.00.00     NACL-多々納  17/11/XX  予約ＩＤ追加対応
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
           03  FLG-PTINF               PIC 9(01).
           03  FLG-YYK                 PIC 9(01).
      *
           03  FLG-ERR                 PIC 9(01).
           03  FLG-SYORI               PIC 9(01).
           03  FLG-YYKOK               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDD                     PIC 9(04).
           03  IDK                     PIC 9(04).
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
       01  WRK-XML-AREA.
           03  WRK-APPOINT-YMD         PIC X(08).
           03  WRK-DRCD                PIC X(05).
           03  WRK-DRCD-NAME           PIC X(80).
           03  WRK-SRYNAIYO            PIC X(02).
           03  WRK-SRYNAIYO-NAME       PIC X(24).
      *
      *    システム管理テーブル
      *    診療科テーブル
       01  TBL-SRYKA-AREA.
           03  TBL-SRYKA-G             OCCURS   99.
               05  TBL-SRYKA           PIC X(02).
               05  TBL-SRYKA-NAME      PIC X(80).
      *    ドクターテーブル
       01  TBL-DRCD-AREA.
           03  TBL-DRCD-G             OCCURS   99.
               05  TBL-DRCD            PIC X(05).
               05  TBL-DRCD-NAME       PIC X(80).
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
           COPY    "CPORCSPTNUM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    日付変換サブ
           COPY     "CPORCSGDAY.INC".
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPAPPTLSTV2REQ.INC".
      *    API XML書き込み用定義体
           COPY    "CPAPPTLSTV2RES.INC".
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *   （診療科情報）
           COPY    "CPSK1005.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *    診療内容情報
           COPY  "CPSK1012.INC".
      *    予約内容
           COPY  "CPSK1028.INC".
      *    予約連絡
           COPY  "CPSK0051.INC".
      *    予約情報
           COPY  "CPSK1045.INC".
      *    患者マスタ−
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    予約マスター
       01  YYK-REC.
           COPY    "CPYYK.INC".
      *
      *
           COPY    "MCPDATA.INC".
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
           COPY    "API01RV2SCRAREA.INC".
      *    COPY    "CPAPIPTAV2.INC".
      *    COPY    "CPAPILSTV2.INC".
      *    COPY    "CPAPILSTV2.INC"    REPLACING
      *                              //APILST-// BY //APILST1-//.
      *    COPY    "CPAPILSTV2.INC"    REPLACING
      *                              //APILST-// BY //APILST2-//.
      *    COPY    "CPAPILSTV2.INC"  REPLACING
      *                              //APILST-// BY //APILST3-//.
      *    COPY    "CPAPILSTV2.INC"    REPLACING
      *                              //APILST-// BY //APILST4-//.
      *    COPY    "CPAPILSTV2.INC"    REPLACING
      *                              //APILST-// BY //APILST5-//.
      *    COPY    "CPAPILSTV2.INC"    REPLACING
      *                              //APILST-// BY //APILST6-//.
      *    COPY    "CPAPILSTV2.INC"    REPLACING
      *                              //APILST-// BY //APILST7-//.
      *    COPY    "CPAPILSTV2.INC"    REPLACING
      *                                //APILST-// BY //APILST8-//.
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
           DISPLAY   "* appointlst start*"
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
               MOVE   404                  TO  APILST1-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-APTLSTMAKE-SEC
           END-IF
      *
           DISPLAY   "*******************"
           DISPLAY   "* appointlst end  *"
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
           INITIALIZE                      XML-APPOINTLSTRES
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Patient Info"       TO  APLSTRES-RESKEY
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  APLSTRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  APLSTRES-INFORMATION-TIME
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
           PERFORM 1000-APTLSTREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           MOVE    ZERO                TO  APLSTRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    APILST1-CLASS
           WHEN    "01"
      *        予約
               MOVE    1                   TO  FLG-SYORI
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
           EVALUATE    APILST1-CLASS
           WHEN    "01"
      *        予約一覧処理
               PERFORM 2002-YYK-LIST-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-APTLSTREAD-SEC   SECTION.
      *
           IF      APILST1-BODY    NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE   "XMLOPEN"            TO  MCP-FUNC
           MOVE    "xml_appointlstv2req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST1-BODY        TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "appointlstreq"     TO  APIREQ-RECORDNAME
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
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_appointlstv2req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST1-BODY        TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "appointlstreq"     TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ          TO  XML-APPOINTLSTREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           .
       1000-APTLSTREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-APTLSTMAKE-SEC   SECTION.
      *
      *    エラーメッセージ編集
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-KEICD           TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "処理終了"
                                           TO  APLSTRES-API-RESULT-MSG
               END-EVALUATE
           ELSE
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD           TO  APLSTRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  APLSTRES-API-RESULT-MSG
           END-IF
      *
           IF      APILST1-BODY    NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_appointlstv2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  APLSTRES-MODE
           MOVE    "appointlstres"     TO  APLSTRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APPOINTLSTRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_appointlstv2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  APLSTRES-MODE
           MOVE    "appointlstres"     TO  APLSTRES-RECORDNAME
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       XML-APPOINTLSTRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               DISPLAY "XMLWRITE OK      = " MCP-RC
               MOVE    APLSTRES-OBJECT     TO  APILST1-BODY 
               MOVE    "application/xml"   TO  APILST1-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
          .
       300-APTLSTMAKE-EXT.
           EXIT.
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
           IF  APLSTREQ-APPOINT-DATE    =   LOW-VALUE
               MOVE  SPACE             TO  APLSTREQ-APPOINT-DATE
           END-IF
           IF  APLSTREQ-APPOINT-DEP-INFO   =   LOW-VALUE
               MOVE  SPACE             TO  APLSTREQ-APPOINT-DEP-INFO
           END-IF
           IF  APLSTREQ-APPOINT-DR-CODE =   LOW-VALUE
               MOVE  SPACE             TO  APLSTREQ-APPOINT-DR-CODE
           END-IF
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
      *    予約日チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-YYKYMD-CHK-SEC
           END-IF
      *
      *    ドクターコードチェック
           IF     (WRK-DRCD        NOT =   SPACE  )
           AND    (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-DRCD-CHK-SEC
           END-IF
      *    診療内容チェック
           IF     (WRK-SRYNAIYO    NOT =   SPACE  )
           AND    (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-SRYNAIYO-CHK-SEC
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
      *    ドクター
           MOVE    APLSTREQ-APPOINT-DR-CODE
                                           TO  WRK-DRCD
      *    診療内容区分
           MOVE    APLSTREQ-APPOINT-DEP-INFO
                                           TO  WRK-SRYNAIYO
      *    予約日
           MOVE    APLSTREQ-APPOINT-DATE   TO  APLSTRES-APPOINT-DATE
      *    数字のみに編集
           MOVE    APLSTREQ-APPOINT-DATE   TO  WRK-HEN-DATE
           MOVE    SPACE                   TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-APPOINT-YMD
      *    予約日未設定はシステム日付
           IF      WRK-APPOINT-YMD     =   SPACE
               MOVE    SPA-SYSYMD          TO  WRK-APPOINT-YMD
               MOVE    WRK-APPOINT-YMD     TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  APLSTRES-APPOINT-DATE
           END-IF
      *
           .
       20010-INPUT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約日チェック
      *****************************************************************
       20011-YYKYMD-CHK-SEC    SECTION.
      *
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-APPOINT-YMD     TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "11"                TO  WRK-ERRCD
               DISPLAY "Accept Ymd =  " WRK-APPOINT-YMD
           END-IF
      *
           .
       20011-YYKYMD-CHK-EXT.
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
           MOVE    WRK-APPOINT-YMD     TO  SYS-1010-STYUKYMD
           MOVE    WRK-APPOINT-YMD     TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
               MOVE    SYS-1010-NAME       TO  WRK-DRCD-NAME
           ELSE
               MOVE    "12"                TO  WRK-ERRCD
               MOVE    WRK-DRCD            TO  APLSTRES-APPOINT-DR-CODE
                                                                  (1)
           END-IF
      *
           .
       20011-DRCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療内容チェック
      *****************************************************************
       20011-SRYNAIYO-CHK-SEC      SECTION.
      *
      *    診療内容情報の存在チェック
           MOVE    SPACE               TO  SYS-1012-REC
           INITIALIZE                  SYS-1012-REC
           MOVE    "1012"              TO  SYS-1012-KANRICD
           MOVE    WRK-SRYNAIYO        TO  SYS-1012-KBNCD
           MOVE    WRK-APPOINT-YMD     TO  SYS-1012-STYUKYMD
           MOVE    WRK-APPOINT-YMD     TO  SYS-1012-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1012-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1012-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC            TO  SYS-1012-REC
               MOVE    SYS-1012-SRYNAIYODSP    TO  WRK-SRYNAIYO-NAME
           ELSE
               MOVE    "13"                TO  WRK-ERRCD
               MOVE    WRK-SRYNAIYO        TO  APLSTRES-APPOINT-DEP-INFO
                                                                  (1)
           END-IF
           .
       20011-SRYNAIYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約一覧編集処理
      *****************************************************************
       2002-YYK-LIST-SEC       SECTION.
      *
      *    システム管理テーブル編集
           PERFORM 2009-SYSKANRITBL-SEC
      *
      *    予約一覧編集
           MOVE    SPACE               TO  YYK-REC
           INITIALIZE                  YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    WRK-APPOINT-YMD     TO  YYK-YYKYMD
           MOVE    WRK-SRYNAIYO        TO  YYK-SRYNAIYO
           MOVE    WRK-DRCD            TO  YYK-DRCD
           IF     (WRK-SRYNAIYO        =   SPACE )  OR
                  (WRK-DRCD            =   SPACE )
               MOVE    "key5"              TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "key3"              TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-YYK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-YYK
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM
                   UNTIL      (FLG-YYK     =   1   )
                         OR   (IDX         >   500 )
               MOVE    ZERO                TO  FLG-YYKOK
               IF     (WRK-SRYNAIYO        =   SPACE )  OR
                      (WRK-DRCD            =   SPACE )
                   IF      WRK-SRYNAIYO    NOT =   SPACE
                       IF      WRK-SRYNAIYO    =   YYK-SRYNAIYO
                           MOVE    1           TO  FLG-YYKOK
                       END-IF
                   END-IF
                   IF      WRK-DRCD        NOT =   SPACE
                       IF      WRK-DRCD        =  YYK-DRCD
                           MOVE    1           TO  FLG-YYKOK
                       END-IF
                   END-IF
                   IF     (WRK-SRYNAIYO        =   SPACE )  AND
                          (WRK-DRCD            =   SPACE )
                       MOVE    1               TO  FLG-YYKOK
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-YYKOK
               END-IF
               IF      FLG-YYKOK           =   1
                   IF     (YYK-YYKID       >   ZERO  )
                       ADD     1               TO  IDX
                       IF      IDX             >   500
                           MOVE    "20"        TO  WRK-ERRCD
                       ELSE
                           PERFORM 20021-YYKLST-HEN-SEC
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-YYK-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX                 =   ZERO
               MOVE    "21"            TO  WRK-ERRCD
      *        １件目に入力値を編集する
               IF      WRK-DRCD        NOT =   SPACE
                   MOVE    WRK-DRCD        TO  APLSTRES-APPOINT-DR-CODE
                                                                  (1)
                   MOVE    WRK-DRCD-NAME   TO  APLSTRES-APPOINT-DR-NAME
                                                                  (1)
               END-IF
               IF      WRK-SRYNAIYO    NOT =   SPACE
                   MOVE    WRK-SRYNAIYO    TO  APLSTRES-APPOINT-DEP-INFO
                                                                  (1)
               END-IF
           END-IF
           .
       2002-YYK-LIST-EXT.
           EXIT.
      *****************************************************************
      *    予約情報編集処理
      *****************************************************************
       20021-YYKLST-HEN-SEC        SECTION.
      *
      *    予約時間
           MOVE    YYK-YYKYMD          TO  WRK-SYMD
           MOVE    YYK-YYKTIME         TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-TIME        TO  APLSTRES-APPOINT-TIME (IDX)
      *    診療内容
           MOVE    YYK-SRYNAIYO        TO  APLSTRES-APPOINT-DEP-INFO
                                                                 (IDX)
      *    ドクター
           MOVE    YYK-DRCD            TO  APLSTRES-APPOINT-DR-CODE
                                                                 (IDX)
           PERFORM VARYING    IDD      FROM    1   BY  1
                   UNTIL     (IDD      >   99  )
                         OR  (TBL-DRCD (IDD)   =   SPACE )
               IF      TBL-DRCD (IDD)      =   YYK-DRCD
                   MOVE    TBL-DRCD-NAME (IDD)
                                       TO  APLSTRES-APPOINT-DR-NAME
                                                                 (IDX)
                   MOVE    99          TO  IDD
               END-IF
           END-PERFORM
      *    診療科
           MOVE    YYK-SRYKA           TO  APLSTRES-APPOINT-DEP-CODE
                                                                 (IDX)
           PERFORM VARYING    IDK      FROM    1   BY  1
                   UNTIL     (IDK      >   99  )
                         OR  (TBL-SRYKA (IDK)   =   SPACE )
               IF      TBL-SRYKA (IDK)     =   YYK-SRYKA
                   MOVE    TBL-SRYKA-NAME (IDK)
                                       TO  APLSTRES-APPOINT-DEP-NAME
                                                                 (IDX)
                   MOVE    99          TO  IDK
               END-IF
           END-PERFORM
      *    予約ＩＤ編集
           MOVE    YYK-YYKID           TO  APLSTRES-APPOINT-ID(IDX)
      *    来院済み
           MOVE    YYK-RAIINZUMI       TO  APLSTRES-VISIT-INFO (IDX)
      *
           IF      YYK-PTID            =   ZERO
      *        新規患者
               MOVE    YYK-NAME            TO  APLSTRES-NAME  (IDX)
           ELSE
      *        患者情報検索
               PERFORM 200212-PTINF-HEN-SEC
           END-IF
      *
           .
       20021-YYKLST-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者情報内容編集処理
      *****************************************************************
       200212-PTINF-HEN-SEC        SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    YYK-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    YYK-NAME            TO  PTINF-NAME
           END-IF
      *
           MOVE    PTINF-NAME          TO  APLSTRES-NAME     (IDX)
           MOVE    PTINF-KANANAME      TO  APLSTRES-KANANAME (IDX)
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  APLSTRES-BIRTHDAY (IDX)
           MOVE    PTINF-SEX           TO  APLSTRES-SEX      (IDX)
           MOVE    PTINF-HOME-TEL1     TO  APLSTRES-HOME-TEL (IDX)
      *    患者番号
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    YYK-PTID            TO  SPTID-PTID
           CALL    "ORCSPTID"          USING  ORCSPTIDAREA
                                              SPA-AREA
           IF      SPTID-RC            =   ZERO
               MOVE    SPTID-PTNUM     TO  APLSTRES-PATIENTID (IDX)
           END-IF
      *
           .
       200212-PTINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目変換編集処理
      *****************************************************************
       2009-SYSKANRITBL-SEC     SECTION.
      *
           INITIALIZE                  TBL-DRCD-AREA
           INITIALIZE                  TBL-SRYKA-AREA
      *
      *    ドクターテーブル
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    WRK-APPOINT-YMD     TO  SYS-1010-STYUKYMD
           MOVE    WRK-APPOINT-YMD     TO  SYS-1010-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDD
           PERFORM UNTIL   (FLG-SYSKANRI   =   1   )
                       OR  (IDD            >=  99  )
               ADD     1                   TO  IDD
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               MOVE    SYS-1010-KBNCD      TO  TBL-DRCD      (IDD)
               MOVE    SYS-1010-NAME       TO  TBL-DRCD-NAME (IDD)
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    診療科テーブル
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    WRK-APPOINT-YMD     TO  SYS-1005-STYUKYMD
           MOVE    WRK-APPOINT-YMD     TO  SYS-1005-EDYUKYMD
      *
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDK
           PERFORM UNTIL   (FLG-SYSKANRI   =   1   )
                       OR  (IDK           >=  99  )
               ADD     1                   TO  IDK
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
               MOVE    SYS-1005-KBNCD(1:2) TO  TBL-SRYKA     (IDK)
               MOVE    SYS-1005-SRYKANAME  TO  TBL-SRYKA-NAME(IDK)
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2009-SYSKANRITBL-EXT.
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
           IF      WRK-TSS             =   SPACE
               MOVE    ZERO                TO  WRK-TSS
           END-IF
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
           WHEN    "11"
               MOVE    "予約日が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "12"
               MOVE    "ドクターが存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE    "診療内容情報が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "20"
               MOVE    "予約対象件数が５００件以上あります。"
                                               TO  WRK-ERRMSG
           WHEN    "21"
               MOVE    "対象の予約はありませんでした。"
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
      *    予約マスター読込処理
      *****************************************************************
       900-YYK-READ-SEC            SECTION.
      *
           MOVE    "tbl_yyk"           TO  MCP-TABLE
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-YYK
               MOVE    MCPDATA-REC         TO  YYK-REC
           ELSE
               MOVE    1                   TO  FLG-YYK
               INITIALIZE                      YYK-REC
           END-IF
      *
           .
       900-YYK-READ-EXT.
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
