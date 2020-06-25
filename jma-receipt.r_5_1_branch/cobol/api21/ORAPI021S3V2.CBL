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
       PROGRAM-ID.         ORAPI021S3V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 初診算定日登録処理
      *  管理者            :
      *  作成日付    作業者        記述
      *  18/10/XX    NACL−多々納　新規作成
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
           03  FLG-SANTEI              PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-SRYKARRK            PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
           03  FLG-SPATMP              PIC 9(01).
      *
           03  FLG-SRY-OK              PIC 9(01).
           03  FLG-HAORI-UP            PIC 9(01).
           03  FLG-SYORI               PIC 9(01).
      ***  03  FLG-ERR                 PIC 9(01).
           03  FLG-UPDOK               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDX2                PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SANTEI              PIC 9(04).
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
           03  WRK-KEICD               PIC X(02).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).
           03  WRK-MCP-WINDOW          PIC X(64).
      *    受診履歴最初の日
           03  WRK-F-RRKYMD            PIC X(08).
           03  WRK-RRK-LSTYMD          PIC X(08).
           03  WRK-F-SRYKA             PIC X(02).
           03  WRK-RRK-SRYKA           PIC X(02).
      *
           03  WRK-SYOSIN-YMD.
               05  WRK-SYOSIN-YM       PIC X(06).
               05  WRK-SYOSIN-DD       PIC 9(02).
      *
       01  WRK-XML-AREA.
           03  WRK-PTID                PIC 9(10).
           03  WRK-PTNUM               PIC X(20).
      *
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-NAME          PIC X(80).
           03  WRK-FIRST-DATE          PIC X(10).
           03  WRK-FIRST-YMD           PIC X(08).
      *
           03  WRK-LAST-DATE           PIC X(10).
           03  WRK-LAST-YMD            PIC X(08).
      *
      *    算定履歴保存
       01  HZN-SANTEI-REC.
           COPY    "CPSANTEI.INC"      REPLACING
                                       //SANTEI-// BY //HZN-SANTEI-//.
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
      **   COPY    "CPORCSKANACHK.INC".
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    診療科履歴更新サブ
           COPY    "CPORCSKARRK.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPMDCXMLV2REQ3.INC".
      *    API XML書き込み用定義体
           COPY    "CPMDCXMLV2RES3.INC".
      *
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
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
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療行為マスター
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療科履歴
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    APIパラメタテーブル
      *01  APIPARA-REC.
      *    COPY    "CPAPIPARA.INC".
      *
      *    ＳＰＡ一時テーブル
      *01  SPATMP-REC.
      *    COPY    "CPSPA-TMP.INC".
      *
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      **   COPY    "MCPDATA2.INC".
      *
           COPY    "ORCA-BLOB".
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
      *
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
           DISPLAY   "**********************"
           DISPLAY   "* medicalreq3v2 start*"
           DISPLAY   "**********************"
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
               MOVE   404                  TO  APILST16-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-APTXMLMAKE-SEC
           END-IF
      *
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
      *
           DISPLAY   "*********************"
           DISPLAY   "* medicalreq3v2 end *"
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
           INITIALIZE                      XML-MEDICALV2RES3
           INITIALIZE                      XML-MEDICALV2REQ3
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Medical Info"       TO  MDCRES-RESKEY
      *
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
           EVALUATE    MDCREQ-REQUEST-NUMBER
           WHEN    "00"
      *        初診算定日検索
               MOVE    1                   TO  FLG-SYORI
           WHEN    "01"
      *        初診算定日登録
               MOVE    2                   TO  FLG-SYORI
           WHEN    "02"
      *        初診算定日削除
               MOVE    3                   TO  FLG-SYORI
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
           END-EVALUATE
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
      *        患者情報内容編集
               PERFORM 20021-MDCRES-HEN-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
      *    初診算定日登録
           EVALUATE    FLG-SYORI
           WHEN    1
               PERFORM 2002-SYORI-01-SEC
           WHEN    2
               PERFORM 2003-SYORI-02-SEC
           WHEN    3
               PERFORM 2004-SYORI-03-SEC
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
           IF      APILST16-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalv2req3"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST16-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalv2req3"     TO  APIREQ-RECORDNAME
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
           MOVE    "xml_medicalv2req3"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST16-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalv2req3"     TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ          TO  XML-MEDICALV2REQ3
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE           TO  APILST16-BODY
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
               MOVE    "W"                 TO  WRK-ERRKBN
           END-IF
           IF      WRK-ERRCD           =   SPACE
               MOVE    ZERO                TO  MDCRES-API-RESULT
               EVALUATE    FLG-SYORI
               WHEN    1
                   MOVE    "検索処理終了"
                                           TO  MDCRES-API-RESULT-MSG
               WHEN    2
                   MOVE    "登録処理終了"
                                           TO  MDCRES-API-RESULT-MSG
               WHEN    3
                   MOVE    "削除処理終了"
                                           TO  MDCRES-API-RESULT-MSG
               END-EVALUATE
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
           IF      APILST16-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalv2res3"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalv2res3"     TO  MDCRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALV2RES3
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_medicalv2res3"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalv2res3"     TO  MDCRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALV2RES3
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    MDCRES-OBJECT      TO  APILST16-BODY 
               MOVE    "application/xml"   TO  APILST16-CONTENT-TYPE
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
      *    患者番号チェック
           IF     (WRK-ERRCD           =   SPACE )
               PERFORM 20011-PTNUM-CHK-SEC
           END-IF
      *    診療科コードチェック
           IF     (WRK-ERRCD           =   SPACE  )
              AND (WRK-SRYKA       NOT =   SPACE  )
               PERFORM 20011-SRYKA-CHK-SEC
           END-IF
      *    初診算定日チェック
           IF     (WRK-ERRCD           =   SPACE  )
              AND (WRK-FIRST-DATE  NOT =   SPACE  )
               PERFORM 20011-FIRST-DATE-CHK-SEC
           END-IF
      *    最終来院日チェック
           IF     (WRK-ERRCD           =   SPACE  )
              AND (WRK-LAST-DATE   NOT =   SPACE  )
               PERFORM 20011-LAST-DATE-CHK-SEC
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
           MOVE    MDCREQ-PATIENTID    TO  WRK-PTNUM
      *    診療科
           MOVE    MDCREQ-DEP-CODE     TO  WRK-SRYKA
      *    初診算定日
           MOVE    MDCREQ-FIRST-DATE   TO  WRK-FIRST-DATE
           MOVE    MDCREQ-FIRST-DATE   TO  WRK-HEN-DATE
           MOVE    SPACE               TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD            TO  WRK-FIRST-YMD
      *
      *    最終来院日
           IF      MDCREQ-LAST-DATE    NOT =   SPACE
               MOVE    MDCREQ-LAST-DATE    TO  WRK-LAST-DATE
               MOVE    MDCREQ-LAST-DATE    TO  WRK-HEN-DATE
               MOVE    SPACE               TO  WRK-HEN-TIME
               PERFORM 802-DAYHEN02-SEC
               MOVE    WRK-SYMD            TO  WRK-LAST-YMD
           END-IF
           .
       20010-INPUT-HEN-EXT.
           EXIT.
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
               MOVE    "12"                TO  WRK-ERRCD
           END-IF
      *
           .
       20011-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日付チェック
      *****************************************************************
       20011-FIRST-DATE-CHK-SEC    SECTION.
      *
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-FIRST-YMD       TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "13"                TO  WRK-ERRCD
           END-IF
           MOVE    WRK-FIRST-YMD       TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  WRK-FIRST-DATE
      *
           .
       20011-FIRST-DATE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日付チェック
      *****************************************************************
       20011-LAST-DATE-CHK-SEC    SECTION.
      *
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-LAST-YMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "14"                TO  WRK-ERRCD
           END-IF
           MOVE    WRK-LAST-YMD        TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  WRK-LAST-DATE
      *
           .
       20011-LAST-DATE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    取得情報編集処理
      *****************************************************************
       20021-MDCRES-HEN-SEC     SECTION.
      *
           MOVE    WRK-PTNUM           TO  MDCRES-PATIENTID
      *
           MOVE    WRK-SRYKA           TO  MDCRES-DEP-CODE
           MOVE    WRK-SRYKA-NAME      TO  MDCRES-DEP-NAME
      *
           IF      SPA-PTID        NOT =   ZERO
               PERFORM 200211-PTINF-HEN-SEC
           END-IF
      *
           MOVE    WRK-FIRST-DATE      TO  MDCRES-FIRST-DATE
      *
           MOVE    WRK-LAST-DATE       TO  MDCRES-LAST-DATE
           .
       20021-MDCRES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者情報出力情報編集処理
      *****************************************************************
       200211-PTINF-HEN-SEC     SECTION.
      *
      *    患者情報
           MOVE    WRK-PTNUM           TO  MDCRES-PATIENTID
      *
           MOVE    PTINF-NAME          TO  MDCRES-NAME
           MOVE    PTINF-KANANAME      TO  MDCRES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MDCRES-BIRTHDAY
           MOVE    PTINF-SEX           TO  MDCRES-SEX
           .
       200211-PTINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診算定日検索処理
      *****************************************************************
       2002-SYORI-01-SEC           SECTION.
      *
      *    患者情報内容編集
           PERFORM 20021-MDCRES-HEN-SEC
      *
      *    受診履歴など情報取得
           PERFORM 20022-SANTEI-HENSYU-SEC
      *
           MOVE    SPACE               TO  MDCRES-FIRST-DATE
           MOVE    SPACE               TO  MDCRES-FIRST-VISIT-DATE
           MOVE    SPACE               TO  MDCRES-LAST-DATE
      *    初診算定日
           IF      WRK-SYOSIN-YMD  NOT =   SPACE
               MOVE    WRK-SYOSIN-YMD      TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  MDCRES-FIRST-DATE
           END-IF
      *    受診履歴最初の日
           IF      WRK-F-RRKYMD    NOT =   SPACE
               MOVE    WRK-F-RRKYMD        TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  MDCRES-FIRST-VISIT-DATE
           END-IF
      *    最終来院日
           IF      WRK-RRK-LSTYMD  NOT =   SPACE
               MOVE    WRK-RRK-LSTYMD      TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  MDCRES-LAST-DATE
           END-IF
      *
      *    初診算定日算定済み
           IF     (WRK-SYOSIN-YMD  NOT =   SPACE )
               MOVE    "K1"            TO  WRK-KEICD
           END-IF
           .
       2002-SYORI-01-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴など情報取得処理
      *****************************************************************
       20022-SANTEI-HENSYU-SEC     SECTION.
      *
           MOVE    SPACE               TO  WRK-F-RRKYMD
           MOVE    SPACE               TO  WRK-SYOSIN-YMD
           MOVE    SPACE               TO  WRK-RRK-LSTYMD
           MOVE    SPACE               TO  WRK-F-SRYKA
           MOVE    SPACE               TO  WRK-RRK-SRYKA
      *    受診履歴の１件目
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key46"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key46"             TO  MCP-PATHNAME
               PERFORM 910-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           IF      FLG-JYURRK          =   ZERO
               MOVE    JYURRK-SRYYMD       TO  WRK-F-RRKYMD
               MOVE    JYURRK-SRYKA        TO  WRK-F-SRYKA
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key46"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    初診算定チェック
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    "999999"            TO  SANTEI-SRYYM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key11"             TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPACE               TO  WRK-SYOSIN-YMD
           IF      FLG-SANTEI          =   ZERO
      *        最終初診日
               MOVE    SANTEI-SRYYM        TO  WRK-SYOSIN-YM
               MOVE    ZERO                TO  WRK-SYOSIN-DD
               PERFORM VARYING     IDX2    FROM    31  BY  -1
                       UNTIL      (IDX2    <   1    )
                   IF      SANTEI-DAY (IDX2)   >   ZERO
                       MOVE    IDX2            TO  WRK-SYOSIN-DD
                       MOVE    1               TO  IDX2
                   END-IF
               END-PERFORM
           END-IF
      *
      *    最終来院日
           MOVE    SPACE               TO  SRYKARRK-REC
           INITIALIZE                      SRYKARRK-REC
           MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
           MOVE    SPA-PTID            TO  KARRK-PTID
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 975-SRYKARRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
           END-IF
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SRYKARRK        =   ZERO
               MOVE    KARRK-LASTYMD       TO  WRK-RRK-LSTYMD
               MOVE    KARRK-SRYKA         TO  WRK-RRK-SRYKA
           END-IF
           .
       20022-SANTEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診算定日登録処理
      *****************************************************************
       2003-SYORI-02-SEC           SECTION.
      *
      *    患者情報内容編集
           PERFORM 20021-MDCRES-HEN-SEC
      *
      *    排他チェック & ロック処理
           PERFORM 990-LOCK-IN-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "90"            TO  WRK-ERRCD
               GO      TO      2003-SYORI-02-EXT
           END-IF
      *
      *    受診履歴など情報取得
           PERFORM 20022-SANTEI-HENSYU-SEC
      *
      *    初診算定日チェック
           IF      WRK-FIRST-YMD       =   SPACE
               MOVE    "15"            TO  WRK-ERRCD
           ELSE
               IF      WRK-FIRST-YMD   >   SPA-SYSYMD
                   MOVE    "20"            TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    診療科
      *    最初の受診履歴診療科
           IF      WRK-F-SRYKA     NOT =   SPACE
               MOVE    WRK-F-SRYKA     TO  WRK-SRYKA
           END-IF
           IF      WRK-SRYKA           =   SPACE
      *        診療科履歴の診療科
               IF      WRK-RRK-SRYKA   NOT =   SPACE
                   MOVE    WRK-RRK-SRYKA   TO  WRK-SRYKA
               ELSE
                   IF      WRK-ERRCD       =   SPACE
                       MOVE    "16"            TO  WRK-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *    初診算定日算定済み
           IF     (WRK-SYOSIN-YMD  NOT =   SPACE )
              AND (WRK-ERRCD           =   SPACE )
               MOVE    "17"            TO  WRK-ERRCD
           END-IF
      *    初回来院日＜初診算定日エラー
           IF     (WRK-SYOSIN-YMD      =   SPACE )
              AND (WRK-ERRCD           =   SPACE )
               IF     (WRK-F-RRKYMD    NOT =   SPACE )
                 AND  (WRK-FIRST-YMD       >   WRK-F-RRKYMD)
                  MOVE    "18"            TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    受診履歴がなく、最終受診日が設定
           IF     (WRK-F-RRKYMD        =   SPACE )
              AND (WRK-LAST-YMD    NOT =   SPACE )
              AND (WRK-ERRCD           =   SPACE )
               IF      WRK-FIRST-YMD       >   WRK-LAST-YMD
                  MOVE    "19"             TO  WRK-ERRCD
               END-IF
      *
               IF     (WRK-LAST-YMD        >   SPA-SYSYMD)
                  AND (WRK-ERRCD           =   SPACE )
                  MOVE    "21"             TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    更新処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20023-KOUSIN-SYORI-SEC
           END-IF
      *
           PERFORM 990-LOCK-OUT-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               DISPLAY "ロック解除失敗"
           END-IF
      *
           .
       2003-SYORI-02-EXT.
           EXIT.
      *****************************************************************
      *    初診算定日更新処理
      *****************************************************************
       20023-KOUSIN-SYORI-SEC     SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                      SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-FIRST-YMD(1:6)
                                       TO  SANTEI-SRYYM
      *    初診料ダミーコード登録
           MOVE    "099110001"         TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    WRK-FIRST-YMD       TO  SANTEI-FSANTEIYMD
           MOVE    WRK-FIRST-YMD(7:2)
                                       TO  SANTEI-MSANTEID
           MOVE    1                   TO  SANTEI-MSANTEICNT
           MOVE    SANTEI-MSANTEID     TO  IDX2
           MOVE    1                   TO  SANTEI-DAY (IDX2)
      *
           MOVE    SMCNDATE-YMD        TO  SANTEI-CREYMD
           MOVE    SMCNDATE-YMD        TO  SANTEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SANTEI-UPHMS
           MOVE    SPA-OPID            TO  SANTEI-OPID
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "22"                TO  WRK-ERRCD
           END-IF
      *
      *    診療科履歴の更新
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200231-ORCSKARRK-SEC
           END-IF
      *    最終来院日確認
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200232-LASTYMD-SET-SEC
           END-IF
      *
           .
       20023-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴の初診算定日修正処理
      *****************************************************************
       200231-ORCSKARRK-SEC             SECTION.
      *
      *    受診履歴がなく、最終受診日が未設定
           IF     (WRK-F-RRKYMD        =   SPACE )
              AND (WRK-LAST-YMD        =   SPACE )
               MOVE    WRK-FIRST-YMD       TO  WRK-LAST-YMD
           END-IF
      *
           INITIALIZE                  ORCSKARRKAREA
           MOVE    WRK-FIRST-YMD(1:6)  TO  SKARRK-SRYYM
           MOVE    WRK-SRYKA           TO  SKARRK-SRYKA
      *    最終受診日（退院日）
           IF     (WRK-RRK-LSTYMD      =   SPACE )
             AND  (WRK-LAST-YMD    NOT =   SPACE )
               MOVE    WRK-LAST-YMD    TO  SKARRK-LASTYMD
           END-IF
      *
           CALL    "ORCSKARRK"         USING
                                       ORCSKARRKAREA
                                       SPA-AREA
      *
           IF      SKARRK-ERRMSG   NOT =   SPACE
               MOVE    "23"                TO  WRK-ERRCD
           END-IF
      *
           .
       200231-ORCSKARRK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴の初診算定日修正処理
      *****************************************************************
       200232-LASTYMD-SET-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-RRK-LSTYMD
           MOVE    SPACE               TO  WRK-SYOSIN-YMD
      *
      *    診療科歴を検索
           MOVE    SPACE               TO  SRYKARRK-REC
           INITIALIZE                  SRYKARRK-REC
           MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
           MOVE    SPA-PTID            TO  KARRK-PTID
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 975-SRYKARRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
               MOVE    SPACE               TO  SRYKARRK-REC
               INITIALIZE                  SRYKARRK-REC
           END-IF
           PERFORM UNTIL       FLG-SRYKARRK    =   1
      *
               IF      KARRK-SRYKA         =   SPACE
                   CONTINUE
               ELSE
      *        初診算定日
               IF     (KARRK-SYOSINYMD2 NOT =  SPACE      ) AND
                      (KARRK-SYOSINYMD2    >=  WRK-SYOSIN-YMD)
                   MOVE    KARRK-SYOSINYMD2    TO  WRK-SYOSIN-YMD
               END-IF
      *        最終受診日
               IF     (KARRK-LASTYMD   NOT =   SPACE      ) AND
                      (KARRK-LASTYMD       >=  WRK-RRK-LSTYMD )
                   MOVE    KARRK-LASTYMD       TO  WRK-RRK-LSTYMD
                   MOVE    KARRK-SRYKA         TO  WRK-RRK-SRYKA
               END-IF
      *
               END-IF
      *
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 975-SRYKARRK-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    初診算定日
           MOVE    SPACE               TO  MDCRES-FIRST-DATE
           IF      WRK-SYOSIN-YMD  NOT =   SPACE
               MOVE    WRK-SYOSIN-YMD      TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  MDCRES-FIRST-DATE
           END-IF
      *    最終来院日
           MOVE    SPACE               TO  MDCRES-LAST-DATE
           IF      WRK-RRK-LSTYMD  NOT =   SPACE
               MOVE    WRK-RRK-LSTYMD      TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  MDCRES-LAST-DATE
           END-IF
           .
       200232-LASTYMD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診算定日削除処理
      *****************************************************************
       2004-SYORI-03-SEC           SECTION.
      *
      *    患者情報内容編集
           PERFORM 20021-MDCRES-HEN-SEC
      *
           MOVE    SPACE               TO  MDCRES-FIRST-DATE
           MOVE    SPACE               TO  MDCRES-LAST-DATE
      *
      *    排他チェック & ロック処理
           PERFORM 990-LOCK-IN-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "90"            TO  WRK-ERRCD
               GO      TO      2004-SYORI-03-EXT
           END-IF
      *
      *    受診履歴など情報取得
           PERFORM 20042-DELETE-CHECK-SEC
      *
      *    更新処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20043-DELETE-SYORI-SEC
           END-IF
      *
           PERFORM 990-LOCK-OUT-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               DISPLAY "ロック解除失敗"
           END-IF
      *
           .
       2004-SYORI-03-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴など情報取得処理
      *****************************************************************
       20042-DELETE-CHECK-SEC     SECTION.
      *
           MOVE    SPACE               TO  HZN-SANTEI-REC
      *    初診算定履歴が１件であること
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    "999999"            TO  SANTEI-SRYYM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key11"             TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           MOVE    ZERO                TO  CNT-SANTEI
           PERFORM UNTIL   FLG-SANTEI      =   1
      *
               IF      CNT-SANTEI          =   ZERO
                   MOVE    SANTEI-REC          TO  HZN-SANTEI-REC
               END-IF
               ADD     1                   TO  CNT-SANTEI
      *
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key11"             TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPACE               TO  WRK-SYOSIN-YMD
           IF      CNT-SANTEI          =   1
               MOVE    HZN-SANTEI-REC      TO  SANTEI-REC
      *        最終初診日
               MOVE    HZN-SANTEI-SRYYM    TO  WRK-SYOSIN-YM
               MOVE    ZERO                TO  WRK-SYOSIN-DD
               PERFORM VARYING     IDX2    FROM    31  BY  -1
                       UNTIL      (IDX2    <   1    )
                   IF      SANTEI-DAY (IDX2)   >   ZERO
                       IF      WRK-SYOSIN-DD   =   ZERO
                           MOVE    IDX2            TO  WRK-SYOSIN-DD
                       ELSE
                           MOVE    "31"            TO  WRK-ERRCD
                       END-IF
                   END-IF
               END-PERFORM
               IF      WRK-SYOSIN-DD           =   ZERO
                   MOVE    "32"                TO  WRK-ERRCD
               END-IF
           ELSE
      *        初診算定は回数エラー
               IF      CNT-SANTEI          =   ZERO
                   MOVE    "32"                TO  WRK-ERRCD
               ELSE
                   MOVE    "31"                TO  WRK-ERRCD
               END-IF
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      20042-DELETE-CHECK-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-F-RRKYMD
      *    受診履歴の１件目
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key46"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key46"             TO  MCP-PATHNAME
               PERFORM 910-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           IF      FLG-JYURRK          =   ZERO
               MOVE    JYURRK-SRYYMD       TO  WRK-F-RRKYMD
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key46"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    初回履歴より前であること
           IF      (WRK-F-RRKYMD   NOT =   SPACE )
              AND  (WRK-F-RRKYMD       <   WRK-SYOSIN-YMD)
               MOVE    "33"                TO  WRK-ERRCD
               GO      TO      20042-DELETE-CHECK-EXT
           END-IF
      *
      *    診療科履歴の診療科
           MOVE    SPACE               TO  SRYKARRK-REC
           INITIALIZE                      SRYKARRK-REC
           MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
           MOVE    SPA-PTID            TO  KARRK-PTID
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 975-SRYKARRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
           END-IF
           PERFORM UNTIL      (FLG-SRYKARRK    =   1  )
               MOVE    KARRK-SRYKA         TO  WRK-SRYKA
               IF      KARRK-SYOSINYMD2    =   WRK-SYOSIN-YMD
                   MOVE    1                   TO  FLG-SRYKARRK
               END-IF
               IF      FLG-SRYKARRK            =   ZERO
                   MOVE    "tbl_srykarrk"      TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 975-SRYKARRK-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    診療行為有無判定
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    SANTEI-SRYYM        TO  SRY-SRYYM
           MOVE    SANTEI-SRYCD        TO  SRY-SRYCD (1)
           MOVE    SANTEI-SRYCD        TO  SRY-SRYCD (2)
           MOVE    SANTEI-SRYCD        TO  SRY-SRYCD (3)
           MOVE    SANTEI-SRYCD        TO  SRY-SRYCD (4)
           MOVE    SANTEI-SRYCD        TO  SRY-SRYCD (5)
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key12"             TO  MCP-PATHNAME
               PERFORM 920-SRYACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           MOVE    ZERO                TO  FLG-SRY-OK
           PERFORM UNTIL      (FLG-SRYACT  =   1 )
                         OR   (FLG-SRY-OK  =   1 )
               PERFORM 200422-SRYACCT-CHK-SEC
               IF      FLG-SRY-OK          =   ZERO
                   MOVE    "tbl_sryact"        TO  MCP-TABLE
                   MOVE    "key12"             TO  MCP-PATHNAME
                   PERFORM 920-SRYACT-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    診療行為入力あり
           IF      FLG-SRY-OK          =   1
               MOVE    "33"            TO  WRK-ERRCD
           END-IF
           .
       20042-DELETE-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    診療会計検索処理
      *****************************************************************
       200422-SRYACCT-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SRY-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SRY-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SRY-PTID            TO  ACCT-PTID
           MOVE    SRY-SRYKA           TO  ACCT-SRYKA
           MOVE    SRY-SRYYM           TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SRYACCT         =   ZERO
               MOVE    1               TO  FLG-SRY-OK
           END-IF
           .
       310321-SRYACCT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       20043-DELETE-SYORI-SEC                SECTION.
      *
           MOVE    HZN-SANTEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SANTEI          =   ZERO
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               IF      MCP-RC              NOT =   ZERO 
                   MOVE    "34"                    TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    診療科履歴更新
           INITIALIZE                  ORCSKARRKAREA
           MOVE    SANTEI-SRYYM        TO  SKARRK-SRYYM
           MOVE    WRK-SRYKA           TO  SKARRK-SRYKA
           CALL    "ORCSKARRK"         USING
                                       ORCSKARRKAREA
                                       SPA-AREA
      *
           IF      SKARRK-ERRMSG   NOT =   SPACE
               MOVE    "35"                TO  WRK-ERRCD
           END-IF
      *
           .
       20043-DELETE-SYORI-EXT.
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
      *    WHEN    "11"
      *        MOVE    "診療日が暦日ではありません"
      *                                        TO  WRK-ERRMSG
           WHEN    "12"
               MOVE    "診療科が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE    "初診算定日が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "14"
               MOVE    "最終来院日が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "15"
               MOVE    "初診算定日が未設定です。"
                                               TO  WRK-ERRMSG
           WHEN    "16"
               MOVE    "診療科が未設定です。"
                                               TO  WRK-ERRMSG
           WHEN    "17"
               STRING  "初診料算定済みです。"
                       "初診算定日の登録はできません。"
                                               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "18"
               STRING  "初診算定日≦初回来院日ではありません。"
                                               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "19"
               STRING  "初診算定日≦最終来院日ではありません。"
                                               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "20"
               STRING  "初診算定日≦システム日付ではありません。"
                                               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "21"
               STRING  "最終来院日≦システム日付ではありません。"
                                               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "22"
               MOVE    "算定履歴更新エラーです。"
                                               TO  WRK-ERRMSG
           WHEN    "23"
               MOVE    "診療科履歴更新エラーです。"
                                               TO  WRK-ERRMSG
      *
           WHEN    "31"
               STRING  "初診料の登録が１件ではありません。"
                       "削除できません。"
                                               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "32"
               STRING  "初診料の登録はありません。削除できません。"
                                               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "33"
               STRING  "削除できる初診料ではありません。"
                                               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "34"
               MOVE    "算定履歴削除エラーです。"
                                               TO  WRK-ERRMSG
           WHEN    "35"
               MOVE    "診療科履歴削除エラーです。"
                                               TO  WRK-ERRMSG
      *警告
           WHEN    "K1"
               STRING  "初診料が算定済みです。"
                       "初診算定日の登録はできません。"
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
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
      *    MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
           MOVE    "K02"               TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-PTID            TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
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
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    診療科歴データ主キー検索
      *****************************************************************
       975-SRYKARRK-READ-SEC           SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYKARRK-REC
               MOVE    ZERO                TO  FLG-SRYKARRK
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
               INITIALIZE                  SRYKARRK-REC
           END-IF
      *
           .
       975-SRYKARRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴順読み
      *****************************************************************
       910-JYURRK-READ-SEC           SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JYURRK-REC 
               MOVE    ZERO                TO  FLG-JYURRK
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           .
       910-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       920-SRYACT-READ-SEC         SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       920-SRYACT-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       940-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       940-SRYACCT-READ-EXT.
           EXIT.
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
