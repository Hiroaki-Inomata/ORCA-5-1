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
       PROGRAM-ID.         ORAPI025R1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 症状詳記情報取得（取得）  (xml2)
      *  管理者            :
      *  作成日付    作業者        記述
      *  19/09/XX    NACL−多々納　新規作成
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
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-RECECOM             PIC 9(01).
           03  FLG-EXAMPLE             PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTRSIINF            PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-HENOK               PIC 9(01).
           03  FLG-HKNCOMBI-ARI        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDY2                    PIC 9(04).
           03  IDK                     PIC 9(04).
           03  IDX-H                   PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-DELETE              PIC 9(06).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(04).
               05  WRK-SMM             PIC X(02).
               05  WRK-SDD             PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-DATE-X.
                   07  WRK-HEN-YY          PIC X(04).
                   07  WRK-HEN-H1          PIC X(01).
                   07  WRK-HEN-MM          PIC X(02).
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
           03  WRK-IN-HKNCOMBI         PIC X(04).
      *    エラーコード
           03  WRK-ERRCD-G.
               05  WRK-ERRCD-H         PIC X(01).
               05  WRK-ERRCD           PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-KEICD               PIC X(02).
      *
           03  WRK-RESULT-MSG          PIC X(100).
      *
           03  WRK-MCP-PATHNAME        PIC X(62).
           03  WRK-MCP-WINDOW          PIC X(62).
      *
           03  WRK-OPID                PIC X(16).
      *
           03  WRK-SJKBN-NAME          PIC X(50).
           03  WRK-COMMENT             PIC X(4800).
      *
      *    03  WRK-MOJI-MAX            PIC 9(04).
      *    03  WRK-RENNUM-MAX          PIC 9(02).
           03  WRK-RSI-HKNKBN          PIC X(01).
      *    03  WRK-KBN2                PIC X(01).
      *
       01  WRK-XML-AREA.
           03  WRK-PTNUM               PIC X(20).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-NAME          PIC X(80).
           03  WRK-PERFORM-DATE        PIC X(10).
           03  WRK-PERFORM-YMD.
               05  WRK-PERFORM-YM          PIC X(06).
               05  WRK-PERFORM-DD          PIC X(02).
           03  WRK-NYUGAIKBN           PIC X(01).
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-SRYDD               PIC X(02).
           03  WRK-SJKBN               PIC X(02).
           03  WRK-RENNUM              PIC X(02).
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ファイルデイレクトリ位置指定サブ
      *    COPY    "CPMKPASS.INC".
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
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
           COPY    "CPORCXKANACONV2.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSHKNSET.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPSUBJECTLSTV2REQ.INC".
      *    API XML書き込み用定義体
           COPY    "CPSUBJECTLSTV2RES.INC".
      *
      *    保険組合せ決定関数サブ
           COPY    "CPORCSAPIHKN.INC".
      *
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム基本
       01  SYSBASE-REC.
           COPY    "CPSYSBASE.INC".
      *
      *    システムユーザー
       01  SYSUSER-REC.
           COPY    "CPSYSUSER.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    診療科情報
           COPY    "CPSK1005.INC".
      *    職員情報
           COPY    "CPSK1010.INC".
      *    患者マスタ−
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    レセプトコメント
       01  RECECOM-REC.
           COPY    "CPRECECOM.INC".
      *
      *    受診履歴
      *01  JYURRK-REC.
      *    COPY    "CPJYURRK.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
      *01  PTHKNINF-REC.
      *    COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
      *01  PTKOHINF-REC.
      *    COPY    "CPPTKOHINF.INC".
      *
      *    患者労災保険情報
      *01  PTRSIINF-REC.
      *    COPY    "CPPTRSIINF.INC".
      *
      *    入院会計マスタ−
      *01  NYUINACCT-REC.
      *    COPY    "CPNYUINACCT.INC".
      *    患者入院履歴
      *01  PTNYUINRRK-REC.
      *    COPY    "CPPTNYUINRRK.INC".
      *
      *    診療会計マスタ−
      *01  SRYACCT-REC.
      ***  COPY    "CPSRYACCT.INC".
      *
           COPY    "ORCA-BLOB".
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
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
           DISPLAY   "***********************"
           DISPLAY   "* subjectiveslst start*"
           DISPLAY   "***********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      CNT-AREA
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
               MOVE   404                  TO  APILST34-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "***********************"
           DISPLAY   "* subjectiveslst end  *"
           DISPLAY   "***********************"
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
           INITIALIZE                      XML-SUBJECTLSTRES
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Acceptance_Info"    TO  SUBJETRES-RESKEY
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
           MOVE    WRK-HEN-DATE        TO  SUBJETRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  SUBJETRES-INFORMATION-TIME
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
           PERFORM 1000-RGTXMLREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           MOVE    ZERO                TO  SUBJETRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD
           MOVE    ZERO                TO  IDK
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    SUBJETREQ-REQUEST-NUMBER
           WHEN    "01"
      *        コメント一覧
               MOVE    1                   TO  FLG-SYORI
           WHEN    "02"
               MOVE    2                   TO  FLG-SYORI
      *        コメント取得
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
           END-EVALUATE
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
      *TEST!!
               PERFORM 890-ERRCD-MSG-SEC
               DISPLAY "必須項目チェックエラー "    WRK-ERRCD
                       " " WRK-ERRMSG
      *TEST!!
               GO      TO      200-MAIN-EXT
           END-IF
      *
           EVALUATE    SUBJETREQ-REQUEST-NUMBER
           WHEN    "01"
      *        コメント一覧取得
               PERFORM 2002-RECECOM-LIST-SEC
           WHEN    "02"
      *        コメント内容取得
               PERFORM 2003-RECECOM-MEI-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-RGTXMLREAD-SEC   SECTION.
      *
           INITIALIZE                      XML-SUBJECTLSTREQ
           INITIALIZE                      XML-APIREQ
      *
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_subjectiveslstv2req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST34-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "subjectiveslstreq" TO  APIREQ-RECORDNAME
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
           MOVE    "xml_subjectiveslstv2req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST34-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "subjectiveslstreq" TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-SUBJECTLSTREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           .
       1000-RGTXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-RGTXMLMAKE-SEC   SECTION.
      *
      *    エラーメッセージ編集
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-KEICD           TO  WRK-ERRCD
               IF      WRK-KEICD       NOT =   SPACE
                   MOVE    "W"             TO  WRK-ERRCD-H
               END-IF
           ELSE
               MOVE    "E"                 TO  WRK-ERRCD-H
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               MOVE    "000"               TO  SUBJETRES-API-RESULT
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "処理終了"
                                           TO  SUBJETRES-API-RESULT-MSG
                   WHEN    2
                       MOVE    "処理終了"
                                           TO  SUBJETRES-API-RESULT-MSG
               END-EVALUATE
           ELSE
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD-G         TO  SUBJETRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  SUBJETRES-API-RESULT-MSG
           END-IF
      *
           IF      APILST34-BODY        NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_subjectiveslstv2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  SUBJETRES-MODE
           MOVE    "subjectiveslstres" TO  SUBJETRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-SUBJECTLSTRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_subjectiveslstv2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  SUBJETRES-MODE
           MOVE    "subjectiveslstres" TO  SUBJETRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-SUBJECTLSTRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               MOVE    SUBJETRES-OBJECT        TO  APILST34-BODY
               MOVE    "subjectiveslst/xml"    TO  APILST34-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       300-RGTXMLMAKE-EXT.
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
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PTNUM-CHK-SEC
           END-IF
      *    診療年月チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PERFORM-CHK-SEC
           END-IF
      *    保険組合せチェック
           IF     (WRK-ERRCD           =   SPACE  )
              AND (WRK-HKNCOMBI     NOT =   SPACE  )
               PERFORM 20011-HKNCOMBI-NUM-SEC
           END-IF
      *    診療日数値チェック
           IF     (WRK-ERRCD           =   SPACE  )
              AND (WRK-SRYDD       NOT =   SPACE  )
               PERFORM 20011-SRYDD-NUM-SEC
           END-IF
      *    連番数値チェック
           IF     (WRK-ERRCD           =   SPACE  )
              AND (WRK-RENNUM      NOT =   SPACE  )
               PERFORM 20011-NUMBER-NUM-SEC
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
           MOVE    SUBJETREQ-PATIENTID     TO  SUBJETRES-PATIENTID
           MOVE    SUBJETREQ-PATIENTID     TO  WRK-PTNUM
      *    診療年月
           MOVE    SUBJETREQ-PERFORM-DATE  TO  SUBJETRES-PERFORM-DATE
      *    数字のみに編集
           MOVE    SPACE                   TO  WRK-HEN-DATE
           MOVE    SUBJETREQ-PERFORM-DATE  TO  WRK-HEN-DATE-X
           MOVE    SPACE                   TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-PERFORM-YMD
           IF     (WRK-PERFORM-YM  NOT =   SPACE )
             AND  (WRK-PERFORM-DD      =   SPACE
                                       OR  "00"  )
               MOVE    "01"                TO  WRK-PERFORM-DD
           END-IF
      *    未設定時はシステム情報設定
           IF      WRK-PERFORM-YMD     =   SPACE
               MOVE    SMCNDATE-YMD        TO  WRK-PERFORM-YMD
               MOVE    "K1"                TO  WRK-KEICD
           END-IF
      *    診療日をシステム日付へ
           MOVE    WRK-PERFORM-YMD     TO  SPA-SYSYMD
      *
      *    診療科
           MOVE    SUBJETREQ-DEP-CODE  TO  WRK-SRYKA
      *
      *    入外区分
           EVALUATE    SUBJETREQ-OUTPATIENT-CLASS
               WHEN    "I"
                   MOVE    "1"             TO  WRK-NYUGAIKBN
               WHEN    "O"
                   MOVE    "2"             TO  WRK-NYUGAIKBN
               WHEN    OTHER
                   MOVE    SPACE           TO  WRK-NYUGAIKBN
           END-EVALUATE
      *    診療日
           MOVE    SUBJETREQ-PERFORM-DAY
                                       TO  WRK-SRYDD
      *    保険組合せ
           MOVE    SUBJETREQ-COMBINATION-NUMBER
                                       TO  WRK-HKNCOMBI
      *
      *    症状詳記区分コード
           MOVE    SUBJETREQ-SUBJECTIVES-CODE
                                       TO  WRK-SJKBN
      *    連番
           MOVE    SUBJETREQ-RENNUM    TO  WRK-RENNUM
      *
      *    レスポンス編集
           PERFORM 200101-RES-INHEN-SEC
      *
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
           MOVE    SPTID-PTNUM         TO  SUBJETRES-PATIENTID
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
      *****************************************************************
      *    保険組合せ番号数値チェック
      *****************************************************************
       20011-HKNCOMBI-NUM-SEC          SECTION.
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SUBJETREQ-COMBINATION-NUMBER   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "12"                TO  WRK-ERRCD
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  WRK-HKNCOMBI
           END-IF
      *
           .
       20011-HKNCOMBI-NUM-EXT.
           EXIT.
      *****************************************************************
      *    診療日数値チェック
      *****************************************************************
       20011-SRYDD-NUM-SEC          SECTION.
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SUBJETREQ-PERFORM-DAY   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "13"                TO  WRK-ERRCD
           ELSE
               MOVE    SNUM-OUTEDT(14:2)   TO  WRK-SRYDD
           END-IF
      *
           .
       20011-SRYDD-NUM-EXT.
           EXIT.
      *****************************************************************
      *    連番数値チェック
      *****************************************************************
       20011-NUMBER-NUM-SEC          SECTION.
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SUBJETREQ-RENNUM    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "14"                TO  WRK-ERRCD
           ELSE
               MOVE    SNUM-OUTEDT(14:2)   TO  WRK-RENNUM
           END-IF
      *
      *    連番号以外に設定があること
           IF      WRK-ERRCD           =   SPACE
               IF     (WRK-HKNCOMBI        =   SPACE )
                   OR (WRK-SRYKA           =   SPACE )
                   OR (WRK-SJKBN           =   SPACE )
                   MOVE    "15"                TO  WRK-ERRCD
               END-IF
           END-IF
           .
       20011-NUMBER-NUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    リクエストをレスポンス情報編集処理
      *****************************************************************
       200101-RES-INHEN-SEC            SECTION.
      *
           MOVE    SUBJETREQ-DEP-CODE  TO  SUBJETRES-DEP-CODE
                                                          (1)
           MOVE    SUBJETREQ-OUTPATIENT-CLASS
                                        TO  SUBJETRES-OUTPATIENT-CLASS
                                                          (1)
           MOVE    SUBJETREQ-PERFORM-DAY
                                       TO  SUBJETRES-PERFORM-DAY
                                                          (1)
           MOVE    SUBJETREQ-COMBINATION-NUMBER
                                       TO  SUBJETRES-COMBINATION-NUMBER
                                                          (1)
           MOVE    SUBJETREQ-SUBJECTIVES-CODE
                                       TO  SUBJETRES-SUBJECTIVES-CODE
                                                          (1)
           MOVE    SUBJETREQ-RENNUM    TO  SUBJETRES-RENNUM
                                                          (1)
           .
       200101-RES-INHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセコメント一覧取得処理
      *****************************************************************
       2002-RECECOM-LIST-SEC            SECTION.
      *
      *    患者情報内容編集
           PERFORM 20021-SUBJETRES-HEN-SEC
      *
           MOVE    ZERO                TO  IDX
           INITIALIZE                  SUBJETRES-SUBJECTIVES-AREA
      *
           MOVE    SPACE               TO  RECECOM-REC
           INITIALIZE                  RECECOM-REC
           MOVE    SPA-HOSPNUM         TO  RECECOM-HOSPNUM
           MOVE    SPA-PTID            TO  RECECOM-PTID
           MOVE    WRK-PERFORM-YM      TO  RECECOM-SRYYM
      *
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key25"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key25"             TO  MCP-PATHNAME
               PERFORM 920-RECECOM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RECECOM
               INITIALIZE                  RECECOM-REC
           END-IF
           PERFORM
                   UNTIL       FLG-RECECOM        =   1
               MOVE    ZERO                TO  FLG-HENOK
      *
               IF     (WRK-SRYKA       NOT =   SPACE   )
                AND   (RECECOM-SRYKA   NOT =   WRK-SRYKA )
                   MOVE    1                   TO  FLG-HENOK
               END-IF
      *
               IF     (WRK-NYUGAIKBN   NOT =   SPACE   )
                AND   (RECECOM-NYUGAIKBN   NOT =   WRK-NYUGAIKBN )
                   MOVE    1                   TO  FLG-HENOK
               END-IF
      *
               IF     (WRK-HKNCOMBI    NOT =   SPACE   )
                AND   (RECECOM-HKNCOMBI    NOT =   WRK-HKNCOMBI )
                   MOVE    1                   TO  FLG-HENOK
               END-IF
      *
               IF     (WRK-SRYDD       NOT =   SPACE   )
                AND   (RECECOM-SRYDD   NOT =   WRK-SRYDD )
                   MOVE    1                   TO  FLG-HENOK
               END-IF
      *
               IF     (WRK-SJKBN       NOT =   SPACE   )
                AND   (RECECOM-SJKBN   NOT =   WRK-SJKBN )
                   MOVE    1                   TO  FLG-HENOK
               END-IF
      *
               IF     (WRK-RENNUM      NOT =   SPACE   )
                AND   (RECECOM-RENNUM  NOT =   WRK-RENNUM )
                   MOVE    1                   TO  FLG-HENOK
               END-IF
      *
               IF      FLG-HENOK           =   ZERO
                   ADD     1                   TO  IDX
                   IF      IDX                 >   50
                       MOVE    "K2"                TO  WRK-KEICD
                   ELSE
                       PERFORM 20022-RECECOM-LIST-SEC
                   END-IF
               END-IF
      *
               IF      FLG-RECECOM         =   ZERO
                   MOVE    "tbl_rececom"       TO  MCP-TABLE
                   MOVE    "key25"             TO  MCP-PATHNAME
                   PERFORM 920-RECECOM-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key25"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      (WRK-ERRCD          =   SPACE )
              AND  (IDX                =   ZERO  )
      *        対象なし
               MOVE    "20"                TO  WRK-ERRCD
      *        レスポンス編集
               PERFORM 200101-RES-INHEN-SEC
           END-IF
           .
       2002-RECECOM-LIST-EXT.
           EXIT.
      *****************************************************************
      *    患者情報出力情報編集処理
      *****************************************************************
       20021-SUBJETRES-HEN-SEC     SECTION.
      *
      *    診療年月
           MOVE    WRK-PERFORM-YMD     TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE-X      TO  SUBJETRES-PERFORM-DATE
      *    患者情報
           MOVE    PTINF-NAME          TO  SUBJETRES-NAME
           MOVE    PTINF-KANANAME      TO  SUBJETRES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  SUBJETRES-BIRTHDAY
           MOVE    PTINF-SEX           TO  SUBJETRES-SEX
           .
       20021-SUBJETRES-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記一覧編集処理
      *****************************************************************
       20022-RECECOM-LIST-SEC      SECTION.
      *
      *    入外区分
           IF      RECECOM-NYUGAIKBN   =   "1"
               MOVE    "I"         TO  SUBJETRES-OUTPATIENT-CLASS (IDX)
           ELSE
               MOVE    "O"         TO  SUBJETRES-OUTPATIENT-CLASS (IDX)
           END-IF
      *    診療科
           MOVE    RECECOM-SRYKA       TO  SUBJETRES-DEP-CODE (IDX)
           PERFORM 200221-SRYKA-HEN-SEC
           MOVE    WRK-SRYKA-NAME      TO  SUBJETRES-DEP-NAME(IDX)
      *    保険組合せ
           MOVE    RECECOM-HKNCOMBI    TO  SUBJETRES-COMBINATION-NUMBER
                                                              (IDX)
           PERFORM 200222-HKNCOMBI-HEN-SEC
      *    診療日
           IF      RECECOM-SRYDD       =   ZERO
               MOVE    SPACE           TO  SUBJETRES-PERFORM-DAY(IDX)
           ELSE
               MOVE    RECECOM-SRYDD   TO  SUBJETRES-PERFORM-DAY(IDX)
           END-IF
      *    症状詳記区分
           MOVE    RECECOM-SJKBN       TO  SUBJETRES-SUBJECTIVES-CODE
                                                              (IDX)
           PERFORM 200223-SJKBN-HEN-SEC
           MOVE    WRK-SJKBN-NAME      TO  SUBJETRES-SUBJECTIVES-NAME
                                                              (IDX)
      *    連番
           MOVE    RECECOM-RENNUM      TO  SUBJETRES-RENNUM   (IDX)
      *
      *    １件目の内容
           IF      IDX                 =   1
               MOVE    RECECOM-COMMENT     TO  WRK-COMMENT
           END-IF
           .
       20022-RECECOM-LIST-EXT.
           EXIT.
      *****************************************************************
      *    診療科コードチェック
      *****************************************************************
       200221-SRYKA-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYKA-NAME
           PERFORM VARYING    IDY      FROM    1   BY  1
                   UNTIL     (IDY      >=  IDX )
                         OR  (WRK-SRYKA-NAME   =   SPACE )
               IF      RECECOM-SRYKA   =   SUBJETRES-DEP-CODE (IDY)
                   MOVE    SUBJETRES-DEP-NAME (IDY)
                                           TO  WRK-SRYKA-NAME
               END-IF
           END-PERFORM
           IF      WRK-SRYKA-NAME  NOT =   SPACE
               GO  TO  200221-SRYKA-HEN-EXT
           END-IF
      *
           IF      RECECOM-SRYKA       =   "00"
               MOVE    "全科指定"          TO  WRK-SRYKA-NAME
           ELSE
      *        診療科の存在チェック
               MOVE    SPACE               TO  SYS-1005-REC
               INITIALIZE                  SYS-1005-REC
               MOVE    "1005"              TO  SYS-1005-KANRICD
               MOVE    RECECOM-SRYKA       TO  SYS-1005-KBNCD
               MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
               MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      *        システム管理検索
               MOVE    SYS-1005-REC        TO  SYSKANRI-REC
               PERFORM 900-SYSKANRI-SEL-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    SYSKANRI-REC        TO  SYS-1005-REC
                   MOVE    SYS-1005-SRYKANAME1 TO  WRK-SRYKA-NAME
               ELSE
     *****         MOVE    "12"                TO  WRK-ERRCD
                   MOVE    SPACE               TO  WRK-SRYKA-NAME
               END-IF
           END-IF
      *
           .
       200221-SRYKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記区分編集処理
      *****************************************************************
       200223-SJKBN-HEN-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-SJKBN-NAME
           EVALUATE    RECECOM-SJKBN
               WHEN    "01"
                   MOVE    "主たる疾患の臨床症状"
                                               TO  WRK-SJKBN-NAME
               WHEN    "02"
                   MOVE    "主たる疾患の診療・検査所見"
                                               TO  WRK-SJKBN-NAME
               WHEN    "03"
                   MOVE    "主な治療行為の必要性"
                                               TO  WRK-SJKBN-NAME
               WHEN    "04"
                   MOVE    "主な治療行為の経過"
                                               TO  WRK-SJKBN-NAME
               WHEN    "05"
                   MOVE    "１００万点以上の薬剤に係る症状等"
                                               TO  WRK-SJKBN-NAME
               WHEN    "06"
                   MOVE    "１００万点以上の処置に係る症状等"
                                               TO  WRK-SJKBN-NAME
               WHEN    "07"
                   MOVE    "その他（１）"
                                               TO  WRK-SJKBN-NAME
               WHEN    "08"
                   MOVE    "その他（２）"
                                               TO  WRK-SJKBN-NAME
               WHEN    "09"
                   MOVE    "その他（３）"
                                               TO  WRK-SJKBN-NAME
               WHEN    "50"
                   MOVE    "治験概要"
                                               TO  WRK-SJKBN-NAME
               WHEN    "51"
                   MOVE    "疾患別リハビリテーションに係る治療継続理由"
                                               TO  WRK-SJKBN-NAME
               WHEN    "52"
                   MOVE    "廃用症候群に係る評価表"
                                               TO  WRK-SJKBN-NAME
      *H26.4
               WHEN    "90"
                   MOVE    "上記以外の診療報酬明細書"
                                               TO  WRK-SJKBN-NAME
               WHEN    "99"
                   MOVE    "レセプト摘要欄コメント"
                                               TO  WRK-SJKBN-NAME
      *H25.7
               WHEN    "AA"
                   MOVE    "労災レセプト「傷病の経過」"
                                               TO  WRK-SJKBN-NAME
      *H25.9
               WHEN    "BB"
                   MOVE    "自賠責（第三者行為）レセプト摘要欄コメント"
                                               TO  WRK-SJKBN-NAME
           END-EVALUATE
      *
           .
       200223-SJKBN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       200222-HKNCOMBI-HEN-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI-ARI
           PERFORM VARYING    IDY      FROM    1   BY  1
                   UNTIL     (IDY      >=  IDX )
                         OR  (FLG-HKNCOMBI-ARI =   1    )
               IF      RECECOM-HKNCOMBI
                                       =   SUBJETRES-COMBINATION-NUMBER
                                                             (IDY)
                   MOVE    SUBJETRES-INSURANCE-INFO (IDY)
                                           TO  SUBJETRES-INSURANCE-INFO
                                                             (IDX)
                   MOVE    1               TO  FLG-HKNCOMBI-ARI
               END-IF
           END-PERFORM
           IF      FLG-HKNCOMBI-ARI    =   1
               GO      TO      200222-HKNCOMBI-HEN-EXT
           END-IF
      *
      *    保険組合せサブＣＡＬＬ処理
           PERFORM 200231-ORCSAPIHKN-SYORI-SEC
           .
       200222-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せサブＣＡＬＬ処理
      *****************************************************************
       200231-ORCSAPIHKN-SYORI-SEC        SECTION.
      *
      *    保険組合せ編集処理
           INITIALIZE                      ORCSAPIHKNAREA
      *    保険組合せ番号分のみ
           MOVE    "3"                 TO  ORCSAPIHKN-KBN
      *    当月分
           MOVE    "2"                 TO  ORCSAPIHKN-KBN2
      *    保険組合せ
           MOVE    RECECOM-HKNCOMBI    TO  ORCSAPIHKN-IN-HKNCOMBI
           CALL    "ORCSAPIHKN"        USING
                                       SPA-AREA
                                       ORCSAPIHKNAREA
           IF      ORCSAPIHKN-OT-HKNCOMBI  =   SPACE
      *        対象なし
               CONTINUE
           ELSE
      *        １件目に編集
               MOVE    1                   TO  IDX-H
               PERFORM 200231-HKNHEN-TBL-SEC
           END-IF
           .
       200231-ORCSAPIHKN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力領域保険情報編集処理
      *****************************************************************
       200231-HKNHEN-TBL-SEC              SECTION.
      *
      *    削除分
           IF      ORCSAPIHKN-ERRCD    =   8
               MOVE    "1"             TO  SUBJETRES-COMBINATION-MODE
                                                             (IDX)
           END-IF
      *    期間外
           IF      ORCSAPIHKN-ERRCD    =   7
               MOVE    "2"             TO  SUBJETRES-COMBINATION-MODE
                                                             (IDX)
           END-IF
      *    保険組合せ有効期間
           MOVE    ORCSAPIHKN-COMB-TEKSTYMD (IDX-H)
                                   TO  SUBJETRES-COMB-START-DATE
                                                             (IDX)
           MOVE    ORCSAPIHKN-COMB-TEKEDYMD (IDX-H)
                                   TO  SUBJETRES-COMB-END-DATE
                                                             (IDX)
      *    保険の種類
           MOVE    ORCSAPIHKN-HKNNUM (IDX-H)
                                   TO  SUBJETRES-MAIN-INSURANCE-CLASS
                                                             (IDX)
      *    保険者番号
           MOVE    ORCSAPIHKN-HKNJANUM (IDX-H)
                                   TO  SUBJETRES-MAIN-INSURANCE-NUMBER
                                                             (IDX)
      *    保険の名称
           MOVE    ORCSAPIHKN-HKNNUM-NAME (IDX-H)
                                   TO  SUBJETRES-MAIN-INSURANCE-NAME
                                                             (IDX)
      *    記号
           MOVE    ORCSAPIHKN-KIGO  (IDX-H)
                                   TO  SUBJETRES-MAIN-MARK 
                                                             (IDX)
      *    番号
           MOVE    ORCSAPIHKN-NUM  (IDX-H)
                                   TO  SUBJETRES-MAIN-NUMBER
                                                             (IDX)
      *    補助区分
           MOVE    ORCSAPIHKN-HOJOKBN  (IDX-H)
                                   TO  SUBJETRES-MAIN-ASSISTANCE
                                                             (IDX)
      *    補助区分名称
           MOVE    ORCSAPIHKN-HOJOKBN-NAME  (IDX-H)
                                   TO  SUBJETRES-MAIN-ASSISTANCE-NAME
                                                             (IDX)
      *    本人家族区分
           MOVE    ORCSAPIHKN-HONKZKKBN  (IDX-H)
                                   TO  SUBJETRES-MAIN-FAMILY
                                                             (IDX)
      *    公費情報
           PERFORM VARYING     IDY2    FROM    1   BY  1
                   UNTIL       IDY2    >   4
      *        公費の種類
               MOVE    ORCSAPIHKN-KOHNUM (IDX-H IDY2)
                                       TO  SUBJETRES-INSURANCE-CLASS
                                                        (IDX IDY2)
      *        公費の種類名称
               MOVE    ORCSAPIHKN-KOHNUM-NAME (IDX-H IDY2)
                                       TO  SUBJETRES-INSURANCE-NAME
                                                        (IDX IDY2)
      *        負担者番号
               MOVE    ORCSAPIHKN-FTNJANUM (IDX-H IDY2)
                                       TO  SUBJETRES-PROVIDER-NUMBER
                                                        (IDX IDY2)
      *        受給者番号
               MOVE    ORCSAPIHKN-JKYSNUM (IDX-H IDY2)
                                       TO  SUBJETRES-RECIPIENT-NUMBER
                                                        (IDX IDY2)
           END-PERFORM
      *
           .
       200231-HKNHEN-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセコメント一覧取得処理
      *****************************************************************
       2003-RECECOM-MEI-SEC            SECTION.
      *
      *    コメント一覧取得
           PERFORM 2002-RECECOM-LIST-SEC
      *
           IF      IDX                 >=  1
      *        １件目の症状詳記
               MOVE    WRK-COMMENT         TO  SUBJETRES-DETAIL-RECORD
           END-IF
           IF      (WRK-ERRCD          =   SPACE )
              AND  (IDX                >   1     )
      *        複数対象あり
               MOVE    "K3"                TO  WRK-KEICD
           END-IF
           .
       2003-RECECOM-MEI-EXT.
           EXIT.
      *
      *
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
               MOVE    "患者番号が未設定です。"
                                               TO  WRK-ERRMSG
           WHEN    "10"
               MOVE    "患者番号に該当する患者が存在しません。"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "診療年月が暦日ではありません。"
                                               TO  WRK-ERRMSG
           WHEN    "12"
               MOVE    "保険組合せ番号が数値エラーです。"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE    "診療日が数値エラーです。"
                                               TO  WRK-ERRMSG
           WHEN    "14"
               MOVE    "連番号が数値エラーです。"
                                               TO  WRK-ERRMSG
           WHEN    "15"
               STRING  "連番号と診療科、保険組合せ、症状詳記区分の"
                       "設定が必要です。"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "20"
               MOVE    "対象が存在しません。"
                                               TO  WRK-ERRMSG
      *警告
           WHEN    "K1"
               MOVE    "診療年月を設定しました。"
                                               TO  WRK-ERRMSG
           WHEN    "K2"
               MOVE    "対象が５０件以上あります。"
                                               TO  WRK-ERRMSG
           WHEN    "K3"
               STRING  "対象が複数あります。"
                       "最初のコメントのみ返却します。"
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
      *    レセプトコメント読込処理
      *****************************************************************
       920-RECECOM-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-RECECOM
               MOVE    MCPDATA-REC         TO  RECECOM-REC
           ELSE
               MOVE    1                   TO  FLG-RECECOM
               INITIALIZE                      RECECOM-REC
           END-IF
      *
           .
       920-RECECOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       920-HKNCOMBI-READ-SEC              SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   MOVE    ZERO                TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                  HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       920-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
           MOVE    "C02"               TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    1                   TO  SLOCK-MODE
           MOVE    PTINF-PTID          TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               DISPLAY "排他エラー = " PTINF-PTID
               DISPLAY "TERMID     = " SPA-TERMID
               DISPLAY "RC         = " SLOCK-RETURN
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
