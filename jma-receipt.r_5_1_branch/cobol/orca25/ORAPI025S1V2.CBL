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
       PROGRAM-ID.         ORAPI025S1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 症状詳記コメント登録  (xml2)
      *  管理者            :
      *  作成日付    作業者        記述
      *  12/07/24    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.00    NACL-多々納  13/09/XX  第三者行為対応
      *  04.07.00    NACL-多々納  14/07/03  ワーニングメッセージ対応他
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
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-COMMENT             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDY2                    PIC 9(04).
           03  IDK                     PIC 9(04).
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
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-IN-HKNCOMBI         PIC X(04).
      *    エラーコード
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-KEICD-G.
               05  WRK-KEICD               PIC X(02)   OCCURS   5.
      *
           03  WRK-RESULT-MSG          PIC X(100).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).
           03  WRK-MCP-WINDOW          PIC X(64).
           03  WRK-MCP-TERM            PIC X(64).
      *
           03  WRK-OPID                PIC X(16).
      *
           03  WRK-MOJI-MAX            PIC 9(04).
           03  WRK-RENNUM-MAX          PIC 9(02).
           03  WRK-RSI-HKNKBN          PIC X(01).
           03  WRK-KBN2                PIC X(01).
      *
       01  WRK-XML-AREA.
           03  WRK-PTNUM               PIC X(20).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-NAME          PIC X(80).
           03  WRK-PERFORM-DATE        PIC X(10).
           03  WRK-PERFORM-YMD.
               05  WRK-PERFORM-YM          PIC X(06).
               05  WRK-PERFORM-DD          PIC X(02).
           03  WRK-SJKBN               PIC X(02).
           03  WRK-SJKBN-NAME          PIC X(50).
           03  WRK-COMMENT             PIC X(4800).
      *
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
           COPY    "CPSUBJECTIVESV2REQ.INC".
      *    API XML書き込み用定義体
           COPY    "CPSUBJECTIVESV2RES.INC".
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
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
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
           COPY    "C50.INC".
           COPY    "C501.INC".
           COPY    "C50ERR.INC".
           COPY    "C50ID1.INC".
           COPY    "CPAPIMODV2.INC".
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
           DISPLAY   "* subjectivesmod start*"
           DISPLAY   "***********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      CNT-AREA
           INITIALIZE                      SPA-AREA
      *
           MOVE    MCP-TERM            TO  WRK-MCP-TERM
           MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
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
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
      *
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
           MOVE    WRK-MCP-TERM        TO  MCP-TERM
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
      *
           DISPLAY   "***********************"
           DISPLAY   "* subjectivesmod end  *"
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
           INITIALIZE                      XML-SUBJECTIVESRES
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
           MOVE    SPACE               TO  WRK-KEICD-G
           MOVE    ZERO                TO  IDK
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    APIMOD-CLASS
           WHEN    "01"
      *        コメント登録
               MOVE    1                   TO  FLG-SYORI
           WHEN    "02"
               MOVE    2                   TO  FLG-SYORI
      *        コメント削除
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
           EVALUATE    APIMOD-CLASS
           WHEN    "01"
      *        コメント登録処理
               PERFORM 2002-RECECOM-ADD-SEC
           WHEN    "02"
      *        コメント削除処理
               PERFORM 2003-RECECOM-DELETE-SEC
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
           INITIALIZE                      XML-SUBJECTIVESREQ
           INITIALIZE                      XML-APIREQ
      *
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_subjectivesmodv2req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APIMOD-BODY         TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "subjectivesmodreq" TO  APIREQ-RECORDNAME
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
           MOVE    "xml_subjectivesmodv2req"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APIMOD-BODY         TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "subjectivesmodreq" TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-SUBJECTIVESREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE           TO  APIMOD-BODY
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
      *    IF      WRK-ERRCD           =   SPACE
      *        IF      FLG-SYORI           =   1
      *            MOVE    WRK-KEICD           TO  WRK-ERRCD
      *        END-IF
      **** END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "レセコメント登録終了"
                                           TO  SUBJETRES-API-RESULT-MSG
                   WHEN    2
                       MOVE    "レセコメント削除終了"
                                           TO  SUBJETRES-API-RESULT-MSG
               END-EVALUATE
           ELSE
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD           TO  SUBJETRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  SUBJETRES-API-RESULT-MSG
           END-IF
      *
      *    警告メッセージ編集
           IF     (WRK-KEICD-G     NOT =   SPACE )
             AND  (FLG-SYORI           =   1     )
             AND  (WRK-ERRCD           =   SPACE )
               PERFORM VARYING     IDK     FROM    1   BY  1
                       UNTIL      (IDK     >   5   )
                              OR  (WRK-KEICD (IDK) =   SPACE )
                   MOVE    WRK-KEICD (IDK)     TO  WRK-ERRCD
                   PERFORM 890-ERRCD-MSG-SEC
                   IF      IDK             =   1
                       MOVE    WRK-ERRCD       TO  SUBJETRES-API-RESULT
                   END-IF
                   MOVE    WRK-ERRMSG      TO  SUBJETRES-API-WARNING-MSG
                                                                 (IDK)
               END-PERFORM
           END-IF
      *
           IF      APIMOD-BODY        NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_subjectivesmodv2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  SUBJETRES-MODE
           MOVE    "subjectivesmodres" TO  SUBJETRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-SUBJECTIVESRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_subjectivesmodv2res"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  SUBJETRES-MODE
           MOVE    "subjectivesmodres" TO  SUBJETRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-SUBJECTIVESRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               MOVE    SUBJETRES-OBJECT        TO  APIMOD-BODY
               MOVE    "subjectivesmod/xml"    TO  APIMOD-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
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
      *R01.12
      *    診療年月チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PERFORM-CHK-SEC
           END-IF
      *
      *    診療科コードチェック
           IF     (WRK-ERRCD           =   SPACE  )
               PERFORM 20011-SRYKA-CHK-SEC
           END-IF
      *    症状詳記区分コードチェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-SJKBN-CHK-SEC
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
      *    診療科
           MOVE    SUBJETREQ-DEP-CODE      TO  SUBJETRES-DEP-CODE
           MOVE    SUBJETREQ-DEP-CODE      TO  WRK-SRYKA
      *    診療年月
           MOVE    SUBJETREQ-PERFORM-DATE  TO  SUBJETRES-PERFORM-DATE
      *    数字のみに編集
           MOVE    SUBJETREQ-PERFORM-DATE  TO  WRK-HEN-DATE
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
               ADD     1                   TO  IDK
               MOVE    "K1"                TO  WRK-KEICD (IDK)
           END-IF
      *    診療日をシステム日付へ
           MOVE    WRK-PERFORM-YMD     TO  SPA-SYSYMD
      *
      *    入外区分
           IF      SUBJETREQ-OUTPATIENT-CLASS  =  "I"
               MOVE    "1"                 TO  SPA-NYUGAIKBN
           ELSE
               MOVE    "2"                 TO  SPA-NYUGAIKBN
           END-IF
      *    症状詳記区分コード
           MOVE    SUBJETREQ-SUBJECTIVES-CODE
                                       TO  SUBJETRES-SUBJECTIVES-CODE
           MOVE    SUBJETREQ-SUBJECTIVES-CODE
                                       TO  WRK-SJKBN
      *
      *    症状詳記内容編集
           PERFORM 20023-COMMENT-HEN-SEC
      *
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
      *    症状詳記区分コード
           IF      WRK-SJKBN           =   SPACE
               IF     (WRK-ERRCD           =   SPACE  )
                   MOVE    "03"                TO  WRK-ERRCD
               END-IF
           END-IF
      *    症状詳記
           IF     (FLG-SYORI           =   1   )   AND
                  (WRK-COMMENT         =   SPACE )
               IF     (WRK-ERRCD           =   SPACE  )
                   MOVE    "04"                TO  WRK-ERRCD
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
      *
      *****************************************************************
      *    診療科コードチェック
      *****************************************************************
       20011-SRYKA-CHK-SEC         SECTION.
      *
           IF      WRK-SRYKA           =   "00"
               MOVE    "全科指定"          TO  WRK-SRYKA-NAME
           ELSE
      *       診療科の存在チェック
               MOVE    SPACE               TO  SYS-1005-REC
               INITIALIZE                  SYS-1005-REC
               MOVE    "1005"              TO  SYS-1005-KANRICD
               MOVE    WRK-SRYKA           TO  SYS-1005-KBNCD
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
                   MOVE    "12"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
           .
       20011-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記区分チェック
      *****************************************************************
       20011-SJKBN-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-SJKBN-NAME
           EVALUATE    WRK-SJKBN
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
               WHEN    OTHER
      *        エラー
                   MOVE    "13"                TO  WRK-ERRCD
           END-EVALUATE
      *
           .
       20011-SJKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセコメント登録処理
      *****************************************************************
       2002-RECECOM-ADD-SEC            SECTION.
      *
      *    患者情報内容編集
           PERFORM 20021-SUBJETRES-HEN-SEC
      * 
      *    排他チェック & ロック処理
           PERFORM 990-LOCK-IN-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "90"            TO  WRK-ERRCD
               GO      TO              2002-RECECOM-ADD-EXT
           END-IF
      * 
           PERFORM 990-LOCK-OUT-SEC
           IF      SLOCK-RETURN   NOT =   ZERO
               DISPLAY "ロック解除失敗"
           END-IF
      *
      *    保険組合せ編集処理
           PERFORM 20023-HKNCOMBI-HEN-SEC
      *
      *    受診履歴チェック処理
           IF     (WRK-ERRCD           =   SPACE )  AND
                  (WRK-HKNCOMBI    NOT =   ZERO  )
               PERFORM 20024-JYURRK-CHK-SEC
           END-IF
      *
      *    レセコメント登録処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20022-RECECOM-INSERT-SEC
           END-IF
           .
       2002-RECECOM-ADD-EXT.
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
      *
           MOVE    PTINF-NAME          TO  SUBJETRES-NAME
           MOVE    PTINF-KANANAME      TO  SUBJETRES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  SUBJETRES-BIRTHDAY
           MOVE    PTINF-SEX           TO  SUBJETRES-SEX
      *
           MOVE    WRK-SRYKA-NAME      TO  SUBJETRES-DEP-NAME
      *
           MOVE    WRK-SJKBN-NAME      TO  SUBJETRES-SUBJECTIVES-NAME
      *    入外区分
           EVALUATE    SPA-NYUGAIKBN
               WHEN    "1"
                   MOVE    "I"         TO  SUBJETRES-OUTPATIENT-CLASS
               WHEN    "2"
                   MOVE    "O"         TO  SUBJETRES-OUTPATIENT-CLASS
           END-EVALUATE
           .
       20021-SUBJETRES-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記内容編集処理
      *****************************************************************
       20023-COMMENT-HEN-SEC      SECTION.
      *
      *    症状詳記内容チェック
           MOVE    2400                TO  WRK-MOJI-MAX
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    WRK-MOJI-MAX        TO  KANACONV-LEN
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    0                   TO  KANACONV-CHAR-TYPE
           MOVE    SUBJETREQ-DETAIL-RECORD TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
           IF     (KANACONV-RETURN     =   ZERO )
               MOVE    KANACONV-OUTDATA
                                       TO  WRK-COMMENT
           ELSE
               MOVE    KANACONV-OUTDATA
                                       TO  WRK-COMMENT
           END-IF
      *    全角変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MOJI-MAX        TO  KANACHK-MAX-CNT
           MOVE    WRK-COMMENT         TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-COMMENT
           IF     (KANACHK-RC      NOT =   ZERO  )
               CONTINUE
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  WRK-COMMENT
           END-IF
           MOVE    WRK-COMMENT         TO  SUBJETRES-DETAIL-RECORD
           IF      SUBJETREQ-DETAIL-RECORD
                                   NOT =   SUBJETRES-DETAIL-RECORD
      *        コメント内容変換あり
               MOVE    1                   TO  FLG-COMMENT
               ADD     1                   TO  IDK
               MOVE    "K4"                TO  WRK-KEICD (IDK)
           END-IF
           .
       20023-COMMENT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       20023-HKNCOMBI-HEN-SEC     SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBI
           MOVE    SPACE               TO  WRK-IN-HKNCOMBI
           IF      SUBJETREQ-COMBINATION-NUMBER    NOT =   SPACE
      *        保険組合せチェック
               PERFORM 200311-HKNCOMBI-NUM-SEC
           END-IF
      *    保険組合せ番号＝ゼロ
           IF      WRK-HKNCOMBI        =   ZERO
               MOVE    WRK-HKNCOMBI    TO  SUBJETRES-COMBINATION-NUMBER
               GO      TO      20023-HKNCOMBI-HEN-EXT
           END-IF
      *
      *    保険組合せサブＣＡＬＬ処理
      *    診療年月の処理
           MOVE    "2"                 TO  WRK-KBN2
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
           MOVE    "2"                 TO  ORCSAPIHKN-KBN
      *    処理区分
           MOVE    WRK-KBN2            TO  ORCSAPIHKN-KBN2
      *    診療科
           MOVE    WRK-SRYKA           TO  ORCSAPIHKN-SRYKA
      *    保険組合せ
           MOVE    WRK-IN-HKNCOMBI     TO  ORCSAPIHKN-IN-HKNCOMBI
      *    保険種類
           MOVE    SUBJETREQ-MAIN-INSURANCE-CLASS 
                                       TO  ORCSAPIHKN-IN-HKNNUM
      *    保険者番号
           MOVE    SUBJETREQ-MAIN-INSURANCE-NUMBER
                                       TO  ORCSAPIHKN-IN-HKNJANUM
      *    保険種類
           MOVE    SUBJETREQ-MAIN-INSURANCE-NAME 
                                       TO  ORCSAPIHKN-IN-HKNNUM-NAME
      *    記号
           MOVE    SUBJETREQ-MAIN-MARK  TO  ORCSAPIHKN-IN-MARK
      *    番号
           MOVE    SUBJETREQ-MAIN-NUMBER
                                       TO  ORCSAPIHKN-IN-NUMBER
      *    有効期間
           MOVE    SUBJETREQ-MAIN-START-DATE
                                       TO  WRK-HEN-DATE
           MOVE    SPACE               TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD            TO  ORCSAPIHKN-IN-TEKSTYMD
           MOVE    SUBJETREQ-MAIN-END-DATE
                                       TO  WRK-HEN-DATE
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD            TO  ORCSAPIHKN-IN-TEKEDYMD
      *    公費情報
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
      *        公費種類
               MOVE    SUBJETREQ-INSURANCE-CLASS (IDX)
                                       TO  ORCSAPIHKN-IN-KOHNUM (IDX)
      *        公費名称
               MOVE    SUBJETREQ-INSURANCE-NAME  (IDX)
                                       TO  ORCSAPIHKN-IN-KOHNUM-NAME
                                                                 (IDX)
      *        負担者番号
               MOVE    SUBJETREQ-PROVIDER-NUMBER(IDX)
                                       TO  ORCSAPIHKN-IN-FTNJANUM (IDX)
      *        受給者番号
               MOVE    SUBJETREQ-RECIPIENT-NUMBER (IDX)
                                       TO  ORCSAPIHKN-IN-JKYSNUM  (IDX)
           END-PERFORM
           CALL    "ORCSAPIHKN"        USING
                                       SPA-AREA
                                       ORCSAPIHKNAREA
           IF      ORCSAPIHKN-OT-HKNCOMBI  =   SPACE
               MOVE    ZERO                    TO  WRK-HKNCOMBI
               IF      WRK-KBN2            =   "1"
                   ADD     1                   TO  IDK
                   MOVE    "K2"                TO  WRK-KEICD (IDK)
               END-IF
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
           MOVE    SUBJETREQ-COMBINATION-NUMBER   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "25"                TO  WRK-ERRCD
               MOVE    SUBJETREQ-COMBINATION-NUMBER
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
      *    当月対象外
           IF      FLG-OK              =   ZERO
      *        全体での処理
               MOVE    "1"                 TO  WRK-KBN2
               PERFORM 200311-ORCSAPIHKN-SYORI-SEC
               IF     (WRK-HKNCOMBI        =   ZERO )
                   IF      WRK-IN-HKNCOMBI     NOT =   SPACE
                       MOVE    "25"                TO  WRK-ERRCD
                   END-IF
               ELSE
                   ADD     1                   TO  IDK
                   MOVE    "K3"                TO  WRK-KEICD(IDK)
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL      (IDX     >   20  )
                             OR   (ORCSAPIHKN-THKNCOMBI (IDX)
                                               =   SPACE  )
                       IF      ORCSAPIHKN-THKNCOMBI (IDX)
                                               =   WRK-HKNCOMBI
                           PERFORM 200231-HKNHEN-TBL-SEC
                           MOVE    20                  TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *    保険組合せ
           MOVE    WRK-HKNCOMBI        TO  SUBJETRES-COMBINATION-NUMBER
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
                                   TO  SUBJETRES-MAIN-INSURANCE-CLASS
      *    保険者番号
           MOVE    ORCSAPIHKN-HKNJANUM (IDX)
                                   TO  SUBJETRES-MAIN-INSURANCE-NUMBER
      *    保険の名称
           MOVE    ORCSAPIHKN-HKNNUM-NAME (IDX)
                                   TO  SUBJETRES-MAIN-INSURANCE-NAME
      *    記号
           MOVE    ORCSAPIHKN-KIGO  (IDX)
                                   TO  SUBJETRES-MAIN-MARK 
      *    番号
           MOVE    ORCSAPIHKN-NUM  (IDX)
                                   TO  SUBJETRES-MAIN-NUMBER
      *    継続区分
           MOVE    ORCSAPIHKN-CONTKBN  (IDX)
                                   TO  SUBJETRES-MAIN-CONTINUATION
      *    補助区分
           MOVE    ORCSAPIHKN-HOJOKBN  (IDX)
                                   TO  SUBJETRES-MAIN-ASSISTANCE
      *    補助区分名称
           MOVE    ORCSAPIHKN-HOJOKBN-NAME  (IDX)
                                   TO  SUBJETRES-MAIN-ASSISTANCE-NAME
      *    本人家族区分
           MOVE    ORCSAPIHKN-HONKZKKBN  (IDX)
                                   TO  SUBJETRES-MAIN-FAMILY
      *    被保険者名
           MOVE    ORCSAPIHKN-HIHKNJANAME  (IDX)
                                   TO  SUBJETRES-MAIN-POLICYHOLDER
      *    有効開始日
           MOVE    ORCSAPIHKN-TEKSTYMD  (IDX)
                                   TO  SUBJETRES-MAIN-START-DATE
      *    有効終了日
           MOVE    ORCSAPIHKN-TEKEDYMD  (IDX)
                                   TO  SUBJETRES-MAIN-END-DATE
      *    労災区分
           MOVE    ORCSAPIHKN-RSI-HKNKBN  (IDX)
                                   TO  WRK-RSI-HKNKBN
      *    公費情報
           PERFORM VARYING     IDY2    FROM    1   BY  1
                   UNTIL       IDY2    >   4
      *        公費の種類
               MOVE    ORCSAPIHKN-KOHNUM (IDX IDY2)
                                       TO  SUBJETRES-INSURANCE-CLASS
                                                             (IDY2)
      *        公費の種類名称
               MOVE    ORCSAPIHKN-KOHNUM-NAME (IDX IDY2)
                                       TO  SUBJETRES-INSURANCE-NAME
                                                             (IDY2)
      *        負担者番号
               MOVE    ORCSAPIHKN-FTNJANUM (IDX IDY2)
                                       TO  SUBJETRES-PROVIDER-NUMBER
                                                             (IDY2)
      *        受給者番号
               MOVE    ORCSAPIHKN-JKYSNUM (IDX IDY2)
                                       TO  SUBJETRES-RECIPIENT-NUMBER
                                                             (IDY2)
      *        有効開始日
               MOVE    ORCSAPIHKN-KOH-TEKSTYMD (IDX IDY2)
                                   TO  SUBJETRES-INSURANCE-START-DATE
                                                             (IDY2)
      *        有効終了日
               MOVE    ORCSAPIHKN-KOH-TEKEDYMD (IDX IDY2)
                                       TO  SUBJETRES-INSURANCE-END-DATE
                                                             (IDY2)
      *        入院−負担率（割）
               MOVE    ORCSAPIHKN-RATE-ADMISSION (IDX IDY2)
                                       TO  SUBJETRES-RATE-ADMISSION
                                                             (IDY2)
      *        入院−固定額
               MOVE    ORCSAPIHKN-MONEY-ADMISSION (IDX IDY2)
                                       TO  SUBJETRES-MONEY-ADMISSION
                                                             (IDY2)
      *        外来−負担率（割）
               MOVE    ORCSAPIHKN-RATE-OUTPATIENT (IDX IDY2)
                                       TO  SUBJETRES-RATE-OUTPATIENT
                                                             (IDY2)
      *        外来−固定額
               MOVE    ORCSAPIHKN-MONEY-OUTPATIENT (IDX IDY2)
                                       TO  SUBJETRES-MONEY-OUTPATIENT
                                                             (IDY2)
           END-PERFORM
      *
           .
       200231-HKNHEN-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴チェック処理
      *****************************************************************
       20024-JYURRK-CHK-SEC         SECTION.
      *
           IF     (SUBJETRES-MAIN-INSURANCE-CLASS  =   "971")
             AND  (WRK-RSI-HKNKBN      =   "3"   )
      *        アフターケアの時、診療日付であること
               MOVE    SUBJETREQ-PERFORM-DATE  TO  WRK-HEN-DATE
               MOVE    SPACE                   TO  WRK-HEN-TIME
               PERFORM 802-DAYHEN02-SEC
               MOVE    WRK-SYMD                TO  WRK-PERFORM-YMD
               IF     (WRK-SYMD        NOT =   SPACE )  AND
                      (WRK-SDD             =   "00"  OR  SPACE)
                   MOVE    "20"                TO  WRK-ERRCD
               END-IF
      *        診療年月
               MOVE    SUBJETREQ-PERFORM-DATE
                                           TO  SUBJETRES-PERFORM-DATE
      *        受診履歴があること
               IF      WRK-ERRCD           =   SPACE
                   PERFORM 2002411-JYURRK-SYORI-SEC
               END-IF
           END-IF
      *
           .
       20024-JYURRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴チェック処理
      *****************************************************************
       2002411-JYURRK-SYORI-SEC         SECTION.
      *
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    WRK-PERFORM-YMD     TO  JYURRK-SRYYMD
           MOVE    WRK-HKNCOMBI        TO  JYURRK-HKNCOMBINUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key48"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key48"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    ZERO                TO  FLG-OK
           PERFORM UNTIL      (FLG-JYURRK  =   1 )
                           OR (FLG-OK      =   1 )
               IF      WRK-SRYKA       NOT =   ZERO
                   IF      WRK-SRYKA       =   JYURRK-SRYKA
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-IF
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key48"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key48"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-OK              =   ZERO
      *        受診無
               MOVE    "26"            TO  WRK-ERRCD
           END-IF
      *
           .
       2002411-JYURRK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセコメント追加処理
      *****************************************************************
       20022-RECECOM-INSERT-SEC         SECTION.
      *
      *    レセコメント検索
           PERFORM 2120-RECECOM-SEL-SEC
      *
      *    最大連番
           IF      WRK-RENNUM-MAX      =   99
               MOVE    "30"                TO  WRK-ERRCD
           ELSE
      *        追加
               MOVE    SPACE               TO  RECECOM-REC
               INITIALIZE                  RECECOM-REC
               MOVE    SPA-HOSPNUM         TO  RECECOM-HOSPNUM
               MOVE    SPA-PTID            TO  RECECOM-PTID
               MOVE    SPA-NYUGAIKBN       TO  RECECOM-NYUGAIKBN
               MOVE    WRK-SRYKA           TO  RECECOM-SRYKA
               MOVE    WRK-PERFORM-YM      TO  RECECOM-SRYYM
               MOVE    WRK-HKNCOMBI        TO  RECECOM-HKNCOMBI
               IF      WRK-RSI-HKNKBN      =   "3"
                   MOVE    WRK-PERFORM-DD      TO  RECECOM-SRYDD
               ELSE
                   MOVE    ZERO                TO  RECECOM-SRYDD
               END-IF
               MOVE    WRK-SJKBN           TO  RECECOM-SJKBN
               ADD     1                   TO  WRK-RENNUM-MAX
               MOVE    WRK-RENNUM-MAX      TO  RECECOM-RENNUM
               MOVE    WRK-COMMENT         TO  RECECOM-COMMENT
      *
               MOVE    SMCNDATE-YMD        TO  RECECOM-CREYMD
               MOVE    SMCNDATE-YMD        TO  RECECOM-UPYMD
               MOVE    SMCNDATE-HMS        TO  RECECOM-UPHMS
               MOVE    WRK-OPID            TO  RECECOM-OPID
      *
      *
               MOVE    RECECOM-REC         TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "RECECOM INS ERR:" MCP-RC
                           ",KEY:"   RECECOM-KEY    "."
                   MOVE    "51"            TO  WRK-ERRCD
               END-IF
      *
      *        連番
               MOVE    WRK-RENNUM-MAX      TO  SUBJETRES-NUMBER
           END-IF
           .
       20022-RECECOM-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトコメント検索処理
      *****************************************************************
       2120-RECECOM-SEL-SEC          SECTION.
      *
           MOVE    SPACE               TO  RECECOM-REC
           INITIALIZE                  RECECOM-REC
           MOVE    SPA-HOSPNUM         TO  RECECOM-HOSPNUM
           MOVE    SPA-PTID            TO  RECECOM-PTID
           MOVE    SPA-NYUGAIKBN       TO  RECECOM-NYUGAIKBN
           MOVE    WRK-SRYKA           TO  RECECOM-SRYKA
           MOVE    WRK-PERFORM-YM      TO  RECECOM-SRYYM
           MOVE    WRK-HKNCOMBI        TO  RECECOM-HKNCOMBI
           IF      WRK-RSI-HKNKBN      =   "3"
               MOVE    WRK-PERFORM-DD      TO  RECECOM-SRYDD
           ELSE
               MOVE    ZERO                TO  RECECOM-SRYDD
           END-IF
           MOVE    WRK-SJKBN           TO  RECECOM-SJKBN
      *
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 920-RECECOM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RECECOM
               INITIALIZE                  RECECOM-REC
           END-IF
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  WRK-RENNUM-MAX
           IF     (FLG-RECECOM         =   ZERO )
               MOVE    RECECOM-RENNUM      TO  WRK-RENNUM-MAX
            END-IF
      *
           .
       2120-RECECOM-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセコメント削除処理
      *****************************************************************
       2003-RECECOM-DELETE-SEC     SECTION.
      *
      *    患者情報内容編集
           PERFORM 20021-SUBJETRES-HEN-SEC
      *
      *    排他チェック & ロック処理
           PERFORM 990-LOCK-IN-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "90"            TO  WRK-ERRCD
               GO      TO              2003-RECECOM-DELET-EXT
           END-IF
      * 
           PERFORM 990-LOCK-OUT-SEC
           IF      SLOCK-RETURN    NOT =   ZERO
               DISPLAY "ロック解除失敗"
           END-IF
      *
      *    保険組合せ編集処理
           PERFORM 20023-HKNCOMBI-HEN-SEC
      *
      *    レセコメント削除処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20031-RECECOM-DELETE-SEC
           END-IF
      *
           .
       2003-RECECOM-DELET-EXT.
           EXIT.
      *****************************************************************
      *    レセコメント削除処理
      *****************************************************************
       20031-RECECOM-DELETE-SEC             SECTION.
      *
           MOVE    ZERO                TO  CNT-DELETE
      *    レセコメント検索
           MOVE    SPACE               TO  RECECOM-REC
           INITIALIZE                  RECECOM-REC
           MOVE    SPA-HOSPNUM         TO  RECECOM-HOSPNUM
           MOVE    SPA-PTID            TO  RECECOM-PTID
           MOVE    SPA-NYUGAIKBN       TO  RECECOM-NYUGAIKBN
           MOVE    WRK-SRYKA           TO  RECECOM-SRYKA
           MOVE    WRK-PERFORM-YM      TO  RECECOM-SRYYM
           MOVE    WRK-HKNCOMBI        TO  RECECOM-HKNCOMBI
           IF      WRK-RSI-HKNKBN      =   "3"
               MOVE    WRK-PERFORM-DD      TO  RECECOM-SRYDD
           ELSE
               MOVE    ZERO                TO  RECECOM-SRYDD
           END-IF
           MOVE    WRK-SJKBN           TO  RECECOM-SJKBN
      *
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 920-RECECOM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RECECOM
               INITIALIZE                  RECECOM-REC
           END-IF
      *
           PERFORM
                   UNTIL     ( FLG-RECECOM     =   1  )
      *
               PERFORM 200321-RECECOM-DEL-SEC
      *
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 920-RECECOM-READ-SEC
           END-PERFORM
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      CNT-DELETE          =   0
      *        削除なし
               MOVE    "17"                TO  WRK-ERRCD
           END-IF
      *
           .
       20031-RECECOM-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセコメント削除処理
      *****************************************************************
       200321-RECECOM-DEL-SEC             SECTION.
      *
           ADD     1                   TO  CNT-DELETE
      *
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC          NOT =   ZERO
               MOVE    "52"                TO  WRK-ERRCD
               DISPLAY "RECECOM DELETE ERR:" MCP-RC
                      ",KEY:"  RECECOM-KEY
           END-IF
      *
      *    削除内容編集
           IF     (CNT-DELETE          =   1    )
             AND  (WRK-ERRCD           =   SPACE)
               PERFORM 2003211-DELETE-HEN-SEC
           END-IF
           .
       200321-RECECOM-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセコメント取消内容編集処理
      *****************************************************************
       2003211-DELETE-HEN-SEC             SECTION.
      *
           MOVE    RECECOM-RENNUM          TO  SUBJETRES-NUMBER
           MOVE    RECECOM-COMMENT         TO  SUBJETRES-DETAIL-RECORD
           .
       2003211-DELETE-HEN-EXT.
           EXIT.
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
               MOVE    "患者番号が未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "02"
               MOVE    "診療科が未設定です"
                                              TO  WRK-ERRMSG
           WHEN    "03"
               MOVE    "症状詳記区分が未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "04"
               MOVE    "症状詳記内容が未設定です"
                                               TO  WRK-ERRMSG
           WHEN    "10"
               MOVE    "患者番号に該当する患者が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "診療日付が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "12"
               MOVE    "診療科が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE    "症状詳記区分が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "17"
               MOVE    "削除対象の症状詳記がありません"
                                               TO  WRK-ERRMSG
           WHEN    "20"
               MOVE    "アフターケアは診療日付を設定して下さい"
                                               TO  WRK-ERRMSG
      **   WHEN    "21"
      *        MOVE    "保険の一致する患者保険情報がありません"
      *                                        TO  WRK-ERRMSG
      *    WHEN    "22"
      *        MOVE    "公費の一致する患者公費情報がありません"
      *                                        TO  WRK-ERRMSG
      *    WHEN    "23"
      *        MOVE    "保険情報と一致する保険組合せがありません"
      ***                                      TO  WRK-ERRMSG
           WHEN    "25"
               MOVE    "保険組合せが存在しません。"
                                               TO  WRK-ERRMSG
           WHEN    "26"
               STRING  "該当の受診履歴がありません。"
                       "アフターケアの登録はできません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "30"
               STRING  "症状詳記の連番が９９件以上となります。"
                       "登録できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "51"
               MOVE    "症状詳記登録エラー"
                                               TO  WRK-ERRMSG
           WHEN    "52"
               MOVE    "症状詳記削除エラー"
                                               TO  WRK-ERRMSG
      *警告
           WHEN    "K1"
               MOVE    "診療年月を設定しました。"
                                               TO  WRK-ERRMSG
           WHEN    "K2"
               MOVE    "保険組合せを００００で設定しました。"
                                               TO  WRK-ERRMSG
           WHEN    "K3"
               MOVE    "期間外の保険組合せで登録しました。"
                                               TO  WRK-ERRMSG
           WHEN    "K4"
               MOVE    "症状詳記内容を変更しました。"
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
      *    受診履歴読込処理
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-JYURRK
               MOVE    MCPDATA-REC         TO  JYURRK-REC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
               INITIALIZE                      JYURRK-REC
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
      **   MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
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
