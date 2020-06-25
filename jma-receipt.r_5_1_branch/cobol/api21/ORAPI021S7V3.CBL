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
       PROGRAM-ID.         ORAPI021S7V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 排他制御　解除処理
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/09/XX    NACL−多々納　新規作成
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
           03  FLG-LOCK                PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
           03  FLG-SPATMP              PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
      *
           03  FLG-ERR                 PIC 9(01).
           03  FLG-SYORI               PIC 9(01).
           03  FLG-SYORIEND            PIC 9(01).
           03  FLG-CONTKBN             PIC 9(01).
           03  FLG-OK                  PIC 9(01).
      *
           03  FLG-DAYERR              PIC 9(01).
           03  FLG-CANSEL              PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
           03  FLG-ARI                 PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
      *
           03  IDX-ERR                 PIC 9(04).
           03  IDX-WAR                 PIC 9(04).
      *
      *    カウント領域
      *01  CNT-AREA.
      *
      *    一時領域
      *
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
      *
           03  WRK-YMD-G.
               05  WRK-YY-2            PIC X(02).
               05  WRK-DATE-6          PIC X(06).
      *    エラーコード
           03  WRK-ERRCD-G.
               05  WRK-ERRKBN              PIC X(01).
               05  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(200).
           03  WRK-ERRCD-2             PIC X(02).
           03  WRK-ERRMSG-1            PIC X(200).
      *
           03  WRK-OKMSG               PIC X(100).
      *
      *    オルカUID
           03  WRK-ORCA-UID            PIC X(36).
           03  WRK-OPID                PIC X(16).
      *
           03  HZN-SPA-OPID            PIC X(16).
      *
      *    03  WRK-MCP-PATHNAME        PIC X(62).
      *    03  WRK-MCP-WINDOW          PIC X(62).
      *    MCP-TERMID
           03  WRK-MCP-TERM            PIC X(64).
      *
           03  WRK-DEL-TERMID          PIC X(64).
           03  WRK-DEL-TERMID-Z        PIC X(64).
      *
           03  WRK-SDATE               PIC X(06).
           03  WRK-STIME.
               05  WRK-STHH            PIC 9(02).
               05  WRK-STMM            PIC 9(02).
               05  WRK-STSS            PIC 9(02).
      *
      *    共通パラメタ内容
       01  WRK-APIV3DATA-AREA.
      *    電子カルテUID
           03  WRK-KARTE-UID           PIC X(36).
      *    患者番号
           03  WRK-PARA-PTNUM          PIC X(20).
           03  WRK-APIV3DATA-REC.
      *        レスポンス番号
               05  WRK-RESPONSE-NUMBER     PIC X(02).
      *        処理ＰＧ
               05  WRK-PARA-SYORIPG        PIC X(02).
      *        確認コード
               05  WRK-SELECT-CODE         PIC X(04).
      *        削除
               05  WRK-DELETE-CLASS        PIC X(03).
      *        削除ＩＤ
               05  WRK-DEL-ORCA-UID        PIC X(64).
               05  WRK-DEL-KARTE-UID       PIC X(64).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *    全角
           03  WRK-HENYMD-Z        PIC X(22).
      *
      *    ＵＩＤ取得用エリア
       01  UIDIO-AREA.
           03  C-RET               PIC X(2).
           03  C-UID               PIC X(36).
       01  ST                      PIC 9(2).
      *
      *    MCPAREA 保存
       01  WRK-MCPAREA             PIC X(5000).
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
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
      *    COPY    "CPORCSPTNUM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    数字変換領域
      *    COPY    "CPORCSNUM.INC".
      *
      *    保険算定チェックサブ
      *    COPY    "CPORCSAGECHK.INC".
      *
      *    全角チェックパラメタ
      *    COPY    "CPORCSKANACHK.INC".
      *    全角チェックパラメタ（拡張漢字）
      *    COPY    "CPORCSKANACHK2.INC".
      *
      *    COPY    "CPORCXKANACONV.INC".
      *
      *    拡張文字対応
      *    COPY    "CPORCSSTRING.INC".
      *
      *    日付変換サブ
      *    COPY     "CPORCSGDAY.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPMDCXMLV3REQ7.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPMDCXMLV3RES7.INC".
      *
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *    処理終了パラメタ
      *    COPY    "CPORAPIV3SUB01.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム基本
      *01  SYSBASE-REC.
      *    COPY    "CPSYSBASE.INC".
      *
      *    システムユーザー
      *01  SYSUSER-REC.
      *    COPY    "CPSYSUSER.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    職員情報
           COPY    "CPSK1010.INC".
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK1009.INC".
      *
      *    排他制御情報（詳細）
           COPY  "CPLDMAP.INC".
      *    排他管理情報（詳細）
           COPY  "CPTBLLOCK.INC".
      *
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *    ＳＰＡ一時テーブル
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
      *    APIパラメタテーブル
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
      *
           COPY    "MCPDATA2.INC".
           COPY    "MCPDATA3.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "ORCA-BLOB".
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
           DISPLAY   "* medicalreq7v3 start*"
           DISPLAY   "**********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
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
               MOVE   404                  TO  APILST11-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
      *
           DISPLAY   "**********************"
           DISPLAY   "* medicalreq7v3 end  *"
           DISPLAY   "**********************"
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
      **** INITIALIZE                      WRK-XML-AREA
           INITIALIZE                      XML-MEDICALV3RES7
           INITIALIZE                      WRK-APIV3DATA-AREA
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Acceptance_Info"    TO  MDCRES-RESKEY
      *
           STRING  "A+"
                   MCP-USER            DELIMITED  BY  SIZE
                                       INTO    WRK-OPID
           END-STRING
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
           MOVE    ZERO                TO  MDCRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    ZERO                TO  IDX-WAR
           MOVE    ZERO                TO  IDX-ERR
      *
           MOVE    MDCREQ-REQUEST-NUMBER
                                       TO  MDCRES-REQUEST-NUMBER
           MOVE    MDCREQ-REQUEST-NUMBER
                                       TO  MDCRES-RESPONSE-NUMBER
      *    オルカＵＩＤ
           IF     (MDCREQ-ORCA-UID   NOT =   SPACE  )
             AND  (MDCREQ-REQUEST-NUMBER NOT =   "99" )
               PERFORM 2009-PARA-INIT-SEC
           END-IF
      *
           MOVE    ZERO                TO  FLG-SYORI
      *
           EVALUATE    MDCREQ-REQUEST-NUMBER
           WHEN    "00"
               MOVE    1                   TO  FLG-SYORI
           WHEN    "01"
               MOVE    2                   TO  FLG-SYORI
      **   WHEN    "99"
      *        強制処理終了
      *        PERFORM 2099-CANCEL-SEC
      *        MOVE    "00"                TO  MDCRES-RESPONSE-NUMBER
      ***      GO      TO      200-MAIN-EXT
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
               MOVE    "00"                TO  MDCRES-RESPONSE-NUMBER
           END-EVALUATE
      *
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               PERFORM 890-ERRCD-MSG-SEC
               IF      WRK-ERRCD           =   "08"
                   MOVE    "00"            TO  MDCRES-RESPONSE-NUMBER
               END-IF
      *        編集処理
               PERFORM 20010-INPUT-HEN-SEC
      *        パラメタ削除処理
               IF     (MDCREQ-ORCA-UID   NOT =   SPACE  )
      *            パラメタ削除処理
                   PERFORM 2902-PARA-APIDEL-SEC
                   MOVE    "00"            TO  MDCRES-RESPONSE-NUMBER
               END-IF
               GO      TO      200-MAIN-EXT
           END-IF
      *
           EVALUATE    MDCREQ-REQUEST-NUMBER
           WHEN    "00"
      *        一覧取得
               PERFORM 2001-SYORI-01-SEC
           WHEN    "01"
      *        解除処理
               PERFORM 2002-SYORI-02-SEC
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
           IF      APILST11-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-MEDICALV3REQ7
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalv3req7" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST11-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalv3req7"     TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "!!!XMLOPEN failure = " MCP-RC
               MOVE    "97"             TO  WRK-ERRCD
               GO      TO      1000-APTXMLREAD-EXT
           END-IF
      *
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_medicalv3req7" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST11-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicalv3req7"     TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-MEDICALV3REQ7
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE           TO  APILST11-BODY
           .
       1000-APTXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-RGTXMLMAKE-SEC   SECTION.
      *
           MOVE    SPACE               TO  WRK-OKMSG
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "検索処理終了"
                                           TO  WRK-OKMSG
                   WHEN    2
                       MOVE    "確認処理終了"
                                           TO  WRK-OKMSG
                   WHEN    3
                       MOVE    "解除処理終了"
                                           TO  WRK-OKMSG
                   WHEN    4
                       MOVE    "処理終了"
                                           TO  WRK-OKMSG
               END-EVALUATE
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               MOVE    ZERO            TO  WRK-ERRCD
               IF      IDX-WAR         >   ZERO
                   MOVE    "W"             TO  WRK-ERRKBN
               END-IF
               IF      WRK-ERRKBN          =   SPACE
                   MOVE    ZERO            TO  WRK-ERRKBN
               END-IF
               MOVE    WRK-ERRCD-G         TO  MDCRES-API-RESULT
               MOVE    WRK-OKMSG           TO  MDCRES-API-RESULT-MSG
           ELSE
      *        エラーメッセージ
               IF      WRK-ERRMSG          =   SPACE
                   PERFORM 890-ERRCD-MSG-SEC
               END-IF
               IF      WRK-ERRKBN          =   SPACE
                   MOVE    "E"                 TO  WRK-ERRKBN
               END-IF
               MOVE    WRK-ERRCD-G         TO  MDCRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  MDCRES-API-RESULT-MSG
           END-IF
      *
           IF      APILST11-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_medicalv3res7" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalv3res7"     TO  MDCRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALV3RES7
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"           TO  MCP-FUNC
           MOVE    "xml_medicalv3res7" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MDCRES-MODE
           MOVE    "medicalv3res7"     TO  MDCRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICALV3RES7
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               MOVE    MDCRES-OBJECT       TO  APILST11-BODY
               MOVE    "patientmod/xml"    TO  APILST11-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
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
      *****************************************************************
      *    ORCA-UID 初期処理
      *****************************************************************
       2009-PARA-INIT-SEC            SECTION.
      *
      *    UIDコードをTERMID
           MOVE    MDCREQ-ORCA-UID     TO  WRK-ORCA-UID
           MOVE    SPACE               TO  SPA-TERMID
           STRING  "API+"
                   WRK-ORCA-UID        DELIMITED  BY  SPACE
                                       INTO    SPA-TERMID
           END-STRING
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "AP21"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "API217V3"          TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
           PERFORM 9010-APIPARA-KEYREAD-SEC
      *
           IF      FLG-APIPARA          =   ZERO
               MOVE    APIPARA-DATA-REC    TO  WRK-APIV3DATA-REC
               MOVE    APIPARA-KARTE-UID   TO  WRK-KARTE-UID
           ELSE
               MOVE    SPACE               TO  WRK-APIV3DATA-AREA
               MOVE    "08"                TO  WRK-ERRCD
           END-IF
      *
           IF     (WRK-ERRCD               =   SPACE )
      *        同じ処理の続きであること
               IF      WRK-PARA-SYORIPG    NOT =   "07"
                   MOVE    "08"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
           .
       2009-PARA-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報設定内容チェック
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
      *    編集処理
           PERFORM 20010-INPUT-HEN-SEC
      *    必須入力チェック
           PERFORM 20010-INIT-CHECK-SEC
      *
      *    電子カルテＵＩＤチェック
           IF     (MDCREQ-ORCA-UID   NOT =   SPACE  )
             AND  (WRK-ERRCD           =   SPACE  )
               IF      MDCREQ-KARTE-UID  NOT =   WRK-KARTE-UID
      *            他端末使用中
                   MOVE    "90"            TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    継続区分
      *    IF      MDCREQ-CONT-MODE  =   "True"
      *        MOVE    1               TO  FLG-CONTKBN
      *    ELSE
      *        MOVE    ZERO            TO  FLG-CONTKBN
      *    END-IF
      *
      *    IF      WRK-ERRCD           =   SPACE
      *        オルカＵＩＤ処理
      *        PERFORM 20020-ORCAUID-SYORI-SEC
      *    END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    オルカＵＩＤ変換編集処理
      *****************************************************************
       2001-TERMID-HEN-SEC     SECTION.
      *
           MOVE    MDCREQ-ORCA-UID     TO  WRK-ORCA-UID
           MOVE    SPACE               TO  SPA-TERMID
           STRING  "API+"
                   WRK-ORCA-UID        DELIMITED  BY  SPACE
                                           INTO    SPA-TERMID
           END-STRING
      *
           .
       2001-TERMID-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目変換編集処理
      *****************************************************************
       20010-INPUT-HEN-SEC               SECTION.
      *
           MOVE    MDCREQ-KARTE-UID    TO  MDCRES-KARTE-UID
           MOVE    MDCREQ-ORCA-UID     TO  MDCRES-ORCA-UID
      *
           .
       20010-INPUT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    必須入力チェック処理
      *****************************************************************
       20010-INIT-CHECK-SEC    SECTION.
      *
      *    UID（電子カルテ）
           IF      MDCREQ-KARTE-UID  =   SPACE
               IF     (WRK-ERRCD           =   SPACE  )
                   MOVE    "06"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    オルカＵＩＤ
           IF      MDCREQ-ORCA-UID   =   SPACE
               IF     (MDCREQ-REQUEST-NUMBER   =   "01" )
                 AND  (MDCREQ-SELECT-ANSWER    NOT =   SPACE )
                   MOVE    "07"                TO  WRK-ERRCD
               END-IF
           END-IF
           .
       20010-INIT-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＵＩＤコード取得処理
      *****************************************************************
       20020-ORCAUID-SYORI-SEC        SECTION.
      *
      *    UIDコード処理
           IF      MDCREQ-ORCA-UID   =   SPACE
      *        UIDコード取得処理
               PERFORM 1001-UID-GET-SEC
               MOVE    C-UID               TO  WRK-ORCA-UID
           ELSE
               MOVE    MDCREQ-ORCA-UID   TO  WRK-ORCA-UID
           END-IF
      *    UIDコードをTERMID
           MOVE    SPACE               TO  SPA-TERMID
           STRING  "API+"
                   WRK-ORCA-UID        DELIMITED  BY  SPACE
                                           INTO    SPA-TERMID
           END-STRING
      *
           MOVE    WRK-ORCA-UID        TO  MDCRES-ORCA-UID
      *
           .
       20020-ORCAUID-SYORI-EXT.
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
      *    排他制御一覧取得処理
      *****************************************************************
       2001-SYORI-01-SEC        SECTION.
      *
      *    排他中一覧
           PERFORM 20011-LOCKLIST-SYORI-SEC
      *
           IF      IDX                 =   ZERO
      *        排他中なし
               MOVE    "10"            TO  WRK-ERRCD
               MOVE    "00"            TO  MDCRES-RESPONSE-NUMBER
           ELSE
               MOVE    "00"            TO  MDCRES-RESPONSE-NUMBER
           END-IF
      *
           .
       2001-SYORI-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他情報一覧編集処理
      *****************************************************************
       20011-LOCKLIST-SYORI-SEC              SECTION.
      *
           INITIALIZE                      MDCRES-LOCK-LIST-G
      *
           MOVE    SPACE               TO  TBL-LOCK
           INITIALIZE                      TBL-LOCK
           MOVE    SPA-HOSPNUM         TO  LOCK-HOSPNUM
           MOVE    TBL-LOCK            TO  MCPDATA-REC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_lock"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-LOCK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-LOCK
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL  (FLG-LOCK        =   1 )  OR
                          (IDX             >=  100 )
               IF      LOCK-PTID (1)       =   SPACE
                   CONTINUE
               ELSE
                   PERFORM 20011-LOCK-HEN-SEC
               END-IF
      *
               MOVE    "tbl_lock"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-LOCK-READ-SEC
           END-PERFORM
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       20011-LOCKLIST-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他情報編集処理
      *****************************************************************
       20011-LOCK-HEN-SEC              SECTION.
      *
           ADD     1                   TO  IDX
           MOVE    LOCK-TERMID         TO  MDCRES-LOCK-TERMID-UID
                                                            (IDX)
      *    API+ は削除する
           IF     (LOCK-KARTE-UID  NOT =   SPACE )
            AND   (LOCK-TERMID(1:4)    =   "API+")
               MOVE    LOCK-TERMID(5:)     TO  MDCRES-LOCK-TERMID-UID
                                                            (IDX)
           END-IF
           MOVE    LOCK-KARTE-UID      TO  MDCRES-LOCK-KARTE-UID
                                                            (IDX)
           MOVE    LOCK-OPID           TO  MDCRES-LOCK-OPID (IDX)
      *
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    LOCK-PTID (1)       TO  SPTID-PTID
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF      SPTID-RC            =   ZERO
               MOVE    SPTID-PTNUM     TO  MDCRES-LOCK-PATIENTID
                                                              (IDX)
      *        氏名検索
               INITIALIZE                      PTINF-REC
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    LOCK-PTID (1)       TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               MOVE    PTINF-NAME      TO  MDCRES-LOCK-NAME   (IDX)
           ELSE
               MOVE    SPACE           TO  MDCRES-LOCK-PATIENTID
                                                              (IDX)
           END-IF
      *
           MOVE    LOCK-LDNAME (1)     TO  MDCRES-LOCK-LDNAME (IDX)
      *    排他日
           IF      LOCK-STARTDATE (1)  =   SPACE
               CONTINUE
           ELSE
               MOVE    "20"                TO  WRK-YY-2
               MOVE    LOCK-STARTDATE (1)  TO  WRK-DATE-6
               MOVE    WRK-YMD-G           TO  WRK-SYMD
               MOVE    LOCK-STARTTIME (1)  TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  MDCRES-LOCK-DATE(IDX)
               MOVE    WRK-HEN-TIME        TO  MDCRES-LOCK-TIME(IDX)
           END-IF
           .
       20011-LOCK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    APIパラメタ登録処理
      *****************************************************************
       2902-APIPARA-INS-SEC        SECTION.
      *
      *    パラメタ削除処理
           PERFORM 2902-PARA-APIDEL-SEC
      *
      *    登録
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "AP21"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "API217V3"          TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
      *    レスポンス番号・電子カルテUID保存
           MOVE    MDCREQ-KARTE-UID    TO  APIPARA-KARTE-UID
           MOVE    SPACE               TO  APIPARA-PTNUM
      *
           MOVE    SPACE               TO  WRK-APIV3DATA-REC
           MOVE    MDCRES-RESPONSE-NUMBER
                                       TO  WRK-RESPONSE-NUMBER
      *    処理ＰＧ
           MOVE    "07"                TO  WRK-PARA-SYORIPG
      *
           MOVE    MDCRES-SELECT-CODE
                                       TO  WRK-SELECT-CODE
           MOVE    MDCRES-DEL-ORCA-UID
                                       TO  WRK-DEL-ORCA-UID
           MOVE    MDCRES-DEL-KARTE-UID
                                       TO  WRK-DEL-KARTE-UID
           MOVE    MDCRES-DELETE-CLASS
                                       TO  WRK-DELETE-CLASS
      *
           MOVE    WRK-APIV3DATA-REC   TO  APIPARA-DATA-REC
      *
           MOVE    SMCNDATE-YMD        TO  APIPARA-UPYMD
           MOVE    SMCNDATE-HMS        TO  APIPARA-UPHMS
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    "80"                TO  WRK-ERRCD
               DISPLAY "API PARA INSERT ERR:"  MCP-RC
                       ",KEY:" PARA-KEY
           END-IF
           .
       2902-APIPARA-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新確認処理
      *****************************************************************
       2002-SYORI-02-SEC     SECTION.
      *
           MOVE    SPACE               TO  MDCRES-RESPONSE-NUMBER
      *
           MOVE    MDCREQ-DEL-KARTE-UID    TO  MDCRES-DEL-KARTE-UID
           MOVE    MDCREQ-DEL-ORCA-UID     TO  MDCRES-DEL-ORCA-UID
           MOVE    MDCREQ-DELETE-CLASS     TO  MDCRES-DELETE-CLASS
      *
      *    オルカＵＩＤ
           IF      MDCREQ-SELECT-ANSWER    =   SPACE
      *        選択確認
               EVALUATE    MDCREQ-DELETE-CLASS
               WHEN    "All"
      *            全部
                   PERFORM 20023-KOUSIN-ALLCHK-SEC
               WHEN    SPACE
      *            選択
                   PERFORM 20021-KOUSIN-CHK-SEC
               WHEN    OTHER
                   MOVE    "15"            TO  WRK-ERRCD
               END-EVALUATE
      *
               IF      WRK-ERRCD           =   SPACE
                   MOVE    "01"            TO  MDCRES-RESPONSE-NUMBER
                   IF      MDCREQ-DELETE-CLASS     =   "All"
                       MOVE    "0002"          TO  MDCRES-SELECT-CODE
                   ELSE
                       MOVE    "0001"          TO  MDCRES-SELECT-CODE
                   END-IF
                   PERFORM 292-SELECT-HEN-SEC
      *            オルカＵＩＤ処理
                   PERFORM 20020-ORCAUID-SYORI-SEC
      *            APIパラメタ登録
                   PERFORM 2902-APIPARA-INS-SEC
               END-IF
           ELSE
      *        パラメタ確認
               IF     (WRK-RESPONSE-NUMBER =   "01" )
                  AND (WRK-SELECT-CODE NOT =   SPACE )
                   PERFORM 20021-ANSWER-SYORI-SEC
               ELSE
                   MOVE    "09"                TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *
           IF     (WRK-ERRCD           =   SPACE   )
      *        更新処理終了
             AND  (MDCRES-RESPONSE-NUMBER  =   SPACE )
               MOVE    "00"            TO  MDCRES-RESPONSE-NUMBER
      *        パラメタ削除処理
               PERFORM 2902-PARA-APIDEL-SEC
               MOVE    SPACE               TO  MDCRES-ORCA-UID
           ELSE
               IF     (MDCRES-RESPONSE-NUMBER  =   SPACE )
                   MOVE    "01"            TO  MDCRES-RESPONSE-NUMBER
      *            パラメタ削除処理
                   PERFORM 2902-PARA-APIDEL-SEC
                   MOVE    SPACE               TO  MDCRES-ORCA-UID
               END-IF
           END-IF
      *
      *    排他中一覧
           PERFORM 20011-LOCKLIST-SYORI-SEC
      *
           .
       2002-SYORI-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新チェック処理
      *****************************************************************
       20021-ANSWER-SYORI-SEC         SECTION.
      *
           EVALUATE    WRK-SELECT-CODE
           WHEN    "0001"
      *        削除確認
               EVALUATE    MDCREQ-SELECT-ANSWER
               WHEN    "Ok"
                   IF      (WRK-DEL-ORCA-UID   =   MDCREQ-DEL-ORCA-UID)
                      AND  (WRK-DEL-KARTE-UID  =   MDCREQ-DEL-KARTE-UID)
                       PERFORM 20021-KOUSIN-CHK-SEC
      *                更新処理
                       IF     (WRK-ERRCD           =   SPACE )
                           PERFORM 20022-KOUSIN-SYORI-SEC
                           IF      WRK-ERRCD           =   SPACE
      *                        削除処理終了
                               MOVE    3               TO  FLG-SYORI
                           END-IF
                       END-IF
                   ELSE
                       MOVE    "11"                TO  WRK-ERRCD
                   END-IF
               WHEN    "No"
                   MOVE    SPACE               TO  MDCRES-SELECT-CODE
                   MOVE    SPACE               TO  MDCRES-DEL-ORCA-UID
                   MOVE    SPACE               TO  MDCRES-DEL-KARTE-UID
                   MOVE    SPACE               TO  MDCRES-DELETE-CLASS
                   MOVE    4                   TO  FLG-SYORI
               WHEN    OTHER
                   MOVE    "12"                TO  WRK-ERRCD
               END-EVALUATE
           WHEN    "0002"
      *        全削除確認
               EVALUATE    MDCREQ-SELECT-ANSWER
               WHEN    "Ok"
                   IF      (WRK-DELETE-CLASS   =   MDCREQ-DELETE-CLASS)
                       PERFORM 20023-KOUSIN-ALLCHK-SEC
      *                更新処理
                       IF     (WRK-ERRCD           =   SPACE )
                           PERFORM 20024-ALLKOUSIN-SYORI-SEC
                           IF      WRK-ERRCD           =   SPACE
      *                        削除処理終了
                               MOVE    3               TO  FLG-SYORI
                           END-IF
                       END-IF
                   ELSE
                       MOVE    "11"                TO  WRK-ERRCD
                   END-IF
               WHEN    "No"
                   MOVE    SPACE               TO  MDCRES-SELECT-CODE
                   MOVE    SPACE               TO  MDCRES-DEL-ORCA-UID
                   MOVE    SPACE               TO  MDCRES-DEL-KARTE-UID
                   MOVE    SPACE               TO  MDCRES-DELETE-CLASS
                   MOVE    4                   TO  FLG-SYORI
               WHEN    OTHER
                   MOVE    "12"                TO  WRK-ERRCD
               END-EVALUATE
           WHEN    SPACE
               MOVE    "12"                TO  WRK-ERRCD
           END-EVALUATE
      *
           .
       20021-ANSWER-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新チェック処理
      *****************************************************************
       20021-KOUSIN-CHK-SEC         SECTION.
      *
           IF      MDCREQ-DEL-KARTE-UID    =   SPACE
               MOVE    MDCREQ-DEL-ORCA-UID TO  WRK-DEL-TERMID
           ELSE
      *        API+ 編集
               MOVE    SPACE               TO  WRK-DEL-TERMID
               STRING  "API+"
                       MDCREQ-DEL-ORCA-UID
                                           DELIMITED  BY  SPACE
                                           INTO    WRK-DEL-TERMID
               END-STRING
           END-IF
      *    LOCKテーブル検索
           MOVE    SPACE               TO  TBL-LOCK
           INITIALIZE                      TBL-LOCK
           MOVE    SPA-HOSPNUM         TO  LOCK-HOSPNUM
           MOVE    WRK-DEL-TERMID      TO  LOCK-TERMID
           MOVE    TBL-LOCK            TO  MCPDATA-REC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_lock"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-LOCK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-LOCK
           END-IF
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (FLG-LOCK            =   ZERO )
              AND (MDCREQ-DEL-KARTE-UID    =   LOCK-KARTE-UID)
      *        削除対象チェック
               PERFORM 2311-TIME-CHK-SEC
               IF      FLG-ERR             =   1
                   MOVE    "14"            TO  WRK-ERRCD
               END-IF
           ELSE
               MOVE    "13"                TO  WRK-ERRCD
           END-IF
      *
           .
       20021-KOUSIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除対象チェック処理
      *****************************************************************
       2311-TIME-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
           ACCEPT  WRK-SDATE           FROM  DATE
           ACCEPT  WRK-STIME           FROM  TIME
      *    現在の時間から１分前とする
           IF      WRK-STMM             >=  1
               COMPUTE WRK-STMM            =   WRK-STMM    -  1
           ELSE
               MOVE    59                  TO  WRK-STMM
               COMPUTE WRK-STHH            =   WRK-STHH    -  1
           END-IF
      *
           PERFORM VARYING    IDZ      FROM    1   BY  1
                   UNTIL     (IDZ          >   50   )
                         OR  (FLG-ERR  NOT =   ZERO )
                         OR  (LOCK-STARTDATE(IDZ)  =   SPACE)
               IF      WRK-SDATE           =   LOCK-STARTDATE(IDZ)
                   IF      WRK-STIME       <   LOCK-STARTTIME(IDZ)
                       MOVE    1               TO  FLG-ERR
                   END-IF
               END-IF
           END-PERFORM
           .
       2311-TIME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       20022-KOUSIN-SYORI-SEC         SECTION.
      *
      *    LOCKテーブル検索
           MOVE    SPACE               TO  TBL-LOCK
           INITIALIZE                      TBL-LOCK
           MOVE    SPA-HOSPNUM         TO  LOCK-HOSPNUM
           MOVE    WRK-DEL-TERMID      TO  LOCK-TERMID
           MOVE    TBL-LOCK            TO  MCPDATA-REC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_lock"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-LOCK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-LOCK
           END-IF
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-LOCK            =   ZERO
      *        データ削除
               PERFORM 200221-DELETE-SYORI-SEC
           END-IF
      *
           .
       20022-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ削除処理
      *****************************************************************
       200221-DELETE-SYORI-SEC         SECTION.
      *
           MOVE    TBL-LOCK            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "20"                TO  WRK-ERRCD
               GO      TO      200221-DELETE-SYORI-EXT
           END-IF
      *
           IF     (LOCK-KARTE-UID      =   SPACE )
            AND   (LOCK-TERMID         =   SPACE )
               GO      TO      200221-DELETE-SYORI-EXT
           END-IF
      *
      *    端末ＩＤ(オンライン）
           MOVE    SPACE               TO  WRK-DEL-TERMID-Z
           IF     (LOCK-KARTE-UID      =   SPACE )
            AND   (WRK-DEL-TERMID  NOT =   SPACE )
               MOVE    WRK-DEL-TERMID      TO  WRK-DEL-TERMID-Z
               PERFORM VARYING IDX     FROM    64  BY  -1
                       UNTIL   IDX     <       1
                   IF      WRK-DEL-TERMID-Z(IDX:1)  NOT =   SPACE
                       MOVE    1               TO  IDX 
                   ELSE
                       MOVE    "Z"             TO  WRK-DEL-TERMID-Z
                                                            (IDX:1)
                   END-IF
               END-PERFORM
           END-IF
      *
      *    一時データ削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    LOCK-TERMID         TO  PARA-TERMID
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del4"              TO  MCP-PATHNAME
           MOVE    "tbl_para"          TO  MCP-TABLE
           PERFORM 900-ORCDBMAIN-SEC
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    WRK-DEL-TERMID      TO  SPATMP-TERMID
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del4"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
      *
      *    APIパラメタ削除
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    WRK-DEL-TERMID      TO  APIPARA-TERMID
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "del6"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
      *    端末ＩＤ(オンライン）
           IF      LOCK-KARTE-UID      =   SPACE
      *        一時データ削除
               INITIALIZE                      PARA-REC
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    WRK-DEL-TERMID-Z    TO  PARA-TERMID
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "del4"              TO  MCP-PATHNAME
               MOVE    "tbl_para"          TO  MCP-TABLE
               PERFORM 900-ORCDBMAIN-SEC
      *
               INITIALIZE                      SPATMP-REC
               MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
               MOVE    WRK-DEL-TERMID-Z    TO  SPATMP-TERMID
               MOVE    SPATMP-REC          TO  MCPDATA2-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "del4"              TO  MCP-PATHNAME
               PERFORM 910-SPATMP-CALL-SEC
           END-IF
      *
           .
       200221-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    全削除更新チェック処理
      *****************************************************************
       20023-KOUSIN-ALLCHK-SEC         SECTION.
      *
           MOVE    SPACE               TO  TBL-LOCK
           INITIALIZE                      TBL-LOCK
           MOVE    SPA-HOSPNUM         TO  LOCK-HOSPNUM
           MOVE    TBL-LOCK            TO  MCPDATA-REC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_lock"          TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 910-LOCK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-LOCK
           END-IF
           MOVE    ZERO                TO  FLG-ERR
           PERFORM UNTIL  (FLG-LOCK        =   1 )  OR
                          (FLG-ERR         =   1 )
               PERFORM 2311-TIME-CHK-SEC
      *
               MOVE    "tbl_lock"          TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 910-LOCK-READ-SEC
           END-PERFORM
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-ERR             =   1
              MOVE    "14"             TO  WRK-ERRCD
           END-IF
      *
           .
       20023-KOUSIN-ALLCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括削除処理
      *****************************************************************
       20024-ALLKOUSIN-SYORI-SEC            SECTION.
      *
           MOVE    SPACE               TO  TBL-LOCK
           INITIALIZE                      TBL-LOCK
           MOVE    SPA-HOSPNUM         TO  LOCK-HOSPNUM
           MOVE    TBL-LOCK            TO  MCPDATA-REC
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_lock"          TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 910-LOCK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-LOCK
           END-IF
           PERFORM UNTIL  (FLG-LOCK        =   1 )
                      OR  (WRK-ERRCD   NOT =   SPACE )
      *        端末ＩＤ(オンライン）
               MOVE    LOCK-TERMID     TO  WRK-DEL-TERMID
      *        データ削除
               PERFORM 200221-DELETE-SYORI-SEC
      *
               MOVE    "tbl_lock"          TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 910-LOCK-READ-SEC
           END-PERFORM
           MOVE    "tbl_lock"          TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       20024-ALLKOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ編集処理
      *****************************************************************
       292-SELECT-HEN-SEC            SECTION.
      *
           MOVE    SPACE               TO  MDCRES-SELECT-MSG
           EVALUATE    MDCRES-SELECT-CODE
               WHEN    "0001"
                   STRING  "選択された排他情報を削除します。"
                           "端末が展開中でないことを確認して下さい。"
                                           DELIMITED  BY  SIZE
                                           INTO    MDCRES-SELECT-MSG
                   END-STRING
               WHEN    "0002"
                   STRING  "排他情報をすべて削除します。"
                           "端末が展開中でないことを確認して下さい。"
                                           DELIMITED  BY  SIZE
                                           INTO    MDCRES-SELECT-MSG
                   END-STRING
           END-EVALUATE
      *
      *    確認あり
      **** MOVE    "True"              TO  MDCRES-SELECT-FLAG
      *
           MOVE    "40"                TO  WRK-ERRCD
           MOVE    "S"                 TO  WRK-ERRKBN
           .
       292-SELECT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ削除処理
      *****************************************************************
       2902-PARA-APIDEL-SEC          SECTION.
      *
      *    APIパラメタ削除
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "AP21"              TO  APIPARA-GYOUMUID
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "APIPARA DELETE ERR:"  MCP-RC
                           ",KEY:" APIPARA-KEY
           END-IF
      *
           .
       2902-PARA-APIDEL-EXT.
           EXIT.
      *****************************************************************
      *    日付暦日チェック
      *****************************************************************
       803-ORCSDAY-CHEK-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-DAYERR
           EVALUATE    WRK-SYMD
               WHEN    SPACE
                   MOVE    SPACE               TO  WRK-HENYMD
               WHEN    ZERO
                   MOVE    "00000000"          TO  WRK-HENYMD
               WHEN    "99999999"
                   MOVE    "99999999"          TO  WRK-HENYMD
               WHEN    OTHER
      *            日付チェックサブ
                   MOVE    SPACE               TO  STS-AREA-DAY
                   INITIALIZE                      STS-AREA-DAY
                   MOVE    SPACE               TO  LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                   IF      STS-DAY-RC1     NOT =   ZERO
                       MOVE    1                   TO  FLG-DAYERR
                   ELSE
                       MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
                   END-IF
           END-EVALUATE
      *
           .
       803-ORCSDAY-CHEK-EXT.
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
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
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
      *                                        TO  WRK-ERRMSG
           WHEN    "06"
               MOVE    "電子カルテＵＩＤが未設定です。"
                                               TO  WRK-ERRMSG
           WHEN    "07"
               MOVE    "オルカＵＩＤが未設定です。"
                                               TO  WRK-ERRMSG
           WHEN    "08"
               STRING  "初回接続が完了していません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "09"
               STRING  "処理の順番が違います。"
                       "正しい順で処理を行って下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "10"
               MOVE    "排他中の情報はありません。"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "削除対象のＵＩＤが前回と違います。"
                                               TO  WRK-ERRMSG
           WHEN    "12"
               STRING  "選択結果エラーです。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "13"
               MOVE    "排他情報が存在しません。"
                                               TO  WRK-ERRMSG
           WHEN    "14"
               STRING  "端末展開中と思われる排他時間です。"
                       "端末が展開中でないことを確認して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "15"
               MOVE    "削除区分が違います。"
                                               TO  WRK-ERRMSG
           WHEN    "20"
               STRING  "削除エラーです。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "40"
               STRING  "選択項目があります。"
                       "選択結果を返却してください。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
      *
           WHEN    "80"
               STRING  "一時データ出力エラー"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "81"
               STRING  "解除対象がありません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
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
               MOVE    "リクエスト番号不正"    TO  WRK-ERRMSG
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
      *    管理マスター読込処理
      *****************************************************************
       910-LOCK-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-LOCK
               MOVE    MCPDATA-REC         TO  TBL-LOCK
           ELSE
               MOVE    1                   TO  FLG-LOCK
               INITIALIZE                      TBL-LOCK
           END-IF
      *
           .
       910-LOCK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEYREAD-SEC      SECTION.
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
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
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
      *****************************************************************
      *    ＰＡＲＡデータ検索処理
      *****************************************************************
       9010-PARA-KEYREAD-SEC         SECTION.
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9010-PARA-KEYREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＡＲＡデータ検索処理
      *****************************************************************
       9010-APIPARA-KEYREAD-SEC         SECTION.
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_api_para"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBFETCH"           TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  APIPARA-REC
                   MOVE    ZERO            TO  FLG-APIPARA
               ELSE
                   INITIALIZE                  APIPARA-REC
                   MOVE    1               TO  FLG-APIPARA
               END-IF
           ELSE
               INITIALIZE                  APIPARA-REC
               MOVE    1               TO  FLG-APIPARA
           END-IF
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9010-APIPARA-KEYREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブル読み込み
      *****************************************************************
       9100-SPATMP-KEYREAD-SEC        SECTION.
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBFETCH"           TO  MCP-FUNC
               PERFORM 910-SPATMP-CALL-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA2-REC    TO  SPATMP-REC
                   MOVE    ZERO            TO  FLG-SPATMP
               ELSE
                   INITIALIZE                  SPATMP-REC
                   MOVE    1               TO  FLG-SPATMP
               END-IF
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
      *
           .
       9100-SPATMP-KEYREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブル検索処理
      *****************************************************************
       910-SPATMP-CALL-SEC                SECTION.
      *
           MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
      *
           CALL    "ORCDBSPATMP"       USING   MCPAREA
                                               MCPDATA2-REC
                                               SPA-AREA
      *
           .
       910-SPATMP-CALL-EXT.
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
