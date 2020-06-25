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
       PROGRAM-ID.         ORAPI101R1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : システム管理　診療科・ドクター一覧取得処理
      *  管理者            :
      *  作成日付    作業者        記述
      *  12/07/XX    NACL−多々納　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.07.00     NACL-多々納  14/05/16  リクエスト番号対応
      *                                     医療機関情報取得追加
      * 05.00.00     NACL-藤原    18/11/07  項目追加
      *****************************************************************
      *
       ENVIRONMENT           DIVISION.
       CONFIGURATION         SECTION.
       INPUT-OUTPUT          SECTION.
      *
       FILE-CONTROL.
       DATA                DIVISION.
      *FILE                    SECTION.
      *
       WORKING-STORAGE             SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
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
           03  WRK-MCP-PATHNAME        PIC X(64).
           03  WRK-ZZ9                 PIC ZZZ9.
      *
           03  WRK-CNT                 PIC 9(04).
      *    リクエスト番号
           03  WRK-REQUEST-NUMBER      PIC X(02).
      *
           03  WRK-NUM-4-X.
               05  WRK-NUM-4           PIC 9(04).
      *
       01  WRK-XML-AREA.
           03  WRK-IN-DATE                PIC X(10).
      *
           03  WRK-IN-YMD                 PIC X(08).
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPAPISYSTEM01V2REQ.INC".
      *    API XML書き込み用定義体（診療科）
           COPY    "CPDEPARTMENTV2RES.INC".
      *    API XML書き込み用定義体（ドクター）
           COPY    "CPPHYSICIANV2RES.INC".
      *
      *    API XML書き込み用定義体（医療機関情報）
            COPY    "CPAPISYS1001V2RES.INC".
      *
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
      *    職員情報
           COPY  "CPSK1010.INC".
      *
      *    診療内容情報
           COPY  "CPSK1005.INC".
      *
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *
           COPY  "CPSK1002.INC".
      *
           COPY  "CPSK1003.INC".
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
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "************************"
           DISPLAY   "* system01_manage start*"
           DISPLAY   "************************"
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
      *
           IF      WRK-ERRCD           =   "99"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST5-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-PTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "***********************"
           DISPLAY   "* system01_manage end *"
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
           INITIALIZE                      XML-DEPARTMENTRES
           INITIALIZE                      XML-PHYSICIANRES
           INITIALIZE                      XML-SYSTEM1001RES
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Patient Info"       TO  DEPTRES-RESKEY
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  DEPTRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  DEPTRES-INFORMATION-TIME
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
           MOVE    ZERO                TO  DEPTRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD
      *
           MOVE    ZERO                TO  FLG-SYORI
      *
      *    リクエスト番号があれば優先
           IF      SYS01REQ-REQUEST-NUMBER >=  "01"   AND
                                           <=  "99"
               MOVE    SYS01REQ-REQUEST-NUMBER
                                           TO  WRK-REQUEST-NUMBER
           ELSE
               MOVE    APILST5-CLASS       TO  WRK-REQUEST-NUMBER
           END-IF
      *
      **** EVALUATE    APILST5-CLASS
           EVALUATE    WRK-REQUEST-NUMBER
           WHEN    "01"
      *        診療科一覧
               MOVE    1                   TO  FLG-SYORI
           WHEN    "02"
      *        ドクター一覧
               MOVE    2                   TO  FLG-SYORI
           WHEN    "03"
      *        職員一覧
               MOVE    3                   TO  FLG-SYORI
           WHEN    "04"
      *        医療機関情報
               MOVE    4                   TO  FLG-SYORI
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
           END-EVALUATE
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
      *    一覧処理
           IF      WRK-ERRCD           =   SPACE
      *******  EVALUATE    APILST5-CLASS
               EVALUATE    WRK-REQUEST-NUMBER
               WHEN    "01"
      *            診療科一覧
                   PERFORM 2002-SRYKA-LIST-SEC
               WHEN    "02"
      *            ドクター一覧
                   PERFORM 2003-DRCD-LIST-SEC
               WHEN    "03"
      *            職員一覧
                   PERFORM 2004-SYOCD-LIST-SEC
               WHEN    "04"
      *            医療機関情報
                   PERFORM 2005-SYS1001-LIST-SEC
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
       1000-APTLSTREAD-SEC   SECTION.
      *
           IF      APILST5-BODY        NOT =   LOW-VALUE
               DISPLAY "SYS01REQ-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-APIREQ
      * XML情報取り出し
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_system01_managev2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST5-BODY        TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "system01_managereq"    TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
               MOVE   "97"             TO  WRK-ERRCD
               GO     TO      1000-APTLSTREAD-EXT
           END-IF
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_system01_managev2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "system01_managereq"    TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ          TO  XML-SYSTEM01MANAGEREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               MOVE   "98"             TO  WRK-ERRCD
               DISPLAY "XMLREAD failure = " MCP-RC
           END-IF
      *
           .
       1000-APTLSTREAD-EXT.
           EXIT.
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
      *    設定内容チェック
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
           MOVE    SYS01REQ-BASE-DATE  TO  WRK-IN-DATE
      *    処理日
           MOVE    SYS01REQ-BASE-DATE  TO  WRK-HEN-DATE
           MOVE    SPACE               TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD            TO  WRK-IN-YMD
      *    開始日未設定はシステム日付
           IF      WRK-IN-DATE         =   SPACE
               MOVE    SPA-SYSYMD          TO  WRK-IN-YMD
               MOVE    WRK-IN-YMD          TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  WRK-IN-DATE
           END-IF
           MOVE    WRK-IN-DATE         TO  DEPTRES-BASE-DATE
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科一覧処理
      *****************************************************************
       2002-SRYKA-LIST-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                      SYS-1005-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-IN-YMD          TO  SYS-1005-STYUKYMD
           MOVE    WRK-IN-YMD          TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM
                   UNTIL       (WRK-ERRCD  NOT =   SPACE)  OR
                               (FLG-SYSKANRI   =   1    )
               ADD     1                   TO  IDX
               IF      IDX                 >   100
                   MOVE    "10"                TO  WRK-ERRCD
               ELSE
                   MOVE    SYSKANRI-REC        TO  SYS-1005-REC
                   MOVE    SYS-1005-KBNCD(1:2) TO  DEPTRES-DEP-CODE
                                                                  (IDX)
                   MOVE    SYS-1005-SRYKANAME  TO  DEPTRES-SRYKANAME
                                                                  (IDX)
                   MOVE    SYS-1005-SRYKANAME1 TO  DEPTRES-SRYKANAME1
                                                                  (IDX)
                   MOVE    SYS-1005-SRYKANAME2 TO  DEPTRES-SRYKANAME2
                                                                  (IDX)
                   MOVE    SYS-1005-SRYKANAME3 TO  DEPTRES-SRYKANAME3
                                                                  (IDX)
                   MOVE    SYS-1005-RECESRYKA  TO  DEPTRES-RECESRYKA
                                                                  (IDX)
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX                 =   ZERO
               MOVE    "11"            TO  WRK-ERRCD
           END-IF
           .
       2002-SRYKA-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター一覧処理
      *****************************************************************
       2003-DRCD-LIST-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WRK-IN-YMD          TO  SYS-1010-STYUKYMD
           MOVE    WRK-IN-YMD          TO  SYS-1010-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM
                   UNTIL       (WRK-ERRCD  NOT =   SPACE)  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
               IF      SYS-1010-KBNCD(1:1) =   "1"
      *            ドクターのみ
                   ADD     1                   TO  IDX
                   IF      IDX                 >   100
                       MOVE    "10"                TO  WRK-ERRCD
                   ELSE
                       MOVE    SYS-1010-KBNCD(1:5) TO  PHYSRES-DRCD
                                                                 (IDX)
      *                氏名
                       MOVE    SYS-1010-NAME       TO  PHYSRES-NAME
                                                                 (IDX)
                       MOVE    SYS-1010-KANANAME   TO  PHYSRES-KANANAME
                                                                 (IDX)
      *                医籍登録番号
                       MOVE    SYS-1010-ISEKINUM   TO  PHYSRES-ISEKINUM
                                                                 (IDX)
      *                麻薬ライセンス
                       MOVE    SYS-1010-MAYAKULICENSE
                                               TO  PHYSRES-MAYAKULICENSE
                                                                 (IDX)
      *                専門科コード
                       PERFORM VARYING    IDY      FROM    1   BY  1
                               UNTIL      IDY      >   5
                           MOVE    SYS-1010-SEMONKACD-X (IDY)
                                               TO  PHYSRES-SEMONKACD-X
                                                              (IDX IDY)
                       END-PERFORM
                   END-IF
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX                 =   ZERO
               MOVE    "11"            TO  WRK-ERRCD
           END-IF
           .
       2003-DRCD-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    職員一覧処理
      *****************************************************************
       2004-SYOCD-LIST-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                      SYS-1010-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WRK-IN-YMD          TO  SYS-1010-STYUKYMD
           MOVE    WRK-IN-YMD          TO  SYS-1010-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM
                   UNTIL       (WRK-ERRCD  NOT =   SPACE)  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
               IF      SYS-1010-KBNCD(1:1) NOT =   "1"
      *            ドクター以外
                   ADD     1                   TO  IDX
                   IF      IDX                 >   100
                       MOVE    "10"                TO  WRK-ERRCD
                   ELSE
                       MOVE    SYS-1010-KBNCD(1:5) TO  PHYSRES-DRCD
                                                                 (IDX)
      *                氏名
                       MOVE    SYS-1010-NAME       TO  PHYSRES-NAME
                                                                 (IDX)
                       MOVE    SYS-1010-KANANAME   TO  PHYSRES-KANANAME
                                                                 (IDX)
      *                医籍登録番号
                       MOVE    SYS-1010-ISEKINUM   TO  PHYSRES-ISEKINUM
                                                                 (IDX)
      *                麻薬ライセンス
                       MOVE    SYS-1010-MAYAKULICENSE
                                               TO  PHYSRES-MAYAKULICENSE
                                                                 (IDX)
      *                専門科コード
                       PERFORM VARYING    IDY      FROM    1   BY  1
                               UNTIL      IDY      >   5
                           MOVE    SYS-1010-SEMONKACD-X (IDY)
                                               TO  PHYSRES-SEMONKACD-X
                                                              (IDX IDY)
                       END-PERFORM
                   END-IF
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX                 =   ZERO
               MOVE    "11"            TO  WRK-ERRCD
           END-IF
           .
       2004-SYOCD-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報編集処理
      *****************************************************************
       2005-SYS1001-LIST-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX
      *
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                      SYS-1001-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-IN-YMD          TO  SYS-1001-STYUKYMD
           MOVE    WRK-IN-YMD          TO  SYS-1001-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1001-REC        TO  SYSKANRI-REC
           PERFORM 9001-SYSKANRI-KEYREAD-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC    TO  SYS-1001-REC
      *        県番号
               MOVE    SYS-1001-PREFNUM
                                       TO  SYS101RES-PREFECTURES-NUMBER
      *        点数表区分
               MOVE    SYS-1001-TENHYOKBN
                                       TO  SYS101RES-POINT-LIST
      *        医療機関コード
               MOVE    SYS-1001-HOSPCD
                                       TO  SYS101RES-INSTITUTION-CODE
      *        医療機関種別
               MOVE    SYS-1001-HOSPSBT
                                   TO  SYS101RES-INSTITUTION-SPECIATION
      *        医療機関ＩＤ
               MOVE    SYS-1001-HOSPID
                                       TO  SYS101RES-INSTITUTION-ID
      *        医療機関名称
               MOVE    SYS-1001-HOSPNAME
                                   TO  SYS101RES-INSTITUTION-WHOLENAME
      *        開設者名
               MOVE    SYS-1001-KAISETUNAME
                                   TO  SYS101RES-ESTABLISHER-WHOLENAME
      *        管理者名
               MOVE    SYS-1001-KANRINAME
                                   TO  SYS101RES-ADMINISTRATOR-WHOLENAME
               MOVE    SYS-1001-TANHOSPNAME
                                           TO  SYS101RES-TANHOSPNAME
               MOVE    SYS-1001-BEDSU      TO  WRK-NUM-4
               MOVE    WRK-NUM-4-X         TO  SYS101RES-BEDSU
               IF     (SYS-1001-BEDSUIPN(1:4)  =   SPACE )  OR
                      (SYS-1001-BEDSUIPN(1:4)  NOT NUMERIC)
                   MOVE    ZERO                TO  WRK-NUM-4
               ELSE
                   MOVE    SYS-1001-BEDSUIPN   TO  WRK-NUM-4
               END-IF
               MOVE    WRK-NUM-4-X         TO  SYS101RES-BEDSUIPN
               MOVE    SYS-1001-ROUPAYKBN  TO  SYS101RES-ROUPAYKBN
               EVALUATE    SYS-1001-ROUPAYKBN
                   WHEN    "1"
                       MOVE    "定率"      TO  SYS101RES-ROUPAYKBNMEI
                   WHEN    "2"
                       MOVE    "定額"      TO  SYS101RES-ROUPAYKBNMEI
               END-EVALUATE
               MOVE    SYS-1001-HOSPSBT1   TO  SYS101RES-HOSPSBT1
               EVALUATE    SYS-1001-HOSPSBT1
                   WHEN    "0"
                       MOVE    "旧総合病院でない"
                                           TO  SYS101RES-HOSPSBT1MEI
                   WHEN    "1"
                       MOVE    "旧総合病院である"
                                           TO  SYS101RES-HOSPSBT1MEI
               END-EVALUATE
               MOVE    SYS-1001-HOSPCDN    TO  SYS101RES-HOSPCDN
               MOVE    SYS-1001-SYOHOKBN   TO  SYS101RES-SYOHOKBN
               EVALUATE    SYS-1001-SYOHOKBN
                   WHEN    "0"
                       MOVE    "院内"      TO  SYS101RES-SYOHOKBNMEI
                   WHEN    "1"
                       MOVE    "院外"      TO  SYS101RES-SYOHOKBNMEI
               END-EVALUATE
               IF    ( SYS-1001-SANKANUM   IS  NUMERIC  )
               AND   ( SYS-1001-SANKANUM   NOT =   ZERO )
                   MOVE    SYS-1001-SANKANUM   TO  SYS101RES-SANKANUM
               ELSE
                   MOVE    SPACE               TO  SYS101RES-SANKANUM
               END-IF
               MOVE    SYS-1001-SKYPRTFLG  TO  SYS101RES-SKYPRTFLG
               EVALUATE    SYS-1001-SKYPRTFLG
                   WHEN    "0"
                       MOVE    "発行しない"
                                           TO  SYS101RES-SKYPRTFLGMEI
                   WHEN    "1"
                       MOVE    "発行する"  TO  SYS101RES-SKYPRTFLGMEI
                   WHEN    "2"
                       MOVE    "発行する（請求あり）"
                                           TO  SYS101RES-SKYPRTFLGMEI
                   WHEN    "3"
                       MOVE    "発行する（訂正時なし）"
                                           TO  SYS101RES-SKYPRTFLGMEI
               END-EVALUATE
               MOVE    SYS-1001-SYOHOPRTFLG
                                           TO  SYS101RES-SYOHOPRTFLG 
               EVALUATE    SYS-1001-SYOHOPRTFLG
                   WHEN    "0"
                       MOVE    "発行しない"
                                           TO  SYS101RES-SYOHOPRTFLGMEI
                   WHEN    "1"
                       MOVE    "発行する"  TO  SYS101RES-SYOHOPRTFLGMEI
                   WHEN    "2"
                       MOVE    "院内処方発行"
                                           TO  SYS101RES-SYOHOPRTFLGMEI
                   WHEN    "3"
                       MOVE    "発行する（訂正時なし）"
                                           TO  SYS101RES-SYOHOPRTFLGMEI
                   WHEN    "4"
                       MOVE    "院内処方発行（訂正時なし）" 
                                           TO  SYS101RES-SYOHOPRTFLGMEI
               END-EVALUATE
               MOVE    SYS-1001-ZENSYOHOFLG
                                           TO  SYS101RES-ZENSYOHOFLG
               EVALUATE    SYS-1001-ZENSYOHOFLG
                   WHEN    "0"
                       MOVE    "表示する"  TO  SYS101RES-ZENSYOHOFLGMEI
                   WHEN    "1"
                       MOVE    "表示しない"
                                           TO  SYS101RES-ZENSYOHOFLGMEI
               END-EVALUATE
      *        薬剤情報発効空白の時、ゼロとする
               IF      SYS-1001-YAKUJYOPRTFLG
                                       =   SPACE
                   MOVE    ZERO            TO  SYS-1001-YAKUJYOPRTFLG
               END-IF
               MOVE    SYS-1001-YAKUJYOPRTFLG
                                           TO  SYS101RES-YAKUJYOPRTFLG
               EVALUATE    SYS-1001-YAKUJYOPRTFLG
                   WHEN    "0"
                       MOVE    "発行しない"
                                       TO  SYS101RES-YAKUJYOPRTFLGMEI
                   WHEN    "1"
                       MOVE    "発行する"
                                       TO  SYS101RES-YAKUJYOPRTFLGMEI
                   WHEN    "2"
                       MOVE    "発行する（訂正時なし）"
                                       TO  SYS101RES-YAKUJYOPRTFLGMEI
               END-EVALUATE
      *        お薬手帳発効空白の時、ゼロとする
               IF      SYS-1001-OKUSURIPRTFLG  =   SPACE
                   MOVE    ZERO            TO  SYS-1001-OKUSURIPRTFLG
               END-IF
               MOVE    SYS-1001-OKUSURIPRTFLG
                                          TO  SYS101RES-OKUSURIPRTFLG
               EVALUATE    SYS-1001-OKUSURIPRTFLG
                   WHEN    "0"
                       MOVE    "発行しない"
                                          TO  SYS101RES-OKUSURIPRTFLGMEI
                   WHEN    "1"
                       MOVE    "発行する（後期高齢）"
                                          TO  SYS101RES-OKUSURIPRTFLGMEI
                   WHEN    "2"
                       MOVE    "発行する（後期高齢）（訂正時なし）"
                                          TO  SYS101RES-OKUSURIPRTFLGMEI
                   WHEN    "3"
                       MOVE    "発行する" TO  SYS101RES-OKUSURIPRTFLGMEI
                   WHEN    "4"
                       MOVE    "発行する（訂正時なし）"
                                          TO  SYS101RES-OKUSURIPRTFLGMEI
               END-EVALUATE
      *        診療費明細書発行区分空白の時、ゼロとする
               IF      SYS-1001-MEIPRTFLG  =   SPACE
                   MOVE    "1"                 TO  SYS-1001-MEIPRTFLG
               END-IF
               MOVE    SYS-1001-MEIPRTFLG  TO  SYS101RES-MEIPRTFLG
               EVALUATE    SYS-1001-MEIPRTFLG
                   WHEN    "0"
                       MOVE    "発行しない"
                                           TO  SYS101RES-MEIPRTFLGMEI
                   WHEN    "1"
                       MOVE    "発行する"  TO  SYS101RES-MEIPRTFLGMEI
                   WHEN    "2"
                       MOVE    "発行する（請求あり）"
                                           TO  SYS101RES-MEIPRTFLGMEI
                   WHEN    "3"
                       MOVE    "発行する（訂正時なし）"
                                           TO  SYS101RES-MEIPRTFLGMEI
               END-EVALUATE
      *        予約票発行区分空白の時、ゼロとする
               IF      SYS-1001-YYKHYOPRTFLG   =   SPACE
                   MOVE    ZERO                TO  SYS-1001-YYKHYOPRTFLG
               END-IF
               MOVE    SYS-1001-YYKHYOPRTFLG   TO
                                               SYS101RES-YYKHYOPRTFLG
               EVALUATE    SYS-1001-YYKHYOPRTFLG
                   WHEN    "0"
                       MOVE    "発行しない"
                                           TO  SYS101RES-YYKHYOPRTFLGMEI
                   WHEN    "1"
                       MOVE    "発行する"  TO  SYS101RES-YYKHYOPRTFLGMEI
               END-EVALUATE
      *        地方公費保険番号タブ区分空白の時、ゼロとする
               IF      SYS-1001-CHIHOUNUMKBN   =   SPACE
                   MOVE    ZERO                TO  SYS-1001-CHIHOUNUMKBN
               END-IF
               MOVE    SYS-1001-CHIHOUNUMKBN
                                           TO  SYS101RES-CHIHOUNUMKBN
               EVALUATE    SYS-1001-CHIHOUNUMKBN
                   WHEN    "0"
                       MOVE    "無効"      TO  SYS101RES-CHIHOUNUMKBNMEI
                   WHEN    "1"
                       MOVE    "有効"      TO  SYS101RES-CHIHOUNUMKBNMEI
               END-EVALUATE
      *        請求額端数区分（減免有） 空白の時、"1"とする
               IF      SYS-1001-DISCOUNT-HASU-KBN  =   SPACE
                   MOVE    "1"             TO
                                           SYS-1001-DISCOUNT-HASU-KBN
               END-IF
               MOVE    SYS-1001-DISCOUNT-HASU-KBN
                                           TO
                                           SYS101RES-DISCOUNT-HASU-KBN
               EVALUATE    SYS-1001-DISCOUNT-HASU-KBN
                   WHEN    "1"
                       MOVE    "１０円未満四捨五入"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBNNM
                   WHEN    "2"
                       MOVE    "１０円未満切り捨て"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBNNM
                   WHEN    "3"
                       MOVE    "１０円未満切り上げ"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBNNM
                   WHEN    "4"
                       MOVE    "１０円未満端数処理なし"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBNNM
               END-EVALUATE
      *        請求額端数区分医保（減免無保険分） 空白の時、"4"とする
               IF      SYS-1001-DISCOUNT-HASU-KBN2 =   SPACE
                   MOVE    "4"             TO
                                           SYS-1001-DISCOUNT-HASU-KBN2
               END-IF
               MOVE    SYS-1001-DISCOUNT-HASU-KBN2
                                           TO
                                           SYS101RES-DISCOUNT-HASU-KBN2
               EVALUATE    SYS-1001-DISCOUNT-HASU-KBN2
                   WHEN    "1"
                       MOVE    "１０円未満四捨五入"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN2NM
                   WHEN    "2"
                       MOVE    "１０円未満切り捨て"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN2NM
                   WHEN    "3"
                       MOVE    "１０円未満切り上げ"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN2NM
                   WHEN    "4"
                       MOVE    "１０円未満端数処理なし"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN2NM
               END-EVALUATE
      *        請求額端数区分医保（減免無自費分） 空白の時、"0"とする
               IF      SYS-1001-DISCOUNT-HASU-KBN2J   =   SPACE
                   MOVE    "0"             TO
                                           SYS-1001-DISCOUNT-HASU-KBN2J
               END-IF
               MOVE    SYS-1001-DISCOUNT-HASU-KBN2J
                                           TO
                                           SYS101RES-DISCOUNT-HASU-KBN2J
               EVALUATE    SYS-1001-DISCOUNT-HASU-KBN2J
                   WHEN    "0"
                       MOVE    "保険分に準ずる"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN2JNM
                   WHEN    "1"
                       MOVE    "１０円未満四捨五入"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN2JNM
                   WHEN    "2"
                       MOVE    "１０円未満切り捨て"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN2JNM
                   WHEN    "3"
                       MOVE    "１０円未満切り上げ"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN2JNM
                   WHEN    "4"
                       MOVE    "１０円未満端数処理なし"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN2JNM
               END-EVALUATE
      *        請求額端数区分労災（減免無保険分） 空白の時、"4"とする
               IF      SYS-1001-DISCOUNT-HASU-KBN3   =   SPACE
                   MOVE    "4"             TO
                                           SYS-1001-DISCOUNT-HASU-KBN3
               END-IF
               MOVE    SYS-1001-DISCOUNT-HASU-KBN3
                                           TO
                                           SYS101RES-DISCOUNT-HASU-KBN3
               EVALUATE    SYS-1001-DISCOUNT-HASU-KBN3
                   WHEN    "1"
                       MOVE    "１０円未満四捨五入"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN3NM
                   WHEN    "2"
                       MOVE    "１０円未満切り捨て"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN3NM
                   WHEN    "3"
                       MOVE    "１０円未満切り上げ"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN3NM
                   WHEN    "4"
                       MOVE    "１０円未満端数処理なし"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN3NM
               END-EVALUATE
      *        請求額端数区分労災（減免無自費分） 空白の時、"0"とする
               IF      SYS-1001-DISCOUNT-HASU-KBN3J   =   SPACE
                   MOVE    "0"             TO
                                           SYS-1001-DISCOUNT-HASU-KBN3J
               END-IF
               MOVE    SYS-1001-DISCOUNT-HASU-KBN3J
                                           TO
                                           SYS101RES-DISCOUNT-HASU-KBN3J
               EVALUATE    SYS-1001-DISCOUNT-HASU-KBN3J
                   WHEN    "0"
                       MOVE    "保険分に準ずる"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN3JNM
                   WHEN    "1"
                       MOVE    "１０円未満四捨五入"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN3JNM
                   WHEN    "2"
                       MOVE    "１０円未満切り捨て"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN3JNM
                   WHEN    "3"
                       MOVE    "１０円未満切り上げ"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN3JNM
                   WHEN    "4"
                       MOVE    "１０円未満端数処理なし"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN3JNM
               END-EVALUATE
      *        請求額端数区分自賠責（減免無保険分） 空白の時、"4"とする
               IF      SYS-1001-DISCOUNT-HASU-KBN4   =   SPACE
                   MOVE    "4"             TO
                                           SYS-1001-DISCOUNT-HASU-KBN4
               END-IF
               MOVE    SYS-1001-DISCOUNT-HASU-KBN4
                                           TO
                                           SYS101RES-DISCOUNT-HASU-KBN4
               EVALUATE    SYS-1001-DISCOUNT-HASU-KBN4
                   WHEN    "1"
                       MOVE    "１０円未満四捨五入"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN4NM
                   WHEN    "2"
                       MOVE    "１０円未満切り捨て"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN4NM
                   WHEN    "3"
                       MOVE    "１０円未満切り上げ"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN4NM
                   WHEN    "4"
                       MOVE    "１０円未満端数処理なし"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN4NM
               END-EVALUATE
      *        請求額端数区分自賠責（減免無自費分） 空白の時、"0"とする
               IF      SYS-1001-DISCOUNT-HASU-KBN4J   =   SPACE
                   MOVE    "0"             TO
                                           SYS-1001-DISCOUNT-HASU-KBN4J
               END-IF
               MOVE    SYS-1001-DISCOUNT-HASU-KBN4J
                                           TO
                                           SYS101RES-DISCOUNT-HASU-KBN4J
               EVALUATE    SYS-1001-DISCOUNT-HASU-KBN4J
                   WHEN    "0"
                       MOVE    "保険分に準ずる"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN4JNM
                   WHEN    "1"
                       MOVE    "１０円未満四捨五入"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN4JNM
                   WHEN    "2"
                       MOVE    "１０円未満切り捨て"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN4JNM
                   WHEN    "3"
                       MOVE    "１０円未満切り上げ"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN4JNM
                   WHEN    "4"
                       MOVE    "１０円未満端数処理なし"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN4JNM
               END-EVALUATE
      *        請求額端数区分公害（減免無自費分） 空白の時、"4"とする
               IF      SYS-1001-DISCOUNT-HASU-KBN5J   =   SPACE
                   MOVE    "4"             TO
                                           SYS-1001-DISCOUNT-HASU-KBN5J
               END-IF
               MOVE    SYS-1001-DISCOUNT-HASU-KBN5J
                                           TO
                                           SYS101RES-DISCOUNT-HASU-KBN5J
               EVALUATE    SYS-1001-DISCOUNT-HASU-KBN5J
                   WHEN    "0"
                       MOVE    "保険分に準ずる"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN5JNM
                   WHEN    "1"
                       MOVE    "１０円未満四捨五入"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN5JNM
                   WHEN    "2"
                       MOVE    "１０円未満切り捨て"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN5JNM
                   WHEN    "3"
                       MOVE    "１０円未満切り上げ"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN5JNM
                   WHEN    "4"
                       MOVE    "１０円未満端数処理なし"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN5JNM
               END-EVALUATE
      *        請求額端数区分第三者行為（減免無保険分） 空白の時、自賠責の設定とする
               IF      SYS-1001-DISCOUNT-HASU-KBN6   =   SPACE
                    MOVE    SYS-1001-DISCOUNT-HASU-KBN6
                                           TO
                                           SYS-1001-DISCOUNT-HASU-KBN6
               END-IF
               MOVE    SYS-1001-DISCOUNT-HASU-KBN6
                                           TO
                                           SYS101RES-DISCOUNT-HASU-KBN6
               EVALUATE    SYS-1001-DISCOUNT-HASU-KBN6
                   WHEN    "1"
                       MOVE    "１０円未満四捨五入"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN6NM
                   WHEN    "2"
                       MOVE    "１０円未満切り捨て"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN6NM
                   WHEN    "3"
                       MOVE    "１０円未満切り上げ"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN6NM
                   WHEN    "4"
                       MOVE    "１０円未満端数処理なし"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN6NM
               END-EVALUATE
      *        請求額端数区分自賠責（減免無自費分） 空白の時、自賠責の設定とする
               IF      SYS-1001-DISCOUNT-HASU-KBN6J   =   SPACE
                    MOVE    SYS-1001-DISCOUNT-HASU-KBN6J
                                           TO
                                           SYS-1001-DISCOUNT-HASU-KBN6J
               END-IF
               MOVE    SYS-1001-DISCOUNT-HASU-KBN6J
                                           TO
                                           SYS101RES-DISCOUNT-HASU-KBN6J
               EVALUATE    SYS-1001-DISCOUNT-HASU-KBN6J
                   WHEN    "0"
                       MOVE    "保険分に準ずる"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN6JNM
                   WHEN    "1"
                       MOVE    "１０円未満四捨五入"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN6JNM
                   WHEN    "2"
                       MOVE    "１０円未満切り捨て"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN6JNM
                   WHEN    "3"
                       MOVE    "１０円未満切り上げ"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN6JNM
                   WHEN    "4"
                       MOVE    "１０円未満端数処理なし"
                                           TO
                                       SYS101RES-DISCOUNT-HASU-KBN6JNM
               END-EVALUATE
      *        第三者行為（医療費）負担金額計算区分、空白の時"2"とする
               IF      SYS-1001-DAISANKBN    =   SPACE
                   MOVE    "2"             TO  SYS-1001-DAISANKBN
               END-IF
               MOVE    SYS-1001-DAISANKBN  TO  SYS101RES-DAISANKBN
               EVALUATE    SYS-1001-DAISANKBN
                   WHEN    "1"
                       MOVE    "負担金額１０円未満四捨五入"
                                           TO  SYS101RES-DAISANKBNMEI
                   WHEN    "2"
                       MOVE    "負担金額１０円未満端数処理なし"
                                           TO  SYS101RES-DAISANKBNMEI
               END-EVALUATE
      *        減免計算対象区分、空白の時"1"とする
               IF      SYS-1001-GENTAIKBN    =   SPACE
                   MOVE    "1"             TO  SYS-1001-GENTAIKBN
               END-IF
               MOVE    SYS-1001-GENTAIKBN  TO  SYS101RES-GENTAIKBN
               EVALUATE    SYS-1001-GENTAIKBN
                   WHEN    "1"
                       MOVE    "自費分を含む"  
                                           TO  SYS101RES-GENTAIKBNMEI
                   WHEN    "2"
                       MOVE    "自費分を除く" 
                                           TO  SYS101RES-GENTAIKBNMEI
                   WHEN    "3"
                       MOVE    "自費分のみ"
                                           TO  SYS101RES-GENTAIKBNMEI
               END-EVALUATE
      *        消費税端数区分 空白の時、"2"とする
               IF      SYS-1001-TAX-HASU-KBN   =   SPACE
                   MOVE    "2"             TO  SYS-1001-TAX-HASU-KBN
               END-IF
               MOVE    SYS-1001-TAX-HASU-KBN
                                           TO  SYS101RES-TAX-HASU-KBN
               EVALUATE    SYS-1001-TAX-HASU-KBN
                   WHEN    "1"
                       MOVE    "１円未満四捨五入"  
                                           TO  SYS101RES-TAX-HASU-KBNMEI
                   WHEN    "2"
                       MOVE    "１円未満切り捨て" 
                                           TO  SYS101RES-TAX-HASU-KBNMEI
                   WHEN    "3"
                       MOVE    "１円未満切り上げ" 
                                           TO  SYS101RES-TAX-HASU-KBNMEI
               END-EVALUATE
      *        自費コード端数 空白の時、"2"とする
               IF      SYS-1001-JIHI-HASU-KBN   =   SPACE
                   MOVE    "2"             TO  SYS-1001-JIHI-HASU-KBN
               END-IF
               MOVE    SYS-1001-JIHI-HASU-KBN
                                           TO  SYS101RES-JIHI-HASU-KBN
               EVALUATE    SYS-1001-JIHI-HASU-KBN
                   WHEN    "1"
                       MOVE    "１円未満四捨五入"
                                           TO
                                           SYS101RES-JIHI-HASU-KBNMEI
                   WHEN    "2"
                       MOVE    "１円未満切り捨て"
                                           TO
                                           SYS101RES-JIHI-HASU-KBNMEI
                   WHEN    "3"
                       MOVE    "１円未満切り上げ"
                                           TO
                                           SYS101RES-JIHI-HASU-KBNMEI
               END-EVALUATE
      *        更生・育成限度額日割計算区分 空白の時、"1"とする
               IF      SYS-1001-HIWARIKBN  =   SPACE
                   MOVE    "1"             TO  SYS-1001-HIWARIKBN
               END-IF
               MOVE    SYS-1001-HIWARIKBN  TO  SYS101RES-HIWARIKBN
               EVALUATE    SYS-1001-HIWARIKBN
                   WHEN    "1"
                       MOVE    "日割計算しない"
                                           TO  SYS101RES-HIWARIKBNMEI
                   WHEN    "2"
                       MOVE    "日割計算する"
                                           TO  SYS101RES-HIWARIKBNMEI
               END-EVALUATE
      *        データ作成フラグ 空白の時、"0"とする
               IF      SYS-1001-DATACREATEFLG  =   SPACE
                   MOVE    "0"             TO  SYS-1001-DATACREATEFLG
               END-IF
               MOVE    SYS-1001-DATACREATEFLG
                                           TO  SYS101RES-DATACREATEFLG
               EVALUATE    SYS-1001-DATACREATEFLG
                   WHEN    "0"
                       MOVE    "作成しない"
                                           TO
                                           SYS101RES-DATACREATEFLGMEI
                   WHEN    "1"
                       MOVE    "作成する"  TO
                                           SYS101RES-DATACREATEFLGMEI
               END-EVALUATE
      *        データ提出方法区分 空白の時、"0"とする
               IF      SYS-1001-DATATEISYUTUKBN  =   SPACE
                   MOVE    "0"             TO  SYS-1001-DATATEISYUTUKBN
               END-IF
               MOVE    SYS-1001-DATATEISYUTUKBN
                                           TO  SYS101RES-DATATEISYUTUKBN
               EVALUATE    SYS-1001-DATATEISYUTUKBN
                   WHEN    "0"
                       MOVE    "提出しない"
                                       TO  SYS101RES-DATATEISYUTUKBNMEI
                   WHEN    "1"
                       MOVE    "自動で送信"
                                       TO  SYS101RES-DATATEISYUTUKBNMEI
                   WHEN    "2"
                       MOVE    "手動で送信"
                                       TO  SYS101RES-DATATEISYUTUKBNMEI
               END-EVALUATE
      *        データ感染症区分 空白の時、"0"とする
               IF      SYS-1001-DATAINFECTIONKBN   =   SPACE
                   IF      SYS-1001-DATACREATEFLG  =   "1"
                       MOVE    "1"         TO  SYS-1001-DATAINFECTIONKBN
                   ELSE
                       MOVE    "0"         TO  SYS-1001-DATAINFECTIONKBN
                   END-IF
               END-IF
               MOVE    SYS-1001-DATAINFECTIONKBN
                                           TO
                                           SYS101RES-DATAINFECTIONKBN
               EVALUATE    SYS-1001-DATAINFECTIONKBN
                   WHEN    "0"
                       MOVE    "作成しない"
                                           TO
                                           SYS101RES-DATAINFECTIONKBNMEI
                   WHEN    "1"
                       MOVE    "作成する"  TO
                                           SYS101RES-DATAINFECTIONKBNMEI
                   WHEN    "2"
                       MOVE    "作成する／日"
                                           TO
                                           SYS101RES-DATAINFECTIONKBNMEI
                   WHEN    "9"
                       MOVE    "作成しない消"
                                           TO
                                           SYS101RES-DATAINFECTIONKBNMEI
               END-EVALUATE
      *        データ提出方法区分 空白の時、"0"とする
               IF      SYS-1001-JIHIPOSITION   =   SPACE
                   MOVE    "1"             TO  SYS-1001-JIHIPOSITION
               END-IF
               MOVE    SYS-1001-JIHIPOSITION
                                           TO  SYS101RES-JIHIPOSITION
               EVALUATE    SYS-1001-JIHIPOSITION
                   WHEN    "1"
                       MOVE    "保険分欄（負担金額四捨五入）"
                                           TO
                                           SYS101RES-JIHIPOSITIONMEI
                   WHEN    "2"
                       MOVE    "自費分欄"  TO
                                           SYS101RES-JIHIPOSITIONMEI
                   WHEN    "3"
                       MOVE    "保険分欄（負担金額端数処理なし）"
                                           TO
                                           SYS101RES-JIHIPOSITIONMEI
               END-EVALUATE
           ELSE
      *        対象なし
               MOVE    "11"            TO  WRK-ERRCD
           END-IF
      *
      *    連絡先情報
           MOVE    SPACE               TO  SYS-1002-REC
           INITIALIZE                      SYS-1002-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           MOVE    WRK-IN-YMD          TO  SYS-1002-STYUKYMD
           MOVE    WRK-IN-YMD          TO  SYS-1002-EDYUKYMD
      *
           MOVE    SYS-1002-REC        TO  SYSKANRI-REC
           PERFORM 9001-SYSKANRI-KEYREAD-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC    TO  SYS-1002-REC
      *        郵便番号
               MOVE    SYS-1002-POST   TO  SYS101RES-ADDRESS-ZIPCODE
      *        所在地
               MOVE    SYS-1002-ADRS   TO  SYS101RES-WHOLEADDRESS
      *        電話番号
               MOVE    SYS-1002-TEL    TO  SYS101RES-PHONENUMBER
      *        ＦＡＸ番号
               MOVE    SYS-1002-FAX    TO  SYS101RES-FAXNUMBER
           END-IF
      *
      *    広告
           MOVE    SPACE               TO  SYS-1003-REC
           INITIALIZE                      SYS-1003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1003-HOSPNUM
           MOVE    "1003"              TO  SYS-1003-KANRICD
           MOVE    "*"                 TO  SYS-1003-KBNCD
           MOVE    WRK-IN-YMD          TO  SYS-1003-STYUKYMD
           MOVE    WRK-IN-YMD          TO  SYS-1003-EDYUKYMD
      *
           MOVE    SYS-1003-REC        TO  SYSKANRI-REC
           PERFORM 9001-SYSKANRI-KEYREAD-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC    TO  SYS-1003-REC
      *        ｅメールアドレス
               MOVE    SYS-1003-EMAIL  TO  SYS101RES-EMAIL-ADDRESS
      *        ホームページアドレス
               MOVE    SYS-1003-HPURL  TO  SYS101RES-HOMEPAGE-ADDRESS
           END-IF
           .
       2005-SYS1001-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-PTXMLMAKE-SEC               SECTION.
      *
           EVALUATE    FLG-SYORI
               WHEN    1
               WHEN    0
      *            診療科一覧
                   PERFORM 3001-DEPARTMENT-MAKE-SEC
               WHEN    2
               WHEN    3
      *            ドクター・職員一覧
                   PERFORM 3002-PHYSICIAN-MAKE-SEC
               WHEN    4
      *            医療機関情報
                   PERFORM 3003-MEDICALINF-MAKE-SEC
           END-EVALUATE
      *
           .
       300-PTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理（診療科一覧）
      *****************************************************************
       3001-DEPARTMENT-MAKE-SEC               SECTION.
      *
      *    エラーメッセージ編集
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-KEICD           TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               MOVE    "処理終了"          TO  DEPTRES-API-RESULT-MSG
           ELSE
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD           TO  DEPTRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  DEPTRES-API-RESULT-MSG
           END-IF
      *
           IF      APILST5-BODY     NOT =   LOW-VALUE
               DISPLAY "DEPTRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_departmentv2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  DEPTRES-MODE
           MOVE    "departmentres"     TO  DEPTRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-DEPARTMENTRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_departmentv2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "departmentres"     TO  DEPTRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-DEPARTMENTRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               DISPLAY "XMLWRITE OK      = " MCP-RC
               MOVE    DEPTRES-OBJECT      TO  APILST5-BODY 
               MOVE    "application/xml"   TO  APILST5-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       3001-DEPARTMENT-MAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理（ドクター一覧）
      *****************************************************************
       3002-PHYSICIAN-MAKE-SEC               SECTION.
      *
           MOVE   "Patient Info"       TO  PHYSRES-RESKEY
      *    日付・時間出力編集
           MOVE    DEPTRES-INFORMATION-DATE
                                       TO  PHYSRES-INFORMATION-DATE
           MOVE    DEPTRES-INFORMATION-TIME
                                       TO  PHYSRES-INFORMATION-TIME
           MOVE    ZERO                TO  PHYSRES-API-RESULT
      *    基準日 
           MOVE    WRK-HEN-DATE        TO  PHYSRES-BASE-DATE
      *    エラーメッセージ編集
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-KEICD           TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               MOVE    "処理終了"          TO  PHYSRES-API-RESULT-MSG
           ELSE
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD           TO  PHYSRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  PHYSRES-API-RESULT-MSG
           END-IF
      *
           IF      APILST5-BODY     NOT =   LOW-VALUE
               DISPLAY "PHYSRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_physicianv2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  PHYSRES-MODE
           MOVE    "physicianres"      TO  PHYSRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-PHYSICIANRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_physicianv2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "physicianres"      TO  PHYSRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-PHYSICIANRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               DISPLAY "XMLWRITE OK      = " MCP-RC
               MOVE    PHYSRES-OBJECT      TO  APILST5-BODY 
               MOVE    "application/xml"   TO  APILST5-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       3002-PHYSICIAN-MAKE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    XML情報書き込み処理（医療機関情報）
      *****************************************************************
       3003-MEDICALINF-MAKE-SEC               SECTION.
      *
           MOVE   "Patient Info"       TO  SYS101RES-RESKEY
      *    日付・時間出力編集
           MOVE    DEPTRES-INFORMATION-DATE
                                       TO  SYS101RES-INFORMATION-DATE
           MOVE    DEPTRES-INFORMATION-TIME
                                       TO  SYS101RES-INFORMATION-TIME
           MOVE    ZERO                TO  SYS101RES-API-RESULT
      *    基準日 
           MOVE    WRK-HEN-DATE        TO  SYS101RES-BASE-DATE
      *    エラーメッセージ編集
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-KEICD           TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               MOVE    "処理終了"          TO  SYS101RES-API-RESULT-MSG
           ELSE
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD           TO  SYS101RES-API-RESULT
               MOVE    WRK-ERRMSG          TO  SYS101RES-API-RESULT-MSG
           END-IF
      *
           IF      APILST5-BODY     NOT =   LOW-VALUE
               DISPLAY "SYS101RES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_system1001v2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  SYS101RES-MODE
           MOVE    "system1001res"     TO  SYS101RES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-SYSTEM1001RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_system1001v2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "system1001res"     TO  SYS101RES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-SYSTEM1001RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               DISPLAY "XMLWRITE OK      = " MCP-RC
               MOVE    SYS101RES-OBJECT    TO  APILST5-BODY 
               MOVE    "application/xml"   TO  APILST5-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       3003-MEDICALINF-MAKE-EXT.
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
               MOVE    "対象が１００件以上となります。"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "対象がありません。"
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
      ****     MOVE    "処理区分未設定"        TO  WRK-ERRMSG
               MOVE    "リクエスト番号がありません"
                                               TO  WRK-ERRMSG
           WHEN    "97"
               MOVE    "送信内容に誤りがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "98"
               MOVE    "送信内容の読込ができませんでした"
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
      *    システム管理読込処理
      *****************************************************************
       9001-SYSKANRI-KEYREAD-SEC            SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                      SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       9001-SYSKANRI-KEYREAD-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    システム管理読込処理
      *****************************************************************
       900-SYSKANRI-READ-SEC            SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC         TO  SYSKANRI-REC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
               INITIALIZE                      SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
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
