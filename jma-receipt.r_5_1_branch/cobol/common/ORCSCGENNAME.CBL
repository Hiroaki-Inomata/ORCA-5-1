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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSCGENNAME.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為
      *  コンポーネント名  : 剤毎の一般名編集サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/02/XX    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-多々納  17/03/XX  後発品変更可（処方毎）対応
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
           03  FLG-GENERICNAME     PIC 9(01).
      *
      *    後発医薬品変更不可
           03  FLG-KOUHATU-FUKA        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-STR             PIC 9(04).
           03  IDX-SRY             PIC 9(04).
           03  IDXA                PIC 9(04).
           03  IDXB                PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC X(06).
               05  WRK-SRYDD           PIC X(02).
      *
           03  WRK-SRYKBN              PIC X(02).
      *
           03  WRK-IPN-AREA.
               05  WRK-YAKKAKJNCD      PIC X(12).
               05  WRK-GENERIC-KBN     PIC X(01).
               05  WRK-GENERICNAME     PIC X(200).
               05  WRK-NAME-KBN        PIC X(01).
               05  WRK-GEN-TANICD      PIC X(03).
               05  WRK-GEN-TANINAME    PIC X(24).
      *
               05  WRK-IPN-KISAIKBN    PIC 9(01).
               05  WRK-KOUHATUKBN      PIC 9(01).
      *
               05  FLG-IPNMEI          PIC 9(01).
               05  FLG-FUKACHK         PIC 9(01).
               05  FLG-FUKACHK2        PIC 9(01).
      *
           03  WRK-GENERIC-CODE        PIC X(12).
           03  WRK-GENERIC-STYMD       PIC X(08).
           03  WRK-GENERIC-EDYMD       PIC X(08).
           03  WRK-YAKKAKJNCD-OLD      PIC X(12).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
      *    COPY    "CPSYSKANRI.INC".
      *    処方箋ＧＥ表示
      *    COPY    "CPSK1030.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *    一般名テーブル
       01  GENERICNAME-REC.
           COPY    "CPGENERICNAME.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
      * 01  SRYACCT-REC.
      *    COPY    "CPSRYACCT.INC".
      *
      *    診療行為マスタワーク
       01  TBL-SRYACT-AREA.
           02  TBL-SRYACT-REC         OCCURS   10.
           COPY    "CPSRYACT.INC"     REPLACING
                                      //SRY-// BY //TBL-SRY-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
      *    COPY    "CPORCSDAY.INC".
      *    COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
      *    COPY    "CPORCSKANACHK.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    院外処方一般名編集パラメタ
           COPY    "CPORCSCGENNAME.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSCGENNAMEAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  ORCSCGENN-ERRCD
      *
           PERFORM 100-INIT-SEC
      *
           IF      ORCSCGENN-INNAIKBN  =   "1"
      *        院外のみ
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *TEST
      *    DISPLAY "ZAINUM:"  ORCSGETGE-ZAINUM
      *          ",SRYKBN:"  WRK-SRYKBN
      *          ",ERRCD:"  ORCSGETGE-ERRCD
      *          ",TBLMAX:"  ORCSGETGE-TBLMAX
      *    PERFORM VARYING   IDX   FROM   1  BY  1
      *            UNTIL     IDX   >   50
      *                OR   (ORCSGETGE-SRYCD (IDX) = SPACE)
      *         DISPLAY "IDX:"  IDX
      *             ",SRYCD:"  ORCSGETGE-SRYCD (IDX)
      *           ",YAKK:" ORCSGETGE-YAKKAKJNCD (IDX)
      *           ",GENERIC-KBN:" ORCSGETGE-GENERIC-KBN (IDX)
      *           ",NAME-KBN:" ORCSGETGE-NAME-KBN (IDX)
      *        IF    ORCSGETGE-GENERICNAME (IDX) NOT = SPACE
      *         DISPLAY  "GENERICNAME:" ORCSGETGE-GENERICNAME (IDX)
      *                      (1:20)
      *        END-IF
      **** END-PERFORM
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　　処理
      *****************************************************************
       100-INIT-SEC                      SECTION.
      *
           MOVE    "1"                 TO  ORCSCGENN-INNAIKBN 
      *    院内・院外判定
           IF      ORCSCGENN-SYORIKBN  =   "1"
               IF      (ORCSCGENN-SRYKBN   =   "21"
                                           OR  "22"
                                           OR  "23"
                                           OR  "14" )
                 AND  ((ORCSCGENN-SRYSYUKBN    =   "213"
                                               OR  "223"
                                               OR  "233"
                                               OR  "231"
                                               OR  "294" )
                   OR  (ORCSCGENN-ZAITEN       >   ZERO  ))
      *            院内
                   MOVE    "0"                 TO  ORCSCGENN-INNAIKBN 
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  ORCSCGENN-TBLMAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   51   )
               IF     (ORCSCGENN-INPUTCD(IDX)   =   SPACE)
                 AND  (ORCSCGENN-SRYCD  (IDX)   =   SPACE)
                   MOVE    51              TO  IDX
               ELSE
                   ADD     1               TO  ORCSCGENN-TBLMAX
               END-IF
           END-PERFORM
      *
      *    診療日付
           IF      ORCSCGENN-SRYYMD    =   SPACE
               MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
           ELSE
               MOVE    ORCSCGENN-SRYYMD    TO  WRK-SRYYMD
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   ORCSCGENN-TBLMAX     )
               INITIALIZE                  WRK-IPN-AREA
               IF      ORCSCGENN-SRYCD (IDX)(1:1)  =   "6"
                   COMPUTE IDX-STR             =   IDX     +   1
                   PERFORM VARYING     IDXB    FROM    IDX-STR BY  1
                           UNTIL      (IDXB    >   ORCSCGENN-TBLMAX)
                       IF      ORCSCGENN-SRYCD (IDXB)(1:1) =   "6"
                           MOVE    ORCSCGENN-TBLMAX    TO  IDXB
                       ELSE
      *                    予約コードチェック処理
                           PERFORM 20011-INP-HENSYU-SEC
                       END-IF
                   END-PERFORM
      *            名称編集処理
                   PERFORM 2002-GENRICNAME-CHEK-SEC
               END-IF
           END-PERFORM
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約コードチェック処理
      *****************************************************************
       20011-INP-HENSYU-SEC      SECTION.
      *
           EVALUATE    ORCSCGENN-SRYCD (IDXB)
               WHEN    "099209903"
      *            後発医薬品へ変更不可
                   MOVE    1                   TO  FLG-FUKACHK
                   MOVE    1                   TO  FLG-IPNMEI
               WHEN    "099209905"
      *            含量規格変更不可
                   MOVE    1                   TO  FLG-FUKACHK2
               WHEN    "099209906"
      *            剤型変更不可
                   MOVE    1                   TO  FLG-FUKACHK2
               WHEN    "099209907"
      *            銘柄名記載
                   MOVE    1                   TO  FLG-IPNMEI
               WHEN    "099209908"
      *            一般名記載
                   MOVE    2                   TO  FLG-IPNMEI
           END-EVALUATE
      *
           .
       20011-INP-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    院外処方せん一般名編集処理
      *****************************************************************
       2002-GENRICNAME-CHEK-SEC       SECTION.
      *
      *    一般名変更不可（剤）入力有
           IF     (FLG-IPNMEI              =   ZERO )
             AND  (ORCSCGENN-KOUHATU-IN    =   "1"  )
      *        銘柄名記載
               MOVE    1                   TO  FLG-IPNMEI
           END-IF
      *
      *!!!!!!!!!!!!!!!
      *    一般名変更可（剤）入力有
           IF     (FLG-IPNMEI              =   ZERO )
             AND  (ORCSCGENN-KOUHATU-IN    =   "2"  )
      *        一般名記載
               MOVE    3                   TO  FLG-IPNMEI
           END-IF
      *
           IF      ORCSCGENN-SYORIKBN2     =   "1"
      *       点数マスタ検索なし
               MOVE    ORCSCGENN-TNS-KOUHATUKBN (IDX)
                                           TO  WRK-KOUHATUKBN
               MOVE    ORCSCGENN-IPN-KISAIKBN (IDX)
                                           TO  WRK-IPN-KISAIKBN
      *
               MOVE    ORCSGETGE-YAKKAKJNCD (IDX)
                                           TO  WRK-YAKKAKJNCD
           ELSE
      *        点数マスタ検索
               MOVE    ORCSCGENN-SRYCD (IDX)   TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
      *        点数附加情報検索
               IF     (FLG-TENSU           =   ZERO)
                   PERFORM 9101-TENSUPLUS-READ-SEC
                   MOVE    TNS-KOUHATUKBN      TO  WRK-KOUHATUKBN
                   MOVE    TNSP-IPN-KISAIKBN   TO  WRK-IPN-KISAIKBN
                   MOVE    TNS-YAKKAKJNCD      TO  WRK-YAKKAKJNCD
      *R02.2
                   IF     (TNSP-USERTANICD NOT =   SPACE)
                        MOVE    TNSP-USERTANICD    TO  TNS-TANICD
                        MOVE    TNSP-USERTANINAME  TO  TNS-TANINAME
                   END-IF
               END-IF
           END-IF
      *
      *    マスタの一般名記載区分なしで後発品の時、システム管理判定
           IF     (WRK-IPN-KISAIKBN        =   0       )
             AND  (WRK-KOUHATUKBN          =   1
                                           OR  2
                                           OR  3
                                           OR  4
                                           OR  7       )
      *        変更可の指定は、一般名
               IF       (ORCSCGENN-1030-KOUHATUKA    =   "1" )
      *!!!!!
                    OR  (FLG-IPNMEI            =   3 )
                   MOVE    1               TO  WRK-IPN-KISAIKBN
               END-IF
           END-IF
      *
      *
           IF      WRK-IPN-KISAIKBN    =   1
                                       OR  3
      *        一般名記載ありをなしへ
               IF     (FLG-IPNMEI          =   1    )
      *            銘柄名記載・後発品変更不可の入力あり
                   MOVE    ZERO            TO  WRK-IPN-KISAIKBN
               END-IF
           ELSE
      *        一般名記載なしをありへ
               IF      FLG-IPNMEI          =   2
      *            一般名記載の入力あり
                   MOVE    1               TO  WRK-IPN-KISAIKBN
               END-IF
           END-IF
      *
      *    マスタ検索なし
           IF     (ORCSCGENN-SYORIKBN2 =   "1" )
               IF     (WRK-IPN-KISAIKBN    =   1    )
                 AND  (ORCSCGENN-HEN-NAME(IDX)  =   SPACE )
      *            一般名名称編集処理
                   PERFORM 20011-GENRICNAME-HEN-SEC
               ELSE
      *            名称区分の編集
                   MOVE    WRK-IPN-KISAIKBN    TO  ORCSCGENN-NAME-KBN
                                                               (IDX)
               END-IF
           ELSE
      *        一般名名称編集処理
               PERFORM 20011-GENRICNAME-HEN-SEC
           END-IF
           .
       2001-GENRICNAME-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一般名名称編集処理
      *****************************************************************
       20011-GENRICNAME-HEN-SEC         SECTION.
      *
      *    一般名記載あり
           IF      WRK-IPN-KISAIKBN    =   1
               PERFORM 9102-GENERICNAME-READ-SEC
      *        一般名テーブルなしは記載なし
               IF      FLG-GENERICNAME     =   1
                   MOVE    ZERO            TO  WRK-IPN-KISAIKBN
               END-IF
           END-IF
      *
           IF     (WRK-IPN-KISAIKBN    =   2
                                       OR  3   )
      *        処方名称
               MOVE    ORCSCGENN-SRYCD (IDX)   TO  TNS-SRYCD
               MOVE    WRK-SRYYMD          TO  TNS-YUKOSTYMD
               MOVE    WRK-SRYYMD          TO  TNS-YUKOEDYMD
      *        処方名称検索
               PERFORM 9101-TENSUPLUS-READ-SEC
           END-IF
      *
           EVALUATE    WRK-IPN-KISAIKBN
      *        一般名記載
               WHEN    1
                   MOVE    SPACE               TO  WRK-GENERICNAME
                   STRING  "≪般≫"                DELIMITED  BY  SIZE
                           GENERIC-GENERICNAME     DELIMITED  BY  SPACE
                                               INTO    WRK-GENERICNAME
                   END-STRING
                   MOVE    GENERIC-TANICD      TO  WRK-GEN-TANICD
                   MOVE    GENERIC-TANINAME    TO  WRK-GEN-TANINAME
                   MOVE    TNS-YAKKAKJNCD      TO  WRK-YAKKAKJNCD
                   MOVE    "1"                 TO  WRK-NAME-KBN
      *        処方名称記載
               WHEN    2
                   MOVE    TNSP-SHONAME        TO  WRK-GENERICNAME
                   MOVE    "2"                 TO  WRK-NAME-KBN
      *        処方名称を一般名称とする
               WHEN    3
                   MOVE    SPACE               TO  WRK-GENERICNAME
                   STRING  "≪般≫"                DELIMITED  BY  SIZE
                           TNSP-SHONAME            DELIMITED  BY  SPACE
                                               INTO    WRK-GENERICNAME
                   END-STRING
                   MOVE    TNS-TANICD          TO  WRK-GEN-TANICD
                   MOVE    TNS-TANINAME        TO  WRK-GEN-TANINAME
                   MOVE    TNS-YAKKAKJNCD      TO  WRK-YAKKAKJNCD
                   MOVE    "3"                 TO  WRK-NAME-KBN
           END-EVALUATE
      *
      *    処方せん名称
           MOVE    WRK-GENERICNAME     TO  ORCSCGENN-HEN-NAME (IDX)
           MOVE    WRK-NAME-KBN        TO  ORCSCGENN-NAME-KBN (IDX)
           MOVE    WRK-GEN-TANINAME    TO  ORCSCGENN-HEN-TANINAME(IDX)
           .
       2001-GENRICNAME-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    WRK-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  TNS-YUKOEDYMD
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
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報検索処理
      *****************************************************************
       9101-TENSUPLUS-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    WRK-SRYYMD          TO  TNSP-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  TNSP-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
                   MOVE    ZERO                TO  FLG-TENSUPLUS
               ELSE
                   INITIALIZE                  TENSUPLUS-REC
                   MOVE    1                   TO  FLG-TENSUPLUS
               END-IF
           ELSE
               INITIALIZE                      TENSUPLUS-REC
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9101-TENSUPLUS-READ-EXT.
           EXIT.
      *****************************************************************
      *    一般名テーブル検索処理
      *****************************************************************
       9102-GENERICNAME-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  GENERICNAME-REC
           INITIALIZE                      GENERICNAME-REC
           MOVE    WRK-YAKKAKJNCD      TO  GENERIC-YAKKAKJNCD
      **** MOVE    SPA-HOSPNUM         TO  GENERIC-HOSPNUM
      *
           MOVE    GENERICNAME-REC     TO  MCPDATA-REC
           MOVE    "tbl_genericname"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_genericname"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  GENERICNAME-REC
                   MOVE    ZERO                TO  FLG-GENERICNAME
               ELSE
                   INITIALIZE                  GENERICNAME-REC
                   MOVE    1                   TO  FLG-GENERICNAME
               END-IF
           ELSE
               INITIALIZE                      GENERICNAME-REC
               MOVE    1                   TO  FLG-GENERICNAME
           END-IF
           MOVE    "tbl_genericname"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9102-GENERICNAME-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       900-SRYACT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *
