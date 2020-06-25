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
       PROGRAM-ID.             ORCSRECEUPD.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 
      *  管理者            : レセプト管理更新処理
      *  05/02/08    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  05/11/11  MONFUNC 対応 他
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *
      *  04.07.01    NACL-多々納  14/05/15  レセプト一括作成がまだのときは
      *                                     更新しない
      *                                     入院は対象外とする
      *                                     第三者行為のときは医保分も更新 
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-RECEUPD         PIC 9(01).
           03  FLG-RECE07          PIC 9(01).
      *
      *    1:医保のみ 2:第三者行為のみ 9:両方あり
           03  FLG-DAISANSYA       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYM       PIC X(06).
               05  WRK-SYSDD       PIC 9(02).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
      *    レセプト作成管理退避
       01  WRK-RECEUPD-REC.
           COPY    "CPRECEUPD.INC" REPLACING  //RECEUPD-//
                                   BY         //WRK-RECEUPD-//.
      *     
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    レセプト作成管理
       01  RECEUPD-REC.
           COPY    "CPRECEUPD.INC".
      *
      *    レセプト管理     
       01  RECE07-REC. 
           COPY    "CPRCF007.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSRECEUPD.INC".
grpsys*
grpsys      COPY    "COMMON-SPA".
      *****************************************************************
     *
       PROCEDURE                   DIVISION    USING
                                   ORCSRECEUPDAREA
           SPA-AREA.
     *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *    
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYSYMD
           IF      WRK-SYSDD           <=   10
               INITIALIZE                   STS-AREA-DAY
               INITIALIZE                   LNK-DAY6-AREA
               MOVE  "61"                   TO  LNK-DAY6-IRAI
               MOVE  WRK-SYSYMD             TO  LNK-DAY6-KIJUN
               MOVE  "2"                    TO  LNK-DAY6-ZOGENPTN
               MOVE  -1                     TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"              USING   STS-AREA-DAY
                                                    LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN      TO  WRK-SYSYMD 
           END-IF
      *
           MOVE    ZERO                TO  LNK-UPD-RC
                                           FLG-DAISANSYA
      *
           IF      LNK-UPD-SYORI       =   "2"
      *        入院は対象外
               IF      LNK-UPD-NYUGAIKBN   =   "1"
                               DISPLAY "NYUIN "
                   GO  TO  000-PROC-EXT
               END-IF
      *
      *        該当月のレセプト一括作成の有無
               INITIALIZE                  RECE07-REC
      *        0:医保
      *        1:労災、公務災害（労災準拠）
      *        2:自賠責（労災準拠）
      *        4:自賠責（健保準拠）
      *        5:公務災害（健保準拠）
      *        6:公害
      *        7:自賠責（第三者行為）
               EVALUATE    LNK-UPD-HKNKBN
                   WHEN    "0"
      *                医保
                       MOVE    "HC06"              TO  RECE07-PRTID
                       MOVE    "0"                 TO  RECE07-RECEKBN
                   WHEN    "1"
                   WHEN    "5"
      *                労災
                       MOVE    "HC21"              TO  RECE07-PRTID
                       MOVE    "3"                 TO  RECE07-RECEKBN
                   WHEN    "2"
                   WHEN    "4"
      *                自賠責（新様式）
                       MOVE    "HC23"              TO  RECE07-PRTID
                       MOVE    "4"                 TO  RECE07-RECEKBN
                   WHEN    "7"
      *                自賠責（第三者行為）
                       MOVE    "HC23"              TO  RECE07-PRTID
                       MOVE    "8"                 TO  RECE07-RECEKBN
                   WHEN    "6"
      *                公害
                       MOVE    "HC25"              TO  RECE07-PRTID
                       MOVE    "6"                 TO  RECE07-RECEKBN
               END-EVALUATE
               MOVE    LNK-UPD-SRYYM       TO  RECE07-SRYYM
               MOVE    "1"                 TO  RECE07-SYOKBN
               MOVE    LNK-UPD-NYUGAIKBN   TO  RECE07-NYUGAIKBN
grpsys         MOVE    LNK-UPD-HOSPNUM     TO  RECE07-HOSPNUM
               MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
               MOVE    "key7"              TO  WRK-MCP-PATHNAME
               MOVE    RECE07-REC          TO  MCPDATA-REC
               PERFORM 900-RECE07-INV-SEC
               IF      FLG-RECE07          =   1
                   EVALUATE    LNK-UPD-HKNKBN
                       WHEN    "2"
                       WHEN    "4"
      *                    自賠責（従来様式）
                           INITIALIZE                  RECE07-REC
                           MOVE    "HC23"          TO  RECE07-PRTID
                           MOVE    "5"             TO  RECE07-RECEKBN
                           MOVE    LNK-UPD-SRYYM   TO  RECE07-SRYYM
                           MOVE    "1"             TO  RECE07-SYOKBN
                           MOVE    LNK-UPD-NYUGAIKBN
                                                   TO  RECE07-NYUGAIKBN
grpsys                     MOVE    LNK-UPD-HOSPNUM TO  RECE07-HOSPNUM
                           MOVE    RECE07-REC      TO  MCPDATA-REC
                           PERFORM 900-RECE07-INV-SEC
                           IF      FLG-RECE07      =   1
                               DISPLAY "RECEPRT NOT FOUND"
                               GO  TO  000-PROC-EXT
                           END-IF
                       WHEN    "7"
                           MOVE    1           TO  FLG-DAISANSYA
                       WHEN    OTHER
                               DISPLAY "RECEPRT NOT FOUND"
                           GO  TO  000-PROC-EXT
                   END-EVALUATE
               END-IF
      *
      *        第三者行為のときは医保分もチェック
               EVALUATE    LNK-UPD-HKNKBN
                   WHEN    "7"
                       INITIALIZE                  RECE07-REC
                       MOVE    "HC06"          TO  RECE07-PRTID
                       MOVE    "0"             TO  RECE07-RECEKBN
                       MOVE    LNK-UPD-SRYYM   TO  RECE07-SRYYM
                       MOVE    "1"             TO  RECE07-SYOKBN
                       MOVE    LNK-UPD-NYUGAIKBN
                                               TO  RECE07-NYUGAIKBN
grpsys                 MOVE    LNK-UPD-HOSPNUM TO  RECE07-HOSPNUM
                       MOVE    RECE07-REC      TO  MCPDATA-REC
                       PERFORM 900-RECE07-INV-SEC
                       IF      FLG-RECE07      =   1
                           IF      FLG-DAISANSYA   =   1
                               DISPLAY "RECEPRT NOT FOUND"
                               GO  TO  000-PROC-EXT
                           ELSE                          
                               MOVE    2           TO  FLG-DAISANSYA
                           END-IF
                       ELSE
                           IF      FLG-DAISANSYA   =   1
                               CONTINUE
                           ELSE                          
                               MOVE    9           TO  FLG-DAISANSYA
                           END-IF
                       END-IF
               END-EVALUATE 
      *??
           DISPLAY "FLG-DAISANSYA=" FLG-DAISANSYA
      *??
           END-IF
      *
           EVALUATE    LNK-UPD-SYORI
               WHEN    "1"
                   INITIALIZE                  JYURRK-REC
                   MOVE    LNK-UPD-HOSPNUM TO  JYURRK-HOSPNUM
                   MOVE    LNK-UPD-PTID    TO  JYURRK-PTID
                   MOVE    WRK-SYSYMD(1:6) TO  JYURRK-SRYYMD (1:6)
                   MOVE    "__"            TO  JYURRK-SRYYMD (7:2)
                   MOVE    "tbl_jyurrk"    TO  WRK-MCP-TABLE
                   MOVE    "key27"         TO  WRK-MCP-PATHNAME
                   PERFORM 200-HENSYU-SEC
               WHEN    "2"
                   INITIALIZE                  JYURRK-REC
                   MOVE    LNK-UPD-HOSPNUM TO  JYURRK-HOSPNUM
                   MOVE    LNK-UPD-PTID    TO  JYURRK-PTID
                   MOVE    LNK-UPD-NYUGAIKBN
                                           TO  JYURRK-NYUGAIKBN
                   MOVE    LNK-UPD-SRYYM   TO  JYURRK-SRYYMD (1:6)
                   MOVE    "__"            TO  JYURRK-SRYYMD (7:2)
                   MOVE    LNK-UPD-HKNKBN  TO  JYURRK-HKNKBN
                   MOVE    "tbl_jyurrk"    TO  WRK-MCP-TABLE
                   MOVE    "key28"         TO  WRK-MCP-PATHNAME
                   PERFORM 200-HENSYU-SEC
               WHEN    OTHER
                   MOVE    "1"             TO  LNK-UPD-RC
           END-EVALUATE
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *     
      *****************************************************************
      *    レセプト作成管理編集処理
      *****************************************************************
       200-HENSYU-SEC            SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    JYURRK-REC          TO  MCPDATA-REC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-JYURRK-READ-N-SEC
           ELSE
               MOVE    1                  TO  FLG-JYURRK
           END-IF
      *
           PERFORM     UNTIL       FLG-JYURRK     =   1
                       OR          LNK-UPD-RC NOT =   ZERO
               INITIALIZE                  RECEUPD-REC
               MOVE    JYURRK-HKNKBN   TO  RECEUPD-HKNKBN
      *        第三者行為で医保のみ存在するとき
               IF      FLG-DAISANSYA   =   1
                   MOVE    "0"             TO  RECEUPD-HKNKBN
               END-IF
               PERFORM 2001-RECEUPD-SEC
               IF      LNK-UPD-RC      =   "2"
                   CONTINUE
               ELSE
      *            第三者行為で医保、第三者行為が両方存在するとき
                   IF      FLG-DAISANSYA   =   9
                       INITIALIZE                  RECEUPD-REC
                       MOVE    "0"             TO  RECEUPD-HKNKBN
                       PERFORM 2001-RECEUPD-SEC
                   END-IF
      *
                   PERFORM 900-JYURRK-READ-N-SEC
               END-IF
           END-PERFORM      
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       200-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト作成管理更新処理
      *****************************************************************
       2001-RECEUPD-SEC         SECTION.
      *
           MOVE    JYURRK-HOSPNUM  TO  RECEUPD-HOSPNUM
           MOVE    JYURRK-NYUGAIKBN
                                       TO  RECEUPD-NYUGAIKBN
           MOVE    JYURRK-PTID     TO  RECEUPD-PTID
           MOVE    JYURRK-SRYKA    TO  RECEUPD-SRYKA
           MOVE    JYURRK-SRYYMD (1:6)
                                   TO  RECEUPD-SRYYM
           MOVE    SPACE           TO  RECEUPD-HKNKBN2
           MOVE    RECEUPD-REC     TO  WRK-RECEUPD-REC
      *??
           DISPLAY "RECEUPD-HKNKBN=" RECEUPD-HKNKBN " " RECEUPD-SRYYM
      *??
           PERFORM 900-RECEUPD-READ-SEC
           IF      FLG-RECEUPD     =   1
               MOVE    WRK-RECEUPD-REC     TO  RECEUPD-REC
           END-IF
      *
           MOVE    "1"             TO  RECEUPD-ONLUPD
      *
           EVALUATE    FLG-RECEUPD
               WHEN    1
                   MOVE    LNK-UPD-TERMID      TO  RECEUPD-TERMID
                   MOVE    LNK-UPD-OPID        TO  RECEUPD-OPID
                   MOVE    SMCNDATE-YMD        TO  RECEUPD-CREYMD
                   MOVE    SMCNDATE-YMD        TO  RECEUPD-UPYMD
                   MOVE    SMCNDATE-HMS        TO  RECEUPD-UPHMS
                   MOVE    "DBINSERT"          TO  MCP-FUNC
               WHEN    ZERO
                   MOVE    SMCNDATE-YMD        TO  RECEUPD-UPYMD
                   MOVE    SMCNDATE-HMS        TO  RECEUPD-UPHMS
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
           END-EVALUATE    
      *
           MOVE    RECEUPD-REC         TO  MCPDATA-REC
           MOVE    "tbl_receupd"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "2"                 TO  LNK-UPD-RC
           END-IF
           .
       2001-RECEUPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト作成管理読込
      *****************************************************************
       900-RECEUPD-READ-SEC         SECTION.
      *
           MOVE    RECEUPD-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_receupd"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_receupd"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECEUPD
                   MOVE    MCPDATA-REC         TO  RECEUPD-REC
               ELSE
                  MOVE    1                    TO  FLG-RECEUPD
               END-IF
           ELSE
               MOVE    1                       TO  FLG-RECEUPD
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_receupd"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-RECEUPD-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴読込
      *****************************************************************
       900-JYURRK-READ-N-SEC         SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-JYURRK
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               IF      JYURRK-NYUGAIKBN    =   "1"
                   GO  TO  900-JYURRK-READ-N-SEC
               END-IF
           ELSE
              MOVE    1               TO  FLG-JYURRK
              INITIALIZE                  JYURRK-REC
           END-IF
           .
       900-JYURRK-READ-N-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    レセプト管理読込
      *****************************************************************
       900-RECE07-INV-SEC         SECTION.
      *
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECE07
                   MOVE    MCPDATA-REC         TO  RECE07-REC
               ELSE
                   MOVE    1                   TO  FLG-RECE07
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-RECE07-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
