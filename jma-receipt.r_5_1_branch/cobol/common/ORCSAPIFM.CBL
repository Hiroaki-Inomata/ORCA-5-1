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
       PROGRAM-ID.             ORCSAPIFM.
      *****************************************************************
      *  システム名        : ORCA
      *  サブシステム名    : 共通
      *  コンポーネント名  : API用帳票データレスポンス作成
      *  作成日付    作業者        記述
      *  09/02/12    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      * 印刷データは削除しない。PRTKANRI-PRTFLGに"D"を設定して残しておく。
      * 一括削除時に削除を行う。
      * "D"を設定することで、再印刷画面には表示されない。
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPASGNPARA.INC"    REPLACING   //ASGN//
                                       BY          //UTF//.
      *
       01  FLG-AREA.
           03  FLG-PRTKANRI            PIC 9(01).
           03  FLG-PRTDATA             PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-YMD                 PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-DATE.
               05  WRK-DATE-YY         PIC 9(04).
               05  WRK-DATE-FL1        PIC X(01).
               05  WRK-DATE-MM         PIC 9(02).
               05  WRK-DATE-FL2        PIC X(01).
               05  WRK-DATE-DD         PIC 9(02).
           03  WRK-HMS.
               05  WRK-HMS-HH          PIC 9(02).
               05  WRK-HMS-MM          PIC 9(02).
               05  WRK-HMS-SS          PIC 9(02).
           03  WRK-TIME.
               05  WRK-TIME-HH         PIC 9(02).
               05  WRK-TIME-FL1        PIC X(01).
               05  WRK-TIME-MM         PIC 9(02).
               05  WRK-TIME-FL2        PIC X(01).
               05  WRK-TIME-SS         PIC 9(02).
      *
           COPY    "COMMON-SPA"        REPLACING   //SPA-//
                                       BY          //TSPA-//.
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
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    オンライン帳票名保存
           COPY    "CPORCSRPLNK.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    ＡＰＩパラメタ
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
      *
      *    帳票データ取得API用パラメタ
           COPY    "CPPARA001.INC".
      *
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
           COPY    "ORCA-BLOB".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
            COPY    "CPORCSAPIFM.INC".
            COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           SAPIFM-AREA
           SPA-AREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  APIPARA-REC
           INITIALIZE                  SRPLNK-AREA
           INITIALIZE                  PARA001-REC
           INITIALIZE                  PRTKANRI-REC
      *
      *
           MOVE    LOW-VALUE       TO  SAPIFM-OBJECT
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( SAPIFM-ERRCD        =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
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
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           PERFORM 1001-INIT-CHECK-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    電文項目チェック処理
      *****************************************************************
       1001-INIT-CHECK-SEC             SECTION.
      *
           IF    ( SAPIFM-ERRCD       =   SPACE )
               IF    ( SPA-PRTCONF    =   "3" )
                   PERFORM 10012-PRTKANRI-CHECK-SEC
               ELSE
                   PERFORM 10011-PRTKANRI-CHECK-SEC
               END-IF
           END-IF
      *
           .
       1001-INIT-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    帳票管理確認処理
      *****************************************************************
       10011-PRTKANRI-CHECK-SEC            SECTION.
      *
           IF    ( SAPIFM-DATAID       NOT =   SPACE )
               INITIALIZE                      PARA001-REC
               MOVE    SAPIFM-DATAID       TO  PARA001-DATAID
           ELSE
               PERFORM 900-APIPARA-KEY2-SEL-SEC
               IF    ( FLG-APIPARA         =   ZERO )
                   MOVE    APIPARA-DATA-REC
                                           TO  PARA001-REC
      *
               ELSE
                   MOVE    "0001"          TO  SAPIFM-ERRCD
               END-IF
           END-IF
      *
           IF    ( SAPIFM-ERRCD        =   SPACE )
      *
               INITIALIZE                  PRTKANRI-REC
               MOVE    SPA-HOSPNUM     TO  PRTKANRI-HOSPNUM
               UNSTRING    PARA001-DATAID  DELIMITED BY "#"
               INTO                        PRTKANRI-TBL-KEY
                                           PRTKANRI-TBL-UUID
                                           PRTKANRI-TBL-GROUP
                                           PRTKANRI-RENNUM
                                           PRTKANRI-SHORI-RENNUM
               END-UNSTRING
      *
               PERFORM 900-PRTKANRI-KEY-SEL-SEC
               IF    ( FLG-PRTKANRI        =   ZERO )
                   PERFORM 900-PRTDATA-KEY3-SEL-SEC
                   MOVE    PRTDATA-PTID    TO  SPA-PTID
               ELSE
                   MOVE    "0001"          TO  SAPIFM-ERRCD
               END-IF
           END-IF
      *
           .
       10011-PRTKANRI-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    帳票管理確認処理
      *****************************************************************
       10012-PRTKANRI-CHECK-SEC            SECTION.
      *
      D    DISPLAY "SPA-PUTTYPE[" SPA-PUTTYPE "]"
      *
           IF    ( SPA-PUTTYPE         =   "PRINTED" )
               CONTINUE
           ELSE
               MOVE    "0001"              TO  SAPIFM-ERRCD
           END-IF
      *
           IF    ( SAPIFM-ERRCD        =   SPACE )
               PERFORM 900-APIPARA-KEY2-SEL-SEC
               IF    ( FLG-APIPARA         =   ZERO )
                   MOVE    APIPARA-DATA-REC
                                           TO  SRPLNK-AREA
                   INITIALIZE                      PRTKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  PRTKANRI-HOSPNUM
                   MOVE    SRPLNK-VAL      TO  PRTKANRI-API-ID
                   MOVE    SRPLNK-TITLE    TO  PRTKANRI-TITLE
                   MOVE    SPA-PRT-GROUP   TO  PRTKANRI-TBL-GROUP
                   MOVE    SPA-PRT-UUID    TO  PRTKANRI-TBL-UUID
                   MOVE    SPACE           TO  PRTKANRI-TBL-KEY
      *
               ELSE
                   MOVE    "0001"          TO  SAPIFM-ERRCD
               END-IF
           END-IF
      *
           .
       10012-PRTKANRI-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           PERFORM 900-XML-WRITE-SEC
      *
           IF    ( SAPIFM-DATAID       =   SPACE )
               CALL    "ORCHCDUMMY"        USING
                                           SPA-AREA
           END-IF
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-DATE-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-DATE
      *
           IF    ( WRK-SYMD        NOT  =   SPACE )
               INITIALIZE                  WRK-DATE
               MOVE    WRK-SYY             TO  WRK-DATE-YY
               MOVE    WRK-SMM             TO  WRK-DATE-MM
               MOVE    WRK-SDD             TO  WRK-DATE-DD
               MOVE    "-"                 TO  WRK-DATE-FL1
               MOVE    "-"                 TO  WRK-DATE-FL2
               IF    ( WRK-SYMD            =   "99999999" )
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-IF
           END-IF
           .
       800-DATE-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-SYMD-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-SYMD
      *
           IF    ( WRK-DATE        NOT  =   SPACE )
               MOVE    WRK-DATE-YY     TO  WRK-SYY
               MOVE    WRK-DATE-MM     TO  WRK-SMM
               MOVE    WRK-DATE-DD     TO  WRK-SDD
           END-IF
      *
           .
       800-SYMD-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           IF  ( WRK-SYMD          =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD (1:8)
                                   TO  WRK-EDTYMD1
           ELSE
               INITIALIZE              STS-AREA-DAY
               INITIALIZE              LNK-DAY2-AREA
               MOVE    "21"        TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
               IF    ( STS-DAY-RC1     =   ZERO )
                   MOVE    LNK-DAY2-EDTYMD1
                                   TO  WRK-EDTYMD1
               END-IF
           END-IF
      *
           .
       800-HIZUKE-EXT.
           EXIT.
      *****************************************************************
      *    時刻編集処理
      *****************************************************************
       800-TIME-SEC                    SECTION.
      *
           INITIALIZE                      WRK-TIME
           MOVE    WRK-HMS-HH          TO  WRK-TIME-HH
           MOVE    WRK-HMS-MM          TO  WRK-TIME-MM
           MOVE    WRK-HMS-SS          TO  WRK-TIME-SS
           MOVE    ":"                 TO  WRK-TIME-FL1
           MOVE    ":"                 TO  WRK-TIME-FL2
      *
           .
       800-TIME-EXT.
           EXIT.
      *
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC          SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    月末日取得処理
      *****************************************************************
       800-GETUMATU-SEC        SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       800-GETUMATU-EXT.
            EXIT.
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       900-XML-WRITE-SEC           SECTION.
      *
           MOVE    SAPIFM-ERRCD        TO  SPA-ERRCD
           MOVE    SPA-AREA            TO  TSPA-AREA
           CALL    "ORCSAPIFMGEN"          USING
                                           PRTKANRI-REC
                                           TSPA-AREA
           MOVE    TSPA-WORK           TO  UTFPARA
           MOVE    TSPA-AREA           TO  SPA-AREA
      *
           INITIALIZE                      ORCA-BLOB
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "blob"              TO  MCP-TABLE
           MOVE     UTFPARA            TO  ORCA-BLOB-FILE
      *
           MOVE    "BLOBIMPORT"        TO  MCP-FUNC
           MOVE    "blob"              TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "MONFUNC"               USING
                                           MCPAREA
                                           ORCA-BLOB
      *
           IF    ( MCP-RC              =   ZERO )
               MOVE    ORCA-BLOB-OBJECT    TO  SAPIFM-OBJECT
      *
           ELSE
               DISPLAY "blob import failure = " MCP-RC
           END-IF
      *
           CALL    "CBL_DELETE_FILE"   USING
                                       UTFPARA
      *
           .
       900-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    帳票管理検索処理
      *****************************************************************
       900-PRTKANRI-KEY-SEL-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-PRTKANRI
      *
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PRTKANRI-REC
           ELSE
               INITIALIZE                  PRTKANRI-REC
               MOVE    1               TO  FLG-PRTKANRI
           END-IF
      *
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PRTKANRI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    帳票データ検索処理
      *****************************************************************
       900-PRTDATA-KEY3-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-PRTDATA
      *
           INITIALIZE                      PRTDATA-REC
           MOVE    SPA-HOSPNUM         TO  PRTDATA-HOSPNUM
           MOVE    PRTKANRI-TBL-KEY    TO  PRTDATA-TBL-KEY
           MOVE    PRTKANRI-RENNUM     TO  PRTDATA-RENNUM
           MOVE    PRTKANRI-TBL-GROUP  TO  PRTDATA-TBL-GROUP
           MOVE    PRTKANRI-TBL-UUID   TO  PRTDATA-TBL-UUID
           MOVE    PRTKANRI-SHORI-RENNUM
                                       TO  PRTDATA-SHORI-RENNUM
           MOVE    PRTDATA-REC         TO  MCPDATA-REC
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PRTDATA-REC
           ELSE
               INITIALIZE                  PRTDATA-REC
               MOVE    1               TO  FLG-PRTDATA
           END-IF
      *
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PRTDATA-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    APIパラメタ検索処理
      *****************************************************************
       900-APIPARA-KEY2-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-APIPARA
      *
           INITIALIZE                  APIPARA-REC
      *
           MOVE    SPA-HOSPNUM     TO  APIPARA-HOSPNUM
      *
           IF    ( SPA-PRTCONF     =   "3" )
               MOVE    "RPLNK"     TO  APIPARA-GYOUMUID
               MOVE    SPA-PRT-UUID TO APIPARA-TERMID
           ELSE
               MOVE    "api001"    TO  APIPARA-GYOUMUID
               MOVE    SPA-TERMID  TO  APIPARA-TERMID
           END-IF
           MOVE    SPA-OPID        TO  APIPARA-FILEMEI
      *
           MOVE    APIPARA-REC     TO  MCPDATA-REC
           MOVE    "tbl_api_para"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  APIPARA-REC
               MOVE    ZERO        TO  FLG-APIPARA
           ELSE
               MOVE    1           TO  FLG-APIPARA
               INITIALIZE              APIPARA-REC
           END-IF
      *
           MOVE    "tbl_api_para"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-APIPARA-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
