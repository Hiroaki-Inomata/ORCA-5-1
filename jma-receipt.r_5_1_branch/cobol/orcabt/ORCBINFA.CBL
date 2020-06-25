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
       PROGRAM-ID.             ORCBINFA.
      *****************************************************************
      *  ★修正時には必ずプログラム識別番号をリセットすること
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 定点調査
      *  コンポーネント名  : 感染症サーベイランス参加報告データ作成
      *  管理者            :
      *  作成日付    作業者        記述
      *  11/05/19    NACL−伊藤    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    感染症データ
           SELECT  INFEC-FILE      ASSIGN  WRK-PARA-INFECFILE
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-INFEC.
      *
      *    エラーファイル
           SELECT  ERR-FILE        ASSIGN  WRK-PARA-ERRFILE
                                   FILE    STATUS  IS  STS-ERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    感染症データ
       FD  INFEC-FILE.
       01  INFEC-REC               PIC X(3000).
      *
      *    エラーファイル
       FD  ERR-FILE.
       01  ERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *01  PRGSKBNO                PIC X(08)  VALUE "20091218".
      *01  PRGSKBNO                PIC X(08)  VALUE "20101026".
      *01  PRGSKBNO                PIC X(08)  VALUE "20101125".
      *    レイアウト変更（リアルタイム導入）
      *01  PRGSKBNO                PIC X(08)  VALUE "20110201".
      *    レイアウト変更（データ作成区分）
      *01  PRGSKBNO                PIC X(08)  VALUE "20110520".
      *    データ作成区分に空白がセットされる修正
       01  PRGSKBNO                PIC X(08)  VALUE "20130425".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-INFEC                       PIC X(02).
           03  STS-ERR                         PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
           03  FLG-INFECTION                   PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                          PIC 9(06).
           03  CNT-INFEC                       PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDU                             PIC 9(03).
           03  IDV                             PIC 9(03).
           03  IDW                             PIC 9(03).
           03  IDZ                             PIC 9(03).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-HOSPNUM            PIC 9(02).
               05  WRK-PARA-INFECFILE          PIC X(128).
               05  WRK-PARA-ERRFILE            PIC X(128).
               05  WRK-PARA-VERSION            PIC X(100).
           03  WRK-YOBI                        PIC 9(01).
           03  WRK-STR-YMD                     PIC X(08).
      *
      *    出力データ編集用
           03  WRK-REC                         PIC X(3000).
           03  WRK-REC-LENGTH                  PIC 9(04).
           03  WRK-DATA-HENSYU-AREA.
               05  WRK-DATA                    PIC X(200).
               05  WRK-NUM                     PIC ZZZZZZZZZ9.99999.
               05  WRK-NAGASA                  PIC 9(03).
               05  WRK-ST                      PIC 9(02).
               05  WRK-SYOSUU                  PIC 9(01).
               05  WRK-END                     PIC 9(01).
      *
           03  WRK-RECKBN                      PIC 9(01).
           03  WRK-INFECERR                    PIC X(200).
           03  WRK-ERRMSG                      PIC X(300).
      *
       01  WRK-CONS-AREA.
      *    コンマ
           03  WRK-KUGIRI              PIC X(01)   VALUE   ",".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報
           COPY    "CPSK1002.INC".
      *
      *    感染症プルーフ
       01  INFECTION-REC.
           COPY    "CPINFECTION.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(1000).
      ****************************************************************
       PROCEDURE                   DIVISION
                                   USING   COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL  FLG-END  =  1
      *
           PERFORM 300-TERM-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA-HOSPNUM
                                               WRK-PARA-INFECFILE
                                               WRK-PARA-ERRFILE
                                               WRK-PARA-VERSION
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           PERFORM 1001-GET-YMD-SEC
      *
           PERFORM 900-DBOPEN-SEC
      *
           OPEN    OUTPUT              INFEC-FILE
      *
      *    医療機関基本情報1001
           INITIALIZE                      SYS-1001-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SMCNDATE-YMD        TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           ELSE
               MOVE    "医療機関情報が取得できませんでした。"
                                           TO  WRK-INFECERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           IF      SYS-1001-DATAINFECTIONKBN   =   SPACE
               IF      SYS-1001-DATACREATEFLG  =   "1"
                   MOVE    "1"         TO  SYS-1001-DATAINFECTIONKBN
               ELSE
                   MOVE    "0"         TO  SYS-1001-DATAINFECTIONKBN
               END-IF
           END-IF
      *
      *    医療機関基本情報1002
           INITIALIZE                      SYS-1002-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           MOVE    SMCNDATE-YMD        TO  SYS-1002-STYUKYMD
                                           SYS-1002-EDYUKYMD
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1002-REC
           ELSE
               INITIALIZE                      SYS-1002-REC
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    月曜日算出処理
      *****************************************************************
       1001-GET-YMD-SEC            SECTION.
      *
      *    マシン日付取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    曜日指定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY3-AREA
           MOVE    "31"                TO  LNK-DAY3-IRAI
           MOVE    SMCNDATE-YMD        TO  LNK-DAY3-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY3-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY3-YOBI       TO  WRK-YOBI
           ELSE
               MOVE    ZERO                TO  WRK-YOBI
           END-IF
      *
           IF      WRK-YOBI                =   1
      *        月曜日
               MOVE    SMCNDATE-YMD        TO  WRK-STR-YMD
           ELSE
      *        マシン日付の週の月曜日は？
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    SMCNDATE-YMD        TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               COMPUTE LNK-DAY6-ZOGEN      =   (WRK-YOBI  *  -1)  +  1
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN     TO  WRK-STR-YMD
           END-IF
           .
       1001-GET-YMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      **   PERFORM 2001-INFECTION-READ-SEC
      *
      **   IF      FLG-INFECTION       =   1
      **       PERFORM 400-I-REC-SEC
      **   END-IF
           PERFORM 400-I-REC-SEC
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    感染症プルーフ処理
      *****************************************************************
       2001-INFECTION-READ-SEC     SECTION.
      *
           INITIALIZE                      INFECTION-REC
           MOVE    SPA-HOSPNUM         TO  INFECT-HOSPNUM
           MOVE    WRK-STR-YMD         TO  INFECT-SENDYMD
      *
           MOVE    INFECTION-REC       TO  MCPDATA-REC
           MOVE    "tbl_infection"     TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_infection"     TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-INFECTION-READ-SEC
           ELSE
               INITIALIZE                      INFECTION-REC
               MOVE    1                   TO  FLG-INFECTION
           END-IF
      *
           MOVE    "tbl_infection"     TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       2001-INFECTION-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   INFEC-FILE
      *
      **   IF      CNT-INFEC           =   ZERO
      **       MOVE    "処理対象のデータがありませんでした"
      **                                   TO  WRK-INFECERR
      **       PERFORM 500-ERR-HENSYU-SEC
      **   END-IF
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "ORCBINFA OUTPUT CNT : " CNT-INFEC
           DISPLAY "*** ORCBINFA END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶデータ（医療機関情報）作成処理
      *****************************************************************
       400-I-REC-SEC               SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
           MOVE    ZERO                TO  WRK-END
      *    レコード識別
           MOVE    "I"                 TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    都道府県
           MOVE    SYS-1001-PREFNUM    TO  WRK-NUM
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 4003-NUM-HENSYU-SEC
      *    データ作成年月日（マシン日付）
           MOVE    SMCNDATE-YMD        TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    データ作成時間（マシン時間）
           MOVE    SMCNDATE-HMS        TO  WRK-DATA
           MOVE    6                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    医療機関ＩＤ
           MOVE    SYS-1001-HOSPID     TO  WRK-DATA
           MOVE    24                  TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    プログラム識別番号
           MOVE    PRGSKBNO            TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    郵便番号
           MOVE    SYS-1002-POST       TO  WRK-DATA
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    日レセバージョン
           MOVE    WRK-PARA-VERSION    TO  WRK-DATA
           MOVE    100                 TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    データ作成区分
           MOVE    SYS-1001-DATAINFECTIONKBN
                                       TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END
           PERFORM 4002-MOJI-HENSYU-SEC
      *
           PERFORM 4001-FILE-WRITE-SEC
           .
       400-I-REC-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル出力処理
      *****************************************************************
       4001-FILE-WRITE-SEC         SECTION.
      *
           MOVE    WRK-REC             TO  INFEC-REC
           WRITE   INFEC-REC
           ADD     1                   TO  CNT-INFEC
           .
       4001-FILE-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（文字）
      *****************************************************************
       4002-MOJI-HENSYU-SEC        SECTION.
      *
           IF      WRK-DATA            =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDW FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDW <       1
                   IF      WRK-DATA (IDW:1)    NOT =   SPACE
                       IF      WRK-REC-LENGTH      =   ZERO
                           MOVE    WRK-DATA (1:IDW)
                                               TO  WRK-REC
                       ELSE
                           STRING  WRK-REC(1:WRK-REC-LENGTH)
                                               DELIMITED   BY  SIZE
                                   WRK-DATA(1:IDW)
                                               DELIMITED   BY  SIZE
                                   INTO        WRK-REC
                           END-STRING
                       END-IF
                       ADD     IDW                 TO  WRK-REC-LENGTH
                       MOVE    1                   TO  IDW
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-END             =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF
           .
       4002-MOJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（数字）ZZZZZZZZZ9.99999の編集
      *****************************************************************
       4003-NUM-HENSYU-SEC         SECTION.
      *
           IF      WRK-NAGASA          =   ZERO
               CONTINUE
           ELSE
      *        開始位置
               COMPUTE WRK-ST  =   11      -   WRK-NAGASA
      *
               PERFORM VARYING IDW FROM    WRK-ST    BY  1
                       UNTIL   IDW >       10
                   IF      WRK-NUM (IDW:1)   NOT =   SPACE
                        COMPUTE IDZ     =    11  -   IDW
                        IF      WRK-SYOSUU       >   ZERO
                          COMPUTE IDU     =    11  + WRK-SYOSUU
                          COMPUTE IDZ     =    IDU - IDW  +  1
                          PERFORM VARYING IDV FROM   IDU  BY  -1
                                  UNTIL   IDV <      12
                            IF  WRK-NUM(IDV:1)   =   "0"
                                COMPUTE IDZ      =   IDZ  -  1
                                IF  IDV              =    12
                                    COMPUTE IDZ      =    IDZ  -  1
                                END-IF
                            ELSE
                                MOVE    11       TO  IDV
                            END-IF
                          END-PERFORM
                        END-IF
                        STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                 DELIMITED  BY  SIZE
                               WRK-NUM(IDW:IDZ)  DELIMITED  BY  SIZE
                               INTO        WRK-REC
                        END-STRING
                        COMPUTE WRK-REC-LENGTH   =   WRK-REC-LENGTH   +
                                                     IDZ
                        MOVE    10               TO  IDW
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF
           .
       4003-NUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   ERR-FILE
           IF      STS-ERR             =   ZERO
               CLOSE   ERR-FILE
           ELSE
               OPEN    OUTPUT              ERR-FILE
               MOVE    WRK-INFECERR        TO  ERR-REC
               WRITE   ERR-REC
               CLOSE   ERR-FILE
           END-IF
      *
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORCBINFA "         DELIMITED  BY  SIZE
                   WRK-INFECERR        DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"          USING   WRK-ERRMSG
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
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
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC        SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    感染症読込
      *****************************************************************
       900-INFECTION-READ-SEC      SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-INFECTION
               MOVE    MCPDATA-REC         TO  INFECTION-REC
           ELSE
               MOVE    1                   TO  FLG-INFECTION
           END-IF
           .
       900-INFECTION-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC           SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       900-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
