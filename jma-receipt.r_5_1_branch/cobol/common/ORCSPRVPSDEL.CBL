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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSPRVPSDEL.
      ****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : リアルタイムプレビュー処理
      *  コンポーネント名  : 中間ファイル削除処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/03/10    NACL−竹田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/01  労災・自賠責処理追加                                            
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
       01  CNT-AREA.
           03  MAX-PAGE-CNT        PIC 9(04).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PRVKANRI        PIC 9(01).
      *
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).  
               05  WRK-SYSDD           PIC 9(02).  
      *シェルＩＤ
           03  WRK-SHELLID             PIC X(08).
           03  WRK-PTID                PIC 9(16).
           03  WRK-PTNUM               PIC X(20).
      *
      *    プレビュー用ワーク追加
           03  WRK-SHELL-ARG1.
             05  WRK-SHELL-SRYYM   PIC X(06).
             05  WRK-SHELL-TERMID  PIC X(16).
             05  WRK-SHELL-SYSYMD  PIC X(08).
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
      *     
       01  WRK-PSFILE-AREA.
             05  WRK-PSFILE-FOLDER     PIC X(165).
             05  WRK-PSFILE-TBL-KEY    PIC X(08).
             05  WRK-PSFILE-RENNUM     PIC X(04).
             05  WRK-PSFILE-GROUP      PIC X(14).
             05  WRK-PSFILE-SRENNUM    PIC X(04).
             05  WRK-PSFILE-PAGE-X.
               07  WRK-PSFILE-PAGE     PIC 9(05).
       01  WRK-FILE-NAME               PIC X(200).
      *    印刷管理ＤＢのキー値
       01  WRK-PREV-AREA.
           03  WRK-CONS-PRTKANRI-TBL-KEY
                                   PIC X(08)   VALUE   "RECEPTX".
      *
      *    ジョブ管理ＤＢのキー値
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "RECEPTX".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   1.
           03  WRK-CONST-Z         PIC X(16)
               VALUE   "ZZZZZZZZZZZZZZZZ".
      *
      *    ジョブ管理ＤＢのキー値（レセプト）
           03  WRK-CONS-JOB-SHELLID1
                                   PIC X(08)   VALUE   "RECEPT1".
           03  WRK-CONS-JOB-SHELLID2
                                   PIC X(08)   VALUE   "RECEPT2".
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
       01  WRK-LNK-AREA.
           COPY    "CPORCSPRTLNK.INC".             
      *
       01  WRK-SHELL-NAME.
           05  FILLER             PIC X(16) VALUE
                                             "scripts/monthly/".
           05  WRK-SHELL-SHELL    PIC X(20).
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    患者番号構成管理情報編集
           COPY    "CPSK1009.INC".
      *
      *    プレビュー管理マスタ
           COPY    "CPPRVKANRI.INC".
      *
           COPY    "CPRCF008.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *???
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
       01  LINK-ERRCD                  PIC X(04).
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
       LINKAGE                 SECTION.
grpsys 01  LINK-HOSPNUM                PIC 9(02).
       01  LINK-PTID                   PIC 9(10).
       01  LINK-SRYYM                  PIC X(06).
       01  LINK-HKNKBN                 PIC X(01).
      *
grpsys     COPY    "COMMON-SPA".
      *
      **************************************************************************
       PROCEDURE           DIVISION
               USING
               LINK-HOSPNUM
               LINK-PTID
               LINK-SRYYM
               LINK-HKNKBN
               SPA-AREA
               .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           EXIT PROGRAM
           .
      * 
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *???
           DISPLAY "ORCSPRVPSDEL ST = hospnum = "
               LINK-HOSPNUM "ptid = "
               LINK-PTID   "sryym = "
               LINK-SRYYM
      *???     
           INITIALIZE                  WRK-AREA
                                       FLG-AREA
                                       LINK-ERRCD
      *
           INITIALIZE                      SGETTEMP-AREA
           CALL    "ORCSGETTEMP"   USING
                                   SGETTEMP-AREA
      *??
          DISPLAY "SGETTEMP-TEMPDIR="  SGETTEMP-TEMPDIR
      *??
      *    前回処理データ削除処理
           PERFORM 1002-PRVDATA-DEL-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回処理データ削除処理
      *****************************************************************
       1002-PRVDATA-DEL-SEC        SECTION.
      *
           INITIALIZE                  PRVKANRI-REC
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  PRVKANRI-TBL-KEY
           MOVE    LINK-PTID           TO  PRVKANRI-TBL-GROUP
grpsys     MOVE    SPA-HOSPNUM         TO  PRVKANRI-HOSPNUM
           MOVE    PRVKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC             =   ZERO
               MOVE    "tbl_prvkanri"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-PRVKANRI-FET-SEC
           ELSE
               MOVE    1               TO  FLG-PRVKANRI
           END-IF
      *
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    データがあれば削除
           IF      FLG-PRVKANRI         =   ZERO
               DISPLAY "PRV EXIST !!"
               DISPLAY "PAGE = [" PRVKANRI-PAGE "]"
               MOVE    PRVKANRI-PAGE   TO  MAX-PAGE-CNT
               PERFORM 10021-PRVDEL-SEC
           ELSE
               DISPLAY "PRV NO DATA !!"
           END-IF
      *
           .
       1002-PRVDATA-DEL-EXT.
      *
      *****************************************************************
      *    前回処理データ削除処理
      *****************************************************************
       10021-PRVDEL-SEC            SECTION.
      *    患者番号構成管理情報編集
      *        患者番号構成管理情報編集
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*%"                TO  SYS-1009-KBNCD
           MOVE    LINK-SRYYM          TO  SYS-1009-STYUKYMD (1:6)
                                           SYS-1009-EDYUKYMD (1:6)
           MOVE    "01"                TO  SYS-1009-STYUKYMD (7:2)
                                           SYS-1009-EDYUKYMD (7:2)
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
      *            MOVE    MCPDATA-REC         TO  SYS-1009-REC
                   MOVE    SYS-1009-PTNUMKSIKBN
                                           TO  SPA-1009-PTNUMKSIKBN
                   MOVE    SYS-1009-HJNKSIKBN
                                           TO  SPA-1009-HJNKSIKBN
                   MOVE    SYS-1009-HJNKSINENKBN
                                           TO  SPA-1009-HJNKSINENKBN
                   MOVE    SYS-1009-HJNKSIRENNUMKBN
                                           TO  SPA-1009-HJNKSIRENNUMKBN
                   MOVE    SYS-1009-HJNKSIRENNUMKETA
                                           TO  SPA-1009-HJNKSIRENNUMKETA
           ELSE
                   DISPLAY "ORCRPRVPSDEL ERR = "
                   "患者番号構成管理情報が取得できませんでした = "
                   LINK-SRYYM
           END-IF
      *    患者番号取得
           INITIALIZE                      ORCSPTIDAREA
grpsys     MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    LINK-PTID           TO  SPTID-PTID
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                           SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
                   MOVE    SPACE           TO  WRK-PTNUM
           ELSE
                   MOVE    SPTID-PTNUM     TO  WRK-PTNUM
                   DISPLAY "PTNUM = [" SPTID-PTNUM "]"
           END-IF
      *    レセプトプリントデータ削除
           PERFORM VARYING IDX FROM    20  BY  -1
                   UNTIL   IDX <       1
               IF      WRK-PTNUM (IDX:1)   NOT =   SPACE
                   MOVE    1               TO  IDX
               ELSE
                   MOVE    "Z"             TO  WRK-PTNUM (IDX:1)
               END-IF
           END-PERFORM
      *
      *
           EVALUATE    LINK-HKNKBN
               WHEN    "0"
                   MOVE    "HC06"          TO  RECE08-PRTID
               WHEN    "1"
                   MOVE    "HC21"          TO  RECE08-PRTID
               WHEN    "2"
                   MOVE    "HC23"          TO  RECE08-PRTID
           END-EVALUATE
           MOVE    "999999"            TO  RECE08-RECEYM
           MOVE    WRK-PTNUM           TO  RECE08-PTNUM
grpsys     MOVE    SPA-HOSPNUM         TO  RECE08-HOSPNUM
           MOVE    RECE08-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_receprt"       TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              NOT  =   ZERO
               DISPLAY "XC01 RECEPRT DELETE ERR [" MCP-RC "]"
           END-IF
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  PRTDATA-TBL-KEY
           MOVE    LINK-PTID           TO  PRTDATA-TBL-GROUP
grpsys     MOVE    SPA-HOSPNUM         TO  PRTDATA-HOSPNUM
           MOVE    PRTDATA-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              NOT  =   ZERO
               DISPLAY "XC01 PRTDATA DELETE ERR [" MCP-RC "]"
           END-IF
      *
      *    印刷データ削除
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  PRTKANRI-TBL-KEY
           MOVE    LINK-PTID           TO  PRTKANRI-TBL-GROUP
grpsys     MOVE    SPA-HOSPNUM         TO  PRTKANRI-HOSPNUM
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              NOT  =   ZERO
               DISPLAY "XC01 PRTKANRI DELETE ERR [" MCP-RC "]"
           END-IF
      *
      *    印刷管理削除
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  PRVKANRI-TBL-KEY
           MOVE    LINK-PTID           TO  PRVKANRI-TBL-GROUP
grpsys     MOVE    SPA-HOSPNUM         TO  PRVKANRI-HOSPNUM
           MOVE    PRVKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_prvkanri"      TO  MCP-TABLE
           MOVE    "del13"             TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              NOT  =   ZERO
               DISPLAY "XC01 PRVKANRI DELETE ERR [" MCP-RC "]"
           END-IF
      *
      *    psファイルクリア
      *****MOVE "/var/lib/jma-receipt/orca_ps/"
           MOVE    SGETTEMP-TEMPDIR
                                       TO  WRK-PSFILE-FOLDER
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  WRK-PSFILE-TBL-KEY
           MOVE    "0001"              TO  WRK-PSFILE-RENNUM
           MOVE    LINK-PTID           TO  WRK-PSFILE-GROUP
           MOVE    "1000"              TO  WRK-PSFILE-SRENNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   MAX-PAGE-CNT
               MOVE  IDX               TO  WRK-PSFILE-PAGE
      *                            TO  LNK-PRTKANRI-TBL-KEY
               STRING  WRK-PSFILE-FOLDER
                                       DELIMITED  BY  SPACE
                   WRK-PSFILE-TBL-KEY  DELIMITED  BY  SPACE
                   WRK-PSFILE-RENNUM   DELIMITED  BY  SPACE
                   WRK-PSFILE-GROUP    DELIMITED  BY  SPACE
                   WRK-PSFILE-SRENNUM  DELIMITED  BY  SPACE
                   WRK-PSFILE-PAGE-X   DELIMITED  BY  SPACE
                                       INTO   WRK-FILE-NAME
               END-STRING
               MOVE    ZERO            TO  IF-RESULT
               MOVE    WRK-FILE-NAME
                                       TO  IF-FILENAME
               DISPLAY "WRK-FILE-NAME = [" WRK-FILE-NAME "]"
               CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
           END-PERFORM
           .
       10021-PRVDEL-EXT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込（ＮＥＸＴ）
      *****************************************************************
       900-SYSKANRI-READ-N-SEC         SECTION.
      *
grpsys      PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC         TO  SYS-1009-REC 
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *     印刷管理ＤＢ読み込み
      *****************************************************************
       900-PRVKANRI-FET-SEC            SECTION.
      *
grpsys      PERFORM 900-DBFETCH-SEC       
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PRVKANRI-REC
               MOVE    ZERO                TO  FLG-PRVKANRI
           ELSE
               MOVE    1                   TO  FLG-PRVKANRI
           END-IF
           .
       900-PRVKANRI-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
