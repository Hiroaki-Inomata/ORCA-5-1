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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCBNOMIMISV.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 薬剤画像データテーブル格納
      *                      アップグレード時のみの処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  14/09/05    NACL−伊藤     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    出力：エラープルーフファイル
           SELECT  PRF-FILE        ASSIGN
                                   ASS-PRF
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PRF.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    出力：エラープルーフファイル
       FD  PRF-FILE.
       01  PRF-REC                     PIC X(1000).
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    ファイル指定領域
       01  ASS-AREA.
           03  ASS-PRF                 PIC X(256).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PRF                 PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYORI               PIC 9(01).
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSBASE             PIC 9(01).
           03  FLG-YAKUJYO             PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SYSBASE             PIC 9(05).
           03  CNT-YAKUJYO             PIC 9(05).
           03  CNT-INS                 PIC 9(05).
           03  CNT-UPD                 PIC 9(05).
           03  CNT-NOT                 PIC 9(05).
           03  CNT-ERR                 PIC 9(05).
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME.
               05  SYS-THH             PIC 9(02).
               05  SYS-TMM             PIC 9(02).
               05  SYS-TSS             PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                    PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           03  WRK-PARA-HOSPNUM        PIC X(02).
           03  WRK-PARA-PRFFILE        PIC X(128).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-BASENAME            PIC X(1024).
           03  WRK-TEMPNAME            PIC X(1024).
           03  WRK-TEMPDIR             PIC X(1024).
           03  WRK-COMMENT             PIC X(1000).
      *
       01  PHOTO-PREFIX.
           03  PPRE-SRYCD1             PIC X(09).
           03  PPRE-HIFUN              PIC X(01)   VALUE   "-".
           03  PPRE-SRYCD2             PIC X(09).
      *
      *    画像イメージ作成パラメタ
           COPY    "CPORCSMIAREA.INC".
      *
      *    コマンド
       01  COMMAND                     PIC X(1000).
      *
      *    返却値
       01  RESULT                      PIC S9(09).
      *
      *    画像ファイルフォルダ名
       01  WRK-CONS-AREA.
           03  WRK-CONS-MEDIMGDIR  PIC X(08)   VALUE   "medimage".
           03  WRK-CONS-PATH-SIZE  PIC 9(05)   VALUE   1024.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム基本マスタ
       01  SYSBASE-REC.
           COPY    "CPSYSBASE.INC".
      *
      *    薬剤情報マスタ
           COPY    "CPYAKUJYO.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    スパ領域
           COPY    "COMMON-SPA".
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ファイル存在チェック
           COPY    "CPORCSFILECHK.INC".
      *
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(256).
      *
      ****************************************************************
      *
       PROCEDURE                   DIVISION    USING
           COMMAND-PARAM
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF      FLG-SYORI           =   1
               PERFORM 200-MAIN-1-SEC  UNTIL   FLG-END =   1
           ELSE
               PERFORM 200-MAIN-2-SEC  UNTIL   FLG-END =   1
           END-IF
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           DISPLAY "*** ORCBNOMIMISV START ***"
           INITIALIZE  ASS-AREA
           INITIALIZE  STS-AREA
           INITIALIZE  FLG-AREA
           INITIALIZE  IDX-AREA
           INITIALIZE  WRK-AREA
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
               INTO   WRK-PARA-HOSPNUM
                      WRK-PARA-PRFFILE
           END-UNSTRING
      *
           IF      WRK-PARA-HOSPNUM    =   "**"
               MOVE    1                   TO  FLG-SYORI
               MOVE    1                   TO  SPA-HOSPNUM
           ELSE
               MOVE    2                   TO  FLG-SYORI
               MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           END-IF
      *
           PERFORM 100-DBOPEN-SEC
      *
           IF      WRK-PARA-PRFFILE    =   SPACE
               MOVE    1                   TO  FLG-END
               GO  TO  100-INIT-EXT
           ELSE
               MOVE    WRK-PARA-PRFFILE    TO  ASS-PRF
               OPEN    OUTPUT  PRF-FILE
               IF      STS-PRF         NOT =   ZERO
                   DISPLAY "PRF-FILE OPEN STATS [ " STS-PRF " ]"
                   MOVE    1                   TO  FLG-END
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           PERFORM 1001-SET-VALUE-SEC
      *
           IF      FLG-SYORI           =   1
               INITIALIZE  SYSBASE-REC
               MOVE    SYSBASE-REC         TO  MCPDATA-REC
               MOVE    "tbl_sysbase"       TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sysbase"       TO  MCP-TABLE
                   MOVE    "all"               TO  MCP-PATHNAME
                   PERFORM 970-SYSBASE-READ-SEC
               ELSE
                   INITIALIZE  SYSBASE-REC
                   MOVE    1               TO  FLG-SYSBASE
               END-IF
           ELSE
               INITIALIZE  YAKUJYO-REC
               MOVE    SPA-HOSPNUM         TO  YAKUJYO-HOSPNUM
               MOVE    YAKUJYO-REC         TO  MCPDATA-REC
               MOVE    "tbl_yakujyo"       TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_yakujyo"       TO  MCP-TABLE
                   MOVE    "all"               TO  MCP-PATHNAME
                   PERFORM 970-YAKUJYO-READ-SEC
               ELSE
                   INITIALIZE  YAKUJYO-REC
                   MOVE    1               TO  FLG-YAKUJYO
               END-IF
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期値セット
      *****************************************************************
       1001-SET-VALUE-SEC          SECTION.
      *
      *    一時ファイル名取得
           INITIALIZE  SGETTEMP-AREA
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-TEMPDIR    TO  WRK-TEMPDIR
      *
      *    更新日取得
           INITIALIZE  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
      *    処理時間
           MOVE    SMCNDATE-HMS        TO  SYS-TIME
           .
       1001-SET-VALUE-EXT.
           EXIT.
      *
      *****************************************************************
      *    主１　処理
      *****************************************************************
       200-MAIN-1-SEC              SECTION.
      *
           MOVE    SYSBASE-HOSPNUM     TO  SPA-HOSPNUM
      *
           INITIALIZE  YAKUJYO-REC
           MOVE    SPA-HOSPNUM         TO  YAKUJYO-HOSPNUM
           MOVE    YAKUJYO-REC         TO  MCPDATA-REC
           MOVE    "tbl_yakujyo"       TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_yakujyo"       TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 970-YAKUJYO-READ-SEC
           ELSE
               INITIALIZE  YAKUJYO-REC
               MOVE    1               TO  FLG-YAKUJYO
           END-IF
      *
           PERFORM 200-MAIN-2-SEC  UNTIL   FLG-END =   1
      *
           MOVE    "tbl_yakujyo"       TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    "tbl_sysbase"       TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 970-SYSBASE-READ-SEC
           IF      FLG-SYSBASE         =   1
               MOVE    1                   TO  FLG-END
           ELSE
               MOVE    ZERO                TO  FLG-END
           END-IF
           .
       200-MAIN-1-EXT.
           EXIT.
      *
      *****************************************************************
      *    主２　処理
      *****************************************************************
       200-MAIN-2-SEC              SECTION.
      *
           PERFORM 2001-MAIN-SEC
      *
           MOVE    "tbl_yakujyo"       TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 970-YAKUJYO-READ-SEC
           IF      FLG-YAKUJYO         =   1
               MOVE    1                   TO  FLG-END
           END-IF
           .
       200-MAIN-2-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       2001-MAIN-SEC               SECTION.
      *
           IF      YAKUJYO-PHOTO       =   SPACE
               CONTINUE
           ELSE
      *        画像ファイルチェック
               INITIALIZE  ORCSFILECHKAREA
               MOVE    YAKUJYO-PHOTO       TO  FILECHK-NAME
               CALL    "ORCSFILECHK"   USING   ORCSFILECHKAREA
               IF      FILECHK-RC          =   0
                   PERFORM 2001-GET-BASENAME-SEC
      *            画像ファイル複写コマンド
                   MOVE    LOW-VALUE           TO  COMMAND
                   STRING  "cp -p "            DELIMITED BY SIZE
                           YAKUJYO-PHOTO       DELIMITED BY SPACE
                           " "                 DELIMITED BY SIZE
                           WRK-TEMPDIR         DELIMITED BY SPACE
                           "/"                 DELIMITED BY SIZE
                           WRK-CONS-MEDIMGDIR  DELIMITED BY SPACE
                           "/"                 DELIMITED BY SIZE
                           WRK-BASENAME        DELIMITED BY SPACE
                                       INTO    COMMAND
                   END-STRING
                   CALL    "system"        USING   COMMAND
                                       RETURNING   RESULT
                   IF      RESULT              =   ZERO
                       INITIALIZE  ORCSMIAREA
                       MOVE    YAKUJYO-SRYCD       TO  ORCSMI-SRYCD
                       MOVE    1                   TO  ORCSMI-IMAGEKBN
                       MOVE    WRK-TEMPDIR         TO  ORCSMI-TEMPDIR
                       MOVE    WRK-BASENAME        TO  ORCSMI-FILENAME
                       CALL    "ORCSMISAVE"    USING   ORCSMIAREA
                                                       SPA-AREA
                       IF      ORCSMI-RETCD        =   ZERO
                           COMPUTE CNT-INS             =   CNT-INS
                                                       +   1
                           PERFORM 2002-SET-PHOTO-SEC
                       ELSE
                           COMPUTE CNT-ERR             =   CNT-ERR
                                                       +   1
                           IF      ORCSMI-RETCD        =   2
                               MOVE    4                   TO  FLG-ERR
                           ELSE
                               MOVE    1                   TO  FLG-ERR
                           END-IF
                           PERFORM 800-PROOF-SEC
                       END-IF
                   ELSE
                       DISPLAY "system result [" RESULT "]"
                       DISPLAY "command       [" COMMAND "]"
                       COMPUTE CNT-ERR             =   CNT-ERR +   1
                       MOVE    2                   TO  FLG-ERR
                       PERFORM 800-PROOF-SEC
                   END-IF
               ELSE
                   COMPUTE CNT-NOT             =   CNT-NOT +   1
                   MOVE    3                   TO  FLG-ERR
                   PERFORM 800-PROOF-SEC
               END-IF
           END-IF
           .
       2001-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル名取得処理
      *****************************************************************
       2001-GET-BASENAME-SEC       SECTION.
      *
           MOVE    YAKUJYO-PHOTO       TO  WRK-BASENAME
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >=  WRK-CONS-PATH-SIZE )
                    OR   ( WRK-BASENAME (IDX0 : )  =   SPACE )
      *
               IF    ( WRK-BASENAME (IDX0 : 1 ) =   "/" )
                   MOVE    WRK-BASENAME (IDX0 + 1 : )
                                       TO  WRK-TEMPNAME
               END-IF
      *
           END-PERFORM
      *
           MOVE    WRK-TEMPNAME        TO  WRK-BASENAME
           .
       2001-GET-BASENAME-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報ファイル名更新処理
      *****************************************************************
       2002-SET-PHOTO-SEC          SECTION.
      *
      *    画像ファイル名を加工
           MOVE    YAKUJYO-SRYCD       TO  PPRE-SRYCD1
                                           PPRE-SRYCD2
           IF      WRK-BASENAME (1:19) =   PHOTO-PREFIX
               MOVE    WRK-BASENAME (11:)  TO  YAKUJYO-PHOTO
           ELSE
               MOVE    WRK-BASENAME        TO  YAKUJYO-PHOTO
           END-IF
           MOVE    SPA-SYSYMD          TO  YAKUJYO-UPYMD
           MOVE    SYS-TIME            TO  YAKUJYO-UPHMS
           MOVE    YAKUJYO-REC         TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_yakujyo"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           IF      MCP-RC          NOT =   ZERO
                DISPLAY "YAKUJYO UPD ERROR [" YAKUJYO-SRYCD "] "
                        "MCP-RC [" MCP-RC "]"
           END-IF
           COMPUTE CNT-UPD             =   CNT-UPD +   1
           .
       2002-SET-PHOTO-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      FLG-SYORI           =   1
               MOVE    "tbl_sysbase"       TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
           IF      FLG-SYORI           =   2
               MOVE    "tbl_yakujyo"       TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
      *
           IF      FLG-SYORI           =   1
               DISPLAY "*** ORCBNOMIMISV SYSBASE   INPUT :"
                       CNT-SYSBASE " ***"
           ELSE
               DISPLAY "*** ORCBNOMIMISV HOSPNUM         :"
                       SPA-HOSPNUM " ***"
           END-IF
           DISPLAY "*** ORCBNOMIMISV YAKUJYO   INPUT :"
                   CNT-YAKUJYO " ***"
           DISPLAY " "
           DISPLAY "*** ORCBNOMIMISV MED_IMAGE INSERT:" CNT-INS " ***"
           DISPLAY "*** ORCBNOMIMISV YAKUJYO   UPDATE:" CNT-UPD " ***"
           DISPLAY " "
           IF      CNT-NOT             >   ZERO
               DISPLAY "*** ORCBNOMIMISV IMAGE NOT FOUND :"
                       CNT-NOT " ***"
           END-IF
           IF      CNT-ERR             >   ZERO
               DISPLAY "*** ORCBNOMIMISV COPY,etc  ERROR :"
                       CNT-ERR " ***"
           END-IF
      *
           CLOSE   PRF-FILE
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORCBNOMIMISV END ***"
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    プルーフ出力
      *****************************************************************
       800-PROOF-SEC               SECTION.
      *
           MOVE    SPACE               TO  PRF-REC
           MOVE    SPACE               TO  WRK-COMMENT
      *
           EVALUATE    FLG-ERR
               WHEN    1
                   STRING  "ORCSMISAVE ERROR," DELIMITED BY SIZE
                           ORCSMI-RETCD        DELIMITED BY SIZE
                                               INTO  WRK-COMMENT
                   END-STRING
               WHEN    2
                   STRING  "cp command ERROR," DELIMITED BY SIZE
                           RESULT              DELIMITED BY SIZE
                           ","                 DELIMITED BY SIZE
                           COMMAND             DELIMITED BY SIZE
                                               INTO  WRK-COMMENT
                   END-STRING
               WHEN    3
                   STRING  "image file not found," DELIMITED BY SIZE
                           YAKUJYO-PHOTO           DELIMITED BY SPACE
                                               INTO  WRK-COMMENT
                   END-STRING
               WHEN    4
                   STRING  "ORCSMISAVE ERROR " DELIMITED BY SIZE
                           "SIZE OVER,"        DELIMITED BY SIZE
                           ORCSMI-RETCD        DELIMITED BY SIZE
                           ","                 DELIMITED BY SIZE
                           YAKUJYO-PHOTO       DELIMITED BY SPACE
                                               INTO  WRK-COMMENT
                   END-STRING
           END-EVALUATE
      *
           STRING  YAKUJYO-SRYCD       DELIMITED BY SIZE
                   ","                 DELIMITED BY SIZE
                   WRK-COMMENT         DELIMITED BY SIZE
                                       INTO  PRF-REC
           END-STRING
      *
           WRITE   PRF-REC
           MOVE    ZERO                TO  FLG-ERR
           .
       800-PROOF-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム基本マスター次読込
      *****************************************************************
       970-SYSBASE-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYSBASE-REC
               MOVE    ZERO                TO  FLG-SYSBASE
               COMPUTE CNT-SYSBASE         =   CNT-SYSBASE   +   1
           ELSE
               INITIALIZE  SYSBASE-REC
               MOVE    1                   TO  FLG-SYSBASE
           END-IF
           .
       970-SYSBASE-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報マスター次読込
      *****************************************************************
       970-YAKUJYO-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  YAKUJYO-REC
               MOVE    ZERO                TO  FLG-YAKUJYO
               COMPUTE CNT-YAKUJYO         =   CNT-YAKUJYO   +   1
           ELSE
               INITIALIZE  YAKUJYO-REC
               MOVE    1                   TO  FLG-YAKUJYO
           END-IF
           .
       970-YAKUJYO-READ-EXT.
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
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　更新処理
      *****************************************************************
       990-DBCOMMIT-SEC            SECTION.
      *
      *    ＤＢ　クローズ処理
           PERFORM 900-DBDISCONNECT-SEC
      *    ＤＢ　オープン処理
           PERFORM 100-DBOPEN-SEC
           .
       990-DBCOMMIT-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    "DBSTART"           TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
