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
       PROGRAM-ID.             ORAPI052BR8V3.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 住所マスタ返却
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/12/16    NACL−藤原    新規作成
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
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEERR.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ADRS          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-OUT             PIC 9(10).
           03  CNT-OUT1            PIC 9(10).
           03  CNT-XMLCOUNT        PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC X(02).
           03  WRK-PARA-TERMID     PIC X(64).
           03  WRK-PARA-PREFNUM-G.
               05  WRK-PARA-PREFNUM
                                   PIC X(02)   OCCURS  10.
           03  WRK-PARA-ERRFILE    PIC X(64).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR         PIC X(200).
           03  WRK-ERRMSG          PIC X(300).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC X(04).
               05  WRK-SMM         PIC X(02).
               05  WRK-SDD         PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY      PIC X(04).
               05  WRK-HEN-H1      PIC X(01).
               05  WRK-HEN-MM      PIC X(02).
               05  WRK-HEN-H2      PIC X(01).
               05  WRK-HEN-DD      PIC X(02).
      *
           03  WRK-THMS.
               05  WRK-THH         PIC X(02).
               05  WRK-TMM         PIC X(02).
               05  WRK-TSS         PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH     PIC X(02).
               05  WRK-HEN-T1      PIC X(01).
               05  WRK-HEN-TMM     PIC X(02).
               05  WRK-HEN-T2      PIC X(01).
               05  WRK-HEN-TSS     PIC X(02).
      *
      *    住所データ
       01  ADRSLST-REC.
           03  ADRSLST-OCC             OCCURS   1903.
               05  ADRSLST-LPUBCD      PIC X(05).
               05  ADRSLST-POST        PIC X(07).
               05  ADRSLST-EDANUM      PIC X(02).
               05  ADRSLST-EDANUM-9    REDEFINES   ADRSLST-EDANUM
                                       PIC 9(02).
               05  ADRSLST-PREFKANA    PIC X(20).
               05  ADRSLST-CITYKANA    PIC X(100).
               05  ADRSLST-TOWNKANA    PIC X(200).
               05  ADRSLST-PREFNAME    PIC X(10).
               05  ADRSLST-CITYNAME    PIC X(50).
               05  ADRSLST-TOWNNAME    PIC X(100).
               05  ADRSLST-EDITADRS-KANA
                                       PIC X(320).
               05  ADRSLST-EDITADRS-NAME
                                       PIC X(160).
               05  ADRSLST-TOWNDIVFLG  PIC X(01).
               05  ADRSLST-MANYTOWNFLG PIC X(01).
               05  ADRSLST-TOWNNAME2   PIC X(600).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    住所
       01  ADRS-REC.
           COPY    "CPADRS.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
           COPY    "MONBLOB".
           COPY    "XMLDATA.INC".
           COPY    "ORCA-BLOB".
      *
           COPY    "CPORCSAPIFILE.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
                                   USING
                                   COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  SYS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SPA-AREA
                                       CNT-AREA
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID
                                               LNK-PRTKANRI-PRTNM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               RECEERR
                                               WRK-PARA-PREFNUM-G
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORAPI052BR8V3"     TO  JOB-PGID
           MOVE    "住所マスタ返却"    TO  JOB-SHELLMSG
           PERFORM 900-CALL-ORCSJOB-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO                TO  CNT-OUT1
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       10
                   OR      WRK-PARA-PREFNUM (IDY)
                                   =       SPACE
               DISPLAY "PREFNUM=" WRK-PARA-PREFNUM (IDY)
      *
               INITIALIZE                      ADRS-REC
               MOVE    SPA-HOSPNUM         TO  ADRS-HOSPNUM
               IF      WRK-PARA-PREFNUM (IDY)
                                           =   "99"
                  MOVE    "%%"                 TO  ADRS-LPUBCD
               ELSE
                   STRING  WRK-PARA-PREFNUM (IDY)  DELIMITED   BY  SIZE
                           "%"                     DELIMITED   BY  SIZE
                                                   INTO  ADRS-LPUBCD
                   END-STRING
               END-IF
               MOVE    ADRS-REC            TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "tbl_adrs"          TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_adrs"          TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 900-ADRS-READ-N-SEC
               ELSE
                   INITIALIZE                      ADRS-REC
                   MOVE    1                   TO  FLG-ADRS
               END-IF
      *
               PERFORM     UNTIL   FLG-ADRS  =   1
                   MOVE    SPACE               TO  ADRSLST-REC
                   INITIALIZE                      ADRSLST-REC
      *
                   PERFORM         UNTIL   FLG-ADRS    =   1
                                   OR      CNT-OUT1    >=  1903
                       PERFORM 2001-ADRSLST-HENSYU-SEC
                   END-PERFORM
      *
                   MOVE    ZERO                TO  CNT-OUT1
                   ADD     1                   TO  CNT-XMLCOUNT
                   PERFORM 910-XMLEXPORT-SEC
               END-PERFORM
      *
               MOVE    "tbl_adrs"        TO  MCP-TABLE
               MOVE    "key3"            TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-PERFORM
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所マスタ編集処理
      *****************************************************************
       2001-ADRSLST-HENSYU-SEC          SECTION.
      *
           ADD     1                   TO  CNT-OUT1
                                           CNT-OUT
           MOVE    CNT-OUT1            TO  IDX
      *
           MOVE    ADRS-LPUBCD         TO  ADRSLST-LPUBCD          (IDX)
           MOVE    ADRS-POST           TO  ADRSLST-POST            (IDX)
           MOVE    ADRS-EDANUM         TO  ADRSLST-EDANUM-9        (IDX)
           MOVE    ADRS-PREFKANA       TO  ADRSLST-PREFKANA        (IDX)
           MOVE    ADRS-CITYKANA       TO  ADRSLST-CITYKANA        (IDX)
           MOVE    ADRS-TOWNKANA       TO  ADRSLST-TOWNKANA        (IDX)
           MOVE    ADRS-PREFNAME       TO  ADRSLST-PREFNAME        (IDX)
           MOVE    ADRS-CITYNAME       TO  ADRSLST-CITYNAME        (IDX)
           MOVE    ADRS-TOWNNAME       TO  ADRSLST-TOWNNAME        (IDX)
           MOVE    ADRS-EDITADRS-KANA  TO  ADRSLST-EDITADRS-KANA   (IDX)
           MOVE    ADRS-EDITADRS-NAME  TO  ADRSLST-EDITADRS-NAME   (IDX)
           MOVE    ADRS-TOWNDIVFLG     TO  ADRSLST-TOWNDIVFLG      (IDX)
           MOVE    ADRS-MANYTOWNFLG    TO  ADRSLST-MANYTOWNFLG     (IDX)
           MOVE    ADRS-TOWNNAME2      TO  ADRSLST-TOWNNAME2       (IDX)
      *
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-ADRS-READ-N-SEC
           .
       2001-ADRSLST-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    EXPORT 処理
      *****************************************************************
       910-XMLEXPORT-SEC          SECTION.
      *
           INITIALIZE                  XMLDATA-REC
           MOVE   ADRSLST-REC    TO  XMLDATA-DATA
           MOVE    1               TO  XMLDATA-MODE
           MOVE    "xml_address_masterlst"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "address_masterlst"
                                   TO  XMLDATA-RECORDNAME
           MOVE    "XMLWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XMLDATA-REC
      *
           INITIALIZE                   ORCA-BLOB
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "blob"          TO  MCP-TABLE
           MOVE    XMLDATA-OBJECT  TO  ORCA-BLOB-OBJECT
           INITIALIZE                  SGETTEMP-AREA
           STRING  "address_masterlst"
                                   DELIMITED   BY  SIZE
                   CNT-XMLCOUNT    DELIMITED   BY  SIZE
                   ".xml"          DELIMITED   BY  SIZE
                                   INTO        SGETTEMP-BASENAMES  (1)
           END-STRING
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  ORCA-BLOB-FILE
           MOVE    "BLOBEXPORT"    TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                       MCPAREA
                                       ORCA-BLOB
           .
       910-XMLEXPORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           INITIALIZE                  WRK-HEN-DATE
           MOVE    "-"                 TO  WRK-HEN-H1
           MOVE    "-"                 TO  WRK-HEN-H2
           IF      WRK-SYMD            =   SPACE   OR  "00000000"
               MOVE    "1900"              TO  WRK-HEN-YY
               MOVE    "01"                TO  WRK-HEN-MM
               MOVE    "01"                TO  WRK-HEN-DD
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
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           IF      WRK-THMS            =   SPACE
               MOVE    "00"                TO  WRK-HEN-THH
                                               WRK-HEN-TMM
                                               WRK-HEN-TSS
           ELSE
               MOVE    WRK-THH             TO  WRK-HEN-THH
               MOVE    WRK-TMM             TO  WRK-HEN-TMM
               MOVE    WRK-TSS             TO  WRK-HEN-TSS
           END-IF
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
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      CNT-OUT         =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               INITIALIZE                 ORCSAPIFILEAREA
               MOVE    SGETTEMP-TEMPDIR
                                       TO  SAPIFILE-IN-DIRNAME
               MOVE    "address_masterlst"
                                       TO  SAPIFILE-IN-FILENAME
               MOVE    "xml"           TO  SAPIFILE-IN-FILE-TYPE
               MOVE    "2"             TO  SAPIFILE-IN-KBN
               MOVE    1               TO  SAPIFILE-IN-RENNUM
               MOVE    CNT-XMLCOUNT    TO  SAPIFILE-IN-COUNT
               MOVE    SGETTEMP-TEMPDIR
                                       TO  SAPIFILE-OT-DIRNAME
               MOVE    "address_masterlst.xml"
                                       TO  SAPIFILE-OT-FILENAME
               CALL    "ORCSAPIFILE"   USING   ORCSAPIFILEAREA
      *
               IF      SAPIFILE-RC     =   0
                   CONTINUE
               ELSE
                   STRING   "ｘｍｌファイル作成エラー RC="
                                           DELIMITED   BY  SIZE
                            SAPIFILE-RC    DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-OUT         TO  JOB-UPDCNT
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORAPI052BR8V3 OUT:" CNT-OUT
           DISPLAY "*** ORAPI052BR8V3 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所マスター読込
      *****************************************************************
       900-ADRS-READ-N-SEC         SECTION.
      *
           MOVE    "tbl_adrs"        TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-ADRS
               MOVE    MCPDATA-REC     TO  ADRS-REC
           ELSE
              MOVE    1               TO  FLG-ADRS
           END-IF
           .
       900-ADRS-READ-N-EXT.
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
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
