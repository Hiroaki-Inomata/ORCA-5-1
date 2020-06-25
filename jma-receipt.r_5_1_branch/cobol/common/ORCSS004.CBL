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
       PROGRAM-ID.             ORCSS004.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : 診療行為取得
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/07/12    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
           SELECT  SS00401-FILE        ASSIGN   SS00401PARA
                                       ORGANIZATION    IS  INDEXED
                                       ACCESS   MODE   IS  DYNAMIC
                                       RECORD   KEY    IS  SS00401-KEY
                                       FILE     STATUS IS  STS-SS00401.
      *
           SELECT  TENSU-FILE          ASSIGN   SS00402PARA
                                       ORGANIZATION    IS  INDEXED
                                       ACCESS   MODE   IS  DYNAMIC
                                       RECORD   KEY    IS  TNS-KEY
                                       FILE     STATUS IS  STS-TENSU.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       FD  SS00401-FILE.
       01  SS00401-REC.
           03  SS00401-KEY.
               05  SS00401-SRYCD       PIC X(09).
      *
       FD  TENSU-FILE.
           COPY    "CPTENSU.INC".
      *
       WORKING-STORAGE                 SECTION.
      *
           COPY    "CPCOMMONDAT5.INC".
      *
           COPY    "CPASGNPARA.INC"    REPLACING
                                       //ASGN// BY //SS00401//.
      *
           COPY    "CPASGNPARA.INC"    REPLACING
                                       //ASGN// BY //SS00402//.
      *
       01  STS-AREA.
           03  STS-SS00401                     PIC X(02).
           03  STS-TENSU                       PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SRYACT                      PIC 9(01).
           03  FLG-INPUTCD                     PIC 9(01).
           03  FLG-TENSU                       PIC 9(01).
           03  FLG-SS00401                     PIC 9(01).
           03  FLG-END                         PIC 9(01).
           03  FLG-TARGET                      PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                            PIC 9(05).
           03  IDX-ZAINUM                      PIC 9(05).
           03  IDX-SRYCD                       PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYCD                       PIC X(09).
           03  WRK-STSRYYM                     PIC X(06).
           03  WRK-EDSRYYM                     PIC X(06).
           03  WRK-YMD                         PIC X(08).
           03  WRK-ZOGENPTN                    PIC X(01).
           03  WRK-ZOGEN                       PIC S9(05).
      *
      *
       01  CONST-AREA.
           03  CONST-ALL                       PIC X(01)   VALUE "0".
           03  CONST-YAKUZAI                   PIC X(01)   VALUE "1".
           03  CONST-KENSA                     PIC X(01)   VALUE "2".
           03  CONST-SRYCD-MAX                 PIC 9(05)   VALUE 5.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    入力コード
       01  ICD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
       01 UIDIO-AREA.
           03  C-RET                   PIC X(2).
           03  C-UID                   PIC X(36).
       01  ST                          PIC 9(2).
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                        SECTION.
      *
           COPY    "CPORCSS004.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                       DIVISION    USING
           SS004-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           PERFORM 200-MAIN-SEC
      *
      *    終了処理
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       ST
                                       UIDIO-AREA
           MOVE    SPA-HOSPNUM         TO  TEIKI01PARA-HOSPNUM
           MOVE    "SS004001"          TO  TEIKI01PARA-FILE-ID
           MOVE    C-UID               TO  TEIKI01PARA-TERMID
           MOVE    TEIKI01PARA-BASENAME TO SS00401PARA
      *
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       ST
                                       UIDIO-AREA
           MOVE    SPA-HOSPNUM         TO  TEIKI01PARA-HOSPNUM
           MOVE    "SS004TNS"          TO  TEIKI01PARA-FILE-ID
           MOVE    C-UID               TO  TEIKI01PARA-TERMID
           MOVE    TEIKI01PARA-BASENAME TO SS00402PARA
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    SS00401PARA     TO  SGETTEMP-BASENAMES (1)
           MOVE    SS00402PARA     TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  SS00401PARA
           MOVE    SGETTEMP-FULLNAMES (2)
                                   TO  SS00402PARA
      *
           OPEN    OUTPUT              SS00401-FILE
           CLOSE   SS00401-FILE
      *
           IF    ( SS004-KIKAN         =   ZERO )
               MOVE    "000000"        TO  WRK-STSRYYM
               MOVE    "999999"        TO  WRK-EDSRYYM
           ELSE
               MOVE    SPA-SYSYMD (1:6) TO WRK-EDSRYYM
               MOVE    SPA-SYSYMD      TO  WRK-YMD
               MOVE    "2"             TO  WRK-ZOGENPTN
               COMPUTE WRK-ZOGEN   = ( SS004-KIKAN      -   1 )  *   -1
               PERFORM 800-CALENDAR-SEC
               MOVE    WRK-YMD (1:6)   TO WRK-STSRYYM
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM 2101-INPUCD-EDIT-SEC
      *
           PERFORM 2101-SRYACT-EDIT-SEC
      *
           PERFORM 2101-SS004-OT-EDIT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードから診療行為コード取得処理
      *****************************************************************
       2101-INPUCD-EDIT-SEC            SECTION.
      *
           OPEN    I-O                 SS00401-FILE
      *
           PERFORM 900-ICD-ALL-SEL-SEC
      *
           PERFORM UNTIL ( FLG-INPUTCD NOT =   ZERO )
      *
               MOVE    ICD-SRYCD       TO  WRK-SRYCD
               PERFORM 220-SS00401-WRITE-SEC
      *
               PERFORM 900-ICD-ALL-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_inputcd"   TO   MCP-TABLE
           MOVE    "all"           TO   MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   SS00401-FILE
      *
           .
       2101-INPUCD-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    対象診療コードファイル作成
      *****************************************************************
       2101-SRYACT-EDIT-SEC            SECTION.
      *
           OPEN    I-O                 SS00401-FILE
      *
           PERFORM 900-SRYACT-KEY16-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SRYACT  NOT =   ZERO )
      *
               PERFORM VARYING IDX-SRYCD   FROM    1   BY  1
                       UNTIL ( IDX-SRYCD   >   CONST-SRYCD-MAX )
                        OR   ( SRY-SRYCD (IDX-SRYCD)   =   SPACE )
                   MOVE    SRY-SRYCD (IDX-SRYCD)
                                       TO  WRK-SRYCD
                   PERFORM 220-SS00401-WRITE-SEC
               END-PERFORM
      *
               PERFORM 900-SRYACT-KEY16-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO   MCP-TABLE
           MOVE    "key16"             TO   MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   SS00401-FILE
      *
           .
       2101-SRYACT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コード中間ファイル出力処理
      *****************************************************************
       220-SS00401-WRITE-SEC           SECTION.
      *
           MOVE  ZERO              TO  FLG-TARGET
      *
           EVALUATE    SS004-SRYCDKBN
           WHEN    CONST-ALL
               MOVE    1           TO  FLG-TARGET
           WHEN    CONST-KENSA
               IF  ( WRK-SRYCD(1:2)    =  "16" )
                OR ( WRK-SRYCD(1:3)    =  "002" )
                   MOVE    1       TO  FLG-TARGET
               END-IF
           WHEN    CONST-YAKUZAI
               IF  ( WRK-SRYCD(1:1)    =  "6" )
                   MOVE    1       TO  FLG-TARGET
               END-IF
           END-EVALUATE
      *
           IF    ( FLG-TARGET          =   1 )
               PERFORM 900-SS00401-READ-DYN-SEC
               IF    ( FLG-SS00401 =   1 )
                   INITIALIZE              SS00401-REC
                   MOVE    WRK-SRYCD   TO  SS00401-SRYCD
                   WRITE   SS00401-REC
               END-IF
           END-IF
      *
           .
      *
       220-SS00401-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    入力コードから診療行為コード取得処理
      *****************************************************************
       2101-SS004-OT-EDIT-SEC          SECTION.
      *
           OPEN    INPUT   SS00401-FILE
           OPEN    OUTPUT  TENSU-FILE
      *
           PERFORM 900-SS00401-READ-SEQ-SEC
      *
           PERFORM UNTIL ( FLG-SS00401 NOT =   ZERO )
      *
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU   =   ZERO )
                   WRITE   TNS-TENSU-REC
               END-IF
      *
               PERFORM 900-SS00401-READ-SEQ-SEC
      *
           END-PERFORM
      *
           CLOSE   SS00401-FILE
                   TENSU-FILE
      *
           .
       2101-SS004-OT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    SS00401PARA     TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
           MOVE    SS00402PARA     TO  SS004-FILENAME
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-YMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    中間ファイル読込み処理
      *****************************************************************
       900-SS00401-READ-DYN-SEC        SECTION.
      *
           MOVE    WRK-SRYCD           TO  SS00401-KEY
           MOVE    ZERO                TO  FLG-SS00401
      *
           READ    SS00401-FILE
           INVALID
               MOVE    1               TO  FLG-SS00401
           END-READ
           .
       900-SS00401-READ-DYN-EXT.
           EXIT.
      *****************************************************************
      *    中間ファイル読込み処理
      *****************************************************************
       900-SS00401-READ-SEQ-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SS00401
      *
           READ    SS00401-FILE    NEXT
           AT  END
               MOVE    1               TO  FLG-SS00401
           END-READ
           .
       900-SS00401-READ-SEQ-EXT.
           EXIT.
      *****************************************************************
      *    入力コード　抽出
      *****************************************************************
       900-ICD-ALL-SEL-SEC             SECTION.
      *
           INITIALIZE                  ICD-REC
           MOVE    ZERO                TO  FLG-INPUTCD
      *
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    ICD-REC             TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO   MCP-TABLE
           MOVE    "all"               TO   MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  ICD-REC
           ELSE
               MOVE    1               TO  FLG-INPUTCD
           END-IF
           .
       900-ICD-ALL-SEL-EXT.
           EXIT.
      *****************************************************************
      *     入力コード　読み込み
      *****************************************************************
       900-ICD-ALL-FET-SEC             SECTION.
      *
           MOVE    "tbl_inputcd"       TO   MCP-TABLE
           MOVE    "all"               TO   MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  ICD-REC
           ELSE
               MOVE    1               TO  FLG-INPUTCD
           END-IF
           .
       900-ICD-ALL-FET-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスタ検索処理
      *****************************************************************
       900-SRYACT-KEY16-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACT
      *
           INITIALIZE                      SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    "%"                 TO  SRY-NYUGAIKBN
           MOVE    WRK-STSRYYM         TO  SRY-SRYYM
           MOVE    WRK-EDSRYYM         TO  SRY-CREYMD
           EVALUATE    SS004-SRYCDKBN
           WHEN    CONST-ALL
               MOVE    "002000000"     TO  SRY-SRYCD (1)
               MOVE    "699999999"     TO  SRY-SRYCD (2)
           WHEN    CONST-KENSA
               MOVE    "002000000"     TO  SRY-SRYCD (1)
               MOVE    "169999999"     TO  SRY-SRYCD (2)
           WHEN    CONST-YAKUZAI
               MOVE    "600000000"     TO  SRY-SRYCD (1)
               MOVE    "699999999"     TO  SRY-SRYCD (2)
           WHEN    OTHER
               MOVE    "XXXXXXXXX"     TO  SRY-SRYCD (1)
               MOVE    "XXXXXXXXX"     TO  SRY-SRYCD (2)
           END-EVALUATE
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACT-REC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
      *
       900-SRYACT-KEY16-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスタFETCH
      *****************************************************************
       900-SRYACT-KEY16-FET-SEC          SECTION.
      *
           MOVE    "tbl_sryact"        TO   MCP-TABLE
           MOVE    "key16"             TO   MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACT-REC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
      *
       900-SRYACT-KEY16-FET-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    SS00401-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-SYSYMD          TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
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
