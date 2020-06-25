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
       PROGRAM-ID.                 ORCXRMST3MIGV51.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : マスタ更新処理
      *  コンポーネント名  : 更新ファイル作成処理
      *                    : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/07/14    NACL-伊藤     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.01.00    NACL-伊藤    17/10/26  V510マスター分離対応特別版
      *                                     対象マスタ
      *                                       09:保険者
      *                                     対象テーブル
      *                                       tbl_mstkanri_org(臨時)
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    入力：パラメータファイル
           SELECT  IN-FILE         ASSIGN
                                   ASS-IN
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-IN.
      *    出力：更新ファイル（標準）
           SELECT  OUT1-FILE       ASSIGN
                                   ASS-OUT1
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-OUT1.
      *    出力：更新ファイル（移行前）
           SELECT  OUT2-FILE       ASSIGN
                                   ASS-OUT2
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-OUT2.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    入力ファイル
       FD  IN-FILE.
       01  IN-REC.
           03  IN-MASTER-KBN           PIC X(02).
           03  IN-MASTER-DELI          PIC X(01).
           03  IN-DBRVERVION           PIC X(21).
           03  IN-DBRNAME              PIC X(60).
           03  IN-UPFILE               PIC X(100).
           03  IN-UPPG                 PIC X(10).
      *
      *    更新ファイル
       FD  OUT1-FILE.
       01  OUT1-REC                    PIC X(200).
      *
       FD  OUT2-FILE.
       01  OUT2-REC                    PIC X(200).
      *
       WORKING-STORAGE             SECTION.
grpsys*
grpsys     COPY    "COMMON-SPA".
      *
      *    ファイル指定領域
       01  ASS-AREA.
           03  ASS-IN                  PIC X(256).
           03  ASS-OUT1                PIC X(256).
           03  ASS-OUT2                PIC X(256).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-IN                  PIC X(02).
           03  STS-OUT1                PIC X(02).
           03  STS-OUT2                PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-IN                  PIC 9(01).
           03  FLG-DBKANRI             PIC 9(01).
           03  FLG-MSTKANRI            PIC 9(01).
           03  FLG-OUTFILE             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-OUT1                PIC 9(06).
           03  CNT-OUT2                PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
      *
       01  OUT-REC.
           03  OUT-MASTER-KBN          PIC X(02).
           03  OUT-MASTER-DELI         PIC X(01).
           03  OUT-DBRVERVION          PIC X(21).
           03  OUT-UPPG                PIC X(10).
           03  OUT-UPFILE              PIC X(100).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-DBRVERSION          PIC X(64).
           03  WRK-DBRVERSION-ORG      PIC X(64).
      *
           03  WRK-DBKANRI-VERSION     PIC X(64).
      *
           03  WRK-IN                  PIC X(01). 
      *
           03  WRK-TBL-DBRVERSION   OCCURS 100   INDEXED DBR-IDX.
               05  WRK-TBL-MASTER-KBN  PIC X(02).
               05  WRK-TBL-DBRVERSION1 PIC X(64).
               05  WRK-TBL-UPDATE-FLG  PIC X(01).
      *
           03  WRK-ORG-DBRVERSION   OCCURS 100   INDEXED ORG-IDX.
               05  WRK-ORG-MASTER-KBN  PIC X(02).
               05  WRK-ORG-DBRVERSION1 PIC X(64).
               05  WRK-ORG-UPDATE-FLG  PIC X(01).
      *
           03  WRK-HOMEDIR             PIC X(128).
      *
       01  CONST-AREA.
           03  CONST-HOMEDIR           PIC X(28)  VALUE
               "/var/lib/jma-receipt/master/".
      *
           COPY    "ENUM-VALUE".
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
      *    ＤＢ管理情報
       01  DBKANRI-REC.
           COPY    "CPDBKANRI.INC".
      *
       01  MSTKANRI-REC.
           COPY    "CPMSTKANRI.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
       01  PARA-REC.
           03  PARA-HOSPNUM            PIC 9(02).
           03  PARA-SUPERVISOR         PIC X(01).
           03  PARA-DBFILE             PIC X(13).
      *
       PROCEDURE                       DIVISION
           USING
           PARA-REC.
      ****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-IN      =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
      *
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           MOVE    ZERO            TO  FLG-AREA
           MOVE    ZERO            TO  CNT-AREA
           MOVE    ZERO            TO  IDX-AREA
           INITIALIZE                  WRK-AREA
                                       SPA-AREA
      *
           MOVE    PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE    PARA-SUPERVISOR TO  SPA-SUPERVISOR
      *
           INITIALIZE                      SGETTEMP-AREA
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
      *
      *    DB OPEN
           PERFORM 100-DBOPEN-SEC
           IF      MCP-RC      NOT =   ZERO
               DISPLAY "DBOPEN ERR"
           END-IF.
      *
      *    ファイル情報
           PERFORM 101-ASSIN-GET-SEC
      *
      *    入力　初期処理
           OPEN    INPUT           IN-FILE
           IF      STS-IN          NOT =   ZERO
               MOVE    1           TO  FLG-IN
               DISPLAY "*(ORCXRMST3MIGV51)* IN-FILE OPN STS["
                                       STS-IN "]"
           END-IF
      *
      *    出力ファイル　初期処理
           OPEN    OUTPUT          OUT1-FILE
           IF      STS-OUT1        NOT =   ZERO
               DISPLAY "*(ORCXRMST3MIGV51)* OUT1-FILE OPN STS["
                                       STS-OUT1 "]"
           END-IF
           OPEN    OUTPUT          OUT2-FILE
           IF      STS-OUT2        NOT =   ZERO
               DISPLAY "*(ORCXRMST3MIGV51)* OUT2-FILE OPN STS["
                                       STS-OUT2 "]"
           END-IF
      *
           PERFORM 101-VERSION-GET-SEC
      *    MSTKANRI レコードバージョンのテーブル溜め込み
           PERFORM 101-DBR-TBL-SET-SEC
           PERFORM 101-ORG-TBL-SET-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル情報取得
      *****************************************************************
       101-ASSIN-GET-SEC                SECTION.
      *
           IF      SGETTEMP-MW         =   SPA-PANDA
               MOVE    CONST-HOMEDIR       TO  WRK-HOMEDIR
           ELSE
               MOVE    SGETTEMP-TEMPDIR    TO  WRK-HOMEDIR
           END-IF
      *
      *    入力ファイル
           MOVE    SPACE               TO  ASS-IN
           STRING  WRK-HOMEDIR         DELIMITED  BY  SPACE
                   PARA-HOSPNUM        DELIMITED  BY  SIZE
                   "/"                 DELIMITED  BY  SIZE
                   PARA-DBFILE         DELIMITED  BY  SPACE
                   "/ORCADBR-LI.DAT"   DELIMITED  BY  SIZE
                                       INTO    ASS-IN
           END-STRING
      *
      *    出力ーファイル
           MOVE    SPACE               TO  ASS-OUT1
           STRING  WRK-HOMEDIR         DELIMITED  BY  SPACE
                   PARA-HOSPNUM        DELIMITED  BY  SIZE
                   "/"                 DELIMITED  BY  SIZE
                   PARA-DBFILE         DELIMITED  BY  SPACE
                   "/ORCADBR.OUT1"     DELIMITED  BY  SIZE
                                       INTO    ASS-OUT1
           END-STRING
           MOVE    SPACE               TO  ASS-OUT2
           STRING  WRK-HOMEDIR         DELIMITED  BY  SPACE
                   PARA-HOSPNUM        DELIMITED  BY  SIZE
                   "/"                 DELIMITED  BY  SIZE
                   PARA-DBFILE         DELIMITED  BY  SPACE
                   "/ORCADBR.OUT2"     DELIMITED  BY  SIZE
                                       INTO    ASS-OUT2
           END-STRING
      *
           .
       101-ASSIN-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    DBKANRI バージョン取得処理
      *****************************************************************
       101-VERSION-GET-SEC             SECTION.
      *
      *    ＤＢ管理マスタ検索
           INITIALIZE                  DBKANRI-REC
           MOVE    "ORCADB00"          TO  DBKANRI-KANRICD
           MOVE    DBKANRI-REC         TO  MCPDATA-REC
           MOVE    "tbl_dbkanri"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_dbkanri"   TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBKANRI-READ-SEC
           ELSE
               INITIALIZE                      DBKANRI-REC
               MOVE    1                   TO  FLG-DBKANRI
           END-IF
      *
           MOVE    SPACE               TO  WRK-DBKANRI-VERSION
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDZ
           IF      DBKANRI-KANRICD(1:8) =  "ORCADB00"
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL       IDZ     >   64
                   IF   (DBKANRI-VERSION(IDZ:1)   NOT =   SPACE)
                         ADD   1       TO  IDY
                         MOVE  DBKANRI-VERSION(IDZ:1)
                                       TO  WRK-DBKANRI-VERSION(IDY:1)
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_dbkanri"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-CLOSE-SEC
      *
           .
       101-VERSION-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    MSTKANRI レコードバージョンのテーブル溜め込み処理
      *****************************************************************
       101-DBR-TBL-SET-SEC              SECTION.
      *
      *    マスタ管理マスタ検索
           INITIALIZE                  MSTKANRI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  MSTKANRI-HOSPNUM
           MOVE    MSTKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_mstkanri"      TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_mstkanri"  TO  MCP-TABLE
               MOVE    "all"           TO  MCP-PATHNAME
               PERFORM 910-MSTKANRI-READ-SEC
           ELSE
               INITIALIZE                      MSTKANRI-REC
               MOVE    1                   TO  FLG-MSTKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL   FLG-MSTKANRI    =   1
               IF      MSTKANRI-KANRICD(7:2)    NUMERIC
                   ADD   1             TO  IDX
                   MOVE MSTKANRI-KANRICD(7:2)
                                       TO  WRK-TBL-MASTER-KBN (IDX)
                   MOVE MSTKANRI-DBRVERSION1
                                       TO  WRK-TBL-DBRVERSION1(IDX)
                   MOVE SPACE          TO  WRK-TBL-UPDATE-FLG (IDX)
               END-IF
               MOVE    "tbl_mstkanri"  TO  MCP-TABLE
               MOVE    "all"           TO  MCP-PATHNAME
               PERFORM 910-MSTKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_mstkanri"      TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-CLOSE-SEC
      *
           .
       101-DBR-TBL-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    MSTKANRI レコードバージョンのテーブル溜め込み処理
      *****************************************************************
       101-ORG-TBL-SET-SEC             SECTION.
           INITIALIZE                  MSTKANRI-REC
           MOVE    SPA-HOSPNUM         TO  MSTKANRI-HOSPNUM
           MOVE    MSTKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_mstkanri_org"  TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_mstkanri_org"  TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 910-MSTKANRI-READ-SEC
           ELSE
               INITIALIZE                      MSTKANRI-REC
               MOVE    1                   TO  FLG-MSTKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL   FLG-MSTKANRI    =   1
               IF      MSTKANRI-KANRICD(7:2)    NUMERIC
                   ADD   1             TO  IDX
                   MOVE MSTKANRI-KANRICD(7:2)
                                       TO  WRK-ORG-MASTER-KBN (IDX)
                   MOVE MSTKANRI-DBRVERSION1
                                       TO  WRK-ORG-DBRVERSION1(IDX)
                   MOVE SPACE          TO  WRK-ORG-UPDATE-FLG (IDX)
               END-IF
               MOVE    "tbl_mstkanri_org"  TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 910-MSTKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_mstkanri_org"  TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-CLOSE-SEC
           .
       101-ORG-TBL-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *    レコード管理ファイル読込み
           PERFORM 900-IN-READ-SEC
           PERFORM UNTIL   FLG-IN      =   1
               MOVE     SPACE              TO  WRK-DBRVERSION
      *        該当マスタのレコードバージョン取得
               SET     DBR-IDX             TO  1
               SEARCH  WRK-TBL-DBRVERSION  VARYING DBR-IDX
                   AT END
                       MOVE    SPACE       TO  WRK-DBRVERSION
                   WHEN    IN-MASTER-KBN   =   WRK-TBL-MASTER-KBN
                                                              (DBR-IDX)
                       MOVE    WRK-TBL-DBRVERSION1(DBR-IDX)
                                           TO  WRK-DBRVERSION
               END-SEARCH
      *****    IF      IN-MASTER-KBN   =   "09"
      *****        IF  WRK-DBRVERSION  <   "R-020900-1-20060531-1"
      *****            MOVE    SPACE   TO  WRK-DBRVERSION
      *****        END-IF
      *****    END-IF
               IF      WRK-DBRVERSION  NOT =   SPACE
                   SET     ORG-IDX             TO  1
                   SEARCH  WRK-ORG-DBRVERSION  VARYING ORG-IDX
                       AT END
                           MOVE    SPACE       TO  WRK-DBRVERSION-ORG
                       WHEN    IN-MASTER-KBN   =   WRK-ORG-MASTER-KBN
                                                              (ORG-IDX)
                           MOVE    WRK-ORG-DBRVERSION1(ORG-IDX)
                                               TO  WRK-DBRVERSION-ORG
                   END-SEARCH
                   EVALUATE   TRUE
                       WHEN   WRK-DBRVERSION-ORG   >   WRK-DBRVERSION
                           IF    IN-DBRVERVION     >   WRK-DBRVERSION
                             IF    IN-DBRVERVION(3:IDY)  >
                                           WRK-DBKANRI-VERSION(1:IDY)
                                 CONTINUE
                             ELSE
                                 MOVE    1           TO  FLG-OUTFILE
                                 PERFORM 210-OUTFILE-SEC
                                 MOVE    IN-DBRVERVION       TO
                                           WRK-TBL-DBRVERSION1(DBR-IDX)
                                 MOVE    "1"                 TO
                                           WRK-TBL-UPDATE-FLG(DBR-IDX)
                             END-IF
                           END-IF
                       WHEN   WRK-DBRVERSION-ORG   <   WRK-DBRVERSION
                           IF    IN-DBRVERVION     >
                                                   WRK-DBRVERSION-ORG
                             IF    IN-DBRVERVION(3:IDY)  >
                                           WRK-DBKANRI-VERSION(1:IDY)
                                 CONTINUE
                             ELSE
                                 MOVE    2           TO  FLG-OUTFILE
                                 PERFORM 210-OUTFILE-SEC
                                 MOVE    IN-DBRVERVION       TO
                                           WRK-ORG-DBRVERSION1(ORG-IDX)
                                 MOVE    "1"                 TO
                                           WRK-ORG-UPDATE-FLG(ORG-IDX)
                             END-IF
                           END-IF
                       WHEN   WRK-DBRVERSION-ORG   =   WRK-DBRVERSION
                           CONTINUE
                   END-EVALUATE
               END-IF
      *        レコード管理ファイル読込み
               PERFORM 900-IN-READ-SEC
           END-PERFORM
      *
      *    マスタ管理マスタ更新
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         =   100 OR
                           WRK-TBL-MASTER-KBN(IDX) =   SPACE
               IF      WRK-TBL-UPDATE-FLG(IDX) =   "1"
                   INITIALIZE              MSTKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  MSTKANRI-HOSPNUM
                   MOVE    "ORCADB"        TO  MSTKANRI-KANRICD
                   MOVE    WRK-TBL-MASTER-KBN(IDX)
                                           TO  MSTKANRI-KANRICD(7:2)
                   MOVE    MSTKANRI-REC    TO  MCPDATA-REC
                   MOVE    "tbl_mstkanri"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC          =   ZERO
                       PERFORM 910-MSTKANRI-READ-SEC
                   ELSE
                       INITIALIZE              MSTKANRI-REC
                       MOVE    1               TO  FLG-MSTKANRI
                   END-IF
      *
                   IF      FLG-MSTKANRI        =   ZERO
                       MOVE    WRK-TBL-DBRVERSION1(IDX)    TO
                                                   MSTKANRI-DBRVERSION2
                       MOVE    MSTKANRI-REC        TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       IF      MSTKANRI-KANRICD (7:2)  =   "01" OR "02"
                                                        OR "15" OR "16"
                           MOVE    "tbl_mstkanri_public"
                                                       TO  MCP-TABLE
                       ELSE
                           MOVE    "tbl_mstkanri_master"
                                                       TO  MCP-TABLE
                       END-IF
                       MOVE    "key"               TO  MCP-PATHNAME
                       PERFORM 900-ORCDBMAIN-SEC
                       IF      MCP-RC          NOT =   ZERO
                           DISPLAY "ORCXRMST3MIGV51 MSTKANRI UPD ERR:"
                                   MCP-RC
                       END-IF
                   END-IF
                   MOVE    "tbl_mstkanri"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-CLOSE-SEC
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         =   100 OR
                           WRK-ORG-MASTER-KBN(IDX) =   SPACE
               IF      WRK-ORG-UPDATE-FLG(IDX) =   "1"
                   INITIALIZE              MSTKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  MSTKANRI-HOSPNUM
                   MOVE    "ORCADB"        TO  MSTKANRI-KANRICD
                   MOVE    WRK-ORG-MASTER-KBN(IDX)
                                           TO  MSTKANRI-KANRICD(7:2)
                   MOVE    MSTKANRI-REC    TO  MCPDATA-REC
                   MOVE    "tbl_mstkanri_org"
                                           TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC          =   ZERO
                       PERFORM 910-MSTKANRI-READ-SEC
                   ELSE
                       INITIALIZE              MSTKANRI-REC
                       MOVE    1               TO  FLG-MSTKANRI
                   END-IF
      *
                   IF      FLG-MSTKANRI        =   ZERO
                       MOVE    WRK-ORG-DBRVERSION1(IDX)    TO
                                                   MSTKANRI-DBRVERSION2
                       MOVE    MSTKANRI-REC        TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "tbl_mstkanri_org"  TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
                       PERFORM 900-ORCDBMAIN-SEC
                       IF      MCP-RC          NOT =   ZERO
                           DISPLAY "ORCXRMST1MIGV51 MSTKANRI UPD ERR:"
                                   MCP-RC
                       END-IF
                   END-IF
                   MOVE    "tbl_mstkanri_org"  TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-CLOSE-SEC
               END-IF
           END-PERFORM
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新ファイル出力  処理
      *****************************************************************
       210-OUTFILE-SEC               SECTION.
      *
           INITIALIZE                  OUT-REC
           MOVE    IN-MASTER-KBN       TO  OUT-MASTER-KBN
           MOVE    ":"                 TO  OUT-MASTER-DELI
           MOVE    IN-DBRVERVION       TO  OUT-DBRVERVION
      *
           MOVE    IN-UPFILE           TO  OUT-UPFILE
           MOVE    IN-UPPG             TO  OUT-UPPG
           IF      FLG-OUTFILE         =   1
               MOVE    OUT-REC             TO  OUT1-REC
               WRITE   OUT1-REC
               ADD     1                   TO  CNT-OUT1
           ELSE
               MOVE    OUT-REC             TO  OUT2-REC
               WRITE   OUT2-REC
               ADD     1                   TO  CNT-OUT2
           END-IF
      *
           .
       210-OUTFILE-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *    DB DISCONNECT
           PERFORM 900-DBDISCONNECT-SEC
      *
      *    出力ファイル　終了処理
           CLOSE                       IN-FILE
           CLOSE                       OUT1-FILE
           CLOSE                       OUT2-FILE
      *
           DISPLAY "*(ORCXRMST3MIGV51)* INPUT  /I   CNT[" CNT-IN   "]"
           DISPLAY "*(ORCXRMST3MIGV51)* OUT1   /O   CNT[" CNT-OUT1 "]"
           DISPLAY "*(ORCXRMST3MIGV51)* OUT2   /O   CNT[" CNT-OUT2 "]"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力ファイル読込み処理
      *****************************************************************
       900-IN-READ-SEC          SECTION.
      *
       900-IN-READ-10.
           READ    IN-FILE
               AT  END
                   MOVE    1                   TO  FLG-IN
               NOT AT  END
                   MOVE    ZERO                TO  FLG-IN
                   ADD     1                   TO  CNT-IN
                   IF      IN-MASTER-KBN       =   "09"
                       CONTINUE
                   ELSE
                       GO  TO  900-IN-READ-10
                   END-IF
           END-READ
      *
           .
       900-IN-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    マスタ管理ファイル読込
      *****************************************************************
       910-MSTKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  MSTKANRI-REC
               MOVE    ZERO            TO  FLG-MSTKANRI
           ELSE
               INITIALIZE                  MSTKANRI-REC
               MOVE    1               TO  FLG-MSTKANRI
           END-IF
      *
           .
       910-MSTKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ管理ファイル読込
      *****************************************************************
       910-DBKANRI-READ-SEC            SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  DBKANRI-REC
               MOVE    ZERO            TO  FLG-DBKANRI
           ELSE
               INITIALIZE                  DBKANRI-REC
               MOVE    1               TO  FLG-DBKANRI
           END-IF
      *
           .
       910-DBKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    ZERO                TO  MCP-RC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソルクローズ処理
      *****************************************************************
       910-CLOSE-SEC                   SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
      *
       910-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                  SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBCLOSE-EXT.
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
      *      
