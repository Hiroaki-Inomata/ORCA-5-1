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
       PROGRAM-ID.             ORCBHOSPCHG.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 随時
      *  コンポーネント名  : 会計クリア（月途中医療機関コード変更対応）
      *  管理者            : 
      *  作成日付    作業者        記述
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-小豆沢  07/06/15  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    エラーファイル
           SELECT  RECEERR-FILE        ASSIGN  RECEERR
                                       FILE    STATUS  IS  STS-RECEERR.
      *
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEERR.INC".
      *
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-SANTEI              PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-SYUNOU              PIC 9(01).
           03  FLG-PTBYOMEI            PIC 9(01).
           03  FLG-RIHA                PIC 9(01).
           03  FLG-SYORI               PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SRYACCT-UPD         PIC 9(05).
           03  CNT-NYUINACCT-UPD       PIC 9(05).
           03  CNT-SANTEI-UPD          PIC 9(05).
           03  CNT-JYURRK-DEL          PIC 9(05).
           03  CNT-SYUNOU-DEL          PIC 9(05).
           03  CNT-PTBYOMEI-UPD        PIC 9(05).
           03  CNT-PTBYOMEI-DEL        PIC 9(05).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                    PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
           03  WRK-PARA-SRYYM          PIC X(06).
           03  WRK-PARA-STIDX          PIC X(02).
           03  WRK-PARA-EDIDX          PIC X(02).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-RECEERR             PIC X(200).
           03  WRK-STIDX               PIC 9(05).
           03  WRK-EDIDX               PIC 9(05).
           03  WRK-STYMD               PIC X(08).
           03  WRK-EDYMD               PIC X(08).
      *
      *    COPY    "CPSHELLTBL.INC".
      *
      *    COPY    "ORCA-DBPATH".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為マスター
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    入院診療会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    患者病名マスター
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(256).
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
                                       USING
                                       COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
      *
           INITIALIZE                  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                       INTO            LNK-PRTKANRI-RENNUM
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
                                       WRK-PARA-SRYYM
                                       WRK-PARA-STIDX
                                       WRK-PARA-EDIDX
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO   SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBHOSPCHG"   TO  JOB-PGID
           MOVE    "会計クリア処理"
                                   TO  JOB-SHELLMSG
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           MOVE    "STP"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    3               TO  JOB-STOPFLG
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  RECEERR
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
           IF    ( FLG-END     NOT =   ZERO )
               GO  TO  100-INIT-EXT
           END-IF
      *
           MOVE    WRK-PARA-STIDX    TO  WRK-STIDX
           MOVE    WRK-PARA-EDIDX    TO  WRK-EDIDX
      *
      *    開始年月日、終了年月日のセット
           MOVE    WRK-PARA-SRYYM    TO  WRK-STYMD (1:6)
           MOVE    WRK-PARA-STIDX    TO  WRK-STYMD (7:2)
           MOVE    WRK-PARA-SRYYM    TO  WRK-EDYMD (1:6)
           MOVE    WRK-PARA-EDIDX    TO  WRK-EDYMD (7:2)
           DISPLAY  "WRK-STYMD = " WRK-STYMD
           DISPLAY  "WRK-EDYMD = " WRK-EDYMD
      *
      *    前半削除、後半削除のフラグセット
           IF     WRK-PARA-STIDX     =    "01"
               MOVE    1             TO  FLG-SYORI
           ELSE
               MOVE    2             TO  FLG-SYORI
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
           MOVE    SPACE           TO  WRK-RECEERR
           IF ( WRK-PARA-STIDX     >=   "01")   AND
              ( WRK-PARA-STIDX     <=   "31")
               CONTINUE
           ELSE
               MOVE    "開始日が誤りです"   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
      *
           IF   WRK-PARA-STIDX  IS  NUMERIC
               CONTINUE
           ELSE
               MOVE    "開始日が誤りです"   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
      *
           IF ( WRK-PARA-EDIDX     >=   "01")   AND
              ( WRK-PARA-EDIDX     <=   "31")
               CONTINUE
           ELSE
               MOVE    "終了日が誤りです"   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
      *
           IF   WRK-PARA-EDIDX  IS  NUMERIC
               CONTINUE
           ELSE
               MOVE    "終了日が誤りです"   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
      *
           IF ( WRK-PARA-STIDX     <=    WRK-PARA-EDIDX)
               CONTINUE
           ELSE
               MOVE    "開始日が終了日より後日付です"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    診療会計更新処理
           PERFORM 201-SRYACCT-UPD-SEC
           IF    ( FLG-END     NOT =   ZERO )
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    入院会計更新処理
           PERFORM 201-NYUINACCT-UPD-SEC
           IF    ( FLG-END     NOT =   ZERO )
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    算定履歴更新処理
           PERFORM 201-SANTEI-UPD-SEC
           IF    ( FLG-END     NOT =   ZERO )
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    受診履歴削除処理
           PERFORM 201-JYURRK-DEL-SEC
           IF    ( FLG-END     NOT =   ZERO )
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    収納削除処理
           PERFORM 201-SYUNOU-DEL-SEC
           IF    ( FLG-END     NOT =   ZERO )
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    患者病名更新処理
           PERFORM 201-PTBYOMEI-UPD-SEC
           IF    ( FLG-END     NOT =   ZERO )
               GO  TO  200-MAIN-EXT
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計更新処理
      *****************************************************************
       201-SRYACCT-UPD-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYACCT
      *
           INITIALIZE                  SRYACCT-REC
           MOVE    WRK-PARA-SRYYM  TO  ACCT-SRYYM
           MOVE    SPA-HOSPNUM     TO  ACCT-HOSPNUM
           MOVE    SRYACCT-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "all2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SRYACCT-REC
           ELSE
               MOVE    1           TO  FLG-SRYACCT
           END-IF
      *
           PERFORM UNTIL ( FLG-SRYACCT NOT =   ZERO )
      *
               PERFORM 201-SRYACCT-RIHA-SYYK-SEC
      *
               IF      FLG-RIHA    =   ZERO
                 PERFORM VARYING IDX1    FROM    WRK-STIDX   BY  1
                         UNTIL ( IDX1    >       WRK-EDIDX )
      *
                   COMPUTE ACCT-ZAIKAISU   =   ACCT-ZAIKAISU
                                           -   ACCT-DAY (1 IDX1)
      *
                   MOVE    ZERO        TO  ACCT-DAY (1 IDX1 )
                                           ACCT-DAY (2 IDX1 )
                                           ACCT-DAY (3 IDX1 )
                                           ACCT-DAY (4 IDX1 )
                                           ACCT-DAY (5 IDX1 )
                                           ACCT-DAY (6 IDX1 )
                                           ACCT-DAY (7 IDX1 )
                                           ACCT-DAY (8 IDX1 )
                                           ACCT-DAY (9 IDX1 )
                                           ACCT-DAY (10 IDX1 )
      *
                 END-PERFORM
      *
                 MOVE    SRYACCT-REC     TO  MCPDATA-REC
                 MOVE    "DBUPDATE"      TO  MCP-FUNC
                 MOVE    "tbl_sryacct"   TO  MCP-TABLE
                 MOVE    "key"           TO  MCP-PATHNAME
                 PERFORM   900-ORCDBMAIN-SEC
                 IF    ( MCP-RC      NOT =   ZERO )
                     DISPLAY "ORCBMRECE SRYACCT UPD ERR:"  MCP-RC
                             ",PTID:"   ACCT-PTID
                     MOVE    "診療会計の更新に失敗しました"
                                       TO  WRK-RECEERR
                     PERFORM 500-ERR-HENSYU-SEC
                     GO  TO  201-SRYACCT-UPD-EXT
                 END-IF
      *
                 COMPUTE CNT-SRYACCT-UPD =   CNT-SRYACCT-UPD +   1
      *
               END-IF
      *
               MOVE    "tbl_sryacct"   TO  MCP-TABLE
               MOVE    "all2"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  SRYACCT-REC
               ELSE
                   MOVE    1           TO  FLG-SRYACCT
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "all2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       201-SRYACCT-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリテーション開始日、終了日のシステム予約コード判定
      *****************************************************************
       201-SRYACCT-RIHA-SYYK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-RIHA
      *
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-SRYACT-SELECT-SEC
           IF      FLG-SRYACT     =    ZERO
      *            心大血管疾患リハビリテーション開始日
                   IF    SRY-SRYCD(1)  =  "099800111"
                       MOVE   1        TO  FLG-RIHA
                   END-IF
      *            心大血管疾患リハビリテーション終了日
                   IF    SRY-SRYCD(1)  =  "099800112"
                       MOVE   1        TO  FLG-RIHA
                   END-IF
      *            脳血管疾患等リハビリテーション開始日
                   IF    SRY-SRYCD(1)  =  "099800121"
                       MOVE   1        TO  FLG-RIHA
                   END-IF
      *            脳血管疾患等リハビリテーション終了日
                   IF    SRY-SRYCD(1)  =  "099800122"
                       MOVE   1        TO  FLG-RIHA
                   END-IF
      *            運動器リハビリテーション開始日
                   IF    SRY-SRYCD(1)  =  "099800131"
                       MOVE   1        TO  FLG-RIHA
                   END-IF
      *            運動器リハビリテーション終了日
                   IF    SRY-SRYCD(1)  =  "099800132"
                       MOVE   1        TO  FLG-RIHA
                   END-IF
      *            呼吸器リハビリテーション開始日
                   IF    SRY-SRYCD(1)  =  "099800141"
                       MOVE   1        TO  FLG-RIHA
                   END-IF
      *            呼吸器リハビリテーション終了日
                   IF    SRY-SRYCD(1)  =  "099800142"
                       MOVE   1        TO  FLG-RIHA
                   END-IF
      *            摂食機能療法開始日
                   IF    SRY-SRYCD(1)  =  "099800151"
                       MOVE   1        TO  FLG-RIHA
                   END-IF
      *            摂食機能療法終了日
                   IF    SRY-SRYCD(1)  =  "099800152"
                       MOVE   1        TO  FLG-RIHA
                   END-IF
      *            難病患者リハビリテーション開始日
                   IF    SRY-SRYCD(1)  =  "099800161"
                       MOVE   1        TO  FLG-RIHA
                   END-IF
      *            難病患者リハビリテーション終了日
                   IF    SRY-SRYCD(1)  =  "099800162"
                       MOVE   1        TO  FLG-RIHA
                   END-IF
      *            障害児（者）リハビリテーション開始日
                   IF    SRY-SRYCD(1)  =  "099800171"
                       MOVE   1        TO  FLG-RIHA
                   END-IF
      *            障害児（者）リハビリテーション終了日
                   IF    SRY-SRYCD(1)  =  "099800172"
                       MOVE   1        TO  FLG-RIHA
                   END-IF
      *            がん患者リハビリテーション開始日
                   IF    SRY-SRYCD(1)  =  "099800181"
                       MOVE   1        TO  FLG-RIHA
                   END-IF
      *            がん患者リハビリテーション終了日
                   IF    SRY-SRYCD(1)  =  "099800182"
                       MOVE   1        TO  FLG-RIHA
                   END-IF
           END-IF
      *    診療行為クローズ
           MOVE     "tbl_sryact"   TO  MCP-TABLE
           MOVE     "key2"          TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       201-SRYACCT-RIHA-SYYK-EXT.
           EXIT.
      *****************************************************************
      *    入院会計更新処理
      *****************************************************************
       201-NYUINACCT-UPD-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-NYUINACCT
      *
           INITIALIZE                  NYUINACCT-REC
           MOVE    WRK-PARA-SRYYM  TO  NACCT-SRYYM
           MOVE    SPA-HOSPNUM     TO  NACCT-HOSPNUM
           MOVE    NYUINACCT-REC   TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
           MOVE    "all2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  NYUINACCT-REC
           ELSE
               MOVE    1           TO  FLG-NYUINACCT
           END-IF
      *
           PERFORM UNTIL ( FLG-NYUINACCT NOT =   ZERO )
      *
               PERFORM VARYING IDX1    FROM    WRK-STIDX   BY  1
                       UNTIL ( IDX1    >       WRK-EDIDX )
      *
                   COMPUTE NACCT-ZAIKAISU  =   NACCT-ZAIKAISU
                                           -   NACCT-DAY (IDX1)
      *
                   MOVE    ZERO        TO  NACCT-DAY (IDX1)
      *
               END-PERFORM
      *
               MOVE    NYUINACCT-REC   TO  MCPDATA-REC
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "tbl_nyuinacct" TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM   900-ORCDBMAIN-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   DISPLAY "ORCBMRECE NYUINACCT UPD ERR:"  MCP-RC
                           ",PTID:"   NACCT-PTID
                   MOVE    "入院会計の更新に失敗しました"
                                       TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  201-NYUINACCT-UPD-EXT
               END-IF
      *
               COMPUTE CNT-NYUINACCT-UPD   =   CNT-NYUINACCT-UPD   +   1
      *
               MOVE    "tbl_nyuinacct" TO  MCP-TABLE
               MOVE    "all2"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  NYUINACCT-REC
               ELSE
                   MOVE    1           TO  FLG-NYUINACCT
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "all2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       201-NYUINACCT-UPD-EXT.
           EXIT.
      *****************************************************************
      *    算定更新処理
      *****************************************************************
       201-SANTEI-UPD-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-SANTEI
      *
           INITIALIZE                  SANTEI-REC
           MOVE    WRK-PARA-SRYYM  TO  SANTEI-SRYYM
           MOVE    SPA-HOSPNUM     TO  SANTEI-HOSPNUM
           MOVE    SANTEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_santei"    TO  MCP-TABLE
           MOVE    "all2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SANTEI-REC
           ELSE
               MOVE    1           TO  FLG-SANTEI
           END-IF
      *
           PERFORM UNTIL ( FLG-SANTEI NOT =   ZERO )
      *
      *        リハビリテーション開始日、終了日のシステム予約コードは
      *        クリア対象外とする
               IF     SANTEI-SRYCD     =    "099800111"  OR
                                            "099800112"  OR
                                            "099800121"  OR
                                            "099800122"  OR
                                            "099800131"  OR
                                            "099800132"  OR
                                            "099800141"  OR
                                            "099800142"  OR
                                            "099800151"  OR
                                            "099800152"  OR
                                            "099800161"  OR
                                            "099800162"  OR
                                            "099800171"  OR
                                            "099800172"  OR
                                            "099800181"  OR
                                            "099800182"
                   CONTINUE
               ELSE
                 PERFORM VARYING IDX1    FROM    WRK-STIDX   BY  1
                         UNTIL ( IDX1    >       WRK-EDIDX )
      *
                     MOVE    ZERO        TO  SANTEI-DAY (IDX1)
      *
                 END-PERFORM
      *
                 MOVE    SANTEI-REC      TO  MCPDATA-REC
                 MOVE    "DBUPDATE"      TO  MCP-FUNC
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key"           TO  MCP-PATHNAME
                 PERFORM   900-ORCDBMAIN-SEC
                 IF    ( MCP-RC      NOT =   ZERO )
                     DISPLAY "ORCBMRECE SANTEI UPD ERR:"  MCP-RC
                             ",PTID:"   SANTEI-PTID
                     MOVE    "算定履歴の更新に失敗しました"
                                         TO  WRK-RECEERR
                     PERFORM 500-ERR-HENSYU-SEC
                     GO  TO  201-SANTEI-UPD-EXT
                 END-IF
      *
                 COMPUTE CNT-SANTEI-UPD  =   CNT-SANTEI-UPD   +   1
      *
               END-IF
      *
               MOVE    "tbl_santei"    TO  MCP-TABLE
               MOVE    "all2"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  SANTEI-REC
               ELSE
                   MOVE    1           TO  FLG-SANTEI
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "all2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       201-SANTEI-UPD-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴削除処理
      *****************************************************************
       201-JYURRK-DEL-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-JYURRK
      *
           INITIALIZE                  JYURRK-REC
           MOVE    WRK-PARA-SRYYM  TO  JYURRK-SRYYMD (1:6)
           MOVE    "%"             TO  JYURRK-SRYYMD (7:1)
           MOVE    SPA-HOSPNUM     TO  JYURRK-HOSPNUM
           MOVE    JYURRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"    TO  MCP-TABLE
           MOVE    "all2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  JYURRK-REC
           ELSE
               MOVE    1           TO  FLG-JYURRK
           END-IF
      *
           PERFORM UNTIL ( FLG-JYURRK NOT =   ZERO )
      *
      *        受診履歴の削除
               IF     ( JYURRK-SRYYMD   >=   WRK-STYMD  )  AND
                      ( JYURRK-SRYYMD   <=   WRK-EDYMD  )
      *
                 MOVE    JYURRK-REC      TO  MCPDATA-REC
                 MOVE    "DBDELETE"      TO  MCP-FUNC
                 MOVE    "tbl_jyurrk"    TO  MCP-TABLE
                 MOVE    "key"           TO  MCP-PATHNAME
                 PERFORM   900-ORCDBMAIN-SEC
                 IF    ( MCP-RC      NOT =   ZERO )
                     DISPLAY "ORCBMRECE JYURRK DEL ERR:"  MCP-RC
                             ",PTID:"   JYURRK-PTID
                     MOVE    "受診履歴の削除に失敗しました"
                                         TO  WRK-RECEERR
                     PERFORM 500-ERR-HENSYU-SEC
                     GO  TO  201-JYURRK-DEL-EXT
                 END-IF
      *
                 COMPUTE CNT-JYURRK-DEL  =   CNT-JYURRK-DEL   +   1
      *
               END-IF
      *
               MOVE    "tbl_jyurrk"    TO  MCP-TABLE
               MOVE    "all2"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  JYURRK-REC
               ELSE
                   MOVE    1           TO  FLG-JYURRK
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "all2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       201-JYURRK-DEL-EXT.
           EXIT.
      *****************************************************************
      *    収納削除処理
      *****************************************************************
       201-SYUNOU-DEL-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    WRK-PARA-SRYYM  TO  SYU-SRYYMD (1:6)
           MOVE    "%"             TO  SYU-SRYYMD (7:1)
           MOVE    SPA-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    "2"             TO  SYU-NYUGAIKBN
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key15"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           PERFORM UNTIL ( FLG-SYUNOU NOT =   ZERO )
      *
      *        収納の削除
               IF     ( SYU-SRYYMD   >=   WRK-STYMD  )  AND
                      ( SYU-SRYYMD   <=   WRK-EDYMD  )
      *
                 MOVE    SYUNOU-REC      TO  MCPDATA-REC
                 MOVE    "DBDELETE"      TO  MCP-FUNC
                 MOVE    "tbl_syunou"    TO  MCP-TABLE
                 MOVE    "key"           TO  MCP-PATHNAME
                 PERFORM   900-ORCDBMAIN-SEC
                 IF    ( MCP-RC      NOT =   ZERO )
                     DISPLAY "ORCBMRECE SYUNOU DEL ERR:"  MCP-RC
                             ",PTID:"   SYU-PTID
                     MOVE    "収納の削除に失敗しました"
                                         TO  WRK-RECEERR
                     PERFORM 500-ERR-HENSYU-SEC
                     GO  TO  201-SYUNOU-DEL-EXT
                 END-IF
      *
                 COMPUTE CNT-SYUNOU-DEL  =   CNT-SYUNOU-DEL   +   1
      *
               END-IF
      *
               MOVE    "tbl_syunou"    TO  MCP-TABLE
               MOVE    "key15"         TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  SYUNOU-REC
               ELSE
                   MOVE    1           TO  FLG-SYUNOU
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       201-SYUNOU-DEL-EXT.
           EXIT.
      *****************************************************************
      *    患者病名更新処理
      *****************************************************************
       201-PTBYOMEI-UPD-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-PTBYOMEI
      *
           INITIALIZE                  PTBYOMEI-REC
           MOVE    SPA-HOSPNUM     TO  PTBYO-HOSPNUM
           MOVE    PTBYOMEI-REC    TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
           MOVE    "all"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTBYOMEI-REC
           ELSE
               MOVE    1           TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM UNTIL ( FLG-PTBYOMEI NOT =   ZERO )
      *
      *        患者病名の前半クリア処理
               IF       FLG-SYORI    =    1
      *          前半期間の終了日より以前の転帰済データを削除する
                 IF     ( PTBYO-TENKIKBN   NOT =   SPACE )      AND
                        ( PTBYO-TENKIYMD   <=      WRK-EDYMD )  AND
                        ( PTBYO-DLTFLG     =       SPACE )
      *
                   MOVE    PTBYOMEI-REC    TO  MCPDATA-REC
                   MOVE    "DBDELETE"      TO  MCP-FUNC
                   MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM   900-ORCDBMAIN-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       DISPLAY "ORCBMRECE PTBYOMEI DEL ERR:"  MCP-RC
                               ",PTID:"   PTBYO-PTID
                       MOVE    "患者病名の削除に失敗しました"
                                           TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  201-PTBYOMEI-UPD-EXT
                   END-IF
      *
                   COMPUTE CNT-PTBYOMEI-DEL = CNT-PTBYOMEI-DEL   +   1
      *
                 END-IF
               END-IF
      *
      *        患者病名の後半クリア処理
               IF       FLG-SYORI    =    2
      *          後半期間に入力された転帰をクリアする
                 IF     ( PTBYO-SRYYMD     <       WRK-STYMD ) AND
                        ( PTBYO-TENKIKBN   NOT =   SPACE )     AND
                        ( PTBYO-TENKIYMD   >=      WRK-STYMD ) AND
                        ( PTBYO-DLTFLG     =       SPACE )
      *
                   MOVE    SPACE           TO  PTBYO-TENKIKBN
                                               PTBYO-TENKIYMD
                   MOVE    PTBYOMEI-REC    TO  MCPDATA-REC
                   MOVE    "DBUPDATE"      TO  MCP-FUNC
                   MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM   900-ORCDBMAIN-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       DISPLAY "ORCBMRECE PTBYOMEI UPD ERR:"  MCP-RC
                               ",PTID:"   PTBYO-PTID
                       MOVE    "患者病名の更新に失敗しました"
                                           TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  201-PTBYOMEI-UPD-EXT
                   END-IF
      *
                   COMPUTE CNT-PTBYOMEI-UPD = CNT-PTBYOMEI-UPD   +   1
      *
                 END-IF
      *
      *          後半期間に入力された患者病名を削除する
                 IF     ( PTBYO-SRYYMD     >=      WRK-STYMD ) AND
                        ( PTBYO-DLTFLG     =       SPACE )
      *
                   MOVE    PTBYOMEI-REC    TO  MCPDATA-REC
                   MOVE    "DBDELETE"      TO  MCP-FUNC
                   MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM   900-ORCDBMAIN-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       DISPLAY "ORCBMRECE PTBYOMEI DEL ERR:"  MCP-RC
                               ",PTID:"   PTBYO-PTID
                       MOVE    "患者病名の削除に失敗しました"
                                           TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  201-PTBYOMEI-UPD-EXT
                   END-IF
      *
                   COMPUTE CNT-PTBYOMEI-DEL = CNT-PTBYOMEI-DEL   +   1
      *
                 END-IF
               END-IF
      *
               MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
               MOVE    "all"           TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  PTBYOMEI-REC
               ELSE
                   MOVE    1           TO  FLG-PTBYOMEI
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       201-PTBYOMEI-UPD-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "*** ORCBMRECE SRYACCT   UPD:"     CNT-SRYACCT-UPD
           DISPLAY "*** ORCBMRECE NYUINACCT UPD:"     CNT-NYUINACCT-UPD
           DISPLAY "*** ORCBMRECE SANTEI    UPD:"     CNT-SANTEI-UPD
           DISPLAY "*** ORCBMRECE JYURRK    DEL:"     CNT-JYURRK-DEL
           DISPLAY "*** ORCBMRECE SYUNOU    DEL:"     CNT-SYUNOU-DEL
           DISPLAY "*** ORCBMRECE PTBYOMEI  UPD:"     CNT-PTBYOMEI-UPD
           DISPLAY "*** ORCBMRECE PTBYOMEI  DEL:"     CNT-PTBYOMEI-DEL
           DISPLAY "*** ORCBMRECE END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-SRYACCT-UPD TO  JOB-UPDCNT
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT               RECEERR-FILE
           IF          STS-RECEERR     =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR     TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM   900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター検索
      *****************************************************************
       910-SRYACT-SELECT-SEC        SECTION.
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       910-SRYACT-SELECT-EXT.
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
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
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
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
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
