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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCSROUKAISU.
      ****************************************************************
      *    OS           LINUX                                         
      *                                                               
      *    PROGRAM      ORCSROUKAISU  /  労災レセプト回数取得サブ     
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/11/22    NACL−門脇    新規作成
      ****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.04.01    NACL-門脇    07/03/12  包括診療対応
      *  03.05.00    NACL-門脇    07/05/08  グループ診療対応
      *****************************************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE SECTION.
      *
      *    添え字領域
       01  IDX-AREA.
         03  IDX                        PIC 9(02).
         03  IDY                        PIC 9(02).
         03  IDZ                        PIC 9(02).
         03  IDW                        PIC 9(02).
         03  IDV                        PIC 9(02).
      *
      *    フラグ領域
       01  FLG-AREA.
         03  FLG-SYSKANRI               PIC 9(01).
         03  FLG-NYUINACCT              PIC 9(01).
         03  FLG-SRYACCT                PIC 9(01).
         03  FLG-ARI                    PIC 9(01).
         03  FLG-NG                     PIC 9(01).
         03  FLG-TAIHI                  PIC 9(01).
      *
      *    ワーク領域
       01  WRK-AREA.
         03  WRK-RYOSTYMD.
           05  WRK-RYOSTYM              PIC 9(06).
           05  WRK-RYOSTDD              PIC 9(02).
         03  WRK-RYOSTYMD-R   REDEFINES   WRK-RYOSTYMD.
           05  WRK-RYOSTYMD-9           PIC 9(08).
      *
         03  WRK-KIJYUNYM-KSI.
           05  WRK-KIJYUNYM-KSI-9       PIC 9(06).
      *
         03  WRK-1001-HOSPSBT1          PIC 9(01).
         03  WRK-NYUGAIKBN              PIC X(01).
      *
         03  WRK-COUNT                  PIC 9(04).
      *
         03  WRK-NYUGAI-DAY             PIC 9(02).
      *
         03  WRK-SRYKAINF.
          04  WRK-SRYKAINF-OCC          OCCURS  20.
           05  WRK-SRYKA                PIC X(02).
           05  WRK-DAYFLG               PIC X(01)   OCCURS  31.
      *
         03  WRK-DAYINF.
           05  WRK-DAY5                 PIC 9(04)   OCCURS  31.
      *
         03  WRK-STYMD                  PIC X(08).
      *
         03  WRK-RECESRYKA              PIC X(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    システム管理
           COPY    "CPSK1001.INC".
           COPY    "CPSK1005.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      ***  COPY    "CPORCMCP.INC".
      *
      /
      ***
       LINKAGE             SECTION.
           COPY  "CPORCSROUKAISU.INC".
      *
grpsys     COPY  "COMMON-SPA".
      /
       PROCEDURE           DIVISION  USING
                                     LNK-ORCSROUKAISU-REC
                                     SPA-AREA.
      ******************************************************************
      *        依頼制御処理
      ******************************************************************
       MAIN-RTN            SECTION.
       MAIN-000.
      *
           INITIALIZE             LNK-ORCSROUKAISU-OUT-AREA
           MOVE     ZERO    TO    LNK-ORCSROUKAISU-RC
      *
           INITIALIZE                  IDX-AREA
                                       FLG-AREA
                                       WRK-AREA
      *
      *    処理開始年月の設定
           IF      LNK-ORCSROUKAISU-KIJYUNYM-KSI  =  SPACE
              MOVE   LNK-ORCSROUKAISU-RYOSTYMD(1:6)
                                    TO   WRK-KIJYUNYM-KSI
           ELSE
              MOVE   LNK-ORCSROUKAISU-KIJYUNYM-KSI
                                    TO   WRK-KIJYUNYM-KSI
           END-IF
           IF      LNK-ORCSROUKAISU-SRYYM  <  WRK-KIJYUNYM-KSI-9
              MOVE   LNK-ORCSROUKAISU-RYOSTYMD  TO   WRK-RYOSTYMD
              MOVE   "01"                       TO   WRK-RYOSTYMD(7:2)
      *       カウント開始回数の設定
              MOVE   ZERO                       TO   WRK-COUNT
           ELSE
              MOVE   WRK-KIJYUNYM-KSI           TO   WRK-RYOSTYMD(1:6)
              MOVE   "01"                       TO   WRK-RYOSTYMD(7:2)
      *       カウント開始回数の設定
              IF      LNK-ORCSROUKAISU-KAISU-KSI     =  ZERO
                 MOVE      1     TO    LNK-ORCSROUKAISU-KAISU-KSI
              END-IF
              COMPUTE    WRK-COUNT  =  LNK-ORCSROUKAISU-KAISU-KSI  -   1
           END-IF
      *
      *    回数取得
           PERFORM   UNTIL  (WRK-RYOSTYM  >  LNK-ORCSROUKAISU-SRYYM)
                      OR    (LNK-ORCSROUKAISU-RC   NOT =   ZERO    )
      *
      *      旧総合病院フラグ取得
             PERFORM    100-SYSKANRI-1001-GET-SEC
      *
             IF    LNK-ORCSROUKAISU-RC   =   ZERO
                PERFORM    200-KEISAN-SEC
             END-IF
      *
             IF    LNK-ORCSROUKAISU-RC   =   ZERO
      *        次月を求める
               INITIALIZE                   STS-AREA-DAY
               INITIALIZE                   LNK-DAY6-AREA
               MOVE    "61"             TO  LNK-DAY6-IRAI
               MOVE    WRK-RYOSTYMD-9   TO  LNK-DAY6-KIJUN
               MOVE    "2"              TO  LNK-DAY6-ZOGENPTN
               MOVE     1               TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"        USING   STS-AREA-DAY
                                                LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN  TO  WRK-RYOSTYMD-9
             END-IF
      *
           END-PERFORM
      *
           IF    LNK-ORCSROUKAISU-RC   =   ZERO
               IF        WRK-COUNT   >   999
                  MOVE   999        TO   LNK-ORCSROUKAISU-RECEKAISU
               ELSE
                  MOVE   WRK-COUNT  TO   LNK-ORCSROUKAISU-RECEKAISU
               END-IF
           END-IF
      *
           .
      *
       MAIN-EXT.
           EXIT PROGRAM
           STOP RUN.
      *
      *****************************************************************
      *    旧総合病院フラグ取得
      *****************************************************************
       100-SYSKANRI-1001-GET-SEC         SECTION.
      *
           INITIALIZE                         SYS-1001-REC
           MOVE    "1001"               TO    SYS-1001-KANRICD
           MOVE    "*"                  TO    SYS-1001-KBNCD
           MOVE    LNK-ORCSROUKAISU-HOSPNUM
                                        TO    SYS-1001-HOSPNUM
           MOVE    WRK-RYOSTYMD         TO    SYS-1001-STYUKYMD
           MOVE    WRK-RYOSTYMD         TO    SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC         TO    MCPDATA-REC
           MOVE    "DBSELECT"           TO    MCP-FUNC
           MOVE    "tbl_syskanri"       TO    MCP-TABLE
           MOVE    "key10"              TO    MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
           IF      MCP-RC               =     ZERO
               MOVE     "tbl_syskanri"  TO    MCP-TABLE
               MOVE     "key10"         TO    MCP-PATHNAME
               PERFORM   900-SYSKANRI-READ-SEC
               IF    FLG-SYSKANRI   =   ZERO
                  MOVE  SYS-1001-HOSPSBT1
                                        TO    WRK-1001-HOSPSBT1
               ELSE
                  MOVE     11           TO    LNK-ORCSROUKAISU-RC
               END-IF
           ELSE
                  MOVE     11           TO    LNK-ORCSROUKAISU-RC
           END-IF
      *
           MOVE     "tbl_syskanri"      TO    MCP-TABLE
           MOVE     "key10"             TO    MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
           .
      *
       100-SYSKANRI-1001-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    回数取得
      *****************************************************************
       200-KEISAN-SEC         SECTION.
      *
      *    ＜該当診療月以前＞
           IF    WRK-RYOSTYM   <    LNK-ORCSROUKAISU-SRYYM
      *
      *******入院分
             MOVE    ZERO          TO   FLG-ARI
             PERFORM      2001-NACCT-KENSAKU-SEC
             IF      FLG-ARI   =   ZERO
                MOVE      "1"      TO   WRK-NYUGAIKBN
                PERFORM   2002-ACCT-KENSAKU-SEC
             END-IF
             IF      FLG-ARI   =   1
                  ADD      1       TO   WRK-COUNT
             END-IF
      *
      *******外来分
      *      旧総合病院の場合
             IF   WRK-1001-HOSPSBT1   =   1
               INITIALIZE               WRK-SRYKAINF
               MOVE       "2"      TO   WRK-NYUGAIKBN
               MOVE        1       TO   FLG-TAIHI
               PERFORM    2003-ACCT-KENSAKU2-SEC
               PERFORM  VARYING  IDX    FROM    1   BY  1
                         UNTIL  (IDX            >    20)
                          OR    (WRK-SRYKA(IDX) = SPACE)
                    ADD      1     TO   WRK-COUNT
               END-PERFORM
             ELSE
      *        旧総合病院でない場合
               MOVE    ZERO        TO   FLG-ARI
               MOVE       "2"      TO   WRK-NYUGAIKBN
               PERFORM    2002-ACCT-KENSAKU-SEC
               IF      FLG-ARI   =   1
                    ADD      1     TO   WRK-COUNT
               END-IF
             END-IF
      *
           END-IF
      *
      *    ＜該当診療月＞
           IF    WRK-RYOSTYM   =    LNK-ORCSROUKAISU-SRYYM
      *
      *******入院時：外来編集処理
             IF    LNK-ORCSROUKAISU-NYUGAIKBN  =  "1"
      *
               INITIALIZE               WRK-SRYKAINF
               MOVE       "2"      TO   WRK-NYUGAIKBN
               IF   WRK-1001-HOSPSBT1   =   1
                  MOVE     2       TO   FLG-TAIHI
               ELSE
                  MOVE     3       TO   FLG-TAIHI
               END-IF
               PERFORM    2003-ACCT-KENSAKU2-SEC
      *
               INITIALIZE                 WRK-DAYINF
               PERFORM    2004-NACCT-KENSAKU2-SEC
      *
               MOVE  LNK-ORCSROUKAISU-SRYYM  TO  WRK-STYMD(1:6)
               MOVE  "01"                    TO  WRK-STYMD(7:2)
               IF    LNK-ORCSROUKAISU-RYOSTYMD   <   WRK-STYMD
                  MOVE   WRK-STYMD(7:2)           TO   IDV
               ELSE
                  MOVE   LNK-ORCSROUKAISU-RYOSTYMD(7:2)
                                                  TO   IDV
               END-IF
               MOVE   LNK-ORCSROUKAISU-KAISHIBI   TO   IDW
      *
               PERFORM  VARYING  IDX    FROM    1   BY  1
                         UNTIL  (IDX            >    20)
                          OR    (WRK-SRYKA(IDX) = SPACE)
                  PERFORM   VARYING   IDY   FROM   IDV   BY   1
                             UNTIL    IDY     >=   IDW
                     IF  WRK-DAYFLG(IDX IDY)   =   SPACE
                     AND WRK-DAY5(IDY)         =   ZERO
                          MOVE    "2"    TO    WRK-DAYFLG(IDX IDY)
                     END-IF
                  END-PERFORM
                  PERFORM   VARYING   IDY   FROM   IDV   BY   1
                             UNTIL    IDY     >    IDW
                     IF  WRK-DAYFLG(IDX IDY)  NOT =  SPACE
                          ADD      1     TO    WRK-COUNT
                          MOVE    IDW    TO    IDY
                     END-IF
                  END-PERFORM
               END-PERFORM
      *
               ADD      1        TO   WRK-COUNT
      *
             ELSE
      *
      *******  外来時：入院編集処理
               INITIALIZE                 WRK-DAYINF
               MOVE    ZERO          TO   WRK-NYUGAI-DAY
      *
               PERFORM    2005-NACCT-KENSAKU3-SEC
      *
               IF   WRK-NYUGAI-DAY   =    ZERO
                 MOVE       ZERO     TO   FLG-ARI
                 MOVE       "1"      TO   WRK-NYUGAIKBN
                 PERFORM    2002-ACCT-KENSAKU-SEC
                 IF   FLG-ARI   =   1
                    PERFORM   VARYING   IDX   FROM   1   BY   1
                               UNTIL   (IDX                 >    31)
                                OR     (WRK-NYUGAI-DAY  NOT =  ZERO)
                       IF  WRK-DAY5(IDX)  NOT  =  ZERO
                            MOVE   IDX    TO   WRK-NYUGAI-DAY
                       END-IF
                    END-PERFORM
                 END-IF
               END-IF
      *
               IF   WRK-NYUGAI-DAY   NOT =   ZERO
               AND  WRK-NYUGAI-DAY   <   LNK-ORCSROUKAISU-KAISHIBI
                     ADD      1      TO   WRK-COUNT
               END-IF
      *
      *        旧総合病院の場合
               IF   WRK-1001-HOSPSBT1   =   1
      *
                  INITIALIZE               WRK-SRYKAINF
                  MOVE       "2"      TO   WRK-NYUGAIKBN
                  MOVE        4       TO   FLG-TAIHI
                  PERFORM    2003-ACCT-KENSAKU2-SEC
      *
                  MOVE  LNK-ORCSROUKAISU-SRYYM  TO  WRK-STYMD(1:6)
                  MOVE  "01"                    TO  WRK-STYMD(7:2)
                  IF    LNK-ORCSROUKAISU-RYOSTYMD   <   WRK-STYMD
                     MOVE   WRK-STYMD(7:2)           TO   IDV
                  ELSE
                     MOVE   LNK-ORCSROUKAISU-RYOSTYMD(7:2)
                                                     TO   IDV
                  END-IF
                  MOVE   LNK-ORCSROUKAISU-KAISHIBI   TO   IDW
      *
                  PERFORM  VARYING  IDX    FROM    1   BY  1
                            UNTIL  (IDX            >    20)
                             OR    (WRK-SRYKA(IDX) = SPACE)
                     PERFORM   VARYING   IDY   FROM   IDV   BY   1
                                UNTIL    IDY     >    IDW
                        IF  WRK-DAYFLG(IDX IDY)   =   SPACE
                        AND WRK-DAY5(IDY)         =   ZERO
                             MOVE    "2"    TO    WRK-DAYFLG(IDX IDY)
                        END-IF
                     END-PERFORM
                     PERFORM   VARYING   IDY   FROM   IDV   BY   1
                                UNTIL    IDY     >    IDW
                        IF  WRK-DAYFLG(IDX IDY)  NOT =  SPACE
                          IF   IDY < IDW
                             ADD      1     TO    WRK-COUNT
                             MOVE    IDW    TO    IDY
                          ELSE
                             PERFORM    2006-SYSKANRI-1005-GET-SEC
                             IF  WRK-RECESRYKA  =  SPACE
                             OR  WRK-RECESRYKA
                                         <  LNK-ORCSROUKAISU-RECESRYKA
                                ADD      1     TO    WRK-COUNT
                                MOVE    IDW    TO    IDY
                             END-IF
                          END-IF
                        END-IF
                     END-PERFORM
                  END-PERFORM
      *
               END-IF
      *
               ADD       1       TO   WRK-COUNT
      *
             END-IF
      *
           END-IF
      *
           .
      *
       200-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計検索
      *****************************************************************
       2001-NACCT-KENSAKU-SEC         SECTION.
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    LNK-ORCSROUKAISU-HOSPNUM TO   NACCT-HOSPNUM
           MOVE    LNK-ORCSROUKAISU-PTID    TO   NACCT-PTID
           MOVE    WRK-RYOSTYM              TO   NACCT-SRYYM
           MOVE    "5"                      TO   NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC            TO   MCPDATA-REC
           MOVE    "DBSELECT"               TO   MCP-FUNC
           MOVE    "tbl_nyuinacct"          TO   MCP-TABLE
           MOVE    "key17"                  TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING   MCPAREA
                                                    MCPDATA-REC
                                                    SPA-AREA
           IF      MCP-RC             =       ZERO
                 MOVE    "tbl_nyuinacct"    TO   MCP-TABLE
                 MOVE    "key17"            TO   MCP-PATHNAME
                 PERFORM  900-NYUINACCT-READ-SEC
                 PERFORM  UNTIL  (FLG-NYUINACCT  =  1)
                           OR    (FLG-ARI        =  1)
      *
                    PERFORM   VARYING   IDX   FROM   1   BY   1
                               UNTIL   (IDX       >    31)
                                OR     (FLG-ARI   =     1)
                       IF  NACCT-DAY(IDX)  =  LNK-ORCSROUKAISU-HKNCOMBI
                              MOVE    1     TO     FLG-ARI
                       END-IF
                    END-PERFORM
      *
                    MOVE    "tbl_nyuinacct" TO   MCP-TABLE
                    MOVE    "key17"         TO   MCP-PATHNAME
                    PERFORM  900-NYUINACCT-READ-SEC
                 END-PERFORM
           END-IF
      *
           MOVE    "tbl_nyuinacct" TO   MCP-TABLE
           MOVE    "key17"         TO   MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           .
      *
       2001-NACCT-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計検索
      *****************************************************************
       2002-ACCT-KENSAKU-SEC         SECTION.
      *
           INITIALIZE                              SRYACCT-REC
           MOVE    LNK-ORCSROUKAISU-HOSPNUM   TO   ACCT-HOSPNUM
           MOVE    WRK-NYUGAIKBN              TO   ACCT-NYUGAIKBN
           MOVE    LNK-ORCSROUKAISU-PTID      TO   ACCT-PTID
           MOVE    WRK-RYOSTYM                TO   ACCT-SRYYM
           MOVE    LNK-ORCSROUKAISU-HKNCOMBI  TO   ACCT-HKNCOMBI
           MOVE    SRYACCT-REC                TO   MCPDATA-REC
           MOVE    "DBSELECT"                 TO   MCP-FUNC
           MOVE    "tbl_sryacct"              TO   MCP-TABLE
           MOVE    "key22"                    TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"                USING   MCPAREA
                                                      MCPDATA-REC
                                                      SPA-AREA
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"          TO   MCP-TABLE
               MOVE    "key22"                TO   MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM    UNTIL   (FLG-SRYACCT   =    1)  OR
                                  (FLG-ARI       =    1)
      *
                  IF    ACCT-SRYKBN   =  "95"  OR  "96"
                     CONTINUE
                  ELSE
                     IF   (ACCT-ZAITEN    NOT =    ZERO) AND
                          (ACCT-ZAIKAISU  NOT =    ZERO) AND
                          (ACCT-JIHOKBN       =   SPACE)
                         MOVE     1     TO    FLG-ARI
                     END-IF
                  END-IF
      *
                  MOVE    "tbl_sryacct"   TO   MCP-TABLE
                  MOVE    "key22"         TO   MCP-PATHNAME
                  PERFORM  900-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"   TO   MCP-TABLE
           MOVE    "key22"         TO   MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           .
      *
       2002-ACCT-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計検索２
      *****************************************************************
       2003-ACCT-KENSAKU2-SEC         SECTION.
      *
           INITIALIZE                              SRYACCT-REC
           MOVE    LNK-ORCSROUKAISU-HOSPNUM   TO   ACCT-HOSPNUM
           MOVE    WRK-NYUGAIKBN              TO   ACCT-NYUGAIKBN
           MOVE    LNK-ORCSROUKAISU-PTID      TO   ACCT-PTID
           MOVE    WRK-RYOSTYM                TO   ACCT-SRYYM
           MOVE    LNK-ORCSROUKAISU-HKNCOMBI  TO   ACCT-HKNCOMBI
           MOVE    SRYACCT-REC                TO   MCPDATA-REC
           MOVE    "DBSELECT"                 TO   MCP-FUNC
           MOVE    "tbl_sryacct"              TO   MCP-TABLE
           MOVE    "key22"                    TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"                USING   MCPAREA
                                                      MCPDATA-REC
                                                      SPA-AREA
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"          TO   MCP-TABLE
               MOVE    "key22"                TO   MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM    UNTIL    FLG-SRYACCT   =    1
                  IF    ACCT-SRYKBN   =  "95"  OR  "96"
                     CONTINUE
                  ELSE
                     IF   (ACCT-ZAITEN    NOT =    ZERO) AND
                          (ACCT-ZAIKAISU  NOT =    ZERO) AND
                          (ACCT-JIHOKBN       =   SPACE)
      *
                        PERFORM   20031-ACCT-HENSYU-SEC
      *
                     END-IF
                  END-IF
                  MOVE     "tbl_sryacct"      TO   MCP-TABLE
                  MOVE     "key22"            TO   MCP-PATHNAME
                  PERFORM   900-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"   TO   MCP-TABLE
           MOVE    "key22"         TO   MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           .
      *
       2003-ACCT-KENSAKU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計編集
      *****************************************************************
       20031-ACCT-HENSYU-SEC         SECTION.
      *
           EVALUATE     FLG-TAIHI
              WHEN          1
      *
                 MOVE     ZERO     TO     FLG-NG
                 PERFORM  VARYING  IDX    FROM    1   BY  1
                           UNTIL  (IDX            >    20)
                            OR    (WRK-SRYKA(IDX) = SPACE)
                            OR    (FLG-NG         =     1)
                    IF   WRK-SRYKA(IDX)  =  ACCT-SRYKA
                           MOVE    1    TO    FLG-NG
                    END-IF
                 END-PERFORM
                 IF   FLG-NG   =   ZERO
                   IF  IDX    <=    20
                      MOVE   ACCT-SRYKA    TO   WRK-SRYKA(IDX)
                   END-IF
                 END-IF
      *
              WHEN          2
      *
                 MOVE     ZERO     TO     FLG-NG
                 MOVE     ZERO     TO     IDZ
                 PERFORM  VARYING  IDX    FROM    1   BY  1
                           UNTIL  (IDX            >    20)
                            OR    (WRK-SRYKA(IDX) = SPACE)
                            OR    (FLG-NG         =     1)
                    IF   WRK-SRYKA(IDX)  =  ACCT-SRYKA
                           MOVE    1    TO    FLG-NG
                           MOVE   IDX   TO    IDZ
                    END-IF
                 END-PERFORM
                 IF   FLG-NG   =   ZERO
                   IF  IDX    <=    20
                      MOVE   ACCT-SRYKA    TO   WRK-SRYKA(IDX)
                      PERFORM  VARYING  IDY  FROM  1  BY  1
                                UNTIL   IDY  >    31
                         IF   ACCT-DAY(1 IDY)  NOT = ZERO
                            MOVE  "1"  TO  WRK-DAYFLG(IDX IDY)
                         END-IF
                      END-PERFORM
                   END-IF
                 ELSE
                      PERFORM  VARYING  IDY  FROM  1  BY  1
                                UNTIL   IDY  >    31
                         IF   ACCT-DAY(1 IDY)  NOT = ZERO
                            MOVE  "1"  TO  WRK-DAYFLG(IDZ IDY)
                         END-IF
                      END-PERFORM
                 END-IF
      *
              WHEN          3
      *
                 MOVE   "00"       TO   WRK-SRYKA(1)
                 PERFORM  VARYING  IDX  FROM  1  BY  1
                           UNTIL   IDX  >    31
                     IF   ACCT-DAY(1 IDX)  NOT = ZERO
                        MOVE  "1"  TO  WRK-DAYFLG(1 IDX)
                     END-IF
                 END-PERFORM
      *
              WHEN          4
      *
                 IF   LNK-ORCSROUKAISU-SRYKA  NOT =  ACCT-SRYKA
                   MOVE     ZERO     TO     FLG-NG
                   MOVE     ZERO     TO     IDZ
                   PERFORM  VARYING  IDX    FROM    1   BY  1
                             UNTIL  (IDX            >    20)
                              OR    (WRK-SRYKA(IDX) = SPACE)
                              OR    (FLG-NG         =     1)
                      IF   WRK-SRYKA(IDX)  =  ACCT-SRYKA
                             MOVE    1    TO    FLG-NG
                             MOVE   IDX   TO    IDZ
                      END-IF
                   END-PERFORM
                   IF   FLG-NG   =   ZERO
                      IF  IDX    <=    20
                         MOVE   ACCT-SRYKA    TO   WRK-SRYKA(IDX)
                         PERFORM  VARYING  IDY  FROM  1  BY  1
                                   UNTIL   IDY  >    31
                            IF   ACCT-DAY(1 IDY)  NOT = ZERO
                               MOVE  "1"  TO  WRK-DAYFLG(IDX IDY)
                            END-IF
                         END-PERFORM
                      END-IF
                   ELSE
                         PERFORM  VARYING  IDY  FROM  1  BY  1
                                   UNTIL   IDY  >    31
                            IF   ACCT-DAY(1 IDY)  NOT = ZERO
                               MOVE  "1"  TO  WRK-DAYFLG(IDZ IDY)
                            END-IF
                         END-PERFORM
                   END-IF
                 END-IF
      *
           END-EVALUATE
      *
           .
      *
       20031-ACCT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計検索２
      *****************************************************************
       2004-NACCT-KENSAKU2-SEC         SECTION.
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    LNK-ORCSROUKAISU-HOSPNUM TO   NACCT-HOSPNUM
           MOVE    LNK-ORCSROUKAISU-PTID    TO   NACCT-PTID
           MOVE    WRK-RYOSTYM              TO   NACCT-SRYYM
           MOVE    "5"                      TO   NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC            TO   MCPDATA-REC
           MOVE    "DBSELECT"               TO   MCP-FUNC
           MOVE    "tbl_nyuinacct"          TO   MCP-TABLE
           MOVE    "key17"                  TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING   MCPAREA
                                                    MCPDATA-REC
                                                    SPA-AREA
           IF      MCP-RC             =       ZERO
                 MOVE    "tbl_nyuinacct"    TO   MCP-TABLE
                 MOVE    "key17"            TO   MCP-PATHNAME
                 PERFORM  900-NYUINACCT-READ-SEC
                 PERFORM  UNTIL   FLG-NYUINACCT  =  1
      *
                    PERFORM   VARYING   IDX   FROM   1   BY   1
                               UNTIL    IDX    >    31
                       IF     NACCT-DAY(IDX)  NOT = ZERO
                          MOVE   NACCT-DAY(IDX)   TO   WRK-DAY5(IDX)
                       END-IF
                    END-PERFORM
      *
                    MOVE    "tbl_nyuinacct" TO   MCP-TABLE
                    MOVE    "key17"         TO   MCP-PATHNAME
                    PERFORM  900-NYUINACCT-READ-SEC
                 END-PERFORM
           END-IF
      *
           MOVE    "tbl_nyuinacct" TO   MCP-TABLE
           MOVE    "key17"         TO   MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           .
      *
       2004-NACCT-KENSAKU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計検索３
      *****************************************************************
       2005-NACCT-KENSAKU3-SEC         SECTION.
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    LNK-ORCSROUKAISU-HOSPNUM TO   NACCT-HOSPNUM
           MOVE    LNK-ORCSROUKAISU-PTID    TO   NACCT-PTID
           MOVE    WRK-RYOSTYM              TO   NACCT-SRYYM
           MOVE    "5"                      TO   NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC            TO   MCPDATA-REC
           MOVE    "DBSELECT"               TO   MCP-FUNC
           MOVE    "tbl_nyuinacct"          TO   MCP-TABLE
           MOVE    "key17"                  TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING   MCPAREA
                                                    MCPDATA-REC
                                                    SPA-AREA
           IF      MCP-RC             =       ZERO
                 MOVE    "tbl_nyuinacct"    TO   MCP-TABLE
                 MOVE    "key17"            TO   MCP-PATHNAME
                 PERFORM  900-NYUINACCT-READ-SEC
                 PERFORM  UNTIL   FLG-NYUINACCT  =  1
      *
                    PERFORM   VARYING   IDX   FROM   1   BY   1
                               UNTIL    IDX    >    31
                       IF  NACCT-DAY(IDX)  =  LNK-ORCSROUKAISU-HKNCOMBI
                           IF  WRK-NYUGAI-DAY  =  ZERO
                                  MOVE   IDX   TO   WRK-NYUGAI-DAY
                           ELSE
                              IF  WRK-NYUGAI-DAY  >  IDX
                                  MOVE   IDX   TO   WRK-NYUGAI-DAY
                              END-IF
                           END-IF
                       END-IF
                    END-PERFORM
      *
                    PERFORM   VARYING   IDX   FROM   1   BY   1
                               UNTIL    IDX    >    31
                       IF     NACCT-DAY(IDX)  NOT = ZERO
                          MOVE   NACCT-DAY(IDX)   TO   WRK-DAY5(IDX)
                       END-IF
                    END-PERFORM
      *
                    MOVE    "tbl_nyuinacct"    TO   MCP-TABLE
                    MOVE    "key17"            TO   MCP-PATHNAME
                    PERFORM  900-NYUINACCT-READ-SEC
                 END-PERFORM
           END-IF
      *
           MOVE    "tbl_nyuinacct" TO   MCP-TABLE
           MOVE    "key17"         TO   MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           .
      *
       2005-NACCT-KENSAKU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電算診療科（コード）取得
      *****************************************************************
       2006-SYSKANRI-1005-GET-SEC         SECTION.
      *
           INITIALIZE                           SYS-1005-REC
           MOVE    "1005"                 TO    SYS-1005-KANRICD
           MOVE    WRK-SRYKA(IDX)         TO    SYS-1005-KBNCD
           MOVE    LNK-ORCSROUKAISU-HOSPNUM
                                          TO    SYS-1005-HOSPNUM
           MOVE    LNK-ORCSROUKAISU-SRYYM TO    SYS-1005-STYUKYMD(1:6)
           MOVE    "01"                   TO    SYS-1005-STYUKYMD(7:2)
           MOVE    LNK-ORCSROUKAISU-SRYYM TO    SYS-1005-EDYUKYMD(1:6)
           MOVE    "01"                   TO    SYS-1005-EDYUKYMD(7:2)
           MOVE    SYS-1005-REC           TO    MCPDATA-REC
           MOVE    "DBSELECT"             TO    MCP-FUNC
           MOVE    "tbl_syskanri"         TO    MCP-TABLE
           MOVE    "key10"                TO    MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING   MCPAREA
                                                  MCPDATA-REC
                                                  SPA-AREA
           IF      MCP-RC               =     ZERO
               MOVE     "tbl_syskanri"    TO    MCP-TABLE
               MOVE     "key10"           TO    MCP-PATHNAME
               PERFORM   900-SYSKANRI-READ-SEC
               IF    FLG-SYSKANRI   =   ZERO
                  MOVE  SYS-1005-RECESRYKA
                                        TO    WRK-RECESRYKA
               ELSE
                  MOVE  SPACE           TO    WRK-RECESRYKA
               END-IF
           END-IF
      *
           MOVE    "tbl_syskanri"       TO    MCP-TABLE
           MOVE    "key10"              TO    MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           .
      *
       2006-SYSKANRI-1005-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
                                           SYS-1005-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYS-1001-REC
                                           SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター読込
      *****************************************************************
       900-NYUINACCT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
      *
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
      *
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
      *
