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
       PROGRAM-ID.             ORCBHYOUKETU.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次
      *  コンポーネント名  : 入院会計標欠対応（平成１８年度以降に対応）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/03/29    NACL-小豆沢   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-小豆沢  07/06/14  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                     PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEERR.INC".
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-RECEERR                 PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACCT-UPD           PIC 9(01).
           03  FLG-NYUINACT                PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-DBRC                    PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                    PIC 9(05).
           03  CNT-PAGE                    PIC 9(05).
           03  CNT-JOB-UPDCNT              PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME                    PIC 9(08).
           03  SYS-YMD.
               05  SYS-YY                  PIC 9(02).
               05  SYS-MM                  PIC 9(02).
               05  SYS-DD                  PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                                 PIC 9(05).
      *
      *    パラメタ領域
       01  PARA-AREA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID                      PIC 9(07).
           03  WRK-PARA-SHELLID                    PIC X(08).
           03  WRK-PARA-HOSPNUM                    PIC 9(02).
           03  WRK-PARA-SRYYM                      PIC X(06).
           03  WRK-PARA-KBN                        PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-DBPATH                          PIC X(100).
           03  WRK-ERRNO                           PIC 9(2).
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-HOSPNUM                         PIC 9(02).
           03  WRK-MEISAISU                        PIC 9(07).
           03  WRK-SURYOTOTAL                      PIC 9(07).
           03  WRK-SRYCDTOTAL                      PIC 9(14).
           03  WRK-SRYCDX                          PIC X(09).
           03  WRK-SRYCD9       REDEFINES     WRK-SRYCDX
                                                   PIC 9(09).
           03  WRK-ZAITEN                          PIC 9(08).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院行為
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
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
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
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
           02  FILLER                  PIC X(1000).
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
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
           DISPLAY "100-INIT-SEC"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
      *
           INITIALIZE                  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
           INITIALIZE                  PARA-AREA
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
                                       WRK-PARA-KBN
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO   SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORCBHYOUKETSU"     TO  JOB-PGID
           EVALUATE   WRK-PARA-KBN
               WHEN   "1"
                 MOVE    "標欠会計更新１００分の１０"
                                       TO  JOB-SHELLMSG
               WHEN   "2"
                 MOVE    "標欠会計更新１００分の１５"
                                       TO  JOB-SHELLMSG
               WHEN   "3"
                 MOVE    "標欠会計更新１００分の２"
                                       TO  JOB-SHELLMSG
               WHEN   "4"
                 MOVE    "標欠会計更新１００分の３"
                                       TO  JOB-SHELLMSG
               WHEN   "5"
                 MOVE    "標欠会計更新（標欠取消）"
                                       TO  JOB-SHELLMSG
           END-EVALUATE
      *
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           MOVE    "STP"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    3                   TO  JOB-STOPFLG
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  RECEERR
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    医療機関ＩＤ編集
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-1001-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
           MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD

           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-SYSKANRI-SELECT-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM     910-SYSKANRI-READ-SEC
                   IF    FLG-SYSKANRI  =   ZERO
                         MOVE    SYS-1001-HOSPNUM  TO  WRK-HOSPNUM
                   ELSE
                         MOVE  "医療機関情報が取得できませんでした。"
                                           TO  WRK-RECEERR
                         PERFORM 500-ERR-HENSYU-SEC
                         GO  TO  100-INIT-EXT
                   END-IF
           ELSE
               MOVE    "医療機関情報が取得できませんでした。"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  100-INIT-EXT
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    処理年月チェック
           IF    WRK-PARA-SRYYM    >=   "200604"
               CONTINUE
           ELSE
               MOVE  "処理対象年月が誤りです。"
                                      TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    処理区分チェック
           IF    WRK-PARA-KBN    =   "1" OR "2" OR "3" OR "4" OR "5"
               CONTINUE
           ELSE
               MOVE  "処理区分が誤りです。"
                                      TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  100-INIT-EXT
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
           MOVE    SYS-1001-HOSPNUM    TO  NACCT-HOSPNUM
           MOVE    WRK-PARA-SRYYM      TO  NACCT-SRYYM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    "1"                 TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT       =   ZERO
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key27"             TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               PERFORM  UNTIL  FLG-NYUINACCT    =    1
                   IF   NACCT-SRYCDTOTAL  NOT = 99999916
                       EVALUATE   WRK-PARA-KBN
                           WHEN   "1"
                             PERFORM   210-NYUINACT-UPD1-SEC
                           WHEN   "2"
                             PERFORM   210-NYUINACT-UPD2-SEC
                           WHEN   "3"
                             PERFORM   210-NYUINACT-UPD3-SEC
                           WHEN   "4"
                             PERFORM   210-NYUINACT-UPD4-SEC
                           WHEN   "5"
                             PERFORM   210-NYUINACT-UPD5-SEC
                       END-EVALUATE
                       IF    FLG-NYUINACCT-UPD   =   1
                          PERFORM   210-NYUINACCT-UPD-SEC
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key27"             TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為編集処理　処理区分１（１００分の１０減額）
      *****************************************************************
       210-NYUINACT-UPD1-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT-UPD
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYKA         TO  NSRY-SRYKA
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACT-SELECT-SEC
           IF      FLG-NYUINACT        =   ZERO
      *
               MOVE    "tbl_nyuinact"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-NYUINACT-READ-SEC
               IF  (FLG-NYUINACT        =   ZERO)   AND
                   (NSRY-SRYCD(1)(1:3)  NOT =  "188")
      *
                   PERFORM   210-HYOUKETU-CLEAR-SEC
      *
                   MOVE    1      TO   FLG-NYUINACCT-UPD
                   IF   NSRY-SRYCD(2)   =  SPACE
                       MOVE  "190107890"    TO   NSRY-SRYCD(2)
                       MOVE   1             TO   NSRY-SRYSURYO(2)
                   ELSE
                       IF   NSRY-SRYCD(3)   =  SPACE
                           MOVE  "190107890"   TO   NSRY-SRYCD(3)
                           MOVE   1            TO   NSRY-SRYSURYO(3)
                       END-IF
                   END-IF
      *            明細数・数量計・診療コード計
                   MOVE     ZERO    TO   WRK-MEISAISU
                                         WRK-SURYOTOTAL
                                         WRK-SRYCDTOTAL
                   PERFORM  VARYING   IDX   FROM  1  BY  1
                     UNTIL  (IDX      >     5)    OR
                            (NSRY-SRYCD(IDX)   =   SPACE)
                         ADD    1       TO   WRK-MEISAISU
                         ADD    1       TO   WRK-SURYOTOTAL
                         MOVE    NSRY-SRYCD(IDX)   TO  WRK-SRYCDX
                         ADD     WRK-SRYCD9        TO  WRK-SRYCDTOTAL
                   END-PERFORM
      *            剤点数
                   MOVE     ZERO      TO   WRK-ZAITEN
                   PERFORM  VARYING   IDX   FROM  1  BY  1
                     UNTIL  (IDX      >     5)    OR
                            (NSRY-SRYCD(IDX)   =   SPACE)
                       PERFORM   210-ZAITEN-SET-SEC
                   END-PERFORM
      *            入院診療行為更新
                   MOVE   NYUINACT-REC    TO   MCPDATA-REC
                   MOVE  "tbl_nyuinact"   TO   MCP-TABLE
                   MOVE  "key"            TO   MCP-PATHNAME
                   MOVE  "DBUPDATE"       TO   MCP-FUNC
                   PERFORM   900-ORCDBMAIN-SEC
                   IF   MCP-RC   NOT =  ZERO
                        DISPLAY  "NYUINACT UPDATE MCP-RC = " MCP-RC
                   END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       210-NYUINACT-UPD1-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為編集処理　処理区分２（１００分の１５減額）
      *****************************************************************
       210-NYUINACT-UPD2-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT-UPD
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYKA         TO  NSRY-SRYKA
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACT-SELECT-SEC
           IF      FLG-NYUINACT        =   ZERO
      *
               MOVE    "tbl_nyuinact"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-NYUINACT-READ-SEC
               IF  (FLG-NYUINACT        =   ZERO)   AND
                   (NSRY-SRYCD(1)(1:3)  NOT =  "188")
      *
                   PERFORM   210-HYOUKETU-CLEAR-SEC
      *
                   MOVE    1      TO   FLG-NYUINACCT-UPD
                   IF   NSRY-SRYCD(2)   =  SPACE
                       MOVE  "190107990"    TO   NSRY-SRYCD(2)
                       MOVE   1             TO   NSRY-SRYSURYO(2)
                   ELSE
                       IF   NSRY-SRYCD(3)   =  SPACE
                           MOVE  "190107990"   TO   NSRY-SRYCD(3)
                           MOVE   1            TO   NSRY-SRYSURYO(3)
                       END-IF
                   END-IF
      *            明細数・数量計・診療コード計
                   MOVE     ZERO    TO   WRK-MEISAISU
                                         WRK-SURYOTOTAL
                                         WRK-SRYCDTOTAL
                   PERFORM  VARYING   IDX   FROM  1  BY  1
                     UNTIL  (IDX      >     5)    OR
                            (NSRY-SRYCD(IDX)   =   SPACE)
                         ADD    1       TO   WRK-MEISAISU
                         ADD    1       TO   WRK-SURYOTOTAL
                         MOVE    NSRY-SRYCD(IDX)   TO  WRK-SRYCDX
                         ADD     WRK-SRYCD9        TO  WRK-SRYCDTOTAL
                   END-PERFORM
      *            剤点数
                   MOVE     ZERO      TO   WRK-ZAITEN
                   PERFORM  VARYING   IDX   FROM  1  BY  1
                     UNTIL  (IDX      >     5)    OR
                            (NSRY-SRYCD(IDX)   =   SPACE)
                       PERFORM   210-ZAITEN-SET-SEC
                   END-PERFORM
      *            入院診療行為更新
                   MOVE   NYUINACT-REC    TO   MCPDATA-REC
                   MOVE  "tbl_nyuinact"   TO   MCP-TABLE
                   MOVE  "key"            TO   MCP-PATHNAME
                   MOVE  "DBUPDATE"       TO   MCP-FUNC
                   PERFORM   900-ORCDBMAIN-SEC
                   IF   MCP-RC   NOT =  ZERO
                        DISPLAY  "NYUINACT UPDATE MCP-RC = " MCP-RC
                   END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       210-NYUINACT-UPD2-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為編集処理　処理区分３（１００分の２減額）
      *****************************************************************
       210-NYUINACT-UPD3-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT-UPD
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYKA         TO  NSRY-SRYKA
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACT-SELECT-SEC
           IF      FLG-NYUINACT        =   ZERO
      *
               MOVE    "tbl_nyuinact"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-NYUINACT-READ-SEC
               IF  (FLG-NYUINACT        =   ZERO)   AND
                   (NSRY-SRYCD(1)(1:3)  NOT =  "188")
      *
                   PERFORM   210-HYOUKETU-CLEAR-SEC
      *
                   MOVE    1      TO   FLG-NYUINACCT-UPD
                   IF   NSRY-SRYCD(2)   =  SPACE
                       MOVE  "190108690"    TO   NSRY-SRYCD(2)
                       MOVE   1             TO   NSRY-SRYSURYO(2)
                   ELSE
                       IF   NSRY-SRYCD(3)   =  SPACE
                           MOVE  "190108690"   TO   NSRY-SRYCD(3)
                           MOVE   1            TO   NSRY-SRYSURYO(3)
                       END-IF
                   END-IF
      *            明細数・数量計・診療コード計
                   MOVE     ZERO    TO   WRK-MEISAISU
                                         WRK-SURYOTOTAL
                                         WRK-SRYCDTOTAL
                   PERFORM  VARYING   IDX   FROM  1  BY  1
                     UNTIL  (IDX      >     5)    OR
                            (NSRY-SRYCD(IDX)   =   SPACE)
                         ADD    1       TO   WRK-MEISAISU
                         ADD    1       TO   WRK-SURYOTOTAL
                         MOVE    NSRY-SRYCD(IDX)   TO  WRK-SRYCDX
                         ADD     WRK-SRYCD9        TO  WRK-SRYCDTOTAL
                   END-PERFORM
      *            剤点数
                   MOVE     ZERO      TO   WRK-ZAITEN
                   PERFORM  VARYING   IDX   FROM  1  BY  1
                     UNTIL  (IDX      >     5)    OR
                            (NSRY-SRYCD(IDX)   =   SPACE)
                       PERFORM   210-ZAITEN-SET-SEC
                   END-PERFORM
      *            入院診療行為更新
                   MOVE   NYUINACT-REC    TO   MCPDATA-REC
                   MOVE  "tbl_nyuinact"   TO   MCP-TABLE
                   MOVE  "key"            TO   MCP-PATHNAME
                   MOVE  "DBUPDATE"       TO   MCP-FUNC
                   PERFORM   900-ORCDBMAIN-SEC
                   IF   MCP-RC   NOT =  ZERO
                        DISPLAY  "NYUINACT UPDATE MCP-RC = " MCP-RC
                   END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       210-NYUINACT-UPD3-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為編集処理　処理区分４（１００分の３減額）
      *****************************************************************
       210-NYUINACT-UPD4-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT-UPD
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYKA         TO  NSRY-SRYKA
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACT-SELECT-SEC
           IF      FLG-NYUINACT        =   ZERO
      *
               MOVE    "tbl_nyuinact"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-NYUINACT-READ-SEC
               IF  (FLG-NYUINACT        =   ZERO)   AND
                   (NSRY-SRYCD(1)(1:3)  NOT =  "188")
      *
                   PERFORM   210-HYOUKETU-CLEAR-SEC
      *
                   MOVE    1      TO   FLG-NYUINACCT-UPD
                   IF   NSRY-SRYCD(2)   =  SPACE
                       MOVE  "190108590"    TO   NSRY-SRYCD(2)
                       MOVE   1             TO   NSRY-SRYSURYO(2)
                   ELSE
                       IF   NSRY-SRYCD(3)   =  SPACE
                           MOVE  "190108590"   TO   NSRY-SRYCD(3)
                           MOVE   1            TO   NSRY-SRYSURYO(3)
                       END-IF
                   END-IF
      *            明細数・数量計・診療コード計
                   MOVE     ZERO    TO   WRK-MEISAISU
                                         WRK-SURYOTOTAL
                                         WRK-SRYCDTOTAL
                   PERFORM  VARYING   IDX   FROM  1  BY  1
                     UNTIL  (IDX      >     5)    OR
                            (NSRY-SRYCD(IDX)   =   SPACE)
                         ADD    1       TO   WRK-MEISAISU
                         ADD    1       TO   WRK-SURYOTOTAL
                         MOVE    NSRY-SRYCD(IDX)   TO  WRK-SRYCDX
                         ADD     WRK-SRYCD9        TO  WRK-SRYCDTOTAL
                   END-PERFORM
      *            剤点数
                   MOVE     ZERO      TO   WRK-ZAITEN
                   PERFORM  VARYING   IDX   FROM  1  BY  1
                     UNTIL  (IDX      >     5)    OR
                            (NSRY-SRYCD(IDX)   =   SPACE)
                       PERFORM   210-ZAITEN-SET-SEC
                   END-PERFORM
      *            入院診療行為更新
                   MOVE   NYUINACT-REC    TO   MCPDATA-REC
                   MOVE  "tbl_nyuinact"   TO   MCP-TABLE
                   MOVE  "key"            TO   MCP-PATHNAME
                   MOVE  "DBUPDATE"       TO   MCP-FUNC
                   PERFORM   900-ORCDBMAIN-SEC
                   IF   MCP-RC   NOT =  ZERO
                        DISPLAY  "NYUINACT UPDATE MCP-RC = " MCP-RC
                   END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       210-NYUINACT-UPD4-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為編集処理　処理区分５（標欠の取消し）
      *****************************************************************
       210-NYUINACT-UPD5-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT-UPD
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYKA         TO  NSRY-SRYKA
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACT-SELECT-SEC
           IF      FLG-NYUINACT        =   ZERO
      *
               MOVE    "tbl_nyuinact"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-NYUINACT-READ-SEC
               IF   FLG-NYUINACT     =     ZERO
      *
                   PERFORM   210-HYOUKETU-CLEAR-SEC
      *
                   MOVE    1      TO   FLG-NYUINACCT-UPD
      *            明細数・数量計・診療コード計
                   MOVE     ZERO    TO   WRK-MEISAISU
                                         WRK-SURYOTOTAL
                                         WRK-SRYCDTOTAL
                   PERFORM  VARYING   IDX   FROM  1  BY  1
                     UNTIL  (IDX      >     5)    OR
                            (NSRY-SRYCD(IDX)   =   SPACE)
                     ADD    1       TO   WRK-MEISAISU
                     ADD    1       TO   WRK-SURYOTOTAL
                     MOVE    NSRY-SRYCD(IDX)   TO  WRK-SRYCDX
                     ADD     WRK-SRYCD9        TO  WRK-SRYCDTOTAL
                   END-PERFORM
      *            剤点数
                   MOVE     ZERO      TO   WRK-ZAITEN
                   PERFORM  VARYING   IDX   FROM  1  BY  1
                     UNTIL  (IDX      >     5)    OR
                            (NSRY-SRYCD(IDX)   =   SPACE)
                       PERFORM   210-ZAITEN-SET-SEC
                   END-PERFORM
      *            入院診療行為更新
                   MOVE   NYUINACT-REC    TO   MCPDATA-REC
                   MOVE  "tbl_nyuinact"   TO   MCP-TABLE
                   MOVE  "key"            TO   MCP-PATHNAME
                   MOVE  "DBUPDATE"       TO   MCP-FUNC
                   PERFORM   900-ORCDBMAIN-SEC
                   IF   MCP-RC   NOT =  ZERO
                       DISPLAY  "NYUINACT UPDATE MCP-RC = " MCP-RC
                   END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_nyuinact"     TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       210-NYUINACT-UPD5-EXT.
           EXIT.
      *
      *****************************************************************
      *    標欠のクリア処理
      *****************************************************************
       210-HYOUKETU-CLEAR-SEC          SECTION.
      *
           IF   (NSRY-SRYCD(2)   =  "190107890")   OR
                (NSRY-SRYCD(2)   =  "190107990")   OR
                (NSRY-SRYCD(2)   =  "190108590")   OR
                (NSRY-SRYCD(2)   =  "190108690")
               MOVE   SPACE      TO  NSRY-SRYCD(2)
               MOVE   ZERO       TO  NSRY-SRYSURYO(2)
           END-IF
      *
           IF   (NSRY-SRYCD(3)   =  "190107890")   OR
                (NSRY-SRYCD(3)   =  "190107990")   OR
                (NSRY-SRYCD(3)   =  "190108590")   OR
                (NSRY-SRYCD(3)   =  "190108690")
               MOVE   SPACE      TO  NSRY-SRYCD(3)
               MOVE   ZERO       TO  NSRY-SRYSURYO(3)
           END-IF
      *
           .
       210-HYOUKETU-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計編集処理
      *****************************************************************
       210-NYUINACCT-UPD-SEC          SECTION.
      *
           MOVE    WRK-ZAITEN      TO   NACCT-ZAITEN
                                        NACCT-SYUTEN1
           MOVE    WRK-SRYCDTOTAL  TO   NACCT-SRYCDTOTAL
           MOVE    WRK-MEISAISU    TO   NACCT-MEISAISU
           MOVE    WRK-SURYOTOTAL  TO   NACCT-SURYOUTOTAL
      *    入院会計更新
           MOVE    NYUINACCT-REC   TO   MCPDATA-REC
           MOVE   "tbl_nyuinacct"  TO   MCP-TABLE
           MOVE   "key"            TO   MCP-PATHNAME
           MOVE   "DBUPDATE"       TO   MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
           IF   MCP-RC   NOT =  ZERO
               DISPLAY  "NYUINACCT UPDATE MCP-RC = " MCP-RC
           END-IF
      *
           .
       2001-NYUINACCT-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤点セット処理
      *****************************************************************
       210-ZAITEN-SET-SEC          SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE   NSRY-HOSPNUM         TO  TNS-HOSPNUM
           MOVE   NSRY-SRYCD(IDX)      TO  TNS-SRYCD
           MOVE   WRK-PARA-SRYYM       TO  TNS-YUKOSTYMD(1:6)
           MOVE   "01"                 TO  TNS-YUKOSTYMD(7:2)
           MOVE   TNS-YUKOSTYMD        TO  TNS-YUKOEDYMD
           PERFORM 910-TENSU-READ-SEC
      *
      *    点数算定
           EVALUATE    TNS-TENSIKIBETU
      *        点数（プラス）
               WHEN    3
                   ADD   TNS-TEN     TO  WRK-ZAITEN
      *        ％加算
               WHEN    5
                   COMPUTE    WRK-ZAITEN        =
                              WRK-ZAITEN        +
                            ((WRK-ZAITEN  * (TNS-TEN   /  100))  + 0.5)
      *        ％減算
               WHEN    6
                   COMPUTE    WRK-ZAITEN  =
                              WRK-ZAITEN  -
                            ((WRK-ZAITEN * (TNS-TEN   /  100))  -  0.5)
      *        点数（マイナス）
               WHEN    8
                   COMPUTE  WRK-ZAITEN  =
                            WRK-ZAITEN  -  TNS-TEN
           END-EVALUATE
      *
           MOVE    "tbl_tensu"    TO  MCP-TABLE
           MOVE    "key"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *
           .
       2001-ZAITEN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC              SECTION.
      *
           OPEN    OUTPUT      RECEERR-FILE
      *
           MOVE    WRK-RECEERR     TO  RECEERR-REC
           WRITE   RECEERR-REC
           CLOSE   RECEERR-FILE
      *
      *    ジョブ管理終了処理
           MOVE    "JBE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-RECEERR     TO  JOB-YOBI
           MOVE    "9999"          TO  JOB-ERRCD
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           STOP RUN
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスター検索
      *****************************************************************
       910-SYSKANRI-SELECT-SEC        SECTION.
      *
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスター読込
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                      SYS-1001-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター検索
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター読込
      *****************************************************************
       910-NYUINACCT-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  NYUINACCT-REC
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                     NYUINACCT-REC
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    入院診療行為マスター検索
      *****************************************************************
       910-NYUINACT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACT
           ELSE
               MOVE    1                  TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為マスター読込
      *****************************************************************
       910-NYUINACT-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  NYUINACT-REC
               MOVE    ZERO               TO  FLG-NYUINACT
           ELSE
               INITIALIZE                     NYUINACT-REC
               MOVE    1                  TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           .
      *
       910-TENSU-READ-EXT.
           EXIT.
      *
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
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
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
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
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
