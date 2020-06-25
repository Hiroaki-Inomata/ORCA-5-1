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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORAPI000R1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : システム情報取得
      *  管理者            :
      *  作成日付    作業者        記述
      *  13/11/14    NACL−太田　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
      *    プログラム更新　管理ファイル
           SELECT  IN01-FILE       ASSIGN
                   "/usr/lib/jma-receipt/patch-lib/patch-program.prf"
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-IN01.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    プログラム更新　管理ファイル
       FD  IN01-FILE.
       01  IN01-REC.
           03  IN01-TIMES          PIC 9(04).
           03  FILLER              PIC X(01).
           03  IN01-UPDATEFLG      PIC X(01).
           03  FILLER              PIC X(01).
           03  IN01-RELEASEYMD     PIC X(08).
           03  FILLER              PIC X(01).
           03  IN01-COMMENT        PIC X(100).
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  STS-AREA.
           03  STS-IN01                PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-IN01                PIC 9(01).
           03  FLG-MSTKANRI            PIC 9(01).
           03  FLG-DBKANRI             PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDXM                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXB                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ERRCD               PIC X(04).
           03  WRK-ERRMSG              PIC X(100).
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
           03  WRK-SRYYMD.
               05  WRK-SRYYMD-YM       PIC X(06).
               05  WRK-SRYYMD-DD       PIC 9(02).
           03  WRK-NUM-REQUEST         PIC 9(12).
           03  WRK-NUM-SERVER          PIC 9(12).
           03  WRK-NUM-YMD-G.
               05  WRK-NUM-YMD         PIC 9(08).
           03  WRK-NUM-HMS-G.
               05  WRK-NUM-HMS-HH      PIC 9(02).
               05  WRK-NUM-HMS-MM      PIC 9(02).
               05  WRK-NUM-HMS-SS      PIC 9(02).
           03  WRK-NUM-YMDHMS-G.
               05  WRK-NUM-YMDHMS      PIC 9(12).
           03  WRK-DELAY               PIC 9(12).
           03  WRK-LASTYMD-MST         PIC X(08).
           03  WRK-LASTYMD-MLI         PIC X(08).
      *
       01  PGUP-AREA.
           03  PGUP-OCC                OCCURS  100.
               05  PGUP-INUSE          PIC 9(01).
               05  PGUP-TIMES          PIC 9(04).
               05  PGUP-UPDATEFLG      PIC X(01).
               05  PGUP-RELEASEYMD     PIC X(08).
               05  PGUP-COMMENT        PIC X(100).
      *
       01  KEY-AREA.
           03  KEY-REQYMD              PIC X(08).
           03  KEY-REQHMS              PIC X(06).
      *
       01  CONST-AREA.
           03  CONST-PGUP-MAX          PIC 9(08)   VALUE 10.
           03  CONST-MNTJOBID          PIC 9(07)  VALUE    1.
           03  CONST-MNTSHELLID        PIC X(08)  VALUE    "MSTMNT".
           03  CONST-MLIJOBID          PIC 9(07)  VALUE    1.
           03  CONST-MLISHELLID        PIC X(08)  VALUE    "MSTMLI".
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
      *    API XML読み込み用定義体
           COPY    "CPSYSINFV2REQ.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPSYSINFV2RES.INC".
      *
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSK1010.INC".
      *
      *    ジョブ管理
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ＤＢ管理
       01  DBKANRI-REC.
           COPY    "CPDBKANRI.INC".
      *
      *    マスタ管理
       01  MSTKANRI-REC.
           COPY    "CPMSTKANRI.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
       01  SCRAREA.
           COPY    "API01RV2SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "***************"
           DISPLAY   "* accept start*"
           DISPLAY   "***************"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  KEY-AREA
           INITIALIZE                  SPA-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( WRK-ERRCD          =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           DISPLAY   "***************"
           DISPLAY   "* accept end  *"
           DISPLAY   "***************"
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
           INITIALIZE                  XML-SYSINFRES
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
      *    医療機関識別番号セット
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE   "0099"       TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                           SYS-1010-REC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               MOVE   "0089"       TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    XML情報取り出し
           PERFORM 900-XML-READ-SEC
           IF    ( WRK-ERRCD           NOT =   SPACE )
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           PERFORM 1001-INIT-CHECK-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    電文項目チェック処理
      *****************************************************************
       1001-INIT-CHECK-SEC             SECTION.
      *
           PERFORM 10011-REQYMD-CHECK-SEC
      *
           PERFORM 10011-REQTIME-CHECK-SEC
      *
           .
       1001-INIT-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    リクエスト日付チェック処理
      *****************************************************************
       10011-REQYMD-CHECK-SEC          SECTION.
      *
           IF    ( SYSINFREQ-REQDATE    =   SPACE )
                   MOVE    "0001"      TO  WRK-ERRCD
                   PERFORM 990-EXIT-PROGRAM-SEC
           ELSE
               MOVE    SYSINFREQ-REQDATE   TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0001"      TO  WRK-ERRCD
                   PERFORM 990-EXIT-PROGRAM-SEC
               END-IF
           END-IF
      *
           MOVE    WRK-SYMD            TO  KEY-REQYMD
      *
           .
       10011-REQYMD-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    リクエスト時刻チェック処理
      *****************************************************************
       10011-REQTIME-CHECK-SEC         SECTION.
      *
           IF    ( SYSINFREQ-REQTIME (1:2)     IS  NUMERIC )
            AND  ( SYSINFREQ-REQTIME (3:1)     =   ":" )
            AND  ( SYSINFREQ-REQTIME (4:2)     IS  NUMERIC )
            AND  ( SYSINFREQ-REQTIME (6:1)     =   ":" )
            AND  ( SYSINFREQ-REQTIME (7:2)     IS  NUMERIC )
                MOVE    SYSINFREQ-REQTIME (1:2)    TO  KEY-REQHMS (1:2)
                MOVE    SYSINFREQ-REQTIME (4:2)    TO  KEY-REQHMS (3:2)
                MOVE    SYSINFREQ-REQTIME (7:2)    TO  KEY-REQHMS (5:2)
           ELSE
               MOVE    "0002"          TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       10011-REQTIME-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-DBVERSION-GET-SEC
      *
           PERFORM 2001-MSTVERSION-GET-SEC
      *
           PERFORM 2001-PGUP-GET-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ管理情報取得処理
      *****************************************************************
       2001-DBVERSION-GET-SEC              SECTION.
      *
           PERFORM 900-DBKANRI-KEY-SEL-SEC
           IF    ( FLG-DBKANRI     NOT =   ZERO )
               MOVE    "0003"      TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           MOVE    DBKANRI-VERSION     TO  SYSINFRES-JMA-VERSION
           MOVE    DBKANRI-DBSVERSION1 TO  SYSINFRES-DB-LOCAL
           MOVE    DBKANRI-DBSVERSION2 TO  SYSINFRES-DB-NEW
      *
           .
       2001-DBVERSION-GET-EXT.
           EXIT.
      *****************************************************************
      *    マスタ管理情報編集処理
      *****************************************************************
       2001-MSTVERSION-GET-SEC             SECTION.
      *
           MOVE    "00000000"      TO  WRK-LASTYMD-MST
                                       WRK-LASTYMD-MLI
      *
      *    ＪＯＢ管理 
           INITIALIZE                  ORCSJOBKANRIAREA
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CONST-MNTJOBID  TO  JOB-JOBID
           MOVE    CONST-MNTSHELLID TO JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           IF    ( SJOBKANRI-RETURN    =   ZERO )
               MOVE    JOB-STARTYMD TO WRK-LASTYMD-MST
           END-IF
      *
      *    ＪＯＢ管理 
           INITIALIZE                  ORCSJOBKANRIAREA
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CONST-MLIJOBID  TO  JOB-JOBID
           MOVE    CONST-MLISHELLID TO JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           IF    ( SJOBKANRI-RETURN    =   ZERO )
               MOVE    JOB-STARTYMD TO WRK-LASTYMD-MLI
           END-IF
      *
           MOVE    WRK-LASTYMD-MST  TO WRK-SYMD
           IF    ( WRK-LASTYMD-MLI  >  WRK-SYMD )
               MOVE    WRK-LASTYMD-MLI
                                    TO WRK-SYMD
           END-IF
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE         TO SYSINFRES-LAST-UPDATE
      *
           PERFORM 900-MSTKANRI-ALL-SEL-SEC
           IF    ( FLG-MSTKANRI    NOT =   ZERO )
               MOVE    "0004"      TO  WRK-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           MOVE    ZERO                TO  IDXM
      *
           PERFORM UNTIL ( FLG-MSTKANRI    NOT =   ZERO )
      *
               COMPUTE IDXM    =   IDXM    +   1
      *
               MOVE    MSTKANRI-DBRVERSION1
                                       TO  SYSINFRES-MST-LOCAL (IDXM)
               MOVE    MSTKANRI-DBRVERSION2
                                       TO  SYSINFRES-MST-NEW (IDXM)
      *
               IF    ( MSTKANRI-NAME   NOT =   SPACE )
                   MOVE    MSTKANRI-NAME  TO  SYSINFRES-MST-NAME (IDXM)
               ELSE
                   MOVE    MSTKANRI-KANRICD
                                          TO  SYSINFRES-MST-NAME (IDXM)
               END-IF
      *
               PERFORM 900-MSTKANRI-ALL-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_mstkanri"      TO  MCP-TABLE 
           MOVE    "all"               TO  MCP-PATHNAME 
           PERFORM 910-DBCLOSECURSOR-SEC
      *
          .
       2001-MSTVERSION-GET-EXT.
           EXIT.
      *****************************************************************
      *    プログラム更新情報編集処理
      *****************************************************************
       2001-PGUP-GET-SEC               SECTION.
      *
           INITIALIZE                  PGUP-AREA
      *
           OPEN    INPUT   IN01-FILE
           IF    ( STS-IN01    NOT =   ZERO )
               MOVE    1           TO  FLG-IN01
           END-IF
      *
           IF    ( FLG-IN01        =   ZERO )
               PERFORM 900-IN01-READ-SEC
           END-IF
      *
           MOVE    ZERO            TO  IDXA
           PERFORM UNTIL ( FLG-IN01    NOT =   ZERO )
      *
               IF    ( IDXA        >=  CONST-PGUP-MAX )
                   MOVE    ZERO    TO  IDXA
               END-IF
      *
               COMPUTE IDXA    =   IDXA    +   1
               MOVE    1           TO  PGUP-INUSE (IDXA)
      *
               MOVE    IN01-TIMES  TO  PGUP-TIMES (IDXA)
               MOVE    IN01-UPDATEFLG
                                   TO  PGUP-UPDATEFLG (IDXA)
               MOVE    IN01-RELEASEYMD
                                   TO  PGUP-RELEASEYMD(IDXA)
               MOVE    IN01-COMMENT
                                   TO  PGUP-COMMENT (IDXA)
      *
               PERFORM 900-IN01-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   IN01-FILE
      *
           IF    ( IDXA            >   ZERO )
      *
               PERFORM VARYING IDXB    FROM    1   BY  1
                       UNTIL ( IDXB    >   CONST-PGUP-MAX )
                        OR   ( PGUP-INUSE (IDXA)   =   ZERO )
      *
                   PERFORM 20011-PGUP-GET-SEC
      *
                   COMPUTE IDXA    =   IDXA - 1
      *
                   IF    ( IDXA    <   1 )
                       MOVE    CONST-PGUP-MAX
                                   TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
           END-IF
      *
           .
       2001-PGUP-GET-EXT.
           EXIT.
      *****************************************************************
      *    プログラム更新情報編集処理
      *****************************************************************
       20011-PGUP-GET-SEC              SECTION.
      *
           IF    ( PGUP-RELEASEYMD (IDXA)  =   SPACE )
               MOVE    SPACE           TO  SYSINFRES-PG-DATE (IDXB)
           ELSE
               MOVE    PGUP-RELEASEYMD (IDXA)
                                       TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE        TO  SYSINFRES-PG-DATE (IDXB)
           END-IF
      *
           EVALUATE    PGUP-UPDATEFLG (IDXA)
           WHEN    "T"
               MOVE    "済"            TO  SYSINFRES-PG-STATE(IDXB)
           WHEN    "F"
               MOVE    "未処理"        TO  SYSINFRES-PG-STATE(IDXB)
           WHEN    "E"
               MOVE    "エラー"        TO  SYSINFRES-PG-STATE(IDXB)
           END-EVALUATE
      *
      *    内容
           MOVE    PGUP-COMMENT (IDXA) TO  SYSINFRES-PG-COMMENT (IDXB)
      *
           .
      *
       20011-PGUP-GET-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  SYSINFRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  SYSINFRES-INFORMATION-TIME
      *
           IF    ( WRK-ERRCD           =   SPACE )
      *
               MOVE    SMCNDATE-YMD        TO  WRK-NUM-YMD-G
               MOVE    SMCNDATE-HMS        TO  WRK-NUM-HMS-G
               PERFORM 800-NUM-YMDHMS-SEC
               MOVE    WRK-NUM-YMDHMS      TO  WRK-NUM-SERVER
      *
               MOVE    KEY-REQYMD          TO  WRK-NUM-YMD-G
               MOVE    KEY-REQHMS          TO  WRK-NUM-HMS-G
               PERFORM 800-NUM-YMDHMS-SEC
               MOVE    WRK-NUM-YMDHMS      TO  WRK-NUM-REQUEST
      *
               IF    ( WRK-NUM-SERVER      <   WRK-NUM-REQUEST )
                   MOVE    "0005"          TO  WRK-ERRCD
               END-IF
      *
               IF    ( WRK-ERRCD           =   SPACE )
      *
                   COMPUTE WRK-DELAY = 30 * 60
      *
                   IF    ( WRK-NUM-SERVER  >=  WRK-NUM-REQUEST
                                                +  WRK-DELAY )
                       MOVE    "0006"  TO  WRK-ERRCD
                   END-IF
               END-IF
      *
           END-IF
      *
           IF    ( WRK-ERRCD           =   SPACE )
               MOVE    "0000"          TO  WRK-ERRCD
           END-IF
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    WRK-ERRCD           TO  SYSINFRES-API-RESULT
           MOVE    WRK-ERRMSG          TO  SYSINFRES-API-RESULT-MSG
      *
           PERFORM 900-XML-WRITE-SEC
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
      *    日付・時刻数値変換処理
      *****************************************************************
       800-NUM-YMDHMS-SEC              SECTION.
      *
           MOVE    FUNCTION INTEGER-OF-DATE(WRK-NUM-YMD)
                                   TO  WRK-NUM-YMDHMS
      *
           COMPUTE WRK-NUM-YMDHMS
               =   WRK-NUM-YMDHMS  *   24 * 60 * 60
               +   WRK-NUM-HMS-HH  *   60 * 60
               +   WRK-NUM-HMS-MM  *   60
               +   WRK-NUM-HMS-SS
      *
           .
       800-NUM-YMDHMS-EXT.
            EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
           WHEN    "0000"
               MOVE    "処理終了"      TO  WRK-ERRMSG
           WHEN    "0001"
               MOVE    "リクエスト日付の設定に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "0002"
               MOVE    "リクエスト時間の設定に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "0003"
               MOVE    "ＤＢ管理情報が取得できません"
                                       TO  WRK-ERRMSG
           WHEN    "0004"
               MOVE    "マスタ更新情報が取得できません"
                                       TO  WRK-ERRMSG
           WHEN    "0005"
               MOVE    "リクエスト時間が受付時間より進んでいます"
                                       TO  WRK-ERRMSG
           WHEN    "0006"
               STRING  "リクエスト時間と受付時間に"
                                           DELIMITED BY SIZE
                       "３０分以上のずれがあります"
                                           DELIMITED BY SIZE
               INTO    WRK-ERRMSG
               END-STRING
      *共通エラー
           WHEN    "0089"
      *        ORCSCOMMON のエラー
               EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "職員情報が取得できません"
                                       TO  WRK-ERRMSG
               WHEN    "1002"
                   MOVE    "医療機関情報が取得できません"
                                       TO  WRK-ERRMSG
               WHEN    "1003"
                   MOVE    "システム日付が取得できません"
                                       TO  WRK-ERRMSG
               WHEN    "1005"
                   MOVE    "患者番号構成情報が取得できません"
                                       TO  WRK-ERRMSG
               WHEN    "1015"
                   STRING  "グループ医療機関が不整合です。"
                                               DELIMITED  BY  SIZE
                           "処理を終了して下さい。"
                                               DELIMITED  BY  SIZE
                   INTO    WRK-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    "システム項目が設定できません"
                                       TO  WRK-ERRMSG
               END-EVALUATE
           WHEN    "0097"
               MOVE   "送信内容に誤りがあります"
                                       TO  WRK-ERRMSG
           WHEN    "0098"
               MOVE   "送信内容の読込ができませんでした"
                                       TO  WRK-ERRMSG
           WHEN    "0099"
               MOVE    "ユーザＩＤが未登録です"
                                       TO  WRK-ERRMSG
           WHEN    OTHER
               MOVE    "返却情報の編集でエラーが発生しました"
                                       TO  WRK-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       900-XML-READ-SEC             SECTION.
      *
           IF    ( APILST20-BODY    NOT =   LOW-VALUE )
               DISPLAY "systeminfv2 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    "xml_systeminfv2req"    TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    APILST20-BODY           TO  APIREQ-OBJECT
           MOVE    ZERO                    TO  APIREQ-MODE
           MOVE    "private_objects"       TO  APIREQ-RECORDNAME
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC              =   ZERO  OR  1 )
               PERFORM 9001-XML-FILTER-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "0098"               TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE               TO  APILST20-BODY
      *
           .
       900-XML-READ-EXT.
           EXIT.
      *****************************************************************
      *    XMLフィルタリング処理
      *****************************************************************
       9001-XML-FILTER-SEC             SECTION.
      *
           MOVE    APIREQ-PATIENTINFOREQ
                                       TO  SYSINFREQ-PRIVATE-OBJECTS
      *
           IF    ( SYSINFREQ-REQDATE   =   LOW-VALUE )
               MOVE    SPACE           TO  SYSINFREQ-REQDATE
           END-IF
      *
           IF    ( SYSINFREQ-REQTIME   =   LOW-VALUE )
               MOVE    SPACE           TO  SYSINFREQ-REQTIME
           END-IF
      *
           .
       9001-XML-FILTER-EXT.
           EXIT.
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       900-XML-WRITE-SEC             SECTION.
      *
           IF    ( APILST20-BODY    NOT =   LOW-VALUE )
               DISPLAY "systeminfv2 object is not low_value"
           END-IF
      *
           MOVE    SYSINFRES-PRIVATE-OBJECTS TO  APIREQ-PATIENTINFOREQ
           MOVE    "xml_systeminfv2res" TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    1                       TO  APIREQ-MODE
           MOVE    "private_objects"       TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT       TO  APILST20-BODY
               MOVE    "application/xml"   TO  APILST20-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       900-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ管理検索処理
      *****************************************************************
       900-DBKANRI-KEY-SEL-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-DBKANRI
      *
           INITIALIZE                      DBKANRI-REC
           MOVE    "ORCADB00"          TO  DBKANRI-KANRICD
           MOVE    DBKANRI-REC         TO  MCPDATA-REC
           MOVE    "tbl_dbkanri"       TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  DBKANRI-REC
           ELSE
               INITIALIZE                  DBKANRI-REC
               MOVE    1               TO  FLG-DBKANRI
           END-IF
      *
           MOVE    "tbl_dbkanri"       TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-DBKANRI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    マスタ管理検索処理
      *****************************************************************
       900-MSTKANRI-ALL-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-MSTKANRI
      *
           INITIALIZE                      MSTKANRI-REC
           MOVE    SPA-HOSPNUM         TO  MSTKANRI-HOSPNUM
           MOVE    MSTKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_mstkanri"      TO  MCP-TABLE 
           MOVE    "all"               TO  MCP-PATHNAME 
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  MSTKANRI-REC
           ELSE
               INITIALIZE                  MSTKANRI-REC
               MOVE    1               TO  FLG-MSTKANRI
           END-IF
      *
           .
       900-MSTKANRI-ALL-SEL-EXT.
           EXIT.
      *****************************************************************
      *    マスタ管理FETCH処理
      *****************************************************************
       900-MSTKANRI-ALL-FET-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-MSTKANRI
      *
           MOVE    "tbl_mstkanri"      TO  MCP-TABLE 
           MOVE    "all"               TO  MCP-PATHNAME 
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  MSTKANRI-REC
           ELSE
               INITIALIZE                  MSTKANRI-REC
               MOVE    1               TO  FLG-MSTKANRI
           END-IF
      *
           .
       900-MSTKANRI-ALL-FET-EXT.
           EXIT.
      *****************************************************************
      *    プログラム更新　管理ファイル読込処理
      *****************************************************************
       900-IN01-READ-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-IN01
      *
           READ    IN01-FILE
           AT  END
               MOVE    1               TO  FLG-IN01
           END-READ
      *
           .
       900-IN01-READ-EXT.
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
      *****************************************************************
      *    ＸＭＬ読込処理
      *****************************************************************
       910-XMLREAD-SEC                 SECTION.
      *
           MOVE    "XMLREAD"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLREAD-EXT.
           EXIT.
      *****************************************************************
      *    ＸＭＬ書込処理
      *****************************************************************
       910-XMLWRITE-SEC                SECTION.
      *
           MOVE    "XMLWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLWRITE-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           IF    ( WRK-ERRCD           =   "0099" )
               MOVE    404             TO  APILST20-HTTPSTATUS
           ELSE
               PERFORM 9901-EXIT-PROGRAM-SEC
           END-IF
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       9901-EXIT-PROGRAM-SEC            SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  SYSINFRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  SYSINFRES-INFORMATION-TIME
      *
           PERFORM 890-ERRCD-MSG-SEC
           MOVE    WRK-ERRCD           TO  SYSINFRES-API-RESULT
           MOVE    WRK-ERRMSG          TO  SYSINFRES-API-RESULT-MSG
      *
           PERFORM 900-XML-WRITE-SEC
      *
           .
       9901-EXIT-PROGRAM-EXT.
           EXIT.
