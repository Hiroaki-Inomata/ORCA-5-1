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
       PROGRAM-ID.             ORCDBMAIN.
      *****************************************************************
      *  システム名        : ORCA
      *  サブシステム名    : 
      *  コンポーネント名  : ORCA DB アクセスメインモジュール
      *  返却エラーコード  : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/01/23    NACL-竹田     新規作成
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
       
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX3                PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD              PIC X(08).
           03  WRK-MCP-RC              PIC ---9.
           03  WRK-STR                 PIC X(500).
           03  WRK-ERRMSG              PIC X(500).
           03  WRK-PATHNAME            PIC X(64).
           03  WRK-FUNC                PIC X(64).
           03  WRK-TABLE               PIC X(64).
           03  WRK-MIDDLEWARE-NAME     PIC X(64).
      *
       01  CONST-AREA.
           03 CONST-MAX-LEN            PIC 9(08)   VALUE 50000.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *    共通領域
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                    DIVISION    USING
           MCPAREA
           MCPDATA-REC
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
      *
           MOVE    MCP-TABLE           TO  WRK-TABLE
           MOVE    MCP-PATHNAME        TO  WRK-PATHNAME
           MOVE    MCP-FUNC            TO  WRK-FUNC
      *
           EVALUATE    TRUE
           WHEN  ( MCP-FUNC            =   "XMLOPEN"
                                    OR     "XMLCLOSE" )
               CONTINUE
           WHEN    OTHER
               PERFORM 100-INIT-SEC
               PERFORM 200-MAIN-SEC
           END-EVALUATE
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           MOVE    WRK-FUNC            TO  MCP-FUNC
           MOVE    ZERO                TO  MCP-LOGFLAG
      *
           .
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( MCP-TABLE           =   "metadb"
                                        OR "blob" )
               CONTINUE
           ELSE
      *
               EVALUATE    MCP-FUNC
               WHEN    "DBOPEN"
               WHEN    "DBDISCONNECT"
               WHEN    "DBSTART"
               WHEN    "DBCOMMIT"
      *!!!!!   WHEN    "PUTWINDOW"
                   MOVE    LOW-VALUE       TO  MCP-TABLE
                                               MCP-PATHNAME
      *!!!!!!!!!!!!!!!
               WHEN    "PUTWINDOW"
                   MOVE    LOW-VALUE       TO  MCP-TABLE
                                               MCP-PATHNAME
                   IF     (MCP-PUTTYPE     =   "CHANGE"  OR
                                               "JOIN"    )
                     AND  (MCP-WINDOW      =   "M01"    )
                       IF        SPA-BEDFLG     >   ZERO
                           MOVE    "M01N"          TO  MCP-WINDOW
                       END-IF
                   END-IF
      *!!!!!!!!!!!!!!!
               WHEN    OTHER
                   IF  MCP-FUNC            =   "DBFETCH"
                       MOVE    LOW-VALUE   TO  MCPDATA-REC
                   END-IF
                   IF    ( SPA-NOCHK-HOSPNUM   =   "1" )
                       CONTINUE
                   ELSE
                       IF    ( SPA-HOSPNUM     =   ZERO )
                        OR   ( SPA-HOSPNUM     IS  NOT NUMERIC )
      *
                           MOVE   1            TO  FLG-ERR
                       END-IF
                   END-IF
               END-EVALUATE
           END-IF
      *
           IF    ( FLG-ERR         =   ZERO )
      *
               MOVE    2           TO  MCP-VERSION
               MOVE    1           TO  MCP-LIMIT
               MOVE    1           TO  MCP-REDIRECT
      *
               IF    ( MCP-TABLE   =   "tbl_srysrh"
                                    OR "tbl_skysrh"
                                    OR "tbl_rrksrh"
                                    OR "tbl_ptsrh"
                                                    )
                   MOVE    0       TO  MCP-REDIRECT
               END-IF
      *
               IF    ( MCP-TABLE   =   "tbl_chksnd"
                                    OR "tbl_sryacct_main"
                                    OR "tbl_sryacct_sub" 
                                    OR "tbl_wksryact" 
                                                         )
                   IF    ( MCP-PATHNAME (1:3)  =   "tmp" )
                       MOVE    0   TO  MCP-REDIRECT
                   END-IF
               END-IF
      *
               IF    ( MCP-TABLE   =   "tbl_jobkanri" )
                   MOVE    MCPDATA-REC TO  JOBKANRI-REC
                   IF    ( JOB-SHELLID =   "PRGMNT" )
                       MOVE    0   TO  MCP-REDIRECT
                   END-IF
               END-IF
      *
               IF    ( MCP-TABLE   =   "tbl_prtdata" )
                   IF    ( MCP-PATHNAME (1:5)  =   "delmb" )
                       MOVE    0   TO  MCP-REDIRECT
                   END-IF
               END-IF
      *
               IF    ( MCP-TABLE   =   "tbl_file_mis" )
                   IF    ( MCP-PATHNAME (1:11) =   "del_monblob" )
                       MOVE    0   TO  MCP-REDIRECT
                   END-IF
               END-IF
      *
               IF    ( MCP-TABLE   =   "tbl_syumei" )
                   MOVE    MCPDATA-REC
                                   TO  SYUMEI-REC
                   IF    ( MCP-FUNC    =   "DBINSERT" )
                     IF  ( SMEI-DENPEDANUM   <   1 )
                       OR( SMEI-DENPEDANUM   >   99 )
                       MOVE    "abort"
                                   TO  MCP-PATHNAME
                     END-IF
                   END-IF
               END-IF
      *
               EVALUATE    MCP-TABLE
               WHEN    "tbl_sryacct"
                   CALL    "ORCDBACCT" USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
               WHEN    "tbl_seikyu"
                   CALL    "ORCDBSEIKYU" USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
               WHEN    "tbl_syunou"
                   CALL    "ORCDBSYUNOU" USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
               WHEN    OTHER
      *
                   CALL    "MONFUNC"   USING
                                       MCPAREA
                                       MCPDATA-REC
               END-EVALUATE
      *
               IF    ( MCP-RC      =   ZERO OR 1)
                   CONTINUE
               ELSE
                   PERFORM 800-EDIT-ERRMSG-SEC
               END-IF
      *
           ELSE
               MOVE    51          TO  MCP-RC
               PERFORM 800-EDIT-ERRMSG-SEC
           END-IF
      *
           .
      *
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           MOVE    ZERO                TO  MCP-LOGFLAG
           IF      MCP-FUNC        =  "DBINSERT" OR "DBUPDATE" 
                                   OR "DBDELETE"
               PERFORM 1001-AUDITLOG-PUT-SEC
           END-IF
      *
           .
      *
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    監視ログ出力指示処理
      *****************************************************************
       1001-AUDITLOG-PUT-SEC             SECTION.
      *
      *    IF      SPA-TERMID      =   SPACE
      *        MOVE   "batch"      TO  MCP-LOGCOMMENT
      *    END-IF
      *
      *    EVALUATE    MCP-TABLE
      *    WHEN    "tbl_lock"
      *    WHEN    "tbl_ptinf"
      *    WHEN    "tbl_jobkanri"
      *        MOVE    1           TO  MCP-LOGFLAG
      *    END-EVALUATE
           .
      *
       1001-AUDITLOG-PUT-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       800-EDIT-ERRMSG-SEC             SECTION.
      *
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   SPACE            TO  WRK-ERRMSG
           MOVE   MCP-RC           TO  WRK-MCP-RC
      *
           STRING "ORCDBMAIN ERROR RC[" DELIMITED   BY  SIZE
                  WRK-MCP-RC           DELIMITED   BY  SIZE
                  "] HOSPNUM["         DELIMITED   BY  SIZE
                  SPA-HOSPNUM          DELIMITED   BY  SIZE
                  "] FUNC["            DELIMITED   BY  SIZE
                  MCP-FUNC             DELIMITED   BY  SPACE
                  "] TABLE["           DELIMITED   BY  SIZE
                  MCP-TABLE            DELIMITED   BY  SPACE
                  "] PATHNAME["        DELIMITED   BY  SIZE
                  MCP-PATHNAME         DELIMITED   BY  SPACE
                  "] SPA["             DELIMITED   BY  SIZE
                  SPA-STAFFCD          DELIMITED   BY  SIZE
                  SPA-SYSYMD           DELIMITED   BY  SIZE
                  SPA-SYSYMDW          DELIMITED   BY  SIZE
                  SPA-SYSYMDWH         DELIMITED   BY  SIZE
                  SPA-SYSYMD-YOBI      DELIMITED   BY  SIZE
                  SPA-MACHIPG          DELIMITED   BY  SIZE
                  SPA-MOTOPG           DELIMITED   BY  SIZE
                  SPA-MOTOPG2          DELIMITED   BY  SIZE
                  "]"                  DELIMITED   BY  SIZE
                  LOW-VALUE            DELIMITED   BY  SIZE
           INTO   WRK-ERRMSG
           END-STRING
      *
           ACCEPT  WRK-MIDDLEWARE-NAME
                                 FROM ENVIRONMENT  "MCP_MIDDLEWARE_NAME"
      *
           IF    ( WRK-MIDDLEWARE-NAME =   "ginbee" )
               DISPLAY SMCNDATE-YMD(1:4) "-"
                       SMCNDATE-YMD(5:2) "-"
                       SMCNDATE-YMD(7:2) " "
                       SMCNDATE-HMS(1:2) ":"
                       SMCNDATE-HMS(3:2) ":"
                       SMCNDATE-HMS(5:2) " "
                       WRK-ERRMSG
           ELSE
               CALL    "coblog"    USING   WRK-ERRMSG
           END-IF
      *
           IF    ( MCP-FUNC    =   "DBINSERT"
                                OR "DBUPDATE" )
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   CONST-MAX-LEN )
                         OR   ( MCPDATA-REC (IDX1 : )   =   SPACE )
                         OR   ( MCPDATA-REC (IDX1 : 1)  < X"20" )
                         OR   ( MCPDATA-REC (IDX1 : 1)  > X"7E" )
      *
                  CONTINUE
      *
               END-PERFORM
      *
               IF    ( IDX1         >   1 )
                   COMPUTE IDX1  =  IDX1  -    1
               END-IF
      *
      *-----------------------------------------------------------------
               DISPLAY "ORCDBMAIN MCPDATA-REC(1:" IDX1 ")=["
                         MCPDATA-REC (1 : IDX1) "]"
           END-IF
      *
           .
       800-EDIT-ERRMSG-EXT.
           EXIT.
