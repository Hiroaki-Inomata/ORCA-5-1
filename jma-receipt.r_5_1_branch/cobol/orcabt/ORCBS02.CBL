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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCBS02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次統計
      *  コンポーネント名  : 請求書変更ログ編集
      *  管理者            :
      *  作成日付    作業者        記述
      *  03/04/08    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田     07/05/24  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                         DIVISION.
       CONFIGURATION                       SECTION.
       INPUT-OUTPUT                        SECTION.
       FILE-CONTROL.
      *
      *    請求書用パラメタファイル
           SELECT  BS01-FILE   ASSIGN      PRFNAME
                                   ORGANIZATION    IS  SEQUENTIAL.
      *
       DATA                                DIVISION.
       FILE                                SECTION.
      *
      *    パラメータファイル
       FD  BS01-FILE.
       01  BS01-REC.
           COPY    "CPPARA.INC"    REPLACING //PARA-//
                                   BY        //BS01-//.
      *
       WORKING-STORAGE                     SECTION.
      *
      *    パラメータファイル名
           COPY    "CPASGNPARA.INC"
                                   REPLACING  //ASGNPARA//
                                   BY         //PRFNAME//.
      *
      *    フラグ
       01  FLG-AREA.
           03  FLG-BS01                        PIC 9(01).
      *
      *    インデックス
       01  IDX-AREA.
           03  IDX0                            PIC 9(05).
           03  IDX1                            PIC 9(05).
           03  IDXP                            PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-MEISAI                      PIC X(80).
           03  WRK-STR                         PIC X(10000).
           03  WRK-YMD                         PIC X(08).
           03  WRK-HMS                         PIC X(06).
           03  WRK-SEIKYUZ                     PIC ---,---,--9.
           03  WRK-SEIKYU-TAX-SAIZ             PIC ---,---,--9.
           03  WRK-MISYUZ                      PIC ---,---,--9.
           03  WRK-GOKEIZ                      PIC ---,---,--9.
           03  WRK-NYUKINZ                     PIC ---,---,--9.
           03  WRK-GOKEI                       PIC S9(10).
      *
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
           COPY    "CPSYUPARA03.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
       01  COMMAND-PARAM       PIC X(400).
      *
      ****************************************************************
       PROCEDURE           DIVISION
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
                                       IDX-AREA
                                       WRK-AREA
                                       PRFNAME
                                       SPA-AREA
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                               INTO    SPA-HOSPNUM
                                       SPA-PTNUM
                                       SPA-OPID
                                       PRFNAME
                                       WRK-YMD
                                       WRK-HMS
           END-UNSTRING
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    PRFNAME         TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME
                                   TO  PRFNAME
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           OPEN    INPUT   BS01-FILE
      *
           PERFORM 900-BS01-READ-SEC
      *
           MOVE    ZERO                TO  IDX1
           PERFORM UNTIL ( FLG-BS01    NOT =   ZERO )
      *
               COMPUTE IDX1    =   IDX1    +   1
      *
               MOVE    BS01-DATA-REC       TO  SYUPARA03-AREA
               MOVE    SYUPARA03-SYU-DATA  TO  SYUNOU-REC
      *
               IF    ( IDX1    =   1 )
                   MOVE    ALL "="     TO  WRK-MEISAI
                   DISPLAY WRK-MEISAI
                   DISPLAY WRK-YMD  " " WRK-HMS " " BS01-TERMID
                   DISPLAY "ユーザ名:"  SPA-OPID
                   IF    ( SYUPARA03-HAKKOFLG  =   4 )
                       DISPLAY "月別請求書兼領収書変更"
                       DISPLAY "患者番号：" SPA-PTNUM
                       DISPLAY "診療年月：" SYU-SRYYMD(1:6)
                   ELSE
                       DISPLAY "請求書兼領収書変更"
                       DISPLAY "患者番号：" SPA-PTNUM
                       DISPLAY "診療年月日：" SYU-SRYYMD
                   END-IF
               END-IF
      *
               PERFORM 2001-EDIT-SEC
      *
               PERFORM 900-BS01-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   BS01-FILE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    編集処理
      *****************************************************************
       2001-EDIT-SEC                   SECTION.
      *
           MOVE    ALL "-"         TO  WRK-MEISAI
           DISPLAY WRK-MEISAI
           DISPLAY "診療科："  SYUPARA03-SRYKAMEI
           MOVE    SPACE           TO  WRK-STR
           MOVE    1               TO  IDXP
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SYUPARA03-SRYKA-MAX )
               IF    ( IDX0        >   1 )
                   STRING  ","         DELIMITED BY SIZE
                   INTO    WRK-STR
                   WITH POINTER    IDXP
                   END-STRING
               END-IF
               STRING  SYUPARA03-SRYKA (IDX0)
                                   DELIMITED BY SIZE
               INTO    WRK-STR
               WITH POINTER    IDXP
               END-STRING
           END-PERFORM
           DISPLAY WRK-STR(1:IDXP)
      *
           DISPLAY "保険種類："  SYUPARA03-HKNCOMBIMEI
           MOVE    SPACE           TO  WRK-STR
           MOVE    1               TO  IDXP
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SYUPARA03-HKNCOMBI-MAX )
               IF    ( IDX0        >   1 )
                   STRING  ","         DELIMITED BY SIZE
                   INTO    WRK-STR
                   WITH POINTER    IDXP
                   END-STRING
               END-IF
               STRING  SYUPARA03-HKNCOMBI (IDX0)
                                   DELIMITED BY SIZE
               INTO    WRK-STR
               WITH POINTER    IDXP
               END-STRING
           END-PERFORM
           DISPLAY WRK-STR(1:IDXP)
      *
           MOVE    SPACE           TO  WRK-STR
           MOVE    1               TO  IDXP
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SYUPARA03-GRP-DENPNUM-MAX )
               IF    ( IDX0        >   1 )
                   STRING  ","         DELIMITED BY SIZE
                   INTO    WRK-STR
                   WITH POINTER    IDXP
                   END-STRING
               END-IF
               STRING  SYUPARA03-GRP-DENPNUM (IDX0)
                                   DELIMITED BY SIZE
               INTO    WRK-STR
               WITH POINTER    IDXP
               END-STRING
           END-PERFORM
           DISPLAY "伝票番号：" WRK-STR(1:IDXP)
      *
           MOVE    SYUPARA03-ORG-SEIKYU    TO  WRK-SEIKYUZ
           MOVE    SYUPARA03-ORG-SEIKYU-TAX-SAI
                                           TO  WRK-SEIKYU-TAX-SAIZ
           MOVE    SYUPARA03-ORG-MISYU     TO  WRK-MISYUZ
           COMPUTE WRK-GOKEI
               =   SYUPARA03-ORG-SEIKYU
               +   SYUPARA03-ORG-MISYU
           MOVE    WRK-GOKEI               TO  WRK-GOKEIZ
           MOVE    SYUPARA03-ORG-NYUKIN    TO  WRK-NYUKINZ
           DISPLAY "変更前："
                   "今回請求額［"     WRK-SEIKYUZ " ］"
                   "消費税（再掲）［" WRK-SEIKYU-TAX-SAIZ " ］"
                   "前回請求額［"     WRK-MISYUZ " ］"
                   "合計請求額［"     WRK-GOKEIZ " ］"
                   "今回入金額［"     WRK-NYUKINZ " ］"
      *
           MOVE    SYUPARA03-SEIKYU        TO  WRK-SEIKYUZ
           MOVE    SYUPARA03-SEIKYU-TAX-SAI
                                           TO  WRK-SEIKYU-TAX-SAIZ
           MOVE    SYUPARA03-MISYU         TO  WRK-MISYUZ
           COMPUTE WRK-GOKEI
               =   SYUPARA03-SEIKYU
               +   SYUPARA03-MISYU
           MOVE    WRK-GOKEI               TO  WRK-GOKEIZ
           MOVE    SYUPARA03-NYUKIN        TO  WRK-NYUKINZ
           DISPLAY "変更後："
                   "今回請求額［"     WRK-SEIKYUZ " ］"
                   "消費税（再掲）［" WRK-SEIKYU-TAX-SAIZ " ］"
                   "前回請求額［"     WRK-MISYUZ " ］"
                   "合計請求額［"     WRK-GOKEIZ " ］"
                   "今回入金額［"     WRK-NYUKINZ " ］"
      *
           .
       2001-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求書用パラメタファイル読込処理
      *****************************************************************
       900-BS01-READ-SEC               SECTION.
      *
           MOVE    ZERO            TO  FLG-BS01
      *
           READ    BS01-FILE
           AT  END
               MOVE    1           TO  FLG-BS01
           END-READ
      *
           .
       900-BS01-READ-EXT.
           EXIT.
      *
