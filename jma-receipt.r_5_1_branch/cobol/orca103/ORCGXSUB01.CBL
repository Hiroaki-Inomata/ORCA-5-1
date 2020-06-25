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
       PROGRAM-ID.                     ORCGXSUB01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : チェックマスタ情報設定
      *  コンポーネント名  : チェックマスタ情報設定サブ１（病名ＣＤ編集）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/01/28    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-多々納  07/05/08  グループ診療対応
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-BYOMEI                  PIC 9(01).
           03  FLG-USERBYOMEI              PIC 9(01).
           03  FLG-PTBYOMEI                PIC 9(01).
           03  FLG-BYOINS                  PIC 9(01).
           03  FLG-BYOMEIERR               PIC 9(01).
           03  FLG-UBYOERR                 PIC 9(01).
           03  FLG-BYOCDARI                PIC 9(01).
           03  FLG-CHECK                   PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
           03  IDX4                        PIC 9(05).
           03  IDX5                        PIC 9(05).
           03  IDX-BYO                     PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
           03  IDXB                        PIC 9(05).
           03  IDXT                        PIC 9(05).
           03  IDXN                        PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SAVE-G.
               05  WRK-SAVE-MAX            PIC 9(05).
               05  WRK-SAVE-IDX-G.
                   07  WRK-SAVE-OCC        OCCURS  100.
                       09  WRK-SAVE-IDX    PIC 9(05).
                       09  WRK-SAVE-CTRL   PIC X(01).
           03  WRK-CTRL-G.
               05  WRK-CTRL                PIC X(01)
                                           OCCURS  6.
           03  WRK-STR-G.
               05  WRK-STR-MAX             PIC 9(05).
               05  WRK-STR-TMP             PIC X(50).
           03  WRK-RECECD                  PIC X(07).
           03  WRK-RECECDMEI               PIC X(200).
           03  WRK-BYOMEI-G.
               05  WRK-BYOCD               PIC X(20).
               05  WRK-BYOCD-IDX           PIC 9(05).
               05  WRK-BYOCD-LEN-MAX       PIC 9(05).
               05  WRK-BYOCD-LEN           PIC 9(05).
               05  WRK-BYOMEI              PIC X(200).
               05  WRK-BYOMEI-LEN          PIC 9(05).
           03  WRK-USBYOMEI                PIC X(200).
           03  WRK-CMB-CD                  PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM             PIC X(200).
           03  WRK-RECECD-MAX              PIC 9(05).
      *
       01  ERR-AREA.
           03  ERR-RC                      PIC 9(03).
           03  ERR-IDX                     PIC 9(03).
      *
       01  CONST-AREA.
           03  CONST-BYOCD-MAX             PIC 9(05)   VALUE 6.
           03  CONST-MKCD-MAX              PIC 9(05)   VALUE 21.
           03  CONST-MICODE                PIC X(07)   VALUE "0000999".
           03  CONST-ERRSTR                PIC X(06)   VALUE "■■■".
      *
      *    病名システム予約コードテーブル
           COPY    "CMBYOMEICD.INC".
      *
       01  TUSBYO-AREA.
         02  TUSBYO-OCC                    OCCURS  6.
           COPY    "CPUSERBYOMEI.INC"      REPLACING   //USBYO-//
                                           BY          //TUSBYO-//.
      *
       01  TUSBYOPLUS-AREA.
         02  TUSBYOPLUS-CNT-USBUI          PIC 9(03).
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSBYOMEI.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    病名マスタ−
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    自院病名マスター
       01  USERBYOMEI-REC.
           COPY    "CPUSERBYOMEI.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "COMMON-SPA".
           COPY    "CPXSUB01.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           XSUB01-LNK.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE              XSUB01-OT-AREA
                                   FLG-AREA
                                   IDX-AREA
                                   WRK-AREA
                                   ERR-AREA
                                   TUSBYO-AREA
                                   TUSBYOPLUS-AREA
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-BYOCD-MAX )
               MOVE    XSUB01-BYOCD (IDX0)
                                   TO  XSUB01-O-BYOCD (IDX0)
           END-PERFORM
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM 2001-BYOMEICD-HEN-SEC
           IF    ( FLG-BYOCDARI        =   ZERO )
      *
               CONTINUE
      *
           ELSE
               PERFORM 2001-RECECD-HEN-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    病名コード編集処理
      *****************************************************************
       2001-BYOMEICD-HEN-SEC           SECTION.
      *
           INITIALIZE                  WRK-SAVE-G
                                       WRK-CTRL-G
                                       WRK-STR-G
      *
           MOVE    ZERO            TO  FLG-BYOINS
      *
           MOVE    1               TO  WRK-STR-MAX
      *
           STRING  XSUB01-O-BYOCD (1)    DELIMITED   BY  SIZE
           INTO    WRK-STR-TMP
           WITH    POINTER WRK-STR-MAX
           END-STRING
      *
           IF    ( WRK-STR-MAX     >   ZERO )
               COMPUTE WRK-STR-MAX =   WRK-STR-MAX -   1
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >       CONST-BYOCD-MAX )
      *
               MOVE    1               TO  IDX2
                                           IDX3
               MOVE    SPACE           TO  WRK-STR-TMP
               PERFORM UNTIL ( IDX2    >   WRK-STR-MAX )
                        OR   ( XSUB01-O-BYOCD (IDX1)(IDX2:) =   SPACE )
      *
                   IF    ( XSUB01-O-BYOCD (IDX1)(IDX2:1)
                                           =   X"A1" )
                       IF    ( IDX2 + 1    >   WRK-STR-MAX )
                           CONTINUE
                       ELSE
                           STRING  XSUB01-O-BYOCD (IDX1)(IDX2:2)
                                                   DELIMITED BY SIZE
                           INTO    WRK-STR-TMP
                           WITH    POINTER IDX3
                           END-STRING
      *
                           MOVE    SPACE   TO  WRK-CTRL (IDX1)
      *
                           COMPUTE IDX2    =   IDX2    +   1
      *
                       END-IF
                   ELSE
                       IF    ( XSUB01-O-BYOCD (IDX1)(IDX2:1)
                                           =   "+" OR "-" )
                           MOVE    XSUB01-O-BYOCD (IDX1)(IDX2:1)
                                           TO  WRK-CTRL(IDX1)
                       ELSE
                           STRING  XSUB01-O-BYOCD (IDX1)(IDX2:1)
                                                   DELIMITED BY SIZE
                           INTO    WRK-STR-TMP
                           WITH    POINTER IDX3
                           END-STRING
      *
                           MOVE    SPACE   TO  WRK-CTRL (IDX1)
      *
                       END-IF
                   END-IF
      *
                   COMPUTE IDX2    =   IDX2    +   1
      *
               END-PERFORM
      *
               IF    ( WRK-CTRL (IDX1)     NOT =   SPACE )
                   MOVE    WRK-STR-TMP     TO  XSUB01-O-BYOCD (IDX1)
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >       CONST-BYOCD-MAX )
      *
               IF    ( XSUB01-O-BYOCD (IDX1)    =   SPACE )
                OR   ( WRK-CTRL       (IDX1)    =   "-"   )
                   CONTINUE
               ELSE
                   COMPUTE WRK-SAVE-MAX
                       =   WRK-SAVE-MAX    +   1
                   MOVE    IDX1    TO  WRK-SAVE-IDX (WRK-SAVE-MAX)
                   MOVE    WRK-CTRL (IDX1)
                                   TO  WRK-SAVE-CTRL(WRK-SAVE-MAX)
               END-IF
      *
           END-PERFORM
      *
           INITIALIZE              WRK-CTRL-G
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-BYOCD-MAX )
      *
               IF    ( IDX1        >   WRK-SAVE-MAX )
                   INITIALIZE          XSUB01-O-BYOCD (IDX1)
               ELSE
                   IF    ( WRK-SAVE-IDX (IDX1) >   IDX1 )
                       MOVE    WRK-SAVE-IDX (IDX1) TO  IDX2
                       MOVE    WRK-SAVE-CTRL(IDX2) TO  WRK-CTRL (IDX1)
                       MOVE    XSUB01-O-BYOCD (IDX2)
                                   TO  XSUB01-O-BYOCD (IDX1)
                   ELSE
                       MOVE    WRK-SAVE-CTRL(IDX1) TO  WRK-CTRL (IDX1)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-BYOCD-MAX )
                    OR   ( XSUB01-O-BYOCD (CONST-BYOCD-MAX)
                                       NOT =   SPACE )
      *
               IF    ( WRK-CTRL (IDX1) =   "+" )
                   MOVE    1               TO  FLG-BYOINS
                   MOVE    SPACE           TO  WRK-CTRL (IDX1)
                   PERFORM VARYING IDX2    FROM    CONST-BYOCD-MAX BY -1
                           UNTIL ( IDX2    <   IDX1 + 1 )
                       COMPUTE IDX3    =   IDX2    -   1
                       MOVE    XSUB01-O-BYOCD (IDX3)
                                   TO  XSUB01-O-BYOCD (IDX2)
                       MOVE    WRK-CTRL (IDX3)
                                   TO  WRK-CTRL (IDX2)
                   END-PERFORM
                   INITIALIZE                  XSUB01-O-BYOCD (IDX1)
               END-IF
      *
           END-PERFORM
      *
           MOVE    ZERO            TO  FLG-BYOCDARI
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-BYOCD-MAX )
      *
               IF    ( XSUB01-O-BYOCD (IDX1)   NOT =   SPACE )
                   MOVE    1       TO  FLG-BYOCDARI
                   IF    ( XSUB01-O-BYOCD (IDX1)(1:1)  =   "." )
                       MOVE    XSUB01-O-BYOCD (IDX1) (2:)
                                   TO  WRK-STR-TMP
                       MOVE    SPACE   TO  XSUB01-O-BYOCD (IDX1)
                       STRING  "ZZZ"       DELIMITED   BY  SIZE
                               WRK-STR-TMP DELIMITED   BY  SIZE
                       INTO    XSUB01-O-BYOCD (IDX1)
                       END-STRING
                   END-IF
      *
                   IF    ( XSUB01-O-BYOCD (IDX1)   =   CONST-MICODE )
                       MOVE    1       TO  ERR-RC
                       MOVE    IDX1    TO  ERR-IDX
                       PERFORM 2500-ERRCD-HEN-SEC
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       2001-BYOMEICD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    レセ電コード編集処理
      *****************************************************************
       2001-RECECD-HEN-SEC             SECTION.
      *
           INITIALIZE                  WRK-STR-G
                                       WRK-BYOMEI-G
      *
           MOVE    1                   TO  WRK-BYOCD-LEN-MAX
           STRING  WRK-BYOCD           DELIMITED   BY  SIZE
           INTO    WRK-STR-TMP
           WITH    POINTER WRK-BYOCD-LEN-MAX
           END-STRING
      *
           IF    ( WRK-BYOCD-LEN-MAX   >   ZERO )
               COMPUTE WRK-BYOCD-LEN-MAX
                   =   WRK-BYOCD-LEN-MAX   -   1
           END-IF
      *
           PERFORM 20011-TUSBYO-HEN-SEC
      *
           MOVE    ZERO                    TO  IDXT
           MOVE    1                       TO  WRK-BYOMEI-LEN
           PERFORM VARYING IDX-BYO FROM    1   BY  1
                   UNTIL ( IDX-BYO         >   CONST-BYOCD-MAX )
      *
               IF    ( XSUB01-O-BYOCD (IDX-BYO)    NOT =   SPACE )
                   IF    ( IDX-BYO         =   1 )
                    OR   ( XSUB01-O-TOP (IDX-BYO)      =   1 )
                       MOVE    IDX-BYO TO  IDXT
                       MOVE    1       TO  WRK-BYOMEI-LEN
                   END-IF
      *
                   MOVE    XSUB01-O-BYOCD (IDX-BYO)
                                       TO  WRK-BYOCD
                   MOVE    IDX-BYO     TO  WRK-BYOCD-IDX
                   PERFORM 20011-RECECD-HEN-SEC
               END-IF
      *
           END-PERFORM
      *
           MOVE    1                   TO  WRK-BYOMEI-LEN
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   CONST-BYOCD-MAX )
      *
               IF    ( XSUB01-O-TOP (IDXB) =   1 )
                   IF    ( XSUB01-O-OVER   (IDXB)  =   1 )
                    OR   ( XSUB01-O-HENSYU (IDXB)  =   1 )
                       INITIALIZE          XSUB01-O-RECECD-G  (IDXB)
                       MOVE    1       TO  XSUB01-O-RECECD-MAX(IDXB)
                       MOVE    CONST-MICODE
                                       TO  XSUB01-O-RECECD    (IDXB 1)
                       MOVE    XSUB01-O-BYOMEI (IDXB)
                                       TO  XSUB01-O-RECECDMEI (IDXB 1)
                       MOVE    "1"     TO  XSUB01-O-BYOSTATUS (IDXB)
                       IF    ( XSUB01-O-OVER   (IDXB)  =   1 )
                           MOVE    2           TO  ERR-RC
                           MOVE    IDXB        TO  ERR-IDX
                           PERFORM 2500-ERRCD-HEN-SEC
                       END-IF
                   ELSE
                       INITIALIZE                  ORCSBYOMEIAREA
                       MOVE    "2"             TO  LNK-BYO-SYORI
                       MOVE    XSUB01-O-RECECD-MAX (IDXB)
                                               TO  WRK-RECECD-MAX
                       PERFORM VARYING IDX5    FROM    1   BY  1
                               UNTIL ( IDX5    >   WRK-RECECD-MAX )
                           MOVE    XSUB01-O-RECECD (IDXB IDX5)
                                           TO  LNK-BYO-TBYOMEICD (IDX5)
                       END-PERFORM
                       CALL    "ORCSBYOMEI"    USING
                                               ORCSBYOMEIAREA
                                               SPA-AREA
                       MOVE    LNK-BYO-BYOMEIHENFLG
                                       TO  XSUB01-O-BYOSTATUS (IDXB)
                   END-IF
      *
                   IF    ( XSUB01-O-BYOMEIALL  NOT =   SPACE )
                       STRING  "／"    DELIMITED   BY  SIZE
                       INTO    XSUB01-O-BYOMEIALL
                       WITH    POINTER WRK-BYOMEI-LEN
                       END-STRING
                   END-IF
      *
                   STRING  XSUB01-O-BYOMEI (IDXB) DELIMITED   BY  SPACE
                   INTO    XSUB01-O-BYOMEIALL
                   WITH    POINTER WRK-BYOMEI-LEN
                   END-STRING
      *
                   IF    ( XSUB01-O-BYOSTATUSALL   =   SPACE )
                       MOVE    XSUB01-O-BYOSTATUS (IDXB)
                                       TO  XSUB01-O-BYOSTATUSALL
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           .
       2001-RECECD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ユーザー病名退避領域編集処理
      *****************************************************************
       20011-TUSBYO-HEN-SEC            SECTION.
      *
           PERFORM VARYING IDX-BYO FROM    1   BY  1
                   UNTIL ( IDX-BYO         >   CONST-BYOCD-MAX )
      *
               IF    ( XSUB01-O-BYOCD (IDX-BYO)    NOT =   SPACE )
                   MOVE    XSUB01-O-BYOCD (IDX-BYO)
                                       TO  WRK-BYOCD
                   PERFORM 900-USERBYOMEI-KEY6-SEL-SEC
                   IF    ( FLG-USERBYOMEI  =   ZERO )
                       MOVE    USERBYOMEI-REC
                                       TO  TUSBYO-OCC (IDX-BYO)
                       IF    ( USBYO-BYOMEISBT =   "1" )
                           COMPUTE TUSBYOPLUS-CNT-USBUI
                               =   TUSBYOPLUS-CNT-USBUI
                               +   1
                       END-IF
                   END-IF
      *
                   IF    ( XSUB01-O-BYOMEI-CNT =   ZERO )
      *
                       MOVE    1       TO  XSUB01-O-TOP (IDX-BYO)
      *
                       COMPUTE XSUB01-O-BYOMEI-CNT
                           =   XSUB01-O-BYOMEI-CNT
                           +   1
      *
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( TUSBYOPLUS-CNT-USBUI  =   ZERO )
            AND  ( XSUB01-SINGLE         =   ZERO )
               PERFORM VARYING IDX-BYO FROM    2   BY  1
                       UNTIL ( IDX-BYO         >   CONST-BYOCD-MAX )
                   IF    ( XSUB01-O-BYOCD (IDX-BYO)    NOT =   SPACE )
                       PERFORM 200111-TUSBYO-HEN-SEC
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       20011-TUSBYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ユーザー病名退避領域編集処理
      *****************************************************************
       200111-TUSBYO-HEN-SEC            SECTION.
      *
           MOVE    ZERO        TO  FLG-CHECK
      *
           COMPUTE IDXB    =   IDX-BYO  -  1
           IF    ( XSUB01-O-BYOCD (IDXB)   =   SPACE )
            AND  ( IDXB                    >   1 )
               COMPUTE IDXB    =   IDXB    -  1
           END-IF
      *
           IF    ( TUSBYO-HOSPNUM (IDX-BYO)    NOT = ZERO )
               IF    ( TUSBYO-BYOMEISBT (IDX-BYO)  =  "7" OR "0" )
                   MOVE    1   TO  FLG-CHECK
               END-IF
           ELSE
               EVALUATE    XSUB01-O-BYOCD(IDX-BYO)(1:1)
                                    ALSO   XSUB01-O-BYOCD(IDX-BYO)(4:1) 
               WHEN    "Z"          ALSO    "1"    THRU    "7"
               WHEN    "0" THRU "9" ALSO    ANY
                   MOVE    1   TO  FLG-CHECK
               END-EVALUATE
           END-IF
      *
           IF    ( FLG-CHECK       =   1 )
               IF    ( TUSBYO-HOSPNUM (IDXB)   NOT = ZERO )
                   IF    ( TUSBYO-BYOMEISBT (IDXB) = "8" OR "0" )
      *
                       MOVE    1       TO    XSUB01-O-TOP (IDX-BYO)
      *
                       COMPUTE XSUB01-O-BYOMEI-CNT
                           =   XSUB01-O-BYOMEI-CNT
                           +   1
      *
                   END-IF
               ELSE
                   EVALUATE    XSUB01-O-BYOCD (IDXB)(1:1)
                               ALSO   XSUB01-O-BYOCD (IDXB)(4:1)
                   WHEN    "Z" ALSO    "8"
      *
                       MOVE    1       TO    XSUB01-O-TOP (IDX-BYO)
      *
                       COMPUTE XSUB01-O-BYOMEI-CNT
                           =   XSUB01-O-BYOMEI-CNT
                           +   1
      *
                   WHEN    "Z" ALSO    ANY
                       CONTINUE
                   WHEN    OTHER
      *
                       MOVE    1       TO    XSUB01-O-TOP (IDX-BYO)
      *
                       COMPUTE XSUB01-O-BYOMEI-CNT
                           =   XSUB01-O-BYOMEI-CNT
                           +   1
      *
                   END-EVALUATE
               END-IF
           END-IF
      *
           .
       200111-TUSBYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    レセ電コード編集処理
      *****************************************************************
       20011-RECECD-HEN-SEC            SECTION.
      *
           MOVE    WRK-BYOCD-LEN-MAX   TO  WRK-BYOCD-LEN
      *
           PERFORM VARYING IDX0    FROM    WRK-BYOCD-LEN-MAX  BY  -1
                   UNTIL ( IDX0    <   1 )
                    OR   ( WRK-BYOCD (IDX0 : )    NOT =   SPACE )
      *
               COMPUTE WRK-BYOCD-LEN   =   WRK-BYOCD-LEN   -   1
      *
           END-PERFORM
      *
           PERFORM 20011-USERBYO-HEN-SEC
           IF    ( FLG-UBYOERR         =   ZERO )
               STRING  WRK-USBYOMEI        DELIMITED   BY  SPACE
               INTO    XSUB01-O-BYOMEI (IDXT)
               WITH    POINTER WRK-BYOMEI-LEN
               END-STRING
           ELSE
               IF    ( WRK-BYOCD-LEN   =   7 )
                   MOVE    WRK-BYOCD   TO  WRK-RECECD
                   PERFORM 20011-BYOMEI-KUMITATE-SEC
                   IF    ( FLG-BYOMEIERR   NOT =   ZERO )
                       MOVE    3               TO  ERR-RC
                       MOVE    WRK-BYOCD-IDX   TO  ERR-IDX
                       PERFORM 2500-ERRCD-HEN-SEC
                   END-IF
      *
                   STRING  WRK-RECECDMEI       DELIMITED   BY  SPACE
                   INTO    XSUB01-O-BYOMEI (IDXT)
                   WITH    POINTER WRK-BYOMEI-LEN
                   END-STRING
               ELSE
                   MOVE    4               TO  ERR-RC
                   MOVE    WRK-BYOCD-IDX   TO  ERR-IDX
                   PERFORM 2500-ERRCD-HEN-SEC
               END-IF
           END-IF
      *
           .
       20011-RECECD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    病名組立処理
      *****************************************************************
       20011-BYOMEI-KUMITATE-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-RECECDMEI
           MOVE    ZERO                TO  FLG-BYOMEIERR
      *
           MOVE    WRK-RECECD          TO  WRK-CMB-CD
           PERFORM 2100-YYKBYOCD-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  WRK-RECECDMEI
           ELSE
               PERFORM 900-BYOMEI-KEY-SEL-SEC
               IF    ( FLG-BYOMEI  =   ZERO )
                   MOVE    BYO-BYOMEI  TO  WRK-RECECDMEI
               ELSE
                   MOVE    CONST-ERRSTR
                                       TO  WRK-RECECDMEI
                   MOVE    1           TO  FLG-BYOMEIERR
               END-IF
           END-IF
      *
           IF    ( FLG-BYOMEIERR       =   ZERO )
               IF    ( XSUB01-O-RECECD-MAX (IDXT)
                                       >=  CONST-MKCD-MAX )
                   MOVE    1           TO  XSUB01-O-OVER (IDXT)
               ELSE
                   COMPUTE XSUB01-O-RECECD-MAX (IDXT)
                       =   XSUB01-O-RECECD-MAX (IDXT) +   1
      *
                   MOVE    XSUB01-O-RECECD-MAX (IDXT)
                                   TO  IDXN
                   MOVE    WRK-RECECD
                                   TO  XSUB01-O-RECECD    (IDXT IDXN)
                   MOVE    WRK-RECECDMEI
                                   TO  XSUB01-O-RECECDMEI (IDXT IDXN)
      *
               END-IF
           END-IF
      *
           .
       20011-BYOMEI-KUMITATE-EXT.
           EXIT.
      *****************************************************************
      *    自院病名編集処理
      *****************************************************************
       20011-USERBYO-HEN-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-USBYOMEI
           MOVE    ZERO                TO  FLG-UBYOERR
      *
           IF    ( TUSBYO-HOSPNUM (WRK-BYOCD-IDX)
                                       NOT =   ZERO )
      *
               MOVE    TUSBYO-OCC (WRK-BYOCD-IDX)
                                       TO  USERBYOMEI-REC
      *
               MOVE    USBYO-BYOMEI    TO  WRK-USBYOMEI
               PERFORM VARYING IDX4    FROM    1   BY  1
                       UNTIL ( IDX4    >   CONST-MKCD-MAX )
                        OR   ( USBYO-BYOMEICD (IDX4)  =   "9999999" )
                        OR   ( USBYO-BYOMEICD (IDX4)  =  SPACE )
                   IF    ( USBYO-BYOMEICD (IDX4)  =   CONST-MICODE )
                       MOVE    1   TO  XSUB01-O-HENSYU (IDXT)
                   ELSE
                       MOVE    USBYO-BYOMEICD (IDX4)
                                       TO  WRK-RECECD
                       PERFORM 20011-BYOMEI-KUMITATE-SEC
                   END-IF
               END-PERFORM
           ELSE
               MOVE    CONST-ERRSTR    TO  WRK-USBYOMEI
               MOVE    1               TO  FLG-UBYOERR
           END-IF
      *
           .
       20011-USERBYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    予約病名コードチェック処理
      *****************************************************************
       2100-YYKBYOCD-CHK-SEC           SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   TBL-BYOMEICD-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( TBL-YYK-BYOMEICD (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE   TBL-YYK-BYOMEI(IDX-CMB)
                                           TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       2100-YYKBYOCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    エラーコード編集処理
      *****************************************************************
       2500-ERRCD-HEN-SEC             SECTION.
      *
           IF    ( XSUB01-O-RC        =    ZERO )
               MOVE    ERR-RC         TO   XSUB01-O-RC
               MOVE    ERR-IDX        TO   XSUB01-O-ERRIDX
           END-IF
      *
           .
       2500-ERRCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *    PERFORM 800-DISPLAY-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    返却値表示処理
      *****************************************************************
       800-DISPLAY-SEC                     SECTION.
      *
           DISPLAY "= ST ======================================"
           DISPLAY "XSUB01-O-BYOMEI-CNT = " XSUB01-O-BYOMEI-CNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-BYOCD-MAX )
      *
               DISPLAY IDX0 ":TOP      [" XSUB01-O-TOP      (IDX0) "]"
               DISPLAY IDX0 ":OVER     [" XSUB01-O-OVER     (IDX0) "]"
               DISPLAY IDX0 ":HENSYU   [" XSUB01-O-HENSYU   (IDX0) "]"
               DISPLAY IDX0 ":TOP      [" XSUB01-O-TOP      (IDX0) "]"
               DISPLAY IDX0 ":BYOCD    [" XSUB01-O-BYOCD    (IDX0) "]"
               DISPLAY IDX0 ":BYOMEI   [" XSUB01-O-BYOMEI   (IDX0) "]"
               DISPLAY IDX0 ":BYOSTATUS[" XSUB01-O-BYOSTATUS(IDX0) "]"
      *
               IF    ( XSUB01-O-TOP      (IDX0) =  1 )
                   MOVE    XSUB01-O-RECECD-MAX (IDX0)
                                       TO  WRK-RECECD-MAX
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL ( IDX1    >   WRK-RECECD-MAX )
      *
                       DISPLAY IDX0 "-" IDX1 ":RECECD ["
                               XSUB01-O-RECECD (IDX0 IDX1) "]"
                       DISPLAY IDX0 "-" IDX1 ":RECEMEI["
                               XSUB01-O-RECECDMEI (IDX0 IDX1) "]"
      *
                   END-PERFORM
               END-IF
      *
           END-PERFORM
           DISPLAY "= ED ======================================"
      *
           .
       800-DISPLAY-EXT.
           EXIT.
      *****************************************************************
      *    病名検索処理(KEY)
      *****************************************************************
       900-BYOMEI-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-BYOMEI
      *
           INITIALIZE                  BYOMEI-REC
           MOVE    WRK-RECECD      TO  BYO-BYOMEICD
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    BYOMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_byomei"    TO MCP-TABLE
           MOVE    "key"           TO MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
                MOVE    MCPDATA-REC TO  BYOMEI-REC
           ELSE
                INITIALIZE              BYOMEI-REC
                MOVE    1           TO  FLG-BYOMEI
           END-IF
      *
           MOVE    "tbl_byomei"    TO MCP-TABLE
           MOVE    "key"           TO MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-BYOMEI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    自院病名検索処理(KEY6)
      *****************************************************************
       900-USERBYOMEI-KEY6-SEL-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-USERBYOMEI
      *
           INITIALIZE                  USERBYOMEI-REC
           MOVE    SPA-HOSPNUM     TO  USBYO-HOSPNUM
           MOVE    WRK-BYOCD       TO  USBYO-BYOMEIINPUTCD
           MOVE    USERBYOMEI-REC  TO  MCPDATA-REC
           MOVE    "tbl_userbyomei"    TO MCP-TABLE
           MOVE    "key6"              TO MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
                MOVE    MCPDATA-REC    TO  USERBYOMEI-REC
           ELSE
                INITIALIZE                 USERBYOMEI-REC
                MOVE    1              TO  FLG-USERBYOMEI
           END-IF
      *
           MOVE    "tbl_userbyomei"    TO MCP-TABLE
           MOVE    "key6"              TO MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-USERBYOMEI-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
