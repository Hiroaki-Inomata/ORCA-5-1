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
       PROGRAM-ID.                     ORCSS016.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : 収納更新処理
      *  管理者            :
      *  作成日付    作業者        記述
      *  17/04/28    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *
       WORKING-STORAGE                 SECTION.
      *
       01  IDX-AREA.
           03  IDX1                            PIC 9(05).
           03  IDX5                            PIC 9(05).
      *
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
           03  FLG-SYUNOU                      PIC 9(01).
           03  FLG-SYUMEI                      PIC 9(01).
           03  FLG-SYUTTL                      PIC 9(01).
           03  FLG-PTTEIKIRRK      PIC 9(01).
      *
       01  WRK-AREA.
           03  WRK-DENPEDANUM                  PIC 9(02).
           03  WRK-OLD-NYUHEN-YMD              PIC X(08).
           03  WRK-OLD-NYUHEN-HMS              PIC X(06).
           03  WRK-SKYMONEY                    PIC S9(10).
           03  WRK-NYUKIN                      PIC S9(10).
      *
       01  CONST-AREA.
           03  CONST-REQMODE-G.
               05  CONST-REQMODE-NYUKIN           PIC X(02) VALUE "01".
               05  CONST-REQMODE-RRKSYU           PIC X(02) VALUE "02".
               05  CONST-REQMODE-TORIKESHI        PIC X(02) VALUE "03".
               05  CONST-REQMODE-SKY-TORIKESHI    PIC X(02) VALUE "08".
           03  CONST-EDANUM-MAX                PIC 9(03) VALUE 99.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細マスター
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    収納合計
       01  SYUTOTAL-REC.
           COPY    "CPSYUTOTAL.INC".
      *
      *    患者定期請求履歴
       01  PTTEIKIRRK-REC.
           COPY    "CPPTTEIKIRRK.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    日報キー取得サブ
           COPY "CPORCSDAILYKEY.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    収納更新差額取得
           COPY  "CPORCSS002.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                         SECTION.
           COPY    "CPORCSS016.INC".
           COPY    "COMMON-SPA".
       01  LNKSYU-REC.
           COPY    "CPSYUNOU.INC"  REPLACING //SYU-//
                                   BY        //LNKSYU-//.
       01  LNKSMEI-REC.
           COPY    "CPSYUMEI.INC"  REPLACING //SMEI-//
                                   BY        //LNKSMEI-//.
      *
      ****************************************************************
       PROCEDURE                       DIVISION
           USING
           SS016-AREA
           LNKSYU-REC
           LNKSMEI-REC
           SPA-AREA
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           PERFORM 990-EXIT-PROGRAM-SEC
      *
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  IDX-AREA
                                       FLG-AREA
                                       WRK-AREA
      *
           PERFORM 800-MCNDATE-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           EVALUATE    SS016-REQMODE
           WHEN    CONST-REQMODE-NYUKIN
               PERFORM   4902-NYUKIN-KOUSIN-SEC
           WHEN    CONST-REQMODE-TORIKESHI
               PERFORM   4902-TORIKESHI-KOUSIN-SEC
           WHEN    CONST-REQMODE-RRKSYU
               PERFORM   4902-NYUKIN-RRKSYU-SEC
           WHEN    CONST-REQMODE-SKY-TORIKESHI
               PERFORM   430-TORIKESI-KOUSHIN-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    終了　処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *************************************************************
      *    入金編集処理
      *************************************************************
       4902-NYUKIN-KOUSIN-SEC          SECTION.
      *
           PERFORM 49021-SYUNOU-UPD-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 49021-SYUMEI-INS-SEC
           END-IF
      *
           .
       4902-NYUKIN-KOUSIN-EXT.
           EXIT.
      *************************************************************
      *    入金取消編集処理
      *************************************************************
       4902-TORIKESHI-KOUSIN-SEC       SECTION.
      *
           PERFORM 49021-SYUNOU-UPD-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 49021-SYUMEI-INS-SEC
           END-IF
      *
           .
       4902-TORIKESHI-KOUSIN-EXT.
           EXIT.
      *************************************************************
      *    履歴修正編集処理
      *************************************************************
       4902-NYUKIN-RRKSYU-SEC          SECTION.
      *
           PERFORM 49021-SYUNOU-UPD-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF  ( SS016-NYUGAIKBN   =   "2" )
                AND( SS016-PROC-FLG    =   1  OR  2 )
                   PERFORM 49021-SYUTTL-UPD-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 49021-SYUMEI-UPD-SEC
           END-IF
      *
           PERFORM 900-SYUMEI-KEY2-SEL-SEC
      *
           MOVE    ZERO                TO  WRK-NYUKIN
           PERFORM UNTIL ( FLG-SYUMEI  NOT =   ZERO )
      *
               COMPUTE WRK-NYUKIN
                   =   WRK-NYUKIN
                   +   SMEI-NYUHEN-MONEY
      *
               PERFORM 900-SYUMEI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    入金額の不整合をチェックし補正
           INITIALIZE                    SYUNOU-REC
           MOVE    SPA-HOSPNUM       TO  SYU-HOSPNUM
           MOVE    SS016-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    SS016-PTID        TO  SYU-PTID
           MOVE    SS016-DENPNUM     TO  SYU-DENPNUM
           PERFORM 900-SYUNOU-KEY-SEL-SEC
           IF    ( WRK-NYUKIN         >=  ZERO )
            AND  ( WRK-NYUKIN      NOT =  SYU-NYUKIN-TOTAL )
               MOVE    WRK-NYUKIN     TO  SYU-NYUKIN-TOTAL
               MOVE    SYUNOU-REC     TO  MCPDATA-REC
               MOVE    "tbl_syunou"   TO  MCP-TABLE
               MOVE    "key"          TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC              NOT =   ZERO )
                   MOVE    "4909"     TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4902-NYUKIN-RRKSYU-EXT.
           EXIT.
      *************************************************************
      *    収納マスタ更新処理
      *************************************************************
       49021-SYUNOU-UPD-SEC            SECTION.
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SS016-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    SS016-PTID        TO  SYU-PTID
           MOVE    SS016-DENPNUM     TO  SYU-DENPNUM
           PERFORM 900-SYUNOU-KEY-SEL-SEC
           IF    ( FLG-SYUNOU      NOT =   ZERO )
               MOVE    "4900"          TO  SPA-ERRCD
               PERFORM 910-DBABORT-SEC
           ELSE
      *
               EVALUATE    SS016-REQMODE
               WHEN    CONST-REQMODE-NYUKIN
               WHEN    CONST-REQMODE-TORIKESHI
                   COMPUTE SYU-NYUKIN-TOTAL
                       =   SYU-NYUKIN-TOTAL
                       +   SS016-NYUKIN
               WHEN    CONST-REQMODE-RRKSYU
      *
                   IF    ( SS016-PROC-FLG    =   1  OR  2 )
      *
                       COMPUTE SYU-SKYMONEY
                           =   SYU-SKYMONEY
                           -   SS016-SKYMONEY-MOTO
                           +   SS016-SKYMONEY
      *
                       COMPUTE SYU-CHOSEI1
                           =   SYU-CHOSEI1
                           -   SS016-CHOSEI1-MOTO
                           +   SS016-CHOSEI1
      *
                       COMPUTE SYU-CHOSEI2
                           =   SYU-CHOSEI2
                           -   SS016-CHOSEI2-MOTO
                           +   SS016-CHOSEI2
      *
                       COMPUTE SYU-CHOSEI
                           =   SYU-CHOSEI1
                           +   SYU-CHOSEI2
                   END-IF
      *
                   COMPUTE SYU-NYUKIN-TOTAL
                       =   SYU-NYUKIN-TOTAL
                       -   SS016-NYUKIN-MOTO
                       +   SS016-NYUKIN
               END-EVALUATE
      *
               COMPUTE SYU-NYUKIN-KAISU    =   SYU-NYUKIN-KAISU    +   1
      *
               IF    ( SYU-DENPJTIKBN      =   "3" )
                   CONTINUE
               ELSE
                   IF    ( SYU-SKYMONEY    =   ZERO )
                       MOVE    SPACE       TO  SYU-DENPJTIKBN
                   ELSE
                       IF    ( SYU-SKYMONEY    >   SYU-NYUKIN-TOTAL )
                           MOVE    "2"     TO  SYU-DENPJTIKBN
                       ELSE
                           MOVE    "1"     TO  SYU-DENPJTIKBN
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    SPA-OPID            TO  SYU-OPID
               MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
      *
               INITIALIZE                  SS002-AREA
               MOVE    "2"                 TO  SS002-UPDKBN
               MOVE    SS016-SYURRKNUM   TO  SS002-IN-SYURRKNUM
               MOVE    SS016-SYUEDANUM   TO  SS002-IN-SYUEDANUM
               MOVE    SS016-SYURRKUPDKBN TO SS002-IN-SYURRKUPDKBN
               CALL    "ORCSS002"  USING   SS002-AREA
                                           SYUNOU-REC
                                           SPA-AREA
               IF    ( SS002-RC    NOT =   ZERO )
                   MOVE    "4901"          TO  SPA-ERRCD
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   MOVE    SYUNOU-REC      TO  MCPDATA-REC
                   MOVE    "tbl_syunou"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
                   IF    ( MCP-RC              NOT =   ZERO )
                       MOVE    "4902"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           MOVE    SYUNOU-REC              TO  LNKSYU-REC
      *
           .
       49021-SYUNOU-UPD-EXT.
           EXIT.
      *************************************************************
      *    収納合計マスタ更新処理
      *************************************************************
       49021-SYUTTL-UPD-SEC            SECTION.
      *
           PERFORM 900-SYUTOTAL-KEY-SEL-SEC
           IF    ( FLG-SYUTTL      =   ZERO )
      *
               COMPUTE SYUTTL-SKYMONEY
                   =   SYUTTL-SKYMONEY
                   -   SS016-SKYMONEY-MOTO
                   +   SS016-SKYMONEY
      *
               COMPUTE SYUTTL-CHOSEI1
                   =   SYUTTL-CHOSEI1
                   -   SS016-CHOSEI1-MOTO
                   +   SS016-CHOSEI1
      *
               COMPUTE SYUTTL-CHOSEI2
                   =   SYUTTL-CHOSEI2
                   -   SS016-CHOSEI2-MOTO
                   +   SS016-CHOSEI2
      *
               COMPUTE SYUTTL-CHOSEI
                   =   SYUTTL-CHOSEI1
                   +   SYUTTL-CHOSEI2
      *
               MOVE    SPA-OPID            TO  SYUTTL-OPID
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  SYUTTL-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYUTTL-UPHMS
               MOVE    SYUTOTAL-REC        TO  MCPDATA-REC
               MOVE    "tbl_syutotal"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC              NOT =   ZERO )
                   MOVE    "4903"          TO  SPA-ERRCD
               END-IF
      *
           END-IF
      *
           .
       49021-SYUTTL-UPD-EXT.
           EXIT.
      *************************************************************
      *    収納明細作成処理
      *************************************************************
       49021-SYUMEI-INS-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUMEI
           INITIALIZE                      SYUMEI-REC
           MOVE    SPA-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SS016-NYUGAIKBN   TO  SMEI-NYUGAIKBN
           MOVE    SS016-PTID        TO  SMEI-PTID
           MOVE    SS016-DENPNUM     TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF     ( MCP-RC             =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-SYUMEI      NOT =   ZERO )
            OR   ( SMEI-DENPEDANUM    >=   CONST-EDANUM-MAX )
               MOVE    "4904"          TO  SPA-ERRCD
               PERFORM 910-DBABORT-SEC
           ELSE
      *
               COMPUTE WRK-DENPEDANUM  =   SMEI-DENPEDANUM +   1
      *
               MOVE    WRK-DENPEDANUM  TO  SS016-DENPEDANUM
      *
               INITIALIZE                  SYUMEI-REC
      *
               MOVE    SPA-HOSPNUM          TO  SMEI-HOSPNUM
               MOVE    SS016-NYUGAIKBN   TO  SMEI-NYUGAIKBN
               MOVE    SS016-PTID        TO  SMEI-PTID
      *
      *        伝票番号
               MOVE    SS016-DENPNUM     TO  SMEI-DENPNUM
      *
      *        伝票番号枝番
               MOVE    WRK-DENPEDANUM      TO  SMEI-DENPEDANUM
      *
      *        入金連番
               MOVE    ZERO                TO  SMEI-NYUKINRENNUM
      *
      *        診療科
               MOVE    SS016-SRYKA       TO  SMEI-SRYKA
      *
      *        伝票状態区分
               MOVE    SPACE               TO  SMEI-MEISAIJOUTAIKBN
      *
      *        請求発行日（発行日の西暦）
               MOVE    SPACE               TO  SMEI-SKYPRINTYMD
      *
      *        請求再発行日
               MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
      *
      *        領収書発行日
               MOVE    SPACE               TO  SMEI-RYOSYUPRINTYMD
      *
      *        領収書再発行日
               MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
      *
      *        請求金額
               MOVE    ZERO                TO  SMEI-SKYMONEY
      *
      *        入返金区分
               IF    ( SS016-REQMODE     =   CONST-REQMODE-NYUKIN )
                   MOVE    "1"             TO  SMEI-NYUHEN-KBN
               ELSE
                   MOVE    "2"             TO  SMEI-NYUHEN-KBN
               END-IF
      *
      *        請求金額
               IF    ( SS016-PROC-FLG    = 1 OR 2 )
                   MOVE    SS016-SKYMONEY
                                           TO  SMEI-SKYMONEY
               ELSE
                   MOVE    ZERO            TO  SMEI-SKYMONEY
               END-IF
      *
      *        入返金額
               MOVE    SS016-NYUKIN      TO  SMEI-NYUHEN-MONEY
      *
      *        入返金日
               IF    ( SS016-NYUKINYMD     IS NOT NUMERIC )
                   MOVE    SPA-SYSYMD      TO  SMEI-NYUHEN-YMD
               ELSE
                   MOVE    SS016-NYUKINYMD
                                           TO  SMEI-NYUHEN-YMD
               END-IF
      *
      *        入金方法
               IF    ( SS016-REQMODE     =   CONST-REQMODE-NYUKIN )
                   MOVE    SS016-NYKN-HOHO
                                           TO  SMEI-NYUKIN-HOHO
               ELSE
                   IF    ( SS016-NYKN-HOHO   =   SPACE )
                       MOVE    "00"        TO  SMEI-NYUKIN-HOHO
                   ELSE
                       MOVE    SS016-NYKN-HOHO
                                           TO  SMEI-NYUKIN-HOHO
                   END-IF
               END-IF
      *
      *        状態区分
               EVALUATE    SS016-REQMODE
               WHEN    CONST-REQMODE-NYUKIN
                   IF    ( SS016-NYUKIN  >=  ZERO )
                       MOVE    "2"         TO  SMEI-JOUTAIKBN
                   ELSE
                       MOVE    "4"         TO  SMEI-JOUTAIKBN
                   END-IF
               WHEN    CONST-REQMODE-TORIKESHI
                       MOVE    "6"         TO  SMEI-JOUTAIKBN
               WHEN    OTHER
                   MOVE    "8"             TO  SMEI-JOUTAIKBN
               END-EVALUATE
      *
               MOVE    SPA-OPID            TO  SMEI-OPID
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  SMEI-CREYMD
                                               SMEI-UPYMD
               MOVE    SMCNDATE-HMS        TO  SMEI-CREHMS
                                               SMEI-UPHMS
      *
               MOVE    SMEI-CREYMD         TO  SDAILYKEY-CREYMD
               MOVE    SMEI-CREHMS         TO  SDAILYKEY-CREHMS
               MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
               IF    ( SS016-NYUKINHMS     IS  NUMERIC )
                   MOVE    SMEI-NYUHEN-YMD TO  SDAILYKEY-CREYMD
                   MOVE    SS016-NYUKINHMS TO  SDAILYKEY-CREHMS
                   MOVE    "FORCE"         TO  SDAILYKEY-BASEYMD
               END-IF
               PERFORM 800-DAILYKEY-SEC
               MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
      *
               MOVE    SYUMEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_syumei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "4905"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           MOVE    SYUMEI-REC          TO  LNKSMEI-REC
      *
           .
       49021-SYUMEI-INS-EXT.
           EXIT.
      *************************************************************
      *    収納明細更新処理
      *************************************************************
       49021-SYUMEI-UPD-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUMEI
           INITIALIZE                      SYUMEI-REC
           MOVE    SPA-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SS016-NYUGAIKBN   TO  SMEI-NYUGAIKBN
           MOVE    SS016-PTID        TO  SMEI-PTID
           MOVE    SS016-DENPNUM     TO  SMEI-DENPNUM
           MOVE    SS016-DENPEDANUM  TO  SMEI-DENPEDANUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =    ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               MOVE    1               TO  FLG-SYUMEI
               INITIALIZE                  SYUMEI-REC
           END-IF
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-SYUMEI      NOT =   ZERO )
               MOVE    "4906"          TO  SPA-ERRCD
               PERFORM 910-DBABORT-SEC
           ELSE
      *
               INITIALIZE                      SYUNOU-REC
               MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
               MOVE    SS016-NYUGAIKBN   TO  SYU-NYUGAIKBN
               MOVE    SS016-PTID        TO  SYU-PTID
               MOVE    SS016-DENPNUM     TO  SYU-DENPNUM
               PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
               MOVE    SMEI-NYUHEN-YMD     TO  WRK-OLD-NYUHEN-YMD
               MOVE    SMEI-DAILYKEY(9:4)  TO  WRK-OLD-NYUHEN-HMS
               MOVE    "00"                TO  WRK-OLD-NYUHEN-HMS(5:2)
      *
      *        請求金額
               IF    ( SS016-PROC-FLG    = 1 OR 2 )
                   MOVE    SS016-SKYMONEY
                                           TO  SMEI-SKYMONEY
               ELSE
                   MOVE    ZERO            TO  SMEI-SKYMONEY
               END-IF
      *
      *        入返金額
               MOVE    SS016-NYUKIN      TO  SMEI-NYUHEN-MONEY
      *
      *        入返金日
               IF    ( SS016-NYUKINYMD     IS NOT NUMERIC )
                   MOVE     SPA-SYSYMD     TO  SMEI-NYUHEN-YMD
               ELSE
                   MOVE     SS016-NYUKINYMD
                                           TO  SMEI-NYUHEN-YMD
               END-IF
      *
      *        入金方法
               IF    ( SS016-NYUKIN      >   ZERO )
                OR   ( SMEI-DENPEDANUM     =   1 )
                   MOVE  SS016-NYKN-HOHO TO  SMEI-NYUKIN-HOHO
               ELSE
                   IF    ( SMEI-JOUTAIKBN  =   "6" )
                    AND  ( SS016-NYUKIN  <   ZERO )
                       IF    ( SS016-NYKN-HOHO   =   SPACE )
                           MOVE    "00"    TO  SMEI-NYUKIN-HOHO
                       ELSE
                           MOVE    SS016-NYKN-HOHO
                                           TO  SMEI-NYUKIN-HOHO
                       END-IF
                   ELSE
                           MOVE    "00"    TO  SMEI-NYUKIN-HOHO
                   END-IF
               END-IF
      *
      *        伝票状態区分
               EVALUATE    SS016-PROC-FLG
               WHEN    1
                   IF    ( SS016-SKYMONEY   <=   ZERO )
                       MOVE    SPACE   TO  SMEI-JOUTAIKBN
                   ELSE
                       IF    ( SS016-NYUGAIKBN
                                       =   "1" )
                           MOVE    "9" TO  SMEI-JOUTAIKBN
                       ELSE
                           MOVE    "1" TO  SMEI-JOUTAIKBN
                       END-IF
                   END-IF
               WHEN    2
               WHEN    4
                   CONTINUE
               WHEN    OTHER
                   IF    ( SS016-NYUKIN     >=   ZERO )
                       MOVE    "2"     TO  SMEI-JOUTAIKBN
                   ELSE
                       MOVE    "4"     TO  SMEI-JOUTAIKBN
                   END-IF
               END-EVALUATE
      *
               MOVE    SPA-OPID            TO  SMEI-OPID
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  SMEI-UPYMD
               MOVE    SMCNDATE-HMS        TO  SMEI-UPHMS
      *
               IF    ( SS016-NYUKINYMD     =   WRK-OLD-NYUHEN-YMD )
                AND (( SS016-NYUKINHMS  IS NOT NUMERIC )
                 OR  ( SS016-NYUKINHMS     =   WRK-OLD-NYUHEN-HMS ))
                   CONTINUE
               ELSE
                   MOVE    SMEI-UPYMD          TO  SDAILYKEY-CREYMD
                   MOVE    SMEI-UPHMS          TO  SDAILYKEY-CREHMS
                   MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
                   IF    ( SS016-NYUKINHMS     IS  NUMERIC )
                       MOVE    SMEI-NYUHEN-YMD TO  SDAILYKEY-CREYMD
                       MOVE    SS016-NYUKINHMS
                                               TO  SDAILYKEY-CREHMS
                       MOVE    "FORCE"         TO  SDAILYKEY-BASEYMD
                   END-IF
                   PERFORM 800-DAILYKEY-SEC
                   MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
               END-IF
      *
               MOVE    SYUMEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_syumei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "4907"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           MOVE    SYUMEI-REC          TO  LNKSMEI-REC
      *
           .
       49021-SYUMEI-UPD-EXT.
           EXIT.
      **************************************************************
      *    請求取消更新処理
      **************************************************************
       430-TORIKESI-KOUSHIN-SEC        SECTION.
      *
      *    収納マスタと収納明細の請求金額を０円に変更する
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    "1"                 TO  SYU-NYUGAIKBN
           MOVE    SS016-PTID          TO  SYU-PTID
           MOVE    SS016-DENPNUM       TO  SYU-DENPNUM
           PERFORM 900-SYUNOU-KEY-SEL-SEC
           MOVE    SYU-SKYMONEY        TO  WRK-SKYMONEY
      *
           IF    ( FLG-SYUNOU          =   ZERO )
               CONTINUE
           ELSE
               MOVE    "4908"          TO  SPA-ERRCD
               GO  TO  430-TORIKESI-KOUSHIN-EXT
           END-IF
      *
           IF    ( SYU-CREATEKBN   =   2 )
      *
      *        患者定期請求履歴更新処理
               PERFORM 4301-PTTEIKIRRK-UPD-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  430-TORIKESI-KOUSHIN-EXT
               END-IF
           END-IF
      *
           MOVE    "3"             TO  SYU-DENPJTIKBN
           MOVE    ZERO            TO  SYU-SKYMONEY
      *
           MOVE    SPA-OPID        TO  SYU-OPID
           MOVE    SMCNDATE-YMD    TO  SYU-UPYMD
           MOVE    SMCNDATE-HMS    TO  SYU-UPHMS
      *
           PERFORM 800-SS002-SEC
      *
      *    収納明細作成処理
           PERFORM 4301-SMEI-SAKUSEI-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  430-TORIKESI-KOUSHIN-EXT
           END-IF
      *
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF   ( MCP-RC       NOT =   ZERO )
                MOVE    "4909"     TO  SPA-ERRCD
                GO  TO  430-TORIKESI-KOUSHIN-EXT
           END-IF
      *
           .
       430-TORIKESI-KOUSHIN-EXT.
           EXIT.
      *
      **************************************************************
      *    請求取消処理（収納明細レコード作成）
      **************************************************************
       4301-SMEI-SAKUSEI-SEC           SECTION.
      *
           PERFORM 900-SYUMEI-KEY7-SEL-SEC
           COMPUTE IDX5    =   SMEI-DENPEDANUM +   1
      *
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
           MOVE    SPA-HOSPNUM         TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
      *伝票番号
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *伝票番号枝番
           MOVE    IDX5                TO  SMEI-DENPEDANUM
      *入金連番
           MOVE    ZERO                TO  SMEI-NYUKINRENNUM
      *診療科
           MOVE    SYU-SRYKA           TO  SMEI-SRYKA
      *伝票状態区分
           MOVE    SPACE               TO  SMEI-MEISAIJOUTAIKBN
      *請求発行日（発行日の西暦）
           MOVE    SPACE               TO  SMEI-SKYPRINTYMD
      *請求再発行日
           MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
      *領収書発行日
           MOVE    SPACE               TO  SMEI-RYOSYUPRINTYMD
      *領収書再発行日
           MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
      *請求金額
           COMPUTE SMEI-SKYMONEY   =   WRK-SKYMONEY
                                   *   -1
      *入返金区分
           MOVE    SPACE               TO  SMEI-NYUHEN-KBN
      *入返金額
           MOVE    ZERO                TO  SMEI-NYUHEN-MONEY
      *入返金日
           IF    ( SS016-NYUKINYMD     IS NOT NUMERIC )
               MOVE    SPA-SYSYMD      TO  SMEI-NYUHEN-YMD
           ELSE
               MOVE    SS016-NYUKINYMD TO  SMEI-NYUHEN-YMD
           END-IF
      *入金方法
           MOVE    "00"                TO  SMEI-NYUKIN-HOHO
      *状態区分
           MOVE    "5"                 TO  SMEI-JOUTAIKBN
      *
           MOVE    SPA-OPID            TO  SMEI-OPID
           MOVE    SMCNDATE-YMD        TO  SMEI-CREYMD
                                           SMEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SMEI-CREHMS
                                           SMEI-UPHMS
      *
           MOVE    SMEI-CREYMD         TO  SDAILYKEY-CREYMD
           MOVE    SMEI-CREHMS         TO  SDAILYKEY-CREHMS
           MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
           IF    ( SS016-NYUKINHMS     IS  NUMERIC )
                   MOVE    SMEI-NYUHEN-YMD TO  SDAILYKEY-CREYMD
                   MOVE    SS016-NYUKINHMS
                                           TO  SDAILYKEY-CREHMS
                   MOVE    "FORCE"         TO  SDAILYKEY-BASEYMD
           END-IF
           PERFORM 800-DAILYKEY-SEC
           MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
      *
           MOVE    SS002-SYURRKNUM     TO  SMEI-SYURRKNUM
           MOVE    SS002-SYUEDANUM     TO  SMEI-SYUEDANUM
           MOVE    SS002-SYURRKUPDKBN  TO  SMEI-SYURRKUPDKBN
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4910"          TO  SPA-ERRCD
           END-IF
      *
           .
       4301-SMEI-SAKUSEI-EXT.
           EXIT.
      *
      **************************************************************
      *    患者定期請求履歴更新処理
      **************************************************************
       4301-PTTEIKIRRK-UPD-SEC         SECTION.
      *
      *    患者定期請求情報更新
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           INITIALIZE  PTTEIKIRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTTEIKIRRK-HOSPNUM
           MOVE    SYU-PTID            TO  PTTEIKIRRK-PTID
           MOVE    SYU-DENPNUM         TO  PTTEIKIRRK-DENPNUM
      *
           MOVE    SPA-OPID            TO  PTTEIKIRRK-OPID
           MOVE    SMCNDATE-YMD        TO  PTTEIKIRRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTTEIKIRRK-UPHMS
      *
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "upd4"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4911"      TO  SPA-ERRCD
           END-IF
      *
           .
       4301-PTTEIKIRRK-UPD-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    日報キー取得サブ
      *****************************************************************
       800-DAILYKEY-SEC                SECTION.
      *
           CALL    "ORCSDAILYKEY"      USING
                                       SDAILYKEYAREA
                                       SPA-AREA
      *
           .
      *
       800-DAILYKEY-EXT.
           EXIT.
      *****************************************************************
      *    収納更新差額取得サブ
      *****************************************************************
       800-SS002-SEC           SECTION.
      *
           INITIALIZE                      SS002-AREA
           CALL    "ORCSS002"      USING   SS002-AREA
                                           SYUNOU-REC
                                           SPA-AREA
      *
           .
      *
       800-SS002-EXT.
           EXIT.
      **************************************************************
      *    収納マスター読込（主キー）
      **************************************************************
       900-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *************************************************************
      *    収納合計マスター検索処理
      *************************************************************
       900-SYUTOTAL-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUTTL
           INITIALIZE                       SYUTOTAL-REC
           MOVE    SPA-HOSPNUM         TO  SYUTTL-HOSPNUM
           MOVE    SS016-NYUGAIKBN   TO  SYUTTL-NYUGAIKBN
           MOVE    SS016-PTID        TO  SYUTTL-PTID
           MOVE    SS016-GRP-DENPNUM TO  SYUTTL-DENPNUM
           MOVE    SYUTOTAL-REC        TO  MCPDATA-REC
           MOVE    "tbl_syutotal"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUTOTAL-REC
           ELSE
               MOVE    1               TO  FLG-SYUTTL
               INITIALIZE                  SYUTOTAL-REC
           END-IF
      *
           MOVE    "tbl_syutotal"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUTOTAL-KEY-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納明細検索処理
      **************************************************************
       900-SYUMEI-KEY7-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUMEI
      *
           INITIALIZE                      SYUMEI-REC
      *
           MOVE    SPA-HOSPNUM         TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN
                                       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUMEI-KEY7-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納明細検索処理
      **************************************************************
       900-SYUMEI-KEY2-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUMEI
      *
           INITIALIZE                      SYUMEI-REC
      *
           MOVE    SPA-HOSPNUM         TO  SMEI-HOSPNUM
           MOVE    SS016-NYUGAIKBN     TO  SMEI-NYUGAIKBN
           MOVE    SS016-PTID          TO  SMEI-PTID
           MOVE    SS016-DENPNUM       TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEI-KEY2-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納明細FETCH処理
      **************************************************************
       900-SYUMEI-KEY2-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUMEI
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEI-KEY2-FET-EXT.
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
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
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
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢインサート処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *************************************************************
      *    トランザクションキャンセル処理
      *************************************************************
       910-DBABORT-SEC               SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "abort"         TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
      *
           .
       910-DBABORT-EXT.
           EXIT.
      *****************************************************************
      *    EXIT PROGRAM
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
