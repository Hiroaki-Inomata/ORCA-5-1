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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGSSUB06.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 収納
      *  コンポーネント名  : 印刷リクエストチェック
      *                      将来的にORCGSAPI01S04のチェック部分を吸収すること
      *  管理者            : 
      *  作成日付    作業者        記述
      *  19/07/10    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *****************************************************************
      *
       ENVIRONMENT                         DIVISION.
       CONFIGURATION                       SECTION.
       INPUT-OUTPUT                        SECTION.
       FILE-CONTROL.
      *
       DATA                                DIVISION.
      *FILE                                SECTION.
      *
      *
       WORKING-STORAGE                     SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYUNOU                  PIC 9(01).
           03  FLG-SYUMEI                  PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-PTRSIINF                PIC 9(01).
           03  FLG-HIT                     PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-ERRCD                   PIC X(04).
           03  WRK-SYMD.
               05  WRK-SYY                 PIC 9(04).
               05  WRK-SMM                 PIC 9(02).
               05  WRK-SDD                 PIC 9(02).
           03  WRK-DAY-X                   PIC X(02).
           03  WRK-DAY                     PIC 9(02).
           03  WRK-EDTYMD1                 PIC X(09).
           03  WRK-DATE.
               05  WRK-DATE-YY             PIC 9(04).
               05  WRK-DATE-FL1            PIC X(01).
               05  WRK-DATE-MM             PIC 9(02).
               05  WRK-DATE-FL2            PIC X(01).
               05  WRK-DATE-DD             PIC 9(02).
           03  WRK-EOMYMD.
               05  WRK-EOMYM               PIC X(06).
               05  WRK-EOMDD               PIC 9(02).
           03  WRK-STYMD.
               05  WRK-STYM                PIC X(06).
               05  WRK-STDD                PIC 9(02).
           03  WRK-EDYMD.
               05  WRK-EDYM                PIC X(06).
               05  WRK-EDDD                PIC 9(02).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-MODE-G.
               05  CONST-MODE-REPRINT          PIC X(02)   VALUE "09".
               05  CONST-MODE-MONTHLY-PRINT    PIC X(02)   VALUE "10".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1039.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY    "CPSSUB06.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SSUB06-AREA
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SSUB06-OUT-AREA
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                  SYS-1039-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1039-HOSPNUM
           MOVE    "1039"          TO  SYS-1039-KANRICD
           MOVE    "*"             TO  SYS-1039-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1039-STYUKYMD
                                       SYS-1039-EDYUKYMD
           MOVE    SYS-1039-REC    TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1039-REC
      *
           IF    ( SYS-1039-TUKIGRPKBN =   SPACE )
               MOVE    "4"         TO  SYS-1039-TUKIGRPKBN
           END-IF
      *
           IF    ( SYS-1039-TUKISUMKBN =   SPACE )
               MOVE    "0"         TO  SYS-1039-TUKISUMKBN
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2002-KANREN-CHK-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    入力項目チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC              SECTION.
      *
           IF    ( SSUB06-REQMODE  =   CONST-MODE-REPRINT )
               PERFORM 20011-INPUT-CHK-SEC
           ELSE
               PERFORM 20011-TUKI-INPUT-CHK-SEC
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    通常印刷チェック処理
      *****************************************************************
       20011-INPUT-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-NYUGAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-DENPNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SSUB06-DENPEDANUM  NOT =   SPACE )
                   PERFORM 2001-DENPEDANUM-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-SKYPRT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-SRYPRT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-SYUNOU-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SSUB06-OUT-DENPEDANUM  NOT =   ZERO )
                   PERFORM 2001-SYUMEI-CHK-SEC
               END-IF
           END-IF
      *
           .
       20011-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    月別印刷チェック処理
      *****************************************************************
       20011-TUKI-INPUT-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-SRYYM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-KIKAN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-SKYPRT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-SKYGRP-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-SRYPRT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2002-SYUNOU-CHK-SEC
           END-IF
      *
           .
       20011-TUKI-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療年月チェック処理
      *****************************************************************
       2001-SRYYM-CHK-SEC              SECTION.
      *
           IF    ( SSUB06-SRYYM    =   SPACE )
               MOVE    "0130"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
      *
               MOVE    SSUB06-SRYYM    TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               MOVE    "01"    TO  WRK-SYMD(7:)
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0033"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD(1:6)
                                       TO  SSUB06-OUT-SRYYM
               END-IF
           END-IF
      *
           .
       2001-SRYYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    伝票番号チェック
      *****************************************************************
       2001-DENPNUM-CHK-SEC            SECTION.
      *
           IF    ( SSUB06-DENPNUM =   SPACE )
               MOVE    "0100"          TO  SPA-ERRCD
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE    SSUB06-DENPNUM
                                       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF  ( SNUM-RC       NOT =   ZERO  )
                OR ( SNUM-MINKBN   NOT =   SPACE )
                OR ( SNUM-SYOKBN   NOT =   SPACE )
                   MOVE    "0101"      TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM TO  SSUB06-OUT-DENPNUM
               END-IF
           END-IF
      *
           .
       2001-DENPNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    明細番号チェック
      *****************************************************************
       2001-DENPEDANUM-CHK-SEC             SECTION.
      *
           INITIALIZE                  ORCSNUMAREA
           MOVE    SSUB06-DENPEDANUM
                                   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF  ( SNUM-RC       NOT =   ZERO  )
            OR ( SNUM-MINKBN   NOT =   SPACE )
            OR ( SNUM-SYOKBN   NOT =   SPACE )
            OR ( SNUM-OUTNUM       =   ZERO )
               MOVE    "3005"      TO  SPA-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM TO  SSUB06-OUT-DENPEDANUM
           END-IF
      *
           .
       2001-DENPEDANUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入外区分チェック
      *****************************************************************
       2001-NYUGAIKBN-CHK-SEC          SECTION.
      *
           IF    ( SSUB06-NYUGAIKBN    =   SPACE )
               MOVE    "0086"      TO  SPA-ERRCD
           ELSE
               EVALUATE    FUNCTION
                             UPPER-CASE(SSUB06-NYUGAIKBN)
               WHEN  "I"
                   MOVE    "1"     TO  SPA-NYUGAIKBN
               WHEN  "O"
                   MOVE    "2"     TO  SPA-NYUGAIKBN
               WHEN  OTHER
                    MOVE    "0034" TO  SPA-ERRCD
               END-EVALUATE
           END-IF
      *
           .
       2001-NYUGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    期間指定チェック
      *****************************************************************
       2001-KIKAN-CHK-SEC             SECTION.
      *
           IF    ( SSUB06-STDAY    =    SPACE )
            AND  ( SSUB06-EDDAY    =    SPACE )
               CONTINUE
           ELSE
      *
               MOVE    SSUB06-OUT-SRYYM    TO  WRK-SYMD
               PERFORM 800-GETUMATU-SEC
               MOVE    WRK-SYMD            TO  WRK-EOMYMD
      *
               MOVE    "1"                 TO  SSUB06-OUT-KIKANKBN
      *
               IF    ( SSUB06-STDAY   =   SPACE )
                   MOVE    1               TO  SSUB06-OUT-STDAY
               ELSE
                   MOVE    "0125"          TO  WRK-ERRCD
                   MOVE    SSUB06-STDAY
                                           TO  WRK-DAY-X
                   PERFORM 20011-KIKAN-CHK-SEC
                   MOVE    WRK-DAY         TO  SSUB06-OUT-STDAY
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( SSUB06-EDDAY = SPACE )
                       MOVE    WRK-EOMDD   TO  SSUB06-OUT-EDDAY
                   ELSE
                       MOVE    "0126"      TO  WRK-ERRCD
                       MOVE    SSUB06-EDDAY
                                           TO  WRK-DAY-X
                       PERFORM 20011-KIKAN-CHK-SEC
                       MOVE    WRK-DAY     TO  SSUB06-OUT-EDDAY
                   END-IF
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( SSUB06-OUT-STDAY   >   SSUB06-OUT-EDDAY )
                       MOVE    "0127"      TO  SPA-ERRCD
                   END-IF
               END-IF
      *
           END-IF
      *
           .
       2001-KIKAN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    期間指定チェック
      *****************************************************************
       20011-KIKAN-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO   WRK-DAY
      *
           IF    ( FUNCTION TRIM(WRK-DAY-X)
                                       IS NOT NUMERIC )
               MOVE    WRK-ERRCD       TO   SPA-ERRCD
           ELSE
               MOVE    FUNCTION NUMVAL(WRK-DAY-X)
                                       TO   WRK-DAY
               IF    ( WRK-DAY    >=   1 )
                AND  ( WRK-DAY    <=   WRK-EOMDD )
                   CONTINUE
               ELSE
                   MOVE    WRK-ERRCD   TO   SPA-ERRCD
               END-IF
           END-IF
      *
           .
       20011-KIKAN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書印刷区分チェック
      *****************************************************************
       2001-SKYPRT-CHK-SEC             SECTION.
      *
           IF    ( SSUB06-SKYPRT   =   SPACE )
               MOVE    "0"         TO  SSUB06-OUT-SKYPRT
           ELSE
               IF    ( SSUB06-SKYPRT  =   "0" OR "1" )
                   MOVE    SSUB06-SKYPRT
                                   TO  SSUB06-OUT-SKYPRT
               ELSE
                   MOVE    "0121"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-SKYPRT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書発行方法チェック
      *****************************************************************
       2001-SKYGRP-CHK-SEC         SECTION.
      *
           IF    ( SSUB06-SKYGRP   =   SPACE )
               MOVE    SYS-1039-TUKIGRPKBN
                                   TO  SSUB06-OUT-SKYGRP
           ELSE
               IF    ( SSUB06-SKYGRP  =   "1" OR "2" OR "3" OR "4")
                   MOVE    SSUB06-SKYGRP
                                   TO  SSUB06-OUT-SKYGRP
               ELSE
                   MOVE    "0129"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-SKYGRP-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療費明細書印刷区分チェック
      *****************************************************************
       2001-SRYPRT-CHK-SEC             SECTION.
      *
           IF    ( SSUB06-SRYPRT       =   SPACE )
               MOVE    "0"     TO      SSUB06-OUT-SRYPRT
           ELSE
               IF    ( SSUB06-SRYPRT   =   "0" OR "1" )
                   MOVE    SSUB06-SRYPRT
                                   TO  SSUB06-OUT-SRYPRT
               ELSE
                   MOVE    "0122"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SSUB06-OUT-SKYPRT      =   "0" )
            AND  ( SSUB06-OUT-SRYPRT      =   "0" )
               MOVE    "0123"          TO  SPA-ERRCD
           END-IF
      *
           .
       2001-SRYPRT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    収納チェック
      *****************************************************************
       2001-SYUNOU-CHK-SEC             SECTION.
      *
            INITIALIZE                     SYUNOU-REC
            MOVE    SPA-HOSPNUM        TO  SYU-HOSPNUM
            MOVE    SPA-PTID           TO  SYU-PTID
            MOVE    SPA-NYUGAIKBN      TO  SYU-NYUGAIKBN
            MOVE    SSUB06-OUT-DENPNUM    TO  SYU-DENPNUM
            PERFORM 900-SYUNOU-KEY-SEL-SEC
            IF    ( FLG-SYUNOU         =   ZERO )
                MOVE   SYU-SRYYMD      TO  SPA-SRYYMD
                IF  (  SYU-SYUHKNNUM   =   "971" OR "973" )
                    MOVE    1          TO  SPA-HKNKBN
                    PERFORM 900-HKNCOMBI-KEY-SEL-SEC
                    PERFORM 900-PTRSIINF-KEY-SEL-SEC
                    MOVE    PTRSI-HKNKBN
                                       TO  SPA-RSI-HKNKBN
                ELSE
                    MOVE    ZERO       TO  SPA-HKNKBN
                END-IF
      *
                IF   ( SSUB06-OUT-DENPEDANUM  =   ZERO )
                 AND ( SYU-CREATEKBN       =   "3" )
                    MOVE    "0085"     TO  SPA-ERRCD
                END-IF
            ELSE
                MOVE    "0085"         TO  SPA-ERRCD
            END-IF
      *
           .
       2001-SYUNOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    収納チェック
      *****************************************************************
       2002-SYUNOU-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-HIT
      *
           PERFORM 900-SYUNOU-KEY73-SEL-SEC
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( FLG-SYUNOU  NOT =   ZERO )
                    OR   ( FLG-HIT         =   1 )
      *
               MOVE    1               TO  FLG-HIT
               MOVE    SYU-NYUGAIKBN   TO  SPA-NYUGAIKBN
               MOVE    SYU-DENPNUM     TO  SSUB06-OUT-DENPNUM
               MOVE    SYU-SRYYMD      TO  SPA-SRYYMD
      *
               IF    ( SSUB06-OUT-KIKANKBN    =   "1" )
                   MOVE    SSUB06-OUT-SRYYM   TO  WRK-STYM
                                               WRK-EDYM
                   MOVE    SSUB06-OUT-STDAY   TO  WRK-STDD
                   MOVE    SSUB06-OUT-EDDAY   TO  WRK-EDDD
                   IF    ( SYU-SRYYMD      >=  WRK-STYMD )
                    AND  ( SYU-SRYYMD      <=  WRK-EDYMD )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-HIT
                   END-IF
               END-IF
      *
               IF    ( SYS-1039-TUKISUMKBN =   "1" )
                   IF    ( SYU-SKYMONEY    >   SYU-NYUKIN-TOTAL )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-HIT
                   END-IF
               END-IF
      *
               PERFORM 900-SYUNOU-KEY73-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key73"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-HIT =   ZERO )
               MOVE    "0128"          TO  SPA-ERRCD
           END-IF
      *
           .
       2002-SYUNOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    収納明細チェック
      *****************************************************************
       2001-SYUMEI-CHK-SEC            SECTION.
      *
           INITIALIZE                     SYUMEI-REC
           MOVE    SPA-HOSPNUM        TO  SMEI-HOSPNUM
           MOVE    SPA-PTID           TO  SMEI-PTID
           MOVE    SPA-NYUGAIKBN      TO  SMEI-NYUGAIKBN
           MOVE    SSUB06-OUT-DENPNUM TO  SMEI-DENPNUM
           MOVE    SSUB06-OUT-DENPEDANUM
                                      TO  SMEI-DENPEDANUM
           PERFORM 900-SYUMEI-KEY-SEL-SEC
           IF    ( FLG-SYUMEI     NOT =   ZERO )
               MOVE    "0116"         TO  SPA-ERRCD
           END-IF
      *
           .
       2001-SYUMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       2002-KANREN-CHK-SEC             SECTION.
      *
           .
       2002-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           IF  ( WRK-SYMD          =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD    TO  WRK-EDTYMD1
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
      *    システム管理マスタ検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNCOMBI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者労災情報検索処理
      *****************************************************************
       900-PTRSIINF-KEY-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-PTRSIINF
      *
           INITIALIZE                      PTRSIINF-REC
           MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
           MOVE    SPA-PTID            TO  PTRSI-PTID
           MOVE    COMB-HKNID          TO  PTRSI-HKNID
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
               INITIALIZE                  PTRSIINF-REC
           END-IF
     *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTRSIINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納検索処理
      *****************************************************************
       900-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"       TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納マスター検索処理
      **************************************************************
       900-SYUNOU-KEY73-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    "2"                 TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    SSUB06-OUT-SRYYM    TO  SYU-SRYYMD
           MOVE    "%"                 TO  SYU-SRYYMD(7:1)
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key73"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY73-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納マスターFETCH処理
      **************************************************************
       900-SYUNOU-KEY73-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key73"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY73-FET-EXT.
           EXIT.
      *************************************************************
      *    収納履歴検索処理
      *************************************************************
       900-SYUMEI-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUMEI
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
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
           .
       900-SYUMEI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
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
       910-DB-SELECT-EXT.
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
