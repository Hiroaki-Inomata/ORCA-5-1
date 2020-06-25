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
       PROGRAM-ID.         ORCGSAPI01S04.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 収納（印刷）
      *  管理者            :
      *  作成日付    作業者        記述
      *  16/10/31    NACL−太田　新規作成
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
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *
       01  FLG-AREA.
           03  FLG-PTINF               PIC 9(01).
           03  FLG-SYUNOU              PIC 9(01).
           03  FLG-SYUMEI              PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-PTRSIINF            PIC 9(01).
           03  FLG-ENTRYCHK            PIC 9(01).
           03  FLG-LOCK                PIC 9(01).
           03  FLG-PARA                PIC 9(01).
           03  FLG-HIT                 PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXD                    PIC 9(05).
           03  IDXL                    PIC 9(05).
      *
       01  CNT-AREA.
           03  CNT-APIPARA             PIC 9(07).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-TERM                PIC X(64).
           03  WRK-ERRCD               PIC X(04).
           03  WRK-WINDOW              PIC X(64).
           03  WRK-MOTOPG              PIC X(08).
           03  WRK-YMD                 PIC X(10). 
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-KEISANYMD           PIC X(08).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
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
           03  WRK-TABLE               PIC X(64).
           03  WRK-PATHNAME            PIC X(64).
           03  WRK-GYOUMUID            PIC X(03).
           03  WRK-UID                 PIC X(36).
           03  WRK-FILEMEI.
               05  WRK-FILEMEI-YMD     PIC X(08).
               05  WRK-FILEMEI-HMS     PIC X(06).
               05  WRK-FILEMEI-CD      PIC X(06).
           03  WRK-PAGE-CNT            PIC 9(06).
           03  WRK-DAY-X               PIC X(02).
           03  WRK-DAY                 PIC 9(02).
           03  WRK-EOMYMD.
               05  WRK-EOMYM           PIC X(06).
               05  WRK-EOMDD           PIC 9(02).
           03  WRK-STYMD.
               05  WRK-STYM                PIC X(06).
               05  WRK-STDD                PIC 9(02).
           03  WRK-EDYMD.
               05  WRK-EDYM                PIC X(06).
               05  WRK-EDDD                PIC 9(02).
           03  WRK-PRINT-G.
               05  WRK-SEIKYU-PG           PIC X(20).
               05  WRK-MEISAI-PG           PIC X(20).
               05  WRK-SEIKYU-N-PG         PIC X(20).
               05  WRK-MEISAI-N-PG         PIC X(20).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-MODE-G.
               05  CONST-MODE-REPRINT          PIC X(02)   VALUE "09".
               05  CONST-MODE-MONTHLY-PRINT    PIC X(02)   VALUE "10".
      *
           COPY    "SAPI01SCR-SPA.INC".
      *
           COPY    "S01COMMON-SPA".
      *
       01  TSYU-REC.
           COPY    "CPSYUNOU.INC"  REPLACING //SYU-//
                                   BY        //TSYU-//.
      *
           COPY    "CPRPKEY.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    オンライン帳票ＩＤ連絡
           COPY    "CPORCSRPLNK.INC".
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
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
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    API XML用定義体
           COPY    "CPAPIV3REQ.INC".
           COPY    "CPINCOMEV3RES03.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    全角チェックパラメタ(拡張文字対応版)
           COPY    "CPORCSKANACHK2.INC".
      *
      *    排他制御
           COPY    "CPORCSLOCK.INC".
      *
      *    請求書印刷パラメタ
           COPY    "CPORCHC03.INC".
           COPY    "CPORCHCN03.INC".
      *
      *    入院請求書兼領収書まとめサブ
           COPY    "CPORCHCN03S02.INC".
      *
      *    明細書印刷パラメタ
           COPY    "CPORCHC04.INC".
           COPY    "CPORCHCN04.INC".
      *
      *    請求書・明細書編集サブ
           COPY    "CPSSUB03.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *
       01  UIDIO-AREA.
           03  C-RET                   PIC X(2).
           03  C-UID                   PIC X(36).
       01  UIDIO-ST                    PIC 9(2).
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
           COPY    "CPSK1010.INC".
           COPY    "CPSK1039.INC".
      *    COPY    "CPSK4001.INC".
      *
      *    ＡＰＩパラメタ
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
           COPY    "CPPARA023.INC".
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
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
           COPY    "CPSYUPARA01.INC".
           COPY    "CPSYUPARA02.INC".
           COPY    "CPSYUPARA03.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
       01  SCRAREA.
           COPY    "ORCA23SCRAREA.INC".
      *
           COPY    "CPINCOMEV3REQ01.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
                                               SCRAREA
                                               INCOMEREQ01-AREA
                                               SPA-AREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-API-AREA
           INITIALIZE                  SPA-S01KYOUTU
           INITIALIZE                  TSYU-REC
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  INCOMERES03-AREA
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
           MOVE    "01"            TO  SPA-API-RESNUM
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2000-PTNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-STATE-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    "02"        TO  SPA-API-RESNUM
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2002-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2003-RESPONSE-EDIT-SEC
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
           IF    ( INCOMEREQ01-REQMODE =   CONST-MODE-REPRINT )
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
               IF    ( INCOMEREQ01-DENPEDANUM  NOT =   SPACE )
                   PERFORM 2001-DENPEDANUM-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-PRINT-MODE-CHK-SEC
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
               IF    ( SPA-API-DENPEDANUM  NOT =   ZERO )
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
               PERFORM 2001-PRINT-MODE-CHK-SEC
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
      *    患者番号チェック処理
      *****************************************************************
       2000-PTNUM-CHK-SEC              SECTION.
      *
           IF    ( INCOMEREQ01-PTNUM =   SPACE )
               MOVE    "0071"          TO  SPA-ERRCD
           ELSE
               INITIALIZE                  ORCSPTIDAREA
               MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
               MOVE    INCOMEREQ01-PTNUM TO  SPTID-PTNUM
               CALL    "ORCSPTID"      USING
                                       ORCSPTIDAREA
                                       SPA-AREA
               IF    ( SPTID-RC        =   ZERO )
                   MOVE    SPTID-PTNUM TO  SPA-PTNUM
                   MOVE    SPTID-PTID  TO  SPA-PTID
                   PERFORM 900-PTINF-KEY-SEL-SEC
                   IF    ( FLG-PTINF   = ZERO )
                       MOVE    PTINF-NAME      TO  SPA-NAME
                       MOVE    PTINF-KANANAME  TO  SPA-KANANAME
                   ELSE
                       MOVE    "0072"  TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    "0072"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2000-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    状態確認処理
      *****************************************************************
       2001-STATE-CHK-SEC              SECTION.
      *
           IF    ( INCOMEREQ01-ORCA-UID   =    SPACE )
               MOVE    "0205"              TO  SPA-ERRCD
           END-IF
      *
           IF    ( INCOMEREQ01-KARTE-UID   =   SPACE )
               MOVE    "0203"              TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 900-APIPARA-KEY5-SEL-SEC
               MOVE    APIPARA-DATA-REC    TO  PARA023-REC
               IF    ( FLG-APIPARA         =   ZERO )
                   EVALUATE    TRUE
                   WHEN  ( PARA023-STATE       <   "02" )
                   WHEN  ( PARA023-PTID        =   ZERO )
                       MOVE    "0210"      TO  SPA-ERRCD
                   WHEN  ( PARA023-PTID    NOT =   SPA-PTID )
                       MOVE    "0209"      TO  SPA-ERRCD
                   END-EVALUATE
               ELSE
                   MOVE    "0206"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 900-APIPARA-KEY7-SEL-SEC
               MOVE    APIPARA-DATA-REC    TO  PARA023-REC
               IF    ( FLG-APIPARA         =   ZERO )
                   MOVE    "1038"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-STATE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療年月チェック処理
      *****************************************************************
       2001-SRYYM-CHK-SEC              SECTION.
      *
           IF    ( INCOMEREQ01-SRYYM   =   SPACE )
               MOVE    "0130"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD    =      SPACE )
      *
               MOVE    INCOMEREQ01-SRYYM
                                       TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               MOVE    "01"    TO  WRK-SYMD(7:)
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0033"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD(1:6)
                                       TO  SPA-API-SRYYM
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
           IF    ( INCOMEREQ01-DENPNUM =   SPACE )
               MOVE    "0100"          TO  SPA-ERRCD
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE    INCOMEREQ01-DENPNUM
                                       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF  ( SNUM-RC       NOT =   ZERO  )
                OR ( SNUM-MINKBN   NOT =   SPACE )
                OR ( SNUM-SYOKBN   NOT =   SPACE )
                   MOVE    "0101"      TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM TO  SPA-API-DENPNUM
               END-IF
           END-IF
      *
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
           MOVE    INCOMEREQ01-DENPEDANUM
                                   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF  ( SNUM-RC       NOT =   ZERO  )
            OR ( SNUM-MINKBN   NOT =   SPACE )
            OR ( SNUM-SYOKBN   NOT =   SPACE )
            OR ( SNUM-OUTNUM       =   ZERO )
               MOVE    "3005"      TO  SPA-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM TO  SPA-API-DENPEDANUM
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
           IF    ( INCOMEREQ01-NYUGAIKBN   =   SPACE )
               MOVE    "0086"      TO  SPA-ERRCD
           ELSE
               EVALUATE    FUNCTION
                             UPPER-CASE(INCOMEREQ01-NYUGAIKBN)
               WHEN  "I"
                   MOVE    "1"     TO  SPA-NYUGAIKBN
               WHEN  "O"
                   MOVE    "2"     TO  SPA-NYUGAIKBN
               WHEN  OTHER
                    MOVE    "0034"  TO  SPA-ERRCD
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
           IF    ( INCOMEREQ01-STDAY  =    SPACE )
            AND  ( INCOMEREQ01-EDDAY  =    SPACE )
               CONTINUE
           ELSE
      *
               MOVE    SPA-API-SRYYM       TO  WRK-SYMD
               PERFORM 800-GETUMATU-SEC
               MOVE    WRK-SYMD            TO  WRK-EOMYMD
      *
               MOVE    "1"                 TO  SPA-API-KIKANKBN
      *
               IF    ( INCOMEREQ01-STDAY   =   SPACE )
                   MOVE    1               TO  SPA-API-STDAY
               ELSE
                   MOVE    "0125"          TO  WRK-ERRCD
                   MOVE    INCOMEREQ01-STDAY
                                           TO  WRK-DAY-X
                   PERFORM 20011-KIKAN-CHK-SEC
                   MOVE    WRK-DAY         TO  SPA-API-STDAY
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( INCOMEREQ01-EDDAY = SPACE )
                       MOVE    WRK-EOMDD   TO  SPA-API-EDDAY
                   ELSE
                       MOVE    "0126"      TO  WRK-ERRCD
                       MOVE    INCOMEREQ01-EDDAY
                                           TO  WRK-DAY-X
                       PERFORM 20011-KIKAN-CHK-SEC
                       MOVE    WRK-DAY     TO  SPA-API-EDDAY
                   END-IF
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( SPA-API-STDAY   >   SPA-API-EDDAY )
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
      *    印刷モードチェック
      *****************************************************************
       2001-PRINT-MODE-CHK-SEC         SECTION.
      *
      D    DISPLAY "INCOMEREQ01-PRINT-MODE=" INCOMEREQ01-PRINT-MODE
      *
           MOVE    FUNCTION UPPER-CASE(INCOMEREQ01-PRINT-MODE)
                                       TO  SPA-API-PRINT-MODE
           EVALUATE    SPA-API-PRINT-MODE
           WHEN    SPACE
               CONTINUE
           WHEN    "PDF"
               INITIALIZE                  SRPLNK-AREA
               MOVE    "CHK_MONBLOB"   TO  SRPLNK-MODE
               CALL    "ORCSRPLNK"         USING
                                           SRPLNK-AREA
                                           SPA-AREA
               IF    ( SRPLNK-RC       NOT =   ZERO )
                   MOVE    "0132"      TO  SPA-ERRCD
               END-IF
           WHEN    OTHER
               MOVE    "0131"          TO  SPA-ERRCD
           END-EVALUATE
           .
       2001-PRINT-MODE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書印刷区分チェック
      *****************************************************************
       2001-SKYPRT-CHK-SEC             SECTION.
      *
           IF    ( INCOMEREQ01-SKYPRT  =   SPACE )
               MOVE    "0"     TO      SPA-API-SKYPRT
           ELSE
               IF    ( INCOMEREQ01-SKYPRT  =   "0" OR "1" )
                   MOVE    INCOMEREQ01-SKYPRT
                                   TO  SPA-API-SKYPRT
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
       2001-SKYGRP-CHK-SEC             SECTION.
      *
           IF    ( INCOMEREQ01-SKYGRP  =   SPACE )
               MOVE    SYS-1039-TUKIGRPKBN
                                   TO  SPA-API-SKYGRP
           ELSE
               IF    ( INCOMEREQ01-SKYGRP  =   "1" OR "2" OR "3" OR "4")
                   MOVE    INCOMEREQ01-SKYGRP
                                   TO  SPA-API-SKYGRP
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
           IF    ( INCOMEREQ01-SRYPRT  =   SPACE )
               MOVE    "0"     TO      SPA-API-SRYPRT
           ELSE
               IF    ( INCOMEREQ01-SRYPRT  =   "0" OR "1" )
                   MOVE    INCOMEREQ01-SRYPRT
                                   TO  SPA-API-SRYPRT
               ELSE
                   MOVE    "0122"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-API-SKYPRT      =   "0" )
            AND  ( SPA-API-SRYPRT      =   "0" )
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
            MOVE    SPA-API-DENPNUM    TO  SYU-DENPNUM
            PERFORM 900-SYUNOU-KEY-SEL-SEC
            IF    ( FLG-SYUNOU         =   ZERO )
                MOVE   SYUNOU-REC      TO  TSYU-REC
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
                IF   ( SPA-API-DENPEDANUM  =   ZERO )
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
               MOVE    SYU-DENPNUM     TO  SPA-API-DENPNUM
               MOVE    SYU-SRYYMD      TO  SPA-SRYYMD
      *
               IF    ( SPA-API-KIKANKBN    =   "1" )
                   MOVE    SPA-API-SRYYM   TO  WRK-STYM
                                               WRK-EDYM
                   MOVE    SPA-API-STDAY   TO  WRK-STDD
                   MOVE    SPA-API-EDDAY   TO  WRK-EDDD
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
           MOVE    SPA-API-DENPNUM    TO  SMEI-DENPNUM
           MOVE    SPA-API-DENPEDANUM TO  SMEI-DENPEDANUM
           PERFORM 900-SYUMEI-KEY-SEL-SEC
           IF    ( FLG-SYUMEI     NOT =   ZERO )
               MOVE    "0116"         TO  SPA-ERRCD
           END-IF
      *
           .
       2001-SYUMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    収納終了処理
      *****************************************************************
       20011-TERMINATE-SEC             SECTION.
      *
           COMPUTE CNT-APIPARA =       CNT-APIPARA +   1
      *
           CALL    "ORCGSSUB04"        USING
                                       APIPARA-REC
                                       SPA-AREA
      *
           .
       20011-TERMINATE-EXT.
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
      *    検索処理編集処理
      *****************************************************************
       2003-RESPONSE-EDIT-SEC          SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20031-DB-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20031-PRINT-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20031-RESPONSE-SEC
           END-IF
      *
           .
       2003-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    データベース更新処理
      *****************************************************************
       20031-DB-SEC                    SECTION.
      *
      *
           .
       20031-DB-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       20031-PRINT-SEC                     SECTION.
      *
           MOVE     1                  TO  SPA-API-FLG
           IF    ( SPA-API-PRINT-MODE  =   "PDF" )
               MOVE    "2"             TO  SPA-PRTCONF
           END-IF
      *
           CALL    "ORCSONPRTSET"      USING
                                       SPA-AREA
      *
           INITIALIZE                  ORCSPRTCTRLAREA
           CALL    "ORCSPRTCTRL"       USING
                                       SPA-AREA
                                       ORCSPRTCTRLAREA
                                       MCPAREA
           MOVE    "00"            TO  SPA-PRT-FLG
           INITIALIZE                  SRPLNK-AREA
           MOVE    "INS"           TO  SRPLNK-MODE
           MOVE    RPKEY-COMMON    TO  SRPLNK-VAL
           MOVE    INCOMEREQ01-KARTE-UID
                                   TO  SRPLNK-KARTE-UID
           MOVE    INCOMEREQ01-ORCA-UID
                                   TO  SRPLNK-ORCA-UID
           CALL    "ORCSRPLNK"         USING
                                       SRPLNK-AREA
                                       SPA-AREA
      *
           PERFORM 2800-PRINT-PG-SEC
      *
           PERFORM 2800-SPAKYOTUKEY-SET-SEC
      *
           IF    ( INCOMEREQ01-REQMODE     =   CONST-MODE-REPRINT )
               IF    ( SPA-API-DENPEDANUM      =   ZERO )
                   PERFORM 200311-PRINT-SEC
               ELSE
                   PERFORM 200312-PRINT-SEC
               END-IF
           ELSE
               PERFORM 200313-PRINT-SEC
           END-IF
      *
      *    印刷ダミー処理
           MOVE    "99"                TO  SPA-PRT-FLG
           CALL    "ORCHCDUMMY"        USING   SPA-AREA
      *
           .
       20031-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       200311-PRINT-SEC                    SECTION.
      *
           IF    ( SPA-NYUGAIKBN       =   "1" )
               PERFORM 2003111-NYU-REPRINT-SEC
           ELSE
               PERFORM 2003111-GAI-REPRINT-SEC
           END-IF
      *
           .
       200311-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       2003111-NYU-REPRINT-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    SPA-API-DENPNUM     TO  SYU-DENPNUM
           PERFORM 900-SYUNOU-KEY-SEL-SEC
           IF   (  FLG-SYUNOU          =   ZERO )
      *
               INITIALIZE      ORCHCN03AREA
      *
               MOVE    SPA-HOSPNUM     TO  ORCHCN03-HEAD-HOSPNUM
               MOVE    SPA-STAFFCD     TO  ORCHCN03-STAFFCD
               MOVE    SPA-SYSYMD      TO  ORCHCN03-SYSYMD
               MOVE    SPA-TERMID      TO  ORCHCN03-TERMID
               MOVE    1               TO  ORCHCN03-CNT
      *
               MOVE    SPA-HOSPNUM     TO  ORCHCN03-HOSPNUM (1)
               MOVE    SPA-PTID        TO  ORCHCN03-PTID   (1)
               MOVE    SPA-PTNUM       TO  ORCHCN03-PTNUM  (1)
               MOVE    "1"             TO  ORCHCN03-NYUGAIKBN (1)
      *
               MOVE    SYU-SKYSTYMD    TO  ORCHCN03-KJNYMD  (1)
               MOVE    SYU-DENPNUM     TO  ORCHCN03-DENPNUM (1)
               MOVE    SYU-SKYMONEY-TAX-SAI
                                       TO  ORCHCN03-SEIKYU-TAX-SAI
                                                                (1)
               MOVE    SYU-SKYMONEY    TO  ORCHCN03-SEIKYU  (1)
               MOVE    SYU-NYUKIN-TOTAL
                                       TO  ORCHCN03-NYUKIN  (1)
               MOVE    1               TO  ORCHCN03-HAKKOFLG(1)
               MOVE    1               TO  ORCHCN03-MOTOID
      *
               IF    ( SPA-API-SKYPRT  =   "1" )
      *
                   MOVE    "03"            TO  SPA-PRT-FLG
                   INITIALIZE                  SRPLNK-AREA
                   MOVE    "INS"           TO  SRPLNK-MODE
                   MOVE    RPKEY-SEIKYUSHO-N
                                           TO  SRPLNK-VAL
                   CALL    "ORCSRPLNK"         USING
                                               SRPLNK-AREA
                                               SPA-AREA
      *
                   CALL    WRK-SEIKYU-N-PG USING
                                           ORCHCN03AREA
                                           SPA-AREA
                   MOVE    SPACE           TO  SPA-PRT-FLG
               END-IF
      *
               IF    ( SPA-API-SRYPRT  =   "1" )
                   PERFORM 2800-NYU-SRYPRT-SEC
               END-IF
      *
           END-IF
      *
           .
       2003111-NYU-REPRINT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       2003111-GAI-REPRINT-SEC             SECTION.
      *
           MOVE    SPA-S01-FILEMEI TO  WRK-FILEMEI
      *
           MOVE    "001%"          TO  WRK-FILEMEI-CD
           PERFORM 900-DELETE-PARA-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *
               CALL    "ORCGSSUB01"    USING
                                       SPA-AREA
                                       SPA-S01KYOUTU
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   PERFORM 20031111-GAI-SKYPRT-SEC
               END-IF
           END-IF
      *
           MOVE    "001%"          TO  WRK-FILEMEI-CD
           PERFORM 900-DELETE-PARA-SEC
      *
           .
       2003111-GAI-REPRINT-EXT.
           EXIT.
      **************************************************************
      *    外来再発行処理
      **************************************************************
       20031111-GAI-SKYPRT-SEC             SECTION.
      *
           MOVE    "00101"             TO  WRK-FILEMEI-CD
           PERFORM 900-PARA-KEY4-SEL-SEC
           MOVE    PARA-DATA-REC       TO  SYUPARA01-AREA
      *
           MOVE    "00102"             TO  WRK-FILEMEI-CD
      *
      D    DISPLAY "SPA-API-SKYPRT[" SPA-API-SKYPRT "]"
           IF    ( SPA-API-SKYPRT      =   "1" )
      D        DISPLAY "PRINT SEIKYUSHO"
               PERFORM 2800-GAI-SKYPRT-SEC
           END-IF
      *
           IF    ( SPA-API-SRYPRT      =   "1" )
               PERFORM 2800-GAI-SRYPRT-SEC
           END-IF
      *
           .
       20031111-GAI-SKYPRT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       200312-PRINT-SEC                    SECTION.
      *
           INITIALIZE                      SSUB03-AREA
           MOVE    2                   TO  SSUB03-REQNO
           MOVE    SPA-API-DENPEDANUM  TO  SSUB03-DENPEDANUM
           MOVE    SPA-API-SKYPRT      TO  SSUB03-SKYPRT
           MOVE    SPA-API-SRYPRT      TO  SSUB03-SRYPRT
           CALL    "ORCGSSUB03"        USING
                                       SPA-AREA
                                       SPA-S01KYOUTU
                                       SSUB03-AREA
      *
           .
       200312-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理（外来月別請求書兼領収書）
      *****************************************************************
       200313-PRINT-SEC                    SECTION.
      *
           MOVE    SPA-S01-FILEMEI TO  WRK-FILEMEI
      *
           MOVE    "001%"          TO  WRK-FILEMEI-CD
           PERFORM 900-DELETE-PARA-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *
               CALL    "ORCGSSUB01"    USING
                                       SPA-AREA
                                       SPA-S01KYOUTU
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   PERFORM 20031111-GAI-TUKISKYPRT-SEC
               END-IF
           END-IF
      *
           MOVE    "001%"          TO  WRK-FILEMEI-CD
           PERFORM 900-DELETE-PARA-SEC
      *
           .
       200313-PRINT-EXT.
           EXIT.
      **************************************************************
      *    外来再発行処理
      **************************************************************
       20031111-GAI-TUKISKYPRT-SEC         SECTION.
      *
           MOVE    "00101"             TO  WRK-FILEMEI-CD
           PERFORM 900-PARA-KEY4-SEL-SEC
           MOVE    PARA-DATA-REC       TO  SYUPARA01-AREA
      *
           MOVE    "00102"             TO  WRK-FILEMEI-CD
      *
           IF    ( SPA-API-SKYPRT      =   "1" )
               PERFORM 2800-GAI-TUKISKYPRT-SEC
           END-IF
      *
           IF    ( SPA-API-SRYPRT      =   "1" )
               PERFORM 2800-GAI-TUKISRYPRT-SEC
           END-IF
      *
           .
       20031111-GAI-TUKISKYPRT-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       20031-RESPONSE-SEC                  SECTION.
      *
           MOVE    SPA-PTNUM           TO  INCOMERES03-PTNUM
      *
           IF    ( SPA-API-PRINT-MODE  NOT =   SPACE )
            AND  ( SPA-PUTTYPE             =   "PRINTED" )
               MOVE    SPA-PRT-UUID    TO  INCOMERES03-DATA-ID
           END-IF
      *
           .
       20031-RESPONSE-EXT.
           EXIT.
      *****************************************************************
      *    診療費明細書出力処理
      *****************************************************************
       2800-NYU-SRYPRT-SEC             SECTION.
      *
           INITIALIZE                  ORCHCN03S02AREA
           MOVE    1       TO          ORCHCN03S02-IDX
           CALL    "ORCHCN03S02"   USING
                                   ORCHCN03AREA
                                   ORCHCN03S02AREA
                                   SYUNOU-REC
                                   SPA-AREA
           IF    ( ORCHCN03S02-PRTFLG  =   1 )
      *
               INITIALIZE                  ORCHCN04AREA
      *
               MOVE    SPA-PTID        TO  ORCHCN04-PTID
               MOVE    SPA-PTNUM       TO  ORCHCN04-PTNUM
               MOVE    SPA-NAME        TO  ORCHCN04-NAME
               MOVE    SYU-SKYSTYMD    TO  ORCHCN04-SKYSTYMD
               MOVE    SYU-SKYEDYMD    TO  ORCHCN04-SKYEDYMD
               MOVE    1               TO  ORCHCN04-HAKKOFLG
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   ORCHCN03S02-GRP-CNT )
      *
                   MOVE    ORCHCN03S02-GRP-DENPNUM (IDX0)
                                       TO  ORCHCN04-DENPNUM (IDX0)
      *
               END-PERFORM
      *
               MOVE    "04"            TO  SPA-PRT-FLG
               INITIALIZE                  SRPLNK-AREA
               MOVE    "INS"           TO  SRPLNK-MODE
               MOVE    RPKEY-MEISAISHO-N
                                       TO  SRPLNK-VAL
               CALL    "ORCSRPLNK"         USING
                                           SRPLNK-AREA
                                           SPA-AREA
      *
               CALL    WRK-MEISAI-N-PG  USING
                                        SPA-AREA
                                        ORCHCN04AREA
               MOVE    SPACE            TO  SPA-PRT-FLG
           END-IF
      *
           .
       2800-NYU-SRYPRT-EXT.
           EXIT.
      *****************************************************************
      *    外来請求書兼領収書印刷処理
      *****************************************************************
       2800-GAI-SKYPRT-SEC                 SECTION.
      *
           INITIALIZE                      ORCHC03AREA
           MOVE    SPA-S01-DENPNUM     TO  ORCHC03-DENPNUM
           MOVE    SYUPARA01-SEIKYU    TO  ORCHC03-SEIKYU
           MOVE    SYUPARA01-NYUKIN    TO  ORCHC03-NYUKIN
           MOVE    1                   TO  ORCHC03-HAKKOFLG
           MOVE    SYUPARA01-SKYSUMFLG TO  ORCHC03-HAKHOUFLG
           MOVE    1                   TO  ORCHC03-PARAFLG
           MOVE    SYUPARA01-ZENMISYU  TO  ORCHC03-ZENMISYU
           MOVE    SYUPARA01-ZENMISYU-2 TO ORCHC03-ZENMISYU-2
           MOVE    SYUPARA01-HENKIN    TO  ORCHC03-HENKIN
           MOVE    SYUPARA01-KANYUKIN  TO  ORCHC03-KANYUKIN
           MOVE    WRK-GYOUMUID        TO  ORCHC03-PARA-GYOUMUID
           MOVE    WRK-UID             TO  ORCHC03-PARA-TERMID
           MOVE    WRK-FILEMEI         TO  ORCHC03-PARA-FILEMEI
           MOVE    1                   TO  ORCHC03-MOTOID
      *
           MOVE    "01"                TO  SPA-PRT-FLG
           INITIALIZE                      SRPLNK-AREA
           MOVE    "INS"               TO  SRPLNK-MODE
           MOVE    RPKEY-SEIKYUSHO     TO  SRPLNK-VAL
           CALL    "ORCSRPLNK"             USING
                                           SRPLNK-AREA
                                           SPA-AREA
      *
           CALL    WRK-SEIKYU-PG           USING
                                           SPA-AREA
                                           ORCHC03AREA
           MOVE    SPACE               TO  SPA-PRT-FLG
      *
           .
       2800-GAI-SKYPRT-EXT.
           EXIT.
           .
      *****************************************************************
      *    外来診療費明細書印刷処理
      *****************************************************************
       2800-GAI-SRYPRT-SEC                 SECTION.
      *
           MOVE    ZERO                TO  WRK-PAGE-CNT
      *
           PERFORM 900-PARA-KEY5-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PARA    NOT =   ZERO )
      *
               MOVE    PARA-DATA-REC   TO  SYUPARA03-AREA
      *
               INITIALIZE                  ORCHC04AREA
               MOVE    SYUPARA03-DENPNUM
                                       TO  ORCHC04-DENPNUM
               MOVE    1               TO  ORCHC04-HAKKOFLG
               MOVE    SYUPARA01-SKYSUMFLG
                                       TO  ORCHC04-HAKHOUFLG
               MOVE    SYUPARA03-STSRYYMD  TO  ORCHC04-KIKAN-STYMD
               MOVE    SYUPARA03-EDSRYYMD  TO  ORCHC04-KIKAN-EDYMD
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   SYUPARA03-GRP-DENPNUM-MAX )
      *
                   MOVE    SYUPARA03-GRP-DENPNUM (IDXD)
                                       TO  ORCHC04-KIKAN-DENPNUM
                                                               (IDXD)
               END-PERFORM
               MOVE    WRK-PAGE-CNT    TO  ORCHC04-PAGE-CNT
      *
               MOVE    "02"            TO  SPA-PRT-FLG
               INITIALIZE                  SRPLNK-AREA
               MOVE    "INS"           TO  SRPLNK-MODE
               MOVE    RPKEY-MEISAISHO TO  SRPLNK-VAL
               CALL    "ORCSRPLNK"         USING
                                           SRPLNK-AREA
                                           SPA-AREA
               CALL    WRK-MEISAI-PG       USING
                                           SPA-AREA
                                           ORCHC04AREA
               MOVE    SPACE           TO  SPA-PRT-FLG
               MOVE    ORCHC04-PAGE-CNT
                                       TO  WRK-PAGE-CNT
      *
               PERFORM 900-PARA-KEY5-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2800-GAI-SRYPRT-EXT.
           EXIT.
           .
      *****************************************************************
      *    帳票プログラム取得処理
      *****************************************************************
       2800-PRINT-PG-SEC               SECTION.
      *
           INITIALIZE                  WRK-PRINT-G
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "00000003"          TO  ORCSPRTNM-KBNCD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYSKANRI-REC
           MOVE    ORCSPRTNM-PRTPG     TO  WRK-SEIKYU-PG
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "00000008"          TO  ORCSPRTNM-KBNCD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYSKANRI-REC
           MOVE    ORCSPRTNM-PRTPG     TO  WRK-MEISAI-PG
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "00000003"          TO  ORCSPRTNM-KBNCD
           MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYSKANRI-REC
           MOVE    ORCSPRTNM-PRTPG     TO  WRK-SEIKYU-N-PG
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "00000008"          TO  ORCSPRTNM-KBNCD
           MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYSKANRI-REC
           MOVE    ORCSPRTNM-PRTPG     TO  WRK-MEISAI-N-PG
      *
           .
      *
       2800-PRINT-PG-EXT.
           EXIT.
      *****************************************************************
      *    収納共通ＳＰＡ編集処理
      *****************************************************************
       2800-SPAKYOTUKEY-SET-SEC        SECTION.
      *
           INITIALIZE                      SPA-S01KYOUTU
      *
           INITIALIZE                      WRK-FILEMEI
           MOVE    SMCNDATE-YMD        TO  WRK-FILEMEI-YMD
           MOVE    SMCNDATE-HMS        TO  WRK-FILEMEI-HMS
      *
           MOVE    SPA-API-DENPNUM     TO  SPA-S01-DENPNUM
           MOVE    WRK-FILEMEI         TO  SPA-S01-FILEMEI
           MOVE    "S02"               TO  SPA-S01-GYOUMUID
                                           WRK-GYOUMUID
           MOVE    SPA-SRYYMD          TO  SPA-S01-SRYYMD
           MOVE    SPA-NYUGAIKBN       TO  SPA-S01-NYUGAIKBN
           MOVE    TSYU-SYUHKNNUM      TO  SPA-S01-HKNNUM
           MOVE    SPA-RSI-HKNKBN      TO  SPA-S01-RSI-HKNKBN
           MOVE    SPA-PTID            TO  SPA-S01-PTID
           MOVE    SPA-PTNUM           TO  SPA-S01-PTNUM
           MOVE    SPA-KANANAME        TO  SPA-S01-KANANAME
           MOVE    SPA-NAME            TO  SPA-S01-NAME
      *
           MOVE    SPA-API-DENPNUM     TO  SPA-S01-TUKI-DENPNUM
           MOVE    SPA-API-SRYYM       TO  SPA-S01-TUKI-SRYYM
           MOVE    SPA-API-KIKANKBN    TO  SPA-S01-TUKI-KIKANKBN
           MOVE    SPA-API-SRYYM       TO  SPA-S01-TUKI-STYM
                                           SPA-S01-TUKI-EDYM
           MOVE    SPA-API-STDAY       TO  SPA-S01-TUKI-STDD
           MOVE    SPA-API-EDDAY       TO  SPA-S01-TUKI-EDDD
           MOVE    SPA-API-SKYGRP      TO  SPA-S01-TUKI-GRPKBN
           MOVE    SPA-API-SKYPRT      TO  SPA-S01-TUKI-SKYPRT
           MOVE    SPA-API-SRYPRT      TO  SPA-S01-TUKI-SRYPRT
      *
           PERFORM 800-ORCUIDSET-SEC
           MOVE    C-UID               TO  SPA-S01-UID
                                           WRK-UID
      *
           .
       2800-SPAKYOTUKEY-SET-EXT.
           EXIT.
      **************************************************************
      *    外来月まとめ請求書兼領収書発行処理
      **************************************************************
       2800-GAI-TUKISKYPRT-SEC         SECTION.
      *
           INITIALIZE                      ORCHC03AREA
           MOVE    SPA-API-DENPNUM     TO  ORCHC03-DENPNUM
           MOVE    SYUPARA01-SEIKYU    TO  ORCHC03-SEIKYU
           MOVE    SYUPARA01-NYUKIN    TO  ORCHC03-NYUKIN
           MOVE    4                   TO  ORCHC03-HAKKOFLG
           MOVE    SYUPARA01-SKYSUMFLG TO  ORCHC03-HAKHOUFLG
           MOVE    1                   TO  ORCHC03-PARAFLG
           MOVE    SYUPARA01-ZENMISYU  TO  ORCHC03-ZENMISYU
           MOVE    SYUPARA01-ZENMISYU-2 TO ORCHC03-ZENMISYU-2
           MOVE    SYUPARA01-HENKIN    TO  ORCHC03-HENKIN
           MOVE    SYUPARA01-KANYUKIN  TO  ORCHC03-KANYUKIN
           MOVE    WRK-GYOUMUID        TO  ORCHC03-PARA-GYOUMUID
           MOVE    WRK-UID             TO  ORCHC03-PARA-TERMID
           MOVE    WRK-FILEMEI         TO  ORCHC03-PARA-FILEMEI
           MOVE    2                   TO  ORCHC03-MOTOID
      *
           MOVE    "01"                TO  SPA-PRT-FLG
           INITIALIZE                      SRPLNK-AREA
           MOVE    "INS"               TO  SRPLNK-MODE
           MOVE    RPKEY-SEIKYUSHO     TO  SRPLNK-VAL
           CALL    "ORCSRPLNK"             USING
                                           SRPLNK-AREA
                                           SPA-AREA
      *
           CALL    WRK-SEIKYU-PG           USING
                                           SPA-AREA
                                           ORCHC03AREA
           MOVE    SPACE               TO  SPA-PRT-FLG
      *
           .
       2800-GAI-TUKISKYPRT-EXT.
           EXIT.
      **************************************************************
      *    外来月まとめ請求書兼領収書発行処理
      **************************************************************
       2800-GAI-TUKISRYPRT-SEC         SECTION.
      *
           MOVE    ZERO                    TO  WRK-PAGE-CNT
      *
           PERFORM 900-PARA-KEY5-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PARA    NOT =   ZERO )
      *
               MOVE    PARA-DATA-REC       TO  SYUPARA03-AREA
      *
               INITIALIZE                      ORCHC04AREA
               MOVE    SYUPARA03-DENPNUM   TO  ORCHC04-DENPNUM
               MOVE    4                   TO  ORCHC04-HAKKOFLG
               MOVE    SYUPARA01-SKYSUMFLG TO  ORCHC04-HAKHOUFLG
               MOVE    SPA-SYSYMD          TO  ORCHC04-DENPPRTYMD
               MOVE    SYUPARA03-STSRYYMD  TO  ORCHC04-KIKAN-STYMD
               MOVE    SYUPARA03-EDSRYYMD  TO  ORCHC04-KIKAN-EDYMD
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   SYUPARA03-GRP-DENPNUM-MAX )
      *
                   MOVE    SYUPARA03-GRP-DENPNUM (IDXD)
                                           TO  ORCHC04-KIKAN-DENPNUM
                                                               (IDXD)
               END-PERFORM
      *
               MOVE    WRK-PAGE-CNT    TO  ORCHC04-PAGE-CNT
               MOVE    "02"            TO  SPA-PRT-FLG
               INITIALIZE                  SRPLNK-AREA
               MOVE    "INS"           TO  SRPLNK-MODE
               MOVE    RPKEY-MEISAISHO TO  SRPLNK-VAL
               CALL    "ORCSRPLNK"         USING
                                           SRPLNK-AREA
                                           SPA-AREA
               CALL    WRK-MEISAI-PG       USING
                                           SPA-AREA
                                           ORCHC04AREA
               MOVE    SPACE           TO  SPA-PRT-FLG
               MOVE    ORCHC04-PAGE-CNT
                                       TO  WRK-PAGE-CNT
      *
               PERFORM 900-PARA-KEY5-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2800-GAI-TUKISRYPRT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    "0000"      TO  SPA-ERRCD
           END-IF
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
      *
               EVALUATE    WRK-SYMD
               WHEN    "00000000"
                   MOVE    01              TO  WRK-DATE-MM
                   MOVE    01              TO  WRK-DATE-DD
               WHEN    "99999999"
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-EVALUATE
      *
           END-IF
      *
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
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
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
      *    UID取得処理
      *****************************************************************
       800-ORCUIDSET-SEC               SECTION.
      *
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       UIDIO-ST
                                       UIDIO-AREA
      *
           .
       800-ORCUIDSET-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPA-MOTOPG      TO  WRK-MOTOPG
           MOVE    "API"           TO  SPA-MOTOPG
           CALL    "ORCGSERRMSG"   USING
                                   SPA-AREA
                                   ORCSLOCKAREA
           MOVE    WRK-MOTOPG      TO  SPA-MOTOPG
      *
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       900-XML-WRITE-SEC             SECTION.
      *
           MOVE    INCOMEREQ01-REQNUM        TO  INCOMERES03-REQNUM
           MOVE    INCOMEREQ01-REQMODE       TO  INCOMERES03-REQMODE
           MOVE    SPA-API-RESNUM            TO  INCOMERES03-RESNUM
      *
           MOVE    INCOMEREQ01-KARTE-UID     TO  INCOMERES03-KARTE-UID
           MOVE    INCOMEREQ01-ORCA-UID      TO  INCOMERES03-ORCA-UID
      *
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  INCOMERES03-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS    TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME        TO  INCOMERES03-INFORMATION-TIME
      *
           PERFORM 890-ERRCD-MSG-SEC
           IF    ( SPA-ERRCD       =   ZERO )
               MOVE    ZERO        TO  INCOMERES03-API-RESULT
           ELSE
               MOVE    "E"         TO  INCOMERES03-API-RESULT
               MOVE    SPA-ERRCD   TO  INCOMERES03-API-RESULT(2:)
           END-IF
           MOVE    SPA-ERRMSG      TO  INCOMERES03-API-RESULT-MSG
      *
           MOVE    WRK-TIME        TO  INCOMERES03-INFORMATION-TIME
      *
           MOVE    "xml_incomev3res03" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    INCOMERES03-AREA    TO  APIREQ-AREA
           MOVE    1                   TO  APIREQ-MODE
           MOVE    "incomev3res"       TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF    ( MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT   TO  SAPI01-BODY
               MOVE    "application/xml"
                                       TO  SAPI01-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       900-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    APIパラメタ検索処理
      *****************************************************************
       900-APIPARA-KEY5-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-APIPARA
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "api023"            TO  APIPARA-GYOUMUID
           MOVE    INCOMEREQ01-ORCA-UID  TO  APIPARA-TERMID
           MOVE    SPA-OPID            TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
           MOVE    INCOMEREQ01-KARTE-UID   TO  APIPARA-KARTE-UID
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  APIPARA-REC
           ELSE
               MOVE    1               TO  FLG-APIPARA
               INITIALIZE                  APIPARA-REC
           END-IF
      *
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-APIPARA-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    APIパラメタ検索処理
      *****************************************************************
       900-APIPARA-KEY7-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-APIPARA
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "api023"            TO  APIPARA-GYOUMUID
           MOVE    INCOMEREQ01-ORCA-UID  TO  APIPARA-TERMID
           MOVE    SPA-OPID            TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
           MOVE    INCOMEREQ01-KARTE-UID TO  APIPARA-KARTE-UID
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  APIPARA-REC
           ELSE
               MOVE    1               TO  FLG-APIPARA
               INITIALIZE                  APIPARA-REC
           END-IF
      *
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-APIPARA-KEY7-SEL-EXT.
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
           MOVE    SPA-API-SRYYM       TO  SYU-SRYYMD
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
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      **************************************************************
      *    患者情報検索処理
      **************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    SPA-PTID        TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
           ELSE
               MOVE    1           TO  FLG-PTINF
               INITIALIZE              PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    パラメタテーブル検索
      *****************************************************************
       900-PARA-KEY4-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA
      *
           INITIALIZE                      PARA-REC
           MOVE    WRK-GYOUMUID        TO  PARA-GYOUMUID
           MOVE    WRK-UID             TO  PARA-TERMID
           MOVE    WRK-FILEMEI         TO  PARA-FILEMEI
           MOVE    1                   TO  PARA-EDANUM
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PARA-REC
           ELSE
               MOVE    1               TO  FLG-PARA
               INITIALIZE                  PARA-REC
           END-IF
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PARA-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    パラメタテーブル検索
      *****************************************************************
       900-PARA-KEY5-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA
      *
           INITIALIZE                      PARA-REC
           MOVE    WRK-GYOUMUID        TO  PARA-GYOUMUID
           MOVE    WRK-UID             TO  PARA-TERMID
           MOVE    WRK-FILEMEI         TO  PARA-FILEMEI
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PARA-REC
           ELSE
               MOVE    1               TO  FLG-PARA
               INITIALIZE                  PARA-REC
           END-IF
      *
           .
       900-PARA-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    パラメタテーブルFETCH
      *****************************************************************
       900-PARA-KEY5-FET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PARA-REC
           ELSE
               MOVE    1               TO  FLG-PARA
               INITIALIZE                  PARA-REC
           END-IF
      *
           .
       900-PARA-KEY5-FET-EXT.
           EXIT.
      *************************************************************
      *    パラメタ削除処理
      *************************************************************
       900-DELETE-PARA-SEC            SECTION.
      *
           INITIALIZE                  PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    WRK-GYOUMUID        TO  PARA-GYOUMUID
           MOVE    WRK-UID             TO  PARA-TERMID
           MOVE    WRK-FILEMEI         TO  PARA-FILEMEI
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4000"          TO  SPA-ERRCD
           END-IF
      *
           .
       900-DELETE-PARA-EXT.
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
      *    ＤＢ検索処理（FETCHはしない）
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
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
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                 SECTION.
      *
           MOVE    "DBINSERT"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    XMLWRITE処理
      *****************************************************************
       910-XMLWRITE-SEC                 SECTION.
      *
           MOVE    "XMLWRITE"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       APIREQ-AREA
      *
           .
       910-XMLWRITE-EXT.
           EXIT.
