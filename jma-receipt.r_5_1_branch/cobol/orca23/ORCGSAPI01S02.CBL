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
       PROGRAM-ID.         ORCGSAPI01S02.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 収納（更新）
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
           03  FLG-SYUNOU              PIC 9(01).
           03  FLG-SYUMEI              PIC 9(01).
           03  FLG-SYURRK              PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
           03  FLG-ENTRYCHK            PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTRSIINF            PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-NYU-SKYPRT          PIC 9(01).
           03  FLG-GAI-SKYPRT          PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXB                    PIC 9(05).
           03  IDXL                    PIC 9(05).
           03  IDX-TMP                 PIC 9(05).
           03  IDX-SRT                 PIC 9(05).
      *
       01  CNT-AREA.
           03  CNT-APIPARA             PIC 9(07).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-TERM                PIC X(64).
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
           03  WRK-KINGAKU             PIC S9(10).
           03  WRK-KINGAKUM9           PIC ---------9.
           03  WRK-KINGAKUMM           PIC ----------.
           03  WRK-MISYU-G.
               05  WRK-MISYU-OCC       OCCURS 2.
                   07  WRK-MISYU       PIC S9(10).
                   07  WRK-KANYU       PIC S9(10).
           03  WRK-INSTR               PIC X(500).
           03  WRK-OUTSTR              PIC X(500).
           03  WRK-LEN                 PIC S9(10).
           03  WRK-NYUKIN              PIC S9(10).
           03  WRK-MIN                 PIC 9(10). 
           03  WRK-NOZ                 PIC Z9.
           03  WRK-SYU-JOTAIKBN        PIC X(1).
           03  WRK-SYU-JOTAINAME       PIC X(10).
           03  WRK-SMEI-JOTAIKBN       PIC X(1).
           03  WRK-SMEI-PROC-FLG       PIC X(1).
           03  WRK-GYOUMUID            PIC X(03).
           03  WRK-UID                 PIC X(36).
           03  WRK-FILEMEI.
               05  WRK-FILEMEI-YMD     PIC X(08).
               05  WRK-FILEMEI-HMS     PIC X(06).
               05  WRK-FILEMEI-CD      PIC X(06).
           03  WRK-DENPITEM-G.
               05  WRK-NYU-NYUKIN          PIC S9(10).
               05  WRK-NYU-HENKIN          PIC S9(10).
               05  WRK-GAI-NYUKIN          PIC S9(10).
               05  WRK-GAI-HENKIN          PIC S9(10).
               05  WRK-GAI-DENPNUM         PIC 9(07).
               05  WRK-NYU-DENPNUM         PIC 9(07).
               05  WRK-GAI-SRYYMD          PIC X(08).
               05  WRK-NYU-SRYYMD          PIC X(08).
               05  WRK-GAI-KONSEIKYU       PIC S9(10).
               05  WRK-NYU-KONSEIKYU       PIC S9(10).
               05  WRK-GAI-MISYU-P         PIC S9(10).
               05  WRK-GAI-MISYU-M         PIC S9(10).
           03  WRK-STIDX               PIC 9(05). 
      *
       01  KEY-AREA.
           03  KEY-GAI-MISYU           OCCURS 2.
               05  KEY-SRYYMD          PIC X(08).
               05  KEY-DENPNUM         PIC 9(07).
           03  KEY-GAI-SKYEDYMD        PIC X(08).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-KINGAKU-MAX           PIC S9(10)  VALUE 9999999.
           03  CONST-DENPEDANUM-MAX        PIC 9(03)   VALUE 99.
           03  CONST-LIMIT                 PIC 9(05)   VALUE 99999.
           03  CONST-RES-MAX               PIC 9(05)   VALUE 500.
           03  CONST-SMEI-PROC-FLG-G.
               05  CONST-SEIKYU        PIC 9(01)   VALUE 1.
               05  CONST-TEISEI        PIC 9(01)   VALUE 2.
               05  CONST-NYUKIN        PIC 9(01)   VALUE 3.
               05  CONST-MINUS-NYUKIN  PIC 9(01)   VALUE 4.
           03  CONST-REQMODE-G.
               05  CONST-REQMODE-NYUKIN       PIC X(02)   VALUE "01".
               05  CONST-REQMODE-RRKSYU       PIC X(02)   VALUE "02".
               05  CONST-REQMODE-TORIKESHI    PIC X(02)   VALUE "03".
               05  CONST-REQMODE-HENKIN       PIC X(02)   VALUE "04".
               05  CONST-REQMODE-SAIKEISAN    PIC X(02)   VALUE "05".
               05  CONST-REQMODE-M-SAIKEISAN  PIC X(02)   VALUE "06".
               05  CONST-REQMODE-M-NYUKIN     PIC X(02)   VALUE "07".
               05  CONST-REQMODE-SKY-TORIKESHI
                                           PIC X(02)   VALUE "08".
      *
           COPY    "SAPI01SCR-SPA.INC".
      *
           COPY    "S01COMMON-SPA".
      *
       01  TSRT-AREA.
           03  TSRT-MAX                    PIC 9(05).
           03  TSRT-OCC                    OCCURS 501.
               05  TSRT-KEY.
                   07  TSRT-SRYYMD         PIC X(08).
                   07  TSRT-SRYKA          PIC X(02).
                   07  TSRT-HKNCOMBI       PIC X(04).
                   07  TSRT-DENPNUM        PIC 9(07).
               05  TSRT-NYUKIN             PIC S9(10).
      *
       01  TSYU-REC.
           COPY    "CPSYUNOU.INC"  REPLACING //SYU-//
                                   BY        //TSYU-//.
      *
       01  TSMEI-REC.
           COPY    "CPSYUMEI.INC"  REPLACING //SMEI-//
                                   BY        //TSMEI-//.
      *
       01  TSYURRK-REC.
           COPY    "CPSYURRK.INC"  REPLACING //SYURRK-//
                                   BY        //TSYURRK-//.
      *
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
      *    収納更新
           COPY   "CPORCSS016.INC".
      *
      *    まとめ入金編集サブ
           COPY    "CPORCSNYUKINS.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    文字列操作サブ
           COPY    "CPORCSSTRING.INC".
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
           COPY    "CPINCOMEV3RES01.INC".
           COPY    "CPINCOMEV3RES03.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    全角チェックパラメタ(拡張文字対応版)
           COPY    "CPORCSKANACHK2.INC".
      *
      *    バッチジョブ制御
           COPY    "CPORCSMONBATCH.INC".
      *
      *    排他制御
           COPY    "CPORCSLOCK.INC".
      *
      *    請求確認情報取得
           COPY    "CPORCSS015.INC".
      *
      *    再計算サブ
           COPY    "CPSSUB05.INC".
      *
      *    請求書印刷パラメタ
           COPY    "CPORCHC03.INC".
           COPY    "CPORCHCN03.INC".
       *
           COPY    "CPORCHC03S02.INC".
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
      *    未収金取得サブ
           COPY    "CPORCSMISYU.INC".
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
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    収納履歴
       01  SYURRK-REC.
           COPY    "CPSYURRK.INC".
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1039.INC".
           COPY    "CPSK1010.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
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
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
           COPY    "CPSYUPARA01.INC".
           COPY    "CPSYUPARA02.INC".
           COPY    "CPSYUPARA03.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
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
           INITIALIZE                  SYS-1039-REC
           INITIALIZE                  PTINF-REC
           INITIALIZE                  TSYU-REC
           INITIALIZE                  TSMEI-REC
           INITIALIZE                  TSYURRK-REC
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
           INITIALIZE                  INCOMERES01-AREA
                                       INCOMERES03-AREA
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1039"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SMCNDATE-YMD    TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1039-REC
           IF    ( SYS-1039-S06SKYSUMKBN  =   SPACE )
               MOVE    "0"         TO  SYS-1039-S06SKYSUMKBN
           END-IF
           IF    ( SYS-1039-MISYUKIKANKBN      =   SPACE )
               MOVE    "0"         TO  SYS-1039-MISYUKIKANKBN
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
      *    入力チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC              SECTION.
      *
           EVALUATE    INCOMEREQ01-REQMODE
      *
           WHEN    CONST-REQMODE-NYUKIN
           WHEN    CONST-REQMODE-HENKIN
               PERFORM 20010-NYUKIN-CHK-SEC
      *
           WHEN    CONST-REQMODE-RRKSYU
               PERFORM 20010-RIREKISYUSEI-CHK-SEC
      *
           WHEN    CONST-REQMODE-TORIKESHI
               PERFORM 20010-TORIKESHI-CHK-SEC
      *
           WHEN    CONST-REQMODE-SAIKEISAN
               PERFORM 20010-SAIKEISAN-CHK-SEC
      *
           WHEN    CONST-REQMODE-SKY-TORIKESHI
               PERFORM 20010-SKY-TORIKESHI-CHK-SEC
      *
           WHEN    CONST-REQMODE-M-SAIKEISAN
               PERFORM 20010-M-SAIKEISAN-CHK-SEC
      *
           WHEN    CONST-REQMODE-M-NYUKIN
               PERFORM 20010-M-NYUKIN-CHK-SEC
           END-EVALUATE
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金チェック処理
      *****************************************************************
       20010-NYUKIN-CHK-SEC            SECTION.
      *
           PERFORM 20010-COMMON-CHK-SEC
      *
           IF    ( INCOMEREQ01-REQMODE =   CONST-REQMODE-NYUKIN )
               MOVE    "1"         TO  SPA-API-NYUHENKBN
           ELSE
               MOVE    "2"         TO  SPA-API-NYUHENKBN
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20011-FORCE-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20011-NYUHENYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20011-NYUHENTIME-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-API-NYUHENKBN       =   "1" )
                   PERFORM 20011-NYUKIN-CHK-SEC
               ELSE
                   COMPUTE WRK-KINGAKU
                       =   TSYU-NYUKIN-TOTAL   -   TSYU-SKYMONEY
                   MOVE    WRK-KINGAKU     TO  WRK-KINGAKUMM
                   MOVE    WRK-KINGAKUMM   TO  INCOMEREQ01-NYUKIN
                   PERFORM 20011-HENKIN-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-API-NYUHENKBN       =   "1" )
                   PERFORM 20011-NYUKIN-HOHO-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-API-NYUHENKBN       =   "1" )
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 20011-PRINT-MODE-CHK-SEC
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 20011-SKYPRT-CHK-SEC
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 20011-SRYPRT-CHK-SEC
               END-IF
           END-IF
      *
           .
       20010-NYUKIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    履歴修正チェック処理
      *****************************************************************
       20010-RIREKISYUSEI-CHK-SEC      SECTION.
      *
           PERFORM 20010-COMMON-CHK-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( INCOMEREQ01-NYUHENYMD
                               NOT =   SPACE )
                   PERFORM 20011-NYUHENYMD-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( INCOMEREQ01-NYUHENTIME
                               NOT =   SPACE )
                   PERFORM 20011-NYUHENTIME-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               IF    ( SPA-API-PROC-FLG    =   CONST-SEIKYU
                                            OR CONST-TEISEI )
                AND  ( TSYURRK-DENPNUM NOT =   ZERO )
                   IF    ( INCOMEREQ01-CHOSEI1 NOT =   SPACE )
                       PERFORM 20011-CHOSEI1-CHK-SEC
                   END-IF
      *
                   IF    ( SPA-ERRCD       =   SPACE )
                       IF    ( INCOMEREQ01-CHOSEI2 NOT =   SPACE )
                           PERFORM 20011-CHOSEI2-CHK-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( INCOMEREQ01-NYUKIN NOT =   SPACE )
                   IF  ( SPA-API-MINUS   =   "1" )
                       PERFORM 20011-MINUS-NYUKIN-CHK-SEC
                   ELSE
                       PERFORM 20011-NYUKIN-CHK-SEC
                   END-IF
               END-IF
      *
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-API-MINUS           =   "1" )
                AND  ( TSMEI-JOUTAIKBN     NOT =   "6" )
                  CONTINUE
               ELSE
      *
                   IF    ( INCOMEREQ01-NYUKIN-HOHO =   SPACE )
                       IF    ( TSMEI-NYUKIN-HOHO   NOT =   ZERO )
                           MOVE    TSMEI-NYUKIN-HOHO
                                       TO  INCOMEREQ01-NYUKIN-HOHO
                       END-IF
                   ELSE
                       MOVE    INCOMEREQ01-NYUKIN-HOHO
                                       TO  INCOMEREQ01-NYUKIN-HOHO
                   END-IF
      *
                   PERFORM 20011-NYUKIN-HOHO-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               COMPUTE WRK-KINGAKU
                   =   TSYU-SKYMONEY
                   -   TSYURRK-CHOSEI1
                   -   TSYURRK-CHOSEI2
                   +   SPA-API-CHOSEI1
                   +   SPA-API-CHOSEI2
               IF    ( WRK-KINGAKU <  ZERO )
                   MOVE    "3016"  TO SPA-ERRCD
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   IF    ( WRK-KINGAKU >  CONST-KINGAKU-MAX )
                       MOVE    "3024"  TO SPA-ERRCD
                   END-IF
               END-IF
      *
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               COMPUTE WRK-KINGAKU
                   =   TSYU-NYUKIN-TOTAL
                   -   TSMEI-NYUHEN-MONEY
                   +   SPA-API-NYUKIN
               IF    ( WRK-KINGAKU <  ZERO )
                   MOVE    "3014"  TO SPA-ERRCD
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   IF    ( WRK-KINGAKU >  CONST-KINGAKU-MAX )
                       MOVE    "3025"  TO SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20011-PRINT-MODE-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20011-SKYPRT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20011-SRYPRT-CHK-SEC
           END-IF
      *
           .
       20010-RIREKISYUSEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金取消チェック処理
      *****************************************************************
       20010-TORIKESHI-CHK-SEC      SECTION.
      *
           PERFORM 20010-COMMON-CHK-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20011-NYUHENYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20011-NYUHENTIME-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20011-MINUS-NYUKIN-CHK-SEC
      *
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
                PERFORM 20011-NYUKIN-HOHO-CHK-SEC
           END-IF
      *
           .
       20010-TORIKESHI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求取消チェック処理
      *****************************************************************
       20010-SKY-TORIKESHI-CHK-SEC     SECTION.
      *
           PERFORM 20010-COMMON-CHK-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( TSYU-DENPJTIKBN =   "3" )
                   MOVE    "0119"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20011-NYUHENYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20011-NYUHENTIME-CHK-SEC
           END-IF
      *
           .
       20010-SKY-TORIKESHI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    共通チェック処理
      *****************************************************************
       20010-COMMON-CHK-SEC            SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               EVALUATE    INCOMEREQ01-REQMODE
               WHEN    CONST-REQMODE-SAIKEISAN
               WHEN    CONST-REQMODE-M-SAIKEISAN
                   MOVE    "2"     TO  SPA-API-NYUGAIKBN
               WHEN    CONST-REQMODE-SKY-TORIKESHI
                   MOVE    "1"     TO  SPA-API-NYUGAIKBN
               WHEN    OTHER
                   PERFORM 20011-NYUGAIKBN-CHK-SEC
               END-EVALUATE
               MOVE    SPA-API-NYUGAIKBN
                                   TO  SPA-NYUGAIKBN
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20011-DENPNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( INCOMEREQ01-REQMODE =   CONST-REQMODE-RRKSYU )
                   PERFORM 20011-DENPEDANUM-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20011-SYUNOU-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( INCOMEREQ01-REQMODE =   CONST-REQMODE-RRKSYU )
                   PERFORM 20011-SYUMEI-CHK-SEC
               ELSE
                   PERFORM 20012-SYUMEI-CHK-SEC
               END-IF
           END-IF
      *
           .
       20010-COMMON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    再計算チェック処理
      *****************************************************************
       20010-SAIKEISAN-CHK-SEC         SECTION.
      *
           PERFORM 20010-COMMON-CHK-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20011-NYUHENYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20011-NYUHENTIME-CHK-SEC
           END-IF
      *
           .
       20010-SAIKEISAN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一括再計算チェック処理
      *****************************************************************
       20010-M-SAIKEISAN-CHK-SEC       SECTION.
      *
           MOVE    "2"            TO   SPA-API-NYUGAIKBN
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20011-SRYYM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20011-NYUHENYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20011-NYUHENTIME-CHK-SEC
           END-IF
      *
           .
      *
       20010-M-SAIKEISAN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一括入返金チェック処理
      *****************************************************************
       20010-M-NYUKIN-CHK-SEC          SECTION.
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   CONST-RES-MAX )
      *
               IF    ( INCOMEREQ01-TDENPNUM    (IDXA)  NOT =   SPACE )
                OR   ( INCOMEREQ01-TNYUGAIKBN  (IDXA)  NOT =   SPACE )
                   PERFORM 20010-M-NYUKIN-SEC
                   MOVE    IDXA    TO  SPA-API-SYUNOU-MAX
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-API-ERRFLG  NOT =   ZERO )
               MOVE    "0112"      TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20011-PRINT-MODE-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20011-SKYPRT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-API-SKYPRT  =   "1" )
                   PERFORM 20011-SKYGRP-CHK-SEC
               END-IF
           END-IF
      *
           .
       20010-M-NYUKIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一括入金チェック処理
      *****************************************************************
       20010-M-NYUKIN-SEC                 SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRCD
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    INCOMEREQ01-TNYUGAIKBN (IDXA)
                                   TO  INCOMEREQ01-NYUGAIKBN
               PERFORM 20011-NYUGAIKBN-CHK-SEC
               MOVE    SPA-API-NYUGAIKBN
                                   TO  SPA-API-TNYUGAIKBN (IDXA)
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    INCOMEREQ01-TDENPNUM (IDXA)
                                   TO  INCOMEREQ01-DENPNUM
               PERFORM 20011-DENPNUM-CHK-SEC
               MOVE    SPA-API-DENPNUM
                                   TO  SPA-API-TDENPNUM (IDXA)
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20011-SYUNOU-CHK-SEC
               MOVE    SPA-API-SRYYMD
                                   TO  SPA-API-TSRYYMD (IDXA)
               MOVE    SPA-API-SRYKA
                                   TO  SPA-API-TSRYKA (IDXA)
               MOVE    SPA-HKNKBN  TO  SPA-API-THKNKBN (IDXA)
               MOVE    SPA-RSI-HKNKBN
                                   TO  SPA-API-TRSI-HKNKBN (IDXA)
               MOVE    SPA-API-HKNCOMBI
                                   TO  SPA-API-THKNCOMBI (IDXA)
               MOVE    SPA-API-MISYU
                                   TO  SPA-API-TMISYU (IDXA)
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20012-SYUMEI-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20011-DUPLICATE-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    INCOMEREQ01-TNYUHENKBN (IDXA)
                                   TO  INCOMEREQ01-NYUHENKBN
               PERFORM 20011-NYUHENKBN-CHK-SEC
               MOVE    SPA-API-NYUHENKBN
                                   TO  SPA-API-TNYUHENKBN (IDXA)
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    INCOMEREQ01-TFORCE (IDXA)
                                   TO  INCOMEREQ01-FORCE
               PERFORM 20011-FORCE-CHK-SEC
               MOVE    SPA-API-FORCE
                                   TO  SPA-API-TFORCE (IDXA)
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    INCOMEREQ01-TNYUHENYMD (IDXA)
                                   TO  INCOMEREQ01-NYUHENYMD
               PERFORM 20011-NYUHENYMD-CHK-SEC
               MOVE    SPA-API-NYUHENYMD
                                   TO  SPA-API-TNYUHENYMD (IDXA)
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    INCOMEREQ01-TNYUHENTIME (IDXA)
                                   TO  INCOMEREQ01-NYUHENTIME
               PERFORM 20011-NYUHENTIME-CHK-SEC
               MOVE    SPA-API-NYUHENHMS
                                   TO  SPA-API-TNYUHENHMS (IDXA)
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    INCOMEREQ01-TNYUKIN (IDXA)
                                   TO  INCOMEREQ01-NYUKIN
               IF    ( SPA-API-TNYUHENKBN (IDXA)   =   "1" )
                   PERFORM 20011-NYUKIN-CHK-SEC
                   COMPUTE SPA-API-TMISYU (IDXA)
                       =   SPA-API-TMISYU (IDXA)
                       -   SPA-API-NYUKIN
                   COMPUTE SPA-API-TNYUKIN (IDXA)
                       =   SPA-API-NYUKIN
               ELSE
                   PERFORM 20011-HENKIN-CHK-SEC
                   COMPUTE SPA-API-TMISYU (IDXA)
                       =   SPA-API-TMISYU (IDXA)
                       +   SPA-API-NYUKIN
                   COMPUTE SPA-API-TNYUKIN (IDXA)
                       =   SPA-API-NYUKIN
                       *   -1
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-API-TNYUHENKBN (IDXA)   =   "1" )
                   MOVE    INCOMEREQ01-TNYUKIN-HOHO (IDXA)
                                   TO  INCOMEREQ01-NYUKIN-HOHO
                   PERFORM 20011-NYUKIN-HOHO-CHK-SEC
                   MOVE    SPA-API-NYUKIN-HOHO
                                   TO  SPA-API-TNYUKIN-HOHO (IDXA)
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    SPA-ERRCD   TO  SPA-API-TERRCD (IDXA)
               MOVE    1           TO  SPA-API-ERRFLG
           END-IF
      *
           .
       20010-M-NYUKIN-EXT.
           EXIT.
      *****************************************************************
      *    入外区分チェック
      *****************************************************************
       20011-NYUGAIKBN-CHK-SEC         SECTION.
      *
           IF    ( INCOMEREQ01-NYUGAIKBN   =   SPACE )
               MOVE    "0086"      TO  SPA-ERRCD
           ELSE
               EVALUATE    FUNCTION
                             UPPER-CASE(INCOMEREQ01-NYUGAIKBN)
               WHEN  "I"
                   MOVE    "1"     TO  SPA-API-NYUGAIKBN
               WHEN  "O"
                   MOVE    "2"     TO  SPA-API-NYUGAIKBN
               WHEN  OTHER
                   MOVE    "0034"  TO  SPA-ERRCD
               END-EVALUATE
           END-IF
      *
           .
       20011-NYUGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    伝票番号チェック
      *****************************************************************
       20011-DENPNUM-CHK-SEC           SECTION.
      *
           IF    ( INCOMEREQ01-DENPNUM =   SPACE )
               MOVE    "0100"      TO  SPA-ERRCD
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
       20011-DENPNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    伝票番号枝番チェック
      *****************************************************************
       20011-DENPEDANUM-CHK-SEC        SECTION.
      *
           IF    ( INCOMEREQ01-DENPEDANUM  =   SPACE )
               MOVE    "0115"          TO  SPA-ERRCD
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE    INCOMEREQ01-DENPEDANUM
                                       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF  ( SNUM-RC       NOT =   ZERO  )
                OR ( SNUM-MINKBN   NOT =   SPACE )
                OR ( SNUM-SYOKBN   NOT =   SPACE )
                   MOVE    "3005"      TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM TO  SPA-API-DENPEDANUM
               END-IF
           END-IF
      *
           .
       20011-DENPEDANUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    収納チェック
      *****************************************************************
       20011-SYUNOU-CHK-SEC        SECTION.
      *
           INITIALIZE                     SYUNOU-REC
           MOVE    SPA-HOSPNUM        TO  SYU-HOSPNUM
           MOVE    SPA-PTID           TO  SYU-PTID
           MOVE    SPA-API-NYUGAIKBN  TO  SYU-NYUGAIKBN
           MOVE    SPA-API-DENPNUM    TO  SYU-DENPNUM
           PERFORM 900-SYUNOU-KEY-SEL-SEC
           MOVE    SYUNOU-REC         TO  TSYU-REC
           IF    ( FLG-SYUNOU         =   ZERO )
      *
               MOVE    SYU-SRYYMD     TO  SPA-API-SRYYMD
               MOVE    SYU-SRYKA      TO  SPA-API-SRYKA
               MOVE    SYU-HKNCOMBINUM TO SPA-API-HKNCOMBI
      *
               COMPUTE SPA-API-MISYU
                   =   SYU-SKYMONEY
                   -   SYU-NYUKIN-TOTAL
      *
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
               IF    ( SYU-CREATEKBN  =   "3" )
                   MOVE    "0085"     TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    "0085"         TO  SPA-ERRCD
           END-IF
      *
           .
       20011-SYUNOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    収納明細チェック
      *****************************************************************
       20011-SYUMEI-CHK-SEC        SECTION.
      *
           INITIALIZE                     SYUMEI-REC
           MOVE    SPA-HOSPNUM        TO  SMEI-HOSPNUM
           MOVE    SPA-PTID           TO  SMEI-PTID
           MOVE    SPA-API-NYUGAIKBN  TO  SMEI-NYUGAIKBN
           MOVE    SPA-API-DENPNUM    TO  SMEI-DENPNUM
           MOVE    SPA-API-DENPEDANUM TO  SMEI-DENPEDANUM
           PERFORM 900-SYUMEI-KEY-SEL-SEC
           MOVE    SYUMEI-REC         TO  TSMEI-REC
           IF    ( FLG-SYUMEI     NOT =   ZERO )
               MOVE    "0116"         TO  SPA-ERRCD
           END-IF
      *
           INITIALIZE                     SYURRK-REC
           IF    ( SMEI-SYURRKNUM NOT =   ZERO )
               PERFORM 900-SYURRK-KEY-SEL-SEC
               IF    ( FLG-SYURRK  =   ZERO )
                   MOVE    SYURRK-REC  TO  TSYURRK-REC
              END-IF
           END-IF
      *
           MOVE    SMEI-NYUKIN-HOHO   TO  SPA-API-NYUKIN-HOHO
           MOVE    SMEI-NYUHEN-YMD    TO  SPA-API-NYUHENYMD
           MOVE    SMEI-DAILYKEY(9:)  TO  SPA-API-NYUHENHMS
           MOVE    SYURRK-CHOSEI1     TO  SPA-API-CHOSEI1
           MOVE    SYURRK-CHOSEI2     TO  SPA-API-CHOSEI2
           MOVE    SMEI-NYUHEN-MONEY  TO  SPA-API-NYUKIN
      *
           MOVE    SMEI-JOUTAIKBN     TO  WRK-SMEI-JOTAIKBN
           PERFORM 800-SMEI-JOTAIKBN-SEC
           MOVE    WRK-SMEI-PROC-FLG  TO  SPA-API-PROC-FLG
      *
           IF    ( SMEI-JOUTAIKBN     =   "4" OR  "6" )
            OR  (( SMEI-JOUTAIKBN     =   "8" )
             AND ( SMEI-NYUHEN-MONEY  <   ZERO ))
               MOVE    "1"            TO  SPA-API-MINUS
           END-IF
      *
           .
       20011-SYUMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    収納明細チェック（明細件数チェック）
      *****************************************************************
       20012-SYUMEI-CHK-SEC        SECTION.
      *
           PERFORM 900-SYUMEI-KEY7-SEL-SEC
           IF    ( SMEI-DENPEDANUM >=  CONST-DENPEDANUM-MAX )
               MOVE  "0117"        TO  SPA-ERRCD
           END-IF
      *
           .
       20012-SYUMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    二重入力チェック
      *****************************************************************
       20011-DUPLICATE-CHK-SEC        SECTION.
      *
            PERFORM VARYING IDXB  FROM    1    BY  1
                    UNTIL ( IDXB  >=  IDXA )
                     OR   ( SPA-ERRCD NOT =    SPACE )
      *
                IF    ( SPA-API-TNYUGAIKBN (IDXA)
                                   =  SPA-API-TNYUGAIKBN (IDXB))
                 AND  ( SPA-API-TDENPNUM (IDXA)
                                   =  SPA-API-TDENPNUM (IDXB))
      *
                    MOVE    "0111" TO  SPA-ERRCD
      *
                END-IF
      *
            END-PERFORM
      *
           .
       20011-DUPLICATE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入返金区分チェック
      *****************************************************************
       20011-NYUHENKBN-CHK-SEC         SECTION.
      *
            IF    ( INCOMEREQ01-NYUHENKBN  =   SPACE )
                MOVE    "0102"         TO  SPA-ERRCD
            ELSE
                IF    ( INCOMEREQ01-NYUHENKBN  =   "1" OR "2" )
                    MOVE    INCOMEREQ01-NYUHENKBN
                                       TO  SPA-API-NYUHENKBN
                ELSE
                    MOVE    "0103"     TO  SPA-ERRCD
                END-IF
            END-IF
      *
           .
       20011-NYUHENKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    過入金許可フラグ
      *****************************************************************
       20011-FORCE-CHK-SEC             SECTION.
      *
            IF   ( SPA-API-NYUHENKBN   =   "1" )
               EVALUATE    FUNCTION
                             UPPER-CASE(INCOMEREQ01-FORCE)
               WHEN    SPACE
               WHEN    "FALSE"
                   CONTINUE
               WHEN    "TRUE"
                   MOVE    "1"     TO  SPA-API-FORCE
               WHEN    OTHER
                   MOVE "0104"     TO  SPA-ERRCD
               END-EVALUATE
            END-IF
      *
           .
       20011-FORCE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処理日チェック処理
      *****************************************************************
       20011-NYUHENYMD-CHK-SEC         SECTION.
      *
           IF   ( INCOMEREQ01-NYUHENYMD    =   SPACE )
               MOVE    SMCNDATE-YMD    TO  SPA-API-NYUHENYMD
           ELSE
               MOVE    INCOMEREQ01-NYUHENYMD
                                       TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "3010"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  SPA-API-NYUHENYMD
               END-IF
           END-IF
      *
           .
       20011-NYUHENYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処理時間チェック処理
      *****************************************************************
       20011-NYUHENTIME-CHK-SEC         SECTION.
      *
           IF   ( INCOMEREQ01-NYUHENTIME   =   SPACE )
               MOVE    SPACE       TO  SPA-API-NYUHENHMS
           ELSE
               IF    ( INCOMEREQ01-NYUHENTIME (1:2)    IS NUMERIC )
                AND  ( INCOMEREQ01-NYUHENTIME (4:2)    IS NUMERIC )
                AND  ( INCOMEREQ01-NYUHENTIME (1:2)   >=  "00" )
                AND  ( INCOMEREQ01-NYUHENTIME (1:2)   <=  "23" )
                AND  ( INCOMEREQ01-NYUHENTIME (4:2)   >=  "00" )
                AND  ( INCOMEREQ01-NYUHENTIME (4:2)   <=  "59" )
                   MOVE    INCOMEREQ01-NYUHENTIME (1:2)
                                   TO  SPA-API-NYUHENHMS
                   MOVE    INCOMEREQ01-NYUHENTIME (4:2)
                                   TO  SPA-API-NYUHENHMS(3:2)
                   MOVE    "00"    TO  SPA-API-NYUHENHMS(5:2)
               ELSE
                   MOVE    "0105"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       20011-NYUHENTIME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金方法チェック処理
      *****************************************************************
       20011-NYUKIN-HOHO-CHK-SEC         SECTION.
      *
           IF    ( INCOMEREQ01-NYUKIN-HOHO =   SPACE )
               IF    ( PTINF-NYUKIN-HOHO   =   SPACE )
                   MOVE    "01"        TO  SPA-API-NYUKIN-HOHO
               ELSE
                   MOVE    PTINF-NYUKIN-HOHO
                                       TO  SPA-API-NYUKIN-HOHO
               END-IF
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE   ZERO             TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "1041"          TO  SYS-KANRICD
               MOVE    INCOMEREQ01-NYUKIN-HOHO
                                       TO  SYS-KBNCD
               MOVE    SPA-SYSYMD      TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    INCOMEREQ01-NYUKIN-HOHO
                                       TO  SPA-API-NYUKIN-HOHO
               ELSE
                   MOVE    "3011"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       20011-NYUKIN-HOHO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    調整金１チェック処理
      *****************************************************************
       20011-CHOSEI1-CHK-SEC           SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( TSYU-DENPJTIKBN     =   "3" )
               MOVE    "3020"          TO  SPA-ERRCD
           WHEN    OTHER
               INITIALIZE                  ORCSNUMAREA
               MOVE    INCOMEREQ01-CHOSEI1 TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF  ( SNUM-RC       NOT =   ZERO  )
                OR ( SNUM-SEISUU       >   7     )
                OR ( SNUM-SYOKBN   NOT =   SPACE )
                   MOVE    "3017"      TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM TO  SPA-API-CHOSEI1
               END-IF
           END-EVALUATE
      *
           .
       20011-CHOSEI1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    調整金２チェック処理
      *****************************************************************
       20011-CHOSEI2-CHK-SEC           SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( TSYU-DENPJTIKBN     =   "3" )
               MOVE    "3021"          TO  SPA-ERRCD
           WHEN    OTHER
               INITIALIZE                  ORCSNUMAREA
               MOVE    INCOMEREQ01-CHOSEI2 TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF  ( SNUM-RC       NOT =   ZERO  )
                OR ( SNUM-SEISUU       >   7     )
                OR ( SNUM-SYOKBN   NOT =   SPACE )
                   MOVE    "3018"      TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM TO  SPA-API-CHOSEI2
               END-IF
           END-EVALUATE
      *
           .
       20011-CHOSEI2-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金チェック処理
      *****************************************************************
       20011-NYUKIN-CHK-SEC         SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( INCOMEREQ01-NYUKIN      =   SPACE )
               MOVE    "3007"          TO  SPA-ERRCD
           WHEN  ( TSYU-DENPJTIKBN     =   "3" )
            AND  ( INCOMEREQ01-REQMODE
                                   NOT =   CONST-REQMODE-RRKSYU )
               MOVE    "0006"          TO  SPA-ERRCD
           WHEN    OTHER
               INITIALIZE                  ORCSNUMAREA
               MOVE    INCOMEREQ01-NYUKIN
                                       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               EVALUATE    TRUE
               WHEN  ( SNUM-RC       NOT =   ZERO  )
                  OR ( SNUM-MINKBN   NOT =   SPACE )
                  OR ( SNUM-SEISUU       >   7     )
                  OR ( SNUM-SYOKBN   NOT =   SPACE )
                   MOVE    "3019"      TO  SPA-ERRCD
               WHEN  ( SNUM-OUTNUM     =   ZERO )
                AND  ( INCOMEREQ01-REQMODE
                                   NOT =   CONST-REQMODE-RRKSYU )
                   MOVE    "3007"      TO  SPA-ERRCD
               WHEN    OTHER
                   MOVE    SNUM-OUTNUM TO  SPA-API-NYUKIN
                   IF    ( INCOMEREQ01-REQMODE = CONST-REQMODE-NYUKIN)
                    OR   ( INCOMEREQ01-REQMODE = CONST-REQMODE-M-NYUKIN)
                       COMPUTE WRK-NYUKIN
                           =   TSYU-NYUKIN-TOTAL
                           +   SNUM-OUTNUM
                       IF    ( SPA-API-FORCE   =   SPACE )
                           IF    ( WRK-NYUKIN  >   TSYU-SKYMONEY )
                               MOVE    "0107"  TO  SPA-ERRCD
                           END-IF
                       END-IF
                       IF    ( SPA-ERRCD       =   SPACE )
                           IF    ( WRK-NYUKIN  >   CONST-KINGAKU-MAX )
                               MOVE    "3025"  TO  SPA-ERRCD
                           END-IF
                       END-IF
                   END-IF
      *
               END-EVALUATE
           END-EVALUATE
      *
           .
       20011-NYUKIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    返金チェック処理
      *****************************************************************
       20011-HENKIN-CHK-SEC         SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( TSYU-SKYMONEY          >=   TSYU-NYUKIN-TOTAL )
            AND  ( INCOMEREQ01-REQMODE NOT =   CONST-REQMODE-RRKSYU )
               MOVE    "0108"          TO  SPA-ERRCD
           WHEN  ( INCOMEREQ01-NYUKIN      =   SPACE )
               MOVE    "0106"          TO  SPA-ERRCD
           WHEN    OTHER
               INITIALIZE                  ORCSNUMAREA
               MOVE    INCOMEREQ01-NYUKIN
                                       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               EVALUATE    TRUE
               WHEN  ( SNUM-RC       NOT =   ZERO  )
                OR   ( SNUM-MINKBN   NOT =   SPACE )
                OR   ( SNUM-SEISUU       >   7     )
                OR   ( SNUM-SYOKBN   NOT =   SPACE )
                   MOVE    "0109"      TO  SPA-ERRCD
               WHEN  ( SNUM-OUTNUM       =   ZERO )
                   MOVE    "0106"      TO  SPA-ERRCD
               WHEN    OTHER
                   MOVE    SNUM-OUTNUM TO  SPA-API-NYUKIN
                   COMPUTE WRK-NYUKIN
                       =   TSYU-NYUKIN-TOTAL
                       -   TSYU-SKYMONEY
                   IF    ( WRK-NYUKIN  <   SNUM-OUTNUM )
                       MOVE    "0110"  TO  SPA-ERRCD
                   END-IF
               END-EVALUATE
           END-EVALUATE
      *
           .
       20011-HENKIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    マイナス入金チェック処理
      *****************************************************************
       20011-MINUS-NYUKIN-CHK-SEC      SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( INCOMEREQ01-NYUKIN  =   SPACE )
               MOVE    "3022"          TO  SPA-ERRCD
           WHEN    OTHER
               INITIALIZE                  ORCSNUMAREA
               MOVE    INCOMEREQ01-NYUKIN
                                       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               EVALUATE    TRUE
               WHEN  ( SNUM-RC       NOT =   ZERO  )
                OR   ( SNUM-SEISUU       >   7     )
                OR   ( SNUM-SYOKBN   NOT =   SPACE )
                   MOVE    "3019"      TO  SPA-ERRCD
               WHEN  ( SNUM-OUTNUM =   ZERO )
                AND  ( INCOMEREQ01-REQMODE
                               NOT =   CONST-REQMODE-RRKSYU )
                       MOVE    "3022"  TO  SPA-ERRCD
               WHEN  ( SNUM-OUTNUM >   ZERO )
                       MOVE    "3013"  TO  SPA-ERRCD
               WHEN    OTHER
                   MOVE    SNUM-OUTNUM
                                       TO  SPA-API-NYUKIN
                   IF    ( INCOMEREQ01-REQMODE
                                   =   CONST-REQMODE-TORIKESHI )
                       COMPUTE WRK-NYUKIN
                           =   TSYU-NYUKIN-TOTAL
                           +   SNUM-OUTNUM
                       IF    ( WRK-NYUKIN  <   0 )
                           MOVE    "3014"  TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-EVALUATE
           END-EVALUATE
      *
           .
       20011-MINUS-NYUKIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療年月チェック処理
      *****************************************************************
       20011-SRYYM-CHK-SEC      SECTION.
      *
           IF    ( INCOMEREQ01-SRYYM   =   SPACE )
               MOVE    "0120"  TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD   =      SPACE )
      *
               MOVE    INCOMEREQ01-SRYYM
                                   TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               MOVE    "01"        TO  WRK-SYMD(7:)
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0033"  TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD (1:6)
                                   TO  SPA-API-SRYYM
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   =      SPACE )
               PERFORM 900-SYUNOU-KEY14-SEL-SEC
               IF    ( FLG-SYUNOU  NOT =   ZERO )
                   MOVE    "0060"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       20011-SRYYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    印刷モードチェック
      *****************************************************************
       20011-PRINT-MODE-CHK-SEC         SECTION.
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
      *
      *
           .
       20011-PRINT-MODE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書印刷区分チェック
      *****************************************************************
       20011-SKYPRT-CHK-SEC            SECTION.
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
       20011-SKYPRT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療費明細書印刷区分チェック
      *****************************************************************
       20011-SRYPRT-CHK-SEC             SECTION.
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
           .
       20011-SRYPRT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書発行方法チェック
      *****************************************************************
       20011-SKYGRP-CHK-SEC            SECTION.
      *
           IF    ( INCOMEREQ01-SKYGRP  =   SPACE )
               MOVE    SYS-1039-S06SKYSUMKBN
                                   TO  SPA-API-SKYGRP
           ELSE
               IF    ( INCOMEREQ01-SKYGRP  =   "0" OR "1" OR "2"
                                                   OR "3" OR "4" )
                   MOVE    INCOMEREQ01-SKYGRP
                                   TO  SPA-API-SKYGRP
               ELSE
                   MOVE    "0129"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       20011-SKYGRP-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       2002-KANREN-CHK-SEC             SECTION.
      *
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
           EVALUATE    INCOMEREQ01-REQMODE
      *
           WHEN    CONST-REQMODE-NYUKIN
           WHEN    CONST-REQMODE-HENKIN
               PERFORM 200311-DB-NYUKIN-SEC
      *
           WHEN    CONST-REQMODE-RRKSYU
               PERFORM 200311-DB-RIREKI-SYUSEI-SEC
      *
           WHEN    CONST-REQMODE-TORIKESHI
               PERFORM 200311-DB-TORIKESHI-SEC
      *
           WHEN    CONST-REQMODE-SAIKEISAN
               PERFORM 200311-DB-SAIKEISAN-SEC
      *
           WHEN    CONST-REQMODE-M-SAIKEISAN
               PERFORM 200311-DB-M-SAIKEISAN-SEC
      *
           WHEN    CONST-REQMODE-M-NYUKIN
               PERFORM 200311-DB-M-NYUKIN-SEC
      *
           WHEN    CONST-REQMODE-SKY-TORIKESHI
               PERFORM 200311-DB-SKY-TORIKESHI-SEC
           END-EVALUATE
      *
           .
       20031-DB-EXT.
           EXIT.
      *****************************************************************
      *    データベース更新処理（入返金）
      *****************************************************************
       200311-DB-NYUKIN-SEC                SECTION.
      *
           MOVE    ZERO                TO  IDXB
      *
           INITIALIZE                      ORCSNYUKINAREAS
           MOVE    SPA-HOSPNUM         TO  LNK-SCNYUKINS-HOSPNUM
           MOVE    SPA-PTID            TO  LNK-SCNYUKINS-PTID
      *
           COMPUTE IDXB  = IDXB  + 1
      *
           MOVE    SPA-API-NYUGAIKBN
                                   TO  LNK-SCNYUKINS-NYUGAIKBN (IDXB)
           MOVE    SPA-API-DENPNUM TO  LNK-SCNYUKINS-DENPNUM   (IDXB)
      *
           IF    ( SPA-API-NYUHENKBN   =   "1" )
               MOVE    SPA-API-NYUKIN
                                   TO  LNK-SCNYUKINS-NYUKIN-MONEY (IDXB)
               MOVE    SPA-API-NYUKIN-HOHO
                                   TO  LNK-SCNYUKINS-NYUKIN-HOHO2 (IDXB)
           ELSE
               COMPUTE LNK-SCNYUKINS-NYUKIN-MONEY (IDXB)
                   =   SPA-API-NYUKIN * -1
           END-IF
      *
           MOVE    SPA-API-NYUHENYMD
                                   TO  LNK-SCNYUKINS-NYUKIN-YMD2 (IDXB)
          MOVE     SPA-API-NYUHENHMS
                                   TO  LNK-SCNYUKINS-NYUKIN-HMS2 (IDXB)
      *
           MOVE    IDXB            TO  LNK-SCNYUKINS-MAX
      *
           CALL    "ORCSNYUKINS"   USING   ORCSNYUKINAREAS
                                           SPA-AREA
           IF    ( LNK-SCNYUKINS-RC    =   ZERO )
               MOVE    LNK-SCNYUKINS-DENPEDANUM   (IDXB)
                                   TO  SPA-API-DENPEDANUM
           ELSE
               MOVE    "4000"      TO  SPA-ERRCD
           END-IF
      *
           .
       200311-DB-NYUKIN-EXT.
           EXIT.
      *****************************************************************
      *    データベース更新処理（履歴修正）
      *****************************************************************
       200311-DB-RIREKI-SYUSEI-SEC         SECTION.
      *
           INITIALIZE                      SS016-AREA
           MOVE SPA-API-PROC-FLG       TO  SS016-PROC-FLG
           MOVE SPA-API-DENPNUM        TO  SS016-DENPNUM
           MOVE SPA-API-DENPEDANUM     TO  SS016-DENPEDANUM
           MOVE TSYU-DENPPRTYMD        TO  SS016-DENPPRTYMD
           MOVE TSYU-GRP-DENPNUM       TO  SS016-GRP-DENPNUM
           MOVE SPA-PTID               TO  SS016-PTID
           MOVE SPA-API-NYUGAIKBN      TO  SS016-NYUGAIKBN
           MOVE TSYU-SRYKA             TO  SS016-SRYKA
           MOVE CONST-REQMODE-RRKSYU   TO  SS016-REQMODE
           MOVE SPA-API-CHOSEI1        TO  SS016-CHOSEI1
           MOVE SPA-API-CHOSEI2        TO  SS016-CHOSEI2
      *
           COMPUTE SS016-SKYMONEY
               =   TSMEI-SKYMONEY
               -   TSYURRK-CHOSEI1
               -   TSYURRK-CHOSEI2
               +   SPA-API-CHOSEI1
               +   SPA-API-CHOSEI2
      *
           MOVE SPA-API-NYUKIN         TO  SS016-NYUKIN
           MOVE SPA-API-NYUKIN-HOHO    TO  SS016-NYKN-HOHO
           MOVE TSYURRK-CHOSEI1        TO  SS016-CHOSEI1-MOTO
           MOVE TSYURRK-CHOSEI2        TO  SS016-CHOSEI2-MOTO
           MOVE TSMEI-SKYMONEY         TO  SS016-SKYMONEY-MOTO
           MOVE TSMEI-NYUHEN-MONEY     TO  SS016-NYUKIN-MOTO
           MOVE SPA-API-NYUHENYMD      TO  SS016-NYUKINYMD
           MOVE SPA-API-NYUHENHMS      TO  SS016-NYUKINHMS
           MOVE TSMEI-SYURRKUPDKBN     TO  SS016-SYURRKUPDKBN
           MOVE TSMEI-SYURRKNUM        TO  SS016-SYURRKNUM
           MOVE TSMEI-SYUEDANUM        TO  SS016-SYUEDANUM
      *
           CALL    "ORCSS016"          USING
                                       SS016-AREA
                                       TSYU-REC
                                       TSMEI-REC
                                       SPA-AREA
      *
           .
       200311-DB-RIREKI-SYUSEI-EXT.
           EXIT.
      *****************************************************************
      *    データベース更新処理（入金取消）
      *****************************************************************
       200311-DB-TORIKESHI-SEC         SECTION.
      *
           INITIALIZE                      SS016-AREA
           MOVE SPA-API-PROC-FLG       TO  SS016-PROC-FLG
           MOVE SPA-API-DENPNUM        TO  SS016-DENPNUM
           MOVE SPA-API-DENPEDANUM     TO  SS016-DENPEDANUM
           MOVE TSYU-DENPPRTYMD        TO  SS016-DENPPRTYMD
           MOVE TSYU-GRP-DENPNUM       TO  SS016-GRP-DENPNUM
           MOVE SPA-PTID               TO  SS016-PTID
           MOVE SPA-API-NYUGAIKBN      TO  SS016-NYUGAIKBN
           MOVE TSYU-SRYKA             TO  SS016-SRYKA
           MOVE CONST-REQMODE-TORIKESHI
                                       TO  SS016-REQMODE
      *
           MOVE SPA-API-NYUKIN         TO  SS016-NYUKIN
           MOVE SPA-API-NYUKIN-HOHO    TO  SS016-NYKN-HOHO
           MOVE SPA-API-NYUHENYMD      TO  SS016-NYUKINYMD
           MOVE SPA-API-NYUHENHMS      TO  SS016-NYUKINHMS
      *
           CALL    "ORCSS016"          USING
                                       SS016-AREA
                                       TSYU-REC
                                       TSMEI-REC
                                       SPA-AREA
           MOVE SS016-DENPEDANUM       TO  SPA-API-DENPEDANUM
      *
           .
       200311-DB-TORIKESHI-EXT.
           EXIT.
      *****************************************************************
      *    データベース更新処理（再計算）
      *****************************************************************
       200311-DB-SAIKEISAN-SEC             SECTION.
      *
           INITIALIZE                  SSUB05-AREA
           MOVE    "1"             TO  SSUB05-SYORIKBN
           MOVE    SPA-API-NYUGAIKBN
                                   TO  SSUB05-NYUGAIKBN
           MOVE    SPA-PTID        TO  SSUB05-PTID
           MOVE    SPA-API-DENPNUM
                                   TO  SSUB05-DENPNUM
           MOVE    TSYU-SRYKA      TO  SSUB05-SRYKA
           MOVE    TSYU-SRYYMD (1:6)
                                   TO  SSUB05-SKYYM
           MOVE    SPA-API-NYUHENYMD  TO  SSUB05-SYSYMD
           MOVE    SPA-API-NYUHENHMS  TO  SSUB05-SYSHMS
           CALL    "ORCGSSUB05"    USING
                                   SSUB05-AREA
                                   SPA-AREA
           IF    ( SPA-ERRCD       =   "0070" )
               MOVE    "4009"      TO  SPA-ERRCD
           END-IF
      *
           .
       200311-DB-SAIKEISAN-EXT.
           EXIT.
      *****************************************************************
      *    データベース更新処理（一括再計算）
      *****************************************************************
       200311-DB-M-SAIKEISAN-SEC             SECTION.
      *
           INITIALIZE                  SSUB05-AREA
           MOVE    "2"             TO  SSUB05-SYORIKBN
           MOVE    SPA-PTID        TO  SSUB05-PTID
           MOVE    SPA-API-SRYYM   TO  SSUB05-SKYYM
           MOVE    SPA-API-NYUHENYMD
                                   TO  SSUB05-SYSYMD
           MOVE    SPA-API-NYUHENHMS
                                   TO  SSUB05-SYSHMS
           CALL    "ORCGSSUB05"    USING
                                   SSUB05-AREA
                                   SPA-AREA
           IF    ( SPA-ERRCD       =   "0070" )
               MOVE    "4009"      TO  SPA-ERRCD
           END-IF
      *
           .
       200311-DB-M-SAIKEISAN-EXT.
           EXIT.
      *****************************************************************
      *    データベース更新処理（入金）
      *****************************************************************
       200311-DB-M-NYUKIN-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDXB
      *
           MOVE    HIGH-VALUE          TO  KEY-GAI-MISYU (1)
           MOVE    LOW-VALUE           TO  KEY-GAI-SKYEDYMD
      *
           INITIALIZE                      ORCSNYUKINAREAS
           MOVE    SPA-HOSPNUM         TO  LNK-SCNYUKINS-HOSPNUM
           MOVE    SPA-PTID            TO  LNK-SCNYUKINS-PTID
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA        >   SPA-API-SYUNOU-MAX )
      *
               IF    ( SPA-API-TDENPNUM (IDXA)
                                       NOT =   ZERO )
                AND  ( SPA-API-TNYUGAIKBN (IDXA)
                                       NOT =   SPACE )
      *
                   COMPUTE IDXB  = IDXB  + 1
      *
                   MOVE    SPA-API-TNYUGAIKBN (IDXA)
                                   TO  LNK-SCNYUKINS-NYUGAIKBN (IDXB)
                   MOVE    SPA-API-TDENPNUM (IDXA)
                                   TO  LNK-SCNYUKINS-DENPNUM   (IDXB)
                   MOVE    SPA-API-TNYUKIN (IDXA)
                                   TO  LNK-SCNYUKINS-NYUKIN-MONEY (IDXB)
                   IF    ( SPA-API-TNYUHENKBN (IDXA)   =   "1" )
                       MOVE    SPA-API-TNYUKIN-HOHO (IDXA)
                                   TO  LNK-SCNYUKINS-NYUKIN-HOHO2 (IDXB)
                   END-IF
      *
                   MOVE    SPA-API-TNYUHENYMD (IDXA)
                                   TO  LNK-SCNYUKINS-NYUKIN-YMD2 (IDXB)
                   MOVE    SPA-API-TNYUHENHMS (IDXA)
                                   TO  LNK-SCNYUKINS-NYUKIN-HMS2 (IDXB)
      *
                   IF    ( SPA-API-TNYUGAIKBN (IDXA)   =   "1" )
                       MOVE    1       TO  FLG-NYU-SKYPRT
                       COMPUTE WRK-NYU-NYUKIN
                           =   WRK-NYU-NYUKIN
                           +   SPA-API-TNYUKIN  (IDXA)
                       COMPUTE WRK-NYU-KONSEIKYU
                           =   WRK-NYU-KONSEIKYU
                           +   SPA-API-TMISYU   (IDXA)
                       IF    ( SPA-API-TDENPNUM  (IDXA)
                                       >   WRK-NYU-DENPNUM )
                           MOVE    SPA-API-TDENPNUM  (IDXA)
                                       TO  WRK-NYU-DENPNUM
                           MOVE    SPA-API-TSRYYMD   (IDXA)
                                       TO  WRK-NYU-SRYYMD
                       END-IF
                   ELSE
                       MOVE    1       TO  FLG-GAI-SKYPRT
                       COMPUTE WRK-GAI-NYUKIN
                           =   WRK-GAI-NYUKIN
                           +   SPA-API-TNYUKIN  (IDXA)
                       COMPUTE WRK-GAI-KONSEIKYU
                           =   WRK-GAI-KONSEIKYU
                           +   SPA-API-TMISYU   (IDXA)
                       IF    ( SPA-API-TDENPNUM  (IDXA)
                                       >   WRK-GAI-DENPNUM )
                           MOVE    SPA-API-TDENPNUM  (IDXA)
                                       TO  WRK-GAI-DENPNUM
                           MOVE    SPA-API-TSRYYMD   (IDXA)
                                       TO  WRK-GAI-SRYYMD
                       END-IF
      *
                       MOVE    SPA-API-TSRYYMD  (IDXA)
                                       TO  KEY-SRYYMD (2)
                       MOVE    SPA-API-TDENPNUM  (IDXA)
                                       TO  KEY-DENPNUM (2)
      *
                       IF    ( KEY-GAI-MISYU (1)
                                       >   KEY-GAI-MISYU (2))
                           MOVE    KEY-GAI-MISYU (2)
                                       TO  KEY-GAI-MISYU (1)
                       END-IF
                       IF    ( KEY-GAI-SKYEDYMD
                                       <   SPA-API-TSRYYMD  (IDXA))
                           MOVE    SPA-API-TSRYYMD  (IDXA)
                                       TO  KEY-GAI-SKYEDYMD
                       END-IF
                   END-IF

               END-IF
      *
           END-PERFORM
      *
           MOVE    IDXB            TO  LNK-SCNYUKINS-MAX
      *
           CALL    "ORCSNYUKINS"   USING   ORCSNYUKINAREAS
                                           SPA-AREA
           IF    ( LNK-SCNYUKINS-RC    NOT =   ZERO )
               MOVE    "4000"      TO  SPA-ERRCD
           END-IF
      *
           .
       200311-DB-M-NYUKIN-EXT.
           EXIT.
      *****************************************************************
      *    データベース更新処理（請求取消）
      *****************************************************************
       200311-DB-SKY-TORIKESHI-SEC     SECTION.
      *
           INITIALIZE                  SS016-AREA
           MOVE    SPA-PTID            TO  SS016-PTID
           MOVE    SPA-API-DENPNUM     TO  SS016-DENPNUM
           MOVE    CONST-REQMODE-SKY-TORIKESHI
                                       TO  SS016-REQMODE
           MOVE    SPA-API-NYUHENYMD   TO  SS016-NYUKINYMD
           MOVE    SPA-API-NYUHENHMS   TO  SS016-NYUKINHMS
           CALL    "ORCSS016"          USING
                                       SS016-AREA
                                       TSYU-REC
                                       TSMEI-REC
                                       SPA-AREA
      *
           .
       200311-DB-SKY-TORIKESHI-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       20031-PRINT-SEC                 SECTION.
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
           PERFORM 2800-SPAKYOTUKEY-SET-SEC
      *
           EVALUATE    INCOMEREQ01-REQMODE
           WHEN    CONST-REQMODE-NYUKIN
           WHEN    CONST-REQMODE-HENKIN
           WHEN    CONST-REQMODE-RRKSYU
           WHEN    CONST-REQMODE-TORIKESHI
               PERFORM 200311-PRINT-SEC
           WHEN    CONST-REQMODE-M-NYUKIN
               PERFORM 200312-PRINT-SEC
           END-EVALUATE
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
       200311-PRINT-SEC                SECTION.
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
       200311-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理（一括入返金）
      *****************************************************************
       200312-PRINT-SEC                SECTION.
      *
           IF    ( FLG-GAI-SKYPRT      =   1 )
            AND  ( SPA-API-SKYPRT      =   "1" )
               PERFORM 2003121-GAI-PRINT-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( FLG-NYU-SKYPRT      =   1 )
                AND  ( SPA-API-SKYPRT      =   "1" )
                   PERFORM 2003121-NYU-PRINT-SEC
               END-IF
           END-IF
      *
           .
       200312-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理（一括入返金）
      *****************************************************************
       2003121-GAI-PRINT-SEC           SECTION.
      *
           MOVE    "2"                 TO  SPA-NYUGAIKBN
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    KEY-DENPNUM(1)      TO  SYU-DENPNUM
           PERFORM 900-SYUNOU-KEY-SEL-SEC
           MOVE    ZERO                TO  SYU-GRP-DENPNUM
           MOVE    KEY-GAI-SKYEDYMD    TO  SYU-SKYEDYMD
           INITIALIZE                  SMISYU-AREA
           CALL    "ORCSMISYU"         USING
                                       SYUNOU-REC
                                       SMISYU-AREA
                                       SPA-AREA
           MOVE    SMISYU-GAIRAI-P     TO  WRK-GAI-MISYU-P
           MOVE    SMISYU-GAIRAI-M     TO  WRK-GAI-MISYU-M
      *
           INITIALIZE                  SYUPARA01-AREA
                                       SYUPARA02-AREA
                                       TSRT-AREA
      *
           MOVE    SPA-NYUGAIKBN       TO  SYUPARA01-NYUGAIKBN
           MOVE    SMISYU-NYUIN        TO  SYUPARA01-NYU-MISYU
           MOVE    SMISYU-GAIRAI       TO  SYUPARA01-GAI-MISYU
           IF    ( SYS-1039-MISYUKIKANKBN  =   "1" )
               CONTINUE
           ELSE
               COMPUTE SYUPARA01-GAI-MISYU
                   =   SYUPARA01-GAI-MISYU   -   WRK-GAI-KONSEIKYU
               COMPUTE SYUPARA01-NYU-MISYU
                   =   SYUPARA01-NYU-MISYU   -   WRK-NYU-KONSEIKYU
               IF    ( WRK-GAI-KONSEIKYU     <   WRK-GAI-NYUKIN )
                   COMPUTE SYUPARA01-GAI-MISYU-M
                       =   SYUPARA01-GAI-MISYU-M
                       - ( WRK-GAI-KONSEIKYU -   WRK-GAI-NYUKIN )
               ELSE
                   COMPUTE SYUPARA01-GAI-MISYU-P
                       =   SYUPARA01-GAI-MISYU-P
                       - ( WRK-GAI-KONSEIKYU -   WRK-GAI-NYUKIN )
               END-IF
           END-IF
           MOVE    WRK-GAI-MISYU-P     TO  SYUPARA01-GAI-MISYU-P
           MOVE    WRK-GAI-MISYU-M     TO  SYUPARA01-GAI-MISYU-M
           MOVE    WRK-NYU-NYUKIN      TO  SYUPARA01-NYU-NYUKIN
           MOVE    WRK-GAI-NYUKIN      TO  SYUPARA01-GAI-NYUKIN
           MOVE    SPA-API-SKYGRP      TO  SYUPARA01-SKYSUMFLG
           MOVE    3                   TO  SYUPARA01-HAKKOFLG
           MOVE    SPA-SYSYMD          TO  SYUPARA01-DENPPRTYMD
      *
      D    DISPLAY "WRK-GAI-NYUKIN       [" WRK-GAI-NYUKIN "]"
      D    DISPLAY "WRK-GAI-KONSEIKYU    [" WRK-GAI-KONSEIKYU "]"
      D    DISPLAY "SYUPARA01-NYU-MISYU  [" SYUPARA01-NYU-MISYU "]"
      D    DISPLAY "SYUPARA01-GAI-MISYU  [" SYUPARA01-GAI-MISYU "]"
      D    DISPLAY "SYUPARA01-GAI-MISYU-P[" SYUPARA01-GAI-MISYU-P "]"
      D    DISPLAY "SYUPARA01-GAI-MISYU-M[" SYUPARA01-GAI-MISYU-M "]"
      *
           MOVE    "001%"          TO  WRK-FILEMEI-CD
           PERFORM 900-DELETE-PARA-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
               INITIALIZE                  PARA-REC
               MOVE    SPA-HOSPNUM     TO  PARA-HOSPNUM
               MOVE    "S02"           TO  PARA-GYOUMUID
               MOVE    SPA-TERMID      TO  PARA-TERMID
               MOVE    "00101"         TO  WRK-FILEMEI-CD
               MOVE    WRK-FILEMEI     TO  PARA-FILEMEI
               MOVE    1               TO  PARA-EDANUM
               MOVE    SYUPARA01-AREA  TO  PARA-DATA-REC
               MOVE    PARA-REC        TO  MCPDATA-REC
               MOVE    "tbl_para"      TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "4003"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0            >   SPA-API-SYUNOU-MAX )
               IF    ( SPA-API-TNYUGAIKBN (IDX0)   =   "2"  )
                AND  ( SPA-API-TNYUKIN (IDX0)  NOT =   ZERO )
                   COMPUTE TSRT-MAX    =   TSRT-MAX    +   1
                   MOVE    SPA-API-TSRYYMD (IDX0)
                                       TO  TSRT-SRYYMD(TSRT-MAX)
                   MOVE    SPA-API-TSRYKA (IDX0)
                                       TO  TSRT-SRYKA(TSRT-MAX)
                   MOVE    SPA-API-THKNCOMBI (IDX0)
                                       TO  TSRT-HKNCOMBI(TSRT-MAX)
                   MOVE    SPA-API-TDENPNUM (IDX0)
                                       TO  TSRT-DENPNUM(TSRT-MAX)
                   MOVE    SPA-API-TNYUKIN (IDX0)
                                       TO  TSRT-NYUKIN (TSRT-MAX)
               END-IF
           END-PERFORM
      *
           COMPUTE IDX-TMP =   SPA-API-SYUNOU-MAX  +   1
           PERFORM VARYING IDXA FROM    1   BY  1
                   UNTIL ( IDXA     >   TSRT-MAX - 1)
               COMPUTE WRK-STIDX   =   IDXA +   1
               PERFORM VARYING IDX-SRT FROM    WRK-STIDX   BY  1
                       UNTIL ( IDX-SRT >   TSRT-MAX )
      *
                   IF    ( TSRT-KEY (IDXA)
                                       >   TSRT-KEY (IDX-SRT))
                       MOVE    TSRT-OCC (IDXA)
                                       TO  TSRT-OCC (IDX-TMP)
                       MOVE    TSRT-OCC (IDX-SRT)
                                       TO  TSRT-OCC (IDXA)
                       MOVE    TSRT-OCC (IDX-TMP)
                                       TO  TSRT-OCC (IDX-SRT)
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0            >   TSRT-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                   COMPUTE PARA-EDANUM =   PARA-EDANUM +   1
      *
                   INITIALIZE              SYUPARA02-AREA
                   MOVE    TSRT-DENPNUM (IDX0)
                                       TO  SYUPARA02-DENPNUM
                   MOVE    TSRT-NYUKIN (IDX0)
                                       TO  SYUPARA02-NYUKIN
                   MOVE    SYUPARA02-AREA
                                       TO  PARA-DATA-REC
                   MOVE    PARA-REC    TO  MCPDATA-REC
                   MOVE    "tbl_para"      TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       MOVE    "4004"      TO  SPA-ERRCD
                   END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-ERRCD           =       SPACE )
               INITIALIZE                      ORCHC03S02AREA
               MOVE    SPA-HOSPNUM     TO      ORCHC03S02-HOSPNUM
               MOVE    "S02"           TO      ORCHC03S02-GYOUMUID
               MOVE    SPA-TERMID      TO      ORCHC03S02-TERMID
               MOVE    "00101"         TO      WRK-FILEMEI-CD
               MOVE    WRK-FILEMEI     TO      ORCHC03S02-FILEMEI
               CALL    "ORCHC03S02"    USING   SPA-AREA
                                               ORCHC03S02AREA
               IF    ( ORCHC03S02-RC   NOT =   ZERO )
                   MOVE    "4005"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =       SPACE )
      *
               MOVE    WRK-GAI-SRYYMD      TO  SPA-SRYYMD
               INITIALIZE                      ORCHC03AREA
               MOVE    WRK-GAI-DENPNUM     TO  ORCHC03-DENPNUM
               MOVE    3                   TO  ORCHC03-HAKKOFLG
               MOVE    ZERO                TO  ORCHC03-HAKHOUFLG
               MOVE    1                   TO  ORCHC03-PARAFLG
      *
               MOVE    "S02"               TO  ORCHC03-PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  ORCHC03-PARA-TERMID
               MOVE    "00102"             TO  WRK-FILEMEI-CD
               MOVE    WRK-FILEMEI         TO  ORCHC03-PARA-FILEMEI
               MOVE    4                   TO  ORCHC03-MOTOID
      *
               MOVE    "01"                TO  SPA-PRT-FLG
               INITIALIZE                      SRPLNK-AREA
               MOVE    "INS"               TO  SRPLNK-MODE
               MOVE    RPKEY-SEIKYUSHO     TO  SRPLNK-VAL
               CALL    "ORCSRPLNK"             USING
                                               SRPLNK-AREA
                                               SPA-AREA
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
      *
      D        "CALL " ORCSPRTNM-PRTPG
      *
               CALL    ORCSPRTNM-PRTPG     USING
                                           SPA-AREA
                                           ORCHC03AREA
               MOVE    SPACE               TO  SPA-PRT-FLG
      *
           END-IF
      *
           MOVE    "001%"          TO  WRK-FILEMEI-CD
           PERFORM 900-DELETE-PARA-SEC
      *
           .
       2003121-GAI-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理（一括入返金）
      *****************************************************************
       2003121-NYU-PRINT-SEC           SECTION.
      *
           MOVE    "1"                 TO  SPA-NYUGAIKBN
      *
           MOVE    WRK-NYU-SRYYMD      TO  SPA-SRYYMD
      *
           INITIALIZE                      ORCHCN03AREA
           MOVE    SPA-HOSPNUM         TO  ORCHCN03-HEAD-HOSPNUM
           MOVE    SPA-STAFFCD         TO  ORCHCN03-STAFFCD
           MOVE    SPA-SYSYMD          TO  ORCHCN03-SYSYMD
           MOVE    SPA-TERMID          TO  ORCHCN03-TERMID
           MOVE    4                   TO  ORCHCN03-MOTOID
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0            >   SPA-API-SYUNOU-MAX )
      *
               IF    ( SPA-API-TNYUGAIKBN (IDX0)   =   "1" )
                 AND ( SPA-API-TNYUKIN  (IDX0) NOT =   ZERO )
      *
                   COMPUTE ORCHCN03-CNT    =   ORCHCN03-CNT    +   1
                   INITIALIZE                      SYUNOU-REC
                   MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
                   MOVE    SPA-PTID            TO  SYU-PTID
                   MOVE    SPA-API-TNYUGAIKBN (IDX0)
                                               TO  SYU-NYUGAIKBN
                   MOVE    SPA-API-TDENPNUM (IDX0)
                                               TO  SYU-DENPNUM
                   PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
                   MOVE    SPA-HOSPNUM     TO  ORCHCN03-HOSPNUM
                                                         (ORCHCN03-CNT)
                   MOVE    SPA-PTID        TO  ORCHCN03-PTID
                                                         (ORCHCN03-CNT)
                   MOVE    SPA-PTNUM       TO  ORCHCN03-PTNUM
                                                         (ORCHCN03-CNT)
                   MOVE    "1"             TO  ORCHCN03-NYUGAIKBN
                                                         (ORCHCN03-CNT)
      *
                   MOVE    SYU-SKYSTYMD    TO  ORCHCN03-KJNYMD
                                                         (ORCHCN03-CNT)
                   MOVE    SYU-DENPNUM     TO  ORCHCN03-DENPNUM
                                                         (ORCHCN03-CNT)
                   IF    ( SYU-NYUKIN-TOTAL
                                           =   ZERO )
                       MOVE    SYU-SKYMONEY-TAX-SAI
                                           TO  ORCHCN03-SEIKYU-TAX-SAI
                                                         (ORCHCN03-CNT)
                   END-IF
                   COMPUTE ORCHCN03-SEIKYU (ORCHCN03-CNT)
                       =   SYU-SKYMONEY
                       -   SYU-NYUKIN-TOTAL
                       +   SPA-API-TNYUKIN (IDX0)
                   MOVE    SPA-API-TNYUKIN (IDX0)
                                           TO  ORCHCN03-NYUKIN
                                                         (ORCHCN03-CNT)
                   MOVE    4               TO  ORCHCN03-HAKKOFLG
                                                         (ORCHCN03-CNT)
               END-IF
      *
           END-PERFORM
      *
           MOVE    "02"                    TO  SPA-PRT-FLG
           INITIALIZE                          SRPLNK-AREA
           MOVE    "INS"                   TO  SRPLNK-MODE
           MOVE    RPKEY-SEIKYUSHO-N       TO  SRPLNK-VAL
           CALL    "ORCSRPLNK"                 USING
                                               SRPLNK-AREA
                                               SPA-AREA
      *
           INITIALIZE                          ORCSPRTNMAREA
           MOVE    "00000003"              TO  ORCSPRTNM-KBNCD
           MOVE    "1"                     TO  ORCSPRTNM-NYUGAIKBN
           MOVE    "1"                     TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD              TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"                 USING
                                               ORCSPRTNMAREA
                                               SPA-AREA
                                               SYSKANRI-REC
      *
           CALL    ORCSPRTNM-PRTPG             USING
                                               ORCHCN03AREA
                                               SPA-AREA
           MOVE    SPACE                   TO  SPA-PRT-FLG
      *
           .
       2003121-NYU-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       20031-RESPONSE-SEC                  SECTION.
      *
           EVALUATE    INCOMEREQ01-REQMODE
           WHEN    CONST-REQMODE-M-NYUKIN
               PERFORM VARYING IDXA    FROM    1   BY  1
                       UNTIL ( IDXA    >   LNK-SCNYUKINS-MAX )
                   PERFORM 200311-RESPONSE-SEC
               END-PERFORM
           WHEN    CONST-REQMODE-M-SAIKEISAN
               PERFORM 200313-RESPONSE-SEC
           WHEN    OTHER
               PERFORM 200312-RESPONSE-SEC
           END-EVALUATE
      *
           IF    ( SPA-API-PRINT-MODE  NOT =   SPACE )
            AND  ( SPA-PUTTYPE             =   "PRINTED" )
               MOVE    SPA-PRT-UUID    TO  INCOMERES01-DATA-ID
                                           INCOMERES03-DATA-ID
           END-IF
      *
           .
       20031-RESPONSE-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       200311-RESPONSE-SEC             SECTION.
      *
           MOVE    SPA-PTNUM       TO  INCOMERES01-PTNUM
      *
           IF    ( LNK-SCNYUKINS-NYUGAIKBN (IDXA)
                                   =   "1" )
               MOVE    "I"         TO  INCOMERES01-TNYUGAIKBN (IDXA)
           ELSE
               MOVE    "O"         TO  INCOMERES01-TNYUGAIKBN (IDXA)
           END-IF
      *
           MOVE    LNK-SCNYUKINS-DENPNUM (IDXA)
                                   TO  INCOMERES01-TDENPNUM (IDXA)
      *
           MOVE    LNK-SCNYUKINS-SKYMONEY (IDXA)
                                   TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9   TO  INCOMERES01-TSEIKYU (IDXA)
      *
           MOVE    LNK-SCNYUKINS-NYUKIN-TOTAL (IDXA)
                                   TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9   TO  INCOMERES01-TNYUKIN (IDXA)
      *
           COMPUTE WRK-KINGAKU
               =   LNK-SCNYUKINS-SKYMONEY (IDXA)
               -   LNK-SCNYUKINS-NYUKIN-TOTAL (IDXA)
           MOVE    WRK-KINGAKU     TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9   TO  INCOMERES01-TMISYU  (IDXA)
      *
           MOVE    LNK-SCNYUKINS-DENPJTIKBN (IDXA)
                                   TO  WRK-SYU-JOTAIKBN
           PERFORM 800-SYU-JOTAIKBN-SEC
           MOVE    WRK-SYU-JOTAIKBN    TO  INCOMERES01-TJOTAIKBN (IDXA)
           MOVE    WRK-SYU-JOTAINAME   TO  INCOMERES01-TJOTAINAME(IDXA)
      *
           .
       200311-RESPONSE-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       200312-RESPONSE-SEC             SECTION.
      *
           MOVE    TSYU-REC        TO  SYUNOU-REC
           PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
           INITIALIZE                  SS015-AREA
           MOVE    SYU-SRYYMD      TO  SS015-SRYYMD
           CALL    "ORCSS015"          USING
                                       SS015-AREA
                                       SPA-AREA
      *
           PERFORM 2003121-SYUNOU-EDIT-SEC
      *
           PERFORM 2003121-SYUMEI-EDIT-SEC
      *
           .
       200312-RESPONSE-EXT.
           EXIT.
      *****************************************************************
      *    収納編集処理
      *****************************************************************
       2003121-SYUNOU-EDIT-SEC             SECTION.
      *
           MOVE    SPA-PTNUM       TO  INCOMERES03-PTNUM
      *
           IF    ( SPA-API-NYUGAIKBN
                                   =   "1" )
               MOVE    "I"         TO  INCOMERES03-NYUGAIKBN
           ELSE
               MOVE    "O"         TO  INCOMERES03-NYUGAIKBN
           END-IF
      *
           MOVE    SPA-API-DENPNUM
                                   TO  INCOMERES03-DENPNUM
      *
           MOVE    SYU-SKYMONEY
                                   TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9   TO  INCOMERES03-SEIKYU
      *
           MOVE    SYU-NYUKIN-TOTAL
                                   TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9   TO  INCOMERES03-NYUKIN
      *
           COMPUTE WRK-KINGAKU
               =   SYU-SKYMONEY
               -   SYU-NYUKIN-TOTAL
           MOVE    WRK-KINGAKU     TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9   TO  INCOMERES03-MISYU
      *
           MOVE    SYU-DENPJTIKBN  TO  WRK-SYU-JOTAIKBN
           PERFORM 800-SYU-JOTAIKBN-SEC
           MOVE    WRK-SYU-JOTAIKBN    TO  INCOMERES03-JOTAIKBN
           MOVE    WRK-SYU-JOTAINAME   TO  INCOMERES03-JOTAINAME
      *
           .
       2003121-SYUNOU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    収納明細編集処理
      *****************************************************************
       2003121-SYUMEI-EDIT-SEC          SECTION.
      *
           MOVE    ZERO            TO  IDXA
      *
           PERFORM 900-SYUMEI-KEY6-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUMEI  NOT =   ZERO )
                    OR   ( IDXA            >=  99 )
      *
               COMPUTE IDXA  = IDXA  + 1
      *
               MOVE    SMEI-DENPEDANUM TO  WRK-NOZ
               MOVE    WRK-NOZ         TO  INCOMERES03-TDENPEDANUM(IDXA)
      *
               MOVE    SMEI-NYUHEN-YMD TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE        TO  INCOMERES03-TNYUHENYMD (IDXA)
      *
               MOVE    SMEI-DAILYKEY(9:)
                                       TO  WRK-HMS
               PERFORM 800-TIME-SEC
               MOVE    WRK-TIME (1:5)  TO  INCOMERES03-TNYUHENTIME(IDXA)
      *
               MOVE    SMEI-JOUTAIKBN     TO  WRK-SMEI-JOTAIKBN
               PERFORM 800-SMEI-JOTAIKBN-SEC
      *
               IF    ( WRK-SMEI-PROC-FLG  = CONST-SEIKYU
                                        OR  CONST-TEISEI )
                   MOVE    SMEI-SKYMONEY   TO   WRK-KINGAKUM9
                   MOVE    WRK-KINGAKUM9
                                       TO   INCOMERES03-TSKYMONEY (IDXA)
      *
                   IF    ( SMEI-SYURRKNUM  NOT =   ZERO )
                       PERFORM 900-SYURRK-KEY-SEL-SEC
                       MOVE    SYURRK-CHOSEI1
                                       TO   WRK-KINGAKUMM
                       MOVE    WRK-KINGAKUMM
                                       TO  INCOMERES03-TCHOSEI1(IDXA)
                       MOVE    SYURRK-CHOSEI2
                                       TO   WRK-KINGAKUMM
                       MOVE    WRK-KINGAKUMM
                                       TO  INCOMERES03-TCHOSEI2(IDXA)
                   END-IF
               END-IF
      *
               MOVE    SMEI-NYUHEN-MONEY
                                       TO   WRK-KINGAKUM9
               MOVE    WRK-KINGAKUM9   TO   INCOMERES03-TNYUKIN (IDXA)
      *
               IF    ( SMEI-JOUTAIKBN  =   SPACE )
                   MOVE    "0"         TO  INCOMERES03-TJOTAI (IDXA)
               ELSE
                   MOVE    SMEI-JOUTAIKBN
                                       TO  INCOMERES03-TJOTAI (IDXA)
               END-IF
      *
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   SS015-SMEIJOTAILST-MAX )
                        OR   ( INCOMERES03-TJOTAI-NAME  (IDXA)
                                       NOT =   SPACE )
      *
                   IF    ( SS015-SMEIJOTAIL (IDXL)
                               =   SMEI-JOUTAIKBN )
                       MOVE    SS015-SMEIJOTAIMEIL (IDXL)
                               TO  INCOMERES03-TJOTAI-NAME (IDXA)
                   END-IF
      *
               END-PERFORM
      *
               IF    ( SMEI-NYUKIN-HOHO    NOT =   ZERO )
                AND  ( SMEI-NYUKIN-HOHO    NOT =   SPACE )
      *
                   MOVE    SMEI-NYUKIN-HOHO
                                   TO  INCOMERES03-TNYUKIN-HOHO (IDXA)
                   PERFORM VARYING IDXL    FROM    1   BY  1
                           UNTIL ( IDXL    >   SS015-NYKN-HOHOLST-MAX )
                            OR   ( INCOMERES03-TNYUKIN-HOHO-NAME (IDXA)
                                       NOT =   SPACE )
      *
                       IF    ( SS015-NYKN-HOHOL (IDXL)
                                           =   SMEI-NYUKIN-HOHO)
                           MOVE    SS015-NYKN-HOHOMEIL (IDXL)
                               TO  INCOMERES03-TNYUKIN-HOHO-NAME (IDXA)
                       END-IF
      *
                   END-PERFORM
      *
               END-IF
      *
               PERFORM 900-SYUMEI-KEY6-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2003121-SYUMEI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       200313-RESPONSE-SEC             SECTION.
      *
           MOVE    ZERO            TO  IDXA
      *
           MOVE    SPA-PTNUM       TO  INCOMERES01-PTNUM
      *
           PERFORM 901-SYUNOU-KEY14-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
      *
               COMPUTE IDXA    =  IDXA     +   1
      *
               IF    ( SYU-NYUGAIKBN
                                   =   "1" )
                   MOVE    "I"     TO  INCOMERES01-TNYUGAIKBN (IDXA)
               ELSE
                   MOVE    "O"     TO  INCOMERES01-TNYUGAIKBN (IDXA)
              END-IF
      *
              MOVE     SYU-DENPNUM TO  INCOMERES01-TDENPNUM (IDXA)
      *
              MOVE     SYU-SKYMONEY
                                   TO  WRK-KINGAKUM9
              MOVE     WRK-KINGAKUM9
                                   TO  INCOMERES01-TSEIKYU (IDXA)
      *
              MOVE     SYU-NYUKIN-TOTAL
                                   TO  WRK-KINGAKUM9
              MOVE     WRK-KINGAKUM9
                                   TO  INCOMERES01-TNYUKIN (IDXA)
      *
               COMPUTE WRK-KINGAKU
                   =   SYU-SKYMONEY
                   -   SYU-NYUKIN-TOTAL
               MOVE    WRK-KINGAKU     TO  WRK-KINGAKUM9
               MOVE    WRK-KINGAKUM9   TO  INCOMERES01-TMISYU  (IDXA)
      *
      *
               MOVE    SYU-DENPJTIKBN  TO  WRK-SYU-JOTAIKBN
               PERFORM 800-SYU-JOTAIKBN-SEC
               MOVE    WRK-SYU-JOTAIKBN
                                       TO  INCOMERES01-TJOTAIKBN (IDXA)
               MOVE    WRK-SYU-JOTAINAME
                                       TO  INCOMERES01-TJOTAINAME(IDXA)
      *
               PERFORM 900-SYUNOU-KEY14-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200313-RESPONSE-EXT.
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
           PERFORM 800-ORCUIDSET-SEC
           MOVE    C-UID               TO  SPA-S01-UID
                                           WRK-UID
      *
           .
       2800-SPAKYOTUKEY-SET-EXT.
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
           IF    ( INCOMEREQ01-REQMODE
                                   =   CONST-REQMODE-M-SAIKEISAN
                                    OR CONST-REQMODE-M-NYUKIN )
               PERFORM 901-XML-WRITE-SEC
           ELSE
               PERFORM 902-XML-WRITE-SEC
           END-IF
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    状態区分編集処理
      *****************************************************************
       800-SYU-JOTAIKBN-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-SYU-JOTAINAME
      *
           EVALUATE    WRK-SYU-JOTAIKBN
           WHEN    SPACE
               MOVE    "0"             TO  WRK-SYU-JOTAIKBN
               MOVE    "請求額なし"    TO  WRK-SYU-JOTAINAME
           WHEN    "1"
               MOVE    "入金済"        TO  WRK-SYU-JOTAINAME
           WHEN    "2"
               MOVE    "未入金"        TO  WRK-SYU-JOTAINAME
           WHEN    "3"
               MOVE    "請求取消"      TO  WRK-SYU-JOTAINAME
           END-EVALUATE
      *
           .
       800-SYU-JOTAIKBN-EXT.
           EXIT.
      *****************************************************************
      *    収納明細状態区分->処理フラグ変換処理
      *****************************************************************
       800-SMEI-JOTAIKBN-SEC             SECTION.
      *
           MOVE    SPACE              TO  WRK-SMEI-PROC-FLG
      *
           EVALUATE   SMEI-JOUTAIKBN
           WHEN    " "
           WHEN    "1"
           WHEN    "9"
               MOVE   CONST-SEIKYU    TO  WRK-SMEI-PROC-FLG
           WHEN    "3"
           WHEN    "7"
           WHEN    "8"
           WHEN    "5"
               MOVE   CONST-TEISEI    TO  WRK-SMEI-PROC-FLG
           WHEN    "2"
                MOVE  CONST-NYUKIN    TO  WRK-SMEI-PROC-FLG
           WHEN    "4"
           WHEN    "6"
                MOVE  CONST-MINUS-NYUKIN
                                      TO  WRK-SMEI-PROC-FLG
           END-EVALUATE
      *
           .
       800-SMEI-JOTAIKBN-EXT.
           EXIT.
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
      *    全角半角チェック処理
      *****************************************************************
       800-KANACHK-SEC                 SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-INSTR       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      ( KANACHK-RC    =   ZERO )
               MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-OUTSTR
           ELSE
               IF    ( KANACHK-RC2 =   7 )
                   MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-OUTSTR
               ELSE
                   MOVE    WRK-INSTR
                                   TO  WRK-OUTSTR
               END-IF
           END-IF
      *
           .
       800-KANACHK-EXT.
           EXIT.
      *****************************************************************
      *    全角半角チェック処理
      *****************************************************************
       801-KANACHK-UTF8-SEC            SECTION.
      *
           INITIALIZE                  ORCSKANACHK2AREA
           MOVE    "1"             TO  KANACHK2-SYORI
           MOVE    WRK-INSTR       TO  KANACHK2-MAE-INPUT
           CALL    "ORCSKANACHK2"  USING   ORCSKANACHK2AREA
           IF      ( KANACHK2-RC   =   ZERO )
               MOVE    KANACHK2-OUT-INPUT
                                   TO  WRK-OUTSTR
           ELSE
               IF    ( KANACHK2-RC2 =   7 )
                   MOVE    KANACHK2-OUT-INPUT
                                   TO  WRK-OUTSTR
               ELSE
                   MOVE    WRK-INSTR
                                   TO  WRK-OUTSTR
               END-IF
           END-IF
      *
           .
       801-KANACHK-UTF8-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-SEC             SECTION.
      *
           INITIALIZE                  WRK-OUTSTR
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE    WRK-INSTR
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      ( KANACHK-RC    =   ZERO )
               MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-OUTSTR
           ELSE
               IF    ( KANACHK-RC2 =   7 )
                   MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-OUTSTR
               ELSE
                   MOVE    WRK-INSTR
                                   TO  WRK-OUTSTR
               END-IF
           END-IF
      *
           .
       800-ZENKAKU-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       801-ZENKAKU-UTF8-SEC            SECTION.
      *
           INITIALIZE                  WRK-OUTSTR
      *
           INITIALIZE                  ORCSKANACHK2AREA
           MOVE    "2"             TO  KANACHK2-SYORI
           MOVE    WRK-INSTR
                                   TO  KANACHK2-MAE-INPUT
           CALL    "ORCSKANACHK2"  USING   ORCSKANACHK2AREA
           IF      ( KANACHK2-RC   =   ZERO )
               MOVE    KANACHK2-OUT-INPUT
                                   TO  WRK-OUTSTR
           ELSE
               IF    ( KANACHK2-RC2 =   7 )
                   MOVE    KANACHK2-OUT-INPUT
                                   TO  WRK-OUTSTR
               ELSE
                   MOVE    WRK-INSTR
                                   TO  WRK-OUTSTR
               END-IF
           END-IF
      *
           .
       801-ZENKAKU-UTF8-EXT.
           EXIT.
      *****************************************************************
      *    半角文字チェック
      *****************************************************************
       800-HANKAKU-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  ( WRK-INSTR NOT =   SPACE )
               MOVE    SPACE       TO  ORCSKANACHKAREA
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "1"         TO  KANACHK-SYORI
               MOVE    WRK-INSTR   TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      ( KANACHK-RC    =   ZERO )
                   IF      ( KANACHK-SYUBETU   NOT =   1 )
                       MOVE    1   TO  FLG-ENTRYCHK
                   END-IF
               ELSE
                       MOVE    1   TO  FLG-ENTRYCHK
               END-IF
           END-IF
      *
           .
       800-HANKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    文字列長取得
      *****************************************************************
       800-LENGTH-SEC              SECTION.
      *
      *    WRK-INSTRが空白の場合、FUNCTIONで長さを求めると１が返却される。
      *    空白の場合は0を返却するように調整
           MOVE    ZERO            TO  WRK-LEN
           IF    ( WRK-INSTR       NOT =   SPACE )
               MOVE    FUNCTION BYTE-LENGTH(
                           FUNCTION TRIM(WRK-INSTR TRAILING)
                       )           TO  WRK-LEN
           END-IF
      *
           .
       800-LENGTH-EXT.
           EXIT.
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
       901-XML-WRITE-SEC             SECTION.
      *
           MOVE    INCOMEREQ01-REQNUM        TO  INCOMERES01-REQNUM
           MOVE    INCOMEREQ01-REQMODE       TO  INCOMERES01-REQMODE
           MOVE    SPA-API-RESNUM            TO  INCOMERES01-RESNUM
      *
           MOVE    INCOMEREQ01-KARTE-UID     TO  INCOMERES01-KARTE-UID
           MOVE    INCOMEREQ01-ORCA-UID      TO  INCOMERES01-ORCA-UID
      *
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  INCOMERES01-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS    TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME        TO  INCOMERES01-INFORMATION-TIME
      *
           PERFORM 890-ERRCD-MSG-SEC
           IF    ( SPA-ERRCD       =   ZERO )
               MOVE    ZERO        TO  INCOMERES01-API-RESULT
           ELSE
               MOVE    "E"         TO  INCOMERES01-API-RESULT
               MOVE    SPA-ERRCD   TO  INCOMERES01-API-RESULT(2:)
           END-IF
           MOVE    SPA-ERRMSG      TO  INCOMERES01-API-RESULT-MSG
      *
           IF    ( SPA-API-ERRFLG  NOT =   ZERO )
      *
               MOVE    ZERO        TO  IDXB
      *
               PERFORM VARYING IDXA    FROM    1   BY  1
                       UNTIL ( IDXA    >   SPA-API-SYUNOU-MAX )
      *
                   IF    ( SPA-API-TERRCD (IDXA)   NOT =   SPACE )
                       COMPUTE IDXB    =   IDXB    +   1
                       MOVE    INCOMEREQ01-TDENPNUM (IDXA)
                                   TO  INCOMERES01-TDENPNUM (IDXB)
                       MOVE    INCOMEREQ01-TNYUGAIKBN (IDXA)
                                   TO  INCOMERES01-TNYUGAIKBN (IDXB)
                       MOVE    "E" TO  INCOMERES01-TERRCD (IDXB)
                       MOVE    SPA-API-TERRCD (IDXA)
                                   TO  INCOMERES01-TERRCD (IDXB)(2:)
                       MOVE    SPA-API-TERRCD (IDXA)
                                   TO  SPA-ERRCD
                       PERFORM 890-ERRCD-MSG-SEC
                       MOVE    SPA-ERRMSG
                                   TO  INCOMERES01-TERRMSG (IDXB)
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           MOVE    "xml_incomev3res01" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    INCOMERES01-AREA    TO  APIREQ-AREA
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
       901-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       902-XML-WRITE-SEC             SECTION.
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
           MOVE    "xml_incomev3res03" TO  MCP-TABLE
           MOVE    "key"             TO  MCP-PATHNAME
      *
           MOVE    INCOMERES03-AREA  TO  APIREQ-AREA
           MOVE    1                 TO  APIREQ-MODE
           MOVE    "incomev3res"     TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF    ( MCP-RC            =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT TO  SAPI01-BODY
               MOVE    "application/xml"
                                     TO  SAPI01-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       902-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
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
      *****************************************************************
      *    収納検索処理
      *****************************************************************
       900-SYUNOU-KEY14-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    SPA-API-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    SPA-API-SRYYM       TO  SYU-SRYYMD
           MOVE    "%"                 TO  SYU-SRYYMD(7:)
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY14-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納検索処理
      *****************************************************************
       901-SYUNOU-KEY14-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    SPA-API-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    SPA-API-SRYYM       TO  SYU-SRYYMD
           MOVE    "%"                 TO  SYU-SRYYMD(7:)
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       901-SYUNOU-KEY14-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納検索処理
      *****************************************************************
       900-SYUNOU-KEY14-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY14-FET-EXT.
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
      *************************************************************
      *    収納履歴検索処理
      *************************************************************
       900-SYURRK-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYURRK
      *
           INITIALIZE                      SYURRK-REC
           MOVE    SMEI-HOSPNUM        TO  SYURRK-HOSPNUM
           MOVE    SMEI-NYUGAIKBN      TO  SYURRK-NYUGAIKBN
           MOVE    SMEI-PTID           TO  SYURRK-PTID
           MOVE    SMEI-DENPNUM        TO  SYURRK-DENPNUM
           MOVE    SMEI-SYURRKNUM      TO  SYURRK-SYURRKNUM
           MOVE    SMEI-SYUEDANUM      TO  SYURRK-SYUEDANUM
           MOVE    SYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_syurrk"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYURRK-REC
           ELSE
               MOVE    1               TO  FLG-SYURRK
               INITIALIZE                  SYURRK-REC
           END-IF
      *
           MOVE    "tbl_syurrk"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYURRK-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納明細検索処理
      *****************************************************************
       900-SYUMEI-KEY6-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUMEI
      *
           INITIALIZE                  SYUMEI-REC
           MOVE    SYU-HOSPNUM     TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN   TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID        TO  SMEI-PTID
           MOVE    SYU-DENPNUM     TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUMEI-REC
           ELSE
               MOVE    1           TO  FLG-SYUMEI
               INITIALIZE              SYUMEI-REC
           END-IF
      *
           .
       900-SYUMEI-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納明細FETCH処理
      *****************************************************************
       900-SYUMEI-KEY6-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUMEI
      *
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUMEI-REC
           ELSE
               MOVE    1           TO  FLG-SYUMEI
               INITIALIZE              SYUMEI-REC
           END-IF
      *
           .
       900-SYUMEI-KEY6-FET-EXT.
           EXIT.
      *****************************************************************
      *    収納明細検索処理
      *****************************************************************
       900-SYUMEI-KEY7-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUMEI
      *
           INITIALIZE                  SYUMEI-REC
           MOVE    TSYU-HOSPNUM    TO  SMEI-HOSPNUM
           MOVE    TSYU-NYUGAIKBN  TO  SMEI-NYUGAIKBN
           MOVE    TSYU-PTID       TO  SMEI-PTID
           MOVE    TSYU-DENPNUM    TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUMEI-REC
           ELSE
               MOVE    1           TO  FLG-SYUMEI
               INITIALIZE              SYUMEI-REC
           END-IF
      *
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUMEI-KEY7-SEL-EXT.
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
