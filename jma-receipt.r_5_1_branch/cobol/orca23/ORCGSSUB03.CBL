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
       PROGRAM-ID.                 ORCGSSUB03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 収納
      *  コンポーネント名  : 請求書・明細書情報編集処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/11/25    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *  03.04.01    NACL-森脇     06/12/19  プレビュー追加
      *  03.05.00    NACL-太田     07/05/20  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-PTINF                       PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
           03  FLG-SYUNOU                      PIC 9(01).
           03  FLG-SYUMEI                      PIC 9(01).
           03  FLG-SYURRK                      PIC 9(01).
           03  FLG-PARA                        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                            PIC 9(05).
           03  IDXD                            PIC 9(05).
           03  IDXM                            PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-FILEMEI.
               05  WRK-FILEMEI-YMD             PIC X(08).
               05  WRK-FILEMEI-HMS             PIC X(06).
               05  WRK-FILEMEI-CD              PIC X(06).
           03  WRK-KBNCD                       PIC X(08).
           03  WRK-FORM-PG                     PIC X(32).
           03  WRK-KAKO-NYUKIN                 PIC S9(10).
           03  WRK-SKYPRT-SKYMONEY             PIC S9(10).
           03  WRK-SKYPRT-NYUKIN               PIC S9(10).
           03  WRK-SKYPRT-TAX-SAI              PIC S9(10).
           03  WRK-PAGE-CNT                    PIC 9(06).
      *
           COPY    "CPRPKEY.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1001.INC".
           COPY    "CPSK1039.INC".
           COPY    "CPSK5000.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細マスター
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    収納履歴
       01  SYURRK-REC.
           COPY    "CPSYURRK.INC".
      *
      *    パラメタテーブル	
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
           COPY    "CPSYUPARA01.INC".
           COPY    "CPSYUPARA02.INC".
           COPY    "CPSYUPARA03.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    オンライン帳票ＩＤ連絡
           COPY    "CPORCSRPLNK.INC".
      *
      *    請求書印刷パラメタ（外来）
           COPY    "CPORCHC03.INC".
      *    明細書印刷パラメタ（外来）
           COPY    "CPORCHCN04.INC".
      *    請求書印刷パラメタ（入院）
           COPY    "CPORCHCN03.INC".
      *    明細書印刷パラメタ（入院）
           COPY    "CPORCHC04.INC".
      *
      *    入院請求書兼領収書まとめサブ
           COPY    "CPORCHCN03S02.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    外来請求書兼領収書サブ
           COPY    "CPORCHC03S02.INC".
      *
      *    未収金編集サブ
       01  LNKSYU-AREA.
           COPY    "CPSYUNOU.INC"      REPLACING //SYU-//
                                       BY        //LNKSYU-//.
           COPY    "CPORCSMISYU.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY    "COMMON-SPA".
           COPY    "S01COMMON-SPA".
           COPY    "CPSSUB03.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           SPA-S01KYOUTU
           SSUB03-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE              FLG-AREA
                                   IDX-AREA
                                   WRK-AREA
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
      *    システム管理マスタ検索処理
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "1001"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1001-REC
      *
      *    システム管理マスタ検索処理
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "1039"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1039-REC
      *
      *    システム管理マスタ検索処理
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5000-REC
      *
           MOVE    SPA-S01-SRYYMD      TO  SPA-SRYYMD
           MOVE    SPA-S01-NYUGAIKBN   TO  SPA-NYUGAIKBN
           MOVE    SPA-S01-PTID        TO  SPA-PTID
           MOVE    SPA-S01-PTNUM       TO  SPA-PTNUM
           MOVE    SPA-S01-KANANAME    TO  SPA-KANANAME
           MOVE    SPA-S01-NAME        TO  SPA-NAME
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
      *    労災判定
           IF   (  SPA-S01-HKNNUM      =   "971" OR "973" )
               MOVE    1               TO  SPA-HKNKBN
           ELSE
               MOVE    ZERO            TO  SPA-HKNKBN
           END-IF
      *
           MOVE    SPA-S01-RSI-HKNKBN  TO  SPA-RSI-HKNKBN
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    SPA-S01-FILEMEI TO  WRK-FILEMEI
      *
           EVALUATE    SSUB03-REQNO
      *    発行指示初設定取得
           WHEN    1
               PERFORM 2001-SKYPRT-EDIT-SEC
               PERFORM 2001-SRYPRT-EDIT-SEC
      *    請求書兼領収書、診療費明細書印刷（伝票番号枝番設定）
           WHEN    2
               PERFORM 2001-PRINT-SEC
      *    請求書兼領収書、診療費明細書印刷（伝票番号枝番未設定）
           WHEN    3
               PERFORM 2003-PRINT-SEC
      *    請求書兼領収書、診療費明細書印刷（外来月別）
           WHEN    4
               PERFORM 200313-PRINT-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    請求書発行指示初期設定処理
      *****************************************************************
       2001-SKYPRT-EDIT-SEC        SECTION.
      *
           IF    ( SSUB03-SKYPRT       =   SPACE )
               IF    ( PTINF-SKYPRTFLG =   "0" OR "8" OR "9" OR "A" )
                   MOVE    "0"         TO  SSUB03-SKYPRT
               END-IF
           END-IF
      *
           IF    ( SSUB03-SKYPRT       =   SPACE )
               IF    ( SPA-NYUGAIKBN   =   "1" )
                   IF    ( SYS-1039-SKYPRTNKBN NOT =   SPACE )
                       MOVE    SYS-1039-SKYPRTNKBN
                                       TO  SSUB03-SKYPRT
                   END-IF
               ELSE
                   IF    ( SYS-1039-SKYPRTGKBN NOT =   SPACE )
                       MOVE    SYS-1039-SKYPRTGKBN
                                       TO  SSUB03-SKYPRT
                   END-IF
               END-IF
      *
           END-IF
      *
           IF    ( SSUB03-SKYPRT       =   SPACE )
               IF    ( SPA-NYUGAIKBN   =   "1" )
                   IF    ( SYS-5000-SKYPRTKBN  NOT =   SPACE )
                       IF    ( SYS-5000-SKYPRTKBN  =   "1" OR  "2" )
                           MOVE    "1"     TO  SSUB03-SKYPRT
                       END-IF
                   ELSE
                       MOVE    "0"         TO  SSUB03-SKYPRT
                   END-IF
               ELSE
                   IF    ( SYS-1001-SKYPRTFLG  NOT =   SPACE )
                       IF    ( SYS-1001-SKYPRTFLG      =   "1" OR  "2" )
                           MOVE    "1"     TO  SSUB03-SKYPRT
                       END-IF
                   ELSE
                       MOVE    "0"         TO  SSUB03-SKYPRT
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SSUB03-SKYPRT       =   SPACE )
               MOVE    "0"             TO  SSUB03-SKYPRT
           END-IF
      *
           .
       2001-SKYPRT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    明細書発行指示初期設定処理
      *****************************************************************
       2001-SRYPRT-EDIT-SEC        SECTION.
      *
           IF    ( SSUB03-SRYPRT       =   SPACE )
               IF    ( PTINF-SKYPRTFLG =   "0" OR "4" OR "5" OR "6" )
                   MOVE    "0"     TO  SSUB03-SRYPRT
               END-IF
           END-IF
      *
           IF    ( SSUB03-SRYPRT   =   SPACE )
               IF    ( SPA-NYUGAIKBN   =   "1" )
                   IF    ( SYS-1039-SRYPRTNKBN NOT =   SPACE )
                       MOVE    SYS-1039-SRYPRTNKBN
                                       TO  SSUB03-SRYPRT
                   END-IF
               ELSE
                   IF    ( SYS-1039-SRYPRTGKBN NOT =   SPACE )
                       MOVE    SYS-1039-SRYPRTGKBN
                                       TO  SSUB03-SRYPRT
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SSUB03-SRYPRT   =   SPACE )
               MOVE    "0"             TO  SSUB03-SRYPRT
           END-IF
      *
           .
       2001-SRYPRT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       2001-PRINT-SEC               SECTION.
      *
           IF    ( SSUB03-SKYPRT    =   "1" )
               PERFORM 20010-SKYPRT-PRINT-SEC
           END-IF
      *
           IF    ( SSUB03-SRYPRT    =   "1" )
               PERFORM 20010-SRYPRT-PRINT-SEC
           END-IF
      *
           .
       2001-PRINT-EXT.
           EXIT.
      *************************************************************
      *    請求書兼領収書印刷処理
      *************************************************************
       20010-SKYPRT-PRINT-SEC          SECTION.
      *
           IF    ( SPA-NYUGAIKBN   =   "1" )
               PERFORM 20011-SKYPRT-PRINT-SEC
           ELSE
               PERFORM 20012-SKYPRT-PRINT-SEC
           END-IF
      *
           .
       20010-SKYPRT-PRINT-SEC-EXT.
           EXIT.
      *************************************************************
      *    診療費明細書印刷処理
      *************************************************************
       20010-SRYPRT-PRINT-SEC          SECTION.
      *
           IF    ( SPA-NYUGAIKBN   =   "1" )
               PERFORM 20011-SRYPRT-PRINT-SEC
           ELSE
               PERFORM 20012-SRYPRT-PRINT-SEC
           END-IF
      *
           .
       20010-SRYPRT-SEC-EXT.
           EXIT.
      *************************************************************
      *    請求書兼領収書印刷処理（入院）
      *************************************************************
       20011-SKYPRT-PRINT-SEC          SECTION.
      *
           PERFORM 200111-SKYPRT-PRINT-SEC
      *
           INITIALIZE                  ORCHCN03AREA
      *
           MOVE    SPA-HOSPNUM     TO  ORCHCN03-HEAD-HOSPNUM
           MOVE    SPA-STAFFCD     TO  ORCHCN03-STAFFCD
           MOVE    SPA-SYSYMD      TO  ORCHCN03-SYSYMD
           MOVE    SPA-TERMID      TO  ORCHCN03-TERMID
           MOVE    1               TO  ORCHCN03-CNT
           MOVE    1               TO  ORCHCN03-SKYPRTFLG
      *
           MOVE    SPA-HOSPNUM     TO  ORCHCN03-HOSPNUM (1)
           MOVE    SPA-PTID        TO  ORCHCN03-PTID   (1)
           MOVE    SPA-PTNUM       TO  ORCHCN03-PTNUM  (1)
           MOVE    "1"             TO  ORCHCN03-NYUGAIKBN (1)
      *
           MOVE    SPA-S01-SRYYMD  TO  ORCHCN03-KJNYMD  (1)
           MOVE    SPA-S01-DENPNUM TO  ORCHCN03-DENPNUM (1)
           IF    ( WRK-KAKO-NYUKIN >   ZERO )
               CONTINUE
           ELSE
               MOVE    WRK-SKYPRT-TAX-SAI
                                   TO  ORCHCN03-SEIKYU-TAX-SAI
                                                        (1)
           END-IF
           MOVE    WRK-SKYPRT-SKYMONEY
                                   TO  ORCHCN03-SEIKYU  (1)
           MOVE    WRK-SKYPRT-NYUKIN
                                   TO  ORCHCN03-NYUKIN  (1)
           MOVE    3               TO  ORCHCN03-HAKKOFLG(1)
           MOVE    SSUB03-DENPEDANUM
                                   TO  ORCHCN03-DENPEDANUM (1)
           MOVE    3               TO  ORCHCN03-MOTOID
      *
      D    DISPLAY "20011-SKYPRT-PRINT-SEC"
      *
           PERFORM 28001-NYU-SKYPRT-SEC
      *
           .
       20011-SKYPRT-PRINT-EXT.
           EXIT.
      *************************************************************
      *    請求額・入金額編集処理
      *************************************************************
       200111-SKYPRT-PRINT-SEC         SECTION.
      *
           INITIALIZE                  WRK-SKYPRT-SKYMONEY
                                       WRK-SKYPRT-NYUKIN
                                       WRK-SKYPRT-TAX-SAI
                                       WRK-KAKO-NYUKIN
      *
           PERFORM 900-SYUMEI-KEY6-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUMEI  NOT =   ZERO )
                    OR   ( SMEI-DENPEDANUM >   SSUB03-DENPEDANUM )
      *
               COMPUTE WRK-SKYPRT-SKYMONEY
                   =   WRK-SKYPRT-SKYMONEY
                   +   SMEI-SKYMONEY
      *
               IF    ( SMEI-DENPEDANUM     =   SSUB03-DENPEDANUM )
                   MOVE    SMEI-NYUHEN-MONEY
                                           TO  WRK-SKYPRT-NYUKIN
               END-IF
      *
               IF    ( SMEI-DENPEDANUM     <   SSUB03-DENPEDANUM )
                   COMPUTE WRK-SKYPRT-SKYMONEY
                       =   WRK-SKYPRT-SKYMONEY
                       -   SMEI-NYUHEN-MONEY
      *
                   COMPUTE WRK-KAKO-NYUKIN
                       =   WRK-KAKO-NYUKIN
                       +   SMEI-NYUHEN-MONEY
      *
               END-IF
      *
               IF    ( SMEI-SYURRKNUM      NOT =   ZERO )
      *
                   PERFORM 900-SYURRK-KEY3-SEL-SEC
      *
                   PERFORM UNTIL ( FLG-SYURRK  NOT =   ZERO )
      *
                       COMPUTE WRK-SKYPRT-TAX-SAI
                           =   WRK-SKYPRT-TAX-SAI
                           +   SYURRK-SKYMONEY-TAX-SAI
      *
                       PERFORM 900-SYURRK-KEY3-FET-SEC
      *
                   END-PERFORM
      *
                   MOVE    "tbl_syurrk"        TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
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
       200111-SKYPRT-PRINT-EXT.
           EXIT.
      *************************************************************
      *    請求書兼領収書印刷処理（外来）
      *************************************************************
       20012-SKYPRT-PRINT-SEC           SECTION.
      *
           PERFORM 200111-SKYPRT-PRINT-SEC
      *
           PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
           INITIALIZE                  SMISYU-AREA
           MOVE    SYUNOU-REC          TO  LNKSYU-AREA
           MOVE    ZERO                TO  LNKSYU-GRP-DENPNUM
           MOVE    SYU-SRYYMD          TO  LNKSYU-SKYEDYMD
           CALL    "ORCSMISYU"         USING
                                       LNKSYU-AREA
                                       SMISYU-AREA
                                       SPA-AREA
      *
      *
           INITIALIZE                  SYUPARA01-AREA
                                       SYUPARA02-AREA
      *
           MOVE    "2"                 TO  SYUPARA01-NYUGAIKBN
           MOVE    WRK-SKYPRT-SKYMONEY TO  SYUPARA01-SEIKYU
           IF   ( WRK-KAKO-NYUKIN      >   ZERO )
               CONTINUE
           ELSE
               MOVE   WRK-SKYPRT-TAX-SAI
                                       TO  SYUPARA01-SEIKYU-TAX-SAI
           END-IF
           MOVE    WRK-SKYPRT-NYUKIN   TO  SYUPARA01-NYUKIN
      *
           MOVE    SMISYU-NYUIN        TO  SYUPARA01-NYU-MISYU
           MOVE    SMISYU-GAIRAI       TO  SYUPARA01-GAI-MISYU
           MOVE    SMISYU-GAIRAI-P     TO  SYUPARA01-GAI-MISYU-P
           MOVE    SMISYU-GAIRAI-M     TO  SYUPARA01-GAI-MISYU-M
      *
           MOVE    ZERO                TO  SYUPARA01-NYU-NYUKIN
           MOVE    ZERO                TO  SYUPARA01-GAI-NYUKIN
           MOVE    3                   TO  SYUPARA01-HAKKOFLG
           MOVE    9                   TO  SYUPARA01-SKYSUMFLG
           MOVE    SPA-SYSYMD          TO  SYUPARA01-DENPPRTYMD
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    SPA-S01-GYOUMUID    TO  PARA-GYOUMUID
           MOVE    SPA-S01-UID         TO  PARA-TERMID
           MOVE    "00101"             TO  WRK-FILEMEI-CD
           MOVE    WRK-FILEMEI         TO  PARA-FILEMEI
           MOVE    1                   TO  PARA-EDANUM
      *
           IF    ( SYS-1039-MISYUKIKANKBN  =   "1" )
               CONTINUE
           ELSE
      *
               COMPUTE SYUPARA01-GAI-MISYU
                   =   SYUPARA01-GAI-MISYU
                   - ( SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL )
      *
               IF    ( SYU-SKYMONEY        <   SYU-NYUKIN-TOTAL )
                   COMPUTE SYUPARA01-GAI-MISYU-M
                       =   SYUPARA01-GAI-MISYU-M
                       - ( SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL )
               ELSE
                   COMPUTE SYUPARA01-GAI-MISYU-P
                       =   SYUPARA01-GAI-MISYU-P
                       - ( SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL )
               END-IF
           END-IF
      *
           PERFORM 200121-SYUPARA02-HEN-SEC
      *
           IF    ( SYS-1039-S03SKYMISYUPRTKBN  =   "0" )
               MOVE    ZERO            TO  SYUPARA01-NYU-MISYU
                                           SYUPARA01-GAI-MISYU
                                           SYUPARA01-GAI-MISYU-P
                                           SYUPARA01-GAI-MISYU-M
           END-IF
      *
           COMPUTE SYUPARA01-ZENMISYU
               =   SYUPARA01-GAI-MISYU
      *
           IF    ( SYUPARA01-ZENMISYU  <   ZERO )
               MOVE    SYUPARA01-ZENMISYU
                                       TO  SYUPARA01-HENKIN
                   MOVE    ZERO        TO  SYUPARA01-ZENMISYU
           END-IF
      *
           MOVE    SYUPARA01-GAI-MISYU-P
                                       TO  SYUPARA01-ZENMISYU-2
      *
           COMPUTE SYUPARA01-KANYUKIN
               =   SYUPARA01-GAI-MISYU-M     *   -1
      *
           PERFORM 200121-SYUPARA01-HEN-SEC
      *
           PERFORM 200121-ORCHC03S02-SEC
      *
           INITIALIZE                      ORCHC03AREA
           MOVE    SPA-S01-DENPNUM     TO  ORCHC03-DENPNUM
           MOVE    SYUPARA01-SEIKYU    TO  ORCHC03-SEIKYU
           MOVE    SYUPARA01-SEIKYU-TAX-SAI
                                       TO  ORCHC03-SEIKYU-TAX-SAI
           MOVE    SYUPARA01-NYUKIN    TO  ORCHC03-NYUKIN
      *
           MOVE    SYUPARA01-ZENMISYU-2 TO ORCHC03-ZENMISYU-2
           MOVE    SYUPARA01-KANYUKIN  TO  ORCHC03-KANYUKIN
           MOVE    SYUPARA01-ZENMISYU  TO  ORCHC03-ZENMISYU
           MOVE    SYUPARA01-HENKIN    TO  ORCHC03-HENKIN
      *
           MOVE    3                   TO  ORCHC03-HAKKOFLG
           MOVE    1                   TO  ORCHC03-PARAFLG
           MOVE    SSUB03-DENPEDANUM   TO  ORCHC03-DENPEDANUM
           MOVE    3                   TO  ORCHC03-MOTOID
           MOVE    SPA-S01-GYOUMUID    TO  ORCHC03-PARA-GYOUMUID
           MOVE    SPA-S01-UID         TO  ORCHC03-PARA-TERMID
           MOVE    "00102"             TO  WRK-FILEMEI-CD
           MOVE    WRK-FILEMEI         TO  ORCHC03-PARA-FILEMEI
      *
      D    DISPLAY "20012-SKYPRT-PRINT-SEC"
      *
           PERFORM 28001-GAI-SKYPRT-SEC
      *
           MOVE    "001%"              TO  WRK-FILEMEI-CD
           PERFORM 200121-DELETE-PARA-SEC
      *
           .
       20012-SKYPRT-PRINT-EXT.
           EXIT.
      **************************************************************
      *    請求書用パラメタテーブル編集処理
      **************************************************************
       200121-SYUPARA02-HEN-SEC        SECTION.
      *
           COMPUTE PARA-EDANUM =   PARA-EDANUM +   1
      *
           INITIALIZE                  SYUPARA02-AREA
           MOVE    SYU-DENPNUM     TO  SYUPARA02-DENPNUM
           MOVE    SYU-NYUKIN-TOTAL TO  SYUPARA02-NYUKIN
           MOVE    SYUPARA02-AREA  TO  PARA-DATA-REC
           MOVE    PARA-REC        TO  MCPDATA-REC
           MOVE    "tbl_para"      TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           .
       200121-SYUPARA02-HEN-EXT.
           EXIT.
      **************************************************************
      *    請求書用パラメタテーブル編集処理
      **************************************************************
       200121-SYUPARA01-HEN-SEC        SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    SPA-S01-GYOUMUID    TO  PARA-GYOUMUID
           MOVE    SPA-S01-UID         TO  PARA-TERMID
           MOVE    "00101"             TO  WRK-FILEMEI-CD
           MOVE    WRK-FILEMEI         TO  PARA-FILEMEI
           MOVE    1                   TO  PARA-EDANUM
           MOVE    SYUPARA01-AREA      TO  PARA-DATA-REC
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
      *
           PERFORM 910-DBINSERT-SEC
      *
           .
       200121-SYUPARA01-HEN-EXT.
           EXIT.
      **************************************************************
      *    請求書編集サブ呼出処理
      **************************************************************
       200121-ORCHC03S02-SEC           SECTION.
      *
           INITIALIZE                      ORCHC03S02AREA
           MOVE    SPA-HOSPNUM         TO  ORCHC03S02-HOSPNUM
           MOVE    SPA-S01-GYOUMUID    TO  ORCHC03S02-GYOUMUID
           MOVE    SPA-S01-UID         TO  ORCHC03S02-TERMID
           MOVE    "00101"             TO  WRK-FILEMEI-CD
           MOVE    WRK-FILEMEI         TO  ORCHC03S02-FILEMEI
           CALL    "ORCHC03S02"    USING   SPA-AREA
                                           ORCHC03S02AREA
      *
           .
       200121-ORCHC03S02-EXT.
           EXIT.
      *************************************************************
      *    パラメタ削除処理
      *************************************************************
       200121-DELETE-PARA-SEC            SECTION.
      *
           INITIALIZE                  PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    SPA-S01-GYOUMUID    TO  PARA-GYOUMUID
           MOVE    SPA-S01-UID         TO  PARA-TERMID
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
       200121-DELETE-PARA-EXT.
           EXIT.
      *************************************************************
      *    診療費明細書印刷処理（入院）
      *************************************************************
       20011-SRYPRT-PRINT-SEC          SECTION.
      *
           INITIALIZE                  ORCHCN04AREA
      *
           MOVE    SPA-PTID        TO  ORCHCN04-PTID
           MOVE    SPA-PTNUM       TO  ORCHCN04-PTNUM
           MOVE    SPA-NAME        TO  ORCHCN04-NAME
           MOVE    SPA-S01-DENPNUM TO  ORCHCN04-DENPNUM (1)
           MOVE    SSUB03-DENPEDANUM
                                   TO  ORCHCN04-DENPEDANUM (1)
           MOVE   3                TO  ORCHCN04-HAKKOFLG
      *
      D    DISPLAY "20011-SRYPRT-PRINT-SEC"
      *
           PERFORM 28001-NYU-SRYPRT-SEC
      *
           .
       20011-SRYPRT-PRINT-EXT.
           EXIT.
      *************************************************************
      *    明細書印刷処理（外来）
      *************************************************************
       20012-SRYPRT-PRINT-SEC           SECTION.
      *
           INITIALIZE                      ORCHC04AREA
      *
           MOVE   SPA-S01-DENPNUM      TO  ORCHC04-DENPNUM
           MOVE   3                    TO  ORCHC04-HAKKOFLG
      *
      D    DISPLAY "20012-SRYPRT-PRINT-SEC"
      *
           PERFORM 28001-GAI-SRYPRT-SEC
      *
           .
       20012-SRYPRT-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       2003-PRINT-SEC               SECTION.
      *
           IF    ( SPA-NYUGAIKBN       =   "1" )
               PERFORM 2003111-NYU-REPRINT-SEC
           ELSE
               PERFORM 2003111-GAI-REPRINT-SEC
           END-IF
      *
           .
       2003-PRINT-EXT.
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
           MOVE    SPA-S01-DENPNUM     TO  SYU-DENPNUM
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
               IF    ( SSUB03-SKYPRT   =   "1" )
      *
      D            DISPLAY "2003111-NYU-REPRINT-SEC"
                   PERFORM 28001-NYU-SKYPRT-SEC
               END-IF
      *
               IF    ( SSUB03-SRYPRT   =   "1" )
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
           IF    ( SSUB03-SKYPRT       =   "1" )
               PERFORM 2800-GAI-SKYPRT-SEC
           END-IF
      *
           IF    ( SSUB03-SRYPRT       =   "1" )
               PERFORM 2800-GAI-SRYPRT-SEC
           END-IF
      *
           .
       20031111-GAI-SKYPRT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理（外来月別請求書兼領収書）
      *****************************************************************
       200313-PRINT-SEC                    SECTION.
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
           IF    ( SSUB03-SKYPRT       =   "1" )
               PERFORM 2800-GAI-TUKISKYPRT-SEC
           END-IF
      *
           IF    ( SSUB03-SRYPRT       =   "1" )
               PERFORM 2800-GAI-TUKISRYPRT-SEC
           END-IF
      *
           .
       20031111-GAI-TUKISKYPRT-EXT.
           EXIT.
      **************************************************************
      *    外来月まとめ請求書兼領収書発行処理
      **************************************************************
       2800-GAI-TUKISKYPRT-SEC         SECTION.
      *
           INITIALIZE                      ORCHC03AREA
           MOVE    SPA-S01-DENPNUM     TO  ORCHC03-DENPNUM
           MOVE    SYUPARA01-SEIKYU    TO  ORCHC03-SEIKYU
           MOVE    SYUPARA01-NYUKIN    TO  ORCHC03-NYUKIN
           MOVE    4                   TO  ORCHC03-HAKKOFLG
           MOVE    SYUPARA01-SKYSUMFLG TO  ORCHC03-HAKHOUFLG
           MOVE    1                   TO  ORCHC03-PARAFLG
           MOVE    SYUPARA01-ZENMISYU  TO  ORCHC03-ZENMISYU
           MOVE    SYUPARA01-ZENMISYU-2 TO ORCHC03-ZENMISYU-2
           MOVE    SYUPARA01-HENKIN    TO  ORCHC03-HENKIN
           MOVE    SYUPARA01-KANYUKIN  TO  ORCHC03-KANYUKIN
           MOVE    SPA-S01-GYOUMUID    TO  ORCHC03-PARA-GYOUMUID
           MOVE    SPA-S01-UID         TO  ORCHC03-PARA-TERMID
           MOVE    WRK-FILEMEI         TO  ORCHC03-PARA-FILEMEI
           MOVE    2                   TO  ORCHC03-MOTOID
      *
      D    DISPLAY "2800-GAI-TUKISKYPRT-SEC"
           PERFORM 28001-GAI-SKYPRT-SEC
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
      D        DISPLAY "2800-GAI-TUKISRYPRT-SEC"
               PERFORM 28001-GAI-SRYPRT-SEC
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
      *    診療費明細書出力処理
      *****************************************************************
       2800-NYU-SRYPRT-SEC             SECTION.
      *
           INITIALIZE              ORCHCN03S02AREA
           MOVE    1       TO      ORCHCN03S02-IDX
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
      D        DISPLAY "2800-NYU-SRYPRT-SEC"
               PERFORM 28001-NYU-SRYPRT-SEC
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
           MOVE    SPA-S01-GYOUMUID    TO  ORCHC03-PARA-GYOUMUID
           MOVE    SPA-S01-UID         TO  ORCHC03-PARA-TERMID
           MOVE    WRK-FILEMEI         TO  ORCHC03-PARA-FILEMEI
           MOVE    1                   TO  ORCHC03-MOTOID
      D    DISPLAY "2800-GAI-SKYPRT-SEC"
           PERFORM 28001-GAI-SKYPRT-SEC
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
      D        DISPLAY "2800-GAI-SRYPRT-SEC"
               PERFORM 28001-GAI-SRYPRT-SEC
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
      **************************************************************
      *    外来請求書兼領収書発行処理
      **************************************************************
       28001-GAI-SKYPRT-SEC                SECTION.
      *
           MOVE    "01"                TO  SPA-PRT-FLG
           INITIALIZE                      SRPLNK-AREA
           MOVE    "INS"               TO  SRPLNK-MODE
           MOVE    RPKEY-SEIKYUSHO     TO  SRPLNK-VAL
           CALL    "ORCSRPLNK"             USING
                                           SRPLNK-AREA
                                           SPA-AREA
      *
           MOVE    "00000003"          TO  WRK-KBNCD
           PERFORM 800-ORCSPRTNM-SEC
      *
           CALL    WRK-FORM-PG             USING
                                           SPA-AREA
                                           ORCHC03AREA
      *
           MOVE    SPACE               TO  SPA-PRT-FLG
      *
           .
       28001-GAI-SKYPRT-EXT.
           EXIT.
      **************************************************************
      *    外来診療費明細書発行処理
      **************************************************************
       28001-GAI-SRYPRT-SEC                SECTION.
      *
           MOVE    "02"                TO  SPA-PRT-FLG
           INITIALIZE                      SRPLNK-AREA
           MOVE    "INS"               TO  SRPLNK-MODE
           MOVE    RPKEY-MEISAISHO     TO  SRPLNK-VAL
           CALL    "ORCSRPLNK"             USING
                                           SRPLNK-AREA
                                           SPA-AREA
      *
           MOVE    "00000008"          TO  WRK-KBNCD
           PERFORM 800-ORCSPRTNM-SEC
      *
           CALL    WRK-FORM-PG             USING
                                           SPA-AREA
                                           ORCHC04AREA
      *
           MOVE    SPACE               TO  SPA-PRT-FLG
      *
           .
       28001-GAI-SRYPRT-EXT.
           EXIT.
      **************************************************************
      *    入院請求書兼領収書発行処理
      **************************************************************
       28001-NYU-SKYPRT-SEC                SECTION.
      *
           MOVE    "03"                TO  SPA-PRT-FLG
           INITIALIZE                      SRPLNK-AREA
           MOVE    "INS"               TO  SRPLNK-MODE
           MOVE    RPKEY-SEIKYUSHO-N   TO  SRPLNK-VAL
           CALL    "ORCSRPLNK"             USING
                                           SRPLNK-AREA
                                           SPA-AREA
      *
           MOVE    "00000003"          TO  WRK-KBNCD
           PERFORM 800-ORCSPRTNM-SEC
      *
           CALL    WRK-FORM-PG     USING   ORCHCN03AREA
                                           SPA-AREA
      *
           MOVE    SPACE               TO  SPA-PRT-FLG
      *
           .
       28001-NYU-SKYPRT-EXT.
           EXIT.
      **************************************************************
      *    入院診療費明細書発行処理
      **************************************************************
       28001-NYU-SRYPRT-SEC                SECTION.
      *
           MOVE    "04"                TO  SPA-PRT-FLG
           INITIALIZE                      SRPLNK-AREA
           MOVE    "INS"               TO  SRPLNK-MODE
           MOVE    RPKEY-MEISAISHO-N   TO  SRPLNK-VAL
           CALL    "ORCSRPLNK"             USING
                                           SRPLNK-AREA
                                           SPA-AREA
      *
           MOVE    "00000008"          TO  WRK-KBNCD
           PERFORM 800-ORCSPRTNM-SEC
      *
           CALL    WRK-FORM-PG             USING
                                           SPA-AREA
                                           ORCHCN04AREA
      *
           MOVE    SPACE               TO  SPA-PRT-FLG
      *
           .
       28001-NYU-SRYPRT-EXT.
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
      *    帳票プログラム取得処理
      *****************************************************************
       800-ORCSPRTNM-SEC               SECTION.
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    WRK-KBNCD           TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    SPA-NYUGAIKBN       TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYSKANRI-REC
           MOVE    ORCSPRTNM-PRTPG  TO  WRK-FORM-PG
      *
           .
       800-ORCSPRTNM-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-S01-PTID        TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *
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
      *************************************************************
      *    収納マスター検索処理
      *************************************************************
       900-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-S01-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    SPA-S01-PTID        TO  SYU-PTID
           MOVE    SPA-S01-DENPNUM     TO  SYU-DENPNUM
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
               INITIALIZE                  SYUNOU-REC
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納明細検索処理
      *****************************************************************
       900-SYUMEI-KEY6-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUMEI
      *
           INITIALIZE                      SYUMEI-REC
           MOVE    SPA-HOSPNUM         TO  SMEI-HOSPNUM
           MOVE    SPA-S01-NYUGAIKBN   TO  SMEI-NYUGAIKBN
           MOVE    SPA-S01-PTID        TO  SMEI-PTID
           MOVE    SPA-S01-DENPNUM     TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF     ( MCP-RC             =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEI-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納明細検索処理
      *****************************************************************
       900-SYUMEI-KEY6-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUMEI
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF     ( MCP-RC         =   ZERO )
               MOVE    MCPDATA-REC TO  SYUMEI-REC
           ELSE
               INITIALIZE              SYUMEI-REC
               MOVE    1           TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEI-KEY6-FET-EXT.
           EXIT.
      *************************************************************
      *    収納履歴検索処理
      *************************************************************
       900-SYURRK-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYURRK
      *
           INITIALIZE                      SYURRK-REC
           MOVE    SMEI-HOSPNUM        TO  SYURRK-HOSPNUM
           MOVE    SMEI-NYUGAIKBN      TO  SYURRK-NYUGAIKBN
           MOVE    SMEI-PTID           TO  SYURRK-PTID
           MOVE    SMEI-DENPNUM        TO  SYURRK-DENPNUM
           MOVE    SMEI-SYURRKNUM      TO  SYURRK-SYURRKNUM
           MOVE    SYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_syurrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYURRK-REC
           ELSE
               MOVE    1               TO  FLG-SYURRK
               INITIALIZE                  SYURRK-REC
           END-IF
      *
           .
       900-SYURRK-KEY3-SEL-EXT.
           EXIT.
      *************************************************************
      *    収納履歴FETCH処理
      *************************************************************
       900-SYURRK-KEY3-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYURRK
      *
           MOVE    "tbl_syurrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYURRK-REC
           ELSE
               MOVE    1               TO  FLG-SYURRK
               INITIALIZE                  SYURRK-REC
           END-IF
      *
           .
       900-SYURRK-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    パラメタテーブル検索
      *****************************************************************
       900-PARA-KEY4-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-S01-GYOUMUID    TO  PARA-GYOUMUID
           MOVE    SPA-S01-UID         TO  PARA-TERMID
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
           MOVE    SPA-S01-GYOUMUID    TO  PARA-GYOUMUID
           MOVE    SPA-S01-UID         TO  PARA-TERMID
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
           MOVE    SPA-S01-GYOUMUID    TO  PARA-GYOUMUID
           MOVE    SPA-S01-UID         TO  PARA-TERMID
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
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
      *    ＤＢ検索処理
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
