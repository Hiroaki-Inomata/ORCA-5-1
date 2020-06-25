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
       PROGRAM-ID.                 ORCGSSUB01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 収納
      *  コンポーネント名  : 請求書用パラメタテーブル編集処理
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
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-HIT             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
       01  WRK-AREA.
           03  WRK-FILEMEI.
               05  WRK-FILEMEI-YMD PIC X(08).
               05  WRK-FILEMEI-HMS PIC X(06).
               05  WRK-FILEMEI-CD  PIC X(06).
           03  WRK-GAI-SRYYMD      PIC X(08).
           03  WRK-GAI-DENPNUM     PIC 9(07).
           03  WRK-GAI-SEIKYU      PIC S9(10).
           03  WRK-GAI-NYUKIN      PIC S9(10).
           03  WRK-MISYU-G.
               05  WRK-MISYU       PIC S9(10).
               05  WRK-MISYU-P     PIC S9(10).
               05  WRK-MISYU-M     PIC S9(10).
           03  WRK-GAI-MISYU-G.
               05  WRK-GAI-MISYU   PIC S9(10).
               05  WRK-GAI-MISYU-P PIC S9(10).
               05  WRK-GAI-MISYU-M PIC S9(10).
           03  WRK-NYU-MISYU       PIC S9(10).
           03  WRK-NYUGAIKBN       PIC X(01).
           03  WRK-DENPNUM         PIC 9(07).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1039.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
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
      *    サブプロ用　領域
      *****************************************************************
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    未収金編集サブ
       01  LNKSYU-AREA.
           COPY    "CPSYUNOU.INC"      REPLACING //SYU-//
                                       BY        //LNKSYU-//.
           COPY    "CPORCSMISYU.INC".
      *
           COPY    "CPORCHC03S02.INC".
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
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           SPA-S01KYOUTU.
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
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-S01-TUKI-SRYYM  =   SPACE )
                   PERFORM 2001-SYUPARA-EDIT-SEC
               ELSE
                   PERFORM 2002-SYUPARA-EDIT-SEC
               END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    請求書用パラメタテーブル編集処理
      *****************************************************************
       2001-SYUPARA-EDIT-SEC               SECTION.
      *
           INITIALIZE                      SYS-1039-REC
           MOVE    "1039"              TO  SYS-1039-KANRICD
           MOVE    "*"                 TO  SYS-1039-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1039-STYUKYMD
                                           SYS-1039-EDYUKYMD
           MOVE    SYS-1039-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1039-REC
      *
           MOVE    SPA-S01-DENPNUM     TO  WRK-DENPNUM
           PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
           INITIALIZE                  SMISYU-AREA
           MOVE    SYUNOU-REC          TO  LNKSYU-AREA
           MOVE    SYU-SRYYMD          TO  LNKSYU-SKYEDYMD
           CALL    "ORCSMISYU"         USING
                                       LNKSYU-AREA
                                       SMISYU-AREA
                                       SPA-AREA
      *
           MOVE    SMISYU-NYUIN        TO  WRK-NYU-MISYU
           MOVE    SMISYU-GAIRAI       TO  WRK-GAI-MISYU
           MOVE    SMISYU-GAIRAI-P     TO  WRK-GAI-MISYU-P
           MOVE    SMISYU-GAIRAI-M     TO  WRK-GAI-MISYU-M
      *
           MOVE    SYU-SRYYMD          TO  WRK-GAI-SRYYMD
           MOVE    SYU-DENPNUM         TO  WRK-GAI-DENPNUM
           MOVE    SYU-SKYMONEY        TO  WRK-GAI-SEIKYU
           MOVE    SYU-NYUKIN-TOTAL    TO  WRK-GAI-NYUKIN
      *
           INITIALIZE                  SYUPARA01-AREA
                                       SYUPARA02-AREA
      *
           MOVE    "2"                 TO  SYUPARA01-NYUGAIKBN
           MOVE    WRK-GAI-SEIKYU      TO  SYUPARA01-SEIKYU
           MOVE    WRK-GAI-NYUKIN      TO  SYUPARA01-NYUKIN
      *
           MOVE    WRK-NYU-MISYU       TO  SYUPARA01-NYU-MISYU
           MOVE    WRK-GAI-MISYU       TO  SYUPARA01-GAI-MISYU
           MOVE    WRK-GAI-MISYU-P     TO  SYUPARA01-GAI-MISYU-P
           MOVE    WRK-GAI-MISYU-M     TO  SYUPARA01-GAI-MISYU-M
      *
           MOVE    ZERO                TO  SYUPARA01-NYU-NYUKIN
           MOVE    ZERO                TO  SYUPARA01-GAI-NYUKIN
           MOVE    1                   TO  SYUPARA01-HAKKOFLG
           IF    ( SYU-GRP-DENPNUM     NOT =   ZERO )
             AND ( SYS-1039-S02SKYSAIKBN   =   "1" )
               MOVE    SYU-GRP-HAKHOUFLG
                                       TO  SYUPARA01-SKYSUMFLG
           ELSE
               MOVE    ZERO            TO  SYUPARA01-SKYSUMFLG
           END-IF
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
           IF    ( SYU-GRP-DENPNUM     NOT =   ZERO )
            AND  ( SYS-1039-S02SKYSAIKBN   =   "1" )
      *
               PERFORM 900-SYUNOU-KEY27-SEL-SEC
      *
               PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
                        OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                   IF    ( SYS-1039-MISYUKIKANKBN  =   "1" )
                       CONTINUE
                   ELSE
      *
                       COMPUTE SYUPARA01-GAI-MISYU
                           =   SYUPARA01-GAI-MISYU
                           - ( SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL )
      *
                       IF    ( SYU-SKYMONEY    <   SYU-NYUKIN-TOTAL )
                           COMPUTE SYUPARA01-GAI-MISYU-M
                               =   SYUPARA01-GAI-MISYU-M
                               - ( SYU-SKYMONEY -  SYU-NYUKIN-TOTAL )
                       ELSE
                           COMPUTE SYUPARA01-GAI-MISYU-P
                               =   SYUPARA01-GAI-MISYU-P
                               - ( SYU-SKYMONEY -  SYU-NYUKIN-TOTAL )
                       END-IF
                   END-IF
      *
                   PERFORM 2011-SYUPARA02-HEN-SEC
      *
                   PERFORM 900-SYUNOU-KEY27-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key27"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           ELSE
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
               PERFORM 2011-SYUPARA02-HEN-SEC
      *
           END-IF
      *
           IF    ( SYS-1039-S02SKYMISYUPRTKBN  =   "0" )
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
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2011-SYUPARA01-HEN-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2011-ORCHC03S02-SEC
           END-IF
      *
           .
       2001-SYUPARA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求書用パラメタテーブル編集処理（月別）
      *****************************************************************
       2002-SYUPARA-EDIT-SEC               SECTION.
      *
           INITIALIZE                      SYS-1039-REC
           MOVE    "1039"              TO  SYS-1039-KANRICD
           MOVE    "*"                 TO  SYS-1039-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1039-STYUKYMD
                                           SYS-1039-EDYUKYMD
           MOVE    SYS-1039-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1039-REC
      *
           MOVE    "1"                 TO  WRK-NYUGAIKBN
           PERFORM 2011-TUKIMISYU-SUM-SEC
           MOVE    WRK-MISYU           TO  WRK-NYU-MISYU
           MOVE    "2"                 TO  WRK-NYUGAIKBN
           PERFORM 2011-TUKIMISYU-SUM-SEC
           MOVE    WRK-MISYU           TO  WRK-GAI-MISYU
           MOVE    WRK-MISYU-P         TO  WRK-GAI-MISYU-P
           MOVE    WRK-MISYU-M         TO  WRK-GAI-MISYU-M
      *
           MOVE    SPA-S01-TUKI-DENPNUM TO  WRK-DENPNUM
           PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
           MOVE    SYU-SRYYMD          TO  WRK-GAI-SRYYMD
           MOVE    SYU-DENPNUM         TO  WRK-GAI-DENPNUM
           MOVE    SYU-SKYMONEY        TO  WRK-GAI-SEIKYU
           MOVE    SYU-NYUKIN-TOTAL    TO  WRK-GAI-NYUKIN
      *
           INITIALIZE                  SYUPARA01-AREA
                                       SYUPARA02-AREA
           MOVE    "2"                 TO  SYUPARA01-NYUGAIKBN
           MOVE    WRK-GAI-SEIKYU      TO  SYUPARA01-SEIKYU
           MOVE    WRK-GAI-NYUKIN      TO  SYUPARA01-NYUKIN
      *
           MOVE    WRK-NYU-MISYU       TO  SYUPARA01-NYU-MISYU
           MOVE    WRK-GAI-MISYU       TO  SYUPARA01-GAI-MISYU
           MOVE    WRK-GAI-MISYU-P     TO  SYUPARA01-GAI-MISYU-P
           MOVE    WRK-GAI-MISYU-M     TO  SYUPARA01-GAI-MISYU-M
      *
           MOVE    ZERO                TO  SYUPARA01-NYU-NYUKIN
           MOVE    ZERO                TO  SYUPARA01-GAI-NYUKIN
      *
           MOVE    4                   TO  SYUPARA01-HAKKOFLG
           MOVE    SPA-SYSYMD          TO  SYUPARA01-DENPPRTYMD
      *
           IF    ( SYS-1039-TUKISKYMISYUPRTKBN =   "0" )
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
           EVALUATE    SPA-S01-TUKI-GRPKBN
           WHEN    "1"
               MOVE    1               TO  SYUPARA01-SKYSUMFLG
           WHEN    "2"
               MOVE    2               TO  SYUPARA01-SKYSUMFLG
           WHEN    "3"
               MOVE    3               TO  SYUPARA01-SKYSUMFLG
           WHEN    "4"
               MOVE    4               TO  SYUPARA01-SKYSUMFLG
           WHEN    OTHER
               MOVE    4               TO  SYUPARA01-SKYSUMFLG
           END-EVALUATE
      *
           IF    ( SYS-1039-TUKISUMKBN     =   "1" )
               MOVE    "1"             TO  SYUPARA01-TUKISUMKBN
           ELSE
               MOVE    "0"             TO  SYUPARA01-TUKISUMKBN
           END-IF
      *
           MOVE    SPA-S01-TUKI-KIKANKBN
                                       TO  SYUPARA01-TUKIKIKANKBN
           MOVE    SPA-S01-TUKI-STYMD  TO  SYUPARA01-TUKISTYMD
           MOVE    SPA-S01-TUKI-EDYMD  TO  SYUPARA01-TUKIEDYMD
      *
           MOVE     "__"               TO  SYU-SRYYMD (7:2)
           PERFORM 900-SYUNOU-KEY73-SEL-SEC
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    SPA-S01-GYOUMUID    TO  PARA-GYOUMUID
           MOVE    SPA-S01-UID         TO  PARA-TERMID
           MOVE    "00101"             TO  WRK-FILEMEI-CD
           MOVE    WRK-FILEMEI         TO  PARA-FILEMEI
           MOVE    1                   TO  PARA-EDANUM
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               MOVE    1               TO  FLG-HIT
      *
               IF    ( SPA-S01-TUKI-KIKANKBN   =   "1" )
                   IF    ( SYU-SRYYMD  >=  SPA-S01-TUKI-STYMD )
                    AND  ( SYU-SRYYMD  <=  SPA-S01-TUKI-EDYMD )
                       CONTINUE
                   ELSE
                       MOVE    ZERO    TO  FLG-HIT
                   END-IF
               END-IF
      *
               IF    ( FLG-HIT         =   1 )
                   PERFORM 2011-SYUPARA02-HEN-SEC
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
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2011-SYUPARA01-HEN-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2011-ORCHC03S02-SEC
           END-IF
      *
           .
       2002-SYUPARA-EDIT-EXT.
           EXIT.
      **************************************************************
      *    請求書用パラメタテーブル編集処理
      **************************************************************
       2011-SYUPARA02-HEN-SEC         SECTION.
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
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4002"      TO  SPA-ERRCD
           END-IF
      *
           .
       2011-SYUPARA02-HEN-EXT.
           EXIT.
      **************************************************************
      *    請求書用パラメタテーブル編集処理
      **************************************************************
       2011-SYUPARA01-HEN-SEC         SECTION.
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
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4001"          TO  SPA-ERRCD
           END-IF
      *
           .
       2011-SYUPARA01-HEN-EXT.
           EXIT.
      **************************************************************
      *    請求書編集サブ呼出処理
      **************************************************************
       2011-ORCHC03S02-SEC         SECTION.
      *
           INITIALIZE                      ORCHC03S02AREA
           MOVE    SPA-HOSPNUM         TO  ORCHC03S02-HOSPNUM
           MOVE    SPA-S01-GYOUMUID    TO  ORCHC03S02-GYOUMUID
           MOVE    SPA-S01-UID         TO  ORCHC03S02-TERMID
           MOVE    "00101"             TO  WRK-FILEMEI-CD
           MOVE    WRK-FILEMEI         TO  ORCHC03S02-FILEMEI
           CALL    "ORCHC03S02"    USING   SPA-AREA
                                           ORCHC03S02AREA
           IF    ( ORCHC03S02-RC   NOT =   ZERO )
               MOVE    "4003"          TO  SPA-ERRCD
           END-IF
      *
           .
       2011-ORCHC03S02-EXT.
           EXIT.
      **************************************************************
      *   未収金集計処理（月別）
      **************************************************************
       2011-TUKIMISYU-SUM-SEC          SECTION.
      *
           INITIALIZE                  WRK-MISYU-G
      *
           PERFORM 900-SYUNOU-KEY55-SEL-SEC
      *
           COMPUTE WRK-MISYU-P
               =   SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL
      *
           PERFORM 900-SYUNOU-KEY56-SEL-SEC
      *
           COMPUTE WRK-MISYU-M
               =   SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL
      *
           COMPUTE WRK-MISYU
               =   WRK-MISYU-P
               +   WRK-MISYU-M
      *
           .
       2011-TUKIMISYU-SUM-EXT.
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
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *
      **************************************************************
      *    収納マスター検索処理
      **************************************************************
       900-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           IF    ( SPA-S01-TUKI-SRYYM  =   SPACE )
               MOVE    SPA-S01-NYUGAIKBN
                                       TO  SYU-NYUGAIKBN
           ELSE
               MOVE    "2"             TO  SYU-NYUGAIKBN
           END-IF
           MOVE    SPA-S01-PTID        TO  SYU-PTID
           MOVE    WRK-DENPNUM         TO  SYU-DENPNUM
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
      **************************************************************
      *    収納マスター検索処理
      **************************************************************
       900-SYUNOU-KEY27-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY27-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納マスターFETCH処理
      **************************************************************
       900-SYUNOU-KEY27-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY27-FET-EXT.
           EXIT.
      **************************************************************
      *    収納マスター検索処理
      **************************************************************
       900-SYUNOU-KEY55-SEL-SEC        SECTION.
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-S01-PTID        TO  SYU-PTID
           IF    ( SPA-S01-TUKI-KIKANKBN   =   "1" )
               MOVE    SPA-S01-TUKI-STYMD
                                   TO  SYU-SRYYMD
           ELSE
               MOVE    SPA-S01-TUKI-SRYYM
                                   TO  SYU-SRYYMD
               MOVE    "01"        TO  SYU-SRYYMD(7:2)
           END-IF
      *
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key55"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key55"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY55-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納マスター検索処理
      **************************************************************
       900-SYUNOU-KEY56-SEL-SEC        SECTION.
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-S01-PTID        TO  SYU-PTID
           IF    ( SPA-S01-TUKI-KIKANKBN   =   "1" )
               MOVE    SPA-S01-TUKI-STYMD
                                   TO  SYU-SRYYMD
           ELSE
               MOVE    SPA-S01-TUKI-SRYYM
                                   TO  SYU-SRYYMD
               MOVE    "01"        TO  SYU-SRYYMD(7:2)
           END-IF
      *
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key56"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key56"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY56-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納マスター検索処理
      **************************************************************
       900-SYUNOU-KEY73-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
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
           MOVE    ZERO            TO  FLG-SYUNOU
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
