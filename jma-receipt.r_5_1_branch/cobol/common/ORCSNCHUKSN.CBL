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
       PROGRAM-ID.         ORCSNCHUKSN.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : 共通
      *  コンポーネント名  : 入院料注加算コード返却
      *  管理者            :
      *  作成日付    作業者        記述
      *  18/03/03    NACL−太田　新規作成
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
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
       01  WRK-AREA.
           03  WRK-KSNCD                   PIC X(09).
      *
       01  CONST-AREA.
           03  CONST-9999999               PIC X(08) VALUE   "99999999".
           03  CONST-H300401               PIC X(08) VALUE   "20180401".
           03  CONST-H280401               PIC X(08) VALUE   "20160401".
           03  CONST-H260401               PIC X(08) VALUE   "20140401".
           03  CONST-H240401               PIC X(08) VALUE   "20120401".
           03  CONST-H220401               PIC X(08) VALUE   "20100401".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "CPORCSNCHUKSN.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SNCHUKSN-AREA
           SPA-AREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           INITIALIZE              WRK-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           PERFORM 200-MAIN-SEC
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
       100-INIT-SEC                SECTION.
      *
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( SNCHUKSN-BASEYMD    <   CONST-9999999 )
            AND  ( SNCHUKSN-BASEYMD    >=  CONST-H300401 )
               PERFORM 2001-H3004-SEC
           WHEN  ( SNCHUKSN-BASEYMD    <   CONST-H300401 )
            AND  ( SNCHUKSN-BASEYMD    >=  CONST-H280401 )
               PERFORM 2001-H2804-SEC
           WHEN  ( SNCHUKSN-BASEYMD    <   CONST-H280401 )
            AND  ( SNCHUKSN-BASEYMD    >=  CONST-H260401 )
               PERFORM 2001-H2604-SEC
           WHEN  ( SNCHUKSN-BASEYMD    <   CONST-H260401 )
            AND  ( SNCHUKSN-BASEYMD    >=  CONST-H240401 )
               PERFORM 2001-H2404-SEC
           END-EVALUATE
      *
           MOVE    WRK-KSNCD           TO   SNCHUKSN-KSNCD
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    平成３０年４月のチェック
      *****************************************************************
       2001-H3004-SEC                      SECTION.
      *
           PERFORM 2018-KANGOHITSUYODOHYOKA-SEC
      *
           PERFORM 2016-KANGOHITSUYODO1-SEC
      *
           PERFORM 2016-KANGOHITSUYODO2-SEC
      *
           PERFORM 2016-KANGOHITSUYODO3-SEC
      *
           PERFORM 2016-ZAITAKUFUKKI-SEC
      *
           PERFORM 2014-ADLIJI-SEC
      *
           PERFORM 2014-SEISHINHOKENFUKUSHI-SEC
      *
           PERFORM 2014-EIYOKANRIJISSHI-SEC
      *
           .
       2001-H3004-EXT.
           EXIT.
      *****************************************************************
      *    平成２８年４月のチェック
      *****************************************************************
       2001-H2804-SEC                      SECTION.
      *
           PERFORM 2012-KANGOHITSUYODOHYOKA-SEC
      *
           PERFORM 2016-KANGOHITSUYODO1-SEC
      *
           PERFORM 2016-KANGOHITSUYODO2-SEC
      *
           PERFORM 2016-KANGOHITSUYODO3-SEC
      *
           PERFORM 2016-ZAITAKUFUKKI-SEC
      *
           PERFORM 2014-ADLIJI-SEC
      *
           PERFORM 2014-SEISHINHOKENFUKUSHI-SEC
      *
           PERFORM 2014-EIYOKANRIJISSHI-SEC
      *
           .
       2001-H2804-EXT.
           EXIT.
      *****************************************************************
      *    平成２６年４月のチェック
      *****************************************************************
       2001-H2604-SEC                      SECTION.
      *
           PERFORM 2012-KANGOHITSUYODOHYOKA-SEC
      *
           PERFORM 2012-KANGOHITSUYODO1-SEC
      *
           PERFORM 2012-KANGOHITSUYODO2-SEC
      *
           PERFORM 2014-ADLIJI-SEC
      *
           PERFORM 2014-SEISHINHOKENFUKUSHI-SEC
      *
           PERFORM 2014-EIYOKANRIJISSHI-SEC
      *
           .
       2001-H2604-EXT.
           EXIT.
      *****************************************************************
      *    平成２４年４月のチェック
      *****************************************************************
       2001-H2404-SEC                      SECTION.
      *
           PERFORM 2012-KANGOHITSUYODOHYOKA-SEC
      *
           PERFORM 2012-KANGOHITSUYODO1-SEC
      *
           PERFORM 2012-KANGOHITSUYODO2-SEC
      *
           .
       2001-H2404-EXT.
           EXIT.
      *****************************************************************
      *    一般病棟看護必要度評価加算チェック
      *****************************************************************
       2012-KANGOHITSUYODOHYOKA-SEC   SECTION.
      *
           IF    ( SNCHUKSN-KSNCD   =      "190131570"
                                       OR  "190134970"
                                       OR  "190152570" )
               EVALUATE    SNCHUKSN-KBNNUM
               WHEN    100
                   MOVE    "190131570" TO  WRK-KSNCD
               WHEN    105
                   MOVE    "190134970" TO  WRK-KSNCD
               WHEN    317
                   MOVE    "190152570" TO  WRK-KSNCD
               END-EVALUATE
           END-IF
      *
           .
       2012-KANGOHITSUYODOHYOKA-EXT.
           EXIT.
      *****************************************************************
      *    一般病棟看護必要度評価加算チェック(2018年から)
      *****************************************************************
       2018-KANGOHITSUYODOHYOKA-SEC    SECTION.
      *
           IF    ( SNCHUKSN-KSNCD   =      "190134970"
                                       OR  "190152570" )
               EVALUATE    SNCHUKSN-KBNNUM
               WHEN    105
                   MOVE    "190134970" TO  WRK-KSNCD
               WHEN    317
                   MOVE    "190152570" TO  WRK-KSNCD
               END-EVALUATE
           END-IF
      *
           .
       2018-KANGOHITSUYODOHYOKA-EXT.
           EXIT.
      *****************************************************************
      *    看護必要度加算１チェック
      *****************************************************************
       2012-KANGOHITSUYODO1-SEC           SECTION.
      *
           IF    ( SNCHUKSN-KSNCD   =      "190143070"
                                       OR  "190144570"
                                       OR  "190143970" )
               EVALUATE    SNCHUKSN-KBNNUM
               WHEN    100
                   MOVE    "190143070" TO  WRK-KSNCD
               WHEN    104
                   MOVE    "190143970" TO  WRK-KSNCD
               WHEN    105
                   MOVE    "190144570" TO  WRK-KSNCD
               END-EVALUATE
           END-IF
      *
           .
       2012-KANGOHITSUYODO1-EXT.
           EXIT.
      *****************************************************************
      *    看護必要度加算１チェック(2016年4月から)
      *****************************************************************
       2016-KANGOHITSUYODO1-SEC           SECTION.
      *
      *
           IF    ( SNCHUKSN-KSNCD   =      "190183070"
                                       OR  "190188870"
                                       OR  "190189170" )
               EVALUATE    SNCHUKSN-KBNNUM
               WHEN    100
                   MOVE    "190183070" TO  WRK-KSNCD
               WHEN    104
                   MOVE    "190188870" TO  WRK-KSNCD
               WHEN    105
                   MOVE    "190189170" TO  WRK-KSNCD
               END-EVALUATE
           END-IF
      *
           .
       2016-KANGOHITSUYODO1-EXT.
           EXIT.
      *****************************************************************
      *    看護必要度加算１チェック(2018年4月から)
      *****************************************************************
       2018-KANGOHITSUYODO1-SEC           SECTION.
      *
      *
           IF    ( SNCHUKSN-KSNCD   =      "190188870"
                                       OR  "190189170" )
               EVALUATE    SNCHUKSN-KBNNUM
               WHEN    104
                   MOVE    "190188870" TO  WRK-KSNCD
               WHEN    105
                   MOVE    "190189170" TO  WRK-KSNCD
               END-EVALUATE
           END-IF
      *
           .
       2018-KANGOHITSUYODO1-EXT.
           EXIT.
      *****************************************************************
      *    看護必要度加算２チェック
      *****************************************************************
       2012-KANGOHITSUYODO2-SEC           SECTION.
      *
           IF    ( SNCHUKSN-KSNCD   =      "190143170"
                                       OR  "190144070"
                                       OR  "190144670" )
               EVALUATE    SNCHUKSN-KBNNUM
               WHEN    100
                   MOVE    "190143170" TO  WRK-KSNCD
               WHEN    104
                   MOVE    "190144070" TO  WRK-KSNCD
               WHEN    105
                   MOVE    "190144670" TO  WRK-KSNCD
               END-EVALUATE
           END-IF
      *
           .
       2012-KANGOHITSUYODO2-EXT.
           EXIT.
      *****************************************************************
      *    看護必要度加算２チェック
      *****************************************************************
       2016-KANGOHITSUYODO2-SEC           SECTION.
      *
           IF    ( SNCHUKSN-KSNCD   =      "190183170"
                                       OR  "190188970"
                                       OR  "190189270" )
               EVALUATE    SNCHUKSN-KBNNUM
               WHEN    100
                     MOVE  "190183170" TO  WRK-KSNCD
               WHEN    104
                   MOVE    "190188970" TO  WRK-KSNCD
               WHEN    105
                   MOVE    "190189270" TO  WRK-KSNCD
               END-EVALUATE
           END-IF
      *
           .
       2016-KANGOHITSUYODO2-EXT.
           EXIT.
      *****************************************************************
      *    看護必要度加算２チェック(2018年4月から)
      *****************************************************************
       2018-KANGOHITSUYODO2-SEC           SECTION.
      *
           IF    ( SNCHUKSN-KSNCD   =      "190188970"
                                       OR  "190189270" )
               EVALUATE    SNCHUKSN-KBNNUM
               WHEN    104
                   MOVE    "190188970" TO  WRK-KSNCD
               WHEN    105
                   MOVE    "190189270" TO  WRK-KSNCD
               END-EVALUATE
           END-IF
      *
           .
       2018-KANGOHITSUYODO2-EXT.
           EXIT.
      *****************************************************************
      *    看護必要度加算３チェック
      *****************************************************************
       2016-KANGOHITSUYODO3-SEC           SECTION.
      *
           IF    ( SNCHUKSN-KSNCD   =      "190183270"
                                       OR  "190189070"
                                       OR  "190189370" )
               EVALUATE    SNCHUKSN-KBNNUM
               WHEN    100
                   MOVE    "190183270" TO  WRK-KSNCD
               WHEN    104
                   MOVE    "190189070" TO  WRK-KSNCD
               WHEN    105
                   MOVE    "190189370" TO  WRK-KSNCD
               END-EVALUATE
           END-IF
      *
           .
       2016-KANGOHITSUYODO3-EXT.
           EXIT.
      *****************************************************************
      *    看護必要度加算３チェック(2018年4月から)
      *****************************************************************
       2018-KANGOHITSUYODO3-SEC           SECTION.
      *
           IF    ( SNCHUKSN-KSNCD   =      "190189070"
                                       OR  "190189370" )
               EVALUATE    SNCHUKSN-KBNNUM
               WHEN    104
                   MOVE    "190189070" TO  WRK-KSNCD
               WHEN    105
                   MOVE    "190189370" TO  WRK-KSNCD
               END-EVALUATE
           END-IF
      *
           .
       2018-KANGOHITSUYODO3-EXT.
           EXIT.
      *****************************************************************
      *    ＡＤＬ維持向上等体制加算チェック
      *****************************************************************
       2014-ADLIJI-SEC                SECTION.
      *
           IF    ( SNCHUKSN-KSNCD   =      "190166070"
                                       OR  "190169070"
                                       OR  "190169170" )
               EVALUATE    SNCHUKSN-KBNNUM
               WHEN    100
                   MOVE    "190166070" TO  WRK-KSNCD
               WHEN    104
                   MOVE    "190169070" TO  WRK-KSNCD
               WHEN    105
                   MOVE    "190169170" TO  WRK-KSNCD
               END-EVALUATE
           END-IF
      *
           .
       2014-ADLIJI-EXT.
           EXIT.
      *****************************************************************
      *    精神保健福祉士配置加算チェック
      *****************************************************************
       2014-SEISHINHOKENFUKUSHI-SEC   SECTION.
      *
           IF    ( SNCHUKSN-KSNCD   =      "190168970"
                                       OR  "190177570" )
               EVALUATE    SNCHUKSN-KBNNUM
               WHEN    103
                   MOVE    "190168970" TO  WRK-KSNCD
               WHEN    312
                   MOVE    "190177570" TO  WRK-KSNCD
               END-EVALUATE
           END-IF
      *
           .
       2014-SEISHINHOKENFUKUSHI-EXT.
           EXIT.
      *****************************************************************
      *    栄養管理実施加算チェック
      *****************************************************************
       2014-EIYOKANRIJISSHI-SEC       SECTION.
      *
           IF    ( SNCHUKSN-KSNCD   =      "190170770"
                                       OR  "190171770" )
               EVALUATE    SNCHUKSN-KBNNUM
               WHEN    108
                   MOVE    "190170770" TO  WRK-KSNCD
               WHEN    109
                   MOVE    "190171770" TO  WRK-KSNCD
               END-EVALUATE
           END-IF
      *
           .
       2014-EIYOKANRIJISSHI-EXT.
           EXIT.
      *****************************************************************
      *    在宅復帰機能評価加算チェック
      *****************************************************************
       2016-ZAITAKUFUKKI-SEC          SECTION.
      *
           IF    ( SNCHUKSN-KSNCD   =      "190168070"
                                       OR  "190190270"
                                       OR  "190190370" )
               EVALUATE    SNCHUKSN-KBNNUM
               WHEN    101
                   MOVE    "190168070" TO  WRK-KSNCD
               WHEN    108
                   MOVE    "190190270" TO  WRK-KSNCD
               WHEN    109
                   MOVE    "190190370" TO  WRK-KSNCD
               END-EVALUATE
           END-IF
      *
           .
       2016-ZAITAKUFUKKI-EXT.
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
      *
