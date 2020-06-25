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
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGW31.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 医療区分表示設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/10/14    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-多々納  07/05/02  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    コンスタント値
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
       01  SPA-W31.
           03  SPA-GMN-CUR                        PIC 9(03).
           03  SPA-GMN-AREA.
               05  SPA-GMN-MEDKBN-G.
                   07  SPA-GMN-MEDKBN-MAX         PIC 9(03).
                   07  SPA-GMN-MEDKBN-OCC         OCCURS  40.
                       09  SPA-GMN-CHK-MEDKBN     PIC X(01).
                       09  SPA-GMN-CHK-LBLMEDKBN  PIC X(60).
               05  SPA-GMN-ADLINPUTKBN-G.
                   07  SPA-GMN-ADLINPUTKBN        PIC X(01).
                   07  SPA-GMN-ADLINPUTKBNFL      PIC X(01).
                   07  SPA-GMN-ADLINPUTKBNMEI     PIC X(20).
               05  SPA-GMN-SETMEDKBN-G.
                   07  SPA-GMN-SETMEDKBN          PIC X(01).
                   07  SPA-GMN-SETMEDKBNFL        PIC X(01).
                   07  SPA-GMN-SETMEDKBNMEI       PIC X(30).
           03  SPA-GMN-CMB-AREA.
               05  SPA-GMN-ADLINPUTKBNLST-G.
                   07  SPA-GMN-ADLINPUTKBN-MAX    PIC 9(03).
                   07  SPA-GMN-ADLINPUTKBNLST     OCCURS  2.
                       09  SPA-GMN-ADLINPUTKBNL   PIC X(01).
                       09  SPA-GMN-ADLINPUTKBNFLL PIC X(01).
                       09  SPA-GMN-ADLINPUTKBNMEIL PIC X(20).
               05  SPA-GMN-SETMEDKBNLST-G.
                   07  SPA-GMN-SETMEDKBN-MAX      PIC 9(03).
                   07  SPA-GMN-SETMEDKBNLST       OCCURS  2.
                       09  SPA-GMN-SETMEDKBNL     PIC X(01).
                       09  SPA-GMN-SETMEDKBNFLL   PIC X(01).
                       09  SPA-GMN-SETMEDKBNMEIL  PIC X(30).
           03  SPA-NAI-AREA.
               05  SPA-GMN-NAI-MEDKBN-G.
                   07  SPA-NAI-MEDKBN-OCC         OCCURS  40.
                       09  SPA-NAI-MEDKBN-ZAINUM  PIC 9(08).
                       09  SPA-NAI-MEDKBN-SRYCD   PIC X(09).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-TENSU                           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDX1                                PIC 9(05).
           03  IDX-CHK                             PIC 9(05).
           03  IDX-CMB                             PIC 9(05).
           03  IDX-MED                             PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-YMD                             PIC X(10).
           03  WRK-ZOGENPTN                        PIC X(01).
           03  WRK-ZOGEN                           PIC S9(05).
           03  WRK-KEISANYMD                       PIC X(08).
      *    入力項目名
           03  WRK-MCP-WIDGET                      PIC X(64).
           03  WRK-WIDMSG                          PIC X(200).
           03  WRK-ZAINUM-X.
               05  WRK-ZAINUM                      PIC 9(08).
           03  WRK-TNS-SRYCD                       PIC X(09).
           03  WRK-WIDGET-MEDKBN-G.
               05  WRK-WIDGET-MEDKBN-LBL           PIC X(10).
               05  WRK-WIDGET-MEDKBN               PIC 9(02).
           03  WRK-CMB-CD                          PIC X(10).
           03  WRK-CMB-ITM-G.
               07  WRK-CMB-ITM                     PIC X(200).
      *
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-H300401           PIC X(08)   VALUE "20180401".
           03  CONST-H280401           PIC X(08)   VALUE "20160401".
           03  CONST-H220401           PIC X(08)   VALUE "20100401".
           03  CONST-H200401           PIC X(08)   VALUE "20080401".
           03  CONST-GMN-MEDKBN-MAX    PIC 9(05)   VALUE 40.
           03  CONST-MEDKBN-G.
               05  CONST-MEDKBN-VAL.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(04)   VALUE 2001.
                   07                  PIC X(09)   VALUE "820000057".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2002.
                   07                  PIC X(09)   VALUE "820000058".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2003.
                   07                  PIC X(09)   VALUE "820000059".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2004.
                   07                  PIC X(09)   VALUE "820000060".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2005.
                   07                  PIC X(09)   VALUE "820000061".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2006.
                   07                  PIC X(09)   VALUE "820000062".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2007.
                   07                  PIC X(09)   VALUE "820000063".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2008.
                   07                  PIC X(09)   VALUE "820000064".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2009.
                   07                  PIC X(09)   VALUE "820000065".
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(04)   VALUE 2010.
                   07                  PIC X(09)   VALUE "820000066".
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(04)   VALUE 2011.
                   07                  PIC X(09)   VALUE "820000067".
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(04)   VALUE 2012.
                   07                  PIC X(09)   VALUE "820000068".
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(04)   VALUE 2013.
                   07                  PIC X(09)   VALUE "820000069".
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(04)   VALUE 2014.
                   07                  PIC X(09)   VALUE "820000070".
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(04)   VALUE 2015.
                   07                  PIC X(09)   VALUE "820000071".
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(04)   VALUE 2016.
                   07                  PIC X(09)   VALUE "820000072".
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(04)   VALUE 2017.
                   07                  PIC X(09)   VALUE "820000073".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2018.
                   07                  PIC X(09)   VALUE "820000074".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2019.
                   07                  PIC X(09)   VALUE "820000075".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2020.
                   07                  PIC X(09)   VALUE "820000076".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2021.
                   07                  PIC X(09)   VALUE "820000077".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2022.
                   07                  PIC X(09)   VALUE "820000078".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2023.
                   07                  PIC X(09)   VALUE "820000079".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2024.
                   07                  PIC X(09)   VALUE "820000080".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2025.
                   07                  PIC X(09)   VALUE "820000081".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2026.
                   07                  PIC X(09)   VALUE "820000082".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2027.
                   07                  PIC X(09)   VALUE "820000083".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2028.
                   07                  PIC X(09)   VALUE "820000084".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2029.
                   07                  PIC X(09)   VALUE "820000085".
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(04)   VALUE 2030.
                   07                  PIC X(09)   VALUE "820000086".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2031.
                   07                  PIC X(09)   VALUE "820000087".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2032.
                   07                  PIC X(09)   VALUE "820000088".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2033.
                   07                  PIC X(09)   VALUE "820000089".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2034.
                   07                  PIC X(09)   VALUE "820000090".
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(04)   VALUE 2035.
                   07                  PIC X(09)   VALUE "820000091".
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(04)   VALUE 2036.
                   07                  PIC X(09)   VALUE "820000092".
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(04)   VALUE 2037.
                   07                  PIC X(09)   VALUE "820000093".
               05  CONST-MEDKBN-R REDEFINES CONST-MEDKBN-VAL.
                   07  CONST-MEDKBN-OCC    OCCURS  37.
                       09  CONST-MEDKBN-ALLKBN PIC 9(01).
                       09  CONST-MEDKBN-ZAINUM PIC 9(04).
                       09  CONST-MEDKBN-SRYCD  PIC X(09).
               05  CONST-MEDKBN-MAX        PIC 9(03)   VALUE   37.
      *
           COPY  "CPMEDKBN.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *
      *    医療区分表示情報
           COPY  "CPSK5014.INC".
      *
      *    ADL点数情報
           COPY  "CPSK5015.INC".
      *
      *    点数マスタ
           COPY  "CPTENSU.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA101SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
                                               MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W31
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"         ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W31         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "WERR" )
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF 
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "W31    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC                  SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "WID1"
               PERFORM 330-WID1-SET-SEC
               MOVE    SPACE           TO  SPA-WID1-FLG
               MOVE    SPACE           TO  SPA-WIDCD
               GO  TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-W31
      *
           EVALUATE    SPA-W01-KANRICD
           WHEN    "5014"
               EVALUATE    TRUE
               WHEN  ( SPA-W01-STYUKYMD    >=  CONST-H200401 )
                   PERFORM 3101-SPA-5014-SET-SEC
               WHEN    OTHER
                   PERFORM 3100-SPA-5014-SET-SEC
               END-EVALUATE
           WHEN    "5015"
               PERFORM 3100-SPA-5015-SET-SEC
           END-EVALUATE
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       3100-SPA-5014-SET-SEC                  SECTION.
      *
           PERFORM VARYING IDX-CHK    FROM    1   BY  1
                   UNTIL ( IDX-CHK    >   CONST-MEDKBN-MAX )
      *
               MOVE    CONST-MEDKBN-ZAINUM (IDX-CHK)
                                       TO  SPA-NAI-MEDKBN-ZAINUM
                                                              (IDX-CHK)
               MOVE    CONST-MEDKBN-SRYCD  (IDX-CHK)
                                       TO  SPA-NAI-MEDKBN-SRYCD
                                                              (IDX-CHK)
                                           WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    TNS-NAME        TO  SPA-GMN-CHK-LBLMEDKBN
                                                              (IDX-CHK)
      *
               PERFORM 3101-SYS5014-SEL-SEC
               IF    ( SYS-5014-DISPKBN    =   "1" )
                   MOVE    "T"         TO  SPA-GMN-CHK-MEDKBN (IDX-CHK)
               ELSE
                   MOVE    "F"         TO  SPA-GMN-CHK-MEDKBN (IDX-CHK)
               END-IF
      *
           END-PERFORM
      *
           MOVE    CONST-MEDKBN-MAX    TO  SPA-GMN-MEDKBN-MAX
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       3100-SPA-5014-SET-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       3101-SPA-5014-SET-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-CHK
      *
           PERFORM VARYING IDX-MED FROM    1   BY  1
                   UNTIL ( IDX-MED >   MEDH20-MAX )
      *
               IF    ( MEDH20-STYMD (IDX-MED)
                                      <=   SPA-W01-STYUKYMD )
                 AND ( MEDH20-EDYMD (IDX-MED)
                                      >=   SPA-W01-STYUKYMD )
                 AND ( MEDH20-ID    (IDX-MED)
                                      <    "80" )
      *
                   COMPUTE IDX-CHK   = IDX-CHK + 1
                   MOVE    IDX-CHK     TO  SPA-GMN-MEDKBN-MAX
      *
                   MOVE    MEDH20-ZAINUM (IDX-MED)
                                       TO  SPA-NAI-MEDKBN-ZAINUM
                                                              (IDX-CHK)
                   MOVE    MEDH20-CDL  (IDX-MED)
                                       TO  SPA-NAI-MEDKBN-SRYCD
                                                              (IDX-CHK)
                   MOVE    MEDH20-MEIL (IDX-MED)
                                       TO  SPA-GMN-CHK-LBLMEDKBN
                                                              (IDX-CHK)
      *
                   PERFORM 3101-SYS5014-SEL-SEC
                   IF    ( SYS-5014-DISPKBN    =   "1" )
                       MOVE    "T"     TO  SPA-GMN-CHK-MEDKBN (IDX-CHK)
                   ELSE
                       MOVE    "F"     TO  SPA-GMN-CHK-MEDKBN (IDX-CHK)
                   END-IF
      *
                   IF    ( SPA-W01-STYUKYMD   >=   CONST-H300401 )
                       IF    ( MEDH20-ZAINUM (IDX-MED)
                                       =   2022 OR 2024 OR 2031 OR 2002
                                        OR 2007 OR 2039 )
                           MOVE    "T" TO  SPA-GMN-CHK-MEDKBN (IDX-CHK)
                       END-IF
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       3101-SPA-5014-SET-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       3100-SPA-5015-SET-SEC                  SECTION.
      *
      *    ADL入力方法区分編集処理
           PERFORM 3101-ADLINPUTKBNLST-SET-SEC
      *
      *    医療区分引継ぎ区分編集処理
           PERFORM 3101-SETMEDKBNLST-SET-SEC
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5015"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC    TO  SYS-5015-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "5015"          TO  SYS-KANRICD
               MOVE    "*"             TO  SYS-KBNCD
               MOVE    SPA-W01-STYUKYMD TO SYS-STYUKYMD
               PERFORM 900-SYSKANRI-KEY16-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5015-REC
           END-IF
      *
           MOVE    SYS-5015-ADLINPUTKBN TO WRK-CMB-CD
           PERFORM 4200-ADLINPUTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-ADLINPUTKBNLST (1)
                                       TO  SPA-GMN-ADLINPUTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-ADLINPUTKBN-G
           END-IF
      *
           MOVE    SYS-5015-SETMEDKBN  TO  WRK-CMB-CD
           PERFORM 4200-SETMEDKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-SETMEDKBNLST (1)
                                       TO  SPA-GMN-SETMEDKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SETMEDKBN-G
           END-IF
      *
           MOVE    101                 TO  SPA-GMN-CUR
      *
           .
       3100-SPA-5015-SET-EXT.
           EXIT.
      *****************************************************************
      *    医療区分表示情報検索処理
      *****************************************************************
       3101-SYS5014-SEL-SEC            SECTION.
      *
           INITIALIZE                  SYS-5014-REC
           MOVE    "5014"          TO  SYS-5014-KANRICD
           MOVE    SPA-NAI-MEDKBN-ZAINUM (IDX-CHK)
                                   TO  WRK-ZAINUM
           MOVE    WRK-ZAINUM-X    TO  SYS-5014-KBNCD
           MOVE    SPA-W01-STYUKYMD
                                   TO  SYS-5014-STYUKYMD
                                       SYS-5014-EDYUKYMD
           MOVE    SYS-5014-REC    TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5014-REC
           IF    ( FLG-SYSKANRI    =   ZERO )
               CONTINUE
           ELSE
               MOVE    SYS-5014-REC    TO  SYSKANRI-REC
               PERFORM 900-SYSKANRI-KEY16-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5014-REC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    "1"         TO  SYS-5014-DISPKBN
               END-IF
           END-IF
      *
           .
       3101-SYS5014-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ADL入力方法区分編集処理
      *****************************************************************
       3101-ADLINPUTKBNLST-SET-SEC     SECTION.
      *
           INITIALIZE                  SPA-GMN-ADLINPUTKBNLST-G
      *
           IF    ( SPA-W01-STYUKYMD    >=  CONST-H220401 )
      *
               MOVE    1               TO  SPA-GMN-ADLINPUTKBN-MAX
      *
               MOVE    "1"             TO  SPA-GMN-ADLINPUTKBNL    (1)
               MOVE    "区分別に入力"  TO  SPA-GMN-ADLINPUTKBNMEIL (1)
           ELSE
      *
               MOVE    2               TO  SPA-GMN-ADLINPUTKBN-MAX
      *
               MOVE    "1"             TO  SPA-GMN-ADLINPUTKBNL    (1)
               MOVE    "区分別に入力"  TO  SPA-GMN-ADLINPUTKBNMEIL (1)
               MOVE    "2"             TO  SPA-GMN-ADLINPUTKBNL    (2)
               MOVE    "合計点数を直接入力"
                                       TO  SPA-GMN-ADLINPUTKBNMEIL (2)
           END-IF
      *
           .
       3101-ADLINPUTKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    医療区分引継ぎ区分編集処理
      *****************************************************************
       3101-SETMEDKBNLST-SET-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-SETMEDKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-SETMEDKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-SETMEDKBNL    (1)
           MOVE    "引継がない"    TO  SPA-GMN-SETMEDKBNMEIL (1)
           MOVE    "2"             TO  SPA-GMN-SETMEDKBNL    (2)
           MOVE    "前月の状態を引継ぐ"
                                   TO  SPA-GMN-SETMEDKBNMEIL (2)
      *
           .
       3101-SETMEDKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF    ( SPA-WID1-FLG    =   "OK" )
               EVALUATE    SPA-WIDCD
               WHEN "3001"
                   PERFORM 4801-TOUROKU-SEC
               WHEN "3002"
                   PERFORM 4301-DEL-SEC
               END-EVALUATE
           END-IF
      *
           .
       330-WID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM  210-BACK
      *    全選択
           WHEN    "CLICKED"       ALSO    "B02"
               IF    ( SPA-W01-KANRICD     =   "5014" )
                   PERFORM 420-CLEAR-SEC
               END-IF
      *    削除
           WHEN    "CLICKED"       ALSO    "B03"
               IF    ( SPA-W01-KANRICD     =   "5015" )
                   PERFORM 430-DEL-SEC
               END-IF
      *    医療区分
           WHEN    "CLICKED"       ALSO    "CHK_MEDKBN01"
                                     THRU  "CHK_MEDKBN40"
               PERFORM 4101-MEDKBN-CHK-SEC
      *    登録
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 480-TOUROKU-SEC
      *
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
      *    入力個所のセット
           PERFORM 400-GMN-INPUT-SEC
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           MOVE    ZERO        TO      SPA-GMN-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC           SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
             OR  ( FLG-END     NOT =   ZERO  )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    W31-ADLINPUTKBN     TO  SPA-GMN-ADLINPUTKBN-G
           MOVE    W31-SETMEDKBN       TO  SPA-GMN-SETMEDKBN-G
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
           EVALUATE    SPA-W01-KANRICD
           WHEN    "5014"
               CONTINUE
           WHEN    "5015"
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 4101-ADLINPUTKBN-CHK-SEC
               END-IF
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 4101-SETMEDKBN-CHK-SEC
               END-IF
           END-EVALUATE
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    医療区分チェック処理
      *****************************************************************
       4101-MEDKBN-CHK-SEC             SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
      *
           MOVE    WRK-MCP-WIDGET      TO  WRK-WIDGET-MEDKBN-G
      *
           MOVE    WRK-WIDGET-MEDKBN   TO  IDX-CHK
      *
           MOVE    W31-CHK-MEDKBN (IDX-CHK)
                                       TO  SPA-GMN-CHK-MEDKBN (IDX-CHK)
      *
           .
       4101-MEDKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ADL入力方法区分チェック処理
      *****************************************************************
       4101-ADLINPUTKBN-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-ADLINPUTKBN TO WRK-CMB-CD
           PERFORM 4200-ADLINPUTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-ADLINPUTKBNLST (1)
                                       TO  SPA-GMN-ADLINPUTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-ADLINPUTKBN-G
           END-IF
      *
           .
       4101-ADLINPUTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    医療区分引継ぎ区分チェック処理
      *****************************************************************
       4101-SETMEDKBN-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-SETMEDKBN TO WRK-CMB-CD
           PERFORM 4200-SETMEDKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-SETMEDKBNLST (1)
                                       TO  SPA-GMN-SETMEDKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SETMEDKBN-G
           END-IF
      *
           .
       4101-SETMEDKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ADL区分入力方法コンボボックスチェック処理
      *****************************************************************
       4200-ADLINPUTKBNLST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-ADLINPUTKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-ADLINPUTKBNL (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    SPA-GMN-ADLINPUTKBNLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-ADLINPUTKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    医療区分引継ぎ区分コンボボックスチェック処理
      *****************************************************************
       4200-SETMEDKBNLST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-SETMEDKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SETMEDKBNL (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    SPA-GMN-SETMEDKBNLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SETMEDKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W31"               TO  SPA-MOTOPG
      *
      ***  MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    "W01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    条件クリア処理
      *****************************************************************
       420-CLEAR-SEC               SECTION.
      *
           PERFORM VARYING IDX-CHK    FROM    1   BY  1
                   UNTIL ( IDX-CHK    >   CONST-MEDKBN-MAX )
      *
               MOVE    "T"             TO  SPA-GMN-CHK-MEDKBN (IDX-CHK)
      *
           END-PERFORM
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       430-DEL-SEC                     SECTION.
      *
           MOVE    "3002"          TO  SPA-WIDCD
      *
           .
       430-DEL-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       4301-DEL-SEC                    SECTION.
      *
           EVALUATE    SPA-W01-KANRICD
           WHEN    "5015"
               INITIALIZE                      SYSKANRI-REC
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    SPA-W01-KANRICD     TO  SYS-KANRICD
               MOVE    "*"                 TO  SYS-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD TO SYS-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD TO SYS-EDYUKYMD
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    "JOIN"          TO  MCP-PUTTYPE
                   PERFORM 210-BACK
               ELSE
                   MOVE    "4005"          TO  SPA-ERRCD
              END-IF
           END-EVALUATE
      *
           .
       4301-DEL-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       480-TOUROKU-SEC                 SECTION.
      *
      *    入力個所のチェック
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           PERFORM 4900-KANRENCHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           MOVE    "3001"          TO  SPA-WIDCD
      *
           .
       480-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       4801-TOUROKU-SEC                SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           EVALUATE    SPA-W01-KANRICD
           WHEN    "5014"
               PERFORM  4801-SYS5014-TOUROKU-SEC
           WHEN    "5015"
               PERFORM  4801-SYS5015-TOUROKU-SEC
           END-EVALUATE
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE   "JOIN"           TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
      *
           .
       4801-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       4801-SYS5014-TOUROKU-SEC        SECTION.
      *
           MOVE    SPACE               TO  SYS-5014-REC
           INITIALIZE                      SYS-5014-REC
           MOVE    "5014"              TO  SYS-5014-KANRICD
           MOVE    "*"                 TO  SYS-5014-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-5014-STYUKYMD
                                           SYS-5014-EDYUKYMD
           MOVE    SYS-5014-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI    =   ZERO )
               MOVE    SMCNDATE-YMD    TO  SYS-5014-UPYMD
               MOVE    SMCNDATE-HMS    TO  SYS-5014-UPHMS
               MOVE    SPA-HOSPNUM         TO  SYS-5014-HOSPNUM
               MOVE    SYS-5014-REC    TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "4003"      TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    SPA-W01-STYUKYMD
                                       TO  SYS-5014-STYUKYMD
               MOVE    SPA-W01-EDYUKYMD
                                       TO  SYS-5014-EDYUKYMD
               MOVE    SMCNDATE-YMD    TO  SYS-5014-CREYMD
                                           SYS-5014-UPYMD
               MOVE    SMCNDATE-HMS    TO  SYS-5014-UPHMS
               MOVE    SPA-HOSPNUM     TO  SYS-5014-HOSPNUM
               MOVE    SYS-5014-REC    TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "4004"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *
           PERFORM VARYING IDX-CHK    FROM    1   BY  1
                   UNTIL ( IDX-CHK    >   SPA-GMN-MEDKBN-MAX )
                    OR   ( SPA-ERRCD  NOT =   SPACE )
      *
               DISPLAY "A SEL " SPA-NAI-MEDKBN-ZAINUM (IDX-CHK)
               PERFORM 3101-SYS5014-SEL-SEC
               DISPLAY "A RESULT " FLG-SYSKANRI
               IF    ( FLG-SYSKANRI    =   ZERO )
      *
                   MOVE    SMCNDATE-YMD    TO  SYS-5014-UPYMD
                   MOVE    SMCNDATE-HMS    TO  SYS-5014-UPHMS
                   MOVE    SPA-HOSPNUM     TO  SYS-5014-HOSPNUM
      *
                   IF    ( SPA-W01-STYUKYMD
                                       >   SYS-5014-STYUKYMD )
                       MOVE    SYS-5014-REC    TO  SYSKANRI-REC
                       MOVE    SYS-HOSPNUM     TO  SYSUP-HOSPNUM
                       MOVE    SYS-KANRICD     TO  SYSUP-KANRICD
                       MOVE    SYS-KBNCD       TO  SYSUP-KBNCD
                       MOVE    SYS-STYUKYMD    TO  SYSUP-STYUKYMD
                       MOVE    SYS-EDYUKYMD    TO  SYSUP-EDYUKYMD
                       MOVE    SPA-W01-STYUKYMD
                                               TO  WRK-YMD
                       MOVE    "1"             TO  WRK-ZOGENPTN
                       MOVE    -1              TO  WRK-ZOGEN
                       PERFORM 800-CALENDAR-SEC
                       MOVE    WRK-KEISANYMD   TO  SYS-EDYUKYMD
                       MOVE    SYSKANRI-REC    TO  MCPDATA-REC
                       MOVE    "tbl_syskanri"  TO  MCP-TABLE
                       MOVE    "upd1"          TO  MCP-PATHNAME
                       PERFORM 910-DBUPDATE-SEC
                       IF    ( MCP-RC      NOT =   ZERO )
                           MOVE    "4008"      TO  SPA-ERRCD
                       END-IF
                   END-IF
      *
                   IF    ( SPA-ERRCD       =   SPACE )
      *
               DISPLAY "B SEL " SPA-NAI-MEDKBN-ZAINUM (IDX-CHK)
                       PERFORM 48011-SYS5014-EDIT-SEC
               DISPLAY "B RESULT " FLG-SYSKANRI
      *
                       IF    ( SPA-W01-STYUKYMD
                                           >   SYS-5014-STYUKYMD )
                           MOVE    SYS-5014-REC    TO  SYSKANRI-REC
                           MOVE    SPA-W01-STYUKYMD TO SYS-STYUKYMD
                           MOVE    SPA-W01-EDYUKYMD TO SYS-EDYUKYMD
                           MOVE    SMCNDATE-YMD    TO  SYS-CREYMD
                           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
                           MOVE    "tbl_syskanri"  TO  MCP-TABLE
                           MOVE    "key"           TO  MCP-PATHNAME
                           PERFORM 910-DBINSERT-SEC
                           IF    ( MCP-RC      NOT =   ZERO )
                               MOVE    "4009"      TO  SPA-ERRCD
                           END-IF
                       ELSE
                           MOVE    SYS-5014-REC    TO  SYSKANRI-REC
                           MOVE    SYS-HOSPNUM     TO  SYSUP-HOSPNUM
                           MOVE    SYS-KANRICD     TO  SYSUP-KANRICD
                           MOVE    SYS-KBNCD       TO  SYSUP-KBNCD
                           MOVE    SYS-STYUKYMD    TO  SYSUP-STYUKYMD
                           MOVE    SYS-EDYUKYMD    TO  SYSUP-EDYUKYMD
      *
                           MOVE    SPA-W01-STYUKYMD TO SYS-STYUKYMD
                           MOVE    SPA-W01-EDYUKYMD TO SYS-EDYUKYMD
                           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
                           MOVE    "tbl_syskanri"  TO  MCP-TABLE
                           MOVE    "upd1"          TO  MCP-PATHNAME
                           PERFORM 910-DBUPDATE-SEC
                           IF    ( MCP-RC      NOT =   ZERO )
                               MOVE    "4008"      TO  SPA-ERRCD
                           END-IF
                       END-IF
                   END-IF
               ELSE
      *
                   MOVE    SPACE       TO      SYS-5014-REC
                   INITIALIZE                  SYS-5014-REC
                   MOVE    "5014"      TO  SYS-5014-KANRICD
                   MOVE    SPA-NAI-MEDKBN-ZAINUM (IDX-CHK)
                                       TO  WRK-ZAINUM
                   MOVE    WRK-ZAINUM-X
                                       TO  SYS-5014-KBNCD
                   MOVE    SPA-W01-STYUKYMD
                                       TO  SYS-5014-STYUKYMD
                   MOVE    SPA-W01-EDYUKYMD
                                       TO  SYS-5014-EDYUKYMD
                   MOVE    SPA-NAI-MEDKBN-ZAINUM (IDX-CHK)
                                       TO  SYS-5014-ZAINUM
                   IF    ( SPA-W01-STYUKYMD    >=  CONST-H200401 )
                       MOVE    "00000000"
                                       TO  SYS-5014-SRYCD 
                   ELSE
                       MOVE    SPA-NAI-MEDKBN-SRYCD  (IDX-CHK)
                                       TO  SYS-5014-SRYCD 
                   END-IF
      *
                   PERFORM 48011-SYS5014-EDIT-SEC
      *
                   MOVE    SMCNDATE-YMD    TO  SYS-5014-CREYMD
                                               SYS-5014-UPYMD
                   MOVE    SMCNDATE-HMS    TO  SYS-5014-UPHMS
      *
                   MOVE    SPA-HOSPNUM         TO  SYS-5014-HOSPNUM
                   MOVE    SYS-5014-REC    TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       MOVE    "4002"      TO  SPA-ERRCD
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4801-SYS5014-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    医療区分表示情報編集処理
      *****************************************************************
       48011-SYS5014-EDIT-SEC          SECTION.
      *
           IF    ( SPA-GMN-CHK-MEDKBN (IDX-CHK)    =   "T" )
               MOVE    "1"             TO  SYS-5014-DISPKBN
           ELSE
               MOVE    SPACE           TO  SYS-5014-DISPKBN
           END-IF
      *
           .
       48011-SYS5014-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       4801-SYS5015-TOUROKU-SEC        SECTION.
      *
           MOVE    SPACE               TO  SYS-5015-REC
           INITIALIZE                      SYS-5015-REC
           MOVE    "5015"              TO  SYS-5015-KANRICD
           MOVE    "*"                 TO  SYS-5015-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-5015-STYUKYMD
                                           SYS-5015-EDYUKYMD
           MOVE    SYS-5015-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI    =   ZERO )
               MOVE    SYSKANRI-REC    TO  SYS-5015-REC
           ELSE
               MOVE    SPACE           TO  SYS-5015-REC
               INITIALIZE                  SYS-5015-REC
           END-IF
      *
           MOVE    SPA-GMN-ADLINPUTKBN
                                       TO  SYS-5015-ADLINPUTKBN
      *
           MOVE    SPA-GMN-SETMEDKBN   TO  SYS-5015-SETMEDKBN
      *
           IF    ( FLG-SYSKANRI    =   ZERO )
      *
               MOVE    SYS-5015-REC        TO  SYSKANRI-REC
      *
               MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
               MOVE    SPA-W01-EDYUKYMD    TO  SYS-EDYUKYMD
               MOVE    SYS-5015-KEY        TO  SYSUPD-KEY
      *
               MOVE    SMCNDATE-YMD        TO  SYS-5015-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-5015-UPHMS
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "4006"      TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    SPA-HOSPNUM     TO  SYS-5015-HOSPNUM
               MOVE    "5015"          TO  SYS-5015-KANRICD
               MOVE    "*"             TO  SYS-5015-KBNCD
               MOVE    SPA-W01-STYUKYMD
                                       TO  SYS-5015-STYUKYMD
               MOVE    SPA-W01-EDYUKYMD
                                       TO  SYS-5015-EDYUKYMD
               MOVE    SMCNDATE-YMD    TO  SYS-5015-CREYMD
                                           SYS-5015-UPYMD
               MOVE    SMCNDATE-HMS    TO  SYS-5015-UPHMS
               MOVE    SYS-5015-REC    TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "4007"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4801-SYS5015-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    共通関連チェック
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
      *
           .
      *
       4900-KANRENCHK-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W31    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  W31
           INITIALIZE                      W31
      *
           MOVE    SPA-W01-STYUKYMDH   TO  W31-STYUKYMD
           MOVE    SPA-W01-EDYUKYMDH   TO  W31-EDYUKYMD
      *
           PERFORM VARYING IDX-CHK    FROM    1   BY  1
                   UNTIL ( IDX-CHK     >   CONST-GMN-MEDKBN-MAX )
      *
               MOVE    SPA-GMN-CHK-MEDKBN (IDX-CHK)
                                       TO  W31-CHK-MEDKBN (IDX-CHK)
               MOVE    SPA-GMN-CHK-LBLMEDKBN (IDX-CHK)
                                       TO  W31-CHK-LBLMEDKBN
                                                          (IDX-CHK)
      *
               IF    ( IDX-CHK         >   SPA-GMN-MEDKBN-MAX )
                   MOVE    "F"         TO  W31-CHK-MEDKBN (IDX-CHK)
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-ADLINPUTKBN-G
                                           TO  W31-ADLINPUTKBN
           MOVE    SPA-GMN-ADLINPUTKBN-MAX
                                           TO  W31-ADLINPUTKBN-CNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-ADLINPUTKBN-MAX )
      *
               MOVE    SPA-GMN-ADLINPUTKBNLST (IDX0)
                                       TO  W31-ADLINPUTKBN-ITM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-SETMEDKBN-G
                                           TO  W31-SETMEDKBN
           MOVE    SPA-GMN-SETMEDKBN-MAX
                                           TO  W31-SETMEDKBN-CNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-SETMEDKBN-MAX )
      *
               MOVE    SPA-GMN-SETMEDKBNLST (IDX0)
                                       TO  W31-SETMEDKBN-ITM (IDX0)
      *
           END-PERFORM
      *
           EVALUATE    SPA-W01-KANRICD
           WHEN    "5014"
               MOVE    "全選択"        TO  W31-B02-LABEL
               MOVE    SPACE           TO  W31-B03-LABEL
           WHEN    "5015"
               MOVE    SPACE           TO  W31-B02-LABEL
               MOVE    "削除"          TO  W31-B03-LABEL
           END-EVALUATE
      *
           IF    ( SPA-W01-STYUKYMD    >=  CONST-H220401 )
               CONTINUE
           ELSE
               STRING  "＊合計点数の直接入力は"    DELIMITED BY SIZE
                       "平成２２年３月まで有効です。"
                                                   DELIMITED BY SIZE
               INTO    W31-LBLADLINPUTINFO
               END-STRING
           END-IF
      *
      *    項目状態編集
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
           MOVE    WIDGET-NORMAL   TO  W31-B03-STATE
                                       W31-ADLINPUTKBN-STATE
                                       W31-ADLINPUTKBN-ITM-STATE
      *
           IF    ( SPA-W01-KANRICD =   "5015" )
               IF    ( SPA-W01-STYUKYMD    =   "00000000" )
                OR   ( SPA-W01-SELNUM      =   ZERO )
                   MOVE    WIDGET-INSENSITIVE  TO  W31-B03-STATE
               END-IF
           END-IF
      *
           PERFORM VARYING IDX-CHK    FROM    1   BY  1
                   UNTIL ( IDX-CHK    >   CONST-GMN-MEDKBN-MAX )
      *
               MOVE    WIDGET-NORMAL  TO  W31-CHK-MEDKBN-STATE (IDX-CHK)
               IF    ( IDX-CHK         >   SPA-GMN-MEDKBN-MAX )
                   MOVE    WIDGET-INSENSITIVE
                                      TO  W31-CHK-MEDKBN-STATE (IDX-CHK)
               END-IF
               IF    ( SPA-W01-STYUKYMD   >=   CONST-H300401 )
                   IF    ( SPA-NAI-MEDKBN-ZAINUM (IDX-CHK)
                               =   2022 OR 2024 OR 2031 OR 2002
                                OR 2007 OR 2039 )
                       MOVE    WIDGET-INSENSITIVE
                                      TO  W31-CHK-MEDKBN-STATE (IDX-CHK)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-CUR         =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-SKIP-CUR-SEC
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    001 THRU    040
               MOVE    "CHK_MEDKBN"        TO  WRK-WIDGET-MEDKBN-LBL
               MOVE    SPA-GMN-CUR         TO  WRK-WIDGET-MEDKBN
               MOVE    WRK-WIDGET-MEDKBN-G TO  MCP-WIDGET
           WHEN    101
               MOVE    "ADLINPUTKBN"       TO  MCP-WIDGET
           WHEN    102
               MOVE    "SETMEDKBN"         TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE        WIDGET-INSENSITIVE  TO  W31-NB01-STATE
                                               W31-NB02-STATE
           EVALUATE    SPA-W01-KANRICD
           WHEN    "5014"
               MOVE    ZERO                TO  W31-PAGENO
               MOVE    WIDGET-NORMAL       TO  W31-NB01-STATE
           WHEN    "5015"
               MOVE    1                   TO  W31-PAGENO
               MOVE    WIDGET-NORMAL       TO  W31-NB02-STATE
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "CHK_MEDKBN01"      THRU    "CHK_MEDKBN39"
               MOVE    WRK-MCP-WIDGET  TO  WRK-WIDGET-MEDKBN-G
               COMPUTE SPA-GMN-CUR =   WRK-WIDGET-MEDKBN   +   1
           WHEN    "CHK_MEDKBN40"
               MOVE    212         TO  SPA-GMN-CUR
           WHEN    "ADLINPUTKBN"
               MOVE    102         TO  SPA-GMN-CUR
           WHEN    "SETMEDKBN"
               MOVE    212         TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5012-SKIP-CUR-SEC           SECTION.
      *
           PERFORM VARYING IDX-CHK    FROM    1   BY  1
                   UNTIL ( IDX-CHK    >   CONST-GMN-MEDKBN-MAX )
      *
               IF    ( SPA-GMN-CUR    =   IDX-CHK )
                 AND ( W31-CHK-MEDKBN-STATE (IDX-CHK)
                                      =   WIDGET-INSENSITIVE )
                   IF    ( IDX-CHK    =   CONST-GMN-MEDKBN-MAX )
                       COMPUTE SPA-GMN-CUR =   212
                   ELSE
                       COMPUTE SPA-GMN-CUR =   SPA-GMN-CUR +   1
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
      *
       5012-SKIP-CUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "4001"  THRU    "4009"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W31"               TO  SPA-MOTOPG
           MOVE    "WERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-WIDCD
           WHEN    "3001"
               MOVE    "登録します。よろしいですか"
                                        TO  WRK-WIDMSG
           WHEN    "3002"
               MOVE    "削除します。よろしいですか"
                                        TO  WRK-WIDMSG
           END-EVALUATE
      *
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W31"                TO  SPA-MOTOPG
           MOVE    "WID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
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
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
      *    増減パターン 1:日／2:月／3:年
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
            EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY16)
      *****************************************************************
       900-SYSKANRI-KEY16-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY16-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理(KEY)
      *****************************************************************
       900-TENSU-KEY-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
      *
           MOVE    WRK-TNS-SRYCD   TO  TNS-SRYCD
           MOVE    SPA-W01-STYUKYMD
                                   TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               INITIALIZE              TNS-TENSU-REC
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ追加処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
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
           MOVE    "DBFETCH"       TO  MCP-FUNC
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
       910-DBCLOSECURSOR-SEC           SECTION.
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
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

