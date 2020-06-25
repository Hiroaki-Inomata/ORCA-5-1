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
      *
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGI02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院業務
      *  コンポーネント名  : 入院歴表示
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02.09.19    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    03.02.05  自院歴作成データを考慮する
      *  01.00.02    NACL-太田    04.10.14  退院事由対応
      *  03.05.00    NACL-太田    07.05.15  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *
      *    画面非表示
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    入退院登録共通領域
           COPY    "I0COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "I0SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-ENTRYCHK                PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-TAIN                    PIC 9(01).
           03  FLG-TAINFUKA                PIC 9(01).
           03  FLG-PTBYOMEI                PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
           03  IDX4                        PIC 9(05).
           03  IDX5                        PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD     PIC X(08).
           03  SYS-TIME    PIC X(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MOTOPG                  PIC X(08).
           03  WRK-YMD                     PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY                 PIC 9(04).
               05  WRK-SMM                 PIC 9(02).
               05  WRK-SDD                 PIC 9(02).
           03  WRK-HENYMDG                 PIC X(09).
           03  WRK-KANACHK-MAE-INPUT       PIC X(400).
           03  WRK-KBNCD                   PIC X(02).
           03  WRK-TAIINCD                 PIC X(20).
           03  WRK-BRMNUM                  PIC X(06).
           03  WRK-999999                  PIC 999999.
           03  WRK-ZZZZZ9                  PIC ZZZZZ9.
           03  WRK-ZZZZ9                   PIC ZZZZ9.
           03  WRK-STYMD                   PIC X(08).
           03  WRK-EDYMD                   PIC X(08).
           03  WRK-NISSU1                  PIC 9(05).
           03  WRK-NISSU2                  PIC 9(05).
           03  WRK-TUSAN                   PIC 9(05).
           03  WRK-NYUINYMD                PIC X(08).
           03  WRK-TAIINYMD                PIC X(08).
           03  WRK-CMB-CD                  PIC X(05).
           03  WRK-CMB-G.
               05  WRK-CMB-IDX             PIC 9(03).
               05  WRK-CMB-ITM             PIC X(100).
           03  WRK-KEY-AREA.
               05  WRK-KEY-BTUNUM          PIC X(02).
               05  WRK-KEY-BRMNUM          PIC X(08).
               05  WRK-KEY-NYUINKA         PIC X(02).
               05  WRK-KEY-TOKUTEI-KBN     PIC X(01).
               05  WRK-KEY-TOKUTEI-NYUIN   PIC X(02).
               05  WRK-KEY-HKNCOMBINUM     PIC X(04).
           03  WRK-BIKO-ITEM-AREA.
               05  WRK-BIKO-YMD            PIC X(10).
               05  WRK-BIKO-ITEM-CNT       PIC 9(03).
               05  WRK-BIKO-ITEM-OCC       OCCURS  4.
                   07  WRK-BIKO-ITEM-KBN   PIC 9(01).
                   07  WRK-BIKO-ITEM       PIC X(10).
               05  WRK-BIKO-NAIYO          PIC X(100).
               05  WRK-BIKO-PNT            PIC 9(03).
               05  WRK-BIKO-IDX            PIC 9(03).
           03  WRK-NYUINRRK-EDIT-MAX       PIC 9(05).
           03  WRK-IDO-CNT                 PIC 9(05).
           03  WRK-ST-IDX                  PIC 9(05).
      *
       01  CONST-AREA.
           03  CONST-H180401       PIC X(08)   VALUE "20060401".
           03  CONST-BIKO-VAL.
               05                  PIC X(20)   VALUE "入院科".
               05                  PIC X(20)   VALUE "科".
      *
               05                  PIC X(20)   VALUE "病室".
               05                  PIC X(20)   VALUE "室".
      *
               05                  PIC X(20)   VALUE "入院料".
               05                  PIC X(20)   VALUE "入".
      *
               05                  PIC X(20)   VALUE "保険組合せ".
               05                  PIC X(20)   VALUE "保".
      *
           03  CONST-BIKO-R  REDEFINES CONST-BIKO-VAL.
               05    CONST-BIKO-OCC1        OCCURS  4.
                   07    CONST-BIKO-OCC2    OCCURS  2.
                       09  CONST-BIKO      PIC  X(20).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    （診療科入院科情報）
           COPY    "CPSK1005.INC".
      *    （職員情報）
           COPY    "CPSK1010.INC".
      *    （病棟情報）
           COPY    "CPSK5001.INC".
      *    （病室情報）
           COPY    "CPSK5002.INC".
      *
           COPY    "CPSK5013.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    他院歴付加情報
       01  PTTAINFUKA-REC.
           COPY    "CPPTTAINFUKA.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    数値号変換サブ
           COPY    "CPORCSNUM.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    入院請求期間取得
           COPY    "CPORCSS006.INC".
      *
      *    病名編集
           COPY    "CPORCSMKBYOMEI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
        LINKAGE                     SECTION.
      *
      *     テーブルアクセス
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA31SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
                                               MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA.
      *****************************************************************
      *    主    処    理
      *****************************************************************
      *
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-I0FREE.
           MOVE    SPA-WORK        TO  SPA-I0KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD.
           MOVE    ZERO            TO  FLG-END.
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF  FLG-END   =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF.
      *
           MOVE    SPA-I0KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON.
           MOVE    SPA-I0FREE      TO  SPA-FREE
           .
      *
       000-PROC-SEC-EXT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                   SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "I0ERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
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
           MOVE    "I02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "I05"
                   PERFORM 350-I05-SET-SEC
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    退院事由設定画面より戻り時の処理
      *****************************************************************
       350-I05-SET-SEC          SECTION.
      *
           IF    ( SPA-I02-I05-TAIINCD     =   SPACE )
               MOVE    SPACE               TO  SPA-GMN-I02-TAIINCD
                                               SPA-NAI-I02-TAIINCD
           ELSE
               MOVE    SPA-I02-I05-TAIINCD TO  WRK-KBNCD
               PERFORM 3101-S5013-SEL-SEC
               MOVE    SYS-5013-TANNAME    TO  SPA-GMN-I02-TAIINCD
      *
               MOVE    SPA-I02-I05-TAIINCD TO  SPA-NAI-I02-TAIINCD
      *
           END-IF
      *
           .
       350-I05-SET-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE              I02
                                   SPA-I02
      *
      *    最新入院歴検索処理
           PERFORM 900-PTNYUINRRK-KEY4-SEL-SEC
           IF    ( FLG-PTNYUINRRK      NOT =   ZERO )
               GO  TO  310-SPASET-EXT
           END-IF
      *
           MOVE    ZERO    TO  FLG-TAIN
      *
      *    最新入院歴編集処理
           PERFORM 3101-NEW-NYUINRRK-EDIT-SEC
      *
      *    入院歴リスト編集処理
           PERFORM 3101-NYUINRRKLST-EDIT-SEC
      *
      *    コンボボックス初期化
           PERFORM 310-CMB-SYOKI-SEC
      *
           MOVE    SPA-GMN-I02-NYUGAI-ITM (01)
                                       TO  SPA-GMN-I02-NYUGAI
      *
           IF    ( FLG-TAIN    =   ZERO )
               MOVE    ZERO    TO  SPA-NAI-I02-NYURRK-FLG
      *        病名リスト編集処理
               PERFORM 3101-BYOMEILST-EDIT-SEC
           ELSE
               MOVE    1       TO  SPA-NAI-I02-NYURRK-FLG
               MOVE    SPACE   TO  SPA-GMN-I02-NYUGAI
      *
               PERFORM 900-PTTAINFUKA-KEY-SEL-SEC
               IF    ( FLG-TAINFUKA    =   ZERO )
      *
      *            入院料算定
                   MOVE    PTTAINFUKA-KHNRYO
                                       TO  SPA-GMN-I02-SANTEI
      *
      *            病名リスト編集処理
                   PERFORM 3101-BYOMEILST-EDIT2-SEC
               END-IF
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-I02-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    最新入院歴編集処理
      *****************************************************************
       3101-NEW-NYUINRRK-EDIT-SEC      SECTION.
      *
           IF    ( PTNYUINRRK-JTIKBN   =   "5" OR "6" )
               MOVE    1           TO  FLG-TAIN
           ELSE
               MOVE    ZERO        TO  FLG-TAIN
           END-IF
      *
      *    初回
           EVALUATE    PTNYUINRRK-SHOKAIKBN
           WHEN    "1"
               MOVE    "初回"      TO  SPA-GMN-I02-SHOKAI
           WHEN    "2"
               MOVE    "継続"      TO  SPA-GMN-I02-SHOKAI
           END-EVALUATE
      *
      *    状態
           EVALUATE    TRUE
           WHEN    PTNYUINRRK-JTIKBN   =   "5"
	       IF    ( PTNYUINRRK-TAINRELKBN   =   "1" )
	           MOVE    "他院歴（特別な関係にある医療機関）"
		                       TO  SPA-GMN-I02-JYOTAI
	       ELSE
                   MOVE    "他院歴"    TO  SPA-GMN-I02-JYOTAI
	       END-IF
           WHEN    PTNYUINRRK-JTIKBN   =   "6"
               MOVE    "自院入院歴"
                                   TO  SPA-GMN-I02-JYOTAI
           WHEN    PTNYUINRRK-TAIINYMD =   "99999999"
               MOVE    "入院中"    TO  SPA-GMN-I02-JYOTAI
           WHEN    OTHER
               MOVE    "退院済み"  TO  SPA-GMN-I02-JYOTAI
           END-EVALUATE
      *
      *    病棟名
           IF    ( PTNYUINRRK-JTIKBN   =   "5" OR "6" )
               MOVE    PTNYUINRRK-BTUNAME
                                       TO  SPA-GMN-I02-BTUNAME
           ELSE
               PERFORM 3101-S5001-SEL-SEC
               MOVE    SYS-5001-BTU-NAME
                                       TO  SPA-GMN-I02-BTUNAME
           END-IF
      *
      *    病室番号
           PERFORM 3101-BRMNUM-EDIT-SEC
           MOVE    WRK-BRMNUM      TO  SPA-GMN-I02-BRMNUM
      *
      *    入院日
           MOVE    PTNYUINRRK-NYUINYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I02-NYUINYMD
           MOVE    PTNYUINRRK-NYUINYMD
                                   TO  SPA-NAI-I02-NYUINYMD
      *
      *    退院日、退院事由
           IF    ( PTNYUINRRK-TAIINYMD     =   "99999999" )
               MOVE    SPACE       TO  SPA-GMN-I02-TAIINYMD
                                       SPA-NAI-I02-TAIINYMD
                                       SPA-GMN-I02-TAIINCD
           ELSE
               MOVE    PTNYUINRRK-TAIINYMD
                                   TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-I02-TAIINYMD
               MOVE    PTNYUINRRK-TAIINYMD
                                   TO  SPA-NAI-I02-TAIINYMD
               MOVE    PTNYUINRRK-TAIINCD
                                   TO  WRK-KBNCD
               PERFORM 3101-S5013-SEL-SEC
               MOVE    SYS-5013-TANNAME
                                   TO  SPA-GMN-I02-TAIINCD
               MOVE    PTNYUINRRK-TAIINCD
                                   TO  SPA-NAI-I02-TAIINCD
           END-IF
      *
      *    入院科
           MOVE    PTNYUINRRK-NYUINKA
                                   TO  WRK-KBNCD
           PERFORM 3101-S1005-SEL-SEC
           MOVE    SYS-1005-SRYKANAME
                                   TO  SPA-GMN-I02-NYUINKA
      *
      *    主治医
           PERFORM 3101-S1010-SEL-SEC
           MOVE    SYS-1010-NAME   TO  SPA-GMN-I02-DR
      *
      *    入院時時間区分
           IF    ( PTNYUINRRK-NYUINYMD <   CONST-H180401 )
               CONTINUE
           ELSE
               IF    ( PTNYUINRRK-DOUJITSUKBN  =   "1" )
                   MOVE    "1"     TO  SPA-NAI-I02-DOUJITSUKBN-IN
                   MOVE    PTNYUINRRK-TIMEKBN-IN
                                   TO  SPA-NAI-I02-TIMEKBN-IN
               END-IF
           END-IF
      *
           IF    ( PTNYUINRRK-TAIINYMD <   CONST-H180401 )
               CONTINUE
           ELSE
               INITIALIZE                      SS006-AREA
               MOVE    SPA-I01-I02-PTID    TO  SPA-PTID
               MOVE    PTNYUINRRK-TAIINYMD TO  SS006-SKYSTYMD
                                               SS006-SKYEDYMD
               MOVE    "1"                 TO  SS006-SKYKBN
               CALL    "ORCSS006"          USING
                                           SS006-AREA
                                           SPA-AREA
               IF    ( SS006-DOJITUFLG-ED  =   "1" )
                   MOVE    "1"         TO  SPA-NAI-I02-DOUJITSUKBN-OT
                   MOVE    SS006-TIMEKBN-ED
                                       TO  SPA-NAI-I02-TIMEKBN-OT
               END-IF
           END-IF
      *
           .
       3101-NEW-NYUINRRK-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟情報検索処理
      *****************************************************************
       3101-S5001-SEL-SEC              SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE   SPA-HOSPNUM          TO  SYS-HOSPNUM
           MOVE    "5001"              TO  SYS-KANRICD
           MOVE    PTNYUINRRK-BTUNUM   TO  SYS-KBNCD
           MOVE    PTNYUINRRK-TENNYUYMD TO SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
           .
      *
       3101-S5001-SEL-EXT.
           EXIT.
      *****************************************************************
      *    病室番号編集処理
      *****************************************************************
       3101-BRMNUM-EDIT-SEC            SECTION.
      *
           MOVE    SPACE           TO  WRK-BRMNUM
      *
           IF    ( PTNYUINRRK-BRMNUM (3 : 6)
                                   IS  NUMERIC )
               MOVE    PTNYUINRRK-BRMNUM (3 : 6)
                                   TO  WRK-999999
               MOVE    WRK-999999
                                   TO  WRK-ZZZZZ9
               MOVE    WRK-ZZZZZ9
                                   TO  WRK-BRMNUM
           ELSE
               MOVE    PTNYUINRRK-BRMNUM (3 : 6)
                                   TO  WRK-BRMNUM
           END-IF
      *
           .
       3101-BRMNUM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療科入院科情報検索処理
      *****************************************************************
       3101-S1005-SEL-SEC              SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE   SPA-HOSPNUM          TO  SYS-HOSPNUM
           MOVE    "1005"              TO  SYS-KANRICD
           MOVE    WRK-KBNCD           TO  SYS-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1005-REC
      *
           .
      *
       3101-S1005-SEL-EXT.
           EXIT.
      *****************************************************************
      *    職員情報検索処理
      *****************************************************************
       3101-S1010-SEL-SEC              SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE   SPA-HOSPNUM          TO  SYS-HOSPNUM
           MOVE    "1010"          TO  SYS-KANRICD
           MOVE    "1"             TO  SYS-KBNCD (1 : 1)
           MOVE    PTNYUINRRK-DRCD1 ( 2 : 4 )
                                   TO  SYS-KBNCD (2 : )
           IF    ( PTNYUINRRK-TAIINYMD    =    "99999999" )
               MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           ELSE
               MOVE    PTNYUINRRK-TAIINYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           END-IF
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1010-REC
      *
           .
      *
       3101-S1010-SEL-EXT.
           EXIT.
      *****************************************************************
      *    退院事由情報検索処理
      *****************************************************************
       3101-S5013-SEL-SEC              SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE   SPA-HOSPNUM          TO  SYS-HOSPNUM
           MOVE    "5013"              TO  SYS-KANRICD
           MOVE    WRK-KBNCD           TO  SYS-KBNCD
           MOVE    PTNYUINRRK-TAIINYMD TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5013-REC
      *
           .
      *
       3101-S5013-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院歴リスト編集処理
      *****************************************************************
       3101-NYUINRRKLST-EDIT-SEC       SECTION.
      *
           MOVE    ZERO            TO  WRK-NYUINRRK-EDIT-MAX
      *
      *    入院履歴検索
           INITIALIZE  PTNYUINRRK-REC
           PERFORM 3101-NYUINRRK-KEY-EDIT-SEC
      *
           PERFORM 900-PTNYUINRRK-KEY5-SEL-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >       100 )
                OR       ( FLG-PTNYUINRRK  NOT =   ZERO )
      *
               MOVE    IDX1        TO  SPA-GMN-I02-TENKALST-CNT
      *
      *        病棟／病室／入院科が変わっているかチェック
               IF    ( PTNYUINRRK-BTUNUM   NOT =   WRK-KEY-BTUNUM  )
                 OR  ( PTNYUINRRK-BRMNUM   NOT =   WRK-KEY-BRMNUM  )
                 OR  ( PTNYUINRRK-NYUINKA  NOT =   WRK-KEY-NYUINKA )
                   PERFORM 3101-NYUINRRK-EDIT-SEC
               END-IF
      *
      *        備考内容
               IF    ( IDX1        >   1 )
                   PERFORM 3101-NYUINRRKLST-BIKO-EDIT-SEC
                   MOVE    WRK-BIKO-NAIYO 
                                       TO  SPA-GMN-I02-TIDONAIYO (IDX1)
               END-IF
      *
      *        転入日
               MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  SPA-NAI-I02-TTENNYUYMD (IDX1)
      *
      *        転出日
               MOVE    PTNYUINRRK-TENSTUYMD
                                       TO  SPA-NAI-I02-TTENSTUYMD (IDX1)
      *
               PERFORM 3101-NYUINRRK-KEY-EDIT-SEC
      *
               PERFORM 900-PTNYUINRRK-KEY5-FET-SEC
      *
           END-PERFORM
      *
      *    入院履歴クローズ
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    転出日を再編集
           MOVE    1               TO  IDX1
           PERFORM UNTIL ( IDX1    >   SPA-GMN-I02-TENKALST-CNT )
      *
               IF    ( SPA-NAI-I02-TLSTEDIT-FLG (IDX1)     =   1 )
                   COMPUTE WRK-ST-IDX  =   IDX1    +   1
                   PERFORM VARYING IDX2    FROM    WRK-ST-IDX  BY  1
                           UNTIL ( IDX2    >   SPA-GMN-I02-TENKALST-CNT)
                             OR  ( SPA-NAI-I02-TLSTEDIT-FLG (IDX2)
                                           =   1 )
                       MOVE    SPA-NAI-I02-TTENSTUYMD (IDX2)
                           TO  SPA-NAI-I02-TTENSTUYMD (IDX1)
                   END-PERFORM
      *
                   MOVE    IDX2        TO  IDX1
      *
               ELSE
                   COMPUTE IDX1    =   IDX1    +   1
               END-IF
      *
           END-PERFORM
      *
      *    転入日・転出日・日数・通算日数を編集
           MOVE    ZERO            TO  WRK-TUSAN
                                       WRK-IDO-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I02-TENKALST-CNT )
      *
               IF    ( SPA-NAI-I02-TLSTEDIT-FLG (IDX1) =   1 )
                   PERFORM 3101-IDOYMD-NISSU-EDIT-SEC
               END-IF
      *
           END-PERFORM
      *
      *    不要な行を削除する
           IF    ( SPA-GMN-I02-TNYUINKA (2)        =   SPACE )
             AND ( SPA-GMN-I02-TIDONAIYO(2)    NOT =   SPACE )
               MOVE    SPA-GMN-I02-TIDONAIYO(2)
                                           TO  SPA-GMN-I02-TIDONAIYO(1)
               PERFORM VARYING IDX1    FROM    2   BY  1
                       UNTIL ( IDX1    >=  SPA-GMN-I02-TENKALST-CNT )
      *
                   COMPUTE IDX2    =   IDX1    +   1
                   MOVE    SPA-GMN-I02-TENKALST-ITM (IDX2)
                                   TO  SPA-GMN-I02-TENKALST-ITM (IDX1)
                   MOVE    SPA-GMN-I02-TENKALST-ITM (IDX2)
                                   TO  SPA-GMN-I02-TENKALST-ITM (IDX1)
                   MOVE    SPA-NAI-I02-TENKALST-ITM (IDX2)
                                   TO  SPA-NAI-I02-TENKALST-ITM (IDX1)
      *
               END-PERFORM
      *
               MOVE    SPA-GMN-I02-TENKALST-CNT    TO  IDX1
      *
               INITIALIZE  SPA-NAI-I02-TENKALST-ITM (IDX1)
                           SPA-NAI-I02-TENKALST-ITM (IDX1)
               COMPUTE SPA-GMN-I02-TENKALST-CNT
                   =   SPA-GMN-I02-TENKALST-CNT    -   1
           END-IF
      *
      *
           .
      *
       3101-NYUINRRKLST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院歴ＫＥＹ項目編集処理
      *****************************************************************
       3101-NYUINRRK-KEY-EDIT-SEC      SECTION.
      *
           MOVE    PTNYUINRRK-BTUNUM   TO  WRK-KEY-BTUNUM
           MOVE    PTNYUINRRK-BRMNUM   TO  WRK-KEY-BRMNUM
           MOVE    PTNYUINRRK-NYUINKA  TO  WRK-KEY-NYUINKA
           MOVE    PTNYUINRRK-TOKUTEI-KBN
                                       TO  WRK-KEY-TOKUTEI-KBN
           MOVE    PTNYUINRRK-TOKUTEI-NYUIN
                                       TO  WRK-KEY-TOKUTEI-NYUIN
           MOVE    PTNYUINRRK-HKNCOMBINUM
                                       TO  WRK-KEY-HKNCOMBINUM
      *
           .
       3101-NYUINRRK-KEY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院歴編集処理
      *****************************************************************
       3101-NYUINRRK-EDIT-SEC          SECTION.
      *
           COMPUTE WRK-NYUINRRK-EDIT-MAX
               =   WRK-NYUINRRK-EDIT-MAX   +   1
      *
      *    入院科
           MOVE    PTNYUINRRK-NYUINKA
                                       TO  WRK-KBNCD
           PERFORM 3101-S1005-SEL-SEC
           MOVE    SYS-1005-SRYKANAME
                                       TO  SPA-GMN-I02-TNYUINKA (IDX1)
      *
      *    病棟名
           IF    ( PTNYUINRRK-JTIKBN   =   "5" OR "6" )
               MOVE    PTNYUINRRK-BTUNAME
                                       TO  SPA-GMN-I02-TBTUNAME (IDX1)
           ELSE
               PERFORM 3101-S5001-SEL-SEC
               MOVE    SYS-5001-BTU-NAME
                                       TO  SPA-GMN-I02-TBTUNAME (IDX1)
           END-IF
      *
      *    病室番号
           PERFORM 3101-BRMNUM-EDIT-SEC
           MOVE    WRK-BRMNUM      TO  SPA-GMN-I02-TBRMNUM (IDX1)
      *
      *    リスト編集フラグ
           MOVE    1               TO  SPA-NAI-I02-TLSTEDIT-FLG (IDX1)
      *
           .
      *
       3101-NYUINRRK-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院歴リスト備考編集処理
      *****************************************************************
       3101-NYUINRRKLST-BIKO-EDIT-SEC  SECTION.
      *
           INITIALIZE   WRK-BIKO-ITEM-AREA
      *
           MOVE    PTNYUINRRK-TENNYUYMD    TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG             TO  WRK-BIKO-YMD
      *
           MOVE    ZERO                    TO  IDX3
           MOVE    "--"                    TO  WRK-BIKO-ITEM (1)
      *
      *    入院科
           IF    ( PTNYUINRRK-NYUINKA  NOT =   WRK-KEY-NYUINKA )
               COMPUTE IDX3    =   IDX3    +   1
               MOVE    1                   TO  WRK-BIKO-ITEM-KBN (IDX3)
           END-IF
      *
      *    病室
           IF    ( PTNYUINRRK-BTUNUM   NOT =   WRK-KEY-BTUNUM )
             OR  ( PTNYUINRRK-BRMNUM   NOT =   WRK-KEY-BRMNUM )
               COMPUTE IDX3    =   IDX3    +   1
               MOVE    2                   TO  WRK-BIKO-ITEM-KBN (IDX3)
           END-IF
      *
      *    入院料
           IF    ( PTNYUINRRK-TOKUTEI-KBN
                                       NOT =   WRK-KEY-TOKUTEI-KBN )
             OR  ( PTNYUINRRK-TOKUTEI-NYUIN
                                       NOT =   WRK-KEY-TOKUTEI-NYUIN )
               COMPUTE IDX3    =   IDX3    +   1
               MOVE    3                   TO  WRK-BIKO-ITEM-KBN (IDX3)
           END-IF
      *
      *    保険組合せ
           IF    ( PTNYUINRRK-HKNCOMBINUM
                                       NOT =   WRK-KEY-HKNCOMBINUM )
               COMPUTE IDX3    =   IDX3    +   1
               MOVE    4                   TO  WRK-BIKO-ITEM-KBN (IDX3)
           END-IF
      *
           IF    ( IDX3    >   ZERO )
      *
               MOVE    IDX3                    TO  WRK-BIKO-ITEM-CNT
               MOVE    1                       TO  WRK-BIKO-IDX
      *        長い名称で編集
               PERFORM 3101-BIKO-EDIT-SEC
               IF    ( WRK-BIKO-PNT    >   34 )
      *            短い名称で編集
                   MOVE    2                   TO  WRK-BIKO-IDX
                   PERFORM 3101-BIKO-EDIT-SEC
               END-IF
           ELSE
               MOVE    WRK-BIKO-YMD            TO  WRK-BIKO-NAIYO
           END-IF
      *
           .
       3101-NYUINRRKLST-BIKO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    備考編集処理
      *****************************************************************
       3101-BIKO-EDIT-SEC              SECTION.
      *
           MOVE    SPACE                   TO  WRK-BIKO-NAIYO
      *
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL ( IDX3        >   WRK-BIKO-ITEM-CNT )
               MOVE    WRK-BIKO-ITEM-KBN (IDX3)
                                           TO  IDX4
               MOVE    CONST-BIKO (IDX4 WRK-BIKO-IDX)
                                           TO  WRK-BIKO-ITEM (IDX3)
           END-PERFORM
      *
           MOVE    1                       TO  WRK-BIKO-PNT
           STRING  WRK-BIKO-YMD        DELIMITED   BY  SIZE
                   "［"                DELIMITED   BY  SIZE
                   WRK-BIKO-ITEM (1)
                                       DELIMITED   BY  SPACE
           INTO    WRK-BIKO-NAIYO
           WITH    POINTER WRK-BIKO-PNT
           END-STRING
      *
           PERFORM VARYING IDX3    FROM    2   BY  1
                   UNTIL ( IDX3        >   WRK-BIKO-ITEM-CNT )
      *
               STRING  "／"                    DELIMITED   BY  SIZE
                   WRK-BIKO-ITEM (IDX3)
                                               DELIMITED   BY  SPACE
               INTO    WRK-BIKO-NAIYO
               WITH    POINTER WRK-BIKO-PNT
               END-STRING
      *
           END-PERFORM
      *
           STRING "］"             DELIMITED   BY  SIZE
           INTO    WRK-BIKO-NAIYO
           WITH    POINTER WRK-BIKO-PNT
           END-STRING
      *
           COMPUTE WRK-BIKO-PNT    =   WRK-BIKO-PNT    -   1
      *
           .
       3101-BIKO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    異動日・日数編集処理
      *****************************************************************
       3101-IDOYMD-NISSU-EDIT-SEC      SECTION.
      *
           COMPUTE WRK-IDO-CNT =   WRK-IDO-CNT +   1
      *
      *    転入日
           MOVE    SPA-NAI-I02-TTENNYUYMD (IDX1)
                                       TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-I02-TTENNYUYMD (IDX1)
      *
      *    転出日
           IF    ( SPA-NAI-I02-TTENSTUYMD (IDX1)
                                       =   "99999999" )
               MOVE    SPACE           TO  SPA-GMN-I02-TTENSTUYMD (IDX1)
           ELSE
               MOVE    SPA-NAI-I02-TTENSTUYMD (IDX1)
                                       TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-I02-TTENSTUYMD (IDX1)
           END-IF
      *
           MOVE    SPA-NAI-I02-TTENNYUYMD (IDX1)
                                       TO  WRK-STYMD
           MOVE    SPA-NAI-I02-TTENSTUYMD (IDX1)
                                       TO  WRK-EDYMD
      *
      *    日数の算出について
      *    １．最新歴の場合
      *       ・入院中の場合は空白
      *       ・退院済みの場合は転入日から転出日の間の日数
      *         （転出日を含める）
      *    ２．最新歴で無い場合
      *       ・転入日から転出日の間の日数
      *         （転出日を含めない）
           IF    ( WRK-IDO-CNT         =   WRK-NYUINRRK-EDIT-MAX )
               IF     ( SPA-GMN-I02-TTENSTUYMD (IDX1)
                                   =   SPACE )
                   MOVE    SPACE       TO  SPA-GMN-I02-TNISSU (IDX1)
                                           SPA-GMN-I02-TTUSAN (IDX1)
               ELSE
                   PERFORM 5002-KIKAN-CALC-SEC
                   MOVE    WRK-NISSU2  TO  WRK-ZZZZ9
                   MOVE    WRK-ZZZZ9   TO  SPA-GMN-I02-TNISSU (IDX1)
                   COMPUTE WRK-TUSAN   =  WRK-TUSAN  + WRK-NISSU2
                   MOVE    WRK-TUSAN   TO  WRK-ZZZZ9
                   MOVE    WRK-ZZZZ9   TO  SPA-GMN-I02-TTUSAN (IDX1)
               END-IF
           ELSE
               PERFORM 5002-KIKAN-CALC-SEC
               MOVE    WRK-NISSU1  TO  WRK-ZZZZ9
               MOVE    WRK-ZZZZ9   TO  SPA-GMN-I02-TNISSU (IDX1)
               COMPUTE WRK-TUSAN   =   WRK-TUSAN  + WRK-NISSU1
               MOVE    WRK-TUSAN   TO  WRK-ZZZZ9
               MOVE    WRK-ZZZZ9   TO  SPA-GMN-I02-TTUSAN (IDX1)
           END-IF
      *
           .
       3101-IDOYMD-NISSU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病名リスト編集処理
      *****************************************************************
       3101-BYOMEILST-EDIT-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-I02-BYOMEILST
      *
           MOVE    SPA-NAI-I02-NYUINYMD
                                       TO  WRK-NYUINYMD
           IF    ( SPA-NAI-I02-TAIINYMD
                                       =   SPACE )
               MOVE    "99999999"      TO  WRK-TAIINYMD
           ELSE
               MOVE    SPA-GMN-I02-TAIINYMD
                                       TO  WRK-TAIINYMD
           END-IF
      *
           MOVE    ZERO                TO  IDX2
      *
      *    患者病名マスタ検索
           PERFORM 900-PTBYOMEI-KEY6-SEL-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1            >   500 )
                OR       ( FLG-PTBYOMEI    NOT =   ZERO )
      *
      *--------IF    ( WRK-NYUINYMD    <=  PTBYO-SRYYMD )
      *-------- AND  ( WRK-TAIINYMD    >=  PTBYO-SRYYMD )
               EVALUATE    TRUE
               WHEN    ( SPA-GMN-I02-NYUGAI-CD =  SPACE )
               WHEN    ( SPA-GMN-I02-NYUGAI-CD =  "1"   )
                   AND ( PTBYO-NYUGAIKBN       =  SPACE OR "1" )
               WHEN    ( SPA-GMN-I02-NYUGAI-CD =  "2"   )
                   AND ( PTBYO-NYUGAIKBN       =  SPACE OR "2" )
      *
                   PERFORM 3101-BYOMEI-EDIT-SEC
               END-EVALUATE
      *
      *--------END-IF
      *
               PERFORM 900-PTBYOMEI-KEY6-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       3101-BYOMEILST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病名編集処理
      *****************************************************************
       3101-BYOMEI-EDIT-SEC            SECTION.
      *
           COMPUTE IDX2    =   IDX2    +   1
           MOVE    IDX2    TO  SPA-GMN-I02-BYOMEILST-CNT
      *
      *    主病区分
           IF    ( PTBYO-SYUBYOFLG =   "1" )
               MOVE    "◎"    TO  SPA-GMN-I02-TSYUBYO (IDX2)
           END-IF
      *
      *    入外区分
           EVALUATE    PTBYO-NYUGAIKBN
           WHEN    SPACE
               MOVE    "入外" TO  SPA-GMN-I02-TNYUGAI (IDX2)
           WHEN    "1"
               MOVE    " 入"  TO  SPA-GMN-I02-TNYUGAI (IDX2)
           WHEN    "2"
               MOVE    " 外"  TO  SPA-GMN-I02-TNYUGAI (IDX2)
           END-EVALUATE
      *
      *    診療科
           MOVE    PTBYO-SRYKA TO  WRK-KBNCD
           PERFORM 3101-S1005-SEL-SEC
           MOVE    SYS-1005-SRYKANAME
                                   TO  SPA-GMN-I02-TSRYKA (IDX2)
      *
      *    病名
           INITIALIZE              ORCSMKBYOMEIAREA
           MOVE    PTBYO-UTAGAIFLG
                                   TO  SMKBYO-UTAGAIFLG
           MOVE    PTBYO-HOSOKU-COMMENT
                                   TO  SMKBYO-HOSOKU-COMMENT
           MOVE    PTBYO-BYOMEI
                                   TO  SMKBYO-BYOMEI
      *
           CALL    "ORCSMKBYOMEI"  USING
                                   ORCSMKBYOMEIAREA
           MOVE    SMKBYO-BYOMEI
                                   TO  SPA-GMN-I02-TBYOMEI (IDX2)
      *
      *    開始日
           MOVE    PTBYO-SRYYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG TO  SPA-GMN-I02-TSTSRYYMD (IDX2)
      *
      *    転帰日
           MOVE    PTBYO-TENKIYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG TO  SPA-GMN-I02-TTENKIYMD (IDX2)
      *
      *    転帰理由
           EVALUATE    PTBYO-TENKIKBN
           WHEN "1"
                   MOVE    "治ゆ"  TO  SPA-GMN-I02-TTENKI (IDX2)
           WHEN "2"
                   MOVE    "死亡"  TO  SPA-GMN-I02-TTENKI (IDX2)
           WHEN "3"
                   MOVE    "中止"  TO  SPA-GMN-I02-TTENKI (IDX2)
           WHEN "4"
                   MOVE    "移行"  TO  SPA-GMN-I02-TTENKI (IDX2)
           END-EVALUATE
      *
           .
       3101-BYOMEI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病名リスト編集処理（他院歴で入力された内容を表示）
      *****************************************************************
       3101-BYOMEILST-EDIT2-SEC        SECTION.
      *
      *    病名
           MOVE    ZERO            TO  IDX2
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   5 )
      *
               IF    ( PTTAINFUKA-BYOMEI (IDX1)    NOT =   SPACE )
                   COMPUTE IDX2    =   IDX2    +   1
                   MOVE    IDX2    TO  SPA-GMN-I02-BYOMEILST-CNT
                   IF    ( IDX1    =   1 )
                       MOVE    "◎"    TO  SPA-GMN-I02-TSYUBYO (IDX2)
                   END-IF
                   MOVE    PTTAINFUKA-BYOMEI (IDX1)
                                       TO  SPA-GMN-I02-TBYOMEI (IDX2)
               END-IF
      *
           END-PERFORM
      *
           .
      *
       3101-BYOMEILST-EDIT2-EXT.
           EXIT.
      *****************************************************************
      *    コンボボックス初期処理
      *****************************************************************
       310-CMB-SYOKI-SEC               SECTION.
      *
           INITIALIZE              SPA-GMN-I02-CMB-DAT-AREA
      *
      *    入外コンボ
           PERFORM 310-CMB-NYUGAI-SET-SEC
      *
           .
       310-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    入外コンボ初期処理
      *****************************************************************
       310-CMB-NYUGAI-SET-SEC          SECTION.
      *
           MOVE    3                   TO  SPA-GMN-I02-NYUGAI-CNT
      *
           MOVE    " "                 TO  SPA-GMN-I02-NYUGAIL    (01)
           MOVE    "すべて"            TO  SPA-GMN-I02-NYUGAIMEIL (01)
      *
           MOVE    "1"                 TO  SPA-GMN-I02-NYUGAIL    (02)
           MOVE    "入院"              TO  SPA-GMN-I02-NYUGAIMEIL (02)
      *
           MOVE    "2"                 TO  SPA-GMN-I02-NYUGAIL    (03)
           MOVE    "入院外"            TO  SPA-GMN-I02-NYUGAIMEIL (03)
      *
           .
       310-CMB-NYUGAI-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 210-BACK
           WHEN    "CLICKED"       ALSO    "B05"
               PERFORM 250-I05-SENI-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    入外区分
           WHEN    "ACTIVATE"      ALSO    "NYUGAI"
               PERFORM 4101-NYUGAI-CHK-SEC
           END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分チェック処理
      *****************************************************************
       4101-NYUGAI-CHK-SEC             SECTION.
      *
           MOVE    1               TO  SPA-GMN-I02-CUR
           MOVE    I02-NYUGAI      TO  SPA-GMN-I02-NYUGAI
      *
      *    入外区分サーチ処理
           MOVE    SPA-GMN-I02-NYUGAI-CD
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-NYUGAI-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    "0001"      TO  SPA-ERRCD
               GO  TO  4101-NYUGAI-CHK-EXT
           END-IF
      *
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I02-NYUGAI
      *
           IF    ( SPA-NAI-I02-NYURRK-FLG     =   ZERO )
               PERFORM 3101-BYOMEILST-EDIT-SEC
           END-IF
      *
           .
      *
       4101-NYUGAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入外区分コンボチェック処理
      *****************************************************************
       4103-CMB-NYUGAI-CHK-SEC         SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I02-NYUGAI-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I02-NYUGAIL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    SPA-GMN-I02-NYUGAI-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-NYUGAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPA-NAI-I02-TAIINYMD
                                       TO  SPA-I01-I02-TAIINYMD
           MOVE    SPA-NAI-I02-TAIINCD TO  SPA-I01-I02-TAIINCD
      *
           MOVE    "I01"               TO  SPA-SAKIPG
           MOVE    "I02"               TO  SPA-MOTOPG
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    退院事由設定画面へ遷移処理
      *****************************************************************
       250-I05-SENI-SEC                SECTION.
      *
           MOVE    SPA-I01-I02-PTID    TO  SPA-I02-I05-PTID
           MOVE    SPA-I01-I02-PTRRKNUM
                                       TO  SPA-I02-I05-PTRRKNUM
           MOVE    SPA-NAI-I02-TAIINYMD
                                       TO  SPA-I02-I05-TAIINYMD
           MOVE    SPA-NAI-I02-TAIINCD TO  SPA-I02-I05-TAIINCD
      *
           MOVE    SPACE           TO  SPA-MOTOPG
      *
           MOVE    SPA-I0KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I0FREE      TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGI05"      USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-I0FREE
           MOVE    SPA-WORK        TO  SPA-I0KYOUTU
      *
           MOVE    "TAIINCD"       TO  MCP-WIDGET
      *
           MOVE    "I02"           TO  SPA-MOTOPG
           MOVE    "I05"           TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "I05"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE  1             TO  FLG-END
      *
           .
      *
       250-I05-SENI-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "I02    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
      *    初回
           MOVE    SPA-GMN-I02-SHOKAI      TO  I02-SHOKAI
      *    状態
           MOVE    SPA-GMN-I02-JYOTAI      TO  I02-JYOTAI
      *    病棟名
           MOVE    SPA-GMN-I02-BTUNAME     TO  I02-BTUNAME
      *    病室番号
           MOVE    SPA-GMN-I02-BRMNUM      TO  I02-BRMNUM
      *    入院日
           MOVE    SPA-GMN-I02-NYUINYMD    TO  I02-NYUINYMD
      *    退院日
           MOVE    SPA-GMN-I02-TAIINYMD    TO  I02-TAIINYMD
      *    退院事由
           MOVE    SPA-GMN-I02-TAIINCD     TO  I02-TAIINCD
      *    入院科
           MOVE    SPA-GMN-I02-NYUINKA     TO  I02-NYUINKA
      *    主治医
           MOVE    SPA-GMN-I02-DR          TO  I02-DR
      *    算定
           MOVE    SPA-GMN-I02-SANTEI      TO  I02-SANTEI
      *
      *    転科（転棟・転室）履歴
           MOVE    SPA-GMN-I02-TENKALST-CNT
                                           TO  I02-TENKALST-CNT
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I02-TENKALST-CNT )
      *        入院科
               MOVE    SPA-GMN-I02-TNYUINKA   (IDX1)
                                           TO  I02-TNYUINKA   (IDX1)
      *        病棟名
               MOVE    SPA-GMN-I02-TBTUNAME   (IDX1)
                                           TO  I02-TBTUNAME   (IDX1)
      *        病室番号
               MOVE    SPA-GMN-I02-TBRMNUM    (IDX1)
                                           TO  I02-TBRMNUM    (IDX1)
      *        転入日
               MOVE    SPA-GMN-I02-TTENNYUYMD (IDX1)
                                           TO  I02-TTENNYUYMD (IDX1)
      *        転出日
               MOVE    SPA-GMN-I02-TTENSTUYMD (IDX1)
                                           TO  I02-TTENSTUYMD (IDX1)
      *        日数
               MOVE    SPA-GMN-I02-TNISSU (IDX1)
                                           TO  I02-TNISSU (IDX1)
      *        通算日数
               MOVE    SPA-GMN-I02-TTUSAN (IDX1)
                                           TO  I02-TTUSAN (IDX1)
               IF    ( SPA-GMN-I02-TENKALST-SEL
                                           =   IDX1    )
                   MOVE    "T"        TO  I02-TENKALST-SELECT(IDX1)
               ELSE
                   MOVE    "F"        TO  I02-TENKALST-SELECT(IDX1)
               END-IF
               MOVE    SPA-GMN-I02-TIDONAIYO (IDX1)
                                      TO  I02-TIDONAIYO (IDX1)
           END-PERFORM
      *
      *    入外区分
           MOVE    SPA-GMN-I02-NYUGAI
                                   TO  I02-NYUGAI
           MOVE    SPA-GMN-I02-NYUGAI-CNT
                                   TO  I02-NYUGAI-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I02-NYUGAI-CNT )
               MOVE    SPA-GMN-I02-NYUGAI-ITM (IDX1)
                                   TO  I02-NYUGAI-ITM (IDX1)
           END-PERFORM
      *
      *    病名情報
           MOVE       SPA-GMN-I02-BYOMEILST-CNT
                                           TO   I02-BYOMEILST-CNT
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I02-BYOMEILST-CNT )
      *        主病名区分
               MOVE    SPA-GMN-I02-TSYUBYO   (IDX1)
                                           TO   I02-TSYUBYO   (IDX1)
      *        入外区分
               MOVE    SPA-GMN-I02-TNYUGAI   (IDX1)
                                           TO   I02-TNYUGAI   (IDX1)
      *        診療科
               MOVE    SPA-GMN-I02-TSRYKA    (IDX1)
                                           TO   I02-TSRYKA    (IDX1)
      *        病名
               MOVE    SPA-GMN-I02-TBYOMEI   (IDX1)
                                           TO   I02-TBYOMEI   (IDX1)
      *        開始日
               MOVE    SPA-GMN-I02-TSTSRYYMD (IDX1)
                                           TO   I02-TSTSRYYMD (IDX1)
      *        終了日
               MOVE    SPA-GMN-I02-TTENKIYMD (IDX1)
                                           TO   I02-TTENKIYMD (IDX1)
      *        転帰日
               MOVE    SPA-GMN-I02-TTENKI    (IDX1)
                                           TO   I02-TTENKI    (IDX1)
               IF    ( SPA-GMN-I02-BYOMEILST-SEL
                                           =   IDX1    )
                   MOVE    "T"       TO   I02-BYOMEILST-SELECT(IDX1)
               ELSE
                   MOVE    "F"       TO   I02-BYOMEILST-SELECT(IDX1)
               END-IF
           END-PERFORM
      *
           IF    ( SPA-NAI-I02-DOUJITSUKBN-IN  =   "1" )
               EVALUATE    SPA-NAI-I02-TIMEKBN-IN
               WHEN    "0"
                   MOVE    "食なし"  TO    I02-LBLTIMEKBN-IN
               WHEN    "1"
                   MOVE    "朝から"  TO    I02-LBLTIMEKBN-IN
               WHEN    "2"
                   MOVE    "昼から"  TO    I02-LBLTIMEKBN-IN
               WHEN    "3"
                   MOVE    "夕から"  TO    I02-LBLTIMEKBN-IN
               END-EVALUATE
           END-IF
      *
           IF    ( SPA-NAI-I02-DOUJITSUKBN-OT  =   "1" )
               EVALUATE    SPA-NAI-I02-TIMEKBN-OT
               WHEN    "0"
                   MOVE    "食なし"  TO    I02-LBLTIMEKBN-OT
               WHEN    "1"
                   MOVE    "朝まで"  TO    I02-LBLTIMEKBN-OT
               WHEN    "2"
                   MOVE    "昼まで"  TO    I02-LBLTIMEKBN-OT
               WHEN    "3"
                   MOVE    "夕まで"  TO    I02-LBLTIMEKBN-OT
               END-EVALUATE
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
           MOVE    WIDGET-NORMAL   TO  I02-B05-STT
                                       I02-NYUGAI-STT
                                       I02-NYUGAI-ITM-STT
      *
           IF    ( SPA-NAI-I02-NYURRK-FLG  NOT =   ZERO )
               MOVE    WIDGET-INSENSITIVE
                                   TO  I02-B05-STT
                                       I02-NYUGAI-STT
                                       I02-NYUGAI-ITM-STT
           END-IF
      *
           IF    ( SPA-NAI-I02-TAIINYMD
                                       =   SPACE )
               MOVE    WIDGET-INSENSITIVE
                                   TO  I02-B05-STT
           END-IF
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC              SECTION.
      *
           IF    ( SPA-GMN-I02-CUR =   1   )
               IF    ( I02-NYUGAI-STT  =   WIDGET-NORMAL )
                   MOVE    "NYUGAI"    TO  MCP-WIDGET
               ELSE
                   MOVE    "B01"       TO  MCP-WIDGET
               END-IF
           ELSE
               MOVE    "B01"           TO  MCP-WIDGET
           END-IF
      *
           .
      *
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "入力エラー"    TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  I0ERR
           MOVE    SPA-ERRCD            TO  I0ERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  I0ERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "I02"                TO  SPA-MOTOPG
           MOVE    "I0ERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "I0ERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
                                           WRK-SYMD
      *
           IF    ( WRK-YMD         =   SPACE )
               GO  TO  5002-HIZUKE-CHK-EXT
           END-IF
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    期間算出処理
      *****************************************************************
       5002-KIKAN-CALC-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-NISSU1
                                           WRK-NISSU2
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY5-AREA
           MOVE    "51"                TO  LNK-DAY5-IRAI
           MOVE    WRK-STYMD           TO  LNK-DAY5-START
           MOVE    WRK-EDYMD           TO  LNK-DAY5-END
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                           LNK-DAY5-AREA
      *
           MOVE    LNK-DAY5-NISSU1     TO  WRK-NISSU1
           MOVE    LNK-DAY5-NISSU2     TO  WRK-NISSU2
      *
           .
       5002-KIKAN-CALC-EXT.
           EXIT.
      *****************************************************************
      *    全角半角混在チェック
      *****************************************************************
       800-ENRTRY-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-MAE-INPUT
                                   =   SPACE )
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    SPACE           TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    1           TO  FLG-ENTRYCHK
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
      *    全角でない場合はエラーとする。
           IF      ( KANACHK-SYUBETU   NOT =   2 )
               MOVE    1           TO  FLG-ENTRYCHK
           END-IF
      *
           .
       800-ENRTRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY4-SEL-SEC       SECTION.
      *
      *    入院履歴検索処理
           MOVE    ZERO            TO  FLG-PTNYUINRRK
           INITIALIZE  PTNYUINRRK-REC
      *
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-I01-I02-PTID    TO  PTNYUINRRK-PTID
           MOVE    SPA-I01-I02-PTRRKNUM TO PTNYUINRRK-RRKNUM
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY5-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-I01-I02-PTID    TO  PTNYUINRRK-PTID
           MOVE    SPA-I01-I02-PTRRKNUM TO PTNYUINRRK-RRKNUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       900-PTNYUINRRK-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴FETCH処理
      *****************************************************************
       900-PTNYUINRRK-KEY5-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       900-PTNYUINRRK-KEY5-FET-EXT.
           EXIT.
      *****************************************************************
      *    他院付加情報検索処理
      *****************************************************************
       900-PTTAINFUKA-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-TAINFUKA
      *
           INITIALIZE  PTTAINFUKA-REC
           MOVE    SPA-HOSPNUM     TO  PTTAINFUKA-HOSPNUM
           MOVE    SPA-I01-I02-PTID TO PTTAINFUKA-PTID
           MOVE    SPA-I01-I02-PTRRKNUM
                                   TO  PTTAINFUKA-RRKNUM
           MOVE    PTTAINFUKA-REC  TO  MCPDATA-REC
           MOVE    "tbl_pttainfuka" TO MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTTAINFUKA-REC
           ELSE
               MOVE    1           TO  FLG-TAINFUKA
           END-IF
      *
           MOVE    "tbl_pttainfuka" TO MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      * 
           .
      *
       900-PTTAINFUKA-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
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
      *****************************************************************
      *    患者病名検索処理
      *****************************************************************
       900-PTBYOMEI-KEY6-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-PTBYOMEI
      *
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-I01-I02-PTID    TO  PTBYO-PTID
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTBYOMEI-REC
           ELSE
               INITIALIZE              PTBYOMEI-REC
               MOVE    1           TO  FLG-PTBYOMEI
           END-IF
      *
           .
       900-PTBYOMEI-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者病名FETCH処理
      *****************************************************************
       900-PTBYOMEI-KEY6-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-PTBYOMEI
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTBYOMEI-REC
           ELSE
               INITIALIZE              PTBYOMEI-REC
               MOVE    1           TO  FLG-PTBYOMEI
           END-IF
      *
           .
       900-PTBYOMEI-KEY6-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
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
      *    ＤＢ検索処理（FHETCHは行わない)
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
       911-DBSELECT-EXT.
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
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
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
       910-DBDELETE-SEC                 SECTION.
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
      *    ＤＢＩＮＳＥＲＴ処理
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
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

