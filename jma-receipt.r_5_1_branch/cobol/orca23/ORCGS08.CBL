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
       PROGRAM-ID.                 ORCGS08.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 収納業務
      *  コンポーネント名  : 出産一時金画面
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04.04.00    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
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
      *    収納共通領域
           COPY    "S01COMMON-SPA".
      *
           COPY    "S08SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-BUNBEN                  PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-ENTRYCHK                PIC 9(01).
           03  FLG-KYUSEIRRK               PIC 9(01).
           03  FLG-SYUNOU                  PIC 9(01).
           03  FLG-SAIKEISAN               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                        PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
           03  IDX-INFO                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-SYMD                    PIC X(10).
           03  WRK-HENYMDG                 PIC X(10).
           03  WRK-CMB-CD                  PIC X(05).
           03  WRK-CMB-G.
               05  WRK-CMB-ITM             PIC X(100).
           03  WRK-KANACHK-STR             PIC X(100).
           03  WRK-AGE-G.
               05  WRK-AGE-ZZ9             PIC ZZ9.
               05  WRK-AGE-LBL             PIC X(02).
           03  WRK-TEATE                   PIC 9(08).
           03  WRK-FTNTOTAL                PIC 9(08).
           03  WRK-SIDMSG                  PIC X(80).
           03  WRK-NINPUFTN-MONEY          PIC 9(10).
           03  WRK-CREYMD                  PIC X(08).
           03  WRK-KYUSEINAME              PIC X(100).
           03  WRK-GMN-SKYYM               PIC X(06).
           03  WRK-SKYYMD.
               05  WRK-SKYYM               PIC X(06).
               05  WRK-SKYDD               PIC X(02).
           03  WRK-GMN-BUNBENKBN           PIC X(01).
           03  WRK-STATE                   PIC S9(09).
           03  WRK-VISIBLE                 PIC X(01).
           03  WRK-LABEL-G.
               05  WRK-LBLNYU-MONEY        PIC X(20).
               05  WRK-LBLRMSGK-MONEY      PIC X(20).
               05  WRK-LBLKAIJO-MONEY      PIC X(20).
               05  WRK-LBLBUNBEN-MONEY     PIC X(20).
               05  WRK-LBLSHIN-MONEY       PIC X(20).
               05  WRK-LBLKNS-MONEY        PIC X(20).
               05  WRK-LBLSYC-MONEY        PIC X(20).
               05  WRK-LBLSNK-MONEY        PIC X(20).
               05  WRK-LBLETC-MONEY        PIC X(20).
               05  WRK-LBLICBFTN-MONEY     PIC X(20).
               05  WRK-LBLNINPUFTN-MONEY   PIC X(20).
               05  WRK-LBLDAIRI-MONEY      PIC X(20).
               05  WRK-LBLMEIPRTKBN        PIC X(20).
           03  WRK-BASE-MONEY              PIC S9(08).
      *
       01  CONST-AREA.
           03  CONST-H211001               PIC X(08)   VALUE "20091001".
           03  CONST-H270101               PIC X(08)   VALUE "20150101".
           03  CONST-H2904                 PIC X(08)   VALUE "201704".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1001.INC".
      *
      *    旧姓履歴
       01  KYUSEIRRK-REC.
           COPY  "CPKYUSEIRRK.INC".
      *
      *    分娩管理
       01  BUNBEN-REC.
           COPY    "CPBUNBEN.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    分娩負担金計算サブ
       01  LNKBUNBEN-REC.
           COPY  "CPBUNBEN.INC"    REPLACING  == BUNBEN-==
                                          BY  ==LNKBUNBEN-==.
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
           COPY    "CPORCSLOCK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "CPORCHCN63.INC".
           COPY    "CPONPRTDATA.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
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
           COPY    "ORCA23SCRAREA.INC".
      *
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
           MOVE    SPA-FREE        TO  SPA-S08FREE.
           MOVE    SPA-WORK        TO  SPA-S01KYOUTU
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
           MOVE    SPA-S01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-S08FREE     TO  SPA-FREE
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
           IF    ( SPA-MOTOPG          =   "SERR" )
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
           MOVE    "S08"               TO  MCP-WINDOW
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
           EVALUATE    SPA-MOTOPG
           WHEN    "SID1"
               PERFORM 3001-SID1-SET-SEC
               MOVE   SPACE        TO  SPA-SIDCD
           WHEN    "S09"
               PERFORM 3001-S09-SET-SEC
           WHEN    OTHER
               PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険情報変更画面ＯＫ処理
      *****************************************************************
       3001-S09-SET-SEC            SECTION.
      *
           IF    ( SPA-S09-BTN =   "B12" )
               MOVE    SPA-GMN-S09-HKNJANUM
                                   TO  SPA-GMN-HKNJANUM
               MOVE    SPA-GMN-S09-SYAKOK
                                   TO  SPA-GMN-SYAKOK
               MOVE    SPA-GMN-S09-HONKZK
                                   TO  SPA-GMN-HONKZK
               MOVE    SPA-GMN-S09-KIGO
                                   TO  SPA-GMN-KIGO
               MOVE    SPA-GMN-S09-NUM TO  SPA-GMN-NUM
               IF    ( SPA-GMN-S09-HKNJANUM (1:2)
                                   =   "67" )
                AND  ( SPA-GMN-S09-HKNJANUM (8:1)  NOT =   SPACE )
                   MOVE    SPA-GMN-S09-HKNJANUM (3:)
                                   TO  SPA-NAI-HKNJANUM-S
               ELSE
                   MOVE    SPA-GMN-S09-HKNJANUM
                                   TO  SPA-NAI-HKNJANUM-S
               END-IF
      *
               PERFORM 3121-TEISYUTUSAKI-EDIT-SEC
               MOVE    SPA-GMN-TEISYUTUSAKI    TO  WRK-CMB-CD
               PERFORM 4200-TEISYUTUSAKILST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-TEISYUTUSAKI-G
      *
           END-IF
      *
           .
       3001-S09-SET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（Ｉ５ＩＤ１）ＯＫ処理
      *****************************************************************
       3001-SID1-SET-SEC         SECTION.
      *
           IF    ( SPA-SID1-FLG   =   "OK" )
               EVALUATE    SPA-SIDCD
               WHEN    "3001"
                   PERFORM 2201-TOUROKU-SEC
               WHEN    "3002"
                   PERFORM 3201-DEL-SEC
               END-EVALUATE
           END-IF
           .
       3001-SID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-S08
           INITIALIZE                  SPA-S08
      *
           PERFORM 800-ORCSBUNBENFTN-SEC
      *
           PERFORM 900-BUNBEN-KEY3-SEL-SEC
           IF    ( FLG-BUNBEN      =   ZERO )
               IF    ( SPA-S08-NYUGAIKBN   =   "1" )
                   IF    ( BUNBEN-UPDKBN   =   ZERO OR "3" )
                       IF    ( BUNBEN-UPDKBN       =   "3" )
                           MOVE    2   TO  SPA-NAI-NNEW
                       ELSE
                           MOVE    1   TO  SPA-NAI-NNEW
                       END-IF
                   END-IF
               END-IF
           ELSE
               IF    ( SPA-S08-NYUGAIKBN   =   "2" )
                   MOVE    1       TO  SPA-NAI-GNEW
               END-IF
           END-IF
      *
      *    コンボボックス編集処理
           PERFORM 310-COMBO-EDIT-SEC
      *
           PERFORM 311-SPASET-SEC
      *
           PERFORM 312-SPASET-SEC
      *
           MOVE    SPA-GMN-MEIPRTKBNLST (2)
                                       TO  SPA-GMN-MEIPRTKBN-G
      *
           MOVE    0               TO  SPA-NAI-LOCK
      *    PERFORM 990-LOCK-IN-SEC
      *    IF    ( SPA-ERRCD   NOT =   SPACE )
      *        MOVE    1               TO  SPA-NAI-LOCK
      *        MOVE    SLOCK-MSG       TO  SPA-NAI-SLOCK-MSG
      *    END-IF
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       311-SPASET-SEC              SECTION.
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           IF    ( SPA-S08-NYUGAIKBN   =   "1" )
               PERFORM 900-PTNYUINRRK-KEY4-SEL-SEC
               MOVE    PTNYUINRRK-NYUINYMD
                                   TO  SPA-NAI-SRYYMD
      *
               MOVE    PTNYUINRRK-NYUINYMD TO  WRK-SYMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-NYUINYMD
               IF    ( PTNYUINRRK-TAIINYMD NOT =   "99999999" )
                   MOVE    PTNYUINRRK-TAIINYMD
                                   TO      SPA-NAI-SRYYMD
                   MOVE    PTNYUINRRK-TAIINYMD
                                           TO  WRK-SYMD
                   PERFORM 800-HIZUKE-CHK-SEC
                   MOVE    WRK-HENYMDG     TO  SPA-GMN-TAIINYMD
               END-IF
      *
           ELSE
               PERFORM 900-SYUNOU-KEY-SEL-SEC
               MOVE    SYU-SYUHKNNUM
                                   TO  SPA-NAI-GHKNNUM
               MOVE    SYU-SRYYMD  TO  SPA-NAI-SRYYMD
               MOVE    SYU-SRYYMD  TO  WRK-SYMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-NYUINYMD
           END-IF
      *
           MOVE    SPACE               TO  WRK-KYUSEINAME
           PERFORM 900-KYUSEIRRK-KEY3-SEL-SEC
           IF    ( FLG-KYUSEIRRK   =   ZERO )
               MOVE    KYUSEI-NAME
                                       TO  WRK-KYUSEINAME
           END-IF
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1001"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-NAI-SRYYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1001-REC
           IF    ( SYS-1001-SANKANUM   IS  NOT NUMERIC )
            OR   ( SYS-1001-SANKANUM   =   ZERO    )
               MOVE    ZERO            TO  SPA-NAI-SANKANUM
           ELSE
               MOVE    SYS-1001-SANKANUM
                                       TO  SPA-NAI-SANKANUM
           END-IF
      *
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
           MOVE    LNKBUNBEN-NAME      TO  SPA-GMN-KANANAME
           IF    ( WRK-KYUSEINAME      =   SPACE )
               MOVE    PTINF-NAME      TO  SPA-GMN-NAME
           ELSE
               MOVE    WRK-KYUSEINAME  TO  SPA-GMN-NAME
           END-IF
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 800-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-BIRTHDAY-MOM
      *
           INITIALIZE ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    SPA-SYSYMD          TO  AGECHK-SRYYMD
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"            USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
           MOVE    AGECHK-NENREI2-YY   TO  SPA-GMN-AGE
           MOVE    BUNBEN-KANYUKBN     TO  SPA-GMN-SYAKOK
           MOVE    BUNBEN-HONKZKKBN    TO  SPA-GMN-HONKZK
           MOVE    BUNBEN-HKNJANUM     TO  SPA-GMN-HKNJANUM
           MOVE    BUNBEN-KIGO         TO  SPA-GMN-KIGO
           MOVE    BUNBEN-NUM          TO  SPA-GMN-NUM
      *
           MOVE    BUNBEN-NYU-MONEY    TO  SPA-GMN-NYU-MONEY
           MOVE    BUNBEN-RMSGK-MONEY  TO  SPA-GMN-RMSGK-MONEY
           MOVE    BUNBEN-KAIJO-MONEY  TO  SPA-GMN-KAIJO-MONEY
           MOVE    BUNBEN-BUNBEN-MONEY TO  SPA-GMN-BUNBEN-MONEY
           MOVE    BUNBEN-SHIN-MONEY   TO  SPA-GMN-SHIN-MONEY
           MOVE    BUNBEN-KNS-MONEY    TO  SPA-GMN-KNS-MONEY
           MOVE    BUNBEN-SYC-MONEY    TO  SPA-GMN-SYC-MONEY
           MOVE    BUNBEN-SNK-MONEY    TO  SPA-GMN-SNK-MONEY
           MOVE    BUNBEN-ETC-MONEY    TO  SPA-GMN-ETC-MONEY
           MOVE    BUNBEN-ICBFTN-MONEY TO  SPA-GMN-ICBFTN-MONEY
           MOVE    BUNBEN-NINPUFTN-MONEY
                                       TO  SPA-GMN-NINPUFTN-MONEY
           MOVE    BUNBEN-DAIRI-MONEY  TO  SPA-GMN-DAIRI-MONEY
           MOVE    BUNBEN-HKNJANUM-S   TO  SPA-NAI-HKNJANUM-S
      *
           IF    ( SPA-NAI-GNEW    =   1 )
            OR   ( FLG-SAIKEISAN   =   1 )
            OR   ( SPA-NAI-NNEW    =   1 )
               MOVE    LNKBUNBEN-KANYUKBN 
                                       TO  SPA-GMN-SYAKOK
               MOVE    LNKBUNBEN-HONKZKKBN
                                       TO  SPA-GMN-HONKZK
               MOVE    LNKBUNBEN-HKNJANUM
                                       TO  SPA-GMN-HKNJANUM
               MOVE    LNKBUNBEN-KIGO  TO  SPA-GMN-KIGO
               MOVE    LNKBUNBEN-NUM   TO  SPA-GMN-NUM
               MOVE    LNKBUNBEN-HKNJANUM-S
                                       TO  SPA-NAI-HKNJANUM-S
           END-IF
      *
           IF    ( SPA-NAI-GNEW    =   1 )
            OR   ( FLG-SAIKEISAN   =   1 )
               MOVE    LNKBUNBEN-NYU-MONEY TO  SPA-GMN-NYU-MONEY
               MOVE    LNKBUNBEN-RMSGK-MONEY
                                           TO  SPA-GMN-RMSGK-MONEY
               MOVE    LNKBUNBEN-KAIJO-MONEY
                                           TO  SPA-GMN-KAIJO-MONEY
               MOVE    LNKBUNBEN-BUNBEN-MONEY
                                           TO  SPA-GMN-BUNBEN-MONEY
               MOVE    LNKBUNBEN-SHIN-MONEY
                                           TO  SPA-GMN-SHIN-MONEY
               MOVE    LNKBUNBEN-KNS-MONEY TO  SPA-GMN-KNS-MONEY
               MOVE    LNKBUNBEN-SYC-MONEY TO  SPA-GMN-SYC-MONEY
               MOVE    LNKBUNBEN-SNK-MONEY TO  SPA-GMN-SNK-MONEY
               MOVE    LNKBUNBEN-ETC-MONEY TO  SPA-GMN-ETC-MONEY
               MOVE    LNKBUNBEN-ICBFTN-MONEY
                                           TO  SPA-GMN-ICBFTN-MONEY
               MOVE    LNKBUNBEN-NINPUFTN-MONEY
                                           TO  SPA-GMN-NINPUFTN-MONEY
               MOVE    LNKBUNBEN-DAIRI-MONEY
                                           TO  SPA-GMN-DAIRI-MONEY
      *
               IF    ( SPA-NAI-GNEW    =   1 )
                AND  ( FLG-SAIKEISAN   =   ZERO )
                   MOVE    "Ｅ外来出産"    TO  SPA-GMN-BIKOU
                                               BUNBEN-BIKOU
                   MOVE    LNKBUNBEN-NYUIN-DAY
                                           TO  BUNBEN-NYUIN-DAY
      *
               END-IF
      *
           END-IF
      *
      *
           IF    ( LNKBUNBEN-KANYUKBN  =   "2" )
               MOVE    "2"         TO  SPA-NAI-RECESAKI
           ELSE
               MOVE    "1"         TO  SPA-NAI-RECESAKI
           END-IF
      *
           IF    ( SPA-S08-MOTOPG          =   "I04")
               CONTINUE
           ELSE
      *
               IF    ( SPA-NAI-GNEW    =   1 )
                OR   ( SPA-NAI-NNEW    =   1 )
                   CONTINUE
               ELSE
                   IF    ( BUNBEN-NAME =   LNKBUNBEN-NAME )
                       CONTINUE
                   ELSE
                       MOVE    1           TO  SPA-NAI-CHGNAME
                   END-IF
               END-IF
      *
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       311-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       312-SPASET-SEC              SECTION.
      *
           MOVE    BUNBEN-RIYOUKBN     TO  WRK-CMB-CD
           PERFORM 4200-RIYOUKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-RIYOUKBN-G
           IF    ( SPA-GMN-RIYOUKBN-G  =   SPACE )
               MOVE    SPA-GMN-RIYOUKBNLST (2)
                                       TO  SPA-GMN-RIYOUKBN-G
           END-IF
      *
           IF    ( BUNBEN-SKYYM        =   ZERO )
               MOVE    SPACE           TO  SPA-GMN-SKYYM
               MOVE    SPACE           TO  SPA-NAI-SKYYM
           ELSE
               MOVE    BUNBEN-SKYYM    TO  SPA-NAI-SKYYM
               MOVE    BUNBEN-SKYYM    TO  WRK-SYMD
               PERFORM 801-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG (1:6)
                                       TO  SPA-GMN-SKYYM
           END-IF
      *
           MOVE    BUNBEN-TEISYUTUBIKBN       TO  WRK-CMB-CD
           PERFORM 4200-TEISYUTUBIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-TEISYUTUBIKBN-G
           IF    ( SPA-GMN-TEISYUTUBIKBN-G    =   SPACE )
               IF    ( SPA-GMN-RIYOUKBN       =   "1" )
                   MOVE    SPA-GMN-TEISYUTUBIKBNLST (1)
                                       TO  SPA-GMN-TEISYUTUBIKBN-G
               END-IF
           END-IF
      *
           MOVE    BUNBEN-SKYKBN       TO  WRK-CMB-CD
           PERFORM 4200-SKYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKYKBN-G
           IF    ( SPA-GMN-SKYKBN-G    =   SPACE )
               MOVE    SPA-GMN-SKYKBNLST (2)
                                       TO  SPA-GMN-SKYKBN-G
           END-IF
      *
           MOVE    BUNBEN-BUNBENKBN    TO  WRK-CMB-CD
           PERFORM 4200-BUNBENKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-BUNBENKBN-G
           IF    ( SPA-GMN-BUNBENKBN-G  =   SPACE )
               MOVE    SPA-GMN-BUNBENKBNLST (1)
                                       TO  SPA-GMN-BUNBENKBN-G
           END-IF
      *
           MOVE    BUNBEN-TEISYUTUSAKI TO SPA-GMN-TEISYUTUSAKI
      *
           MOVE    SPA-GMN-TEISYUTUSAKI TO WRK-CMB-CD
           PERFORM 4200-TEISYUTUSAKILST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-TEISYUTUSAKI-G
           IF    ( WRK-CMB-ITM             =   SPACE )
               PERFORM 3121-TEISYUTUSAKI-EDIT-SEC
               MOVE    SPA-GMN-TEISYUTUSAKI    TO  WRK-CMB-CD
               PERFORM 4200-TEISYUTUSAKILST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-TEISYUTUSAKI-G
           END-IF
      *
           MOVE    BUNBEN-BIRTHDAY     TO  SPA-NAI-BIRTHDAY-BABY
           MOVE    BUNBEN-BIRTHDAY     TO  WRK-SYMD
           PERFORM 800-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-BIRTHDAY-BABY
      *
           MOVE    BUNBEN-ZAIWEEK      TO  SPA-GMN-ZAIWEEK
      *
           MOVE    BUNBEN-SIZANKBN     TO  WRK-CMB-CD
           PERFORM 4200-SIZANKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SIZANKBN-G
           IF    ( SPA-GMN-SIZANKBN-G  =   SPACE )
               MOVE    SPA-GMN-SIZANKBNLST (2)
                                       TO  SPA-GMN-SIZANKBN-G
           END-IF
      *
           MOVE    BUNBEN-BIRTHCNT     TO  SPA-GMN-BIRTHCNT
      *
           MOVE    BUNBEN-NYUIN-DAY    TO  SPA-GMN-NYUIN-DAY
      *
           MOVE    BUNBEN-SANKAKBN TO  WRK-CMB-CD
           PERFORM 4200-SANKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-SANKAKBN-G
           IF    ( SPA-GMN-SANKAKBN-G      =   SPACE )
               IF    ( SPA-NAI-SANKANUM    =   ZERO )
                   MOVE    SPA-GMN-SANKAKBNLST (2)
                                   TO  SPA-GMN-SANKAKBN-G
               ELSE
                   MOVE    SPA-GMN-SANKAKBNLST (1)
                                   TO  SPA-GMN-SANKAKBN-G
               END-IF
           END-IF
           MOVE    BUNBEN-BIKOU        TO  SPA-GMN-BIKOU
      *
           .
       312-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    提出先編集処理
      *****************************************************************
       3121-TEISYUTUSAKI-EDIT-SEC              SECTION.
      *
           IF   (( SPA-NAI-SKYYM        NOT =   SPACE )
             AND ( SPA-NAI-SKYYM           >=   CONST-H2904 ))
             OR  ( SPA-NAI-SRYYMD (1:6)    >=   CONST-H2904 )
                   EVALUATE    SPA-GMN-SYAKOK
                   WHEN    "1"
                       MOVE    "1"     TO  SPA-GMN-TEISYUTUSAKI
                   WHEN    "2"
                       MOVE    "2"     TO  SPA-GMN-TEISYUTUSAKI
                   END-EVALUATE
           ELSE
               IF    ( SPA-GMN-BUNBENKBN       =   "1" )
                       MOVE    "2"     TO   SPA-GMN-TEISYUTUSAKI
               ELSE
      *
                   IF    ( SPA-NAI-RECESAKI    =   "1" )
                       MOVE    "1"     TO  SPA-GMN-TEISYUTUSAKI
                   END-IF
      *
                   IF    ( SPA-NAI-RECESAKI    =   "2" )
                       MOVE    "2"     TO  SPA-GMN-TEISYUTUSAKI
                   END-IF
               END-IF
           END-IF
      *
           .
       3121-TEISYUTUSAKI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    コンボボックス編集処理
      *****************************************************************
       310-COMBO-EDIT-SEC              SECTION.
      *
      *    直接支払制度利用区分
           PERFORM 3101-RIYOUKBNLST-SEC
      *
      *    提出日区分
           PERFORM 3101-TEISYUTUBIKBNLST-SEC
      *
      *    請求区分
           PERFORM 3101-SKYKBNLST-SEC
      *
      *    分娩区分
           PERFORM 3101-BUNBENKBNLST-SEC
      *
      *    提出先区分
           PERFORM 3101-TEISYUTUSAKILST-SEC
      *
      *    死産有無区分
           PERFORM 3101-SIZANKBNLST-SEC
      *
      *    産科医療補償制度区分
           PERFORM 3101-SANKAKBNLST-SEC
      *
      *    内訳明細書印刷区分
           PERFORM 3101-MEIPRTKBNLST-SEC
      *
           .
       310-COMBO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    直接支払制度利用区分コンボ初期処理
      *****************************************************************
       3101-RIYOUKBNLST-SEC            SECTION.
      *
           INITIALIZE                  SPA-GMN-RIYOUKBNLST-G
           MOVE    "0"             TO  SPA-GMN-RIYOUKBNL   (1)
           MOVE    "利用しない"    TO  SPA-GMN-RIYOUKBNMEIL(1)
           MOVE    "1"             TO  SPA-GMN-RIYOUKBNL   (2)
           MOVE    "利用する"      TO  SPA-GMN-RIYOUKBNMEIL(2)
      *
           MOVE    2               TO  SPA-GMN-RIYOUKBN-MAX
      *
           .
       3101-RIYOUKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    提出日区分コンボ初期処理
      *****************************************************************
       3101-TEISYUTUBIKBNLST-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-TEISYUTUBIKBNLST-G
      *
           IF    ( SPA-NAI-NNEW            =   ZERO  )
               MOVE    "1"             TO  SPA-GMN-TEISYUTUBIKBNL   (1)
               MOVE    "１０日請求"    TO  SPA-GMN-TEISYUTUBIKBNMEIL(1)
               MOVE    "2"             TO  SPA-GMN-TEISYUTUBIKBNL   (2)
               MOVE    "２５日請求"    TO  SPA-GMN-TEISYUTUBIKBNMEIL(2)
      *
               MOVE    2               TO  SPA-GMN-TEISYUTUBIKBN-MAX
           END-IF
      *
           .
       3101-TEISYUTUBIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    請求区分コンボ初期処理
      *****************************************************************
       3101-SKYKBNLST-SEC            SECTION.
      *
           INITIALIZE                  SPA-GMN-SKYKBNLST-G
      *
           IF    ( SPA-NAI-NNEW            =   ZERO  )
               MOVE    "0"             TO  SPA-GMN-SKYKBNL   (1)
               MOVE    "請求しない"    TO  SPA-GMN-SKYKBNMEIL(1)
               MOVE    "1"             TO  SPA-GMN-SKYKBNL   (2)
               MOVE    "未請求"        TO  SPA-GMN-SKYKBNMEIL(2)
               MOVE    "2"             TO  SPA-GMN-SKYKBNL   (3)
               MOVE    "請求済"        TO  SPA-GMN-SKYKBNMEIL(3)
      *
               MOVE    3               TO  SPA-GMN-SKYKBN-MAX
           END-IF
      *
           .
       3101-SKYKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    分娩区分区分コンボ初期処理
      *****************************************************************
       3101-BUNBENKBNLST-SEC            SECTION.
      *
           INITIALIZE                  SPA-GMN-BUNBENKBNLST-G
           MOVE    "1"             TO  SPA-GMN-BUNBENKBNL   (1)
           MOVE    "正常分娩"      TO  SPA-GMN-BUNBENKBNMEIL(1)
           MOVE    "2"             TO  SPA-GMN-BUNBENKBNL   (2)
           MOVE    "異常分娩"      TO  SPA-GMN-BUNBENKBNMEIL(2)
      *
           MOVE    2               TO  SPA-GMN-BUNBENKBN-MAX
      *
           .
       3101-BUNBENKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    提出先コンボ初期処理
      *****************************************************************
       3101-TEISYUTUSAKILST-SEC            SECTION.
      *
           INITIALIZE                  SPA-GMN-TEISYUTUSAKILST-G
           MOVE    "1"             TO  SPA-GMN-TEISYUTUSAKIL   (1)
           MOVE    "支払基金"      TO  SPA-GMN-TEISYUTUSAKIMEIL(1)
           MOVE    "2"             TO  SPA-GMN-TEISYUTUSAKIL   (2)
           MOVE    "国保連合会"    TO  SPA-GMN-TEISYUTUSAKIMEIL(2)
      *
           MOVE    2               TO  SPA-GMN-TEISYUTUSAKI-MAX
      *
           .
       3101-TEISYUTUSAKILST-EXT.
           EXIT.
      *****************************************************************
      *    死産有無区分コンボ初期処理
      *****************************************************************
       3101-SIZANKBNLST-SEC            SECTION.
      *
           INITIALIZE                  SPA-GMN-SIZANKBNLST-G
           MOVE    "1"             TO  SPA-GMN-SIZANKBNL   (1)
           MOVE    "死産"          TO  SPA-GMN-SIZANKBNMEIL(1)
           MOVE    "2"             TO  SPA-GMN-SIZANKBNL   (2)
           MOVE    "死産でない"    TO  SPA-GMN-SIZANKBNMEIL(2)
           MOVE    "3"             TO  SPA-GMN-SIZANKBNL   (3)
           MOVE    "混在"          TO  SPA-GMN-SIZANKBNMEIL(3)
      *
           MOVE    3               TO  SPA-GMN-SIZANKBN-MAX
      *
           .
       3101-SIZANKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    産科医療補償制度区分コンボ初期処理
      *****************************************************************
       3101-SANKAKBNLST-SEC            SECTION.
      *
           INITIALIZE                      SPA-GMN-SANKAKBNLST-G
           MOVE    "1"                 TO  SPA-GMN-SANKAKBNL   (1)
           MOVE    "対象分娩"          TO  SPA-GMN-SANKAKBNMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-SANKAKBNL   (2)
           MOVE    "対象分娩でない"    TO  SPA-GMN-SANKAKBNMEIL(2)
           MOVE    "3"                 TO  SPA-GMN-SANKAKBNL   (3)
           MOVE    "混在"              TO  SPA-GMN-SANKAKBNMEIL(3)
      *
           MOVE    3                   TO  SPA-GMN-SANKAKBN-MAX
      *
           .
       3101-SANKAKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    内訳明細書印刷区分コンボ初期処理
      *****************************************************************
       3101-MEIPRTKBNLST-SEC            SECTION.
      *
           INITIALIZE                      SPA-GMN-MEIPRTKBNLST-G
      *
           IF    ( SPA-NAI-NNEW            =   ZERO  )
               MOVE    "0"                 TO  SPA-GMN-MEIPRTKBNL   (1)
               MOVE    "発行しない"        TO  SPA-GMN-MEIPRTKBNMEIL(1)
               MOVE    "1"                 TO  SPA-GMN-MEIPRTKBNL   (2)
               MOVE    "発行する"          TO  SPA-GMN-MEIPRTKBNMEIL(2)
      *
               MOVE    2                   TO  SPA-GMN-MEIPRTKBN-MAX
           END-IF
      *
           .
       3101-MEIPRTKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       320-DEL-SEC                 SECTION.
      *
           IF    ( SPA-S08-NYUGAIKBN   =   "2" )
            AND  ( SPA-NAI-GNEW        =   ZERO )
               MOVE    "3002"          TO  SPA-SIDCD
           END-IF
      *
           .
       320-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納取込処理（再計算）
      *****************************************************************
       320-SAIKEISAN-SEC                 SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           IF    ( SPA-S08-NYUGAIKBN    =   "2" )
               PERFORM 800-ORCSBUNBENFTN-SEC
      *
               PERFORM 900-BUNBEN-KEY3-SEL-SEC
      *
               MOVE    1               TO  FLG-SAIKEISAN
               PERFORM 311-SPASET-SEC
               PERFORM 4102-KIHON-CHK-SEC
               MOVE    SPACE           TO  SPA-ERRCD
           END-IF
      *
           .
       320-SAIKEISAN-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       3201-DEL-SEC                SECTION.
      *
           INITIALIZE                      BUNBEN-REC
           MOVE    SPA-HOSPNUM         TO  BUNBEN-HOSPNUM
           MOVE    SPA-PTID            TO  BUNBEN-PTID
           MOVE    SPA-S08-NYUGAIKBN   TO  BUNBEN-NYUGAIKBN
           MOVE    SPA-S08-RRKNUM      TO  BUNBEN-RRKNUM
           MOVE    BUNBEN-REC          TO  MCPDATA-REC
           MOVE    "tbl_bunben"        TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    "4004"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-S08-NYUGAIKBN   =   "2" )
                   INITIALIZE                      SYUNOU-REC
                   MOVE    BUNBEN-HOSPNUM      TO  SYU-HOSPNUM
                   MOVE    BUNBEN-PTID         TO  SYU-PTID
                   MOVE    BUNBEN-NYUGAIKBN    TO  SYU-NYUGAIKBN
                   MOVE    BUNBEN-RRKNUM       TO  SYU-DENPNUM
                   MOVE    ZERO                TO  SYU-BUNBENINFFLG
                   MOVE    SPA-OPID            TO  SYU-OPID
                   MOVE    BUNBEN-UPYMD        TO  SYU-UPYMD
                   MOVE    BUNBEN-UPHMS        TO  SYU-UPHMS
                   MOVE    SYUNOU-REC          TO  MCPDATA-REC
                   MOVE    "tbl_syunou"        TO  MCP-TABLE
                   MOVE    "upd2"              TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
                   IF    ( MCP-RC          =   ZERO )
                       CONTINUE
                   ELSE
                       MOVE    "4005"          TO  SPA-ERRCD
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    "JOIN"          TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
      *
           .
       3201-DEL-EXT.
           EXIT.
      *****************************************************************
      *    保険情報変更画面遷移処理
      *****************************************************************
       320-S09-SENI-SEC              SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           INITIALIZE                  SPA-S09-AREA
      *
           MOVE    SPACE           TO  SPA-MOTOPG
      *
           MOVE    SPA-S01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-S08FREE     TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGS09"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-S08FREE
           MOVE    SPA-WORK        TO  SPA-S01KYOUTU
      *
           MOVE    "HKNJANUM"      TO  MCP-WIDGET
      *
           MOVE    "S08"           TO  SPA-MOTOPG
           MOVE    "S09"           TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "S09"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE  1             TO  FLG-END
      *
           .
       320-S09-SENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM 210-BACK
      *    WHEN    "CLICKED"       ALSO    "B02"
      *        PERFORM 310-SPASET-SEC
           WHEN    "CLICKED"       ALSO    "B04"
               PERFORM 320-DEL-SEC
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 320-SAIKEISAN-SEC
           WHEN    "CLICKED"       ALSO    "B05"
               PERFORM 320-S09-SENI-SEC
           WHEN    "CLICKED"       ALSO    "B09"
               MOVE    "9999"      TO  SPA-ERRCD
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 220-TOUROKU-SEC
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
       410-INPUT-CHK-SEC          SECTION.
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
           MOVE    SPA-GMN-BUNBENKBN-G TO  WRK-GMN-BUNBENKBN
           MOVE    SPA-GMN-SKYYM       TO  WRK-GMN-SKYYM
      *
           MOVE    S08-RIYOUKBN        TO  SPA-GMN-RIYOUKBN-G
           MOVE    S08-SKYYM           TO  SPA-GMN-SKYYM
           MOVE    S08-TEISYUTUBIKBN   TO  SPA-GMN-TEISYUTUBIKBN-G
           MOVE    S08-SKYKBN          TO  SPA-GMN-SKYKBN-G
           MOVE    S08-BUNBENKBN       TO  SPA-GMN-BUNBENKBN-G
           MOVE    S08-TEISYUTUSAKI    TO  SPA-GMN-TEISYUTUSAKI-G
           MOVE    S08-ZAIWEEK         TO  SPA-GMN-ZAIWEEK
           MOVE    S08-BIRTHDAY-BABY   TO  SPA-GMN-BIRTHDAY-BABY
           MOVE    S08-SIZANKBN        TO  SPA-GMN-SIZANKBN-G
           MOVE    S08-BIRTHCNT        TO  SPA-GMN-BIRTHCNT
           MOVE    S08-NYUIN-DAY       TO  SPA-GMN-NYUIN-DAY
           MOVE    S08-SANKAKBN        TO  SPA-GMN-SANKAKBN-G
           MOVE    S08-NYU-MONEY       TO  SPA-GMN-NYU-MONEY
           MOVE    S08-RMSGK-MONEY     TO  SPA-GMN-RMSGK-MONEY
           MOVE    S08-KAIJO-MONEY     TO  SPA-GMN-KAIJO-MONEY
           MOVE    S08-BUNBEN-MONEY    TO  SPA-GMN-BUNBEN-MONEY
           MOVE    S08-SHIN-MONEY      TO  SPA-GMN-SHIN-MONEY
           MOVE    S08-KNS-MONEY       TO  SPA-GMN-KNS-MONEY
           MOVE    S08-SYC-MONEY       TO  SPA-GMN-SYC-MONEY
           MOVE    S08-SNK-MONEY       TO  SPA-GMN-SNK-MONEY
           MOVE    S08-ETC-MONEY       TO  SPA-GMN-ETC-MONEY
           MOVE    S08-ICBFTN-MONEY    TO  SPA-GMN-ICBFTN-MONEY
           MOVE    S08-BIKOU           TO  SPA-GMN-BIKOU
           MOVE    S08-MEIPRTKBN       TO  SPA-GMN-MEIPRTKBN-G
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
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-RIYOUKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SKYYM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-TEISYUTUBIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SKYKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-BUNBENKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-TEISYUTUSAKI-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-ZAIWEEK-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-BIRTHDAY-BABY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SIZANKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-BIRTHCNT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-NYUIN-DAY-CHK-SEC
           END-IF
      *
           IF    ( SPA-NAI-SANKANUM    NOT =   ZERO )
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 4101-SANKAKBN-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-NYU-MONEY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-RMSGK-MONEY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-KAIJO-MONEY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-BUNBEN-MONEY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SHIN-MONEY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-KNS-MONEY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SYC-MONEY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SNK-MONEY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-ETC-MONEY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-ICBFTN-MONEY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-BIKOU-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-MEIPRTKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-NINPUFTN-MONEY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-DAIRI-MONEY-CHK-SEC
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    直接支払制度利用区分チェック処理
      *****************************************************************
       4101-RIYOUKBN-CHK-SEC           SECTION.
      *
           MOVE    SPA-GMN-RIYOUKBN    TO  WRK-CMB-CD
           PERFORM 4200-RIYOUKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-RIYOUKBN-G
           IF    ( SPA-GMN-RIYOUKBN-G  =   SPACE )
               MOVE    SPA-GMN-RIYOUKBNLST (2)
                                       TO  SPA-GMN-RIYOUKBN-G
           END-IF
      *
           IF    ( SPA-GMN-RIYOUKBN    =   "0" )
            OR   ( SPA-GMN-HKNJANUM    =   ZERO )
               MOVE    SPACE           TO  SPA-GMN-SKYYM
               MOVE    SPACE           TO  SPA-NAI-SKYYM
               MOVE    SPACE           TO  SPA-GMN-TEISYUTUBIKBN
               MOVE    "1"             TO  SPA-GMN-SKYKBN
           END-IF
      *
           .
       4101-RIYOUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求年月チェック処理
      *****************************************************************
       4101-SKYYM-CHK-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-NAI-SKYYM
      *
           IF    ( SPA-GMN-SKYYM       =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-SKYYM   TO  WRK-SYMD
               PERFORM 801-HIZUKE-CHK-SEC
               IF    ( WRK-HENYMDG     =   SPACE )
                   MOVE    "0001"      TO  SPA-ERRCD
                   MOVE    2           TO  SPA-GMN-CUR
               ELSE
                   MOVE    WRK-SYMD    (1:6)
                                       TO  SPA-NAI-SKYYM
                   MOVE    WRK-HENYMDG (1:6)
                                       TO  SPA-GMN-SKYYM
                   IF    ( SPA-GMN-SKYYM   NOT =   WRK-GMN-SKYYM )
                    AND  ( SPA-NAI-SKYYM      >=   CONST-H2904 )
                       PERFORM 3121-TEISYUTUSAKI-EDIT-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       4101-SKYYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    提出日区分チェック処理
      *****************************************************************
       4101-TEISYUTUBIKBN-CHK-SEC      SECTION.
      *
           MOVE    SPA-GMN-TEISYUTUBIKBN      TO  WRK-CMB-CD
           PERFORM 4200-TEISYUTUBIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-TEISYUTUBIKBN-G
           IF    ( SPA-GMN-TEISYUTUBIKBN-G    =   SPACE )
               IF    ( SPA-GMN-RIYOUKBN       =   "1" )
                   MOVE    SPA-GMN-TEISYUTUBIKBNLST (1)
                                       TO  SPA-GMN-TEISYUTUBIKBN-G
               END-IF
           END-IF
      *
           .
       4101-TEISYUTUBIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求区分チェック処理
      *****************************************************************
       4101-SKYKBN-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-SKYKBN      TO  WRK-CMB-CD
           PERFORM 4200-SKYKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SKYKBN-G
           IF    ( SPA-GMN-SKYKBN-G  =   SPACE )
               MOVE    SPA-GMN-SKYKBNLST (2)
                                       TO  SPA-GMN-SKYKBN-G
           END-IF
      *
           .
       4101-SKYKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    分娩区分チェック処理
      *****************************************************************
       4101-BUNBENKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-BUNBENKBN      TO  WRK-CMB-CD
           PERFORM 4200-BUNBENKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-BUNBENKBN-G
           IF    ( SPA-GMN-BUNBENKBN-G  =   SPACE )
               MOVE    SPA-GMN-BUNBENKBNLST (1)
                                       TO  SPA-GMN-BUNBENKBN-G
           END-IF
      *
           IF    ( SPA-GMN-BUNBENKBN   NOT =   WRK-GMN-BUNBENKBN )
               PERFORM 3121-TEISYUTUSAKI-EDIT-SEC
           END-IF
      *
           IF    ( SPA-GMN-BUNBENKBN   =   "2" )
               IF    ( SPA-GMN-TEISYUTUBIKBN   =   "2" )
                   MOVE    "0011"      TO  SPA-ERRCD
                   MOVE    23          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4101-BUNBENKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    提出先チェック処理
      *****************************************************************
       4101-TEISYUTUSAKI-CHK-SEC           SECTION.
      *
           MOVE    SPA-GMN-TEISYUTUSAKI    TO  WRK-CMB-CD
           PERFORM 4200-TEISYUTUSAKILST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-TEISYUTUSAKI-G
           IF    ( WRK-CMB-ITM             =   SPACE )
               PERFORM 3121-TEISYUTUSAKI-EDIT-SEC
               MOVE    SPA-GMN-TEISYUTUSAKI    TO  WRK-CMB-CD
               PERFORM 4200-TEISYUTUSAKILST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-TEISYUTUSAKI-G
           END-IF
      *
           .
       4101-TEISYUTUSAKI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    在胎週数チェック処理
      *****************************************************************
       4101-ZAIWEEK-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-ZAIWEEK     =   ZERO )
               CONTINUE
           ELSE
               IF    ( SPA-GMN-ZAIWEEK >=  12 )
                   CONTINUE
               ELSE
                   MOVE    "0006"      TO  SPA-ERRCD
                   MOVE    5           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4101-ZAIWEEK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    出産年月日チェック処理
      *****************************************************************
       4101-BIRTHDAY-BABY-CHK-SEC      SECTION.
      *
           MOVE    SPACE           TO  SPA-NAI-BIRTHDAY-BABY
      *
           IF    ( SPA-GMN-BIRTHDAY-BABY   =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-BIRTHDAY-BABY
                                       TO  WRK-SYMD
               PERFORM 800-HIZUKE-CHK-SEC
               IF    ( WRK-HENYMDG     =   SPACE )
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE    6           TO  SPA-GMN-CUR
               ELSE
                   MOVE    WRK-SYMD    TO  SPA-NAI-BIRTHDAY-BABY
                   MOVE    WRK-HENYMDG TO  SPA-GMN-BIRTHDAY-BABY
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   IF    ( SPA-NAI-BIRTHDAY-BABY   <   CONST-H211001 )
                       MOVE    "0008"  TO  SPA-ERRCD
                       MOVE    6       TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
       4101-BIRTHDAY-BABY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    死産有無区分チェック処理
      *****************************************************************
       4101-SIZANKBN-CHK-SEC           SECTION.
      *
           MOVE    SPA-GMN-SIZANKBN      TO  WRK-CMB-CD
           PERFORM 4200-SIZANKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SIZANKBN-G
           IF    ( SPA-GMN-SIZANKBN-G  =   SPACE )
               MOVE    SPA-GMN-SIZANKBNLST (2)
                                       TO  SPA-GMN-SIZANKBN-G
           END-IF
      *
           .
       4101-SIZANKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    出産数チェック処理
      *****************************************************************
       4101-BIRTHCNT-CHK-SEC           SECTION.
      *
      *
           .
       4101-BIRTHCNT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院日数チェック処理
      *****************************************************************
       4101-NYUIN-DAY-CHK-SEC          SECTION.
      *
      *
           .
       4101-NYUIN-DAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    産科医療補償制度区分チェック処理
      *****************************************************************
       4101-SANKAKBN-CHK-SEC           SECTION.
      *
           MOVE    SPA-GMN-SANKAKBN      TO  WRK-CMB-CD
           PERFORM 4200-SANKAKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SANKAKBN-G
           IF    ( SPA-GMN-SANKAKBN-G  =   SPACE )
               IF    ( SPA-NAI-SANKANUM    =   ZERO )
                   MOVE    SPA-GMN-SANKAKBNLST (2)
                                       TO  SPA-GMN-SANKAKBN-G
               ELSE
                   MOVE    SPA-GMN-SANKAKBNLST (1)
                                       TO  SPA-GMN-SANKAKBN-G
               END-IF
           END-IF
      *
           .
       4101-SANKAKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院料チェック処理
      *****************************************************************
       4101-NYU-MONEY-CHK-SEC          SECTION.
      *
      *
           .
       4101-NYU-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額チェック処理
      *****************************************************************
       4101-RMSGK-MONEY-CHK-SEC        SECTION.
      *
      *
           .
       4101-RMSGK-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    分娩介助料チェック処理
      *****************************************************************
       4101-KAIJO-MONEY-CHK-SEC        SECTION.
      *
      *
           .
       4101-KAIJO-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    分娩料チェック処理
      *****************************************************************
       4101-BUNBEN-MONEY-CHK-SEC       SECTION.
      *
      *
           .
       4101-BUNBEN-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    新生児管理保育料チェック処理
      *****************************************************************
       4101-SHIN-MONEY-CHK-SEC         SECTION.
      *
      *
           .
       4101-SHIN-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検査・薬剤料チェック処理
      *****************************************************************
       4101-KNS-MONEY-CHK-SEC          SECTION.
      *
      *
           .
       4101-KNS-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処置・手当料チェック処理
      *****************************************************************
       4101-SYC-MONEY-CHK-SEC          SECTION.
      *
      *
           .
       4101-SYC-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    産科医療補償制度チェック処理
      *****************************************************************
       4101-SNK-MONEY-CHK-SEC          SECTION.
      *
      *
           .
       4101-SNK-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    その他チェック処理
      *****************************************************************
       4101-ETC-MONEY-CHK-SEC          SECTION.
      *
      *
           .
       4101-ETC-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一部負担金等チェック処理
      *****************************************************************
       4101-ICBFTN-MONEY-CHK-SEC       SECTION.
      *
      *
           .
       4101-ICBFTN-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    備考チェック処理
      *****************************************************************
       4101-BIKOU-CHK-SEC              SECTION.
      *
           MOVE    SPA-GMN-BIKOU   TO  WRK-KANACHK-STR
           PERFORM 800-ENRTRY-CHK-SEC
           MOVE    WRK-KANACHK-STR TO  SPA-GMN-BIKOU
           IF    ( FLG-ENTRYCHK    NOT =   ZERO )
               MOVE    "0003"      TO  SPA-ERRCD
               MOVE    21          TO  SPA-GMN-CUR
           END-IF
      *
           .
       4101-BIKOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    内訳明細書印刷区分チェック処理
      *****************************************************************
       4101-MEIPRTKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-MEIPRTKBN   TO  WRK-CMB-CD
           PERFORM 4200-MEIPRTKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-MEIPRTKBN-G
           IF    ( SPA-GMN-MEIPRTKBN-G  =   SPACE )
               MOVE    SPA-GMN-MEIPRTKBNLST (2)
                                       TO  SPA-GMN-MEIPRTKBN-G
           END-IF
      *
           .
       4101-MEIPRTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    妊婦合計負担額チェック処理
      *****************************************************************
       4101-NINPUFTN-MONEY-CHK-SEC     SECTION.
      *
           COMPUTE WRK-NINPUFTN-MONEY
               =   SPA-GMN-NYU-MONEY
               +   SPA-GMN-RMSGK-MONEY
               +   SPA-GMN-KAIJO-MONEY
               +   SPA-GMN-BUNBEN-MONEY
               +   SPA-GMN-SHIN-MONEY
               +   SPA-GMN-KNS-MONEY
               +   SPA-GMN-SYC-MONEY
               +   SPA-GMN-SNK-MONEY
               +   SPA-GMN-ETC-MONEY
               +   SPA-GMN-ICBFTN-MONEY
      *
           IF    ( WRK-NINPUFTN-MONEY >=   10000000 )
               MOVE    "0004"          TO  SPA-ERRCD
               MOVE    11              TO  SPA-GMN-CUR
               MOVE    9999999         TO  SPA-GMN-NINPUFTN-MONEY
           ELSE
               MOVE    WRK-NINPUFTN-MONEY
                                       TO  SPA-GMN-NINPUFTN-MONEY
           END-IF
      *
           .
       4101-NINPUFTN-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    代理受取額チェック処理
      *****************************************************************
       4101-DAIRI-MONEY-CHK-SEC        SECTION.
      *
           MOVE    ZERO    TO  SPA-GMN-DAIRI-MONEY
           MOVE    ZERO    TO  WRK-BASE-MONEY
      *
           IF    ( SPA-GMN-RIYOUKBN        =   "1" )
      *
               EVALUATE    TRUE
               WHEN  ( SPA-NAI-SANKANUM    =   ZERO )
               WHEN  ( SPA-GMN-ZAIWEEK     <   22 )
               WHEN  ( SPA-GMN-SANKAKBN    =   "2" OR "3" )
                   IF    ( SPA-NAI-BIRTHDAY-BABY   NOT =   SPACE )
                       EVALUATE   TRUE
                       WHEN    ( SPA-NAI-BIRTHDAY-BABY
                                          >= CONST-H270101 )
                           MOVE    404000  TO  WRK-BASE-MONEY
                       WHEN    OTHER
                           MOVE    390000  TO  WRK-BASE-MONEY
                       END-EVALUATE
                   END-IF
               WHEN    OTHER
                   MOVE    420000      TO  WRK-BASE-MONEY
               END-EVALUATE
      *
               IF    ( SPA-GMN-SANKAKBN    =   "3" )
                   COMPUTE SPA-GMN-DAIRI-MONEY
                       =   WRK-BASE-MONEY  *   SPA-GMN-BIRTHCNT
                       +   SPA-GMN-SNK-MONEY
               ELSE
                   COMPUTE SPA-GMN-DAIRI-MONEY
                       =   WRK-BASE-MONEY  *   SPA-GMN-BIRTHCNT
               END-IF
      *
           END-IF
      *
           IF  ( SPA-GMN-DAIRI-MONEY >   ZERO )
            AND( SPA-GMN-DAIRI-MONEY >   SPA-GMN-NINPUFTN-MONEY )
               MOVE    SPA-GMN-NINPUFTN-MONEY
                                   TO  SPA-GMN-DAIRI-MONEY
           END-IF
      *
           .
       4101-DAIRI-MONEY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    直接支払制度利用区分コンボチェック処理
      *****************************************************************
       4200-RIYOUKBNLST-CHK-SEC    SECTION.
      *
           INITIALIZE                  WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-RIYOUKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-RIYOUKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-RIYOUKBNLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-RIYOUKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    提出日区分コンボチェック処理
      *****************************************************************
       4200-TEISYUTUBIKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-TEISYUTUBIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-TEISYUTUBIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-TEISYUTUBIKBNLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-TEISYUTUBIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求区分コンボチェック処理
      *****************************************************************
       4200-SKYKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SKYKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SKYKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-SKYKBNLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-SKYKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    分娩区分コンボチェック処理
      *****************************************************************
       4200-BUNBENKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-BUNBENKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-BUNBENKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-BUNBENKBNLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-BUNBENKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    提出先コンボチェック処理
      *****************************************************************
       4200-TEISYUTUSAKILST-CHK-SEC    SECTION.
      *
           INITIALIZE                  WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-TEISYUTUSAKI-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-TEISYUTUSAKIL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-TEISYUTUSAKILST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-TEISYUTUSAKILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    死産有無区分コンボチェック処理
      *****************************************************************
       4200-SIZANKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SIZANKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SIZANKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-SIZANKBNLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-SIZANKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    産科医療補償制度区分コンボチェック処理
      *****************************************************************
       4200-SANKAKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SANKAKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SANKAKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-SANKAKBNLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-SANKAKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    内訳明細書印刷区分コンボチェック処理
      *****************************************************************
       4200-MEIPRTKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-MEIPRTKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-MEIPRTKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-MEIPRTKBNLST  (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-MEIPRTKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       411-KANREN-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD                   =   SPACE )
            AND  ( SPA-GMN-SKYYM           NOT =   SPACE )
            AND  ( SPA-GMN-TEISYUTUBIKBN   NOT =   SPACE )
               IF    ( SPA-GMN-BUNBENKBN       =   "1" )
                   MOVE    SPA-NAI-SKYYM
                                       TO  WRK-SKYYM
                   IF    ( SPA-GMN-TEISYUTUBIKBN   =   "1" )
                       MOVE    "10"    TO  WRK-SKYDD
                   ELSE
                       MOVE    "25"    TO  WRK-SKYDD
                   END-IF
                   IF    ( WRK-SKYYMD
                                       <   SPA-NAI-SRYYMD )
                       MOVE    "0005"  TO  SPA-ERRCD
                       MOVE    2       TO  SPA-GMN-CUR
                   END-IF
               ELSE
                   IF    ( SPA-NAI-SKYYM
                                       <=  SPA-NAI-SRYYMD (1:6) )
                       MOVE    "0007"  TO  SPA-ERRCD
                       MOVE    2       TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-ZAIWEEK     =   ZERO )
                   MOVE    "1001"          TO  SPA-ERRCD
                   MOVE    5               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-BIRTHDAY-BABY   =   SPACE )
                   MOVE    "1002"          TO  SPA-ERRCD
                   MOVE    6               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-BIRTHCNT    =   ZERO )
                   MOVE    "1003"          TO  SPA-ERRCD
                   MOVE    8               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-NNEW        =   ZERO )
                AND  ( SPA-GMN-NYUIN-DAY   =   ZERO )
                   MOVE    "1004"          TO  SPA-ERRCD
                   MOVE    9               TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-GMN-BUNBENKBN   =   "1" )
                AND   ( SPA-GMN-KAIJO-MONEY >   ZERO )
                   MOVE    "1007"          TO  SPA-ERRCD
                   MOVE    13              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-GMN-BUNBENKBN   =   "2" )
                AND  ( SPA-GMN-BUNBEN-MONEY >   ZERO )
                   MOVE    "1008"          TO  SPA-ERRCD
                   MOVE    14              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-GMN-RIYOUKBN    =   "1" )
                   IF    ( SPA-NAI-SKYYM   NOT =   SPACE )
                       PERFORM 4111-TEISYUTUSAKI-CHK-SEC
                   END-IF
               END-IF
               
           END-IF
      *
           IF    ( SPA-GMN-BIRTHCNT    =   1 )
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( SPA-GMN-SIZANKBN    =   "3" )
                       MOVE    "1005"      TO  SPA-ERRCD
                       MOVE    7           TO  SPA-GMN-CUR
                   END-IF
               END-IF
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( SPA-GMN-SANKAKBN    =   "3" )
                       MOVE    "1006"      TO  SPA-ERRCD
                       MOVE    10          TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-GMN-SANKAKBN        =   "1" OR "3" )
                   CONTINUE
               ELSE
                   IF    ( SPA-GMN-SNK-MONEY   >   ZERO )
                       MOVE    "1011"      TO  SPA-ERRCD
                       MOVE    18          TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-GMN-SANKAKBN    =   "3" )
                AND  ( SPA-NAI-SANKANUM    =   ZERO )
                   MOVE    "0009"          TO  SPA-ERRCD
                   MOVE    10              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-SANKAKBN         =   "1" )
                   IF    ( SPA-GMN-ZAIWEEK     >=   22 )
                    AND  ( SPA-NAI-SANKANUM NOT =   ZERO )
                       CONTINUE
                   ELSE
                       IF    ( SPA-NAI-SANKANUM =   ZERO )
                           MOVE    "0009"  TO  SPA-ERRCD
                           MOVE    10      TO  SPA-GMN-CUR
                       END-IF
                       IF    ( SPA-ERRCD        =   SPACE )
                           IF    ( SPA-GMN-ZAIWEEK >=  22 )
                               CONTINUE
                           ELSE
                               MOVE    "0010"  TO  SPA-ERRCD
                               MOVE    10      TO  SPA-GMN-CUR
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-ETC-MONEY
                                       <   ZERO )
                   MOVE    "0012"          TO  SPA-ERRCD
                   MOVE    19              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *
               PERFORM 900-BUNBEN-KEY5-SEL-SEC
      *
               PERFORM UNTIL ( FLG-BUNBEN  NOT =   ZERO )
                         OR  ( SPA-ERRCD   NOT =   SPACE )
      *
                   IF    ( BUNBEN-BUNBENKBN    =   "1" OR "2" )
                       IF    ( SPA-HOSPNUM =   BUNBEN-HOSPNUM )
                        AND  ( SPA-PTID    =   BUNBEN-PTID )
                        AND  ( SPA-S08-NYUGAIKBN
                                           =   BUNBEN-NYUGAIKBN )
                        AND  ( SPA-S08-RRKNUM
                                           =   BUNBEN-RRKNUM )
                           CONTINUE
                       ELSE
                           MOVE    "0013"  TO  SPA-ERRCD
                       END-IF
                   END-IF
      *
                   PERFORM 900-BUNBEN-KEY5-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_bunben"    TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           END-IF
      *
           .
      *
       411-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理（提出先チェック）
      *****************************************************************
       4111-TEISYUTUSAKI-CHK-SEC              SECTION.
      *
           IF    ( SPA-NAI-SKYYM      >=   CONST-H2904 )
               EVALUATE    SPA-GMN-SYAKOK
               WHEN    "1"
                   IF    ( SPA-GMN-TEISYUTUSAKI    =   "2" )
                       MOVE    "1014"  TO  SPA-ERRCD
                       MOVE    24      TO  SPA-GMN-CUR
                   END-IF
               WHEN    "2"
                   IF    ( SPA-GMN-TEISYUTUSAKI    =   "1" )
                       MOVE    "1015"  TO  SPA-ERRCD
                       MOVE    24      TO  SPA-GMN-CUR
                   END-IF
               END-EVALUATE
           ELSE
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( SPA-GMN-TEISYUTUSAKI    =   SPACE )
                       MOVE    "1012"  TO  SPA-ERRCD
                       MOVE    24      TO  SPA-GMN-CUR
                   END-IF
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( SPA-GMN-BUNBENKBN   =   "1" )
                       IF    ( SPA-GMN-TEISYUTUSAKI    =   "1" )
                           MOVE    "1013"      TO  SPA-ERRCD
                           MOVE    24          TO  SPA-GMN-CUR
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( SPA-GMN-BUNBENKBN   =   "2" )
                    AND  ( SPA-NAI-RECESAKI    =   "1" )
                    AND  ( SPA-GMN-SYAKOK      =   "1" )
                       IF    ( SPA-GMN-TEISYUTUSAKI    =   "2" )
                           MOVE    "1014"  TO  SPA-ERRCD
                           MOVE    24      TO  SPA-GMN-CUR
                       END-IF
                   END-IF
      *
                   IF    ( SPA-ERRCD           =   SPACE )
                       IF    ( SPA-GMN-BUNBENKBN   =   "2" )
                        AND  ( SPA-NAI-RECESAKI    =   "2" )
                        AND  ( SPA-GMN-SYAKOK      =   "2" )
                           IF    ( SPA-GMN-TEISYUTUSAKI    =   "1" )
                               MOVE    "1015"  TO  SPA-ERRCD
                               MOVE    24      TO  SPA-GMN-CUR
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       4111-TEISYUTUSAKI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    排他制御解除処理
      *    PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    "S02"               TO  SPA-SAKIPG
           MOVE    "S08"               TO  SPA-MOTOPG
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           MOVE    SPACE               TO  LINKAREA
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       220-TOUROKU-SEC                 SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
      *
           IF    (  SPA-ERRCD          =   SPACE )
               PERFORM 411-KANREN-CHK-SEC
           END-IF
      *
           IF    (  SPA-ERRCD          =   SPACE )
               MOVE    "3001"          TO  SPA-SIDCD
           END-IF
      *
           .
      *
       220-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       2201-TOUROKU-SEC                SECTION.
      *
           PERFORM 900-BUNBEN-KEY3-SEL-SEC
           MOVE    BUNBEN-CREYMD           TO  WRK-CREYMD
      *
           INITIALIZE                          BUNBEN-REC
           MOVE    SPA-HOSPNUM             TO  BUNBEN-HOSPNUM
           MOVE    SPA-PTID                TO  BUNBEN-PTID
           MOVE    SPA-S08-NYUGAIKBN       TO  BUNBEN-NYUGAIKBN
           IF    ( SPA-NAI-SKYYM           =   SPACE )
               MOVE    "000000"            TO  BUNBEN-SKYYM
           ELSE
               MOVE    SPA-NAI-SKYYM       TO  BUNBEN-SKYYM
           END-IF
           MOVE    SPA-S08-RRKNUM          TO   BUNBEN-RRKNUM
           MOVE    SPA-GMN-PTNUM           TO   BUNBEN-PTNUM
           MOVE    SPA-GMN-TEISYUTUBIKBN   TO   BUNBEN-TEISYUTUBIKBN
           MOVE    SPA-GMN-SKYKBN          TO   BUNBEN-SKYKBN
           MOVE    SPA-GMN-RIYOUKBN        TO   BUNBEN-RIYOUKBN
           MOVE    SPA-GMN-BUNBENKBN       TO   BUNBEN-BUNBENKBN
      *
           IF    ( SPA-GMN-RIYOUKBN    =   "1" )
               MOVE    SPA-GMN-TEISYUTUSAKI TO BUNBEN-TEISYUTUSAKI
           END-IF
      *
           MOVE    SPA-GMN-SYAKOK          TO  BUNBEN-KANYUKBN
           MOVE    SPA-GMN-HONKZK          TO  BUNBEN-HONKZKKBN
           MOVE    SPA-GMN-HKNJANUM        TO  BUNBEN-HKNJANUM
           MOVE    SPA-GMN-KIGO            TO  BUNBEN-KIGO
           MOVE    SPA-GMN-NUM             TO  BUNBEN-NUM
           MOVE    SPA-GMN-KANANAME        TO  BUNBEN-NAME
           MOVE    SPA-GMN-ZAIWEEK         TO  BUNBEN-ZAIWEEK
           MOVE    SPA-NAI-BIRTHDAY-BABY   TO  BUNBEN-BIRTHDAY
           MOVE    SPA-GMN-SIZANKBN        TO  BUNBEN-SIZANKBN
           MOVE    SPA-GMN-BIRTHCNT        TO  BUNBEN-BIRTHCNT
           MOVE    SPA-GMN-NYUIN-DAY       TO  BUNBEN-NYUIN-DAY
           MOVE    SPA-GMN-SANKAKBN        TO  BUNBEN-SANKAKBN
           MOVE    SPA-GMN-NYU-MONEY       TO  BUNBEN-NYU-MONEY
           MOVE    SPA-GMN-RMSGK-MONEY     TO  BUNBEN-RMSGK-MONEY
           MOVE    SPA-GMN-KAIJO-MONEY     TO  BUNBEN-KAIJO-MONEY
           MOVE    SPA-GMN-BUNBEN-MONEY    TO  BUNBEN-BUNBEN-MONEY
           MOVE    SPA-GMN-SHIN-MONEY      TO  BUNBEN-SHIN-MONEY
           MOVE    SPA-GMN-KNS-MONEY       TO  BUNBEN-KNS-MONEY
           MOVE    SPA-GMN-SYC-MONEY       TO  BUNBEN-SYC-MONEY
           MOVE    SPA-GMN-SNK-MONEY       TO  BUNBEN-SNK-MONEY
           MOVE    SPA-GMN-ETC-MONEY       TO  BUNBEN-ETC-MONEY
           MOVE    SPA-GMN-ICBFTN-MONEY    TO  BUNBEN-ICBFTN-MONEY
           MOVE    SPA-GMN-NINPUFTN-MONEY  TO  BUNBEN-NINPUFTN-MONEY
           MOVE    SPA-GMN-DAIRI-MONEY     TO  BUNBEN-DAIRI-MONEY
           MOVE    SPA-GMN-BIKOU           TO  BUNBEN-BIKOU
           MOVE    SPA-NAI-HKNJANUM-S      TO  BUNBEN-HKNJANUM-S
           IF    ( SPA-NAI-NNEW            =   ZERO )
               MOVE    "1"                 TO  BUNBEN-UPDKBN
           ELSE
               MOVE    "3"                 TO  BUNBEN-UPDKBN
           END-IF
           MOVE    SPA-OPID                TO  BUNBEN-OPID
           PERFORM 800-MCNDATE-SEC
           MOVE    WRK-CREYMD              TO  BUNBEN-CREYMD
           MOVE    SMCNDATE-YMD            TO  BUNBEN-UPYMD
           MOVE    SMCNDATE-HMS            TO  BUNBEN-UPHMS
           MOVE    BUNBEN-REC              TO  MCPDATA-REC
           MOVE    "tbl_bunben"            TO  MCP-TABLE
           MOVE    "del3"                  TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    "4001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    BUNBEN-REC      TO  MCPDATA-REC
               MOVE    "tbl_bunben"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC          =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    "4002"      TO  SPA-ERRCD
                   MOVE    1           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-S08-NYUGAIKBN   =   "2" )
                   INITIALIZE                      SYUNOU-REC
                   MOVE    BUNBEN-HOSPNUM      TO  SYU-HOSPNUM
                   MOVE    BUNBEN-PTID         TO  SYU-PTID
                   MOVE    BUNBEN-NYUGAIKBN    TO  SYU-NYUGAIKBN
                   MOVE    BUNBEN-RRKNUM       TO  SYU-DENPNUM
                   MOVE    "1"                 TO  SYU-BUNBENINFFLG
                   MOVE    SPA-OPID            TO  SYU-OPID
                   MOVE    BUNBEN-UPYMD        TO  SYU-UPYMD
                   MOVE    BUNBEN-UPHMS        TO  SYU-UPHMS
                   MOVE    SYUNOU-REC          TO  MCPDATA-REC
                   MOVE    "tbl_syunou"        TO  MCP-TABLE
                   MOVE    "upd2"              TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
                   IF    ( MCP-RC          =   ZERO )
                       CONTINUE
                   ELSE
                       MOVE    "4003"          TO  SPA-ERRCD
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *
               IF    ( SPA-GMN-MEIPRTKBN   =   "1" )
      *
                   CALL    "ORCSONPRTSET"  USING   SPA-AREA
                   IF    ( SPA-CLIENT-PRT-FLG  =   2 )
                    OR   ( SPA-PRTCONF         =   "1" )
                       PERFORM 800-CLIENT-PRT-SEC
                   END-IF
                   PERFORM 22011-PRINT-SEC
                   PERFORM 800-CLIENT-DUMMY-PRT-SEC
      *
               END-IF
      *
               IF    ( SPA-S08-MOTOPG  =   "I04" )
                   MOVE    "JOIN"          TO  MCP-PUTTYPE
                   PERFORM 210-BACK
               ELSE
                   PERFORM 310-SPASET-SEC
               END-IF
           END-IF
      *
           .
      *
       2201-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       22011-PRINT-SEC                 SECTION.
      *
           INITIALIZE                      ONPRTDATA
                                           ORCHCN63AREA
           MOVE    SPA-PTID            TO  ORCHCN63-PTID
           MOVE    SPA-S08-NYUGAIKBN   TO  ORCHCN63-NYUGAIKBN
           MOVE    SPA-S08-RRKNUM      TO  ORCHCN63-RRKNUM
           MOVE    ORCHCN63AREA        TO  ONPRTDATA-DATA
           MOVE    "01"                TO  SPA-PRT-FLG
           CALL    "ORCHCN63"              USING
                                           SPA-AREA
                                           ONPRTDATA
           MOVE    SPACE               TO  SPA-PRT-FLG
      *
           .
      *
       22011-PRINT-EXT.
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
           IF    ( SPA-SIDCD       NOT =   SPACE )
               PERFORM 520-SIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "S08    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC                      SECTION.
      *
           MOVE   SPACE                TO  S08
           INITIALIZE                      S08
      *
           IF    ( SPA-S08-NYUGAIKBN   =   "1" )
               MOVE    "入院日"        TO  S08-LBLNYUINYMD
               MOVE    "入院日数"      TO  S08-LBLNYUINDAY
               MOVE    "退院日"        TO  S08-LBLTAIINYMD
           ELSE
               MOVE    "診療日"        TO  S08-LBLNYUINYMD
               MOVE    "診療日数"      TO  S08-LBLNYUINDAY
               MOVE    "収納取込"      TO  S08-B07-LABEL
               IF    ( SPA-NAI-GNEW    =   ZERO )
                   MOVE    "削除"      TO  S08-B04-LABEL
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-PTNUM           TO  S08-PTNUM
           MOVE    SPA-GMN-NAME            TO  S08-NAME
           MOVE    SPA-GMN-KANANAME        TO  S08-KANANAME
           MOVE    SPA-GMN-BIRTHDAY-MOM    TO  S08-BIRTHDAY-MOM
      *
           INITIALIZE                          WRK-AGE-G
           MOVE    SPA-GMN-AGE             TO  WRK-AGE-ZZ9
           MOVE    "才"                    TO  WRK-AGE-LBL
           MOVE    WRK-AGE-G               TO  S08-AGE
      *
           MOVE    SPA-GMN-NYUINYMD        TO  S08-NYUINYMD
           MOVE    SPA-GMN-TAIINYMD        TO  S08-TAIINYMD
      *
           EVALUATE    SPA-GMN-SYAKOK
           WHEN    "1"
               MOVE    "社保"              TO  S08-SYAKOK
           WHEN    "2"
               MOVE    "国保"              TO  S08-SYAKOK
           END-EVALUATE
      *
           IF    ( SPA-GMN-SYAKOK      NOT =   SPACE )
               IF    ( SPA-GMN-HONKZK      =   "1" )
                   MOVE    "本人"          TO  S08-HONKZK
               ELSE
                   MOVE    "家族"          TO  S08-HONKZK
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-HKNJANUM        TO  S08-HKNJANUM
           MOVE    SPA-GMN-KIGO            TO  S08-KIGO
           MOVE    SPA-GMN-NUM             TO  S08-NUM
      *
           MOVE    SPA-GMN-RIYOUKBN-G      TO  S08-RIYOUKBN
           MOVE    SPA-GMN-RIYOUKBN-MAX    TO  S08-RIYOUKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-RIYOUKBN-MAX )
               MOVE    SPA-GMN-RIYOUKBNLST (IDX-CMB)
                                           TO  S08-RIYOUKBN-ITEM
                                                           (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-SKYYM           TO  S08-SKYYM
      *
           MOVE    SPA-GMN-TEISYUTUBIKBN-G TO  S08-TEISYUTUBIKBN
           MOVE    SPA-GMN-TEISYUTUBIKBN-MAX
                                           TO  S08-TEISYUTUBIKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-TEISYUTUBIKBN-MAX )
               MOVE    SPA-GMN-TEISYUTUBIKBNLST (IDX-CMB)
                                           TO  S08-TEISYUTUBIKBN-ITEM
                                                           (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-SKYKBN-G        TO  S08-SKYKBN
           MOVE    SPA-GMN-SKYKBN-MAX      TO  S08-SKYKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SKYKBN-MAX )
               MOVE    SPA-GMN-SKYKBNLST (IDX-CMB)
                                           TO  S08-SKYKBN-ITEM
                                                           (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-BUNBENKBN-G     TO  S08-BUNBENKBN
           MOVE    SPA-GMN-BUNBENKBN-MAX   TO  S08-BUNBENKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-BUNBENKBN-MAX )
               MOVE    SPA-GMN-BUNBENKBNLST (IDX-CMB)
                                           TO  S08-BUNBENKBN-ITEM
                                                           (IDX-CMB)
           END-PERFORM
      *
           IF    ( SPA-GMN-RIYOUKBN    =   "0" )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-TEISYUTUSAKI-G
                                           TO  S08-TEISYUTUSAKI
               MOVE    SPA-GMN-TEISYUTUSAKI-MAX
                                           TO  S08-TEISYUTUSAKI-COUNT
               PERFORM VARYING IDX-CMB FROM    1   BY  1
                       UNTIL ( IDX-CMB >   SPA-GMN-TEISYUTUSAKI-MAX )
                   MOVE    SPA-GMN-TEISYUTUSAKILST (IDX-CMB)
                                               TO  S08-TEISYUTUSAKI-ITEM
                                                               (IDX-CMB)
               END-PERFORM
           END-IF
           MOVE    SPA-GMN-ZAIWEEK         TO  S08-ZAIWEEK
           MOVE    SPA-GMN-BIRTHDAY-BABY   TO  S08-BIRTHDAY-BABY
      *
           MOVE    SPA-GMN-SIZANKBN-G      TO  S08-SIZANKBN
           MOVE    SPA-GMN-SIZANKBN-MAX    TO  S08-SIZANKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SIZANKBN-MAX )
               MOVE    SPA-GMN-SIZANKBNLST (IDX-CMB)
                                           TO  S08-SIZANKBN-ITEM
                                                           (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-BIRTHCNT        TO  S08-BIRTHCNT
           MOVE    SPA-GMN-NYUIN-DAY       TO  S08-NYUIN-DAY
      *
           MOVE    SPA-GMN-SANKAKBN-G      TO  S08-SANKAKBN
           MOVE    SPA-GMN-SANKAKBN-MAX    TO  S08-SANKAKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SANKAKBN-MAX )
               MOVE    SPA-GMN-SANKAKBNLST (IDX-CMB)
                                           TO  S08-SANKAKBN-ITEM
                                                           (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-NYU-MONEY       TO  S08-NYU-MONEY
           MOVE    SPA-GMN-RMSGK-MONEY     TO  S08-RMSGK-MONEY
           MOVE    SPA-GMN-KAIJO-MONEY     TO  S08-KAIJO-MONEY
           MOVE    SPA-GMN-BUNBEN-MONEY    TO  S08-BUNBEN-MONEY
           MOVE    SPA-GMN-SHIN-MONEY      TO  S08-SHIN-MONEY
           MOVE    SPA-GMN-KNS-MONEY       TO  S08-KNS-MONEY
           MOVE    SPA-GMN-SYC-MONEY       TO  S08-SYC-MONEY
           MOVE    SPA-GMN-SNK-MONEY       TO  S08-SNK-MONEY
           MOVE    SPA-GMN-ETC-MONEY       TO  S08-ETC-MONEY
           MOVE    SPA-GMN-ICBFTN-MONEY    TO  S08-ICBFTN-MONEY
           MOVE    SPA-GMN-NINPUFTN-MONEY  TO  S08-NINPUFTN-MONEY
           MOVE    SPA-GMN-DAIRI-MONEY     TO  S08-DAIRI-MONEY
           MOVE    SPA-GMN-BIKOU           TO  S08-BIKOU
      *
           MOVE    SPA-GMN-MEIPRTKBN-G     TO  S08-MEIPRTKBN
           MOVE    SPA-GMN-MEIPRTKBN-MAX   TO  S08-MEIPRTKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-MEIPRTKBN-MAX )
               MOVE    SPA-GMN-MEIPRTKBNLST (IDX-CMB)
                                           TO  S08-MEIPRTKBN-ITEM
                                                           (IDX-CMB)
           END-PERFORM
      *
           MOVE    "black"                 TO  S08-LBLINFO-STYLE (1)
           MOVE    "black"                 TO  S08-LBLINFO-STYLE (2)
           MOVE    "black"                 TO  S08-LBLINFO-STYLE (3)
           MOVE    "black"                 TO  S08-LBLINFO-STYLE (4)
           MOVE    "black"                 TO  S08-LBLHEADER-STYLE
           MOVE    "black"                 TO  S08-NAME-STYLE
      *
           MOVE    ZERO                    TO  IDX-INFO
      *
           IF    ( IDX-INFO           >=   4 )
               CONTINUE
           ELSE
               IF    ( SPA-NAI-CHGNAME     =   "1" )
                   COMPUTE IDX-INFO    =   IDX-INFO    +   1
                   MOVE    "red"       TO  S08-LBLINFO-STYLE (IDX-INFO)
                   STRING  "氏名が変更されています。"
                                               DELIMITED BY SIZE
                           "登録ボタンを押してください。"
                                               DELIMITED BY SIZE
                   INTO     S08-LBLINFO (IDX-INFO)
                   END-STRING
               END-IF
           END-IF
      *
           IF    ( IDX-INFO           >=   4 )
               CONTINUE
           ELSE
               IF   (( SPA-GMN-BUNBENKBN   =   "2"   )
                   OR( SPA-S08-NYUGAIKBN   =   "2"   ))
                AND  ( SPA-GMN-BIKOU       =   SPACE )
                AND  ( SPA-GMN-RIYOUKBN    =   "1" )
                   COMPUTE IDX-INFO        =   IDX-INFO    +   1
                   MOVE    "bule"      TO  S08-LBLINFO-STYLE (IDX-INFO)
                   STRING  "補足情報等が必要な場合は"
                                               DELIMITED BY SIZE
                           "備考欄に入力してください。"
                                               DELIMITED BY SIZE
                   INTO     S08-LBLINFO (IDX-INFO)
                   END-STRING
               END-IF
           END-IF
      *
           IF    ( IDX-INFO           >=   4 )
               CONTINUE
           ELSE
               IF  ( SPA-S08-NYUGAIKBN       =   "2" )
                AND( SPA-NAI-GHKNNUM(1:2)    =   "98" )
                AND( SPA-GMN-BUNBENKBN       =   "2" )
                AND( SPA-GMN-RIYOUKBN        =   "1" )
                   COMPUTE IDX-INFO    =   IDX-INFO    +   1
                   STRING  "自費保険に異常分娩を紐付けする場合、"
                                           DELIMITED BY SIZE
                           "患者登録または診療行為業務で特記事項を"
                                           DELIMITED BY SIZE
                           "登録してください。"
                                           DELIMITED BY SIZE
                   INTO     S08-LBLINFO (IDX-INFO)
                   END-STRING
               END-IF
           END-IF
      *
           IF    ( IDX-INFO           >=   4 )
               CONTINUE
           ELSE
               IF    ( SPA-GMN-SANKAKBN        =   "1" OR "3" )
                AND  ( SPA-NAI-NNEW            =   ZERO )
                   IF    ( SPA-GMN-SNK-MONEY   =   ZERO )
                       COMPUTE IDX-INFO    =   IDX-INFO    +   1
                       MOVE    "blue"  TO  S08-LBLINFO-STYLE (IDX-INFO)
                       STRING  "産科医療補償制度の金額が未入力です。"
                                               DELIMITED BY SIZE
                       INTO     S08-LBLINFO (IDX-INFO)
                       END-STRING
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-HKNJANUM        =   SPACE )
            AND  ( SPA-GMN-RIYOUKBN        =   "1" )
               MOVE    "red"               TO  S08-LBLHEADER-STYLE
               MOVE    "保険者番号未入力"  TO  S08-LBLHEADER
           END-IF
      *
           IF    ( SPA-NAI-LOCK        NOT =   ZERO )
      *
               MOVE    "red"                   TO  S08-NAME-STYLE
               MOVE    "red"                   TO  S08-LBLHEADER-STYLE
               MOVE    "他端末で使用中です。"  TO  S08-LBLHEADER
               MOVE    SPA-NAI-SLOCK-MSG       TO  S08-LBLHEADER(21:)
      *
               MOVE    "black"                 TO  S08-LBLINFO-STYLE (1)
               MOVE    "black"                 TO  S08-LBLINFO-STYLE (2)
               MOVE    "black"                 TO  S08-LBLINFO-STYLE (3)
               MOVE    "black"                 TO  S08-LBLINFO-STYLE (4)
               MOVE    SPACE                   TO  S08-LBLINFO (1)
               MOVE    SPACE                   TO  S08-LBLINFO (2)
               MOVE    SPACE                   TO  S08-LBLINFO (3)
               MOVE    SPACE                   TO  S08-LBLINFO (4)
      *
           END-IF
      *
           MOVE    "T"                     TO  WRK-VISIBLE
      *
           MOVE    "入院料"                TO  WRK-LBLNYU-MONEY
           MOVE    "室料差額"              TO  WRK-LBLRMSGK-MONEY
           MOVE    "分娩介助料"            TO  WRK-LBLKAIJO-MONEY
           MOVE    "分娩料"                TO  WRK-LBLBUNBEN-MONEY
           MOVE    "新生児管理保育料"      TO  WRK-LBLSHIN-MONEY
           MOVE    "検査・薬剤料"          TO  WRK-LBLKNS-MONEY
           MOVE    "処置・手当料"          TO  WRK-LBLSYC-MONEY
           MOVE    "産科医療補償制度"      TO  WRK-LBLSNK-MONEY
           MOVE    "その他"                TO  WRK-LBLETC-MONEY
           MOVE    "一部負担金等"          TO  WRK-LBLICBFTN-MONEY
           MOVE    "妊婦合計負担額"        TO  WRK-LBLNINPUFTN-MONEY
           MOVE    "代理受取額"            TO  WRK-LBLDAIRI-MONEY
           MOVE    "明細書"                TO  WRK-LBLMEIPRTKBN
      *
           MOVE    WRK-LBLNYU-MONEY        TO  S08-LBLNYU-MONEY
           MOVE    WRK-LBLRMSGK-MONEY      TO  S08-LBLRMSGK-MONEY
           MOVE    WRK-LBLKAIJO-MONEY      TO  S08-LBLKAIJO-MONEY
           MOVE    WRK-LBLBUNBEN-MONEY     TO  S08-LBLBUNBEN-MONEY
           MOVE    WRK-LBLSHIN-MONEY       TO  S08-LBLSHIN-MONEY
           MOVE    WRK-LBLKNS-MONEY        TO  S08-LBLKNS-MONEY
           MOVE    WRK-LBLSYC-MONEY        TO  S08-LBLSYC-MONEY
           MOVE    WRK-LBLSNK-MONEY        TO  S08-LBLSNK-MONEY
           MOVE    WRK-LBLETC-MONEY        TO  S08-LBLETC-MONEY
           MOVE    WRK-LBLICBFTN-MONEY     TO  S08-LBLICBFTN-MONEY
           MOVE    WRK-LBLNINPUFTN-MONEY   TO  S08-LBLNINPUFTN-MONEY
           MOVE    WRK-LBLDAIRI-MONEY      TO  S08-LBLDAIRI-MONEY
           MOVE    WRK-LBLMEIPRTKBN        TO  S08-LBLMEIPRTKBN
      *
           MOVE    WRK-VISIBLE         TO  S08-NYU-MONEY-VISIBLE
                                           S08-RMSGK-MONEY-VISIBLE
                                           S08-KAIJO-MONEY-VISIBLE
                                           S08-BUNBEN-MONEY-VISIBLE
                                           S08-SHIN-MONEY-VISIBLE
                                           S08-KNS-MONEY-VISIBLE
                                           S08-SYC-MONEY-VISIBLE
                                           S08-SNK-MONEY-VISIBLE
                                           S08-ETC-MONEY-VISIBLE
                                           S08-ICBFTN-MONEY-VISIBLE
                                           S08-NINPUFTN-MONEY-VISIBLE
                                           S08-DAIRI-MONEY-VISIBLE
                                           S08-MEIPRTKBN-VISIBLE
                                           S08-MEIPRTKBN-ITEM-VISIBLE
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
           MOVE    WIDGET-NORMAL       TO  S08-SKYYM-STATE
                                           S08-TEISYUTUBIKBN-STATE
                                           S08-TEISYUTUBIKBN-ITEM-STATE
                                           S08-SKYKBN-STATE
                                           S08-SKYKBN-ITEM-STATE
                                           S08-TEISYUTUSAKI-STATE
                                           S08-TEISYUTUSAKI-ITEM-STATE
                                           S08-SANKAKBN-STATE
                                           S08-SANKAKBN-ITEM-STATE
                                           S08-B05-STATE
                                           S08-B12-STATE
      *
           MOVE    WIDGET-NORMAL       TO  WRK-STATE
      *
           IF    ( SPA-NAI-LOCK        NOT =   ZERO )
               MOVE    WIDGET-INSENSITIVE
                                       TO  S08-B05-STATE
                                           S08-B12-STATE
           END-IF
      *
           IF    ( SPA-GMN-RIYOUKBN    =   "0" )
            OR   ( SPA-GMN-HKNJANUM    =   SPACE )
            OR   ( SPA-NAI-NNEW    NOT =   ZERO )
               MOVE    WIDGET-INSENSITIVE
                                       TO  S08-SKYYM-STATE
                                           S08-TEISYUTUBIKBN-STATE
                                           S08-TEISYUTUBIKBN-ITEM-STATE
                                           S08-SKYKBN-STATE
                                           S08-SKYKBN-ITEM-STATE
           END-IF
      *
           IF    ( SPA-GMN-RIYOUKBN    =   "0" )
               MOVE    WIDGET-INSENSITIVE
                                       TO  S08-TEISYUTUSAKI-STATE
                                           S08-TEISYUTUSAKI-ITEM-STATE
           END-IF
      *
           IF    ( SPA-NAI-SANKANUM    =   ZERO )
               MOVE    WIDGET-INSENSITIVE
                                       TO  S08-SANKAKBN-STATE
                                           S08-SANKAKBN-ITEM-STATE
           END-IF
      *
           IF    ( SPA-NAI-NNEW            NOT =   ZERO )
               MOVE    WIDGET-INSENSITIVE  TO  WRK-STATE
           END-IF
      *
           MOVE    WRK-STATE           TO  S08-NYUIN-DAY-STATE
                                           S08-NYU-MONEY-STATE
                                           S08-RMSGK-MONEY-STATE
                                           S08-KAIJO-MONEY-STATE
                                           S08-BUNBEN-MONEY-STATE
                                           S08-SHIN-MONEY-STATE
                                           S08-KNS-MONEY-STATE
                                           S08-SYC-MONEY-STATE
                                           S08-SNK-MONEY-STATE
                                           S08-ETC-MONEY-STATE
                                           S08-ICBFTN-MONEY-STATE
                                           S08-NINPUFTN-MONEY-STATE
                                           S08-DAIRI-MONEY-STATE
                                           S08-MEIPRTKBN-STATE
                                           S08-MEIPRTKBN-ITEM-STATE
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC              SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-CUR         =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-SKIP-CUR-SEC
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    001
               MOVE    "RIYOUKBN"      TO  MCP-WIDGET
           WHEN    002
               MOVE    "SKYYM"         TO  MCP-WIDGET
           WHEN    003
               MOVE    "SKYKBN"        TO  MCP-WIDGET
           WHEN    004
               MOVE    "BUNBENKBN"     TO  MCP-WIDGET
           WHEN    005
               MOVE    "ZAIWEEK"       TO  MCP-WIDGET
           WHEN    006
               MOVE    "BIRTHDAY_BABY" TO  MCP-WIDGET
           WHEN    007
               MOVE    "SIZANKBN"      TO  MCP-WIDGET
           WHEN    008
               MOVE    "BIRTHCNT"      TO  MCP-WIDGET
           WHEN    009
               MOVE    "NYUIN_DAY"     TO  MCP-WIDGET
           WHEN    010
               MOVE    "SANKAKBN"      TO  MCP-WIDGET
           WHEN    011
               MOVE    "NYU_MONEY"     TO  MCP-WIDGET
           WHEN    012
               MOVE    "RMSGK_MONEY"   TO  MCP-WIDGET
           WHEN    013
               MOVE    "KAIJO_MONEY"   TO  MCP-WIDGET
           WHEN    014
               MOVE    "BUNBEN_MONEY"  TO  MCP-WIDGET
           WHEN    015
               MOVE    "SHIN_MONEY"    TO  MCP-WIDGET
           WHEN    016
               MOVE    "KNS_MONEY"     TO  MCP-WIDGET
           WHEN    017
               MOVE    "SYC_MONEY"     TO  MCP-WIDGET
           WHEN    018
               MOVE    "SNK_MONEY"     TO  MCP-WIDGET
           WHEN    019
               MOVE    "ETC_MONEY"     TO  MCP-WIDGET
           WHEN    020
               MOVE    "ICBFTN_MONEY"  TO  MCP-WIDGET
           WHEN    021
               MOVE    "BIKOU"         TO  MCP-WIDGET
           WHEN    022
               MOVE    "MEIPRTKBN"     TO  MCP-WIDGET
           WHEN    023
               MOVE    "TEISYUTUBIKBN" TO  MCP-WIDGET
           WHEN    024
               MOVE    "TEISYUTUSAKI"  TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
      *
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "RIYOUKBN"
               MOVE    002             TO  SPA-GMN-CUR
           WHEN    "SKYYM"
               MOVE    023             TO  SPA-GMN-CUR
           WHEN    "TEISYUTUBIKBN"
               MOVE    003             TO  SPA-GMN-CUR
           WHEN    "SKYKBN"
               MOVE    004             TO  SPA-GMN-CUR
           WHEN    "BUNBENKBN"
               MOVE    024             TO  SPA-GMN-CUR
           WHEN    "TEISYUTUSAKI"
               MOVE    005             TO  SPA-GMN-CUR
           WHEN    "ZAIWEEK"
               MOVE    006             TO  SPA-GMN-CUR
           WHEN    "BIRTHDAY_BABY"
               MOVE    007             TO  SPA-GMN-CUR
           WHEN    "SIZANKBN"
               MOVE    008             TO  SPA-GMN-CUR
           WHEN    "BIRTHCNT"
               MOVE    009             TO  SPA-GMN-CUR
           WHEN    "NYUIN_DAY"
               MOVE    010             TO  SPA-GMN-CUR
           WHEN    "SANKAKBN"
               MOVE    011             TO  SPA-GMN-CUR
           WHEN    "NYU_MONEY"
               MOVE    012             TO  SPA-GMN-CUR
           WHEN    "RMSGK_MONEY"
               MOVE    013             TO  SPA-GMN-CUR
           WHEN    "KAIJO_MONEY"
               MOVE    014             TO  SPA-GMN-CUR
           WHEN    "BUNBEN_MONEY"
               MOVE    015             TO  SPA-GMN-CUR
           WHEN    "SHIN_MONEY"
               MOVE    016             TO  SPA-GMN-CUR
           WHEN    "KNS_MONEY"
               MOVE    017             TO  SPA-GMN-CUR
           WHEN    "SYC_MONEY"
               MOVE    018             TO  SPA-GMN-CUR
           WHEN    "SNK_MONEY"
               MOVE    019             TO  SPA-GMN-CUR
           WHEN    "ETC_MONEY"
               MOVE    020             TO  SPA-GMN-CUR
           WHEN    "ICBFTN_MONEY"
               MOVE    021             TO  SPA-GMN-CUR
           WHEN    "BIKOU"
               MOVE    022             TO  SPA-GMN-CUR
           WHEN    "MEIPRTKBN"
               MOVE    212             TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5012-SKIP-CUR-SEC               SECTION.
      *
           IF    ( SPA-GMN-CUR     =   2 )
               IF    ( S08-SKYYM-STATE =   WIDGET-INSENSITIVE )
                   MOVE    23      TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   23 )
               IF    ( S08-TEISYUTUBIKBN-STATE
                                   =   WIDGET-INSENSITIVE )
                   MOVE    3       TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   3 )
               IF    ( S08-SKYKBN-STATE =   WIDGET-INSENSITIVE )
                   MOVE    4       TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   24 )
               IF    ( S08-TEISYUTUSAKI-STATE
                                   =   WIDGET-INSENSITIVE )
                   MOVE    5       TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   9 )
               IF    ( S08-NYUIN-DAY-STATE
                                        =   WIDGET-INSENSITIVE )
                   MOVE    10      TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   10 )
               IF    ( S08-SANKAKBN-STATE
                                        =   WIDGET-INSENSITIVE )
                   MOVE    11      TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   11 )
               IF    ( S08-NYU-MONEY-STATE
                                        =   WIDGET-INSENSITIVE )
                   MOVE    12      TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   12 )
               IF    ( S08-RMSGK-MONEY-STATE
                                        =   WIDGET-INSENSITIVE )
                   MOVE    13      TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   13 )
               IF    ( S08-KAIJO-MONEY-STATE
                                        =   WIDGET-INSENSITIVE )
                   MOVE    14      TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   14 )
               IF    ( S08-BUNBEN-MONEY-STATE
                                        =   WIDGET-INSENSITIVE )
                   MOVE    15      TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   15 )
               IF    ( S08-SHIN-MONEY-STATE
                                        =   WIDGET-INSENSITIVE )
                   MOVE    16      TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   16 )
               IF    ( S08-KNS-MONEY-STATE
                                        =   WIDGET-INSENSITIVE )
                   MOVE    17      TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   17 )
               IF    ( S08-SYC-MONEY-STATE
                                        =   WIDGET-INSENSITIVE )
                   MOVE    18      TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   18 )
               IF    ( S08-SNK-MONEY-STATE
                                        =   WIDGET-INSENSITIVE )
                   MOVE    19      TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   19 )
               IF    ( S08-ETC-MONEY-STATE
                                        =   WIDGET-INSENSITIVE )
                   MOVE    20      TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   20 )
               IF    ( S08-ICBFTN-MONEY-STATE
                                        =   WIDGET-INSENSITIVE )
                   MOVE    21      TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF    ( SPA-GMN-CUR     =   23 )
               IF    ( S08-MEIPRTKBN-STATE
                                        =   WIDGET-INSENSITIVE )
                   MOVE    212     TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       5012-SKIP-CUR-CUR-EXT.
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
               MOVE    "請求年月入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "出産年月日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "備考欄は全て全角で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "妊婦負担額の上限をオーバーしています"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               IF    ( SPA-S08-NYUGAIKBN   =    "1" )
                   MOVE    "請求日＜退院日となっています"
                                       TO  SPA-ERRMSG
               ELSE
                   MOVE    "請求日＜診療日となっています"
                                       TO  SPA-ERRMSG
               END-IF
           WHEN    "0006"
               MOVE    "在胎週数が１２週未満です"
                                       TO  SPA-ERRMSG
           WHEN    "0007"
               IF    ( SPA-S08-NYUGAIKBN   =    "1" )
                   MOVE    "請求年月≦退院月となっています"
                                       TO  SPA-ERRMSG
               ELSE
                   MOVE    "請求年月≦診療月となっています"
                                       TO  SPA-ERRMSG
               END-IF
           WHEN    "0008"
               MOVE    "出産年月日が直接支払制度開始前の日付です"
                                       TO  SPA-ERRMSG
           WHEN    "0009"
               MOVE   "分娩機関管理番号が未設定です"
                                       TO  SPA-ERRMSG
           WHEN    "0010"
               MOVE   "在胎週数が２２週未満です"
                                       TO  SPA-ERRMSG
           WHEN    "0011"
               MOVE   "異常分娩は２５日請求できません"
                                       TO  SPA-ERRMSG
           WHEN    "0012"
               MOVE   "その他の金額がマイナスです"
                                       TO  SPA-ERRMSG
           WHEN    "0013"
               MOVE   "出産年月日の同じデータが既に登録されています"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "在胎週数を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE    "出産年月日を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1003"
               MOVE    "出産数を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1004"
               IF    ( SPA-S08-NYUGAIKBN   =    "1" )
                   MOVE    "入院日数を入力してください"
                                       TO  SPA-ERRMSG
               ELSE
                   MOVE    "診療日数を入力してください"
                                       TO  SPA-ERRMSG
               END-IF
           WHEN    "1005"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "出産数が１人で死産有無区分に混在が"
                                           DELIMITED BY SIZE
                       "設定されています"  DELIMITED BY SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1006"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "出産数が１人で産科医療補償制度区分に混在が"
                                           DELIMITED BY SIZE
                       "設定されています"  DELIMITED BY SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1007"
               MOVE    "正常分娩で分娩介助料が入力されています"
                                       TO  SPA-ERRMSG
           WHEN    "1008"
               MOVE    "異常分娩で分娩料が入力されています"
                                       TO  SPA-ERRMSG
           WHEN    "1011"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "産科医療補償制度の金額が"
                                           DELIMITED BY SIZE
                       "入力されています"  DELIMITED BY SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1012"
               MOVE    "提出先を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1013"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "正常分娩の場合、"  DELIMITED   BY  SIZE
                       "提出先に国保連合会を選択してください"
                                           DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1014"
               MOVE    "提出先に支払基金を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "1015"
               MOVE    "提出先に国保連合会を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "4001"  THRU    "4005"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           WHEN    "9999"
               MOVE    "他端末で使用中です。"
                                               TO  SPA-ERRMSG
               MOVE    SPA-NAI-SLOCK-MSG       TO  SPA-ERRMSG(21:)
           END-EVALUATE
      *
           MOVE    SPACE               TO  SERR
           MOVE    SPA-ERRCD           TO  SERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  SERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "S08"               TO  SPA-MOTOPG
           MOVE    "SERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "SERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-SIDSET-SEC                  SECTION.
      *
           EVALUATE    SPA-SIDCD
           WHEN    "3001"
               MOVE    "登録します。よろしいですか"
                                       TO  WRK-SIDMSG
           WHEN    "3002"
               MOVE    "削除します。よろしいですか"
                                       TO  WRK-SIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SID1
           INITIALIZE                      SID1
           MOVE    SPA-SIDCD           TO  SID1-ID1CODE
           MOVE    WRK-SIDMSG          TO  SID1-ID1MSG
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "S08"               TO  SPA-MOTOPG
           MOVE    "SID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "SID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-SIDSET-EXT.
           EXIT.
      *****************************************************************
      *    クライアント印刷前　処理
      *****************************************************************
       800-CLIENT-PRT-SEC         SECTION.
      *
      *    クライアント印刷制御情報取得サブ
           INITIALIZE                          ORCSPRTCTRLAREA
           CALL    "ORCSPRTCTRL"       USING   SPA-AREA
                                               ORCSPRTCTRLAREA
                                               MCPAREA
      *
           .
       800-CLIENT-PRT-EXT.
           EXIT.
      *****************************************************************
      *    クライアントダミー印刷　処理
      *****************************************************************
       800-CLIENT-DUMMY-PRT-SEC        SECTION.
      *
           IF      (SPA-CLIENT-PRT-FLG =   1  )
      *        印刷ダミー処理
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
      *
           .
       800-CLIENT-DUMMY-PRT-EXT.
           EXIT.
      *****************************************************************
      *    分娩負担金サブ
      *****************************************************************
       800-ORCSBUNBENFTN-SEC              SECTION.
      *
           INITIALIZE                     LNKBUNBEN-REC
           MOVE    SPA-HOSPNUM         TO LNKBUNBEN-HOSPNUM
           MOVE    SPA-PTID            TO LNKBUNBEN-PTID
           MOVE    SPA-S08-NYUGAIKBN   TO LNKBUNBEN-NYUGAIKBN
           MOVE    SPA-S08-RRKNUM      TO LNKBUNBEN-RRKNUM
           CALL    "ORCSBUNBENFTN"     USING
                                       LNKBUNBEN-REC
                                       SPA-AREA
      *
           .
       800-ORCSBUNBENFTN-EXT.
           EXIT.


      *****************************************************************
      *    全角半角混在チェック
      *****************************************************************
       800-ENRTRY-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  (   WRK-KANACHK-STR     =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    WRK-KANACHK-STR TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING
                                       ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-KANACHK-STR
               IF      ( KANACHK-RC    NOT =   ZERO )
                   MOVE    1       TO  FLG-ENTRYCHK
               END-IF
               IF      ( KANACHK-SYUBETU   NOT =   2 )
                   MOVE    1       TO  FLG-ENTRYCHK
               END-IF
           END-IF
      *
           .
       800-ENRTRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       800-HIZUKE-CHK-SEC             SECTION.
      *
           MOVE        SPACE       TO  WRK-HENYMDG
      *
           INITIALIZE                  ORCSGDAYAREA
           MOVE    WRK-SYMD        TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF  ( SGDAY-RC          =   ZERO )
               MOVE    SGDAY-OTDAY TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY  TO  WRK-SYMD
           END-IF
      *
           .
       800-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       801-HIZUKE-CHK-SEC             SECTION.
      *
           MOVE        SPACE       TO  WRK-HENYMDG
      *
           INITIALIZE                  ORCSGDAYAREA
           MOVE    "1"             TO  SGDAY-INTYPE
           MOVE    WRK-SYMD        TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF  ( SGDAY-RC          =   ZERO )
               MOVE    SGDAY-OTDAY TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY  TO  WRK-SYMD
           END-IF
      *
           .
       801-HIZUKE-CHK-EXT.
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
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-PTID            TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF    ( SLOCK-RETURN    NOT =   ZERO )
               MOVE    "9999"          TO  SPA-ERRCD
           END-IF
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    排他制御解除処理
           MOVE    2                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索処理(KEY)
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY4-SEL-SEC     SECTION.
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    SPA-S08-RRKNUM      TO  PTNYUINRRK-RRKNUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
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
      *    収納検索処理
      *****************************************************************
       900-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-S08-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    SPA-S08-RRKNUM      TO  SYU-DENPNUM
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
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
      *****************************************************************
      *    分娩管理検索処理
      *****************************************************************
       900-BUNBEN-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BUNBEN
           INITIALIZE                  BUNBEN-REC
      *
           MOVE    SPA-HOSPNUM     TO  BUNBEN-HOSPNUM
           MOVE    SPA-PTID        TO  BUNBEN-PTID
           MOVE    SPA-S08-NYUGAIKBN
                                   TO  BUNBEN-NYUGAIKBN
           MOVE    SPA-S08-RRKNUM  TO  BUNBEN-RRKNUM
      *
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BUNBEN-REC
           ELSE
               INITIALIZE              BUNBEN-REC
               MOVE    1           TO  FLG-BUNBEN
           END-IF
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-BUNBEN-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理検索処理
      *****************************************************************
       900-BUNBEN-KEY5-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BUNBEN
           INITIALIZE                  BUNBEN-REC
      *
           MOVE    SPA-HOSPNUM     TO  BUNBEN-HOSPNUM
           MOVE    SPA-PTID        TO  BUNBEN-PTID
           MOVE    SPA-NAI-BIRTHDAY-BABY
                                   TO  BUNBEN-BIRTHDAY
      *
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BUNBEN-REC
           ELSE
               INITIALIZE              BUNBEN-REC
               MOVE    1           TO  FLG-BUNBEN
           END-IF
      *
           .
      *
       900-BUNBEN-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理検索処理
      *****************************************************************
       900-BUNBEN-KEY5-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BUNBEN
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BUNBEN-REC
           ELSE
               INITIALIZE              BUNBEN-REC
               MOVE    1           TO  FLG-BUNBEN
           END-IF
      *
           .
      *
       900-BUNBEN-KEY5-FET-EXT.
           EXIT.
      *****************************************************************
      *    旧姓履歴検索処理
      *****************************************************************
       900-KYUSEIRRK-KEY3-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-KYUSEIRRK
      *
           INITIALIZE                      KYUSEIRRK-REC
           MOVE    SPA-HOSPNUM         TO  KYUSEI-HOSPNUM
           MOVE    SPA-PTID            TO  KYUSEI-PTID
           MOVE    SPA-NAI-SRYYMD      TO  KYUSEI-CHGYMD
           MOVE    KYUSEIRRK-REC       TO  MCPDATA-REC
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  KYUSEIRRK-REC
           ELSE
               INITIALIZE                  KYUSEIRRK-REC
               MOVE    1               TO  FLG-KYUSEIRRK
           END-IF
      *
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-KYUSEIRRK-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    旧姓履歴FETCH処理
      *****************************************************************
       900-KYUSEIRRK-KEY3-FET-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-KYUSEIRRK
      *
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  KYUSEIRRK-REC
           ELSE
               INITIALIZE                  KYUSEIRRK-REC
               MOVE    1               TO  FLG-KYUSEIRRK
           END-IF
      *
           .
       900-KYUSEIRRK-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC  SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO SYS-HOSPNUM
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
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
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
       910-DBSELECT-EXT.
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
       910-DBCLOSECURSOR-SEC                 SECTION.
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
      *    DBUPDATE処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    DBINSERT処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
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
      *    DBDELETE処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
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
