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
       PROGRAM-ID.                 ORCGD04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェック
      *  コンポーネント名  : チェック内容確認画面
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07.02.22    太田          新規作成
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
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    データチェック共通パラメタ
           COPY    "D01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "D04SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-ETENSU3                 PIC 9(01).
           03  FLG-ETENSU5                 PIC 9(01).
           03  FLG-CHKMST                  PIC 9(01).
           03  FLG-CHKSND                  PIC 9(01).
           03  FLG-CHKTRD                  PIC 9(01).
           03  FLG-CHK004                  PIC 9(01).
           03  FLG-CHK005                  PIC 9(01).
           03  FLG-MATERK                  PIC 9(01).
           03  FLG-PTNUM                   PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-PTBYOMEI                PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-SRYACT                  PIC 9(01).
           03  FLG-SRYACCT                 PIC 9(01).
           03  FLG-NSRYSRH                 PIC 9(01).
           03  FLG-NRRKSRH                 PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-LOOPEND                 PIC 9(01).
           03  FLG-HIZUKE-ERR              PIC 9(01).
           03  FLG-ERRLST-ROW              PIC 9(01).
           03  FLG-BYOLST-ROW              PIC 9(01).
           03  FLG-CHKLST-ROW              PIC 9(01).
           03  FLG-CHKREIGAI               PIC 9(01).
           03  FLG-INPUTCD                 PIC 9(01).
           03  FLG-TEKIOUBYOMEI            PIC 9(01).
           03  FLG-SKIP                    PIC 9(01).
           03  FLG-ERROR                   PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-2001                    PIC 9(05).
           03  CNT-RENNUM                  PIC 9(05).
           03  CNT-CHK004                  PIC 9(05).
           03  CNT-OTHER                   PIC 9(05).
           03  CNT-SKIP                    PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDXA                        PIC 9(05).
           03  IDXD                        PIC 9(05).
           03  IDXE                        PIC 9(05).
           03  IDXI                        PIC 9(05).
           03  IDXK                        PIC 9(05).
           03  IDXL                        PIC 9(05).
           03  IDXM                        PIC 9(05).
           03  IDXN                        PIC 9(05).
           03  IDXH                        PIC 9(05).
           03  IDXR                        PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
      *
      *    作業領域
       01  WRK-AREA.
           03  WRK-CMB-CD                  PIC X(10).
           03  WRK-CMB-ITM-G.
               07  WRK-CMB-ITM             PIC X(200).
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-YMD                     PIC X(10).
           03  WRK-SYMD                    PIC X(08).
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-EDTYMD1                 PIC X(10).
           03  WRK-EDTYMD3                 PIC X(22).
           03  WRK-HENYMDG                 PIC X(09).
           03  WRK-DIDMSG                  PIC X(200).
           03  WRK-PTID                    PIC 9(10).
           03  WRK-PTNUM                   PIC X(20).
           03  WRK-DELKBNCD                PIC X(08).
           03  WRK-KBNCD-G.
               05  WRK-KBNCD-KBN           PIC X(01).
               05  WRK-KBNCD-SEQ           PIC 9(07).
           03  WRK-ZZZZ9                   PIC ZZZZ9.
           03  WRK-ZZZ9                    PIC  ZZZ9.
           03  WRK-ERRLST-SEL              PIC 9(05).
           03  WRK-BYOLST-SEL              PIC 9(05).
           03  WRK-CHKLST-SEL              PIC 9(05).
           03  WRK-MOTOPG                  PIC X(05).
           03  WRK-SELKBNCD                PIC X(08).
           03  WRK-NAME                    PIC X(100).
           03  WRK-SRYCD                   PIC X(09).
           03  WRK-KJNYMD                  PIC X(08).
           03  WRK-TENKIYMD                PIC X(08).
           03  WRK-OFFSET                  PIC 9(08).
           03  WRK-BYOMEI                  PIC X(100).
           03  WRK-HOSOKU                  PIC X(40).
           03  WRK-CHKKBN                  PIC X(01).
           03  WRK-EDIT-COMPKBN            PIC X(01).
           03  WRK-EDIT-COMPKBN-MEI        PIC X(04).
           03  WRK-EDIT-BYOMEI-COMPKBN     PIC X(04).
           03  WRK-IKOCD                   PIC X(07).
           03  WRK-TBYOMEICD-G.
               05  WRK-TBYOMEICD           PIC X(07)
                                           OCCURS  21.
           03  WRK-LTRIM                   PIC X(500).
           03  WRK-LTRIM-Z                 REDEFINES WRK-LTRIM.
               05  WRK-LTRIM-ZX99          PIC Z(9)9.
               05  WRK-LTRIM-FL            PIC X(490).
           03  WRK-LTRIM-TMP               PIC X(500).
           03  WRK-KANACHK-STR             PIC X(500).
           03  WRK-SRYYMD.
               05  WRK-SRYYM               PIC X(06).
               05  WRK-SRYDD               PIC 9(02).
           03  WRK-KAISU                   PIC 9(05).
           03  WRK-EOMYMD                  PIC X(08).
           03  WRK-STIDX                   PIC 9(05).
           03  WRK-TABLE                   PIC X(64).
           03  WRK-PATHNAME                PIC X(64).
      *
       01  TDOJITSU-AREA.
           03  TDOJITSU-MAX                PIC 9(05).
           03  TDOJITSU-OCC                OCCURS 50.
               05  TDOJITSU-NYUINYMD       PIC X(08).
      *
       01  TNYUINRRK-AREA.
           03  TNYUINRRK-MAX               PIC 9(05).
           03  TNYUINRRK-OCC               OCCURS 50.
               05  TNYUINRRK-DOJITSU       PIC 9(01).
               05  TNYUINRRK-RRKNUM        PIC 9(03).
               05  TNYUINRRK-SRYSTYMD      PIC X(08).
               05  TNYUINRRK-SRYEDYMD      PIC X(08).
      *
       01  TSKIP-AREA.
           03  TSKIP-MAX                  PIC 9(05).
           03  TSKIP-OCC                  OCCURS    500.
               05  TSKIP-CD               PIC X(09).
      *
       01  TKAISU-AREA.
           03  TKAISU-MAX                  PIC 9(05).
           03  TKAISU-OCC                  OCCURS    501.
               05  TKAISU-SRYYMD           PIC X(08).
               05  TKAISU-KAISU            PIC 9(05).
      *
       01  WRK-ROW-AREA.
           03  WRK-ERRLST-ROW              PIC S9(9).
           03  WRK-BYOLST-ROW              PIC S9(9).
           03  WRK-CHKLST-ROW              PIC S9(9).
      *
       01  CONST-AREA.
           03  CONST-TDOJITSU-MAX          PIC 9(03)   VALUE 50.
           03  CONST-TNYUINRRK-MAX         PIC 9(03)   VALUE 50.
           03  CONST-ERRLST-MAX            PIC 9(05)   VALUE   200.
           03  CONST-BYOLST-MAX            PIC 9(05)   VALUE   300.
           03  CONST-CHKLST-MAX            PIC 9(05)   VALUE   500.
           03  CONST-TSKIP-MAX             PIC 9(05)   VALUE   500.
           03  CONST-MKCD-MAX              PIC 9(05)   VALUE   21.
      *
      *    データチェック一括処理パラメータ
           COPY    "CPORCBSD01PARA.INC".
      *
           COPY "CPLNKD04.INC".
      *
           COPY "COMMON-SPA"               REPLACING //SPA-//
                                           BY        //WKSPA-//.
      *
           COPY "CPDTCHKERRMES.INC".
      *
           COPY "CPDTCHKKOMOKU.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
       01  ETENSU3-REC.
           COPY    "CPETENSU3.INC".
      *
       01  ETENSU5-REC.
           COPY    "CPETENSU5.INC".
      *
       01  CHKMST-REC.
           COPY "CPCHK.INC"                REPLACING //CHK-//
                                           BY        //CHKMST-//.
      *
       01  CHKSND-REC.
           COPY    "CPCHKSND.INC".
      *
       01  CHKTRD-REC.
           COPY "CPCHKTRD.INC".
      *
       01  CHK005-REC.
           COPY "CPCHK005.INC".
      *
           COPY    "CPCHK004.INC".
           COPY    "CPCK1200.INC".
           COPY    "CPCK1201.INC".
           COPY    "CPCK2000.INC".
      *
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1005.INC".
           COPY    "CPSK1010.INC".
           COPY    "CPSK1101.INC".
           COPY    "CPSK200501.INC".
      *
           COPY    "CPCHKREIGAI.INC".
      *
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
           COPY    "CPTENSU.INC".
      *
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
       01  TEKIOUBYOMEI-REC.
           COPY    "CPTEKIOUBYOMEI.INC".
      *
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
       01  NSRYSRH-REC.
           COPY    "CPNSRYSRH.INC".
      *
       01  NRRKSRH-REC.
           COPY    "CPNRRKSRH.INC".
      *
       01  MATERIAL-K-REC.
           COPY    "CPMATERIAL-K.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPDSUB01.INC".
      *
           COPY    "CPSENDX01.INC".
      *
      *    文字列操作サブ
           COPY    "CPORCSSTRING.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    保険組み合せ取得
           COPY    "CPORCSHKNSET.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    一時チェックマスタ２編集サブ
           COPY    "CPORCSS013.INC".
      *
           COPY    "CPORCSNUM.INC".
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
           COPY    "ORCA41SCRAREA.INC".
      *
       PROCEDURE                       DIVISION    USING
                                                   MCPAREA
                                                   SPAAREA
                                                   LINKAREA
                                                   SCRAREA.
      *****************************************************************
      *    主    処    理
      *****************************************************************
      *
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                      FLG-AREA
                                           IDX-AREA
                                           WRK-AREA
                                           CNT-AREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA.
           MOVE    SPA-WORK            TO  SPA-D01KYOUTU
           MOVE    SPA-FREE            TO  SPA-D04FREE
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-END
      *
           PERFORM 800-MCNDATE-SEC
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN    "PUTG"          ALSO    "CLICKED"
               PERFORM 200-GMNSENI
      *    入力
           WHEN    OTHER
               PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END             =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-D01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-D04FREE         TO  SPA-FREE
           .
      *
       000-PROC-SEC-EXT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "DERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF
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
           MOVE    "D04    "           TO  MCP-WINDOW
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
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
      *
               EVALUATE    SPA-MOTOPG
               WHEN    "X01"
               WHEN    "C02"
                   PERFORM 800-LOAD-D04SPA-SEC
                   MOVE    DSUB01-SCRSPA-AREA
                                           TO  SPA-D04
                   MOVE    DSUB01-COMMON   TO  SPA-D01KYOUTU
                   MOVE    SPA-GMN-BYOMEI  TO  WRK-BYOMEI
                   MOVE    SPA-GMN-HOSOKU  TO  WRK-HOSOKU
                   IF    ( SPA-MOTOPG      =   "X01" )
                       MOVE    SPA-GMN-CHKLST-SEL
                                           TO  WRK-CHKLST-SEL
                       MOVE    ZERO        TO  SPA-GMN-CHKLST-SELNUM
                                               SPA-GMN-CHKLST-SEL
                       INITIALIZE              SPA-GMN-CHKLST-AREA
                                               SPA-NAI-CHKLST-AREA
                       PERFORM 31011-CHKLST-SEC
                       IF    ( SPA-GMN-TCHKBYOMEI (WRK-CHKLST-SEL)
                                           =   WRK-BYOMEI )
                        AND  ( SPA-GMN-TCHKHOSOKU (WRK-CHKLST-SEL)
                                           =   WRK-HOSOKU )
                           MOVE    WRK-CHKLST-SEL
                                           TO  SPA-GMN-CHKLST-SELNUM
                                               SPA-GMN-CHKLST-SEL
                       END-IF
                   ELSE
                       IF    ( SPA-NAI-PTID    NOT =   ZERO )
                           MOVE    SPA-GMN-BYOLST-SEL
                                           TO  WRK-BYOLST-SEL
                           MOVE    ZERO        TO  SPA-GMN-BYOLST-SELNUM
                                                   SPA-GMN-BYOLST-SEL
                           INITIALIZE          SPA-GMN-BYOLST-AREA
                                               SPA-NAI-BYOLST-AREA
                           PERFORM 3101-BYOLST-SEC
                       END-IF
                       IF    ( SPA-GMN-TBYOMEI (WRK-BYOLST-SEL)
                                           =   WRK-BYOMEI )
                        AND  ( SPA-GMN-TBYOHOSOKU (WRK-BYOLST-SEL)
                                           =   WRK-HOSOKU )
                           MOVE    WRK-BYOLST-SEL
                                           TO  SPA-GMN-BYOLST-SELNUM
                                               SPA-GMN-BYOLST-SEL
                       END-IF
                   END-IF
                   MOVE    WRK-BYOMEI  TO  SPA-GMN-BYOMEI
                   MOVE    WRK-HOSOKU  TO  SPA-GMN-HOSOKU
                   EVALUATE    TRUE
                   WHEN  ( SPA-GMN-BYOLST-SELNUM   >   ZERO )
                       MOVE    4       TO  SPA-GMN-CUR
                   WHEN  ( SPA-GMN-CHKLST-SELNUM   >   ZERO )
                       MOVE    6       TO  SPA-GMN-CUR
                   WHEN  ( SPA-MOTOPG  =   "X01" )
                       MOVE    4       TO  SPA-GMN-CUR
                   WHEN  ( SPA-MOTOPG  =   "C02" )
                       MOVE    6       TO  SPA-GMN-CUR
                   END-EVALUATE
                   GO  TO  300-SCREEN-EXT
               END-EVALUATE
      *
           END-IF
      *
           EVALUATE    SPA-MOTOPG
      *    例外一覧画面
           WHEN    "D05"
               PERFORM 800-LOAD-D04SPA-SEC
               PERFORM 3102-ERRLST-SEC
               GO  TO  300-SCREEN-EXT
      *    確認画面１
           WHEN    "DID1"
               PERFORM 330-DID1-SET-SEC
               GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC.
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-D04
      *
      *    コンボボックス編集処理
           PERFORM 3101-COMBO-EDIT-SEC
      *
           INITIALIZE                  SYS-1010-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1010-HOSPNUM
           MOVE    "1010"          TO  SYS-1010-KANRICD
           MOVE    SPA-SYSYMD      TO  SYS-1010-STYUKYMD
                                       SYS-1010-EDYUKYMD
           MOVE    SPA-OPID        TO  SYS-1010-USERID
           MOVE    SYS-1010-REC    TO  SYSKANRI-REC
           STRING  SYS-1010-USERID DELIMITED    BY SIZE
                   "%"             DELIMITED    BY SIZE
           INTO    SYS-KANRITBL
           END-STRING
           PERFORM 900-SYSKANRI-KEY3-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1010-REC
           MOVE    SYS-1010-GSRAUTH(22:1)
                                   TO  SPA-NAI-GSRAUTH-22
           MOVE    SYS-1010-GSRAUTH(63:1)
                                   TO  SPA-NAI-GSRAUTH-103
      *
           MOVE    1               TO  SPA-GMN-ERRLST-PAGE
      *
           INITIALIZE                  CHK004-REC
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    "2000"          TO  CHK004-KANRICD
           MOVE    "4"             TO  CHK004-KBNCD
           MOVE    SPA-SYSYMD      TO  CHK004-STYUKYMD
                                       CHK004-EDYUKYMD
           PERFORM 900-CHK004-KEY-SEL-SEC
           IF    ( FLG-CHK004      =   ZERO )
               MOVE    CHK004-REC      TO  CK4-2000-REC
               MOVE    CK4-2000-TBL    TO  ORCBSD01PARA
               MOVE    ORCBSD01-SRYYM  TO  SPA-NAI-SRYYM
               MOVE    ORCBSD01-NYUGAIKBN
                                       TO  SPA-NAI-NYUGAIKBN
      *
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "1101"          TO  SYS-KANRICD
               MOVE    "*"             TO  SYS-KBNCD
               MOVE    ORCBSD01-SRYYM  TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               MOVE    "01"            TO  SYS-STYUKYMD (7:2)
                                           SYS-EDYUKYMD (7:2)
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-1101-REC
               MOVE    SYS-1101-REIGAICHKKBN
                                       TO  SPA-NAI-REIGAICHKKBN
      *
               IF    ( SYS-1101-TEKBYOYKZKBN   =   SPACE )
                   MOVE    "0"         TO  SPA-NAI-TEKBYOYKZKBN
               ELSE
                   MOVE    SYS-1101-TEKBYOYKZKBN
                                       TO  SPA-NAI-TEKBYOYKZKBN
               END-IF
      *
               IF    ( SYS-1101-TEKBYOSRYKBN   =   SPACE )
                   MOVE    "0"         TO  SPA-NAI-TEKBYOSRYKBN
               ELSE
                  MOVE    SYS-1101-TEKBYOSRYKBN
                                       TO  SPA-NAI-TEKBYOSRYKBN
               END-IF
      *
               IF    ( SYS-1101-SRYHEIMSTKBN   =   SPACE )
                   MOVE    "2"         TO  SPA-NAI-SRYHEIMSTKBN
               ELSE
                   MOVE    SYS-1101-SRYHEIMSTKBN
                                       TO  SPA-NAI-SRYHEIMSTKBN
               END-IF
      *
               DISPLAY "SYS-1101-SRYMATERKKBN=" SYS-1101-SRYMATERKKBN
               IF    ( SYS-1101-SRYMATERKKBN   =   SPACE )
                   MOVE    "2"         TO  SPA-NAI-SRYMATERKKBN
               ELSE
                   MOVE    SYS-1101-SRYMATERKKBN
                                       TO  SPA-NAI-SRYMATERKKBN
               END-IF
      *
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "2005"          TO  SYS-KANRICD
               MOVE    "01"            TO  SYS-KBNCD
               MOVE    ORCBSD01-SRYYM  TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               MOVE    "01"            TO  SYS-STYUKYMD (7:2)
                                           SYS-EDYUKYMD (7:2)
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-200501-REC
               IF    ( SYS-200501-TENKIBYOMEI-KBN
                                       =   SPACE )
                   MOVE    "0"         TO  SPA-NAI-TENKIBYOMEIKBN
               ELSE
                   MOVE    SYS-200501-TENKIBYOMEI-KBN
                                       TO  SPA-NAI-TENKIBYOMEIKBN
               END-IF
      *
               MOVE    SPA-NAI-SRYYM       TO  WRK-YMD
               MOVE    "01"                TO  WRK-YMD(7:2)
               PERFORM 800-SEIWA-HEN-SEC
               STRING  WRK-EDTYMD3 (1:16)  DELIMITED   BY  SIZE
                       "診療分"            DELIMITED   BY  SIZE
               INTO    SPA-GMN-LBLSRYYM
               END-STRING
      *
               IF    ( SPA-NAI-NYUGAIKBN   =   "1" )
                   MOVE    "入院"          TO  SPA-GMN-LBLNYUGAI
               ELSE
                   MOVE    "外来"          TO  SPA-GMN-LBLNYUGAI
               END-IF
      *
               MOVE    "00"            TO  WRK-CMB-CD
               PERFORM 4200-CHKNUMLST-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-CHKNUM-G
      *
               PERFORM 3101-ERRLST-SEC
           END-IF
      *
           MOVE    2               TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象コンボ編集処理
      *****************************************************************
       3101-COMBO-EDIT-SEC             SECTION.
      *
           PERFORM 3101-CHKNUMLST-SEC
      *
           PERFORM 3101-BEHAVELST-SEC
      *
           .
       3101-COMBO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    チェック区分コンボ編集処理
      *****************************************************************
       3101-CHKNUMLST-SEC              SECTION.
      *
           MOVE    "00"                TO  SPA-GMN-CHKNUML    (1)
           MOVE    "全て"              TO  SPA-GMN-CHKNUMMEIL (1)
           MOVE    1                   TO  IDX-CMB
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   DTCHKKOMOKU-MAX )
      *
               COMPUTE IDX-CMB =   IDX-CMB +   1
      *
               MOVE    DTCHKKOMOKU-CHKNUM (IDX0)
                                       TO  SPA-GMN-CHKNUML    (IDX-CMB)
      *
               MOVE    DTCHKKOMOKU-CHKKOMOKU (IDX0)
                                       TO  SPA-GMN-CHKNUMMEIL (IDX-CMB)
      *
           END-PERFORM
      *
           COMPUTE IDX-CMB =   IDX-CMB +   1
           MOVE    "99"                TO  SPA-GMN-CHKNUML    (IDX-CMB)
           MOVE    "その他"            TO  SPA-GMN-CHKNUMMEIL (IDX-CMB)
      *
           COMPUTE IDX-CMB =   IDX-CMB +   1
           MOVE    "A1"                TO  SPA-GMN-CHKNUML    (IDX-CMB)
           MOVE    "未登録薬剤"
                                       TO  SPA-GMN-CHKNUMMEIL (IDX-CMB)
      *
           MOVE    IDX-CMB             TO  SPA-GMN-CHKNUM-MAX
      *
           .
       3101-CHKNUMLST-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象外コンボ編集処理
      *****************************************************************
       3101-BEHAVELST-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-BEHAVELST-G
           MOVE    "1"                 TO  SPA-GMN-BEHAVEL    (1)
           MOVE    "チェックしない（当月）"
                                       TO  SPA-GMN-BEHAVEMEIL (1)
           MOVE    "2"                 TO  SPA-GMN-BEHAVEL    (2)
           MOVE    "チェックしない（常時）"
                                       TO  SPA-GMN-BEHAVEMEIL (2)
      *
           MOVE    2                   TO  SPA-GMN-BEHAVE-MAX
      *
           .
       3101-BEHAVELST-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象外コンボ編集処理
      *****************************************************************
       3102-BEHAVELST-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-BEHAVELST-G
           MOVE    "0"                 TO  SPA-GMN-BEHAVEL    (1)
           MOVE    "例外設定を解除"
                                       TO  SPA-GMN-BEHAVEMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-BEHAVEL    (2)
           MOVE    "チェックしない（当月）"
                                       TO  SPA-GMN-BEHAVEMEIL (2)
           MOVE    "2"                 TO  SPA-GMN-BEHAVEL    (3)
           MOVE    "チェックしない（常時）"
                                       TO  SPA-GMN-BEHAVEMEIL (3)
      *
           MOVE    3                   TO  SPA-GMN-BEHAVE-MAX
      *
           .
       3102-BEHAVELST-EXT.
           EXIT.
      *****************************************************************
      *    エラー内容コラムリスト編集処理
      *****************************************************************
       3101-ERRLST-SEC                 SECTION.
      *
           MOVE    1               TO  FLG-ERRLST-ROW
      *
           INITIALIZE                  SPA-GMN-PTINF-AREA
                                       SPA-NAI-PTINF-AREA
                                       SPA-GMN-PTINFERR-AREA
                                       SPA-NAI-PTINFERR-AREA
      *
           INITIALIZE                  SPA-GMN-ERRLST-AREA
                                       SPA-NAI-ERRLST-AREA
      *
           INITIALIZE                  SPA-GMN-CHKLST-AREA
                                       SPA-NAI-CHKLST-AREA
      *
           MOVE    ZERO            TO  IDXL
      *
           INITIALIZE                  CHK004-REC
           MOVE    "1200"          TO  CHK004-KANRICD
           MOVE    "______"        TO  CHK004-KBNCD (1:6)
           IF    ( SPA-GMN-CHKNUM  =   "00" )
               MOVE    "__"            TO  CHK004-KBNCD (7:2)
           ELSE
               MOVE    SPA-GMN-CHKNUM  TO  CHK004-KBNCD (7:2)
           END-IF
           MOVE    SPA-SYSYMD          TO  CHK004-STYUKYMD
                                           CHK004-EDYUKYMD
           PERFORM 900-CHK004-KEY7-SEL-SEC
           MOVE    CHK004-REC      TO  CK4-1200-REC
      *
           MOVE    ZERO                TO  CNT-CHK004
           COMPUTE WRK-OFFSET
               = ( SPA-GMN-ERRLST-PAGE - 1 )   *   CONST-ERRLST-MAX
      *
           PERFORM UNTIL ( FLG-CHK004  NOT  =  ZERO )
                    OR   ( CNT-CHK004      >=  WRK-OFFSET )
      *
               COMPUTE CNT-CHK004
                   =   CNT-CHK004      +   1
      *
               PERFORM 900-CHK004-KEY7-FET-SEC
               MOVE    CHK004-REC  TO  CK4-1200-REC
      *
           END-PERFORM
      *
           PERFORM UNTIL ( FLG-CHK004      NOT  =  ZERO )
                    OR   ( SPA-GMN-ERRLST-MAX  >=  CONST-ERRLST-MAX )
      *
               COMPUTE IDXL  =  IDXL     +   1
      *
               COMPUTE SPA-GMN-TNUM (IDXL)
                   = ( SPA-GMN-ERRLST-PAGE - 1 )   *   CONST-ERRLST-MAX
                   +   IDXL
      *
               MOVE    IDXL  TO SPA-GMN-ERRLST-MAX
      *
               MOVE    CK4-1200-PTID   TO  WRK-PTID
               EVALUATE    CK4-1200-ERRCD
               WHEN    "A10001"
                   MOVE    "未登録薬剤"
                                           TO  SPA-GMN-TNAME  (IDXL)
               WHEN OTHER
                   PERFORM 900-PTINF-KEY-SEL-SEC
                   PERFORM 900-PTNUM-KEY-SEL-SEC
      *
                   MOVE    CK4-1200-PTID   TO  SPA-NAI-TPTID  (IDXL)
                   MOVE    PTNUM-PTNUM     TO  SPA-GMN-TPTNUM (IDXL)
                   MOVE    PTINF-NAME      TO  SPA-GMN-TNAME  (IDXL)
               END-EVALUATE
      *
               EVALUATE    CK4-1200-HKNKBN
               WHEN    "1"
                   MOVE    "社保"          TO  SPA-GMN-THKNSBT(IDXL)
               WHEN    "2"
                   MOVE    "国保"          TO  SPA-GMN-THKNSBT(IDXL)
               WHEN    "3"
                   MOVE    "労災"          TO  SPA-GMN-THKNSBT(IDXL)
               WHEN    "4"
                   MOVE    "自賠責"        TO  SPA-GMN-THKNSBT(IDXL)
               WHEN    "5"
                   MOVE    "自費"          TO  SPA-GMN-THKNSBT(IDXL)
               WHEN    "6"
                   MOVE    "公害"          TO  SPA-GMN-THKNSBT(IDXL)
               WHEN    "7"
                   MOVE    "後期高齢者"    TO  SPA-GMN-THKNSBT(IDXL)
               WHEN    "8"
                   MOVE    "治験"          TO  SPA-GMN-THKNSBT(IDXL)
               WHEN    "9"
                   MOVE    "第三者行為"    TO  SPA-GMN-THKNSBT(IDXL)
               WHEN    "H"
                   MOVE    "包括"          TO  SPA-GMN-THKNSBT(IDXL)
               WHEN    "Z"
                   MOVE    "その他"        TO  SPA-GMN-THKNSBT(IDXL)
               END-EVALUATE
      *
               MOVE    CK4-1200-ERRMSG TO  SPA-GMN-TERRMSG (IDXL)
               MOVE    CK4-1200-KBNCD  TO  SPA-NAI-TKBNCD  (IDXL)
               MOVE    CK4-1200-ERRCD  TO  SPA-NAI-TERRCD  (IDXL)
               MOVE    SPA-NAI-TKBNCD(IDXL)(7:2)
                                       TO  SPA-NAI-TCHKNUM (IDXL)
      *
               MOVE        SPACE       TO  WRK-CHKKBN
               EVALUATE    SPA-NAI-TCHKNUM (IDXL)
               WHEN    "08"
               WHEN    "A1"
                   MOVE    "1"         TO  WRK-CHKKBN
               WHEN    "09"
                   MOVE    "2"         TO  WRK-CHKKBN
               WHEN    "10"
                   MOVE    "4"         TO  WRK-CHKKBN
               WHEN    "11"
                   EVALUATE    SPA-NAI-TERRCD  (IDXL)
                   WHEN  "110001"
                       MOVE    "5"     TO  WRK-CHKKBN
                   WHEN  "110002"
                       MOVE    "A"     TO  WRK-CHKKBN
                   WHEN  "110003"
                       MOVE    "B"     TO  WRK-CHKKBN
                   END-EVALUATE
               WHEN    "15"
                   MOVE    "6"         TO  WRK-CHKKBN
               WHEN    "16"
                   MOVE    "7"         TO  WRK-CHKKBN
               WHEN    "17"
                   MOVE    "8"         TO  WRK-CHKKBN
               WHEN    "18"
                   MOVE    "9"         TO  WRK-CHKKBN
               END-EVALUATE
               MOVE    WRK-CHKKBN      TO  SPA-NAI-TCHKKBN (IDXL)
      *
               INITIALIZE                      CHK004-REC
               MOVE    "1201"              TO  CHK004-KANRICD
               MOVE    SPA-NAI-TKBNCD(IDXL)
                                           TO  CHK004-KBNCD
               MOVE    SPA-SYSYMD          TO  CHK004-STYUKYMD
                                               CHK004-EDYUKYMD
               PERFORM 900-CHK004-KEY-SEL-SEC
               MOVE    CHK004-REC          TO  CK4-1201-REC
               MOVE    CHK004-KANRITBL     TO  SPA-NAI-TCK4-1201 (IDXL)
      *
               PERFORM 900-CHKREIGAI-KEY5-SEL-SEC
               IF    ( FLG-CHKREIGAI   =   ZERO )
                   IF    ( CHKREIGAI-STYUKYMD  =   "00000000" )
                       MOVE    "2"         TO  SPA-GMN-TREIGAI(IDXL)
                   ELSE
                       MOVE    "1"         TO  SPA-GMN-TREIGAI(IDXL)
                   END-IF
               END-IF
      *
               PERFORM 900-CHK004-KEY7-FET-SEC
               MOVE    CHK004-REC  TO  CK4-1200-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-CHK004      =   ZERO )
               MOVE    1           TO  SPA-GMN-ERRLST-NEXT
           END-IF
      *
           MOVE    2               TO  SPA-GMN-CUR
      *
           .
       3101-ERRLST-EXT.
           EXIT.
      *****************************************************************
      *    エラー内容コラムリスト編集処理
      *****************************************************************
       3102-ERRLST-SEC                 SECTION.
      *
           PERFORM VARYING IDXL FROM    1   BY  1
                   UNTIL ( IDXL >   CONST-ERRLST-MAX )
      *
               MOVE    SPACE               TO  SPA-GMN-TREIGAI(IDXL)
      *
               INITIALIZE                      CHK004-REC
               MOVE    "1201"              TO  CHK004-KANRICD
               MOVE    SPA-NAI-TKBNCD(IDXL)
                                           TO  CHK004-KBNCD
               MOVE    SPA-SYSYMD          TO  CHK004-STYUKYMD
                                               CHK004-EDYUKYMD
               MOVE    SPA-NAI-TCK4-1201 (IDXL)
                                           TO  CHK004-KANRITBL
               MOVE    CHK004-REC          TO  CK4-1201-REC
               PERFORM 900-CHKREIGAI-KEY5-SEL-SEC
               IF    ( FLG-CHKREIGAI   =   ZERO )
                   IF    ( CHKREIGAI-STYUKYMD  =   "00000000" )
                       MOVE    "2"         TO  SPA-GMN-TREIGAI(IDXL)
                   ELSE
                       MOVE    "1"         TO  SPA-GMN-TREIGAI(IDXL)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       3102-ERRLST-EXT.
           EXIT.
      *****************************************************************
      *    患者病名コラムリスト編集処理
      *****************************************************************
       3101-BYOLST-SEC                 SECTION.
      *
           IF  ( SPA-NAI-BYOLST-PTID =   SPA-NAI-PTID )
               CONTINUE
           ELSE
      *
               MOVE    SPA-AREA            TO  WKSPA-AREA
               INITIALIZE                      ORCSHKNSETAREA
               MOVE    SPA-NAI-PTID        TO  WKSPA-PTID
               MOVE    SPA-NAI-BIRTHDAY    TO  WKSPA-BIRTHDAY
               MOVE    "2"                 TO  ORCSHKN-KBN
      *
               CALL    "ORCSHKNSET"        USING
                                           WKSPA-AREA
                                           ORCSHKNSETAREA
               IF    ( ORCSHKN-ERRCD   NOT =   ZERO )
                   INITIALIZE              ORCSHKNSETAREA
               END-IF
      *
               INITIALIZE                  SPA-GMN-BYOLST-AREA
                                           SPA-NAI-BYOLST-AREA
                                           SPA-GMN-BYOMEI
                                           SPA-GMN-HOSOKU
      *
               MOVE    SPA-NAI-PTID    TO  SPA-NAI-BYOLST-PTID
      *
               MOVE    ZERO            TO  IDXL
      *
               MOVE    1               TO  FLG-BYOLST-ROW
               PERFORM 900-PTBYOMEI-KEY39-SEL-SEC
      *
               PERFORM UNTIL ( FLG-PTBYOMEI    NOT =   ZERO )
                        OR   ( SPA-GMN-BYOLST-MAX >=   CONST-BYOLST-MAX)
      *
                   COMPUTE IDXL =   IDXL +   1
                   MOVE    IDXL         TO  SPA-GMN-BYOLST-MAX
      *
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
                   MOVE    "1005"          TO  SYS-KANRICD
                   MOVE    PTBYO-SRYKA     TO  SYS-KBNCD
                   MOVE    PTBYO-SRYYMD    TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   MOVE    SYSKANRI-REC    TO  SYS-1005-REC
      *
                   MOVE    PTBYO-BYOMEI    TO  SPA-GMN-TBYOMEI (IDXL)
                   MOVE    PTBYO-HOSOKU-COMMENT
                                            TO  SPA-GMN-TBYOHOSOKU(IDXL)
                   MOVE    SYS-1005-SRYKANAME3
                                           TO  SPA-GMN-TSRYKA  (IDXL)
                   MOVE    PTBYO-SRYYMD    TO  WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-EDTYMD1     TO  SPA-GMN-TSRYYMD (IDXL)
      *
                   EVALUATE    PTBYO-TENKIKBN
                   WHEN "1"
                       MOVE    "治ゆ"  TO  SPA-GMN-TTENKIKBN (IDXL)
                   WHEN "2"
                       MOVE    "死亡"  TO  SPA-GMN-TTENKIKBN (IDXL)
                   WHEN "3"
                       MOVE    "中止"  TO  SPA-GMN-TTENKIKBN (IDXL)
                   WHEN "4"
                       MOVE    "移行"  TO  SPA-GMN-TTENKIKBN (IDXL)
                   END-EVALUATE
      *
                   MOVE    PTBYO-TENKIYMD  TO  WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-EDTYMD1     TO  SPA-GMN-TTENKIYMD
                                                             (IDXL)
      *
                   IF    ( PTBYO-SYUBYOFLG =   "1" )
                       MOVE    "主"        TO  SPA-GMN-TSYUBYO
                                                             (IDXL)
                   END-IF
      *
                   IF    ( PTBYO-SYUBYOFLG =   "1" )
                       MOVE    "主"        TO  SPA-GMN-TSYUBYO
                                                             (IDXL)
                   END-IF
      *
                   EVALUATE    PTBYO-UTAGAIFLG
                   WHEN    "1"
                       MOVE    "疑い"      TO  SPA-GMN-TUTAGAI
                                                             (IDXL)
                   WHEN    "2"
                       MOVE    "急性"      TO  SPA-GMN-TUTAGAI
                                                             (IDXL)
                   WHEN    "3"
                       MOVE    "急性かつ疑い"
                                           TO  SPA-GMN-TUTAGAI
                                                             (IDXL)
                   END-EVALUATE
      *
                   EVALUATE    PTBYO-NYUGAIKBN
                   WHEN    "1"
                       MOVE    "入"        TO  SPA-GMN-TNYUGAIKBN
                                                             (IDXL)
                   WHEN    "2"
                       MOVE    "外"        TO  SPA-GMN-TNYUGAIKBN
                                                             (IDXL)
                   END-EVALUATE
      *
                   IF    ( PTBYO-HKNCOMBI  =   ZERO )
                       CONTINUE
                   ELSE
                       PERFORM VARYING IDXH FROM    1   BY  1
                               UNTIL ( IDXH >   ORCSHKN-HKN-MAX )
                                OR   ( SPA-GMN-THKNCOMBI (IDXL)
                                           NOT =   SPACE )
      *
                           IF    ( PTBYO-HKNCOMBI
                                   =   ORCSHKN-GMN-HKNCOMBINUM(IDXH))
                               MOVE    ORCSHKN-GMN-HKNCOMBIMEI(IDXH)
                                        TO  SPA-GMN-THKNCOMBI (IDXL)
                               INITIALIZE   ORCSKANACHKAREA
                               MOVE    "1"  TO  KANACHK-SYORI
                               MOVE    SPA-GMN-THKNCOMBI (IDXL)
                                            TO  KANACHK-MAE-INPUT
                               CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                               MOVE    KANACHK-MAE-INPUT
                                            TO  SPA-GMN-THKNCOMBI
                                                         (IDXL)
                           END-IF
      *
                       END-PERFORM
                   END-IF
      *
                   PERFORM 900-PTBYOMEI-KEY39-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
               MOVE    "key39"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
       3101-BYOLST-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタコラムリスト編集処理
      *****************************************************************
       3101-CHKLST-SEC                 SECTION.
      *
           IF    ( SPA-NAI-CHKNUM      NOT =   "20" )
            AND  ( SPA-NAI-CHKLST-CHKNUM   =   SPA-NAI-CHKNUM )
            AND  ( SPA-NAI-CHKLST-CHKKBN   =   SPA-NAI-CHKKBN )
            AND  ( SPA-NAI-CHKLST-SRYCD    =   SPA-GMN-SRYCD  )
            AND  ( SPA-NAI-CHKLST-BYOMEI   =   SPA-GMN-CBYOMEI)
               CONTINUE
           ELSE
               MOVE    1           TO  SPA-GMN-CHKLST-PAGE
               PERFORM 31011-CHKLST-SEC
           END-IF
      *
           .
       3101-CHKLST-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタコラムリスト編集処理
      *****************************************************************
       31011-CHKLST-SEC                 SECTION.
      *
           COMPUTE SPA-GMN-CHKLST-SKIP
               = ( SPA-GMN-CHKLST-PAGE - 1 )
               *   CONST-CHKLST-MAX
      *
           INITIALIZE                      SPA-GMN-CHKLST-AREA
                                           SPA-NAI-CHKLST-AREA
                                           TSKIP-AREA
      *
           MOVE    SPA-NAI-CHKNUM      TO  SPA-NAI-CHKLST-CHKNUM
           MOVE    SPA-NAI-CHKKBN      TO  SPA-NAI-CHKLST-CHKKBN
           MOVE    SPA-GMN-SRYCD       TO  SPA-NAI-CHKLST-SRYCD
           MOVE    SPA-GMN-CBYOMEI     TO  SPA-NAI-CHKLST-BYOMEI
      *
           EVALUATE    SPA-NAI-CHKLST-CHKKBN
           WHEN    SPACE
               IF    ( SPA-NAI-CHKNUM  =   "20" )
                AND  ( SPA-GMN-ERRCD   =   "200001" )
                   PERFORM 31011-CHKLST-KAISU-SEC
               END-IF
           WHEN    "1"
           WHEN    "2"
           WHEN    "6"
               PERFORM 31011-CHKLST-CHKSND-SEC
           WHEN    "4"
           WHEN    "5"
           WHEN    "A"
           WHEN    "B"
               PERFORM 31011-CHKLST-CHKMST-SEC
           WHEN    "7"
               PERFORM 31011-CHKLST-CHKTRD-SEC
           WHEN    "8"
           WHEN    "9"
               PERFORM 31011-CHKLST-CHK005-SEC
           END-EVALUATE
      *
           .
       31011-CHKLST-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタコラムリスト編集処理（回数）
      *****************************************************************
       31011-CHKLST-KAISU-SEC          SECTION.
      *
           MOVE    ZERO                TO  IDXL
      *
           INITIALIZE                      TKAISU-AREA
                                           TNYUINRRK-MAX
                                           TDOJITSU-AREA
      *
           MOVE    1                   TO  FLG-CHKLST-ROW
      *
           IF    ( SPA-GMN-TANICD  =   132 OR 133 OR 134 )
               PERFORM 310111-TNYUINRRK-EDIT-SEC
               PERFORM 310111-TDOJITSU-EDIT-SEC
           END-IF
      *
           PERFORM 310111-CHKLST-KAISU-SEC
           IF    ( SPA-NAI-NYUGAIKBN   =   "1" )
               PERFORM 310112-CHKLST-KAISU-SEC
           END-IF
      *
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    >   TKAISU-MAX - 1 )
      *
               COMPUTE WRK-STIDX   =   IDXL    +   1
      *
               PERFORM VARYING IDXK    FROM    WRK-STIDX   BY  1
                       UNTIL ( IDXK    >   TKAISU-MAX )
      *
                   IF    ( TKAISU-SRYYMD (IDXL)
                                       <   TKAISU-SRYYMD (IDXK))
                       MOVE    TKAISU-OCC (IDXL)
                                       TO  TKAISU-SRYYMD (501)
                       MOVE    TKAISU-SRYYMD (IDXK)
                                       TO  TKAISU-SRYYMD (IDXL)
                       MOVE    TKAISU-OCC (501)
                                       TO  TKAISU-SRYYMD (IDXK)
                   END-IF
      *
               END-PERFORM
      *
           END-PERFORM
      *
           MOVE    TKAISU-MAX      TO  SPA-GMN-CHKLST-MAX
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    >   TKAISU-MAX )
      *
              MOVE    IDXL         TO  SPA-GMN-TCHKLSTNUM  (IDXL)
              MOVE    TKAISU-SRYYMD (IDXL)
                                   TO  SPA-NAI-TSNTYMD (IDXL)
               MOVE    TKAISU-SRYYMD (IDXL)
                                   TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-EDTYMD1 TO  SPA-GMN-TSNTYMD (IDXL)
               MOVE    TKAISU-KAISU (IDXL)
                                   TO  SPA-GMN-TKAISU  (IDXL)
      *
           END-PERFORM
      *
           .
       31011-CHKLST-KAISU-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴退避領域初期処理
      *****************************************************************
       310111-TNYUINRRK-EDIT-SEC            SECTION.
      *
           MOVE    SPA-NAI-SRYYM   TO   WRK-SYMD
           MOVE    "01"            TO   WRK-SYMD(7:2)
           PERFORM 800-GETUMATU-SEC
           MOVE    WRK-SYMD        TO   WRK-EOMYMD
      *
           PERFORM 900-PTNYUINRRK-KEY61-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
                    OR   ( TNYUINRRK-MAX      >=   CONST-TNYUINRRK-MAX )
      *
               COMPUTE TNYUINRRK-MAX   =   TNYUINRRK-MAX    +   1
      *
               MOVE    PTNYUINRRK-RRKNUM
                                   TO  TNYUINRRK-RRKNUM  (TNYUINRRK-MAX)
               MOVE    PTNYUINRRK-NYUINYMD
                                   TO  TNYUINRRK-SRYSTYMD(TNYUINRRK-MAX)
               MOVE    PTNYUINRRK-TAIINYMD
                                   TO  TNYUINRRK-SRYEDYMD(TNYUINRRK-MAX)
      *
               PERFORM 900-PTNYUINRRK-KEY61-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key61"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       310111-TNYUINRRK-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    同日再入院情報退避領域初期処理
      *****************************************************************
       310111-TDOJITSU-EDIT-SEC        SECTION.
      *
           PERFORM 900-PTNYUINRRK-KEY60-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
                    OR   ( TDOJITSU-MAX       >=   CONST-TDOJITSU-MAX )
      *
               COMPUTE TDOJITSU-MAX = TDOJITSU-MAX     +   1
      *
               MOVE    PTNYUINRRK-NYUINYMD
                                   TO  TDOJITSU-NYUINYMD (TDOJITSU-MAX)
      *
               PERFORM VARYING IDXR    FROM    1   BY  1
                       UNTIL ( IDXR    >   TNYUINRRK-MAX )
                   IF    ( PTNYUINRRK-NYUINYMD
                                   =   TNYUINRRK-SRYSTYMD (IDXR) )
                       MOVE    1           TO  TNYUINRRK-DOJITSU (IDXR)
                       MOVE    TNYUINRRK-MAX    TO  IDXR
                   END-IF
               END-PERFORM
      *
               PERFORM 900-PTNYUINRRK-KEY60-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key60"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       310111-TDOJITSU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタコラムリスト編集処理（回数）
      *****************************************************************
       310111-CHKLST-KAISU-SEC         SECTION.
      *
           PERFORM 900-SRYACT-KEY20-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SRYACT  NOT =   ZERO )
      *
               PERFORM 900-SRYACCT-KEY57-SEL-SEC
      *
               PERFORM UNTIL ( FLG-SRYACT  NOT =   ZERO )
                        OR   ( SRY-ZAINUM  NOT =   ACCT-ZAINUM )
      *
                   IF  ( ACCT-SRYKBN   =   "95" OR "96"
                                                OR "98" )
                    OR ( ACCT-HKNCOMBI =   9999 )
                    OR ( ACCT-JIHOKBN  =   "1" )
      *
                       CONTINUE
                   ELSE
                       PERFORM VARYING IDXA    FROM    1   BY  1
                               UNTIL ( IDXA    >   5 )
                                OR   ( SRY-SRYCD (IDXA)    =   SPACE )
      *
                           IF    ( SPA-GMN-SRYCD   =   SRY-SRYCD(IDXA))
                               PERFORM 3101111-CHKLST-KAISU-SEC
                           END-IF
      *
                       END-PERFORM
                   END-IF
      *
                   PERFORM 900-SRYACT-KEY20-FET-SEC
      *
               END-PERFORM
      *
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       310111-CHKLST-KAISU-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタコラムリスト編集処理（回数）
      *****************************************************************
       3101111-CHKLST-KAISU-SEC        SECTION.
      *
           MOVE    ACCT-SRYYM      TO  WRK-SRYYM
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               MOVE    IDXD            TO  WRK-SRYDD
               IF    ( SPA-NAI-SRYSTYMD   <=   WRK-SRYYMD )
                AND  ( SPA-NAI-SRYEDYMD   >=   WRK-SRYYMD )
                AND  ( ACCT-DAY (1 IDXD)   >   ZERO )
      *
                   IF    ( SPA-GMN-TANICD  =   132 OR 133 OR 134 )
                       PERFORM VARYING IDXR    FROM    1  BY   1
                               UNTIL ( IDXR    >   TNYUINRRK-MAX )
                           IF    ( TNYUINRRK-SRYSTYMD (IDXR)
                                           <=  WRK-SRYYMD )
                            AND  ( TNYUINRRK-SRYEDYMD (IDXR)
                                           >=  WRK-SRYYMD )
                               IF    ( TNYUINRRK-SRYSTYMD (IDXR)
                                           =   WRK-SRYYMD )
                                AND  ( TNYUINRRK-DOJITSU  (IDXR)
                                           =   1 )
                                   IF    ( ACCT-DAY (3 IDXD) > ZERO )
                                       COMPUTE WRK-KAISU
                                           =   ACCT-DAY (3 IDXD)
                                       PERFORM 31011111-TKAISU-EDIT-SEC
                                   END-IF
                               ELSE
                                   IF    ( ACCT-DAY (2 IDXD) > ZERO )
                                       COMPUTE WRK-KAISU
                                           =   ACCT-DAY (2 IDXD)
                                       PERFORM 31011111-TKAISU-EDIT-SEC
                                   END-IF
                               END-IF
                               MOVE    TNYUINRRK-MAX   TO  IDXR
                           END-IF
                       END-PERFORM
                   ELSE
                       COMPUTE WRK-KAISU   =   ACCT-DAY (1 IDXD)
                       PERFORM 31011111-TKAISU-EDIT-SEC
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           .
       3101111-CHKLST-KAISU-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタコラムリスト編集処理（回数）
      *****************************************************************
       31011111-TKAISU-EDIT-SEC        SECTION.
      *
           PERFORM VARYING IDXK    FROM    1   BY  1
                   UNTIL ( IDXK    >   CONST-CHKLST-MAX )
      *
               IF    ( TKAISU-SRYYMD (IDXK)    =   SPACE )
                 OR  ( WRK-SRYYMD  =   TKAISU-SRYYMD (IDXK))
      *
                   IF    ( TKAISU-SRYYMD (IDXK)    =   SPACE )
                       MOVE    IDXK    TO  TKAISU-MAX
                       MOVE    WRK-SRYYMD
                                       TO  TKAISU-SRYYMD (IDXK)
                   END-IF
      *
                   COMPUTE TKAISU-KAISU (IDXK)
                       =   TKAISU-KAISU (IDXK) +   WRK-KAISU
      *
                   MOVE    CONST-CHKLST-MAX
                                       TO  IDXK
      *
               END-IF
      *
           END-PERFORM
      *
           .
       31011111-TKAISU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタコラムリスト編集処理（回数）
      *****************************************************************
       310112-CHKLST-KAISU-SEC         SECTION.
      *
           PERFORM 900-NSRYSRH-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NSRYSRH NOT =   ZERO )
      *
               PERFORM 900-NRRKSRH-KEY3-SEL-SEC
      *
               PERFORM UNTIL ( FLG-NRRKSRH NOT =   ZERO )
      *
                   PERFORM 3101121-KAKO-KAISU-EDIT-SEC
      *
                   PERFORM 900-NRRKSRH-KEY3-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_nrrksrh"   TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               PERFORM 900-NSRYSRH-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nsrysrh"   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       310112-CHKLST-KAISU-EXT.
           EXIT.
      *****************************************************************
      *    過去算定回数編集処理
      *****************************************************************
       3101121-KAKO-KAISU-EDIT-SEC     SECTION.
      *
           IF    ( SPA-NAI-SRYSTYMD   <=   NRRKSRH-SRYYMD )
            AND  ( SPA-NAI-SRYEDYMD   >=   NRRKSRH-SRYYMD )
      *
               IF    ( SPA-GMN-TANICD  =   132 OR 133 OR 134 )
                   PERFORM VARYING IDXR    FROM    1  BY   1
                           UNTIL ( IDXR    >   TNYUINRRK-MAX )
                       IF    ( TNYUINRRK-SRYSTYMD (IDXR)
                                       <=  NRRKSRH-SRYYMD )
                        AND  ( TNYUINRRK-SRYEDYMD (IDXR)
                                       >=  NRRKSRH-SRYYMD )
                        AND  ( TNYUINRRK-RRKNUM   (IDXR)
                                        =  NRRKSRH-RRKNUM )
                           COMPUTE WRK-KAISU   =   1
                           MOVE    NRRKSRH-SRYYMD  TO  WRK-SRYYMD
                           PERFORM 31011111-TKAISU-EDIT-SEC
                           MOVE    TNYUINRRK-MAX   TO  IDXR
                       END-IF
                   END-PERFORM
               ELSE
                   COMPUTE WRK-KAISU   =   1
                   MOVE    NRRKSRH-SRYYMD  TO  WRK-SRYYMD
                   PERFORM 31011111-TKAISU-EDIT-SEC
               END-IF
      *
           END-IF
      *
           .
       3101121-KAKO-KAISU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタコラムリスト編集処理
      *****************************************************************
       31011-CHKLST-CHKSND-SEC         SECTION.
      *
           INITIALIZE                      SS013-AREA
           MOVE    "DBCREATE"          TO  SS013-FUNC
           MOVE     SPA-NAI-CHKKBN     TO  SS013-CHKKBN
           IF     ( SPA-GMN-SRYCD      =   SPACE )
               MOVE     ALL "@"        TO  SS013-SRYCD
           ELSE
               MOVE     SPA-GMN-SRYCD  TO  SS013-SRYCD
           END-IF
           MOVE     SPA-NAI-SRYYM      TO  SS013-KJNYMD
           MOVE     "01"               TO  SS013-KJNYMD(7:2)
           CALL     "ORCSS013"         USING
                                       SS013-AREA
                                       SPA-AREA
      *
           MOVE    ZERO                TO  IDXL
                                           IDXN
      *
           MOVE    1                   TO  FLG-CHKLST-ROW
      *
           MOVE    SPA-NAI-SRYYM       TO  WRK-KJNYMD
           MOVE    "01"                TO  WRK-KJNYMD(7:2)
           PERFORM 900-CHKSND-KEY3-SEL-SEC
           DISPLAY "KEY3 FLG-CHKSND=" FLG-CHKSND
           IF    ( FLG-CHKSND          =   ZERO )
               IF    ( CHKSND-NYUGAIKBN    =   "0" )
                   MOVE    "1"         TO  SPA-NAI-CHKLST-NOCHK
               END-IF
           END-IF
      *
           PERFORM 900-CHKSND-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
                    OR   ( IDXN           >=   SPA-GMN-CHKLST-SKIP )
      *
               PERFORM 31011-CHKSND-SKIP-CHK-SEC
               IF    ( FLG-SKIP  =     ZERO )
                   COMPUTE IDXN  = IDXN  + 1
               END-IF
      *
               PERFORM 900-CHKSND-KEY2-FET-SEC
           END-PERFORM
      *
           PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
                    OR   ( SPA-GMN-CHKLST-MAX >=   CONST-CHKLST-MAX)
      *
               PERFORM 31011-CHKSND-SKIP-CHK-SEC
               IF    ( FLG-SKIP  =     ZERO )
      *
                   COMPUTE IDXL =   IDXL +   1
                   MOVE    IDXL         TO  SPA-GMN-CHKLST-MAX
      *
                   COMPUTE IDXN =   IDXN +   1
                   MOVE    IDXN         TO  SPA-GMN-TCHKLSTNUM  (IDXL)
      *
                   IF    ( CHKSND-CHKKBN   =   "1" OR "2" )
      *
                       MOVE    CHKSND-GRPID
                                           TO  SPA-GMN-TGRPID
                                                           (IDXL)
                       MOVE    CHKSND-HOSOKU
                                           TO  SPA-GMN-TCHKHOSOKU
                                                           (IDXL)
                   END-IF
      *
                   MOVE    CHKSND-BYOMEI   TO  SPA-GMN-TCHKBYOMEI
                                                               (IDXL)
      *
                   MOVE    CHKSND-COMPKBN  TO  WRK-EDIT-COMPKBN
                   PERFORM 800-EDIT-COMPKBN-MEI-SEC
                   IF    ( CHKSND-CHKKBN   =   "1" OR "2" )
                    AND  ( CHKSND-HOSOKU   NOT =   SPACE )
                       MOVE    WRK-EDIT-COMPKBN-MEI
                                            TO  WRK-EDIT-BYOMEI-COMPKBN
                       MOVE    CHKSND-HOSOKU-COMPKBN
                                            TO  WRK-EDIT-COMPKBN
                       MOVE    SPACE        TO  SPA-GMN-TCOMPKBN (IDXL)
                       STRING  WRK-EDIT-BYOMEI-COMPKBN DELIMITED BY SIZE
                               "（"                    DELIMITED BY SIZE
                               WRK-EDIT-COMPKBN-MEI    DELIMITED BY SIZE
                               "）"                    DELIMITED BY SIZE
                       INTO    SPA-GMN-TCOMPKBN (IDXL)
                       END-STRING
                   ELSE
                       MOVE    WRK-EDIT-COMPKBN-MEI
                                            TO  SPA-GMN-TCOMPKBN (IDXL)
                   END-IF
               END-IF
      *
               IF    ( CHKSND-TERMID   =   SS013-TEMP-NAME )
                AND  ( CHKSND-OPID     =   SS013-TEMP-KEY )
                   MOVE    "1"              TO  SPA-GMN-TOTHERMST
                                                           (IDXL)
               END-IF
      *
             PERFORM 900-CHKSND-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-CHKSND  =   ZERO )
               MOVE    1       TO  SPA-GMN-CHKLST-NEXT
           END-IF
      *
      *
           INITIALIZE                      SS013-AREA
           MOVE     "DBDROP"           TO  SS013-FUNC
           CALL     "ORCSS013"         USING
                                       SS013-AREA
                                       SPA-AREA
      *
           .
       31011-CHKLST-CHKSND-EXT.
           EXIT.
      *****************************************************************
      *    対象外データチェック処理
      *****************************************************************
       31011-CHKSND-SKIP-CHK-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SKIP
      *
           IF    ( CHKSND-BYOMEI   = SPACE )
               MOVE    1           TO  FLG-SKIP
           END-IF
      *
           .
       31011-CHKSND-SKIP-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタコラムリスト編集処理
      *****************************************************************
       31011-CHKLST-CHKMST-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDXL
      *
           MOVE    1                   TO  FLG-CHKLST-ROW
      *
           MOVE    SPA-NAI-SRYYM       TO  WRK-KJNYMD
           MOVE    "01"                TO  WRK-KJNYMD(7:2)
           PERFORM 900-CHKMST-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKMST  NOT =   ZERO )
                    OR   ( IDXN           >=   SPA-GMN-CHKLST-SKIP )
      *
               MOVE    CHKMST-CD       TO  WRK-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    FLG-TENSU       TO  FLG-ERROR
      *
               PERFORM 31011-CHKMST-SKIP-CHK-SEC
      *
               IF    ( FLG-SKIP        =   ZERO )
                   COMPUTE IDXN =   IDXN +   1
                   PERFORM 900-CHKMST-KEY2-FET-SEC
               END-IF
      *
           END-PERFORM
      *
           PERFORM UNTIL ( FLG-CHKMST  NOT =   ZERO )
                    OR   ( SPA-GMN-CHKLST-MAX >=   CONST-CHKLST-MAX)
      *
               MOVE    CHKMST-CD       TO  WRK-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    FLG-TENSU       TO  FLG-ERROR
      *
               PERFORM 31011-CHKMST-SKIP-CHK-SEC
      *
               IF    ( FLG-SKIP        =   ZERO )
      *
                   COMPUTE IDXL =   IDXL +   1
                   MOVE    IDXL        TO  SPA-GMN-CHKLST-MAX
      *
                   COMPUTE IDXN =   IDXN +   1
                   MOVE    IDXN        TO  SPA-GMN-TCHKLSTNUM  (IDXL)
                   MOVE    TNS-SRYCD   TO  SPA-GMN-TCHKTEKCD   (IDXL)
                   MOVE    TNS-NAME    TO  SPA-GMN-TCHKSRYNAME (IDXL)
                   IF    ( CHKMST-CHKERR   =   9 )
                       MOVE    "警告"  TO  SPA-GMN-TCOMPKBN (IDXL)
                   END-IF
      *
               END-IF
      *
               PERFORM 900-CHKMST-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chk"       TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( SPA-NAI-CHKLST-CHKKBN   =   "5" OR "A" OR "B" )
             AND ( SPA-NAI-SRYHEIMSTKBN    =   "1" )
               PERFORM 31011-CHKLST-ETENSU3-SEC
           ELSE
               IF    ( FLG-CHKMST  =   ZERO )
                   MOVE    1       TO  SPA-GMN-CHKLST-NEXT
               END-IF
           END-IF
      *
           .
       31011-CHKLST-CHKMST-EXT.
           EXIT.
      *****************************************************************
      *    対象外データチェック処理
      *****************************************************************
       31011-CHKMST-SKIP-CHK-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SKIP
      *
           IF    ( CHKMST-CHKERR   =   8 )
               MOVE    1           TO  FLG-SKIP
               IF    ( TSKIP-MAX   <   CONST-TSKIP-MAX )
                   COMPUTE TSKIP-MAX = TSKIP-MAX +   1
                   MOVE    CHKMST-CD   TO  TSKIP-CD (TSKIP-MAX)
               END-IF
           END-IF
      *
           IF    ( FLG-ERROR   NOT =   ZERO )
               MOVE    1           TO  FLG-SKIP
           END-IF
      *
           .
       31011-CHKMST-SKIP-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタコラムリスト編集処理（電子点数表より）
      *****************************************************************
       31011-CHKLST-ETENSU3-SEC        SECTION.
      *
           MOVE    SPA-GMN-CHKLST-MAX  TO    IDXL
      *
           EVALUATE    SPA-NAI-CHKLST-CHKKBN
           WHEN    "5"
               MOVE    "tbl_etensu_3_2"    TO  WRK-TABLE
               MOVE    "key3"              TO  WRK-PATHNAME
           WHEN    "A"
               MOVE    "tbl_etensu_3_1"    TO  WRK-TABLE
               MOVE    "key3"              TO  WRK-PATHNAME
           WHEN    "B"
               MOVE    "tbl_etensu_3_3"    TO  WRK-TABLE
               MOVE    "key3"              TO  WRK-PATHNAME
           END-EVALUATE
      *
           MOVE    SPA-NAI-SRYYM       TO  WRK-KJNYMD
           MOVE    "01"                TO  WRK-KJNYMD(7:2)
           PERFORM 900-ETENSU3-SEL-SEC
      *
           PERFORM UNTIL ( FLG-ETENSU3 NOT =   ZERO )
                    OR   ( IDXN           >=   SPA-GMN-CHKLST-SKIP )
      *
               MOVE    ETENSU3-SRYCD2  TO  WRK-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    FLG-TENSU       TO  FLG-ERROR
      *
               PERFORM 31011-ETENSU3-SKIP-CHK-SEC
      *
               IF    ( FLG-SKIP =   ZERO )
                   COMPUTE IDXN =   IDXN +   1
                   MOVE    1           TO  SPA-NAI-CHKLST-ANOTHER
               END-IF
      *
               PERFORM 900-ETENSU3-FET-SEC
      *
           END-PERFORM
      *
           PERFORM UNTIL ( FLG-ETENSU3 NOT =   ZERO )
                    OR   ( SPA-GMN-CHKLST-MAX >=   CONST-CHKLST-MAX)
      *
               MOVE    ETENSU3-SRYCD2  TO  WRK-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    FLG-TENSU       TO  FLG-ERROR
      *
               PERFORM 31011-ETENSU3-SKIP-CHK-SEC
      *
               IF    ( FLG-SKIP =   ZERO )
      *
                   COMPUTE IDXL =   IDXL +   1
                   MOVE    IDXL        TO  SPA-GMN-CHKLST-MAX
      *
                   COMPUTE IDXN =   IDXN +   1
                   MOVE    IDXN        TO  SPA-GMN-TCHKLSTNUM  (IDXL)
                   MOVE    TNS-SRYCD   TO  SPA-GMN-TCHKTEKCD   (IDXL)
                   MOVE    TNS-NAME    TO  SPA-GMN-TCHKSRYNAME (IDXL)
                   IF    ( ETENSU3-TOKUREI     NOT =   ZERO )
                       MOVE    "特例条件あり"
                                       TO  SPA-GMN-TCOMPKBN (IDXL)
                   END-IF
                   MOVE    "2"         TO  SPA-GMN-TOTHERMST
                                                           (IDXL)
      *
               END-IF
      *
               PERFORM 900-ETENSU3-FET-SEC
      *
           END-PERFORM
      *
           MOVE    WRK-TABLE           TO   MCP-TABLE
           MOVE    WRK-PATHNAME        TO   MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-ETENSU3     =   ZERO )
               MOVE    1       TO  SPA-GMN-CHKLST-NEXT
           END-IF
      *
           .
       31011-CHKLST-ETENSU3-EXT.
           EXIT.
      *****************************************************************
      *    対象外データチェック処理
      *****************************************************************
       31011-ETENSU3-SKIP-CHK-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SKIP
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TSKIP-MAX )
                    OR   ( FLG-SKIP    NOT =   ZERO )
               IF    ( TSKIP-CD (IDX0) =   ETENSU3-SRYCD2 )
                   MOVE    1       TO  FLG-SKIP
               END-IF
           END-PERFORM
      *
           IF    ( FLG-ERROR   NOT =   ZERO )
               MOVE    1           TO  FLG-SKIP
           END-IF
      *
           .
       31011-ETENSU3-SKIP-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３コラムリスト編集処理
      *****************************************************************
       31011-CHKLST-CHKTRD-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDXL
      *
           MOVE    1                   TO  FLG-CHKLST-ROW
      *
           MOVE    SPA-NAI-SRYYM       TO  WRK-KJNYMD
           MOVE    "01"                TO  WRK-KJNYMD(7:2)
           PERFORM 900-CHKTRD-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKTRD  NOT =   ZERO )
                    OR   ( IDXN           >=   SPA-GMN-CHKLST-SKIP )
               MOVE    CHKTRD-CD       TO  WRK-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    FLG-TENSU       TO  FLG-ERROR
      *
               PERFORM 31011-CHKTRD-SKIP-CHK-SEC
      *
               IF    ( FLG-SKIP        =   ZERO )
                   COMPUTE IDXN  = IDXN  + 1
               END-IF
               PERFORM 900-CHKTRD-KEY2-FET-SEC
           END-PERFORM
      *
           PERFORM UNTIL ( FLG-CHKTRD  NOT =   ZERO )
                    OR   ( SPA-GMN-CHKLST-MAX >=   CONST-CHKLST-MAX)
      *
               MOVE    CHKTRD-CD       TO  WRK-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    FLG-TENSU       TO  FLG-ERROR
      *
               PERFORM 31011-CHKTRD-SKIP-CHK-SEC
      *
               IF    ( FLG-SKIP        =   ZERO )
      *
                   COMPUTE IDXL =   IDXL +   1
                   MOVE    IDXL        TO  SPA-GMN-CHKLST-MAX
      *
                   COMPUTE IDXN =   IDXN +   1
                   MOVE    IDXN         TO  SPA-GMN-TCHKLSTNUM  (IDXL)
                   MOVE    TNS-NAME    TO  SPA-GMN-TCHKSRYNAME (IDXL)
      *
                   IF    ( CHKTRD-DAYMONTHKBN  =   "1" )
                       MOVE    "同月"  TO  SPA-GMN-TDAYMONTHKBN (IDXL)
                   ELSE
                       MOVE    "同日"  TO  SPA-GMN-TDAYMONTHKBN (IDXL)
                   END-IF
               END-IF
      *
               PERFORM 900-CHKTRD-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chktrd"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           DISPLAY "SPA-NAI-CHKLST-CHKKBN=" SPA-NAI-CHKLST-CHKKBN
           DISPLAY "SPA-NAI-SRYMATERKKBN =" SPA-NAI-SRYMATERKKBN
           IF    ( SPA-NAI-CHKLST-CHKKBN   =   "7" )
             AND ( SPA-NAI-SRYMATERKKBN    =   "1" )
               PERFORM 31011-CHKLST-MATERK-SEC
           ELSE
               IF    ( FLG-CHKTRD  =   ZERO )
                   MOVE    1       TO  SPA-GMN-CHKLST-NEXT
               END-IF
           END-IF
      *
           .
       31011-CHKLST-CHKTRD-EXT.
           EXIT.
      *****************************************************************
      *    対象外データチェック処理
      *****************************************************************
       31011-CHKTRD-SKIP-CHK-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SKIP
      *
           IF    ( FLG-ERROR   NOT =   ZERO )
               MOVE    1           TO  FLG-SKIP
           END-IF
      *
           .
       31011-CHKTRD-SKIP-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタコラムリスト編集処理（電子点数表より）
      *****************************************************************
       31011-CHKLST-MATERK-SEC        SECTION.
      *
           MOVE    SPA-GMN-CHKLST-MAX  TO    IDXL
      *
           PERFORM 900-MATERIAL-K-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-MATERK NOT =   ZERO )
                    OR   ( IDXN        >=   SPA-GMN-CHKLST-SKIP )
      *
               MOVE    MATERK-SCD      TO  WRK-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    FLG-TENSU       TO  FLG-ERROR
      *
               PERFORM 31011-MATERK-SKIP-CHK-SEC
      *
               IF    ( FLG-SKIP =   ZERO )
                   COMPUTE IDXN =   IDXN +   1
                   MOVE    1           TO  SPA-NAI-CHKLST-ANOTHER
               END-IF
      *
               PERFORM 900-MATERIAL-K-KEY2-FET-SEC
      *
           END-PERFORM
      *
           DISPLAY "FLG-MATERK =" FLG-MATERK 
           PERFORM UNTIL ( FLG-MATERK NOT =   ZERO )
                    OR   ( SPA-GMN-CHKLST-MAX >=   CONST-CHKLST-MAX)
      *
               MOVE    MATERK-SCD      TO  WRK-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    FLG-TENSU       TO  FLG-ERROR
      *
               PERFORM 31011-MATERK-SKIP-CHK-SEC
               DISPLAY "2FLG-SKIP=" FLG-SKIP
      *
               IF    ( FLG-SKIP =   ZERO )
      *
                   COMPUTE IDXL =   IDXL +   1
                   MOVE    IDXL         TO  SPA-GMN-CHKLST-MAX
      *
                   COMPUTE IDXN =   IDXN +   1
                   MOVE    IDXN        TO  SPA-GMN-TCHKLSTNUM  (IDXL)
                   MOVE    TNS-NAME    TO  SPA-GMN-TCHKSRYNAME (IDXL)
                   MOVE    "同月"      TO  SPA-GMN-TDAYMONTHKBN (IDXL)
                   MOVE    "3"         TO  SPA-GMN-TOTHERMST  (IDXL)
      *
               END-IF
      *
               PERFORM 900-MATERIAL-K-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_material_k"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-MATERK     =   ZERO )
               MOVE    1       TO  SPA-GMN-CHKLST-NEXT
           END-IF
      *
           .
       31011-CHKLST-MATERK-EXT.
           EXIT.
      *****************************************************************
      *    対象外データチェック処理
      *****************************************************************
       31011-MATERK-SKIP-CHK-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SKIP
      *
           IF    ( FLG-ERROR   NOT =   ZERO )
               MOVE    1           TO  FLG-SKIP
           END-IF
      *
           .
       31011-MATERK-SKIP-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５コラムリスト編集処理
      *****************************************************************
       31011-CHKLST-CHK005-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDXL
      *
           MOVE    1                   TO  FLG-CHKLST-ROW
      *
           MOVE    SPA-NAI-SRYYM       TO  WRK-KJNYMD
           MOVE    "01"                TO  WRK-KJNYMD(7:2)
           PERFORM 900-CHK005-KEY11-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHK005  NOT =   ZERO )
                    OR   ( IDXN           >=   SPA-GMN-CHKLST-SKIP )
               MOVE    CHK005-SRYCD    TO  WRK-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    FLG-TENSU       TO  FLG-ERROR
      *
               PERFORM 31011-CHK005-SKIP-CHK-SEC
      *
               IF    ( FLG-SKIP        =   ZERO )
                   COMPUTE IDXN  = IDXN  + 1
               END-IF
               PERFORM 900-CHK005-KEY11-FET-SEC
           END-PERFORM
      *
           PERFORM UNTIL ( FLG-CHK005  NOT =   ZERO )
                    OR   ( SPA-GMN-CHKLST-MAX >=   CONST-CHKLST-MAX)
      *
               MOVE    CHK005-SRYCD    TO  WRK-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    FLG-TENSU       TO  FLG-ERROR
      *
               PERFORM 31011-CHK005-SKIP-CHK-SEC
      *
               IF    ( FLG-SKIP        =   ZERO )
      *
                   COMPUTE IDXL =   IDXL +   1
                   MOVE    IDXL        TO  SPA-GMN-CHKLST-MAX
      *
                   COMPUTE IDXN =   IDXN +   1
                   MOVE    IDXN        TO  SPA-GMN-TCHKLSTNUM  (IDXL)
                   MOVE    TNS-NAME    TO  SPA-GMN-TCHKSRYNAME (IDXL)
      *
                   IF    ( CHK005-DAYMONTHKBN  =   "1" )
                       MOVE    "同月"  TO  SPA-GMN-TDAYMONTHKBN (IDXL)
                   ELSE
                       MOVE    "同日"  TO  SPA-GMN-TDAYMONTHKBN (IDXL)
                   END-IF
               END-IF
      *
               PERFORM 900-CHK005-KEY11-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chk005"    TO  MCP-TABLE
           MOVE    "key11"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-CHK005      =   ZERO )
               MOVE    1       TO  SPA-GMN-CHKLST-NEXT
           END-IF
      *
           .
       31011-CHKLST-CHK005-EXT.
           EXIT.
      *****************************************************************
      *    対象外データチェック処理
      *****************************************************************
       31011-CHK005-SKIP-CHK-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SKIP
      *
           IF    ( FLG-ERROR   NOT =   ZERO )
               MOVE    1           TO  FLG-SKIP
           END-IF
      *
           .
       31011-CHK005-SKIP-CHK-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＤＩＤ１）ＯＫ処理
      *****************************************************************
       330-DID1-SET-SEC            SECTION.
      *
      *    IF    ( SPA-DID1-FLG    =   "OK" )
      *        EVALUATE    SPA-DIDCD
      *        WHEN    "6666"
      *            MOVE    "JOIN"      TO  MCP-PUTTYPE
      *            PERFORM 210-BACK-SYORI-SEC
      *        WHEN    OTHER
      *        END-EVALUATE
      *    END-IF
      *
           MOVE    SPACE               TO  SPA-DID1-FLG
           MOVE    SPACE               TO  SPA-DIDCD
      *
           .
       330-DID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
           WHEN    "CLICKED"       ALSO    "B01"
      *        戻る
               MOVE    SPACE           TO  SPA-DIDCD
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM 210-BACK
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 220-CLEAR-SEC
           WHEN    "CLICKED"       ALSO    "B04"
               PERFORM 240-D05-SENI-SEC
           WHEN    "CLICKED"       ALSO    "B06"
               PERFORM 260-PREVPAGE-SEC
           WHEN    "CLICKED"       ALSO    "B06C"
               PERFORM 261-PREVPAGE-SEC
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 270-NEXTPAGE-SEC
           WHEN    "CLICKED"       ALSO    "B07C"
               PERFORM 271-NEXTPAGE-SEC
           WHEN    "CLICKED"       ALSO    "B08"
               PERFORM 280-C02-SENI-SEC
           WHEN    "CLICKED"       ALSO    "B09"
               PERFORM 290-X01-SENI-SEC
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 412-REIGAI-SEC
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
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
           WHEN    "ACTIVATE"      ALSO    "CHKNUM"
               PERFORM 4100-CHKNUM-CHK-SEC
           WHEN    "ACTIVATE"      ALSO    "SELNUM"
               PERFORM 4100-SELNUM-CHK-SEC
           WHEN    "ACTIVATE"      ALSO    "BYOSELNUM"
               PERFORM 4100-BYOSELNUM-CHK-SEC
           WHEN    "ACTIVATE"      ALSO    "CHKSELNUM"
               IF    ( SPA-NAI-CHKKBN      =   "1" OR "2" OR "6" )
                   PERFORM 4100-CHKSELNUM-CHK-SEC
               END-IF
           WHEN    ANY             ALSO    "ERRLST"
               PERFORM 4100-ERRLST-CHK-SEC
           WHEN    ANY             ALSO    "BYOLST"
               PERFORM 4100-BYOLST-CHK-SEC
           WHEN    ANY             ALSO    "CHKLST"
               IF    ( SPA-NAI-CHKKBN      =   "1" OR "2" OR "6" )
                   PERFORM 4100-CHKLST-CHK-SEC
               END-IF
           WHEN    OTHER
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC           SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
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
           MOVE    D04-BEHAVE      TO  SPA-GMN-BEHAVE-G
           MOVE    D04-BYOMEI      TO  SPA-GMN-BYOMEI
           MOVE    D04-HOSOKU      TO  SPA-GMN-HOSOKU
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
               PERFORM 4101-SELNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-BEHAVE-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-BYOSELNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-BYOMEI-CHK-SEC
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック区分チェック処理
      *****************************************************************
       4100-CHKNUM-CHK-SEC             SECTION.
      *
           MOVE    D04-CHKNUM      TO  SPA-GMN-CHKNUM-G
      *
           PERFORM 4101-CHKNUM-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    1           TO  SPA-GMN-ERRLST-PAGE
               PERFORM 3101-ERRLST-SEC
           END-IF
      *
           .
      *
       4100-CHKNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  SPA-GMN-ERRLST-SEL
      *
           MOVE    D04-SELNUM          TO  SPA-GMN-ERRLST-SELNUM
      *
           PERFORM 4101-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( WRK-ERRLST-SEL  >   ZERO )
                   MOVE    WRK-ERRLST-SEL  TO  SPA-GMN-ERRLST-SEL
                   PERFORM 4110-PTINF-EDIT-SEC
                   MOVE    3           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4101-SELNUM-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  WRK-ERRLST-SEL
      *
           IF    ( SPA-GMN-ERRLST-SELNUM  >   ZERO )
               PERFORM VARYING IDXL     FROM    1   BY  1
                       UNTIL ( IDXL     >   SPA-GMN-ERRLST-MAX )
                        OR   ( WRK-ERRLST-SEL  >   ZERO )
      *
                   IF    ( SPA-GMN-ERRLST-SELNUM
                                       =   SPA-GMN-TNUM (IDXL))
                       MOVE    IDXL
                                       TO  WRK-ERRLST-SEL
                   END-IF
      *
               END-PERFORM
      *
               IF    ( WRK-ERRLST-SEL  =   ZERO )
                   MOVE    "0003"      TO  SPA-ERRCD
                   MOVE    2           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       4101-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名選択番号チェック処理
      *****************************************************************
       4100-BYOSELNUM-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  SPA-GMN-BYOLST-SEL
      *
           MOVE    D04-BYOSELNUM       TO  SPA-GMN-BYOLST-SELNUM
      *
           PERFORM 4101-BYOSELNUM-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( WRK-BYOLST-SEL  >   ZERO )
                   MOVE    WRK-BYOLST-SEL  TO  SPA-GMN-BYOLST-SEL
                   MOVE    SPA-GMN-TBYOMEI (SPA-GMN-BYOLST-SEL)
                                       TO  SPA-GMN-BYOMEI
                   MOVE    SPA-GMN-TBYOHOSOKU (SPA-GMN-BYOLST-SEL)
                                       TO  SPA-GMN-HOSOKU
                   MOVE    5           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       4100-BYOSELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名選択番号チェック処理
      *****************************************************************
       4101-BYOSELNUM-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  WRK-BYOLST-SEL
      *
           IF    ( SPA-GMN-BYOLST-SELNUM  >   ZERO )
      *
               IF    ( SPA-GMN-BYOLST-SELNUM
                                       <= SPA-GMN-BYOLST-MAX )
                   MOVE    SPA-GMN-BYOLST-SELNUM
                                       TO  WRK-BYOLST-SEL
               ELSE
                   MOVE    "0004"      TO  SPA-ERRCD
                   MOVE    4           TO  SPA-GMN-CUR
               END-IF
               MOVE    ZERO            TO  SPA-GMN-CHKLST-SELNUM
               MOVE    ZERO            TO  SPA-GMN-CHKLST-SEL
           END-IF
      *
           .
      *
       4101-BYOSELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ病名選択番号チェック処理
      *****************************************************************
       4100-CHKSELNUM-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  SPA-GMN-CHKLST-SEL
      *
           MOVE    D04-CHKSELNUM       TO  SPA-GMN-CHKLST-SELNUM
      *
           PERFORM 4101-CHKSELNUM-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( WRK-CHKLST-SEL  >   ZERO )
                   MOVE    WRK-CHKLST-SEL  TO  SPA-GMN-CHKLST-SEL
                   MOVE    SPA-GMN-TCHKBYOMEI (SPA-GMN-CHKLST-SEL)
                                       TO  SPA-GMN-BYOMEI
                   MOVE    SPA-GMN-TCHKHOSOKU (SPA-GMN-CHKLST-SEL)
                                       TO  SPA-GMN-HOSOKU
                   MOVE    5           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       4100-CHKSELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ病名選択番号チェック処理
      *****************************************************************
       4101-CHKSELNUM-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  WRK-CHKLST-SEL
      *
           IF    ( SPA-GMN-CHKLST-SELNUM  >   ZERO )
               PERFORM VARYING IDXM    FROM   1   BY   1
                       UNTIL ( IDXM    >   SPA-GMN-CHKLST-MAX )
                        OR   ( WRK-CHKLST-SEL
                                   NOT =   ZERO )
      *
                   IF    ( SPA-GMN-TCHKLSTNUM (IDXM)
                                   =   SPA-GMN-CHKLST-SELNUM )
                       MOVE    IDXM    TO  WRK-CHKLST-SEL
                   END-IF
      *
               END-PERFORM
      *
               IF    ( WRK-CHKLST-SEL  =   ZERO )
                   MOVE    "0005"      TO  SPA-ERRCD
                   MOVE    6           TO  SPA-GMN-CUR
               END-IF
               MOVE    ZERO            TO  SPA-GMN-BYOLST-SELNUM
               MOVE    ZERO            TO  SPA-GMN-BYOLST-SEL
           END-IF
      *
           .
      *
       4101-CHKSELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック内容リストチェック処理
      *****************************************************************
       4100-ERRLST-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-ERRLST-SEL
      *
           PERFORM VARYING IDXL FROM    1  BY  1
                   UNTIL ( IDXL >   SPA-GMN-ERRLST-MAX )
                    OR   ( SPA-GMN-ERRLST-SEL  >   ZERO )
      *
               IF    ( D04-ERRLST-SELECT (IDXL)    =   "T")
                   MOVE    IDXL        TO  SPA-GMN-ERRLST-SEL
                   MOVE    SPA-GMN-TNUM (IDXL)
                                       TO  D04-SELNUM
                   MOVE    3           TO  SPA-GMN-CUR
                   PERFORM 4100-SELNUM-CHK-SEC
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4100-ERRLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者病名リストチェック処理
      *****************************************************************
       4100-BYOLST-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-BYOLST-SEL
      *
           PERFORM VARYING IDXL FROM    1  BY  1
                   UNTIL ( IDXL >   SPA-GMN-BYOLST-MAX )
                    OR   ( SPA-GMN-BYOLST-SEL  >   ZERO )
      *
               IF    ( D04-BYOLST-SELECT (IDXL)    =   "T")
                   MOVE    IDXL        TO  SPA-GMN-BYOLST-SEL
                                           D04-BYOSELNUM
                   MOVE    5           TO  SPA-GMN-CUR
                   PERFORM 4100-BYOSELNUM-CHK-SEC
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4100-BYOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタリストチェック処理
      *****************************************************************
       4100-CHKLST-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-CHKLST-SEL
      *
           PERFORM VARYING IDXL FROM   1   BY  1
                   UNTIL ( IDXL >   D04-CHKLST-COUNT )
                    OR   ( SPA-GMN-CHKLST-SEL  >   ZERO )
      *
               IF    ( D04-CHKLST-SELECT (IDXL)    =   "T")
                   IF    ( D04-TCHKLSTNO (IDXL)    =   SPACE )
                       CONTINUE
                   ELSE
                       INITIALIZE          ORCSNUMAREA
                       MOVE    D04-TCHKLSTNO (IDXL)
                                       TO  SNUM-INX
                       CALL    "ORCSNUM"   USING
                                           ORCSNUMAREA
                       MOVE    SNUM-OUTNUM TO  D04-CHKSELNUM
                       MOVE    6       TO  SPA-GMN-CUR
                       PERFORM 4100-CHKSELNUM-CHK-SEC
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4100-CHKLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック区分チェック処理
      *****************************************************************
       4101-CHKNUM-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-CHKNUM-G    =   SPACE )
                   MOVE    "0001"      TO  SPA-ERRCD
           ELSE
               MOVE    SPA-GMN-CHKNUM  TO  WRK-CMB-CD
               PERFORM 4200-CHKNUMLST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE   1            TO  SPA-GMN-CUR
               ELSE
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-CHKNUM-G
               END-IF
           END-IF
      *
      *
           .
      *
       4101-CHKNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象外区分チェック処理
      *****************************************************************
       4101-BEHAVE-CHK-SEC             SECTION.
      *
           IF    ( SPA-NAI-REIGAICHKKBN    =   "1" )
               MOVE    SPA-GMN-BEHAVE  TO  WRK-CMB-CD
               PERFORM 4200-BEHAVELST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   IF    ( SPA-NAI-REIGAI  =   "1" OR "2" )
                       MOVE    "0"         TO  WRK-CMB-CD
                       PERFORM 4200-BEHAVELST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-BEHAVE-G
                   ELSE
                       MOVE    SPACE   TO  SPA-GMN-BEHAVE-G
                   END-IF
               ELSE
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-BEHAVE-G
               END-IF
           END-IF
      *
           .
      *
       4101-BEHAVE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名チェック処理
      *****************************************************************
       4101-BYOMEI-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-BYOMEI  NOT =   SPACE )
               INITIALIZE   ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-BYOMEI      TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING
                                       ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-BYOMEI
               IF    ( KANACHK-RC          =   ZERO )
                AND  ( KANACHK-SYUBETU     =   "2" )
                   CONTINUE
               ELSE
                   MOVE    "0006"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *
           .
      *
       4101-BYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    補足コメントチェック処理
      *****************************************************************
       4101-HOSOKU-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-HOSOKU  NOT =   SPACE )
               INITIALIZE   ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-HOSOKU      TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING
                                       ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-HOSOKU
               IF    ( KANACHK-RC          =   ZERO )
                AND  ( KANACHK-SYUBETU     =   "2" )
                   CONTINUE
               ELSE
                   MOVE    "0009"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       4101-HOSOKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       4110-PTINF-EDIT-SEC             SECTION.
      *
           PERFORM 41101-PTERR-EDIT-SEC
      *
           PERFORM 3101-BYOLST-SEC
      *
           PERFORM 3101-CHKLST-SEC
      *
           .
      *
       4110-PTINF-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       41101-PTERR-EDIT-SEC            SECTION.
      *
           INITIALIZE                  SPA-GMN-PTINF-AREA
                                       SPA-NAI-PTINF-AREA
                                       SPA-GMN-PTINFERR-AREA
                                       SPA-NAI-PTINFERR-AREA
      *
           MOVE    SPA-GMN-ERRLST-SEL      TO  IDXL
      *
           MOVE    SPA-NAI-TPTID (IDXL)    TO  WRK-PTID
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           INITIALIZE                      CHK004-REC
           MOVE    "1201"              TO  CHK004-KANRICD
           MOVE    SPA-NAI-TKBNCD(IDXL)
                                       TO  CHK004-KBNCD
           MOVE    SPA-SYSYMD          TO  CHK004-STYUKYMD
                                           CHK004-EDYUKYMD
           MOVE    SPA-NAI-TCK4-1201 (IDXL)
                                       TO  CHK004-KANRITBL
           MOVE    CHK004-REC          TO  CK4-1201-REC
      *
           MOVE    SPA-GMN-ERRLST-SEL  TO  IDXL
      *
           MOVE    SPA-NAI-TPTID  (IDXL)   TO  SPA-NAI-PTID
           MOVE    SPA-GMN-TPTNUM (IDXL)   TO  SPA-GMN-PTNUM
           MOVE    SPA-GMN-TNAME  (IDXL)   TO  SPA-GMN-NAME
           EVALUATE    PTINF-SEX
           WHEN    "1"
               MOVE    "男"    TO  SPA-GMN-SEX
           WHEN    "2"
               MOVE    "女"    TO  SPA-GMN-SEX
           END-EVALUATE
           MOVE    PTINF-BIRTHDAY          TO  SPA-NAI-BIRTHDAY
           MOVE    PTINF-BIRTHDAY          TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD1 TO  SPA-GMN-BIRTHDAY
      *
           MOVE    SPA-NAI-TCHKNUM(IDXL)
                                       TO  SPA-NAI-CHKNUM
           MOVE    CK4-1201-SRYCD      TO  SPA-GMN-SRYCD
           MOVE    CK4-1201-SRYKA      TO  SPA-NAI-SRYKA
           MOVE    CK4-1201-BYOMEI     TO  SPA-GMN-CBYOMEI
      *
           IF    ( CK4-1201-SRYCD(1:1) =   "S" OR "P" )
               MOVE    CK4-1201-SRYCD      TO  WRK-SRYCD
               PERFORM 900-INPUTCD-KEY2-SEL-SEC
               MOVE    ICD-DSPNAME         TO  SPA-GMN-SRYNAME
           ELSE
               MOVE    CK4-1201-SRYCD      TO  WRK-SRYCD
               MOVE    CK4-1201-LASTYMD    TO  WRK-KJNYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    TNS-NAME            TO  SPA-GMN-SRYNAME
           END-IF
           MOVE    SPA-NAI-TERRCD (IDXL)
                                       TO  SPA-GMN-ERRCD
           MOVE    SPA-GMN-TERRMSG(IDXL)
                                       TO  SPA-GMN-ERRMSG
           MOVE    SPACE               TO  SPA-NAI-ERRMSG
           PERFORM VARYING IDXE    FROM    1   BY  1
                   UNTIL ( IDXE    >   ERRMES-MAX )
                    OR   ( SPA-NAI-ERRMSG  NOT =   SPACE )
      *
               IF    ( SPA-GMN-ERRCD   =   ERRMES-CD (IDXE))
                   MOVE    ERRMES-MESSAGE (IDXE)
                                       TO  SPA-NAI-ERRMSG
                                           SPA-GMN-ERRMSG
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-NAI-TCHKKBN(IDXL)
                                       TO  SPA-NAI-CHKKBN
      *
           EVALUATE    SPA-NAI-CHKKBN
           WHEN    "4"
           WHEN    "5"
           WHEN    "A"
           WHEN    "B"
               MOVE    CK4-1201-TEKCD      TO  SPA-GMN-TEKCD
                                               WRK-SRYCD
               MOVE    CK4-1201-LASTYMD    TO  WRK-KJNYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    TNS-NAME            TO  SPA-GMN-TEKNAME
           WHEN    "6"
               MOVE    CK4-1201-TEKBYOMEI  TO  SPA-GMN-TEKBYOMEI
               MOVE    CK4-1201-TEKCOMPKBN TO  SPA-GMN-TEKCOMPKBN
           END-EVALUATE
      *
           IF    ( SPA-NAI-CHKNUM      =   "20" )
               MOVE    CK4-1201-TANICD     TO  SPA-GMN-TANICD
               MOVE    CK4-1201-LASTYMD    TO  WRK-KJNYMD
               MOVE    CK4-1201-TANINAME   TO  SPA-GMN-TANINAME
               MOVE    SPACE               TO  WRK-LTRIM
               MOVE    CK4-1201-LIMIT      TO  WRK-LTRIM-ZX99
               PERFORM 800-LTRIM-SEC
               MOVE    WRK-LTRIM           TO  WRK-KANACHK-STR
               PERFORM 800-ZENKAKU-SEC
               MOVE    WRK-KANACHK-STR     TO  SPA-GMN-LIMITMEI
               MOVE    CK4-1201-SHONUM     TO  SPA-NAI-SHONUM
               MOVE    CK4-1201-SRYSTYMD   TO  SPA-NAI-SRYSTYMD
               MOVE    CK4-1201-SRYEDYMD   TO  SPA-NAI-SRYEDYMD
      *
           END-IF
      *
           MOVE    SPA-GMN-TREIGAI(IDXL)
                                       TO  SPA-NAI-REIGAI
      *
           IF    ( SPA-NAI-REIGAI      =   "1" OR "2" )
               PERFORM 3102-BEHAVELST-SEC
           ELSE
               PERFORM 3101-BEHAVELST-SEC
           END-IF
      *
           MOVE    SPA-NAI-REIGAI      TO  WRK-CMB-CD
           PERFORM 4200-BEHAVELST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-BEHAVE-G
      *
           .
      *
       41101-PTERR-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "D01"               TO  SPA-SAKIPG
           MOVE    "D04"               TO  SPA-MOTOPG
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
      *
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       220-CLEAR-SEC                   SECTION.
      *
           PERFORM 310-SPASET-SEC
      *
           .
       220-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    例外登録一覧画面遷移処理
      *****************************************************************
       240-D05-SENI-SEC                SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2401-D05-SENI-SEC
           END-IF
      *
           .
       240-D05-SENI-EXT.
           EXIT.
      *****************************************************************
      *    例外登録一覧画面遷移処理
      *****************************************************************
       2401-D05-SENI-SEC               SECTION.
      *
      *    ＳＰＡの内容を退避
           PERFORM 800-SAVE-D04SPA-SEC
      *
           INITIALIZE                      SPA-D04-INF-AREA
           MOVE    SPA-NAI-SRYYM       TO  SPA-D04-SRYYM
           MOVE    SPA-NAI-NYUGAIKBN   TO  SPA-D04-NYUGAIKBN
      *
           MOVE    "D04"               TO  SPA-MOTOPG
           MOVE    "D05"               TO  SPA-SAKIPG
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE.
           MOVE    "D05    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       2401-D05-SENI-EXT.
           EXIT.
      *****************************************************************
      *    前頁処理
      *****************************************************************
       260-PREVPAGE-SEC                SECTION.
      *
           IF    ( SPA-GMN-ERRLST-PAGE >   1 )
      *
               INITIALIZE              SPA-GMN-ERRLST-AREA
                                       SPA-NAI-ERRLST-AREA
      *
               COMPUTE SPA-GMN-ERRLST-PAGE
                   =   SPA-GMN-ERRLST-PAGE     -   1
      *
               PERFORM 3101-ERRLST-SEC
           END-IF
      *
           .
       260-PREVPAGE-EXT.
           EXIT.
      *****************************************************************
      *    次頁処理
      *****************************************************************
       270-NEXTPAGE-SEC                SECTION.
      *
           IF    ( SPA-GMN-ERRLST-NEXT  =   1 )
      *
               INITIALIZE              SPA-GMN-ERRLST-AREA
                                       SPA-NAI-ERRLST-AREA
      *
               COMPUTE SPA-GMN-ERRLST-PAGE
                   =   SPA-GMN-ERRLST-PAGE     +   1
      *
               PERFORM 3101-ERRLST-SEC
           END-IF
      *
           .
       270-NEXTPAGE-EXT.
           EXIT.
      *****************************************************************
      *    前頁処理
      *****************************************************************
       261-PREVPAGE-SEC                SECTION.
      *
           IF    ( SPA-GMN-CHKLST-PAGE >   1 )
      *
               COMPUTE SPA-GMN-CHKLST-PAGE
                   =   SPA-GMN-CHKLST-PAGE     -   1
      *
               PERFORM 31011-CHKLST-SEC
           END-IF
      *
           .
       261-PREVPAGE-EXT.
           EXIT.
      *****************************************************************
      *    次頁処理
      *****************************************************************
       271-NEXTPAGE-SEC                SECTION.
      *
           IF    ( SPA-GMN-CHKLST-NEXT  =   1 )
      *
               COMPUTE SPA-GMN-CHKLST-PAGE
                   =   SPA-GMN-CHKLST-PAGE     +   1
      *
               PERFORM 31011-CHKLST-SEC
           END-IF
      *
           .
       271-NEXTPAGE-EXT.
           EXIT.
      *****************************************************************
      *    病名登録画面遷移処理
      *****************************************************************
       280-C02-SENI-SEC                SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
      *
               IF    ( SPA-GMN-HOSOKU  NOT =   SPACE )
                   INITIALIZE   ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI
                   MOVE    SPA-GMN-HOSOKU      TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING
                                           ORCSKANACHKAREA
                   MOVE    KANACHK-MAX         TO  IDX0
                   IF    ( IDX0                <   2 )
                       MOVE    1               TO  IDX0
                   ELSE
                       COMPUTE IDX0 = IDX0 - 1
                   END-IF
                   IF    ( SPA-GMN-HOSOKU (1:2)    =   "（" )
                     OR  ( SPA-GMN-HOSOKU (IDX0:2) =   "）" )
                      MOVE    "0010"           TO  SPA-ERRCD
                   END-IF
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   PERFORM 2801-C02-SENI-SEC
               END-IF
      *
           END-IF
      *
           .
       280-C02-SENI-EXT.
           EXIT.
      *****************************************************************
      *    病名登録画面遷移処理
      *****************************************************************
       2801-C02-SENI-SEC                SECTION.
      *
      *    ＳＰＡの内容を退避
           PERFORM 800-SAVE-D04SPA-SEC
      *
           MOVE    SPA-AREA            TO  SPA-AREA
           INITIALIZE                      SPA-KYOTUKEY
           MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
           MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
      *
           IF    ( SPA-NAI-PTID    =   ZERO )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-PTNUM   TO  SPA-PTNUM
               MOVE    SPA-NAI-PTID    TO  SPA-PTID
               MOVE    SPA-NAI-SRYKA   TO  SPA-SRYKA
               MOVE    SPA-GMN-NAME    TO  SPA-NAME
           END-IF
      *
           MOVE    "D04"               TO  SPA-MOTOPG
           MOVE    "C02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      LNK-D04-AREA
           MOVE    SPA-GMN-BYOMEI      TO  LNK-D04-BYOMEI
           MOVE    SPA-GMN-HOSOKU      TO  LNK-D04-HOSOKU-COMMENT
           MOVE    LNK-D04-AREA        TO  SPA-WORK
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "C02"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       2801-C02-SENI-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ登録画面遷移処理
      *****************************************************************
       290-X01-SENI-SEC                SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-SRYCD       =   SPACE )
                AND  ( SPA-GMN-CBYOMEI     =   SPACE )
                   MOVE    "0007"      TO  SPA-ERRCD
                   MOVE    2           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2901-X01-SENI-SEC
           END-IF
      *
           .
       290-X01-SENI-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ登録画面遷移処理
      *****************************************************************
       2901-X01-SENI-SEC               SECTION.
      *
      *    ＳＰＡの内容を退避
           PERFORM 800-SAVE-D04SPA-SEC
      *
           MOVE    "D04"               TO  SPA-MOTOPG
           MOVE    "X01"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPA-AREA            TO  WKSPA-AREA
      *
           INITIALIZE                      SENDX01-AREA
           MOVE     "D04"              TO  SENDX01-MOTOPG
           MOVE     SPA-GMN-SRYCD      TO  SENDX01-SRYCD
           MOVE     SPA-NAI-SRYYM      TO  SENDX01-SRYYMD
           MOVE     "01"               TO  SENDX01-SRYYMD(7:2)
           MOVE     SPA-NAI-CHKKBN     TO  SENDX01-CHKKBN
           EVALUATE SPA-NAI-CHKKBN
           WHEN     "1"
           WHEN     "2"
           WHEN     "6"
               MOVE     SPA-GMN-BYOMEI TO  SENDX01-BYOMEI
               IF     ( SPA-NAI-CHKKBN =   "1" OR "2" )
                   MOVE    SPA-GMN-HOSOKU
                                       TO  SENDX01-HOSOKU
               END-IF
           WHEN     "8"
           WHEN     "9"
               MOVE     SPA-GMN-CBYOMEI TO SENDX01-BYOMEI
           END-EVALUATE
      *
           MOVE     SENDX01-AREA       TO  WKSPA-WORK
           MOVE     WKSPA-KYOUTU       TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "X01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       2901-X01-SENI-EXT.
           EXIT.
      *****************************************************************
      *    例外登録処理
      *****************************************************************
       412-REIGAI-SEC                 SECTION.
      *
           IF    ( SPA-GMN-ERRCD      =   SPACE )
               MOVE    "0007"     TO  SPA-ERRCD
               MOVE    2          TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD      =   SPACE )
               IF    ( SPA-GMN-BEHAVE  =   SPACE )
                   MOVE    "0008" TO  SPA-ERRCD
                   MOVE    3      TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD      =   SPACE )
               PERFORM  4121-REIGAI-SEC
           END-IF
      *
           .
      *
       412-REIGAI-EXT.
           EXIT.
      *****************************************************************
      *    例外処理
      *****************************************************************
       4121-REIGAI-SEC                 SECTION.
      *
           INITIALIZE                      CHKREIGAI-REC
           MOVE    SPA-HOSPNUM         TO  CHKREIGAI-HOSPNUM
           MOVE    SPA-NAI-PTID        TO  CHKREIGAI-PTID
           MOVE    SPA-NAI-CHKNUM      TO  CHKREIGAI-CHKNUM
           MOVE    SPA-NAI-NYUGAIKBN   TO  CHKREIGAI-NYUGAIKBN
           MOVE    SPA-NAI-CHKKBN      TO  CHKREIGAI-CHKKBN
           EVALUATE    SPA-NAI-CHKKBN
           WHEN    SPACE
               EVALUATE    TRUE
               WHEN  ( SPA-NAI-CHKNUM  =   "20" )
                AND  ( SPA-GMN-ERRCD   =   "200001" )
                   MOVE    SPA-GMN-TANICD
                                       TO  CHKREIGAI-TANICD
                   MOVE    SPA-GMN-SRYCD
                                       TO  CHKREIGAI-SRYCD
               WHEN  ( SPA-NAI-CHKNUM  =   "21" )
                   MOVE    SPA-GMN-SRYCD
                                       TO  CHKREIGAI-SRYCD
               WHEN    OTHER
                   MOVE    SPA-GMN-ERRCD
                                       TO  CHKREIGAI-ERRCD
               END-EVALUATE
           WHEN    "8"
           WHEN    "9"
               MOVE    SPA-GMN-CBYOMEI TO  CHKREIGAI-BYOMEI
           WHEN    "4"
           WHEN    "5"
           WHEN    "A"
           WHEN    "B"
               MOVE    SPA-GMN-SRYCD   TO  CHKREIGAI-SRYCD
               MOVE    SPA-GMN-TEKCD   TO  CHKREIGAI-TEKCD
           WHEN    "6"
               MOVE    SPA-GMN-SRYCD   TO  CHKREIGAI-SRYCD
               MOVE    SPA-GMN-TEKBYOMEI
                                       TO  CHKREIGAI-TEKBYOMEI
               MOVE    SPA-GMN-TEKCOMPKBN
                                       TO  CHKREIGAI-TEKCOMPKBN
           WHEN    OTHER
               MOVE    SPA-GMN-SRYCD   TO  CHKREIGAI-SRYCD
           END-EVALUATE
           IF    ( SPA-GMN-BEHAVE      =   "1" )
               MOVE    SPA-NAI-SRYYM       TO  CHKREIGAI-STYUKYMD
               MOVE    "01"                TO  CHKREIGAI-STYUKYMD(7:2)
               MOVE    CHKREIGAI-STYUKYMD  TO  CHKREIGAI-EDYUKYMD
           ELSE
               MOVE    "99999999"          TO  CHKREIGAI-STYUKYMD
               MOVE    "00000000"          TO  CHKREIGAI-EDYUKYMD
           END-IF
           MOVE    CHKREIGAI-REC       TO  MCPDATA-REC
           MOVE    "tbl_chkreigai"     TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC              =   ZERO )
               CONTINUE
           ELSE
               MOVE    "4001"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
            AND  ( SPA-GMN-BEHAVE      =   "1" OR "2" )
               PERFORM 900-CHKREIGAI-KEY4-SEL-SEC
               COMPUTE CNT-RENNUM  =   CHKREIGAI-RENNUM    +   1
      *
               INITIALIZE                       CHKREIGAI-REC
               MOVE    SPA-HOSPNUM         TO   CHKREIGAI-HOSPNUM
               MOVE    SPA-NAI-PTID        TO   CHKREIGAI-PTID
               MOVE    CNT-RENNUM          TO   CHKREIGAI-RENNUM
               MOVE    SPA-NAI-CHKNUM      TO   CHKREIGAI-CHKNUM
               MOVE    SPA-NAI-NYUGAIKBN   TO   CHKREIGAI-NYUGAIKBN
               MOVE    SPA-NAI-CHKKBN      TO   CHKREIGAI-CHKKBN
               MOVE    SPA-GMN-ERRCD       TO   CHKREIGAI-ERRCD
               IF    ( SPA-NAI-ERRMSG      =    SPACE )
                   MOVE    SPA-GMN-ERRMSG  TO   CHKREIGAI-ERRMSG
               END-IF
               MOVE    SPA-GMN-SRYCD       TO   CHKREIGAI-SRYCD
               MOVE    SPA-GMN-CBYOMEI     TO   CHKREIGAI-BYOMEI
               MOVE    SPA-GMN-TEKCD       TO   CHKREIGAI-TEKCD
               MOVE    SPA-GMN-TEKBYOMEI   TO   CHKREIGAI-TEKBYOMEI
               MOVE    SPA-GMN-TEKCOMPKBN  TO   CHKREIGAI-TEKCOMPKBN
               MOVE    SPA-GMN-TANICD      TO   CHKREIGAI-TANICD
               MOVE    SPA-GMN-TANINAME    TO   CHKREIGAI-TANINAME
               IF    ( SPA-GMN-BEHAVE      =    "1" )
                   MOVE    SPA-NAI-SRYYM   TO   CHKREIGAI-STYUKYMD
                   MOVE    "01"            TO   CHKREIGAI-STYUKYMD(7:2)
                   MOVE    CHKREIGAI-STYUKYMD   TO WRK-SYMD
                   PERFORM 800-GETUMATU-SEC
                   MOVE    WRK-SYMD        TO   CHKREIGAI-EDYUKYMD
               ELSE
                   MOVE    "00000000"      TO   CHKREIGAI-STYUKYMD
                   MOVE    "99999999"      TO   CHKREIGAI-EDYUKYMD
               END-IF
               MOVE    SMCNDATE-YMD        TO   CHKREIGAI-CREYMD
                                                CHKREIGAI-UPYMD
               MOVE    SMCNDATE-HMS        TO   CHKREIGAI-UPHMS
      *
               MOVE    CHKREIGAI-REC       TO  MCPDATA-REC
               MOVE    "tbl_chkreigai"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC              =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    "4002"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 3101-ERRLST-SEC
      *
               INITIALIZE                  SPA-GMN-BYOLST-AREA
                                           SPA-NAI-BYOLST-AREA
                                           SPA-GMN-CHKLST-AREA
                                           SPA-NAI-CHKLST-AREA
               PERFORM 3101-BEHAVELST-SEC
      *
               MOVE    ZERO                TO  FLG-ERRLST-ROW
      *
           END-IF
      *
           .
       4121-REIGAI-EXT.
           EXIT.
      *****************************************************************
      *    チェック区分チェック処理
      *****************************************************************
       4200-CHKNUMLST-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-CHKNUM-MAX )
                OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
                IF   ( WRK-CMB-CD  =   SPA-GMN-CHKNUML (IDX-CMB))
                   MOVE    SPA-GMN-CHKNUMLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                END-IF
      *
           END-PERFORM
      *
           .
       4200-CHKNUMLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック対象外区分コンボチェック処理
      *****************************************************************
       4200-BEHAVELST-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-BEHAVE-MAX )
                OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
                IF   ( WRK-CMB-CD  =   SPA-GMN-BEHAVEL (IDX-CMB))
                   MOVE    SPA-GMN-BEHAVELST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                END-IF
      *
           END-PERFORM
      *
           .
       4200-BEHAVELST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-DIDCD       NOT =   SPACE )
               PERFORM 520-DID1SET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "D04    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE   D04-ERRLST-ROW       TO  WRK-ERRLST-ROW
           MOVE   D04-BYOLST-ROW       TO  WRK-BYOLST-ROW
           MOVE   D04-CHKLST-ROW       TO  WRK-CHKLST-ROW
      *
           MOVE    SPACE               TO  D04
           INITIALIZE                      D04
      *
           IF    ( FLG-ERRLST-ROW      =   1 )
               MOVE    ZERO            TO  D04-ERRLST-ROW
                                           D04-ERRLST-ROWATTR
           ELSE
               MOVE    WRK-ERRLST-ROW  TO  D04-ERRLST-ROW
               MOVE    ZERO            TO  D04-ERRLST-ROWATTR
           END-IF
      *
           IF    ( FLG-BYOLST-ROW      =   1 )
               MOVE    ZERO            TO  D04-BYOLST-ROW
                                           D04-BYOLST-ROWATTR
           ELSE
               MOVE    WRK-BYOLST-ROW  TO  D04-BYOLST-ROW
               MOVE    ZERO            TO  D04-BYOLST-ROWATTR
           END-IF
      *
           IF    ( FLG-CHKLST-ROW      =   1 )
               MOVE    ZERO            TO  D04-CHKLST-ROW
                                           D04-CHKLST-ROWATTR
           ELSE
               MOVE    WRK-CHKLST-ROW  TO  D04-CHKLST-ROW
               MOVE    ZERO            TO  D04-CHKLST-ROWATTR
           END-IF
      *
           MOVE    "blue"              TO  D04-LBLSRYYM-STYLE
                                           D04-LBLNYUGAI-STYLE
           MOVE    SPA-GMN-LBLSRYYM    TO  D04-LBLSRYYM
           MOVE    SPA-GMN-LBLNYUGAI   TO  D04-LBLNYUGAI
           MOVE    SPA-GMN-CHKNUM-G    TO  D04-CHKNUM
           MOVE    SPA-GMN-ERRLST-SELNUM
                                       TO  D04-SELNUM
      *
           MOVE    SPA-GMN-ERRLST-MAX  TO  D04-ERRLST-COUNT
           PERFORM VARYING IDXL FROM    1   BY  1
                   UNTIL ( IDXL >   SPA-GMN-ERRLST-MAX )
               MOVE    SPA-GMN-TNUM (IDXL)
                                       TO  WRK-ZZZZ9
               MOVE    WRK-ZZZZ9       TO  D04-TERRLSTNO (IDXL)
      *
               IF    ( SPA-GMN-TREIGAI(IDXL)    =   "1" OR "2" )
                   MOVE    "*"         TO  D04-TERRLSTNO (IDXL)(1:1)
               END-IF
      *
               MOVE    SPA-GMN-TPTNUM (IDXL)
                                       TO  D04-TPTNUM    (IDXL)
               MOVE    SPA-GMN-TNAME  (IDXL)
                                       TO  D04-TNAME     (IDXL)
               MOVE    SPA-GMN-THKNSBT(IDXL)
                                       TO  D04-THKNSBT   (IDXL)
               MOVE    SPA-GMN-TERRMSG (IDXL)
                                       TO  D04-TERRMSG   (IDXL)
      *
               IF    ( IDXL     =   SPA-GMN-ERRLST-SEL )
                   MOVE    "T"         TO  D04-ERRLST-SELECT (IDXL)
               ELSE
                   MOVE    "F"         TO  D04-ERRLST-SELECT (IDXL)
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-CHKNUM-G    TO  D04-CHKNUM
           MOVE    SPA-GMN-CHKNUM-MAX  TO  D04-CHKNUM-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   D04-CHKNUM-COUNT )
               MOVE    SPA-GMN-CHKNUMLST (IDX-CMB)
                                       TO  D04-CHKNUM-ITEM  (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-PTNUM       TO  D04-PTNUM
           MOVE    SPA-GMN-NAME        TO  D04-NAME
           MOVE    SPA-GMN-SEX         TO  D04-SEX
           MOVE    SPA-GMN-BIRTHDAY    TO  D04-BIRTHDAY
           EVALUATE    SPA-NAI-CHKKBN
           WHEN    SPACE
               EVALUATE    TRUE
               WHEN  ( SPA-NAI-CHKNUM  =   "20" )
                AND  ( SPA-GMN-ERRCD   =   "200001" )
                   STRING  SPA-GMN-SRYCD   DELIMITED BY SIZE
                           " "             DELIMITED BY SIZE
                           SPA-GMN-SRYNAME DELIMITED BY SPACE
                           "　上限（"      DELIMITED BY SIZE
                           SPA-GMN-TANINAME DELIMITED BY SPACE
                           "）："          DELIMITED BY SIZE
                           SPA-GMN-LIMITMEI DELIMITED BY SPACE
                   INTO    D04-SRYNAME
                   END-STRING
               WHEN  ( SPA-NAI-CHKNUM  =   "21" )
                   STRING  SPA-GMN-SRYCD   DELIMITED BY SIZE
                           " "             DELIMITED BY SIZE
                           SPA-GMN-SRYNAME DELIMITED BY SPACE
                   INTO    D04-SRYNAME
                   END-STRING
               WHEN    OTHER
                   STRING  SPA-GMN-ERRCD   DELIMITED BY SIZE
                           " "             DELIMITED BY SIZE
                           SPA-GMN-ERRMSG
                   INTO    D04-SRYNAME
                   END-STRING
               END-EVALUATE
           WHEN    "8"
           WHEN    "9"
               MOVE    SPA-GMN-CBYOMEI TO  D04-SRYNAME
           WHEN    "4"
           WHEN    "5"
           WHEN    "A"
           WHEN    "B"
               STRING  SPA-GMN-SRYCD   DELIMITED BY SIZE
                       " "             DELIMITED BY SIZE
                       SPA-GMN-SRYNAME DELIMITED BY SPACE
                       "／"            DELIMITED BY SIZE
                       SPA-GMN-TEKCD   DELIMITED BY SIZE
                       " "             DELIMITED BY SIZE
                       SPA-GMN-TEKNAME DELIMITED BY SPACE
               INTO    D04-SRYNAME
               END-STRING
           WHEN    "6"
               MOVE    SPA-GMN-TEKCOMPKBN
                                       TO  WRK-EDIT-COMPKBN
               PERFORM 800-EDIT-COMPKBN-MEI-SEC
               STRING  SPA-GMN-SRYCD   DELIMITED BY SIZE
                       " "             DELIMITED BY SIZE
                       SPA-GMN-SRYNAME DELIMITED BY SPACE
                       "／"            DELIMITED BY SIZE
                       SPA-GMN-TEKBYOMEI   DELIMITED BY SPACE
                       "［"            DELIMITED BY SIZE
                       WRK-EDIT-COMPKBN-MEI
                                       DELIMITED BY SPACE
                       "］"            DELIMITED BY SIZE
               INTO    D04-SRYNAME
               END-STRING
           WHEN    OTHER
               STRING  SPA-GMN-SRYCD   DELIMITED BY SIZE
                       " "             DELIMITED BY SIZE
                       SPA-GMN-SRYNAME DELIMITED BY SPACE
               INTO    D04-SRYNAME
               END-STRING
           END-EVALUATE
      *
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    D04-SRYNAME         TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  D04-SRYNAME
      *
           MOVE    SPA-GMN-BYOLST-SELNUM
                                       TO  D04-BYOSELNUM
           MOVE    SPA-GMN-BYOMEI      TO  D04-BYOMEI
           MOVE    SPA-GMN-HOSOKU      TO  D04-HOSOKU
           MOVE    SPA-GMN-CHKLST-SELNUM
                                       TO  D04-CHKSELNUM
      *
           IF    ( SPA-NAI-REIGAICHKKBN    =   "1" )
               MOVE    SPA-GMN-BEHAVE-G    TO  D04-BEHAVE
               MOVE    SPA-GMN-BEHAVE-MAX  TO  D04-BEHAVE-COUNT
               PERFORM VARYING IDX-CMB FROM    1   BY  1
                       UNTIL ( IDX-CMB >   D04-BEHAVE-COUNT )
                   MOVE    SPA-GMN-BEHAVELST (IDX-CMB)
                                           TO  D04-BEHAVE-ITEM(IDX-CMB)
               END-PERFORM
           END-IF
      *
           IF   (  SPA-GMN-NAME        =   SPACE )
            OR  (  SPA-NAI-CHKNUM      =   "A1" )
               MOVE    "患者病名"      TO  D04-BYOLST-TITLE
           ELSE
               STRING  "患者病名（"        DELIMITED   BY  SIZE
                       SPA-GMN-NAME        DELIMITED   BY  SPACE
                       "）"
               INTO    D04-BYOLST-TITLE
               END-STRING
           END-IF
      *
           MOVE    SPA-GMN-BYOLST-MAX  TO  D04-BYOLST-COUNT
           PERFORM VARYING IDXL FROM    1   BY  1
                   UNTIL ( IDXL >   SPA-GMN-BYOLST-MAX )
               MOVE    IDXL         TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9        TO  D04-TBYOLSTNO (IDXL)
               IF    ( SPA-GMN-BYOMEI  NOT =   SPACE )
                   MOVE    "search"        TO  ORCSSTR-COMMAND
                   MOVE    SPA-GMN-TBYOMEI (IDXL)
                                           TO  ORCSSTR-STRING1
                   MOVE    SPA-GMN-BYOMEI  TO  ORCSSTR-STRING2
                   CALL    "orcsstring" USING  ORCSSTRINGAREA
                   IF    ( ORCSSTR-NUM1    >   ZERO )
                       IF    ( SPA-GMN-HOSOKU
                                           NOT =   SPACE )
                           MOVE    "search"
                                           TO  ORCSSTR-COMMAND
                           MOVE    SPA-GMN-TBYOHOSOKU (IDXL)
                                           TO  ORCSSTR-STRING1
                           MOVE    SPA-GMN-HOSOKU
                                           TO  ORCSSTR-STRING2
                           CALL    "orcsstring" USING  ORCSSTRINGAREA
                           IF    ( ORCSSTR-NUM1    >   ZERO )
                               MOVE    "*" TO  D04-TBYOMEI   (IDXL)(1:1)
                           END-IF
                       ELSE
                           MOVE    "*" TO  D04-TBYOMEI   (IDXL)(1:1)
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( SPA-GMN-TBYOHOSOKU (IDXL)   NOT =   SPACE )
                   STRING  SPA-GMN-TBYOMEI (IDXL)
                                               DELIMITED BY SPACE
                           "（"                DELIMITED BY SIZE
                           SPA-GMN-TBYOHOSOKU (IDXL)
                                               DELIMITED BY SPACE
                           "）"                DELIMITED BY SIZE
                   INTO    D04-TBYOMEI (IDXL)(2:)
                   END-STRING
               ELSE
                   MOVE    SPA-GMN-TBYOMEI (IDXL)
                                       TO  D04-TBYOMEI
                                                  (IDXL)(2:)
               END-IF
      *
               MOVE    SPA-GMN-TSRYKA  (IDXL)
                                       TO  D04-TSRYKA    (IDXL)
               MOVE    SPA-GMN-TSRYYMD(IDXL)
                                       TO  D04-TSRYYMD   (IDXL)
               MOVE    SPA-GMN-TTENKIYMD (IDXL)
                                       TO  D04-TTENKIYMD (IDXL)
               MOVE    SPA-GMN-TTENKIKBN (IDXL)
                                       TO  D04-TTENKIKBN (IDXL)
               MOVE    SPA-GMN-TSYUBYO (IDXL)
                                       TO  D04-TSYUBYO   (IDXL)
               MOVE    SPA-GMN-TUTAGAI (IDXL)
                                       TO  D04-TUTAGAI   (IDXL)
               MOVE    SPA-GMN-TNYUGAIKBN (IDXL)
                                       TO  D04-TNYUGAIKBN(IDXL)
               MOVE    SPA-GMN-THKNCOMBI (IDXL)
                                       TO  D04-THKNCOMBI(IDXL)
      *
               IF    ( IDXL     =   SPA-GMN-BYOLST-SEL )
                   MOVE    "T"         TO  D04-BYOLST-SELECT (IDXL)
               ELSE
                   MOVE    "F"         TO  D04-BYOLST-SELECT (IDXL)
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-NAI-CHKNUM      =   "20" )
            AND  ( SPA-GMN-ERRCD       =   "200001" )
               PERFORM 5002-GMNHEN-CHKLST-SEC
           ELSE
               PERFORM 5001-GMNHEN-CHKLST-SEC
           END-IF
      *
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
      *    カーソル位置
           PERFORM 5001-MAPCUR-SEC
           .
      *
       500-GMNHEN-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタコラムリスト編集処理（チェックマスタ内容）
      *****************************************************************
       5001-GMNHEN-CHKLST-SEC          SECTION.
      *
           IF    ( SPA-NAI-CHKLST-NOCHK    =   "1" )
               MOVE    "チェックしない"
                                       TO  D04-CHKLST-LBLCHKMST
           ELSE
               MOVE    "チェックマスタ"
                                       TO  D04-CHKLST-LBLCHKMST
           END-IF
      *
           EVALUATE    SPA-NAI-CHKKBN
           WHEN    "1"
           WHEN    "2"
           WHEN    "6"
               MOVE    "一致"          TO  D04-CHKLST-LBLCOMPKBN
           WHEN    "7"
               MOVE    "日／月"        TO  D04-CHKLST-LBLCOMPKBN
           END-EVALUATE
      *
           MOVE    ZERO                TO  CNT-OTHER
                                           IDXM
      *
           PERFORM VARYING IDXL FROM    1   BY  1
                   UNTIL ( IDXL >   SPA-GMN-CHKLST-MAX )
      *
               COMPUTE IDXM    =   IDXM    +   1
      *
               IF   ( SPA-NAI-CHKLST-ANOTHER       =   ZERO )
                AND ( SPA-GMN-TOTHERMST (IDXL)
                                               NOT =   SPACE )
      *
                   COMPUTE CNT-OTHER   =    CNT-OTHER  +   1
      *
                   IF    ( CNT-OTHER   =   1 )
                       EVALUATE    SPA-GMN-TOTHERMST (IDXL)
                       WHEN    "1"
                           MOVE
                           "―――　以下適応病名マスタより　―――"
                                       TO  D04-TCHKBYOMEI  (IDXM)
      *
                       WHEN    "2"
                           MOVE
                           "―――　以下電子点数表マスタより　―――"
                                       TO  D04-TCHKBYOMEI  (IDXM)
                       WHEN    "3"
                           MOVE
                           "―――　以下特材品目マスタより　―――"
                                       TO  D04-TCHKBYOMEI  (IDXM)
      *
                       END-EVALUATE
                       COMPUTE IDXM    =   IDXM    +   1
                   END-IF
      *
      *
               END-IF
      *
               MOVE    SPA-GMN-TCHKLSTNUM (IDXL)
                                       TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9        TO  D04-TCHKLSTNO (IDXM)
      *
               EVALUATE    SPA-NAI-CHKKBN
               WHEN    "1"
               WHEN    "2"
               WHEN    "6"
                   MOVE    SPA-GMN-TGRPID (IDXL)
                                       TO  D04-TCHKGRPID (IDXM)
                   IF    ( SPA-GMN-BYOMEI  NOT =   SPACE )
                       MOVE    "search"
                                       TO  ORCSSTR-COMMAND
                       MOVE    SPA-GMN-TCHKBYOMEI (IDXL)
                                       TO  ORCSSTR-STRING1
                       MOVE    SPA-GMN-BYOMEI  TO  ORCSSTR-STRING2
                       CALL    "orcsstring" USING  ORCSSTRINGAREA
                       IF    ( ORCSSTR-NUM1    >   ZERO )
                           IF    ( SPA-GMN-HOSOKU
                                               NOT =   SPACE )
                               MOVE    "search"
                                       TO  ORCSSTR-COMMAND
                               MOVE    SPA-GMN-TCHKHOSOKU (IDXL)
                                       TO  ORCSSTR-STRING1
                               MOVE    SPA-GMN-HOSOKU TO ORCSSTR-STRING2
                               CALL    "orcsstring" USING ORCSSTRINGAREA
                               IF    ( ORCSSTR-NUM1    >   ZERO )
                                   MOVE    "*"
                                           TO  D04-TCHKBYOMEI(IDXM)(1:1)
                               END-IF
                           ELSE
                               MOVE    "*" TO  D04-TCHKBYOMEI(IDXM)(1:1)
                           END-IF
                       END-IF
                   END-IF
      *
                   IF    ( SPA-GMN-TCHKHOSOKU (IDXL)   NOT =   SPACE )
                       STRING  SPA-GMN-TCHKBYOMEI (IDXL)
                                                   DELIMITED BY SPACE
                               "（"                DELIMITED BY SIZE
                               SPA-GMN-TCHKHOSOKU (IDXL)
                                                   DELIMITED BY SPACE
                               "）"                DELIMITED BY SIZE
                       INTO    D04-TCHKBYOMEI (IDXM)(2:)
                       END-STRING
                   ELSE
                       MOVE    SPA-GMN-TCHKBYOMEI (IDXL)
                                       TO  D04-TCHKBYOMEI
                                                  (IDXM)(2:)
                   END-IF
      *
                   MOVE    SPA-GMN-TCOMPKBN   (IDXL)
                                       TO  D04-TCOMPKBN (IDXM)
               WHEN    "4"
               WHEN    "5"
               WHEN    "A"
               WHEN    "B"
                   MOVE    SPA-GMN-TCHKSRYNAME(IDXL)
                                       TO  D04-TCHKBYOMEI
                                                  (IDXM)(2:)
                   MOVE    SPA-GMN-TCOMPKBN   (IDXL)
                                       TO  D04-TCOMPKBN     (IDXM)
               WHEN    "8"
               WHEN    "9"
                   MOVE    SPA-GMN-TCHKSRYNAME(IDXL)
                                       TO  D04-TCHKBYOMEI
                                                  (IDXM)(2:)
               WHEN    "7"
                   MOVE    SPA-GMN-TCHKSRYNAME(IDXL)
                                       TO  D04-TCHKBYOMEI
                                                  (IDXM)(2:)
                   MOVE    SPA-GMN-TDAYMONTHKBN   (IDXL)
                                       TO  D04-TCOMPKBN     (IDXM)
               END-EVALUATE
      *
               IF    ( IDXL     =   SPA-GMN-CHKLST-SELNUM )
                   MOVE    "T"         TO  D04-CHKLST-SELECT (IDXM)
               ELSE
                   MOVE    "F"         TO  D04-CHKLST-SELECT (IDXM)
               END-IF
      *
           END-PERFORM
      *
           MOVE    IDXM            TO  D04-CHKLST-COUNT
      *
           .
       5001-GMNHEN-CHKLST-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタコラムリスト編集処理（算定回数）
      *****************************************************************
       5002-GMNHEN-CHKLST-SEC          SECTION.
      *
           MOVE    "算定日"            TO  D04-CHKLST-LBLCHKMST
      *
           MOVE    "回数"              TO  D04-CHKLST-LBLCOMPKBN
      *
      *
           PERFORM VARYING IDXL FROM    1   BY  1
                   UNTIL ( IDXL >   SPA-GMN-CHKLST-MAX )
      *
               MOVE    IDXL         TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9     TO  D04-TCHKLSTNO (IDXL)
               MOVE    SPA-GMN-TSNTYMD (IDXL)
                                    TO  D04-TCHKBYOMEI (IDXL)
               MOVE    SPA-GMN-TKAISU  (IDXL)
                                    TO  WRK-ZZZZ9
               MOVE    WRK-ZZZZ9    TO  D04-TCOMPKBN     (IDXL)
      *
               IF    ( IDXL     =   SPA-GMN-CHKLST-SELNUM )
                   MOVE    "T"         TO  D04-CHKLST-SELECT (IDXL)
               ELSE
                   MOVE    "F"         TO  D04-CHKLST-SELECT (IDXL)
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-CHKLST-MAX  TO  D04-CHKLST-COUNT
      *
           .
       5002-GMNHEN-CHKLST-EXT.
           EXIT.
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
           MOVE    WIDGET-NORMAL       TO  D04-B06-STATE
                                           D04-B07-STATE
                                           D04-B06C-STATE
                                           D04-B07C-STATE
                                           D04-B09-STATE
                                           D04-B09-STATE
                                           D04-B12-STATE
                                           D04-BEHAVE-STATE
                                           D04-BEHAVE-ITEM-STATE
      *
           IF    ( SPA-GMN-ERRLST-PAGE    <=   1 )
               MOVE    WIDGET-INSENSITIVE  TO  D04-B06-STATE
           END-IF
      *
           IF    ( SPA-GMN-ERRLST-NEXT     =   ZERO )
               MOVE    WIDGET-INSENSITIVE  TO  D04-B07-STATE
           END-IF
      *
           IF    ( SPA-GMN-CHKLST-PAGE    <=   1 )
               MOVE    WIDGET-INSENSITIVE  TO  D04-B06C-STATE
           END-IF
      *
           IF    ( SPA-GMN-CHKLST-NEXT     =   ZERO )
               MOVE    WIDGET-INSENSITIVE  TO  D04-B07C-STATE
           END-IF
      *
           IF    ( SPA-NAI-GSRAUTH-22      =   SPACE )
               MOVE    WIDGET-INSENSITIVE  TO  D04-B08-STATE
           END-IF
      *
           IF    ( SPA-NAI-GSRAUTH-103     =   SPACE )
               MOVE    WIDGET-INSENSITIVE  TO  D04-B09-STATE
           END-IF
      *
           IF    ( SPA-NAI-CHKKBN          =   SPACE )
               MOVE    WIDGET-INSENSITIVE  TO  D04-B09-STATE
           END-IF
      *
           IF    ( SPA-NAI-REIGAICHKKBN NOT    =   "1" )
            OR   ( SPA-NAI-CHKNUM              =   "A1" )
            OR   ( SPA-NAI-PTID                =   ZERO )
               MOVE    WIDGET-INSENSITIVE  TO  D04-B12-STATE
                                               D04-BEHAVE-STATE
                                               D04-BEHAVE-ITEM-STATE
           END-IF
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    カーソル位置処理
      *****************************************************************
       5001-MAPCUR-SEC                 SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE)
             AND ( SPA-GMN-CUR         =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5011-INPUT-CUR-SKIP-SEC
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    001
               MOVE    "CHKNUM"        TO  MCP-WIDGET
           WHEN    002
               MOVE    "SELNUM"        TO  MCP-WIDGET
           WHEN    003
               MOVE    "BEHAVE"        TO  MCP-WIDGET
           WHEN    004
               MOVE    "BYOSELNUM"     TO  MCP-WIDGET
           WHEN    005
               MOVE    "BYOMEI"        TO  MCP-WIDGET
           WHEN    006
               MOVE    "CHKSELNUM"     TO  MCP-WIDGET
           WHEN    007
               MOVE    "HOSOKU"        TO  MCP-WIDGET
           WHEN    208
               MOVE    "B08"           TO  MCP-WIDGET
           WHEN    209
               MOVE    "B09"           TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "CHKNUM"
               MOVE    002             TO  SPA-GMN-CUR
           WHEN    "SELNUM"
               MOVE    003             TO  SPA-GMN-CUR
           WHEN    "BEHAVE"
               IF    ( SPA-GMN-BEHAVE-G    =   SPACE )
                   MOVE    004         TO  SPA-GMN-CUR
               ELSE
                   MOVE    212         TO  SPA-GMN-CUR
               END-IF
           WHEN    "BYOSELNUM"
               MOVE    005             TO  SPA-GMN-CUR
           WHEN    "BYOMEI"
               MOVE    007             TO  SPA-GMN-CUR
           WHEN    "HOSOKU"
               MOVE    006             TO  SPA-GMN-CUR
               IF    ( SPA-GMN-BYOLST-SELNUM   >   ZERO )
                   MOVE    209         TO  SPA-GMN-CUR
               END-IF
               IF    ( SPA-GMN-CHKLST-SELNUM   >   ZERO )
                   MOVE    208         TO  SPA-GMN-CUR
               END-IF
           WHEN    "CHKSELNUM"
               IF    ( SPA-GMN-CHKLST-SELNUM   >   ZERO )
                   MOVE    208         TO  SPA-GMN-CUR
               ELSE
                   MOVE    209         TO  SPA-GMN-CUR
               END-IF
           WHEN    "BREIGAI"
               MOVE    002             TO  SPA-GMN-CUR
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
       5011-INPUT-CUR-SKIP-SEC         SECTION.
      *
           IF    ( SPA-GMN-CUR     =   3 )
               IF    ( D04-BEHAVE-STATE    =   WIDGET-INSENSITIVE )
                   MOVE    4       TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       5011-INPUT-CUR-SKIP-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC                  SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "確認項目を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "確認項目入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "選択番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "病名選択番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "チェックマスタ病名選択番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "病名は全角で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "エラー内容を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "「チェックしない」が選択されていません"
                                       TO  SPA-ERRMSG
           WHEN    "0009"
               MOVE    "補足コメントは全角で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0010"
               MOVE    "補足コメントの前後に（）があります"
                                       TO  SPA-ERRMSG   
           WHEN    "4001"  THRU    "4999"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  DERR
           MOVE    SPA-ERRCD           TO  DERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  DERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "D04"               TO  SPA-MOTOPG
           MOVE    "DERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "DERR"              TO  MCP-WINDOW
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
       520-DID1SET-SEC                 SECTION.
      *
           MOVE    SPACE               TO  WRK-DIDMSG
      *
      *    EVALUATE    SPA-DIDCD
      *    WHEN    "6666"
      *        MOVE    "処理中です"    TO  WRK-DIDMSG
      *    END-EVALUATE
      *
           MOVE    SPACE               TO  DID1
           INITIALIZE                      DID1
           MOVE    SPA-DIDCD           TO  DID1-ID1CODE
           MOVE    WRK-DIDMSG          TO  DID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "D04"               TO  SPA-MOTOPG
           MOVE    "DID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "DID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-DID1SET-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-EDTYMD1
                                           WRK-EDTYMD3
      *
           IF    ( WRK-YMD             =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-YMD             TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
               INSPECT WRK-EDTYMD3     REPLACING  ALL "  "  BY  "　"
           END-IF
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC               SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    SPA保存処理
      *****************************************************************
       800-SAVE-D04SPA-SEC         SECTION.
      *
           INITIALIZE                  DSUB01-LNK
           MOVE    "O"                 TO  DSUB01-KBN
           MOVE    SPA-D04             TO  DSUB01-SCRSPA-AREA
           MOVE    SPA-D01KYOUTU       TO  DSUB01-COMMON
      *
           CALL    "ORCGDSUB01"        USING   DSUB01-LNK
                                               SPA-AREA
      *
           .
      *
       800-SAVE-D04SPA-EXT.
           EXIT.
      *****************************************************************
      *    SPA復元処理
      *****************************************************************
       800-LOAD-D04SPA-SEC         SECTION.
      *
           INITIALIZE                  DSUB01-LNK
           MOVE    SPA-TERMID      TO  DSUB01-TERMID
           MOVE    "I"             TO  DSUB01-KBN
      *
           CALL    "ORCGDSUB01"        USING   DSUB01-LNK
                                               SPA-AREA
           .
      *
       800-LOAD-D04SPA-EXT.
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
      *    月末日取得処理
      *****************************************************************
       800-GETUMATU-SEC      SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY7-AREA
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
      *    一致条件名称編集処理
      *****************************************************************
       800-EDIT-COMPKBN-MEI-SEC        SECTION.
      *
           MOVE    SPACE           TO  WRK-EDIT-COMPKBN-MEI
      *
           EVALUATE    WRK-EDIT-COMPKBN
           WHEN    "1"
               MOVE    "部分"      TO  WRK-EDIT-COMPKBN-MEI
           WHEN    "2"
               MOVE    "完全"      TO  WRK-EDIT-COMPKBN-MEI
           WHEN    "3"
               MOVE    "前方"      TO  WRK-EDIT-COMPKBN-MEI
           WHEN    "4"
               MOVE    "後方"      TO  WRK-EDIT-COMPKBN-MEI
           END-EVALUATE
      *
           .
       800-EDIT-COMPKBN-MEI-EXT.
            EXIT.
      *****************************************************************
      *    文字列左詰処理
      *****************************************************************
       800-LTRIM-SEC               SECTION.
      *
           MOVE    SPACE           TO  WRK-LTRIM-FL
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >       500 )
      *
               IF    ( WRK-LTRIM (IDX1 : 1)
                                       NOT =   SPACE )
                   MOVE    WRK-LTRIM (IDX1:)
                                           TO  WRK-LTRIM-TMP
                   MOVE    WRK-LTRIM-TMP   TO  WRK-LTRIM
                   MOVE    500             TO  IDX1
               END-IF
      *
           END-PERFORM
      *
           .
       800-LTRIM-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-SEC                 SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE   "2"              TO  KANACHK-SYORI
           MOVE  500               TO  KANACHK-MAX-CNT
           MOVE  WRK-KANACHK-STR   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-STR
      *
           .
       800-ZENKAKU-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索(KEY)
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM      TO  PTINF-HOSPNUM
           MOVE    WRK-PTID        TO  PTINF-PTID
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
      *    患者番号情報検索処理(KEY)
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM          TO  PTNUM-HOSPNUM
           MOVE    WRK-PTID            TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               INITIALIZE                  PTNUM-REC
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理
      *****************************************************************
       900-CHK004-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK004
      *
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    CHK004-REC      TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHK004-REC
           ELSE
               INITIALIZE              CHK004-REC
               MOVE    1           TO  FLG-CHK004
           END-IF
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHK004-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理
      *****************************************************************
       900-CHK004-KEY7-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK004
      *
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    CHK004-REC      TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 900-CHK004-KEY7-FET-SEC
           ELSE
               INITIALIZE              CHK004-REC
               MOVE    1           TO  FLG-CHK004
           END-IF
      *
           .
       900-CHK004-KEY7-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４FETCH処理
      *****************************************************************
       900-CHK004-KEY7-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK004
                                       FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-CHK004      NOT =   ZERO )
                    OR   ( FLG-LOOPEND     NOT =   ZERO )
      *
               MOVE    "tbl_chk004"    TO  MCP-TABLE
               MOVE    "key7"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  CHK004-REC
      *
                   MOVE    1           TO  FLG-LOOPEND
      *
               ELSE
                   INITIALIZE              CHK004-REC
                   MOVE    1           TO  FLG-CHK004
               END-IF
      *
           END-PERFORM
      *
           .
       900-CHK004-KEY7-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理
      *****************************************************************
       900-CHK004-KEY5-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK004
      *
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    CHK004-REC      TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHK004-REC
           ELSE
               INITIALIZE              CHK004-REC
               MOVE    1           TO  FLG-CHK004
           END-IF
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHK004-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理
      *****************************************************************
       900-CHK004-KEY6-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK004
      *
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    CHK004-REC      TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHK004-REC
           ELSE
               INITIALIZE              CHK004-REC
               MOVE    1           TO  FLG-CHK004
           END-IF
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHK004-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター検索（KEY）
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-SRYCD       TO  TNS-SRYCD
           MOVE    WRK-KJNYMD      TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者病名検索処理
      *****************************************************************
       900-PTBYOMEI-KEY39-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-PTBYOMEI
      *
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-NAI-PTID        TO  PTBYO-PTID
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key39"              TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 900-PTBYOMEI-KEY39-FET-SEC
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           .
       900-PTBYOMEI-KEY39-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者病名FETCH処理
      *****************************************************************
       900-PTBYOMEI-KEY39-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTBYOMEI
                                           FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-PTBYOMEI    NOT =   ZERO )
                    OR   ( FLG-LOOPEND     NOT =   ZERO )
      *
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key39"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
      *
                   MOVE    1               TO  FLG-LOOPEND
      *
                   MOVE    "99999999"      TO  WRK-TENKIYMD
                   IF    ( PTBYO-TENKIYMD  NOT =   SPACE )
                       MOVE    PTBYO-TENKIYMD
                                           TO  WRK-TENKIYMD
                   END-IF
      *
                   IF    ( PTBYO-NYUGAIKBN =   SPACE
                                           OR  SPA-NAI-NYUGAIKBN )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
      *
                   IF    ( PTBYO-SRYYMD (1:6) <=  SPA-NAI-SRYYM )
                    AND  ( WRK-TENKIYMD (1:6) >=  SPA-NAI-SRYYM )
                       CONTINUE
                   ELSE
      *                IF    ( SPA-NAI-TENKIBYOMEIKBN  =   "0" )
      *                 OR   ( PTBYO-REZEMM            =   ZERO )
                           MOVE    ZERO        TO  FLG-LOOPEND
      *                END-IF
                   END-IF
      *
                   IF    ( PTBYO-REZEMM        >   ZERO )
                       MOVE    PTBYO-SRYYMD    TO  WRK-SYMD
                       IF    ( PTBYO-REZEMM    >   1 )
                           MOVE    "2"         TO  WRK-ZOGENPTN
                           COMPUTE WRK-ZOGEN   =   PTBYO-REZEMM    -   1
                           PERFORM 800-CALENDAR-SEC
                       END-IF
                       IF    ( SPA-NAI-SRYYM   >   WRK-SYMD(1:6))
                           MOVE    ZERO    TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
                   IF    ( PTBYO-REZEFLG  =   "1" )
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
      *
               ELSE
                   INITIALIZE                  PTBYOMEI-REC
                   MOVE    1               TO  FLG-PTBYOMEI
               END-IF
           END-PERFORM
      *
           .
       900-PTBYOMEI-KEY39-FET-EXT.
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
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY3-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY3-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           INITIALIZE                      CHKSND-REC
           MOVE    SPA-NAI-CHKKBN      TO  CHKSND-CHKKBN
           MOVE    SPA-GMN-SRYCD       TO  CHKSND-SRYCD
           MOVE    WRK-KJNYMD          TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    "1"                 TO  CHKSND-CDKBN
      *
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-CHKSND-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
      *
       900-CHKSND-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２FETCH処理
      *****************************************************************
       900-CHKSND-KEY2-FET-SEC      SECTION.
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
      *
       900-CHKSND-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHKMST-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
           INITIALIZE                      CHKMST-REC
           MOVE    SPA-NAI-CHKKBN      TO  CHKMST-CHKKBN
           MOVE    SPA-GMN-SRYCD       TO  CHKMST-SRYCD
           MOVE    WRK-KJNYMD          TO  CHKMST-YUKOSTYMD
                                           CHKMST-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  CHKMST-HOSPNUM
           MOVE    CHKMST-REC          TO  MCPDATA-REC
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKMST-REC
           ELSE
               MOVE    1               TO  FLG-CHKMST
               INITIALIZE                  CHKMST-REC
           END-IF
      *
           .
      *
       900-CHKMST-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタFETCH処理
      *****************************************************************
       900-CHKMST-KEY2-FET-SEC      SECTION.
      *
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC     TO  CHKMST-REC
           ELSE
               MOVE    1               TO  FLG-CHKMST
               INITIALIZE                  CHKMST-REC
           END-IF
      *
           .
      *
       900-CHKMST-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理
      *****************************************************************
       900-CHKTRD-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKTRD
      *
           INITIALIZE                      CHKTRD-REC
           MOVE    SPA-NAI-CHKKBN      TO  CHKTRD-CHKKBN
           MOVE    SPA-GMN-SRYCD       TO  CHKTRD-SRYCD
           MOVE    WRK-KJNYMD          TO  CHKTRD-YUKOSTYMD
                                           CHKTRD-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  CHKTRD-HOSPNUM
           MOVE    CHKTRD-REC          TO  MCPDATA-REC
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKTRD-REC
           ELSE
               MOVE    1               TO  FLG-CHKTRD
               INITIALIZE                  CHKTRD-REC
           END-IF
      *
           .
      *
       900-CHKTRD-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３FETCH処理
      *****************************************************************
       900-CHKTRD-KEY2-FET-SEC      SECTION.
      *
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC     TO  CHKTRD-REC
           ELSE
               MOVE    1               TO  FLG-CHKTRD
               INITIALIZE                  CHKTRD-REC
           END-IF
      *
           .
      *
       900-CHKTRD-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５検索処理
      *****************************************************************
       900-CHK005-KEY11-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK005
      *
           INITIALIZE                      CHK005-REC
           MOVE    SPA-NAI-CHKKBN      TO  CHK005-CHKKBN
           MOVE    SPA-GMN-CBYOMEI     TO  CHK005-BYOMEI
           MOVE    SPA-HOSPNUM         TO  CHK005-HOSPNUM
           MOVE    WRK-KJNYMD          TO  CHK005-YUKOSTYMD
                                           CHK005-YUKOEDYMD
           MOVE    CHK005-REC          TO  MCPDATA-REC
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "key11"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK005-REC
           ELSE
               MOVE    1               TO  FLG-CHK005
               INITIALIZE                  CHK005-REC
           END-IF
      *
           .
      *
       900-CHK005-KEY11-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３FETCH処理
      *****************************************************************
       900-CHK005-KEY11-FET-SEC      SECTION.
      *
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "key11"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC     TO  CHK005-REC
           ELSE
               MOVE    1               TO  FLG-CHK005
               INITIALIZE                  CHK005-REC
           END-IF
      *
           .
      *
       900-CHK005-KEY11-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェック例外マスタ検索処理
      *****************************************************************
       900-CHKREIGAI-KEY4-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-CHKREIGAI
      *
           INITIALIZE                  CHKREIGAI-REC
           MOVE    SPA-HOSPNUM     TO  CHKREIGAI-HOSPNUM
           MOVE    SPA-NAI-PTID    TO  CHKREIGAI-PTID
           MOVE    CHKREIGAI-REC   TO  MCPDATA-REC
           MOVE    "tbl_chkreigai" TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHKREIGAI-REC
           ELSE
               INITIALIZE              CHKREIGAI-REC
               MOVE    1           TO  FLG-CHKREIGAI
           END-IF
      *
           MOVE    "tbl_chkreigai" TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHKREIGAI-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェック例外マスタ検索処理
      *****************************************************************
       900-CHKREIGAI-KEY5-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKREIGAI
      *
           INITIALIZE                      CHKREIGAI-REC
           MOVE    SPA-HOSPNUM         TO  CHKREIGAI-HOSPNUM
           MOVE    SPA-NAI-TPTID  (IDXL)
                                       TO  CHKREIGAI-PTID
           MOVE    SPA-NAI-TCHKNUM(IDXL)
                                       TO  CHKREIGAI-CHKNUM
           MOVE    SPA-NAI-NYUGAIKBN   TO  CHKREIGAI-NYUGAIKBN
           EVALUATE    SPA-NAI-TCHKKBN(IDXL)
           WHEN    SPACE
               EVALUATE    TRUE
               WHEN  ( SPA-NAI-TCHKNUM(IDXL)   =   "20" )
                 AND ( SPA-NAI-TERRCD(IDXL)    =   "200001" )
                   MOVE    CK4-1201-TANICD
                                       TO  CHKREIGAI-TANICD
                   MOVE    CK4-1201-SRYCD
                                       TO  CHKREIGAI-SRYCD
               WHEN    OTHER
                   MOVE    SPA-NAI-TERRCD(IDXL)
                                       TO  CHKREIGAI-ERRCD
               END-EVALUATE
           WHEN    "8"
           WHEN    "9"
               MOVE    CK4-1201-BYOMEI
                                       TO  CHKREIGAI-BYOMEI
           WHEN    "4"
           WHEN    "5"
           WHEN    "A"
           WHEN    "B"
               MOVE    CK4-1201-SRYCD
                                       TO  CHKREIGAI-SRYCD
               MOVE    CK4-1201-TEKCD
                                       TO  CHKREIGAI-TEKCD
           WHEN    "6"
               MOVE    CK4-1201-SRYCD
                                       TO  CHKREIGAI-SRYCD
               MOVE    CK4-1201-TEKBYOMEI
                                       TO  CHKREIGAI-TEKBYOMEI
               MOVE    CK4-1201-TEKCOMPKBN
                                       TO  CHKREIGAI-TEKCOMPKBN
           WHEN    OTHER
               MOVE    CK4-1201-SRYCD  TO  CHKREIGAI-SRYCD
           END-EVALUATE
           MOVE    SPA-NAI-SRYYM       TO  CHKREIGAI-STYUKYMD
           MOVE    "01"                TO  CHKREIGAI-STYUKYMD(7:2)
           MOVE    CHKREIGAI-STYUKYMD  TO  CHKREIGAI-EDYUKYMD
           MOVE    CHKREIGAI-REC       TO  MCPDATA-REC
           MOVE    "tbl_chkreigai"     TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHKREIGAI-REC
           ELSE
               INITIALIZE              CHKREIGAI-REC
               MOVE    1           TO  FLG-CHKREIGAI
           END-IF
      *
           MOVE    "tbl_chkreigai" TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHKREIGAI-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入力コード検索処理
      *****************************************************************
       900-INPUTCD-KEY2-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-INPUTCD
      *
           INITIALIZE                  INPUTCD-REC
           MOVE    SPACE           TO  INPUTCD-REC
           MOVE    SPA-HOSPNUM     TO  ICD-HOSPNUM
           MOVE    "6"             TO  ICD-CDSYU
           MOVE    WRK-SRYCD       TO  ICD-INPUTCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  INPUTCD-REC
           ELSE
               INITIALIZE                  INPUTCD-REC
               MOVE    1               TO  FLG-INPUTCD
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-INPUTCD-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    適応病名マスタ検索処理(KEY2)
      *****************************************************************
       900-TEKIOUBYOMEI-KEY2-SEL-SEC   SECTION.
      *
           MOVE    ZERO            TO  FLG-TEKIOUBYOMEI
      *
           INITIALIZE              TEKIOUBYOMEI-REC
           MOVE    SPA-NAI-CHKKBN  TO  TEKBYO-CHKKBN
           MOVE    SPA-GMN-SRYCD   TO  TEKBYO-SRYCD
           MOVE    TEKIOUBYOMEI-REC    TO  MCPDATA-REC
           MOVE    "tbl_tekioubyomei"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  TEKIOUBYOMEI-REC
           ELSE
               INITIALIZE              TEKIOUBYOMEI-REC
               MOVE    1           TO  FLG-TEKIOUBYOMEI
           END-IF
      *
           .
      *
       900-TEKIOUBYOMEI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    適応病名マスタ検索処理(KEY2)
      *****************************************************************
       900-TEKIOUBYOMEI-KEY2-FET-SEC   SECTION.
      *
           MOVE    "tbl_tekioubyomei"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  TEKIOUBYOMEI-REC
           ELSE
               INITIALIZE              TEKIOUBYOMEI-REC
               MOVE    1           TO  FLG-TEKIOUBYOMEI
           END-IF
      *
           .
      *
       900-TEKIOUBYOMEI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    電子点数表（背反）検索処理
      *****************************************************************
       900-ETENSU3-SEL-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-ETENSU3
      *
           INITIALIZE                      ETENSU3-REC
           MOVE    SPA-GMN-SRYCD       TO  ETENSU3-SRYCD1
           MOVE    WRK-KJNYMD          TO  ETENSU3-YUKOSTYMD
                                           ETENSU3-YUKOEDYMD
           MOVE    ETENSU3-REC         TO  MCPDATA-REC
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  ETENSU3-REC
           ELSE
               MOVE    1               TO  FLG-ETENSU3
               INITIALIZE                  ETENSU3-REC
           END-IF
      *
           .
      *
       900-ETENSU3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    電子点数表（背反）FETCH処理
      *****************************************************************
       900-ETENSU3-FET-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-ETENSU3
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  ETENSU3-REC
           ELSE
               MOVE    1               TO  FLG-ETENSU3
               INITIALIZE                  ETENSU3-REC
           END-IF
      *
           .
      *
       900-ETENSU3-FET-EXT.
           EXIT.
      *****************************************************************
      *    電子点数表（算定回数）検索処理
      *****************************************************************
       900-ETENSU5-KEY-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ETENSU5
      *
           INITIALIZE                      ETENSU5-REC
           MOVE    SPA-GMN-SRYCD       TO  ETENSU5-SRYCD
           MOVE    SPA-GMN-TANICD      TO  ETENSU5-TANICD
           MOVE    WRK-KJNYMD          TO  ETENSU5-YUKOSTYMD
                                           ETENSU5-YUKOEDYMD
           MOVE    ETENSU5-REC         TO  MCPDATA-REC
           MOVE    "tbl_etensu_5"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  ETENSU5-REC
           ELSE
               MOVE    1               TO  FLG-ETENSU5
               INITIALIZE                  ETENSU5-REC
           END-IF
      *
           MOVE    "tbl_etensu_5"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-ETENSU5-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       900-SRYACT-KEY20-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYACT
      *
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPNUM     TO  SRY-HOSPNUM
           MOVE    "%"             TO  SRY-NYUGAIKBN
           MOVE    SPA-NAI-PTID    TO  SRY-PTID
           MOVE    SPA-NAI-SRYSTYMD (1:6)
                                   TO  SRY-SRYYM
           MOVE    SPA-NAI-SRYEDYMD (1:6)
                                   TO  SRY-CREYMD
           MOVE    "%"             TO  SRY-SRYKBN
           MOVE    SPA-GMN-SRYCD   TO  SRY-SRYCD (1)
                                       SRY-SRYCD (2)
                                       SRY-SRYCD (3)
                                       SRY-SRYCD (4)
                                       SRY-SRYCD (5)
           MOVE    SRYACT-REC      TO  MCPDATA-REC
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key20"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  SRYACT-REC
           ELSE
               MOVE    1           TO  FLG-SRYACT
               INITIALIZE              SRYACT-REC
           END-IF
      *
           .
       900-SRYACT-KEY20-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為FETCH処理
      *****************************************************************
       900-SRYACT-KEY20-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYACT
      *
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key20"         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  SRYACT-REC
           ELSE
               MOVE    1           TO  FLG-SRYACT
               INITIALIZE              SRYACT-REC
           END-IF
      *
           .
       900-SRYACT-KEY20-FET-EXT.
           EXIT.
      *****************************************************************
      *    診療会計検索処理
      *****************************************************************
       900-SRYACCT-KEY57-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYACCT
      *
           INITIALIZE                  SRYACCT-REC
           MOVE    SRY-HOSPNUM     TO  ACCT-HOSPNUM
           MOVE    SRY-PTID        TO  ACCT-PTID
           MOVE    SRY-ZAINUM      TO  ACCT-ZAINUM
           MOVE    SRYACCT-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key57"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  SRYACCT-REC
           ELSE
               MOVE    1           TO  FLG-SRYACCT
               INITIALIZE              SRYACCT-REC
           END-IF
      *
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key57"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SRYACCT-KEY57-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY60-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-PTID        TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key60"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE              PTNYUINRRK-REC
               MOVE    1           TO  FLG-PTNYUINRRK
           END-IF
      *
           .
      *
       900-PTNYUINRRK-KEY60-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴FETCH処理
      *****************************************************************
       900-PTNYUINRRK-KEY60-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-PTID        TO  PTNYUINRRK-PTID
           MOVE    WRK-EOMYMD          TO  PTNYUINRRK-NYUINYMD
           MOVE    SPA-NAI-SHONUM      TO  PTNYUINRRK-SHONUM
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key60"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE              PTNYUINRRK-REC
               MOVE    1           TO  FLG-PTNYUINRRK
           END-IF
      *
           .
      *
       900-PTNYUINRRK-KEY60-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY61-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-PTID        TO  PTNYUINRRK-PTID
           MOVE    WRK-EOMYMD          TO  PTNYUINRRK-NYUINYMD
           MOVE    SPA-NAI-SHONUM      TO  PTNYUINRRK-SHONUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key61"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE              PTNYUINRRK-REC
               MOVE    1           TO  FLG-PTNYUINRRK
           END-IF
      *
           .
      *
       900-PTNYUINRRK-KEY61-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴FETCH処理
      *****************************************************************
       900-PTNYUINRRK-KEY61-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key61"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE              PTNYUINRRK-REC
               MOVE    1           TO  FLG-PTNYUINRRK
           END-IF
      *
           .
      *
       900-PTNYUINRRK-KEY61-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為照会検索処理
      *****************************************************************
       900-NSRYSRH-KEY2-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-NSRYSRH
      *
           INITIALIZE                  NSRYSRH-REC
           MOVE    SPA-HOSPNUM     TO  NSRYSRH-HOSPNUM
           MOVE    SPA-NAI-PTID    TO  NSRYSRH-PTID
           MOVE    SPA-NAI-SRYSTYMD (1:6)
                                   TO  NSRYSRH-SRYYM
           MOVE    SPA-NAI-SRYEDYMD (1:6)
                                   TO  NSRYSRH-CREYMD
           MOVE    SPA-GMN-SRYCD   TO  NSRYSRH-SRYCD
           MOVE    NSRYSRH-REC     TO  MCPDATA-REC
           MOVE    "tbl_nsrysrh"   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  NSRYSRH-REC
           ELSE
               MOVE    1           TO  FLG-NSRYSRH
               INITIALIZE              NSRYSRH-REC
           END-IF
      *
           .
       900-NSRYSRH-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為照会FETCH処理
      *****************************************************************
       900-NSRYSRH-KEY2-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-NSRYSRH
           MOVE    "tbl_nsrysrh"   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  NSRYSRH-REC
           ELSE
               MOVE    1           TO  FLG-NSRYSRH
               INITIALIZE              NSRYSRH-REC
           END-IF
      *
           .
       900-NSRYSRH-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴照会検索処理
      *****************************************************************
       900-NRRKSRH-KEY3-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-NRRKSRH
      *
           INITIALIZE                  NRRKSRH-REC
           MOVE    SPA-HOSPNUM     TO  NRRKSRH-HOSPNUM
           MOVE    NSRYSRH-PTID    TO  NRRKSRH-PTID
           MOVE    NSRYSRH-SRYYM   TO  NRRKSRH-SRYYMD
           MOVE    "__"            TO  NRRKSRH-SRYYMD (7:2)
           MOVE    NSRYSRH-ZAINUM  TO  NRRKSRH-ZAINUM
           MOVE    NRRKSRH-REC     TO  MCPDATA-REC
           MOVE    "tbl_nrrksrh"   TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  NRRKSRH-REC
           ELSE
               MOVE    1           TO  FLG-NRRKSRH
               INITIALIZE              NRRKSRH-REC
           END-IF
      *
           .
       900-NRRKSRH-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴照会FETCH処理
      *****************************************************************
       900-NRRKSRH-KEY3-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-NRRKSRH
      *
           MOVE    "tbl_nrrksrh"   TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  NRRKSRH-REC
           ELSE
               MOVE    1           TO  FLG-NRRKSRH
               INITIALIZE              NRRKSRH-REC
           END-IF
      *
           .
       900-NRRKSRH-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    特定器材関連手技マスタ検索処理
      *****************************************************************
       900-MATERIAL-K-KEY2-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-MATERK
      *
           INITIALIZE                      MATERIAL-K-REC
      *
           MOVE    SPA-GMN-SRYCD       TO  MATERK-SRYCD
           MOVE    MATERIAL-K-REC      TO  MCPDATA-REC
           MOVE    "tbl_material_k"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  MATERIAL-K-REC
           ELSE
               INITIALIZE                  MATERIAL-K-REC
               MOVE    1               TO  FLG-MATERK
           END-IF
      *
           .
      *
       900-MATERIAL-K-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    特定器材関連手技マスタFETCH処理
      *****************************************************************
       900-MATERIAL-K-KEY2-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-MATERK
      *
           MOVE    "tbl_material_k"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  MATERIAL-K-REC
           ELSE
               INITIALIZE                  MATERIAL-K-REC
               MOVE    1               TO  FLG-MATERK
           END-IF
      *
           .
      *
       900-MATERIAL-K-KEY2-FET-EXT.
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
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理(FETCHは行わない）
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
       910-DBCLOSECURSOR-SEC           SECTION.
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
      *    ＤＢＩＮＳＥＲＴ処理
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
      *    ＤＢＵＰＤＡＴＥ処理
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
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
