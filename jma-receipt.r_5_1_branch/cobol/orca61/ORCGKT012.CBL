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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGKT012.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 外来まとめ入力
      *  コンポーネント名  : 診療行為選択（ＤＯ）（ＫＴ０１２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/02/XX    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.05.00     NACL-多々納  09/06/XX  数量小数５桁対応
      * 04.07.00     NACL-多々納  12/03/07  処方せん料表示対応（H24.4対応）
      *  04.08.00    NACL-多々納  14/05/29  一時ファイル変更
      *  05.01.00    NACL-多々納  20/02/XX  ユーザー単位対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメタデータ
      *    SELECT  PARA-FILE       ASSIGN  FILE-NAME2
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-PARA.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *    パラメタデータ
      *FD  PARA-FILE.
      *01  PARA-REC.
      *    03  PARA-KEY.
      *        05  PARA-FILEMEI         PIC X(08).
      *        05  PARA-EDANUM          PIC 9(03).
      *    03  PARA-DATA-REC            PIC X(60000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    パラメタファイル名
      *01  FILE-NAME2.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(08)   VALUE   "KT01PARA".
      *    03  FILE-HOSPNUM2       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID2        PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
      *    画面非表示
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面用共通スパ領域
           COPY    "KT01SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-SRYKBN          PIC X(02).
           03  STS-SPASCR          PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-END2            PIC 9(01).
           03  FLG-SPASCR          PIC 9(01).
           03  FLG-SRYKBN          PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-OK2             PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-SINKI           PIC 9(01).
           03  FLG-YAKUSOKU        PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
           03  FLG-KAI             PIC 9(01).
           03  FLG-DOALL           PIC 9(01).
           03  FLG-WRITE           PIC 9(01).
           03  FLG-HEN             PIC 9(01).
           03  FLG-HENSYU          PIC 9(01).
           03  FLG-KETA5           PIC 9(01).
      *H24.4
           03  FLG-OK3             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDG                 PIC 9(04).
           03  IDG3                PIC 9(04).
           03  IDM                 PIC 9(04).
           03  IDK1                PIC 9(04).
           03  IDK2                PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX-P               PIC 9(04).
           03  IDX-KOUI            PIC 9(04).
           03  IDX-SIN             PIC 9(04).
           03  IDX-ATO             PIC 9(04).
           03  IDX-YAKUZAI         PIC 9(04).
           03  IDX-Y2              PIC 9(02).
      *
           03  IDX-R                   PIC 9(04).
           03  IDX-W                   PIC 9(04).
           03  IDX-KAI                 PIC 9(04).
           03  IDX-STR                 PIC 9(02).
           03  IDX-H                   PIC 9(04).
      *
           03  IDX-S                   PIC 9(04).
           03  IDX-D1                  PIC 9(04).
           03  IDX-D2                  PIC 9(04).
           03  IDX-D3                  PIC 9(04).
           03  IDX-Z                   PIC 9(04).
           03  IDX-ZAI                 PIC 9(04).
      *
           03  IDX-S1              PIC 9(04).
           03  IDX-S2              PIC 9(06).
           03  IDX-SP              PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
      *
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WFIL1       PIC X(01).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WFIL2       PIC X(01).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-BANGO               PIC 9(03).
           03  WRK-BANGO-X             PIC X(03).
           03  WRK-DOSELNUM            PIC 9(04).
           03  WRK-IDXNUM              PIC 9(04).
           03  WRK-MAE-NYUGAIKBN       PIC X(01).
           03  WRK-MAE-GRPHOSPNUM      PIC X(02).
      *
           03  WRK-SELNUM-CHK-G.
               05  WRK-SELNUM-CHK  PIC 9(03)   OCCURS  135.
           03  WRK-HEN-SELNUM      PIC X(40).
           03  WRK-HEN-SELNUM-2    PIC X(40).
           03  WRK-HEN-SELNUM-HZN  PIC X(40).
           03  WRK-SELNUM-AREA.
               05  WRK-SELNUM-OCC    OCCURS   135.
                   07  WRK-SEL9        PIC 9(01).
      *
           03  WRK-DOSEL.
               05  WRK-DOSEL-OCC   OCCURS   135.
                   07  WRK-DO          PIC X(03).
                   07  WRK-DONUM       PIC 9(03).
                   07  WRK-KAI         PIC X(03).
                   07  WRK-KAINUM      PIC 9(03).
                   07  WRK-SEL         PIC X(01).
      *
           03  WRK-DOZAINUM-AREA.
               05  WRK-DOZAINUM-OCC    OCCURS   135.
                   07  WRK-DOZAINUM    PIC 9(08).
                   07  WRK-DOKAI       PIC 9(03).
      *
           03  HZN-DOZAINUM-AREA.
               05  HZN-DOZAINUM-OCC    OCCURS   135.
                   07  HZN-DOZAINUM    PIC 9(08).
                   07  HZN-DOKAI       PIC 9(03).
      *
           03  WRK-DOKAISU             PIC 9(03).
      *
           03  WRK-ZAINUM          PIC 9(08).
           03  WRK-ZAIKAISU        PIC 9(08).
           03  WRK-EDANUM          PIC 9(02).
      *
           03  WRK-TENSU-X.
               05  WRK-TENSU       PIC Z(08).
           03  WRK-KAISU-X.
               05  WRK-KAISU       PIC Z(08).
           03  WRK-KEI-X.
               05  WRK-KEI         PIC Z(08).
           03  WRK-KEI-G           PIC 9(08).
      *
      *    診療行為名称
           03  WRK-SRYSYUMEI       PIC X(40).
      *
           03  WRK-SRYSYUMEI-H     PIC X(400).
      *
           03  WRK-MEISYO          PIC X(40).
      *    数量表示用
           03  WRK-SURYO           PIC 9(05)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(05).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZZ.
               05  WRK-SURYO-Z1-9  REDEFINES   WRK-SURYO-Z1
                                   PIC ZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZZZ.
           03  WRK-SURYO-X2        PIC X(09).
           03  WRK-SURYO-J-G.
               05  WRK-SURYO-J         PIC X(18).
               05  WRK-SURYO-J2        PIC X(04).
      *    分画数画面編集用
           03  WRK-BUNGSU-X.
               05  WRK-BUNGSU-Z    PIC ZZZ.
           03  WRK-BUNGSU-J        PIC X(06).
      *    剤回数等画面編集用
           03  WRK-KAISU-H         PIC X(08).
           03  WRK-KEI-H           PIC X(08).
           03  WRK-ZAIKEI          PIC X(40).
           03  WRK-IDG             PIC 9(04).
           03  WRK-ZAIKEIJ         PIC X(80).
           03  IDJ                 PIC 9(04).
           03  IDJ1                PIC 9(01).
           03  IDJ2                PIC 9(04).
      *
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
           03  WRK-Z92             PIC --,---,---.
           03  WRK-ZZ              PIC ZZZ.
           03  WRK-ZZZ             PIC ZZZZ.
      *
           03  WRK-KIN             PIC 9(07).
      *
           03  WRK-ZAITEN              PIC 9(08).
           03  WRK-SRYKAISUKEI         PIC 9(03).
           03  WRK-SRYSURYOKEI         PIC 9(07)V9(05).
           03  WRK-MEISAISU            PIC 9(07).
           03  WRK-JIHIMONEY           PIC 9(07).
      *
           03  WRK-SEIKYU              PIC S9(07).
      *
           03  WRK-DENPNUM         PIC 9(07).
           03  WRK-MEIBAN          PIC 9(03).
      *
           03  WRK-HOUKNS-CNT          PIC 9(02).
           03  WRK-HOUKNS-IDX          PIC 9(04).
           03  WRK-HOUKNS-NAME         PIC X(100).
      *
           03  WRK-ERRMSG          PIC X(40).
      *
           03  WRK-GOKEI-X2.
               05  WRK-GOKEI-Z2    PIC ZZZ,ZZZ,ZZZ.
      *
           03  WRK-PATH                    PIC X(64).
      *H26.10
           03  WRK-DAY                 PIC X(02).
      *
      *    コラムリスト位置
           03  WRK-KT012-RRKLIST-ROW     PIC S9(09).
           03  WRK-KT012-MEILIST-ROW     PIC S9(09).
           03  FLG-GMN-INIT1           PIC  9(01).
           03  FLG-GMN-INIT2           PIC  9(01).
      *
      *    換算値数量表示用
           03  WRK-TSURYO              PIC X(30).
           03  IDM2                    PIC 9(04).
      *    商品名単位名称
           03  WRK-SYO-TANINAME        PIC X(12).
      *H28.5
           03  WRK-FUKA-INPUT          PIC X(01).
           03  WRK-SRYYMD              PIC X(08).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *画面最大行
       01  MAX-LINE                PIC 9(04)   VALUE   800.
      *選択数ＭＡＸ（１受診最大剤数）
       01  MAX-SEL                 PIC 9(04)   VALUE   135.
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    診療行為マスタワーク
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC      OCCURS   300.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
       01  LNK-WKSRYACT-MAX           PIC 9(04).
      *
      *    診療行為マスタワーク
       01  WRK-WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                    //WKSRY-// BY //WRK-WKSRY-//.
      *    受診履歴
       01  HZN-JYURRK-REC.
           COPY    "CPJYURRK.INC"    REPLACING
                                     //JYURRK-// BY //HZN-JYURRK-//.
      *
      *v4.8
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC". 
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *    表示名称編集パラメタ領域
           COPY    "CPORCSMEIHEN.INC".
      *    全角変換サブ
           COPY    "CPORCSKANACHK.INC".
      *
      *H28.5
      *    院外処方名編集パラメタ
           COPY    "CPORCSCGENNAME.INC".
      *
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "KT01.INC".
           COPY    "KT011.INC".
           COPY    "KT012.INC".
           COPY    "KT013.INC".
           COPY    "KT02.INC".
           COPY    "KT03.INC".
           COPY    "KT98.INC".
           COPY    "KTERR.INC".
           COPY    "KTERR2.INC".
           COPY    "KTID1.INC".
           COPY    "KTID2.INC".
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
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-KT01
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
      *    診療種別テーブルセット
           PERFORM 1001-SRYKBN-SET-SEC
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-K01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-KT01        TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    診療種別テーブルセット処理
      *****************************************************************
       1001-SRYKBN-SET-SEC             SECTION.
      * 
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
           .
       1001-SRYKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "KTERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "KT012"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
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
       300-SCREEN-SEC             SECTION.
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "KT013"
                   IF      SPA-KT012-DOSELNUM    >   ZERO
      *                診察内容編集
                       PERFORM 4101-JYURRK-HEN-SEC
                   ELSE
                       MOVE    1               TO  SPA-KT012GMN-CUR
                   END-IF
      *
               WHEN    OTHER
                   PERFORM 3001-INIT-SPA-SEC
           END-EVALUATE
           .
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       3001-INIT-SPA-SEC                  SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT1
           MOVE    1                   TO  FLG-GMN-INIT2
           INITIALIZE                  SPA-KT012-AREA
           INITIALIZE                  SPA-KT012KYOU-AREA
           MOVE    ZERO                TO  SPA-KT012-SELOK
           MOVE    SPA-RRK-PAGE        TO  SPA-KT012-RRK-PAGE
           MOVE    SPA-RRK-NEXT        TO  SPA-KT012-RRK-NEXT
      *H28.5
           MOVE    SPA-GENERICFLG      TO  SPA-KT012-GENERICFLG
      *
           PERFORM                     310-SPA-SET-SEC
      *    選択
           MOVE    SPA-NAI-DOSELNUM    TO  SPA-KT012-DOSELNUM
           MOVE    SPA-NAI-SELNUM      TO  SPA-KT012-SELNUM
           IF      SPA-KT012-DOSELNUM    >   ZERO
      *        診察内容編集
               PERFORM 4101-JYURRK-HEN-SEC
           ELSE
               MOVE    1               TO  SPA-KT012GMN-CUR
           END-IF
      *
           .
       3001-INIT-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
      *    医療機関設定
           MOVE    SPA-HOSPNUM         TO  SPA-KT012-HOSPNUM
           MOVE    SPA-PTID            TO  SPA-KT012-PTID
      *    入外区分設定
           MOVE    SPA-NYUGAIKBN       TO  SPA-KT012-NYUGAIKBN
           IF      SPA-K02N-TAIINYMD   =   SPACE
               MOVE    "2 外来"        TO  SPA-KT012-NYUGAIKBN-LST (1)
               MOVE    1               TO  SPA-KT012-NYUGAIKBN-MAX
           ELSE
               MOVE    "1 入院"        TO  SPA-KT012-NYUGAIKBN-LST (1)
               MOVE    "2 外来"        TO  SPA-KT012-NYUGAIKBN-LST (2)
               MOVE    2               TO  SPA-KT012-NYUGAIKBN-MAX
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KT012-NYUGAIKBN-MAX
               IF      SPA-KT012-NYUGAIKBN   =   SPA-KT012-NYUGAIKBN-LST
                                                           (IDX)(1:1)
                   MOVE    SPA-KT012-NYUGAIKBN-LST (IDX)
                                           TO  SPA-KT012-NYUGAIKBN-G
               END-IF
           END-PERFORM
      *
      *    診療区分
           INITIALIZE                  SPA-KT012-SRYKBN-LIST
           MOVE    "00 全体"           TO  SPA-KT012-SRYKBN-LST (01)
           MOVE    "10 医学管理・在宅" TO  SPA-KT012-SRYKBN-LST (02)
           MOVE    "20 投薬"           TO  SPA-KT012-SRYKBN-LST (03)
           MOVE    "30 注射"           TO  SPA-KT012-SRYKBN-LST (04)
           MOVE    "40 処置"           TO  SPA-KT012-SRYKBN-LST (05)
           MOVE    "50 手術・麻酔"     TO  SPA-KT012-SRYKBN-LST (06)
           MOVE    "60 検査・病理診断"
                                       TO  SPA-KT012-SRYKBN-LST (07)
           MOVE    "70 画像診断"       TO  SPA-KT012-SRYKBN-LST (08)
           MOVE    "80 リハビリ・その他"
                                       TO  SPA-KT012-SRYKBN-LST (09)
           MOVE    "90 自費・その他"   TO  SPA-KT012-SRYKBN-LST (10)
           MOVE    "29 投薬・注射"     TO  SPA-KT012-SRYKBN-LST (11)
      *
           MOVE    11                  TO  SPA-KT012-SRYKBN-MAX
           MOVE    SPA-DOSRYKBNFLG     TO  SPA-KT012-SRYKBN
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-KT012-SRYKBN-MAX )  OR
                              (FLG-OK  =   1        )
               IF      SPA-KT012-SRYKBN  =   SPA-KT012-SRYKBN-LST
                                                           (IDX)(1:2)
                   MOVE    SPA-KT012-SRYKBN-LST (IDX)
                                           TO  SPA-KT012-SRYKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-KT012-SRYKBN-LST (01) TO  SPA-KT012-SRYKBN-G
           END-IF
      *
      *    受診履歴編集
           PERFORM 3101-JYURRK-DBHEN-SEC
      *    紐付け患者の医療機関
           INITIALIZE                  SPA-KT012-GRPHOSPNUM-AREA
           IF      SPA-NAI-GRPKBN      =   1
               PERFORM 3103-GRPHOSPNUM-HEN-SEC
           END-IF
           .
       310-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    紐付け患者編集処理
      *****************************************************************
       3103-GRPHOSPNUM-HEN-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   9   )   OR
                              (SPA-NAI-GRPHOSPNUM (IDX)    =   ZERO )
               MOVE    SPA-NAI-GRPHOSPNUM (IDX)
                                           TO  SPA-KT012-GRPHOSPNUM-LST
                                                        (IDX)(1:2)
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY     >   9   )   OR
                                  (SPA-GRPHOSPNUM (IDY)
                                              =   ZERO )
                   IF      SPA-NAI-GRPHOSPNUM (IDX)
                                               =   SPA-GRPHOSPNUM (IDY)
                       MOVE    SPA-GRPHOSPNAME (IDY)
                                           TO  SPA-KT012-GRPHOSPNUM-LST
                                                         (IDX)(4:)
                   END-IF
               END-PERFORM
               MOVE    IDX            TO  SPA-KT012-GRPMAX
           END-PERFORM
           .
       3103-GRPHOSPNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴から編集処理
      *****************************************************************
       3101-JYURRK-DBHEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-KT012-DOSELNUM
           MOVE    ZERO                TO  SPA-KT012-SELNUM
      *
           INITIALIZE                  SPA-KT012GMN-RRKTBL
           INITIALIZE                  SPA-KT012NAI-RRKTBL
           INITIALIZE                  HZN-JYURRK-REC
      *    受診歴を検索する  医療機関、患者ＩＤ
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-KT012-HOSPNUM     TO  JYURRK-HOSPNUM
           MOVE    SPA-KT012-PTID        TO  JYURRK-PTID
           MOVE    SPA-KT012-NYUGAIKBN   TO  JYURRK-NYUGAIKBN
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
      *    開始位置指定
           IF      SPA-RRK-PAGE        =   ZERO
               MOVE    1               TO  SPA-KT012-RRK-PAGE
           END-IF
           COMPUTE IDX-SP              =  ((SPA-KT012-RRK-PAGE  -   1  )
                                       *   150  )
                                       +   1
           MOVE    ZERO                TO  IDX-S1
           MOVE    ZERO                TO  IDX-S2
           MOVE    ZERO                TO  SPA-KT012-RRK-NEXT
      *
           MOVE    ZERO                TO  IDX
           PERFORM
               UNTIL   (FLG-JYURRK         =   1   )  OR
                       (SPA-KT012-RRK-NEXT   =   1   )
               MOVE    ZERO                TO  IDX-S
               IF      JYURRK-EDANUM       =   1
                   ADD     1                   TO  IDX-S2
                   IF      IDX-S1              =   9999
                       MOVE    ZERO            TO  IDX-S1
                   END-IF
                   ADD     1                   TO  IDX-S1
               ELSE
                   IF     (JYURRK-SRYYMD       =   HZN-JYURRK-SRYYMD)
                     AND  (JYURRK-SRYKA        =   HZN-JYURRK-SRYKA )
                     AND  (JYURRK-HKNCOMBINUM  =
                                               HZN-JYURRK-HKNCOMBINUM )
                     AND  (JYURRK-RENNUM       =   HZN-JYURRK-RENNUM)
                     AND  (JYURRK-DOUJI-RENNUM =
                                               HZN-JYURRK-DOUJI-RENNUM)
                       MOVE    IDX-S1              TO  IDX-S
                   ELSE
                       ADD     1                   TO  IDX-S2
                       IF      IDX-S1              =   9999
                           MOVE    ZERO            TO  IDX-S1
                       END-IF
                       ADD     1                   TO  IDX-S1
                       DISPLAY "KT012 JYURRK EDANUM ERR??:"
                                                    JYURRK-KEY
                   END-IF
               END-IF
               IF      IDX-SP          <=   IDX-S2
      *            編集対象
                   IF      IDX-S           =   ZERO
                       ADD     1               TO  IDX
                       IF      IDX             >   150
                           MOVE    1           TO  SPA-KT012-RRK-NEXT
                       ELSE
                           MOVE    IDX         TO  SPA-KT012GMN-MAX1
                       END-IF
                   END-IF
                   IF      SPA-KT012-RRK-NEXT    =   ZERO
      *                明細項目退避
                       PERFORM 31011-JYURRK-SPAHEN-SEC
                   END-IF
               END-IF
      *
      **       明細項目退避
      **       PERFORM 31011-JYURRK-SPAHEN-SEC
      *
               MOVE    JYURRK-REC          TO  HZN-JYURRK-REC
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    診察日の再編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KT012GMN-MAX1
               MOVE    ZERO            TO  FLG-HEN
               PERFORM VARYING     IDX2    FROM    IDX BY  1
                       UNTIL       IDX2    >   SPA-KT012GMN-MAX1
                   IF     (IDX2            NOT =   IDX )  AND
                          (SPA-KT012NAI-RRKYMD  (IDX)
                                               =   SPA-KT012NAI-RRKYMD 
                                                          (IDX2)) AND
                          (SPA-KT012NAI-RRK-SRYKA(IDX)
                                           =   SPA-KT012NAI-RRK-SRYKA
                                                          (IDX2))
                       MOVE    1                   TO  FLG-HEN
                       MOVE    SPA-KT012GMN-MAX1     TO  IDX2
                   ELSE
                       IF     (SPA-KT012NAI-RRKYMD(IDX)
                                           NOT =   SPA-KT012NAI-RRKYMD
                                                       (IDX2))
                           MOVE    SPA-KT012GMN-MAX1 TO  IDX2
                       END-IF
                   END-IF
               END-PERFORM
               IF     (SPA-KT012NAI-RENNUM (IDX)        >   1 ) OR
                      (SPA-KT012NAI-DOUJI-RENNUM (IDX)  >  ZERO )
                   MOVE    1                   TO  FLG-HEN
               END-IF
               IF      FLG-HEN             =   1
                   MOVE    "("             TO  SPA-KT012GMN-RRKYMD (IDX)
                                                               (10:1)
                   MOVE    SPA-KT012NAI-RENNUM (IDX) 
                                           TO  SPA-KT012GMN-RRKYMD (IDX)
                                                              (11:1)
                   MOVE    ")"             TO  SPA-KT012GMN-RRKYMD (IDX)
                                                              (12:1)
                   IF      SPA-KT012NAI-DOUJI-RENNUM (IDX)  NOT =   ZERO
                       MOVE    SPA-KT012NAI-DOUJI-RENNUM (IDX) 
                                           TO  SPA-KT012GMN-RRKYMD (IDX)
                                                              (13:1)
                   END-IF
               END-IF
      *        複数診療科
               IF      SPA-KT012NAI-RRK-FUKU-KBN (IDX)  =   "1"  OR  "2"
                   MOVE    "F"             TO  SPA-KT012GMN-RRKYMD (IDX)
                                                              (13:1)
               END-IF
      *
               MOVE    SPA-KT012NAI-RRK-HKNCOMBI(IDX)
                                           TO  SPA-KT012GMN-RRKHKNCOMBI
                                                                  (IDX)
               IF      SPA-KT012NAI-RRK-HKNKBN  (IDX)  =   "1"
                                                     OR  "5"
                   MOVE    "労"            TO  SPA-KT012GMN-RRKHKNCOMBI
                                                                  (IDX)
               END-IF
               IF      SPA-KT012NAI-RRK-HKNKBN  (IDX)  =   "2"
                                                      OR  "4"
                   MOVE    "自"            TO  SPA-KT012GMN-RRKHKNCOMBI
                                                                  (IDX)
               END-IF
      *        科名
               PERFORM VARYING IDY     FROM    1   BY  1
                       UNTIL   IDY         >   SPA-SRYKA-MAX
                   IF      SPA-KT012NAI-RRK-SRYKA (IDX)
                                           =   SPA-GMN-SRYKAL(IDY)
                       MOVE    SPA-GMN-SRYKAMEIL(IDY)(1:2)
                                           TO  SPA-KT012GMN-RRKSRYKA
                                                        (IDX)
                       MOVE    SPA-SRYKA-MAX   TO  IDY
                   END-IF
               END-PERFORM
      *!!!
               IF      SPA-KT012GMN-RRKSRYKA (IDX) =   SPACE
                   MOVE   SPA-KT012NAI-RRK-SRYKA (IDX)
                                           TO  SPA-KT012GMN-RRKSRYKA
                                                        (IDX)
               END-IF
      *!!!
           END-PERFORM
      *
           .
       3101-JYURRK-DBHEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       31011-JYURRK-SPAHEN-SEC             SECTION.
      *
      **** MOVE    IDX                 TO  SPA-KT012GMN-NO    (IDX)
           MOVE    IDX-S1              TO  SPA-KT012GMN-NO (IDX)
      *    連番
           MOVE    JYURRK-RENNUM       TO  SPA-KT012NAI-RENNUM(IDX)
      *    連番
           MOVE    JYURRK-DOUJI-RENNUM TO  SPA-KT012NAI-DOUJI-RENNUM
                                                               (IDX)
      *    伝票番号
           MOVE    JYURRK-DENPNUM      TO  SPA-KT012NAI-RRK-DENPNUM
                                                              (IDX)
      *    診察日
           MOVE    JYURRK-SRYYMD       TO  SPA-KT012NAI-RRKYMD(IDX)
           MOVE    JYURRK-SRYYMD       TO  WRK-SYMD
           PERFORM 520-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-KT012GMN-RRKYMD (IDX)
      *    保険区分
           MOVE    JYURRK-HKNKBN       TO  SPA-KT012NAI-RRK-HKNKBN (IDX)
      *    保険組合せ
           MOVE    JYURRK-HKNCOMBINUM  TO  SPA-KT012NAI-RRK-HKNCOMBI
                                                                  (IDX)
      *    ドクター
           MOVE    JYURRK-DRCD         TO  SPA-KT012NAI-RRK-DRCD  (IDX)
      *    診療科
           MOVE    JYURRK-SRYKA        TO  SPA-KT012NAI-RRK-SRYKA  (IDX)
      *    複数診療科・保険
           MOVE    JYURRK-GRP-DENPNUM TO  SPA-KT012NAI-GRP-DENPNUM (IDX)
           MOVE    JYURRK-GRP-RENNUM  TO  SPA-KT012NAI-GRP-RENNUM (IDX)
           MOVE    JYURRK-SYU-UPDKBN  TO  SPA-KT012NAI-SYU-UPDKBN (IDX)
      *
           MOVE    IDX                 TO  SPA-KT012GMN-MAX1
      *
           .
       31011-JYURRK-SPAHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 220-CLEAR-SEC
      *        受診履歴へ
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 450-RIREKI-SEC
      *        入外区分変更
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 460-NYUGAIKBN-HEN-SEC
      *        診療区分変更
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 470-SRYKBN-HEN-SEC
      *        医療機関番号変更
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 430-GRPHOSPNUM-HEN-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                    PERFORM 450-TOROKU-SEC
      *        受診履歴 前頁
               WHEN    "CLICKED"       ALSO    "BC06"
                   PERFORM 4109-BC06-SYORI-SEC
      *        受診履歴 次頁
               WHEN    "CLICKED"       ALSO    "BC07"
                   PERFORM 4109-BC07-SYORI-SEC
      *H28.5
      *        処方名変換
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 440-GENERIC-HEN-SEC
           END-EVALUATE
          .
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        診療日選択番号
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 410-SELNUM-CHK-SEC
      *        診療日選択
               WHEN    ANY             ALSO    "RRKLIST"
                   PERFORM 430-RRKLIST-CHK-SEC
      *        明細選択
               WHEN    ANY             ALSO    "MEILIST"
                   PERFORM 440-MEILIST-CHK-SEC
      *        剤番号選択
               WHEN    "ACTIVATE"      ALSO    "DOSEL"
                   PERFORM 420-DOSEL-CHK-SEC
      *        入外区分
               WHEN    "ACTIVATE"      ALSO    "NYUGAIKBN"
                   PERFORM 450-NYUGAIKBN-CHK-SEC
      *        診療区分
               WHEN    "ACTIVATE"      ALSO    "SRYKBN"
                   PERFORM 460-SRYKBNN-CHK-SEC
      *        医療機関番号
               WHEN    "ACTIVATE"      ALSO    "GRPHOSPNUM"
                   PERFORM 460-GRPHOSPNUM-CHK-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日選択番号入力チェック処理
      *****************************************************************
       410-SELNUM-CHK-SEC              SECTION.
      *
           MOVE    1                   TO  SPA-KT012GMN-CUR
      *
           MOVE    KT012-SELNUM        TO  SPA-KT012-SELNUM
           MOVE    ZERO                TO  SPA-KT012-DOSELNUM
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-KT012GMN-MAX1
              IF      SPA-KT012GMN-NO (IDX)      =   SPA-KT012-SELNUM
                  MOVE    IDX                    TO  SPA-KT012-DOSELNUM
                  MOVE    SPA-KT012GMN-MAX1      TO  IDX
              END-IF
           END-PERFORM
      *
           IF      SPA-KT012-DOSELNUM    <   01   OR
                                       >   SPA-KT012GMN-MAX1
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
      *        診察内容編集
               PERFORM 4101-JYURRK-HEN-SEC
           END-IF
      *
           .
       410-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日選択処理
      *****************************************************************
       430-RRKLIST-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-DOSELNUM
           MOVE    ZERO                TO  WRK-IDXNUM
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-KT012GMN-MAX1
              IF      KT012-RRKLIST-SELECT(IDX)  =  "T"
                  IF      IDX                  =   SPA-KT012-DOSELNUM
                      CONTINUE
                  ELSE
                      MOVE    SPA-KT012GMN-NO (IDX)  TO  WRK-IDXNUM
                      MOVE    IDX                  TO  WRK-DOSELNUM
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-DOSELNUM        >   ZERO
               MOVE    WRK-DOSELNUM        TO  SPA-KT012-DOSELNUM
           END-IF
      *
           IF      SPA-KT012-DOSELNUM  <   01   OR
                                       >   SPA-KT012GMN-MAX1
               CONTINUE
           ELSE
               MOVE    KT012-NO  (SPA-KT012-DOSELNUM)
                                           TO  SPA-KT012-SELNUM
      *        診察内容編集
               PERFORM 4101-JYURRK-HEN-SEC
           END-IF
      *
           .
       430-RRKLIST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細選択選択処理
      *****************************************************************
       440-MEILIST-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-BANGO
           MOVE    ZERO                TO  WRK-SELNUM-CHK-G
           MOVE    ZERO                TO  IDX2
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-GMN-MAX2
              IF      KT012-MEILIST-SELECT(IDX)  =  "T"
                  MOVE    SPA-NAI-TBANGO (IDX) TO  WRK-BANGO
      *           同じ剤選択は削除とする
                  MOVE    ZERO             TO  FLG-CHK
                  PERFORM VARYING  IDY     FROM   1   BY  1
                          UNTIL   (IDY     >   MAX-SEL )  OR
                                  (WRK-SELNUM-CHK (IDY)   =   ZERO)
                      IF      WRK-SELNUM-CHK (IDY)
                                           =   WRK-BANGO
                          MOVE    1        TO  FLG-CHK
                          MOVE    ZERO     TO  WRK-SELNUM-CHK (IDY)
                      END-IF
                  END-PERFORM
                  IF      FLG-CHK          =   ZERO
                      ADD     1                TO  IDX2
                      MOVE    SPA-NAI-TBANGO (IDX)
                                           TO  WRK-SELNUM-CHK (IDX2)
                  END-IF
              END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  WRK-HEN-SELNUM
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   MAX-SEL )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      (WRK-SELNUM-CHK (IDX)   NOT =   ZERO) 
                   MOVE    WRK-SELNUM-CHK (IDX)    TO  WRK-BANGO
                   PERFORM 4401-SELNUM-CHK-HEN-SEC
               END-IF
           END-PERFORM
      *
           MOVE    WRK-HEN-SELNUM      TO  SPA-GMN-DOSEL
      *
           IF      SPA-GMN-DOSEL       =   SPACE
               INITIALIZE                  SPA-NAI-DOSEL-G
           ELSE
               MOVE    SPACE               TO  SPA-ERRCD
      *        選択番号チェック
               PERFORM 4201-DOSEL-CHECK-SEC
           END-IF
      *
           .
       440-MEILIST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号編集処理
      *****************************************************************
       4401-SELNUM-CHK-HEN-SEC             SECTION.
      *
           MOVE    WRK-BANGO           TO  WRK-ZZ
           MOVE    SPACE               TO  WRK-BANGO-X
           MOVE    ZERO                TO  IDX-D1
           PERFORM VARYING     IDX-D2  FROM    1   BY  1
                   UNTIL       IDX-D2  >   3
               IF      WRK-ZZ (IDX-D2:1)   NOT =   SPACE
                   ADD     1                   TO  IDX-D1
                   MOVE    WRK-ZZ (IDX-D2:1)   TO  WRK-BANGO-X(IDX-D1:1)
               END-IF
           END-PERFORM
           COMPUTE IDX-D2              =   40  -  IDX-D1
           IF      WRK-HEN-SELNUM (IDX-D2:)    NOT =   SPACE
               MOVE    "0037"              TO  SPA-ERRCD
               MOVE    SPACE               TO  WRK-BANGO-X
           END-IF
      *
           MOVE    WRK-HEN-SELNUM      TO  WRK-HEN-SELNUM-2
           MOVE    SPACE               TO  WRK-HEN-SELNUM
           IF      WRK-HEN-SELNUM-2    =   SPACE
               STRING  WRK-BANGO-X         DELIMITED   BY  SPACE
                       INTO                WRK-HEN-SELNUM
               END-STRING
           ELSE
               IF      WRK-BANGO-X         =   SPACE
                   MOVE    WRK-HEN-SELNUM-2    TO  WRK-HEN-SELNUM
               ELSE
                   STRING  WRK-HEN-SELNUM-2    DELIMITED   BY  SPACE
                           ","                 DELIMITED   BY  SIZE
                           WRK-BANGO-X         DELIMITED   BY  SPACE
                           INTO                WRK-HEN-SELNUM
                   END-STRING
               END-IF
           END-IF
      *
           .
       4401-SELNUM-CHK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤番号選択入力チェック処理
      *****************************************************************
       420-DOSEL-CHK-SEC              SECTION.
      *
           MOVE    2                   TO  SPA-KT012GMN-CUR
           MOVE    KT012-DOSEL           TO  SPA-GMN-DOSEL
      *    選択番号チェック
           PERFORM 4201-DOSEL-CHECK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      420-DOSEL-CHK-EXT
           END-IF
      *
      *
           INITIALIZE                      WRK-DOZAINUM-AREA
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDX-D2      FROM    1   BY  1
                   UNTIL      (IDX-D2      >   MAX-SEL  )  OR
                              (WRK-DO(IDX-D2)  =   SPACE  )
               IF     (IDX-D2                  >   1   )  AND
                      (WRK-SEL  (IDX-D2 - 1)   =   "-" )
                   COMPUTE IDX-Z           =   WRK-DONUM (IDX-D2 - 1)
                                           +   1
                   PERFORM VARYING     IDX2    FROM    IDX-Z  BY  1
                           UNTIL       IDX2    >=  WRK-DONUM(IDX-D2)
                       MOVE    IDX2                TO  IDX-ZAI
                       ADD     1                   TO  IDX
                       MOVE    SPA-NAI-ZAINUM (IDX-ZAI)
                                               TO  WRK-DOZAINUM (IDX)
                       MOVE    WRK-KAINUM(IDX-D2)  TO  WRK-DOKAI (IDX)
      *                投薬以外の回数はエラー
                       IF     (WRK-KAINUM(IDX-D2)  >   1  )  AND
                              (SPA-NAI-SRYKBN(IDX-ZAI) NOT =   "21"
                                                           AND "22"
                                                           AND "23")
                           MOVE    "0004"              TO  SPA-ERRCD
                       END-IF
                   END-PERFORM
               END-IF
               MOVE    WRK-DONUM(IDX-D2)       TO  IDX-ZAI
               ADD     1                       TO  IDX
               MOVE    SPA-NAI-ZAINUM (IDX-ZAI)
                                               TO  WRK-DOZAINUM (IDX)
               MOVE    WRK-KAINUM(IDX-D2)      TO  WRK-DOKAI (IDX)
      *        '-'があり、回数が無い時後の回数
               IF     (WRK-SEL  (IDX-D2)       =   "-" )  AND
                      (WRK-KAINUM(IDX-D2)      =   ZERO)
                   MOVE    WRK-KAINUM(IDX-D2 + 1)  TO  WRK-DOKAI (IDX)
               END-IF
      *
      *        投薬以外の回数はエラー
               IF     (WRK-KAINUM(IDX-D2)      >   1  )  AND
                      (SPA-NAI-SRYKBN(IDX-ZAI) NOT =   "21"
                                                   AND "22"
                                                   AND "23")
                   MOVE    "0004"              TO  SPA-ERRCD
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      420-DOSEL-CHK-EXT
           END-IF
      *
      *
      *    同一剤を削除
           INITIALIZE                      HZN-DOZAINUM-AREA
           MOVE    WRK-DOZAINUM-AREA   TO  HZN-DOZAINUM-AREA
           INITIALIZE                      WRK-DOZAINUM-AREA
           MOVE    ZERO                TO  IDX-H
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1    >   IDX
               IF      HZN-DOZAINUM (IDX1) NOT =   ZERO
                   MOVE    ZERO                TO  FLG-CHK
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL  (IDX2        >   MAX-SEL)    OR
                                  (WRK-DOZAINUM (IDX2) =   ZERO ) OR
                                  (FLG-CHK             =   1    )
                       IF       HZN-DOZAINUM (IDX1)    =   WRK-DOZAINUM
                                                                (IDX2)
                           MOVE    1                   TO  FLG-CHK
                       END-IF
                   END-PERFORM
                   IF      FLG-CHK             =   ZERO
                       ADD     1               TO  IDX-H
                       MOVE    HZN-DOZAINUM-OCC (IDX1)
                                               TO  WRK-DOZAINUM-OCC
                                                                (IDX-H)
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      IDX-H               =   ZERO
               CONTINUE
           ELSE
      *        対象診療行為内容編集
               PERFORM 4201-WKSRYACT-SET-SEC
      *        戻る
               PERFORM 210-BACK
           END-IF
      *
           .
       420-DOSEL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     選択番号チェック処理
      *****************************************************************
       4201-DOSEL-CHECK-SEC              SECTION.
      *
           INITIALIZE                      WRK-DOSEL
           MOVE    ZERO                TO  FLG-KAI
           MOVE    ZERO                TO  IDX-D1
           MOVE    ZERO                TO  IDX-D3
           MOVE    1                   TO  IDX-D2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   40  )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               EVALUATE    SPA-GMN-DOSEL(IDX:1)
                   WHEN    ","
                   WHEN    "."
                       IF      IDX-D1          >   ZERO
                           ADD     1               TO  IDX-D2
                           MOVE    ZERO            TO  IDX-D1
                           MOVE    ZERO            TO  IDX-D3
                           MOVE    ZERO            TO  FLG-KAI
                       END-IF
                   WHEN    "*"
      *                １桁目の＊は全体
                       IF     (IDX-D2          =   1  ) AND
                              (IDX-D1          =   ZERO)
                           ADD     1               TO  IDX-D1
                           MOVE    SPA-GMN-DOSEL(IDX:1)
                                               TO  WRK-DO(IDX-D2)
                                                            (IDX-D1:1)
                       ELSE
                           IF     (IDX-D1      =   ZERO) OR
                                  (FLG-KAI     =   1   )
      *                        １桁目の＊はエラー
                               MOVE    "0003"          TO  SPA-ERRCD
                           ELSE
      *                        回数
                               MOVE    1               TO  FLG-KAI
                           END-IF
                       END-IF
                   WHEN    "0"  THRU  "9" 
                     IF      FLG-KAI         =   ZERO
      *
                       ADD     1               TO  IDX-D1
                       IF      IDX-D1          >   3
                           MOVE    "0002"          TO  SPA-ERRCD
                       ELSE
                           MOVE    SPA-GMN-DOSEL(IDX:1)
                                               TO  WRK-DO(IDX-D2)
                                                            (IDX-D1:1)
                       END-IF
      *
                     ELSE
      *
                       ADD     1               TO  IDX-D3
                       IF      IDX-D3         >   3
                           MOVE    "0002"          TO  SPA-ERRCD
                       ELSE
                           MOVE    SPA-GMN-DOSEL(IDX:1)
                                               TO  WRK-KAI(IDX-D2)
                                                            (IDX-D3:1)
                       END-IF
                     END-IF
                   WHEN    "-"
                       MOVE    "-"             TO  WRK-SEL(IDX-D2)
                       IF      IDX-D1          >   ZERO
                           ADD     1               TO  IDX-D2
                           MOVE    ZERO            TO  IDX-D1
                           MOVE    ZERO            TO  IDX-D3
                           MOVE    ZERO            TO  FLG-KAI
                       END-IF
                   WHEN    SPACE
                       CONTINUE
                   WHEN    OTHER
                       MOVE    "0002"          TO  SPA-ERRCD
               END-EVALUATE
           END-PERFORM
      *    全件コピー
           IF     (IDX-D2              =   1  )  AND
                  (IDX-D1              =   1  )  AND
                  (WRK-DO(IDX-D2)(1:1) =   "*")
               MOVE    "1"                 TO  WRK-DO(IDX-D2)
               MOVE    "-"                 TO  WRK-SEL(IDX-D2)
               MOVE    SPA-NAI-MAX2(2:3)   TO  WRK-DO(IDX-D2 + 1)
               MOVE    1                   TO  FLG-DOALL
           ELSE
               MOVE    ZERO                TO  FLG-DOALL
           END-IF
      *
           PERFORM VARYING     IDX-D2      FROM    1   BY  1
                   UNTIL      (IDX-D2      >   MAX-SEL  )  OR
                              (WRK-DO(IDX-D2)  =   SPACE  )  OR
                              (SPA-ERRCD   NOT =   SPACE  )
               INITIALIZE                      ORCSNUMAREA
               MOVE    WRK-DO (IDX-D2)     TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "0002"              TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-DONUM (IDX-D2)
                   IF      WRK-DONUM (IDX-D2)  >=  01  AND
                                               <=  SPA-NAI-MAX2
                       CONTINUE
                   ELSE
                       MOVE    "0002"          TO  SPA-ERRCD
                   END-IF
      *
                   IF     (IDX-D2          >   1   )  AND
                          (WRK-SEL (IDX-D2 - 1 )   =   "-" )
                       IF      WRK-DONUM (IDX-D2 - 1)  >
                                                   WRK-DONUM(IDX-D2)
                           MOVE    "0002"              TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
      *        回数
               IF     WRK-KAI(IDX-D2)      =   SPACE
                   MOVE    ZERO            TO  WRK-KAINUM(IDX-D2)
               ELSE
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    WRK-KAI(IDX-D2)     TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF     (SNUM-RC         NOT =   ZERO  )   OR
                          (SNUM-MINKBN     NOT =   SPACE )   OR
                          (SNUM-SYOKBN     NOT =   SPACE )
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    SNUM-OUTNUM     TO  WRK-KAINUM(IDX-D2)
                   END-IF
               END-IF
           END-PERFORM
      *    画面表示用
           INITIALIZE                      SPA-NAI-DOSEL-G
           INITIALIZE                      WRK-SELNUM-AREA
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDX-D2      FROM    1   BY  1
                   UNTIL      (IDX-D2      >   MAX-SEL  )  OR
                              (WRK-DO (IDX-D2)
                                               =   SPACE  )
                          OR  (SPA-ERRCD   NOT =   SPACE )
               IF     (IDX-D2                  >   1   )  AND
                      (WRK-SEL (IDX-D2 - 1)    =   "-" )
                   COMPUTE IDX-Z           =   WRK-DONUM(IDX-D2 - 1)
                                           +   1
                   PERFORM VARYING     IDX2    FROM    IDX-Z  BY  1
                           UNTIL       IDX2    >=  WRK-DONUM(IDX-D2)
                       MOVE    1               TO  WRK-SEL9(IDX2)
                   END-PERFORM
               END-IF
               MOVE    WRK-DONUM(IDX-D2)       TO  IDX2
               MOVE    1                       TO  WRK-SEL9(IDX2)
           END-PERFORM
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDX-D2  FROM    1   BY  1
                   UNTIL      (IDX-D2  >   MAX-SEL  )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      WRK-SEL9(IDX-D2)    =   1
                   ADD     1               TO  IDX
                   IF      IDX             >   MAX-SEL
                       MOVE    "0008"          TO  SPA-ERRCD
                   ELSE
                       MOVE    IDX-D2          TO  SPA-NAI-DOSEL(IDX)
                       MOVE    IDX             TO  SPA-NAI-DOSEL-MAX
                   END-IF
               END-IF
           END-PERFORM
           .
       4201-DOSEL-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *     入外区分処理
      *****************************************************************
       450-NYUGAIKBN-CHK-SEC              SECTION.
      *
           MOVE    SPA-KT012-NYUGAIKBN   TO  WRK-MAE-NYUGAIKBN
      *
           MOVE    KT012-NYUGAIKBN       TO  SPA-KT012-NYUGAIKBN-G
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KT012-NYUGAIKBN-MAX
               IF      SPA-KT012-NYUGAIKBN   =   SPA-KT012-NYUGAIKBN-LST
                                                           (IDX)(1:1)
                   MOVE    SPA-KT012-NYUGAIKBN-LST (IDX)
                                           TO  SPA-KT012-NYUGAIKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    10                  TO  SPA-KT012GMN-CUR
           ELSE
             IF      SPA-KT012-NYUGAIKBN   =   WRK-MAE-NYUGAIKBN
               CONTINUE
             ELSE
      *        入外区分変更次は、１頁から表示
               MOVE    1                   TO  SPA-KT012-RRK-PAGE
               MOVE    ZERO                TO  SPA-KT012-RRK-NEXT
      *        受診履歴編集
               PERFORM 3101-JYURRK-DBHEN-SEC
               INITIALIZE                  SPA-KT012GMN-TBL
               INITIALIZE                  SPA-KT012NAI-TBL
               MOVE    ZERO                TO  SPA-GMN-MAX2
               MOVE    ZERO                TO  SPA-NAI-MAX2
               IF      SPA-KT012GMN-MAX1     >   ZERO
                   MOVE    1                   TO  SPA-KT012-DOSELNUM
                   MOVE    SPA-KT012GMN-NO(1)  TO  SPA-KT012-SELNUM
      *            診察内容編集
                   PERFORM 4101-JYURRK-HEN-SEC
               ELSE
                   MOVE    10              TO  SPA-KT012GMN-CUR
               END-IF
             END-IF
      *
           END-IF
           .
       450-NYUGAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療区分処理
      *****************************************************************
       460-SRYKBNN-CHK-SEC              SECTION.
      *
           MOVE    KT012-SRYKBN          TO  SPA-KT012-SRYKBN-G
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KT012-SRYKBN-MAX
               IF      SPA-KT012-SRYKBN   =   SPA-KT012-SRYKBN-LST
                                                           (IDX)(1:2)
                   MOVE    SPA-KT012-SRYKBN-LST (IDX)
                                           TO  SPA-KT012-SRYKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
               MOVE    11                  TO  SPA-KT012GMN-CUR
           ELSE
               IF      SPA-KT012-DOSELNUM    >   ZERO
                   PERFORM 4101-JYURRK-HEN-SEC
               ELSE
                   MOVE    1                   TO  SPA-KT012GMN-CUR
               END-IF
           END-IF
           .
       460-SRYKBNN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療区分処理
      *****************************************************************
       460-GRPHOSPNUM-CHK-SEC              SECTION.
      *
           MOVE    SPA-KT012-GRPHOSPNUM  TO  WRK-MAE-GRPHOSPNUM
      *
           MOVE    KT012-GRPHOSPNUM      TO  SPA-KT012-GRPHOSPNUM-G
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KT012-SRYKBN-MAX
               IF      SPA-KT012-GRPHOSPNUM
                                           =   SPA-KT012-GRPHOSPNUM-LST
                                                           (IDX)(1:2)
                   MOVE    SPA-KT012-GRPHOSPNUM-LST (IDX)
                                           TO  SPA-KT012-GRPHOSPNUM-G
                   MOVE    SPA-NAI-GRPPTID (IDX)
                                           TO  SPA-KT012-PTID
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      SPA-KT012-GRPHOSPNUM  =   SPACE
               MOVE    1                   TO  FLG-OK
           END-IF
           MOVE    ZERO                TO  FLG-HENSYU
           IF      FLG-OK              =   ZERO
               MOVE    "0009"              TO  SPA-ERRCD
               MOVE    12                  TO  SPA-KT012GMN-CUR
           ELSE
               IF      SPA-KT012-GRPHOSPNUM  =   WRK-MAE-GRPHOSPNUM
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-HENSYU
               END-IF
           END-IF
           IF      FLG-HENSYU          =   1
               IF      SPA-KT012-GRPHOSPNUM  =   SPACE
                   MOVE    SPA-HOSPNUM         TO  SPA-KT012-HOSPNUM
                   MOVE    SPA-PTID            TO  SPA-KT012-PTID
               ELSE
                   MOVE    SPA-KT012-GRPHOSPNUM  TO  SPA-KT012-HOSPNUM
               END-IF
      *        入外区分変更次は、１頁から表示
               MOVE    1                   TO  SPA-KT012-RRK-PAGE
               MOVE    ZERO                TO  SPA-KT012-RRK-NEXT
      *        受診履歴編集
               PERFORM 3101-JYURRK-DBHEN-SEC
               INITIALIZE                  SPA-KT012GMN-TBL
               INITIALIZE                  SPA-KT012NAI-TBL
               MOVE    ZERO                TO  SPA-GMN-MAX2
               MOVE    ZERO                TO  SPA-NAI-MAX2
               IF      SPA-KT012GMN-MAX1     >   ZERO
                   MOVE    1                     TO  SPA-KT012-DOSELNUM
                   MOVE    SPA-KT012GMN-NO(1)    TO  SPA-KT012-SELNUM
      *            診察内容編集
                   PERFORM 4101-JYURRK-HEN-SEC
               ELSE
                   PERFORM 41012-JYURRK-NASI-SEC
                   MOVE    12                  TO  SPA-KT012GMN-CUR
               END-IF
           END-IF
           .
       460-GRPHOSPNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     診察内容編集処理
      *****************************************************************
       4101-JYURRK-HEN-SEC              SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT2
      *
      *    パラメタファイルより、診療会計マスターを編集
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDG
      *
           INITIALIZE                  SPA-GMN-DOSEL
           INITIALIZE                  SPA-NAI-DOSEL-G
           MOVE    SPACE               TO  SPA-KT012GMN-TBL
           INITIALIZE                  SPA-KT012GMN-TBL
           INITIALIZE                  SPA-KT012NAI-TBL
           MOVE    ZERO                TO  SPA-GMN-MAX2
           MOVE    ZERO                TO  SPA-NAI-MAX2
      *
      *H28.5
           MOVE    SPACE               TO  WRK-FUKA-INPUT
           MOVE    SPA-KT012NAI-RRKYMD(SPA-KT012-DOSELNUM)
                                       TO  WRK-SRYYMD
      *H29.3
      *    後発品変更可（処方）の対応
           IF     (WRK-SRYYMD          >=  "20160401" )
             AND  (SPA-KT012-GENERICFLG  =   "1"   )
             AND  (SPA-KT012-SRYKBN    =   "00"
                                       OR  "20"
                                       OR  "29"
                                       OR  "99" )
               PERFORM 41012-KOUHATU-CHEK-SEC
           END-IF
      *
      *    受診歴を検索する
           INITIALIZE                  JYURRK-KEY
           MOVE    SPA-KT012-HOSPNUM     TO  JYURRK-HOSPNUM
           MOVE    SPA-KT012-PTID        TO  JYURRK-PTID
           MOVE    SPA-KT012-NYUGAIKBN   TO  JYURRK-NYUGAIKBN
           MOVE    SPA-KT012NAI-RRK-SRYKA(SPA-KT012-DOSELNUM)
                                       TO  JYURRK-SRYKA
           MOVE    SPA-KT012NAI-RRKYMD(SPA-KT012-DOSELNUM)
                                       TO  JYURRK-SRYYMD
           MOVE    SPA-KT012NAI-RENNUM(SPA-KT012-DOSELNUM)
                                       TO  JYURRK-RENNUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK2
           PERFORM
                   UNTIL    FLG-JYURRK     =   1
               IF      SPA-KT012NAI-DOUJI-RENNUM (SPA-KT012-DOSELNUM)
                                           =   JYURRK-DOUJI-RENNUM
      *
                   PERFORM VARYING     IDX-R   FROM    1   BY  1
                           UNTIL      (IDG             >   MAX-LINE ) OR
                                      (IDX-R           >   15  )
                       IF      JYURRK-ZAINUM (IDX-R)   NOT =   ZERO
                           PERFORM 41011-ZAISET-HEN-SEC
                       END-IF
                   END-PERFORM
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    最大件数
           MOVE    IDG                 TO  SPA-GMN-MAX2
      *
      *H28.5
      *    一般名対応
           IF     (WRK-SRYYMD              >=  "20160401" )
             AND  (SPA-KT012-GENERICFLG    =   "1"   )
             AND  (WRK-FUKA-INPUT          =   SPACE
                                           OR  "2"   )
             AND  (SPA-GMN-MAX2            >   ZERO  )
               PERFORM 30010-GENE-HEN-SEC
           END-IF
      *
           IF      SPA-GMN-MAX2        >   ZERO
               MOVE    2                   TO  SPA-KT012GMN-CUR
           ELSE
               PERFORM 41012-JYURRK-NASI-SEC
               MOVE    1                   TO  SPA-KT012GMN-CUR
           END-IF
      *
           .
       4101-JYURRK-HEN-EXT.
           EXIT.
      *
      *H29.3
      *****************************************************************
      *     後発品変更可（処方）の対応処理
      *****************************************************************
       41012-KOUHATU-CHEK-SEC           SECTION.
      *
      *    受診歴を検索する
           INITIALIZE                  JYURRK-KEY
           MOVE    SPA-KT012-HOSPNUM   TO  JYURRK-HOSPNUM
           MOVE    SPA-KT012-PTID      TO  JYURRK-PTID
           MOVE    SPA-KT012-NYUGAIKBN TO  JYURRK-NYUGAIKBN
           MOVE    SPA-KT012NAI-RRK-SRYKA(SPA-KT012-DOSELNUM)
                                       TO  JYURRK-SRYKA
           MOVE    SPA-KT012NAI-RRKYMD(SPA-KT012-DOSELNUM)
                                       TO  JYURRK-SRYYMD
           MOVE    SPA-KT012NAI-RENNUM(SPA-KT012-DOSELNUM)
                                       TO  JYURRK-RENNUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK2
           PERFORM
                   UNTIL    FLG-JYURRK     =   1
               IF      SPA-KT012NAI-DOUJI-RENNUM (SPA-KT012-DOSELNUM)
                                           =   JYURRK-DOUJI-RENNUM
      *            診療区分の判定
                   PERFORM VARYING     IDX-R   FROM    1   BY  1
                           UNTIL      (IDG             >   MAX-LINE ) OR
                                      (IDX-R           >   15  )
                       IF      JYURRK-ZAINUM (IDX-R)   NOT =   ZERO
                           PERFORM 41012-ZAINUM-CHK-SEC
                       END-IF
                   END-PERFORM
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       41012-KOUHATU-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療行為検索処理処理
      *****************************************************************
       41012-ZAINUM-CHK-SEC           SECTION.
      *
           INITIALIZE                  SRYACT-REC
           MOVE    JYURRK-HOSPNUM      TO  SRY-HOSPNUM
           MOVE    JYURRK-NYUGAIKBN    TO  SRY-NYUGAIKBN
           MOVE    SPA-KT012-PTID      TO  SRY-PTID
           MOVE    JYURRK-SRYKA        TO  SRY-SRYKA
           MOVE    JYURRK-SRYYMD(1:6)  TO  SRY-SRYYM
           MOVE    JYURRK-ZAINUM(IDX-R)
                                       TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           MOVE    ZERO                TO  WRK-ZAINUM
           PERFORM
                   UNTIL  (FLG-SRYACT      =   1   )
                      OR  (WRK-FUKA-INPUT  NOT =   SPACE)
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX         >   5   )
                              OR  (SRY-SRYCD (IDX) =   SPACE )
                              OR  (WRK-FUKA-INPUT  NOT =   SPACE)
                   EVALUATE    SRY-SRYCD (IDX)
                       WHEN    "099209910"
                           MOVE    "1"         TO  WRK-FUKA-INPUT
                       WHEN    "099209911"
                           MOVE    "2"         TO  WRK-FUKA-INPUT
                   END-EVALUATE
               END-PERFORM
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       41012-ZAINUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     診察内容なし編集処理
      *****************************************************************
       41012-JYURRK-NASI-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-DOSEL
           INITIALIZE                  SPA-NAI-DOSEL-G
           MOVE    SPACE               TO  SPA-KT012GMN-TBL
           INITIALIZE                  SPA-KT012GMN-TBL
           INITIALIZE                  SPA-KT012NAI-TBL
           MOVE    ZERO                TO  SPA-GMN-MAX2
           MOVE    ZERO                TO  SPA-NAI-MAX2
      *
           MOVE    "＝＝＝＝＝＝＝＝　表示内容なし　＝＝＝＝＝＝＝＝"
                                       TO  SPA-GMN-TMEISYO (01)
           MOVE    1                   TO  SPA-GMN-MAX2
           .
       41012-JYURRK-NASI-EXT.
           EXIT.
      *****************************************************************
      *    診療区分の判定処理
      *****************************************************************
       41012-SRYKBN-CHK-SEC           SECTION.
      *
           EVALUATE    SPA-KT012-SRYKBN
               WHEN    "10"
                   IF      (JYURRK-SRYKBN1     =   "01" )
                       MOVE    1                   TO  FLG-OK2
                   END-IF
               WHEN    "20"
                   IF      (JYURRK-SRYKBN2     =   "01" )  OR
                           (JYURRK-SRYKBN3     =   "01" )  OR
                           (JYURRK-SRYKBN4     =   "01" )
                       MOVE    1                   TO  FLG-OK2
                   END-IF
               WHEN    "29"
      *            投薬・注射
                   IF     ((JYURRK-SRYKBN2     =   "01" )  OR
                           (JYURRK-SRYKBN3     =   "01" )  OR
                           (JYURRK-SRYKBN4     =   "01" ) )  OR
                          ( JYURRK-SRYKBN5     =   "01"   )
                       MOVE    1                   TO  FLG-OK2
                   END-IF
               WHEN    "30"
                   IF      (JYURRK-SRYKBN5     =   "01" )
                       MOVE    1                   TO  FLG-OK2
                   END-IF
               WHEN    "40"
                   IF      (JYURRK-SRYKBN6     =   "01" )
                       MOVE    1                   TO  FLG-OK2
                   END-IF
               WHEN    "50"
                   IF      (JYURRK-SRYKBN7     =   "01" )  OR
                           (JYURRK-SRYKBN8     =   "01" )
                       MOVE    1                   TO  FLG-OK2
                   END-IF
               WHEN    "60"
                   IF      (JYURRK-SRYKBN9     =   "01" )
                       MOVE    1                   TO  FLG-OK2
                   END-IF
               WHEN    "70"
                   IF      (JYURRK-SRYKBN10    =   "01" )
                       MOVE    1                   TO  FLG-OK2
                   END-IF
               WHEN    "80"
                   IF      (JYURRK-SRYKBN11    =   "01" )
                       MOVE    1                   TO  FLG-OK2
                   END-IF
           END-EVALUATE
           .
       41012-SRYKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       41011-ZAISET-HEN-SEC           SECTION.
      *
      *    ワーク診療行為のレイアウトへ編集後、画面用編集へ
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
           MOVE    ZERO                TO  IDX-KOUI
           MOVE    ZERO                TO  IDX-KAI
      *    対象日の位置セット
           MOVE    JYURRK-SRYYMD(7:2)  TO  IDX-STR
           MOVE    IDX-STR             TO  SPA-SEL-IDX
           MOVE    JYURRK-RENNUM       TO  SPA-SEL-RENNUM
      *
           INITIALIZE                  SRYACT-REC
           MOVE    JYURRK-HOSPNUM      TO  SRY-HOSPNUM
           MOVE    JYURRK-NYUGAIKBN    TO  SRY-NYUGAIKBN
           MOVE    SPA-KT012-PTID      TO  SRY-PTID
           MOVE    JYURRK-SRYKA        TO  SRY-SRYKA
           MOVE    JYURRK-SRYYMD(1:6)  TO  SRY-SRYYM
           MOVE    JYURRK-ZAINUM(IDX-R)
                                       TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           MOVE    ZERO                TO  WRK-ZAINUM
           PERFORM
                   UNTIL   FLG-SRYACT      =   1
      *
               MOVE    ZERO                TO  FLG-OK
      *        初再診を対象外とする
               IF      SRY-SRYKBN          =   "11"
      ******                               OR  "12"
      *            労災の再診時療養指導管理料は対象
                   IF     (SRY-SRYCD (01)      =   "101120030" )  OR
                          (SRY-SRYCD (02)      =   "101120030" )  OR
                          (SRY-SRYCD (03)      =   "101120030" )  OR
                          (SRY-SRYCD (04)      =   "101120030" )  OR
                          (SRY-SRYCD (05)      =   "101120030" )
                       MOVE    1               TO  FLG-OK
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-OK
               END-IF
      *        選択診療区分判定
               EVALUATE    SPA-KT012-SRYKBN
                   WHEN    "10"
                       IF      SRY-SRYKBN(1:1)     =   "1"
                           CONTINUE
                       ELSE
                           MOVE    ZERO            TO  FLG-OK
                       END-IF
                   WHEN    "20"
                       IF     (SRY-SRYKBN(1:1)     =   "2" )  OR
                              (SRY-SRYKBN          =   "98")
                           CONTINUE
                       ELSE
                           MOVE    ZERO            TO  FLG-OK
                       END-IF
                   WHEN    "29"
                       IF     (SRY-SRYKBN(1:1)     =   "2" )  OR
                              (SRY-SRYKBN          =   "98")  OR
                              (SRY-SRYKBN(1:1)     =   "3" )
                           CONTINUE
                       ELSE
                           MOVE    ZERO            TO  FLG-OK
                       END-IF
                   WHEN    "30"
                       IF      SRY-SRYKBN(1:1)     =   "3"
                           CONTINUE
                       ELSE
                           MOVE    ZERO            TO  FLG-OK
                       END-IF
                   WHEN    "40"
                       IF      SRY-SRYKBN(1:1)     =   "4"
                           CONTINUE
                       ELSE
                           MOVE    ZERO            TO  FLG-OK
                       END-IF
                   WHEN    "50"
                       IF      SRY-SRYKBN(1:1)     =   "5"
                           CONTINUE
                       ELSE
                           MOVE    ZERO            TO  FLG-OK
                       END-IF
                   WHEN    "60"
                       IF      SRY-SRYKBN(1:1)     =   "6"
                           CONTINUE
                       ELSE
                           MOVE    ZERO            TO  FLG-OK
                       END-IF
                   WHEN    "70"
                       IF      SRY-SRYKBN(1:1)     =   "7"
                           CONTINUE
                       ELSE
                           MOVE    ZERO            TO  FLG-OK
                       END-IF
                   WHEN    "80"
                       IF      SRY-SRYKBN(1:1)     =   "8"
                           CONTINUE
                       ELSE
                           MOVE    ZERO            TO  FLG-OK
                       END-IF
                   WHEN    "90"
                       IF     (SRY-SRYKBN (1:1)    =   "9" )  AND
                              (SRY-SRYKBN      NOT =   "98")
                           CONTINUE
                       ELSE
                           MOVE    ZERO            TO  FLG-OK
                       END-IF
               END-EVALUATE
      *!!!!!!
      *        レセ電からは調剤料・処方料は対象外
               IF     (SRY-SRYKA (1:1)     =   "A"  )
                  IF     (SRY-SRYSYUKBN(1:2)   =   "24" )
                     OR  (SRY-SRYSYUKBN        =   "250")
                     OR  (SRY-SRYSYUKBN        =   "260")
                       MOVE    ZERO                TO  FLG-OK
                   END-IF
               END-IF
      *
      *
               IF      FLG-OK                  =   1
                   MOVE    ZERO                TO  WRK-DOKAISU
      *            診療会計検索編集
                   PERFORM 41012-SRYACT-HEN-SEC
      *
                   IF      IDY                 >   ZERO
                       ADD     1               TO  IDX-KOUI
                       MOVE    WKSRYACT-REC    TO  LNK-WKSRYACT-REC
                                                          (IDX-KOUI)
                   END-IF
               END-IF
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX-KOUI        >   ZERO
      *        剤毎に処理を行う
               ADD     1               TO  SPA-NAI-MAX2
               MOVE    JYURRK-ZAINUM(IDX-R)    TO  SPA-NAI-ZAINUM
                                                        (SPA-NAI-MAX2)
               PERFORM 3101-WKSRYACT-HEN-SEC
           END-IF
      *
           .
       490111-ZAISET-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスタを作成する 
      *****************************************************************
       41012-SRYACT-HEN-SEC           SECTION.
      *
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  WRK-ZAIKAISU
      *    剤回数取得
           INITIALIZE                      SRYACCT-REC
           MOVE    SRY-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SRY-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SRY-PTID            TO  ACCT-PTID
           MOVE    SRY-SRYKA           TO  ACCT-SRYKA
           MOVE    SRY-SRYYM           TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
      *    主キー読み
           PERFORM 940-SRYACCT-READ-SEC
           IF      FLG-SRYACCT         =   ZERO
      *    回数決定
      *        同日再診の最終回セット
      *ver.4.8
      *        EVALUATE    SPA-SEL-RENNUM
      *            WHEN    1
      *                MOVE    ACCT-DAY (2 IDX-STR)    TO  WRK-ZAIKAISU
      *            WHEN    2
      *                MOVE    ACCT-DAY (3 IDX-STR)    TO  WRK-ZAIKAISU
      *            WHEN    OTHER
      *                MOVE    ACCT-DAY (4 IDX-STR)    TO  WRK-ZAIKAISU
      ******   END-EVALUATE
               MOVE    ACCT-DAY (SPA-SEL-RENNUM + 1  IDX-STR)
                                                   TO  WRK-ZAIKAISU
           END-IF
      *
           INITIALIZE                  WKSRYACT-REC
      *
      *    再診で減点分以外は対象外
           IF     (SRY-SRYKBN          =   "12"  )  AND
                  (ACCT-ZAIKBN     NOT =   2     )
               MOVE    ZERO                TO  WRK-ZAIKAISU
           END-IF
      *
      *    回数ゼロは対象外とする
           IF      WRK-ZAIKAISU        =   ZERO
               GO      TO      41012-SRYACT-HEN-EXT
           END-IF
      *
      **** IF      WRK-ZAINUM      NOT =   SRY-ZAINUM
      *        MOVE    ZERO                TO  IDX-KAI
      **** END-IF
      *
           INITIALIZE                  WKSRYACT-REC
           MOVE    SRY-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SRY-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SRY-PTID            TO  WKSRY-PTID
           MOVE    SRY-SRYKA           TO  WKSRY-SRYKA
     ***** MOVE    SRY-SRYYM           TO  WKSRY-SRYYMD(1:6)
           MOVE    WRK-SRYYMD          TO  WKSRY-SRYYMD
           MOVE    SRY-SRYKBN          TO  WKSRY-SRYKBN
           MOVE    SRY-ZAINUM          TO  WKSRY-ZAINUM
           MOVE    SRY-RENNUM          TO  WKSRY-RENNUM
           MOVE    SRY-SRYSYUKBN       TO  WKSRY-SRYSYUKBN
           MOVE    SRY-JIHIMONEYTOTAL  TO  WKSRY-JIHIMONEYTOTAL
      *剤回数計
           MOVE    WRK-ZAIKAISU        TO  WKSRY-ZAIKAIKEI
      *
           IF     (SRY-SRYKBN          =   "21"  OR  "22"  OR  "23") AND
                  (WRK-DOKAISU     NOT =   ZERO                    )
               MOVE    WRK-DOKAISU     TO  WKSRY-ZAIKAIKEI
           END-IF
      *    剤点数計
           MOVE    ACCT-ZAITEN         TO  WKSRY-ZAITENKEI
      *保険組合
           MOVE    ACCT-HKNCOMBI       TO  WKSRY-HKNCOMBI
      *剤区分
           MOVE    ACCT-ZAIKBN         TO  WKSRY-ZAIKBN
      *
           MOVE    ZERO                TO  IDX-W
           MOVE    ZERO                TO  IDY
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   5   )   OR
                              (SRY-SRYCD (IDX)     =   SPACE )
      *        登録時の自動算定分は対象外（点滴以外）
      *H24.4
               MOVE    ZERO                TO  FLG-OK3
               IF    ((SRY-AUTOKBN (IDX)   =   "3" ) AND
                      (SRY-SRYSYUKBN   NOT =   "330" AND
                                               "340" ))
      *            H24.4から処方せん料を対象とする
                   IF     (SRY-SRYYM           >=  "201204"  ) AND
                          (SRY-SRYSYUKBN       =   "820"     )
                       MOVE    1               TO  FLG-OK3
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-OK3
               END-IF
               IF      FLG-OK3             =   1
                   ADD     1                   TO  IDY
                   MOVE    SRY-SRYCD (IDX)     TO  WKSRY-SRYCD    (IDY)
                   MOVE    SRY-SRYSURYO (IDX)  TO  WKSRY-SRYSURYO (IDY)
                   MOVE    SRY-SRYKAISU (IDX)  TO  WKSRY-SRYKAISU (IDY)
                   MOVE    SRY-AUTOKBN  (IDX)  TO  WKSRY-AUTOKBN  (IDY)
      *!!
      *            器材商品名対応
                   IF      SRY-MEISKYFLG (IDX) =   "1"
                       MOVE    "T"             TO  WKSRY-AUTOKBN  (IDY)
                   END-IF
      *ver.4.5
      *            換算値
                   MOVE    SRY-KANSURYO (IDX)  TO  WKSRY-KANSURYO (IDY)
      *            関連コメント指示
                   IF       SRY-MEISKYFLG (IDX) =   "2"
                       MOVE    "1"             TO  WKSRY-INPUTKBN (IDY)
                   END-IF
      *            内服種類数指示
                   IF       SRY-MEISKYFLG (IDX) =   "3"
                       MOVE    "2"             TO  WKSRY-INPUTKBN (IDY)
                   END-IF
      *
      *!!
      *            自動発生の中心静脈注射のみ選択可能とする
                   IF     (SRY-AUTOKBN (IDX)   =   "3" )  AND
                          (SRY-SRYSYUKBN       =   "340")
                       MOVE    SPACE           TO  WKSRY-AUTOKBN (IDY)
                   END-IF
      *            自動発生選択可能とする
                   IF     (FLG-WRITE           =   1   )  AND
                          (SRY-AUTOKBN (IDX)   =   "4"
                                               OR  "5"
                                               OR  "6"
                                               OR  "7"
      *H24.4
                                               OR  "3"  )
                       MOVE    SPACE           TO  WKSRY-AUTOKBN (IDY)
                   END-IF
                   MOVE    SRY-INPUTNUM (IDX)  TO  WKSRY-INPUTNUM (IDY)
                   MOVE    SRY-JIHIMONEY(IDX)  TO  WKSRY-JIHIMONEY(IDY)
                   IF      SRY-INPUTNUM (IDX)  NOT =   ZERO
                       PERFORM 490111-PTCOM-SET-SEC
                   END-IF
                   MOVE    IDY                 TO  IDX-W
               END-IF
               ADD     1                   TO  IDX-KAI
           END-PERFORM
      *
           IF     (ACCT-MEISAISU       =   IDX-KAI )  AND
                  (IDX-W               >   ZERO    )
               MOVE    WRK-ZAIKAISU        TO  WKSRY-ZAIKAISU (IDX-W)
           END-IF
      *
           .
       41012-SRYACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者コメントから編集する 
      *****************************************************************
       490111-PTCOM-SET-SEC              SECTION.
      *
           MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
           MOVE    SRY-PTID            TO  PTCOM-PTID
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
           MOVE    SRY-SRYCD (IDX)     TO  PTCOM-SRYCD
           MOVE    SRY-INPUTNUM(IDX)   TO  PTCOM-RENNUM
      *主キーで検索
           PERFORM 950-PTCOM-READ-SEC
           IF      FLG-PTCOM           =   ZERO
               MOVE    PTCOM-INPUTCOMENT   TO  WKSRY-INPUTCOMENT (IDY)
               MOVE    PTCOM-INPUTCHI (1)  TO  WKSRY-INPUTCHI (IDY 1)
               MOVE    PTCOM-INPUTCHI (2)  TO  WKSRY-INPUTCHI (IDY 2)
               MOVE    PTCOM-INPUTCHI (3)  TO  WKSRY-INPUTCHI (IDY 3)
               MOVE    PTCOM-INPUTCHI (4)  TO  WKSRY-INPUTCHI (IDY 4)
               MOVE    PTCOM-INPUTCHI (5)  TO  WKSRY-INPUTCHI (IDY 5)
           END-IF
           .
       490111-PTCOM-SET-EXT.
           EXIT.
      *
      *H28.5
      *****************************************************************
      *     診療行為内容編集処理
      *****************************************************************
       31018-GENERIC-NAME-SEC              SECTION.
      *
           INITIALIZE                  ORCSCGENNAMEAREA
           MOVE    "0"                 TO  ORCSCGENN-SYORIKBN
           MOVE    "0"                 TO  ORCSCGENN-SYORIKBN2
           MOVE    LNK-WKSRY-SRYYMD (1)    TO  ORCSCGENN-SRYYMD
      *    後発医薬品への変更可署名(SYS-1030-KOUHATUKA)
           MOVE    SPA-KOUHATUKA       TO  ORCSCGENN-1030-KOUHATUKA
      *    後発品変更不可（処方毎）有
           IF      WRK-FUKA-INPUT      =   "1"
               MOVE    "1"             TO  ORCSCGENN-KOUHATU-IN
           END-IF
      *    後発品変更可（処方毎）有
           IF      WRK-FUKA-INPUT      =   "2"
               MOVE    "2"             TO  ORCSCGENN-KOUHATU-IN
           END-IF
           MOVE    ZERO                TO  IDX-Z
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   IDX-KOUI )
                          OR  (IDX-Z   >=  50 )
      *        剤明細編集
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ         >   5   )  OR
                                  (LNK-WKSRY-SRYCD (IDX IDZ) =  SPACE)
                              OR  (IDX-Z       >=  50 )
                   ADD     1                   TO  IDX-Z
                   MOVE    LNK-WKSRY-SRYCD (IDX IDZ)
                                           TO  ORCSCGENN-SRYCD (IDX-Z)
                   MOVE    IDX-Z           TO  ORCSCGENN-TBLMAX
               END-PERFORM
           END-PERFORM
      *
           CALL    "ORCSCGENNAME"      USING    ORCSCGENNAMEAREA
                                                SPA-AREA
      *
           MOVE    ZERO                TO  IDX-Z
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   IDX-KOUI )
                          OR  (IDX-Z   >=  50 )
      *        剤明細編集
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ         >   5   )  OR
                                  (LNK-WKSRY-SRYCD (IDX IDZ) =  SPACE)
                              OR  (IDX-Z       >=  50 )
                   ADD     1                   TO  IDX-Z
                   IF     (ORCSCGENN-SRYCD (IDX-Z)
                                               =   LNK-WKSRY-SRYCD
                                                         (IDX IDZ))
                   AND    (ORCSCGENN-SRYCD (IDX-Z)(1:1)  =   "6"  )
                       IF     (ORCSCGENN-NAME-KBN (IDX-Z)  =   "1"
                                                           OR  "3")
                          AND (ORCSCGENN-HEN-NAME(IDX-Z) NOT =   SPACE)
      *                    一般名
                           MOVE    ORCSCGENN-HEN-NAME (IDX-Z)
                                           TO  LNK-WKSRY-INPUTCOMENT
                                                            (IDX IDZ)
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           .
       31018-GENERIC-NAME-EXT.
           EXIT.
      *!!
      *
      *****************************************************************
      *     診療行為内容編集処理
      *****************************************************************
       3101-WKSRYACT-HEN-SEC              SECTION.
      *
      *H28.5
      *    一般名対応
           IF     (LNK-WKSRY-SRYKBN(1)     =   "14"
                                           OR  "21"
                                           OR  "22"
                                           OR  "23" )
             AND  (LNK-WKSRY-ZAITENKEI(1)  =   ZERO )
             AND  (LNK-WKSRY-SRYYMD (1)    >=  "20160401" )
             AND  (SPA-KT012-GENERICFLG      =   "1"   )
               IF     (LNK-WKSRY-SRYSYUKBN (1) =   "213"
                                               OR  "223"
                                               OR  "233"
                                               OR  "231"
                                               OR  "294" )
                   CONTINUE
               ELSE
      *            院外処方のみ
                   PERFORM 31018-GENERIC-NAME-SEC
               END-IF
           END-IF
      *!!
      *
      *    診療コード計計算
           MOVE    ZERO                TO  IDZ
           MOVE    ZERO                TO  FLG-YAKUSOKU
           MOVE    ZERO                TO  WRK-MEISAISU
           MOVE    ZERO                TO  WRK-ZAITEN
           MOVE    ZERO                TO  WRK-SRYKAISUKEI
           MOVE    ZERO                TO  WRK-JIHIMONEY
           MOVE    ZERO                TO  WRK-HOUKNS-CNT
           MOVE    ZERO                TO  WRK-HOUKNS-IDX
           MOVE    SPACE               TO  WRK-HOUKNS-NAME
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-KOUI
      *
      *        診療種別
               MOVE    LNK-WKSRY-SRYKBN   (IDX)    TO  SPA-NAI-SRYKBN
                                                        (SPA-NAI-MAX2)
      *
      *        剤点数計
               MOVE    LNK-WKSRY-ZAITENKEI(IDX)    TO  WRK-ZAITEN
      *        回数計
               MOVE    LNK-WKSRY-ZAIKAIKEI(IDX)    TO  WRK-SRYKAISUKEI
      **       自費
               IF      LNK-WKSRY-JIHIMONEYTOTAL (IDX)   NOT =   ZERO
                   MOVE    LNK-WKSRY-JIHIMONEYTOTAL (IDX)
                                                   TO  WRK-JIHIMONEY
               END-IF
      *
      *        剤内１行目
               IF      IDX                 =   1
                   ADD     1               TO  IDG
                   PERFORM 31011-GYO1-HEN-SEC
               END-IF
      *
      *        剤明細編集
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ         >   5   )  OR
                                  (IDG         >   MAX-LINE )  OR
                                  (LNK-WKSRY-SRYCD (IDX IDZ) =  SPACE)
                   ADD     1                   TO  WRK-MEISAISU
      *            自費の時、金額を表示
                   IF      LNK-WKSRY-SRYKBN (IDX)  =   "95"  OR
                                                       "96"
                       PERFORM 31013-JIHIMEI-HEN-SEC
                   ELSE
                       PERFORM 31012-ZAIMEI-HEN-SEC
                   END-IF
      *            約束セットの時
                   IF      LNK-WKSRY-SRYCD (IDX IDZ)(1:1)   =   "S"
                       MOVE    1                  TO  FLG-YAKUSOKU
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    剤点数がゼロの時、自費を点数へ
      *    IF      WRK-ZAITEN          =   ZERO
      *        MOVE   WRK-JIHIMONEY        TO  WRK-ZAITEN
      *    END-IF
      *
      *    診療コードがない時、処理をしない
           IF      WRK-MEISAISU        =   ZERO
               COMPUTE IDZ             =   IDZ     -   1
               GO      TO      3101-WKSRYACT-HEN-EXT
           END-IF
      *!
      *    包括検査数
           IF      WRK-HOUKNS-CNT      >   1
               PERFORM 3009-INPUT-MEISYO-SEC
           END-IF
      *
      *    自費の時、計を表示しない
           IF     (WRK-JIHIMONEY       >   ZERO     )
            AND   (LNK-WKSRY-SRYKBN (01)   =   "95" 
                                           OR  "96" )
               CONTINUE
           ELSE
      *
      *        剤　点数＊日数　計　編集 （最終行に追加）
               IF      SPA-GMN-HEN1 (IDG)      =   SPACE  OR  ALL "　"
                   CONTINUE
               ELSE
                   ADD    1                    TO  IDG
               END-IF
               PERFORM 31013-INPUTCD-HEN-SEC
               MOVE    WRK-ZAIKEIJ         TO  SPA-GMN-TZAIKEI (IDG)
      *
               INSPECT SPA-GMN-TMEISYO2(IDG)   REPLACING
                                               ALL "  "  BY  "　"
      *        INSPECT SPA-GMN-TYOBI   (IDG)   REPLACING
      *                                        ALL "  "  BY  "　"
               INSPECT SPA-GMN-TZAIKEI (IDG)   REPLACING
                                               ALL "  "  BY  "　"
      *
               MOVE    SPA-NAI-MAX2        TO  SPA-NAI-TBANGO    (IDG)
           END-IF
      *
      *    剤終了編集
           ADD     1                   TO  IDG
           IF      IDG                 >   MAX-LINE
               CONTINUE
           ELSE
               MOVE    ALL "-"             TO  SPA-GMN-TMEISYO (IDG)
           END-IF
      *
            .
       3101-WKSRYACT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     包括検査数再編集処理
      *****************************************************************
       3009-INPUT-MEISYO-SEC        SECTION.
      *
           INITIALIZE                  ORCSMEIHENAREA
           MOVE    "2"                 TO  ORCSMEIHEN-KBN
           MOVE    WRK-HOUKNS-CNT      TO  ORCSMEIHEN-SURYO
           MOVE    WRK-HOUKNS-NAME     TO  ORCSMEIHEN-TNS-NAME
      *    医療機関番号
           IF      SPA-KT012-HOSPNUM NOT =   SPA-HOSPNUM
               MOVE    SPA-KT012-HOSPNUM     TO  ORCSMEIHEN-GRPHOSPNUM
           END-IF
           CALL    "ORCSMEIHEN"        USING
                                       SPA-AREA
                                       ORCSMEIHENAREA
           IF      WRK-HOUKNS-NAME(29:)    =   SPACE
               MOVE    ORCSMEIHEN-MEISYO   TO  SPA-GMN-TMEISYO2
                                                  (WRK-HOUKNS-IDX)
           ELSE
               IF      IDG             =   WRK-HOUKNS-IDX
                   ADD     1               TO  IDG
                   MOVE    IDG             TO  WRK-HOUKNS-IDX
               ELSE
                   PERFORM VARYING     IDG3    FROM    IDG   BY  -1
                           UNTIL       IDG3    <=  WRK-HOUKNS-IDX
                       MOVE    SPA-GMN-TBLREC-G (IDG3)
                                               TO  SPA-GMN-TBLREC-G
                                                           (IDG3 + 1)
                   END-PERFORM
                   COMPUTE WRK-HOUKNS-IDX  =   WRK-HOUKNS-IDX  +  1
                   INITIALIZE                  SPA-GMN-TBLREC-G
                                                        (WRK-HOUKNS-IDX)
                   MOVE    SPA-NAI-MAX2        TO  SPA-NAI-TBANGO
                                                        (WRK-HOUKNS-IDX)
                   ADD     1               TO  IDG
               END-IF
               IF      WRK-HOUKNS-IDX      <=   MAX-LINE
                   MOVE    ORCSMEIHEN-MEISYO2
                                       TO  SPA-GMN-TMEISYO2
                                                  (WRK-HOUKNS-IDX)
                                                  (29:)
                   INSPECT SPA-GMN-TMEISYO2(WRK-HOUKNS-IDX)   REPLACING
                                               ALL "  "  BY  "　"
                   INSPECT SPA-GMN-TZAIKEI (WRK-HOUKNS-IDX)   REPLACING
                                               ALL "  "  BY  "　"
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  WRK-HOUKNS-CNT
           MOVE    ZERO                TO  WRK-HOUKNS-IDX
           MOVE    SPACE               TO  WRK-HOUKNS-NAME
           .
       3009-INPUT-MEISYO-EXT.
           EXIT.
      *
      *****************************************************************
      *   一般名対応編集処理
      *****************************************************************
       30010-GENE-HEN-SEC                  SECTION.
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-GMN-MAX2
               IF      SPA-GMN-TGENENAME (IDX)  NOT =   SPACE
                   MOVE    SPA-GMN-TGENENAME (IDX)
                                     TO  SPA-GMN-TMEISYO2(IDX)
                   INSPECT SPA-GMN-TMEISYO2(IDX)
                                     REPLACING  ALL "  "  BY  "　"
               END-IF
           END-PERFORM
           .
       30010-GENE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤内１行目  編集処理
      *****************************************************************
       31011-GYO1-HEN-SEC                  SECTION.
      *
      *    診療種別区分名称、診療行為区分セット
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE       TO  WRK-SRYSYUMEI
                   WHEN    LNK-WKSRY-SRYSYUKBN (IDX) =   MSYU-SRYSYUKBN
                                                      (MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  WRK-SRYSYUMEI
           END-SEARCH
      *
      *    診療種別がない時、診療行為区分より
           IF      LNK-WKSRY-SRYSYUKBN (IDX)   =   SPACE
               SET     MSYU-IDX            TO  1
               SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYSYUMEI
                   WHEN    LNK-WKSRY-SRYKBN(IDX)
                                           =   MSYU-SRYKBN(MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                               TO  WRK-SRYSYUMEI
               END-SEARCH
           END-IF
      *
           IF      LNK-WKSRY-SRYSYUKBN (IDX)   =   SPACE
               MOVE    SPACE           TO  SPA-GMN-TMEISYO (IDG)
               MOVE    WRK-SRYSYUMEI   TO  SPA-GMN-TMEISYO (IDG)(7:)
           ELSE
               MOVE    "."             TO  SPA-GMN-TMEISYO (IDG)(1:1)
               MOVE    LNK-WKSRY-SRYSYUKBN (IDX)
                                       TO  SPA-GMN-TMEISYO (IDG)(2:3)
               MOVE    WRK-SRYSYUMEI   TO  SPA-GMN-TMEISYO (IDG)(7:)
           END-IF
      *    自動発生分
           IF      LNK-WKSRY-AUTOKBN(IDX 1)    =   "4"
                                               OR  "5"
                                               OR  "6"
                                               OR  "7"
      *H24.4
                                               OR  "3"
               MOVE    SPACE           TO  WRK-SRYSYUMEI-H
               MOVE    "◎"            TO  WRK-SRYSYUMEI-H
               MOVE    SPA-GMN-TMEISYO (IDG)
                                       TO  WRK-SRYSYUMEI-H(3:)
               MOVE    WRK-SRYSYUMEI-H TO  SPA-GMN-TMEISYO (IDG)
           END-IF
      *
           INSPECT SPA-GMN-TMEISYO  (IDG)   REPLACING
                                            ALL "  "  BY  "　"
           INSPECT SPA-GMN-TYOBI0   (IDG)   REPLACING
                                            ALL "  "  BY  "　"
      *
      *    番号
           MOVE    SPA-NAI-MAX2        TO  SPA-GMN-TBANGO    (IDG)
           MOVE    SPA-NAI-MAX2        TO  SPA-NAI-TBANGO    (IDG)
      *
           .
       31011-GYO1-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤内明細  編集処理
      *****************************************************************
       31012-ZAIMEI-HEN-SEC                  SECTION.
      *
           ADD     1                   TO  IDG
           IF      IDG                 >   MAX-LINE
               GO      TO      31012-ZAIMEI-HEN-EXT
           END-IF
           MOVE    SPA-NAI-MAX2        TO  SPA-NAI-TBANGO    (IDG)
      *
      *    約束セット編集
           IF      LNK-WKSRY-SRYCD (IDX IDZ)(1:1)   =   "S"
               PERFORM 310120-YAKUSOKU-SET-SEC
               GO      TO      31012-ZAIMEI-HEN-EXT
           END-IF
      *
      *H28.5
      *    後発品変更不可（処方単位）判定
           IF      LNK-WKSRY-SRYCD (IDX IDZ)   =   "099209910"
               MOVE    "1"                 TO  WRK-FUKA-INPUT
           END-IF
      *H29.3
      *    後発品変更可（処方単位）判定
           IF      LNK-WKSRY-SRYCD (IDX IDZ)   =   "099209911"
               MOVE    "2"                 TO  WRK-FUKA-INPUT
           END-IF
      *
      *    診療コードより診療行為名称を取得
           MOVE    LNK-WKSRY-SRYCD (IDX IDZ)   TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
               MOVE    LNK-WKSRY-SRYCD (IDX IDZ)   TO  TNS-NAME
           END-IF
      *    行為名称
           MOVE    TNS-NAME            TO  SPA-GMN-TMEISYO2(IDG)
      *H28.5
           IF     (LNK-WKSRY-SRYCD (IDX IDZ)(1:1)  =   "6" )
              AND (LNK-WKSRY-INPUTCOMENT(IDX IDZ)  NOT =   SPACE )
               MOVE    LNK-WKSRY-INPUTCOMENT(IDX IDZ)
                                       TO  SPA-GMN-TGENENAME(IDG)
           END-IF
      *!!!
      *    薬剤服用コメントの時、再編集
           IF      LNK-WKSRY-SRYCD (IDX IDZ)(1:3)  =   "001"
                PERFORM 3100121-FUKIYO-MEISYO-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *
      *H26.10
      *    在宅　診療人数合計　再編集
           IF      LNK-WKSRY-SRYCD (IDX IDZ)   =   "099140012"
                PERFORM 31001214-ZAICOM-MEISYO-SEC
           END-IF
      *!
      *    後発医薬品変更再編集
           IF      LNK-WKSRY-SRYCD (IDX IDZ)   =   "099209903"
                                               OR  "099209905"
                                               OR  "099209906"
                                               OR  "099209907"
                                               OR  "099209908"
                PERFORM 3100122-KOUHATU-MEISYO-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *    用量割合再編集
           IF     (LNK-WKSRY-SRYCD(IDX IDZ)(1:7)   =   "0992000")  AND
                  (LNK-WKSRY-SRYSURYO(IDX IDZ) NOT =   ZERO     )
                PERFORM 3005-INPUT-MEISYO3-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *!
      *    器材商品名の器材コード
           IF     (LNK-WKSRY-SRYCD(IDX IDZ)(1:1)   =   "7"  )  AND
                  (LNK-WKSRY-AUTOKBN (IDX IDZ)     =   "T"  )
                PERFORM 3005-INPUT-MEISYO4-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO2(IDG)
      *ver.4.7
      *        商品名コードの単位へ
               IF      WRK-SYO-TANINAME    NOT =   SPACE
                   MOVE    WRK-SYO-TANINAME    TO  TNS-TANINAME
                   MOVE    SPACE               TO  WRK-SYO-TANINAME
               END-IF
           END-IF
      *    器材商品名コード
           IF     (LNK-WKSRY-SRYCD(IDX IDZ)(1:3)   =   "058"  )
               MOVE    TNS-TANINAME        TO  WRK-SYO-TANINAME
           END-IF
      *ver.4.7
      *!
      *
      *    約束の内容の時、再編集
           IF      FLG-YAKUSOKU        =   1
               MOVE    SPA-GMN-TMEISYO2(IDG)   TO  WRK-MEISYO
               MOVE    SPACE               TO  SPA-GMN-TMEISYO2(IDG)
               STRING     "＞"             DELIMITED   BY  SIZE
                          WRK-MEISYO       DELIMITED   BY  SIZE
                          INTO             SPA-GMN-TMEISYO2(IDG)
               END-STRING
      *H28.5
               IF      SPA-GMN-TGENENAME (IDG) NOT =   SPACE
                   MOVE    SPA-GMN-TGENENAME(IDG)   TO  WRK-MEISYO
                   MOVE    SPACE           TO  SPA-GMN-TGENENAME(IDG)
                   STRING     "＞"         DELIMITED   BY  SIZE
                          WRK-MEISYO       DELIMITED   BY  SIZE
                          INTO             SPA-GMN-TGENENAME(IDG)
                   END-STRING
               END-IF
      *
           END-IF
      *
      *    コメントの時、内容設定
           IF    ((LNK-WKSRY-SRYCD   (IDX IDZ)(1:1)  =   "8"   ) OR
                  (LNK-WKSRY-SRYCD   (IDX IDZ)(1:3)  =   "008" ))  AND
      *DDDDDDDDDD (LNK-WKSRY-INPUTNUM(IDX IDZ)    NOT =   ZERO OR
                  (LNK-WKSRY-INPUTCOMENT(IDX IDZ) NOT =  SPACE )
               MOVE    LNK-WKSRY-INPUTCOMENT(IDX IDZ)
                                           TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *
      *H30.4
           IF     (LNK-WKSRY-SRYCD   (IDX IDZ)(1:7)  =   "0992081") AND
                  (LNK-WKSRY-INPUTCOMENT(IDX IDZ) NOT =  SPACE )
               MOVE    LNK-WKSRY-INPUTCOMENT(IDX IDZ)
                                           TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *
      *    数量（薬剤、器材のとき）
           IF     (LNK-WKSRY-SRYCD (IDX IDZ)(1:1)   =   "6"  OR  "7")
              OR  (LNK-WKSRY-SRYCD (IDX IDZ)(1:3)   =   "059"       )
      *        金額で点数ゼロの時、表示なし
               IF     (LNK-WKSRY-SRYCD (IDX IDZ)(1:1)   =   "6" )  AND
                     ((TNS-TEN                 =   ZERO   )  OR
                      (TNS-TENSIKIBETU         =   7      ))
                   CONTINUE
               ELSE
                   MOVE    LNK-WKSRY-SRYSURYO (IDX IDZ)  TO  WRK-SURYO
                   PERFORM 310121-SURYO-HEN-SEC
                   IF      FLG-KETA5           =   ZERO
                       MOVE    WRK-SURYO-J     TO  SPA-GMN-TSURYOU (IDG)
                       MOVE    TNS-TANINAME    TO  SPA-GMN-TTANI1  (IDG)
                   ELSE
                       MOVE    WRK-SURYO-J-G   TO  SPA-GMN-TSURYOU3(IDG)
                       MOVE    TNS-TANINAME    TO  SPA-GMN-TTANI3  (IDG)
                   END-IF
               END-IF
           END-IF
      *    単位
           IF     (LNK-WKSRY-SRYCD (IDX IDZ)(1:1) =   "1" )  AND
                  (TNS-TANICD                 NOT =   ZERO ) AND
                 ((LNK-WKSRY-SRYKBN   (IDX)   NOT =   "70" )  OR
                  (LNK-WKSRY-AUTOKBN (IDX IDZ)    =   "S"  ))
      ****     IF      LNK-WKSRY-SRYSURYO (IDX IDZ)   >   1
                   MOVE    LNK-WKSRY-SRYSURYO (IDX IDZ)  TO  WRK-SURYO
                   PERFORM 310121-SURYO-HEN-SEC
                   MOVE    WRK-SURYO-J         TO  SPA-GMN-TSURYOU (IDG)
      ****     END-IF
               MOVE    TNS-TANINAME        TO  SPA-GMN-TTANI1  (IDG)
           END-IF
      *    分画数
           IF     (LNK-WKSRY-SRYCD (IDX IDZ)(1:1)   =   "7"  )  AND
                  (TNS-DATAKBN             =   3    )  AND
                  (LNK-WKSRY-SRYKAISU (IDX IDZ)      >   ZERO )
               MOVE    LNK-WKSRY-SRYKAISU (IDX IDZ)
                                           TO  WRK-BUNGSU-Z
               PERFORM 310122-BUNGSU-HEN-SEC
               MOVE    WRK-BUNGSU-J        TO  SPA-GMN-TTANMEI (IDG)
               MOVE    "分画"              TO  SPA-GMN-TTANI2  (IDG)
           END-IF
      *!
      *    包括検査の判定
           IF     (LNK-WKSRY-SRYCD (IDX IDZ)(1:1) =   "1" )  AND
                  (LNK-WKSRY-SRYKBN (IDX)      =   "60" ) AND
                  (TNS-DATAKBN                 =   1    ) AND
                  (TNS-HOUKNSKBN           NOT =   ZERO )
               ADD     1                   TO  WRK-HOUKNS-CNT
               MOVE    IDG                 TO  WRK-HOUKNS-IDX
               MOVE    SPA-GMN-TMEISYO2(IDG)   TO  WRK-HOUKNS-NAME
           END-IF
      *!
      *!
      *    自賠責金額入力
           IF     (LNK-WKSRY-SRYSYUKBN (IDX)   =   "809")  AND
                  (LNK-WKSRY-JIHIMONEY(IDX IDZ)  >   ZERO )
               PERFORM 310124-RSI-JIHIMONEY-HEN-SEC
           END-IF
      *!
      *
           INSPECT SPA-GMN-TMEISYO2(IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TSURYOU (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TTANMEI (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TTANI1  (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TTANI2  (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TYOBI1  (IDG) REPLACING  ALL "  "  BY  "　"
      *ver.4.5
      *    換算値数量（薬剤）
           IF     (LNK-WKSRY-SRYCD (IDX IDZ)(1:1)  =   "6" )
              AND (LNK-WKSRY-KANSURYO(IDX IDZ) NOT =   ZERO )
               PERFORM 310123-KANSURYO-HEN-SEC
           END-IF
      *
      *    コメント以外の最終行セット
           IF      LNK-WKSRY-SRYCD (IDX IDZ)(1:1)   =   "0"  OR  "8"
               CONTINUE
           ELSE
               MOVE    IDG             TO  WRK-IDG
           END-IF
      *
           .
       31012-ZAIMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   自賠責金額入力編集処理
      *****************************************************************
       310124-RSI-JIHIMONEY-HEN-SEC                  SECTION.
      *
           MOVE    LNK-WKSRY-JIHIMONEY(IDX IDZ)    TO  WRK-KEI
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-KEI             TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  WRK-ZAIKEIJ
      *
           MOVE    SPACE               TO  WRK-TSURYO
           MOVE    1                   TO  IDM
           MOVE    1                   TO  IDM2
           PERFORM
                   UNTIL       IDM     >   22
               IF      WRK-ZAIKEIJ(IDM:1)  =   SPACE
                   ADD     1               TO  IDM
               ELSE
                   IF      WRK-ZAIKEIJ(IDM:2)  =   "　"
                       ADD     2               TO  IDM
                   ELSE
                       MOVE    WRK-ZAIKEIJ (IDM:2)
                                               TO  WRK-TSURYO
                                                        (IDM2:2)
                       ADD     2               TO  IDM2
                       ADD     2               TO  IDM
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  SPA-GMN-HEN1(IDG)
           STRING  "（"                DELIMITED   BY  SIZE
                   WRK-TSURYO          DELIMITED   BY  SPACE
                   "円）"              DELIMITED   BY  SIZE
                      INTO             SPA-GMN-HEN1 (IDG)
           END-STRING
           .
       310124-RSI-JIHIMONEY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤内明細（自費）  編集処理
      *****************************************************************
       31013-JIHIMEI-HEN-SEC           SECTION.
      *
           ADD     1                   TO  IDG
           IF      IDG                 >   MAX-LINE
               GO      TO      31013-JIHIMEI-HEN-EXT
           END-IF
           MOVE    SPA-NAI-MAX2        TO  SPA-NAI-TBANGO    (IDG)
      *
      *    約束セット編集
           IF      LNK-WKSRY-SRYCD (IDX IDZ)(1:1)   =   "S"
               PERFORM 310120-YAKUSOKU-SET-SEC
               GO      TO      31013-JIHIMEI-HEN-EXT
           END-IF
      *
      *    診療コードより診療行為名称を取得
           MOVE    LNK-WKSRY-SRYCD (IDX IDZ)   TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
               MOVE    LNK-WKSRY-SRYCD (IDX IDZ)   TO  TNS-NAME
           END-IF
      *    行為名称
           MOVE    TNS-NAME            TO  SPA-GMN-TMEISYO2(IDG)
      *
      *    約束の内容の時、再編集
           IF      FLG-YAKUSOKU        =   1
               MOVE    SPA-GMN-TMEISYO2(IDG)   TO  WRK-MEISYO
               MOVE    SPACE               TO  SPA-GMN-TMEISYO2(IDG)
               STRING     "＞"             DELIMITED   BY  SIZE
                          WRK-MEISYO       DELIMITED   BY  SIZE
                          INTO             SPA-GMN-TMEISYO2(IDG)
               END-STRING
           END-IF
      *
      *    コメントの時、内容設定
           IF    ((LNK-WKSRY-SRYCD   (IDX IDZ)(1:1)  =   "8"   ) OR
                  (LNK-WKSRY-SRYCD   (IDX IDZ)(1:3)  =   "008"))  AND
                  (LNK-WKSRY-INPUTCOMENT(IDX IDZ) NOT =  SPACE )
               MOVE    LNK-WKSRY-INPUTCOMENT(IDX IDZ)
                                           TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *
      *H30.4
           IF     (LNK-WKSRY-SRYCD   (IDX IDZ)(1:7)  =   "0992081") AND
                  (LNK-WKSRY-INPUTCOMENT(IDX IDZ) NOT =  SPACE )
               MOVE    LNK-WKSRY-INPUTCOMENT(IDX IDZ)
                                           TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *
      *    計
           MOVE    LNK-WKSRY-JIHIMONEY(IDX IDZ)
                                       TO  WRK-KEI
           MOVE    WRK-KEI                 TO  WRK-ZAIKEI
      *
      *    日本語変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-ZAIKEI          TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  WRK-ZAIKEIJ
           INSPECT WRK-ZAIKEIJ         REPLACING
                                           ALL "Ｘ"  BY  "×"
      *
           IF      LNK-WKSRY-JIHIMONEY(IDX IDZ)    =   ZERO
               MOVE    SPACE               TO  SPA-GMN-TZAIKEI (IDG)
           ELSE
               MOVE    "円"                TO  WRK-ZAIKEIJ(21:2)
               MOVE    WRK-ZAIKEIJ         TO  SPA-GMN-TZAIKEI (IDG)
           END-IF
      *
           INSPECT SPA-GMN-TMEISYO2(IDG) REPLACING  ALL "  "  BY  "　"
      **** INSPECT SPA-GMN-TYOBI   (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TZAIKEI (IDG) REPLACING  ALL "  "  BY  "　"
      *
      *    コメント以外の最終行セット
           IF      LNK-WKSRY-SRYCD (IDX IDZ)(1:1)   =   "0"  OR  "8"
               CONTINUE
           ELSE
               MOVE    IDG             TO  WRK-IDG
           END-IF
      *
           .
       31013-JIHIMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *     約束セットの時、再編集処理
      *****************************************************************
       310120-YAKUSOKU-SET-SEC        SECTION.
      *
           MOVE    SPACE               TO  INPUTCD-REC
           MOVE    SPA-KT012-HOSPNUM     TO  ICD-HOSPNUM
      *    セットの桁数は６とする
           MOVE    "6"                 TO  ICD-CDSYU
           MOVE    LNK-WKSRY-SRYCD (IDX IDZ)(1:6)
                                       TO  ICD-INPUTCD
      *
           PERFORM 930-INPUTCD-READ-SEC
      *
           IF      FLG-INPUTCD         =   ZERO
               MOVE    ICD-DSPNAME         TO  SPA-GMN-TMEISYO2(IDG)
           ELSE
               MOVE    LNK-WKSRY-SRYCD (IDX IDZ)
                                           TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *
      *    数量（薬剤、器材のとき）
           MOVE    LNK-WKSRY-SRYSURYO (IDX IDZ)  TO  WRK-SURYO
           PERFORM 310121-SURYO-HEN-SEC
           MOVE    WRK-SURYO-J         TO  SPA-GMN-TSURYOU (IDG)
      *
           .
       310120-YAKUSOKU-SET-EXT.
           EXIT.
      *****************************************************************
      *     薬剤服用コメントの時、再編集処理
      *****************************************************************
       3100121-FUKIYO-MEISYO-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO
      *
           STRING     "【"             DELIMITED   BY  SIZE
                      TNS-NAME         DELIMITED   BY  "    "
                      "】"             DELIMITED   BY  SIZE
                      INTO             WRK-MEISYO
           END-STRING
      *    埋め込み有
           IF     (LNK-WKSRY-INPUTCOMENT(IDX IDZ) NOT =  SPACE )
               MOVE    SPACE               TO  WRK-MEISYO
               STRING     "【"             DELIMITED   BY  SIZE
                          LNK-WKSRY-INPUTCOMENT(IDX IDZ)
                                           DELIMITED   BY  SPACE
                          "】"             DELIMITED   BY  SIZE
                          INTO             WRK-MEISYO
               END-STRING
           END-IF
           .
       3100121-FUKIYO-MEISYO-EXT.
           EXIT.
      *****************************************************************
      *     後発医薬品変更再編集処理
      *****************************************************************
       3100122-KOUHATU-MEISYO-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO
      *
           STRING     "【"             DELIMITED   BY  SIZE
                      TNS-NAME         DELIMITED   BY  "    "
                      "】"             DELIMITED   BY  SIZE
                      INTO             WRK-MEISYO
           END-STRING
           .
       3100122-KOUHATU-MEISYO-EXT.
           EXIT.
      *
      *****************************************************************
      *     用量割合再編集処理
      *****************************************************************
       3005-INPUT-MEISYO3-SEC        SECTION.
      *
           INITIALIZE                  ORCSMEIHENAREA
           MOVE    "1"                 TO  ORCSMEIHEN-KBN
           MOVE    LNK-WKSRY-SRYSURYO(IDX IDZ)
                                       TO  ORCSMEIHEN-SURYO
           MOVE    LNK-WKSRY-SRYCD(IDX IDZ)
                                       TO  ORCSMEIHEN-SRYCD
           MOVE    TNS-NAME            TO  ORCSMEIHEN-TNS-NAME
           CALL    "ORCSMEIHEN"        USING
                                       SPA-AREA
                                       ORCSMEIHENAREA
           MOVE    ORCSMEIHEN-MEISYO   TO  WRK-MEISYO
           .
       3005-INPUT-MEISYO3-EXT.
           EXIT.
      *****************************************************************
      *    器材商品名コードの器材編集
      *****************************************************************
       3005-INPUT-MEISYO4-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO
      *
           STRING     "〔"             DELIMITED   BY  SIZE
                      TNS-NAME         DELIMITED   BY  "    "
                      "〕"             DELIMITED   BY  SIZE
                      INTO             WRK-MEISYO
           END-STRING
           .
       3005-INPUT-MEISYO4-EXT.
           EXIT.
      *
      *H26.10
      *****************************************************************
      *    在宅　診療人数合計　再編集処理
      *****************************************************************
       31001214-ZAICOM-MEISYO-SEC            SECTION.
      *
      *    回数を合計人数
           MOVE    WRK-SRYKAISUKEI     TO  WRK-KAISU
           MOVE    ZERO                TO  IDK2
           MOVE    SPACE               TO  WRK-KAISU-H
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   8
               IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KAISU-X (IDK1:1)
                                           TO  WRK-KAISU-H (IDK2:1)
               END-IF
           END-PERFORM
      *
           MOVE    SPA-KT012NAI-RRKYMD(SPA-KT012-DOSELNUM)(7:2)
                                       TO  WRK-DAY
           MOVE    SPACE               TO  WRK-MEISYO
           STRING  TNS-NAME            DELIMITED  BY  SPACE
                   "　　　　【"        DELIMITED  BY  SIZE
                   WRK-DAY             DELIMITED  BY  SPACE
                   "日（"              DELIMITED  BY  SIZE
                   WRK-KAISU-H         DELIMITED  BY  SPACE
                   "人）"              DELIMITED  BY  SIZE
                   "】"                DELIMITED  BY  SIZE
                                               INTO    WRK-MEISYO
           END-STRING
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MEISYO          TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-TMEISYO2(IDG)
      *
           .
       31001214-ZAICOM-MEISYO-EXT.
           EXIT.
      *H26.10
      *
      *****************************************************************
      *    数量表示用編集処理
      *****************************************************************
       310121-SURYO-HEN-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
           MOVE    ZERO                TO  FLG-SP
           MOVE    ZERO                TO  FLG-KETA5
           PERFORM VARYING     IDM     FROM    5  BY  -1
                   UNTIL       IDM     <   1
               IF      WRK-SURYO-S2(IDM:1) NOT =   ZERO
                   MOVE    1               TO  FLG-SP
                   IF      IDM             >   3
                       MOVE    1               TO  FLG-KETA5
                   END-IF
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDM:1) TO  WRK-SURYO-Z3(IDM:1)
               END-IF
           END-PERFORM
      *
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
               MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1-9
           END-IF
      *
           IF      FLG-KETA5           =   1
               IF      WRK-SURYO-X(1:2)    =   SPACE
                   MOVE    WRK-SURYO-X(3:9)    TO  WRK-SURYO-X2
                   MOVE    SPACE               TO  WRK-SURYO-X
                   MOVE    WRK-SURYO-X2        TO  WRK-SURYO-X
                   MOVE    ZERO                TO  FLG-KETA5
                END-IF
           END-IF
      *
      *    日本語変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-SURYO-X         TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      FLG-KETA5           =   ZERO
               MOVE    KANACHK-OUT-INPUT   TO  WRK-SURYO-J
               INSPECT WRK-SURYO-J         REPLACING
                                           ALL "Ｘ"  BY  "×"
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  WRK-SURYO-J-G
               INSPECT WRK-SURYO-J-G       REPLACING
                                           ALL "Ｘ"  BY  "×"
           END-IF
           .
       310121-SURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量表示用編集処理
      *****************************************************************
       310122-BUNGSU-HEN-SEC           SECTION.
      *
      *    日本語変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-BUNGSU-X        TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  WRK-BUNGSU-J
           INSPECT WRK-BUNGSU-J        REPLACING
                                       ALL "Ｘ"  BY  "×"
           .
       310122-BUNSU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   換算値数量編集処理
      *****************************************************************
       310123-KANSURYO-HEN-SEC                  SECTION.
      *
           PERFORM 980-TENSUPLUS-READ-SEC
           IF     (FLG-TENSUPLUS       =   1    )
             OR   (IDG                 >=  MAX-LINE )
               GO      TO      310123-KANSURYO-HEN-EXT
           END-IF
      *
           ADD     1                   TO  IDG
           MOVE    SPA-NAI-MAX2        TO  SPA-NAI-TBANGO    (IDG)
      *
           MOVE    LNK-WKSRY-KANSURYO (IDX IDZ)  TO  WRK-SURYO
           PERFORM 310121-SURYO-HEN-SEC
           MOVE    1                   TO  IDM
           MOVE    1                   TO  IDM2
           MOVE    SPACE               TO  WRK-TSURYO
           PERFORM
                   UNTIL       IDM     >   22
               IF      WRK-SURYO-J-G(IDM:1)    =   SPACE
                   ADD     1               TO  IDM
               ELSE
                   IF      WRK-SURYO-J-G(IDM:2)    =   "　"
                       ADD     2               TO  IDM
                   ELSE
                       MOVE    WRK-SURYO-J-G(IDM:2)
                                               TO  WRK-TSURYO
                                                        (IDM2:2)
                       ADD     2               TO  IDM2
                       ADD     2               TO  IDM
                   END-IF
               END-IF
           END-PERFORM
      *    数量換算単位名称
           MOVE    TNSP-KANZANTANINAME TO  WRK-TSURYO  (IDM2:)
           MOVE    SPACE               TO  SPA-GMN-TSURYOU4 (IDG)
           STRING  "（"
                   WRK-TSURYO
                   "）"                DELIMITED  BY  SPACE
                                       INTO    SPA-GMN-TSURYOU4 (IDG)
           END-STRING
      *
           INSPECT SPA-GMN-TMEISYO2(IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TSURYOU (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TTANMEI (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TTANI1  (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TTANI2  (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TYOBI1  (IDG) REPLACING  ALL "  "  BY  "　"
           .
       310123-KANSURYO-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *   入力コードの画面用再編集処理
      *****************************************************************
       31013-INPUTCD-HEN-SEC                  SECTION.
      *
      *    点数＊回数　計
      *    点数
           MOVE    WRK-ZAITEN          TO  WRK-TENSU
      *
           IF      WRK-SRYKAISUKEI     =   ZERO
               MOVE   1                    TO  WRK-SRYKAISUKEI
           END-IF
           COMPUTE WRK-KEI-G           =   WRK-ZAITEN
                                       *   WRK-SRYKAISUKEI
           MOVE    WRK-KEI-G           TO  WRK-KEI
      *    回数
           MOVE    WRK-SRYKAISUKEI     TO  WRK-KAISU
           MOVE    ZERO                TO  IDK2
           MOVE    SPACE               TO  WRK-ZAIKEI 
           MOVE    SPACE               TO  WRK-KAISU-H 
           MOVE    SPACE               TO  WRK-KEI-H 
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   8
               IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KAISU-X (IDK1:1)
                                       TO  WRK-KAISU-H (IDK2:1)
               END-IF
           END-PERFORM
      *    計
           MOVE    ZERO                TO  IDK2
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   8
               IF      WRK-KEI-X   (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KEI-X   (IDK1:1)
                                       TO  WRK-KEI-H   (IDK2:1)
               END-IF
           END-PERFORM
      *
           STRING  WRK-TENSU               DELIMITED   BY  SIZE
                   "X"                     DELIMITED   BY  SIZE
                   WRK-KAISU-H             DELIMITED   BY  SPACE
      *DDDDDD      "  "                    DELIMITED   BY  SIZE
      *DDDDDD      WRK-KEI-H               DELIMITED   BY  SPACE
                   INTO                    WRK-ZAIKEI
           END-STRING
      *
      *    日本語変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-ZAIKEI          TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  WRK-ZAIKEIJ
           INSPECT WRK-ZAIKEIJ         REPLACING
                                       ALL "Ｘ"  BY  "×"
      *
           .
       31013-INPUTCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象診療行為内容編集処理
      *****************************************************************
       4201-WKSRYACT-SET-SEC             SECTION.
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *    OPEN    OUTPUT              PARA-FILE
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "KT01"              TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *
           MOVE    ZERO                TO  WRK-EDANUM
      *
           PERFORM VARYING     IDX-R   FROM    1   BY  1
                   UNTIL      (IDX-R           >   MAX-SEL  )  OR
                              (WRK-DOZAINUM (IDX-R)    =   ZERO  )
               PERFORM 42011-PARA-HEN-SEC
           END-PERFORM
      *
           MOVE    1                   TO  SPA-KT012-SELOK
      *
      *****CLOSE                       PARA-FILE
      *
           .
       4201-WKSRYACT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為作成処理
      *****************************************************************
       42011-PARA-HEN-SEC           SECTION.
      *
           MOVE    1                   TO  FLG-WRITE
      *
      *     ワーク診療行為のレイアウトへ編集後、画面用編集へ
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
           MOVE    ZERO                TO  IDX-KOUI
           MOVE    ZERO                TO  IDX-KAI
      *    対象日
           MOVE    SPA-SEL-IDX         TO  IDX-STR
      *
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-KT012-HOSPNUM     TO  SRY-HOSPNUM
           MOVE    SPA-KT012-NYUGAIKBN   TO  SRY-NYUGAIKBN
           MOVE    SPA-KT012-PTID        TO  SRY-PTID
           MOVE    SPA-KT012NAI-RRK-SRYKA(SPA-KT012-DOSELNUM)
                                       TO  SRY-SRYKA
           MOVE    SPA-KT012NAI-RRKYMD(SPA-KT012-DOSELNUM)(1:6)
                                       TO  SRY-SRYYM
           MOVE    WRK-DOZAINUM (IDX-R)
                                       TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           MOVE    ZERO                TO  WRK-ZAINUM
           PERFORM
                   UNTIL   FLG-SRYACT      =   1
      *
      ***      初再診を対象外とする
      *        IF      SRY-SRYKBN          =   "11"  OR  "12"
      *            CONTINUE
      ****     ELSE
      *        ”＊”の時、自動発生分は対象外
               IF     (FLG-DOALL           =   1  )  AND
                      (SRY-AUTOKBN (1)     =   "4"
                                           OR  "5"
                                           OR  "6"
      *H24.4
                                           OR  "3"
                                           OR  "7" )
                   CONTINUE
               ELSE
      *        すべてを対象とする
                   MOVE    WRK-DOKAI (IDX-R)   TO  WRK-DOKAISU
      *            診療会計検索編集
                   PERFORM 41012-SRYACT-HEN-SEC
      *
      *            パラメタファイル作成
      *****        MOVE    SPACE               TO  PARA-REC
      *            MOVE    "WKSRYACT"          TO  PARA-FILEMEI
      *            ADD     1                   TO  WRK-EDANUM
      *            MOVE    WRK-EDANUM          TO  PARA-EDANUM
      *            MOVE    WKSRYACT-REC        TO  PARA-DATA-REC
      *
      *****        WRITE                       PARA-REC
      *
                   INITIALIZE                      PARA-REC
                   MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
                   MOVE    "KT01"              TO  PARA-GYOUMUID
                   MOVE    SPA-TERMID          TO  PARA-TERMID
                   MOVE    "WKSRYACT"          TO  PARA-FILEMEI
                   ADD     1                   TO  WRK-EDANUM
                   MOVE    WRK-EDANUM          TO  PARA-EDANUM
                   MOVE    WKSRYACT-REC        TO  PARA-DATA-REC
      *
                   PERFORM 8002-PARA-DBINSERT-SEC
               END-IF
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       42011-PARA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴処理へ
      *****************************************************************
       450-RIREKI-SEC            SECTION.
      *
      *    初期画面セット
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-KT012-NYUGAIKBN TO  SPA-KT013-NYUGAIKBN
           MOVE    SPA-KT01            TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGKT013"         USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-KT01
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           MOVE    "KT013"             TO  SPA-SAKIPG
           MOVE    "KT012"             TO  SPA-MOTOPG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KT013"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       450-RIREKI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
      *    中間ファイル削除
      *    MOVE    ZERO                TO  IF-RESULT
      *    MOVE    FILE-NAME1          TO  IF-FILENAME
      *    CALL    "orcfiledel"        USING
      *                                ORCSFDELAREA
      *
           MOVE    1                   TO  FLG-END2
      *
           MOVE    ZERO                TO  SPA-NAI-SELNUM
           MOVE    ZERO                TO  SPA-NAI-DOSELNUM
      **** MOVE    SPACE               TO  SPA-GMN-DOSELNUM
      *
           MOVE    "KT01"              TO  SPA-SAKIPG
           MOVE    "KT012"             TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       220-CLEAR-SEC             SECTION.
      *
           IF      SPA-GMN-DOSEL       =   SPACE
               MOVE    1                   TO  SPA-KT012GMN-CUR
           ELSE
               MOVE    SPACE               TO  SPA-GMN-DOSEL
               INITIALIZE                  SPA-NAI-DOSEL-G
               MOVE    2                   TO  SPA-KT012GMN-CUR
           END-IF
           .
       220-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    入外区分変更処理
      *****************************************************************
       460-NYUGAIKBN-HEN-SEC             SECTION.
      *
           IF      SPA-KT012-NYUGAIKBN-MAX   >   1
               EVALUATE    SPA-KT012-NYUGAIKBN
                   WHEN    "1"
                       MOVE    SPA-KT012-NYUGAIKBN-LST (2)
                                               TO  SPA-KT012-NYUGAIKBN-G
                   WHEN    "2"
                       MOVE    SPA-KT012-NYUGAIKBN-LST (1)
                                               TO  SPA-KT012-NYUGAIKBN-G
               END-EVALUATE
      *        受診履歴編集
      *        入外区分変更次は、１頁から表示
               MOVE    1                   TO  SPA-KT012-RRK-PAGE
               MOVE    ZERO                TO  SPA-KT012-RRK-NEXT
               PERFORM 3101-JYURRK-DBHEN-SEC
      *
               IF      SPA-KT012GMN-MAX1   >   ZERO
                   MOVE    1                   TO  SPA-KT012-DOSELNUM
                   MOVE    SPA-KT012GMN-NO(1)  TO  SPA-KT012-SELNUM
      *            診察内容編集
                   PERFORM 4101-JYURRK-HEN-SEC
               ELSE
                   PERFORM 41012-JYURRK-NASI-SEC
                   MOVE    2                   TO  SPA-KT012GMN-CUR
               END-IF
           END-IF
           .
       460-NYUGAIKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療区分変更処理
      *****************************************************************
       470-SRYKBN-HEN-SEC             SECTION.
      *
           PERFORM 4701-SRYKBN-HENKOU-SEC
      *
           IF      SPA-KT012-DOSELNUM    >   ZERO
               PERFORM 4101-JYURRK-HEN-SEC
      *
               PERFORM UNTIL  (SPA-KT012-SRYKBN  =   "00" )
                       OR     (SPA-GMN-MAX2    >   1    )
                   PERFORM 4701-SRYKBN-HENKOU-SEC
                   PERFORM 4101-JYURRK-HEN-SEC
               END-PERFORM
           ELSE
               MOVE    1                   TO  SPA-KT012GMN-CUR
           END-IF
           .
       470-SRYKBN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療区分変更処理
      *****************************************************************
       4701-SRYKBN-HENKOU-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KT012-SRYKBN-MAX
               IF      SPA-KT012-SRYKBN  =   SPA-KT012-SRYKBN-LST
                                                           (IDX)(1:2)
                   IF      IDX             =   SPA-KT012-SRYKBN-MAX
                       MOVE    SPA-KT012-SRYKBN-LST (1)
                                           TO  SPA-KT012-SRYKBN-G
                   ELSE
                       MOVE    SPA-KT012-SRYKBN-LST (IDX + 1)
                                           TO  SPA-KT012-SRYKBN-G
                   END-IF
                   MOVE    SPA-KT012-SRYKBN-MAX  TO  IDX
               END-IF
           END-PERFORM
           .
       4701-SRYKBN-HENKOU-EXT.
           EXIT.
      *****************************************************************
      *    医療機関変更処理
      *****************************************************************
       430-GRPHOSPNUM-HEN-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KT012-GRPMAX
               IF      SPA-KT012-GRPHOSPNUM  =   SPACE
                   MOVE    SPA-KT012-GRPHOSPNUM-LST (IDX)
                                           TO  SPA-KT012-GRPHOSPNUM-G
                   MOVE    SPA-NAI-GRPPTID (IDX)
                                               TO  SPA-KT012-PTID
                   MOVE    SPA-KT012-GRPMAX      TO  IDX
               ELSE
                   IF      SPA-KT012-GRPHOSPNUM
                                           =   SPA-KT012-GRPHOSPNUM-LST
                                                           (IDX)(1:2)
                       IF      IDX             =   SPA-KT012-GRPMAX
                           MOVE    SPACE   TO  SPA-KT012-GRPHOSPNUM-G
                       ELSE
                           MOVE    SPA-KT012-GRPHOSPNUM-LST (IDX + 1)
                                           TO  SPA-KT012-GRPHOSPNUM-G
                           MOVE    SPA-NAI-GRPPTID (IDX)
                                               TO  SPA-KT012-PTID
                       END-IF
                       MOVE    SPA-KT012-GRPMAX  TO  IDX
                   END-IF
               END-IF
           END-PERFORM
           IF       SPA-KT012-GRPHOSPNUM =   SPACE
               MOVE    SPA-HOSPNUM         TO  SPA-KT012-HOSPNUM
               MOVE    SPA-PTID            TO  SPA-KT012-PTID
           ELSE
               MOVE    SPA-KT012-GRPHOSPNUM  TO  SPA-KT012-HOSPNUM
           END-IF
      *    １頁から表示
           MOVE    1                   TO  SPA-KT012-RRK-PAGE
           MOVE    ZERO                TO  SPA-KT012-RRK-NEXT
           PERFORM 3101-JYURRK-DBHEN-SEC
      *
           IF      SPA-KT012GMN-MAX1     >   ZERO
               MOVE    1                   TO  SPA-KT012-DOSELNUM
               MOVE    SPA-KT012GMN-NO(1)    TO  SPA-KT012-SELNUM
      *        診察内容編集
               PERFORM 4101-JYURRK-HEN-SEC
           ELSE
               PERFORM 41012-JYURRK-NASI-SEC
               MOVE    12                  TO  SPA-KT012GMN-CUR
           END-IF
           .
       430-GRPHOSPNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    確定　処理
      *****************************************************************
       450-TOROKU-SEC             SECTION.
      *
           PERFORM 420-DOSEL-CHK-SEC
      *
           IF      IDX-H               =   ZERO
      *        戻る
               PERFORM 210-BACK
           END-IF
      *
      *    MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
      *    MOVE    "KT012"               TO  SPA-MOTOPG
      *
      *    MOVE    SPACE               TO  LINKAREA
      *
      *    MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
      *    MOVE    "CHANGE"            TO  MCP-PUTTYPE
      *    MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
      *    PERFORM 900-PUT-WINDOW
      *
      *    MOVE    1                   TO  FLG-END
           .
       450-TOROKU-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴 前頁　処理
      *****************************************************************
       4109-BC06-SYORI-SEC             SECTION.
      *
           IF      SPA-KT012-RRK-PAGE      =   1
               CONTINUE
           ELSE
               COMPUTE SPA-KT012-RRK-PAGE  =   SPA-KT012-RRK-PAGE  -  1
      *
               PERFORM 3101-JYURRK-DBHEN-SEC
      *
               INITIALIZE                  SPA-KT012GMN-TBL
               INITIALIZE                  SPA-KT012NAI-TBL
               MOVE    ZERO                TO  SPA-GMN-MAX2
               MOVE    ZERO                TO  SPA-NAI-MAX2
               IF      SPA-KT012GMN-MAX1   >   ZERO
                   MOVE    1                   TO  SPA-KT012-DOSELNUM
                   MOVE    SPA-KT012GMN-NO(1)  TO  SPA-KT012-SELNUM
      *            診察内容編集
                   PERFORM 4101-JYURRK-HEN-SEC
               ELSE
                   MOVE    10              TO  SPA-KT012GMN-CUR
               END-IF
           END-IF
           .
       4109-BC06-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴 次頁　処理
      *****************************************************************
       4109-BC07-SYORI-SEC             SECTION.
      *
           IF      SPA-KT012-RRK-NEXT        =   ZERO
               MOVE    "0008"              TO  SPA-ERRCD
           ELSE
               COMPUTE SPA-KT012-RRK-PAGE  =   SPA-KT012-RRK-PAGE  +  1
      *
               PERFORM 3101-JYURRK-DBHEN-SEC
      *
               INITIALIZE                  SPA-KT012GMN-TBL
               INITIALIZE                  SPA-KT012NAI-TBL
               MOVE    ZERO                TO  SPA-GMN-MAX2
               MOVE    ZERO                TO  SPA-NAI-MAX2
               IF      SPA-KT012GMN-MAX1   >   ZERO
                   MOVE    1                   TO  SPA-KT012-DOSELNUM
                   MOVE    SPA-KT012GMN-NO(1)  TO  SPA-KT012-SELNUM
      *            診察内容編集
                   PERFORM 4101-JYURRK-HEN-SEC
               ELSE
                   MOVE    10              TO  SPA-KT012GMN-CUR
               END-IF
           END-IF
      *
           .
       4109-BC07-SYORI-EXT.
           EXIT.
      *
      *H28.5
      *****************************************************************
      *    処方名変換処理
      *****************************************************************
       440-GENERIC-HEN-SEC             SECTION.
      *
           IF      SPA-KT012-GENERICFLG    =   "1"
               MOVE    SPACE           TO  SPA-KT012-GENERICFLG
           ELSE
               MOVE    "1"             TO  SPA-KT012-GENERICFLG
           END-IF
      *    診察内容編集
           PERFORM 4101-JYURRK-HEN-SEC
      *
           .
       440-GENERIC-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       520-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       520-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "KT012"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           IF      FLG-GMN-INIT1       =   1
               MOVE    ZERO            TO  WRK-KT012-RRKLIST-ROW
               MOVE    ZERO            TO  WRK-KT012-MEILIST-ROW
           ELSE
               MOVE    KT012-RRKLIST-ROW   TO  WRK-KT012-RRKLIST-ROW
               IF      FLG-GMN-INIT2       =   1
                   MOVE    ZERO            TO  WRK-KT012-MEILIST-ROW
               ELSE
                   MOVE    KT012-MEILIST-ROW TO  WRK-KT012-MEILIST-ROW
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  KT012
           INITIALIZE                      KT012
      *
      *    コラムリストの位置指定
           MOVE    ZERO                TO  KT012-RRKLIST-ROW
           MOVE    ZERO                TO  KT012-RRKLIST-ROWATTR
           MOVE    ZERO                TO  KT012-MEILIST-ROW
           MOVE    ZERO                TO  KT012-MEILIST-ROWATTR
           IF      WRK-KT012-RRKLIST-ROW NOT =   ZERO
               MOVE    WRK-KT012-RRKLIST-ROW TO  KT012-RRKLIST-ROW
           END-IF
           IF      WRK-KT012-MEILIST-ROW NOT =   ZERO
               MOVE    WRK-KT012-MEILIST-ROW TO  KT012-MEILIST-ROW
           END-IF
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-KT012-NYUGAIKBN-MAX
               MOVE    SPA-KT012-NYUGAIKBN-LST (IDX)
                                           TO  KT012-NYUGAIKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-KT012-NYUGAIKBN-MAX TO  KT012-NYUGAIKBN-COUNT
           MOVE    SPA-KT012-NYUGAIKBN-G   TO  KT012-NYUGAIKBN
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-KT012-SRYKBN-MAX
               MOVE    SPA-KT012-SRYKBN-LST (IDX)
                                           TO  KT012-SRYKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-KT012-SRYKBN-MAX  TO  KT012-SRYKBN-COUNT
           MOVE    SPA-KT012-SRYKBN-G    TO  KT012-SRYKBN
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-KT012-GRPMAX
               MOVE    SPA-KT012-GRPHOSPNUM-LST (IDX)
                                         TO  KT012-GRPHOSPNUM-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-KT012-GRPMAX      TO  KT012-GRPHOSPNUM-COUNT
           MOVE    SPA-KT012-GRPHOSPNUM-G    TO  KT012-GRPHOSPNUM
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-KT012GMN-MAX1
               MOVE    SPA-KT012GMN-NO  (IDX)  TO  WRK-ZZZ
               MOVE    WRK-ZZZ                 TO  KT012-NO  (IDX)
               MOVE    SPA-KT012GMN-RRKYMD (IDX) TO  KT012-RRKYMD(IDX)
               MOVE    SPA-KT012GMN-RRKSRYKA (IDX)
                                               TO  KT012-RRKSRYKA(IDX)
               MOVE    SPA-KT012GMN-RRKHKNCOMBI(IDX)
                                               TO  KT012-RRKHKNCOMBI
                                                               (IDX)
               IF      SPA-KT012GMN-NO (IDX)   =   SPA-KT012-SELNUM
                   MOVE    "T"                 TO  KT012-RRKLIST-SELECT
                                                               (IDX)
      *DDD         初期表示で選択がある場合、選択行を中央へ
      *DDD         IF      FLG-GMN-INIT1   =   1
                       MOVE    IDX             TO  KT012-RRKLIST-ROW
                       MOVE    2               TO  KT012-RRKLIST-ROWATTR
      *DDDDD       END-IF
               ELSE
                   MOVE    "F"             TO  KT012-RRKLIST-SELECT(IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-KT012GMN-MAX1     TO  KT012-RRKLIST-COUNT
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-GMN-MAX2
               MOVE    "F"             TO  KT012-MEILIST-SELECT  (IDX)
               IF      SPA-GMN-TMEISYO(IDX)(1:1)   =   "-"
                   MOVE    ALL "-"             TO  KT012-MNO (IDX)
      **********   MOVE    ALL "−"            TO  KT012-MEISAI (IDX)
                   MOVE    ALL "―"            TO  KT012-MEISAI (IDX)
               ELSE
                   IF      SPA-GMN-TBANGO (IDX)    NOT =   ZERO
                       MOVE    SPA-GMN-TBANGO (IDX)    TO  WRK-ZZ
                       MOVE    WRK-ZZ              TO  KT012-MNO (IDX)
                       PERFORM VARYING   IDX2  FROM    1   BY  1
                               UNTIL     IDX2  >   SPA-NAI-DOSEL-MAX
                           IF      SPA-NAI-DOSEL (IDX2) 
                                               =   SPA-GMN-TBANGO (IDX)
                               MOVE    "T"     TO  KT012-MEILIST-SELECT
                                                                 (IDX)
                               MOVE    SPA-NAI-DOSEL-MAX  TO  IDX2
                           END-IF
                       END-PERFORM
                   END-IF
                   MOVE    SPA-GMN-TMEISYO (IDX)   TO  KT012-MEISAI(IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-MAX2        TO  KT012-MEILIST-COUNT
      *
           MOVE    SPA-KT012-SELNUM    TO  KT012-SELNUM
      *
           MOVE    SPA-GMN-DOSEL       TO  KT012-DOSEL
      *    患者紐付け
           IF      SPA-KT012-GRPMAX      >   ZERO
               MOVE    WIDGET-NORMAL       TO  KT012-B03-STATE
               MOVE    "F3 他院情報"       TO  KT012-B03-LABEL
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  KT012-B03-STATE
               MOVE    SPACE               TO  KT012-B03-LABEL
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-KT012GMN-CUR
               WHEN    01
                   MOVE    "SELNUM"          TO  MCP-WIDGET
               WHEN    02
                   MOVE    "DOSEL"           TO  MCP-WIDGET
               WHEN    10
                   MOVE    "NYUGAIKBN"       TO  MCP-WIDGET
               WHEN    11
                   MOVE    "SRYKBN"          TO  MCP-WIDGET
               WHEN    12
                   MOVE    "GRPHOSPNUM"      TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "診療日選択入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "番号選択入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "＊は１文字だけです"
                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "回数変更は投薬のみ可能です。"
                                       TO  SPA-ERRMSG
                   MOVE    "投薬以外に回数変更があります。"
                                       TO  SPA-ERRMSG(29:)
               WHEN    "0005"
                   MOVE    "入外区分がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "診療区分がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "医療機関番号がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "選択は１００行までです。"
                                       TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "次ページはありません。"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KTERR
           MOVE    SPA-ERRCD           TO  KTERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  KTERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "KT012"             TO  SPA-MOTOPG
           MOVE    "KTERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KTERR"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    パラメタ一時ファイル書き込み処理
      *****************************************************************
       8002-PARA-DBINSERT-SEC          SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    SPA-TERMID          TO  PARA-TERMID
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
               DISPLAY "KT012 PARA INSERT ERR:"  MCP-RC
                       ",KEY:" PARA-KEY
           END-IF
      *
           .
       8002-PARA-DBINSERT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-KT012NAI-RRKYMD(SPA-KT012-DOSELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-KT012NAI-RRKYMD(SPA-KT012-DOSELNUM)
                                       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-KT012-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *!
      *    検査正式名称
           IF      SPA-FORMALFLG       =   "1"
               IF     (TNS-SRYCD(1:1)      =   "1") AND
                      (TNS-SRYKBN          =   "60"
                                           OR  "64"  )  AND
                      (TNS-FORMALNAME  NOT =   SPACE )  AND
                      (TNS-FORMALNAME  NOT =   TNS-NAME )
                   MOVE    TNS-FORMALNAME  TO  TNS-NAME
               END-IF
           END-IF
      *
      *R02.2
      *    ユーザー単位編集
           IF     (TNS-SRYCD(1:1)      =   "6"
                                       OR  "7" )
               PERFORM 980-TENSUPLUS-READ-SEC
      *        ユーザー単位設定
               IF     (FLG-TENSUPLUS       =   ZERO  )
                  AND (TNSP-USERTANICD NOT =   SPACE )
                    MOVE    TNSP-USERTANICD    TO  TNS-TANICD
                    MOVE    TNSP-USERTANINAME  TO  TNS-TANINAME
               END-IF
           END-IF
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       930-SRYACT-READ-SEC         SECTION.
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       930-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター読込（主キー）
      *****************************************************************
       940-SRYACCT-READ-SEC           SECTION.
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SRYACCT-REC
                   MOVE    ZERO                TO  FLG-SRYACCT
               ELSE
                   MOVE    1                   TO  FLG-SRYACCT
                   INITIALIZE                  SRYACCT-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       940-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       950-PTCOM-READ-SEC           SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   MOVE    1                   TO  FLG-PTCOM
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                  PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    パラメタデータ順読み
      *****************************************************************
      *980-PARA-READ-SEC         SECTION.
      *
      *    READ    PARA-FILE 
      *        AT  END
      *            MOVE    1           TO  FLG-PARA
      *        NOT AT  END
      *            MOVE    ZERO        TO  FLG-PARA
      *    END-READ
      *
      *    .
      *980-PARA-READ-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    パラメタデータ順読み
      *****************************************************************
      *990-PARA-NEXT-SEC           SECTION.
      *
      *    READ    PARA-FILE       NEXT
      *        AT  END
      *            MOVE    1           TO  FLG-PARA
      *        NOT AT  END
      *            MOVE    ZERO        TO  FLG-PARA
      *    END-READ
      *
      *    .
      *990-PARA-NEXT-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       930-INPUTCD-READ-SEC         SECTION.
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  INPUTCD-REC
                   MOVE    ZERO                TO  FLG-INPUTCD
               ELSE
                   INITIALIZE                  INPUTCD-REC
                   MOVE    1                   TO  FLG-INPUTCD
               END-IF
           ELSE
               INITIALIZE                  INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       930-INPUTCD-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報より編集処理
      *****************************************************************
       980-TENSUPLUS-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    SPA-KT012NAI-RRKYMD(SPA-KT012-DOSELNUM)
                                       TO  TNSP-YUKOSTYMD
           MOVE    SPA-KT012NAI-RRKYMD(SPA-KT012-DOSELNUM)
                                       TO  TNSP-YUKOEDYMD
           MOVE    SPA-KT012-HOSPNUM   TO  TNSP-HOSPNUM
      *
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
                   MOVE    ZERO                TO  FLG-TENSUPLUS
               ELSE
                   INITIALIZE                  TENSUPLUS-REC
                   MOVE    1                   TO  FLG-TENSUPLUS
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSUPLUS
               INITIALIZE                      TENSUPLUS-REC
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       980-TENSUPLUS-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＣＬＯＳＥ処理
      *****************************************************************
       990-DBCLOSE-SEC                  SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
