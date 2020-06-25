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
       PROGRAM-ID.                 ORCVTRECEDEN.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＤＢ作成処理
      *  コンポーネント名  : ＣＳＶファイルコンバート処理
      *                     （レセ電データから診療行為・診療会計・
      *                                       受診履歴　）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  13/02/06    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.00    NACL-多々納  13/07/10  算定日記録なし対応
      *  04.07.00    NACL-多々納  13/10/XX  部位・用法コード変換対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメータファイル
           SELECT  PARA-FILE       ASSIGN
                                   ASS-PARA
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PARA.
      *
      *    レセ電データ
           SELECT  CSV-FILE        ASSIGN
                                   ASS-CSV
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-CSV.
      *
      *    出力エラーファイル
           SELECT  ERR-FILE        ASSIGN
                                   ASS-ERR
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-LST.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *    パラメータファイル
       FD  PARA-FILE.
       01  PARA-REC.
           03  PARA-RECKBN             PIC X(01).
           03  PARA-DATKBN             PIC X(04).
           03  FILLER                  PIC X(01).
           03  PARA-DATA               PIC X(100).
      *
       01  PARA2-REC.
           03  PARA2-RECKBN             PIC X(01).
           03  PARA2-DATKBN             PIC X(03).
           03  PARA2-DATKBNREN          PIC X(03).
           03  FILLER                   PIC X(01).
           03  PARA2-DATA               PIC X(98).
      *
      *    レセ電データＣＳＶファイル
       FD  CSV-FILE.
       01  CSV-REC.
           03  CSV-R                   PIC X(2404).
      *
      *    出力エラーファイル
       FD  ERR-FILE.
       01  ERR-REC                     PIC X(100).
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    ファイル指定領域
       01  ASS-AREA.
           03  ASS-PARA                PIC X(256).
           03  ASS-CSV                 PIC X(256).
           03  ASS-ERR                 PIC X(256).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA                PIC X(02).
           03  STS-CSV                 PIC X(02).
           03  STS-LST                 PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PARA                PIC 9(01).
           03  FLG-END                 PIC 9(01).
           03  FLG-CSV                 PIC 9(01).
           03  FLG-CSV-READ             PIC 9(01).
      *
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-PTCOM               PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-BYOMEI              PIC 9(01).
           03  FLG-RECECOM             PIC 9(01).
      *
           03  FLG-PTNUM-ERR           PIC 9(01).
           03  FLG-ZAIEND              PIC 9(01).
           03  FLG-RE-END              PIC 9(01).
           03  FLG-YMD                 PIC 9(01).
           03  FLG-COMMENT             PIC 9(01).
           03  FLG-PLUSKBN             PIC 9(01).
           03  FLG-SJ-KBN              PIC 9(01).
      *
           03  FLG-ERR                 PIC 9(01).
           03  FLG-ERR2                PIC 9(01).
           03  FLG-HENKAN              PIC 9(01).
           03  FLG-HENKANERR           PIC 9(01).
           03  FLG-PARACHK             PIC 9(01).
           03  FLG-BUIARI              PIC 9(01).
           03  FLG-BUIOK               PIC 9(01).
           03  FLG-SYUGI               PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PARA                PIC 9(06).
           03  CNT-CSV                 PIC 9(06).
           03  CNT-ERR                 PIC 9(06).
           03  CNT-COMMIT              PIC 9(06).
           03  CNT-RE                  PIC 9(06).
           03  CNT-OUTRE               PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDE                     PIC 9(04).
           03  IDS                     PIC 9(04).
           03  IDD                     PIC 9(02).
           03  IDC                     PIC 9(04).
           03  IDX-SIN                 PIC 9(04).
           03  IDX-SST                 PIC 9(04).
      *
           03  IDX-J                   PIC 9(04).
           03  IDX-J2                  PIC 9(04).
           03  IDX-D                   PIC 9(04).
      *
           03  IDH                     PIC 9(04).
           03  IDP                     PIC 9(04).
      *
      *    パラメータ保存
       01  PARA-SAVE.
           03  PARA-05-1               PIC X(02).
           03  PARA-09-C               PIC X(06).
           03  PARA-11-C               PIC X(06).
           03  PARA-HOSPNUM            PIC X(02).
           03  PARA-12-G.
               05  PARA-12-1           PIC X(09)
                                       OCCURS   200.
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYMD2.
               05  WRK-SYY2            PIC 9(02).
               05  WRK-SMM2            PIC 9(02).
               05  WRK-SDD2            PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).
               05  WRK-SYSDD           PIC 9(02).
      *
           03  WRK-MOVE-AREA.
               05  WRK-MO-CNT          PIC 9(04).
               05  WRK-MO-ST           PIC 9(04).
      *    03  WRK-DATA                PIC X(100).
      *    03  WRK-LEN                 PIC 9(03).
      *    03  WRK-CNT                 PIC 9(04). 
      *
      *    パラメタ内容
           03  WRK-RENKETA             PIC 9(02).
           03  WRK-COMMIT              PIC 9(06).
           03  WRK-ERR                 PIC 9(06).
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC X(06).
               05  WRK-SRYDD           PIC X(02).
      *
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-R             REDEFINES   WRK-SRYKA.
               05  WRK-SRYKA-1         PIC X(01).
               05  WRK-SRYKA-EDA       PIC 9(01).
           03  WRK-ZAINUM              PIC 9(08).
           03  WRK-HZN-ZAINUM          PIC 9(08).
      *
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-SRYSYUKBN           PIC X(03).
      *    診療コード計
           03  WRK-ACCT-SRYCDTOTAL     PIC  9(14).
      *    数量計
           03  WRK-ACCT-SURYOUTOTAL    PIC  9(07)V9(05).
      *    明細数
           03  WRK-ACCT-MEISAISU       PIC  9(07).
      *    枝番
           03  WRK-SRY-RENNUM          PIC  9(02).
      *    コメント連番
           03  WRK-MEIBAN              PIC  9(03).
      *
           03  WRK-JYURRK-EDANUM       PIC  9(01).
      *
           03  WRK-SRYCD               PIC X(09).
           03  WRK-SRYCD-9             REDEFINES   WRK-SRYCD
                                       PIC 9(09).
      *
           03  WRK-HEN-TEXT            PIC X(20).
           03  WRK-HEN-NUM             PIC 9(10)V9(05).
      *    コメント変換
           03  WRK-MEISYO              PIC X(100).
           03  WRK-INPUTCHI-G.
               05  WRK-INPUTCHI        PIC  X(08)
                                       OCCURS   5.
      *    文字データを半角へ
           03  WRK-INPUT-MOJI          PIC X(100).
           03  WRK-INPUT-G.
               05  WRK-INPUT           PIC X(1)
                                       OCCURS   50.
           03  WRK-BUI-INPUTCD-G.
               05  WRK-BUICD           PIC X(4)
                                       OCCURS   12.
           03  WRK-MEISYO-H            PIC X(100).
           03  WRK-BUIMEI              PIC X(100).
      *
           03  WRK-MOJI                PIC X(02).
           03  IDK                     PIC 9(02).
           03  IDK1                    PIC 9(01).
           03  IDX-H1                  PIC 9(04).
           03  IDX-H2                  PIC 9(04).
           03  WRK-KETA                PIC 9(03).
           03  WRK-KETA2               PIC 9(03).
           03  WRK-ICHI                PIC 9(03).
           03  WRK-ICHI2               PIC 9(03).
      *
           03  WRK-CO-SRYCD            PIC X(09).
      *
      *    コメントの入力値用
       01  WRK-INPUT-SET-AREA.
           03  WRK-INPUT-TBL.
               05  WRK-INPUT-SET       PIC 9(03)   OCCURS  8.
           03  WRK-INPUT-TBL-R         REDEFINES   WRK-INPUT-TBL.
               05  WRK-INPUT-TBL2          OCCURS  4.
                   07  WRK-INPUT-ICHI      PIC 9(03).
                   07  WRK-INPUT-KETA      PIC 9(03).
      *
       01  WRK-HEN-AREA.
           03  WRK-HEN-PTNUM           PIC X(20).
           03  WRK-HEN-SRYYM           PIC X(06).
           03  WRK-HEN-NAME            PIC X(40).
      *
       01  WRK-RECHEN-AREA.
           03  WRK-HEN-SRYKBN          PIC X(02).
           03  WRK-HEN-SRYSYUKBN       PIC X(03).
           03  WRK-HEN-SRYCD           PIC X(09).
           03  WRK-HEN-SRYSURYO        PIC 9(07)V9(05).
           03  WRK-HEN-TENSU           PIC 9(07).
           03  WRK-HEN-ZAIKAIKEI       PIC 9(03).
           03  WRK-HEN-SHOHINMEI       PIC X(300).
           03  WRK-HEN-COMMENT-G       OCCURS   3.
               05  WRK-HEN-SRYCD-C         PIC X(09).
               05  WRK-HEN-MEISYO          PIC X(100).
           03  WRK-HEN-DAY-G.
               05  WRK-HEN-DAY             PIC 9(03)  OCCURS   31.
      *
       01  TBL-HENKAN-AREA.
           03  TBL-HENKAN-OCC          OCCURS   500.
               05  TBL-ZAINUM          PIC 9(10).
               05  TBL-SRYKBN          PIC X(02).
               05  TBL-SRYSYUKBN       PIC X(03).
               05  TBL-KAISU           PIC 9(05).
               05  TBL-ZAITEN          PIC 9(07).
               05  TBL-SYUTEN          PIC 9(07).
               05  TBL-YKZTEN          PIC 9(07).
               05  TBL-KIZAITEN        PIC 9(07).
               05  TBL-PLUSKBN         PIC 9(01).
               05  TBL-DAY-G.
                   07  TBL-DAY         PIC 9(03)    OCCURS   31.
               05  TBL-HEN-MEISAI-OCC      OCCURS   50.
                   07  TBL-SRYCD           PIC X(09).
                   07  TBL-SRYSURYO        PIC 9(07)V9(5).
                   07  TBL-MEISYO          PIC X(100).
                   07  TBL-MEISKYFLG       PIC X(01).
                   07  TBL-SURFLG          PIC X(01).
      *
      *    受診履歴剤編集
       01  TBL-JYURRKZAI-AREA.
           03  TBL-JYURRKZAI-REC       OCCURS   31.
               05  TBL-JYUZAI-SRYKBN-G.
                   07  TBL-JYUZAI-SRYKBN   PIC X(02)
                                           OCCURS   11.
               05  TBL-JYUZAI-ZAINUM-G.
                   07  TBL-JYUZAI-ZAINUM   PIC 9(08)
                                           OCCURS   135.
               05  TBL-JYUZAI-MAX          PIC 9(04).
      *
      *    症状詳記用
       01  WRK-RECHEN-SJKBN-AREA.
           03  WRK-HEN-SJKBN           PIC X(02).
           03  WRK-HEN-COMMENT         PIC X(2400).
      *
           03  WRK-RENNUM-MAX          PIC 9(02).
      *
      *    レセ電テーブル分解
       01  HZN-IN-TBL.
           03  HZN-IN-KOUMOKU          PIC X(300)  OCCURS  50.
      *    レセ電テーブル分解
       01  HZN-SJ-TBL.
           03  HZN-SJ-KOUMOKU          PIC X(2400) OCCURS  3.
      *
      *    帳票出力領域
       01  MES-AREA.
           03  MES-TITLE1.
               05  FILLER              PIC X(26) VALUE
                                       "【PGID:ORCVTRECEDEN.CBL】".
               05  FILLER              PIC X(26) VALUE
                                       "  レセ電データＤＯ移行".
               05  MES-TITLE1-YY       PIC X(04) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE ".".
               05  MES-TITLE1-MM       PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE ".".
               05  MES-TITLE1-DD       PIC X(02) VALUE SPACE.
           03  MES-TITLE2.
               05  MES-TITLE2-L1.
                   07  FILLER          PIC X(07) VALUE "LINE".
                   07  FILLER          PIC X(21) VALUE
                                                 "患者番号".
                   07  FILLER          PIC X(21) VALUE
                                                 "患者氏名".
                   07  FILLER          PIC X(101) VALUE
                                                 "診療年月".
                   07  FILLER          PIC X(40) VALUE
                                                 "エラーメッセージ".
               05  MES-TITLE2-L3.
                   07  FILLER          PIC X(50) VALUE
                   "--------------------------------------------------".
                   07  FILLER          PIC X(49) VALUE
                   "-------------------------------------------------".
      *
       01  ERRLST-AREA.
           03  ERR-LST-REC.
               05  ERR-LINENO          PIC X(06).
               05  FILLER              PIC X(01).
               05  ERR-PTNUM           PIC X(20).
               05  FILLER              PIC X(01).
               05  ERR-NAME            PIC X(20).
               05  FILLER              PIC X(01).
               05  ERR-SRYYM           PIC X(09).
               05  FILLER              PIC X(01).
               05  ERR-ERRMSG          PIC X(40).
      *
      *   日本語数字
       01  TBL-SUJI-AREA.
           03  FILLER          PIC X(20)   VALUE
              "１２３４５６７８９０".
       01  TBL-SUJI-R          REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI        PIC X(02)   OCCURS   10.
      *
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
      *    システム管理（医療機関情報）
           COPY    "CPSK1001.INC".
      *    システム管理（患者番号構成管理）
           COPY    "CPSK1009.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    職員情報
           COPY  "CPSK1010.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    診療科履歴
      *01  SRYKARRK-REC.
      *    COPY    "CPSRYKARRK.INC".
      *    算定履歴
      *01  SANTEI-REC.
      ***  COPY    "CPSANTEI.INC".
      *
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *    点数マスタ薬剤付加コード
        01 TENSUPLUS2-REC.
           COPY    "CPTENSUPLUS2.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    受診履歴
       01  TBL-JYURRK-AREA.
           02  TBL-JYURRK-REC        OCCURS   60.
           COPY    "CPJYURRK.INC"    REPLACING
                                     //JYURRK-// BY //HZN-JYURRK-//.
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *    診療会計附加情報マスタ−
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *    病名マスタ−
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    レセプトコメント
       01  RECECOM-REC.
           COPY    "CPRECECOM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   数値変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *    LIKE使用KEY対応
           COPY    "CPORCSADDSIGN.INC".
      *
      *!!!
      *    コメントコード検索
           COPY    "CPORCSSRCCOMMCD.INC".
      *
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      ****************************************************************
       PROCEDURE               DIVISION
               USING
           COMMAND-PARAM.
      ****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-CSV     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           MOVE    ZERO            TO  FLG-AREA
           MOVE    ZERO            TO  CNT-AREA
           MOVE    ZERO            TO  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  PARA-SAVE
      *
      *    パラメタセット
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    ASS-PARA
           END-UNSTRING
      *
      *    パラメータ情報取得
           PERFORM 101-PARA-GET-SEC
           IF  FLG-PARA            =   2
               MOVE    1           TO  FLG-CSV
               GO  TO  100-INIT-EXT
           END-IF
      *    医療機関番号
           IF      PARA-HOSPNUM    NUMERIC
               MOVE    PARA-HOSPNUM    TO  SPA-HOSPNUM
           ELSE
               MOVE    1               TO  FLG-CSV
               DISPLAY "*(ORCVTRECEDEN)* HOSPNUM ERR [" PARA-HOSPNUM"]"
               GO  TO  100-INIT-EXT
           END-IF
      *
           ACCEPT  WRK-SYMD2       FROM    DATE
           MOVE    WRK-SYMD2       TO  WRK-SYSYMD (3:)
           ADD     2000            TO  WRK-SYSYY
           MOVE    WRK-SYSYMD      TO  SPA-SYSYMD
           MOVE    SPA-SYSYMD(1:4) TO  MES-TITLE1-YY
           MOVE    SPA-SYSYMD(5:2) TO  MES-TITLE1-MM
           MOVE    SPA-SYSYMD(7:2) TO  MES-TITLE1-DD
      *    端末ＩＤ固定
           MOVE    "ORCATERM"      TO  SPA-TERMID
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    データベースオープン
           PERFORM 100-DBOPEN-SEC
      *
      *    システム管理情報　検索処理
           PERFORM 120-CP1001-KENSAKU-SEC
           IF  FLG-CSV             =   1
               GO  TO  100-INIT-EXT
           END-IF
      *    端末ＩＤ固定
           MOVE    "ORCATERM"      TO  SPA-TERMID
      *
      *   PERFORM 120-CP1009-KENSAKU-SEC
      *    IF  FLG-CSV             =   1
      *        GO  TO  100-INIT-EXT
      *    END-IF
      *
      *    ＣＳＶ入力ファイル　初期処理
           OPEN    INPUT           CSV-FILE
           IF  STS-CSV         NOT =   ZERO
               MOVE    1           TO  FLG-CSV
               DISPLAY "*(ORCVTRECEDEN)* RESEDEN-FILE OPN STS["
                                       STS-CSV "]"
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    帳票出力　初期処理
           PERFORM 2710-ERRLST-INIT-SEC
           IF  FLG-CSV             =   1
               GO  TO  100-INIT-EXT
           END-IF
      *
           MOVE    PARA-05-1       TO  WRK-RENKETA
           MOVE    PARA-09-C       TO  WRK-COMMIT
           MOVE    PARA-11-C       TO  WRK-ERR
      *
      *    ＣＳＶファイル　検索処理
           PERFORM 900-CSV-KENSAKU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメータ情報取得
      *****************************************************************
       101-PARA-GET-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-PARA
           OPEN    INPUT           PARA-FILE
           IF      STS-PARA        NOT =   ZERO
               MOVE    2               TO  FLG-PARA
               DISPLAY "*(ORCVTRECEDEN)* PARA-FILE OPN STS["
                       STS-PARA "]"
               GO  TO  101-PARA-GET-EXT
           END-IF
      *
           PERFORM 900-PARA-READ-SEC
      *
           PERFORM UNTIL    FLG-PARA  =  1
               IF  PARA-RECKBN         =   "@"
                   PERFORM 101-PARA-GET1-SEC
               END-IF
               PERFORM  900-PARA-READ-SEC
           END-PERFORM
      *
           CLOSE   PARA-FILE
      *
      *    パラメータチェック処理
           IF    ( PARA-05-1           IS  NUMERIC )   AND
                 ( PARA-09-C           IS  NUMERIC )   AND
                 ( PARA-11-C           IS  NUMERIC )
               CONTINUE
           ELSE
               MOVE    1               TO  FLG-CSV
               GO  TO  101-PARA-GET-EXT
           END-IF
      *
           .
       101-PARA-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメータ情報取得（１）
      *****************************************************************
       101-PARA-GET1-SEC           SECTION.
      *
           EVALUATE                PARA-DATKBN
               WHEN  "01-C"
                   MOVE  PARA-DATA     TO  ASS-CSV
               WHEN  "02-C"
                   MOVE  PARA-DATA     TO  ASS-ERR
               WHEN  "05-1"
                   MOVE  PARA-DATA     TO  PARA-05-1
               WHEN  "09-C"
                   MOVE  PARA-DATA     TO  PARA-09-C
               WHEN  "11-C"
                   MOVE  PARA-DATA     TO  PARA-11-C
               WHEN    "HN"
                   MOVE    PARA-DATA   TO  PARA-HOSPNUM
               WHEN    OTHER
                   IF     (PARA2-DATKBN    =   "12-" )
                    AND   (PARA2-DATA  NOT =   SPACE
                                           AND ZERO  )
                       ADD     1               TO  IDP
                       IF      IDP             <=  200
                           MOVE    PARA2-DATA   TO  PARA-12-1 (IDP)
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       101-PARA-GET1-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理情報　検索処理
      *****************************************************************
       120-CP1001-KENSAKU-SEC      SECTION.
      *
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
           IF      SPA-ERRCD           =   "1001"
      *        職員コードなしはそのまま（SPA-OPID)
               MOVE    SPACE           TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    1               TO  FLG-CSV
           END-IF
      *    MOVE    ZERO            TO  FLG-SYSKANRI
      *    MOVE    SPACE           TO  SYS-1001-REC
      *    MOVE    "1001"          TO  SYS-1001-KANRICD
      *    MOVE    WRK-SYSYMD      TO  SYS-1001-STYUKYMD
      *                                SYS-1001-EDYUKYMD
      *    MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
      *    MOVE    SYS-1001-REC    TO  MCPDATA-REC
      *    MOVE    "tbl_syskanri"  TO  MCP-TABLE
      *    MOVE    "key2"          TO  MCP-PATHNAME
      *    PERFORM 910-DBSELECT-SEC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "tbl_syskanri"  TO  MCP-TABLE
      *        MOVE    "key2"          TO  MCP-PATHNAME
      *        PERFORM 900-SYSTEM-NEXT-SEC
      *    ELSE
      *        MOVE    1               TO  FLG-SYSKANRI
      *    END-IF
      *    IF  FLG-SYSKANRI            =   1
      *        MOVE    1               TO  FLG-CSV
      *    ELSE
      *        MOVE    MCPDATA-REC         TO  SYS-1001-REC
      *        MOVE    SYS-1001-HOSPID     TO  SPA-HOSPID
      *        MOVE    SYS-1001-HOSPSBT    TO  SPA-HOSPSBT
      *        MOVE    SYS-1001-ROUPAYKBN  TO  SPA-ROUPAYKBN
      *    END-IF
      *    MOVE    "tbl_syskanri"  TO  MCP-TABLE
      *    MOVE    "key2"          TO  MCP-PATHNAME
      *    PERFORM 990-DBCLOSE-SEC
      *
           .
       120-CP1001-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理情報　検索処理
      *****************************************************************
       120-CP1009-KENSAKU-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
           MOVE    SPACE           TO  SYS-1009-REC
           MOVE    "1009"          TO  SYS-1009-KANRICD
           MOVE    "*"             TO  SYS-1009-KBNCD
           MOVE    WRK-SYSYMD      TO  SYS-1009-STYUKYMD
                                       SYS-1009-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF  FLG-SYSKANRI            =   1
               MOVE    1               TO  FLG-CSV
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-1009-REC
           END-IF
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       120-CP1009-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF      CNT-CSV (4:3)       =   ZERO
               DISPLAY "***(ORCVTRECEDEN)*** CNT-CSV[" CNT-CSV "]"
           END-IF
      *
           MOVE    ZERO            TO  FLG-PTNUM-ERR
           MOVE    ZERO            TO  IDX-AREA
           MOVE    SPACE           TO  SPA-PTNUM
           MOVE    ZERO            TO  SPA-PTID
           MOVE    SPACE           TO  WRK-HEN-AREA
           INITIALIZE                  WRK-HEN-AREA
           INITIALIZE                  WRK-RECHEN-AREA
           INITIALIZE                  WRK-RECHEN-SJKBN-AREA
      *
           PERFORM
                   UNTIL  (FLG-CSV         =   1   )
               MOVE    ZERO                TO  FLG-CSV-READ
               EVALUATE    HZN-IN-KOUMOKU (1)(1:2)
      *            レコード識別判定
                   WHEN    "IR"
                       CONTINUE
                   WHEN    "HO"
      *                保険レコード
                   WHEN    "KO"
      *                公費レコード
                   WHEN    "SY"
      *                傷病名レコード
                       CONTINUE
                   WHEN    "GO"
      *                終了
                       IF      IDX             >   ZERO
                           PERFORM 2009-TOUROKU-SYORI-SEC
                       END-IF
                   WHEN    "RE"
      *                レセプト共通レコード
                       IF      IDX             >   ZERO
                           PERFORM 2009-TOUROKU-SYORI-SEC
                       END-IF
                       PERFORM 2001-PTNUM-SYORI-SEC
                   WHEN    "SJ"
      *                症状詳記レコード
                       IF      FLG-PTNUM-ERR   =   ZERO
                           MOVE    1               TO  FLG-CSV-READ
                           PERFORM 2001-SJ-SYORI-SEC
                       END-IF
                   WHEN    OTHER
      *                診療行為レコードなど
                       IF      FLG-PTNUM-ERR   =   ZERO
                           MOVE    1               TO  FLG-CSV-READ
                           PERFORM 2001-MEISAI-SYORI-SEC
                       END-IF
               END-EVALUATE
      *
      *        COMMIT処理など
               PERFORM 200-COUNT-SYORI-SEC
      *
      *        レセ電データ　読込処理
               IF      FLG-CSV-READ            =   ZERO
                   PERFORM 900-CSV-KENSAKU-SEC
               END-IF
           END-PERFORM
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理件数処理
      *****************************************************************
       200-COUNT-SYORI-SEC        SECTION.
      *
      *    指定した件数になったらコミット処理を行う
           IF      CNT-COMMIT          =   WRK-COMMIT
               PERFORM 900-DBCOMMIT-SEC
               MOVE    ZERO            TO  CNT-COMMIT
           END-IF
      *    エラーが指定した件数になったら処理を抜ける
           IF    ( WRK-ERR         NOT =   ZERO )  AND
                 ( CNT-ERR             =   WRK-ERR )
               MOVE    1               TO  FLG-CSV
               MOVE    1               TO  FLG-CSV-READ
           END-IF
           .
       200-COUNT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト共通レコード編集チェック処理
      *****************************************************************
       2001-PTNUM-SYORI-SEC        SECTION.
      *
           ADD     1                   TO  CNT-RE
      *
           MOVE    ZERO                TO  FLG-PTNUM-ERR
           MOVE    SPACE               TO  SPA-PTNUM
           MOVE    ZERO                TO  SPA-PTID
           MOVE    "2"                 TO  SPA-NYUGAIKBN
           MOVE    SPACE               TO  WRK-HEN-AREA
           INITIALIZE                  WRK-HEN-AREA
      *
      *    患者氏名
           MOVE    HZN-IN-KOUMOKU (5)
                                       TO  WRK-HEN-NAME
      *    患者番号変換
           PERFORM 20011-PTNUM-SET-SEC
      *    診療年月
           PERFORM 20012-SRYYM-SET-SEC
      *    入院年月日の設定　入院分
           IF      HZN-IN-KOUMOKU (9)  NOT =   SPACE
               MOVE    "1"                 TO  SPA-NYUGAIKBN
           END-IF
      *
           IF     (SPA-PTID        NOT =   ZERO )
             AND  (WRK-SRYYM       NOT =   SPACE )
      *        受診履歴登録済みチェック
               PERFORM 20013-JYURRK-CHK-SEC
           END-IF
      *
           IF      FLG-PTNUM-ERR   NOT =   ZERO
      *        エラーリスト出力処理
               PERFORM 270-ERRREC-EDIT-SEC
               PERFORM 270-ERRLST-OUT-SEC
           END-IF
           .
       2001-PTNUM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目１４：患者番号　処理
      *****************************************************************
       20011-PTNUM-SET-SEC          SECTION.
      *
           MOVE    SPACE               TO  SPA-PTNUM
      *    前空白の削除
           MOVE    SPACE               TO  WRK-HEN-PTNUM
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL       IDH     >   20
               IF      HZN-IN-KOUMOKU (14) (IDH:1)
                                       NOT =   SPACE
                   MOVE    HZN-IN-KOUMOKU (14) (IDH:)
                                       TO  WRK-HEN-PTNUM
                   MOVE    20          TO  IDH
               END-IF
           END-PERFORM
      *
           IF      WRK-HEN-PTNUM       =   SPACE
               MOVE    1               TO  FLG-PTNUM-ERR
               GO  TO  20011-PTNUM-SET-EXT
           END-IF
      *    患者番号取得
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    WRK-HEN-PTNUM       TO  SPTID-PTNUM
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
               INITIALIZE                      PTINF-REC
               MOVE    1               TO  FLG-PTNUM-ERR
               GO  TO  20011-PTNUM-SET-EXT
           END-IF
      *
           MOVE    SPTID-PTNUM         TO  WRK-HEN-PTNUM
           MOVE    SPTID-PTNUM         TO  SPA-PTNUM
           MOVE    SPTID-PTID          TO  SPA-PTID
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPTID-PTID          TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    1               TO  FLG-PTNUM-ERR
           END-IF
      *
           .
       20011-PTNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目４：診療年月　処理
      *****************************************************************
       20012-SRYYM-SET-SEC          SECTION.
      *
           MOVE    HZN-IN-KOUMOKU (4)  TO  WRK-HEN-SRYYM
      *
           MOVE    SPACE               TO  WRK-SRYYMD
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-HEN-SRYYM       TO  SGDAY-INDAY
           MOVE    "1"                 TO  SGDAY-INTYPE
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY(1:6)    TO  WRK-HEN-SRYYM
               MOVE    SGDAY-SDAY(1:6)     TO  WRK-SRYYM
               MOVE    "01"                TO  WRK-SRYDD
           ELSE
               MOVE    2                   TO  FLG-PTNUM-ERR
           END-IF
      *
           .
       20012-SRYYM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴登録チェック処理
      *****************************************************************
       20013-JYURRK-CHK-SEC          SECTION.
      *
      *    受診歴を検索
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    WRK-SRYYMD          TO  JYURRK-SRYYMD
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key52"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key52"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key52"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-JYURRK          =   ZERO
               IF     (JYURRK-SRYYMD(1:6)  <=  WRK-SRYYM  )
      *            診療行為入力あり
                   MOVE    3               TO  FLG-PTNUM-ERR
                   GO      TO      20013-JYURRK-CHK-EXT
               END-IF
           END-IF
      *    レセ電移行の受診歴を検索
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key53"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key53"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    SPACE               TO  WRK-SRYKA
           PERFORM
               UNTIL   (FLG-JYURRK         =   1    )  OR
                       (FLG-PTNUM-ERR  NOT =   ZERO )
               IF     (JYURRK-SRYYMD(1:6)  =   WRK-SRYYM )
                 AND  (JYURRK-SRYKA(1:1)   =   "A"        )
                   IF     (WRK-SRYKA       =   SPACE )
                      OR  (WRK-SRYKA       <   JYURRK-SRYKA )
                      MOVE    JYURRK-SRYKA     TO  WRK-SRYKA
                   END-IF
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key53"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key53"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      WRK-SRYKA           =   SPACE
               MOVE    "A1"            TO  SPA-SRYKA
           ELSE
               ADD     1               TO  WRK-SRYKA-EDA
               MOVE    WRK-SRYKA       TO  SPA-SRYKA
           END-IF
           .
       20013-JYURRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト共通レコード編集チェック処理
      *****************************************************************
       2001-MEISAI-SYORI-SEC        SECTION.
      *
           INITIALIZE                  TBL-HENKAN-AREA
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  FLG-RE-END
           PERFORM
                   UNTIL  (FLG-CSV         =   1   )
                       OR (FLG-RE-END      =   1   )
               EVALUATE    HZN-IN-KOUMOKU (1)(1:2)
      *            診療行為レコード
                   WHEN    "SI"
                       PERFORM 20011-SI-SYORI-SEC
      *            医薬品レコード
                   WHEN    "IY"
                       PERFORM 20011-IY-SYORI-SEC
      *            特定器材レコード
                   WHEN    "TO"
                       PERFORM 20011-TO-SYORI-SEC
      *            コメントレコード
                   WHEN    "CO"
                       PERFORM 20011-CO-SYORI-SEC
      *            日計表レコード
                   WHEN    "NI"
                       PERFORM 20011-NI-SYORI-SEC
                   WHEN    "SJ"
      *                症状詳記レコード
                       PERFORM 2001-SJ-SYORI-SEC
               END-EVALUATE
      *
      *        レセ電データ　読込処理
               PERFORM 900-CSV-KENSAKU-SEC
               IF      HZN-IN-KOUMOKU (1)  =   "RE"
                                           OR  "GO"
                   MOVE    1               TO  FLG-RE-END
                   MOVE    ZERO            TO  FLG-SJ-KBN
               END-IF
           END-PERFORM
           .
       2001-MEISAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為レコード処理
      *****************************************************************
       20011-SI-SYORI-SEC          SECTION.
      *
           INITIALIZE                  WRK-RECHEN-AREA
      *    診療識別
           MOVE    HZN-IN-KOUMOKU (2)(1:2) TO  WRK-HEN-SRYKBN
      *    剤点数
           MOVE    HZN-IN-KOUMOKU (6)  TO  WRK-HEN-TEXT
           PERFORM 290-SURYO-HENKAN-SEC
           MOVE    WRK-HEN-NUM         TO  WRK-HEN-TENSU
      *    剤回数
           MOVE    HZN-IN-KOUMOKU (7)  TO  WRK-HEN-TEXT
           PERFORM 290-SURYO-HENKAN-SEC
           MOVE    WRK-HEN-NUM         TO  WRK-HEN-ZAIKAIKEI
      *    診療コード
           MOVE    HZN-IN-KOUMOKU (4)(1:9) TO  WRK-HEN-SRYCD
      *    数量
           MOVE    HZN-IN-KOUMOKU (5)  TO  WRK-HEN-TEXT
           PERFORM 290-SURYO-HENKAN-SEC
           MOVE    WRK-HEN-NUM         TO  WRK-HEN-SRYSURYO
      *    コメント
           MOVE    HZN-IN-KOUMOKU(8)(1:9)  TO  WRK-HEN-SRYCD-C (1)
           MOVE    HZN-IN-KOUMOKU(9)       TO  WRK-HEN-MEISYO  (1)
           MOVE    HZN-IN-KOUMOKU(10)(1:9) TO  WRK-HEN-SRYCD-C (2)
           MOVE    HZN-IN-KOUMOKU(11)      TO  WRK-HEN-MEISYO  (2)
           MOVE    HZN-IN-KOUMOKU(12)(1:9) TO  WRK-HEN-SRYCD-C (3)
           MOVE    HZN-IN-KOUMOKU(13)      TO  WRK-HEN-MEISYO  (3)
      *    算定日
           MOVE    13                  TO  IDX-D
           PERFORM VARYING      IDD     FROM    1   BY  1
                   UNTIL        IDD     >   31
               ADD     1                   TO  IDX-D
               MOVE    HZN-IN-KOUMOKU (IDX-D)  TO  WRK-HEN-TEXT
               PERFORM 290-SURYO-HENKAN-SEC
               MOVE    WRK-HEN-NUM         TO  WRK-HEN-DAY (IDD)
      *        最終日
               IF      WRK-HEN-DAY (IDD)   >   ZERO
                   MOVE    IDD             TO  WRK-SRYDD
               END-IF
      *
           END-PERFORM
      *    点数マスタ検索
           MOVE    WRK-HEN-SRYCD       TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *    減点はマイナスとする
           IF      TNS-TENSIKIBETU     =   8
                                       OR  7
               MOVE    1               TO  FLG-PLUSKBN
           ELSE
               MOVE    ZERO            TO  FLG-PLUSKBN
           END-IF
      *
      *    診療識別で剤分離
           IF      WRK-HEN-SRYKBN  NOT =   SPACE
               ADD     1                   TO  IDX
               MOVE    ZERO                TO  IDY
               MOVE    WRK-HEN-SRYKBN      TO  TBL-SRYKBN  (IDX)
      *        病理診断変換
               IF     (TNS-SRYKBN          =   "64" )  AND
                      (WRK-HEN-SRYKBN      =   "60" )
                   MOVE    TNS-SRYKBN          TO  WRK-HEN-SRYKBN
               END-IF
               IF      WRK-HEN-SRYKBN      =   TNS-SRYKBN
                   MOVE    TNS-SRYSYUKBN       TO  TBL-SRYSYUKBN (IDX)
      *            加算の時、診療種別の変更
                   IF      TNS-DATAKBN         =   "2"
                       EVALUATE    TNS-SRYSYUKBN
                       WHEN    "140"
                           MOVE    "143"       TO  TBL-SRYSYUKBN (IDX)
                       WHEN    "400"
                           MOVE    "403"       TO  TBL-SRYSYUKBN (IDX)
                       WHEN    "600"
                           MOVE    "603"       TO  TBL-SRYSYUKBN (IDX)
                       WHEN    "640"
                           MOVE    "643"       TO  TBL-SRYSYUKBN (IDX)
                       WHEN    "700"
                           MOVE    "704"       TO  TBL-SRYSYUKBN (IDX)
                       END-EVALUATE
                   END-IF
               END-IF
               MOVE    WRK-HEN-ZAIKAIKEI   TO  TBL-KAISU  (IDX)
               MOVE    WRK-HEN-DAY-G       TO  TBL-DAY-G  (IDX)
               MOVE    FLG-PLUSKBN         TO  TBL-PLUSKBN (IDX)
               MOVE    ZERO                TO  FLG-ZAIEND
               MOVE    ZERO                TO  FLG-COMMENT
               MOVE    ZERO                TO  FLG-SJ-KBN
           END-IF
      *    前のコードがコメント
           IF     (FLG-COMMENT         =   1  )
              AND (WRK-HEN-SRYKBN      =   SPACE )
               MOVE    WRK-HEN-ZAIKAIKEI   TO  TBL-KAISU  (IDX)
               MOVE    WRK-HEN-DAY-G       TO  TBL-DAY-G  (IDX)
               MOVE    FLG-PLUSKBN         TO  TBL-PLUSKBN (IDX)
               MOVE    ZERO                TO  FLG-COMMENT
           END-IF
      *    前のコードで終了
           IF     (FLG-ZAIEND          =   1  )
              AND (WRK-HEN-SRYKBN      =   SPACE )
              AND (TNS-DATAKBN         =   "1"   )
               ADD     1                   TO  IDX
               MOVE    ZERO                TO  IDY
               MOVE    TBL-SRYKBN(IDX - 1) TO  TBL-SRYKBN  (IDX)
               MOVE    TBL-SRYSYUKBN(IDX - 1)
                                           TO  TBL-SRYSYUKBN  (IDX)
               MOVE    WRK-HEN-ZAIKAIKEI   TO  TBL-KAISU  (IDX)
               MOVE    WRK-HEN-DAY-G       TO  TBL-DAY-G  (IDX)
               MOVE    FLG-PLUSKBN         TO  TBL-PLUSKBN (IDX)
               MOVE    ZERO                TO  FLG-ZAIEND
           END-IF
      *
      *    点数
           IF      WRK-HEN-TENSU       >   ZERO
               ADD     WRK-HEN-TENSU       TO  TBL-SYUTEN (IDX)
      *        剤終了判定
               IF     (TNS-DATAKBN         =   "1" )  AND
                      (TNS-SRYKBN          =   "11"
                                           OR  "12"
                                           OR  "13"
                                           OR  "14"
                                           OR  "60" )
                   MOVE    1               TO  FLG-ZAIEND
               END-IF
           END-IF
      *
      *    明細編集
           PERFORM 20011-TBL-UPD-SEC
           MOVE    WRK-HEN-SRYCD       TO  TBL-SRYCD (IDX IDY)
           MOVE    WRK-HEN-SRYSURYO    TO  TBL-SRYSURYO(IDX IDY)
      *    診療行為のきざみ値入力
           IF     (TBL-SRYSURYO(IDX IDY)   >   1  )
              AND (TBL-SRYCD (IDX IDY)(1:1)    =   "1" )
               MOVE    "1"                 TO  TBL-SURFLG (IDX IDY)
           END-IF
      *
           PERFORM VARYING     IDC     FROM    1   BY  1
                   UNTIL       IDC     >   3
               IF      WRK-HEN-SRYCD-C (IDC)   NOT =   SPACE
      *            コメントコードの対象外判定
                   MOVE    WRK-HEN-SRYCD-C (IDC)   TO  WRK-CO-SRYCD
                   PERFORM 200110-COMMENT-PARACHK-SEC
      *
                   IF      FLG-PARACHK         =   ZERO
                       PERFORM 20011-TBL-UPD-SEC
                       MOVE    WRK-HEN-SRYCD-C (IDC)
                                           TO  TBL-SRYCD (IDX IDY)
                       MOVE    WRK-HEN-MEISYO   (IDC)
                                           TO  TBL-MEISYO(IDX IDY)
                       MOVE    "2"         TO  TBL-MEISKYFLG(IDX IDY)
                   END-IF
               END-IF
           END-PERFORM
           .
       20011-SI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細テーブル処理
      *****************************************************************
       20011-TBL-UPD-SEC          SECTION.
      *
           ADD     1                   TO  IDY
           IF      IDY                 >   50
               ADD     1                   TO  IDX
               MOVE    TBL-SRYKBN (IDX - 1)    TO  TBL-SRYKBN  (IDX)
               MOVE    1                   TO  IDY
           END-IF
           .
       20011-TBL-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    医薬品レコード処理
      *****************************************************************
       20011-IY-SYORI-SEC         SECTION.
      *
           INITIALIZE                  WRK-RECHEN-AREA
      *    診療識別
           MOVE    HZN-IN-KOUMOKU (2)(1:2) TO  WRK-HEN-SRYKBN
      *    剤点数
           MOVE    HZN-IN-KOUMOKU (6)  TO  WRK-HEN-TEXT
           PERFORM 290-SURYO-HENKAN-SEC
           MOVE    WRK-HEN-NUM         TO  WRK-HEN-TENSU
      *    剤回数
           MOVE    HZN-IN-KOUMOKU (7)  TO  WRK-HEN-TEXT
           PERFORM 290-SURYO-HENKAN-SEC
           MOVE    WRK-HEN-NUM         TO  WRK-HEN-ZAIKAIKEI
      *    診療コード
           MOVE    HZN-IN-KOUMOKU (4)(1:9) TO  WRK-HEN-SRYCD
      *    数量
           MOVE    HZN-IN-KOUMOKU (5)  TO  WRK-HEN-TEXT
           PERFORM 290-SURYO-HENKAN-SEC
           MOVE    WRK-HEN-NUM         TO  WRK-HEN-SRYSURYO
      *    コメント
           MOVE    HZN-IN-KOUMOKU(8)(1:9)  TO  WRK-HEN-SRYCD-C (1)
           MOVE    HZN-IN-KOUMOKU(9)       TO  WRK-HEN-MEISYO  (1)
           MOVE    HZN-IN-KOUMOKU(10)(1:9) TO  WRK-HEN-SRYCD-C (2)
           MOVE    HZN-IN-KOUMOKU(11)      TO  WRK-HEN-MEISYO  (2)
           MOVE    HZN-IN-KOUMOKU(12)(1:9) TO  WRK-HEN-SRYCD-C (3)
           MOVE    HZN-IN-KOUMOKU(13)      TO  WRK-HEN-MEISYO  (3)
      *    算定日
           MOVE    13                  TO  IDX-D
           PERFORM VARYING     IDD     FROM    1   BY  1
                   UNTIL       IDD     >   31
               ADD     1                   TO  IDX-D
               MOVE    HZN-IN-KOUMOKU (IDX-D)  TO  WRK-HEN-TEXT
               PERFORM 290-SURYO-HENKAN-SEC
               MOVE    WRK-HEN-NUM         TO  WRK-HEN-DAY (IDD)
           END-PERFORM
      *    点数マスタ検索
           MOVE    WRK-HEN-SRYCD       TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *    減点はマイナスとする
           IF      TNS-TENSIKIBETU     =   7
               MOVE    1               TO  FLG-PLUSKBN
           ELSE
               MOVE    ZERO            TO  FLG-PLUSKBN
           END-IF
      *
      *    診療識別で剤分離
           IF      WRK-HEN-SRYKBN  NOT =   SPACE
               ADD     1                   TO  IDX
               MOVE    ZERO                TO  IDY
               MOVE    WRK-HEN-SRYKBN      TO  TBL-SRYKBN  (IDX)
      *        注射の時は、手技料なしの診療種別へ
               IF     (WRK-HEN-SRYKBN      =   "31"
                                           OR  "32"
                                           OR  "33")
               AND    (SPA-NYUGAIKBN       =   "2" )
                   MOVE    WRK-HEN-SRYKBN      TO  TBL-SRYSYUKBN(IDX)
                                                             (1:2)
                   MOVE    "1"                 TO  TBL-SRYSYUKBN(IDX)
                                                             (3:1)
               END-IF
      *
               MOVE    FLG-PLUSKBN         TO  TBL-PLUSKBN (IDX)
               MOVE    WRK-HEN-ZAIKAIKEI   TO  TBL-KAISU  (IDX)
               MOVE    WRK-HEN-DAY-G       TO  TBL-DAY-G  (IDX)
               MOVE    ZERO                TO  FLG-ZAIEND
               MOVE    ZERO                TO  FLG-COMMENT
               MOVE    ZERO                TO  FLG-SJ-KBN
           END-IF
      *    前のコードがコメント
           IF     (FLG-COMMENT         =   1  )
              AND (WRK-HEN-SRYKBN      =   SPACE )
               MOVE    WRK-HEN-ZAIKAIKEI   TO  TBL-KAISU  (IDX)
               MOVE    WRK-HEN-DAY-G       TO  TBL-DAY-G  (IDX)
               MOVE    FLG-PLUSKBN         TO  TBL-PLUSKBN (IDX)
               MOVE    ZERO                TO  FLG-COMMENT
           END-IF
      *    前のコードで終了
           IF     (FLG-ZAIEND          =   1    )
              AND (WRK-HEN-SRYKBN      =   SPACE )
               ADD     1                   TO  IDX
               MOVE    ZERO                TO  IDY
               MOVE    TBL-SRYKBN(IDX - 1) TO  TBL-SRYKBN  (IDX)
               MOVE    WRK-HEN-ZAIKAIKEI   TO  TBL-KAISU  (IDX)
               MOVE    WRK-HEN-DAY-G       TO  TBL-DAY-G  (IDX)
               MOVE    ZERO                TO  FLG-ZAIEND
           END-IF
      *
      *    投薬の時のみ、点数で剤終了とする
           IF     (WRK-HEN-TENSU       >   ZERO )
             AND  (TBL-SRYKBN (IDX)    =   "21"
                                       OR  "22"
                                       OR  "23" )
               ADD     WRK-HEN-TENSU       TO  TBL-YKZTEN (IDX)
               MOVE    WRK-HEN-ZAIKAIKEI   TO  TBL-KAISU  (IDX)
               MOVE    WRK-HEN-DAY-G       TO  TBL-DAY-G  (IDX)
               MOVE    1                   TO  FLG-ZAIEND
           ELSE
      *        点数は集計する
               ADD     WRK-HEN-TENSU       TO  TBL-YKZTEN (IDX)
           END-IF
      *    明細編集
           PERFORM 20011-TBL-UPD-SEC
           MOVE    WRK-HEN-SRYCD       TO  TBL-SRYCD (IDX IDY)
           MOVE    WRK-HEN-SRYSURYO    TO  TBL-SRYSURYO(IDX IDY)
           PERFORM VARYING      IDC     FROM    1   BY  1
                   UNTIL       IDC     >   3
               IF      WRK-HEN-SRYCD-C (IDC)   NOT =   SPACE
                   PERFORM 20011-TBL-UPD-SEC
                   MOVE    WRK-HEN-SRYCD-C (IDC)
                                           TO  TBL-SRYCD (IDX IDY)
                   MOVE    WRK-HEN-MEISYO (IDC)
                                           TO  TBL-MEISYO(IDX IDY)
                   MOVE    "2"             TO  TBL-MEISKYFLG(IDX IDY)
               END-IF
           END-PERFORM
           .
       20011-IY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定器材レコード処理
      *****************************************************************
       20011-TO-SYORI-SEC         SECTION.
      *
           INITIALIZE                  WRK-RECHEN-AREA
      *    診療識別
           MOVE    HZN-IN-KOUMOKU (2)(1:2) TO  WRK-HEN-SRYKBN
      *    剤点数
           MOVE    HZN-IN-KOUMOKU (6)  TO  WRK-HEN-TEXT
           PERFORM 290-SURYO-HENKAN-SEC
           MOVE    WRK-HEN-NUM         TO  WRK-HEN-TENSU
      *    剤回数
           MOVE    HZN-IN-KOUMOKU (7)  TO  WRK-HEN-TEXT
           PERFORM 290-SURYO-HENKAN-SEC
           MOVE    WRK-HEN-NUM         TO  WRK-HEN-ZAIKAIKEI
      *    診療コード
           MOVE    HZN-IN-KOUMOKU (4)(1:9) TO  WRK-HEN-SRYCD
      *    数量
           MOVE    HZN-IN-KOUMOKU (5)  TO  WRK-HEN-TEXT
           PERFORM 290-SURYO-HENKAN-SEC
           MOVE    WRK-HEN-NUM         TO  WRK-HEN-SRYSURYO
      *    商品名
           MOVE    HZN-IN-KOUMOKU (11) TO  WRK-HEN-SHOHINMEI
      *    その他の特定器材は、名称を商品名とする
           IF      WRK-HEN-SRYCD       =   "777770000"
               IF      HZN-IN-KOUMOKU (11)     =   SPACE
                   MOVE    HZN-IN-KOUMOKU (10) TO  WRK-HEN-SHOHINMEI
               END-IF
           END-IF
      *    コメント
           MOVE    HZN-IN-KOUMOKU(12)(1:9) TO  WRK-HEN-SRYCD-C (1)
           MOVE    HZN-IN-KOUMOKU(13)      TO  WRK-HEN-MEISYO  (1)
           MOVE    HZN-IN-KOUMOKU(14)(1:9) TO  WRK-HEN-SRYCD-C (2)
           MOVE    HZN-IN-KOUMOKU(15)      TO  WRK-HEN-MEISYO  (2)
           MOVE    HZN-IN-KOUMOKU(16)(1:9) TO  WRK-HEN-SRYCD-C (3)
           MOVE    HZN-IN-KOUMOKU(17)      TO  WRK-HEN-MEISYO  (3)
      *    算定日
           MOVE    17                  TO  IDX-D
           PERFORM VARYING      IDD     FROM    1   BY  1
                   UNTIL       IDD     >   31
               ADD     1                   TO  IDX-D
               MOVE    HZN-IN-KOUMOKU (IDX-D)  TO  WRK-HEN-TEXT
               PERFORM 290-SURYO-HENKAN-SEC
               MOVE    WRK-HEN-NUM         TO  WRK-HEN-DAY (IDD)
           END-PERFORM
      *
      *    診療識別で剤分離
           IF      WRK-HEN-SRYKBN  NOT =   SPACE
               ADD     1                   TO  IDX
               MOVE    ZERO                TO  IDY
               MOVE    WRK-HEN-SRYKBN      TO  TBL-SRYKBN (IDX)
               MOVE    WRK-HEN-ZAIKAIKEI   TO  TBL-KAISU  (IDX)
               MOVE    WRK-HEN-DAY-G       TO  TBL-DAY-G  (IDX)
               MOVE    ZERO                TO  FLG-ZAIEND
               MOVE    ZERO                TO  FLG-COMMENT
               MOVE    ZERO                TO  FLG-SJ-KBN
           END-IF
      *    前のコードがコメント
           IF     (FLG-COMMENT         =   1  )
              AND (WRK-HEN-SRYKBN      =   SPACE )
               MOVE    WRK-HEN-ZAIKAIKEI   TO  TBL-KAISU  (IDX)
               MOVE    WRK-HEN-DAY-G       TO  TBL-DAY-G  (IDX)
               MOVE    ZERO                TO  FLG-COMMENT
           END-IF
      *
           IF     (WRK-HEN-TENSU       >   ZERO )
      *        点数は集計する
               ADD     WRK-HEN-TENSU       TO  TBL-KIZAITEN (IDX)
           END-IF
      *    明細編集
           PERFORM 20011-TBL-UPD-SEC
           MOVE    WRK-HEN-SRYCD       TO  TBL-SRYCD (IDX IDY)
           MOVE    WRK-HEN-SRYSURYO    TO  TBL-SRYSURYO(IDX IDY)
      *    商品名称はコメントとする
           IF      WRK-HEN-SHOHINMEI   NOT =   SPACE
               PERFORM 20011-TBL-UPD-SEC
               MOVE    "810000001"         TO  TBL-SRYCD (IDX IDY)
               MOVE    WRK-HEN-SHOHINMEI   TO  TBL-MEISYO(IDX IDY)
               MOVE    "2"                 TO  TBL-MEISKYFLG(IDX IDY)
           END-IF
           PERFORM VARYING     IDC     FROM    1   BY  1
                   UNTIL       IDC     >   3
               IF      WRK-HEN-SRYCD-C (IDC)   NOT =   SPACE
                   PERFORM 20011-TBL-UPD-SEC
                   MOVE    WRK-HEN-SRYCD-C (IDC)
                                           TO  TBL-SRYCD (IDX IDY)
                   MOVE    WRK-HEN-MEISYO (IDC)
                                           TO  TBL-MEISYO(IDX IDY)
                   MOVE    "2"             TO  TBL-MEISKYFLG(IDX IDY)
               END-IF
           END-PERFORM
           .
       20011-TO-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントレコード処理
      *****************************************************************
       20011-CO-SYORI-SEC         SECTION.
      *
      *    コメントコードの対象外判定
           MOVE    HZN-IN-KOUMOKU (4)(1:9) TO  WRK-CO-SRYCD
           PERFORM 200110-COMMENT-PARACHK-SEC
      ***  IF      FLG-PARACHK         =   1
      *        GO      TO      20011-CO-SYORI-EXT
      ***  END-IF
      *
           INITIALIZE                  WRK-RECHEN-AREA
      *    診療識別
           MOVE    HZN-IN-KOUMOKU (2)(1:2) TO  WRK-HEN-SRYKBN
      *    診療コード
           MOVE    HZN-IN-KOUMOKU (4)(1:9) TO  WRK-HEN-SRYCD-C (1)
      *    名称
           MOVE    HZN-IN-KOUMOKU (5)      TO  WRK-HEN-MEISYO  (1)
      *
      *    診療識別＝９９、０１は症状詳記とする
           IF    ((WRK-HEN-SRYKBN      =   "99"
                                       OR  "01" )
             OR   (FLG-SJ-KBN          =   1    ) )
           AND   ( FLG-PARACHK         =   ZERO   )
      *        名称を変更
               MOVE    SPACE               TO  HZN-SJ-TBL
               MOVE    "99"                TO  HZN-SJ-KOUMOKU(2)
               MOVE    HZN-IN-KOUMOKU (5)  TO  HZN-SJ-KOUMOKU(3)
               MOVE    1                   TO  FLG-SJ-KBN
      *        症状詳記レコード
               PERFORM 2001-SJ-INS-SYORI-SEC
               INITIALIZE                  WRK-RECHEN-AREA
               GO       TO      20011-CO-SYORI-EXT
           END-IF
           MOVE    ZERO                TO  FLG-SJ-KBN
      *
      *    診療識別で剤分離
           IF      WRK-HEN-SRYKBN  NOT =   SPACE
               ADD     1                   TO  IDX
               MOVE    ZERO                TO  IDY
               MOVE    WRK-HEN-SRYKBN      TO  TBL-SRYKBN (IDX)
               MOVE    1                   TO  FLG-COMMENT
               MOVE    ZERO                TO  FLG-ZAIEND
           END-IF
      *    読み飛ばし
           IF      FLG-PARACHK         =   1
               INITIALIZE                  WRK-RECHEN-AREA
               GO      TO      20011-CO-SYORI-EXT
           END-IF
      *
      *    明細編集
           PERFORM 20011-TBL-UPD-SEC
           MOVE    WRK-HEN-SRYCD-C (1) TO  TBL-SRYCD (IDX IDY)
           MOVE    WRK-HEN-MEISYO  (1) TO  TBL-MEISYO(IDX IDY)
           .
       20011-CO-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントコードの対象外判定処理
      *****************************************************************
       200110-COMMENT-PARACHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-PARACHK
           PERFORM VARYING    IDP      FROM    1   BY  1
                   UNTIL     (IDP      >   200 )
                        OR   (PARA-12-1 (IDP)  =   SPACE)
                        OR   (FLG-PARACHK  NOT =   ZERO )
               IF      WRK-CO-SRYCD    =   PARA-12-1 (IDP)
                   MOVE    1               TO  FLG-PARACHK
               END-IF
           END-PERFORM
           .
       200110-COMMENT-PARACHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    日計表レコード処理
      *****************************************************************
       20011-NI-SYORI-SEC         SECTION.
      *
           INITIALIZE                  WRK-RECHEN-AREA
      *    算定日
           MOVE    2                   TO  IDX-D
           PERFORM VARYING      IDD     FROM    1   BY  1
                   UNTIL        IDD     >   31
               ADD     1                   TO  IDX-D
               MOVE    HZN-IN-KOUMOKU (IDX-D)  TO  WRK-HEN-TEXT
               PERFORM 290-SURYO-HENKAN-SEC
               MOVE    WRK-HEN-NUM         TO  WRK-HEN-DAY (IDD)
           END-PERFORM
      *    剤の算定回数とする
           IF     (IDX                 >   ZERO )
              AND (TBL-DAY-G (IDX)     =   ZERO )
               PERFORM VARYING     IDD     FROM    1   BY  1
                       UNTIL       IDD     >   31
                   IF      WRK-HEN-DAY (IDD)   NOT =   ZERO
                       ADD     WRK-HEN-DAY (IDD)   TO
                                               TBL-DAY (IDX IDD)
                   END-IF
               END-PERFORM
           END-IF
           .
       20011-NI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       2009-TOUROKU-SYORI-SEC          SECTION.
      *
           INITIALIZE                  TBL-JYURRK-AREA
      *
           INITIALIZE                  TBL-JYURRKZAI-AREA
           MOVE    ZERO                TO  IDX-J
      *    剤番号
           MOVE    PTINF-MAXZAINUM     TO  WRK-ZAINUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   900 )
                         OR   (TBL-SRYKBN (IDX)    =   SPACE )
                         OR   (FLG-END     NOT =   ZERO )
               PERFORM 20091-MEISAI-HEN-SEC
           END-PERFORM
      *
      *    受診履歴追加
           PERFORM VARYING     IDD     FROM    1   BY  1
                   UNTIL       IDD     >   31
               IF      TBL-JYUZAI-MAX (IDD)    >   ZERO
      *            受診履歴作成
                   PERFORM 20092-JYURRK-INS-SEC
                END-IF
           END-PERFORM
      *
           IF      FLG-ERR             =   ZERO
      *        患者の剤番号更新
               PERFORM 20093-PTINF-UPD-SEC
           END-IF
      *
           ADD     1                   TO  CNT-COMMIT
      *
           ADD     1                   TO  CNT-OUTRE
      *
      *    症状詳記連番エラー
           IF     (FLG-ERR2        NOT =   ZERO )
            AND   (FLG-ERR             =   ZERO )
               MOVE    FLG-ERR2        TO  FLG-ERR
           END-IF
      *
           IF      FLG-ERR         NOT =   ZERO
      *        エラーリスト出力処理
               PERFORM 270-ERRREC-EDIT-SEC
               PERFORM 270-ERRLST-OUT-SEC
           END-IF
           MOVE    ZERO                TO  FLG-ERR
      *
           INITIALIZE                  TBL-HENKAN-AREA
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  FLG-SJ-KBN
           .
       2009-TOUROKU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       20091-MEISAI-HEN-SEC        SECTION.
      *
      *    入院料は対象外とする
           IF      TBL-SRYKBN (IDX)    =   "90"
                                       OR  "92"
                                       OR  "97"
               GO      TO      20091-MEISAI-HEN-EXT
           END-IF
      *
           ADD     1                   TO  WRK-ZAINUM
      *    診療区分・診療種別決定
           IF      TBL-SRYKBN (IDX)    =   "01"
               MOVE    "99"                TO  WRK-SRYKBN
               MOVE    "990"               TO  WRK-SRYSYUKBN
           ELSE
               MOVE    TBL-SRYKBN(IDX)     TO  WRK-SRYKBN
               IF      TBL-SRYSYUKBN (IDX) =   SPACE
                   MOVE    TBL-SRYKBN(IDX)     TO  WRK-SRYSYUKBN(1:2)
                   MOVE    "0"                 TO  WRK-SRYSYUKBN(3:1)
               ELSE
                   MOVE    TBL-SRYSYUKBN(IDX)  TO  WRK-SRYSYUKBN
               END-IF
           END-IF
      *    診療会計テーブル
           PERFORM 200911-SRYACCT-INIT-SEC
      *    算定日なしは対象外
           IF      ACCT-ZAIKAISU       =   ZERO
               COMPUTE WRK-ZAINUM      =   WRK-ZAINUM  -   1
               GO      TO      20091-MEISAI-HEN-EXT
           END-IF
      *
      *    診療行為テーブル
           PERFORM 200912-SRYACT-SYORI-SEC
      *    診療会計テーブル
           PERFORM 200913-SRYACCT-INS-SEC
           .
       20091-MEISAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブル編集処理
      *****************************************************************
       200911-SRYACCT-INIT-SEC        SECTION.
      *
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    SPA-SRYKA           TO  ACCT-SRYKA
           MOVE    WRK-SRYYM           TO  ACCT-SRYYM
           MOVE    WRK-SRYKBN          TO  ACCT-SRYKBN
           MOVE    WRK-ZAINUM          TO  ACCT-ZAINUM
           MOVE    9999                TO  ACCT-HKNCOMBI
      *    剤点数
           COMPUTE ACCT-ZAITEN         =   TBL-SYUTEN (IDX)
                                       +   TBL-YKZTEN (IDX)
                                       +   TBL-KIZAITEN (IDX)
      *    手技点数１
           MOVE    TBL-SYUTEN (IDX)    TO  ACCT-SYUTEN1
      *    手技点数２
           MOVE    ZERO                TO  ACCT-SYUTEN2
      *    薬剤点数
           MOVE    TBL-YKZTEN (IDX)    TO  ACCT-YKZTEN
      *    器材点数
           MOVE    TBL-KIZAITEN (IDX)  TO  ACCT-KIZAITEN
      *
      *    マイナス点数編集
           IF      TBL-PLUSKBN (IDX)   =   1
      *        附加情報あり
               MOVE    "1"                 TO  ACCT-ZAIREQFLG
           END-IF
      *
      *    算定日より受診履歴判定
           PERFORM 2009112-DAY-JYURRK-SEC
      *
      *    診療コード計
           MOVE    ZERO                TO  WRK-ACCT-SRYCDTOTAL
      *    数量計
           MOVE    ZERO                TO  WRK-ACCT-SURYOUTOTAL
      *    明細数
           MOVE    ZERO                TO  WRK-ACCT-MEISAISU
      *
           .
       200911-SRYACCT-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター登録処理
      *****************************************************************
       200913-SRYACCT-INS-SEC        SECTION.
      *
      *    診療コード計
           MOVE    WRK-ACCT-SRYCDTOTAL     TO  ACCT-SRYCDTOTAL
      *    数量計
           MOVE    WRK-ACCT-SURYOUTOTAL    TO  ACCT-SURYOUTOTAL
      *    明細数
           MOVE    WRK-ACCT-MEISAISU       TO  ACCT-MEISAISU
      *
           MOVE    SMCNDATE-YMD        TO  ACCT-CREYMD
      *!!  MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
      *!!  MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
      *
           MOVE    SPA-OPID            TO  ACCT-OPID
      *    移行用
           MOVE    SPA-TERMID          TO  ACCT-TERMID
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "SRYACCT INS ERR:" ACCT-KEY  "."
               MOVE    9               TO  FLG-ERR
           END-IF
      *    マイナス点数編集
           IF     (TBL-PLUSKBN (IDX)   =   1  )
             AND  (FLG-ERR             =   ZERO )
      *        附加情報追加
               PERFORM 200913-SRYACCTPLUS-SYORI-SEC
           END-IF
      *
           .
       200913-SRYACCT-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計附加情報処理
      *****************************************************************
       200913-SRYACCTPLUS-SYORI-SEC              SECTION.
      *
           MOVE    SPACE               TO  SRYACCTPLUS-REC
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    ACCT-HOSPNUM        TO  ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID           TO  ACCTP-PTID
           MOVE    ACCT-SRYKA          TO  ACCTP-SRYKA
           MOVE    ACCT-SRYYM          TO  ACCTP-SRYYM
           MOVE    ACCT-ZAINUM         TO  ACCTP-ZAINUM
      *    連番号
           MOVE    1                   TO  ACCTP-RENNUM
      *    点数区分
           MOVE    TBL-PLUSKBN (IDX)   TO  ACCTP-PLUSKBN
      *
           MOVE    SMCNDATE-YMD        TO  ACCTP-CREYMD
      **   MOVE    SMCNDATE-YMD        TO  ACCTP-UPYMD
      **   MOVE    SMCNDATE-HMS        TO  ACCTP-UPHMS
      *
           MOVE    SPA-OPID            TO  ACCTP-OPID
      *    移行用
           MOVE    SPA-TERMID          TO  ACCTP-TERMID
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC          NOT =   ZERO
              DISPLAY "SRYACCTPLUS INS ERR:" ACCTP-KEY  "."
              DISPLAY "MCP-RC:" MCP-RC ":" 
              MOVE    "0005"          TO  SPA-ERRCD
           END-IF
      *
           .
       200913-SRYACCTPLUS-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    算定日より受診履歴判定処理
      *****************************************************************
       2009112-DAY-JYURRK-SEC        SECTION.
      *
           PERFORM VARYING     IDD     FROM    1   BY  1
                   UNTIL       IDD     >   31
               IF      TBL-DAY (IDX IDD)   NOT =   ZERO
      *            算定日編集
                   PERFORM 2009111-ACCT-DAYSET-SEC
      *            受診履歴編集 
                   PERFORM 2009112-JYURRK-TBLSET-SEC
                END-IF
           END-PERFORM
           .
       2009112-DAY-JYURRK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定日編集処理
      *****************************************************************
       2009111-ACCT-DAYSET-SEC        SECTION.
      *
      *    受診履歴の連番は１のみとする
           MOVE    TBL-DAY (IDX IDD)   TO  ACCT-DAY (2 IDD)
           ADD     TBL-DAY (IDX IDD)   TO  ACCT-DAY (1 IDD)
      *
           ADD     TBL-DAY (IDX IDD)   TO  ACCT-ZAIKAISU
      *
           .
       2009111-ACCT-DAYSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴剤番号編集処理
      *****************************************************************
       2009112-JYURRK-TBLSET-SEC        SECTION.
      *
      *    診療区分
           EVALUATE    WRK-SRYKBN
               WHEN   "11"
               WHEN   "12"
               WHEN   "13"
               WHEN   "14"
                   MOVE    "01"        TO  TBL-JYUZAI-SRYKBN (IDD 1)
               WHEN   "21"
                   MOVE    "01"        TO  TBL-JYUZAI-SRYKBN (IDD 2)
               WHEN   "22"
                   MOVE    "01"        TO  TBL-JYUZAI-SRYKBN (IDD 3)
               WHEN   "23"
                   MOVE    "01"        TO  TBL-JYUZAI-SRYKBN (IDD 4)
               WHEN   "31"
               WHEN   "32"
               WHEN   "33"
                   MOVE    "01"        TO  TBL-JYUZAI-SRYKBN (IDD 5)
               WHEN   "40"
                   MOVE    "01"        TO  TBL-JYUZAI-SRYKBN (IDD 6)
               WHEN   "50"
                   MOVE    "01"        TO  TBL-JYUZAI-SRYKBN (IDD 7)
               WHEN   "54"
                   MOVE    "01"        TO  TBL-JYUZAI-SRYKBN (IDD 8)
               WHEN   "60"
                   MOVE    "01"        TO  TBL-JYUZAI-SRYKBN (IDD 9)
               WHEN   "64"
                   MOVE    "01"        TO  TBL-JYUZAI-SRYKBN (IDD 9)
               WHEN   "70"
                   MOVE    "01"        TO  TBL-JYUZAI-SRYKBN (IDD 10)
               WHEN   "80"
                   MOVE    "01"        TO  TBL-JYUZAI-SRYKBN (IDD 11)
           END-EVALUATE
      *
      *    剤番号
           MOVE    TBL-JYUZAI-MAX (IDD)    TO  IDX-J
           ADD     1                   TO  IDX-J
      *    １３５剤以上は変換不可
           IF      IDX-J               >=  135
               GO      TO      2009112-JYURRK-TBLSET-EXT
           END-IF
           MOVE    WRK-ZAINUM          TO  TBL-JYUZAI-ZAINUM (IDD IDX-J)
           MOVE    IDX-J               TO  TBL-JYUZAI-MAX (IDD)
      *
           .
       2009112-JYURRK-TBLSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為テーブル編集処理
      *****************************************************************
       200912-SRYACT-SYORI-SEC        SECTION.
      *
           MOVE    ZERO                TO  WRK-SRY-RENNUM
           MOVE    ZERO                TO  IDX-SIN
           MOVE    ZERO                TO  FLG-BUIARI
           MOVE    ZERO                TO  FLG-SYUGI
      *
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                      SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    SPA-SRYKA           TO  SRY-SRYKA
           MOVE    WRK-SRYYM           TO  SRY-SRYYM
      *    剤番号
           MOVE    WRK-ZAINUM          TO  SRY-ZAINUM
           MOVE    WRK-SRYSYUKBN       TO  SRY-SRYSYUKBN
           MOVE    WRK-SRYKBN          TO  SRY-SRYKBN
           MOVE    ZERO                TO  SRY-JIHIMONEYTOTAL
      *    連番号
           ADD     1                   TO  WRK-SRY-RENNUM
      *    コメント枝番番号
           MOVE    ZERO                TO  WRK-MEIBAN
           MOVE    ZERO                TO  IDX-SIN
      *
           PERFORM VARYING     IDY     FROM    1    BY  1
                   UNTIL      (IDY     >   50 )
                          OR  (TBL-SRYCD (IDX IDY) =   SPACE )
               ADD     1                   TO  IDX-SIN
      *
               IF      IDX-SIN             >   5
                   PERFORM 2009121-SRYACT-INS-SEC
      *            連番号
                   MOVE    1               TO  IDX-SIN
                   ADD     1               TO  WRK-SRY-RENNUM
               END-IF
      *
               MOVE    TBL-SRYCD (IDX IDY)     TO  SRY-SRYCD (IDX-SIN)
               MOVE    TBL-SRYSURYO(IDX IDY)   TO  SRY-SRYSURYO
                                                            (IDX-SIN)
               MOVE    ZERO                    TO  SRY-SRYKAISU
                                                            (IDX-SIN)
               IF      TBL-SURFLG (IDX IDY)    =   "1"
                   MOVE    "S"                 TO  SRY-AUTOKBN
                                                            (IDX-SIN)
               END-IF
      *
      *        （減）は自動発生分とする
               IF      TBL-SRYCD (IDX IDY)     =   "820000047"
                   MOVE    "3"                 TO  SRY-AUTOKBN
                                                            (IDX-SIN)
               END-IF
      *        コメント登録
               IF      SRY-SRYCD (IDX-SIN)(1:1)    =   "8"
                   IF      TBL-MEISYO (IDX IDY)    NOT =   SPACE
                       PERFORM 2009121-PTCOM-SYORI-SEC
                   END-IF
      *            関連コメント指示
                   IF     (TBL-MEISKYFLG (IDX IDY) =   "2" )
                     AND  (IDY                 >   1       )
                       MOVE    "2"             TO  SRY-MEISKYFLG
                                                            (IDX-SIN)
                   END-IF
               END-IF
      *        手技料あり
               IF      SRY-SRYCD (IDX-SIN)(1:1)    =   "1"
                   MOVE    1               TO  FLG-SYUGI
               END-IF
      *
      *        明細数
               ADD     1                   TO  WRK-ACCT-MEISAISU
      *        診療コード計
               MOVE    SRY-SRYCD (IDX-SIN) TO  WRK-SRYCD-9
               ADD     WRK-SRYCD-9         TO  WRK-ACCT-SRYCDTOTAL
      *        数量計
               ADD     SRY-SRYSURYO (IDX-SIN)
                                           TO  WRK-ACCT-SURYOUTOTAL
      *
           END-PERFORM
      *
           IF      IDX-SIN         NOT =   ZERO
               PERFORM 2009121-SRYACT-INS-SEC
           END-IF
      *
           .
       200912-SRYACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター処理
      *****************************************************************
       2009121-SRYACT-INS-SEC        SECTION.
      *
           MOVE    WRK-SRY-RENNUM      TO  SRY-RENNUM
      *
           MOVE    SMCNDATE-YMD        TO  SRY-CREYMD
      *!!  MOVE    SMCNDATE-YMD        TO  SRY-UPYMD
      *!!  MOVE    SMCNDATE-HMS        TO  SRY-UPHMS
      *
           MOVE    SPA-OPID            TO  SRY-OPID
      *    移行用
           MOVE    SPA-TERMID          TO  SRY-TERMID
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "SRYACT INS ERR:" SRY-KEY  "."
               MOVE    9               TO  FLG-ERR
           END-IF
      *
      *    診療テーブルクリア
           PERFORM VARYING IDX-SIN     FROM    1   BY  1
                   UNTIL   IDX-SIN     >   5
               INITIALIZE              SRY-SRYACT-TBL (IDX-SIN)
           END-PERFORM
           .
       2009121-SRYACT-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメント名称編集処理
      *****************************************************************
       2009121-PTCOM-SYORI-SEC        SECTION.
      *
      *!!!!
           IF      TBL-SRYCD (IDX IDY) =   "810000001"
      *        画像診断の部位変換
               IF      (WRK-SRYKBN         =   "70" )
                  AND  (FLG-BUIARI         =   ZERO )
                  AND  (FLG-SYUGI          =   ZERO )
                   MOVE    TBL-MEISYO (IDX IDY)    TO  WRK-MEISYO
                   PERFORM 2009122-BUIHEN-SEC
               END-IF
      *        投薬のコメントを用法変換
               IF      (WRK-SRYKBN         =   "21"
                                           OR  "22"
                                           OR  "23" )
                   MOVE    TBL-MEISYO (IDX IDY)    TO  WRK-MEISYO
                   PERFORM 2009122-YOUHOUHEN-SEC
               END-IF
           END-IF
      *!!!!
      *
           IF      TBL-SRYCD (IDX IDY) =   "810000001"
      *        フリーコメント
               MOVE    TBL-MEISYO (IDX IDY)    TO  WRK-MEISYO
               MOVE    SPACE                   TO  WRK-INPUTCHI-G
               PERFORM 2009121-PTCOM-INS-SEC
           ELSE
               EVALUATE    TBL-SRYCD (IDX IDY)(1:2)
               WHEN    "83"
                   PERFORM 2009122-PTCOM-83-SEC
               WHEN    "84"
                   PERFORM 2009122-PTCOM-84-SEC
               WHEN    "89"
                   PERFORM 2009122-PTCOM-89-SEC
               END-EVALUATE
           END-IF
           .
       2009121-PTCOM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画像診断コメント部位変換処理
      *****************************************************************
       2009122-BUIHEN-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-BUIOK
      *
           INITIALIZE                  ORCSSRCCOMMCD-AREA
           MOVE    2                   TO  SRCCOM-COMMENT-KBN
           MOVE    WRK-SRYYMD          TO  SRCCOM-MSTYMD
           MOVE    WRK-MEISYO          TO  SRCCOM-COMMENT
      *
           CALL    "ORCSSRCCOMMCD" USING   ORCSSRCCOMMCD-AREA
                                           SPA-AREA
      *
           IF     (SRCCOM-RC               =   ZERO  )
             AND  (SRCCOM-COMMENT-CODE NOT =   SPACE )
               MOVE    SRCCOM-COMMENT-CODE TO  TBL-SRYCD (IDX IDY)
      *
               MOVE    TBL-SRYCD (IDX IDY) TO  SRY-SRYCD (IDX-SIN)
               MOVE    1                   TO  FLG-BUIOK
               MOVE    1                   TO  FLG-BUIARI
           END-IF
      *
           .
       2009122-BUIHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    投薬コメント用法変換処理
      *****************************************************************
       2009122-YOUHOUHEN-SEC        SECTION.
      *
           INITIALIZE                  ORCSSRCCOMMCD-AREA
           MOVE    1                   TO  SRCCOM-COMMENT-KBN
           MOVE    WRK-SRYYMD          TO  SRCCOM-MSTYMD
           MOVE    WRK-MEISYO          TO  SRCCOM-COMMENT
      *
           CALL    "ORCSSRCCOMMCD" USING   ORCSSRCCOMMCD-AREA
                                           SPA-AREA
      *
           IF     (SRCCOM-RC               =   ZERO  )
             AND  (SRCCOM-COMMENT-CODE NOT =   SPACE )
               MOVE    SRCCOM-COMMENT-CODE TO  TBL-SRYCD (IDX IDY)
      *
               MOVE    TBL-SRYCD (IDX IDY) TO  SRY-SRYCD (IDX-SIN)
           END-IF
      *
           .
       2009122-YOUHOUHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメント登録処理
      *****************************************************************
       2009121-PTCOM-INS-SEC        SECTION.
      *
           ADD     1                   TO  WRK-MEIBAN
           INITIALIZE                  PTCOM-REC
      *医療機関ＩＤ
           MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
      *患者ＩＤ
           MOVE    SRY-PTID            TO  PTCOM-PTID
      *剤番号
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
      *診療コード
           MOVE    SRY-SRYCD (IDX-SIN)
                                       TO  PTCOM-SRYCD
      *コメント枝番番号
           MOVE    WRK-MEIBAN          TO  PTCOM-RENNUM
      *入力コメント
           MOVE    WRK-MEISYO          TO  PTCOM-INPUTCOMENT
      *入力値
           MOVE    WRK-INPUTCHI-G     TO   PTCOM-INPUTCHI-AREA
      *
           MOVE    SMCNDATE-YMD        TO  PTCOM-CREYMD
      *    MOVE    SMCNDATE-YMD        TO  PTCOM-UPYMD
      *    MOVE    SMCNDATE-HMS        TO  PTCOM-UPHMS
      *
           MOVE    SPA-OPID            TO  PTCOM-OPID
           MOVE    SPA-TERMID          TO  PTCOM-TERMID
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "K03 PTCOM INS ERR:" PTCOM-KEY
               MOVE    9               TO  FLG-ERR
           END-IF
      *
      *    明細番号
           MOVE    WRK-MEIBAN          TO  SRY-INPUTNUM  (IDX-SIN)
      *
           .
       2009121-PTCOM-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントコード（８３）編集処理
      *****************************************************************
       2009122-PTCOM-83-SEC        SECTION.
      *
           MOVE    SRY-SRYCD (IDX-SIN) TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *    マスタの名称＋文字データ
           MOVE    SPACE               TO  WRK-MEISYO
           STRING  TNS-NAME
                   TBL-MEISYO (IDX IDY)
                                       DELIMITED   BY  SPACE
                                       INTO    WRK-MEISYO
           END-STRING
           MOVE    SPACE                   TO  WRK-INPUTCHI-G
           PERFORM 2009121-PTCOM-INS-SEC
      *
           .
       2009122-PTCOM-83-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントコード（８４）編集処理
      *****************************************************************
       2009122-PTCOM-84-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-INPUT-G
           MOVE    ZERO                TO  IDX-H2
           PERFORM VARYING     IDX-H1      FROM    1   BY  2
                   UNTIL       IDX-H1      >   100
               MOVE    TBL-MEISYO (IDX IDY)(IDX-H1:2)
                                          TO  WRK-MOJI
               IF      WRK-MOJI           =   SPACE
                   MOVE    100                TO  IDX-H1
               ELSE
                   PERFORM VARYING     IDK      FROM    1   BY  1
                           UNTIL       IDK      >   10
                       IF      WRK-MOJI        =   TBL-SUJI (IDK)
                           ADD     1           TO  IDX-H2
                           IF      IDK         =   10
                               MOVE    "0"     TO  WRK-INPUT (IDX-H2)
                           ELSE
                               MOVE    IDK     TO  IDK1
                               MOVE    IDK1    TO  WRK-INPUT (IDX-H2)
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           MOVE    SRY-SRYCD (IDX-SIN) TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *    施設基準コード（入力値編集）
           PERFORM VARYING IDX-SST     FROM    1   BY  1
                   UNTIL   IDX-SST     >   8
               MOVE    TNS-SSTKIJUNCD (IDX-SST)
                                      TO  WRK-INPUT-SET (IDX-SST)
           END-PERFORM
      *
           MOVE    SPACE                   TO  WRK-INPUTCHI-G
      *    入力値を桁数にあわせて数字変換する
           MOVE    1                   TO  IDX-H1
           PERFORM VARYING IDX-SST     FROM    1   BY  1
                   UNTIL  (IDX-SST         >   4     )   OR
                          (WRK-INPUT-KETA (IDX-SST)  =   ZERO  )
               MOVE    WRK-INPUT-KETA (IDX-SST)    TO  WRK-KETA
               IF      WRK-KETA            >   ZERO
                   MOVE    WRK-INPUT-G (IDX-H1:WRK-KETA)
                                           TO  WRK-INPUTCHI (IDX-SST)
                   COMPUTE IDX-H1          =   IDX-H1
                                           +   WRK-KETA
               END-IF
           END-PERFORM
      *    名称変換
           MOVE    TNS-NAME            TO  WRK-MEISYO
           MOVE    1                   TO  IDX-H1
           PERFORM VARYING IDX-SST     FROM    1   BY  1
                   UNTIL  (IDX-SST         >   4     )   OR
                          (WRK-INPUT-KETA (IDX-SST)  =   ZERO  )
               MOVE    WRK-INPUT-KETA (IDX-SST)    TO  WRK-KETA
               MOVE    WRK-INPUT-ICHI (IDX-SST)    TO  WRK-ICHI
               COMPUTE WRK-KETA2           =   WRK-KETA  *  2
               COMPUTE WRK-ICHI2           =  (WRK-ICHI  *  2 )
                                           -   1
               MOVE    TBL-MEISYO (IDX IDY)(IDX-H1:WRK-KETA2)
                                           TO  WRK-MEISYO
                                                   (WRK-ICHI2:WRK-KETA2)
               COMPUTE IDX-H1          =   IDX-H1
                                       +   WRK-KETA2
           END-PERFORM
      *
           PERFORM 2009121-PTCOM-INS-SEC
      *
           .
       2009122-PTCOM-84-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントコード（８９）編集処理
      *****************************************************************
       2009122-PTCOM-89-SEC        SECTION.
      *
      *    フリーコメントコードとする
           MOVE    "810000001"         TO  SRY-SRYCD (IDX-SIN)
      *
           MOVE    SPACE               TO  WRK-INPUT-G
           MOVE    ZERO                TO  IDX-H2
           MOVE    ZERO                TO  FLG-HENKANERR
           PERFORM VARYING     IDX-H1      FROM    1   BY  2
                   UNTIL      (IDX-H1      >   100 )
                           OR (FLG-HENKANERR   =   1  )
               MOVE    TBL-MEISYO (IDX IDY)(IDX-H1:2)
                                          TO  WRK-MOJI
               IF      WRK-MOJI           =   SPACE
                   MOVE    100                TO  IDX-H1
               ELSE
                   MOVE    ZERO                TO  FLG-HENKAN
                   PERFORM VARYING     IDK      FROM    1   BY  1
                           UNTIL      (IDK      >   10 )
                                   OR (FLG-HENKAN   =   1   )
                       IF      WRK-MOJI        =   TBL-SUJI (IDK)
                           ADD     1           TO  IDX-H2
                           IF      IDK         =   10
                               MOVE    "0"     TO  WRK-INPUT (IDX-H2)
                           ELSE
                               MOVE    IDK     TO  IDK1
                               MOVE    IDK1    TO  WRK-INPUT (IDX-H2)
                           END-IF
                           MOVE    1           TO  FLG-HENKAN
                       END-IF
                   END-PERFORM
      *            全角数値変換エラー
                   IF      FLG-HENKAN          =   ZERO
                       MOVE    1               TO  FLG-HENKANERR
                   END-IF
               END-IF
           END-PERFORM
      *    病名部位変換なし
           IF      FLG-HENKANERR       =   1
               MOVE    TBL-MEISYO (IDX IDY)    TO  WRK-MEISYO
           ELSE
      *
      *        病名部位から部位名称編集
               MOVE    WRK-INPUT-G         TO  WRK-BUI-INPUTCD-G
               MOVE    SPACE               TO  WRK-MEISYO
               PERFORM VARYING IDX-SST     FROM    1   BY  1
                   UNTIL  (IDX-SST         >   12    )   OR
                          (WRK-BUICD (IDX-SST) =   SPACE )
                   PERFORM 20091229-BUICD-HEN-SEC
                   MOVE    WRK-MEISYO          TO  WRK-MEISYO-H
                   MOVE    SPACE               TO  WRK-MEISYO
                   STRING  WRK-MEISYO-H
                           BYO-BYOMEI          DELIMITED   BY  SPACE
                                               INTO      WRK-MEISYO
                   END-STRING
               END-PERFORM
           END-IF
      *
      *    画像診断の１行目を部位変換
           MOVE    ZERO                TO  FLG-BUIOK
           IF      (WRK-SRYKBN         =   "70" )
              AND  (FLG-BUIARI         =   ZERO )
              AND  (FLG-SYUGI          =   ZERO )
              AND  (WRK-MEISYO     NOT =   SPACE)
               PERFORM 2009122-BUIHEN-SEC
           END-IF
      *
           IF      FLG-BUIOK           =   ZERO
               MOVE    SPACE                   TO  WRK-INPUTCHI-G
               PERFORM 2009121-PTCOM-INS-SEC
           END-IF
      *
           .
       2009122-PTCOM-89-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名部位から部位名称取得処理
      *****************************************************************
       20091229-BUICD-HEN-SEC        SECTION.
      *
           INITIALIZE                      BYOMEI-REC
           MOVE    "ZZZ"               TO  BYO-BYOMEICD(1:3)
           MOVE    WRK-BUICD (IDX-SST) TO  BYO-BYOMEICD(4:)
      *
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_byomei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  BYOMEI-REC
                   MOVE    ZERO                TO  FLG-BYOMEI
               ELSE
                   MOVE    1                   TO  FLG-BYOMEI
                   INITIALIZE                  BYOMEI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-BYOMEI
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           MOVE    "tbl_byomei"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       20091229-BUICD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴レコード編集処理
      *****************************************************************
       20092-JYURRK-INS-SEC        SECTION.
      *
           MOVE    IDD                 TO  WRK-SRYDD
      *
           MOVE    ZERO                TO  WRK-JYURRK-EDANUM
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           MOVE    WRK-SRYYMD          TO  JYURRK-SRYYMD
           MOVE    1                   TO  JYURRK-RENNUM
           MOVE    ZERO                TO  JYURRK-DOUJI-RENNUM
           MOVE    1                   TO  JYURRK-KAIKEI-RENNUM
           MOVE    ZERO                TO  JYURRK-DRCD
           MOVE    "9999"              TO  JYURRK-HKNCOMBINUM
      *
           MOVE    ZERO                TO  JYURRK-DENPNUM
           MOVE    ZERO                TO  JYURRK-GRP-DENPNUM
           MOVE    ZERO                TO  JYURRK-GRP-RENNUM
      *
           MOVE    TBL-JYUZAI-SRYKBN-G (IDD)
                                       TO  JYURRK-SRYKBN-G
      *
           MOVE    ZERO                TO  IDX-J2
           PERFORM VARYING     IDX-J   FROM    1   BY  1
                   UNTIL      (IDX-J       >   135 )
                         OR   (TBL-JYUZAI-ZAINUM (IDD IDX-J) =   ZERO )
               ADD     1                   TO  IDX-J2
               IF      IDX-J2              >   15
                   PERFORM 200921-JYURRK-DBINS-SEC
                   MOVE    ZERO            TO  JYURRK-ZAINUM-G
                   INITIALIZE                  JYURRK-ZAINUM-G
                   MOVE    1               TO  IDX-J2
               END-IF
               MOVE    TBL-JYUZAI-ZAINUM (IDD IDX-J)
                                           TO  JYURRK-ZAINUM (IDX-J2)
           END-PERFORM
           IF      IDX-J2              >   ZERO
               PERFORM 200921-JYURRK-DBINS-SEC
           END-IF
           .
       20092-JYURRK-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴レコード編集処理
      *****************************************************************
       200921-JYURRK-DBINS-SEC        SECTION.
      *
           ADD     1                   TO  WRK-JYURRK-EDANUM
           MOVE    WRK-JYURRK-EDANUM   TO  JYURRK-EDANUM
      *
      *
           MOVE    SMCNDATE-YMD        TO  JYURRK-CREYMD
      **   MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
      **   MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
      *
           MOVE    SPA-OPID            TO  JYURRK-OPID
           MOVE    SPA-TERMID          TO  JYURRK-TERMID
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    9               TO  FLG-ERR
               DISPLAY "JYURRK INS ERR:" MCP-RC 
                        ",KEY:" JYURRK-KEY "."
           END-IF
           .
       200921-JYURRK-DBINS-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量変換処理
      *****************************************************************
       290-SURYO-HENKAN-SEC        SECTION.
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-HEN-TEXT        TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF      SNUM-RC             =   ZERO
               MOVE    SNUM-OUTNUM         TO  WRK-HEN-NUM
           ELSE
               MOVE    ZERO                TO  WRK-HEN-NUM
           END-IF
           .
       290-SURYO-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記レコード処理
      *****************************************************************
       2001-SJ-SYORI-SEC        SECTION.
      *
           INITIALIZE                      HZN-SJ-TBL
           MOVE    1                   TO  WRK-MO-CNT
           PERFORM VARYING WRK-MO-ST   FROM    1   BY  1
                   UNTIL ( WRK-MO-ST   >   3 )
               UNSTRING                CSV-REC
                   DELIMITED  BY  ","
                   INTO                HZN-SJ-KOUMOKU(WRK-MO-ST)
                   WITH POINTER        WRK-MO-CNT
               END-UNSTRING
           END-PERFORM
      *
      *    症状詳記レコード処理
           PERFORM 2001-SJ-INS-SYORI-SEC
           .
       2001-SJ-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記レコード処理
      *****************************************************************
       2001-SJ-INS-SYORI-SEC        SECTION.
      *
           INITIALIZE                  WRK-RECHEN-AREA
      *    症状詳記区分
           IF      HZN-SJ-KOUMOKU(2)(1:2)  NOT =   SPACE
               MOVE    HZN-SJ-KOUMOKU(2) (1:2) TO  WRK-HEN-SJKBN
           END-IF
           IF      WRK-HEN-SJKBN       =   "90"
                                       OR  SPACE
               MOVE    "99"                TO  WRK-HEN-SJKBN
           END-IF
      *    症状詳記データ
           MOVE    HZN-SJ-KOUMOKU(3)       TO  WRK-HEN-COMMENT
      *    空白はなし
           IF      WRK-HEN-COMMENT     =   SPACE
               GO      TO      2001-SJ-INS-SYORI-EXT
           END-IF
      *    レセコメント検索
           PERFORM 2120-RECECOM-SEL-SEC
      *
      *    最大連番
           IF      WRK-RENNUM-MAX      =   99
               MOVE    7                   TO  FLG-ERR2
           ELSE
      *        追加
               MOVE    SPACE               TO  RECECOM-REC
               INITIALIZE                  RECECOM-REC
               MOVE    SPA-HOSPNUM         TO  RECECOM-HOSPNUM
               MOVE    SPA-PTID            TO  RECECOM-PTID
               MOVE    SPA-NYUGAIKBN       TO  RECECOM-NYUGAIKBN
               MOVE    "00"                TO  RECECOM-SRYKA
               MOVE    WRK-SRYYM           TO  RECECOM-SRYYM
               MOVE    ZERO                TO  RECECOM-HKNCOMBI
               MOVE    ZERO                TO  RECECOM-SRYDD
               MOVE    WRK-HEN-SJKBN       TO  RECECOM-SJKBN
               ADD     1                   TO  WRK-RENNUM-MAX
               MOVE    WRK-RENNUM-MAX      TO  RECECOM-RENNUM
               MOVE    WRK-HEN-COMMENT     TO  RECECOM-COMMENT
      *
               MOVE    SMCNDATE-YMD        TO  RECECOM-CREYMD
      **       MOVE    SMCNDATE-YMD        TO  RECECOM-UPYMD
      **       MOVE    SMCNDATE-HMS        TO  RECECOM-UPHMS
               MOVE    SPA-OPID            TO  RECECOM-OPID
               MOVE    SPA-TERMID          TO  RECECOM-TERMID
      *
      *
               MOVE    RECECOM-REC         TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "RECECOM INS ERR:" MCP-RC
                           ",KEY:"   RECECOM-KEY    "."
                   MOVE    9               TO  FLG-ERR
               END-IF
           END-IF
      *
           .
       2001-SJ-INS-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトコメント検索処理
      *****************************************************************
       2120-RECECOM-SEL-SEC          SECTION.
      *
           MOVE    SPACE               TO  RECECOM-REC
           INITIALIZE                  RECECOM-REC
           MOVE    SPA-HOSPNUM         TO  RECECOM-HOSPNUM
           MOVE    SPA-PTID            TO  RECECOM-PTID
           MOVE    SPA-NYUGAIKBN       TO  RECECOM-NYUGAIKBN
           MOVE    "00"                TO  RECECOM-SRYKA
           MOVE    WRK-SRYYM           TO  RECECOM-SRYYM
           MOVE    ZERO                TO  RECECOM-HKNCOMBI
           MOVE    ZERO                TO  RECECOM-SRYDD
           MOVE    WRK-HEN-SJKBN       TO  RECECOM-SJKBN
      *
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 920-RECECOM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RECECOM
               INITIALIZE                  RECECOM-REC
           END-IF
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  WRK-RENNUM-MAX
           IF     (FLG-RECECOM         =   ZERO )
               MOVE    RECECOM-RENNUM      TO  WRK-RENNUM-MAX
            END-IF
      *
           .
       2120-RECECOM-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者の剤番号更新処理
      *****************************************************************
       20093-PTINF-UPD-SEC        SECTION.
      *
      *    剤番号
           MOVE    PTINF-MAXZAINUM     TO  WRK-HZN-ZAINUM
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *    最大剤番号
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    8               TO  FLG-ERR
               GO      TO   20093-PTINF-UPD-EXT
           END-IF
           IF      PTINF-MAXZAINUM NOT =   WRK-HZN-ZAINUM
               MOVE    8               TO  FLG-ERR
               GO      TO   20093-PTINF-UPD-EXT
           END-IF
      *
      *    最大剤番号更新
           MOVE    WRK-ZAINUM          TO  PTINF-MAXZAINUM
      *
           MOVE    SMCNDATE-YMD        TO  PTINF-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTINF-UPHMS
      *
           MOVE    SPA-OPID            TO  PTINF-OPID
           MOVE    SPA-TERMID          TO  PTINF-TERMID
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "PTINF UPDTE ERR:"   MCP-RC
                                ",KEY:" PTINF-KEY
               MOVE    9                   TO  FLG-ERR
           END-IF 
           .
       20093-PTINF-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーリスト編集処理
      *****************************************************************
       270-ERRREC-EDIT-SEC         SECTION.
      *
           ADD     1               TO  CNT-ERR
      *
           MOVE    SPACE           TO  ERR-LST-REC
           MOVE    CNT-CSV         TO  ERR-LINENO
           MOVE    WRK-HEN-PTNUM   TO  ERR-PTNUM
           MOVE    WRK-HEN-NAME    TO  ERR-NAME
           MOVE    WRK-HEN-SRYYM   TO  ERR-SRYYM
      *
           EVALUATE    FLG-PTNUM-ERR
           WHEN   1
               MOVE    "患者番号未登録"
                                       TO  ERR-ERRMSG
           WHEN   2
               MOVE    "診療年月エラー"
                                       TO  ERR-ERRMSG
           WHEN   3
               MOVE    "診療年月以降の受診履歴登録済み"
                                       TO  ERR-ERRMSG
           END-EVALUATE
           EVALUATE    FLG-ERR
           WHEN   9
               MOVE    "ＤＢ更新エラー"
                                       TO  ERR-ERRMSG
           WHEN   8
               MOVE    "患者情報エラー"
                                       TO  ERR-ERRMSG
           WHEN   7
               MOVE    "症状詳記の連番上限オーバー"
                                       TO  ERR-ERRMSG
           END-EVALUATE
      *
      *
           .
       270-ERRREC-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーリスト出力処理
      *****************************************************************
       270-ERRLST-OUT-SEC          SECTION.
      *
           MOVE    ERR-LST-REC         TO      ERR-REC
           WRITE   ERR-REC
      *
           .
       270-ERRLST-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力　初期処理
      *****************************************************************
       2710-ERRLST-INIT-SEC        SECTION.
      *
           OPEN    OUTPUT              ERR-FILE
           IF  STS-LST             NOT =   ZERO
               MOVE    1               TO  FLG-CSV
               DISPLAY "*(ORCVTRECEDEN)* ERRFILE OPN STS[" STS-LST "]"
               GO  TO  2710-ERRLST-INIT-EXT
           END-IF
      *
           MOVE    MES-TITLE1      TO  ERR-REC
           WRITE   ERR-REC
           MOVE    MES-TITLE2-L1   TO  ERR-REC
           WRITE   ERR-REC
           MOVE    MES-TITLE2-L3   TO  ERR-REC
           WRITE   ERR-REC
      *
           .
       2710-ERRLST-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック処理
      *****************************************************************
       31012-SEIWA-HEN-SEC         SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "11"            TO  LNK-DAY1-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY1-AREA
           IF      STS-DAY-RC1     =   ZERO
               MOVE    0               TO  FLG-YMD
           ELSE
               MOVE    1               TO  FLG-YMD
           END-IF
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    帳票出力　終了処理
           CLOSE                       CSV-FILE
           CLOSE                       ERR-FILE
      *
      *    DB DISCONNECT
           PERFORM 900-DBDISCONNECT-SEC
      *
      *
           DISPLAY "*(ORCVTRECEDEN)* CSV     /I CNT[" CNT-CSV      "]"
           DISPLAY "*(ORCVTRECEDEN)* CNT-RE  /I CNT[" CNT-RE       "]"
           DISPLAY "*(ORCVTRECEDEN)* OUT     /O CNT[" CNT-OUTRE    "]"
           DISPLAY "*(ORCVTRECEDEN)* ERR     /O CNT[" CNT-ERR      "]"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタＦＥＴＣＨ処理
      *****************************************************************
       900-SYSTEM-NEXT-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSTEM-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号読み込み
      *****************************************************************
       900-PTNUM-READ-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNUM
           MOVE    SPACE           TO  PTNUM-REC
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    SPA-PTNUM       TO  PTNUM-PTNUM
      *
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnum"     TO  MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  PTNUM-REC
                   MOVE    ZERO            TO  FLG-PTNUM
               ELSE
                   INITIALIZE                  PTNUM-REC
                   MOVE    1               TO  FLG-PTNUM
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTNUM
           END-IF
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴読み込み
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
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
      *    レセプトコメント読み込み
      *****************************************************************
       920-RECECOM-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  RECECOM-REC
               MOVE    ZERO            TO  FLG-RECECOM
           ELSE
               INITIALIZE                  RECECOM-REC
               MOVE    1               TO  FLG-RECECOM
           END-IF
      *
           .
       920-RECECOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    WRK-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
      *
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
          .
       910-TENSU-READ-EXT.
           EXIT.
      *!
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
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電データＣＳＶファイル　読込処理
      *****************************************************************
       900-CSV-KENSAKU-SEC         SECTION.
      *
          MOVE    SPACE            TO  CSV-REC
      *
          READ    CSV-FILE
              AT  END
                  MOVE    1            TO  FLG-CSV
              NOT AT  END
                  MOVE    ZERO         TO  FLG-CSV
                  ADD     1            TO  CNT-CSV
      *           ADD     1            TO  CNT-COMMIT
          END-READ
      *
          IF      FLG-CSV              =    ZERO
               PERFORM 9001-INM-SET-SEC
          END-IF
      *
           IF      CNT-CSV (4:3)       =   ZERO
               DISPLAY "***(ORCVTRECEDEN)*** CNT-CSV[" CNT-CSV "]"
           END-IF
      *
          .
       900-CSV-KENSAKU-EXT.
          EXIT.
      *
      *****************************************************************
      *    入力ＣＳＶファイルテーブルセット処理
      *****************************************************************
       9001-INM-SET-SEC                SECTION.
      *
           INITIALIZE                      HZN-IN-TBL
           MOVE    1                   TO  WRK-MO-CNT
           PERFORM VARYING WRK-MO-ST   FROM    1   BY  1
                   UNTIL ( WRK-MO-ST   >   50 )
               UNSTRING                CSV-REC
                   DELIMITED  BY  ","
                   INTO                HZN-IN-KOUMOKU(WRK-MO-ST)
                   WITH POINTER        WRK-MO-CNT
               END-UNSTRING
           END-PERFORM
           .
       9001-INM-SET-EXT.
          EXIT.
      *
      *****************************************************************
      *    パラメータファイル読み込み処理
      *****************************************************************
       900-PARA-READ-SEC           SECTION.
      *
          MOVE    SPACE            TO  PARA-REC
      *
          READ    PARA-FILE
              AT  END
                  MOVE    1            TO  FLG-PARA
              NOT AT  END
                  ADD     1            TO  CNT-PARA
          END-READ
      *
          .
       900-PARA-READ-EXT.
          EXIT.
      *      
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
