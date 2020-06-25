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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBG033.
      *****************************************************************
      *  システム名        :   ＯＲＣＡ
      *  サブシステム名    :   月次帳票
      *  コンポーネント名  :   向精神薬長期処方対象医薬品投与患者リスト
      *  管理者            : 
      *  作成日付    作業者        記述
      *  19/02/07    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *
      *    一覧（縦）
      **   COPY    "HCMSL80.INC".
      *    一覧（横）
           COPY    "HCMSL55.INC".
      *
      *    一時ファイル対応
           COPY    "CPRECEERR.INC".
      *
      *
      *    ＳＴＳ領域
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *
           03  FLG-OK                  PIC 9(01).
           03  FLG-SP                  PIC 9(01).
           03  FLG-ARI                 PIC 9(01).
           03  FLG-DATA-OK             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDX3                    PIC 9(05).
           03  IDY                     PIC 9(05).
           03  IDY2                    PIC 9(05).
           03  IDZ                     PIC 9(05).
           03  IDS                     PIC 9(05).
           03  IDX-TBL                 PIC 9(05).
           03  IDS2                    PIC 9(05).
           03  IDS3                    PIC 9(05).
           03  IDX-DAY                 PIC 9(02).
           03  IDX-AC                  PIC 9(02).
           03  IDD                     PIC 9(04).
           03  IDD2                    PIC 9(04).
      *
           03  IDXM                    PIC 9(05).
           03  IDXM2                   PIC 9(05).
           03  IDW                     PIC 9(05).
      *
           03  SRT-IDX                 PIC 9(03).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
      *****03  RECEERR                 PIC X(100).
           03  WRK-PARA-SRYYM          PIC X(06).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-NUM                 PIC 9(05).
           03  CNT-PAGE                PIC 9(05).
           03  CNT-LINE                PIC 9(02).
           03  CNT-CSV                 PIC 9(05).
      *
           03  CNT-DATA                PIC 9(05).
           03  CNT-OUT                 PIC 9(05).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
           03  SYS-TIME.
               05  SYS-THH             PIC 9(02).
               05  SYS-TMM             PIC 9(02).
               05  SYS-TSS             PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
      *
      *    エラー情報
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).
               05  WRK-SYSDD           PIC 9(02).
           03  WRK-SYSYMDG             PIC X(22).
      *    処理年月
           03  WRK-SRY-YYMM.
               05  WRK-SRY-YY          PIC 9(04).
               05  WRK-SRY-MM          PIC 9(02).
      *    処理年月開始日（処理年月−11月）
           03  WRK-STR-YMD.
               05  WRK-STR-YYMM.
                   07  WRK-STR-YY          PIC 9(04).
                   07  WRK-STR-MM          PIC 9(02).
               05  WRK-STR-DD          PIC 9(02).
           03  WRK-END-YMD.
               05  WRK-END-YYMM.
                   07  WRK-END-YY          PIC 9(04).
                   07  WRK-END-MM          PIC 9(02).
               05  WRK-END-DD          PIC 9(02).
      *
      *    月テーブル
           03  WRK-TUKI-AREA.
               05  WRK-TUKI-TBL            OCCURS   12.
                   07  WRK-TUKI-9          PIC 9(02).
      *
           03  WRK-KEY-AREA.
               05  WRK-KEY-PTID            PIC 9(10).
           03  WRK-PTID                PIC 9(10).
      *
           03  WRK-MAE-PTID            PIC 9(10).
      *
           03  WRK-TNS-NAME            PIC X(80).
           03  WRK-YAKKAKJNCD          PIC X(12).
           03  WRK-TANINAME            PIC X(24).
      *
           03  WRK-SRYCD               PIC X(09).
           03  WRK-SRYYM.
               05  WRK-SRYDD           PIC X(04).
               05  WRK-SRYMM           PIC 9(02).
      *
      *    SORT
           03  WRK-SORT-CD             PIC X(09).
      *    集計
           03  WRK-GOK-KAISU           PIC 9(05).
           03  WRK-TUKIKEI-G.
               05  WRK-TUKIKEI-OCC         OCCURS   3.
                   07  WRK-DAYKEI          PIC 9(03).
                   07  WRK-DAYCNT          PIC 9(02).
           03  WRK-ZZZ                 PIC ZZZ.
      *
      *
           03  WRK-SURYO           PIC 9(05)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(05).
      *
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZZZ.
      *
           03  WRK-SURYO-HG.
               05  WRK-SURYO-H     PIC X(05).
               05  WRK-SURYO-H2    PIC X(06).
      *
      *向精神薬長期処方対象薬剤テーブル
       01  TBL-PSYCHO-AREA.
           03  TBL-PSYCHO-OCC          OCCURS   20000
                                       INDEXED    TBL-IDX.
               05  TBL-PSY-SRYCD              PIC X(09).
               05  TBL-PSY-YUKOSTYMD          PIC X(08).
               05  TBL-PSY-YUKOEDYMD          PIC X(08).
               05  TBL-PSY-TNS-NAME           PIC X(80).
               05  TBL-PSY-YAKKAKJNCD         PIC X(12).
               05  TBL-PSY-TANINAME           PIC X(12).
      *
      *    患者毎の情報
       01  TBL-DATA-AREA.
           03  TBL-DATA-OCC            OCCURS   200.
               05  TBL-SRYCD               PIC X(09).
               05  TBL-SRYKA               PIC X(02).
               05  TBL-HKNCOMBI            PIC X(04).
               05  TBL-SRYKBN              PIC X(02).
               05  TBL-SRYSURYO            PIC 9(05)V9(5).
               05  TBL-KAISU-G.
                   07  TBL-KAISU-OCC       OCCURS   12.
                       09  TBL-KAISU       PIC 9(04)
                                           OCCURS   31.
      *
      *    剤毎
       01  TBL-SRYACT-AREA.
           03  TBL-SRYACT-OCC            OCCURS   5.
               05  TBL-SRY-SRYCD               PIC X(09).
               05  TBL-SRY-SRYSURYO            PIC 9(05)V9(5).
      *
      *
      * 薬剤SORT
       01  SORT-DATA-AREA.
           03  SORT-DATA-OCC            OCCURS   200.
               05  SORT-SRYCD               PIC X(09).
               05  SORT-SRYKA               PIC X(02).
               05  SORT-HKNCOMBI            PIC X(04).
               05  SORT-SRYKBN              PIC X(02).
               05  SORT-SRYSURYO            PIC 9(05)V9(5).
               05  SORT-KAISU-G.
                   07  SORT-KAISU-OCC       OCCURS   12.
                       09  SORT-KAISU       PIC 9(04)
                                           OCCURS   31.
      *
       01  TBL-SRYKA-AREA.
           03  TBL-SRYKA-OCC           OCCURS   40.
               05  TBL-SRYKA-CD            PIC X(02).
               05  TBL-SRYKA-MEI           PIC X(10).
      *
      *
      * 確認リスト(X(158))
       01  LST-PRT-AREA.
           03  LST-PRT-HEAD1.
               05  FILLER              PIC X(05)   VALUE   SPACE.
               05  FILLER              PIC X(50)   VALUE
                   "＃＃　向精神薬長期処方対象医薬品投与患者一覧　＃＃".
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  PRT-STRYM           PIC X(16).
               05  FILLER              PIC X(02)   VALUE   "〜".
               05  PRT-SRYYM           PIC X(16).
               05  FILLER              PIC X(04)   VALUE
                   "分".
               05  FILLER              PIC X(07)   VALUE   SPACE.
               05  PRT-SYSYMD          PIC X(10)   VALUE   SPACE.
               05  PRT-TIME            PIC X(07)   VALUE   SPACE.
               05  PRT-PAGE            PIC ZZZ9.
               05  FILLER              PIC X(02)   VALUE   "頁".
           03  LST-PRT-HEAD2.
               05  FILLER              PIC X(20)   VALUE
                   "患者番号".
               05  FILLER              PIC X(01)   VALUE   SPACE.
               05  FILLER              PIC X(50)   VALUE
                   "氏名".
           03  LST-PRT-HEAD3.
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(12)   VALUE
                   "科".
               05  FILLER              PIC X(10)   VALUE
                   "保険組合せ".
               05  FILLER              PIC X(02)   VALUE   SPACE.
               05  FILLER              PIC X(10)   VALUE
                   "薬効".
               05  FILLER              PIC X(80)   VALUE
                   "薬剤名称".
               05  FILLER              PIC X(10)   VALUE
                   "用量".
      *
           03  LST-PRT-HEAD4.
               05  FILLER              PIC X(09)   VALUE   SPACE.
               05  FILLER              PIC X(06)   VALUE
                   "日・回".
               05  PRT-HEAD4-TBL       OCCURS   12.
                   07  PRT-HEAD4-K1    PIC X(04).
                   07  PRT-HEAD4-TUKI  PIC X(06).
                   07  PRT-HEAD4-F2    PIC X(02).
      *
           03  LST-PRT-BODY-01.
               05  PRT-PTNUM           PIC X(20).
               05  PRT-FIL1            PIC X(01).
               05  PRT-NAME            PIC X(50).
               05  PRT-FIL2            PIC X(01).
      *
           03  LST-PRT-BODY-02.
               05  PRT-FIL20           PIC X(02).
               05  PRT-FIL2K1          PIC X(01).
               05  PRT-RENNUM2         PIC ZZZ.
               05  PRT-FIL2K2          PIC X(01).
               05  PRT-FIL21           PIC X(02).
               05  PRT-SRYKA           PIC X(10).
               05  PRT-FIL22           PIC X(02).
               05  PRT-HKNCOMBI        PIC X(04).
               05  PRT-FIL23           PIC X(02).
               05  PRT-SRYKBN          PIC X(04).
               05  PRT-FIL24           PIC X(02).
               05  PRT-FIL2K3          PIC X(01).
               05  PRT-YAKKCD          PIC X(09).
               05  PRT-FIL2K4          PIC X(02).
               05  PRT-TNS-NAME        PIC X(70).
               05  PRT-SURYO           PIC X(11).
               05  PRT-FIL27           PIC X(01).
               05  PRT-TANINAME        PIC X(04).
               05  PRT-FIL28           PIC X(01).
               05  PRT-SRYCD           PIC X(11).
      *
           03  LST-PRT-BODY-03.
               05  PRT-FIL30           PIC X(02).
               05  PRT-FIL3K1          PIC X(01).
               05  PRT-RENNUM3         PIC ZZZ.
               05  PRT-FIL3K2          PIC X(01).
               05  PRT-FIL33           PIC X(02).
               05  PRT-GOKKAI          PIC ZZZ9.
               05  PRT-FIL34           PIC X(02).
               05  PRT-CNT-TBL         OCCURS   12.
                   07  PRT-FIL32       PIC X(02).
                   07  PRT-CNT-G.
                       09  PRT-CNT     PIC X(03)
                                       OCCURS   3.
                   07  PRT-FIL33       PIC X(01).
      *
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
           03  WRK-HENTIME.
               05  FILLER          PIC X(01)   VALUE   "(".
               05  WRK-THH         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ":".
               05  WRK-TMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ")".
      *    ＣＳＶ用
           03  WRK-CSVHENYMD.
               05  WRK-CYY         PIC 9(04).
               05  FILLER          PIC X(01)   VALUE   "/".
               05  WRK-CMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   "/".
               05  WRK-CDD         PIC 9(02).
      *
       01  MAX-LINE           PIC 9(03)   VALUE   55.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
      *01  PRTKANRI-REC.
      *    COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
      *01  PRTDATA-REC.
      *    COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    向精神薬長期処方成分コードテーブル
           COPY    "CMPSYCHOTROPIC2.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
      *    COPY    "CPORCSGDAY.INC".
      *
      *    ファイル削除パラメタ
      **   COPY    "CPORCSFDEL.INC".
      *
      *    統計ＣＳＶ制御サブ
      *    COPY    "CPORCSTOUKEICSV.INC".
      *
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      ****************************************************************
       PROCEDURE               DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF      FLG-END             =   ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *
           DISPLAY "*** ORCBG033 START ***"
      *
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-PARA
           INITIALIZE                  SPA-AREA
      *
      *    ＤＢオープン処理
           PERFORM 100-DBOPEN-SEC
      *
      *    パラメタセット
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID
                                               LNK-PRTKANRI-PRTNM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               RECEERR
                                               WRK-PARA-SRYYM
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  RECEERR
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    "ORCBG033"          TO  JOB-PGID
           MOVE    "向精神薬長期処方対象医薬品投与患者一覧"
                                       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  WRK-SYSYMD
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "11"            TO  LNK-DAY1-IRAI
           MOVE    WRK-SYSYMD      TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY1-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY1-YMD (2:7)  TO  SPA-SYSYMDW
               MOVE    WRK-SYSYMD          TO  SPA-SYSYMD
           END-IF
      *
      *    処理日
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  PRT-SYSYMD
      *
      *    処理時間
           ACCEPT  SYS-TIME            FROM    TIME
           MOVE    SYS-THH             TO  WRK-THH
           MOVE    SYS-TMM             TO  WRK-TMM
           MOVE    WRK-HENTIME         TO  PRT-TIME
      *
      *    診療日
           IF      WRK-PARA-SRYYM      =   SPACE
      *        未設定はシステム日付
               MOVE    WRK-SYSYMD(1:6)     TO  WRK-SRY-YYMM
           ELSE
               MOVE    WRK-PARA-SRYYM      TO  WRK-SRY-YYMM
           END-IF
      *
           MOVE    WRK-SRY-YYMM        TO  SPA-SRYYMD(1:6)
           MOVE    "01"                TO  SPA-SRYYMD(7:2)
      *    処理日
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    SPA-SRYYMD      TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3(1:16)  TO  PRT-SRYYM
      *
      *
      *    11か月前を開始日
           IF      WRK-SRY-MM          <= 11
               COMPUTE WRK-STR-YY      =   WRK-SRY-YY  -  1
               COMPUTE WRK-STR-MM      =  (12  +  WRK-SRY-MM)
                                           -   11
           ELSE 
               MOVE    WRK-SRY-YY      TO  WRK-STR-YY
               COMPUTE WRK-STR-MM      =   WRK-SRY-MM  -  11
           END-IF
           MOVE    01                  TO  WRK-STR-DD
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-STR-YMD     TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3(1:16)  TO  PRT-STRYM
      *    処理終了年月
           MOVE    WRK-SRY-YYMM        TO  WRK-END-YMD(1:6)
           MOVE    31                  TO  WRK-END-DD
      *
      *    月見出し編集
           PERFORM 1103-TUKI-HEAD-SEC
      *
      *    開始日
           IF      WRK-SRY-YYMM       <   "201903"
      *        H31.3　以前はエラー
               PERFORM 2009-PRINT00-SEC
               MOVE    1                   TO  FLG-END
               GO      TO    110-PARA-HENSYU-EXT
           END-IF
      *
      *    診療科をＴＢＬにセット
           PERFORM 1101-SRYKA-TBLSET-SEC
      *
      *    該当薬剤コードをＴＢＬにセット
           PERFORM 1102-SRYCD-TBLSET-SEC
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    月見出し編集編集処理
      *****************************************************************
       1103-TUKI-HEAD-SEC              SECTION.
      *
           MOVE    WRK-STR-MM          TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   12
               MOVE    IDY                 TO  WRK-TUKI-9 (IDX)
               EVALUATE    IDY
               WHEN    1
                   MOVE    "　１月"        TO  PRT-HEAD4-TUKI(IDX)
               WHEN    2
                   MOVE    "　２月"        TO  PRT-HEAD4-TUKI(IDX)
               WHEN    3
                   MOVE    "　３月"        TO  PRT-HEAD4-TUKI(IDX)
               WHEN    4
                   MOVE    "　４月"        TO  PRT-HEAD4-TUKI(IDX)
               WHEN    5
                   MOVE    "　５月"        TO  PRT-HEAD4-TUKI(IDX)
               WHEN    6
                   MOVE    "　６月"        TO  PRT-HEAD4-TUKI(IDX)
               WHEN    7
                   MOVE    "　７月"        TO  PRT-HEAD4-TUKI(IDX)
               WHEN    8
                   MOVE    "　８月"        TO  PRT-HEAD4-TUKI(IDX)
               WHEN    9
                   MOVE    "　９月"        TO  PRT-HEAD4-TUKI(IDX)
               WHEN    10
                   MOVE    "１０月"        TO  PRT-HEAD4-TUKI(IDX)
               WHEN    11
                   MOVE    "１１月"        TO  PRT-HEAD4-TUKI(IDX)
               WHEN    12
                   MOVE    "１２月"        TO  PRT-HEAD4-TUKI(IDX)
               END-EVALUATE
               MOVE    "｜"                TO  PRT-HEAD4-K1 (IDX)
      *
               IF      IDY                 =   12
                   MOVE    1               TO  IDY
               ELSE
                   ADD     1               TO  IDY
               END-IF
           END-PERFORM
           .
       1103-TUKI-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    診療科  テーブル編集処理
      *****************************************************************
       1101-SRYKA-TBLSET-SEC              SECTION.
      *
           INITIALIZE                  TBL-SRYKA-AREA
           MOVE    ZERO                TO  IDX
      *
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-SRYYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM VARYING IDX
                   FROM    1       BY  1
                   UNTIL      (IDX             >   40    )   OR
                              (FLG-SYSKANRI    =   1     )
               MOVE    SYSKANRI-REC            TO  SYS-1005-REC
               MOVE    SYS-1005-KBNCD (1:2)    TO  TBL-SRYKA-CD (IDX)
               MOVE    SYS-1005-SRYKANAME1     TO  TBL-SRYKA-MEI (IDX)
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       1101-SRYKA-TBLSET-EXT.
           EXIT.
      *****************************************************************
      *    向精神薬長期処方対象薬剤テーブル編集処理
      *****************************************************************
       1102-SRYCD-TBLSET-SEC              SECTION.
      *
           INITIALIZE                  TBL-PSYCHO-AREA
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX2
      *
           PERFORM
                   UNTIL      (IDX     >=   34   )
                           OR (IDX2    >=   20000)
               ADD     1                   TO  IDX
               SET     TBL-PSY2            TO  IDX
               IF     (TBL-PSYCHO2-STYMD (TBL-PSY2)  <=  WRK-STR-YMD)
                  AND (TBL-PSYCHO2-EDYMD (TBL-PSY2)  >=  WRK-END-YMD)
      *
                   MOVE    SPACE               TO  TNS-TENSU-REC
                   MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
      ***          MOVE    WRK-STR-YMD         TO  TNS-YUKOSTYMD
      ***          MOVE    WRK-END-YMD         TO  TNS-YUKOEDYMD
      *H31.4       有効開始日≦終了日　AND 有効終了日≧開始日　を対象
                   MOVE    WRK-END-YMD         TO  TNS-YUKOSTYMD
                   MOVE    WRK-STR-YMD         TO  TNS-YUKOEDYMD
                   STRING  TBL-PSYCHO2-CODE(TBL-PSY2)
                       "%"                 DELIMITED  BY  SPACE
                                           INTO  TNS-YAKKAKJNCD
                   END-STRING
      *
                   MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
                   MOVE    "tbl_tensu"         TO  MCP-TABLE
                   MOVE    "key37"             TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       PERFORM 910-TENSU-READ-SEC
                   ELSE
                       MOVE    1               TO  FLG-TENSU
                   END-IF
                   PERFORM UNTIL      (FLG-TENSU   =   1  )
                                  OR  (IDX2        >=  20000)
                       PERFORM 11021-PSY-TBL-SEC
      *
                       MOVE    "tbl_tensu"         TO  MCP-TABLE
                       MOVE    "key37"             TO  MCP-PATHNAME
                       PERFORM 910-TENSU-READ-SEC
      *
                   END-PERFORM
                   MOVE    "tbl_tensu"         TO  MCP-TABLE
                   MOVE    "key37"             TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
               END-IF
           END-PERFORM
      *
           .
       1102-SRYCD-TBLSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コードテーブル編集処理
      *****************************************************************
       11021-PSY-TBL-SEC                SECTION.
      *
      *    コードで１件
           IF     (IDX2                >   ZERO )
            AND   (TBL-PSY-SRYCD(IDX2)     =   TNS-SRYCD )
               MOVE    TNS-YUKOEDYMD   TO  TBL-PSY-YUKOEDYMD
                                                            (IDX2)
               MOVE    TNS-NAME        TO  TBL-PSY-TNS-NAME
                                                             (IDX2)
               MOVE    TNS-YAKKAKJNCD  TO  TBL-PSY-YAKKAKJNCD
                                                             (IDX2)
           ELSE
               ADD     1               TO  IDX2
               MOVE    TNS-SRYCD       TO  TBL-PSY-SRYCD (IDX2)
               MOVE    TNS-YUKOSTYMD   TO  TBL-PSY-YUKOSTYMD
                                                             (IDX2)
               MOVE    TNS-YUKOEDYMD   TO  TBL-PSY-YUKOEDYMD
                                                             (IDX2)
               MOVE    TNS-NAME        TO  TBL-PSY-TNS-NAME
                                                             (IDX2)
               MOVE    TNS-YAKKAKJNCD  TO  TBL-PSY-YAKKAKJNCD
                                                  (IDX2)
               MOVE    TNS-TANINAME    TO  TBL-PSY-TANINAME (IDX2)
           END-IF
           .
       11021-PSY-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    "2"                 TO  JYURRK-NYUGAIKBN
           MOVE    WRK-STR-YMD         TO  JYURRK-SRYYMD
           MOVE    WRK-END-YMD         TO  JYURRK-UPSRYYMD
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key72"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key72"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           INITIALIZE                      WRK-KEY-AREA
           INITIALIZE                      TBL-SRYACT-AREA
           MOVE    ZERO                TO  IDX-TBL
           MOVE    ZERO                TO  WRK-MAE-PTID
      *
           PERFORM
               UNTIL   (FLG-JYURRK         =   1   )
      *
               IF     (WRK-KEY-PTID        =   JYURRK-PTID   )
                   CONTINUE
               ELSE
      *            患者番号毎の処理
                   PERFORM 2001-MAIN-SYORI-SEC
               END-IF
      *
               MOVE    JYURRK-PTID         TO  WRK-KEY-PTID
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key72"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key72"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *
           IF      CNT-LINE        >   ZERO
               PERFORM 400-PRINT-SEC
           ELSE
      *        対象なし
               IF      CNT-OUT             =   ZERO
                   PERFORM 2009-PRINT00-SEC
               END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号毎の処理
      *****************************************************************
       2001-MAIN-SYORI-SEC        SECTION.
      *
           INITIALIZE                      TBL-DATA-AREA
           MOVE    ZERO                TO  IDX-TBL
           MOVE    ZERO                TO  FLG-DATA-OK
           MOVE    JYURRK-PTID         TO  WRK-PTID
      *
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    "2"                 TO  SRY-NYUGAIKBN
           MOVE    WRK-PTID            TO  SRY-PTID
           MOVE    WRK-STR-YYMM        TO  SRY-SRYYM
           MOVE    WRK-END-YYMM        TO  SRY-CREYMD
           MOVE    "2%"                TO  SRY-SRYKBN
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           PERFORM UNTIL       FLG-SRYACT      =   1
               INITIALIZE                      TBL-SRYACT-AREA
               MOVE    ZERO                TO  IDX2
               PERFORM VARYING     IDS     FROM    1   BY  1
                       UNTIL      (IDS         >   5   )
                             OR   (SRY-SRYCD (IDS) =   SPACE )
                   IF      SRY-SRYCD (IDS)(1:1)    =   "6"
      *                該当薬剤チェック
                       MOVE    SRY-SRYCD (IDS)     TO  WRK-SRYCD
                       PERFORM 20011-SRYCD-CHEK-SEC
      *
                       IF      FLG-OK              =   1
                           ADD     1               TO  IDX2
                           MOVE    SRY-SRYCD (IDS) TO  TBL-SRY-SRYCD
                                                                (IDX2)
                           MOVE    SRY-SRYSURYO(IDS)
                                                   TO  TBL-SRY-SRYSURYO
                                                                (IDX2)
                       END-IF
                   END-IF
               END-PERFORM
      *
               IF      IDX2                >   ZERO
      *            算定回数編集
                   PERFORM 20012-SRYACCT-HEN-SEC
               END-IF
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF    FLG-DATA-OK           =   1
      *        患者毎印刷処理
               PERFORM 20012-PRITN-SYORI-SEC
           END-IF
      *
           .
       2001-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    該当薬剤チェック処理
      *****************************************************************
       20011-SRYCD-CHEK-SEC        SECTION.
      *
           SET     TBL-IDX             TO  1
           SEARCH      TBL-PSYCHO-OCC  VARYING   TBL-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-TNS-NAME
                       MOVE    SPACE           TO  WRK-YAKKAKJNCD
                       MOVE    SPACE           TO  WRK-TANINAME
                       MOVE    ZERO            TO  FLG-OK
                   WHEN    WRK-SRYCD       =   TBL-PSY-SRYCD
                                                           (TBL-IDX)
                       MOVE    TBL-PSY-TNS-NAME (TBL-IDX)
                                                TO  WRK-TNS-NAME
                       MOVE    TBL-PSY-YAKKAKJNCD (TBL-IDX)
                                                TO  WRK-YAKKAKJNCD
                       MOVE    TBL-PSY-TANINAME (TBL-IDX)
                                                TO  WRK-TANINAME
                       MOVE    1                TO  FLG-OK
           END-SEARCH
      *
           .
       20011-SRYCD-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴処理
      *****************************************************************
       20012-SRYACCT-HEN-SEC        SECTION.
      *
      *    剤回数取得
           MOVE    SPACE               TO  SRYACCT-REC
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
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   5   )
                         OR   (TBL-SRY-SRYCD (IDX2) =   SPACE)
                         OR   (FLG-SRYACCT     =   1   )
               MOVE    ZERO                TO  IDX-TBL
               PERFORM VARYING     IDX3    FROM    1   BY  1
                       UNTIL      (IDX3        >   200 )
                              OR  (IDX-TBL     >   ZERO)
      *            コードなど一致
                   IF      (TBL-SRY-SRYCD (IDX2)
                                       =   TBL-SRYCD (IDX3))
                     AND   (SRY-SRYKBN =   TBL-SRYKBN  (IDX3))
                     AND   (SRY-SRYKA  =   TBL-SRYKA   (IDX3))
                     AND   (ACCT-HKNCOMBI
                                       =   TBL-HKNCOMBI(IDX3))
                     AND   (TBL-SRY-SRYSURYO(IDX2)
                                       =   TBL-SRYSURYO(IDX3))
                       MOVE    IDX3            TO  IDX-TBL
                   END-IF
      *
                   IF      TBL-SRYCD (IDX3)    =   SPACE
                       MOVE    TBL-SRY-SRYCD (IDX2)
                                               TO  TBL-SRYCD (IDX3)
                       MOVE    TBL-SRY-SRYSURYO (IDX2)
                                               TO  TBL-SRYSURYO(IDX3)
                       MOVE    SRY-SRYKBN
                                               TO  TBL-SRYKBN(IDX3)
                       MOVE    SRY-SRYKA
                                               TO  TBL-SRYKA(IDX3)
                       MOVE    ACCT-HKNCOMBI
                                               TO  TBL-HKNCOMBI(IDX3)
                       MOVE    IDX3            TO  IDX-TBL
                   END-IF
               END-PERFORM
      *        回数テーブル編集
               IF      IDX-TBL             >   ZERO
                   MOVE    SRY-SRYYM       TO  WRK-SRYYM
                   MOVE    ZERO            TO  IDY
                   PERFORM VARYING     IDY2    FROM    1   BY  1
                           UNTIL       IDY2    >   12
                       IF      WRK-SRYMM       =   WRK-TUKI-9(IDY2)
                           MOVE    IDY2            TO  IDY
                           MOVE    12              TO  IDY2
                       END-IF
                   END-PERFORM
                   IF      IDY             =   ZERO
                       MOVE    WRK-SRYMM   TO  IDY
                   END-IF
      *
                   PERFORM VARYING     IDD     FROM    1   BY  1
                           UNTIL       IDD     >   31
                      ADD     ACCT-DAY (1 IDD) TO  TBL-KAISU
                                                     (IDX-TBL IDY IDD)
                   END-PERFORM
               END-IF
      *
               MOVE    1                   TO  FLG-DATA-OK
           END-PERFORM
      *
           .
       20012-SRYACCT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者毎印刷処理
      *****************************************************************
       20012-PRITN-SYORI-SEC        SECTION.
      *
      *    診療コード毎にSORT
           PERFORM 20021-TBL-SORT-SEC
      *
           PERFORM 200121-PRITN-SYORI-SEC
      *
           .
       20012-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コード毎にＳＯＲＴ処理
      *****************************************************************
       20021-TBL-SORT-SEC        SECTION.
      *
           MOVE    TBL-DATA-AREA       TO  SORT-DATA-AREA
           MOVE    SPACE               TO  TBL-DATA-AREA
           INITIALIZE                      TBL-DATA-AREA
           MOVE    SPACE               TO  WRK-SORT-CD
      *
           MOVE    ZERO                TO  IDS
           PERFORM
                   UNTIL      (IDS         >=  200 )
               MOVE    SPACE               TO  WRK-SORT-CD
               MOVE    ZERO                TO  SRT-IDX
               PERFORM VARYING     IDS2    FROM    1   BY  1
                       UNTIL      (IDS2        >   200  )
                   IF      SORT-SRYCD (IDS2)   NOT =   SPACE
                       IF     WRK-SORT-CD          =   SPACE
                           MOVE    SORT-SRYCD (IDS2)   TO  WRK-SORT-CD
                       END-IF
                       IF     SORT-SRYCD (IDS2)    =   WRK-SORT-CD
                           ADD     1               TO  IDS
                           MOVE    SORT-DATA-OCC (IDS2)
                                              TO  TBL-DATA-OCC (IDS)
                           INITIALIZE             SORT-DATA-OCC(IDS2)
                           MOVE    IDS2           TO  SRT-IDX
                       END-IF
                   END-IF
               END-PERFORM
      *        処理終了
               IF      SRT-IDX         =   ZERO
                   MOVE    200             TO  IDS
               END-IF
           END-PERFORM
      *
           .
       20021-TBL-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       200121-PRITN-SYORI-SEC        SECTION.
      *
      *    明細編集
           MOVE    SPACE               TO  LST-PRT-BODY-01
      *
      *    患者情報取得
           PERFORM 20020-PTNUM-HEN-SEC
           ADD     1                   TO  CNT-OUT
      *
           IF      CNT-LINE            >=  MAX-LINE
               PERFORM 400-PRINT-SEC
               MOVE    ZERO            TO  CNT-LINE
               PERFORM 280-HEAD-HENSYU-SEC
           END-IF
           IF      CNT-LINE            =   ZERO
               PERFORM 280-HEAD-HENSYU-SEC
           END-IF
      *    患者変更
           IF     (CNT-LINE             >   5        )
                ADD     1               TO  CNT-LINE
                MOVE    ALL "-"         TO  HCMSL55-MOJI (CNT-LINE)
           END-IF
           IF      CNT-LINE            >=  MAX-LINE
               PERFORM 400-PRINT-SEC
               MOVE    ZERO            TO  CNT-LINE
               PERFORM 280-HEAD-HENSYU-SEC
           END-IF
           ADD     1                   TO  CNT-LINE
           MOVE    LST-PRT-BODY-01     TO  HCMSL55-MOJI (CNT-LINE)
      *
           MOVE    WRK-PTID            TO  WRK-MAE-PTID
      *
           MOVE    ZERO                TO  CNT-DATA
      *
      *    明細編集（薬剤情報）
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   200  )
                           OR (TBL-SRYCD (IDX)     =   SPACE)
      *        ３行毎に１行空ける
               ADD     1                   TO  CNT-DATA
               IF      CNT-DATA            >   3
                   ADD     1                   TO  CNT-LINE
                   MOVE    1                   TO  CNT-DATA
               END-IF
      *
               MOVE    SPACE               TO  LST-PRT-BODY-02
               PERFORM 20021-BODY02-HEN-SEC
      *
               IF      CNT-LINE           >=  MAX-LINE
                   PERFORM 400-PRINT-SEC
                   MOVE    ZERO            TO  CNT-LINE
                   PERFORM 280-HEAD-HENSYU-SEC
                   MOVE    1                   TO  CNT-DATA
               END-IF
               ADD     1                   TO  CNT-LINE
               MOVE    LST-PRT-BODY-02     TO  HCMSL55-MOJI
                                                         (CNT-LINE)
           END-PERFORM
      *
           IF      CNT-LINE            >= (MAX-LINE - 1 )
               PERFORM 400-PRINT-SEC
               MOVE    ZERO            TO  CNT-LINE
               PERFORM 280-HEAD-HENSYU-SEC
           END-IF
           ADD     2                   TO  CNT-LINE
           MOVE    LST-PRT-HEAD4       TO  HCMSL55-MOJI (CNT-LINE)
           MOVE    ZERO                TO  CNT-DATA
      *
      *    明細編集（算定回数情報）
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   200  )
                           OR (TBL-SRYCD (IDX)     =   SPACE)
      *        ３行毎に１行空ける
               ADD     1                   TO  CNT-DATA
               IF      CNT-DATA            >   3
                   ADD     1                   TO  CNT-LINE
                   MOVE    1                   TO  CNT-DATA
               END-IF
      *
               MOVE    SPACE               TO  LST-PRT-BODY-03
               PERFORM 20021-BODY03-HEN-SEC
      *
               IF      CNT-LINE           >=  MAX-LINE
                   PERFORM 400-PRINT-SEC
                   MOVE    ZERO            TO  CNT-LINE
                   PERFORM 280-HEAD-HENSYU-SEC
                   MOVE    1                   TO  CNT-DATA
               END-IF
               ADD     1                   TO  CNT-LINE
               MOVE    LST-PRT-BODY-03     TO  HCMSL55-MOJI
                                                         (CNT-LINE)
           END-PERFORM
      *
           .
       200121-PRITN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       20020-PTNUM-HEN-SEC        SECTION.
      *
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    WRK-PTID            TO  SPTID-PTID
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF      SPTID-RC            =   ZERO
               MOVE    SPTID-PTNUM         TO  PRT-PTNUM
           END-IF
      *    患者マスター読み込み
           MOVE    SPACE               TO  PTINF-REC
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    WRK-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   1
               MOVE    "＊＊＊＊"          TO  PRT-NAME
           ELSE
               MOVE    PTINF-NAME          TO  PRT-NAME
           END-IF
      *
           .
       20020-PTNUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細編集（薬剤情報）編集処理
      *****************************************************************
       20021-BODY02-HEN-SEC        SECTION.
      *
           MOVE    "("                 TO  PRT-FIL2K1
           MOVE    IDX                 TO  PRT-RENNUM2
           MOVE    ")"                 TO  PRT-FIL2K2
      *    診療科編集
           PERFORM 200201-SRYKA-HEN-SEC
      *
           MOVE    TBL-HKNCOMBI (IDX)  TO  PRT-HKNCOMBI
      *    診療区分
           EVALUATE    TBL-SRYKBN(IDX)
               WHEN    "21"
                   MOVE    "内服"          TO  PRT-SRYKBN
               WHEN    "22"
                   MOVE    "頓服"          TO  PRT-SRYKBN
               WHEN    "23"
                   MOVE    "外用"          TO  PRT-SRYKBN
           END-EVALUATE
      *    名称
           MOVE    TBL-SRYCD (IDX)     TO  WRK-SRYCD
           PERFORM 20011-SRYCD-CHEK-SEC
           MOVE    WRK-TNS-NAME        TO  PRT-TNS-NAME
      *
           MOVE    WRK-YAKKAKJNCD(1:9) TO  PRT-YAKKCD
           MOVE    "("                 TO  PRT-FIL2K3
           MOVE    ")"                 TO  PRT-FIL2K4
      *    数量
           MOVE    TBL-SRYSURYO (IDX)  TO  WRK-SURYO
           PERFORM 10011-SURYO-HEN-SEC
           MOVE    WRK-SURYO-HG        TO  PRT-SURYO
           MOVE    WRK-TANINAME        TO  PRT-TANINAME
      *
      *    薬剤コード
           STRING  "("
                   TBL-SRYCD (IDX)
                   ")"                     DELIMITED  BY  SIZE
                                           INTO  PRT-SRYCD
           END-STRING
           .
       20021-BODY02-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細編集算定回数情報）編集処理
      *****************************************************************
       20021-BODY03-HEN-SEC        SECTION.
      *
           MOVE    "("                 TO  PRT-FIL3K1
           MOVE    IDX                 TO  PRT-RENNUM3
           MOVE    ")"                 TO  PRT-FIL3K2
      *    合計
           MOVE    ZERO                TO  WRK-GOK-KAISU
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   12
               MOVE    ZERO                TO  WRK-TUKIKEI-G
               MOVE    ZERO                TO  IDD2
               PERFORM VARYING     IDD     FROM    1   BY  1
                       UNTIL       IDD     >   31
                  ADD     TBL-KAISU (IDX IDY IDD)
                                           TO  WRK-GOK-KAISU
      *
                  IF      IDD              <=  10
                       MOVE    1               TO  IDD2
                  END-IF
                  IF     (IDD              >   10 )
                     AND (IDD              <=  20 )
                       MOVE    2               TO  IDD2
                  END-IF
                  IF      IDD              >   20
                       MOVE    3               TO  IDD2
                  END-IF
                  IF      TBL-KAISU (IDX IDY IDD) >   ZERO
                       ADD     TBL-KAISU (IDX IDY IDD)
                                               TO  WRK-DAYKEI(IDD2)
                       ADD     1               TO  WRK-DAYCNT(IDD2)
                  END-IF
               END-PERFORM
      *
               PERFORM VARYING     IDD2    FROM    1   BY  1
                       UNTIL       IDD2    >   3
                  MOVE    WRK-DAYKEI(IDD2)
                                           TO  WRK-ZZZ
                  MOVE    WRK-ZZZ          TO  PRT-CNT (IDY IDD2)
      *           期間内に複数算定有
                  IF    ( WRK-DAYCNT (IDD2)    >   1  )
                    AND ( WRK-DAYKEI(IDD2)     <   100)
                       MOVE    "*"             TO  PRT-CNT (IDY IDD2)
                                                               (1:1)
                  END-IF
               END-PERFORM
               MOVE    "｜"            TO  PRT-FIL32 (IDY)
           END-PERFORM
      *
           MOVE    WRK-GOK-KAISU       TO  PRT-GOKKAI
           .
       20021-BODY03-HEN-EXT.
           EXIT.
      *****************************************************************
      *    数量表示用編集処理
      *****************************************************************
       10011-SURYO-HEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    SPACE               TO  WRK-SURYO-HG
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDXM    FROM    5  BY  -1
                   UNTIL       IDXM    <   1
               IF      WRK-SURYO-S2(IDXM:1) NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDXM:1) TO  WRK-SURYO-Z3
                                                             (IDXM:1)
               END-IF
           END-PERFORM
      *
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
           END-IF
      *
      *    右づめ
           MOVE    SPACE               TO  WRK-SURYO-HG
           MOVE    12                  TO  IDXM2
           PERFORM VARYING     IDXM    FROM   11 BY -1
                   UNTIL       IDXM    <   1
               IF      WRK-SURYO-X(IDXM:1) NOT =   SPACE
                   COMPUTE IDXM2       =   IDXM2   -   1
                   MOVE    WRK-SURYO-X(IDXM:1)
                                           TO  WRK-SURYO-HG(IDXM2:1)
               END-IF
           END-PERFORM
           .
       10011-SURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       200201-SRYKA-HEN-SEC        SECTION.
      *
           MOVE    SPACE               TO  PRT-SRYKA
           PERFORM VARYING     IDZ     FROM    1   BY  1
                   UNTIL      (IDZ         >   40  )   OR
                              (TBL-SRYKA-CD (IDZ)  =   SPACE)
               IF      TBL-SRYKA-CD (IDZ)  =   TBL-SRYKA (IDX)
                   MOVE    TBL-SRYKA-MEI (IDZ)     TO  PRT-SRYKA
                   MOVE    40                  TO  IDZ
               END-IF
           END-PERFORM
      *
           IF      PRT-SRYKA           =   SPACE
               MOVE    TBL-SRYKA (IDX)     TO  PRT-SRYKA
           END-IF
           .
       200201-SRYKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    見出し編集処理
      *****************************************************************
       280-HEAD-HENSYU-SEC        SECTION.
      *
           MOVE    SPACE               TO  HCMSL55
           ADD     1                   TO  CNT-PAGE
           MOVE    CNT-PAGE            TO  PRT-PAGE
      *
           MOVE    LST-PRT-HEAD1       TO  HCMSL55-MOJI (1)
           MOVE    LST-PRT-HEAD2       TO  HCMSL55-MOJI (3)
           MOVE    LST-PRT-HEAD3       TO  HCMSL55-MOJI (4)
           MOVE    ALL "-"             TO  HCMSL55-MOJI (5)
      *
           MOVE    5                   TO  CNT-LINE
      *
           .
       280-HEAD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象なし印刷処理
      *****************************************************************
       2009-PRINT00-SEC          SECTION.
      *
           PERFORM 280-HEAD-HENSYU-SEC
      *
           ADD     5               TO  CNT-LINE
           MOVE    "　　　　　　＝＝＝＝　　対象患者なし　　＝＝＝＝"
                                   TO  HCMSL55-MOJI (CNT-LINE)
      *
           PERFORM 400-PRINT-SEC
           .
       2009-PRINT00-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       400-PRINT-SEC          SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"                TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM
                                        TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                        TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                        TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM
                                        TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD
                                        TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                        TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                        TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                        TO  SPRT-PRIORITY
           MOVE    "HCMSL55.red"        TO  SPRT-PRTID
           MOVE    "向精神薬長期処方対象患者一覧"
                                        TO  SPRT-TITLE
           MOVE    HCMSL55              TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID
                                        TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                        TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                        TO  SPRT-PRTNM
           MOVE    "1"                  TO  SPRT-SITEKBN
           CALL    "ORCSPRT"            USING
                                        ORCSPRTAREA
                                        SPA-AREA
           IF      SPRT-RETURN          =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                        TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       400-PRINT-EXIT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       800-SEIWA-HEN-SEC             SECTION.
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
       800-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT                  RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
               MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
               MOVE    WRK-RECEERR         TO  JOB-YOBI
               MOVE    "9999"              TO  JOB-ERRCD
               CALL    "ORCSJOB"               USING
                                               ORCSJOBKANRIAREA
                                               JOBKANRI-REC
                                               SPA-AREA
           END-IF
      *
           MOVE    1               TO  FLG-END
                                       FLG-ERR
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      CNT-PAGE        =   ZERO 
               MOVE    "処理対象のデータがありませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBG033 END ***"
      *
      *    ステップ管理終了処理
           MOVE    "STE"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    CNT-PAGE            TO  JOB-UPDCNT
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
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
      *    診療行為マスター読込
      *****************************************************************
       930-SRYACT-READ-SEC         SECTION.
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
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
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
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
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               MOVE    ZERO                TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込処理
      *****************************************************************
       910-TENSU-KEYREAD-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-TENSU-READ-SEC
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       910-TENSU-KEYREAD-EXT.
           EXIT.
      *
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
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
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
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       100-DBOPEN-EXT.
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
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
