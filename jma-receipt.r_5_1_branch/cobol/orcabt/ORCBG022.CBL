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
       PROGRAM-ID.             ORCBG022.
      *****************************************************************
      *  システム名        :   ＯＲＣＡ
      *  サブシステム名    :   月次帳票
      *  コンポーネント名  :   向精神薬投与患者リスト
      *  管理者            : 
      *  作成日付    作業者        記述
      *  14/06/XX    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      * 04.08.00     NACL-多々納   14/07/17  一時ファイル対応
      * 04.08.00     NACL-多々納   16/03/XX  平成２８年４月改正対応
      * 04.08.00     NACL-多々納   16/07/XX  単月対象・患者数集計対応
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
       WORKING-STORAGE             SECTION.
      *
      *
      *    一覧（縦）
           COPY    "HCMSL80.INC".
      *    一覧（横）
      **   COPY    "HCMSL55.INC".
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
           03  FLG-ARI                 PIC 9(01).
      *    対象有
           03  FLG-CNT-ARI             PIC 9(01).
           03  FLG-CNT-TAZAI           PIC 9(01).
           03  FLG-CNT-K1              PIC 9(01).
           03  FLG-CNT-K2              PIC 9(01).
           03  FLG-CNT-K3              PIC 9(01).
           03  FLG-CNT-K4              PIC 9(01).
      *H28.4
      *    うつ薬あり
           03  FLG-CNT-A3              PIC 9(01).
      *    抗精神薬あり
           03  FLG-CNT-A4              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(05).
           03  IDY                     PIC 9(05).
           03  IDZ                     PIC 9(05).
           03  IDS                     PIC 9(05).
           03  IDX-TBL                 PIC 9(05).
           03  IDS2                    PIC 9(05).
           03  IDS3                    PIC 9(05).
           03  IDX-DAY                 PIC 9(02).
           03  IDX-AC                  PIC 9(02).
      *
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
      *H28.7
           03  WRK-PARA-CSVKBN         PIC X(01).
           03  WRK-PARA-SYORIKBN       PIC X(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-NUM                 PIC 9(05).
           03  CNT-PAGE                PIC 9(05).
           03  CNT-LINE                PIC 9(02).
           03  CNT-CSV                 PIC 9(05).
      *
           03  CNT-OUT-1               PIC 9(05).
           03  CNT-OUT-2               PIC 9(05).
           03  CNT-OUT-3-1             PIC 9(05).
           03  CNT-OUT-3-2             PIC 9(05).
           03  CNT-OUT-3-3             PIC 9(05).
           03  CNT-OUT-3-4             PIC 9(05).
           03  CNT-OUT-3-8             PIC 9(05).
      *
           03  CNT-OUT-4               PIC 9(05).
           03  CNT-OUT-4-3             PIC 9(05).
           03  CNT-OUT-4-4             PIC 9(05).
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
      *    処理年月開始日（処理年月−３月）
           03  WRK-STR-YMD.
               05  WRK-STR-YY          PIC 9(04).
               05  WRK-STR-MM          PIC 9(02).
               05  WRK-STR-DD          PIC 9(02).
           03  WRK-END-YMD.
               05  WRK-END-YY          PIC 9(04).
               05  WRK-END-MM          PIC 9(02).
               05  WRK-END-DD          PIC 9(02).
      *
           03  WRK-KEY-AREA.
               05  WRK-KEY-PTID            PIC 9(10).
               05  WRK-KEY-SRYYMD          PIC X(08).
               05  WRK-KEY-DENPNUM         PIC 9(07).
           03  WRK-ZAIKAISU            PIC 9(03).
      *
           03  WRK-MAE-PTID            PIC 9(10).
           03  SORT-KEY-GROUP          PIC X(07).
           03  SORT-KEY-FFECT          PIC X(01).
      *
      *
       01  TBL-SRY-AREA.
           03  TBL-PTID                PIC 9(10).
           03  TBL-SRYYMD              PIC X(08).
           03  TBL-SRYKA               PIC X(02).
           03  TBL-SRYKAMEI            PIC X(10).
           03  TBL-HKNCOMBI            PIC X(04).
           03  TBL-JYURRK-RENNUM       PIC 9(01).
           03  TBL-MEISAI-G.
               05  TBL-SRY-OCCS            OCCURS   50.
                   07  TBL-SRY-SRYSYUKBN       PIC X(03).
                   07  TBL-ZAIKAISU            PIC 9(03).
                   07  TBL-SRY-SRYCD           PIC X(09).
                   07  TBL-YAKKAKJNCD          PIC X(12).
                   07  TBL-NAME                PIC X(80).
      *            薬剤の効能区分
                   07  TBL-FFECT-KBN           PIC 9(01).
      *            種類区分
                   07  TBL-GROUP-KBN           PIC X(07).
                   07  TBL-RINJI-KBN           PIC X(01).
      *
      * 薬剤SORT
       01  SORT-TBL-AREA.
           03  SORT-SRY-OCCS            OCCURS   50.
               05  SORT-SRY-SRYSYUKBN       PIC X(03).
               05  SORT-ZAIKAISU            PIC 9(03).
               05  SORT-SRY-SRYCD           PIC X(09).
               05  SORT-YAKKAKJNCD          PIC X(12).
               05  SORT-NAME                PIC X(80).
               05  SORT-FFECT-KBN           PIC 9(01).
               05  SORT-GROUP-KBN           PIC X(07).
               05  SORT-RINJI-KBN           PIC X(01).
      *
       01  TBL-SRYKA-AREA.
           03  TBL-SRYKA-OCC           OCCURS   40.
               05  TBL-SRYKA-CD            PIC X(02).
               05  TBL-SRYKA-MEI           PIC X(10).
      *
      *
      * 確認リスト
       01  LST-PRT-AREA.
           03  LST-PRT-HEAD1.
               05  FILLER              PIC X(02)   VALUE   SPACE.
               05  FILLER              PIC X(40)   VALUE
                   "＃＃　向精神薬投与患者一覧　＃＃".
     ******    05  FILLER              PIC X(16)   VALUE   SPACE.
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
               05  FILLER              PIC X(15)   VALUE   SPACE.
               05  FILLER              PIC X(12)   VALUE
                   "診療日".
               05  FILLER              PIC X(12)   VALUE
                   "科".
               05  FILLER              PIC X(10)   VALUE
                   "保険組合せ".
      *
           03  LST-PRT-BODY-01.
               05  PRT-PTNUM           PIC X(20).
               05  PRT-FIL1            PIC X(01).
               05  PRT-NAME            PIC X(50).
               05  PRT-FIL2            PIC X(01).
      *
           03  LST-PRT-BODY-02.
               05  PRT-FIL20           PIC X(15).
               05  PRT-SRYYMD          PIC X(09).
               05  PRT-FIL21           PIC X(02).
               05  PRT-SRYKA           PIC X(10).
               05  PRT-FIL22           PIC X(01).
               05  PRT-HKNCOMBI        PIC X(04).
               05  PRT-FIL23           PIC X(01).
               05  PRT-SRYKBN          PIC X(06).
               05  PRT-FIL24           PIC X(01).
               05  PRT-KBN             PIC X(06).
               05  PRT-FIL25           PIC X(01).
               05  PRT-YAKKCD          PIC X(09).
               05  PRT-FIL26           PIC X(01).
               05  PRT-TNS-NAME        PIC X(44).
      *
           03  LST-PRT-BODY-03.
               05  PRT-FIL31           PIC X(10)   VALUE   SPACE.
               05  PRT-ERRMSG          PIC X(12).
               05  PRT-FIL32           PIC X(08)   VALUE   SPACE.
               05  FILLER              PIC X(10)   VALUE   "種類数".
               05  FILLER              PIC X(10)   VALUE   "抗不安薬：".
               05  PRT-CNT1            PIC Z9.
               05  FILLER              PIC X(04)   VALUE   SPACE.
               05  FILLER              PIC X(08)   VALUE   "睡眠薬：".
               05  PRT-CNT2            PIC Z9.
               05  FILLER              PIC X(04)   VALUE   SPACE.
               05  FILLER              PIC X(10)   VALUE   "抗うつ薬：".
               05  PRT-CNT3            PIC Z9.
               05  FILLER              PIC X(04)   VALUE   SPACE.
               05  FILLER              PIC X(12)   VALUE
                                                   "抗精神病薬：".
               05  PRT-CNT4            PIC Z9.
               05  FILLER              PIC X(02)   VALUE   SPACE.
      *
           03  LST-PRT-FOOT-01.
               05  FILLER              PIC X(02)   VALUE   SPACE.
               05  FILLER              PIC X(16)   VALUE
                                                   "対象患者数(1)：".
               05  PRT-F-CNT1          PIC ZZZ9.
               05  FILLER              PIC X(04)   VALUE   "名".
      *
           03  LST-PRT-FOOT-02.
               05  FILLER              PIC X(04)   VALUE   SPACE.
               05  FILLER              PIC X(32)   VALUE
                                   "抗うつ薬・抗精神病薬患者数(2)：".
               05  PRT-F-CNT4          PIC ZZZ9.
               05  FILLER              PIC X(06)   VALUE   "名".
               05  FILLER              PIC X(15)   VALUE
                                           "［抗うつ薬(3)：".
               05  PRT-F-CNT43         PIC ZZZ9.
               05  FILLER              PIC X(04)   VALUE   "名、".
               05  FILLER              PIC X(15)   VALUE
                                           "抗精神病薬(4)：".
               05  PRT-F-CNT44         PIC ZZZ9.
               05  FILLER              PIC X(04)   VALUE   "名］".
      *
           03  LST-PRT-FOOT-03.
               05  FILLER              PIC X(04)   VALUE   SPACE.
               05  FILLER              PIC X(22)   VALUE
                                           "多剤投与薬患者数(5)：".
               05  PRT-F-CNT2          PIC ZZZ9.
               05  FILLER              PIC X(04)   VALUE   "名".
               05  FILLER              PIC X(16)   VALUE
                                           "【 抗不安薬(6)：".
               05  PRT-F-CNT31         PIC ZZZ9.
               05  FILLER              PIC X(04)   VALUE   "名、".
               05  FILLER              PIC X(11)   VALUE
                                           "睡眠薬(7)：".
               05  PRT-F-CNT32         PIC ZZZ9.
               05  FILLER              PIC X(04)   VALUE   "名、".
               05  FILLER              PIC X(25)   VALUE
                                       "抗うつ薬・抗精神病薬(8)：".
               05  PRT-F-CNT38         PIC ZZZ9.
               05  FILLER              PIC X(02)   VALUE   "名".
      *
           03  LST-PRT-FOOT-04.
               05  FILLER              PIC X(04)   VALUE   SPACE.
               05  FILLER              PIC X(22)   VALUE   SPACE.
               05  FILLER              PIC X(09)   VALUE   SPACE.
               05  FILLER              PIC X(15)   VALUE
                                           "［抗うつ薬(9)：".
               05  PRT-F-CNT33         PIC ZZZ9.
               05  FILLER              PIC X(04)   VALUE   "名、".
               05  FILLER              PIC X(16)   VALUE
                                           "抗精神病薬(10)：".
               05  PRT-F-CNT34         PIC ZZZ9.
               05  FILLER              PIC X(06)   VALUE   "名］】".
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
      *
       01  MAX-LINE           PIC 9(03)   VALUE   80.
      *
      *
      *    ＣＳＶデータ編集用
       01  WRK-CSV-AREA.
           03  WRK-IN-DATA             PIC X(2000).
           03  WRK-OUT-REC             PIC X(2000).
           03  WRK-CSV-REC             PIC X(2000).
           03  WRK-REC-LENGTH          PIC 9(05).
           03  WRK-IN-MAX              PIC 9(04).
           03  WRK-END                 PIC 9(01).
      *
           03  WRK-TOUKEIERR           PIC X(200).
      *
      *    ＣＳＶ出力先
       01  WRK-TO-DATA-AREA.
           03  WRK-TO-DATA.
               05  WRK-TO-DATA-HOSPNUM PIC 9(02).
               05  FILLER              PIC X(20)   VALUE
                                       "kouseisinyaku_touyo_".
               05  WRK-TO-DATA-SRYYM   PIC X(06).
               05  FILLER              PIC X(04)   VALUE   ".csv".
      *
       01  WRK-CONS-AREA.
      *    コンマ
           03  WRK-KUGIRI          PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO          PIC X(01)   VALUE   X"0D".
      *
       01  CSV-HEAD-01.
           03  FILLER              PIC X(08)   VALUE   "患者番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "患者氏名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "診療日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "診療科名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "保険組合せ番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "受診履歴連番".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "向精神薬種類".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "一般名グループ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "臨時投与".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "医薬品コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "薬剤名".
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
      *    向精神薬多剤投与チェックパラメタ
           COPY    "CPORCSPSYCHO.INC".
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
           COPY    "CPORCSTOUKEICSV.INC".
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
           PERFORM 200-MAIN-SEC
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
                                               WRK-PARA-CSVKBN
                                               WRK-PARA-SYORIKBN
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
           MOVE    "ORCBG022"          TO  JOB-PGID
           MOVE    "向精神薬投与患者一覧"
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
      *        未設定はシステム年の６月
               MOVE    WRK-SYSYMD(1:4)     TO  WRK-SRY-YY
               MOVE    06                  TO  WRK-SRY-MM
               IF      WRK-SRY-YYMM        >   WRK-SYSYMD(1:6)
      *            未来日なら前年の６月
                    COMPUTE WRK-SRY-YY     =   WRK-SRY-YY   -   1
               END-IF
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
      *H28.4
      *    開始日
           IF      WRK-SRY-YYMM        >=  "201606"
      *        H28.6 からは前３カ月分
               IF      WRK-SRY-MM          <= 2
                   COMPUTE WRK-STR-YY      =   WRK-SRY-YY  -  1
                   COMPUTE WRK-STR-MM      =  (12  +  WRK-SRY-MM)
                                           -   2
               ELSE 
                   MOVE    WRK-SRY-YY      TO  WRK-STR-YY
                   COMPUTE WRK-STR-MM      =   WRK-SRY-MM  -  2
               END-IF
      *H28.7
      *        処理区分判定
               IF       WRK-PARA-SYORIKBN      =   "1"
      *            処理月分のみ
                   MOVE    WRK-SRY-YY      TO  WRK-STR-YY
                   MOVE    WRK-SRY-MM      TO  WRK-STR-MM
               END-IF
           ELSE
               MOVE    WRK-SRY-YY      TO  WRK-STR-YY
               MOVE    WRK-SRY-MM      TO  WRK-STR-MM
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
      *    診療科をＴＢＬにセット
           PERFORM 1101-SRYKA-TBLSET-SEC
      *    ＣＳＶ
           MOVE    SPA-HOSPNUM         TO  WRK-TO-DATA-HOSPNUM
           MOVE    WRK-SRY-YYMM        TO  WRK-TO-DATA-SRYYM
      *
           .
       110-PARA-HENSYU-EXT.
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
      *
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    "2"                 TO  JYURRK-NYUGAIKBN
      *    STRING  WRK-SRY-YYMM
      *            "%"                 DELIMITED  BY  SIZE
      *                INTO                JYURRK-SRYYMD
      *    END-STRING
      *H28.4
           MOVE    WRK-STR-YMD         TO  JYURRK-SRYYMD
           MOVE    WRK-END-YMD         TO  JYURRK-UPSRYYMD
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
     ***** MOVE    "key57"             TO  MCP-PATHNAME
           MOVE    "key64"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key64"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           INITIALIZE                      WRK-KEY-AREA
           INITIALIZE                      TBL-SRY-AREA
           MOVE    ZERO                TO  IDX-TBL
           MOVE    ZERO                TO  WRK-MAE-PTID
      *
      *
           PERFORM
               UNTIL   (FLG-JYURRK         =   1   )
      *
               IF     (WRK-KEY-PTID        =   JYURRK-PTID   )
                 AND  (WRK-KEY-SRYYMD      =   JYURRK-SRYYMD )
                 AND  (WRK-KEY-DENPNUM     =   JYURRK-DENPNUM)
                   CONTINUE
               ELSE
                   IF    IDX-TBL           >   ZERO
      *                伝票番号毎処理
                       PERFORM 2002-HENSYU-SYORI-SEC
                   END-IF
                   INITIALIZE                      TBL-SRY-AREA
                   MOVE    ZERO                TO  IDX-TBL
                   MOVE    JYURRK-PTID         TO  WRK-KEY-PTID
                   MOVE    JYURRK-SRYYMD       TO  WRK-KEY-SRYYMD
                   MOVE    JYURRK-DENPNUM      TO  WRK-KEY-DENPNUM
               END-IF
      *
               PERFORM 2001-JYURRK-HEN-SEC
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key64"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key64"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF    IDX-TBL           >   ZERO
      *        伝票番号毎処理
               PERFORM 2002-HENSYU-SYORI-SEC
           END-IF
      *        件数集計
           IF      FLG-CNT-ARI         =   1
               PERFORM 20029-CNT-SYORI-SEC
           END-IF
      *    合計
           IF      CNT-OUT-1       >   ZERO
               PERFORM 2004-GOUKEI-HENSYU-SEC
           END-IF
      *
           IF      CNT-LINE        >   ZERO
               PERFORM 400-PRINT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴処理
      *****************************************************************
       2001-JYURRK-HEN-SEC        SECTION.
      *
           MOVE    JYURRK-SRYYMD(7:2)  TO  IDX-DAY
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   15  )
                          OR  (JYURRK-ZAINUM (IDX) =   ZERO )
               MOVE    SPACE               TO  SRYACT-REC
               INITIALIZE                  SRYACT-REC
               MOVE    JYURRK-HOSPNUM      TO  SRY-HOSPNUM
               MOVE    JYURRK-NYUGAIKBN    TO  SRY-NYUGAIKBN
               MOVE    JYURRK-PTID         TO  SRY-PTID
               MOVE    JYURRK-SRYKA        TO  SRY-SRYKA
               MOVE    JYURRK-SRYYMD(1:6)  TO  SRY-SRYYM
               MOVE    JYURRK-ZAINUM (IDX) TO  SRY-ZAINUM
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
      *        投薬以外対象外
               IF      (SRY-SRYKBN         =   "21"
                                           OR  "22"
                                           OR  "23" )
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-SRYACT
               END-IF
      *
               PERFORM
                       UNTIL   FLG-SRYACT      =   1
      *
      *
                   PERFORM 20011-SRYACCT-CHK-SEC
      *            薬評・減点対象外
                   IF     (ACCT-ZAIKBN         =   ZERO )
                     AND  (WRK-ZAIKAISU        >   ZERO )
                       PERFORM 20012-SRYACT-SYORI-SEC
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
           END-PERFORM
      *
           .
       2001-JYURRK-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療会計チェック処理
      *****************************************************************
       20011-SRYACCT-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-ZAIKAISU
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
           IF      FLG-SRYACCT         =   ZERO
      *        連番＋１
               COMPUTE IDX-AC          =   JYURRK-RENNUM   +  1
               MOVE    ACCT-DAY(IDX-AC IDX-DAY)
                                           TO  WRK-ZAIKAISU
           END-IF
           .
       20011-SRYACCT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為明細編集処理
      *****************************************************************
       20012-SRYACT-SYORI-SEC           SECTION.
      *
           IF      IDX-TBL             =   ZERO
               MOVE    JYURRK-SRYYMD       TO  TBL-SRYYMD
               MOVE    JYURRK-SRYKA        TO  TBL-SRYKA
               MOVE    JYURRK-PTID         TO  TBL-PTID
               MOVE    JYURRK-HKNCOMBINUM  TO  TBL-HKNCOMBI
               MOVE    JYURRK-RENNUM       TO  TBL-JYURRK-RENNUM
           END-IF
      *
           PERFORM VARYING     IDS     FROM    1   BY  1
                   UNTIL      (IDS         >   5   )
                         OR   (SRY-SRYCD (IDS) =   SPACE )
                         OR   (IDX-TBL         >=  50    )
               IF      SRY-SRYCD (IDS)(1:1)    =   "6"
                   ADD     1                   TO  IDX-TBL
                   MOVE    SRY-SRYCD (IDS)     TO  TBL-SRY-SRYCD
                                                         (IDX-TBL)
                   MOVE    SRY-SRYSYUKBN       TO  TBL-SRY-SRYSYUKBN
                                                         (IDX-TBL)
                   MOVE    WRK-ZAIKAISU        TO  TBL-ZAIKAISU
                                                         (IDX-TBL)
      *            外用薬
                   IF      ACCT-SRYKBN          =   "23"
                       MOVE    SRY-SRYKAISU (IDS)  TO  TBL-ZAIKAISU
                                                         (IDX-TBL)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       20012-SRYACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    投薬内容チェック処理
      *****************************************************************
       2002-HENSYU-SYORI-SEC        SECTION.
      *
      *    向精神薬多剤投与チェック
           MOVE    ZERO                TO  IDS3
           INITIALIZE                  ORCSPSYCHOAREA
           PERFORM VARYING     IDS     FROM    1   BY  1
                   UNTIL      (IDS         >   50  )   OR
                              (TBL-SRY-SRYCD (IDS) =   SPACE )
               MOVE    TBL-SRY-SRYCD (IDS) TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
      *H26.12
      *        減点は対象外
               IF     (TNS-TENSIKIBETU     =   7     )
                 OR   (TNS-YAKKAKJNCD      =   SPACE )
                   CONTINUE
               ELSE
      *
               ADD     1                   TO  IDS3
      *
               MOVE    TBL-SRY-SRYCD (IDS) TO  ORCSPSYCHO-SRYCD (IDS3)
               MOVE    TNS-YAKKAKJNCD      TO  ORCSPSYCHO-YAKKAKJNCD
                                                                 (IDS3)
      *
               MOVE    TBL-SRY-SRYSYUKBN(IDS)
                                           TO  ORCSPSYCHO-SRYSYUKBN
                                                                 (IDS3)
               MOVE    TBL-ZAIKAISU (IDS)
                                           TO  ORCSPSYCHO-KAISU
                                                                 (IDS3)
      *
               MOVE    TNS-NAME            TO  TBL-NAME      (IDS3)
               MOVE    TNS-YAKKAKJNCD      TO  TBL-YAKKAKJNCD(IDS3)
      *
               END-IF
           END-PERFORM
      *
           MOVE    WRK-KEY-SRYYMD      TO  ORCSPSYCHO-SRYYMD
           CALL    "ORCSPSYCHO"    USING   ORCSPSYCHOAREA
                                           SPA-AREA
      *
      *    向精神薬投与対象
           IF      ORCSPSYCHO-FFECT-FLG    =   1
      *
      *        種類毎にＳＯＲＴ
               PERFORM 20021-TBL-SORT-SEC
      *
               PERFORM 20020-PRITN-SYORI-SEC
           END-IF
      *
           .
       2002-HENSYU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    種類毎のＳＯＲＴ処理
      *****************************************************************
       20021-TBL-SORT-SEC        SECTION.
      *
           PERFORM VARYING     IDS     FROM    1   BY  1
                   UNTIL      (IDS         >   50  )   OR
                              (TBL-SRY-SRYCD (IDS) =   SPACE )
      *        薬剤の効能区分
               MOVE    ORCSPSYCHO-FFECT-KBN (IDS)
                                           TO  TBL-FFECT-KBN (IDS)
      *        種類区分
               MOVE    ORCSPSYCHO-GROUP-KBN (IDS)
                                           TO  TBL-GROUP-KBN (IDS)
      *        臨時投薬区分
               MOVE    ORCSPSYCHO-RINJI-KBN (IDS)
                                           TO  TBL-RINJI-KBN (IDS)
           END-PERFORM
      *
           MOVE    TBL-MEISAI-G        TO  SORT-TBL-AREA
           MOVE    SPACE               TO  TBL-MEISAI-G
           INITIALIZE                      TBL-MEISAI-G
           PERFORM VARYING     IDS     FROM    1   BY  1
                   UNTIL      (IDS         >   50  )
               MOVE    HIGH-VALUE          TO  SORT-KEY-GROUP
               MOVE    ZERO                TO  SORT-KEY-FFECT
               MOVE    ZERO                TO  SRT-IDX
               PERFORM VARYING     IDS2    FROM    1   BY  1
                       UNTIL      (IDS2        >   50  )
                   IF      SORT-SRY-SRYCD (IDS2)    NOT =   SPACE
                       IF     SORT-KEY-GROUP       =   HIGH-VALUE
                           MOVE    SORT-GROUP-KBN (IDS2)
                                                   TO  SORT-KEY-GROUP
                           MOVE    SORT-FFECT-KBN (IDS2)
                                                   TO  SORT-KEY-FFECT
                           MOVE    IDS2            TO  SRT-IDX
                       END-IF
                       IF      SORT-FFECT-KBN (IDS2)
                                                   <  SORT-KEY-FFECT
                           MOVE    SORT-GROUP-KBN (IDS2)
                                                   TO  SORT-KEY-GROUP
                           MOVE    SORT-FFECT-KBN (IDS2)
                                                   TO  SORT-KEY-FFECT
                           MOVE    IDS2            TO  SRT-IDX
                       END-IF
                       IF     (SORT-FFECT-KBN(IDS2)   =  SORT-KEY-FFECT)
                       AND    (SORT-GROUP-KBN(IDS2)   <  SORT-KEY-GROUP)
                           MOVE    SORT-GROUP-KBN (IDS2)
                                                   TO  SORT-KEY-GROUP
                           MOVE    IDS2            TO  SRT-IDX
                       END-IF
                   END-IF
               END-PERFORM
               IF      SRT-IDX            >   ZERO
                   MOVE    SORT-SRY-OCCS (SRT-IDX)
                                              TO  TBL-SRY-OCCS (IDS)
                   INITIALIZE                 SORT-SRY-OCCS (SRT-IDX)
               ELSE
                   MOVE    50                 TO  IDS
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
       20020-PRITN-SYORI-SEC        SECTION.
      *
      *    明細編集
           MOVE    SPACE               TO  LST-PRT-BODY-01
      *    患者変更なし
           IF      WRK-MAE-PTID    NOT =   TBL-PTID
      *        件数集計
               PERFORM 20029-CNT-SYORI-SEC
      *        患者情報取得
               PERFORM 20020-PTNUM-HEN-SEC
           END-IF
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
           IF     (WRK-MAE-PTID     NOT =   TBL-PTID )
              AND (CNT-LINE             >   5        )
                ADD     1               TO  CNT-LINE
                MOVE    ALL "-"         TO  HCMSL80-MOJI (CNT-LINE)
           END-IF
           IF      CNT-LINE            >=  MAX-LINE
               PERFORM 400-PRINT-SEC
               MOVE    ZERO            TO  CNT-LINE
               PERFORM 280-HEAD-HENSYU-SEC
           END-IF
           ADD     1                   TO  CNT-LINE
           MOVE    LST-PRT-BODY-01     TO  HCMSL80-MOJI (CNT-LINE)
           IF      WRK-MAE-PTID    NOT =   TBL-PTID
               ADD     1                   TO  CNT-LINE
           END-IF
      *
           MOVE    TBL-PTID            TO  WRK-MAE-PTID
      *
      *    明細編集
           MOVE    SPACE               TO  LST-PRT-BODY-02
           MOVE    TBL-SRYYMD          TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  PRT-SRYYMD
      *    診療科編集
           PERFORM 200201-SRYKA-HEN-SEC
           MOVE    TBL-HKNCOMBI        TO  PRT-HKNCOMBI
      *
      *    明細編集
           PERFORM VARYING     IDS     FROM    1   BY  1
                   UNTIL      (IDS         >   50  )   OR
                              (TBL-SRY-SRYCD (IDS) =   SPACE )
      *        向精神薬のみ
               IF      TBL-FFECT-KBN(IDS)  NOT =   ZERO
      *
                   MOVE    TBL-NAME (IDS)      TO  PRT-TNS-NAME
      *
                   EVALUATE    TBL-FFECT-KBN(IDS)
                   WHEN    "1"
                       MOVE    "【不】"        TO  PRT-KBN
                   WHEN    "2"
                       MOVE    "【睡】"        TO  PRT-KBN
                   WHEN    "3"
                       MOVE    "【う】"        TO  PRT-KBN
                   WHEN    "4"
                       MOVE    "【精】"        TO  PRT-KBN
                   END-EVALUATE
      *
                   STRING  "("
                           TBL-GROUP-KBN(IDS)
                           ")"                DELIMITED  BY  SIZE
                                              INTO    PRT-YAKKCD
                   END-STRING
      *            臨時投薬
                   IF      TBL-RINJI-KBN(IDS)      =   "1"
                       MOVE    "（臨）"            TO  PRT-SRYKBN
                   END-IF
      *
                   IF      CNT-LINE           >=  MAX-LINE
                       PERFORM 400-PRINT-SEC
                       MOVE    ZERO            TO  CNT-LINE
                       PERFORM 280-HEAD-HENSYU-SEC
                   END-IF
                   ADD     1                   TO  CNT-LINE
                   MOVE    LST-PRT-BODY-02     TO  HCMSL80-MOJI
                                                         (CNT-LINE)
      *
                   IF      WRK-PARA-CSVKBN     =   "1"
      *                ＣＳＶデータ出力
                       PERFORM 500-CSV-SYORI-SEC
                   END-IF
      *
                   MOVE    SPACE               TO  LST-PRT-BODY-02
               END-IF
           END-PERFORM
      *
           MOVE    ORCSPSYCHO-COUNT (1)    TO  PRT-CNT1
           MOVE    ORCSPSYCHO-COUNT (2)    TO  PRT-CNT2
           MOVE    ORCSPSYCHO-COUNT (3)    TO  PRT-CNT3
           MOVE    ORCSPSYCHO-COUNT (4)    TO  PRT-CNT4
           IF     (ORCSPSYCHO-TAZAI    =   1 )
               MOVE    "【多剤投与】"      TO  PRT-ERRMSG
           ELSE
               MOVE    SPACE               TO  PRT-ERRMSG
           END-IF
           IF     (CNT-LINE +  2 )     >   MAX-LINE
               PERFORM 400-PRINT-SEC
               MOVE    ZERO            TO  CNT-LINE
               PERFORM 280-HEAD-HENSYU-SEC
           END-IF
           ADD     2                   TO  CNT-LINE
           MOVE    LST-PRT-BODY-03     TO  HCMSL80-MOJI  (CNT-LINE)
      *    １行空ける
           ADD     1                   TO  CNT-LINE
      *
      *    対象有設定
           MOVE    1                   TO  FLG-CNT-ARI
           IF     (ORCSPSYCHO-TAZAI    =   1 )
               MOVE    1                   TO  FLG-CNT-TAZAI
           END-IF
           IF      ORCSPSYCHO-COUNT (1)    >=  3
               MOVE    1                   TO  FLG-CNT-K1
           END-IF
           IF      ORCSPSYCHO-COUNT (2)    >=  3
               MOVE    1                   TO  FLG-CNT-K2
           END-IF
      *Ｈ28.4
           IF      TBL-SRYYMD          <   "20160401"
      *    Ｈ28.3まで　４種類以上
               IF      ORCSPSYCHO-COUNT (3)    >=  4
                  MOVE    1                   TO  FLG-CNT-K3
               END-IF
               IF      ORCSPSYCHO-COUNT (4)    >=  4
                   MOVE    1                   TO  FLG-CNT-K4
               END-IF
           ELSE
      *    Ｈ28.4から　３種類以上
               IF      ORCSPSYCHO-COUNT (3)    >=  3
                   MOVE    1                   TO  FLG-CNT-K3
               END-IF
               IF      ORCSPSYCHO-COUNT (4)    >=  3
                   MOVE    1                   TO  FLG-CNT-K4
               END-IF
           END-IF
           IF      ORCSPSYCHO-COUNT (3)    >   ZERO
               MOVE    1                   TO  FLG-CNT-A3
           END-IF
           IF      ORCSPSYCHO-COUNT (4)    >   ZERO
               MOVE    1                   TO  FLG-CNT-A4
           END-IF
           .
       20020-PRITN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       20020-PTNUM-HEN-SEC        SECTION.
      *
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    TBL-PTID            TO  SPTID-PTID
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF      SPTID-RC            =   ZERO
               MOVE    SPTID-PTNUM         TO  PRT-PTNUM
           END-IF
      *    患者マスター読み込み
           MOVE    SPACE               TO  PTINF-REC
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    TBL-PTID            TO  PTINF-PTID
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
      *    件数集計編集処理
      *****************************************************************
       20029-CNT-SYORI-SEC        SECTION.
      *
           IF      FLG-CNT-ARI         =   1
               ADD     1               TO  CNT-OUT-1
           END-IF
      *    多剤投与あり
           IF      FLG-CNT-TAZAI       =   1
               ADD     1               TO  CNT-OUT-2
           END-IF
      *    種類あり
           IF      FLG-CNT-K1          =   1
               ADD     1               TO  CNT-OUT-3-1
           END-IF
           IF      FLG-CNT-K2          =   1
               ADD     1               TO  CNT-OUT-3-2
           END-IF
           IF      FLG-CNT-K3          =   1
               ADD     1               TO  CNT-OUT-3-3
           END-IF
           IF      FLG-CNT-K4          =   1
               ADD     1               TO  CNT-OUT-3-4
           END-IF
      *H28.7
      *    うつ・精神の多剤投与
           IF     (FLG-CNT-K3          =   1 )
              OR  (FLG-CNT-K4          =   1 )
               ADD     1               TO  CNT-OUT-3-8
           END-IF
      *
      *    うつ薬
           IF      FLG-CNT-A3          =   1
               ADD     1               TO  CNT-OUT-4-3
           END-IF
           IF      FLG-CNT-A4          =   1
               ADD     1               TO  CNT-OUT-4-4
           END-IF
      *    うつ薬・抗精神薬あり
           IF     (FLG-CNT-A3          =   1 )
              OR  (FLG-CNT-A4          =   1 )
               ADD     1               TO  CNT-OUT-4
           END-IF
      *
           MOVE    ZERO                TO  FLG-CNT-ARI
           MOVE    ZERO                TO  FLG-CNT-TAZAI
           MOVE    ZERO                TO  FLG-CNT-K1
           MOVE    ZERO                TO  FLG-CNT-K2
           MOVE    ZERO                TO  FLG-CNT-K3
           MOVE    ZERO                TO  FLG-CNT-K4
      *
           MOVE    ZERO                TO  FLG-CNT-A3
           MOVE    ZERO                TO  FLG-CNT-A4
      *
           .
       20029-CNT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       200201-SRYKA-HEN-SEC        SECTION.
      *
           PERFORM VARYING     IDZ     FROM    1   BY  1
                   UNTIL      (IDZ         >   40  )   OR
                              (TBL-SRYKA-CD (IDZ)  =   SPACE)
               IF      TBL-SRYKA-CD (IDZ)  =   TBL-SRYKA
                   MOVE    TBL-SRYKA-MEI (IDZ)     TO  PRT-SRYKA
                   MOVE    TBL-SRYKA-MEI (IDZ)     TO  TBL-SRYKAMEI
                   MOVE    40                  TO  IDZ
               END-IF
           END-PERFORM
      *
           .
       200201-SRYKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    見出し編集処理
      *****************************************************************
       280-HEAD-HENSYU-SEC        SECTION.
      *
           MOVE    SPACE               TO  HCMSL80
           ADD     1                   TO  CNT-PAGE
           MOVE    CNT-PAGE            TO  PRT-PAGE
      *
           MOVE    LST-PRT-HEAD1       TO  HCMSL80-MOJI (1)
           MOVE    LST-PRT-HEAD2       TO  HCMSL80-MOJI (3)
           MOVE    LST-PRT-HEAD3       TO  HCMSL80-MOJI (4)
           MOVE    ALL "-"             TO  HCMSL80-MOJI (5)
      *
           MOVE    5                   TO  CNT-LINE
      *
           .
       280-HEAD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    合計編集処理
      *****************************************************************
       2004-GOUKEI-HENSYU-SEC          SECTION.
      *
           MOVE    CNT-OUT-1           TO  PRT-F-CNT1
           MOVE    CNT-OUT-2           TO  PRT-F-CNT2
           MOVE    CNT-OUT-3-1         TO  PRT-F-CNT31
           MOVE    CNT-OUT-3-2         TO  PRT-F-CNT32
           MOVE    CNT-OUT-3-3         TO  PRT-F-CNT33
           MOVE    CNT-OUT-3-4         TO  PRT-F-CNT34
      *H28.7
           MOVE    CNT-OUT-3-8         TO  PRT-F-CNT38
      *H28.4
           MOVE    CNT-OUT-4           TO  PRT-F-CNT4
           MOVE    CNT-OUT-4-3         TO  PRT-F-CNT43
           MOVE    CNT-OUT-4-4         TO  PRT-F-CNT44
      *
           IF     (CNT-LINE +  7 )     >   MAX-LINE
               PERFORM 400-PRINT-SEC
               MOVE    ZERO            TO  CNT-LINE
               PERFORM 280-HEAD-HENSYU-SEC
           END-IF
           ADD     1                   TO  CNT-LINE
           MOVE    ALL "-"             TO  HCMSL80-MOJI (CNT-LINE)
           ADD     2                   TO  CNT-LINE
           MOVE    LST-PRT-FOOT-01     TO  HCMSL80-MOJI (CNT-LINE)
           ADD     1                   TO  CNT-LINE
           MOVE    LST-PRT-FOOT-02     TO  HCMSL80-MOJI (CNT-LINE)
      *H28.7
           ADD     2                   TO  CNT-LINE
           MOVE    LST-PRT-FOOT-03     TO  HCMSL80-MOJI (CNT-LINE)
           ADD     1                   TO  CNT-LINE
           MOVE    LST-PRT-FOOT-04     TO  HCMSL80-MOJI (CNT-LINE)
      *
           .
       2004-GOUKEI-HENSYU-EXT.
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
           MOVE    "HCMSL80.red"        TO  SPRT-PRTID
           MOVE    "向精神薬投与患者一覧"
                                        TO  SPRT-TITLE
           MOVE    HCMSL80              TO  SPRT-PRTDATA
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
      *    ＣＳＶデータ処理
      *****************************************************************
       500-CSV-SYORI-SEC             SECTION.
      *
           IF      CNT-CSV             =   ZERO
               PERFORM 5001-CSV-HEAD-SEC
           END-IF
      *
           MOVE    SPACE               TO  WRK-CSV-AREA
           INITIALIZE                      WRK-CSV-AREA
      *    患者番号
           MOVE    SPTID-PTNUM         TO  WRK-IN-DATA
           MOVE    20                  TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    患者氏名
           MOVE    PTINF-NAME          TO  WRK-IN-DATA
           MOVE    100                 TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    診療日付
           MOVE    TBL-SRYYMD          TO  WRK-SYMD
           MOVE    WRK-SYY             TO  WRK-CYY
           MOVE    WRK-SMM             TO  WRK-CMM
           MOVE    WRK-SDD             TO  WRK-CDD
           MOVE    WRK-CSVHENYMD       TO  WRK-IN-DATA
           MOVE    10                  TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    診療科
           MOVE    TBL-SRYKAMEI        TO  WRK-IN-DATA
           MOVE    10                  TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    保険組合せ番号
           MOVE    TBL-HKNCOMBI        TO  WRK-IN-DATA
           MOVE    4                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    連番
           MOVE    TBL-JYURRK-RENNUM   TO  WRK-IN-DATA
           MOVE    1                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    種類
           EVALUATE    TBL-FFECT-KBN(IDS)
               WHEN    "1"
                   MOVE    "抗不安薬"            TO  WRK-IN-DATA
               WHEN    "2"
                   MOVE    "睡眠薬"              TO  WRK-IN-DATA
               WHEN    "3"
                   MOVE    "抗うつ薬"            TO  WRK-IN-DATA
               WHEN    "4"
                   MOVE    "抗精神病薬"          TO  WRK-IN-DATA
               WHEN    OTHER
                   MOVE    SPACE                 TO  WRK-IN-DATA
           END-EVALUATE
           MOVE    10                  TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    グループ
           MOVE    TBL-GROUP-KBN(IDS)  TO  WRK-IN-DATA
           MOVE    7                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    臨時投薬
           IF      TBL-RINJI-KBN(IDS)      =   "1"
               MOVE   "臨時投与"           TO  WRK-IN-DATA
           ELSE
               MOVE    SPACE               TO  WRK-IN-DATA
           END-IF
           MOVE    8                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    薬剤コード
           MOVE    TBL-SRY-SRYCD (IDS) TO  WRK-IN-DATA
           MOVE    9                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    名称
           MOVE    TBL-NAME  (IDS)     TO  WRK-IN-DATA
           MOVE    80                  TO  WRK-IN-MAX
           MOVE    1                   TO  WRK-END
           PERFORM 5002-MOJI-HENSYU-SEC
      *
           MOVE    SPACE               TO  WRK-OUT-REC
           STRING  WRK-CSV-REC(1:WRK-REC-LENGTH)
                   WRK-KAIGYO              DELIMITED  BY  SIZE
                   INTO        WRK-OUT-REC
           END-STRING
           MOVE    WRK-OUT-REC         TO  WRK-CSV-REC
           PERFORM 600-TOUKEICSV-SEC
      *
           .
       500-CSV-SYORI-EXIT.
           EXIT.
      *
      *
      *****************************************************************
      *    統計ＣＳＶ出力処理
      *****************************************************************
       5001-CSV-HEAD-SEC           SECTION.
      *
           MOVE    CSV-HEAD-01         TO  WRK-OUT-REC
           MOVE    SPACE               TO  WRK-CSV-REC
           STRING  WRK-OUT-REC         DELIMITED  BY  SPACE
                   WRK-KAIGYO          DELIMITED  BY  SIZE
                                       INTO  WRK-CSV-REC
           END-STRING
           PERFORM 600-TOUKEICSV-SEC
      *
           .
       5001-CSV-HEAD-EXIT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（文字）
      *****************************************************************
       5002-MOJI-HENSYU-SEC          SECTION.
      *
           IF      WRK-IN-DATA         =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDW     FROM    WRK-IN-MAX  BY  -1
                       UNTIL   IDW     <       1
                   IF      WRK-IN-DATA (IDW:1) NOT =   SPACE
                       IF      WRK-REC-LENGTH      =   ZERO
                           MOVE    WRK-IN-DATA (1:IDW)
                                                   TO  WRK-CSV-REC
                       ELSE
                           MOVE    SPACE           TO  WRK-OUT-REC
                           STRING  WRK-CSV-REC(1:WRK-REC-LENGTH)
                                               DELIMITED   BY  SIZE
                                   WRK-IN-DATA(1:IDW)
                                               DELIMITED   BY  SIZE
                                   INTO        WRK-OUT-REC
                           END-STRING
                           MOVE    WRK-OUT-REC         TO  WRK-CSV-REC
                       END-IF
                       ADD     IDW                 TO  WRK-REC-LENGTH
                       MOVE    1                   TO  IDW
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-END             =   ZERO
               MOVE    SPACE               TO  WRK-OUT-REC
               STRING  WRK-CSV-REC(1:WRK-REC-LENGTH)
                       WRK-KUGIRI              DELIMITED  BY  SIZE
                       INTO        WRK-OUT-REC
               END-STRING
               MOVE    WRK-OUT-REC         TO  WRK-CSV-REC
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF
      *
           .
       5002-MOJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ＣＳＶ出力処理
      *****************************************************************
       600-TOUKEICSV-SEC           SECTION.
      *
           INITIALIZE                      ORCSTOUKEICSVAREA
           MOVE    "INS"               TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM
                                       TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  STOUKEICSV-SHELLID
           MOVE    WRK-SRY-YYMM        TO  STOUKEICSV-SRYYM
           MOVE    "2"                 TO  STOUKEICSV-NYUGAIKBN
           MOVE    ZERO                TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"         TO  STOUKEICSV-TO-FOLDER
           MOVE    WRK-TO-DATA         TO  STOUKEICSV-TO-DATA
           MOVE    "2"                 TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSV-REC         TO  STOUKEICSV-CSVDATA
           MOVE    "向精神薬投与患者一覧ＣＳＶ作成"
                                       TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"     USING
                                       ORCSTOUKEICSVAREA
                                       SPA-AREA
           IF      STOUKEICSV-RETURN   =   ZERO
               ADD     1               TO  CNT-CSV
               CONTINUE
           ELSE
               MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                      TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
      *
       600-TOUKEICSV-EXT.
           EXIT.
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
      *    異常終了処理
      *****************************************************************
       500-COBABORT-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-RECEERR
           STRING  "ORCBG022"      DELIMITED  BY  SIZE
                   WRK-TOUKEIERR   DELIMITED  BY  SIZE
                   LOW-VALUE       DELIMITED  BY  SIZE
                                   INTO    WRK-RECEERR
           END-STRING
           CALL    "cobabort"      USING   WRK-RECEERR
           .
       500-COBABORT-EXT.
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
           DISPLAY "*** ORCBG022 END ***"
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
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TBL-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    TBL-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
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
      *
           .
       910-TENSU-READ-EXT.
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
