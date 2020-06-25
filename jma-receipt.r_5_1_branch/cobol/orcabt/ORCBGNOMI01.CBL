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
       PROGRAM-ID.             ORCBGNOMI01.
      *****************************************************************
      *  システム名        :   ＯＲＣＡ
      *  サブシステム名    :   月次帳票
      *  コンポーネント名  :   令和１年１０月改定
      *                        点数マスタ点数エラーの可能性
      *                        がある患者リスト
      *  管理者            : 
      *  作成日付    作業者        記述
      *  19/09/25    NACL-多々納   新規作成
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
       WORKING-STORAGE             SECTION.
      *
      *    一覧（縦）
           COPY    "HCMSL80.INC".
      *    一覧（横）
      **   COPY    "HCMSL55.INC".
      *    一時ファイル対応
           COPY    "CPRECEERR.INC".
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
           03  FLG-SRYACCTPLUS     PIC 9(01).
      *
           03  FLG-ARI                 PIC 9(01).
      *
           03  FLG-YAKUARI             PIC 9(01).
           03  FLG-CHKKBN              PIC 9(01).
           03  FLG-NO                 PIC 9(01).
           03  TBL-ARI                 PIC 9(01).
      *
      *
      *
      *H24.4
      *    一般名処方加算
           03  FLG-IPPANKSN            PIC 9(01).
      *    一般名あり
           03  FLG-GENERIC-OK          PIC 9(01).
           03  FLG-GENERIC-NO          PIC 9(01).
           03  FLG-CHK-P               PIC 9(01).
           03  FLG-NAIYAK              PIC 9(01).
           03  FLG-TONPUK              PIC 9(01).
           03  FLG-GAIYAK              PIC 9(01).
           03  FLG-ZAI                 PIC 9(01).
           03  FLG-NAIYAK2             PIC 9(01).
           03  FLG-TONPUK2             PIC 9(01).
           03  FLG-GAIYAK2             PIC 9(01).
           03  FLG-NAIFUKU             PIC 9(01).
      *
           03  FLG-YAKUZAI             PIC 9(01).
           03  FLG-SYOHOU              PIC 9(01).
           03  FLG-CHKIKBN             PIC 9(01).
           03  FLG-ERRKBN              PIC 9(01).
           03  FLG-ZAIEND              PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-NAIGEN              PIC 9(01).
      *
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
           03  IDX2                    PIC 9(04).
           03  IDX-SYOHOU              PIC 9(04).
           03  IDX-IP                  PIC 9(04).
           03  IDXS                    PIC 9(04).
           03  IDXJ                    PIC 9(04).
           03  IDX-G                   PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
      *****03  RECEERR                 PIC X(100).
           03  WRK-PARA-SRYYM          PIC X(06).
           03  WRK-PARA-SRYYMD         PIC X(08).
           03  WRK-PARA-SYORIKBN       PIC X(01).
           03  WRK-PARA-CSVKBN         PIC X(01).
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
      *    処理年月開始日
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
               05  WRK-KEY-SRYKA           PIC X(02).
               05  WRK-KEY-HKNCOMBINUM     PIC X(04).
               05  WRK-KEY-RENNUM          PIC X(01).
               05  WRK-KEY-DENPNUM         PIC 9(07).
               05  WRK-KEY-GRPDENPNUM      PIC 9(07).
      *
           03  WRK-NYUGAIKBN           PIC X(01).
           03  WRK-SRYCD               PIC X(09).
           03  WRK-ZAIKAISU            PIC 9(03).
      *
           03  WRK-MAE-PTID            PIC 9(10).
           03  SORT-KEY-GROUP          PIC X(07).
           03  SORT-KEY-FFECT          PIC X(01).
      *
           03  WRK-ZAINUM              PIC 9(07).
           03  WRK-CHK-SRYSYUKBN       PIC X(03).
           03  WRK-GENECHG-KBN         PIC X(01).
           03  CNT-GENERIC             PIC 9(02).
           03  WRK-HKNCOMBI            PIC X(04).
      *
           03  WRK-SYOKEIS         PIC 9(08)V9(8).
           03  WRK-SYOKEIS2        PIC 9(08)V9(8).
           03  WRK-SYOKEI          PIC 9(08).
      *
           03  WRK-TENSUKEI        PIC 9(08)V9(4).
           03  WRK-KAISU           PIC 9(04).
           03  WRK-KEI-KISOTEN          PIC 9(07)V9(4).
      *
           03  WRK-ZAIKEI-AREA.
               05  WRK-KONGO           PIC 9(05).
               05  WRK-SONOTA          PIC 9(05).
               05  WRK-ZAICNT          PIC 9(05).
               05  WRK-KEIKEI          PIC 9(08).
               05  WRK-ZAICNT2         PIC 9(05).
               05  WRK-KEIKEI2         PIC 9(08).
               05  WRK-SANTEN          PIC 9(08).
               05  WRK-SANTEN-9        PIC 9(08)V9(4).
               05  WRK-TENKEI          PIC 9(08).
               05  WRK-ZAITEN          PIC 9(08).
               05  WRK-ZAITEN-G        PIC 9(08)V9(8).
      *H28.4   向精神薬以外
               05  WRK-KONGO-2         PIC 9(05).
               05  WRK-SONOTA-2        PIC 9(05).
               05  WRK-ZAICNT-2        PIC 9(05).
               05  WRK-KEIKEI-2        PIC 9(08).
               05  WRK-ZAICNT2-2       PIC 9(05).
               05  WRK-ZAITEN-G-2      PIC 9(08)V9(8).
      *
       01  TBL-SRY-AREA.
           03  TBL-PTID                PIC 9(10).
           03  TBL-SRYYMD              PIC X(08).
           03  TBL-SRYKA               PIC X(02).
           03  TBL-SRYKAMEI            PIC X(10).
           03  TBL-HKNCOMBI            PIC X(04).
           03  TBL-JYURRK-RENNUM       PIC 9(01).
           03  TBL-IPPKSN              PIC 9(01).
           03  TBL-MEISAI-G.
               05  TBL-SRY-OCCS            OCCURS   50.
                   07  TBL-SRY-SRYSYUKBN       PIC X(03).
                   07  TBL-SRY-SRYCD           PIC X(09).
                   07  TBL-YAKKAKJNCD          PIC X(12).
                   07  TBL-NAME                PIC X(80).
      *            薬剤の後発区分
                   07  TBL-KOUHATU-KBN         PIC 9(01).
                   07  TBL-GENERIC-KBN         PIC 9(01).
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
      * 確認リスト
       01  LST-PRT-AREA.
           03  LST-PRT-HEAD1.
               05  FILLER              PIC X(02)   VALUE   SPACE.
               05  FILLER              PIC X(48)   VALUE
                   "＃　令和１年９月薬価誤り　算定患者一覧　＃".
               05  PRT-NYUGAIKBN       PIC X(08).
               05  FILLER              PIC X(02)   VALUE   SPACE.
               05  PRT-SRYYM           PIC X(16).
               05  FILLER              PIC X(04)   VALUE
                   "分".
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  PRT-SYSYMD          PIC X(10)   VALUE   SPACE.
               05  PRT-TIME            PIC X(07)   VALUE   SPACE.
               05  PRT-PAGE            PIC ZZZ9.
               05  FILLER              PIC X(02)   VALUE   "頁".
           03  LST-PRT-HEAD2.
               05  FILLER              PIC X(20)   VALUE
                   "患者番号".
               05  FILLER              PIC X(01)   VALUE   SPACE.
               05  FILLER              PIC X(30)   VALUE
                   "氏名".
     **    03  LST-PRT-HEAD3.
     **        05  FILLER              PIC X(15)   VALUE   SPACE.
               05  FILLER              PIC X(01)   VALUE   SPACE.
               05  FILLER              PIC X(12)   VALUE
                   "診療日".
               05  FILLER              PIC X(10)   VALUE
                   "科".
               05  FILLER              PIC X(12)   VALUE
                   "保険組合せ".
               05  FILLER              PIC X(10)   VALUE
                   "薬剤コード".
      *
           03  LST-PRT-BODY-01.
               05  PRT-PTNUM           PIC X(20).
               05  PRT-FIL1            PIC X(01).
      *?       05  PRT-NAME            PIC X(50).
               05  PRT-NAME            PIC X(30).
               05  PRT-FIL2            PIC X(01).
      *
      **   03  LST-PRT-BODY-02.
      **       05  PRT-FIL20           PIC X(05).
               05  PRT-SRYYMD          PIC X(09).
               05  PRT-FIL21           PIC X(02).
               05  PRT-SRYKA           PIC X(10).
               05  PRT-FIL22           PIC X(01).
               05  PRT-HKNCOMBI        PIC X(04).
               05  PRT-FIL23           PIC X(08).
      *?       05  PRT-SRYKBN          PIC X(04).
      *?       05  PRT-FIL24           PIC X(01).
      ***      05  PRT-KBN             PIC X(08).
      *        05  PRT-FIL25           PIC X(01).
      **       05  PRT-GEKBN           PIC X(01).
      **       05  PRT-FIL26           PIC X(01).
               05  PRT-SRYCD           PIC X(09).
      *        05  PRT-FIL27           PIC X(01).
      *        05  PRT-PSYKBN          PIC X(02).
               05  PRT-TNS-NAME        PIC X(50).
           03  LST-PRT-BODY-02.
               05  PRT-FIL20           PIC X(05).
      *
           03  LST-PRT-BODY-03.
               05  PRT-FIL30           PIC X(24).
      *        05  PRT-SRYKBN-3        PIC X(06).
      *        05  PRT-FIL31           PIC X(01).
               05  PRT-MSG-3           PIC X(30).
     *******   05  PRT-KBN-3           PIC X(12).
               05  PRT-FIL32           PIC X(02).
               05  PRT-SRYCD-3         PIC X(09).
               05  PRT-FIL33           PIC X(01).
               05  PRT-TNS-NAME-3      PIC X(50).
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
       01  MAX-LINE           PIC 9(03)   VALUE   80.
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
               05  FILLER              PIC X(11)   VALUE
                                       "yakka_miss_".
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
           03  FILLER              PIC X(12)   VALUE   "医薬品コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "入外".
      *
      *
      *
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC          OCCURS   400.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
       01  LNK-WKSRYACT-MAX           PIC 9(04).
      *
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *    帳票編集情報
      **   COPY    "CPSK1030.INC".
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
      *    診療会計附加情報マスタ−
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
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
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *    院外処方一般名編集サブ
      **   COPY    "CPORCSGETGENERIC.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
      *    COPY    "K01COMMON-SPA".
      *
      *    診療行為入力 共通画面ＳＰＡ
      *    COPY    "K02SCR-SPA".
      *
      *    初期ＳＰＡ編集処理パラメタ
      *    COPY    "CPORCSC95.INC".
      *
      *    診療行為点数編集・基本チェック
      *    COPY    "CPORCSC92.INC".
      *
      *
           COPY    "CPYMISS201909.INC".
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
           INITIALIZE  FLG-AREA
           INITIALIZE  STS-AREA
           INITIALIZE  WRK-AREA
           INITIALIZE  CNT-AREA
           INITIALIZE  WRK-PARA
           INITIALIZE  SPA-AREA
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
                                               WRK-PARA-SRYYMD
                                               WRK-PARA-SYORIKBN
                                               WRK-PARA-CSVKBN
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    一時ファイル名取得
           INITIALIZE  SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  RECEERR
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    "ORCBGNOMI01"       TO  JOB-PGID
           MOVE    "令和１年９月薬価誤り算定患者一覧"
                                       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
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
           INITIALIZE  STS-AREA-DAY
           INITIALIZE  LNK-DAY1-AREA
           MOVE    "11"                TO  LNK-DAY1-IRAI
           MOVE    WRK-SYSYMD          TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
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
           ACCEPT  SYS-TIME        FROM    TIME
           MOVE    SYS-THH             TO  WRK-THH
           MOVE    SYS-TMM             TO  WRK-TMM
           MOVE    WRK-HENTIME         TO  PRT-TIME
      *
      *    診療日
           IF      WRK-PARA-SRYYM      =   SPACE
      *        未設定は令和１年９月
               MOVE    "201909"            TO  WRK-SRY-YYMM
           ELSE
               MOVE    WRK-PARA-SRYYM      TO  WRK-SRY-YYMM
           END-IF
      *
           MOVE    WRK-SRY-YYMM        TO  SPA-SRYYMD(1:6)
           MOVE    "01"                TO  SPA-SRYYMD(7:2)
      *    診療日
           IF      WRK-PARA-SRYYMD     NOT =   SPACE
               MOVE    WRK-PARA-SRYYMD     TO  SPA-SRYYMD
           END-IF
      *
      *    処理日
           INITIALIZE  STS-AREA-DAY
           INITIALIZE  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    SPA-SRYYMD          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3(1:16)
                                       TO  PRT-SRYYM
      *
      *    処理開始年月
           MOVE    WRK-SRY-YY          TO  WRK-STR-YY
           MOVE    WRK-SRY-MM          TO  WRK-STR-MM
           MOVE    01                  TO  WRK-STR-DD
      *    処理終了年月
           MOVE    WRK-SRY-YYMM        TO  WRK-END-YMD(1:6)
           MOVE    31                  TO  WRK-END-DD
      *    診療日
           IF      WRK-PARA-SRYYMD     NOT =   SPACE
               MOVE    WRK-PARA-SRYYMD     TO  WRK-STR-YMD
               MOVE    WRK-PARA-SRYYMD     TO  WRK-END-YMD
           END-IF
      *
      *    診療科をＴＢＬにセット
           PERFORM 1101-SRYKA-TBLSET-SEC
      *    帳票編集情報取得
      **** PERFORM 1102-SYS1030-GET-SEC
      *    ＣＳＶ
           MOVE    SPA-HOSPNUM         TO  WRK-TO-DATA-HOSPNUM
           MOVE    WRK-SRY-YYMM        TO  WRK-TO-DATA-SRYYM
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科  テーブル編集処理
      *****************************************************************
       1101-SRYKA-TBLSET-SEC       SECTION.
      *
           INITIALIZE  TBL-SRYKA-AREA
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
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
               MOVE    SYS-1005-KBNCD (1:2)
                                           TO  TBL-SRYKA-CD (IDX)
               MOVE    SYS-1005-SRYKANAME1 TO  TBL-SRYKA-MEI (IDX)
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
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
           MOVE    ZERO                TO  FLG-NAIGEN
      *
           MOVE    "2"                 TO  WRK-NYUGAIKBN
           PERFORM 200-MAIN-SYORI-SEC
      *
           MOVE    "1"                 TO  WRK-NYUGAIKBN
           PERFORM 200-MAIN-SYORI-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴処理
      *****************************************************************
       200-MAIN-SYORI-SEC         SECTION.
      *
           INITIALIZE  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    WRK-STR-YMD         TO  JYURRK-SRYYMD
           MOVE    WRK-END-YMD         TO  JYURRK-UPSRYYMD
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key64"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key64"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE  JYURRK-REC
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           INITIALIZE  WRK-KEY-AREA
           MOVE    ZERO                TO  WRK-MAE-PTID
      *
           MOVE    ZERO                TO  FLG-YAKUARI
           PERFORM
               UNTIL   (FLG-JYURRK         =   1   )
      *
               MOVE    ZERO            TO  FLG-NO
               IF     WRK-PARA-SYORIKBN    =   "1"
      *            2019/9/24以降の更新
                 IF      JYURRK-UPYMD      <  "20190924"
                     MOVE    1             TO  FLG-NO
                 END-IF
               END-IF
      *
             IF   FLG-NO     =   ZERO
      *
               IF     (WRK-KEY-PTID        =   JYURRK-PTID   )
                 AND  (WRK-KEY-SRYYMD      =   JYURRK-SRYYMD )
                 AND  (WRK-KEY-SRYKA       =   JYURRK-SRYKA  )
                 AND  (WRK-KEY-HKNCOMBINUM =   JYURRK-HKNCOMBINUM)
                 AND  (WRK-KEY-RENNUM      =   JYURRK-RENNUM)
                   MOVE    1               TO  FLG-OK
               ELSE
                   MOVE    ZERO            TO  FLG-OK
               END-IF
               IF      FLG-OK          =   ZERO
                   IF    FLG-YAKUARI       =   1
      *                印刷処理
                       PERFORM 20020-PRINT-SYORI-SEC
                   END-IF
                   MOVE    ZERO                TO  FLG-YAKUARI
                   MOVE    JYURRK-PTID         TO  WRK-KEY-PTID
                   MOVE    JYURRK-SRYYMD       TO  WRK-KEY-SRYYMD
                   MOVE    JYURRK-SRYKA        TO  WRK-KEY-SRYKA
                   MOVE    JYURRK-HKNCOMBINUM  TO  WRK-KEY-HKNCOMBINUM
                   MOVE    JYURRK-RENNUM       TO  WRK-KEY-RENNUM
               END-IF
      *
               PERFORM 2001-JYURRK-HEN-SEC
      *
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key64"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key64"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF    FLG-YAKUARI       =   1
      *        印刷処理
               PERFORM 20020-PRINT-SYORI-SEC
           END-IF
      *
           IF      CNT-LINE            >   ZERO
               PERFORM 400-PRINT-SEC
               MOVE    ZERO                TO  CNT-LINE
           END-IF
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴処理
      *****************************************************************
       2001-JYURRK-HEN-SEC         SECTION.
      *
           MOVE    JYURRK-SRYYMD(7:2)  TO  IDX-DAY
      *
           PERFORM VARYING     IDXJ    FROM    1   BY  1
                   UNTIL      (IDXJ    >   15  )
                         OR   (JYURRK-ZAINUM (IDXJ) =   ZERO )
                         OR   (FLG-YAKUARI     =    1    )
               MOVE    SPACE               TO  SRYACT-REC
               INITIALIZE  SRYACT-REC
               MOVE    JYURRK-HOSPNUM      TO  SRY-HOSPNUM
               MOVE    JYURRK-NYUGAIKBN    TO  SRY-NYUGAIKBN
               MOVE    JYURRK-PTID         TO  SRY-PTID
               MOVE    JYURRK-SRYKA        TO  SRY-SRYKA
               MOVE    JYURRK-SRYYMD(1:6)  TO  SRY-SRYYM
               MOVE    JYURRK-ZAINUM (IDXJ) TO  SRY-ZAINUM
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
                   INITIALIZE  SRYACT-REC
                   MOVE    1                   TO  FLG-SRYACT
               END-IF
      *
               MOVE    ZERO                TO  FLG-CHKIKBN
      *
      *        IF      FLG-SRYACT          =   ZERO
      *          AND  (FLG-CHKIKBN         =   ZERO )
      *            PERFORM 20011-SRYACCT-CHK-SEC
      *        END-IF
      *        IF      SRY-SRYKBN          =   "12"
      *            MOVE    1                   TO  FLG-CHKIKBN
      *        END-IF
      *
               MOVE    ZERO                TO  FLG-CHKKBN
               IF      (SRY-SRYKBN         =   "11"
                                           OR  "12"
                                           OR  "13"
                                           OR  "99")
                   MOVE    1                   TO  FLG-CHKKBN
               END-IF
               PERFORM
                       UNTIL  (FLG-SRYACT      =   1  )
                          OR  (FLG-CHKIKBN     =   1  )
                         OR   (FLG-YAKUARI     =    1    )
      *
                   PERFORM VARYING     IDS2     FROM    1   BY  1
                       UNTIL      (IDS2         >   5   )   OR
                                  (SRY-SRYCD (IDS2)     =   SPACE )
                             OR   (FLG-YAKUARI     =    1    )
      *                薬剤チェック
                       IF      SRY-SRYCD (IDS2)(1:1)   =   "6"
                           PERFORM 20012-SRYACT-CHK-SEC
                       END-IF
                   END-PERFORM
      *
                   MOVE    "tbl_sryact"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 930-SRYACT-READ-SEC
               END-PERFORM
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
           END-PERFORM
      *
           .
       2001-JYURRK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計チェック処理
      *****************************************************************
       20012-SRYACT-CHK-SEC       SECTION.
      *
           SET     YMI-IDX            TO  1
           SEARCH      TBL-YMNSA-YMI-OCC    VARYING   YMI-IDX
                   AT  END
                       MOVE    ZERO            TO  TBL-ARI
                   WHEN    SRY-SRYCD (IDS2)    =   TBL-YMI-SRYCD
                                                       (YMI-IDX)
                       MOVE    1               TO  TBL-ARI
           END-SEARCH
           IF      TBL-ARI             =   1
               MOVE   1                TO  FLG-YAKUARI
               MOVE   SRY-SRYCD (IDS2) TO  WRK-SRYCD
           END-IF
      *
           .
       20012-SRYACT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       20020-PRINT-SYORI-SEC       SECTION.
      *
      *    明細編集
           MOVE    SPACE               TO  LST-PRT-BODY-01
      *    患者変更なし
           IF      WRK-MAE-PTID    NOT =   WRK-KEY-PTID
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
           IF     (WRK-MAE-PTID     NOT =   WRK-KEY-PTID )
              AND (CNT-LINE             >   4        )
                ADD     1               TO  CNT-LINE
                MOVE    ALL "-"         TO  HCMSL80-MOJI (CNT-LINE)
           END-IF
           IF      CNT-LINE            >=  MAX-LINE
               PERFORM 400-PRINT-SEC
               MOVE    ZERO            TO  CNT-LINE
               PERFORM 280-HEAD-HENSYU-SEC
           END-IF
      *    ADD     1                   TO  CNT-LINE
      *    MOVE    LST-PRT-BODY-01     TO  HCMSL80-MOJI (CNT-LINE)
      *    IF      WRK-MAE-PTID    NOT =   WRK-KEY-PTID
      *        ADD     1                   TO  CNT-LINE
      *    END-IF
      *
           MOVE    WRK-KEY-PTID        TO  WRK-MAE-PTID
      *
      *    明細編集
           MOVE    SPACE               TO  LST-PRT-BODY-02
           MOVE    WRK-KEY-SRYYMD      TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  PRT-SRYYMD
      *    診療科編集
           PERFORM 200201-SRYKA-HEN-SEC
           MOVE    WRK-KEY-HKNCOMBINUM     TO  PRT-HKNCOMBI
           MOVE    WRK-KEY-HKNCOMBINUM     TO  WRK-HKNCOMBI
      *
           MOVE    WRK-SRYCD           TO  PRT-SRYCD
      *
           IF      CNT-LINE               >=  MAX-LINE
               PERFORM 400-PRINT-SEC
               MOVE    ZERO                TO  CNT-LINE
               PERFORM 280-HEAD-HENSYU-SEC
           END-IF
           ADD     1                   TO  CNT-LINE
           MOVE    LST-PRT-BODY-01     TO  HCMSL80-MOJI (CNT-LINE)
      *
           IF      WRK-PARA-CSVKBN     =   "1"
      *         ＣＳＶデータ出力
                PERFORM 500-CSV-SYORI-SEC
           END-IF
           .
       20020-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       20020-PTNUM-HEN-SEC         SECTION.
      *
           INITIALIZE  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    WRK-KEY-PTID        TO  SPTID-PTID
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                           SPA-AREA
           IF      SPTID-RC            =   ZERO
               MOVE    SPTID-PTNUM         TO  PRT-PTNUM
           END-IF
      *    患者マスター読み込み
           MOVE    SPACE               TO  PTINF-REC
           INITIALIZE  PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    WRK-KEY-PTID        TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   1
               MOVE    "＊＊＊＊"          TO  PRT-NAME
           ELSE
               MOVE    PTINF-NAME          TO  PRT-NAME
           END-IF
           .
       20020-PTNUM-HEN-EXT.
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
               IF      TBL-SRYKA-CD (IDZ)  =   WRK-KEY-SRYKA
                   MOVE    TBL-SRYKA-MEI (IDZ)     TO  PRT-SRYKA
                   MOVE    TBL-SRYKA-MEI (IDZ)     TO  TBL-SRYKAMEI
                   MOVE    40                  TO  IDZ
               END-IF
           END-PERFORM
           .
       200201-SRYKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    見出し編集処理
      *****************************************************************
       280-HEAD-HENSYU-SEC         SECTION.
      *
           MOVE    SPACE               TO  HCMSL80
           ADD     1                   TO  CNT-PAGE
           MOVE    CNT-PAGE            TO  PRT-PAGE
      *
           IF      WRK-NYUGAIKBN       =   "2"
               MOVE    "【外来】"      TO  PRT-NYUGAIKBN
           ELSE
               MOVE    "【入院】"      TO  PRT-NYUGAIKBN
           END-IF
      *
           MOVE    LST-PRT-HEAD1       TO  HCMSL80-MOJI (1)
           MOVE    LST-PRT-HEAD2       TO  HCMSL80-MOJI (3)
      **   MOVE    LST-PRT-HEAD3       TO  HCMSL80-MOJI (4)
      **   MOVE    ALL "-"             TO  HCMSL80-MOJI (5)
           MOVE    ALL "-"             TO  HCMSL80-MOJI (4)
      *
      **   MOVE    5                   TO  CNT-LINE
           MOVE    4                   TO  CNT-LINE
           .
       280-HEAD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       400-PRINT-SEC               SECTION.
      *
           INITIALIZE  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    "HCMSL80.red"       TO  SPRT-PRTID
           MOVE    "薬剤点数　疑い患者一覧"
                                       TO  SPRT-TITLE
           MOVE    HCMSL80             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"       USING   ORCSPRTAREA
                                           SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       400-PRINT-EXIT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶデータ処理
      *****************************************************************
       500-CSV-SYORI-SEC           SECTION.
      *
           IF      CNT-CSV             =   ZERO
               PERFORM 5001-CSV-HEAD-SEC
           END-IF
      *
           MOVE    SPACE               TO  WRK-CSV-AREA
           INITIALIZE  WRK-CSV-AREA
      *    患者番号
           MOVE    SPTID-PTNUM         TO  WRK-IN-DATA
           MOVE    20                  TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    患者氏名
           MOVE    PTINF-NAME          TO  WRK-IN-DATA
           MOVE    100                 TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    診療日付
           MOVE    WRK-KEY-SRYYMD      TO  WRK-SYMD
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
           MOVE    WRK-KEY-HKNCOMBINUM TO  WRK-IN-DATA
           MOVE    4                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    薬剤コード
           MOVE    WRK-SRYCD           TO  WRK-IN-DATA
           MOVE    9                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    入外
           IF      WRK-NYUGAIKBN       =   "2"
               MOVE    "外"            TO  WRK-IN-DATA
           ELSE
               MOVE    "入"            TO  WRK-IN-DATA
           END-IF
           MOVE    2                   TO  WRK-IN-MAX
           MOVE    1                   TO  WRK-END
           PERFORM 5002-MOJI-HENSYU-SEC
      *    MOVE    SPA-GMN-MEISYO(IDX)   TO  WRK-IN-DATA
      *    MOVE    80                  TO  WRK-IN-MAX
      *    MOVE    1                   TO  WRK-END
      *    PERFORM 5002-MOJI-HENSYU-SEC
      *
           MOVE    SPACE               TO  WRK-OUT-REC
           STRING  WRK-CSV-REC(1:WRK-REC-LENGTH)
                   WRK-KAIGYO              DELIMITED  BY  SIZE
                   INTO        WRK-OUT-REC
           END-STRING
           MOVE    WRK-OUT-REC         TO  WRK-CSV-REC
           PERFORM 600-TOUKEICSV-SEC
           .
       500-CSV-SYORI-EXIT.
           EXIT.
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
           .
       5001-CSV-HEAD-EXIT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（文字）
      *****************************************************************
       5002-MOJI-HENSYU-SEC        SECTION.
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
           .
       5002-MOJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ＣＳＶ出力処理
      *****************************************************************
       600-TOUKEICSV-SEC           SECTION.
      *
           INITIALIZE  ORCSTOUKEICSVAREA
           MOVE    "INS"               TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  STOUKEICSV-SHELLID
           MOVE    WRK-SRY-YYMM        TO  STOUKEICSV-SRYYM
           MOVE    WRK-NYUGAIKBN       TO  STOUKEICSV-NYUGAIKBN
           MOVE    ZERO                TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"         TO  STOUKEICSV-TO-FOLDER
           MOVE    WRK-TO-DATA         TO  STOUKEICSV-TO-DATA
           MOVE    "2"                 TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSV-REC         TO  STOUKEICSV-CSVDATA
           MOVE    "令和１年９月薬価誤り算定患者一覧ＣＳＶ作成"
                                       TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV" USING   ORCSTOUKEICSVAREA
                                           SPA-AREA
           IF      STOUKEICSV-RETURN   =   ZERO
               ADD     1                   TO  CNT-CSV
               CONTINUE
           ELSE
               MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                           TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
       600-TOUKEICSV-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       800-SEIWA-HEN-SEC           SECTION.
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
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
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
               OPEN    OUTPUT  RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"               TO  SJOBKANRI-MODE
               INITIALIZE  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
               MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
               MOVE    WRK-RECEERR         TO  JOB-YOBI
               MOVE    "9999"              TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA
                                               JOBKANRI-REC
                                               SPA-AREA
           END-IF
      *
           MOVE    1                   TO  FLG-END
                                           FLG-ERR
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    異常終了処理
      *****************************************************************
       500-COBABORT-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-RECEERR
           STRING  "ORCBGNOMI01"       DELIMITED  BY  SIZE
                   WRK-TOUKEIERR       DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
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
           IF      CNT-PAGE            =   ZERO 
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBGNOMI01 END ***"
      *
      *    ステップ管理終了処理
           MOVE    "STE"               TO  SJOBKANRI-MODE
           INITIALIZE  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    CNT-PAGE            TO  JOB-UPDCNT
           CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       900-SYSKANRI-READ-SEC       SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYSKANRI-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               INITIALIZE  SYSKANRI-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
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
                   INITIALIZE  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
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
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE  SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           .
       930-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター読込（主キー）
      *****************************************************************
       940-SRYACCT-READ-SEC        SECTION.
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
                   INITIALIZE  SRYACCT-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
               INITIALIZE  SRYACCT-REC
           END-IF
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       940-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JYURRK-REC
               MOVE    ZERO                TO  FLG-JYURRK
           ELSE
               INITIALIZE  JYURRK-REC
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           .
       970-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC          SECTION.
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
                   INITIALIZE  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計附加情報検索処理
      *****************************************************************
       32012-SRYACCTPLUS-READ-SEC       SECTION.
      *
           MOVE    SPACE               TO  SRYACCTPLUS-REC
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    ACCT-HOSPNUM        TO  ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID           TO  ACCTP-PTID
           MOVE    ACCT-SRYKA          TO  ACCTP-SRYKA
           MOVE    ACCT-SRYYM          TO  ACCTP-SRYYM
           MOVE    ACCT-ZAINUM         TO  ACCTP-ZAINUM
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SRYACCTPLUS-REC
                   MOVE    ZERO                TO  FLG-SRYACCTPLUS
               ELSE
                   INITIALIZE                      SRYACCTPLUS-REC
                   MOVE    1                   TO  FLG-SRYACCTPLUS
               END-IF
           ELSE
               INITIALIZE                  SRYACCTPLUS-REC
               MOVE    1               TO  FLG-SRYACCTPLUS
           END-IF
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       32012-SRYACCTPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
