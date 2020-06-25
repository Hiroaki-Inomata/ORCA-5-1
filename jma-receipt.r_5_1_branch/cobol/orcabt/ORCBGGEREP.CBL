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
       PROGRAM-ID.             ORCBGGEREP.
      *****************************************************************
      *  システム名        :   ＯＲＣＡ
      *  サブシステム名    :   月次帳票
      *  コンポーネント名  :   平成28年４月改定
      *                        医薬品マスタの後発区分の設定ミスにより
      *                        一般名処方加算が誤算定されている可能性
      *                        がある患者リスト
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/04/07    NACL-伊藤     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *  04.08.00    NACL-多々納  17/03/XX   Ｈ２９年３月１７日以降対応
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
      *
           03  FLG-ARI                 PIC 9(01).
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
      *
           03  FLG-YAKUZAI             PIC 9(01).
           03  FLG-SYOHOU              PIC 9(01).
           03  FLG-CHKIKBN             PIC 9(01).
           03  FLG-ERRKBN              PIC 9(01).
           03  FLG-ZAIEND              PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
           03  FLG-OK                  PIC 9(01).
      *    一般名あり
           03  FLG-GENERIC-OK12        PIC 9(01).
           03  FLG-GENERIC-NO2          PIC 9(01).
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
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
      *****03  RECEERR                 PIC X(100).
           03  WRK-PARA-SRYYM          PIC X(06).
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
               05  WRK-KEY-DENPNUM         PIC 9(07).
               05  WRK-KEY-SRYKA           PIC X(02).
               05  WRK-KEY-GRPDENPNUM      PIC 9(07).
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
      *
           03  CNT-GENERIC2            PIC 9(02).
           03  CNT-GENERIC3            PIC 9(02).
      *
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-MEISYO              PIC X(80).
      *R01.5
      * 一般名加算チェック
        01  TBL-GENERIC-CHEK-AREA.
           03  TBL-GENERIC-CHK-OCC     OCCURS   2.
               05  TBL-GENERIC-CHK-OCC2    OCCURS   200.
                   07  TBL-GEN-SRYCD       PIC X(09).
                   07  TBL-GEN-GECODE      PIC X(12).
           03  IDXG1                   PIC 9(04).
           03  IDXG2                   PIC 9(04).
           03  FLG-IPKSNOK             PIC 9(01).
           03  FLG-IPKSNEND            PIC 9(01).
      *
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
               05  FILLER              PIC X(44)   VALUE
                   "＃＃　一般名処方加算誤算定疑い患者一覧　＃＃".
               05  PRT-MIDASI          PIC X(10).
     ******    05  FILLER              PIC X(16)   VALUE   SPACE.
               05  PRT-SRYYM           PIC X(16).
               05  FILLER              PIC X(04)   VALUE
                   "分".
               05  FILLER              PIC X(05)   VALUE   SPACE.
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
               05  PRT-SRYKBN          PIC X(04).
               05  PRT-FIL24           PIC X(01).
               05  PRT-KBN             PIC X(08).
               05  PRT-FIL25           PIC X(01).
      **       05  PRT-GEKBN           PIC X(01).
      **       05  PRT-FIL26           PIC X(01).
               05  PRT-SRYCD           PIC X(09).
               05  PRT-FIL27           PIC X(01).
               05  PRT-TNS-NAME        PIC X(50).
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
               05  FILLER              PIC X(19)   VALUE
                                       "ippanmeikasan_miss_".
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
           COPY    "CPSK1030.INC".
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
           COPY    "CPORCSGETGENERIC.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    診療行為入力 共通画面ＳＰＡ
           COPY    "K02SCR-SPA".
      *
      *    初期ＳＰＡ編集処理パラメタ
           COPY    "CPORCSC95.INC".
      *
      *    診療行為点数編集・基本チェック
           COPY    "CPORCSC92.INC".
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
                                               WRK-PARA-SYORIKBN
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
           MOVE    "ORCBGGEREP"        TO  JOB-PGID
           MOVE    "一般名処方加算誤算定疑い患者一覧"
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
      *        未設定は平成２８年４月
               MOVE    "201604"            TO  WRK-SRY-YYMM
           ELSE
               MOVE    WRK-PARA-SRYYM      TO  WRK-SRY-YYMM
           END-IF
      *
           MOVE    WRK-SRY-YYMM        TO  SPA-SRYYMD(1:6)
           MOVE    "01"                TO  SPA-SRYYMD(7:2)
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
      *
           IF      WRK-PARA-SYORIKBN   =   "1"
               MOVE    "（処理１）"        TO  PRT-MIDASI
           ELSE
               MOVE    SPACE               TO  PRT-MIDASI
           END-IF
      *
      *    診療科をＴＢＬにセット
           PERFORM 1101-SRYKA-TBLSET-SEC
      *    帳票編集情報取得
           PERFORM 1102-SYS1030-GET-SEC
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
      *****************************************************************
      *    帳票編集情報取得処理
      *****************************************************************
       1102-SYS1030-GET-SEC        SECTION.
      *
      *    帳票編集情報
           MOVE    SPACE               TO  SYS-1030-REC
           MOVE    "1030"              TO  SYS-1030-KANRICD
           MOVE    "*"                 TO  SYS-1030-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1030-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1030-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1030-HOSPNUM
           MOVE    SYS-1030-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               MOVE    SYSKANRI-REC        TO  SYS-1030-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       1102-SYS1030-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *
           INITIALIZE  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    "2"                 TO  JYURRK-NYUGAIKBN
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
           INITIALIZE  TBL-SRY-AREA
           MOVE    ZERO                TO  IDX-TBL
           MOVE    ZERO                TO  WRK-MAE-PTID
           INITIALIZE  WRK-KEY-AREA
           MOVE    ZERO                TO  WRK-ZAINUM
           MOVE    ZERO                TO  FLG-SYOHOU
           INITIALIZE   LNK-WKSRYACT-AREA
           MOVE    JYURRK-RENNUM       TO  TBL-JYURRK-RENNUM
      *
           PERFORM
               UNTIL   (FLG-JYURRK         =   1   )
      *
               IF     (WRK-KEY-PTID        =   JYURRK-PTID   )
                 AND  (WRK-KEY-SRYYMD      =   JYURRK-SRYYMD )
                 AND  (WRK-KEY-SRYKA       =   JYURRK-SRYKA  )
                   IF     (JYURRK-DENPNUM      =   WRK-KEY-DENPNUM)
                     OR  ((JYURRK-GRP-DENPNUM  NOT =   ZERO   )
                     AND  (WRK-KEY-GRPDENPNUM  =   JYURRK-GRP-DENPNUM))
                       MOVE    1               TO  FLG-OK
                   ELSE
                       MOVE    ZERO            TO  FLG-OK
                   END-IF
               ELSE
                   MOVE    ZERO            TO  FLG-OK
               END-IF
               IF      FLG-OK          =   ZERO
                   IF    IDX-TBL           >   ZERO
                   AND  (FLG-SYOHOU        =   1    )
      *                伝票番号毎処理
                       PERFORM 2002-HENSYU-SYORI-SEC
                   END-IF
                   INITIALIZE  TBL-SRY-AREA
                   MOVE    ZERO                TO  FLG-SYOHOU
                   INITIALIZE   LNK-WKSRYACT-AREA
                   MOVE    ZERO                TO  IDX-TBL
                   MOVE    JYURRK-PTID         TO  WRK-KEY-PTID
                   MOVE    JYURRK-SRYYMD       TO  WRK-KEY-SRYYMD
                   MOVE    JYURRK-DENPNUM      TO  WRK-KEY-DENPNUM
                   MOVE    JYURRK-GRP-DENPNUM  TO  WRK-KEY-GRPDENPNUM
                   MOVE    JYURRK-SRYKA        TO  WRK-KEY-SRYKA
                   MOVE    JYURRK-RENNUM       TO  TBL-JYURRK-RENNUM
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
           IF    IDX-TBL               >   ZERO
           AND  (FLG-SYOHOU        =   1    )
      *        伝票番号毎処理
               PERFORM 2002-HENSYU-SYORI-SEC
           END-IF
      *
           IF      CNT-LINE            >   ZERO
               PERFORM 400-PRINT-SEC
           END-IF
           .
       200-MAIN-EXT.
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
                          OR  (JYURRK-ZAINUM (IDXJ) =   ZERO )
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
      *        投薬とその他（処方せん）以外対象外
               IF      (SRY-SRYKBN         =   "21"
                                           OR  "22"
                                           OR  "23"
                                           OR  "98")
                  OR  (SRY-SRYSYUKBN       =   "820")
                   CONTINUE
               ELSE
                   MOVE    1                   TO  FLG-CHKIKBN
               END-IF
      *
               IF      FLG-SRYACT          =   ZERO
                 AND  (FLG-CHKIKBN         =   ZERO )
                   PERFORM 20011-SRYACCT-CHK-SEC
               END-IF
      *
               PERFORM
                       UNTIL  (FLG-SRYACT      =   1  )
                          OR  (FLG-CHKIKBN     =   1  )
      *
                   PERFORM 49011-SRYACT-HEN-SEC
      *
                   MOVE    "tbl_sryact"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 930-SRYACT-READ-SEC
               END-PERFORM
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-PERFORM
      *
           .
       2001-JYURRK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計チェック処理
      *****************************************************************
       20011-SRYACCT-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-ZAIKAISU
      *    剤回数取得
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE  SRYACCT-REC
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
      *    院外投薬のみ
           IF     (SRY-SRYKBN(1:1)     =   "2" )
             AND  (WRK-ZAIKAISU        >   ZERO )
               IF     (ACCT-ZAITEN     NOT =   ZERO )
                 OR   (SRY-SRYSYUKBN(3:1)  =   "3")
                   MOVE    1               TO  FLG-CHKIKBN
               END-IF
           END-IF
           IF     (WRK-ZAIKAISU        =   ZERO )
                MOVE    1               TO  FLG-CHKIKBN
           END-IF
      *@
      *    自保区分が１の場合、包括剤、薬評対象外
           IF     (ACCT-JIHOKBN        =   1   )
              OR  (ACCT-HKNCOMBI       =   "9999" )
              OR  (ACCT-ZAIKBN     NOT =  ZERO  )
                MOVE    1               TO  FLG-CHKIKBN
           END-IF
      *
           .
       20011-SRYACCT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスタを作成する 
      *****************************************************************
       49011-SRYACT-HEN-SEC           SECTION.
      *
           MOVE    SPACE               TO  WKSRYACT-REC
           INITIALIZE                  WKSRYACT-REC
           MOVE    SRY-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SRY-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SRY-PTID            TO  WKSRY-PTID
           MOVE    SRY-SRYKA           TO  WKSRY-SRYKA
           MOVE    JYURRK-SRYYMD       TO  WKSRY-SRYYMD
           MOVE    SRY-SRYKBN          TO  WKSRY-SRYKBN
           MOVE    SRY-ZAINUM          TO  WKSRY-ZAINUM
           MOVE    SRY-RENNUM          TO  WKSRY-RENNUM
           MOVE    SRY-SRYSYUKBN       TO  WKSRY-SRYSYUKBN
           MOVE    SRY-JIHIMONEYTOTAL  TO  WKSRY-JIHIMONEYTOTAL
      *剤回数計
           MOVE    WRK-ZAIKAISU        TO  WKSRY-ZAIKAIKEI
      *剤点数
           MOVE    ACCT-ZAITEN         TO  WKSRY-ZAITENKEI
      *保険組合
           MOVE    ACCT-HKNCOMBI       TO  WKSRY-HKNCOMBI
      *@
      *自保区分が１の場合、包括剤として継続区分に'H'を編集
           IF      ACCT-JIHOKBN        =   1
               MOVE    "H"                 TO  WKSRY-CONTKBN
           END-IF
      *    剤内容（薬評）
           MOVE    ACCT-ZAIKBN         TO  WKSRY-ZAIKBN
      *
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING     IDS2     FROM    1   BY  1
                   UNTIL      (IDS2         >   5   )   OR
                              (SRY-SRYCD (IDS2)     =   SPACE )
      *        約束セットは内容のみ
               IF      SRY-SRYCD (IDS2)(1:1)     =   "S"
                   CONTINUE
               ELSE
      *
               ADD     1                   TO  IDX2
               MOVE    SRY-SRYCD (IDS2)     TO  WKSRY-SRYCD    (IDX2)
               MOVE    SRY-SRYSURYO (IDS2)  TO  WKSRY-SRYSURYO (IDX2)
               MOVE    SRY-SRYKAISU (IDS2)  TO  WKSRY-SRYKAISU (IDX2)
               MOVE    SRY-AUTOKBN  (IDS2)  TO  WKSRY-AUTOKBN  (IDX2)
               MOVE    SRY-INPUTNUM (IDS2)  TO  WKSRY-INPUTNUM (IDX2)
               MOVE    SRY-JIHIMONEY(IDS2)  TO  WKSRY-JIHIMONEY (IDX2)
      *        IF      SRY-INPUTNUM (IDS2)  NOT =   ZERO
      *            PERFORM 490111-PTCOM-SET-SEC
      *        END-IF
      *!!
      *       器材商品名対応
              IF       SRY-MEISKYFLG (IDS2) =   "1"
                   MOVE    "T"             TO  WKSRY-AUTOKBN  (IDX2)
               END-IF
      *!!
      *ver.4.5
      *        換算値
               MOVE    SRY-KANSURYO (IDS2)  TO  WKSRY-KANSURYO (IDX2)
      *        関連コメント指示
               IF       SRY-MEISKYFLG (IDS2) =   "2"
                   MOVE    "1"             TO  WKSRY-INPUTKBN (IDX2)
               END-IF
      *        内服種類数指示
               IF       SRY-MEISKYFLG (IDS2) =   "3"
                   MOVE    "2"             TO  WKSRY-INPUTKBN (IDX2)
               END-IF
      *処方せん料あり
               IF      SRY-SRYCD (IDS2)     =   "120003710"
      *            その他
                                           OR  "120002910"
      *            ７種類以上 
                                           OR  "120002710"
                   MOVE    1                   TO  FLG-SYOHOU
               END-IF
      *投薬で薬剤
               IF     (SRY-SRYCD (IDS2)(1:1)    =   "6" )
                   MOVE    1                   TO  FLG-YAKUZAI
               END-IF
             END-IF
      *?       MOVE    WRK-ZAIKAISU        TO  WKSRY-ZAIKAISU (IDX)
      *
      *        MOVE    IDX                 TO  IDX-W
      *        ADD     1                   TO  IDX-KAI
           END-PERFORM
      *
      *??  MOVE    WRK-ZAIKAISU        TO  WKSRY-ZAIKAISU (IDX-W)
           ADD     1                   TO  IDX-TBL
           MOVE    WKSRYACT-REC        TO  LNK-WKSRYACT-REC (IDX-TBL)
           MOVE    IDX-TBL             TO  LNK-WKSRYACT-MAX
      *
           .
       49011-SRYACT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    投薬内容チェック処理
      *****************************************************************
       2002-HENSYU-SYORI-SEC       SECTION.
      *
           MOVE    LNK-WKSRY-SRYYMD  (1)
                                      TO  SPA-SRYYMD
      *    初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMDG
           MOVE    "1"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    LNK-WKSRY-PTID(1)  TO  SPA-PTID
           MOVE    LNK-WKSRY-SRYKA(1) TO  SPA-SRYKA
           MOVE    LNK-WKSRY-HKNCOMBI(1)
                                      TO  SPA-HKNCOMBINUM
      *    患者マスター読み込み
           MOVE    SPACE               TO  PTINF-REC
           INITIALIZE  PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   1
               MOVE    "20000401"          TO  SPA-BIRTHDAY
           ELSE
               MOVE    PTINF-BIRTHDAY      TO  SPA-BIRTHDAY
           END-IF
      *
           MOVE    400                 TO  SPA-MAX-LINE
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   400      )   OR
                              (IDX         >   LNK-WKSRYACT-MAX  )
               MOVE    ZERO                TO  IDXS
               PERFORM     VARYING   IDY   FROM    1      BY  1
                           UNTIL    (IDY   >   5  )
                      OR  (LNK-WKSRY-SRYCD  (IDX IDY) =   SPACE )
                       PERFORM 10020-SPA-HEN-SEC
               END-PERFORM
      *        剤の最後に回数入力をセット
               IF     (IDXS                >   ZERO  ) 
                   IF     (IDX                 =   LNK-WKSRYACT-MAX ) OR
                          ((IDX                <   400  ) AND
                           (LNK-WKSRY-ZAINUM (IDX + 1)
                                           NOT =   LNK-WKSRY-ZAINUM
                                                                (IDX)))
                       IF      IDX2            <=  SPA-MAX-LINE
                           MOVE    "1"             TO  SPA-COM-KAIFLG
                                                               (IDX2)
                           MOVE    "1"             TO  SPA-COM-ZAIEND
                                                               (IDX2)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *    院外
           MOVE    "1"             TO  SPA-INGAIKBN
           MOVE    1               TO  SPA-SYORIFLG
           MOVE    400             TO  SPA-MAX-LINE
      *    約束コード以外の画面再編集
           PERFORM VARYING     IDX FROM    1       BY  1
                   UNTIL      (IDX     >   400 )  OR
                             ((SPA-GMN-INPUTCD(IDX) =   SPACE )
                          AND (SPA-COM-INPUTCD(IDX) =   SPACE ))
               IF     (SPA-GMN-INPUTCD(IDX)(1:1)   =   ".")
                   CONTINUE
               ELSE
      *            点数マスタ編集
                   INITIALIZE                  ORCSC92AREA
                   MOVE    SPACE               TO  LNK-SC92-KBN
                   MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
                   MOVE    "K02"               TO  LNK-SC92-PG
                   MOVE    IDX                 TO  LNK-SC92-GMN-IDX
                   MOVE    SPA-COM-INPUTCD(IDX)
                                               TO  LNK-SC92-SRYCD
                   MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
                   MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
                   MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
                   MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
                   MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
                   MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
                   CALL    "ORCSC92"           USING
                                               MCPAREA
                                               ORCSC92AREA
                                               SPA-SCR-REC
                                               SPA-AREA
                   IF     (SPA-ERRCD           NOT =   SPACE )  OR
                          (LNK-SC92-ERRAREA    NOT =   SPACE )
                       MOVE    SPACE           TO  SPA-ERRCD
                       MOVE    "1"             TO  SPA-COM-CUR (IDX)
                   END-IF
               END-IF
      *
      *        IF      SPA-COM-ZAIEND (IDX)  =   "1"
      *            MOVE    ZERO                TO  IDX-TEI
      *        END-IF
      *        最終行
               MOVE    IDX               TO  IDX2
           END-PERFORM
      *    投薬の登録処理のみ
           PERFORM 3000-JIDOU-SYORI-SEC
      *
           .
       2002-HENSYU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   再表示あり　編集処理
      *****************************************************************
       10020-SPA-HEN-SEC              SECTION.
      *
      *    診療種別セット
           IF     (IDXS                    =   ZERO)  AND
                  (LNK-WKSRY-RENNUM (IDX)  =   1   )
               IF      LNK-WKSRY-SRYSYUKBN (IDX)   =   SPACE
                   MOVE    LNK-WKSRY-SRYKBN (IDX)  TO
                                       LNK-WKSRY-SRYSYUKBN (IDX)(1:2)
                   MOVE    "0"         TO
                                       LNK-WKSRY-SRYSYUKBN (IDX)(3:1)
               END-IF
               ADD     1                   TO  IDX2
               MOVE    "."                 TO  SPA-GMN-INPUTCD
                                                            (IDX2)(1:1)
               MOVE    LNK-WKSRY-SRYSYUKBN (IDX)
                                           TO  SPA-GMN-INPUTCD
                                                           (IDX2)(2:3)
               MOVE    LNK-WKSRY-SRYSYUKBN (IDX)
                                           TO  SPA-COM-SRYSYUKBN
                                                              (IDX2)
               MOVE    LNK-WKSRY-SRYKBN (IDX)
                                           TO  SPA-COM-SRYKBN(IDX2)
               MOVE    LNK-WKSRY-SRYKBN (IDX)
                                           TO  SPA-GMN-SRYKBN(IDX2)
           END-IF
      *
      *    明細編集
           ADD     1                   TO  IDX2
           IF      IDX2                >   400
    ****        MOVE    "9100"         TO  WRK-ERRCD1
                GO      TO      10020-SPA-HEN-EXT
           END-IF
      *
           MOVE    LNK-WKSRY-HKNCOMBI(IDX)
                                       TO  SPA-COM-HKNCOMBI (IDX2)
      *
           MOVE    LNK-WKSRY-SRYKBN(IDX)
                                       TO  SPA-COM-SRYKBN  (IDX2)
           MOVE    LNK-WKSRY-SRYKBN(IDX)
                                       TO  SPA-GMN-SRYKBN  (IDX2)
           MOVE    LNK-WKSRY-SRYSYUKBN(IDX)
                                       TO  SPA-COM-SRYSYUKBN  (IDX2)
           MOVE    LNK-WKSRY-SRYCD (IDX IDY)
                                       TO  SPA-COM-INPUTCD (IDX2)
           MOVE    LNK-WKSRY-SRYSURYO(IDX IDY)
                                       TO  SPA-COM-SURYO   (IDX2)
           MOVE    LNK-WKSRY-SRYSURYO(IDX IDY)
                                       TO  SPA-COM-SURYO2  (IDX2)
           MOVE    LNK-WKSRY-AUTOKBN (IDX IDY)
                                       TO  SPA-COM-AUTOKBN  (IDX2)
           IF      LNK-WKSRY-AUTOKBN (IDX IDY)
                                       =   "A" OR "B" OR "C"  OR  "D"
                                       OR  "E" OR "F" OR "G"  OR  "H"
               MOVE    SPACE               TO  SPA-COM-AUTOKBN (IDX2)
           END-IF
      *!!
      *    器材商品名対応
           IF      LNK-WKSRY-AUTOKBN (IDX IDY)
                                       =   "T"
               MOVE    "1"             TO  SPA-COM-AUTOKBN2(IDX2)
               MOVE    SPACE           TO  SPA-COM-AUTOKBN (IDX2)
           END-IF
      *    数量入力時
           IF      LNK-WKSRY-AUTOKBN (IDX IDY)
                                       =   "S"
               MOVE    "1"                 TO  SPA-COM-SURFLG  (IDX2)
               MOVE    SPACE               TO  SPA-COM-AUTOKBN (IDX2)
           END-IF
      *    訂正時登録時の区分
      *    IF      ORCSCWKSRY-SYORIFLG =   ZERO
               MOVE    SPA-COM-AUTOKBN (IDX2)
                                       TO  SPA-COM-MAE-AUTOKBN  (IDX2)
      *    END-IF
      *
      *    IF      FLG-AUTO-OK         =   1
      *        MOVE    SPACE               TO  SPA-COM-AUTOKBN (IDX2)
      *    END-IF
      *@
      *    包括剤の設定
           IF      LNK-WKSRY-CONTKBN (IDX) =   "H"
               MOVE    1               TO  SPA-COM-HOUKATU-FLG(IDX2)
               MOVE    1               TO  SPA-COM-HOUKATU-ZAI(IDX2)
      *        包括診療行為チェックをオンにする
               IF      SPA-HKTSANTEI-FLG   =   ZERO
                   MOVE   1                TO  SPA-HKTSANTEI-FLG
               END-IF
           END-IF
      *@
      *
           MOVE    LNK-WKSRY-INPUTCHI-G (IDX IDY)
                                       TO  SPA-COM-INPUTCHI-G  (IDX2)
      *
      *    回数を編集
           MOVE    LNK-WKSRY-ZAIKAIKEI(IDX) TO  SPA-COM-KAISU(IDX2)
           MOVE    LNK-WKSRY-ZAIKAIKEI(IDX) TO  SPA-COM-KAISU2(IDX2)
           MOVE    IDX2                TO  IDXS
      *    分画数を編集
           IF      LNK-WKSRY-SRYKBN (IDX)  =   "70"
               MOVE    LNK-WKSRY-SRYKAISU (IDX IDY)
                                       TO  SPA-COM-BUNGSU (IDX2)
           END-IF
      *!!
      *    内服１種類区分
           IF     (SPA-COM-INPUTCD (IDX2)(1:1) =   "6"  )  AND
                  (LNK-WKSRY-INPUTKBN(IDX IDY) =   "2"  )
                MOVE    "1"                TO  SPA-COM-INPUTNAIF (IDX2)
                MOVE    "1"                TO  SPA-COM-TANDKKBN  (IDX2)
           END-IF
      *    コメントの継続
           IF     (SPA-COM-INPUTCD (IDX2)(1:1) =   "8"  )  OR
                  (SPA-COM-INPUTCD (IDX2)(1:3) =   "008")
               IF     (LNK-WKSRY-INPUTKBN (IDX IDY)    =   "1" )
                   MOVE    "1"             TO  SPA-COM-INPUTCONT (IDX2)
               END-IF
           END-IF
      *!!
      *!!
      *    外用の回数を編集
           IF      LNK-WKSRY-SRYKBN (IDX)  =   "23"
               MOVE    LNK-WKSRY-SRYKAISU (IDX IDY)
                                       TO  SPA-COM-KAISU2(IDX2)
               IF      SPA-COM-KAISU2(IDX2)    =   ZERO
                   MOVE    1               TO  SPA-COM-KAISU2(IDX2)
               END-IF
      *        数量の再計算
               IF     (SPA-COM-KAISU2  (IDX2)  >   1    )  AND
                     ((SPA-COM-INPUTCD (IDX2)(1:1) =   "6"
                                                   OR  "7" ) OR
                      (SPA-COM-INPUTCD (IDX2)(1:3) =   "059") OR
                      (SPA-COM-INPUTCD (IDX2)(1:1) =   "S"  ))
                   COMPUTE SPA-COM-SURYO2  (IDX2)
                                       =   SPA-COM-SURYO2  (IDX2)
                                       /   SPA-COM-KAISU2(IDX2)
               END-IF
               IF      LNK-WKSRY-ZAIKAIKEI(IDX) >   1
                   COMPUTE SPA-COM-KAISU2  (IDX2)
                                       =   SPA-COM-KAISU2  (IDX2)
                                       *   LNK-WKSRY-ZAIKAIKEI
                                                           (IDX)
               END-IF
               MOVE    SPA-COM-KAISU2  (IDX2)
                                       TO  SPA-COM-KAISU (IDX2)
               MOVE    SPA-COM-SURYO2  (IDX2)
                                       TO  SPA-COM-SURYO (IDX2)
           END-IF
      *    薬評・減点
           IF      LNK-WKSRY-ZAIKBN  (IDX) =   1
               MOVE    "1"                 TO  SPA-COM-YAKUHYO (IDX2)
           END-IF
           IF      LNK-WKSRY-ZAIKBN  (IDX) =   2
               MOVE    "1"                 TO  SPA-COM-GENTEN  (IDX2)
           END-IF
      *!!
           IF      LNK-WKSRY-INPUTCD (IDX IDY) =   SPACE
               MOVE    SPA-COM-INPUTCD (IDX2)
                                           TO  SPA-GMN-INPUTCD (IDX2)
           ELSE
               MOVE    LNK-WKSRY-INPUTCD (IDX IDY)
                                           TO  SPA-GMN-INPUTCD (IDX2)
           END-IF
           MOVE    SPA-GMN-INPUTCD (IDX2)  TO  SPA-MAE-INPUTCD (IDX2)
      *
      *    剤点数の設定（訂正前）
           MOVE    LNK-WKSRY-ZAITENKEI(IDX)
                                       TO  SPA-COM-MAE-TENSU (IDX2)
           .
       10020-SPA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動算定発生処理
      *****************************************************************
       3000-JIDOU-SYORI-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-NAIYAK
           MOVE    ZERO                TO  FLG-TONPUK
           MOVE    ZERO                TO  FLG-GAIYAK
           MOVE    ZERO                TO  FLG-NAIYAK2
           MOVE    ZERO                TO  FLG-TONPUK2
           MOVE    ZERO                TO  FLG-GAIYAK2
           MOVE    ZERO                TO  IDX-SYOHOU
      *
           MOVE    ZERO                TO  FLG-ZAIEND
           MOVE    ZERO                TO  IDX-IP
           MOVE    ZERO                TO  FLG-IPPANKSN
      *
           MOVE    ZERO                TO  FLG-GENERIC-OK
      *H28.4
           MOVE    ZERO                TO  FLG-GENERIC-NO
           MOVE    ZERO                TO  CNT-GENERIC
      *#######
           MOVE    ZERO                TO  FLG-GENERIC-NO2
           MOVE    ZERO                TO  CNT-GENERIC2
           MOVE    ZERO                TO  CNT-GENERIC3
           MOVE    ZERO                TO  FLG-GENERIC-OK12
      *
           MOVE    400                 TO  SPA-MAX-LINE
      *
           MOVE    SPACE               TO  WRK-GENECHG-KBN
      *R01.5
      *    一般名加算チェック
           INITIALIZE                  TBL-GENERIC-CHEK-AREA
      *
      *    診療科毎に後発変更不可処方毎の設定
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                             ((SPA-GMN-INPUTCD(IDX) =   SPACE) AND
                              (SPA-COM-INPUTCD(IDX) =   SPACE))
               IF      SPA-COM-INPUTCD (IDX)   =  "099209910"
                   MOVE    "1"                 TO  WRK-GENECHG-KBN
               END-IF
               IF      SPA-COM-INPUTCD (IDX)   =  "099209911"
                   MOVE    "2"                 TO  WRK-GENECHG-KBN
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                             ((SPA-GMN-INPUTCD(IDX) =   SPACE) AND
                              (SPA-COM-INPUTCD(IDX) =   SPACE))
      *
      *        診療種別区分変換
               PERFORM 30001-SRYSYUKBN-SET-SEC
      *        投薬の時、薬剤コードがあること
      *        IF     (WRK-CHK-SRYSYUKBN(1:2)  =   "21"
      *                                        OR  "22"
      *                                        OR  "23"
      *                                        OR  "29" )  AND
      *                (FLG-ZAIEND             =   ZERO )
      *            PERFORM 30001-TOUYAKU-CHK-SEC
      *            MOVE    1                   TO  FLG-ZAIEND
      *        END-IF
      *
               EVALUATE    WRK-CHK-SRYSYUKBN
                   WHEN    "211"
                   WHEN    "291"
                       MOVE     1              TO  FLG-NAIYAK
                   WHEN    "212"
                   WHEN    "292"
                       MOVE     1              TO  FLG-NAIYAK2
                   WHEN    "221"
                   WHEN    "297"
                       MOVE     1              TO  FLG-TONPUK
                   WHEN    "222"
                   WHEN    "298"
                       MOVE     1              TO  FLG-TONPUK2
                   WHEN    "231"
                   WHEN    "294"
                       MOVE     1              TO  FLG-GAIYAK
                   WHEN    "232"
                   WHEN    "295"
                       MOVE     1              TO  FLG-GAIYAK2
               END-EVALUATE
      *
      *     処方のみの時以下、算定なし
               IF     (WRK-CHK-SRYSYUKBN(1:1)  NOT =   "2"  ) OR
                      (WRK-CHK-SRYSYUKBN       =   "213" OR
                                                   "223" OR
                                                   "233"  )
                   CONTINUE
               ELSE
                   IF      SPA-SRYYMD          >=  "20170317"
      *
      *            処方内容チェク
                   PERFORM 3000-SYOHOU02-CHK-SEC
      *
                   ELSE
      *            処方内容チェク
                   PERFORM 3000-SYOHOU-CHK-SEC
      *
                   END-IF
               END-IF
      *
      *H24.4　改正
      *        一般名処方加算（処方せん料）
               IF      SPA-COM-INPUTCD (IDX)   =   "120003570"
                                               OR  "120004270"
                   IF      FLG-IPPANKSN        =   ZERO
                       MOVE    1                   TO  FLG-IPPANKSN
                       MOVE    IDX                 TO  IDX-IP
                   ELSE
                       MOVE    "1021"          TO  SPA-ERRCD
                       MOVE    "1"             TO  SPA-COM-CUR (IDX)
                   END-IF
               END-IF
               IF      SPA-COM-ZAIEND (IDX)    =   "1"
                   MOVE    ZERO                TO  FLG-ZAIEND
               END-IF
      *
           END-PERFORM
      *
      *H29.4
      *    一般名加算算定編集
           IF      SPA-SRYYMD          >=  "20170317"
               IF      (FLG-GENERIC-OK     =   1   )
                   OR ((CNT-GENERIC        >=  2   )  AND
                       (FLG-GENERIC-NO     =   ZERO))
                   MOVE    1               TO  FLG-GENERIC-OK12
               END-IF
      *        4/1までは、３，４で２つ以上あれば対象
               IF      (SPA-SRYYMD         <   "20170401")
                   AND (FLG-GENERIC-NO2    =   ZERO )
                   AND (CNT-GENERIC3       >=  2    )
                   MOVE    1               TO  FLG-GENERIC-OK12
               END-IF
           ELSE
               IF      (FLG-GENERIC-OK     =   1   )
                   MOVE    1               TO  FLG-GENERIC-OK12
               END-IF
           END-IF
      *
           MOVE    ZERO            TO  FLG-ERRKBN
      *    一般名加算の対象チェック
           IF    ((FLG-NAIYAK2         =   1    )  OR
                  (FLG-TONPUK2         =   1    )  OR
                  (FLG-GAIYAK2         =   1    ))
            IF      SPA-SRYYMD          >=  "20170317"
      *H29.3.17　以降
               PERFORM 30002-KSNCHK-SEC
      *
            ELSE
      *H29.3.17以前
      *     加算入力あり
            IF     (FLG-IPPANKSN        =   1   )
      *     一般名加算と院外投薬あり
               IF      FLG-GENERIC-OK      =   ZERO
      *            一般名加算投薬なし
                   MOVE    1                   TO  FLG-ERRKBN
      *
               ELSE
      *            加算のチェック
                   IF     (FLG-GENERIC-NO      =   1 )
                      OR  (CNT-GENERIC         <   2 )
      *                一般名表示なし有
                       IF      SPA-COM-INPUTCD (IDX-IP)
                                              NOT =   "120003570"
                           MOVE    2                   TO  FLG-ERRKBN
                       END-IF
                   ELSE
      *                すべて一般名
                       IF      SPA-COM-INPUTCD (IDX-IP)
                                              NOT =   "120004270"
                           MOVE    3                   TO  FLG-ERRKBN
                       END-IF
                   END-IF
               END-IF
            ELSE
      *        加算入力なし
               IF      FLG-GENERIC-OK      =   1
                   IF     (FLG-GENERIC-NO      =   1 )
                      OR  (CNT-GENERIC         <   2 )
      *               加算１
                       MOVE    2               TO  FLG-ERRKBN
                   ELSE
      *               加算2
                       MOVE    3               TO  FLG-ERRKBN
                   END-IF
               END-IF
             END-IF
           END-IF
      *
           END-IF
      *
           IF      WRK-PARA-SYORIKBN   =   "1"
      *        全件表示
               CONTINUE
           ELSE
      *        警告対象のみ
               IF      FLG-ERRKBN          =   ZERO
                   GO      TO  3000-JIDOU-SYORI-EXT
               END-IF
           END-IF
      *    印刷処理
           PERFORM 20020-PRINT-SYORI-SEC
           .
       3000-JIDOU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療種別区分変換処理
      *****************************************************************
        30002-KSNCHK-SEC              SECTION.
      *
      *     加算入力あり
            IF     (FLG-IPPANKSN        =   1   )
      *     一般名加算と院外投薬あり
               IF      FLG-GENERIC-OK12     =   ZERO
      *            一般名加算投薬なし
                   MOVE    1                   TO  FLG-ERRKBN
               ELSE
      *H29.4
                   IF    ((FLG-GENERIC-NO      =   ZERO )
                      AND (CNT-GENERIC         >=  2    ))
                   OR    ((SPA-SRYYMD          <   "20170401")
                      AND (SPA-SRYYMD          >=  "20170317")
                      AND (FLG-GENERIC-NO2     =   ZERO )
                      AND (CNT-GENERIC2        >=  2    ))
                   OR    ((SPA-SRYYMD          <   "20170401")
                      AND (SPA-SRYYMD          >=  "20170317")
                      AND (FLG-GENERIC-NO2    =   ZERO  )
                      AND (CNT-GENERIC3        >=  2    ))
      *                一般名加算１
                       IF      SPA-COM-INPUTCD (IDX-IP)
                                              NOT =   "120004270"
                           MOVE    3                   TO  FLG-ERRKBN
                       END-IF
                   ELSE
      *                一般名加算２
                       IF      SPA-COM-INPUTCD (IDX-IP)
                                              NOT =   "120003570"
                           MOVE    2                   TO  FLG-ERRKBN
                       END-IF
                   END-IF
               END-IF
           ELSE
      *        加算入力なし
               IF      FLG-GENERIC-OK      =   1
                   IF    ((FLG-GENERIC-NO      =   ZERO )
                      AND (CNT-GENERIC         >=  2    ))
                   OR    ((SPA-SRYYMD          <   "20170401")
                      AND (SPA-SRYYMD          >=  "20170317")
                      AND (FLG-GENERIC-NO2     =   ZERO )
                      AND (CNT-GENERIC2        >=  2    ))
                   OR    ((SPA-SRYYMD          <   "20170401")
                      AND (SPA-SRYYMD          >=  "20170317")
                      AND (FLG-GENERIC-NO2    =   ZERO  )
                      AND (CNT-GENERIC3        >=  2    ))
      *                一般名加算１
                       MOVE    3               TO  FLG-ERRKBN
                   ELSE
      *                加算１
                       MOVE    2               TO  FLG-ERRKBN
                   END-IF
               END-IF
           END-IF
           .
        30002-KSNCHK-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療種別区分変換処理
      *****************************************************************
       30001-SRYSYUKBN-SET-SEC              SECTION.
      *
           MOVE    SPA-COM-SRYSYUKBN (IDX) TO  WRK-CHK-SRYSYUKBN
      *
           IF      WRK-CHK-SRYSYUKBN       =   SPACE
               EVALUATE    SPA-COM-SRYKBN (IDX)
                   WHEN    "21"
                       MOVE    "210"       TO  WRK-CHK-SRYSYUKBN
                   WHEN    "22"
                       MOVE    "220"       TO  WRK-CHK-SRYSYUKBN
                   WHEN    "23"
                       MOVE    "230"       TO  WRK-CHK-SRYSYUKBN
               END-EVALUATE
           END-IF
      *
      *    院外
               EVALUATE    WRK-CHK-SRYSYUKBN
                   WHEN    "210"
                       MOVE    "212"          TO  WRK-CHK-SRYSYUKBN
                   WHEN    "290" 
                       MOVE    "292"          TO  WRK-CHK-SRYSYUKBN
                   WHEN    "220"
                       MOVE    "222"          TO  WRK-CHK-SRYSYUKBN
                   WHEN    "230" 
                       MOVE    "232"          TO  WRK-CHK-SRYSYUKBN
      *H26.4
                   WHEN    "293"
                       MOVE    "295"          TO  WRK-CHK-SRYSYUKBN
      *H26.10      頓服臨時
                   WHEN    "296"
                       MOVE    "298"          TO  WRK-CHK-SRYSYUKBN
               END-EVALUATE
          .
       30001-SRYSYUKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   処方内容チェク処理
      *****************************************************************
       3000-SYOHOU-CHK-SEC              SECTION.
      *
           IF      SPA-COM-INPUTCD(IDX)(1:1)   =   "6"
      *
      ****     MOVE    SPA-COM-KISOTEN(IDX)    TO  WRK-KEI-KISOTEN
      *
               EVALUATE    WRK-CHK-SRYSYUKBN
      *平成２４年４月
                   WHEN    "212"
                   WHEN    "222"
                   WHEN    "232"
                   WHEN    "292"
                   WHEN    "295"
                   WHEN    "298"
      *                院外処方　一般名記載判定
                       MOVE    ZERO                TO  FLG-CHK-P
                       IF     (SPA-COM-IPN-KISAIKBN (IDX)  =   1  )
      *H27.4               後発品変更可
                         OR   (WRK-GENECHG-KBN             =   "2" )
      *
                           MOVE    1                   TO  FLG-CHK-P
      *H27.4               後発品変更不可
                           IF      WRK-GENECHG-KBN     =   "1"
                              MOVE    ZERO                TO  FLG-CHK-P
                           END-IF
      *
                           IF      (SPA-COM-INPUTCD (IDX + 1)
                                                   =   "099209907"
                                                   OR  "099209903"  )
      *                        銘柄名記載、後発品変更不可
                               MOVE    ZERO                TO  FLG-CHK-P
                           END-IF
                       END-IF
                       IF     ( SPA-COM-INPUTCD (IDX + 1)
                                                   =   "099209908"  )
      *                 一般名記載あり
                           MOVE    1                   TO  FLG-CHK-P
                       END-IF
      *H28.4           後発品　一般名記載有無件数
                       IF      (FLG-CHK-P          =   1   )  AND
                               (SPA-COM-KOUHATUKBN (IDX) =   "1"
                                                         OR  "2" )
                           MOVE    1               TO  FLG-GENERIC-OK
                           MOVE    "1"             TO  SPA-COM-YOBI1
                                                          (IDX)(1:1)
                       END-IF
                       IF      (SPA-COM-KOUHATUKBN (IDX) =   "1"
                                                         OR  "2" )
                           ADD     1               TO  CNT-GENERIC
                           IF      (FLG-CHK-P      =   ZERO)
                               MOVE    1           TO  FLG-GENERIC-NO
                           END-IF
                       END-IF
               END-EVALUATE
           END-IF
           .
       3000-SYOHOU-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *   処方内容チェク処理（2017/3/17から）
      *****************************************************************
       3000-SYOHOU02-CHK-SEC              SECTION.
      *
           IF      SPA-COM-INPUTCD(IDX)(1:1)   =   "6"
      *
      ****     MOVE    SPA-COM-KISOTEN(IDX)    TO  WRK-KEI-KISOTEN
      *
               EVALUATE    WRK-CHK-SRYSYUKBN
      *平成２４年４月
                   WHEN    "212"
                   WHEN    "222"
                   WHEN    "232"
                   WHEN    "292"
                   WHEN    "295"
                   WHEN    "298"
      *                院外処方　一般名記載判定
                       MOVE    ZERO                TO  FLG-CHK-P
                       IF     (SPA-COM-IPN-KISAIKBN (IDX)  =   1  )
      *H27.4               後発品変更可
                         OR   (WRK-GENECHG-KBN             =   "2" )
      *
                           MOVE    1                   TO  FLG-CHK-P
      *H27.4               後発品変更不可
                           IF      WRK-GENECHG-KBN     =   "1"
                              MOVE    ZERO                TO  FLG-CHK-P
                           END-IF
      *
                           IF      (SPA-COM-INPUTCD (IDX + 1)
                                                   =   "099209907"
                                                   OR  "099209903"  )
      *                        銘柄名記載、後発品変更不可
                               MOVE    ZERO                TO  FLG-CHK-P
                           END-IF
                       END-IF
                       IF     ( SPA-COM-INPUTCD (IDX + 1)
                                                   =   "099209908"  )
      *                 一般名記載あり
                           MOVE    1                   TO  FLG-CHK-P
                       END-IF
      *R01.5
      *                同一薬剤判定
                       IF      SPA-SRYYMD          >=  "20180401"
                           PERFORM 300011-IPN-JYUFU-SEC
                       ELSE
                           MOVE    1            TO  FLG-IPKSNOK
                       END-IF
      *R01.5
      *                同一薬剤は対象外
                       IF      FLG-IPKSNOK         =   1
      *
      *H28.4           後発品　一般名記載有無件数
                       IF      (FLG-CHK-P          =   1   )  AND
                               (SPA-COM-KOUHATUKBN (IDX) =   "1"
                                                         OR  "2" )
                           MOVE    1               TO  FLG-GENERIC-OK
                       END-IF
                       IF      (SPA-COM-KOUHATUKBN (IDX) =   "1"
                                                         OR  "2" )
                           ADD     1               TO  CNT-GENERIC
                           ADD     1               TO  CNT-GENERIC2
                           IF      (FLG-CHK-P      =   ZERO)
                               MOVE    1           TO  FLG-GENERIC-NO
                               MOVE    1           TO  FLG-GENERIC-NO2
                           ELSE
                               ADD     1           TO  CNT-GENERIC3
                           MOVE    "1"             TO  SPA-COM-YOBI1
                                                          (IDX)(1:1)
                           END-IF
                       END-IF
      *H29.3
      *H29.4から       先発なし後発医薬品（加算対象）判定
                       IF      (SPA-COM-KOUHATUKBN (IDX) =   "3"
                                                         OR  "4"  )
                          AND  (SPA-SRYYMD         >=   "20170317")
                           ADD     1               TO  CNT-GENERIC
                           IF      (FLG-CHK-P      =   ZERO)
                               MOVE    1           TO  FLG-GENERIC-NO
                           ELSE
                               ADD     1           TO  CNT-GENERIC3
                           MOVE    "1"             TO  SPA-COM-YOBI1
                                                          (IDX)(1:1)
                           END-IF
                       END-IF
      *R01.5
                       END-IF
               END-EVALUATE
           END-IF
           .
       3000-SYOHOU02-CHK-EXT.
           EXIT.
      *
      *
      *R01.5
      *****************************************************************
      *    一般名処方加算　同一薬剤判定処理
      *****************************************************************
       300011-IPN-JYUFU-SEC         SECTION.
      *
      *    同じ薬剤コードは１件とする
           IF      (FLG-CHK-P          =   ZERO)
      *        銘柄名
               MOVE    2               TO  IDXG1
           ELSE
      *        一般名
               MOVE    1               TO  IDXG1
           END-IF
      *
           MOVE    ZERO                TO  FLG-IPKSNOK
           MOVE    ZERO                TO  FLG-IPKSNEND
           PERFORM VARYING     IDXG2   FROM    1   BY  1
                   UNTIL      (IDXG2   >   200 )
                          OR  (FLG-IPKSNOK     =   1  )
                          OR  (FLG-IPKSNEND    =   1  )
               IF      SPA-COM-INPUTCD (IDX)
                                       =   TBL-GEN-SRYCD
                                                      (IDXG1 IDXG2)
                   MOVE    200             TO  IDXG2
                   MOVE    1               TO  FLG-IPKSNEND
               ELSE
                   IF      TBL-GEN-SRYCD (IDXG1 IDXG2)  =   SPACE
      *                テーブル編集
                       MOVE    SPA-COM-INPUTCD (IDX)
                                             TO  TBL-GEN-SRYCD
                                                      (IDXG1 IDXG2)
                       MOVE    SPA-COM-GECODE (IDX)
                                             TO  TBL-GEN-GECODE
                                                      (IDXG1 IDXG2)
                       MOVE    1             TO  FLG-IPKSNOK
                   ELSE
      *            一般名コードがあれば判定
                       IF    (SPA-COM-GECODE (IDX)
                                         NOT =   SPACE )
                         AND (SPA-COM-GECODE (IDX)
                                             =   TBL-GEN-GECODE
                                                      (IDXG1 IDXG2))
                           MOVE    200             TO  IDXG2
                           MOVE    1               TO  FLG-IPKSNEND
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .
       300011-IPN-JYUFU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       20020-PRINT-SYORI-SEC       SECTION.
      *
      *    明細編集
           MOVE    SPACE               TO  LST-PRT-BODY-01
      *    患者変更なし
           IF      WRK-MAE-PTID    NOT =   SPA-PTID
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
           IF     (WRK-MAE-PTID     NOT =   SPA-PTID )
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
           IF      WRK-MAE-PTID    NOT =   SPA-PTID
               ADD     1                   TO  CNT-LINE
           END-IF
      *
           MOVE    SPA-PTID            TO  WRK-MAE-PTID
      *
      *    明細編集
           MOVE    SPACE               TO  LST-PRT-BODY-02
           MOVE    SPA-SRYYMD          TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  PRT-SRYYMD
      *    診療科編集
           PERFORM 200201-SRYKA-HEN-SEC
           MOVE    SPA-HKNCOMBINUM     TO  PRT-HKNCOMBI
           MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBI
      *
      *    明細編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   400           )  OR
                             ((SPA-GMN-INPUTCD(IDX) =   SPACE) AND
                              (SPA-COM-INPUTCD(IDX) =   SPACE))
               MOVE    ZERO                TO  FLG-CHK
               IF     (SPA-COM-SRYKBN (IDX) (1:1)   =   "2" )
                 AND ((SPA-COM-INPUTCD (IDX)(1:1)   =   "6" )
                  OR  (SPA-COM-INPUTCD (IDX)        =   "099209903"
                                                    OR  "099209907"
                                                    OR  "099209908"))
                   MOVE    1               TO  FLG-CHK
               END-IF
               IF     (SPA-COM-SRYKBN (IDX)         =   "98")  AND
                      (SPA-COM-INPUTCD (IDX)        =   "099209910")
                   MOVE    1               TO  FLG-CHK
               END-IF
      *名称の☆削除
      *    名称
               IF     (SPA-COM-INPUTCD (IDX)(1:1)    =   "6"   )
                 AND  (SPA-COM-PSY-CLASS(IDX)    NOT =   SPACE )
                 AND  (SPA-GMN-MEISYO(IDX)(1:2)  =   "☆"      )
                   MOVE    SPA-GMN-MEISYO(IDX)(3:)   TO  WRK-MEISYO
                   MOVE    WRK-MEISYO            TO  SPA-GMN-MEISYO(IDX)
               END-IF
      *
               IF      FLG-CHK             =   1
      *保険組合せ
                   IF      WRK-HKNCOMBI    NOT =   SPA-COM-HKNCOMBI(IDX)
                       MOVE    SPA-COM-HKNCOMBI(IDX)   TO  PRT-HKNCOMBI
                       MOVE    SPA-COM-HKNCOMBI(IDX)   TO  WRK-HKNCOMBI
                   END-IF
                   MOVE    SPA-COM-SRYSYUKBN(IDX)
                                                   TO  PRT-SRYKBN
                   MOVE    SPA-GMN-MEISYO (IDX)    TO  PRT-TNS-NAME
                   MOVE    SPA-COM-INPUTCD (IDX)   TO  PRT-SRYCD
      *
                   IF      SPA-COM-YOBI1 (IDX)(1:1)    =   "1"
      *                一般名処方加算対象
                       MOVE    "【対象】"          TO  PRT-KBN
                   END-IF
      *
                   IF      CNT-LINE               >=  MAX-LINE
                       PERFORM 400-PRINT-SEC
                       MOVE    ZERO                TO  CNT-LINE
                       PERFORM 280-HEAD-HENSYU-SEC
                   END-IF
                   ADD     1                   TO  CNT-LINE
                   MOVE    LST-PRT-BODY-02     TO  HCMSL80-MOJI
                                                     (CNT-LINE)
      *
      *            IF      WRK-PARA-CSVKBN     =   "1"
      *                ＣＳＶデータ出力
      *                PERFORM 500-CSV-SYORI-SEC
      *            END-IF
      *
                   MOVE    SPACE               TO  LST-PRT-BODY-02
               END-IF
           END-PERFORM
      *
      *    一般名編集
           MOVE    SPACE               TO  LST-PRT-BODY-03
      *    加算入力あり
           EVALUATE    FLG-ERRKBN
               WHEN    1
                   MOVE    "■一般名処方加算は算定不可？"
                                           TO  PRT-MSG-3
               WHEN    2
                   MOVE    "★一般名処方加算２を算定可能？"
                                           TO  PRT-MSG-3
               WHEN    3
                   MOVE    "◆一般名処方加算１を算定可能？"
                                           TO  PRT-MSG-3
           END-EVALUATE
           IF     (FLG-IPPANKSN        =   1   )
              AND (IDX-IP              >   ZERO )
               MOVE    SPA-COM-INPUTCD (IDX-IP)    TO  PRT-SRYCD-3
               MOVE    SPA-GMN-MEISYO (IDX-IP)     TO  PRT-TNS-NAME-3
           END-IF
           ADD     2                   TO  CNT-LINE
           MOVE    LST-PRT-BODY-03     TO  HCMSL80-MOJI (CNT-LINE)
           MOVE    SPACE               TO  LST-PRT-BODY-03
      *
           IF     (CNT-LINE +  2 )     >   MAX-LINE
               PERFORM 400-PRINT-SEC
               MOVE    ZERO            TO  CNT-LINE
               PERFORM 280-HEAD-HENSYU-SEC
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
           MOVE    SPA-PTID            TO  SPTID-PTID
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                           SPA-AREA
           IF      SPTID-RC            =   ZERO
               MOVE    SPTID-PTNUM         TO  PRT-PTNUM
           END-IF
      *    患者マスター読み込み
           MOVE    SPACE               TO  PTINF-REC
           INITIALIZE  PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
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
               IF      TBL-SRYKA-CD (IDZ)  =   SPA-SRYKA
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
           MOVE    LST-PRT-HEAD1       TO  HCMSL80-MOJI (1)
           MOVE    LST-PRT-HEAD2       TO  HCMSL80-MOJI (3)
           MOVE    LST-PRT-HEAD3       TO  HCMSL80-MOJI (4)
           MOVE    ALL "-"             TO  HCMSL80-MOJI (5)
      *
           MOVE    5                   TO  CNT-LINE
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
           MOVE    "一般名処方加算誤算定疑い患者一覧"
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
           MOVE    SPA-SRYYMD          TO  WRK-SYMD
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
           MOVE    SPA-HKNCOMBINUM     TO  WRK-IN-DATA
           MOVE    4                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    連番
           MOVE    TBL-JYURRK-RENNUM   TO  WRK-IN-DATA
           MOVE    1                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
           IF      SPA-COM-YOBI1 (IDX)(1:1)    =   "1"
      *          一般名処方加算対象
                MOVE    "【対象】"          TO  WRK-IN-DATA
           END-IF
      *    種類
      *    EVALUATE    TBL-KOUHATU-KBN(IDS)
      *        WHEN    0
      *            MOVE    "先・不可"            TO  WRK-IN-DATA
      *        WHEN    1
      *            MOVE    "後・　可"            TO  WRK-IN-DATA
      *        WHEN    2
      *            MOVE    "先・　可"            TO  WRK-IN-DATA
      *        WHEN    7
      *            MOVE    "後・不可"            TO  WRK-IN-DATA
      *        WHEN    OTHER
      *            MOVE    SPACE                 TO  WRK-IN-DATA
      *****END-EVALUATE
           MOVE    10                  TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    薬剤コード
           MOVE    SPA-COM-INPUTCD (IDX) TO  WRK-IN-DATA
           MOVE    9                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
           MOVE    SPA-GMN-MEISYO(IDX)   TO  WRK-IN-DATA
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
           MOVE    "2"                 TO  STOUKEICSV-NYUGAIKBN
           MOVE    ZERO                TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"         TO  STOUKEICSV-TO-FOLDER
           MOVE    WRK-TO-DATA         TO  STOUKEICSV-TO-DATA
           MOVE    "2"                 TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSV-REC         TO  STOUKEICSV-CSVDATA
           MOVE    "一般名処方加算誤算定疑い患者一覧ＣＳＶ作成"
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
           STRING  "ORCBGGEREP"        DELIMITED  BY  SIZE
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
           DISPLAY "*** ORCBGGEREP END ***"
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
