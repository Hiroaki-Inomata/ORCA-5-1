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
       PROGRAM-ID.             ORCBGKNSCHK.
      *****************************************************************
      *  システム名        :   ＯＲＣＡ
      *  サブシステム名    :   月次帳票
      *  コンポーネント名  :   平成29年6月
      *                        検査包括の包括ミスによる、包括検査が
      *                        エラーである患者リスト
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/06/28    NACL-多々納   新規作成
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
           03  CNT-PAGE2               PIC 9(05).
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
               05  WRK-KEY-RENNUM          PIC 9(01).
               05  WRK-KEY-HKNCOMBI        PIC X(04).
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
           03  WRK-SRYCD               PIC X(09).
           03  WRK-SRYYMD              PIC X(08).
           03  WRK-RENNUM              PIC 9(01).
      *
      *    包括対象検査
       01  WRK-HOUKNS-AREA.
           03  WRK-HOUKNS-TBL          OCCURS   11.
               05  WRK-HOUKNS-CNT      PIC 9(04).
               05  WRK-HOUKNS-TENSU    PIC 9(07)V9(03).
      *
               05  WRK-ZAICNT          PIC 9(02).
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
                   "＃＃　包括検査　誤算定疑い患者一覧　＃＃".
               05  PRT-MIDASI          PIC X(10).
     ******    05  FILLER              PIC X(16)   VALUE   SPACE.
     ****      05  PRT-SRYYM           PIC X(16).
               05  FILLER              PIC X(22)   VALUE
                   "平成２９年　６・７月分".
     ***       05  FILLER              PIC X(04)   VALUE
     ***           "分".
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
      **   03  LST-PRT-HEAD3.
               05  FILLER              PIC X(01)   VALUE   SPACE.
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
      *    03  LST-PRT-BODY-02.
      *        05  PRT-FIL20           PIC X(15).
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
      *        05  PRT-SRYCD           PIC X(09).
      *        05  PRT-FIL27           PIC X(01).
      *        05  PRT-TNS-NAME        PIC X(50).
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
     **    03  FILLER              PIC X(12)   VALUE   "向精神薬種類".
      *    03  FILLER              PIC X(01)   VALUE   ",".
      *    03  FILLER              PIC X(14)   VALUE   "一般名グループ".
      *    03  FILLER              PIC X(01)   VALUE   ",".
      *    03  FILLER              PIC X(08)   VALUE   "臨時投与".
      *    03  FILLER              PIC X(01)   VALUE   ",".
      *    03  FILLER              PIC X(12)   VALUE   "医薬品コード".
      *    03  FILLER              PIC X(01)   VALUE   ",".
      *    03  FILLER              PIC X(06)   VALUE   "薬剤名".
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
      *    COPY    "CPSK1030.INC".
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
                                               WRK-PARA-SYORIKBN
                                               WRK-PARA-SRYYM
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
           MOVE    "ORCBGKNSCHK"        TO  JOB-PGID
           MOVE    "包括検査誤算定疑い患者一覧"
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
           MOVE    "201706"            TO  WRK-SRY-YYMM
           MOVE    "20170601"          TO  SPA-SRYYMD
      *    処理日
           INITIALIZE  STS-AREA-DAY
           INITIALIZE  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    SPA-SRYYMD          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
      **   MOVE    LNK-DAY2-EDTYMD3(1:16)
      **                               TO  PRT-SRYYM
      *
      *    処理開始年月
           MOVE    "20170601"          TO  WRK-STR-YMD
      *    処理終了年月
           MOVE    "20170731"          TO  WRK-END-YMD
      *
      *    IF      WRK-PARA-SYORIKBN   =   "1"
      *        MOVE    "（処理１）"        TO  PRT-MIDASI
      *    ELSE
      *        MOVE    SPACE               TO  PRT-MIDASI
      *    END-IF
      *
      *    診療科をＴＢＬにセット
           PERFORM 1101-SRYKA-TBLSET-SEC
      *    ＣＳＶ
      *    MOVE    SPA-HOSPNUM         TO  WRK-TO-DATA-HOSPNUM
      *    MOVE    WRK-SRY-YYMM        TO  WRK-TO-DATA-SRYYM
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
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    "（外来）"          TO  PRT-MIDASI
      *
           INITIALIZE  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    "2"                 TO  JYURRK-NYUGAIKBN
           MOVE    WRK-STR-YMD         TO  JYURRK-SRYYMD
           MOVE    WRK-END-YMD         TO  JYURRK-UPSRYYMD
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key69"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key69"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE  JYURRK-REC
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           INITIALIZE  WRK-KEY-AREA
      *    INITIALIZE  TBL-SRY-AREA
           MOVE    ZERO                TO  IDX-TBL
           MOVE    ZERO                TO  WRK-MAE-PTID
           INITIALIZE  WRK-KEY-AREA
           MOVE    ZERO                TO  WRK-ZAINUM
           MOVE    ZERO                TO  FLG-SYOHOU
           INITIALIZE   LNK-WKSRYACT-AREA
           MOVE    JYURRK-RENNUM       TO  WRK-RENNUM
      *
           PERFORM
               UNTIL   (FLG-JYURRK         =   1   )
      *
               IF     (WRK-KEY-PTID        =   JYURRK-PTID   )
                 AND  (WRK-KEY-SRYYMD      =   JYURRK-SRYYMD )
                 AND  (WRK-KEY-SRYKA       =   JYURRK-SRYKA  )
                   IF     (JYURRK-DENPNUM      =   WRK-KEY-DENPNUM)
                       MOVE    1               TO  FLG-OK
                   ELSE
                       MOVE    ZERO            TO  FLG-OK
                   END-IF
               ELSE
                   MOVE    ZERO            TO  FLG-OK
               END-IF
               IF      FLG-OK          =   ZERO
                   IF   (FLG-SYOHOU        =   1    )
      *                伝票番号毎処理
                       PERFORM 2002-HENSYU-SYORI-SEC
                   END-IF
                   INITIALIZE  WRK-HOUKNS-AREA
                   MOVE    ZERO            TO  FLG-SYOHOU
                   MOVE    ZERO                TO  IDX-TBL
                   MOVE    JYURRK-PTID         TO  WRK-KEY-PTID
                   MOVE    JYURRK-SRYYMD       TO  WRK-KEY-SRYYMD
                   MOVE    JYURRK-DENPNUM      TO  WRK-KEY-DENPNUM
                   MOVE    JYURRK-GRP-DENPNUM  TO  WRK-KEY-GRPDENPNUM
                   MOVE    JYURRK-SRYKA        TO  WRK-KEY-SRYKA
                   MOVE    JYURRK-RENNUM       TO  WRK-KEY-RENNUM
               END-IF
      *
               PERFORM 2001-JYURRK-HEN-SEC
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key69"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key69"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF   (FLG-SYOHOU        =   1    )
      *        伝票番号毎処理
               PERFORM 2002-HENSYU-SYORI-SEC
           END-IF
      *
           IF      CNT-LINE            >   ZERO
               PERFORM 400-PRINT-SEC
           END-IF
      *
      *    入院
      *    IF      WRK-PARA-SYORIKBN   =   "1"
           PERFORM 2100-NYUIN-MAIN-SEC
      *
      *    END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院処理
      *****************************************************************
       2100-NYUIN-MAIN-SEC         SECTION.
      *
           MOVE    ZERO            TO  CNT-LINE
           MOVE    ZERO            TO  CNT-PAGE
      *
           MOVE    "（入院）"          TO  PRT-MIDASI
           MOVE    ZERO            TO  FLG-SYOHOU
      *
           INITIALIZE  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    "1"                 TO  JYURRK-NYUGAIKBN
           MOVE    WRK-STR-YMD         TO  JYURRK-SRYYMD
           MOVE    WRK-END-YMD         TO  JYURRK-UPSRYYMD
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key69"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key69"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE  JYURRK-REC
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           INITIALIZE  WRK-KEY-AREA
      *    INITIALIZE  TBL-SRY-AREA
           MOVE    ZERO                TO  IDX-TBL
           MOVE    ZERO                TO  WRK-MAE-PTID
           INITIALIZE  WRK-KEY-AREA
           MOVE    ZERO                TO  WRK-ZAINUM
           MOVE    ZERO                TO  FLG-SYOHOU
           INITIALIZE   LNK-WKSRYACT-AREA
           MOVE    JYURRK-RENNUM       TO  WRK-RENNUM
      *
           PERFORM
               UNTIL   (FLG-JYURRK         =   1   )
      *
               IF     (WRK-KEY-PTID        =   JYURRK-PTID   )
                 AND  (WRK-KEY-SRYYMD      =   JYURRK-SRYYMD )
                 AND  (WRK-KEY-SRYKA       =   JYURRK-SRYKA  )
                 AND  (WRK-KEY-HKNCOMBI    =   JYURRK-HKNCOMBINUM )
                 AND  (WRK-KEY-RENNUM      =   JYURRK-RENNUM  )
                       MOVE    1               TO  FLG-OK
               ELSE
                       MOVE    ZERO            TO  FLG-OK
               END-IF
               IF      FLG-OK          =   ZERO
                   IF   (FLG-SYOHOU        =   1    )
      *                伝票番号毎処理
                       PERFORM 2002-HENSYU-SYORI-SEC
                   END-IF
                   INITIALIZE  WRK-HOUKNS-AREA
                   MOVE    ZERO            TO  FLG-SYOHOU
                   MOVE    ZERO                TO  IDX-TBL
                   MOVE    JYURRK-PTID         TO  WRK-KEY-PTID
                   MOVE    JYURRK-SRYYMD       TO  WRK-KEY-SRYYMD
                   MOVE    JYURRK-HKNCOMBINUM  TO  WRK-KEY-HKNCOMBI
                   MOVE    JYURRK-SRYKA        TO  WRK-KEY-SRYKA
                   MOVE    JYURRK-RENNUM       TO  WRK-KEY-RENNUM
               END-IF
      *
               PERFORM 2001-JYURRK-HEN-SEC
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key69"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key69"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF   (FLG-SYOHOU        =   1    )
      *        伝票番号毎処理
               PERFORM 2002-HENSYU-SYORI-SEC
           END-IF
      *
           IF      CNT-LINE            >   ZERO
               PERFORM 400-PRINT-SEC
           END-IF
      *
           .
       2100-NYUIN-MAIN-EXT.
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
      *
               MOVE    ZERO                TO  IDX
      *
               MOVE    SPACE               TO  SRYACT-REC
               INITIALIZE  SRYACT-REC
               MOVE    JYURRK-HOSPNUM      TO  SRY-HOSPNUM
               MOVE    JYURRK-NYUGAIKBN    TO  SRY-NYUGAIKBN
               MOVE    JYURRK-PTID         TO  SRY-PTID
               MOVE    JYURRK-SRYKA        TO  SRY-SRYKA
               MOVE    JYURRK-SRYYMD(1:6)  TO  SRY-SRYYM
               MOVE    JYURRK-ZAINUM (IDXJ) TO  SRY-ZAINUM
      *
               MOVE    JYURRK-SRYYMD       TO  WRK-SRYYMD
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
      *        検査
               IF     (SRY-SRYSYUKBN       =   "600")
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
      *        剤判定
               PERFORM 20011-ZAICHK-SEC
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
           IF     (WRK-ZAIKAISU        =   ZERO )
                MOVE    1               TO  FLG-CHKIKBN
           END-IF
      *@
      *    自保区分が１の場合、包括剤、薬評対象外
      *    IF     (ACCT-JIHOKBN        =   1   )
      *       OR  (ACCT-HKNCOMBI       =   "9999" )
      *       OR  (ACCT-ZAIKBN     NOT =  ZERO  )
      *         MOVE    1               TO  FLG-CHKIKBN
      *    END-IF
      *
           .
       20011-SRYACCT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスタを作成する 
      *****************************************************************
       49011-SRYACT-HEN-SEC           SECTION.
      *
           PERFORM VARYING     IDS2     FROM    1   BY  1
                   UNTIL      (IDS2         >   5   )   OR
                              (SRY-SRYCD (IDS2)     =   SPACE )
               IF      SRY-SRYCD (IDS2)(1:1)    =   "1"
                   MOVE    SRY-SRYCD (IDS2)     TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
                   IF     (TNS-DATAKBN     =   "1" )
                     AND  (TNS-SRYKBN      =   "60")
      *    検査用　包括対象検査
                   EVALUATE    TNS-HOUKNSKBN
      *内分泌負荷試験は包括計算外
                   WHEN    01   THRU   07
                   WHEN    09   THRU   10
                       MOVE    TNS-HOUKNSKBN   TO  IDX
                       ADD     1               TO  WRK-HOUKNS-CNT (IDX)
                       ADD     TNS-TEN
                                               TO  WRK-HOUKNS-TENSU
                                                                 (IDX)
                       MOVE    1               TO  FLG-SYOHOU
      *特異的ＩＧＥ
                   WHEN    11
                       MOVE    TNS-HOUKNSKBN   TO  IDX
                       ADD     1               TO  WRK-HOUKNS-CNT (IDX)
                       ADD     TNS-TEN
                                               TO  WRK-HOUKNS-TENSU
                                                                 (IDX)
                       MOVE    1               TO  FLG-SYOHOU
                   END-EVALUATE
                   END-IF
               END-IF
           END-PERFORM
           .
       49011-SRYACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    チェック 
      *****************************************************************
       20011-ZAICHK-SEC           SECTION.
      *
           IF      IDX                 >   ZERO
      *        検査あり
      *        ADD     1               TO  IDX-ZA
      *        MOVE    ACCT-SYUTEN1    TO  TBL-SYUTEN1 (IDX-ZA IDX)
               ADD     1               TO  WRK-ZAICNT (IDX)
           END-IF
      *
           MOVE    JYURRK-PTID         TO  SPA-PTID
           MOVE    JYURRK-SRYYMD       TO  SPA-SRYYMD
           MOVE    JYURRK-HKNCOMBINUM  TO  SPA-HKNCOMBINUM
           MOVE    JYURRK-SRYKA        TO  SPA-SRYKA
           MOVE    JYURRK-RENNUM       TO  WRK-RENNUM
           MOVE    JYURRK-NYUGAIKBN    TO  SPA-NYUGAIKBN
           .
       20011-ZAICHK-EXT.
           EXIT.
      *****************************************************************
      *    点数チェック 
      *****************************************************************
       20011-TNSCHK-SEC           SECTION.
      *
      **   MOVE    ZERO                TO  WRK-TENSUKEI
           MOVE    ZERO                TO  FLG-ERR
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   11  )
               EVALUATE    IDX
                   WHEN    1
      *                血液化学検査の包括
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    5   THRU  7
                               MOVE    "099600101"     TO  WRK-SRYCD
                           WHEN    8   THRU  9
                               MOVE    "099600102"     TO  WRK-SRYCD
                           WHEN    10  THRU  9999
                               MOVE    "099600103"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD      NOT  =   SPACE
                           IF      WRK-ZAICNT (IDX) NOT =  1
                               MOVE    1            TO  FLG-ERR
                           END-IF
                       END-IF
                   WHEN    2
      *                内分泌学検査の包括
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    3   THRU  5
                               MOVE    "099600201"     TO  WRK-SRYCD
                           WHEN    6   THRU  7
                               MOVE    "099600202"     TO  WRK-SRYCD
                           WHEN    8   THRU  9999
                               MOVE    "099600203"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD      NOT  =   SPACE
                           IF      WRK-ZAICNT (IDX) NOT =  1
                               MOVE    1            TO  FLG-ERR
                           END-IF
                       END-IF
                   WHEN    3
      *                肝炎ウイルス関連の包括
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    3
                               MOVE    "099600301"     TO  WRK-SRYCD
                           WHEN    4
                               MOVE    "099600302"     TO  WRK-SRYCD
                           WHEN    5   THRU  9999
                               MOVE    "099600303"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD      NOT  =   SPACE
                           IF      WRK-ZAICNT (IDX) NOT =  1
                               MOVE    1            TO  FLG-ERR
                           END-IF
                       END-IF
                   WHEN    4
      *                腫瘍マーカーの包括
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    2   THRU  9999
                               MOVE    "099600401"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD      NOT  =   SPACE
                           IF      WRK-ZAICNT (IDX) NOT =  1
                               MOVE    1            TO  FLG-ERR
                           END-IF
                       END-IF
                   WHEN    5
      *                ４以外の腫瘍マーカーの包括
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    2
                               MOVE    "099600501"     TO  WRK-SRYCD
                           WHEN    3
                               MOVE    "099600502"     TO  WRK-SRYCD
                           WHEN    4   THRU  9999
                               MOVE    "099600503"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD      NOT  =   SPACE
                           IF      WRK-ZAICNT (IDX) NOT =  1
                               MOVE    1            TO  FLG-ERR
                           END-IF
                       END-IF
                   WHEN    6
      *                出血・凝固検査の包括
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    3   THRU  4
                               MOVE    "099600601"     TO  WRK-SRYCD
                           WHEN    5   THRU  9999
                               MOVE    "099600602"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD      NOT  =   SPACE
                           IF      WRK-ZAICNT (IDX) NOT =  1
                               MOVE    1            TO  FLG-ERR
                           END-IF
                       END-IF
                   WHEN    7
      *                自己抗体検査の包括
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    2
                               MOVE    "099600701"     TO  WRK-SRYCD
                           WHEN    3   THRU  9999
                               MOVE    "099600702"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD      NOT  =   SPACE
                           IF      WRK-ZAICNT (IDX) NOT =  1
                               MOVE    1            TO  FLG-ERR
                           END-IF
                       END-IF
                   WHEN    9
      *            ウイルス抗体価
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    8   THRU  9999
                               MOVE    "099600901"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD      NOT  =   SPACE
                           IF      WRK-ZAICNT (IDX) NOT =  1
                               MOVE    1            TO  FLG-ERR
                           END-IF
                       END-IF
                   WHEN    10
      *            グロブリンクラス別ウイルス抗体価
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    2   THRU  9999
                               MOVE    "099601001"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD      NOT  =   SPACE
                           IF      WRK-ZAICNT (IDX) NOT =  1
                               MOVE    1            TO  FLG-ERR
                           END-IF
                       END-IF
                   WHEN    11
      *            特異的ＩＧＥ
                       IF      WRK-HOUKNS-CNT(IDX)  >   1
                           IF      WRK-ZAICNT (IDX) NOT =  1
                               MOVE    1            TO  FLG-ERR
                           END-IF
                       END-IF
      **               PERFORM 10042-IGE-HOUKNS-ADD-SEC
      *内分泌負荷試験は包括計算外
      *D           WHEN    8
      *                内部泌負荷試験の包括
      *                ADD     WRK-HOUKNS-TENSU (IDX)
      *D                                           TO  WRK-TENSUKEI
               END-EVALUATE
           END-PERFORM
           .
       20011-ZAICHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    投薬内容チェック処理
      *****************************************************************
       2002-HENSYU-SYORI-SEC       SECTION.
      *
      *    点数編集
           PERFORM 20011-TNSCHK-SEC
      *
           IF      FLG-ERR         =   1
      *        印刷処理
               PERFORM 20020-PRINT-SYORI-SEC
           END-IF
      *
           .
       2002-HENSYU-SYORI-EXT.
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
      *    IF     (WRK-MAE-PTID     NOT =   SPA-PTID )
      *       AND (CNT-LINE             >   5        )
      *         ADD     1               TO  CNT-LINE
      *         MOVE    ALL "-"         TO  HCMSL80-MOJI (CNT-LINE)
      *    END-IF
      *    IF      CNT-LINE            >=  MAX-LINE
      *        PERFORM 400-PRINT-SEC
      *        MOVE    ZERO            TO  CNT-LINE
      *        PERFORM 280-HEAD-HENSYU-SEC
      *    END-IF
      *
           MOVE    SPA-SRYYMD          TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  PRT-SRYYMD
      *    診療科編集
           PERFORM 200201-SRYKA-HEN-SEC
           MOVE    SPA-HKNCOMBINUM     TO  PRT-HKNCOMBI
           MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBI
      *
           ADD     1                   TO  CNT-LINE
           MOVE    LST-PRT-BODY-01     TO  HCMSL80-MOJI (CNT-LINE)
      *    IF      WRK-MAE-PTID    NOT =   SPA-PTID
      *        ADD     1                   TO  CNT-LINE
      *    END-IF
      *
           MOVE    SPA-PTID            TO  WRK-MAE-PTID
      *
      *    明細編集
      *    MOVE    SPACE               TO  LST-PRT-BODY-02
      *    MOVE    SPA-SRYYMD          TO  WRK-SYMD
      *    PERFORM 800-SEIWA-HEN-SEC
      *    MOVE    WRK-HENYMDG         TO  PRT-SRYYMD
      *    診療科編集
      *    PERFORM 200201-SRYKA-HEN-SEC
      *    MOVE    SPA-HKNCOMBINUM     TO  PRT-HKNCOMBI
      *    MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBI
      *
      *    IF      CNT-LINE               >=  MAX-LINE
      *        PERFORM 400-PRINT-SEC
      *        MOVE    ZERO                TO  CNT-LINE
      *        PERFORM 280-HEAD-HENSYU-SEC
      *    END-IF
      *    ADD     1                   TO  CNT-LINE
      *    MOVE    LST-PRT-BODY-02     TO  HCMSL80-MOJI
      *                                         (CNT-LINE)
      *
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
     ****          MOVE    TBL-SRYKA-MEI (IDZ)     TO  TBL-SRYKAMEI
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
           ADD     1                   TO  CNT-PAGE2
           MOVE    CNT-PAGE            TO  PRT-PAGE
      *
           MOVE    LST-PRT-HEAD1       TO  HCMSL80-MOJI (1)
           MOVE    LST-PRT-HEAD2       TO  HCMSL80-MOJI (3)
     ***   MOVE    LST-PRT-HEAD3       TO  HCMSL80-MOJI (4)
           MOVE    ALL "-"             TO  HCMSL80-MOJI (5)
      *
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
           MOVE    "包括検査誤算定疑い患者一覧"
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
           STRING  "ORCBGKNSCHK"        DELIMITED  BY  SIZE
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
           IF      CNT-PAGE2           =   ZERO 
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBGKNSCHK END ***"
      *
      *    ステップ管理終了処理
           MOVE    "STE"               TO  SJOBKANRI-MODE
           INITIALIZE  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    CNT-PAGE2           TO  JOB-UPDCNT
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
           MOVE    WRK-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  TNS-YUKOEDYMD
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
