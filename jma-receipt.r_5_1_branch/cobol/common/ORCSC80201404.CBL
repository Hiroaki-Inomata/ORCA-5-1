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
       PROGRAM-ID.                 ORCSC80201404.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為チェック、算定（その他）
      *                      Ｈ２６年４月改正対応
      *  管理者            : 
      *  作成日付    作業者        記述
      *  14/03/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-多々納   02/04/16  労災用追加
      * 01.00.02     NACL-多々納  02/10/03  労災加算なしの追加
      * 01.00.03     NACL-多々納  02/11/20  自賠責器材追加
      * 01.00.04     NACL-多々納  03/03/19  老人の労災加算の修正
      * 01.00.05     NACL-多々納  03/09/22  家族通院精神療法算定追加
      * 01.00.06     NACL-多々納  03/10/10  リハビリチェック変更
      * 01.00.07     NACL-多々納  04/03/17  Ｈ１６年４月改正追加
      * 01.00.08     NACL-多々納  05/01/27  酸素追加
      * 01.00.09     NACL-多々納  05/11/22  MONFUNC 対応 他
      * 01.00.11     NACL-多々納  06/06/12  診療科毎の同日初診判定
      * 03.04.00     NACL-多々納  07/01/11  リハビリ終了日追加
      * 03.04.01     NACL-多々納  07/03/XX  リハビリ逓減追加(H19.4改正）
      * 03.05.00     NACL-多々納  07/04/XX  グループ診療対応
      * 04.02.00     NACL-多々納  08/03/XX  平成２０年４月改正対応
      * 04.05.00     NACL-多々納  09/07/XX  早期リハビリテーション加算チェック
      * 04.05.00     NACL-多々納  09/11/XX  入院精神療法６か月対応
      * 04.05.00     NACL-多々納  10/03/XX  平成２２年４月改正対応
      * 04.06.00     NACL-多々納  10/07/01  残量廃棄自動算定対応
      * 04.06.00     NACL-多々納  10/09/XX  精神療法２０歳未満加算未対応
      * 04.07.00     NACL-多々納  11/08/XX  リハビリ算定日毎終了日付対応
      * 04.07.00     NACL-多々納  11/09/XX  精神科ケア早期加算チェック対応
      * 04.07.00     NACL-多々納  12/03/XX  Ｈ２４．４改正対応
      * 04.07.00     NACL-多々納  13/05/15  疾患別リハビリ月上限対応
      * 04.07.00     NACL-多々納  14/03/XX  Ｈ２６年４月改正対応
      * 04.07.00     NACL-多々納  14/04/03  精２０歳未満加算削除対応
      * 04.07.00     NACL-多々納  14/05/02  労災リハビリ加算計算対応
      * 04.07.00     NACL-多々納  14/07/14  労災リハ（1.2倍）対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-CHKMST          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-SANTEIPLUS      PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-ARIOK           PIC 9(01).
           03  FLG-NAIYAK          PIC 9(01).
           03  FLG-TONPUK          PIC 9(01).
           03  FLG-GAIYAK          PIC 9(01).
           03  FLG-MADOKU          PIC 9(01).
           03  FLG-ZAI             PIC 9(01).
           03  FLG-NAIYAK2         PIC 9(01).
           03  FLG-TONPUK2         PIC 9(01).
           03  FLG-GAIYAK2         PIC 9(01).
      *
           03  FLG-YAKJYO          PIC 9(01).
           03  FLG-ADD             PIC 9(01).
      *    施設基準不適合
           03  FLG-KIJUNN          PIC 9(01).
      *    早期リハビリあり
      *****03  FLG-SOUKI           PIC 9(01).
           03  FLG-SOUKI-ALL       PIC 9(01).
           03  FLG-SOUKI-END       PIC 9(01).
           03  FLG-SOUKI-ARI       PIC 9(01).
           03  FLG-SOUKI-ARI1      PIC 9(01).
           03  FLG-SOUKI-ARI2      PIC 9(01).
           03  FLG-SANTEI-ARI      PIC 9(01).
      *    手技料計算フラグ
           03  FLG-SYUGI           PIC 9(01).
      *    労災用
           03  FLG-RIHA            PIC 9(01).
      *    労災加算あり
           03  FLG-ROU-ARI         PIC 9(01).
           03  FLG-ROUSAI          PIC 9(01).
      *    リハビリあり
           03  FLG-RIGAKU          PIC 9(01).
      *
           03  FLG-SOUKIKSN            PIC 9(01).
           03  FLG-KYUSEI-CHK          PIC 9(01).
      *
           03  FLG-NENREI              PIC 9(01).
           03  FLG-TOUJITU             PIC 9(01).
           03  FLG-SEISIN              PIC 9(01).
           03  FLG-SHOKAIKBN           PIC 9(01).
      *    他院退院日
           03  FLG-TAITAIIN            PIC 9(01).
           03  FLG-NINCHI              PIC 9(01).
      *
           03  FLG-CHK-END             PIC 9(01).
      *H24.4
           03  FLG-NENREI2             PIC 9(01).
           03  FLG-KAIGO               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
      *
           03  IDX2                PIC 9(02).
           03  IDY2                PIC 9(02).
      *
           03  IDD                 PIC 9(02).
           03  IDD2                PIC 9(02).
      *
           03  IDXS                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-Z1              PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-C               PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-SA              PIC 9(04).
           03  IDX-MAX             PIC 9(04).
      *
           03  IDX-SYU             PIC 9(04).
           03  HZN-IDX             PIC 9(04).
           03  HZN-IDX2             PIC 9(04).
      *
           03  IDX-Y               PIC 9(04).
           03  IDX-Y2              PIC 9(02).
           03  IDX-T               PIC 9(04).
           03  IDX-R               PIC 9(04).
           03  IDX-R1              PIC 9(04).
      *
           03  IDX-RS              PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
           03  IDX-Z2              PIC 9(04).
           03  IDX-RSI             PIC 9(04).
      *
           03  IDX-HZN             PIC 9(04).
      *
           03  IDX-STR             PIC 9(04).
      *
           03  IDX-DAY             PIC 9(02).
           03  IDX-KSN             PIC 9(04).
           03  IDE                 PIC 9(02).
           03  IDE2                PIC 9(02).
      *H24.4
      *    施設基準用
           03  IDS                 PIC 9(04).
           03  IDX-ZS              PIC 9(04).
           03  IDX-ZS2             PIC 9(04).
      *H25.4
           03  IDE3                PIC 9(02).
           03  IDXT                PIC 9(04).
           03  IDT2                PIC 9(04).
           03  IDT3                PIC 9(04).
      *
           03  IDX-ZS3                PIC 9(04).
           03  IDX-Z3                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYCD           PIC X(09).
           03  WRK-SRYCD2          PIC X(09).
      *    減額診療コード（労災）
           03  WRK-GEN-SRYCD       PIC X(09).
      *
           03  WRK-MOJI            PIC X(01). 
      *
           03  WRK-ERRCD           PIC X(04). 
      *
           03  WRK-SYOKEIS         PIC 9(08)V99.
           03  WRK-SYOKEIS2        PIC 9(08)V99.
           03  WRK-SYOKEI          PIC 9(08).
      *
           03  WRK-ZAIKEI          PIC 9(08)V99.
           03  WRK-GAIKEI          PIC 9(08)V99.
           03  WRK-GAITEN          PIC 9(08).
           03  WRK-KIZKEI          PIC 9(08).
           03  WRK-KIZTEN          PIC 9(08).
           03  WRK-FIRTEN          PIC 9(08).
           03  WRK-MAISU           PIC 9(08).
           03  WRK-KEIMAI          PIC 9(08).
           03  WRK-KAISU           PIC 9(04).
           03  WRK-DAYCNT          PIC 9(03).
           03  WRK-TUKCNT          PIC 9(03).
      *
           03  WRK-TENSUKEI        PIC 9(08)V99.
           03  WRK-TENSUKEI-9      PIC S9(08).
           03  WRK-TENSUKEI-G      PIC S9(08).
           03  WRK-TENSUKEI-K      PIC S9(08).
           03  WRK-TENSUKEI-RSI    PIC S9(08).
           03  WRK-TENGAKEI-G      PIC S9(10).
           03  WRK-TENGAKEI-K      PIC S9(10).
      *
           03  WRK-TENSU-KEISAN.
               05  WRK-SUM-TENSUKEI    PIC 9(08)V99.
               05  WRK-SUM-TENGAKEI    PIC 9(08)V99.
      *
           03  WRK-GOK-TENSUKEI        PIC S9(10).
           03  WRK-GOK-TENGAKEI        PIC S9(10).
      *
           03  WRK-IDX             PIC 9(04).
      *
      *    03  WRK-RIGAKU-KBN      PIC X(01).
      *    03  WRK-RIGAKU-RYO      PIC X(01).
      *    03  WRK-RIGAK-CNTALL    PIC 9(03).
      *    03  WRK-TBL-RSI-KBN     PIC X(01).
      *!
      *    酸素計算用
           03  WRK-SANSO               PIC 9(08).
           03  WRK-SANSOKIN            PIC 9(08)V99.
           03  WRK-SANKEI              PIC 9(08).
           03  WRK-SANTEN              PIC 9(08).
           03  WRK-SANSOKIN-SUM        PIC 9(08)V99.
           03  IDX-SANSO               PIC 9(04).
      *
      *    理学最大単位
           03  WRK-RIGAK-MAX       PIC 9(03).
      *
           03   WRK-SYOYMD.
                05   WRK-SYO-YY    PIC 9(04).
                05   WRK-SYO-MM    PIC 9(02).
                05   WRK-SYO-DD    PIC 9(02).
           03   WRK-SYOYMD2.
                05   WRK-SYO2-YY    PIC 9(04).
                05   WRK-SYO2-MM    PIC 9(02).
                05   WRK-SYO2-DD    PIC 9(02).
      *H24.4
           03   WRK-SYOYMD3.
                05   WRK-SYO3-YY    PIC 9(04).
                05   WRK-SYO3-MM    PIC 9(02).
                05   WRK-SYO3-DD    PIC 9(02).
      *    診療日
           03   WRK-SRYYMD         PIC X(08).
           03   WRK-EDSRYYMD       PIC X(08).
      **** 急性発生日＋15
      *    03   WRK-KYUSEI-15      PIC X(08).
      *    急性発生日＋30
      *    03   WRK-KYUSEI-30      PIC X(08).
      *    急性発生日＋14
      *    03   WRK-KYUSEI-14      PIC X(08).
      *    急性発生日＋31
      *    03   WRK-KYUSEI-31      PIC X(08).
      *****03   WRK-KYUSEI-89      PIC X(08).
           03   WRK-KYUSEI-YMD     PIC X(08).
           03   WRK-KYUSEI         PIC X(08).
           03   WRK-KYUSEI-CHK     PIC X(08).
           03  WRK-YMD.
               05  WRK-YYMM.
                   07   WRK-YY             PIC X(04).
                   07   WRK-MM             PIC X(02).
               05  WRK-DD                  PIC X(02).
           03  WRK-SDD             PIC 9(02).
           03  WRK-EDMM            PIC X(02).
           03  WRK-EDDD            PIC X(02).
           03  WRK-KYUSEI-MSG      PIC X(80).
      *
           03   WRK-RIHA-KBN       PIC X(01).
           03   WRK-RIHA-KBN2      PIC X(01).
           03   WRK-RIHA-GEN       PIC X(01).
      *@    逓減開始日
           03   WRK-TEIGEN-YMD     PIC X(08).
           03   WRK-TEIGEN         PIC X(08).
           03   WRK-TEIGEN-MSG     PIC X(80).
      *
      *    自賠責金額用
           03  WRK-KINGAK          PIC 9(08).
           03  FLG-JIHI            PIC 9(01).
      *
           03  WRK-NYU-KAI         PIC 9(03).
      *    集団の月上限
           03  WRK-MAX-CNT         PIC 9(04).
      *    訂正
      ***  03  WRK-TEI-CNT         PIC 9(04).
           03  WRK-ZOGEN           PIC S9(03).
      *    診療年月日−６ヶ月
           03  WRK-SRYYMD-1.
               05  WRK-SRYYM-1.
                   07  WRK-SRYYY       PIC 9(04).
                   07  WRK-SRYMM       PIC 9(02).
               05  WRK-SRYDD           PIC 9(02).
      *
           03  WRK-SRYKA           PIC  X(02).
      *
      *    発症日＋３０日
           03  WRK-KYUSEI-30               PIC X(08).
           03  WRK-KYUSEI-30-MSG           PIC X(80).
      *    入院精神療法期間チェック
           03  WRK-TUKI-9                  PIC 9(03).
           03  WRK-NYUINYMD                PIC X(08).
           03  WRK-NYUIN-YMD               PIC X(08).
           03  WRK-SHONUM                  PIC 9(03).
      *    退院日＋1か月
           03  WRK-TAIINYMD                PIC X(08).
           03  WRK-TAIINYMD-1              PIC X(08).
           03  WRK-TAIINYMD-3              PIC X(08).
      *H24.4
      *    発症日＋１４日
           03  WRK-KYUSEI-14               PIC X(08).
           03  WRK-KYUSEI-14-MSG           PIC X(80).
           03  WRK-KYUSEI-MSG02            PIC X(80).
      *
           03  WRK-SURYO               PIC 9(07)V9(05).
           03  WRK-SURYO-R             REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1        PIC 9(07).
               05  WRK-SURYO-S2        PIC 9(05).
      *
      *    精神科ケア開始日
           03  WRK-KEASTRYMD.
               05  WRK-KEASTR-YY       PIC 9(04).
               05  WRK-KEASTR-MM       PIC 9(02).
               05  WRK-KEASTR-DD       PIC 9(02).
      *
      *H25.7
      *    疾患別リハビリ月回数
           03  WRK-TUKI-SRYCD.
               05  WRK-SRYCDT          PIC X(09)   OCCURS   18.
           03  WRK-ACCT-CNT            PIC 9(03).
           03  WRK-TUKI-CNT2           PIC 9(03).
      *    月回数
           03  WRK-TUKI-CNT            PIC 9(03).
      *    訂正
           03  WRK-TEI-CNT             PIC 9(04).
      *
      *    外来リハビリ（要介護）施設基準不適合
           03  WRK-ERRCHK              PIC 9(01).
      *
      *
      *   きざみ値行為再計算用
           03  WRK-RIHA-KIZAMI-G.
               05  WRK-KEI-TEN         PIC 9(08)V9(03).
               05  WRK-KEI-TEN1        PIC 9(08)V9(03).
               05  WRK-KEI-TEN2        PIC 9(08).
               05  WRK-KSNTEN          PIC 9(08)V9(05).
               05  WRK-KZMTEN          PIC 9(08)V9(05).
               05  WRK-SYU-TEN         PIC 9(08).
               05  WRK-SYU-KZMTEN      PIC 9(08).
               05  FLG-RIHAEND         PIC 9(01).
      *
      *
       01  TBL-RIHA-ZAINUM-G.
           03  TBL-RIHA-ZAINUM-OCC     OCCURS  50.
               05  TBL-R-ZAINUM            PIC 9(08).
               05  TBL-R-NYUGAIKBN         PIC X(01).
               05  TBL-R-SRYKA             PIC X(02).
               05  TBL-R-SRYYM             PIC X(06).
               05  TBL-R-SRYKBN            PIC X(02).
               05  TBL-R-SURYO             PIC 9(05).
      *
      *    リハビリ回数集計
       01  WRK-RIGAKU-NCNT-AREA.
           03  WRK-RIGAKU-INT2             PIC X(01).
      *    今回リハビリ発生日
           03  WRK-KYUSEI-INP-G.
               05  WRK-KYUSEI-IN-G         OCCURS   8.
                   07  WRK-KYUSEI-ST       PIC X(01).
                   07  WRK-KYUSEI-ED       PIC X(01).
               05  WRK-TEIGEN-IN           PIC X(01).
      *    当月算定回数（算定履歴）
           03  WRK-RIHA-DAY-G.
               05  WRK-RIHA-DAY            PIC 9(03)
                                           OCCURS   31.
      *    疾患別今回算定回数
           03  WRK-RIHA-CNT-G.
               05  WRK-RIHA-CNT-T          OCCURS   4.
                   07  WRK-RIHA-CNT2       PIC 9(03)
                                           OCCURS   31.
      *    月回数
      **   03  WRK-TUKI-CNT              PIC 9(03).
      *    リハビリ発生日
           03  WRK-KYUSEI1-YMD             PIC X(08).
           03  WRK-KYUSEI2-YMD             PIC X(08).
           03  WRK-KYUSEI3-YMD             PIC X(08).
           03  WRK-KYUSEI4-YMD             PIC X(08).
      *    リハビリ終了日チェック用
           03  WRK-KYUSEI5-YMD             PIC X(08).
           03  WRK-KYUSEI5-CHK             PIC X(01).
           03  WRK-KYUSEI6-YMD             PIC X(08).
           03  WRK-KYUSEI6-CHK             PIC X(01).
           03  WRK-KYUSEI7-YMD             PIC X(08).
           03  WRK-KYUSEI7-CHK             PIC X(01).
           03  WRK-KYUSEI8-YMD             PIC X(08).
           03  WRK-KYUSEI8-CHK             PIC X(01).
      *    逓減対象外
      **** 03  WRK-TEIGEN-GAI              PIC X(08).
      *
      *    通院精神用同日初診
           03  WRK-DOUSYOSIN-AREA.
               05  WRK-DOUSYOSIN-OCC       OCCURS   10.
                   07  WRK-DOU-SRYKA       PIC X(02).
                   07  WRK-DOU-SRYYMD      PIC X(08).
      *H26.4
      *    通院・在宅精神開始日
           03  WRK-TUINSTR-INT             PIC X(01).
           03  WRK-TUINSTR-YMD             PIC X(08).
      *    精神科ケア早期加算開始日
           03  WRK-KEASOKI-INT             PIC X(01).
           03  WRK-KEASOKI-YMD             PIC X(08).
      *    初期区分
           03  WRK-RIGAKU-INT              PIC X(01).
      *
       01  WRK-ERRCD-AREA.
           03  WRK-ERRCD-H                 PIC X(04). 
      *    保存用
       01  HZN-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HZN-//.
      *    リハビリ同一算定コードテーブル
           COPY   "CMRIHACDTBL.INC".
      *    保険別算定履歴テーブル
           COPY   "CMSANTEITBL.INC".
      *
      *    リハビリの早期リハ加算テーブル
      **** COPY   "CMRIHAKSNTBL.INC".
       01  CON-RIHAKSN                 PIC X(09)   VALUE    "180030770".
      *H24.4
      *    初期加算（リハビリテーション料）
       01  CON-SYOKIKSN                PIC X(09)   VALUE    "180033870".
      *
      *    短期集中リハ実施加算テーブル
       01  CON-TANKIKSN1               PIC X(09)   VALUE    "180032970".
       01  CON-TANKIKSN2               PIC X(09)   VALUE    "180033070".
      *
      *ver.4.7
      *精神科ケア早期加算テーブル
      *    精神科ショート・ケア早期
       01  CON-KEAKSN1                 PIC X(09)   VALUE    "180033370".
      *    精神科デイ・ケア早期
       01  CON-KEAKSN2                 PIC X(09)   VALUE    "180033470".
      *    精神科ナイト・ケア早期
       01  CON-KEAKSN3                 PIC X(09)   VALUE    "180036270".
      *    精神科デイ・ナイト・ケア早期
       01  CON-KEAKSN4                 PIC X(09)   VALUE    "180036370".
      *    重度認知症デイ・ケア早期
       01  CON-KEAKSN5                 PIC X(09)   VALUE    "180039670".
      *
      *    酸素コードテーブル
           COPY     "CMSANSOTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ（施設基準）
           COPY    "CPSK1006.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
      *    COPY    "CPTENSU.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *    算定履歴付加
       01  SANTEIPLUS-REC.
           COPY    "CPSANTEIPLUS.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    チェックマスタ−
      *01  CHK-REC.
      *    COPY    "CPCHK.INC".
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *
      *    診療行為算定編集パラメタ
           COPY    "CPORCSC90TEN2.INC".
      *
      *    診療行為算定歴チェックパラメタ
           COPY    "CPORCSC91.INC".
      *
      *    診療行為点数編集・基本チェック
           COPY    "CPORCSC92.INC".
      *
      *    リハビリ当月算定カウントパラメタ
           COPY    "CPORCSC80CNT.INC".
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面表示編集パラメタ
           COPY    "CPORCSC94.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           ORCSC00AREA
           WRK-SCR-REC
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           MOVE    LNK-RIGAKU-NCNT-AREA    TO  WRK-RIGAKU-NCNT-AREA
      *
           IF      LNK-ORCSC-PG        =   "K02"
                                       OR  "KT01"
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           END-IF
      *    初回に当月分編集
           IF      LNK-ORCSC-PG        =   "K02"
      *
               IF      WRK-RIGAKU-INT      =   SPACE
                   PERFORM 1000-RIHBIRI-INIT-SEC
               END-IF
               IF      WRK-RIGAKU-INT2     =   SPACE
      *            今回算定回数クリア
                   INITIALIZE  WRK-RIHA-CNT-G
                   MOVE    SPACE           TO  WRK-KEASOKI-INT
      ************ INITIALIZE  WRK-KYUSEI-INP-G
                   MOVE    "1"             TO  WRK-RIGAKU-INT2
               END-IF
           ELSE
               INITIALIZE              WRK-RIGAKU-NCNT-AREA
           END-IF
      *
           EVALUATE    LNK-ORCSC-KBN
      *        剤別　チェック、編集、自動算定
               WHEN    "1"
                   PERFORM 100-SEL-SYORI-SEC
      *        全体のチェック
               WHEN    "2"
                   PERFORM 200-ALL-SYORI-SEC
      *        自動算定
               WHEN    "3"
                   PERFORM 300-AUTO-SYORI-SEC
               WHEN    OTHER
                   MOVE    "1001"          TO  SPA-ERRCD
           END-EVALUATE
           MOVE    WRK-RIGAKU-NCNT-AREA    TO  LNK-RIGAKU-NCNT-AREA
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    リハビリの回数編集処理
      *****************************************************************
       1000-RIHBIRI-INIT-SEC                SECTION.
      *
           INITIALIZE                  WRK-RIGAKU-NCNT-AREA
           MOVE    "099800201"         TO  WRK-SRYCD
      *    訂正の時、当日算定有無の判定
           MOVE    ZERO                TO  WRK-TEI-CNT
           IF      LNK-ORCSC-SYORIFLG      =   1
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ     >   SPA-MAX-LINE )  OR
                                  (LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   SPACE )
                   IF      LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   WRK-SRYCD
                       ADD     LNK-ORCSC-TEI-KAISU(IDZ)
                                               TO  WRK-TEI-CNT
                   END-IF
               END-PERFORM
           END-IF
      *    リハビリ算定回数
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-SRYYMD (1:6)    TO  SANTEI-SRYYM
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
      *    労災毎とする
           IF      SPA-HKNKBN          =   1
                                       OR  2
               MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           END-IF
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           MOVE    SPA-SRYYMD(7:2)     TO  IDD2
      *    月上限
           MOVE    ZERO                TO  WRK-TUKI-CNT
           IF      FLG-SANTEI          =   ZERO
      *        当日の算定回数
               PERFORM VARYING     IDD     FROM    1   BY  1
                       UNTIL       IDD     >   31
      *            訂正分
                   IF      IDD             =   IDD2
                       IF      WRK-TEI-CNT     >   SANTEI-DAY (IDD)
                           MOVE    ZERO        TO  SANTEI-DAY (IDD)
                       ELSE
                           COMPUTE SANTEI-DAY (IDD)
                                               =   SANTEI-DAY (IDD)
                                               -   WRK-TEI-CNT
                       END-IF
                   END-IF
                   ADD     SANTEI-DAY (IDD)    TO  WRK-RIHA-DAY(IDD)
                   ADD     SANTEI-DAY (IDD)    TO  WRK-TUKI-CNT
               END-PERFORM
           END-IF
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    各急性発生日
      *    心大血管疾患リハビリテーション
           MOVE    "099800111"         TO  WRK-SRYCD
           PERFORM 10001-RIHA-STRYMD-SEC
           MOVE    WRK-YMD             TO  WRK-KYUSEI1-YMD
      *    終了日判定
           IF      WRK-KYUSEI1-YMD NOT =   SPACE
               MOVE    "099800112"         TO  WRK-SRYCD
               PERFORM 10001-KYUSEI-YMD-SEC
               IF      WRK-YMD         NOT =   SPACE
                   IF      WRK-KYUSEI1-YMD     <   WRK-YMD
                       MOVE    SPACE           TO  WRK-KYUSEI1-YMD
                   END-IF
               END-IF
           END-IF
      *    脳血管疾患リハビリテーション
           MOVE    "099800121"         TO  WRK-SRYCD
           PERFORM 10001-RIHA-STRYMD-SEC
           MOVE    WRK-YMD             TO  WRK-KYUSEI2-YMD
           IF      WRK-KYUSEI2-YMD NOT =   SPACE
               MOVE    "099800122"         TO  WRK-SRYCD
               PERFORM 10001-KYUSEI-YMD-SEC
               IF      WRK-YMD         NOT =   SPACE
                   IF      WRK-KYUSEI2-YMD     <   WRK-YMD
                       MOVE    SPACE           TO  WRK-KYUSEI2-YMD
                   END-IF
               END-IF
           END-IF
      *    運動器リハビリテーション
           MOVE    "099800131"         TO  WRK-SRYCD
           PERFORM 10001-RIHA-STRYMD-SEC
           MOVE    WRK-YMD             TO  WRK-KYUSEI3-YMD
           IF      WRK-KYUSEI3-YMD NOT =   SPACE
               MOVE    "099800132"         TO  WRK-SRYCD
               PERFORM 10001-KYUSEI-YMD-SEC
               IF      WRK-YMD         NOT =   SPACE
                   IF      WRK-KYUSEI3-YMD     <   WRK-YMD
                       MOVE    SPACE           TO  WRK-KYUSEI3-YMD
                   END-IF
               END-IF
           END-IF
      *    呼吸器リハビリテーション
           MOVE    "099800141"         TO  WRK-SRYCD
           PERFORM 10001-RIHA-STRYMD-SEC
           MOVE    WRK-YMD             TO  WRK-KYUSEI4-YMD
           IF      WRK-KYUSEI4-YMD NOT =   SPACE
               MOVE    "099800142"         TO  WRK-SRYCD
               PERFORM 10001-KYUSEI-YMD-SEC
               IF      WRK-YMD         NOT =   SPACE
                   IF      WRK-KYUSEI4-YMD     <   WRK-YMD
                       MOVE    SPACE           TO  WRK-KYUSEI4-YMD
                   END-IF
               END-IF
           END-IF
      *    逓減対象外
      ***  MOVE    "099800401"         TO  WRK-SRYCD
      *    PERFORM 10001-KYUSEI-YMD-SEC
      **** MOVE    WRK-YMD             TO  WRK-TEIGEN-GAI
      *
      *    外来の時、精神通院
           IF      SPA-NYUGAIKBN       =   "2"
               PERFORM 10002-DOUJIT-YMD-SEC
           END-IF
      *
           MOVE    "1"                 TO  WRK-RIGAKU-INT
           .
       1000-RIHBIRI-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    急性発生日設定処理
      *****************************************************************
       10001-KYUSEI-YMD-SEC                SECTION.
      *
      *    MOVE    WRK-SRYCD           TO  TNS-SRYCD
      *    訂正時
           MOVE    ZERO                TO  FLG-TOUJITU
           IF      LNK-ORCSC-SYORIFLG      =   1
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ     >   100 )  OR
                                  (LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   SPACE )
                   IF      LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   WRK-SRYCD
                       MOVE    1           TO  FLG-TOUJITU
                   END-IF
               END-PERFORM
           END-IF
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
      *
      *    労災毎とする
           IF     (SPA-HKNKBN          =   1
                                       OR  2   )
               MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           END-IF
      *    逓減対象外、通院精神療法開始日は１件
           IF     (WRK-SRYCD           =   "099800401" )
               OR (WRK-SRYCD           =   "099830102" )
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           END-IF
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           MOVE    SPACE               TO  WRK-YMD
           PERFORM
               UNTIL       FLG-SANTEI      =   1
               IF     (SANTEI-MSANTEICNT   >   ZERO )  AND
                      (SANTEI-SRYYM        <=  SPA-SRYYMD(1:6))
      *            最後の発生日、当日以降は対象外
                   IF      SANTEI-SRYYM        =   SPA-SRYYMD(1:6)
                       MOVE    SPA-SRYYMD(7:2)     TO  IDD2
      *                訂正分は対象外
                       IF      FLG-TOUJITU         =   1
                           COMPUTE IDD2            =   IDD2   -  1
                       END-IF
                   ELSE
                       MOVE    31                  TO  IDD2
                   END-IF
                   MOVE    ZERO                TO  WRK-SDD
                   PERFORM VARYING     IDD     FROM    IDD2  BY  -1
                           UNTIL       IDD     <   1
                       IF      SANTEI-DAY (IDD)    >   ZERO
                           MOVE    IDD             TO  WRK-SDD
                           MOVE    1               TO  IDD
                       END-IF
                   END-PERFORM
                   IF      WRK-SDD             >   ZERO
                       MOVE    SANTEI-SRYYM    TO  WRK-YYMM
                       MOVE    WRK-SDD         TO  WRK-DD
                       MOVE    1               TO  FLG-SANTEI
                   END-IF
               END-IF
      *
               IF     FLG-SANTEI           =   ZERO
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 920-SANTEI-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    IF      WRK-YMD(1:6)        =   SPACE
      *        MOVE    SPACE           TO  WRK-YMD
      *    END-IF
      *
           .
       10001-KYUSEI-YMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ開始日設定処理
      *****************************************************************
       10001-RIHA-STRYMD-SEC            SECTION.
      *
      *    MOVE    WRK-SRYCD           TO  TNS-SRYCD
      *    訂正時
           MOVE    ZERO                TO  FLG-TOUJITU
           IF      LNK-ORCSC-SYORIFLG      =   1
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ     >   100 )  OR
                                  (LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   SPACE )
                   IF      LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   WRK-SRYCD
                       MOVE    1           TO  FLG-TOUJITU
                   END-IF
               END-PERFORM
           END-IF
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
      *
      *    労災毎とする
           IF     (SPA-HKNKBN          =   1
                                       OR  2   )
               MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           END-IF
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           MOVE    SPACE               TO  WRK-YMD
           PERFORM
               UNTIL       FLG-SANTEI      =   1
               IF     (SANTEI-MSANTEICNT   >   ZERO )  AND
                      (SANTEI-SRYYM        <=  SPA-SRYYMD(1:6))
      *            最後の発生日、当日以降は対象外
                   IF      SANTEI-SRYYM        =   SPA-SRYYMD(1:6)
                       MOVE    SPA-SRYYMD(7:2)     TO  IDD2
      *                訂正分は対象外
                       IF      FLG-TOUJITU         =   1
                           COMPUTE IDD2            =   IDD2   -  1
                       END-IF
                   ELSE
                       MOVE    31                  TO  IDD2
                   END-IF
                   MOVE    ZERO                TO  WRK-SDD
                   PERFORM VARYING     IDD     FROM    IDD2  BY  -1
                           UNTIL       IDD     <   1
                       IF      SANTEI-DAY (IDD)    >   ZERO
                           MOVE    IDD             TO  WRK-SDD
      *                    終了日付検索
                           MOVE    ZERO            TO  FLG-CHK-END
                           PERFORM 100011-RIHA-SANTEIPLUS-SEC
                           IF      FLG-CHK-END     =   ZERO
                               MOVE    1               TO  IDD
                           END-IF
                       END-IF
                   END-PERFORM
                   IF      WRK-SDD             >   ZERO
                       MOVE    SANTEI-SRYYM    TO  WRK-YYMM
                       MOVE    WRK-SDD         TO  WRK-DD
                       MOVE    1               TO  FLG-SANTEI
                   END-IF
               END-IF
      *
               IF     FLG-SANTEI           =   ZERO
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 920-SANTEI-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       10001-RIHA-STRYMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ開始日算定履歴付加情報編集処理
      *****************************************************************
       100011-RIHA-SANTEIPLUS-SEC                SECTION.
      *
      *
           INITIALIZE                  SANTEIPLUS-REC
           MOVE    SANTEI-HOSPNUM      TO  SANTEIPLUS-HOSPNUM
           MOVE    SANTEI-PTID         TO  SANTEIPLUS-PTID
           MOVE    SANTEI-SRYCD        TO  SANTEIPLUS-SRYCD
           MOVE    SANTEI-SRYYM        TO  SANTEIPLUS-SRYYM
           MOVE    SANTEI-SRYKA        TO  SANTEIPLUS-SRYKA
           MOVE    SANTEI-NYUGAIKBN    TO  SANTEIPLUS-NYUGAIKBN
           MOVE    SANTEI-HKNCOMBINUM  TO  SANTEIPLUS-HKNCOMBINUM
           MOVE    WRK-SDD             TO  SANTEIPLUS-DAYKEY
           MOVE    ZERO                TO  SANTEIPLUS-RENNUM
      *
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santeiplus"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 921-SANTEIPLUS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEIPLUS
           END-IF
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SANTEIPLUS      =   ZERO
               IF      SANTEIPLUS-ENDYMD   <   SPA-SRYYMD
      *            システム日付より前の終了
                   MOVE    1                   TO  FLG-CHK-END
                   MOVE    ZERO                TO  WRK-SDD
               END-IF
           END-IF
      *
           .
       100011-RIHA-SANTEIPLUS-EXT.
           EXIT.
      *
      *****************************************************************
      *    精神通院の同日初診編集処理
      *****************************************************************
       10002-DOUJIT-YMD-SEC                SECTION.
      *
      *    開始日検索
           MOVE    "099830102"         TO  WRK-SRYCD
           PERFORM 10001-KYUSEI-YMD-SEC
           IF      WRK-YMD         NOT =   SPACE
               MOVE    WRK-YMD             TO  WRK-TUINSTR-YMD
               MOVE    "1"                 TO  WRK-TUINSTR-INT
               GO      TO      10002-DOUJIT-YMD-EXT
           END-IF
      *
      *    同日初診の診療科設定
           MOVE    SPA-SRYYMD          TO  WRK-SRYYMD-1
      *H25.3
      *    診療日付から前２年間で判定する
           MOVE    SPA-SRYYMD          TO  WRK-SRYYMD-1
           COMPUTE WRK-SRYYY           =   WRK-SRYYY   -   2
      *
           IF      WRK-SRYYM-1         <   "200604"
               MOVE    "200604"        TO  WRK-SRYYM-1
           END-IF
           IF      (SPA-DOUYMD         =   SPACE )  OR
                  ((SPA-DOUYMD     NOT =   SPACE )  AND
                   (SPA-SYOYMD         >   SPA-DOUYMD OR
                    SPA-SYOYMD-OLD     >   SPA-DOUYMD ))
               CONTINUE
           ELSE
      *        同日初診
               MOVE    "111011810"         TO  WRK-SRYCD
               PERFORM 10021-DOUSYOSIN-SRYKA-SEC
      *        同日初診（労災）
               MOVE    "101110040"         TO  WRK-SRYCD
               PERFORM 10021-DOUSYOSIN-SRYKA-SEC
      *        同日初診（紹介なし）
               MOVE    "111012610"         TO  WRK-SRYCD
               PERFORM 10021-DOUSYOSIN-SRYKA-SEC
           END-IF
           .
       10002-DOUJIT-YMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日初診算定処理
      *****************************************************************
       10021-DOUSYOSIN-SRYKA-SEC                SECTION.
      *
      *    算定履歴検索
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
      *    労災毎とする
           IF     (SPA-HKNKBN          =   1   )  AND
                  (WRK-SRYCD           =   "101110040")
               MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           END-IF
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           MOVE    SPACE               TO  WRK-YMD
           PERFORM
               UNTIL       FLG-SANTEI      =   1
      *
               IF     (SANTEI-MSANTEICNT   >   ZERO )  AND
                      (SANTEI-SRYYM        >=  WRK-SRYYM-1 )  AND
                      (SANTEI-SRYYM        <=  SPA-SRYYMD(1:6))
      *            最後の発生日、当日以降は対象外
                   IF      SANTEI-SRYYM        =   SPA-SRYYMD(1:6)
                       MOVE    SPA-SRYYMD(7:2)     TO  IDD2
                   ELSE
                       MOVE    31                  TO  IDD2
                   END-IF
                   MOVE    ZERO                TO  WRK-DD
                   PERFORM VARYING     IDD     FROM    IDD2  BY  -1
                           UNTIL       IDD     <   1
                       IF      SANTEI-DAY (IDD)    >   ZERO
                           MOVE    IDD             TO  WRK-DD
                           MOVE    1               TO  IDD
                       END-IF
                   END-PERFORM
                   IF      WRK-DD              >   ZERO
                       PERFORM 100211-DOU-SANTEI-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       10021-DOUSYOSIN-SRYKA-EXT.
           EXIT.
      *****************************************************************
      *    同日初診診療科設定処理 
      *****************************************************************
       100211-DOU-SANTEI-SEC               SECTION.
      *
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    SANTEI-SRYYM        TO  SRY-SRYYM
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(1)
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(2)
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(3)
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(4)
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(5)
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
           PERFORM
                   UNTIL   FLG-SRYACT      =   1
      *        剤回数取得
               MOVE    SPACE               TO  SRYACCT-REC
               INITIALIZE                      SRYACCT-REC
               MOVE    SRY-HOSPNUM         TO  ACCT-HOSPNUM
               MOVE    SRY-NYUGAIKBN       TO  ACCT-NYUGAIKBN
               MOVE    SRY-PTID            TO  ACCT-PTID
               MOVE    SRY-SRYKA           TO  ACCT-SRYKA
               MOVE    SRY-SRYYM           TO  ACCT-SRYYM
               MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
               MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
      *        主キー読み
               PERFORM 940-SRYACCT-READ-SEC
               IF      FLG-SRYACCT         =   ZERO
                   MOVE    SPACE           TO  WRK-YMD
                   MOVE    ACCT-SRYYM      TO  WRK-YMD(1:6)
                   MOVE    ZERO            TO  WRK-DD
                   PERFORM VARYING     IDX-Y2  FROM    31 BY  -1
                           UNTIL       IDX-Y2  <   1
                       IF      ACCT-DAY (1 IDX-Y2) >   ZERO
                           MOVE    IDX-Y2          TO  WRK-DD
                           IF      WRK-YMD         >   SPA-SRYYMD
                               MOVE    ZERO        TO  WRK-DD
                           ELSE
                               MOVE    1               TO  IDX-Y2
                           END-IF
                       END-IF
                   END-PERFORM
                   IF      WRK-DD              =   ZERO
                       MOVE    SPACE           TO  WRK-YMD
                   END-IF
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL      (IDX     >   10  )
                                  OR  (WRK-YMD =   SPACE )
                       IF      WRK-DOU-SRYKA (IDX) =   SPACE
                           MOVE    ACCT-SRYKA      TO  WRK-DOU-SRYKA
                                                             (IDX)
                           MOVE    WRK-YMD         TO  WRK-DOU-SRYYMD
                                                             (IDX)
                           MOVE    10              TO  IDX
                       ELSE
                           IF      WRK-DOU-SRYKA (IDX)
                                                   =   ACCT-SRYKA
                               IF     WRK-DOU-SRYYMD (IDX)
                                                   <   WRK-YMD
                                   MOVE    WRK-YMD TO  WRK-DOU-SRYYMD
                                                             (IDX)
                               END-IF
                               MOVE    10              TO  IDX
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       300911-DOUSYOSIN-SRYKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤別のチェック、編集、自動算定処理
      *****************************************************************
       100-SEL-SYORI-SEC                SECTION.
      *
      *    チェック、自動算定
           PERFORM 1001-NAI-CHK-SEC
           IF      SPA-ERRCD(1:1)  =   "K"
               MOVE    SPA-ERRCD       TO  WRK-ERRCD-H
               MOVE    SPACE           TO  SPA-ERRCD
           ELSE
               MOVE    SPACE           TO  WRK-ERRCD-H
           END-IF
           IF     (WRK-GMN-INPUTCD(IDX-MAX)(1:1)
                                       =   "."   )  OR
                  (SPA-ERRCD       NOT =   SPACE )  OR
                  (LNK-ORCSC-KIDCD NOT =   SPACE )
               GO      TO      100-SEL-SYORI-EXT
           END-IF
      *
      *    点数計算
           PERFORM 1002-TENSU-KEI-SEC
           IF      WRK-ERRCD-H     NOT =   SPACE
               MOVE    WRK-ERRCD-H     TO  SPA-ERRCD
           END-IF
      *
           .
       100-SEL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細別内容チェック、自動算定編集処理
      *****************************************************************
       1001-NAI-CHK-SEC                  SECTION.
      *
      *    剤内に早期リハビリ存在チェック
      **** PERFORM 10010-SOUKI-CHK-SEC
      *
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  IDX-SYU
           INITIALIZE                  WRK-AREA
           MOVE    ZERO                TO  IDX-ZS
           MOVE    ZERO                TO  IDX-ZS2
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )   OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "." OR "S"
                                                               OR "*"
                   MOVE    WRK-GMN-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
               ELSE
                   MOVE    WRK-COM-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
                   IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059"
                       MOVE    "7"                     TO  WRK-MOJI
                   END-IF
               END-IF
      *        労災加算振替え
               IF      WRK-COM-INPUTCD(IDX-Z)  =   "099509901"
                   MOVE    "1"                 TO  WRK-MOJI
               END-IF
      *
               EVALUATE    WRK-MOJI
                   WHEN    "0"
                       PERFORM 10012-CHK2-SEC
                   WHEN    "8"
                   WHEN    "."
                   WHEN    "S"
                   WHEN    "*"
                       CONTINUE
      *
                   WHEN    OTHER
      *                編集チェックへ
                       PERFORM 10011-CHK-SEC
               END-EVALUATE
           END-PERFORM
      *
      *H24.4
           IF     (WRK-ERRCD           =   SPACE)   AND
                  (IDX-ZS              >   ZERO )
      *        乳幼児の自動加算を追加する
               PERFORM 1002-JIDO-KASAN-SEC
           END-IF
      *
      *    酸素の補正率を自動発生する
           IF     (WRK-ERRCD           =   SPACE)   AND
                  (IDX-SANSO           >   ZERO )
               PERFORM 10011-SANSO-ADD-SEC
           END-IF
      *
           MOVE    WRK-ERRCD          TO  SPA-ERRCD
      *
           .
       1001-NAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細別内容チェック、自動算定編集処理
      *****************************************************************
       10011-CHK-SEC                  SECTION.
      *
      *    各チェック処理
           EVALUATE    WRK-MOJI
               WHEN    "1"
      *H24.4
      *            手技料
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "1"
                       IF      IDX-ZS          >   ZERO
      *                    乳幼児の自動加算を追加する
                           PERFORM 1002-JIDO-KASAN-SEC
                       END-IF
                       MOVE    IDX-Z           TO  IDX-ZS
                       MOVE    IDX-Z           TO  IDX-ZS2
                   END-IF
      *            施設基準不適合チェック
                   IF      WRK-COM-KIJUNTEIGENKBN(IDX-Z)
                                               NOT =   ZERO
                       PERFORM 100111-KIJUNN-CHK-SEC
                   END-IF
      *
      *            労災の時、労災加算の算定
                   IF      SPA-HKNKBN          =   1
                     AND  (SPA-RSI-JUNKYO  NOT =   "1" )
                       PERFORM 100113-RSIHKN-SYORI-SEC
                   ELSE
                       IF      WRK-COM-DATAKBN (IDX-Z) =   "1"
      *                    リハビリの上限回数
                           MOVE    SPACE               TO  WRK-GEN-SRYCD
                           IF      LNK-ORCSC-PG        =   "K02"
                               PERFORM 100112-RIGAKU-CHK-SEC
                           END-IF
                       END-IF
                   END-IF
      *
      *            精神通院のチェック
                   IF      LNK-ORCSC-PG        =   "K02"
                       PERFORM 100113-SEISIN-CHK-SEC
                   END-IF
      *ver.4.7
      *            加算チェック
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "2"
                       PERFORM 1005-KASAN-CHK-SEC
                   END-IF
      *        薬剤
               WHEN    "6"
      *            残量廃棄
                   PERFORM 1006-HAIKI-SYORI-SEC
      *        器材
               WHEN    "7"
      ***********  IF      WRK-COM-DATAKBN (IDX-Z) =   "3"
      *                フィルムの時エラー
      *                MOVE    "0047"          TO  WRK-ERRCD
      *                MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
      ************ END-IF
      *            酸素区分判定
                   IF      WRK-COM-SANSOKBN (IDX-Z)    =   1
      *                酸素の時、酸素補正率を自動加算
                       MOVE    WRK-COM-INPUTCD (IDX-Z) TO  WRK-SRYCD
                       PERFORM 100115-SANSO-CHK-SEC
                       IF      FLG-SANSO-OK        =   1
                           MOVE    IDX-Z       TO  IDX-SANSO
                       ELSE
                           IF      IDX-SANSO        =   ZERO
                               MOVE    "0038"       TO  WRK-ERRCD
                               MOVE    "1"          TO  WRK-COM-CUR
                                                              (IDX-Z)
                           END-IF
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       10011-CHK-EXT.
           EXIT.
      *****************************************************************
      *    酸素判定処理
      *****************************************************************
       100115-SANSO-CHK-SEC           SECTION.
      *
      *
           SET     TBL-SANSO           TO  1
           SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
                   AT  END
                       MOVE    ZERO            TO  FLG-SANSO-OK
                   WHEN    WRK-SRYCD       =   TBL-SANSO-SRYCD
                                                           (TBL-SANSO)
                       MOVE    1               TO  FLG-SANSO-OK
           END-SEARCH 
      *
           .
       100115-SANSO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素補正率自動加算処理
      *****************************************************************
       10011-SANSO-ADD-SEC                  SECTION.
      *
           MOVE    "770020070"         TO  WRK-SRYCD
           MOVE    ZERO                TO  FLG-ARI
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1    >   SPA-MAX-LINE  )  OR
                              (WRK-COM-INPUTCD(IDX1)   =   SPACE AND
                               WRK-GMN-INPUTCD(IDX1)   =   SPACE )
               IF       WRK-COM-INPUTCD (IDX1) =   WRK-SRYCD
                   MOVE    IDX1                TO  IDX-T
                   MOVE    1                   TO  FLG-ARI
               END-IF
           END-PERFORM
           IF      FLG-ARI             =   ZERO
               MOVE    IDX-SANSO           TO  IDX-S
           END-IF
      *    酸素補正率自動加算
           PERFORM 1002-KASAN-SET-SEC
      *
           .
       10011-SANSO-ADD-EXT.
           EXIT.
      *****************************************************************
      *    残量廃棄コメントの有無
      *****************************************************************
       1006-HAIKI-SYORI-SEC           SECTION.
      *
      *    アンプルの時、小数以下判定
           IF     (WRK-COM-TANICD (IDX-Z)  =   "014" OR "022" OR "046" )
           AND    (LNK-ORCSC-HAIKIKBN-FLG  =   "2"            )
      *
               MOVE    WRK-COM-SURYO(IDX-Z)    TO  WRK-SURYO
               IF     (WRK-SURYO-S2                >   ZERO    )  AND
                      (WRK-COM-AUTOFLG2 (IDX-Z)    =   SPACE   )
                   PERFORM 10011-HAIKI-ADD-SEC
               END-IF
           END-IF
           .
       1006-HAIKI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    破棄コメント自動発生成処理
      *****************************************************************
       10011-HAIKI-ADD-SEC             SECTION.
      *
           MOVE    "099309901"         TO  WRK-SRYCD
           IF      WRK-COM-INPUTCD (IDX-Z + 1) =   WRK-SRYCD
               CONTINUE
           ELSE
               MOVE    IDX-Z               TO  IDX-S
               MOVE    ZERO                TO  FLG-ARI
               PERFORM 1002-KASAN-SET-SEC
           END-IF
           .
       10011-HAIKI-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    急性発生日チェック処理
      *****************************************************************
       10012-CHK2-SEC                  SECTION.
      *
      *    急性発生終了
           IF      WRK-COM-INPUTCD(IDX-Z)  =   "099800102"
               IF      SPA-HKNKBN          =   1
                   MOVE    "8025"              TO  WRK-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
               END-IF
           END-IF
      *
      *    診療行為のみでチェック
           IF     (WRK-ERRCD           =   SPACE  )  AND
                  (LNK-ORCSC-PG        =   "K02"  )
               PERFORM 100121-KYSEI-CHK-SEC
           END-IF
      *
      *    自賠責明細料・診断料の時、数量は１であること
           IF     (WRK-COM-INPUTCD(IDX-Z)(1:5) =   "09591"  OR
                                                   "09592"  OR
                                                   "09593"  OR
                                                   "09594"  )  AND
      ****        (WRK-COM-TENOTEN (IDX-Z)     >   ZERO     )
                  (WRK-COM-TEN     (IDX-Z)     >   ZERO     )
      *D       IF      WRK-COM-SURYO(IDX-Z)    >   1
               IF      WRK-COM-SURYO(IDX-Z)    NOT =   1
                   MOVE    "1002"              TO  WRK-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
               END-IF
           END-IF
      *
      *ver.4.7
      *    精神科ケア開始日
           IF     (LNK-ORCSC-PG        =   "K02"  )  AND
                  (WRK-COM-INPUTCD (IDX-Z) =   "099830101" )
               MOVE    "2"                 TO  WRK-KEASOKI-INT
           END-IF
      *
      *    通院・在宅精神開始日
           IF     (LNK-ORCSC-PG        =   "K02"  )  AND
                  (WRK-COM-INPUTCD (IDX-Z) =   "099830102" )
               MOVE    "2"                 TO  WRK-TUINSTR-INT
           END-IF
      *
           .
       10012-CHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    急性発生日チェック処理
      *****************************************************************
       100121-KYSEI-CHK-SEC                  SECTION.
      *
      *    急性発生日
           IF      WRK-COM-INPUTCD(IDX-Z)  =   "099800101"
               IF      SPA-KYUSEI-END      =   SPA-SRYYMD
                   MOVE    "8023"             TO  WRK-ERRCD
                   MOVE    "1"                TO  WRK-COM-CUR (IDX-Z)
               END-IF
           END-IF
      *    急性発生終了
           IF      WRK-COM-INPUTCD(IDX-Z)  =   "099800102"
               IF     (SPA-KYUSEI-FLG      =   1    )  OR
                      (SPA-KYUSEI-YMD      =   SPA-SRYYMD)
                   MOVE    "8023"             TO  WRK-ERRCD
                   MOVE    "1"                TO  WRK-COM-CUR (IDX-Z)
               ELSE
                   IF     (SPA-KYUSEI-YMD  NOT =   SPACE )  AND
                          (SPA-KYUSEI-END      =   SPACE )
                       CONTINUE
                   ELSE
                       MOVE    "8024"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   END-IF
               END-IF
           END-IF
      *    リハビリ終了日
           EVALUATE    WRK-COM-INPUTCD(IDX-Z)
      *        心大血管疾患リハビリテーション
               WHEN    "099800111"
                   MOVE    "1"             TO  WRK-KYUSEI-ST (1)
               WHEN    "099800112"
                   MOVE    "1"             TO  WRK-KYUSEI-ED (1)
                   IF     (WRK-KYUSEI1-YMD     =   SPACE  )  AND
                          (WRK-KYUSEI-ST (1)   =   SPACE  )
                       MOVE    "8029"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   ELSE
                       IF     (WRK-KYUSEI1-YMD     =   SPA-SRYYMD) OR
                              (WRK-KYUSEI-ST (1)   =   "1"    )
                           MOVE    "8030"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR(IDX-Z)
                       END-IF
                   END-IF
      *        脳血管疾患リハビリテーション
               WHEN    "099800121"
                   MOVE    "1"             TO  WRK-KYUSEI-ST (2)
               WHEN    "099800122"
                   IF     (WRK-KYUSEI2-YMD     =   SPACE  )  AND
                          (WRK-KYUSEI-ST (2)   =   SPACE  )
                       MOVE    "8031"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   ELSE
                       IF     (WRK-KYUSEI2-YMD     =   SPA-SRYYMD) OR
                              (WRK-KYUSEI-ST (2)   =   "1"    )
                           MOVE    "8032"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR(IDX-Z)
                       END-IF
                   END-IF
      *        運動器リハビリテーション
               WHEN    "099800131"
                   MOVE    "1"             TO  WRK-KYUSEI-ST (3)
               WHEN    "099800132"
                   IF     (WRK-KYUSEI3-YMD     =   SPACE  )  AND
                          (WRK-KYUSEI-ST (3)   =   SPACE  )
                       MOVE    "8033"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   ELSE
                       IF     (WRK-KYUSEI3-YMD     =   SPA-SRYYMD) OR
                              (WRK-KYUSEI-ST (3)   =   "1"    )
                           MOVE    "8034"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR(IDX-Z)
                       END-IF
                   END-IF
      *        呼吸器リハビリテーション
               WHEN    "099800141"
                   MOVE    "1"             TO  WRK-KYUSEI-ST (4)
               WHEN    "099800142"
                   IF     (WRK-KYUSEI4-YMD     =   SPACE  )  AND
                          (WRK-KYUSEI-ST (4)   =   SPACE  )
                       MOVE    "8035"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   ELSE
                       IF     (WRK-KYUSEI4-YMD     =   SPA-SRYYMD) OR
                              (WRK-KYUSEI-ST (4)   =   "1"    )
                           MOVE    "8036"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR(IDX-Z)
                       END-IF
                   END-IF
      *        摂食機能療法終了日
               WHEN    "099800151"
                   MOVE    "1"             TO  WRK-KYUSEI-ST (5)
               WHEN    "099800152"
                   IF      WRK-KYUSEI5-CHK     =   SPACE
                       MOVE    "099800151"         TO  WRK-SRYCD
      ************     PERFORM 10001-KYUSEI-YMD-SEC
                       PERFORM 10001-RIHA-STRYMD-SEC
                       MOVE    WRK-YMD             TO  WRK-KYUSEI5-YMD
                       MOVE    "1"                 TO  WRK-KYUSEI5-CHK
                   END-IF
                   IF     (WRK-KYUSEI5-YMD     =   SPACE  )  AND
                          (WRK-KYUSEI-ST (5)   =   SPACE  )
                       MOVE    "8037"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   ELSE
                       IF     (WRK-KYUSEI5-YMD     =   SPA-SRYYMD) OR
                              (WRK-KYUSEI-ST (5)   =   "1"    )
                           MOVE    "8038"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR(IDX-Z)
                       END-IF
                   END-IF
      *        難病患者リハビリ
               WHEN    "099800161"
                   MOVE    "1"             TO  WRK-KYUSEI-ST (6)
               WHEN    "099800162"
                   MOVE    "1"             TO  WRK-KYUSEI-ED (6)
                   IF      WRK-KYUSEI6-CHK     =   SPACE
                       MOVE    "099800161"         TO  WRK-SRYCD
      ************     PERFORM 10001-KYUSEI-YMD-SEC
                       PERFORM 10001-RIHA-STRYMD-SEC
                       MOVE    WRK-YMD             TO  WRK-KYUSEI6-YMD
                       MOVE    "1"                 TO  WRK-KYUSEI6-CHK
                   END-IF
                   IF     (WRK-KYUSEI6-YMD     =   SPACE  )  AND
                          (WRK-KYUSEI-ST (6)   =   SPACE  )
                       MOVE    "8039"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   ELSE
                       IF     (WRK-KYUSEI6-YMD     =   SPA-SRYYMD) OR
                              (WRK-KYUSEI-ST (6)   =   "1"    )
                           MOVE    "8040"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR(IDX-Z)
                       END-IF
                   END-IF
      *        障害者リハビリ
               WHEN    "099800171"
                   MOVE    "1"             TO  WRK-KYUSEI-ST (7)
               WHEN    "099800172"
                   MOVE    "1"             TO  WRK-KYUSEI-ED (7)
                   IF      WRK-KYUSEI7-CHK     =   SPACE
                       MOVE    "099800171"         TO  WRK-SRYCD
      ************     PERFORM 10001-KYUSEI-YMD-SEC
                       PERFORM 10001-RIHA-STRYMD-SEC
                       MOVE    WRK-YMD             TO  WRK-KYUSEI7-YMD
                       MOVE    "1"                 TO  WRK-KYUSEI7-CHK
                   END-IF
                   IF     (WRK-KYUSEI7-YMD     =   SPACE  )  AND
                          (WRK-KYUSEI-ST (7)   =   SPACE  )
                       MOVE    "8041"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   ELSE
                       IF     (WRK-KYUSEI7-YMD     =   SPA-SRYYMD) OR
                              (WRK-KYUSEI-ST (7)   =   "1"    )
                           MOVE    "8042"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR(IDX-Z)
                       END-IF
                   END-IF
      *
      *        がん患者リハビリ
               WHEN    "099800181"
                   MOVE    "1"             TO  WRK-KYUSEI-ST (8)
               WHEN    "099800182"
                   MOVE    "1"             TO  WRK-KYUSEI-ED (8)
                   IF      WRK-KYUSEI8-CHK     =   SPACE
                       MOVE    "099800181"         TO  WRK-SRYCD
      ************     PERFORM 10001-KYUSEI-YMD-SEC
                       PERFORM 10001-RIHA-STRYMD-SEC
                       MOVE    WRK-YMD             TO  WRK-KYUSEI8-YMD
                       MOVE    "1"                 TO  WRK-KYUSEI8-CHK
                   END-IF
                   IF     (WRK-KYUSEI8-YMD     =   SPACE  )  AND
                          (WRK-KYUSEI-ST (8)   =   SPACE  )
                       MOVE    "8048"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   ELSE
                       IF     (WRK-KYUSEI8-YMD     =   SPA-SRYYMD) OR
                              (WRK-KYUSEI-ST (8)   =   "1"    )
                           MOVE    "8049"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR(IDX-Z)
                       END-IF
                   END-IF
      *
               WHEN    "099800401"
                   MOVE    "1"                 TO  WRK-TEIGEN-IN
           END-EVALUATE
      *
           .
       100121-KYSEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算料チェック処理
      *****************************************************************
       1005-KASAN-CHK-SEC                  SECTION.
      *
      *    加算データの時、前に手技料があること
           MOVE    ZERO                TO  FLG-ARI
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1    >=  IDX-Z
               IF     (WRK-COM-INPUTCD(IDX1)(1:1)  =   "1" )  AND
                      (WRK-COM-DATAKBN(IDX1)   NOT =   "2" )
                   MOVE    1               TO  FLG-ARI
                   MOVE    IDX-Z           TO  IDX1
               END-IF
           END-PERFORM
      *
           IF      FLG-ARI             =   ZERO
               MOVE    "0013"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
           END-IF
      *
      *    早期リハビリ加算チェック
           IF     (WRK-ERRCD           =   SPACE  )
             AND  (LNK-ORCSC-PG        =   "K02"  )
             AND  (WRK-COM-INPUTCD (IDX-Z) =   CON-RIHAKSN )
      *      包括まとめ入力は対象外
             AND  (WRK-COM-HKNCOMBI (IDX-Z)
                                   NOT =   "9999"   )
      *
               PERFORM 20011-RIHAKSN-CHK-SEC
           END-IF
      *H24.4
      *    初期加算（リハビリテーション料）
           IF     (WRK-ERRCD           =   SPACE  )
             AND  (LNK-ORCSC-PG        =   "K02"  )
             AND  (WRK-COM-INPUTCD (IDX-Z) =   CON-SYOKIKSN)
      *      包括まとめ入力は対象外
             AND  (WRK-COM-HKNCOMBI (IDX-Z)
                                   NOT =   "9999"   )
      *
               PERFORM 20011-SYOKIKSN-CHK-SEC
           END-IF
      *
      *
      *ver.4.7
      *    精神科ケア早期加算チェック
           IF     (WRK-ERRCD           =   SPACE  )
             AND  (LNK-ORCSC-PG        =   "K02"  )
             AND  (WRK-COM-INPUTCD (IDX-Z) =   CON-KEAKSN1
                                           OR  CON-KEAKSN2
                                           OR  CON-KEAKSN3
                                           OR  CON-KEAKSN4
                                           OR  CON-KEAKSN5)
      *      包括まとめ入力は対象外
             AND  (WRK-COM-HKNCOMBI (IDX-Z)
                                   NOT =   "9999"   )
      *
               PERFORM 10053-KEAKASAN-CHK-SEC
           END-IF
           .
       1005-KASAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    早期リハビリ加算チェック処理
      *****************************************************************
       20011-RIHAKSN-CHK-SEC                SECTION.
      *
      *    各リハビリ発症日判定
           IF      WRK-KYUSEI-CHK      =   SPACE
               IF     (WRK-COM-KZMFLG (IDX-Z)   =   SPACE)  AND
                      (LNK-ORCSC-KEI-IDX        =   ZERO )
                   MOVE    IDX-Z               TO  LNK-ORCSC-KEI-IDX
                   MOVE    "1"                 TO  WRK-COM-KZMFLG
                                                           (IDX-Z)
                   MOVE    "K839"              TO  WRK-ERRCD
               END-IF
           ELSE
      *        発症日＋３０日
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-KYUSEI-CHK      TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    30                  TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KOYOMI     TO  WRK-KYUSEI-30
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    LNK-DAY6-KOYOMI     TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               STRING  "発症日の３０日後は"  DELIMITED  BY  SIZE
                       LNK-DAY2-EDTYMD3      DELIMITED  BY  SIZE
                       "です"                DELIMITED  BY  SIZE
                      INTO                 WRK-KYUSEI-30-MSG
               END-STRING
      *H26.4   外来あり
               IF      SPA-NYUGAIKBN       =   "2"
      *            外来
                   MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
                   PERFORM 200111-RIHAKSN-CHK02-SEC
               ELSE
      *            入院　入力日分のチェック
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2    >   31  )   OR
                                  (WRK-ERRCD   NOT =   SPACE )
                       IF      WRK-COM-NYUINDAY (IDX-Z IDX2)   >   ZERO
                           MOVE    IDX2            TO  IDD
                           MOVE    SPA-SRYYMD      TO  WRK-SRYYMD
                           MOVE    IDD             TO  WRK-SRYYMD (7:2)
                           PERFORM 200111-RIHAKSN-CHK02-SEC
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
           .
       20011-RIHAKSN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    早期リハビリ加算チェック処理
      *****************************************************************
       200111-RIHAKSN-CHK02-SEC                SECTION.
      *
           IF     (WRK-KYUSEI-30       <   WRK-SRYYMD)
      *        発生日＋３０日以上はエラー
               MOVE    "8047"              TO  WRK-ERRCD
               MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
               MOVE    WRK-KYUSEI-30-MSG   TO  LNK-ORCSC-ERRMSG
      *        労災は警告とする
               IF     (SPA-HKNKBN          =   1   )
                 AND  (SPA-RSI-JUNKYO  NOT =   "1" )
                   IF     (WRK-COM-KZMFLG (IDX-Z)  =   SPACE)
                   AND    (LNK-ORCSC-KEI-IDX       =   ZERO )
                       MOVE    "K845"          TO  WRK-ERRCD
                       MOVE    IDX-Z           TO  LNK-ORCSC-KEI-IDX
                       MOVE    "1"             TO  WRK-COM-KZMFLG
                                                              (IDX-Z)
                   ELSE
                       MOVE    SPACE           TO  WRK-ERRCD
                       MOVE    SPACE           TO  LNK-ORCSC-ERRMSG
                       MOVE    SPACE           TO  WRK-COM-CUR(IDX-Z)
                   END-IF
               END-IF
           END-IF
           .
       200111-RIHAKSN-CHK02-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期加算（リハビリテーション料）チェック処理（入院）
      *****************************************************************
       20011-SYOKIKSN-CHK-SEC                SECTION.
      *
      *    各リハビリ発症日判定
           IF      WRK-KYUSEI-CHK      =   SPACE
               IF     (WRK-COM-KZMFLG (IDX-Z)   =   SPACE)  AND
                      (LNK-ORCSC-KEI-IDX        =   ZERO )
                   MOVE    IDX-Z               TO  LNK-ORCSC-KEI-IDX
                   MOVE    "1"                 TO  WRK-COM-KZMFLG
                                                           (IDX-Z)
                   MOVE    "K839"              TO  WRK-ERRCD
               END-IF
           ELSE
      *        発症日＋１４日
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-KYUSEI-CHK      TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    14                  TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KOYOMI     TO  WRK-KYUSEI-14
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    LNK-DAY6-KOYOMI     TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               STRING  "発症日の１４日後は"  DELIMITED  BY  SIZE
                       LNK-DAY2-EDTYMD3      DELIMITED  BY  SIZE
                       "です"                DELIMITED  BY  SIZE
                      INTO                 WRK-KYUSEI-14-MSG
               END-STRING
      *H26.4   外来追加
               IF      SPA-NYUGAIKBN       =   "2"
      *            外来
                   MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
                   PERFORM 200111-SYOKIKSN-CHK02-SEC
               ELSE
      *            入院　入力日分のチェック
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL      (IDX2    >   31  )   OR
                                      (WRK-ERRCD   NOT =   SPACE )
                       IF      WRK-COM-NYUINDAY (IDX-Z IDX2)   >   ZERO
                           MOVE    IDX2            TO  IDD
                           MOVE    SPA-SRYYMD      TO  WRK-SRYYMD
                           MOVE    IDD             TO  WRK-SRYYMD (7:2)
                           PERFORM 200111-SYOKIKSN-CHK02-SEC
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
           .
       20011-SYOKIKSN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初期加算（リハビリテーション料）チェック処理
      *****************************************************************
       200111-SYOKIKSN-CHK02-SEC                SECTION.
      *
           IF     (WRK-KYUSEI-14       <   WRK-SRYYMD)
      *        発生日＋１４日以上はエラー
               MOVE    "8070"              TO  WRK-ERRCD
               MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
               MOVE    WRK-KYUSEI-14-MSG   TO  LNK-ORCSC-ERRMSG
      *        労災は警告とする
               IF     (SPA-HKNKBN          =   1   )
                 AND  (SPA-RSI-JUNKYO  NOT =   "1" )
                   IF     (WRK-COM-KZMFLG (IDX-Z)  =   SPACE)
                   AND    (LNK-ORCSC-KEI-IDX       =   ZERO )
                       MOVE    "K870"          TO  WRK-ERRCD
                       MOVE    IDX-Z           TO  LNK-ORCSC-KEI-IDX
                       MOVE    "1"             TO  WRK-COM-KZMFLG
                                                             (IDX-Z)
                   ELSE
                       MOVE    SPACE           TO  WRK-ERRCD
                       MOVE    SPACE           TO  LNK-ORCSC-ERRMSG
                       MOVE    SPACE           TO  WRK-COM-CUR (IDX-Z)
                   END-IF
               END-IF
           END-IF
           .
       200111-SYOKIKSN-CHK02-EXT.
           EXIT.
      *****************************************************************
      *    精神科ケア早期加算チェック処理
      *****************************************************************
       10053-KEAKASAN-CHK-SEC                SECTION.
      *
           IF      WRK-KEASOKI-INT         =   SPACE
      *        開始日検索
               MOVE    "099830101"         TO  WRK-SRYCD
               PERFORM 10001-KYUSEI-YMD-SEC
               MOVE    WRK-YMD             TO  WRK-KEASOKI-YMD
               MOVE    "1"                 TO  WRK-KEASOKI-INT
           END-IF
      *
           IF     (WRK-COM-KZMFLG (IDX-Z)  =   SPACE)
           AND    (LNK-ORCSC-KEI-IDX       =   ZERO )
           AND    (WRK-KEASOKI-INT         =   "1"  )
           AND    (WRK-KEASOKI-YMD     NOT =   SPACE)
      *        開始日から１年後はエラー
               MOVE    WRK-KEASOKI-YMD         TO  WRK-KEASTRYMD
               COMPUTE WRK-KEASTR-YY       =   WRK-KEASTR-YY   +   1
               IF      WRK-KEASTRYMD       <=  SPA-SRYYMD
                   MOVE    IDX-Z               TO  LNK-ORCSC-KEI-IDX
                   MOVE    "1"                 TO  WRK-COM-KZMFLG
                                                           (IDX-Z)
                   MOVE    "K270"              TO  WRK-ERRCD
      *            発生日
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-KEASOKI-YMD     TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY2-AREA
                   MOVE    SPACE               TO  LNK-ORCSC-ERRMSG
                   STRING  "精神科ケア早期加算開始日："
                                                   DELIMITED  BY  SIZE
                           LNK-DAY2-EDTYMD3        DELIMITED  BY  SIZE
                           INTO                    LNK-ORCSC-ERRMSG
                   END-STRING
               END-IF
           END-IF
           .
       10053-KEAKASAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設基準不適合チェック加算処理
      *****************************************************************
       100111-KIJUNN-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-ERRCHK
      *    手技の時、適合チェック
           IF      WRK-COM-KIJUNTEIGENKBN (IDX-Z)  =   2  OR  3
               PERFORM 1001111-SSTKIJUN-CHK-SEC
      *        施設基準適応
               IF      FLG-ERR             =   ZERO
                   MOVE    1               TO  FLG-KIJUNN
                   GO      TO      100111-KIJUNN-CHK-EXT
               END-IF
      *        施設基準不適応
               IF     (WRK-COM-KIJUNTEIGENKBN (IDX-Z)  =   2 )  OR
                     ((WRK-COM-KIJUNTEIGENKBN (IDX-Z)  =   3   ) AND
                      (LNK-ORCSC-NENREI-YY             =   ZERO))
                   MOVE    SPACE               TO  WRK-SRYCD
      *            施設基準不適合（放射線）
                   IF      WRK-COM-CDKBN-KBN (IDX-Z)   =   "M"
                       MOVE    "180025270"         TO  WRK-SRYCD
                   END-IF
      *            施設基準不適合（脳血管疾患リハビリ）
                   IF     (WRK-COM-CDKBN-KBN (IDX-Z)   =   "H" )
                      AND (WRK-COM-CDKBN-KBNNUM(IDX-Z) =   "001")
      **廃止*******    MOVE    "180042770"         TO  WRK-SRYCD
                       MOVE    1                   TO  WRK-ERRCHK
                   END-IF
      *            施設基準不適合（運動器リハビリ）
                   IF     (WRK-COM-CDKBN-KBN (IDX-Z)   =   "H" )
                      AND (WRK-COM-CDKBN-KBNNUM(IDX-Z) =   "002")
      **廃止*******    MOVE    "180043370"         TO  WRK-SRYCD
                       MOVE    1                   TO  WRK-ERRCHK
                   END-IF
      *            不適合あり
                   IF      WRK-SRYCD       NOT =   SPACE
                       MOVE    2                   TO  FLG-KIJUNN
                       MOVE    IDX-Z               TO  IDX-S
                       IF      WRK-COM-INPUTCD (IDX-Z + 1) =   WRK-SRYCD
                           CONTINUE
                       ELSE
                           MOVE    ZERO            TO  FLG-ARI
                           PERFORM 1002-KASAN-SET-SEC
                      END-IF
                  END-IF
              END-IF
           END-IF
      *    外来リハビリ（要介護）施設基準不適合判定
           IF     (WRK-ERRCHK          =   1  )
            AND   (SPA-NYUGAIKBN       =   "2")
               MOVE    "0024"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
           END-IF
      *
      *    加算の時、対象の手技が有ること
           IF      WRK-COM-KIJUNTEIGENKBN (IDX-Z)  =   1
               IF      FLG-KIJUNN          =   ZERO
                   MOVE    "0073"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               END-IF
           END-IF
           .
       100111-KIJUNN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設基準不適応チェック処理
      *****************************************************************
       1001111-SSTKIJUN-CHK-SEC        SECTION.
      *
           MOVE    WRK-COM-KIJUNTEIGENCD (IDX-Z)
                                       TO  IDY
      *
           MOVE    ZERO                TO  FLG-ERR
           MOVE    ZERO                TO  IDS
      *    施設基準情報検索
           MOVE    SPACE               TO  SYS-1006-REC
           MOVE    "1006"              TO  SYS-1006-KANRICD
           EVALUATE    TRUE
               WHEN    IDY             <=  500
                   MOVE    "1"                 TO  SYS-1006-KBNCD
                   MOVE    ZERO                TO  IDS
               WHEN    IDY             <=  1000
                   MOVE    "2"                 TO  SYS-1006-KBNCD
                   MOVE    500                 TO  IDS
               WHEN    IDY             <=  3500
                   MOVE    "7"                 TO  SYS-1006-KBNCD
                   MOVE    3000                TO  IDS
               WHEN    IDY             <=  8500
                   MOVE    "17"                TO  SYS-1006-KBNCD
                   MOVE    8000                TO  IDS
           END-EVALUATE
           MOVE    SPA-SRYYMD          TO  SYS-1006-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1006-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1006-HOSPNUM
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1006-REC
               ELSE
                   INITIALIZE                  SYS-1006-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYS-1006-REC
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    基準不適合逓減対象施設基準
           IF      SYS-1006-SSTKIJUN (IDY - IDS)   =   1
               CONTINUE
           ELSE
               MOVE    1                   TO  FLG-ERR
           END-IF
      *
           .
       1001111-SSTKIJUN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリの回数チェック処理
      *****************************************************************
       100112-RIGAKU-CHK-SEC            SECTION.
      *
      *    包括まとめ入力は対象外
           IF      SPA-HKNCOMBINUM     =   "9999"
             OR   (WRK-COM-HKNCOMBI(IDX-Z) =   "9999")
               GO      TO      100112-RIGAKU-CHK-EXT
           END-IF
      *
           SET     TBL-RIH             TO  1
           SEARCH  TBL-RIHABIRI-OCC    VARYING   TBL-RIH
                   AT  END
                       MOVE    ZERO            TO  FLG-RIHA-OK
                       MOVE    SPACE           TO  WRK-RIHA-KBN
                       MOVE    SPACE           TO  WRK-RIHA-KBN2
                       MOVE    SPACE           TO  WRK-RIHA-GEN
                   WHEN    WRK-COM-INPUTCD (IDX-Z)
                                               =   TBL-RIHA-SRYCD
                                                           (TBL-RIH)
                       MOVE    TBL-RIHA-KBN (TBL-RIH)
                                               TO  WRK-RIHA-KBN
                       MOVE    TBL-RIHA-KBN2 (TBL-RIH)
                                               TO  WRK-RIHA-KBN2
                       MOVE    TBL-RIHA-GEN (TBL-RIH)
                                               TO  WRK-RIHA-GEN
                       MOVE    1               TO  FLG-RIHA-OK
           END-SEARCH
      *    リハビリテーション料の労災コード
           IF     (WRK-COM-INPUTCD   (IDX-Z)(1:3)  =   "101")
              AND (WRK-COM-CDKBN-KBN (IDX-Z)       =   "H"  )
               MOVE    2                   TO  FLG-RIHA-OK
               EVALUATE    WRK-COM-CDKBN-KBNNUM (IDX-Z)
                   WHEN    "000"
      *                心大血管リハ
                       MOVE    "1"             TO  WRK-RIHA-KBN
                   WHEN    "001"
      *                脳血管疾患等リハ
                       MOVE    "2"             TO  WRK-RIHA-KBN
                   WHEN    "002"
      *                運動器リハ
                       MOVE    "3"             TO  WRK-RIHA-KBN
                   WHEN    "003"
      *                呼吸リハ
                       MOVE    "4"             TO  WRK-RIHA-KBN
               END-EVALUATE
           END-IF
      *    H22.4 から　新規の脳血管疾患等・運動器リハコードは区分で判定
           IF     (FLG-RIHA-OK                 =    ZERO )
              AND (WRK-COM-CDKBN-KBN (IDX-Z)   =   "H"   )
              AND (WRK-COM-CDKBN-KBNNUM (IDX-Z)
                                               =   "001"
                                               OR  "002" )
              AND (WRK-COM-CDKBN-KBNNUM-EDA (IDX-Z)
                                               =   "00"    )
               MOVE    1                   TO  FLG-RIHA-OK
               EVALUATE    WRK-COM-CDKBN-KBNNUM (IDX-Z)
                   WHEN    "001"
      *                脳血管疾患等リハ
                       MOVE    "2"             TO  WRK-RIHA-KBN
                   WHEN    "002"
      *                運動器リハ
                       MOVE    "3"             TO  WRK-RIHA-KBN
               END-EVALUATE
           END-IF
      *
           IF     (FLG-RIHA-OK         =   ZERO )
               GO      TO      100112-RIGAKU-CHK-EXT
           END-IF
      *
      *    急性発生日計算処理
           PERFORM 1001121-KYUSEIYMD15-SEC
      *
           IF     (SPA-NYUGAIKBN       =   "2" ) OR
                  (LNK-ORCSC-SYORIFLG  =   1   )
      *        外来か訂正の時の日上限と月上限のチェック
               PERFORM 2002-GAIRAI-ALL-CHK-SEC
           ELSE
      *        入院の日上限と月上限のチェック
               PERFORM 2001-NYUIN-ALL-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    SPA-ERRCD           TO  WRK-ERRCD
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
      *
           .
       100112-RIGAKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     急性発生日計算処理
      *****************************************************************
       1001121-KYUSEIYMD15-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-KYUSEI
           MOVE    SPACE           TO  WRK-KYUSEI-CHK
           MOVE    SPACE           TO  WRK-KYUSEI-YMD
           MOVE    SPACE           TO  WRK-KYUSEI-MSG
      *    急性発生日計算処理
           EVALUATE    WRK-RIHA-KBN
               WHEN    "1"
                   MOVE    WRK-KYUSEI1-YMD     TO  WRK-KYUSEI
                   IF      WRK-KYUSEI-ST (1)   =   "1"
                       MOVE    SPA-SRYYMD          TO  WRK-KYUSEI
                   END-IF
                   MOVE    150                 TO  WRK-ZOGEN
               WHEN    "2"
                   MOVE    WRK-KYUSEI2-YMD     TO  WRK-KYUSEI
                   IF      WRK-KYUSEI-ST (2)   =   "1"
                       MOVE    SPA-SRYYMD          TO  WRK-KYUSEI
                   END-IF
                   MOVE    180                 TO  WRK-ZOGEN
               WHEN    "3"
                   MOVE    WRK-KYUSEI3-YMD     TO  WRK-KYUSEI
                   IF      WRK-KYUSEI-ST (3)   =   "1"
                       MOVE    SPA-SRYYMD          TO  WRK-KYUSEI
                   END-IF
                   MOVE    150                 TO  WRK-ZOGEN
               WHEN    "4"
                   MOVE    WRK-KYUSEI4-YMD     TO  WRK-KYUSEI
                   IF      WRK-KYUSEI-ST (4)   =   "1"
                       MOVE    SPA-SRYYMD          TO  WRK-KYUSEI
                   END-IF
                   MOVE    90                  TO  WRK-ZOGEN
           END-EVALUATE
      *TEST
      *    DISPLAY "WRK-KYUSEI:" WRK-KYUSEI
      *          ",IDX-Z:"  IDX-Z 
      *           ",COM;"  WRK-COM-INPUTCD (IDX-Z)
      *             ",WRK-RIHA-KBN:" WRK-RIHA-KBN
      *
           IF      WRK-KYUSEI          =   SPACE
      *        発症日なし
               IF      WRK-ZOGEN           >    ZERO
                   IF     (WRK-COM-KZMFLG3(IDX-Z)  =   SPACE)  AND
                          (LNK-ORCSC-KEI-IDX       =   ZERO )
                       MOVE    IDX-Z           TO  LNK-ORCSC-KEI-IDX
                       MOVE    "1"             TO  WRK-COM-KZMFLG3
                                                             (IDX-Z)
                       MOVE    "K839"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "2"             TO  WRK-COM-IN2 (IDX-Z)
                   END-IF
               END-IF
           ELSE
               MOVE    WRK-KYUSEI          TO  WRK-KYUSEI-CHK
      *        労災は開始日なしとする
               IF      FLG-RIHA-OK         =   2
                   MOVE    SPACE           TO  WRK-KYUSEI
                   MOVE    SPACE           TO  WRK-KYUSEI-YMD
                   MOVE    SPACE           TO  WRK-KYUSEI-MSG
               END-IF
           END-IF
      *    算定上限日編集
           IF      WRK-KYUSEI      NOT =   SPACE
      *    日後計算
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-KYUSEI          TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KOYOMI     TO  WRK-KYUSEI-YMD
      *        発生日
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-KYUSEI-YMD      TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               STRING  "算定終了日："      DELIMITED  BY  SIZE
                       LNK-DAY2-EDTYMD3    DELIMITED  BY  SIZE
                      INTO                 WRK-KYUSEI-MSG
               END-STRING
      *        要介護開始日
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-KYUSEI-YMD      TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    1                   TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    LNK-DAY6-KEISAN     TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               STRING  "算定開始日："      DELIMITED  BY  SIZE
                       LNK-DAY2-EDTYMD3    DELIMITED  BY  SIZE
                      INTO                 WRK-KYUSEI-MSG02
               END-STRING
           END-IF
      *
           .
       1001121-KYUSEIYMD15-EXT.
           EXIT.
      *
      *****************************************************************
      *     精神通院の加算チェック処理
      *****************************************************************
       100113-SEISIN-CHK-SEC            SECTION.
      *
      *    包括まとめ入力は対象外
           IF      SPA-HKNCOMBINUM     =   "9999"
               CONTINUE
           ELSE
               IF      SPA-NYUGAIKBN       =   "1"
      *            入院
                   PERFORM 1001131-NYUIN-SEISIN-SEC
               ELSE
      *            入院外
                   PERFORM 1001132-GAIRI-SEISIN-SEC
               END-IF
           END-IF
           .
       100113-SEISIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     外来　精神通院の加算チェック処理
      *****************************************************************
       1001132-GAIRI-SEISIN-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-NENREI
           MOVE    ZERO                TO  FLG-NENREI2
      *
      *    通院・在宅精神のみ
           IF     (WRK-COM-CDKBN-KBN (IDX-Z)   =   "I" )  AND
                  (WRK-COM-CDKBN-KBNNUM(IDX-Z) =   "002")
               CONTINUE
           ELSE
               GO      TO      1001132-GAIRI-SEISIN-EXT
           END-IF
      *
           EVALUATE    WRK-TUINSTR-INT
               WHEN    "1"
      *            通院・在宅精神開始日あり
                   MOVE    WRK-TUINSTR-YMD     TO  WRK-SYOYMD
               WHEN    "2"
      *            今回入力
                   MOVE    SPA-SRYYMD          TO  WRK-SYOYMD
               WHEN    OTHER
      *            初診算定日から設定
                   PERFORM 10011321-SYOSIN-SET-SEC
           END-EVALUATE
      *
           MOVE    WRK-SYOYMD          TO  WRK-SYOYMD2
           MOVE    WRK-SYOYMD          TO  WRK-SYOYMD3
      *
      *    Ｈ２０．４から１年未満とする
           COMPUTE WRK-SYO2-YY         =   WRK-SYO2-YY   +   1
           COMPUTE WRK-SYO3-YY         =   WRK-SYO3-YY   +   2
           IF      SPA-SRYYMD          >=  WRK-SYOYMD2
      *H24.4
               IF      SPA-SRYYMD          <   WRK-SYOYMD3
      *            ２年未満であれば、施設基準の判定
                   PERFORM 10011321-GAIRI-SEISINCHK-SEC
                   IF      FLG-NENREI          =   ZERO
      *                ２年未満で加算あり
                       MOVE    1                   TO  FLG-NENREI2
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-NENREI
               END-IF
           END-IF
      *    自動算定なし
           IF      SPA-SEI20KSNKBN     =   "1"
               MOVE    1               TO  FLG-NENREI
           END-IF
      *
           EVALUATE    WRK-COM-INPUTCD(IDX-Z)
               WHEN    "180020570"
      *            逓減チェック
                   COMPUTE IDX-Z3      =   IDX-Z   -  1
                   IF     (WRK-COM-INPUTCD (IDX-Z3)
                                               =   "180012210"  OR
                                                   "180012310"
                                               OR  "180020410"
      *            家族
                                               OR  "180007250"
                                               OR  "180007350"
      *            ３０分未満
                                               OR  "180031010"
                                               OR  "180031110"
                                               OR  "180031210"
                                               OR  "180031310"
      *H26.4       在宅精神療法
                                               OR  "180040210" )
      *H26.4 
      *        注加算診療コードが存在する
                     OR   (WRK-COM-AGEKSNSRYCD (IDX-Z3  1)
                                           =   WRK-COM-INPUTCD(IDX-Z))
                       IF     (SPA-SRYYMD      >=  WRK-SYOYMD2 )
      *H24.4
      *                 １年以上で２年未満で算定不可
                         IF     (SPA-SRYYMD      <   WRK-SYOYMD3 ) AND
                                (FLG-NENREI2     =   1           )
                           CONTINUE
                         ELSE
      *H24.4
      *
                           IF     (WRK-COM-KZMFLG (IDX-Z)   =   SPACE)
                           AND    (LNK-ORCSC-KEI-IDX        =   ZERO )
                               MOVE    IDX-Z       TO  LNK-ORCSC-KEI-IDX
                               MOVE    "1"         TO  WRK-COM-KZMFLG
                                                           (IDX-Z)
                               MOVE    "0215"      TO  WRK-ERRCD
      *H24.4
                               MOVE    "K873"      TO  WRK-ERRCD
      *H26.4
                               IF      WRK-TUINSTR-INT NOT =   SPACE
                                   MOVE    "K874"      TO  WRK-ERRCD
                               END-IF
                               MOVE    "1"         TO  WRK-COM-CUR
                                                           (IDX-Z)
                           END-IF
                         END-IF
                       END-IF
                   ELSE
                       MOVE    "0083"      TO  WRK-ERRCD
                       MOVE    "1"         TO  WRK-COM-CUR (IDX-Z)
                   END-IF
           END-EVALUATE
      *
           .
       1001132-GAIRI-SEISIN-EXT.
           EXIT.
      *H26.4
      *****************************************************************
      *     初診算定日・同日初診算定日設定処理
      *****************************************************************
       10011321-SYOSIN-SET-SEC            SECTION.
      *
           IF      LNK-ORCSC-SYORIFLG      =   1
      *        訂正時点の初診算定日
               MOVE    LNK-ORCSC-SYOYMD-OLD    TO  WRK-SYOYMD
           ELSE
               MOVE    LNK-ORCSC-SYOYMD        TO  WRK-SYOYMD
           END-IF
      *    同じ科で同日初診がある時、同日初診
           IF      WRK-COM-SRYKA (IDX-Z)   =   SPACE
               MOVE    SPA-SRYKA           TO  WRK-SRYKA
           ELSE
               MOVE    WRK-COM-SRYKA (IDX-Z)   TO  WRK-SRYKA
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   10  )   OR
                              (WRK-DOU-SRYKA (IDX) =   SPACE)
               IF      WRK-DOU-SRYKA (IDX) =   WRK-SRYKA
                   IF      WRK-DOU-SRYYMD(IDX) >   WRK-SYOYMD
                       MOVE    WRK-DOU-SRYYMD(IDX)
                                               TO  WRK-SYOYMD
                   END-IF
                   MOVE    10          TO  IDX
               END-IF
           END-PERFORM
      *    初診算定日から６月以内
           IF     (WRK-SYOYMD          =   SPACE  )  OR
                  (LNK-ORCSC-SYOSIN2   =   1      )
               MOVE    SPA-SRYYMD          TO  WRK-SYOYMD
           END-IF
      *
           .
       10011321-SYOSIN-SET-EXT.
           EXIT.
      *H24.4
      *****************************************************************
      *     精神通院の施設基準の判定チェック処理
      *****************************************************************
       10011321-GAIRI-SEISINCHK-SEC            SECTION.
      *
      *    施設基準情報検索
           MOVE    SPACE               TO  SYS-1006-REC
           MOVE    "1006"              TO  SYS-1006-KANRICD
           MOVE    "7"                 TO  SYS-1006-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1006-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1006-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1006-HOSPNUM
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1006-REC
           ELSE
               INITIALIZE                  SYS-1006-REC
           END-IF
      *
      *    対象施設基準(3039)（児童・思春期精神科入院管理料）
      *H25.3　まで     (3146)（〃（栄養管理経過措置））追加
      *****IF    ((SYS-1006-SSTKIJUN (039)  =   1  )  OR 
      *********   (SYS-1006-SSTKIJUN (146)  =   1  )) AND
           IF     (SYS-1006-SSTKIJUN (039)  =   1  )  AND
      *           １６歳未満
                  (LNK-ORCSC-NENREI-YY      <   16 )
               CONTINUE
           ELSE
               MOVE    1                   TO  FLG-NENREI
           END-IF
           .
       10011321-GAIRI-SEISINCHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *     入院　精神通院の加算チェック処理
      *****************************************************************
       1001131-NYUIN-SEISIN-SEC            SECTION.
      *
      *    警告チェック済みの時、チェックなし
           IF     (WRK-COM-KZMFLG (IDX-Z)  NOT =   SPACE)
               GO      TO      1001131-NYUIN-SEISIN-EXT
           END-IF
      *
           MOVE    ZERO                TO  WRK-TUKI-9
           MOVE    SPA-K02N-NYUINYMD   TO  WRK-NYUINYMD
           MOVE    ZERO                TO  FLG-SEISIN
           EVALUATE    WRK-COM-INPUTCD(IDX-Z)
      *        入院精神療法（１）
               WHEN    "180018110"
                   MOVE    3                   TO  WRK-TUKI-9
      *        入院精神療法（２）（６月以内）
               WHEN    "180012010"
                   MOVE    6                   TO  WRK-TUKI-9
      *        入院集団精神療法
               WHEN    "180016710"
                   MOVE    6                   TO  WRK-TUKI-9
      *        入院生活技能訓練療法（６月以内）
               WHEN    "180018210"
                   MOVE    6                   TO  WRK-TUKI-9
      *        家族入院精神療法（１）
               WHEN    "180028850"
                   MOVE    3                   TO  WRK-TUKI-9
                   MOVE    1                   TO  FLG-SEISIN
      *        家族入院精神療法（２）（６月以内）
               WHEN    "180028950"
                   MOVE    6                   TO  WRK-TUKI-9
                   MOVE    1                   TO  FLG-SEISIN
      *        家族入院精神療法（２）（６月超）
               WHEN    "180020950"
                   MOVE    1                   TO  FLG-SEISIN
                   PERFORM 10011310-NYUINYMD-HEN-SEC
           END-EVALUATE
           IF      WRK-TUKI-9          =   ZERO
               GO      TO      1001131-NYUIN-SEISIN-EXT
           END-IF
      *    初回入院日
           PERFORM 10011310-NYUINYMD-HEN-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      1001131-NYUIN-SEISIN-EXT
           END-IF
      *    入院日＋Ｎ月後
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-NYUINYMD        TO  LNK-DAY6-KIJUN
           MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-TUKI-9          TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KOYOMI     TO  WRK-NYUIN-YMD
      *    入力日分のチェック
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   31  )   OR
                              (WRK-ERRCD   NOT =   SPACE )
               IF      WRK-COM-NYUINDAY (IDX-Z IDX2)   >   ZERO
                   MOVE    IDX2                TO  IDD
                   MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
                   MOVE    IDD                 TO  WRK-SRYYMD (7:2)
                   IF      WRK-SRYYMD          >   WRK-NYUIN-YMD
                       MOVE    "KXXX"              TO  WRK-ERRCD
                   END-IF
               END-IF
           END-PERFORM
           IF      WRK-ERRCD       NOT =   SPACE
               IF     (WRK-COM-KZMFLG (IDX-Z)  =   SPACE)
               AND    (LNK-ORCSC-KEI-IDX       =   ZERO )
                   IF      WRK-TUKI-9          =   6
                       IF      FLG-SHOKAIKBN       =   ZERO
                           MOVE    "K840"              TO  WRK-ERRCD
                       ELSE
                           MOVE    "K842"              TO  WRK-ERRCD
                       END-IF
                   ELSE
                       IF      FLG-SHOKAIKBN       =   ZERO
                           MOVE    "K841"              TO  WRK-ERRCD
                       ELSE
                           MOVE    "K843"              TO  WRK-ERRCD
                       END-IF
                   END-IF
                   MOVE    IDX-Z               TO  LNK-ORCSC-KEI-IDX
                   MOVE    "1"                 TO  WRK-COM-KZMFLG
                                                           (IDX-Z)
                   MOVE    "1"                 TO  WRK-COM-CUR
                                                           (IDX-Z)
               ELSE
                   MOVE    SPACE               TO  WRK-ERRCD
               END-IF
           END-IF
      *
           .
       1001131-NYUIN-SEISIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初回入院日判定処理
      *****************************************************************
       10011310-NYUINYMD-HEN-SEC        SECTION.
      *
      *ver.4.7
           IF     (FLG-SEISIN          =   1   )
             AND  (SPA-STR-NYUINYMD    NOT =   SPACE )
      *        継続入院日ありの時、家族は算定不可
               IF     (WRK-COM-KZMFLG (IDX-Z)  =   SPACE)
               AND    (LNK-ORCSC-KEI-IDX       =   ZERO )
                   MOVE    "K844"              TO  WRK-ERRCD
                   MOVE    IDX-Z               TO  LNK-ORCSC-KEI-IDX
                   MOVE    "1"                 TO  WRK-COM-KZMFLG
                                                           (IDX-Z)
                   MOVE    "1"                 TO  WRK-COM-CUR
                                                           (IDX-Z)
               END-IF
           END-IF
      *    継続初回入院日
           IF     (SPA-STR-NYUINYMD    NOT =   SPACE)
           AND    (FLG-SEISIN              =   ZERO )
               MOVE    SPA-STR-NYUINYMD    TO  WRK-NYUINYMD
           END-IF
      *
           .
       10011310-NYUINYMD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災加算の算定処理
      *****************************************************************
       100113-RSIHKN-SYORI-SEC        SECTION.
      *
           MOVE    IDX-Z               TO  HZN-IDX
      *
      *    健保のリハビリ料はエラー
           MOVE    WRK-COM-INPUTCD (IDX-Z)     TO  WRK-SRYCD2
           PERFORM 10011312-RIGAKU-SERCH-SEC
           IF      FLG-RIHA-OK         =   1
               MOVE    "8046"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               GO      TO      100113-RSIHKN-SYORI-EXT
           END-IF
      *
      *    リハビリテーション総合計画評価料・
      *    リハビリテーション総合計画提供料　対象外
           IF      WRK-COM-INPUTCD (IDX-Z) =   "180024710"
                                           OR  "180042910"
               GO      TO      100113-RSIHKN-SYORI-EXT
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        労災四肢加算自動発生
               PERFORM 1001131-RSIKASAN-AUTO-SEC
           END-IF
      *    リハビリ回数・開始日チェク
           IF     (WRK-ERRCD           =   SPACE    )
            AND   (LNK-ORCSC-PG        =   "K02"    )
               MOVE    IDX-Z               TO  HZN-IDX2
               MOVE    HZN-IDX             TO  IDX-Z
               IF     (WRK-COM-DATAKBN (IDX-Z) =   "1"  )
                   PERFORM 100112-RIGAKU-CHK-SEC
               END-IF
           END-IF
           .
       100113-RSIHKN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災四肢加算自動発生処理
      *****************************************************************
       1001131-RSIKASAN-AUTO-SEC            SECTION.
      *
      *    労災の時、加算なし（リハビリ料は区分に’Ｈ’）
           IF     (WRK-COM-INPUTCD(IDX-Z)(1:3) =   "101" )
      *        四肢加算チェック
               IF      WRK-COM-INPUTCD (IDX-Z) =   "101800010" 
                   MOVE    2               TO  WRK-COM-SANFLG (IDX-Z)
               END-IF
               IF     (WRK-COM-CDKBN-KBN (IDX-Z)   NOT =   "H" )
                   OR (WRK-COM-INPUTCD (IDX-Z) =   "101800010" )
                   OR (WRK-COM-DATAKBN (IDX-Z) =   "2"  )
                   GO      TO      1001131-RSIKASAN-AUTO-EXT
               END-IF
      *H26.7
      *        合成コードは対象外
               IF     (WRK-COM-KOKUJISKBKBN1 (IDX-Z) =   3 )
                   GO      TO      1001131-RSIKASAN-AUTO-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE                TO  WRK-SRYCD
           MOVE    ZERO                 TO  FLG-ROUSAI
      * 
      *労災コードのみ対象
      *    労災用　区分番号
      *    IF      WRK-COM-CDKBN-KBN (IDX-Z)  =   "H"
      *      AND  (WRK-COM-DATAKBN (IDX-Z) =   "1"  )
      *        EVALUATE    WRK-COM-CDKBN-KBNNUM (IDX-Z)
      *            WHEN    "000"
      *            WHEN    "001"
      *            WHEN    "002"
      *            WHEN    "003"
      *疾患別リハビリのみ対象
      *********    WHEN    "004"
      *********    WHEN    "005"
      *                EVALUATE    SPA-RSI-SISIKBN
      *                    WHEN    1
      *                    WHEN    2
      *                        １．５倍
      *                        MOVE    "101800010"      TO  WRK-SRYCD
      *                END-EVALUATE
      *                MOVE    1                    TO  FLG-ROUSAI
      *        END-EVALUATE
      *****END-IF
      *
      *    労災コードのリハビリ料のみ対応
           IF     (WRK-COM-INPUTCD(IDX-Z)(1:3) =   "101" )
              AND (WRK-COM-CDKBN-KBN (IDX-Z)   =   "H"  )
              AND (WRK-COM-DATAKBN   (IDX-Z)   =   "1"  )
               EVALUATE    SPA-RSI-SISIKBN
                   WHEN    1
                   WHEN    2
      *                １．５倍
                       MOVE    "101800010"      TO  WRK-SRYCD
               END-EVALUATE
               MOVE    1                    TO  FLG-ROUSAI
      *!!!
               MOVE    8               TO  WRK-COM-SANFLG (IDX-Z)
      *!!!
           END-IF
      *
      *    既に他の労災加算が入力されている時は自動算定しない
           MOVE    ZERO                TO  FLG-ROU-ARI
           COMPUTE IDX-Z2              =   IDX-Z     +   1
           PERFORM VARYING     IDX1    FROM    IDX-Z2    BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE  )   OR
                              (FLG-ROU-ARI             =   1    ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
               IF       WRK-COM-INPUTCD (IDX1) =   "101800010"  OR
                                                   "099509901"
                   IF      WRK-COM-AUTOKBN (IDX1)  =   SPACE
                        MOVE    1                  TO  FLG-ROU-ARI
                   END-IF
               END-IF
               IF     (WRK-COM-INPUTCD (IDX1)(1:1) =  "1" ) AND
                      (WRK-COM-DATAKBN (IDX1)      =  "1" )
                  MOVE    SPA-MAX-LINE             TO  IDX1
               END-IF
           END-PERFORM
           IF      FLG-ROU-ARI         =   1
               IF      FLG-ROUSAI          =   ZERO
                   MOVE    "8004"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX1 - 1)
               END-IF
           END-IF
           IF     (WRK-SRYCD           =   SPACE )
             OR   (FLG-ROU-ARI         =   1     )
               GO      TO      1001131-RSIKASAN-AUTO-EXT
           END-IF
      *
      *    加算料チェック処理（次ぎの手技料の前に）
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-T
           COMPUTE IDX-Z2              =   IDX-Z     +   1
           PERFORM VARYING     IDX1    FROM    IDX-Z2    BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE   )   OR
                              (FLG-ARI                 =   1    ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
               IF       WRK-COM-INPUTCD (IDX1) =   WRK-SRYCD
                   MOVE    IDX1            TO  IDX-T
                   MOVE    1               TO  FLG-ARI
               ELSE
      *!四肢加算は手技料の直下とする
                   IF     (WRK-COM-INPUTCD (IDX1)(1:1) =  "1" ) AND
                          (WRK-COM-DATAKBN (IDX1)      =  "1" )
                      MOVE    SPA-MAX-LINE          TO  IDX1
                   END-IF
      *H26.7
      *            労災リハ（1.2）より下
                   IF     (WRK-COM-INPUTCD (IDX1)   =  "101800080")
                       MOVE    IDX1            TO  IDX-T
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-ARI             =   ZERO
               IF      IDX-T           =   ZERO
                   MOVE    IDX-Z           TO  IDX-S
               ELSE
                   MOVE    IDX-T           TO  IDX-S
               END-IF
           END-IF
      *
      *    加算料追加処理
           PERFORM 1002-KASAN-SET-SEC
           IF      FLG-ADD             =   1
               MOVE    2               TO  WRK-COM-SANFLG (IDX-S)
           END-IF
           .
       1001131-RSIKASAN-AUTO-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリテーション料判定処理
      *****************************************************************
       10011312-RIGAKU-SERCH-SEC            SECTION.
      *
      *
           SET     TBL-RIH             TO  1
           SEARCH  TBL-RIHABIRI-OCC    VARYING   TBL-RIH
                   AT  END
                       MOVE    ZERO            TO  FLG-RIHA-OK
                   WHEN    WRK-COM-INPUTCD (IDX-Z)
                                               =   TBL-RIHA-SRYCD
                                                           (TBL-RIH)
                       MOVE    1               TO  FLG-RIHA-OK
           END-SEARCH
      *    H22.4 から　新規の脳血管疾患等・運動器リハコードは区分で判定
           IF     (FLG-RIHA-OK             =   1       )
              OR  (WRK-COM-INPUTCD(IDX-Z)(1:3) =   "101" )
               CONTINUE
           ELSE
               IF     (WRK-COM-CDKBN-KBN (IDX-Z)   =   "H"   )
                  AND (WRK-COM-CDKBN-KBNNUM (IDX-Z)
                                                   =   "001"
                                                   OR  "002"   )
                  AND (WRK-COM-CDKBN-KBNNUM-EDA (IDX-Z)
                                                   =   "00"    )
                   MOVE    1                   TO  FLG-RIHA-OK
               END-IF
           END-IF
      *
           .
       10011312-RIGAKU-SERCH-EXT.
           EXIT.
      *
      *H24.4
      *****************************************************************
      *    年齢加算自動加算追加処理
      *****************************************************************
       10021-JIDO-AGEKASAN-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-ADD
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   4       )   OR
                              (WRK-COM-AGEKSNSRYCD(IDX-S IDX2)
                                               =   SPACE  OR
                                                   ZERO )
      *        加算追加
               MOVE    WRK-COM-AGEKSNSRYCD (IDX-S IDX2)  TO  WRK-SRYCD
      *        加算料チェック処理
               PERFORM 100111-KASAN-CHK-SEC
      *        加算料追加処理
               PERFORM 1002-KASAN-SET-SEC
               IF      FLG-ADD             =   1
                   MOVE    4                   TO  IDX2
               END-IF
           END-PERFORM
      *
           .
       10011-NENREI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動加算追加処理
      *****************************************************************
       1002-JIDO-KASAN-SEC        SECTION.
      *
           MOVE    IDX-ZS              TO  IDX-S
           MOVE    IDX-ZS              TO  IDX-Z
      *
      *    通院精神の時、初診日から１年以内チェック
           IF     (WRK-COM-CDKBN-KBN (IDX-Z)   =   "I" )  AND
                  (WRK-COM-CDKBN-KBNNUM(IDX-Z) =   "002")
               IF      (LNK-ORCSC-PG        =   "K02"
                                            OR  "KT01"  )
                   PERFORM 100113-SEISIN-CHK-SEC
               ELSE
                   MOVE    1               TO  FLG-NENREI
               END-IF
      *H26.4
      *        自動発生は入力時のみとする
               IF     (FLG-NENREI          =   ZERO   )
                 AND  (WRK-COM-KSNAUTO(IDX-Z)  =   "1" )
                   MOVE    1               TO  FLG-NENREI
               END-IF
      *
               IF      FLG-NENREI          =   ZERO
                   MOVE    IDX-Z           TO  IDX-ZS3
                   PERFORM 10021-JIDO-AGEKASAN-SEC
      *H26.4
      *            自動発生は手入力分とする
                   IF      FLG-ADD         =   1
                       MOVE    "1"         TO  WRK-COM-KSNAUTO(IDX-ZS3)
                       MOVE    SPACE       TO  WRK-COM-AUTOKBN (IDX-S)
                   END-IF
               END-IF
           ELSE
               PERFORM 10021-JIDO-AGEKASAN-SEC
               IF      FLG-ADD             =   1
                   MOVE    1               TO  WRK-COM-SANFLG (IDX-S)
               END-IF
           END-IF
      *
           COMPUTE IDX-Z2              =   IDX-ZS2     +   1
           PERFORM VARYING     IDX1    FROM    IDX-Z2  BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE     )   OR
                             ((WRK-GMN-INPUTCD (IDX1)  =   SPACE ) AND
                              (WRK-COM-INPUTCD (IDX1)  =   SPACE ))
      *        次の手技料
               IF     (WRK-COM-INPUTCD (IDX1)(1:1) =   "1" ) AND
                      (WRK-COM-DATAKBN (IDX1)      =   "1" )
                   MOVE    IDX1            TO  IDX-Z
                   MOVE    SPA-MAX-LINE    TO  IDX1
               END-IF
           END-PERFORM
           .
       1002-JIDO-KASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動加算　存在チェック加処理
      *****************************************************************
       100111-KASAN-CHK-SEC        SECTION.
      *
      *    時間自動加算　存在チェック
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-T
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE   )   OR
                              (FLG-ARI                 =   1    ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
               IF       WRK-COM-INPUTCD (IDX1) =   WRK-SRYCD
                    MOVE    IDX1           TO  IDX-T
                    MOVE    1              TO  FLG-ARI
               END-IF
           END-PERFORM
      *
           IF      FLG-ARI             =   ZERO
               MOVE    ZERO                TO  IDX-T
               PERFORM VARYING     IDX1    FROM    IDX-S   BY  1
                       UNTIL      (IDX1        >   SPA-MAX-LINE     ) OR
                                  (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
      *            次の手技の前とする
                   IF      (WRK-COM-INPUTCD (IDX1)(1:1) =   "1" )  AND
                           (WRK-COM-DATAKBN (IDX1)      =   "2" )
      *           通則外加算の前
                     AND  ((WRK-COM-TUSOKUGAIKBN(IDX1)  NOT =  "1" )  OR
      *           施設基準逓減の後
                           (WRK-COM-TENSIKIBETU(IDX1)   =   6   ))
                       MOVE    IDX1           TO  IDX-T
                   END-IF
               END-PERFORM
               IF      IDX-T           NOT =   ZERO
                   MOVE    IDX-T               TO  IDX-S
               END-IF
           END-IF
           .
       100111-KASAN-CHK-EXT.
           EXIT.
      *H24.4
      *
      *****************************************************************
      *    加算料追加処理
      *****************************************************************
       1002-KASAN-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-ADD
      *
           IF      FLG-ARI             =   ZERO
               ADD     1               TO  IDX-S
           ELSE
               MOVE    IDX-T           TO  IDX-S
           END-IF
      *
      *    点数マスタ編集・基本チェック
           MOVE    WRK-SCR-REC         TO  HZN-SCR-REC
           INITIALIZE                  ORCSC92AREA
           IF      FLG-ARI             =   ZERO
               MOVE    "1"                 TO  LNK-SC92-KBN
           ELSE
               MOVE    SPACE               TO  WRK-COM-CHKFLG (IDX-S)
           END-IF
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI   TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    エラーの時、追加なし
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    HZN-SCR-REC         TO  WRK-SCR-REC
               IF      FLG-ARI             =   ZERO
                   COMPUTE IDX-S           =   IDX-S
                                           -   1
               END-IF
               GO      TO      1002-KASAN-SET-EXT
           END-IF
      *
           MOVE    WRK-COM-SRYKBN (IDX-S - 1)
                                       TO  WRK-COM-SRYKBN(IDX-S)
           MOVE    SPACE               TO  WRK-GMN-SRYKBN(IDX-S)
           MOVE    WRK-COM-SRYSYUKBN (IDX-S - 1)
                                       TO  WRK-COM-SRYSYUKBN(IDX-S)
      *
      *    自動追加分（入力時に追加）
           MOVE    "2"                 TO  WRK-COM-AUTOKBN (IDX-S)
      *
           ADD     1                   TO  WRK-GMN-MAX
      *
           MOVE    1                   TO  FLG-ADD
           ADD     1                   TO  IDX-Z
           .
       1002-KASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算編集処理
      *****************************************************************
       1002-TENSU-KEI-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  ORCSC90AREA
           MOVE    1                   TO  IDX-STR
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE )   OR
                              (SPA-ERRCD       NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               MOVE    SPACE               TO  WRK-GMN-SURYO (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-TENSU (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-KAISU (IDX-Z)
               MOVE    ZERO                TO  WRK-GMN-KEI   (IDX-Z)
      *
      *        手技毎
               INITIALIZE                   WRK-COM-PLUS-AREA(IDX-Z)
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."  OR  "S"
                                                               OR "*"
                   MOVE    WRK-GMN-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
               ELSE
                   MOVE    WRK-COM-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
                   IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059"
                       MOVE    "7"                     TO  WRK-MOJI
                   END-IF
               END-IF
      *
               EVALUATE    WRK-MOJI
                   WHEN    "0"
      *                自賠責
                       PERFORM 10021-JIHI-KEI-SEC
                   WHEN    "8"
                   WHEN    "."
                   WHEN    "S"
                   WHEN    "*"
                       CONTINUE
      *
                   WHEN    OTHER
      *                編集チェックへ
                       PERFORM 10021-KEISAN-SEC
               END-EVALUATE
           END-PERFORM
      *
      *    最終行
      *    割合計算加算
           IF      IDX-SYU             >   ZERO
               COMPUTE IDX-Z           =   IDX-MAX    +  1
               PERFORM 100212-TENSUKEI-SEC
           END-IF
      *    合計点数計算・編集
           MOVE    "2"                 TO  LNK-SC90-KBN
           MOVE    IDX-SYU             TO  LNK-SC90-IDX
           MOVE    IDX-STR             TO  LNK-SC90-IDX-STR
           MOVE    IDX-MAX             TO  LNK-SC90-IDX-END
           CALL    "ORCSC90TEN2"       USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *    計
           COMPUTE WRK-COM-KEI (IDX-MAX)   =   WRK-COM-TENSU  (IDX-MAX)
                                           *   LNK-ORCSC-KAISU
      *    セット用合計点数
           MOVE    WRK-COM-KEI  (IDX-MAX)  TO  LNK-ORCSC-ZAIKEI
           MOVE    WRK-COM-TENSU(IDX-MAX)  TO  LNK-ORCSC-ZAITEN
      *
           .
       1002-TENSU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責計算処理
      *****************************************************************
       10021-JIHI-KEI-SEC                  SECTION.
      *
           IF      WRK-COM-INPUTCD(IDX-Z)(1:5) =   "09591"  OR
                                                   "09592"  OR
                                                   "09593"  OR
                                                   "09594"
               CONTINUE
           ELSE
               GO      TO      10021-JIHI-KEI-EXT
           END-IF
      *    金額入力対応
           IF     (WRK-COM-SRYSYUKBN(IDX-Z)    =   "809")
            AND   (WRK-COM-TEN      (IDX-Z)    =   ZERO )
            AND   (WRK-COM-JIHIMONEY(IDX-Z)    >   ZERO )
               COMPUTE WRK-COM-KISOTEN(IDX-Z)  =   WRK-COM-JIHIMONEY
                                                            (IDX-Z)
                                               /   10
               ADD     WRK-COM-KISOTEN (IDX-Z) TO  WRK-SUM-TENGAKEI
               MOVE    WRK-SUM-TENGAKEI        TO  LNK-SC90-TENSU
      *        回数
               MOVE    LNK-ORCSC-KAISU         TO  WRK-COM-KAISU (IDX-Z)
               MOVE    "1"                     TO  WRK-COM-KEIFLG(IDX-Z)
               GO      TO      10021-JIHI-KEI-EXT
           END-IF
      *
      *    自賠責明細料・診断料
           COMPUTE WRK-COM-KISOTEN(IDX-Z)  =   WRK-COM-TEN (IDX-Z)
                                           /   10
      *
           ADD     WRK-COM-KISOTEN (IDX-Z) TO  WRK-SUM-TENGAKEI
           MOVE    WRK-SUM-TENGAKEI        TO  LNK-SC90-TENSU
      *    回数
           MOVE    LNK-ORCSC-KAISU         TO  WRK-COM-KAISU (IDX-Z)
      *
           MOVE    SPACE                   TO  WRK-COM-KEIFLG (IDX-Z)
      *
           .
       10021-JIHI-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算処理
      *****************************************************************
       10021-KEISAN-SEC                  SECTION.
      *
      *    手技料判定
           IF      (WRK-MOJI                   =   "1"  )  AND
                   (WRK-COM-DATAKBN (IDX-Z)    =   "1"  )
               IF      IDX-SYU             >   ZERO
      *            手技料の計算
                   MOVE    1               TO  FLG-SYUGI
                   PERFORM 100212-TENSUKEI-SEC
      *            点数計算
                   PERFORM 1007-TENSU-SUM-SEC
               END-IF
               MOVE    IDX-Z               TO  IDX-SYU
           END-IF
      *
      *!!!!
      *    労災リハビリ四肢加算再計算
           IF      (WRK-COM-INPUTCD (IDX-Z)(1:3)   =   "101" )
               AND (WRK-COM-SANFLG (IDX-Z)     =   8   )
               PERFORM 10061-RIHA-KASANKEI-SEC
           END-IF
      *!!!!
      *
      *    手技点数計算
           EVALUATE    WRK-MOJI
               WHEN    "1"
                   PERFORM 100211-SYUGITEN-KEI-SEC
           END-EVALUATE
      *    回数
           MOVE    LNK-ORCSC-KAISU     TO  WRK-COM-KAISU (IDX-Z)
      *
           .
       10021-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技点数計算処理
      *****************************************************************
       100211-SYUGITEN-KEI-SEC              SECTION.
      *
           IF      WRK-COM-TENSIKIBETU (IDX-Z)     =   5
                                                   OR  6
               CONTINUE
           ELSE
      *        通則加算対象外区分判定
               IF      WRK-COM-TUSOKUGAIKBN(IDX-Z) =   "1"
                   ADD     WRK-COM-KISOTEN (IDX-Z) TO  WRK-SUM-TENGAKEI
               ELSE
                   ADD     WRK-COM-KISOTEN(IDX-Z)  TO  WRK-SUM-TENSUKEI
               END-IF
           END-IF
           .
       100211-SYUGITEN-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技料の計算処理
      *****************************************************************
       100212-TENSUKEI-SEC             SECTION.
      *
           MOVE    IDX-Z               TO  HZN-IDX
           PERFORM 1006-KASAN-SET-SEC
      *    手技料の合計を編集する
           MOVE    LNK-SC90-TENSU      TO  WRK-COM-SYUTEN2 (IDX-SYU)
      *
      *    労災の外来管理加算読み替え処理
           IF      SPA-HKNKBN          =   1
      *****  AND  (SPA-RSI-HKNKBN  NOT =   "5" )
             AND  (SPA-RSI-JUNKYO  NOT =   "1" )
      *!       回数を掛けた点数で判定する
      *!         （２単位以上の時、読み替えはないはず）
      *!       COMPUTE WRK-SYOKEI      =   WRK-COM-SYUTEN2 (IDX-SYU)
      *!                               *   LNK-ORCSC-KAISU
               MOVE    WRK-COM-SYUTEN2 (IDX-SYU)
                                       TO  WRK-SYOKEI
               IF     (WRK-COM-RSIGIRI (IDX-SYU)
                                           =   1  )  AND
                      (WRK-SYOKEI          <   LNK-ORCSC-GAIRAI-TEN)
                   MOVE     LNK-ORCSC-GAIRAI-TEN
                                           TO  LNK-SC90-TENSU
               END-IF
           END-IF
           COMPUTE LNK-SC90-TENSU      =   LNK-SC90-TENSU
                                       +   WRK-SUM-TENGAKEI
      *
           .
       100212-TENSUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    割合加算計算処理
      *****************************************************************
       1006-KASAN-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-TENSUKEI-G
           MOVE    ZERO                TO  WRK-TENSUKEI-K
      *
           PERFORM VARYING     IDX     FROM    IDX-SYU  BY  1
                   UNTIL       IDX         >=  HZN-IDX
      *      手技料の点数を所定点数とする
      *      ％加算
             IF       (WRK-COM-INPUTCD(IDX)(1:3)   NOT =   "101") AND
                      (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   ZERO)
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-COM-KISOTEN
                                                             (IDX-SYU)
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-K
               END-IF
      *        ％減算（労災減算も計算する）
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   6   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   ZERO)
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-COM-KISOTEN
                                                             (IDX-SYU)
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
               END-IF
           END-PERFORM
      *
           COMPUTE WRK-SUM-TENSUKEI    =   WRK-SUM-TENSUKEI
                                       +   WRK-TENSUKEI-K
                                       -   WRK-TENSUKEI-G
      *
      *    放射線の乳幼児加算計算
           MOVE    ZERO                TO  WRK-TENSUKEI-K
           PERFORM VARYING     IDX     FROM    IDX-SYU  BY  1
                   UNTIL       IDX         >=  HZN-IDX
      *      ％加算
             IF       (WRK-COM-INPUTCD(IDX)(1:3)   NOT =   "101") AND
                      (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   1   )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-SUM-TENSUKEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-K
               END-IF
           END-PERFORM
      *
           COMPUTE LNK-SC90-TENSU      =   WRK-SUM-TENSUKEI
                                       +   WRK-TENSUKEI-K
      *
      *    労災の時、労災加算
           IF      SPA-HKNKBN          =   1
      *****  AND  (SPA-RSI-HKNKBN  NOT =   "5" )
             AND  (SPA-RSI-JUNKYO  NOT =   "1" )
               PERFORM 10061-RSIKASAN-SET-SEC
           END-IF
      *
           .
       1006-KASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災割合加算計算処理
      *****************************************************************
       10061-RSIKASAN-SET-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-TENSUKEI-RSI
           MOVE    ZERO                TO  IDX-RSI
      *    労災加算対象点数集計
           PERFORM VARYING     IDX     FROM    IDX-SYU  BY  1
                   UNTIL       IDX         >=  HZN-IDX
      *
      *        手技料の点数を所定点数とする
      *        労災１．５ ％加算
      *        IF     (WRK-COM-INPUTCD(IDX)(1:3)   =   "101")  AND
      *               (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
      *               (WRK-COM-SANFLG     (IDX)    =   2   )
      *??          MOVE    ZERO                TO  WRK-TENSUKEI
      ********     COMPUTE WRK-TENSUKEI        =   LNK-SC90-TENSU
      *            COMPUTE WRK-TENSUKEI        =  (WRK-COM-KISOTEN
      *                                                      (IDX-SYU)
      *                                        -   WRK-TENSUKEI-G )
      *                                        *   WRK-COM-KISOTEN(IDX)
      *                                        /   100
      *                                        +   0.5
      *            MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
      ***          ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-RSI
      ***      END-IF
      *        労災リハ（１．２）加算
               IF     (WRK-COM-INPUTCD(IDX)(1:3)   =   "101")  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
      *??             (WRK-COM-SANFLG     (IDX)    =   0   )
                      (WRK-COM-INPUTCD (IDX)       =   "101800080")
      *
      *            基礎点数の１.2倍
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =  (WRK-COM-TEN
                                                             (IDX-SYU))
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-RSI
               END-IF
      *        その他の労災加算
               IF     (WRK-COM-INPUTCD(IDX)(1:3)   =   "101")  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   0   )
                 AND  (WRK-COM-INPUTCD (IDX)   NOT =   "101800080")
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =  (WRK-COM-KISOTEN
                                                             (IDX-SYU)
                                               -   WRK-TENSUKEI-G )
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-RSI
               END-IF
           END-PERFORM
      *
           COMPUTE LNK-SC90-TENSU      =   LNK-SC90-TENSU
                                       +   WRK-TENSUKEI-RSI
      *
           .
       10061-RSIKASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災１．５ ％加算計算処理
      *****************************************************************
       10061-RIHA-KASANKEI-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-RIHAEND
           INITIALIZE                  WRK-RIHA-KIZAMI-G
      *
      *    労災加算対象点数集計
           PERFORM VARYING     IDX     FROM    IDX-SYU  BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE )
                         OR   (WRK-GMN-INPUTCD (IDX)
                                           =   SPACE         )
                         OR   (FLG-RIHAEND =   1        )
      *
      *        労災１．５ ％加算
               IF     (WRK-COM-INPUTCD(IDX)(1:3)   =   "101")
                 AND  (WRK-COM-TENSIKIBETU(IDX)    =   5   )
                 AND  (WRK-COM-SANFLG     (IDX)    =   2   )
      *        リハビリの労災加算は単位点数毎とする（他の加算なし）
      *            点数の％加算
                   COMPUTE WRK-TENSUKEI        =   WRK-COM-TEN
                                                             (IDX-SYU)
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   MOVE    WRK-TENSUKEI-9      TO  WRK-KSNTEN
      *
      *            きざみ点数の％加算
                   COMPUTE WRK-TENSUKEI        =   WRK-COM-TEN
                                                             (IDX-SYU)
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   MOVE    WRK-TENSUKEI-9      TO  WRK-KZMTEN
      *
                   MOVE    1               TO  FLG-RIHAEND
               END-IF
      *
               IF      (WRK-COM-INPUTCD(IDX)     =   "1"  )  AND
                       (WRK-COM-DATAKBN (IDX)    =   "1"  )
                   MOVE    1           TO  FLG-RIHAEND
               END-IF
           END-PERFORM
      *
      *    きざみ値再計算
           MOVE    WRK-COM-SURYO (IDX-SYU) TO  WRK-SURYO
      *    点数の％加算
           COMPUTE WRK-SYU-TEN         =   WRK-COM-TEN (IDX-SYU)
                                       +   WRK-KSNTEN
      *    点数編集
           MOVE    WRK-SYU-TEN         TO  WRK-COM-KISOTEN  (IDX-SYU)
      *
      *    きざみ値の％加算
           COMPUTE WRK-SYU-KZMTEN      =   WRK-COM-KZMTEN (IDX-SYU)
                                       +   WRK-KZMTEN
      *
           MOVE    ZERO                    TO  WRK-KEI-TEN2
      *    きざみ値判定
           EVALUATE    TRUE
      *            データ≦（下限値−きざみ値）
               WHEN    WRK-SURYO       <=  (WRK-COM-KZMKGN(IDX-SYU)
                                        -   WRK-COM-KZM   (IDX-SYU) )
                   IF      WRK-COM-KZMERR (IDX-SYU)  =   2  OR  3
                       MOVE    "0021"          TO  SPA-ERRCD
                   END-IF
      *
      *            下限値＜データ≦上限値
               WHEN     (WRK-COM-KZMKGN (IDX-SYU)  <   WRK-SURYO )   AND
                        (WRK-COM-KZMJGN (IDX-SYU)  >=  WRK-SURYO )
      *            点数計算式１
                   COMPUTE WRK-KEI-TEN1    =  (WRK-SURYO
                                           -   WRK-COM-KZMKGN(IDX-SYU))
                                           /   WRK-COM-KZM  (IDX-SYU)
                                           +   0.999
                   MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
                   COMPUTE WRK-KEI-TEN     =   WRK-SYU-TEN
                                           + ( WRK-KEI-TEN2
                                           *   WRK-SYU-KZMTEN  )
                   COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN    +  0.5
      *
      *            上限値＜データ
               WHEN    WRK-COM-KZMJGN (IDX-SYU)  <   WRK-SURYO
                   IF      WRK-COM-KZMERR (IDX-SYU)  =   0  OR  2
      *            警告エラーとする
      *???             MOVE    "K030"          TO  SPA-ERRCD
      *            点数計算式１
                       COMPUTE WRK-KEI-TEN1    =  (WRK-SURYO
                                               -   WRK-COM-KZMKGN
                                                             (IDX-SYU))
                                               /   WRK-COM-KZM(IDX-SYU)
                                               +   0.999
                       MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
                       COMPUTE WRK-KEI-TEN     =   WRK-SYU-TEN
                                               + ( WRK-KEI-TEN2
                                               *   WRK-SYU-KZMTEN)
                       COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN    +  0.5
                   ELSE
      *            点数計算式２
                       COMPUTE WRK-KEI-TEN1    =  (WRK-COM-KZMJGN
                                                             (IDX-SYU)
                                               -   WRK-COM-KZMKGN
                                                              (IDX-SYU))
                                               /   WRK-COM-KZM(IDX-SYU)
                                               +   0.999
                       MOVE    WRK-KEI-TEN1    TO  WRK-KEI-TEN2
                       COMPUTE WRK-KEI-TEN     =   WRK-SYU-TEN
                                               + ( WRK-KEI-TEN2
                                               *   WRK-SYU-KZMTEN)
                       COMPUTE WRK-KEI-TEN2    =   WRK-KEI-TEN  +  0.5
                   END-IF
           END-EVALUATE
      *
           IF     (SPA-ERRCD           =   SPACE ) AND
                  (WRK-KEI-TEN2        >   ZERO  )
               MOVE    WRK-KEI-TEN2    TO  WRK-COM-KISOTEN  (IDX-SYU)
           END-IF
      *
           .
       10061-RIHA-KASANKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技毎の計算処理
      *****************************************************************
       1007-TENSU-SUM-SEC            SECTION.
      *
           MOVE    "1"                 TO  LNK-SC90-KBN
           MOVE    IDX-SYU             TO  LNK-SC90-IDX
           MOVE    IDX-STR             TO  LNK-SC90-IDX-STR
           COMPUTE LNK-SC90-IDX-END    =   IDX-Z   -   1
           CALL    "ORCSC90TEN2"       USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *
           MOVE    IDX-Z               TO  IDX-STR
           INITIALIZE                  WRK-TENSU-KEISAN
           MOVE    ZERO                TO  LNK-SC90-TENSU
           .
       1007-TENSU-SUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来の日上限と月上限のチェックと自動逓減処理
      *****************************************************************
       2002-GAIRAI-ALL-CHK-SEC                SECTION.
      *
           MOVE    SPA-SRYYMD(7:2)    TO  IDD
           MOVE    SPA-SRYYMD         TO  WRK-SRYYMD
      *
           MOVE    ZERO               TO  WRK-DAYCNT
      *
      *    当日上限チェック
           COMPUTE WRK-KAISU           =   WRK-COM-SURYO(IDX-Z)
                                       *   WRK-COM-KAISU(IDX-Z)
      *
           PERFORM 20021-DAYMAX-CHK-SEC
      *
           .
       2002-GAIRAI-ALL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    当日上限チェック処理
      *****************************************************************
       20021-DAYMAX-CHK-SEC         SECTION.
      *
           EVALUATE    WRK-RIHA-KBN
               WHEN    "1"
                   MOVE    1               TO  IDE3
               WHEN    "2"
                   MOVE    2               TO  IDE3
               WHEN    "3"
                   MOVE    3               TO  IDE3
               WHEN    "4"
                   MOVE    4               TO  IDE3
               WHEN    OTHER
                   MOVE    ZERO            TO  IDE3
           END-EVALUATE
      *
      *    急性発生日以降の月上限
           MOVE    ZERO                TO  WRK-TUKCNT
           MOVE    ZERO                TO  WRK-ACCT-CNT
           IF     (WRK-KYUSEI-YMD  NOT =   SPACE )  AND
                  (WRK-KYUSEI-YMD      <   WRK-SRYYMD)
             AND  (IDE3                >   ZERO  )
               IF      WRK-KYUSEI-YMD(1:6)    =   WRK-SRYYMD(1:6)
                   MOVE    WRK-KYUSEI-YMD(7:2)    TO  IDE
                   ADD     1                      TO  IDE
               ELSE
                   MOVE    1                      TO  IDE
               END-IF
               MOVE    ZERO                TO  WRK-TUKI-CNT
               MOVE    ZERO                TO  WRK-TUKI-CNT2
               PERFORM VARYING     IDE2    FROM    IDE   BY  1
                       UNTIL       IDE2    >   31
                   ADD     WRK-RIHA-DAY (IDE2) TO  WRK-TUKI-CNT
                   IF      IDE2            <=  IDD
      *            疾患別今回入力分
                       ADD     WRK-RIHA-CNT2 (IDE3 IDE2)
                                           TO  WRK-TUKI-CNT2
                   END-IF
               END-PERFORM
      *
               IF      WRK-TUKI-CNT    >   ZERO
      *            診療会計から疾患別算定集計
                   PERFORM 20021-SRYACCT-TUKI-SEC
               END-IF
      *TEST
      *    DISPLAY "WRK-ACCT-CNT:" WRK-ACCT-CNT
      *          ",WRK-TUKI-CNT2:"  WRK-TUKI-CNT2
      *          ",WRK-KAISU:"  WRK-KAISU
      *          ",WRK-TEI-CNT:"  WRK-TEI-CNT
      *TEST
      *T
               COMPUTE WRK-TUKCNT          =   WRK-ACCT-CNT
                                           +   WRK-TUKI-CNT2
                                           +   WRK-KAISU
               IF      WRK-TUKCNT          >   WRK-TEI-CNT
                   COMPUTE  WRK-TUKCNT     =   WRK-TUKCNT
                                           -   WRK-TEI-CNT
               ELSE
                   MOVE    ZERO            TO  WRK-TUKCNT
               END-IF
           END-IF
      *H24.4
      *    要介護のリハビリ
           MOVE    ZERO                TO  FLG-KAIGO
           IF      WRK-COM-INPUTCD (IDX-Z) =   "180033910"
      *        脳血管疾患
                                           OR  "180034010"
                                           OR  "180034110"
                                           OR  "180034210"
                                           OR  "180034310"
                                           OR  "180034410"
      *        運動器
                                           OR  "180034510"
                                           OR  "180034610"
                                           OR  "180034710"
      *        基準不適合
                                           OR  "180043430"
                                           OR  "180043530"
                                           OR  "180043630"
                                           OR  "180043730"
                                           OR  "180043830"
                                           OR  "180043930"
                                           OR  "180044030"
                                           OR  "180044130"
                                           OR  "180044230"
               MOVE    1                   TO  FLG-KAIGO
           END-IF
      *
      *    急性発生日以降
           IF     (WRK-KYUSEI-YMD  NOT =   SPACE )  AND
                  (WRK-KYUSEI-YMD      <   WRK-SRYYMD)
      *        上限日数越えで 月１３単位以上
               IF     (WRK-COM-KZMFLG (IDX-Z)   =   SPACE)  AND
                      (LNK-ORCSC-KEI-IDX        =   ZERO )
      ***          MOVE    IDX-Z           TO  LNK-ORCSC-KEI-IDX
      ***          MOVE    "1"             TO  WRK-COM-KZMFLG (IDX-Z)
                   EVALUATE    WRK-RIHA-KBN
                       WHEN    "1"
                           MOVE    "K831"          TO  SPA-ERRCD
                           IF      WRK-TUKCNT          >   13
                               MOVE    "K835"          TO  SPA-ERRCD
                           END-IF
                       WHEN    "2"
      *                    脳血管疾患
                           IF      FLG-KAIGO       =   ZERO
                               MOVE    "K832"          TO  SPA-ERRCD
                           END-IF
                           IF      WRK-TUKCNT          >   13
                               MOVE    "K836"          TO  SPA-ERRCD
                           END-IF
                       WHEN    "3"
      *                    運動器
                           IF      FLG-KAIGO       =   ZERO
                               MOVE    "K833"          TO  SPA-ERRCD
                           END-IF
                           IF      WRK-TUKCNT          >   13
                               MOVE    "K837"          TO  SPA-ERRCD
                           END-IF
                       WHEN    "4"
                           MOVE    "K834"          TO  SPA-ERRCD
                           IF      WRK-TUKCNT          >   13
                               MOVE    "K838"          TO  SPA-ERRCD
                           END-IF
                   END-EVALUATE
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    IDX-Z           TO  LNK-ORCSC-KEI-IDX
                       MOVE    "1"             TO  WRK-COM-KZMFLG(IDX-Z)
                       MOVE    WRK-KYUSEI-MSG  TO  LNK-ORCSC-ERRMSG
                       MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
                   END-IF
               END-IF
           END-IF
      *H24.4
      *    脳血管・運動器　上限日数以下のコードチェック
           IF     (WRK-KYUSEI-YMD  NOT =   SPACE )  AND
                  (WRK-KYUSEI-YMD      >=   WRK-SRYYMD)
      *        上限日数越えで 月１３単位以上
               IF     (WRK-COM-KZMFLG (IDX-Z)   =   SPACE)  AND
                      (LNK-ORCSC-KEI-IDX        =   ZERO )
                   EVALUATE    WRK-RIHA-KBN
                       WHEN    "2"
      *                    脳血管疾患
                           IF      WRK-COM-INPUTCD (IDX-Z)
                                                   =   "180033910"
                                                   OR  "180034010"
                                                   OR  "180034110"
                                                   OR  "180034210"
                                                   OR  "180034310"
                                                   OR  "180034410"
      *        基準不適合
                                                   OR  "180043430"
                                                   OR  "180043530"
                                                   OR  "180043630"
                                                   OR  "180043730"
                                                   OR  "180043830"
                                                   OR  "180043930"
                               MOVE    "K871"          TO  SPA-ERRCD
                           END-IF
                       WHEN    "3"
      *                    運動器
                           IF      WRK-COM-INPUTCD (IDX-Z)
                                                   =   "180034510"
                                                   OR  "180034610"
                                                   OR  "180034710"
      *                            不適合
                                                   OR  "180044030"
                                                   OR  "180044130"
                                                   OR  "180044230"
                               MOVE    "K872"          TO  SPA-ERRCD
                           END-IF
                   END-EVALUATE
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    IDX-Z           TO  LNK-ORCSC-KEI-IDX
                       MOVE    "1"             TO  WRK-COM-KZMFLG(IDX-Z)
                       MOVE    WRK-KYUSEI-MSG02
                                               TO  LNK-ORCSC-ERRMSG
                       MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
                   END-IF
               END-IF
           END-IF
      *
      *    COMPUTE WRK-DAYCNT          =   WRK-RIHA-CNT (IDD)
      *                                +   WRK-KAISU
           MOVE    ZERO                TO  WRK-DAYCNT
           COMPUTE WRK-DAYCNT          =   WRK-RIHA-CNT2(1 IDD)
                                       +   WRK-RIHA-CNT2(2 IDD)
                                       +   WRK-RIHA-CNT2(3 IDD)
                                       +   WRK-RIHA-CNT2(4 IDD)
                                       +   WRK-KAISU
      *TEST
      *    DISPLAY "WRK-RIHA-CNT1:" WRK-RIHA-CNT2(1 IDD)
      *            ",2:"  WRK-RIHA-CNT2(2 IDD)
      *            ",3:"  WRK-RIHA-CNT2(3 IDD)
      *            ",4:"  WRK-RIHA-CNT2(4 IDD)
      *         ",WRK-KAISU :"  WRK-KAISU
      *       ",WRK-DAYCNT:"  WRK-DAYCNT
      *TEST
      *
      *    １日９単位でエラー
           IF      SPA-ERRCD           =   SPACE
               IF     (WRK-RIHA-DAY (IDD) +   WRK-DAYCNT)
                                           >    9
                   IF      SPA-NYUGAIKBN   =   "1"
                       MOVE    SPACE           TO  LNK-ORCSC-ERRMSG
                       STRING  IDD             DELIMITED  BY  SIZE
                               "日の"
                                               DELIMITED  BY  SIZE
                                           INTO    LNK-ORCSC-ERRMSG
                       END-STRING
                   END-IF
                   IF      SPA-HKNKBN      =   1
      *******        AND  (SPA-RSI-HKNKBN  NOT =   "5" )
                     AND  (SPA-RSI-JUNKYO  NOT =   "1" )
                       IF     (WRK-COM-KZMFLG (IDX-Z)   =   SPACE)  AND
                              (LNK-ORCSC-KEI-IDX        =   ZERO )
                           MOVE    IDX-Z       TO  LNK-ORCSC-KEI-IDX
                           MOVE    "1"         TO  WRK-COM-KZMFLG(IDX-Z)
                           MOVE    "8027"      TO  SPA-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR (IDX-Z)
                       END-IF
                   ELSE
                       MOVE    "8027"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
                   END-IF
               END-IF
           END-IF
      *    １日６単位で警告
           IF      SPA-ERRCD           =   SPACE
               IF     (WRK-RIHA-DAY (IDD) +   WRK-DAYCNT)
                                          >    6
                   IF     (WRK-COM-KZMFLG (IDX-Z)   =   SPACE)  AND
                          (LNK-ORCSC-KEI-IDX        =   ZERO )
                       MOVE    IDX-Z       TO  LNK-ORCSC-KEI-IDX
                       MOVE    "1"         TO  WRK-COM-KZMFLG (IDX-Z)
                       MOVE    "K827"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
                   END-IF
               END-IF
           END-IF
      *    今回分集計
           IF     (SPA-ERRCD           =   SPACE )
             OR   (SPA-ERRCD (1:1)     =   "K"   )
               PERFORM 20023-KAISU-HEN-SEC
           END-IF
      *
           .
       20021-DAYMAX-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    今回分集計処理
      *****************************************************************
       20023-KAISU-HEN-SEC         SECTION.
      *
      **** ADD     WRK-KAISU       TO  WRK-RIHA-CNT (IDD)
      *
           ADD     WRK-KAISU       TO  WRK-RIHA-CNT2 (IDE3 IDD)
      *
           .
       20023-KAISU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    疾患別月上限集計処理
      *****************************************************************
       20021-SRYACCT-TUKI-SEC         SECTION.
      *
           INITIALIZE                  WRK-TUKI-SRYCD
           INITIALIZE                  TBL-RIHA-ZAINUM-G
           MOVE    ZERO                TO  WRK-TEI-CNT
      *
           EVALUATE    WRK-RIHA-KBN
               WHEN    "1"
      *            心大血管疾患
                   MOVE    "180027410"         TO  WRK-SRYCDT (1)
                   MOVE    "180027510"         TO  WRK-SRYCDT (2)
               WHEN    "2"
      *            脳血管疾患等
                   MOVE    "180027610"         TO  WRK-SRYCDT (1)
                   MOVE    "180027710"         TO  WRK-SRYCDT (2)
                   MOVE    "180030810"         TO  WRK-SRYCDT (3)
                   MOVE    "180032410"         TO  WRK-SRYCDT (4)
                   MOVE    "180032510"         TO  WRK-SRYCDT (5)
                   MOVE    "180032610"         TO  WRK-SRYCDT (6)
                   MOVE    "180033910"         TO  WRK-SRYCDT (7)
                   MOVE    "180034010"         TO  WRK-SRYCDT (8)
                   MOVE    "180034110"         TO  WRK-SRYCDT (9)
                   MOVE    "180034210"         TO  WRK-SRYCDT (10)
                   MOVE    "180034310"         TO  WRK-SRYCDT (11)
                   MOVE    "180034410"         TO  WRK-SRYCDT (12)
      *            不適合
                   MOVE    "180043430"         TO  WRK-SRYCDT (13)
                   MOVE    "180043530"         TO  WRK-SRYCDT (14)
                   MOVE    "180043630"         TO  WRK-SRYCDT (15)
                   MOVE    "180043730"         TO  WRK-SRYCDT (16)
                   MOVE    "180043830"         TO  WRK-SRYCDT (17)
                   MOVE    "180043930"         TO  WRK-SRYCDT (18)
               WHEN    "3"
      *            運動器
                   MOVE    "180027810"         TO  WRK-SRYCDT (1)
                   MOVE    "180027910"         TO  WRK-SRYCDT (2)
                   MOVE    "180032710"         TO  WRK-SRYCDT (3)
                   MOVE    "180034510"         TO  WRK-SRYCDT (4)
                   MOVE    "180034610"         TO  WRK-SRYCDT (5)
                   MOVE    "180034710"         TO  WRK-SRYCDT (6)
      *            不適合
                   MOVE    "180044030"         TO  WRK-SRYCDT (7)
                   MOVE    "180044130"         TO  WRK-SRYCDT (8)
                   MOVE    "180044230"         TO  WRK-SRYCDT (9)
               WHEN    "4"
      *            呼吸
                   MOVE    "180028010"         TO  WRK-SRYCDT (1)
                   MOVE    "180028110"         TO  WRK-SRYCDT (2)
           END-EVALUATE
      *
           PERFORM VARYING     IDXT    FROM    1   BY  1
                   UNTIL      (IDXT        >   18  )
                         OR   (WRK-SRYCDT (IDXT)   =   SPACE )
      *
               IF      LNK-ORCSC-SYORIFLG      =   1
                   PERFORM VARYING     IDZ     FROM    1   BY  1
                           UNTIL  (IDZ     >   SPA-MAX-LINE )  OR
                                  (LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   SPACE )
                       IF      LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   WRK-SRYCDT (IDXT)
                           ADD     LNK-ORCSC-TEI-KAISU(IDZ)
                                               TO  WRK-TEI-CNT
                       END-IF
                   END-PERFORM
               END-IF
      *
               MOVE    SPACE               TO  SRYACT-REC
               INITIALIZE                  SRYACT-REC
               MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
               MOVE    SPA-PTID            TO  SRY-PTID
               MOVE    SPA-SRYYMD(1:6)     TO  SRY-SRYYM
               MOVE    WRK-SRYCDT (IDXT)   TO  SRY-SRYCD(1)
               MOVE    WRK-SRYCDT (IDXT)   TO  SRY-SRYCD(2)
               MOVE    WRK-SRYCDT (IDXT)   TO  SRY-SRYCD(3)
               MOVE    WRK-SRYCDT (IDXT)   TO  SRY-SRYCD(4)
               MOVE    WRK-SRYCDT (IDXT)   TO  SRY-SRYCD(5)
      *
               MOVE    SRYACT-REC          TO  MCPDATA-REC
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key12"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryact"        TO  MCP-TABLE
                   MOVE    "key12"             TO  MCP-PATHNAME
                   PERFORM 930-SRYACT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACT-REC
                   MOVE    1               TO  FLG-SRYACT
               END-IF
               PERFORM
                   UNTIL   FLG-SRYACT      =   1
      *            剤番号
                   PERFORM VARYING    IDT2     FROM    1   BY  1
                           UNTIL      IDT2     >   50
                       IF      TBL-R-ZAINUM (IDT2)     =   ZERO
                           MOVE    SRY-ZAINUM      TO  TBL-R-ZAINUM
                                                               (IDT2)
                           MOVE    SRY-NYUGAIKBN   TO  TBL-R-NYUGAIKBN
                                                              (IDT2)
                           MOVE    SRY-SRYKA       TO  TBL-R-SRYKA
                                                              (IDT2)
                           MOVE    SRY-SRYYM       TO  TBL-R-SRYYM
                                                              (IDT2)
                           MOVE    SRY-SRYKBN      TO  TBL-R-SRYKBN
                                                              (IDT2)
                       END-IF
                       IF      SRY-ZAINUM      =   TBL-R-ZAINUM (IDT2)
      *                    数量計算
                           PERFORM VARYING    IDT3     FROM    1   BY  1
                                   UNTIL     (IDT3     >   5 )
                                        OR   (SRY-SRYCD(IDT3) =   SPACE)
                               IF     SRY-SRYCD (IDT3) =   WRK-SRYCDT
                                                             (IDXT)
                                   ADD     SRY-SRYSURYO (IDT3)
                                                       TO  TBL-R-SURYO
                                                              (IDT2)
                               END-IF
                           END-PERFORM
                           MOVE    50          TO  IDT2
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_sryact"        TO  MCP-TABLE
                   MOVE    "key12"             TO  MCP-PATHNAME
                   PERFORM 930-SRYACT-READ-SEC
               END-PERFORM
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key12"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-PERFORM
      *
           IF      WRK-KYUSEI-YMD(1:6)    =   WRK-SRYYMD(1:6)
               MOVE    WRK-KYUSEI-YMD(7:2)    TO  IDE
               ADD     1                      TO  IDE
           ELSE
               MOVE    1                      TO  IDE
           END-IF
           MOVE    ZERO                TO  WRK-ACCT-CNT
      *
           PERFORM VARYING    IDT2     FROM    1   BY  1
                   UNTIL     (IDT2     >   50  )
                       OR    (TBL-R-ZAINUM (IDT2)  =   ZERO )
      *        剤回数取得
               MOVE    SPACE               TO  SRYACCT-REC
               INITIALIZE                      SRYACCT-REC
               MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
               MOVE    TBL-R-NYUGAIKBN(IDT2)
                                           TO  ACCT-NYUGAIKBN
               MOVE    SPA-PTID            TO  ACCT-PTID
               MOVE    TBL-R-SRYKA (IDT2)  TO  ACCT-SRYKA
               MOVE    TBL-R-SRYYM (IDT2)  TO  ACCT-SRYYM
               MOVE    TBL-R-SRYKBN(IDT2)  TO  ACCT-SRYKBN
               MOVE    TBL-R-ZAINUM(IDT2)  TO  ACCT-ZAINUM
      *        主キー読み
               PERFORM 940-SRYACCT-READ-SEC
               IF     (FLG-SRYACCT         =   ZERO )
                 AND  (ACCT-HKNCOMBI   NOT =   "9999")
                   PERFORM VARYING     IDE2    FROM    IDE   BY  1
                           UNTIL       IDE2    >   31
                       IF      ACCT-DAY (1 IDE2)   >   ZERO
                           COMPUTE WRK-ACCT-CNT    =   WRK-ACCT-CNT
                                                   +  (ACCT-DAY (1 IDE2)
                                                   *   TBL-R-SURYO
                                                                (IDT2))
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           .
       20021-SRYACCT-TUKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院全体チェック処理
      *****************************************************************
       2001-NYUIN-ALL-CHK-SEC         SECTION.
      *
      *@@
      *    逓減チェック処理
      *    IF     (SPA-SRYYMD         >=  "20070401" )
      *      AND  (WRK-TEIGEN-YMD NOT =   SPACE      )
      *        PERFORM  20010-TEIGEN-CHEKN-SEC
      *    END-IF
      *@@
      *    入力日分のチェック
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   31  )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      WRK-COM-NYUINDAY (IDX-Z IDX2)   >   ZERO
                   ADD     1                   TO  WRK-NYU-KAI
                   MOVE    IDX2                TO  IDD
                   MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
                   MOVE    IDD                 TO  WRK-SRYYMD (7:2)
      *            当日上限チェック
                   COMPUTE WRK-KAISU           =   WRK-COM-SURYO(IDX-Z)
                                               *   WRK-COM-KAISU(IDX-Z)
                   PERFORM 20021-DAYMAX-CHK-SEC
               END-IF
           END-PERFORM
      *
           .
       2001-NYUIN-ALL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    材料再計算編集処理
      *****************************************************************
       1007-KIZAI-TEN-SEC                SECTION.
      *
      *    酸素の点数数計算
           IF      WRK-SANSOKIN-SUM    NOT =   ZERO
               PERFORM 10071-SANSO-KEISAN-SEC
           END-IF
      *
           .
       1007-KIZAI-TEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     酸素の点数数計算処理
      *****************************************************************
       10071-SANSO-KEISAN-SEC                SECTION.
      *
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-MAX
               EVALUATE    WRK-COM-INPUTCD (IDX)
      *            酸素補正率自動加算
                   WHEN    "770020070"
      *                酸素金額＋（酸素金額×補正率／１００）　四捨五入
                       COMPUTE WRK-SYOKEIS     =   WRK-SANSOKIN-SUM
                                               *   WRK-COM-KISOTEN (IDX)
                                               /   100
                       COMPUTE WRK-SANSOKIN    =   WRK-SANSOKIN-SUM
                                               +   WRK-SYOKEIS
               END-EVALUATE
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-MAX
               EVALUATE    WRK-COM-INPUTCD (IDX)
      *            高気圧酸素計算
                   WHEN    "770030070"
      *                酸素金額＋（酸素金額×補正率／１００）
      *                                           ×気圧数　四捨五入
                       COMPUTE WRK-SYOKEIS     =   WRK-SANSOKIN
                                               *   WRK-COM-SURYO (IDX)
                       MOVE    WRK-SYOKEIS     TO  WRK-SANSOKIN
               END-EVALUATE
           END-PERFORM
      *    酸素の点数数計算
      *    金額計　四捨五入
           COMPUTE WRK-SYOKEI          =   WRK-SANSOKIN
                                       +   0.5
      *    金額計÷１０　四捨五入
           COMPUTE WRK-SYOKEIS2        =   WRK-SYOKEI
                                       /   10
                                       +   0.5
           MOVE    WRK-SYOKEIS2        TO  WRK-SANTEN
      *    酸素の点数を器材点数に加算
           ADD     WRK-SANTEN          TO  LNK-SC90-KIZTEN
      *
           .
       10071-SANSO-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全診療行為チェック処理
      *****************************************************************
       200-ALL-SYORI-SEC                SECTION.
      *
      *    他医院退院日
           MOVE    ZERO                TO  FLG-TAITAIIN
           MOVE    ZERO                TO  FLG-NINCHI
      *    診察料チェック
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE      )  OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
               IF     (WRK-COM-HKNCOMBI (IDX-Z) =   "9999" )
                   CONTINUE
               ELSE
      *            他医院退院日
                   IF      WRK-COM-INPUTCD (IDX-Z) =   "099999906"
                       MOVE    1                   TO  FLG-TAITAIIN
                   END-IF
      *            認知療法・認知行動療法
                   IF      WRK-COM-INPUTCD (IDX-Z) =   "180033210"
                       MOVE    1                   TO  FLG-NINCHI
                   END-IF
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE     )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
           IF     (WRK-COM-HKNCOMBI (IDX-Z) =   "9999" )
                CONTINUE
           ELSE
      *        リハビリ発生日と終了日の判定
               IF      WRK-COM-INPUTCD(IDX-Z)(1:7) =   "0998001"
                   PERFORM 2001-RIHACHK-SEC
               END-IF
      *        短期集中リハビリ実施加算チェック
               IF     (SPA-ERRCD           =   SPACE  )
                 AND  (LNK-ORCSC-PG        =   "K02"  )
                 AND  (WRK-COM-INPUTCD (IDX-Z) =   CON-TANKIKSN1
                                               OR  CON-TANKIKSN2 )
                   PERFORM 20012-TANKIKSN-CHK-SEC
               END-IF
      *        認知療法・認知行動療法と他の精神科専門療法とのチェック
               IF     (FLG-NINCHI          =   1      )
                 AND  (LNK-ORCSC-PG        =   "K02"  )
                   IF     (WRK-COM-CDKBN-KBN (IDX-Z)   =   "I" )
                   AND    (WRK-COM-INPUTCD (IDX-Z) NOT =   "180033210")
                       MOVE    "0266"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   END-IF
               END-IF
      *
           END-IF
           END-PERFORM
           .
       200-ALL-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    短期集中リハ実施加算チェック処理
      *****************************************************************
       20012-TANKIKSN-CHK-SEC                SECTION.
      *
      *    退院日
           IF     (LNK-ORCSC-TAIINYMD  =   SPACE )  OR
                  (LNK-ORCSC-TAIINYMD  >   SPA-SRYYMD)
               MOVE    SPACE               TO  WRK-TAIINYMD
           ELSE
               MOVE    LNK-ORCSC-TAIINYMD  TO  WRK-TAIINYMD
           END-IF
      *    他院退院日がある時、退院日
           IF      FLG-TAITAIIN        =   1
               MOVE    SPA-SRYYMD          TO  WRK-TAIINYMD
           ELSE
               IF     (SPA-TAI-TAIINYMD    =   SPACE )  OR
                      (SPA-TAI-TAIINYMD    <=  WRK-TAIINYMD)
                   CONTINUE
               ELSE
                   MOVE    SPA-TAI-TAIINYMD    TO  WRK-TAIINYMD
               END-IF
           END-IF
      *    退院日なし
           IF      WRK-TAIINYMD        =   SPACE
               IF     (WRK-COM-KZMFLG (IDX-Z)   =   SPACE)  AND
                      (LNK-ORCSC-KEI-IDX        =   ZERO )
                   MOVE    IDX-Z               TO  LNK-ORCSC-KEI-IDX
                   MOVE    "1"                 TO  WRK-COM-KZMFLG
                                                           (IDX-Z)
                   MOVE    "K264"              TO  SPA-ERRCD
               END-IF
           ELSE
      *        退院日＋１か月
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-TAIINYMD        TO  LNK-DAY6-KIJUN
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    1                   TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KOYOMI     TO  WRK-TAIINYMD-1
      *
      *        退院日＋３か月
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-TAIINYMD        TO  LNK-DAY6-KIJUN
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    3                   TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KOYOMI     TO  WRK-TAIINYMD-3
           END-IF
      *
           IF     (WRK-COM-INPUTCD (IDX-Z) =   CON-TANKIKSN1 )
             AND  (SPA-ERRCD               =   SPACE         )
      *        一か月以上
               IF      SPA-SRYYMD          >   WRK-TAIINYMD-1
                   IF     (WRK-COM-KZMFLG (IDX-Z)   =   SPACE)  AND
                          (LNK-ORCSC-KEI-IDX        =   ZERO )
                       MOVE    IDX-Z               TO  LNK-ORCSC-KEI-IDX
                       MOVE    "1"                 TO  WRK-COM-KZMFLG
                                                           (IDX-Z)
                       MOVE    "K262"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
           IF     (WRK-COM-INPUTCD (IDX-Z) =   CON-TANKIKSN2 )
             AND  (SPA-ERRCD               =   SPACE         )
      *        一か月以上
               IF      SPA-SRYYMD          >   WRK-TAIINYMD-3
                   IF     (WRK-COM-KZMFLG (IDX-Z)   =   SPACE)  AND
                          (LNK-ORCSC-KEI-IDX        =   ZERO )
                       MOVE    IDX-Z               TO  LNK-ORCSC-KEI-IDX
                       MOVE    "1"                 TO  WRK-COM-KZMFLG
                                                           (IDX-Z)
                       MOVE    "K263"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
           .
       20012-TANKIKSN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ発生日と終了日の判定処理
      *****************************************************************
       2001-RIHACHK-SEC              SECTION.
      *
           COMPUTE IDX-Z1              =   IDX-Z    +   1
           PERFORM VARYING     IDX-Z2  FROM    IDX-Z1  BY  1
                   UNTIL      (IDX-Z2      >   SPA-MAX-LINE )   OR
                              (SPA-ERRCD   NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z2)
                                               =   SPACE )
      *        リハビリ発生日と終了日の判定
               IF      WRK-COM-INPUTCD(IDX-Z)(1:8)
                                       =   WRK-COM-INPUTCD(IDX-Z2)(1:8)
                   EVALUATE    WRK-COM-INPUTCD(IDX-Z)(8:1)
                   WHEN    "1"
                       MOVE    "8030"          TO  SPA-ERRCD
                   WHEN    "2"
                       MOVE    "8032"          TO  SPA-ERRCD
                   WHEN    "3"
                       MOVE    "8034"          TO  SPA-ERRCD
                   WHEN    "4"
                       MOVE    "8036"          TO  SPA-ERRCD
                   WHEN    "5"
                       MOVE    "8038"          TO  SPA-ERRCD
                   WHEN    "6"
                       MOVE    "8040"          TO  SPA-ERRCD
                   WHEN    "7"
                       MOVE    "8042"          TO  SPA-ERRCD
                   WHEN    "8"
                       MOVE    "8049"          TO  SPA-ERRCD
                   END-EVALUATE
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z2)
               END-IF
           END-PERFORM
           .
       2001-RIHACHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動算定処理
      *****************************************************************
       300-AUTO-SYORI-SEC              SECTION.
      *
      *H26.10 から
      *    精神科継続外来支援
           IF      SPA-SRYYMD          >=  "20141001"
      *        向精神薬多剤逓減ありの時、チェック
               IF      SPA-PSYCHO-OK       =   1
                   PERFORM 3001-SEISINSYORI-SEC
               END-IF
           END-IF
      *
           .
       300-AUTO-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    精神科継続外来支援算定チェック処理
      *****************************************************************
       3001-SEISINSYORI-SEC         SECTION.
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE      )  OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
               IF     (WRK-COM-HKNCOMBI (IDX-Z) =   "9999" )
                   CONTINUE
               ELSE
      *            精神科継続外来支援・指導料
                   IF      WRK-COM-INPUTCD (IDX-Z) =   "180031410"
                       IF     (WRK-COM-KZMFLG (IDX-Z)   =   SPACE)  AND
                              (LNK-ORCSC-KEI-IDX        =   ZERO )
                           MOVE    IDX-Z           TO  LNK-ORCSC-KEI-IDX
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                           (IDX-Z)
                           MOVE    "1"             TO  WRK-COM-CUR
                                                           (IDX-Z)
                           MOVE    "K265"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       3001-SEISINSYORI-EXT.
           EXIT.
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-KANRIMST-READ-EXT.
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
      *
      *****************************************************************
      *    入院履歴マスター読込
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTNYUINRRK-REC
               MOVE    ZERO                TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                      PTNYUINRRK-REC
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    算定歴付加マスタ読み込み
      *****************************************************************
       921-SANTEIPLUS-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEIPLUS-REC
               MOVE    ZERO                TO  FLG-SANTEIPLUS
           ELSE
               INITIALIZE                      SANTEIPLUS-REC
               MOVE    1                   TO  FLG-SANTEIPLUS
           END-IF
      *
           .
       921-SANTEIPLUS-READ-EXT.
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
