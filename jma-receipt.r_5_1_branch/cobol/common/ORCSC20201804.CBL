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
       PROGRAM-ID.                 ORCSC20201804.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為チェック、算定（薬剤）
      *                      Ｈ３０年４月対応
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/03/09    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    多々納       01/04/18  麻毒の判定、薬剤数カウント
      *                                     の修正等
      *  01.00.02    多々納       01/05/21  診療種別の判定の追加
      *  02.00.00    多々納       02/03/20  改正対応
      *  02.00.01    多々納       02/03/29  臨時投薬１４日以上の組み込み
      *  02.00.02    多々納       02/04/19  生活習慣病指導算定時の追加
      *  02.00.03    NACL-多々納  02/09/04  臨時投薬の１４日以内を修正
      *  02.00.04    NACL-多々納  02/12/03  他科受診メッセージ削除
      *  02.00.05    NACL-多々納  03/07/10  入院調剤料算定なし追加
      *  02.00.06    NACL-多々納  04/03/10  改正対応
      *  02.00.07    NACL-多々納  04/06/XX  複数科・保険対応
      *  02.00.08    NACL-多々納  05/12/14  .213 自動変換対応
      *  02.00.09    NACL-多々納  05/12/14  MONFUNC 対応 他
      *  02.00.10    NACL-多々納  05/12/20  処方せん料と調基の訂正判定
      *  02.00.11    NACL-多々納  06/06/09  後発医薬品ありの判定追加
      *  03.04.00    NACL-多々納  07/02/XX  包括保険同時入力追加
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  04.02.00    NACL-多々納  08/03/XX  Ｈ２０年４月改正対応
      *  04.03.00    NACL-多々納  08/08/18  特定疾患処方管理加算単独対応
      *  04.04.00    NACL-多々納  09/02/XX  外来まとめ対応
      *  04.05.00    NACL-多々納  09/10/XX  多剤投与対応
      *  04.05.00    NACL-多々納  10/01/XX  治験対応
      *  04.05.00    NACL-多々納  10/03/XX  Ｈ２２年４月改正対応
      *  04.06.00    NACL-多々納  10/08/18  手帳記載加算確認追加
      *  04.06.00    NACL-多々納  10/11/02  難病外来疾患区分対応
      *  04.07.00    NACL-多々納  11/11/XX  訂正処方せん料展開対応
      *  04.07.00    NACL-多々納  12/03/XX  Ｈ２４年４月改正対応
      *  04.07.00    NACL-多々納  12/07/04  処方せん料判定用法対応
      *  04.07.00    NACL-多々納  12/08/10  サンプル薬（サ）対応
      *  04.07.00    NACL-多々納  14/03/XX  Ｈ２６年４月改正対応
      *  04.07.00    NACL-多々納  14/06/XX  外用薬（臨時投薬）対応
      *  04.07.00    NACL-多々納  14/09/XX  Ｈ２６年１０月改正対応
      *                                     包括エラー分対応
      *  04.07.00    NACL-多々納  14/10/07  頓服（臨時投薬）対応
      *  04.07.00    NACL-多々納  15/03/XX  Ｈ２７年４月改正対応
      *  04.07.00    NACL-多々納  15/04/XX  後発品変更不可処方毎の対応
      *  04.08.00    NACL-多々納  16/03/XX  Ｈ２８年４月改正対応
      *  04.08.00    NACL-多々納  16/04/2X  向精神薬逓減の内服逓減対応
      *  04.08.00    NACL-多々納  16/08/2X  認知症地域包括の投薬対応
      *                                     入院時持参薬対応
      *  04.08.00    NACL-多々納  17/03/XX  処方料訂正展開対応
      *                                     一般名加算対象医薬品対応
      *  04.08.00    NACL-多々納  17/04/XX  処方３０日超警告対応
      *  04.08.00    NACL-多々納  18/03/XX  Ｈ３０年４月改正対応
      *  05.00.00    NACL-多々納  19/03/XX  Ｈ３１年４月改正対応他
      *                                     （向精神薬長期処方減算）
      *  05.00.00    NACL-多々納  19/05/14  一般名処方加算判定修正
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
      **   03  FLG-SANTEI          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
      **   03  FLG-CHKMST          PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
      **** 03  FLG-PSYCHOTROPIC    PIC 9(01).
      *
           03  FLG-SRYACCT-ARI     PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-ARIOK           PIC 9(01).
           03  FLG-NAIYAK          PIC 9(01).
           03  FLG-TONPUK          PIC 9(01).
           03  FLG-GAIYAK          PIC 9(01).
           03  FLG-MADOKU          PIC 9(01).
           03  FLG-MADOKU-ARI      PIC 9(01).
           03  FLG-ZAI             PIC 9(01).
           03  FLG-NAIYAK2         PIC 9(01).
           03  FLG-TONPUK2         PIC 9(01).
           03  FLG-GAIYAK2         PIC 9(01).
      *
      **** 03  FLG-YAKJYO         PIC 9(01).
           03  FLG-ADD            PIC 9(01).
      *
           03  FLG-TOKUSYO1         PIC 9(01).
           03  FLG-TOKUSYO2         PIC 9(01).
           03  FLG-SYOHORYO         PIC 9(01).
           03  FLG-SYOHOSENN        PIC 9(01).
           03  FLG-SYOHO-CHK        PIC 9(01).
           03  FLG-SYOHO-CHK2       PIC 9(01).
      *
           03  FLG-KOUHATU          PIC 9(01).
           03  FLG-KOUHATU-HEN1     PIC 9(01).
           03  FLG-KOUHATU-HEN2     PIC 9(01).
      *    内服
           03  FLG-NAIFUKU          PIC 9(01).
           03  FLG-OK3              PIC 9(01).
      *    診察料
           03  FLG-SINSATU          PIC 9(01).
           03  FLG-JYURRK-END       PIC 9(01).
      *    長期あり
           03  FLG-CHOUKI           PIC 9(01).
      *H24.7
           03  FLG-CHOUKI2          PIC 9(01).
           03  FLG-CHOUKI3          PIC 9(01).
      *
           03  FLG-CHOKIKA1         PIC 9(01).
           03  FLG-CHOKIKA2         PIC 9(01).
      *    抗悪性腫瘍剤処方管理加算
           03  FLG-AKUSKSN1            PIC 9(01).
           03  FLG-AKUSKSN2            PIC 9(01).
      *H30.4
      *    向精神薬調整連携加算
           03  FLG-RENKSN1            PIC 9(01).
           03  FLG-RENKSN2            PIC 9(01).
      *
           03  FLG-CHOKIKSN            PIC 9(01).
      *    薬剤判定
           03  FLG-YAKUZAI-ARI         PIC 9(01).
           03  FLG-ZAIEND              PIC 9(01).
      *
           03  FLG-ALL                  PIC 9(01).
           03  FLG-YAKUJYO              PIC 9(01).
           03  FLG-CHOKI                PIC 9(01).
      *
           03  FLG-SYOHOU-ERR           PIC 9(01).
           03  FLG-TOKUTEI              PIC 9(01).
      *
           03  FLG-TENSUPLUS-CHK        PIC 9(01).
           03  FLG-TENSUPLUS            PIC 9(01).
      *
           03  FLG-CHK3                 PIC 9(01).
      *H24.4
      *    一般名処方加算
           03  FLG-IPPANKSN            PIC 9(01).
      *    一般名あり
           03  FLG-GENERIC-OK          PIC 9(01).
           03  FLG-GENERIC-NO          PIC 9(01).
           03  FLG-CHK-P               PIC 9(01).
      *    一般名あり
           03  FLG-GENERIC-OK12        PIC 9(01).
           03  FLG-GENERIC-NO2          PIC 9(01).
      *
      *サンプル薬対応
           03  FLG-SANPURU             PIC 9(01).
           03  FLG-YAKZAI-ALL          PIC 9(01).
           03  FLG-ZAIEND2             PIC 9(01).
      *
      *    地域包括診療加算有無
           03  FLG-CHKCARKSN           PIC 9(01).
           03  FLG-DEKIDAKA            PIC 9(01).
      *H26.10
      *    向精神薬多剤投与判定
           03  FLG-TAZAI1              PIC 9(01).
           03  FLG-TAZAI2              PIC 9(01).
           03  FLG-TAZAI-ARI           PIC 9(01).
           03  FLG-NASI                PIC 9(01).
           03  FLG-ARI2                PIC 9(01).
      *H29.3
           03  FLG-SYOHOU2             PIC 9(01).
           03  FLG-GENKSN2             PIC 9(01).
           03  FLG-SYOHOUMD2           PIC 9(01).
           03  FLG-TAZAI12             PIC 9(01).
           03  FLG-250IN               PIC 9(01).
           03  FLG-SGEM                PIC 9(01).
      *H30.4
      *    オンライン診療料あり
           03  FLG-ONLNE-ARI           PIC 9(01).
           03  FLG-ONLNE-AUTO          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-C               PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-SA              PIC 9(04).
           03  IDX-MAX             PIC 9(04).
      *
           03  IDX-SYU             PIC 9(04).
      *
           03  IDX-Y               PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-Y2              PIC 9(02).
           03  IDX-Y3              PIC 9(02).
           03  IDX-T1              PIC 9(04).
           03  IDX-T2              PIC 9(04).
      *
           03  IDN1                PIC 9(02).
      *
           03  IDX-SYOHOU          PIC 9(04).
           03  IDX-KA              PIC 9(04).
           03  IDX-KA2             PIC 9(04).
           03  IDX-CHOKI           PIC 9(04).
           03  IDT                 PIC 9(04).
      *
           03  IDX-GEN             PIC 9(04).
      *
           03  IDX-A1              PIC 9(04).
           03  IDX-A2              PIC 9(04).
           03  IDX-IP              PIC 9(04).
      *
           03  IDZ5                PIC 9(04).
           03  IDZ6                PIC 9(04).
      *サンプル薬対応
           03  IDX-G               PIC 9(04).
      *
           03  IDXW                PIC 9(04).
      *
      *H26.10
           03  IDX-PS              PIC 9(04).
           03  IDX-Z2              PIC 9(04).
      *H27.3
           03  IDXS-2              PIC 9(04).
           03  IDX-STR             PIC 9(04).
           03  IDXG3               PIC 9(04).
           03  IDXG4               PIC 9(04).
           03  IDX-STRG            PIC 9(04).
           03  IDX-GEN30           PIC 9(04).
           03  IDX-Z3              PIC 9(04).
      *
           03  IDXG5               PIC 9(04).
           03  IDXG6               PIC 9(04).
      *H28.6
           03  IDX-80              PIC 9(04).
      *H28.9
           03  IDX-NINCHI          PIC 9(04).
           03  IDX-NINYK           PIC 9(04).
           03  IDX-NINYK2          PIC 9(04).
      *H29.3
           03  IDX-SYOHOU2         PIC 9(04).
           03  IDX-SYOHOUMD2       PIC 9(04).
           03  IDX-G2              PIC 9(04).
           03  IDX-SGEM            PIC 9(04).
      *H30.4
           03  IDX-R1              PIC 9(04).
           03  IDX-R2              PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYCD           PIC X(09).
           03  WRK-SRYCD2          PIC X(09).
      *
           03  WRK-MOJI            PIC X(01). 
      *
           03  WRK-ERRCD           PIC X(04). 
      *
           03  WRK-ERRCD3          PIC X(04). 
           03  WRK-ERRCD6          PIC X(04). 
      *
           03  WRK-SYOKEIS         PIC 9(08)V9(8).
           03  WRK-SYOKEIS2        PIC 9(08)V9(8).
           03  WRK-SYOKEI          PIC 9(08).
      *
           03  WRK-TENSUKEI        PIC 9(08)V9(4).
           03  WRK-KAISU           PIC 9(04).
      *
           03  WRK-TENSU-KEISAN.
               05  WRK-TENSU-GAI       PIC 9(08)V9(2).
               05  WRK-TENSU-KEI       PIC 9(08)V9(2).
           03  WRK-TENSUKEI-9      PIC 9(08).
      ***  03  WRK-TENSUKEI-G      PIC 9(08).
      ***  03  WRK-TENSUKEI-GEN    PIC 9(08).
           03  WRK-TENSUKEI-G      PIC 9(08)V99.
           03  WRK-TENSUKEI-GEN    PIC 9(08)V99.
      *
           03  WRK-TEI-CNT         PIC 9(04).
      *
           03  WRK-IDX             PIC 9(04).
      *
           03  WRK-TENSU-X.
               05  WRK-TENSU-Z     PIC ZZZZZZZZ.
           03  WRK-KAISU-X.
               05  WRK-KAISU-Z     PIC ZZZZ.
           03  WRK-KAISU-H         PIC X(04).
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
               05  WRK-CHK-SRYSYUKBN   PIC X(03).
      *    保険毎の逓減計算
           03  WRK-HKNKEI-G.
               05  WRK-CNT-HKN         PIC 9(03).
               05  WRK-SANTEN-G        PIC 9(08).
               05  WRK-SANTEN-GEN      PIC 9(08).
               05  WRK-SANTENSU        PIC 9(08).
               05  WRK-SANTEN-KEI      PIC 9(08).
               05  WRK-HKNKEI-TBL      OCCURS   15.
                   07  WRK-HKNKEI-COMBI    PIC X(04).
                   07  WRK-HKNKEI-CHIKEN   PIC X(01).
                   07  WRK-HKNKEI-KEIKEI   PIC 9(08).
                   07  WRK-HKNKEI-KEIKEI-2   PIC 9(08).
      *H26.10
      *    保険毎の逓減計算（頓服）
           03  WRK-HKNKEI22-G.
               05  WRK-CNT-HKN22         PIC 9(03).
               05  WRK-KEIKEI22          PIC 9(08).
               05  WRK-SANTEN22-GEN      PIC 9(08).
               05  WRK-SANTENSU22        PIC 9(08).
               05  WRK-SANTEN22-KEI      PIC 9(08).
               05  WRK-HKNKEI22-TBL      OCCURS   15.
                   07  WRK-HKNKEI22-COMBI    PIC X(04).
                   07  WRK-HKNKEI22-CHIKEN   PIC X(01).
                   07  WRK-HKNKEI22-KEIKEI   PIC 9(08).
      *    保険毎の逓減計算（外用）
           03  WRK-HKNKEI23-G.
               05  WRK-CNT-HKN23         PIC 9(03).
               05  WRK-KEIKEI23          PIC 9(08).
               05  WRK-SANTEN23-GEN      PIC 9(08).
               05  WRK-SANTENSU23        PIC 9(08).
               05  WRK-SANTEN23-KEI      PIC 9(08).
               05  WRK-HKNKEI23-TBL      OCCURS   15.
                   07  WRK-HKNKEI23-COMBI    PIC X(04).
                   07  WRK-HKNKEI23-CHIKEN   PIC X(01).
                   07  WRK-HKNKEI23-KEIKEI   PIC 9(08).
      *H28.4
      *    保険毎の逓減計算（内服）（向精神）
           03  WRK-HKNKEI21-G.
               05  WRK-CNT-HKN21       PIC 9(03).
               05  WRK-KEIKEI21          PIC 9(08).
               05  WRK-SANTEN21-GEN      PIC 9(08).
               05  WRK-SANTENSU21        PIC 9(08).
               05  WRK-SANTEN21-KEI      PIC 9(08).
               05  WRK-HKNKEI21-TBL      OCCURS   15.
                   07  WRK-HKNKEI21-COMBI    PIC X(04).
                   07  WRK-HKNKEI21-CHIKEN   PIC X(01).
                   07  WRK-HKNKEI21-KEIKEI   PIC 9(08).
      *
      *    薬剤再計算
           03  WRK-ZAIKEI              PIC 9(08)V9(8).
           03  WRK-SC90-YKZTEN         PIC 9(08).
           03  WRK-ZAITEN-K            PIC 9(08).
           03  WRK-SYOKEIS3            PIC 9(08)V9(8).
      *
           03  FLG-GENOK               PIC 9(01).
           03  FLG-GENOK-ALL           PIC 9(01).
      *
      *H27.4から
      *    保険毎の逓減計算（３０日以上の減点）
           03  WRK-HKNKEI30-G.
      *        内服
               05  WRK-GEN30N-KEI          PIC 9(08).
               05  WRK-GEN30N-CNT          PIC 9(02).
               05  WRK-HKNKEI30-TBL        OCCURS   15.
                   07  WRK-GEN30N-COMBI        PIC X(04).
                   07  WRK-GEN30N-CHIKEN       PIC X(01).
                   07  WRK-GEN30N-HKNKEI       PIC 9(08).
                   07  WRK-GEN30N-HKNKEI5      PIC 9(08).
                   07  WRK-GEN30N-HKNKEI6      PIC 9(08).
      *        頓服
               05  WRK-GEN30P-KEI          PIC 9(08).
               05  WRK-GEN30P-CNT          PIC 9(02).
               05  WRK-HKNKEI30P-TBL        OCCURS   15.
                   07  WRK-GEN30P-COMBI        PIC X(04).
                   07  WRK-GEN30P-CHIKEN       PIC X(01).
                   07  WRK-GEN30P-HKNKEI       PIC 9(08).
                   07  WRK-GEN30P-HKNKEI5      PIC 9(08).
                   07  WRK-GEN30P-HKNKEI6      PIC 9(08).
               05  WRK-GEN30-TEN         PIC 9(08).
               05  WRK-GEN30-KEI         PIC 9(08).
               05  WRK-GEN30-KEIGK       PIC 9(08).
               05  WRK-GEN-RT            PIC 9(02).
               05  WRK-SANTEN30-KEI      PIC 9(08).
      *H28.4
               05  WRK-ZAITENSU5           PIC 9(08).
               05  WRK-ZAITENSU6           PIC 9(08).
               05  WRK-GEN30N-KEI5         PIC 9(08).
               05  WRK-GEN30N-KEI6         PIC 9(08).
               05  WRK-GEN30P-KEI5         PIC 9(08).
               05  WRK-GEN30P-KEI6         PIC 9(08).
               05  WRK-SANTEN-GEN5         PIC S9(08).
               05  WRK-SANTEN-GEN6         PIC S9(08).
               05  WRK-GEN30-TEN1          PIC 9(08).
               05  WRK-GEN30-TEN5          PIC 9(08).
               05  WRK-GEN30-TEN6          PIC 9(08).
      *
               05  WRK-ZAITENSU          PIC 9(08).
               05  WRK-GEN30-STR         PIC 9(04).
      *
      *H27.4
           03  FLG-GEN30                PIC 9(01).
           03  FLG-GEN30-N              PIC 9(01).
           03  FLG-GEN30-G              PIC 9(01).
           03  FLG-GEN30-K1             PIC 9(01).
           03  FLG-GEN30-K2             PIC 9(01).
           03  FLG-GEN30-PSY            PIC 9(01).
           03  FLG-GEN30-7GEN           PIC 9(01).
           03  FLG-YAKZ-ARI             PIC 9(01).
           03  WRK-IDXG3                PIC 9(04).
      *
      *    年齢判定用
           03  WRK-KGNAGE          PIC 9(03).
           03  WRK-JGNAGE          PIC 9(03).
      *    入院の回数
           03  WRK-NISUU-AREA.
               05  WRK-NISUU-TBL           OCCURS   31.
                   07  WRK-DAY             PIC 9(02).
                   07  WRK-SEL             PIC X(01).
           03  IDN2                    PIC 9(04).
           03  IDN3                    PIC 9(04).
           03  IDN4                    PIC 9(04).
           03  FLG-DAY                 PIC 9(01).
           03  WRK-H-DAY               PIC 9(02).
           03  WRK-H-SEL               PIC X(01).
      *    末日
           03  WRK-MATU                PIC 9(02).
      *
           03  WRK-ORC-DBPATH          PIC X(63).
      *
           03  WRK-SRYSYUKBN           PIC X(03).
      *
           03  WRK-HEN-MEI             PIC X(20).
           03  WRK-HENSYU-MEISYO       PIC X(60).
      *
           03  WRK-SRYKA               PIC X(02).
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-CHIKENKBN           PIC X(01).
           03  WRK-GENECHG-KBN         PIC X(01).
      *    薬剤毎の保険
           03  WRK-HKNCOMBI-AREA.
               05  WRK-NAI-HKNCOMBI        PIC X(04).
               05  WRK-GAI-HKNCOMBI        PIC X(04).
               05  WRK-MADO1-HKNCOMBI      PIC X(04).
               05  WRK-MADO2-HKNCOMBI      PIC X(04).
               05  WRK-CHOU-HKNCOMBI       PIC X(04).
               05  WRK-SYO-HKNCOMBI        PIC X(04).
      *        治験区分
               05  WRK-NAI-CHIKENKBN       PIC X(01).
               05  WRK-GAI-CHIKENKBN       PIC X(01).
               05  WRK-MADO1-CHIKENKBN     PIC X(01).
               05  WRK-MADO2-CHIKENKBN     PIC X(01).
               05  WRK-CHOU-CHIKENKBN      PIC X(01).
               05  WRK-SYO-CHIKENKBN       PIC X(01).
      *
           03  WRK-YAKKAKJNCD           PIC X(12).
           03  WRK-KEI-KISOTEN          PIC 9(07)V9(4).
      *H24.6
           03  WRK-ERRCD2               PIC X(04).
      *H26.4
      *    うがい薬チェック
           03  WRK-UGAI-YAK            PIC 9(03).
           03  WRK-UGAI-IGAI           PIC 9(03).
           03  IDX-UGAI                PIC 9(04).
      *
      *H26.10
           03  WRK-HEN-SRYKBN           PIC X(02).
           03  WRK-HEN-SRYSYUKBN        PIC X(03).
      *H28.4
      *    湿布薬
           03  IDX-SP70                PIC 9(04).
           03  WRK-SUM-SURYO           PIC 9(05)V9(5).
           03  WRK-SIP-SURYO           PIC 9(05)V9(5).
           03  WRK-SIP-SURYO1          PIC 9(05)V9(5).
           03  WRK-SIP-SURYO2          PIC 9(05)V9(5).
      *
           03  WRK-CHK-SURYO           PIC 9(05)V9(5).
           03  WRK-CHK-SURYO-R         REDEFINES WRK-CHK-SURYO.
               05  WRK-CHK-SURYO1      PIC 9(05).
               05  WRK-CHK-SURYO2      PIC 9(05).
      *
           03  WRK-CHK-FLG1            PIC 9(01).
           03  WRK-CHK-FLG2            PIC 9(01).
           03  WRK-SIP-KEI             PIC 9(05)V9(5).
           03  WRK-SIP-KBN             PIC X(01).
           03  FLG-SIP70-ARI           PIC 9(01).
           03  FLG-SAIKEISAN           PIC 9(01).
           03  FLG-SAIKEISAN2          PIC 9(01).
           03  IDX-ST                  PIC 9(04).
           03  IDX-SIP                 PIC 9(04).
           03  FLG-COMMENT             PIC 9(01).
           03  FLG-SIP-ARI             PIC 9(01).
      *
           03  FLG-SIP-ALL             PIC 9(01).
           03  FLG-SIP70-ALL           PIC 9(01).
           03  FLG-COM-OK              PIC 9(01).
           03  IDX-SP702               PIC 9(04).
           03  IDX-SPOK                PIC 9(04).
      *    保険毎の逓減計算（外用）
           03  WRK-HKNKEISP-G.
               05  WRK-CNT-HKNSP         PIC 9(03).
               05  WRK-KEIKEISP          PIC 9(08).
               05  WRK-SPI-KEI           PIC 9(08).
               05  WRK-SPI-SAIKEI        PIC 9(08).
               05  WRK-SPI-GENKEI        PIC S9(08).
               05  WRK-HKNKEI23-TBL      OCCURS   15.
                   07  WRK-HKNKEISP-COMBI    PIC X(04).
                   07  WRK-HKNKEISP-CHIKEN   PIC X(01).
                   07  WRK-SPI-KEI-HKN       PIC 9(08).
                   07  WRK-SPI-SAIKEI-HKN    PIC 9(08).
                   07  WRK-SPI-GENTEN        PIC S9(08).
      *
      *
           03  CNT-GENERIC             PIC 9(02).
      *
           03  CNT-GENERIC2            PIC 9(02).
           03  CNT-GENERIC3            PIC 9(02).
      *
      *H28.9
      *    認知症地域包括　投薬チェック
           03  WRK-NINCHI-CHK-G.
               05  WRK-NINCHI-ARI      PIC 9(01).
               05  WRK-NINCHI-CNT      PIC 9(03).
               05  WRK-SEICNT          PIC 9(03).
           03  FLG-NINCHIKSN           PIC 9(01).
      *
      *H30.4
      *    皮膚保湿薬チェック
           03  WRK-HIFU-YAK            PIC 9(03).
           03  IDX-HIFU                PIC 9(04).
      *
           03  WRK-FUANYAKU            PIC 9(01).
      *    分割調剤チェック
           03  WRK-BUN-KAISU           PIC 9(03).
           03  IDX-BNKT                PIC 9(04).
           03  IDX-BC                  PIC 9(04).
           03  FLG-BNKT                PIC 9(01).
      *
      *H31.3
      *    向精神薬長期投与
           03  FLG-PSYCHOKI-OK         PIC 9(01).
           03  FLG-CHOKIGEN            PIC 9(01).
           03  IDX-CHOGEN              PIC 9(04).
      *
      *    継続の時、自動発生のチェック
       01  WRK-CONTCHK-TBL.
           03  WRK-CONTCHK-OCC         OCCURS  30.
               05  WRK-TBL-SRYCD       PIC X(09).
           03  WRK-TBL-HKNCOMBI        PIC X(04).
           03  WRK-IDX-Y               PIC 9(04).
           03  IDX-TBL                 PIC 9(04).
           03  FLG-CNTCHK              PIC 9(01).
      *
      *    入院日
       01  TBL-NYUINDAY-AREA.
           03  TBL-NYUINDAY-OCC        OCCURS   31.
               05  TBL-NYUINDAY        PIC 9(01).
      *
      *    診療科・保険
       01  TBL-SRYKA-AREA.
           03  TBL-SRYKA-OCC           OCCURS   10.
               05  TBL-SRYKA           PIC X(02).
               05  TBL-HKNCOMBI        PIC X(04).
               05  TBL-CHIKENKBN       PIC X(01).
               05  TBL-GENECHG         PIC X(01).
      *H28.4
      *        湿布薬枚数制限解除
               05  TBL-SIP70           PIC X(01).
      *
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
      *    保存用
       01  HOZ-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HOZ-//.
      *H27.4
      *    ３０日逓減再計算用
       01  HZN3-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HZN3-//.
       01  HZN5-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HZN5-//.
       01  HZN6-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HZN6-//.
      *H28.4
      *    保存用
       01  HZNS-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HZNS-//.
      *
      *ver.4.7
      * 院外投薬用法毎編集
        01 TBL-YOUHO-HEN-AREA.
           03  TBL-YOUHO-ZAI-OCC        OCCURS   50.
               05  TBL-YOUHO-MAX           PIC 9(02).
               05  TBL-YOUHO-SRYSYUKBN     PIC X(03).
               05  TBL-YOUHO-KAISU         PIC 9(03).
               05  TBL-YOUHO-ZAI-OCCURS    OCCURS   50.
                   07  TBL-YOUHO-SRYCD         PIC X(09).
                   07  TBL-YOUHO-KISOTEN       PIC 9(08)V99.
                   07  TBL-YOUHO-SURYO         PIC 9(05)V9(05).
      *            内服１種類区分
                   07  TBL-YOUHO-TANDKKBN      PIC X(01).
                   07  TBL-YOUHO-ZAIGATAKBN    PIC 9(01).
                   07  TBL-YOUHO-INPUTCHI-G.
                       09  TBL-YOUHO-ATAI      PIC X(08)
                                                     OCCURS  5.
                   07  TBL-YOUHO-KEI       PIC 9(08).
           03  IDG1                        PIC 9(03).
           03  IDG2                        PIC 9(03).
           03  IDG3                        PIC 9(03).
           03  IDG4                        PIC 9(03).
           03  IDG5                        PIC 9(03).
      *
           03  TBL-YOUHO-HEN02-AREA.
               05  TBL-Y2-SRYSYUKBN        PIC X(03).
               05  TBL-Y2-YOUHOU           PIC X(09).
               05  TBL-Y2-KAISU            PIC 9(03).
               05  TBL-Y2-KINPUTCHI-G.
                   07  TBL-Y2-KATAI        PIC X(08)  OCCURS  5.
               05  TBL-Y2-ZAI-REC          OCCURS   200.
                   07  TBL-Y2-SRYCD        PIC X(09).
                   07  TBL-Y2-KISOTEN      PIC 9(08)V99.
                   07  TBL-Y2-SURYO        PIC 9(05)V9(05).
      *            内服１種類区分
                   07  TBL-Y2-TANDKKBN     PIC X(01).
                   07  TBL-Y2-ZAIGATAKBN   PIC 9(01).
                   07  TBL-Y2-INPUTCHI-G.
                       09  TBL-Y2-ATAI     PIC X(08)
                                                     OCCURS  5.
                   07  TBL-Y2-KEI          PIC 9(08).
      *
           03  WRK-ZAICNT-AL           PIC 9(08).
           03  WRK-ZAICNT2-AL          PIC 9(08).
           03  FLG-SYORI-END           PIC 9(01).
      ****
      *
      *
      *H26.10
      * 向精神薬多剤投与処方せん料
       01  CONS-TAZAI-SYOSEN       PIC X(09)   VALUE    "120003710".
      * 向精神薬多剤投与処方料
       01  CONS-TAZAI-SYOHOU       PIC X(09)   VALUE    "120003610".
      * 向精神薬多剤投与減算コード
       01  CONS-TAZAI-SRYCD        PIC X(09)   VALUE    "630010005".
      * 向精神薬多剤投与減算％
       01  CONS-RT80               PIC 9(02)   VALUE    80.
      * （精減）
       01  CONS-SGEN-SRYCD         PIC X(09)   VALUE    "820000166".
      *
      *
      *H27.4
      * 低紹介率病院　処方せん料加算
       01  CONS-TEIHP-SYOSEN       PIC X(09)   VALUE    "120003970".
      * 低紹介率病院　処方料加算
       01  CONS-TEIHP-SYOHOU       PIC X(09)   VALUE    "120003870".
      * 低紹介率病院　減算コード
       01  CONS-TEIHP-SRYCD        PIC X(09)   VALUE    "630010006".
      * 低紹介率病院　投与減算％
      *01  CONS-RT60               PIC 9(02)   VALUE    60.
       01  CONS-RT60               PIC 9(02)   VALUE    40.
      * （減）
       01  CONS-TGEN-SRYCD         PIC X(09)   VALUE    "820000047".
      *
      *
      *H28.4
      * 湿布薬枚数制限解除
       01  CONS-SP70-SRYCD         PIC X(09)   VALUE    "099200201".
      *H30.4
      * 後発外来医薬品使用体制加算
       01  CONS-GEN-KSN1           PIC X(09)   VALUE    "120004370".
       01  CONS-GEN-KSN2           PIC X(09)   VALUE    "120004070".
       01  CONS-GEN-KSN3           PIC X(09)   VALUE    "120004170".
      *
      * 湿布薬枚数減算
       01  CONS-SPGEN-SRYCD        PIC X(09)   VALUE    "630010007".
      * 湿布薬（減）
       01  CONS-SPCM-SRYCD         PIC X(09)   VALUE    "820000047".
      *
      * ７０枚を超えて湿布薬を投与した理由
       01  CONS-SP70-COMT          PIC X(09)   VALUE    "830000052".
      *
      *H30.4
       01  CONS-REC.
      *    地域包括診療加算１
           03  CONS-CHKCARKSN1     PIC X(09)   VALUE   "112021770".
      *    地域包括診療加算２
           03  CONS-CHKCARKSN2     PIC X(09)   VALUE   "112017270".
      *    認知症地域包括診療加算１
           03  CONS-NNCHIKSN1      PIC X(09)   VALUE   "112021870".
      *    認知症地域包括診療加算２
           03  CONS-NNCHIKSN2      PIC X(09)   VALUE   "112017570".
      *
      *    向精神薬調整連携加算（処方せん料）
           03  CONS-CHOSEIKSN1     PIC X(09)   VALUE   "120004570".
      *    向精神薬調整連携加算（処方料）
           03  CONS-CHOSEIKSN2     PIC X(09)   VALUE   "120004470".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    算定履歴
      *01  SANTEI-REC.
      *    COPY    "CPSANTEI.INC".
      *
      *    チェックマスタ−
      *01  CHK-REC.
      *    COPY    "CPCHK.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *    向精神薬成分コード
      *01  PSYCHOTROPIC-REC.
      *    COPY    "CPPSYCHOTROPIC.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    診療行為算定編集パラメタ（Ｈ２２年４月から）
           COPY    "CPORCSC90TEN2.INC".
      *
      *    診療行為算定歴チェックパラメタ
           COPY    "CPORCSC91.INC".
      *
      *    診療行為点数編集・基本チェック
           COPY    "CPORCSC92.INC".
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    向精神薬多剤投与チェックパラメタ
           COPY    "CPORCSPSYCHO.INC".
      *
      *H30.4
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
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
           COPY    "CPK02SPASCR.INC"   REPLACING
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
           IF      LNK-ORCSC-PG        =   "K02"
                                       OR  "KT01"
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           ELSE
               INITIALIZE                      SPA-K01KYOUTU
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
                   MOVE    "1099"          TO  SPA-ERRCD
           END-EVALUATE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    剤別のチェック、編集、自動算定処理
      *****************************************************************
       100-SEL-SYORI-SEC                SECTION.
      *
      *    チェック、自動算定
           PERFORM 1001-NAI-CHK-SEC
           IF     (WRK-GMN-INPUTCD(IDX-MAX)(1:1)
                                       =   "."   )  OR
                  (SPA-ERRCD       NOT =   SPACE )
               GO      TO      100-SEL-SYORI-EXT
           END-IF
      *
      *    点数計算
           PERFORM 1002-TENSU-KEI-SEC
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
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  IDX-SYU
           INITIALIZE                  WRK-AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE )   OR
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
      *            粉砕の時、９とする
                   IF      WRK-COM-INPUTCD(IDX-Z)      =   "099209901"
                       MOVE    "9"                     TO  WRK-MOJI
                   END-IF
               END-IF
      *        診療種別の変更
               IF      WRK-SRYSYUKBN   NOT =   SPACE
                   MOVE    WRK-SRYSYUKBN   TO  WRK-COM-SRYSYUKBN (IDX-Z)
               END-IF
      *
               EVALUATE    WRK-MOJI
                   WHEN    "0"
                       PERFORM 10010-YOYAKU-CHK-SEC
                   WHEN    "8"
                   WHEN    "S"
                   WHEN    "*"
                       CONTINUE
                   WHEN    "."
                       IF      LNK-ORCSC-PG        =   "K05"
      *                   セット登録のチェックなし
                           CONTINUE
                       ELSE
                           PERFORM 10010-SRYKBN-CHK-SEC
                       END-IF
      *
                   WHEN    OTHER
      *                編集チェックへ
                       PERFORM 10011-CHK-SEC
               END-EVALUATE
           END-PERFORM
      *
           MOVE    WRK-ERRCD          TO  SPA-ERRCD
      *
           .
       1001-NAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約コードのチェック処理
      *****************************************************************
       10010-YOYAKU-CHK-SEC             SECTION.
      *
           EVALUATE    WRK-COM-INPUTCD(IDX-Z)
      *        後発医薬品への変更不可（薬剤毎）
               WHEN    "099209903"
                   IF      (IDX-Z          >   1   ) AND
                           (WRK-COM-INPUTCD(IDX-Z - 1)(1:1)
                                               =   "6"  )
      **               後発医薬品区分判定
      *!               IF      WRK-COM-KOUHATUKBN (IDX-Z - 1) =   "1"
      *                    MOVE    "2065"          TO  WRK-ERRCD
      *                    MOVE    "1"             TO  WRK-COM-CUR
      *                                                      (IDX-Z)
      *!               END-IF
      *                マスタ更新後
      *                IF      SPA-DBUPDKBN        =   1
      *                後発医薬品区分判定
      *                    IF      WRK-COM-KOUHATUKBN (IDX-Z - 1)
      *                                                =   ZERO
      *                        MOVE    "2066"          TO  WRK-ERRCD
      *                        MOVE    "1"             TO  WRK-COM-CUR
      *                                                      (IDX-Z)
      *                    END-IF
      *****            END-IF
                       CONTINUE
                   ELSE
                       MOVE    "2064"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR
                                                             (IDX-Z)
                   END-IF
      *        含量規格変更不可（薬剤毎）
               WHEN    "099209905"
      *        剤型変更不可（薬剤毎）
               WHEN    "099209906"
      *            前に薬剤があること
                   COMPUTE IDZ5                =   IDX-Z   -   1
                   MOVE    ZERO                TO  FLG-CHK3
                   PERFORM VARYING     IDZ6    FROM    IDZ5  BY  -1
                           UNTIL      (IDZ6    <   1   )
                                OR    (WRK-ERRCD   NOT =   SPACE )
                                OR    (FLG-CHK3        =   1     )
                       IF      (WRK-COM-INPUTCD(IDZ6)
                                                   =   "099209905"
                                                   OR  "099209906"
                                                   OR  "099209907"
                                                   OR  "099209908" )
                           CONTINUE
                       ELSE
                           IF      (WRK-COM-INPUTCD(IDZ6)(1:1)
                                                           =   "6"  )
                               MOVE    1               TO  FLG-CHK3
                           ELSE
                               MOVE    1               TO  IDZ6
                           END-IF
                       END-IF
                   END-PERFORM
                   IF      FLG-CHK3            =   ZERO
                       MOVE    "0267"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR
                                                             (IDX-Z)
                   END-IF
      *H24.4
      *        銘柄名記載
               WHEN    "099209907"
      *        一般名記載
               WHEN    "099209908"
      *            前行に薬剤があること
                   IF      (IDX-Z          >   1   ) AND
                           (WRK-COM-INPUTCD(IDX-Z - 1)(1:1)
                                               =   "6"  )
                       CONTINUE
                   ELSE
                       MOVE    "2071"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR
                                                             (IDX-Z)
                   END-IF
           END-EVALUATE
      *
           .
       10010-YOYAKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    投薬の処方のみチェック処理
      *****************************************************************
       10010-SRYKBN-CHK-SEC             SECTION.
      *
           MOVE    WRK-COM-SRYSYUKBN (IDX-Z)
                                       TO  WRK-SRYSYUKBN
           EVALUATE    WRK-GMN-INPUTCD(IDX-Z)(2:3)
               WHEN    "213"
               WHEN    "223"
               WHEN    "233"
      *
      *            投薬の処方のみは、投薬が算定できない時のみ
                   IF      (LNK-ORCSC-ROUMANSEI-ARI    =   ZERO) AND
                           (LNK-ORCSC-ROUMANSEI-ARI2   =   ZERO) AND
                           (LNK-ORCSC-NETAKIRI-ARI     =   ZERO) AND
                           (LNK-ORCSC-NETAKIRI-ARI2    =   ZERO) AND
                           (LNK-ORCSC-SYONIKA-ARI      =   ZERO)  AND
                           (LNK-ORCSC-SEIKATU-ARI2     =   ZERO) AND
                           (LNK-ORCSC-SEIKATU-ARI      =   ZERO) AND
                           (SPA-ZAIMATU-ARI2           =   ZERO) AND
                           (SPA-ZAIMATU-ARI            =   ZERO)
                       AND (SPA-ZAIKANRI-ARI           =   ZERO)
                       AND (SPA-ZAIKANRI-ARI2          =   ZERO)
      *                特定施設入居時等医学管理料
                       AND (SPA-IGAKUKAN-ARI           =   ZERO)
                       AND (SPA-IGAKUKAN-ARI2          =   ZERO)
      *H28.4           小児かかりつけ診療料
                       AND (SPA-SYONIKKR-ARI           =   ZERO)
                       AND (SPA-SYONIKKR-ARI2          =   ZERO)
      *************    MOVE    "0074"          TO  WRK-ERRCD
      *                MOVE    "1"             TO  WRK-COM-CUR
      *************                                          (IDX-Z)
      *                自動変換なしはエラーとする
                       IF      SPA-CHGSRYSYU-FLG   =   "1"
                           MOVE    "0074"          TO  WRK-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                             (IDX-Z)
                       ELSE
                           MOVE    "0"             TO  WRK-SRYSYUKBN
                                                               (3:1) 
                           MOVE    "0"             TO  WRK-GMN-INPUTCD
                                                        (IDX-Z)(4:1)
                           PERFORM 100101-SRYSYUMEI-HEN-SEC
                       END-IF
                   END-IF
      *            生活習慣病指導管理料チェック
                   IF     (SPA-SEIKATU-ARI     NOT =   ZERO )  OR
                          (SPA-SEIKATU-ARI2    NOT =   ZERO )
                       IF      SPA-HKTSANTEI-203       =   1
                           MOVE    "0"             TO  WRK-SRYSYUKBN
                                                               (3:1) 
                           MOVE    "0"             TO  WRK-GMN-INPUTCD
                                                        (IDX-Z)(4:1)
                           PERFORM 100101-SRYSYUMEI-HEN-SEC
                       END-IF
                   END-IF
      *            小児科外来診察料チェック
                   IF      (LNK-ORCSC-SYONIKA-ARI  NOT =   ZERO)
                       IF      SPA-HKTSANTEI-202       =   1
                           MOVE    "0"             TO  WRK-SRYSYUKBN
                                                               (3:1) 
                           MOVE    "0"             TO  WRK-GMN-INPUTCD
                                                        (IDX-Z)(4:1)
                           PERFORM 100101-SRYSYUMEI-HEN-SEC
                       END-IF
                   END-IF
      *H28.4       小児かかりつけチェック
                   IF      (SPA-SYONIKKR-ARI   NOT =   ZERO)
                      OR   (SPA-SYONIKKR-ARI2  NOT =   ZERO )
                       IF      SPA-HKTSANTEI-209       =   1
                           MOVE    "0"             TO  WRK-SRYSYUKBN
                                                               (3:1) 
                           MOVE    "0"             TO  WRK-GMN-INPUTCD
                                                        (IDX-Z)(4:1)
                           PERFORM 100101-SRYSYUMEI-HEN-SEC
                       END-IF
                   END-IF
      *ver.4.6
      *            在宅時医学総合管理料チェック
                   IF     (SPA-ZAIKANRI-ARI    NOT =   ZERO )  OR
                          (SPA-ZAIKANRI-ARI2   NOT =   ZERO )
                       IF      SPA-HKTSANTEI-204       =   1
                           MOVE    "0"             TO  WRK-SRYSYUKBN
                                                               (3:1) 
                           MOVE    "0"             TO  WRK-GMN-INPUTCD
                                                        (IDX-Z)(4:1)
                           PERFORM 100101-SRYSYUMEI-HEN-SEC
                       END-IF
                   END-IF
      *            特定施設入居時等医学管理料チェック
                   IF     (SPA-IGAKUKAN-ARI    NOT =   ZERO )  OR
                          (SPA-IGAKUKAN-ARI2   NOT =   ZERO )
                       IF      SPA-HKTSANTEI-205       =   1
                           MOVE    "0"             TO  WRK-SRYSYUKBN
                                                               (3:1) 
                           MOVE    "0"             TO  WRK-GMN-INPUTCD
                                                        (IDX-Z)(4:1)
                           PERFORM 100101-SRYSYUMEI-HEN-SEC
                       END-IF
                   END-IF
      *            在宅末期医療総合診察料チェック
                   IF     (SPA-ZAIMATU-ARI    NOT =   ZERO )  OR
                          (SPA-ZAIMATU-ARI2   NOT =   ZERO )
                       IF      SPA-HKTSANTEI-206       =   1
                           MOVE    "0"             TO  WRK-SRYSYUKBN
                                                               (3:1) 
                           MOVE    "0"             TO  WRK-GMN-INPUTCD
                                                        (IDX-Z)(4:1)
                           PERFORM 100101-SRYSYUMEI-HEN-SEC
                       END-IF
                   END-IF
      *        WHEN    "212"
      *        WHEN    "222"
      *        WHEN    "232"
      *            院外処方は外来のみ
      *            IF      SPA-NYUGAIKBN   =   "1"
      *                MOVE    "9028"          TO  WRK-ERRCD
      *                MOVE    "1"             TO  WRK-COM-CUR
      *                                                      (IDX-Z)
      *            END-IF
               WHEN    "214"
               WHEN    "224"
               WHEN    "234"
      *            入院調剤料なしは入院のみ
                   IF      SPA-NYUGAIKBN   =   "2"
                       MOVE    "9024"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR
                                                             (IDX-Z)
                   END-IF
           END-EVALUATE
           .
       10010-SRYKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療種別名編集処理
      *****************************************************************
       100101-SRYSYUMEI-HEN-SEC             SECTION.
      *
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE       TO  WRK-GMN-MEISYO (IDX-Z)
                   WHEN    WRK-SRYSYUKBN       =   MSYU-SRYSYUKBN 
                                                           (MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                       TO  WRK-GMN-MEISYO (IDX-Z)
           END-SEARCH 
      *
           .
       100101-SRYSYUMEI-HEN-EXT.
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
      *            乳幼児の自動加算を追加する
                   PERFORM 10011-NENREI-SET-SEC
      *            粉砕のチェック
               WHEN    "9"
                   IF      WRK-COM-INPUTCD(IDX-Z + 1)  NOT =   SPACE
                       IF     (WRK-COM-INPUTCD(IDX-Z + 1)(1:1)
                                                       =   "6"  )  AND
                              (WRK-COM-ZAIGATAKBN(IDX-Z + 1)
                                                       =   0    )
                           CONTINUE
                       ELSE
                           MOVE    "0063"          TO  WRK-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                            (IDX-Z + 1)
                       END-IF
                   END-IF
      *            薬剤の診療種別
               WHEN    "6"
                   IF      WRK-SRYSYUKBN       =   SPACE
                       IF      WRK-COM-SRYSYUKBN (IDX-Z) NOT =   SPACE
                           MOVE    WRK-COM-SRYSYUKBN (IDX-Z)
                                                   TO  WRK-SRYSYUKBN
                        END-IF
                   END-IF
      *ver.4.7
      *            内服滴剤の数量チェック
                   IF    (WRK-COM-SOURYOHENKBN (IDX-Z) =   "1" )
                   AND   (LNK-ORCSC-KAISU          >   1       )
                   AND   (WRK-SRYSYUKBN(1:2)       =   "21"  )
      *                内服は総量入力
                       IF     (WRK-COM-KZMFLG (IDX-Z)
                                                       =   SPACE) AND
                              (LNK-ORCSC-KEI-IDX   =   ZERO )
                           MOVE    "K119"          TO  WRK-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-Z)
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                              (IDX-Z)
                           MOVE    IDX-Z           TO  LNK-ORCSC-KEI-IDX
                       END-IF
                   END-IF
      *
           END-EVALUATE
      *
           .
       10011-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢加算追加処理（共通）
      *****************************************************************
       10011-NENREI-SET-SEC            SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   4       )   OR
                              (WRK-COM-AGEKSNSRYCD(IDX-Z IDX2)
                                               =   SPACE  OR
                                                   ZERO )
      *        加算追加
               MOVE    WRK-COM-AGEKSNSRYCD (IDX-Z IDX2)  TO  WRK-SRYCD
      *        加算料チェック処理
               MOVE    IDX-Z               TO  IDX-S
               PERFORM 100111-KASAN-CHK-SEC
      *        加算料追加処理
      *********MOVE    IDX-Z               TO  IDX-S
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
      *H27.4
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
      *           減算の後
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
      *
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
           MOVE    WRK-SCR-REC         TO  HOZ-SCR-REC
           INITIALIZE                  ORCSC92AREA
           IF      FLG-ARI             =   ZERO
               MOVE    "1"                 TO  LNK-SC92-KBN
           ELSE
               MOVE    SPACE               TO  WRK-COM-CHKFLG (IDX-S)
           END-IF
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI   TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           MOVE    LNK-ORCSC-KT01-AREA
                                       TO  LNK-SC92-KT01-AREA
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
               MOVE    HOZ-SCR-REC         TO  WRK-SCR-REC
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
           INITIALIZE                  ORCSC90AREA
           MOVE    1                   TO  IDX-STR
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )   OR
                              (SPA-ERRCD       NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               MOVE    SPACE               TO  WRK-GMN-SURYO (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-TENSU (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-KAISU (IDX-Z)
               MOVE    ZERO                TO  WRK-GMN-KEI   (IDX-Z)
               MOVE    ZERO                TO  WRK-COM-SYUTEN1 (IDX-Z)
               MOVE    ZERO                TO  WRK-COM-SYUTEN2 (IDX-Z)
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
                   WHEN    "8"
                   WHEN    "."
                   WHEN    "*"
                       CONTINUE
                   WHEN    "S"
      *                編集チェックへ
                       PERFORM 10023-SETHEN-SEC
      *
                   WHEN    OTHER
      *                編集チェックへ
                       PERFORM 10021-KEISAN-SEC
               END-EVALUATE
           END-PERFORM
      *
      *    最終行
      *    割合計算加算
           IF      IDX-SYU             =   ZERO
               MOVE    1                   TO  IDX-SYU
           END-IF
      *    割合加算
           PERFORM 1006-KASAN-SET-SEC
      *    合計点数計算・編集
           MOVE    "2"                 TO  LNK-SC90-KBN
           MOVE    ZERO                TO  LNK-SC90-IDX
           MOVE    1                   TO  LNK-SC90-IDX-STR
           MOVE    IDX-MAX             TO  LNK-SC90-IDX-END
           CALL    "ORCSC90TEN2"       USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *H31.2
      *    桁数チェック
           IF      LNK-SC90-ERRCD  NOT =   SPACE
               MOVE    LNK-SC90-ERRCD      TO  SPA-ERRCD
               MOVE    "1"                 TO  WRK-COM-CUR (IDX-MAX)
           END-IF
      *
      *    計
      *    外用薬時、回数は１とする
           IF      WRK-COM-SRYKBN (IDX-MAX)    =   "23"
               MOVE   1                    TO  LNK-ORCSC-KAISU
               MOVE    WRK-COM-TENSU  (IDX-MAX)
                                               TO  WRK-COM-KEI (IDX-MAX)
           ELSE
               COMPUTE WRK-COM-KEI (IDX-MAX)
                                           =   WRK-COM-TENSU  (IDX-MAX)
                                           *   LNK-ORCSC-KAISU
      *H31.3
      *        最大７桁
               IF     (WRK-COM-TENSU(IDX-MAX)  *   LNK-ORCSC-KAISU)
                                           >   9999999
                   MOVE    "0249"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR(IDX-MAX)
               END-IF
      *
           END-IF
      *
      *    院外の時、点数をゼロとする
           MOVE    WRK-COM-SRYSYUKBN(IDX-MAX)  TO  WRK-CHK-SRYSYUKBN
      *----(01.00.01) LINE ADD START ----------------------------------
           IF      WRK-CHK-SRYSYUKBN       =   SPACE
               EVALUATE    WRK-COM-SRYKBN (IDX-MAX)
                   WHEN    "21"
                       MOVE    "210"           TO  WRK-CHK-SRYSYUKBN
                   WHEN    "22"
                       MOVE    "220"           TO  WRK-CHK-SRYSYUKBN
                   WHEN    "23"
                       MOVE    "230"           TO  WRK-CHK-SRYSYUKBN
               END-EVALUATE
           END-IF
           IF     (WRK-CHK-SRYSYUKBN           =   "212"  OR
                                                   "222"  OR
                                                   "232"  OR
                                                   "292"  OR
                                                   "295"  OR
                                                   "298" )  OR
                 ((LNK-ORCSC-INGAIKBN          =   "1"   ) AND
                  (WRK-CHK-SRYSYUKBN           =   "210"  OR
                                                   "220"  OR
                                                   "230"  OR
                                                   "293"  OR
                                                   "290"  OR
                                                   "296" ))  OR
                 ( WRK-CHK-SRYSYUKBN           =   "213"  OR
                                                   "223"  OR
                                                   "233" ) 
      *ver.4.7
      *        院外処方の点数
               IF     (WRK-CHK-SRYSYUKBN       =   "213"  OR
                                                   "223"  OR
                                                   "233" )
                   CONTINUE
               ELSE
                   MOVE    WRK-COM-TENSU  (IDX-MAX)
                                       TO  WRK-COM-INGAITEN (IDX-MAX)
               END-IF
      *
               MOVE    ZERO            TO  WRK-COM-KEI    (IDX-MAX)
               MOVE    ZERO            TO  WRK-COM-TENSU  (IDX-MAX)
               MOVE    ZERO            TO  WRK-COM-YKZTEN (IDX-MAX)
               MOVE    ZERO            TO  WRK-COM-YKZTEN-M(IDX-MAX)
               MOVE    ZERO            TO  WRK-COM-KIZAITEN(IDX-MAX)
               MOVE    ZERO            TO  WRK-COM-KIZTEN-M(IDX-MAX)
           END-IF
      *
      *    セット用合計点数
           MOVE    WRK-COM-KEI  (IDX-MAX)  TO  LNK-ORCSC-ZAIKEI
           MOVE    WRK-COM-TENSU(IDX-MAX)  TO  LNK-ORCSC-ZAITEN
      *
           .
       1002-TENSU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算処理
      *****************************************************************
       10021-KEISAN-SEC                  SECTION.
      *
      *    外用薬時、回数は１回とする（数量＊回数を数量とする）
           IF     (WRK-COM-SRYKBN (IDX-Z)  =   "23" ) AND
                 ((WRK-COM-INPUTCD(IDX-Z)(1:1) =   "6"
                                               OR  "7" )  OR
                  (WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059" ))
               COMPUTE WRK-COM-SURYO (IDX-Z)   =   WRK-COM-SURYO2(IDX-Z)
                                               *   LNK-ORCSC-KAISU
               MOVE    1                   TO  WRK-KAISU
           ELSE
               MOVE    LNK-ORCSC-KAISU     TO  WRK-KAISU
           END-IF
      *
      *H27.4
      *    手技料判定
           IF      (WRK-MOJI                   =   "1"  )  AND
                   (WRK-COM-DATAKBN (IDX-Z)    =   "1"  )
               IF      IDX-SYU             >   ZERO
      *            割合加算
                   PERFORM 1006-KASAN-SET-SEC
      *            点数計算
                   PERFORM 1007-TENSU-SUM-SEC
               END-IF
               MOVE    IDX-Z               TO  IDX-SYU
           END-IF
      *
      *    手技点数計算
           EVALUATE    WRK-MOJI
               WHEN    "1"
                   PERFORM 100211-SYUGITEN-KEI-SEC
           END-EVALUATE
      *    回数
           MOVE    WRK-KAISU           TO  WRK-COM-KAISU (IDX-Z)
      *
      *H24.4改正
           IF      (WRK-MOJI           =   "6"  )  AND
                   (WRK-COM-SRYKBN (IDX-Z)     =   "21" )
      *        院外内服投薬
               IF    (WRK-COM-SRYSYUKBN(IDX-Z) =   "212"
                                               OR  "292" )  OR
                    ((WRK-COM-SRYSYUKBN(IDX-Z)  =   SPACE
                                                OR  "210"
                                                OR  "290" ) AND
                     (LNK-ORCSC-INGAIKBN       =   "1"   ))
      *            一般名記載有無
                   MOVE    ZERO                TO  FLG-CHK-P
      ************ IF     (WRK-COM-KOKUJISKBKBN2(IDX-Z)  =   9  )
                   IF     (WRK-COM-IPN-KISAIKBN (IDX-Z)  =   1  )
      *H27.4           後発品変更可
                     OR   (SPA-GENECHG-KBN             =   2   )
      *
                       MOVE    1                   TO  FLG-CHK-P
      *H27.4           後発品変更不可
                       IF      SPA-GENECHG-KBN     =   1
                           MOVE    ZERO                TO  FLG-CHK-P
                       END-IF
      *
                       IF      (WRK-COM-INPUTCD (IDX-Z + 1)
                                                   =   "099209907"
                                                   OR  "099209903"  )
      *                    銘柄名記載、後発品変更不可
                           MOVE    ZERO                TO  FLG-CHK-P
                       END-IF
                   END-IF
                   IF     ( WRK-COM-INPUTCD (IDX-Z + 1)
                                                   =   "099209908"  )
      *                 一般名記載あり
                       MOVE    1                   TO  FLG-CHK-P
                   END-IF
                   IF      (FLG-CHK-P          =   1   )  AND
                           (WRK-COM-KOUHATUKBN (IDX-Z) =   "1"
                                                       OR  "2"
                                                       OR  "3"
                                                       OR  "4" )
      *ver.4.7         最低薬価を金額へ
                       MOVE    WRK-COM-GEN-PRICE(IDX-Z)
                                               TO  WRK-COM-KISOTEN
                                                             (IDX-Z)
                   END-IF
               END-IF
           END-IF
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
                   ADD     WRK-COM-KISOTEN (IDX-Z) TO  WRK-TENSU-GAI
               ELSE
                   ADD     WRK-COM-KISOTEN(IDX-Z)  TO  WRK-TENSU-KEI
               END-IF
           END-IF
           .
       100211-SYUGITEN-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    割合加算計算処理
      *****************************************************************
       1006-KASAN-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-TENSUKEI-G
           MOVE    ZERO                TO  WRK-TENSUKEI-GEN
      *    １剤１手技とする
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE     )   OR
                             ((WRK-GMN-INPUTCD (IDX)   =   SPACE ) AND
                              (WRK-COM-INPUTCD (IDX)   =   SPACE ))
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )
                   COMPUTE WRK-TENSUKEI        =   WRK-TENSU-KEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
               END-IF
      *        %減算　（手技料の点数を所定点数とする)
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   6   )
                   COMPUTE WRK-TENSUKEI        =   WRK-COM-KISOTEN
                                                             (IDX-SYU)
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100.00
                                               +   0.4
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-GEN
               END-IF
           END-PERFORM
      *
           COMPUTE LNK-SC90-TENSU      =   WRK-TENSU-KEI
                                       +   WRK-TENSU-GAI
                                       +   WRK-TENSUKEI-G
                                       -   WRK-TENSUKEI-GEN
           .
       1006-KASAN-SET-EXT.
           EXIT.
      *H27.4
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
      *    約束セット回数処理
      *****************************************************************
       10023-SETHEN-SEC                SECTION.
      *
      *    外用薬時、回数は１回とする（数量＊回数を数量とする）
           IF      WRK-COM-SRYKBN (IDX-Z)  =   "23"
               COMPUTE WRK-COM-SURYO (IDX-Z)   =   WRK-COM-SURYO2(IDX-Z)
                                               *   LNK-ORCSC-KAISU
           END-IF
      *
           .
       10023-SETHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全診療行為チェック処理
      *****************************************************************
       200-ALL-SYORI-SEC                SECTION.
      *
      *H27.4
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                             ((WRK-GMN-INPUTCD(IDX) =   SPACE) AND
                              (WRK-COM-INPUTCD(IDX) =   SPACE))
                          OR  (SPA-ERRCD       NOT =   SPACE  )
      *        低紹介率病院　３０日逓減処方料・処方せん料
               IF     (WRK-COM-HKNCOMBI(IDX)   NOT =   "9999")
                 AND  (WRK-COM-INPUTCD (IDX)   =   CONS-TEIHP-SYOSEN
                                               OR  CONS-TEIHP-SYOHOU )
                   IF      SPA-TEIRITU-KBN     =   ZERO
                       IF     (WRK-COM-KZMFLG (IDX)
                                                   =   SPACE) AND
                              (LNK-ORCSC-KEI-IDX   =   ZERO )
                           MOVE    "K256"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                          (IDX)
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                          (IDX)
                           MOVE    IDX            TO  LNK-ORCSC-KEI-IDX
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      200-ALL-SYORI-EXT
           END-IF
      *
      *    包括まとめ入力は以下対象外
           IF     (SPA-HKNCOMBINUM     =   "9999" )  OR
                  (LNK-ORCSC-PG    NOT =   "K02"  )
               CONTINUE
           ELSE
      *H28.4
      *        湿布薬枚数判定用（診療科テーブル）
               INITIALIZE                  TBL-SRYKA-AREA
      *
               MOVE    ZERO                TO  IDX-CHOKI
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                                 ((WRK-GMN-INPUTCD(IDX) =   SPACE) AND
                                  (WRK-COM-INPUTCD(IDX) =   SPACE))
      *@
               IF      WRK-COM-HKNCOMBI (IDX)  NOT =   "9999"
      *            調基の判定
                   IF     (WRK-COM-SRYKBN (IDX)    =   "27"  )  AND
                          (WRK-COM-INPUTCD(IDX)    =   "120001810" OR
                                                       "120001710")
                       MOVE    IDX                 TO  IDX-CHOKI
                   END-IF
      *@
               END-IF
      *H28.4
      *            湿布薬換算数量クリア
                   MOVE    ZERO        TO  WRK-COM-SIP-SURYO(IDX)
                   MOVE    SPACE       TO  WRK-COM-SIPGEN (IDX)
      *            診療科毎　湿布薬
                   IF     (WRK-COM-HKNCOMBI (IDX)  NOT =   "9999" )
                       IF      WRK-COM-SRYKA (IDX)     =   SPACE
                           MOVE    SPA-SRYKA   TO  WRK-COM-SRYKA(IDX)
                       END-IF
                       PERFORM VARYING     IDX-KA  FROM    1   BY  1
                               UNTIL       IDX-KA  >   10
                           IF      TBL-SRYKA (IDX-KA)  =   SPACE
                               MOVE    WRK-COM-SRYKA(IDX)  TO  TBL-SRYKA
                                                              (IDX-KA)
                           END-IF
                           IF     (TBL-SRYKA (IDX-KA)  =   WRK-COM-SRYKA
                                                               (IDX))
                              IF     (WRK-COM-INPUTCD (IDX)
                                                   =   CONS-SP70-SRYCD)
      *                            湿布薬枚数制限解除
                                   MOVE    "1"         TO  TBL-SIP70
                                                              (IDX-KA)
                               END-IF
                               MOVE    10              TO  IDX-KA
                           END-IF
                       END-PERFORM
                   END-IF
      *
      *H29.4
      *            投薬日数が３０日超は警告
                   IF     (WRK-COM-HKNCOMBI(IDX)   NOT =   "9999")
                     AND  (SPA-SHOHOU30CHK         =   "1" )
                     AND  (WRK-COM-SRYKBN (IDX)    =   "21"
                                                   OR  "22")
                     AND  (WRK-COM-ZAIEND (IDX)    =   "1" )
                     AND  (WRK-COM-KAISU  (IDX)    >   30  )
                       IF     (WRK-COM-KZMFLG (IDX)
                                                   =   SPACE) AND
                              (LNK-ORCSC-KEI-IDX   =   ZERO )
                           MOVE    "K268"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                          (IDX)
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                          (IDX)
                           MOVE    IDX             TO  LNK-ORCSC-KEI-IDX
                       END-IF
                   END-IF
      *
               END-PERFORM
      *        調基が算定時の処方せん料とのチェック
               IF      IDX-CHOKI           >   ZERO
      *            処方せん料の算定チェック
                   PERFORM 30024-SYOHOUSEN-CHK-SEC
                   IF      FLG-SYOHOU-ERR  NOT =   ZERO
                       MOVE    "1039"      TO  SPA-ERRCD
                       MOVE    "1"         TO  WRK-COM-CUR (IDX-CHOKI)
                   END-IF
               END-IF
      *
           END-IF
      *
      *H28.4
      *    湿布薬枚数判定
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-NYUGAIKBN       =   "2"   )
              AND (SPA-HKNCOMBINUM NOT =   "9999" )
              AND (LNK-ORCSC-PG        =   "K02"  )
               PERFORM 2001-SIPPU-CHK-SEC
           END-IF
      *
      *H30.4
      *    分割調剤コードチェック
           IF     (SPA-ERRCD           =   SPACE  )
               PERFORM 2004-BUNKATUCHK-SEC
           END-IF
           .
       200-ALL-SYORI-EXT.
           EXIT.
      *
      *H28.4
      *****************************************************************
      *    湿布薬枚数判定処理
      *****************************************************************
       2001-SIPPU-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDX-SP70
           MOVE    ZERO                TO  FLG-SAIKEISAN
      *
           PERFORM VARYING     IDX-KA  FROM    1   BY  1
                   UNTIL      (IDX-KA      >   10   )
                           OR (TBL-SRYKA (IDX-KA)  =   SPACE )
               MOVE    ZERO                TO  WRK-SUM-SURYO
      *
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                                 ((WRK-GMN-INPUTCD(IDX) =   SPACE) AND
                                  (WRK-COM-INPUTCD(IDX) =   SPACE))
                              OR ( TBL-SIP70 (IDX-KA)  =   "1"   )
      *
                   IF     (WRK-COM-SRYKA (IDX) NOT =   TBL-SRYKA
                                                            (IDX-KA))
                   OR     (WRK-COM-YAKUHYO (IDX)   =   "1"  )
                   OR     (WRK-COM-HKNCOMBI(IDX)   =   "9999")
                   OR     (WRK-COM-GENTEN  (IDX)   =   "1"    )
      *H28.9       持参薬
                   OR     (WRK-COM-JISANYAKU(IDX)  =   "1"   )
                       CONTINUE
                   ELSE
      *                湿布薬判定
                       IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "6" )
                          AND (WRK-COM-SRYKBN (IDX)    =   "23"    )
                          AND (WRK-COM-SIPPUKBN (IDX)  =   "1"     )
      *                    湿布薬換算数量
                           MOVE    WRK-COM-SURYO (IDX) TO  WRK-SIP-SURYO
      *                    枚数以外は枚数へ変換
                           IF     (WRK-COM-TANICD (IDX)   NOT =  "006")
                              AND (WRK-COM-SIPRYO1(IDX)   NOT =  ZERO )
                               PERFORM 20011-SIP-SURYO-SEC
                           END-IF
      *                    ７０枚判定
                           IF    ( WRK-SIP-SURYO   +   WRK-SUM-SURYO)
                                                       <=  70
                               MOVE    WRK-COM-SURYO (IDX) TO
                                               WRK-COM-SIP-SURYO(IDX)
                           ELSE
      *                        計算用数量
                               IF     (WRK-SUM-SURYO   <   70 )
      *                            数量換算
                                   PERFORM 20012-SURYO-SIP-SEC
                               ELSE
                                   MOVE    ZERO    TO  WRK-COM-SIP-SURYO
                                                                   (IDX)
                               END-IF
                               MOVE    "1"         TO  WRK-COM-SIPGEN
                                                                   (IDX)
                               MOVE    1           TO  FLG-SAIKEISAN
                           END-IF
                           ADD     WRK-SIP-SURYO       TO  WRK-SUM-SURYO
                           IF      WRK-SUM-SURYO       >   70
                               IF      WRK-COM-KZMFLG7 (IDX)  =   SPACE
                                   IF      IDX-SP70        =   ZERO
                                       MOVE    IDX         TO  IDX-SP70
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           IF      IDX-SP70        NOT =   ZERO
               IF     (WRK-COM-KZMFLG7 (IDX-SP70)   =   SPACE )
                   MOVE    "K108"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-KZMFLG7
                                                       (IDX-SP70)
                   MOVE    "1"             TO  WRK-COM-CUR
                                                       (IDX-SP70)
               END-IF
           END-IF
      *
      *H28.4
           IF     (LNK-ORCSC-KBN2  NOT =   SPACE )
             AND  (SPA-ERRCD           =   SPACE )
      *        コメント判定
               PERFORM 20012-SIPCOM-CHK-SEC
           END-IF
      *
           .
       2001-SIPPU-CHK-EXT.
           EXIT.
      *
      *H28.4
      *****************************************************************
      *    湿布薬換算数量処理
      *****************************************************************
       20011-SIP-SURYO-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-SIP-SURYO
           MOVE    ZERO                TO  WRK-SIP-SURYO1
           MOVE    ZERO                TO  WRK-SIP-SURYO2
           MOVE    ZERO                TO  WRK-CHK-FLG1
           MOVE    ZERO                TO  WRK-CHK-FLG2
           IF      WRK-COM-SIPRYO1(IDX)  NOT =   ZERO
               COMPUTE WRK-SIP-SURYO1      =   WRK-COM-SURYO (IDX)
                                           /   WRK-COM-SIPRYO1(IDX)
               MOVE    WRK-SIP-SURYO1      TO  WRK-CHK-SURYO
               IF      WRK-CHK-SURYO2      =   ZERO
                   MOVE    1                   TO  WRK-CHK-FLG1
               END-IF
           END-IF
           IF      WRK-COM-SIPRYO2(IDX)  NOT =   ZERO
               COMPUTE WRK-SIP-SURYO2      =   WRK-COM-SURYO (IDX)
                                           /   WRK-COM-SIPRYO2(IDX)
               MOVE    WRK-SIP-SURYO2      TO  WRK-CHK-SURYO
               IF      WRK-CHK-SURYO2      =   ZERO
                   MOVE    1                   TO  WRK-CHK-FLG2
               END-IF
           END-IF
      *
           IF     (WRK-CHK-FLG1        =   1    )
              AND (WRK-CHK-FLG2        =   ZERO )
      *        膏体量１対象
               MOVE    WRK-SIP-SURYO1      TO  WRK-SIP-SURYO
               MOVE    "1"                 TO  WRK-SIP-KBN
               GO      TO      20011-SIP-SURYO-EXT
           END-IF
      *
           IF     (WRK-CHK-FLG1        =   ZERO )
              AND (WRK-CHK-FLG2        =   1    )
      *        膏体量２対象
               MOVE    WRK-SIP-SURYO2      TO  WRK-SIP-SURYO
               MOVE    "2"                 TO  WRK-SIP-KBN
               GO      TO      20011-SIP-SURYO-EXT
           END-IF
      *
           IF     (WRK-SIP-SURYO1      <=  WRK-SIP-SURYO2)
              OR  (WRK-SIP-SURYO2      =   ZERO     )
      *        膏体量１対象
               MOVE    WRK-SIP-SURYO1      TO  WRK-SIP-SURYO
               MOVE    "1"                 TO  WRK-SIP-KBN
           ELSE
      *        膏体量２対象
               MOVE    WRK-SIP-SURYO2      TO  WRK-SIP-SURYO
               MOVE    "2"                 TO  WRK-SIP-KBN
           END-IF
           .
       20011-SIP-SURYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    湿布薬換算計算数量処理
      *****************************************************************
       20012-SURYO-SIP-SEC              SECTION.
      *
           COMPUTE WRK-SIP-KEI          =  70
                                        -  WRK-SUM-SURYO
      *
           IF      WRK-COM-SIPRYO1(IDX)    =   ZERO
               MOVE    WRK-SIP-KEI         TO   WRK-COM-SIP-SURYO(IDX)
           ELSE
      *        換算あり
               IF      WRK-SIP-KBN         =   "1"
                   COMPUTE WRK-COM-SIP-SURYO(IDX)
                                           =   WRK-SIP-KEI
                                           *   WRK-COM-SIPRYO1(IDX)
               END-IF
               IF      WRK-SIP-KBN         =   "2"
                   COMPUTE WRK-COM-SIP-SURYO(IDX)
                                           =   WRK-SIP-KEI
                                           *   WRK-COM-SIPRYO2(IDX)
               END-IF
           END-IF
           .
       20012-SURYO-SIP-EXT.
           EXIT.
      *****************************************************************
      *    湿布薬コメント判定処理
      *****************************************************************
       20012-SIPCOM-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-COMMENT
           MOVE    ZERO                TO  FLG-SIP-ARI
           MOVE    ZERO                TO  IDX-STR
      *
           MOVE    ZERO                TO  FLG-SIP-ALL
           MOVE    ZERO                TO  FLG-SIP70-ALL
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )
                         OR  ((WRK-GMN-INPUTCD(IDX) =   SPACE) AND
                              (WRK-COM-INPUTCD(IDX) =   SPACE))
                         OR  ( SPA-ERRCD       NOT  =   SPACE )
             IF    (WRK-COM-YAKUHYO (IDX)   =   "1"   )
               OR  (WRK-COM-HKNCOMBI(IDX)   =   "9999")
               OR  (WRK-COM-GENTEN  (IDX)  =   "1"    )
      *H28.9   持参薬
               OR  (WRK-COM-JISANYAKU(IDX)  =   "1"   )
                 CONTINUE
             ELSE
      *
      *        剤先頭
               IF      IDX-STR             =   ZERO
                   MOVE    IDX             TO  IDX-STR
               END-IF
      *
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "6" )
                  AND (WRK-COM-SRYKBN (IDX)    =   "23"    )
                  AND (WRK-COM-SIPPUKBN (IDX)  =   "1"     )
                   MOVE    1                   TO  FLG-SIP-ARI
                   MOVE    1                   TO  FLG-SIP-ALL
               END-IF
               IF     (WRK-COM-SRYKBN (IDX)    =   "23"    )
                 AND ((WRK-COM-INPUTCD(IDX)(1:1)   =   "8" ) OR
                      (WRK-COM-INPUTCD(IDX)(1:3)   =   "001"
                                                   OR  "008"))
                   MOVE    1                   TO  FLG-COMMENT
               END-IF
      *        湿布薬枚数制限解除
               IF     (WRK-COM-INPUTCD (IDX)
                                           =   CONS-SP70-SRYCD)
                   MOVE    1               TO  FLG-SIP70-ALL
               END-IF
      *
               IF     (WRK-COM-ZAIEND (IDX)    =   "1" )
                   IF     (FLG-SIP-ARI         =   1    )
                     AND  (FLG-COMMENT         =   ZERO )
                       IF      WRK-COM-KZMFLG (IDX)    =   SPACE
                           MOVE    "K109"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR(IDX)
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                                  (IDX)
                       END-IF
                   END-IF
      *
      ****         IF     (SPA-ERRCD           =   SPACE )
      *              AND  (WRK-COM-KEI (IDX)   =   ZERO  )
      *              AND  (FLG-SIP-ARI         =   1     )
      *                診療種別チェック変換
      *                PERFORM 30001-SRYSYUKBN-SET-SEC
      *                IF     ( WRK-CHK-SRYSYUKBN       =   "231"
      *                                                 OR  "294")
      *                  AND  ( WRK-COM-SRYSYUKBN(IDX)  NOT =
      *                                           WRK-CHK-SRYSYUKBN)
      *                    MOVE    "0236"          TO  SPA-ERRCD
      *                    MOVE    "1"             TO  WRK-COM-CUR
      *                                                 (IDX-STR)
      *                END-IF
      ******       END-IF
                   MOVE    ZERO                TO  FLG-COMMENT
                   MOVE    ZERO                TO  FLG-SIP-ARI
                   MOVE    ZERO                TO  IDX-STR
               END-IF
             END-IF
           END-PERFORM
      *
      *    ７０枚以上解除あり
           IF     (FLG-SIP-ALL         =   1  )
              AND (FLG-SIP70-ALL       =   1  )
              AND (SPA-ERRCD           =   SPACE )
               PERFORM 200121-SIP70OK-CHK-SEC
           END-IF
      *
           .
       20012-SIPCOM-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    湿布薬剤７０枚以上解除あり処理
      *****************************************************************
       200121-SIP70OK-CHK-SEC              SECTION.
      *
      *
           PERFORM VARYING     IDX-KA  FROM    1   BY  1
                   UNTIL      (IDX-KA      >   10   )
                           OR (TBL-SRYKA (IDX-KA)  =   SPACE )
                           OR (SPA-ERRCD   NOT =   SPACE )
               MOVE    ZERO                TO  WRK-SUM-SURYO
               MOVE    ZERO                TO  IDX-SP702
               MOVE    ZERO                TO  IDX-SPOK
               MOVE    ZERO                TO  FLG-COM-OK
      *
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                                 ((WRK-GMN-INPUTCD(IDX) =   SPACE) AND
                                  (WRK-COM-INPUTCD(IDX) =   SPACE))
                              OR ( TBL-SIP70 (IDX-KA)   NOT =   "1" )
      *
                   IF     (WRK-COM-SRYKA (IDX) NOT =   TBL-SRYKA
                                                            (IDX-KA))
                   OR     (WRK-COM-YAKUHYO (IDX)   =   "1"  )
                   OR     (WRK-COM-HKNCOMBI(IDX)   =   "9999")
                   OR     (WRK-COM-GENTEN  (IDX)  =   "1"    )
      *H28.9       持参薬
                   OR     (WRK-COM-JISANYAKU(IDX)  =   "1"   )
                       CONTINUE
                   ELSE
      *                湿布薬判定
                       IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "6" )
                          AND (WRK-COM-SRYKBN (IDX)    =   "23"    )
                          AND (WRK-COM-SIPPUKBN (IDX)  =   "1"     )
      *                    湿布薬換算数量
                           MOVE    WRK-COM-SURYO (IDX) TO  WRK-SIP-SURYO
      *                    枚数以外は枚数へ変換
                           IF     (WRK-COM-TANICD (IDX)   NOT =  "006")
                              AND (WRK-COM-SIPRYO1(IDX)   NOT =  ZERO )
                               PERFORM 20011-SIP-SURYO-SEC
                           END-IF
      *                    ７０枚判定
                           ADD     WRK-SIP-SURYO       TO  WRK-SUM-SURYO
                           IF      WRK-SUM-SURYO       >   70
                               MOVE    IDX             TO  IDX-SP702
                           END-IF
                       END-IF
      *                コメント存在チェック
                       IF      WRK-COM-INPUTCD (IDX)
                                                   =   CONS-SP70-COMT
                           MOVE    1                   TO  FLG-COM-OK
                       END-IF
      *                解除
                       IF      WRK-COM-INPUTCD (IDX)
                                                   =   CONS-SP70-SRYCD
                           MOVE    IDX             TO  IDX-SPOK
                       END-IF
                   END-IF
               END-PERFORM
      *
               IF      IDX-SP702           >   ZERO
                   MOVE    IDX-SP702       TO  IDX
                   IF      IDX-SPOK        >   ZERO
                       MOVE    IDX-SPOK        TO  IDX
                   END-IF
                   IF      FLG-COM-OK      =   ZERO
                       IF      WRK-COM-KZMFLG (IDX)    =   SPACE
                           MOVE    "K110"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR(IDX)
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                                  (IDX)
                       END-IF
                  END-IF
               END-IF
           END-PERFORM
      *
           .
       200121-SIP70OK-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    湿布薬剤点数再計算処理
      *****************************************************************
       2009-SIP-SAIKEISAN-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-SPI-KEI
           MOVE    ZERO                TO  WRK-SPI-SAIKEI
      *
           INITIALIZE                  HZNS-SCR-REC
           MOVE    ZERO                TO  IDX-Z3
           MOVE    ZERO                TO  FLG-SAIKEISAN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                             ((WRK-GMN-INPUTCD(IDX) =   SPACE) AND
                              (WRK-COM-INPUTCD(IDX) =   SPACE))
      *
           IF     (WRK-COM-SRYKA (IDX)     =   WRK-SRYKA  )
            AND   (WRK-COM-HKNCOMBI (IDX)  NOT =   "9999" )
      *        薬評対象外
            AND   (WRK-COM-YAKUHYO  (IDX)  =   SPACE  )
      *         減点対象外
            AND   (WRK-COM-GENTEN   (IDX)  =   SPACE  )
      *H28.9    持参薬以外
            AND   (WRK-COM-JISANYAKU(IDX)  =   SPACE )
      *
               ADD     1                   TO  IDX-Z3
               MOVE    WRK-GMN-TBL(IDX)    TO  HZNS-GMN-TBL (IDX-Z3)
               IF      IDX-ST              =   ZERO
                   MOVE    IDX             TO  IDX-ST
               END-IF
      *        数量変更
               IF     (WRK-COM-SRYKBN  (IDX) =   "23"   )
                 AND  (WRK-COM-INPUTCD (IDX)(1:1)  =   "6" )
                 AND  (WRK-COM-SIPPUKBN (IDX)  =   "1"     )
                   MOVE    1                   TO  FLG-SAIKEISAN2
      *            数量換算
                   MOVE    WRK-COM-SIP-SURYO(IDX)
                                            TO  HZNS-COM-SURYO (IDX-Z3)
                   IF      WRK-COM-SIPGEN (IDX)    =   "1"
                       MOVE    1               TO  FLG-SAIKEISAN
                   END-IF
               END-IF
               MOVE    IDX                 TO  IDX-MAX
      *        院内のみ
               IF    (WRK-COM-ZAIEND (IDX)     =   "1" )
                 AND (WRK-COM-KEI (IDX)        >   ZERO)
                 AND (WRK-COM-SRYKBN(IDX)      =   "23" )
                   INITIALIZE                  ORCSC90AREA
                   IF     (WRK-COM-SRYKBN(IDX)     =   "23" )
                     AND  (FLG-SAIKEISAN2          =   1    )
                     AND  (IDX-Z3                  >   ZERO )
                       PERFORM 20091-SAIKEISAN-SEC
                   END-IF
                   IF      FLG-SAIKEISAN2      =   1
      *                保険毎にまとめる
                       PERFORM 200911-SPI-GENCHK-SEC
                       MOVE    "1"             TO  WRK-COM-SIPZAI
                                                           (IDX-ST)
                   END-IF
               END-IF
               IF    (WRK-COM-ZAIEND (IDX)     =   "1" )
                   MOVE    ZERO                TO  IDX-Z3
                   MOVE    ZERO                TO  IDX-ST
                   MOVE    ZERO                TO  FLG-SAIKEISAN
                   MOVE    ZERO                TO  FLG-SAIKEISAN2
                   INITIALIZE                  HZNS-SCR-REC
               END-IF
      *
               END-IF
           END-PERFORM
      *
      *    保険毎に減算
           IF      WRK-SPI-KEI     NOT =   WRK-SPI-SAIKEI
               PERFORM 200912-SPI-GENHEN-SEC
      *        (減）挿入
               PERFORM 20092-SPGENCOM-HEN-SEC
           END-IF
           .
       2009-SIP-SAIKEISAN-EXT.
           EXIT.
      *****************************************************************
      *    湿布薬剤　外用集計処理
      *****************************************************************
       200911-SPI-GENCHK-SEC         SECTION.
      *
      *    保険毎にまとめる
           PERFORM VARYING   IDXW      FROM    1   BY  1
                   UNTIL    (IDXW      >   15  )
               IF      WRK-HKNKEISP-COMBI (IDXW) =   SPACE
                   MOVE    WRK-COM-HKNCOMBI (IDX)
                                               TO  WRK-HKNKEISP-COMBI
                                                      (IDXW)
                   MOVE    WRK-COM-CHIKENKBN(IDX)
                                               TO  WRK-HKNKEISP-CHIKEN
                                                    (IDXW)
               END-IF
               IF      WRK-HKNKEISP-COMBI (IDXW)
                                       =   WRK-COM-HKNCOMBI (IDX)
                   MOVE    IDXW            TO  WRK-CNT-HKNSP
                   ADD     WRK-COM-KEI (IDX)   TO  WRK-SPI-KEI
                   ADD     WRK-COM-KEI (IDX)   TO  WRK-SPI-KEI-HKN
                                                       (WRK-CNT-HKNSP)
      *
                   ADD     LNK-SC90-YKZTEN     TO  WRK-SPI-SAIKEI
                   ADD     LNK-SC90-YKZTEN     TO  WRK-SPI-SAIKEI-HKN
                                                       (WRK-CNT-HKNSP)
                   MOVE    15              TO  IDXW
               END-IF
           END-PERFORM
      *
           .
       200911-SPI-GENCHK-EXT.
           EXIT.
      *****************************************************************
      *    湿布薬剤減算　　外用編集処理
      *****************************************************************
       200912-SPI-GENHEN-SEC         SECTION.
      *
      *    薬剤料逓減
           COMPUTE WRK-SPI-GENKEI      =   WRK-SPI-KEI
                                       -   WRK-SPI-SAIKEI
      *
      *    複数保険最終行
           MOVE    ZERO                TO  WRK-CNT-HKNSP
           PERFORM VARYING IDX-GEN     FROM    1   BY  1
                   UNTIL  (IDX-GEN         >   15 )
               IF      WRK-HKNKEISP-COMBI (IDX-GEN)
                                       NOT =   SPACE
                   MOVE    IDX-GEN         TO  WRK-CNT-HKNSP
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX-GEN     FROM    1   BY  1
                   UNTIL   IDX-GEN     >   WRK-CNT-HKNSP
               IF      WRK-SPI-KEI-HKN (IDX-GEN)
                                       NOT =   WRK-SPI-SAIKEI-HKN
                                                         (IDX-GEN)
                   COMPUTE WRK-SANTENSU    =   WRK-SPI-KEI-HKN
                                                         (IDX-GEN)
                                           -   WRK-SPI-SAIKEI-HKN
                                                         (IDX-GEN)
      *            減点
                   MOVE    WRK-SANTENSU    TO  WRK-SPI-GENTEN
                                                         (IDX-GEN)
      *
                   MOVE    CONS-SPGEN-SRYCD    TO  WRK-SRYCD
                   MOVE    SPACE               TO  WRK-SRYCD2
                   PERFORM 3002-KASAN-SYORI-SEC
                   IF      FLG-ERR             =   ZERO
                   MOVE    "230"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "23"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
                   MOVE    WRK-HKNKEISP-COMBI (IDX-GEN)
                                           TO  LNK-ORCSC-HKNCOMBI
                                                         (02 IDXS)
                   MOVE    WRK-HKNKEISP-CHIKEN(IDX-GEN)
                                           TO  LNK-ORCSC-CHIKENKBN
                                                         (02 IDXS)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       200912-SPI-GENHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    湿布薬換算数量点数再計算処理
      *****************************************************************
       20091-SAIKEISAN-SEC              SECTION.
      *
           INITIALIZE                  ORCSC90AREA
           MOVE    "2"                 TO  LNK-SC90-KBN
           MOVE    ZERO                TO  LNK-SC90-IDX
           MOVE    1                   TO  LNK-SC90-IDX-STR
           MOVE    IDX-Z3              TO  LNK-SC90-IDX-END
           CALL    "ORCSC90TEN2"       USING
                                       ORCSC90AREA
                                       HZNS-SCR-REC
           .
       20091-SAIKEISAN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    湿布薬（減）挿入処理
      *****************************************************************
       20092-SPGENCOM-HEN-SEC              SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                             ((WRK-GMN-INPUTCD(IDX) =   SPACE) AND
                              (WRK-COM-INPUTCD(IDX) =   SPACE))
      *
           IF     (WRK-COM-SRYKA (IDX)     =   WRK-SRYKA  )
            AND   (WRK-COM-HKNCOMBI (IDX)  NOT =   "9999" )
      *        薬評対象外
            AND   (WRK-COM-YAKUHYO  (IDX)  =   SPACE  )
      *ver.4.7  減点対象外
            AND   (WRK-COM-GENTEN   (IDX)  =   SPACE  )
      *H28.9    持参薬以外
            AND   (WRK-COM-JISANYAKU(IDX)  =   SPACE )
      *
            AND   (WRK-COM-SIPZAI (IDX)    =   "1"    )
               IF      WRK-GMN-INPUTCD(IDX)(1:1)   =   "."
      *            約束セットの時、約束セットの後に追加
                   IF      WRK-GMN-INPUTCD(IDX + 1)(1:1)   =   "S"
                       COMPUTE IDX-Z           =   IDX     +   2
                   ELSE
                       COMPUTE IDX-Z           =   IDX     +   1
                   END-IF
               ELSE
                   MOVE    IDX             TO  IDX-Z
               END-IF
               MOVE    CONS-SPCM-SRYCD     TO  WRK-SRYCD
               PERFORM 30011-GENTUIKA-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    "1"             TO  WRK-COM-CUR (IDX)
               ELSE
                   ADD     1               TO  IDX
               END-IF
           END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  WRK-GMN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                             ((WRK-GMN-INPUTCD(IDX) =   SPACE ) AND
                              (WRK-COM-INPUTCD(IDX) =   SPACE ))
               ADD     1               TO  WRK-GMN-MAX
           END-PERFORM
           .
       20092-SPGENCOM-HEN-EXT.
           EXIT.
      *
      *H30.4
      *****************************************************************
      *    分割調剤コードチェック処理
      *****************************************************************
       2004-BUNKATUCHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDX-BNKT
           MOVE    ZERO                TO  FLG-BNKT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                             ((WRK-GMN-INPUTCD(IDX) =   SPACE) AND
                              (WRK-COM-INPUTCD(IDX) =   SPACE))
                          OR  (SPA-ERRCD       NOT =   SPACE  )
      *        診療種別区分変換
               PERFORM 30001-SRYSYUKBN-SET-SEC
      *
      *        分割調剤
               IF      WRK-COM-INPUTCD (IDX)   =   "099208101"
                   IF      WRK-CHK-SRYSYUKBN   =   "212"
                                               OR  "292"
                                               OR  "222"
                                               OR  "232"
                                               OR  "295"
                                               OR  "298"
      *                院内のみ
                       IF      IDX-BNKT        =   ZERO
                           MOVE    IDX             TO  IDX-BNKT
                       ELSE
      *                    １剤１件
                           MOVE    "0282"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                          (IDX)
                       END-IF
                   ELSE
      *                院外はエラー
                       MOVE    "0281"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR
                                                          (IDX)
                   END-IF
               END-IF
      *
      *        薬評対象外
               IF    (WRK-COM-YAKUHYO  (IDX)  =   SPACE  )
      *         減点対象外
               AND   (WRK-COM-GENTEN   (IDX)  =   SPACE  )
      *         持参薬
               AND   (WRK-COM-JISANYAKU(IDX)  =   SPACE  )
      *            剤内容があること
                   IF     (WRK-COM-INPUTCD(IDX)(1:1)  =   "6"
                                                      OR  "7"  )
                     OR   (WRK-COM-INPUTCD(IDX)(1:3)  =   "059")
                       MOVE    1                TO  FLG-BNKT
                   END-IF
               END-IF
      *
               IF     (WRK-COM-ZAIEND (IDX)    =   "1" )
                AND   (SPA-ERRCD               =   SPACE )
      *            回数入力あり、または、登録時
                   IF     ((WRK-COM-KAIFLG (IDX)   =   "1"  ) OR
                           (LNK-ORCSC-KBN2         =   "1"  ))
                     AND   (IDX-BNKT               >   ZERO )
                       PERFORM 20041-BUNKATU-SURYO-SEC
                   END-IF
                   MOVE    ZERO            TO  IDX-BNKT
                   MOVE    ZERO            TO  FLG-BNKT
               END-IF
           END-PERFORM
           .
       2004-BUNKATUCHK-EXT.
           EXIT.
      *****************************************************************
      *    分割調剤コード数量チェック処理
      *****************************************************************
       20041-BUNKATU-SURYO-SEC              SECTION.
      *
      *    剤内容なし
           IF      FLG-BNKT            =   ZERO
               MOVE    "0286"              TO  SPA-ERRCD
           END-IF
      *
      *H30.4.23 削除
      *    ３０日以上の投薬のみ
      *    IF      WRK-COM-KAISU(IDX)  <   30
      *        MOVE    "0283"              TO  SPA-ERRCD
      **** END-IF
      *    回数合計
           MOVE    ZERO                TO  WRK-BUN-KAISU
           PERFORM VARYING     IDX-BC  FROM    1   BY  1
                   UNTIL      (IDX-BC      >   5   )
                         OR   (SPA-ERRCD   NOT =   SPACE )
               IF      WRK-COM-ATAI (IDX-BNKT IDX-BC)  =   SPACE
                   CONTINUE
               ELSE
                   IF      IDX-BC              >   3
                       MOVE    "0284"          TO  SPA-ERRCD
                   ELSE
                       INITIALIZE                      ORCSNUMAREA
                       MOVE    WRK-COM-ATAI (IDX-BNKT IDX-BC)
                                                   TO  SNUM-INX
                       CALL    "ORCSNUM"           USING
                                                   ORCSNUMAREA
                       IF     (SNUM-RC         NOT =   ZERO  )
                        OR    (SNUM-MINKBN     NOT =   SPACE )
                        OR    (SNUM-SYOKBN     NOT =   SPACE )
                           MOVE    "0284"            TO  SPA-ERRCD
                       ELSE
                           ADD     SNUM-OUTNUM       TO  WRK-BUN-KAISU
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
               IF      WRK-BUN-KAISU   NOT =   WRK-COM-KAISU(IDX)
                   MOVE    "0285"            TO  SPA-ERRCD
               END-IF
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1"             TO  WRK-COM-CUR (IDX-BNKT)
           END-IF
           .
       20041-BUNKATU-SURYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動算定処理
      *****************************************************************
       300-AUTO-SYORI-SEC              SECTION.
      *
      *    包括まとめ入力は以下対象外
           IF      SPA-HKNCOMBINUM     =   "9999"
               GO      TO      300-AUTO-SYORI-EXT
           END-IF
      *
           IF      LNK-ORCSC-PG        =   "K02"
                                       OR  "KT01"
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *        他保険で算定済み
               INITIALIZE                  SPA-SYOHOU-ZUMI
           END-IF
      *
           INITIALIZE                      WRK-CONTCHK-TBL
           MOVE    ZERO                TO  WRK-KAISU
      *H26.4
      *    地域包括診療加算有無
           MOVE    ZERO                TO  FLG-CHKCARKSN
           MOVE    ZERO                TO  FLG-DEKIDAKA
           IF     (SPA-CHKHKT-ARI      =   1   )
              OR  (SPA-CHKHKT-ARI2     =   1   )
      *        地域包括診療料算定　有
               MOVE    2                   TO  FLG-CHKCARKSN
           END-IF
      *!
      *H28.9
      *    認知症地域包括診療加算有無
           MOVE    ZERO                TO  FLG-NINCHIKSN
           IF     (SPA-NNCHKT-ARI      =   1   )
              OR  (SPA-NNCHKT-ARI2     =   1   )
               MOVE    2                   TO  FLG-NINCHIKSN
           END-IF
      *
      *H30.4
      *    オンライン診療料が同時
           IF      SPA-ONLINE-ARI      =   1
               MOVE    1               TO  FLG-ONLNE-ARI
           ELSE
               MOVE    ZERO            TO  FLG-ONLNE-ARI
           END-IF
      *
      *H31.3
      *    向精神薬長期投与減額
           MOVE    ZERO                TO  SPA-FUANCHOKI
           MOVE    ZERO                TO  FLG-PSYCHOKI-OK
           MOVE    ZERO                TO  IDX-CHOGEN
      *
      *    継続の時、算定分の編集
      *!!  IF     (LNK-ORCSC-CONTKBN    =   "1"  OR  "2" )
      *        入院の時
      *!!      OR (SPA-NYUGAIKBN        =   "1"  )
               PERFORM 30010-JYURRK-SYORI-SEC
      *!!  END-IF
      *
      *    科毎
           INITIALIZE                  TBL-SRYKA-AREA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                             ((WRK-GMN-INPUTCD(IDX) =   SPACE) AND
                              (WRK-COM-INPUTCD(IDX) =   SPACE))
      *
             IF   (WRK-COM-YAKUHYO (IDX)       =   "1"  )  OR
                  (WRK-GMN-INPUTCD (IDX)(1:1)  =   "."  )  OR
                  (WRK-COM-HKNCOMBI (IDX)      =   "9999")
      *ver.4.7  減点対象外
             OR   (WRK-COM-GENTEN   (IDX)      =   "1"    )
      *H28.9    持参薬
             OR   (WRK-COM-JISANYAKU(IDX)      =   "1"    )
               CONTINUE
             ELSE
      *H26.4
      *        地域包括診療加算
               IF      WRK-COM-INPUTCD(IDX)    =   CONS-CHKCARKSN1
                                               OR  CONS-CHKCARKSN2
                   MOVE    1                   TO  FLG-CHKCARKSN
               END-IF
      *!
      *H28.9
      *        認知症地域包括診療加算
               IF     (WRK-COM-INPUTCD(IDX)    =   CONS-NNCHIKSN1
                                               OR  CONS-NNCHIKSN2 )
                   MOVE    1                   TO  FLG-NINCHIKSN
                   MOVE    IDX                 TO  IDX-NINCHI
               END-IF
      *!
      *        出来高（会計）
               IF      WRK-COM-INPUTCD (IDX)   =   "099999905"
                   MOVE    1                   TO  FLG-DEKIDAKA
               END-IF
      *
               IF     (WRK-COM-SRYKBN(IDX)(1:1)    =   "2" )  OR
                      (WRK-COM-SRYSYUKBN(IDX)      =   "820")
                  OR  (WRK-COM-SRYSYUKBN(IDX)      =   "980")
                   IF      WRK-COM-SRYKA (IDX)     =   SPACE
                       MOVE    SPA-SRYKA      TO  WRK-COM-SRYKA(IDX)
                   END-IF
                   IF      WRK-COM-HKNCOMBI (IDX)  =   SPACE
                       MOVE    SPA-HKNCOMBINUM
                                              TO  WRK-COM-HKNCOMBI(IDX)
                   END-IF
      *@
                   PERFORM VARYING     IDX-KA  FROM    1   BY  1
                           UNTIL       IDX-KA  >   10
                       IF      TBL-SRYKA (IDX-KA)  =   SPACE
                           MOVE    WRK-COM-SRYKA(IDX)  TO  TBL-SRYKA
                                                              (IDX-KA)
                           MOVE    WRK-COM-HKNCOMBI(IDX)
                                                       TO  TBL-HKNCOMBI
                                                              (IDX-KA)
                           MOVE    WRK-COM-CHIKENKBN(IDX)
                                                       TO  TBL-CHIKENKBN
                                                              (IDX-KA)
                       END-IF
                       IF      TBL-SRYKA (IDX-KA)  =   WRK-COM-SRYKA
                                                               (IDX)
      *H27.4           診療科毎に後発変更不可処方毎の設定
                           EVALUATE    WRK-COM-INPUTCD (IDX)
                           WHEN   "099209910"
                               MOVE    "1"         TO  TBL-GENECHG
                                                              (IDX-KA)
                           WHEN   "099209911"
                               MOVE    "2"         TO  TBL-GENECHG
                                                              (IDX-KA)
      *H28.4               湿布薬枚数制限解除
                           WHEN   CONS-SP70-SRYCD
                               MOVE    "1"         TO  TBL-SIP70
                                                              (IDX-KA)
                           END-EVALUATE
      *H27.4
                           MOVE    10              TO  IDX-KA
                       END-IF
                   END-PERFORM
               END-IF
      *
      *H31.3
      *        向精神薬長期投与減算対象
               IF      WRK-COM-INPUTCD(IDX)    =   "099200301"
                   MOVE    1                   TO  SPA-FUANCHOKI
                   MOVE    IDX                 TO  IDX-CHOGEN
               END-IF
      *H31.3
      *        向精神薬長期投与対象
               IF     (WRK-COM-SRYKBN(IDX)(1:1)    =   "2" )
                AND   (WRK-COM-INPUTCD(IDX)(1:1)   =   "6" )
                AND   (SPA-NYUGAIKBN           =   "2"     )
      *            向精神薬長期処方対象薬剤
                   IF      WRK-COM-PSYCHO2 (IDX)   =   "1"
                       MOVE    1               TO  FLG-PSYCHOKI-OK
                   END-IF
               END-IF
      *
             END-IF
      *H27.4
      *        ３０日以上対象区分クリア
               MOVE    SPACE           TO  WRK-COM-YOBI2  (IDX)
           END-PERFORM
      *
      *H31.3
      *    向精神薬長期処方減算チェック
           IF    (SPA-FUANCHOKI       =   1    )
             AND (SPA-NYUGAIKBN       =   "2"  )
               IF      FLG-PSYCHOKI-OK     =   ZERO
                   IF     (WRK-COM-KZMFLG (IDX-CHOGEN) =   SPACE)
                     AND  (LNK-ORCSC-KEI-IDX       =   ZERO )
                       MOVE    "K229"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR
                                                          (IDX-CHOGEN)
                       MOVE    "1"             TO  WRK-COM-KZMFLG
                                                          (IDX-CHOGEN)
                       MOVE    IDX-CHOGEN      TO  LNK-ORCSC-KEI-IDX
                   END-IF
               END-IF
           END-IF
      *
      *
      *H26.4
      *    地域包括診療料算定有で、出来高（会計）の時、包括外とする
           IF      FLG-CHKCARKSN       =   2
               IF      FLG-DEKIDAKA        =   1
                   MOVE    ZERO            TO  FLG-CHKCARKSN
               ELSE
                   MOVE    1               TO  FLG-CHKCARKSN
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  IDXS
           MOVE    SPACE               TO  LNK-ORCSC-KIDCD
      *H28.6   精神用
           MOVE    ZERO                TO  IDX-80
           INITIALIZE                  SPA-PSYCHO-80-AREA
      *
           PERFORM VARYING     IDX-KA  FROM    1   BY  1
                   UNTIL      (IDX-KA      >   10   )  OR
                              (TBL-SRYKA (IDX-KA)  =   SPACE)  OR
                              (LNK-ORCSC-KIDCD NOT =   SPACE)
                          OR  (SPA-ERRCD       NOT =   SPACE)
               MOVE    TBL-SRYKA (IDX-KA)      TO  WRK-SRYKA
               MOVE    TBL-HKNCOMBI(IDX-KA)    TO  WRK-HKNCOMBI
               MOVE    TBL-CHIKENKBN(IDX-KA)   TO  WRK-CHIKENKBN
               MOVE    TBL-GENECHG  (IDX-KA)   TO  WRK-GENECHG-KBN
      *
               MOVE    ZERO            TO  FLG-TOKUTEI
               PERFORM VARYING     IDX-KA2 FROM    1   BY  1
                       UNTIL      (IDX-KA2     >   10   )  OR
                                  (SPA-TOKU-SRYKA (IDX-KA2)
                                               =   SPACE   )
                   IF      SPA-TOKU-SRYKA (IDX-KA2)    =   WRK-SRYKA
                       IF     (SPA-TOKU-FLG(IDX-KA2)   =   05 )
                          OR  (SPA-TOKU05-FLG(IDX-KA2) =   05 )
                           MOVE    1           TO  FLG-TOKUTEI
                       END-IF
                       MOVE    10              TO  IDX-KA2
                   END-IF
               END-PERFORM
               IF      (SPA-TOKUTEIFLG     NOT =   ZERO )  OR
                       (SPA-CHOKITOFLG     NOT =   ZERO )
                   IF      (SPA-TOKUTEI-SRYKA  NOT =   SPACE  )  AND
                           (SPA-TOKUTEI-SRYKA  NOT =   WRK-SRYKA)
                       MOVE    ZERO        TO  FLG-TOKUTEI
                   END-IF
               END-IF
               PERFORM 3000-JIDOU-SYORI-SEC
           END-PERFORM
      *
           IF      LNK-ORCSC-PG        =   "K02"
                                       OR  "KT01"
               MOVE    WRK-HENSYU-MEISYO   TO  SPA-SYOHO-MEI
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           END-IF
           .
       300-AUTO-SYORI-EXT.
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
           MOVE    ZERO                TO  FLG-MADOKU
           MOVE    ZERO                TO  FLG-KOUHATU
           MOVE    ZERO                TO  FLG-KOUHATU-HEN1
           MOVE    ZERO                TO  FLG-KOUHATU-HEN2
           MOVE    ZERO                TO  IDX-SYOHOU
           MOVE    ZERO                TO  FLG-MADOKU-ARI
           MOVE    ZERO                TO  FLG-SINSATU
           MOVE    ZERO                TO  FLG-CHOUKI 
           MOVE    ZERO                TO  FLG-CHOUKI2
           MOVE    ZERO                TO  FLG-CHOUKI3
      *
           MOVE    ZERO                TO  FLG-TOKUSYO1
           MOVE    ZERO                TO  FLG-TOKUSYO2
           MOVE    ZERO                TO  FLG-CHOKIKA1
           MOVE    ZERO                TO  FLG-CHOKIKA2
      *
           MOVE    ZERO                TO  FLG-ZAIEND
           MOVE    ZERO                TO  IDX-CHOKI
      *    抗悪性腫瘍剤処方管理加算
           MOVE    ZERO                TO  FLG-AKUSKSN1
           MOVE    ZERO                TO  FLG-AKUSKSN2
           MOVE    ZERO                TO  FLG-IPPANKSN
      *
           MOVE    ZERO                TO  FLG-GENERIC-OK
      *H29.4
           MOVE    ZERO                TO  FLG-GENERIC-OK12
      *H28.4
           MOVE    ZERO                TO  FLG-GENERIC-NO
           MOVE    ZERO                TO  CNT-GENERIC
      *#######
           MOVE    ZERO                TO  FLG-GENERIC-NO2
           MOVE    ZERO                TO  CNT-GENERIC2
           MOVE    ZERO                TO  CNT-GENERIC3
      *#######
      *H29.3
           MOVE    ZERO                TO  IDX-SYOHOU2
           MOVE    ZERO                TO  IDX-G2
           MOVE    ZERO                TO  FLG-SYOHOU2
           MOVE    ZERO                TO  FLG-GENKSN2
           MOVE    ZERO                TO  FLG-SYOHOUMD2
           MOVE    ZERO                TO  IDX-SYOHOUMD2
      *
           INITIALIZE                  WRK-HKNCOMBI-AREA
      *
           INITIALIZE                  WRK-ZAIKEI-AREA
           INITIALIZE                  TBL-NYUINDAY-AREA
           MOVE    SPACE               TO  WRK-HENSYU-MEISYO
      *
           INITIALIZE                  WRK-HKNKEI-G
      *ver.4.7
           INITIALIZE                  TBL-YOUHO-HEN-AREA
      *    サンプル薬投薬
           MOVE    ZERO                TO  FLG-YAKZAI-ALL
           MOVE    ZERO                TO  FLG-SANPURU
      *H26.4
      *    うがい薬チェック
           MOVE    ZERO                TO  WRK-UGAI-YAK
           MOVE    ZERO                TO  WRK-UGAI-IGAI
           MOVE    ZERO                TO  IDX-UGAI
      *
      *H26.10
      *    向精神薬多剤投与チェック
           INITIALIZE                  ORCSPSYCHOAREA
           MOVE    ZERO                TO  IDX-PS
           MOVE    ZERO                TO  FLG-TAZAI1
           MOVE    ZERO                TO  FLG-TAZAI2
           MOVE    ZERO                TO  FLG-TAZAI-ARI
           MOVE    ZERO                TO  IDX-Z2
      *H27.4
           INITIALIZE                  WRK-HKNKEI30-G
           MOVE    ZERO                TO  FLG-GEN30-K1
           MOVE    ZERO                TO  FLG-GEN30-K2
           MOVE    ZERO                TO  FLG-GEN30
           MOVE    ZERO                TO  FLG-GEN30-N
           MOVE    ZERO                TO  FLG-GEN30-G
           MOVE    ZERO                TO  FLG-GEN30-PSY
           MOVE    ZERO                TO  FLG-GEN30-7GEN
           MOVE    ZERO                TO  WRK-GEN30-STR
      *H28.4
           MOVE    ZERO                TO  FLG-SIP70-ARI
      *H30.4
      *    皮膚保湿薬チェック
           MOVE    ZERO                TO  WRK-HIFU-YAK
           MOVE    ZERO                TO  IDX-HIFU
      *H31.3
      *    向精神薬長期投与
           MOVE    ZERO                TO  FLG-PSYCHOKI-OK
           MOVE    ZERO                TO  FLG-CHOKIGEN
      *R01.5
      *    一般名加算チェック
           INITIALIZE                  TBL-GENERIC-CHEK-AREA
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                             ((WRK-GMN-INPUTCD(IDX) =   SPACE) AND
                              (WRK-COM-INPUTCD(IDX) =   SPACE))
      *
           IF     (WRK-COM-SRYKA (IDX)     =   WRK-SRYKA  )
            AND   (WRK-COM-HKNCOMBI (IDX)  NOT =   "9999" )
      *        薬評対象外
            AND   (WRK-COM-YAKUHYO  (IDX)  =   SPACE  )
      *ver.4.7  減点対象外
            AND   (WRK-COM-GENTEN   (IDX)  =   SPACE  )
      *H28.9    持参薬
            AND   (WRK-COM-JISANYAKU(IDX)  =   SPACE  )
      *
      *        診療種別区分変換
               PERFORM 30001-SRYSYUKBN-SET-SEC
      *        投薬の時、薬剤コードがあること
               IF     (WRK-CHK-SRYSYUKBN(1:2)  =   "21"
                                               OR  "22"
                                               OR  "23"
                                               OR  "29" )  AND
                       (FLG-ZAIEND             =   ZERO )
                   PERFORM 30001-TOUYAKU-CHK-SEC
                   MOVE    1                   TO  FLG-ZAIEND
               END-IF
      *
               EVALUATE    WRK-CHK-SRYSYUKBN
                   WHEN    "211"
                   WHEN    "291"
                       MOVE     1              TO  FLG-NAIYAK
                       IF      WRK-NAI-HKNCOMBI    =   SPACE
                           MOVE    WRK-COM-HKNCOMBI (IDX)
                                                   TO  WRK-NAI-HKNCOMBI
                           MOVE    WRK-COM-CHIKENKBN(IDX)
                                                   TO  WRK-NAI-CHIKENKBN
                       END-IF
                   WHEN    "212"
                   WHEN    "292"
                       MOVE     1              TO  FLG-NAIYAK2
                   WHEN    "221"
                   WHEN    "297"
                       MOVE     1              TO  FLG-TONPUK
                       IF      WRK-NAI-HKNCOMBI    =   SPACE
                           MOVE    WRK-COM-HKNCOMBI (IDX)
                                                   TO  WRK-NAI-HKNCOMBI
                           MOVE    WRK-COM-CHIKENKBN(IDX)
                                                   TO  WRK-NAI-CHIKENKBN
                       END-IF
                   WHEN    "222"
                   WHEN    "298"
                       MOVE     1              TO  FLG-TONPUK2
                   WHEN    "231"
                   WHEN    "294"
                       MOVE     1              TO  FLG-GAIYAK
                       IF      WRK-GAI-HKNCOMBI    =   SPACE
                           MOVE    WRK-COM-HKNCOMBI (IDX)
                                                   TO  WRK-GAI-HKNCOMBI
                           MOVE    WRK-COM-CHIKENKBN(IDX)
                                                   TO  WRK-GAI-CHIKENKBN
                       END-IF
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
      *            処方内容チェク
                   PERFORM 3000-SYOHOU-CHK-SEC
               END-IF
      *
      *        処方せん料算定チェック
               EVALUATE    WRK-COM-INPUTCD(IDX)
      *            その他
                   WHEN    "120002910"
      *            ７種類以上 
                   WHEN    "120002710"
      *H26.10
      *            向精神薬多剤投与処方せん料
                   WHEN    CONS-TAZAI-SYOSEN
                       IF      IDX-SYOHOU      NOT =   ZERO
                           MOVE    "1021"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR (IDX)
                       ELSE
                           MOVE    IDX             TO  IDX-SYOHOU
                           MOVE    WRK-COM-HKNCOMBI (IDX)
                                                   TO  WRK-SYO-HKNCOMBI
                           MOVE    WRK-COM-CHIKENKBN(IDX)
                                                   TO  WRK-SYO-CHIKENKBN
                       END-IF
               END-EVALUATE
      *        後発医薬品への変更チェック
               EVALUATE    WRK-COM-INPUTCD(IDX)
                   WHEN    "099209902"
                       MOVE    1               TO  FLG-KOUHATU-HEN1
               END-EVALUATE
      *
      *        特定疾患処方箋
               IF      WRK-COM-INPUTCD (IDX)   =   "120002570"
                   MOVE    1                   TO  FLG-TOKUSYO1
                   MOVE    IDX                 TO  IDX-T1
               END-IF
      *        特定疾患処方料
               IF      WRK-COM-INPUTCD (IDX)   =   "120002270"
                   MOVE    1                   TO  FLG-TOKUSYO2
                   MOVE    IDX                 TO  IDX-T2
               END-IF
      *
      *        長期投与加算（処方）
               IF      WRK-COM-INPUTCD (IDX)   =   "120003170"
                   MOVE    1                   TO  FLG-CHOKIKA2
                   MOVE    IDX                 TO  IDX-T2
               END-IF
      *        長期投与加算（処方せん）
               IF      WRK-COM-INPUTCD (IDX)   =   "120003270"
                   MOVE    1                   TO  FLG-CHOKIKA1
                   MOVE    IDX                 TO  IDX-T1
               END-IF
      *        抗悪性腫瘍剤処方管理加算（処方）
               IF      WRK-COM-INPUTCD (IDX)   =   "120003370"
                   MOVE    1                   TO  FLG-AKUSKSN2
                   MOVE    IDX                 TO  IDX-A2
               END-IF
      *        抗悪性腫瘍剤処方管理加算（処方せん）
               IF      WRK-COM-INPUTCD (IDX)   =   "120003470"
                   MOVE    1                   TO  FLG-AKUSKSN1
                   MOVE    IDX                 TO  IDX-A1
               END-IF
      *H24.4　改正
      *        一般名処方加算（処方せん料）
               IF      WRK-COM-INPUTCD (IDX)   =   "120003570"
                                               OR  "120004270"
                   IF      FLG-IPPANKSN        =   ZERO
                       MOVE    1                   TO  FLG-IPPANKSN
                       MOVE    IDX                 TO  IDX-IP
                   ELSE
                       MOVE    "1021"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX)
                   END-IF
               END-IF
      *
      *        投薬で入院回数の時、回数集計
               IF     (SPA-NYUGAIKBN               =   "1" )  AND
                      (WRK-COM-SRYKBN (IDX)(1:1)   =   "2" )  AND
                      ((WRK-GMN-INPUTCD(IDX)(1:1)  =   "*" )  OR
                       (WRK-COM-KAIFLG (IDX)       =   "1" )  OR
                       (WRK-COM-ZAIEND (IDX)       =   "1" ))
      *         薬剤のない剤は対象外
                AND   (WRK-CHK-SRYSYUKBN       NOT =   "990" )
                   PERFORM 3009-NYUINKAISU-SEC
               END-IF
               IF      WRK-COM-ZAIEND (IDX)    =   "1"
                   MOVE    ZERO                TO  FLG-MADOKU-ARI
                   MOVE    ZERO                TO  FLG-ZAIEND
               END-IF
      *
      *        他保険にて算定の判定
               IF     (WRK-COM-SRYKBN (IDX)    =   "11"  OR
                                                   "12"  )
                   MOVE    1                   TO  FLG-SINSATU
               END-IF
      *        調基の判定
               IF     (WRK-COM-INPUTCD(IDX)    =   "120001810" OR
                                                   "120001710"  )
                   MOVE    IDX                 TO  IDX-CHOKI
               END-IF
      *
      *H27.4
      *        低紹介率病院　処方せん料加算
               IF      WRK-COM-INPUTCD (IDX)   =   CONS-TEIHP-SYOSEN
                   MOVE    1                   TO  FLG-GEN30-K1
                   MOVE    IDX                 TO  IDX-GEN30
               END-IF
      *        低紹介率病院　処方料加算
               IF      WRK-COM-INPUTCD (IDX)   =   CONS-TEIHP-SYOHOU
                   MOVE    1                   TO  FLG-GEN30-K2
               END-IF
      *
      *H26.10
      *        向精神薬多剤投与処方せん料
               IF      WRK-COM-INPUTCD (IDX)   =   CONS-TAZAI-SYOSEN
                   MOVE    1                   TO  FLG-TAZAI1
               END-IF
      *        向精神薬多剤投与処方料
               IF      WRK-COM-INPUTCD (IDX)   =   CONS-TAZAI-SYOHOU
                   MOVE    1                   TO  FLG-TAZAI2
                   MOVE    IDX                 TO  IDX-Z2
               END-IF
      *#######
      *H29.3
      *        処方料入力チェック
               EVALUATE    WRK-COM-INPUTCD (IDX)
      *            処方料
                   WHEN    "120001210"
                   WHEN    "120002610"
                   WHEN    CONS-TAZAI-SYOHOU
                       IF     (IDX-SYOHOU      NOT =   ZERO )
                         OR   (IDX-SYOHOU2     NOT =   ZERO )
      *                    処方料・処方箋料は１件
                           MOVE    "1021"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR(IDX)
                       ELSE
                           MOVE    IDX             TO  IDX-SYOHOU2
                           MOVE    1               TO  FLG-SYOHOU2
                       END-IF
                   WHEN    CONS-GEN-KSN1
                   WHEN    CONS-GEN-KSN2
                   WHEN    CONS-GEN-KSN3
      *            外来後発医薬品使用体制加算
                       MOVE    1                   TO  FLG-GENKSN2
                       MOVE    IDX                 TO  IDX-G2
                   WHEN    "120001310"
      *            麻毒処方料麻毒加算
                       MOVE    1                   TO  FLG-SYOHOUMD2
                       MOVE    IDX                 TO  IDX-SYOHOUMD2
                   WHEN    CONS-TEIHP-SYOHOU
      *            処方料減算（低紹介率）
                       MOVE    1                   TO  FLG-SGEM
                       MOVE    IDX                 TO  IDX-SGEM
      *H30.3
                   WHEN    CONS-CHOSEIKSN2
      *            向精神薬調整連携加算（処方料）
                       MOVE    1                   TO  FLG-RENKSN2
                       MOVE    IDX                 TO  IDX-R2
                   WHEN    CONS-CHOSEIKSN1
      *            向精神薬調整連携加算（処方せん料）
                       MOVE    1                   TO  FLG-RENKSN1
                       MOVE    IDX                 TO  IDX-R1
               END-EVALUATE
      *#######
      *
      *        抗精神薬剤チェック編集
               IF     (WRK-CHK-SRYSYUKBN(1:2)  =   "21"
                                               OR  "22"
                                               OR  "23"
                                               OR  "29" )
                AND   (WRK-COM-INPUTCD (IDX)(1:1)  =   "6")
                AND   (SPA-NYUGAIKBN           =   "2"  )
                   PERFORM 30002-PSYCHO-HEN-SEC
               END-IF
      *
      *H31.4
      *        向精神薬長期投与対象
               IF     (WRK-CHK-SRYSYUKBN(1:2)  =   "21"
                                               OR  "22"
                                               OR  "23"
                                               OR  "29" )
                AND   (WRK-COM-INPUTCD (IDX)(1:1)  =   "6")
                AND   (SPA-NYUGAIKBN       =   "2"  )
                AND   (SPA-FUANCHOKI       =   1    )
      *            向精神薬長期処方対象薬剤
                   IF      WRK-COM-PSYCHO2 (IDX)   =   "1"
                       MOVE    1               TO  FLG-PSYCHOKI-OK
                   END-IF
               END-IF
      *
      *
      *H28.4
      *        湿布薬枚数チェック
               IF    (SPA-NYUGAIKBN        =   "2"  )
                 AND (WRK-CHK-SRYSYUKBN(1:2)   =   "23")
                 AND (WRK-COM-INPUTCD (IDX)(1:1)   =   "6")
                 AND (WRK-COM-SIPGEN (IDX)     =   "1"   )
                   MOVE    1               TO  FLG-SIP70-ARI
               END-IF
      *
               END-IF
      *H27.4
      *        剤開始位置設定
               IF      WRK-COM-ZAIEND (IDX)   =   "1"
                   MOVE    IDX                 TO  WRK-GEN30-STR
               END-IF
      *
           END-PERFORM
      *
      *H29.4
      *    一般名加算算定編集
      *H31.3   (2018からの判定のみとする)
           IF      (FLG-GENERIC-OK     =   1   )
               OR ((CNT-GENERIC        >=  2   )  AND
                   (FLG-GENERIC-NO     =   ZERO))
      *        1,2あり、3,4を含めて２件
               MOVE    1               TO  FLG-GENERIC-OK12
           END-IF
      *
      ***  IF      SPA-SRYYMD          >=  "20170317"
      *        IF      (FLG-GENERIC-OK     =   1   )
      *            OR ((CNT-GENERIC        >=  2   )  AND
      *                (FLG-GENERIC-NO     =   ZERO))
      *            MOVE    1               TO  FLG-GENERIC-OK12
      *        END-IF
      *        4/1までは、３，４で２つ以上あれば対象
      *        IF      (SPA-SRYYMD         <   "20170401")
      *            AND (FLG-GENERIC-NO2    =   ZERO )
      *            AND (CNT-GENERIC3       >=  2    )
      *            MOVE    1               TO  FLG-GENERIC-OK12
      *        END-IF
      *    ELSE
      *        IF      (FLG-GENERIC-OK     =   1   )
      *            MOVE    1               TO  FLG-GENERIC-OK12
      *        END-IF
      *****END-IF
      *
      *H26.4
      *    外来のうがい薬のみのチェック
           IF     (WRK-UGAI-YAK        >   ZERO )
             AND  (WRK-UGAI-IGAI       =   ZERO )
             AND  (SPA-NYUGAIKBN       =   "2"  )
               IF     (WRK-COM-KZMFLG(IDX-UGAI)    =   SPACE) AND
                      (LNK-ORCSC-KEI-IDX           =   ZERO )
                   MOVE    "K250"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR(IDX-UGAI)
                   MOVE    "1"             TO  WRK-COM-KZMFLG(IDX-UGAI)
                   MOVE    IDX-UGAI        TO  LNK-ORCSC-KEI-IDX
               END-IF
           END-IF
      *
      *H30.4
      *H30.4.3 廃止
      *    皮膚保湿薬チェック
      *    IF     (WRK-HIFU-YAK        >   ZERO )
      *      AND  (SPA-NYUGAIKBN       =   "2"  )
      *        IF     (WRK-COM-KZMFLG(IDX-HIFU)    =   SPACE) AND
      *               (LNK-ORCSC-KEI-IDX           =   ZERO )
      *            MOVE    "K120"          TO  SPA-ERRCD
      *            MOVE    "1"             TO  WRK-COM-CUR(IDX-HIFU)
      *            MOVE    "1"             TO  WRK-COM-KZMFLG(IDX-HIFU)
      *            MOVE    IDX-HIFU        TO  LNK-ORCSC-KEI-IDX
      *        END-IF
      **** END-IF
      *
      *H28.4
      *    保険毎の逓減計算（外用）（入力チェックの後へ）
      *    INITIALIZE                  WRK-HKNKEISP-G
      *    湿布薬７０枚以上
      *    IF      FLG-SIP70-ARI      =   1
      *        剤点数再計算
      *        PERFORM 2009-SIP-SAIKEISAN-SEC
      *    END-IF
      *
      *H26.10
      *    抗精神薬剤投与チェック処理
           IF     (SPA-ERRCD           =   SPACE)
            AND   (SPA-NYUGAIKBN       =   "2"  )
               PERFORM 30003-PSYCHO-CHK-SEC
           END-IF
      *
      *
      *H28.9
      *    認知症地域包括診療　薬剤チェック処理
           IF     (SPA-ERRCD           =   SPACE)
            AND   (SPA-NYUGAIKBN       =   "2"  )
            AND   (FLG-NINCHIKSN   NOT =   ZERO )
            AND   (SPA-NINCHI-CHK      =   ZERO )
               PERFORM 3009-NINCHIKSN-CHK-SEC
           END-IF
      *
      *    入力分のチェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 3003-CHEK-SYORI-SEC
           END-IF
      *H28.4
      *    保険毎の逓減計算（外用）
           INITIALIZE                  WRK-HKNKEISP-G
           IF      SPA-ERRCD           =   SPACE
      *        湿布薬７０枚以上
               IF      FLG-SIP70-ARI      =   1
      *            剤点数再計算
                   PERFORM 2009-SIP-SAIKEISAN-SEC
               END-IF
           END-IF
      *
           IF     (LNK-ORCSC-KIDCD     NOT =   SPACE )  OR
                  (SPA-ERRCD           NOT =   SPACE )
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           ELSE
      *        院外分
               IF      SPA-NYUGAIKBN       =   "2"
                   PERFORM 3001-INGAI-SYORI-SEC
               END-IF
      *        院内分
               IF     (SPA-ERRCD           =   SPACE )
                  AND (LNK-ORCSC-KIDCD     =   SPACE )
                   IF      SPA-NYUGAIKBN       =   "1"
      *                入院
                       PERFORM 30021-NYUIN-INNAI-SYORI-SEC
                   ELSE
      *            外来
                       PERFORM 3002-INNAI-SYORI-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       3000-JIDOU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療種別区分変換処理
      *****************************************************************
       30001-SRYSYUKBN-SET-SEC              SECTION.
      *
           MOVE    WRK-COM-SRYSYUKBN (IDX) TO  WRK-CHK-SRYSYUKBN
      *
           IF      WRK-CHK-SRYSYUKBN       =   SPACE
               EVALUATE    WRK-COM-SRYKBN (IDX)
                   WHEN    "21"
                       MOVE    "210"       TO  WRK-CHK-SRYSYUKBN
                   WHEN    "22"
                       MOVE    "220"       TO  WRK-CHK-SRYSYUKBN
                   WHEN    "23"
                       MOVE    "230"       TO  WRK-CHK-SRYSYUKBN
               END-EVALUATE
           END-IF
      *
      *    院外の時
           IF      LNK-ORCSC-INGAIKBN     =   "1"
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
           ELSE
               EVALUATE    WRK-CHK-SRYSYUKBN
                   WHEN    "210"
                       MOVE    "211"          TO  WRK-CHK-SRYSYUKBN
                   WHEN    "290" 
                       MOVE    "291"          TO  WRK-CHK-SRYSYUKBN
                   WHEN    "220" 
                       MOVE    "221"          TO  WRK-CHK-SRYSYUKBN
                   WHEN    "230" 
                       MOVE    "231"          TO  WRK-CHK-SRYSYUKBN
      *H26.4
                   WHEN    "293"
                       MOVE    "294"          TO  WRK-CHK-SRYSYUKBN
      *H26.10      頓服臨時
                   WHEN    "296"
                       MOVE    "297"          TO  WRK-CHK-SRYSYUKBN
               END-EVALUATE
           END-IF
          .
       30001-SRYSYUKBN-SET-EXT.
           EXIT.
      *
      *H26.10
      *****************************************************************
      *   抗精神薬剤チェック編集処理
      *****************************************************************
       30002-PSYCHO-HEN-SEC              SECTION.
      *
           ADD     1                    TO  IDX-PS
           IF      IDX-PS               >   50
               GO  TO      30002-PSYCHO-HEN-EXT
           END-IF
           MOVE    WRK-COM-INPUTCD (IDX)   TO  ORCSPSYCHO-SRYCD(IDX-PS)
           MOVE    WRK-COM-YAKKAKJNCD(IDX) TO  ORCSPSYCHO-YAKKAKJNCD
                                                               (IDX-PS)
      *
           MOVE    WRK-CHK-SRYSYUKBN       TO  ORCSPSYCHO-SRYSYUKBN
                                                               (IDX-PS)
           MOVE    WRK-COM-KAISU (IDX)     TO  ORCSPSYCHO-KAISU(IDX-PS)
      *
           .
       30002-PSYCHO-HEN-EXT.
           EXIT.
      *****************************************************************
      *   向精神薬剤投与チェック処理
      *****************************************************************
       30003-PSYCHO-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-TAZAI-ARI
      *
           MOVE    SPA-SRYYMD          TO  ORCSPSYCHO-SRYYMD
           CALL    "ORCSPSYCHO"    USING   ORCSPSYCHOAREA
                                           SPA-AREA
           IF      ORCSPSYCHO-TAZAI    =   1
      *        多剤投与対象
               MOVE    1               TO  FLG-TAZAI-ARI
           END-IF
      *
      *H28.6
      *    精神逓減用
           ADD     1                   TO  IDX-80
           IF      IDX-80              >   10
               MOVE    10                  TO  IDX-80
           END-IF
           MOVE    WRK-SRYKA           TO  SPA-PSY80-KA (IDX-80)
           IF    (ORCSPSYCHO-COUNT (3) >=  3 )
             OR  (ORCSPSYCHO-COUNT (4) >=  3 )
      *         抗うつ薬(3)、向精神薬(4)を３種類以上
               MOVE    1               TO  SPA-PSY80-ARI(IDX-80)
               IF   ((ORCSPSYCHO-COUNT (3)  =  3 )
                 AND (ORCSPSYCHO-COUNT (4)  <= 3 ))
               OR   ((ORCSPSYCHO-COUNT (3)  <= 3 )
                 AND (ORCSPSYCHO-COUNT (4)  =  3 ))
      *          どちらかが３種類
                   MOVE    2               TO  SPA-PSY80-ARI(IDX-80)
               END-IF
           ELSE
               MOVE    ZERO            TO  SPA-PSY80-ARI(IDX-80)
           END-IF
      *H30.4
      *    精神科継続外来支援のチェック（不安・睡眠２種類は対象外）
           IF    (ORCSPSYCHO-COUNT (1) >=  3 )
             OR  (ORCSPSYCHO-COUNT (2) >=  3 )
             OR  (ORCSPSYCHO-COUNT (3) >=  3 )
             OR  (ORCSPSYCHO-COUNT (4) >=  3 )
               IF      SPA-PSY80-ARI(IDX-80)   =   ZERO
                   MOVE    3               TO  SPA-PSY80-ARI(IDX-80)
               END-IF
           END-IF
      *!!
      *H30.4
      *    不安不眠薬入力あり
      *    IF    (ORCSPSYCHO-COUNT (1) >=  ZERO )
      *      OR  (ORCSPSYCHO-COUNT (2) >=  ZERO )
      *        MOVE    1               TO  WRK-FUANYAKU
      ***  END-IF
           .
       30003-PSYCHO-CHK-EXT.
           EXIT.
      *
      *H28.9
      *****************************************************************
      *   認知症地域包括診療　薬剤チェック処理
      *****************************************************************
       3009-NINCHIKSN-CHK-SEC              SECTION.
      *
           INITIALIZE                  WRK-NINCHI-CHK-G
           MOVE    ZERO                TO  IDX-NINYK
      *    向精神薬多剤投与チェック
           INITIALIZE                  ORCSPSYCHOAREA
           MOVE    ZERO                TO  IDX-PS
      *
           MOVE    ZERO                TO  FLG-ZAIEND
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                             ((WRK-GMN-INPUTCD(IDX) =   SPACE) AND
                              (WRK-COM-INPUTCD(IDX) =   SPACE))
      *
               IF     (WRK-COM-SRYKA (IDX)     =   WRK-SRYKA  )
                AND   (WRK-COM-HKNCOMBI (IDX)  NOT =   "9999" )
      *            薬評対象外
                AND   (WRK-COM-YAKUHYO  (IDX)  =   SPACE  )
      *             減点対象外
                AND   (WRK-COM-GENTEN   (IDX)  =   SPACE  )
      *
      *            診療種別区分変換
                   PERFORM 30001-SRYSYUKBN-SET-SEC
      *            投薬の時、薬剤コードがあること
                   IF     (WRK-CHK-SRYSYUKBN(1:2)  =   "21"
                                                   OR  "22"
                                                   OR  "23"
                                                   OR  "29" )  AND
                           (FLG-ZAIEND             =   ZERO )
                       PERFORM 30001-TOUYAKU-CHK-SEC
                       MOVE    1                   TO  FLG-ZAIEND
                   END-IF
      *            内服投薬
                   IF      (WRK-CHK-SRYSYUKBN(1:2) =   "21")
                       AND (WRK-COM-INPUTCD(IDX)(1:1)   =   "6")
                       ADD     1                   TO  WRK-NINCHI-CNT
                       MOVE    IDX                 TO  IDX-NINYK
                   END-IF
      *            臨時投薬の１４日以上
                   IF      (WRK-CHK-SRYSYUKBN(1:2) =   "29")
                       AND (WRK-COM-INPUTCD(IDX)(1:1)   =   "6")
                       AND (WRK-COM-KAISU (IDX)         >   14 )
                       ADD     1                   TO  WRK-NINCHI-CNT
                       MOVE    IDX                 TO  IDX-NINYK
                   END-IF
      *
                   IF      (WRK-CHK-SRYSYUKBN(1:2) =   "21"
                                                   OR  "22"
                                                   OR  "29")
                       AND (WRK-COM-INPUTCD(IDX)(1:1)  =   "6")
                       MOVE    IDX                 TO  IDX-NINYK2
                   END-IF
      *            抗精神薬剤チェック編集
                   IF     (WRK-CHK-SRYSYUKBN(1:2)  =   "21"
                                                   OR  "22"
                                                   OR  "23"
                                                   OR  "29" )
                    AND   (WRK-COM-INPUTCD (IDX)(1:1)  =   "6")
                       PERFORM 30002-PSYCHO-HEN-SEC
                   END-IF
      *
               END-IF
               IF      WRK-COM-ZAIEND (IDX)    =   "1"
                   MOVE    ZERO                TO  FLG-ZAIEND
               END-IF
           END-PERFORM
      *
           IF      IDX-NINCHI          =   ZERO
               MOVE    IDX-NINYK           TO  IDX-NINCHI
           END-IF
      *
      *    抗精神薬剤チェック処理
           MOVE    SPA-SRYYMD          TO  ORCSPSYCHO-SRYYMD
           CALL    "ORCSPSYCHO"    USING   ORCSPSYCHOAREA
                                           SPA-AREA
      *    抗うつ薬、抗精神薬、抗不安薬、睡眠薬を合わせて３種類を超える
           COMPUTE WRK-SEICNT          =   ORCSPSYCHO-COUNT (1)
                                       +   ORCSPSYCHO-COUNT (2)
                                       +   ORCSPSYCHO-COUNT (3)
                                       +   ORCSPSYCHO-COUNT (4)
      *
      *    内服が５種類を超える
      *    抗うつ薬、抗精神薬、抗不安薬、睡眠薬を合わせて３種類を超える
           IF     (WRK-NINCHI-CNT      >   5  )
               OR (WRK-SEICNT          >   3  )
               IF     (SPA-NINCHI-CHK          =   ZERO )
                 AND  (LNK-ORCSC-KEI-IDX       =   ZERO )
                   MOVE    "K257"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR
                                                   (IDX-NINCHI)
                   MOVE    IDX-NINCHI          TO  LNK-ORCSC-KEI-IDX
                   MOVE    1                   TO  SPA-NINCHI-CHK
               END-IF
           END-IF
      *
           .
       3009-NINCHIKSN-CHK-EXT.
           EXIT.
      *H28.9
      *
      *
      *****************************************************************
      *   入力内容のチェック処理
      *****************************************************************
       3003-CHEK-SYORI-SEC              SECTION.
      *
      *H24.6
           MOVE    SPACE               TO  WRK-ERRCD2
      *    処方箋を交付する診察料を算定している場合、算定なし
           IF      (LNK-ORCSC-ROUMANSEI-ARI    =   1  )  OR
                   (LNK-ORCSC-ROUMANSEI-ARI2   >   ZERO) OR
                   (LNK-ORCSC-NETAKIRI-ARI     =   1  )  OR
                   (LNK-ORCSC-NETAKIRI-ARI2    >   ZERO) OR
                   (LNK-ORCSC-SYONIKA-ARI  NOT =   ZERO) OR
                   (LNK-ORCSC-SEIKATU-ARI2     >   ZERO) OR
                   (LNK-ORCSC-SEIKATU-ARI      >   ZERO)
      *             在宅末期医療総合診察料あり
               OR  (SPA-ZAIMATU-ARI            >   ZERO )
      *             在宅時医療総合管理料あり
               OR  (SPA-ZAIKANRI-ARI           >   ZERO)
               OR  (SPA-ZAIKANRI-ARI2          >   ZERO)
      *        特定施設入居時等医学管理料
               OR  (SPA-IGAKUKAN-ARI       NOT =   ZERO)
               OR  (SPA-IGAKUKAN-ARI2      NOT =   ZERO)
      *        処方せん料が算定不可の時
               MOVE    ZERO                TO  FLG-SYOHO-CHK
      *        処方せん料の入力あり
               IF     (IDX-SYOHOU      NOT =   ZERO )
                  OR  (IDX-IP          NOT =   ZERO )
                   MOVE    "K237"              TO  WRK-ERRCD2
      *            生活習慣病指導管理料チェック
                   IF     (SPA-SEIKATU-ARI     NOT =   ZERO )  OR
                          (SPA-SEIKATU-ARI2    NOT =   ZERO )
                       IF      SPA-HKTSANTEI-203   NOT =   ZERO
                           MOVE    SPACE               TO  WRK-ERRCD2
                       END-IF
                   END-IF
      *            小児科外来診察料チェック
                   IF      (LNK-ORCSC-SYONIKA-ARI  NOT =   ZERO)
                       IF      SPA-HKTSANTEI-202   NOT =   ZERO
                           MOVE    SPACE               TO  WRK-ERRCD2
                       END-IF
                   END-IF
      *            在宅時医学総合管理料チェック
                   IF     (SPA-ZAIKANRI-ARI    NOT =   ZERO )  OR
                          (SPA-ZAIKANRI-ARI2   NOT =   ZERO )
                       IF      SPA-HKTSANTEI-204   NOT =   ZERO
                           MOVE    SPACE               TO  WRK-ERRCD2
                       END-IF
                   END-IF
      *            特定施設入居時等医学管理料チェック
                   IF     (SPA-IGAKUKAN-ARI    NOT =   ZERO )  OR
                          (SPA-IGAKUKAN-ARI2   NOT =   ZERO )
                       IF      SPA-HKTSANTEI-205   NOT =   ZERO
                           MOVE    SPACE               TO  WRK-ERRCD2
                       END-IF
                   END-IF
      *            在宅末期医療総合診察料チェック
                   IF     (SPA-ZAIMATU-ARI    NOT =   ZERO )  OR
                          (SPA-ZAIMATU-ARI2   NOT =   ZERO )
                       IF      SPA-HKTSANTEI-206   NOT =   ZERO
                           MOVE    SPACE               TO  WRK-ERRCD2
                       END-IF
                   END-IF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYOHO-CHK
           END-IF
           IF      WRK-ERRCD2       NOT =   SPACE
               IF      IDX-SYOHOU      NOT =   ZERO
                   IF     (WRK-COM-KZMFLG (IDX-SYOHOU) =   SPACE) AND
                          (LNK-ORCSC-KEI-IDX       =   ZERO )
                       MOVE    WRK-ERRCD2      TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR
                                                        (IDX-SYOHOU)
                       MOVE    "1"             TO  WRK-COM-KZMFLG
                                                        (IDX-SYOHOU)
                       MOVE    IDX-IP          TO  LNK-ORCSC-KEI-IDX
                   END-IF
               END-IF
               IF     (IDX-IP          NOT =   ZERO )
               AND    (SPA-ERRCD           =   SPACE)
                   IF     (WRK-COM-KZMFLG (IDX-IP) =   SPACE) AND
                          (LNK-ORCSC-KEI-IDX       =   ZERO )
                       MOVE    WRK-ERRCD2      TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-IP)
                       MOVE    "1"             TO  WRK-COM-KZMFLG
                                                              (IDX-IP)
                       MOVE    IDX-IP          TO  LNK-ORCSC-KEI-IDX
                   END-IF
               END-IF
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      3003-CHEK-SYORI-EXT
               END-IF
           END-IF
      *!!!
      *
      *
           MOVE    ZERO                TO  FLG-SYOHOSENN
      *    処方箋料算定
           IF    ((FLG-NAIYAK2         =   1    )  OR
                  (FLG-TONPUK2         =   1    )  OR
                  (FLG-GAIYAK2         =   1    )) OR
                  (IDX-SYOHOU      NOT =   ZERO )
               MOVE    1               TO  FLG-SYOHOSENN
      *H24.6
      *        処方せん料自動発生なしの時
               IF     (FLG-SYOHO-CHK       =   ZERO )
                 AND  (IDX-SYOHOU          =   ZERO )
                   MOVE    ZERO                TO  FLG-SYOHOSENN
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-SYOHORYO
      *    処方料算定
           IF    ((FLG-NAIYAK2         =   1    )  OR
                  (FLG-TONPUK2         =   1    )  OR
                  (FLG-GAIYAK2         =   1    ))   OR
                  (IDX-SYOHOU      NOT =   ZERO )
      *        院外投薬・処方箋料算定あり（処方料算定不可）
               MOVE    ZERO                TO  FLG-SYOHORYO
           ELSE
               IF     (FLG-NAIYAK          =   1    )  OR
                      (FLG-TONPUK          =   1    )  OR
                      (FLG-GAIYAK          =   1    )
      *            院内投薬あり（処方料算定可）
                   MOVE    1               TO  FLG-SYOHORYO
               END-IF
           END-IF
      *
      *    特定疾患処方管理加算処方箋料がある時、処方箋料があること
           IF      FLG-TOKUSYO1        =   1
               IF      FLG-SYOHOSENN       =   ZERO
                   MOVE    "1014"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-T1)
      *            処方料もない時、警告とする
                   IF      FLG-SYOHORYO        =   ZERO
                       MOVE    SPACE           TO  SPA-ERRCD
                       MOVE    SPACE           TO  WRK-COM-CUR (IDX-T1)
                       IF     (WRK-COM-KZMFLG (IDX-T1) =   SPACE) AND
                              (LNK-ORCSC-KEI-IDX       =   ZERO )
                           MOVE    "K114"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-T1)
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                              (IDX-T1)
                           MOVE    IDX-T1          TO  LNK-ORCSC-KEI-IDX
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    特定疾患処方管理加算処方料がある時の、処方料があること
           IF      FLG-TOKUSYO2        =   1
               IF      FLG-SYOHORYO        =   ZERO
                   MOVE    "1015"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-T2)
      *            処方せん料もない時、警告とする
                   IF      FLG-SYOHOSENN        =   ZERO
                       MOVE    SPACE           TO  SPA-ERRCD
                       MOVE    SPACE           TO  WRK-COM-CUR (IDX-T2)
                       IF     (WRK-COM-KZMFLG (IDX-T2) =   SPACE) AND
                              (LNK-ORCSC-KEI-IDX       =   ZERO )
                           MOVE    "K115"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-T2)
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                              (IDX-T2)
                           MOVE    IDX-T2          TO  LNK-ORCSC-KEI-IDX
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    長期投薬処方箋料がある時、処方箋料があること
           IF      FLG-CHOKIKA1        =   1
               IF      FLG-SYOHOSENN       =   ZERO
                   MOVE    "1038"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-T1)
      *            処方料もない時、警告とする
                   IF      FLG-SYOHORYO        =   ZERO
                       MOVE    SPACE           TO  SPA-ERRCD
                       MOVE    SPACE           TO  WRK-COM-CUR (IDX-T1)
                       IF     (WRK-COM-KZMFLG (IDX-T1) =   SPACE) AND
                              (LNK-ORCSC-KEI-IDX       =   ZERO )
                           MOVE    "K038"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-T1)
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                              (IDX-T1)
                           MOVE    IDX-T1          TO  LNK-ORCSC-KEI-IDX
                       END-IF
                   END-IF
               ELSE
      *            長期投薬があること
                   IF      FLG-CHOUKI          =   ZERO
                      IF     (WRK-COM-KZMFLG (IDX-T1)  =   SPACE) AND
                             (LNK-ORCSC-KEI-IDX        =   ZERO )
                           MOVE    "K021"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-T1)
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                              (IDX-T1)
                           MOVE    IDX-T1          TO  LNK-ORCSC-KEI-IDX
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    長期投薬加算処方料がある時の、処方料があること
           IF      FLG-CHOKIKA2        =   1
               IF      FLG-SYOHORYO        =   ZERO
                   MOVE    "1037"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-T2)
      *            処方せん料もない時、警告とする
                   IF      FLG-SYOHOSENN        =   ZERO
                       MOVE    SPACE           TO  SPA-ERRCD
                       MOVE    SPACE           TO  WRK-COM-CUR (IDX-T2)
                       IF     (WRK-COM-KZMFLG (IDX-T2) =   SPACE) AND
                              (LNK-ORCSC-KEI-IDX       =   ZERO )
                           MOVE    "K037"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-T2)
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                              (IDX-T2)
                           MOVE    IDX-T2          TO  LNK-ORCSC-KEI-IDX
                       END-IF
                   END-IF
               ELSE
      *            長期投薬があること
                   IF      FLG-CHOUKI          =   ZERO
                       IF    (WRK-COM-KZMFLG (IDX-T2)  =   SPACE) AND
                             (LNK-ORCSC-KEI-IDX        =   ZERO )
                           MOVE    "K021"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-T2)
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                              (IDX-T2)
                           MOVE    IDX-T2          TO  LNK-ORCSC-KEI-IDX
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *Ｈ２２年４月から
      *    抗悪性腫瘍剤処方管理加算（処方せん）は処方箋料があること
           IF      FLG-AKUSKSN1        =   1
               IF      FLG-SYOHOSENN       =   ZERO
                   MOVE    "0230"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-A1)
      *            処方料もない時、警告とする
                   IF      FLG-SYOHORYO        =   ZERO
                       MOVE    SPACE           TO  SPA-ERRCD
                       MOVE    SPACE           TO  WRK-COM-CUR (IDX-A1)
                       IF     (WRK-COM-KZMFLG (IDX-A1) =   SPACE) AND
                              (LNK-ORCSC-KEI-IDX       =   ZERO )
                           MOVE    "K230"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-A1)
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                              (IDX-A1)
                           MOVE    IDX-A1          TO  LNK-ORCSC-KEI-IDX
                       END-IF
                   END-IF
               END-IF
           END-IF
      *    抗悪性腫瘍剤処方管理加算（処方料）は処方箋料があること
           IF      FLG-AKUSKSN2        =   1
               IF      FLG-SYOHORYO        =   ZERO
                   MOVE    "0231"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-A2)
      *            処方せん料もない時、警告とする
                   IF      FLG-SYOHOSENN        =   ZERO
                       MOVE    SPACE           TO  SPA-ERRCD
                       MOVE    SPACE           TO  WRK-COM-CUR (IDX-A2)
                       IF     (WRK-COM-KZMFLG (IDX-A2) =   SPACE) AND
                              (LNK-ORCSC-KEI-IDX       =   ZERO )
                           MOVE    "K231"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-A2)
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                              (IDX-A2)
                           MOVE    IDX-A2          TO  LNK-ORCSC-KEI-IDX
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *
      *H30.4
      *    向精神薬調整連携加算（処方せん）は処方箋料があること
           IF      FLG-RENKSN1         =   1
               IF      FLG-SYOHOSENN       =   ZERO
                   MOVE    "0245"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-R1)
      *            処方料もない時、警告とする
                   IF      FLG-SYOHORYO        =   ZERO
                       MOVE    SPACE           TO  SPA-ERRCD
                       MOVE    SPACE           TO  WRK-COM-CUR (IDX-R1)
                       IF     (WRK-COM-KZMFLG (IDX-R1) =   SPACE) AND
                              (LNK-ORCSC-KEI-IDX       =   ZERO )
                           MOVE    "K245"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-R1)
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                              (IDX-R1)
                           MOVE    IDX-R1          TO  LNK-ORCSC-KEI-IDX
                       END-IF
                   END-IF
               END-IF
           END-IF
      *    向精神薬調整連携加算（処方料）は処方料があること
           IF      FLG-RENKSN2        =   1
               IF      FLG-SYOHORYO        =   ZERO
                   MOVE    "0246"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-R2)
      *            処方せん料もない時、警告とする
                   IF      FLG-SYOHOSENN        =   ZERO
                       MOVE    SPACE           TO  SPA-ERRCD
                       MOVE    SPACE           TO  WRK-COM-CUR (IDX-R2)
                       IF     (WRK-COM-KZMFLG (IDX-R2) =   SPACE) AND
                              (LNK-ORCSC-KEI-IDX       =   ZERO )
                           MOVE    "K246"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-R2)
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                              (IDX-R2)
                           MOVE    IDX-R2          TO  LNK-ORCSC-KEI-IDX
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *H24.4　改正
      *    一般名処方加算（処方せん料）処方箋料があること
           IF      FLG-IPPANKSN        =   1
               IF      FLG-SYOHOSENN       =   ZERO
                   MOVE    "0234"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-IP)
      *            処方料もない時、警告とする
                   IF      FLG-SYOHORYO        =   ZERO
                       MOVE    SPACE           TO  SPA-ERRCD
                       MOVE    SPACE           TO  WRK-COM-CUR (IDX-IP)
                       IF     (WRK-COM-KZMFLG (IDX-IP) =   SPACE) AND
                              (LNK-ORCSC-KEI-IDX       =   ZERO )
                           MOVE    "K234"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-IP)
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                              (IDX-IP)
                           MOVE    IDX-IP          TO  LNK-ORCSC-KEI-IDX
                       END-IF
                   END-IF
               END-IF
           END-IF
      *!!
      *ver.4.7
      *    院外投薬の用法まとめ対応
           IF     (SPA-ERRCD           =   SPACE)  AND
                 ((FLG-NAIYAK2         =   1    )  OR
                  (FLG-TONPUK2         =   1    )  OR
                  (FLG-GAIYAK2         =   1    ))
      *        用法まとめ内服種類チェック処理
               PERFORM 30030-YOUHOU-HEN-SEC
           END-IF
      *
      *ver.4.7
      *    処方せん料と投薬チェック
           IF     (IDX-SYOHOU      NOT =   ZERO )  AND
                  (FLG-NAIYAK2         =   ZERO )  AND
                  (FLG-TONPUK2         =   ZERO )  AND
                  (FLG-GAIYAK2         =   ZERO )  AND
                  (SPA-ERRCD           =   SPACE )
               IF     (FLG-NAIYAK          =   1    )  OR
                      (FLG-TONPUK          =   1    )  OR
                      (FLG-GAIYAK          =   1    )
      *            処方せん料と院内投薬のみは警告
                   IF     (WRK-COM-KZMFLG6 (IDX-SYOHOU) =   SPACE) AND
                          (LNK-ORCSC-KEI-IDX           =   ZERO )
                       MOVE    "K122"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR
                                                          (IDX-SYOHOU)
                       MOVE    "1"             TO  WRK-COM-KZMFLG6
                                                          (IDX-SYOHOU)
                       MOVE    IDX-SYOHOU      TO  LNK-ORCSC-KEI-IDX
                   END-IF
               END-IF
           END-IF
      *
      *H26.4
      *    地域包括診療算定時　逓減なし
           IF      FLG-CHKCARKSN       =   1
               MOVE    6               TO  WRK-ZAICNT2
               MOVE    6               TO  WRK-ZAICNT2-AL
           END-IF
      *
      *    処方せん料と投薬チェック
           IF     (IDX-SYOHOU      NOT =   ZERO )  AND
                  (SPA-ERRCD           =   SPACE)  AND
      *H26.10    向精神薬多剤処方せん料以外
                  (FLG-TAZAI1          =   ZERO )  AND
      *
                 ((FLG-NAIYAK2         =   1    )  OR
                  (FLG-TONPUK2         =   1    )  OR
                  (FLG-GAIYAK2         =   1    ))
      *        処方せん料の内容チェック
               IF      WRK-ZAICNT2         >=  7
      *            ７種類以上
                   IF      WRK-COM-INPUTCD (IDX-SYOHOU)
                                           NOT =   "120002710"
                       MOVE    "K232"          TO  SPA-ERRCD
      *                用法まとめ分
                       IF      WRK-ZAICNT2-AL      <    7
                           STRING  "用法まとめでは"
                                   "内服薬剤が７種類未満です。"
                                       DELIMITED  BY  SIZE
                                       INTO  LNK-ORCSC-ERRMSG
                           END-STRING
                       END-IF
                   ELSE
                       IF      WRK-ZAICNT2-AL      <    7
      *                用法まとめ分
                           MOVE    "K238"          TO  SPA-ERRCD
                       END-IF
      *H31.3
      *                向精神薬長期投与　対象警告なし
                       IF     (FLG-PSYCHOKI-OK     =   1 )
                          AND (SPA-SRYYMD          >=  "20190401")
                           MOVE    SPACE           TO  SPA-ERRCD
                           MOVE    SPACE           TO  LNK-ORCSC-ERRMSG
                       END-IF
                   END-IF
               ELSE
      *            ７種類以内
                   IF     (WRK-COM-INPUTCD (IDX-SYOHOU)
                                               =   "120002710" )
                       MOVE    "K233"          TO  SPA-ERRCD
      *                用法まとめ分
                       IF      WRK-ZAICNT2-AL      >=   7
                           STRING  "用法まとめでは"
                                   "内服薬剤が７種類以上となります。"
                                       DELIMITED  BY  SIZE
                                       INTO  LNK-ORCSC-ERRMSG
                           END-STRING
                       END-IF
      *H31.3
      *                向精神薬長期投与　対象警告なし
                       IF     (FLG-PSYCHOKI-OK     =   1 )
                          AND (SPA-SRYYMD          >=  "20190401")
                           MOVE    SPACE           TO  SPA-ERRCD
                           MOVE    SPACE           TO  LNK-ORCSC-ERRMSG
                       END-IF
      *
                   ELSE
      *                用法まとめ分
                       IF      WRK-ZAICNT2-AL      >=   7
                           MOVE    "K239"          TO  SPA-ERRCD
                       END-IF
      *H31.3
      *                向精神薬長期投与　対象警告
                       IF     (FLG-PSYCHOKI-OK     =   1 )
                          AND (SPA-SRYYMD          >=  "20190401")
                           MOVE    "K278"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
      *
               IF      SPA-ERRCD       NOT =   SPACE
                   IF     (WRK-COM-KZMFLG (IDX-SYOHOU) =   SPACE) AND
                          (LNK-ORCSC-KEI-IDX           =   ZERO )
                       MOVE    "1"             TO  WRK-COM-CUR
                                                          (IDX-SYOHOU)
                       MOVE    "1"             TO  WRK-COM-KZMFLG
                                                          (IDX-SYOHOU)
                       MOVE    IDX-SYOHOU      TO  LNK-ORCSC-KEI-IDX
                   ELSE
                       MOVE    SPACE               TO  SPA-ERRCD
                       MOVE    SPACE               TO  LNK-ORCSC-ERRMSG
                   END-IF
               END-IF
           END-IF
      *H24.6
      *    処方せん料の入力があり一般名処方加算なし
           IF     (IDX-SYOHOU      NOT =   ZERO )
             AND  (FLG-IPPANKSN        =   ZERO )
             AND  (SPA-ERRCD           =   SPACE)
             AND  (FLG-GENERIC-OK12    =   1    )
      *        処方せん料の入力があり、一般名加算対象
               IF     (WRK-COM-KZMFLG2 (IDX-SYOHOU) =   SPACE) AND
                      (LNK-ORCSC-KEI-IDX       =   ZERO )
                   MOVE    "K236"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR
                                                        (IDX-SYOHOU)
                   MOVE    "1"             TO  WRK-COM-KZMFLG2
                                                        (IDX-SYOHOU)
                   MOVE    IDX-SYOHOU      TO  LNK-ORCSC-KEI-IDX
                END-IF
           END-IF
      *H24.6
      *    一般名加算の対象チェック
           IF     (FLG-IPPANKSN        =   1   )
             AND  (SPA-ERRCD           =   SPACE)
             AND ((FLG-NAIYAK2         =   1    )  OR
                  (FLG-TONPUK2         =   1    )  OR
                  (FLG-GAIYAK2         =   1    ))
      *     一般名加算と院外投薬あり
               IF     (FLG-GENERIC-OK12    =   ZERO )
      *            一般名加算投薬なし
                   IF     (WRK-COM-KZMFLG (IDX-IP) =   SPACE) AND
                          (LNK-ORCSC-KEI-IDX       =   ZERO )
                       MOVE    "K235"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-IP)
                       MOVE    "1"             TO  WRK-COM-KZMFLG
                                                              (IDX-IP)
                       MOVE    IDX-IP          TO  LNK-ORCSC-KEI-IDX
                   END-IF
      *H28.4
               ELSE
      *H29.4
      *H31.3       (2018からの判定のみとする)
                   IF    ((FLG-GENERIC-NO      =   ZERO )
                      AND (CNT-GENERIC         >=  2    ))
      ***          OR    ((SPA-SRYYMD          <   "20170401")
      *               AND (SPA-SRYYMD          >=  "20170317")
      *               AND (FLG-GENERIC-NO2     =   ZERO )
      *               AND (CNT-GENERIC2        >=  2    ))
      *            OR    ((SPA-SRYYMD          <   "20170401")
      *               AND (SPA-SRYYMD          >=  "20170317")
      *               AND (FLG-GENERIC-NO2    =   ZERO  )
      ****            AND (CNT-GENERIC3        >=  2    ))
      *                一般名加算１
                       IF      WRK-COM-INPUTCD (IDX-IP)
                                              NOT =   "120004270"
                           MOVE    "K223"          TO  SPA-ERRCD
                       END-IF
                   ELSE
      *                一般名加算２
                       IF      WRK-COM-INPUTCD (IDX-IP)
                                              NOT =   "120003570"
                           MOVE    "K222"          TO  SPA-ERRCD
                       END-IF
                   END-IF
      *
                   IF     (SPA-ERRCD          NOT =   SPACE)
                       IF     (WRK-COM-KZMFLG (IDX-IP) =   SPACE)
                         AND  (LNK-ORCSC-KEI-IDX       =   ZERO )
                           MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-IP)
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                              (IDX-IP)
                           MOVE    IDX-IP          TO  LNK-ORCSC-KEI-IDX
                       ELSE
                           MOVE    SPACE           TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *!!!!
      *
      *    調基が算定時の処方せん料とのチェック
           IF      IDX-CHOKI           >   ZERO
      *        処方せん料の算定チェック
               PERFORM 30024-SYOHOUSEN-CHK-SEC
               IF      FLG-SYOHOU-ERR  NOT =   ZERO
                   MOVE    "1039"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-CHOKI)
               END-IF
           END-IF
      *
      *H26.10
      *    向精神薬多剤投与処方せん料のチェック
           IF     (SPA-ERRCD           =   SPACE )
             AND  (FLG-TAZAI1          =   1     )
             AND ((FLG-NAIYAK2         =   1    )  OR
                  (FLG-TONPUK2         =   1    )  OR
                  (FLG-GAIYAK2         =   1    ))
      *        多剤投与対象外で、向精神薬処方せん料は警告
               IF      FLG-TAZAI-ARI   =   ZERO
                   IF     (WRK-COM-KZMFLG (IDX-SYOHOU)
                                                   =   SPACE) AND
                          (LNK-ORCSC-KEI-IDX       =   ZERO )
                       MOVE    "K252"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR
                                                          (IDX-SYOHOU)
                       MOVE    "1"             TO  WRK-COM-KZMFLG
                                                          (IDX-SYOHOU)
                       MOVE    IDX-SYOHOU      TO  LNK-ORCSC-KEI-IDX
                   END-IF
               END-IF
           END-IF
           IF     (SPA-ERRCD           =   SPACE )
             AND  (IDX-SYOHOU      NOT =   ZERO  )
             AND  (FLG-TAZAI1          =   ZERO  )
      *        多剤投与対象で、他の処方せん料は警告
               IF      FLG-TAZAI-ARI   =   1
                   IF     (WRK-COM-KZMFLG (IDX-SYOHOU)
                                                   =   SPACE) AND
                          (LNK-ORCSC-KEI-IDX       =   ZERO )
                       MOVE    "K251"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR
                                                          (IDX-SYOHOU)
                       MOVE    "1"             TO  WRK-COM-KZMFLG
                                                          (IDX-SYOHOU)
                       MOVE    IDX-SYOHOU      TO  LNK-ORCSC-KEI-IDX
                   END-IF
               END-IF
           END-IF
      *
      *H27.4
      *    低紹介率３０日以上投薬のチェック
           IF     (SPA-ERRCD           =   SPACE )
             AND  (FLG-GEN30           =   1     )
             AND ((FLG-NAIYAK2         =   1    )  OR
                  (FLG-TONPUK2         =   1    )  OR
                  (FLG-GAIYAK2         =   1    ))
             AND  (IDX-SYOHOU          >   ZERO   )
      *        ３０日以上で、処方せん料の加算なしは警告
               IF     (FLG-GEN30-K1        =   ZERO)
                 AND  (SPA-TEIRITU-KBN     =   1   )
                   IF     (WRK-COM-KZMFLG (IDX-SYOHOU)
                                                   =   SPACE) AND
                          (LNK-ORCSC-KEI-IDX       =   ZERO )
                       MOVE    "K253"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR
                                                          (IDX-SYOHOU)
                       MOVE    "1"             TO  WRK-COM-KZMFLG
                                                          (IDX-SYOHOU)
                       MOVE    IDX-SYOHOU      TO  LNK-ORCSC-KEI-IDX
                   END-IF
               END-IF
           END-IF
           IF     (SPA-ERRCD           =   SPACE )
             AND  (IDX-SYOHOU      NOT =   ZERO  )
             AND  (FLG-GEN30-K1        =   1     )
             AND ((FLG-NAIYAK2         =   1    )  OR
                  (FLG-TONPUK2         =   1    )  OR
                  (FLG-GAIYAK2         =   1    ))
      *        ３０日以上対象外で、処方せん料加算は警告
               IF      FLG-GEN30           =   ZERO
                   IF     (WRK-COM-KZMFLG (IDX-GEN30)
                                                   =   SPACE) AND
                          (LNK-ORCSC-KEI-IDX       =   ZERO )
                       MOVE    "K254"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR
                                                          (IDX-GEN30)
                       MOVE    "1"             TO  WRK-COM-KZMFLG
                                                          (IDX-GEN30)
                       MOVE    IDX-GEN30      TO  LNK-ORCSC-KEI-IDX
                   END-IF
               END-IF
           END-IF
      *
      *H29.3
      *    処方料の入力チェック
           IF     (SPA-NYUGAIKBN       =   "2" )
              AND (SPA-TEISHOHOU-FLG   =   "1" )
               PERFORM 30032-SYOHOU-INCHK-SEC
           END-IF
      *
           .
       3003-CHEK-SYORI-EXT.
           EXIT.
      *
      *H29.3
      *****************************************************************
      *   処方料の入力チェック処理
      *****************************************************************
       30032-SYOHOU-INCHK-SEC              SECTION.
      *
      *    処方料の入力チェック
           IF     (FLG-SYOHOU2         =   1 )
              AND (SPA-ERRCD           =   SPACE )
      *        院内投薬なし（処方料不可）エラー
               IF      FLG-SYOHORYO        =   ZERO
                   MOVE    "0235"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR
                                                   (IDX-SYOHOU2)
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-250IN
      *    処方料と投薬チェック
           IF     (IDX-SYOHOU2     NOT =   ZERO )
             AND  (SPA-ERRCD           =   SPACE)
               EVALUATE    WRK-COM-INPUTCD (IDX-SYOHOU2)
               WHEN    "120001210"
      *        ７種類未満
      *            ７種類以上、向精神薬多剤は警告
                   IF    ( FLG-TAZAI-ARI       =   1     )
                       MOVE    "K258"          TO  SPA-ERRCD
                   ELSE
                       IF    ((WRK-ZAICNT          >=  7    )
                          AND (FLG-CHKCARKSN       =   ZERO ))
                           MOVE    "K259"          TO  SPA-ERRCD
                       END-IF
                   END-IF
      *H31.3
      *            向精神薬長期投与　対象警告
                   IF      SPA-ERRCD           =   SPACE
                       IF     (FLG-PSYCHOKI-OK     =   1 )
                          AND (SPA-SRYYMD          >=  "20190401")
                           MOVE    "K277"          TO  SPA-ERRCD
                       END-IF
                   END-IF
      *
               WHEN    "120002610"
      *        ７種類以上
      *            ７種類以上でも向精神薬多剤は警告
                   IF    ((WRK-ZAICNT          >=  7    )
                      AND (FLG-CHKCARKSN       =   ZERO ))
                       IF    ( FLG-TAZAI-ARI       =   1     )
                           MOVE    "K258"          TO  SPA-ERRCD
                       END-IF
      *H31.3
      *                向精神薬長期投与　減算あり
                       IF     (FLG-PSYCHOKI-OK     =   1 )
                          AND (SPA-SRYYMD          >=  "20190401")
                           MOVE    1           TO  FLG-CHOKIGEN
                       END-IF
                   ELSE
      *                ７種類未満の時
                       MOVE    "K261"          TO  SPA-ERRCD
      *                地域包括診療算定時　逓減なし
                       IF      FLG-CHKCARKSN       =   1
                           MOVE    "K266"          TO  SPA-ERRCD
                       END-IF
      *H31.3
      *                向精神薬長期投与　対象警告なし
                       IF     (FLG-PSYCHOKI-OK     =   1 )
                          AND (SPA-SRYYMD          >=  "20190401")
                           MOVE    SPACE           TO  SPA-ERRCD
                           IF     (WRK-ZAICNT      >=  7    )
                               MOVE    1           TO  FLG-CHOKIGEN
                           END-IF
                       END-IF
      *                向精神薬多剤
                       IF    ( FLG-TAZAI-ARI       =   1     )
                         AND ( SPA-ERRCD           =   SPACE )
                           MOVE    "K258"          TO  SPA-ERRCD
                       END-IF
      *
                   END-IF
      *
               WHEN    CONS-TAZAI-SYOHOU
      *            向精神薬多剤
                   IF    ( FLG-TAZAI-ARI       =   ZERO    )
                       MOVE    "K260"          TO  SPA-ERRCD
                   END-IF
               END-EVALUATE
      *        湿布70枚以上
               IF     (FLG-SIP70-ARI       =   1    )
                   MOVE    "0244"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR
                                                   (IDX-SYOHOU2)
               END-IF
      *
               IF      SPA-ERRCD(1:1)      =   "K"
                   IF     (WRK-COM-KZMFLG (IDX-SYOHOU2) =   SPACE) AND
                          (LNK-ORCSC-KEI-IDX           =   ZERO )
                       MOVE    "1"             TO  WRK-COM-CUR
                                                          (IDX-SYOHOU2)
                       MOVE    "1"             TO  WRK-COM-KZMFLG
                                                          (IDX-SYOHOU2)
                       MOVE    IDX-SYOHOU2     TO  LNK-ORCSC-KEI-IDX
                   ELSE
                       MOVE    SPACE               TO  SPA-ERRCD
                       MOVE    SPACE               TO  LNK-ORCSC-ERRMSG
                   END-IF
               END-IF
      *        処方料入力あり
               IF      SPA-ERRCD           =   SPACE
                   EVALUATE    WRK-COM-INPUTCD (IDX-SYOHOU2)
                   WHEN    CONS-TAZAI-SYOHOU
      *                （向精神薬多剤投与）
                       MOVE    1                   TO  SPA-PSYCHO-CHK
                       MOVE    1                   TO  SPA-PSYCHO-OK
                       MOVE    3                   TO  FLG-250IN
                   WHEN    "120002610"
      *               （７種類以上）
                       MOVE    1                   TO  SPA-NAIFUKU-CHK
                       MOVE    1                   TO  SPA-NAIFUKU-OK
                       MOVE    1                   TO  SPA-PSYCHO-CHK
                       MOVE    ZERO                TO  SPA-PSYCHO-OK
                       MOVE    2                   TO  FLG-250IN
      *H31.3
      *                向精神薬長期投与対象で７種類未満減額なし
                       IF     (SPA-FUANCHOKI       =   1 )
                          AND (FLG-CHOKIGEN        =   ZERO)
                           MOVE    ZERO            TO  SPA-NAIFUKU-OK
                       END-IF
                   WHEN    "120001210"
      *                （その他）
                       MOVE    1                   TO  FLG-250IN
                       MOVE    1                   TO  SPA-PSYCHO-CHK
                       MOVE    1                   TO  SPA-NAIFUKU-CHK
                       MOVE    ZERO                TO  SPA-PSYCHO-OK
                       MOVE    ZERO                TO  SPA-NAIFUKU-OK
                   END-EVALUATE
               END-IF
           END-IF
      *
      *    処方料減算（低紹介率）入力チェック
           IF     (FLG-SGEM            =   1     )
              AND (SPA-ERRCD           =   SPACE )
               IF     (FLG-250IN       NOT =   ZERO )
                AND   (FLG-GEN30-N         =   1     )
                AND   (FLG-GEN30-G         =   ZERO  )
                   MOVE    1               TO  SPA-TEIGEN30-OK
                   MOVE    1               TO  SPA-TEIGEN30-CHK
               ELSE
                   MOVE    "0243"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR
                                                   (IDX-SGEM)
               END-IF
           END-IF
      *
      *    処方料麻毒加算の入力チェック
           IF     (FLG-SYOHOUMD2       =   1 )
              AND (SPA-ERRCD           =   SPACE )
      *        処方料算定不可の時、エラーとする
               IF     (FLG-SYOHORYO        =   ZERO )
                  OR  (FLG-MADOKU          =   ZERO )
                   MOVE    "0238"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR
                                                   (IDX-SYOHOUMD2)
               END-IF
           END-IF
      *    外来後発医薬品使用体制加算入力時、処方料があること
           IF     (FLG-GENKSN2         =   1     )
              AND (SPA-ERRCD           =   SPACE )
               IF      FLG-SYOHORYO        =   ZERO
                   MOVE    "0239"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-G2)
      *            処方せん料もない時、警告とする
                   IF      FLG-SYOHOSENN        =   ZERO
                       MOVE    SPACE           TO  SPA-ERRCD
                       MOVE    SPACE           TO  WRK-COM-CUR (IDX-G2)
                       IF     (WRK-COM-KZMFLG (IDX-G2) =   SPACE) AND
                              (LNK-ORCSC-KEI-IDX       =   ZERO )
                           MOVE    "K262"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-G2)
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                              (IDX-G2)
                           MOVE    IDX-G2          TO  LNK-ORCSC-KEI-IDX
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
       30032-SYOHOU-INCHK-EXT.
           EXIT.
      *
      *ver.4.7
      *****************************************************************
      *   用法まとめ毎内服種類チェック処理
      *****************************************************************
       30030-YOUHOU-HEN-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-ZAICNT-AL
           MOVE    ZERO                TO  WRK-ZAICNT2-AL
           MOVE    ZERO                TO  FLG-SYORI-END
           PERFORM
                   UNTIL      (FLG-SYORI-END   =   1  )
               INITIALIZE                  TBL-YOUHO-HEN02-AREA
               MOVE    ZERO                TO  IDG1
               MOVE    ZERO                TO  IDG2
               MOVE    ZERO                TO  IDG3
               MOVE    ZERO                TO  IDG4
               PERFORM VARYING    IDG2     FROM    1   BY  1
                       UNTIL     (IDG2    >   50  )
                   IF     (TBL-YOUHO-MAX(IDG2) >   ZERO  )
                       MOVE    TBL-YOUHO-MAX(IDG2) TO  IDG4
                       IF     ( IDG3               =   ZERO   )
      *                    用法編集
                           MOVE    TBL-YOUHO-SRYCD(IDG2 IDG4)
                                                   TO  TBL-Y2-YOUHOU
                           MOVE    TBL-YOUHO-INPUTCHI-G(IDG2 IDG4)
                                               TO  TBL-Y2-KINPUTCHI-G
                           MOVE    TBL-YOUHO-SRYSYUKBN (IDG2)
                                                   TO  TBL-Y2-SRYSYUKBN
                           MOVE    TBL-YOUHO-KAISU     (IDG2)
                                                   TO  TBL-Y2-KAISU
                           PERFORM VARYING    IDG5     FROM    1   BY  1
                                   UNTIL     (IDG5    >   50  )
                                     OR   (TBL-YOUHO-SRYCD (IDG2 IDG5)
                                                      =   SPACE )
                                     OR   (IDG3       >=  200 )
                                ADD     1               TO  IDG3
                                MOVE    TBL-YOUHO-ZAI-OCCURS (IDG2 IDG5)
                                                TO  TBL-Y2-ZAI-REC(IDG3)
                           END-PERFORM
                           MOVE    ZERO         TO  TBL-YOUHO-MAX(IDG2)
      *
                       ELSE
      *                    同一用法コード判定
                           IF     (TBL-YOUHO-SRYCD (IDG2 IDG4)(1:3)
                                                   =   "001" )
                           AND    (TBL-YOUHO-SRYCD (IDG2 IDG4)
                                                   =   TBL-Y2-YOUHOU )
                           AND    (TBL-YOUHO-INPUTCHI-G (IDG2 IDG4)
                                               =   TBL-Y2-KINPUTCHI-G)
                           AND    (TBL-YOUHO-SRYSYUKBN (IDG2)
                                               =   TBL-Y2-SRYSYUKBN  )
                           AND    (TBL-YOUHO-KAISU     (IDG2)
                                               =   TBL-Y2-KAISU      )
                           PERFORM VARYING    IDG5     FROM    1   BY  1
                                   UNTIL     (IDG5    >   50  )
                                     OR   (TBL-YOUHO-SRYCD (IDG2 IDG5)
                                                      =   SPACE )
                                     OR   (IDG3       >=  200 )
                                ADD     1               TO  IDG3
                                MOVE    TBL-YOUHO-ZAI-OCCURS (IDG2 IDG5)
                                                TO  TBL-Y2-ZAI-REC(IDG3)
                               END-PERFORM
                               MOVE    ZERO     TO  TBL-YOUHO-MAX(IDG2)
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
               IF      IDG3            =   ZERO
                   MOVE    1               TO  FLG-SYORI-END
               ELSE
                   PERFORM 300301-YOUHO-HENSUB-SEC
               END-IF
           END-PERFORM
           .
       30030-YOUHOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   処方内容チェク処理
      *****************************************************************
       300301-YOUHO-HENSUB-SEC         SECTION.
      *
           MOVE    ZERO            TO  WRK-KONGO
           MOVE    ZERO            TO  WRK-SONOTA
           MOVE    ZERO            TO  WRK-ZAITEN
           MOVE    ZERO            TO  WRK-ZAITEN-G
           PERFORM VARYING    IDG3     FROM    1   BY  1
                   UNTIL     (IDG3    >   200 )
                         OR  (TBL-Y2-SRYCD (IDG3)  =   SPACE )
           IF      TBL-Y2-SRYCD (IDG3)(1:1)    =   "6"
      *        剤数加算
               IF     (TBL-Y2-ZAIGATAKBN (IDG3)    =   1  OR
                                                       2  OR
                                                       3   )
      *            種類数指示入力なし
               AND    (TBL-Y2-TANDKKBN (IDG3)  NOT =  "1"  )
                   MOVE    1               TO  WRK-KONGO
               ELSE
      *            粉砕コードがある時、混合とする
                   IF      (IDG3            >   1   )  AND
                           (TBL-Y2-SRYCD (IDG3  - 1)
                                           =   "099209901" )
                       MOVE    1               TO  WRK-KONGO
                   ELSE
                       ADD     1               TO  WRK-SONOTA
                   END-IF
               END-IF
      *        点数
               COMPUTE WRK-SYOKEIS2        =   TBL-Y2-KISOTEN(IDG3)
                                           *   TBL-Y2-SURYO  (IDG3)
               ADD     WRK-SYOKEIS2        TO  WRK-ZAITEN-G
               MOVE    WRK-ZAITEN-G        TO  WRK-ZAITEN
            END-IF
           END-PERFORM
           IF      WRK-ZAITEN-G        <=  205
               IF      WRK-ZAITEN-G        =   ZERO
                   CONTINUE
               ELSE
                   IF      TBL-Y2-SRYSYUKBN    =   "211"
                                               OR  "291"
      *            院内
                       ADD     1               TO  WRK-ZAICNT-AL
                   ELSE
      *            院外
                       ADD     1               TO  WRK-ZAICNT2-AL
                   END-IF
      *
               END-IF
           ELSE
               IF      TBL-Y2-SRYSYUKBN    =   "211"
                                           OR  "291"
                   COMPUTE WRK-ZAICNT-AL   =   WRK-ZAICNT-AL
                                           +   WRK-KONGO
                                           +   WRK-SONOTA
               ELSE
                   COMPUTE WRK-ZAICNT2-AL  =   WRK-ZAICNT2-AL
                                           +   WRK-KONGO
                                           +   WRK-SONOTA
               END-IF
           END-IF
           .
       300301-YOUHO-HENSUB-EXT.
           EXIT.
      *ver.4.7
      *
      *
      *****************************************************************
      *   処方内容チェク処理
      *****************************************************************
       30001-TOUYAKU-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-YAKUZAI-ARI
           MOVE    ZERO                TO  FLG-SANPURU
      *
           PERFORM VARYING     IDX3    FROM    IDX   BY  1
                   UNTIL      (IDX3    >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD(IDX3) =   SPACE)  OR
                              (FLG-YAKUZAI-ARI       =   1    )
      *@
           IF      WRK-COM-HKNCOMBI (IDX3)  NOT =   "9999"
      *        薬評対象外
            AND   (WRK-COM-YAKUHYO  (IDX3)  =   SPACE  )
      *ver.4.7  減点対象外
            AND   (WRK-COM-GENTEN   (IDX3)  =   SPACE  )
      *H28.9    持参薬
            AND   (WRK-COM-JISANYAKU(IDX3)  =   SPACE  )
      *
               IF      WRK-COM-INPUTCD(IDX3)(1:1)  =   "6"
                   MOVE    1              TO  FLG-YAKUZAI-ARI
               ELSE
      *ver.4.7
      *        サンプル薬コメント
                   IF      WRK-COM-INPUTCD(IDX3)   =   "820000046"
                       MOVE    1                   TO  FLG-SANPURU
                   END-IF
      *!!
                   IF      WRK-COM-ZAIEND (IDX3) =   "1"
                       MOVE    IDX3           TO  IDX
                       MOVE    SPA-MAX-LINE   TO  IDX3
                   END-IF
               END-IF
      *@
           END-IF
           END-PERFORM
      *
           IF      FLG-YAKUZAI-ARI      =   ZERO
             AND   FLG-SANPURU          =   ZERO
               MOVE    "990"            TO  WRK-CHK-SRYSYUKBN
           END-IF
      *
           IF      FLG-YAKUZAI-ARI      =   1
               MOVE    1                TO  FLG-YAKZAI-ALL
           END-IF
      *
           .
       30001-TOUYAKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   処方内容チェク処理
      *****************************************************************
       3000-SYOHOU-CHK-SEC              SECTION.
      *
      *    複数保険判定（内服のみ）
           IF      WRK-CHK-SRYSYUKBN   =   "211"
                                       OR  "291"
      *
      *        保険毎にまとめる
               PERFORM VARYING   IDXW      FROM    1   BY  1
                       UNTIL    (IDXW      >   15  )
                   IF      WRK-HKNKEI-COMBI (IDXW) =   SPACE
                       MOVE    WRK-COM-HKNCOMBI (IDX)
                                               TO  WRK-HKNKEI-COMBI
                                                      (IDXW)
                       MOVE    WRK-COM-CHIKENKBN(IDX)
                                               TO  WRK-HKNKEI-CHIKEN
                                                      (IDXW)
                   END-IF
                   IF      WRK-HKNKEI-COMBI (IDXW)
                                       =   WRK-COM-HKNCOMBI (IDX)
                       MOVE    IDXW            TO  WRK-CNT-HKN
                       MOVE    15              TO  IDXW
                   END-IF
               END-PERFORM
           END-IF
      *
      *H24.4
           IF      WRK-COM-INPUTCD(IDX)(1:1)   =   "6"
      *
               MOVE    WRK-COM-KISOTEN(IDX)    TO  WRK-KEI-KISOTEN
      *
               EVALUATE    WRK-CHK-SRYSYUKBN
                   WHEN    "211"
                   WHEN    "221"
                   WHEN    "231"
                   WHEN    "291"
                   WHEN    "294"
                   WHEN    "297"
      *            内服　麻毒区分判定
                      IF      WRK-COM-MADOKUKBN (IDX) NOT =   ZERO
                           MOVE    1           TO  FLG-MADOKU
                           MOVE    1           TO  FLG-MADOKU-ARI
                           IF      WRK-CHK-SRYSYUKBN   =   "231"
                               IF      WRK-MADO2-HKNCOMBI  =   SPACE
                                   MOVE    WRK-COM-HKNCOMBI (IDX)
                                               TO  WRK-MADO2-HKNCOMBI
                                   MOVE    WRK-COM-CHIKENKBN(IDX)
                                               TO  WRK-MADO2-CHIKENKBN
                               END-IF
                           ELSE
                               IF      WRK-MADO1-HKNCOMBI  =   SPACE
                                   MOVE    WRK-COM-HKNCOMBI (IDX)
                                               TO  WRK-MADO1-HKNCOMBI
                                   MOVE    WRK-COM-CHIKENKBN(IDX)
                                               TO  WRK-MADO1-CHIKENKBN
                               END-IF
                           END-IF
                       END-IF
      *平成２４年４月
                   WHEN    "212"
                   WHEN    "222"
                   WHEN    "232"
                   WHEN    "292"
                   WHEN    "295"
                   WHEN    "298"
      *                院外処方　一般名記載判定
                       MOVE    ZERO                TO  FLG-CHK-P
                       IF     (WRK-COM-IPN-KISAIKBN (IDX)  =   1  )
      *H27.4               後発品変更可
                         OR   (WRK-GENECHG-KBN             =   "2" )
      *
                           MOVE    1                   TO  FLG-CHK-P
      *H27.4               後発品変更不可
                           IF      WRK-GENECHG-KBN     =   "1"
                              MOVE    ZERO                TO  FLG-CHK-P
                           END-IF
      *
                           IF      (WRK-COM-INPUTCD (IDX + 1)
                                                   =   "099209907"
                                                   OR  "099209903"  )
      *                        銘柄名記載、後発品変更不可
                               MOVE    ZERO                TO  FLG-CHK-P
                           END-IF
                       END-IF
                       IF     ( WRK-COM-INPUTCD (IDX + 1)
                                                   =   "099209908"  )
      *                 一般名記載あり
                           MOVE    1                   TO  FLG-CHK-P
                       END-IF
      *
      *R01.5
      *                同一薬剤判定
                       PERFORM 300011-IPN-JYUFU-SEC
      *R01.5
      *                同一薬剤は対象外
                       IF      FLG-IPKSNOK         =   1
      *
      *H28.4           後発品　一般名記載有無件数
                       IF      (FLG-CHK-P          =   1   )  AND
                               (WRK-COM-KOUHATUKBN (IDX) =   "1"
                                                         OR  "2" )
                           MOVE    1               TO  FLG-GENERIC-OK
                       END-IF
                       IF      (WRK-COM-KOUHATUKBN (IDX) =   "1"
                                                         OR  "2" )
                           ADD     1               TO  CNT-GENERIC
                           ADD     1               TO  CNT-GENERIC2
                           IF      (FLG-CHK-P      =   ZERO)
                               MOVE    1           TO  FLG-GENERIC-NO
                               MOVE    1           TO  FLG-GENERIC-NO2
                           ELSE
                               ADD     1           TO  CNT-GENERIC3
                           END-IF
                       END-IF
      *H29.3
      *H29.4から       先発なし後発医薬品（加算対象）判定
                       IF      (WRK-COM-KOUHATUKBN (IDX) =   "3"
                                                         OR  "4"  )
                          AND  (SPA-SRYYMD         >=   "20170317")
                           ADD     1               TO  CNT-GENERIC
                           IF      (FLG-CHK-P      =   ZERO)
                               MOVE    1           TO  FLG-GENERIC-NO
                           ELSE
                               ADD     1           TO  CNT-GENERIC3
                           END-IF
                       END-IF
      *R01.5
                       END-IF
      *
               END-EVALUATE
      *
      *H26.4
      *        うがい薬判定
               IF     WRK-COM-YAKKAKJNCD(IDX)(1:3) =   "226"
                   ADD     1                   TO  WRK-UGAI-YAK
                   IF      IDX-UGAI            =   ZERO
                       MOVE    IDX             TO  IDX-UGAI
                   END-IF
               ELSE
                   ADD     1                   TO  WRK-UGAI-IGAI
               END-IF
      *H30.4
      *        皮膚保湿薬チェック
               IF     WRK-COM-YAKKAKJNCD(IDX)(1:9) =   "2649950M1"
                                                   OR  "3334700M1"
                                                   OR  "3339950M1"
                                                   OR  "3339950N1"
                                                   OR  "3339950Q1"
                                                   OR  "3339950R1"
                                                   OR  "2649865N1"
                                                   OR  "2649865Q1"
                   ADD     1                   TO  WRK-HIFU-YAK
                   IF      IDX-HIFU            =   ZERO
                       MOVE    IDX             TO  IDX-HIFU
                   END-IF
               END-IF
      *
           END-IF
      *
      *    内服薬チェック
      *      臨時で１４日以内の時は対象外
           IF      (WRK-CHK-SRYSYUKBN (1:2)   =   "21" ) OR
                  ((WRK-CHK-SRYSYUKBN (1:2)   =   "29" ) AND
                   (WRK-COM-KAISU (IDX)       >   14   ))
               MOVE    1                   TO  FLG-NAIFUKU
      *        薬剤なし
               IF      FLG-YAKUZAI-ARI     =   ZERO
                   MOVE    ZERO                TO  FLG-NAIFUKU
               END-IF
      *H26.4
      *        外用（臨時）は対象外
               IF      (WRK-CHK-SRYSYUKBN       =   "293"
                                                OR  "294"
                                                OR  "295")
                   MOVE    ZERO                TO  FLG-NAIFUKU
               END-IF
      *H26.10
      *        頓服（臨時）は対象外
               IF      (WRK-CHK-SRYSYUKBN       =   "296"
                                                OR  "297"
                                                OR  "298")
                   MOVE    ZERO                TO  FLG-NAIFUKU
               END-IF
           ELSE
               MOVE    ZERO                TO  FLG-NAIFUKU
           END-IF
           IF      (FLG-NAIFUKU            =   1  )   AND
                   (WRK-COM-INPUTCD(IDX)(1:1)  =   "6"   )
      *        剤数加算
               IF     (WRK-COM-ZAIGATAKBN (IDX)    =   1  OR
                                                       2  OR
                                                       3   )
      *            種類数指示入力なし
               AND    (WRK-COM-TANDKKBN (IDX)  NOT =  "1"  )
                   MOVE    1               TO  WRK-KONGO
      *H28.4       向精神薬以外の集計
                   IF      WRK-COM-PSY-CLASS (IDX)     =   SPACE
                       MOVE    1               TO  WRK-KONGO-2
                   END-IF
               ELSE
      *            粉砕コードがある時、混合とする
                   IF      (IDX            >   1   )  AND
                           (WRK-COM-INPUTCD (IDX - 1)
                                           =   "099209901" )
                       MOVE    1               TO  WRK-KONGO
      *H28.4           向精神薬以外の集計
                       IF      WRK-COM-PSY-CLASS (IDX) =   SPACE
                           MOVE    1               TO  WRK-KONGO-2
                       END-IF
                   ELSE
                       ADD     1               TO  WRK-SONOTA
      *H28.4           向精神薬以外の集計
                       IF      WRK-COM-PSY-CLASS (IDX) =   SPACE
                           ADD     1               TO  WRK-SONOTA-2
                       END-IF
                   END-IF
               END-IF
      *        点数
      *H24.4
      *******  COMPUTE WRK-SYOKEIS2        =   WRK-COM-KISOTEN(IDX)
               COMPUTE WRK-SYOKEIS2        =   WRK-KEI-KISOTEN
                                           *   WRK-COM-SURYO  (IDX)
               ADD     WRK-SYOKEIS2        TO  WRK-ZAITEN-G
      *H28.4   向精神薬以外の集計
               IF      WRK-COM-PSY-CLASS (IDX) =   SPACE
                   ADD     WRK-SYOKEIS2        TO  WRK-ZAITEN-G-2
               END-IF
      *
           END-IF
      *----(01.00.01) LINE ADD START ----------------------------------
           IF      (FLG-NAIFUKU            =   1  )  AND
                   (WRK-COM-ZAIEND (IDX)   =   "1")
               MOVE    WRK-ZAITEN-G        TO  WRK-ZAITEN
               IF      WRK-ZAITEN-G        <=  205
                   IF      WRK-ZAITEN-G        =   ZERO
                       CONTINUE
                   ELSE
                       IF      WRK-CHK-SRYSYUKBN   =   "211"
                                                   OR  "291"
      *                院内
                           ADD     1               TO  WRK-ZAICNT
                       ELSE
      *                院外
                           ADD     1               TO  WRK-ZAICNT2
                       END-IF
      *
                   END-IF
               ELSE
                   IF      WRK-CHK-SRYSYUKBN   =   "211"
                                               OR  "291"
                       COMPUTE WRK-ZAICNT      =   WRK-ZAICNT
                                               +   WRK-KONGO
                                               +   WRK-SONOTA
                   ELSE
                       COMPUTE WRK-ZAICNT2     =   WRK-ZAICNT2
                                               +   WRK-KONGO
                                               +   WRK-SONOTA
                   END-IF
               END-IF
      *        点数加算
               IF      WRK-CHK-SRYSYUKBN   =   "211"
                                           OR  "291"
                   ADD     WRK-COM-KEI (IDX)   TO  WRK-KEIKEI
                   ADD     WRK-COM-KEI (IDX)   TO  WRK-HKNKEI-KEIKEI
                                                       (WRK-CNT-HKN)
               ELSE
                   ADD     WRK-COM-KEI (IDX)   TO  WRK-KEIKEI2
               END-IF
      *
               MOVE    ZERO            TO  WRK-KONGO
               MOVE    ZERO            TO  WRK-SONOTA
               MOVE    ZERO            TO  WRK-ZAITEN
               MOVE    ZERO            TO  WRK-ZAITEN-G
      *
      *H28.4   向精神薬以外の集計
               IF      WRK-ZAITEN-G-2      <=  205
                   IF      WRK-ZAITEN-G-2      =   ZERO
                       CONTINUE
                   ELSE
                       IF      WRK-CHK-SRYSYUKBN   =   "211"
                                                   OR  "291"
      *                院内
                           ADD     1               TO  WRK-ZAICNT-2
                       ELSE
      *                院外
                           ADD     1               TO  WRK-ZAICNT2-2
                       END-IF
      *
                   END-IF
               ELSE
                   IF      WRK-CHK-SRYSYUKBN   =   "211"
                                               OR  "291"
                       COMPUTE WRK-ZAICNT-2    =   WRK-ZAICNT-2
                                               +   WRK-KONGO-2
                                               +   WRK-SONOTA-2
                   ELSE
                       COMPUTE WRK-ZAICNT2-2   =   WRK-ZAICNT2-2
                                               +   WRK-KONGO-2
                                               +   WRK-SONOTA-2
                   END-IF
               END-IF
               IF     (WRK-CHK-SRYSYUKBN   =   "211"
                                           OR  "291" )
                 AND  (WRK-ZAITEN-G-2      >   ZERO  )
                   MOVE    WRK-ZAITEN-G-2      TO  WRK-ZAIKEI
                   MOVE    IDX                 TO  IDX-G
                   PERFORM 300212-ZAITENSYUKEI-SEC
                   ADD     WRK-ZAITEN-K        TO  WRK-KEIKEI-2
                   ADD     WRK-ZAITEN-K        TO  WRK-HKNKEI-KEIKEI-2
                                                       (WRK-CNT-HKN)
               END-IF
               MOVE    ZERO            TO  WRK-ZAIKEI
               MOVE    ZERO            TO  WRK-KONGO-2
               MOVE    ZERO            TO  WRK-SONOTA-2
               MOVE    ZERO            TO  WRK-ZAITEN-G-2
      *!!!!
           ELSE
               IF      (FLG-NAIFUKU            =   ZERO )
                 AND   (WRK-COM-ZAIEND (IDX)   =   "1"  )
                 AND  ((WRK-CHK-SRYSYUKBN      =   "291") AND
                       (WRK-COM-KAISU(IDX) NOT >   14   ))
      *            院内臨時投薬の１４日未満の点数集計
                   ADD     WRK-COM-KEI (IDX)   TO  WRK-KEIKEI
                   ADD     WRK-COM-KEI (IDX)   TO  WRK-HKNKEI-KEIKEI
                                                       (WRK-CNT-HKN)
      *H28.4
                   IF     (WRK-ZAITEN-G-2      >   ZERO  )
                       MOVE    WRK-ZAITEN-G-2      TO  WRK-ZAIKEI
                       MOVE    IDX                 TO  IDX-G
                       PERFORM 300212-ZAITENSYUKEI-SEC
                       ADD     WRK-ZAITEN-K        TO  WRK-KEIKEI-2
                       ADD     WRK-ZAITEN-K    TO  WRK-HKNKEI-KEIKEI-2
                                                       (WRK-CNT-HKN)
                   END-IF
               END-IF
           END-IF
      *
      *    長期投薬品チェック（内服と外用のみ）
           IF      (WRK-COM-INPUTCD(IDX)(1:1) =   "6"  )  AND
                   (WRK-CHK-SRYSYUKBN(1:2)    =   "21"
                                              OR  "23"
                                              OR  "29" )  AND
                   (WRK-COM-KAISU (IDX)       >=  28   )
               MOVE    1                   TO  FLG-CHOUKI
               IF      WRK-CHK-SRYSYUKBN(3:1)  =   "1"
      *            院内
                   MOVE    1                   TO  FLG-CHOUKI3
               ELSE
      *            院外
                   MOVE    1                   TO  FLG-CHOUKI2
               END-IF
               IF      WRK-CHOU-HKNCOMBI   =   SPACE
                   MOVE    WRK-COM-HKNCOMBI (IDX)
                                               TO  WRK-CHOU-HKNCOMBI
                   MOVE    WRK-COM-CHIKENKBN(IDX)
                                               TO  WRK-CHOU-CHIKENKBN
               END-IF
           END-IF
      *    外用の場合
           IF      (WRK-COM-INPUTCD(IDX)(1:1) =   "6"  )  AND
                  ((WRK-CHK-SRYSYUKBN (1:2)   =   "23" ) OR  
                   (WRK-CHK-SRYSYUKBN         =   "293"
                                              OR  "294"
                                              OR  "295")) AND
                   (WRK-COM-KAISU2(IDX)       >=  28   )
               MOVE    1                   TO  FLG-CHOUKI
               IF      WRK-CHK-SRYSYUKBN(3:1)  =   "1"
      *            院内
                   MOVE    1                   TO  FLG-CHOUKI3
               ELSE
      *            院外
                   MOVE    1                   TO  FLG-CHOUKI2
               END-IF
               IF      WRK-CHOU-HKNCOMBI   =   SPACE
                   MOVE    WRK-COM-HKNCOMBI (IDX)
                                               TO  WRK-CHOU-HKNCOMBI
                   MOVE    WRK-COM-CHIKENKBN(IDX)
                                               TO  WRK-CHOU-CHIKENKBN
               END-IF
           END-IF
      *
      *ver.4.7
      *    用法毎まとめ対応
           IF      (FLG-NAIFUKU        =   1  )
      *H24.8  薬剤あり
              AND  (FLG-YAKUZAI-ARI    =   1  )
              AND  (IDG1               <=  50 )
              AND  (WRK-COM-INPUTCD(IDX)   NOT =   SPACE )
               ADD     1               TO  IDG2
               IF      IDG1            =   ZERO
                   ADD     1               TO  IDG1
               END-IF
               MOVE    WRK-COM-INPUTCD(IDX)  TO  TBL-YOUHO-SRYCD
                                                          (IDG1 IDG2)
               MOVE    WRK-COM-KISOTEN(IDX)  TO  TBL-YOUHO-KISOTEN
                                                          (IDG1 IDG2)
               MOVE    WRK-COM-SURYO  (IDX)  TO  TBL-YOUHO-SURYO
                                                          (IDG1 IDG2)
               MOVE    WRK-COM-ZAIGATAKBN (IDX)
                                             TO  TBL-YOUHO-ZAIGATAKBN
                                                          (IDG1 IDG2)
               MOVE    WRK-COM-TANDKKBN(IDX) TO  TBL-YOUHO-TANDKKBN
                                                          (IDG1 IDG2)
               MOVE    WRK-COM-INPUTCHI-G(IDX)
                                             TO  TBL-YOUHO-INPUTCHI-G
                                                          (IDG1 IDG2)
               MOVE    WRK-COM-KEI (IDX)     TO  TBL-YOUHO-KEI
                                                          (IDG1 IDG2)
               IF    (WRK-COM-ZAIEND (IDX)   =   "1")
                   MOVE    IDG2              TO  TBL-YOUHO-MAX (IDG1)
                   MOVE    WRK-CHK-SRYSYUKBN TO  TBL-YOUHO-SRYSYUKBN
                                                               (IDG1)
                   MOVE    WRK-COM-KAISU (IDX)
                                             TO  TBL-YOUHO-KAISU
                                                               (IDG1)
                   ADD     1                 TO  IDG1
                   MOVE    ZERO              TO  IDG2
               END-IF
           END-IF
      *ver.4.7
      *
      *H27.4から
      *    低紹介率の病院で３０日以上の投薬
           IF      (SPA-SRYYMD         >=  "20150401")
              AND  (SPA-TEIRITU-KBN    =   1         )
              AND  (WRK-COM-ZAIEND(IDX)   =   "1"   )
              AND  (WRK-COM-KAISU (IDX)    >=  30    )
      *        対象の薬剤のみ集計
               PERFORM 300091-GEN30-HENSYU-SEC
      *
               IF      IDXG4               >   ZERO
                   IF      WRK-CHK-SRYSYUKBN   =   "211"
                                               OR  "291"
      *!頓服判定                               OR  "221"
      *!                                       OR  "297"
                       MOVE    1                   TO  FLG-GEN30
      *                院内
                       MOVE    1                   TO  FLG-GEN30-N
      *                院内投薬３０日以上点数集計
                       PERFORM 300091-GEN30-ADD-SEC
                   END-IF
                   IF      WRK-CHK-SRYSYUKBN   =   "212"
                                               OR  "292"
                       MOVE    1                   TO  FLG-GEN30
      *                院外
                       MOVE    1                   TO  FLG-GEN30-G
                   END-IF
               END-IF
           END-IF
           .
       3000-SYOHOU-CHK-EXT.
           EXIT.
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
               IF      WRK-COM-INPUTCD (IDX)
                                       =   TBL-GEN-SRYCD
                                                      (IDXG1 IDXG2)
                   MOVE    200             TO  IDXG2
                   MOVE    1               TO  FLG-IPKSNEND
               ELSE
                   IF      TBL-GEN-SRYCD (IDXG1 IDXG2)  =   SPACE
      *                テーブル編集
                       MOVE    WRK-COM-INPUTCD (IDX)
                                             TO  TBL-GEN-SRYCD
                                                      (IDXG1 IDXG2)
                       MOVE    WRK-COM-GECODE (IDX)
                                             TO  TBL-GEN-GECODE
                                                      (IDXG1 IDXG2)
                       MOVE    1             TO  FLG-IPKSNOK
                   ELSE
      *            一般名コードがあれば判定
                       IF    (WRK-COM-GECODE (IDX)
                                         NOT =   SPACE )
                         AND (WRK-COM-GECODE (IDX)
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
      *****************************************************************
      *    対象薬剤編集処理
      *****************************************************************
       300091-GEN30-HENSYU-SEC         SECTION.
      *
      *    対象の薬剤のみで剤点数を再計算
           INITIALIZE                  HZN3-SCR-REC
           MOVE    ZERO                TO  IDXG4
           MOVE    ZERO                TO  WRK-ZAITENSU
           MOVE    ZERO                TO  FLG-YAKZ-ARI
           COMPUTE IDX-STRG            =   WRK-GEN30-STR   +  1
      *H28.4
           INITIALIZE                  HZN5-SCR-REC
           MOVE    ZERO                TO  IDXG5
           INITIALIZE                  HZN6-SCR-REC
           MOVE    ZERO                TO  IDXG6
           MOVE    ZERO                TO  WRK-ZAITENSU5
           MOVE    ZERO                TO  WRK-ZAITENSU6
      *
           MOVE    ZERO                TO  WRK-IDXG3
           PERFORM VARYING     IDXG3   FROM    IDX-STRG    BY  1
                   UNTIL       IDXG3   >   IDX
               IF     (WRK-COM-INPUTCD (IDXG3)(1:1) =   "6" )
                   MOVE    IDXG3               TO  WRK-IDXG3
               END-IF
               IF      WRK-COM-INPUTCD (IDXG3) =   "099200101"
      *            ３０日以上投薬可能
                   IF      WRK-IDXG3           >   ZERO
                       MOVE    "1"             TO  WRK-COM-YOBI2
                                                       (WRK-IDXG3)(1:1)
                   END-IF
                   MOVE    ZERO                TO  WRK-IDXG3
               END-IF
           END-PERFORM
           PERFORM VARYING     IDXG3   FROM    IDX-STRG    BY  1
                   UNTIL       IDXG3   >   IDX
               IF    ((WRK-COM-INPUTCD (IDXG3)(1:1) =   "6" )
                  AND (WRK-COM-YOBI2   (IDXG3)(1:1) =   "1" ))
                  OR  (WRK-COM-INPUTCD (IDXG3) =   "099200101")
                   CONTINUE
               ELSE
                   ADD     1                   TO  IDXG4
                   MOVE    WRK-GMN-TBL (IDXG3) TO  HZN3-GMN-TBL (IDXG4)
                   IF     (WRK-COM-INPUTCD (IDXG3)(1:1) =   "6" )
                       MOVE    1               TO  FLG-YAKZ-ARI
                   END-IF
      *H28.4
                   IF     (WRK-COM-INPUTCD (IDXG3)(1:1) =   "6" )
                       IF     WRK-COM-PSY-CLASS (IDXG3) NOT =   SPACE
      *                    向精神薬
                           ADD     1                   TO  IDXG5
                           MOVE    WRK-GMN-TBL (IDXG3) TO  HZN5-GMN-TBL
                                                                (IDXG5)
                       ELSE
      *                    向精神薬以外
                           ADD     1                   TO  IDXG6
                           MOVE    WRK-GMN-TBL (IDXG3) TO  HZN6-GMN-TBL
                                                                (IDXG6)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *    薬剤なしは対象外
           IF      FLG-YAKZ-ARI        =   ZERO
               MOVE    ZERO                TO  IDXG4
           END-IF
           IF      IDXG4               >   ZERO
      *        合計点数計算
               INITIALIZE                  ORCSC90AREA
               MOVE    "2"                 TO  LNK-SC90-KBN
               MOVE    ZERO                TO  LNK-SC90-IDX
               MOVE    1                   TO  LNK-SC90-IDX-STR
               MOVE    IDXG4               TO  LNK-SC90-IDX-END
               CALL    "ORCSC90TEN2"       USING
                                           ORCSC90AREA
                                           HZN3-SCR-REC
               COMPUTE WRK-ZAITENSU        =   HZN3-COM-TENSU  (IDXG4)
                                           *   HZN3-COM-KAISU  (IDXG4)
           END-IF
      *H28.4
           IF      IDXG5               >   ZERO
      *        合計点数計算
               INITIALIZE                  ORCSC90AREA
               MOVE    "2"                 TO  LNK-SC90-KBN
               MOVE    ZERO                TO  LNK-SC90-IDX
               MOVE    1                   TO  LNK-SC90-IDX-STR
               MOVE    IDXG5               TO  LNK-SC90-IDX-END
               CALL    "ORCSC90TEN2"       USING
                                           ORCSC90AREA
                                           HZN5-SCR-REC
               COMPUTE WRK-ZAITENSU5       =   HZN5-COM-TENSU  (IDXG5)
                                           *   HZN5-COM-KAISU  (IDXG5)
           END-IF
           IF      IDXG6               >   ZERO
      *        合計点数計算
               INITIALIZE                  ORCSC90AREA
               MOVE    "2"                 TO  LNK-SC90-KBN
               MOVE    ZERO                TO  LNK-SC90-IDX
               MOVE    1                   TO  LNK-SC90-IDX-STR
               MOVE    IDXG6               TO  LNK-SC90-IDX-END
               CALL    "ORCSC90TEN2"       USING
                                           ORCSC90AREA
                                           HZN6-SCR-REC
               COMPUTE WRK-ZAITENSU6       =   HZN6-COM-TENSU  (IDXG6)
                                           *   HZN6-COM-KAISU  (IDXG6)
           END-IF
      *
           .
       300091-GEN30-HENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    院内投薬３０日以上点数集計処理
      *****************************************************************
       300091-GEN30-ADD-SEC              SECTION.
      *
           EVALUATE    WRK-CHK-SRYSYUKBN
               WHEN    "211"
               WHEN    "291"
      *            院内内服
                   ADD     WRK-ZAITENSU        TO  WRK-GEN30N-KEI
                   MOVE    ZERO                TO  WRK-GEN30N-CNT
                   PERFORM VARYING     IDXG3   FROM    1   BY  1
                           UNTIL       IDXG3   >   15
                       IF      WRK-GEN30N-COMBI (IDXG3) =   SPACE
                           MOVE    WRK-COM-HKNCOMBI (IDX)
                                               TO  WRK-GEN30N-COMBI
                                                              (IDXG3)
                           MOVE    WRK-COM-CHIKENKBN(IDX)
                                               TO  WRK-GEN30N-CHIKEN
                                                              (IDXG3)
                       END-IF
                       IF      WRK-GEN30N-COMBI (IDXG3) =
                                               WRK-COM-HKNCOMBI (IDX)
                           MOVE    IDXG3       TO  WRK-GEN30N-CNT
                           MOVE    15          TO  IDXG3
                       END-IF
                   END-PERFORM
                   ADD     WRK-ZAITENSU        TO  WRK-GEN30N-HKNKEI
                                                       (WRK-GEN30N-CNT)
      *H28.4
                   ADD     WRK-ZAITENSU5       TO  WRK-GEN30N-KEI5
                   ADD     WRK-ZAITENSU6       TO  WRK-GEN30N-KEI6
                   ADD     WRK-ZAITENSU5       TO  WRK-GEN30N-HKNKEI5
                                                       (WRK-GEN30N-CNT)
                   ADD     WRK-ZAITENSU6       TO  WRK-GEN30N-HKNKEI6
                                                       (WRK-GEN30N-CNT)
      *頓服を対象外とするなら削除
               WHEN    "221"
               WHEN    "297"
      *            院内頓服
                   ADD     WRK-ZAITENSU        TO  WRK-GEN30P-KEI
                   MOVE    ZERO                TO  WRK-GEN30P-CNT
                   PERFORM VARYING     IDXG3   FROM    1   BY  1
                           UNTIL       IDXG3   >   15
                       IF      WRK-GEN30P-COMBI (IDXG3) =   SPACE
                           MOVE    WRK-COM-HKNCOMBI (IDX)
                                               TO  WRK-GEN30P-COMBI
                                                              (IDXG3)
                           MOVE    WRK-COM-CHIKENKBN(IDX)
                                               TO  WRK-GEN30P-CHIKEN
                                                              (IDXG3)
                       END-IF
                       IF      WRK-GEN30P-COMBI (IDXG3) =
                                               WRK-COM-HKNCOMBI (IDX)
                           MOVE    IDXG3       TO  WRK-GEN30P-CNT
                           MOVE    15          TO  IDXG3
                       END-IF
                   END-PERFORM
                   ADD     WRK-ZAITENSU        TO  WRK-GEN30P-HKNKEI
                                                       (WRK-GEN30P-CNT)
      *H28.4
                   ADD     WRK-ZAITENSU5       TO  WRK-GEN30P-KEI5
                   ADD     WRK-ZAITENSU6       TO  WRK-GEN30P-KEI6
                   ADD     WRK-ZAITENSU5       TO  WRK-GEN30P-HKNKEI5
                                                       (WRK-GEN30N-CNT)
                   ADD     WRK-ZAITENSU6       TO  WRK-GEN30P-HKNKEI6
                                                       (WRK-GEN30N-CNT)
           END-EVALUATE
           .
       300091-GEN30-ADD-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    入院回数算定処理
      *****************************************************************
       3009-NYUINKAISU-SEC              SECTION.
      *
      *    調剤料算定なしの時、対象外
           IF      WRK-COM-SRYSYUKBN(IDX) =   "214"
                                          OR  "224"
                                          OR  "234"
               GO      TO      3009-NYUINKAISU-EXT
           END-IF
      *
           MOVE    1                   TO  WRK-KAISU
      *    内服は日数分作成する
           IF      WRK-COM-SRYKBN (IDX)    =   "21"
               IF      WRK-COM-KAISU (IDX)     >   WRK-KAISU
                   MOVE    WRK-COM-KAISU (IDX) TO  WRK-KAISU
               END-IF
           END-IF
      *
      *    入院回数以外
           IF       WRK-GMN-INPUTCD(IDX)(1:1)  NOT =   "*"
               MOVE    SPA-SRYYMD (7:2)    TO  IDN1
               MOVE    1                   TO  WRK-COM-NYUINDAY
                                                      (IDX IDN1)
           END-IF
      *
      *    投薬で入院回数の時、回数集計
           PERFORM VARYING     IDN1    FROM    1   BY  1
                   UNTIL       IDN1    >   31
               IF      WRK-COM-NYUINDAY (IDX IDN1) >   ZERO
                   MOVE    IDN1                TO  IDN4
                   PERFORM VARYING     IDN3    FROM    1   BY  1
                           UNTIL      (IDN3    >   WRK-KAISU )  OR
                                      (IDN4    >   31        )
                       IF      FLG-MADOKU-ARI      =   1
                           MOVE    2               TO  TBL-NYUINDAY
                                                              (IDN4)
                       ELSE
                           IF      TBL-NYUINDAY (IDN4) =   ZERO
                               MOVE    1           TO  TBL-NYUINDAY
                                                             (IDN4)
                           END-IF
                       END-IF
                       ADD     1                   TO  IDN4
      *TEST
      *TEST
      *      翌月あり
      *                IF     (IDN4                >   WRK-MATU )
      *                  AND  (WRK-KAISU           >=  IDN3     )
      *                  AND  (WRK-COM-HOUKATU-FLG (IDX)   =   1 )
      *                     MOVE    1                  TO  FLG-NEXTTUKI
      *                END-IF
      *TEST
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *TEST
      *TEST
      *    翌月あり
      *    IF      FLG-NEXTTUKI   =   1
      *       AND  (SPA-NYU-ERR        =   2          )
      *         MOVE    "XXXX"          TO  SPA-ERCD
      *     END-IF
           .
       3009-NYUINKAISU-EXT.
           EXIT.
      *
      *****************************************************************
      *    院外　算定処理
      *****************************************************************
       3001-INGAI-SYORI-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-SYOHOSENN
           MOVE    ZERO                TO  FLG-SYOHO-CHK
           MOVE    ZERO                TO  FLG-SYOHO-CHK2
      *
      *    長期投薬加算算定時、保険の変更
           IF      SPA-CHOKITOFLG          =   2
               IF     (WRK-CHOU-HKNCOMBI   NOT =   WRK-HKNCOMBI) AND
                      (WRK-CHOU-HKNCOMBI   NOT =   SPACE       )
                   MOVE    WRK-CHOU-HKNCOMBI   TO  WRK-HKNCOMBI
                   MOVE    WRK-CHOU-CHIKENKBN  TO  WRK-CHIKENKBN
               END-IF
           END-IF
           IF      WRK-SYO-HKNCOMBI    NOT =   SPACE
               MOVE    WRK-SYO-HKNCOMBI    TO  WRK-HKNCOMBI
               MOVE    WRK-SYO-CHIKENKBN   TO  WRK-CHIKENKBN
           END-IF
      *
      *    処方箋を交付する診察料を算定している場合、算定なし
           IF      (LNK-ORCSC-ROUMANSEI-ARI    =   1  )  OR
                   (LNK-ORCSC-ROUMANSEI-ARI2   >   ZERO) OR
                   (LNK-ORCSC-NETAKIRI-ARI     =   1  )  OR
                   (LNK-ORCSC-NETAKIRI-ARI2    >   ZERO) OR
                   (LNK-ORCSC-SYONIKA-ARI  NOT =   ZERO) OR
                   (LNK-ORCSC-SEIKATU-ARI2     >   ZERO) OR
                   (LNK-ORCSC-SEIKATU-ARI      >   ZERO)
      *             在宅末期医療総合診察料あり
               OR  (SPA-ZAIMATU-ARI            >   ZERO )
      *             在宅時医療総合管理料あり
               OR  (SPA-ZAIKANRI-ARI           >   ZERO)
               OR  (SPA-ZAIKANRI-ARI2          >   ZERO)
      *        特定施設入居時等医学管理料
               OR  (SPA-IGAKUKAN-ARI       NOT =   ZERO)
               OR  (SPA-IGAKUKAN-ARI2      NOT =   ZERO)
               CONTINUE
      *H24.10
      *        包括剤で算定
      *            生活習慣病指導管理料チェック
                   IF     (SPA-SEIKATU-ARI     NOT =   ZERO )  OR
                          (SPA-SEIKATU-ARI2    NOT =   ZERO )
      *H26.10          IF      SPA-HKTSANTEI-203   NOT =   ZERO
                       IF      SPA-HKTSANTEI-203       =   1
                           MOVE    1               TO  FLG-SYOHO-CHK
                       END-IF
                   END-IF
      *            小児科外来診察料チェック
                   IF      (LNK-ORCSC-SYONIKA-ARI  NOT =   ZERO)
      *H26.10          IF      SPA-HKTSANTEI-202   NOT =   ZERO
                       IF      SPA-HKTSANTEI-202       =   1
                           MOVE    1               TO  FLG-SYOHO-CHK
                       END-IF
                   END-IF
      *            在宅時医学総合管理料チェック
                   IF     (SPA-ZAIKANRI-ARI    NOT =   ZERO )  OR
                          (SPA-ZAIKANRI-ARI2   NOT =   ZERO )
      *H26.10          IF      SPA-HKTSANTEI-204   NOT =   ZERO
                       IF      SPA-HKTSANTEI-204       =   1
                           MOVE    1               TO  FLG-SYOHO-CHK
                       END-IF
                   END-IF
      *            特定施設入居時等医学管理料チェック
                   IF     (SPA-IGAKUKAN-ARI    NOT =   ZERO )  OR
                          (SPA-IGAKUKAN-ARI2   NOT =   ZERO )
      *H26.10          IF      SPA-HKTSANTEI-205   NOT =   ZERO
                       IF      SPA-HKTSANTEI-205       =   1
                           MOVE    1               TO  FLG-SYOHO-CHK
                       END-IF
                   END-IF
      *            在宅末期医療総合診察料チェック
                   IF     (SPA-ZAIMATU-ARI    NOT =   ZERO )  OR
                          (SPA-ZAIMATU-ARI2   NOT =   ZERO )
      *H26.10          IF      SPA-HKTSANTEI-206   NOT =   ZERO
                       IF      SPA-HKTSANTEI-206       =   1
                           MOVE    1               TO  FLG-SYOHO-CHK
                       END-IF
                   END-IF
      *
           ELSE
               MOVE    1                   TO  FLG-SYOHO-CHK
           END-IF
           IF    ((FLG-NAIYAK2         =   1    )  OR
                  (FLG-TONPUK2         =   1    )  OR
                  (FLG-GAIYAK2         =   1    ))   AND
                  (IDX-SYOHOU          =   ZERO )
               MOVE    1                   TO  FLG-SYOHO-CHK2
           END-IF
      *H24.8
      *    サンプル薬のみはなし
           IF     (FLG-YAKZAI-ALL      =   ZERO )
             AND  (FLG-SANPURU         =   1    )
               MOVE    ZERO                TO  FLG-SYOHO-CHK
           END-IF
      *
      *H28.4  湿布70枚以上
           IF     (FLG-SIP70-ARI       =   1    )
               MOVE    ZERO                TO  FLG-SYOHO-CHK
           END-IF
      *
      *H26.4
      *    地域包括診療算定時　逓減なし
           IF      FLG-CHKCARKSN       =   1
               IF      WRK-ZAICNT2     >=  7
                   MOVE    6               TO  WRK-ZAICNT2
               END-IF
               IF      WRK-ZAICNT2-AL  >=  7
                   MOVE    6               TO  WRK-ZAICNT2-AL
               END-IF
           END-IF
      *
      *    処方せん料算定あり
           IF     (FLG-SYOHO-CHK       =   1   )  AND
                  (FLG-SYOHO-CHK2      =   1   )
               IF     (LNK-ORCSC-KIDCD     =   SPACE )  AND
                      (SPA-CHOZAI-CHK      =   ZERO  )
      *            調基算定チェック
                   PERFORM 30019-CHOUKI-CHK-SEC
               END-IF
      *
      *H27.4
      *        低紹介率３０日以上投与
               IF     (SPA-TEIGEN30-CHK    =   ZERO )
                AND   (LNK-ORCSC-KIDCD     =   SPACE )
                AND   (SPA-ERRCD           =   SPACE )
                AND   (FLG-GEN30           =   1     )
                   MOVE    1                   TO  SPA-TEIGEN30-CHK
                   MOVE    "2007"              TO  LNK-ORCSC-KIDCD
                END-IF
      *H26.10
      *        向精神薬多剤投与の確認
               MOVE    ZERO                TO  FLG-NASI
               IF     (SPA-PSYCHO-OK       =   1     )
                 AND  (SPA-PSYCHO-CHK      =   1     )
                   MOVE    1               TO  FLG-NASI
               END-IF
               IF     (SPA-PSYCHO-CHK      =   ZERO  )
                 AND  (SPA-ERRCD           =   SPACE )
                 AND  (FLG-TAZAI-ARI       =   1     )
                 AND  (LNK-ORCSC-KIDCD     =   SPACE )
                   MOVE    1                   TO  SPA-PSYCHO-CHK
                   MOVE    "2006"              TO  LNK-ORCSC-KIDCD
               END-IF
      *
               IF     (SPA-NAIFUKU-CHK     =   ZERO  )  AND
                      (SPA-ERRCD           =   SPACE )
                 AND  (LNK-ORCSC-KIDCD     =   SPACE )
                 AND  (FLG-NASI            =   ZERO  )
      *            ７種類以上の場合、確認
                   EVALUATE    TRUE
                   WHEN   (WRK-ZAICNT2         >=  7 )  AND
                          (WRK-ZAICNT2-AL      >=  7 )
      *                内服の薬剤逓減有りチェック
                       MOVE    1               TO  SPA-NAIFUKU-CHK
                       MOVE    "2002"          TO  LNK-ORCSC-KIDCD
                   WHEN   (WRK-ZAICNT2         >=  7 )  AND
                          (WRK-ZAICNT2-AL      <   7 )
      *                用法まとめは７種類以下
                       MOVE    1               TO  SPA-NAIFUKU-CHK
                       MOVE    "2004"          TO  LNK-ORCSC-KIDCD
                   WHEN   (WRK-ZAICNT2         <   7 )  AND
                          (WRK-ZAICNT2-AL      >=  7 )
      *                用法まとめで７種類以上
                       MOVE    1               TO  SPA-NAIFUKU-CHK
                       MOVE    "2005"          TO  LNK-ORCSC-KIDCD
                       MOVE    WRK-ZAICNT2-AL  TO  WRK-ZAICNT2
                   END-EVALUATE
               ELSE
                   IF     (WRK-ZAICNT2         <   7 )  AND
                          (WRK-ZAICNT2-AL      >=  7 )
      *                用法まとめで７種類以上
                       MOVE    WRK-ZAICNT2-AL  TO  WRK-ZAICNT2
                   END-IF
               END-IF
      *
               IF     (LNK-ORCSC-KIDCD     =   SPACE )  AND
                      (SPA-ERRCD           =   SPACE )
      *            処方せん料編集
                   PERFORM 30011-SYOHOUSEN-SYORI-SEC
               ELSE
                   GO      TO      3001-INGAI-SYORI-EXT
               END-IF
           ELSE
      *        処方箋料を算定しない場合の処方箋料の有無
               IF     (FLG-SYOHO-CHK       =   1   )  AND
                      (IDX-SYOHOU      NOT =   ZERO)
                   MOVE    1               TO  FLG-SYOHOSENN
               END-IF
      *H26.10
              IF     (IDX-SYOHOU      NOT =   ZERO  )
                AND  (FLG-TAZAI1          =   1     )
                AND  (FLG-TAZAI-ARI       =   1     )
      *            多剤投与対象で、多剤処方せん料入力あり
                   MOVE    1              TO  SPA-PSYCHO-OK
                   MOVE    1              TO  SPA-PSYCHO-CHK
               END-IF
      *
           END-IF
      *
      *    主科の時のみ行う
      *****IF     (IDX-KA          NOT =   1  )  OR
      *TEST
      *    新規の時のみ行う
      *!   IF     (LNK-ORCSC-SYORIFLG  =   1  )  OR
      *    処方せん料算定時の時のみ行う
           IF     (FLG-SYOHO-CHK       =   ZERO) OR
                  (FLG-SYOHOSENN       =   ZERO)
               GO      TO      3001-INGAI-SYORI-EXT
           END-IF
      *H30.4
      *    オンライン診療料算定時なし
           IF      FLG-ONLNE-ARI       =   1
               GO      TO      3001-INGAI-SYORI-EXT
           END-IF
      *
      *    訂正で自動発生なしの場合
           IF    ((LNK-ORCSC-SYORIFLG  =   1  )  AND
                  (SPA-TEISEITOKU-FLG  =   "0") )
      *        病名疾患区分の自動発生なし
               OR (SPA-BYOMEIAUTO-FLG  NOT =   "1" )
               CONTINUE
           ELSE
      *        特定疾患処方管理加算の処理
               PERFORM 3003-INGAI-TOKUTEI-SEC
           END-IF
      *
           .
       3001-INGAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   特定疾患処方管理加算の処理
      *****************************************************************
       3003-INGAI-TOKUTEI-SEC            SECTION.
      *
      *    特定疾患処方管理加算のチェック
           IF     (FLG-SYOHOSENN       =   1  )  AND
                  (FLG-TOKUTEI         =   1  )
      *     長期投薬加算、特定疾患処方管理加算の入力が無い時
            AND   (FLG-TOKUSYO1            =   ZERO)
            AND   (FLG-CHOKIKA1            =   ZERO)
               IF      (SPA-TOKUTEIFLG         =   ZERO )  OR
                       (SPA-CHOKITOFLG         =   ZERO )
      *            算定内容チェック
                   PERFORM 30011-INGAITOKUTEIKSN-CHK-SEC
      *            確認メッセージ表示か、加算なしの時
                   IF     (LNK-ORCSC-KIDCD NOT =   SPACE )  OR
                         ((SPA-TOKUTEIFLG          =  9  ) AND
                          (SPA-CHOKITOFLG          =  9  ))
                       GO      TO      3003-INGAI-TOKUTEI-EXT
                   END-IF
               END-IF
      *        長期投薬加算発生
               IF      SPA-CHOKITOFLG          =   2
      *            長期投薬加算
                   MOVE    "120003270"         TO  WRK-SRYCD
                   MOVE    SPACE               TO  WRK-SRYCD2
                   PERFORM 3002-KASAN-SYORI-SEC
                   IF      FLG-ERR             =   ZERO
                       MOVE    "820"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                       MOVE    "80"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
      *                自動発生分としない
      *T               MOVE    "1"             TO  LNK-ORCSC-AUTOKBN
                       MOVE    "5"             TO  LNK-ORCSC-AUTOKBN
                                                         (02 IDXS)
                   END-IF
                   MOVE    1                   TO  FLG-TOKUSYO1
                   GO      TO      3003-INGAI-TOKUTEI-EXT
               END-IF
      *        特定疾患処方管理
               IF      SPA-TOKUTEIFLG          =   2
                   MOVE    "120002570"         TO  WRK-SRYCD
                   MOVE    SPACE               TO  WRK-SRYCD2
                   PERFORM 3002-KASAN-SYORI-SEC
                   IF      FLG-ERR             =   ZERO
                       MOVE    "820"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                       MOVE    "80"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
      *                自動発生分としない
      *T               MOVE    "1"             TO  LNK-ORCSC-AUTOKBN
                       MOVE    "5"             TO  LNK-ORCSC-AUTOKBN
                                                         (02 IDXS)
                   END-IF
                   MOVE    1                   TO  FLG-TOKUSYO1
               END-IF
      *
           END-IF
      *
           .
       3003-INGAI-TOKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    処方せん料編集処理
      *****************************************************************
       30011-SYOHOUSEN-SYORI-SEC            SECTION.
      *
      *    ７種類以上確認後、しない場合は回数を変更する
           IF      SPA-NAIFUKU-CHK     =   1
               IF      SPA-NAIFUKU-OK      =   ZERO
                   MOVE    6               TO  WRK-ZAICNT2
               END-IF
           END-IF
      *
      *****後発医薬品への変更判定
      *    IF      FLG-KOUHATU         =   ZERO
      *        後発医薬品への変更可があり、薬剤に変更不可がない場合
      *        IF     ((SPA-KOUHATUKA      =   "1"
      *                                    OR  "2" )  OR
      *                (FLG-KOUHATU-HEN1   =   1   )) AND
      *                (FLG-KOUHATU-HEN2   =   1   )
      *            MOVE    1               TO  FLG-KOUHATU
      *        END-IF
      **** END-IF
      *
      *    処方箋料
           IF      WRK-ZAICNT2         >=  7
      *       ７種類以上
               MOVE    "120002710"         TO  WRK-SRYCD
               MOVE    "120002910"         TO  WRK-SRYCD2
           ELSE
               MOVE    "120002910"         TO  WRK-SRYCD
               MOVE    "120002710"         TO  WRK-SRYCD2
           END-IF
      *!!
      *H31.3
      *    向精神薬長期投与処方せん料
           IF     (FLG-PSYCHOKI-OK     =   1     )
              AND (SPA-SRYYMD          >=  "20190401")
               MOVE    "120002710"         TO  WRK-SRYCD
               MOVE    "120002910"         TO  WRK-SRYCD2
           END-IF
      *!!
      *
      *H26.10
      *    向精神薬多剤投与処方せん料
           IF     (SPA-PSYCHO-OK       =   1     )
             AND  (SPA-PSYCHO-CHK      =   1     )
             AND  (FLG-TAZAI-ARI       =   1     )
               MOVE    CONS-TAZAI-SYOSEN   TO  WRK-SRYCD
               MOVE    CONS-TAZAI-SYOSEN   TO  WRK-SRYCD2
           END-IF
      *
      *H30.4
           MOVE    1                   TO  FLG-ONLNE-AUTO
      *
           PERFORM 3002-KASAN-SYORI-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    "820"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
               MOVE    "80"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
               MOVE    1               TO  FLG-SYOHOSENN
      *H27.4
      *        ３０日以上逓減
               IF     (SPA-TEIGEN30-OK     =   1 )
                 AND  (FLG-GEN30-G         =   1 )
                 AND  (FLG-SYOHOSENN       =   1 )
      *H30.4
      *          オンライン診療料算定時なし
                 AND  (FLG-ONLNE-ARI       =   ZERO )
      *
                   MOVE    CONS-TEIHP-SYOSEN   TO  WRK-SRYCD
                   MOVE    SPACE               TO  WRK-SRYCD2
                   PERFORM 30019-GEN30KSN-HENSYU-SEC
               END-IF
      *
           END-IF
      *H30.4
           MOVE    ZERO                TO  FLG-ONLNE-AUTO
      *
      *H30.4
      *    オンライン診療料算定時なし
           IF      FLG-ONLNE-ARI       =   1
               GO      TO      30011-SYOHOUSEN-SYORI-EXT
           END-IF
      *
      *H29.4 から
           IF     (FLG-ERR             =   ZERO )
            AND   (SPA-SRYYMD          >=  "20170317")
               MOVE    SPACE               TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
      *        一般名加算１算定
               IF     (FLG-GENERIC-NO      =   ZERO )
                  AND (CNT-GENERIC         >=  2    )
      *            一般名加算１対象がすべて一般名
                   MOVE    "120004270"         TO  WRK-SRYCD
                   MOVE    "120003570"         TO  WRK-SRYCD2
               ELSE
      *!!!
      *        ４月１日までは、1,2のみ一般名でも加算１
      *        IF     (SPA-SRYYMD          <   "20170401")
      *            IF    ((CNT-GENERIC2        >=  2         )
      *               AND (FLG-GENERIC-NO2     =   ZERO      ))
      *            3,4が２件以上、一般名であれば加算１
      *            OR    ((CNT-GENERIC3        >=  2         )
      *               AND (FLG-GENERIC-NO2     =   ZERO      ))
      *                MOVE    "120004270"         TO  WRK-SRYCD
      *                MOVE    "120003570"         TO  WRK-SRYCD2
      *            END-IF
      ******** END-IF
      *********IF      WRK-SRYCD           =   SPACE
                   IF      FLG-GENERIC-OK12    =   1
      *                後発品が１つ以上一般名
                       MOVE    "120003570"         TO  WRK-SRYCD
                       MOVE    "120004270"         TO  WRK-SRYCD2
                   END-IF
               END-IF
      *
      ******** END-IF
               IF      WRK-SRYCD       NOT =   SPACE
                   PERFORM 3002-KASAN-SYORI-SEC
                   IF      FLG-ERR             =   ZERO
                       MOVE    "820"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                       MOVE    "80"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
                   END-IF
               END-IF
           END-IF
      *
           .
       30011-SYOHOUSEN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    調基算定チェック処理
      *****************************************************************
       30019-CHOUKI-CHK-SEC            SECTION.
      *
           MOVE    "120001810"         TO  WRK-SRYCD
           MOVE    ZERO                TO  FLG-TENSUPLUS-CHK
           PERFORM 3001-SANTEI-CHK-SEC
           IF      FLG-ERR             =   1
               IF      SPA-CHOZAI-CHK      =   ZERO
                   MOVE    1               TO  SPA-CHOZAI-CHK
                   MOVE    "K024"          TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    "120001710"         TO  WRK-SRYCD
               MOVE    ZERO                TO  FLG-TENSUPLUS-CHK
               PERFORM 3001-SANTEI-CHK-SEC
               IF      FLG-ERR             =   1
                   IF      SPA-CHOZAI-CHK      =   ZERO
                       MOVE    1               TO  SPA-CHOZAI-CHK
                       MOVE    "K024"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
           MOVE    1                   TO  SPA-CHOZAI-CHK
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           .
       30019-CHOUKI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    院外特定疾患加算チェック処理
      *****************************************************************
       30011-INGAITOKUTEIKSN-CHK-SEC            SECTION.
      *
           IF     (SPA-CHOKITOFLG          =   2  )  OR
                  (SPA-TOKUTEIFLG          =   2  )
               GO      TO      30011-INGAITOKUTEIKSN-CHK-EXT
           END-IF
      *
           IF     (FLG-CHOUKI          =   1   )  AND
                  (SPA-CHOKITOFLG      =   ZERO)
      *        長期投薬加算チェック
               MOVE    "120003270"         TO  WRK-SRYCD
               MOVE    1                   TO  FLG-CHOKIKSN
               MOVE    WRK-SRYKA           TO  SPA-TOKUTEI-SRYKA
      *        算定可能かのチェック
               PERFORM 3001-KYOUTU-CHEK-SEC
      *        算定回数エラー
               IF     (FLG-ERR             =   ZERO )  AND
                      (WRK-ERRCD3      NOT =   SPACE)
      *            算定済み
                   MOVE    9               TO  SPA-TOKUTEIFLG
                   MOVE    9               TO  SPA-CHOKITOFLG
                   GO      TO      30011-INGAITOKUTEIKSN-CHK-EXT
               END-IF
      *        併用算定エラー
               IF     (FLG-ERR             =   ZERO )  AND
                      (WRK-ERRCD6      NOT =   SPACE)
                   IF      SPA-CHOKITOFLG-CHK  =   ZERO
                       MOVE    "K019"          TO  SPA-ERRCD
      *R01.6
                       MOVE    LNK-SC92-ERRMSG TO  LNK-ORCSC-ERRMSG
      *
                       MOVE    1               TO  SPA-CHOKITOFLG-CHK
                       MOVE    9               TO  SPA-TOKUTEIFLG
                       MOVE    9               TO  SPA-CHOKITOFLG
                   ELSE
      *                特定疾患処方管理チェック
                       PERFORM 300111-INGAITOKU-CHK-SEC
                       IF      LNK-ORCSC-KIDCD =   SPACE
                           MOVE    9               TO  SPA-TOKUTEIFLG
                           MOVE    9               TO  SPA-CHOKITOFLG
                       ELSE
                           MOVE    1               TO  SPA-CHOKITOFLG
                       END-IF
                   END-IF
                   GO      TO      30011-INGAITOKUTEIKSN-CHK-EXT
               END-IF
      *        以外のエラー
               IF      FLG-ERR             =   1
                   MOVE    9               TO  SPA-CHOKITOFLG
               ELSE
      *            算定可能
      ******       MOVE    "0130"          TO  LNK-ORCSC-KIDCD
      *            長期投薬加算警告チェック
                   PERFORM 300212-CHOKI-KEI-SEC
                   GO      TO      30011-INGAITOKUTEIKSN-CHK-EXT
               END-IF
           END-IF
      *
           IF      SPA-TOKUTEIFLG      =   ZERO
      *        特定疾患処方管理チェック
               PERFORM 300111-INGAITOKU-CHK-SEC
           END-IF
      *
           .
       30011-INGAITOKUTEIKSN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    院外特定疾患処方管理チェック処理
      *****************************************************************
       300111-INGAITOKU-CHK-SEC        SECTION.
      *
      *    特定疾患処方管理
           MOVE    "120002570"         TO  WRK-SRYCD
           MOVE    WRK-SRYKA           TO  SPA-TOKUTEI-SRYKA
      *    算定可能かのチェック
           PERFORM 3001-KYOUTU-CHEK-SEC
      *    算定エラー
           IF      FLG-ERR             =   1
               MOVE    9               TO  SPA-TOKUTEIFLG
           ELSE
      *        算定可能
      ****     MOVE    "0113"          TO  LNK-ORCSC-KIDCD
      *!!!
      *        警告チェック
               MOVE    "120002570"         TO  WRK-SRYCD
               MOVE    WRK-SRYKA           TO  SPA-TOKUTEI-SRYKA
               PERFORM 300212-KEICHK-SEC
               IF      FLG-ERR             =   1
      *            算定可能
                   MOVE    "0213"          TO  LNK-ORCSC-KIDCD
               ELSE
      *            算定可能
                   MOVE    "0113"          TO  LNK-ORCSC-KIDCD
               END-IF
           END-IF
           .
       300111-INGAITOKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    院内 算定処理
      *****************************************************************
       3002-INNAI-SYORI-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-SYOHORYO
      *
      *H26.10
      *    院内処方がなければ対象外
           IF     (FLG-NAIYAK          =   ZERO )  AND
                  (FLG-TONPUK          =   ZERO )  AND
                  (FLG-GAIYAK          =   ZERO )
               GO      TO      3002-INNAI-SYORI-EXT
           END-IF
      *
      *H27.4
      *    低紹介率３０日以上投与（院内である時）
           IF     (SPA-TEIGEN30-CHK    =   ZERO )
            AND   (LNK-ORCSC-KIDCD     =   SPACE )
            AND   (SPA-ERRCD           =   SPACE )
            AND   (FLG-GEN30-N         =   1     )
            AND   (FLG-GEN30-G         =   ZERO  )
               MOVE    1                   TO  SPA-TEIGEN30-CHK
               MOVE    "2007"              TO  LNK-ORCSC-KIDCD
               GO      TO      3002-INNAI-SYORI-EXT
            END-IF
      *
      *    院内処方のみの時
           IF     (FLG-NAIYAK2         =   1    )
             OR   (FLG-TONPUK2         =   1    )
             OR   (FLG-GAIYAK2         =   1    )
             OR   (IDX-SYOHOU      NOT =   ZERO )
               CONTINUE
           ELSE
      *    向精神薬多剤投与処理
               IF     (SPA-PSYCHO-CHK      =   ZERO  )
                 AND  (FLG-TAZAI-ARI       =   1     )
                   MOVE    1                   TO  SPA-PSYCHO-CHK
                   MOVE    "2006"              TO  LNK-ORCSC-KIDCD
                   GO      TO      3002-INNAI-SYORI-EXT
               END-IF
           END-IF
      *H26.10
      *
      *H26.4
      *    地域包括診療算定時　内服逓減なし
           IF      FLG-CHKCARKSN       =   1
               IF      WRK-ZAICNT      >=  7
                   MOVE    6               TO  WRK-ZAICNT
      *            向精神薬以外もなし
                   MOVE    6               TO  WRK-ZAICNT-2
               END-IF
           END-IF
      *
      *H26.10
      *    向精神薬多剤投与処理
           IF     (SPA-PSYCHO-OK       =   1     )
             AND  (SPA-PSYCHO-CHK      =   1     )
             AND  (FLG-TAZAI-ARI       =   1     )
               PERFORM 3002-TAZAI-GENHEN-SEC
      *        内服逓減なし
               MOVE    6               TO  WRK-ZAICNT
      *H27.4
               MOVE    1               TO  FLG-GEN30-PSY
           END-IF
      *H26.10
      *
           IF    ( WRK-ZAICNT          >=  7 )
      *H28.4
           OR    ((FLG-GEN30-PSY       =   1 )  AND
                  (WRK-ZAICNT-2        >=  7 ))
      *H29.3
      *    処方料手入力
           OR   ((FLG-250IN            =   2  )  AND
                 (FLG-CHKCARKSN        =   ZERO ))
      *H31.3
      *         向精神薬長期投与なし
      ***    AND ( FLG-PSYCHOKI-OK     =   ZERO ))
      *
      *        内服の薬剤逓減有りチェック
               IF      SPA-NAIFUKU-CHK     =   ZERO
                   MOVE    1               TO  SPA-NAIFUKU-CHK
                   MOVE    "2001"          TO  LNK-ORCSC-KIDCD
                   GO      TO      3002-INNAI-SYORI-EXT
      ****     ELSE
               END-IF
      *
               IF      SPA-NAIFUKU-OK       =   1
      *
      *           薬剤料逓減あり
                  IF    ( WRK-ZAICNT          >=  7 )
      *H29.3
      *    処方料手入力
                   OR   ( FLG-250IN            =   2  )
      *            全内服薬
                       COMPUTE WRK-SANTEN-9    =   WRK-KEIKEI
                                           *   90
                                           /   100.00
                                           +   0.5
                       MOVE    WRK-SANTEN-9    TO  WRK-SANTEN
                       PERFORM 3001-GENHEN-SEC
                  ELSE
      *H28.4       向精神薬以外
                       COMPUTE WRK-SANTEN-9    =   WRK-KEIKEI-2
                                           *   90
                                           /   100.00
                                           +   0.5
                       MOVE    WRK-SANTEN-9    TO  WRK-SANTEN
                       PERFORM 3001-GENHEN-SEC
                  END-IF
      *H27.4
                   MOVE    1               TO  FLG-GEN30-7GEN
               ELSE
      *            逓減なし
                   MOVE    6           TO  WRK-ZAICNT
               END-IF
      *****    END-IF
           END-IF
      *H27.4
      *    ３０日以上の逓減
           IF      (SPA-TEIGEN30-OK    =   1  )
               AND (SPA-TEIGEN30-CHK   =   1  )
               PERFORM 30013-GENHEN30-SYORI-SEC
           END-IF
      *
      *    麻毒の算定されている保険で作成する
           IF      WRK-MADO1-HKNCOMBI  NOT =   SPACE
      *        内服
               MOVE    WRK-MADO1-HKNCOMBI  TO  WRK-NAI-HKNCOMBI
               MOVE    WRK-MADO1-CHIKENKBN TO  WRK-NAI-CHIKENKBN
               MOVE    WRK-MADO1-HKNCOMBI  TO  WRK-HKNCOMBI
               MOVE    WRK-MADO1-CHIKENKBN TO  WRK-CHIKENKBN
           ELSE
      *        外用
               IF      WRK-MADO2-HKNCOMBI  NOT =   SPACE
                   MOVE    WRK-MADO2-HKNCOMBI  TO  WRK-GAI-HKNCOMBI
                   MOVE    WRK-MADO2-CHIKENKBN TO  WRK-GAI-CHIKENKBN
                   MOVE    WRK-MADO2-HKNCOMBI  TO  WRK-HKNCOMBI
                   MOVE    WRK-MADO2-CHIKENKBN TO  WRK-CHIKENKBN
               END-IF
           END-IF
      *    長期投薬加算算定時、保険の変更
           IF      SPA-CHOKITOFLG          =   2
               IF     (WRK-CHOU-HKNCOMBI   NOT =   WRK-HKNCOMBI) AND
                      (WRK-CHOU-HKNCOMBI   NOT =   SPACE       )
                   IF     (WRK-MADO1-HKNCOMBI      =   SPACE )  AND
                          (WRK-MADO2-HKNCOMBI      =   SPACE )
                       MOVE    WRK-CHOU-HKNCOMBI   TO  WRK-HKNCOMBI
                       MOVE    WRK-CHOU-CHIKENKBN  TO  WRK-CHIKENKBN
                   END-IF
               END-IF
           END-IF
      *
      *    院外処方が有れば自動加算をしない
           IF     (FLG-NAIYAK2         =   1    )  OR
                  (FLG-TONPUK2         =   1    )  OR
                  (FLG-GAIYAK2         =   1    )
           OR     (IDX-SYOHOU      NOT =   ZERO )
               GO      TO      3002-INNAI-SYORI-EXT
           END-IF
      *
      *H28.4  湿布70枚以上
           IF     (FLG-SIP70-ARI       =   1    )
      *        訂正で自動発生なしの場合
               IF    ((LNK-ORCSC-SYORIFLG  =   1  )  AND
                      (SPA-TEISEI-FLG      =   "0"))
      *H30.4
      *        オンライン診療料算定時なし
                 OR   (FLG-ONLNE-ARI       =   1  )
                   CONTINUE
               ELSE
      *            薬剤情報提供料、調基
                   PERFORM 30021-INNAI-AUTO1-SEC
               END-IF
               GO      TO      3002-INNAI-SYORI-EXT
           END-IF
      *
      *
      *    外来
      *    内服調剤料加算
           IF    ((FLG-NAIYAK          =   1    )  OR
                  (FLG-TONPUK          =   1    ))
      *H30.4
      *        オンライン診療料算定時　処方料以外なし
             AND  (FLG-ONLNE-ARI       =   ZERO )
      *
               MOVE    "120000710"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "241"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "21"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
                   MOVE    WRK-NAI-HKNCOMBI    TO  LNK-ORCSC-HKNCOMBI
                                                         (02 IDXS)
                   MOVE    WRK-NAI-CHIKENKBN   TO  LNK-ORCSC-CHIKENKBN
                                                         (02 IDXS)
               END-IF
           END-IF
      *    外用調剤料加算
           IF      FLG-GAIYAK          =   1
      *H30.4
      *        オンライン診療料算定時　処方料以外なし
             AND  (FLG-ONLNE-ARI       =   ZERO )
      *
               MOVE    "120001010"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "242"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "23"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
                   MOVE    WRK-GAI-HKNCOMBI    TO  LNK-ORCSC-HKNCOMBI
                                                         (02 IDXS)
                   MOVE    WRK-GAI-CHIKENKBN   TO  LNK-ORCSC-CHIKENKBN
                                                         (02 IDXS)
               END-IF
           END-IF
      *    処方料
           IF     (FLG-NAIYAK          =   1    )  OR
                  (FLG-TONPUK          =   1    )  OR
                  (FLG-GAIYAK          =   1    )
               IF      WRK-ZAICNT          >=  7
                   MOVE    "120002610"         TO  WRK-SRYCD
                   MOVE    "120001210"         TO  WRK-SRYCD2
               ELSE
                   MOVE    "120001210"         TO  WRK-SRYCD
                   MOVE    "120002610"         TO  WRK-SRYCD2
               END-IF
      *!!
      *H31.3
      *        向精神薬長期投与処方料
               IF     (FLG-PSYCHOKI-OK     =   1     )
                   MOVE    "120002610"         TO  WRK-SRYCD
                   MOVE    "120001210"         TO  WRK-SRYCD2
               END-IF
      *!!
      *H26.10
      *        向精神薬多剤投与処方料
               IF     (SPA-PSYCHO-OK       =   1     )
                 AND  (SPA-PSYCHO-CHK      =   1     )
                 AND  (FLG-TAZAI-ARI       =   1     )
                   MOVE    CONS-TAZAI-SYOHOU   TO  WRK-SRYCD
                   MOVE    CONS-TAZAI-SYOHOU   TO  WRK-SRYCD2
               END-IF
      *!!
      *H29.3
               IF      FLG-250IN           =   ZERO
      *        処方料入力なし
      *H30.4
               MOVE    1                   TO  FLG-ONLNE-AUTO
      *
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "250"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "25"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
                   MOVE    1               TO  FLG-SYOHORYO
               END-IF
               MOVE    ZERO                TO  FLG-ONLNE-AUTO
      *H27.4
      *        ３０日以上逓減
               IF     (SPA-TEIGEN30-OK     =   1 )
                 AND  (FLG-GEN30-N         =   1 )
                 AND  (FLG-SYOHORYO        =   1 )
      *H30.4
      *          オンライン診療料算定時なし
                 AND  (FLG-ONLNE-ARI       =   ZERO )
      *
                   MOVE    CONS-TEIHP-SYOHOU   TO  WRK-SRYCD
                   MOVE    SPACE               TO  WRK-SRYCD2
                   PERFORM 30019-GEN30KSN-HENSYU-SEC
               END-IF
      *!!
               ELSE
                   MOVE    1               TO  FLG-SYOHORYO
               END-IF
      *H30.4
      *        オンライン診療料算定時　処方料以外なし
               IF     (FLG-ONLNE-ARI       =   1  )
                   GO      TO      3002-INNAI-SYORI-EXT
               END-IF
      *
      *!!
      *H28.4
      *        外来後発医薬品使用体制加算（診療所）
               IF     (FLG-SYOHORYO        =   1 )
                 AND  (SPA-HOSPSBT         =   2 )
      *手入力があればなし
                 AND   (FLG-GENKSN2        =   ZERO )
                   MOVE    CONS-GEN-KSN1       TO  WRK-SRYCD
                   MOVE    SPACE               TO  WRK-SRYCD2
                   PERFORM 3002-KASAN-SYORI-SEC
                   IF      FLG-ERR             =   ZERO
                       MOVE    "250"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                       MOVE    "25"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
                   ELSE
      *                加算２
                       MOVE    CONS-GEN-KSN2       TO  WRK-SRYCD
                       MOVE    SPACE               TO  WRK-SRYCD2
                       PERFORM 3002-KASAN-SYORI-SEC
                       IF      FLG-ERR             =   ZERO
                           MOVE    "250"       TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                           MOVE    "25"        TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
                       ELSE
      *
      *                加算３
                       MOVE    CONS-GEN-KSN3       TO  WRK-SRYCD
                       MOVE    SPACE               TO  WRK-SRYCD2
                       PERFORM 3002-KASAN-SYORI-SEC
                       IF      FLG-ERR             =   ZERO
                           MOVE    "250"       TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                           MOVE    "25"        TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
                       END-IF
                       END-IF
                   END-IF
               END-IF
      *
           END-IF
      *
      *H30.4
      *    オンライン診療料算定時　処方料以外なし
           IF     (FLG-ONLNE-ARI       =   1  )
               GO      TO      3002-INNAI-SYORI-EXT
           END-IF
      *
      *    麻毒加算料
           IF      FLG-MADOKU          =   1
      *        麻毒調剤料
               MOVE    "120000110"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "260"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "26"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
               END-IF
      *        麻毒処方料
               MOVE    "120001310"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "260"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "26"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
               END-IF
           END-IF
      *
      *TEST
      *    訂正で自動発生なしの場合
           IF     (LNK-ORCSC-SYORIFLG  =   1  )  AND
                  (SPA-TEISEI-FLG      =   "0")
               CONTINUE
           ELSE
      *        薬剤情報提供料、調基
               PERFORM 30021-INNAI-AUTO1-SEC
           END-IF
      *
      *    訂正で自動発生なしの場合
           IF    ((LNK-ORCSC-SYORIFLG  =   1  )  AND
                  (SPA-TEISEITOKU-FLG  =   "0"))
      *        病名疾患区分の自動発生なし
               OR (SPA-BYOMEIAUTO-FLG  NOT =   "1" )
      *H24.8
      *    サンプル薬のみはなし
               OR (FLG-YAKZAI-ALL      =   ZERO )
               CONTINUE
           ELSE
      *        特定疾患処方管理加算
               PERFORM 30021-INNAI-TOKUTEI-SEC
           END-IF
           .
       3002-INNAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報提供料、調基算定処理
      *****************************************************************
       30021-INNAI-AUTO1-SEC     SECTION.
      *
      *    薬剤情報提供料
           IF     (FLG-NAIYAK          =   1    )  OR
                  (FLG-TONPUK          =   1    )  OR
                  (FLG-GAIYAK          =   1    )
               IF     (FLG-YAKUJYO         =   ZERO )
      *H24.8
      *    サンプル薬のみはなし
               AND    (FLG-YAKZAI-ALL      =   1    )
                   PERFORM 30022-INNAI-YAKUJYOUCHK-SEC
               END-IF
           END-IF
      *
      *H28.4  湿布70枚以上は調基なし
           IF     (FLG-SIP70-ARI       =   1    )
               GO      TO      30021-INNAI-AUTO1-EXT
           END-IF
      *
      *    調剤技術基本料（入院外）
           IF     (FLG-NAIYAK          =   1    )  OR
                  (FLG-TONPUK          =   1    )  OR
                  (FLG-GAIYAK          =   1    )
      *----(01.00.02) LINE ADD START ----------------------------------
      *           調基が１であること
             IF     (LNK-ORCSC-CHOKI-FLG   =   1    )
               AND  (FLG-CHOKI             =   ZERO )
      *----(01.00.02) LINE ADD END   ----------------------------------
               MOVE    "120001810"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               MOVE    1                   TO  FLG-ALL
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
      *            処方せん料の算定チェック
                   PERFORM 30024-SYOHOUSEN-CHK-SEC
                   IF      FLG-SYOHOU-ERR      =   ZERO
                       MOVE    "270"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                       MOVE    "27"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
                       MOVE    "4"             TO  LNK-ORCSC-AUTOKBN
                                                         (02 IDXS)
                       MOVE    1               TO  FLG-CHOKI
                   ELSE
                       INITIALIZE             LNK-ORCSC-KASN-TBL2
                                                         (02 IDXS)
                       COMPUTE IDXS            =   IDXS   -   1
                   END-IF
               END-IF
             END-IF
           END-IF
           .
       30021-INNAI-AUTO1-EXT.
           EXIT.
      *****************************************************************
      *    特定疾患処方管理加算算定処理
      *****************************************************************
       30021-INNAI-TOKUTEI-SEC     SECTION.
      *
      *    特定疾患処方管理加算のチェック
           IF     (FLG-SYOHORYO        =   1  )  AND
                  (FLG-TOKUTEI         =   1  )
      *     長期投薬加算、特定疾患処方管理加算の入力が無い時
            AND   (FLG-TOKUSYO2            =   ZERO)
            AND   (FLG-CHOKIKA2            =   ZERO)
               IF      (SPA-TOKUTEIFLG         =   ZERO )  OR
                       (SPA-CHOKITOFLG         =   ZERO )
      *            算定内容チェック
                   PERFORM 30021-INNAITOKUTEIKSN-CHK-SEC
      *            確認メッセージ表示か、加算なしの時
                   IF     (LNK-ORCSC-KIDCD NOT =   SPACE )  OR
                         ((SPA-TOKUTEIFLG          =  9  ) AND
                          (SPA-CHOKITOFLG          =  9  ))
                       GO      TO      30021-INNAI-TOKUTEI-EXT
                   END-IF
               END-IF
      *        長期投薬加算発生
               IF      SPA-CHOKITOFLG          =   2
      *            長期投薬加算
                   MOVE    "120003170"         TO  WRK-SRYCD
                   MOVE    SPACE               TO  WRK-SRYCD2
                   PERFORM 3002-KASAN-SYORI-SEC
                   IF      FLG-ERR             =   ZERO
                       MOVE    "250"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                       MOVE    "25"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
                       MOVE    WRK-CHOU-HKNCOMBI
                                               TO  LNK-ORCSC-HKNCOMBI
                                                         (02 IDXS)
                       MOVE    WRK-CHOU-CHIKENKBN
                                               TO  LNK-ORCSC-CHIKENKBN
                                                         (02 IDXS)
      *                自動発生分としない
                       MOVE    "5"             TO  LNK-ORCSC-AUTOKBN
                                                         (02 IDXS)
                   END-IF
                   MOVE    1                   TO  FLG-TOKUSYO2
                   GO      TO      30021-INNAI-TOKUTEI-EXT
               END-IF
      *        特定疾患処方管理
               IF      SPA-TOKUTEIFLG          =   2
                   MOVE    "120002270"         TO  WRK-SRYCD
                   MOVE    SPACE               TO  WRK-SRYCD2
                   PERFORM 3002-KASAN-SYORI-SEC
                   IF      FLG-ERR             =   ZERO
                       MOVE    "250"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                       MOVE    "25"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
      *                自動発生分としない
                       MOVE    "5"             TO  LNK-ORCSC-AUTOKBN
                                                         (02 IDXS)
                   END-IF
                   MOVE    1                   TO  FLG-TOKUSYO2
               END-IF
      *
           END-IF
           .
       30021-INNAI-TOKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    処方せん料の算定チェック処理
      *****************************************************************
       30024-SYOHOUSEN-CHK-SEC     SECTION.
      *
           IF      SPA-SYOHOU-CHK      =   1
               MOVE    1                   TO  FLG-SYOHOU-ERR
           ELSE
               MOVE    ZERO                TO  FLG-SYOHOU-ERR
           END-IF
      *
           IF      FLG-SYOHOU-ERR      =   ZERO
               MOVE    "120002710"         TO  WRK-SRYCD
               PERFORM 300241-SRYACT-CHK-SEC
           END-IF
      *
           IF      FLG-SYOHOU-ERR      =   ZERO
               MOVE    "120002910"         TO  WRK-SRYCD
               PERFORM 300241-SRYACT-CHK-SEC
           END-IF
      *
      *H26.10
      *    向精神薬多剤投与処方せん料
           IF      FLG-SYOHOU-ERR      =   ZERO
               MOVE    CONS-TAZAI-SYOSEN   TO  WRK-SRYCD
               PERFORM 300241-SRYACT-CHK-SEC
           END-IF
      *
           IF      FLG-SYOHOU-ERR  NOT =   ZERO
               IF      SPA-SYOHOU-CHK      =   ZERO
                   MOVE    1               TO  SPA-SYOHOU-CHK
      *??          MOVE    "K023"          TO  SPA-ERRCD
               END-IF
      **       MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           END-IF
           .
       30024-SYOHOUSEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    院内薬剤情報提供料処理
      *****************************************************************
       300241-SRYACT-CHK-SEC     SECTION.
      *
           MOVE    SPA-SRYYMD(7:2)     TO  IDN1
      *
           MOVE    ZERO                TO  WRK-TEI-CNT
           PERFORM VARYING     IDT     FROM    1   BY  1
                   UNTIL      (IDT     >   SPA-MAX-LINE )  OR
                              (LNK-ORCSC-TEI-SRYCD(IDT)
                                           =   SPACE )
               IF      WRK-SRYCD       =   LNK-ORCSC-TEI-SRYCD(IDT)
                   ADD     LNK-ORCSC-TEI-KAISU(IDT)
                                           TO  WRK-TEI-CNT
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                      SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    "2"                 TO  SRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    SPA-SRYYMD(1:6)     TO  SRY-SRYYM
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
               PERFORM 900-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL     (FLG-SRYACT   =   1 )  OR
                             (FLG-SYOHOU-ERR   =   1   )
      *        診療会計マスタ読み込み
               MOVE    SPACE               TO  SRYACCT-REC
               INITIALIZE                      SRYACCT-REC
               MOVE    SRY-HOSPNUM         TO  ACCT-HOSPNUM
               MOVE    SRY-NYUGAIKBN       TO  ACCT-NYUGAIKBN
               MOVE    SRY-PTID            TO  ACCT-PTID
               MOVE    SRY-SRYKA           TO  ACCT-SRYKA
               MOVE    SRY-SRYYM           TO  ACCT-SRYYM
               MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
               MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
      *
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 940-SRYACCT-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-SRYACCT
                   INITIALIZE                      SRYACCT-REC
               END-IF
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               PERFORM VARYING     IDX2    FROM    1   BY  1
      *!!!!!!!!!!!     UNTIL      (IDX2        >   IDN1  )  OR
      *            当月内で算定
                       UNTIL      (IDX2        >   31    )  OR
                                  (FLG-SYOHOU-ERR  =   1 )
      *            当日以前に算定
                   IF      IDN1            =   IDX2
                       IF      WRK-TEI-CNT     <=   ACCT-DAY (1 IDX2)
                           COMPUTE ACCT-DAY (1 IDX2)
                                                   =   ACCT-DAY (1 IDX2)
                                                   -   WRK-TEI-CNT
                       ELSE
                           MOVE    ZERO            TO  ACCT-DAY (1 IDX2)
                       END-IF
                   END-IF
                   IF      ACCT-DAY (1 IDX2)   >   ZERO
                       MOVE    1               TO  FLG-SYOHOU-ERR
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       300241-SRYACT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    院内薬剤情報提供料処理
      *****************************************************************
       30022-INNAI-YAKUJYOUCHK-SEC     SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *    老人区分判定
           IF      LNK-ORCSC-ROUJIN    =   0
               IF      LNK-ORCSC-YAKJYO-1      =    1
                                               OR   2
                    MOVE    "120002370"         TO  WRK-SRYCD
                    MOVE    SPACE               TO  WRK-SRYCD2
               END-IF
           ELSE
               MOVE    SPACE               TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               IF      FLG-YAKUJYO         =   ZERO
      *            いずれかの区分が自動発生ありの時
                   IF     (LNK-ORCSC-YAKJYO-2      =    1
                                                   OR   2  )
                   OR     (LNK-ORCSC-YAKJYO-3      =    1
                                                   OR   2  )
                       MOVE    "120002370"         TO  WRK-SRYCD
                       MOVE    SPACE               TO  WRK-SRYCD2
                   END-IF
               END-IF
           END-IF
      *
           IF      WRK-SRYCD           =   SPACE
               GO      TO      30022-INNAI-YAKUJYOUCHK-EXT
           END-IF
           MOVE    1                   TO  FLG-ALL
           PERFORM 3003-YAKJYO-CHK-SEC
           MOVE    ZERO                TO  FLG-ALL
           IF      FLG-ERR             =   ZERO 
           AND     LNK-ORCSC-KIDCD     =   SPACE
               MOVE    "130"           TO  LNK-ORCSC-SRYSYUKBN(02 IDXS)
               MOVE    "13"            TO  LNK-ORCSC-SRYKBN   (02 IDXS)
               MOVE    1               TO  FLG-YAKUJYO
      *        自動発生分としない
               MOVE    "6"             TO  LNK-ORCSC-AUTOKBN
                                                         (02 IDXS)
           END-IF
           .
       30022-INNAI-YAKUJYOUCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    院内特定疾患加算チェック処理
      *****************************************************************
       30021-INNAITOKUTEIKSN-CHK-SEC            SECTION.
      *
           IF     (SPA-CHOKITOFLG          =   2   )  OR
                  (SPA-TOKUTEIFLG          =   2  )
               GO      TO      30021-INNAITOKUTEIKSN-CHK-EXT
           END-IF
      *
           IF     (FLG-CHOUKI          =   1   )  AND
                  (SPA-CHOKITOFLG      =   ZERO)
      *        長期投薬加算チェック
               MOVE    "120003170"         TO  WRK-SRYCD
      *        算定可能かのチェック
               MOVE     1                  TO  FLG-CHOKIKSN
               MOVE    WRK-SRYKA           TO  SPA-TOKUTEI-SRYKA
               PERFORM 3001-KYOUTU-CHEK-SEC
      *        算定回数エラー
               IF     (FLG-ERR             =   ZERO )  AND
                      (WRK-ERRCD3      NOT =   SPACE)
      *            算定済み
                   MOVE    9               TO  SPA-TOKUTEIFLG
                   MOVE    9               TO  SPA-CHOKITOFLG
                   GO      TO      30021-INNAITOKUTEIKSN-CHK-EXT
               END-IF
      *        併用算定エラー
               IF     (FLG-ERR             =   ZERO )  AND
                      (WRK-ERRCD6      NOT =   SPACE)
                   IF      SPA-CHOKITOFLG-CHK  =   ZERO
                       MOVE    "K019"          TO  SPA-ERRCD
      *R01.6
                       MOVE    LNK-SC92-ERRMSG TO  LNK-ORCSC-ERRMSG
      *H25.1
      *                手帳加算確認は警告の後表示
                       IF      LNK-ORCSC-KIDCD     =   "2003"
                           MOVE    SPACE       TO  LNK-ORCSC-KIDCD
                       END-IF
      *!!!
                       MOVE    1               TO  SPA-CHOKITOFLG-CHK
                       MOVE    9               TO  SPA-TOKUTEIFLG
                       MOVE    9               TO  SPA-CHOKITOFLG
                   ELSE
      *                特定疾患処方管理チェック
                       PERFORM 300211-INNAITOKU-CHK-SEC
                       IF      LNK-ORCSC-KIDCD =   SPACE
                           MOVE    9               TO  SPA-TOKUTEIFLG
                           MOVE    9               TO  SPA-CHOKITOFLG
                       ELSE
                           MOVE    1               TO  SPA-CHOKITOFLG
                       END-IF
                   END-IF
                   GO      TO      30021-INNAITOKUTEIKSN-CHK-EXT
               END-IF
      *        以外のエラー
               IF      FLG-ERR             =   1
                   MOVE    9               TO  SPA-CHOKITOFLG
               ELSE
      *            算定可能
      *****        MOVE    "0130"          TO  LNK-ORCSC-KIDCD
      *            長期投薬加算警告チェック
                   PERFORM 300212-CHOKI-KEI-SEC
                   GO      TO      30021-INNAITOKUTEIKSN-CHK-EXT
               END-IF
           END-IF
      *
           IF      SPA-TOKUTEIFLG      =   ZERO
      *        特定疾患処方管理
               PERFORM 300211-INNAITOKU-CHK-SEC
           END-IF
      *
           .
       30021-INNAITOKUTEIKSN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    院内特定疾患処方管理チェック処理
      *****************************************************************
       300211-INNAITOKU-CHK-SEC            SECTION.
      *
      *    特定疾患処方管理
           MOVE    "120002270"         TO  WRK-SRYCD
           MOVE    WRK-SRYKA           TO  SPA-TOKUTEI-SRYKA
      *    算定可能かのチェック
           PERFORM 3001-KYOUTU-CHEK-SEC
      *    算定エラー
           IF      FLG-ERR             =   1
               MOVE    9               TO  SPA-TOKUTEIFLG
           ELSE
      *!!!
      *        警告チェック
               MOVE    "120002270"         TO  WRK-SRYCD
               MOVE    WRK-SRYKA           TO  SPA-TOKUTEI-SRYKA
               PERFORM 300212-KEICHK-SEC
               IF      FLG-ERR             =   1
      *            算定可能
                   MOVE    "0213"          TO  LNK-ORCSC-KIDCD
               ELSE
      *            算定可能
                   MOVE    "0113"          TO  LNK-ORCSC-KIDCD
               END-IF
           END-IF
           .
       300211-INNAITOKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    長期投薬加算警告チェック処理
      *****************************************************************
       300212-KEICHK-SEC              SECTION.
      *
           MOVE    WRK-SCR-REC         TO  HOZ-SCR-REC
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "3"                 TO  LNK-SC92-KBN
      *
           COMPUTE LNK-SC92-GMN-IDX    =   WRK-GMN-MAX
                                       +   1
           IF      LNK-SC92-GMN-IDX    >   SPA-MAX-LINE
               MOVE    SPA-MAX-LINE        TO  LNK-SC92-GMN-IDX
           END-IF
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKA
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           MOVE    LNK-ORCSC-KT01-AREA
                                       TO  LNK-SC92-KT01-AREA
           MOVE    "1"                 TO  LNK-SC92-KEIFLG
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       HOZ-SCR-REC
                                       SPA-AREA
      *    エラーの時、追加なし
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
      *        追加なし
               MOVE    1               TO  FLG-ERR
           ELSE
      *        追加あり
               MOVE    ZERO            TO  FLG-ERR
           END-IF
           .
       300212-KEICHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    長期投薬加算警告チェック処理
      *****************************************************************
       300212-CHOKI-KEI-SEC              SECTION.
      *
      *    長期投薬加算警告チェック
           MOVE    "120003170"         TO  WRK-SRYCD
           MOVE    WRK-SRYKA           TO  SPA-TOKUTEI-SRYKA
           PERFORM 300212-KEICHK-SEC
           IF      FLG-ERR             =   1
               MOVE    "0230"          TO  LNK-ORCSC-KIDCD
           ELSE
               MOVE    "0130"          TO  LNK-ORCSC-KIDCD
           END-IF
           .
       300212-CHOKI-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤料逓減自動算定処理
      *****************************************************************
       3001-GENHEN-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-ZAI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                             ((WRK-GMN-INPUTCD(IDX) =   SPACE)  AND
                              (WRK-COM-INPUTCD(IDX) =   SPACE ))
                       OR    ( SPA-ERRCD       NOT  =   SPACE )
      *
             IF     (WRK-COM-SRYKA(IDX)    =   WRK-SRYKA  )
                AND (WRK-COM-HKNCOMBI(IDX) NOT =   "9999" )
      *        薬評対象外
               AND  (WRK-COM-YAKUHYO (IDX) =   SPACE  )
      *ver.4.7  減点対象外
               AND  (WRK-COM-GENTEN (IDX)  =   SPACE  )
      *
      *        診療種別区分変換
               PERFORM 30001-SRYSYUKBN-SET-SEC
      *        院内の内服薬チェック
      *          臨時投薬もすべて対象
      ********** 臨時で１４日以内の時は対象外
               IF     ( WRK-CHK-SRYSYUKBN      =   "211" )  OR
                      ( WRK-CHK-SRYSYUKBN      =   "291" )
      *!!             ((WRK-CHK-SRYSYUKBN      =   "291") AND
      *!!!             (WRK-COM-KAISU (IDX)    >   14   ))
                   MOVE    1                   TO  FLG-NAIFUKU
               ELSE
                   MOVE    ZERO                TO  FLG-NAIFUKU
               END-IF
      *        点数ゼロ対象外（サ）のみの剤等
               IF      FLG-NAIFUKU             =   1
      *            薬剤料逓減チェック処理
                AND   (FLG-ZAI                 =   ZERO  )
                   PERFORM 300110-GENTUIKA-CHK-SEC
               END-IF
      *
               IF     (FLG-NAIFUKU             =   1     )  AND
                      (FLG-ZAI                 =   ZERO  )
                   IF      WRK-GMN-INPUTCD(IDX)(1:1)   =   "."
      *                約束セットの時、約束セットの後に追加
                       IF      WRK-GMN-INPUTCD(IDX + 1)(1:1)   =   "S"
                           COMPUTE IDX-Z           =   IDX     +   2
                       ELSE
                           COMPUTE IDX-Z           =   IDX     +   1
                       END-IF
                   ELSE
                       MOVE    IDX             TO  IDX-Z
                   END-IF
      *H28.4
      *            （精減）の後
                   IF      WRK-COM-INPUTCD (IDX-Z) =   CONS-SGEN-SRYCD
                       ADD     1               TO  IDX-Z
                   END-IF
                   MOVE    "820000047"     TO  WRK-SRYCD
                   PERFORM 30011-GENTUIKA-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    "1"             TO  WRK-COM-CUR (IDX)
                   END-IF
                   MOVE    1               TO  FLG-ZAI
               END-IF
               MOVE    1               TO  FLG-ZAI
             END-IF
      *
               IF      WRK-COM-ZAIEND (IDX)    =   "1"
                   MOVE    ZERO            TO  FLG-ZAI
               END-IF
      *
           END-PERFORM
      *
           MOVE    ZERO                TO  WRK-GMN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                             ((WRK-GMN-INPUTCD(IDX) =   SPACE ) AND
                              (WRK-COM-INPUTCD(IDX) =   SPACE ))
               ADD     1               TO  WRK-GMN-MAX
           END-PERFORM
      *
      *    薬剤料逓減
           MOVE    ZERO                TO  WRK-SANTEN-G
           COMPUTE WRK-SANTEN-KEI      =   WRK-KEIKEI
                                       -   WRK-SANTEN
      *H28.4
           IF      FLG-GEN30-PSY       =   1
               COMPUTE WRK-SANTEN-KEI      =   WRK-KEIKEI-2
                                           -   WRK-SANTEN
           END-IF
      *
      *    複数保険最終行
           MOVE    ZERO                TO  WRK-CNT-HKN
           PERFORM VARYING IDX-GEN     FROM    1   BY  1
                   UNTIL  (IDX-GEN         >   15 )
               IF      WRK-HKNKEI-COMBI (IDX-GEN)
                                       NOT =   SPACE
                   MOVE    IDX-GEN         TO  WRK-CNT-HKN
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX-GEN     FROM    1   BY  1
                   UNTIL   IDX-GEN     >   WRK-CNT-HKN
               IF      IDX-GEN         =   WRK-CNT-HKN
                   COMPUTE WRK-SANTENSU    =   WRK-SANTEN-KEI
                                           -   WRK-SANTEN-G
               ELSE
                   COMPUTE WRK-SANTEN-9    =   WRK-HKNKEI-KEIKEI
                                                       (IDX-GEN)
                                           *   90
                                           /   100.00
                                           +   0.5
                   MOVE    WRK-SANTEN-9    TO  WRK-SANTEN-GEN
                   COMPUTE WRK-SANTENSU    =   WRK-HKNKEI-KEIKEI
                                                       (IDX-GEN)
                                           -   WRK-SANTEN-GEN
      *H28.4
                   IF      FLG-GEN30-PSY       =   1
                       MOVE    ZERO            TO  WRK-SANTEN-9
                       COMPUTE WRK-SANTEN-9    =   WRK-HKNKEI-KEIKEI-2
                                                       (IDX-GEN)
                                               *   90
                                               /   100.00
                                               +   0.5
                       MOVE    WRK-SANTEN-9    TO  WRK-SANTEN-GEN
                       COMPUTE WRK-SANTENSU    =   WRK-HKNKEI-KEIKEI-2
                                                       (IDX-GEN)
                                               -   WRK-SANTEN-GEN
                   END-IF
      *
                   ADD     WRK-SANTENSU    TO  WRK-SANTEN-G
               END-IF
      *
               MOVE    "630010002"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "210"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "21"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
                   MOVE    WRK-HKNKEI-COMBI (IDX-GEN)
                                           TO  LNK-ORCSC-HKNCOMBI
                                                         (02 IDXS)
                   MOVE    WRK-HKNKEI-CHIKEN(IDX-GEN)
                                           TO  LNK-ORCSC-CHIKENKBN
                                                         (02 IDXS)
               END-IF
           END-PERFORM
           .
       3001-GENHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤料逓減チェック処理
      *****************************************************************
       300110-GENTUIKA-CHK-SEC         SECTION.
      *
      *
           MOVE    ZERO                TO  FLG-ZAIEND2
           MOVE    ZERO                TO  FLG-ARI2
           PERFORM VARYING     IDX-G   FROM    IDX BY  1
                   UNTIL      (IDX-G   >   SPA-MAX-LINE  )
                       OR    ( FLG-ZAIEND2     =   1     )
      *H28.4
      *        向精神薬以外あり
               IF     (WRK-COM-INPUTCD (IDX-G)(1:1) =  "6")
                  AND (WRK-COM-PSY-CLASS (IDX-G)    =  SPACE)
                   MOVE    1               TO  FLG-ARI2
               END-IF
      *
               IF      WRK-COM-ZAIEND (IDX-G)  =   "1"
                   MOVE    1               TO  FLG-ZAIEND2
                   IF     (WRK-COM-KEI (IDX-G) =   ZERO   )
      *                剤点数がゼロの時、対象外
                       MOVE    ZERO            TO  FLG-NAIFUKU
                   END-IF
      *H28.4
                   IF    ( WRK-ZAICNT          >=  7 )
      *H29.3
      *    処方料手入力
                   OR   ( FLG-250IN            =   2  )
      *????
                       CONTINUE
                   ELSE
                       IF      FLG-ARI2        =   ZERO
      *                向精神薬のみは対象外
                           MOVE    ZERO            TO  FLG-NAIFUKU
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .
       300110-GENTUIKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤料逓減追加処理
      *****************************************************************
       30011-GENTUIKA-SEC         SECTION.
      *
      *H27.4
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-T
           PERFORM VARYING     IDX1    FROM    IDX-Z   BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE   )   OR
                              (FLG-ARI                 =   1  ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
      *H29.7
                         OR   (WRK-COM-ZAIEND(IDX1)    =   "1"  )
               IF       WRK-COM-INPUTCD (IDX1) =   WRK-SRYCD
                    MOVE    IDX1           TO  IDX-T
                    MOVE    1              TO  FLG-ARI
               END-IF
           END-PERFORM
      *
      *    診療区分（挿入下と同じ）
           MOVE    WRK-COM-SRYKBN (IDX)    TO  WRK-HEN-SRYKBN
           MOVE    WRK-COM-SRYSYUKBN(IDX)  TO  WRK-HEN-SRYSYUKBN
      *
      *H27.4
           IF      FLG-ARI             =   1
               MOVE    IDX-T               TO  IDX-Z
           END-IF
           MOVE    IDX-Z               TO  IDX-S
      *
           MOVE    WRK-SCR-REC         TO  HOZ-SCR-REC
      *
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           IF      WRK-COM-INPUTCD (IDX-Z) =   WRK-SRYCD
               MOVE    SPACE               TO  LNK-SC92-KBN
           ELSE
               MOVE    "1"                 TO  LNK-SC92-KBN
           END-IF
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI   TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           MOVE    LNK-ORCSC-KT01-AREA
                                       TO  LNK-SC92-KT01-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    エラーの時、追加なし
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               IF      SPA-ERRCD           =   "0020"  OR  "0007"
                   MOVE    "7007"              TO  SPA-ERRCD
               ELSE
                   MOVE    SPACE               TO  SPA-ERRCD
               END-IF
               MOVE    HOZ-SCR-REC         TO  WRK-SCR-REC
               GO      TO      30011-GENTUIKA-EXT
           END-IF
      *
           IF     (IDX-S               =   SPA-MAX-LINE)  OR
                  (WRK-COM-SRYKBN (IDX-S + 1)(1:1) NOT =   "2")
               MOVE    WRK-HEN-SRYKBN      TO  WRK-COM-SRYKBN(IDX-S)
               MOVE    WRK-HEN-SRYKBN      TO  WRK-GMN-SRYKBN(IDX-S)
               MOVE    WRK-HEN-SRYSYUKBN   TO  WRK-COM-SRYSYUKBN(IDX-S)
               MOVE    WRK-SRYKA           TO  WRK-COM-SRYKA    (IDX-S)
               MOVE    WRK-HKNCOMBI        TO  WRK-COM-HKNCOMBI (IDX-S)
               MOVE    WRK-CHIKENKBN       TO  WRK-COM-CHIKENKBN(IDX-S)
           ELSE
               MOVE    WRK-HEN-SRYKBN      TO  WRK-COM-SRYKBN(IDX-S)
               MOVE    SPACE               TO  WRK-GMN-SRYKBN(IDX-S)
               MOVE    WRK-HEN-SRYSYUKBN   TO  WRK-COM-SRYSYUKBN(IDX-S)
               MOVE    WRK-COM-SRYKA     (IDX-S + 1)
                                           TO  WRK-COM-SRYKA    (IDX-S)
               MOVE    WRK-COM-HKNCOMBI  (IDX-S + 1)
                                           TO  WRK-COM-HKNCOMBI (IDX-S)
               MOVE    WRK-COM-CHIKENKBN  (IDX-S + 1)
                                           TO  WRK-COM-CHIKENKBN(IDX-S)
           END-IF
      *
      *    自動追加分（入力時に追加）
           MOVE    "3"                 TO  WRK-COM-AUTOKBN (IDX-S)
      *
           .
       30011-GENTUIKA-EXT.
           EXIT.
      *
      *H26.10
      *****************************************************************
      *    向精神薬多剤投与算定処理
      *****************************************************************
       3002-TAZAI-GENHEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ZAI
           INITIALIZE                  WRK-HKNKEI22-G
           INITIALIZE                  WRK-HKNKEI23-G
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                             ((WRK-GMN-INPUTCD(IDX) =   SPACE)  AND
                              (WRK-COM-INPUTCD(IDX) =   SPACE ))
                       OR    ( SPA-ERRCD       NOT  =   SPACE )
      *
             IF     (WRK-COM-SRYKA(IDX)    =   WRK-SRYKA  )
               AND  (WRK-COM-HKNCOMBI(IDX) NOT =   "9999" )
      *        薬評対象外
               AND  (WRK-COM-YAKUHYO (IDX) =   SPACE  )
      *        減点対象外
               AND  (WRK-COM-GENTEN (IDX)  =   SPACE  )
      *
      *        診療種別区分変換
               PERFORM 30001-SRYSYUKBN-SET-SEC
      *        すべての院内投薬を対象とする
               IF     (WRK-CHK-SRYSYUKBN   =   "211"
                                           OR  "291"
                                           OR  "221"
                                           OR  "231"
                                           OR  "294"
                                           OR  "297" )
               AND    (FLG-ZAI             =   ZERO  )
                   MOVE    1               TO  FLG-NAIFUKU
      *            薬剤料逓減チェック処理
                   PERFORM 30021-TAZAIGENTUIKA-CHK-SEC
               ELSE
                   MOVE    ZERO            TO  FLG-NAIFUKU
               END-IF
      *
               IF     (FLG-NAIFUKU             =   1     )  AND
                      (FLG-ZAI                 =   ZERO  )
                   IF      WRK-GMN-INPUTCD(IDX)(1:1)   =   "."
      *                約束セットの時、約束セットの後に追加
                       IF      WRK-GMN-INPUTCD(IDX + 1)(1:1)   =   "S"
                           COMPUTE IDX-Z           =   IDX     +   2
                       ELSE
                           COMPUTE IDX-Z           =   IDX     +   1
                       END-IF
                   ELSE
                       MOVE    IDX             TO  IDX-Z
                   END-IF
      *H28.4
      *            （減）の後（湿布減）
                   IF      WRK-COM-INPUTCD (IDX-Z) =   CONS-SPCM-SRYCD
                       ADD     1               TO  IDX-Z
                   END-IF
      *            （精減）
                   MOVE    CONS-SGEN-SRYCD     TO  WRK-SRYCD
                   PERFORM 30011-GENTUIKA-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    "1"             TO  WRK-COM-CUR (IDX)
                   END-IF
                   MOVE    1               TO  FLG-ZAI
               END-IF
               MOVE    1               TO  FLG-ZAI
      *H27.1
      *        IF     (FLG-NAIFUKU             =   1   )
      *          AND  (WRK-COM-ZAIEND (IDX)    =   "1" )
      *            頓服・外用毎の点数集計
      *            EVALUATE    WRK-CHK-SRYSYUKBN
      *            頓服
      *            WHEN    "221"
      *            WHEN    "297"
      *                PERFORM 30022-TAZAI22-GENCHK-SEC
      *            外用
      *            WHEN    "231"
      *            WHEN    "294"
      *                PERFORM 30023-TAZAI23-GENCHK-SEC
      *            END-EVALUATE
      *******  END-IF
             END-IF
      *
               IF      WRK-COM-ZAIEND (IDX)    =   "1"
                   MOVE    ZERO            TO  FLG-ZAI
               END-IF
      *
           END-PERFORM
      *
           MOVE    ZERO                TO  WRK-GMN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                             ((WRK-GMN-INPUTCD(IDX) =   SPACE ) AND
                              (WRK-COM-INPUTCD(IDX) =   SPACE ))
               ADD     1               TO  WRK-GMN-MAX
           END-PERFORM
      *
      * 薬剤料逓減
      *    内服の逓減
           IF      WRK-KEIKEI21        >   ZERO
               PERFORM 30023-TAZAI21-GENHEN-SEC
           END-IF
      *
      *    頓服の逓減
           IF      WRK-KEIKEI22        >   ZERO
               PERFORM 30024-TAZAI22-GENHEN-SEC
           END-IF
      *    外用の逓減
           IF      WRK-KEIKEI23        >   ZERO
               PERFORM 30025-TAZAI23-GENHEN-SEC
           END-IF
           .
       3002-TAZAI-GENHEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    向精神薬薬剤料逓減チェック処理
      *****************************************************************
       30021-TAZAIGENTUIKA-CHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-ZAIEND2
           MOVE    ZERO                TO  FLG-GENOK-ALL
           MOVE    ZERO                TO  WRK-ZAIKEI
           MOVE    ZERO                TO  WRK-ZAITEN-K
           PERFORM VARYING     IDX-G   FROM    IDX BY  1
                   UNTIL      (IDX-G   >   SPA-MAX-LINE  )
                       OR    ( FLG-ZAIEND2     =   1     )
      *        判定
               IF      WRK-COM-INPUTCD (IDX-G)(1:1)  =   "6"
                   PERFORM 300211-GENTUIKACHK-HAN-SEC
               END-IF
      *
               IF      WRK-COM-ZAIEND (IDX-G)  =   "1"
                   MOVE    1               TO  FLG-ZAIEND2
                   IF      FLG-GENOK-ALL   =   ZERO
      *                剤点数がゼロの時、対象外
                       MOVE    ZERO            TO  FLG-NAIFUKU
                   ELSE
      *                対象剤点数
      ***************  PERFORM 300212-ZAITENSYUKEI-SEC
                       PERFORM 300212-TAZAITENSYUKEI-SEC
                   END-IF
               END-IF
           END-PERFORM
           .
       30021-TAZAIGENTUIKA-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    薬剤料逓減追加処理
      *****************************************************************
       300211-GENTUIKACHK-HAN-SEC         SECTION.
      *
           IF      WRK-COM-PSY-CLASS (IDX-G)   NOT =   SPACE
               MOVE    1                   TO  FLG-GENOK-ALL
               COMPUTE WRK-SYOKEIS2        =   WRK-COM-KISOTEN (IDX-G)
                                           *   WRK-COM-SURYO   (IDX-G)
               ADD     WRK-SYOKEIS2        TO  WRK-ZAIKEI
           END-IF
           .
       300211-GENTUIKACHK-HAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤料点数集計処理
      *****************************************************************
       300212-ZAITENSYUKEI-SEC         SECTION.
      *
           MOVE    WRK-ZAIKEI          TO  WRK-SYOKEIS2
           IF      WRK-SYOKEIS2        <=  15
               IF     (WRK-SYOKEIS2        >   ZERO       )
                   MOVE    1               TO  WRK-SYOKEIS3
               ELSE
                   MOVE    ZERO            TO  WRK-SYOKEIS3
               END-IF
           ELSE
               COMPUTE WRK-SYOKEIS3        =  ((WRK-SYOKEIS2   -  15)
                                           /    10 )
                                           +   1
           END-IF
      *
           COMPUTE WRK-SC90-YKZTEN     =   WRK-SYOKEIS3   + 0.999999
           COMPUTE WRK-ZAITEN-K        =   WRK-SC90-YKZTEN
                                       *   WRK-COM-KAISU(IDX-G)
           .
       300212-ZAITENSYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    向精神薬薬剤料点数集計処理
      *****************************************************************
       300212-TAZAITENSYUKEI-SEC         SECTION.
      *
      *    薬剤点数計算処理
           PERFORM 300212-ZAITENSYUKEI-SEC
      *
      *    頓服・外用毎の点数集計
           EVALUATE    WRK-CHK-SRYSYUKBN
      *        内服
               WHEN    "211"
               WHEN    "291"
                   PERFORM 30022-TAZAI21-GENCHK-SEC
      *        頓服
               WHEN    "221"
               WHEN    "297"
                   PERFORM 30022-TAZAI22-GENCHK-SEC
      *        外用
               WHEN    "231"
               WHEN    "294"
                   PERFORM 30023-TAZAI23-GENCHK-SEC
           END-EVALUATE
           .
       300212-TAZAITENSYUKEI-EXT.
           EXIT.
      *
      *H26.10
      *****************************************************************
      *    向精神薬多剤投与　内服逓減編集処理
      *****************************************************************
       30023-TAZAI21-GENHEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  WRK-SANTEN-G
      *    内服全体
           COMPUTE WRK-SANTEN-9        =   WRK-KEIKEI21
                                       *   CONS-RT80
                                       /   100.00
                                       +   0.5
           MOVE    WRK-SANTEN-9        TO  WRK-SANTEN
           COMPUTE WRK-SANTEN-KEI      =   WRK-KEIKEI21
                                       -   WRK-SANTEN
      *
      *    複数保険最終行
           MOVE    ZERO                TO  WRK-CNT-HKN21
           PERFORM VARYING IDX-GEN     FROM    1   BY  1
                   UNTIL  (IDX-GEN         >   15 )
               IF      WRK-HKNKEI21-COMBI (IDX-GEN)
                                       NOT =   SPACE
                   MOVE    IDX-GEN         TO  WRK-CNT-HKN21
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX-GEN     FROM    1   BY  1
                   UNTIL   IDX-GEN     >   WRK-CNT-HKN21
               IF      IDX-GEN         =   WRK-CNT-HKN21
                   COMPUTE WRK-SANTENSU    =   WRK-SANTEN-KEI
                                           -   WRK-SANTEN-G
               ELSE
                   COMPUTE WRK-SANTEN-9    =   WRK-HKNKEI21-KEIKEI
                                                       (IDX-GEN)
                                           *   CONS-RT80
                                           /   100.00
                                           +   0.5
                   MOVE    WRK-SANTEN-9    TO  WRK-SANTEN-GEN
                   COMPUTE WRK-SANTENSU    =   WRK-HKNKEI21-KEIKEI
                                                       (IDX-GEN)
                                           -   WRK-SANTEN-GEN
                   ADD     WRK-SANTENSU    TO  WRK-SANTEN-G
               END-IF
      *
               MOVE    CONS-TAZAI-SRYCD    TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "210"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "21"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
                   MOVE    WRK-HKNKEI21-COMBI (IDX-GEN)
                                           TO  LNK-ORCSC-HKNCOMBI
                                                         (02 IDXS)
                   MOVE    WRK-HKNKEI21-CHIKEN(IDX-GEN)
                                           TO  LNK-ORCSC-CHIKENKBN
                                                         (02 IDXS)
               END-IF
           END-PERFORM
           .
       30023-TAZAI21-GENHEN-EXT.
           EXIT.
      *
      *H26.10
      *****************************************************************
      *    向精神薬多剤投与　内服集計処理
      *****************************************************************
       30022-TAZAI21-GENCHK-SEC         SECTION.
      *
      *    保険毎にまとめる
           PERFORM VARYING   IDXW      FROM    1   BY  1
                   UNTIL    (IDXW      >   15  )
               IF      WRK-HKNKEI21-COMBI (IDXW) =   SPACE
                   MOVE    WRK-COM-HKNCOMBI (IDX-G)
                                               TO  WRK-HKNKEI21-COMBI
                                                      (IDXW)
                   MOVE    WRK-COM-CHIKENKBN(IDX-G)
                                               TO  WRK-HKNKEI21-CHIKEN
                                                    (IDXW)
               END-IF
               IF      WRK-HKNKEI21-COMBI (IDXW)
                                       =   WRK-COM-HKNCOMBI (IDX-G)
                   MOVE    IDXW                TO  WRK-CNT-HKN21
                   ADD     WRK-ZAITEN-K        TO  WRK-KEIKEI21
                   ADD     WRK-ZAITEN-K        TO  WRK-HKNKEI21-KEIKEI
                                                       (WRK-CNT-HKN21)
                   MOVE    15              TO  IDXW
               END-IF
           END-PERFORM
      *
           .
       30022-TAZAI21-GENCHK-EXT.
           EXIT.
      *****************************************************************
      *    向精神薬多剤投与　頓服集計処理
      *****************************************************************
       30022-TAZAI22-GENCHK-SEC         SECTION.
      *
      *    保険毎にまとめる
           PERFORM VARYING   IDXW      FROM    1   BY  1
                   UNTIL    (IDXW      >   15  )
               IF      WRK-HKNKEI22-COMBI (IDXW) =   SPACE
                   MOVE    WRK-COM-HKNCOMBI (IDX-G)
                                               TO  WRK-HKNKEI22-COMBI
                                                      (IDXW)
                   MOVE    WRK-COM-CHIKENKBN(IDX-G)
                                               TO  WRK-HKNKEI22-CHIKEN
                                                    (IDXW)
               END-IF
               IF      WRK-HKNKEI22-COMBI (IDXW)
                                       =   WRK-COM-HKNCOMBI (IDX-G)
                   MOVE    IDXW            TO  WRK-CNT-HKN22
      *            ADD     WRK-COM-KEI (IDX)   TO  WRK-KEIKEI22
      *            ADD     WRK-COM-KEI (IDX)   TO  WRK-HKNKEI22-KEIKEI
                   ADD     WRK-ZAITEN-K        TO  WRK-KEIKEI22
                   ADD     WRK-ZAITEN-K        TO  WRK-HKNKEI22-KEIKEI
                                                       (WRK-CNT-HKN22)
                   MOVE    15              TO  IDXW
               END-IF
           END-PERFORM
      *
           .
       30022-TAZAI22-GENCHK-EXT.
           EXIT.
      *****************************************************************
      *    向精神薬多剤投与　外用集計処理
      *****************************************************************
       30023-TAZAI23-GENCHK-SEC         SECTION.
      *
      *    保険毎にまとめる
           PERFORM VARYING   IDXW      FROM    1   BY  1
                   UNTIL    (IDXW      >   15  )
               IF      WRK-HKNKEI23-COMBI (IDXW) =   SPACE
                   MOVE    WRK-COM-HKNCOMBI (IDX-G)
                                               TO  WRK-HKNKEI23-COMBI
                                                      (IDXW)
                   MOVE    WRK-COM-CHIKENKBN(IDX-G)
                                               TO  WRK-HKNKEI23-CHIKEN
                                                    (IDXW)
               END-IF
               IF      WRK-HKNKEI23-COMBI (IDXW)
                                       =   WRK-COM-HKNCOMBI (IDX-G)
                   MOVE    IDXW            TO  WRK-CNT-HKN23
      *            ADD     WRK-COM-KEI (IDX)   TO  WRK-KEIKEI23
      *            ADD     WRK-COM-KEI (IDX)   TO  WRK-HKNKEI23-KEIKEI
                   ADD     WRK-ZAITEN-K        TO  WRK-KEIKEI23
                   ADD     WRK-ZAITEN-K        TO  WRK-HKNKEI23-KEIKEI
                                                       (WRK-CNT-HKN23)
                   MOVE    15              TO  IDXW
               END-IF
           END-PERFORM
      *
           .
       30023-TAZAI23-GENCHK-EXT.
           EXIT.
      *****************************************************************
      *    向精神薬多剤投与　頓服編集処理
      *****************************************************************
       30024-TAZAI22-GENHEN-SEC         SECTION.
      *
      *    薬剤料逓減
           MOVE    ZERO                TO  WRK-SANTEN-G
      *    全体
           COMPUTE WRK-SANTEN-9        =   WRK-KEIKEI22
                                       *   CONS-RT80
                                       /   100.00
                                       +   0.5
           MOVE    WRK-SANTEN-9        TO  WRK-SANTEN
           COMPUTE WRK-SANTEN-KEI      =   WRK-KEIKEI22
                                       -   WRK-SANTEN
      *
      *    複数保険最終行
           MOVE    ZERO                TO  WRK-CNT-HKN22
           PERFORM VARYING IDX-GEN     FROM    1   BY  1
                   UNTIL  (IDX-GEN         >   15 )
               IF      WRK-HKNKEI22-COMBI (IDX-GEN)
                                       NOT =   SPACE
                   MOVE    IDX-GEN         TO  WRK-CNT-HKN22
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX-GEN     FROM    1   BY  1
                   UNTIL   IDX-GEN     >   WRK-CNT-HKN22
               IF      IDX-GEN         =   WRK-CNT-HKN22
                   COMPUTE WRK-SANTENSU    =   WRK-SANTEN-KEI
                                           -   WRK-SANTEN-G
               ELSE
                   COMPUTE WRK-SANTEN-9    =   WRK-HKNKEI22-KEIKEI
                                                       (IDX-GEN)
                                           *   CONS-RT80
                                           /   100.00
                                           +   0.5
                   MOVE    WRK-SANTEN-9    TO  WRK-SANTEN-GEN
                   COMPUTE WRK-SANTENSU    =   WRK-HKNKEI22-KEIKEI
                                                       (IDX-GEN)
                                           -   WRK-SANTEN-GEN
                   ADD     WRK-SANTENSU    TO  WRK-SANTEN-G
               END-IF
      *
               MOVE    CONS-TAZAI-SRYCD    TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "220"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "22"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
                   MOVE    WRK-HKNKEI22-COMBI (IDX-GEN)
                                           TO  LNK-ORCSC-HKNCOMBI
                                                         (02 IDXS)
                   MOVE    WRK-HKNKEI22-CHIKEN(IDX-GEN)
                                           TO  LNK-ORCSC-CHIKENKBN
                                                         (02 IDXS)
               END-IF
           END-PERFORM
      *
           .
       30024-TAZAI22-GENHEN-EXT.
           EXIT.
      *****************************************************************
      *    向精神薬多剤投与　外用編集処理
      *****************************************************************
       30025-TAZAI23-GENHEN-SEC         SECTION.
      *
      *    薬剤料逓減
           MOVE    ZERO                TO  WRK-SANTEN-G
      *H28.4
      *    湿布薬７０枚以上の減点
           COMPUTE WRK-KEIKEI23        =   WRK-KEIKEI23
                                       -   WRK-SPI-GENKEI
           PERFORM VARYING IDX-GEN     FROM    1   BY  1
                   UNTIL   IDX-GEN     >   WRK-CNT-HKN23
               PERFORM VARYING IDX-SIP     FROM    1   BY  1
                       UNTIL   IDX-SIP     >   WRK-CNT-HKNSP
                   IF     (WRK-HKNKEI23-COMBI (IDX-GEN)
                                           =   WRK-HKNKEISP-COMBI
                                                         (IDX-SIP))
                     AND  (WRK-HKNKEI23-CHIKEN(IDX-GEN)
                                           =   WRK-HKNKEISP-CHIKEN
                                                         (IDX-SIP))
                       COMPUTE WRK-HKNKEI23-KEIKEI(IDX-GEN)
                                           =   WRK-HKNKEI23-KEIKEI
                                                              (IDX-GEN)
                                           -   WRK-SPI-GENTEN (IDX-SIP)
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    全体
           COMPUTE WRK-SANTEN-9        =   WRK-KEIKEI23
                                       *   CONS-RT80
                                       /   100.00
                                       +   0.5
           MOVE    WRK-SANTEN-9        TO  WRK-SANTEN
           COMPUTE WRK-SANTEN-KEI      =   WRK-KEIKEI23
                                       -   WRK-SANTEN
      *
      *    複数保険最終行
           MOVE    ZERO                TO  WRK-CNT-HKN23
           PERFORM VARYING IDX-GEN     FROM    1   BY  1
                   UNTIL  (IDX-GEN         >   15 )
               IF      WRK-HKNKEI23-COMBI (IDX-GEN)
                                       NOT =   SPACE
                   MOVE    IDX-GEN         TO  WRK-CNT-HKN23
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX-GEN     FROM    1   BY  1
                   UNTIL   IDX-GEN     >   WRK-CNT-HKN23
               IF      IDX-GEN         =   WRK-CNT-HKN23
                   COMPUTE WRK-SANTENSU    =   WRK-SANTEN-KEI
                                           -   WRK-SANTEN-G
               ELSE
                   COMPUTE WRK-SANTEN-9    =   WRK-HKNKEI23-KEIKEI
                                                       (IDX-GEN)
                                           *   CONS-RT80
                                           /   100.00
                                           +   0.5
                   MOVE    WRK-SANTEN-9    TO  WRK-SANTEN-GEN
                   COMPUTE WRK-SANTENSU    =   WRK-HKNKEI23-KEIKEI
                                                       (IDX-GEN)
                                           -   WRK-SANTEN-GEN
                   ADD     WRK-SANTENSU    TO  WRK-SANTEN-G
               END-IF
      *
               MOVE    CONS-TAZAI-SRYCD    TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "230"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "23"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
                   MOVE    WRK-HKNKEI23-COMBI (IDX-GEN)
                                           TO  LNK-ORCSC-HKNCOMBI
                                                         (02 IDXS)
                   MOVE    WRK-HKNKEI23-CHIKEN(IDX-GEN)
                                           TO  LNK-ORCSC-CHIKENKBN
                                                         (02 IDXS)
               END-IF
           END-PERFORM
      *
           .
       30024-TAZAI22-GENHEN-EXT.
           EXIT.
      *
      *H26.10
      *H27.4
      *****************************************************************
      *    ３０日以上の逓減算定処理
      *****************************************************************
       30013-GENHEN30-SYORI-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-ZAI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                             ((WRK-GMN-INPUTCD(IDX) =   SPACE)  AND
                              (WRK-COM-INPUTCD(IDX) =   SPACE ))
                       OR    ( SPA-ERRCD       NOT  =   SPACE )
      *
             IF     (WRK-COM-SRYKA(IDX)    =   WRK-SRYKA  )
               AND  (WRK-COM-HKNCOMBI(IDX) NOT =   "9999" )
      *        薬評対象外
               AND  (WRK-COM-YAKUHYO (IDX) =   SPACE  )
      *        減点対象外
               AND  (WRK-COM-GENTEN (IDX)  =   SPACE  )
      *
      *        診療種別区分変換
               PERFORM 30001-SRYSYUKBN-SET-SEC
      *        院内投薬で３０日以上を対象とする
               IF     (WRK-CHK-SRYSYUKBN   =   "211"
                                           OR  "291")
      **                                   OR  "221"
      **                                   OR  "297" )
                   MOVE    1               TO  FLG-NAIFUKU
      *            薬剤料逓減チェック処理
                   PERFORM 300110-GENTUIKA-CHK30-SEC
               ELSE
                   MOVE    ZERO            TO  FLG-NAIFUKU
               END-IF
               IF     (FLG-NAIFUKU             =   1     )  AND
                      (FLG-ZAI                 =   ZERO  )
                   IF      WRK-GMN-INPUTCD(IDX)(1:1)   =   "."
      *                約束セットの時、約束セットの後に追加
                       IF      WRK-GMN-INPUTCD(IDX + 1)(1:1)   =   "S"
                           COMPUTE IDX-Z           =   IDX     +   2
                       ELSE
                           COMPUTE IDX-Z           =   IDX     +   1
                       END-IF
                   ELSE
                       MOVE    IDX             TO  IDX-Z
                   END-IF
      *            （精減）の後
                   IF      WRK-COM-INPUTCD (IDX-Z) =   CONS-SGEN-SRYCD
                       ADD     1               TO  IDX-Z
                   END-IF
      *            （減）
                   MOVE    CONS-TGEN-SRYCD     TO  WRK-SRYCD
                   PERFORM 30011-GENTUIKA-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    "1"             TO  WRK-COM-CUR (IDX)
                   END-IF
                   MOVE    1               TO  FLG-ZAI
               END-IF
               MOVE    1               TO  FLG-ZAI
               END-IF
      *
               IF      WRK-COM-ZAIEND (IDX)    =   "1"
                   MOVE    ZERO            TO  FLG-ZAI
               END-IF
      *
           END-PERFORM
      *
           MOVE    ZERO                TO  WRK-GMN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                             ((WRK-GMN-INPUTCD(IDX) =   SPACE ) AND
                              (WRK-COM-INPUTCD(IDX) =   SPACE ))
               ADD     1               TO  WRK-GMN-MAX
           END-PERFORM
      *
      * 薬剤料逓減
           MOVE    ZERO                TO  WRK-GEN-RT
           IF      FLG-GEN30-PSY       =   1
      *        向精神薬多剤投与逓減あり
               MOVE    CONS-RT80           TO  WRK-GEN-RT
           END-IF
           IF      FLG-GEN30-7GEN      =   1
      *        ７種類以上の逓減あり
               MOVE    90                  TO  WRK-GEN-RT
           END-IF
      *
      *    内服の逓減
           IF      WRK-GEN30N-KEI      >   ZERO
               PERFORM 300131-GEN30-HEN21-SEC
           END-IF
      *
      *    頓服の逓減
           IF      WRK-GEN30P-KEI      >   ZERO
               PERFORM 300132-GEN30-HEN22-SEC
           END-IF
           .
       30013-GENHEN30-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    薬剤料逓減チェック処理
      *****************************************************************
       300110-GENTUIKA-CHK30-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ZAIEND2
           MOVE    ZERO                TO  FLG-YAKZ-ARI
           PERFORM VARYING     IDX-G   FROM    IDX BY  1
                   UNTIL      (IDX-G   >   SPA-MAX-LINE  )
                       OR    ( FLG-ZAIEND2     =   1     )
      *
               IF     (WRK-COM-INPUTCD (IDX-G)(1:1) =   "6" )
                   IF     (WRK-COM-YOBI2   (IDX-G)(1:1) =   "1" )
      *                ３０日以上投薬科
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-YAKZ-ARI
                   END-IF
               END-IF
      *
               IF      WRK-COM-ZAIEND (IDX-G)  =   "1"
                   MOVE    1               TO  FLG-ZAIEND2
                   IF     (WRK-COM-KEI (IDX-G) =   ZERO   )
                   OR     (WRK-COM-KAISU(IDX-G) <   30    )
      *                剤点数がゼロ、３０日未満対象外
                       MOVE    ZERO            TO  FLG-NAIFUKU
                   END-IF
               END-IF
           END-PERFORM
      *    対象薬剤がない時、対象外
           IF      FLG-YAKZ-ARI        =   ZERO
               MOVE    ZERO            TO  FLG-NAIFUKU
           END-IF
           .
       300110-GENTUIKA-CHK30-EXT.
           EXIT.
      *
      *****************************************************************
      *    ３０日以上の逓減　内服逓減編集処理
      *****************************************************************
       300131-GEN30-HEN21-SEC         SECTION.
      *
      *    内服全体
           MOVE    WRK-GEN30N-KEI      TO  WRK-GEN30-TEN
      *
      *    IF      WRK-GEN-RT      NOT =   ZERO
      *        COMPUTE WRK-SANTEN-9        =   WRK-GEN30N-KEI
      *                                    *  (WRK-GEN-RT
      *                                    /   100.00  )
      *                                    +   0.5
      *        MOVE    WRK-SANTEN-9        TO  WRK-GEN30-TEN
      *    END-IF
      *H28.4
           MOVE    WRK-GEN30N-KEI      TO  WRK-GEN30-TEN1
           MOVE    WRK-GEN30N-KEI5     TO  WRK-GEN30-TEN5
           MOVE    WRK-GEN30N-KEI6     TO  WRK-GEN30-TEN6
           PERFORM 300131-GEN30-KEISAN-SEC
      *
      *    ３０日以上の逓減計算
           COMPUTE WRK-SANTEN-9        =   WRK-GEN30-TEN
                                       *   CONS-RT60
                                       /   100.00
                                       +   0.5
           MOVE    WRK-SANTEN-9        TO  WRK-SANTEN
           COMPUTE WRK-GEN30-KEI       =   WRK-GEN30-TEN
                                       -   WRK-SANTEN
      *
      *    複数保険最終行
           MOVE    ZERO                TO  WRK-GEN30N-CNT
           PERFORM VARYING IDX-GEN     FROM    1   BY  1
                   UNTIL  (IDX-GEN         >   15 )
               IF      WRK-GEN30N-COMBI (IDX-GEN)
                                       NOT =   SPACE
                   MOVE    IDX-GEN         TO  WRK-GEN30N-CNT
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  WRK-GEN30-KEIGK
           PERFORM VARYING IDX-GEN     FROM    1   BY  1
                   UNTIL   IDX-GEN     >   WRK-GEN30N-CNT
               IF      IDX-GEN         =   WRK-GEN30N-CNT
                   COMPUTE WRK-SANTENSU    =   WRK-GEN30-KEI
                                           -   WRK-GEN30-KEIGK
               ELSE
                   MOVE    WRK-GEN30N-HKNKEI(IDX-GEN)
                                               TO  WRK-GEN30-TEN
                   MOVE    WRK-GEN30N-HKNKEI(IDX-GEN)
                                               TO  WRK-GEN30-TEN1
                   MOVE    WRK-GEN30N-HKNKEI5(IDX-GEN)
                                               TO  WRK-GEN30-TEN5
                   MOVE    WRK-GEN30N-HKNKEI6(IDX-GEN)
                                               TO  WRK-GEN30-TEN6
                   PERFORM 300131-GEN30-KEISAN-SEC
                   COMPUTE WRK-SANTEN-9    =   WRK-GEN30-TEN
                                           *  (CONS-RT60
                                           /   100.00  )
                                           +   0.5
                   MOVE    WRK-SANTEN-9    TO  WRK-SANTEN-GEN
                   COMPUTE WRK-SANTENSU    =   WRK-GEN30-TEN
                                           -   WRK-SANTEN-GEN
                   ADD     WRK-SANTENSU    TO  WRK-GEN30-KEIGK
               END-IF
      *
               MOVE    CONS-TEIHP-SRYCD    TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "210"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "21"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
                   MOVE    WRK-GEN30N-COMBI (IDX-GEN)
                                           TO  LNK-ORCSC-HKNCOMBI
                                                         (02 IDXS)
                   MOVE    WRK-GEN30N-CHIKEN(IDX-GEN)
                                           TO  LNK-ORCSC-CHIKENKBN
                                                         (02 IDXS)
               END-IF
           END-PERFORM
           .
       300131-GEN30-HEN21-EXT.
           EXIT.
      *
      *****************************************************************
      *    ３０日以上の逓減計算処理
      *****************************************************************
       300131-GEN30-KEISAN-SEC        SECTION.
      *
      *
           IF      FLG-GEN30-PSY       =   1
               MOVE    ZERO                TO  WRK-SANTEN-GEN5
               MOVE    ZERO                TO  WRK-SANTEN-GEN6
      *        向精神薬多剤投与逓減あり
               MOVE    CONS-RT80           TO  WRK-GEN-RT
               COMPUTE WRK-SANTEN-9        =   WRK-GEN30-TEN5
                                           *  (WRK-GEN-RT
                                           /   100.00  )
                                           +   0.5
               MOVE    WRK-SANTEN-9        TO  WRK-SANTEN
               COMPUTE WRK-SANTEN-GEN5     =   WRK-GEN30-TEN5
                                           -   WRK-SANTEN
               IF      FLG-GEN30-7GEN      =   1
      *            ７種類以上の逓減あり
                   MOVE    90                  TO  WRK-GEN-RT
                   COMPUTE WRK-SANTEN-9        =   WRK-GEN30-TEN6
                                               *  (WRK-GEN-RT
                                               /   100.00  )
                                               +   0.5
                   MOVE    WRK-SANTEN-9        TO  WRK-SANTEN
                   COMPUTE WRK-SANTEN-GEN6     =   WRK-GEN30-TEN6
                                               -   WRK-SANTEN
               END-IF
               COMPUTE WRK-GEN30-TEN       =   WRK-GEN30-TEN1
                                           -   WRK-SANTEN-GEN5
                                           -   WRK-SANTEN-GEN6
           ELSE
      *        ７種類以上の逓減あり
               IF      FLG-GEN30-7GEN      =   1
                   MOVE    90                  TO  WRK-GEN-RT
                   COMPUTE WRK-SANTEN-9        =   WRK-GEN30-TEN1
                                               *  (WRK-GEN-RT
                                               /   100.00  )
                                               +   0.5
                   MOVE    WRK-SANTEN-9        TO  WRK-GEN30-TEN
               END-IF
           END-IF
           .
       300131-GEN30-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ３０日以上の逓減　頓服逓減編集処理
      *****************************************************************
       300132-GEN30-HEN22-SEC         SECTION.
      *
      *    内服全体
           MOVE    WRK-GEN30P-KEI      TO  WRK-GEN30-TEN
      **   IF      WRK-GEN-RT      NOT =   ZERO
      *        COMPUTE WRK-SANTEN-9        =   WRK-GEN30P-KEI
      *                                    *  (WRK-GEN-RT
      *                                    /   100.00  )
      *                                    +   0.5
      *        MOVE    WRK-SANTEN-9        TO  WRK-GEN30-TEN
      **   END-IF
      *H28.4
           MOVE    WRK-GEN30P-KEI      TO  WRK-GEN30-TEN1
           MOVE    WRK-GEN30P-KEI5     TO  WRK-GEN30-TEN5
           MOVE    WRK-GEN30P-KEI6     TO  WRK-GEN30-TEN6
           PERFORM 300131-GEN30-KEISAN-SEC
      *
      *    ３０日以上の逓減計算
           COMPUTE WRK-SANTEN-9        =   WRK-GEN30-TEN
                                       *  (CONS-RT60
                                       /   100.00 )
                                       +   0.5
           MOVE    WRK-SANTEN-9        TO  WRK-SANTEN
           COMPUTE WRK-GEN30-KEI       =   WRK-GEN30-TEN
                                       -   WRK-SANTEN
      *
      *    複数保険最終行
           MOVE    ZERO                TO  WRK-GEN30P-CNT
           PERFORM VARYING IDX-GEN     FROM    1   BY  1
                   UNTIL  (IDX-GEN         >   15 )
               IF      WRK-GEN30P-COMBI (IDX-GEN)
                                       NOT =   SPACE
                   MOVE    IDX-GEN         TO  WRK-GEN30P-CNT
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  WRK-GEN30-KEIGK
           PERFORM VARYING IDX-GEN     FROM    1   BY  1
                   UNTIL   IDX-GEN     >   WRK-GEN30P-CNT
               IF      IDX-GEN         =   WRK-GEN30P-CNT
                   COMPUTE WRK-SANTENSU    =   WRK-GEN30-KEI
                                           -   WRK-GEN30-KEIGK
               ELSE
                   MOVE    WRK-GEN30P-HKNKEI(IDX-GEN)
                                               TO  WRK-GEN30-TEN
      *H28.4
                   MOVE    WRK-GEN30P-HKNKEI(IDX-GEN)
                                               TO  WRK-GEN30-TEN1
                   MOVE    WRK-GEN30P-HKNKEI5(IDX-GEN)
                                               TO  WRK-GEN30-TEN5
                   MOVE    WRK-GEN30P-HKNKEI6(IDX-GEN)
                                               TO  WRK-GEN30-TEN6
                   PERFORM 300131-GEN30-KEISAN-SEC
      **           IF      WRK-GEN-RT      NOT =   ZERO
      *                COMPUTE WRK-SANTEN-9    =   WRK-GEN30-TEN
      *                                        *  (WRK-GEN-RT
      *                                        /   100.00)
      *                                        +   0.5
      *                MOVE    WRK-SANTEN-9    TO  WRK-GEN30-TEN
      **           END-IF
                   COMPUTE WRK-SANTEN-9    =   WRK-GEN30-TEN
                                           *  (CONS-RT60
                                           /   100.00 )
                                           +   0.5
                   MOVE    WRK-SANTEN-9    TO  WRK-SANTEN-GEN
                   COMPUTE WRK-SANTENSU    =   WRK-GEN30-TEN
                                           -   WRK-SANTEN-GEN
                   ADD     WRK-SANTENSU    TO  WRK-GEN30-KEIGK
               END-IF
      *
               MOVE    CONS-TEIHP-SRYCD    TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               PERFORM 3002-KASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    "220"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "22"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
                   MOVE    WRK-GEN30P-COMBI (IDX-GEN)
                                           TO  LNK-ORCSC-HKNCOMBI
                                                         (02 IDXS)
                   MOVE    WRK-GEN30P-CHIKEN(IDX-GEN)
                                           TO  LNK-ORCSC-CHIKENKBN
                                                         (02 IDXS)
               END-IF
           END-PERFORM
           .
       300132-GEN30-HEN22-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    入院の自動算定処理
      *****************************************************************
       30021-NYUIN-INNAI-SYORI-SEC         SECTION.
      *
      *    診療年月の末日算定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SPA-SRYYMD(1:6)     TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI(7:2)    TO  WRK-MATU
      *
      *    調剤料（入院）加算
           IF     (FLG-NAIYAK          =   1    )  OR
                  (FLG-TONPUK          =   1    )  OR
                  (FLG-GAIYAK          =   1    )
               MOVE    "120001110"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               PERFORM 300211-NYUKASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    ZERO            TO  FLG-DAY
                   PERFORM 300212-NISUU-SET-SEC
                   MOVE    "240"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "24"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
               END-IF
           END-IF
      *    麻毒加算料
           IF      FLG-MADOKU          =   1
      *        麻毒調剤料
               MOVE    "120000410"         TO  WRK-SRYCD
               MOVE    SPACE               TO  WRK-SRYCD2
               PERFORM 300211-NYUKASAN-SYORI-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    1               TO  FLG-DAY
                   PERFORM 300212-NISUU-SET-SEC
                   MOVE    "260"           TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                   MOVE    "26"            TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
               END-IF
           END-IF
      *    訂正で自動発生なしの場合
           IF     (LNK-ORCSC-SYORIFLG  =   1  )  AND
                  (SPA-TEISEI-FLG      =   "0")
               GO      TO      30021-NYUIN-INNAI-SYORI-EXT
           END-IF
      *
      *    調剤技術基本料（入院）
           IF     (FLG-NAIYAK          =   1    )  OR
                  (FLG-TONPUK          =   1    )  OR
                  (FLG-GAIYAK          =   1    )
      *        調基が１であること
               IF      LNK-ORCSC-CHOKI-FLG   =   1
      **********   IF      SPA-HOSPSBT         =   1
      *                病院
                       MOVE    "120001710"         TO  WRK-SRYCD
      ************ ELSE
      *                診療所
      *                MOVE    "120001810"         TO  WRK-SRYCD
      *************END-IF
                   MOVE    SPACE               TO  WRK-SRYCD2
                   MOVE    1                   TO  FLG-ALL
                   PERFORM 3002-KASAN-SYORI-SEC
                   IF      FLG-ERR             =   ZERO
                       PERFORM 30024-SYOHOUSEN-CHK-SEC
                       IF      FLG-SYOHOU-ERR      =   ZERO
                           MOVE    3               TO  FLG-DAY
                           PERFORM 300212-NISUU-SET-SEC
                           MOVE    "270"       TO  LNK-ORCSC-SRYSYUKBN
                                                         (02 IDXS)
                           MOVE    "27"        TO  LNK-ORCSC-SRYKBN
                                                         (02 IDXS)
                           MOVE    "4"         TO  LNK-ORCSC-AUTOKBN
                                                         (02 IDXS)
                       ELSE
                           INITIALIZE             LNK-ORCSC-KASN-TBL2
                                                         (02 IDXS)
                           COMPUTE IDXS            =   IDXS   -   1
                       END-IF
                   ELSE
      *                病院で外来で算定済みかの判断
                       IF     (WRK-SRYCD           =   "120001710") AND
                              (SPA-CHOKICHK-FLG    =   "1"     )   AND
                              (SPA-CHOKI-ARI       =   SPACE   )
                       AND    (SPA-CHOZAI-CHK      =   ZERO    )
                           PERFORM 300211-CHOZAI-CHK-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       30021-NYUIN-INNAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来の調剤技術基本料算定チェック処理
      *****************************************************************
       300211-CHOZAI-CHK-SEC           SECTION.
      *
           IF      SPA-CHOKI-ARI       =   SPACE
      *        入院の調剤料のチェック
               MOVE    "120001110"         TO  WRK-SRYCD
               PERFORM 3002110-SRYACT-CHOZAI-SEC
               IF      FLG-SRYACCT-ARI     =   1
                   MOVE    "1"             TO  SPA-CHOKI-ARI
               END-IF
           END-IF
      *
           IF      SPA-CHOKI-ARI       =   SPACE
               MOVE    "120001810"         TO  WRK-SRYCD
               MOVE    ZERO                TO  FLG-TENSUPLUS-CHK
               PERFORM 3001-SANTEI-CHK-SEC
               IF      FLG-ERR             =   1
                   IF      SPA-CHOZAI-CHK      =   ZERO
                       MOVE    1               TO  SPA-CHOZAI-CHK
                       MOVE    "K017"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       300211-CHOZAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院調剤料算定判定処理
      *****************************************************************
       3002110-SRYACT-CHOZAI-SEC     SECTION.
      *
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    SPA-SRYYMD(1:6)     TO  ACCT-SRYYM
           MOVE    "24"                TO  ACCT-SRYKBN
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM UNTIL       (FLG-SRYACCT         =   1       )
      *
               IF     (ACCT-HKNCOMBI     =   "9999" ) OR
                      (ACCT-ZAIKAISU     =   ZERO   )
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-SRYACCT-ARI
               END-IF
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3002110-SRYACT-CHOZAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院の日数自動算定処理
      *****************************************************************
       300212-NISUU-SET-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDN1
           MOVE    ZERO                TO  IDN2
      *
           INITIALIZE                      WRK-NISUU-AREA
           MOVE    ZERO                TO  WRK-H-DAY
           MOVE    SPACE               TO  WRK-H-SEL
           PERFORM VARYING     IDN1    FROM    1   BY  1
                   UNTIL       IDN1    >   32
               IF     (IDN1                =   32  )  OR
                      (IDN1                >   WRK-MATU)  OR
                      (TBL-NYUINDAY(IDN1)  =   ZERO)
                   IF      WRK-H-DAY       NOT =   ZERO
                       IF      WRK-H-SEL   NOT =   SPACE
                           ADD     1               TO  IDN2
                           MOVE    WRK-H-DAY       TO  WRK-DAY(IDN2)
                           MOVE    WRK-H-SEL       TO  WRK-SEL(IDN2 - 1)
                       END-IF
                       MOVE    ZERO            TO  WRK-H-DAY
                       MOVE    SPACE           TO  WRK-H-SEL
                   END-IF
               ELSE
                   IF    ((FLG-DAY                 =   ZERO
                                                   OR  3   )  AND
                          (TBL-NYUINDAY (IDN1) NOT =   ZERO ))  OR
                         ((FLG-DAY                 =   1    )  AND
                          (TBL-NYUINDAY (IDN1)     =   2    ))
                       MOVE    1               TO  FLG-OK3
                   ELSE
                       MOVE    ZERO            TO  FLG-OK3
                   END-IF
                 IF      FLG-OK3             =   1
                   IF      WRK-H-DAY           =   ZERO
                       MOVE    IDN1            TO  WRK-H-DAY
                       ADD     1               TO  IDN2
                       MOVE    WRK-H-DAY       TO  WRK-DAY(IDN2)
      *                調基は最初の日とする
                       IF      FLG-DAY         =   3
                           MOVE    32              TO  IDN1
                       END-IF
                   ELSE
                       IF      WRK-H-DAY       =  (IDN1  -  1 )
                           MOVE    "-"             TO  WRK-H-SEL
                       END-IF
                       MOVE    IDN1            TO  WRK-H-DAY
                   END-IF
                 END-IF
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDN1    FROM    1   BY  1
                   UNTIL       IDN1    >   31
               IF     (WRK-SEL (IDN1)      =   SPACE )  AND
                      (IDN1                <   31    )
                   IF      WRK-DAY (IDN1 + 1 )     NOT =   ZERO
                       MOVE    ","          TO  WRK-SEL (IDN1)
                   END-IF
               ELSE
                   IF     (IDN1                    =   31    )  OR
                          (WRK-DAY (IDN1 + 1 )     =   ZERO  )
                       MOVE    SPACE        TO  WRK-SEL (IDN1)
                   END-IF
               END-IF
           END-PERFORM
           INITIALIZE                      LNK-ORCSC-INPUTCD-G (02 IDXS)
      *
      *    日数なしか調基は最初の日とする
           MOVE    1                   TO  WRK-KAISU
           IF    ( WRK-KAISU           =   1  OR  ZERO ) OR
                 ( FLG-DAY             =   3    )
               MOVE    SPACE           TO  WRK-KAISU-H
               MOVE    ZERO            TO  IDN4
           ELSE
               MOVE    WRK-KAISU       TO  WRK-KAISU-Z
               MOVE    SPACE           TO  WRK-KAISU-H
               MOVE    ZERO            TO  IDN4
               PERFORM VARYING     IDN1    FROM    1   BY  1
                       UNTIL       IDN1    >   4
                   IF     (WRK-KAISU-X (IDN1:1)    NOT =   SPACE) OR
                          (IDN4                    >   ZERO  )
                       ADD     1               TO  IDN4
                       MOVE    WRK-KAISU-X (IDN1:1)    TO  WRK-KAISU-H
                                                             (IDN4:1)
                   END-IF
               END-PERFORM
           END-IF
           MOVE    1                   TO  IDN2
           MOVE    1                   TO  IDN3
           PERFORM VARYING     IDN1    FROM    1   BY  1
                   UNTIL      (IDN1    >   31  )  OR
                              (IDN2            >=  5  )  OR
                              (WRK-DAY (IDN1)  =   ZERO )
               IF      IDN3                >   19
                   ADD     1               TO  IDN2
                   MOVE    1               TO  IDN3
               END-IF
      *        '-' の次判定
               IF     (WRK-SEL (IDN1)      =   "-" )
                  AND ((IDN3   +   3 )     >=  19  )
                   ADD     1               TO  IDN2
                   MOVE    1               TO  IDN3
               END-IF
      *
               IF      LNK-ORCSC-INPUTCD(02 IDXS IDN2)   =   SPACE
                   STRING  "*"             DELIMITED  BY  SIZE
                           WRK-KAISU-H     DELIMITED  BY  SPACE
                           "/"             DELIMITED  BY  SIZE
                           INTO            LNK-ORCSC-INPUTCD
                                                   (02 IDXS IDN2)
                   END-STRING
                   ADD     2               TO  IDN3
                   ADD     IDN4            TO  IDN3
               END-IF
               IF      WRK-SEL (IDN1)      =   SPACE
                   MOVE    ","             TO  WRK-SEL (IDN1)
               END-IF
               MOVE    WRK-NISUU-TBL(IDN1) TO  LNK-ORCSC-INPUTCD
                                                  (02 IDXS IDN2)(IDN3:3)
               ADD     3                   TO  IDN3
           END-PERFORM
      * 
           .
       300212-NISUU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算料算定処理(入院）
      *****************************************************************
       300211-NYUKASAN-SYORI-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
      *    今回入力にないこと
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                              (FLG-ERR              =   1    )   OR
                              (WRK-COM-INPUTCD(IDX) =   SPACE  AND
                               WRK-GMN-INPUTCD(IDX) =   SPACE)
               IF     (WRK-COM-INPUTCD(IDX)    =   WRK-SRYCD )  OR
                      (WRK-SRYCD2          NOT =   SPACE   AND
                       WRK-COM-INPUTCD(IDX)    =   WRK-SRYCD2)
                   MOVE    1                   TO  FLG-ERR
               END-IF
           END-PERFORM
           IF      FLG-ERR             =   1
               GO      TO      300211-NYUKASAN-SYORI-EXT
           END-IF
      *
           PERFORM 3001-KYOUTU-CHEK-SEC
           IF      FLG-ERR             =   1
               GO      TO      300211-NYUKASAN-SYORI-EXT
           END-IF
      *
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    1                   TO  FLG-TENSUPLUS-CHK
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
      *!       MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  FLG-ERR
               GO      TO      300211-NYUKASAN-SYORI-EXT
           END-IF
      *
      *    算定履歴チェック処理
           IF      TNS-SANTEIRRKKBN    NOT =   ZERO
               PERFORM 3001-SANTEI-CHK-SEC
           END-IF
           IF      FLG-ERR             =   ZERO
               ADD     1               TO  IDXS
               MOVE    WRK-SRYCD       TO  LNK-ORCSC-SRYCD
                                                   (02 IDXS)
               MOVE    TNS-TEN         TO  LNK-ORCSC-TENSU
                                                   (02 IDXS)
               IF      WRK-SRYCD (1:1)     =   "1"
                   MOVE    TNS-SANTEIRRKKBN
                                           TO  LNK-ORCSC-SANTEIRRKKBN
                                                          (02 IDXS)
               END-IF
      *        年齢加算処理
               PERFORM 3002-NENREI-KASAN-SEC
           END-IF
      *
           .
       300211-NYUKASAN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算料算定処理
      *****************************************************************
       3002-KASAN-SYORI-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
      *
      *    今回入力にないこと
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                              (FLG-ERR              =   1    )   OR
                              (WRK-COM-INPUTCD(IDX) =   SPACE  AND
                               WRK-GMN-INPUTCD(IDX) =   SPACE)
            IF     (WRK-COM-SRYKA (IDX)    =   WRK-SRYKA)  OR
                   (FLG-ALL                =   1        )  OR
                   (SPA-NYUGAIKBN          =   "1"      )
               IF     (WRK-COM-INPUTCD(IDX)    =   WRK-SRYCD )  OR
                     ((WRK-SRYCD2          NOT =   SPACE )  AND
                      (WRK-COM-INPUTCD(IDX)    =   WRK-SRYCD2))
                   MOVE    1                   TO  FLG-ERR
               END-IF
      *        処方箋料のチェック
               EVALUATE    WRK-SRYCD
                   WHEN    "120002710"
                   WHEN    "120002910"
                       EVALUATE    WRK-COM-INPUTCD(IDX)
                           WHEN    "120002710"
                           WHEN    "120002910"
                               MOVE    1               TO  FLG-ERR
                       END-EVALUATE
               END-EVALUATE
             END-IF
           END-PERFORM
           MOVE    ZERO                TO  FLG-ALL
           IF      FLG-ERR             =   1
               GO      TO      3002-KASAN-SYORI-EXT
           END-IF
      *
      *    継続分にないこと
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   30      )  OR
                              (FLG-ERR              =   1    )   OR
                              (WRK-TBL-SRYCD(IDX)   =   SPACE)
               IF     (WRK-TBL-SRYCD(IDX)      =   WRK-SRYCD )  OR
                      (WRK-SRYCD2          NOT =   SPACE   AND
                       WRK-TBL-SRYCD(IDX)      =   WRK-SRYCD2)
                   MOVE    1                   TO  FLG-ERR
               END-IF
      *        処方箋料のチェック
               EVALUATE    WRK-SRYCD
                   WHEN    "120002710"
                   WHEN    "120002910"
                       EVALUATE    WRK-TBL-SRYCD (IDX)
                           WHEN    "120002710"
                           WHEN    "120002910"
                               MOVE    1               TO  FLG-ERR
                       END-EVALUATE
               END-EVALUATE
           END-PERFORM
           IF      FLG-ERR             =   1
      *        継続、入院の場合算定しない
               IF     (LNK-ORCSC-CONTKBN    =   "1"  OR  "2" )
      *            入院の時
                   OR (SPA-NYUGAIKBN        =   "1"  )
                   GO      TO      3002-KASAN-SYORI-EXT
               ELSE
                   MOVE    ZERO                TO  FLG-ERR
                   IF      FLG-SINSATU         =   ZERO
                       MOVE    1               TO  SPA-SYOHO-KBN
                       MOVE    WRK-TBL-HKNCOMBI
                                               TO  SPA-SYOHO-HKN
                       PERFORM 30021-DOUJI-HEN-SEC
                   END-IF
               END-IF
           END-IF
      *
           PERFORM 3001-KYOUTU-CHEK-SEC
           IF      FLG-ERR             =   1
               GO      TO      3002-KASAN-SYORI-EXT
           END-IF
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    1                   TO  FLG-TENSUPLUS-CHK
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
      *!       MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  FLG-ERR
               GO      TO      3002-KASAN-SYORI-EXT
           END-IF
      *
      *    算定履歴チェック処理
           IF      TNS-SANTEIRRKKBN    NOT =   ZERO
               PERFORM 3001-SANTEI-CHK-SEC
           END-IF
           IF      FLG-ERR             =   ZERO
               ADD     1               TO  IDXS
               IF      IDXS            >   30
                   MOVE    "XXXX"          TO  SPA-ERRCD
                   GO      TO     3002-KASAN-SYORI-EXT
               END-IF
               MOVE    WRK-SRYCD       TO  LNK-ORCSC-SRYCD
                                                   (02 IDXS)
               MOVE    TNS-TEN         TO  LNK-ORCSC-TENSU
                                                   (02 IDXS)
      *        減算点数
               IF     (WRK-SRYCD       =   "630010002")
                 OR   (WRK-SRYCD       =   CONS-TAZAI-SRYCD )
                 OR   (WRK-SRYCD       =   CONS-TEIHP-SRYCD )
                 OR   (WRK-SRYCD       =   CONS-SPGEN-SRYCD )
                   MOVE    WRK-SANTENSU    TO  LNK-ORCSC-TENSU
                                                   (02 IDXS)
               END-IF
               MOVE    WRK-SRYKA       TO  LNK-ORCSC-SRYKA
                                                         (02 IDXS)
               MOVE    WRK-HKNCOMBI    TO  LNK-ORCSC-HKNCOMBI
                                                         (02 IDXS)
               MOVE    WRK-CHIKENKBN   TO  LNK-ORCSC-CHIKENKBN
                                                         (02 IDXS)
               IF      WRK-SRYCD (1:1)     =   "1"
                   MOVE    TNS-SANTEIRRKKBN
                                           TO  LNK-ORCSC-SANTEIRRKKBN
                                                          (02 IDXS)
               END-IF
      *        年齢加算処理
               PERFORM 3002-NENREI-KASAN-SEC
           END-IF
      *
           .
       3002-KASAN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    共通チェック処理
      *****************************************************************
       3001-KYOUTU-CHEK-SEC         SECTION.
      *
           MOVE    WRK-SCR-REC         TO  HOZ-SCR-REC
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "3"                 TO  LNK-SC92-KBN
      *
           COMPUTE LNK-SC92-GMN-IDX    =   WRK-GMN-MAX
                                       +   1
           IF      LNK-SC92-GMN-IDX    >   SPA-MAX-LINE
               MOVE    SPA-MAX-LINE        TO  LNK-SC92-GMN-IDX
           END-IF
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKA
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           MOVE    LNK-ORCSC-KT01-AREA
                                       TO  LNK-SC92-KT01-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       HOZ-SCR-REC
                                       SPA-AREA
      *    エラーの時、追加なし
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
      *        追加なし
               MOVE    1               TO  FLG-ERR
           ELSE
      *        追加あり
               MOVE    ZERO            TO  FLG-ERR
           END-IF
      *    長期の時、併用・回数エラーは別へ
           IF      FLG-CHOKIKSN        =   1
               MOVE    LNK-SC92-ERRCD3     TO  WRK-ERRCD3
               MOVE    LNK-SC92-ERRCD6     TO  WRK-ERRCD6
               MOVE    SPACE               TO  LNK-SC92-ERRCD3
               MOVE    SPACE               TO  LNK-SC92-ERRCD6
               IF     (SPA-ERRCD           NOT =   SPACE )  OR
                      (LNK-SC92-ERRAREA    NOT =   SPACE )
      *            追加なし
                   MOVE    1               TO  FLG-ERR
               ELSE
                   MOVE    ZERO            TO  FLG-ERR
               END-IF
           END-IF
           MOVE    ZERO                TO  FLG-CHOKIKSN
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  LNK-SC92-ERRAREA
      *
           .
       3001-KYOUTU-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    調剤料の他保険の算定済み編集処理
      *****************************************************************
       30021-DOUJI-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-HEN-MEI
           EVALUATE    WRK-SRYCD
               WHEN    "120000710"
                   MOVE    "内服調剤料"        TO  WRK-HEN-MEI
               WHEN    "120001010"
                   MOVE    "外用調剤料"        TO  WRK-HEN-MEI
               WHEN    "120001210"
               WHEN    "120002610"
                   MOVE    "処方料"            TO  WRK-HEN-MEI
               WHEN    "120002710"
               WHEN    "120002910"
                   MOVE    "処方せん料"         TO  WRK-HEN-MEI
           END-EVALUATE
           IF      WRK-HEN-MEI         =   SPACE
               GO      TO      30021-DOUJI-HEN-EXT
           END-IF
      *
           IF      WRK-HENSYU-MEISYO   =   SPACE
               MOVE    WRK-HEN-MEI         TO  WRK-HENSYU-MEISYO
           ELSE
               STRING  WRK-HENSYU-MEISYO   DELIMITED  BY  SPACE
                       "、"                DELIMITED  BY  SIZE
                       WRK-HEN-MEI         DELIMITED  BY  SPACE
                                           INTO      WRK-HENSYU-MEISYO
               END-STRING
           END-IF
      *
           .
       30021-DOUJI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算料算定処理
      *****************************************************************
       3002-KASAN-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
      *
      *    今回入力にないこと
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                              (FLG-ERR              =   1    )   OR
                              (WRK-COM-INPUTCD(IDX) =   SPACE  AND
                               WRK-GMN-INPUTCD(IDX) =   SPACE)
               IF     (WRK-COM-INPUTCD(IDX)    =   WRK-SRYCD )  OR
                      (WRK-SRYCD2          NOT =   SPACE   AND
                       WRK-COM-INPUTCD(IDX)    =   WRK-SRYCD2)
                   MOVE    1                   TO  FLG-ERR
               END-IF
      *        処方箋料のチェック
               EVALUATE    WRK-SRYCD
                   WHEN    "120002710"
                   WHEN    "120002910"
                       EVALUATE    WRK-COM-INPUTCD (IDX)
                           WHEN    "120002710"
                           WHEN    "120002910"
                               MOVE    1               TO  FLG-ERR
                      END-EVALUATE
               END-EVALUATE
           END-PERFORM
      *
           .
       3002-KASAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算料算定チェック（薬剤情報提供料）処理
      *****************************************************************
       3003-YAKJYO-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
           MOVE    ZERO                TO  FLG-YAKUJYO
      *
      *    今回入力にないこと
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                              (FLG-ERR              =   1    )   OR
                              (WRK-COM-INPUTCD(IDX) =   SPACE  AND
                               WRK-GMN-INPUTCD(IDX) =   SPACE)
               IF     (WRK-COM-INPUTCD(IDX)    =   WRK-SRYCD )  OR
                     ((WRK-SRYCD2          NOT =   SPACE  )  AND
                       (WRK-COM-INPUTCD(IDX)   =   WRK-SRYCD2))
                   MOVE    1                   TO  FLG-YAKUJYO
     ************* MOVE    WRK-COM-MAE-AUTOKBN(IDX)
     ************                          TO   WRK-COM-AUTOKBN(IDX)
               END-IF
           END-PERFORM
           IF      FLG-YAKUJYO         =   1
               MOVE    1                   TO  FLG-ERR
               GO      TO      3003-YAKJYO-CHK-EXT
           END-IF
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    1                   TO  FLG-TENSUPLUS-CHK
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
               MOVE    1                   TO  FLG-ERR
               GO      TO      3003-YAKJYO-CHK-EXT
           END-IF
      *    算定履歴チェック処理
           IF      TNS-SANTEIRRKKBN    NOT =   ZERO
               PERFORM 3001-SANTEI-CHK-SEC
           END-IF
      *
           MOVE    ZERO            TO  FLG-ERR
           IF     (LNK-SC91-ERRCD      =   SPACE )  AND
                  (LNK-SC91-ERRCD2     =   SPACE )
               MOVE    ZERO            TO  FLG-ERR
           ELSE
               IF     (LNK-ORCSC-ROUJIN    =   0    )  AND
                      (LNK-ORCSC-YAKJYO-1  =   1    )
                   MOVE    1                   TO  FLG-ERR
               END-IF
               IF     (LNK-ORCSC-ROUJIN    =   1    )  AND
                     ((LNK-ORCSC-YAKJYO-2  =   1    )  OR
                      (LNK-ORCSC-YAKJYO-3  =   1    ))
                   MOVE    1                   TO  FLG-ERR
               END-IF
           END-IF
      *
           IF      FLG-ERR             =   ZERO
      *        併用算定チェック
               PERFORM 30012-KAYJYO-SENCHK-SEC
           END-IF
      *
      *    手帳加算確認メッセージ
           IF     (SPA-TECHOKBN-FLG    =   "1"  )  AND
                  (FLG-ERR             =   ZERO )
               IF      SPA-TECHOKBN-CHK    =   ZERO 
                   MOVE    "2003"          TO  LNK-ORCSC-KIDCD
                   GO     TO  3003-YAKJYO-CHK-EXT
               END-IF
           END-IF
      *
           IF      FLG-ERR             =   ZERO
      *!!!
               MOVE    SPACE               TO  WRK-SRYCD2
      *        手帳加算の判定（老人のみ）
               IF     (LNK-ORCSC-YAKJYO-2      =    1
                                               OR   2  ) AND
                      (LNK-ORCSC-ROUJIN    NOT =   0   )
                   IF      SPA-TECHOKBN-CHK    =   ZERO 
                       MOVE    "2003"          TO  LNK-ORCSC-KIDCD
                       GO     TO  3003-YAKJYO-CHK-EXT
                   END-IF
                   IF      SPA-TECHOKBN-CHK    =   1
                       MOVE    SPACE               TO  WRK-SRYCD2
                   ELSE
                       MOVE    "113701310"         TO  WRK-SRYCD2
                   END-IF
               END-IF
      *!!!
               IF     (SPA-TECHOKBN-FLG    =   "1"  )
                   IF      SPA-TECHOKBN-CHK    =   1
                       MOVE    SPACE               TO  WRK-SRYCD2
                   ELSE
                       MOVE    "113701310"         TO  WRK-SRYCD2
                   END-IF
               END-IF
      *
               ADD     1               TO  IDXS
               MOVE    WRK-SRYCD       TO  LNK-ORCSC-SRYCD
                                                   (02 IDXS)
               MOVE    TNS-TEN         TO  LNK-ORCSC-TENSU
                                                   (02 IDXS)
               MOVE    WRK-SRYKA       TO  LNK-ORCSC-SRYKA
                                                         (02 IDXS)
               MOVE    WRK-HKNCOMBI    TO  LNK-ORCSC-HKNCOMBI
                                                         (02 IDXS)
               MOVE    WRK-CHIKENKBN   TO  LNK-ORCSC-CHIKENKBN
                                                         (02 IDXS)
               IF      WRK-SRYCD (1:1)     =   "1"
                   MOVE    TNS-SANTEIRRKKBN
                                           TO  LNK-ORCSC-SANTEIRRKKBN
                                                          (02 IDXS)
               END-IF
      *        年齢加算処理
               PERFORM 3002-NENREI-KASAN-SEC
      *!!!
               IF      WRK-SRYCD2      NOT =   SPACE
                    MOVE    WRK-SRYCD2          TO  TNS-SRYCD
                    MOVE    ZERO                TO  FLG-TENSUPLUS-CHK
                    PERFORM 910-TENSU-READ-SEC
                    IF      FLG-TENSU           =   ZERO
      *
      *                加算料計算
                       MOVE    WRK-SRYCD2      TO  LNK-ORCSC-SRYCD-KS
                                                       (02 IDXS)
                       MOVE    TNS-TEN         TO  LNK-ORCSC-TENSU-KS
                                                       (02 IDXS)
                   END-IF
               END-IF
      *!!!
           END-IF
      *
           .
       3003-YAKJYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    併用算定チェック処理
      *****************************************************************
       30012-KAYJYO-SENCHK-SEC            SECTION.
      *
           MOVE    WRK-SCR-REC         TO  HOZ-SCR-REC
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "4"                 TO  LNK-SC92-KBN
      *
           COMPUTE LNK-SC92-GMN-IDX    =   WRK-GMN-MAX
                                       +   1
           IF      LNK-SC92-GMN-IDX    >   SPA-MAX-LINE
               MOVE    SPA-MAX-LINE        TO  LNK-SC92-GMN-IDX
           END-IF
           MOVE    WRK-SRYCD           TO  HOZ-COM-INPUTCD
                                                   (LNK-SC92-GMN-IDX)
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKA
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           MOVE    LNK-ORCSC-KT01-AREA
                                       TO  LNK-SC92-KT01-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       HOZ-SCR-REC
                                       SPA-AREA
      *    エラーの時、追加なし
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
      *        追加なし
               MOVE    1               TO  FLG-ERR
           ELSE
      *        追加あり
               MOVE    ZERO            TO  FLG-ERR
           END-IF
           .
       30012-KAYJYO-SENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢加算追加処理
      *****************************************************************
       3002-NENREI-KASAN-SEC            SECTION.
      *
      *H30.4
      *    オンライン診療料算定時　年齢加算なし
           IF     (FLG-ONLNE-ARI       =   1  )
             AND  (FLG-ONLNE-AUTO      =   1  )
               GO      TO      3002-NENREI-KASAN-EXT
           END-IF
      *
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   4       )   OR
                              (TNS-AGEKSNSRYCD(IDX2)   =   SPACE  OR
                                                           ZERO )
               MOVE    TNS-AGEKSNSRYCD(IDX2)   TO  WRK-SRYCD
               MOVE    WRK-SRYCD           TO  TNS-SRYCD
               MOVE    ZERO                TO  FLG-TENSUPLUS-CHK
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
      *            年齢チェック
                   PERFORM 30021-NENREI-CHK-SEC
                   IF      FLG-CHK              =   ZERO
      *
      *                加算料計算
                       MOVE    WRK-SRYCD       TO  LNK-ORCSC-SRYCD-KS
                                                       (02 IDXS)
                       MOVE    TNS-TEN         TO  LNK-ORCSC-TENSU-KS
                                                       (02 IDXS)
                       MOVE    4               TO  IDX2
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       3002-NENREI-KASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢チェック処理
      *****************************************************************
       30021-NENREI-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           MOVE    ZERO                TO  WRK-KGNAGE
           MOVE    ZERO                TO  WRK-JGNAGE
           MOVE    TNS-KGNAGE          TO  WRK-KGNAGE(2:2)
           MOVE    TNS-JGNAGE          TO  WRK-JGNAGE(2:2)
      *
      *    下限年齢チェック
           IF      TNS-KGNAGE          =   SPACE  OR  ZERO
               CONTINUE
           ELSE
               IF      TNS-KGNAGE          =   "AA"
      *            新生児判定
                   IF     (LNK-ORCSC-NENREI-YY   =   ZERO )  AND
                          (LNK-ORCSC-NENREI-MM   =   ZERO )  AND
                          (LNK-ORCSC-NENREI-DD   <=  28   )
                       MOVE    1                 TO  FLG-CHK
                   END-IF
               ELSE
                   IF      LNK-ORCSC-NENREI-YY   >=  WRK-KGNAGE
                       CONTINUE
                   ELSE
                       MOVE    1                 TO  FLG-CHK
                   END-IF
               END-IF
           END-IF
      *
      *    上限年齢チェック
           IF      TNS-JGNAGE          =   SPACE  OR  ZERO
               CONTINUE
           ELSE
               IF      TNS-JGNAGE          =   "AA"
      *            新生児判定
                   IF     (LNK-ORCSC-NENREI-YY    =   ZERO )  AND
                          (LNK-ORCSC-NENREI-MM    =   ZERO )  AND
                          (LNK-ORCSC-NENREI-DD    <=  28   )
                       CONTINUE
                   ELSE
                       MOVE    1                 TO  FLG-CHK
                   END-IF
               ELSE
                   IF      LNK-ORCSC-NENREI-YY   <   WRK-JGNAGE
                       CONTINUE
                   ELSE
                       MOVE    1                 TO  FLG-CHK
                   END-IF
               END-IF
           END-IF
      *
           .
       30021-NENREI-CHK-EXT.
           EXIT.
      *
      *H27.4
      *****************************************************************
      *    処方せん料３０日以上の逓減加算編集処理
      *****************************************************************
       30019-GEN30KSN-HENSYU-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
      *
           PERFORM 3001-KYOUTU-CHEK-SEC
           IF      FLG-ERR             =   1
               GO      TO      30019-GEN30KSN-HENSYU-EXT
           END-IF
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    1                   TO  FLG-TENSUPLUS-CHK
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
               MOVE    1                   TO  FLG-ERR
               GO      TO      30019-GEN30KSN-HENSYU-EXT
           END-IF
      *
      *    算定履歴チェック処理
           IF      TNS-SANTEIRRKKBN    NOT =   ZERO
               PERFORM 3001-SANTEI-CHK-SEC
           END-IF
           IF      FLG-ERR         NOT =   ZERO
               GO      TO      30019-GEN30KSN-HENSYU-EXT
           END-IF
      *
           IF      LNK-ORCSC-SRYCD-KS (02 IDXS)    =   SPACE
               MOVE    IDXS            TO  IDXS-2
           ELSE
      *    乳幼児加算コードは、減算の下に変更
               MOVE    IDXS            TO  IDXS-2
               ADD     1               TO  IDXS
               IF      IDXS            >   30
                   GO      TO     30019-GEN30KSN-HENSYU-EXT
               END-IF
               MOVE    LNK-ORCSC-KASN-TBL2 (02 IDXS-2)
                                       TO  LNK-ORCSC-KASN-TBL2 (02 IDXS)
      *        加算以外は空白
               MOVE    SPACE           TO  LNK-ORCSC-SRYCD   (02 IDXS)
               MOVE    ZERO            TO  LNK-ORCSC-TENSU   (02 IDXS)
               MOVE    ZERO            TO  LNK-ORCSC-SURYO   (02 IDXS)
               MOVE    SPACE           TO  LNK-ORCSC-ATAI-G  (02 IDXS)
               MOVE    "NET"           TO  LNK-ORCSC-SRYSYUKBN
                                                             (02 IDXS)
      *        入院時の回数は最終行を有効
               MOVE    SPACE           TO  LNK-ORCSC-INPUTCD-G
                                                         (02 IDXS-2)
           END-IF
      *
      *    加算料計算
           MOVE    WRK-SRYCD           TO  LNK-ORCSC-SRYCD-KS
                                                       (02 IDXS-2)
           MOVE    ZERO                TO  LNK-ORCSC-TENSU-KS
                                                       (02 IDXS-2)
      *    %減算
           IF      TNS-TENSIKIBETU     =   6
      *        手技料点数から減算（端数切り捨て）
               MOVE    ZERO                TO  WRK-SANTEN-G
               COMPUTE WRK-SANTEN-9        =   LNK-ORCSC-TENSU
                                                      (02 IDXS-2)
                                           *  (TNS-TEN
                                           /   100.00  )
               COMPUTE WRK-SANTEN-KEI      =  (LNK-ORCSC-TENSU
                                                      (02 IDXS-2)
                                           -   WRK-SANTEN-9 )
                                           +   0.5
      *        逓減後の手技料点数
               MOVE    WRK-SANTEN-KEI      TO  LNK-ORCSC-TENSU
                                                      (02 IDXS-2)
           END-IF
      *    点数
           IF      TNS-TENSIKIBETU     =   3
               MOVE    TNS-TEN             TO  LNK-ORCSC-TENSU-KS
                                                       (02 IDXS-2)
           END-IF
      *
           .
       30019-GEN30KSN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴チェック処理
      *****************************************************************
       3001-SANTEI-CHK-SEC         SECTION.
      *
           INITIALIZE                  ORCSC91AREA
           MOVE    "2"                 TO  LNK-SC91-KBN
           IF      FLG-TENSUPLUS-CHK   =   1
               MOVE    "1"                 TO  LNK-SC91-KBN
           END-IF
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC91-SYORIFLG
           MOVE    WRK-SRYCD           TO  LNK-SC91-SRYCD
           MOVE    LNK-ORCSC-TEISEI-AREA
                                       TO  LNK-SC91-TEISEI-AREA
           MOVE    LNK-ORCSC-KT01-AREA
                                       TO  LNK-SC91-KT01-AREA
           CALL    "ORCSC91"           USING
                                       MCPAREA
                                       ORCSC91AREA
                                       WRK-SCR-REC
                                       SPA-AREA
                                       TNS-TENSU-REC
      *
           IF      LNK-SC91-ERRCD      =   SPACE
      *        追加あり
               MOVE    ZERO            TO  FLG-ERR
           ELSE
      *        追加なし
               MOVE    1               TO  FLG-ERR
           END-IF
      *
           .
       3001-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    継続の時、算定分の編集処理
      *****************************************************************
       30010-JYURRK-SYORI-SEC         SECTION.
      *
      *    継続分の時、算定
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    1                   TO  JYURRK-RENNUM
               MOVE    "key5"              TO  WRK-ORC-DBPATH
           ELSE
      *        継続の時、算定分の編集
               IF     (LNK-ORCSC-CONTKBN    =   "1"  OR  "2" )
                   MOVE    LNK-ORCSC-DOUJI-RENNUM  TO  JYURRK-RENNUM
                   MOVE    "key5"           TO  WRK-ORC-DBPATH
               ELSE
                   MOVE    "key4"           TO  WRK-ORC-DBPATH
               END-IF
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM
               UNTIL    FLG-JYURRK         =   1
               MOVE     ZERO            TO  FLG-JYURRK-END
               IF      (LNK-ORCSC-SYORIFLG     =   1  )   AND
                       (LNK-ORCSC-DENPNUM      =   JYURRK-DENPNUM)
      *            訂正の時対象外
                   MOVE    1               TO  FLG-JYURRK-END
               ELSE
      *            入院、継続の時、算定分の編集
                   IF      (SPA-NYUGAIKBN        =   "1"    )  OR
                           (LNK-ORCSC-CONTKBN    =   "1"  OR  "2" )
                       CONTINUE
                   ELSE
                       IF      JYURRK-HKNCOMBINUM  =   SPA-HKNCOMBINUM
                           MOVE    1               TO  FLG-JYURRK-END
                       END-IF
                   END-IF
               END-IF
      *        包括まとめは対象外
               IF      JYURRK-HKNCOMBINUM      =   "9999"
                   MOVE    1                   TO  FLG-JYURRK-END
               END-IF
      *
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX                 >   15  )  OR
                                  (FLG-JYURRK-END      =   1    ) OR
                                  (JYURRK-ZAINUM(IDX)  =   ZERO )
      *            診療会計マスタ読み込み
                   MOVE    SPACE               TO  SRYACCT-REC
                   INITIALIZE                      SRYACCT-REC
                   MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
                   MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
                   MOVE    SPA-PTID            TO  ACCT-PTID
                   MOVE    SPA-SRYKA           TO  ACCT-SRYKA
                   MOVE    SPA-SRYYMD(1:6)     TO  ACCT-SRYYM
                   MOVE    JYURRK-ZAINUM(IDX)  TO  ACCT-ZAINUM
      *
                   MOVE    SRYACCT-REC         TO  MCPDATA-REC
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_sryacct"       TO  MCP-TABLE
                       MOVE    "key5"              TO  MCP-PATHNAME
                       PERFORM 940-SRYACCT-READ-SEC
                   ELSE
                       MOVE    1                   TO  FLG-SRYACCT
                   END-IF
                   PERFORM UNTIL    FLG-SRYACCT    =   1
      *
                      COMPUTE IDX-Y3              =   JYURRK-RENNUM
                                                  +   1
      *ver.4.8
      *               IF      IDX-Y3              >   4
      *                    MOVE    4               TO  IDX-Y3
      ****            END-IF
                      MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
                      IF      (ACCT-DAY (IDX-Y3 IDX-Y2)
                                                   >   ZERO  ) AND
                              (ACCT-SRYKBN(1:1)    =   "2"   OR
                               ACCT-SRYKBN         =   "80"  )
      *                    診療行為マスター編集
                           PERFORM 300011-YAKFTN-SRYACT-SEC
                       END-IF
      *
                       MOVE    "tbl_sryacct"       TO  MCP-TABLE
                       MOVE    "key5"              TO  MCP-PATHNAME
                       PERFORM 940-SRYACCT-READ-SEC
                   END-PERFORM
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
               END-PERFORM
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       30010-JYURRK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター編集処理
      *****************************************************************
       300011-YAKFTN-SRYACT-SEC            SECTION.
      *
      *    診療会計マスターから診療行為マスターを検索する
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL      FLG-SRYACT   =   1
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   5   )  OR
                                  (SRY-SRYCD(IDX2)     =   SPACE )
      *
      *            投薬の自動発生チェック用
                   IF     ( SRY-SRYCD (IDX2)(1:1)  =   "1"   )  AND
                          ( SRY-SRYKBN(1:1)        =   "2"   OR
                            SRY-SRYSYUKBN          =   "820" )
                       ADD     1                   TO  IDX-TBL
                       EVALUATE     SRY-SRYCD (IDX2)
      *                処方箋料
      *                    WHEN    "180016410"
      *                    WHEN    "180011110"
      *                        MOVE    "180011110"     TO  WRK-SRYCD
      *                 処方料
                           WHEN    "120002610"
                           WHEN    "120001210"
                               MOVE    "120001210"     TO  WRK-SRYCD
                           WHEN     OTHER
                               MOVE      SRY-SRYCD (IDX2)
                                                   TO  WRK-SRYCD
                       END-EVALUATE
                       MOVE    WRK-SRYCD           TO  WRK-TBL-SRYCD
                                                        (IDX-TBL)
                       MOVE    JYURRK-HKNCOMBINUM  TO  WRK-TBL-HKNCOMBI
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       300011-YAKFTN-SRYACT-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
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
      *    附加情報の上限回数設定
           IF     (FLG-TENSU           =   ZERO )  AND
                  (FLG-TENSUPLUS-CHK   =   1    )
               PERFORM 9101-TENSUPLUS-SYORI-SEC
           END-IF
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報より編集処理
      *****************************************************************
       9101-TENSUPLUS-SYORI-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    SPA-SRYYMD          TO  TNSP-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNSP-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
                   MOVE    ZERO                TO  FLG-TENSUPLUS
               ELSE
                   MOVE    1                   TO  FLG-TENSUPLUS
                   INITIALIZE                      TENSUPLUS-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSUPLUS
               INITIALIZE                      TENSUPLUS-REC
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    上限回数
           IF      TNSP-SANTEIRRKKBN   =   1
               MOVE    TNSP-SANTEIRRKKBN   TO  TNS-SANTEIRRKKBN
               MOVE    TNSP-JGNCNT         TO  TNS-JGNCNT
               MOVE    TNSP-JGNCNT1D       TO  TNS-JGNCNT1D
               MOVE    TNSP-JGNCNTERR      TO  TNS-JGNCNTERR
           END-IF
      *
           .
       9101-TENSUPLUS-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       900-SRYACT-NEXT-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ読み込み
      *****************************************************************
       940-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       940-SRYACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴マスター読込
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
