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
      *----------------------------------------------------------------*
      *    算定履歴保険番号毎のコードテーブル COPY (CMSANTEITBL.INC)
      *----------------------------------------------------------------*
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  02/08/09  療養の給付請求書取扱料追加
      *  01.00.02    NACL-多々納  04/03/15  Ｈ１６年４月改正追加
      *                                     介達牽引追加
      *                                     言語視覚療法３追加
      *  01.00.03    NACL-多々納  04/12/02  急性発生日追加
      *  03.03.00    NACL-多々納  06/10/20  矯正固定等追加
      *  03.04.00    NACL-多々納  07/01/11  リハビリ終了日追加
      *  03.04.01    NACL-多々納  07/03/XX  医学管理料追加
      *  04.08.00    NACL-多々納  16/03/XX  Ｈ２８年４月改正対応
      *****************************************************************
      *
      *    算定履歴保険番号毎のコード
       01  TBL-SANTEI-AREA.
      *    理学療法（個別）１０単位チェックテーブル
           03  FILLER              PIC X(13)   VALUE   "KR 180022110".
           03  FILLER              PIC X(13)   VALUE   "KR 180022310".
           03  FILLER              PIC X(13)   VALUE   "KR 180022510".
           03  FILLER              PIC X(13)   VALUE   "KR 180022710".
      *    作業療法（個別）
           03  FILLER              PIC X(13)   VALUE   "KS 180023610".
           03  FILLER              PIC X(13)   VALUE   "KS 180023810".
      *    言語療法（個別）
           03  FILLER              PIC X(13)   VALUE   "KG 180024810".
           03  FILLER              PIC X(13)   VALUE   "KG 180025010".
      *    言語療法３（個別）
           03  FILLER              PIC X(13)   VALUE   "KG 180025410".
      *    理学療法（個別）（老人）
           03  FILLER              PIC X(13)   VALUE   "KR 180706810".
           03  FILLER              PIC X(13)   VALUE   "KR 180707010".
           03  FILLER              PIC X(13)   VALUE   "KR 180707210".
           03  FILLER              PIC X(13)   VALUE   "KR 180707410".
      *    作業療法（個別）（老人）
           03  FILLER              PIC X(13)   VALUE   "KS 180708110".
           03  FILLER              PIC X(13)   VALUE   "KS 180708310".
      *
      *    理学療法（集団）
           03  FILLER              PIC X(13)   VALUE   "SR 180022210".
           03  FILLER              PIC X(13)   VALUE   "SR 180022410".
           03  FILLER              PIC X(13)   VALUE   "SR 180022610".
           03  FILLER              PIC X(13)   VALUE   "SR 180022810".
      *    作業療法（集団）
           03  FILLER              PIC X(13)   VALUE   "SS 180023710".
           03  FILLER              PIC X(13)   VALUE   "SS 180023910".
      *    言語療法（集団）
           03  FILLER              PIC X(13)   VALUE   "SG 180024910".
           03  FILLER              PIC X(13)   VALUE   "SG 180025110".
      *    言語療法３（集団）
           03  FILLER              PIC X(13)   VALUE   "SG 180025510".
      *    理学療法（集団）（老人）
           03  FILLER              PIC X(13)   VALUE   "SR 180706910".
           03  FILLER              PIC X(13)   VALUE   "SR 180707110".
           03  FILLER              PIC X(13)   VALUE   "SR 180707310".
           03  FILLER              PIC X(13)   VALUE   "SR 180707510".
      *    作業療法（集団）（老人）
           03  FILLER              PIC X(13)   VALUE   "SS 180708210".
           03  FILLER              PIC X(13)   VALUE   "SS 180708410".
      *
      *    消炎鎮痛処置
      *    マッサージ等の手技による
           03  FILLER              PIC X(13)   VALUE   " M 140029610".
      *    器具等による
           03  FILLER              PIC X(13)   VALUE   " K 140040310".
      *    湿布処置１
           03  FILLER              PIC X(13)   VALUE   " P 140002210".
      *    湿布処置２
           03  FILLER              PIC X(13)   VALUE   " P 140002310".
      *    介達牽引
           03  FILLER              PIC X(13)   VALUE   " T 140048010".
      *H18/04から
      *    介達牽引（矯正固定）
           03  FILLER              PIC X(13)   VALUE   "HT 140030350".
      *    介達牽引（変形機械矯正術）
           03  FILLER              PIC X(13)   VALUE   "HT 140048250".
      *    腰部固定帯固定
           03  FILLER              PIC X(13)   VALUE   "HE 140048350".
      *    胸部固定帯固定
           03  FILLER              PIC X(13)   VALUE   "HE 140048450".
      *    低出力レーザ照射
           03  FILLER              PIC X(13)   VALUE   "HE 140048550".
      *    肛門処置
           03  FILLER              PIC X(13)   VALUE   "HE 140002450".
      *
      *    労災の救急医療管理加算
           03  FILLER              PIC X(13)   VALUE   "  R101110020".
           03  FILLER              PIC X(13)   VALUE   "  R101110030".
      *    労災の再就労療養指導管理料
           03  FILLER              PIC X(13)   VALUE   "   101130010".
      *    労災の療養の給付請求書取扱料
           03  FILLER              PIC X(13)   VALUE   "  R101130020".
      *    労災の初診時ブラッシング料
           03  FILLER              PIC X(13)   VALUE   "  R101400030".
      *    労災の手指の創傷に係る機能回復指導加算
           03  FILLER              PIC X(13)   VALUE   "  R101500080".
      *
      *    脳血管急性発生日
           03  FILLER              PIC X(13)   VALUE   "   099800101".
      *    脳血管急性発生終了
           03  FILLER              PIC X(13)   VALUE   "   099800102".
      *Ｈ１８／４から
      *    リハビリ算定回数（？？）
           03  FILLER              PIC X(13)   VALUE   "   099800201".
      *    リハビリ各発生日
           03  FILLER              PIC X(13)   VALUE   "  H0998001111".
           03  FILLER              PIC X(13)   VALUE   "  H0998001212".
           03  FILLER              PIC X(13)   VALUE   "  H0998001313".
           03  FILLER              PIC X(13)   VALUE   "  H0998001414".
           03  FILLER              PIC X(13)   VALUE   "   099800151".
           03  FILLER              PIC X(13)   VALUE   "   099800161".
           03  FILLER              PIC X(13)   VALUE   "   099800171".
           03  FILLER              PIC X(13)   VALUE   "  H0998001919".
      *    リハビリ各終了日
           03  FILLER              PIC X(13)   VALUE   "  E0998001121".
           03  FILLER              PIC X(13)   VALUE   "  E0998001222".
           03  FILLER              PIC X(13)   VALUE   "  E0998001323".
           03  FILLER              PIC X(13)   VALUE   "  E0998001424".
           03  FILLER              PIC X(13)   VALUE   "   099800152".
           03  FILLER              PIC X(13)   VALUE   "   099800162".
           03  FILLER              PIC X(13)   VALUE   "   099800172".
           03  FILLER              PIC X(13)   VALUE   "  E0998001929".
      *自賠責健保準拠追加（Ｈ１８／４から）
      *    ＣＴ・ＭＲＩ算定
           03  FILLER              PIC X(13)   VALUE   "   099700101".
      *リハビリ医学管理料追加（Ｈ１９／４から）
           03  FILLER              PIC X(13)   VALUE   "  S1800299101".
           03  FILLER              PIC X(13)   VALUE   "  S1800300101".
           03  FILLER              PIC X(13)   VALUE   "  S1800301102".
           03  FILLER              PIC X(13)   VALUE   "  S1800302102".
           03  FILLER              PIC X(13)   VALUE   "  S1800303103".
           03  FILLER              PIC X(13)   VALUE   "  S1800304103".
           03  FILLER              PIC X(13)   VALUE   "  S1800305104".
           03  FILLER              PIC X(13)   VALUE   "  S1800306104".
      *リハビリ医学管理料算定時の実施日（Ｈ１９／４）
           03  FILLER              PIC X(13)   VALUE   "   099800311".
           03  FILLER              PIC X(13)   VALUE   "   099800321".
           03  FILLER              PIC X(13)   VALUE   "   099800331".
           03  FILLER              PIC X(13)   VALUE   "   099800341".
      *
       01  TBL-SANTEI-AREA-R       REDEFINES   TBL-SANTEI-AREA.
      **** 03  TBL-SANTEI-OCC      OCCURS   39  INDEXED   TBL-SAN.
      *****03  TBL-SANTEI-OCC      OCCURS   41  INDEXED   TBL-SAN.
      **** 03  TBL-SANTEI-OCC      OCCURS   65  INDEXED   TBL-SAN.
      **** 03  TBL-SANTEI-OCC      OCCURS   73  INDEXED   TBL-SAN.
           03  TBL-SANTEI-OCC      OCCURS   79  INDEXED   TBL-SAN.
      *        リハビリ（K:個別、S:集団）(H:Ｈ１８．４から）
               05  TBL-RIGAKU-RYO      PIC X(01).
      *        リハビリ（R:理学,S:作業, G:言語)
      *        消炎鎮痛処置(M:マッサージ等,K:器具等、P:湿布
      *                     T:介達牽引、E:その他)
               05  TBL-RIGAKU-KBN      PIC X(01).
      *        (R:労災毎１回),(S:リハビリ医学管理料)
               05  TBL-RSI-KBN         PIC X(01).
      *
               05  TBL-SANTEI-SRYCD    PIC X(09).
      *        疾患別リハビリ区分(1:心大血管,2:脳血管,3:運動器,4:呼吸器
      *                           9:廃用症候群)
               05  TBL-RIHA-SYUKBN     PIC X(01).
      *
      *    保険毎算定あり
       01  FLG-HKNSAN                  PIC 9(01).
      *    リハビリ有無
       01  FLG-HKN-SANTEI              PIC 9(01).
      *    労災毎算定
       01  WRK-RSI-KBN                 PIC X(01).
      *
