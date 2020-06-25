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
      ************************************************************
      *    保険組合せ　    　 COPY  (CPHKNCOMBI.INC)             *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  02/09/09  支払区分追加
      *  04.01.00    NACL-多々納  07/11/XX  表示区分追加
      ************************************************************
      *01  HKNCOMBI-REC.
      *キー
           03 COMB-KEY.
      *医療機関ＩＤ
               05 COMB-HOSPNUM          PIC 9(2).
      *患者ＩＤ
               05 COMB-PTID            PIC  9(10).
      *組合せ番号
               05 COMB-HKNCOMBINUM     PIC  9(04).
      *主保険−ＩＤ
           03 COMB-HKNID               PIC  9(10).
      *主保険−保険番号
           03 COMB-HKNNUM              PIC  X(03).
      *主保険−本人家族区分
           03 COMB-HONKZKKBN           PIC  X(01).
      *主保険−補助区分
           03 COMB-HOJOKBN             PIC  X(01).
      *主保険−継続区分
           03 COMB-CONTKBN             PIC  X(01).
      *公費領域
           03 COMB-KOHI-AREA.
      *公費１−ＩＤ
               05 COMB-KOH1ID              PIC  9(10).
      *公費１−保険番号
               05 COMB-KOH1HKNNUM          PIC  X(03).
      *公費１−支払区分
               05 COMB-KOH1PAYKBN          PIC  X(02).
      *公費２−ＩＤ
               05 COMB-KOH2ID              PIC  9(10).
      *公費２−保険番号
               05 COMB-KOH2HKNNUM          PIC  X(03).
      *公費２−支払区分
               05 COMB-KOH2PAYKBN          PIC  X(02).
      *公費３−ＩＤ
               05 COMB-KOH3ID              PIC  9(10).
      *公費３−保険番号
               05 COMB-KOH3HKNNUM          PIC  X(03).
      *公費３−支払区分
               05 COMB-KOH3PAYKBN          PIC  X(02).
      *公費４−ＩＤ
               05 COMB-KOH4ID              PIC  9(10).
      *公費４−保険番号
               05 COMB-KOH4HKNNUM          PIC  X(03).
      *公費４−支払区分
               05 COMB-KOH4PAYKBN          PIC  X(02).
           03 COMB-KOHI-AREA-R     REDEFINES  COMB-KOHI-AREA
                                           OCCURS   4.
      *公費−ＩＤ
               05 COMB-KOHID              PIC  9(10).
      *公費−保険番号
               05 COMB-KOHHKNNUM          PIC  X(03).
      *公費−支払区分
               05 COMB-KOHPAYKBN          PIC  X(02).
      *適用開始日
           03 COMB-TEKSTYMD               PIC  X(08).
      *適用終了日
           03 COMB-TEKEDYMD               PIC  X(08).
      *主保険−短縮制度名
           03 COMB-SYU-TANSEIDONAME       PIC  X(40).
      *主保険−保険公費種別
           03 COMB-SYU-HKNKOHSBTKBN       PIC  X(01).
      *主保険−外来−上限区分
           03 COMB-SYU-GAIJGNKBN          PIC  X(01).
      *主保険−入院−上限区分
           03 COMB-SYU-NYUJGNKBN          PIC  X(01).
      *公費−短縮制度名
           03 COMB-KOHTANSEIDO.
      *公費１−短縮制度名
               05 COMB-KOH1-TANSEIDONAME  PIC  X(20).
      *公費１−保険公費種別
               05 COMB-KOH1-HKNKOHSBTKBN  PIC  X(01).
      *公費１−外来−上限区分
               05 COMB-KOH1-GAIJGNKBN     PIC  X(01).
      *公費１−入院−上限区分
               05 COMB-KOH1-NYUJGNKBN     PIC  X(01).
      *公費２−短縮制度名
               05 COMB-KOH2-TANSEIDONAME  PIC  X(20).
      *公費２−保険公費種別
               05 COMB-KOH2-HKNKOHSBTKBN  PIC  X(01).
      *公費２−外来−上限区分
               05 COMB-KOH2-GAIJGNKBN     PIC  X(01).
      *公費２−入院−上限区分
               05 COMB-KOH2-NYUJGNKBN     PIC  X(01).
      *公費３−短縮制度名
               05 COMB-KOH3-TANSEIDONAME  PIC  X(20).
      *公費３−保険公費種別
               05 COMB-KOH3-HKNKOHSBTKBN  PIC  X(01).
      *公費３−外来−上限区分
               05 COMB-KOH3-GAIJGNKBN     PIC  X(01).
      *公費３−入院−上限区分
               05 COMB-KOH3-NYUJGNKBN     PIC  X(01).
      *公費４−短縮制度名
               05 COMB-KOH4-TANSEIDONAME  PIC  X(20).
      *公費４−保険公費種別
               05 COMB-KOH4-HKNKOHSBTKBN  PIC  X(01).
      *公費４−外来−上限区分
               05 COMB-KOH4-GAIJGNKBN     PIC  X(01).
      *公費４−入院−上限区分
               05 COMB-KOH4-NYUJGNKBN     PIC  X(01).
      *公費−短縮制度名
           03 COMB-KOHTANSEIDO-R  REDEFINES  COMB-KOHTANSEIDO
                                           OCCURS  4.
      *公費−短縮制度名
               05 COMB-KOH-TANSEIDONAME   PIC  X(20).
      *公費−保険公費種別
               05 COMB-KOH-HKNKOHSBTKBN   PIC  X(01).
      *公費−外来−上限区分
               05 COMB-KOH-GAIJGNKBN      PIC  X(01).
      *公費−入院−上限区分
               05 COMB-KOH-NYUJGNKBN      PIC  X(01).
      *外来−入院
           03 COMB-GAINYU.
      *外来−負担区分
             05 COMB-GAI-FTNKBN           PIC  X(01).
      *  外来−内容
             05 COMB-GAI-NAIYO.
      *    外来−回−負担割合
               07 COMB-GAI-KFTNRATE           PIC  9(03).
      *    外来−回−固定額
               07 COMB-GAI-KKOTGAK            PIC  9(06).
      *    外来−回−上限額
               07 COMB-GAI-KJGNGAK            PIC  9(06).
      *    外来−日−上限額
               07 COMB-GAI-DJGNGAK            PIC  9(06).
      *    外来−日−上限回数
               07 COMB-GAI-DJGNCNT            PIC  9(02).
      *    外来−月−院内−上限額
               07 COMB-GAI-MNAIJGNGAK         PIC  9(06).
      *    外来−月−院外−上限額
               07 COMB-GAI-MGAIJGNGAK         PIC  9(06).
      *    外来−月−上限回数
               07 COMB-GAI-MJGNCNT            PIC  9(02).
      *  外来−薬剤負担区分
             05 COMB-GAI-YKZFTNKBN        PIC  X(01).
      *  入院−負担区分
             05 COMB-NYU-FTNKBN           PIC  X(01).
      *  入院−内容
             05 COMB-NYU-NAIYO.
      *    入院−回−負担割合
               07 COMB-NYU-KFTNRATE            PIC  9(03).
      *    入院−回−固定額
               07 COMB-NYU-KKOTGAK             PIC  9(06).
      *    入院−回−上限額
               07 COMB-NYU-KJGNGAK             PIC  9(06).
      *    入院−日−上限額
               07 COMB-NYU-DJGNGAK             PIC  9(06).
      *    入院−日−上限回数
               07 COMB-NYU-DJGNCNT             PIC  9(02).
      *    入院−月−上限額
               07 COMB-NYU-MJGNGAK             PIC  9(06).
      *    入院−月−上限回数
               07 COMB-NYU-MJGNCNT             PIC  9(02).
      *  入院−食事療養費区分
             05 COMB-NYU-SKJRYOKBN        PIC  X(01).
      *低所得
           03 COMB-TSY.
      *  低所得−外来−負担区分
             05 COMB-TSY-GAIFTNKBN        PIC  X(01).
      *  低所得−内容
             05 COMB-TSY-GAI-NAIYO.
      *    低所得−外来−回−負担割合
               07 COMB-TSY-GAIKFTNRATE         PIC  9(03).
      *    低所得−外来−回−固定額
               07 COMB-TSY-GAIKKOTGAK          PIC  9(06).
      *    低所得−外来−回−上限額
               07 COMB-TSY-GAIKJGNGAK          PIC  9(06).
      *    低所得−外来−日−上限額
               07 COMB-TSY-GAIDJGNGAK          PIC  9(06).
      *    低所得−外来−日−上限回数
               07 COMB-TSY-GAIDJGNCNT          PIC  9(02).
      *    低所得−外来−月−院内−上限額
               07 COMB-TSY-GAIMNAIJGNGAK       PIC  9(06).
      *    低所得−外来−月−院外−上限額
               07 COMB-TSY-GAIMGAIJGNGAK       PIC  9(06).
      *    低所得−外来−月−上限回数
               07 COMB-TSY-GAIMJGNCNT          PIC  9(02).
      *  低所得−外来−薬剤負担区分
             05 COMB-TSY-GAIYKZFTNKBN     PIC  X(01).
      *  低所得−入院−負担区分
             05 COMB-TSY-NYUFTNKBN        PIC  X(01).
      *  低所得−入院−内容
             05 COMB-TSY-NYU-NAIYO.
      *    低所得−入院−回−負担割合
               07 COMB-TSY-NYUKFTNRATE        PIC  9(03).
      *    低所得−入院−回−固定額
               07 COMB-TSY-NYUKKOTGAK         PIC  9(06).
      *    低所得−入院−回−上限額
               07 COMB-TSY-NYUKJGNGAK         PIC  9(06).
      *    低所得−入院−日−上限額
               07 COMB-TSY-NYUDJGNGAK         PIC  9(06).
      *    低所得−入院−日−上限回数
               07 COMB-TSY-NYUDJGNCNT         PIC  9(02).
      *    低所得−入院−月−上限額
               07 COMB-TSY-NYUMJGNGAK         PIC  9(06).
      *    低所得−入院−月−上限回数
               07 COMB-TSY-NYUMJGNCNT         PIC  9(02).
      *  低所得−入院−食事療養区分
             05 COMB-TSY-NYUSKJRYOKBN     PIC  X(01).
      *低年金
           03 COMB-TNK.
      *  低年金−外来−負担区分
             05 COMB-TNK-GAIFTNKBN        PIC  X(01).
      *  低年金−外来−内容
             05 COMB-TNK-GAI-NAIYO.
      *    低年金−外来−回−負担割合
               07 COMB-TNK-GAIKFTNRATE      PIC  9(03).
      *    低年金−外来−回−固定額
               07 COMB-TNK-GAIKKOTGAK       PIC  9(06).
      *    低年金−外来−回−上限額
               07 COMB-TNK-GAIKJGNGAK       PIC  9(06).
      *    低年金−外来−日−上限額
               07 COMB-TNK-GAIDJGNGAK       PIC  9(06).
      *    低年金−外来−日−上限回数
               07 COMB-TNK-GAIDJGNCNT       PIC  9(02).
      *    低年金−外来−月−院内−上限額
               07 COMB-TNK-GAIMNAIJGNGAK    PIC  9(06).
      *    低年金−外来−月−院外−上限額
               07 COMB-TNK-GAIMGAIJGNGAK    PIC  9(06).
      *    低年金−外来−月−上限回数
               07 COMB-TNK-GAIMJGNCNT       PIC  9(02).
      *  低年金−外来−薬剤負担区分
             05 COMB-TNK-GAIYKZFTNKBN     PIC  X(01).
      *  低年金−入院−負担区分
             05 COMB-TNK-NYUFTNKBN        PIC  X(01).
      *  低年金−入院−内容
             05 COMB-TNK-NYU-NAIYO.
      *    低年金−入院−回−負担割合
               07 COMB-TNK-NYUKFTNRATE        PIC  9(03).
      *    低年金−入院−回−固定額
               07 COMB-TNK-NYUKKOTGAK         PIC  9(06).
      *    低年金−入院−回−上限額
               07 COMB-TNK-NYUKJGNGAK         PIC  9(06).
      *    低年金−入院−日−上限額
               07 COMB-TNK-NYUDJGNGAK         PIC  9(06).
      *    低年金−入院−日−上限回数
               07 COMB-TNK-NYUDJGNCNT         PIC  9(02).
      *    低年金−入院−月−上限額
               07 COMB-TNK-NYUMJGNGAK         PIC  9(06).
      *    低年金−入院−月−上限回数
               07 COMB-TNK-NYUMJGNCNT         PIC  9(02).
      *  低年金−入院−食事療養区分
             05 COMB-TNK-NYUSKJRYOKBN     PIC  X(01).
      *点数単価
           03 COMB-TENTNK                 PIC  9(04).
      *消費税区分
           03 COMB-TAXKBN                 PIC  X(01).
      *変更区分
           03 COMB-CHGKBN                 PIC  X(01).
      *削除区分
           03 COMB-DELKBN                 PIC  X(01).
      *表示区分
           03 COMB-HYOJIKBN               PIC  X(01).
      *端末ＩＤ
           03 COMB-TERMID                 PIC  X(16).
      *オペーレータＩＤ
           03 COMB-OPID                   PIC  X(16).
      *作成年月日
           03 COMB-CREYMD                 PIC  X(08).
      *更新年月日
           03 COMB-UPYMD                  PIC  X(08).
      *更新時間
           03 COMB-UPHMS                  PIC  X(06).
