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
      *    保険番号          　 COPY (CPHKNNUM.INC)              *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  HKNNUM-REC.
      *キー
           03 HKN-KEY.
      *医療機関ＩＤ
               05 HKN-HOSPNUM            PIC 9(2).
      *保険番号
               05 HKN-HKNNUM            PIC  X(03).
      *適用開始年月日
               05 HKN-TEKSTYMD          PIC  X(08).
      *支払区分
               05 HKN-PAYKBN            PIC  X(02).
      *適用終了年月日
           03 HKN-TEKEDYMD              PIC  X(08).
      *法別番号
           03 HKN-HBTNUM                PIC  X(02).
      *制度名
           03 HKN-SEIDONAME             PIC  X(100).
      *短縮制度名
           03 HKN-TANSEIDONAME          PIC  X(20).
      *保険公費種別区分
           03 HKN-HKNKOHSBTKBN          PIC  X(01).
      *法別番号チェック区分
           03 HKN-HBTNUMCHKKBN          PIC  X(01).
      *検証番号チェック区分
           03 HKN-KENSNUMCHKKBN         PIC  X(01).
      *受給者検証番号チェック区分
           03 HKN-JKYSKENSNUMCHKKBN     PIC  X(01).
      *公費主保区分
           03 HKN-HKNKOHKBN             PIC  X(01).
      *限定保険番号
           03 HKN-GENHKNNUM-G.
      *限定保険番号１
               05 HKN-GENHKNNUM1        PIC  X(03).
      *限定保険番号２
               05 HKN-GENHKNNUM2        PIC  X(03).
      *限定保険番号３
               05 HKN-GENHKNNUM3        PIC  X(03).
      *
           03 HKN-GENHKNNUM-R           REDEFINES   HKN-GENHKNNUM-G.
               05 HKN-GENHKNNUM         PIC  X(03)  OCCURS  3.
      *条件−年齢開始
           03 HKN-JOKEN-STYEAR          PIC  9(03).
      *条件−年齢終了
           03 HKN-JOKEN-EDYEAR          PIC  9(03).
      *点数単価
           03 HKN-TENTNK                PIC  9(03).
      *レセプト請求区分
           03 HKN-RECESKYKBN            PIC  X(01).
      *レセプト１円単位表示区分
           03 HKN-RECEFTNDSPKBN         PIC  X(01).
      *レセプト患者負担額判定表示区分
           03 HKN-RECEJGNDSPKBN         PIC  X(01).
      *
           03 HKN-NAIYOU.
      *本人
              05 HKN-HON.
      *  本人−外来−負担区分
                07 HKN-HON-GAIFTNKBN         PIC  X(01).
      *  本人−外来−内容
                07 HKN-HON-GAI-NAIYO.
      *    本人−外来−回−負担割合
                  09 HKN-HON-GAIKFTNRATE         PIC  9(03).
      *    本人−外来−回−固定額
                  09 HKN-HON-GAIKKOTGAK          PIC  9(06).
      *    本人−外来−回−上限額
                  09 HKN-HON-GAIKJGNGAK          PIC  9(06).
      *    本人−外来−日−上限額
                  09 HKN-HON-GAIDJGNGAK          PIC  9(06).
      *    本人−外来−日−上限回数
                  09 HKN-HON-GAIDJGNCNT          PIC  9(02).
      *    本人−外来−月−院内−上限額
                  09 HKN-HON-GAIMNAIJGNGAK       PIC  9(06).
      *    本人−外来−月−院外−上限額
                  09 HKN-HON-GAIMGAIJGNGAK       PIC  9(06).
      *    本人−外来−月−上限回数
                  09 HKN-HON-GAIMJGNCNT          PIC  9(02).
      *    本人−外来−薬剤負担区分
                  09 HKN-HON-GAIYKZFTNKBN        PIC  X(01).
      *  本人−入院−負担区分
                07 HKN-HON-NYUFTNKGN             PIC  X(01).
      *  本人−入院−内容
                07 HKN-HON-NYU-NAIYO.
      *    本人−入院−回−負担割合
                  09 HKN-HON-NYUKFTNRATE         PIC  9(03).
      *    本人−入院−回−固定額
                  09 HKN-HON-NYUKKOTGAK          PIC  9(06).
      *    本人−入院−回−上限額
                  09 HKN-HON-NYUKJGNGAK          PIC  9(06).
      *    本人−入院−日−上限額
                  09 HKN-HON-NYUDJGNGAK          PIC  9(06).
      *    本人−入院−日−上限回数
                  09 HKN-HON-NYUDGJNCNT          PIC  9(02).
      *    本人−入院−月−上限額
                  09 HKN-HON-NYUMJGNGAK          PIC  9(06).
      *    本人−入院−月−上限回数
                  09 HKN-HON-NYUMJGNCNT          PIC  9(02).
      *    本人−入院−食事療養区分
                  09 HKN-HON-NYUSKJRYYKBN        PIC  X(01).
      *    本人−入院−食事負担助成額（日）
                  09 HKN-HON-NYUSKJJYOGAK        PIC  9(06).
      *家族
              05 HKN-KZK.
      *  家族−外来−負担区分
                07 HKN-KZK-GAIFTNKBN           PIC  X(01).
      *  家族−外来−内容
                07 HKN-KZK-GAI-NAIYO.
      *    家族−外来−回−負担割合
                  09 HKN-KZK-GAIKFTNRATE         PIC  9(03).
      *    家族−外来−回−固定額
                  09 HKN-KZK-GAIKKOTGAK          PIC  9(06).
      *    家族−外来−回−上限額
                  09 HKN-KZK-GAIKJGNGAK          PIC  9(06).
      *    家族−外来−日−上限額
                  09 HKN-KZK-GAIDJGNGAK          PIC  9(06).
      *    家族−外来−日−上限回数
                  09 HKN-KZK-GAIDJGNCNT          PIC  9(02).
      *    家族−外来−月−院内−上限額
                  09 HKN-KZK-GAIMNAIJGNGAK       PIC  9(06).
      *    家族−外来−月−院外−上限額
                  09 HKN-KZK-GAIMGAIJGNGAK       PIC  9(06).
      *    家族−外来−月−上限回数
                  09 HKN-KZK-GAIMJGNCNT          PIC  9(02).
      *    家族−外来−薬剤負担区分
                  09 HKN-KZK-GAIYKZFTNKBN        PIC  X(01).
      *  家族−入院−負担区分
                07 HKN-KZK-NYUFTNKBN             PIC  X(01).
      *  家族−入院−内容
                07 HKN-KZK-NYU-NAIYO.
      *    家族−入院−回−負担割合
                  09 HKN-KZK-NYUKFTNRATE         PIC  9(03).
      *    家族−入院−回−固定額
                  09 HKN-KZK-NYUKKOTGAK          PIC  9(06).
      *    家族−入院−回−上限額
                  09 HKN-KZK-NYUKJGNGAK          PIC  9(06).
      *    家族−入院−日−上限額
                  09 HKN-KZK-NYUDJGNGAK          PIC  9(06).
      *    家族−入院−日−上限回数
                  09 HKN-KZK-NYUDJGNCNT          PIC  9(02).
      *    家族−入院−月−上限額
                  09 HKN-KZK-NYUMJGNGAK          PIC  9(06).
      *    家族−入院−月−上限回数
                  09 HKN-KZK-NYUMJGNCNT          PIC  9(02).
      *    家族−入院−食事療養区分
                  09 HKN-KZK-NYUSKJRYYKBN        PIC  X(01).
      *    家族−入院−食事負担助成額（日）
                  09 HKN-KZK-NYUSKJJYOGAK        PIC  9(06).
      *低所得
              05 HKN-TSY.
      *  低所得−外来−負担区分
                07 HKN-TSY-GAIFTNKBN         PIC  X(01).
      *  低所得−外来−内容
                07 HKN-TSY-GAI-NAIYO.
      *    低所得−外来−回−負担割合
                  09 HKN-TSY-GAIKFTNRATE      PIC  9(03).
      *    低所得−外来−回−固定額
                  09 HKN-TSY-GAIKKOTGAK       PIC  9(06).
      *    低所得−外来−回−上限額
                  09 HKN-TSY-GAIKJGNGAK       PIC  9(06).
      *    低所得−外来−日−上限額
                  09 HKN-TSY-GAIDJGNGAK       PIC  9(06).
      *    低所得−外来−日−上限回数
                  09 HKN-TSY-GAIDJGNCNT       PIC  9(02).
      *    低所得−外来−月−院内−上限額
                  09 HKN-TSY-GAIMNAIJGNGAK    PIC  9(06).
      *    低所得−外来−月−院外−上限額
                  09 HKN-TSY-GAIMGAIJGNGAK    PIC  9(06).
      *    低所得−外来−月−上限回数
                  09 HKN-TSY-GAIMJGNCNT       PIC  9(02).
      *    低所得−外来−薬剤負担区分
                  09 HKN-TSY-GAIYKZFTNKBN     PIC  X(01).
      *  低所得−入院−負担区分
                07 HKN-TSY-NYUFTNNBK          PIC  X(01).
      *  低所得−入院−内容
                07 HKN-TSY-NYU-NAIYO.
      *    低所得−入院−回−負担割合
                  09 HKN-TSY-NYUKFTNRATE      PIC  9(03).
      *    低所得−入院−回−固定額
                  09 HKN-TSY-NYUKKOTGAK       PIC  9(06).
      *    低所得−入院−回−上限額
                  09 HKN-TSY-NYUKJGNGAK       PIC  9(06).
      *    低所得−入院−日−上限額
                  09 HKN-TSY-NYUDJGNGAK       PIC  9(06).
      *    低所得−入院−日−上限回数
                  09 HKN-TSY-NYUDJGNCNT       PIC  9(02).
      *    低所得−入院−月−上限額
                  09 HKN-TSY-NYUMJGNGAK       PIC  9(06).
      *    低所得−入院−月−上限回数
                  09 HKN-TSY-NYUMJGNCNT       PIC  9(02).
      *    低所得−入院−食事療養区分
                  09 HKN-TSY-NYUSKJRYYKBN     PIC  X(01).
      *    低所得−入院−食事負担助成額（日）
                  09 HKN-TSY-NYUSKJJYOGAK     PIC  9(06).
      *低年金
              05 HKN-TNK.
      *  低年金−外来−負担区分
                07 HKN-TNK-GAIFTNKBN         PIC  X(01).
      *  低年金−外院−内容
                07 HKN-TNK-GAI-NAIYO.
      *    低年金−外来−回−負担割合
                  09 HKN-TNK-GAIKFTNRATE      PIC  9(03).
      *    低年金−外来−回−固定額
                  09 HKN-TNK-GAIKKOTGAK       PIC  9(06).
      *    低年金−外来−回−上限額
                  09 HKN-TNK-GAIKJGNGAK       PIC  9(06).
      *    低年金−外来−日−上限額
                  09 HKN-TNK-GAIDJGNGAK       PIC  9(06).
      *    低年金−外来−日−上限回数
                  09 HKN-TNK-GAIDJGNCNT       PIC  9(02).
      *    低年金−外来−月−院内−上限額
                  09 HKN-TNK-GAIMNAIJGNGAK    PIC  9(06).
      *    低年金−外来−月−院外−上限額
                  09 HKN-TNK-GAIMGAIJGNGAK    PIC  9(06).
      *    低年金−外来−月−上限回数
                  09 HKN-TNK-GAIMJGNCNT       PIC  9(02).
      *    低年金−外来−薬剤負担区分
                  09 HKN-TNK-GAIYKZFTNKBN     PIC  X(01).
      *  低年金−入院−負担区分
                07 HKN-TNK-NYUFTNKBN        PIC  X(01).
      *  低年金−入院−内容
                07 HKN-TNK-NYU-NAIYO.
      *    低年金−入院−回−負担割合
                  09 HKN-TNK-NYUKFTNRATE      PIC  9(03).
      *    低年金−入院−回−固定額
                  09 HKN-TNK-NYUKKOTGAK       PIC  9(06).
      *    低年金−入院−回−上限額
                  09 HKN-TNK-NYUKJGNGAK       PIC  9(06).
      *    低年金−入院−日−上限額
                  09 HKN-TNK-NYUDJGNGAK       PIC  9(06).
      *    低年金−入院−日−上限回数
                  09 HKN-TNK-NYUDJGNCNT       PIC  9(02).
      *    低年金−入院−月−上限額
                  09 HKN-TNK-NYUMJGNGAK       PIC  9(06).
      *    低年金−入院−月−上限回数
                  09 HKN-TNK-NYUMJGNCNT       PIC  9(02).
      *    低年金−入院−食事療養区分
                  09 HKN-TNK-NYUSKJRYYKBN     PIC  X(01).
      *    低年金−入院−食事負担助成額（日）
                  09 HKN-TNK-NYUSKJJYOGAK     PIC  9(06).
      *
           03 HKN-NAIYOU-R                REDEFINES   HKN-NAIYOU.
              05 HKN-TBL                  OCCURS  4.
      *  外来−負担区分
                07 HKN-GAIFTNKBN          PIC  X(01).
      *  外来−内容
                07 HKN-GAI-NAIYO.
      *    外来−回−負担割合
                  09 HKN-GAIKFTNRATE         PIC  9(03).
      *    外来−回−固定額
                  09 HKN-GAIKKOTGAK          PIC  9(06).
      *    外来−回−上限額
                  09 HKN-GAIKJGNGAK          PIC  9(06).
      *    外来−日−上限額
                  09 HKN-GAIDJGNGAK          PIC  9(06).
      *    外来−日−上限回数
                  09 HKN-GAIDJGNCNT          PIC  9(02).
      *    外来−月−院内−上限額
                  09 HKN-GAIMNAIJGNGAK       PIC  9(06).
      *    外来−月−院外−上限額
                  09 HKN-GAIMGAIJGNGAK       PIC  9(06).
      *    外来−月−上限回数
                  09 HKN-GAIMJGNCNT          PIC  9(02).
      *    外来−薬剤負担区分
                  09 HKN-GAIYKZFTNKBN        PIC  X(01).
      *  入院−負担区分
                07 HKN-NYUFTNKBN             PIC  X(01).
      *  入院−内容
                07 HKN-NYU-NAIYO.
      *    入院−回−負担割合
                  09 HKN-NYUKFTNRATE         PIC  9(03).
      *    入院−回−固定額
                  09 HKN-NYUKKOTGAK          PIC  9(06).
      *    入院−回−上限額
                  09 HKN-NYUKJGNGAK          PIC  9(06).
      *    入院−日−上限額
                  09 HKN-NYUDJGNGAK          PIC  9(06).
      *    入院−日−上限回数
                  09 HKN-NYUDJGNCNT          PIC  9(02).
      *    入院−月−上限額
                  09 HKN-NYUMJGNGAK          PIC  9(06).
      *    入院−月−上限回数
                  09 HKN-NYUMJGNCNT          PIC  9(02).
      *    入院−食事療養区分
                  09 HKN-NYUSKJRYYKBN        PIC  X(01).
      *    入院−食事負担助成額（日）
                  09 HKN-NYUSKJJYOGAK        PIC  9(06).
      *　　端末ＩＤ
           03 HKN-TERMID                  PIC  X(16).
      *　　オペーレータＩＤ
           03 HKN-OPID                    PIC  X(16).
      *　　作成年月日
           03 HKN-CREYMD                  PIC  X(08).
      *　　更新年月日
           03 HKN-UPYMD                   PIC  X(08).
      *　　更新時間
           03 HKN-UPHMS                   PIC  X(06).
