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
      * redistribute this program, but only under the conditions desc
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ************************************************************
      *    保険番号マスタ更新データ  (CPHKNNUMWKV02.INC)         *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  HKNWKV02-REC.
      *処理区分
          03  HKNWKV02-SYORIKBN             PIC X(01).
      *キー
          03  HKNWKV02-KEY.
      *医療機関ＩＤ
               05 HKNWKV02-HOSPID            PIC  X(24).
      *保険番号
               05 HKNWKV02-HKNNUM            PIC  X(03).
      *適用開始年月日
               05 HKNWKV02-TEKSTYMD          PIC  X(08).
      *支払区分
               05 HKNWKV02-PAYKBN            PIC  X(02).
      *適用終了年月日
           03 HKNWKV02-TEKEDYMD              PIC  X(08).
      *適用終了年月日２
           03 HKNWKV02-TEKEDYMD2             PIC  X(08).
      *法別番号
           03 HKNWKV02-HBTNUM                PIC  X(02).
      *制度名
           03 HKNWKV02-SEIDONAME             PIC  X(100).
      *短縮制度名
           03 HKNWKV02-TANSEIDONAME          PIC  X(20).
      *保険公費種別区分
           03 HKNWKV02-HKNKOHSBTKBN          PIC  X(01).
      *法別番号チェック区分
           03 HKNWKV02-HBTNUMCHKKBN          PIC  X(01).
      *検証番号チェック区分
           03 HKNWKV02-KENSNUMCHKKBN         PIC  X(01).
      *受給者検証番号チェック区分
           03 HKNWKV02-JKYSKENSNUMCHKKBN     PIC  X(01).
      *公費主保区分
           03 HKNWKV02-HKNKOHKBN             PIC  X(01).
      *限定保険番号
           03 HKNWKV02-GENHKNNUM-G.
      *限定保険番号１
               05 HKNWKV02-GENHKNNUM1        PIC  X(03).
      *限定保険番号２
               05 HKNWKV02-GENHKNNUM2        PIC  X(03).
      *限定保険番号３
               05 HKNWKV02-GENHKNNUM3        PIC  X(03).
      *
           03 HKNWKV02-GENHKNNUM-R REDEFINES   HKNWKV02-GENHKNNUM-G.
               05 HKNWKV02-GENHKNNUM         PIC  X(03)  OCCURS  3.
      *条件−年齢開始
           03 HKNWKV02-JOKEN-STYEAR          PIC  9(03).
      *条件−年齢終了
           03 HKNWKV02-JOKEN-EDYEAR          PIC  9(03).
      *点数単価
           03 HKNWKV02-TENTNK                PIC  9(03).
      *レセプト請求区分
           03 HKNWKV02-RECESKYKBN            PIC  X(01).
      *レセプト１円単位表示区分
           03 HKNWKV02-RECEFTNDSPKBN         PIC  X(01).
      *
           03 HKNWKV02-NAIYOU.
      *本人
              05 HKNWKV02-HON.
      *  本人−外来−負担区分
                07 HKNWKV02-HON-GAIFTNKBN         PIC  X(01).
      *  本人−外来−内容
                07 HKNWKV02-HON-GAI-NAIYO.
      *    本人−外来−回−負担割合
                  09 HKNWKV02-HON-GAIKFTNRATE         PIC  9(03).
      *    本人−外来−回−固定額
                  09 HKNWKV02-HON-GAIKKOTGAK          PIC  9(06).
      *    本人−外来−回−上限額
                  09 HKNWKV02-HON-GAIKJGNGAK          PIC  9(06).
      *    本人−外来−日−上限額
                  09 HKNWKV02-HON-GAIDJGNGAK          PIC  9(06).
      *    本人−外来−日−上限回数
                  09 HKNWKV02-HON-GAIDJGNCNT          PIC  9(02).
      *    本人−外来−月−院内−上限額
                  09 HKNWKV02-HON-GAIMNAIJGNGAK       PIC  9(06).
      *    本人−外来−月−院外−上限額
                  09 HKNWKV02-HON-GAIMGAIJGNGAK       PIC  9(06).
      *    本人−外来−月−上限回数
                  09 HKNWKV02-HON-GAIMJGNCNT          PIC  9(02).
      *    本人−外来−薬剤負担区分
                  09 HKNWKV02-HON-GAIYKZFTNKBN        PIC  X(01).
      *  本人−入院−負担区分
                07 HKNWKV02-HON-NYUFTNKGN             PIC  X(01).
      *  本人−入院−内容
                07 HKNWKV02-HON-NYU-NAIYO.
      *    本人−入院−回−負担割合
                  09 HKNWKV02-HON-NYUKFTNRATE         PIC  9(03).
      *    本人−入院−回−固定額
                  09 HKNWKV02-HON-NYUKKOTGAK          PIC  9(06).
      *    本人−入院−回−上限額
                  09 HKNWKV02-HON-NYUKJGNGAK          PIC  9(06).
      *    本人−入院−日−上限額
                  09 HKNWKV02-HON-NYUDJGNGAK          PIC  9(06).
      *    本人−入院−日−上限回数
                  09 HKNWKV02-HON-NYUDGJNCNT          PIC  9(02).
      *    本人−入院−月−上限額
                  09 HKNWKV02-HON-NYUMJGNGAK          PIC  9(06).
      *    本人−入院−月−上限回数
                  09 HKNWKV02-HON-NYUMJGNCNT          PIC  9(02).
      *    本人−入院−食事療養区分
                  09 HKNWKV02-HON-NYUSKJRYYKBN        PIC  X(01).
      *家族
              05 HKNWKV02-KZK.
      *  家族−外来−負担区分
                07 HKNWKV02-KZK-GAIFTNKBN           PIC  X(01).
      *  家族−外来−内容
                07 HKNWKV02-KZK-GAI-NAIYO.
      *    家族−外来−回−負担割合
                  09 HKNWKV02-KZK-GAIKFTNRATE         PIC  9(03).
      *    家族−外来−回−固定額
                  09 HKNWKV02-KZK-GAIKKOTGAK          PIC  9(06).
      *    家族−外来−回−上限額
                  09 HKNWKV02-KZK-GAIKJGNGAK          PIC  9(06).
      *    家族−外来−日−上限額
                  09 HKNWKV02-KZK-GAIDJGNGAK          PIC  9(06).
      *    家族−外来−日−上限回数
                  09 HKNWKV02-KZK-GAIDJGNCNT          PIC  9(02).
      *    家族−外来−月−院内−上限額
                  09 HKNWKV02-KZK-GAIMNAIJGNGAK       PIC  9(06).
      *    家族−外来−月−院外−上限額
                  09 HKNWKV02-KZK-GAIMGAIJGNGAK       PIC  9(06).
      *    家族−外来−月−上限回数
                  09 HKNWKV02-KZK-GAIMJGNCNT          PIC  9(02).
      *    家族−外来−薬剤負担区分
                  09 HKNWKV02-KZK-GAIYKZFTNKBN        PIC  X(01).
      *  家族−入院−負担区分
                07 HKNWKV02-KZK-NYUFTNKBN             PIC  X(01).
      *  家族−入院−内容
                07 HKNWKV02-KZK-NYU-NAIYO.
      *    家族−入院−回−負担割合
                  09 HKNWKV02-KZK-NYUKFTNRATE         PIC  9(03).
      *    家族−入院−回−固定額
                  09 HKNWKV02-KZK-NYUKKOTGAK          PIC  9(06).
      *    家族−入院−回−上限額
                  09 HKNWKV02-KZK-NYUKJGNGAK          PIC  9(06).
      *    家族−入院−日−上限額
                  09 HKNWKV02-KZK-NYUDJGNGAK          PIC  9(06).
      *    家族−入院−日−上限回数
                  09 HKNWKV02-KZK-NYUDJGNCNT          PIC  9(02).
      *    家族−入院−月−上限額
                  09 HKNWKV02-KZK-NYUMJGNGAK          PIC  9(06).
      *    家族−入院−月−上限回数
                  09 HKNWKV02-KZK-NYUMJGNCNT          PIC  9(02).
      *    家族−入院−食事療養区分
                  09 HKNWKV02-KZK-NYUSKJRYYKBN        PIC  X(01).
      *低所得
              05 HKNWKV02-TSY.
      *  低所得−外来−負担区分
                07 HKNWKV02-TSY-GAIFTNKBN         PIC  X(01).
      *  低所得−外来−内容
                07 HKNWKV02-TSY-GAI-NAIYO.
      *    低所得−外来−回−負担割合
                  09 HKNWKV02-TSY-GAIKFTNRATE      PIC  9(03).
      *    低所得−外来−回−固定額
                  09 HKNWKV02-TSY-GAIKKOTGAK       PIC  9(06).
      *    低所得−外来−回−上限額
                  09 HKNWKV02-TSY-GAIKJGNGAK       PIC  9(06).
      *    低所得−外来−日−上限額
                  09 HKNWKV02-TSY-GAIDJGNGAK       PIC  9(06).
      *    低所得−外来−日−上限回数
                  09 HKNWKV02-TSY-GAIDJGNCNT       PIC  9(02).
      *    低所得−外来−月−院内−上限額
                  09 HKNWKV02-TSY-GAIMNAIJGNGAK    PIC  9(06).
      *    低所得−外来−月−院外−上限額
                  09 HKNWKV02-TSY-GAIMGAIJGNGAK    PIC  9(06).
      *    低所得−外来−月−上限回数
                  09 HKNWKV02-TSY-GAIMJGNCNT       PIC  9(02).
      *    低所得−外来−薬剤負担区分
                  09 HKNWKV02-TSY-GAIYKZFTNKBN     PIC  X(01).
      *  低所得−入院−負担区分
                07 HKNWKV02-TSY-NYUFTNNBK          PIC  X(01).
      *  低所得−入院−内容
                07 HKNWKV02-TSY-NYU-NAIYO.
      *    低所得−入院−回−負担割合
                  09 HKNWKV02-TSY-NYUKFTNRATE      PIC  9(03).
      *    低所得−入院−回−固定額
                  09 HKNWKV02-TSY-NYUKKOTGAK       PIC  9(06).
      *    低所得−入院−回−上限額
                  09 HKNWKV02-TSY-NYUKJGNGAK       PIC  9(06).
      *    低所得−入院−日−上限額
                  09 HKNWKV02-TSY-NYUDJGNGAK       PIC  9(06).
      *    低所得−入院−日−上限回数
                  09 HKNWKV02-TSY-NYUDJGNCNT       PIC  9(02).
      *    低所得−入院−月−上限額
                  09 HKNWKV02-TSY-NYUMJGNGAK       PIC  9(06).
      *    低所得−入院−月−上限回数
                  09 HKNWKV02-TSY-NYUMJGNCNT       PIC  9(02).
      *    低所得−入院−食事療養区分
                  09 HKNWKV02-TSY-NYUSKJRYYKBN     PIC  X(01).
      *低年金
              05 HKNWKV02-TNK.
      *  低年金−外来−負担区分
                07 HKNWKV02-TNK-GAIFTNKBN         PIC  X(01).
      *  低年金−外院−内容
                07 HKNWKV02-TNK-GAI-NAIYO.
      *    低年金−外来−回−負担割合
                  09 HKNWKV02-TNK-GAIKFTNRATE      PIC  9(03).
      *    低年金−外来−回−固定額
                  09 HKNWKV02-TNK-GAIKKOTGAK       PIC  9(06).
      *    低年金−外来−回−上限額
                  09 HKNWKV02-TNK-GAIKJGNGAK       PIC  9(06).
      *    低年金−外来−日−上限額
                  09 HKNWKV02-TNK-GAIDJGNGAK       PIC  9(06).
      *    低年金−外来−日−上限回数
                  09 HKNWKV02-TNK-GAIDJGNCNT       PIC  9(02).
      *    低年金−外来−月−院内−上限額
                  09 HKNWKV02-TNK-GAIMNAIJGNGAK    PIC  9(06).
      *    低年金−外来−月−院外−上限額
                  09 HKNWKV02-TNK-GAIMGAIJGNGAK    PIC  9(06).
      *    低年金−外来−月−上限回数
                  09 HKNWKV02-TNK-GAIMJGNCNT       PIC  9(02).
      *    低年金−外来−薬剤負担区分
                  09 HKNWKV02-TNK-GAIYKZFTNKBN     PIC  X(01).
      *  低年金−入院−負担区分
                07 HKNWKV02-TNK-NYUFTNKBN        PIC  X(01).
      *  低年金−入院−内容
                07 HKNWKV02-TNK-NYU-NAIYO.
      *    低年金−入院−回−負担割合
                  09 HKNWKV02-TNK-NYUKFTNRATE      PIC  9(03).
      *    低年金−入院−回−固定額
                  09 HKNWKV02-TNK-NYUKKOTGAK       PIC  9(06).
      *    低年金−入院−回−上限額
                  09 HKNWKV02-TNK-NYUKJGNGAK       PIC  9(06).
      *    低年金−入院−日−上限額
                  09 HKNWKV02-TNK-NYUDJGNGAK       PIC  9(06).
      *    低年金−入院−日−上限回数
                  09 HKNWKV02-TNK-NYUDJGNCNT       PIC  9(02).
      *    低年金−入院−月−上限額
                  09 HKNWKV02-TNK-NYUMJGNGAK       PIC  9(06).
      *    低年金−入院−月−上限回数
                  09 HKNWKV02-TNK-NYUMJGNCNT       PIC  9(02).
      *    低年金−入院−食事療養区分
                  09 HKNWKV02-TNK-NYUSKJRYYKBN     PIC  X(01).
      *
           03 HKNWKV02-NAIYOU-R REDEFINES   HKNWKV02-NAIYOU.
              05 HKNWKV02-TBL                  OCCURS  4.
      *  外来−負担区分
                07 HKNWKV02-GAIFTNKBN          PIC  X(01).
      *  外来−内容
                07 HKNWKV02-GAI-NAIYO.
      *    外来−回−負担割合
                  09 HKNWKV02-GAIKFTNRATE         PIC  9(03).
      *    外来−回−固定額
                  09 HKNWKV02-GAIKKOTGAK          PIC  9(06).
      *    外来−回−上限額
                  09 HKNWKV02-GAIKJGNGAK          PIC  9(06).
      *    外来−日−上限額
                  09 HKNWKV02-GAIDJGNGAK          PIC  9(06).
      *    外来−日−上限回数
                  09 HKNWKV02-GAIDJGNCNT          PIC  9(02).
      *    外来−月−院内−上限額
                  09 HKNWKV02-GAIMNAIJGNGAK       PIC  9(06).
      *    外来−月−院外−上限額
                  09 HKNWKV02-GAIMGAIJGNGAK       PIC  9(06).
      *    外来−月−上限回数
                  09 HKNWKV02-GAIMJGNCNT          PIC  9(02).
      *    外来−薬剤負担区分
                  09 HKNWKV02-GAIYKZFTNKBN        PIC  X(01).
      *  入院−負担区分
                07 HKNWKV02-NYUFTNKBN             PIC  X(01).
      *  入院−内容
                07 HKNWKV02-NYU-NAIYO.
      *    入院−回−負担割合
                  09 HKNWKV02-NYUKFTNRATE         PIC  9(03).
      *    入院−回−固定額
                  09 HKNWKV02-NYUKKOTGAK          PIC  9(06).
      *    入院−回−上限額
                  09 HKNWKV02-NYUKJGNGAK          PIC  9(06).
      *    入院−日−上限額
                  09 HKNWKV02-NYUDJGNGAK          PIC  9(06).
      *    入院−日−上限回数
                  09 HKNWKV02-NYUDJGNCNT          PIC  9(02).
      *    入院−月−上限額
                  09 HKNWKV02-NYUMJGNGAK          PIC  9(06).
      *    入院−月−上限回数
                  09 HKNWKV02-NYUMJGNCNT          PIC  9(02).
      *    入院−食事療養区分
                  09 HKNWKV02-NYUSKJRYYKBN        PIC  X(01).
      *　　端末ＩＤ
           03 HKNWKV02-TERMID                  PIC  X(16).
      *　　オペーレータＩＤ
           03 HKNWKV02-OPID                    PIC  X(16).
      *　　作成年月日
           03 HKNWKV02-CREYMD                  PIC  X(08).
      *　　更新年月日
           03 HKNWKV02-UPYMD                   PIC  X(08).
      *　　更新時間
           03 HKNWKV02-UPHMS                   PIC  X(06).
