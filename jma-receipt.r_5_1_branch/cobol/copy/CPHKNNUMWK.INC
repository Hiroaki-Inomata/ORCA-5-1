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
      *    保険番号マスタ更新データ  (CPHKNNUMWK.INC)            *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  HKNWK-REC.
      *処理区分
          03  HKNWK-SYORIKBN             PIC X(01).
      *キー
          03  HKNWK-KEY.
      *医療機関ＩＤ
               05 HKNWK-HOSPID            PIC  X(24).
      *保険番号
               05 HKNWK-HKNNUM            PIC  X(03).
      *適用開始年月日
               05 HKNWK-TEKSTYMD          PIC  X(08).
      *支払区分
               05 HKNWK-PAYKBN            PIC  X(02).
      *適用終了年月日
           03 HKNWK-TEKEDYMD              PIC  X(08).
      *適用終了年月日２
           03 HKNWK-TEKEDYMD2             PIC  X(08).
      *法別番号
           03 HKNWK-HBTNUM                PIC  X(02).
      *制度名
           03 HKNWK-SEIDONAME             PIC  X(100).
      *短縮制度名
           03 HKNWK-TANSEIDONAME          PIC  X(20).
      *保険公費種別区分
           03 HKNWK-HKNKOHSBTKBN          PIC  X(01).
      *法別番号チェック区分
           03 HKNWK-HBTNUMCHKKBN          PIC  X(01).
      *検証番号チェック区分
           03 HKNWK-KENSNUMCHKKBN         PIC  X(01).
      *受給者検証番号チェック区分
           03 HKNWK-JKYSKENSNUMCHKKBN     PIC  X(01).
      *公費主保区分
           03 HKNWK-HKNKOHKBN             PIC  X(01).
      *限定保険番号
           03 HKNWK-GENHKNNUM-G.
      *限定保険番号１
               05 HKNWK-GENHKNNUM1        PIC  X(03).
      *限定保険番号２
               05 HKNWK-GENHKNNUM2        PIC  X(03).
      *限定保険番号３
               05 HKNWK-GENHKNNUM3        PIC  X(03).
      *
           03 HKNWK-GENHKNNUM-R       REDEFINES   HKNWK-GENHKNNUM-G.
               05 HKNWK-GENHKNNUM         PIC  X(03)  OCCURS  3.
      *条件−年齢開始
           03 HKNWK-JOKEN-STYEAR          PIC  9(03).
      *条件−年齢終了
           03 HKNWK-JOKEN-EDYEAR          PIC  9(03).
      *点数単価
           03 HKNWK-TENTNK                PIC  9(03).
      *レセプト請求区分
           03 HKNWK-RECESKYKBN            PIC  X(01).
      *レセプト１円単位表示区分
           03 HKNWK-RECEFTNDSPKBN         PIC  X(01).
      *レセプト患者負担額判定表示区分
           03 HKNWK-RECEJGNDSPKBN         PIC  X(01).
      *
           03 HKNWK-NAIYOU.
      *本人
              05 HKNWK-HON.
      *  本人−外来−負担区分
                07 HKNWK-HON-GAIFTNKBN         PIC  X(01).
      *  本人−外来−内容
                07 HKNWK-HON-GAI-NAIYO.
      *    本人−外来−回−負担割合
                  09 HKNWK-HON-GAIKFTNRATE         PIC  9(03).
      *    本人−外来−回−固定額
                  09 HKNWK-HON-GAIKKOTGAK          PIC  9(06).
      *    本人−外来−回−上限額
                  09 HKNWK-HON-GAIKJGNGAK          PIC  9(06).
      *    本人−外来−日−上限額
                  09 HKNWK-HON-GAIDJGNGAK          PIC  9(06).
      *    本人−外来−日−上限回数
                  09 HKNWK-HON-GAIDJGNCNT          PIC  9(02).
      *    本人−外来−月−院内−上限額
                  09 HKNWK-HON-GAIMNAIJGNGAK       PIC  9(06).
      *    本人−外来−月−院外−上限額
                  09 HKNWK-HON-GAIMGAIJGNGAK       PIC  9(06).
      *    本人−外来−月−上限回数
                  09 HKNWK-HON-GAIMJGNCNT          PIC  9(02).
      *    本人−外来−薬剤負担区分
                  09 HKNWK-HON-GAIYKZFTNKBN        PIC  X(01).
      *  本人−入院−負担区分
                07 HKNWK-HON-NYUFTNKGN             PIC  X(01).
      *  本人−入院−内容
                07 HKNWK-HON-NYU-NAIYO.
      *    本人−入院−回−負担割合
                  09 HKNWK-HON-NYUKFTNRATE         PIC  9(03).
      *    本人−入院−回−固定額
                  09 HKNWK-HON-NYUKKOTGAK          PIC  9(06).
      *    本人−入院−回−上限額
                  09 HKNWK-HON-NYUKJGNGAK          PIC  9(06).
      *    本人−入院−日−上限額
                  09 HKNWK-HON-NYUDJGNGAK          PIC  9(06).
      *    本人−入院−日−上限回数
                  09 HKNWK-HON-NYUDGJNCNT          PIC  9(02).
      *    本人−入院−月−上限額
                  09 HKNWK-HON-NYUMJGNGAK          PIC  9(06).
      *    本人−入院−月−上限回数
                  09 HKNWK-HON-NYUMJGNCNT          PIC  9(02).
      *    本人−入院−食事療養区分
                  09 HKNWK-HON-NYUSKJRYYKBN        PIC  X(01).
      *    本人−入院−食事負担助成額（日）
                  09 HKNWK-HON-NYUSKJJYOGAK        PIC  9(06).
      *家族
              05 HKNWK-KZK.
      *  家族−外来−負担区分
                07 HKNWK-KZK-GAIFTNKBN           PIC  X(01).
      *  家族−外来−内容
                07 HKNWK-KZK-GAI-NAIYO.
      *    家族−外来−回−負担割合
                  09 HKNWK-KZK-GAIKFTNRATE         PIC  9(03).
      *    家族−外来−回−固定額
                  09 HKNWK-KZK-GAIKKOTGAK          PIC  9(06).
      *    家族−外来−回−上限額
                  09 HKNWK-KZK-GAIKJGNGAK          PIC  9(06).
      *    家族−外来−日−上限額
                  09 HKNWK-KZK-GAIDJGNGAK          PIC  9(06).
      *    家族−外来−日−上限回数
                  09 HKNWK-KZK-GAIDJGNCNT          PIC  9(02).
      *    家族−外来−月−院内−上限額
                  09 HKNWK-KZK-GAIMNAIJGNGAK       PIC  9(06).
      *    家族−外来−月−院外−上限額
                  09 HKNWK-KZK-GAIMGAIJGNGAK       PIC  9(06).
      *    家族−外来−月−上限回数
                  09 HKNWK-KZK-GAIMJGNCNT          PIC  9(02).
      *    家族−外来−薬剤負担区分
                  09 HKNWK-KZK-GAIYKZFTNKBN        PIC  X(01).
      *  家族−入院−負担区分
                07 HKNWK-KZK-NYUFTNKBN             PIC  X(01).
      *  家族−入院−内容
                07 HKNWK-KZK-NYU-NAIYO.
      *    家族−入院−回−負担割合
                  09 HKNWK-KZK-NYUKFTNRATE         PIC  9(03).
      *    家族−入院−回−固定額
                  09 HKNWK-KZK-NYUKKOTGAK          PIC  9(06).
      *    家族−入院−回−上限額
                  09 HKNWK-KZK-NYUKJGNGAK          PIC  9(06).
      *    家族−入院−日−上限額
                  09 HKNWK-KZK-NYUDJGNGAK          PIC  9(06).
      *    家族−入院−日−上限回数
                  09 HKNWK-KZK-NYUDJGNCNT          PIC  9(02).
      *    家族−入院−月−上限額
                  09 HKNWK-KZK-NYUMJGNGAK          PIC  9(06).
      *    家族−入院−月−上限回数
                  09 HKNWK-KZK-NYUMJGNCNT          PIC  9(02).
      *    家族−入院−食事療養区分
                  09 HKNWK-KZK-NYUSKJRYYKBN        PIC  X(01).
      *    家族−入院−食事負担助成額（日）
                  09 HKNWK-KZK-NYUSKJJYOGAK        PIC  9(06).
      *低所得
              05 HKNWK-TSY.
      *  低所得−外来−負担区分
                07 HKNWK-TSY-GAIFTNKBN         PIC  X(01).
      *  低所得−外来−内容
                07 HKNWK-TSY-GAI-NAIYO.
      *    低所得−外来−回−負担割合
                  09 HKNWK-TSY-GAIKFTNRATE      PIC  9(03).
      *    低所得−外来−回−固定額
                  09 HKNWK-TSY-GAIKKOTGAK       PIC  9(06).
      *    低所得−外来−回−上限額
                  09 HKNWK-TSY-GAIKJGNGAK       PIC  9(06).
      *    低所得−外来−日−上限額
                  09 HKNWK-TSY-GAIDJGNGAK       PIC  9(06).
      *    低所得−外来−日−上限回数
                  09 HKNWK-TSY-GAIDJGNCNT       PIC  9(02).
      *    低所得−外来−月−院内−上限額
                  09 HKNWK-TSY-GAIMNAIJGNGAK    PIC  9(06).
      *    低所得−外来−月−院外−上限額
                  09 HKNWK-TSY-GAIMGAIJGNGAK    PIC  9(06).
      *    低所得−外来−月−上限回数
                  09 HKNWK-TSY-GAIMJGNCNT       PIC  9(02).
      *    低所得−外来−薬剤負担区分
                  09 HKNWK-TSY-GAIYKZFTNKBN     PIC  X(01).
      *  低所得−入院−負担区分
                07 HKNWK-TSY-NYUFTNNBK          PIC  X(01).
      *  低所得−入院−内容
                07 HKNWK-TSY-NYU-NAIYO.
      *    低所得−入院−回−負担割合
                  09 HKNWK-TSY-NYUKFTNRATE      PIC  9(03).
      *    低所得−入院−回−固定額
                  09 HKNWK-TSY-NYUKKOTGAK       PIC  9(06).
      *    低所得−入院−回−上限額
                  09 HKNWK-TSY-NYUKJGNGAK       PIC  9(06).
      *    低所得−入院−日−上限額
                  09 HKNWK-TSY-NYUDJGNGAK       PIC  9(06).
      *    低所得−入院−日−上限回数
                  09 HKNWK-TSY-NYUDJGNCNT       PIC  9(02).
      *    低所得−入院−月−上限額
                  09 HKNWK-TSY-NYUMJGNGAK       PIC  9(06).
      *    低所得−入院−月−上限回数
                  09 HKNWK-TSY-NYUMJGNCNT       PIC  9(02).
      *    低所得−入院−食事療養区分
                  09 HKNWK-TSY-NYUSKJRYYKBN     PIC  X(01).
      *    低所得−入院−食事負担助成額（日）
                  09 HKNWK-TSY-NYUSKJJYOGAK     PIC  9(06).
      *低年金
              05 HKNWK-TNK.
      *  低年金−外来−負担区分
                07 HKNWK-TNK-GAIFTNKBN         PIC  X(01).
      *  低年金−外院−内容
                07 HKNWK-TNK-GAI-NAIYO.
      *    低年金−外来−回−負担割合
                  09 HKNWK-TNK-GAIKFTNRATE      PIC  9(03).
      *    低年金−外来−回−固定額
                  09 HKNWK-TNK-GAIKKOTGAK       PIC  9(06).
      *    低年金−外来−回−上限額
                  09 HKNWK-TNK-GAIKJGNGAK       PIC  9(06).
      *    低年金−外来−日−上限額
                  09 HKNWK-TNK-GAIDJGNGAK       PIC  9(06).
      *    低年金−外来−日−上限回数
                  09 HKNWK-TNK-GAIDJGNCNT       PIC  9(02).
      *    低年金−外来−月−院内−上限額
                  09 HKNWK-TNK-GAIMNAIJGNGAK    PIC  9(06).
      *    低年金−外来−月−院外−上限額
                  09 HKNWK-TNK-GAIMGAIJGNGAK    PIC  9(06).
      *    低年金−外来−月−上限回数
                  09 HKNWK-TNK-GAIMJGNCNT       PIC  9(02).
      *    低年金−外来−薬剤負担区分
                  09 HKNWK-TNK-GAIYKZFTNKBN     PIC  X(01).
      *  低年金−入院−負担区分
                07 HKNWK-TNK-NYUFTNKBN        PIC  X(01).
      *  低年金−入院−内容
                07 HKNWK-TNK-NYU-NAIYO.
      *    低年金−入院−回−負担割合
                  09 HKNWK-TNK-NYUKFTNRATE      PIC  9(03).
      *    低年金−入院−回−固定額
                  09 HKNWK-TNK-NYUKKOTGAK       PIC  9(06).
      *    低年金−入院−回−上限額
                  09 HKNWK-TNK-NYUKJGNGAK       PIC  9(06).
      *    低年金−入院−日−上限額
                  09 HKNWK-TNK-NYUDJGNGAK       PIC  9(06).
      *    低年金−入院−日−上限回数
                  09 HKNWK-TNK-NYUDJGNCNT       PIC  9(02).
      *    低年金−入院−月−上限額
                  09 HKNWK-TNK-NYUMJGNGAK       PIC  9(06).
      *    低年金−入院−月−上限回数
                  09 HKNWK-TNK-NYUMJGNCNT       PIC  9(02).
      *    低年金−入院−食事療養区分
                  09 HKNWK-TNK-NYUSKJRYYKBN     PIC  X(01).
      *    低年金−入院−食事負担助成額（日）
                  09 HKNWK-TNK-NYUSKJJYOGAK     PIC  9(06).
      *
           03 HKNWK-NAIYOU-R                REDEFINES   HKNWK-NAIYOU.
              05 HKNWK-TBL                  OCCURS  4.
      *  外来−負担区分
                07 HKNWK-GAIFTNKBN          PIC  X(01).
      *  外来−内容
                07 HKNWK-GAI-NAIYO.
      *    外来−回−負担割合
                  09 HKNWK-GAIKFTNRATE         PIC  9(03).
      *    外来−回−固定額
                  09 HKNWK-GAIKKOTGAK          PIC  9(06).
      *    外来−回−上限額
                  09 HKNWK-GAIKJGNGAK          PIC  9(06).
      *    外来−日−上限額
                  09 HKNWK-GAIDJGNGAK          PIC  9(06).
      *    外来−日−上限回数
                  09 HKNWK-GAIDJGNCNT          PIC  9(02).
      *    外来−月−院内−上限額
                  09 HKNWK-GAIMNAIJGNGAK       PIC  9(06).
      *    外来−月−院外−上限額
                  09 HKNWK-GAIMGAIJGNGAK       PIC  9(06).
      *    外来−月−上限回数
                  09 HKNWK-GAIMJGNCNT          PIC  9(02).
      *    外来−薬剤負担区分
                  09 HKNWK-GAIYKZFTNKBN        PIC  X(01).
      *  入院−負担区分
                07 HKNWK-NYUFTNKBN             PIC  X(01).
      *  入院−内容
                07 HKNWK-NYU-NAIYO.
      *    入院−回−負担割合
                  09 HKNWK-NYUKFTNRATE         PIC  9(03).
      *    入院−回−固定額
                  09 HKNWK-NYUKKOTGAK          PIC  9(06).
      *    入院−回−上限額
                  09 HKNWK-NYUKJGNGAK          PIC  9(06).
      *    入院−日−上限額
                  09 HKNWK-NYUDJGNGAK          PIC  9(06).
      *    入院−日−上限回数
                  09 HKNWK-NYUDJGNCNT          PIC  9(02).
      *    入院−月−上限額
                  09 HKNWK-NYUMJGNGAK          PIC  9(06).
      *    入院−月−上限回数
                  09 HKNWK-NYUMJGNCNT          PIC  9(02).
      *    入院−食事療養区分
                  09 HKNWK-NYUSKJRYYKBN        PIC  X(01).
      *    入院−食事負担助成額（日）
                  09 HKNWK-NYUSKJJYOGAK        PIC  9(06).
      *　　端末ＩＤ
           03 HKNWK-TERMID                  PIC  X(16).
      *　　オペーレータＩＤ
           03 HKNWK-OPID                    PIC  X(16).
      *　　作成年月日
           03 HKNWK-CREYMD                  PIC  X(08).
      *　　更新年月日
           03 HKNWK-UPYMD                   PIC  X(08).
      *　　更新時間
           03 HKNWK-UPHMS                   PIC  X(06).
