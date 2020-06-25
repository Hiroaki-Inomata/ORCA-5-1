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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     吉川       11/07/13  傷病名追加
      *                                   備考欄に公費対象点数追加
      * 01.00.02     吉川       11/08/19  開設者名のバイト数増加
      * 01.00.03     吉川       11/09/22  備考２追加(診療日数ゼロ理由)
      * 01.00.04     吉川       12/07/05  傷病名欄と総点数のバイト数増加
      * 01.00.05     吉川       14/02/04  備考３追加（複数入院期間）
      * 01.00.06     吉川       16/04/11  備考５追加
      * 01.00.07     門間       18/09/05  不要コメントを削除
      *                                   備考欄の記載修正
      * 01.00.08     萩野       19/06/10  生年月日に新元号の項目を追加
      *                                   入院日、退院日に和暦の項目を追加
      *                                   特定疾病区分の項目を２つに変更
      ********************************************************************
       01  SC23.
      *表示年月
              03  SC23-PRTYM		PIC  X(16).
      *請求年月日
              03  SC23-SEIYMD           PIC  X(22).
      *医療機関住所
              03  SC23-ADRS             PIC  X(100).
      *医療機関名称
              03  SC23-HOSPNAME         PIC  X(100).
      *開設者名
              03  SC23-KAISETU          PIC  X(60).
      *医療機関コード
              03  SC23-HOSPCD 		PIC  X(14).
      *診療科コード(旧総合病院のみ)
              03  SC23-SRYKACD          PIC  X(02).
      *保険区分(１社保 ２国保 ３国保組合)
              03  SC23-HKNBKN-TBL OCCURS 3.
                  05  SC23-HKNKBN       PIC X(02).
      *保険種別(１一般 ２老人 ３退本 ４退家)
              03  SC23-HKNSYUBT-TBL OCCURS 4.
                  05  SC23-HKNSYUBT     PIC X(02).
      *前期高齢者負担割合
              03  SC23-ZNKKBN-TBL.
                  05  SC23-ZNK9         PIC  X(02).
                  05  SC23-ZNK8         PIC  X(02).
                  05  SC23-ZNK7         PIC  X(02).
      *一部負担割合
              03  SC23-ITBFTNWARI-TBL.
                  05  SC23-ITB1         PIC  X(02).
                  05  SC23-ITB2         PIC  X(02).
                  05  SC23-ITB3         PIC  X(02).
      *在宅
              03  SC23-ZAI1             PIC  X(02).
              03  SC23-ZAI2             PIC  X(02).
      *備考
              03  SC23-BIKO             PIC  X(40).
      *患者データテーブル
              03  SC23-KANJA-TBL.
      *保険者番号
                  05  SC23-HKNJANUM     PIC  X(16).
      *記号・番号
                  05  SC23-KIGO         PIC  X(32).
                  05  SC23-BANGO        PIC  X(32).
      *市町村番号
                  05  SC23-CITYNUM      PIC  X(16).
      *老人医療・受給者番号
                  05  SC23-RJNJKYSNUM   PIC  X(14).
      *公費負担者番号
                  05  SC23-KOHFTNJANUM  PIC  X(16).
      *公費負担医療・受給者番号
                  05  SC23-KOHJKYSNUM   PIC  X(14).
      *患者氏名
                  05  SC23-PTNAME       PIC  X(24).
      *性別
                  05  SC23-SEX1         PIC  X(02).
                  05  SC23-SEX2         PIC  X(02).
      *生年月日
                  05  SC23-TANJOG1      PIC X(02).
                  05  SC23-TANJOG2      PIC X(02).
                  05  SC23-TANJOG3      PIC X(02).
                  05  SC23-TANJOG4      PIC X(02).
                  05  SC23-TANJOG5      PIC X(02).
                  05  SC23-TANJOY       PIC X(02).
                  05  SC23-TANJOM       PIC X(02).
                  05  SC23-TANJOD       PIC X(02).
      *入外区分(１、入院  ２、外来)
                  05  SC23-NYUGAIKBN1   PIC X(02).
                  05  SC23-NYUGAIKBN2   PIC X(02).
      *入院診療日数
                  05  SC23-N-SRYNISSU   PIC X(02).
      *入院日
                  05  SC23-NYUING       PIC X(04).
                  05  SC23-NYUINY       PIC X(02).
                  05  SC23-NYUINM       PIC X(02).
                  05  SC23-NYUIND       PIC X(02).
      *退院日
                  05  SC23-TAIING       PIC X(04).
                  05  SC23-TAIINY       PIC X(02).
                  05  SC23-TAIINM       PIC X(02).
                  05  SC23-TAIIND       PIC X(02).
      *外来診療日数
                  05  SC23-G-SRYNISSU   PIC X(02).
      *受診日テーブル
                  05  SC23-SRY-TBL.
                      07  SC23-DAY OCCURS 31 PIC X(02).
      *特定疾病区分
                  05  SC23-TOKUTEI      OCCURS 2    PIC X(02) .
      *総診療報酬点数
                  05  SC23-TOTALTEN     PIC X(09).
      *自己負担月額
                  05  SC23-JIKOFTNGK    PIC X(07).
      *公費負担額
                  05  SC23-KOHFTN       PIC X(07).
      *請求金額
                  05  SC23-SEIKYUGK     PIC X(10).
      *公費種別区分
                  05  SC23-KOHKBN-TBL.
                      07  SC23-KOHKBN OCCURS 6 PIC X(02).
      *月遅れ薬剤一部負担金
                  05  SC23-YAKUZAI-TBL.
                      07  SC23-YAKUZAI1 PIC X(20).
                      07  SC23-YAKUZAI2 PIC X(07).
                      07  SC23-YAKUZAI3 PIC X(02).
      *傷病名及び期間
                  05  SC23-BYOMEI       PIC X(160).
      *公費対象点数
                  05  SC23-TAHOTEN      PIC X(30).
      *備考２
                  05  SC23-BIKO2        PIC X(40).
      *備考３
                  05  SC23-BIKO3        PIC X(208).
      *備考５
                  05  SC23-BIKO5        PIC X(90).
