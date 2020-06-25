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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    門間    2019/01/07  修正履歴コメント削除
      *  01.00.02    井上    2019/09/11  備考追加
      ************************************************************
       01  SC03.
      *表示年月
            03  SC03-PRTYM                        PIC X(16).
      *請求年月日
            03  SC03-SEIYMD                       PIC X(22).
      *医療機関コード
            03  SC03-HOSPCDN                      PIC X(20).
      *医療機関住所
            03  SC03-ADRS                         PIC X(100).
      *医療機関名称
            03  SC03-HOSPNAME                     PIC X(100).
      *開設者名
            03  SC03-KAISETU                      PIC X(40).
      *市町村名
            03  SC03-CITYNAME                     PIC X(26).
      *負担者番号
            03  SC03-FTNJANUMG.
              05  SC03-FTNJANUM
                                          PIC X(02)    OCCURS  8 TIMES.
      *受給者番号
            03  SC03-JKYSNUMG.
              05  SC03-JKYSNUM
                                          PIC X(02)    OCCURS  7 TIMES.
      *保険者番号
            03  SC03-HKNJANUM                     PIC X(16).
      *記号・番号
            03  SC03-KIGO                         PIC X(20).
            03  SC03-BANGO                        PIC X(20).
      *カナ氏名
            03  SC03-KANANAME                     PIC X(50).
      *氏名
            03  SC03-NAME                         PIC X(30).
      *給付割合
            03  SC03-KYURATE                      PIC X(02).
      *性別
            03  SC03-SEXG                     OCCURS  2 TIMES.
              05  SC03-SEX                        PIC X(02).
      *請求明細
            03  SC03-SEIKYU-TBL               OCCURS  3 TIMES.
      *実日数
              05  SC03-NISSU                      PIC X(04).
      *点数
              05  SC03-TENSU                      PIC X(12).
      *金額
              05  SC03-SEIKYUGK                   PIC X(12).
      *長期
            03  SC03-TYOKI1                       PIC X(02).
            03  SC03-TYOKI2                       PIC X(02).
      *備考
            03  SC03-BIKO                         PIC X(16).
            03  SC03-BIKO2                        PIC X(16).
            03  SC03-BIKO3                        PIC X(16).
      *他法法別
            03  SC03-TAHOUHBT                     PIC X(02).
      *他法負担額
            03  SC03-TAHOUFTN                     PIC X(14).
      *備考（全国公費に係る患者負担）
            03  SC03-BIKOTAHOU                    PIC X(28).
      *備考 (後期高齢入院)
            03  SC03-BIKO4                        PIC X(22).
