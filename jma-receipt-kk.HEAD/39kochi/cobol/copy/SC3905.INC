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
        01  SC05.
      *表示年月
            03  SC05-PRTYM                    PIC X(16).
      *請求年月日
            03  SC05-SEIYMD                   PIC X(22).
      *医療機関コード
            03  SC05-HOSPCDN                  PIC X(20).
      *医療機関住所
            03  SC05-ADRS                     PIC X(100).
      *医療機関名称
            03  SC05-HOSPNAME                 PIC X(100).
      *開設者名
            03  SC05-KAISETU                  PIC X(40).
      *市町村名
            03  SC05-CITYNAME                 PIC X(26).
      *負担者番号
            03  SC05-FTNJANUMG.
              05  SC05-FTNJANUM
                                          PIC X(02)    OCCURS  8 TIMES.
      *受給者番号
            03  SC05-JKYSNUMG.
              05  SC05-JKYSNUM
                                          PIC X(02)    OCCURS  7 TIMES.
      *保険者番号
            03  SC05-HKNJANUM                 PIC X(16).
      *記号・番号
            03  SC05-KIGO                     PIC X(20).
            03  SC05-BANGO                    PIC X(20).
      *カナ氏名
            03  SC05-KANANAME                 PIC X(50).
      *氏名
            03  SC05-NAME                     PIC X(30).
      *給付割合
            03  SC05-KYURATE                  PIC X(02).
      *性別
            03  SC05-SEXG                   OCCURS  2 TIMES.
              05  SC05-SEX                    PIC X(02).
      *請求明細
            03  SC05-SEIKYU-TBL             OCCURS  3 TIMES.
      *実日数
              05  SC05-NISSU                  PIC X(04).
      *点数
              05  SC05-TENSU                  PIC X(12).
      *金額
              05  SC05-SEIKYUGK               PIC X(12).
      *長期
            03  SC05-TYOKI1                   PIC X(02).
            03  SC05-TYOKI2                   PIC X(02).
      *備考
            03  SC05-BIKO                     PIC X(16).
            03  SC05-BIKO2                    PIC X(16).
            03  SC05-BIKO3                    PIC X(16).
      *他法法別
            03  SC05-TAHOUHBT                 PIC X(02).
      *他法負担額
            03  SC05-TAHOUFTN                 PIC X(14).
