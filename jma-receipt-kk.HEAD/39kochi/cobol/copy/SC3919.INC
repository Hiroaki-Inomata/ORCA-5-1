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
      ************************************************************
        01  SC19.
      *表示年月
            03  SC19-PRTYM                       PIC X(16).
      *請求年月日
            03  SC19-SEIYMD                      PIC X(22).
      *医療機関コード
            03  SC19-HOSPCDN                     PIC X(20).
      *医療機関住所
            03  SC19-ADRS                        PIC X(100).
      *医療機関名称
            03  SC19-HOSPNAME                    PIC X(100).
      *開設者名
            03  SC19-KAISETU                     PIC X(40).
      *市町村名
            03  SC19-CITYNAME                    PIC X(26).
      *負担者番号
            03  SC19-FTNJANUMG.
              05  SC19-FTNJANUM
                                    PIC X(02)    OCCURS  8 TIMES.
      *受給者番号
            03  SC19-JKYSNUMG.
              05  SC19-JKYSNUM
                                    PIC X(02)    OCCURS  7 TIMES.
      *保険者番号
            03  SC19-HKNJANUM                    PIC X(16).
      *記号・番号
            03  SC19-KIGO                        PIC X(20).
            03  SC19-BANGO                       PIC X(20).
      *カナ氏名
            03  SC19-KANANAME                    PIC X(50).
      *氏名
            03  SC19-NAME                        PIC X(30).
      *給付割合
            03  SC19-KYURATE                     PIC X(02).
      *性別
            03  SC19-SEXG                        OCCURS  2 TIMES.
              05  SC19-SEX                       PIC X(02).
      *請求明細
            03  SC19-SEIKYU-TBL                  OCCURS  2 TIMES.
      *実日数
              05  SC19-NISSU                     PIC X(04).
      *点数
              05  SC19-TENSU                     PIC X(12).
      *金額
              05  SC19-SEIKYUGK                  PIC X(12).
      *長期
            03  SC19-TYOKI1                      PIC X(02).
            03  SC19-TYOKI2                      PIC X(02).
      *備考
            03  SC19-BIKO                        PIC X(16).
            03  SC19-BIKO2                       PIC X(16).
      *他法法別
            03  SC19-TAHOUHBT                    PIC X(02).
      *他法負担額
            03  SC19-TAHOUFTN                    PIC X(14).
      *備考（全国公費に係る患者負担）
            03  SC19-BIKOTAHOU                   PIC X(28).
