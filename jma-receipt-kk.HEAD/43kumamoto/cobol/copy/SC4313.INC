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
      *  01.00.01    萩野         12/08/14  ページ数、総ページ数の桁数増加
      *  01.00.02    門間         18/09/05  不要コメントを削除
      *  01.00.03    萩野         19/05/10  印刷日付に元号追加
      *****************************************************************
       01  SC13.
      *印刷日付
           03  SC13-PRTG              PIC  X(04).
           03  SC13-PRTY              PIC  X(04).
           03  SC13-PRTM              PIC  X(04).
      *総合計
           03  SC13-GOKEI-TBL.
      *合計−件数
               05  SC13-GKENSU        PIC  X(05).
      *合計−保険総点数
               05  SC13-GTENSU        PIC  X(09).
      *合計−結核・精神負担額
               05  SC13-GKESETEN      PIC  X(09).
      *医療機関コード
           03  SC13-HOSPCD            PIC  X(14).
      *請求日付
           03  SC13-SEIYMD            PIC  X(22).
      *医療機関所在地
           03  SC13-ADRS              PIC  X(100).
      *医療機関名称
           03  SC13-HOSPNAME          PIC  X(100).
      *管理者名
           03  SC13-KAISETU           PIC  X(40).
      *電話番号
           03  SC13-TEL               PIC  X(15).
      *総ページ数
           03  SC13-GPAGE             PIC  X(03).
      *ページ数
           03  SC13-PAGE              PIC  X(03).
      *
           03  SC13-MEISAI-TBL OCCURS 10.
      *受給資格者証記号番号
               05  SC13-KIGO           PIC  X(04).
               05  SC13-NUM            PIC  X(14).
      *保険者番号
               05  SC13-HKNJANUM       PIC  X(16).
      *受給者氏名
               05  SC13-KANANAME       PIC  X(30).
               05  SC13-NAME           PIC  X(20).
      *入院外来区分
               05  SC13-NYUGAIKBN      PIC  X(02) OCCURS 2.
      *保険総点数
               05  SC13-TENSU.
                   07  SC13-TENSU-OCC  PIC  X(02) OCCURS 5.
      *結核・精神区分
               05  SC13-KESEKBN        PIC  X(02) OCCURS 2.
      *結核・精神点数
               05  SC13-KESETEN        PIC  X(10).
      *診療科目
               05  SC13-SRYKA          PIC  X(02).
      *診療年月
               05  SC13-SRYYY          PIC  X(04).
               05  SC13-SRYMM          PIC  X(04).
      *小計
           03  SC13-SYOKEI-TBL.
      *
      *小計−保険総点数
               05  SC13-STENSU.
                   07  SC13-STENSU-OCC  PIC  X(02) OCCURS 5.
      *小計−結核・精神負担額
               05  SC13-SKESETEN       PIC  X(10).
