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
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    萩野    2019/03/08  修正履歴コメント削除
      *****************************************************************
       01  SC05.
      *印刷年月
         03  SC05-PRTYM          PIC  X(16).
      *請求年月日
         03  SC05-SEIYMD         PIC  X(22).
      *請求先
         03  SC05-CITYNAME       PIC  X(20).
      *住所
         03  SC05-ADRS           PIC  X(100).
      *医療機関名
         03  SC05-HOSPNAME       PIC  X(100).
      *管理者名
         03  SC05-KAISETU        PIC  X(40).
      *医療機関コード
         03  SC05-HOSPCD         PIC  X(14).
      *割合
         03  SC05-FTNWARI        PIC X(02) OCCURS 3.
      *
         03  SC05-MEISAI-TBL    OCCURS 20.
      *長期
             05  SC05-TYOKI1     PIC  X(04).
             05  SC05-TYOKI2     PIC  X(02).
      *氏名
             05  SC05-NAME       PIC  X(24).
      *誕生年月
             05  SC05-TANJOYM    PIC  X(10).
      *法別番号
             05  SC05-HBTNUM     PIC  X(04).
      *公費負担者番号
             05  SC05-FTNJANUM   PIC  X(08).
      *受給者番号
             05  SC05-JKYSNUM    PIC  X(14).
      *診療年月
             05  SC05-SRYYM      PIC  X(08).
      *入外区分
             05  SC05-NYUGAIKBN  PIC  X(02) OCCURS 2.
      *日数
             05  SC05-NISSU      PIC  X(04).
      *点数
             05  SC05-TENSU      PIC  X(12).
      *一部負担金
             05  SC05-ITBFTN     PIC  X(10).
      *備考
             05  SC05-BIKO1       PIC  X(02).
             05  SC05-BIKO2       PIC  X(02).
      *合計
         03  SC05-GOKEI-TBL.
      *合計件数
             05  SC05-GKENSU     PIC  X(04).
      *合計日数
             05  SC05-GNISSU     PIC  X(06).
      *合計点数
             05  SC05-GTENSU     PIC  X(14).
      *合計一部負担金
             05  SC05-GITBFTN    PIC  X(14).
