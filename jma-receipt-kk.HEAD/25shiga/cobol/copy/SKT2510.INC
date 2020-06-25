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
      *                     COPY (CPSKT2510TMP.INC)              *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2009/06/01  帳票様式の変更に伴う修正
      *  01.00.02    吉川    2010/04/14  帳票様式の変更に伴う修正
      *  01.00.03    吉川    2010/11/01  特別療養費国保対応
      *  01.00.04    井上    2018/12/12  修正履歴コメント削除
      ************************************************************
       01  SKT2510.
      *診療年月
           03  SKT2510-SRYYM               PIC  X(16).
      *医療機関コード
           03  SKT2510-HOSPCD              PIC  X(07).
      *医療機関所在地
           03  SKT2510-ADRS                PIC  X(100).
      *医療機関名称
           03  SKT2510-HOSPNAME            PIC  X(100).
      *開設者氏名
           03  SKT2510-KAISETU             PIC  X(80).
      *電話番号
           03  SKT2510-TEL                 PIC  X(15).
      *請求年月日
           03  SKT2510-SEIYMD              PIC  X(22).
      *特別療養費
           03  SKT2510-TOKUBETU            PIC  X(10).
      *帳票固定エリア
           03  SKT2510-KOTEI-TBL  OCCURS 25.
               05  SKT2510-K-KENSU         PIC  X(05).
      *請求エリア
           03  SKT2510-SEIKYU-TBL  OCCURS 50.
      *保険者名
               05  SKT2510-HKNJANAME1      PIC  X(12).
               05  SKT2510-HKNJANAME2      PIC  X(28).
      *保険者番号
               05  SKT2510-HKNJANUM        PIC  X(06).
      *給付割合
               05  SKT2510-KYURATE         PIC  X(02).
      *件数
               05  SKT2510-KENSU           PIC  X(05).
      *福祉医療費請求書枚数合計
           03  SKT2510-FKSGMAISU           PIC  X(05).
      *磁気媒体枚数合計
           03  SKT2510-JIKIMAISU           PIC  X(05).
      *合計件数
           03  SKT2510-GKENSU              PIC  X(05).
      *後期高齢者エリア
           03  SKT2510-KOUKI-TBL  OCCURS 24.
               05  SKT2510-TODOHUKEN       PIC  X(12).
               05  SKT2510-KENSUK          PIC  X(05).
      *合計件数（後期高齢者）
           03  SKT2510-GKENSUK             PIC  X(05).
