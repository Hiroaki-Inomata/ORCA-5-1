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
      *    分娩管理情報編集サブ　COPY (CPORCSI02.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SI02-AREA.
      *    REQID
      *    1:チェック/2:更新/3:チェック＆更新
           03  SI02-REQID          PIC X(01).
           03  SI02-BUNBENKBN      PIC X(01).
           03  SI02-RIYOUKBN       PIC X(01).
           03  SI02-BUNBENCHK      PIC 9(01).
           03  SI02-BUNBENNUM      PIC 9(03).
           03  SI02-RC             PIC 9(02).
