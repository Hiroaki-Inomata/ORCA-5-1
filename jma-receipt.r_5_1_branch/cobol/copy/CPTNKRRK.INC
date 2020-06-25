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
      *    年金履歴　        　 COPY (CPTNKRRK.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  TNKRRK-REC.
      *キー
           03  TNKRRK-KEY.
      *医療機関ＩＤ
               05  TNKRRK-HOSPNUM           PIC 9(2).
      *患者ＩＤ
               05  TNKRRK-PTID             PIC  9(10).
      *開始年月日
               05  TNKRRK-STYMD            PIC  X(08).
      *終了年月日
           03  TNKRRK-EDYMD                PIC  X(08).
      *食事標準負担額減額開始日
           03  TNKRRK-SKJNINSTYMD          PIC  X(08).
      *老齢福祉年金受給者証区分（０：なし　１：あり）
           03  TNKRRK-RRI-FUKUSHI-KBN      PIC  X(01).
      *保険適用区分（０：全て対象　１：地方公費は対象外
      *              ２：地方公費のみ対象）
           03  TNKRRK-HKNTEKKBN            PIC  X(01).
      *境界層該当区分（空白又は０：境界層非該当　１：境界層該当
           03  TNKRRK-KYOUKAISOUKBN        PIC  X(01).
      *端末ＩＤ
           03  TNKRRK-TERMID               PIC  X(16).
      *オペーレータＩＤ
           03  TNKRRK-OPID                 PIC  X(16).
      *作成年月日
           03  TNKRRK-CREYMD               PIC  X(08).
      *更新年月日
           03  TNKRRK-UPYMD                PIC  X(08).
      *更新時間
           03  TNKRRK-UPHMS                PIC  X(06).
      *　　主キー更新用項目
           03  TNKRRK-KEY-O.
               05  TNKRRK-HOSPNUM-O           PIC 9(2).
               05  TNKRRK-PTID-O             PIC  9(10).
               05  TNKRRK-STYMD-O            PIC  X(08).
