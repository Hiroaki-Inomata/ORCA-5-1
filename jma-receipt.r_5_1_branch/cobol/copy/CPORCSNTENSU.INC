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
      ******************************************************************
      *    入院点数マスタ付加情報
      ******************************************************************
        01 SNTENSU-AREA.
           03  SNTENSU-BASEYMD                 PIC X(8).
           03  SNTENSU-SRYCD                   PIC X(9).
           03  SNTENSU-RETURN-AREA.
               05  SNTENSU-KBNNUM              PIC 9(3).
               05  SNTENSU-EDANUM              PIC 9(2).
      *        出来高入院料区分 1:出来高入院料
               05  SNTENSU-DEKIDAKAKBN         PIC X(1).
      *        特別入院基本料等区分
      *          1:特別入院基本料
      *          2:月平均夜勤時間超過減算
      *          3:夜勤時間特別入院基本料
      *          4:重症患者割合特別入院基本料
               05  SNTENSU-TOKUBETSUKBN        PIC X(1).
