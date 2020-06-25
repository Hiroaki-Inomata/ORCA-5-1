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
      *    旧姓履歴　    　　　 COPY  (CPKYUSEIRRK.INC)    *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/07/23  主キー更新用項目 追加
      ************************************************************
      *01  KYUSEIRRK-REC.
      *旧姓履歴
           03  KYUSEI-KEY.
      *医療機関ＩＤ
               05  KYUSEI-HOSPNUM           PIC 9(2).
      *患者ＩＤ
               05  KYUSEI-PTID             PIC  9(10).
      *変更年月日
               05  KYUSEI-CHGYMD           PIC  X(08).
      *カナ氏名
           03  KYUSEI-KANANAME             PIC  X(100).
      *漢字氏名
           03  KYUSEI-NAME                 PIC  X(100).
      *通称
           03  KYUSEI-NICKNAME             PIC  X(100).
      *端末ＩＤ
           03  KYUSEI-TERMID               PIC  X(16).
      *オペーレータＩＤ
           03  KYUSEI-OPID                 PIC  X(16).
      *作成年月日
           03  KYUSEI-CREYMD               PIC  X(08).
      *更新年月日
           03  KYUSEI-UPYMD                PIC  X(08).
      *更新時間
           03  KYUSEI-UPHMS                PIC  X(06).
      *　　主キー更新用項目
           03  KYUSEIUP-KEY.
               05  KYUSEIUP-HOSPNUM         PIC 9(2).
               05  KYUSEIUP-PTID           PIC 9(10).
               05  KYUSEIUP-CHGYMD         PIC X(08).
