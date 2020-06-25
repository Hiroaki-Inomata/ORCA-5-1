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
      *    入院会計マスタ    COPY (CPNYUINACCT.INC)       *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  NYUIN-SRYACCT-REC.
           03  NACCT-KEY.
      *医療機関ＩＤ
               05  NACCT-HOSPNUM          PIC 9(2).
      *入外区分
               05  NACCT-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  NACCT-PTID            PIC  9(10).
      *診療科
               05  NACCT-SRYKA           PIC  X(02).
      *診療年月
               05  NACCT-SRYYM           PIC  X(06).
      *診療区分
               05  NACCT-SRYKBN          PIC  X(02).
      *剤番号
               05  NACCT-ZAINUM          PIC  9(08).
      *剤識別区分（１：入院料　２：外泊　３：室料差額　４：食事
      *            ５：保険組合せ　６：入院料加算　７：療養担当手当）
           03  NACCT-ZAISKBKBN           PIC  X(01).           
      *保険組合せ
           03  NACCT-HKNCOMBI            PIC  9(04).
      *剤点数
           03  NACCT-ZAITEN              PIC  9(08).
      *診療コード計
           03  NACCT-SRYCDTOTAL          PIC  9(14).
      *数量計
           03  NACCT-SURYOUTOTAL         PIC  9(07)V9(03).
      *明細数
           03  NACCT-MEISAISU            PIC  9(07).
      *剤回数
           03  NACCT-ZAIKAISU            PIC  9(07).
      *手技点数１
           03  NACCT-SYUTEN1             PIC  9(07).
      *手技点数２
           03  NACCT-SYUTEN2             PIC  9(07).
      *薬剤点数
           03  NACCT-YKZTEN              PIC  9(07).
      *器材点数
           03  NACCT-KIZAITEN            PIC  9(07).
      *ドクターコード
           03  NACCT-DRCD                PIC  X(05).
      *診療回数テーブル
           03  NACCT-DAY-AREA.
      *        診療回数テーブル
             04  NACCT-DAY-TBL.
      *        診療回数（１日から３１日）
               05  NACCT-DAY-1           PIC  9(03).
               05  NACCT-DAY-2           PIC  9(03).
               05  NACCT-DAY-3           PIC  9(03).
               05  NACCT-DAY-4           PIC  9(03).
               05  NACCT-DAY-5           PIC  9(03).
               05  NACCT-DAY-6           PIC  9(03).
               05  NACCT-DAY-7           PIC  9(03).
               05  NACCT-DAY-8           PIC  9(03).
               05  NACCT-DAY-9           PIC  9(03).
               05  NACCT-DAY-10          PIC  9(03).
               05  NACCT-DAY-11          PIC  9(03).
               05  NACCT-DAY-12          PIC  9(03).
               05  NACCT-DAY-13          PIC  9(03).
               05  NACCT-DAY-14          PIC  9(03).
               05  NACCT-DAY-15          PIC  9(03).
               05  NACCT-DAY-16          PIC  9(03).
               05  NACCT-DAY-17          PIC  9(03).
               05  NACCT-DAY-18          PIC  9(03).
               05  NACCT-DAY-19          PIC  9(03).
               05  NACCT-DAY-20          PIC  9(03).
               05  NACCT-DAY-21          PIC  9(03).
               05  NACCT-DAY-22          PIC  9(03).
               05  NACCT-DAY-23          PIC  9(03).
               05  NACCT-DAY-24          PIC  9(03).
               05  NACCT-DAY-25          PIC  9(03).
               05  NACCT-DAY-26          PIC  9(03).
               05  NACCT-DAY-27          PIC  9(03).
               05  NACCT-DAY-28          PIC  9(03).
               05  NACCT-DAY-29          PIC  9(03).
               05  NACCT-DAY-30          PIC  9(03).
               05  NACCT-DAY-31          PIC  9(03).
           03  NACCT-DAY-AREA-R          REDEFINES   NACCT-DAY-AREA.
      *        診療回数テーブル
               05  NACCT-DAY-TBL2.
                   07  NACCT-DAY         PIC  9(03)
                                         OCCURS   31.
      *端末ＩＤ
           03  NACCT-TERMID              PIC  X(16).
      *オペーレータＩＤ
           03  NACCT-OPID                PIC  X(16).
      *作成年月日
           03  NACCT-CREYMD              PIC  X(08).
      *更新年月日
           03  NACCT-UPYMD               PIC  X(08).
      *更新時間
           03  NACCT-UPHMS               PIC  X(06).
      *
