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
      *    診療会計マスタ    COPY (CPSRYACCT.INC)       *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-多々納  13/01/11  診療回数テーブル１０対応
      ************************************************************
      *01  SRYACCT-REC.
           03  ACCT-KEY.
      *医療機関ＩＤ
               05  ACCT-HOSPNUM          PIC 9(2).
      *入外区分
               05  ACCT-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  ACCT-PTID            PIC  9(10).
      *診療科
               05  ACCT-SRYKA           PIC  X(02).
      *診療年月
               05  ACCT-SRYYM           PIC  X(06).
      *診療区分
               05  ACCT-SRYKBN          PIC  X(02).
      *剤番号
               05  ACCT-ZAINUM          PIC  9(08).
      *保険組合せ
           03  ACCT-HKNCOMBI            PIC  9(04).
      *自保区分（包括剤区分）
           03  ACCT-JIHOKBN             PIC  X(01).
      *剤点数
           03  ACCT-ZAITEN              PIC  9(08).
      *診療コード計
           03  ACCT-SRYCDTOTAL          PIC  9(14).
      *数量計
           03  ACCT-SURYOUTOTAL         PIC  9(07)V9(05).
      *明細数
           03  ACCT-MEISAISU            PIC  9(07).
      *剤回数
           03  ACCT-ZAIKAISU            PIC  9(07).
      *手技点数１
           03  ACCT-SYUTEN1             PIC  9(07).
      *手技点数２
           03  ACCT-SYUTEN2             PIC  9(07).
      *薬剤点数
           03  ACCT-YKZTEN              PIC  9(07).
      *器材点数
           03  ACCT-KIZAITEN            PIC  9(07).
      *ドクターコード
           03  ACCT-DRCD                PIC  X(05).
      *剤請求フラグ（付加レコードフラグ）
           03  ACCT-ZAIREQFLG           PIC  X(01).
      *逓減率
           03  ACCT-TEIGENRATE          PIC  9(03).
      *剤識別区分（１：薬評、２：減点分）
           03  ACCT-ZAIKBN              REDEFINES
               ACCT-TEIGENRATE          PIC  9(03).
      *診療回数テーブル
           03  ACCT-DAY-AREA.
      ******** 診療回数テーブル（1:合計,2:１回,3:２回, 4:３回以上）
      ****** 04  ACCT-DAY-TBL         OCCURS  4.
      *        診療回数テーブル（1:合計,2〜10：１回〜９回）
             04  ACCT-DAY-TBL         OCCURS  10.
      *        診療回数（１日から３１日）
               05  ACCT-DAY-1           PIC  9(03).
               05  ACCT-DAY-2           PIC  9(03).
               05  ACCT-DAY-3           PIC  9(03).
               05  ACCT-DAY-4           PIC  9(03).
               05  ACCT-DAY-5           PIC  9(03).
               05  ACCT-DAY-6           PIC  9(03).
               05  ACCT-DAY-7           PIC  9(03).
               05  ACCT-DAY-8           PIC  9(03).
               05  ACCT-DAY-9           PIC  9(03).
               05  ACCT-DAY-10          PIC  9(03).
               05  ACCT-DAY-11          PIC  9(03).
               05  ACCT-DAY-12          PIC  9(03).
               05  ACCT-DAY-13          PIC  9(03).
               05  ACCT-DAY-14          PIC  9(03).
               05  ACCT-DAY-15          PIC  9(03).
               05  ACCT-DAY-16          PIC  9(03).
               05  ACCT-DAY-17          PIC  9(03).
               05  ACCT-DAY-18          PIC  9(03).
               05  ACCT-DAY-19          PIC  9(03).
               05  ACCT-DAY-20          PIC  9(03).
               05  ACCT-DAY-21          PIC  9(03).
               05  ACCT-DAY-22          PIC  9(03).
               05  ACCT-DAY-23          PIC  9(03).
               05  ACCT-DAY-24          PIC  9(03).
               05  ACCT-DAY-25          PIC  9(03).
               05  ACCT-DAY-26          PIC  9(03).
               05  ACCT-DAY-27          PIC  9(03).
               05  ACCT-DAY-28          PIC  9(03).
               05  ACCT-DAY-29          PIC  9(03).
               05  ACCT-DAY-30          PIC  9(03).
               05  ACCT-DAY-31          PIC  9(03).
           03  ACCT-DAY-AREA-R           REDEFINES   ACCT-DAY-AREA.
      ******   診療回数テーブル（1:合計,2:１回,3:２回, 4:３回、5:４回)
      *******  05  ACCT-DAY-TBL2        OCCURS  4.
      *        診療回数テーブル（1:合計,2〜10：１回〜９回）
               05  ACCT-DAY-TBL2        OCCURS  10.
                   07  ACCT-DAY         PIC  9(03)
                                        OCCURS   31.
      *端末ＩＤ
           03  ACCT-TERMID              PIC  X(16).
      *オペーレータＩＤ
           03  ACCT-OPID                PIC  X(16).
      *作成年月日
           03  ACCT-CREYMD              PIC  X(08).
      *更新年月日
           03  ACCT-UPYMD               PIC  X(08).
      *更新時間
           03  ACCT-UPHMS               PIC  X(06).
      *診療年月検索用項目
           03  ACCT-STSRYYM             PIC  X(06).
           03  ACCT-EDSRYYM             PIC  X(06).
