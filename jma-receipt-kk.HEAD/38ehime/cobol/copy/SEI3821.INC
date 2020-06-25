      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will beuseful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditionsdescribed
      * in the JMA Open Source License. You should have received acopy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku,Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ************************************************************
      *          医療費請求書（複写式レセ） COPY (CPRMF10.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *    ＫＥＹ
      *    法別番号用ソートKEY(81,82,83の順)
               05  MF100-SORT1               PIC  9(01).
      *    合計用区分(１：合計データ ２：一般データ)
               05  MF100-KBN                 PIC  9(01).
      *    市町村番号
               05  MF100-CITYNUM             PIC  X(05).
      *    負担者番号降順用KEY(8*が先で7*が後)
               05  MF100-SORT2               PIC  9(01).
      *    負担者番号
           03  MF100-FTNJANUM                PIC  X(08).
      *    請求エリア
           03  MF100-SEIKYU-AREA.
               05  MF100-HKN-TBL  OCCURS 2.
      *    件数
                   07  MF100-KENSU           PIC  9(05).
      *    点数
                   07  MF100-SEIKYUGK        PIC  9(09).
      *    合計市町村数カウント用エリア
           03  MF100-CNT-AREA.
               05  MF100-CNT-CITY            PIC  9(03).
               05  MF100-CNT-TBL  OCCURS 100.
                   07  MF100-CNT-CITYNUM     PIC  X(05).
