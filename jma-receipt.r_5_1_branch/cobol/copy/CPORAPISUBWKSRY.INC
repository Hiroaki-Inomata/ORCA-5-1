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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      ******************************************************************
      *    ＡＰＩ中途終了データ編集サブパラメタ
      *                                           (CPORAPISUBWKSRY.INC)
      ******************************************************************
       01  ORAPISUBWKSRYAREA.
      *    処理区分
           03  ORAPIWKSRY-SYORIKBN     PIC X(01).
      *    時間外加算区分
           03  ORAPIWKSRY-TIMEFLG      PIC X(01).
      *    保険情報
           03  ORAPIWKSRY-HKNCOMBI-AREA.
      *        保険組合せ番号
               05  ORAPIWKSRY-HKNCOMBI     PIC X(04).
      *
      *    診療内容
           03  ORAPIWKSRY-SRYACT-AREA.
               05  ORAPIWKSRY-MDC-INFO         OCCURS  40 TIMES.
      *            診療種別区分
                   07  ORAPIWKSRY-MDC-CLASS        PIC X(3).
      *            診療区分名称
                   07  ORAPIWKSRY-MDC-CLASS-NAME   PIC X(50).
      *            回数
                   07  ORAPIWKSRY-MDC-CLASS-NUMBER PIC X(50).
      *            剤内容
                   07  ORAPIWKSRY-PRESCRIPTION-INFO    OCCURS  40 TIMES.
      *                診療コード
                       09  ORAPIWKSRY-PRSCRPT-CODE     PIC X(9).
      *                診療名称（コメント）
                       09  ORAPIWKSRY-PRSCRPT-NAME     PIC X(140).
      *                数量
                       09  ORAPIWKSRY-PRSCRPT-NUMBER   PIC X(40).
      *                処方せん一般名記載
                       09  ORAPIWKSRY-GENERIC-FLG      PIC X(03).
      *
      *    エラー区分
           03  ORAPIWKSRY-ERRCD        PIC X(04).
           03  ORAPIWKSRY-ERRMSG       PIC X(100).
      *
