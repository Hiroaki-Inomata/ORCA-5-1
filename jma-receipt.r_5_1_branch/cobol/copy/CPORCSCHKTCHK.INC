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
      *----------------------------------------------------------*
      *    診療行為包括診療チェクパラメタ
      *                                      COPY (CPORCSCHKTCHK.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCSCHKTCHKAREA.
      *    処理区分（１：全体チェック、２：自動発生分チェック）
           03  ORCSCHKTCHK-SYORI          PIC X(01).
      *    ＰＧ名
           03  ORCSCHKTCHK-PG             PIC X(06).
      *
           03  ORCSCHKTCHK-OUTAREA.
      *        包括対象あり
               05  ORCSCHKTCHK-HKTFLG     PIC 9(01).
      *        エラーあり
               05  ORCSCHKTCHK-HKTERR     PIC 9(01).
      *        入院料算定なし
               05  ORCSCHKTCHK-NYUERR     PIC 9(01).
      *H29.1
      *        翌月入院料算定なし
               05  ORCSCHKTCHK-NYUERR-2     PIC 9(01).
      *    入院料包括情報
           03  ORCSCHKTCHK-NYUINAREA.
               05  ORCSCHKTCHK-NYUHKTCHK       PIC 9(01).
      *        診療日の包括区分
               05  ORCSCHKTCHK-NYUHKTCD        PIC X(03).
      *        診療日の入院料コード
               05  ORCSCHKTCHK-NYUSRYCD        PIC X(09).
               05  ORCSCHKTCHK-NYUHKT-G.
                   07  ORCSCHKTCHK-NYUHKT-OCC  OCCURS   31.
      *                ０：違う包括、１：同じ包括
                       09  ORCSCHKTCHK-NYUHKTFLG   PIC 9(01).
      *                １：出来高算定日
                       09  ORCSCHKTCHK-DEKIDAFLG   PIC 9(01).
      *外来まとめ算定履歴領域
           03  ORCSCHKTCHK-KT01-AREA.
               05  ORCSCHKTCHK-KT01FLG         PIC X(01).
               05  ORCSCHKTCHK-KT01-OCC        OCCURS   100.
                   07  ORCSCHKTCHK-KT01-SRYCD  PIC X(09).
                   07  ORCSCHKTCHK-KT01-DAY-G.
                       09  ORCSCHKTCHK-KT01-DAY     PIC 9(03)
                                                  OCCURS   31.
      *
