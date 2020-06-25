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
      *    収集データ（基本情報）         COPY (CPCDF001.INC)    *
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  COLDAT01-REC.
           03  COLDAT01-KEY.
      *        医療機関ＩＤ
               05  COLDAT01-HOSPID         PIC  X(24).
      *        診療年月
               05  COLDAT01-SRYYM          PIC  9(06).
      *        入外区分
               05  COLDAT01-NYUGAIKBN      PIC  X(01).
      *        患者ＩＤ
               05  COLDAT01-PTID           PIC  9(10).
      *        レセプト診療科
               05  COLDAT01-RECEKA         PIC  X(02).
      *        主保険保険番号
               05  COLDAT01-HKNNUM         PIC  X(03).
      *        公費ＩＤ
               05  COLDAT01-KOHID          PIC  9(10).
      *        保険者番号
               05  COLDAT01-HKNJANUM       PIC  X(08).
      *        本人家族区分
               05  COLDAT01-HONKZKKBN      PIC  X(01).
      *        補助区分
               05  COLDAT01-HOJOKBN        PIC  X(01).
      *
      *        主保険ＩＤ
           03  COLDAT01-HKNID              PIC  9(10).
      *
      *    主保険情報
           03  COLDAT01-HKN-AREA.
               05  COLDAT01-HKN-TBL        OCCURS  5.
      *            主保険−保険ＩＤ
                   07  COLDAT01-THKNID     PIC  9(10).
      *            適用開始年月日
                   07  COLDAT01-TTEKSTYMD  PIC  X(08).
      *            適用終了年月日
                   07  COLDAT01-TTEKEDYMD  PIC  X(08).
      *            主保険−継続区分
                   07  COLDAT01-TCONTKBN   PIC  X(01).
      *            主保険−補助区分
                   07  COLDAT01-THOJOKBN   PIC X(01).
      *            主保険−本人家族区分
                   07  COLDAT01-THONKZKKBN PIC  X(01).
      *        主保険−法別番号
               05  COLDAT01-HKNHBTNUM      PIC  X(02).
      *        主保険−公費種別区分
               05  COLDAT01-HKNKOHSBTKBN   PIC X(01).
      *    実日数
           03  COLDAT01-HKNJNISSU          PIC  9(02).
      *    合計点数
           03  COLDAT01-HKNTOTALTENSU      PIC  9(08).
      *    食事療養回数
           03  COLDAT01-HKNSKJNISSU        PIC  9(02).
      *    食事療養費
           03  COLDAT01-HKNSKJRYOYOHI      PIC  9(08).
      *    食事療養費標準負担額
           03  COLDAT01-HKNSKJFTN          PIC  9(08).
      *
      *    老人保健情報
           03  COLDAT01-RJNHKN.
      *        老人保険−保険番号
               05  COLDAT01-RJNHKNNUM      PIC  X(03).
      *        老人支払区分
               05  COLDAT01-RJNPAYKBN      PIC  X(02).
      *        老人保険−法別番号
               05  COLDAT01-RJNHBTNUM      PIC  X(02).
      *        老人適用開始年月日
               05  COLDAT01-RJNTEKSTYMD    PIC  X(08).
      *        老人適用終了年月日
               05  COLDAT01-RJNTEKEDYMD    PIC  X(08).
      *
      *    公費情報
           03 COLDAT01-KOHI-AREA.
               05  COLDAT01-KOHI-TBL       OCCURS  10.
      *            公費ＩＤ
                   07  COLDAT01-TKOHID     PIC  9(10).
      *            公費保険番号
                   07  COLDAT01-TKOHHKNNUM PIC  X(03).
      *            公費支払区分
                   07  COLDAT01-TKOHPAYKBN PIC  X(02).
      *            公費−法別番号
                   07  COLDAT01-TKOHHBTNUM PIC  X(02).
      *            公費−レセプト請求区分
                   07  COLDAT01-TRECESKYKBN PIC  X(01).
      *            適用開始年月日
                   07  COLDAT01-TKOHTEKSTYMD    PIC  X(08).
      *            適用終了年月日
                   07  COLDAT01-TKOHTEKEDYMD    PIC  X(08).
      *            実日数
                   07  COLDAT01-TKOHJNISSU      PIC  9(02).
      *            合計点数
                   07  COLDAT01-TKOHTOTALTENSU  PIC  9(08).
      *            食事療養回数
                   07  COLDAT01-TKOHSKJNISSU    PIC  9(02).
      *            食事療養費
                   07  COLDAT01-TKOHSKJRYOYOHI  PIC  9(08).
      *            食事療養費標準負担額
                   07  COLDAT01-TKOHSKJFTN      PIC  9(08).
      *
      *    労災情報
           03  COLDAT01-HKN.
      *        労災−労災保険区分
               05  COLDAT01-HKNKBN          PIC  X(01).
      *        労災−災害区分
               05  COLDAT01-SAIGAIKBN       PIC  X(01).
      *
      *    患者情報
           03  COLDAT01-PTINF-AREA.
      *        性別
               05  COLDAT01-SEX             PIC  X(01).
      *        生年月日
               05  COLDAT01-BIRTHDAY        PIC  9(08).
      *        年齢
               05  COLDAT01-AGE             PIC  9(03).
      *        就学前区分（１：就学前）
               05  COLDAT01-SYUGAKUKBN      PIC  9(01).
      *        自宅−郵便番号
               05  COLDAT01-HOME-POST       PIC  X(07).
      *    保険組合せ
           03  COLDAT01-HKNCOMBIINF.
               05  COLDAT01-HKNCOMBIINF     OCCURS  10.
      *保険組合せ番号
                   07  COLDAT01-HKNCOMBI    PIC  9(04).
      *主保険番号
                   07  COLDAT01-HKNNUMHC    PIC  X(03).
      *組合せ公費保険番号情報
                   07  COLDAT01-KOHNUMHCINF OCCURS  4.
      *組合せ公費保険番号
                       09  COLDAT01-KOHNUMHC
                                            PIC  X(03).
      *組合せ公費保険ＩＤ
                       09  COLDAT01-KOHIDHC PIC  9(10).
      *    診療科情報
           03  COLDAT01-SRYKAINF.
               05  COLDAT01-SRYKA           PIC  X(02)  OCCURS  20.
      *    入院年月日
           03  COLDAT01-NYUINYMD            PIC  X(08).
      *    退院年月日
           03  COLDAT01-TAIINYMD            PIC  X(08).
      *    審査支払機関
           03  COLDAT01-TEISYUTUSAKI        PIC  9(01).
      *    レセプト種別
           03  COLDAT01-RECESYUBETU         PIC  9(04).
      *
      *    未訂正区分
           03  COLDAT01-TEISEIKBN           PIC  X(01).
