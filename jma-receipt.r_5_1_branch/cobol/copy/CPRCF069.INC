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
      *    レセプト明細書（入院） 保険履歴 (CPRCF069.INC)        *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-小豆沢  07/05/30  グループ診療対応
      *
      *  04.06.01    NACL-藤原    11/02/01  主保険がない場合、児童保護(53)
      *                                     は公費単独のレセプトとする
      *                                     キーにkohnum 追加
      *  04.06.02    NACL-藤原    12/09/27  生活保護期間重複対応
      ************************************************************
      *01  RECE69-REC.
           03  RECE69-KEY.
      *医療機関ＩＤ
               05  RECE69-HOSPNUM         PIC  9(2).
      *診療年月
               05  RECE69-SRYYM           PIC  9(06).
      *入外区分
               05  RECE69-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  RECE69-PTID            PIC  9(10).
      *レセプト科
               05  RECE69-RECEKA          PIC  X(02).
      *公費番号
               05  RECE69-KOHNUM          PIC  X(03).
      *公費ＩＤ
               05  RECE69-KOHID           PIC  9(10).
      *適用開始年月日
               05  RECE69-TEKSTYMD        PIC  X(08). 
      *主保険情報
           03  RECE69-HKN.
      *主保険−保険ＩＤ
               05  RECE69-HKNID           PIC  9(10).
      *主保険−保険番号
               05  RECE69-HKNNUM          PIC  X(03).
      *主保険−法別番号
               05  RECE69-HKNHBTNUM       PIC  X(02).
      *主保険−保険者番号
               05  RECE69-HKNJANUM        PIC  X(08).
      *主保険−本人家族区分
               05  RECE69-HONKZKKBN       PIC  X(01).
      *主保険−補助区分
               05  RECE69-HOJOKBN         PIC  X(01).
      *主保険−継続区分
               05  RECE69-CONTKBN         PIC  X(01).
      *主保険−記号
               05  RECE69-KIGO            PIC  X(80).
      *主保険−番号
               05  RECE69-NUM             PIC  X(80).
      *主保険−給付割合
               05  RECE69-KYURATE         PIC  9(03).
      *主保険−証明書番号
               05  RECE69-CERTNUM         PIC  X(03).
      *主保険−短縮制度名
               05 RECE69-TANSEIDONAME      PIC  X(20).
      *審査支払機関
           03  RECE69-TEISYUTUSAKI        PIC  9(01).
      *適用開始年月日
           03  RECE69-TEKSTYMDX           PIC  X(08). 
      *適用終了年月日    
           03  RECE69-TEKEDYMDX           PIC  X(08). 
