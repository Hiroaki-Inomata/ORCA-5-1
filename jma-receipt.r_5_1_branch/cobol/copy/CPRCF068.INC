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
      *    レセプト明細書     入退院履歴情報 (CPRCF068.INC)      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    03.05.09  自院歴−入院中区分の追加
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.04.01    NACL-藤原    09/12/10  同月に初回による入院歴が複数
      *                                     ある場合の記録の修正 
      *                                     初回番号の追加
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *  04.06.02    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *
      *  04.07.01    NACL-藤原    15/01/24  同日再入院時の入退院履歴の記録
      *                                     を修正
      *                                    （入院日と退院日が同じとき）
      ************************************************************
      *01  RECE68-REC.
           03  RECE68-KEY.
               05  RECE68-KEY1.
      *医療機関ＩＤ
                   07  RECE68-HOSPNUM      PIC 9(2).
      *診療年月
                   07  RECE68-SRYYM        PIC 9(06).
      *入外区分
                   07  RECE68-NYUGAIKBN    PIC X(01).
      *患者ＩＤ
                   07  RECE68-PTID         PIC 9(10).
      *レセプト科
                   07  RECE68-RECEKA       PIC X(02).
      *審査支払機関
                   07  RECE68-TEISYUTUSAKI
                                           PIC 9(01).
      *レセプト種別
                   07  RECE68-RECESYUBETU  PIC 9(04).
      *保険者番号        
                   07  RECE68-HKNJANUM     PIC X(08).
      *補助区分
                   07  RECE68-HOJOKBN      PIC X(01).
      *公費ＩＤ
                   07  RECE68-KOHID        PIC 9(10).
      *適用開始年月日
                   07  RECE68-TEKSTYMD     PIC X(08). 
      *分割区分
                   07  RECE68-RECEIPT-KBN  PIC X(10).
      *入院年月日
               05  RECE68-NYUINYMD        PIC X(08).
      *入院年月日連番
               05  RECE68-NYUINYMD-SEQNO  PIC 9(01).
      *退院年月日
           03  RECE68-TAIINYMD            PIC X(08).
      *自院歴−入院中区分
           03  RECE68-NYUINCHUKBN         PIC  X(01).
      *初回番号（入院年月日と同じときだけセット）
           03  RECE68-SHONUM              PIC  9(03).
