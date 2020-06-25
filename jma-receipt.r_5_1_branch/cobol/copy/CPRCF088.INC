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
      *    労災レセプト明細書（入院）                            *
      *                     入退院履歴情報   (CPRCF088.INC)      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-門脇    07.06.01  グループ診療対応
      *
      *  04.07.01    NACL-藤原    15/01/24  同日再入院時の入退院履歴の記録
      *                                     を修正
      *                                    （入院日と退院日が同じとき）
      ************************************************************
      *01  RECE88-REC.
           03  RECE88-KEY.
               05  RECE88-KEY1.
      *医療機関ＩＤ
                   07  RECE88-HOSPNUM      PIC 9(02).
      *診療年月
                   07  RECE88-SRYYM        PIC 9(06).
      *入外区分
                   07  RECE88-NYUGAIKBN    PIC X(01).
      *患者ＩＤ
                   07  RECE88-PTID         PIC 9(10).
      *レセプト科
                   07  RECE88-RECEKA       PIC X(02).
      *保険ＩＤ        
                   07  RECE88-HKNID        PIC 9(10).
      *適用開始年月日
                   07  RECE88-TEKSTYMD     PIC X(08).
      *入院年月日
               05  RECE88-NYUINYMD         PIC X(08).
      *入院年月日連番
               05  RECE88-NYUINYMD-SEQNO   PIC 9(01).
      *退院年月日
           03  RECE88-TAIINYMD             PIC X(08).
      *自院歴−入院中区分
           03  RECE88-NYUINCHUKBN          PIC X(01).
      *初回番号（入院年月日と同じときだけセット）
           03  RECE88-SHONUM               PIC 9(03).
