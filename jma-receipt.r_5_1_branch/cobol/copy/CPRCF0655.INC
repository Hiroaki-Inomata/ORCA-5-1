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
      *    レセプト明細書     COPY (CPRCF0655.INC)               *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *
      *  04.07.01    NACL-藤原    12/12/17  複数枚レセプト分割対応
      *
      *  05.00.01    NACL-門脇    20/03/30  令和２年４月改正対応
      ************************************************************
      *01  RECE655-REC.
           03  RECE655-KEY.
             05  RECE655-KEY1.
      *医療機関ＩＤ
               07  RECE655-HOSPNUM         PIC 9(2).
      *診療年月
               07  RECE655-SRYYM           PIC 9(06).
      *入外区分
               07  RECE655-NYUGAIKBN       PIC X(01).
      *患者ＩＤ
               07  RECE655-PTID            PIC 9(10).
      *レセプト科
               07  RECE655-RECEKA          PIC X(02).
      *審査支払機関
               07  RECE655-TEISYUTUSAKI    PIC 9(01).
      *レセプト種別
               07  RECE655-RECESYUBETU     PIC 9(04).
      *保険者番号        
               07  RECE655-HKNJANUM        PIC X(08).
      *補助区分
               07  RECE655-HOJOKBN         PIC X(01).
      *公費ＩＤ
               07  RECE655-KOHID           PIC 9(10).
      *適用開始年月日
               07  RECE655-TEKSTYMD        PIC X(08).
      *分割区分
               07  RECE655-RECEIPT-KBN     PIC X(10).
      *剤番号
               07  RECE655-ZAINUM          PIC 9(08).
      *剤連番
               07  RECE655-ZAIREN          PIC 9(02).
      *連番号
               07  RECE655-RENNUM          PIC 9(02).
      *
             05  RECE655-SHORI-RENNUM      PIC 9(01).
      *
             05  RECE655-SEQNO             PIC 9(02).
      *コメント情報
           03  RECE655-SRYACTINF.
      *診療コード
               05  RECE655-SRYCD           PIC X(09).
      *漢字名称
               05  RECE655-NAME            PIC X(300).
      *入力コメント
               05  RECE655-INPUTCOMENT     PIC X(140).
