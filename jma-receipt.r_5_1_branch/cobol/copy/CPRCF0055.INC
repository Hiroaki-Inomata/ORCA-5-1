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
      *    レセプト明細書     COPY (CPRCF0055.INC)               *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *  04.06.02    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *
      *  05.00.01    NACL-門脇    20/03/30  令和２年４月改正対応
      ************************************************************
      *01  RECE055-REC.
           03  RECE055-KEY.
             05  RECE055-KEY1.
      *医療機関ＩＤ
               07  RECE055-HOSPNUM         PIC 9(2).
      *診療年月
               07  RECE055-SRYYM           PIC 9(06).
      *入外区分
               07  RECE055-NYUGAIKBN       PIC X(01).
      *患者ＩＤ
               07  RECE055-PTID            PIC 9(10).
      *レセプト科
               07  RECE055-RECEKA          PIC X(02).
      *審査支払機関
               07  RECE055-TEISYUTUSAKI    PIC 9(01).
      *レセプト種別
               07  RECE055-RECESYUBETU     PIC 9(04).
      *保険者番号        
               07  RECE055-HKNJANUM        PIC X(08).
      *補助区分
               07  RECE055-HOJOKBN         PIC X(01).
      *公費ＩＤ
               07  RECE055-KOHID           PIC 9(10).
      *適用開始年月日
               07  RECE055-TEKSTYMD        PIC X(08). 
      *分割区分
               07  RECE055-RECEIPT-KBN     PIC X(10).
      *剤番号
               07  RECE055-ZAINUM          PIC 9(08).
      *連番号
               07  RECE055-RENNUM          PIC 9(02).
      *
             05  RECE055-SHORI-RENNUM      PIC 9(01).
      *
             05  RECE055-SEQNO             PIC 9(02).
      *コメント情報
           03  RECE055-SRYACTINF.
      *診療コード
               05  RECE055-SRYCD           PIC X(09).
      *漢字名称
               05  RECE055-NAME            PIC X(300).
      *入力コメント
               05  RECE055-INPUTCOMENT     PIC X(140).
