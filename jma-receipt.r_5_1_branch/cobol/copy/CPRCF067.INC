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
      *    レセプト明細書（入院） 県単用項目ファイル             *
      *                               COPY (CPRCF067.INC)        *
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  00.00.01    NACL-藤原    04/04/07  コメント欄（上部）追加
      *  01.00.02    NACL-藤原    04/08/23  特記事項（レセ電用）の追加
      *
      *  02.06.01    NACL-藤原    05/09/12  一部負担金（レセ電用）追加
      *  03.05.01    NACL-小豆沢  07/05/30  グループ診療対応
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *
      *  04.07.01    NACL-藤原    12/12/17  複数枚レセプト分割対応
      ************************************************************
      *01  RECE67-REC.
           03  RECE67-KEY.
      *医療機関ＩＤ
               05  RECE67-HOSPNUM         PIC  9(2).
      *診療年月
               05  RECE67-SRYYM           PIC  9(06).
      *入外区分
               05  RECE67-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  RECE67-PTID            PIC  9(10).
      *レセプト科
               05  RECE67-RECEKA          PIC  X(02).
      *審査支払機関
               05  RECE67-TEISYUTUSAKI    PIC  9(01).
      *レセプト種別
               05  RECE67-RECESYUBETU     PIC  9(04).
      *保険者番号        
               05  RECE67-HKNJANUM        PIC  X(08).
      *補助区分
               05  RECE67-HOJOKBN         PIC  X(01).
      *公費ＩＤ
               05  RECE67-KOHID           PIC 9(10).
      *適用開始年月日
               05  RECE67-TEKSTYMD        PIC  X(08). 
      *分割区分
               05  RECE67-RECEIPT-KBN     PIC  X(10).
      *主保険−給付割合
           03  RECE67-KYURATE             PIC  9(03).
      *療養の給付情報
           03  RECE67-TOTALTENINF         OCCURS  5.
      *合計点数
               05  RECE67-TOTALTEN        PIC  9(08).
      *負担金額
               05  RECE67-FTNMONEY        PIC  9(09).
      *実日数
               05  RECE67-JNISSU          PIC  9(02).
      *食事療養費情報
           03  RECE67-SHOKUJIINF          OCCURS  5.
      *食事日数
               05  RECE67-SHOKUJINISSU    PIC  9(02).
      *食事療養費
               05  RECE67-SHOKUJIRYOYOHI  PIC  9(09).
      *標準負担額
               05  RECE67-SHOKUJIFTN      PIC  9(09).
      *公費情報
           03  RECE67-KOHINF              OCCURS  4.
      *公費−負担者番号
               05  RECE67-KOHFTNJANUM     PIC  X(08).
      *公費−受給者番号
               05  RECE67-KOHJKYSNUM      PIC  X(20).
      *特記事項情報
           03  RECE67-TOKKIINF            OCCURS  5.
               05  RECE67-TOKKI           PIC  X(02).
      *療養の給付情報（レセ電用）
           03  RECE67-RECEDEN-TOTALTENINF OCCURS  5.
      *負担金額
               05  RECE67-RECEDEN-FTNMONEY
                                          PIC  9(09).
      *コメント欄（上部）
           03  RECE67-HEAD4-G.
               05  RECE67-HEAD4           PIC  X(62)   OCCURS  2.
      *帳票データ
           03  RECE67-PRTDATA             PIC  X(4500).
