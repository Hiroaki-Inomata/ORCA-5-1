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
      *    レセプト明細書０３ COPY (CPRCF03.INC)        *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01.06.29  保険者番号・
      *                                     適用開始年月日の追加
      *  01.00.02    MCC-藤原     01.09.28  医療機関ＩＤの修正
      *  01.00.03    MCC-藤原     01.12.17  病名コードの桁数の修正
      *  01.00.04    MCC-藤原     02.05.14  転帰区分をＫＥＹに追加
      *  01.00.05    MCC-藤原     02.06.11  転帰日をＫＥＹに追加
      *  01.00.06    NACL-藤原    03/03/12  補助区分の追加
      *  01.00.07    NACL-藤原    04/02/01  病名コード２１に変更
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.05.01    NACL-藤原    09/11/25  レセ電コメント拡張対応
      *                                     補足コメントの追加
      *                                     連番号を３桁に変更
      *                                     入力病名、入力病名文字数の追加
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *  04.06.02    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      ************************************************************
      *01  RECE03-REC.
           03  RECE03-KEY.
               05  RECE03-KEY1.
      *医療機関ＩＤ
                   07  RECE03-HOSPNUM     PIC 9(2).
      *診療年月
                   07  RECE03-SRYYM       PIC 9(06).
      *入外区分
                   07  RECE03-NYUGAIKBN   PIC X(01).
      *患者ＩＤ
                   07  RECE03-PTID        PIC 9(10).
      *レセプト科
                   07  RECE03-RECEKA      PIC X(02).
      *審査支払機関
                   07  RECE03-TEISYUTUSAKI
                                          PIC 9(01).
      *レセプト種別
                   07  RECE03-RECESYUBETU PIC 9(04).
      *保険者番号        
                   07  RECE03-HKNJANUM    PIC X(08).
      *補助区分
                   07  RECE03-HOJOKBN     PIC X(01).
      *公費ＩＤ
                   07  RECE03-KOHID        PIC 9(10).
      *適用開始年月日
                   07  RECE03-TEKSTYMD    PIC X(08). 
      *分割区分
                   07  RECE03-RECEIPT-KBN PIC X(10).
      *診療開始日
               05  RECE03-SRYYMD          PIC X(08).
      *転帰区分
               05  RECE03-TENKIKBN        PIC X(01).
      *転帰日
               05  RECE03-TENKIYMD        PIC X(08).
      *病名
               05  RECE03-BYOMEI          PIC X(160).
      *主病名フラグ
               05  RECE03-SYUBYOFLG       PIC X(01).
      *連番号
               05  RECE03-RENNUM          PIC 9(03).
      *患者番号
           03  RECE03-PTNUM               PIC X(20).
      *診療科
           03  RECE03-SRYKA               PIC X(02).
      *基本病名コード
           03  RECE03-KHNBYOMEICD         PIC X(07).
      *病名コード
           03  RECE03-BYOMEICD-G.
               05  RECE03-BYOMEICD-1    PIC X(07).
               05  RECE03-BYOMEICD-2    PIC X(07).
               05  RECE03-BYOMEICD-3    PIC X(07).
               05  RECE03-BYOMEICD-4    PIC X(07).
               05  RECE03-BYOMEICD-5    PIC X(07).
               05  RECE03-BYOMEICD-6    PIC X(07).
               05  RECE03-BYOMEICD-7    PIC X(07).
               05  RECE03-BYOMEICD-8    PIC X(07).
               05  RECE03-BYOMEICD-9    PIC X(07).
               05  RECE03-BYOMEICD-10   PIC X(07).
               05  RECE03-BYOMEICD-11   PIC X(07).
               05  RECE03-BYOMEICD-12   PIC X(07).
               05  RECE03-BYOMEICD-13   PIC X(07).
               05  RECE03-BYOMEICD-14   PIC X(07).
               05  RECE03-BYOMEICD-15   PIC X(07).
               05  RECE03-BYOMEICD-16   PIC X(07).
               05  RECE03-BYOMEICD-17   PIC X(07).
               05  RECE03-BYOMEICD-18   PIC X(07).
               05  RECE03-BYOMEICD-19   PIC X(07).
               05  RECE03-BYOMEICD-20   PIC X(07).
               05  RECE03-BYOMEICD-21   PIC X(07).
      *
           03  RECE03-BYOMEICD-R        REDEFINES   RECE03-BYOMEICD-G.
               05  RECE03-BYOMEICD      PIC X(07)
                                       OCCURS   21.
      *疑いフラグ
           03  RECE03-UTAGAIFLG         PIC X(01).
      *慢性フラグ
           03  RECE03-MANSEIKBN         PIC 9(02).
      *保険組合番号
           03  RECE03-HKNCOMBI          PIC X(04).
      *レセプト表示フラグ
           03  RECE03-REZEFLG           PIC X(01).
      *レセプト表示月数
           03  RECE03-REZEMM            PIC 9(02).
      *病名文字数
           03  RECE03-BYOMEIMOJI        PIC 9(03).
      *補足コメント
           03  RECE03-HOSOKU-COMMENT   PIC X(40).
      *入力病名
           03  RECE03-IN-BYOMEI         PIC X(160).
      *入力病名文字数
           03  RECE03-IN-BYOMEIMOJI     PIC 9(03).
      *病名編集フラグ
           03  RECE03-BYOMEIHENFLG      PIC X(01).
