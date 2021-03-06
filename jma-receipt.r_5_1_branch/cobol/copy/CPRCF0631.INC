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
      *    レセプト明細書（入院） 病名並替え (CPRCF063.INC)      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    04/02/01  病名コード２１に変更
      *  03.05.01    NACL-小豆沢  07/05/30  グループ診療対応
      *
      *  04.05.01    NACL-藤原    09/11/25  レセ電コメント拡張対応
      *                                     補足コメントの追加
      *                                     連番号を３桁に変更
      *                                     入力病名、入力病名文字数の追加
      *
      *  04.06.01    NACL-藤原    10/07/20  レセプト病名記載順対応
      *  04.06.02    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *
      *  04.07.01    NACL-藤原    12/12/17  複数枚レセプト分割対応
      ************************************************************
      *01  RECE631-REC.
           03  RECE631-KEY.
               05  RECE631-KEY1.
      *医療機関ＩＤ
                   07  RECE631-HOSPNUM     PIC 9(2).
      *診療年月
                   07  RECE631-SRYYM       PIC 9(06).
      *入外区分
                   07  RECE631-NYUGAIKBN   PIC X(01).
      *患者ＩＤ
                   07  RECE631-PTID        PIC 9(10).
      *レセプト科
                   07  RECE631-RECEKA      PIC X(02).
      *審査支払機関
                   07  RECE631-TEISYUTUSAKI
                                           PIC 9(01).
      *レセプト種別
                   07  RECE631-RECESYUBETU PIC 9(04).
      *保険者番号        
                   07  RECE631-HKNJANUM    PIC X(08).
      *補助区分
                   07  RECE631-HOJOKBN     PIC X(01).
      *公費ＩＤ
                   07  RECE631-KOHID       PIC 9(10).
      *適用開始年月日
                   07  RECE631-TEKSTYMD    PIC X(08). 
      *分割区分
                   07  RECE631-RECEIPT-KBN PIC X(10).
      *主病名フラグ
               05  RECE631-SYUBYOFLG       PIC X(01).
      *診療開始日
               05  RECE631-SRYYMD          PIC X(08).
      *転帰区分
               05  RECE631-TENKIKBN-X      PIC X(01).
      *転帰日
               05  RECE631-TENKIYMD-X      PIC X(08).
      *連番号
               05  RECE631-RENNUM          PIC 9(03).
      *患者番号
           03  RECE631-PTNUM               PIC  X(20).
      *診療科
           03  RECE631-SRYKA               PIC  X(02).
      *転帰区分
           03  RECE631-TENKIKBN            PIC X(01).
      *転帰日
           03  RECE631-TENKIYMD            PIC X(08).
      *基本病名コード
           03  RECE631-KHNBYOMEICD         PIC  X(07).
      *病名コード
           03  RECE631-BYOMEICD-G.
               05  RECE631-BYOMEICD-1    PIC X(07).
               05  RECE631-BYOMEICD-2    PIC X(07).
               05  RECE631-BYOMEICD-3    PIC X(07).
               05  RECE631-BYOMEICD-4    PIC X(07).
               05  RECE631-BYOMEICD-5    PIC X(07).
               05  RECE631-BYOMEICD-6    PIC X(07).
               05  RECE631-BYOMEICD-7    PIC X(07).
               05  RECE631-BYOMEICD-8    PIC X(07).
               05  RECE631-BYOMEICD-9    PIC X(07).
               05  RECE631-BYOMEICD-10   PIC X(07).
               05  RECE631-BYOMEICD-11   PIC X(07).
               05  RECE631-BYOMEICD-12   PIC X(07).
               05  RECE631-BYOMEICD-13   PIC X(07).
               05  RECE631-BYOMEICD-14   PIC X(07).
               05  RECE631-BYOMEICD-15   PIC X(07).
               05  RECE631-BYOMEICD-16   PIC X(07).
               05  RECE631-BYOMEICD-17   PIC X(07).
               05  RECE631-BYOMEICD-18   PIC X(07).
               05  RECE631-BYOMEICD-19   PIC X(07).
               05  RECE631-BYOMEICD-20   PIC X(07).
               05  RECE631-BYOMEICD-21   PIC X(07).
      *
           03  RECE631-BYOMEICD-R        REDEFINES   RECE631-BYOMEICD-G.
               05  RECE631-BYOMEICD      PIC X(07)
                                       OCCURS   21.
      *疑いフラグ
           03  RECE631-UTAGAIFLG         PIC X(01).
      *慢性フラグ
           03  RECE631-MANSEIKBN         PIC 9(02).
      *保険組合番号
           03  RECE631-HKNCOMBI          PIC X(04).
      *レセプト表示フラグ
           03  RECE631-REZEFLG           PIC X(01).
      *レセプト表示月数
           03  RECE631-REZEMM            PIC 9(02).
      *病名
           03  RECE631-BYOMEI            PIC X(160).
      *病名文字数
           03  RECE631-BYOMEIMOJI        PIC 9(03).
      *補足コメント
           03  RECE631-HOSOKU-COMMENT    PIC X(40).
      *入力病名
           03  RECE631-IN-BYOMEI         PIC X(160).
      *入力病名文字数
           03  RECE631-IN-BYOMEIMOJI     PIC 9(03).
      *病名編集フラグ
           03  RECE631-BYOMEIHENFLG      PIC X(01).
