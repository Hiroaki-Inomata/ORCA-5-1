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
      *    レセプト明細書０３１ COPY (CPRCF031.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    04/02/01  病名コード２１に変更
      *  03.05.01    NACL-門脇    07/05/28  グループ診療対応
      *
      *  04.07.01    NACL-藤原    13/01/21  労災レセ電対応
      ************************************************************
      *01  RECE43-REC.
           03  RECE43-KEY.
               05  RECE43-KEY1.
      *医療機関ＩＤ
                   07  RECE43-HOSPNUM     PIC 9(02).
      *診療年月
                   07  RECE43-SRYYM       PIC 9(06).
      *入外区分
                   07  RECE43-NYUGAIKBN   PIC X(01).
      *患者ＩＤ
                   07  RECE43-PTID        PIC 9(10).
      *レセプト科
                   07  RECE43-RECEKA      PIC X(02).
      *保険ＩＤ        
                   07  RECE43-HKNID       PIC 9(10).
      *適用開始年月日
                   07  RECE43-TEKSTYMD    PIC X(08). 
      *主病名フラグ
               05  RECE43-SYUBYOFLG       PIC X(01).
      *診療開始日
               05  RECE43-SRYYMD          PIC X(08).
      *転帰区分
               05  RECE43-TENKIKBN        PIC X(01).
      *転帰日
               05  RECE43-TENKIYMD        PIC X(08).
      *連番号
               05  RECE43-RENNUM          PIC 9(03).
      *患者番号
           03  RECE43-PTNUM               PIC  X(20).
      *診療科
           03  RECE43-SRYKA               PIC  X(02).
      *基本病名コード
           03  RECE43-KHNBYOMEICD         PIC  X(07).
      *病名コード
           03  RECE43-BYOMEICD-G.
               05  RECE43-BYOMEICD      PIC X(07)
                                        OCCURS   21.
      *疑いフラグ
           03  RECE43-UTAGAIFLG         PIC X(01).
      *慢性フラグ
           03  RECE43-MANSEIKBN         PIC 9(02).
      *保険組合番号
           03  RECE43-HKNCOMBI          PIC X(04).
      *レセプト表示フラグ
           03  RECE43-REZEFLG           PIC X(01).
      *レセプト表示月数
           03  RECE43-REZEMM            PIC 9(02).
      *病名
           03  RECE43-BYOMEI            PIC X(160).
      *病名文字数
           03  RECE43-BYOMEIMOJI        PIC 9(03).
      *補足コメント
           03  RECE43-HOSOKU-COMMENT    PIC X(40).
      *入力病名
           03  RECE43-IN-BYOMEI         PIC X(160).
      *入力病名文字数
           03  RECE43-IN-BYOMEIMOJI     PIC 9(03).
      *病名編集フラグ
           03  RECE43-BYOMEIHENFLG      PIC X(01).
      *レセプト種別
           03  RECE43-RECESYUBETU       PIC  9(04).
