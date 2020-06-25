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
      *    自賠責ＰＤＦ用         COPY (CPJIBAI-PRT.INC)         *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.01    NACL-藤原    17/04/10  患者単位での点数算定設定
      *                                     （労災準拠又は健保準拠）対応
      *                                     保険区分の追加
      ************************************************************
      *01  JIBAI-PRT-REC.
      *ＫＥＹ
           03  JIBAI-PRT-KEY.
               05  JIBAI-PRT-KEY1.
      *医療機関ＩＤ
                   07  JIBAI-PRT-HOSPNUM   PIC 9(2).
      *診療年月
                   07  JIBAI-PRT-SRYYM     PIC 9(06).
      *入外区分
                   07  JIBAI-PRT-NYUGAIKBN PIC X(01).
      *患者ＩＤ
                   07  JIBAI-PRT-PTID      PIC 9(10).
      *レセプト科
                   07  JIBAI-PRT-RECEKA    PIC X(02).
      *審査支払機関
                   07  JIBAI-PRT-TEISYUTUSAKI
                                           PIC 9(01).
      *レセプト種別
                   07  JIBAI-PRT-RECESYUBETU
                                           PIC X(04).
      *保険者番号
                   07  JIBAI-PRT-HKNJANUM  PIC X(08).
      *補助区分
                   07  JIBAI-PRT-HOJOKBN-KEY
                                           PIC X(01).
      *公費ＩＤ（公費単独のみ）
                   07  JIBAI-PRT-KOHID-KEY PIC 9(10).
      *適用開始年月日
                   07  JIBAI-PRT-TEKSTYMD  PIC X(08).
      *分割区分
                   07  JIBAI-PRT-RECEIPT-KBN
                                           PIC X(10).
      *データ連番
               05  JIBAI-PRT-RENNUM        PIC 9(03).
      *患者番号
           03  JIBAI-PRT-PTNUM             PIC X(20).
      *保険番号
           03  JIBAI-PRT-HKNNUM            PIC X(03).
      *様式区分（４：新様式、５：従来様式、６：第三者行為）
           03  JIBAI-PRT-RECEKBN           PIC X(01).
      *保険区分（５：自賠責（労災準拠）、６：自賠責（健保準拠））
           03  JIBAI-PRT-HKNKBN            PIC X(01).
      *帳票区分
           03  JIBAI-PRT-PRTKBN            PIC X(03).
      *作成年月日
           03  JIBAI-PRT-CREYMD            PIC X(08).
      *作成時間
           03  JIBAI-PRT-CREHMS            PIC X(06).
      *帳票ＩＤ
           03  JIBAI-PRT-FORMID            PIC X(20).
      *帳票データ
           03  JIBAI-PRT-PRTDATA           PIC X(10000).
