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
      *    レセプト電算マスター   COPY (CPRECEDEN.INC)            *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  ver.2.9.1
      *  02.09.01    NACL-藤原    06/05/08  改定通信医１６０４９号
      *                                     症状詳記レコード対応
      *                                     レセ電データを２５００に変更
      *
      *  03.00.01    NACL-藤原    06/06/15  症状詳記区分の追加
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *                                     調査用にcrehms,updhms 追加
      *  04.06.02    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *                                     receipt_kbn 追加 
      ************************************************************
       01  RECEDEN-REC.
      *ＫＥＹ
           03  RECEDEN-KEY.
      *医療機関ＩＤ
               05  RECEDEN-HOSPNUM           PIC 9(2).
      *診療年月
               05  RECEDEN-SRYYM            PIC  9(06).
      *入外区分
               05  RECEDEN-NYUGAIKBN        PIC  X(01).
      *患者ＩＤ
               05  RECEDEN-PTID             PIC  9(10).
      *レセプト科（空白の時ゼロをセット）
               05  RECEDEN-RECEKA           PIC  X(02).
      *審査支払機関
               05  RECEDEN-TEISYUTUSAKI     PIC  9(01).
      *レセプト種別
               05  RECEDEN-RECESYUBETU      PIC  X(04).
      *保険者番号
               05  RECEDEN-HKNJANUM         PIC  X(08).
      *補助区分（船員保険のときのみ）
               05  RECEDEN-HOJOKBN-KEY      PIC  X(01).
      *公費ＩＤ（公費単独のみ）
               05  RECEDEN-KOHID-KEY        PIC  9(10).
      *適用開始年月日
               05  RECEDEN-TEKSTYMD         PIC  X(08).
      *分割区分
               05  RECEDEN-RECEIPT-KBN      PIC  X(10).
      *レコード区分（１：レセ電、２：症状詳記）
               05  RECEDEN-RECKBN           PIC  X(01).
      *データ連番
               05  RECEDEN-RENNUM           PIC  9(04).
      *作成年月日
           03  RECEDEN-CREYMD               PIC  X(08).
      *作成時間
           03  RECEDEN-CREHMS               PIC  X(06).
      *合計点数
           03  RECEDEN-TOTALTEN             PIC  9(10).
      *症状詳記区分（レコード区分２のみ）
           03  RECEDEN-SJKBN                PIC  X(02).
      *レセ電データ
           03  RECEDEN-RECEDATA             PIC  X(2500).
