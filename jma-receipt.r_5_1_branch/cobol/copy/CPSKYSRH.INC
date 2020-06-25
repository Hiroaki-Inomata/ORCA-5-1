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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  SKYSRH-REC.
      *    医療機関番号
           03  SKYSRH-HOSPNUM          PIC 9(02).
      *    端末ＩＤ
           03  SKYSRH-TERMID           PIC X(64).
      *    患者ＩＤ
           03  SKYSRH-PTID             PIC 9(10).
      *    入外区分
           03  SKYSRH-NYUGAIKBN        PIC X(1).
      *    診療年月
           03  SKYSRH-SRYYM            PIC X(6).
      *    レセプト科（空白の時ゼロをセット）
           03  SKYSRH-RECEKA           PIC  X(02).
      *    審査支払機関
           03  SKYSRH-TEISYUTUSAKI     PIC  9(01).
      *    レセプト種別
           03  SKYSRH-RECESYUBETU      PIC  X(04).
      *    保険者番号
           03  SKYSRH-HKNJANUM         PIC  X(08).
      *    補助区分（船員保険のときのみ）
           03  SKYSRH-HOJOKBN-KEY      PIC  X(01).
      *    公費ＩＤ（医保：公費単独のみ、労災、自賠責：保険ＩＤ）
           03  SKYSRH-KOHID-KEY        PIC  9(10).
      *    適用開始年月日
           03  SKYSRH-TEKSTYMD         PIC  X(08).
      *    分割区分
           03  SKYSRH-RECEIPT-KBN      PIC  X(10).
      *    保険ＩＤ
           03  SKYSRH-HKNID            PIC 9(10).
      *    保険番号
           03  SKYSRH-HKNNUM           PIC X(3).
      *    公費情報
           03  SKYSRH-KOH-G.
               05  SKYSRH-KOH-OCC      OCCURS 4.
      *    公費ＩＤ
                   07  SKYSRH-KOHID    PIC 9(10).
      *    公費番号
                   07  SKYSRH-KOHNUM   PIC X(3).
      *    保険点数
           03  SKYSRH-TOTALTEN         PIC 9(10).
      *    特記事項番号
           03  SKYSRH-TOKKI-G.
               05  SKYSRH-TOKKI        PIC X(2)  OCCURS 5.
      *    特記事項名称
           03  SKYSRH-TOKKINM-G.
               05  SKYSRH-TOKKINM      PIC X(12) OCCURS 5.
      *    記載外公費−薬剤一部負担金情報
           03  SKYSRH-ETC-YKZFTN1      PIC  9(05).
      *    請求区分（レセ電対象）
           03  SKYSRH-SKYKBN-RECEDEN   PIC X(1).
      *    レセプト記載外公費情報作成フラグ（1:記載外公費情報作成）
           03  SKYSRH-ETCCREATEFLG     PIC  9(01).
      *    データ区分(1:請求管理/2:収納）
           03  SKYSRH-DATAKBN          PIC X(1).
      *    連番(請求管理から取得するデータに対して採番）
           03  SKYSRH-SEQNUM           PIC 9(7).
      *    作成日
           03  SKYSRH-CREYMD           PIC X(8).
      *    更新日
           03  SKYSRH-UPYMD            PIC X(8).
      *    更新時間
           03  SKYSRH-UPHMS            PIC X(6).
      *    開始診療年月
           03  SKYSRH-STSRYYM          PIC X(6).
      *    終了診療年月
           03  SKYSRH-EDSRYYM          PIC X(6).
      *    日付＋時刻
           03  SKYSRH-YMDHMS           PIC X(14).
      *    保険点数上限
           03  SKYSRH-TOTALTEN-MAX     PIC 9(10).
