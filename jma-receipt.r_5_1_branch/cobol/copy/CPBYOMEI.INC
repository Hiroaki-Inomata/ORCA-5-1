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
      *****************************************************************
      *  病名マスター       (CPBYOMEI.INC)  
      *****************************************************************
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01/00/01    MCC-藤原     01.11.09  新しいレイアウトに変更     
      *  02/00/01    NACL-伊藤    02.07.29  新しいレイアウトに変更     
      *  04/06/01    NACL-伊藤    10.10.19  新しいレイアウトに変更     
      *  05/00/00    NACL-伊藤    17.01.06  ICD10(2013)追加     
      *****************************************************************
      *01  BYOMEI-REC.
      *
           03  BYO-KEY.
      *病名コード
               05  BYO-BYOMEICD        PIC X(07).
      *病名
           03  BYO-BYOMEI              PIC X(200).
      *病名文字数
           03  BYO-BYOMEIMOJI          PIC 9(03).
      *省略病名
           03  BYO-TANBYOMEI           PIC X(200).
      *省略病名文字数
           03  BYO-TANBYOMEIMOJI       PIC 9(03).      
      *病名カナ
           03  BYO-BYOMEIKANA          PIC X(200).
      *移行先コード
           03  BYO-IKOSAKICD           PIC X(07).
      *特定疾患コード
           03  BYO-TOKSKNCD            PIC 9(02).
      *難病外来コード
           03  BYO-NANBYOCD            PIC 9(02).
      *疑いフラグ
           03  BYO-UTAGAIFLG           PIC X(01).
      *単独使用禁止区分
           03  BYO-TANDOKUKBN          PIC 9(02).
      *保険請求外区分
           03  BYO-HKNSKYKBN           PIC 9(01).
      *病名管理区分
           03  BYO-HYOJUNCD            PIC X(08).
      *採択区分
           03  BYO-SAITAKUKBN          PIC X(01).
      *病名交換用コード
           03  BYO-BYOMEICHGCD         PIC X(04).
      *ＩＣＤ１０ー１
           03  BYO-ICD10-1             PIC X(05).
      *ＩＣＤ１０ー２
           03  BYO-ICD10-2             PIC X(05).
      *ＩＣＤ１０ー１（２０１３）
           03  BYO-ICD10-1-2           PIC X(05).
      *ＩＣＤ１０ー２（２０１３）
           03  BYO-ICD10-2-2           PIC X(05).
      *収載年月日
           03  BYO-SYUSAIYMD           PIC X(08).
      *変更年月日
           03  BYO-CHGYMD              PIC X(08).
      *廃止年月日
           03  BYO-HAISIYMD            PIC X(08).
      *端末ＩＤ
           03  BYO-TERMID              PIC X(32).
      *オペーレータＩＤ
           03  BYO-OPID                PIC X(16).
      *作成年月日
           03  BYO-CREYMD              PIC X(08).
      *更新年月日
           03  BYO-UPYMD               PIC X(08).
      *更新時間
           03  BYO-UPHMS               PIC X(06).
