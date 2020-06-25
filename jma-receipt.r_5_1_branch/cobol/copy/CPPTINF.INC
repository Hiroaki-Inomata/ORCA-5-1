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
      *    患者                           COPY (CPPTINF.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  05/09/13  入金方法追加
      ************************************************************
      *01  PTINF-REC.
      *キー
           03  PTINF-KEY.
      *医療機関ＩＤ
               05  PTINF-HOSPNUM              PIC 9(2).
      *患者ＩＤ
               05  PTINF-PTID                PIC  9(10).
      *カナ氏名
           03  PTINF-KANANAME                PIC  X(100).
      *漢字氏名
           03  PTINF-NAME                    PIC  X(100).
      *通称
           03  PTINF-NICKNAME                PIC  X(100).
      *性別
           03  PTINF-SEX                     PIC  X(01).
      *生年月日
           03  PTINF-BIRTHDAY                PIC  X(08).
      *死亡区分
           03  PTINF-DEATHKBN                PIC  X(01).
      *自宅−郵便番号
           03  PTINF-HOME-POST               PIC  X(07).
      *自宅−住所
           03  PTINF-HOME-ADRS               PIC  X(200).
      *自宅−番地方書
           03  PTINF-HOME-BANTI              PIC  X(200).
      *自宅−昼電話番号
           03  PTINF-HOME-TEL1               PIC  X(015).
      *自宅−夜電話番号
           03  PTINF-HOME-TEL2               PIC  X(015).
      *世帯主名
           03  PTINF-SETAINUSI               PIC  X(100).
      *続柄
           03  PTINF-ZOKUGARA                PIC  X(060).
      *職業
           03  PTINF-JOB                     PIC  X(040).
      *携帯電話番号
           03  PTINF-KEITAI-TEL              PIC  X(015).
      *ＦＡＸ番号
           03  PTINF-FAX                     PIC  X(015).
      *電子メールアドレス
           03  PTINF-EMAIL                   PIC  X(050).
      *病院減免事由区分
           03  PTINF-DISCOUNT-KBN            PIC  X(02).
      *割引率
           03  PTINF-DISCOUNT                PIC  X(02).
      *状態区分１
           03  PTINF-CONDITION-1             PIC  X(02).
      *状態区分２
           03  PTINF-CONDITION-2             PIC  X(02).
      *状態区分３
           03  PTINF-CONDITION-3             PIC  X(02).
      *コメント１
           03  PTINF-COMMENT-1               PIC  X(200).
      *コメント２
           03  PTINF-COMMENT-2               PIC  X(200).
      *禁忌１
           03  PTINF-TABOO-1                 PIC  X(200).
      *禁忌２
           03  PTINF-TABOO-2                 PIC  X(200).
      *アレルギー１
           03  PTINF-ALLERGY-1               PIC  X(200).
      *アレルギー２
           03  PTINF-ALLERGY-2               PIC  X(200).
      *感染症１
           03  PTINF-KANSENSYO-1             PIC  X(200).
      *感染症２
           03  PTINF-KANSENSYO-2             PIC  X(200).
      *連絡先−名称
           03  PTINF-RENRAKU-NAME            PIC  X(100).
      *連絡先−続柄
           03  PTINF-RENRAKU-ZOKUGARA        PIC  X(60).
      *連絡先−郵便番号
           03  PTINF-RENRAKU-POST            PIC  X(07).
      *連絡先−住所
           03  PTINF-RENRAKU-ADRS            PIC  X(200).
      *連絡先−番地方書
           03  PTINF-RENRAKU-BANTI           PIC  X(200).
      *連絡先−昼電話番号
           03  PTINF-RENRAKU-TEL1            PIC  X(015).
      *連絡先−夜電話番号
           03  PTINF-RENRAKU-TEL2            PIC  X(015).
      *勤務先−名称
           03  PTINF-OFFICE-NAME             PIC  X(100).
      *勤務先−郵便番号
           03  PTINF-OFFICE-POST             PIC  X(07).
      *勤務先−住所
           03  PTINF-OFFICE-ADRS             PIC  X(200).
      *勤務先−番地方書
           03  PTINF-OFFICE-BANTI            PIC  X(200).
      *勤務先−電話番号
           03  PTINF-OFFICE-TEL              PIC  X(015).
      *帰省先−名称
           03  PTINF-KISEI-NAME              PIC  X(100).
      *帰省先−郵便番号
           03  PTINF-KISEI-POST              PIC  X(07).
      *帰省先−住所
           03  PTINF-KISEI-ADRS              PIC  X(200).
      *帰省先−番地方書
           03  PTINF-KISEI-BANTI             PIC  X(200).
      *帰省先−電話番号
           03  PTINF-KISEI-TEL               PIC  X(015).
      *テスト患者番号区分
           03  PTINF-TSTPTNUMKBN             PIC  X(01).
      *入金方法
           03  PTINF-NYUKIN-HOHO             PIC  X(02).
      *最大剤番号
           03  PTINF-MAXZAINUM               PIC  9(08).
      *入院履歴番号
           03  PTINF-RRKNUM                  PIC  9(03).
      *入院履歴枝番号
           03  PTINF-RRKEDANUM               PIC  9(03).
      *請求書発行区分
           03  PTINF-SKYPRTFLG               PIC  X(01).
      *端末ＩＤ
           03  PTINF-TERMID                  PIC  X(16).
      *オペーレータＩＤ
           03  PTINF-OPID                    PIC  X(16).
      *作成年月日
           03  PTINF-CREYMD                  PIC  X(08).
      *更新年月日
           03  PTINF-UPYMD                   PIC  X(08).
      *更新時間
           03  PTINF-UPHMS                   PIC  X(06).
      *virtual項目
           03  PTINF-ST-SRYYMD               PIC  X(08).
           03  PTINF-ED-SRYYMD               PIC  X(08).
           03  PTINF-NYUGAIKBN               PIC  X(01).
           03  PTINF-HKNKBN                  PIC  X(01).
           03  PTINF-SRYYMD                  PIC  X(08).
           03  PTINF-SRYYM                   PIC  X(06).
           03  PTINF-HKNKBN2                 PIC  X(01).
           03  PTINF-VPTNUM                  PIC  X(20).
