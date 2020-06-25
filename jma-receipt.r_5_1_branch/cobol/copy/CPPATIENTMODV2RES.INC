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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      ******************************************************************
      *    ＡＰＩ患者登録情報（基本情報・保険・公費）
      *                                   COPY (CPPATIENTMODV2RES.INC)
      ******************************************************************
        01  XML-PATIENTMODRES.
            02  PTMODRES-CONTEXT           PIC S9(9)   BINARY.
            02  PTMODRES-OBJECT            PIC X(36).
            02  PTMODRES-MODE              PIC S9(9)   BINARY.
            02  PTMODRES-RECORDNAME        PIC X(128).
            02  PTMODRES-APPOINTRES.
      *       実施日
              03  PTMODRES-INFORMATION-DATE    PIC X(10).
      *       実施時間
              03  PTMODRES-INFORMATION-TIME    PIC X(8).
      *       返却コード
              03  PTMODRES-API-RESULT          PIC X(2).
      *       返却メッセージ
              03  PTMODRES-API-RESULT-MSG      PIC X(100).
      *       ワーニングメッセージ
              03  PTMODRES-API-WARNING-MSG-G.
                  05  PTMODRES-API-WARNING-MSG     PIC X(100)
                                                   OCCURS   5.
      *
              03  PTMODRES-RESKEY              PIC X(100).
      *       患者番号
              03  PTMODRES-PATIENTID           PIC X(20).
      *       患者情報
              03  PTMODRES-PTINF-INFO.
      *         患者氏名
                05  PTMODRES-NAME             PIC X(100).
      *         患者カナ氏名
                05  PTMODRES-KANANAME         PIC X(100).
      *         生年月日
                05  PTMODRES-BIRTHDAY         PIC X(10).
      *         性別
                05  PTMODRES-SEX              PIC X(01).
      *         世帯主名
                05  PTMODRES-SETAI-NAME       PIC X(100).
      *         続柄
                05  PTMODRES-ZOKUGARA         PIC X(60).
      *         職業
                05  PTMODRES-JOB              PIC X(40).
      *         携帯電話番号
                05  PTMODRES-KEITAI-TEL       PIC  X(015).
      *         ＦＡＸ番号
                05  PTMODRES-FAX              PIC  X(015).
      *         電子メールアドレス
                05  PTMODRES-EMAIL            PIC  X(050).
      *         自宅情報
                05  PTMODRES-ADDRESS-INFO.
      *           郵便番号
                  07  PTMODRES-HOME-ZIP-CODE     PIC X(7).
      *           住所
                  07  PTMODRES-HOME-ADDRESSES1   PIC X(200).
      *           番地番号
                  07  PTMODRES-HOME-ADDRESSES2   PIC X(200).
      *           電話番号（自宅）
                  07  PTMODRES-HOME-HPONE1       PIC X(15).
      *           電話番号（連絡先）
                  07  PTMODRES-HOME-HPONE2       PIC X(15).
      *         勤務先情報
                05  PTMODRES-OFFICE-INFO.
      *           勤務先名
                  07  PTMODRES-OFFICE-NAME        PIC X(100).
      *           郵便番号
                  07  PTMODRES-OFFICE-ZIP-CODE    PIC X(7).
      *           住所
                  07  PTMODRES-OFFICE-ADDRESSES1  PIC X(200).
      *           番地番号
                  07  PTMODRES-OFFICE-ADDRESSES2  PIC X(200).
      *           電話番号
                  07  PTMODRES-OFFICE-HPONE        PIC X(15).
      *         禁忌１
                05  PTMODRES-TABOO-1              PIC  X(200).
      *         禁忌２
                05  PTMODRES-TABOO-2              PIC  X(200).
      *         アレルギー１
                05  PTMODRES-ALLERGY-1            PIC  X(200).
      *         アレルギー２
                05  PTMODRES-ALLERGY-2            PIC  X(200).
      *         感染症１
                05  PTMODRES-KANSENSYO-1          PIC  X(200).
      *         感染症２
                05  PTMODRES-KANSENSYO-2          PIC  X(200).
      *         コメント１
                05  PTMODRES-COMMENT-1            PIC  X(200).
      *         コメント２
                05  PTMODRES-COMMENT-2            PIC  X(200).
      *
      *         保険情報
                05  PTMODRES-INSURANCE-INFO-G.
      *           保険情報（保険組合せ）
                  06  PTMODRES-INSURANCE-INFO     OCCURS  20 TIMES.
      *               保険組合せ番号
                      07  PTMODRES-COMBINATION-NUMBER     PIC X(04).
      *               保険の種類
                      07  PTMODRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *               保険者番号
                      07  PTMODRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *               保険の種類名称
                      07  PTMODRES-MAIN-INSURANCE-NAME    PIC X(20).
      *               記号
                      07  PTMODRES-MAIN-MARK              PIC X(80).
      *               番号
                      07  PTMODRES-MAIN-NUMBER            PIC X(80).
      *R02.4          枝番
                      07  PTMODRES-MAIN-EDABAN            PIC X(02).
      *               継続区分
                      07  PTMODRES-MAIN-CONTINUATION      PIC X(01).
      *               補助区分
                      07  PTMODRES-MAIN-ASSISTANCE        PIC X(01).
      *               本人家族区分
                      07  PTMODRES-MAIN-FAMILY            PIC X(01).
      *               被保険者名
                      07  PTMODRES-MAIN-POLICYHOLDER      PIC X(100).
      *               有効開始日
                      07  PTMODRES-MAIN-START-DATE        PIC X(10).
      *               有効終了日
                      07  PTMODRES-MAIN-END-DATE          PIC X(10).
      *               資格取得日
                      07  PTMODRES-MAIN-GET-DATE          PIC X(10).
      *               公費情報
                      07  PTMODRES-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *                 公費の種類
                        09  PTMODRES-INSURANCE-CLASS         PIC X(03).
      *                 公費の種類名称
                        09  PTMODRES-INSURANCE-NAME          PIC X(20).
      *                 負担者番号
                        09  PTMODRES-PROVIDER-NUMBER         PIC X(08).
      *                 受給者番号
                        09  PTMODRES-RECIPIENT-NUMBER        PIC X(20).
      *                 入院−負担率（割）
                        09  PTMODRES-RATE-ADMISSION          PIC X(04).
      *                 入院−固定額
                        09  PTMODRES-MONEY-ADMISSION         PIC X(06).
      *                 外来−負担率（割）
                        09  PTMODRES-RATE-OUTPATIENT         PIC X(04).
      *                 外来−固定額
                        09  PTMODRES-MONEY-OUTPATIENT        PIC X(06).
      *                 有効開始日
                        09  PTMODRES-INSURANCE-START-DATE    PIC X(10).
      *                 有効終了日
                        09  PTMODRES-INSURANCE-END-DATE      PIC X(10).
      *H30.4
      *         患者個別情報
                05  PTMODRES-PTCONF-INFO.
      *             妊婦設定
                    07  PTMODRES-NINPUKBN             PIC X(05).
      *R01.6
                05  PTMODRES-DICOUN-INFO.
      *             減免事由
                    07  PTMODRES-DISCOUNTKBN          PIC  X(02).
                    07  PTMODRES-DISCOUNTKBN-NAME     PIC  X(20).
      *             割引率
                    07  PTMODRES-DISCOUNT             PIC  X(02).
                    07  PTMODRES-DISCOUNT-NAME        PIC  X(14).
      *             支払区分（入金方法）
                    07  PTMODRES-IC-CODE              PIC  X(02).
                    07  PTMODRES-IC-CODE-NAME         PIC  X(20).
      *
