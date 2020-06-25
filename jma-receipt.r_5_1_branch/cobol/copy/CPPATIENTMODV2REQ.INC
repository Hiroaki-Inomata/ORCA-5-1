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
      * 05.00.00     NACL-多々納  20/04/XX  被保険者証枝番追加
      *****************************************************************
      ******************************************************************
      *    ＡＰＩ患者登録情報（基本情報・保険・公費）(xml2)
      *                                   COPY (CPPATIENTMODV2REQ.INC)
      ******************************************************************
        01  XML-PATIENTMODREQ.
            02  PTMODREQ-CONTEXT           PIC S9(9)   BINARY.
            02  PTMODREQ-OBJECT            PIC X(36).
            02  PTMODREQ-MODE              PIC S9(9)   BINARY.
            02  PTMODREQ-RECORDNAME        PIC X(128).
            02  PTMODREQ-MEDICALREQ.
      *       処理日付
      *       03  PTMODREQ-INFORMATION-DATE    PIC X(10).
      *       03  PTMODREQ-INFORMATION-TIME    PIC X(8).
      *       処理区分
              03  PTMODREQ-MOD-KEY             PIC X(01).
      *       患者番号
              03  PTMODREQ-PATIENTID           PIC X(20).
      *       患者情報
              03  PTMODREQ-PTINF-INFO.
      *         患者氏名
                05  PTMODREQ-NAME             PIC X(100).
      *         患者カナ氏名
                05  PTMODREQ-KANANAME         PIC X(100).
      *         生年月日
                05  PTMODREQ-BIRTHDAY         PIC X(10).
      *         性別
                05  PTMODREQ-SEX              PIC X(01).
      *         世帯主名
                05  PTMODREQ-SETAI-NAME       PIC X(100).
      *         続柄
                05  PTMODREQ-ZOKUGARA         PIC X(60).
      *         職業
                05  PTMODREQ-JOB              PIC X(40).
      *         携帯電話番号
                05  PTMODREQ-KEITAI-TEL       PIC  X(015).
      *         ＦＡＸ番号
                05  PTMODREQ-FAX              PIC  X(015).
      *         電子メールアドレス
                05  PTMODREQ-EMAIL            PIC  X(050).
      *         自宅情報
                05  PTMODREQ-ADDRESS-INFO.
      *           郵便番号
                  07  PTMODREQ-HOME-ZIP-CODE     PIC X(7).
      *           住所
                  07  PTMODREQ-HOME-ADDRESSES1   PIC X(200).
      *           番地番号
                  07  PTMODREQ-HOME-ADDRESSES2   PIC X(200).
      *           電話番号（自宅）
                  07  PTMODREQ-HOME-HPONE1       PIC X(15).
      *           電話番号（連絡先）
                  07  PTMODREQ-HOME-HPONE2       PIC X(15).
      *         勤務先情報
                05  PTMODREQ-OFFICE-INFO.
      *           勤務先名
                  07  PTMODREQ-OFFICE-NAME        PIC X(100).
      *           郵便番号
                  07  PTMODREQ-OFFICE-ZIP-CODE    PIC X(7).
      *           住所
                  07  PTMODREQ-OFFICE-ADDRESSES1  PIC X(200).
      *           番地番号
                  07  PTMODREQ-OFFICE-ADDRESSES2  PIC X(200).
      *           電話番号
                  07  PTMODREQ-OFFICE-HPONE       PIC X(15).
      *         禁忌１
                05  PTMODREQ-TABOO-1              PIC  X(200).
      *         禁忌２
                05  PTMODREQ-TABOO-2              PIC  X(200).
      *         アレルギー１
                05  PTMODREQ-ALLERGY-1            PIC  X(200).
      *         アレルギー２
                05  PTMODREQ-ALLERGY-2            PIC  X(200).
      *         感染症１
                05  PTMODREQ-KANSENSYO-1          PIC  X(200).
      *         感染症２
                05  PTMODREQ-KANSENSYO-2          PIC  X(200).
      *         コメント１
                05  PTMODREQ-COMMENT-1            PIC  X(200).
      *         コメント２
                05  PTMODREQ-COMMENT-2            PIC  X(200).
      *
      *         保険情報
                05  PTMODREQ-INSURANCE-INFO-G.
      *             保険の種類
                    07  PTMODREQ-MAIN-INSURANCE-CLASS    PIC X(03).
      *             保険者番号
                    07  PTMODREQ-MAIN-INSURANCE-NUMBER   PIC X(08).
      *             保険の種類名称
                    07  PTMODREQ-MAIN-INSURANCE-NAME     PIC X(20).
      *             記号
                    07  PTMODREQ-MAIN-MARK               PIC X(80).
      *             番号
                    07  PTMODREQ-MAIN-NUMBER             PIC X(80).
      *R02.4        枝番
                    07  PTMODREQ-MAIN-EDABAN             PIC X(02).
      *             継続区分
                    07  PTMODREQ-MAIN-CONTINUATION       PIC X(01).
      *             補助区分
                    07  PTMODREQ-MAIN-ASSISTANCE         PIC X(01).
      *             本人家族区分
                    07  PTMODREQ-MAIN-FAMILY             PIC X(01).
      *             高齢者負担区分
                    07  PTMODREQ-RATE-CLASS              PIC X(01).
      *             被保険者名
                    07  PTMODREQ-MAIN-POLICYHOLDER       PIC X(100).
      *             有効開始日
                    07  PTMODREQ-MAIN-START-DATE         PIC X(10).
      *             有効終了日
                    07  PTMODREQ-MAIN-END-DATE           PIC X(10).
      *             資格取得日
                    07  PTMODREQ-MAIN-GET-DATE           PIC X(10).
      *           公費情報
                  05  PTMODREQ-PUBLIC-INSURANCE-INF  OCCURS  4 TIMES.
      *             公費の種類
                    07  PTMODREQ-INSURANCE-CLASS       PIC X(03).
      *             公費の種類名称
                    07  PTMODREQ-INSURANCE-NAME        PIC X(20).
      *             負担者番号
                    07  PTMODREQ-PROVIDER-NUMBER       PIC X(08).
      *             受給者番号
                    07  PTMODREQ-RECIPIENT-NUMBER      PIC X(20).
      *             有効開始日
                    07  PTMODREQ-INSURANCE-START-DATE  PIC X(10).
      *             有効終了日
                    07  PTMODREQ-INSURANCE-END-DATE    PIC X(10).
      *H30.4
      *         患者個別情報
                05  PTMODREQ-PTCONF-INFO.
      *             妊婦設定
                    07  PTMODREQ-NINPUKBN             PIC X(05).
      *R01.6
                05  PTMODREQ-DICOUN-INFO.
      *             減免事由
                    07  PTMODREQ-DISCOUNTKBN          PIC  X(02).
      *             割引率
                    07  PTMODREQ-DISCOUNT             PIC  X(02).
      *             支払区分（入金方法）
                    07  PTMODREQ-IC-CODE              PIC  X(02).
