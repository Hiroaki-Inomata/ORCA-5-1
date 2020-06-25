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
      *    ＡＰＩ患者登録情報（介護情報）(一体化　V3)
      *                                   COPY (CPPATIENTMODV3REQ6.INC)
      ******************************************************************
        01  XML-PATIENTMODREQ.
            02  PTMODREQ-CONTEXT           PIC S9(9)   BINARY.
            02  PTMODREQ-OBJECT            PIC X(36).
            02  PTMODREQ-MODE              PIC S9(9)   BINARY.
            02  PTMODREQ-RECORDNAME        PIC X(128).
            02  PTMODREQ-MEDICALREQ.
      *       リクエスト番号
              03  PTMODREQ-REQUEST-NUMBER   PIC X(2).
      *       UID（電子カルテ）
              03  PTMODREQ-KARTE-UID        PIC X(36).
      *       UID（オルカ）
              03  PTMODREQ-ORCA-UID         PIC X(36).
      *       基準日付（システム日付）
              03  PTMODREQ-BASE-DATE        PIC X(10).
      *       確認メッセージ返答
              03  PTMODREQ-SELECT-ANSWER    PIC X(03).
      *       継続区分
              03  PTMODREQ-CONT-MODE        PIC X(05).
      *       患者情報
              03  PTMODREQ-PTINF-INFO.
      *         患者番号
                05  PTMODREQ-PATIENTID        PIC X(20).
      *         患者氏名
                05  PTMODREQ-NAME             PIC X(100).
      *         患者カナ氏名
                05  PTMODREQ-KANANAME         PIC X(100).
      *         生年月日
                05  PTMODREQ-BIRTHDAY         PIC X(10).
      *         性別
                05  PTMODREQ-SEX              PIC X(01).
      *
      *       介護保険情報
              03  PTMODREQ-CARE-INFO-G.
      *          処理区分（MODIFY,DELETE)
                 05  PTMODREQ-CARE-MODE        PIC X(06).
      *          介護保険情報
                 05  PTMODREQ-CARE-INSURANCE-G     OCCURS   10.
      *              保険者番号
                     07  PTMODREQ-CARE-INSURANCE-NUMBER  PIC X(06).
      *              被保険者番号
                     07  PTMODREQ-CARE-POLICYHOLDER      PIC X(10).
      *              有効開始日
                     07  PTMODREQ-CARE-START-DATE        PIC X(10).
      *              有効終了日
                     07  PTMODREQ-CARE-END-DATE          PIC X(10).
      *
      *       要介護認定情報
              03  PTMODREQ-NEED-INFO-G.
      *          処理区分（MODIFY,DELETE)
                 05  PTMODREQ-NEED-MODE        PIC X(06).
      *          要介護認定情報
                 05  PTMODREQ-CERTIFICATION-G    OCCURS   50.
      *              要介護状態区分
                     07  PTMODREQ-NEED-CARE-STATE-CODE   PIC X(02).
      *              認定日付
                     07  PTMODREQ-NEED-DATE              PIC X(10).
      *              有効開始日
                     07  PTMODREQ-NEED-START-DATE        PIC X(10).
      *              有効終了日
                     07  PTMODREQ-NEED-END-DATE          PIC X(10).
      *
