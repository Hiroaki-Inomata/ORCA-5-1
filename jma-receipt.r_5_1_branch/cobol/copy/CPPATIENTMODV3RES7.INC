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
      *    ＡＰＩ患者登録情報（患者禁忌薬剤）(一体化　V3)
      *                                   COPY (CPPATIENTMODV3RES7.INC)
      ******************************************************************
       01  XML-PATIENTMODRES.
           02  PTMODRES-CONTEXT           PIC S9(9)   BINARY.
           02  PTMODRES-OBJECT            PIC X(36).
           02  PTMODRES-MODE              PIC S9(9)   BINARY.
           02  PTMODRES-RECORDNAME        PIC X(128).
           02  PTMODRES-APPOINTRES.
      *      リクエスト番号
             03  PTMODRES-REQUEST-NUMBER    PIC X(2).
      *      レスポンス番号
             03  PTMODRES-RESPONSE-NUMBER   PIC X(2).
      *      UID（電子カルテ）
             03  PTMODRES-KARTE-UID         PIC X(36).
      *      UID（オルカ）
             03  PTMODRES-ORCA-UID          PIC X(36).
      *      継続区分
             03  PTMODRES-CONT-MODE         PIC X(05).
      *      実施日
             03  PTMODRES-INFORMATION-DATE  PIC X(10).
      *      実施時間
             03  PTMODRES-INFORMATION-TIME  PIC X(8).
      *      返却コード
             03  PTMODRES-API-RESULT        PIC X(3).
      *      返却メッセージ
             03  PTMODRES-API-RESULT-MSG    PIC X(100).
      *
             03  PTMODRES-RESKEY              PIC X(100).
      *      患者番号
             03  PTMODRES-PATIENTID        PIC X(20).
      *      患者情報
             03  PTMODRES-PTINF-INFO.
      *        患者氏名
               05  PTMODRES-NAME             PIC X(100).
      *        患者カナ氏名
               05  PTMODRES-KANANAME         PIC X(100).
      *        生年月日
               05  PTMODRES-BIRTHDAY         PIC X(10).
      *        性別
               05  PTMODRES-SEX              PIC X(01).
      *
      *       患者禁忌薬剤情報
              03  PTMODRES-PTKINKI-INFO-G.
      *          患者禁忌薬剤情報
                 05  PTMODRES-PTKINKI-G       OCCURS   500.
      *              禁忌薬剤コード
                     07  PTMODRES-DRUGCD-CODE      PIC X(09).
      *              禁忌薬剤名称
                     07  PTMODRES-DRUGCD-NAME      PIC X(80).
      *              有効終了日（マスタ終了日）
                     07  PTMODRES-DRUGCD-END-DATE  PIC X(10).
      *              経過措置品目移行コード
                     07  PTMODRES-CHANGE-CODE      PIC X(09).
                     07  PTMODRES-CHANGE-NAME      PIC X(80).
      *              禁忌開始日
                     07  PTMODRES-START-DATE       PIC X(10).
      *              投与日
                     07  PTMODRES-TOUYO-DATE       PIC X(10).
      *              エラーコード
                     07  PTMODRES-ERRCD            PIC X(04).
                     07  PTMODRES-ERRCD-MSG        PIC X(100).
      *
