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
      *    ＡＰＩ患者登録情報（基本情報）(一体化　V3)
      *                                   COPY (CPPATIENTMODV3REQ1.INC)
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
      *       患者番号
              03  PTMODREQ-PATIENTID        PIC X(20).
      *       処理区分
              03  PTMODREQ-PATIENT-MODE     PIC X(06).
      *       UID（オルカ）
              03  PTMODREQ-ORCA-UID         PIC X(36).
      *       確認メッセージ返答
              03  PTMODREQ-SELECT-ANSWER    PIC X(02).
      *       継続区分
              03  PTMODREQ-CONT-MODE        PIC X(05).
      *       患者情報
              03  PTMODREQ-PTINF-INFO.
      *         患者氏名
                05  PTMODREQ-NAME             PIC X(50).
      *         患者カナ氏名
                05  PTMODREQ-KANANAME         PIC X(50).
      *         生年月日
                05  PTMODREQ-BIRTHDAY         PIC X(10).
      *         性別
                05  PTMODREQ-SEX              PIC X(01).
      *         世帯主名
                05  PTMODREQ-SETAI-NAME       PIC X(50).
      *         続柄
                05  PTMODREQ-ZOKUGARA         PIC X(30).
      *         職業
                05  PTMODREQ-JOB              PIC X(20).
      *         通称
                05  PTMODREQ-NICKNAME         PIC  X(50).
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
                  07  PTMODREQ-HOME-ADDRESSES1   PIC X(100).
      *           番地番号
                  07  PTMODREQ-HOME-ADDRESSES2   PIC X(100).
      *           電話番号（自宅）
                  07  PTMODREQ-HOME-HPONE1       PIC X(15).
      *           電話番号（連絡先）
                  07  PTMODREQ-HOME-HPONE2       PIC X(15).
      *         勤務先情報
                05  PTMODREQ-OFFICE-INFO.
      *           勤務先名
                  07  PTMODREQ-OFFICE-NAME        PIC X(50).
      *           郵便番号
                  07  PTMODREQ-OFFICE-ZIP-CODE    PIC X(7).
      *           住所
                  07  PTMODREQ-OFFICE-ADDRESSES1  PIC X(100).
      *           番地番号
                  07  PTMODREQ-OFFICE-ADDRESSES2  PIC X(100).
      *           電話番号
                  07  PTMODREQ-OFFICE-HPONE       PIC X(15).
      *         連絡先情報
                05  PTMODREQ-RENRAKU-INFO.
      *           連絡先名
                  07  PTMODREQ-RENRAKU-NAME        PIC X(50).
      *           連絡先続柄
                  07  PTMODREQ-RENRAKU-ZOKUGARA    PIC X(30).
      *           郵便番号
                  07  PTMODREQ-RENRAKU-ZIP-CODE    PIC X(7).
      *           住所
                  07  PTMODREQ-RENRAKU-ADDRESSES1  PIC X(100).
      *           番地番号
                  07  PTMODREQ-RENRAKU-ADDRESSES2  PIC X(100).
      *           電話番号　昼
                  07  PTMODREQ-RENRAKU-HPONE1      PIC X(15).
      *           電話番号　夜
                  07  PTMODREQ-RENRAKU-HPONE2      PIC X(15).
      *         帰省先情報
                05  PTMODREQ-KISEI-INFO.
      *           帰省先名
                  07  PTMODREQ-KISEI-NAME          PIC X(50).
      *           郵便番号
                  07  PTMODREQ-KISEI-ZIP-CODE      PIC X(7).
      *           住所
                  07  PTMODREQ-KISEI-ADDRESSES1    PIC X(100).
      *           番地番号
                  07  PTMODREQ-KISEI-ADDRESSES2    PIC X(100).
      *           電話番号
                  07  PTMODREQ-KISEI-HPONE         PIC X(15).
      *         禁忌１
                05  PTMODREQ-TABOO-1              PIC  X(100).
      *         禁忌２
                05  PTMODREQ-TABOO-2              PIC  X(100).
      *         アレルギー１
                05  PTMODREQ-ALLERGY-1            PIC  X(100).
      *         アレルギー２
                05  PTMODREQ-ALLERGY-2            PIC  X(100).
      *         感染症１
                05  PTMODREQ-KANSENSYO-1          PIC  X(100).
      *         感染症２
                05  PTMODREQ-KANSENSYO-2          PIC  X(100).
      *         コメント１
                05  PTMODREQ-COMMENT-1            PIC  X(100).
      *         コメント２
                05  PTMODREQ-COMMENT-2            PIC  X(100).
      *         テスト患者区分
                05  PTMODREQ-TEST-PATIENT         PIC  X(01).
      *         死亡区分
                05  PTMODREQ-DEATHKBN             PIC  X(01).
      *         減免事由
                05  PTMODREQ-DISCOUNTKBN          PIC  X(02).
      *         割引率
                05  PTMODREQ-DISCOUNT             PIC  X(02).
      *         状態
                05  PTMODREQ-CONDITION-KBN-G.
                    07  PTMODREQ-CONDITION-KBN    PIC  X(02)
                                                  OCCURS   3.
      *
             03  PTMODREQ-MODE-AREA.
      *          旧姓履歴登録区分
                 05  PTMODREQ-KYUSEI-MODE          PIC  X(05).
      *
      *
