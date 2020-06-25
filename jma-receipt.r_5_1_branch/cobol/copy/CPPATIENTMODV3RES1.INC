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
      *                                   COPY (CPPATIENTMODV3RES1.INC)
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
      *      患者情報
             03  PTMODRES-PTINF-INFO.
      *        患者番号
               05  PTMODRES-PATIENTID        PIC X(20).
      *        患者氏名
               05  PTMODRES-NAME             PIC X(50).
      *        患者カナ氏名
               05  PTMODRES-KANANAME         PIC X(50).
      *        生年月日
               05  PTMODRES-BIRTHDAY         PIC X(10).
      *        性別
               05  PTMODRES-SEX              PIC X(01).
      *        世帯主名
               05  PTMODRES-SETAI-NAME       PIC X(50).
      *        続柄
               05  PTMODRES-ZOKUGARA         PIC X(30).
      *        職業
               05  PTMODRES-JOB              PIC X(20).
      *        通称
               05  PTMODRES-NICKNAME         PIC  X(50).
      *        携帯電話番号
               05  PTMODRES-KEITAI-TEL       PIC  X(015).
      *        ＦＡＸ番号
               05  PTMODRES-FAX              PIC  X(015).
      *        電子メールアドレス
               05  PTMODRES-EMAIL            PIC  X(050).
      *        自宅情報
               05  PTMODRES-ADDRESS-INFO.
      *            郵便番号
                   07  PTMODRES-HOME-ZIP-CODE     PIC X(7).
      *            住所
                   07  PTMODRES-HOME-ADDRESSES1   PIC X(100).
      *            番地番号
                   07  PTMODRES-HOME-ADDRESSES2   PIC X(100).
      *            電話番号（自宅）
                   07  PTMODRES-HOME-HPONE1       PIC X(15).
      *            電話番号（連絡先）
                   07  PTMODRES-HOME-HPONE2       PIC X(15).
      *        勤務先情報
               05  PTMODRES-OFFICE-INFO.
      *            勤務先名
                   07  PTMODRES-OFFICE-NAME        PIC X(50).
      *            郵便番号
                   07  PTMODRES-OFFICE-ZIP-CODE    PIC X(7).
      *            住所
                   07  PTMODRES-OFFICE-ADDRESSES1  PIC X(100).
      *            番地番号
                   07  PTMODRES-OFFICE-ADDRESSES2  PIC X(100).
      *            電話番号
                   07  PTMODRES-OFFICE-HPONE        PIC X(15).
      *         連絡先情報
                05  PTMODRES-RENRAKU-INFO.
      *           連絡先名
                  07  PTMODRES-RENRAKU-NAME        PIC X(50).
      *           連絡先続柄
                  07  PTMODRES-RENRAKU-ZOKUGARA    PIC X(30).
      *           郵便番号
                  07  PTMODRES-RENRAKU-ZIP-CODE    PIC X(7).
      *           住所
                  07  PTMODRES-RENRAKU-ADDRESSES1  PIC X(100).
      *           番地番号
                  07  PTMODRES-RENRAKU-ADDRESSES2  PIC X(100).
      *           電話番号　昼
                  07  PTMODRES-RENRAKU-HPONE1      PIC X(15).
      *           電話番号　夜
                  07  PTMODRES-RENRAKU-HPONE2      PIC X(15).
      *         帰省先情報
                05  PTMODRES-KISEI-INFO.
      *           勤務先名
                  07  PTMODRES-KISEI-NAME          PIC X(50).
      *           郵便番号
                  07  PTMODRES-KISEI-ZIP-CODE      PIC X(7).
      *           住所
                  07  PTMODRES-KISEI-ADDRESSES1    PIC X(100).
      *           番地番号
                  07  PTMODRES-KISEI-ADDRESSES2    PIC X(100).
      *           電話番号
                  07  PTMODRES-KISEI-HPONE         PIC X(15).
      *        禁忌１
               05  PTMODRES-TABOO-1              PIC  X(100).
      *        禁忌２
               05  PTMODRES-TABOO-2              PIC  X(100).
      *        アレルギー１
               05  PTMODRES-ALLERGY-1            PIC  X(100).
      *        アレルギー２
               05  PTMODRES-ALLERGY-2            PIC  X(100).
      *        感染症１
               05  PTMODRES-KANSENSYO-1          PIC  X(100).
      *        感染症２
               05  PTMODRES-KANSENSYO-2          PIC  X(100).
      *        コメント１
               05  PTMODRES-COMMENT-1            PIC  X(100).
      *        コメント２
               05  PTMODRES-COMMENT-2            PIC  X(100).
      *        テスト患者区分
               05  PTMODRES-TEST-PATIENT         PIC  X(01).
      *        死亡区分
               05  PTMODRES-DEATHKBN             PIC  X(01).
      *        減免事由
               05  PTMODRES-DISCOUNTKBN          PIC  X(02).
               05  PTMODRES-DISCOUNTKBN-NAME     PIC  X(20).
      *        割引率
               05  PTMODRES-DISCOUNT             PIC  X(02).
               05  PTMODRES-DISCOUNT-NAME        PIC  X(14).
      *        状態１から３
               05  PTMODRES-CONDITION-KBN-G.
                    07  PTMODREQ-CONDITION-OCC    OCCURS   3.
                        09  PTMODRES-CONDITION-KBN     PIC X(02).
                        09  PTMODRES-CONDITION-NAME    PIC X(20).
      *
      *      同一患者情報
             03  PTMODRES-JUFUKU-PATIENT-G.
                 05  PTMODRES-JUFUKU-PATIENT-INFO   OCCURS   20.
      *              患者番号
                     07  PTMODRES-JFK-PATIENTID      PIC X(20).
      *              患者氏名
                     07  PTMODRES-JFK-NAME           PIC X(50).
      *              患者カナ氏名
                     07  PTMODRES-JFK-KANANAME       PIC X(50).
      *              生年月日
                     07  PTMODRES-JFK-BIRTHDAY       PIC X(10).
      *              性別
                     07  PTMODRES-JFK-SEX            PIC X(01).
      *              住所
                     07  PTMODRES-JFK-HOME-ADRS      PIC X(100).
      *              番地番号
                     07  PTMODRES-JFK-HOME-BANTI     PIC X(100).
      *
      *      エラーメッセージ
             03  PTMODRES-PATIENT-ERRMSG-G.
                 05  PTMODRES-PATIENT-ERRMSG-INF  OCCURS   50.
                     07  PTMODRES-ERROR        PIC X(04).
                     07  PTMODRES-ERROR-MSG    PIC X(200).
      *      ワーニングメッセージ
             03  PTMODRES-WARNING-MSG-G.
                 05  PTMODRES-WARNING-INF    OCCURS   50.
                     07  PTMODRES-WARNING      PIC X(04).
                     07  PTMODRES-WARNING-MSG  PIC X(200).
      *
      *      確認メッセージ
             03  PTMODRES-SELECT-G.
                 05  PTMODRES-SELECT-CODE      PIC X(04).
                 05  PTMODRES-SELECT-MSG       PIC X(200).
      *
