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
      *         システム管理マスタ     (CPSK1010.INC)   *
      *         （１０１０）                            *
      *         職員情報                                *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 00-00-01     MCC-森脇     02/05/07  予備の修正
      *                                     麻薬ライセンスの追加
      * 01.00.02     NACL-多々納  05/12/01  請求書発行フラグ等追加
      * 01.00.03     NACL-多々納  06/04/10  診療費明書発行追加
      * 03.04.00     NACL-多々納  06/11/13  検査正式名称表示フラグ追加
      * 03.05.01     NACL-藤原    07/09/14  クライアント保存可否フラグ追加
      * 04.04.00     NACL-小豆沢  09/01/27  本院分院対応（他院ログインＩＤ追加）
      * 04.05.00     NACL-森脇    09/11/25  ユーザーログイン権限追加
      * 04.08.00     NACL-多々納  15/10/27  表示区分追加
      ************************************************************
       01  SYS-1010-REC.
           03  SYS-1010-KEY.
               05  SYS-1010-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-1010-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-1010-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-1010-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-1010-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-1010-TBL.
      *オペレータＩＤ
               05  SYS-1010-USERID      PIC  X(16).
      *フリガナ
               05  SYS-1010-KANANAME    PIC  X(80).
      *氏名
               05  SYS-1010-NAME        PIC  X(80).
      *医籍登録番号
               05  SYS-1010-ISEKINUM    PIC  X(08).
      *保険医登録番号（社保）
               05  SYS-1010-HKNDRNUMS   PIC  X(08).
      *保険医登録番号（国保）
               05  SYS-1010-HKNDRNUMK   PIC  X(12).
      *専門科コード
               05  SYS-1010-SEMONKACD  OCCURS  5.
                   07  SYS-1010-SEMONKACD-X  PIC  X(02).
      *業務処理権限
               05  SYS-1010-GSRAUTH     PIC  X(70).
      *管理者権限フラグ
               05  SYS-1010-KANRIAUTHFLG  PIC  X(01).
      *麻薬ライセンス
               05  SYS-1010-MAYAKULICENSE    PIC  X(40).
      *診療行為初期表示フラグ
               05  SYS-1010-ORCA21FLG     PIC  X(01).
      *
      *請求書発行フラグ
               05  SYS-1010-SKYPRTFLG      PIC  X(01).
      *院外処方せん発行フラグ
               05  SYS-1010-SYOHOPRTFLG    PIC  X(01).
      *薬剤情報発行フラグ
               05  SYS-1010-YAKUJYOPRTFLG  PIC  X(01).
      *診療費明細書発行区分フラグ
               05  SYS-1010-MEIPRTFLG      PIC  X(01).
      *検査正式名称表示フラグ
               05  SYS-1010-FORMALFLG      PIC  X(01).
      *クライアント保存可否フラグ
               05  SYS-1010-CLIENT-SAVE    PIC  X(01).
      *グループ医療機関ログイン情報
               05  SYS-1010-GRPHOSPINF-G.
                   07  SYS-1010-GRPHOSPINF     OCCURS  9.
                       09  SYS-1010-GRPHOSPNUMX   PIC  X(02).
                       09  SYS-1010-GRPHOSPNUM    REDEFINES
                           SYS-1010-GRPHOSPNUMX   PIC  9(02).
                       09  SYS-1010-GRPHOSPUSERID PIC  X(16).
      *ユーザーログイン権限フラグ
               05  SYS-1010-USER-KANRI     PIC  X(01).
      *クライアント印刷可否フラグ
               05  SYS-1010-CLIENT-PRT     PIC  X(01).
      *ダイアログ表示フラグ
               05  SYS-1010-DIALOGFLG      PIC  X(01).
      *表示区分
               05  SYS-1010-HYOJIFLG       PIC  X(01).
      *予備
      **** 03                           PIC  X(175).
      *****03                           PIC  X(174).
      *****03                           PIC  X(168).
      *****03                           PIC  X(06).
      *****03                           PIC  X(05).
      *****03  SYS-1010-YOBI            PIC  X(03).
           03  SYS-1010-YOBI            PIC  X(02).
      *端末ＩＤ
           03  SYS-1010-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-1010-OPID            PIC  X(16).
      *作成年月日
           03  SYS-1010-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-1010-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-1010-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-1010-UPD-KEY.
               05  SYS-1010-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1010-UPD-KANRICD     PIC X(04).
               05  SYS-1010-UPD-KBNCD       PIC X(08).
               05  SYS-1010-UPD-STYUKYMD    PIC X(08).
               05  SYS-1010-UPD-EDYUKYMD    PIC X(08).
