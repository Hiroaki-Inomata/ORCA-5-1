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
      *         システム管理マスタ     (CPSK1009.INC)   *
      *         （１００９）                            *
      *         患者番号構成管理                        *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-多々納   01/11/27  自由構成区分追加
      ************************************************************
       01  SYS-1009-REC.
           03  SYS-1009-KEY.
               05  SYS-1009-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-1009-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-1009-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-1009-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-1009-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-1009-TBL.
      *患者番号構成区分
               05  SYS-1009-PTNUMKSIKBN             PIC X(01).
      *自由構成
               05  SYS-1009-JIYKSI.
      *    自由構成区分
                   07  SYS-1009-JIYKSIKBN           PIC X(01).
      *    自由構成桁数
                   07  SYS-1009-JIYKSIKETA          PIC 9(02).
      *標準構成区分
               05  SYS-1009-HJNKSI.
      *    標準構成区分
                   07  SYS-1009-HJNKSIKBN           PIC X(01).
      *    標準構成年区分
                   07  SYS-1009-HJNKSINENKBN        PIC X(01).
      *    標準構成連番初期化区分
                   07  SYS-1009-HJNKSIRENNUMKBN     PIC X(01).
      *    標準構成連番号桁数
                   07  SYS-1009-HJNKSIRENNUMKETA    PIC 9(02).
      *拡張構成区分
               05  SYS-1009-KKCYKSI.
      *    拡張構成区分
                   07  SYS-1009-KKCKSIKBN           PIC X(01).
      *    拡張構成前桁数
                   07  SYS-1009-KKCKSIMAEKETA       PIC 9(02).
      *    拡張構成連番号桁数
                   07  SYS-1009-KKCKSIRENNUMKETA    PIC 9(02).
      *    拡張構成後桁数
                   07  SYS-1009-KKCKSIATOKETA       PIC 9(02).
      *年
               05  SYS-1009-NEN                 PIC X(04).
      *連番号
               05  SYS-1009-RENNUM              PIC 9(10).
      *患者ＩＤ
               05  SYS-1009-PTID                PIC 9(10).
      *年切替え月
               05  SYS-1009-NENTUKI             PIC X(02).
      *予備
      *********05                           PIC  X(461).
      *******  05                           PIC  X(460).
               05  SYS-1009-YOBI            PIC  X(458).
      *端末ＩＤ
               05  SYS-1009-TERMID          PIC  X(16).
      *オペーレータＩＤ
               05  SYS-1009-OPID            PIC  X(16).
      *作成年月日
               05  SYS-1009-CREYMD          PIC  X(08).
      *更新年月日
               05  SYS-1009-UPYMD           PIC  X(08).
      *更新時間
               05  SYS-1009-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-1009-UPD-KEY.
               05  SYS-1009-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1009-UPD-KANRICD     PIC X(04).
               05  SYS-1009-UPD-KBNCD       PIC X(08).
               05  SYS-1009-UPD-STYUKYMD    PIC X(08).
               05  SYS-1009-UPD-EDYUKYMD    PIC X(08).
