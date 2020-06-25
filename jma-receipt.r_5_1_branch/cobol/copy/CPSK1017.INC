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
      *-------------------------------------------------*
      *         システム管理マスタ     (CPSK1017.INC)   *
      *         （１０１７）                            *
      *         患者登録機能制御情報                    *
      *-------------------------------------------------*
      *****************************************************************
      *  修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    04.03.08  生活保護受給者番号の追加
      *  01.00.02    NACL-多々納  04.10.08  同姓同名・二重登録疑いの追加
      *  01.00.02    NACL-多々納  06.02.08  地方公費単独チェックの追加
      *  01.00.03    NACL-多々納  06.05.19  患者削除機能チェックの追加
      *  03.05.00    NACL-多々納  07.06.22  本人・家族区分設定追加
      *  04.03.00    NACL-多々納  08.06.22  家族年齢等追加
      *  04.03.00    NACL-多々納  08.10.30  世帯主名自動記載区分
      *  04.04.00    NACL-多々納  09.01.27  グループ医療機関リスト追加
      *  04.05.00    NACL-多々納  09.11.20  郵便番号記載追加
      *  04.06.00    NACL-多々納  11.02.09  京都府通り名称編集区分追加
      *  04.07.00    NACL-多々納  11.07.XX  公費法別一覧表示区分追加
      *  04.07.00    NACL-多々納  15.06.11  保険初期表示区分追加
      *****************************************************************
       01  SYS-1017-REC.
           03  SYS-1017-KEY.
               05  SYS-1017-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-1017-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-1017-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-1017-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-1017-EDYUKYMD    PIC  X(08).
      *人名辞書使用チェック
           03  SYS-1017-JINMEI-USE      PIC X(01).
      *カルテ病名編集
           03  SYS-1017-KARUTE-BYOMEI   PIC X(01).
      *患者登録のカルテ発行
           03  SYS-1017-KARUTE-HAKKO    PIC X(01).
      *自費保険の補助区分
           03  SYS-1017-JIHIHKN-TAX     PIC X(01).
      *生活保護受給者番号（未使用）
           03  SYS-1017-SEIHO-JKYSNUM   PIC X(01).
      *同姓同名チェック
           03  SYS-1017-DOUMEI-CHKFLG   PIC X(01).
      *二重登録疑いチェック区分
           03  SYS-1017-JYUFUKU-CHKKBN  PIC X(01).
      *地方公費単独チェック
           03  SYS-1017-KOHONLY-CHKFLG  PIC X(01).
      *患者情報削除機能
           03  SYS-1017-PTNUMDEL-FLG    PIC X(01).
      *本人・家族区分設定
           03  SYS-1017-HONKZKKBN-FLG   PIC X(01).
      *家族年齢設定
           03  SYS-1017-KZKNEN          PIC X(02).
      *被保険者名設定区分
           03  SYS-1017-HIHKNJAKBN-FLG  PIC X(01).
      *世帯主名自動記載区分
           03  SYS-1017-SETAIKBN-FLG    PIC X(01).
      *グループ医療機関患者リスト
           03  SYS-1017-GRPPTLST-FLG    PIC X(01).
      *公費上限額履歴チェック区分
           03  SYS-1017-KOHRRKKBN-FLG   PIC X(01).
      *郵便番号自動記載区分
           03  SYS-1017-POSTKBN-FLG     PIC X(01).
      *都道府県名記載区分
           03  SYS-1017-KENMEIKBN-FLG   PIC X(01).
      *京都府通り名称編集区分
           03  SYS-1017-TOWNNAME2KBN-FLG   PIC X(01).
      *公費法別一覧表示区分
           03  SYS-1017-KOHHBTKBN-FLG      PIC X(01).
      *保険初期表示区分
           03  SYS-1017-HKNINITKBN-FLG     PIC X(01).
      *予備
      **** 03  FILLER                   PIC X(495).
      **** 03  FILLER                   PIC X(493).
      **** 03  FILLER                   PIC X(490).
           03  SYS-1017-YOBI            PIC X(479).
      *端末ＩＤ
           03  SYS-1017-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-1017-OPID            PIC  X(16).
      *作成年月日
           03  SYS-1017-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-1017-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-1017-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-1017-UPD-KEY.
               05  SYS-1017-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1017-UPD-KANRICD     PIC X(04).
               05  SYS-1017-UPD-KBNCD       PIC X(08).
               05  SYS-1017-UPD-STYUKYMD    PIC X(08).
               05  SYS-1017-UPD-EDYUKYMD    PIC X(08).
