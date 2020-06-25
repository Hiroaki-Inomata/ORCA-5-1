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
      *         システム管理マスタ     (CPSK1043.INC)   *
      *         （１０４３）                            *
      *         会計照会機能情報                        *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  06/01/20  診療科初期表示追加
      *  04.03.00    NACL-多々納  08/08/20  レセプトブレビュー区分追加
      *  04.07.00    NACL-多々納  12/11/21  レセプトプレビューレイアウト追加
      ************************************************************
       01  SYS-1043-REC.
           03  SYS-1043-KEY.
               05  SYS-1043-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-1043-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-1043-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-1043-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-1043-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-1043-TBL.
      *全科の表示順区分
               05  SYS-1043-SRYKBN-FLG  PIC  X(01).
      *診療科の初期表示
               05  SYS-1043-SRYKA-FLG   PIC  X(01).
      *レセプトブレビュー
               05  SYS-1043-RESPRVFLG   PIC  X(01).
      *レセプトブレビュー　レイアウト
               05  SYS-1043-PRVLAYOUT   PIC  X(01).
      *予備
      ******** 05  SYS-1043-YOBI        PIC  X(497).
               05  SYS-1043-YOBI        PIC  X(496).
      *端末ＩＤ
           03  SYS-1043-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-1043-OPID            PIC  X(16).
      *作成年月日
           03  SYS-1043-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-1043-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-1043-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-1043-UPD-KEY.
               05  SYS-1043-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1043-UPD-KANRICD     PIC X(04).
               05  SYS-1043-UPD-KBNCD       PIC X(08).
               05  SYS-1043-UPD-STYUKYMD    PIC X(08).
               05  SYS-1043-UPD-EDYUKYMD    PIC X(08).
