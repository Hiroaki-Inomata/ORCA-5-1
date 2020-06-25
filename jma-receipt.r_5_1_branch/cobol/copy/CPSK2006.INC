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
      *         システム管理マスタ     (CPSK2006.INC)            *
      *         （２００６）                                     *
      *         レセプト特記事項編集情報                         *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.01    NACL-藤原    14/02/10  入外区分、所得区分の追加
      ************************************************************
       01  SYS-2006-REC.
           03  SYS-2006-KEY.
               05  SYS-2006-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-2006-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-2006-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-2006-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-2006-EDYUKYMD    PIC  X(08).
      *条件：保険番号
           03  SYS-2006-HKNNUM          PIC  X(03).
      *条件：審査支払機関
           03  SYS-2006-TEISYUTUSAKI    PIC  X(01).
      *条件：保険者番号
           03  SYS-2006-HKNJANUM        PIC  X(08).
      *条件：老人区分
           03  SYS-2006-RJNKBN          PIC  X(01).
      *条件：在総診区分
           03  SYS-2006-ZAITAKU         PIC  X(01).
      *特記事項
           03  SYS-2006-TOKKI           PIC  X(24).
      *条件：負担者番号
           03  SYS-2006-FTNJANUMKBN-JYOKEN
                                        PIC  X(01).
      *条件：負担者番号内容
           03  SYS-2006-FTNJANUM-JYOKEN PIC  X(08).
      *条件：入外区分
           03  SYS-2006-NYUGAIKBN       PIC  X(01).
      *条件：所得区分
           03  SYS-2006-SYOTOKUKBN      PIC  X(01).
      *予備
           03  SYS-2006-YOBI1           PIC  X(89).
      *給付割合
           03  SYS-2006-KYURATE         PIC  9(03).
      *予備
           03  SYS-2006-YOBI2           PIC  X(02).
      *レセ電：特記事項
           03  SYS-2006-RECEDEN-TOKKI   PIC  X(02).
      *レセ電：給付割合
           03  SYS-2006-RECEDEN-KYURATE PIC  9(03).
      *レセ電：公費レコード区分
           03  SYS-2006-RECEDEN-KORECKBN
                                        PIC  X(01).
      *適用欄上段コメント
           03  SYS-2006-TEKIYOU-COMMENT1
                                        PIC  X(52).
      *適用欄上段コメントレセ電編集区分
           03  SYS-2006-TEKIYOU-COMMENT1KBN
                                        PIC  X(01).
      *適用欄下段コメント
           03  SYS-2006-TEKIYOU-COMMENT2
                                        PIC  X(52).
      *適用欄下段コメントレセ電編集区分
           03  SYS-2006-TEKIYOU-COMMENT2KBN
                                        PIC  X(01).
      *予備
           03  SYS-2006-YOBI3           PIC  X(49).
      *負担者番号編集区分
           03  SYS-2006-FTNJANUMKBN     PIC  X(02).
      *負担者番号編集内容
           03  SYS-2006-FTNJANUM        PIC  X(08).
      *受給者番号編集区分
           03  SYS-2006-JKYSNUMKBN      PIC  X(02).
      *受給者番号編集内容
           03  SYS-2006-JKYSNUM         PIC  X(07).
      *レセ電：負担者番号編集区分
           03  SYS-2006-RECEDEN-FTNJANUMKBN
                                        PIC  X(02).
      *レセ電：負担者番号編集内容
           03  SYS-2006-RECEDEN-FTNJANUM
                                        PIC  X(08).
      *レセ電：受給者番号編集区分
           03  SYS-2006-RECEDEN-JKYSNUMKBN
                                        PIC  X(02).
      *レセ電：受給者番号編集内容
           03  SYS-2006-RECEDEN-JKYSNUM PIC  X(07).
      *予備
           03  SYS-2006-YOBI4           PIC  X(158).
      *端末ＩＤ
           03  SYS-2006-TERMID          PIC  X(16).
      *オペレータＩＤ
           03  SYS-2006-OPID            PIC  X(16).
      *作成年月日
           03  SYS-2006-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-2006-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-2006-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-2006-UPD-KEY.
               05  SYS-2006-UPD-HOSPNUM     PIC 9(2).
               05  SYS-2006-UPD-KANRICD     PIC X(04).
               05  SYS-2006-UPD-KBNCD       PIC X(08).
               05  SYS-2006-UPD-STYUKYMD    PIC X(08).
               05  SYS-2006-UPD-EDYUKYMD    PIC X(08).
