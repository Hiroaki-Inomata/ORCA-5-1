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
      *      レセプト記載要領マスター     COPY (CPRECEKISAI.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  RKISAI-REC.
           03  RKISAI-KEY.
      *        項番
               05  RKISAI-ITEMNO               PIC 9(04).
      *        枝番
               05  RKISAI-EDANO                PIC 9(02).
      *        開始年月
               05  RKISAI-STYM                 PIC X(06).
      *        終了年月
               05  RKISAI-EDYM                 PIC X(06).
      *        診療行為コード
               05  RKISAI-SRYCD                PIC X(09).
      *        連番
               05  RKISAI-RENNUM               PIC 9(02).
      *    コメントコード
           03  RKISAI-C-CD                     PIC X(09).
      *    患者の状態コード
           03  RKISAI-JYOTAICD                 PIC X(03).
      *    掲載位置区分
           03  RKISAI-KEISAIKBN                PIC 9(01).
      *    区分
           03  RKISAI-KUBUN                    PIC X(64).
      *    条件区分
           03  RKISAI-JYOKEN                   PIC 9(02).
      *    非算定理由コメント
           03  RKISAI-HISANTEI                 PIC 9(01).
      *    処理区分
      *      0:
      *      1:入力（選択肢）
      *      2:入力（自動発生）
      *      3:レセプト（自動記載）
           03  RKISAI-SYORIKBN                 PIC 9(01).
      *    診療行為名称等
           03  RKISAI-MEISYOETC                PIC X(300).
      *    変更年月日
           03  RKISAI-CHGYMD                   PIC X(08).
      *    廃止年月日
           03  RKISAI-HAISIYMD                 PIC X(08).
      *    作成年月日
           03  RKISAI-CREYMD                   PIC X(08).
      *    更新年月日
           03  RKISAI-UPYMD                    PIC X(08).
      *    更新時間
           03  RKISAI-UPHMS                    PIC X(06).
      *    入外区分
           03  RKISAI-NYUGAIKBN                PIC X(01).
      *    算定回数
           03  RKISAI-KAISU                    PIC 9(03).
      *更新用
           03  RKISAI-UP-KEY.
               05  RKISAI-UPITEMNO             PIC 9(04).
               05  RKISAI-UPEDANO              PIC 9(02).
               05  RKISAI-UPSTYM               PIC X(06).
               05  RKISAI-UPEDYM               PIC X(06).
               05  RKISAI-UPSRYCD              PIC X(09).
               05  RKISAI-UPRENNUM             PIC 9(02).
