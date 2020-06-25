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
      *    ＤＰＣ患者算定基本情報        COPY (CPDPCPTKHN.INC)       *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  DPCPTKHN-REC.
      *キー
           03  DPCPTKHN-KEY.
      *医療機関ＩＤ
               05  DPCPTKHN-HOSPNUM            PIC  9(2).
      *患者ＩＤ
               05  DPCPTKHN-PTID               PIC  9(10).
      *ＤＰＣ履歴番号（７日以内の再入院を考慮した履歴番号）
               05  DPCPTKHN-DPCRRKNUM          PIC  9(03).
      *ＤＰＣ履歴番号枝番
               05  DPCPTKHN-DPCRRKEDANUM       PIC  9(03).
      *ＤＰＣ履歴番号最大枝番
           03  DPCPTKHN-DPCMAXEDANUM           PIC  9(03).
      *ＤＰＣ算定病棟入院区分（0:該当しない/1:該当する）
           03  DPCPTKHN-DPCTEKKBN              PIC  X(01).
      *適用開始日
           03  DPCPTKHN-TEKSTYMD               PIC  X(08).
      *適用開始日区分（1:入院日/2:転棟日）
           03  DPCPTKHN-TEKSTKBN               PIC  X(01).
      *適用終了日
           03  DPCPTKHN-TEKEDYMD               PIC  X(08).
      *適用終了日区分（1:退院日/2:転棟日）
           03  DPCPTKHN-TEKEDKBN               PIC  X(01).
      *同一傷病での７日以内再入院予定の有無（0:なし/1:あり）
           03  DPCPTKHN-SAINYUKBN              PIC  X(01).
      *診断群分類番号
           03  DPCPTKHN-DPC-CD                 PIC  X(14).
      *ＤＰＣ転帰区分
           03  DPCPTKHN-DPCTENKIKBN            PIC  X(01).
      *死因
           03  DPCPTKHN-COD                    PIC  X(100).
      *区分番号
           03  DPCPTKHN-KBNNUM                 PIC  X(10).
      *実施（予定）年月日
           03  DPCPTKHN-JISSIYMD               PIC  X(08).
      *診療区分コード
           03  DPCPTKHN-SRYKBNCD               PIC  X(04).
      *診療名称
           03  DPCPTKHN-STYNAME                PIC  X(200).
      *端末ＩＤ
           03  DPCPTKHN-TERMID                 PIC  X(16).
      *オペーレータＩＤ
           03  DPCPTKHN-OPID                   PIC  X(16).
      *作成年月日
           03  DPCPTKHN-CREYMD                 PIC  X(08).
      *更新年月日
           03  DPCPTKHN-UPYMD                  PIC  X(08).
      *更新時間
           03  DPCPTKHN-UPHMS                  PIC  X(06).
      *以下virtual項目
           03  DPCPTKHN-RRKNUM                 PIC  9(03).
      *ソートフラグ
           03  DPCPTKHN-SORTFLG                PIC  9(01).
