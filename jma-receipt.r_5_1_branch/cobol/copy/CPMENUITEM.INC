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
      *****************************************************************
      *    メニュー番号とシステム管理：1010のGSRAUTHの関連付け
      *****************************************************************
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       01  MENU-ITEM-AREA.
      *医事業務
         02  MENU-ITEM-VAL.
           03  MENU-ITEM001-G.
               05  MENU-ITEM001-NUMBER         PIC 9(03)   VALUE 1.
               05  MENU-ITEM001-NAME           PIC X(30)
                                               VALUE "医事業務".
               05  MENU-ITEM001-GSRAUTH        PIC 9(02)   VALUE 1.
      *プログラム更新
           03  MENU-ITEM003-G.
               05  MENU-ITEM003-NUMBER         PIC 9(03)   VALUE   3.
               05  MENU-ITEM003-NAME           PIC X(30)   
                                               VALUE "プログラム更新".
               05  MENU-ITEM003-GSRAUTH        PIC 9(02)   VALUE   3.
      *外来まとめ
           03  MENU-ITEM029-G.
               05  MENU-ITEM029-NUMBER         PIC 9(03)   VALUE   29.
               05  MENU-ITEM029-NAME           PIC X(30)   
                                               VALUE "外来まとめ".
               05  MENU-ITEM029-GSRAUTH        PIC 9(02)   VALUE   5.
      *受付
           03  MENU-ITEM011-G.
               05  MENU-ITEM011-NUMBER         PIC 9(03)   VALUE  11.
               05  MENU-ITEM011-NAME           PIC X(30)   
                                               VALUE "受付".
               05  MENU-ITEM011-GSRAUTH        PIC 9(02)   VALUE  16.
      *登録
           03  MENU-ITEM012-G.
               05  MENU-ITEM012-NUMBER         PIC 9(03)   VALUE  12.
               05  MENU-ITEM012-NAME           PIC X(30)   
                                               VALUE "登録".
               05  MENU-ITEM012-GSRAUTH        PIC 9(02)   VALUE  17.
      *照会
           03  MENU-ITEM013-G.
               05  MENU-ITEM013-NUMBER         PIC 9(03)   VALUE  13.
               05  MENU-ITEM013-NAME           PIC X(30)   
                                               VALUE "照会".
               05  MENU-ITEM013-GSRAUTH        PIC 9(02)   VALUE  18.
      *予約
           03  MENU-ITEM014-G.
               05  MENU-ITEM014-NUMBER         PIC 9(03)   VALUE  14.
               05  MENU-ITEM014-NAME           PIC X(30)   
                                               VALUE "予約".
               05  MENU-ITEM014-GSRAUTH        PIC 9(02)   VALUE  19.
      *診療行為
           03  MENU-ITEM021-G.
               05  MENU-ITEM021-NUMBER         PIC 9(03)   VALUE  21.
               05  MENU-ITEM021-NAME           PIC X(30)   
                                               VALUE "診療行為".
               05  MENU-ITEM021-GSRAUTH        PIC 9(02)   VALUE  21.
      *病名
           03  MENU-ITEM022-G.
               05  MENU-ITEM022-NUMBER         PIC 9(03)   VALUE  22.
               05  MENU-ITEM022-NAME           PIC X(30)   
                                               VALUE "病名".
               05  MENU-ITEM022-GSRAUTH        PIC 9(02)   VALUE  22.
      *収納
           03  MENU-ITEM023-G.
               05  MENU-ITEM023-NUMBER         PIC 9(03)   VALUE  23.
               05  MENU-ITEM023-NAME           PIC X(30)   
                                               VALUE "収納".
               05  MENU-ITEM023-GSRAUTH        PIC 9(02)   VALUE  23.
      *会計照会
           03  MENU-ITEM024-G.
               05  MENU-ITEM024-NUMBER         PIC 9(03)   VALUE  24.
               05  MENU-ITEM024-NAME           PIC X(30)   
                                               VALUE "会計照会".
               05  MENU-ITEM024-GSRAUTH        PIC 9(02)   VALUE  24.
      *入退院登録
           03  MENU-ITEM031-G.
               05  MENU-ITEM031-NUMBER         PIC 9(03)   VALUE  31.
               05  MENU-ITEM031-NAME           PIC X(30)   
                                               VALUE "入退院登録".
               05  MENU-ITEM031-GSRAUTH        PIC 9(02)   VALUE  36.
      *入院会計照会
           03  MENU-ITEM032-G.
               05  MENU-ITEM032-NUMBER         PIC 9(03)   VALUE  32.
               05  MENU-ITEM032-NAME           PIC X(30)   
                                               VALUE "入院会計照会".
               05  MENU-ITEM032-GSRAUTH        PIC 9(02)   VALUE  37.
      *入院定期請求
           03  MENU-ITEM033-G.
               05  MENU-ITEM033-NUMBER         PIC 9(03)   VALUE  33.
               05  MENU-ITEM033-NAME           PIC X(30)   
                                               VALUE "入院定期請求".
               05  MENU-ITEM033-GSRAUTH        PIC 9(02)   VALUE  38.
      *退院時仮計算
           03  MENU-ITEM034-G.
               05  MENU-ITEM034-NUMBER         PIC 9(03)   VALUE  34.
               05  MENU-ITEM034-NAME           PIC X(30)   
                                               VALUE "退院時仮計算".
               05  MENU-ITEM034-GSRAUTH        PIC 9(02)   VALUE  39.
      *入院患者照会
           03  MENU-ITEM036-G.
               05  MENU-ITEM036-NUMBER         PIC 9(03)   VALUE  36.
               05  MENU-ITEM036-NAME           PIC X(30)   
                                               VALUE "入院患者照会".
               05  MENU-ITEM036-GSRAUTH        PIC 9(02)   VALUE  57.
      *データチェック
           03  MENU-ITEM041-G.
               05  MENU-ITEM041-NUMBER         PIC 9(03)   VALUE  41.
               05  MENU-ITEM041-NAME           PIC X(30)   
                                               VALUE "データチェック".
               05  MENU-ITEM041-GSRAUTH        PIC 9(02)   VALUE  26.
      *明細書
           03  MENU-ITEM042-G.
               05  MENU-ITEM042-NUMBER         PIC 9(03)   VALUE  42.
               05  MENU-ITEM042-NAME           PIC X(30)   
                                               VALUE "明細書".
               05  MENU-ITEM042-GSRAUTH        PIC 9(02)   VALUE  27.
      *請求管理
           03  MENU-ITEM043-G.
               05  MENU-ITEM043-NUMBER         PIC 9(03)   VALUE  43.
               05  MENU-ITEM043-NAME           PIC X(30)   
                                               VALUE "請求管理".
               05  MENU-ITEM043-GSRAUTH        PIC 9(02)   VALUE  28.
      *総括表
           03  MENU-ITEM044-G.
               05  MENU-ITEM044-NUMBER         PIC 9(03)   VALUE  44.
               05  MENU-ITEM044-NAME           PIC X(30)   
                                               VALUE "総括表".
               05  MENU-ITEM044-GSRAUTH        PIC 9(02)   VALUE  29.
      *日次統計
           03  MENU-ITEM051-G.
               05  MENU-ITEM051-NUMBER         PIC 9(03)   VALUE  51.
               05  MENU-ITEM051-NAME           PIC X(30)   
                                               VALUE "日次統計".
               05  MENU-ITEM051-GSRAUTH        PIC 9(02)   VALUE  31.
      *月次統計
           03  MENU-ITEM052-G.
               05  MENU-ITEM052-NUMBER         PIC 9(03)   VALUE  52.
               05  MENU-ITEM052-NAME           PIC X(30)   
                                               VALUE "月次統計".
               05  MENU-ITEM052-GSRAUTH        PIC 9(02)   VALUE  32.
      *データ出力
           03  MENU-ITEM071-G.
               05  MENU-ITEM071-NUMBER         PIC 9(03)   VALUE  71.
               05  MENU-ITEM071-NAME           PIC X(30)   
                                               VALUE "データ出力".
               05  MENU-ITEM071-GSRAUTH        PIC 9(02)   VALUE  41.
      *外部媒体
           03  MENU-ITEM082-G.
               05  MENU-ITEM082-NUMBER         PIC 9(03)   VALUE  82.
               05  MENU-ITEM082-NAME           PIC X(30)   
                                               VALUE "外部媒体".
               05  MENU-ITEM082-GSRAUTH        PIC 9(02)   VALUE  47.
      *マスタ登録
           03  MENU-ITEM091-G.
               05  MENU-ITEM091-NUMBER         PIC 9(03)   VALUE  91.
               05  MENU-ITEM091-NAME           PIC X(30)   
                                               VALUE "マスタ登録".
               05  MENU-ITEM091-GSRAUTH        PIC 9(02)   VALUE  51.
      *マスタ更新
           03  MENU-ITEM092-G.
               05  MENU-ITEM092-NUMBER         PIC 9(03)   VALUE  92.
               05  MENU-ITEM092-NAME           PIC X(30)   
                                               VALUE "マスタ更新".
               05  MENU-ITEM092-GSRAUTH        PIC 9(02)   VALUE  52.
      *システム管理
           03  MENU-ITEM101-G.
               05  MENU-ITEM101-NUMBER         PIC 9(03)   VALUE  101.
               05  MENU-ITEM101-NAME           PIC X(30)   
                                               VALUE "システム管理".
               05  MENU-ITEM101-GSRAUTH        PIC 9(02)   VALUE  61.
      *点数マスタ
           03  MENU-ITEM102-G.
               05  MENU-ITEM102-NUMBER         PIC 9(03)   VALUE  102.
               05  MENU-ITEM102-NAME           PIC X(30)   
                                               VALUE "点数マスタ".
               05  MENU-ITEM102-GSRAUTH        PIC 9(02)   VALUE  62.
      *チェックマスタ
           03  MENU-ITEM103-G.
               05  MENU-ITEM103-NUMBER         PIC 9(03)   VALUE  103.
               05  MENU-ITEM103-NAME           PIC X(30)   
                                               VALUE "チェックマスタ".
               05  MENU-ITEM103-GSRAUTH        PIC 9(02)   VALUE  63.
      *保険番号マスタ
           03  MENU-ITEM104-G.
               05  MENU-ITEM104-NUMBER         PIC 9(03)   VALUE  104.
               05  MENU-ITEM104-NAME           PIC X(30)   
                                               VALUE "保険番号マスタ".
               05  MENU-ITEM104-GSRAUTH        PIC 9(02)   VALUE  64.
      *保険者マスタ
           03  MENU-ITEM105-G.
               05  MENU-ITEM105-NUMBER         PIC 9(03)   VALUE  105.
               05  MENU-ITEM105-NAME           PIC X(30)   
                                               VALUE "保険者マスタ".
               05  MENU-ITEM105-GSRAUTH        PIC 9(02)   VALUE  65.
      *人名辞書マスタ
           03  MENU-ITEM106-G.
               05  MENU-ITEM106-NUMBER         PIC 9(03)   VALUE  106.
               05  MENU-ITEM106-NAME           PIC X(30)   
                                               VALUE "人名辞書マスタ".
               05  MENU-ITEM106-GSRAUTH        PIC 9(02)   VALUE  66.
      *薬剤情報マスタ
           03  MENU-ITEM107-G.
               05  MENU-ITEM107-NUMBER         PIC 9(03)   VALUE  107.
               05  MENU-ITEM107-NAME           PIC X(30)   
                                               VALUE "薬剤情報マスタ".
               05  MENU-ITEM107-GSRAUTH        PIC 9(02)   VALUE  67.
      *住所マスタ
           03  MENU-ITEM108-G.
               05  MENU-ITEM108-NUMBER         PIC 9(03)   VALUE  108.
               05  MENU-ITEM108-NAME           PIC X(30)   
                                               VALUE "住所マスタ".
               05  MENU-ITEM108-GSRAUTH        PIC 9(02)   VALUE  68.
      *ヘルプマスタ
           03  MENU-ITEM109-G.
               05  MENU-ITEM109-NUMBER         PIC 9(03)   VALUE  109.
               05  MENU-ITEM109-NAME           PIC X(30)   
                                               VALUE "ヘルプマスタ".
               05  MENU-ITEM109-GSRAUTH        PIC 9(02)   VALUE  69.
         02    MENU-ITEM-R REDEFINES   MENU-ITEM-VAL.
           03  MENU-ITEM-OCC   OCCURS          35.
               05  MENU-ITEM-NUMBER            PIC 9(03).
               05  MENU-ITEM-NAME              PIC X(30).
               05  MENU-ITEM-GSRAUTH           PIC 9(02).
      *
       01  MENU-ITEM-MAX                       PIC 9(02)   VALUE  35.
