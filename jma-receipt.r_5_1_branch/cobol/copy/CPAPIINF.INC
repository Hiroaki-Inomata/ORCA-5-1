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
      * 0510.202004
      ********************************************************************
       01  APIINF-AREA.
         03  APIINF-DATA.
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "acceptlstv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "受付一覧取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201906".
           05  FILLER                  PIC  X(16)      VALUE
           "orca11".
           05  FILLER                  PIC  X(32)      VALUE
           "acceptmodv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "受付登録".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "acsimulatev2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "請求金額取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202004".
           05  FILLER                  PIC  X(16)      VALUE
           "orca51".
           05  FILLER                  PIC  X(32)      VALUE
           "addressmasterlstv3".
           05  FILLER                  PIC  X(64)      VALUE
           "住所マスタ取得".
           05  FILLER                  PIC  X(64)      VALUE
           "住所マスタ取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "apiversiongetv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "日レセＡＰＩバージョン取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "appointlst2v2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "患者予約情報取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201901".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "appointlstv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "予約一覧取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "orca14".
           05  FILLER                  PIC  X(32)      VALUE
           "appointmodv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "予約登録".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "orca31".
           05  FILLER                  PIC  X(32)      VALUE
           "birthdeliveryv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "出産育児一時金情報取得・登録".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "contraindicationcheckv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "薬剤併用禁忌チェック".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "orca41".
           05  FILLER                  PIC  X(32)      VALUE
           "datacheckv3".
           05  FILLER                  PIC  X(64)      VALUE
           "データチェック".
           05  FILLER                  PIC  X(64)      VALUE
           "データチェック".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201901".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "diseasegetv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "患者病名情報取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201909".
           05  FILLER                  PIC  X(16)      VALUE
           "orca51".
           05  FILLER                  PIC  X(32)      VALUE
           "diseasemasterlstv3".
           05  FILLER                  PIC  X(64)      VALUE
           "病名マスタ一括取得".
           05  FILLER                  PIC  X(64)      VALUE
           "病名マスタ一括取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "orca22".
           05  FILLER                  PIC  X(32)      VALUE
           "diseasev2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "患者病名登録".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202002".
           05  FILLER                  PIC  X(16)      VALUE
           "orca22".
           05  FILLER                  PIC  X(32)      VALUE
           "diseasev3".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "患者病名登録２".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202002".
           05  FILLER                  PIC  X(16)      VALUE
           "orca13".
           05  FILLER                  PIC  X(32)      VALUE
           "findinfv3".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "照会検索項目情報取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202003".
           05  FILLER                  PIC  X(16)      VALUE
           "orca13".
           05  FILLER                  PIC  X(32)      VALUE
           "findv3".
           05  FILLER                  PIC  X(64)      VALUE
           "照会".
           05  FILLER                  PIC  X(64)      VALUE
           "照会".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201907".
           05  FILLER                  PIC  X(16)      VALUE
           "orca13".
           05  FILLER                  PIC  X(32)      VALUE
           "foundv3".
           05  FILLER                  PIC  X(64)      VALUE
           "照会".
           05  FILLER                  PIC  X(64)      VALUE
           "照会印刷指示".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201907".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "formdatagetv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "帳票データ取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "orca31".
           05  FILLER                  PIC  X(32)      VALUE
           "hsacctmodv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "入院会計照会".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201905".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "hsacsimulatev2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "仮計算情報取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "hsconfbasev2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "入院基本情報取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "hsconfwardv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "病棟・病室情報取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "orca36".
           05  FILLER                  PIC  X(32)      VALUE
           "hsfindv3".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "入院患者照会".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "hsmealv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "入院患者食事等情報取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "orca31".
           05  FILLER                  PIC  X(32)      VALUE
           "hspmmv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "入院会計未作成チェック".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "orca32".
           05  FILLER                  PIC  X(32)      VALUE
           "hsptevalmodv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "医療区分・ADL点数登録".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202003".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "hsptevalv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "医療区分・ADL点数情報取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202003".
           05  FILLER                  PIC  X(16)      VALUE
           "orca31".
           05  FILLER                  PIC  X(32)      VALUE
           "hsptinfmodv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "入退院登録".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201901".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "hsptinfv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "入院患者基本情報取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "imagegetv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "画像データ取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202001".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "incomeinfv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "収納情報取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201903".
           05  FILLER                  PIC  X(16)      VALUE
           "orca23".
           05  FILLER                  PIC  X(32)      VALUE
           "incomev3".
           05  FILLER                  PIC  X(64)      VALUE
           "収納".
           05  FILLER                  PIC  X(64)      VALUE
           "収納登録".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201910".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "insprogetv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "保険者一覧情報取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "insuranceinf1v2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "保険・公費一覧取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201912".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "invoicereceipthv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "請求書兼領収書（入院）印刷".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201910".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "invoicereceiptv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "請求書兼領収書（外来）印刷".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201910".
           05  FILLER                  PIC  X(16)      VALUE
           "orca02".
           05  FILLER                  PIC  X(32)      VALUE
           "jobmanagev3".
           05  FILLER                  PIC  X(64)      VALUE
           "ジョブ管理一覧取得、情報削除".
           05  FILLER                  PIC  X(64)      VALUE
           "ジョブ管理一覧取得、情報削除".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201908".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "karteno1hv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "カルテ１号紙（入院）印刷".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202004".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "karteno1v2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "カルテ１号紙（外来）印刷".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202004".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "karteno3hv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "カルテ３号紙（入院）印刷".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201910".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "karteno3v2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "カルテ３号紙（外来）印刷".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201910".
           05  FILLER                  PIC  X(16)      VALUE
           "orca101".
           05  FILLER                  PIC  X(32)      VALUE
           "manageusersv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "ユーザ管理".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201902".
           05  FILLER                  PIC  X(16)      VALUE
           "orca51".
           05  FILLER                  PIC  X(32)      VALUE
           "masterexportv3".
           05  FILLER                  PIC  X(64)      VALUE
           "マスタ取得".
           05  FILLER                  PIC  X(64)      VALUE
           "マスタ取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201903".
           05  FILLER                  PIC  X(16)      VALUE
           "orca51".
           05  FILLER                  PIC  X(32)      VALUE
           "masterlastupdatev3".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "マスタデータ最終更新日取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "medicalgetv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "診療情報取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202003".
           05  FILLER                  PIC  X(16)      VALUE
           "api21".
           05  FILLER                  PIC  X(32)      VALUE
           "medicalmodav31".
           05  FILLER                  PIC  X(64)      VALUE
           "入院診療行為".
           05  FILLER                  PIC  X(64)      VALUE
           "入院患者決定".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "api21".
           05  FILLER                  PIC  X(32)      VALUE
           "medicalmodav32".
           05  FILLER                  PIC  X(64)      VALUE
           "入院診療行為".
           05  FILLER                  PIC  X(64)      VALUE
           "診療内容基本チェック".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202003".
           05  FILLER                  PIC  X(16)      VALUE
           "api21".
           05  FILLER                  PIC  X(32)      VALUE
           "medicalmodav33".
           05  FILLER                  PIC  X(64)      VALUE
           "入院診療行為".
           05  FILLER                  PIC  X(64)      VALUE
           "診療確認".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202003".
           05  FILLER                  PIC  X(16)      VALUE
           "api21".
           05  FILLER                  PIC  X(32)      VALUE
           "medicalmodav34".
           05  FILLER                  PIC  X(64)      VALUE
           "入院診療行為".
           05  FILLER                  PIC  X(64)      VALUE
           "診療行為削除".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202003".
           05  FILLER                  PIC  X(16)      VALUE
           "api21".
           05  FILLER                  PIC  X(32)      VALUE
           "medicalmodav35".
           05  FILLER                  PIC  X(64)      VALUE
           "会計照会".
           05  FILLER                  PIC  X(64)      VALUE
           "入院調剤料変更".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202003".
           05  FILLER                  PIC  X(16)      VALUE
           "api21".
           05  FILLER                  PIC  X(32)      VALUE
           "medicalmodv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "中途終了データ登録".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202003".
           05  FILLER                  PIC  X(16)      VALUE
           "api21".
           05  FILLER                  PIC  X(32)      VALUE
           "medicalmodv23".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "初診算定日登録".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "api21".
           05  FILLER                  PIC  X(32)      VALUE
           "medicalmodv31".
           05  FILLER                  PIC  X(64)      VALUE
           "診療行為".
           05  FILLER                  PIC  X(64)      VALUE
           "診察料取得（初回接続）".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202003".
           05  FILLER                  PIC  X(16)      VALUE
           "api21".
           05  FILLER                  PIC  X(32)      VALUE
           "medicalmodv32".
           05  FILLER                  PIC  X(64)      VALUE
           "診療行為".
           05  FILLER                  PIC  X(64)      VALUE
           "診療内容基本チェック".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202003".
           05  FILLER                  PIC  X(16)      VALUE
           "api21".
           05  FILLER                  PIC  X(32)      VALUE
           "medicalmodv33".
           05  FILLER                  PIC  X(64)      VALUE
           "診療行為".
           05  FILLER                  PIC  X(64)      VALUE
           "診療確認・請求確認".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202003".
           05  FILLER                  PIC  X(16)      VALUE
           "api21".
           05  FILLER                  PIC  X(32)      VALUE
           "medicalmodv34".
           05  FILLER                  PIC  X(64)      VALUE
           "診療行為".
           05  FILLER                  PIC  X(64)      VALUE
           "診療行為削除".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202003".
           05  FILLER                  PIC  X(16)      VALUE
           "api21".
           05  FILLER                  PIC  X(32)      VALUE
           "medicalmodv35".
           05  FILLER                  PIC  X(64)      VALUE
           "診療行為".
           05  FILLER                  PIC  X(64)      VALUE
           "リハビリコメント登録".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202004".
           05  FILLER                  PIC  X(16)      VALUE
           "api21".
           05  FILLER                  PIC  X(32)      VALUE
           "medicalmodv36".
           05  FILLER                  PIC  X(64)      VALUE
           "会計照会".
           05  FILLER                  PIC  X(64)      VALUE
           "保険履歴情報一括変更".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201902".
           05  FILLER                  PIC  X(16)      VALUE
           "api21".
           05  FILLER                  PIC  X(32)      VALUE
           "medicalmodv37".
           05  FILLER                  PIC  X(64)      VALUE
           "排他制御解除".
           05  FILLER                  PIC  X(64)      VALUE
           "排他制御解除".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "orca21".
           05  FILLER                  PIC  X(32)      VALUE
           "medicalsetv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "セット登録".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202003".
           05  FILLER                  PIC  X(16)      VALUE
           "orca51".
           05  FILLER                  PIC  X(32)      VALUE
           "medicationmasterlstv3".
           05  FILLER                  PIC  X(64)      VALUE
           "点数マスタ一括取得".
           05  FILLER                  PIC  X(64)      VALUE
           "点数マスタ一括取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202003".
           05  FILLER                  PIC  X(16)      VALUE
           "orca102".
           05  FILLER                  PIC  X(32)      VALUE
           "medicatonmodv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "点数マスタ情報登録".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "medicinenotebookv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "お薬手帳印刷".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201910".
           05  FILLER                  PIC  X(16)      VALUE
           "orca51".
           05  FILLER                  PIC  X(32)      VALUE
           "patientbasisallv3".
           05  FILLER                  PIC  X(64)      VALUE
           "患者基本情報一括取得".
           05  FILLER                  PIC  X(64)      VALUE
           "患者基本情報一括取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202001".
           05  FILLER                  PIC  X(16)      VALUE
           "orca51".
           05  FILLER                  PIC  X(32)      VALUE
           "patientdiseaseallv3".
           05  FILLER                  PIC  X(64)      VALUE
           "患者病名情報一括取得".
           05  FILLER                  PIC  X(64)      VALUE
           "患者病名情報一括取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "patientgetv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "患者基本情報".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202004".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "patientlst1v2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "患者番号一覧取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "patientlst2v2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "複数患者情報取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "patientlst3v2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "患者情報取得(氏名検索)".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "patientlst6v2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "全保険組合せ一覧取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201906".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "patientlst7v2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "患者メモ情報取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "orca51".
           05  FILLER                  PIC  X(32)      VALUE
           "patientmedicalallv3".
           05  FILLER                  PIC  X(64)      VALUE
           "患者診療情報一括取得".
           05  FILLER                  PIC  X(64)      VALUE
           "患者診療情報一括取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202003".
           05  FILLER                  PIC  X(16)      VALUE
           "orca12".
           05  FILLER                  PIC  X(32)      VALUE
           "patientmodv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "患者登録".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202004".
           05  FILLER                  PIC  X(16)      VALUE
           "orca12".
           05  FILLER                  PIC  X(32)      VALUE
           "patientmodv31".
           05  FILLER                  PIC  X(64)      VALUE
           "患者登録".
           05  FILLER                  PIC  X(64)      VALUE
           "患者基本情報登録".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202002".
           05  FILLER                  PIC  X(16)      VALUE
           "orca12".
           05  FILLER                  PIC  X(32)      VALUE
           "patientmodv32".
           05  FILLER                  PIC  X(64)      VALUE
           "患者登録".
           05  FILLER                  PIC  X(64)      VALUE
           "患者保険公費情報登録".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202004".
           05  FILLER                  PIC  X(16)      VALUE
           "orca12".
           05  FILLER                  PIC  X(32)      VALUE
           "patientmodv33".
           05  FILLER                  PIC  X(64)      VALUE
           "患者登録".
           05  FILLER                  PIC  X(64)      VALUE
           "患者労災自賠責情報登録".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202004".
           05  FILLER                  PIC  X(16)      VALUE
           "orca12".
           05  FILLER                  PIC  X(32)      VALUE
           "patientmodv34".
           05  FILLER                  PIC  X(64)      VALUE
           "患者登録".
           05  FILLER                  PIC  X(64)      VALUE
           "患者低所得情報登録".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202002".
           05  FILLER                  PIC  X(16)      VALUE
           "orca12".
           05  FILLER                  PIC  X(32)      VALUE
           "patientmodv35".
           05  FILLER                  PIC  X(64)      VALUE
           "患者登録".
           05  FILLER                  PIC  X(64)      VALUE
           "患者公費一部負担金登録".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201908".
           05  FILLER                  PIC  X(16)      VALUE
           "orca12".
           05  FILLER                  PIC  X(32)      VALUE
           "patientmodv36".
           05  FILLER                  PIC  X(64)      VALUE
           "患者登録".
           05  FILLER                  PIC  X(64)      VALUE
           "介護情報登録".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201908".
           05  FILLER                  PIC  X(16)      VALUE
           "orca12".
           05  FILLER                  PIC  X(32)      VALUE
           "patientmodv37".
           05  FILLER                  PIC  X(64)      VALUE
           "患者登録".
           05  FILLER                  PIC  X(64)      VALUE
           "患者禁忌薬剤登録".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201908".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "prescriptionv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "処方せん印刷".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202004".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "pusheventgetv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "PUSH通知一括取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "orca44".
           05  FILLER                  PIC  X(32)      VALUE
           "receiptdatacheckmakev3".
           05  FILLER                  PIC  X(64)      VALUE
           "チェック用個別レセ電データ取得".
           05  FILLER                  PIC  X(64)      VALUE
           "チェック用個別レセ電データ取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "orca44".
           05  FILLER                  PIC  X(32)      VALUE
           "receiptdatamakev3".
           05  FILLER                  PIC  X(64)      VALUE
           "レセ電データ一括取得".
           05  FILLER                  PIC  X(64)      VALUE
           "レセ電データ一括取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "orca44".
           05  FILLER                  PIC  X(32)      VALUE
           "receiptdatapatientmakev3".
           05  FILLER                  PIC  X(64)      VALUE
           "個別レセ電データ取得".
           05  FILLER                  PIC  X(64)      VALUE
           "個別レセ電データ取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "orca42".
           05  FILLER                  PIC  X(32)      VALUE
           "receiptmakev3".
           05  FILLER                  PIC  X(64)      VALUE
           "明細書".
           05  FILLER                  PIC  X(64)      VALUE
           "レセプト作成".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "orca42".
           05  FILLER                  PIC  X(32)      VALUE
           "receiptprintv3".
           05  FILLER                  PIC  X(64)      VALUE
           "明細書".
           05  FILLER                  PIC  X(64)      VALUE
           "レセプト印刷".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "orca01".
           05  FILLER                  PIC  X(32)      VALUE
           "reprintv3".
           05  FILLER                  PIC  X(64)      VALUE
           "再印刷".
           05  FILLER                  PIC  X(64)      VALUE
           "再印刷データ一覧返却、印刷データ取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201910".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "statementhv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "診療費明細書（入院）印刷".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201910".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "statementv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "診療費明細書（外来）印刷".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201910".
           05  FILLER                  PIC  X(16)      VALUE
           "orca51".
           05  FILLER                  PIC  X(32)      VALUE
           "statisticsformv3".
           05  FILLER                  PIC  X(64)      VALUE
           "日次・月次統計".
           05  FILLER                  PIC  X(64)      VALUE
           "日次・月次統計".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "subjectiveslstv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "症状詳記情報取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "orca25".
           05  FILLER                  PIC  X(32)      VALUE
           "subjectivesv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "症状詳記登録".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201912".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "system01dailyv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "基本情報取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "system01lstv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "システム管理情報取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "systeminfv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "システム状態取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "orca51".
           05  FILLER                  PIC  X(32)      VALUE
           "tempmedicaladdv3".
           05  FILLER                  PIC  X(64)      VALUE
           "入院診療データ一括登録".
           05  FILLER                  PIC  X(64)      VALUE
           "入院診療データ一括登録".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "tmedicalgetv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "中途終了患者情報一覧取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.202004".
           05  FILLER                  PIC  X(16)      VALUE
           "api21".
           05  FILLER                  PIC  X(32)      VALUE
           "tmedicalmodv2".
           05  FILLER                  PIC  X(64)      VALUE
           "中途データ管理".
           05  FILLER                  PIC  X(64)      VALUE
           "中途データ管理".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
           05  FILLER                  PIC  X(16)      VALUE
           "api01rv2".
           05  FILLER                  PIC  X(32)      VALUE
           "visitptlstv2".
           05  FILLER                  PIC  X(64)      VALUE
           SPACE.
           05  FILLER                  PIC  X(64)      VALUE
           "来院患者一覧取得".
           05  FILLER                  PIC  X(16)      VALUE
           "0510.201812".
         03  APIINF-R REDEFINES APIINF-DATA.
           05  APIINF-OCC              OCCURS 101.
             07  APIINF-LD             PIC  X(16).
             07  APIINF-ID             PIC  X(32).
             07  APIINF-HAORI-NAME     PIC  X(64).
             07  APIINF-NAME           PIC  X(64).
             07  APIINF-VERSION        PIC  X(16).
         03  APIINF-MAX                PIC  9(05)      VALUE 101.
