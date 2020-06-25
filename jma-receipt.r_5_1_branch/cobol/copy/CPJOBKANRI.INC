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
      *         ジョブ管理    COPY   (CPJOBKANRI.INC)            *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    02.08.30  ステップ件数の追加
      *  02.05.00    NACL-竹田    04.09.27  JOB中止フラグ等追加
      *  02.06.00    NACL-太田    05.08.29  診療年月等追加
      *
      *  04.07.01    NACL-藤原    12.09.07  更新件数（個別）の追加
      *
      *  05.01.01    NACL-藤原    17.07.07  カルテＵＩＤ、オルカＵＩＤの追加
      ************************************************************
      *01  JOBKANRI-REC.
      *キー
           03  JOB-KEY.
               05  JOB-HOSPNUM           PIC 9(2).
      *ジョブ管理コード
               05  JOB-JOBID            PIC  9(07).
      *シェルＩＤ
               05  JOB-SHELLID          PIC  X(08).
      *実施オペーレータＩＤ
               05  JOB-RUN-OPID         PIC  X(16).
      *シェルＭＳＧ
           03  JOB-SHELLMSG             PIC  X(100).
      *端末ＩＤ
           03  JOB-TERMID               PIC  X(32).
      *オペーレータＩＤ
           03  JOB-OPID                 PIC  X(16).
      *オペーレータＩＤ
           03  JOB-PGID                 PIC  X(20).
      *ジョブ開始年月日
           03  JOB-STARTYMD             PIC  X(08).
      *ジョブ終了年月日
           03  JOB-ENDYMD               PIC  X(08).
      *ジョブ開始時間
           03  JOB-STARTTIME            PIC  X(08).
      *ジョブ終了時間
           03  JOB-ENDTIME              PIC  X(08).
      *ステップ開始時間
           03  JOB-STEPSTARTTIME        PIC  X(08).
      *ステップ終了時間
           03  JOB-STEPENDTIME          PIC  X(08).
      *パラメータ
           03  JOB-PARAM                PIC  X(256).
      *エラーコード
           03  JOB-ERRCD                PIC  X(04).
      *一括・個別区分
           03  JOB-SYOKBN               PIC  X(01).
      *診療年月
           03  JOB-SRYYM                PIC  X(06).
      *入外区分
           03  JOB-NYUGAIKBN            PIC  X(01).
      *診療年月日
           03  JOB-SRYYMD               PIC  X(08).
      *患者ＩＤ
           03  JOB-PTID                 PIC  9(10).
      *更新件数
           03  JOB-UPDCNT               PIC  9(10).
      *ステップ件数
           03  JOB-STEPCNT              PIC  9(03).
      *処理中止フラグ
           03  JOB-STOPFLG              PIC  9(01).
      *プロセスID
           03  JOB-PID1                 PIC  9(05).
           03  JOB-PID2                 PIC  9(05).
           03  JOB-PID3                 PIC  9(05).
           03  JOB-PID4                 PIC  9(05).
      *更新件数（個別）
           03  JOB-UPDCNT1              PIC  9(10).
           03  JOB-UPDCNT2              PIC  9(10).
      *カルテＵＩＤ
           03  JOB-KARTE-UID            PIC X(36).
      *オルカＵＩＤ
           03  JOB-ORCA-UID             PIC X(36).
      *予備
           03  JOB-YOBI                 PIC  X(500).
