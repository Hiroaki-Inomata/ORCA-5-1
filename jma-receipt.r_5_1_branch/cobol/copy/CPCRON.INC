      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
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
      *         �ãңϣξ���ơ��֥�   (CPCRON.INC)              *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  CRON-REC.
           03  CRON-KEY.
               05  CRON-CRONID         PIC 9(04).
      *�¹Զ�ʬ
           03  CRON-RUN                PIC X(01).
      *����̾
           03  CRON-NAME               PIC X(30).
      *ʬ
           03  CRON-CRNMIN             PIC X(02).
      *��
           03  CRON-CRNHOUR            PIC X(02).
      *��
           03  CRON-CRNDOM             PIC X(02).
      *��
           03  CRON-CRNMON             PIC X(02).
      *����
           03  CRON-CRNDOW             PIC X(01).
      *������ץ�̾
           03  CRON-SCRIPT             PIC X(100).
      *ü���ɣ�
           03  CRON-TERMID             PIC  X(16).
      *���ڡ��졼���ɣ�
           03  CRON-OPID               PIC  X(16).
      *����ǯ����
           03  CRON-CREYMD             PIC  X(08).
      *����ǯ����
           03  CRON-UPYMD              PIC  X(08).
      *��������
           03  CRON-UPHMS              PIC  X(06).
      *--- �ʲ�virtual����
           03  CRON-STCRONID           PIC  9(04).
           03  CRON-EDCRONID           PIC  9(04).