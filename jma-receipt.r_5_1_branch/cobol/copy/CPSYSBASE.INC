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
      *         �����ƥ����  COPY (CPSYSBASE.INC)               *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  SYSBASE-REC.
           03  SYSBASE-KEY.
      *    ���롼���ֹ�
               05  SYSBASE-GRPNUM      PIC  9(02).
      *    ���ŵ��ؼ����ֹ�
           03  SYSBASE-HOSPNUM         PIC  9(02).
      *    ���ŵ���̾��
           03  SYSBASE-HOSPNAME        PIC  X(120).
      *    ���Ѵ���
           03  SYSBASE-KIGEN           PIC  X(08).
      *    �ܱ�ʬ�����롼��
           03  SYSBASE-HONBUNGRP       PIC  9(02).
      *    �ܱ�ʬ����ʬ
           03  SYSBASE-HONBUNKBN       PIC  X(01).
      *
           03  SYSBASE-CREYMD          PIC  X(08).
           03  SYSBASE-UPYMD           PIC  X(08).
           03  SYSBASE-UPHMS           PIC  X(06).