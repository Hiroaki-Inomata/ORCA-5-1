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
      *    �ɾ����ֵ����ޥ���        COPY (CPSSKIJYO.INC)       *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  SSKIJYO-REC.
           03  SSKIJYO-KEY.
      *�ɾ�������
               05  SSKIJYO-SYOJYOUCD        PIC  X(07).
      *�ɾ�
           03  SSKIJYO-SYOJYOU             PIC  X(2048).
      *���ѵ���
           03  SSKIJYO-SAYOKIJYO           PIC  X(2048).
      *ü���ɣ�
           03  SSKIJYO-TERMID              PIC  X(32).
      *���ڡ��졼���ɣ�
           03  SSKIJYO-OPID                PIC  X(16).
      *����ǯ����
           03  SSKIJYO-CREYMD              PIC  X(08).
      *����ǯ����
           03  SSKIJYO-UPYMD               PIC  X(08).
      *��������
           03  SSKIJYO-UPHMS               PIC  X(06).
      *