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
      *    ��ߺ��ѥޥ���            COPY (CPINTERACT.INC)       *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  INTERACT-REC.
           03  INTERACT-KEY.
      *���ޥ�����
               05  INTERACT-DRUGCD             PIC  X(09).
      *���ޥ����ɣ�
               05  INTERACT-DRUGCD2            PIC  X(09).
      *�ɾ�������
               05  INTERACT-SYOJYOUCD          PIC  X(07).
      *ü���ɣ�
           03  INTERACT-TERMID              PIC  X(32).
      *���ڡ��졼���ɣ�
           03  INTERACT-OPID                PIC  X(16).
      *����ǯ����
           03  INTERACT-CREYMD              PIC  X(08).
      *����ǯ����
           03  INTERACT-UPYMD               PIC  X(08).
      *��������
           03  INTERACT-UPHMS               PIC  X(06).
      *