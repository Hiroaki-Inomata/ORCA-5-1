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
      *-------------------------------------------------*
      *    ���������񡦰��̥����ɿ���    COPY (CPSENTEICDCHG.INC)
      *
      *-------------------------------------------------*
      *01  SENTEICDCHG-REC.
           03  SENCHG-KEY.
      *���̿��ť�����
               05  SENCHG-IPNSRYCD     PIC  X(09).
      *������������ť�����
               05  SENCHG-SENTEISRYCD  PIC  X(09).
      *ü���ɣ�
           03  SENCHG-TERMID           PIC  X(16).
      *���ڥ졼���ɣ�
           03  SENCHG-OPID             PIC  X(16).
      *����ǯ����
           03  SENCHG-CREYMD           PIC  X(08).
      *����ǯ����
           03  SENCHG-UPYMD            PIC  X(08).
      *��������
           03  SENCHG-UPHMS            PIC  X(06).
