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
      ************************************************************
      ************************************************************
      *    ����ʬ�ࡡ        �� COPY (CPKENSASORT.INC)     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  KENSASORT-REC.
      *����
           03  KENSASORT-KEY.
      *����ʬ�ॳ����
               05  KENSASORT-KNSBUNRUI     PIC  9(02).
      *���ť�����
               05  KENSASORT-SRYCD         PIC  X(09).
      *ɽ��Ϣ��
           03  KENSASORT-DSPSEQ            PIC  9(04).
      *ü���ɣ�
           03  KENSASORT-TERMID            PIC  X(32).
      *���ڥ졼���ɣ�
           03  KENSASORT-OPID              PIC  X(16).
      *����ǯ����
           03  KENSASORT-CREYMD            PIC  X(08).
      *����ǯ����
           03  KENSASORT-UPYMD             PIC  X(08).
      *��������
           03  KENSASORT-UPHMS             PIC  X(06).