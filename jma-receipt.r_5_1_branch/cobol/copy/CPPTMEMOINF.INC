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
      ***************************************************************
      *    ���ԥ����Ͽ                  COPY  (CPPTMEMOINF.INC)    *
      ***************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  PTMEMOINF-REC.
      *���ԥ��
           03  PTMEMOINF-KEY.
      *  ���ŵ��أɣ�
               05  PTMEMOINF-HOSPNUM       PIC 9(2).
      *  ���ԣɣ�
               05  PTMEMOINF-PTID          PIC  9(10).
      *  ��Ͽ��
               05  PTMEMOINF-SYSYMD        PIC  X(08).
      *  ��ʬ��1:��⣱��2:��⣲(�ạ̃��ɣͤ��Ϥ���
               05  PTMEMOINF-MEMOKBN       PIC  9(01).
      *  Ϣ��(���ζ�ʬ�����Ų����Ϣ�֡�����ɽ���˻��Ѥ����
               05  PTMEMOINF-RENNUM        PIC  9(02).
      *  ���Ų�
               05  PTMEMOINF-SRYKA         PIC  X(02).
      *�������
           03  PTMEMOINF-MEMO              PIC  X(1000).
      *ü���ɣ�
           03  PTMEMOINF-TERMID            PIC  X(16).
      *���ڡ��졼���ɣ�
           03  PTMEMOINF-OPID              PIC  X(16).
      *����ǯ����
           03  PTMEMOINF-CREYMD            PIC  X(08).
      *����ǯ����
           03  PTMEMOINF-UPYMD             PIC  X(08).
      *��������
           03  PTMEMOINF-UPHMS             PIC  X(06).
