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
      *    �����Ų�ץޥ����ʽ���     COPY (CPSRYACCTS.INC)      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  SRYACCTS-REC.
           03  ACCTS-KEY.
      *���ŵ��أɣ�
               05  ACCTS-HOSPNUM           PIC  9(02).
      *���ԣɣ�
               05  ACCTS-PTID              PIC  9(10).
      *���ֹ�
               05  ACCTS-ZAINUM            PIC  9(08).
      *Ϣ��
               05  ACCTS-RENNUM            PIC  9(03).
      *����
               05  ACCTS-DATE-HI           PIC  9(02).
      *���
           03  ACCTS-DAY-CNT               PIC  9(03).
      *----    �ʲ������ѹ���
      *������ʬ
           03  ACCTS-VNYUGAIKBN            PIC  X(01).
      *����ǯ��
           03  ACCTS-VSRYYM                PIC  X(06).
      *�ϰϻ��곫��
           03  ACCTS-VFROM                 PIC  9(02).
      *�ϰϻ��꽪λ
           03  ACCTS-VTO                   PIC  9(02).
