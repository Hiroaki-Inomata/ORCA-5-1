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
      *    �����å���ǧ�ǡ�������ʬ��   COPY (CPRECORCGD06.INC)  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.10    �Ҥ��ʤ�     01/10/15  ������򴵼��ֹ���ѹ�
      *
      ************************************************************
      *01  DID6-REC.
           03  DID6-KEY.
      *���ŵ��أɣ�
               05  DID6-HOSPID       PIC  X(024).
      *����/*����*01.10.15********************************************
      *���ԣɣ�
      *         05  DID6-PTID         PIC  9(010).
      *�����ֹ�
               05  DID6-PTNUM        PIC  X(020).
      *����/*��λ*01.10.15********************************************
      *��å������ɣ�
               05  DID6-MESGID       PIC  X(006).
      *�ǡ������Ͻ�
               05  DID6-SEQNO        PIC  9(009).
      *���Դ�����̾
           03  DID6-NAME             PIC  X(100).
      *����ǯ��
           03  DID6-NENREI           PIC  9(003).
      *��å�����
           03  DID6-MESG             PIC  X(200).
