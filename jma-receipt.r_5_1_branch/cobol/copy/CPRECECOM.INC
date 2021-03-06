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
      *****************************************************************
      *  �쥻�ץȥ����ȥޥ�����       (CPRECECOM.INC)
      *****************************************************************
      *  ��������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-ƣ��    03/03/05  ������ʬ���ɲ�
      *  01.00.02    NACL-¿��Ǽ  05/07/19  �����Ȥ򣱣�������
      *
      *  03.00.01    NACL-ƣ��    06/06/13  �ɾ��ܵ���ʬ���ɲ�
      *  04.06.00    NACL-¿��Ǽ  10/10/01  �����Ȥ򣴣�������
      *                                     Ϣ���ɲ�
      *****************************************************************
      *
      *01  RECECOM-REC.
           03  RECECOM-KEY.
      *���ŵ��أɣ�
               05  RECECOM-HOSPNUM        PIC 9(2).
      *���ԣɣ�
               05  RECECOM-PTID          PIC 9(10).
      *������ʬ
               05  RECECOM-NYUGAIKBN     PIC X(01).
      *���Ų�
               05  RECECOM-SRYKA         PIC X(02).
      *����ǯ��
               05  RECECOM-SRYYM         PIC X(06).
      *�ݸ��ȹ礻�ֹ�
               05  RECECOM-HKNCOMBI      PIC 9(04).
      *������
               05  RECECOM-SRYDD         PIC X(02).
      *�ɾ��ܵ���ʬ
               05  RECECOM-SJKBN         PIC X(02).
      *Ϣ��
               05  RECECOM-RENNUM        PIC 9(02).
      *������
           03  RECECOM-COMMENT           PIC X(4800).
      *ü���ɣ�
           03  RECECOM-TERMID            PIC X(16).
      *���ڡ��졼���ɣ�
           03  RECECOM-OPID              PIC X(16).
      *����ǯ����
           03  RECECOM-CREYMD            PIC X(08).
      *����ǯ����
           03  RECECOM-UPYMD             PIC X(08).
      *��������
           03  RECECOM-UPHMS             PIC X(06).
      *
      *�����ѥ���
           03  RECECOM-UPD-KEY.
      *���ŵ��أɣ�
               05  RECECOM-UPD-HOSPNUM       PIC 9(2).
      *���ԣɣ�
               05  RECECOM-UPD-PTID          PIC 9(10).
      *������ʬ
               05  RECECOM-UPD-NYUGAIKBN     PIC X(01).
      *���Ų�
               05  RECECOM-UPD-SRYKA         PIC X(02).
      *����ǯ��
               05  RECECOM-UPD-SRYYM         PIC X(06).
      *�ݸ��ȹ礻�ֹ�
               05  RECECOM-UPD-HKNCOMBI      PIC 9(04).
      *������
               05  RECECOM-UPD-SRYDD         PIC X(02).
      *�ɾ��ܵ���ʬ
               05  RECECOM-UPD-SJKBN         PIC X(02).
      *Ϣ��
               05  RECECOM-UPD-RENNUM        PIC 9(02).
