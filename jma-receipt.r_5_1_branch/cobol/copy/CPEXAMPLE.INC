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
      *  �����ޥ�����       (CPEXAMPLE.INC)
      *****************************************************************
      *  ��������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
      *01  EXAMPLE-REC.
           03  EXAMPLE-KEY.
      *���ŵ��أɣ�
               05  EXAMPLE-HOSPNUM        PIC 9(2).
      *��ʬ�ʣ���ʸ�������
               05  EXAMPLE-KUBUN         PIC X(01).
      *Ϣ��
               05  EXAMPLE-RENNUM        PIC 9(03).
      *����
           03  EXAMPLE-COMMENT           PIC X(800).
      *ü���ɣ�
           03  EXAMPLE-TERMID            PIC X(16).
      *���ڡ��졼���ɣ�
           03  EXAMPLE-OPID              PIC X(16).
      *����ǯ����
           03  EXAMPLE-CREYMD            PIC X(08).
      *����ǯ����
           03  EXAMPLE-UPYMD             PIC X(08).
      *��������
           03  EXAMPLE-UPHMS             PIC X(06).